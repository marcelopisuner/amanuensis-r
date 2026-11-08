KEY2 <- "AIzaSyANOTHER-key-abcdefghijklmnopqrstu"

local_renviron <- function(lines = NULL, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, ".Renviron")
  if (!is.null(lines)) writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), path)
  withr::local_envvar(R_ENVIRON_USER = path, GEMINI_API_KEY = NA, GOOGLE_API_KEY = NA, .local_envir = env)
  path
}

test_that("the key's shape is checked with plain explanations", {
  expect_error(.check_key_shape("AIza key with spaces 12345678"), class = "amanuensis_error_key",
               regexp = "contains a space")
  expect_error(.check_key_shape("AIzaABC\n12345678901234567890"), class = "amanuensis_error_key",
               regexp = "line break")
  expect_error(.check_key_shape("AIza12345"), class = "amanuensis_error_key", regexp = "only 9 characters")
  expect_error(.check_key_shape("   "), class = "amanuensis_error_key", regexp = "empty")
  expect_identical(.check_key_shape(paste0("  ", FAKE_KEY, "\n")), FAKE_KEY)
})

test_that("the key is shown masked", {
  expect_identical(.mask_key(FAKE_KEY), paste0("AIza...fgh (", nchar(FAKE_KEY), " characters)"))
})

test_that("set_key(store = TRUE) replaces the key line and keeps every other line", {
  path <- local_renviron(c("GITHUB_PAT=ghp_abc", "  GEMINI_API_KEY = old-key", "R_LIBS=~/lib",
                           "GEMINI_API_KEY=duplicate"))
  msgs <- all_condition_text(set_key(FAKE_KEY, store = TRUE, check = FALSE))
  expect_identical(Sys.getenv("GEMINI_API_KEY"), FAKE_KEY)
  expect_identical(readLines(path), c("GITHUB_PAT=ghp_abc", paste0("GEMINI_API_KEY=", FAKE_KEY), "R_LIBS=~/lib"))
  # the file ends with a newline and is private
  bytes <- readBin(path, "raw", file.size(path))
  expect_identical(bytes[length(bytes)], charToRaw("\n"))
  if (.Platform$OS.type == "unix") expect_identical(as.character(file.mode(path)), "600")
  # messages show the masked key only
  expect_false(any(grepl(FAKE_KEY, msgs, fixed = TRUE)))
  expect_true(any(grepl("AIza...fgh", msgs, fixed = TRUE)))
  expect_true(any(grepl("Saved in", msgs)))
})

test_that("set_key(store = TRUE) appends to a file without a key line, or creates the file", {
  path <- local_renviron(c("FOO=bar", ""))
  quietly(set_key(FAKE_KEY, store = TRUE, check = FALSE))
  expect_identical(readLines(path), c("FOO=bar", paste0("GEMINI_API_KEY=", FAKE_KEY)))

  path2 <- local_renviron()
  expect_false(file.exists(path2))
  res <- quietly(set_key(KEY2, store = TRUE, check = FALSE))
  expect_identical(res, path2)
  expect_identical(readLines(path2), paste0("GEMINI_API_KEY=", KEY2))
})

test_that("store = FALSE sets the key for this session only", {
  path <- local_renviron()
  res <- quietly(set_key(FAKE_KEY, store = FALSE, check = FALSE))
  expect_null(res)
  expect_false(file.exists(path))
  expect_identical(Sys.getenv("GEMINI_API_KEY"), FAKE_KEY)
})

test_that("store = NA outside an interactive session does not write and says how to save", {
  skip_if(interactive())
  path <- local_renviron()
  expect_true(any(grepl("this session only", all_condition_text(set_key(FAKE_KEY, check = FALSE)))))
  expect_false(file.exists(path))
})

test_that("a key .Renviron cannot hold is used for the session but not saved", {
  path <- local_renviron()
  odd <- "AIza$weird#key-0123456789abcdefghijk"
  expect_true(any(grepl("not saved", all_condition_text(set_key(odd, store = TRUE, check = FALSE)))))
  expect_false(file.exists(path))
  expect_identical(Sys.getenv("GEMINI_API_KEY"), odd)
})

test_that("set_key(remove = TRUE) removes the line and the session key", {
  path <- local_renviron(c("A=1", paste0("GEMINI_API_KEY=", FAKE_KEY), "B=2"))
  Sys.setenv(GEMINI_API_KEY = FAKE_KEY)
  expect_message(res <- set_key(remove = TRUE), "Removed the key")
  expect_identical(res, path)
  expect_identical(readLines(path), c("A=1", "B=2"))
  expect_identical(Sys.getenv("GEMINI_API_KEY"), "")
  # nothing stored: says so
  expect_message(set_key(remove = TRUE), "No stored key")
})

test_that("set_key() without a key outside an interactive session stops", {
  skip_if(interactive())
  local_renviron()
  expect_error(set_key(), class = "amanuensis_error_key", regexp = "No key given")
})

test_that("set_key() mentions a different GOOGLE_API_KEY", {
  local_renviron()
  withr::local_envvar(GOOGLE_API_KEY = KEY2)
  expect_true(any(grepl("GOOGLE_API_KEY", all_condition_text(set_key(FAKE_KEY, store = FALSE, check = FALSE)))))
})

test_that(".gemini_key prefers GEMINI_API_KEY, then GOOGLE_API_KEY, else explains", {
  withr::local_envvar(GEMINI_API_KEY = FAKE_KEY, GOOGLE_API_KEY = KEY2)
  expect_identical(.gemini_key(), FAKE_KEY)
  withr::local_envvar(GEMINI_API_KEY = NA)
  expect_identical(.gemini_key(), KEY2)
  withr::local_envvar(GOOGLE_API_KEY = NA)
  expect_error(.gemini_key(), class = "amanuensis_error_key", regexp = "set_key")
})

test_that("the online check reports acceptance, refusal and network trouble", {
  local_renviron()
  local_amanuensis()
  seen <- list()
  httr2::local_mocked_responses(function(req) {
    seen[[length(seen) + 1]] <<- req
    httr2::response_json(body = list(models = list(list(name = "models/gemini-3.8-flash"))))
  })
  msgs <- all_condition_text(set_key(FAKE_KEY, store = FALSE, check = TRUE))
  expect_true(any(grepl("Google accepted the key", msgs)))
  expect_length(seen, 1)
  req <- seen[[1]]
  expect_match(req$url, "^https://generativelanguage.googleapis.com/v1beta/models\\?pageSize=1$")
  expect_false(grepl(FAKE_KEY, req$url, fixed = TRUE))
  expect_identical(httr2::req_get_headers(req, "reveal")[["x-goog-api-key"]], FAKE_KEY)

  httr2::local_mocked_responses(function(req) error_response(400, "API key not valid. Please pass a valid API key."))
  msgs <- all_condition_text(res <- .check_key_online(FAKE_KEY))
  expect_false(res)
  expect_match(paste(msgs, collapse = " "), "refused the key \\(HTTP 400\\)")

  httr2::local_mocked_responses(function(req) network_error())
  msgs <- all_condition_text(res <- .check_key_online(FAKE_KEY))
  expect_identical(res, NA)
  expect_match(paste(msgs, collapse = " "), "Could not check the key")
  expect_false(any(grepl(FAKE_KEY, msgs, fixed = TRUE)))
})

test_that("set_key(remove = TRUE) keeps .Renviron private (it often holds other keys)", {
  skip_on_os("windows")
  # the usual umask, under which a new file is readable by every user
  old <- Sys.umask("022")
  withr::defer(Sys.umask(old))
  path <- local_renviron(c("GITHUB_PAT=ghp_abc", paste0("GEMINI_API_KEY=", FAKE_KEY)))
  Sys.chmod(path, "0600", use_umask = FALSE)
  quietly(set_key(remove = TRUE))
  expect_identical(readLines(path), "GITHUB_PAT=ghp_abc")
  expect_identical(format(file.mode(path)), "600")
  # a new .Renviron is private too, and the umask is left as it was
  path2 <- local_renviron()
  quietly(set_key(FAKE_KEY, store = TRUE, check = FALSE))
  expect_identical(format(file.mode(path2)), "600")
  expect_identical(format(Sys.umask()), "22")
})

test_that("a .Renviron that is a symbolic link is written through, and the link survives", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  real <- file.path(dir, "dotfiles", "Renviron")
  dir.create(dirname(real))
  writeLines("GITHUB_PAT=ghp_abc", real)
  link <- file.path(dir, ".Renviron")
  # a relative link, as stow makes them
  skip_if_not(file.symlink(file.path("dotfiles", "Renviron"), link), "cannot make symbolic links here")
  withr::local_envvar(R_ENVIRON_USER = link, GEMINI_API_KEY = NA, GOOGLE_API_KEY = NA)

  msgs <- all_condition_text(res <- set_key(FAKE_KEY, store = TRUE, check = FALSE))
  expect_identical(res, link)
  expect_true(nzchar(Sys.readlink(link)))
  expect_identical(readLines(real), c("GITHUB_PAT=ghp_abc", paste0("GEMINI_API_KEY=", FAKE_KEY)))
  expect_identical(format(file.mode(real)), "600")
  # the message names the file that now holds the key
  expect_true(any(grepl(basename(dirname(real)), msgs, fixed = TRUE)))

  quietly(set_key(remove = TRUE))
  expect_true(nzchar(Sys.readlink(link)))
  expect_identical(readLines(real), "GITHUB_PAT=ghp_abc")
  expect_setequal(list.files(dir, all.files = TRUE, no.. = TRUE), c(".Renviron", "dotfiles"))
})

test_that(".resolve_link follows a chain of links to a file that may not exist yet", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a")
  b <- file.path(dir, "b")
  skip_if_not(file.symlink("b", a) && file.symlink(file.path(dir, "c"), b), "cannot make symbolic links here")
  expect_identical(.resolve_link(a), file.path(dir, "c"))
  expect_identical(.resolve_link(file.path(dir, "plain")), file.path(dir, "plain"))
})
