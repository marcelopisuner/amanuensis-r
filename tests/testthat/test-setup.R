# check_setup(): one row per check, the key shown masked and traced to where
# it is stored, Google contacted only when asked (mocked here), nothing left
# behind in the working folder.

SETUP_CHECKS <- c("R", "text encoding", "pdftools", "magick", "image formats", "readxl", "writexl", "httr2",
                  "cli", "other packages", "haven", "shiny")
SECOND_KEY <- "AIzaSyANOTHER-key-abcdefghijklmnopqrstu"

# A clean working folder and a personal .Renviron of our own (not the real one).
local_setup <- function(key = FAKE_KEY, env = parent.frame()) {
  local_amanuensis(env = env, key = key)
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_dir(dir, .local_envir = env)
  withr::local_envvar(R_ENVIRON_USER = file.path(withr::local_tempdir(.local_envir = env), ".Renviron"),
                      .local_envir = env)
  dir
}

run_setup <- function(...) {
  out <- NULL
  printed <- utils::capture.output(out <- check_setup(...))
  attr(out, "printed") <- printed
  out
}

row_of <- function(out, check) out[out$check == check, , drop = FALSE]

test_that("check_setup(online = FALSE) returns one row per check and writes nothing", {
  dir <- local_setup()
  out <- run_setup(online = FALSE)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("check", "status", "detail", "fix"))
  expect_identical(out$check, c(SETUP_CHECKS, "Gemini key", "working folder", "synced folder"))
  expect_true(all(out$status %in% c("OK", "NOTE", "PROBLEM")))
  expect_identical(row_of(out, "R")$status, "OK")
  expect_identical(row_of(out, "pdftools")$status, "OK")
  expect_identical(row_of(out, "magick")$status, "OK")
  expect_identical(row_of(out, "image formats")$status %in% c("OK", "NOTE"), TRUE)
  expect_identical(row_of(out, "working folder")$status, "OK")
  expect_identical(row_of(out, "synced folder")$status, "OK")
  expect_true(all(out$status[out$check %in% c("haven", "shiny")] %in% c("OK", "NOTE")))
  expect_true(all(nzchar(out$fix[out$status != "OK"])))
  expect_false("Google" %in% out$check)
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), character())
})

test_that("the key is shown masked, with the variable and file it comes from", {
  local_setup()
  out <- run_setup(online = FALSE)
  key_row <- row_of(out, "Gemini key")
  expect_identical(key_row$status, "OK")
  expect_match(key_row$detail, "AIza...fgh", fixed = TRUE)
  expect_match(key_row$detail, "GEMINI_API_KEY", fixed = TRUE)
  expect_match(key_row$detail, "not stored", fixed = TRUE)
  expect_false(any(grepl(FAKE_KEY, c(out$detail, attr(out, "printed")), fixed = TRUE)))

  writeLines(c("GITHUB_PAT=abc", paste0("GEMINI_API_KEY=", FAKE_KEY)), Sys.getenv("R_ENVIRON_USER"))
  out <- run_setup(online = FALSE)
  expect_match(row_of(out, "Gemini key")$detail, basename(Sys.getenv("R_ENVIRON_USER")), fixed = TRUE)
  expect_no_match(row_of(out, "Gemini key")$detail, "not stored", fixed = TRUE)

  writeLines(paste0("GEMINI_API_KEY=", SECOND_KEY), Sys.getenv("R_ENVIRON_USER"))
  out <- run_setup(online = FALSE)
  expect_match(row_of(out, "Gemini key")$detail, "holds a different key", fixed = TRUE)
})

test_that("a missing key is a problem with the fix spelled out", {
  local_setup()
  withr::local_envvar(GEMINI_API_KEY = NA, GOOGLE_API_KEY = NA)
  out <- run_setup(online = FALSE)
  key_row <- row_of(out, "Gemini key")
  expect_identical(key_row$status, "PROBLEM")
  expect_match(key_row$fix, "set_key()", fixed = TRUE)
  expect_true(any(grepl("PROBLEM", attr(out, "printed"))))
  expect_true(any(grepl("Fix: Run set_key()", attr(out, "printed"), fixed = TRUE)))
  expect_match(attr(out, "printed")[length(attr(out, "printed"))], "problem")
})

test_that("GOOGLE_API_KEY is used when it is the only key, and a different second key is a note", {
  local_setup()
  withr::local_envvar(GEMINI_API_KEY = NA, GOOGLE_API_KEY = SECOND_KEY)
  out <- run_setup(online = FALSE)
  expect_identical(row_of(out, "Gemini key")$status, "OK")
  expect_match(row_of(out, "Gemini key")$detail, "from GOOGLE_API_KEY", fixed = TRUE)
  expect_false("second key" %in% out$check)

  withr::local_envvar(GEMINI_API_KEY = FAKE_KEY, GOOGLE_API_KEY = SECOND_KEY)
  out <- run_setup(online = FALSE)
  expect_identical(row_of(out, "second key")$status, "NOTE")
  expect_match(row_of(out, "second key")$detail, "AIza...stu", fixed = TRUE)

  withr::local_envvar(GOOGLE_API_KEY = FAKE_KEY)
  expect_false("second key" %in% run_setup(online = FALSE)$check)
})

test_that("a key with the wrong shape is a problem", {
  local_setup(key = "AIza key with spaces 12345678")
  out <- run_setup(online = FALSE)
  expect_identical(row_of(out, "Gemini key")$status, "PROBLEM")
  expect_match(row_of(out, "Gemini key")$detail, "contains a space")
})

test_that("key = FALSE skips the key, and online = TRUE then only checks that Google answers", {
  local_setup()
  out <- run_setup(key = FALSE, online = FALSE)
  expect_identical(out$check, c(SETUP_CHECKS, "working folder", "synced folder"))

  httr2::local_mocked_responses(function(req) {
    httr2::response_json(status_code = 403, body = list(error = list(message = "unregistered callers")))
  })
  out <- run_setup(key = FALSE)
  expect_identical(row_of(out, "Google")$status, "OK")
  expect_match(row_of(out, "Google")$detail, "reachable")
})

test_that("online = TRUE asks Google about the key (mocked)", {
  local_setup()
  seen <- character()
  httr2::local_mocked_responses(function(req) {
    seen <<- c(seen, req$url)
    httr2::response_json(body = list(models = list()))
  })
  out <- run_setup()
  expect_identical(row_of(out, "Google")$status, "OK")
  expect_match(row_of(out, "Google")$detail, "accepts the key")
  expect_length(seen, 1)
  expect_match(seen, "v1beta/models", fixed = TRUE)

  httr2::local_mocked_responses(function(req) {
    httr2::response_json(status_code = 400, body = list(error = list(message = "API key not valid.")))
  })
  out <- run_setup()
  expect_identical(row_of(out, "Google")$status, "PROBLEM")
  expect_match(row_of(out, "Google")$detail, "refused")

  httr2::local_mocked_responses(function(req) stop("Could not resolve host"))
  out <- run_setup()
  expect_identical(row_of(out, "Google")$status, "PROBLEM")
  expect_match(row_of(out, "Google")$detail, "Could not reach Google")
})

test_that("a working folder inside OneDrive gets a note", {
  dir <- local_setup()
  synced <- file.path(dir, "OneDrive - Universitat", "padro")
  dir.create(synced, recursive = TRUE)
  withr::local_dir(synced)
  out <- run_setup(online = FALSE)
  expect_identical(row_of(out, "synced folder")$status, "NOTE")
  expect_match(row_of(out, "synced folder")$detail, "OneDrive")
  expect_identical(list.files(synced, all.files = TRUE, no.. = TRUE), character())
})

test_that("a working folder that cannot be written to is a problem", {
  skip_on_os("windows")
  dir <- local_setup()
  locked <- file.path(dir, "locked")
  dir.create(locked)
  Sys.chmod(locked, "0555")
  withr::defer(Sys.chmod(locked, "0755"))
  skip_if(file.access(locked, 2) == 0, "running with permission to write anywhere")
  withr::local_dir(locked)
  out <- run_setup(online = FALSE)
  expect_identical(row_of(out, "working folder")$status, "PROBLEM")
  expect_match(row_of(out, "working folder")$fix, "New Project", fixed = TRUE)
})

test_that("the printout has one aligned entry per check and no emoji", {
  local_setup()
  out <- run_setup(online = FALSE)
  printed <- attr(out, "printed")
  expect_identical(printed[1], "amanuensis setup check")
  starts <- grep("^  \\S", printed, value = TRUE)
  expect_length(starts, nrow(out))
  status_col <- regexpr("(OK|NOTE|PROBLEM) ", starts)
  expect_length(unique(status_col), 1)
  expect_identical(capture.output(print(out)), printed)
  expect_false(any(grepl("[^\\x01-\\x7F]", printed, perl = TRUE)))
})

test_that("a .Renviron saved as UTF-16 is reported, not read as having no key", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, ".Renviron")
  txt <- "GEMINI_API_KEY=AIzaSyA1234567890abcdefghijklmnopqrstu\n"
  bytes <- c(as.raw(c(0xff, 0xfe)), iconv(txt, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]])
  writeBin(bytes, path)
  withr::local_envvar(R_ENVIRON_USER = path, GEMINI_API_KEY = NA, GOOGLE_API_KEY = NA)
  utils::capture.output(res <- suppressMessages(check_setup(online = FALSE)))
  row <- res[res$check == "Gemini key", ]
  expect_identical(row$status, "PROBLEM")
  expect_match(row$detail, "UTF-16", fixed = TRUE)
  expect_match(row$fix, "set_key()", fixed = TRUE)
})
