# transcribe() end to end against a fake Gemini (helper-gemini.R): every
# request is built, sent, parsed, checked and saved exactly as with Google.

setup_run <- function(n = 3, handler = NULL, env = parent.frame()) {
  local_amanuensis(env)
  dir <- withr::local_tempdir(.local_envir = env)
  pages <- file.path(dir, "pages")
  make_jpegs(pages, n)
  srv <- fake_gemini(pages, handler)
  httr2::local_mocked_responses(srv$mock, env = env)
  list(dir = dir, pages = pages, out = file.path(dir, "out"), srv = srv)
}

run_extract <- function(s, ..., instructions = test_instructions(), cb = roster_codebook()) {
  quietly(transcribe(s$pages, instructions, cb, save_to = s$out, ...))
}

page_status <- function(res) {
  p <- .result_info(res)$pages
  stats::setNames(p$status, p$file)
}

record_on_disk <- function(s, stem) .read_json(file.path(s$out, paste0(stem, ".json")))

test_that("each page is one generateContent request: image first, instructions, schema, settings", {
  s <- setup_run()
  ins <- test_instructions()
  cb <- roster_codebook()
  res <- quietly(transcribe(s$pages, ins, cb, save_to = s$out))
  expect_identical(s$srv$files_called(), c("p001.jpg", "p002.jpg", "p003.jpg"))
  call <- s$srv$calls[[1]]
  expect_identical(call$url, "https://generativelanguage.googleapis.com/v1beta/models/gemini-3.8-flash:generateContent")
  b <- call$body
  expect_identical(names(b), c("contents", "generationConfig"))
  parts <- b$contents[[1]]$parts
  expect_identical(parts[[1]]$inlineData$mimeType, "image/jpeg")
  expect_identical(parts[[1]]$inlineData$data, .encode_image(file.path(s$pages, "p001.jpg")))
  expect_identical(parts[[1]]$mediaResolution, list(level = "MEDIA_RESOLUTION_ULTRA_HIGH"))
  expect_identical(parts[[2]]$text, ins$full)
  gc <- b$generationConfig
  expect_identical(gc$responseJsonSchema, cb$schema)
  expect_identical(gc$maxOutputTokens, 32768L)
  expect_identical(gc$thinkingConfig, list(thinkingLevel = "LOW", includeThoughts = FALSE))
  expect_null(gc$temperature)
  expect_null(gc$mediaResolution)
  expect_null(b$systemInstruction)
  expect_identical(httr2::req_get_headers(call$req, "reveal")[["x-goog-api-key"]], FAKE_KEY)

  # the table: provenance first, then page variables, then row variables
  expect_s3_class(res, "amanuensis_result")
  expect_identical(names(res), c("file", "source", "source_page", "row", "heading", "name", "age", "sex"))
  expect_identical(nrow(res), 9L)
  expect_identical(unique(res$file), c("p001.jpg", "p002.jpg", "p003.jpg"))
  expect_type(res$age, "integer")
  info <- .result_info(res)
  expect_identical(info$pages$status, rep("ok", 3))
  expect_identical(info$pages$input_tokens, rep(2500, 3))
  expect_identical(info$pages$image_tokens, rep(2240, 3))
  expect_true(all(is.na(info$pages$thinking_tokens)))
  expect_identical(info$folder, .show_path(s$out))
  expect_identical(info$settings$engine, "sync")
})

test_that("records are written one file per page, atomically, with the contract", {
  s <- setup_run()
  dir.create(s$out)
  writeLines("half a record", file.path(s$out, ".amanuensis-leftover"))
  run_extract(s)
  files <- list.files(s$out, all.files = TRUE, no.. = TRUE)
  expect_setequal(files, c("contract.json", "p001.json", "p002.json", "p003.json"))
  rec <- record_on_disk(s, "p002")
  expect_identical(rec$status, "ok")
  expect_identical(rec$file, "p002.jpg")
  expect_identical(rec$model, "gemini-3.8-flash")
  expect_identical(rec$model_version, "gemini-3.8-flash")
  expect_identical(rec$settings$resolution, "ultra_high")
  expect_identical(rec$settings$temperature, "not sent")
  expect_equal(rec$image_bytes, file.size(file.path(s$pages, "p002.jpg")))
  expect_match(rec$date, "Z$")
  expect_identical(.parse_json(rec$reply), rec$data)
  contracts <- .read_contracts(s$out)
  expect_identical(names(contracts), rec$contract)
  expect_identical(contracts[[1]]$instructions, test_instructions()$full)
})

test_that("a second run sends nothing and returns the same table", {
  s <- setup_run()
  res1 <- run_extract(s)
  msgs <- all_condition_text(res2 <- transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out))
  expect_length(s$srv$calls, 3)
  expect_true(any(grepl("Nothing to send", msgs)))
  expect_identical(as.data.frame(res2), as.data.frame(res1))
})

test_that("pages that failed on Google's side are retried on the next run", {
  s <- setup_run(handler = function(file, body, n, req) if (file == "p002.jpg") error_response(503, "Overloaded."))
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out))
  expect_identical(s$srv$count[["p002.jpg"]], 5L)   # 1 + 4 retries
  expect_identical(page_status(res), c(p001.jpg = "ok", p002.jpg = "failed", p003.jpg = "ok"))
  rec <- record_on_disk(s, "p002")
  expect_identical(rec$problem, "server")
  expect_match(rec$message, "server problem \\(HTTP 503\\)")
  expect_true(any(grepl("1 of them will be tried again", msgs)))
  expect_false(any(res$file == "p002.jpg"))
  s$srv$handler <- NULL
  res2 <- run_extract(s)
  expect_identical(s$srv$files_called()[-(1:7)], "p002.jpg")
  expect_identical(unname(page_status(res2)), rep("ok", 3))
  expect_identical(record_on_disk(s, "p002")$attempts, 1L)
})

test_that("a reply cut off at max_tokens is retried only with a larger max_tokens", {
  s <- setup_run(handler = function(file, body, n, req) {
    if (file == "p001.jpg" && body$generationConfig$maxOutputTokens < 20000) {
      gemini_reply(text = '{"heading": "cut', finish = "MAX_TOKENS")
    }
  })
  res <- run_extract(s, max_tokens = 10000)
  expect_identical(record_on_disk(s, "p001")$problem, "truncated")
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out,
                                            max_tokens = 10000))
  expect_identical(s$srv$count[["p001.jpg"]], 1L)
  expect_true(any(grepl("failed earlier and not retried", msgs)))
  expect_true(any(grepl("p001.jpg: cut off at max_tokens = 10000; run again with a larger max_tokens", msgs)))
  res3 <- run_extract(s, max_tokens = 30000)
  expect_identical(s$srv$count[["p001.jpg"]], 2L)
  expect_identical(page_status(res3)[["p001.jpg"]], "ok")
})

test_that("a blocked page is retried once, then held until redo = 'failed'", {
  s <- setup_run(handler = function(file, body, n, req) if (file == "p003.jpg") gemini_reply(text = "", finish = "SAFETY"))
  run_extract(s)
  expect_identical(record_on_disk(s, "p003")$attempts, 1L)
  run_extract(s)
  expect_identical(s$srv$count[["p003.jpg"]], 2L)
  expect_identical(record_on_disk(s, "p003")$attempts, 2L)
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out))
  expect_identical(s$srv$count[["p003.jpg"]], 2L)
  expect_true(any(grepl("tried 2 times; use redo = \"failed\"", msgs)))
  s$srv$handler <- NULL
  res <- run_extract(s, redo = "failed")
  expect_identical(s$srv$count[["p003.jpg"]], 3L)
  expect_identical(page_status(res)[["p003.jpg"]], "ok")
})

test_that("an image Google cannot use is not retried automatically", {
  s <- setup_run(handler = function(file, body, n, req) {
    if (file == "p002.jpg") error_response(400, "Unable to process input image. Please retry.")
  })
  run_extract(s)
  expect_identical(record_on_disk(s, "p002")$problem, "image")
  run_extract(s)
  expect_identical(s$srv$count[["p002.jpg"]], 1L)
})

test_that("a failed page record stays small when Google quotes the request back", {
  echo <- paste0("Invalid value at 'contents[0].parts[0].inline_data.data' (TYPE_BYTES), \"",
                 strrep("QUJDREVG", 200000), "\"")
  s <- setup_run(handler = function(file, body, n, req) {
    if (file == "p002.jpg") error_response(400, echo)
  })
  run_extract(s)
  path <- file.path(s$out, "p002.json")
  expect_lt(file.size(path), 10000)
  rec <- .read_json(path)
  expect_identical(rec$status, "failed")
  expect_lte(nchar(rec$message), 1000)
  expect_match(rec$message, "[...]", fixed = TRUE)
})

test_that("a Base64 decoding error stops the run without writing records", {
  s <- setup_run(handler = function(file, body, n, req) {
    error_response(400, paste0("Invalid value at 'contents[0].parts[0].inline_data.data' (TYPE_BYTES), Base64 decoding failed for \"",
                               strrep("QUJD", 50000), "\""))
  })
  txt <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out))
  expect_identical(s$srv$files_called(), "p001.jpg")
  expect_false(file.exists(file.path(s$out, "p001.json")))
  expect_true(any(grepl("problem in the package", txt, fixed = TRUE)))
  expect_lt(max(nchar(txt)), 2000)
})

test_that("the same image error on three pages in a row stops the run", {
  s <- setup_run(n = 5, handler = function(file, body, n, req) {
    error_response(400, "Unable to process input image. Please retry.")
  })
  txt <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out))
  expect_identical(s$srv$files_called(), c("p001.jpg", "p002.jpg", "p003.jpg"))
  expect_true(any(grepl("3 pages in a row failed the same way", txt, fixed = TRUE)))
})

test_that("an image failure recorded by an older version of the package is retried once", {
  s <- setup_run(handler = function(file, body, n, req) {
    if (file == "p002.jpg" && n == 1) error_response(400, "Unable to process input image. Please retry.")
  })
  run_extract(s)
  path <- file.path(s$out, "p002.json")
  rec <- .read_json(path)
  rec$amanuensis <- "0.1.0"
  .write_json(rec, path)
  res <- run_extract(s)
  expect_identical(s$srv$count[["p002.jpg"]], 2L)
  expect_identical(page_status(res)[["p002.jpg"]], "ok")
})

test_that("changed instructions stop a non-interactive run, naming both ways forward", {
  s <- setup_run()
  run_extract(s)
  new <- test_instructions("These are pages of a 1931 municipal register.")
  err <- expect_error(quietly(transcribe(s$pages, new, roster_codebook(), save_to = s$out)),
                      class = "amanuensis_error_contract")
  msg <- conditionMessage(err)
  expect_match(msg, "3 pages")
  expect_match(msg, "instructions: 2 lines changed")
  expect_match(msg, paste0("save_to = \"", .show_path(s$out), "_2\""), fixed = TRUE)
  expect_match(msg, "redo = \"all\"", fixed = TRUE)
  expect_length(s$srv$calls, 3)
  # a different model is a different setting too
  err2 <- expect_error(quietly(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out,
                                              model = "gemini-3.6-flash")), class = "amanuensis_error_contract")
  expect_match(conditionMessage(err2), "model: gemini-3.8-flash -> gemini-3.6-flash")
})

test_that("redo = 'all' moves the old records to superseded/ and extracts again", {
  s <- setup_run()
  run_extract(s)
  new <- test_instructions("These are pages of a 1931 municipal register.")
  msgs <- all_condition_text(res <- transcribe(s$pages, new, roster_codebook(), save_to = s$out, redo = "all"))
  expect_length(s$srv$calls, 6)
  expect_true(any(grepl("Moved 3 earlier results", msgs)))
  old <- list.files(file.path(s$out, "superseded"), recursive = TRUE)
  expect_setequal(basename(old), c("p001.json", "p002.json", "p003.json"))
  expect_identical(record_on_disk(s, "p001")$contract, .contract(new, roster_codebook())$id)
  expect_length(.read_contracts(s$out), 2)
})

test_that("pages to check are kept apart and redone with redo = 'check'", {
  s <- setup_run(handler = function(file, body, n, req) {
    if (file == "p002.jpg") {
      d <- roster_data(file)
      d$records[[1]]$sex <- "unknown"
      gemini_reply(data = d)
    }
  })
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out))
  expect_identical(page_status(res)[["p002.jpg"]], "check")
  expect_false("p002.jpg" %in% res$file)
  expect_true(any(grepl("1 page to check", msgs)))
  expect_identical(nrow(.result_table(res, "all")), 8L)
  run_extract(s)
  expect_identical(s$srv$count[["p002.jpg"]], 1L)
  s$srv$handler <- NULL
  res3 <- run_extract(s, redo = "check")
  expect_identical(s$srv$count[["p002.jpg"]], 2L)
  expect_identical(s$srv$count[["p001.jpg"]], 1L)
  expect_identical(page_status(res3)[["p002.jpg"]], "ok")
  expect_identical(basename(list.files(file.path(s$out, "superseded"), recursive = TRUE)), "p002.json")
})

test_that("only picks pages by file name, stem, number or benchmark", {
  s <- setup_run()
  res <- run_extract(s, only = "p002.jpg")
  expect_identical(s$srv$files_called(), "p002.jpg")
  expect_identical(unname(page_status(res)), c("not run", "ok", "not run"))
  run_extract(s, only = c(1, 3))
  expect_identical(s$srv$files_called(), c("p002.jpg", "p001.jpg", "p003.jpg"))
  run_extract(s, only = "P002", redo = "P002")
  expect_identical(s$srv$files_called()[4], "p002.jpg")
  g <- .new_gold(data.frame(file = "p003.jpg"), roster_codebook(),
                 pages = data.frame(file = "p003.jpg", done = TRUE, n_rows = 1L))
  run_extract(s, only = g, redo = g)
  expect_identical(s$srv$files_called()[5], "p003.jpg")
  expect_error(run_extract(s, only = "p009.jpg"), class = "amanuensis_error_input", regexp = "not among the images")
  expect_error(run_extract(s, only = 7), class = "amanuensis_error_input", regexp = "no page number 7")
  expect_error(run_extract(s, only = list(1)), class = "amanuensis_error_input")
})

test_that("limit sends at most that many pages and leaves the rest for later", {
  s <- setup_run()
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out, limit = 2))
  expect_identical(s$srv$files_called(), c("p001.jpg", "p002.jpg"))
  expect_true(any(grepl("1 more left for later: limit", msgs)))
  expect_true(any(grepl("The 1 page not run yet would cost about", msgs)))
  run_extract(s, limit = 2)
  expect_identical(s$srv$files_called(), c("p001.jpg", "p002.jpg", "p003.jpg"))
})

test_that("budget stops the run before going over it", {
  s <- setup_run(n = 5)
  # (2500 * 1 + 200 * 2) / 1e6 = 0.0029 per page: two pages fit in 0.007, a third would not
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out,
                                                   budget = 0.007, prices = c(input = 1, output = 2)))
  expect_length(s$srv$calls, 2)
  expect_true(any(grepl("Stopped before going over your budget of \\$0.0070 \\(spent \\$0.0058\\)", msgs)))
  expect_equal(sum(.result_info(res)$pages$cost_usd, na.rm = TRUE), 0.0058)
  expect_error(run_extract(s, budget = 1, model = "gemini-9-ultra"), class = "amanuensis_error_input",
               regexp = "needs prices")
})

test_that("workers > 1 send pages in parallel rounds, retry transient failures and slow down on 429", {
  s <- setup_run(n = 5, handler = function(file, body, n, req) {
    if (file == "p002.jpg" && n == 1) return(error_response(503, "Busy."))
    if (file == "p003.jpg" && n == 1) return(error_response(429, "Slow down.", retry_delay = "1s"))
    if (file == "p004.jpg" && n == 1) return(network_error())
  })
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(),
                                                   save_to = s$out, workers = 4))
  expect_identical(unname(page_status(res)), rep("ok", 5))
  expect_identical(unlist(s$srv$count[c("p001.jpg", "p002.jpg", "p003.jpg", "p004.jpg", "p005.jpg")]),
                   c(p001.jpg = 1L, p002.jpg = 2L, p003.jpg = 2L, p004.jpg = 2L, p005.jpg = 1L))
  slow <- grep("asked to slow down", msgs, value = TRUE)
  expect_length(slow, 1)
  expect_match(slow, "sending 2 pages at a time")
  expect_identical(nrow(res), 15L)
  expect_error(run_extract(s, workers = 9), class = "amanuensis_error_input", regexp = "at most 8")
})

test_that("a refused key stops the run: no record for that page, earlier pages kept", {
  s <- setup_run(handler = function(file, body, n, req) {
    if (file == "p002.jpg") error_response(401, "API key not valid. Please pass a valid API key.")
  })
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(),
                                                   save_to = s$out, log = TRUE))
  expect_identical(s$srv$files_called(), c("p001.jpg", "p002.jpg"))
  expect_true(file.exists(file.path(s$out, "p001.json")))
  expect_false(file.exists(file.path(s$out, "p002.json")))
  expect_identical(unname(page_status(res)), c("ok", "not run", "not run"))
  expect_true(any(grepl("Google refused the request \\(HTTP 401\\).*set_key", msgs)))
  # the key is never printed or written anywhere
  expect_false(any(grepl(FAKE_KEY, msgs, fixed = TRUE)))
  for (f in list.files(s$out, full.names = TRUE, recursive = TRUE)) {
    expect_false(grepl(FAKE_KEY, .read_text(f), fixed = TRUE), info = f)
  }
})

test_that("in a parallel round, a stop keeps the replies already received", {
  s <- setup_run(handler = function(file, body, n, req) {
    if (file == "p001.jpg") error_response(403, "Permission denied.")
  })
  res <- run_extract(s, workers = 2)
  expect_false(file.exists(file.path(s$out, "p001.json")))
  expect_true(file.exists(file.path(s$out, "p002.json")))
  expect_false("p003.jpg" %in% s$srv$files_called())
})

test_that("five server failures in a row stop the run", {
  s <- setup_run(n = 6, handler = function(file, body, n, req) error_response(500, "Internal error."))
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out,
                                            retries = 0))
  expect_length(s$srv$calls, 5)
  expect_true(any(grepl("5 pages in a row failed the same way", msgs)))
  expect_false(file.exists(file.path(s$out, "p006.json")))
})

test_that("practice results are extracted again with a real model", {
  s <- setup_run()
  quietly(transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock", save_to = s$out))
  expect_identical(record_on_disk(s, "p001")$model, "mock")
  expect_length(s$srv$calls, 0)
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out))
  expect_length(s$srv$calls, 3)
  expect_true(any(grepl("3 pages held practice", msgs)))
  expect_identical(record_on_disk(s, "p001")$model, "gemini-3.8-flash")
})

test_that("a page image that changed since extraction is extracted again", {
  s <- setup_run()
  run_extract(s)
  magick::image_write(magick::image_blank(120, 90, "navy"), file.path(s$pages, "p002.jpg"), format = "jpeg")
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out))
  expect_identical(s$srv$files_called()[4], "p002.jpg")
  expect_length(s$srv$calls, 4)
  expect_true(any(grepl("1 page image changed", msgs)))
  expect_identical(basename(list.files(file.path(s$out, "superseded"), recursive = TRUE)), "p002.json")
})

test_that("by default transcribe() writes nothing and asks nothing", {
  s <- setup_run()
  withr::local_options(amanuensis.ask = TRUE)   # any folder question would stop a non-interactive run
  wd <- withr::local_tempdir()
  withr::local_dir(wd)
  listing <- function(d) sort(list.files(d, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, no.. = TRUE))
  before <- listing(s$dir)
  res <- quietly(transcribe(s$pages, test_instructions(), roster_codebook()))
  expect_identical(nrow(res), 9L)
  expect_null(.result_info(res)$folder)
  expect_identical(listing(s$dir), before)
  expect_identical(listing(wd), character())
  # FALSE means the same as leaving save_to out
  res2 <- quietly(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = FALSE, redo = "all"))
  expect_identical(nrow(res2), 9L)
  expect_identical(listing(s$dir), before)
  expect_error(quietly(transcribe(s$pages, log = TRUE)), class = "amanuensis_error_input", regexp = "save_to")
})

test_that("with save_to each page is saved, and the run resumes even after the session forgets", {
  s <- setup_run()
  res <- quietly(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out))
  expect_true(all(file.exists(file.path(s$out, c("p001.json", "p002.json", "p003.json", "contract.json")))))
  expect_identical(.result_info(res)$folder, .show_path(s$out))
  .session_reset()
  res2 <- quietly(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out))
  expect_length(s$srv$calls, 3)
  expect_identical(as.data.frame(res2), as.data.frame(res))
})

test_that("without a folder, the same line run again in this R session sends nothing twice", {
  s <- setup_run()
  .session_reset()
  ins <- test_instructions()
  cb <- roster_codebook()
  res1 <- quietly(transcribe(s$pages, ins, cb))
  msgs <- all_condition_text(res2 <- transcribe(s$pages, ins, cb))
  expect_length(s$srv$calls, 3)
  expect_identical(as.data.frame(res2), as.data.frame(res1))
  expect_true(any(grepl("read earlier in this R session, not sent again", msgs, fixed = TRUE)))
  # other instructions are other settings: every page is read again
  quietly(transcribe(s$pages, test_instructions("Other instructions."), cb))
  expect_length(s$srv$calls, 6)
  # redo = "all" reads them again on purpose, without claiming otherwise
  msgs <- all_condition_text(transcribe(s$pages, ins, cb, redo = "all"))
  expect_length(s$srv$calls, 9)
  expect_false(any(grepl("not sent again", msgs, fixed = TRUE)))
  # another folder holding files of the same names is other pages
  pages2 <- file.path(s$dir, "pages2")
  dir.create(pages2)
  file.copy(list.files(s$pages, full.names = TRUE), pages2)
  quietly(transcribe(pages2, ins, cb))
  expect_length(s$srv$calls, 12)
  # and options(amanuensis.session = FALSE) turns the reuse off
  withr::local_options(amanuensis.session = FALSE)
  quietly(transcribe(s$pages, ins, cb))
  expect_length(s$srv$calls, 15)
})

test_that("Esc without a folder returns the pages read; the same line then reads only the rest", {
  s <- setup_run(handler = function(file, body, n, req) if (file == "p002.jpg" && n == 1) rlang::interrupt())
  .session_reset()
  ins <- test_instructions()
  cb <- roster_codebook()
  msgs <- all_condition_text(out <- withVisible(transcribe(s$pages, ins, cb)))
  expect_false(out$visible)
  expect_identical(unname(page_status(out$value)), c("ok", "not run", "not run"))
  expect_identical(nrow(out$value), 3L)
  expect_true(any(grepl("Stopped by you after 1 of 3 pages", msgs, fixed = TRUE)))
  expect_true(any(grepl("run the same line again", msgs, fixed = TRUE)))
  expect_true(any(grepl(".Last.value", msgs, fixed = TRUE)))
  res <- quietly(transcribe(s$pages, ins, cb))
  expect_identical(s$srv$files_called(), c("p001.jpg", "p002.jpg", "p002.jpg", "p003.jpg"))
  expect_identical(unname(page_status(res)), rep("ok", 3))
})

test_that("an unexpected error keeps the pages already read, with a warning", {
  s <- setup_run()
  .session_reset()
  real <- .record_from_reply
  fail_at <- 2
  calls <- 0
  local_mocked_bindings(.record_from_reply = function(...) {
    calls <<- calls + 1
    if (calls == fail_at) stop("the disk is full")
    real(...)
  })
  msgs <- all_condition_text(out <- transcribe(s$pages, test_instructions(), roster_codebook()))
  expect_identical(unname(page_status(out)), c("ok", "not run", "not run"))
  expect_true(any(grepl("unexpected error", msgs, fixed = TRUE)))
  expect_true(any(grepl("the disk is full", msgs, fixed = TRUE)))
  fail_at <- -1
  res <- quietly(transcribe(s$pages, test_instructions(), roster_codebook()))
  expect_identical(s$srv$files_called(), c("p001.jpg", "p002.jpg", "p002.jpg", "p003.jpg"))
  expect_identical(unname(page_status(res)), rep("ok", 3))
})

test_that("continue = an earlier result carries its pages into a new session, even through readRDS()", {
  s <- setup_run(handler = function(file, body, n, req) if (file == "p002.jpg" && n == 1) rlang::interrupt())
  .session_reset()
  ins <- test_instructions()
  cb <- roster_codebook()
  out <- quietly(transcribe(s$pages, ins, cb))
  rds <- file.path(s$dir, "res.rds")
  saveRDS(out, rds)
  .session_reset()                                   # as if R had been restarted
  msgs <- all_condition_text(res <- transcribe(s$pages, ins, cb, continue = readRDS(rds)))
  expect_identical(s$srv$files_called(), c("p001.jpg", "p002.jpg", "p002.jpg", "p003.jpg"))
  expect_identical(unname(page_status(res)), rep("ok", 3))
  expect_true(any(grepl("1 kept from continue, not sent again", msgs, fixed = TRUE)))
  # a result read with other settings is refused before anything is sent
  expect_error(quietly(transcribe(s$pages, test_instructions("Other instructions."), cb, continue = res)),
               class = "amanuensis_error_contract", regexp = "other settings")
  expect_length(s$srv$calls, 4)
  expect_error(transcribe(s$pages, continue = data.frame(x = 1)), class = "amanuensis_error_input",
               regexp = "earlier result")
})

test_that("pages read without a folder are saved into one given later, not sent again", {
  s <- setup_run(handler = function(file, body, n, req) if (file == "p002.jpg" && n == 1) rlang::interrupt())
  .session_reset()
  ins <- test_instructions()
  cb <- roster_codebook()
  out <- quietly(transcribe(s$pages, ins, cb))
  .session_reset()
  msgs <- all_condition_text(res <- transcribe(s$pages, ins, cb, save_to = s$out, continue = out))
  expect_true(file.exists(file.path(s$out, "p001.json")))
  expect_true(any(grepl("read earlier", msgs, fixed = TRUE)))
  expect_identical(s$srv$files_called(), c("p001.jpg", "p002.jpg", "p002.jpg", "p003.jpg"))
  # this session's pages, too, when the folder comes after a run without one
  s2 <- setup_run()
  .session_reset()
  quietly(transcribe(s2$pages, ins, cb))
  quietly(transcribe(s2$pages, ins, cb, save_to = s2$out))
  expect_length(s2$srv$calls, 3)
  expect_true(all(file.exists(file.path(s2$out, c("p001.json", "p002.json", "p003.json")))))
})

test_that("batch needs a folder; a long run without one suggests save_to", {
  s <- setup_run()
  expect_error(quietly(transcribe(s$pages, batch = TRUE)), class = "amanuensis_error_input",
               regexp = "save_to")
  expect_length(s$srv$calls, 0)
  withr::local_options(amanuensis.long_run = 2)
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook()))
  expect_true(any(grepl("A long run", msgs, fixed = TRUE) & grepl("save_to", msgs, fixed = TRUE)))
  msgs2 <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out, redo = "all"))
  expect_false(any(grepl("A long run", msgs2, fixed = TRUE)))
})

test_that("log = TRUE keeps the run manifest, token usage and error log", {
  s <- setup_run(handler = function(file, body, n, req) if (file == "p003.jpg") error_response(500, "Internal error."))
  run_extract(s, log = TRUE, retries = 0)
  man <- readLines(file.path(s$out, "run_manifest.jsonl"))
  expect_length(man, 1)
  m <- .parse_json(man)
  expect_identical(names(m), c("date", "script", "model", "temperature", "media_resolution", "thinking_level",
                               "max_output_tokens", "prompt_sha256", "schema_sha256", "images", "engine",
                               "schema_channel", "api_args", "r_version"))
  expect_identical(m$model, "gemini-3.8-flash")
  expect_identical(m$temperature, "not sent")
  expect_identical(m$media_resolution, "MEDIA_RESOLUTION_ULTRA_HIGH")
  expect_identical(m$thinking_level, "LOW")
  expect_identical(m$prompt_sha256, test_instructions()$sha12)
  expect_identical(m$schema_sha256, roster_codebook()$sha12)
  expect_identical(m$images, 3L)
  expect_match(m$date, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")
  expect_match(m$script, "^amanuensis ")
  tok <- readLines(file.path(s$out, "token_usage.txt"))
  expect_identical(tok, c("p001.jpg\tinput=2500\tthinking=None\toutput=200\ttotal=2700",
                          "p002.jpg\tinput=2500\tthinking=None\toutput=200\ttotal=2700"))
  err <- readLines(file.path(s$out, "errors.log"))
  expect_length(err, 1)
  expect_match(err, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} p003\\.jpg Google had a server problem \\(HTTP 500\\)")
  # thinking counts are written when Google sends them; logs can go to their own folder
  s$srv$handler <- function(file, body, n, req) gemini_reply(data = roster_data(file), thoughts = 40L)
  logs <- file.path(s$dir, "logs")
  run_extract(s, log = logs)
  expect_identical(readLines(file.path(logs, "token_usage.txt")),
                   "p003.jpg\tinput=2500\tthinking=40\toutput=200\ttotal=2740")
})

test_that("models: aliases and previews are explained, unknown models run without a cost", {
  s <- setup_run()
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = file.path(s$dir, "a"),
                                            model = "gemini-3.7-flash"))
  expect_true(any(grepl("old name: Google sends these requests to .gemini-3.8-flash.", msgs)))
  expect_match(s$srv$calls[[length(s$srv$calls)]]$url, "models/gemini-3.7-flash:generateContent$")
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = file.path(s$dir, "b"),
                                            model = "gemini-3.1-pro-preview"))
  expect_true(any(grepl("preview model", msgs)))
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(),
                                                   save_to = file.path(s$dir, "c"), model = "models/gemini-9-ultra"))
  expect_true(any(grepl("not in this package's model table", msgs)))
  expect_true(all(is.na(.result_info(res)$pages$cost_usd)))
  expect_match(s$srv$calls[[length(s$srv$calls)]]$url, "models/gemini-9-ultra:generateContent$")
})

test_that("settings are checked before anything is sent", {
  s <- setup_run()
  expect_error(run_extract(s, thinking = "minimal"), class = "amanuensis_error", regexp = "does not accept")
  expect_error(run_extract(s, resolution = "max"), class = "amanuensis_error", regexp = "must be one of")
  expect_error(run_extract(s, batch = TRUE, flex = TRUE), class = "amanuensis_error_input", regexp = "not both")
  expect_error(quietly(transcribe(s$pages, batch = TRUE, save_to = FALSE)), class = "amanuensis_error_input",
               regexp = "save_to")
  expect_error(run_extract(s, prices = c(1, 2)), class = "amanuensis_error_input", regexp = "prices")
  expect_error(run_extract(s, api_args = "seed"), class = "amanuensis_error_input", regexp = "list")
  expect_error(run_extract(s, max_tokens = 0), class = "amanuensis_error")
  expect_error(transcribe(), class = "amanuensis_error_input", regexp = "which page images")
  expect_length(s$srv$calls, 0)
  withr::local_envvar(GEMINI_API_KEY = NA)
  expect_error(run_extract(s), class = "amanuensis_error_key", regexp = "No Gemini API key")
})

test_that("max_tokens is capped at the model's limit; temperature, flex and api_args reach the body", {
  s <- setup_run(n = 1, handler = function(file, body, n, req) gemini_reply(data = roster_data(file), model_version = "gemini-4-argon"))
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out,
                                            model = "gemini-4-argon", max_tokens = 65536))
  expect_true(any(grepl("replies with at most 32,768 tokens", msgs)))
  expect_identical(s$srv$calls[[1]]$body$generationConfig$maxOutputTokens, 32768L)
  expect_identical(record_on_disk(s, "p001")$settings$max_tokens, 32768L)

  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), save_to = file.path(s$dir, "t"),
                                                   temperature = 0.2, flex = TRUE, resolution = "high",
                                                   api_args = list(generationConfig = list(seed = 3L))))
  b <- s$srv$calls[[2]]$body
  expect_identical(b$generationConfig$temperature, 0.2)
  expect_identical(b$generationConfig$seed, 3L)
  expect_identical(b$generationConfig$mediaResolution, "MEDIA_RESOLUTION_HIGH")
  expect_identical(b$serviceTier, "flex")
  expect_true(any(grepl("ignore .temperature.", msgs)))
  p <- .result_info(res)$pages
  expect_identical(p$engine, "flex")
  price <- .price_for("gemini-3.8-flash", discount = TRUE)
  expect_equal(p$cost_usd, (2500 * price$input + 200 * price$output) / 1e6)
})

test_that("images can be given as images() output, a folder or files; names must be unique", {
  s <- setup_run(n = 2)
  files <- file.path(s$pages, c("p001.jpg", "p002.jpg"))
  res <- quietly(transcribe(files, test_instructions(), roster_codebook(), save_to = s$out))
  expect_identical(unique(res$file), c("p001.jpg", "p002.jpg"))
  expect_identical(unique(res$source), c("p001.jpg", "p002.jpg"))
  file.copy(files[1], file.path(s$pages, "P001.jpeg"))
  expect_error(quietly(transcribe(s$pages, save_to = s$out)), class = "amanuensis_error_input",
               regexp = "share one results file")
  writeLines("x", file.path(s$dir, "scan.tif"))
  expect_error(quietly(transcribe(file.path(s$dir, "scan.tif"), save_to = s$out)), class = "amanuensis_error_input",
               regexp = "must be prepared first")
  expect_error(quietly(transcribe(file.path(s$dir, "nothing.jpg"))), class = "amanuensis_error_input",
               regexp = "Cannot find")
})

test_that("Esc during a run keeps the finished pages and returns the partial result invisibly", {
  s <- setup_run(handler = function(file, body, n, req) if (file == "p002.jpg") rlang::interrupt())
  msgs <- all_condition_text(out <- withVisible(transcribe(s$pages, test_instructions(), roster_codebook(),
                                                               save_to = s$out)))
  expect_false(out$visible)
  expect_identical(unname(page_status(out$value)), c("ok", "not run", "not run"))
  expect_true(any(grepl("Stopped by you", msgs)))
  expect_true(file.exists(file.path(s$out, "p001.json")))
  expect_false(file.exists(file.path(s$out, "p002.json")))
})

test_that("free text by default, announced", {
  s <- setup_run(n = 1, handler = function(file, body, n, req) gemini_reply(data = list(text = paste("Text of", file))))
  msgs <- all_condition_text(res <- transcribe(s$pages, save_to = s$out))
  expect_true(any(grepl("No instructions given", msgs)))
  expect_true(any(grepl("No codebook given", msgs)))
  expect_identical(names(res), c("file", "source", "source_page", "text"))
  expect_identical(res$text, "Text of p001.jpg")
  expect_identical(s$srv$calls[[1]]$body$contents[[1]]$parts[[2]]$text, instructions()$full)
})

# ------- results never mix settings, and old results move only when replaced --

test_that("practice replies never enter a real model's table, even when limit leaves them behind", {
  s <- setup_run(n = 4, handler = function(file, body, n, req) gemini_reply(data = roster_data(file)))
  quietly(transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock", save_to = s$out))
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(),
                                                   save_to = s$out, limit = 1))
  expect_identical(s$srv$files_called(), "p001.jpg")
  expect_true(any(grepl("4 pages held practice", msgs)))
  expect_true(any(grepl("extracting 1 for real now", msgs)))
  expect_identical(unique(res$file), "p001.jpg")
  expect_false(any(grepl("^name [0-9]$", res$name)))
  expect_identical(unname(page_status(res)), c("ok", "not run", "not run", "not run"))
  p <- .result_info(res)$pages
  expect_match(p$message[2], "practice")
  expect_true(all(is.na(p$model[2:4])))
  expect_equal(sum(p$cost_usd, na.rm = TRUE), record_on_disk(s, "p001")$cost_usd)
  expect_true(any(grepl("3 pages have only practice", msgs)))
  expect_output(print(res), "rows from 1 of 4 pages")
  # the folder gives the same answer
  back <- quietly(.result_from_folder(s$out))
  expect_identical(unique(back$file), "p001.jpg")
})

test_that("after a codebook change, pages not yet redone are left out of the table", {
  s <- setup_run(n = 4)
  run_extract(s)
  cb2 <- quietly(codebook(data.frame(variable = c("heading", "surname", "age", "sex"),
                                     type = c("text", "text", "integer", "category"),
                                     level = c("page", "row", "row", "row"), values = c("", "", "", "male; female"),
                                     description = c("Page heading.", "Surname as written.", "Age in years.", "Sex."))))
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), cb2, save_to = s$out,
                                                   only = 1, redo = "all"))
  expect_identical(unique(res$file), "p001.jpg")
  expect_false("name" %in% names(res))
  expect_identical(unname(page_status(res)), c("ok", "not run", "not run", "not run"))
  expect_match(.result_info(res)$pages$message[2], "other settings \\(other instructions or codebook\\)")
  expect_true(all(vapply(.result_info(res)$records, function(r) r$contract, "") == .contract(test_instructions(), cb2)$id))
  expect_true(any(grepl("3 pages have only results made with other settings", msgs)))
  # redo = "all" with limit: the pages not reached keep their old results on disk
  res2 <- run_extract(s, cb = cb2, redo = "all", limit = 2)
  expect_identical(unname(page_status(res2)), c("ok", "ok", "not run", "not run"))
  expect_false("name" %in% names(res2))
  expect_identical(record_on_disk(s, "p004")$contract, .contract(test_instructions(), roster_codebook())$id)
  moved <- basename(list.files(file.path(s$out, "superseded"), recursive = TRUE))
  # p001 was replaced twice (two runs), p002 once; a second copy in the same
  # second's folder gets a suffix rather than overwriting the first
  expect_identical(sort(sub("-[0-9]+\\.json$", ".json", moved)), c("p001.json", "p001.json", "p002.json"))
})

test_that("redo stopped by the budget keeps every old result in the folder until it is replaced", {
  s <- setup_run()
  run_extract(s)
  # 0.0029 per page at these prices: the second page would go over 0.004
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out,
                                                   redo = "all", budget = 0.004, prices = c(input = 1, output = 2)))
  expect_length(s$srv$calls, 4)
  expect_true(any(grepl("Stopped before going over your budget", msgs)))
  expect_true(any(grepl("Moved 1 earlier result", msgs)))
  expect_true(any(grepl("2 pages to redo were not reached and keep their earlier results", msgs)))
  expect_true(all(file.exists(file.path(s$out, c("p001.json", "p002.json", "p003.json")))))
  expect_identical(basename(list.files(file.path(s$out, "superseded"), recursive = TRUE)), "p001.json")
  back <- .result_from_folder(s$out)
  expect_identical(as.data.frame(back), as.data.frame(res))
  # with new instructions, the pages not reached are not shown as part of the new result
  new <- test_instructions("These are pages of a 1931 municipal register.")
  res2 <- quietly(transcribe(s$pages, new, roster_codebook(), save_to = s$out, redo = "all", budget = 0.004,
                                 prices = c(input = 1, output = 2)))
  expect_identical(unname(page_status(res2)), c("ok", "not run", "not run"))
  expect_identical(unique(res2$file), "p001.jpg")
  expect_true(all(file.exists(file.path(s$out, c("p002.json", "p003.json")))))
})

test_that("a batch refused by the budget moves nothing aside", {
  s <- setup_run()
  run_extract(s)
  expect_error(quietly(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out,
                                      redo = "all", batch = TRUE, budget = 1e-6)),
               class = "amanuensis_error_input")
  expect_true(all(file.exists(file.path(s$out, c("p001.json", "p002.json", "p003.json")))))
  expect_false(dir.exists(file.path(s$out, "superseded")))
  expect_identical(nrow(.result_from_folder(s$out)), 9L)
})

test_that("Esc during a parallel round stops the run after keeping that round's replies", {
  s <- setup_run(n = 12)
  testthat::local_mocked_bindings(
    req_perform_parallel = function(reqs, ...) {
      out <- lapply(reqs, function(r) httr2::req_perform(r))
      # what httr2 does after catching Esc: wait for the requests in flight,
      # warn, and return normally
      cli::cli_alert_warning("Terminating iteration; returning {length(out)} responses.")
      out
    },
    .package = "httr2")
  msgs <- all_condition_text(out <- withVisible(transcribe(s$pages, test_instructions(), roster_codebook(),
                                                               save_to = s$out, workers = 4)))
  expect_length(s$srv$calls, 4)
  expect_false(out$visible)
  expect_identical(unname(page_status(out$value)), c(rep("ok", 4), rep("not run", 8)))
  expect_true(any(grepl("Stopped by you", msgs)))
  expect_false(any(grepl("Terminating iteration", msgs)))
  expect_length(list.files(s$out, pattern = "^p[0-9]+\\.json$"), 4)
})

test_that("results kept apart in <folder>_2 are continued there by the same line", {
  s <- setup_run(n = 4)
  run_extract(s)
  new <- test_instructions("These are pages of a 1931 municipal register.")
  alt <- paste0(s$out, "_2")
  # what choosing "keep the old results apart" does, stopped after two pages
  quietly(transcribe(s$pages, new, roster_codebook(), save_to = alt, limit = 2))
  expect_length(s$srv$calls, 6)
  msgs <- all_condition_text(res <- transcribe(s$pages, new, roster_codebook(), save_to = s$out))
  expect_true(any(grepl("Continuing in", msgs)))
  expect_identical(s$srv$files_called()[7:8], c("p003.jpg", "p004.jpg"))
  expect_length(s$srv$calls, 8)
  expect_false(dir.exists(paste0(s$out, "_3")))
  expect_false(dir.exists(file.path(s$out, "superseded")))
  expect_identical(record_on_disk(s, "p001")$contract, .contract(test_instructions(), roster_codebook())$id)
  expect_identical(.result_info(res)$folder, .show_path(alt))
  expect_identical(unname(page_status(res)), rep("ok", 4))
  # finished: the same line again sends nothing
  res2 <- run_extract(s, instructions = new)
  expect_length(s$srv$calls, 8)
  expect_identical(nrow(res2), 12L)
  # settings no folder holds yet: the hint names a fresh folder
  newer <- test_instructions("These are pages of a 1932 municipal register.")
  err <- expect_error(quietly(transcribe(s$pages, newer, roster_codebook(), save_to = s$out)),
                      class = "amanuensis_error_contract")
  expect_match(conditionMessage(err), paste0("save_to = \"", .show_path(s$out), "_3\""), fixed = TRUE)
})

test_that("a batch job kept apart in <folder>_2 is collected by the same line, not sent again", {
  local_amanuensis()
  dir <- withr::local_tempdir()
  pages <- file.path(dir, "pages")
  make_jpegs(pages, 3)
  out <- file.path(dir, "out")
  quietly(transcribe(pages, test_instructions(), roster_codebook(), model = "mock", save_to = out))
  new <- test_instructions("These are pages of a 1931 municipal register.")
  quietly(transcribe(pages, new, roster_codebook(), model = "mock", batch = TRUE, wait = FALSE,
                         save_to = paste0(out, "_2")))
  n_jobs <- .mock_state$job
  res <- quietly(transcribe(pages, new, roster_codebook(), model = "mock", batch = TRUE, save_to = out))
  expect_identical(.mock_state$job, n_jobs)
  expect_identical(unname(page_status(res)), rep("ok", 3))
  expect_false(dir.exists(paste0(out, "_3")))
})

test_that("an old model name and the model it routes to count as the same model", {
  s <- setup_run()
  a <- file.path(s$dir, "a")
  quietly(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = a, model = "gemini-3.7-flash"))
  res <- quietly(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = a, model = "gemini-3.8-flash"))
  expect_length(s$srv$calls, 3)
  expect_identical(unname(page_status(res)), rep("ok", 3))
  b <- file.path(s$dir, "b")
  quietly(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = b, model = "gemini-3.8-flash"))
  quietly(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = b, model = "gemini-3.7-flash"))
  expect_length(s$srv$calls, 6)
  c <- file.path(s$dir, "c")
  quietly(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = c, model = "gemini-3.5-flash"))
  quietly(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = c, model = "gemini-3.6-flash"))
  expect_length(s$srv$calls, 9)
  # a really different model is still a change
  err <- expect_error(quietly(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = a,
                                             model = "gemini-3.6-flash")), class = "amanuensis_error_contract")
  expect_match(conditionMessage(err), "model: gemini-3.7-flash -> gemini-3.6-flash")
})

test_that("the cost projection after batch pages is at the standard price, halved once for batch", {
  local_amanuensis()
  dir <- withr::local_tempdir()
  pages <- file.path(dir, "pages")
  make_jpegs(pages, 10)
  out <- file.path(dir, "out")
  msgs <- all_condition_text(transcribe(pages, test_instructions(), roster_codebook(), model = "mock",
                                            batch = TRUE, save_to = out, limit = 2))
  recs <- .read_records(out)
  p <- .price_for("gemini-3.8-flash")
  per <- mean(vapply(recs, function(r) .cost(list(input = r$tokens$input, cached = 0, output = r$tokens$billed_output), p), 1))
  line <- grep("8 pages not run yet would cost", msgs, value = TRUE)
  expect_length(line, 1)
  expect_match(line, paste0("about ", .fmt_usd(per * 8 / 2), " with batch"), fixed = TRUE)
  expect_match(line, paste0(.fmt_usd(per * 8), " one page at a time"), fixed = TRUE)
})

test_that("batch pages collected from an earlier call do not count towards this run's budget", {
  local_price_date("2026-11-08")
  local_amanuensis()
  dir <- withr::local_tempdir()
  pages <- file.path(dir, "pages")
  make_jpegs(pages, 6)
  out <- file.path(dir, "out")
  quietly(transcribe(pages, test_instructions(), roster_codebook(), model = "mock", batch = TRUE, wait = FALSE,
                         save_to = out, limit = 4))
  quietly(.poll_once(.new_run(NULL, NULL, out, NULL, NULL), "mock-key"))
  # one sync page costs about $0.0013: the two pages left fit in $0.003, the
  # four collected batch pages (paid when the job ran) would not
  msgs <- all_condition_text(res <- transcribe(pages, test_instructions(), roster_codebook(), model = "mock",
                                                   save_to = out, budget = 0.003))
  expect_false(any(grepl("Stopped before going over your budget", msgs)))
  expect_identical(unname(page_status(res)), rep("ok", 6))
  expect_true(any(grepl("Sent 2 pages to mock", msgs)))
  expect_true(any(grepl("collected 4 pages from a batch job sent earlier", msgs)))
})

test_that("the summary says what this run did, not a success it did not have", {
  s <- setup_run(handler = function(file, body, n, req) error_response(400, "API key not valid. Please pass a valid API key."))
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out))
  summary <- grep("No page was read", msgs, value = TRUE)
  expect_length(summary, 1)
  expect_match(summary, "^x ")
  expect_match(summary, "API key")
  expect_false(any(grepl("Read 3 pages", msgs)))
  expect_false(any(grepl("saves the table", msgs)))
  s$srv$handler <- NULL
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out, only = 2))
  expect_true(any(grepl("^v Sent 1 page to gemini-3.8-flash", msgs)))
  expect_true(any(grepl("Of the 3 pages: 1 ok, 2 not run yet", msgs)))
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out, only = 2))
  expect_true(any(grepl("^v Nothing to send", msgs)))
  # a batch sent without waiting: its pages are waiting, not "not run"
  b <- file.path(s$dir, "b")
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock",
                                            batch = TRUE, wait = FALSE, save_to = b))
  expect_true(any(grepl("3 pages waiting in a batch job", msgs)))
  expect_false(any(grepl("not run yet", msgs)))
  # collected later: those pages were sent by the earlier call
  msgs <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock",
                                            batch = TRUE, save_to = b))
  expect_true(any(grepl("^v Collected 3 pages from a batch job sent earlier", msgs)))
  expect_false(any(grepl("Sent 3 pages", msgs)))
})

test_that("redo = 'all' with limit warns that it starts from the top each time", {
  s <- setup_run(n = 3)
  run_extract(s)
  txt <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), save_to = s$out,
                                           redo = "all", limit = 1))
  expect_true(any(grepl("redoes the first 1 page", txt, fixed = TRUE)))
})
