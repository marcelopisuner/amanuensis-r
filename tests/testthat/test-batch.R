# The Batch API cycle: with model = "mock" (the package's own offline server)
# and with a real model name against fake_batch() (helper-gemini.R), which can
# return per-line errors, expired jobs and shuffled results.

batch_setup <- function(n = 3, env = parent.frame()) {
  local_amanuensis(env)
  dir <- withr::local_tempdir(.local_envir = env)
  pages <- file.path(dir, "pages")
  make_jpegs(pages, n)
  list(dir = dir, pages = pages, out = file.path(dir, "out"))
}

batch_extract <- function(s, ..., model = "mock", batch = TRUE) {
  quietly(transcribe(s$pages, test_instructions(), roster_codebook(), model = model, batch = batch,
                         save_to = s$out, ...))
}

statuses <- function(res) {
  p <- .result_info(res)$pages
  stats::setNames(p$status, p$file)
}

test_that("model = 'mock' runs the whole batch cycle: upload, create, poll, download, collect", {
  s <- batch_setup()
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock",
                                                   batch = TRUE, save_to = s$out))
  expect_true(any(grepl("Sending 3 pages as a batch", msgs)))
  expect_true(any(grepl("Batch job 1 created \\(batches/mock-[0-9]+\\): 3 pages", msgs)))
  expect_true(any(grepl("Collected batch job batches/mock-[0-9]+: 3 of 3 pages returned", msgs)))
  expect_identical(unname(statuses(res)), rep("ok", 3))
  expect_identical(nrow(res), 9L)
  expect_false(file.exists(file.path(s$out, "batch-jobs.json")))
  rec <- .read_json(file.path(s$out, "p001.json"))
  expect_identical(rec$settings$engine, "batch")
  expect_match(rec$batch_job, "^batches/mock-")
  expect_identical(rec$settings$schema_channel, "openapi")
  expect_true(rec$price$discount)
  # half the price of the same reply sent one page at a time
  p <- .price_for("gemini-3.8-flash", discount = TRUE)
  expect_equal(rec$cost_usd, (rec$tokens$input * p$input + rec$tokens$billed_output * p$output) / 1e6)
  # nothing left to do
  msgs2 <- all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock",
                                             batch = TRUE, save_to = s$out))
  expect_true(any(grepl("Nothing to send", msgs2)))
})

test_that("a job sent with wait = FALSE is collected by a later call, even with batch = FALSE", {
  s <- batch_setup()
  msgs <- all_condition_text(r1 <- transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock",
                                                  batch = TRUE, wait = FALSE, save_to = s$out))
  expect_true(any(grepl("Run the same line again later to collect", msgs)))
  jobs <- .read_jobs(s$out)
  expect_length(jobs, 1)
  expect_identical(jobs[[1]]$state, "submitted")
  expect_match(jobs[[1]]$name, "^batches/mock-")
  expect_identical(vapply(jobs[[1]]$pages, function(p) p$file, ""), c("p001.jpg", "p002.jpg", "p003.jpg"))
  expect_identical(jobs[[1]]$contract, .contract(test_instructions(), roster_codebook())$id)
  expect_identical(unname(statuses(r1)), rep("not run", 3))

  # still running: its pages are not sent again
  msgs <- all_condition_text(r2 <- transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock",
                                                  save_to = s$out))
  expect_true(any(grepl("1 batch job recorded", msgs)))
  expect_true(any(grepl("running, 0 of 3 pages done", msgs)))
  expect_true(any(grepl("3 in a pending batch job", msgs)))
  expect_false(any(file.exists(file.path(s$out, c("p001.json", "p002.json", "p003.json")))))
  expect_length(.read_jobs(s$out), 1)

  # finished: collected, and stamped as batch results
  r3 <- quietly(transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock", save_to = s$out))
  expect_identical(unname(statuses(r3)), rep("ok", 3))
  engines <- vapply(c("p001", "p002", "p003"), function(f) .read_json(file.path(s$out, paste0(f, ".json")))$settings$engine, "")
  expect_identical(unname(engines), rep("batch", 3))
  expect_false(file.exists(file.path(s$out, "batch-jobs.json")))
})

test_that("batch_size splits the pages into several jobs", {
  s <- batch_setup(n = 3)
  res <- batch_extract(s, batch_size = 2)
  expect_identical(unname(statuses(res)), rep("ok", 3))
  jobs <- vapply(c("p001", "p002", "p003"), function(f) .read_json(file.path(s$out, paste0(f, ".json")))$batch_job, "")
  expect_identical(jobs[[1]], jobs[[2]])
  expect_false(identical(jobs[[1]], jobs[[3]]))
})

test_that("a 'submitting' entry is recovered by its display name", {
  s <- batch_setup()
  batch_extract(s, wait = FALSE)
  jobs <- .read_jobs(s$out)
  name <- jobs[[1]]$name
  jobs[[1]]["name"] <- list(NULL)
  jobs[[1]]$state <- "submitting"
  .write_jobs(s$out, jobs)
  n_jobs <- .mock_state$job
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock",
                                                   batch = TRUE, save_to = s$out))
  expect_true(any(grepl(paste0("Found batch job ", name, ", created just before an earlier run stopped"), msgs, fixed = TRUE)))
  expect_identical(.mock_state$job, n_jobs)   # no second job was created
  expect_identical(unname(statuses(res)), rep("ok", 3))
})

test_that("a 'submitting' entry Google never created is dropped and its pages sent again", {
  s <- batch_setup()
  srv <- fake_batch()
  httr2::local_mocked_responses(srv$mock)
  dir.create(s$out)
  entry <- list(name = NULL, display_name = "amanuensis-lost-1", state = "submitting", input_file = "files/input-0",
                submitted = .utc_now(), contract = .contract(test_instructions(), roster_codebook())$id,
                settings = list(model = "gemini-3.8-flash"),
                pages = list(list(file = "p001.jpg", source = "p001.jpg", source_page = 1L)))
  .write_jobs(s$out, list(entry))
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(),
                                                   model = "gemini-3.8-flash", batch = TRUE, save_to = s$out))
  expect_true("files/input-0" %in% srv$deleted)
  expect_identical(unname(statuses(res)), rep("ok", 3))
  expect_false(file.exists(file.path(s$out, "batch-jobs.json")))
})

test_that("batch requests are snake_case JSONL lines keyed by file, and the input file is deleted", {
  s <- batch_setup(n = 2)
  srv <- fake_batch(states = c("PENDING", "RUNNING", "SUCCEEDED"))
  httr2::local_mocked_responses(srv$mock)
  res <- batch_extract(s, model = "gemini-3.8-flash")
  expect_identical(unname(statuses(res)), c("ok", "ok"))
  expect_identical(srv$polls, 3L)
  lines <- strsplit(srv$uploads[["files/input-1"]], "\n", fixed = TRUE)[[1]]
  expect_length(lines, 2)
  first <- .parse_json(lines[1])
  expect_identical(names(first), c("key", "request"))
  expect_identical(first$key, "p001.jpg")
  req <- first$request
  expect_identical(names(req), c("contents", "generation_config"))
  expect_identical(names(req$contents[[1]]$parts[[1]]), c("inline_data", "media_resolution"))
  expect_identical(req$contents[[1]]$parts[[1]]$inline_data$mime_type, "image/jpeg")
  expect_identical(req$contents[[1]]$parts[[2]]$text, test_instructions()$full)
  gc <- req$generation_config
  expect_identical(gc$thinking_config, list(thinking_level = "LOW", include_thoughts = FALSE))
  expect_identical(gc$response_schema$type, "OBJECT")
  expect_identical(gc$response_schema$propertyOrdering, list("heading", "records"))
  expect_identical(gc$response_schema$properties$records$items$properties$sex$enum, list("male", "female", "other"))
  expect_true(gc$response_schema$properties$records$items$properties$sex$nullable)
  expect_null(gc$response_json_schema)
  created <- srv$created[[1]]$batch
  expect_identical(created$model, "models/gemini-3.8-flash")
  expect_identical(created$inputConfig$fileName, "files/input-1")
  expect_match(created$displayName, "^amanuensis-[0-9a-f]{8}-[0-9]{14}-1$")
  expect_identical(srv$deleted, "files/input-1")
  expect_identical(sub(" .*", "", srv$calls), c("POST", "POST", "POST", "GET", "GET", "GET", "GET", "DELETE"))
})

test_that("the JSON-schema channel can be chosen for batch with an option", {
  s <- batch_setup(n = 1)
  withr::local_options(amanuensis.batch_schema = "json")
  srv <- fake_batch()
  httr2::local_mocked_responses(srv$mock)
  res <- batch_extract(s, model = "gemini-3.8-flash")
  req <- .parse_json(srv$uploads[[1]])$request
  expect_identical(req$generation_config$response_json_schema, roster_codebook()$schema)
  expect_identical(.read_json(file.path(s$out, "p001.json"))$settings$schema_channel, "json")
})

test_that("results are matched by key when the results file comes back in another order", {
  s <- batch_setup(n = 3)
  srv <- fake_batch(shuffle = TRUE, reply = function(key, request) {
    if (key == "p003.jpg") return(list(key = "someone-else.jpg", response = gemini_reply(data = roster_data(key))))
    list(key = toupper(key), response = gemini_reply(data = roster_data(key)))
  })
  httr2::local_mocked_responses(srv$mock)
  res <- batch_extract(s, model = "gemini-3.8-flash")
  expect_identical(statuses(res), c(p001.jpg = "ok", p002.jpg = "ok", p003.jpg = "failed"))
  expect_identical(unique(res$heading[res$file == "p001.jpg"]), "Heading of p001.jpg")
  expect_identical(unique(res$heading[res$file == "p002.jpg"]), "Heading of p002.jpg")
  expect_match(.read_json(file.path(s$out, "p003.json"))$message, "No reply in Google's results file")
})

test_that("a per-line error fails that page only; it is sent again on the next run", {
  s <- batch_setup(n = 3)
  srv <- fake_batch(reply = function(key, request) {
    if (key == "p002.jpg") list(key = key, error = list(code = 500L, message = "Internal error encountered."))
  })
  httr2::local_mocked_responses(srv$mock)
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(),
                                                   model = "gemini-3.8-flash", batch = TRUE, save_to = s$out))
  expect_identical(statuses(res), c(p001.jpg = "ok", p002.jpg = "failed", p003.jpg = "ok"))
  rec <- .read_json(file.path(s$out, "p002.json"))
  expect_identical(rec$problem, "batch")
  expect_identical(rec$message, "Internal error encountered.; the page will be sent again.")
  expect_true(any(grepl("1 of them will be tried again", msgs)))
  res2 <- batch_extract(s, model = "gemini-3.8-flash")
  lines <- strsplit(srv$uploads[["files/input-2"]], "\n", fixed = TRUE)[[1]]
  expect_identical(vapply(lines, function(l) .parse_json(l)$key, "", USE.NAMES = FALSE), "p002.jpg")
})

test_that("an EXPIRED or FAILED job turns its pages into failed records", {
  s <- batch_setup(n = 2)
  srv <- fake_batch(states = "EXPIRED")
  httr2::local_mocked_responses(srv$mock)
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(),
                                                   model = "gemini-3.8-flash", batch = TRUE, save_to = s$out))
  expect_identical(unname(statuses(res)), c("failed", "failed"))
  rec <- .read_json(file.path(s$out, "p001.json"))
  expect_identical(rec$problem, "batch")
  expect_match(rec$message, "within 48 hours; the page will be sent again")
  expect_true(any(grepl("ended as EXPIRED", msgs)))
  expect_identical(srv$deleted, "files/input-1")
  expect_false(file.exists(file.path(s$out, "batch-jobs.json")))

  s2 <- batch_setup(n = 1)
  srv2 <- fake_batch(states = "FAILED")
  httr2::local_mocked_responses(srv2$mock)
  res <- batch_extract(s2, model = "gemini-3.8-flash")
  expect_match(.read_json(file.path(s2$out, "p001.json"))$message, "the batch job failed \\(internal batch failure\\)")
})

test_that("a full batch queue stops submitting and removes the job entry", {
  s <- batch_setup(n = 2)
  srv <- fake_batch(create_error = error_response(429, "Too many enqueued tokens."))
  httr2::local_mocked_responses(srv$mock)
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(),
                                                   model = "gemini-3.8-flash", batch = TRUE, save_to = s$out))
  expect_true(any(grepl("batch queue for your account is full; 2 page\\(s\\) were not sent", msgs)))
  expect_false(file.exists(file.path(s$out, "batch-jobs.json")))
  expect_identical(srv$deleted, "files/input-1")
  # any other refusal at creation is an error naming what to do
  srv2 <- fake_batch(create_error = error_response(403, "Batch API requires billing."))
  httr2::local_mocked_responses(srv2$mock)
  expect_error(batch_extract(s, model = "gemini-3.8-flash"), class = "amanuensis_api_error", regexp = "billing")
  expect_false(file.exists(file.path(s$out, "batch-jobs.json")))
})

test_that("a batch that would cost more than the budget is not sent", {
  s <- batch_setup(n = 3)
  srv <- fake_batch()
  httr2::local_mocked_responses(srv$mock)
  expect_error(batch_extract(s, model = "gemini-3.8-flash", budget = 0.000001), class = "amanuensis_error_input",
               regexp = "could cost up to")
  expect_length(srv$calls, 0)
  # measured on earlier pages of the folder: checked against that estimate
  quietly(transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock", save_to = s$out, limit = 2))
  expect_error(transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock", batch = TRUE,
                              save_to = s$out, budget = 0.000001, quiet = TRUE),
               class = "amanuensis_error_input", regexp = "would cost about .*measured on earlier pages")
})

test_that("before any page is measured, a batch budget covers the most the batch could cost", {
  # the example's light pages cost $0.0062 at batch prices; the input-only
  # estimate ($0.0040) let a budget of $0.0045 through and the batch overspent
  local_amanuensis()
  dir <- withr::local_tempdir()
  ex <- quietly(example_project(file.path(dir, "ex")))
  pages <- quietly(images(ex$scans, save_to = file.path(dir, "pages")))
  out <- file.path(dir, "out")
  n_jobs <- .mock_state$job %||% 0L
  err <- expect_error(quietly(transcribe(pages, ex$instructions, ex$codebook, model = "example", batch = TRUE,
                                             budget = 0.0045, save_to = out)),
                      class = "amanuensis_error_input", regexp = "could cost up to")
  expect_match(conditionMessage(err), "limit = 5", fixed = TRUE)
  expect_match(conditionMessage(err), "cannot be stopped once it is sent", fixed = TRUE)
  expect_false(file.exists(file.path(out, "batch-jobs.json")))
  expect_identical(.mock_state$job %||% 0L, n_jobs)   # nothing was created
  # a budget that covers the ceiling is sent, and the spend stays within it
  msgs <- all_condition_text(res <- transcribe(pages, ex$instructions, ex$codebook, model = "example", batch = TRUE,
                                                   budget = 0.5, save_to = out))
  expect_true(any(grepl("would cost at most \\$0\\.[0-9]+, within your budget of \\$0\\.50", msgs)))
  expect_lte(sum(.result_info(res)$pages$cost_usd), 0.5)
})

test_that("a batch that costs more than its budget, measured on earlier pages, says so", {
  local_price_date("2026-11-08")
  s <- batch_setup(n = 4)
  srv <- fake_batch(reply = function(key, request) {
    list(key = key, response = gemini_reply(data = roster_data(key), output = if (key %in% c("p001.jpg", "p002.jpg")) 10L else 20000L))
  })
  httr2::local_mocked_responses(srv$mock)
  quietly(transcribe(s$pages, test_instructions(), roster_codebook(), model = "gemini-3.8-flash", batch = TRUE,
                         save_to = s$out, limit = 2))
  # two light pages measured; the two left are dense
  w <- NULL
  withCallingHandlers(quietly(transcribe(s$pages, test_instructions(), roster_codebook(), model = "gemini-3.8-flash",
                                             batch = TRUE, save_to = s$out, budget = 0.01)),
                      warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") })
  expect_true(any(grepl("cost \\$0\\.0[0-9]+, more than your budget of \\$0\\.01", w)))
  expect_true(any(grepl("cannot be stopped once it is sent", w)))
})

test_that("cancel_batch() cancels recorded jobs; their pages are sent again later", {
  s <- batch_setup()
  batch_extract(s, wait = FALSE)
  name <- .read_jobs(s$out)[[1]]$name
  msgs <- all_condition_text(done <- cancel_batch(s$out))
  expect_true(any(grepl("Asked Google to cancel 1 job", msgs)))
  # no "run it to record the result": the next run sends the pages again
  expect_true(any(grepl("records the cancellation and sends the 3 cancelled pages again, paid again", msgs)))
  expect_false(any(grepl("only if you run it", msgs)))
  expect_identical(done, name)
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock",
                                                   save_to = s$out))
  expect_true(any(grepl("ended as CANCELLED", msgs)))
  expect_true(any(grepl("go back on the list of pages to send, and are paid again when they are sent", msgs)))
  expect_false(any(grepl("when you run this line", msgs)))
  expect_identical(unname(statuses(res)), rep("ok", 3))
  rec <- .read_json(file.path(s$out, "p001.json"))
  expect_identical(rec$settings$engine, "sync")
  expect_message(cancel_batch(s$out), "No pending batch jobs")
  expect_error(cancel_batch(file.path(s$dir, "nowhere")), class = "amanuensis_error_input", regexp = "Cannot find")
})

test_that("Google forgetting a job (404) means its pages are sent again", {
  s <- batch_setup(n = 1)
  dir.create(s$out)
  .write_jobs(s$out, list(list(name = "batches/mock-999999", display_name = "x", state = "submitted",
                               contract = "c", settings = list(model = "mock"),
                               pages = list(list(file = "p001.jpg", source = "p001.jpg", source_page = 1L)))))
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock",
                                                   save_to = s$out))
  expect_true(any(grepl("no longer knows batch job batches/mock-999999", msgs)))
  expect_identical(unname(statuses(res)), "ok")
})

test_that(".batch_estimate measures earlier pages when it can", {
  s <- batch_setup(n = 2)
  quietly(transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock", save_to = s$out))
  run <- .new_run(list(model = "mock", resolution = "high", prices = NULL), .contract(test_instructions(), roster_codebook()),
                  s$out, NULL, NULL)
  run$records <- .read_records(s$out)
  est <- .batch_estimate(run, 10)
  expect_match(est$note, "measured on earlier pages")
  run$records <- list()
  est2 <- .batch_estimate(run, 10)
  expect_match(est2$note, "input alone")
  run$settings$model <- "gemini-9-ultra"
  expect_true(is.na(.batch_estimate(run, 10)$cost))
})

test_that("practice jobs never reach the network, even when a lost job is cleaned up", {
  s <- batch_setup(n = 1)
  outside <- character()
  httr2::local_mocked_responses(function(req) {
    outside <<- c(outside, req$url)
    httr2::response_json(status_code = 404, body = list(error = list(message = "no")))
  })
  dir.create(s$out)
  .write_jobs(s$out, list(list(name = NULL, display_name = "amanuensis-never-made", state = "submitting",
                               input_file = "files/mock-424242", contract = "c", settings = list(model = "mock"),
                               pages = list(list(file = "p001.jpg", source = "p001.jpg", source_page = 1L)))))
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock",
                                                   batch = TRUE, save_to = s$out))
  expect_true(any(grepl("Google has no record of it", msgs)))
  expect_identical(unname(statuses(res)), "ok")
  expect_length(outside, 0)
})

# ------- prices, unclear creation, file size, lost results, encoding ----------

test_that("batch = TRUE with your own prices collects every page, costed at those prices", {
  s <- batch_setup()
  res <- batch_extract(s, prices = c(input = 1, output = 2))
  expect_identical(unname(statuses(res)), rep("ok", 3))
  # sent with wait = FALSE and collected by later calls without prices
  s2 <- batch_setup()
  batch_extract(s2, wait = FALSE, prices = c(input = 1, output = 2))
  expect_equal(.read_jobs(s2$out)[[1]]$settings$prices, list(input = 1, output = 2))   # names kept
  quietly(transcribe(s2$pages, test_instructions(), roster_codebook(), model = "mock", save_to = s2$out))
  res2 <- quietly(transcribe(s2$pages, test_instructions(), roster_codebook(), model = "mock", save_to = s2$out))
  expect_identical(unname(statuses(res2)), rep("ok", 3))
  expect_false(file.exists(file.path(s2$out, "batch-jobs.json")))
  for (f in c("p001", "p002", "p003")) {
    rec <- .read_json(file.path(s2$out, paste0(f, ".json")))
    expect_length(rec$cost_usd, 1)
    expect_equal(rec$cost_usd, (rec$tokens$input * 0.5 + rec$tokens$billed_output * 1) / 1e6)
  }
})

test_that("prices an older version stored without their names still give each page its cost", {
  s <- batch_setup()
  batch_extract(s, wait = FALSE)
  jobs <- .read_jobs(s$out)
  jobs[[1]]$settings$prices <- list(1, 2)   # what 0.1.1 wrote for c(input = 1, output = 2)
  .write_jobs(s$out, jobs)
  quietly(transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock", save_to = s$out))
  res <- quietly(transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock", save_to = s$out))
  expect_identical(unname(statuses(res)), rep("ok", 3))
  expect_false(file.exists(file.path(s$out, "batch-jobs.json")))
  rec <- .read_json(file.path(s$out, "p002.json"))
  expect_equal(rec$cost_usd, (rec$tokens$input * 0.5 + rec$tokens$billed_output * 1) / 1e6)
})

test_that("an unclear answer when a job is created keeps it recorded; the next run finds it instead of paying twice", {
  for (lost_reply in list(error_response(504, "Deadline exceeded."),
                          network_error("Timeout was reached after 300000 milliseconds"))) {
    s <- batch_setup()
    srv <- fake_batch()
    lost <- TRUE
    httr2::local_mocked_responses(function(req) {
      out <- srv$mock(req)   # Google creates the job ...
      if (lost && grepl(":batchGenerateContent$", req$url)) {
        lost <<- FALSE
        return(lost_reply)   # ... but its answer does not arrive
      }
      out
    })
    expect_error(batch_extract(s, model = "gemini-3.8-flash", wait = FALSE), class = "amanuensis_error",
                 regexp = "may or may not exist")
    jobs <- .read_jobs(s$out)
    expect_length(jobs, 1)
    expect_identical(jobs[[1]]$state, "submitting")
    expect_identical(jobs[[1]]$input_file, "files/input-1")
    expect_length(srv$deleted, 0)
    res <- batch_extract(s, model = "gemini-3.8-flash")
    expect_length(srv$created, 1)   # no second job
    expect_identical(unname(statuses(res)), rep("ok", 3))
    expect_false(file.exists(file.path(s$out, "batch-jobs.json")))
  }
  # a clear refusal (4xx) means no job: the entry and the uploaded file go
  s <- batch_setup(n = 1)
  srv <- fake_batch(create_error = error_response(400, "Request contains an invalid argument."))
  httr2::local_mocked_responses(srv$mock)
  expect_error(batch_extract(s, model = "gemini-3.8-flash"), class = "amanuensis_api_error")
  expect_false(file.exists(file.path(s$out, "batch-jobs.json")))
  expect_identical(srv$deleted, "files/input-1")
})

test_that("when Google cannot be asked about a job being created, it stays recorded and its pages are not sent again", {
  s <- batch_setup()
  srv <- fake_batch()
  listing_down <- FALSE
  httr2::local_mocked_responses(function(req) {
    if (listing_down && grepl("/v1beta/batches$", sub("\\?.*$", "", req$url))) {
      return(error_response(503, "The service is currently unavailable."))
    }
    srv$mock(req)
  })
  batch_extract(s, model = "gemini-3.8-flash", wait = FALSE)
  jobs <- .read_jobs(s$out)
  jobs[[1]]["name"] <- list(NULL)   # as if R stopped right after creating it
  jobs[[1]]$state <- "submitting"
  .write_jobs(s$out, jobs)
  listing_down <- TRUE
  before <- length(srv$calls)
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(),
                                                   model = "gemini-3.8-flash", batch = TRUE, save_to = s$out))
  sent <- srv$calls[-seq_len(before)]
  expect_false(any(grepl("upload|batchGenerateContent|DELETE", sent)))
  expect_length(srv$created, 1)
  expect_length(srv$deleted, 0)
  expect_true(any(grepl("could not be asked whether it exists", msgs)))
  expect_false(any(grepl("no record of it", msgs)))
  expect_true(any(grepl("3 in a pending batch job", msgs)))
  expect_identical(unname(statuses(res)), rep("not run", 3))
  expect_length(.read_jobs(s$out), 1)
  # once Google answers, the job is found and collected
  listing_down <- FALSE
  res2 <- batch_extract(s, model = "gemini-3.8-flash")
  expect_identical(unname(statuses(res2)), rep("ok", 3))
  expect_length(srv$created, 1)
  # a listing that cannot be read to the end is not a "not found" either
  expect_error(httr2::with_mocked_responses(function(req) {
    httr2::response_json(body = list(batches = list(), nextPageToken = "more"))
  }, .find_job_by_display("amanuensis-x", FAKE_KEY, "gemini-3.8-flash")), class = "amanuensis_api_error",
  regexp = "did not look through them all")
})

test_that("cancel_batch() also finds and cancels a job whose creation was not confirmed", {
  s <- batch_setup()
  batch_extract(s, wait = FALSE)
  jobs <- .read_jobs(s$out)
  name <- jobs[[1]]$name
  jobs[[1]]["name"] <- list(NULL)
  jobs[[1]]$state <- "submitting"
  .write_jobs(s$out, jobs)
  done <- quietly(cancel_batch(s$out))
  expect_identical(done, name)
})

test_that("a batch file over the size limit is refused before anything is recorded or sent", {
  s <- batch_setup(n = 2)
  srv <- fake_batch()
  httr2::local_mocked_responses(srv$mock)
  withr::local_options(amanuensis.batch_max_bytes = 100)
  expect_error(batch_extract(s, model = "gemini-3.8-flash"), class = "amanuensis_error_input", regexp = "at most 2 GB")
  expect_false(file.exists(file.path(s$out, "batch-jobs.json")))
  expect_length(srv$calls, 0)
})

test_that("pages are split into jobs whose files stay under Google's size limit", {
  s <- batch_setup(n = 3)
  paths <- list.files(s$pages, full.names = TRUE)
  pages <- data.frame(image = paths, file = basename(paths), source = basename(paths), source_page = 1L,
                      stringsAsFactors = FALSE)
  settings <- list(model = "mock", resolution = "high", thinking_level = "LOW", max_tokens = 100L,
                   schema_channel = "openapi", api_args = list())
  run <- .new_run(settings, .contract(test_instructions(), roster_codebook()), NULL, NULL, pages)
  expect_length(.batch_chunks(run, 1:3, 500), 1)
  expect_identical(lengths(.batch_chunks(run, 1:3, 2)), c(2L, 1L))
  f <- withr::local_tempfile(fileext = ".jsonl")
  .write_batch_file(f, run, 1L)
  withr::local_options(amanuensis.batch_max_bytes = 1.9 * file.size(f))   # one page fits, two do not
  expect_identical(lengths(.batch_chunks(run, 1:3, 500)), c(1L, 1L, 1L))
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(), model = "mock",
                                                   batch = TRUE, save_to = s$out))
  expect_true(any(grepl("go in 3 jobs, so that no batch file passes Google's 2 GB limit", msgs)))
  expect_identical(unname(statuses(res)), rep("ok", 3))
})

test_that("a failure while the batch file is built leaves no job entry and no open file", {
  s <- batch_setup(n = 2)
  open_before <- nrow(showConnections())
  local_mocked_bindings(.encode_image = function(path) stop("cannot read the image"))
  expect_error(batch_extract(s), "cannot read the image")
  expect_false(file.exists(file.path(s$out, "batch-jobs.json")))
  expect_identical(nrow(showConnections()), open_before)
})

test_that("results Google says it does not have never block the folder, and are not paid for twice", {
  s <- batch_setup(n = 3)
  gone <- character()
  inner <- .practice_mock("mock")
  httr2::local_mocked_responses(function(req) {
    for (f in gone) {
      if (grepl(paste0(f, "-out:download"), req$url, fixed = TRUE)) {
        return(error_response(404, "Requested entity was not found."))
      }
    }
    inner(req)
  })
  run_it <- function(...) all_condition_text(transcribe(s$pages, test_instructions(), roster_codebook(),
                                                            model = "gemini-3.8-flash", save_to = s$out, ...))
  run_it(batch = TRUE, wait = FALSE, batch_size = 2)   # two jobs: p001-p002, p003
  jobs <- .read_jobs(s$out)
  gone <- jobs[[1]]$input_file
  run_it()            # both still running
  msgs <- run_it()    # both finished; the first job's results file answers 404
  expect_false(any(grepl("does not know the model", msgs)))
  expect_true(any(grepl("Could not download the results of batch job", msgs)))
  expect_true(any(grepl("batch-jobs.json", msgs, fixed = TRUE)))
  left <- .read_jobs(s$out)
  expect_length(left, 1)                               # the other job was collected
  expect_identical(left[[1]]$name, jobs[[1]]$name)
  expect_true(file.exists(file.path(s$out, "p003.json")))
  expect_false(file.exists(file.path(s$out, "p001.json")))   # kept pending, not sent again
  # older than Google keeps results: the pages are failed and sent again
  left[[1]]$submitted <- "2026-01-01T00:00:00Z"
  .write_jobs(s$out, left)
  msgs <- all_condition_text(res <- transcribe(s$pages, test_instructions(), roster_codebook(),
                                                   model = "gemini-3.8-flash", save_to = s$out))
  expect_true(any(grepl("no longer has this batch job's results", msgs)))
  expect_false(file.exists(file.path(s$out, "batch-jobs.json")))
  expect_identical(unname(statuses(res)), rep("ok", 3))
  expect_identical(.read_json(file.path(s$out, "p001.json"))$settings$engine, "sync")
})

test_that("batch results keep accented text intact in a session that is not UTF-8", {
  withr::local_locale(c(LC_CTYPE = "C"))
  local_amanuensis()
  dir <- withr::local_tempdir()
  ex <- quietly(example_project(file.path(dir, "ex")))
  pages <- quietly(images(ex$scans, save_to = file.path(dir, "pages")))
  strip <- function(x) {
    attr(x, "amanuensis") <- NULL
    class(x) <- "data.frame"
    x
  }
  sync <- quietly(transcribe(pages, ex$instructions, ex$codebook, model = "example", save_to = file.path(dir, "sync")))
  batch <- quietly(transcribe(pages, ex$instructions, ex$codebook, model = "example", batch = TRUE,
                                  save_to = file.path(dir, "batch")))
  expect_identical(strip(batch), strip(sync))
  text <- unlist(Filter(is.character, strip(batch)))
  expect_false(any(grepl("<[0-9a-f]{2}>", text)))
  expect_true(any(grepl("\u00e0", text, fixed = TRUE)))
  rec <- .read_text(file.path(dir, "batch", "padro1930_p001.json"))
  expect_false(grepl("<c3>", rec, fixed = TRUE))
})

test_that("a JSON schema you wrote goes to batch as it is, and works as it does without batch", {
  s <- batch_setup(n = 1)
  cb <- quietly(codebook(json = '{"type": "object", "properties": {
      "surname": {"type": "string"},
      "sex": {"oneOf": [{"type": "string", "enum": ["m", "f"]}, {"type": "null"}]},
      "age": {"anyOf": [{"type": "integer"}, {"type": "null"}]},
      "kind": {"const": "household"},
      "place": {"allOf": [{"type": "string"}, {"description": "Where the person lives."}]}},
    "required": ["surname", "sex", "age", "kind", "place"]}'))
  quietly(transcribe(s$pages, test_instructions(), cb, model = "mock", batch = TRUE, wait = FALSE, save_to = s$out))
  job <- .read_jobs(s$out)[[1]]
  expect_identical(job$settings$schema_channel, "json")
  line <- .parse_json(.mock_state$files[[job$input_file]])
  expect_identical(line$request$generation_config$response_json_schema, .schema_for_api(cb$schema))
  expect_null(line$request$generation_config$response_schema)
  res <- quietly(transcribe(s$pages, test_instructions(), cb, model = "mock", batch = TRUE, save_to = s$out))
  expect_identical(unname(statuses(res)), "ok")
  # forced through the OpenAPI channel: every node an object with a type, none []
  withr::local_options(amanuensis.batch_schema = "openapi")
  out2 <- file.path(s$dir, "out2")
  quietly(transcribe(s$pages, test_instructions(), cb, model = "mock", batch = TRUE, wait = FALSE, save_to = out2))
  job2 <- .read_jobs(out2)[[1]]
  text <- .mock_state$files[[job2$input_file]]
  expect_false(grepl('":[]', text, fixed = TRUE))
  props <- .parse_json(text)$request$generation_config$response_schema$properties
  expect_true(all(vapply(props, function(p) is.character(p$type) || !is.null(p$anyOf), logical(1))))
  res2 <- quietly(transcribe(s$pages, test_instructions(), cb, model = "mock", batch = TRUE, save_to = out2))
  expect_identical(unname(statuses(res2)), "ok")
})
