# The Batch API: half price, results usually within minutes, at most 24 hours
# (a job still pending after 48 hours expires). Lessons from real batch runs
# built in here:
#   - a JSONL file uploaded through the Files API, not inline requests (the
#     inline limit is 20 MB); written to a temporary file one line at a time
#   - every request keyed by its image file name, and results matched by key,
#     never by position (python-genai #1909: responses come back out of order)
#   - jobs of at most batch_size pages, submitted one at a time (Tier 1
#     accounts may queue only ~3M tokens of Flash requests at once), and
#     split further so no JSONL file passes Google's 2 GB limit
#   - the job is recorded in batch-jobs.json *before* it is created
#     ("submitting", with a unique display name) and again right after, so an
#     R crash in between never leaves an unknown paid job: the next run finds
#     it by its display name. Creating a job is not idempotent, so the entry
#     is dropped only when Google clearly refused the job (a 4xx); after a
#     server error or a lost reply it stays until a complete listing of the
#     account's jobs says whether the job exists
#   - each job entry carries its own contract and settings; results are
#     stamped with those, whatever the current call says
#   - every transcribe() call collects pending jobs first and never sends
#     their pages again; one job that cannot be collected never blocks the
#     others, or a run without batch, in the same folder
#   - per-line errors inside a successful job become failed pages, retried
#     on the next run; the uploaded input file is deleted afterwards

.batch_poll_seconds <- function(elapsed, model) {
  if (model %in% .practice_models) return(getOption("amanuensis.poll_seconds", 0))
  base <- getOption("amanuensis.poll_seconds", 30)
  if (elapsed > 600) max(base, 120) else base
}

# The schema channel of a batch run. The package's own codebooks go through
# the OpenAPI channel (verified in v1beta batch, and they use nothing it
# loses); a JSON schema the user wrote goes as it is through the JSON channel,
# as it does without batch: the OpenAPI conversion cannot express all of JSON
# Schema (allOf, const, numeric enums), and codebook() promises the schema is
# sent unchanged. options(amanuensis.batch_schema = "json"/"openapi") forces
# one channel.
.batch_schema_channel <- function(cb) {
  getOption("amanuensis.batch_schema", if (identical(cb$kind, "json")) "json" else "openapi")
}

.run_batch <- function(run, plan, key, batch_size = 500, wait = TRUE, budget = NULL) {
  s <- run$settings
  s$schema_channel <- .batch_schema_channel(run$contract$codebook)
  run$settings <- s
  todo <- plan$todo
  if (length(todo)) {
    est <- .batch_estimate(run, length(todo))
    .check_batch_budget(est, budget)
    .inform(c("i" = "Sending {length(todo)} page{?s} as a batch (half price): about {(.fmt_usd(est$cost))}{est$note} at Google's paid prices."))
    if (!is.null(budget) && !isTRUE(est$measured) && !is.na(est$cost)) {
      .inform(c("i" = "Even if every reply ran to {.arg max_tokens}, it would cost at most {(.fmt_usd(est$ceiling))}, within your budget of {(.fmt_usd(budget))}."))
    }
    if (interactive() && length(todo) >= 25 && is.null(budget) && !s$model %in% .practice_models &&
        isTRUE(getOption("amanuensis.ask", TRUE))) {
      if (!isTRUE(utils::askYesNo("Send the batch?", default = TRUE))) {
        run$stopped <- "Batch not sent."
        return(run)
      }
    }
    chunks <- .batch_chunks(run, todo, batch_size)
    if (length(chunks) > ceiling(length(todo) / batch_size)) {
      .inform(c("i" = "The pages go in {length(chunks)} jobs, so that no batch file passes Google's 2 GB limit."))
    }
    for (j in seq_along(chunks)) {
      share <- if (!is.null(budget)) list(total = budget, share = budget * length(chunks[[j]]) / length(todo))
      ok <- .submit_job(run, chunks[[j]], key, j, budget = share)
      if (!isTRUE(ok)) {
        left <- sum(lengths(chunks[j:length(chunks)]))
        run$stopped <- paste0("Google's batch queue for your account is full; ", left,
                              " page(s) were not sent yet. Run the same line again after the current job(s) finish.")
        break
      }
    }
  }
  jobs <- .read_jobs(run$dir)
  run$pending_jobs <- .job_files(jobs)
  # nothing to wait for when every recorded job is one whose creation is not
  # confirmed yet (it is looked up again on the next run)
  if (!any(vapply(jobs, function(j) !is.null(j$name), logical(1)))) return(run)
  if (!isTRUE(wait)) {
    .inform(c("i" = "Sent. Run the same line again later to collect the results; Google usually finishes within minutes, at most 24 hours."))
    return(run)
  }
  .wait_for_jobs(run, key)
}

# A batch cannot be stopped once it is sent, so `budget` is checked once,
# before sending. With earlier pages of this folder measured, against the
# measured cost per page. Before any page is measured, against the most the
# batch could cost (every reply running to max_tokens): the output - five
# times the input price, and most of the bill on dense pages - is not known
# yet, and the input-only figure once let a first batch spend several times
# its budget.
.check_batch_budget <- function(est, budget, call = rlang::caller_env()) {
  if (is.null(budget) || is.na(est$cost)) return(invisible())
  if (isTRUE(est$measured) && est$cost > budget) {
    .abort(c("The batch would cost about {(.fmt_usd(est$cost))}{est$note}, more than your budget of {(.fmt_usd(budget))}.",
             "i" = "A batch cannot be stopped once it is sent. Use {.arg limit} to send fewer pages, or raise {.arg budget}."),
           call = call, class = "amanuensis_error_input")
  }
  if (!isTRUE(est$measured) && est$ceiling > budget) {
    .abort(c("No page of this folder has been measured yet, so this batch could cost up to {(.fmt_usd(est$ceiling))} (if every reply ran to {.arg max_tokens}), more than your budget of {(.fmt_usd(budget))}.",
             "i" = "A batch cannot be stopped once it is sent. Run a few pages first (for example {.code limit = 5}) so the cost per page is measured, then run the same line again; or raise {.arg budget}."),
           call = call, class = "amanuensis_error_input")
  }
  invisible()
}

# Rough cost of a batch: measured from earlier pages of this folder when
# there are some, else input tokens only (output depends on how dense the
# pages are) together with a ceiling - every reply at max_tokens - for the
# budget check.
.batch_estimate <- function(run, n) {
  s <- run$settings
  price <- .price_for(if (s$model %in% .practice_models) "gemini-3.8-flash" else s$model, discount = TRUE, prices = s$prices)
  if (is.null(price)) return(list(cost = NA_real_, ceiling = NA_real_, measured = FALSE, note = " (cost unknown for this model)"))
  done <- Filter(function(r) !is.null(r$tokens) && identical(r$model, s$model), run$records)
  if (length(done) >= 2) {
    per <- mean(vapply(done, function(r) .cost(list(input = r$tokens$input, cached = r$tokens$cached %||% 0,
                                                     output = r$tokens$billed_output %||% 0, thinking = 0), price), numeric(1)), na.rm = TRUE)
    return(list(cost = per * n, ceiling = NA_real_, measured = TRUE, note = ", measured on earlier pages"))
  }
  image <- c(low = 280, medium = 560, high = 1120, ultra_high = 2240)[[s$resolution]]
  prompt <- nchar(run$contract$instructions$full) / 4 + nchar(.to_json(run$contract$codebook$schema)) / 4
  input <- n * (image + prompt) * price$input / 1e6
  list(cost = input, ceiling = input + n * (s$max_tokens %||% 65536) * price$output / 1e6, measured = FALSE,
       note = " for the input alone (the output adds more; run a few pages first with limit = 5 to measure it)")
}

# The largest batch file amanuensis writes (Google accepts at most 2 GB).
.batch_max_bytes <- function() getOption("amanuensis.batch_max_bytes", 1.9e9)

# Jobs of at most batch_size pages whose files stay under .batch_max_bytes().
# Each line's size is known before anything is written: the image in base64
# (4 bytes for every 3) plus the instructions, the schema and a little JSON.
# Camera JPEGs used as they are (about 4 MB) made a 500-page file of 2.7 GB,
# written in full to the temporary folder before it was refused.
.batch_chunks <- function(run, todo, batch_size) {
  limit <- .batch_max_bytes()
  fixed <- nchar(run$contract$instructions$full, type = "bytes") +
    2 * nchar(.to_json(run$contract$codebook$schema), type = "bytes") + 1000
  bytes <- ceiling(file.size(run$pages$image[todo]) / 3) * 4 + fixed
  bytes[is.na(bytes)] <- fixed
  chunks <- list()
  cur <- integer()
  size <- 0
  for (k in seq_along(todo)) {
    if (length(cur) && (length(cur) >= batch_size || size + bytes[k] > limit)) {
      chunks[[length(chunks) + 1]] <- cur
      cur <- integer()
      size <- 0
    }
    cur <- c(cur, todo[k])
    size <- size + bytes[k]
  }
  if (length(cur)) chunks[[length(chunks) + 1]] <- cur
  chunks
}

.job_display_name <- function(run, j) {
  paste0("amanuensis-", substr(.sha12(run$dir %||% "memory"), 1, 8), "-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", j)
}

.page_info <- function(page) {
  list(file = page$file, source = page$source %||% page$file, source_page = as.integer(page$source_page %||% 1L),
       image = page$image)
}

# The page files of a list of job entries.
.job_files <- function(jobs) {
  unlist(lapply(jobs, function(j) vapply(j$pages, function(p) p$file, ""))) %||% character()
}

# Write the JSONL of one job; the connection is closed however this ends (an
# open file cannot be deleted on Windows).
.write_batch_file <- function(path, run, idx) {
  s <- run$settings
  pages <- run$pages
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  schema <- .schema_for_api(run$contract$codebook$schema)
  for (i in idx) {
    body <- .build_body(run$contract$instructions$full, .encode_image(pages$image[i]), .mime_of(pages$image[i]),
                        schema, s, case = "snake")
    writeBin(charToRaw(paste0(.to_json(list(key = pages$file[i], request = body)), "\n")), con)
  }
  invisible(path)
}

# TRUE when Google clearly refused to create a job (a 4xx other than 408):
# no job exists. A 5xx, a 408, a lost reply or anything else leaves the
# outcome unknown.
.batch_create_refused <- function(e) {
  st <- .error_status(e)
  inherits(e, "amanuensis_api_error") && !is.na(st) && st >= 400 && st < 500 && st != 408
}

# Build, upload and create one job. TRUE on success, FALSE when Google's queue
# is full (429 at creation).
.submit_job <- function(run, idx, key, j, budget = NULL) {
  s <- run$settings
  pages <- run$pages
  display <- .job_display_name(run, j)

  # 1. the file, built and checked before anything is recorded or sent: a
  # failure here leaves no job entry behind
  jsonl <- tempfile(fileext = ".jsonl")
  on.exit(unlink(jsonl), add = TRUE)
  .write_batch_file(jsonl, run, idx)
  size <- file.size(jsonl)
  if (size > .batch_max_bytes()) {
    .abort(c("This batch file would be {round(size / 1e9, 1)} GB; Google accepts at most 2 GB.",
             "i" = "Use a smaller {.arg batch_size}."), class = "amanuensis_error_input")
  }

  # 2. recorded as "submitting" before anything billable happens
  entry <- list(name = NULL, display_name = display, state = "submitting", input_file = NULL,
                submitted = .utc_now(), contract = run$contract$id,
                settings = s[c("model", "resolution", "thinking", "thinking_level", "max_tokens", "temperature",
                               "engine", "schema_channel", "api_args", "retries", "timeout")],
                pages = lapply(idx, function(i) .page_info(pages[i, ])))
  # a list, so the names survive the JSON file (c(input = 1, output = 2) was
  # stored as [1, 2], which later stopped the collection of the paid results)
  if (!is.null(s$prices)) entry$settings$prices <- as.list(s$prices)
  entry$budget <- budget
  jobs <- .read_jobs(run$dir)
  jobs[[length(jobs) + 1]] <- entry
  pos <- length(jobs)
  .write_jobs(run$dir, jobs)

  # 3. upload, then create
  .inform(c("i" = "Uploading batch job {j}: {length(idx)} page{?s}, {round(size / 1e6)} MB."))
  stage <- "upload"
  result <- .with_practice_transport(s$model, tryCatch({
    file_name <- .gemini_upload(jsonl, key, display_name = display)
    jobs[[pos]]$input_file <- file_name
    .write_jobs(run$dir, jobs)
    stage <- "create"
    # creation and recording happen together: Esc is held until both are done
    suspendInterrupts({
      created <- tryCatch(.gemini_batch_create(s$model, file_name, key, display), error = function(e) e)
      if (!inherits(created, "error")) {
        jobs[[pos]]$name <- created$name
        jobs[[pos]]$state <- "submitted"
        .write_jobs(run$dir, jobs)
      }
    })
    created
  }, error = function(e) e))
  if (inherits(result, "error")) {
    if (identical(stage, "create") && !.batch_create_refused(result)) {
      # the entry and its input file stay: the next run looks the job up by
      # its display name before it sends any of these pages again
      .abort(c("Creating batch job {j} ended without a clear answer from Google, so the job may or may not exist.",
               "i" = "Its pages are not sent again until that is known: run the same line again in a few minutes, and it first looks the job up by its name."),
             class = if (inherits(result, "amanuensis_network")) "amanuensis_network" else "amanuensis_api_error",
             parent = result)
    }
    # refused at creation, or the upload failed: no job exists
    uploaded <- jobs[[pos]]$input_file
    jobs <- jobs[-pos]
    .write_jobs(run$dir, jobs)
    if (!is.null(uploaded)) .with_practice_transport(s$model, .gemini_file_delete(uploaded, key))
    if (identical(stage, "create") &&
        (identical(.error_status(result), 429L) || grepl("HTTP 429|RESOURCE_EXHAUSTED", conditionMessage(result)))) {
      return(FALSE)
    }
    stop(result)
  }
  .log_manifest(run, length(idx), extra = list(batch_job = result$name))
  .inform(c("v" = "Batch job {j} created ({result$name}): {length(idx)} page{?s}."))
  TRUE
}

# Called at the start of every transcribe(): collect finished jobs, and
# list the pages of unfinished ones so they are not sent again.
.collect_pending_jobs <- function(run, records, key) {
  jobs <- .read_jobs(run$dir)
  if (!length(jobs)) return(run)
  run$records <- records
  .inform(c("i" = "{length(jobs)} batch job{?s} recorded in {.path {run$dir}}; checking {cli::qty(length(jobs))}{?it/them} first."))
  jobs <- .recover_submitting(jobs, run, key)
  .write_jobs(run$dir, jobs)
  .poll_once(run, key, announce = TRUE)
  run$records_on_disk <- .read_records(run$dir)
  run
}

# A "submitting" entry without a job name: R stopped between upload and
# creation, or Google's answer to the creation was unclear. Look the job up
# by its display name; drop the entry only when a complete listing of the
# account's jobs does not hold it. When Google cannot be asked, the entry
# stays and its pages are not sent again.
.recover_submitting <- function(jobs, run, key) {
  for (k in seq_along(jobs)) {
    if (!is.null(jobs[[k]]$name)) next
    model <- jobs[[k]]$settings$model %||% ""
    found <- tryCatch(.find_job_by_display(jobs[[k]]$display_name, key, model), error = function(e) e)
    n <- length(jobs[[k]]$pages)
    if (inherits(found, "error")) {
      .inform(c("!" = "A batch job was being created when an earlier run stopped, and Google could not be asked whether it exists.",
                "x" = .cli_escape(conditionMessage(found)),
                "i" = "Its {n} page{?s} {cli::qty(n)}{?is/are} not sent again until that is known; the next run asks again."))
    } else if (!is.null(found)) {
      jobs[[k]]$name <- found
      jobs[[k]]$state <- "submitted"
      .inform(c("i" = "Found batch job {found}, created just before an earlier run stopped."))
    } else {
      if (!is.null(jobs[[k]]$input_file)) {
        .with_practice_transport(model, .gemini_file_delete(jobs[[k]]$input_file, key))
      }
      jobs[[k]]$drop <- TRUE
      .inform(c("i" = "A batch job was being created when an earlier run stopped, and Google has no record of it; its pages will be sent again."))
    }
  }
  Filter(function(j) !isTRUE(j$drop), jobs)
}

# The job name for a display name; NULL only when the complete list of the
# account's jobs does not hold it. Raises an error when the list could not be
# read in full (an HTTP error, the network, or more pages than it reads):
# "could not ask" must never be taken for "not there".
.find_job_by_display <- function(display, key, model) {
  .with_practice_transport(model, {
    token <- NULL
    for (page in 1:20) {
      req <- httr2::req_url_query(.gemini_request("batches", key = key, timeout = 60), pageSize = 100, pageToken = token)
      resp <- .gemini_perform(req, retries = 2, quiet = TRUE)
      .stop_if_failed(resp, "Listing the batch jobs failed")
      body <- httr2::resp_body_json(resp)
      for (b in body$batches %||% body$operations %||% list()) {
        dn <- b$metadata$displayName %||% b$displayName %||% b$metadata$display_name
        if (identical(dn, display)) return(b$name)
      }
      token <- body$nextPageToken
      if (is.null(token) || !nzchar(token)) return(NULL)
    }
    .abort("Google lists more than 2,000 batch jobs for this key; amanuensis did not look through them all.",
           class = "amanuensis_api_error")
  })
}

# Check every recorded job once; collect those that ended. Returns the number
# of jobs worth waiting for (a job that could not be collected, or whose
# creation is not confirmed, stays recorded but is not waited for).
.poll_once <- function(run, key, announce = FALSE) {
  jobs <- .read_jobs(run$dir)
  still <- list()
  status <- character()
  waiting <- 0L
  for (job in jobs) {
    if (is.null(job$name)) {
      # not known yet whether Google created it (see .recover_submitting()):
      # its pages stay pending so they are not paid for twice
      still[[length(still) + 1]] <- job
      status <- c(status, paste0(job$display_name %||% "a batch job", ": not known yet whether Google created it"))
      next
    }
    got <- tryCatch(.with_practice_transport(job$settings$model, .gemini_batch_get(job$name, key)),
                    error = function(e) e)
    if (inherits(got, "error")) {
      if (inherits(got, "amanuensis_api_error") && identical(.error_status(got), 404L)) {
        .inform(c("!" = "Google no longer knows batch job {job$name}; its pages will be sent again."))
        next
      }
      still[[length(still) + 1]] <- job
      waiting <- waiting + 1L
      status <- c(status, paste0(job$name, ": could not check (", conditionMessage(got), ")"))
      next
    }
    state <- .batch_state(got)
    if (state %in% c("SUCCEEDED", "FAILED", "CANCELLED", "EXPIRED")) {
      # one job that cannot be collected must not stop the others, nor every
      # later run in this folder: it stays recorded and is tried next run
      done <- tryCatch(.collect_job(run, job, got, state, key), error = function(e) e)
      if (inherits(done, "error")) {
        still[[length(still) + 1]] <- job
        .inform(c("!" = "Could not collect batch job {job$name}; it stays recorded and is tried again next run.",
                  "x" = .cli_escape(conditionMessage(done))))
      } else if (!isTRUE(done)) {
        still[[length(still) + 1]] <- job
        if (isTRUE(attr(done, "wait"))) waiting <- waiting + 1L
      }
    } else {
      still[[length(still) + 1]] <- job
      waiting <- waiting + 1L
      stats <- got$metadata$batchStats %||% list()
      done <- as.integer(stats$successfulRequestCount %||% 0) + as.integer(stats$failedRequestCount %||% 0)
      status <- c(status, paste0(job$name, ": ", tolower(state), ", ", done, " of ", length(job$pages), " pages done"))
    }
  }
  .write_jobs(run$dir, still)
  run$pending_jobs <- .job_files(still)
  if (announce && length(status)) .inform(stats::setNames(.cli_escape(status), rep("*", length(status))))
  waiting
}

.wait_for_jobs <- function(run, key) {
  start <- Sys.time()
  model <- run$settings$model
  .inform(c("i" = "Waiting for Google. Batch jobs usually finish within minutes (at most 24 hours).",
            "i" = "You can press Esc or close R at any time: the job keeps running, and running this same line later collects the results."))
  result <- tryCatch({
    repeat {
      left <- .poll_once(run, key)
      if (!left) break
      elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
      cli::cli_progress_message(paste0("Waiting for ", left, " batch job", if (left != 1) "s", " (", round(elapsed / 60), " min so far)"))
      .sleep(.batch_poll_seconds(elapsed, model))
    }
    "done"
  }, interrupt = function(e) "interrupted")
  if (identical(result, "interrupted")) {
    run$stopped <- "Stopped waiting. The batch job keeps running at Google; run the same line later to collect it."
    run$interrupted <- TRUE
  }
  run$records <- .read_records(run$dir)
  run
}

# Prices stored with a job, as a named vector for .price_for(). Versions up
# to 0.1.1 wrote c(input =, output =) without its names (jsonlite drops the
# names of a vector), which gave no cost at all and stopped the collection;
# such a list is read in the documented order: input, output, cached.
.job_prices <- function(p) {
  if (is.null(p) || !length(p)) return(NULL)
  v <- unlist(p)
  if (!is.numeric(v)) return(NULL)
  if (is.null(names(v)) || !all(c("input", "output") %in% names(v))) {
    if (length(v) < 2) return(NULL)
    v <- v[seq_len(min(3L, length(v)))]
    names(v) <- c("input", "output", "cached")[seq_along(v)]
  }
  v
}

# Google keeps a batch job's results for about 6 weeks. A 404 on the results
# of a younger job is more likely a passing fault (or a change at Google)
# than lost results, and treating it as lost would pay for those pages twice.
.results_kept_days <- 42

.job_age_days <- function(job) {
  t <- tryCatch(as.POSIXct(job$submitted %||% NA_character_, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                error = function(e) NA)
  if (is.na(t)) return(NA_real_)
  as.numeric(difftime(Sys.time(), t, units = "days"))
}

# Turn a finished job into page records, stamped with the job's own contract
# and settings. TRUE when the job is done with; FALSE to keep it recorded
# (with attribute wait = TRUE when it is worth waiting for).
.collect_job <- function(run, job, got, state, key) {
  settings <- job$settings
  settings$batch_job <- job$name
  settings$flex <- FALSE
  settings$prices <- .job_prices(settings$prices)
  contracts <- .read_contracts(run$dir)
  entry <- contracts[[job$contract]]
  cb <- if (!is.null(entry)) .contract_objects(entry, job$contract)$codebook else run$contract$codebook
  pages <- job$pages
  keys <- vapply(pages, function(p) p$file, "")
  model <- settings$model
  n <- length(pages)
  again <- "go back on the list of pages to send, and are paid again when they are sent"
  if (state != "SUCCEEDED") {
    why <- switch(state,
      EXPIRED = "Google did not finish the batch job within 48 hours",
      CANCELLED = "the batch job was cancelled",
      paste0("the batch job failed (", got$error$message %||% got$metadata$error$message %||% "no reason given", ")"))
    for (p in pages) .keep_record(run, .failed_record(p, run, "batch", paste0(why, "; the page will be sent again."), settings, job$contract))
    .inform(c("!" = "Batch job {job$name} ended as {state}: {why}. Its {n} page{?s} {again}."))
  } else {
    file <- .batch_results_file(got)
    if (is.null(file)) {
      .inform(c("!" = "Batch job {job$name} finished but Google gave no results file yet; checking again later."))
      return(invisible(structure(FALSE, wait = TRUE)))
    }
    text <- tryCatch(.with_practice_transport(model, .gemini_download(file, key)), error = function(e) e)
    if (inherits(text, "error")) {
      age <- .job_age_days(job)
      if (identical(.error_status(text), 404L) && !is.na(age) && age > .results_kept_days) {
        why <- "Google no longer has this batch job's results (it keeps them for about 6 weeks)"
        for (p in pages) .keep_record(run, .failed_record(p, run, "batch", paste0(why, "; the page will be sent again."), settings, job$contract))
        .inform(c("!" = "Batch job {job$name}: {why}. Its {n} page{?s} {again}."))
        if (!is.null(job$input_file)) .with_practice_transport(model, .gemini_file_delete(job$input_file, key))
        return(invisible(TRUE))
      }
      lines <- c("!" = "Could not download the results of batch job {job$name}; it stays recorded and is tried again next run, so its pages are not paid for twice.",
                 "x" = .cli_escape(conditionMessage(text)))
      if (identical(.error_status(text), 404L)) {
        lines <- c(lines, "i" = paste0("If you know these results are gone, delete {.file batch-jobs.json} in {.path {run$dir}}: ",
                                       "the job's pages are then sent again (and paid again)."))
      }
      .inform(lines)
      return(invisible(FALSE))
    }
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    seen <- character()
    spent <- 0
    for (line in lines[nzchar(trimws(lines))]) {
      obj <- tryCatch(.parse_json(line), error = function(e) NULL)
      if (is.null(obj)) next
      k <- obj$key %||% ""
      hit <- match(.page_key(k), .page_key(keys))
      if (is.na(hit)) next
      seen <- c(seen, keys[hit])
      p <- pages[[hit]]
      resp <- obj$response
      rec <- if (!is.null(resp) && is.null(obj$error)) {
        .record_from_reply(resp, line, p, run, settings, job$contract, cb)
      } else {
        msg <- obj$error$message %||% obj$status$message %||% "Google returned no reply for this page"
        .failed_record(p, run, "batch", paste0(msg, "; the page will be sent again."), settings, job$contract)
      }
      if (length(rec$cost_usd) == 1 && !is.na(rec$cost_usd)) spent <- spent + rec$cost_usd
      .keep_record(run, rec)
    }
    for (p in pages[!keys %in% seen]) {
      .keep_record(run, .failed_record(p, run, "batch", "No reply in Google's results file; the page will be sent again.", settings, job$contract))
    }
    .inform(c("v" = "Collected batch job {job$name}: {length(seen)} of {n} page{?s} returned."))
    .warn_over_budget(job, spent)
  }
  if (!is.null(job$input_file)) .with_practice_transport(model, .gemini_file_delete(job$input_file, key))
  invisible(TRUE)
}

# A batch is checked against the budget once, before sending, on an estimate
# (see .check_batch_budget()); say so when the pages turned out dearer.
.warn_over_budget <- function(job, spent) {
  b <- job$budget
  if (!is.list(b) || !is.numeric(b$share) || !is.numeric(b$total)) return(invisible())
  if (spent <= b$share + 1e-9) return(invisible())
  allowed <- if (abs(b$share - b$total) < 1e-9) paste0("your budget of ", .fmt_usd(b$total)) else
    paste0("its share of your budget (", .fmt_usd(b$share), " of ", .fmt_usd(b$total), ")")
  .warn(c("Batch job {job$name} cost {(.fmt_usd(spent))}, more than {allowed}.",
          "i" = "A batch cannot be stopped once it is sent: the budget was checked before sending, against the cost measured on earlier pages, and these pages cost more."))
}

#' Cancel a pending batch job
#'
#' Pressing Esc while [transcribe()] waits for a batch job only stops the
#' waiting: the job keeps running at Google, and you pay for it when it
#' finishes. Use this to cancel the jobs recorded in a results folder. Pages
#' Google already finished may still be billed.
#'
#' The next [transcribe()] run on the folder records the cancellation and
#' sends the cancelled pages again, so they are paid again (half price with
#' `batch = TRUE`, full price without). Run it when you want those pages
#' extracted.
#'
#' @param save_to The results folder of the run (as given to
#'   [transcribe()]).
#' @return The names of the cancelled jobs, invisibly.
#' @export
#' @examples
#' \dontrun{
#' cancel_batch("results")
#' }
cancel_batch <- function(save_to) {
  .check_string(save_to, arg = "save_to")
  if (!dir.exists(save_to)) .abort("Cannot find the folder {.path {(.show_path(save_to))}}.", class = "amanuensis_error_input")
  dir <- .show_path(save_to)
  jobs <- .read_jobs(dir)
  if (!length(jobs)) {
    .inform(c("i" = "No pending batch jobs in {.path {dir}}."))
    return(invisible(character()))
  }
  key <- NULL
  done <- character()
  n_pages <- 0L
  for (job in jobs) {
    model <- job$settings$model %||% ""
    if (is.null(key)) key <- if (model %in% .practice_models) "mock-key" else .gemini_key()
    name <- job$name
    if (is.null(name)) {
      # created when an earlier run stopped, or with an unclear answer: it
      # may exist at Google, so look it up rather than skip it
      name <- tryCatch(.find_job_by_display(job$display_name, key, model), error = function(e) e)
      if (inherits(name, "error")) {
        .inform(c("!" = "Could not check whether batch job {.val {job$display_name}} was created; run {.fn cancel_batch} again later.",
                  "x" = .cli_escape(conditionMessage(name))))
        next
      }
      if (is.null(name)) next
    }
    req <- httr2::req_method(.gemini_request(paste0(name, ":cancel"), key = key, timeout = 60), "POST")
    req <- httr2::req_body_raw(req, "{}", type = "application/json")
    resp <- .with_practice_transport(model, .gemini_perform(req, retries = 2))
    if (httr2::resp_status(resp) < 300) {
      done <- c(done, name)
      n_pages <- n_pages + length(job$pages)
    } else {
      .inform(c("!" = "Could not cancel {name}: {(.gemini_error_message(resp))}"))
    }
  }
  if (length(done)) {
    .inform(c("v" = "Asked Google to cancel {length(done)} job{?s}: {.val {done}}.",
              "i" = "The next {.fn transcribe} run on this folder records the cancellation and sends the {n_pages} cancelled page{?s} again, paid again (half price with {.code batch = TRUE}, full price without). Run it when you want {?it/them} extracted."))
  }
  invisible(done)
}
