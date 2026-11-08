# transcribe(): one call to the model per page image, each with a fresh
# context, the instructions and the codebook's schema; every reply parsed and
# checked against the codebook as soon as it arrives.
#
# Nothing is written to the user's computer unless a results folder is given
# (save_to): the results come back to R and output() saves them. Without a
# folder, what has been paid for is still protected: a run stopped by Esc, an
# error, the budget or a refused key returns the pages read so far, and every
# page is also kept in memory for the rest of the R session (store.R), so the
# same line run again continues where it stopped. With a folder, each page is
# saved as it arrives and a run resumes even after R is closed; batch jobs,
# which can outlive the session, need one.
#
# What decides whether a page is done (plan, below): a record with status
# "ok" or "check" made with the same contract (instructions + codebook),
# model, resolution and thinking. Failures are retried only when a retry can
# help: server and network trouble always; a reply cut off at max_tokens only
# when max_tokens was raised; a blocked or unreadable reply once; a page
# Google could not use as an image never automatically.

#' Transcribe page images into data
#'
#' Sends each page image to Google Gemini with your instructions and
#' codebook, and collects the replies into a table: one row per record, with
#' the file, source and page each row comes from.
#'
#' Nothing is written to your computer: the results come back to R as a data
#' frame. Save them with [output()] (Excel, CSV, Stata, Word, ...), or give a
#' results folder with `save_to` (see "Saving and resuming").
#'
#' @section Saving and resuming:
#' Without `save_to`, the pages read so far are never lost while R is open.
#' If a run stops (you press Esc, the budget is reached, Google refuses the
#' key, an error), it returns the pages already read. Every page is also kept
#' for the rest of the R session, so running the same line again sends only
#' the pages still missing: nothing is paid twice. Closing R forgets them,
#' unless you saved the result.
#'
#' With `save_to = "a folder"`, each page's result is saved there as soon as
#' it arrives (one small file per page, plus a record of the instructions and
#' codebook). Run the same line again, even after closing R, and only the
#' missing pages are sent; when everything is done it costs nothing and
#' returns the table. Use a folder for long runs and for batch jobs.
#'
#' Pages that failed are tried again when that can help (see the end-of-run
#' message). Pages read with other instructions, codebook or settings, or by
#' a practice model, never mix into the table. With a folder, if you change
#' the instructions, the codebook or the model, you are asked whether to keep
#' the old results apart (in a new folder, `extraction_2`) or redo the pages;
#' once you have kept them apart, running the same line again continues in
#' that folder. An old model name and the model Google sends it to (e.g.
#' `"gemini-3.7-flash"` and `"gemini-3.8-flash"`) count as the same model.
#'
#' @section Cost:
#' Every page's tokens and estimated cost are recorded, at Google's paid
#' prices on the day of the run (see [gemini_models()]); keys without billing
#' are not charged. In an interactive session with 25 or more pages to do,
#' the first two pages are sent first and you are shown the projected cost
#' before the rest are sent. `budget` stops a run before it spends more than
#' you allow. A batch cannot stop halfway, so with `batch = TRUE` the budget
#' is checked once, before sending: against the cost per page measured on
#' earlier pages of the folder, or, before any page is measured, against the
#' most the batch could cost (every reply as long as `max_tokens`). Run a few
#' pages first (`limit = 5`) to measure. `batch = TRUE` and `flex = TRUE`
#' cost half.
#'
#' @section Batch:
#' `batch = TRUE` sends all pages as one job (or several) to Google's Batch
#' API: half price, results usually within minutes, at most 24 hours. A job
#' can outlive your R session, so batch needs a results folder (`save_to`),
#' where the job is recorded: you can press Esc or close R while waiting and
#' run the same line later to collect the results. Batch needs a key from a
#' project with billing.
#'
#' @param images The page images: the result of [images()], a folder of page
#'   images, or image files.
#' @param instructions From [instructions()], or a file path. Default: the
#'   package's general transcription instructions.
#' @param codebook From [codebook()], or a codebook file. Default: free text
#'   (a transcription of each page).
#' @param model The Gemini model. See [gemini_models()]. `"mock"` gives
#'   made-up replies without a key or any cost, for practice; `"example"`
#'   replays prepared replies for the example project ([example_project()]).
#' @param batch `TRUE` to use the Batch API (half price, slower).
#' @param save_to A folder to save each page's result in as it arrives, so a
#'   run can resume even after R is closed (and to keep the run's records
#'   and logs). Default `NULL`: nothing is written to your computer.
#' @param continue An earlier result of `transcribe()` (for example one saved
#'   with `output(res, "res.rds")` and read back with [readRDS()]) whose pages
#'   are kept and not sent again. Not needed within the same R session, where
#'   running the same line again already continues.
#' @param resolution How much detail of each image Google reads: `"low"`,
#'   `"medium"`, `"high"` (about 1,100 image tokens) or `"ultra_high"` (about
#'   2,200; default). Higher reads small and handwritten text better and costs
#'   a little more input.
#' @param thinking How much the model reasons before answering: `"low"`
#'   (default; transcription is not reasoning, and thinking is billed as
#'   output), `"medium"`, `"high"`, or `"minimal"` on models that accept it.
#' @param max_tokens Longest reply allowed, in tokens (thinking included). A
#'   reply that reaches it is cut off and the page fails; raise it for very
#'   dense pages. Default 32768.
#' @param temperature Not sent by default: Gemini 3.6 and newer models ignore
#'   it. Give a number only for older models.
#' @param flex `TRUE` to use Google's flex tier: half price, each page may take
#'   minutes. Not with `batch`.
#' @param only Process only these pages: file names, row numbers of `images`,
#'   or a benchmark from [benchmark()] (to extract just your benchmark pages).
#' @param limit Process at most this many pages in this run (for working in
#'   batches of pages and checking each).
#' @param redo Pages to extract again even though they have results: `"all"`,
#'   `"failed"`, `"check"` (pages whose reply did not match the codebook), or
#'   file names. Each earlier result is moved to `superseded/` (never
#'   deleted) when its new one arrives, so a run that stops early leaves the
#'   pages it did not reach as they were.
#' @param budget Maximum to spend in this run, in US dollars. The run stops
#'   before going over it. Pages collected from a batch job sent earlier do
#'   not count: they were paid for when that job ran.
#' @param confirm Ask before spending on 25 or more pages? Default: yes in an
#'   interactive session.
#' @param workers Number of pages sent at the same time (1-8). Default 1.
#' @param batch_size Maximum pages per batch job. Default 500, which fits the
#'   queue limit of new Google accounts.
#' @param wait For batch: wait for the results (default), or return at once and
#'   collect later by running the same line.
#' @param log `TRUE` to also keep log files in the results folder
#'   (`run_manifest.jsonl`, `token_usage.txt`, `errors.log`; needs `save_to`),
#'   or a folder for them. Default `FALSE`.
#' @param retries How many times to retry a page when Google is busy or the
#'   network fails. Default 4 (waiting 5, 10, 20, 40 seconds).
#' @param timeout Seconds to wait for one reply. Default depends on the model
#'   (10 minutes; 20 for gemini-4-argon; 15 with flex).
#' @param prices Your own prices, in US dollars per million tokens:
#'   `c(input = , output = , cached = )`. Default: the package's table.
#' @param api_args For experts: a list merged into every request body, e.g.
#'   `list(generationConfig = list(seed = 1))`.
#' @param quiet `TRUE` to print only warnings and errors.
#' @return A data frame of class `amanuensis_result`: one row per record (or
#'   per page for a page-level codebook or free text), starting with `file`,
#'   `source`, `source_page` and `row`. Only pages whose reply matched the
#'   codebook, read with these settings, are included; see
#'   `output(x, table = "pages")` for every page with its status, tokens and
#'   cost.
#' @export
#' @examples
#' ex <- example_project()
#' pages <- images(ex$scans, save_to = file.path(ex$folder, "pages"))
#' res <- transcribe(pages, ex$instructions, ex$codebook, model = "example")
#' res
#'
#' \dontrun{
#' # with a real model (needs set_key() first)
#' res <- transcribe(pages, "instructions.docx", "codebook.xlsx")
#' output(res, "results.xlsx")
#'
#' # a long run, saved page by page so it can resume after closing R
#' res <- transcribe(pages, "instructions.docx", "codebook.xlsx", save_to = "results")
#' }
transcribe <- function(images, instructions = NULL, codebook = NULL,
                       model = "gemini-3.8-flash", batch = FALSE, save_to = NULL, continue = NULL,
                       resolution = "ultra_high", thinking = "low", max_tokens = 32768,
                       temperature = NULL, flex = FALSE, only = NULL, limit = NULL,
                       redo = NULL, budget = NULL, confirm = interactive(), workers = 1,
                       batch_size = 500, wait = TRUE, log = FALSE, retries = 4,
                       timeout = NULL, prices = NULL, api_args = list(), quiet = FALSE) {
  if (missing(images)) .abort("Tell {.fn transcribe} which page images to read, e.g. {.code transcribe(pages)}.", class = "amanuensis_error_input")
  if (isTRUE(quiet)) {
    rlang::local_options(cli.progress_show_after = Inf)
    return(suppressMessages(transcribe(images = images, instructions = instructions, codebook = codebook,
      model = model, batch = batch, save_to = save_to, continue = continue, resolution = resolution,
      thinking = thinking, max_tokens = max_tokens, temperature = temperature, flex = flex, only = only,
      limit = limit, redo = redo, budget = budget, confirm = confirm, workers = workers,
      batch_size = batch_size, wait = wait, log = log, retries = retries, timeout = timeout,
      prices = prices, api_args = api_args, quiet = FALSE)))
  }
  # ------- check every argument before reading a file or calling Google
  .check_model(model)
  model <- sub("^models/", "", model)
  .check_choice(resolution, .resolution_levels)
  .check_number(max_tokens, min = 1)
  .check_number(retries, min = 0)
  .check_number(workers, min = 1)
  .check_number(batch_size, min = 1)
  .check_number(budget, min = 0, null_ok = TRUE)
  .check_number(limit, min = 1, null_ok = TRUE)
  .check_number(timeout, min = 1, null_ok = TRUE)
  if (!is.null(temperature)) .check_number(temperature, min = 0)
  if (workers > 8) .abort("{.arg workers} can be at most 8.", class = "amanuensis_error_input")
  if (isTRUE(batch) && isTRUE(flex)) .abort("Use either {.code batch = TRUE} or {.code flex = TRUE}, not both (both cost half).", class = "amanuensis_error_input")
  no_folder <- is.null(save_to) || isFALSE(save_to)
  if (isTRUE(batch) && no_folder) {
    # a job can outlive the R session, and its record must too: without it
    # the results could not be collected, and the pages would be paid again
    suggest <- tryCatch(.default_results_dir(.as_images(images)), error = function(e) "results")
    .abort(c("A batch job can take hours, so it needs a folder where it is recorded until its results are collected, even if R is closed.",
             "i" = "Add {.code save_to = \"{(.show_path(suggest))}\"} (or another folder), or leave out {.code batch = TRUE} to read the pages one by one, at full price."),
           class = "amanuensis_error_input")
  }
  if (!is.null(continue) && is.null(.result_info(continue))) {
    .abort(c("{.arg continue} must be an earlier result of {.fn transcribe}.",
             "i" = "For example {.code continue = res}, or {.code continue = readRDS(\"res.rds\")} for one saved with {.code output(res, \"res.rds\")}."),
           class = "amanuensis_error_input")
  }
  if (!is.list(api_args)) .abort("{.arg api_args} must be a list.", class = "amanuensis_error_input")
  if (!is.null(prices) && (!is.numeric(prices) || !all(c("input", "output") %in% names(prices)))) {
    .abort("{.arg prices} must look like {.code c(input = 0.75, output = 3.75)} (dollars per million tokens).", class = "amanuensis_error_input")
  }
  info <- .model_info(model)
  thinking_level <- .thinking_for(model, thinking)
  if (!is.null(info) && max_tokens > info$max_output) {
    .inform(c("!" = "{.val {model}} replies with at most {(.fmt_int(info$max_output))} tokens; using {.code max_tokens = {info$max_output}}."))
    max_tokens <- info$max_output
  }
  if (!is.null(temperature) && (is.null(info) || !grepl("2\\.5|^gemini-3-|3\\.1|3\\.5-flash-lite", model))) {
    .inform(c("i" = "Gemini 3.6 and newer models ignore {.arg temperature}; it is sent anyway as you asked."))
  }
  if (!is.null(budget) && is.null(.price_for(model, prices = prices)) && !model %in% .practice_models) {
    .abort(c("{.arg budget} needs prices, and {.val {model}} is not in the package's price table.",
             "i" = "Give them with {.code prices = c(input = ..., output = ...)} (dollars per million tokens)."),
           class = "amanuensis_error_input")
  }

  pages <- .as_images(images)
  instr <- .as_instructions(instructions)
  cb <- .as_codebook(codebook)
  key <- if (model %in% .practice_models) "mock-key" else .gemini_key()
  if (model == "example") .check_example_pages(pages)

  settings <- list(
    model = model, resolution = resolution, thinking = thinking, thinking_level = thinking_level,
    max_tokens = as.integer(max_tokens), temperature = temperature,
    engine = if (isTRUE(batch)) "batch" else if (isTRUE(flex)) "flex" else "sync",
    flex = isTRUE(flex), api_args = api_args,
    timeout = timeout %||% (if (isTRUE(flex)) 900 else info$timeout %||% 600),
    retries = as.integer(retries), prices = prices
  )
  contract <- .contract(instr, cb)
  dir <- .results_dir(pages, save_to)
  log_dir <- .log_dir(log, dir)

  # ------- what is already there: the folder, then `continue`, then what this
  # R session has already read for these images with these settings
  records <- .read_records(dir)
  run <- .new_run(settings, contract, dir, log_dir, pages)
  records <- .add_earlier_records(records, run, pages, continue)
  run$records <- records
  if (!is.null(dir)) {
    .save_contract(dir, contract)
    run$earlier_jobs <- vapply(tryCatch(.read_jobs(dir), error = function(e) list()), function(j) j$name %||% "", "")
    run <- .collect_pending_jobs(run, records, key)   # never pay twice for a page in a pending job
    records <- run$records_on_disk %||% records
    # pages collected from a job sent by an earlier call were paid for then:
    # they do not count towards this run's budget or its cost projection
    .count_collected(run)
  }
  plan <- .plan(pages, records, run, only = only, redo = redo, limit = limit)
  if (!is.null(plan$restart)) {
    # results for these settings are kept apart in another folder (the user
    # chose it now or earlier): continue there
    return(transcribe(images = images, instructions = instr, codebook = cb, model = model, batch = batch,
                      save_to = plan$restart, continue = continue, resolution = resolution, thinking = thinking,
                      max_tokens = max_tokens, temperature = temperature, flex = flex, only = only, limit = limit,
                      redo = redo, budget = budget, confirm = confirm, workers = workers, batch_size = batch_size,
                      wait = wait, log = log, retries = retries, timeout = timeout, prices = prices,
                      api_args = api_args, quiet = quiet))
  }
  # Old results are not moved aside here but one by one, as each replacement
  # arrives (.keep_record()): a run that stops before (budget, Esc, a batch
  # not sent) leaves them where they were.
  .announce_plan(plan, run)

  run$records <- records
  if (isTRUE(batch) && (length(plan$todo) || length(run$pending_jobs))) {
    run <- .run_batch(run, plan, key, batch_size = batch_size, wait = wait, budget = budget)
  } else if (length(plan$todo)) {
    run <- .run_sync(run, plan, key, workers = workers, budget = budget, confirm = isTRUE(confirm))
  }
  .finish(run, pages, plan)
}

# Pages read before, outside the folder: those of `continue` (an earlier
# result passed back in) and those this R session already read for these
# images with these settings. The folder, when there is one, comes first;
# pages it lacks are written into it, so it becomes complete. Pages of
# `continue` read with other settings stop the run before anything is spent:
# continuing them silently would mix two settings, and reading them again
# silently would pay twice.
.add_earlier_records <- function(records, run, pages, continue, call = rlang::caller_env()) {
  fp <- .run_fingerprint(run)
  keys <- .page_key(pages$file)
  from_continue <- list()
  if (!is.null(continue)) {
    cinfo <- .result_info(continue)
    recs <- cinfo$records[intersect(names(cinfo$records), keys)]
    # practice replies are read again for real (as in .plan()), never mixed
    practice <- vapply(recs, function(r) (r$model %||% "") %in% .practice_models, logical(1)) &
      !run$settings$model %in% .practice_models
    recs <- recs[!practice]
    other <- vapply(recs, function(r) r$status %in% c("ok", "check") && !identical(.record_fingerprint(r), fp), logical(1))
    if (any(other)) {
      what <- .describe_continue_change(cinfo, run)
      .abort(c("{sum(other)} page{?s} in {.arg continue} {cli::qty(sum(other))}{?was/were} read with other settings:",
               stats::setNames(.cli_escape(what), rep("*", length(what))),
               "i" = "Continue with the same instructions, codebook, model, resolution and thinking, or leave out {.arg continue} to read the pages again with the new settings (they are paid again)."),
             call = call, class = "amanuensis_error_contract")
    }
    if (!length(recs) && length(cinfo$records)) {
      .inform(c("!" = "None of the pages in {.arg continue} are among these images; nothing to continue."))
    }
    from_continue <- recs
  }
  from_session <- .session_records(fp, pages)
  extra <- c(from_continue, from_session[setdiff(names(from_session), names(from_continue))])
  extra <- extra[setdiff(names(extra), names(records))]
  if (!length(extra)) return(records)
  n_cont <- sum(names(extra) %in% names(from_continue))
  n_sess <- length(extra) - n_cont
  if (!is.null(run$dir)) {
    for (r in extra) .write_record(run$dir, r)
    .inform(c("i" = "Saved {length(extra)} page{?s} read earlier ({n_cont} from {.arg continue}, {n_sess} from this R session) in {.path {run$dir}}; {cli::qty(length(extra))}{?it is/they are} not sent again."))
  } else {
    img <- stats::setNames(pages$image, keys)
    for (k in names(extra)) .session_keep(extra[[k]], img[[k]])
    # said in the plan's summary (.announce_plan()), which knows how many of
    # them are really left alone (redo may send some again)
    run$from_session <- setdiff(names(extra), names(from_continue))
    run$from_continue <- intersect(names(extra), names(from_continue))
  }
  c(records, extra)
}

# Plain words for how the settings of an earlier result differ from the run's.
.describe_continue_change <- function(cinfo, run) {
  s <- run$settings
  old_entry <- tryCatch(.contract_entry(list(id = cinfo$contract, instructions = cinfo$instructions,
                                             codebook = cinfo$codebook)), error = function(e) NULL)
  what <- if (!identical(cinfo$contract, run$contract$id)) {
    if (is.null(old_entry)) "instructions or codebook" else .describe_change(old_entry, .contract_entry(run$contract))
  } else character()
  os <- cinfo$settings %||% list()
  if (!identical(.canonical_model(os$model %||% ""), .canonical_model(s$model))) what <- c(what, paste0("model: ", os$model, " -> ", s$model))
  if (!identical(os$resolution %||% "", s$resolution)) what <- c(what, paste0("resolution: ", os$resolution, " -> ", s$resolution))
  if (!identical(os$thinking %||% "", s$thinking)) what <- c(what, paste0("thinking: ", os$thinking, " -> ", s$thinking))
  if (!length(what)) what <- "settings"
  unique(what)
}

# ------- run state ------------------------------------------------------------

.new_run <- function(settings, contract, dir, log_dir, pages) {
  run <- new.env(parent = emptyenv())
  run$settings <- settings
  run$contract <- contract
  run$dir <- dir
  run$log_dir <- log_dir
  run$pages <- pages
  run$records <- list()
  run$pending_jobs <- character()
  run$started <- Sys.time()
  run$spent <- 0          # this run's sends only (see .count_collected())
  run$n_sent <- 0         # records written for requests sent in this run
  run$n_priced <- 0       # ... of which with a cost (the budget's average)
  run$collected <- 0      # records collected from jobs sent by an earlier call
  run$earlier_jobs <- character()
  run$statuses <- character()
  run$written <- character()  # .page_key() of every page written by this run
  run$moved <- 0L         # earlier results moved to superseded/
  run$moved_to <- NULL
  run$stopped <- NULL
  run$from_session <- character()  # .page_key() of pages reused from this session
  run$from_continue <- character() # ... and from `continue`
  run
}

# Records collected so far (from batch jobs sent by an earlier call, including
# one recovered by its display name) are counted apart from what this run
# sends.
.count_collected <- function(run) {
  run$collected <- run$collected + run$n_sent
  run$n_sent <- 0
  run$n_priced <- 0
  run$spent <- 0
  invisible(run)
}

# What makes a page done: the contract (instructions + codebook), the model
# that answers, the resolution and the thinking level. An old model name and
# the model Google sends it to (gemini-3.7-flash -> gemini-3.8-flash) are the
# same model: the package itself advises the switch, which must not make
# every finished page count as made with other settings.
.fingerprint <- function(contract_id, model, resolution, thinking) {
  paste(contract_id, .canonical_model(model), resolution, thinking, sep = "|")
}

.record_fingerprint <- function(rec) {
  .fingerprint(rec$contract %||% "", rec$model %||% "", rec$settings$resolution %||% "", rec$settings$thinking %||% "")
}

.run_fingerprint <- function(run) {
  s <- run$settings
  .fingerprint(run$contract$id, s$model, s$resolution, s$thinking)
}

# The model that actually answers a model name (vectorised).
.canonical_model <- function(model) {
  model <- sub("^models/", "", as.character(model))
  routes <- .alias_routes()
  hit <- unname(routes[model])
  ifelse(is.na(hit), model, hit)
}

.alias_cache <- new.env(parent = emptyenv())

.alias_routes <- function() {
  if (is.null(.alias_cache$routes)) {
    tab <- .model_table()
    alias <- !is.na(tab$routes_to)
    .alias_cache$routes <- stats::setNames(as.character(tab$routes_to[alias]), tab$model[alias])
  }
  .alias_cache$routes
}

.practice_models <- c("mock", "example")

# ------- planning -------------------------------------------------------------

# Decide which pages to send. Returns list(todo = <row indices of pages>,
# done, held (failed pages not retried, with reasons), supersede (files whose
# old records move aside), restart (a new save_to, when the user chose it)).
.plan <- function(pages, records, run, only = NULL, redo = NULL, limit = NULL, call = rlang::caller_env()) {
  s <- run$settings
  fp_now <- .run_fingerprint(run)
  keys <- .page_key(pages$file)
  scope <- seq_len(nrow(pages))
  if (!is.null(only)) scope <- .resolve_only(only, pages, call)
  redo_mode <- NULL
  redo_files <- character()
  if (!is.null(redo)) {
    if (is.character(redo) && length(redo) == 1 && redo %in% c("all", "failed", "check")) {
      redo_mode <- redo
    } else {
      idx <- .resolve_only(redo, pages, call)
      redo_files <- keys[idx]
    }
  }
  in_job <- .page_key(run$pending_jobs)
  todo <- integer(); done <- integer(); changed <- integer(); supersede <- character()
  held <- list(); practice <- integer(); image_changed <- integer()
  for (i in scope) {
    k <- keys[i]
    if (k %in% in_job) next
    rec <- records[[k]]
    forced <- identical(redo_mode, "all") || k %in% redo_files
    if (is.null(rec)) { todo <- c(todo, i); next }
    if (forced) {
      todo <- c(todo, i)
      if (rec$status %in% c("ok", "check")) supersede <- c(supersede, pages$file[i])
      next
    }
    if (!is.null(rec$image_bytes) && !identical(as.numeric(rec$image_bytes), as.numeric(file.size(pages$image[i])))) {
      image_changed <- c(image_changed, i)
      todo <- c(todo, i)
      if (rec$status %in% c("ok", "check")) supersede <- c(supersede, pages$file[i])
      next
    }
    if (rec$status %in% c("ok", "check")) {
      if ((rec$model %||% "") %in% .practice_models && !s$model %in% .practice_models) {
        practice <- c(practice, i)
        todo <- c(todo, i)
        next
      }
      if (!identical(.record_fingerprint(rec), fp_now)) { changed <- c(changed, i); next }
      if (rec$status == "check" && identical(redo_mode, "check")) {
        todo <- c(todo, i)
        supersede <- c(supersede, pages$file[i])
        next
      }
      done <- c(done, i)
      next
    }
    # failed
    retry <- .should_retry(rec, s, redo_mode)
    if (isTRUE(retry)) todo <- c(todo, i) else held[[pages$file[i]]] <- retry
  }
  restart <- NULL
  if (length(changed) && is.null(run$dir)) {
    # cannot happen through transcribe() (records from continue are checked
    # before, and this session's are kept by settings), but never ask about
    # folders when there is none: read those pages again
    todo <- c(todo, changed)
    changed <- integer()
  }
  if (length(changed)) {
    # results for the current settings already kept apart in <folder>_N
    # (an earlier "keep the old results apart"): continue there, so the same
    # line resumes instead of paying again for every page in a new folder
    alt <- if (!length(done)) .find_alternate(run, pages) else NULL
    choice <- if (!is.null(alt)) list(action = "restart", folder = alt) else .ask_changed(changed, pages, records, run, call)
    if (choice$action == "redo") {
      todo <- c(todo, changed)
      supersede <- c(supersede, pages$file[changed])
    } else if (choice$action == "restart") {
      return(list(restart = choice$folder))
    }
  }
  todo <- sort(unique(todo))
  skipped_by_limit <- 0L
  if (!is.null(limit) && length(todo) > limit) {
    skipped_by_limit <- length(todo) - limit
    todo <- todo[seq_len(limit)]
    # redo = "all" cannot tell a page redone by the previous run of this line
    # from one not reached yet, so with limit it starts from the top again
    if (identical(redo_mode, "all")) {
      .inform(c("!" = "{.code redo = \"all\"} with {.arg limit} redoes the first {limit} page{?s} each time this line runs.",
                "i" = "To go on with the next pages, name them instead: {.code redo = } or {.code only = } with file names or page numbers."))
    }
  }
  .inform_redone(length(practice), sum(practice %in% todo),
                 "{n} page{?s} held practice ({.val mock}/{.val example}) results; extracting {now_what} for real now")
  .inform_redone(length(image_changed), sum(image_changed %in% todo),
                 "{n} page image{?s} changed since {cli::qty(n)}{?it was/they were} extracted; extracting {now_what} again")
  list(todo = todo, done = done, held = held, supersede = intersect(supersede, pages$file[todo]),
       restart = restart, scope = scope, skipped_by_limit = skipped_by_limit,
       in_job = sum(keys[scope] %in% in_job))
}

# TRUE to retry a failed page now, or a sentence saying why not.
.should_retry <- function(rec, s, redo_mode) {
  if (identical(redo_mode, "failed")) return(TRUE)
  problem <- rec$problem %||% "server"
  attempts <- as.integer(rec$attempts %||% 1)
  switch(problem,
    truncated = if (s$max_tokens > as.integer(rec$settings$max_tokens %||% 0)) TRUE else
      paste0("cut off at max_tokens = ", rec$settings$max_tokens, "; run again with a larger max_tokens to retry it"),
    blocked = , not_json = , control_chars = , empty =
      if (attempts < 2) TRUE else paste0(rec$message %||% problem, " (tried ", attempts, " times; use redo = \"failed\" to try again)"),
    # a newer version of the package may have fixed the cause (0.1.1 fixed
    # the image encoding that made Google refuse every page): try once more
    image = if (!identical(rec$amanuensis, as.character(utils::packageVersion("amanuensis")))) TRUE else
      paste0(rec$message %||% "Google could not use this image", " (use redo = \"failed\" to try again)"),
    TRUE
  )
}

.resolve_only <- function(x, pages, call) {
  if (inherits(x, "amanuensis_gold")) x <- unique(x$file)
  if (is.data.frame(x) && "file" %in% names(x)) x <- unique(x$file)
  if (is.numeric(x)) {
    bad <- x[x < 1 | x > nrow(pages) | x != round(x)]
    if (length(bad)) .abort("There is no page number {bad} (there are {nrow(pages)} pages).", call = call, class = "amanuensis_error_input")
    return(as.integer(x))
  }
  if (is.character(x)) {
    k <- .page_key(.stem(x))
    hit <- match(k, .page_key(.stem(pages$file)))
    if (anyNA(hit)) {
      .abort(c("These pages are not among the images: {.file {x[is.na(hit)]}}.",
               "i" = "Use file names as listed by {.fn images} (e.g. {.val {pages$file[1]}})."), call = call,
             class = "amanuensis_error_input")
    }
    return(hit)
  }
  .abort("Pages must be given as file names, page numbers, or a benchmark.", call = call, class = "amanuensis_error_input")
}

# Pages already extracted with other settings: ask (interactive) or stop.
.ask_changed <- function(changed, pages, records, run, call) {
  s <- run$settings
  recs <- records[.page_key(pages$file[changed])]
  contracts <- .read_contracts(run$dir)
  new_entry <- .contract_entry(run$contract)
  what <- character()
  old_ids <- unique(vapply(recs, function(r) r$contract %||% "", ""))
  for (id in setdiff(old_ids, run$contract$id)) {
    old_entry <- contracts[[id]]
    what <- c(what, if (is.null(old_entry)) "instructions or codebook" else .describe_change(old_entry, new_entry))
  }
  old_models <- unique(vapply(recs, function(r) r$model %||% "", ""))
  other_models <- old_models[.canonical_model(old_models) != .canonical_model(s$model)]
  if (length(other_models)) what <- c(what, paste0("model: ", paste(other_models, collapse = ", "), " -> ", s$model))
  old_res <- unique(vapply(recs, function(r) r$settings$resolution %||% "", ""))
  if (any(old_res != s$resolution)) what <- c(what, paste0("resolution: ", paste(setdiff(old_res, s$resolution), collapse = ", "), " -> ", s$resolution))
  old_th <- unique(vapply(recs, function(r) r$settings$thinking %||% "", ""))
  if (any(old_th != s$thinking)) what <- c(what, paste0("thinking: ", paste(setdiff(old_th, s$thinking), collapse = ", "), " -> ", s$thinking))
  what <- unique(what)
  n <- length(changed)
  new_folder <- .next_free_folder(run$dir)
  head <- c("{n} page{?s} in {.path {run$dir}} {cli::qty(n)}{?was/were} extracted with different settings:",
            stats::setNames(.cli_escape(what), rep("*", length(what))))
  if (!interactive() || !isTRUE(getOption("amanuensis.ask", TRUE))) {
    .abort(c(head,
             "i" = "To keep those results and save the new ones apart: {.code save_to = \"{new_folder}\"} (afterwards this same line continues there too).",
             "i" = "To redo {cli::qty(n)}{?that page/those pages} with the new settings: {.code redo = \"all\"} (old results are kept in {.path superseded/})."),
           call = call, class = "amanuensis_error_contract")
  }
  .inform(head)
  choice <- utils::menu(c(
    paste0("Keep the old results and save the new ones in ", new_folder),
    paste0("Redo these ", n, " pages with the new settings (old results kept in superseded/)"),
    "Stop"), title = "What should happen?")
  if (choice == 1) {
    .inform(c("i" = "The new results are saved in {.path {new_folder}}. Running this same line again continues there."))
    return(list(action = "restart", folder = new_folder))
  }
  if (choice == 2) return(list(action = "redo"))
  .abort("Stopped. Nothing was sent.", call = call, class = "amanuensis_error_contract")
}

.next_free_folder <- function(dir) {
  i <- 2
  repeat {
    cand <- paste0(dir, "_", i)
    if (!dir.exists(cand)) return(cand)
    i <- i + 1
  }
}

# The folders "keep the old results apart" makes next to `dir`: <dir>_2,
# <dir>_3, ... in that order (gaps allowed: one may have been deleted).
.alternate_folders <- function(dir) {
  base <- paste0(basename(dir), "_")
  cand <- list.dirs(dirname(dir), full.names = FALSE, recursive = FALSE)
  num <- substring(cand, nchar(base) + 1L)
  cand <- cand[startsWith(cand, base) & grepl("^[0-9]+$", num)]
  cand <- cand[order(as.integer(substring(cand, nchar(base) + 1L)))]
  file.path(dirname(dir), cand)
}

# The first of those folders that holds results (or a pending batch job) made
# with the current settings, or was started for them and holds nothing yet;
# NULL if none.
.find_alternate <- function(run, pages) {
  if (is.null(run$dir)) return(NULL)
  fp <- .run_fingerprint(run)
  for (alt in .alternate_folders(run$dir)) {
    recs <- suppressWarnings(.read_records(alt))
    jobs <- tryCatch(.read_jobs(alt), error = function(e) list())
    mine <- vapply(recs, function(r) identical(.record_fingerprint(r), fp), logical(1))
    job_fp <- vapply(jobs, function(j) .fingerprint(j$contract %||% "", j$settings$model %||% "",
                                                    j$settings$resolution %||% "", j$settings$thinking %||% ""), "")
    started <- !length(recs) && !length(jobs) && !is.null(.read_contracts(alt)[[run$contract$id]])
    if (any(mine) || any(job_fp == fp) || started) {
      done <- sum(mine & vapply(recs, function(r) r$status %in% c("ok", "check"), logical(1)) &
                    names(recs) %in% .page_key(pages$file))
      alt <- .show_path(alt)
      .inform(c("i" = "Continuing in {.path {alt}}, which holds results made with these settings ({done} of {nrow(pages)} page{?s} done there). The results in {.path {run$dir}} were made with other settings and stay as they are.",
                "i" = "To go there directly, use {.code save_to = \"{alt}\"}."))
      return(alt)
    }
  }
  NULL
}

.announce_plan <- function(plan, run) {
  n_scope <- length(plan$scope)
  n_done <- length(plan$done)
  n_todo <- length(plan$todo)
  where <- if (is.null(run$dir)) {
    "Nothing is written to your computer: the results come back to R (save them with output()). If the run stops, running the same line again in this R session continues it."
  } else {
    paste0("Results are saved in ", run$dir, ", one file per page, so an interrupted run can resume.")
  }
  done_keys <- .page_key(run$pages$file[plan$done])
  n_sess <- sum(done_keys %in% run$from_session)
  n_cont <- sum(done_keys %in% run$from_continue)
  earlier <- c(if (n_sess) paste0(.fmt_int(n_sess), " read earlier in this R session"),
               if (n_cont) paste0(.fmt_int(n_cont), " kept from continue"))
  earlier <- if (length(earlier)) paste0(" (", paste(earlier, collapse = ", "), ", not sent again)") else ""
  lines <- c("i" = paste0(.fmt_int(n_scope), " page", if (n_scope != 1) "s", ": ",
                          .fmt_int(n_done), " already done", earlier, ", ", .fmt_int(n_todo), " to do",
                          if (plan$in_job) paste0(", ", plan$in_job, " in a pending batch job") else "",
                          if (length(plan$held)) paste0(", ", length(plan$held), " failed earlier and not retried") else "",
                          if (plan$skipped_by_limit) paste0(" (", plan$skipped_by_limit, " more left for later: limit)") else "",
                          "."))
  if (n_todo) lines <- c(lines, "i" = where)
  n_old <- length(plan$supersede)
  if (n_old && !is.null(run$dir)) {
    lines <- c(lines, "i" = paste0(n_old, " earlier result", if (n_old != 1) "s", " will be moved to superseded/ ",
                                   "(kept, not deleted) as the new ones arrive."))
  }
  .inform(.cli_escape(lines))
  # a long run without a folder is allowed, but worth a word: what is read
  # survives Esc and errors, not closing R or a crash
  long <- getOption("amanuensis.long_run", 50)
  if (is.null(run$dir) && n_todo >= long && run$settings$engine != "batch") {
    suggest <- .show_path(.default_results_dir(run$pages))
    .inform(c("i" = paste0("A long run: if R is closed or crashes, the pages read would be lost. To keep each page on disk as it arrives ",
                           "(and resume even after closing R), add save_to = \"", suggest, "\".")))
  }
}

# "4 pages held practice results; extracting them for real now", or, when
# `limit` leaves some for later, "...; extracting 1 for real now (3 left for
# later)".
.inform_redone <- function(n, now, template) {
  if (!n) return(invisible())
  now_what <- if (now == n) (if (n == 1) "it" else "them") else if (now == 0) "none of them" else as.character(now)
  later <- if (now < n) paste0(" (", n - now, " left for later)") else ""
  .inform(c("i" = paste0(template, later, ".")))
}

# ------- the sync engine ------------------------------------------------------

.run_sync <- function(run, plan, key, workers = 1, budget = NULL, confirm = FALSE) {
  s <- run$settings
  todo <- plan$todo
  n <- length(todo)
  .log_manifest(run, n)
  ask_after <- if (confirm && n >= 25 && is.null(budget) && !s$model %in% .practice_models) 2L else NA_integer_
  status_line <- function() paste0(.fmt_usd(run$spent), " so far")
  pb <- cli::cli_progress_bar("Extracting", total = n, clear = FALSE,
                              format = "{cli::pb_spin} Page {cli::pb_current}/{cli::pb_total} {cli::pb_bar} {cli::pb_eta} | {status_line()}")
  streak <- list(n = 0L, why = "", problem = "")
  i <- 1L
  w <- min(workers, n)
  esc <- FALSE
  result <- tryCatch({
    while (i <= n) {
      # the average of the pages with a cost (a network failure costs nothing
      # and would make the next pages look cheaper than they are)
      if (!is.null(budget) && run$n_priced > 0) {
        avg <- run$spent / run$n_priced
        if (run$spent + avg * w > budget) {
          run$stopped <- paste0("Stopped before going over your budget of ", .fmt_usd(budget), " (spent ", .fmt_usd(run$spent), ").")
          break
        }
      }
      chunk <- todo[i:min(n, i + w - 1L)]
      outcomes <- .send_pages(run, chunk, key)
      for (o in outcomes) {
        # a stop (key refused, quota used up) ends the run, but the other
        # replies of the same round were paid for: they are kept
        if (identical(o$action, "stop")) {
          run$stopped <- run$stopped %||% o$message
          next
        }
        if (identical(o$action, "skip")) next
        .keep_record(run, o$record)
        # the same failure page after page is not about one page: stop
        # (5 for server or network trouble, 3 for an image Google refuses -
        # that pattern was a package bug on the first real run)
        problem <- o$record$problem %||% ""
        if (o$record$status == "failed" && problem %in% c("server", "network", "image", "request")) {
          same <- identical(problem, streak$problem) &&
            (!problem %in% c("image", "request") || identical(o$record$message, streak$why))
          streak$n <- if (same) streak$n + 1L else 1L
          streak$problem <- problem
          streak$why <- o$record$message
        } else {
          streak$n <- 0L
          streak$problem <- ""
        }
      }
      # Esc pressed during a parallel round (see .send_pages()): the round's
      # replies are kept above, and nothing more is sent
      if (isTRUE(attr(outcomes, "interrupted"))) {
        esc <- TRUE
        break
      }
      # halve once per round in which Google answered 429
      if (w > 1 && any(vapply(outcomes, function(o) isTRUE(o$slow_down), logical(1)))) {
        w <- max(1L, w %/% 2L)
        .inform(c("i" = "Google asked to slow down; sending {w} page{?s} at a time from now on."))
      }
      if (!is.null(run$stopped)) break
      if (streak$n >= (if (streak$problem %in% c("image", "request")) 3L else 5L)) {
        run$stopped <- paste0(streak$n, " pages in a row failed the same way (", streak$why, "). ",
                              "Stopping so you can check before more pages fail; run the same line again later.")
        break
      }
      cli::cli_progress_update(id = pb, inc = length(chunk))
      i <- i + length(chunk)
      if (!is.na(ask_after) && i - 1L >= ask_after && i <= n) {
        ask_after <- NA_integer_
        if (!.confirm_projection(run, n - (i - 1L))) {
          run$stopped <- "Stopped by you after the first pages; run the same line again to continue."
          break
        }
      }
    }
    "done"
  },
  interrupt = function(e) "interrupted",
  # an unexpected error (a bug, a full disk) must not take the pages already
  # paid for with it: they are kept and returned, and the error is reported
  error = function(e) {
    run$error <- e
    "error"
  })
  cli::cli_progress_done(id = pb)
  read <- length(run$written)
  if (identical(result, "interrupted") || esc) {
    run$stopped <- paste0("Stopped by you after ", read, " of ", n, " page", if (n != 1) "s", ". ", .how_to_continue(run))
    run$interrupted <- TRUE
  } else if (identical(result, "error")) {
    run$stopped <- paste0("An unexpected error stopped the run after ", read, " of ", n, " page", if (n != 1) "s",
                          ": ", .short_message(conditionMessage(run$error), 300), ". ", .how_to_continue(run),
                          " Please report the error.")
    run$interrupted <- TRUE
    .warn(c("The run stopped on an unexpected error: {(.short_message(conditionMessage(run$error), 300))}",
            "i" = "The pages read before it are kept (see the result's message)."))
  }
  run
}

# What to do after a run stopped early, with or without a results folder.
.how_to_continue <- function(run) {
  if (!is.null(run$dir)) {
    return(paste0("The pages read are saved in ", run$dir, "; run the same line again to continue."))
  }
  paste0("The pages read are in the result and kept for the rest of this R session: run the same line again ",
         "to read the others (pages already read are not sent again). If you did not assign the result, ",
         "`res <- .Last.value` gets it now.")
}

# Ask before spending on many pages, with a projection from the pages so far.
.confirm_projection <- function(run, remaining) {
  if (run$n_priced == 0) return(TRUE)
  avg <- run$spent / run$n_priced
  half <- avg / (if (run$settings$engine == "sync") 2 else 1)
  msg <- paste0(run$n_sent, " page", if (run$n_sent != 1) "s", " cost ", .fmt_usd(run$spent),
                ". The other ", .fmt_int(remaining), " would cost about ", .fmt_usd(avg * remaining),
                if (run$settings$engine == "sync") paste0(" (about ", .fmt_usd(half * remaining), " with batch = TRUE)") else "",
                " at Google's paid prices. Continue?")
  isTRUE(utils::askYesNo(msg, default = TRUE))
}

# Build and send the requests for a chunk of pages; returns one outcome per
# page: list(action = "keep"|"stop", record, message, slow_down).
.send_pages <- function(run, idx, key) {
  s <- run$settings
  pages <- run$pages
  schema <- .schema_for_api(run$contract$codebook$schema)
  reqs <- lapply(idx, function(i) {
    body <- .build_body(run$contract$instructions$full, .encode_image(pages$image[i]), .mime_of(pages$image[i]),
                        schema, s, case = "camel")
    req <- .generate_request(.to_json(body), s$model, key, s$timeout)
    if (s$model %in% .practice_models) req <- httr2::req_headers(req, `x-amanuensis-file` = pages$file[i])
    req
  })
  is_429 <- function(r) !is.null(r) && !inherits(r, "error") && httr2::resp_status(r) == 429
  slow <- FALSE
  esc <- FALSE
  responses <- .with_practice_transport(s$model, {
    if (length(reqs) == 1) {
      list(tryCatch(.gemini_perform(reqs[[1]], retries = s$retries), error = function(e) e))
    } else {
      # httr2 catches Esc inside a parallel round: it waits for the requests
      # already sent, warns "Terminating iteration" and returns normally, so
      # the interrupt never reaches .run_sync(). Its warning is the only sign:
      # it becomes a stop once this round's replies (paid for) are kept.
      first <- withCallingHandlers(
        httr2::req_perform_parallel(reqs, on_error = "continue", progress = FALSE, max_active = length(reqs)),
        message = function(m) {
          if (grepl("Terminating iteration", conditionMessage(m), fixed = TRUE)) {
            esc <<- TRUE
            invokeRestart("muffleMessage")
          }
        })
      # a 429 in the parallel round is the signal to send fewer at a time,
      # even when the retry below then succeeds
      slow <- any(vapply(first, is_429, logical(1)))
      # retry transient failures one by one, with the usual waits (not after
      # Esc: those pages are tried again on the next run)
      lapply(seq_along(first), function(j) {
        r <- first[[j]]
        if (esc || is.null(r)) return(r)
        transient <- inherits(r, "error") || .is_transient(httr2::resp_status(r))
        if (!transient) return(r)
        tryCatch(.gemini_perform(reqs[[j]], retries = s$retries), error = function(e) e)
      })
    }
  })
  slow <- slow || any(vapply(responses, is_429, logical(1)))
  out <- lapply(seq_along(idx), function(j) {
    # a request httr2 never finished after Esc: no record, sent next time
    if (is.null(responses[[j]])) return(list(action = "skip"))
    o <- .outcome_from_response(responses[[j]], run, idx[j])
    o$slow_down <- slow
    o
  })
  attr(out, "interrupted") <- esc
  out
}

.outcome_from_response <- function(resp, run, i) {
  page <- run$pages[i, ]
  s <- run$settings
  if (inherits(resp, "error")) {
    if (inherits(resp, "interrupt")) stop(resp)
    rec <- .failed_record(page, run, "network", conditionMessage(resp))
    return(list(action = "keep", record = rec))
  }
  status <- httr2::resp_status(resp)
  raw_body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  if (status >= 300) {
    why <- .classify_failure(status, .gemini_error_message(resp), model = s$model, engine = s$engine,
                             details = .gemini_error_details(resp))
    # look at Google's message only: the raw body can quote the request back
    if (status == 400 && grepl("media_resolution|mediaresolution", tolower(.gemini_error_message(resp)))) {
      why <- list(action = "stop", message = paste0("Google rejected the resolution setting (", .gemini_error_message(resp),
                                                    "). Use resolution = \"high\"."))
    }
    if (why$action == "stop") return(list(action = "stop", message = why$message))
    problem <- why$problem %||% (if (status >= 500 || status == 408) "server" else "image")
    return(list(action = "keep", record = .failed_record(page, run, problem, why$message)))
  }
  json <- tryCatch(.parse_json(raw_body), error = function(e) NULL)
  if (is.null(json)) {
    return(list(action = "keep", record = .failed_record(page, run, "server", "Google's reply could not be read.")))
  }
  list(action = "keep", record = .record_from_reply(json, raw_body, page, run))
}

# ------- turning a reply into a record ---------------------------------------

.base_record <- function(page, run, settings = run$settings, contract_id = run$contract$id) {
  s <- settings
  path <- page$image
  list(
    amanuensis = as.character(utils::packageVersion("amanuensis")),
    record_version = .record_version,
    file = page$file, source = page$source %||% page$file, source_page = as.integer(page$source_page %||% 1L),
    image_bytes = if (!is.null(path) && file.exists(path)) file.size(path) else NULL,
    status = NA_character_, problem = NULL, message = NULL, notes = list(), errors = list(),
    attempts = 1L,
    contract = contract_id,
    model = s$model, model_version = NULL,
    settings = list(resolution = s$resolution, thinking = s$thinking, max_tokens = s$max_tokens,
                    temperature = if (is.null(s$temperature)) "not sent" else s$temperature,
                    engine = s$engine, schema_channel = s$schema_channel %||% "json",
                    api_args = if (length(s$api_args)) s$api_args else NULL),
    batch_job = s$batch_job %||% NULL,
    finish_reason = NULL, tokens = NULL, price = NULL, cost_usd = NULL,
    date = .utc_now(), reply = NULL, data = NULL
  )
}

.failed_record <- function(page, run, problem, message, settings = run$settings, contract_id = run$contract$id) {
  rec <- .base_record(page, run, settings, contract_id)
  rec$status <- "failed"
  rec$problem <- problem
  rec$message <- .short_message(message, 1000)
  rec
}

.record_from_reply <- function(json, raw_body, page, run, settings = run$settings, contract_id = run$contract$id,
                               codebook = run$contract$codebook) {
  s <- settings
  rec <- .base_record(page, run, settings, contract_id)
  reply <- .read_reply(json, raw_body)
  rec$model_version <- reply$model_version
  rec$finish_reason <- reply$finish_reason
  tk <- reply$tokens
  billed_out <- if (!is.na(tk$total) && !is.na(tk$input)) tk$total - tk$input else sum(c(tk$output, tk$thinking), na.rm = TRUE)
  rec$tokens <- list(input = tk$input, cached = tk$cached, image = reply$image_tokens, thinking = tk$thinking_raw,
                     output = tk$output, billed_output = billed_out, total = tk$total)
  price <- .price_for(if (s$model %in% .practice_models) "gemini-3.8-flash" else s$model,
                      discount = s$engine %in% c("batch", "flex"), prices = s$prices)
  if (!is.null(price)) {
    rec$price <- c(price, list(discount = s$engine %in% c("batch", "flex"), date = as.character(Sys.Date())))
    rec$cost_usd <- .cost(list(input = tk$input, cached = tk$cached, output = billed_out, thinking = 0), price)
  }
  fail <- function(problem, message, raw = reply$text) {
    rec$status <- "failed"; rec$problem <- problem; rec$message <- message; rec$reply <- raw
    rec
  }
  if (!is.na(reply$block_reason)) {
    return(fail("blocked", paste0("Google blocked this page before reading it (", reply$block_reason, ")."), NULL))
  }
  if (is.na(reply$finish_reason) || reply$finish_reason != "STOP") {
    problem <- if (reply$finish_reason %in% c("MAX_TOKENS", "CONTINUATION")) "truncated" else "blocked"
    msg <- .finish_message(reply$finish_reason, s$max_tokens)
    if (problem == "truncated" && !is.null(reply$text)) {
      cap <- .model_info(s$model)$max_output %||% 65536
      if (s$max_tokens >= cap) {
        msg <- paste0("The reply was cut off at the model's limit of ", .fmt_int(cap), " tokens. ",
                      if (.repeats_punctuation(reply$text)) "It repeats one punctuation mark over and over: add 'never write the same punctuation mark more than three times in a row' to the instructions." else
                        "The page holds too much text for one reply: split the image in two.")
      }
    }
    return(fail(problem, msg))
  }
  if (isTRUE(reply$nul)) {
    return(fail("control_chars", "The reply contained NUL control characters, which would cut text short; the page will be tried again."))
  }
  if (is.null(reply$text) || !nzchar(trimws(reply$text))) return(fail("empty", "Google sent an empty reply."))
  data <- tryCatch(.parse_json(reply$text), error = function(e) NULL)
  if (is.null(data)) return(fail("not_json", "The reply was not valid JSON (it may have been cut short)."))
  rec$reply <- reply$text
  rec$data <- data
  errors <- .validate(data, codebook$schema)
  rec$notes <- as.list(.reply_notes(data, reply, s, codebook))
  if (length(errors)) {
    rec$status <- "check"
    rec$problem <- "invalid"
    rec$message <- paste0("The reply does not match the codebook: ", errors[1],
                          if (length(errors) > 1) paste0(" (and ", length(errors) - 1, " more)") else "")
    rec$errors <- as.list(errors)
  } else {
    rec$status <- "ok"
  }
  rec
}

.repeats_punctuation <- function(text) grepl("([^[:alnum:][:space:]])\\s?\\1{20,}", text, perl = TRUE)

# Cheap mechanical checks on a reply, written as sentences (stored as notes).
.reply_notes <- function(data, reply, s, cb) {
  notes <- character()
  strings <- .all_strings(data)
  if (length(strings)) {
    ctrl <- grepl("[\u0001-\u0008\u000b\u000c\u000e-\u001f\t]", strings)
    if (any(ctrl)) notes <- c(notes, paste0(sum(ctrl), " value(s) contain a tab or control character (a backslash in the source may have been misread), e.g. ", names(strings)[ctrl][1], "."))
    rep <- grepl("([^[:alnum:][:space:]])\\s?\\1{20,}", strings, perl = TRUE)
    if (any(rep)) notes <- c(notes, paste0("The model may have got stuck repeating a mark in ", names(strings)[rep][1], "."))
    sp <- grepl("^\\s|\\s$", strings) & nzchar(strings)
    if (any(sp)) notes <- c(notes, paste0(sum(sp), " value(s) start or end with a space."))
  }
  path <- .records_path(cb)
  if (!is.null(path)) {
    rows <- .page_rows(data, path)
    if (!length(rows)) notes <- c(notes, "No rows were found on this page.")
    if (length(rows)) {
      empty <- vapply(rows, function(r) all(vapply(r, function(v) is.null(v) || (length(v) == 1 && is.na(v)), logical(1))), logical(1))
      if (any(empty)) notes <- c(notes, paste0(sum(empty), " row(s) are completely empty."))
    }
  }
  billed <- reply$tokens$total - reply$tokens$input
  if (!is.na(billed) && billed > 0.9 * s$max_tokens) {
    notes <- c(notes, paste0("The reply used ", round(100 * billed / s$max_tokens), "% of max_tokens; a denser page would be cut off."))
  }
  # an alias is answered by the model it routes to; routes_to is NA for the others
  routes <- .model_info(s$model)$routes_to
  if (is.null(routes) || is.na(routes)) routes <- s$model
  if (!is.na(reply$model_version) && !s$model %in% .practice_models &&
      !startsWith(reply$model_version, s$model) && !startsWith(reply$model_version, routes)) {
    notes <- c(notes, paste0("Google answered with ", reply$model_version, " although ", s$model, " was requested."))
  }
  if (identical(s$resolution, "ultra_high") && !is.na(reply$image_tokens) && reply$image_tokens < 1600 &&
      !s$model %in% .practice_models) {
    notes <- c(notes, paste0("Only ", reply$image_tokens, " image tokens were counted: Google may have read the image at a lower resolution than ultra_high."))
  }
  notes
}

# Every string value in a parsed reply, named by its path.
.all_strings <- function(x, path = "") {
  if (is.character(x) && length(x) == 1) return(stats::setNames(x, if (nzchar(path)) path else "value"))
  if (!is.list(x)) return(character())
  nm <- names(x)
  out <- character()
  for (i in seq_along(x)) {
    p <- if (is.null(nm)) paste0(path, "[", i, "]") else if (nzchar(path)) paste0(path, ".", nm[i]) else nm[i]
    out <- c(out, .all_strings(x[[i]], p))
  }
  out
}

# Store a new record (disk + memory), carrying over the attempt count of a
# failed earlier record, and log it. Used for replies to requests sent now and
# for pages collected from batch jobs.
#
# A page's earlier result (ok or to check) is moved to superseded/ only here,
# when its replacement has arrived: until then it stays where it was, so a
# run that stops early never leaves a page without its result. Failed records
# hold no data and practice replies cost nothing: those are just replaced.
.keep_record <- function(run, rec) {
  k <- .page_key(rec$file)
  old <- run$records[[k]]
  if (!is.null(old) && identical(old$status, "failed") && identical(rec$status, "failed")) {
    rec$attempts <- as.integer(old$attempts %||% 1L) + 1L
  }
  if (isTRUE(old$status %in% c("ok", "check")) && !(old$model %||% "") %in% .practice_models && !is.null(run$dir)) {
    run$moved_to <- run$moved_to %||% .superseded_dir(run$dir, run$started)
    run$moved <- run$moved + .supersede(run$dir, old$file, dest = run$moved_to, quiet = TRUE)
  }
  run$records[[k]] <- rec
  .write_record(run$dir, rec)
  # without a folder, this session keeps it: the same line run again reuses it
  if (is.null(run$dir)) .session_keep(rec, run$pages$image[match(k, .page_key(run$pages$file))])
  run$statuses <- c(run$statuses, rec$status)
  run$written <- c(run$written, k)
  # a page of a batch job sent by an earlier call was paid for then
  if (!is.null(rec$batch_job) && rec$batch_job %in% run$earlier_jobs) {
    run$collected <- run$collected + 1
    .log_page(run, rec)
    return(invisible(rec))
  }
  run$n_sent <- run$n_sent + 1
  if (length(rec$cost_usd) == 1 && is.numeric(rec$cost_usd) && !is.na(rec$cost_usd)) {
    run$spent <- run$spent + rec$cost_usd
    run$n_priced <- run$n_priced + 1
  }
  .log_page(run, rec)
  invisible(rec)
}

# ------- logs (opt-in) --------------------------------------------------------

.log_dir <- function(log, dir, call = rlang::caller_env()) {
  if (isFALSE(log) || is.null(log)) return(NULL)
  if (isTRUE(log)) {
    if (is.null(dir)) {
      .abort(c("{.code log = TRUE} writes the log files into the results folder, and no folder was given.",
               "i" = "Add {.code save_to = \"a folder\"}, or give a folder for the logs alone: {.code log = \"logs\"}."),
             call = call, class = "amanuensis_error_input")
    }
    return(dir)
  }
  .check_string(log, arg = "log", call = call)
  if (!dir.exists(log)) dir.create(log, recursive = TRUE)
  .show_path(log)
}

.log_manifest <- function(run, n_images, extra = list()) {
  if (is.null(run$log_dir)) return(invisible())
  s <- run$settings
  line <- c(list(
    date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    script = paste0("amanuensis ", utils::packageVersion("amanuensis")),
    model = s$model,
    temperature = if (is.null(s$temperature)) "not sent" else s$temperature,
    media_resolution = .media_level(s$resolution),
    thinking_level = s$thinking_level,
    max_output_tokens = s$max_tokens,
    prompt_sha256 = run$contract$instructions$sha12,
    schema_sha256 = run$contract$codebook$sha12,
    images = n_images,
    engine = s$engine,
    schema_channel = s$schema_channel %||% "json",
    api_args = if (length(s$api_args)) s$api_args else NULL,
    r_version = paste(R.version$major, R.version$minor, sep = ".")
  ), extra)
  .append_text(.to_json(line), file.path(run$log_dir, "run_manifest.jsonl"))
}

.log_page <- function(run, rec) {
  if (is.null(run$log_dir)) return(invisible())
  tk <- rec$tokens
  if (!is.null(tk)) {
    fmt <- function(v) if (is.null(v) || is.na(v)) "None" else format(v, scientific = FALSE)
    .append_text(paste0(rec$file, "\tinput=", fmt(tk$input), "\tthinking=", fmt(tk$thinking),
                        "\toutput=", fmt(tk$output), "\ttotal=", fmt(tk$total)),
                 file.path(run$log_dir, "token_usage.txt"))
  }
  if (!identical(rec$status, "ok")) {
    .append_text(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), rec$file, rec$message %||% rec$status),
                 file.path(run$log_dir, "errors.log"))
  }
}
