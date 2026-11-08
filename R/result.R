# The object transcribe() returns: a plain data frame of records (so
# View(), dplyr and write.csv just work) with one attribute, "amanuensis",
# an environment holding the pages table, the page records (with the model's
# data), the codebook, the instructions and the run's settings. An
# environment, so copies and subsets of the table do not copy the data, and
# str() stays readable. Subsetting keeps the attribute; printing shows the run
# header only for the complete table.

# A result holds only the page records made with the run's own settings. The
# folder can hold others: results made with an earlier codebook or other
# settings that a partial redo (only, limit, budget, Esc) did not reach yet,
# or invented practice replies under a real model. Those are never flattened
# into the table: the page is listed as "not run", with the reason.
.finish <- function(run, pages, plan = NULL) {
  recs <- run$records
  keys <- .page_key(pages$file)
  page_recs <- recs[intersect(keys, names(recs))]
  fp <- .run_fingerprint(run)
  mine <- vapply(page_recs, .belongs_to, logical(1), fp = fp, model = run$settings$model)
  left_out <- vapply(page_recs[!mine], .why_left_out, "", settings = run$settings, contract_id = run$contract$id)
  page_recs <- page_recs[mine]
  waiting <- .pending_job_files(run$dir)
  waiting <- stats::setNames(rep("Waiting in a batch job at Google; run the same line later to collect it.", length(waiting)),
                             .page_key(waiting))
  # an earlier failure of a page now waiting in a batch job is not its state
  stale_fail <- names(page_recs)[names(page_recs) %in% names(waiting) &
                                   vapply(page_recs, function(r) identical(r$status, "failed"), logical(1))]
  page_recs <- page_recs[setdiff(names(page_recs), stale_fail)]
  why <- c(waiting, left_out[setdiff(names(left_out), names(waiting))])
  info <- .new_result_info(run, pages, page_recs, why)
  tab <- .flatten_records(Filter(function(r) identical(r$status, "ok"), page_recs), run$contract$codebook)
  out <- .new_result(tab, info)
  .report_run(out, run, plan, left_out = left_out[setdiff(names(left_out), names(waiting))],
              waiting = names(waiting))
  if (isTRUE(run$interrupted)) return(invisible(out))
  out
}

# Whether a page record belongs to a result made with fingerprint `fp` by
# `model`: data (ok, to check) only when made with the same contract and
# settings; a failure holds no data and is reported whatever made it. Practice
# replies never count under a real model.
.belongs_to <- function(rec, fp, model) {
  if ((rec$model %||% "") %in% .practice_models && !model %in% .practice_models) return(FALSE)
  if (identical(rec$status, "failed")) return(TRUE)
  identical(.record_fingerprint(rec), fp)
}

# The pages table's message for a page whose record is left out.
.why_left_out <- function(rec, settings, contract_id) {
  if ((rec$model %||% "") %in% .practice_models) {
    return(paste0("Has only a practice (", rec$model, ") result, which is never mixed with real ones."))
  }
  what <- c(if (!identical(rec$contract %||% "", contract_id)) "other instructions or codebook",
            if (!identical(.canonical_model(rec$model %||% ""), .canonical_model(settings$model %||% ""))) paste0("model ", rec$model %||% "unknown"),
            if (!identical(rec$settings$resolution %||% "", settings$resolution %||% "")) paste0("resolution ", rec$settings$resolution %||% "unknown"),
            if (!identical(rec$settings$thinking %||% "", settings$thinking %||% "")) paste0("thinking ", rec$settings$thinking %||% "unknown"))
  paste0("Has only a result made with other settings (", paste(what, collapse = "; "), "); not part of this result.")
}

.new_result_info <- function(run, pages, page_recs, why = character()) {
  info <- new.env(parent = emptyenv())
  info$pages <- .pages_table(pages, page_recs, run$contract$codebook, why)
  info$records <- page_recs
  info$codebook <- run$contract$codebook
  info$instructions <- run$contract$instructions
  info$contract <- run$contract$id
  info$settings <- run$settings[c("model", "resolution", "thinking", "max_tokens", "engine")]
  info$folder <- run$dir
  info$date <- .utc_now()
  info
}

.new_result <- function(tab, info) {
  info$n_rows <- nrow(tab)
  attr(tab, "amanuensis") <- info
  class(tab) <- c("amanuensis_result", "data.frame")
  tab
}

.result_info <- function(x) {
  info <- attr(x, "amanuensis", exact = TRUE)
  if (is.environment(info)) info else NULL
}

# The records table rebuilt from the page records: include = "ok" (the
# default table) or "all" (pages to check too, with a page_status column).
.result_table <- function(x, include = c("ok", "all")) {
  include <- match.arg(include)
  info <- .result_info(x)
  keep <- if (include == "all") c("ok", "check") else "ok"
  recs <- Filter(function(r) r$status %in% keep, info$records)
  tab <- .flatten_records(recs, info$codebook)
  if (include == "all" && nrow(tab)) {
    status <- vapply(recs, function(r) r$status, "")
    names(status) <- vapply(recs, function(r) .page_key(r$file), "")
    ps <- unname(status[.page_key(tab$file)])
    pos <- match("source_page", names(tab))
    tab <- data.frame(tab[seq_len(pos)], page_status = ps, tab[-seq_len(pos)],
                      stringsAsFactors = FALSE, check.names = FALSE)
  }
  tab
}

# Per-page units of a result for scoring: list keyed by file, each
# list(status, page, rows) - see .page_unit(). Pages to check are included
# (their data is what the model returned); failed pages have status "failed"
# and no data; pages never run are absent.
.result_units <- function(x) {
  info <- .result_info(x)
  out <- list()
  for (r in info$records) {
    u <- .page_unit(if (r$status %in% c("ok", "check")) r$data else NULL, info$codebook)
    u$status <- r$status
    out[[r$file]] <- u
  }
  out
}

# One row per page image in scope: what happened to it. `why`, keyed by
# .page_key(), is the message of a page without a record in this result.
.pages_table <- function(pages, page_recs, cb, why = character()) {
  path <- .records_path(cb)
  rows <- lapply(seq_len(nrow(pages)), function(i) {
    k <- .page_key(pages$file[i])
    rec <- page_recs[[k]]
    tk <- rec$tokens %||% list()
    num <- function(v) if (is.null(v)) NA_real_ else as.numeric(v)
    n_rows <- if (!is.null(rec$data)) {
      if (is.null(path)) 1L else length(.page_rows(rec$data, path))
    } else NA_integer_
    data.frame(
      file = pages$file[i], source = pages$source[i] %||% pages$file[i],
      source_page = as.integer(pages$source_page[i] %||% 1L),
      status = rec$status %||% "not run",
      problem = rec$problem %||% NA_character_,
      n_rows = as.integer(n_rows),
      model = rec$model %||% NA_character_,
      model_version = rec$model_version %||% NA_character_,
      engine = rec$settings$engine %||% NA_character_,
      finish_reason = rec$finish_reason %||% NA_character_,
      input_tokens = num(tk$input), cached_tokens = num(tk$cached), image_tokens = num(tk$image),
      thinking_tokens = num(tk$thinking), output_tokens = num(tk$output),
      cost_usd = num(rec$cost_usd),
      attempts = as.integer(rec$attempts %||% NA),
      notes = paste(unlist(rec$notes), collapse = " "),
      message = rec$message %||% (if (k %in% names(why)) unname(why[[k]]) else NA_character_),
      date = rec$date %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# The end-of-run summary. The headline says what this run did (pages sent,
# pages collected from a batch job, nothing to send, or nothing read and
# why), with a success bullet only when it went well; the lines below
# describe every page of the result.
.report_run <- function(x, run, plan, left_out = character(), waiting = character()) {
  info <- .result_info(x)
  p <- info$pages
  s <- run$settings
  n <- nrow(p)
  counts <- table(factor(p$status, levels = c("ok", "check", "failed", "not run")))
  is_waiting <- p$status == "not run" & .page_key(p$file) %in% waiting
  n_wait <- sum(is_waiting)
  n_left <- counts[["not run"]] - n_wait
  cost <- sum(p$cost_usd, na.rm = TRUE)
  sent <- run$n_sent %||% 0
  collected <- run$collected %||% 0
  got <- run$statuses %||% character()
  how <- paste0(" (resolution ", s$resolution, ", thinking ", s$thinking,
                if (s$engine != "sync") paste0(", ", s$engine) else "", ")")
  if (sent + collected > 0) {
    head <- paste0(paste(c(
      if (sent) paste0("Sent ", .fmt_int(sent), " page", if (sent != 1) "s", " to ", s$model, how),
      if (collected) paste0(if (sent) "collected " else "Collected ", .fmt_int(collected), " page",
                            if (collected != 1) "s", " from a batch job sent earlier")), collapse = "; "), ".")
    bullet <- if (!any(got %in% c("ok", "check"))) "x" else if (is.null(run$stopped) && all(got == "ok")) "v" else "!"
  } else if (n_wait) {
    head <- paste0(n_wait, " page", if (n_wait != 1) "s", " waiting in a batch job at Google; run the same line later to collect ",
                   if (n_wait != 1) "them" else "it", ".")
    bullet <- "i"
  } else if (!is.null(run$stopped)) {
    head <- "No page was read in this run."
    bullet <- "x"
  } else if (!is.null(plan) && length(plan$held)) {
    head <- paste0("Nothing to send: ", length(plan$held), " page", if (length(plan$held) != 1) "s",
                   " that failed earlier ", if (length(plan$held) != 1) "are" else "is", " not retried (see below).")
    bullet <- "!"
  } else {
    head <- paste0("Nothing to send: every page", if (!is.null(plan) && length(plan$scope) < n) " you chose" else "",
                   " is already done. (To extract pages again, use `redo`.)")
    bullet <- "v"
  }
  parts <- c(if (counts[["ok"]]) paste0(counts[["ok"]], " ok"),
             if (counts[["check"]]) paste0(counts[["check"]], " to check"),
             if (counts[["failed"]]) paste0(counts[["failed"]], " failed"),
             if (n_wait) paste0(n_wait, " waiting in a batch job"),
             if (n_left) paste0(n_left, " not run yet"))
  price_note <- if (s$model %in% .practice_models) " (practice model: nothing was charged; shown at gemini-3.8-flash prices)" else
    paste0(" at Google's paid prices of ", .models_checked, if (s$engine != "sync") " (half price)" else "", "; keys without billing are not charged")
  lines <- stats::setNames(head, bullet)
  if (!is.null(run$stopped)) lines <- c(lines, "!" = run$stopped)
  lines <- c(lines,
             "*" = paste0("Of the ", .fmt_int(n), " page", if (n != 1) "s", ": ", paste(parts, collapse = ", "), "."),
             "*" = paste0(.fmt_int(nrow(x)), " row", if (nrow(x) != 1) "s", " in the table; estimated cost ",
                          .fmt_usd(cost), " for all pages so far", price_note, "."))
  if (run$spent > 0 && abs(run$spent - cost) > 1e-9) {
    lines <- c(lines, "*" = paste0("This run: ", .fmt_usd(run$spent), "."))
  }
  if (isTRUE(run$moved > 0)) {
    lines <- c(lines, "*" = paste0("Moved ", run$moved, " earlier result", if (run$moved != 1) "s", " to ",
                                   .show_path(run$moved_to), " (kept, not deleted)."))
  }
  not_reached <- if (is.null(plan) || is.null(info$folder)) 0L else
    sum(!.page_key(plan$supersede) %in% c(run$written, waiting))
  if (not_reached) {
    lines <- c(lines, "*" = paste0(not_reached, " page", if (not_reached != 1) "s", " to redo ",
                                   if (not_reached != 1) "were" else "was", " not reached and ",
                                   if (not_reached != 1) "keep their" else "keeps its", " earlier result",
                                   if (not_reached != 1) "s", "."))
  }
  if (!is.null(info$folder)) lines <- c(lines, "*" = paste0("Saved in ", info$folder, "."))
  .inform(.cli_escape(lines))

  if (length(left_out)) {
    practice <- grepl("practice", left_out, fixed = TRUE)
    if (any(practice)) {
      .inform(c("!" = "{sum(practice)} page{?s} {?has/have} only practice ({.val mock}/{.val example}) results, never mixed with real ones: not in this table.",
                "i" = "Run the same line again to extract {cli::qty(sum(practice))}{?it/them} for real."))
    }
    if (any(!practice)) {
      .inform(c("!" = "{sum(!practice)} page{?s} {?has/have} only results made with other settings (instructions, codebook, model, resolution or thinking): not in this table.",
                "i" = "Their old results stay in {.path {info$folder %||% 'the results folder'}}; the column {.field message} of {.code output(res, table = \"pages\")} says what differs."))
    }
  }
  chk <- p[p$status == "check", , drop = FALSE]
  if (nrow(chk)) {
    .inform(c("!" = "{nrow(chk)} page{?s} to check (the reply did not match the codebook; kept apart from the table): {.file {utils::head(chk$file, 3)}}{if (nrow(chk) > 3) ' ...' else ''}.",
              "i" = "{chk$message[1]}",
              "i" = "Include them with {.code output(res, include = \"all\")}, or extract them again with {.code redo = \"check\"}."))
  }
  fl <- p[p$status == "failed", , drop = FALSE]
  if (nrow(fl)) {
    retried <- is.na(fl$problem) | fl$problem %in% c("server", "network", "batch", "request")
    msg <- c("!" = "{nrow(fl)} page{?s} failed: {.file {utils::head(fl$file, 3)}}{if (nrow(fl) > 3) ' ...' else ''}.",
             "i" = "{fl$message[1]}")
    if (any(retried)) msg <- c(msg, "i" = "{sum(retried)} of them will be tried again when you run the same line.")
    .inform(msg)
  }
  if (!is.null(plan) && length(plan$held)) {
    .inform(c("!" = "{length(plan$held)} page{?s} that failed earlier {?was/were} not retried:",
              stats::setNames(.cli_escape(paste0(names(plan$held), ": ", unlist(plan$held))), rep("*", length(plan$held)))))
  }
  if (n_left) {
    per <- .standard_cost_per_page(info$records, s)
    if (!is.na(per) && per > 0) {
      if (s$engine == "sync") {
        .inform(c("i" = "The {n_left} page{?s} not run yet would cost about {(.fmt_usd(per * n_left))} ({(.fmt_usd(per * n_left / 2))} with batch = TRUE)."))
      } else {
        .inform(c("i" = "The {n_left} page{?s} not run yet would cost about {(.fmt_usd(per * n_left / 2))} with {s$engine} ({(.fmt_usd(per * n_left))} one page at a time)."))
      }
    }
  }
  notes <- sum(nzchar(p$notes))
  if (notes) .inform(c("i" = "{notes} page{?s} carry notes worth a look (column {.field notes} of {.code output(res, table = \"pages\")})."))
  if (isTRUE(getOption("amanuensis.next_steps", TRUE))) {
    if (nrow(x) && is.null(info$folder)) {
      .inform(c(">" = "Next: {.code output(res, \"results.xlsx\")} saves the table (nothing has been written to your computer yet); {.code output(res, table = \"pages\")} lists every page."))
    } else if (nrow(x)) {
      .inform(c(">" = "Next: {.code output(res, \"results.xlsx\")} saves the table; {.code output(res, table = \"pages\")} lists every page."))
    } else if (is.null(run$stopped) && !n_wait) {
      .inform(c(">" = "Next: {.code output(res, table = \"pages\")} lists every page and what happened to it."))
    }
  }
  invisible()
}

# What one page would cost at the standard price (one page at a time), from
# the tokens of the result's pages: each record's own cost_usd carries its
# engine's discount (batch and flex cost half), so it cannot be averaged as is.
.standard_cost_per_page <- function(recs, s) {
  price <- .price_for(if (s$model %in% .practice_models) "gemini-3.8-flash" else s$model, prices = s$prices)
  if (is.null(price)) return(NA_real_)
  num <- function(v) if (is.null(v) || !length(v) || is.na(v)) NA_real_ else as.numeric(v)
  costs <- vapply(recs, function(r) {
    tk <- r$tokens
    if (!r$status %in% c("ok", "check") || is.null(tk) || is.na(num(tk$input))) return(NA_real_)
    cached <- num(tk$cached)
    out <- num(tk$billed_output)
    .cost(list(input = num(tk$input), cached = if (is.na(cached)) 0 else cached,
               output = if (is.na(out)) 0 else out, thinking = 0), price)
  }, numeric(1))
  if (!any(!is.na(costs))) return(NA_real_)
  mean(costs, na.rm = TRUE)
}

#' @export
print.amanuensis_result <- function(x, ...) {
  info <- .result_info(x)
  df <- x
  attr(df, "amanuensis") <- NULL
  class(df) <- "data.frame"
  if (!is.null(info) && identical(nrow(x), info$n_rows)) {
    p <- info$pages
    s <- info$settings
    counts <- table(factor(p$status, levels = c("ok", "check", "failed", "not run")))
    cat("amanuensis result: ", .fmt_int(nrow(df)), " rows from ", counts[["ok"]], " of ", nrow(p), " pages",
        " (", s$model, ")", sep = "")
    if (counts[["check"]] + counts[["failed"]] + counts[["not run"]]) {
      cat(" | ", counts[["check"]], " to check, ", counts[["failed"]], " failed, ", counts[["not run"]], " not run", sep = "")
    }
    cat("\n")
  } else if (!is.null(info)) {
    cat("(part of an amanuensis result: ", nrow(df), " of ", info$n_rows, " rows)\n", sep = "")
  }
  .print_table(df)
  invisible(x)
}

# Print a data frame cut to the console: long text cells shortened, at most
# 10 rows.
.print_table <- function(df, n = 10) {
  show <- utils::head(df, n)
  width <- max(20, getOption("width", 80) %/% max(1, min(ncol(show), 6)))
  for (j in seq_along(show)) {
    if (is.character(show[[j]])) {
      v <- gsub("\n", " / ", show[[j]], fixed = TRUE)
      long <- !is.na(v) & nchar(v) > width
      v[long] <- paste0(substr(v[long], 1, width - 3), "...")
      show[[j]] <- v
    }
  }
  if (!nrow(show)) {
    cat("(no rows)\n")
  } else {
    print(show, row.names = FALSE, right = FALSE)
    if (nrow(df) > n) cat("... and", .fmt_int(nrow(df) - n), "more rows\n")
  }
  invisible(df)
}

#' @export
`[.amanuensis_result` <- function(x, i, j, ..., drop = FALSE) {
  info <- attr(x, "amanuensis", exact = TRUE)
  df <- x
  class(df) <- "data.frame"
  out <- if (missing(i) && missing(j)) df[, , drop = drop] else if (missing(j)) {
    if (nargs() == 2) df[i] else df[i, , drop = drop]
  } else if (missing(i)) df[, j, drop = drop] else df[i, j, drop = drop]
  if (is.data.frame(out)) {
    attr(out, "amanuensis") <- info
    class(out) <- c("amanuensis_result", "data.frame")
  }
  out
}

#' @export
as.data.frame.amanuensis_result <- function(x, ...) {
  attr(x, "amanuensis") <- NULL
  class(x) <- "data.frame"
  x
}

# Rebuild a result from a results folder alone (a later session): the page
# records found, flattened with the contract stored in contract.json. With
# more than one contract, `contract` picks one. As in transcribe(), a
# result never mixes settings: practice replies are left out when the folder
# also holds real results, and of the records made with the contract only
# those with the settings of the newest one are used (the others, left from
# a partial redo, are listed as not run with the reason).
.result_from_folder <- function(dir, contract = NULL, call = rlang::caller_env()) {
  if (!dir.exists(dir)) .abort("Cannot find the folder {.path {(.show_path(dir))}}.", call = call, class = "amanuensis_error_input")
  dir <- .show_path(dir)
  recs <- .read_records(dir)
  if (!length(recs)) {
    .abort(c("{.path {dir}} holds no results.", "i" = "Give the folder {.fn transcribe} saved into (its {.arg save_to})."),
           call = call, class = "amanuensis_error_input")
  }
  contracts <- .read_contracts(dir)
  is_practice <- function(r) (r$model %||% "") %in% .practice_models
  real <- !vapply(recs, is_practice, logical(1))
  used <- unique(vapply(recs[if (any(real)) real else TRUE], function(r) r$contract %||% "", ""))
  used <- used[nzchar(used)]
  if (is.null(contract)) {
    if (length(used) > 1) {
      .abort(c("{.path {dir}} holds results made with {length(used)} different instructions/codebooks.",
               "i" = "Choose one with {.code contract = } one of {.val {used}} (see {.file contract.json})."),
             call = call, class = "amanuensis_error_contract")
    }
    contract <- used[1]
  }
  entry <- contracts[[contract]]
  if (is.null(entry)) .abort("No contract {.val {contract}} in {.file contract.json}.", call = call, class = "amanuensis_error_contract")
  objs <- .contract_objects(entry, contract)
  recs <- Filter(function(r) identical(r$contract, contract), recs)
  if (!length(recs)) .abort("No results in {.path {dir}} were made with contract {.val {contract}}.", call = call, class = "amanuensis_error_contract")
  ord <- .natural_order(vapply(recs, function(r) r$file, ""))
  recs <- recs[ord]
  # the settings of the newest real data record (else the newest record)
  has_data <- vapply(recs, function(r) r$status %in% c("ok", "check"), logical(1))
  real <- !vapply(recs, is_practice, logical(1))
  cand <- which(has_data & real)
  if (!length(cand)) cand <- which(has_data)
  if (!length(cand)) cand <- seq_along(recs)
  dates <- vapply(recs[cand], function(r) as.character(r$date %||% ""), "")
  pick <- recs[[cand[order(dates, decreasing = TRUE)[1]]]]
  settings <- list(model = pick$model, resolution = pick$settings$resolution, thinking = pick$settings$thinking,
                   max_tokens = pick$settings$max_tokens, engine = pick$settings$engine)
  mine <- vapply(recs, .belongs_to, logical(1), fp = .record_fingerprint(pick), model = pick$model %||% "")
  pages <- data.frame(image = NA_character_, file = vapply(recs, function(r) r$file, ""),
                      source = vapply(recs, function(r) r$source %||% r$file, ""),
                      source_page = vapply(recs, function(r) as.integer(r$source_page %||% NA), 1L),
                      stringsAsFactors = FALSE)
  keys <- .page_key(pages$file)
  why <- stats::setNames(vapply(recs[!mine], .why_left_out, "", settings = settings, contract_id = contract), keys[!mine])
  run <- list(contract = list(id = contract, codebook = objs$codebook, instructions = objs$instructions),
              settings = settings, dir = dir)
  info <- .new_result_info(run, pages, stats::setNames(recs[mine], keys[mine]), why)
  tab <- .flatten_records(Filter(function(r) identical(r$status, "ok"), recs[mine]), objs$codebook)
  if (any(!mine)) {
    n_out <- sum(!mine)
    .inform(c("!" = "{n_out} page{?s} in {.path {dir}} {cli::qty(n_out)}{?has/have} only results made with other settings or by a practice model: left out of the table (they are listed as not run).",
              "i" = "The table holds the results made with {.val {pick$model}} (resolution {pick$settings$resolution}, thinking {pick$settings$thinking}); the column {.field message} of {.code output(..., table = \"pages\")} says what differs."))
  }
  .new_result(tab, info)
}

.as_result <- function(x, call = rlang::caller_env()) {
  if (inherits(x, "amanuensis_result") && !is.null(.result_info(x))) return(x)
  if (.is_string(x) && dir.exists(x)) return(.result_from_folder(x, call = call))
  .abort(c("Expected the result of {.fn transcribe}, or its results folder.",
           "i" = "For example {.code output(res, \"results.xlsx\")} or {.code output(\"results\", \"results.xlsx\")} for a folder given as {.arg save_to}."),
         call = call, class = "amanuensis_error_input")
}
