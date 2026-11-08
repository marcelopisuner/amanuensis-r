# score(): how close the model's output is to the user's own transcription
# (the benchmark), with these formulas:
#
#   rows        rows whose every compared row variable matches / row pairs
#               (the sum over pages of the pairs made, so extra rows in the
#               output enlarge the denominator)
#   values      matching values / values compared (row variables in every
#               pair, page variables once per page)
#   characters  1 - sum(edits) / sum(max(nchar(gold), nchar(output))), text
#               variables only, edits counted only for wrong values, empty
#               counted as ""
#
# Judgment calls, each argued where it is made below:
# - Strict by default: text is compared exactly after Unicode NFC, with ""
#   counted as empty (a spreadsheet cannot tell them apart). The lenient
#   rates (normalize = TRUE) are reported beside the strict ones, never
#   instead of them.
# - A gold page with no record in the output is "not run" and left out of
#   every rate; a page that failed counts as entirely wrong. Otherwise the
#   hardest page (often the densest, the one that was cut off) would drop
#   out silently and flatter the model.
# - A row present on one side only is paired with a MISSING sentinel, never
#   with a row of NA values (which would match every empty gold cell).
# - Rows are paired by position when both sides have the same number of rows
#   and by content when they differ (align = "auto"): one skipped row must not
#   turn every later row of the page into a mismatch.
# - A benchmark of the first N rows only (benchmark(rows = N)) says nothing
#   about where the page ends, so equal counts prove nothing there: a page
#   whose benchmark reaches N rows is always paired by content, against all
#   of the output's rows, and the output's rows after the one paired with the
#   benchmark's last row are left out (they lie beyond what was typed, so
#   they are neither right nor wrong). Cutting the output at N rows instead
#   would count the row after a skipped one as an extra error.
# - The benchmark is read the same way whatever form it comes in: a data
#   frame goes through benchmark()'s own reader (types, file names, rows
#   numbered when the row column is blank), and only the variables it has a
#   column for are compared. A missing column scored as empty would count
#   every value the model read there as wrong.
# - An empty denominator gives NA, never 100%.

#' Measure how accurate the extraction is
#'
#' Compares the model's output with your benchmark (your own transcription
#' of a few pages, made with [benchmark()]) and tells you how much of it is
#' right: rows with every value right, values right, and characters right.
#' It also lists every difference, so you can see what kind of mistakes the
#' model makes before you trust the whole table.
#'
#' @section How it measures:
#' Pages are paired by file name. On each page the rows of your benchmark
#' are paired with the rows of the output; every value is then compared
#' after converting both sides to the codebook's type (text exactly as
#' written, after a standard Unicode normalisation; numbers and yes/no by
#' value). An empty cell matches an empty cell. Three figures follow:
#' \describe{
#'   \item{Rows with every value right}{Rows whose every variable is right,
#'     out of all rows paired (a row missing from the output, or one the
#'     output added, counts as a wrong row).}
#'   \item{Values right}{Values that match, out of all values compared.
#'     Page-level variables count once per page.}
#'   \item{Characters right}{For text variables: one minus the number of
#'     single-character edits needed to turn the output into your benchmark,
#'     divided by the number of characters. A small misreading costs little
#'     here and a lot in the other two figures.}
#' }
#' For free text (a codebook made with `codebook(free_text = TRUE)`) the
#' figures are characters right and words right instead.
#'
#' A page of your benchmark that has not been extracted yet is left out and
#' listed. A page whose extraction failed counts as entirely wrong: leaving it
#' out would flatter the model.
#'
#' @section Pairing rows:
#' With `align = "auto"` (the default), rows are paired in order when your
#' benchmark and the output have the same number of rows on a page. When the
#' numbers differ, rows are paired by their content (in order, allowing
#' gaps), so that one row the model skipped counts as one missing row rather
#' than making every later row wrong. The printout says which pages were
#' paired this way. `align = "position"` always pairs in order;
#' `align = "content"` always pairs by content.
#'
#' When your benchmark covers only the first rows of each page
#' (`benchmark(rows = 20)`), a page where you typed all 20 is paired by
#' content, and the output's rows after the one that pairs with your last
#' row are left out: they lie beyond what you typed.
#'
#' @section Comparing two runs:
#' Give two extraction results (or their folders) to see where they
#' disagree, for example two models or two versions of your instructions:
#' `score(run1, run2)` treats `run1` as the reference. Disagreement shows
#' where to look; it does not tell you which run is right.
#'
#' @param benchmark Your benchmark: the result of [benchmark()], or the
#'   benchmark file it wrote (e.g. `"gold.xlsx"`). It can also be another
#'   extraction result (or its results folder), to compare two runs, or a
#'   data frame in the layout of the results table (`file`, `row`, then the
#'   variables). A data frame is read like a benchmark file: every page with
#'   a value counts as done, and only the variables it has a column for are
#'   compared.
#' @param output The results to check: the result of [transcribe()], or
#'   its results folder.
#' @param align How rows are paired on each page: `"auto"` (default),
#'   `"position"` or `"content"` (see "Pairing rows").
#' @param normalize `TRUE` to also report lenient figures that ignore
#'   differences of capital letters, accents, punctuation, spacing and doubt
#'   marks ("?"), next to the strict ones. Numbers are never changed.
#'   Default `FALSE`.
#' @param report A file name ending in `.html` to also write a report you can
#'   open in a web browser: the figures, a table per variable, and every page
#'   side by side with the wrong values marked. An existing file of that name
#'   is replaced.
#' @return An `amanuensis_score` object, printed as a short summary. It is a
#'   list of tables you can look at with `View()`:
#'   \describe{
#'     \item{summary}{Each figure (`measure`), as a share (`value`) with its
#'       counts (`matched` of `compared`).}
#'     \item{by_variable}{One row per variable: values compared, wrong values
#'       (`differences`), share right, characters right and edits (text
#'       variables).}
#'     \item{by_page}{One row per page of your benchmark: its status in the
#'       output, how rows were paired, rows on each side, rows missing from
#'       the output, extra rows, and wrong values.}
#'     \item{differences}{Every wrong value: file, row in your benchmark, row
#'       in the output, variable, both values, the edits between them, and a
#'       note (missing row, extra row, failed page, or a difference of a
#'       final "?" only).}
#'     \item{not_run}{Pages of your benchmark not extracted yet.}
#'     \item{notes}{Anything worth knowing about how the figures were made.}
#'   }
#' @seealso [benchmark()] to make the benchmark; [transcribe()] with
#'   `only = ` to extract just the benchmark pages.
#' @export
#' @examples
#' ex <- example_project()
#' pages <- images(ex$scans, save_to = file.path(ex$folder, "pages"))
#' res <- transcribe(pages, ex$instructions, ex$codebook, model = "example")
#' s <- score(ex$gold, res)
#' s
#' s$differences
#'
#' # lenient figures too, and a report to open in a browser
#' score(ex$gold, res, normalize = TRUE, report = file.path(ex$folder, "accuracy.html"))
score <- function(benchmark, output, align = c("auto", "position", "content"), normalize = FALSE,
                  report = NULL) {
  if (missing(benchmark) || missing(output)) {
    .abort(c("Give your benchmark and the results to check.",
             "i" = "For example {.code score(\"gold.xlsx\", res)}, where {.code res} comes from {.fn transcribe}."),
           class = "amanuensis_error_input")
  }
  choices <- c("auto", "position", "content")
  if (identical(align, choices)) align <- "auto"
  align <- .check_choice(align, choices, arg = "align")
  if (!isTRUE(normalize) && !isFALSE(normalize)) {
    .abort("{.arg normalize} must be {.code TRUE} or {.code FALSE}.", class = "amanuensis_error_input")
  }
  if (!is.null(report)) {
    .check_string(report, arg = "report")
    if (!.ext(report) %in% c("html", "htm")) {
      .abort(c("The report must be an HTML file.", "i" = "For example {.code report = \"accuracy.html\"}."),
             class = "amanuensis_error_input")
    }
  }
  if (inherits(output, "amanuensis_gold")) {
    .abort(c("Your benchmark goes first and the results second.", "i" = "For example {.code score(gold, res)}."),
           class = "amanuensis_error_input")
  }
  out <- .as_result(output)
  out_info <- .result_info(out)
  out_units <- .result_units(out)
  ref <- .score_reference(benchmark, out_info$codebook)
  # the page's other text (codebook(free_text = TRUE) with a table) is scored
  # apart from the values; a benchmark file written before that choice was
  # recorded in it has lost it, and the results' codebook still knows
  rv <- ref$codebook$variables
  if (!isTRUE(ref$codebook$free_text) && isTRUE(out_info$codebook$free_text) &&
      identical(ref$codebook$kind, "table") && "text" %in% rv$variable[rv$level == "page"]) {
    ref$codebook$free_text <- TRUE
  }
  cmp <- .score_variables(ref$codebook, out_info$codebook, ref$labels)

  s <- .score_units(ref$units, out_units, ref$codebook, align = align, normalize = normalize,
                    rows = ref$rows, variables = cmp$variables, labels = ref$labels)

  a <- attr(s, "about")
  a$reference <- ref$kind
  a$model <- out_info$settings$model %||% NA_character_
  a$reference_model <- ref$model %||% NA_character_
  a$source <- ref$source
  a$prefilled <- ref$prefilled
  a$draw <- ref$draw
  a$output_folder <- out_info$folder %||% NA_character_
  scored <- s$by_page$file[s$by_page$status != "not run"]
  dates <- vapply(out_info$records, function(r) {
    if (.page_key(r$file) %in% .page_key(scored)) substr(r$date %||% NA_character_, 1, 10) else NA_character_
  }, character(1))
  dates <- sort(unique(dates[!is.na(dates)]))
  a$output_dates <- if (length(dates)) dates[c(1, length(dates))] else NA_character_

  # most important first: an optimistic benchmark, pages left out or failed;
  # then how the benchmark and the codebooks were read; then page details
  first <- utils::head(s$notes, a$n_first)
  rest <- utils::tail(s$notes, length(s$notes) - a$n_first)
  notes <- c(if (isTRUE(ref$prefilled)) paste("Your benchmark was pre-filled with the model's own output, so these figures",
                                              "are optimistic: whoever checks a page tends to keep what is already written."),
             first, ref$notes, cmp$notes)
  others <- setdiff(.page_key(names(out_units)), .page_key(names(ref$units)))
  if (length(others)) {
    notes <- c(notes, paste0("The results hold ", .fmt_int(length(others)), " other page", if (length(others) != 1) "s",
                             " that ", if (length(others) != 1) "are" else "is", " not in ", ref$labels$ref,
                             "; only the ", .plural(length(scored), "page", "pages"), " in both ",
                             if (length(scored) != 1) "are" else "is", " scored."))
  }
  s$notes <- c(notes, rest)
  attr(s, "about") <- a
  if (!is.null(report)) {
    .write_score_report(s, report)
    a$report <- report
    attr(s, "about") <- a
  }
  s
}

# ------- the reference side ---------------------------------------------------

# Whatever the first argument is, reduce it to per-page units keyed by file
# (only pages that count as done), its codebook, and how it was made.
.score_reference <- function(x, out_cb, call = rlang::caller_env()) {
  bench_labels <- list(ref = "your benchmark", out = "the output", ref_col = "benchmark", out_col = "output")
  run_labels <- list(ref = "the first run", out = "the second run", ref_col = "first run", out_col = "second run")
  if (.is_string(x) && dir.exists(x)) x <- .result_from_folder(x, call = call)
  if (inherits(x, "amanuensis_result") && !is.null(.result_info(x))) {
    info <- .result_info(x)
    units <- .result_units(x)
    ok <- vapply(units, function(u) u$status %in% c("ok", "check"), logical(1))
    notes <- character()
    if (any(!ok)) {
      notes <- paste0("The first run has no data for ", .plural(sum(!ok), "page", "pages"), " (failed): ",
                      .list_files(names(units)[!ok]), ". ", if (sum(!ok) == 1) "It is" else "They are", " left out.")
    }
    if (!any(ok)) {
      .abort(c("The first result has no pages with data to compare against.",
               "i" = "Give a benchmark from {.fn benchmark}, or a result whose pages were extracted."),
             call = call, class = "amanuensis_error_input")
    }
    return(list(units = units[ok], codebook = info$codebook, rows = NULL, prefilled = FALSE, kind = "run",
                model = info$settings$model, source = info$folder %||% NA_character_, draw = NULL,
                labels = run_labels, notes = notes))
  }
  if (.is_string(x)) {
    if (!file.exists(x)) {
      .abort(c("Cannot find the benchmark file {.path {(.show_path(x))}}.",
               "i" = "Make one with {.code benchmark(\"{basename(x)}\", images = pages, codebook = cb)}, fill it in, then score again.",
               "i" = "Relative paths start from your working folder, {.path {(.show_path(getwd()))}}."),
             call = call, class = "amanuensis_error_input")
    }
    x <- .read_benchmark_file(x, out_cb, call = call)
  }
  # subset() keeps the class of a benchmark but drops what benchmark()
  # recorded about it: what is left is read as a plain table
  if (inherits(x, "amanuensis_gold") && is.null(.gold_info(x))) x <- as.data.frame(x)
  if (is.data.frame(x) && !inherits(x, "amanuensis_gold")) x <- .score_table_as_gold(x, out_cb, call = call)
  if (inherits(x, "amanuensis_gold")) {
    info <- .gold_info(x)
    pages <- info$pages
    tab <- as.data.frame(x)
    # only the variables the table still has a column for: a benchmark cut
    # down with gold[, cols] or dplyr::select() would otherwise count every
    # value the model read in a dropped column as wrong
    cb <- .score_codebook_subset(info$codebook, names(tab))
    done <- pages$file[pages$done %in% TRUE]
    if (!length(done)) {
      .abort(c("No page of your benchmark is marked as done.",
               "i" = "Mark the pages you have finished with {.val yes} in the {.field done} column of the {.val pages} sheet, save, and score again."),
             call = call, class = "amanuensis_error_input")
    }
    # benchmark() writes at least one line for every page marked done, so a
    # done page with no line in the table was taken out of it afterwards
    # (gold[gold$file != f, ], dplyr::filter()); scored as a page with no
    # rows, every row the model read there would count as wrong
    present <- .page_key(unique(as.character(tab$file)))
    gone <- done[!.page_key(done) %in% present]
    done <- done[.page_key(done) %in% present]
    if (!length(done)) {
      .abort(c("None of the pages marked done is in the benchmark table you gave.",
               "i" = "Pages marked done: {.file {utils::head(gone, 4)}}{if (length(gone) > 4) ' ...' else ''}",
               "i" = "Score the whole benchmark, or keep at least one of those pages in the table."),
             call = call, class = "amanuensis_error_input")
    }
    units <- .units_from_table(tab, cb)
    out <- units[match(.page_key(done), .page_key(names(units)))]
    names(out) <- done
    notes <- character()
    n_left <- sum(!pages$done %in% TRUE)
    if (n_left) {
      notes <- paste0(.plural(n_left, "page", "pages"), " of your benchmark ", if (n_left == 1) "is" else "are",
                      " not marked as done and ", if (n_left == 1) "was" else "were", " left out.")
    }
    if (length(gone)) {
      notes <- c(notes, paste0(.plural(length(gone), "page", "pages"), " marked done in your benchmark ",
                               if (length(gone) == 1) "has" else "have", " no lines in the table you gave and ",
                               if (length(gone) == 1) "was" else "were", " left out: ", .list_files(gone), "."))
    }
    if (any(cb$variables$level == "row")) {
      n_now <- vapply(out, function(u) nrow(u$rows), integer(1))
      n_then <- pages$n_rows[match(.page_key(done), .page_key(pages$file))]
      cut <- !is.na(n_then) & n_now < n_then
      if (any(cut)) {
        notes <- c(notes, paste0("The benchmark table you gave has fewer rows than your benchmark on ",
                                 .list_files(paste0(done[cut], " (", n_now[cut], " of ", n_then[cut], ")")),
                                 "; the output's other rows there count as rows not in your benchmark."))
      }
    }
    rows <- info$rows
    if (!is.null(rows)) {
      notes <- c(notes, paste0("Your benchmark covers the first ", rows, " rows of each page: where the page goes on, ",
                               "the output was compared up to the row that pairs with your last one, and its later ",
                               "rows were left out."))
    }
    return(list(units = out, codebook = cb, rows = rows, prefilled = isTRUE(info$prefilled), kind = "benchmark",
                model = NULL, source = info$source %||% NA_character_, draw = info$draw,
                labels = bench_labels, notes = notes))
  }
  .abort(c("{.arg benchmark} must be your benchmark: the result of {.fn benchmark} or the file it wrote.",
           "i" = "For example {.code score(\"gold.xlsx\", res)}. To compare two runs, give two results: {.code score(res1, res2)}."),
         call = call, class = "amanuensis_error_input")
}

# The benchmark file is read by benchmark() itself (same checks, same type
# conversion, same messages); the results' codebook is passed for files that
# do not carry their own. Errors name score(), the function the user called.
.read_benchmark_file <- function(path, codebook, call = rlang::caller_env()) {
  .as_gold(path, codebook = codebook, call = call)
}

# A table given as the benchmark goes through benchmark()'s own reader, as a
# file would: values converted to the codebook's types (a value that cannot
# be stops, with its row), file names matched, rows numbered by line order
# when the row column is blank, every page with a value counted as done.
# The codebook is the results' own, cut down to the variables the table has
# a column for; score() then lists the others as not compared.
.score_table_as_gold <- function(x, out_cb, call = rlang::caller_env()) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
  if (is.na(.gold_match_columns(names(x), "file")[[1]])) {
    .abort(c("The benchmark table needs a {.field file} column naming the page image of each row.",
             "i" = "Its columns are: {.val {names(x)}}."), call = call, class = "amanuensis_error_input")
  }
  cols <- .gold_match_columns(names(x), out_cb$variables$variable)
  have <- names(cols)[!is.na(cols)]
  if (!length(have)) {
    .abort(c("The benchmark table has no column for any variable of the results.",
             "i" = "Its columns are: {.val {names(x)}}.", "i" = "Result variables: {.val {out_cb$variables$variable}}."),
           call = call, class = "amanuensis_error_input")
  }
  .as_gold(x, codebook = .score_codebook_subset(out_cb, have), call = call)
}

# The codebook with only the variables named in `keep` (used for scoring
# only: the schema, sent to the model, is left as it was).
.score_codebook_subset <- function(cb, keep) {
  v <- cb$variables
  if (all(v$variable %in% keep)) return(cb)
  v <- v[v$variable %in% keep, , drop = FALSE]
  rownames(v) <- NULL
  cb$variables <- v
  cb
}

# Variables compared: those of the benchmark's codebook that the results
# also have, at the same level. If the codebook changed after the benchmark
# was made, the shared variables are still scored and the rest listed.
.score_variables <- function(ref_cb, out_cb, labels, call = rlang::caller_env()) {
  rv <- ref_cb$variables
  ov <- out_cb$variables
  common <- rv$variable %in% ov$variable
  level_ok <- common & rv$level == ov$level[match(rv$variable, ov$variable)]
  notes <- character()
  only_ref <- rv$variable[!common]
  only_out <- setdiff(ov$variable, rv$variable)
  if (length(only_ref)) {
    notes <- c(notes, paste0("Not compared (only in ", labels$ref, "): ", paste(only_ref, collapse = ", "), "."))
  }
  if (length(only_out)) {
    notes <- c(notes, paste0("Not compared (only in ", labels$out, "): ", paste(only_out, collapse = ", "), "."))
  }
  if (any(common & !level_ok)) {
    notes <- c(notes, paste0("Not compared (a page value on one side and a row value on the other): ",
                             paste(rv$variable[common & !level_ok], collapse = ", "), "."))
  }
  keep <- rv[level_ok, , drop = FALSE]
  if (!nrow(keep)) {
    .abort(c("Your benchmark and the results have no variables in common.",
             "i" = "Benchmark variables: {.val {rv$variable}}.", "i" = "Result variables: {.val {ov$variable}}.",
             "i" = "Score with the codebook the results were extracted with."),
           call = call, class = "amanuensis_error_input")
  }
  other_type <- ov$type[match(keep$variable, ov$variable)]
  differ <- keep$type != other_type
  if (any(differ)) {
    notes <- c(notes, paste0("Compared with the type in ", labels$ref, "'s codebook: ",
                             paste0(keep$variable[differ], " (", keep$type[differ], ", ", other_type[differ], " in ",
                                    labels$out, ")", collapse = ", "), "."))
  }
  list(variables = keep$variable, notes = notes)
}

# ------- scoring units -----------------------------------------------------

# The core, on per-page units (see .page_unit()): gold_units keyed by file
# (list(page, rows)), out_units keyed by file (list(status, page, rows)).
# Exposed internally so the tests (errors planted in a copy of the
# benchmark) can drive it without files.
.score_units <- function(gold_units, out_units, codebook, align = "auto", normalize = FALSE, rows = NULL,
                         variables = NULL, labels = NULL, call = rlang::caller_env()) {
  labels <- labels %||% list(ref = "your benchmark", out = "the output", ref_col = "benchmark", out_col = "output")
  vars <- codebook$variables[, c("variable", "type", "level")]
  if (!is.null(variables)) vars <- vars[vars$variable %in% variables, , drop = FALSE]
  rownames(vars) <- NULL
  # free text: the page's transcription is the only thing compared, so rows
  # and values (always about 0%) give way to characters and words
  free_text <- nrow(vars) == 1 && identical(vars$variable, "text") && vars$level == "page" && vars$type == "text"
  # a table with free_text = TRUE: the page's other text is one long value;
  # counted as a value it would make "page values right" about 0% and swamp
  # the character pool of short cells, so it is scored on its own
  separate <- if (!free_text && isTRUE(codebook$free_text) && "text" %in% vars$variable[vars$level == "page"]) "text" else character()

  gold_files <- names(gold_units)
  gold_files <- gold_files[.natural_order(gold_files)]
  out_keys <- .page_key(names(out_units))
  found <- !is.na(match(.page_key(gold_files), out_keys))
  if (!any(found)) {
    .abort(c("No page of {labels$ref} is in the results.",
             "i" = "Pages in {labels$ref}: {.file {utils::head(gold_files, 4)}}{if (length(gold_files) > 4) ' ...' else ''}",
             "i" = "Pages in the results: {.file {utils::head(names(out_units), 4)}}{if (length(out_units) > 4) ' ...' else ''}",
             "i" = "Extract the benchmark pages first: {.code transcribe(pages, instructions, codebook, only = gold)}."),
           call = call, class = "amanuensis_error_input")
  }

  page_rows <- list()
  cell_list <- list()
  notes <- character()
  for (f in gold_files) {
    hit <- match(.page_key(f), out_keys)
    if (is.na(hit)) {
      page_rows[[f]] <- data.frame(file = f, status = "not run", alignment = NA_character_, gold_rows = NA_integer_,
                                   output_rows = NA_integer_, missing_rows = NA_integer_, extra_rows = NA_integer_,
                                   differences = NA_integer_, stringsAsFactors = FALSE)
      next
    }
    pg <- .score_page(f, gold_units[[f]], out_units[[hit]], vars, align, rows)
    page_rows[[f]] <- pg$page
    cell_list[[f]] <- pg$cells
    p <- pg$page
    if (identical(p$alignment, "content") && (p$missing_rows + p$extra_rows > 0 || p$gold_rows != p$output_rows)) {
      tally <- paste0(p$missing_rows, " missing from ", labels$out, ", ", p$extra_rows, " not in ", labels$ref, ")")
      notes <- c(notes, if (pg$partial) {
        paste0(f, ": ", labels$ref, " has the first ", .plural(p$gold_rows, "row", "rows"), " of the page, paired by ",
               "content with the first ", .fmt_int(p$output_rows), " of the ", .fmt_int(pg$output_all), " in ",
               labels$out, " (", tally, ".")
      } else {
        paste0(f, ": ", labels$ref, " has ", .plural(p$gold_rows, "row", "rows"), ", ", labels$out, " ",
               .fmt_int(p$output_rows), "; rows paired by content (", tally, ".")
      })
    }
  }
  by_page <- do.call(rbind, unname(page_rows))
  cells <- if (length(cell_list)) do.call(rbind, unname(cell_list)) else NULL
  if (is.null(cells) || !nrow(cells)) cells <- .empty_cells()
  rownames(cells) <- NULL
  cells$separate <- cells$variable %in% separate
  cells <- .measure_cells(cells)
  diffs_by_page <- tapply(!cells$match, factor(cells$file, levels = gold_files), sum)
  run <- by_page$status != "not run"
  by_page$differences[run] <- as.integer(diffs_by_page[by_page$file[run]])
  by_page$differences[run & is.na(by_page$differences)] <- 0L
  rownames(by_page) <- NULL

  not_run <- by_page$file[!run]
  failed <- by_page$file[by_page$status == "failed"]
  if (length(failed)) {
    notes <- c(paste0(.plural(length(failed), "page", "pages"), " failed in ", labels$out, " and ",
                      if (length(failed) == 1) "counts" else "count", " as entirely wrong: ", .list_files(failed),
                      ". Run the extraction again (with redo = \"failed\" if need be) to retry ", if (length(failed) == 1) "it" else "them", "."), notes)
  }
  if (length(not_run)) {
    notes <- c(paste0(.plural(length(not_run), "page", "pages"), " of ", labels$ref, " ",
                      if (length(not_run) == 1) "is" else "are", " not in the results yet and ",
                      if (length(not_run) == 1) "was" else "were", " left out: ", .list_files(not_run),
                      ". Extract ", if (length(not_run) == 1) "it" else "them",
                      " with transcribe(pages, instructions, codebook, only = gold)."), notes)
  }
  if (length(separate)) {
    notes <- c(notes, "The page's other text (variable text) is scored on its own, as characters and words right, not as a value.")
  }
  n_first <- as.integer(length(not_run) > 0) + as.integer(length(failed) > 0)

  mode <- if (free_text) "text" else "table"
  summary <- .score_summary(cells, mode, normalize)
  by_variable <- .score_by_variable(cells, vars, normalize)
  differences <- .score_differences(cells, normalize, labels)
  s <- structure(list(summary = summary, by_variable = by_variable, by_page = by_page, differences = differences,
                      not_run = not_run, notes = notes),
                 class = "amanuensis_score")
  attr(s, "cells") <- cells
  attr(s, "about") <- list(mode = mode, separate = separate, normalize = isTRUE(normalize), align = align,
                           rows_limit = rows, labels = labels, n_first = n_first,
                           n_pages = sum(run), gold_rows = sum(by_page$gold_rows[run]),
                           n_variables = nrow(vars), row_variables = sum(vars$level == "row"),
                           page_variables = sum(vars$level == "page" & !vars$variable %in% separate),
                           variables = vars)
  s
}

# One page: its cells (every value compared, gold beside output) and its line
# of the pages table.
.score_page <- function(file, g, o, vars, align, rows_limit) {
  failed <- !isTRUE(o$status %in% c("ok", "check"))
  pv <- vars[vars$level == "page", , drop = FALSE]
  rv <- vars[vars$level == "row", , drop = FALSE]
  G <- g$rows
  O <- if (failed) NULL else o$rows
  n_g <- if (is.null(G)) 0L else nrow(G)
  n_o <- if (is.null(O)) 0L else nrow(O)
  n_o_all <- n_o
  # a benchmark of the first rows_limit rows that reaches the limit: the page
  # may go on beyond what was typed (with fewer rows, the benchmark holds
  # the whole page and every output row counts)
  partial <- !is.null(rows_limit) && n_g >= rows_limit
  if (partial) n_g <- as.integer(rows_limit)
  cells <- list()
  for (k in seq_len(nrow(pv))) {
    v <- pv$variable[k]
    ty <- pv$type[k]
    gv <- .score_values(.col(g$page, v, 1L)[1], ty)
    ov <- .score_values(if (failed) NA else .col(o$page, v, 1L)[1], ty)
    cells[[length(cells) + 1]] <- .cell_frame(file, "page", 0L, NA_integer_, NA_integer_, v, ty, gv, ov,
                                              FALSE, failed, failed)
  }
  method <- NA_character_
  missing_rows <- extra_rows <- NA_integer_
  if (nrow(rv)) {
    # equal counts mean nothing when the benchmark stops at the limit: the
    # model may have skipped a row in the first N and gone on to N rows
    method <- if (failed) "none" else if (align == "auto") (if (!partial && n_g == n_o) "position" else "content") else align
    if (partial && method != "content") n_o <- min(n_o, n_g)
    gk <- ok <- list()
    for (k in seq_len(nrow(rv))) {
      v <- rv$variable[k]
      gk[[v]] <- .score_values(utils::head(.col(G, v, n_g), n_g), rv$type[k])
      ok[[v]] <- .score_values(utils::head(.col(O, v, n_o), n_o), rv$type[k])
    }
    pairs <- if (method == "content") .align_content(gk, ok, rv, n_g, n_o, open_end = partial) else .align_position(n_g, n_o)
    # with an open end, the output's rows after the last one paired lie
    # beyond the benchmark: they are left out, not counted as extra
    if (partial && method == "content") n_o <- max(c(0L, pairs$oi), na.rm = TRUE)
    missing_rows <- sum(!is.na(pairs$gi) & is.na(pairs$oi))
    extra_rows <- sum(is.na(pairs$gi) & !is.na(pairs$oi))
    np <- nrow(pairs)
    if (np) {
      rc <- list()
      for (k in seq_len(nrow(rv))) {
        v <- rv$variable[k]
        gv <- lapply(gk[[v]], function(z) z[pairs$gi])
        ov <- lapply(ok[[v]], function(z) z[pairs$oi])
        rc[[k]] <- .cell_frame(file, "row", seq_len(np), pairs$gi, pairs$oi, v, rv$type[k], gv, ov,
                               is.na(pairs$gi), is.na(pairs$oi), failed)
        rc[[k]]$var_order <- k
      }
      rc <- do.call(rbind, rc)
      rc <- rc[order(rc$pair, rc$var_order), , drop = FALSE]
      rc$var_order <- NULL
      cells[[length(cells) + 1]] <- rc
    }
  } else {
    n_g <- n_o <- NA_integer_
  }
  cells <- if (length(cells)) do.call(rbind, cells) else .empty_cells()
  page <- data.frame(file = file, status = if (failed) "failed" else o$status, alignment = method,
                     gold_rows = as.integer(n_g), output_rows = as.integer(n_o),
                     missing_rows = as.integer(missing_rows), extra_rows = as.integer(extra_rows),
                     differences = NA_integer_, stringsAsFactors = FALSE)
  list(page = page, cells = cells, partial = partial && nrow(rv) > 0, output_all = n_o_all)
}

.col <- function(df, v, n) {
  if (!is.null(df) && v %in% names(df)) df[[v]] else rep(NA, n)
}

.cell_frame <- function(file, level, pair, row_gold, row_output, variable, type, gv, ov, g_missing, o_missing,
                        failed) {
  n <- length(pair)
  g_missing <- rep_len(g_missing, n)
  o_missing <- rep_len(o_missing, n)
  pick <- function(x, miss) {
    x <- rep_len(x, n)
    x[miss] <- NA
    x
  }
  data.frame(file = rep(file, n), level = rep(level, n), pair = as.integer(pair),
             row_gold = as.integer(rep_len(row_gold, n)), row_output = as.integer(rep_len(row_output, n)),
             variable = rep(variable, n), type = rep(type, n),
             gold = pick(gv$text, g_missing), output = pick(ov$text, o_missing),
             gold_key = pick(gv$key, g_missing), output_key = pick(ov$key, o_missing),
             gold_lenient = pick(gv$lenient, g_missing), output_lenient = pick(ov$lenient, o_missing),
             g_missing = g_missing, o_missing = o_missing, failed = rep(failed, n),
             stringsAsFactors = FALSE)
}

.empty_cells <- function() {
  data.frame(file = character(), level = character(), pair = integer(), row_gold = integer(), row_output = integer(),
             variable = character(), type = character(), gold = character(), output = character(),
             gold_key = character(), output_key = character(), gold_lenient = character(),
             output_lenient = character(), g_missing = logical(), o_missing = logical(), failed = logical(),
             stringsAsFactors = FALSE)
}

# Match, edits and lenient match for every cell at once.
.measure_cells <- function(cells) {
  present <- !cells$g_missing & !cells$o_missing
  cells$match <- present & .same(cells$gold_key, cells$output_key)
  g_len <- .lenient_key(cells$gold_lenient)
  o_len <- .lenient_key(cells$output_lenient)
  cells$lenient_match <- present & .same(g_len, o_len)
  is_text <- cells$type == "text"
  gt <- .blank(cells$gold)
  ot <- .blank(cells$output)
  cells$length <- ifelse(is_text, pmax(nchar(gt), nchar(ot)), NA_integer_)
  cells$edits <- ifelse(is_text, 0, NA_real_)
  idx <- which(is_text & !cells$match)
  cells$edits[idx] <- .lev_pairs(gt[idx], ot[idx])
  gl <- .blank(g_len)
  ol <- .blank(o_len)
  cells$lenient_length <- ifelse(is_text, pmax(nchar(gl), nchar(ol)), NA_integer_)
  cells$lenient_edits <- ifelse(is_text, 0, NA_real_)
  idx <- which(is_text & !cells$lenient_match)
  cells$lenient_edits[idx] <- .lev_pairs(gl[idx], ol[idx])
  # a wrong value that would be right without a final "?": the model (or the
  # benchmark) marked a doubt about a correct reading
  cells$doubt_only <- !cells$match & present & cells$type %in% c("text", "category") &
    !is.na(cells$gold) & !is.na(cells$output) & .same(.strip_doubt(cells$gold), .strip_doubt(cells$output))
  cells
}

.same <- function(a, b) {
  out <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  out[is.na(out)] <- FALSE
  out
}

.blank <- function(x) {
  x[is.na(x)] <- ""
  x
}

.strip_doubt <- function(x) sub("\\s*\\?+$", "", x)

# ------- values -------------------------------------------------------------

# Both sides converted to the codebook's type: `key` is what is compared,
# `text` what is shown and measured in characters, `lenient` the key for the
# lenient rates. NA = empty. A benchmark value that cannot be read as the
# type (a word in a number column) is kept as written, so it shows up as a
# difference instead of silently becoming empty. Yes/no is read with the
# same spellings as benchmark() accepts (.gold_yes, .gold_no), so the two
# never disagree on what a cell says.
.score_values <- function(x, type) {
  if (is.factor(x)) x <- as.character(x)
  if (type %in% c("number", "integer")) {
    if (is.numeric(x)) {
      key <- .chr_values(as.numeric(x))
    } else {
      raw <- .empty_na(trimws(.nfc(as.character(x))))
      num <- suppressWarnings(as.numeric(raw))
      key <- .chr_values(num)
      bad <- !is.na(raw) & is.na(num)
      key[bad] <- raw[bad]
    }
    return(list(key = key, text = key, lenient = key))
  }
  if (type == "boolean") {
    if (is.logical(x)) {
      key <- ifelse(is.na(x), NA_character_, ifelse(x, "TRUE", "FALSE"))
    } else {
      raw <- .empty_na(trimws(.nfc(as.character(x))))
      yes <- .gold_parse_yes_no(raw)
      key <- ifelse(is.na(yes), raw, ifelse(yes, "TRUE", "FALSE"))
      key[is.na(raw)] <- NA_character_
    }
    return(list(key = key, text = key, lenient = key))
  }
  if (is.logical(x)) {
    x <- ifelse(is.na(x), NA_character_, tolower(as.character(x)))
  } else if (is.numeric(x)) {
    x <- .chr_values(x)
  }
  key <- .empty_na(.nfc(as.character(x)))
  list(key = key, text = key, lenient = .lenient(key))
}

.empty_na <- function(x) {
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

# The lenient form of a text value: capital letters and accents folded,
# punctuation turned into a space (a doubt mark "?" is punctuation), spacing
# collapsed. Punctuation between two digits is kept, so "1.5" never matches
# "15": the lenient rates forgive spelling conventions, never a changed
# number. Case is folded by the utf8 package on text marked as UTF-8, so the
# result is the same whatever the computer's language settings (in a "C"
# locale tolower() turns an accented letter into its byte codes, and the
# accents would never be forgiven). This spaced form is what words are
# counted on; values are matched on .lenient_key().
.lenient <- function(x) {
  out <- x
  ok <- !is.na(x)
  if (any(ok)) {
    y <- utf8::utf8_normalize(.utf8_marked(x[ok]), map_case = TRUE)
    y <- .fold_accents(y)
    y <- gsub("\\p{M}", "", y, perl = TRUE)
    y <- gsub("\u00a0", " ", y, fixed = TRUE)
    y <- gsub("(?<![0-9])[[:punct:]\\p{P}]+|[[:punct:]\\p{P}]+(?![0-9])", " ", y, perl = TRUE)
    y <- gsub("\\s+", " ", trimws(y), perl = TRUE)
    out[ok] <- y
  }
  .empty_na(out)
}

# Lenient forms compared without their spaces, except a space between two
# digits: punctuation inside a word then costs nothing either, so the
# Catalan middle dot of "Ul\u00b7ldemolins", "D'Arcy" and "Puig-Ferrer" match
# "Ulldemolins", "DArcy", "Puig Ferrer" and "PuigFerrer", as "ignoring
# punctuation and spacing" promises, while "1 5" and "15" still differ.
.lenient_key <- function(x) gsub("(?<![0-9]) | (?![0-9])", "", x, perl = TRUE)


# ------- pairing rows ---------------------------------------------------------

# Pairs as data.frame(gi, oi): row numbers on each side, NA = MISSING.
.align_position <- function(n_g, n_o) {
  k <- seq_len(max(n_g, n_o))
  data.frame(gi = ifelse(k <= n_g, k, NA_integer_), oi = ifelse(k <= n_o, k, NA_integer_))
}

# Monotone alignment by content: the order of rows is kept, gaps are
# allowed, and the sum of similarities of the pairs made is as large as
# possible. Similarity of two rows = the average over the variables filled on
# at least one side of: 1 when equal after lenient normalisation, else (text)
# 1 - normalised edit distance or (other types) 0. Variables empty on both
# sides are left out: with them, two different people in a sparse register
# would look alike just for sharing empty columns. Pairs below 0.5 are not
# made; their rows count as missing on one side and extra on the other.
#
# open_end = TRUE is for a benchmark of the page's first rows only: the
# output's rows after the last one paired are left out of the pairs (they
# lie beyond what was typed), and every unpaired row inside the paired
# stretch costs 0.5. Without that cost nothing would stop the benchmark's
# last row from pairing with a look-alike far down the page, turning every
# output row in between into an extra row.
.align_content <- function(gk, ok, rv, n_g, n_o, open_end = FALSE) {
  if (!n_g || !n_o) {
    if (open_end) n_o <- 0L
    return(data.frame(gi = c(seq_len(n_g), rep(NA_integer_, n_o)), oi = c(rep(NA_integer_, n_g), seq_len(n_o))))
  }
  num <- matrix(0, n_g, n_o)
  den <- matrix(0, n_g, n_o)
  for (k in seq_len(nrow(rv))) {
    v <- rv$variable[k]
    a <- .lenient_key(gk[[v]]$lenient)
    b <- .lenient_key(ok[[v]]$lenient)
    filled <- outer(!is.na(a), !is.na(b), "|")
    if (rv$type[k] == "text") {
      ta <- .blank(a)
      tb <- .blank(b)
      d <- utils::adist(ta, tb)
      len <- outer(nchar(ta), nchar(tb), pmax)
      s <- ifelse(len > 0, 1 - d / len, 1)
    } else {
      s <- outer(a, b, "==")
      s[is.na(s)] <- FALSE
      s <- s * 1
    }
    num <- num + s * filled
    den <- den + filled
  }
  sim <- ifelse(den > 0, num / den, 1)
  if (open_end) .dp_align(sim, sim >= 0.5, gap = 0.5, open_end = TRUE) else .dp_align(sim, sim >= 0.5)
}

# Best monotone alignment for a similarity matrix (rows = gold, columns =
# output), pairing only where `allowed`. D[i, j] = best total for the first
# i gold and j output rows, each pair adding its similarity and each unpaired
# row subtracting `gap` (0 by default: the pairs' total alone decides); each
# row of D is built in one vectorised step (cummax), so a page of 1,000 rows
# takes 1,000 steps, not a million. With open_end the alignment may stop at
# any output row (the best total wins; on a tie, the later row) and the rows
# after it are not returned.
# Traceback prefers a pair, then an extra output row, then a missing gold row,
# so a missing benchmark row is listed before the extra output row beside it.
.dp_align <- function(sim, allowed, gap = 0, open_end = FALSE) {
  n <- nrow(sim)
  m <- ncol(sim)
  w <- ifelse(allowed, sim, -Inf)
  steps <- gap * (0:m)
  D <- matrix(0, n + 1, m + 1)
  D[1, ] <- -steps
  D[, 1] <- -gap * (0:n)
  for (i in seq_len(n)) {
    # a[j]: gold row i paired with output row j, or left unpaired
    a <- c(D[i, 1] - gap, pmax(D[i, -1] - gap, D[i, -(m + 1)] + w[i, ]))
    # then any run of unpaired output rows before column j
    D[i + 1, ] <- cummax(a + steps) - steps
  }
  tol <- 1e-9
  gi <- oi <- integer(n + m)
  k <- 0L
  i <- n
  j <- m
  if (open_end) {
    last <- D[n + 1, ]
    j <- max(which(last > max(last) - tol)) - 1L
  }
  while (i > 0 || j > 0) {
    k <- k + 1L
    if (i > 0 && j > 0 && allowed[i, j] && abs(D[i + 1, j + 1] - (D[i, j] + w[i, j])) < tol) {
      gi[k] <- i; oi[k] <- j; i <- i - 1L; j <- j - 1L
    } else if (j > 0 && abs(D[i + 1, j + 1] - (D[i + 1, j] - gap)) < tol) {
      gi[k] <- NA_integer_; oi[k] <- j; j <- j - 1L
    } else {
      gi[k] <- i; oi[k] <- NA_integer_; i <- i - 1L
    }
  }
  idx <- rev(seq_len(k))
  data.frame(gi = gi[idx], oi = oi[idx])
}

# ------- distances ------------------------------------------------------------

# Levenshtein distance of each pair a[i], b[i] (characters, not bytes),
# one pair at a time: adist on the two vectors would compute the whole
# matrix of every gold value against every output value.
.lev_pairs <- function(a, b) {
  if (!length(a)) return(numeric())
  vapply(seq_along(a), function(i) as.numeric(utils::adist(a[i], b[i])[1, 1]), numeric(1))
}

.words <- function(x) {
  if (is.na(x)) return(character())
  x <- trimws(gsub("\u00a0", " ", x, fixed = TRUE))
  if (!nzchar(x)) return(character())
  strsplit(x, "\\s+", perl = TRUE)[[1]]
}

# Word-level Levenshtein: each distinct word becomes one private-use Unicode
# character, so adist counts whole words inserted, deleted or replaced.
.word_lev <- function(a, b) {
  wa <- .words(a)
  wb <- .words(b)
  vocab <- unique(c(wa, wb))
  if (!length(vocab)) return(0)
  codes <- .private_codes(length(vocab))
  ea <- intToUtf8(codes[match(wa, vocab)])
  eb <- intToUtf8(codes[match(wb, vocab)])
  as.numeric(utils::adist(ea, eb)[1, 1])
}

# The private-use code points: U+E000-U+F8FF, then planes 15 and 16.
.private_codes <- function(n) {
  pools <- c(0xE000:0xF8FF, 0xF0000:0xFFFFD, 0x100000:0x10FFFD)
  if (n > length(pools)) .abort("A page has more than {(.fmt_int(length(pools)))} different words; words right cannot be measured.")
  pools[seq_len(n)]
}

.word_totals <- function(gold, output, match) {
  if (!length(gold)) return(c(edits = 0, total = 0))
  total <- 0
  edits <- 0
  for (i in seq_along(gold)) {
    total <- total + max(length(.words(gold[i])), length(.words(output[i])))
    if (!match[i]) edits <- edits + .word_lev(gold[i], output[i])
  }
  c(edits = edits, total = total)
}

# ------- the tables -----------------------------------------------------------

.measure_row <- function(measure, matched, compared) {
  data.frame(measure = measure, value = if (compared > 0) matched / compared else NA_real_,
             matched = as.numeric(matched), compared = as.numeric(compared), stringsAsFactors = FALSE)
}

.score_summary <- function(cells, mode, normalize) {
  out <- list()
  add <- function(measure, matched, compared) out[[length(out) + 1]] <<- .measure_row(measure, matched, compared)
  chars <- function(sub, lenient = FALSE) {
    len <- if (lenient) sub$lenient_length else sub$length
    ed <- if (lenient) sub$lenient_edits else sub$edits
    total <- sum(len[sub$type == "text"], na.rm = TRUE)
    c(matched = total - sum(ed[sub$type == "text"], na.rm = TRUE), compared = total)
  }
  words <- function(sub, lenient = FALSE) {
    sub <- sub[sub$type == "text", , drop = FALSE]
    w <- if (lenient) .word_totals(.blank(sub$gold_lenient), .blank(sub$output_lenient), sub$lenient_match) else
      .word_totals(.blank(sub$gold), .blank(sub$output), sub$match)
    c(matched = w[["total"]] - w[["edits"]], compared = w[["total"]])
  }
  if (mode == "text") {
    ch <- chars(cells)
    add("characters", ch[["matched"]], ch[["compared"]])
    wd <- words(cells)
    add("words", wd[["matched"]], wd[["compared"]])
    add("pages", sum(cells$match), nrow(cells))
    if (normalize) {
      ch <- chars(cells, TRUE)
      add("characters (lenient)", ch[["matched"]], ch[["compared"]])
      wd <- words(cells, TRUE)
      add("words (lenient)", wd[["matched"]], wd[["compared"]])
    }
    return(do.call(rbind, out))
  }
  main <- cells[!cells$separate, , drop = FALSE]
  rc <- main[main$level == "row", , drop = FALSE]
  row_counts <- function(m) {
    if (!nrow(rc)) return(c(0, 0))
    ok <- tapply(m, paste(rc$file, rc$pair, sep = "\r"), all)
    c(sum(ok), length(ok))
  }
  r <- row_counts(rc$match)
  add("rows", r[1], r[2])
  add("values", sum(main$match), nrow(main))
  ch <- chars(main)
  add("characters", ch[["matched"]], ch[["compared"]])
  pc <- main[main$level == "page", , drop = FALSE]
  if (nrow(pc)) {
    ok <- tapply(pc$match, pc$file, all)
    add("page values", sum(ok), length(ok))
  }
  add("doubt marks", sum(main$doubt_only), sum(!main$match))
  sep <- cells[cells$separate, , drop = FALSE]
  if (nrow(sep)) {
    ch <- chars(sep)
    add("other text: characters", ch[["matched"]], ch[["compared"]])
    wd <- words(sep)
    add("other text: words", wd[["matched"]], wd[["compared"]])
  }
  if (normalize) {
    r <- row_counts(rc$lenient_match)
    add("rows (lenient)", r[1], r[2])
    add("values (lenient)", sum(main$lenient_match), nrow(main))
    ch <- chars(main, TRUE)
    add("characters (lenient)", ch[["matched"]], ch[["compared"]])
    if (nrow(sep)) {
      ch <- chars(sep, TRUE)
      add("other text: characters (lenient)", ch[["matched"]], ch[["compared"]])
      wd <- words(sep, TRUE)
      add("other text: words (lenient)", wd[["matched"]], wd[["compared"]])
    }
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

.score_by_variable <- function(cells, vars, normalize) {
  rows <- lapply(seq_len(nrow(vars)), function(k) {
    v <- vars$variable[k]
    sub <- cells[cells$variable == v, , drop = FALSE]
    is_text <- vars$type[k] == "text"
    n <- nrow(sub)
    d <- sum(!sub$match)
    len <- sum(sub$length, na.rm = TRUE)
    ed <- sum(sub$edits, na.rm = TRUE)
    row <- data.frame(variable = v, type = vars$type[k], level = vars$level[k], compared = n, differences = d,
                      variable_match = if (n) 1 - d / n else NA_real_,
                      character_match = if (is_text && len > 0) 1 - ed / len else NA_real_,
                      edit_distance = if (is_text) ed else NA_real_, stringsAsFactors = FALSE)
    if (normalize) {
      ld <- sum(!sub$lenient_match)
      llen <- sum(sub$lenient_length, na.rm = TRUE)
      led <- sum(sub$lenient_edits, na.rm = TRUE)
      row$lenient_differences <- ld
      row$lenient_variable_match <- if (n) 1 - ld / n else NA_real_
      row$lenient_character_match <- if (is_text && llen > 0) 1 - led / llen else NA_real_
    }
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.score_differences <- function(cells, normalize, labels) {
  d <- cells[!cells$match, , drop = FALSE]
  note <- rep(NA_character_, nrow(d))
  if (normalize) note[d$lenient_match] <- "only case, accents, punctuation or spacing"
  note[d$doubt_only] <- "only a final ?"
  note[d$o_missing & !d$g_missing & d$level == "row"] <- paste("row missing from the", labels$out_col)
  note[d$g_missing] <- paste("row not in the", labels$ref_col)
  note[d$failed] <- "page failed"
  out <- data.frame(file = d$file, row_gold = d$row_gold, row_output = d$row_output, variable = d$variable,
                    gold = d$gold, output = d$output, edits = d$edits, note = note, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

# ------- printing -------------------------------------------------------------

.doubt_sentence <- function(dm, agree) {
  paste0(.fmt_int(dm$matched), " of the ", .fmt_int(dm$compared),
         if (agree) " differences are only a final \"?\"" else " wrong values differ only by a final \"?\"")
}

.plural <- function(n, one, many) paste(.fmt_int(n), if (n == 1) one else many)

.list_files <- function(files, n = 5) {
  paste0(paste(utils::head(files, n), collapse = ", "), if (length(files) > n) paste0(" and ", length(files) - n, " more") else "")
}

# One decimal, but never "100.0%" for a figure below 100% (or "0.0%" above
# 0): a reader takes 100.0% to mean no mistakes at all.
.pct1 <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("-")
    s <- sprintf("%.1f%%", 100 * v)
    if (v < 1 && s == "100.0%") return(">99.9%")
    if (v > 0 && s == "0.0%") return("<0.1%")
    s
  }, character(1), USE.NAMES = FALSE)
}

# "2 pages, 412 rows, 10 variables; gemini-3.8-flash"
.score_subtitle <- function(a) {
  parts <- if (identical(a$mode, "text")) paste(.plural(a$n_pages, "page", "pages"), "of free text") else
    c(.plural(a$n_pages, "page", "pages"),
      if (a$row_variables) .plural(a$gold_rows, "row", "rows"),
      .plural(a$n_variables, "variable", "variables"))
  known <- function(m) !is.null(m) && !is.na(m)
  model <- if (identical(a$reference, "run")) {
    if (known(a$model) && known(a$reference_model)) paste(a$model, "against", a$reference_model)
  } else if (known(a$model)) {
    a$model
  }
  paste0(paste(parts, collapse = ", "), if (length(model)) paste0("; ", model))
}

.score_title <- function(a) {
  head <- if (identical(a$reference, "run")) "Agreement with the first run" else "Accuracy against your benchmark"
  paste0(head, " (", .score_subtitle(a), ")")
}

#' @export
print.amanuensis_score <- function(x, ...) {
  a <- attr(x, "about")
  sm <- x$summary
  agree <- identical(a$reference, "run")
  right <- if (agree) "the same" else "right"
  get <- function(m) {
    r <- sm[sm$measure == m, , drop = FALSE]
    if (nrow(r)) r else NULL
  }
  cat(.score_title(a), "\n", sep = "")
  lines <- list()
  line <- function(label, r, unit = "", counts = TRUE) {
    if (is.null(r)) return()
    cnt <- if (counts) paste0(.fmt_int(r$matched), " of ", .fmt_int(r$compared), unit) else ""
    lines[[length(lines) + 1]] <<- c(label, cnt, .pct1(r$value))
  }
  block <- function(lenient) {
    sfx <- if (lenient) " (lenient)" else ""
    if (identical(a$mode, "text")) {
      line(paste("Characters", right), get(paste0("characters", sfx)), counts = FALSE)
      line(paste("Words", right), get(paste0("words", sfx)), counts = FALSE)
      if (!lenient) line(if (agree) "Pages identical" else "Pages entirely right", get("pages"))
      return()
    }
    if (a$row_variables) line(paste("Rows with every value", right), get(paste0("rows", sfx)))
    line(paste("Values", right), get(paste0("values", sfx)))
    ch <- get(paste0("characters", sfx))
    if (!is.null(ch) && ch$compared > 0) line(paste("Characters", right, "(text)"), ch, counts = FALSE)
    if (!lenient && a$page_variables && a$row_variables) line(paste("Pages with every page value", right), get("page values"))
    if (length(a$separate)) {
      line(paste("Other text: characters", right), get(paste0("other text: characters", sfx)), counts = FALSE)
      if (!lenient) line(paste("Other text: words", right), get("other text: words"), counts = FALSE)
    }
  }
  print_lines <- function(indent) {
    if (!length(lines)) return()
    m <- do.call(rbind, lines)
    w1 <- max(nchar(m[, 1]))
    w2 <- max(nchar(m[, 2]))
    for (i in seq_len(nrow(m))) {
      cat(indent, formatC(m[i, 1], width = -w1), "  ", formatC(m[i, 2], width = w2), "  ",
          formatC(m[i, 3], width = 6), "\n", sep = "")
    }
    lines <<- list()
  }
  block(FALSE)
  print_lines("  ")
  if (isTRUE(a$normalize)) {
    cat("  Lenient (ignoring capital letters, accents, punctuation, spacing and \"?\"):\n")
    block(TRUE)
    print_lines("    ")
  }
  dm <- get("doubt marks")
  if (!is.null(dm) && dm$matched > 0) {
    cat("  ", .doubt_sentence(dm, agree), "\n", sep = "")
  }
  bv <- x$by_variable
  bv <- bv[bv$differences > 0 & !bv$variable %in% a$separate, , drop = FALSE]
  if (nrow(bv) && !identical(a$mode, "text")) {
    bv <- bv[order(bv$variable_match, -bv$differences, match(bv$variable, x$by_variable$variable)), , drop = FALSE]
    bv <- utils::head(bv, 2)
    cat("  ", if (agree) "Most disagreement" else "Weakest", ": ",
        paste0(bv$variable, " ", .pct1(bv$variable_match), " (", .fmt_int(bv$differences),
               if (!agree) " wrong" else ifelse(bv$differences == 1, " differs", " differ"), ")", collapse = ", "),
        "\n", sep = "")
  }
  if (length(x$notes)) {
    for (n in x$notes) cat(paste0(strwrap(n, width = getOption("width", 80) - 2, initial = "- ", exdent = 2), "\n"), sep = "")
  }
  nxt <- if (!is.null(a$report)) paste0("Next: View(s$differences); the report is in ", a$report) else if (agree)
    "Next: View(s$differences), or score(run1, run2, report = \"agreement.html\")" else
    "Next: View(s$differences), or score(gold, res, report = \"accuracy.html\")"
  cat(nxt, "\n", sep = "")
  invisible(x)
}
