# output(): the results, presented as a table in R or saved as a file. Nothing
# here calls the model or changes a result; it only re-presents what
# transcribe() saved, so it can be run as often as wanted, on a result
# in memory or on its results folder in a later session.
#
# Judgment calls:
# - The records written are the rows the user gives: a filtered or
#   reordered result is written as it is. The pages, codebook and about
#   sheets always describe the whole run, so a selection of rows still
#   carries its provenance.
# - Pages to check (their reply did not match the codebook) stay out unless
#   include = "all", and then every row carries a page_status column, so a
#   doubtful page can never pass for a checked one.
# - Nothing is overwritten without overwrite = TRUE (Stata users know
#   `replace`). Every file of a call is checked before the first is written,
#   so a call with several files writes all of them or none.
# - Each file is written to a temporary file first and then moved into
#   place, so an error (or a file open in Excel) never leaves half a file.
# - Formats that cannot hold everything say so: Excel cells hold at most
#   32,767 characters, Stata names are short and plain ASCII. The JSON file
#   is the lossless one: it keeps the model's reply for every page as it was.

.output_formats <- c("xlsx", "csv", "dta", "rds", "json", "txt", "text", "md", "docx")

.output_state <- new.env(parent = emptyenv())

#' Save or show the results
#'
#' Turns the result of [transcribe()] into a table in R, or saves it as
#' an Excel, CSV, Stata, Word, text, Markdown, JSON or R file. Nothing is
#' sent to the model: you can save the same result in as many formats as
#' you like, now or in a later session (give the results folder instead of
#' the result).
#'
#' @section Formats:
#' The format follows the file's extension:
#' \describe{
#'   \item{`.xlsx`}{An Excel workbook with the sheets `records` (called
#'     `text` for a free-text transcription), `pages` (every page with its
#'     status, tokens and cost), `codebook` and `about` (the model, settings,
#'     instructions, date and cost of the run). Results from a JSON schema
#'     get an extra sheet for each further list in the schema.}
#'   \item{`.csv`}{One table (the records, unless `table` says otherwise),
#'     in UTF-8. By default the file starts with a byte-order mark so that
#'     Excel on Windows shows accents correctly (`bom = FALSE` leaves it
#'     out). Excel set to Spanish, Catalan and many other languages expects a
#'     semicolon between columns and a comma in numbers (2,5): use
#'     `sep = ";"`, which also writes numbers with a decimal comma. The values
#'     are written exactly as transcribed, but Excel changes some of them
#'     when it opens a CSV file: 1-2 becomes a date, 007 becomes 7, and a
#'     value starting with `=`, `+`, `-` or `@` is run as a formula. To work
#'     in Excel, save an `.xlsx` file instead, where every value stays as it
#'     was.}
#'   \item{`.dta`}{A Stata 14 file. Column names are made valid for Stata
#'     (the changes are listed), the codebook descriptions become variable
#'     labels, and yes/no variables become 0/1 with value labels.}
#'   \item{`.rds`}{The whole result as an R object; `readRDS()` gets it back.}
#'   \item{`.json`}{One entry per page with its file, source, page, status
#'     and the model's reply exactly as returned (nothing is lost).}
#'   \item{`.txt`, `.md`, `.docx`}{A document to read: one section per page,
#'     with its text, or its values and rows as a table.}
#' }
#' Old Excel `.xls` files cannot be written; `.xlsx` opens in every version
#' of Excel.
#'
#' @section Which rows:
#' The records written are the rows of `x`: if you filtered or sorted the
#' result in R, that is what is saved. The `pages`, `codebook` and `about`
#' sheets always describe the whole run. Pages whose reply did not match the
#' codebook ("to check") are left out unless `include = "all"`, which adds
#' them with a `page_status` column; it rebuilds the table from the run, so
#' use it on the complete result rather than on a selection of rows.
#'
#' Word, text, Markdown and JSON files are arranged by page, so a selection
#' of columns saved in those formats must keep the column `file`, e.g.
#' `res[rows, c("file", "surname", "age")]`. Excel and CSV files hold any
#' selection as it is.
#'
#' @param x The result of [transcribe()] (or part of it), or its results
#'   folder, or an `.rds` file saved by `output()`.
#' @param file Where to save, e.g. `"results.xlsx"`; several files at once
#'   are fine, e.g. `c("results.xlsx", "results.dta")`. Leave it out to get
#'   the table back in R instead.
#' @param table Which table to return or write to a one-table format (CSV,
#'   Stata, JSON, text, Word): `"records"` (default; the extracted data),
#'   `"pages"` (one row per page: status, problems, tokens, cost) or
#'   `"codebook"` (the variables). Excel files always hold all three.
#' @param include `"ok"` (default) for the pages read correctly only; `"all"`
#'   to add the pages to check, marked in a column `page_status`.
#' @param overwrite Replace files that already exist? Default `FALSE`.
#' @param bom For CSV: start the file with a byte-order mark, which Excel on
#'   Windows needs to show accents correctly. Default `TRUE`.
#' @param sep For CSV: the character between columns. Default `","`; use
#'   `";"` for Excel in Spanish, Catalan, French, German and other languages
#'   that write decimals with a comma.
#' @param dec For CSV: the decimal mark of numbers, `"."` or `","`. By default
#'   it follows `sep`: a comma (2,5) with `sep = ";"`, as that Excel expects,
#'   and a point (2.5) otherwise. Whole numbers and text are never changed.
#' @param contract Only for a results folder holding results made with
#'   different instructions or codebooks: which set to use (the ids are
#'   listed in the error message and in `contract.json`).
#' @return Without `file`, the table as a plain data frame. With `file`, the
#'   path(s) written, invisibly.
#' @seealso [transcribe()]
#' @export
#' @examples
#' ex <- example_project()
#' pages <- images(ex$scans, save_to = file.path(ex$folder, "pages"))
#' res <- transcribe(pages, ex$instructions, ex$codebook, model = "example")
#'
#' output(res)                       # the records as a data frame
#' output(res, table = "pages")      # one row per page
#'
#' out <- file.path(ex$folder, "padro1930.xlsx")
#' output(res, out)
#' output(res, file.path(ex$folder, c("padro1930.csv", "padro1930.docx")))
#'
#' # only some rows
#' output(res[res$source_page == 1, ], file.path(ex$folder, "page1.csv"))
output <- function(x, file = NULL, table = c("records", "pages", "codebook"), include = c("ok", "all"),
                   overwrite = FALSE, bom = TRUE, sep = ",", dec = if (identical(sep, ";")) "," else ".",
                   contract = NULL) {
  if (missing(x)) {
    .abort(c("Tell {.fn output} which result to save.",
             "i" = "For example {.code output(res, \"results.xlsx\")}, where {.code res} came from {.fn transcribe}."),
           class = "amanuensis_error_input")
  }
  # output("results.xlsx", res): the two arguments the other way round
  if (inherits(file, "amanuensis_result") && is.character(x)) {
    .abort(c("The result comes first, then the file name.",
             "i" = "Write {.code output(res, \"{x[1]}\")}."), class = "amanuensis_error_input")
  }
  table <- rlang::arg_match(table)
  include <- rlang::arg_match(include)
  .check_flag(overwrite)
  .check_flag(bom)
  .check_string(sep, arg = "sep")
  if (nchar(sep) != 1 || sep %in% c("\"", "\n", "\r")) {
    .abort(c("{.arg sep} must be a single character such as {.val ,} or {.val ;}.", "x" = "You gave {.val {sep}}."),
           class = "amanuensis_error_input")
  }
  .check_string(dec, arg = "dec")
  if (!dec %in% c(".", ",")) {
    .abort(c("{.arg dec} must be {.val .} or {.val ,}.", "x" = "You gave {.val {dec}}."), class = "amanuensis_error_input")
  }
  if (identical(dec, sep)) {
    .abort(c("{.arg dec} and {.arg sep} cannot both be {.val {sep}}: numbers would then look like two columns.",
             "i" = "Use {.code sep = \";\"} with a decimal comma, or {.code sep = \",\"} with a decimal point."),
           class = "amanuensis_error_input")
  }
  if (!is.null(contract)) .check_string(contract, arg = "contract")
  if (!is.null(file)) {
    if (!is.character(file) || !length(file) || anyNA(file) || any(!nzchar(file))) {
      .abort("{.arg file} must be one or more file names, e.g. {.val results.xlsx}.", class = "amanuensis_error_input")
    }
    file <- unique(file)
    .check_output_files(file, overwrite)
  }

  res <- .output_source(x, contract)
  info <- .result_info(res)
  if (is.null(file)) return(.output_table(res, info, table, include))
  if (table == "records") .check_page_column(res, info, file)

  if (table == "records" && include == "ok" && !any(info$pages$status == "ok")) {
    .inform(c("!" = "The table has no rows: no page has been read correctly yet.",
              "i" = "{.code output(res, table = \"pages\")} shows what happened to each page."))
  }
  for (f in file) .write_output(res, info, f, table, include, bom, sep, dec)
  if (table == "records" && include == "ok" && any(.ext(file) != "rds")) .note_pages_to_check(info)
  invisible(file)
}

# ------- arguments --------------------------------------------------------------

.check_flag <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    .abort("{.arg {arg}} must be {.code TRUE} or {.code FALSE}.", call = call, class = "amanuensis_error_input")
  }
  invisible(x)
}

# Every file checked before any is written: format known, not a folder, not
# inside R's library, not already there (unless overwrite = TRUE).
.check_output_files <- function(files, overwrite, call = rlang::caller_env()) {
  for (f in files) {
    ext <- .ext(f)
    if (ext == "xls") {
      .abort(c("Old .xls files cannot be written; use .xlsx, which every version of Excel opens.",
               "i" = "For example {.code output(res, \"{(.stem(f))}.xlsx\")}."), call = call, class = "amanuensis_error_input")
    }
    if (dir.exists(f)) {
      .abort(c("{.path {(.show_path(f))}} is a folder.", "i" = "Give a file name inside it, e.g. {.code \"{f}/results.xlsx\"}."),
             call = call, class = "amanuensis_error_input")
    }
    if (!ext %in% .output_formats) {
      .abort(c(if (nzchar(ext)) "{.file {basename(f)}} has the extension {.val {ext}}, which {.fn output} cannot write." else
                 "{.file {basename(f)}} has no extension, so {.fn output} cannot tell which format to write.",
               "i" = "Use .xlsx (Excel), .csv, .dta (Stata), .docx (Word), .txt, .md, .json or .rds (R)."),
             call = call, class = "amanuensis_error_input")
    }
    # every .json file in a results folder is read as a page's result
    if (ext == "json" && file.exists(file.path(dirname(f), "contract.json"))) {
      .abort(c("{.path {(.show_path(dirname(f)))}} is a results folder: a .json file saved there would be taken for a page's result.",
               "i" = "Save it next to the folder instead, e.g. {.code \"{file.path(dirname(dirname(f)), basename(f))}\"}."),
             call = call, class = "amanuensis_error_input")
    }
    tryCatch(.refuse_library_path(f), amanuensis_error = function(e) {
      .abort(c("{.path {(.show_path(f))}} is inside R's package library, which is replaced when packages are updated.",
               "i" = "Save it in a folder of your own, such as your project folder."), call = call,
             class = "amanuensis_error_input")
    })
  }
  there <- files[file.exists(files)]
  if (length(there) && !isTRUE(overwrite)) {
    .abort(c("{.path {(.show_path(there))}} already exist{?s/}.",
             "i" = "Choose another name, or add {.code overwrite = TRUE} to replace {cli::qty(length(there))}{?it/them}."),
           call = call, class = "amanuensis_error_input")
  }
  invisible(files)
}

# Documents (txt, md, docx) and the JSON file are arranged by page, and an
# Excel file of a JSON-schema result has a sheet for each further list of
# the pages the rows come from: all of them find a row's page through its
# `file` column. A selection of columns without it (res[, c("surname",
# "age")] is still a result) would otherwise give every page "(no rows on
# this page)", or no page at all, under a "Wrote" message. Row order is no
# guide either (dplyr renumbers rows, sorting reorders them), so the column
# is asked for, before any file of the call is written.
.check_page_column <- function(res, info, files, call = rlang::caller_env()) {
  if ("file" %in% names(res)) return(invisible())
  ext <- .ext(files)
  cb <- info$codebook
  main <- .records_path(cb)
  extra_lists <- identical(cb$kind, "json") &&
    length(Filter(function(p) !identical(p, main), .array_paths(cb$schema))) > 0
  by_page <- ext %in% c("json", "txt", "text", "md", "docx") | (ext == "xlsx" & extra_lists)
  if (!any(by_page)) return(invisible())
  plain <- if (extra_lists) "A .csv file holds" else "An .xlsx or .csv file holds"
  .abort(c("{.file {basename(files[by_page])}} {cli::qty(sum(by_page))}{?is/are} arranged by page, and this selection has no {.field file} column to say which page each row comes from.",
           "i" = "Keep {.field file} in the selection, e.g. {.code res[rows, c(\"file\", \"surname\", \"age\")]}.",
           "i" = "{plain} the selection exactly as it is."),
         call = call, class = "amanuensis_error_input")
}

# The result to present: a result object (or part of one), a results folder,
# or an .rds written by output().
.output_source <- function(x, contract, call = rlang::caller_env()) {
  if (inherits(x, "amanuensis_result") && !is.null(.result_info(x))) {
    if (!is.null(contract)) {
      .inform(c("i" = "{.arg contract} is only needed with a results folder; ignored for a result already in R."))
    }
    return(x)
  }
  if (.is_string(x) && dir.exists(x)) return(.result_from_folder(x, contract = contract, call = call))
  if (.is_string(x) && file.exists(x) && .ext(x) == "rds") {
    obj <- tryCatch(readRDS(x), error = function(e) NULL)
    if (inherits(obj, "amanuensis_result") && !is.null(.result_info(obj))) return(obj)
    .abort(c("{.file {basename(x)}} does not hold a result of {.fn transcribe}.",
             "i" = "Give the result itself, or its results folder."), call = call, class = "amanuensis_error_input")
  }
  if (is.data.frame(x)) {
    .abort(c("This table is no longer linked to its extraction run (that happens after {.fn as.data.frame}, {.fn merge} and some other functions).",
             "i" = "Give {.fn output} the result of {.fn transcribe} (or part of it with {.code res[rows, ]}), or its results folder.",
             "i" = "To save a plain table, use {.fn write.csv} or {.fn writexl::write_xlsx}."),
           call = call, class = "amanuensis_error_input")
  }
  .as_result(x, call = call)
}

# ------- tables -----------------------------------------------------------------

.result_kind <- function(info) info$codebook$kind %||% "table"

# The records table to present. With include = "all" the table is rebuilt
# from the run (pages to check have no rows in `x`), keeping the columns of
# `x` in their order, with page_status after source_page.
.output_records <- function(res, info, include, call = rlang::caller_env()) {
  tab <- as.data.frame(res)
  rownames(tab) <- NULL
  if (include == "ok") return(tab)
  if (!identical(nrow(res), info$n_rows)) {
    .abort(c("{.code include = \"all\"} rebuilds the table from the whole run, so it cannot be used on a selection of rows.",
             "i" = "Use {.code output(res, include = \"all\")} on the complete result, then select rows in the table it returns."),
           call = call, class = "amanuensis_error_input")
  }
  full <- .result_table(res, "all")
  if (!"page_status" %in% names(full)) full$page_status <- rep(NA_character_, nrow(full))
  extra <- setdiff(names(tab), names(full))
  if (length(extra)) {
    .abort(c("{.code include = \"all\"} rebuilds the table from the run, and {cli::qty(length(extra))}column{?s} {.field {extra}} {?is/are} not part of it.",
             "i" = "Save the table without them, or without {.code include = \"all\"}."),
           call = call, class = "amanuensis_error_input")
  }
  cols <- setdiff(names(tab), "page_status")
  pos <- match("source_page", cols)
  cols <- if (is.na(pos)) c("page_status", cols) else append(cols, "page_status", after = pos)
  out <- full[, cols, drop = FALSE]
  rownames(out) <- NULL
  out
}

.codebook_table <- function(cb) {
  v <- cb$variables
  data.frame(variable = v$variable, type = v$type, level = v$level, description = v$description,
             values = vapply(v$values, function(z) paste(z, collapse = "; "), ""),
             stringsAsFactors = FALSE)
}

.output_table <- function(res, info, table, include, call = rlang::caller_env()) {
  out <- switch(table,
    records = .output_records(res, info, include, call = call),
    pages = info$pages,
    codebook = .codebook_table(info$codebook)
  )
  out <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  rownames(out) <- NULL
  out
}

# The page records behind the rows being written, in reading order: every
# page in scope for the complete result (pages with no rows included), only
# the pages the rows come from for a selection. The rows' `file` column
# decides (output() makes sure it is there): the right number of rows alone
# does not make a complete result, since rows can be dropped and others
# repeated.
.output_pages <- function(res, info, include, tab) {
  keep <- if (include == "all") c("ok", "check") else "ok"
  in_scope <- Filter(function(r) r$status %in% keep, info$records)
  keys <- unique(.page_key(tab[["file"]]))
  if (identical(nrow(res), info$n_rows)) {
    full <- .result_table(res, if (include == "all") "all" else "ok")
    if (all(.page_key(unique(full[["file"]])) %in% keys)) return(unname(in_scope))
  }
  recs <- info$records[keys]
  unname(Filter(Negate(is.null), recs))
}

.note_pages_to_check <- function(info) {
  n <- sum(info$pages$status == "check")
  if (n) {
    .inform(c("i" = "{n} page{?s} to check {?is/are} not in the file (the reply did not match the codebook); add {?it/them} with {.code include = \"all\"}."))
  }
}

# ------- writing ------------------------------------------------------------------

.write_output <- function(res, info, f, table, include, bom, sep, dec = ".", call = rlang::caller_env()) {
  ext <- .ext(f)
  dir <- dirname(f)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    .inform(c("i" = "Created the folder {.path {(.show_path(dir))}}."))
  }
  switch(ext,
    xlsx = .output_xlsx(res, info, f, table, include, call),
    csv = .output_csv(res, info, f, table, include, bom, sep, dec, call),
    dta = .output_dta(res, info, f, table, include, call),
    rds = .output_rds(res, info, f, table, include, call),
    json = .output_json(res, info, f, table, include, call),
    txt = , text = , md = .output_text(res, info, f, table, include, md = ext == "md", call),
    docx = .output_docx(res, info, f, table, include, call)
  )
  invisible(f)
}

# A binary writer (writexl, haven, saveRDS) writes to a temporary file, which
# is then moved into place by the same atomic writer as every text file.
.write_via_temp <- function(path, writer, call = rlang::caller_env()) {
  tmp <- tempfile(fileext = paste0(".", .ext(path)))
  on.exit(unlink(tmp), add = TRUE)
  .with_write_errors(path, writer(tmp), call = call)
  .write_raw_atomic(readBin(tmp, "raw", n = file.size(tmp)), path)
}

.wrote <- function(f, what) {
  .inform(c("v" = "Wrote {.path {(.show_path(f))}} ({what})."))
}

.rows_label <- function(n) paste0(.fmt_int(n), " row", if (n != 1) "s")

.pages_label <- function(n) paste0(.fmt_int(n), " page", if (n != 1) "s")

.free_text_fit <- function(info, table, what) {
  if (table == "records" && .result_kind(info) == "text") {
    .inform(c("i" = "Free text: {what} has one row per page, with the page's text in the column {.field text}."))
  }
}

# ------- CSV ----------------------------------------------------------------------

# The values are written exactly as transcribed, whatever Excel will make of
# them: a leading apostrophe or tab would stop Excel running "= id." as a
# formula, but would corrupt the file for R, Stata and Python, and quoting
# does not stop Excel either. So the file stays faithful and the user is told,
# naming the columns, and pointed to .xlsx, where every value stays text.
.output_csv <- function(res, info, f, table, include, bom, sep, dec = ".", call) {
  df <- .output_table(res, info, table, include, call = call)
  .free_text_fit(info, table, "the CSV file")
  .write_csv(df, f, bom = bom, sep = sep, dec = dec)
  .wrote(f, .rows_label(nrow(df)))
  if (dec == ",") {
    .inform(c("i" = "Numbers with decimals are written with a decimal comma (2,5), as Excel in Spanish, Catalan, French or German expects; {.code dec = \".\"} keeps the point."))
  }
  if (table == "records") {
    .inform(c("i" = "The pages table (status, tokens and cost of every page) can be written with {.code table = \"pages\"}."))
  }
  formula <- .formula_like(df)
  if (length(formula)) {
    .inform(c("!" = "{cli::qty(length(formula))}The column{?s} {.field {formula}} {?has/have} values starting with =, +, - or @, which Excel runs as formulas when it opens a CSV file (the file itself keeps them as written).",
              "i" = "To look at the results in Excel, save an .xlsx file: {.code output(res, \"results.xlsx\")}."))
  }
  if (!isTRUE(.output_state$csv_hint)) {
    .output_state$csv_hint <- TRUE
    .inform(c("i" = "To work in Excel, an .xlsx file is safer: Excel changes some values when it opens a CSV file (1-2 becomes a date, 007 becomes 7).",
              "i" = if (sep == ",") "If Excel shows everything in one column (Excel in Spanish, Catalan, French, German...), write the file with {.code sep = \";\"}, which also writes decimals with a comma."))
  }
}

# Text columns with a value Excel would take for a formula: one starting with
# =, +, - or @ ("= id.", "- id. -", "+3"). A plain negative number ("-5") is
# left out: Excel reads it as the same number.
.formula_like <- function(df) {
  hit <- vapply(df, function(v) {
    if (!is.character(v)) return(FALSE)
    v <- v[!is.na(v)]
    any(grepl("^[=+@-]", v) & !grepl("^-[0-9]+([.,][0-9]+)?$", v))
  }, logical(1))
  names(df)[hit]
}

# ------- RDS ----------------------------------------------------------------------

.output_rds <- function(res, info, f, table, include, call) {
  if (table != "records" || include != "ok") {
    .inform(c("i" = "An .rds file holds the whole result object; {.arg table} and {.arg include} do not apply to it."))
  }
  .write_via_temp(f, function(tmp) saveRDS(res, tmp), call = call)
  .wrote(f, .rows_label(nrow(res)))
  .inform(c("i" = "{.code readRDS(\"{f}\")} gets the result back, with its pages table and codebook."))
}

# ------- JSON ---------------------------------------------------------------------

# Lossless: the model's parsed reply for each page, written back as JSON.
.output_json <- function(res, info, f, table, include, call) {
  if (table == "codebook") {
    v <- info$codebook$variables
    v$values <- lapply(v$values, as.list)
    rownames(v) <- NULL
    .write_text(c(.to_json(v, pretty = TRUE), ""), f)
    .wrote(f, paste0(nrow(v), " variable", if (nrow(v) != 1) "s"))
    return(invisible())
  }
  if (table == "pages") {
    df <- info$pages
    rownames(df) <- NULL
    .write_text(c(.to_json(df, pretty = TRUE), ""), f)
    .wrote(f, .pages_label(nrow(df)))
    return(invisible())
  }
  tab <- .output_records(res, info, include, call = call)
  recs <- .output_pages(res, info, include, tab)
  pages <- lapply(recs, function(r) {
    obj <- list(file = r$file, source = r$source %||% r$file, source_page = as.integer(r$source_page %||% NA),
                status = r$status)
    if (identical(r$status, "check")) obj$message <- r$message %||% NA_character_
    obj$data <- r$data %||% .json_object()
    obj
  })
  .write_text(c(.to_json(pages, pretty = TRUE), ""), f)
  .wrote(f, .pages_label(length(pages)))
  if (!identical(nrow(res), info$n_rows)) {
    .inform(c("i" = "The JSON file holds whole pages (the model's complete reply for each): the {length(pages)} page{?s} your rows come from."))
  }
}

# ------- Stata --------------------------------------------------------------------

.stata_reserved <- c("_all", "_b", "byte", "_coef", "_cons", "double", "float", "if", "in", "int", "long",
                     "_n", "_N", "_pi", "_pred", "_rc", "_skip", "strL", "using", "with")

# Valid Stata names: letters, digits and _ only, starting with a letter, at
# most 32 characters, not a reserved word, all different. Stata 14 itself
# accepts accented letters, but haven (ReadStat) refuses any non-ASCII
# character in a name, so accents are folded (edat_\u00e0nima ->
# edat_anima) and other characters become _.
# A name that is already valid keeps it: when a changed name comes out the
# same as one of those (edat_\u00e0nima and edat_anima, a.b and a_b), the
# changed one takes the _2, wherever it stands, so the name a Stata user
# types never points at another column. Among changed names (or exact
# repeats), the first comes first.
.stata_names <- function(x) {
  orig <- x
  x <- .fold_accents(x)
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  trail <- !grepl("_$", orig) & grepl("_$", x)
  x[trail] <- sub("_+$", "", x[trail])
  x[!nzchar(x)] <- "v"
  start <- !grepl("^[A-Za-z]", x)
  x[start] <- paste0("v", x[start])
  x <- substr(x, 1, 32)
  res <- x %in% .stata_reserved | grepl("^str[0-9]+$", x)
  x[res] <- paste0(substr(x[res], 1, 31), "_")
  # names needing no change are claimed first (an exact repeat only once)
  kept <- !is.na(orig) & x == orig
  kept[kept] <- !duplicated(x[kept])
  taken <- x[kept]
  for (i in which(!kept)) {
    if (!x[i] %in% taken) {
      taken <- c(taken, x[i])
      next
    }
    later <- x[setdiff(seq_along(x)[-seq_len(i)], which(kept))]
    k <- 2
    repeat {
      suffix <- paste0("_", k)
      cand <- paste0(substr(x[i], 1, 32 - nchar(suffix)), suffix)
      if (!cand %in% c(taken, later)) break
      k <- k + 1
    }
    x[i] <- cand
    taken <- c(taken, cand)
  }
  x
}

.provenance_labels <- c(
  file = "Page image file",
  source = "Source document the page comes from",
  source_page = "Page number within the source document (not a folio)",
  row = "Row on the page, in reading order",
  page_status = "ok, or check (the reply did not match the codebook)"
)

.pages_labels <- c(
  status = "ok, check, failed or not run",
  problem = "Kind of problem, if any",
  n_rows = "Rows the model returned for the page",
  model = "Model asked", model_version = "Model version that answered",
  engine = "sync, batch or flex", finish_reason = "Why the model stopped",
  input_tokens = "Input tokens", cached_tokens = "Cached input tokens", image_tokens = "Image tokens",
  thinking_tokens = "Thinking tokens", output_tokens = "Output tokens",
  cost_usd = "Estimated cost in US dollars", attempts = "Attempts",
  notes = "Notes worth a look", message = "What went wrong, in words", date = "Date of the reply (UTC)"
)

.output_dta <- function(res, info, f, table, include, call) {
  rlang::check_installed("haven", "to write Stata files", call = call)
  df <- .output_table(res, info, table, include, call = call)
  .free_text_fit(info, table, "the Stata file")
  cb <- info$codebook
  desc <- stats::setNames(cb$variables$description, cb$variables$variable)
  known <- switch(table, records = c(.provenance_labels, desc), pages = c(.provenance_labels, .pages_labels),
                  codebook = character())
  labels <- unname(known[names(df)])
  labels[is.na(labels) | !nzchar(labels)] <- NA_character_
  long <- !is.na(labels) & nchar(labels) > 80
  labels[long] <- paste0(substr(labels[long], 1, 77), "...")
  for (j in seq_along(df)) {
    v <- df[[j]]
    if (is.logical(v)) {
      v <- haven::labelled(as.integer(v), labels = c(no = 0L, yes = 1L))
    }
    if (!is.na(labels[j])) attr(v, "label") <- labels[j]
    df[[j]] <- v
  }
  new <- .stata_names(names(df))
  changed <- new != names(df)
  if (any(changed)) {
    lines <- paste0(names(df)[changed], " -> ", new[changed])
    .inform(c("i" = "Stata needs short plain names; {sum(changed)} column{?s} renamed:",
              stats::setNames(lines, rep(" ", length(lines)))))
  }
  names(df) <- new
  sources <- unique(info$pages$source)
  data_label <- paste0("amanuensis: ", paste(utils::head(sources, 3), collapse = ", "), if (length(sources) > 3) " ...")
  data_label <- if (nchar(data_label) > 80) paste0(substr(data_label, 1, 77), "...") else data_label
  .write_via_temp(f, function(tmp) haven::write_dta(df, tmp, version = 14, label = data_label), call = call)
  .wrote(f, .rows_label(nrow(df)))
  if (any(long)) {
    .inform(c("i" = "{sum(long)} variable label{?s} shortened to Stata's limit of 80 characters (the full descriptions are in the codebook)."))
  }
}

# ------- Excel --------------------------------------------------------------------

.excel_max_chars <- 32767L

.output_xlsx <- function(res, info, f, table, include, call) {
  if (table != "records") {
    .inform(c("i" = "An Excel file holds all the tables (records, pages, codebook, about); {.code table = \"{table}\"} only matters for one-table formats."))
  }
  cb <- info$codebook
  recs_tab <- .output_records(res, info, include, call = call)
  main <- if (.result_kind(info) == "text") "text" else "records"
  .free_text_fit(info, "records", "the records sheet")
  sheets <- list()
  sheets[[main]] <- recs_tab
  if (cb$kind == "json") {
    extra <- .extra_tables(.output_pages(res, info, include, recs_tab), cb)
    for (nm in names(extra)) {
      sheet <- .sheet_name(nm, c(names(sheets), "pages", "codebook", "about"))
      sheets[[sheet]] <- extra[[nm]]
    }
  }
  sheets$pages <- info$pages
  sheets$codebook <- .codebook_table(cb)
  sheets$about <- .about_table(res, info, include, nrow(recs_tab))
  too_many <- vapply(sheets, nrow, integer(1)) > 1048575L
  if (any(too_many)) {
    .abort(c("The sheet {.val {names(sheets)[too_many][1]}} has more rows than Excel can hold (1,048,575).",
             "i" = "Write a .csv file instead."), call = call, class = "amanuensis_error_input")
  }
  cut <- 0L
  for (nm in names(sheets)) {
    fit <- .excel_fit(sheets[[nm]])
    cut <- cut + attr(fit, "cut")
    sheets[[nm]] <- fit
  }
  if (cut) {
    .warn(c("{cut} cell{?s} held more than 32,767 characters, the most an Excel cell can hold, and {?was/were} cut short in the Excel file.",
            "i" = "The full text is in a .txt, .docx or .json file: {.code output(res, \"results.txt\")}."))
  }
  book <- .xl_book(sheets)
  .write_via_temp(f, function(tmp) writexl::write_xlsx(book, tmp), call = call)
  others <- setdiff(names(sheets), main)
  .wrote(f, paste0(.rows_label(nrow(recs_tab)), " in sheet ", main, "; sheets ", paste(others, collapse = ", ")))
}

# Cells longer than Excel allows are cut, marked with "..." at the end.
.excel_fit <- function(df) {
  cut <- 0L
  for (j in seq_along(df)) {
    v <- df[[j]]
    if (!is.character(v)) next
    long <- !is.na(v) & nchar(v, type = "chars", allowNA = TRUE, keepNA = FALSE) > .excel_max_chars
    long[is.na(long)] <- FALSE
    if (any(long)) {
      v[long] <- paste0(substr(v[long], 1, .excel_max_chars - 3L), "...")
      cut <- cut + sum(long)
      df[[j]] <- v
    }
  }
  attr(df, "cut") <- cut
  df
}

# Sheets with a frozen header row and column widths that fit the content,
# when writexl can do it (version 2.0 and later); plain sheets otherwise.
.xl_book <- function(sheets) {
  if (!"xl_sheet" %in% getNamespaceExports("writexl")) {
    return(lapply(sheets, function(df) { attr(df, "cut") <- NULL; df }))
  }
  lapply(sheets, function(df) {
    attr(df, "cut") <- NULL
    widths <- .col_widths(df)
    cols <- lapply(seq_along(df), function(j) writexl::xl_col_spec(j, width = widths[j]))
    writexl::xl_sheet(df, cols = if (length(cols)) cols else NULL, freeze = "A2")
  })
}

.col_widths <- function(df) {
  vapply(seq_along(df), function(j) {
    v <- utils::head(df[[j]], 500)
    if (is.logical(v)) return(max(8, nchar(names(df)[j]) + 2))
    s <- .chr_values(v)
    s <- s[!is.na(s)]
    first_line <- sub("\n.*", "", s)
    longest <- max(c(nchar(names(df)[j]), nchar(first_line, type = "chars", allowNA = TRUE, keepNA = FALSE)), na.rm = TRUE)
    min(max(longest + 2, 8), 60)
  }, numeric(1))
}

.sheet_name <- function(name, taken) {
  base <- substr(gsub("[][\\\\/?*:]", "_", name), 1, 31)
  out <- base
  k <- 2
  while (tolower(out) %in% tolower(taken)) {
    suffix <- paste0("_", k)
    out <- paste0(substr(base, 1, 31 - nchar(suffix)), suffix)
    k <- k + 1
  }
  out
}

# What the about sheet records: enough to cite and to repeat the run.
.about_table <- function(res, info, include, n_rows) {
  p <- info$pages
  cb <- info$codebook
  ins <- info$instructions
  s <- info$settings
  counts <- table(factor(p$status, levels = c("ok", "check", "failed", "not run")))
  groups <- function(v) {
    v <- v[!is.na(v) & nzchar(v)]
    if (!length(v)) return("not recorded")
    t <- table(v)
    paste0(names(t), " (", .pages_label(as.integer(t)), ")", collapse = "; ")
  }
  recs <- info$records
  priced <- Filter(function(r) !is.null(r$price), recs)
  price_dates <- sort(unique(unlist(lapply(priced, function(r) r$price$date))))
  price_text <- if (length(priced)) {
    pr <- priced[[1]]$price
    paste0("input $", pr$input, ", cached input $", pr$cached %||% pr$input, ", output $", pr$output,
           " per million tokens", if (isTRUE(pr$discount)) " (half price: batch or flex)" else "")
  } else "not recorded"
  dates <- sort(p$date[!is.na(p$date)])
  practice <- (s$model %||% "") %in% .practice_models
  rules <- ins$rules_id %||% (if (isTRUE(ins$rules)) "standard rules added" else "none")
  text <- ins$full %||% ins$text %||% ""
  if (nchar(text) > .excel_max_chars) text <- paste0(substr(text, 1, .excel_max_chars - 3L), "...")
  items <- c(
    package = paste0("amanuensis ", utils::packageVersion("amanuensis")),
    written = .utc_now(),
    sources = paste(unique(p$source), collapse = "; "),
    pages = paste0(nrow(p), " (", counts[["ok"]], " ok, ", counts[["check"]], " to check, ",
                   counts[["failed"]], " failed, ", counts[["not run"]], " not run)"),
    rows_written = paste0(n_rows, if (!identical(nrow(res), info$n_rows)) paste0(" (a selection of the ", info$n_rows, " rows)") else ""),
    included = if (include == "all") "pages read correctly and pages to check (see the column page_status)" else
      "pages read correctly only (status ok)",
    model = s$model %||% NA_character_,
    model_versions = groups(p$model_version),
    engine = groups(p$engine),
    resolution = s$resolution %||% NA_character_,
    thinking = s$thinking %||% NA_character_,
    max_tokens = as.character(s$max_tokens %||% NA),
    extracted = if (length(dates)) paste(unique(c(dates[1], dates[length(dates)])), collapse = " to ") else "not recorded",
    contract = info$contract %||% NA_character_,
    instructions_source = ins$source %||% NA_character_,
    rules = rules,
    codebook_source = cb$source %||% NA_character_,
    codebook_kind = switch(cb$kind, table = if (isTRUE(cb$free_text)) "table plus free text" else "table",
                           text = "free text", json = "JSON schema", cb$kind),
    cost_usd = format(round(sum(p$cost_usd, na.rm = TRUE), 6), scientific = FALSE),
    cost_note = if (practice) "practice model: nothing was charged; shown at gemini-3.8-flash prices" else
      "estimate at Google's paid prices; keys without billing are not charged",
    prices = price_text,
    prices_date = if (length(price_dates)) paste(price_dates, collapse = "; ") else "not recorded",
    results_folder = info$folder %||% "kept in memory only",
    r_version = R.version.string,
    instructions_text = text
  )
  data.frame(item = names(items), value = unname(items), stringsAsFactors = FALSE)
}

# For a JSON schema, every further list of objects (other than the one the
# records table is made of) as a table of its own: file, source, page and
# row, then its fields (with those of any enclosing list, as in the records
# table). Lists of plain values are joined with "; ".
.extra_tables <- function(recs, cb) {
  main <- .records_path(cb)
  paths <- Filter(function(p) !identical(p, main), .array_paths(cb$schema))
  out <- list()
  for (path in paths) {
    rows <- list()
    for (r in recs) {
      if (is.null(r$data)) next
      rr <- .page_rows(r$data, path)
      prov <- list(file = r$file, source = r$source %||% r$file, source_page = as.integer(r$source_page %||% NA))
      for (k in seq_along(rr)) {
        vals <- lapply(rr[[k]], function(v) {
          if (!is.list(v)) return(v)
          paste(vapply(v, function(e) {
            if (is.null(e)) "" else if (is.numeric(e)) .chr_values(e[1]) else as.character(e)[1]
          }, ""), collapse = "; ")
        })
        rows[[length(rows) + 1]] <- c(prov, list(row = k), vals)
      }
    }
    cols <- unique(c("file", "source", "source_page", "row", unlist(lapply(rows, names))))
    df <- lapply(stats::setNames(cols, cols), function(col) {
      vals <- lapply(rows, function(r) r[[col]])
      type <- if (col %in% c("source_page", "row", .counter_cols(path))) "integer" else
        if (col %in% c("file", "source")) "text" else .guess_type(vals)
      .as_column(vals, type)
    })
    out[[paste(path, collapse = ".")]] <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  }
  out
}

# Paths of the innermost lists of objects in a schema, nested lists of
# objects included (tables -> rows), in schema order.
.array_paths <- function(schema) {
  out <- list()
  is_obj_array <- function(p) {
    identical(setdiff(unlist(p$type), "null"), "array") && is.list(p$items) &&
      (identical(setdiff(unlist(p$items$type), "null"), "object") || !is.null(p$items$properties))
  }
  is_plain_obj <- function(p) {
    identical(setdiff(unlist(p$type), "null"), "object") || (is.null(p$type) && !is.null(p$properties))
  }
  walk <- function(props, path) {
    for (nm in names(props)) {
      p <- props[[nm]]
      if (!is.list(p)) next
      if (is_plain_obj(p)) {
        # lists inside a plain object (body.people) are found too
        walk(p$properties %||% list(), c(path, nm))
        next
      }
      if (!is_obj_array(p)) next
      before <- length(out)
      walk(p$items$properties %||% list(), c(path, nm))
      if (length(out) == before) out[[length(out) + 1]] <<- c(path, nm)
    }
  }
  schema <- .deref_schema_tree(schema)
  walk(schema$properties %||% list(), character())
  out
}

# ------- documents (txt, md, docx) ------------------------------------------------

# One section per page: list(file, source, source_page, status, message,
# values = named list of page values, text = the page's text or NULL,
# rows = data frame of its rows or NULL when the codebook has no rows).
.page_sections <- function(tab, recs, cb) {
  prov <- c("file", "source", "source_page", "row", "page_status")
  has_rows <- !is.null(.records_path(cb))
  page_vars <- cb$variables$variable[cb$variables$level == "page"]
  page_cols <- if (has_rows) intersect(page_vars, names(tab)) else setdiff(names(tab), prov)
  row_cols <- setdiff(names(tab), c(prov, page_cols))
  keys <- .page_key(tab[["file"]])
  lapply(recs, function(r) {
    sub <- tab[keys == .page_key(r$file), , drop = FALSE]
    vals <- if (nrow(sub)) {
      as.list(sub[1, page_cols, drop = FALSE])
    } else if (!is.null(r$data) && length(page_cols)) {
      u <- .page_unit(r$data, cb)$page
      as.list(u[intersect(page_cols, names(u))])
    } else list()
    text <- NULL
    if ("text" %in% names(vals)) {
      text <- vals$text
      vals$text <- NULL
    }
    rows <- if (has_rows) sub[, intersect(c("row", row_cols), names(sub)), drop = FALSE] else NULL
    if (!is.null(rows)) rownames(rows) <- NULL
    list(file = r$file, source = r$source %||% r$file, source_page = r$source_page, status = r$status,
         message = r$message, values = vals, text = text, rows = rows)
  })
}

.document_fit <- function(info) {
  cb <- info$codebook
  if (.result_kind(info) == "text") return(invisible())
  .inform(c("i" = paste0("A document has one section per page, with the page's values",
                         if (!is.null(.records_path(cb))) " and its rows as a table" else "", ".")))
}

.document_title <- function(info) {
  p <- info$pages
  sources <- unique(p$source)
  src <- paste0(paste(utils::head(sources, 3), collapse = ", "), if (length(sources) > 3) paste0(" and ", length(sources) - 3, " more") else "")
  c(title = paste0(src, ": results"),
    sub = paste0("Read by ", info$settings$model %||% "the model", " with amanuensis ", utils::packageVersion("amanuensis"),
                 "; written on ", format(Sys.Date()), ". Values are as the model returned them; ",
                 "empty means illegible or not on the page."))
}

# "=== book_p001.jpg (book.pdf p. 1) ===", or "=== photo.jpg ===" for an
# image that is its own source.
.section_heading <- function(s) {
  where <- if (!identical(s$source, s$file)) paste0(" (", s$source, " p. ", s$source_page, ")") else ""
  paste0("=== ", s$file, where, " ===")
}

# Values shown in documents: empty for NA, yes/no for logical, numbers
# without scientific notation; tabs and line breaks in table cells become
# spaces and " / " so the table stays a table.
.cell_text <- function(x) .docx_cell_text(x)

.flat_cells <- function(x) {
  x <- .cell_text(x)
  x <- gsub("\r?\n", " / ", x)
  gsub("[[:cntrl:]]", " ", x)
}

.text_width <- function(x) {
  w <- utf8::utf8_width(enc2utf8(x), encode = FALSE, utf8 = TRUE)
  w[is.na(w)] <- nchar(x[is.na(w)])
  w
}

.pad <- function(x, width, right = FALSE) {
  gap <- strrep(" ", pmax(0, width - .text_width(x)))
  if (right) paste0(gap, x) else paste0(x, gap)
}

# A plain-text table: columns padded to their widest cell, numbers on the
# right, a line of dashes under the header.
.txt_table <- function(df) {
  if (!ncol(df)) return(character())
  cells <- lapply(df, .flat_cells)
  heads <- names(df)
  widths <- vapply(seq_along(cells), function(j) max(.text_width(c(heads[j], cells[[j]]))), numeric(1))
  num <- vapply(df, is.numeric, logical(1))
  line <- function(v) sub(" +$", "", paste(vapply(seq_along(v), function(j) .pad(v[j], widths[j], num[j]), ""), collapse = "  "))
  out <- c(line(heads), paste(strrep("-", widths), collapse = "  "))
  if (nrow(df)) out <- c(out, vapply(seq_len(nrow(df)), function(i) line(vapply(cells, `[`, "", i)), ""))
  out
}

.md_escape <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("([*_`|<>\\[\\]])", "\\\\\\1", x, perl = TRUE)
  # characters that start a heading, quote or list at the start of a line
  x <- gsub("(^|\n)([#>+-])", "\\1\\\\\\2", x, perl = TRUE)
  gsub("(^|\n)([0-9]+)\\.", "\\1\\2\\\\.", x, perl = TRUE)
}

.md_table <- function(df) {
  if (!ncol(df)) return(character())
  cells <- lapply(df, function(v) {
    v <- .md_escape(.cell_text(v))
    gsub("\r?\n", "<br>", v)
  })
  num <- vapply(df, is.numeric, logical(1))
  row <- function(v) paste0("| ", paste(v, collapse = " | "), " |")
  out <- c(row(.md_escape(names(df))), paste0("|", paste(ifelse(num, "---:", "---"), collapse = "|"), "|"))
  if (nrow(df)) out <- c(out, vapply(seq_len(nrow(df)), function(i) row(vapply(cells, `[`, "", i)), ""))
  out
}

.value_lines <- function(vals, md = FALSE) {
  if (!length(vals)) return(character())
  nms <- names(vals)
  v <- vapply(vals, function(z) .cell_text(z)[1], "")
  if (md) return(paste0("- **", .md_escape(nms), ":** ", gsub("\r?\n", "  \n  ", .md_escape(v))))
  w <- max(.text_width(nms)) + 1
  paste0(.pad(paste0(nms, ":"), w), " ", gsub("\r?\n", " / ", v))
}

.output_text <- function(res, info, f, table, include, md, call) {
  head <- .document_title(info)
  lines <- if (md) c(paste0("# ", .md_escape(head[["title"]])), "", .md_escape(head[["sub"]]), "") else
    c(head[["title"]], head[["sub"]], "")
  if (table != "records") {
    df <- .output_table(res, info, table, include, call = call)
    lines <- c(lines, if (md) c(paste0("## ", if (table == "pages") "Pages" else "Codebook"), "", .md_table(df)) else
      c(paste0("=== ", if (table == "pages") "Pages" else "Codebook", " ==="), "", .txt_table(df)), "")
    .write_text(lines, f)
    .wrote(f, .rows_label(nrow(df)))
    return(invisible())
  }
  tab <- .output_records(res, info, include, call = call)
  recs <- .output_pages(res, info, include, tab)
  sections <- .page_sections(tab, recs, info$codebook)
  .document_fit(info)
  for (s in sections) {
    if (md) {
      lines <- c(lines, paste0("## ", .md_escape(s$file)), "")
      if (!identical(s$source, s$file)) lines <- c(lines, paste0("*", .md_escape(s$source), ", page ", s$source_page, "*"), "")
    } else {
      lines <- c(lines, .section_heading(s), "")
    }
    if (identical(s$status, "check")) {
      note <- paste0("To check: ", s$message %||% "the reply did not match the codebook.")
      lines <- c(lines, if (md) paste0("**", .md_escape(note), "**") else note, "")
    }
    if (length(s$values)) lines <- c(lines, .value_lines(s$values, md), "")
    if (!is.null(s$text) || .result_kind(info) == "text") {
      txt <- if (is.null(s$text) || is.na(s$text)) "(no text)" else s$text
      if (md) txt <- gsub("\n", "  \n", .md_escape(txt), fixed = TRUE)
      lines <- c(lines, txt, "")
    }
    if (!is.null(s$rows)) {
      lines <- c(lines, if (nrow(s$rows)) { if (md) .md_table(s$rows) else .txt_table(s$rows) } else "(no rows on this page)", "")
    }
  }
  .write_text(lines, f)
  .wrote(f, .pages_label(length(sections)))
}

.output_docx <- function(res, info, f, table, include, call) {
  head <- .document_title(info)
  blocks <- list(list(type = "title", text = head[["title"]]), list(type = "para", text = head[["sub"]]))
  if (table != "records") {
    df <- .output_table(res, info, table, include, call = call)
    blocks <- c(blocks, list(list(type = "heading", text = if (table == "pages") "Pages" else "Codebook"),
                             list(type = "table", table = df)))
    .write_docx(blocks, f, call = call)
    .wrote(f, .rows_label(nrow(df)))
    return(invisible())
  }
  tab <- .output_records(res, info, include, call = call)
  recs <- .output_pages(res, info, include, tab)
  sections <- .page_sections(tab, recs, info$codebook)
  .document_fit(info)
  dropped <- 0L
  for (s in sections) {
    where <- if (!identical(s$source, s$file)) paste0(" (", s$source, " p. ", s$source_page, ")") else ""
    blocks[[length(blocks) + 1]] <- list(type = "heading", text = paste0(s$file, where))
    if (identical(s$status, "check")) {
      blocks[[length(blocks) + 1]] <- list(type = "para", text = paste0("To check: ", s$message %||% "the reply did not match the codebook."))
    }
    if (length(s$values)) {
      v <- vapply(s$values, function(z) .cell_text(z)[1], "")
      blocks[[length(blocks) + 1]] <- list(type = "para", text = paste0(names(s$values), ": ", v, collapse = "\n"))
      dropped <- dropped + .docx_control_chars(v)
    }
    if (!is.null(s$text) || .result_kind(info) == "text") {
      txt <- if (is.null(s$text) || is.na(s$text)) "(no text)" else s$text
      blocks[[length(blocks) + 1]] <- list(type = "para", text = txt)
      dropped <- dropped + .docx_control_chars(txt)
    }
    if (!is.null(s$rows)) {
      blocks[[length(blocks) + 1]] <- if (nrow(s$rows)) list(type = "table", table = s$rows) else
        list(type = "para", text = "(no rows on this page)")
      dropped <- dropped + sum(vapply(s$rows, function(col) .docx_control_chars(.chr_values(col)), integer(1)))
    }
  }
  .write_docx(blocks, f, call = call)
  .wrote(f, .pages_label(length(sections)))
  if (dropped) {
    .inform(c("i" = "{dropped} invisible control character{?s} {?was/were} left out of the Word file (Word refuses them); the .json output keeps them."))
  }
}
