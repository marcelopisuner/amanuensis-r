# benchmark(): the gold standard - your own transcription of a few
# pages, made before you look at the model's output, which score() compares
# the model with. One call makes it and, run again, reads it back.
#
# The draw. The pages are a seeded permutation of the page file names
# (Unicode-normalised, natural order with ASCII-only case folding and a byte
# sort, so the order does not depend on the computer's language settings),
# and the benchmark is its first n pages. A later call with a larger n
# therefore keeps every page already transcribed and adds the next ones of
# the same permutation: the draw is never silently redone. The generator is
# set with explicit kinds (Mersenne-Twister, rejection sampling) inside a
# saved and restored state, so the same seed gives the same pages on any
# machine and the user's own random numbers are left as they were. Without a
# seed one is made from the file names (sha256), so the draw is still
# reproducible; a seed taken from the source itself (its year, its shelfmark
# number) is better, since a reader can check it, hence the advice printed
# with it.
#
# Reading. Every cell is read as text and converted to the codebook's type
# here, never guessed by readxl. A value that cannot be converted stops the
# read with its sheet and cell: turned into an empty value it would silently
# count for or against the model. Excel's damage to text (1-2 turned into a
# date, long numbers into 1.2E+15, 007 into 7) is warned about, because it
# cannot be undone here. A cell Excel stored as a date in a number or yes/no
# column stops the read: its serial number (46024 for 3/12) would pass for
# a valid number. Only pages marked done are used: a half-typed page would
# count every value not yet typed as a model error. So an explicit "no"
# always keeps a page out; only when the done column was left empty for
# every page are the pages with values used (said aloud), since then
# nothing says which are finished.
#
# Prefilling from the model's output is allowed (it saves typing on dense
# tables) but recorded, because people correcting a machine's output accept
# some of its mistakes: accuracy measured against it is optimistic, and
# score() says so.

.gold_meta_sheets <- c("pages", "about", "codebook", "how_to")
.gold_xlsx_exts <- c("xlsx", "xlsm", "xls")
.gold_text_exts <- c("txt", "md", "docx")
.gold_all_exts <- c(.gold_xlsx_exts, "csv", "tsv", "dta", "rds", "json", .gold_text_exts)
.gold_about_items <- c("package", "made", "seed", "n", "method", "drawn", "rows", "prefilled",
                       "codebook_source", "free_text", "images")
# Columns a results table or an output() file carries besides the variables:
# ignored without comment when a benchmark file has them.
.gold_quiet_cols <- c("source", "source_page", "page_status", "notes", "note", "comment", "comments")
# How a yes/no cell may be written (after lower-casing and folding accents,
# so si, s\u00ed and s\u00ec all count). Shared with score(), so a benchmark
# given as a data frame is read the same way as a file. S and N, as Spanish
# and Catalan registers write them, are both accepted: taking N alone would
# let "N" pass and stop at "S" in the same column.
.gold_yes <- c("yes", "y", "si", "s", "s\u00ed", "s\u00ec", "true", "t", "1", "x", "oui", "ja", "vrai",
               "verdadero", "cert")
.gold_no <- c("no", "n", "false", "f", "0", "non", "nein", "falso", "fals", "faux")

#' Make or read a benchmark: your own transcription of a few pages
#'
#' To know how accurately the model reads your source, transcribe a few pages
#' yourself and compare them with the model's output using [score()]. This
#' function does both halves of that job with the same line of code. The
#' first time, it draws the pages at random and writes a file for you to fill
#' in (or opens an editor in your browser). Once you have filled it in, run
#' the same line again: it reads your transcription and returns it, ready for
#' [score()].
#'
#' @section Making the benchmark:
#' When `file` does not exist yet, `n` pages are drawn at random from
#' `images`, and a file is written with one line ready for each page:
#' \itemize{
#'   \item For a table codebook, an Excel file with a sheet `benchmark`
#'     (columns `file`, `row` and your variables), a sheet `pages` where you
#'     mark each finished page `done = yes`, and sheets `about`, `codebook`
#'     and `how_to` that record how the pages were drawn, the codebook, and
#'     what to do. Every column is formatted as Text, so Excel keeps values
#'     such as `007`, `1-2` or `3/12` as you type them, and category and
#'     yes/no columns have drop-down lists.
#'   \item For free text (`codebook(free_text = TRUE)`), a `.txt`, `.md` or
#'     `.docx` file with a line `=== <file name> ===` for each page, under
#'     which you type the page's text. Leave a page empty until you have
#'     transcribed it; for a page with no writing, type `[no text]`.
#' }
#' In an interactive session with the shiny package installed, an editor
#' opens in your browser instead: the page image on the left (with a zoom),
#' a form built from your codebook on the right. It saves the same file
#' after every page, so you can stop and come back later.
#'
#' Type exactly what is written, following the same rules you give the model
#' (spelling, abbreviations and accents as written; a doubtful reading ending
#' in `?`; an empty cell for what is illegible or absent). Do it before you
#' look at the model's output for those pages.
#'
#' @section The draw:
#' The pages are drawn as a random ordering of all the page file names,
#' fixed by `seed`, and the first `n` are taken. Running again with a larger
#' `n` keeps the pages you have already transcribed and adds the next ones.
#' Choose the seed from the source itself, for example its year, and report
#' it: anyone can then check which pages were drawn. Without a seed, one is
#' made from the file names. Your own random numbers in R are not affected.
#'
#' Two pages are enough to try the method; for a figure to publish, use
#' `n = 10` or more. On dense tables, `rows = 20` asks you to type only the
#' first 20 rows of each page, and [score()] then compares only those.
#'
#' @section Reading it back:
#' Only pages marked done are used (in a `.txt`, `.md` or `.docx` file:
#' pages with text, unless marked "not done"). If the done column was left
#' empty for every page, the pages with values are used, and said so; a page
#' marked `no` is never used. Values are checked against the codebook: a
#' number that is not a number, or a yes/no that is neither, stops with the
#' sheet and cell to correct, and so does a date Excel made in a number or
#' yes/no column (from an entry such as `3/12`). Page variables are read from
#' the first line of each page. The function also warns about text values
#' Excel may have damaged: dates made out of entries such as `1-2`, long
#' numbers shown as `1.2E+15`, and lost leading zeros.
#'
#' Besides its own template, `benchmark()` reads a transcription you made
#' another way: an `.xlsx`, `.csv`, `.dta` (needs haven) or `.rds` file with
#' the columns `file`, `row` and your variables (every page with a value
#' counts as done), a `.json` file as [output()] writes it, or a text file
#' with `=== <file name> ===` lines. Give `codebook` for these.
#'
#' @param file The benchmark file. If it does not exist, it is made (`.xlsx`;
#'   for free text also `.txt`, `.md` or `.docx`); if it exists, it is read.
#' @param images The page images, as for [transcribe()]: the result of
#'   [images()], a folder of page images, or image files. Needed to make the
#'   benchmark; when reading, used to check the file names.
#' @param codebook The codebook, from [codebook()] (or a codebook file).
#'   Needed to make the benchmark. When reading the package's own Excel
#'   template it can be left out: the file keeps a copy.
#' @param n How many pages to draw. Default 2. Given again with a larger
#'   number, pages are added to an existing benchmark.
#' @param seed A whole number that fixes the draw, ideally taken from the
#'   source (its year, say). Default: one made from the file names.
#' @param rows Type only the first `rows` rows of each page (for dense
#'   tables). Default: all rows.
#' @param app `TRUE` to fill in the benchmark in an editor in your browser
#'   (needs the shiny package and an interactive session), `FALSE` to get a
#'   file to fill in with Excel or a text editor. Default: the editor when
#'   possible. Given as `TRUE` for an existing file, the editor opens to
#'   continue it.
#' @param prefill A result of [transcribe()] to start from: the drawn
#'   pages are filled with the model's output, for you to correct. It saves
#'   typing, but accuracy measured this way is optimistic (mistakes you do
#'   not notice count as right), and this is recorded.
#' @return When reading: the benchmark, a data frame (class
#'   `amanuensis_gold`) with the columns `file`, `row` and your variables,
#'   one line per row of each page done, to pass to [score()]. When making
#'   the file: `NULL`, invisibly, after telling you what to fill in (or the
#'   benchmark, if you finished it in the editor).
#' @seealso [score()] to compare the model with it; `transcribe(only =
#'   gold)` to extract just the benchmark pages.
#' @export
#' @examples
#' ex <- example_project()
#' pages <- images(ex$scans, save_to = file.path(ex$folder, "pages"))
#' cb <- codebook(ex$codebook)
#'
#' # draw 2 pages and write a file to fill in
#' benchmark(file.path(ex$folder, "my-benchmark.xlsx"), images = pages,
#'           codebook = cb, n = 2, seed = 1930, app = FALSE)
#'
#' # the example project comes with a filled-in benchmark: read it
#' gold <- benchmark(ex$gold)
#' gold
#'
#' \dontrun{
#' # fill it in with the editor in your browser (needs shiny)
#' benchmark("gold.xlsx", images = pages, codebook = cb, seed = 1930, app = TRUE)
#' }
benchmark <- function(file, images = NULL, codebook = NULL, n = 2, seed = NULL, rows = NULL,
                      app = interactive(), prefill = NULL) {
  if (missing(file) || is.null(file)) {
    .abort(c("Say which file holds the benchmark.",
             "i" = "For example {.code benchmark(\"gold.xlsx\", images = pages, codebook = cb)}: the first time it writes the file for you to fill in; run again, the same line reads it."),
           class = "amanuensis_error_input")
  }
  .check_string(file, arg = "file")
  .gold_check_count(n, "n")
  if (!is.null(rows)) .gold_check_count(rows, "rows")
  if (!is.null(seed)) .gold_check_seed(seed)
  if (!is.logical(app) || length(app) != 1 || is.na(app)) {
    .abort("{.arg app} must be {.code TRUE} or {.code FALSE}.", class = "amanuensis_error_input")
  }
  ext <- .ext(file)
  if (!ext %in% .gold_all_exts) {
    .abort(c("A benchmark must be an Excel file (.xlsx), or a .csv, .dta, .rds or .json file; for free text also .txt, .md or .docx.",
             "x" = "{.file {basename(file)}} has the extension {.val {ext}}."), class = "amanuensis_error_input")
  }
  pages <- if (is.null(images)) NULL else .as_images(images)
  if (!is.null(prefill)) prefill <- .as_result(prefill)
  cb <- if (is.null(codebook)) NULL else .gold_as_codebook(codebook)
  app_given <- !missing(app)

  if (file.exists(file)) {
    if (!missing(n) && .gold_extend(file, pages, cb, n, seed, prefill)) {
      if (.gold_use_app(app, app_given)) return(.gold_after_app(file, cb, pages))
      return(invisible(NULL))
    }
    if (app_given && isTRUE(app) && ext %in% c("xlsx", .gold_text_exts) && .gold_use_app(app, TRUE)) {
      return(.gold_after_app(file, cb, pages))
    }
    return(.gold_read(file, cb, pages, rows))
  }
  .gold_create(file, pages, cb, n, seed, rows, prefill, use_app = .gold_use_app(app, app_given))
}

# ------- arguments ------------------------------------------------------------

.gold_check_count <- function(x, arg, call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 1 || x != round(x)) {
    .abort("{.arg {arg}} must be a whole number of at least 1.", call = call, class = "amanuensis_error_input")
  }
  invisible(x)
}

.gold_check_seed <- function(seed, call = rlang::caller_env()) {
  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed) || seed != round(seed) ||
      abs(seed) > .Machine$integer.max) {
    .abort(c("{.arg seed} must be a whole number, such as the year of the source ({.code seed = 1930}).",
             "x" = "You gave {.val {format(seed)}}."), call = call, class = "amanuensis_error_input")
  }
  invisible(seed)
}

.gold_as_codebook <- function(x, call = rlang::caller_env()) {
  if (inherits(x, "amanuensis_codebook")) return(x)
  if (is.data.frame(x) || .is_string(x)) return(codebook(x))
  .abort("{.arg codebook} must come from {.fn codebook}, or be a codebook file.", call = call,
         class = "amanuensis_error_input")
}

# The editor opens only in an interactive session. Asked for explicitly, a
# missing shiny is offered for installation; by default its absence just
# means the Excel template.
.gold_use_app <- function(app, app_given) {
  if (!isTRUE(app)) return(FALSE)
  if (!interactive()) {
    if (app_given) .inform(c("i" = "The benchmark editor needs an interactive R session; writing a file to fill in instead."))
    return(FALSE)
  }
  if (app_given) {
    rlang::check_installed("shiny", "for the benchmark editor")
    return(TRUE)
  }
  rlang::is_installed("shiny")
}

# ------- small helpers ----------------------------------------------------------

.gold_n <- function(n, word, plural = paste0(word, "s")) paste(.fmt_int(n), if (n == 1) word else plural)

.gold_list <- function(x, max = 5) {
  x <- as.character(x)
  if (length(x) > max) paste0(paste(x[seq_len(max)], collapse = ", "), " and ", length(x) - max, " more") else
    paste(x, collapse = ", ")
}

# Messages made of composed sentences: each line is passed as a value, so
# braces in file names or cell values are never read as cli markup. `fun`
# is .inform, .warn or .abort; `...` goes to it (call, class).
.gold_tell <- function(lines, fun = .inform, ...) {
  types <- names(lines)
  if (is.null(types)) types <- rep("", length(lines))
  fmt <- stats::setNames(paste0("{lines[[", seq_along(lines), "]]}"), types)
  fun(fmt, ..., .envir = environment())
}

.gold_program <- function(ext) {
  if (ext %in% c(.gold_xlsx_exts, "csv", "tsv")) "Excel" else if (ext == "docx") "Word" else "the file"
}

.gold_columns <- function(cb) {
  v <- cb$variables
  c("file", if (any(v$level == "row")) "row", v$variable[v$level == "page"], v$variable[v$level == "row"])
}

.gold_parse_yes_no <- function(x, extra_yes = character(), extra_no = character()) {
  key <- tolower(.fold_accents(trimws(.utf8_marked(x))))
  out <- rep(NA, length(x))
  out[!is.na(key) & key %in% c(.gold_yes, extra_yes)] <- TRUE
  out[!is.na(key) & key %in% c(.gold_no, extra_no)] <- FALSE
  out
}

.gold_parse_done <- function(x) {
  .gold_parse_yes_no(x, extra_yes = c("done", "checked", "ok", "hecho", "fet", "terminado", "acabat"),
                     extra_no = c("not done", "to check", "pending", "pendiente", "pendent"))
}

# Excel column letters: 1 -> A, 27 -> AA.
.excel_col <- function(j) {
  vapply(j, function(n) {
    s <- ""
    while (n > 0) {
      r <- (n - 1) %% 26
      s <- paste0(LETTERS[r + 1], s)
      n <- (n - 1) %/% 26
    }
    s
  }, character(1))
}

# Natural order (p2 before p10) that gives the same answer on every
# computer: only ASCII letters are case-folded and ties are broken by a byte
# sort, where .natural_order() lets the locale lower-case accented capitals.
.gold_order <- function(files) {
  files <- .nfc(files)
  key <- chartr(paste(LETTERS, collapse = ""), paste(letters, collapse = ""), files)
  key <- gsub("([0-9]+)", "\001\\1\001", key)
  parts <- strsplit(key, "\001", fixed = TRUE)
  key <- vapply(parts, function(p) {
    num <- grepl("^[0-9]+$", p)
    p[num] <- formatC(as.numeric(p[num]), width = 15, format = "d", flag = "0")
    paste(p, collapse = "")
  }, character(1))
  order(key, files, method = "radix")
}

.gold_pkg_label <- function() {
  v <- tryCatch(as.character(utils::packageVersion("amanuensis")), error = function(e) "")
  trimws(paste("amanuensis", v))
}

# Text of a typed value for a template cell: yes/no for booleans, numbers
# without scientific notation, "" for empty.
.gold_cell_text <- function(x) {
  if (is.logical(x)) return(ifelse(is.na(x), "", ifelse(x, "yes", "no")))
  out <- .chr_values(x)
  out[is.na(out)] <- ""
  out
}

# Columns of a file matched to the names wanted: exactly, then ignoring
# case and surrounding spaces, then as Stata and Excel users rename them
# (accents dropped, other characters to "_", cut to 32 characters).
.gold_match_columns <- function(have, want) {
  have <- as.character(have)
  have[is.na(have)] <- ""
  norm1 <- function(x) tolower(trimws(.nfc(x)))
  norm2 <- function(x) {
    x <- tolower(.fold_accents(trimws(x)))
    x <- gsub("[^a-z0-9]+", "_", x)
    gsub("^_+|_+$", "", x)
  }
  passes <- list(.nfc, norm1, norm2, function(x) substr(norm2(x), 1, 32))
  out <- stats::setNames(rep(NA_integer_, length(want)), want)
  taken <- logical(length(have))
  for (pass in passes) {
    h <- pass(have)
    w <- pass(want)
    for (k in which(is.na(out))) {
      hit <- which(!taken & nzchar(h) & h == w[k])
      if (length(hit)) {
        out[k] <- hit[1]
        taken[hit[1]] <- TRUE
      }
    }
  }
  out
}

# Typed file names mapped to the page images' own spelling (case, Unicode
# form, a missing or different extension); names that match no image are
# returned apart.
.gold_canonical <- function(files, image_files = NULL) {
  files <- trimws(.nfc(files))
  if (is.null(image_files) || !length(files)) return(list(files = files, unknown = character()))
  m <- match(.page_key(files), .page_key(image_files))
  m2 <- match(.page_key(.stem(files)), .page_key(.stem(image_files)))
  m[is.na(m)] <- m2[is.na(m)]
  out <- files
  out[!is.na(m)] <- image_files[m[!is.na(m)]]
  list(files = out, unknown = unique(files[is.na(m) & nzchar(files)]))
}

# ------- the draw -----------------------------------------------------------------

# A seed made from the file names alone: sha256 of the sorted, normalised
# names, cut to 28 bits so it is a valid integer seed. Sorted by bytes, so
# no locale or file-system order enters.
.gold_seed_from_names <- function(files) {
  files <- .nfc(files)
  files <- files[order(files, method = "radix")]
  hex <- as.character(openssl::sha256(charToRaw(enc2utf8(paste(files, collapse = "\n")))))
  strtoi(substr(hex, 1, 7), 16L)
}

# Evaluate `expr` with the generator set to `seed`, then put back exactly
# what was there before: the user's .Random.seed (or its absence) and their
# generator kinds.
.gold_with_seed <- function(seed, expr) {
  env <- globalenv()
  had <- exists(".Random.seed", envir = env, inherits = FALSE)
  old_seed <- if (had) get(".Random.seed", envir = env, inherits = FALSE) else NULL
  old_kind <- RNGkind()
  on.exit({
    suppressWarnings(do.call(RNGkind, as.list(old_kind)))
    if (had) {
      assign(".Random.seed", old_seed, envir = env)
    } else if (exists(".Random.seed", envir = env, inherits = FALSE)) {
      rm(".Random.seed", envir = env)
    }
  }, add = TRUE)
  suppressWarnings(set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
                            sample.kind = "Rejection"))
  force(expr)
}

# The whole permutation; a draw of n is its first n pages.
.gold_permutation <- function(files, seed) {
  files <- .nfc(files)
  base <- files[.gold_order(files)]
  perm <- .gold_with_seed(seed, sample.int(length(base)))
  base[perm]
}

.gold_draw <- function(files, n, seed) {
  full <- .gold_permutation(files, seed)
  full[seq_len(min(n, length(full)))]
}

# Pages to add to an earlier draw so that it holds n pages. With the same
# images this is exactly the next pages of the same permutation; if images
# were added or removed since, the earlier pages are kept and the new ones
# are the first of the current permutation not drawn yet (`same` = FALSE).
.gold_extend_draw <- function(files, drawn, n, seed) {
  full <- .gold_permutation(files, seed)
  k <- length(drawn)
  same <- k <= length(full) && identical(.page_key(full[seq_len(k)]), .page_key(drawn))
  rest <- full[!.page_key(full) %in% .page_key(drawn)]
  list(add = rest[seq_len(min(max(n - k, 0), length(rest)))], same = same)
}

.gold_method <- function(derived) {
  paste0("seeded random order of the page file names (natural order, R's Mersenne-Twister generator, ",
         "sample.int with rejection sampling); the first n pages taken",
         if (derived) "; seed made from the file names" else "")
}

# What the draw covers and what it leaves untested, said once, plainly.
.gold_describe_draw <- function(drawn, total, seed, derived, cb, rows, units) {
  n <- length(drawn)
  vars <- cb$variables
  out <- c("v" = paste0("Drew ", n, " of ", .gold_n(total, "page"), " for the benchmark (seed ",
                        format(seed, scientific = FALSE), "): ", .gold_list(drawn, 6), "."))
  if (derived) {
    out <- c(out, "i" = paste0("The seed was made from the file names, so the same pages always give the same draw. ",
                               "For a draw others can check, choose the seed yourself: use a number from the source ",
                               "itself, e.g. its year (seed = 1930)."))
  }
  if (n >= total) {
    out <- c(out, "i" = "The benchmark covers every page.")
  } else {
    cats <- vars[vars$type == "category", , drop = FALSE]
    gaps <- character()
    if (nrow(cats)) {
      shown <- vapply(seq_len(min(3, nrow(cats))), function(i) {
        paste0(cats$variable[i], ": ", .gold_list(cats$values[[i]], 4))
      }, "")
      gaps <- c(gaps, paste0("category values that do not occur on ", if (n == 1) "that page" else "those pages",
                             " go unchecked (", paste(shown, collapse = "; "), ")"))
    }
    pv <- vars$variable[vars$level == "page"]
    if (length(pv) && any(vars$level == "row")) {
      gaps <- c(gaps, paste0("page variables (", .gold_list(pv, 4), ") are compared on ", .gold_n(n, "value"), " each"))
    }
    line <- paste0("A benchmark of ", .gold_n(n, "page"), " measures accuracy on what is on ",
                   if (n == 1) "that page" else "those pages", " only",
                   if (length(gaps)) paste0(": ", paste(gaps, collapse = "; ")) else "", ".")
    if (n < 10) line <- paste(line, "For a figure to publish, use n = 10 or more.")
    out <- c(out, "!" = line)
  }
  if (any(vars$level == "row")) {
    typed <- sum(vapply(units, function(u) nrow(u$rows), integer(1)))
    if (!is.null(rows)) {
      out <- c(out, "i" = paste0("Only the first ", rows, " rows of each page need typing (at most ",
                                 .fmt_int(rows * n), " rows in all); this is recorded in the file and score() compares only those."))
    } else if (length(units)) {
      out <- c(out, "i" = paste0("About ", .gold_n(typed, "row"), " to check, from the model's output."))
    } else {
      out <- c(out, "i" = paste0("Every row of each page is to be typed; for dense pages, rows = 20 asks for only the ",
                                 "first 20 rows of each page."))
    }
  }
  .gold_tell(out)
}

# ------- making the benchmark -----------------------------------------------------

.gold_create <- function(file, pages, cb, n, seed, rows, prefill, use_app, call = rlang::caller_env()) {
  ext <- .ext(file)
  if (is.null(pages) || (is.null(cb) && is.null(prefill))) {
    .abort(c("{.file {basename(file)}} does not exist yet.",
             "i" = "To make it, give the page images and the codebook: {.code benchmark(\"{basename(file)}\", images = pages, codebook = cb)}.",
             "i" = "To read an existing benchmark, check the spelling. Relative paths start from your working folder, {.path {(.show_path(getwd()))}}."),
           call = call, class = "amanuensis_error_input")
  }
  if (is.null(cb)) {
    cb <- .result_info(prefill)$codebook
    .inform(c("i" = "Using the codebook of the extraction given in {.arg prefill}."))
  }
  .gold_check_template_ext(ext, cb, file, call)
  folder <- attr(pages, "folder") %||% .show_path(dirname(pages$image[1]))
  total <- nrow(pages)
  derived <- is.null(seed)
  if (derived) seed <- .gold_seed_from_names(pages$file)
  drawn <- .gold_draw(pages$file, n, seed)
  if (n > total) .inform(c("i" = "There are only {total} page image{?s}; the benchmark takes all of them."))
  drawn <- .gold_canonical(drawn, pages$file)$files
  files <- drawn[.gold_order(drawn)]

  units <- if (is.null(prefill)) list() else .gold_prefill_units(prefill, files, cb, rows)
  prefilled <- length(units) > 0
  about <- .gold_about(seed, derived, drawn, rows, prefilled, cb, folder)
  book <- list(file = file, ext = ext, cb = cb, about = about,
               lines = .gold_lines_chr(units, cb, files),
               pages = data.frame(file = files, done = .gold_initial_done(ext, files, units),
                                  notes = NA_character_, stringsAsFactors = FALSE))
  .gold_describe_draw(drawn, total, seed, derived, cb, rows, units)
  if (!is.null(prefill)) .gold_prefill_messages(units, files)
  .gold_book_write(book)
  if (use_app) return(.gold_after_app(file, cb, pages))
  .gold_fill_message(file, files, folder, prefilled)
  invisible(NULL)
}

.gold_check_template_ext <- function(ext, cb, file, call) {
  name <- basename(file)
  stem <- .stem(file)
  if (ext == "xlsx") return(invisible())
  if (ext %in% .gold_text_exts) {
    if (cb$kind == "text") return(invisible())
    .abort(c("A {.val .{ext}} benchmark holds only the text of each page, but the codebook asks for a table.",
             "i" = "Use an Excel file: {.code benchmark(\"{stem}.xlsx\", ...)}."), call = call,
           class = "amanuensis_error_input")
  }
  .abort(c("A new benchmark is written as an Excel file{if (cb$kind == 'text') ' (or .txt, .md, .docx for free text)' else ''}.",
           "x" = "{.file {name}} does not exist, and a .{ext} file cannot be made into a template to fill in.",
           "i" = "Use {.code benchmark(\"{stem}.xlsx\", ...)}; files of other kinds are read once they exist."),
         call = call, class = "amanuensis_error_input")
}

# Pages start "not done". In a text file an unmarked page with text counts
# as done, so only prefilled pages (whose text is the model's) are marked.
.gold_initial_done <- function(ext, files, units) {
  if (ext %in% .gold_text_exts) {
    ifelse(.page_key(files) %in% .page_key(names(units)), "no", NA_character_)
  } else {
    rep("no", length(files))
  }
}

.gold_about <- function(seed, derived, drawn, rows, prefilled, cb, folder) {
  vals <- list(package = .gold_pkg_label(), made = .now(), seed = format(seed, scientific = FALSE),
               n = as.character(length(drawn)), method = .gold_method(derived),
               drawn = paste(drawn, collapse = "; "), rows = if (is.null(rows)) NA_character_ else as.character(rows),
               prefilled = if (prefilled) "yes" else "no", codebook_source = as.character(cb$source %||% NA),
               # the codebook sheet lists the page's other text as a plain
               # variable; this says it is the other text, scored apart
               free_text = if (isTRUE(cb$free_text)) "yes" else "no",
               images = as.character(folder %||% NA))
  vals[.gold_about_items]
}

.gold_fill_message <- function(file, files, folder, prefilled) {
  ext <- .ext(file)
  what <- if (ext %in% .gold_text_exts) {
    "Open it and type the text of each page under its === line, exactly as written."
  } else {
    paste0("Open it in Excel and type what each page says: one line per row of the page's table ",
           "(the first line of each page is there already). Then write yes in the done column of the pages sheet.")
  }
  if (prefilled) {
    what <- if (ext %in% .gold_text_exts) {
      "Open it and correct the model's text of each page against the image; then change \"not done\" to \"done\" on its === line."
    } else {
      "Open it in Excel and correct the model's values against each page image; then write yes in the done column of the pages sheet."
    }
  }
  .gold_tell(c("v" = paste0("Wrote ", .show_path(file), " with ", .gold_n(length(files), "page"), " to transcribe."),
               "i" = what,
               "i" = paste0("The page images are in ", folder, "."),
               ">" = paste0("Then save, close ", .gold_program(ext), ", and run the same line again to read it.")))
}

# The model's output for the drawn pages, as template text. Pages the model
# has not read (or failed) are left empty.
.gold_prefill_units <- function(prefill, files, cb, rows = NULL) {
  units <- .result_units(prefill)
  keys <- .page_key(names(units))
  vars <- cb$variables
  pv <- vars$variable[vars$level == "page"]
  rv <- vars$variable[vars$level == "row"]
  out <- list()
  for (f in files) {
    k <- match(.page_key(f), keys)
    if (is.na(k) || !isTRUE(units[[k]]$status %in% c("ok", "check"))) next
    u <- units[[k]]
    page <- vapply(pv, function(v) if (v %in% names(u$page)) .gold_cell_text(u$page[[v]][1]) else "", "")
    r <- u$rows
    rws <- as.data.frame(lapply(stats::setNames(rv, rv), function(v) {
      if (v %in% names(r)) .gold_cell_text(r[[v]]) else rep("", nrow(r))
    }), stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
    if (!length(rv)) rws <- data.frame()
    if (!is.null(rows) && nrow(rws) > rows) rws <- rws[seq_len(rows), , drop = FALSE]
    out[[f]] <- list(page = page, rows = rws)
  }
  out
}

.gold_prefill_messages <- function(units, files) {
  missing <- files[!.page_key(files) %in% .page_key(names(units))]
  if (length(units)) {
    .gold_tell(c(paste0("The benchmark starts from the model's own output for ", .gold_n(length(units), "page"), "."),
                 "!" = "Correct every value against the page image before marking a page done.",
                 "i" = "Accuracy measured against corrected model output is optimistic: mistakes you do not notice count as right. This is recorded in the file."),
               fun = .warn)
  }
  if (length(missing)) {
    .gold_tell(c("i" = paste0("The model has no output for ", .gold_list(missing), "; ",
                              if (length(missing) == 1) "it is" else "they are", " left empty to type from scratch.")))
  }
}

# ------- the book: the file's content as text -----------------------------------
#
# The template, the extension of a draw and the editor all work on the same
# plain description of a benchmark file, written by one function:
#   file, ext, cb   where it goes and the codebook it follows
#   lines           data.frame of text in the sheet layout (file, row,
#                   variables; or file, text for a text file)
#   pages           data.frame(file, done "yes"/"no"/NA, notes)
#   about           named list of the "about" items

.gold_book_write <- function(book) {
  ext <- book$ext
  if (ext %in% c("txt", "md")) {
    .with_write_errors(book$file, .write_text(.gold_text_lines(book), book$file))
  } else if (ext == "docx") {
    .gold_write_docx(.gold_text_lines(book), book$file)
  } else {
    .gold_write_xlsx(book)
  }
  invisible(book$file)
}

# Read a benchmark file into a book, as text, converting nothing (for the
# editor and for adding pages). The codebook kept in the file comes first, so
# the file stays consistent; `fallback` serves files that keep none.
.gold_load <- function(file, fallback = NULL, call = rlang::caller_env()) {
  ext <- .ext(file)
  if (ext %in% .gold_text_exts) {
    raw <- .gold_raw_text(if (ext == "docx") .read_docx_text(file, call = call) else .read_text(file), file)
    cb <- .codebook_text_only()
    pages <- raw$pages[, c("file", "done", "notes")]
    lines <- raw$lines
  } else if (ext %in% .gold_xlsx_exts) {
    raw <- .gold_raw_xlsx(file, call)
    cb <- raw$codebook %||% fallback
    if (is.null(cb)) {
      .abort(c("{.file {basename(file)}} has no {.val codebook} sheet.",
               "i" = "Give the codebook: {.code benchmark(\"{basename(file)}\", codebook = cb, ...)}."),
             call = call, class = "amanuensis_error_input")
    }
    lines <- raw$lines
    pages <- raw$pages
    if (is.null(pages)) {
      fc <- .gold_match_columns(names(lines), "file")[[1]]
      f <- if (is.na(fc)) character() else unique(stats::na.omit(trimws(lines[[fc]])))
      pages <- data.frame(file = f, done = rep("no", length(f)), notes = NA_character_, stringsAsFactors = FALSE)
    } else {
      pages <- pages[, c("file", "done", "notes")]
    }
  } else {
    .abort("Only benchmarks in .xlsx, .txt, .md or .docx files can be edited or extended.", call = call,
           class = "amanuensis_error_input")
  }
  list(file = file, ext = ext, cb = cb, lines = lines, pages = pages, about = raw$about %||% list())
}

# A copy of a file about to be rewritten, next to it, so nothing typed is
# ever lost by adding pages.
.gold_backup <- function(file) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  to <- file.path(dirname(file), paste0(.stem(file), "-backup-", stamp, ".", tools::file_ext(file)))
  if (!file.copy(file, to, overwrite = FALSE)) .abort_unwritable(to)
  to
}

# Add pages to an existing drawn benchmark when a larger n is asked for.
# Returns TRUE when pages were added.
.gold_extend <- function(file, pages, cb, n, seed, prefill, call = rlang::caller_env()) {
  ext <- .ext(file)
  name <- basename(file)
  if (!ext %in% c("xlsx", .gold_text_exts)) {
    .inform(c("i" = "{.arg n} draws pages for a new benchmark; {.file {name}} is read as it is."))
    return(FALSE)
  }
  book <- .gold_load(file, fallback = cb, call = call)
  a <- book$about
  drawn <- if (is.null(a$drawn) || is.na(a$drawn)) character() else trimws(strsplit(a$drawn, ";", fixed = TRUE)[[1]])
  drawn <- drawn[nzchar(drawn)]
  old_seed <- suppressWarnings(as.numeric(a$seed %||% NA))
  if (!length(drawn) || is.na(old_seed)) {
    .inform(c("i" = "{.file {name}} has no record of a draw, so pages cannot be added to it; {.arg n} is ignored."))
    return(FALSE)
  }
  if (n <= length(drawn)) {
    if (n < length(drawn)) {
      .inform(c("i" = "{.file {name}} already has {length(drawn)} drawn pages; pages are never taken out of a benchmark, so {.code n = {n}} is ignored."))
    }
    return(FALSE)
  }
  if (is.null(pages)) {
    .abort(c("To add pages to {.file {name}}, give the page images too.",
             "i" = "For example {.code benchmark(\"{name}\", images = pages, n = {n})}."),
           call = call, class = "amanuensis_error_input")
  }
  if (!is.null(seed) && seed != old_seed) {
    .inform(c("i" = "{.file {name}} was drawn with seed {old_seed}; the added pages continue that draw (the seed you gave is not used)."))
  }
  grow <- .gold_extend_draw(pages$file, drawn, n, old_seed)
  add <- .gold_canonical(grow$add, pages$file)$files
  if (!length(add)) {
    .inform(c("i" = "Every page image is already in {.file {name}}."))
    return(FALSE)
  }
  if (!grow$same) {
    .inform(c("!" = "The page images are not the same as when {.file {name}} was drawn: the pages already drawn are kept, and the added ones come from the same seeded draw over the current images."))
  }
  cbk <- book$cb
  rows <- suppressWarnings(as.integer(a$rows %||% NA))
  rows <- if (is.na(rows)) NULL else rows
  units <- if (is.null(prefill)) list() else .gold_prefill_units(prefill, add, cbk, rows)
  add <- add[.gold_order(add)]
  new_lines <- .gold_lines_chr(units, cbk, add)
  book$lines <- .gold_append_lines(book$lines, new_lines)
  book$pages <- rbind(book$pages, data.frame(file = add, done = .gold_initial_done(ext, add, units),
                                             notes = NA_character_, stringsAsFactors = FALSE))
  a$n <- as.character(length(drawn) + length(grow$add))
  a$drawn <- paste(c(drawn, grow$add), collapse = "; ")
  if (length(units)) a$prefilled <- "yes"
  book$about <- a
  backup <- .gold_backup(file)
  .gold_book_write(book)
  if (!is.null(prefill)) .gold_prefill_messages(units, add)
  .gold_tell(c("v" = paste0("Added ", .gold_n(length(add), "page"), " to ", name, ": ", .gold_list(add), "."),
               "i" = paste0("What you had typed is kept; a copy of the previous file is in ", basename(backup), "."),
               ">" = paste0("Fill in the new pages, mark them done, save, close ", .gold_program(ext),
                            ", and run the same line again.")))
  TRUE
}

# New template lines under the existing ones, matched by column name (the
# file may have extra columns of the user's own, kept empty for new lines).
.gold_append_lines <- function(old, new) {
  if (!nrow(old)) return(new)
  cols <- .gold_match_columns(names(old), names(new))
  add <- as.data.frame(lapply(names(old), function(nm) rep(NA_character_, nrow(new))),
                       stringsAsFactors = FALSE, optional = TRUE)
  names(add) <- names(old)
  for (k in seq_along(cols)) if (!is.na(cols[k])) add[[cols[k]]] <- new[[names(cols)[k]]]
  out <- rbind(as.data.frame(lapply(old, as.character), stringsAsFactors = FALSE, optional = TRUE), add)
  names(out) <- names(old)
  out
}

# Character page units from lines in the sheet layout (no conversion):
# page values = first non-empty value on the page's lines; rows = lines with
# a row number or any row value, in row-number order when every row has one.
.gold_units_chr <- function(lines, cb, files) {
  vars <- cb$variables
  pv <- vars$variable[vars$level == "page"]
  rv <- vars$variable[vars$level == "row"]
  cols <- .gold_match_columns(names(lines), c("file", "row", pv, rv))
  get <- function(nm, idx) {
    j <- cols[[nm]]
    if (is.na(j)) return(rep("", length(idx)))
    v <- as.character(lines[[j]][idx])
    v[is.na(v)] <- ""
    v
  }
  all_idx <- seq_len(nrow(lines))
  fk <- .page_key(trimws(get("file", all_idx)))
  out <- list()
  for (f in files) {
    idx <- which(fk == .page_key(f))
    page <- vapply(pv, function(v) {
      x <- get(v, idx)
      x <- x[nzchar(trimws(x))]
      if (length(x)) x[1] else ""
    }, "")
    if (length(rv)) {
      rows <- as.data.frame(lapply(stats::setNames(rv, rv), function(v) get(v, idx)),
                            stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
      rnum <- trimws(get("row", idx))
      is_row <- nzchar(rnum) | Reduce(`|`, lapply(rows, function(x) nzchar(trimws(x))), rep(FALSE, length(idx)))
      rows <- rows[is_row, , drop = FALSE]
      num <- suppressWarnings(as.numeric(rnum[is_row]))
      if (length(num) && !anyNA(num)) rows <- rows[order(num), , drop = FALSE]
      rownames(rows) <- NULL
    } else {
      rows <- data.frame()
    }
    out[[f]] <- list(page = page, rows = rows)
  }
  out
}

# Lines in the sheet layout from character page units: rows numbered 1..k,
# page values on the first line of the page, one line with an empty row for
# a page with no rows. Rows left entirely empty are dropped.
.gold_lines_chr <- function(units, cb, files) {
  vars <- cb$variables
  pv <- vars$variable[vars$level == "page"]
  rv <- vars$variable[vars$level == "row"]
  cols <- .gold_columns(cb)
  pieces <- lapply(files, function(f) {
    u <- units[[f]] %||% list(page = character(), rows = data.frame())
    rows <- u$rows
    n <- 0L
    if (length(rv) && nrow(rows)) {
      vals <- lapply(rv, function(v) { x <- rows[[v]]; if (is.null(x)) rep("", nrow(rows)) else ifelse(is.na(x), "", x) })
      keep <- Reduce(`|`, lapply(vals, function(x) nzchar(trimws(x))), rep(FALSE, nrow(rows)))
      rows <- rows[keep, , drop = FALSE]
      n <- nrow(rows)
    }
    m <- max(1L, n)
    d <- lapply(stats::setNames(cols, cols), function(c) rep(NA_character_, m))
    d$file <- rep(f, m)
    if (n) d$row <- as.character(seq_len(n))
    for (v in pv) {
      val <- if (v %in% names(u$page)) u$page[[v]] else ""
      if (!is.na(val) && nzchar(trimws(val))) d[[v]][1] <- val
    }
    for (v in rv) {
      if (!n || is.null(rows[[v]])) next
      val <- as.character(rows[[v]])
      val[is.na(val) | !nzchar(trimws(val))] <- NA
      d[[v]] <- val
    }
    as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  })
  if (!length(pieces)) {
    return(as.data.frame(lapply(stats::setNames(cols, cols), function(c) character()),
                         stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE))
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

# ------- writing the Excel file -----------------------------------------------------

# writexl 2.0 can format columns as Text and add drop-down lists; with an
# older writexl the same sheets are written plain (the how_to sheet tells
# the user to type an apostrophe before values such as 007).
.gold_rich_xlsx <- function() {
  ns <- asNamespace("writexl")
  all(vapply(c("xl_sheet", "xl_col_spec", "xl_num_format", "xl_validation"), exists, logical(1),
             envir = ns, inherits = FALSE))
}

.gold_xl <- function(name) getExportedValue("writexl", name)

.gold_trunc <- function(x, n) {
  x <- gsub("\\s+", " ", x)
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 3), "..."), x)
}

.gold_write_xlsx <- function(book) {
  a <- book$about
  rows <- suppressWarnings(as.integer(a$rows %||% NA))
  sheets <- list(
    benchmark = .gold_bench_sheet(book$lines, book$cb),
    pages = .gold_pages_sheet(book$pages),
    about = .gold_plain_sheet(data.frame(item = names(a), value = vapply(unname(a), function(v) {
      if (is.null(v) || !length(v)) NA_character_ else as.character(v)[1]
    }, ""), stringsAsFactors = FALSE), c(18, 90)),
    codebook = .gold_plain_sheet(.gold_codebook_table(book$cb), c(18, 10, 8, 70, 30)),
    how_to = .gold_plain_sheet(data.frame(how_to = .gold_how_to(book$cb, if (is.na(rows)) NULL else rows,
                                                                identical(a$prefilled, "yes"), a$images),
                                          stringsAsFactors = FALSE), 130)
  )
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  .with_write_errors(book$file, writexl::write_xlsx(sheets, path = tmp))
  .write_raw_atomic(readBin(tmp, "raw", n = file.size(tmp)), book$file)
}

.gold_plain_sheet <- function(df, widths) {
  if (!.gold_rich_xlsx()) return(df)
  specs <- lapply(seq_along(df), function(j) .gold_xl("xl_col_spec")(j, width = widths[min(j, length(widths))]))
  .gold_xl("xl_sheet")(df, cols = specs, freeze = "A2")
}

.gold_codebook_table <- function(cb) {
  v <- cb$variables
  data.frame(variable = v$variable, type = v$type, level = v$level, description = v$description,
             values = vapply(v$values, function(z) paste(z, collapse = "; "), ""),
             stringsAsFactors = FALSE)
}

.gold_pages_sheet <- function(pages) {
  pages <- data.frame(file = as.character(pages$file), done = as.character(pages$done),
                      notes = as.character(pages$notes), stringsAsFactors = FALSE)
  if (!.gold_rich_xlsx()) return(pages)
  text <- .gold_xl("xl_num_format")("@")
  last <- max(nrow(pages) + 1L, 500L)
  .gold_xl("xl_sheet")(pages,
    cols = list(.gold_xl("xl_col_spec")(1, width = 26, format = text),
                .gold_xl("xl_col_spec")(2, width = 8, format = text),
                .gold_xl("xl_col_spec")(3, width = 50, format = text)),
    freeze = "A2",
    validation = list(.gold_xl("xl_validation")(paste0("B2:B", last), list = c("yes", "no"),
      input_title = "Done?", input_message = "yes when the page is completely transcribed and checked; only pages marked yes are used.",
      error_type = "warning", error_title = "yes or no", error_message = "Write yes or no.")))
}

.gold_bench_sheet <- function(lines, cb) {
  lines <- as.data.frame(lapply(lines, as.character), stringsAsFactors = FALSE, optional = TRUE)
  if (!.gold_rich_xlsx()) return(lines)
  vars <- cb$variables
  match_v <- .gold_match_columns(names(lines), vars$variable)
  var_of <- rep(NA_integer_, ncol(lines))
  var_of[match_v[!is.na(match_v)]] <- which(!is.na(match_v))
  last <- max(nrow(lines) + 1L, 2000L)
  text <- .gold_xl("xl_num_format")("@")
  specs <- list()
  checks <- list()
  for (j in seq_along(lines)) {
    nm <- names(lines)[j]
    k <- var_of[j]
    type <- if (!is.na(k)) vars$type[k] else if (tolower(nm) == "row") "integer" else "text"
    width <- if (tolower(nm) == "file") 26 else if (tolower(nm) == "row") 6 else if (type == "text") 24 else 14
    # every column as Text, numbers too: in a General column Excel turns an
    # age typed 3/12 or a range 1-2 into a date, whose serial number (46024)
    # would read as a valid number. Values are converted here on reading.
    specs[[j]] <- .gold_xl("xl_col_spec")(j, width = width, format = text)
    if (is.na(k)) next
    range <- paste0(.excel_col(j), "2:", .excel_col(j), last)
    title <- substr(vars$variable[k], 1, 32)
    msg <- .gold_trunc(vars$description[k], 250)
    vals <- vars$values[[k]]
    if (type == "category" && length(vals) && nchar(paste(vals, collapse = ",")) <= 255 && !any(grepl(",", vals, fixed = TRUE))) {
      checks[[length(checks) + 1]] <- .gold_xl("xl_validation")(range, list = vals, input_title = title,
        input_message = if (nzchar(msg)) msg else NA, error_type = "warning", error_title = "Not in the list",
        error_message = "This is not one of the codebook's values. Keep it only if it is right.")
    } else if (type == "boolean") {
      checks[[length(checks) + 1]] <- .gold_xl("xl_validation")(range, list = c("yes", "no"), input_title = title,
        input_message = if (nzchar(msg)) msg else NA, error_type = "warning", error_title = "yes or no",
        error_message = "Write yes or no (or leave the cell empty).")
    } else if (nzchar(msg)) {
      checks[[length(checks) + 1]] <- .gold_xl("xl_validation")(range, type = "any", input_title = title,
                                                               input_message = msg)
    }
  }
  .gold_xl("xl_sheet")(lines, cols = specs, freeze = "B2", validation = if (length(checks)) checks,
                       ignore_errors = list(number_stored_as_text = paste0("A2:", .excel_col(ncol(lines)), last)))
}

.gold_how_to <- function(cb, rows, prefilled, folder) {
  v <- cb$variables
  pv <- v$variable[v$level == "page"]
  has_rows <- any(v$level == "row")
  c("This file is your benchmark: your own transcription of a few pages, which score() compares with the model's output to measure its accuracy.",
    if (has_rows) "Sheet 'benchmark': one line per row of the page's table. The first line of each page is there already; type the page's rows on it and on new lines below it, copying the file name onto every line." else
      "Sheet 'benchmark': one line per page, already there; type the page's values on it.",
    if (has_rows && length(pv)) paste0("Page variables (", paste(pv, collapse = ", "), ") need only be filled on the first line of each page."),
    if (has_rows) "The row column may stay empty: rows are numbered in the order of the lines.",
    if (has_rows) "A page with no rows: leave the row variables empty on its line.",
    if (!is.null(rows)) paste0("Type only the first ", rows, " rows of each page; score() compares only those."),
    "Type exactly what is written, following the same rules as the instructions you give the model: spelling, abbreviations and accents as written; a doubtful reading ending in ?; an empty cell for what is illegible or not on the page.",
    "Do it before you look at the model's output for these pages.",
    "Yes/no variables: yes or no. Category variables: one of the values in the drop-down list (also in the codebook sheet).",
    if (.gold_rich_xlsx()) "Every column is formatted as Text, so Excel keeps what you type (007, 1-2, 3/12) as typed instead of turning it into a number or a date. If a value still changes, type an apostrophe first ('007)." else
      "Type an apostrophe before any value Excel might change into a number or a date ('007, '1-2, '3/12).",
    if (prefilled) "The lines hold the model's own output: check every value against the page image and correct it before marking the page done.",
    "When a page is finished, write yes in the done column of the 'pages' sheet. Only pages marked yes are used.",
    if (!is.null(folder) && !is.na(folder)) paste0("The page images are in ", folder, "."),
    "Keep the sheets 'pages', 'about' and 'codebook': they record the pages, how they were drawn, and the codebook.",
    "When you have finished, save, close Excel, and run the same benchmark() line again.")
}

# ------- writing a text benchmark ---------------------------------------------------

.gold_text_intro <- function(prefilled, folder) {
  c("Benchmark: your own transcription of the pages below, which score() compares with the model's output.",
    "Type the text of each page under its === line, exactly as written, following the same rules as the instructions you give the model.",
    "Leave a page empty until you have transcribed it: empty pages are left out. For a page with no writing at all, type [no text].",
    if (prefilled) "The text under each line is the model's own output: correct it against the image, then change \"not done\" to \"done\" on its === line.",
    if (!is.null(folder) && !is.na(folder)) paste0("The page images are in ", folder),
    "When you have finished, save, close the file, and run the same benchmark() line again.",
    "Keep the === lines and the record of the draw below as they are.")
}

.gold_text_lines <- function(book) {
  a <- book$about
  vals <- vapply(a, function(v) if (is.null(v) || !length(v) || is.na(v[1])) "" else as.character(v)[1], "")
  rec <- if (length(a)) paste0(names(a), ": ", vals) else character()
  keys <- .page_key(book$lines$file)
  body <- character()
  for (i in seq_len(nrow(book$pages))) {
    f <- book$pages$file[i]
    d <- .gold_parse_done(book$pages$done[i])
    status <- if (is.na(d)) "" else if (d) " done" else " not done"
    txt <- book$lines$text[match(.page_key(f), keys)]
    if (length(txt) != 1 || is.na(txt)) txt <- ""
    if (!nzchar(trimws(txt)) && isTRUE(d)) txt <- "[no text]"
    body <- c(body, paste0("=== ", f, " ===", status), if (nzchar(txt)) strsplit(txt, "\n", fixed = TRUE)[[1]], "")
  }
  c(.gold_text_intro(identical(a$prefilled, "yes"), a$images), "", rec, "", body)
}

# A minimal Word file: one paragraph per line, tabs kept. Read back by
# .read_docx_text(), which joins the paragraphs with line breaks.
.gold_write_docx <- function(paragraphs, path) {
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  paras <- vapply(enc2utf8(paragraphs), function(p) {
    if (!nzchar(p)) return("<w:p></w:p>")
    t <- gsub("\t", "</w:t><w:tab/><w:t xml:space=\"preserve\">", esc(p), fixed = TRUE)
    paste0("<w:p><w:r><w:t xml:space=\"preserve\">", t, "</w:t></w:r></w:p>")
  }, "", USE.NAMES = FALSE)
  parts <- list(
    "[Content_Types].xml" = paste0(
      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
      "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">",
      "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>",
      "<Default Extension=\"xml\" ContentType=\"application/xml\"/>",
      "<Override PartName=\"/word/document.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml\"/>",
      "</Types>"),
    "_rels/.rels" = paste0(
      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
      "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
      "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"word/document.xml\"/>",
      "</Relationships>"),
    "word/document.xml" = paste0(
      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
      "<w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:body>",
      paste(paras, collapse = ""), "<w:sectPr/></w:body></w:document>")
  )
  td <- tempfile("docx")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  for (nm in names(parts)) {
    dir.create(file.path(td, dirname(nm)), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(enc2utf8(parts[[nm]])), file.path(td, nm))
  }
  zipfile <- file.path(td, "out.docx")
  .with_write_errors(path, zip::zip(zipfile, files = names(parts), root = td, mode = "mirror"))
  .write_raw_atomic(readBin(zipfile, "raw", n = file.size(zipfile)), path)
}

# ------- reading: from each kind of file to raw lines ---------------------------------
#
# Every reader returns the same "raw" description, which .gold_build() turns
# into the benchmark:
#   lines       data.frame of text, the file's columns as named in the file
#   loc         function(i, var): where line i's value of var is, in words
#   pages       NULL, or data.frame(file, done (text, NA = not said), notes)
#   done_blank  what a blank done cell means: FALSE (Excel) or NA (= has text)
#   about       named list, codebook (or NULL), label, kind, date_cells

.gold_raw_xlsx <- function(file, call = rlang::caller_env()) {
  sheets <- tryCatch(readxl::excel_sheets(file), error = function(e) {
    .abort(c("{.file {basename(file)}} could not be opened as an Excel file.", "x" = conditionMessage(e),
             "i" = "If it is open in Excel, save it, close Excel and run the line again."),
           call = call, class = "amanuensis_error_input")
  })
  low <- tolower(trimws(sheets))
  bench <- sheets[low == "benchmark"][1]
  if (is.na(bench)) bench <- sheets[!low %in% .gold_meta_sheets][1]
  if (is.na(bench)) {
    .abort("{.file {basename(file)}} has no sheet with the benchmark (expected a sheet called {.val benchmark}).",
           call = call, class = "amanuensis_error_input")
  }
  read <- function(sheet, types) {
    as.data.frame(readxl::read_excel(file, sheet = sheet, col_types = types, .name_repair = "minimal"),
                  stringsAsFactors = FALSE, optional = TRUE)
  }
  lines <- read(bench, "text")
  # a second read keeps each cell's own type, to tell which cells Excel
  # stored as dates
  kinds <- tryCatch(read(bench, "list"), error = function(e) NULL)
  date_cells <- NULL
  if (!is.null(kinds) && identical(dim(kinds), dim(lines))) {
    date_cells <- matrix(unlist(lapply(kinds, function(col) vapply(col, function(v) inherits(v, "POSIXct"), logical(1)))),
                         nrow = nrow(kinds))
  }
  pages <- NULL
  ps <- sheets[low == "pages"][1]
  if (!is.na(ps)) {
    p <- read(ps, "text")
    pc <- .gold_match_columns(names(p), c("file", "done", "notes"))
    if (is.na(pc[["done"]])) pc[["done"]] <- .gold_match_columns(names(p), "checked")[[1]]
    if (!is.na(pc[["file"]])) {
      get <- function(nm) if (is.na(pc[[nm]])) rep(NA_character_, nrow(p)) else as.character(p[[pc[[nm]]]])
      pages <- data.frame(file = trimws(get("file")), done = trimws(get("done")), notes = get("notes"),
                          .line = seq_len(nrow(p)) + 1L, stringsAsFactors = FALSE)
      pages <- pages[!is.na(pages$file) & nzchar(pages$file), , drop = FALSE]
    }
  }
  about <- list()
  as <- sheets[low == "about"][1]
  if (!is.na(as)) {
    ab <- read(as, "text")
    if (ncol(ab) >= 2) {
      items <- trimws(as.character(ab[[1]]))
      vals <- trimws(as.character(ab[[2]]))
      ok <- !is.na(items) & nzchar(items)
      about <- stats::setNames(as.list(vals[ok]), items[ok])
    }
  }
  cbk <- NULL
  cs <- sheets[low == "codebook"][1]
  if (!is.na(cs)) cbk <- .gold_codebook_from_sheet(file, cs, about$codebook_source, about$free_text)
  list(lines = lines, date_cells = date_cells, pages = pages, done_blank = FALSE, about = about,
       codebook = cbk, label = basename(file), kind = "xlsx", sheet = bench,
       loc = function(i, var, j = NA) {
         if (is.na(j)) sprintf("sheet %s, row %d (%s)", bench, i + 1L, var) else
           sprintf("sheet %s, cell %s%d (%s)", bench, .excel_col(j), i + 1L, var)
       },
       page_loc = function(i) sprintf("sheet pages, row %d", i))
}

# The codebook kept in the file. The values already hold any catch-all
# "other", and the messages were seen when the codebook was first made.
# A table codebook made with free_text = TRUE lists the page's other text as
# a page variable `text`; read back as a plain variable, score() would count
# that long text as one more value (about always wrong) instead of scoring
# it apart. The about sheet records the choice; files written before it did
# are recognised by the text variable's description, which the package
# writes itself.
.gold_codebook_from_sheet <- function(file, sheet, source = NULL, free_text = NULL) {
  tab <- .read_codebook_sheet(file, sheet)
  if (!nrow(tab)) return(NULL)
  cbk <- suppressMessages(.codebook_from_table(tab, source = source %||% file, other = FALSE))
  v <- cbk$variables
  if (nrow(v) == 1 && v$variable == "text" && v$level == "page" && v$type == "text") return(.codebook_text_only())
  is_other <- v$variable == "text" & v$level == "page" & v$type == "text"
  said <- .gold_parse_yes_no(free_text %||% NA)
  other <- any(is_other) && nrow(v) > 1 &&
    (isTRUE(said) || (is.na(said) && identical(trimws(v$description[is_other][1]), .text_description_with_records)))
  if (other) {
    vc <- .gold_match_columns(names(tab), "variable")[[1]]
    keep <- if (is.na(vc)) rep(TRUE, nrow(tab)) else is.na(tab[[vc]]) | trimws(tab[[vc]]) != "text"
    cbk <- suppressMessages(.codebook_from_table(tab[keep, , drop = FALSE], source = source %||% file,
                                                 other = FALSE, free_text = TRUE))
  }
  if (!is.null(source) && !is.na(source)) cbk$source <- source
  cbk
}

.gold_raw_table <- function(df, file, kind, loc = NULL) {
  df <- as.data.frame(lapply(df, .gold_chr_column), stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  label <- basename(file)
  loc <- loc %||% function(i, var, j = NA) sprintf("row %d (%s)", i, var)
  list(lines = df, date_cells = NULL, pages = NULL, done_blank = FALSE, about = list(), codebook = NULL,
       label = label, kind = kind, loc = loc)
}

.gold_chr_column <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::as_factor(x, levels = "default")
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.character(x))
  out <- .chr_values(x)
  attributes(out) <- NULL
  out
}

.gold_raw_csv <- function(file) {
  label <- basename(file)
  tab <- .read_delim_text(file)
  raw <- .gold_raw_table(tab, file, "csv",
                         loc = function(i, var, j = NA) sprintf("line %d of %s (%s)", i + 1L, label, var))
  # a ";" file is what Spanish or Catalan Excel saves, with decimal commas
  # (and what output(sep = ";") writes): read 2,5 as 2.5 there only
  raw$dec <- if (identical(attr(tab, "sep"), ";")) "," else "."
  raw
}

.gold_raw_dta <- function(file) {
  rlang::check_installed("haven", "to read Stata files")
  .gold_raw_table(haven::read_dta(file), file, "dta")
}

# A JSON file as output() writes it (an array of pages, each with its file
# and the model-shaped data) or an object keyed by file name.
.gold_raw_json <- function(file, cb, call = rlang::caller_env()) {
  x <- tryCatch(.read_json(file), error = function(e) {
    .abort(c("{.file {basename(file)}} is not valid JSON.", "x" = conditionMessage(e)), call = call,
           class = "amanuensis_error_input")
  })
  entries <- list()
  if (is.list(x) && length(x) && is.null(names(x))) {
    for (el in x) {
      if (!is.list(el) || is.null(el$file)) {
        .abort(c("{.file {basename(file)}} is a list whose elements do not all have a {.field file}.",
                 "i" = "Expected one object per page, as {.fn output} writes: {.code [{{\"file\": \"p001.jpg\", \"data\": {{...}}}}, ...]}."),
               call = call, class = "amanuensis_error_input")
      }
      entries[[length(entries) + 1]] <- list(file = as.character(el$file)[1], data = el$data)
    }
  } else if (is.list(x) && !is.null(names(x))) {
    for (nm in names(x)) {
      v <- x[[nm]]
      d <- if (is.list(v) && "data" %in% names(v) && !"data" %in% cb$variables$variable) v$data else v
      entries[[length(entries) + 1]] <- list(file = nm, data = d)
    }
  } else {
    .abort("{.file {basename(file)}} holds no pages.", call = call, class = "amanuensis_error_input")
  }
  vars <- cb$variables
  pv <- vars$variable[vars$level == "page"]
  rv <- vars$variable[vars$level == "row"]
  path <- .records_path(cb)
  skip <- path %||% character()
  cols <- .gold_columns(cb)
  as_text <- function(v) {
    if (is.null(v) || !length(v)) return("")
    if (is.list(v)) return(.to_json(v))
    if (is.logical(v)) return(if (is.na(v[1])) "" else if (v[1]) "yes" else "no")
    out <- .chr_values(v[1])
    if (is.na(out)) "" else out
  }
  pieces <- list()
  where_row <- character()
  done <- logical()
  for (e in entries) {
    done <- c(done, !is.null(e$data))
    if (is.null(e$data)) next
    scal <- .object_scalars(e$data, skip = skip)
    rr <- if (is.null(path)) list() else .page_rows(e$data, path)
    m <- max(1L, length(rr))
    d <- lapply(stats::setNames(cols, cols), function(c) rep("", m))
    d$file <- rep(e$file, m)
    if (length(rr)) d$row <- as.character(seq_along(rr))
    for (v in pv) d[[v]][1] <- as_text(scal[[v]])
    if (length(rr)) for (v in rv) d[[v]] <- vapply(rr, function(r) as_text(r[[v]]), "")
    pieces[[length(pieces) + 1]] <- as.data.frame(d, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
    where_row <- c(where_row, if (length(rr)) paste0("row ", seq_along(rr)) else "page values")
  }
  lines <- if (length(pieces)) do.call(rbind, pieces) else
    as.data.frame(lapply(stats::setNames(cols, cols), function(c) character()), stringsAsFactors = FALSE,
                  check.names = FALSE, optional = TRUE)
  files <- vapply(entries, function(e) e$file, "")
  label <- basename(file)
  list(lines = lines, date_cells = NULL, done_blank = FALSE, about = list(), codebook = NULL, label = label,
       kind = "json",
       pages = data.frame(file = files, done = ifelse(done, "yes", "no"), notes = NA_character_,
                          .line = seq_along(files), stringsAsFactors = FALSE),
       loc = function(i, var, j = NA) sprintf("%s, %s (%s)", lines$file[i], where_row[i], var),
       page_loc = function(i) sprintf("page %d of %s", i, label))
}

# Free text: each page starts with a line "=== <file name> ===", optionally
# followed by "done" or "not done". Lines before the first page are the
# instructions and the record of the draw ("seed: 1930").
.gold_raw_text <- function(text, file) {
  label <- basename(file)
  all <- strsplit(text, "\n", fixed = TRUE)[[1]]
  head_re <- "^\\s*={3,}\\s*(.*?)\\s*={3,}\\s*(.*?)\\s*$"
  # a header needs a file name: a Markdown underline (=======) is not one
  heads <- which(grepl(head_re, all, perl = TRUE) & nzchar(trimws(sub(head_re, "\\1", all, perl = TRUE))))
  pre <- if (length(heads)) all[seq_len(heads[1] - 1)] else all
  about <- list()
  rec <- regmatches(pre, regexec("^([a-z_]+):\\s?(.*)$", pre))
  for (m in rec) {
    if (length(m) == 3 && m[2] %in% .gold_about_items) about[[m[2]]] <- if (nzchar(trimws(m[3]))) trimws(m[3]) else NA_character_
  }
  files <- trimws(sub(head_re, "\\1", all[heads], perl = TRUE))
  # output(res, "x.txt") writes "=== file (source p. N) ===": drop the parenthesis
  files <- sub("\\s*\\([^()]*\\bp\\. ?[0-9]+\\)$", "", files, perl = TRUE)
  status <- tolower(trimws(sub(head_re, "\\2", all[heads], perl = TRUE)))
  ends <- c(heads[-1] - 1L, length(all))
  texts <- vapply(seq_along(heads), function(k) {
    a <- heads[k] + 1L
    b <- ends[k]
    if (b < a) return("")
    body <- sub("[ \t]+$", "", all[a:b])
    nz <- which(nzchar(trimws(body)))
    if (!length(nz)) return("")
    paste(body[min(nz):max(nz)], collapse = "\n")
  }, "")
  no_text <- tolower(trimws(texts)) == "[no text]"
  texts[no_text] <- ""
  done <- ifelse(status == "", NA_character_,
                 ifelse(.gold_parse_done(status) %in% TRUE, "yes", ifelse(.gold_parse_done(status) %in% FALSE, "no", NA_character_)))
  done[no_text] <- "yes"
  dup <- duplicated(.page_key(files))
  if (any(dup)) {
    .gold_tell(c(paste0(label, " has more than one === line for ", .gold_list(unique(files[dup])),
                        "; only the first is used.")), fun = .warn)
    files <- files[!dup]
    texts <- texts[!dup]
    done <- done[!dup]
  }
  list(lines = data.frame(file = files, text = texts, stringsAsFactors = FALSE),
       pages = data.frame(file = files, done = done, notes = NA_character_, .line = seq_along(files),
                          stringsAsFactors = FALSE),
       done_blank = NA, about = about, codebook = NULL, date_cells = NULL, label = label, kind = "text",
       loc = function(i, var, j = NA) sprintf("the page %s in %s", files[i], label),
       page_loc = function(i) sprintf("the === line of %s", files[i]))
}

# ------- reading: from raw lines to the benchmark -------------------------------------

# `cb` (the user's codebook) wins over the one the file carries; `fallback`
# (score()'s results codebook) is used only for files that carry none.
.gold_read <- function(file, cb = NULL, pages = NULL, rows = NULL, fallback = NULL, call = rlang::caller_env()) {
  ext <- .ext(file)
  if (ext == "rds") {
    obj <- tryCatch(readRDS(file), error = function(e) {
      .abort(c("{.file {basename(file)}} could not be read as an .rds file.", "x" = conditionMessage(e)),
             call = call, class = "amanuensis_error_input")
    })
    if (inherits(obj, "amanuensis_gold")) return(obj)
    if (inherits(obj, "amanuensis_result") && !is.null(.result_info(obj))) {
      cb <- cb %||% .result_info(obj)$codebook
      obj <- as.data.frame(obj)
    }
    if (!is.data.frame(obj)) {
      .abort("{.file {basename(file)}} does not hold a table (it holds {.cls {class(obj)}}).", call = call,
             class = "amanuensis_error_input")
    }
    raw <- .gold_raw_table(obj, file, "rds")
  } else if (ext == "json") {
    cb <- cb %||% fallback
    if (is.null(cb)) .gold_need_codebook(file, call)
    raw <- .gold_raw_json(file, cb, call)
  } else {
    raw <- switch(ext,
      xlsx = , xlsm = , xls = .gold_raw_xlsx(file, call),
      csv = , tsv = .gold_raw_csv(file),
      dta = .gold_raw_dta(file),
      txt = , md = .gold_raw_text(.read_text(file), file),
      docx = .gold_raw_text(.read_docx_text(file, call = call), file))
  }
  if (is.null(cb)) cb <- raw$codebook
  if (is.null(cb) && raw$kind == "text") cb <- .codebook_text_only()
  if (is.null(cb)) cb <- fallback
  if (is.null(cb)) .gold_need_codebook(file, call)
  if (raw$kind == "text" && !identical(cb$variables$variable, "text")) {
    other <- setdiff(cb$variables$variable, "text")
    .abort(c("{.file {basename(file)}} holds only the text of each page, but the codebook also has {.val {other}}.",
             "i" = "Keep a table benchmark in an Excel file, or read this one with {.code codebook = codebook(free_text = TRUE)}."),
           call = call, class = "amanuensis_error_input")
  }
  .gold_build(raw, cb, image_files = if (is.null(pages)) NULL else pages$file, rows = rows,
              source = .show_path(file), call = call)
}

.gold_need_codebook <- function(file, call) {
  .abort(c("To read {.file {basename(file)}}, give the codebook it follows.",
           "i" = "For example {.code benchmark(\"{basename(file)}\", codebook = cb)}."),
         call = call, class = "amanuensis_error_input")
}

# For score(): a benchmark object, or anything benchmark() can read. The
# codebook given is a fallback for files that carry none (csv, json, a plain
# table): a benchmark keeps the codebook it was made with, and score()
# compares the variables it shares with the results.
.as_gold <- function(x, codebook = NULL, call = rlang::caller_env()) {
  if (inherits(x, "amanuensis_gold")) return(x)
  cb <- if (is.null(codebook)) NULL else .gold_as_codebook(codebook, call = call)
  if (.is_string(x)) {
    if (!file.exists(x)) {
      .abort(c("Cannot find the benchmark {.path {(.show_path(x))}}.",
               "i" = "Make it with {.fn benchmark}; relative paths start from your working folder, {.path {(.show_path(getwd()))}}."),
             call = call, class = "amanuensis_error_input")
    }
    return(.gold_read(x, fallback = cb, call = call))
  }
  if (is.data.frame(x)) {
    if (is.null(cb)) .abort("A benchmark given as a data frame needs {.arg codebook}.", call = call, class = "amanuensis_error_input")
    raw <- .gold_raw_table(x, "data frame", "data frame")
    raw$label <- "the data frame"
    return(.gold_build(raw, cb, source = NA_character_, call = call))
  }
  .abort("The benchmark must come from {.fn benchmark}, or be a benchmark file.", call = call,
         class = "amanuensis_error_input")
}

.gold_build <- function(raw, cb, image_files = NULL, rows = NULL, source = NA_character_,
                        call = rlang::caller_env()) {
  label <- raw$label
  program <- .gold_program(if (raw$kind == "xlsx") "xlsx" else if (raw$kind == "text") "txt" else raw$kind)
  vars <- cb$variables
  pv <- vars$variable[vars$level == "page"]
  rv <- vars$variable[vars$level == "row"]
  all_v <- c(pv, rv)
  types <- stats::setNames(vars$type, vars$variable)
  cats <- stats::setNames(vars$values, vars$variable)
  has_rows <- length(rv) > 0
  lines <- raw$lines

  # 1. columns
  cols <- .gold_match_columns(names(lines), c("file", "row", all_v))
  if (is.na(cols[["file"]])) {
    .abort(c("{label} has no {.field file} column.",
             "i" = "Its columns are {.val {names(lines)}}. The {.field file} column names the page image each line comes from."),
           call = call, class = "amanuensis_error_input")
  }
  miss <- all_v[is.na(cols[all_v])]
  if (length(miss)) {
    .abort(c("{label} has no column for {cli::qty(length(miss))}the codebook variable{?s} {.val {miss}}.",
             "i" = "Its columns are {.val {names(lines)}}.",
             "i" = "If the codebook changed after the benchmark was made, read it without {.arg codebook} (an Excel benchmark keeps its own), or add the missing columns."),
           call = call, class = "amanuensis_error_input")
  }
  extra <- names(lines)[setdiff(seq_along(lines), cols)]
  extra <- extra[!is.na(extra) & nzchar(trimws(extra)) & !tolower(trimws(extra)) %in% .gold_quiet_cols]
  if (length(extra)) .inform(c("i" = "Ignoring column{?s} {.val {extra}} of {label} (not in the codebook)."))
  get <- function(nm) {
    j <- cols[[nm]]
    if (is.na(j)) return(rep("", nrow(lines)))
    v <- as.character(lines[[j]])
    v[is.na(v)] <- ""
    trimws(v)
  }
  M <- lapply(stats::setNames(all_v, all_v), get)
  file_v <- .nfc(get("file"))
  row_v <- if (has_rows) get("row") else rep("", nrow(lines))
  nonempty <- function(vs) Reduce(`|`, lapply(M[vs], nzchar), rep(FALSE, nrow(lines)))
  any_val <- nonempty(all_v)
  row_val <- nonempty(rv)
  keep <- nzchar(file_v) | nzchar(row_v) | any_val
  loc <- function(i, var) raw$loc(i, var, if (var %in% names(cols)) cols[[var]] else NA)

  # 2. a file name on every line: added lines often leave it out
  last <- ""
  filled <- 0L
  for (i in which(keep)) {
    if (nzchar(file_v[i])) {
      last <- file_v[i]
    } else if (nzchar(last)) {
      file_v[i] <- last
      filled <- filled + 1L
    } else {
      .gold_tell(c(paste0(label, ": ", loc(i, "file"), " has values but no file name."),
                   "i" = "Write the page image's file name on every line, then run the line again."),
                 fun = .abort, call = call, class = "amanuensis_error_input")
    }
  }
  if (filled) .inform(c("i" = "{filled} line{?s} of {label} had no file name; took the file of the line above."))
  # names are matched to the page images when given, else to the names the
  # file itself lists (its pages sheet and the record of the draw)
  drawn_names <- trimws(strsplit(raw$about$drawn %||% "", ";", fixed = TRUE)[[1]])
  drawn_names <- drawn_names[!is.na(drawn_names) & nzchar(drawn_names)]
  known <- image_files %||% unique(c(if (!is.null(raw$pages)) raw$pages$file, drawn_names))
  if (!length(known)) known <- NULL
  canon <- .gold_canonical(file_v, known)
  file_v <- canon$files
  unknown <- if (is.null(image_files)) character() else canon$unknown

  # 3. which pages are done
  keys <- .page_key(file_v)
  shown <- character()
  for (i in which(keep)) if (!keys[i] %in% names(shown)) shown[keys[i]] <- file_v[i]
  has_val <- stats::setNames(vapply(names(shown), function(k) any(any_val[keep & keys == k]), logical(1)), names(shown))
  prefilled <- isTRUE(.gold_parse_yes_no(raw$about$prefilled %||% NA))
  ps <- raw$pages
  listed <- character()
  explicit <- FALSE
  if (!is.null(ps) && nrow(ps)) {
    pc <- .gold_canonical(ps$file, known)
    if (!is.null(image_files)) unknown <- unique(c(unknown, pc$unknown))
    ps$file <- pc$files
    d <- .gold_parse_done(ps$done)
    blank <- is.na(ps$done) | !nzchar(trimws(ps$done))
    bad <- !blank & is.na(d)
    if (any(bad)) {
      where <- vapply(which(bad), function(i) raw$page_loc(ps$.line[i]), "")
      .gold_tell(c(paste0("The done column of ", label, " must say yes or no: ",
                          .gold_list(paste0(where, " says \"", ps$done[bad], "\"")), "."),
                   "i" = paste0("Correct it, save, close ", program, ", and run the line again.")),
                 fun = .abort, call = call, class = "amanuensis_error_input")
    }
    # a done cell that says anything at all (the template writes "no")
    explicit <- any(!blank)
    pk <- .page_key(ps$file)
    for (i in seq_len(nrow(ps))) if (!pk[i] %in% names(shown)) shown[pk[i]] <- ps$file[i]
    hv <- function(k) if (k %in% names(has_val)) has_val[[k]] else FALSE
    d[blank] <- if (is.na(raw$done_blank)) vapply(pk[blank], hv, logical(1)) else raw$done_blank
    done <- stats::setNames(d, pk)
    done <- done[!duplicated(names(done))]
    listed <- names(done)
    outside <- setdiff(names(has_val)[has_val], listed)
    if (length(outside)) {
      .gold_tell(c("!" = paste0(.gold_n(length(outside), "page"), " in the benchmark sheet of ", label,
                               " ", if (length(outside) == 1) "is" else "are", " not listed in its pages sheet and ",
                               if (length(outside) == 1) "is" else "are", " left out: ", .gold_list(shown[outside]), "."),
                   "i" = "Add them to the pages sheet with done = yes to use them."))
    }
    done <- c(done, stats::setNames(rep(FALSE, length(setdiff(names(shown), listed))), setdiff(names(shown), listed)))
  } else {
    done <- has_val
  }
  if (!any(done)) {
    pool <- if (length(listed)) intersect(names(has_val)[has_val], listed) else names(has_val)[has_val]
    if (length(pool) && prefilled) {
      .gold_tell(c(paste0("No page of ", label, " is marked done yet."),
                   "i" = "The benchmark was prefilled with the model's output, so pages count only once you have corrected them and marked them done.",
                   "i" = if (raw$kind == "text") "Change \"not done\" to \"done\" on the === line of each corrected page." else
                     "Write yes in the done column of the pages sheet for each corrected page."),
                 fun = .abort, call = call, class = c("amanuensis_error_gold_empty", "amanuensis_error_input"))
    }
    # every page marked no (as the template starts): a page typed only in
    # part would count each value not typed yet as a model error, so nothing
    # is used until the finished pages say yes
    if (length(pool) && explicit) {
      .gold_tell(c(paste0("No page of ", label, " is marked done yet."),
                   "i" = "Only pages marked done are used, so that a page you have not finished typing does not count against the model.",
                   "i" = paste0(if (raw$kind == "text") "Change \"not done\" to \"done\" on the === line of each finished page" else
                     "Write yes in the done column of the pages sheet for each finished page", " (pages with values: ",
                     .gold_list(shown[pool]), "), save, close ", program, ", and run the same line again.")),
                 fun = .abort, call = call, class = c("amanuensis_error_gold_empty", "amanuensis_error_input"))
    }
    if (length(pool)) {
      done[pool] <- TRUE
      .gold_tell(c("!" = paste0("No page of ", label, " is marked done; using the ", .gold_n(length(pool), "page"),
                               " with values: ", .gold_list(shown[pool]), "."),
                   "i" = if (raw$kind == "text") "Add \"done\" after a page's === line to choose pages yourself." else
                     "Write yes in the done column of the pages sheet to choose them yourself."))
    } else {
      todo <- if (length(listed)) shown[listed] else shown
      .gold_tell(c(paste0(label, " has no values yet", if (length(todo)) paste0(" for ", .gold_list(todo)) else "", "."),
                   "i" = paste0("Fill them in, save, close ", program, ", and run the same line again.")),
                 fun = .abort, call = call, class = c("amanuensis_error_gold_empty", "amanuensis_error_input"))
    }
  } else if (length(listed)) {
    waiting <- intersect(names(has_val)[has_val], names(done)[!done])
    waiting <- intersect(waiting, listed)
    if (length(waiting)) {
      .gold_tell(c("i" = paste0("Using the ", .gold_n(sum(done), "page"), " marked done; ",
                               .gold_n(length(waiting), "other page"), " with values ",
                               if (length(waiting) == 1) "is" else "are", " left out until marked done: ",
                               .gold_list(shown[waiting]), ".")))
    }
  }
  done_keys <- names(done)[done]
  done_keys <- done_keys[.gold_order(shown[done_keys])]

  # 4. for each done page: the line holding each page value, and its rows
  # (a row number Excel stored as a date would pass for row 46024)
  row_dates <- logical(nrow(lines))
  if (has_rows && !is.null(raw$date_cells) && !is.na(cols[["row"]])) row_dates <- raw$date_cells[, cols[["row"]]] %in% TRUE
  errors <- character()
  conflicts <- character()
  renumbered <- character()
  empty_done <- character()
  page_cell <- lapply(stats::setNames(pv, pv), function(v) rep(NA_integer_, length(done_keys)))
  row_line <- integer()
  row_num <- integer()
  row_page <- integer()
  for (p in seq_along(done_keys)) {
    li <- which(keep & keys == done_keys[p])
    if (!length(li) || !any(any_val[li])) empty_done <- c(empty_done, shown[[done_keys[p]]])
    for (v in pv) {
      nz <- li[nzchar(M[[v]][li])]
      if (!length(nz)) next
      page_cell[[v]][p] <- nz[1]
      differ <- nz[-1][M[[v]][nz[-1]] != M[[v]][nz[1]]]
      if (length(differ)) {
        conflicts <- c(conflicts, paste0(shown[[done_keys[p]]], ", ", v, ": \"", M[[v]][nz[1]], "\" (", loc(nz[1], v),
                                         ") but \"", M[[v]][differ[1]], "\" (", loc(differ[1], v), ")"))
      }
    }
    if (!has_rows) next
    rl <- li[nzchar(row_v[li]) | row_val[li]]
    if (!length(rl)) next
    given <- nzchar(row_v[rl])
    num <- suppressWarnings(as.numeric(row_v[rl]))
    stamped <- given & row_dates[rl]
    bad <- given & !stamped & (is.na(num) | num < 1 | num != round(num))
    for (i in rl[stamped]) errors <- c(errors, paste0(loc(i, "row"), ": Excel turned what was typed into a date (it reads \"",
                                                      row_v[i], "\"); type the row number again"))
    for (i in rl[bad]) errors <- c(errors, paste0(loc(i, "row"), ": \"", row_v[i], "\" is not a row number (1, 2, 3, ...)"))
    bad <- bad | stamped
    if (all(given) && !any(bad)) {
      dup <- num[duplicated(num)]
      for (d in unique(dup)) {
        errors <- c(errors, paste0(shown[[done_keys[p]]], ": row number ", d, " is used on more than one line (",
                                   paste(vapply(rl[num == d], function(i) loc(i, "row"), ""), collapse = "; "), ")"))
      }
      ord <- order(num)
      rl <- rl[ord]
      nums <- as.integer(num[ord])
    } else {
      if (any(given)) renumbered <- c(renumbered, shown[[done_keys[p]]])
      nums <- seq_along(rl)
    }
    row_line <- c(row_line, rl)
    row_num <- c(row_num, nums)
    row_page <- c(row_page, rep(p, length(rl)))
  }

  # 5. values converted to the codebook's types, every failure with its cell
  page_vals <- list()
  row_vals <- list()
  case_fixed <- character()
  off_list <- character()
  damage <- list(serial = character(), sci = character(), zeros = character())
  for (v in all_v) {
    idx <- if (v %in% pv) page_cell[[v]] else row_line
    vals <- ifelse(is.na(idx), "", M[[v]][ifelse(is.na(idx), 1L, idx)])
    locs <- vapply(seq_along(idx), function(k) if (is.na(idx[k])) "" else loc(idx[k], v), "")
    dates <- rep(FALSE, length(idx))
    if (!is.null(raw$date_cells) && !is.na(cols[[v]])) {
      dates <- vapply(idx, function(i) if (is.na(i)) FALSE else isTRUE(raw$date_cells[i, cols[[v]]]), logical(1))
    }
    conv <- .gold_convert(vals, types[[v]], cats[[v]], dec = raw$dec %||% ".")
    # a date Excel made in a number or yes/no column: its serial number
    # passes for a number (46024 for an age typed 3/12) or gives a puzzling
    # "not yes or no", so it stops the read with what happened
    stamped <- types[[v]] %in% c("number", "integer", "boolean") & dates
    if (any(stamped)) {
      errors <- c(errors, paste0(locs[stamped], ": Excel turned what was typed into a date (it reads \"", vals[stamped],
                                 "\"); type the value again as ", if (types[[v]] == "boolean") "yes or no" else "a number"))
    }
    bad <- which(!is.na(conv$problem) & !stamped)
    if (length(bad)) errors <- c(errors, paste0(locs[bad], ": \"", vals[bad], "\" ", conv$problem[bad]))
    if (any(conv$case_fixed)) case_fixed <- c(case_fixed, paste0("\"", vals[conv$case_fixed], "\" -> \"", conv$value[conv$case_fixed], "\""))
    if (any(conv$unknown)) {
      off_list <- c(off_list, paste0(locs[conv$unknown], ": \"", vals[conv$unknown], "\" is not one of ",
                                     .gold_list(cats[[v]], 8)))
    }
    if (types[[v]] == "text" && raw$kind %in% c("xlsx", "csv")) {
      dmg <- .gold_excel_damage(vals, dates)
      for (kind in names(damage)) {
        hit <- which(dmg[[kind]])
        if (length(hit)) damage[[kind]] <- c(damage[[kind]], paste0(locs[hit], " reads \"", vals[hit], "\""))
      }
    }
    if (v %in% pv) page_vals[[v]] <- conv$value else row_vals[[v]] <- conv$value
  }
  if (length(errors)) {
    shown_err <- utils::head(errors, 10)
    msg <- c(paste0(.gold_n(length(errors), "value"), " in ", label, " cannot be read as the codebook says:"),
             stats::setNames(shown_err, rep("x", length(shown_err))),
             if (length(errors) > 10) c("x" = paste0("... and ", length(errors) - 10, " more.")),
             "i" = paste0("Correct ", if (length(errors) == 1) "it" else "them", ", save, close ", program, ", and run the same line again."),
             "i" = "Numbers: digits, with a point for decimals (1234.5). Yes/no: yes or no. Leave a cell empty when the page has nothing there.")
    .gold_tell(msg, fun = .abort, call = call, class = "amanuensis_error_input")
  }
  .gold_read_warnings(label, unknown, image_files, conflicts, renumbered, case_fixed, off_list, damage,
                      if (raw$kind == "xlsx") empty_done else character())

  # 6. the table, in the results layout: page values repeated on every row
  out_file <- character()
  out_row <- integer()
  out_page <- integer()
  out_rowline <- integer()
  for (p in seq_along(done_keys)) {
    here <- which(row_page == p)
    if (has_rows && length(here)) {
      out_file <- c(out_file, rep(shown[[done_keys[p]]], length(here)))
      out_row <- c(out_row, row_num[here])
      out_page <- c(out_page, rep(p, length(here)))
      out_rowline <- c(out_rowline, here)
    } else {
      out_file <- c(out_file, shown[[done_keys[p]]])
      out_row <- c(out_row, NA_integer_)
      out_page <- c(out_page, p)
      out_rowline <- c(out_rowline, NA_integer_)
    }
  }
  tab <- list(file = out_file)
  if (has_rows) tab$row <- as.integer(out_row)
  for (v in pv) tab[[v]] <- page_vals[[v]][out_page]
  for (v in rv) tab[[v]] <- row_vals[[v]][out_rowline]
  tab <- as.data.frame(tab, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  rownames(tab) <- NULL

  all_keys <- names(done)
  pages_df <- data.frame(file = unname(shown[all_keys]), done = unname(done[all_keys]), stringsAsFactors = FALSE)
  pages_df$n_rows <- vapply(all_keys, function(k) {
    if (!done[[k]]) return(NA_integer_)
    p <- match(k, done_keys)
    if (has_rows) sum(row_page == p) else 1L
  }, integer(1), USE.NAMES = FALSE)
  pages_df <- pages_df[.gold_order(pages_df$file), , drop = FALSE]
  rownames(pages_df) <- NULL

  a <- raw$about
  draw <- NULL
  if (!is.null(a$drawn) && !is.na(a$drawn) && nzchar(a$drawn)) {
    drawn <- trimws(strsplit(a$drawn, ";", fixed = TRUE)[[1]])
    drawn <- drawn[nzchar(drawn)]
    draw <- list(seed = suppressWarnings(as.numeric(a$seed %||% NA)), files = drawn,
                 n = suppressWarnings(as.integer(a$n %||% length(drawn))), date = a$made %||% NA_character_,
                 method = a$method %||% NA_character_)
  }
  rows_file <- suppressWarnings(as.integer(a$rows %||% NA))
  if (is.null(rows) && !is.na(rows_file)) rows <- rows_file
  if (!is.null(rows) && !is.na(rows_file) && rows != rows_file) {
    .inform(c("i" = "{label} records that only the first {rows_file} rows of each page were to be typed; using {.code rows = {rows}} as you asked."))
  }
  .new_gold(tab, cb, pages_df, draw = draw, rows = rows, prefilled = prefilled, source = source)
}

# Convert the text of one variable's cells to its codebook type. Returns the
# typed values and, per cell, the problem (NA when fine), plus category
# values matched by ignoring case and accents, and those matching nothing.
.gold_convert <- function(vals, type, cats = character(), dec = ".") {
  vals <- as.character(vals)
  empty <- is.na(vals) | !nzchar(vals)
  problem <- rep(NA_character_, length(vals))
  case_fixed <- logical(length(vals))
  unknown <- logical(length(vals))
  if (type %in% c("number", "integer")) {
    x <- .as_number(vals, dec)
    x[empty] <- NA
    bad <- !empty & !is.finite(x)
    if (type == "integer") {
      frac <- !empty & !bad & x != round(x)
      bad <- bad | frac
      problem[bad] <- "is not a whole number"
    } else {
      problem[bad] <- "is not a number"
    }
    x[bad] <- NA
    value <- if (type == "integer" && all(is.na(x) | abs(x) <= .Machine$integer.max)) as.integer(x) else x
  } else if (type == "boolean") {
    value <- .gold_parse_yes_no(vals)
    value[empty] <- NA
    problem[!empty & is.na(value)] <- "is not yes or no"
  } else {
    value <- .nfc(vals)
    value[empty] <- NA
    if (type == "category" && length(cats)) {
      fold <- function(z) tolower(.fold_accents(trimws(z)))
      exact <- value %in% cats
      m <- match(fold(value), fold(cats))
      case_fixed <- !empty & !exact & !is.na(m)
      value[case_fixed] <- cats[m[case_fixed]]
      unknown <- !empty & !exact & is.na(m)
    }
  }
  list(value = value, problem = problem, case_fixed = case_fixed, unknown = unknown)
}

# Signs that Excel changed what was typed in a text column: a date serial
# (a five-digit number between 20000 and 60000, i.e. 1954-2064, or a cell
# Excel stores as a date), a number shown in scientific notation, and digit
# strings shorter than the zero-padded ones beside them (007 typed, 7 kept).
.gold_excel_damage <- function(vals, date_cells = NULL) {
  nz <- !is.na(vals) & nzchar(vals)
  num <- suppressWarnings(as.numeric(vals))
  serial <- nz & grepl("^[0-9]{5}$", vals) & !is.na(num) & num >= 20000 & num <= 60000
  if (!is.null(date_cells)) serial <- serial | (nz & date_cells)
  sci <- nz & grepl("^[-+]?[0-9]+(\\.[0-9]+)?[eE][-+]?[0-9]+$", vals)
  digits <- nz & grepl("^[0-9]+$", vals)
  padded <- digits & startsWith(vals, "0") & nchar(vals) > 1
  zeros <- logical(length(vals))
  if (any(padded)) zeros <- digits & !startsWith(vals, "0") & nchar(vals) < max(nchar(vals[padded]))
  list(serial = serial, sci = sci, zeros = zeros & !serial)
}

.gold_read_warnings <- function(label, unknown, image_files, conflicts, renumbered, case_fixed, off_list,
                                damage, empty_done) {
  if (length(unknown) && !is.null(image_files)) {
    .gold_tell(c(paste0(.gold_n(length(unknown), "file name"), " in ", label, " ",
                        if (length(unknown) == 1) "is" else "are", " not among the page images: ", .gold_list(unknown), "."),
                 "i" = paste0("The file column must hold the page image names (e.g. ", image_files[1], "); check the spelling.")),
               fun = .warn)
  }
  if (length(conflicts)) {
    sh <- utils::head(conflicts, 5)
    .gold_tell(c("Some page variables have different values on different lines of the same page; the first is used:",
                 stats::setNames(sh, rep("*", length(sh))),
                 if (length(conflicts) > 5) c("*" = paste0("... and ", length(conflicts) - 5, " more."))),
               fun = .warn)
  }
  if (length(renumbered)) {
    .gold_tell(c("i" = paste0("The row column was incomplete on ", .gold_list(renumbered),
                              "; rows were numbered in the order of the lines.")))
  }
  if (length(case_fixed)) {
    .gold_tell(c("i" = paste0("Matched ", .gold_n(length(case_fixed), "category value"),
                              " to the codebook's spelling (", .gold_list(unique(case_fixed), 3), ").")))
  }
  if (length(off_list)) {
    sh <- utils::head(off_list, 5)
    .gold_tell(c(paste0(.gold_n(length(off_list), "category value"), " in ", label,
                        " ", if (length(off_list) == 1) "is" else "are", " not in the codebook's list; kept as typed (the model cannot give ",
                        if (length(off_list) == 1) "it" else "them", ", so ", if (length(off_list) == 1) "it counts" else "they count",
                        " as a difference):"),
                 stats::setNames(sh, rep("*", length(sh))),
                 if (length(off_list) > 5) c("*" = paste0("... and ", length(off_list) - 5, " more."))),
               fun = .warn)
  }
  what <- c(serial = "look like dates Excel made out of what was typed (e.g. 1-2 or 3/4)",
            sci = "are long numbers Excel shows in scientific notation, which loses digits",
            zeros = "look like numbers that lost their leading zeros (others in the column start with 0)")
  msg <- character()
  for (kind in names(what)) {
    hits <- damage[[kind]]
    if (!length(hits)) next
    msg <- c(msg, "*" = paste0(.gold_n(length(hits), "value"), " ", what[[kind]], ": ", .gold_list(hits, 3), "."))
  }
  if (length(msg)) {
    .gold_tell(c(paste0("Excel may have changed some text values in ", label, ":"), msg,
                 "i" = "Retype them with an apostrophe first ('1-2, '007) or format the column as Text, then run the line again."),
               fun = .warn)
  }
  if (length(empty_done)) {
    .gold_tell(c(paste0(.gold_n(length(empty_done), "page"), " marked done in ", label, " ",
                        if (length(empty_done) == 1) "has" else "have", " no values at all: ", .gold_list(empty_done), "."),
                 "i" = "They count as pages with nothing written on them. If that is wrong, fill them in or mark them done = no."),
               fun = .warn)
  }
  invisible()
}

# ------- the editor's way back in -----------------------------------------------------

# Run the editor on a benchmark file, then read what was saved. Nothing
# marked done yet is not an error here: the typing is kept for next time.
.gold_after_app <- function(file, cb, pages) {
  .gold_app_run(file, cb, pages)
  tryCatch(.gold_read(file, cb, pages),
           amanuensis_error_gold_empty = function(e) {
             .inform(c("i" = "No page is marked done yet. What you typed is saved in {.file {(.show_path(file))}}; run the same line again to continue."))
             invisible(NULL)
           })
}

# ------- printing ---------------------------------------------------------------------

#' @export
print.amanuensis_gold <- function(x, ...) {
  info <- .gold_info(x)
  df <- as.data.frame(x)
  if (is.null(info)) {
    .print_table(df)
    return(invisible(x))
  }
  p <- info$pages
  v <- info$codebook$variables
  n_done <- sum(p$done)
  src <- if (is.na(info$source)) "" else paste0(" (", basename(info$source), ")")
  head <- paste0("Benchmark", src, ": ", .gold_n(n_done, "page"), " done",
                 if (nrow(p) > n_done) paste0(" of ", nrow(p)) else "")
  if (any(v$level == "row")) head <- paste0(head, ", ", .gold_n(sum(p$n_rows[p$done], na.rm = TRUE), "row"))
  if (info$codebook$kind == "text") {
    head <- paste0(head, ", free text")
  } else {
    head <- paste0(head, ", ", .gold_n(nrow(v), "variable"))
  }
  cat(head, "\n", sep = "")
  if (!is.null(info$draw)) {
    cat("Drawn with seed ", format(info$draw$seed, scientific = FALSE), " (", .gold_n(length(info$draw$files), "page"),
        " drawn", if (!is.na(info$draw$date)) paste0(" on ", substr(info$draw$date, 1, 10)) else "", ").\n", sep = "")
  } else {
    cat("Not drawn by benchmark() (a file of your own).\n")
  }
  if (!is.null(info$rows)) cat("Only the first ", info$rows, " rows of each page are compared.\n", sep = "")
  if (isTRUE(info$prefilled)) cat("Prefilled with the model's output: accuracy measured against it is optimistic.\n")
  .print_table(df)
  invisible(x)
}
