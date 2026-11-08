# example_project(): the bundled example (inst/extdata/example, built by
# data-raw/make-example.R) copied into a folder of the user's. The files are
# copied rather than used where they are installed because every later step
# writes next to its input (page images, results, a gold file), and the
# package library is read-only on many computers, is replaced on every
# update, and must never be written to (CRAN policy; images() refuses it).
#
# The default folder is inside tempdir(), so trying the example leaves
# nothing behind and needs no question. Files already in the folder are kept
# unless overwrite = TRUE, so a codebook or gold file the user has edited
# survives running the line again. replies.json, which the "example" model
# reads from the installed package, and the README that lists the planted
# errors are not copied: the exercise is to find the errors with score().

.example_files <- c(scans = "padro1930.pdf", instructions = "instructions.md", codebook = "codebook.xlsx",
                    codebook_csv = "codebook.csv", gold = "gold.xlsx")

#' Copy the example project to practise with
#'
#' Copies a small example project into a folder: a made-up 1930 municipal
#' register of three pages (a scanned PDF), instructions for the model, a
#' codebook, and a benchmark (gold standard) transcribed by hand. Use it to
#' try every step of the workflow before working with your own sources. With
#' `model = "example"`, [transcribe()] replays prepared replies for these
#' pages, so nothing needs a key and nothing costs money.
#'
#' The town, the people and the document are invented. The prepared replies
#' are not real Gemini output: they are the benchmark with a few typical
#' mistakes planted on purpose (a misread age, a dropped accent, a skipped
#' row, a "corrected" spelling and others), so that [score()] has something
#' to find.
#'
#' The files copied are `padro1930.pdf` (the scans), `instructions.md`,
#' `codebook.xlsx` (with the same codebook as `codebook.csv`) and
#' `gold.xlsx` (the benchmark). Open them to see what a finished project
#' looks like: the codebook and the benchmark are ordinary Excel files.
#'
#' @param save_to Folder to copy the files into; it is created if needed.
#'   Default: a folder in R's temporary folder, which is deleted when R
#'   closes. Give a folder of your own to keep the files, e.g.
#'   `"C:/research/amanuensis-example"`.
#' @param overwrite `FALSE` (default) keeps example files that are already in
#'   `save_to`, for example a codebook you have changed. `TRUE` replaces them
#'   with fresh copies.
#' @return A list of file paths, invisibly: `folder` (the folder),
#'   `scans` (the PDF), `instructions`, `codebook` (the Excel codebook),
#'   `codebook_csv` (the same codebook as CSV) and `gold` (the benchmark).
#'   A message lists the steps to try next.
#' @seealso [transcribe()] with `model = "example"`; [score()].
#' @export
#' @examples
#' ex <- example_project()
#' ex$scans
#' list.files(ex$folder)
#'
#' # the first steps of the workflow, without a key or any cost
#' pages <- images(ex$scans, save_to = file.path(ex$folder, "pages"))
#' res <- transcribe(pages, ex$instructions, ex$codebook, model = "example")
example_project <- function(save_to = file.path(tempdir(), "amanuensis-example"), overwrite = FALSE) {
  .check_string(save_to, arg = "save_to")
  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    .abort("{.arg overwrite} must be {.code TRUE} or {.code FALSE}.", class = "amanuensis_error_input")
  }
  src <- system.file("extdata", "example", package = "amanuensis")
  from <- file.path(src, .example_files)
  if (!nzchar(src) || !all(file.exists(from))) {
    .abort(c("The example files are missing from the installed package.",
             "i" = "Reinstall amanuensis, restart R and try again."),
           class = "amanuensis_error_input")
  }
  if (file.exists(save_to) && !dir.exists(save_to)) {
    .abort(c("{.path {(.show_path(save_to))}} is a file, not a folder.",
             "i" = "Choose a folder with {.arg save_to}."), class = "amanuensis_error_input")
  }
  .refuse_library_path(save_to)
  if (!dir.exists(save_to) && !dir.create(save_to, recursive = TRUE, showWarnings = FALSE)) {
    .abort(c("Could not create the folder {.path {(.show_path(save_to))}}.",
             "i" = "Check the path, or choose another folder with {.arg save_to}."),
           class = "amanuensis_error_input")
  }
  folder <- .show_path(save_to)
  to <- file.path(folder, .example_files)
  kept <- file.exists(to) & !isTRUE(overwrite)
  for (i in which(!kept)) {
    if (!file.copy(from[i], to[i], overwrite = TRUE)) .abort_unwritable(to[i])
  }
  out <- c(list(folder = folder), stats::setNames(as.list(to), names(.example_files)))

  n_kept <- sum(kept)
  done <- if (n_kept == length(to)) {
    c("v" = "The example project is already in {.path {folder}}; its files were kept.",
      "i" = "Use {.code overwrite = TRUE} for fresh copies.")
  } else if (n_kept) {
    c("v" = "Copied the example project to {.path {folder}}.",
      "i" = "Kept {n_kept} file{?s} already there ({.file {basename(to[kept])}}); {.code overwrite = TRUE} replaces {cli::qty(n_kept)}{?it/them}.")
  } else {
    c("v" = "Copied the example project to {.path {folder}}.")
  }
  .inform(c(done,
    "i" = "A made-up 1930 municipal register of 3 scanned pages, with instructions, a codebook and a benchmark (gold standard) made by hand.",
    "i" = "Try each step, with the result saved as {.code ex <- example_project()}:",
    " " = "{.code pages <- images(ex$scans, save_to = file.path(ex$folder, \"pages\"))}",
    " " = "{.code res <- transcribe(pages, ex$instructions, ex$codebook, model = \"example\")}",
    " " = "{.code output(res, file.path(ex$folder, \"padro1930.xlsx\"))}",
    " " = "{.code gold <- benchmark(file = ex$gold)}",
    " " = "{.code score(gold, res)}",
    "i" = "{.code model = \"example\"} needs no key and costs nothing: it replays prepared replies with a few planted mistakes for {.fn score} to find."))
  invisible(out)
}
