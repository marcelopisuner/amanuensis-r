#' amanuensis: turn scanned historical documents into data with Gemini
#'
#' A vision language model (Google Gemini) reads each page image and returns
#' records that follow a codebook you write in a spreadsheet. The workflow:
#'
#' 1. [set_key()] once, to store your Gemini API key ([check_setup()] checks
#'    everything).
#' 2. [images()] to turn PDFs, TIFFs, photos and scans into page images.
#' 3. [instructions()] to say what the source is and what to do;
#'    [codebook()] to list the variables you want.
#' 4. [transcribe()] to read every page. The results come back to R and
#'    nothing is written to your computer; a stopped run keeps the pages
#'    read, and `save_to` saves each page as it arrives ([gemini_models()]
#'    lists models and prices).
#' 5. [output()] to save the results as Excel, CSV, Stata, Word, text, JSON
#'    or RDS.
#' 6. [benchmark()] and [score()] to measure accuracy against your own
#'    transcription of a few pages.
#'
#' [example_project()] copies a small example to practise with, and
#' `model = "example"` or `model = "mock"` let you run everything without a
#' key or any cost.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||%
## usethis namespace: end
NULL
