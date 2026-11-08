# The benchmark ("gold") object shared by benchmark() and score(): a data
# frame in the same layout as the results table (file, row, page variables,
# row variables; types converted to the codebook's), plus one attribute with
# what score() needs to know about how it was made.
#
#   codebook   the amanuensis_codebook it follows
#   pages      data.frame(file, done, n_rows): the pages it covers; only pages
#              marked done are scored
#   draw       list(seed, files, n, date, method) or NULL (user's own file)
#   rows       NULL, or N when only the first N rows of each page were
#              transcribed (score() then compares only those)
#   prefilled  TRUE when the benchmark started from the model's own output
#              (accuracy measured against it is optimistic)
#   source     the file it was read from

.new_gold <- function(tab, codebook, pages, draw = NULL, rows = NULL, prefilled = FALSE, source = NA_character_) {
  stopifnot(is.data.frame(tab), inherits(codebook, "amanuensis_codebook"), is.data.frame(pages))
  attr(tab, "amanuensis_gold") <- list(codebook = codebook, pages = pages, draw = draw, rows = rows,
                                       prefilled = isTRUE(prefilled), source = source)
  class(tab) <- c("amanuensis_gold", "data.frame")
  tab
}

.gold_info <- function(x) attr(x, "amanuensis_gold", exact = TRUE)

#' @export
as.data.frame.amanuensis_gold <- function(x, ...) {
  attr(x, "amanuensis_gold") <- NULL
  class(x) <- "data.frame"
  x
}
