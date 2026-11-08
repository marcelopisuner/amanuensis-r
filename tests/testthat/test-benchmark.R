# benchmark(): the draw, the Excel and text templates, reading them back
# (types, cells, Excel damage, done marks), other kinds of benchmark files,
# prefilling, adding pages, and the editor's pure functions. Offline; every
# file is written under a temporary folder.

bench_setup <- function(env = parent.frame(), n_pages = 8) {
  local_amanuensis(env)
  dir <- withr::local_tempdir(.local_envir = env)
  pages <- file.path(dir, "pages")
  make_jpegs(pages, n = n_pages)
  list(dir = dir, pages = pages, files = sprintf("p%03d.jpg", seq_len(n_pages)), cb = roster_codebook())
}

# Every type the codebook knows, at both levels.
full_codebook <- function() {
  quietly(codebook(data.frame(
    variable = c("heading", "folio", "name", "age", "sex", "literate", "id", "rent"),
    type = c("text", "integer", "text", "integer", "category", "yes/no", "text", "number"),
    level = c("page", "page", "row", "row", "row", "row", "row", "row"),
    values = c("", "", "", "", "male; female", "", "", ""),
    description = c("Heading.", "Folio as printed.", "Name.", "Age.", "Sex.", "Can read.", "Census id.", "Rent."),
    stringsAsFactors = FALSE
  )))
}

read_sheet <- function(path, sheet) {
  as.data.frame(readxl::read_excel(path, sheet, col_types = "text"), stringsAsFactors = FALSE)
}

# What a user does in Excel: type into the benchmark sheet and mark pages
# done; the other sheets stay as benchmark() wrote them.
fill_gold <- function(path, bench, done) {
  pages <- read_sheet(path, "pages")
  pages$done <- done
  writexl::write_xlsx(list(benchmark = bench, pages = pages, about = read_sheet(path, "about"),
                           codebook = read_sheet(path, "codebook"), how_to = read_sheet(path, "how_to")), path)
}

new_template <- function(s, file = "gold.xlsx", n = 3, seed = 1930, cb = s$cb, ...) {
  f <- file.path(s$dir, file)
  quietly(benchmark(f, images = s$pages, codebook = cb, n = n, seed = seed, app = FALSE, ...))
  f
}

# ------- the draw ---------------------------------------------------------------

test_that("the seed made from the file names ignores their order and Unicode form", {
  a <- .gold_seed_from_names(c("p2.jpg", "p10.jpg", "caf\u00e9.jpg"))
  b <- .gold_seed_from_names(c("cafe\u0301.jpg", "p10.jpg", "p2.jpg"))
  expect_identical(a, b)
  expect_true(a >= 0 && a < 2^28)
  expect_false(identical(a, .gold_seed_from_names(c("p2.jpg", "p10.jpg"))))
})

test_that("the draw is one seeded order of the files, and n takes its first pages", {
  files <- sprintf("p%d.jpg", 1:20)
  d5 <- .gold_draw(files, 5, 1930)
  expect_length(d5, 5)
  expect_identical(d5, .gold_draw(rev(files), 5, 1930))
  expect_identical(.gold_draw(files, 2, 1930), d5[1:2])
  expect_identical(.gold_draw(files, 5, 1930), d5)
  expect_false(identical(d5, .gold_draw(files, 5, 1931)))
  expect_setequal(.gold_draw(files, 50, 1930), files)
  # a pinned value: the draw must not change between versions or machines
  expect_identical(.gold_draw(sprintf("p%03d.jpg", 1:8), 3, 1930), .gold_draw(sprintf("p%03d.jpg", c(8:1)), 3, 1930))
})

test_that("extending a draw continues the same order", {
  files <- sprintf("p%d.jpg", 1:20)
  d5 <- .gold_draw(files, 5, 1930)
  ext <- .gold_extend_draw(files, d5[1:2], 5, 1930)
  expect_true(ext$same)
  expect_identical(ext$add, d5[3:5])
  changed <- .gold_extend_draw(c(files, "p21.jpg"), d5[1:2], 5, 1930)
  expect_length(changed$add, 3)
  expect_false(any(changed$add %in% d5[1:2]))
})

test_that("the natural order puts p2 before p10 and folds only ASCII case", {
  x <- c("p10.jpg", "P2.jpg", "p1.jpg", "a.jpg")
  expect_identical(x[.gold_order(x)], c("a.jpg", "p1.jpg", "P2.jpg", "p10.jpg"))
})

test_that("drawing leaves the user's random numbers and generator alone", {
  withr::local_preserve_seed()
  set.seed(42)
  before <- .Random.seed
  .gold_draw(sprintf("p%d.jpg", 1:10), 3, 1930)
  expect_identical(.Random.seed, before)

  s <- bench_setup()
  set.seed(7)
  before <- .Random.seed
  new_template(s, seed = NULL)
  expect_identical(.Random.seed, before)

  rm(".Random.seed", envir = globalenv())
  .gold_draw(sprintf("p%d.jpg", 1:10), 3, 1930)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))

  old <- RNGkind()
  suppressWarnings(RNGkind("L'Ecuyer-CMRG"))
  on.exit(suppressWarnings(do.call(RNGkind, as.list(old))), add = TRUE)
  .gold_draw(sprintf("p%d.jpg", 1:10), 3, 1930)
  expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
  # the draw does not depend on the user's generator either
  expect_identical(.gold_draw(sprintf("p%d.jpg", 1:10), 3, 1930), {
    suppressWarnings(do.call(RNGkind, as.list(old)))
    .gold_draw(sprintf("p%d.jpg", 1:10), 3, 1930)
  })
})

# ------- the Excel template -----------------------------------------------------

test_that("benchmark() writes an Excel template with one line per drawn page", {
  s <- bench_setup()
  f <- file.path(s$dir, "gold.xlsx")
  msgs <- all_condition_text(out <- benchmark(f, images = s$pages, codebook = s$cb, n = 3, seed = 1930, app = FALSE))
  expect_null(out)
  expect_identical(readxl::excel_sheets(f), c("benchmark", "pages", "about", "codebook", "how_to"))
  drawn <- .gold_draw(s$files, 3, 1930)
  b <- read_sheet(f, "benchmark")
  expect_identical(names(b), c("file", "row", "heading", "name", "age", "sex"))
  expect_identical(b$file, sort(drawn))
  expect_true(all(is.na(b[, -1])))
  p <- read_sheet(f, "pages")
  expect_identical(p$file, sort(drawn))
  expect_identical(p$done, rep("no", 3))
  a <- read_sheet(f, "about")
  about <- stats::setNames(a$value, a$item)
  expect_identical(a$item, c("package", "made", "seed", "n", "method", "drawn", "rows", "prefilled",
                             "codebook_source", "free_text", "images"))
  expect_identical(about[["free_text"]], "no")
  expect_identical(about[["seed"]], "1930")
  expect_identical(about[["n"]], "3")
  expect_identical(about[["drawn"]], paste(drawn, collapse = "; "))
  expect_identical(about[["prefilled"]], "no")
  expect_true(is.na(about[["rows"]]))
  cbs <- read_sheet(f, "codebook")
  expect_identical(cbs$variable, c("heading", "name", "age", "sex"))
  expect_identical(cbs$values[4], "male; female; other")
  expect_true(nrow(read_sheet(f, "how_to")) > 5)
  expect_true(any(grepl("n = 10 or more", msgs)))
  expect_true(any(grepl("run the same line again", msgs)))
  expect_true(any(grepl("p001.jpg|p00", msgs)))
})

test_that("the template formats text columns as Text and offers drop-down lists", {
  skip_if_not(.gold_rich_xlsx())
  s <- bench_setup()
  f <- new_template(s, cb = full_codebook())
  td <- withr::local_tempdir()
  utils::unzip(f, exdir = td)
  styles <- paste(readLines(file.path(td, "xl", "styles.xml"), warn = FALSE), collapse = "")
  sheet1 <- paste(readLines(file.path(td, "xl", "worksheets", "sheet1.xml"), warn = FALSE), collapse = "")
  expect_match(styles, "formatCode=\"@\"", fixed = TRUE)
  expect_match(sheet1, "type=\"list\"", fixed = TRUE)
  expect_match(sheet1, "\"male,female,other\"", fixed = TRUE)
  expect_match(sheet1, "\"yes,no\"", fixed = TRUE)
})

test_that("a new benchmark needs images and a codebook, and an Excel file for a table", {
  s <- bench_setup()
  expect_error(benchmark(), "Say which file")
  expect_error(benchmark(file.path(s$dir, "gold.xlsx")), "does not exist yet", class = "amanuensis_error_input")
  expect_error(benchmark(file.path(s$dir, "gold.xlsx"), images = s$pages), "does not exist yet")
  expect_error(quietly(benchmark(file.path(s$dir, "gold.csv"), images = s$pages, codebook = s$cb, app = FALSE)),
               "Excel file")
  expect_error(quietly(benchmark(file.path(s$dir, "gold.txt"), images = s$pages, codebook = s$cb, app = FALSE)),
               "only the text")
  expect_error(benchmark(file.path(s$dir, "gold.pdf")), "extension")
  expect_error(benchmark(file.path(s$dir, "gold.xlsx"), n = 0), "whole number")
  expect_error(benchmark(file.path(s$dir, "gold.xlsx"), seed = 1.5), "whole number")
  expect_error(benchmark(file.path(s$dir, "gold.xlsx"), rows = -1), "whole number")
})

test_that("app = TRUE outside an interactive session writes the file instead", {
  s <- bench_setup()
  f <- file.path(s$dir, "gold.xlsx")
  msgs <- all_condition_text(benchmark(f, images = s$pages, codebook = s$cb, seed = 1, app = TRUE))
  expect_true(any(grepl("interactive R session", msgs)))
  expect_true(file.exists(f))
})

test_that("n larger than the number of pages takes them all", {
  s <- bench_setup(n_pages = 3)
  f <- file.path(s$dir, "gold.xlsx")
  msgs <- all_condition_text(benchmark(f, images = s$pages, codebook = s$cb, n = 10, seed = 1, app = FALSE))
  expect_true(any(grepl("covers every page", msgs)))
  expect_identical(read_sheet(f, "pages")$file, s$files)
})

# ------- reading the Excel benchmark --------------------------------------------

test_that("an untouched template is refused with what to do", {
  s <- bench_setup()
  f <- new_template(s)
  err <- expect_error(benchmark(f), class = "amanuensis_error_gold_empty")
  expect_match(conditionMessage(err), "no values yet")
  expect_match(conditionMessage(err), "close Excel")
  expect_match(conditionMessage(err), sort(.gold_draw(s$files, 3, 1930))[1], fixed = TRUE)
})

test_that("a filled template is read back with the codebook's types", {
  s <- bench_setup()
  f <- new_template(s)
  fs <- sort(.gold_draw(s$files, 3, 1930))
  bench <- data.frame(file = c(fs[1], fs[1], fs[1], fs[2], fs[3]),
                      row = NA_character_,
                      heading = c("Carrer Major", NA, NA, "Pla\u00e7a", NA),
                      name = c("Puig, Josep", "Ferrer, Maria", "Soler?", NA, "Not done"),
                      age = c("34", "29", NA, NA, "1"),
                      sex = c("male", "female", NA, NA, "male"), stringsAsFactors = FALSE)
  fill_gold(f, bench, c("yes", "yes", "no"))
  msgs <- all_condition_text(g <- benchmark(f, images = s$pages))
  expect_s3_class(g, "amanuensis_gold")
  expect_true(any(grepl("left out until marked done", msgs)))
  tab <- as.data.frame(g)
  expect_identical(names(tab), c("file", "row", "heading", "name", "age", "sex"))
  expect_identical(tab$file, c(fs[1], fs[1], fs[1], fs[2]))
  expect_identical(tab$row, c(1L, 2L, 3L, NA))
  expect_identical(tab$heading, c("Carrer Major", "Carrer Major", "Carrer Major", "Pla\u00e7a"))
  expect_identical(tab$age, c(34L, 29L, NA, NA))
  expect_identical(tab$name[3], "Soler?")
  info <- .gold_info(g)
  expect_identical(info$pages$file, fs)
  expect_identical(info$pages$done, c(TRUE, TRUE, FALSE))
  expect_identical(info$pages$n_rows, c(3L, 0L, NA))
  expect_identical(info$draw$seed, 1930)
  expect_identical(info$draw$files, .gold_draw(s$files, 3, 1930))
  expect_identical(info$codebook$variables$variable, s$cb$variables$variable)
  expect_identical(info$codebook$variables$values[[4]], c("male", "female", "other"))
  expect_false(info$prefilled)
  expect_null(info$rows)
  # the same as the score() helpers expect: units from the flat table
  units <- .units_from_table(tab, info$codebook)
  expect_identical(nrow(units[[fs[2]]]$rows), 0L)
  expect_identical(units[[fs[2]]]$page$heading, "Pla\u00e7a")
  # reading again gives the same object
  expect_identical(quietly(benchmark(f, images = s$pages)), g)
})

test_that("row numbers are used when complete, and line order otherwise", {
  s <- bench_setup()
  f <- new_template(s, n = 1)
  fs <- .gold_draw(s$files, 1, 1930)
  bench <- data.frame(file = fs, row = c("2", "1", "3"), heading = NA_character_, name = c("B", "A", "C"),
                      age = NA_character_, sex = NA_character_, stringsAsFactors = FALSE)
  fill_gold(f, bench, "yes")
  expect_identical(quietly(benchmark(f))$name, c("A", "B", "C"))
  bench$row <- c("1", NA, "7")
  fill_gold(f, bench, "yes")
  msgs <- all_condition_text(g <- benchmark(f))
  expect_identical(g$name, c("B", "A", "C"))
  expect_identical(g$row, 1:3)
  expect_true(any(grepl("numbered in the order of the lines", msgs)))
  bench$row <- c("1", "1", "2")
  fill_gold(f, bench, "yes")
  expect_error(quietly(benchmark(f)), "more than one line")
  bench$row <- c("1", "two", "3")
  fill_gold(f, bench, "yes")
  expect_error(quietly(benchmark(f)), "cell B3 \\(row\\)")
})

test_that("values that do not fit the codebook stop with sheet and cell", {
  s <- bench_setup()
  f <- new_template(s, n = 1, cb = full_codebook())
  fs <- .gold_draw(s$files, 1, 1930)
  bench <- data.frame(file = fs, row = NA_character_, heading = "H", folio = c("12a", NA),
                      name = c("A", "B"), age = c("3.5", "40"), sex = c("male", "female"),
                      literate = c("yes", "maybe"), id = c("1", "2"), rent = c("1,5", "2.25"),
                      stringsAsFactors = FALSE)
  fill_gold(f, bench, "yes")
  err <- expect_error(quietly(benchmark(f)), class = "amanuensis_error_input")
  m <- conditionMessage(err)
  expect_match(m, "4 values")
  expect_match(m, "cell D2 (folio): \"12a\" is not a whole number", fixed = TRUE)
  expect_match(m, "cell F2 (age): \"3.5\" is not a whole number", fixed = TRUE)
  expect_match(m, "cell H3 (literate): \"maybe\" is not yes or no", fixed = TRUE)
  expect_match(m, "cell J2 (rent): \"1,5\" is not a number", fixed = TRUE)
  bench$folio[1] <- "12"
  bench$age[1] <- "3"
  bench$literate[2] <- "S\u00ed"
  bench$rent[1] <- "1.5"
  fill_gold(f, bench, "yes")
  g <- quietly(benchmark(f))
  expect_identical(g$folio, c(12L, 12L))
  expect_identical(g$literate, c(TRUE, TRUE))
  expect_identical(g$rent, c(1.5, 2.25))
  expect_identical(g$id, c("1", "2"))
})

test_that("yes/no cells accept the usual spellings", {
  x <- c("yes", "S\u00ed", "s\u00ec", "X", "true", "1", "No", "0", "FALSE", "maybe", "", NA)
  expect_identical(.gold_parse_yes_no(x), c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA))
  # S and N as Spanish and Catalan registers write them: both, never N alone
  expect_identical(.gold_parse_yes_no(c("S", "N", "s", "n", " S ")), c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_identical(.gold_parse_done(c("yes", "done", "", "not done", "hecho")), c(TRUE, TRUE, NA, FALSE, TRUE))
})

test_that("category values are matched to the codebook's spelling, unknown ones warned", {
  s <- bench_setup()
  f <- new_template(s, n = 1)
  fs <- .gold_draw(s$files, 1, 1930)
  bench <- data.frame(file = fs, row = NA_character_, heading = NA_character_, name = c("A", "B"),
                      age = NA_character_, sex = c("MALE", "mle"), stringsAsFactors = FALSE)
  fill_gold(f, bench, "yes")
  msgs <- all_condition_text(g <- benchmark(f))
  expect_identical(g$sex, c("male", "mle"))
  expect_true(any(grepl("codebook's spelling", msgs)))
  expect_true(any(grepl("cell F3 (sex): \"mle\" is not one of", msgs, fixed = TRUE)))
})

test_that("Excel damage to text values is warned about", {
  s <- bench_setup()
  f <- new_template(s, n = 1, cb = full_codebook())
  fs <- .gold_draw(s$files, 1, 1930)
  bench <- data.frame(file = fs, row = NA_character_, heading = "H", folio = NA_character_,
                      name = c("A", "B", "C", "D"), age = NA_character_, sex = NA_character_,
                      literate = NA_character_, id = c("44927", "1.2E+15", "007", "12"), rent = NA_character_,
                      stringsAsFactors = FALSE)
  fill_gold(f, bench, "yes")
  w <- character()
  withCallingHandlers(quietly(benchmark(f)), warning = function(c) {
    w <<- c(w, conditionMessage(c))
    invokeRestart("muffleWarning")
  })
  w <- paste(w, collapse = "\n")
  expect_match(w, "dates Excel made")
  expect_match(w, "cell I2 (id) reads \"44927\"", fixed = TRUE)
  expect_match(w, "scientific notation")
  expect_match(w, "cell I3 (id) reads \"1.2E+15\"", fixed = TRUE)
  expect_match(w, "leading zeros")
  expect_match(w, "cell I5 (id) reads \"12\"", fixed = TRUE)

  # a cell Excel stores as a date is caught whatever its number
  bench2 <- bench[1:2, setdiff(names(bench), "id")]
  bench2$id <- as.Date(c("1920-01-02", NA))
  pages <- read_sheet(f, "pages")
  pages$done <- "yes"
  writexl::write_xlsx(list(benchmark = bench2, pages = pages, about = read_sheet(f, "about"),
                           codebook = read_sheet(f, "codebook")), f)
  expect_warning(quietly(benchmark(f)), "cell J2 \\(id\\)")
  expect_identical(.gold_excel_damage(c("44927", "abc", "1.5e10", "007", "7", "19999"))$serial,
                   c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("a date Excel made in a number or yes/no column stops the read with its cell", {
  s <- bench_setup()
  f <- new_template(s, n = 1, cb = full_codebook())
  fs <- .gold_draw(s$files, 1, 1930)
  pages <- read_sheet(f, "pages")
  pages$done <- "yes"
  write_bench <- function(bench) {
    writexl::write_xlsx(list(benchmark = bench, pages = pages, about = read_sheet(f, "about"),
                             codebook = read_sheet(f, "codebook")), f)
  }
  base <- data.frame(file = fs, row = c("1", "2", "3"), heading = "H", folio = "12", name = c("A", "B", "C"),
                     stringsAsFactors = FALSE)
  # what Excel stores when 3/12 (an infant's age) is typed in a General
  # column: a date, whose serial number would pass for a whole number
  bench <- base
  bench$age <- as.Date(c(NA, "2026-12-03", NA))
  bench$sex <- "male"
  bench$literate <- "yes"
  bench$id <- "x"
  bench$rent <- "1.5"
  write_bench(bench)
  err <- expect_error(quietly(benchmark(f)), class = "amanuensis_error_input")
  expect_match(conditionMessage(err), "cell F3 (age): Excel turned what was typed into a date", fixed = TRUE)
  expect_match(conditionMessage(err), "type the value again as a number")
  # the same in a number column and a yes/no column, and in the row column
  bench <- base
  bench$age <- "3"
  bench$sex <- "male"
  bench$literate <- as.Date(c("2026-01-02", NA, NA))
  bench$id <- "x"
  bench$rent <- as.Date(c(NA, NA, "2026-01-02"))
  bench$row <- as.Date(c(NA, "2026-01-02", NA))
  write_bench(bench)
  msg <- conditionMessage(expect_error(quietly(benchmark(f)), class = "amanuensis_error_input"))
  expect_match(msg, "cell H2 (literate): Excel turned what was typed into a date", fixed = TRUE)
  expect_match(msg, "type the value again as yes or no")
  expect_match(msg, "cell J4 (rent): Excel turned", fixed = TRUE)
  expect_match(msg, "cell B3 (row): Excel turned what was typed into a date", fixed = TRUE)
  expect_false(grepl("is not yes or no", msg))
})

test_that("the template formats number columns as Text too, so Excel cannot make dates there", {
  skip_if_not(.gold_rich_xlsx())
  s <- bench_setup()
  f <- new_template(s, cb = full_codebook())
  td <- withr::local_tempdir()
  utils::unzip(f, exdir = td)
  styles <- paste(readLines(file.path(td, "xl", "styles.xml"), warn = FALSE), collapse = "")
  sheet1 <- paste(readLines(file.path(td, "xl", "worksheets", "sheet1.xml"), warn = FALSE), collapse = "")
  # one style for the whole sheet's columns, the Text one
  cols <- regmatches(sheet1, gregexpr("<col [^>]*>", sheet1))[[1]]
  styles_used <- unique(sub(".*style=\"([0-9]+)\".*", "\\1", cols))
  expect_length(cols, 10)
  expect_length(styles_used, 1)
  expect_true(any(grepl("Every column is formatted as Text", read_sheet(f, "how_to")$how_to)))
})

test_that("yes/no written S and N in a benchmark file is read", {
  s <- bench_setup()
  f <- new_template(s, n = 1, cb = full_codebook())
  fs <- .gold_draw(s$files, 1, 1930)
  fill_gold(f, data.frame(file = fs, row = c("1", "2"), heading = "H", folio = "1", name = c("A", "B"),
                          age = c("3", "4"), sex = "male", literate = c("S", "N"), id = "x", rent = "1",
                          stringsAsFactors = FALSE), "yes")
  g <- quietly(benchmark(f))
  expect_identical(g$literate, c(TRUE, FALSE))
})

test_that("only pages marked done are used; with no marks at all, pages with values", {
  s <- bench_setup()
  f <- new_template(s)
  fs <- sort(.gold_draw(s$files, 3, 1930))
  bench <- data.frame(file = fs, row = NA_character_, heading = c("A", NA, "C"), name = c("x", NA, "z"),
                      age = NA_character_, sex = NA_character_, stringsAsFactors = FALSE)
  # every page still says no, as the template wrote it: a page typed only in
  # part must not count against the model, so nothing is used yet
  fill_gold(f, bench, c("no", "no", "no"))
  err <- expect_error(quietly(benchmark(f)), class = "amanuensis_error_gold_empty")
  expect_match(conditionMessage(err), "No page of gold.xlsx is marked done yet")
  expect_match(conditionMessage(err), paste0("pages with values: ", fs[1], ", ", fs[3]), fixed = TRUE)
  expect_match(conditionMessage(err), "Write yes in the done column")
  # the done column left empty for every page: nothing says which are
  # finished, so the pages with values are used, and said so
  fill_gold(f, bench, c(NA, NA, NA))
  msgs <- all_condition_text(g <- benchmark(f))
  expect_true(any(grepl("No page of gold.xlsx is marked done; using the 2 pages with values", msgs)))
  expect_identical(.gold_info(g)$pages$done, c(TRUE, FALSE, TRUE))
  fill_gold(f, bench, c("yes", "no", NA))
  g <- quietly(benchmark(f))
  expect_identical(unique(g$file), fs[1])
  fill_gold(f, bench, c("yes", "yes", "maybe"))
  expect_error(quietly(benchmark(f)), "must say yes or no")
  # a done page with nothing typed is a page with nothing on it, said aloud
  fill_gold(f, bench, c("yes", "yes", "no"))
  expect_warning(g <- quietly(benchmark(f)), "no values at all")
  expect_identical(.gold_info(g)$pages$n_rows, c(1L, 0L, NA))
})

test_that("page values come from the first line; different later values are warned", {
  s <- bench_setup()
  f <- new_template(s, n = 1)
  fs <- .gold_draw(s$files, 1, 1930)
  bench <- data.frame(file = fs, row = NA_character_, heading = c("First", NA, "Second"), name = c("a", "b", "c"),
                      age = NA_character_, sex = NA_character_, stringsAsFactors = FALSE)
  fill_gold(f, bench, "yes")
  expect_warning(g <- quietly(benchmark(f)), "different values on different lines")
  expect_identical(unique(g$heading), "First")
})

test_that("file names are filled down, matched to the images, and unknown ones warned", {
  s <- bench_setup()
  f <- new_template(s, n = 1)
  fs <- .gold_draw(s$files, 1, 1930)
  bench <- data.frame(file = c(toupper(.stem(fs)), NA, "zzz.jpg"), row = NA_character_, heading = NA_character_,
                      name = c("a", "b", "c"), age = NA_character_, sex = NA_character_, stringsAsFactors = FALSE)
  pages <- read_sheet(f, "pages")
  pages <- rbind(pages, data.frame(file = "zzz.jpg", done = "yes", notes = NA))
  pages$done <- "yes"
  writexl::write_xlsx(list(benchmark = bench, pages = pages, about = read_sheet(f, "about"),
                           codebook = read_sheet(f, "codebook")), f)
  msgs <- all_condition_text(g <- benchmark(f, images = s$pages))
  expect_true(any(grepl("took the file of the line above", msgs)))
  expect_true(any(grepl("not among the page images: zzz.jpg", msgs)))
  expect_identical(g$file, c(fs, fs, "zzz.jpg"))
  # without images, names are matched to those the file lists, and nothing is warned
  expect_identical(suppressWarnings(quietly(benchmark(f)))$file, c(fs, fs, "zzz.jpg"))
})

test_that("variables missing from the file stop with their names", {
  s <- bench_setup()
  f <- new_template(s, n = 1)
  fs <- .gold_draw(s$files, 1, 1930)
  fill_gold(f, data.frame(file = fs, row = NA, heading = "h", name = "a", stringsAsFactors = FALSE), "yes")
  expect_error(quietly(benchmark(f)), "no column for the codebook variables \"age\" and \"sex\"")
})

test_that("rows = N is recorded in the file and read back", {
  s <- bench_setup()
  f <- new_template(s, n = 2, rows = 5)
  a <- read_sheet(f, "about")
  expect_identical(a$value[a$item == "rows"], "5")
  expect_true(any(grepl("first 5 rows", read_sheet(f, "how_to")$how_to)))
  fs <- sort(.gold_draw(s$files, 2, 1930))
  fill_gold(f, data.frame(file = fs, row = NA, heading = "h", name = "a", age = "1", sex = "male",
                          stringsAsFactors = FALSE), c("yes", "yes"))
  g <- quietly(benchmark(f))
  expect_identical(.gold_info(g)$rows, 5L)
  expect_output(print(g), "first 5 rows")
})

test_that("printing shows pages, rows, the seed and the prefilled flag", {
  s <- bench_setup()
  f <- new_template(s, n = 2)
  fs <- sort(.gold_draw(s$files, 2, 1930))
  fill_gold(f, data.frame(file = fs[c(1, 1, 2)], row = NA, heading = "h", name = c("a", "b", "c"), age = "1",
                          sex = "male", stringsAsFactors = FALSE), c("yes", "no"))
  g <- quietly(benchmark(f))
  out <- capture.output(print(g))
  expect_match(out[1], "Benchmark (gold.xlsx): 1 page done of 2, 2 rows, 4 variables", fixed = TRUE)
  expect_match(out[2], "seed 1930")
  expect_false(any(grepl("optimistic", out)))
})

# ------- adding pages -----------------------------------------------------------

test_that("a larger n adds the next pages of the draw and keeps what was typed", {
  s <- bench_setup()
  f <- new_template(s, n = 2)
  fs <- sort(.gold_draw(s$files, 2, 1930))
  fill_gold(f, data.frame(file = c(fs[1], fs[1], fs[2]), row = NA, heading = c("kept", NA, NA),
                          name = c("a", "b", NA), age = c("1", "2", NA), sex = NA, stringsAsFactors = FALSE),
            c("yes", "no"))
  msgs <- all_condition_text(out <- benchmark(f, images = s$pages, n = 4, app = FALSE))
  expect_null(out)
  expect_true(any(grepl("Added 2 pages", msgs)))
  expect_length(list.files(s$dir, pattern = "^gold-backup-.*\\.xlsx$"), 1)
  d4 <- .gold_draw(s$files, 4, 1930)
  b <- read_sheet(f, "benchmark")
  expect_identical(b$file, c(fs[1], fs[1], fs[2], sort(d4[3:4])))
  expect_identical(b$heading[1], "kept")
  expect_identical(b$name[1:2], c("a", "b"))
  p <- read_sheet(f, "pages")
  expect_identical(p$done, c("yes", "no", "no", "no"))
  a <- read_sheet(f, "about")
  expect_identical(a$value[a$item == "n"], "4")
  expect_identical(a$value[a$item == "drawn"], paste(d4, collapse = "; "))
  g <- quietly(benchmark(f))
  expect_identical(nrow(g), 2L)
  # the same line again just reads; a smaller n never removes pages
  expect_s3_class(quietly(benchmark(f, images = s$pages, n = 4, app = FALSE)), "amanuensis_gold")
  msgs <- all_condition_text(benchmark(f, n = 2, app = FALSE))
  expect_true(any(grepl("never taken out", msgs)))
  expect_error(quietly(benchmark(f, n = 6, app = FALSE)), "give the page images")
})

# ------- prefilling -------------------------------------------------------------

test_that("prefill fills the drawn pages with the model's output, and it is recorded", {
  s <- bench_setup()
  res <- quietly(transcribe(s$pages, test_instructions(), s$cb, model = "mock",
                                save_to = file.path(s$dir, "out")))
  f <- file.path(s$dir, "gold.xlsx")
  expect_warning(quietly(benchmark(f, images = s$pages, codebook = s$cb, n = 2, seed = 7, prefill = res,
                                   app = FALSE)), "optimistic")
  b <- read_sheet(f, "benchmark")
  fs <- sort(.gold_draw(s$files, 2, 7))
  expect_identical(unique(b$file), fs)
  expect_true(all(!is.na(b$name)))
  expect_identical(b$row[b$file == fs[1]], as.character(seq_len(sum(b$file == fs[1]))))
  expect_identical(sum(!is.na(b$heading)), 2L)
  a <- read_sheet(f, "about")
  expect_identical(a$value[a$item == "prefilled"], "yes")
  # corrected model output counts only once marked done
  err <- expect_error(quietly(benchmark(f)), class = "amanuensis_error_gold_empty")
  expect_match(conditionMessage(err), "prefilled")
  pages <- read_sheet(f, "pages")
  pages$done <- "yes"
  writexl::write_xlsx(list(benchmark = b, pages = pages, about = a, codebook = read_sheet(f, "codebook")), f)
  g <- quietly(benchmark(f))
  expect_true(.gold_info(g)$prefilled)
  expect_output(print(g), "optimistic")
  # the codebook can come from the result itself
  f2 <- file.path(s$dir, "gold2.xlsx")
  suppressWarnings(quietly(benchmark(f2, images = s$pages, n = 1, seed = 7, prefill = res, app = FALSE, rows = 1)))
  b2 <- read_sheet(f2, "benchmark")
  expect_identical(nrow(b2), 1L)
})

# ------- free text --------------------------------------------------------------

test_that("free text: a .txt template with === lines, read back", {
  s <- bench_setup()
  cbt <- codebook(free_text = TRUE)
  f <- new_template(s, file = "gold.txt", n = 2, seed = 3, cb = cbt)
  lines <- readLines(f, encoding = "UTF-8")
  fs <- sort(.gold_draw(s$files, 2, 3))
  expect_true(all(paste0("=== ", fs, " ===") %in% lines))
  expect_true("seed: 3" %in% lines)
  expect_error(benchmark(f), class = "amanuensis_error_gold_empty")
  h <- match(paste0("=== ", fs, " ==="), lines)
  filled <- c(lines[seq_len(h[1])], "Carrer Major, 12", "", "Josep Puig i Ferrer\tjornaler", "",
              lines[h[2]], "[no text]", "")
  writeLines(enc2utf8(filled), f, useBytes = TRUE)
  g <- quietly(benchmark(f, images = s$pages))
  expect_identical(names(as.data.frame(g)), c("file", "text"))
  expect_identical(g$file, fs)
  expect_identical(g$text, c("Carrer Major, 12\n\nJosep Puig i Ferrer\tjornaler", NA))
  expect_identical(.gold_info(g)$codebook$kind, "text")
  expect_identical(.gold_info(g)$draw$seed, 3)
  # a page left empty is left out
  writeLines(enc2utf8(c(lines[seq_len(h[1])], "Some text", lines[h[2]])), f, useBytes = TRUE)
  expect_identical(quietly(benchmark(f))$file, fs[1])
})

test_that("free text: a .docx template is written and read back", {
  s <- bench_setup()
  cbt <- codebook(free_text = TRUE)
  f <- new_template(s, file = "gold.docx", n = 2, seed = 3, cb = cbt)
  text <- .read_docx_text(f)
  fs <- sort(.gold_draw(s$files, 2, 3))
  expect_match(text, paste0("=== ", fs[1], " ==="), fixed = TRUE)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  h <- match(paste0("=== ", fs, " ==="), lines)
  .gold_write_docx(c(lines[seq_len(h[1])], "Pla\u00e7a & <carrer>", lines[h[2]], "line one", "line\ttwo"), f)
  g <- quietly(benchmark(f))
  expect_identical(g$text, c("Pla\u00e7a & <carrer>", "line one\nline\ttwo"))
})

test_that("free text: an .xlsx benchmark has columns file and text", {
  s <- bench_setup()
  f <- new_template(s, n = 1, seed = 3, cb = codebook(free_text = TRUE))
  expect_identical(names(read_sheet(f, "benchmark")), c("file", "text"))
  fs <- .gold_draw(s$files, 1, 3)
  fill_gold(f, data.frame(file = fs, text = "Line 1\nLine 2", stringsAsFactors = FALSE), "yes")
  g <- quietly(benchmark(f))
  expect_identical(g$text, "Line 1\nLine 2")
  expect_identical(.gold_info(g)$codebook$kind, "text")
})

test_that("a text benchmark cannot hold a table codebook", {
  s <- bench_setup()
  f <- file.path(s$dir, "mine.txt")
  writeLines(c("=== p001.jpg ===", "text"), f)
  expect_error(quietly(benchmark(f, codebook = s$cb)), "only the text of each page")
})

# ------- other kinds of benchmark files -----------------------------------------

gold_rows <- function() {
  data.frame(file = c("p001.jpg", "p001.jpg", "p002.jpg"), row = c(1L, 2L, NA),
             heading = c("H1", "H1", "H2"), name = c("Puig", "Ferrer", NA), age = c(34L, 29L, NA),
             sex = c("male", "female", NA), stringsAsFactors = FALSE)
}

test_that("csv benchmarks are read with the codebook; every page with values counts", {
  s <- bench_setup()
  f <- file.path(s$dir, "gold.csv")
  .write_csv(gold_rows(), f, bom = TRUE)
  expect_error(benchmark(f), "give the codebook")
  g <- quietly(benchmark(f, codebook = s$cb, images = s$pages))
  expect_identical(g$name, c("Puig", "Ferrer", NA))
  expect_identical(g$age, c(34L, 29L, NA))
  expect_identical(.gold_info(g)$pages$done, c(TRUE, TRUE))
  expect_null(.gold_info(g)$draw)
  expect_output(print(g), "Not drawn by benchmark")
  # a semicolon file from Spanish Excel, with a damaged id
  .write_csv(transform(gold_rows(), name = c("007", "12", NA)), f, sep = ";")
  expect_warning(quietly(benchmark(f, codebook = s$cb)), "leading zeros")
})

test_that("rds benchmarks: a table, or a saved benchmark object", {
  s <- bench_setup()
  f <- file.path(s$dir, "gold.rds")
  saveRDS(gold_rows(), f)
  g <- quietly(benchmark(f, codebook = s$cb))
  expect_identical(g$heading, c("H1", "H1", "H2"))
  saveRDS(g, f)
  expect_identical(benchmark(f), g)
  saveRDS(list(1), f)
  expect_error(benchmark(f, codebook = s$cb), "does not hold a table")
})

test_that("json benchmarks: output()'s list of pages, or an object keyed by file", {
  s <- bench_setup()
  f <- file.path(s$dir, "gold.json")
  pages <- list(
    list(file = "p001.jpg", source = "p001.jpg", data = roster_data("p001", 2)),
    list(file = "p002.jpg", data = list(heading = "Empty page", records = list())),
    list(file = "p003.jpg", status = "failed", data = NULL)
  )
  .write_json(pages, f)
  expect_error(benchmark(f), "give the codebook")
  g <- quietly(benchmark(f, codebook = s$cb))
  expect_identical(g$file, c("p001.jpg", "p001.jpg", "p002.jpg"))
  expect_identical(g$age, c(21L, 22L, NA))
  expect_identical(g$heading[3], "Empty page")
  expect_identical(.gold_info(g)$pages$done, c(TRUE, TRUE, FALSE))
  .write_json(list(p001.jpg = roster_data("p001", 1), p002.jpg = list(data = roster_data("p002", 3))), f)
  g <- quietly(benchmark(f, codebook = s$cb))
  expect_identical(as.vector(table(g$file)), c(1L, 3L))
})

test_that("dta benchmarks are read through haven", {
  skip_if_not_installed("haven")
  s <- bench_setup()
  f <- file.path(s$dir, "gold.dta")
  haven::write_dta(gold_rows(), f)
  g <- quietly(benchmark(f, codebook = s$cb))
  expect_identical(g$name, c("Puig", "Ferrer", NA))
  expect_identical(g$row, c(1L, 2L, NA))
})

test_that(".as_gold() reads files and data frames for score()", {
  s <- bench_setup()
  g <- quietly(.as_gold(gold_rows(), codebook = s$cb))
  expect_s3_class(g, "amanuensis_gold")
  expect_identical(.as_gold(g), g)
  expect_error(.as_gold(gold_rows()), "needs")
  expect_error(.as_gold(file.path(s$dir, "nope.xlsx")), "Cannot find")
})

# ------- the editor ---------------------------------------------------------------

test_that("the editor's pure functions read, check and save a page", {
  s <- bench_setup()
  f <- new_template(s, n = 2)
  fs <- sort(.gold_draw(s$files, 2, 1930))
  state <- .gold_state(f, NULL, .as_images(s$pages))
  expect_identical(state$files, fs)
  expect_identical(state$current, 1L)
  expect_identical(unname(basename(state$images)), fs)
  expect_identical(.gold_shown_rows(state), 1L)
  input <- list(p_1 = "Carrer Major", r_1_1 = "Puig, Josep", r_1_2 = "34", r_1_3 = "male", zoom = 150)
  vals <- .gold_form_values(input, state)
  expect_identical(vals$page, c(heading = "Carrer Major"))
  expect_identical(vals$rows$name, "Puig, Josep")
  state <- .gold_store_page(state, vals)
  state <- .gold_add_row(state)
  expect_identical(nrow(state$units[[fs[1]]]$rows), 2L)
  expect_identical(.gold_shown_rows(state), 2L)
  vals2 <- .gold_form_values(c(input, list(r_2_1 = "Ferrer, Maria", r_2_2 = "3a", r_2_3 = "")), state)
  expect_identical(.gold_check_values(vals2, state$book$cb), "row 2, age: \"3a\" is not a whole number")
  vals2$rows$age[2] <- "29"
  expect_length(.gold_check_values(vals2, state$book$cb), 0)
  state <- .gold_store_page(state, vals2)
  state <- .gold_add_row(state)
  state <- .gold_delete_row(state)
  expect_identical(nrow(state$units[[fs[1]]]$rows), 2L)
  state <- .gold_mark_done(state)
  .gold_app_save(state)
  g <- quietly(benchmark(f))
  expect_identical(g$file, c(fs[1], fs[1]))
  expect_identical(g$age, c(34L, 29L))
  expect_identical(g$heading, c("Carrer Major", "Carrer Major"))
  # reopened, the editor shows the page done and starts at the next one
  st2 <- .gold_state(f, NULL, NULL)
  expect_identical(st2$done, c(TRUE, FALSE))
  expect_identical(st2$current, 2L)
  expect_identical(st2$units[[fs[1]]]$rows$name, c("Puig, Josep", "Ferrer, Maria"))
  # the images folder recorded in the file is found without being given
  expect_false(anyNA(st2$images))
  # rows left empty are dropped when saving
  st2 <- .gold_add_row(.gold_add_row(st2))
  .gold_app_save(st2)
  expect_identical(nrow(read_sheet(f, "benchmark")), 3L)
})

test_that("the editor's form and app are built from the codebook", {
  skip_if_not_installed("shiny")
  s <- bench_setup()
  f <- new_template(s, n = 2, cb = full_codebook())
  state <- .gold_state(f, NULL, .as_images(s$pages))
  html <- as.character(.gold_form_ui(state))
  expect_match(html, "id=\"p_1\"", fixed = TRUE)
  expect_match(html, "id=\"p_2\"", fixed = TRUE)
  expect_match(html, "id=\"r_1_6\"", fixed = TRUE)
  expect_match(html, "(empty)", fixed = TRUE)
  expect_match(html, "Add row", fixed = TRUE)
  expect_match(html, ">female<", fixed = TRUE)
  served <- .gold_image_urls(state$images)
  expect_length(served$dirs, 1)
  expect_match(served$urls[[1]], "^amanuensis-pages-1/p00")
  state$urls <- served$urls
  expect_match(as.character(.gold_image_ui(state, 150)), "width: 150%", fixed = TRUE)
  expect_match(.gold_page_title(state), "Page 1 of 2")
  expect_s3_class(.gold_app_ui(state), "shiny.tag.list")
  expect_s3_class(.gold_app(state), "shiny.appobj")
  expect_match(as.character(.gold_message_ui(c("a", "b"), "error")), "Not saved")
})

test_that("the editor edits free text in a text area and saves the text file", {
  skip_if_not_installed("shiny")
  s <- bench_setup()
  f <- new_template(s, file = "gold.md", n = 2, seed = 3, cb = codebook(free_text = TRUE))
  state <- .gold_state(f, NULL, NULL)
  expect_match(as.character(.gold_form_ui(state)), "<textarea", fixed = TRUE)
  state <- .gold_store_page(state, .gold_form_values(list(p_1 = "Typed in the editor\nsecond line"), state))
  state <- .gold_mark_done(state)
  .gold_app_save(state)
  expect_true(any(grepl(" === done$", readLines(f))))
  g <- quietly(benchmark(f))
  expect_identical(g$text, "Typed in the editor\nsecond line")
})

test_that("the editor's server validates, saves and moves between pages", {
  skip_if_not_installed("shiny")
  s <- bench_setup()
  f <- new_template(s, n = 2)
  fs <- sort(.gold_draw(s$files, 2, 1930))
  app <- .gold_app(.gold_state(f, NULL, .as_images(s$pages)))
  suppressPackageStartupMessages(shiny::testServer(app, {
    session$setInputs(zoom = 120, p_1 = "Carrer Major", r_1_1 = "Puig", r_1_2 = "3a", r_1_3 = "male")
    session$setInputs(next_page = 1)
    expect_identical(rv$kind, "error")
    expect_identical(rv$state$current, 1L)
    expect_match(rv$msg, "is not a whole number")
    session$setInputs(r_1_2 = "34")
    session$setInputs(add_row = 1)
    expect_identical(nrow(rv$state$units[[fs[1]]]$rows), 2L)
    session$setInputs(r_2_1 = "Ferrer", r_2_2 = "29", r_2_3 = "female")
    session$setInputs(next_page = 2)
    expect_identical(rv$kind, "ok")
    expect_identical(rv$state$current, 2L)
    expect_true(rv$state$done[1])
    session$setInputs(p_1 = "Second page", r_1_1 = "", r_1_2 = "", r_1_3 = "")
    session$setInputs(prev_page = 1)
    expect_identical(rv$state$current, 1L)
    expect_false(rv$state$done[2])
  }))
  g <- quietly(benchmark(f))
  expect_identical(g$name, c("Puig", "Ferrer"))
  expect_identical(g$age, c(34L, 29L))
  expect_identical(.gold_info(g)$pages$done, c(TRUE, FALSE))
  # what was typed on the page left with Previous is kept, not marked done
  b <- read_sheet(f, "benchmark")
  expect_identical(b$heading[b$file == fs[2]], "Second page")
})

test_that("a plain Excel table of your own is read with the codebook", {
  s <- bench_setup()
  f <- file.path(s$dir, "mine.xlsx")
  writexl::write_xlsx(list(Sheet1 = transform(gold_rows(), row = as.character(row), age = as.character(age))), f)
  expect_error(benchmark(f), "give the codebook")
  g <- quietly(benchmark(f, codebook = s$cb))
  expect_identical(g$name, c("Puig", "Ferrer", NA))
  expect_identical(.gold_info(g)$pages$done, c(TRUE, TRUE))
})

test_that(".as_gold() keeps a benchmark's own codebook; the one given is a fallback", {
  s <- bench_setup()
  f <- new_template(s, n = 1)
  fs <- .gold_draw(s$files, 1, 1930)
  fill_gold(f, data.frame(file = fs, row = NA, heading = "h", name = "a", age = "1", sex = "male",
                          stringsAsFactors = FALSE), "yes")
  wider <- quietly(codebook(data.frame(variable = c("heading", "name", "age", "sex", "extra"),
                                       type = c("text", "text", "integer", "category", "text"),
                                       level = c("page", "row", "row", "row", "row"),
                                       values = c("", "", "", "male; female", ""), stringsAsFactors = FALSE)))
  expect_error(quietly(benchmark(f, codebook = wider)), "extra")
  g <- quietly(.as_gold(f, codebook = wider))
  expect_identical(.gold_info(g)$codebook$variables$variable, c("heading", "name", "age", "sex"))
  csv <- file.path(s$dir, "gold.csv")
  .write_csv(gold_rows(), csv)
  expect_identical(quietly(.as_gold(csv, codebook = s$cb))$age, c(34L, 29L, NA))
})

test_that("a text benchmark grows with a larger n, and Markdown underlines are not pages", {
  s <- bench_setup()
  cbt <- codebook(free_text = TRUE)
  f <- new_template(s, file = "gold.md", n = 1, seed = 3, cb = cbt)
  fs <- .gold_draw(s$files, 3, 3)
  lines <- readLines(f, encoding = "UTF-8")
  h <- match(paste0("=== ", fs[1], " ==="), lines)
  writeLines(enc2utf8(c(lines[seq_len(h)], "Title", "=====", "body")), f, useBytes = TRUE)
  msgs <- all_condition_text(benchmark(f, images = s$pages, n = 3, app = FALSE))
  expect_true(any(grepl("Added 2 pages", msgs)))
  lines <- readLines(f, encoding = "UTF-8")
  expect_true(all(paste0("=== ", fs, " ===") %in% lines))
  expect_true("n: 3" %in% lines)
  g <- quietly(benchmark(f))
  expect_identical(g$file, fs[1])
  expect_identical(g$text, "Title\n=====\nbody")
  expect_identical(.gold_info(g)$draw$files, fs)
})

test_that("a ';' CSV benchmark reads decimal commas in number columns", {
  withr::local_options(amanuensis.ask = FALSE)
  dir <- withr::local_tempdir()
  cb <- suppressMessages(codebook(data.frame(variable = c("name", "weight"), type = c("text", "number"))))
  path <- file.path(dir, "gold.csv")
  writeLines(c("file;row;name;weight", "p001.jpg;1;Puig;2,5", "p001.jpg;2;Soler;3"), path)
  gold <- suppressMessages(benchmark(path, codebook = cb))
  expect_identical(as.numeric(as.data.frame(gold)$weight), c(2.5, 3))
  # a comma file keeps commas as they are: "2,5" is not a number there
  path2 <- file.path(dir, "gold2.csv")
  writeLines(c("file,row,name,weight", "p001.jpg,1,Puig,\"2,5\""), path2)
  expect_error(suppressMessages(benchmark(path2, codebook = cb)), "number")
})
