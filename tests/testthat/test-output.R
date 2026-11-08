# output(): every format written and read back, from results made offline
# with model = "mock" on a two-page PDF drawn with pdf().

# ------- fixtures ---------------------------------------------------------------

# Built once per file: a table result (page + row variables, a category, an
# integer, a yes/no), the same result with accents and a page to check
# written into its records, and a free-text result.
out_fx <- local({
  withr::local_options(amanuensis.ask = FALSE, amanuensis.next_steps = FALSE, .local_envir = testthat::teardown_env())
  dir <- withr::local_tempdir(.local_envir = testthat::teardown_env())
  pdf_path <- file.path(dir, "book.pdf")
  pdf(pdf_path, width = 5, height = 7)
  for (i in 1:2) {
    plot.new()
    text(0.5, 0.5, paste("Page", i))
  }
  dev.off()
  suppressMessages({
    pages <- images(pdf_path, save_to = file.path(dir, "pages"))
    cb <- codebook(data.frame(
      variable = c("printed_page", "surname", "age", "sex", "literate"),
      type = c("text", "text", "integer", "category", "yes/no"),
      level = c("page", "row", "row", "row", "row"),
      values = c("", "", "", "male; female", ""),
      description = c("Page number as printed", "Surname exactly as written", "Age in years",
                      "Sex as recorded", "Whether the person can read"),
      stringsAsFactors = FALSE))
    res <- transcribe(pages, codebook = cb, model = "mock", save_to = file.path(dir, "res"))
    transcribe(pages, codebook = cb, model = "mock", save_to = file.path(dir, "edited"))
    txt <- transcribe(pages, codebook = codebook(free_text = TRUE), model = "mock",
                          save_to = file.path(dir, "text"))
  })
  edit <- function(folder, file, fun) {
    path <- .record_file(folder, file)
    .write_json(fun(.read_json(path)), path)
  }
  accent <- "Puig i Ferr\u00e9, \u00c0ngels \u00e7\u00f1"
  edit(file.path(dir, "edited"), "book_p001.jpg", function(r) {
    r$data$printed_page <- "xii"
    r$data$records[[1]]$surname <- accent
    r$data$records[[2]]$surname <- "line one\nline two | pipe & <amp> \"q\""
    r
  })
  edit(file.path(dir, "edited"), "book_p002.jpg", function(r) {
    r$status <- "check"
    r$message <- "Field age: expected a whole number."
    r
  })
  long_text <- paste(rep("Vilanova de la R\u00edba ", 2100), collapse = "")   # about 44,000 characters
  edit(file.path(dir, "text"), "book_p001.jpg", function(r) {
    r$data$text <- paste0("Primera l\u00ednia\nSegona l\u00ednia amb \u00e0")
    r
  })
  edit(file.path(dir, "text"), "book_p002.jpg", function(r) {
    r$data$text <- long_text
    r
  })
  edited <- .result_from_folder(file.path(dir, "edited"))
  text_res <- .result_from_folder(file.path(dir, "text"))
  list(dir = dir, res = res, edited = edited, text = text_res, accent = accent, long_text = long_text,
       res_dir = file.path(dir, "res"), edited_dir = file.path(dir, "edited"))
})

# macOS textutil, when present, as an independent reader of the .docx files
# (it writes to stdout: some sandboxes stop it writing files).
textutil_text <- function(path) {
  if (!nzchar(Sys.which("textutil"))) return(NULL)
  out <- suppressWarnings(system2("textutil", c("-convert", "txt", "-stdout", shQuote(path)), stdout = TRUE, stderr = TRUE))
  expect_null(attr(out, "status"))
  out <- paste(out, collapse = "\n")
  Encoding(out) <- "UTF-8"
  out
}

# Run `expr` silently and check that one of its messages contains `text`.
expect_said <- function(expr, text) {
  msgs <- character()
  value <- withCallingHandlers(expr, message = function(m) {
    msgs <<- c(msgs, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  expect_true(any(grepl(text, msgs, fixed = TRUE)), label = paste0("a message containing \"", text, "\""))
  invisible(value)
}

local_out <- function(env = parent.frame()) {
  withr::local_options(amanuensis.ask = FALSE, .local_envir = env)
  withr::local_tempdir(.local_envir = env)
}

# ------- tables in R --------------------------------------------------------------

test_that("without a file, output() returns the plain table visibly", {
  res <- out_fx$res
  expect_visible(output(res))
  tab <- output(res)
  expect_identical(class(tab), "data.frame")
  expect_null(attr(tab, "amanuensis"))
  expect_equal(tab, as.data.frame(res), ignore_attr = TRUE)
  expect_named(tab, c("file", "source", "source_page", "row", "printed_page", "surname", "age", "sex", "literate"))
  expect_type(tab$age, "integer")
  expect_type(tab$literate, "logical")

  pages <- output(res, table = "pages")
  expect_equal(nrow(pages), 2)
  expect_true(all(c("file", "status", "cost_usd", "notes") %in% names(pages)))

  cb <- output(res, table = "codebook")
  expect_named(cb, c("variable", "type", "level", "description", "values"))
  expect_equal(cb$values[cb$variable == "sex"], "male; female; other")
})

test_that("include = 'all' adds the pages to check with a page_status column", {
  ed <- out_fx$edited
  ok <- output(ed)
  expect_false("page_status" %in% names(ok))
  expect_equal(unique(ok$file), "book_p001.jpg")
  all <- output(ed, include = "all")
  expect_equal(names(all)[1:4], c("file", "source", "source_page", "page_status"))
  expect_equal(nrow(all), 6)
  expect_equal(unique(all$page_status[all$file == "book_p002.jpg"]), "check")
  expect_equal(unique(all$page_status[all$file == "book_p001.jpg"]), "ok")
  # a selection of columns keeps its columns
  sel <- output(ed[, c("file", "surname")], include = "all")
  expect_named(sel, c("page_status", "file", "surname"))
})

test_that("include = 'all' refuses a selection of rows", {
  ed <- out_fx$edited
  expect_error(output(ed[1:2, ], include = "all"), class = "amanuensis_error_input")
})

test_that("documents and JSON of a selection without the file column are refused, before anything is written", {
  dir <- local_out()
  res <- out_fx$res
  cols <- res[, c("surname", "age")]
  expect_s3_class(cols, "amanuensis_result")
  for (ext in c("txt", "md", "docx", "json")) {
    f <- file.path(dir, paste0("sel.", ext))
    expect_error(output(cols, f), "no file column", class = "amanuensis_error_input")
    expect_false(file.exists(f))
  }
  rows <- res[res$source_page == 1, "age"]
  expect_error(output(rows, file.path(dir, "rows.json")), "Keep file in the selection", class = "amanuensis_error_input")
  # one call writes all of its files or none: the CSV is not written either
  expect_error(output(cols, file.path(dir, c("sel.csv", "sel.txt"))), class = "amanuensis_error_input")
  expect_length(list.files(dir), 0)
  # include = "all" rebuilds the rows but keeps the columns given: still no file
  expect_error(output(out_fx$edited[, c("surname")], file.path(dir, "all.txt"), include = "all"),
               class = "amanuensis_error_input")
  # tables hold any selection as it is
  suppressMessages(output(cols, file.path(dir, c("sel.csv", "sel.xlsx"))))
  expect_equal(nrow(.read_delim_text(file.path(dir, "sel.csv"))), nrow(res))
  # with file kept, every page shows its own rows
  keep <- res[res$source_page == 1, c("file", "surname", "age")]
  suppressMessages(output(keep, file.path(dir, c("keep.txt", "keep.json"))))
  txt <- .read_text(file.path(dir, "keep.txt"))
  expect_false(grepl("no rows on this page", txt, fixed = TRUE))
  expect_match(txt, keep$surname[1], fixed = TRUE)
  expect_equal(vapply(jsonlite::read_json(file.path(dir, "keep.json")), function(p) p$file, ""), "book_p001.jpg")
})

test_that("as many rows as the run, but not its rows, is written as a selection", {
  dir <- local_out()
  res <- out_fx$res
  p1 <- which(res$file == "book_p001.jpg")
  same_n <- res[rep(p1, length.out = nrow(res)), ]
  expect_equal(nrow(same_n), .result_info(res)$n_rows)
  f <- file.path(dir, "same_n.json")
  suppressMessages(output(same_n, f))
  expect_equal(vapply(jsonlite::read_json(f), function(p) p$file, ""), "book_p001.jpg")
  # a sorted complete result is still the whole run, in reading order
  g <- file.path(dir, "sorted.json")
  suppressMessages(output(res[rev(seq_len(nrow(res))), ], g))
  expect_equal(vapply(jsonlite::read_json(g), function(p) p$file, ""), c("book_p001.jpg", "book_p002.jpg"))
})

test_that("a results folder gives the same table, and contract is passed on", {
  expect_equal(output(out_fx$res_dir), output(out_fx$res))
  id <- .result_info(out_fx$res)$contract
  expect_equal(output(out_fx$res_dir, contract = id), output(out_fx$res))
  expect_error(output(out_fx$res_dir, contract = "nope-nope"), class = "amanuensis_error_contract")
})

test_that("bad input gets plain errors", {
  dir <- local_out()
  expect_error(output(), class = "amanuensis_error_input")
  expect_error(output(as.data.frame(out_fx$res), file.path(dir, "a.csv")), "no longer linked", class = "amanuensis_error_input")
  expect_error(output(file.path(dir, "a.xlsx"), out_fx$res), "result comes first", class = "amanuensis_error_input")
  expect_error(output(out_fx$res, file.path(dir, "a.xls")), "Old .xls files cannot be written", class = "amanuensis_error_input")
  expect_error(output(out_fx$res, file.path(dir, "a.pdf")), "cannot write", class = "amanuensis_error_input")
  expect_error(output(out_fx$res, file.path(dir, "results")), "no extension", class = "amanuensis_error_input")
  expect_error(output(out_fx$res, dir), class = "amanuensis_error_input")
  expect_error(output(out_fx$res, file.path(dir, "a.csv"), sep = ";;"), class = "amanuensis_error_input")
  expect_error(output(out_fx$res, file.path(dir, "a.csv"), overwrite = NA), class = "amanuensis_error_input")
  expect_error(output(out_fx$res, table = "rows"))
  expect_error(output(out_fx$res, file.path(.libPaths()[1], "amanuensis-test.csv")), "package library",
               class = "amanuensis_error_input")
  expect_false(file.exists(file.path(.libPaths()[1], "amanuensis-test.csv")))
  expect_error(output(out_fx$res, file.path(out_fx$res_dir, "x.json")), "results folder", class = "amanuensis_error_input")
  expect_length(list.files(dir), 0)
})

# ------- overwriting ----------------------------------------------------------------

test_that("existing files are never replaced without overwrite = TRUE", {
  dir <- local_out()
  f <- file.path(dir, "res.csv")
  suppressMessages(output(out_fx$res, f))
  before <- readBin(f, "raw", file.size(f))
  expect_error(output(out_fx$res, f, sep = ";"), "overwrite = TRUE", class = "amanuensis_error_input")
  expect_identical(readBin(f, "raw", file.size(f)), before)
  # several files: one exists, so none is written
  g <- file.path(dir, "res.xlsx")
  expect_error(output(out_fx$res, c(g, f)), class = "amanuensis_error_input")
  expect_false(file.exists(g))
  expect_said(output(out_fx$res, f, sep = ";", overwrite = TRUE), "Wrote")
  expect_false(identical(readBin(f, "raw", file.size(f)), before))
})

test_that("a missing folder is created", {
  dir <- local_out()
  f <- file.path(dir, "new", "sub", "res.csv")
  expect_said(output(out_fx$res, f), "Created the folder")
  expect_true(file.exists(f))
})

# ------- CSV ------------------------------------------------------------------------

test_that("CSV is UTF-8 with a BOM by default, and sep and bom are honoured", {
  dir <- local_out()
  f1 <- file.path(dir, "a.csv")
  paths <- expect_said(output(out_fx$edited, f1), "table = \"pages\"")
  expect_identical(paths, f1)
  bytes <- readBin(f1, "raw", file.size(f1))
  expect_identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf)))
  back <- .read_delim_text(f1)
  expect_equal(nrow(back), 3)
  expect_equal(back$surname[1], out_fx$accent)
  expect_equal(back$surname[2], "line one\nline two | pipe & <amp> \"q\"")

  f2 <- file.path(dir, "b.csv")
  suppressMessages(output(out_fx$edited, f2, sep = ";", bom = FALSE))
  bytes2 <- readBin(f2, "raw", file.size(f2))
  expect_false(identical(bytes2[1:3], as.raw(c(0xef, 0xbb, 0xbf))))
  expect_match(rawToChar(bytes2[1:60]), "^file;source;source_page;row;")
  back2 <- .read_delim_text(f2)
  expect_equal(back2$surname, back$surname)

  f3 <- file.path(dir, "pages.csv")
  suppressMessages(output(out_fx$edited, f3, table = "pages"))
  expect_equal(nrow(.read_delim_text(f3)), 2)
})

test_that("sep = ';' writes decimal commas (unquoted); whole numbers and text are untouched", {
  dir <- local_out()
  r <- out_fx$res
  r$amount <- c(2.5, 3.125, 1069544.25, NA, 7, -0.5)
  expect_s3_class(r, "amanuensis_result")
  f <- file.path(dir, "dec.csv")
  expect_said(output(r, f, sep = ";", bom = FALSE), "decimal comma")
  lines <- readLines(f, encoding = "UTF-8")
  expect_equal(lines[1], "file;source;source_page;row;printed_page;surname;age;sex;literate;amount")
  amount <- vapply(strsplit(lines[-1], ";", fixed = TRUE), function(z) if (length(z) == 10) z[10] else "", "")
  expect_identical(amount, c("2,5", "3,125", "1069544,25", "", "7", "-0,5"))
  expect_false(any(grepl("\"", lines)))
  ages <- vapply(strsplit(lines[-1], ";", fixed = TRUE), `[`, "", 7)
  expect_identical(ages, as.character(output(r)$age))
  # the pages table: the cost is a number with decimals too
  g <- file.path(dir, "pages.csv")
  suppressMessages(output(r, g, table = "pages", sep = ";"))
  pages <- .read_delim_text(g)
  cost <- pages$cost_usd[nzchar(pages$cost_usd)]
  expect_true(length(cost) > 0)
  expect_false(any(grepl(".", cost, fixed = TRUE)))
  expect_equal(.as_number(cost, dec = ","), output(r, table = "pages")$cost_usd[nzchar(pages$cost_usd)])
  # a comma separator keeps the point; dec can be set either way, never equal to sep
  h <- file.path(dir, "point.csv")
  suppressMessages(output(r, h, bom = FALSE))
  expect_match(readLines(h)[2], ",2.5$")
  k <- file.path(dir, "semi_point.csv")
  suppressMessages(output(r, k, sep = ";", dec = ".", bom = FALSE))
  expect_match(readLines(k)[2], ";2.5$")
  expect_error(output(r, file.path(dir, "x.csv"), dec = ","), "cannot both be", class = "amanuensis_error_input")
  expect_error(output(r, file.path(dir, "x.csv"), sep = ";", dec = ";"), class = "amanuensis_error_input")
  expect_false(file.exists(file.path(dir, "x.csv")))
})

test_that("CSV keeps values Excel would run as formulas exactly, and says which columns have them", {
  dir <- local_out()
  r <- out_fx$res
  r$surname[1:3] <- c("=SUM(A1)", "- id. -", "@x")
  f <- file.path(dir, "f.csv")
  expect_said(output(r, f, bom = FALSE), "Excel runs as formulas")
  txt <- rawToChar(readBin(f, "raw", file.size(f)))
  expect_match(txt, ",=SUM(A1),", fixed = TRUE)
  expect_match(txt, ",- id. -,", fixed = TRUE)
  expect_match(txt, ",@x,", fixed = TRUE)
  expect_identical(.formula_like(output(r)), "surname")
  expect_identical(.formula_like(data.frame(a = c("-5", "-2.5", "a-b", NA), b = 1:4)), character())
  # the once-per-session hint points Excel users to .xlsx
  old <- .output_state$csv_hint
  withr::defer(.output_state$csv_hint <- old)
  .output_state$csv_hint <- NULL
  expect_said(output(out_fx$res, file.path(dir, "h.csv")), "an .xlsx file is safer")
})

test_that("writing tells about the pages left out, and about an empty table", {
  dir <- local_out()
  expect_said(output(out_fx$edited, file.path(dir, "a.csv")), "1 page to check is not in the file")
  # a fresh copy of the run (its environment is its own) with every page to check
  r <- .result_from_folder(out_fx$edited_dir)
  info <- .result_info(r)
  for (k in names(info$records)) info$records[[k]]$status <- "check"
  info$pages$status <- "check"
  empty <- .new_result(.flatten_records(list(), info$codebook), info)
  files <- file.path(dir, c("e.xlsx", "e.txt", "e.json", "e.docx"))
  expect_said(output(empty, files), "no page has been read correctly")
  expect_equal(nrow(readxl::read_excel(files[1], sheet = "records")), 0)
  expect_identical(jsonlite::read_json(files[3]), list())
  expect_equal(nrow(output(empty, include = "all")), 6)
})

# ------- Excel ----------------------------------------------------------------------

test_that("xlsx has records, pages, codebook and about sheets that read back", {
  dir <- local_out()
  f <- file.path(dir, "a.xlsx")
  expect_said(output(out_fx$edited, f), "3 rows in sheet records")
  expect_equal(readxl::excel_sheets(f), c("records", "pages", "codebook", "about"))
  rec <- as.data.frame(readxl::read_excel(f, sheet = "records"))
  tab <- output(out_fx$edited)
  expect_equal(rec$surname, tab$surname)
  expect_equal(rec$age, as.numeric(tab$age))
  expect_equal(rec$literate, tab$literate)
  expect_equal(rec$printed_page, rep("xii", 3))
  pages <- readxl::read_excel(f, sheet = "pages")
  expect_equal(pages$status, c("ok", "check"))
  cb <- readxl::read_excel(f, sheet = "codebook")
  expect_equal(cb$values[cb$variable == "sex"], "male; female; other")
  about <- as.data.frame(readxl::read_excel(f, sheet = "about"))
  val <- stats::setNames(about$value, about$item)
  expect_equal(val[["model"]], "mock")
  expect_equal(val[["contract"]], .result_info(out_fx$edited)$contract)
  expect_match(val[["package"]], "^amanuensis ")
  expect_match(val[["pages"]], "1 ok, 1 to check")
  expect_true(all(c("cost_usd", "prices_date", "instructions_source", "instructions_text", "written") %in% about$item))
  expect_match(val[["instructions_text"]], "# Rules")
})

test_that("xlsx with include = 'all' carries page_status; a selection keeps the run's pages", {
  dir <- local_out()
  f <- file.path(dir, "all.xlsx")
  suppressMessages(output(out_fx$edited, f, include = "all"))
  rec <- readxl::read_excel(f, sheet = "records")
  expect_equal(nrow(rec), 6)
  expect_true("page_status" %in% names(rec))

  g <- file.path(dir, "sel.xlsx")
  res <- out_fx$res
  suppressMessages(output(res[res$row == 1, ], g))
  expect_equal(nrow(readxl::read_excel(g, sheet = "records")), 2)
  expect_equal(nrow(readxl::read_excel(g, sheet = "pages")), 2)
  about <- as.data.frame(readxl::read_excel(g, sheet = "about"))
  expect_match(about$value[about$item == "rows_written"], "a selection of the 6 rows")
})

test_that("free text goes to a 'text' sheet, and over-long cells are cut with a warning", {
  dir <- local_out()
  f <- file.path(dir, "t.xlsx")
  expect_warning(expect_said(output(out_fx$text, f), "one row per page"), "32,767")
  expect_equal(readxl::excel_sheets(f), c("text", "pages", "codebook", "about"))
  t <- readxl::read_excel(f, sheet = "text")
  expect_equal(t$text[1], "Primera l\u00ednia\nSegona l\u00ednia amb \u00e0")
  expect_equal(nchar(t$text[2]), 32767)
  expect_match(t$text[2], "\\.\\.\\.$")
  # the CSV keeps the whole text
  g <- file.path(dir, "t.csv")
  suppressMessages(output(out_fx$text, g))
  back <- .read_delim_text(g)
  expect_equal(back$text[2], sub(" +$", "", out_fx$long_text))
})

test_that("a JSON schema's further lists get sheets of their own", {
  dir <- local_out()
  schema <- '{"type": "object", "properties": {
    "page_no": {"type": ["string", "null"]},
    "entries": {"type": "array", "items": {"type": "object", "properties": {
      "first.name": {"type": ["string", "null"]}, "amount": {"type": ["number", "null"]}},
      "required": ["first.name", "amount"]}},
    "notes": {"type": "array", "items": {"type": "object", "properties": {
      "text": {"type": ["string", "null"]}}, "required": ["text"]}}},
    "required": ["page_no", "entries", "notes"]}'
  pdf_path <- file.path(dir, "led.pdf")
  pdf(pdf_path)
  plot.new()
  text(0.5, 0.5, "Ledger")
  dev.off()
  suppressMessages({
    pg <- images(pdf_path, save_to = file.path(dir, "pages"))
    res <- transcribe(pg, codebook = codebook(json = schema), model = "mock", save_to = FALSE)
  })
  f <- file.path(dir, "led.xlsx")
  suppressMessages(output(res, f))
  expect_equal(readxl::excel_sheets(f), c("records", "notes", "pages", "codebook", "about"))
  notes <- readxl::read_excel(f, sheet = "notes")
  expect_equal(names(notes), c("file", "source", "source_page", "row", "text"))
  expect_equal(nrow(notes), 3)
  rec <- readxl::read_excel(f, sheet = "records")
  expect_equal(nrow(rec), 3)
  # Stata names: the dotted name is renamed, and the renaming is listed
  g <- file.path(dir, "led.dta")
  skip_if_not_installed("haven")
  msgs <- testthat::capture_messages(output(res, g))
  expect_true(any(grepl("renamed", msgs)))
  expect_true(any(grepl("first.name -> first_name", msgs, fixed = TRUE)))
  expect_true("first_name" %in% names(haven::read_dta(g)))
})

# ------- Stata ----------------------------------------------------------------------

test_that("dta reads back with labels, 0/1 yes/no and accents", {
  skip_if_not_installed("haven")
  dir <- local_out()
  f <- file.path(dir, "a.dta")
  suppressMessages(output(out_fx$edited, f))
  d <- haven::read_dta(f)
  expect_equal(names(d), names(output(out_fx$edited)))
  expect_equal(d$surname[1], out_fx$accent)
  expect_equal(attr(d$surname, "label"), "Surname exactly as written")
  expect_equal(attr(d$source_page, "label"), "Page number within the source document (not a folio)")
  expect_equal(as.numeric(d$literate), c(1, 0, 1))
  expect_equal(attr(d$literate, "labels"), c(no = 0, yes = 1))
  expect_equal(attr(d, "label"), "amanuensis: book.pdf")
})

test_that("dta keeps very long text (strL)", {
  skip_if_not_installed("haven")
  dir <- local_out()
  f <- file.path(dir, "t.dta")
  suppressMessages(output(out_fx$text, f))
  d <- haven::read_dta(f)
  expect_equal(d$text[2], out_fx$long_text)
})

test_that("Stata names are made valid, short, unreserved and unique", {
  x <- c("printed.page", "in", "long", "\u00e9poca", "1930_pop", strrep("a", 40), "a", "a", "age (years)",
         "str12", "_x", "ok_name")
  y <- .stata_names(x)
  expect_equal(y, c("printed_page", "in_", "long_", "epoca", "v1930_pop", strrep("a", 32), "a", "a_2",
                    "age_years", "str12_", "v_x", "ok_name"))
  expect_true(all(nchar(y) <= 32))
  expect_false(anyDuplicated(y) > 0)
  expect_equal(.stata_names(c(strrep("b", 40), strrep("b", 41))), c(strrep("b", 32), paste0(strrep("b", 30), "_2")))
})

test_that("a name that is already valid for Stata is never taken by a renamed column", {
  expect_identical(.stata_names(c("edat_\u00e0nima", "edat_anima")), c("edat_anima_2", "edat_anima"))
  expect_identical(.stata_names(c("a.b", "a_b")), c("a_b_2", "a_b"))
  expect_identical(.stata_names(c("a_b", "a.b")), c("a_b", "a_b_2"))
  # a suffix never takes a valid name further on either
  expect_identical(.stata_names(c("a.b", "a_b_2", "a_b")), c("a_b_3", "a_b_2", "a_b"))
  # two renamed names that collide: the first keeps the plain name
  expect_identical(.stata_names(c("a.b", "a-b")), c("a_b", "a_b_2"))
  skip_if_not_installed("haven")
  dir <- local_out()
  r <- out_fx$res
  r[["edat_\u00e0nima"]] <- rep("accented", nrow(r))
  r$edat_anima <- rep("plain", nrow(r))
  f <- file.path(dir, "names.dta")
  expect_said(output(r, f), "edat_\u00e0nima -> edat_anima_2")
  d <- haven::read_dta(f)
  expect_equal(unique(d$edat_anima), "plain")
  expect_equal(unique(d$edat_anima_2), "accented")
})

# ------- RDS ------------------------------------------------------------------------

test_that("rds gives the whole result back, and output() reads it", {
  dir <- local_out()
  f <- file.path(dir, "a.rds")
  expect_said(output(out_fx$res, f), "readRDS")
  back <- readRDS(f)
  expect_s3_class(back, "amanuensis_result")
  expect_equal(nrow(back), 6)
  expect_equal(.result_info(back)$contract, .result_info(out_fx$res)$contract)
  expect_equal(output(f), output(out_fx$res))
})

# ------- JSON -----------------------------------------------------------------------

test_that("json holds one object per page with the model's data", {
  dir <- local_out()
  f <- file.path(dir, "a.json")
  suppressMessages(output(out_fx$edited, f))
  j <- jsonlite::read_json(f)
  expect_length(j, 1)
  expect_equal(names(j[[1]]), c("file", "source", "source_page", "status", "data"))
  expect_equal(j[[1]]$data, .result_info(out_fx$edited)$records[["book_p001.jpg"]]$data)
  expect_equal(j[[1]]$data$records[[1]]$surname, out_fx$accent)

  g <- file.path(dir, "all.json")
  suppressMessages(output(out_fx$edited, g, include = "all"))
  j2 <- jsonlite::read_json(g)
  expect_equal(vapply(j2, function(p) p$status, ""), c("ok", "check"))
  expect_equal(j2[[2]]$message, "Field age: expected a whole number.")

  h <- file.path(dir, "sel.json")
  res <- out_fx$res
  expect_said(output(res[res$file == "book_p002.jpg", ], h), "whole pages")
  expect_equal(vapply(jsonlite::read_json(h), function(p) p$file, ""), "book_p002.jpg")

  k <- file.path(dir, "pages.json")
  suppressMessages(output(res, k, table = "pages"))
  expect_equal(nrow(jsonlite::fromJSON(k)), 2)
})

# ------- text documents -------------------------------------------------------------

test_that("txt has a section per page with an aligned table", {
  dir <- local_out()
  f <- file.path(dir, "a.txt")
  expect_said(output(out_fx$edited, f, include = "all"), "one section per page")
  lines <- strsplit(.read_text(f), "\n", fixed = TRUE)[[1]]
  expect_match(lines[1], "^book.pdf: results")
  expect_true("=== book_p001.jpg (book.pdf p. 1) ===" %in% lines)
  expect_true("=== book_p002.jpg (book.pdf p. 2) ===" %in% lines)
  expect_true("To check: Field age: expected a whole number." %in% lines)
  expect_true(any(grepl("^printed_page: xii$", lines)))
  head <- grep("^row +surname", lines)
  expect_length(head, 2)
  expect_match(lines[head[1] + 2], out_fx$accent, fixed = TRUE)
  expect_match(lines[head[1] + 3], "line one / line two | pipe", fixed = TRUE)
  # columns line up: every column starts where its dashes start, accents or not
  starts <- gregexpr("-+", lines[head[1] + 1])[[1]]
  for (line in lines[head[1] + 0:4]) {
    chars <- strsplit(line, "")[[1]]
    upto <- cumsum(.text_width(chars))
    for (s in starts[-1]) expect_equal(chars[match(s - 1, upto)], " ")
  }
})

test_that("free text in txt and md is the page's text under each heading", {
  dir <- local_out()
  f <- file.path(dir, "t.txt")
  suppressMessages(output(out_fx$text, f))
  txt <- .read_text(f)
  expect_match(txt, "=== book_p001.jpg (book.pdf p. 1) ===\n\nPrimera l\u00ednia\nSegona l\u00ednia amb \u00e0\n", fixed = TRUE)
  g <- file.path(dir, "t.md")
  suppressMessages(output(out_fx$text, g))
  md <- .read_text(g)
  expect_match(md, "## book\\_p001.jpg\n\n*book.pdf, page 1*\n\nPrimera l\u00ednia  \nSegona", fixed = TRUE)
})

test_that("md escapes pipes in tables and writes a markdown table", {
  dir <- local_out()
  f <- file.path(dir, "a.md")
  suppressMessages(output(out_fx$edited, f))
  md <- strsplit(.read_text(f), "\n", fixed = TRUE)[[1]]
  expect_match(md[1], "^# book.pdf: results")
  expect_true("| row | surname | age | sex | literate |" %in% md)
  expect_true("|---:|---|---:|---|---|" %in% md)
  row2 <- md[grepl("^\\| 2 \\|", md)]
  expect_match(row2, "line one<br>line two \\| pipe &", fixed = TRUE)
  expect_true(any(grepl(out_fx$accent, md, fixed = TRUE)))
  # the pages table as a document
  g <- file.path(dir, "pages.md")
  suppressMessages(output(out_fx$edited, g, table = "pages"))
  expect_match(.read_text(g), "## Pages", fixed = TRUE)
})

# ------- Word -----------------------------------------------------------------------

test_that("docx opens and reads back", {
  dir <- local_out()
  f <- file.path(dir, "a.docx")
  suppressMessages(output(out_fx$edited, f))
  parts <- utils::unzip(f, list = TRUE)$Name
  expect_true(all(c("[Content_Types].xml", "_rels/.rels", "word/document.xml", "word/styles.xml",
                    "word/_rels/document.xml.rels") %in% parts))
  txt <- .read_docx_text(f)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  expect_equal(lines[1], "book.pdf: results")
  expect_true("book_p001.jpg (book.pdf p. 1)" %in% lines)
  expect_true("printed_page: xii" %in% lines)
  expect_true(out_fx$accent %in% lines)
  expect_match(txt, "line one\nline two | pipe & <amp> \"q\"", fixed = TRUE)
  conv <- textutil_text(f)
  if (!is.null(conv)) {
    expect_match(conv, "book_p001.jpg (book.pdf p. 1)", fixed = TRUE)
    expect_match(conv, out_fx$accent, fixed = TRUE)
  }
})

test_that("free text docx keeps line breaks", {
  dir <- local_out()
  f <- file.path(dir, "t.docx")
  suppressMessages(output(out_fx$text, f))
  txt <- .read_docx_text(f)
  expect_match(txt, "book_p001.jpg (book.pdf p. 1)\nPrimera l\u00ednia\nSegona l\u00ednia amb \u00e0\n", fixed = TRUE)
  expect_match(txt, out_fx$long_text, fixed = TRUE)
})

test_that(".write_docx round-trips through .read_docx_text exactly", {
  dir <- local_out()
  f <- file.path(dir, "rt.docx")
  texts <- c("Heading with \u00e0ccents & <tags>", "", "A paragraph\nwith a line break\tand a tab",
             "Quotes \"double\" and 'single'; ampersand &amp; literal", "  leading and trailing spaces  ",
             "ends with a break\n", "\u00c0ngels \u00e7\u00f1 \u20ac 1.069,50")
  blocks <- list(list(type = "title", text = texts[1]), list(type = "para", text = texts[2]),
                 list(type = "heading", text = texts[3]), list(type = "heading", level = 2, text = texts[4]),
                 list(type = "para", text = texts[5]), list(type = "para", text = texts[6]),
                 list(type = "para", text = texts[7]))
  .write_docx(blocks, f)
  expect_identical(.read_docx_text(f), paste(texts, collapse = "\n"))
  # a table: one paragraph per cell, header first
  g <- file.path(dir, "tab.docx")
  .write_docx(list(list(type = "table", table = data.frame(a = c("x", NA), b = c(TRUE, FALSE), n = c(1.5, 2)))), g)
  expect_identical(.read_docx_text(g), paste(c("a", "b", "n", "x", "yes", "1.5", "", "no", "2"), collapse = "\n"))
  # control characters XML forbids are left out, not written
  h <- file.path(dir, "ctl.docx")
  .write_docx(list(list(type = "para", text = "a\001b\002c")), h)
  expect_identical(.read_docx_text(h), "abc")
  expect_equal(.docx_control_chars(c("a\001b\002c", NA)), 2L)
})

test_that("instructions_template() writes a .docx that instructions() reads back", {
  dir <- local_out()
  f <- file.path(dir, "instr.docx")
  suppressMessages(instructions_template(f))
  txt <- .read_docx_text(f)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  expect_equal(lines[1:2], c("Role", "You are transcribing <what the source is: title, place, date, archive and shelfmark>."))
  expect_true("Details" %in% lines)
  conv <- textutil_text(f)
  if (!is.null(conv)) expect_match(conv, "Reading order", fixed = TRUE)
})

test_that("several formats in one call all get written", {
  dir <- local_out()
  files <- file.path(dir, c("r.xlsx", "r.csv", "r.dta", "r.rds", "r.json", "r.txt", "r.text", "r.md", "r.docx"))
  if (!requireNamespace("haven", quietly = TRUE)) files <- files[!grepl("dta$", files)]
  paths <- suppressMessages(output(out_fx$res, files))
  expect_identical(paths, files)
  expect_true(all(file.exists(files)))
  expect_false(any(grepl("^\\.amanuensis-", list.files(dir, all.files = TRUE))))
})
