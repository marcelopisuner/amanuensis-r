# From the model's JSON to the records table: types, big numbers, and the
# shapes of a user's own JSON schema (records inside an object, $ref items,
# entries of an enclosing list with no records of their own).

page_rec <- function(data, file = "book_p001.jpg", page = 1L) {
  list(file = file, source = "book.pdf", source_page = page, data = data)
}

test_that("whole numbers above R's integer limit stay exact, with no warning", {
  expect_no_warning(col <- .as_column(list(3500000000, 2147483648, 12L, NULL), "integer"))
  expect_type(col, "double")
  expect_identical(col, c(3500000000, 2147483648, 12, NA))
  # within range the column is still an R integer
  expect_identical(.as_column(list(2147483647, -3L, NULL), "integer"), c(2147483647L, -3L, NA))
  # a decimal keeps the column a double, as before
  expect_identical(.as_column(list(1.5, 2L), "integer"), c(1.5, 2))
  # numbers written into text never use scientific notation
  expect_identical(.as_column(list(3500000000, 1250000.5, 12L, 1e-7, TRUE), "text"),
                   c("3500000000", "1250000.5", "12", "0.0000001", "true"))
})

test_that("an integer variable with a budget total keeps it in the table and every output file", {
  local_amanuensis()
  dir <- withr::local_tempdir()
  make_jpegs(file.path(dir, "scans"), n = 1, prefix = "budget")
  cb <- quietly(codebook(data.frame(variable = c("concepto", "pesetas"), type = c("text", "integer"),
                                    description = c("Budget line.", "Amount in pesetas."), stringsAsFactors = FALSE)))
  reply <- list(records = list(list(concepto = "Total gastos", pesetas = 3500000000),
                               list(concepto = "Guerra", pesetas = 2147483648),
                               list(concepto = "Marina", pesetas = 12L)))
  res <- quietly(httr2::with_mocked_responses(function(req) ok_response(data = reply),
    transcribe(file.path(dir, "scans"), codebook = cb, save_to = file.path(dir, "res"))))
  tab <- as.data.frame(res)
  expect_identical(tab$pesetas, c(3500000000, 2147483648, 12))
  expect_identical(.result_info(res)$pages$status, "ok")

  csv <- file.path(dir, "t.csv")
  quietly(output(res, csv))
  lines <- readLines(csv, encoding = "UTF-8")
  expect_true(any(grepl("Total gastos,3500000000$", lines)))
  expect_true(any(grepl("Guerra,2147483648$", lines)))
  xlsx <- file.path(dir, "t.xlsx")
  quietly(output(res, xlsx))
  expect_identical(as.numeric(readxl::read_excel(xlsx, sheet = "records")$pesetas), c(3500000000, 2147483648, 12))
  skip_if_not_installed("haven")
  dta <- file.path(dir, "t.dta")
  quietly(output(res, dta))
  expect_identical(as.numeric(haven::read_dta(dta)$pesetas), c(3500000000, 2147483648, 12))

  # a benchmark holding the same values scores a correct extraction as correct
  gold <- file.path(dir, "gold.csv")
  writeLines(c("file,row,concepto,pesetas", "budget001.jpg,1,Total gastos,3500000000",
               "budget001.jpg,2,Guerra,2147483648", "budget001.jpg,3,Marina,12"), gold)
  s <- quietly(score(gold, res))
  expect_identical(nrow(s$differences), 0L)
})

test_that("lists of numbers are spread as text without scientific notation", {
  cb <- quietly(codebook(json = '{"type": "object", "properties": {
    "totals": {"type": "array", "items": {"type": "number"}},
    "people": {"type": "array", "items": {"type": "object", "properties": {"name": {"type": "string"}}}}}}'))
  tab <- .flatten_records(list(page_rec(list(totals = list(3500000000, 1250000.5), people = list(list(name = "Puig"))))), cb)
  expect_identical(tab$totals_1, "3500000000")
  expect_identical(tab$totals_2, "1250000.5")
})

test_that("records inside a plain object are found, and the object's other fields stay page variables", {
  cb <- quietly(codebook(json = '{"type": "object", "properties": {
    "header": {"type": "object", "properties": {"town": {"type": ["string", "null"]}}},
    "body": {"type": "object", "properties": {"volume": {"type": ["string", "null"]},
      "people": {"type": "array", "items": {"type": "object", "properties": {
        "name": {"type": "string"}, "age": {"type": ["integer", "null"]}}}}}}}}'))
  expect_identical(.records_path(cb), c("body", "people"))
  expect_identical(cb$variables$variable, c("header.town", "body.volume", "name", "age"))
  data <- list(header = list(town = "Vilanova"),
               body = list(volume = "II", people = list(list(name = "Puig", age = 34L), list(name = "Soler", age = 40L))))
  tab <- .flatten_records(list(page_rec(data)), cb)
  expect_identical(nrow(tab), 2L)
  expect_identical(tab$name, c("Puig", "Soler"))
  expect_identical(tab$body.volume, c("II", "II"))
  expect_identical(tab$header.town, c("Vilanova", "Vilanova"))
  u <- .page_unit(data, cb)
  expect_identical(u$page$body.volume, "II")
  expect_identical(u$rows$age, c(34L, 40L))
  expect_length(.page_rows(data, .records_path(cb)), 2L)
})

test_that("list items given as a $ref become the records table", {
  cb <- quietly(codebook(json = '{"type": "object",
    "$defs": {"person": {"type": "object", "properties": {"name": {"type": ["string", "null"]},
                                                          "age": {"type": ["integer", "null"]}}}},
    "properties": {"page_no": {"type": ["string", "null"]},
                   "people": {"type": "array", "items": {"$ref": "#/$defs/person"}}}}'))
  # the schema sent to Google is kept as written
  expect_identical(cb$schema$properties$people$items[["$ref"]], "#/$defs/person")
  expect_identical(cb$variables$variable, c("page_no", "name", "age"))
  expect_identical(cb$variables$type, c("text", "text", "integer"))
  expect_identical(.records_path(cb), "people")
  tab <- .flatten_records(list(page_rec(list(page_no = "12", people = list(list(name = "Puig", age = 3L),
                                                                         list(name = "Soler", age = 4L))))), cb)
  expect_identical(tab$name, c("Puig", "Soler"))
  expect_identical(tab$age, c(3L, 4L))
  # a reference to itself (a person's children are persons) does not loop
  rec <- quietly(codebook(json = '{"type": "object",
    "$defs": {"person": {"type": "object", "properties": {"name": {"type": "string"},
      "children": {"type": "array", "items": {"$ref": "#/$defs/person"}}}}},
    "properties": {"people": {"type": "array", "items": {"$ref": "#/$defs/person"}}}}'))
  expect_identical(.records_path(rec), "people")
})

test_that("an entry of an enclosing list with an empty list of its own keeps one row", {
  cb <- quietly(codebook(json = '{"type": "object", "properties": {
    "page_no": {"type": ["string", "null"]},
    "households": {"type": "array", "items": {"type": "object", "properties": {
      "head": {"type": "string"}, "folio_n": {"type": ["string", "null"]},
      "members": {"type": "array", "items": {"type": "object", "properties": {"name": {"type": "string"}}}}}}}}}'))
  data <- list(page_no = "12", households = list(
    list(head = "Puig", folio_n = "12v", members = list(list(name = "Anna"), list(name = "Joan"))),
    list(head = "Soler (lives alone)", folio_n = "13", members = list()),
    list(head = "Vidal", folio_n = NULL, members = list(list(name = "Pere")))))
  tab <- .flatten_records(list(page_rec(data)), cb)
  expect_identical(tab$households.head, c("Puig", "Puig", "Soler (lives alone)", "Vidal"))
  expect_identical(tab$name, c("Anna", "Joan", NA, "Pere"))
  expect_identical(tab$households_n, c(1L, 1L, 2L, 3L))
  expect_identical(tab$row, 1:4)
  # only the package's own counters are made whole numbers: folio_n is text
  expect_identical(tab$households.folio_n, c("12v", "12v", "13", NA))
  expect_identical(nrow(.page_unit(data, cb)$rows), 4L)
})

test_that("codebook tables are unchanged: a page with no records gives no rows", {
  cb <- roster_codebook()
  tab <- .flatten_records(list(page_rec(roster_data("p1", n = 2)), page_rec(list(heading = "H", records = list()), "p2.jpg", 2L)), cb)
  expect_identical(tab$file, c("book_p001.jpg", "book_p001.jpg"))
  expect_identical(tab$age, c(21L, 22L))
  expect_identical(names(tab), c("file", "source", "source_page", "row", "heading", "name", "age", "sex"))
})
