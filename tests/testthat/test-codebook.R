cb_df <- function(...) data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)

test_that("a data frame becomes variables and a draft-07 schema with every field required and nullable", {
  cb <- roster_codebook()
  expect_s3_class(cb, "amanuensis_codebook")
  expect_identical(cb$kind, "table")
  expect_identical(cb$variables$variable, c("heading", "name", "age", "sex"))
  expect_identical(cb$variables$level, c("page", "row", "row", "row"))
  expect_identical(cb$variables$values[[4]], c("male", "female", "other"))
  s <- cb$schema
  expect_identical(s$type, "object")
  expect_identical(names(s$properties), c("heading", "records"))
  expect_identical(s$required, list("heading", "records"))
  expect_false(s$additionalProperties)
  expect_identical(s$properties$heading$type, list("string", "null"))
  items <- s$properties$records$items
  expect_identical(items$required, list("name", "age", "sex"))
  expect_identical(items$properties$age$type, list("integer", "null"))
  expect_identical(items$properties$sex$enum, list("male", "female", "other", NULL))
  expect_match(items$properties$age$description, "Give a whole number")
  expect_identical(cb$sha12, .sha12(.to_json(s)))
  expect_output(print(cb), "4 variables - 1 per page, 3 per row")
})

test_that("every required list serialises as a JSON array, even with one field", {
  cb <- quietly(codebook(cb_df(variable = "name", description = "Name.")))
  j <- .to_json(cb$schema)
  expect_match(j, "\"required\":[\"records\"]", fixed = TRUE)
  expect_match(j, "\"required\":[\"name\"]", fixed = TRUE)
  expect_false(grepl("\"required\":\"", j, fixed = TRUE))
  cb2 <- quietly(codebook(cb_df(variable = "folio", level = "page", description = "Folio.")))
  expect_match(.to_json(cb2$schema), "\"required\":[\"folio\"]", fixed = TRUE)
  expect_match(.to_json(codebook(free_text = TRUE)$schema), "\"required\":[\"text\"]", fixed = TRUE)
})

test_that("spreadsheet header and type synonyms are understood (English, Spanish, Catalan)", {
  df <- cb_df(Nombre = c("pagina", "edad", "casado", "estado", "fecha nacimiento"),
              Tipo = c("texto", "entero", "si/no", "factor", "fecha"),
              Descripcion = c("Folio impreso.", "Edad.", "Casado.", "Estado civil.", "Fecha."),
              Valores = c("", "", "", "soltero; casado", ""),
              Nivel = c("pagina", "fila", "registro", "", "linea"))
  msgs <- all_condition_text(cb <- codebook(df))
  expect_identical(cb$variables$type, c("text", "integer", "boolean", "category", "text"))
  expect_identical(cb$variables$level, c("page", "row", "row", "row", "row"))
  expect_true(any(grepl("Dates are kept as text", msgs)))
  # renamed to underscore
  expect_identical(cb$variables$variable[5], "fecha_nacimiento")
  expect_true(any(grepl("cannot contain spaces", msgs)))
  # values with no type mean a category
  cb2 <- quietly(codebook(cb_df(variable = "sex", values = "H; D", description = "Sex.")))
  expect_identical(cb2$variables$type, "category")
})

test_that("unknown columns are mentioned and ignored; values of non-category types too", {
  msgs <- all_condition_text(cb <- codebook(cb_df(variable = "a", type = "text", values = "x; y",
                                                  colour = "red", description = "A.")))
  expect_true(any(grepl("Ignoring codebook column.*colour", msgs)))
  expect_true(any(grepl("Ignoring the values given", msgs)))
  expect_length(cb$variables$values[[1]], 0)
})

test_that("an Excel codebook may have a title row above the header", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "cb.xlsx")
  sheet <- cb_df(A = c("Codebook of the 1930 padro", "Variable", "surname", "age"),
                 B = c(NA, "Type", "text", "integer"),
                 C = c(NA, "Description", "Surname as written.", "Age."))
  writexl::write_xlsx(list(codebook = sheet), path, col_names = FALSE)
  msgs <- all_condition_text(cb <- codebook(path))
  expect_identical(cb$variables$variable, c("surname", "age"))
  expect_identical(cb$variables$type, c("text", "integer"))
  expect_identical(cb$source, path)
})

test_that("the sheet argument picks a sheet; by default a sheet called codebook", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "cb.xlsx")
  writexl::write_xlsx(list(notes = cb_df(note = "read me"),
                           vars = cb_df(variable = "x", description = "X."),
                           codebook = cb_df(variable = "y", description = "Y.")), path)
  expect_identical(quietly(codebook(path))$variables$variable, "y")
  expect_identical(quietly(codebook(path, sheet = "vars"))$variables$variable, "x")
  expect_error(codebook(path, sheet = "nope"), class = "amanuensis_error_input", regexp = "no sheet called")
})

test_that("a CSV codebook with semicolons, a BOM and Windows line endings is read", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "cb.csv")
  # a quoted cell with a line break, and one with a comma inside a value
  txt <-"variable;type;values;description\r\nsex;category;\"H\nD\";Sex.\r\ntown;category;\"Barcelona, ciutat; Girona\";Town.\r\n"
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(txt)), path)
  cb <- quietly(codebook(path))
  expect_identical(cb$variables$variable, c("sex", "town"))
  expect_identical(cb$variables$values[[1]], c("H", "D", "other"))
  expect_identical(cb$variables$values[[2]], c("Barcelona, ciutat", "Girona", "other"))
})

test_that("the catch-all 'other' is added only when no such value is listed", {
  msgs <- all_condition_text(cb <- codebook(cb_df(variable = c("a", "b", "c"), type = "category",
                                                  values = c("x; y", "x; Otro", "x; altres"), description = "D.")))
  expect_identical(cb$variables$values, list(c("x", "y", "other"), c("x", "Otro"), c("x", "altres")))
  expect_true(any(grepl("Added the value .other. to .a.", msgs)))
  cb2 <- quietly(codebook(cb_df(variable = "a", type = "category", values = "x; y", description = "D."), other = FALSE))
  expect_identical(cb2$variables$values[[1]], c("x", "y"))
})

test_that("category values split on ; | and new lines, never on commas", {
  expect_identical(.split_values("a; b |c\nd ;; a"), c("a", "b", "c", "d"))
  expect_identical(.split_values("Barcelona, ciutat"), "Barcelona, ciutat")
  expect_identical(.split_values(""), character())
  # a list written with commas is one value, so the codebook stops (see below)
  expect_error(codebook(cb_df(variable = "t", type = "category", values = "a, b, c", description = "D.")),
               class = "amanuensis_error_input", regexp = "only one allowed value")
})

test_that("a category with a single listed value stops before anything is sent", {
  err <- expect_error(codebook(cb_df(variable = c("estado", "sexo", "casado", "ok"), type = "category",
                                     values = c("soltero, casado, viudo", "H/M", "S", "a; b"),
                                     description = "D.")),
                      class = "amanuensis_error_input")
  msg <- conditionMessage(err)
  expect_match(msg, "only one allowed value")
  # each variable with its own value, and the likely fix
  expect_match(msg, "estado.*soltero, casado, viudo.*soltero; casado; viudo")
  expect_match(msg, "sexo.*H/M.*H; M")
  expect_match(msg, "casado.*lists only.*S")
  expect_false(grepl("\"ok\"", msg))
  expect_match(msg, "yes/no")
  # the same with other = FALSE, and from values given with no type
  expect_error(codebook(cb_df(variable = "sexo", values = "H/M", description = "D."), other = FALSE),
               class = "amanuensis_error_input", regexp = "only one allowed value")
  # two values are fine
  expect_identical(quietly(codebook(cb_df(variable = "sexo", values = "H; M", description = "D.")))$variables$values[[1]],
                   c("H", "M", "other"))
})

test_that("category values written one per row below their variable are all kept", {
  dir <- withr::local_tempdir()
  # one value per row, the variable's other cells left empty (also what readxl
  # gives for a variable name merged down over its values)
  sheet <- data.frame(variable = c("surname", "sex", NA, "status", NA, NA, "age"),
                      type = c("text", "category", NA, "category", NA, NA, "integer"),
                      values = c(NA, "male", "female", "single", "married", "widowed", NA),
                      description = c("Surname.", "Sex as recorded.", NA, "Marital status.", NA, NA, "Age."),
                      stringsAsFactors = FALSE)
  path <- file.path(dir, "rows.xlsx")
  writexl::write_xlsx(list(codebook = sheet), path)
  msgs <- all_condition_text(cb <- codebook(path))
  expect_identical(cb$variables$variable, c("surname", "sex", "status", "age"))
  expect_identical(cb$variables$values[[2]], c("male", "female", "other"))
  expect_identical(cb$variables$values[[3]], c("single", "married", "widowed", "other"))
  expect_identical(cb$schema$properties$records$items$properties$status$enum,
                   list("single", "married", "widowed", "other", NULL))
  expect_true(any(grepl("rows below .sex. and .status.", msgs)))
  # the same from a data frame and from a CSV
  expect_identical(quietly(codebook(sheet))$variables$values, cb$variables$values)
  csv <- file.path(dir, "rows.csv")
  .write_csv(sheet, csv)
  expect_identical(quietly(codebook(csv))$variables$values, cb$variables$values)
})

test_that("category values on a row of their own, away from a category variable, stop with the row", {
  # first row of the table
  expect_error(codebook(cb_df(variable = c("", "sex"), type = c("", "category"), values = c("x", "H; D"),
                              description = c("", "Sex."))),
               class = "amanuensis_error_input", regexp = "Row 2 .*category values but no variable")
  # after an empty row
  expect_error(codebook(cb_df(variable = c("sex", "", ""), type = c("category", "", ""), values = c("H; D", "", "x"),
                              description = c("Sex.", "", ""))),
               class = "amanuensis_error_input", regexp = "Row 4 .*category values but no variable")
  # below a text variable
  expect_error(codebook(cb_df(variable = c("surname", ""), type = c("text", ""), values = c("", "x"),
                              description = c("S.", ""))),
               class = "amanuensis_error_input", regexp = "Row 3 .*category values but no variable")
})

test_that("an exact column name beats a synonym; loose synonyms never replace a real column", {
  # notes before description: the descriptions sent to Google are the real ones
  msgs <- all_condition_text(cb <- codebook(cb_df(variable = c("surname", "age"), notes = c("checked by MP", "todo"),
                                                  description = c("Surname exactly as written", "Age in years"),
                                                  type = c("text", "integer"))))
  expect_identical(cb$variables$description, c("Surname exactly as written", "Age in years"))
  expect_match(cb$schema$properties$records$items$properties$surname$description, "^Surname exactly as written")
  expect_true(any(grepl("Ignoring codebook column .notes.", msgs)))
  expect_false(any(grepl("Ignoring codebook column .description.", msgs)))
  # a Stata-style label column before the description
  cb <- quietly(codebook(cb_df(variable = "surname", label = "Surname", description = "Surname as written.")))
  expect_identical(cb$variables$description, "Surname as written.")
  # a measurement unit is not a level, a date format is not a type
  msgs <- all_condition_text(cb <- codebook(cb_df(variable = c("town", "revenue", "area"),
                                                  type = c("text", "number", "number"),
                                                  unit = c("", "pesetas", "hectares"),
                                                  description = c("Town.", "Revenue.", "Area."))))
  expect_identical(cb$variables$level, c("row", "row", "row"))
  expect_true(any(grepl("Ignoring codebook column .unit.", msgs)))
  for (df in list(cb_df(variable = c("d", "n"), type = c("text", "integer"), format = c("dd/mm/yyyy", ""), description = "D."),
                  cb_df(variable = c("d", "n"), format = c("dd/mm/yyyy", ""), type = c("text", "integer"), description = "D."))) {
    expect_identical(quietly(codebook(df))$variables$type, c("text", "integer"))
  }
  # the template with a unit column added after type keeps its own level column
  dir <- withr::local_tempdir()
  tmpl <- file.path(dir, "t.xlsx")
  quietly(codebook_template(tmpl))
  sheet <- as.data.frame(readxl::read_excel(tmpl, sheet = "codebook"))
  sheet <- cbind(sheet[, 1:2], unit = c("", "", "", "years", "", "", ""), sheet[, -(1:2)])
  cb <- quietly(codebook(sheet))
  expect_identical(cb$variables$level, c("page", rep("row", 6)))
  # with no real column, a loose synonym is read, and the user is told
  msgs <- all_condition_text(cb <- codebook(cb_df(variable = c("a", "b"), notes = c("A.", "B."), kind = c("text", "integer"))))
  expect_identical(cb$variables$description, c("A.", "B."))
  expect_identical(cb$variables$type, c("text", "integer"))
  expect_true(any(grepl("Reading the descriptions from column .notes.", msgs)))
  expect_true(any(grepl("Reading the types from column .kind.", msgs)))
  # two columns for one thing: the first is used and the other named
  msgs <- all_condition_text(cb <- codebook(cb_df(variable = "a", descripcion = "Uno.", description = "One.")))
  expect_identical(cb$variables$description, "One.")
  expect_true(any(grepl("Ignoring codebook column .descripcion.: the descriptions are read from column .description.", msgs)))
})

test_that("a title such as 'Variables' above the header is not taken for the header", {
  dir <- withr::local_tempdir()
  for (title in c("Variables", "Campos", "Variables of the 1930 padro")) {
    path <- file.path(dir, paste0(title, ".xlsx"))
    sheet <- cb_df(A = c(title, NA, "variable", "surname", "age", "marital"),
                   B = c(NA, NA, "type", "text", "integer", "category"),
                   C = c(NA, NA, "description", "Surname.", "Age.", "Marital status."),
                   D = c(NA, NA, "values", NA, NA, "single; married; widowed"))
    writexl::write_xlsx(list(codebook = sheet), path, col_names = FALSE)
    cb <- quietly(codebook(path))
    expect_identical(cb$variables$variable, c("surname", "age", "marital"))
    expect_identical(cb$variables$type, c("text", "integer", "category"))
    expect_identical(cb$variables$values[[3]], c("single", "married", "widowed", "other"))
  }
  # a header with only a variable column still works under a title
  path <- file.path(dir, "one.xlsx")
  writexl::write_xlsx(list(codebook = cb_df(A = c("My codebook", "variable", "surname"))), path, col_names = FALSE)
  expect_identical(quietly(codebook(path))$variables$variable, "surname")
})

test_that("as_written adds a text twin just before each number", {
  cb <- quietly(codebook(cb_df(variable = c("name", "amount", "n"), type = c("text", "number", "integer"),
                               description = c("N.", "Amount.", "Count.")), as_written = TRUE))
  expect_identical(cb$variables$variable, c("name", "amount_as_written", "amount", "n_as_written", "n"))
  expect_identical(cb$variables$type, c("text", "text", "number", "text", "integer"))
  expect_match(cb$variables$description[2], "exactly as written")
})

test_that("reserved, duplicate and missing names stop with the row to fix", {
  expect_error(codebook(cb_df(variable = c("file", "x"), description = "D.")), class = "amanuensis_error",
               regexp = "cannot be used")
  expect_error(codebook(cb_df(variable = "row", description = "D.")), class = "amanuensis_error", regexp = "printed_row")
  expect_error(codebook(cb_df(variable = c("a", "a"), description = "D.")), class = "amanuensis_error",
               regexp = "more than once")
  expect_error(codebook(cb_df(variable = c("a", ""), description = c("A.", "B."))), class = "amanuensis_error",
               regexp = "Row 3")
  expect_error(codebook(cb_df(variable = "a", type = "colour")), class = "amanuensis_error", regexp = "Row 2.*colour")
  expect_error(codebook(cb_df(variable = "a", level = "chapter")), class = "amanuensis_error", regexp = "level")
  expect_error(codebook(cb_df(variable = "a", type = "category")), class = "amanuensis_error",
               regexp = "needs a list of allowed values")
  expect_error(codebook(cb_df(name_of_thing = "a")), class = "amanuensis_error", regexp = "column called")
  expect_error(codebook(cb_df(variable = character())), class = "amanuensis_error_input", regexp = "no rows")
})

test_that("missing descriptions are pointed out", {
  msgs <- all_condition_text(codebook(cb_df(variable = c("a", "b"), description = c("A.", ""))))
  expect_true(any(grepl("1 of 2 variables has no description", msgs)))
})

test_that("free text alone, and free text with a table", {
  ft <- codebook(free_text = TRUE)
  expect_identical(ft$kind, "text")
  expect_identical(ft$variables$variable, "text")
  expect_identical(names(ft$schema$properties), "text")
  expect_identical(ft$schema$properties$text$description, .text_description_alone)
  expect_output(print(ft), "free text")

  both <- quietly(codebook(cb_df(variable = c("folio", "name"), level = c("page", "row"),
                                 description = c("F.", "N.")), free_text = TRUE))
  expect_identical(both$kind, "table")
  expect_true(both$free_text)
  expect_identical(both$variables$variable, c("folio", "name", "text"))
  expect_identical(both$variables$level[3], "page")
  expect_identical(names(both$schema$properties), c("folio", "records", "text"))
  expect_identical(both$schema$properties$text$description, .text_description_with_records)
  expect_error(quietly(codebook(cb_df(variable = "text", description = "T."), free_text = TRUE)),
               class = "amanuensis_error_input", regexp = "already has a variable called")
  expect_error(codebook(), class = "amanuensis_error", regexp = "free_text = TRUE")
})

test_that("a JSON schema is kept as it is, from text or a file", {
  js <- '{"type": "object", "properties": {"folio": {"type": ["string", "null"]},
          "people": {"type": "array", "items": {"type": "object",
            "properties": {"name": {"type": "string"}, "sex": {"enum": ["H", "D", null]}}}}}}'
  cb <- codebook(json = js)
  expect_identical(cb$kind, "json")
  expect_identical(cb$schema, .parse_json(js))
  expect_identical(cb$variables$variable, c("folio", "name", "sex"))
  expect_identical(cb$variables$level, c("page", "row", "row"))
  expect_identical(cb$variables$type, c("text", "text", "category"))
  expect_identical(cb$variables$values[[3]], c("H", "D"))
  expect_identical(.records_path(cb), "people")
  # a string that starts with { is JSON
  expect_identical(codebook(js)$schema, cb$schema)
  dir <- withr::local_tempdir()
  path <- file.path(dir, "schema.json")
  writeLines(js, path)
  fromfile <- codebook(path)
  expect_identical(fromfile$schema, cb$schema)
  expect_identical(fromfile$source, path)
  expect_message(codebook(json = js, free_text = TRUE), "ignored for a JSON schema")
})

test_that("JSON schema input is checked", {
  expect_error(codebook(json = "{not json"), class = "amanuensis_error", regexp = "not valid JSON")
  expect_error(codebook(json = "[1, 2]"), class = "amanuensis_error", regexp = "JSON object")
  expect_error(codebook(json = '{"type": "array"}'), class = "amanuensis_error", regexp = "top level")
  expect_error(codebook(json = '{"type": "object", "properties": {"file": {"type": "string"}}}'),
               class = "amanuensis_error", regexp = "provenance")
  expect_message(codebook(json = '{"type": "object", "properties": {"a": {"type": "string", "format": "date"}},
                                   "minProperties": 1, "patternProperties": {}}'),
                 "not checked locally.*minProperties.*patternProperties")
  expect_error(codebook(file = "a.csv", json = "{}"), class = "amanuensis_error_input", regexp = "not both")
})

test_that("a JSON schema whose fields would share a column, or overwrite provenance, is refused", {
  # the same name for the page and for each entry
  expect_error(codebook(json = '{"type": "object", "properties": {"notes": {"type": ["string", "null"]},
    "entries": {"type": "array", "items": {"type": "object", "properties": {
      "name": {"type": "string"}, "notes": {"type": "string"}}}}}}'),
    class = "amanuensis_error_input", regexp = "column .notes.*entry_notes")
  # a further list (its own Excel sheet) with fields called source and row
  expect_error(codebook(json = '{"type": "object", "properties": {
    "entries": {"type": "array", "items": {"type": "object", "properties": {"name": {"type": "string"}}}},
    "footnotes": {"type": "array", "items": {"type": "object", "properties": {
      "marker": {"type": "string"}, "source": {"type": "string"}, "row": {"type": "string"}}}}}}'),
    class = "amanuensis_error_input", regexp = "footnotes.*source.*row.*provenance")
  # different names, or the same name in two different lists, are fine
  cb <- quietly(codebook(json = '{"type": "object", "properties": {"page_notes": {"type": ["string", "null"]},
    "entries": {"type": "array", "items": {"type": "object", "properties": {"notes": {"type": "string"}}}},
    "footnotes": {"type": "array", "items": {"type": "object", "properties": {"notes": {"type": "string"}}}}}}'))
  expect_identical(cb$variables$variable, c("page_notes", "notes"))
  expect_output(print(cb), "one row per entry of 'entries'")
})

test_that("the main array of a nested schema is found depth first (tables -> rows)", {
  s <- .parse_json('{"type": "object", "properties": {
    "page_no": {"type": ["string", "null"]},
    "meta": {"type": "object", "properties": {"archive": {"type": "string"}}},
    "tags": {"type": "array", "items": {"type": "string"}},
    "tables": {"type": "array", "items": {"type": "object", "properties": {
       "title": {"type": "string"},
       "rows": {"type": "array", "items": {"type": "object", "properties": {
          "name": {"type": "string"}, "age": {"type": ["integer", "null"]}, "paid": {"type": "boolean"},
          "amount": {"type": "number"}}}}}}}}}')
  m <- .main_array_path(s)
  expect_identical(m$path, c("tables", "rows"))
  expect_identical(names(m$items$properties), c("name", "age", "paid", "amount"))
  vars <- .vars_from_schema(s)
  expect_identical(vars$variable, c("page_no", "meta.archive", "name", "age", "paid", "amount"))
  expect_identical(vars$level, c("page", "page", "row", "row", "row", "row"))
  expect_identical(vars$type, c("text", "text", "text", "integer", "boolean", "number"))
  expect_null(.main_array_path(.parse_json('{"type": "object", "properties": {"a": {"type": "string"}}}')))
  none <- .vars_from_schema(.parse_json('{"type": "object", "properties": {}}'))
  expect_identical(nrow(none), 0L)
})

test_that("codebook files that do not exist or have the wrong type are refused", {
  expect_error(codebook("nowhere/codebook.xlsx"), class = "amanuensis_error_input", regexp = "Cannot find")
  dir <- withr::local_tempdir()
  p <- file.path(dir, "cb.docx")
  writeLines("x", p)
  expect_error(codebook(p), class = "amanuensis_error_input", regexp = "must be an Excel file")
})

test_that("codebook_template() writes a starter that codebook() reads", {
  dir <- withr::local_tempdir()
  x <- file.path(dir, "cb.xlsx")
  expect_message(codebook_template(x), "Wrote")
  cb <- quietly(codebook(x))
  expect_identical(cb$variables$variable[1:2], c("printed_page", "surname"))
  expect_identical(readxl::excel_sheets(x), c("codebook", "how_to"))
  expect_error(codebook_template(x), class = "amanuensis_error", regexp = "already exists")
  quietly(codebook_template(x, overwrite = TRUE))
  csv <- file.path(dir, "cb.csv")
  quietly(codebook_template(csv))
  expect_identical(quietly(codebook(csv))$variables$type, cb$variables$type)
  expect_error(codebook_template(file.path(dir, "cb.ods")), class = "amanuensis_error")
})

test_that(".as_codebook accepts objects, data frames and files, and defaults to free text", {
  cb <- roster_codebook()
  expect_identical(.as_codebook(cb), cb)
  expect_message(d <- .as_codebook(NULL), "transcribe each page as text")
  expect_identical(d$kind, "text")
  expect_s3_class(quietly(.as_codebook(cb_df(variable = "a", description = "A."))), "amanuensis_codebook")
  expect_error(.as_codebook(42), class = "amanuensis_error")
})

test_that(".fold_accents and .norm_header", {
  expect_identical(.fold_accents("Descripci\u00f3n \u00c0"), "Descripcion A")
  expect_identical(.norm_header("  Field_Name "), "field name")
})
