# The codebook is the contract: it becomes the JSON schema that Google enforces
# on every reply. Historians write it as a spreadsheet, one row per variable;
# this file turns that into a draft-07 schema following these rules:
# every field required and nullable (null = illegible or absent, never an
# omitted key), page-level fields first, then one array of records.
#
# Whether the model returns a table or free text is decided here
# (free_text = TRUE), not in output(): it changes what the model is asked, so
# it is part of the contract that is hashed into every result, while output()
# only re-presents results already obtained.

.reserved_names <- c("file", "source", "source_page", "row", "image", "page_status")

# Accepted spellings for the spreadsheet's column headers, after lower-casing
# and folding accents. English, Spanish and Catalan, since the users write in all three.
# The exact name (variable, type, description, values, level) always wins,
# wherever the column sits; then these synonyms, first in sheet order.
.codebook_headers <- list(
  variable = c("variable", "variables", "variable name", "name", "field", "field name", "var",
               "nombre", "nombre de la variable", "campo", "nom", "camp"),
  type = c("type", "data type", "tipo", "tipus"),
  description = c("description", "descripcion", "descripcio", "definition",
                  "instructions", "instruction", "what to write"),
  values = c("values", "allowed values", "categories", "category values", "options", "levels",
             "valores", "categorias", "valors", "categories permeses", "opciones"),
  level = c("level", "nivel", "nivell")
)

# Loose synonyms, used only when the codebook has no column of the kind above
# and (for type and level) only when every value in the column is a type or a
# level. Codebooks often carry columns of their own with these names that mean
# something else: a measurement "unit" (pesetas) is not a level, a date
# "format" (dd/mm/yyyy) is not a type, and private "notes" ("checked by MP")
# must not replace the descriptions sent to Google.
.codebook_headers_loose <- list(
  variable = character(),
  type = c("kind", "format"),
  description = c("label", "notes", "nota", "notas"),
  values = character(),
  level = c("scope", "unit")
)

# What each column holds, for messages.
.codebook_header_words <- c(variable = "the variable names", type = "the types",
                            description = "the descriptions", values = "the category values",
                            level = "the levels")

# Types a user may write, mapped to the package's five types.
.type_synonyms <- list(
  text = c("", "text", "string", "character", "char", "str", "texto", "words", "free text", "verbatim"),
  number = c("number", "numeric", "num", "double", "float", "decimal", "real", "numero", "nombre", "cifra"),
  integer = c("integer", "int", "whole number", "whole", "count", "entero", "numero entero", "enter", "sencer"),
  boolean = c("yes/no", "yes-no", "yes no", "yesno", "boolean", "bool", "logical", "true/false",
              "si/no", "si-no", "binary", "dummy"),
  category = c("category", "categories", "categorical", "factor", "enum", "choice", "list",
               "categoria", "categorica"),
  date = c("date", "fecha", "data", "year", "ano", "any")
)

.level_synonyms <- list(
  row = c("", "record", "records", "row", "rows", "line", "entry", "item", "person",
             "registro", "fila", "linea", "registre", "linia"),
  page = c("page", "pages", "image", "document", "sheet", "header", "card",
           "pagina", "imagen", "documento", "hoja", "full", "fitxa", "ficha")
)

#' Describe the variables you want: the codebook
#'
#' The codebook lists the variables to extract from every page, with a type
#' and a description for each. Write it as a spreadsheet (Excel or CSV) with
#' one row per variable, or build it as a data frame in R. The package turns
#' it into the formal schema that Google enforces, so every reply has exactly
#' these variables, and checks every reply against it again.
#'
#' @section The spreadsheet:
#' One row per variable, with these columns (only `variable` is required;
#' [codebook_template()] writes an example):
#' \describe{
#'   \item{variable}{Short name, e.g. `surname`. Spaces become `_`.}
#'   \item{type}{`text` (the default), `number`, `integer` (whole number),
#'     `yes/no`, or `category` (one of a fixed list of values). Dates and
#'     identifiers are best kept as `text`, exactly as printed.}
#'   \item{description}{What the model should write. This matters as much as
#'     the instructions: say what to copy, give an example as printed and what
#'     to return, and say what must never happen ("read it, never compute
#'     it").}
#'   \item{values}{For `category` only: the allowed values, separated by `;`
#'     or on separate lines in the cell (Alt+Enter in Excel), or one per row
#'     on the rows just below the variable, with its other cells left empty.
#'     Commas and slashes are kept as part of a value (`Barcelona, ciutat`),
#'     so a category needs at least two values: `H/M` stops with a message
#'     suggesting `H; M`. A catch-all `other` is added unless you include one
#'     (see `other`).}
#'   \item{level}{`row` (the default): one value per row of the table or
#'     list on the page. `page`: one value for the whole page, such as a
#'     printed page number or a heading; it is repeated on every row of the
#'     results.}
#' }
#' Every variable may be empty: the model returns an empty value when
#' something is illegible or not on the page, and never has to guess.
#'
#' A `number`, `integer`, `yes/no` or `category` variable asks the model to
#' interpret what is written (`"1 069 544"` becomes `1069544`; a value not in
#' the list becomes `other`). Use `text` to keep exactly what is written, or
#' `as_written = TRUE` to get both.
#'
#' @section Table or text:
#' Without `free_text`, the model returns a table: the page-level variables
#' and a list of records. With `free_text = TRUE` and no file, the model
#' returns a transcription of the whole page as text. With both, it returns
#' the table plus a `text` variable with any other writing on the page. This
#' choice belongs to the codebook (not to [output()]) because it changes what
#' the model is asked.
#'
#' @section JSON schemas:
#' Experienced users can give a JSON schema instead (a `.json` file, or the
#' text in `json`). It is sent to Google as it is and used to check every
#' reply.
#'
#' @param file A spreadsheet (`.xlsx`, `.xls`, `.csv`), a JSON schema
#'   (`.json`), or a data frame with the columns described below.
#' @param json A JSON schema as text, instead of a file.
#' @param free_text `TRUE` to ask for a full transcription of each page as
#'   text (see "Table or text"). Default `FALSE`.
#' @param record What one row of the table is, in a few words, e.g. `"person
#'   listed on the page"` or `"entry in the ledger"`. It tells the model what
#'   each row should be.
#' @param other For category variables: add a catch-all value `"other"`
#'   (unless the list already has one, e.g. `other`, `otro`, `altre`) so the
#'   model is never forced to pick a wrong value. Default `TRUE`.
#' @param as_written For number and integer variables: also ask for the value
#'   exactly as written, in a text variable `<name>_as_written` next to it.
#'   Default `FALSE`.
#' @param sheet For Excel files: the sheet with the codebook. Default: a sheet
#'   called "codebook", else the first sheet.
#' @return An `amanuensis_codebook` object to pass to [transcribe()].
#' @seealso [codebook_template()]
#' @export
#' @examples
#' cb <- codebook(data.frame(
#'   variable = c("surname", "first_name", "age"),
#'   type = c("text", "text", "integer"),
#'   description = c("Surname exactly as written.", "First name exactly as written.",
#'                   "Age in years, e.g. 34.")
#' ))
#' cb
#'
#' codebook(free_text = TRUE)              # whole-page transcription
#'
#' ex <- example_project()
#' codebook(ex$codebook, record = "person listed in the register")
codebook <- function(file = NULL, json = NULL, free_text = FALSE, record = NULL,
                     other = TRUE, as_written = FALSE, sheet = NULL) {
  if (!is.null(file) && !is.null(json)) .abort("Give either {.arg file} or {.arg json}, not both.", class = "amanuensis_error_input")
  if (!is.null(record)) .check_string(record, arg = "record")
  if (.is_string(file) && grepl("^\\s*\\{", file)) {
    json <- file
    file <- NULL
  }
  if (!is.null(json)) {
    .check_string(json, arg = "json")
    schema <- tryCatch(.parse_json(json), error = function(e) {
      .abort(c("{.arg json} is not valid JSON.", "x" = "{conditionMessage(e)}"))
    })
    return(.codebook_from_schema(schema, source = "json", free_text = free_text))
  }
  if (is.null(file)) {
    if (!isTRUE(free_text)) {
      .abort(c("Give a codebook file (or a data frame), or use {.code free_text = TRUE}.",
               "i" = "For a table: {.code codebook(\"codebook.xlsx\")}; {.fn codebook_template} writes an example to start from.",
               "i" = "For a plain transcription of each page: {.code codebook(free_text = TRUE)}."))
    }
    return(.codebook_text_only())
  }
  opts <- list(free_text = free_text, record = record, other = other, as_written = as_written)
  if (is.data.frame(file)) {
    return(do.call(.codebook_from_table, c(list(file, source = "data frame"), opts)))
  }
  .check_string(file, arg = "file")
  if (!file.exists(file)) {
    .abort(c("Cannot find the codebook {.path {(.show_path(file))}}.",
             "i" = "Check the spelling. Relative paths start from your working folder, {.path {(.show_path(getwd()))}}."),
           class = "amanuensis_error_input")
  }
  ext <- .ext(file)
  if (ext == "json") {
    schema <- tryCatch(.read_json(file), error = function(e) {
      .abort(c("{.file {basename(file)}} is not valid JSON.", "x" = "{conditionMessage(e)}"), class = "amanuensis_error_input")
    })
    return(.codebook_from_schema(schema, source = file, free_text = free_text))
  }
  tab <- switch(ext,
    xlsx = , xls = , xlsm = .read_codebook_sheet(file, sheet),
    csv = , tsv = , txt = .read_delim_text(file),
    .abort(c("The codebook must be an Excel file (.xlsx, .xls), a .csv file, or a .json schema.",
             "x" = "{.file {basename(file)}} has the extension {.val {ext}}."), class = "amanuensis_error_input")
  )
  do.call(.codebook_from_table, c(list(tab, source = file), opts))
}

.read_codebook_sheet <- function(path, sheet = NULL) {
  sheets <- readxl::excel_sheets(path)
  if (is.null(sheet)) {
    pick <- sheets[tolower(sheets) %in% c("codebook", "variables")]
    sheet <- if (length(pick)) pick[1] else sheets[1]
  } else if (!sheet %in% sheets) {
    .abort(c("{.file {basename(path)}} has no sheet called {.val {sheet}}.", "i" = "Its sheets are {.val {sheets}}."),
           class = "amanuensis_error_input")
  }
  tab <- readxl::read_excel(path, sheet = sheet, col_types = "text", col_names = FALSE, .name_repair = "minimal")
  tab <- as.data.frame(tab, stringsAsFactors = FALSE, optional = TRUE)
  first <- .codebook_header_row(tab)
  out <- tab[-seq_len(first), , drop = FALSE]
  names(out) <- ifelse(is.na(unlist(tab[first, ])), "", unlist(tab[first, ]))
  rownames(out) <- NULL
  attr(out, "header_row") <- first
  out
}

# The header is the first row that names a "variable" column and at least one
# other known column (a title row above the table is common, and a title such
# as "Variables" or "Campos" names a variable column on its own). A sheet
# whose header has only a variable column falls back to the first row that
# names one.
.codebook_header_row <- function(tab) {
  if (!nrow(tab)) return(1L)
  cells <- as.matrix(tab)
  cells[is.na(cells)] <- ""
  keys <- names(.codebook_headers)
  score <- vapply(seq_len(nrow(cells)), function(i) {
    h <- .norm_header(cells[i, ])
    if (!any(h %in% .codebook_headers$variable)) return(0L)
    sum(vapply(keys, function(k) any(h %in% c(.codebook_headers[[k]], .codebook_headers_loose[[k]])), logical(1)))
  }, integer(1))
  first <- which(score >= 2L)[1]
  if (is.na(first)) first <- which(score >= 1L)[1]
  if (is.na(first)) 1L else first
}

# A CSV/TSV read as text so nothing is guessed: delimiter sniffed from the
# header line (comma, semicolon or tab - Spanish Excel saves with ";"),
# BOM and encoding handled by .read_text().
.read_delim_text <- function(path) {
  text <- .read_text(path)
  header <- strsplit(text, "\n", fixed = TRUE)[[1]][1]
  counts <- c("," = lengths(regmatches(header, gregexpr(",", header, fixed = TRUE))),
              ";" = lengths(regmatches(header, gregexpr(";", header, fixed = TRUE))),
              "\t" = lengths(regmatches(header, gregexpr("\t", header, fixed = TRUE))))
  sep <- names(counts)[which.max(counts)]
  if (all(counts == 0)) sep <- ","
  tab <- utils::read.table(text = text, sep = sep, header = TRUE, quote = "\"", comment.char = "",
                           colClasses = "character", na.strings = character(), check.names = FALSE,
                           strip.white = TRUE, blank.lines.skip = TRUE, fill = TRUE, encoding = "UTF-8")
  tab[] <- lapply(tab, function(col) { Encoding(col) <- "UTF-8"; col })
  attr(tab, "sep") <- sep
  tab
}

# Lower-case, trim and fold Latin accents (written as escapes: R code is ASCII).
.fold_accents <- function(x) {
  from <- "\u00e1\u00e0\u00e4\u00e2\u00e3\u00e9\u00e8\u00eb\u00ea\u00ed\u00ec\u00ef\u00ee\u00f3\u00f2\u00f6\u00f4\u00f5\u00fa\u00f9\u00fc\u00fb\u00f1\u00e7\u00c1\u00c0\u00c4\u00c2\u00c3\u00c9\u00c8\u00cb\u00ca\u00cd\u00cc\u00cf\u00ce\u00d3\u00d2\u00d6\u00d4\u00d5\u00da\u00d9\u00dc\u00db\u00d1\u00c7"
  to <- "aaaaaeeeeiiiiooooouuuuncAAAAAEEEEIIIIOOOOOUUUUNC"
  chartr(from, to, utf8::utf8_normalize(.utf8_marked(x)))
}

.norm_header <- function(x) {
  x <- tolower(.fold_accents(x))
  x <- gsub("[_.]+", " ", x)
  gsub("\\s+", " ", trimws(x))
}

# Which column of the sheet holds each of variable, type, description,
# values and level: the exact name first, then a synonym, then a loose
# synonym (see .codebook_headers_loose). Says which columns are ignored and
# why, never naming a column "not one of" the names it has.
.codebook_columns <- function(tab, call = rlang::caller_env()) {
  heads <- .norm_header(names(tab))
  keys <- names(.codebook_headers)
  idx <- stats::setNames(rep(NA_integer_, length(keys)), keys)
  for (key in keys) {
    hit <- c(which(heads == key), which(heads %in% .codebook_headers[[key]]))
    if (length(hit)) idx[[key]] <- hit[1]
  }
  loose_used <- character()
  for (key in keys[is.na(idx)]) {
    for (j in setdiff(which(heads %in% .codebook_headers_loose[[key]]), idx)) {
      vals <- trimws(as.character(tab[[j]]))
      vals <- vals[!is.na(vals) & nzchar(vals)]
      fits <- length(vals) > 0 && switch(key,
        type = all(!is.na(vapply(vals, .match_synonym, "", synonyms = .type_synonyms))),
        level = all(!is.na(vapply(vals, .match_synonym, "", synonyms = .level_synonyms))),
        TRUE)
      if (fits) {
        idx[[key]] <- j
        loose_used[key] <- names(tab)[j]
        break
      }
    }
  }
  if (is.na(idx[["variable"]])) {
    .abort(c("The codebook needs a column called {.val variable} with the variable names.",
             "i" = "Its columns are: {.val {names(tab)}}.",
             "i" = "Expected columns: variable, type, description, values, level. {.fn codebook_template} writes an example."),
           call = call)
  }
  for (key in names(loose_used)) {
    used <- loose_used[[key]]
    what <- .codebook_header_words[[key]]
    .inform(c("i" = "Reading {what} from column {.val {used}}: the codebook has no {.val {key}} column."))
  }
  # columns named like one that is already taken (two description columns)
  for (key in keys[!is.na(idx)]) {
    also <- setdiff(which(heads %in% .codebook_headers[[key]]), idx[[key]])
    if (length(also)) {
      also_names <- names(tab)[also]
      used <- names(tab)[idx[[key]]]
      what <- .codebook_header_words[[key]]
      .inform(c("i" = "Ignoring codebook {cli::qty(length(also_names))}column{?s} {.val {also_names}}: {what} are read from column {.val {used}}."))
    }
  }
  named_like <- heads %in% unlist(.codebook_headers)
  unknown <- names(tab)[!seq_along(tab) %in% idx & !named_like]
  unknown <- unknown[nzchar(trimws(unknown))]
  if (length(unknown)) {
    .inform(c("i" = "Ignoring codebook column{?s} {.val {unknown}} (not one of variable, type, description, values, level)."))
  }
  idx
}

# The package's word for `x` in a list of synonyms (.type_synonyms,
# .level_synonyms); NA when it is none of them.
.match_synonym <- function(x, synonyms) {
  key <- tolower(.fold_accents(trimws(x)))
  for (s in names(synonyms)) if (key %in% synonyms[[s]]) return(s)
  NA_character_
}

.codebook_from_table <- function(tab, source, free_text = FALSE, record = NULL, other = TRUE,
                                 as_written = FALSE, call = rlang::caller_env()) {
  header_row <- attr(tab, "header_row") %||% 1L
  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  if (!nrow(tab)) .abort("The codebook has no rows. Add one row per variable.", call = call, class = "amanuensis_error_input")
  idx <- .codebook_columns(tab, call = call)
  get <- function(key) {
    if (is.na(idx[[key]])) return(rep("", nrow(tab)))
    v <- as.character(tab[[idx[[key]]]])
    v[is.na(v)] <- ""
    trimws(v)
  }
  var <- get("variable")
  desc_all <- get("description")
  type_all <- get("type")
  values_all <- get("values")
  level_all <- get("level")
  # A row with nothing but values, right below a variable (or below another
  # such row), holds more values of that variable: one category per row, or
  # the variable's name merged down over its values (Excel leaves the merged
  # cells empty). Anywhere else it is an error: dropping it would send Google
  # a shorter list, and every person with a dropped value would come back as
  # "other" for the whole paid run.
  no_name_row <- !nzchar(var) & !nzchar(desc_all) & !nzchar(type_all) & !nzchar(level_all)
  only_values <- no_name_row & nzchar(values_all)
  owner <- rep(NA_integer_, nrow(tab))
  current <- NA_integer_
  for (i in seq_len(nrow(tab))) {
    if (nzchar(var[i])) current <- i else if (only_values[i]) owner[i] <- current else current <- NA_integer_
  }
  # only a category (or a variable with no type, which values make one) takes them
  is_category <- !nzchar(type_all) | vapply(type_all, .match_synonym, "", synonyms = .type_synonyms) %in% "category"
  orphan <- only_values & (is.na(owner) | !is_category[owner])
  if (any(orphan)) {
    n_bad <- sum(orphan)
    rows_bad <- paste(which(orphan) + header_row, collapse = ", ")
    .abort(c("{cli::qty(n_bad)}Row{?s} {rows_bad} of the codebook {cli::qty(n_bad)}ha{?s/ve} category values but no variable.",
             "i" = "Write the values in the {.val values} cell of their variable, separated by {.val ;}, or on the rows directly below a category variable."),
           call = call, class = "amanuensis_error_input")
  }
  if (any(only_values)) {
    for (i in which(only_values)) values_all[owner[i]] <- paste(values_all[owner[i]], values_all[i], sep = "\n")
    took <- unique(var[owner[only_values]])
    .inform(c("i" = "Read the values on the rows below {.val {took}} as more category values of {cli::qty(length(took))}{?that variable/those variables}."))
  }
  keep <- !no_name_row
  sheet_row <- which(keep) + header_row
  var <- var[keep]
  if (any(!nzchar(var))) {
    # the row numbers are numbers, so cli would pluralise on their value: count them instead
    n_bad <- sum(!nzchar(var))
    rows_bad <- paste(sheet_row[!nzchar(var)], collapse = ", ")
    .abort("{cli::qty(n_bad)}Row{?s} {rows_bad} of the codebook {cli::qty(n_bad)}ha{?s/ve} a description, type or level but no variable name.", call = call)
  }
  clean <- gsub("\\s+", "_", var)
  renamed <- clean != var
  if (any(renamed)) {
    .inform(c("i" = "Variable names cannot contain spaces; renamed {.val {var[renamed]}} to {.val {clean[renamed]}}."))
  }
  var <- clean
  dup <- unique(var[duplicated(var)])
  if (length(dup)) .abort("The codebook lists {.val {dup}} more than once. Each variable needs its own name.", call = call)
  bad <- var[var %in% .reserved_names]
  if (length(bad)) {
    .abort(c("{.val {bad}} cannot be used as variable name{?s}: amanuensis uses {?it/them} to record where each row comes from.",
             "i" = "Rename it, for example {.val {paste0('printed_', bad[1])}}."), call = call)
  }

  type_raw <- type_all[keep]
  type <- vapply(seq_along(type_raw), function(i) .parse_type(type_raw[i], sheet_row[i], call), character(1))
  values_raw <- values_all[keep]
  values <- lapply(values_raw, .split_values)
  # values given but no type: the user clearly means a category
  implied <- type == "text" & !nzchar(type_raw) & lengths(values) > 0
  type[implied] <- "category"
  is_date <- type == "date"
  if (any(is_date)) {
    .inform(c("i" = "Dates are kept as text, exactly as written ({.val {var[is_date]}}). Convert them in R afterwards if you need to."))
    type[is_date] <- "text"
  }
  need <- type == "category" & lengths(values) == 0
  if (any(need)) {
    .abort(c("Category variable{?s} {.val {var[need]}} need{?s/} a list of allowed values.",
             "i" = "Write them in the {.val values} column separated by {.val ;}, e.g. {.val single; married; widowed; other}."), call = call)
  }
  extra <- type != "category" & lengths(values) > 0
  if (any(extra)) {
    .inform(c("i" = "Ignoring the values given for {.val {var[extra]}}, which {?is/are} not of type category."))
    values[extra] <- list(character())
  }
  # A category with one listed value is almost never meant: "H/M", "S/N" or
  # "soltero, casado, viudo" written in one piece (values are never split on
  # commas or slashes: think of "Barcelona, ciutat"). Google enforces the
  # list, so every row would come back as that one value or "other", and
  # fixing the codebook afterwards means paying for every page again. Stop
  # before anything is sent.
  single <- which(type == "category" & lengths(values) == 1)
  if (length(single)) {
    one_var <- var[single]
    one_val <- vapply(values[single], `[`, "", 1)
    guess <- ifelse(grepl("[/,]", one_val), gsub("\\s*[/,]\\s*", "; ", one_val), NA_character_)
    lines <- vapply(seq_along(single), function(k) {
      paste0("{.val {one_var[", k, "]}} lists only {.val {one_val[", k, "]}}",
             if (!is.na(guess[k])) paste0(" (did you mean {.val {guess[", k, "]}}?)") else "", ".")
    }, "")
    .abort(c("{cli::qty(length(single))}{?A category variable has/Category variables have} only one allowed value, so the model could only ever answer that value or {.val other}:",
             stats::setNames(lines, rep("*", length(lines))),
             "i" = "Separate the values with {.val ;} or put each on its own line in the cell (Alt+Enter in Excel), e.g. {.val H; M}. Commas and slashes are kept as part of a value.",
             "i" = "For a mark that is either there or not, use type {.val yes/no} instead."),
           call = call, class = "amanuensis_error_input")
  }
  if (isTRUE(other)) {
    escape <- c("other", "others", "otro", "otra", "otros", "altre", "altra", "altres", "autre", "other value")
    add <- type == "category" & !vapply(values, function(v) any(tolower(.fold_accents(v)) %in% escape), logical(1))
    values[add] <- lapply(values[add], function(v) c(v, "other"))
    if (any(add)) {
      .inform(c("i" = "Added the value {.val other} to {.val {var[add]}}, so the model is never forced to pick a wrong category ({.code other = FALSE} to turn this off)."))
    }
  }
  level_raw <- level_all[keep]
  level <- vapply(seq_along(level_raw), function(i) .parse_level(level_raw[i], sheet_row[i], call), character(1))
  desc <- desc_all[keep]

  vars <- data.frame(variable = var, type = type, level = level, description = desc,
                     stringsAsFactors = FALSE)
  vars$values <- values
  if (isTRUE(as_written) && any(type %in% c("number", "integer"))) {
    vars <- .add_as_written(vars)
  }
  if (sum(!nzchar(desc)) > 0) {
    n_missing <- sum(!nzchar(desc))
    .inform(c("!" = "{n_missing} of {length(desc)} variable{?s} {cli::qty(n_missing)}ha{?s/ve} no description.",
              "i" = "Descriptions guide the model: say what to copy, give an example as printed, and what must never happen."))
  }
  if (isTRUE(free_text) && "text" %in% vars$variable) {
    .abort("The codebook already has a variable called {.val text}; with {.code free_text = TRUE} that name is used for the page's text.",
           call = call, class = "amanuensis_error_input")
  }
  if (isTRUE(free_text)) {
    tv <- data.frame(variable = "text", type = "text", level = "page",
                     description = .text_description_with_records, stringsAsFactors = FALSE)
    tv$values <- list(character())
    vars_all <- rbind(vars, tv)
  } else {
    vars_all <- vars
  }
  schema <- .schema_from_vars(vars, free_text = isTRUE(free_text), record = record)
  structure(list(variables = vars_all, schema = schema, kind = "table", free_text = isTRUE(free_text),
                 record = record, source = source, sha12 = .sha12(.to_json(schema))),
            class = "amanuensis_codebook")
}

# For each number/integer variable, a text twin "<name>_as_written" placed
# just before it (an interpreted value sits next to the raw one, never
# replaces it).
.add_as_written <- function(vars) {
  out <- list()
  for (i in seq_len(nrow(vars))) {
    if (vars$type[i] %in% c("number", "integer")) {
      tw <- vars[i, ]
      tw$variable <- paste0(vars$variable[i], "_as_written")
      tw$type <- "text"
      tw$description <- paste0("The value of ", vars$variable[i], " exactly as written on the page, characters and spacing included (e.g. '1 069 544', '(66.0)'). Null if there is none.")
      tw$values <- list(character())
      out[[length(out) + 1]] <- tw
    }
    out[[length(out) + 1]] <- vars[i, ]
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

.parse_type <- function(x, row, call) {
  t <- .match_synonym(x, .type_synonyms)
  if (!is.na(t)) return(t)
  .abort(c("Row {row} of the codebook has type {.val {x}}, which is not one of the known types.",
           "i" = "Use text, number, integer, yes/no or category."), call = call)
}

.parse_level <- function(x, row, call) {
  l <- .match_synonym(x, .level_synonyms)
  if (!is.na(l)) return(l)
  .abort(c("Row {row} of the codebook has level {.val {x}}.",
           "i" = "Use {.val row} (one value per row of the table; the default) or {.val page} (one value per page)."), call = call)
}

# Category values are separated by line breaks, ";" or "|", never by commas
# (a value such as "Barcelona, ciutat" must survive).
.split_values <- function(x) {
  if (!nzchar(x)) return(character())
  v <- trimws(strsplit(x, "\n|;|\\|")[[1]])
  unique(v[nzchar(v)])
}

# Package wording that goes into the schema (and so into the contract hash):
# like the rules, never edit these texts in a released version - add new ones.
.text_description_alone <- paste(
  "Everything written on the image, transcribed exactly as written, in reading order.",
  "Start a new line only for a new paragraph, list item, line of verse or table row; join the lines",
  "of a paragraph with spaces and rejoin words split by a hyphen at the end of a line.",
  "Write a table as one line per row, with the cells separated by ' | '.",
  "Mark doubtful readings with '?' and illegible passages with [...].",
  "Return null if the image has no writing."
)

.text_description_with_records <- paste(
  "All other writing on the image that is not part of the records (for example headings, notes,",
  "signatures), transcribed exactly as written, in reading order. Null if there is none."
)

.type_suffix <- c(
  number = "Give a number. If unsure, give your best reading; null only if illegible or absent.",
  integer = "Give a whole number. If unsure, give your best reading; null only if illegible or absent.",
  boolean = "Answer true or false. Null if it cannot be told.",
  category = "Choose one of the listed values. If what is written is not in the list, choose 'other' if it is listed; null only if illegible or absent."
)

.json_type <- c(text = "string", number = "number", integer = "integer", boolean = "boolean", category = "string")

# `v` is one row of the variables table; its `values` cell is a list column.
.schema_property <- function(v) {
  p <- list(type = list(.json_type[[v$type]], "null"))
  desc <- v$description
  suffix <- .type_suffix[v$type]
  if (!is.na(suffix)) desc <- if (nzchar(desc)) paste(desc, suffix) else suffix
  if (nzchar(desc)) p$description <- desc
  vals <- v$values
  if (is.list(vals)) vals <- vals[[1]]
  if (v$type == "category") p$enum <- c(as.list(as.character(vals)), list(NULL))
  p
}

.schema_from_vars <- function(vars, free_text = FALSE, record = NULL) {
  page <- vars[vars$level == "page", , drop = FALSE]
  rec <- vars[vars$level == "row", , drop = FALSE]
  props <- list()
  for (i in seq_len(nrow(page))) props[[page$variable[i]]] <- .schema_property(page[i, ])
  if (nrow(rec)) {
    item_props <- list()
    for (i in seq_len(nrow(rec))) item_props[[rec$variable[i]]] <- .schema_property(rec[i, ])
    what <- record %||% "record (row of the table or entry of the list)"
    props$records <- list(
      type = "array",
      description = paste0("One object for each ", what, " on this image, in reading order. ",
                           "Return an empty list if there is none."),
      items = list(type = "object", properties = item_props,
                   required = as.list(names(item_props)), additionalProperties = FALSE)
    )
  }
  if (free_text) {
    props$text <- list(type = list("string", "null"),
                       description = if (nrow(vars)) .text_description_with_records else .text_description_alone)
  }
  list(type = "object", properties = props, required = as.list(names(props)), additionalProperties = FALSE)
}

.codebook_text_only <- function() {
  vars <- data.frame(variable = "text", type = "text", level = "page",
                     description = .text_description_alone, stringsAsFactors = FALSE)
  vars$values <- list(character())
  schema <- .schema_from_vars(vars[0, ], free_text = TRUE)
  structure(list(variables = vars, schema = schema, kind = "text", free_text = TRUE, record = NULL,
                 source = "free text", sha12 = .sha12(.to_json(schema))),
            class = "amanuensis_codebook")
}

# A user's own JSON schema: sent verbatim; variables derived for output/score.
.codebook_from_schema <- function(schema, source, free_text = FALSE, call = rlang::caller_env()) {
  if (!is.list(schema) || is.null(names(schema))) {
    brace <- "{"
    .abort("The JSON schema must be a JSON object (it starts with {.val {brace}}).", call = call)
  }
  if (!identical(unlist(schema$type)[1], "object") || is.null(schema$properties)) {
    example <- "{\"type\": \"object\", \"properties\": {...}}"
    .abort(c("The top level of the JSON schema must be an object with properties.",
             "i" = "For example {.code {example}}."), call = call)
  }
  if (isTRUE(free_text)) {
    .inform(c("i" = "{.arg free_text} is ignored for a JSON schema: add a text field to the schema itself if you want one."))
  }
  vars <- .vars_from_schema(schema)
  bad <- vars$variable[vars$variable %in% .reserved_names]
  if (length(bad)) {
    .abort("The schema uses {.val {bad}} as field name{?s}; amanuensis needs {?it/them} for provenance columns. Rename {?it/them}.", call = call)
  }
  # One column per name: with a page field and a record field of the same name
  # (notes, date) the table would show the page value on every row and lose
  # the record values. A codebook spreadsheet refuses duplicates too.
  dup <- unique(vars$variable[duplicated(vars$variable)])
  if (length(dup)) {
    .abort(c("The schema has more than one field that becomes the column {.val {dup}} of the results table (for example a field of the page and a field of each entry with the same name); the table can keep only one.",
             "i" = "Rename one of them, for example the one inside the list to {.val {paste0('entry_', dup[1])}}."),
           call = call, class = "amanuensis_error_input")
  }
  # The other lists of objects become sheets of their own in the Excel output,
  # which start with file, source, source_page and row: a field with one of
  # those names would be overwritten by the provenance.
  main <- .main_array_path(schema)
  for (arr in .object_arrays(schema)) {
    if (!is.null(main) && identical(arr$path, main$path)) next
    props <- arr$items$properties %||% list()
    plain <- vapply(props, function(p) {
      ty <- setdiff(unlist(p$type), "null")
      !(identical(ty, "object") || (!length(ty) && !is.null(p$properties)))
    }, logical(1))
    clash <- intersect(names(props)[plain], .provenance_cols)
    if (length(clash)) {
      where <- paste(arr$path, collapse = ".")
      .abort(c("The list {.val {where}} in the schema has {cli::qty(length(clash))}{?a field/fields} called {.val {clash}}; amanuensis needs {cli::qty(length(clash))}{?that name/those names} for provenance columns. Rename {cli::qty(length(clash))}{?it/them}.",
               "i" = "For example {.val {paste0(arr$path[length(arr$path)], '_', clash[1])}}."),
             call = call, class = "amanuensis_error_input")
    }
  }
  unchecked <- .unchecked_keywords(schema)
  if (length(unchecked)) {
    .inform(c("i" = "These schema keywords are sent to Google but not checked locally: {.val {unchecked}}."))
  }
  structure(list(variables = vars, schema = schema, kind = "json", free_text = FALSE, record = NULL,
                 source = source, sha12 = .sha12(.to_json(schema))),
            class = "amanuensis_codebook")
}

# Variables of a JSON schema as the flattened table will show them: root
# scalars are page level; the fields of the first array of objects (searched
# depth-first in schema order) are record level. Nested objects become
# parent.child columns.
.vars_from_schema <- function(schema) {
  schema <- .deref_schema_tree(schema)
  scalar_rows <- function(props, prefix, level) {
    out <- list()
    for (nm in names(props)) {
      p <- props[[nm]]
      ty <- setdiff(unlist(p$type), "null")
      full <- if (nzchar(prefix)) paste0(prefix, ".", nm) else nm
      if (identical(ty, "object") || (!length(ty) && !is.null(p$properties))) {
        out <- c(out, scalar_rows(p$properties %||% list(), full, level))
      } else if (identical(ty, "array")) {
        next
      } else {
        type <- if (!is.null(p$enum)) "category" else switch(ty[1] %||% "string",
          string = "text", integer = "integer", number = "number", boolean = "boolean", "text")
        vals <- if (!is.null(p$enum)) unlist(Filter(Negate(is.null), p$enum)) else character()
        out[[length(out) + 1]] <- list(variable = full, type = type, level = level,
                                       description = p$description %||% "", values = as.character(vals))
      }
    }
    out
  }
  rows <- scalar_rows(schema$properties, "", "page")
  main <- .main_array_path(schema)
  if (!is.null(main)) rows <- c(rows, scalar_rows(main$items$properties %||% list(), "", "row"))
  if (!length(rows)) {
    vars <- data.frame(variable = character(), type = character(), level = character(),
                       description = character(), stringsAsFactors = FALSE)
    vars$values <- list()
    return(vars)
  }
  vars <- data.frame(variable = vapply(rows, `[[`, "", "variable"),
                     type = vapply(rows, `[[`, "", "type"),
                     level = vapply(rows, `[[`, "", "level"),
                     description = vapply(rows, `[[`, "", "description"),
                     stringsAsFactors = FALSE)
  vars$values <- lapply(rows, `[[`, "values")
  vars
}

# The first array-of-objects in the schema (depth-first, schema order), with
# its path, e.g. c("tables", "rows"). NULL when there is none. Local $ref
# (items given as {"$ref": "#/$defs/person"}) are followed: the schema itself
# is sent to Google as written.
.main_array_path <- function(schema) {
  schema <- .deref_schema_tree(schema)
  find <- function(props, path) {
    for (nm in names(props)) {
      p <- props[[nm]]
      ty <- setdiff(unlist(p$type), "null")
      if (identical(ty, "array") && is.list(p$items) &&
          (identical(setdiff(unlist(p$items$type), "null"), "object") || !is.null(p$items$properties))) {
        inner <- find(p$items$properties %||% list(), c(path, nm))
        if (!is.null(inner)) return(inner)
        return(list(path = c(path, nm), items = p$items))
      }
      if (identical(ty, "object") && !is.null(p$properties)) {
        inner <- find(p$properties, c(path, nm))
        if (!is.null(inner)) return(inner)
      }
    }
    NULL
  }
  find(schema$properties %||% list(), character())
}

# Every array of objects in a schema, at any depth (inside objects and inside
# other arrays), as list(path, items), in schema order.
.object_arrays <- function(schema) {
  schema <- .deref_schema_tree(schema)
  out <- list()
  walk <- function(props, path) {
    for (nm in names(props)) {
      p <- props[[nm]]
      if (!is.list(p)) next
      ty <- setdiff(unlist(p$type), "null")
      if (identical(ty, "array") && is.list(p$items) &&
          (identical(setdiff(unlist(p$items$type), "null"), "object") || !is.null(p$items$properties))) {
        out[[length(out) + 1]] <<- list(path = c(path, nm), items = p$items)
        walk(p$items$properties %||% list(), c(path, nm))
      } else if (identical(ty, "object") || (!length(ty) && !is.null(p$properties))) {
        walk(p$properties %||% list(), c(path, nm))
      }
    }
  }
  walk(schema$properties %||% list(), character())
  out
}

# A copy of the schema with local references ("#/$defs/person") replaced by
# what they point to, for working out the variables and the lists; the schema
# sent to Google stays as written. A reference met again inside itself (a
# person whose children are persons) is left as it is, so this always ends.
.deref_schema_tree <- function(node, root = node, seen = character()) {
  if (!is.list(node) || !length(node)) return(node)
  ref <- node[["$ref"]]
  if (is.character(ref) && length(ref) == 1 && !ref %in% seen) {
    target <- .resolve_ref(ref, root)
    if (is.list(target)) {
      rest <- node[setdiff(names(node), "$ref")]
      merged <- c(rest, target[setdiff(names(target), names(rest))])
      return(.deref_schema_tree(merged, root, c(seen, ref)))
    }
  }
  lapply(node, .deref_schema_tree, root = root, seen = seen)
}

#' @export
print.amanuensis_codebook <- function(x, ...) {
  v <- x$variables
  src <- if (x$source %in% c("data frame", "json", "free text")) x$source else basename(x$source)
  if (x$kind == "text") {
    cat("Codebook: free text - the model transcribes each page as text.\n")
    return(invisible(x))
  }
  n_page <- sum(v$level == "page")
  n_rec <- sum(v$level == "row")
  cat("Codebook (", src, "): ", nrow(v), " variable", if (nrow(v) != 1) "s", sep = "")
  if (n_rec) cat(" - ", n_page, " per page, ", n_rec, " per row", sep = "")
  if (x$free_text) cat(", plus the page's other text")
  cat("\n")
  if (x$kind == "json") {
    main <- .main_array_path(x$schema)
    if (!is.null(main)) {
      cat("From a JSON schema: one row per entry of '", paste(main$path, collapse = "."),
          "'; other lists get sheets of their own in Excel output and are all kept in JSON output.\n", sep = "")
    } else {
      cat("From a JSON schema: one row per page; lists are kept in Excel and JSON output.\n")
    }
  }
  show <- data.frame(
    variable = v$variable, type = v$type, level = v$level,
    values = vapply(v$values, function(z) {
      s <- paste(z, collapse = "; ")
      if (nchar(s) > 30) paste0(substr(s, 1, 27), "...") else s
    }, ""),
    description = vapply(v$description, function(d) if (nchar(d) > 50) paste0(substr(d, 1, 47), "...") else d, "", USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  print(show, row.names = FALSE, right = FALSE)
  invisible(x)
}

#' Write a starter codebook spreadsheet
#'
#' Writes an example codebook, for a list of people, that you can open in
#' Excel and change into your own. The Excel version has a second sheet that
#' explains each column. Read your finished codebook with [codebook()].
#'
#' @param file Where to write it: an `.xlsx` (recommended) or `.csv` file.
#' @param overwrite Replace the file if it already exists? Default `FALSE`.
#' @return The path, invisibly.
#' @export
#' @examples
#' path <- file.path(tempdir(), "codebook.xlsx")
#' codebook_template(path)
#' codebook(path)
codebook_template <- function(file, overwrite = FALSE) {
  .check_string(file, arg = "file")
  if (file.exists(file) && !isTRUE(overwrite)) {
    .abort(c("{.file {file}} already exists.", "i" = "Choose another name, or use {.code overwrite = TRUE}."))
  }
  example <- data.frame(
    variable = c("printed_page", "surname", "first_name", "age", "sex", "occupation", "can_read"),
    type = c("text", "text", "text", "integer", "category", "text", "yes/no"),
    description = c(
      "The page number printed on the page, exactly as printed (e.g. '12' or 'xii'). Read it, never compute it. Empty if there is none.",
      "Surname(s) exactly as written, with the original spelling and accents (e.g. 'Puig i Ferrer').",
      "First name(s) exactly as written (e.g. 'Josep Maria'). Do not expand abbreviations such as 'Ma.'.",
      "Age in years as written (e.g. 34). Empty if not given.",
      "Sex as recorded in the source.",
      "Occupation exactly as written (e.g. 'jornaler').",
      "Whether the column 'sabe leer' is marked for this person."
    ),
    values = c("", "", "", "", "male; female; other", "", ""),
    level = c("page", "row", "row", "row", "row", "row", "row"),
    stringsAsFactors = FALSE
  )
  how <- data.frame(
    column = c("variable", "type", "description", "values", "level"),
    what_to_write = c(
      "A short name for the variable, without spaces (use _ instead), e.g. surname.",
      "text (default), number, integer (whole number), yes/no, or category (one value from a fixed list). Keep dates and identifiers as text.",
      "What the model should write: what to copy, an example as printed and what to return, and what must never happen. This guides the model as much as the instructions do.",
      "Only for category: the allowed values, separated by ; (include a catch-all such as 'other').",
      "record (default): one value per row of the table or list on the page. page: one value for the whole page (a printed page number, a heading)."
    ),
    stringsAsFactors = FALSE
  )
  ext <- .ext(file)
  if (ext == "xlsx") {
    writexl::write_xlsx(list(codebook = example, how_to = how), path = file)
  } else if (ext == "csv") {
    .write_csv(example, file, bom = TRUE)
  } else {
    .abort("The template must be an .xlsx or .csv file.")
  }
  .inform(c("v" = "Wrote {.file {(.show_path(file))}}.",
            "i" = "Change the rows to your own variables, save, then use {.code codebook(\"{basename(file)}\")}."))
  invisible(file)
}

.as_codebook <- function(x, call = rlang::caller_env()) {
  if (is.null(x)) {
    .inform(c("i" = "No codebook given: the model will transcribe each page as text ({.code codebook(free_text = TRUE)})."))
    return(.codebook_text_only())
  }
  if (inherits(x, "amanuensis_codebook")) return(x)
  if (is.data.frame(x) || .is_string(x)) return(codebook(x))
  .abort("{.arg codebook} must come from {.fn codebook}, or be a codebook file or data frame.", call = call)
}
