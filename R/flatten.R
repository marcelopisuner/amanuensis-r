# From the model's JSON (one object per page) to one rectangular table: one
# row per record, provenance first (file, source, page, row - stamped by the
# package, never asked of the model), then the page-level variables repeated
# on every row, then the record variables in codebook order. Pages with no
# records do not appear here; the pages table lists every page.
#
# For a user's own JSON schema the table is a "long table": one row per
# element of the first array of objects, with the fields of any enclosing
# arrays repeated beside it (prefixed with their path) and an <array>_n
# counter for each enclosing level. Nested objects become parent.child
# columns; lists of plain values are spread over numbered columns
# (cells_raw_1, cells_raw_2, ...). The JSON output keeps everything.

.provenance_cols <- c("file", "source", "source_page", "row")

.is_json_object <- function(x) is.list(x) && !is.null(names(x))

# Scalars of an object, nested objects flattened to a.b names; arrays of
# objects skipped; arrays of plain values kept as lists (spread later).
# `skip` is the path of the records array (e.g. c("body", "people")): only
# that array is left out, so the other fields of an object that encloses it
# (body.volume) stay page variables. A path that runs through an array
# (tables -> rows) leaves out the whole array: its fields are repeated on
# the rows instead.
.object_scalars <- function(obj, prefix = "", skip = character()) {
  out <- list()
  for (nm in names(obj)) {
    v <- obj[[nm]]
    inner_skip <- character()
    if (length(skip) && identical(nm, skip[1])) {
      if (length(skip) == 1 || !.is_json_object(v)) next
      inner_skip <- skip[-1]
    }
    full <- if (nzchar(prefix)) paste0(prefix, ".", nm) else nm
    if (is.list(v) && !is.null(names(v)) && length(v)) {
      out <- c(out, .object_scalars(v, full, skip = inner_skip))
    } else if (is.list(v) && is.null(names(v))) {
      is_obj <- vapply(v, function(e) is.list(e) && !is.null(names(e)), logical(1))
      if (length(v) && all(is_obj)) next
      out[full] <- list(lapply(v, function(e) if (is.list(e)) .to_json(e) else e))
      attr(out[[full]], "spread") <- TRUE
    } else if (is.list(v) && !length(v)) {
      next
    } else {
      out[full] <- list(v)
    }
  }
  out
}

# Rows (named lists) of the records of one page. For codebook schemas the
# array is "records"; for JSON schemas it is the path found by
# .main_array_path(), possibly nested (tables -> rows) or inside a plain
# object (body -> people), which is stepped into, not iterated.
# An entry of an enclosing list whose own list is empty or missing (a
# household with no members listed) still gives one row, with its fields and
# empty record fields: no sheet of the output would hold it otherwise.
.page_rows <- function(data, path) {
  if (is.null(path) || !length(path)) return(list())
  walk <- function(node, path, parents) {
    arr <- node[[path[1]]]
    if (.is_json_object(arr)) {
      if (length(path) == 1) return(list())
      return(walk(arr, path[-1], parents))
    }
    if (!is.list(arr) || !length(arr)) return(list())
    rows <- list()
    for (i in seq_along(arr)) {
      el <- arr[[i]]
      if (!is.list(el)) next
      if (length(path) == 1) {
        rows[[length(rows) + 1]] <- c(parents, .object_scalars(el))
      } else {
        here <- .object_scalars(el, prefix = path[1])
        here[[paste0(path[1], "_n")]] <- i
        inner <- walk(el, path[-1], c(parents, here))
        rows <- c(rows, if (length(inner)) inner else list(c(parents, here)))
      }
    }
    rows
  }
  walk(data, path, list())
}

# The <array>_n counters .page_rows() adds for the enclosing lists of a path:
# only these are made whole numbers without a codebook type, never a field of
# the user's own that happens to end in _n (folio_n = "12v").
.counter_cols <- function(path) {
  if (length(path) < 2) return(character())
  paste0(path[-length(path)], "_n")
}

.records_path <- function(cb) {
  if (cb$kind == "json") {
    m <- .main_array_path(cb$schema)
    if (is.null(m)) NULL else m$path
  } else if (any(cb$variables$level == "row")) {
    "records"
  } else {
    NULL
  }
}

# Convert a list of JSON values to an R column of the codebook type.
# Whole numbers become an R integer column only when every value fits in one
# (R integers stop at 2,147,483,647); otherwise the column stays a double,
# which holds whole numbers exactly up to 2^53. Budget totals in pesetas or
# centimos pass 2.1 billion all the time, and as.integer() would turn them
# into NA on a page marked ok. Numbers written into text (a text column, a
# spread list) never use scientific notation: 3500000000, not 3.5e+09.
.as_column <- function(values, type) {
  is_null <- vapply(values, function(v) is.null(v) || (length(v) == 1 && is.na(v)), logical(1))
  pick <- function(fun, na) {
    out <- rep(na, length(values))
    if (any(!is_null)) out[!is_null] <- fun(vapply(values[!is_null], function(v) as.character(v)[1], ""))
    out
  }
  as_num <- function() {
    out <- rep(NA_real_, length(values))
    if (any(!is_null)) {
      out[!is_null] <- vapply(values[!is_null], function(v) {
        if (is.numeric(v)) as.numeric(v[1]) else suppressWarnings(as.numeric(as.character(v)[1]))
      }, numeric(1))
    }
    out
  }
  switch(type %||% "text",
    integer = {
      num <- as_num()
      fits <- is.na(num) | (num == round(num) & abs(num) <= .Machine$integer.max)
      if (all(fits)) as.integer(num) else num
    },
    number = as_num(),
    boolean = pick(function(s) tolower(s) %in% c("true", "1", "yes"), NA),
    {
      out <- rep(NA_character_, length(values))
      if (any(!is_null)) {
        out[!is_null] <- vapply(values[!is_null], function(v) {
          if (is.logical(v)) tolower(as.character(v))[1] else if (is.numeric(v)) .chr_values(v[1]) else as.character(v)[1]
        }, "")
      }
      out
    }
  )
}

# The records table for a set of page records (each with $file, $source,
# $page, $data). Returns a data.frame.
.flatten_records <- function(recs, cb) {
  vars <- cb$variables
  page_vars <- vars$variable[vars$level == "page"]
  rec_vars <- vars$variable[vars$level == "row"]
  path <- .records_path(cb)
  rows <- list()
  for (r in recs) {
    if (is.null(r$data)) next
    prov <- list(file = r$file, source = r$source %||% r$file, source_page = as.integer(r$source_page %||% NA))
    page_part <- .object_scalars(r$data, skip = path %||% character())
    if (is.null(path)) {
      rows[[length(rows) + 1]] <- c(prov, page_part)
    } else {
      rr <- .page_rows(r$data, path)
      for (k in seq_along(rr)) rows[[length(rows) + 1]] <- c(prov, list(row = k), page_part, rr[[k]])
    }
  }
  types <- stats::setNames(vars$type, vars$variable)
  first_cols <- if (is.null(path)) c("file", "source", "source_page") else .provenance_cols
  known <- c(page_vars, rec_vars)
  counters <- .counter_cols(path)
  if (!length(rows)) {
    empty <- c(list(file = character(), source = character(), source_page = integer()),
               if (!is.null(path)) list(row = integer()))
    for (v in known) empty[[v]] <- .as_column(list(), types[[v]])
    return(as.data.frame(empty, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE))
  }
  all_names <- unique(unlist(lapply(rows, names)))
  extra <- setdiff(all_names, c(first_cols, known))
  # enclosing-array columns (tables.title, tables_n) go before the record fields
  order_cols <- c(first_cols, intersect(page_vars, all_names), extra, intersect(rec_vars, all_names))
  order_cols <- c(order_cols, setdiff(known, order_cols))
  out <- list()
  for (col in order_cols) {
    vals <- lapply(rows, function(r) r[[col]])
    spread <- any(vapply(rows, function(r) isTRUE(attr(r[[col]], "spread")), logical(1)))
    if (spread) {
      width <- max(0, vapply(vals, length, integer(1)))
      for (j in seq_len(width)) {
        out[[paste0(col, "_", j)]] <- .as_column(lapply(vals, function(v) if (length(v) >= j) v[[j]] else NULL), "text")
      }
      next
    }
    type <- if (col %in% c("source_page", "row", setdiff(counters, known))) "integer" else
      if (col %in% c("file", "source")) "text" else types[col]
    if (is.na(type)) type <- .guess_type(vals)
    out[[col]] <- .as_column(vals, type)
  }
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
}

# Per-page units for scoring and checks: for each page, its page-level values
# (a one-row data frame of the codebook's page variables, NA when absent) and
# its rows (a data frame of the row variables, possibly zero rows). Unlike the
# flat table, a page with zero rows keeps its page values.
.page_unit <- function(data, cb) {
  vars <- cb$variables
  types <- stats::setNames(vars$type, vars$variable)
  page_vars <- vars$variable[vars$level == "page"]
  row_vars <- vars$variable[vars$level == "row"]
  path <- .records_path(cb)
  scal <- if (is.null(data)) list() else .object_scalars(data, skip = path %||% character())
  page <- as.data.frame(lapply(stats::setNames(page_vars, page_vars), function(v) .as_column(list(scal[[v]]), types[[v]])),
                        stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  rows_raw <- if (is.null(data) || is.null(path)) list() else .page_rows(data, path)
  rows <- as.data.frame(lapply(stats::setNames(row_vars, row_vars), function(v) {
    .as_column(lapply(rows_raw, function(r) r[[v]]), types[[v]])
  }), stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  if (!length(row_vars)) rows <- data.frame(row.names = seq_along(rows_raw))
  list(page = page, rows = rows)
}

# The same units from a flat table in the result/gold layout (file, row,
# page variables repeated or given on the first row of each page, row
# variables). A line with an empty `row` carries page values only (a page
# with no rows).
.units_from_table <- function(tab, cb) {
  vars <- cb$variables
  page_vars <- intersect(vars$variable[vars$level == "page"], names(tab))
  row_vars <- vars$variable[vars$level == "row"]
  files <- unique(tab$file)
  out <- list()
  for (f in files) {
    sub <- tab[tab$file == f, , drop = FALSE]
    page <- sub[1, page_vars, drop = FALSE]
    for (v in page_vars) {
      vals <- sub[[v]][!is.na(sub[[v]]) & as.character(sub[[v]]) != ""]
      page[[v]] <- if (length(vals)) vals[1] else sub[[v]][NA_integer_][1]
    }
    has_row <- if ("row" %in% names(sub)) !is.na(sub$row) & as.character(sub$row) != "" else rep(TRUE, nrow(sub))
    rows <- sub[has_row, intersect(row_vars, names(sub)), drop = FALSE]
    if ("row" %in% names(sub)) rows <- rows[order(suppressWarnings(as.numeric(sub$row[has_row]))), , drop = FALSE]
    rownames(rows) <- NULL
    out[[f]] <- list(page = page, rows = rows)
  }
  out
}

.guess_type <- function(vals) {
  v <- Filter(Negate(is.null), vals)
  if (!length(v)) return("text")
  if (all(vapply(v, is.logical, logical(1)))) return("boolean")
  if (all(vapply(v, is.numeric, logical(1)))) {
    return(if (all(vapply(v, function(x) x == round(x), logical(1)))) "integer" else "number")
  }
  "text"
}
