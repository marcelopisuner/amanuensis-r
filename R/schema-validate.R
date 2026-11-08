# Local validation of each reply against the schema. Google enforces the
# schema while generating, but it silently ignores keywords it does not
# support (pattern, minLength, ...), and replies have carried slugs with
# accents that only a local check caught, so every reply is checked again here.
#
# This is a plain-R validator for the draft-07 subset that codebook schemas use
# (type, including ["string","null"] unions, enum, const, properties, required,
# additionalProperties, items, minItems/maxItems, minimum/maximum and their
# exclusive forms, minLength/maxLength, pattern, anyOf/oneOf/allOf/not, local
# $ref). Other keywords (format, $id, title, ...) are accepted and not checked.
# It avoids jsonvalidate, which pulls in the V8 JavaScript engine.

.schema_keywords_checked <- c(
  "type", "enum", "const", "properties", "required", "additionalProperties",
  "items", "minItems", "maxItems", "minimum", "maximum", "exclusiveMinimum",
  "exclusiveMaximum", "minLength", "maxLength", "pattern", "anyOf", "oneOf",
  "allOf", "not", "$ref", "definitions", "$defs", "description", "title",
  "$schema", "$id", "format", "propertyOrdering", "default", "examples",
  "nullable", "$comment"
)

# Returns a character vector of problems (empty when valid), at most `max`.
.validate <- function(x, schema, max = 20) {
  env <- new.env(parent = emptyenv())
  env$errors <- character()
  env$max <- max
  .validate_node(x, schema, "", schema, env)
  env$errors
}

.add_error <- function(env, path, msg) {
  if (length(env$errors) < env$max) {
    env$errors <- c(env$errors, paste0(if (nzchar(path)) path else "(whole reply)", ": ", msg))
  }
}

.json_type_of <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) return(if (is.null(names(x))) "array" else "object")
  if (is.logical(x) && length(x) == 1) return(if (is.na(x)) "null" else "boolean")
  if (is.numeric(x) && length(x) == 1) {
    if (is.na(x)) return("null")
    return(if (is.finite(x) && x == round(x)) "integer" else "number")
  }
  if (is.character(x) && length(x) == 1) return(if (is.na(x)) "null" else "string")
  "unknown"
}

.type_matches <- function(actual, wanted) {
  actual == wanted || (wanted == "number" && actual == "integer")
}

.type_words <- c(null = "empty (null)", boolean = "yes/no (true or false)", integer = "a whole number",
                 number = "a number", string = "text", array = "a list", object = "a set of fields")

.resolve_ref <- function(ref, root) {
  if (!grepl("^#/", ref)) return(NULL)
  parts <- strsplit(sub("^#/", "", ref), "/", fixed = TRUE)[[1]]
  node <- root
  for (p in parts) {
    p <- gsub("~1", "/", gsub("~0", "~", p, fixed = TRUE), fixed = TRUE)
    node <- node[[p]]
    if (is.null(node)) return(NULL)
  }
  node
}

.values_equal <- function(a, b) {
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  if (is.list(a) || is.list(b)) return(identical(a, b))
  if (is.numeric(a) && is.numeric(b)) return(isTRUE(all.equal(as.numeric(a), as.numeric(b))))
  identical(a, b)
}

.validate_node <- function(x, s, path, root, env) {
  if (length(env$errors) >= env$max) return(invisible())
  if (isTRUE(s)) return(invisible())
  if (isFALSE(s)) {
    .add_error(env, path, "no value is allowed here")
    return(invisible())
  }
  if (!is.list(s)) return(invisible())
  if (!is.null(s[["$ref"]])) {
    target <- .resolve_ref(s[["$ref"]], root)
    if (!is.null(target)) .validate_node(x, target, path, root, env)
  }
  actual <- .json_type_of(x)

  if (!is.null(s$type)) {
    wanted <- unlist(s$type)
    if (isTRUE(s$nullable)) wanted <- union(wanted, "null")
    if (!any(vapply(wanted, function(w) .type_matches(actual, w), logical(1)))) {
      words <- paste(unname(.type_words[intersect(wanted, names(.type_words))]), collapse = " or ")
      .add_error(env, path, paste0("expected ", words, ", got ", .show_value(x)))
      return(invisible())
    }
  }
  if (!is.null(s$enum)) {
    if (!any(vapply(s$enum, function(v) .values_equal(x, v), logical(1)))) {
      allowed <- vapply(s$enum, function(v) if (is.null(v)) "empty" else as.character(v), character(1))
      .add_error(env, path, paste0(.show_value(x), " is not one of the allowed values (",
                                   paste(allowed, collapse = ", "), ")"))
    }
  }
  if ("const" %in% names(s) && !.values_equal(x, s$const)) {
    .add_error(env, path, paste0("must be exactly ", .show_value(s$const)))
  }

  if (actual == "object") {
    props <- s$properties %||% list()
    for (req in unlist(s$required)) {
      if (!req %in% names(x)) .add_error(env, .join_path(path, req), "is missing")
    }
    for (nm in names(x)) {
      sub <- props[[nm]]
      if (!is.null(sub)) {
        .validate_node(x[[nm]], sub, .join_path(path, nm), root, env)
      } else if (isFALSE(s$additionalProperties)) {
        .add_error(env, .join_path(path, nm), "is not a field in the codebook")
      } else if (is.list(s$additionalProperties)) {
        .validate_node(x[[nm]], s$additionalProperties, .join_path(path, nm), root, env)
      }
    }
  }

  if (actual == "array") {
    n <- length(x)
    if (!is.null(s$minItems) && n < s$minItems) .add_error(env, path, paste0("has ", n, " item(s), needs at least ", s$minItems))
    if (!is.null(s$maxItems) && n > s$maxItems) .add_error(env, path, paste0("has ", n, " item(s), at most ", s$maxItems, " allowed"))
    items <- s$items
    if (is.list(items) && !is.null(items) && is.null(names(items)) && length(items)) {
      # tuple form: one schema per position
      for (i in seq_len(min(n, length(items)))) .validate_node(x[[i]], items[[i]], paste0(path, "[", i, "]"), root, env)
    } else if (!is.null(items)) {
      for (i in seq_len(n)) .validate_node(x[[i]], items, paste0(path, "[", i, "]"), root, env)
    }
  }

  if (actual %in% c("integer", "number")) {
    if (!is.null(s$minimum) && x < s$minimum) .add_error(env, path, paste0(x, " is below the minimum ", s$minimum))
    if (!is.null(s$maximum) && x > s$maximum) .add_error(env, path, paste0(x, " is above the maximum ", s$maximum))
    if (is.numeric(s$exclusiveMinimum) && x <= s$exclusiveMinimum) .add_error(env, path, paste0(x, " must be above ", s$exclusiveMinimum))
    if (is.numeric(s$exclusiveMaximum) && x >= s$exclusiveMaximum) .add_error(env, path, paste0(x, " must be below ", s$exclusiveMaximum))
  }

  if (actual == "string") {
    n <- nchar(x, type = "chars")
    if (!is.null(s$minLength) && n < s$minLength) .add_error(env, path, paste0("is shorter than ", s$minLength, " characters"))
    if (!is.null(s$maxLength) && n > s$maxLength) .add_error(env, path, paste0("is longer than ", s$maxLength, " characters"))
    if (!is.null(s$pattern)) {
      # a pattern R cannot compile is not held against the reply (PCRE warns first)
      ok <- tryCatch(suppressWarnings(grepl(s$pattern, x, perl = TRUE)), error = function(e) NA)
      if (isFALSE(ok)) .add_error(env, path, paste0(.show_value(x), " does not match the pattern ", s$pattern))
    }
  }

  if (!is.null(s$allOf)) for (sub in s$allOf) .validate_node(x, sub, path, root, env)
  for (key in c("anyOf", "oneOf")) {
    alts <- s[[key]]
    if (is.null(alts)) next
    hits <- vapply(alts, function(sub) length(.validate(x, .with_root(sub, root), max = 1)) == 0, logical(1))
    if (!any(hits)) .add_error(env, path, paste0(.show_value(x), " matches none of the allowed forms"))
    if (key == "oneOf" && sum(hits) > 1) .add_error(env, path, "matches more than one of the allowed forms")
  }
  if (!is.null(s$not) && length(.validate(x, .with_root(s$not, root), max = 1)) == 0) {
    .add_error(env, path, "has a form that is not allowed")
  }
  invisible()
}

# Sub-schemas validated on their own still need the root's definitions.
.with_root <- function(sub, root) {
  if (!is.list(sub)) return(sub)
  if (!is.null(root$definitions) && is.null(sub$definitions)) sub$definitions <- root$definitions
  if (!is.null(root[["$defs"]]) && is.null(sub[["$defs"]])) sub[["$defs"]] <- root[["$defs"]]
  sub
}

.join_path <- function(path, name) if (nzchar(path)) paste0(path, ".", name) else name

.show_value <- function(x) {
  if (is.null(x)) return("empty (null)")
  if (is.list(x)) return(if (is.null(names(x))) "a list" else "a set of fields")
  if (is.character(x)) {
    s <- if (nchar(x) > 40) paste0(substr(x, 1, 37), "...") else x
    return(paste0("\"", s, "\""))
  }
  format(x)
}

# Keywords in a user's schema that the local check does not enforce.
.unchecked_keywords <- function(schema) {
  found <- character()
  walk <- function(node, in_props = FALSE) {
    if (!is.list(node)) return()
    nm <- names(node)
    if (!is.null(nm) && !in_props) found <<- c(found, nm)
    for (i in seq_along(node)) {
      key <- if (is.null(nm)) "" else nm[i]
      walk(node[[i]], in_props = key %in% c("properties", "definitions", "$defs"))
    }
  }
  walk(schema)
  sort(unique(setdiff(found, c(.schema_keywords_checked, ""))))
}
