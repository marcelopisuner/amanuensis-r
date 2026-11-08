v <- function(json, schema) .validate(.parse_json(json), .parse_json(schema))

test_that("types are checked, including unions with null and integer as number", {
  s <- '{"type": ["string", "null"]}'
  expect_length(v('"a"', s), 0)
  expect_length(v("null", s), 0)
  expect_match(v("3", s), "expected text or empty \\(null\\), got 3")
  expect_length(v("3", '{"type": "number"}'), 0)
  expect_length(v("3", '{"type": "integer"}'), 0)
  expect_match(v("3.5", '{"type": "integer"}'), "expected a whole number")
  expect_match(v("true", '{"type": "string"}'), "got TRUE")
  expect_match(v('"x"', '{"type": "boolean"}'), "yes/no")
  expect_match(v("[1]", '{"type": "object"}'), "got a list")
  expect_match(v('{"a": 1}', '{"type": "array"}'), "got a set of fields")
  expect_length(v("null", '{"type": "string", "nullable": true}'), 0)
  expect_length(v("{}", '{"type": "object"}'), 0)
  expect_length(v("[]", '{"type": "array"}'), 0)
})

test_that("enum accepts null when listed and names the allowed values", {
  s <- '{"type": ["string", "null"], "enum": ["H", "D", null]}'
  expect_length(v('"H"', s), 0)
  expect_length(v("null", s), 0)
  err <- v('"X"', s)
  expect_match(err, "\"X\" is not one of the allowed values \\(H, D, empty\\)")
  expect_match(v("null", '{"enum": ["a"]}'), "not one of")
  expect_match(v("2", '{"const": 1}'), "must be exactly 1")
})

test_that("required and additionalProperties", {
  s <- '{"type": "object", "properties": {"a": {"type": "string"}, "b": {"type": "integer"}},
         "required": ["a", "b"], "additionalProperties": false}'
  expect_length(v('{"a": "x", "b": 1}', s), 0)
  expect_identical(v('{"a": "x"}', s), "b: is missing")
  expect_identical(v('{"a": "x", "b": 1, "c": 2}', s), "c: is not a field in the codebook")
  s2 <- '{"type": "object", "additionalProperties": {"type": "integer"}}'
  expect_length(v('{"x": 1}', s2), 0)
  expect_identical(v('{"x": "a"}', s2), "x: expected a whole number, got \"a\"")
})

test_that("items, minItems and maxItems, with paths to the bad value", {
  s <- '{"type": "object", "properties": {"records": {"type": "array", "minItems": 1, "maxItems": 2,
         "items": {"type": "object", "properties": {"age": {"type": ["integer", "null"]}}}}}}'
  expect_length(v('{"records": [{"age": 3}, {"age": null}]}', s), 0)
  expect_identical(v('{"records": [{"age": "x"}]}', s), "records[1].age: expected a whole number or empty (null), got \"x\"")
  expect_match(v('{"records": []}', s), "needs at least 1")
  expect_match(v('{"records": [{}, {}, {}]}', s), "at most 2 allowed")
  # tuple form
  expect_match(v('[1, "a"]', '{"type": "array", "items": [{"type": "integer"}, {"type": "integer"}]}'), "^\\[2\\]")
})

test_that("minimum, maximum and their exclusive forms; string lengths and pattern", {
  expect_match(v("0", '{"minimum": 1}'), "below the minimum 1")
  expect_match(v("9", '{"maximum": 5}'), "above the maximum 5")
  expect_match(v("1", '{"exclusiveMinimum": 1}'), "must be above 1")
  expect_match(v("5", '{"exclusiveMaximum": 5}'), "must be below 5")
  expect_match(v('"ab"', '{"minLength": 3}'), "shorter than 3")
  expect_match(v('"abcd"', '{"maxLength": 3}'), "longer than 3")
  expect_length(v('"\u00e9t\u00e9"', '{"maxLength": 3}'), 0)
  expect_length(v('"ab-12"', '{"pattern": "^[a-z]+-[0-9]+$"}'), 0)
  expect_match(v('"\u00e1b-12"', '{"pattern": "^[a-z]+-[0-9]+$"}'), "does not match the pattern")
  # an invalid regex is not held against the reply
  expect_length(v('"x"', '{"pattern": "[unclosed"}'), 0)
})

test_that("$ref to definitions, anyOf, oneOf, allOf and not", {
  s <- '{"definitions": {"person": {"type": "object", "properties": {"name": {"type": "string"}},
         "required": ["name"]}},
         "type": "object", "properties": {"who": {"$ref": "#/definitions/person"}}}'
  expect_length(v('{"who": {"name": "Puig"}}', s), 0)
  expect_identical(v('{"who": {}}', s), "who.name: is missing")
  any <- '{"anyOf": [{"type": "integer"}, {"type": "string", "maxLength": 2}]}'
  expect_length(v("3", any), 0)
  expect_length(v('"ab"', any), 0)
  expect_match(v('"abc"', any), "matches none of the allowed forms")
  # anyOf branches can use the root definitions
  s2 <- '{"$defs": {"n": {"type": "integer"}}, "anyOf": [{"$ref": "#/$defs/n"}, {"type": "null"}]}'
  expect_length(v("4", s2), 0)
  expect_match(v('"x"', s2), "none of the allowed forms")
  one <- '{"oneOf": [{"type": "number"}, {"type": "integer"}]}'
  expect_match(v("3", one), "more than one")
  expect_length(v("3.5", one), 0)
  expect_match(v('"x"', '{"allOf": [{"type": "string"}, {"minLength": 2}]}'), "shorter than 2")
  expect_match(v("null", '{"not": {"type": "null"}}'), "not allowed")
  expect_match(v("1", "false"), "no value is allowed")
  expect_length(v("1", "true"), 0)
})

test_that("the number of problems reported is capped", {
  s <- '{"type": "array", "items": {"type": "string"}}'
  bad <- paste0("[", paste(1:50, collapse = ","), "]")
  expect_length(.validate(.parse_json(bad), .parse_json(s)), 20)
  expect_length(.validate(.parse_json(bad), .parse_json(s), max = 3), 3)
})

test_that("a codebook's own schema validates the mock data and catches wrong replies", {
  cb <- roster_codebook()
  good <- roster_data("p001.jpg")
  expect_length(.validate(good, cb$schema), 0)
  bad <- good
  bad$records[[1]]$sex <- "unknown"
  bad$records[[2]]$extra <- 1
  bad$heading <- NULL
  err <- .validate(bad, cb$schema)
  expect_true(any(grepl("^heading: is missing", err)))
  expect_true(any(grepl("records\\[1\\]\\.sex: \"unknown\" is not one of", err)))
  expect_true(any(grepl("records\\[2\\]\\.extra: is not a field", err)))
})

test_that(".unchecked_keywords lists keywords the local check ignores, not property names", {
  s <- .parse_json('{"type": "object", "minProperties": 1, "properties": {"uniqueItems": {"type": "string"},
                    "list": {"type": "array", "uniqueItems": true, "items": {"type": "string", "if": {}}}}}')
  expect_identical(.unchecked_keywords(s), c("if", "minProperties", "uniqueItems"))
  expect_length(.unchecked_keywords(roster_codebook()$schema), 0)
})

test_that(".resolve_ref follows JSON pointers and gives NULL otherwise", {
  root <- list(definitions = list(`a/b` = list(type = "string")))
  expect_identical(.resolve_ref("#/definitions/a~1b", root), list(type = "string"))
  expect_null(.resolve_ref("#/definitions/missing", root))
  expect_null(.resolve_ref("other.json#/x", root))
})
