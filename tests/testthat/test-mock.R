test_that(".mock_value invents data of every JSON-schema type", {
  root <- list()
  expect_identical(.mock_value(list(type = list("string", "null")), "name", 2L, "f.jpg", root), "name 2")
  expect_identical(.mock_value(list(type = "integer"), "age", 3L, "f.jpg", root), 3L)
  expect_identical(.mock_value(list(type = "number"), "x", 1L, "f.jpg", root), 1.5)
  expect_true(.mock_value(list(type = "boolean"), "x", 1L, "f.jpg", root))
  expect_false(.mock_value(list(type = "boolean"), "x", 2L, "f.jpg", root))
  enum <- list(type = list("string", "null"), enum = list("H", "D", NULL))
  expect_identical(vapply(1:3, function(i) .mock_value(enum, "s", i, "f", root), ""), c("H", "D", "H"))
  expect_null(.mock_value(list(enum = list(NULL)), "s", 1L, "f", root))
  expect_identical(.to_json(.mock_value(list(type = "object"), "o", 1L, "f", root)), "{}")
  expect_length(.mock_value(list(type = "array", items = list(type = "string")), "a", 1L, "f", root), 2)
  expect_length(.mock_value(list(type = "array", items = list(type = "object", properties = list(a = list(type = "string")))),
                            "a", 1L, "f", root), 3)
  expect_length(.mock_value(list(type = "array", minItems = 5, items = list(type = "string")), "a", 1L, "f", root), 5)
  expect_match(.mock_value(list(type = "string"), "text", 1L, "p001.jpg", root), "^Practice transcription of p001.jpg")
  expect_identical(.mock_value(list(anyOf = list(list(type = "integer"), list(type = "null"))), "n", 4L, "f", root), 4L)
  r <- list(definitions = list(n = list(type = "integer")))
  expect_identical(.mock_value(list(`$ref` = "#/definitions/n"), "n", 2L, "f", r), 2L)
  # OpenAPI-style schemas (upper-case types) from batch requests
  expect_identical(.mock_value(list(type = "INTEGER", nullable = TRUE), "n", 2L, "f", root), 2L)
  expect_identical(.mock_value(list(properties = list(a = list(type = "STRING"))), "o", 1L, "f", root), list(a = "a 1"))
  # the rest of JSON Schema a user may write: const, allOf, a null branch first
  expect_identical(.mock_value(list(const = "household"), "k", 1L, "f", root), "household")
  expect_identical(.mock_value(list(allOf = list(list(type = "integer"), list(minimum = 0))), "n", 3L, "f", root), 3L)
  expect_identical(.mock_value(list(oneOf = list(list(type = "null"), list(type = "integer"))), "n", 2L, "f", root), 2L)
  expect_null(.mock_value(list(type = "null"), "n", 1L, "f", root))
})

test_that("mock data fits every codebook it is asked about", {
  cb <- quietly(codebook(data.frame(
    variable = c("folio", "name", "age", "amount", "literate", "status"),
    type = c("text", "text", "integer", "number", "yes/no", "category"),
    level = c("page", "row", "row", "row", "row", "row"),
    values = c("", "", "", "", "", "single; married"),
    description = c("F.", "N.", "A.", "M.", "L.", "S.")), as_written = TRUE, free_text = TRUE))
  d <- .mock_value(cb$schema, "value", 1L, "p.jpg", cb$schema)
  expect_length(.validate(d, cb$schema), 0)
  expect_length(d$records, 3)
  expect_identical(names(d$records[[1]]), c("name", "age_as_written", "age", "amount_as_written", "amount",
                                            "literate", "status"))
  oa <- .to_response_schema(cb$schema)
  expect_length(.validate(.mock_value(oa, "value", 1L, "p.jpg", oa), cb$schema), 0)
})

test_that(".practice_reply answers camelCase and snake_case requests like Google", {
  cb <- roster_codebook()
  s <- list(resolution = "high", thinking_level = "LOW", max_tokens = 100L, schema_channel = "json")
  camel <- .build_body("Read.", "QUJD", "image/jpeg", cb$schema, s, case = "camel")
  r <- .practice_reply("mock", camel, "p001.jpg")
  expect_identical(r$candidates[[1]]$finishReason, "STOP")
  expect_identical(r$modelVersion, "mock")
  data <- .parse_json(r$candidates[[1]]$content$parts[[1]]$text)
  expect_length(.validate(data, cb$schema), 0)
  u <- r$usageMetadata
  expect_identical(u$totalTokenCount, u$promptTokenCount + u$candidatesTokenCount)
  expect_identical(u$promptTokensDetails[[1]], list(modality = "IMAGE", tokenCount = 2240L))
  snake <- .build_body("Read.", "QUJD", "image/jpeg", cb$schema, modifyList(s, list(schema_channel = "openapi")), case = "snake")
  r2 <- .practice_reply("mock", snake, "p001.jpg")
  expect_length(.validate(.parse_json(r2$candidates[[1]]$content$parts[[1]]$text), cb$schema), 0)
})

test_that("the practice server answers the package's endpoints and refuses others", {
  mock <- .practice_mock("mock")
  get <- function(path) mock(httr2::request(paste0("https://generativelanguage.googleapis.com/", path)))
  expect_identical(httr2::resp_status(get("v1beta/models")), 200L)
  unknown <- get("v1beta/somewhere")
  expect_identical(httr2::resp_status(unknown), 404L)
  expect_match(httr2::resp_body_json(unknown)$error$message, "mock: unknown endpoint")
  expect_identical(httr2::resp_status(get("v1beta/batches/mock-0")), 404L)
})

test_that("model = 'mock' needs no key, writes nothing by default, and says it is practice", {
  local_amanuensis()
  withr::local_envvar(GEMINI_API_KEY = NA)
  dir <- withr::local_tempdir()
  make_jpegs(file.path(dir, "pages"), 2)
  msgs <- all_condition_text(res <- transcribe(file.path(dir, "pages"), model = "mock"))
  expect_identical(res$text[1], "Practice transcription of p001.jpg.\nThe mock model invents this text; use a real model to read the page.")
  expect_null(.result_info(res)$folder)
  expect_identical(list.files(file.path(dir, "pages")), c("p001.jpg", "p002.jpg"))
  expect_true(any(grepl("practice model: nothing was charged", msgs)))
  expect_identical(.result_info(res)$pages$model_version, c("mock", "mock"))
})

test_that("a nested JSON schema gives the long table with enclosing fields and counters", {
  local_amanuensis()
  dir <- withr::local_tempdir()
  make_jpegs(file.path(dir, "pages"), 1)
  cb <- codebook(json = '{"type": "object", "properties": {
      "page_no": {"type": ["string", "null"]},
      "tables": {"type": "array", "items": {"type": "object", "properties": {
         "title": {"type": "string"},
         "rows": {"type": "array", "items": {"type": "object", "properties": {
            "name": {"type": "string"}, "age": {"type": ["integer", "null"]},
            "cells": {"type": "array", "items": {"type": "string"}}}}}}}}}}')
  res <- quietly(transcribe(file.path(dir, "pages"), test_instructions(), cb, model = "mock"))
  expect_identical(names(res), c("file", "source", "source_page", "row", "page_no", "tables.title", "tables_n",
                                 "cells_1", "cells_2", "name", "age"))
  expect_identical(nrow(res), 9L)    # 3 tables x 3 rows
  expect_identical(res$tables_n, rep(1:3, each = 3))
  expect_identical(res$row, 1:9)
  expect_type(res$age, "integer")
})

test_that("the example model knows only the example pages", {
  local_amanuensis()
  dir <- withr::local_tempdir()
  make_jpegs(file.path(dir, "pages"), 1)
  expect_error(quietly(transcribe(file.path(dir, "pages"), model = "example")), class = "amanuensis_error_input",
               regexp = "only knows the pages of the example project")
  replies <- .example_replies()
  skip_if(!length(replies), "the example project's replies are not installed yet")
  first <- names(replies)[1]
  expect_true(all(c("data", "tokens") %in% names(replies[[first]])))
  r <- .practice_reply("example", list(contents = list(list(parts = list(list(text = "x"))))), first)
  expect_identical(.parse_json(r$candidates[[1]]$content$parts[[1]]$text), replies[[first]]$data)
})
