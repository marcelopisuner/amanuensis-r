# ------- request bodies -------------------------------------------------------

body_settings <- function(...) {
  s <- list(resolution = "high", thinking_level = "LOW", max_tokens = 32768L, temperature = NULL,
            flex = FALSE, api_args = list(), schema_channel = "json")
  utils::modifyList(s, list(...))
}

tiny_schema <- list(type = "object",
                    properties = list(sex = list(type = list("string", "null"), enum = list("H", "D", NULL)),
                                      firstName = list(type = list("string", "null"))),
                    required = list("sex", "firstName"), additionalProperties = FALSE)

test_that("the sync body is camelCase: image part first, then the text; no system instruction", {
  b <- .build_body("Read it.", "QUJD", "image/jpeg", tiny_schema, body_settings(), case = "camel")
  expect_identical(.to_json(b), paste0(
    '{"contents":[{"role":"user","parts":[{"inlineData":{"mimeType":"image/jpeg","data":"QUJD"}},{"text":"Read it."}]}],',
    '"generationConfig":{"responseMimeType":"application/json",',
    '"responseJsonSchema":{"type":"object","properties":{"sex":{"type":["string","null"],"enum":["H","D",null]},',
    '"firstName":{"type":["string","null"]}},"required":["sex","firstName"],"additionalProperties":false},',
    '"maxOutputTokens":32768,"thinkingConfig":{"thinkingLevel":"LOW","includeThoughts":false},',
    '"mediaResolution":"MEDIA_RESOLUTION_HIGH"}}'))
  expect_null(b$systemInstruction)
  expect_null(b$generationConfig$temperature)
  expect_null(b$serviceTier)
})

test_that("the batch body is snake_case with the same content", {
  b <- .build_body("Read it.", "QUJD", "image/jpeg", tiny_schema, body_settings(), case = "snake")
  expect_identical(.to_json(b), paste0(
    '{"contents":[{"role":"user","parts":[{"inline_data":{"mime_type":"image/jpeg","data":"QUJD"}},{"text":"Read it."}]}],',
    '"generation_config":{"response_mime_type":"application/json",',
    '"response_json_schema":{"type":"object","properties":{"sex":{"type":["string","null"],"enum":["H","D",null]},',
    '"firstName":{"type":["string","null"]}},"required":["sex","firstName"],"additionalProperties":false},',
    '"max_output_tokens":32768,"thinking_config":{"thinking_level":"LOW","include_thoughts":false},',
    '"media_resolution":"MEDIA_RESOLUTION_HIGH"}}'))
})

test_that("ultra_high goes on the image part; the other levels in the config", {
  b <- .build_body("x", "QUJD", "image/png", NULL, body_settings(resolution = "ultra_high"), case = "camel")
  part <- b$contents[[1]]$parts[[1]]
  expect_identical(part$mediaResolution, list(level = "MEDIA_RESOLUTION_ULTRA_HIGH"))
  expect_null(b$generationConfig$mediaResolution)
  expect_null(b$generationConfig$responseJsonSchema)
  s <- .build_body("x", "QUJD", "image/png", NULL, body_settings(resolution = "ultra_high"), case = "snake")
  expect_identical(s$contents[[1]]$parts[[1]]$media_resolution, list(level = "MEDIA_RESOLUTION_ULTRA_HIGH"))
  for (r in c("low", "medium", "high")) {
    b <- .build_body("x", "QUJD", "image/png", NULL, body_settings(resolution = r), case = "camel")
    expect_identical(b$generationConfig$mediaResolution, paste0("MEDIA_RESOLUTION_", toupper(r)))
    expect_null(b$contents[[1]]$parts[[1]]$mediaResolution)
  }
})

test_that("temperature is sent only when given; flex sets the service tier (sync only)", {
  b <- .build_body("x", "QUJD", "image/jpeg", NULL, body_settings(temperature = 0), case = "camel")
  expect_identical(b$generationConfig$temperature, 0)
  expect_match(.to_json(b), '"temperature":0')
  f <- .build_body("x", "QUJD", "image/jpeg", NULL, body_settings(flex = TRUE), case = "camel")
  expect_identical(f$serviceTier, "flex")
  fs <- .build_body("x", "QUJD", "image/jpeg", NULL, body_settings(flex = TRUE), case = "snake")
  expect_null(fs$serviceTier)
  expect_null(fs$service_tier)
})

test_that("the OpenAPI schema channel keeps enum, nullable and property order", {
  b <- .build_body("x", "QUJD", "image/jpeg", tiny_schema, body_settings(schema_channel = "openapi"), case = "snake")
  rs <- b$generation_config$response_schema
  expect_null(b$generation_config$response_json_schema)
  expect_identical(rs$type, "OBJECT")
  expect_identical(rs$propertyOrdering, list("sex", "firstName"))
  expect_identical(rs$properties$sex, list(type = "STRING", nullable = TRUE, enum = list("H", "D")))
  expect_identical(rs$properties$firstName, list(type = "STRING", nullable = TRUE))
  expect_identical(rs$required, list("sex", "firstName"))
  expect_null(rs$additionalProperties)
  camel <- .build_body("x", "QUJD", "image/jpeg", tiny_schema, body_settings(schema_channel = "openapi"), case = "camel")
  expect_identical(camel$generationConfig$responseSchema, rs)
})

test_that(".to_response_schema resolves $ref, keeps ranges and items, and writes empty properties as {}", {
  s <- list(definitions = list(n = list(type = "integer", minimum = 0, maximum = 9)),
            type = "object",
            properties = list(rows = list(type = "array", minItems = 1, items = list(`$ref` = "#/definitions/n")),
                              any = list(anyOf = list(list(type = "string"), list(type = "null"))),
                              loose = list(type = "object", properties = .json_object()),
                              cat = list(enum = list("a", NULL)),
                              pat = list(type = "string", pattern = "^a", format = "date")))
  out <- .to_response_schema(s)
  expect_identical(out$properties$rows$items, list(type = "INTEGER", minimum = 0, maximum = 9))
  expect_identical(out$properties$rows$minItems, 1)
  # OpenAPI has no null type: a null-only branch makes the node nullable, and
  # the one branch left is merged in (no string branch the schema never allowed)
  expect_identical(out$properties$any, list(type = "STRING", nullable = TRUE))
  expect_match(.to_json(out$properties$loose), '"properties":{}', fixed = TRUE)
  expect_identical(out$properties$cat, list(nullable = TRUE, type = "STRING", enum = list("a")))
  # "date" is not a format Google's Schema object accepts; date-time is
  expect_identical(out$properties$pat, list(type = "STRING"))
  expect_identical(.to_response_schema(list(type = "string", format = "date-time"))$format, "date-time")
})

test_that(".to_response_schema turns every JSON-schema node into one Google can parse", {
  s <- list(type = "object",
            properties = list(
              sex = list(oneOf = list(list(type = "string", enum = list("m", "f")), list(type = "null"))),
              age = list(anyOf = list(list(type = "integer"), list(type = "null"))),
              both = list(anyOf = list(list(type = "integer"), list(type = "string"), list(type = "null"))),
              place = list(allOf = list(list(type = "object", properties = list(a = list(type = "string")), required = list("a")),
                                        list(properties = list(b = list(type = "integer")), required = list("b")))),
              kind = list(const = "household"),
              year = list(type = "integer", enum = list(1930L, 1931L), format = "int32"),
              guess = list(enum = list(1930L, 1931L)),
              anything = list(description = "Any value.")),
            required = list("sex"))
  out <- .to_response_schema(s)
  j <- .to_json(out)
  expect_false(grepl('":[]', j, fixed = TRUE))
  p <- out$properties
  expect_identical(p$sex, list(type = "STRING", enum = list("m", "f"), nullable = TRUE))
  expect_identical(p$age, list(type = "INTEGER", nullable = TRUE))
  expect_identical(p$both, list(anyOf = list(list(type = "INTEGER"), list(type = "STRING")), nullable = TRUE))
  expect_identical(p$place$type, "OBJECT")
  expect_identical(names(p$place$properties), c("a", "b"))
  expect_identical(p$place$required, list("a", "b"))
  expect_identical(p$kind, list(type = "STRING", enum = list("household")))
  # enum only on strings: numbers keep their type, never stringified values
  expect_identical(p$year, list(type = "INTEGER", format = "int32"))
  expect_identical(p$guess, list(type = "INTEGER"))
  expect_identical(p$anything, list(description = "Any value.", type = "STRING"))
  # mock data from the converted schema fits the original one (numeric enums
  # aside: OpenAPI cannot carry them, so only the local check enforces them)
  s$properties[c("year", "guess")] <- NULL
  out <- .to_response_schema(s)
  expect_length(.validate(.mock_value(out, "value", 1L, "p.jpg", out), s), 0)
})

test_that("every required in a request serialises as a JSON array, in both channels", {
  cb <- quietly(codebook(data.frame(variable = c("folio", "name"), level = c("page", "row"),
                                    description = c("F.", "N."))))
  for (ch in c("json", "openapi")) for (case in c("camel", "snake")) {
    j <- .to_json(.build_body("x", "QUJD", "image/jpeg", cb$schema, body_settings(schema_channel = ch), case = case))
    expect_false(grepl('"required":"', j, fixed = TRUE))
    expect_match(j, '"required":["name"]', fixed = TRUE)
  }
})

test_that("api_args are deep-merged last; in batch their keys become snake_case, schema names untouched", {
  args <- list(generationConfig = list(seed = 7L, thinkingConfig = list(thinkingLevel = "HIGH")),
               safetySettings = list(list(category = "HARM_CATEGORY_HARASSMENT", threshold = "BLOCK_NONE")))
  b <- .build_body("x", "QUJD", "image/jpeg", NULL, body_settings(api_args = args), case = "camel")
  expect_identical(b$generationConfig$seed, 7L)
  expect_identical(b$generationConfig$thinkingConfig, list(thinkingLevel = "HIGH", includeThoughts = FALSE))
  expect_identical(b$generationConfig$maxOutputTokens, 32768L)
  expect_identical(b$safetySettings[[1]]$threshold, "BLOCK_NONE")
  s <- .build_body("x", "QUJD", "image/jpeg", tiny_schema, body_settings(api_args = args), case = "snake")
  expect_identical(s$generation_config$seed, 7L)
  expect_identical(s$generation_config$thinking_config, list(thinking_level = "HIGH", include_thoughts = FALSE))
  expect_null(s$generationConfig)
  expect_identical(names(s$generation_config$response_json_schema$properties), c("sex", "firstName"))
  expect_identical(s$safety_settings[[1]]$category, "HARM_CATEGORY_HARASSMENT")
  # a schema given through api_args keeps its property names and keywords in batch lines
  own <- list(generationConfig = list(responseJsonSchema = list(type = "object",
    properties = list(firstName = list(type = "string")), additionalProperties = FALSE)))
  s2 <- .build_body("x", "QUJD", "image/jpeg", NULL, body_settings(api_args = own), case = "snake")
  expect_identical(s2$generation_config$response_json_schema,
                   list(type = "object", properties = list(firstName = list(type = "string")), additionalProperties = FALSE))
  # NULL removes a field
  d <- .build_body("x", "QUJD", "image/jpeg", NULL, body_settings(api_args = list(generationConfig = list(thinkingConfig = NULL))), case = "snake")
  expect_null(d$generation_config$thinking_config)
})

test_that(".snake_keys converts field names only", {
  expect_identical(.snake_keys(list(maxOutputTokens = 1, a = list(responseMimeType = "x"))),
                   list(max_output_tokens = 1, a = list(response_mime_type = "x")))
  expect_identical(.snake_keys(list(list(fooBar = 1))), list(list(foo_bar = 1)))
  expect_identical(.snake_keys("camelText"), "camelText")
})

test_that(".schema_for_api drops $schema only", {
  s <- list(`$schema` = "http://json-schema.org/draft-07/schema#", type = "object")
  expect_identical(.schema_for_api(s), list(type = "object"))
  expect_null(.schema_for_api(NULL))
})

test_that(".mime_of knows what Gemini accepts", {
  expect_identical(.mime_of("a.JPG"), "image/jpeg")
  expect_identical(.mime_of("a.png"), "image/png")
  expect_identical(.mime_of("a.webp"), "image/webp")
  expect_identical(.mime_of("a.heic"), "image/heic")
  expect_error(.mime_of("a.tif"), class = "amanuensis_error", regexp = "images")
})

# ------- the request: URL and key ----------------------------------------------

test_that("the key travels only in a redacted x-goog-api-key header", {
  req <- .generate_request('{"a":1}', "gemini-3.8-flash", FAKE_KEY, timeout = 60)
  expect_identical(req$url, "https://generativelanguage.googleapis.com/v1beta/models/gemini-3.8-flash:generateContent")
  expect_false(grepl(FAKE_KEY, req$url, fixed = TRUE))
  expect_identical(httr2::req_get_headers(req, "reveal")[["x-goog-api-key"]], FAKE_KEY)
  expect_false(any(grepl(FAKE_KEY, capture.output(print(req)), fixed = TRUE)))
  expect_true(any(grepl("REDACTED", capture.output(print(req)))))
  expect_identical(.req_body_text(req), '{"a":1}')
  expect_match(httr2::req_get_headers(req)[["user-agent"]] %||% req$options$useragent, "^amanuensis/")
})

test_that("the base URL can be changed for a proxy", {
  withr::local_options(amanuensis.base_url = "https://proxy.test/")
  req <- .gemini_request("models", key = FAKE_KEY)
  expect_identical(req$url, "https://proxy.test/v1beta/models")
})

# ------- the retry loop ---------------------------------------------------------

local_sleeps <- function(env = parent.frame()) {
  rec <- new.env()
  rec$sleeps <- numeric()
  withr::local_options(amanuensis.sleep = function(s) rec$sleeps <- c(rec$sleeps, s), .local_envir = env)
  rec
}

scripted <- function(...) {
  replies <- list(...)
  env <- new.env()
  env$n <- 0L
  env$mock <- function(req) {
    env$n <- env$n + 1L
    r <- replies[[min(env$n, length(replies))]]
    if (is.function(r)) r() else r
  }
  env
}

perform <- function(srv, retries = 4, quiet = TRUE) {
  httr2::with_mocked_responses(srv$mock, .gemini_perform(.gemini_request("models", FAKE_KEY), retries = retries, quiet = quiet))
}

test_that("a 503 is retried and the 200 that follows is returned", {
  rec <- local_sleeps()
  srv <- scripted(error_response(503, "The model is overloaded."), ok_response())
  expect_message(resp <- perform(srv, quiet = FALSE), "busy or rate-limited \\(HTTP 503\\); waiting [56] s before try 2 of 5")
  expect_identical(httr2::resp_status(resp), 200L)
  expect_identical(srv$n, 2L)
  expect_length(rec$sleeps, 1)
  expect_true(rec$sleeps >= 5 && rec$sleeps <= 6)
})

test_that("Google's RetryInfo delay and the Retry-After header are honoured, capped at two minutes", {
  rec <- local_sleeps()
  srv <- scripted(error_response(429, "Quota exceeded.", retry_delay = "17s"), ok_response())
  resp <- perform(srv)
  expect_identical(rec$sleeps, 17)
  srv <- scripted(error_response(503, "Busy.", headers = list(`Retry-After` = "3")), ok_response())
  perform(srv)
  expect_identical(rec$sleeps, c(17, 3))
  srv <- scripted(error_response(429, "Quota exceeded.", retry_delay = "600s"), ok_response())
  perform(srv)
  expect_identical(rec$sleeps[3], 120)
  srv <- scripted(error_response(429, "Quota exceeded.", retry_delay = "0.2s"), ok_response())
  perform(srv)
  expect_identical(rec$sleeps[4], 1)
})

test_that("500, 502, 504 and 408 are retried; 400, 401, 403 and 404 are not", {
  rec <- local_sleeps()
  for (st in c(500L, 502L, 504L, 408L)) {
    srv <- scripted(error_response(st, "Trouble."), ok_response())
    expect_identical(httr2::resp_status(perform(srv)), 200L)
    expect_identical(srv$n, 2L)
  }
  for (st in c(400L, 401L, 403L, 404L)) {
    srv <- scripted(error_response(st, "No."), ok_response())
    expect_identical(httr2::resp_status(perform(srv)), st)
    expect_identical(srv$n, 1L)
  }
})

test_that("waits grow 5, 10, 20 s and the last reply is returned when retries run out", {
  rec <- local_sleeps()
  srv <- scripted(error_response(503, "Busy."))
  resp <- perform(srv, retries = 3)
  expect_identical(httr2::resp_status(resp), 503L)
  expect_identical(srv$n, 4L)
  expect_length(rec$sleeps, 3)
  expect_true(all(rec$sleeps >= c(5, 10, 20) & rec$sleeps <= c(6, 11, 21)))
  srv <- scripted(error_response(503, "Busy."))
  perform(srv, retries = 0)
  expect_identical(srv$n, 1L)
})

test_that("network failures are retried, then reported as an amanuensis_network error", {
  rec <- local_sleeps()
  srv <- scripted(network_error(), ok_response())
  expect_message(resp <- perform(srv, quiet = FALSE), "Network problem")
  expect_identical(httr2::resp_status(resp), 200L)
  srv <- scripted(network_error("Timeout was reached"))
  err <- expect_error(perform(srv, retries = 2), class = "amanuensis_network")
  expect_identical(srv$n, 3L)
  expect_match(conditionMessage(err), "Could not reach Google: Timeout was reached")
  expect_match(conditionMessage(err), "finished pages are kept")
  expect_false(grepl(FAKE_KEY, conditionMessage(err), fixed = TRUE))
})

# ------- classifying failures ---------------------------------------------------

test_that(".classify_failure stops the run or skips the page, with what to do next", {
  k <- function(status, msg, model = NULL) .classify_failure(status, msg, model)
  x <- k(400, "API key not valid. Please pass a valid API key.")
  expect_identical(x$action, "stop"); expect_match(x$message, "set_key\\(\\) again")
  x <- k(401, "Request had invalid authentication credentials.")
  expect_identical(x$action, "stop"); expect_match(x$message, "HTTP 401.*set_key")
  x <- k(403, "Batch API is only available with billing enabled.")
  expect_identical(x$action, "stop"); expect_match(x$message, "billing switched on")
  x <- k(403, "Permission denied.")
  expect_match(x$message, "valid key")
  x <- k(402, "No credit.")
  expect_identical(x$action, "stop"); expect_match(x$message, "no credit left")
  x <- k(404, "models/gemini-9 is not found.", model = "gemini-9")
  expect_identical(x$action, "stop"); expect_match(x$message, "does not know the model 'gemini-9'.*gemini_models")
  x <- k(429, "Resource has been exhausted (e.g. check quota).")
  expect_identical(x$action, "stop"); expect_match(x$message, "quota.*midnight Pacific")
  x <- k(413, "Request payload size exceeds the limit.")
  expect_identical(x$action, "skip"); expect_match(x$message, "could not use this image")
  x <- k(400, "Unable to process input image. Please retry or report.")
  expect_identical(x$action, "skip"); expect_match(x$message, "could not use this image")
  x <- k(400, "Thinking level MINIMAL is not supported for this model.")
  expect_identical(x$action, "stop"); expect_match(x$message, "thinking = \"low\"")
  x <- k(400, "Invalid JSON payload: response_json_schema has an unsupported keyword.")
  expect_identical(x$action, "stop"); expect_match(x$message, "rejected the codebook")
  for (st in c(500, 503)) {
    x <- k(st, "Internal error.")
    expect_identical(x$action, "skip"); expect_match(x$message, "server problem.*tried again next run")
  }
  x <- k(418, "I'm a teapot.")
  expect_identical(x$action, "skip"); expect_match(x$message, "HTTP 418")
})

test_that("a skipped page says which kind of failure it was; only Google's word makes it an image problem", {
  expect_identical(.classify_failure(400, "Unable to process input image.")$problem, "image")
  expect_identical(.classify_failure(413, "Request payload size exceeds the limit.")$problem, "image")
  expect_identical(.classify_failure(503, "Overloaded.")$problem, "server")
  expect_identical(.classify_failure(408, "Timeout.")$problem, "server")
  expect_identical(.classify_failure(418, "I'm a teapot.")$problem, "request")
})

test_that("a 404 names the model only when a model was asked for", {
  x <- .classify_failure(404, "Requested entity was not found.")
  expect_identical(x$action, "stop")
  expect_false(grepl("model", x$message))
  expect_match(x$message, "could not find it (HTTP 404): Requested entity was not found.", fixed = TRUE)
  err <- expect_error(.stop_if_failed(error_response(404, "Requested entity was not found."), "Downloading the batch results failed"),
                      class = "amanuensis_api_error")
  expect_identical(.error_status(err), 404L)
  expect_false(grepl("does not know the model", conditionMessage(err)))
})

test_that("a 429 says whether flex capacity, a per-minute limit or the daily quota ran out", {
  flex <- .classify_failure(429, "The service is currently at capacity for the flex tier. Please retry later.")
  expect_identical(flex$action, "stop")
  expect_match(flex$message, "flex tier is at capacity")
  expect_match(flex$message, "flex = FALSE", fixed = TRUE)
  expect_false(grepl("midnight", flex$message))
  expect_false(grepl("later..", flex$message, fixed = TRUE))
  expect_match(.classify_failure(429, "Resource exhausted.", engine = "flex")$message, "flex = FALSE", fixed = TRUE)
  minute <- .classify_failure(429, "Quota exceeded.",
                              details = list(quota = "GenerateContentPaidTierInputTokensPerModelPerMinute"))
  expect_match(minute$message, "per-minute limit")
  expect_false(grepl("midnight", minute$message))
  day <- .classify_failure(429, "Quota exceeded.", details = list(quota = "GenerateRequestsPerDayPerProjectPerModel-FreeTier"))
  expect_match(day$message, "daily quota.*midnight Pacific")
  # the quota ids are read from Google's error details
  resp <- httr2::response_json(status_code = 429, body = list(error = list(
    code = 429L, message = "Quota exceeded.", status = "RESOURCE_EXHAUSTED",
    details = list(list(`@type` = "type.googleapis.com/google.rpc.QuotaFailure",
                        violations = list(list(quotaId = "GenerateRequestsPerDayPerProjectPerModel")))))))
  expect_identical(.gemini_error_details(resp), list(reason = "RESOURCE_EXHAUSTED", quota = "GenerateRequestsPerDayPerProjectPerModel"))
  expect_identical(.gemini_error_details(httr2::response(status_code = 502, body = charToRaw("<html>"))),
                   list(reason = NULL, quota = character()))
})

test_that("an account-level HTTP 400 stops the run instead of failing page after page", {
  x <- .classify_failure(400, "Gemini API free tier is not available in your country. Please enable billing on your project in Google AI Studio.")
  expect_identical(x$action, "stop")
  expect_match(x$message, "refused the account, not the page")
  expect_identical(.classify_failure(400, "User location is not supported for the API use.")$action, "stop")
  expect_identical(.classify_failure(400, "Precondition check failed.", details = list(reason = "FAILED_PRECONDITION"))$action, "stop")

  local_amanuensis()
  dir <- withr::local_tempdir()
  pages <- file.path(dir, "pages")
  make_jpegs(pages, 3)
  out <- file.path(dir, "out")
  srv <- fake_gemini(pages, handler = function(file, body, n, req) {
    error_response(400, "Gemini API free tier is not available in your country. Please enable billing.",
                   status_text = "FAILED_PRECONDITION")
  })
  httr2::local_mocked_responses(srv$mock)
  msgs <- all_condition_text(transcribe(pages, test_instructions(), roster_codebook(), model = "gemini-3.8-flash",
                                            save_to = out))
  expect_length(srv$calls, 1)
  expect_true(any(grepl("refused the account, not the page", msgs)))
  expect_false(any(file.exists(file.path(out, c("p001.json", "p002.json", "p003.json")))))
  # once billing is on, the same line reads every page; no redo needed
  srv$handler <- NULL
  res <- quietly(transcribe(pages, test_instructions(), roster_codebook(), model = "gemini-3.8-flash", save_to = out))
  expect_identical(.result_info(res)$pages$status, rep("ok", 3))
})

test_that("flex at capacity is not reported as a used-up daily quota", {
  local_amanuensis()
  dir <- withr::local_tempdir()
  pages <- file.path(dir, "pages")
  make_jpegs(pages, 2)
  srv <- fake_gemini(pages, handler = function(file, body, n, req) {
    error_response(429, "The service is currently at capacity for the flex tier. Please retry later.")
  })
  httr2::local_mocked_responses(srv$mock)
  msgs <- all_condition_text(transcribe(pages, test_instructions(), roster_codebook(), model = "gemini-3.8-flash",
                                            flex = TRUE, save_to = file.path(dir, "out")))
  expect_identical(srv$calls[[1]]$body$serviceTier, "flex")
  expect_true(any(grepl("flex tier is at capacity", msgs)))
  expect_true(any(grepl("flex = FALSE", msgs, fixed = TRUE)))
  expect_false(any(grepl("midnight", msgs)))
})

test_that(".gemini_error_message reads Google's message, else the HTTP reason", {
  expect_identical(.gemini_error_message(error_response(400, "Bad thing.")), "Bad thing.")
  expect_identical(.gemini_error_message(httr2::response(status_code = 502, body = charToRaw("<html>"))), "Bad Gateway")
})

test_that("images are encoded as one line of base64 (Google rejects wrapped base64)", {
  dir <- withr::local_tempdir()
  make_jpegs(dir, 1, width = 400, height = 300)
  f <- list.files(dir, full.names = TRUE)[1]
  b64 <- .encode_image(f)
  expect_gt(nchar(b64), 200)
  expect_false(grepl("\n", b64, fixed = TRUE))
  expect_false(grepl("\r", b64, fixed = TRUE))
  expect_identical(jsonlite::base64_dec(b64), readBin(f, "raw", file.size(f)))
  # and that is what goes over the wire, in both body flavours
  for (case in c("camel", "snake")) {
    body <- .build_body("Read it.", b64, "image/jpeg", tiny_schema, body_settings(), case = case)
    wire <- .to_json(body)
    part <- .parse_json(wire)$contents[[1]]$parts[[1]]
    data <- part$inlineData$data %||% part$inline_data$data
    expect_false(grepl("\n", data, fixed = TRUE))
    expect_identical(data, b64)
  }
})

test_that("Google's messages are cut short, and quoted base64 is dropped", {
  echo <- paste0("Invalid value at 'contents[0].parts[0].inline_data.data' (TYPE_BYTES), \"",
                 strrep("QUJDREVG", 200000), "\"")
  msg <- .gemini_error_message(error_response(400, echo))
  expect_lte(nchar(msg), 500)
  expect_match(msg, "inline_data.data", fixed = TRUE)
  expect_match(msg, "[...]", fixed = TRUE)
  expect_identical(.short_message("short"), "short")
  expect_identical(nchar(.short_message(strrep("word ", 400), 100)), 100L)
})

test_that("a Base64 decoding error stops the run as a package problem", {
  why <- .classify_failure(400, "Invalid value at 'contents[0].parts[0].inline_data.data' (TYPE_BYTES), Base64 decoding failed for \"[...]\"")
  expect_identical(why$action, "stop")
  expect_match(why$message, "problem in the package", fixed = TRUE)
  expect_identical(.classify_failure(400, "Invalid JSON payload received. Unknown name \"foo\" at 'generation_config': Cannot find field.")$action, "stop")
  expect_identical(.classify_failure(400, "Unable to process input image. Please retry.")$action, "skip")
})

test_that(".stop_if_failed keeps Google's message intact, braces included", {
  resp <- error_response(400, "Invalid value at 'response_json_schema' ({\"type\": \"objekt\"}).")
  err <- expect_error(.stop_if_failed(resp, "Creating the batch job failed"), class = "amanuensis_api_error")
  expect_match(conditionMessage(err), "Creating the batch job failed.", fixed = TRUE)
  expect_match(conditionMessage(err), "{\"type\": \"objekt\"}", fixed = TRUE)
  expect_invisible(.stop_if_failed(ok_response(), "x"))
})

# ------- reading replies ---------------------------------------------------------

test_that(".read_reply joins non-thought parts and reads the token counts", {
  r <- .read_reply(gemini_reply(data = list(text = "Hola"), thought_part = TRUE, thoughts = 300L, cached = 100L))
  expect_identical(r$text, "{\"text\":\"Hola\"}")
  expect_identical(r$finish_reason, "STOP")
  expect_identical(r$model_version, "gemini-3.8-flash")
  expect_identical(r$tokens$input, 2500)
  expect_identical(r$tokens$cached, 100)
  expect_identical(r$tokens$thinking, 300)
  expect_identical(r$tokens$thinking_raw, 300)
  expect_identical(r$tokens$output, 200)
  expect_identical(r$tokens$total, 3000)
  expect_identical(r$image_tokens, 2240)
  expect_true(is.na(r$block_reason))
  expect_false(r$nul)
})

test_that("absent thinking counts are NA, not zero, and missing image details give NA", {
  r <- .read_reply(gemini_reply(image = NULL))
  expect_identical(r$tokens$thinking, 0)
  expect_true(is.na(r$tokens$thinking_raw))
  expect_true(is.na(r$image_tokens))
  expect_identical(r$tokens$cached, 0)
})

test_that("snake_case replies from batch results are read too", {
  r <- .read_reply(list(candidates = list(list(content = list(parts = list(list(text = "{}"))), finish_reason = "STOP")),
                        usage_metadata = list(prompt_token_count = 10, candidates_token_count = 2, total_token_count = 12,
                                              thoughts_token_count = 0,
                                              prompt_tokens_details = list(list(modality = "IMAGE", token_count = 8))),
                        model_version = "gemini-3.8-flash"))
  expect_identical(r$text, "{}")
  expect_identical(r$finish_reason, "STOP")
  expect_identical(r$tokens$input, 10)
  expect_identical(r$tokens$thinking_raw, 0)
  expect_identical(r$image_tokens, 8)
})

test_that("no candidates gives the block reason, or 'no reply'", {
  r <- .read_reply(gemini_reply(no_candidates = TRUE, block_reason = "PROHIBITED_CONTENT"))
  expect_identical(r$block_reason, "PROHIBITED_CONTENT")
  expect_null(r$text)
  r2 <- .read_reply(list(usageMetadata = list(promptTokenCount = 5)))
  expect_identical(r2$block_reason, "no reply")
})

test_that("a NUL escape in the raw body is noticed", {
  raw <- '{"candidates":[{"content":{"parts":[{"text":"{\\"text\\":\\"a\\u0000b\\"}"}]},"finishReason":"STOP"}]}'
  r <- .read_reply(gemini_reply(), raw_body = raw)
  expect_true(r$nul)
})

test_that(".finish_message explains each finish reason", {
  expect_match(.finish_message("MAX_TOKENS", 32768), "max_tokens = 32,768.*for example 65,536")
  expect_match(.finish_message("MAX_TOKENS", 65536), "for example 65,536")
  expect_match(.finish_message("SAFETY", 1), "content filter blocked this page \\(SAFETY\\)")
  expect_match(.finish_message("IMAGE_SAFETY", 1), "IMAGE_SAFETY")
  expect_match(.finish_message("RECITATION", 1), "RECITATION")
  expect_match(.finish_message(NULL, 1), "no finish reason")
  expect_match(.finish_message("MALFORMED_FUNCTION_CALL", 1), "ended unexpectedly \\(MALFORMED_FUNCTION_CALL\\)")
})

test_that("a downloaded results file is read as UTF-8, whatever the session's locale", {
  withr::local_locale(c(LC_CTYPE = "C"))
  bytes <- c(charToRaw('{"key":"p001.jpg","name":"F'), as.raw(c(0xc3, 0xa0)), charToRaw('bregas"}'))
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, headers = list(`content-type` = "application/octet-stream"), body = bytes)
  })
  text <- .gemini_download("files/out-1", FAKE_KEY)
  expect_identical(Encoding(text), "UTF-8")
  name <- .parse_json(strsplit(text, "\n", fixed = TRUE)[[1]][1])$name
  expect_identical(charToRaw(enc2utf8(name)), c(charToRaw("F"), as.raw(c(0xc3, 0xa0)), charToRaw("bregas")))
})

test_that("batch states and results files are read from either place", {
  expect_identical(.batch_state(list(metadata = list(state = "BATCH_STATE_RUNNING"))), "RUNNING")
  expect_identical(.batch_state(list(state = "JOB_STATE_SUCCEEDED")), "SUCCEEDED")
  expect_identical(.batch_state(list()), "UNSPECIFIED")
  expect_identical(.batch_results_file(list(response = list(responsesFile = "files/a"))), "files/a")
  expect_identical(.batch_results_file(list(metadata = list(output = list(responsesFile = "files/b")))), "files/b")
  expect_identical(.batch_results_file(list(dest = list(fileName = "files/c"))), "files/c")
  expect_null(.batch_results_file(list()))
})

# ------- turning a reply into a record -------------------------------------------

fake_run <- function(model = "gemini-3.8-flash", engine = "sync", max_tokens = 32768L, prices = NULL,
                     cb = roster_codebook(), resolution = "ultra_high") {
  settings <- list(model = model, resolution = resolution, thinking = "low", thinking_level = "LOW",
                   max_tokens = as.integer(max_tokens), temperature = NULL, engine = engine,
                   flex = engine == "flex", api_args = list(), timeout = 600, retries = 4L, prices = prices)
  pages <- data.frame(image = NA_character_, file = "p001.jpg", source = "book.pdf", source_page = 3L,
                      stringsAsFactors = FALSE)
  .new_run(settings, .contract(test_instructions(), cb), NULL, NULL, pages)
}

record_for <- function(reply, run = fake_run(), raw = NULL) {
  .record_from_reply(reply, raw %||% .to_json(reply), run$pages[1, ], run)
}

test_that("a good reply becomes an ok record with tokens, price and cost", {
  run <- fake_run()
  rec <- record_for(gemini_reply(data = roster_data("p001.jpg")), run)
  expect_identical(rec$status, "ok")
  expect_identical(rec$file, "p001.jpg")
  expect_identical(rec$source, "book.pdf")
  expect_identical(rec$source_page, 3L)
  expect_identical(rec$data, roster_data("p001.jpg"))
  expect_identical(rec$reply, .to_json(roster_data("p001.jpg")))
  expect_identical(rec$tokens, list(input = 2500, cached = 0, image = 2240, thinking = NA_real_, output = 200,
                                    billed_output = 200, total = 2700))
  p <- .price_for("gemini-3.8-flash")
  expect_equal(rec$cost_usd, (2500 * p$input + 200 * p$output) / 1e6)
  expect_false(rec$price$discount)
  expect_identical(rec$contract, run$contract$id)
  expect_identical(rec$settings$temperature, "not sent")
  expect_identical(rec$finish_reason, "STOP")
  expect_length(rec$notes, 0)
})

test_that("thinking is billed as output: billed output = total - prompt", {
  rec <- record_for(gemini_reply(thoughts = 300L))
  expect_identical(rec$tokens$thinking, 300)
  expect_identical(rec$tokens$billed_output, 500)
  p <- .price_for("gemini-3.8-flash")
  expect_equal(rec$cost_usd, (2500 * p$input + 500 * p$output) / 1e6)
  # cached input at the cached price
  rec2 <- record_for(gemini_reply(cached = 1000L))
  expect_equal(rec2$cost_usd, (1500 * p$input + 1000 * p$cached + 200 * p$output) / 1e6)
})

test_that("batch and flex cost half; unknown models have no cost; practice models use flash prices", {
  ok <- gemini_reply(data = roster_data())
  sync <- record_for(ok)$cost_usd
  expect_equal(record_for(ok, fake_run(engine = "batch"))$cost_usd, sync / 2)
  flex <- record_for(ok, fake_run(engine = "flex"))
  expect_equal(flex$cost_usd, sync / 2)
  expect_true(flex$price$discount)
  unknown <- record_for(ok, fake_run(model = "gemini-9-ultra"))
  expect_null(unknown$cost_usd)
  expect_null(unknown$price)
  expect_identical(unknown$status, "ok")
  expect_equal(record_for(ok, fake_run(model = "mock"))$cost_usd, sync)
  own <- record_for(ok, fake_run(prices = c(input = 1, output = 2)))
  expect_equal(own$cost_usd, (2500 * 1 + 200 * 2) / 1e6)
})

test_that("MAX_TOKENS fails the page as truncated, keeps the raw text and suggests the next step", {
  rec <- record_for(gemini_reply(text = '{"heading": "abc', finish = "MAX_TOKENS"))
  expect_identical(rec$status, "failed")
  expect_identical(rec$problem, "truncated")
  expect_match(rec$message, "max_tokens = 32,768.*max_tokens \\(for example 65,536\\)")
  expect_identical(rec$reply, '{"heading": "abc')
  expect_null(rec$data)
  # at the model's limit there is no larger max_tokens to suggest
  at_cap <- record_for(gemini_reply(text = "x", finish = "MAX_TOKENS"), fake_run(max_tokens = 65536L))
  expect_match(at_cap$message, "model's limit of 65,536 tokens.*split the image")
  stuck <- record_for(gemini_reply(text = paste0('{"text": "', strrep(".", 50), '"'), finish = "MAX_TOKENS"),
                      fake_run(max_tokens = 65536L))
  expect_match(stuck$message, "repeats one punctuation mark")
})

test_that("SAFETY, RECITATION and missing candidates fail the page as blocked", {
  s <- record_for(gemini_reply(text = "", finish = "SAFETY"))
  expect_identical(c(s$status, s$problem), c("failed", "blocked"))
  expect_match(s$message, "content filter")
  r <- record_for(gemini_reply(text = "", finish = "RECITATION"))
  expect_identical(r$problem, "blocked")
  expect_match(r$message, "RECITATION")
  b <- record_for(gemini_reply(no_candidates = TRUE, block_reason = "OTHER"))
  expect_identical(c(b$status, b$problem), c("failed", "blocked"))
  expect_match(b$message, "blocked this page before reading it \\(OTHER\\)")
  expect_null(b$reply)
})

test_that("NUL characters, empty and non-JSON replies fail with their own problem", {
  raw <- '{"candidates":[{"content":{"parts":[{"text":"{\\"text\\":\\"a\\u0000b\\"}"}]},"finishReason":"STOP"}]}'
  n <- record_for(.parse_json(raw), raw = raw)
  expect_identical(c(n$status, n$problem), c("failed", "control_chars"))
  e <- record_for(gemini_reply(text = "  "))
  expect_identical(e$problem, "empty")
  j <- record_for(gemini_reply(text = "Here is the table: name, age"))
  expect_identical(c(j$status, j$problem), c("failed", "not_json"))
  expect_identical(j$reply, "Here is the table: name, age")
})

test_that("a reply that does not match the codebook is kept as 'check' with the problems", {
  bad <- roster_data("p001.jpg")
  bad$records[[1]]$sex <- "unknown"
  bad$records[[2]]$age <- "22 years"
  rec <- record_for(gemini_reply(data = bad))
  expect_identical(rec$status, "check")
  expect_identical(rec$problem, "invalid")
  expect_length(rec$errors, 2)
  expect_match(rec$message, "does not match the codebook: records\\[1\\]\\.sex.*\\(and 1 more\\)")
  expect_identical(rec$data, bad)
})

test_that("mechanical checks become notes", {
  d <- list(heading = "a\tb", records = list())
  rec <- record_for(gemini_reply(data = d, output = 30000L, model_version = "gemini-3.6-flash-002", image = 1100L))
  notes <- unlist(rec$notes)
  expect_true(any(grepl("tab or control character.*heading", notes)))
  expect_true(any(grepl("No rows were found", notes)))
  expect_true(any(grepl("used 92% of max_tokens", notes)))
  expect_true(any(grepl("Google answered with gemini-3.6-flash-002", notes)))
  expect_true(any(grepl("Only 1100 image tokens", notes)))
  d2 <- list(heading = paste0(" Title", strrep("-", 30)),
             records = list(list(name = NULL, age = NULL, sex = NULL), list(name = "A", age = 1L, sex = "male")))
  notes2 <- unlist(record_for(gemini_reply(data = d2))$notes)
  expect_true(any(grepl("stuck repeating a mark in heading", notes2)))
  expect_true(any(grepl("start or end with a space", notes2)))
  expect_true(any(grepl("1 row\\(s\\) are completely empty", notes2)))
  # an alias answered by the model it routes to is not a surprise
  alias <- record_for(gemini_reply(model_version = "gemini-3.8-flash"), fake_run(model = "gemini-3.7-flash"))
  expect_false(any(grepl("Google answered with", unlist(alias$notes))))
})
