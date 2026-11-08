# Fake Gemini replies for httr2::local_mocked_responses(). The helpers build
# generateContent bodies the way Google sends them (candidates with
# finishReason, usageMetadata, modelVersion, promptFeedback), HTTP errors in
# Google's error format, and two small fake servers: one for generateContent
# that knows which page image each request carries, and one for the whole
# batch cycle (upload, create, poll, download, delete).

# A generateContent reply body. `data` is the model's JSON (serialised into
# the text part); `text` overrides it with any string. `thoughts` adds
# thoughtsTokenCount (absent by default, as Google omits it when there was no
# thinking); `thought_part` adds a thought part that must be skipped.
gemini_reply <- function(data = list(text = "Some text."), text = NULL, finish = "STOP",
                         prompt = 2500L, output = 200L, thoughts = NULL, total = NULL,
                         image = 2240L, cached = NULL, model_version = "gemini-3.8-flash",
                         thought_part = FALSE, block_reason = NULL, no_candidates = FALSE) {
  if (is.null(text)) text <- amanuensis:::.to_json(data)
  usage <- list(promptTokenCount = prompt, candidatesTokenCount = output)
  if (!is.null(cached)) usage$cachedContentTokenCount <- cached
  if (!is.null(thoughts)) usage$thoughtsTokenCount <- thoughts
  usage$totalTokenCount <- total %||% (prompt + output + (thoughts %||% 0L))
  if (!is.null(image)) {
    usage$promptTokensDetails <- list(list(modality = "TEXT", tokenCount = prompt - image),
                                      list(modality = "IMAGE", tokenCount = image))
  }
  body <- list(usageMetadata = usage, modelVersion = model_version, responseId = "resp-1")
  if (no_candidates) {
    body$promptFeedback <- list(blockReason = block_reason %||% "OTHER")
    return(body)
  }
  parts <- list(list(text = text))
  if (thought_part) parts <- c(list(list(text = "Let me think about the page.", thought = TRUE)), parts)
  body$candidates <- list(list(content = list(role = "model", parts = parts), finishReason = finish, index = 0L))
  body
}

ok_response <- function(...) httr2::response_json(body = gemini_reply(...))

# Google's error envelope; `retry_delay` adds a RetryInfo detail ("17s").
error_response <- function(status, message = "Something went wrong.", status_text = NULL,
                           retry_delay = NULL, headers = list()) {
  err <- list(code = status, message = message,
              status = status_text %||% switch(as.character(status),
                "400" = "INVALID_ARGUMENT", "401" = "UNAUTHENTICATED", "403" = "PERMISSION_DENIED",
                "404" = "NOT_FOUND", "429" = "RESOURCE_EXHAUSTED", "500" = "INTERNAL",
                "503" = "UNAVAILABLE", "504" = "DEADLINE_EXCEEDED", "UNKNOWN"))
  if (!is.null(retry_delay)) {
    err$details <- list(list(`@type` = "type.googleapis.com/google.rpc.RetryInfo", retryDelay = retry_delay))
  }
  httr2::response_json(status_code = status, headers = headers, body = list(error = err))
}

# What curl reports when the network is down. Returned (not thrown) by a mock,
# httr2 signals it from req_perform() and keeps it as the result in
# req_perform_parallel(), as with a real connection failure.
network_error <- function(message = "Could not resolve host: generativelanguage.googleapis.com") {
  structure(class = c("httr2_failure", "httr2_error", "rlang_error", "error", "condition"),
            list(message = message, call = NULL))
}

request_json <- function(req) amanuensis:::.parse_json(amanuensis:::.req_body_text(req))

# A fake generateContent endpoint. `images` is a folder or image paths; the
# page a request is about is recognised from its base64 image. `handler`
# (file, body, n, req) returns an httr2 response, a reply body list (sent with
# status 200), or NULL for a schema-shaped default reply; `n` counts the calls
# made for that file. The returned environment keeps every call in $calls;
# assign a new function to $handler to change the replies between runs.
fake_gemini <- function(images, handler = NULL) {
  srv <- new.env(parent = emptyenv())
  srv$calls <- list()
  srv$count <- list()
  srv$handler <- handler
  files_now <- function() {
    if (length(images) == 1 && dir.exists(images)) {
      list.files(images, pattern = "\\.(jpe?g|png|webp)$", full.names = TRUE)
    } else images
  }
  srv$mock <- function(req) {
    if (!grepl(":generateContent$", req$url)) {
      return(httr2::response_json(status_code = 404, body = list(error = list(message = paste("unexpected request", req$url)))))
    }
    body <- request_json(req)
    b64 <- NULL
    for (p in body$contents[[1]]$parts) if (!is.null(p$inlineData)) b64 <- p$inlineData$data
    files <- files_now()
    # decode what the package sent and compare bytes, so the test does not
    # depend on how the package encodes (a line-wrapped encoder once matched
    # a line-wrapped expectation here and hid a real bug)
    sent <- if (is.null(b64)) raw() else jsonlite::base64_dec(gsub("\\s", "", b64))
    known_raw <- lapply(files, function(f) readBin(f, "raw", file.size(f)))
    hit <- which(vapply(known_raw, function(k) identical(k, sent), logical(1)))
    known <- vapply(files, function(f) "", "")
    if (length(hit)) known[hit[1]] <- b64
    file <- basename(files[match(b64, known)])
    if (!length(file) || is.na(file)) file <- "unknown"
    n <- (srv$count[[file]] %||% 0L) + 1L
    srv$count[[file]] <- n
    srv$calls[[length(srv$calls) + 1]] <- list(file = file, body = body, url = req$url, req = req)
    out <- if (is.null(srv$handler)) NULL else srv$handler(file, body, n, req)
    if (inherits(out, "httr2_response") || inherits(out, "error")) return(out)
    if (is.null(out)) {
      schema <- body$generationConfig$responseJsonSchema %||% list(type = "object")
      out <- gemini_reply(data = amanuensis:::.mock_value(schema, "value", 1L, file, schema))
    }
    httr2::response_json(body = out)
  }
  srv$files_called <- function() vapply(srv$calls, function(x) x$file, "")
  srv
}

# A fake Batch API for a real model name. `states`: the states returned by
# successive polls (the last one repeats). `reply` (key, request) returns a
# results line as a list: list(key =, response = <reply body>) or
# list(key =, error = list(message =)); default: a schema-shaped reply.
# `shuffle = TRUE` returns the result lines in reverse order; `create_error`
# is a response returned when a job is created (e.g. a full queue, 429).
fake_batch <- function(states = "SUCCEEDED", reply = NULL, shuffle = FALSE, create_error = NULL) {
  srv <- new.env(parent = emptyenv())
  srv$uploads <- list()
  srv$calls <- character()
  srv$polls <- 0L
  srv$deleted <- character()
  srv$created <- list()
  srv$mock <- function(req) {
    url <- sub("\\?.*$", "", req$url)
    method <- req$method %||% (if (is.null(req$body)) "GET" else "POST")
    srv$calls <- c(srv$calls, paste(method, url))
    if (grepl("/upload/v1beta/files$", url)) {
      return(httr2::response(status_code = 200, headers = list(`x-goog-upload-url` = "https://upload.test/session-1"),
                             body = charToRaw("{}")))
    }
    if (startsWith(url, "https://upload.test/")) {
      name <- paste0("files/input-", length(srv$uploads) + 1L)
      srv$uploads[[name]] <- amanuensis:::.req_body_text(req)
      return(httr2::response_json(body = list(file = list(name = name, sizeBytes = "100"))))
    }
    if (grepl(":batchGenerateContent$", url)) {
      if (!is.null(create_error)) return(create_error)
      body <- request_json(req)
      srv$created[[length(srv$created) + 1]] <- body
      srv$input <- body$batch$inputConfig$fileName
      srv$display <- body$batch$displayName
      return(httr2::response_json(body = list(name = "batches/test-1",
                                              metadata = list(state = "BATCH_STATE_PENDING", displayName = srv$display))))
    }
    if (grepl("/v1beta/batches$", url)) {
      return(httr2::response_json(body = list(batches = if (is.null(srv$display)) list() else
        list(list(name = "batches/test-1", metadata = list(displayName = srv$display))))))
    }
    if (grepl("/v1beta/batches/test-1$", url)) {
      srv$polls <- srv$polls + 1L
      state <- states[min(srv$polls, length(states))]
      out <- list(name = "batches/test-1", metadata = list(state = paste0("BATCH_STATE_", state),
                                                           displayName = srv$display))
      if (state == "SUCCEEDED") out$response <- list(responsesFile = "files/output-1")
      if (state == "FAILED") out$error <- list(message = "internal batch failure")
      return(httr2::response_json(body = out))
    }
    if (grepl("files/output-1:download$", url)) {
      lines <- strsplit(srv$uploads[[srv$input]], "\n", fixed = TRUE)[[1]]
      lines <- lines[nzchar(lines)]
      out <- vapply(lines, function(line) {
        obj <- amanuensis:::.parse_json(line)
        res <- if (is.null(reply)) NULL else reply(obj$key, obj$request)
        if (is.null(res)) {
          gc <- obj$request$generation_config
          schema <- gc$response_json_schema %||% gc$response_schema %||% list(type = "object")
          res <- list(key = obj$key, response = gemini_reply(data = amanuensis:::.mock_value(schema, "value", 1L, obj$key, schema)))
        }
        amanuensis:::.to_json(res)
      }, "", USE.NAMES = FALSE)
      if (shuffle) out <- rev(out)
      return(httr2::response(status_code = 200, headers = list(`content-type` = "application/octet-stream"),
                             body = charToRaw(enc2utf8(paste(out, collapse = "\n")))))
    }
    if (method == "DELETE") {
      srv$deleted <- c(srv$deleted, sub(".*/v1beta/", "", url))
      return(httr2::response_json(body = setNames(list(), character())))
    }
    httr2::response_json(status_code = 404, body = list(error = list(message = paste("unexpected request", url))))
  }
  srv
}

`%||%` <- function(x, y) if (is.null(x)) y else x
