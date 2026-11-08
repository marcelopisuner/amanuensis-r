# Practice models. model = "mock" invents schema-shaped replies for any page;
# model = "example" replays prepared replies for the example project's pages.
# Neither needs a key or costs anything. They are not a separate code path:
# they answer the package's real HTTP requests through httr2's mocking hook,
# so the request building, reply parsing, validation, checks, records,
# resuming and the whole batch cycle (upload, create, poll, download) run
# exactly as with Google. That is also how the tests exercise everything
# offline.

.mock_state <- new.env(parent = emptyenv())

.with_practice_transport <- function(model, expr) {
  if (!model %in% .practice_models) return(expr)
  httr2::with_mocked_responses(.practice_mock(model), expr)
}

.mock_counter <- function(what) {
  n <- (.mock_state[[what]] %||% 0L) + 1L
  .mock_state[[what]] <- n
  n
}

# Request bodies are UTF-8 bytes; mark them so, whatever the session's locale.
.req_body_text <- function(req) {
  d <- req$body$data
  if (is.raw(d)) {
    x <- rawToChar(d)
    Encoding(x) <- "UTF-8"
    return(x)
  }
  if (identical(req$body$type, "file") && is.character(d) && file.exists(d)) return(.read_text(d))
  if (is.character(d)) return(paste(d, collapse = ""))
  ""
}

.practice_mock <- function(model) {
  function(req) {
    url <- req$url
    method <- req$method %||% (if (is.null(req$body)) "GET" else "POST")
    path <- sub("^https?://[^/]+/", "", sub("\\?.*$", "", url))
    if (grepl(":generateContent$", path)) {
      body <- .parse_json(.req_body_text(req))
      file <- req$headers[["x-amanuensis-file"]] %||% "page"
      return(httr2::response_json(body = .practice_reply(model, body, file)))
    }
    if (grepl("upload/v1beta/files$", path)) {
      n <- .mock_counter("upload")
      return(httr2::response(status_code = 200,
                             headers = list(`x-goog-upload-url` = paste0("https://mock.amanuensis/upload/", n)),
                             body = charToRaw("{}")))
    }
    if (startsWith(url, "https://mock.amanuensis/upload/")) {
      n <- sub(".*/", "", url)
      name <- paste0("files/mock-", n)
      .mock_state$files[[name]] <- .req_body_text(req)
      return(httr2::response_json(body = list(file = list(name = name))))
    }
    if (grepl(":batchGenerateContent$", path)) {
      body <- .parse_json(.req_body_text(req))
      n <- .mock_counter("job")
      name <- paste0("batches/mock-", n)
      .mock_state$jobs[[name]] <- list(file = body$batch$inputConfig$fileName, polls = 0L,
                                      display = body$batch$displayName, state = "PENDING")
      return(httr2::response_json(body = list(name = name, metadata = list(state = "BATCH_STATE_PENDING",
                                                                           displayName = body$batch$displayName))))
    }
    if (grepl("batches/mock-[0-9]+:cancel$", path)) {
      name <- sub(":cancel$", "", sub(".*?(batches/mock-[0-9]+).*", "\\1", path))
      if (!is.null(.mock_state$jobs[[name]])) .mock_state$jobs[[name]]$state <- "CANCELLED"
      return(httr2::response_json(body = .json_object()))
    }
    if (grepl("batches/mock-[0-9]+$", path)) {
      name <- sub(".*?(batches/mock-[0-9]+)$", "\\1", path)
      job <- .mock_state$jobs[[name]]
      if (is.null(job)) return(httr2::response_json(status_code = 404, body = list(error = list(message = "not found"))))
      if (job$state %in% c("PENDING", "RUNNING")) {
        job$polls <- job$polls + 1L
        job$state <- if (job$polls >= 2) "SUCCEEDED" else "RUNNING"
        .mock_state$jobs[[name]] <- job
      }
      lines <- strsplit(.mock_state$files[[job$file]] %||% "", "\n", fixed = TRUE)[[1]]
      n_lines <- sum(nzchar(lines))
      out <- list(name = name, metadata = list(state = paste0("BATCH_STATE_", job$state), displayName = job$display,
                  batchStats = list(requestCount = n_lines,
                                    successfulRequestCount = if (job$state == "SUCCEEDED") n_lines else 0L)))
      if (job$state == "SUCCEEDED") out$response <- list(responsesFile = paste0(job$file, "-out"))
      return(httr2::response_json(body = out))
    }
    if (grepl("v1beta/batches$", path)) {
      jobs <- .mock_state$jobs %||% list()
      return(httr2::response_json(body = list(batches = lapply(names(jobs), function(nm) {
        list(name = nm, metadata = list(displayName = jobs[[nm]]$display))
      }))))
    }
    if (grepl(":download$", path)) {
      input <- sub("-out:download$", "", sub("^download/v1beta/", "", path))
      lines <- strsplit(.mock_state$files[[input]] %||% "", "\n", fixed = TRUE)[[1]]
      out <- vapply(lines[nzchar(lines)], function(line) {
        obj <- .parse_json(line)
        .to_json(list(key = obj$key, response = .practice_reply(model, obj$request, obj$key)))
      }, character(1), USE.NAMES = FALSE)
      return(httr2::response(status_code = 200, headers = list(`content-type` = "application/octet-stream"),
                             body = charToRaw(enc2utf8(paste(out, collapse = "\n")))))
    }
    if (method == "DELETE") return(httr2::response_json(body = .json_object()))
    if (grepl("v1beta/models$", path)) return(httr2::response_json(body = list(models = list())))
    httr2::response_json(status_code = 404, body = list(error = list(message = paste("mock: unknown endpoint", path))))
  }
}

# A generateContent reply for a request body (camel- or snake-case).
.practice_reply <- function(model, body, file) {
  gc <- body$generationConfig %||% body$generation_config %||% list()
  schema <- gc$responseJsonSchema %||% gc$response_json_schema %||% gc$responseSchema %||% gc$response_schema
  prompt <- ""
  for (p in body$contents[[1]]$parts) if (!is.null(p$text)) prompt <- p$text
  if (identical(model, "example")) {
    ex <- .example_reply(file)
    text <- .to_json(ex$data)
    tokens <- ex$tokens
  } else {
    text <- .to_json(.mock_value(schema %||% list(type = "object"), "value", 1L, file, schema))
    tokens <- list(prompt = 1300L + as.integer(nchar(prompt) / 4), output = as.integer(nchar(text) / 4) + 1L)
  }
  list(
    candidates = list(list(content = list(role = "model", parts = list(list(text = text))), finishReason = "STOP")),
    usageMetadata = list(promptTokenCount = tokens$prompt, candidatesTokenCount = tokens$output,
                         totalTokenCount = tokens$prompt + tokens$output,
                         promptTokensDetails = list(list(modality = "IMAGE", tokenCount = 2240L),
                                                    list(modality = "TEXT", tokenCount = tokens$prompt - 2240L))),
    modelVersion = model
  )
}

# Made-up data that fits a schema (JSON-schema or OpenAPI style).
.mock_value <- function(s, name, i, file, root) {
  if (!is.list(s)) return(paste(name, i))
  if (!is.null(s[["$ref"]])) s <- .resolve_ref(s[["$ref"]], root) %||% s
  if (!is.null(s$allOf)) s <- .merge_all_of(s, root)
  if (.is_null_schema(s)) return(NULL)
  # the first branch that is not just null, so a nullable field gets a value
  alts <- s$anyOf %||% s$oneOf
  if (!is.null(alts)) {
    alts <- lapply(alts, .deref_schema, root = root)
    keep <- Filter(Negate(.is_null_schema), alts)
    return(.mock_value(if (length(keep)) keep[[1]] else alts[[1]], name, i, file, root))
  }
  if (!is.null(s[["const"]])) return(s[["const"]])
  if (!is.null(s$enum)) {
    vals <- Filter(Negate(is.null), s$enum)
    return(if (length(vals)) vals[[((i - 1L) %% length(vals)) + 1L]] else NULL)
  }
  ty <- tolower(setdiff(unlist(s$type), c("null", "NULL")))
  if (!length(ty)) ty <- if (!is.null(s$properties)) "object" else if (!is.null(s$items)) "array" else "string"
  switch(ty[1],
    object = {
      out <- list()
      for (nm in names(s$properties)) out[nm] <- list(.mock_value(s$properties[[nm]], nm, i, file, root))
      if (length(out)) out else .json_object()
    },
    array = {
      it <- s$items %||% list(type = "string")
      n <- if (!is.null(it$properties)) 3L else 2L
      if (!is.null(s$minItems)) n <- max(n, as.integer(s$minItems))
      lapply(seq_len(n), function(j) .mock_value(it, name, j, file, root))
    },
    integer = as.integer(i),
    number = i + 0.5,
    boolean = (i %% 2L == 1L),
    if (name == "text") {
      paste0("Practice transcription of ", file, ".\nThe mock model invents this text; use a real model to read the page.")
    } else {
      paste(name, i)
    }
  )
}

# ------- the example model ----------------------------------------------------

.example_replies <- function() {
  if (is.null(.mock_state$example)) {
    path <- system.file("extdata", "example", "replies.json", package = "amanuensis")
    .mock_state$example <- if (nzchar(path)) .read_json(path) else list()
  }
  .mock_state$example
}

.example_reply <- function(file) {
  rep <- .example_replies()[[file]]
  if (is.null(rep)) {
    .abort(c("The {.val example} model only knows the pages of the example project.",
             "i" = "Use a real model (after {.fn set_key}) for your own pages, or {.val mock} to practise."),
           class = "amanuensis_error_input")
  }
  rep
}

.check_example_pages <- function(pages, call = rlang::caller_env()) {
  known <- names(.example_replies())
  bad <- pages$file[!pages$file %in% known]
  if (length(bad)) {
    .abort(c("The {.val example} model only knows the pages of the example project; it cannot read {.file {utils::head(bad, 3)}}.",
             "i" = "Use a real model (after {.fn set_key}) for your own pages, or {.code model = \"mock\"} to practise."),
           call = call, class = "amanuensis_error_input")
  }
  invisible(TRUE)
}
