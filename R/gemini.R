# A thin client for the Gemini REST API (generateContent, Files, Batch), on
# httr2. All HTTP lives in this file and batch.R, so switching transport later
# (for example to ellmer, once it exposes per-image media resolution and keeps
# the raw reply) touches nothing else. ellmer 0.5.0 does neither, and its
# batch path ignores api_args and forces include_thoughts = TRUE.
#
# Retries are done here, in our own loop, not with httr2::req_retry(): we need
# 500/502/504 retried as well as 429/503, Google's RetryInfo delay honoured,
# plain-language waiting messages, and a loop that httr2's mocks exercise in
# tests (mocks bypass req_retry).

# ------- requests -------------------------------------------------------------

.gemini_base_url <- function() {
  getOption("amanuensis.base_url", "https://generativelanguage.googleapis.com/")
}

.gemini_request <- function(path, key, version = "v1beta", timeout = 600, base = .gemini_base_url()) {
  req <- httr2::request(base)
  if (!is.null(version)) req <- httr2::req_url_path_append(req, version)
  req <- httr2::req_url_path_append(req, path)
  req <- httr2::req_headers(req, `x-goog-api-key` = key, .redact = "x-goog-api-key")
  req <- httr2::req_user_agent(req, paste0("amanuensis/", utils::packageVersion("amanuensis"), " (R)"))
  req <- httr2::req_timeout(req, timeout)
  # never let httr2 turn an HTTP error into an R error: we classify it ourselves
  httr2::req_error(req, is_error = function(resp) FALSE)
}

.sleep <- function(seconds) {
  getOption("amanuensis.sleep", Sys.sleep)(seconds)
}

# 408 timeout, 429 rate limit, 5xx server trouble: worth waiting for.
.is_transient <- function(status) status %in% c(408L, 429L, 500L, 502L, 503L, 504L)

# Seconds to wait before retry number `attempt` (1-based): Google's own advice
# when it gives one (Retry-After header or RetryInfo.retryDelay), else
# 5, 10, 20, 40 ... seconds plus a little jitter, never more than two minutes.
.retry_wait <- function(attempt, resp = NULL) {
  advised <- NA_real_
  if (!is.null(resp)) {
    ra <- httr2::resp_header(resp, "retry-after")
    if (!is.null(ra) && grepl("^[0-9.]+$", ra)) advised <- as.numeric(ra)
    if (is.na(advised)) {
      body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
      m <- regmatches(body, regexpr('"retryDelay"\\s*:\\s*"[0-9.]+s"', body))
      if (length(m)) advised <- as.numeric(gsub("[^0-9.]", "", sub(".*:", "", m)))
    }
  }
  base <- 5 * 2^(attempt - 1)
  wait <- if (!is.na(advised)) max(advised, 1) else base + stats::runif(1, 0, 1)
  min(wait, 120)
}

# Perform one request, retrying transient failures. Returns the httr2 response
# (whatever its status) or raises an amanuensis_network error after the last
# try when the network itself failed.
.gemini_perform <- function(req, retries = 4, quiet = FALSE, what = "Google") {
  attempt <- 0
  repeat {
    resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
    if (!inherits(resp, "error")) {
      status <- httr2::resp_status(resp)
      if (!.is_transient(status) || attempt >= retries) return(resp)
      attempt <- attempt + 1
      wait <- .retry_wait(attempt, resp)
      if (!quiet) {
        .inform(c("i" = "{what} is busy or rate-limited (HTTP {status}); waiting {round(wait)} s before try {attempt + 1} of {retries + 1}."))
      }
    } else {
      if (attempt >= retries) {
        .abort(c("Could not reach Google: {conditionMessage(resp)}",
                 "i" = "Check your internet connection and run the same command again; finished pages are kept."),
               class = "amanuensis_network", parent = resp)
      }
      attempt <- attempt + 1
      wait <- .retry_wait(attempt)
      if (!quiet) {
        .inform(c("i" = "Network problem ({conditionMessage(resp)}); waiting {round(wait)} s before try {attempt + 1} of {retries + 1}."))
      }
    }
    .sleep(wait)
  }
}

# The text of Google's error message, or the HTTP reason.
.gemini_error_message <- function(resp) {
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  msg <- body$error$message %||% NULL
  if (is.null(msg)) msg <- httr2::resp_status_desc(resp) %||% "unknown error"
  .short_message(msg)
}

# Google's error messages can quote the request back, base64 image included
# (one failed page record was 1.3 MB). Long base64-looking runs become
# "[...]" and the whole message is cut to `n` characters before it is shown,
# logged or stored.
.short_message <- function(x, n = 500) {
  if (is.null(x) || !length(x)) return(x)
  x <- paste(as.character(x), collapse = " ")
  x <- gsub("[A-Za-z0-9+/=_-]{80,}", "[...]", x, perl = TRUE)
  if (nchar(x) > n) x <- paste0(substr(x, 1, n - 3), "...")
  x
}

# Google's error status (e.g. "FAILED_PRECONDITION") and the quota ids of a
# 429 (e.g. "GenerateRequestsPerDayPerProjectPerModel"), which the message
# alone does not always carry. Empty when the reply is not Google's JSON.
.gemini_error_details <- function(resp) {
  none <- list(reason = NULL, quota = character())
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  err <- if (is.list(body)) body$error else NULL
  if (!is.list(err)) return(none)
  quota <- character()
  for (d in err$details %||% list()) {
    if (!is.list(d)) next
    for (v in d$violations %||% list()) {
      id <- if (is.list(v)) v$quotaId %||% v$quota_id else NULL
      if (is.character(id)) quota <- c(quota, id)
    }
  }
  list(reason = if (is.character(err$status) && length(err$status) == 1) err$status else NULL, quota = quota)
}

# What to do about a failed HTTP reply:
#   stop  - the whole run cannot succeed (key, account, model, billing, quota,
#           bad request)
#   skip  - this page failed; `problem` says which kind of failure it is
#           ("image" only when Google said so about the image, "server" for
#           5xx and 408, "request" for any other refusal of the page)
# Each comes with a plain-language message saying what to do next. `engine`
# ("sync", "flex", "batch") and `details` (.gemini_error_details()) are
# optional: they let a 429 say whether flex capacity, a per-minute limit or
# the daily quota ran out.
.classify_failure <- function(status, msg, model = NULL, engine = NULL, details = NULL) {
  low <- tolower(msg)
  # Google's message is followed by our own sentence: no ".." in between
  m <- sub("[.[:space:]]+$", "", msg)
  if (status == 400 && grepl("api key|api_key", low)) {
    return(list(action = "stop", message = paste0(
      "Google refused the API key (", msg, "). Run set_key() again and paste only the key.")))
  }
  # the account, not the page: every page would fail the same way (found
  # with "Gemini API free tier is not available in your country. Please
  # enable billing ...", HTTP 400 FAILED_PRECONDITION)
  if (status == 400 && (identical(details$reason, "FAILED_PRECONDITION") ||
                        grepl("precondition|free tier|billing|user location|location is not supported|your country|your region", low))) {
    return(list(action = "stop", message = paste0(
      "Google refused the account, not the page (HTTP 400): ", m, ". Switch on billing for this Google project ",
      "(or use a key from a project in a supported country) and run the same line again.")))
  }
  if (status %in% c(401, 403)) {
    extra <- if (grepl("billing|batch", low)) " This feature needs a key from a project with billing switched on." else
      " Run set_key() again with a valid key."
    return(list(action = "stop", message = paste0("Google refused the request (HTTP ", status, "): ", m, ".", extra)))
  }
  if (status == 402) {
    return(list(action = "stop", message = paste0(
      "Google says the account has no credit left (HTTP 402): ", m, ". Top up the billing account and run again.")))
  }
  if (status == 404) {
    # only a generateContent or batch-create call names a model; for files and
    # batch jobs a 404 means the item is gone, not that the model is unknown
    if (is.null(model)) {
      return(list(action = "stop", message = paste0("Google could not find it (HTTP 404): ", m, ".")))
    }
    return(list(action = "stop", message = paste0(
      "Google does not know the model '", model, "' (HTTP 404): ", m, ". Check the name; gemini_models() lists current ones.")))
  }
  if (status == 429) {
    # a 429 that outlived the retries: flex at capacity and per-minute limits
    # clear within minutes; only a per-day quota waits for midnight Pacific
    said <- tolower(paste(c(msg, details$quota), collapse = " "))
    if (grepl("perday|per day|per_day|daily", said)) {
      return(list(action = "stop", message = paste0(
        "Google's daily quota for this key is used up (HTTP 429): ", m,
        ". Finished pages are kept: run the same line again after midnight Pacific time (when daily quotas reset), or raise your usage tier.")))
    }
    if (identical(engine, "flex") || grepl("flex", low)) {
      return(list(action = "stop", message = paste0(
        "Google's flex tier is at capacity right now (HTTP 429): ", m,
        ". Finished pages are kept: run the same line again in a few minutes, or use flex = FALSE (full price) to go on now.")))
    }
    if (grepl("perminute|per minute|per_minute", said)) {
      return(list(action = "stop", message = paste0(
        "Google's per-minute limit for this key was reached (HTTP 429): ", m,
        ". Finished pages are kept: wait a minute or two and run the same line again (with fewer workers if you used several).")))
    }
    return(list(action = "stop", message = paste0(
      "Google's rate limit or quota for this key was reached (HTTP 429): ", m,
      ". Finished pages are kept: wait a minute or two and run the same line again (with fewer workers if you used several). ",
      "If it happens again, the daily quota may be used up: daily quotas reset at midnight Pacific time, or raise your usage tier.")))
  }
  if (status == 400 && grepl("base64 decoding failed", low)) {
    return(list(action = "stop", message = paste0(
      "Google could not read the image data amanuensis sent (", msg, "). This is a problem in the package, not in ",
      "your pages: update amanuensis, or report it with this message.")))
  }
  if (status == 400 && grepl("cannot find field|unknown name", low) && !grepl("schema", low)) {
    return(list(action = "stop", message = paste0(
      "Google did not recognise a field in the request (", msg, "). If you used api_args, check them; ",
      "otherwise Google may have changed its API: update amanuensis.")))
  }
  if (status == 413 || (status == 400 && grepl("image|inline_data|inlinedata|media", low))) {
    return(list(action = "skip", problem = "image", message = paste0("Google could not use this image (HTTP ", status, "): ", msg)))
  }
  if (status == 400 && grepl("thinking", low)) {
    return(list(action = "stop", message = paste0(
      "Google rejected the thinking setting (", msg, "). Use thinking = \"low\".")))
  }
  if (status == 400 && grepl("schema|response_json|responsejson", low)) {
    return(list(action = "stop", message = paste0(
      "Google rejected the codebook (", msg, "). Simplify the codebook, or check the JSON schema if you wrote one.")))
  }
  if (status >= 500 || status == 408) {
    return(list(action = "skip", problem = "server", message = paste0(
      "Google had a server problem (HTTP ", status, "): ", m, ". The page will be tried again next run.")))
  }
  list(action = "skip", problem = "request", message = paste0("Google returned an error (HTTP ", status, "): ", msg))
}

# ------- request bodies -------------------------------------------------------

.resolution_levels <- c("low", "medium", "high", "ultra_high")

.media_level <- function(resolution) paste0("MEDIA_RESOLUTION_", toupper(resolution))

.mime_of <- function(path) {
  switch(.ext(path),
         jpg = , jpeg = "image/jpeg",
         png = "image/png",
         webp = "image/webp",
         heic = "image/heic",
         heif = "image/heif",
         .abort("Cannot send {.file {basename(path)}} to Gemini: it accepts JPEG, PNG, WEBP and HEIC. Run {.fn images} first."))
}

# The generateContent request for one image: the image part first, then the
# instructions (Google's advice), no system instruction, schema-constrained
# JSON. `case = "snake"` gives the field names used in batch JSONL lines.
# ultra_high only exists per image part; the other levels go in the config.
.build_body <- function(prompt, image_b64, mime, schema, settings, case = c("camel", "snake")) {
  case <- match.arg(case)
  k <- function(camel, snake) if (case == "camel") camel else snake
  res <- settings$resolution
  image_part <- stats::setNames(list(stats::setNames(list(mime, image_b64), c(k("mimeType", "mime_type"), "data"))),
                                k("inlineData", "inline_data"))
  if (identical(res, "ultra_high")) {
    image_part[[k("mediaResolution", "media_resolution")]] <- list(level = .media_level(res))
  }
  config <- list()
  config[[k("responseMimeType", "response_mime_type")]] <- "application/json"
  if (!is.null(schema)) {
    if (identical(settings$schema_channel, "openapi")) {
      config[[k("responseSchema", "response_schema")]] <- .to_response_schema(schema)
    } else {
      config[[k("responseJsonSchema", "response_json_schema")]] <- schema
    }
  }
  config[[k("maxOutputTokens", "max_output_tokens")]] <- as.integer(settings$max_tokens)
  config[[k("thinkingConfig", "thinking_config")]] <-
    stats::setNames(list(settings$thinking_level, FALSE), c(k("thinkingLevel", "thinking_level"), k("includeThoughts", "include_thoughts")))
  if (!identical(res, "ultra_high")) config[[k("mediaResolution", "media_resolution")]] <- .media_level(res)
  if (!is.null(settings$temperature)) config$temperature <- settings$temperature
  body <- list(contents = list(list(role = "user", parts = list(image_part, list(text = prompt)))))
  body[[k("generationConfig", "generation_config")]] <- config
  if (isTRUE(settings$flex) && case == "camel") body$serviceTier <- "flex"
  api_args <- settings$api_args %||% list()
  if (length(api_args)) {
    if (case == "snake") api_args <- .snake_keys(api_args)
    body <- .merge_lists(body, api_args)
  }
  body
}

# Field names of the request in snake_case for batch lines. A schema inside
# api_args is the user's data, not request fields: its property names
# (firstName) and keywords must reach Google unchanged, so the walk stops at
# any schema-valued field.
.snake_keys <- function(x) {
  if (!is.list(x)) return(x)
  nm <- names(x)
  if (is.null(nm)) return(lapply(x, .snake_keys))
  names(x) <- tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", nm))
  verbatim <- names(x) %in% c("response_json_schema", "response_schema", "parameters_json_schema",
                              "properties", "definitions", "$defs")
  x[!verbatim] <- lapply(x[!verbatim], .snake_keys)
  x
}

# The older OpenAPI-style schema ("responseSchema"): one upper-case type plus
# "nullable" instead of type arrays. Batch runs use it by default for the
# package's own codebooks, because it is the channel verified in real
# file-based v1beta batch jobs and those codebooks use nothing it loses; a
# JSON schema the user wrote goes to batch as it is, through the JSON channel
# (see .batch_schema_channel()).
# The conversion keeps enum (minus null, which becomes nullable), adds
# propertyOrdering (OpenAPI schemas otherwise come back in alphabetical order)
# and keeps the size and range keywords Gemini's Schema object supports. Where
# JSON Schema says more than OpenAPI can:
#   - oneOf is read as anyOf, and allOf branches are merged into one node
#   - a {type: null} branch makes the node nullable (a single branch left is
#     merged in); it never becomes a string branch the schema did not allow
#   - const becomes a one-value enum
#   - enum is kept on strings only, the one type Google's Schema allows it on;
#     numbers and booleans keep their type
#   - only formats the Schema object accepts are kept (enum and date-time on
#     strings, int32/int64, float/double)
#   - every node gets a type: a node with nothing OpenAPI can say became [],
#     which Google cannot parse, so every page of the batch failed
# pattern, additionalProperties and $ref are dropped; the local check still
# enforces them.
.to_response_schema <- function(node, root = node) {
  out <- .openapi_node(node, root)
  if (!is.list(out)) return(out)
  if (is.null(out$type) && is.null(out$anyOf)) {
    out$type <- if (!is.null(out$properties)) "OBJECT" else if (!is.null(out$items)) "ARRAY" else "STRING"
  }
  out
}

.openapi_formats <- list(STRING = c("enum", "date-time"), INTEGER = c("int32", "int64"), NUMBER = c("float", "double"))

# One node, without the final type fallback (a parent of anyOf branches may
# rightly have no type of its own).
.openapi_node <- function(node, root) {
  if (!is.list(node)) return(node)
  if (!is.null(node[["$ref"]])) {
    target <- .resolve_ref(node[["$ref"]], root)
    if (!is.null(target)) return(.openapi_node(target, root))
  }
  if (!is.null(node$allOf)) return(.openapi_node(.merge_all_of(node, root), root))
  alts <- node$anyOf %||% node$oneOf
  if (!is.null(alts)) {
    alts <- lapply(alts, .deref_schema, root = root)
    is_null <- vapply(alts, .is_null_schema, logical(1))
    rest <- alts[!is_null]
    parent <- node[setdiff(names(node), c("anyOf", "oneOf"))]
    if (length(rest) == 1) {
      lone <- rest[[1]]
      out <- .openapi_node(c(parent, lone[setdiff(names(lone), names(parent))]), root)
    } else {
      out <- .openapi_node(parent, root)
      if (length(rest)) out$anyOf <- lapply(rest, .to_response_schema, root = root)
    }
    if (any(is_null)) out$nullable <- TRUE
    return(out)
  }
  out <- list()
  if (!is.null(node$description)) out$description <- node$description
  enum <- node$enum
  if (is.null(enum) && !is.null(node[["const"]])) enum <- list(node[["const"]])
  ty <- unlist(node$type)
  if (length(ty)) {
    base <- setdiff(ty, "null")
    out$type <- toupper(if (length(base)) base[1] else "string")
    if ("null" %in% ty) out$nullable <- TRUE
  }
  if (!is.null(enum)) {
    vals <- Filter(Negate(is.null), enum)
    if (length(vals) < length(enum)) out$nullable <- TRUE
    if (is.null(out$type)) out$type <- .openapi_type_of(vals)
    if (identical(out$type, "STRING")) out$enum <- lapply(vals, as.character)
  }
  fmt <- node$format
  if (is.character(fmt) && length(fmt) == 1 && !is.null(out$type) && fmt %in% .openapi_formats[[out$type]]) out$format <- fmt
  if (!is.null(node$properties)) {
    props <- lapply(node$properties, .to_response_schema, root = root)
    out$properties <- if (length(props)) props else .json_object()
    out$propertyOrdering <- as.list(names(node$properties))
  }
  if (!is.null(node$required)) out$required <- as.list(unlist(node$required))
  if (!is.null(node$items)) out$items <- .to_response_schema(node$items, root)
  for (kw in c("minItems", "maxItems", "minimum", "maximum")) if (!is.null(node[[kw]])) out[[kw]] <- node[[kw]]
  out
}

.deref_schema <- function(node, root) {
  for (i in 1:20) {
    if (!is.list(node) || is.null(node[["$ref"]])) break
    target <- .resolve_ref(node[["$ref"]], root)
    if (is.null(target)) break
    node <- target
  }
  node
}

.is_null_schema <- function(node) {
  if (!is.list(node)) return(FALSE)
  if (identical(unlist(node$type), "null")) return(TRUE)
  !is.null(node$enum) && length(node$enum) > 0 && all(vapply(node$enum, is.null, logical(1)))
}

# The type an enum's values imply when the schema gives none.
.openapi_type_of <- function(vals) {
  if (length(vals) && all(vapply(vals, is.logical, logical(1)))) return("BOOLEAN")
  if (length(vals) && all(vapply(vals, is.numeric, logical(1)))) {
    return(if (all(vapply(vals, function(v) v == round(v), logical(1)))) "INTEGER" else "NUMBER")
  }
  "STRING"
}

# allOf branches merged into the node: properties joined, required lists
# joined, any other keyword taken from the last branch that has it.
.merge_all_of <- function(node, root) {
  out <- node[setdiff(names(node), "allOf")]
  for (b in node$allOf) {
    b <- .deref_schema(b, root)
    if (!is.list(b)) next
    for (nm in names(b)) {
      if (identical(nm, "required")) {
        out$required <- as.list(unique(c(unlist(out$required), unlist(b$required))))
      } else if (identical(nm, "properties")) {
        props <- out$properties %||% list()
        for (p in names(b$properties)) props[[p]] <- b$properties[[p]]
        out$properties <- props
      } else if (!is.null(b[[nm]])) {
        out[[nm]] <- b[[nm]]
      }
    }
  }
  out
}

# The schema as sent to Google: the user's schema minus "$schema" (not part of
# Gemini's supported keyword list).
.schema_for_api <- function(schema) {
  if (is.null(schema)) return(NULL)
  schema[["$schema"]] <- NULL
  schema
}

# Plain base64 on one line. Not jsonlite::base64_enc(), which wraps lines
# every 72 characters (MIME style): Google rejects that with HTTP 400 "Base64
# decoding failed" (found on the first real run).
.encode_image <- function(path) {
  bytes <- readBin(path, "raw", n = file.size(path))
  openssl::base64_encode(bytes, linebreaks = FALSE)
}

# ------- one call -------------------------------------------------------------

.generate_request <- function(body_json, model, key, timeout) {
  req <- .gemini_request(paste0("models/", model, ":generateContent"), key = key, timeout = timeout)
  httr2::req_body_raw(req, charToRaw(body_json), type = "application/json")
}

# ------- replies --------------------------------------------------------------

# Turn a generateContent reply (parsed JSON list) into what we keep: the text
# (non-thought parts joined, as the SDKs do), finish reason, block reason,
# model version and token counts. `raw_body` is the reply text before parsing,
# used to catch NUL characters that jsonlite would silently cut strings at.
.read_reply <- function(json, raw_body = NULL) {
  usage <- json$usageMetadata %||% json$usage_metadata %||% list()
  pick <- function(camel, snake) usage[[camel]] %||% usage[[snake]]
  thinking_raw <- pick("thoughtsTokenCount", "thoughts_token_count")
  tokens <- list(
    input = as.numeric(pick("promptTokenCount", "prompt_token_count") %||% NA),
    cached = as.numeric(pick("cachedContentTokenCount", "cached_content_token_count") %||% 0),
    thinking = as.numeric(thinking_raw %||% 0),
    thinking_raw = if (is.null(thinking_raw)) NA_real_ else as.numeric(thinking_raw),
    output = as.numeric(pick("candidatesTokenCount", "candidates_token_count") %||% NA),
    total = as.numeric(pick("totalTokenCount", "total_token_count") %||% NA)
  )
  details <- pick("promptTokensDetails", "prompt_tokens_details") %||% list()
  image_tokens <- NA_real_
  for (d in details) {
    if (identical(d$modality, "IMAGE")) image_tokens <- as.numeric(d$tokenCount %||% d$token_count %||% NA)
  }
  cand <- json$candidates %||% list()
  out <- list(text = NULL, finish_reason = NA_character_, block_reason = NA_character_,
              model_version = json$modelVersion %||% json$model_version %||% NA_character_, tokens = tokens,
              image_tokens = image_tokens,
              nul = !is.null(raw_body) && grepl("\\u0000", raw_body, fixed = TRUE))
  if (!length(cand)) {
    fb <- json$promptFeedback %||% json$prompt_feedback %||% list()
    out$block_reason <- fb$blockReason %||% fb$block_reason %||% "no reply"
    return(out)
  }
  c1 <- cand[[1]]
  out$finish_reason <- c1$finishReason %||% c1$finish_reason %||% NA_character_
  parts <- c1$content$parts %||% list()
  texts <- vapply(parts, function(p) {
    if (isTRUE(p$thought) || is.null(p$text)) "" else p$text
  }, character(1))
  out$text <- paste(texts, collapse = "")
  out
}

# Plain-language reason for a finish reason other than STOP.
.finish_message <- function(reason, max_tokens) {
  switch(reason %||% "NA",
    MAX_TOKENS = paste0("The reply was cut off at max_tokens = ", .fmt_int(max_tokens),
                        ". The page is very dense, or the model got stuck repeating a character. ",
                        "Run again with a larger max_tokens (for example ", .fmt_int(min(2 * max_tokens, 65536)), ")."),
    CONTINUATION = paste0("The reply was cut off at max_tokens = ", .fmt_int(max_tokens), ". Run again with a larger max_tokens."),
    SAFETY = , PROHIBITED_CONTENT = , BLOCKLIST = , IMAGE_SAFETY = , SPII =
      paste0("Google's content filter blocked this page (", reason, "). Try again later; if it repeats, transcribe this page by hand."),
    RECITATION = "Google blocked the reply because it resembles published text it was trained on (RECITATION). This can happen with famous printed works; try again, or transcribe this page by hand.",
    `NA` = "Google sent an incomplete reply (no finish reason).",
    paste0("The reply ended unexpectedly (", reason, ").")
  )
}

# ------- Files and Batch endpoints -------------------------------------------

.gemini_upload <- function(path, key, display_name, mime = "application/jsonl", retries = 4) {
  size <- file.size(path)
  start <- httr2::request(.gemini_base_url()) |>
    httr2::req_url_path_append("upload", "v1beta", "files") |>
    httr2::req_headers(`x-goog-api-key` = key, .redact = "x-goog-api-key",
                       `X-Goog-Upload-Protocol` = "resumable",
                       `X-Goog-Upload-Command` = "start",
                       `X-Goog-Upload-Header-Content-Length` = format(size, scientific = FALSE),
                       `X-Goog-Upload-Header-Content-Type` = mime) |>
    httr2::req_body_raw(.to_json(list(file = list(display_name = display_name))), type = "application/json") |>
    httr2::req_timeout(300) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- .gemini_perform(start, retries = retries)
  .stop_if_failed(resp, "Starting the upload of the batch file failed")
  url <- httr2::resp_header(resp, "x-goog-upload-url")
  if (is.null(url)) .abort("Google did not return an upload address for the batch file.")
  send <- httr2::request(url) |>
    httr2::req_headers(`x-goog-api-key` = key, .redact = "x-goog-api-key",
                       `X-Goog-Upload-Offset` = "0",
                       `X-Goog-Upload-Command` = "upload, finalize") |>
    httr2::req_body_file(path, type = mime) |>
    httr2::req_timeout(3600) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- .gemini_perform(send, retries = retries)
  .stop_if_failed(resp, "Uploading the batch file failed")
  httr2::resp_body_json(resp)$file$name
}

.gemini_batch_create <- function(model, file_name, key, display_name, retries = 4) {
  req <- .gemini_request(paste0("models/", model, ":batchGenerateContent"), key = key, timeout = 300) |>
    httr2::req_body_raw(.to_json(list(batch = list(
      displayName = display_name,
      model = paste0("models/", model),
      inputConfig = list(fileName = file_name)))), type = "application/json")
  # creating a job is not idempotent: never retry a request that may have
  # reached Google, only network failures before it was sent
  resp <- .gemini_perform(req, retries = 0)
  .stop_if_failed(resp, "Creating the batch job failed", model = model)
  httr2::resp_body_json(resp)
}

.gemini_batch_get <- function(name, key, retries = 4) {
  resp <- .gemini_perform(.gemini_request(name, key = key, timeout = 120), retries = retries, quiet = TRUE)
  .stop_if_failed(resp, "Checking the batch job failed")
  httr2::resp_body_json(resp)
}

.gemini_download <- function(file_name, key, retries = 4) {
  req <- .gemini_request(paste0(file_name, ":download"), key = key, version = "download/v1beta", timeout = 3600) |>
    httr2::req_url_query(alt = "media")
  resp <- .gemini_perform(req, retries = retries)
  .stop_if_failed(resp, "Downloading the batch results failed")
  # the results file is UTF-8 whatever the content type says; unmarked, the
  # JSON parser would read it in the session's encoding and, in a C or
  # Latin-1 session (cron, a server with LANG unset), store "F<c3><a0>bregas"
  # in the paid page records
  text <- rawToChar(httr2::resp_body_raw(resp))
  Encoding(text) <- "UTF-8"
  text
}

.gemini_file_delete <- function(name, key) {
  req <- .gemini_request(name, key = key, timeout = 60) |> httr2::req_method("DELETE")
  resp <- tryCatch(.gemini_perform(req, retries = 1, quiet = TRUE), error = function(e) NULL)
  !is.null(resp) && httr2::resp_status(resp) < 300
}

# The error carries the HTTP status (`status` field) so callers decide by the
# number, not by matching the message text.
.stop_if_failed <- function(resp, what, model = NULL, call = rlang::caller_env()) {
  status <- httr2::resp_status(resp)
  if (status < 300) return(invisible(resp))
  why <- .classify_failure(status, .gemini_error_message(resp), model = model, details = .gemini_error_details(resp))
  .abort(c(paste0(what, "."), "x" = "{why$message}"), class = "amanuensis_api_error", status = status, call = call)
}

# The HTTP status of an amanuensis_api_error, or NA.
.error_status <- function(e) {
  s <- if (is.list(e) || is.environment(e)) e[["status"]] else NULL
  if (is.numeric(s) && length(s) == 1) as.integer(s) else NA_integer_
}

# Batch states arrive as BATCH_STATE_* on the wire and JOB_STATE_* in the docs.
.batch_state <- function(job) {
  state <- job$metadata$state %||% job$state %||% "UNSPECIFIED"
  sub("^(BATCH|JOB)_STATE_", "", state)
}

.batch_results_file <- function(job) {
  job$response$responsesFile %||% job$metadata$output$responsesFile %||%
    job$dest$fileName %||% NULL
}
