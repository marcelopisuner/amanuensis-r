# The package's own table of Gemini models: which thinking levels each accepts,
# its output cap, and its price. Model names change every few months, so the
# table carries the date it was checked and every function treats an unknown
# model as allowed-but-unchecked (no price, no setting checks) rather than an
# error. Prices are USD per million tokens from
# https://ai.google.dev/gemini-api/docs/pricing, read 2026-10-23. Thinking
# tokens are billed at the output price. Batch and flex cost half.

.models_checked <- "2026-10-23"

.model_table <- function() {
  rows <- list(
    # model,                   status,    routes_to,          thinking levels,            max_out, ultra, timeout, in,   out,   cached, next_from,    next_in, next_out, next_cached
    list("gemini-3.8-flash",       "stable",  NA,                 "low,medium,high",          65536L, TRUE,  600,  0.75,  3.75, 0.075, "2027-01-01", 1.50,  7.50,  0.15),
    list("gemini-4-argon",         "stable",  NA,                 "low,medium,high",          32768L, TRUE,  1200, 2.00, 10.00, 0.10,  "2026-12-01", 4.00, 20.00,  0.20),
    list("gemini-3.6-flash",       "stable",  NA,                 "minimal,low,medium,high",  65536L, TRUE,  600,  0.75,  3.75, 0.075, "2027-01-01", 1.50,  7.50,  0.15),
    list("gemini-3.5-flash-lite",  "stable",  NA,                 "minimal,low,medium,high",  65536L, TRUE,  600,  0.30,  2.50, 0.03,  NA,           NA,    NA,    NA),
    list("gemini-3.1-flash-lite",  "stable, shuts down 2027-05-07", NA, "minimal,low,medium,high", 65536L, TRUE, 600, 0.25, 1.50, 0.025, NA,     NA,    NA,    NA),
    list("gemini-3.1-pro-preview", "preview", NA,                 "low,medium,high",          65536L, TRUE,  900,  2.00, 12.00, 0.20,  NA,           NA,    NA,    NA),
    list("gemini-3-flash-preview", "preview", NA,                 "minimal,low,medium,high",  65536L, TRUE,  600,  0.50,  3.00, 0.05,  NA,           NA,    NA,    NA),
    list("gemini-3.7-flash",       "alias",   "gemini-3.8-flash", "low,medium,high",          65536L, TRUE,  600,  0.75,  3.75, 0.075, "2027-01-01", 1.50,  7.50,  0.15),
    list("gemini-3.5-flash",       "alias",   "gemini-3.6-flash", "minimal,low,medium,high",  65536L, TRUE,  600,  0.75,  3.75, 0.075, "2027-01-01", 1.50,  7.50,  0.15)
  )
  cols <- c("model", "status", "routes_to", "thinking", "max_output", "ultra_high",
            "timeout", "price_input", "price_output", "price_cached", "next_from",
            "next_input", "next_output", "next_cached")
  df <- as.data.frame(do.call(rbind, lapply(rows, function(r) {
    stats::setNames(as.data.frame(r, stringsAsFactors = FALSE), cols)
  })), stringsAsFactors = FALSE)
  df
}

#' Gemini models known to the package, with prices
#'
#' Shows the models this version of the package knows about: whether they are
#' stable, which `thinking` levels they accept, their output limit, and their
#' price per million tokens. Other model names still work in
#' [transcribe()]; the package just cannot check their settings or
#' estimate their cost.
#'
#' Prices change. The table records the date it was checked; from that date
#' on, check <https://ai.google.dev/gemini-api/docs/pricing> and pass your own
#' prices to `transcribe(prices = ...)` if they differ.
#'
#' @return A data frame, one row per model. Prices are US dollars per million
#'   tokens at the standard (one page at a time) rate; batch and flex runs cost
#'   half. Columns `next_*` give announced price changes and the date they
#'   take effect.
#' @export
#' @examples
#' gemini_models()
gemini_models <- function() {
  df <- .model_table()
  attr(df, "checked") <- .models_checked
  class(df) <- c("amanuensis_models", "data.frame")
  df
}

#' @export
print.amanuensis_models <- function(x, ...) {
  cat("Gemini models known to amanuensis (checked ", attr(x, "checked"), ")\n", sep = "")
  cat("Prices: US$ per million tokens, standard rate; batch and flex cost half.\n\n")
  show <- data.frame(
    model = x$model,
    status = ifelse(is.na(x$routes_to), x$status, paste0("alias of ", x$routes_to)),
    thinking = x$thinking,
    max_output = .fmt_int(x$max_output),
    input = sprintf("%.3f", x$price_input),
    output = sprintf("%.2f", x$price_output),
    from = ifelse(is.na(x$next_from), "",
                  sprintf("%.2f / %.2f from %s", x$next_input, x$next_output, x$next_from)),
    stringsAsFactors = FALSE
  )
  names(show)[7] <- "price change"
  print(show, row.names = FALSE, right = FALSE)
  cat("\nDefault: gemini-3.8-flash. Harder handwriting: gemini-4-argon. Cheapest clean print: gemini-3.5-flash-lite.\n")
  invisible(x)
}

# One row of the table for `model`, or NULL for a model the table does not know.
# The practice models (.practice_models in R/transcribe.R) are never in the table:
# their costs are shown at gemini-3.8-flash prices by the callers.
.model_info <- function(model) {
  if (model %in% .practice_models) return(NULL)
  tab <- .model_table()
  hit <- tab[tab$model == model, , drop = FALSE]
  if (!nrow(hit)) return(NULL)
  as.list(hit[1, ])
}

.thinking_levels <- c("minimal", "low", "medium", "high")

# Check the thinking level against what the model accepts; returns the API
# spelling ("LOW").
.thinking_for <- function(model, thinking, call = rlang::caller_env()) {
  .check_choice(thinking, .thinking_levels, arg = "thinking", call = call)
  info <- .model_info(model)
  if (!is.null(info)) {
    allowed <- strsplit(info$thinking, ",", fixed = TRUE)[[1]]
    if (!thinking %in% allowed) {
      .abort(c("{.val {model}} does not accept {.code thinking = \"{thinking}\"}.",
               "i" = "It accepts {.val {allowed}}. The default, {.val low}, works with every current model."),
             call = call)
    }
  }
  toupper(thinking)
}

# Price per million tokens for a model on a date, honouring announced changes,
# batch/flex discounts and a user override c(input =, output =, cached =).
.price_for <- function(model, date = Sys.Date(), discount = FALSE, prices = NULL) {
  if (!is.null(prices)) {
    if (!all(c("input", "output") %in% names(prices))) return(NULL)
    p <- list(input = unname(prices[["input"]]), output = unname(prices[["output"]]),
              cached = unname(if ("cached" %in% names(prices)) prices[["cached"]] else prices[["input"]]))
  } else {
    info <- .model_info(model)
    if (is.null(info)) return(NULL)
    p <- list(input = info$price_input, output = info$price_output, cached = info$price_cached)
    if (!is.na(info$next_from) && as.Date(date) >= as.Date(info$next_from)) {
      p <- list(input = info$next_input, output = info$next_output, cached = info$next_cached)
    }
  }
  if (discount) p <- lapply(p, function(v) v / 2)
  p
}

# Cost in dollars of one reply's token counts.
.cost <- function(tokens, price) {
  if (is.null(price)) return(NA_real_)
  input <- tokens$input %||% 0
  cached <- tokens$cached %||% 0
  out <- (tokens$output %||% 0) + (tokens$thinking %||% 0)
  ((input - cached) * price$input + cached * price$cached + out * price$output) / 1e6
}

.check_model <- function(model, call = rlang::caller_env()) {
  .check_string(model, arg = "model", call = call)
  # Every practice model, not only "mock": model = "example" is the README's
  # first run, and warning there that costs cannot be estimated contradicts
  # the estimate the same run prints at the end.
  if (model %in% .practice_models) return(invisible(model))
  if (grepl("^models/", model)) model <- sub("^models/", "", model)
  info <- .model_info(model)
  if (is.null(info)) {
    .inform(c("!" = "{.val {model}} is not in this package's model table (checked {(.models_checked)}).",
              "i" = "The run will go ahead, but costs cannot be estimated. See {.fn gemini_models}."))
  } else if (identical(info$status, "alias")) {
    .inform(c("!" = "{.val {model}} is an old name: Google sends these requests to {.val {info$routes_to}}.",
              "i" = "Use {.code model = \"{info$routes_to}\"} so your records name the model that actually ran; pages already done are not redone for this change."))
  } else if (grepl("preview", info$status)) {
    .inform(c("!" = "{.val {model}} is a preview model: Google may change or withdraw it without notice."))
  }
  invisible(model)
}
