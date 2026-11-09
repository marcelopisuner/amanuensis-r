# Shared set-up for the tests: options that make every run non-interactive,
# instant and offline, a fake API key, and small page images built on the fly.
# Nothing here touches the network or writes outside tempdir().

FAKE_KEY <- "AIzaSyFAKE-test-key-0123456789abcdefgh"

# Options for one test: no questions, no sleeping, no polling delay, no
# progress bars, and a fake key in GEMINI_API_KEY (GOOGLE_API_KEY unset so a
# key on the test machine can never be used).
local_amanuensis <- function(env = parent.frame(), key = FAKE_KEY) {
  withr::local_options(
    amanuensis.ask = FALSE,
    amanuensis.sleep = function(s) NULL,
    amanuensis.poll_seconds = 0,
    cli.progress_show_after = Inf,
    cli.dynamic = FALSE,
    .local_envir = env
  )
  withr::local_envvar(GEMINI_API_KEY = key, GOOGLE_API_KEY = NA, .local_envir = env)
  invisible()
}

# Prices as on `day` for the rest of the calling test: the model table
# announces price changes on dates, so a test of today's prices would fail on
# the day one takes effect.
local_price_date <- function(day, env = parent.frame()) {
  day <- as.Date(day)
  real <- .price_for
  local_mocked_bindings(
    .price_for = function(model, date = day, discount = FALSE, prices = NULL) real(model, date, discount, prices),
    .env = env
  )
}

# n small JPEGs of distinct colours (so each page's bytes, and so its base64,
# differ), named p001.jpg, p002.jpg, ... in `dir`. Returns the paths.
make_jpegs <- function(dir, n = 3, prefix = "p", width = 60, height = 40) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  paths <- character(n)
  for (i in seq_len(n)) {
    col <- sprintf("#%02x%02x%02x", (37 * i) %% 256, (91 * i) %% 256, (151 * i) %% 256)
    img <- magick::image_blank(width, height, col)
    paths[i] <- file.path(dir, sprintf("%s%03d.jpg", prefix, i))
    magick::image_write(img, paths[i], format = "jpeg", quality = 90)
  }
  paths
}

# A small text PDF made with base graphics, n pages.
make_pdf <- function(path, n = 3) {
  grDevices::pdf(path, width = 4, height = 5)
  on.exit(grDevices::dev.off())
  for (p in seq_len(n)) {
    graphics::plot.new()
    graphics::text(0.5, 0.8, paste("Page", p), cex = 2)
    for (i in 1:5) graphics::text(0.1, 0.7 - i * 0.1, paste(i, "Puig, Josep", 20 + i), adj = 0)
  }
  invisible(path)
}

# Run an expression without its messages (the package talks a lot on purpose).
quietly <- function(expr) suppressMessages(expr)

# All condition messages (messages, warnings, the error) of an expression,
# as one character vector; the error, if any, is caught.
all_condition_text <- function(expr) {
  out <- character()
  tryCatch(
    withCallingHandlers(expr,
      message = function(m) { out <<- c(out, conditionMessage(m)); invokeRestart("muffleMessage") },
      warning = function(w) { out <<- c(out, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) out <<- c(out, conditionMessage(e)))
  out
}

# A codebook with one page variable and three row variables, used by the
# extraction tests.
roster_codebook <- function() {
  quietly(codebook(data.frame(
    variable = c("heading", "name", "age", "sex"),
    type = c("text", "text", "integer", "category"),
    level = c("page", "row", "row", "row"),
    values = c("", "", "", "male; female"),
    description = c("Page heading.", "Name as written.", "Age in years.", "Sex as recorded."),
    stringsAsFactors = FALSE
  )))
}

roster_data <- function(file = "page", n = 2) {
  list(heading = paste("Heading of", file),
       records = lapply(seq_len(n), function(i) list(name = paste("Name", i), age = 20L + i, sex = "male")))
}

test_instructions <- function(text = "These are pages of a 1930 municipal register.") {
  instructions(text = text)
}
