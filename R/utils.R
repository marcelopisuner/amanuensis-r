# Small internal helpers shared by every file: text and JSON input/output,
# hashing, natural sorting, number formatting and condition helpers.
#
# Every text file the package writes goes through .write_text(): UTF-8 bytes,
# "\n" line endings, written to a temporary file in the same folder and then
# renamed, so a crash or an Esc in the middle of a write never leaves half a
# file behind (the per-page records are what resume relies on).

# ------- conditions -----------------------------------------------------------

# cli glue expressions ({x}) are evaluated in the caller's frame, so each
# wrapper passes its parent frame on as .envir.
.abort <- function(message, ..., class = NULL, call = rlang::caller_env(), .envir = parent.frame()) {
  cli::cli_abort(message, ..., class = c(class, "amanuensis_error"), call = call, .envir = .envir)
}

.warn <- function(message, ..., class = NULL, .envir = parent.frame()) {
  cli::cli_warn(message, ..., class = c(class, "amanuensis_warning"), .envir = .envir)
}

.inform <- function(message, ..., .envir = parent.frame()) {
  cli::cli_inform(message, ..., class = "amanuensis_message", .envir = .envir)
}

# Text from outside (Google's error messages, file names, folder paths) put
# straight into a message must not be read as cli markup: a "{" in a schema
# error would otherwise break the message itself.
.cli_escape <- function(x) {
  nm <- names(x)
  x <- gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
  names(x) <- nm
  x
}

# ------- text files -----------------------------------------------------------

# Read a whole text file as one UTF-8 string: BOM stripped, CRLF -> LF. A file
# that is not valid UTF-8 is read as Windows-1252 (what Notepad and older Excel
# write) with a warning, rather than failing. A file that starts with a UTF-16
# or UTF-32 byte-order mark is decoded from it: Windows writes those for
# "Unicode" text (Notepad's Unicode option, Excel's "Unicode Text", PowerShell
# 5.1's `echo ... >`), and they are full of zero bytes that would otherwise
# look like a binary file. `hint` is what the caller tells the user to do with
# a file it cannot read (only the caller knows which formats it accepts).
.read_text <- function(path, hint = NULL) {
  bytes <- readBin(path, "raw", n = file.size(path))
  hint <- hint %||% "Save it as plain text (UTF-8) and try again."
  bom <- .file_bom(bytes)
  if (!is.na(bom) && bom != "UTF-8") {
    skip <- if (startsWith(bom, "UTF-32")) 4L else 2L
    text <- tryCatch(iconv(list(bytes[-seq_len(skip)]), from = bom, to = "UTF-8"), error = function(e) NA_character_)
    if (is.na(text)) {
      .abort(c("{.file {path}} is saved as {bom} text but could not be read as such.",
               "i" = hint))
    }
    Encoding(text) <- "UTF-8"
    return(gsub("\r\n?", "\n", text))
  }
  if (identical(bom, "UTF-8")) bytes <- bytes[-(1:3)]
  if (any(bytes == as.raw(0))) {
    .abort(c("{.file {path}} does not look like a text file.", "i" = hint))
  }
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  if (!validUTF8(text)) {
    text <- iconv(rawToChar(bytes), from = "CP1252", to = "UTF-8", sub = "?")
    .warn(c("{.file {basename(path)}} is not saved as UTF-8; read it as Windows-1252.",
            "i" = "Check that accented letters look right, or re-save the file as UTF-8."))
  }
  gsub("\r\n?", "\n", text)
}

# The byte-order mark a file (a path, or its bytes) starts with: "UTF-8",
# "UTF-16LE", "UTF-16BE", "UTF-32LE", "UTF-32BE", or NA for none. UTF-32 is
# tested first: its little-endian mark begins with the UTF-16 one. Callers
# that hand a file to R itself (a .Renviron) can tell the user that R cannot
# read a UTF-16 file, even though .read_text() can.
.file_bom <- function(x) {
  if (is.character(x)) {
    if (!file.exists(x) || dir.exists(x)) return(NA_character_)
    x <- readBin(x, "raw", n = 4L)
  }
  starts <- function(...) {
    m <- as.raw(c(...))
    length(x) >= length(m) && identical(x[seq_along(m)], m)
  }
  if (starts(0xff, 0xfe, 0x00, 0x00)) return("UTF-32LE")
  if (starts(0x00, 0x00, 0xfe, 0xff)) return("UTF-32BE")
  if (starts(0xef, 0xbb, 0xbf)) return("UTF-8")
  if (starts(0xff, 0xfe)) return("UTF-16LE")
  if (starts(0xfe, 0xff)) return("UTF-16BE")
  NA_character_
}

.write_raw_atomic <- function(bytes, path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = ".amanuensis-", tmpdir = dir)
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  # file() warns before it fails; the friendly error below says it all
  con <- tryCatch(suppressWarnings(file(tmp, open = "wb")), error = function(e) NULL)
  if (is.null(con)) .abort_unwritable(path)
  ok <- tryCatch({
    writeBin(bytes, con)
    TRUE
  }, error = function(e) FALSE, finally = close(con))
  if (!ok) .abort_unwritable(path)
  # The old file is never deleted first: a rename replaces it in one step on
  # every OS (Windows included), so until the new file is in place the old one
  # is there whole - a crash at any moment loses neither. This matters for the
  # files that cannot be rebuilt: batch-jobs.json (a pending paid job) and the
  # user's .Renviron. Windows: antivirus or OneDrive may hold the new file for
  # a moment, hence the retries; a copy is the last resort, and if that fails
  # too the old file is left as it was.
  for (try in 1:3) {
    if (.rename_file(tmp, path)) return(invisible(path))
    getOption("amanuensis.sleep", Sys.sleep)(0.2)
  }
  if (!isTRUE(.copy_file(tmp, path))) .abort_unwritable(path)
  invisible(path)
}

# The two file moves behind .write_raw_atomic(), as functions of their own so
# that tests can make them fail.
.rename_file <- function(from, to) suppressWarnings(file.rename(from, to))

.copy_file <- function(from, to) suppressWarnings(file.copy(from, to, overwrite = TRUE))

.abort_unwritable <- function(path, call = rlang::caller_env()) {
  .abort(c("Could not write {.path {(.show_path(path))}}.",
           "i" = "If the file is open in Excel or Word, close it and run the line again.",
           "i" = "Otherwise check that the folder exists and that you may write to it."),
         call = call, class = "amanuensis_error_input")
}

# Run a writer function (writexl, haven, zip) with the same friendly error.
.with_write_errors <- function(path, expr, call = rlang::caller_env()) {
  tryCatch(expr, error = function(e) {
    if (inherits(e, "amanuensis_error")) stop(e)
    .abort(c("Could not write {.path {(.show_path(path))}}: {conditionMessage(e)}",
             "i" = "If the file is open in Excel or Word, close it and run the line again."),
           call = call, class = "amanuensis_error_input")
  })
}

# Where to write when the user did not say. A folder that already exists is
# used without asking; a new one is created only after a yes in an
# interactive session (CRAN policy on default write locations). Not
# interactive: stop with the argument to add. options(amanuensis.ask = FALSE)
# skips the question.
.confirm_location <- function(path, what, arg, call = rlang::caller_env()) {
  if (dir.exists(path)) return(invisible(path))
  if (!isTRUE(getOption("amanuensis.ask", TRUE))) return(invisible(path))
  if (!interactive()) {
    .abort(c("Say where to save {what}.",
             "i" = "Add {.code {arg} = \"{(.show_path(path))}\"} (or another folder) to the call."),
           call = call, class = "amanuensis_error_input")
  }
  ok <- utils::askYesNo(paste0("Save ", what, " in a new folder ", .show_path(path), "?"), default = TRUE)
  if (!isTRUE(ok)) {
    .abort(c("Nothing was written.", "i" = "Choose a folder with {.arg {arg}}."), call = call,
           class = "amanuensis_error_input")
  }
  invisible(path)
}

.nfc <- function(x) {
  x <- enc2utf8(as.character(x))
  ok <- !is.na(x) & validUTF8(x)
  x[ok] <- utf8::utf8_normalize(x[ok])
  x
}

# The key used to name results and match pages: case- and Unicode-insensitive
# (macOS and Windows file systems are case-insensitive; a Mac/Windows sync can
# turn an accented name from NFC into NFD).
.page_key <- function(file) tolower(.nfc(file))

.write_text <- function(text, path) {
  text <- enc2utf8(paste(text, collapse = "\n"))
  .write_raw_atomic(charToRaw(text), path)
}

.append_text <- function(lines, path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "ab")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\n"), "\n"))), con)
  invisible(path)
}

# Text as UTF-8, marked so. Unmarked text is in the locale's encoding, which
# enc2utf8() translates; but in a "C" locale it cannot, and writes the byte
# codes instead ("Marcell<c3><ad>"). So, unless the locale is Latin-1 (whose
# accented letters are not valid UTF-8 anyway), unmarked text that is valid
# UTF-8 is taken for what it is first.
.utf8_marked <- function(x) {
  x <- as.character(x)
  if (!isTRUE(l10n_info()[["Latin-1"]])) {
    fix <- !is.na(x) & Encoding(x) == "unknown" & validUTF8(x)
    Encoding(x[fix]) <- "UTF-8"
  }
  enc2utf8(x)
}

# ------- CSV ------------------------------------------------------------------

# Numbers as text without scientific notation or padding. `dec` is the
# decimal mark of numbers with decimals ("," for Excel in languages that write
# 2,5); whole numbers, yes/no and text are never touched.
.chr_values <- function(x, dec = ".") {
  if (is.factor(x)) x <- as.character(x)
  if (is.logical(x)) return(ifelse(is.na(x), NA_character_, ifelse(x, "TRUE", "FALSE")))
  if (is.numeric(x) && !is.integer(x)) {
    out <- vapply(x, function(v) if (is.na(v)) NA_character_ else
      format(v, digits = 15, scientific = FALSE, trim = TRUE), character(1))
    # format() never writes a thousands mark, so the only "." is the decimal one
    if (dec != ".") out <- sub(".", dec, out, fixed = TRUE)
    return(out)
  }
  as.character(x)
}

# A number typed with a decimal comma ("2,5", from a semicolon CSV saved by
# Excel in Spanish, Catalan, French or German) read as 2.5 when `dec` is ",".
# Only a plain number with one comma is converted: "1.250,5" (a thousands
# mark as well) is left for the caller to report as not a number rather than
# guessed at, and "2.5" reads as it always did.
.as_number <- function(x, dec = ".") {
  x <- as.character(x)
  if (dec == ",") {
    comma <- !is.na(x) & grepl("^\\s*[-+]?[0-9]*,[0-9]+\\s*$", x)
    x[comma] <- sub(",", ".", x[comma], fixed = TRUE)
  }
  suppressWarnings(as.numeric(x))
}

# A CSV written byte for byte the same on every OS and locale: UTF-8, "\n"
# line endings, fields quoted only when they contain the separator, a quote
# or a line break, empty for NA. `bom = TRUE` starts the file with the UTF-8
# byte-order mark, which Excel on Windows needs to show accents correctly.
# `dec` is the decimal mark of numbers (output() pairs ";" with ",", as R's
# write.csv2 does); it must differ from `sep`, so a number never needs quotes.
.write_csv <- function(df, path, bom = TRUE, sep = ",", dec = ".") {
  if (identical(dec, sep)) {
    .abort("The decimal mark and the column separator must differ.", class = "amanuensis_error_internal")
  }
  esc <- function(x) {
    x <- enc2utf8(.chr_values(x, dec = dec))
    x[is.na(x)] <- ""
    needs <- grepl(paste0("[\"\n\r", if (sep == "\t") "\t" else sep, "]"), x)
    x[needs] <- paste0("\"", gsub("\"", "\"\"", x[needs], fixed = TRUE), "\"")
    x
  }
  header <- paste(esc(names(df)), collapse = sep)
  body <- if (nrow(df)) do.call(paste, c(lapply(df, esc), sep = sep)) else character()
  text <- paste0(paste(c(header, body), collapse = "\n"), "\n")
  bytes <- charToRaw(enc2utf8(text))
  if (bom) bytes <- c(as.raw(c(0xef, 0xbb, 0xbf)), bytes)
  .write_raw_atomic(bytes, path)
}

# ------- JSON -----------------------------------------------------------------

# jsonlite with the settings that keep a JSON schema valid: lists stay arrays
# (so "required": ["a"] never collapses to "required": "a"), length-one atomic
# vectors become scalars, NULL and NA become null, numbers keep full precision.
.to_json <- function(x, pretty = FALSE) {
  out <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null",
                          digits = NA, pretty = pretty)
  enc2utf8(as.character(out))
}

.parse_json <- function(text) {
  jsonlite::parse_json(text, simplifyVector = FALSE)
}

.read_json <- function(path) {
  .parse_json(.read_text(path))
}

.write_json <- function(x, path, pretty = TRUE) {
  .write_text(.to_json(x, pretty = pretty), path)
}

# an empty JSON object ({}), as opposed to list() which serialises as []
.json_object <- function() stats::setNames(list(), character())

# ------- hashing --------------------------------------------------------------

# A plain character string: openssl's "hash" class would otherwise travel into
# records and the manifest, and jsonlite cannot serialise it.
.sha12 <- function(text) {
  unclass(substr(as.character(openssl::sha256(charToRaw(enc2utf8(text)))), 1, 12))
}

.md5 <- function(path) unname(tools::md5sum(path))

# ------- paths and names ------------------------------------------------------

.stem <- function(path) tools::file_path_sans_ext(basename(path))

.ext <- function(path) tolower(tools::file_ext(path))

.show_path <- function(path) normalizePath(path, winslash = "/", mustWork = FALSE)

# Natural order: "p2" before "p10", case-insensitive, so folders of phone
# photos and "scan (12).tif" style names come out in reading order.
.natural_order <- function(x) {
  key <- tolower(x)
  key <- gsub("([0-9]+)", "\001\\1\001", key)
  parts <- strsplit(key, "\001", fixed = TRUE)
  key <- vapply(parts, function(p) {
    num <- grepl("^[0-9]+$", p)
    p[num] <- formatC(as.numeric(p[num]), width = 15, format = "d", flag = "0")
    paste(p, collapse = "")
  }, character(1))
  order(key, method = "radix")
}

# ------- formatting -----------------------------------------------------------

.now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

.fmt_int <- function(n) {
  n <- as.numeric(n)
  ifelse(is.na(n), "-", formatC(n, format = "d", big.mark = ","))
}

.fmt_usd <- function(x) {
  x <- as.numeric(x)
  ifelse(is.na(x), "unknown",
         ifelse(x < 0.1, sprintf("$%.4f", x), sprintf("$%.2f", x)))
}

.fmt_pct <- function(x) ifelse(is.na(x), "-", sprintf("%.2f%%", 100 * x))

# ------- misc -----------------------------------------------------------------

.is_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x)

.check_string <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!.is_string(x)) .abort("{.arg {arg}} must be a single piece of text.", call = call)
}

.check_choice <- function(x, choices, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!.is_string(x) || !x %in% choices) {
    .abort(c("{.arg {arg}} must be one of {.val {choices}}.",
             "x" = "You gave {.val {format(x)}}."), call = call)
  }
  x
}

.check_number <- function(x, arg = rlang::caller_arg(x), min = -Inf, call = rlang::caller_env(),
                          null_ok = FALSE) {
  if (null_ok && is.null(x)) return(invisible(x))
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < min) {
    .abort("{.arg {arg}} must be a single number{if (is.finite(min)) paste0(' of at least ', min) else ''}.",
           call = call)
  }
  invisible(x)
}

.compact <- function(x) x[!vapply(x, is.null, logical(1))]

# Deep merge for request bodies: values in `new` replace those in `old`, named
# lists merge recursively, and NULL deletes a key (as utils::modifyList).
.merge_lists <- function(old, new) {
  if (!length(new)) return(old)
  utils::modifyList(old, new)
}
