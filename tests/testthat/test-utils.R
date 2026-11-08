test_that(".write_text writes UTF-8 with LF endings and leaves no temporary file", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "sub", "a.txt")
  .write_text(c("one", "Pe\u00f1a"), path)
  expect_identical(readBin(path, "raw", 100), c(charToRaw("one\nPe"), as.raw(c(0xc3, 0xb1)), charToRaw("a")))
  expect_identical(list.files(dirname(path), all.files = TRUE, no.. = TRUE), "a.txt")
  # replaces an existing file in one step
  .write_text("two", path)
  expect_identical(readLines(path, warn = FALSE), "two")
})

test_that(".write_raw_atomic gives a plain-language error when the folder cannot be written", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  locked <- file.path(dir, "locked")
  dir.create(locked)
  Sys.chmod(locked, "0500")
  withr::defer(Sys.chmod(locked, "0700"))
  skip_if(file.access(locked, 2) == 0, "running as a user who can write anywhere")
  expect_error(.write_text("x", file.path(locked, "a.txt")), class = "amanuensis_error_input",
               regexp = "Could not write")
})

test_that(".read_text strips the BOM, turns CRLF into LF and falls back to Windows-1252", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "bom.txt")
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("a\r\nb\rc\n")), p)
  expect_identical(.read_text(p), "a\nb\nc\n")

  p2 <- file.path(dir, "cp1252.txt")
  writeBin(c(charToRaw("Pe"), as.raw(0xf1), charToRaw("a")), p2)
  expect_warning(txt <- .read_text(p2), class = "amanuensis_warning", regexp = "Windows-1252")
  expect_identical(txt, "Pe\u00f1a")

  p3 <- file.path(dir, "binary.txt")
  writeBin(as.raw(c(0x41, 0x00, 0x42)), p3)
  expect_error(.read_text(p3), class = "amanuensis_error", regexp = "does not look like a text file")
})

test_that(".append_text adds lines with a final newline", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "log", "x.log")
  .append_text("a", p)
  .append_text(c("b", "c"), p)
  expect_identical(readLines(p), c("a", "b", "c"))
})

test_that(".write_csv quotes only when needed, writes NA as empty and keeps full numbers", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "x.csv")
  df <- data.frame(a = c("plain", "with, comma", "with \"quote\"", "two\nlines", NA),
                   n = c(1234567890123, 0.0000123456, 1.5, NA, 2),
                   b = c(TRUE, FALSE, NA, TRUE, FALSE),
                   stringsAsFactors = FALSE)
  .write_csv(df, p)
  bytes <- readBin(p, "raw", file.size(p))
  expect_identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf)))
  txt <- rawToChar(bytes[-(1:3)])
  expect_identical(txt, paste0(
    "a,n,b\n",
    "plain,1234567890123,TRUE\n",
    "\"with, comma\",0.0000123456,FALSE\n",
    "\"with \"\"quote\"\"\",1.5,\n",
    "\"two\nlines\",,TRUE\n",
    ",2,FALSE\n"))
  .write_csv(data.frame(a = "x;y", b = "x,y"), p, bom = FALSE, sep = ";")
  expect_identical(readLines(p), c("a;b", "\"x;y\";x,y"))
  # zero rows: header only
  .write_csv(df[0, ], p, bom = FALSE)
  expect_identical(readLines(p), "a,n,b")
})

test_that(".to_json keeps schemas valid: arrays stay arrays, null for NULL/NA, full precision", {
  x <- list(required = list("a"), enum = list("x", NULL), n = 0.0000123456, m = 1234.56789,
            na = NA, empty = .json_object(), none = NULL)
  j <- .to_json(x)
  expect_identical(j, "{\"required\":[\"a\"],\"enum\":[\"x\",null],\"n\":1.23456e-05,\"m\":1234.56789,\"na\":null,\"empty\":{},\"none\":null}")
  expect_identical(.parse_json(j)$m, 1234.56789)
  expect_identical(.to_json(.json_object()), "{}")
  expect_identical(.to_json(list()), "[]")
})

test_that(".write_json and .read_json round-trip non-ASCII text", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "x.json")
  .write_json(list(name = "Su\u00f1er", n = 2L), p)
  expect_identical(.read_json(p), list(name = "Su\u00f1er", n = 2L))
})

test_that(".sha12 is a stable 12-character hex digest of the UTF-8 text", {
  expect_identical(.sha12("abc"), "ba7816bf8f01")
  expect_identical(nchar(.sha12("Pe\u00f1a")), 12L)
  expect_false(identical(.sha12("a"), .sha12("b")))
})

test_that(".natural_order puts p2 before p10, ignoring case", {
  x <- c("p10.jpg", "P2.jpg", "p1.jpg", "scan (12).tif", "scan (3).tif")
  expect_identical(x[.natural_order(x)], c("p1.jpg", "P2.jpg", "p10.jpg", "scan (3).tif", "scan (12).tif"))
})

test_that(".page_key ignores case and Unicode normalisation", {
  nfd <- "Ce\u0301sar.jpg"
  nfc <- "C\u00e9sar.JPG"
  expect_identical(.page_key(nfd), .page_key(nfc))
  expect_identical(.nfc(NA_character_), NA_character_)
})

test_that("number formatting helpers", {
  expect_identical(.fmt_int(c(1234567, NA)), c("1,234,567", "-"))
  expect_identical(.fmt_usd(c(0.00123, 12.345, NA)), c("$0.0012", "$12.35", "unknown"))
  expect_identical(.fmt_pct(c(0.9337, NA)), c("93.37%", "-"))
})

test_that("argument checks say what was expected", {
  f <- function(x) .check_string(x)
  expect_error(f(1), class = "amanuensis_error", regexp = "single piece of text")
  expect_error(f(c("a", "b")), class = "amanuensis_error")
  g <- function(x) .check_choice(x, c("low", "high"))
  expect_identical(g("low"), "low")
  expect_error(g("medium"), class = "amanuensis_error", regexp = "must be one of")
  h <- function(x) .check_number(x, min = 1)
  expect_error(h(0), class = "amanuensis_error", regexp = "at least 1")
  expect_error(h("2"), class = "amanuensis_error")
  expect_invisible(h(3))
  expect_null(.check_number(NULL, null_ok = TRUE))
})

test_that(".merge_lists merges nested lists, replaces values and deletes with NULL", {
  old <- list(a = 1, cfg = list(x = 1, y = list(z = 2)), keep = "k")
  new <- list(a = 2, cfg = list(y = list(w = 3)), keep = NULL)
  expect_identical(.merge_lists(old, new), list(a = 2, cfg = list(x = 1, y = list(z = 2, w = 3))))
  expect_identical(.merge_lists(old, list()), old)
})

test_that(".confirm_location uses existing folders, and stops non-interactively when asked to ask", {
  dir <- withr::local_tempdir()
  expect_invisible(.confirm_location(dir, "things", "save_to"))
  new <- file.path(dir, "new")
  withr::local_options(amanuensis.ask = FALSE)
  expect_identical(.confirm_location(new, "things", "save_to"), new)
  withr::local_options(amanuensis.ask = TRUE)
  skip_if(interactive())
  expect_error(.confirm_location(new, "the page images", "save_to"), class = "amanuensis_error_input",
               regexp = "save_to")
  expect_false(dir.exists(new))
})

test_that(".with_write_errors turns writer errors into a friendly message", {
  expect_error(.with_write_errors("out.xlsx", stop("disk full")), class = "amanuensis_error_input",
               regexp = "Could not write.*disk full")
  # amanuensis errors pass through unchanged
  expect_error(.with_write_errors("x", .abort("mine", class = "amanuensis_error_key")), class = "amanuensis_error_key")
})

test_that("condition helpers carry the package classes", {
  expect_error(.abort("x"), class = "amanuensis_error")
  expect_warning(.warn("x"), class = "amanuensis_warning")
  expect_message(.inform("x"), class = "amanuensis_message")
  n <- 3
  expect_message(.inform("{n} page{?s}"), "3 pages")
})

test_that("small path helpers", {
  expect_identical(.stem("a/b/Scan 1.JPG"), "Scan 1")
  expect_identical(.ext("a/b/Scan 1.JPG"), "jpg")
  expect_identical(.compact(list(a = 1, b = NULL)), list(a = 1))
  expect_true(.is_string("a"))
  expect_false(.is_string(NA_character_))
  expect_match(.utc_now(), "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
})

test_that(".read_text decodes UTF-16 and UTF-32 files that start with a byte-order mark", {
  dir <- withr::local_tempdir()
  text <- "variable\ttype\r\nedat\tinteger\r\nPe\u00f1a\ttext\r\n"
  write_bom <- function(name, bom, enc) {
    p <- file.path(dir, name)
    writeBin(c(as.raw(bom), iconv(text, "UTF-8", enc, toRaw = TRUE)[[1]]), p)
    p
  }
  want <- "variable\ttype\nedat\tinteger\nPe\u00f1a\ttext\n"
  le <- write_bom("le.txt", c(0xff, 0xfe), "UTF-16LE")
  expect_identical(.file_bom(le), "UTF-16LE")
  expect_identical(.read_text(le), want)
  expect_identical(Encoding(.read_text(le)), "UTF-8")
  be <- write_bom("be.txt", c(0xfe, 0xff), "UTF-16BE")
  expect_identical(.file_bom(be), "UTF-16BE")
  expect_identical(.read_text(be), want)
  le32 <- write_bom("le32.txt", c(0xff, 0xfe, 0x00, 0x00), "UTF-32LE")
  expect_identical(.file_bom(le32), "UTF-32LE")
  expect_identical(.read_text(le32), want)
  # what Windows PowerShell 5.1's `echo GEMINI_API_KEY=... > .Renviron` writes
  renv <- file.path(dir, ".Renviron")
  writeBin(c(as.raw(c(0xff, 0xfe)), iconv("GEMINI_API_KEY=AIzaFAKE\r\n", "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), renv)
  expect_identical(strsplit(.read_text(renv), "\n", fixed = TRUE)[[1]], "GEMINI_API_KEY=AIzaFAKE")
  # a UTF-16 file cut in the middle of a character: a plain error, not garbage
  odd <- file.path(dir, "odd.txt")
  writeBin(c(as.raw(c(0xff, 0xfe, 0x00, 0xd8))), odd)
  expect_error(.read_text(odd), class = "amanuensis_error")
  # other byte-order marks, and none
  utf8 <- file.path(dir, "u8.txt")
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("a")), utf8)
  expect_identical(.file_bom(utf8), "UTF-8")
  expect_identical(.file_bom(charToRaw("abc")), NA_character_)
  expect_identical(.file_bom(file.path(dir, "missing.txt")), NA_character_)
})

test_that(".read_text's advice for a binary file is the caller's, not a fixed list of formats", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "binary.csv")
  writeBin(as.raw(c(0x41, 0x00, 0x42)), p)
  msg <- tryCatch(.read_text(p), error = conditionMessage)
  expect_match(msg, "plain text (UTF-8)", fixed = TRUE)
  expect_false(grepl(".docx", msg, fixed = TRUE))
  expect_error(.read_text(p, hint = "Save the codebook from Excel as .xlsx."), "as .xlsx", class = "amanuensis_error")
})

test_that(".write_raw_atomic never deletes the old file before the new one is in place", {
  dir <- withr::local_tempdir()
  withr::local_options(amanuensis.sleep = function(s) NULL)
  p <- file.path(dir, "batch-jobs.json")
  writeLines("{\"jobs\": [\"batches/abc\"]}", p)
  before <- readBin(p, "raw", file.size(p))
  # rename and copy both fail (antivirus or OneDrive holding the new file)
  local({
    local_mocked_bindings(.rename_file = function(from, to) FALSE, .copy_file = function(from, to) FALSE)
    expect_error(.write_text("{\"jobs\": []}", p), "Could not write", class = "amanuensis_error_input")
  })
  expect_identical(readBin(p, "raw", file.size(p)), before)
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), "batch-jobs.json")
  # rename fails, the copy works: the new content is written, no temporary file is left
  local({
    local_mocked_bindings(.rename_file = function(from, to) FALSE)
    .write_text("new", p)
  })
  expect_identical(readLines(p, warn = FALSE), "new")
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), "batch-jobs.json")
  # with nothing mocked, the rename replaces the existing file by itself
  .write_text("newer", p)
  expect_identical(readLines(p, warn = FALSE), "newer")
})

test_that(".write_csv writes a decimal comma when asked, without quotes, and refuses dec = sep", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "d.csv")
  df <- data.frame(x = c(2.5, 3.125, NA, 1069544.25, -0.5), n = c(1L, 2L, 3L, NA, 5L), t = c("1.5", "a,b", "c", "d", "e"),
                   stringsAsFactors = FALSE)
  .write_csv(df, p, bom = FALSE, sep = ";", dec = ",")
  expect_identical(readLines(p), c("x;n;t", "2,5;1;1.5", "3,125;2;a,b", ";3;c", "1069544,25;;d", "-0,5;5;e"))
  .write_csv(df, p, bom = FALSE)
  expect_identical(readLines(p)[2:3], c("2.5,1,1.5", "3.125,2,\"a,b\""))
  expect_error(.write_csv(df, p, sep = ",", dec = ","), class = "amanuensis_error")
  expect_identical(.chr_values(c(2.5, 3), dec = ","), c("2,5", "3"))
  expect_identical(.chr_values(c(TRUE, NA), dec = ","), c("TRUE", NA))
  expect_identical(.chr_values(c(2.5, 3)), c("2.5", "3"))
})

test_that(".as_number reads decimal commas only when told to, and never guesses at thousands marks", {
  x <- c("2,5", "-1,25", "2.5", "1.250,5", ",5", "x", NA, "7")
  expect_identical(.as_number(x, dec = ","), c(2.5, -1.25, 2.5, NA, 0.5, NA, NA, 7))
  expect_identical(.as_number(x), c(NA, NA, 2.5, NA, NA, NA, NA, 7))
})

test_that(".write_csv keeps values that look like formulas byte for byte", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "f.csv")
  vals <- c("=SUM(A1)", "+3", "- id. -", "@x", "1-2", "007")
  .write_csv(data.frame(a = vals, b = 1:6), p, bom = FALSE)
  expect_identical(readLines(p), c("a,b", paste0(vals, ",", 1:6)))
})
