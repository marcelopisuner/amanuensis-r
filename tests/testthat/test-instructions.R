# A minimal .docx made by hand: only word/document.xml matters to the reader.
make_docx <- function(path, body_xml) {
  dir <- tempfile("docx")
  dir.create(file.path(dir, "word"), recursive = TRUE)
  dir.create(file.path(dir, "_rels"))
  writeLines(c('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
               '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
               '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
               '<Default Extension="xml" ContentType="application/xml"/>',
               '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>',
               '</Types>'), file.path(dir, "[Content_Types].xml"))
  writeLines(c('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
               '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
               '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>',
               '</Relationships>'), file.path(dir, "_rels", ".rels"))
  xml <- paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
                '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body>',
                body_xml, '<w:sectPr/></w:body></w:document>')
  writeBin(charToRaw(enc2utf8(xml)), file.path(dir, "word", "document.xml"))
  zip::zip(path, files = c("[Content_Types].xml", "_rels/.rels", "word/document.xml"), root = dir)
  unlink(dir, recursive = TRUE)
  path
}

test_that("text instructions get the standard rules appended by default", {
  ins <- instructions(text = "These are pages of a 1930 register.\r\nOne page per image.  ")
  expect_s3_class(ins, "amanuensis_instructions")
  expect_identical(ins$text, "These are pages of a 1930 register.\nOne page per image.")
  expect_true(ins$rules)
  expect_identical(ins$rules_id, "rules-1")
  expect_identical(ins$full, paste0(ins$text, "\n\n", .rules_texts[["rules-1"]]))
  expect_identical(ins$source, "text")
  expect_identical(ins$sha12, .sha12(ins$full))
  out <- capture.output(print(ins))
  expect_match(out[1], "typed in R.*standard rules added")
  expect_true(any(grepl("^# Rules", out)))
})

test_that("rules = FALSE sends exactly the text; rules can name an older version", {
  ins <- instructions(text = "Only this.", rules = FALSE)
  expect_identical(ins$full, "Only this.")
  expect_false(ins$rules)
  expect_identical(ins$rules_id, "none")
  expect_output(print(ins), "NOT added")
  old <- instructions(text = "Only this.", rules = "rules-1")
  expect_identical(old$full, instructions(text = "Only this.")$full)
  expect_error(instructions(text = "x", rules = "rules-99"), class = "amanuensis_error_input", regexp = "rules-1")
  expect_false(identical(ins$sha12, old$sha12))
})

test_that("no file and no text gives the default instructions", {
  ins <- instructions()
  expect_identical(ins$source, "default")
  expect_identical(ins$text, .default_instructions)
  expect_output(print(ins), "the package's default")
})

test_that("text given where a file was expected is used as the instructions", {
  expect_message(ins <- instructions("Transcribe every row of the register exactly."), "Using this text")
  expect_identical(ins$source, "text")
  expect_identical(ins$text, "Transcribe every row of the register exactly.")
  # a missing file name with an extension is not mistaken for text
  expect_error(instructions("my notes.md"), class = "amanuensis_error", regexp = "Cannot find")
})

test_that(".txt, .md and .tex files are read as UTF-8 with BOM and CRLF handled", {
  dir <- withr::local_tempdir()
  for (ext in c("txt", "md", "tex")) {
    p <- file.path(dir, paste0("ins.", ext))
    writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(enc2utf8("# Role\r\nPadr\u00f3 de 1930.\r\n"))), p)
    ins <- instructions(p)
    expect_identical(ins$text, "# Role\nPadr\u00f3 de 1930.")
    expect_identical(ins$source, p)
    expect_output(print(ins), paste0("ins.", ext))
  }
})

test_that("a Windows-1252 file is read with a warning", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "ins.txt")
  writeBin(c(charToRaw("Padr"), as.raw(0xf3), charToRaw(" de 1930.")), p)
  expect_warning(ins <- instructions(p), "Windows-1252")
  expect_identical(ins$text, "Padr\u00f3 de 1930.")
})

test_that("text is normalised to NFC, so the same file hashes the same everywhere", {
  a <- instructions(text = "Padro\u0301")
  b <- instructions(text = "Padr\u00f3")
  expect_identical(a$sha12, b$sha12)
})

test_that("a .docx is read paragraph by paragraph, with tabs, breaks and entities", {
  dir <- withr::local_tempdir()
  p <- make_docx(file.path(dir, "ins.docx"), paste0(
    '<w:p><w:r><w:t>Role</w:t></w:r></w:p>',
    '<w:p><w:r><w:t xml:space="preserve">Padr&#243; &amp; cens </w:t></w:r><w:r><w:t>de 1930</w:t></w:r></w:p>',
    '<w:p><w:r><w:t>a</w:t><w:tab/><w:t>b</w:t><w:br/><w:t>&lt;c&gt; &#xE9;</w:t></w:r></w:p>',
    '<w:p><w:r><w:delText>deleted</w:delText><w:instrText>PAGE</w:instrText></w:r></w:p>'))
  ins <- instructions(p)
  expect_identical(ins$text, "Role\nPadr\u00f3 & cens de 1930\na\tb\n<c> \u00e9")
  expect_identical(ins$source, p)
})

test_that("a .docx with automatic numbering or text boxes warns", {
  dir <- withr::local_tempdir()
  p <- make_docx(file.path(dir, "num.docx"),
                 '<w:p><w:pPr><w:numPr><w:ilvl w:val="0"/></w:numPr></w:pPr><w:r><w:t>First point</w:t></w:r></w:p>')
  expect_warning(instructions(p), "automatic numbering")
  p2 <- make_docx(file.path(dir, "box.docx"),
                  '<w:p><w:r><w:t>Text</w:t><w:txbxContent><w:p><w:r><w:t>boxed</w:t></w:r></w:p></w:txbxContent></w:r></w:p>')
  expect_warning(instructions(p2), "text boxes")
})

test_that("a .docx written by the package's own writer reads back", {
  skip_if_not(exists(".write_docx", envir = asNamespace("amanuensis"), inherits = FALSE),
              "the docx writer is not part of this build yet")
  dir <- withr::local_tempdir()
  p <- file.path(dir, "template.docx")
  expect_message(instructions_template(p), "Wrote")
  expect_warning(ins <- instructions(p), "placeholder")
  expect_match(ins$text, "^Role\n")
  expect_match(ins$text, "Details")
})

test_that("unreadable or unsupported files say what to do", {
  dir <- withr::local_tempdir()
  bad <- file.path(dir, "broken.docx")
  writeLines("not a zip", bad)
  expect_error(suppressWarnings(instructions(bad)), class = "amanuensis_error", regexp = "could not be opened")
  doc <- file.path(dir, "old.doc")
  writeLines("x", doc)
  expect_error(instructions(doc), class = "amanuensis_error", regexp = "save it as .docx")
  rtf <- file.path(dir, "x.rtf")
  writeLines("x", rtf)
  expect_error(instructions(rtf), class = "amanuensis_error", regexp = "RTF")
  odt <- file.path(dir, "x.odt")
  writeLines("x", odt)
  expect_error(instructions(odt), class = "amanuensis_error", regexp = "\\.docx, \\.txt")
})

test_that("unfilled placeholders and very long instructions warn", {
  expect_warning(instructions(text = "You are transcribing <title and date of the source>."), "placeholder")
  # HTML-like tags are not placeholders
  expect_silent(instructions(text = "Keep <b> marks as written."))
  long <- paste(rep("word", 1300), collapse = " ")
  expect_warning(instructions(text = long), "long \\(1300 words\\)")
})

test_that("bad combinations and empty instructions stop", {
  expect_error(instructions(file = "a.md", text = "b"), class = "amanuensis_error_input", regexp = "not both")
  expect_error(instructions(text = "   \n "), class = "amanuensis_error_input", regexp = "empty")
  expect_error(instructions(text = 3), class = "amanuensis_error_input", regexp = "must be text")
})

test_that("instructions_template() writes .md and .txt starters", {
  dir <- withr::local_tempdir()
  md <- file.path(dir, "ins.md")
  expect_message(instructions_template(md), "Wrote")
  expect_match(readLines(md)[1], "^# Role")
  expect_warning(instructions(md), "placeholder")
  expect_error(instructions_template(md), class = "amanuensis_error", regexp = "already exists")
  quietly(instructions_template(md, overwrite = TRUE))
  expect_error(instructions_template(file.path(dir, "x.pdf")), class = "amanuensis_error")
})

test_that(".as_instructions accepts objects and files, refuses a missing file, defaults otherwise", {
  ins <- test_instructions()
  expect_identical(.as_instructions(ins), ins)
  expect_message(d <- .as_instructions(NULL), "general transcription instructions")
  expect_identical(d$source, "default")
  expect_error(.as_instructions("documents/promt.md"), class = "amanuensis_error_input",
               regexp = "Cannot find the instructions file")
  expect_error(.as_instructions(1), class = "amanuensis_error_input")
})

test_that("every placeholder of the template is found and counted, until all are filled", {
  # A length limit of 60 characters once found 1 of the template's 7 slots,
  # so a template with only that one filled in went out with every paid page.
  n <- sum(lengths(regmatches(.template_lines, gregexpr("<[^<>]+>", .template_lines))))
  expect_identical(n, 7L)
  dir <- withr::local_tempdir()
  for (ext in c("md", "docx")) {
    p <- file.path(dir, paste0("template.", ext))
    quietly(instructions_template(p))
    expect_warning(instructions(p), "contain 7 placeholders")
  }
  one <- sub("<one page / a two-page spread / one card>", "one page", .template_lines, fixed = TRUE)
  expect_warning(instructions(text = one), "contain 6 placeholders")
  all <- gsub("<[^<>]+>", "filled in", .template_lines)
  expect_silent(instructions(text = all))
  # the longest slot a template line can hold is found; comparisons and tags are not slots
  expect_warning(instructions(text = paste0("Source: <", strrep("title, place, date, ", 9), "shelfmark>.")), "1 placeholder")
  expect_silent(instructions(text = "Ages < 20 and > 60 are rare; keep <b> and </b> marks as written."))
})
