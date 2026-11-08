# A minimal Word writer with no extra packages. A .docx file is a zip of a
# few XML files; this writes the five a word processor needs (the content
# types, the package relationships, the document, its relationships and a
# style sheet) and zips them with the zip package. Word, LibreOffice, Pages
# and macOS textutil open the result.
#
# Judgment calls:
# - Only what the package needs: a title, headings, paragraphs and plain
#   tables. No images, footnotes, numbering or tracked changes (officer does
#   all that, but would be a heavy dependency for writing a few headings).
# - Text is written exactly: every character escaped for XML, line breaks
#   kept as Word line breaks (<w:br/>) and tabs as Word tabs, so that
#   instructions() reads back the same text it was given. Control characters
#   that XML forbids (they make Word refuse the whole file) are left out;
#   .docx_control_chars() counts them so callers can say so.
# - The headings use Word's built-in "heading 1" and "heading 2" styles, so
#   the navigation pane and a table of contents work; tables use a style of
#   their own but also carry their borders, bold header and font size
#   directly, because LibreOffice ignores parts of table styles.
# - Pages are A4 (the users are mostly in Europe); a document with a table of
#   more than seven columns is set in landscape.

# Blocks: a list of list(type = "title" | "heading" | "para" | "table",
# text = <character>, table = <data.frame>, level = 1 or 2 for headings).
.write_docx <- function(blocks, path, landscape = NULL, call = rlang::caller_env()) {
  if (is.null(landscape)) {
    landscape <- any(vapply(blocks, function(b) identical(b$type, "table") && ncol(b$table) > 7, logical(1)))
  }
  page <- if (landscape) list(w = 16838L, h = 11906L) else list(w = 11906L, h = 16838L)
  margin <- 1134L   # 2 cm
  text_width <- page$w - 2L * margin
  body <- vapply(blocks, .docx_block, character(1), text_width = text_width)
  sect <- paste0('<w:sectPr><w:pgSz w:w="', page$w, '" w:h="', page$h, '"',
                 if (landscape) ' w:orient="landscape"' else '', '/>',
                 '<w:pgMar w:top="', margin, '" w:right="', margin, '" w:bottom="', margin, '" w:left="', margin,
                 '" w:header="709" w:footer="709" w:gutter="0"/></w:sectPr>')
  document <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
    '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    '<w:body>', paste(body, collapse = ""), sect, '</w:body></w:document>')

  parts <- list(
    "[Content_Types].xml" = .docx_content_types,
    "_rels/.rels" = .docx_rels,
    "word/document.xml" = document,
    "word/styles.xml" = .docx_styles,
    "word/_rels/document.xml.rels" = .docx_document_rels
  )
  td <- tempfile("docx-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  zipfile <- file.path(td, "out.docx")
  .with_write_errors(path, {
    for (nm in names(parts)) .write_text(parts[[nm]], file.path(td, "parts", nm))
    zip::zip(zipfile, files = names(parts), root = file.path(td, "parts"), mode = "mirror",
             include_directories = FALSE)
  }, call = call)
  .write_raw_atomic(readBin(zipfile, "raw", n = file.size(zipfile)), path)
  invisible(path)
}

.docx_block <- function(b, text_width) {
  type <- b$type %||% "para"
  if (type == "table") return(.docx_table(b$table, text_width))
  style <- switch(type,
    title = "Title",
    heading = if (identical(as.integer(b$level %||% 1L), 2L)) "Heading2" else "Heading1",
    para = NULL,
    .abort("Unknown Word block type {.val {type}}.", class = "amanuensis_error_internal")
  )
  .docx_paragraph(paste(b$text %||% "", collapse = "\n"), style = style)
}

.docx_paragraph <- function(text, style = NULL, bold = FALSE, size = NULL, spacing_after = NULL) {
  ppr <- paste(c(if (!is.null(style)) paste0('<w:pStyle w:val="', style, '"/>'),
                  if (!is.null(spacing_after)) paste0('<w:spacing w:after="', spacing_after, '"/>')), collapse = "")
  paste0('<w:p>', if (nzchar(ppr)) paste0('<w:pPr>', ppr, '</w:pPr>'),
         .docx_run(text, bold = bold, size = size), '</w:p>')
}

# One run holding the whole text: lines separated by <w:br/>, tab-separated
# pieces by <w:tab/>.
.docx_run <- function(text, bold = FALSE, size = NULL) {
  text <- if (length(text) != 1 || is.na(text)) "" else text
  if (!nzchar(text)) return("")
  rpr <- paste(c(if (bold) '<w:b/>', if (!is.null(size)) paste0('<w:sz w:val="', size, '"/><w:szCs w:val="', size, '"/>')),
               collapse = "")
  lines <- strsplit(gsub("\r\n?", "\n", text), "\n", fixed = TRUE)[[1]]
  if (endsWith(text, "\n")) lines <- c(lines, "")
  inner <- vapply(lines, function(line) {
    pieces <- strsplit(line, "\t", fixed = TRUE)[[1]]
    if (endsWith(line, "\t")) pieces <- c(pieces, "")
    if (!length(pieces)) pieces <- ""
    paste0('<w:t xml:space="preserve">', .xml_escape(pieces), '</w:t>', collapse = '<w:tab/>')
  }, character(1), USE.NAMES = FALSE)
  paste0('<w:r>', if (nzchar(rpr)) paste0('<w:rPr>', rpr, '</w:rPr>'), paste(inner, collapse = '<w:br/>'), '</w:r>')
}

# A table with a bold, shaded header row that repeats on every page. Column
# widths follow the length of the content, within the page width.
.docx_table <- function(df, text_width) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, optional = TRUE)
  if (!ncol(df)) return(.docx_paragraph(""))
  cells <- lapply(df, .docx_cell_text)
  heads <- names(df)
  weight <- vapply(seq_along(heads), function(j) {
    v <- utils::head(cells[[j]], 200)
    longest <- max(c(nchar(heads[j]), nchar(unlist(strsplit(v, "\n", fixed = TRUE)))), 1, na.rm = TRUE)
    min(max(longest, 3), 40)
  }, numeric(1))
  widths <- pmax(500L, as.integer(floor(text_width * weight / sum(weight))))
  border <- function(side) paste0('<w:', side, ' w:val="single" w:sz="4" w:space="0" w:color="808080"/>')
  tblpr <- paste0('<w:tblPr><w:tblStyle w:val="AmanuensisTable"/><w:tblW w:w="0" w:type="auto"/>',
                  '<w:tblBorders>', paste(vapply(c("top", "left", "bottom", "right", "insideH", "insideV"), border, ""),
                                          collapse = ""), '</w:tblBorders>',
                  '<w:tblLayout w:type="autofit"/>',
                  '<w:tblLook w:val="04A0" w:firstRow="1" w:lastRow="0" w:firstColumn="0" w:lastColumn="0" w:noHBand="1" w:noVBand="1"/>',
                  '</w:tblPr>')
  grid <- paste0('<w:tblGrid>', paste0('<w:gridCol w:w="', widths, '"/>', collapse = ""), '</w:tblGrid>')
  cell <- function(text, w, header = FALSE) {
    paste0('<w:tc><w:tcPr><w:tcW w:w="', w, '" w:type="dxa"/>',
           if (header) '<w:shd w:val="clear" w:color="auto" w:fill="E7E6E6"/>', '</w:tcPr>',
           .docx_paragraph(text, bold = header, size = 18, spacing_after = 0), '</w:tc>')
  }
  header <- paste0('<w:tr><w:trPr><w:tblHeader/></w:trPr>',
                   paste(mapply(cell, heads, widths, MoreArgs = list(header = TRUE)), collapse = ""), '</w:tr>')
  rows <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    rows[i] <- paste0('<w:tr>', paste(vapply(seq_along(cells), function(j) cell(cells[[j]][i], widths[j]), ""),
                                       collapse = ""), '</w:tr>')
  }
  paste0('<w:tbl>', tblpr, grid, header, paste(rows, collapse = ""), '</w:tbl>')
}

# Cell values as text: empty for NA, yes/no for logical, numbers without
# scientific notation.
.docx_cell_text <- function(x) {
  if (is.logical(x)) return(ifelse(is.na(x), "", ifelse(x, "yes", "no")))
  out <- .chr_values(x)
  out[is.na(out)] <- ""
  out
}

# Characters XML 1.0 forbids (C0 controls other than tab, line feed and
# carriage return) are dropped; everything else is escaped. (U+FFFE and
# U+FFFF are forbidden too, but PCRE refuses them in a pattern outside a
# UTF-8 locale, and no transcription contains them.)
.xml_bad_chars <- "[\\x{01}-\\x{08}\\x{0B}\\x{0C}\\x{0E}-\\x{1F}]"

.xml_escape <- function(x) {
  x <- enc2utf8(as.character(x))
  x[is.na(x)] <- ""
  x <- gsub(.xml_bad_chars, "", x, perl = TRUE)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# How many characters .xml_escape() would drop from a set of values.
.docx_control_chars <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- x[!is.na(x)]
  if (!length(x)) return(0L)
  sum(vapply(gregexpr(.xml_bad_chars, x, perl = TRUE), function(m) sum(m > 0), integer(1)))
}

.docx_content_types <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
  '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
  '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
  '<Default Extension="xml" ContentType="application/xml"/>',
  '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>',
  '<Override PartName="/word/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>',
  '</Types>')

.docx_rels <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>',
  '</Relationships>')

.docx_document_rels <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>',
  '</Relationships>')

.docx_styles <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n',
  '<w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
  '<w:docDefaults><w:rPrDefault><w:rPr>',
  '<w:rFonts w:ascii="Calibri" w:hAnsi="Calibri" w:eastAsia="Calibri" w:cs="Calibri"/>',
  '<w:sz w:val="22"/><w:szCs w:val="22"/><w:lang w:val="en-GB"/></w:rPr></w:rPrDefault>',
  '<w:pPrDefault><w:pPr><w:spacing w:after="120" w:line="264" w:lineRule="auto"/></w:pPr></w:pPrDefault>',
  '</w:docDefaults>',
  '<w:style w:type="paragraph" w:default="1" w:styleId="Normal"><w:name w:val="Normal"/><w:qFormat/></w:style>',
  '<w:style w:type="paragraph" w:styleId="Title"><w:name w:val="Title"/><w:basedOn w:val="Normal"/>',
  '<w:next w:val="Normal"/><w:qFormat/><w:pPr><w:spacing w:after="240"/></w:pPr>',
  '<w:rPr><w:b/><w:sz w:val="36"/><w:szCs w:val="36"/></w:rPr></w:style>',
  '<w:style w:type="paragraph" w:styleId="Heading1"><w:name w:val="heading 1"/><w:basedOn w:val="Normal"/>',
  '<w:next w:val="Normal"/><w:uiPriority w:val="9"/><w:qFormat/>',
  '<w:pPr><w:keepNext/><w:keepLines/><w:spacing w:before="360" w:after="120"/><w:outlineLvl w:val="0"/></w:pPr>',
  '<w:rPr><w:b/><w:color w:val="1F3864"/><w:sz w:val="30"/><w:szCs w:val="30"/></w:rPr></w:style>',
  '<w:style w:type="paragraph" w:styleId="Heading2"><w:name w:val="heading 2"/><w:basedOn w:val="Normal"/>',
  '<w:next w:val="Normal"/><w:uiPriority w:val="9"/><w:qFormat/>',
  '<w:pPr><w:keepNext/><w:keepLines/><w:spacing w:before="240" w:after="80"/><w:outlineLvl w:val="1"/></w:pPr>',
  '<w:rPr><w:b/><w:color w:val="1F3864"/><w:sz w:val="26"/><w:szCs w:val="26"/></w:rPr></w:style>',
  '<w:style w:type="table" w:default="1" w:styleId="TableNormal"><w:name w:val="Normal Table"/>',
  '<w:uiPriority w:val="99"/><w:semiHidden/><w:tblPr><w:tblInd w:w="0" w:type="dxa"/>',
  '<w:tblCellMar><w:top w:w="0" w:type="dxa"/><w:left w:w="108" w:type="dxa"/>',
  '<w:bottom w:w="0" w:type="dxa"/><w:right w:w="108" w:type="dxa"/></w:tblCellMar></w:tblPr></w:style>',
  '<w:style w:type="table" w:styleId="AmanuensisTable"><w:name w:val="Amanuensis Table"/>',
  '<w:basedOn w:val="TableNormal"/><w:pPr><w:spacing w:after="0" w:line="240" w:lineRule="auto"/></w:pPr>',
  '<w:rPr><w:sz w:val="18"/><w:szCs w:val="18"/></w:rPr><w:tblPr><w:tblBorders>',
  '<w:top w:val="single" w:sz="4" w:space="0" w:color="808080"/>',
  '<w:left w:val="single" w:sz="4" w:space="0" w:color="808080"/>',
  '<w:bottom w:val="single" w:sz="4" w:space="0" w:color="808080"/>',
  '<w:right w:val="single" w:sz="4" w:space="0" w:color="808080"/>',
  '<w:insideH w:val="single" w:sz="4" w:space="0" w:color="808080"/>',
  '<w:insideV w:val="single" w:sz="4" w:space="0" w:color="808080"/>',
  '</w:tblBorders></w:tblPr>',
  '<w:tblStylePr w:type="firstRow"><w:rPr><w:b/></w:rPr>',
  '<w:tcPr><w:shd w:val="clear" w:color="auto" w:fill="E7E6E6"/></w:tcPr></w:tblStylePr>',
  '</w:style></w:styles>')
