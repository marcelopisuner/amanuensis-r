# The instructions (prompt) sent with every page. The prompt has four parts -
# Role, Task, Rules, Details - and the Rules part is the same for every source:
# transcribe exactly, "?" for doubt, null for illegible, never invent, only
# this image. So instructions() appends that standard block by default; a user
# writes only what is specific to the source.

.default_instructions <- paste(
  "# Role",
  "You are transcribing one image of a historical document for a research project.",
  "",
  "# Task",
  "Read the image and return what the response schema asks for, for this image only.",
  sep = "\n"
)

# The standard rules are part of the contract hashed into every result, so a
# published text is never edited: a new wording gets a new id and the old ids
# stay available (instructions(rules = "rules-1")), so a folder extracted
# under an older version of the package can be continued unchanged.
.rules_texts <- list(
  "rules-1" = paste(
    "# Rules",
    "- Transcribe exactly as written: keep the original spelling, abbreviations, capitalisation, accents and punctuation, including the document's own mistakes. Never correct, expand, modernise or translate.",
    "- Never compute anything: copy numbers, dates and page numbers as they are written, even when they look wrong.",
    "- If you are unsure of a reading in a text field, give your best reading followed by \"?\" (for example: Puig?). In number, yes/no and category fields give your best reading without a question mark.",
    "- If something cannot be read at all, or is not on the page, return null for that field; inside running text, write [...] for an illegible passage.",
    "- Never invent or guess anything that is not visible. Use only what is on THIS image; do not carry anything over from other pages.",
    "- Printed marks such as a dash, a ditto mark (\", \u201e, id., idem) or a blank are content: copy dashes and ditto marks as written and never replace them with the value they stand for.",
    "- Ignore library stamps, rulers, colour charts and slivers of a neighbouring page.",
    "- If the text contains a backslash, write it as \\\\ so the JSON stays valid.",
    sep = "\n"
  )
)
.rules_id <- "rules-1"
.standard_rules <- .rules_texts[[.rules_id]]

#' Write the instructions for the model
#'
#' The instructions tell the model what the source is and what to do with
#' each image. Write them in a text file (Word, plain text or Markdown) or
#' directly as text. Keep them to about one page: what the source is, what one
#' image shows, what to return, and anything unusual about how the source is
#' written (reading order, abbreviations, what to ignore). What each variable
#' means belongs in the codebook, not here.
#'
#' The standard transcription rules are added at the end
#' automatically (see `rules`), so you do not need to write them: transcribe
#' exactly as written, mark doubtful readings with `?`, leave illegible
#' things empty, never invent, use only what is on this image.
#'
#' @param file Path to a `.docx`, `.txt`, `.md` or `.tex` file with your
#'   instructions.
#' @param text Your instructions as text, instead of a file.
#' @param rules `TRUE` (default) to add the standard transcription rules at
#'   the end; `FALSE` to send exactly your text and nothing else.
#' @return An `amanuensis_instructions` object to pass to [transcribe()].
#'   Printing it shows exactly what will be sent.
#' @seealso [instructions_template()] for a file to start from.
#' @export
#' @examples
#' instructions(text = "These are pages of a 1930 electoral roll from Girona.
#' Each page lists voters in a table with columns for name, age and address.")
#'
#' ex <- example_project()
#' instructions(ex$instructions)
instructions <- function(file = NULL, text = NULL, rules = TRUE) {
  if (!is.null(file) && !is.null(text)) {
    .abort("Give either {.arg file} or {.arg text}, not both.", class = "amanuensis_error_input")
  }
  # instructions("Transcribe every row ...") : text given where a file was expected
  if (.is_string(file) && !file.exists(file) && !.ext(file) %in% c("txt", "md", "markdown", "tex", "text", "docx", "doc", "rtf") &&
      grepl("\\s", file)) {
    .inform(c("i" = "Using this text as the instructions (no file with that name exists)."))
    text <- file
    file <- NULL
  }
  rules_id <- if (isTRUE(rules)) .rules_id else if (isFALSE(rules)) NULL else {
    if (!.is_string(rules) || is.null(.rules_texts[[rules]])) {
      .abort("{.arg rules} must be TRUE, FALSE or one of {.val {names(.rules_texts)}}.", class = "amanuensis_error_input")
    }
    rules
  }
  if (!is.null(file)) {
    .check_string(file, arg = "file")
    body <- .read_instructions_file(file)
    source <- file
  } else if (!is.null(text)) {
    if (!is.character(text) || !length(text)) .abort("{.arg text} must be text.", class = "amanuensis_error_input")
    body <- gsub("\r\n?", "\n", paste(enc2utf8(text), collapse = "\n"))
    source <- "text"
  } else {
    body <- .default_instructions
    source <- "default"
  }
  body <- .nfc(sub("\\s+$", "", body))
  if (!nzchar(body)) .abort("The instructions are empty.", class = "amanuensis_error_input")
  full <- if (!is.null(rules_id)) paste0(body, "\n\n", .rules_texts[[rules_id]]) else body
  out <- structure(list(text = body, rules = !is.null(rules_id), rules_id = rules_id %||% "none",
                        full = full, source = source, sha12 = .sha12(full)),
                   class = "amanuensis_instructions")
  .check_instructions(out)
  out
}

.as_instructions <- function(x, call = rlang::caller_env()) {
  if (is.null(x)) {
    .inform(c("i" = "No instructions given: using the package's general transcription instructions ({.code instructions()})."))
    return(instructions())
  }
  if (inherits(x, "amanuensis_instructions")) return(x)
  if (.is_string(x)) {
    if (.ext(x) %in% c("txt", "md", "markdown", "tex", "text", "docx") && !file.exists(x)) {
      .abort(c("Cannot find the instructions file {.path {(.show_path(x))}}.",
               "i" = "To give the instructions as text, use {.code instructions(text = \"...\")}."),
             call = call, class = "amanuensis_error_input")
    }
    # errors read as coming from the user's call (transcribe), not from
    # instructions() called on their behalf
    return(tryCatch(instructions(x), amanuensis_error = function(e) {
      e$call <- call
      stop(e)
    }))
  }
  .abort("{.arg instructions} must come from {.fn instructions}, or be a file path.", call = call,
         class = "amanuensis_error_input")
}

.read_instructions_file <- function(file, call = rlang::caller_env()) {
  if (!file.exists(file)) {
    .abort(c("Cannot find the instructions file {.path {(.show_path(file))}}.",
             "i" = "Check the spelling. Relative paths start from your working folder, {.path {(.show_path(getwd()))}}.",
             "i" = "To give the instructions as text, use {.code instructions(text = \"...\")}."),
           call = call, class = "amanuensis_error_input")
  }
  ext <- .ext(file)
  switch(ext,
    txt = , md = , markdown = , tex = , text = .read_text(file),
    docx = .read_docx_text(file),
    doc = .abort("Old Word .doc files cannot be read. Open it in Word and save it as .docx.", call = call),
    rtf = .abort("RTF files cannot be read. Save the file as .docx or plain text (.txt).", call = call),
    .abort(c("Instructions must be a .docx, .txt, .md or .tex file.",
             "x" = "{.file {basename(file)}} has the extension {.val {ext}}."), call = call)
  )
}

# Text of a .docx without extra packages: unzip word/document.xml (R's internal
# unzip needs no external program on any OS) and take the w:t runs of each
# paragraph, with tabs and line breaks. Verified identical to an xml2 reader
# on three documents. Deleted text (w:delText) and field codes (w:instrText)
# are not w:t, so they are left out.
.read_docx_text <- function(path, call = rlang::caller_env()) {
  td <- tempfile("docx")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  ok <- tryCatch({
    utils::unzip(path, files = "word/document.xml", exdir = td)
    TRUE
  }, warning = function(w) FALSE, error = function(e) FALSE)
  xml_path <- file.path(td, "word", "document.xml")
  if (!ok || !file.exists(xml_path)) {
    .abort("{.file {basename(path)}} could not be opened as a Word document.", call = call)
  }
  x <- rawToChar(readBin(xml_path, "raw", n = file.size(xml_path)))
  Encoding(x) <- "UTF-8"
  if (grepl("<w:numPr>", x, fixed = TRUE)) {
    .warn(c("{.file {basename(path)}} uses automatic numbering or bullets; the numbers and bullets themselves are not read.",
            "i" = "If they matter, type them as plain text."))
  }
  if (grepl("w:txbxContent", x, fixed = TRUE)) {
    .warn("{.file {basename(path)}} has text boxes; check that their text appears in the printed instructions.")
  }
  body <- sub("(?s).*?<w:body>", "", x, perl = TRUE)
  paras <- strsplit(body, "</w:p>", fixed = TRUE)[[1]]
  paras <- paras[-length(paras)]
  one <- function(p) {
    p <- gsub("<w:tab/>", "<w:t>\t</w:t>", p, fixed = TRUE)
    p <- gsub("<w:br/>|<w:br [^>]*/>|<w:cr/>", "<w:t>\n</w:t>", p)
    m <- regmatches(p, gregexpr("<w:t(?: [^>]*)?>[^<]*</w:t>", p, perl = TRUE))[[1]]
    .xml_unescape(paste(sub("^<w:t[^>]*>", "", sub("</w:t>$", "", m)), collapse = ""))
  }
  paste(vapply(paras, one, character(1), USE.NAMES = FALSE), collapse = "\n")
}

.xml_unescape <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  # numeric character references, e.g. &#233; or &#xE9;
  repl <- function(m, base) {
    vapply(m, function(s) intToUtf8(strtoi(gsub("[^0-9A-Fa-f]", "", sub("^&#x?", "", s)), base)), "")
  }
  for (pat in list(c("&#x[0-9A-Fa-f]+;", 16L), c("&#[0-9]+;", 10L))) {
    hits <- gregexpr(pat[1], x, perl = TRUE)
    regmatches(x, hits) <- lapply(regmatches(x, hits), repl, base = as.integer(pat[2]))
  }
  gsub("&amp;", "&", x, fixed = TRUE)
}

# An unfilled placeholder: text in angle brackets with a letter in it, up to
# 200 characters. The bound must clear every slot instructions_template()
# writes (the longest is 88; a bound of 60 once missed six of its seven, so a
# half-filled template went out with every paid page). The brackets must hug
# the text, so a comparison such as "ages < 20 and > 60" is not taken for a
# placeholder; short HTML-like tags (<b>, </i>) are left alone below.
.placeholder_re <- "<[^<>\\s][^<>\\n]{0,198}[^<>\\s]>"

.check_instructions <- function(x) {
  slots <- regmatches(x$text, gregexpr(.placeholder_re, x$text, perl = TRUE))[[1]]
  slots <- slots[grepl("[[:alpha:]]", slots) & !grepl("^</?[a-z]+>$", slots)]
  if (length(slots)) {
    .warn(c("The instructions still contain {length(slots)} placeholder{?s} to fill in, such as {.val {slots[1]}}.",
            "i" = "Replace the text in angle brackets with what is true of your source."))
  }
  words <- length(strsplit(x$text, "\\s+")[[1]])
  if (words > 1200) {
    .warn(c("The instructions are long ({words} words).",
            "i" = "About one page works best. Put what each variable means in the codebook descriptions instead."))
  }
  invisible(x)
}

#' @export
print.amanuensis_instructions <- function(x, ...) {
  src <- switch(x$source, text = "typed in R", default = "the package's default", basename(x$source))
  words <- length(strsplit(x$full, "\\s+")[[1]])
  cat("Instructions (", src, ", ", words, " words", if (x$rules) ", standard rules added" else "", ")\n", sep = "")
  cat(cli::rule(), "\n", sep = "")
  lines <- strsplit(x$full, "\n", fixed = TRUE)[[1]]
  show <- if (length(lines) > 30) c(lines[1:30], paste0("... (", length(lines) - 30, " more lines)")) else lines
  cat(show, sep = "\n")
  cat(cli::rule(), "\n", sep = "")
  if (!x$rules) cat("The standard transcription rules are NOT added (rules = FALSE).\n")
  invisible(x)
}

# The starter text of instructions_template(), kept as one constant so the
# tests can check that .check_instructions() finds every slot in it.
.template_lines <- c(
  "# Role",
  "You are transcribing <what the source is: title, place, date, archive and shelfmark>.",
  "Each image shows <one page / a two-page spread / one card> of this source.",
  "",
  "# Task",
  "For each image, return <what you want: for example, one record for every person listed on the page>, following the codebook.",
  "",
  "# Details",
  "- Reading order: <for example: two columns; read the left column from top to bottom, then the right column>.",
  "- Ignore: <for example: the printed page header, library stamps, the ruler at the bottom>.",
  "- Notation: <for example: \"id.\" means the same as the line above; transcribe \"id.\" as printed>.",
  "- <anything else the model should know about how this source is written>",
  "",
  "(The standard transcription rules - transcribe exactly, mark doubt with ?, leave illegible things empty,",
  "never invent - are added automatically by instructions(). Delete this note.)"
)

#' Write a starter file for the instructions
#'
#' Writes a short template with the four parts that good instructions have
#' (Role, Task, Details and the standard Rules), with the parts you need to
#' fill in marked by angle brackets, for example `<title and date of the
#' source>`. Open the file, replace the bracketed text, save it, and read it
#' with [instructions()].
#'
#' @param file Where to write the template: a `.docx`, `.md` or `.txt` file.
#' @param overwrite Replace the file if it already exists? Default `FALSE`.
#' @return The path, invisibly.
#' @export
#' @examples
#' path <- file.path(tempdir(), "my_instructions.docx")
#' instructions_template(path)
instructions_template <- function(file, overwrite = FALSE) {
  .check_string(file, arg = "file")
  if (file.exists(file) && !isTRUE(overwrite)) {
    .abort(c("{.file {file}} already exists.", "i" = "Choose another name, or use {.code overwrite = TRUE}."))
  }
  lines <- .template_lines
  ext <- .ext(file)
  if (ext == "docx") {
    .write_docx(lapply(lines, function(l) list(type = if (grepl("^# ", l)) "heading" else "para",
                                               text = sub("^# ", "", l))), file)
  } else if (ext %in% c("md", "txt", "markdown")) {
    .write_text(c(lines, ""), file)
  } else {
    .abort("The template must be a .docx, .md or .txt file.")
  }
  .inform(c("v" = "Wrote {.file {(.show_path(file))}}.",
            "i" = "Fill in the parts in <angle brackets>, save, then use {.code instructions(\"{basename(file)}\")}."))
  invisible(file)
}
