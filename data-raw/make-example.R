# data-raw/make-example.R
#
# Builds the example project shipped in inst/extdata/example/: a synthetic,
# scanned-looking three-page "Padro municipal d'habitants" (municipal register
# of inhabitants) of Vilanova de la Riba, 1930, with the instructions, the
# codebook, a gold standard and the replies that model = "example" replays.
# example_project() copies these files into a folder of the user's.
#
# Judgment calls:
#
# - Everything is invented: the town, the people, the clerk and his
#   certificate. The names are ordinary Catalan names chosen for the hazards a
#   real register has: accents (F\u00e0bregas, Ant\u00f2nia), the middle dot of
#   l\u00b7l (Ul\u00b7ldemolins, Marcel\u00b7l\u00ed), ditto marks "id." for surnames and
#   occupations, an old spelling (Joseph), blank occupations for children, a
#   household that runs on from one sheet to the next, a double line between
#   two households, unused rows struck through, a handwritten certificate at
#   the foot of the last sheet, and one smudged cell.
# - The gold IS the generator's data: what is drawn on a page is exactly what
#   the gold says. The one exception is the smudged first name on sheet 2
#   (row num 19), which the gold records as a doubtful reading, "Teresa?", as
#   the transcription rules ask.
# - The replies are hand-made, NOT Gemini output: the gold with the planted
#   errors listed below, chosen to be the kinds of error real runs make. They
#   are written in the JSON a model returns under the schema that
#   codebook("codebook.xlsx") builds, and every reply is checked against that
#   schema before it is written.
# - Token counts are plausible rather than measured: prompt = 2,240 image
#   tokens (one ultra_high image) plus the instructions and schema text at
#   about four and a half characters per token, the same on every page as
#   with Google; output = the reply's JSON length / 3.5.
# - Pages are drawn at 200 dpi, the images() default, so images() renders
#   them one to one and prints no "the scans are about N dpi" note. They are
#   embedded as grayscale JPEGs by the small PDF writer below rather than by
#   magick, whose PDFs carry a temporary file name, a random ID and the clock;
#   the xlsx files get their creation stamp frozen for the same reason. With
#   the noise, specks and paper unevenness drawn from R's generator under
#   set.seed(1930), a rerun on the same machine rebuilds every file byte for
#   byte (the md5 of each is printed with "unchanged" or "CHANGED"); another
#   machine may draw with other fonts.
#
# Planted errors (model reply against the gold), 7 in all:
#
#   page  row  num  variable     gold           reply          kind
#   p001    6    6  edat         38             36             misread digit
#   p001   14   14  nom          Ant\u00f2nia        Antonia        dropped accent
#   p002    5   19  nom          Teresa?        Teresa         doubt mark dropped (smudged cell)
#   p002    6   20  (whole row)  Agust\u00ed ...     -              row skipped: rows 21-28 move up one
#   p003    1   29  nom          Joseph         Josep          spelling "corrected" (modernised)
#   p003    3   31  estat_civil  C              V              wrong category
#   p003    5   33  cognoms      id.            Mas Riera      ditto mark replaced by the surname
#
# So the replies have 14, 13 and 6 rows where the gold has 14, 14 and 6; the
# page variable `full` is right on every page. inst/extdata/example/README.md
# repeats this table.
#
# Usage, from the package folder:
#   Rscript data-raw/make-example.R
# Needs pkgload (to use the package's own codebook(), images() and validator),
# magick, pdftools, writexl, readxl, jsonlite and zip.

# ------- setup ----------------------------------------------------------------

if (!isTRUE(l10n_info()[["UTF-8"]])) invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
Sys.setenv(TZ = "UTC")    # zip entry times are local times
options(amanuensis.ask = FALSE)

.script_path <- function() {
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(arg)) return(normalizePath(sub("^--file=", "", arg[1])))
  NA_character_
}
ROOT <- if (!is.na(.script_path())) dirname(dirname(.script_path())) else normalizePath(".")
stopifnot(file.exists(file.path(ROOT, "DESCRIPTION")))
OUT <- file.path(ROOT, "inst", "extdata", "example")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
suppressMessages(pkgload::load_all(ROOT, quiet = TRUE, export_all = TRUE))

SEED <- 1930
MADE <- "2026-10-23"                      # frozen: no clock reads
STAMP <- "2026-10-23T00:00:00Z"
VERSION <- unname(read.dcf(file.path(ROOT, "DESCRIPTION"))[, "Version"])
STEM <- "padro1930"
FILES <- sprintf("%s_p%03d.jpg", STEM, 1:3)   # what images() will name the pages

md5_before <- tools::md5sum(list.files(OUT, full.names = TRUE))

# ------- the register ---------------------------------------------------------

# One line per person: sheet | num | cognoms | nom | edat | sexe | estat civil
# | professio | sap llegir. An empty professio is a blank cell (null).
# Sheet 2 carries on the Bertran household from sheet 1 (row 15), and a double
# line separates rows 21 and 22.
PEOPLE <- c(
  "1|1|Puig Ferrer|Josep|52|H|C|pag\u00e8s|S",
  "1|2|Soler Vidal|Maria|47|D|C|feines de casa|N",
  "1|3|Puig Soler|Joan|21|H|S|pag\u00e8s|S",
  "1|4|id.|Montserrat|17|D|S|teixidora|S",
  "1|5|id.|Marcel\u00b7l\u00ed|9|H|S|estudiant|S",
  "1|6|F\u00e0bregas Sol\u00e9|Ramon|38|H|C|fuster|S",
  "1|7|Roca Pujol|Rosalia|35|D|C|feines de casa|S",
  "1|8|F\u00e0bregas Roca|Pere|12|H|S|estudiant|S",
  "1|9|id.|Pilar|6|D|S||N",
  "1|10|Ul\u00b7ldemolins Casals|Jaume|64|H|V|jornaler|N",
  "1|11|Ul\u00b7ldemolins Mas|Avel\u00b7l\u00ed|29|H|S|id.|S",
  "1|12|Bertran Serra|Francesc|44|H|C|ferrer|S",
  "1|13|Grau Bosch|Dolors|41|D|C|feines de casa|S",
  "1|14|Bertran Grau|Ant\u00f2nia|15|D|S|minyona|S",
  "2|15|Bertran Grau|Llu\u00eds|11|H|S|estudiant|S",
  "2|16|Vilar\u00f3 Riera|Salvador|58|H|C|moliner|S",
  "2|17|Camps Font|Eul\u00e0lia|55|D|C|feines de casa|N",
  "2|18|Vilar\u00f3 Camps|Narc\u00eds|26|H|S|moliner|S",
  "2|19|id.|Teresa|23|D|S|sastressa|S",
  "2|20|id.|Agust\u00ed|19|H|S|mosso|S",
  "2|21|Font Rovira|Magdalena|81|D|V||N",
  "2|22|Sabat\u00e9 Mir\u00f3|Isidre|33|H|C|paleta|S",
  "2|23|Pla Ventura|Merc\u00e8|30|D|C|feines de casa|S",
  "2|24|Sabat\u00e9 Pla|Jordi|7|H|S|estudiant|S",
  "2|25|id.|N\u00faria|4|D|S||N",
  "2|26|id.|Marcel\u00b7la|1|D|S||N",
  "2|27|Rovira Fust\u00e9|Climent|49|H|V|rajoler|S",
  "2|28|Rovira Sala|Carme|20|D|S|teixidora|S",
  "3|29|Casals Bosch|Joseph|72|H|V|pag\u00e8s|N",
  "3|30|Mas Casals|Rafel|45|H|C|traginer|S",
  "3|31|Riera Puig|\u00c0ngela|43|D|C|feines de casa|S",
  "3|32|Mas Riera|Ernest|16|H|S|aprenent|S",
  "3|33|id.|Josefa|13|D|S|estudiant|S",
  "3|34|id.|Ignasi|10|H|S|id.|S"
)
parts <- strsplit(PEOPLE, "|", fixed = TRUE)
stopifnot(all(lengths(parts) == 9))
reg <- data.frame(
  sheet = as.integer(vapply(parts, `[`, "", 1)),
  num = as.integer(vapply(parts, `[`, "", 2)),
  cognoms = vapply(parts, `[`, "", 3),
  nom = vapply(parts, `[`, "", 4),
  edat = as.integer(vapply(parts, `[`, "", 5)),
  sexe = vapply(parts, `[`, "", 6),
  estat_civil = vapply(parts, `[`, "", 7),
  professio = vapply(parts, `[`, "", 8),
  sap_llegir = vapply(parts, `[`, "", 9) == "S",
  stringsAsFactors = FALSE
)
reg$professio[reg$professio == ""] <- NA
stopifnot(identical(reg$num, 1:34), identical(as.vector(table(reg$sheet)), c(14L, 14L, 6L)),
          all(reg$sexe %in% c("H", "D")), all(reg$estat_civil %in% c("S", "C", "V")))

SEPARATOR_AFTER <- 21L                    # double line between rows 21 and 22
SMUDGED <- 19L                            # first name of row 19 is smudged

# The gold: the register as drawn, except the smudged cell, which a careful
# transcriber records as doubtful.
gold <- reg
stopifnot(identical(gold$nom[gold$num == SMUDGED], "Teresa"))
gold$nom[gold$num == SMUDGED] <- "Teresa?"

# ------- planted errors -------------------------------------------------------

# Each change asserts the exact value it replaces, so an edit to PEOPLE that
# moves a row stops the script instead of planting the error somewhere else.
.plant <- function(tab, num, variable, from, to) {
  i <- which(tab$num == num)
  stopifnot(length(i) == 1, identical(tab[[variable]][i], from))
  tab[[variable]][i] <- to
  tab
}
model <- gold
model <- .plant(model, 6L, "edat", 38L, 36L)
model <- .plant(model, 14L, "nom", "Ant\u00f2nia", "Antonia")
model <- .plant(model, 19L, "nom", "Teresa?", "Teresa")
stopifnot(identical(model$nom[model$num == 20L], "Agust\u00ed"))
model <- model[model$num != 20L, ]
model <- .plant(model, 29L, "nom", "Joseph", "Josep")
model <- .plant(model, 31L, "estat_civil", "C", "V")
model <- .plant(model, 33L, "cognoms", "id.", "Mas Riera")
stopifnot(identical(as.vector(table(model$sheet)), c(14L, 13L, 6L)))

# ------- drawing the sheets ---------------------------------------------------

DPI <- 200
PAGE_W <- 11.69                           # A4 landscape, inches
PAGE_H <- 8.27
W_PX <- round(PAGE_W * DPI)
H_PX <- round(PAGE_H * DPI)
PAPER <- "#f3ecdb"

COLS <- data.frame(
  key = c("num", "cognoms", "nom", "edat", "sexe", "estat_civil", "professio", "sap_llegir"),
  label = c("N\u00fam.", "Cognoms", "Nom", "Edat", "Sexe", "Estat\ncivil", "Professi\u00f3", "Sap\nllegir"),
  width = c(0.65, 2.9, 1.75, 0.75, 0.75, 0.85, 2.2, 0.8),
  align = c("c", "l", "l", "c", "c", "c", "l", "c"),
  stringsAsFactors = FALSE
)
X0 <- 0.5
COL_X <- X0 + c(0, cumsum(COLS$width))   # column edges
X1 <- COL_X[length(COL_X)]
TABLE_TOP <- 1.3
HEAD_H <- 0.55
ROW_H <- 0.36
N_ROWS <- 14                              # ruled rows on every printed sheet
ROW_TOP <- TABLE_TOP + HEAD_H
TABLE_BOTTOM <- ROW_TOP + N_ROWS * ROW_H

# A script face for the clerk's certificate, if the machine has one.
.script_family <- function() {
  wanted <- c("Apple Chancery", "URW Chancery L", "Z003", "Monotype Corsiva", "Segoe Script")
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    have <- unique(systemfonts::system_fonts()$family)
    hit <- wanted[wanted %in% have]
    if (length(hit)) return(hit[1])
  }
  "serif"
}
SCRIPT <- .script_family()

# The magick graphics device ignores bold faces, so headings are struck twice.
.bold_text <- function(x, y, label, ...) {
  graphics::text(x, y, label, ...)
  graphics::text(x + 0.006, y, label, ...)
}

.ellipse <- function(cx, cy, rx, ry, col) {
  t <- seq(0, 2 * pi, length.out = 60)
  graphics::polygon(cx + rx * cos(t), cy + ry * sin(t), col = col, border = NA)
}

.cell_value <- function(row, key) {
  v <- row[[key]]
  if (is.na(v)) return("")
  if (key == "sap_llegir") return(if (v) "S" else "N")
  as.character(v)
}

.draw_sheet <- function(rows, sheet, last = FALSE) {
  img <- magick::image_graph(width = W_PX, height = H_PX, res = DPI, bg = PAPER, pointsize = 12)
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", family = "serif")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, PAGE_W), ylim = c(PAGE_H, 0))
  ink <- "#262320"

  # printed furniture
  .bold_text(PAGE_W / 2, 0.5, "PADR\u00d3 MUNICIPAL D'HABITANTS", cex = 1.9, col = ink)
  graphics::text(PAGE_W / 2, 0.9, "Ajuntament de Vilanova de la Riba \u2014 Any 1930", cex = 1.2, col = ink)
  graphics::text(X0, 0.5, "Districte 1r \u00b7 Secci\u00f3 2a", adj = c(0, 0.5), cex = 0.95, col = ink)
  .bold_text(X1, 0.5, paste("Full", sheet), adj = c(1, 0.5), cex = 1.4, col = ink)

  # the printed form: frame, column rules, heading, ruled rows
  graphics::rect(X0, TABLE_TOP, X1, TABLE_BOTTOM, lwd = 2, border = ink)
  graphics::segments(COL_X, TABLE_TOP, COL_X, TABLE_BOTTOM, lwd = 1.1, col = ink)
  graphics::segments(X0, ROW_TOP, X1, ROW_TOP, lwd = 2, col = ink)
  ys <- ROW_TOP + seq_len(N_ROWS - 1) * ROW_H
  graphics::segments(X0, ys, X1, ys, lwd = 0.6, col = "#77706a")
  mid <- (COL_X[-1] + COL_X[-length(COL_X)]) / 2
  graphics::text(mid, TABLE_TOP + HEAD_H / 2, COLS$label, cex = 0.95, col = ink)

  # the clerk's entries: a little irregular, as written with a pen
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, ]
    y <- ROW_TOP + (i - 0.5) * ROW_H + stats::runif(1, -0.015, 0.015)
    pen <- grDevices::grey(stats::runif(1, 0.06, 0.2))
    for (k in seq_len(nrow(COLS))) {
      label <- .cell_value(row, COLS$key[k])
      if (!nzchar(label)) next
      dx <- stats::runif(1, -0.02, 0.02)
      if (COLS$align[k] == "c") {
        graphics::text(mid[k] + dx, y, label, cex = 1.15, col = pen)
      } else {
        graphics::text(COL_X[k] + 0.1 + dx, y, label, adj = c(0, 0.5), cex = 1.15, col = pen)
      }
    }
    if (row$num == SEPARATOR_AFTER) {
      yb <- ROW_TOP + i * ROW_H
      graphics::segments(X0, yb - 0.022, X1, yb - 0.022, lwd = 1.8, col = ink)
      graphics::segments(X0, yb + 0.022, X1, yb + 0.022, lwd = 1.8, col = ink)
    }
    if (row$num == SMUDGED) {
      k <- which(COLS$key == "nom")
      for (j in 1:10) {
        .ellipse(COL_X[k] + 0.55 + stats::rnorm(1, 0, 0.1), y + stats::rnorm(1, 0, 0.04),
                 stats::runif(1, 0.18, 0.34), stats::runif(1, 0.07, 0.13),
                 grDevices::rgb(0.25, 0.23, 0.2, alpha = 0.09))
      }
    }
  }

  if (last) {
    # unused rows struck through, then the certificate in the clerk's hand
    y_free <- ROW_TOP + nrow(rows) * ROW_H
    graphics::segments(X0 + 0.4, y_free + 0.12, X1 - 0.4, TABLE_BOTTOM - 0.12, lwd = 1.4, col = ink)
    hand <- grDevices::grey(0.15)
    graphics::text(X0 + 0.3, 7.22, paste("Certifico que el present padr\u00f3 compr\u00e8n trenta-quatre",
                                         "habitants, inscrits en tres fulls."),
                   adj = c(0, 0.5), cex = 1.3, family = SCRIPT, col = hand, srt = 0.4)
    graphics::text(X0 + 0.3, 7.55, "Vilanova de la Riba, 31 de desembre de 1930.",
                   adj = c(0, 0.5), cex = 1.3, family = SCRIPT, col = hand, srt = 0.3)
    graphics::text(6.9, 7.84, "El Secretari,", adj = c(0, 0.5), cex = 1.3, family = SCRIPT, col = hand)
    graphics::text(8.45, 7.8, "J. Bonet", adj = c(0, 0.5), cex = 1.75, family = SCRIPT, col = hand)
    t <- seq(0, 1, length.out = 80)
    graphics::lines(8.4 + 1.5 * t, 8.03 + 0.04 * sin(t * 3 * pi) - 0.04 * t, lwd = 1.3, col = hand)
  }

  # dust and specks on the glass
  n <- 260
  graphics::points(stats::runif(n, 0, PAGE_W), stats::runif(n, 0, PAGE_H), pch = 16,
                   cex = stats::runif(n, 0.03, 0.14), col = grDevices::grey(0.3, alpha = 0.45))
  grDevices::dev.off()
  img
}

# Make a drawn sheet look scanned: grayscale, uneven paper, sensor noise, a
# slight blur and a slight tilt. All randomness comes from R's generator.
.age_sheet <- function(img, tilt) {
  a <- as.integer(magick::image_data(magick::image_convert(img, colorspace = "gray"), "gray"))
  h <- dim(a)[1]
  w <- dim(a)[2]
  coarse <- matrix(stats::rnorm(9 * 13, 0, 1), 9, 13)
  coarse <- magick::image_read(array((coarse - min(coarse)) / diff(range(coarse)), c(9, 13, 1)))
  field <- as.integer(magick::image_data(magick::image_resize(coarse, paste0(w, "x", h, "!")), "gray"))[, , 1]
  shade <- 1 - 0.05 * (field / 255)           # paper up to 5% darker in patches
  v <- a[, , 1] * shade + stats::rnorm(h * w, 0, 4.5)
  v <- pmin(255, pmax(0, v))
  out <- magick::image_read(array(v / 255, c(h, w, 1)))
  out <- magick::image_blur(out, radius = 2, sigma = 0.6)
  bg <- grDevices::grey(stats::median(v) / 255)
  out <- magick::image_rotate(magick::image_background(out, bg), tilt)
  out <- magick::image_crop(out, paste0(w, "x", h), gravity = "center")
  magick::image_strip(out)
}

.jpeg_bytes <- function(img, quality) {
  tmp <- tempfile(fileext = ".jpg")
  on.exit(unlink(tmp))
  magick::image_write(img, tmp, format = "jpeg", quality = quality)
  readBin(tmp, "raw", file.size(tmp))
}

# Width, height and number of colour components from a JPEG's frame header.
.jpeg_frame <- function(b) {
  i <- 3L
  while (i < length(b)) {
    stopifnot(b[i] == as.raw(0xff))
    marker <- as.integer(b[i + 1L])
    len <- as.integer(b[i + 2L]) * 256L + as.integer(b[i + 3L])
    if (marker %in% c(0xc0:0xc3, 0xc5:0xc7, 0xc9:0xcb, 0xcd:0xcf)) {
      return(list(height = as.integer(b[i + 5L]) * 256L + as.integer(b[i + 6L]),
                  width = as.integer(b[i + 7L]) * 256L + as.integer(b[i + 8L]),
                  components = as.integer(b[i + 9L])))
    }
    i <- i + 2L + len
  }
  stop("no JPEG frame header found")
}

# ------- the PDF --------------------------------------------------------------

# A minimal PDF 1.4: one page per JPEG, the JPEG bytes embedded as they are
# (DCTDecode), page size from the pixels at `dpi`. No dates, no random ID.
.write_pdf <- function(jpegs, path, title, dpi) {
  chunks <- list()
  offsets <- integer()
  pos <- 0L
  add <- function(x) {
    if (is.character(x)) x <- charToRaw(x)
    chunks[[length(chunks) + 1L]] <<- x
    pos <<- pos + length(x)
  }
  obj <- function(n, dict, stream = NULL) {
    offsets[n] <<- pos
    add(sprintf("%d 0 obj\n%s\n", n, dict))
    if (!is.null(stream)) {
      add("stream\n")
      add(stream)
      add("\nendstream\n")
    }
    add("endobj\n")
  }
  n_pages <- length(jpegs)
  page_obj <- 4L + 3L * (seq_len(n_pages) - 1L)
  title_hex <- paste(sprintf("%02X", as.integer(iconv(title, "UTF-8", "UTF-16BE", toRaw = TRUE)[[1]])), collapse = "")
  add("%PDF-1.4\n")
  add(as.raw(c(0x25, 0xe2, 0xe3, 0xcf, 0xd3, 0x0a)))
  obj(1L, "<< /Type /Catalog /Pages 2 0 R >>")
  obj(2L, sprintf("<< /Type /Pages /Kids [%s] /Count %d >>", paste(page_obj, "0 R", collapse = " "), n_pages))
  obj(3L, sprintf("<< /Title <FEFF%s> /Producer (amanuensis data-raw/make-example.R) >>", title_hex))
  for (i in seq_len(n_pages)) {
    f <- .jpeg_frame(jpegs[[i]])
    w_pt <- sprintf("%.2f", f$width / dpi * 72)
    h_pt <- sprintf("%.2f", f$height / dpi * 72)
    space <- if (f$components == 1L) "/DeviceGray" else "/DeviceRGB"
    obj(page_obj[i], sprintf("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 %s %s] /Resources << /XObject << /Im1 %d 0 R >> >> /Contents %d 0 R >>",
                             w_pt, h_pt, page_obj[i] + 1L, page_obj[i] + 2L))
    obj(page_obj[i] + 1L, sprintf("<< /Type /XObject /Subtype /Image /Width %d /Height %d /ColorSpace %s /BitsPerComponent 8 /Filter /DCTDecode /Length %d >>",
                                  f$width, f$height, space, length(jpegs[[i]])), jpegs[[i]])
    content <- sprintf("q %s 0 0 %s 0 0 cm /Im1 Do Q", w_pt, h_pt)
    obj(page_obj[i] + 2L, sprintf("<< /Length %d >>", nchar(content)), charToRaw(content))
  }
  xref <- pos
  n_obj <- length(offsets) + 1L
  add(sprintf("xref\n0 %d\n0000000000 65535 f \n", n_obj))
  add(paste0(sprintf("%010d 00000 n \n", offsets), collapse = ""))
  add(sprintf("trailer\n<< /Size %d /Root 1 0 R /Info 3 0 R >>\nstartxref\n%d\n%%%%EOF\n", n_obj, xref))
  .write_raw_atomic(do.call(c, chunks), path)
}

set.seed(SEED)
tilts <- c(0.35, -0.45, 0.25)              # degrees; a hand-fed scanner
jpegs <- lapply(1:3, function(s) {
  img <- .draw_sheet(reg[reg$sheet == s, ], s, last = s == 3)
  .jpeg_bytes(.age_sheet(img, tilts[s]), quality = 70)
})
pdf_path <- file.path(OUT, paste0(STEM, ".pdf"))
.write_pdf(jpegs, pdf_path, title = "Padr\u00f3 municipal d'habitants, Vilanova de la Riba, 1930 (synthetic example)",
           dpi = DPI)
stopifnot(file.size(pdf_path) < 1.5e6, pdftools::pdf_info(pdf_path)$pages == 3)

# ------- instructions ---------------------------------------------------------

# Role, Task and Details only: instructions() appends the standard Rules.
INSTRUCTIONS <- c(
  "# Role",
  "You are transcribing one page of the \"Padr\u00f3 municipal d'habitants\" (municipal register of inhabitants) of Vilanova de la Riba, 1930. Each page is one sheet (\"Full\") of a printed form filled in by the town clerk: a table with one row per person, grouped by household.",
  "",
  "# Task",
  "Return the sheet number printed at the top of the page and one record for each person in the table, in the order they are written, from top to bottom.",
  "",
  "# Details",
  "- The table has eight columns: N\u00fam., Cognoms, Nom, Edat, Sexe, Estat civil, Professi\u00f3 and Sap llegir. Read each row across all eight columns before moving to the next row, and do not skip rows.",
  "- The printed furniture is not data: the title, the town and year, the district line and the column headings. Only the sheet number (\"Full 2\") is wanted from the top of the page.",
  "- A double line between two rows separates two households; it is not a row. Rows struck through with a diagonal line are unused.",
  "- Within a household the clerk writes \"id.\" (\u00eddem) instead of repeating a surname or an occupation. Copy \"id.\" as it is written.",
  "- Keep the Catalan spelling exactly: accents (\u00e0, \u00e8, \u00e9, \u00ed, \u00f2, \u00f3, \u00fa) and the middle dot of \"l\u00b7l\" (Ul\u00b7ldemolins, Marcel\u00b7l\u00ed), and old spellings of names.",
  "- The handwritten certificate and signature at the foot of the last sheet are not records; ignore them.",
  ""
)
.write_text(INSTRUCTIONS, file.path(OUT, "instructions.md"))
instr <- instructions(file.path(OUT, "instructions.md"))

# ------- codebook -------------------------------------------------------------

CODEBOOK <- data.frame(
  variable = c("full", "num", "cognoms", "nom", "edat", "sexe", "estat_civil", "professio", "sap_llegir"),
  type = c("text", "integer", "text", "text", "integer", "category", "category", "text", "yes/no"),
  level = c("page", rep("row", 8)),
  description = c(
    "The sheet number printed at the top right after the word 'Full', exactly as printed (e.g. 'Full 2' -> '2'). Read it, never compute it from the order of the pages. Empty if there is none.",
    "The row number printed in the first column ('N\u00fam.'), as printed (e.g. '17' -> 17). The numbering runs on from one sheet to the next; copy it, never renumber the rows or compute a number.",
    "The surnames in the column 'Cognoms', exactly as written, with accents and the middle dot of l\u00b7l (e.g. 'Ul\u00b7ldemolins Casals'). A ditto mark is copied as written (e.g. 'id.' -> 'id.'); never replace it with the surname above.",
    "The first name in the column 'Nom', exactly as written, keeping accents and old spellings (e.g. 'Joseph' -> 'Joseph', never 'Josep').",
    "The age in years in the column 'Edat', as written (e.g. '38' -> 38). Copy it; never estimate or compute an age.",
    "The letter in the column 'Sexe': H (home, man) or D (dona, woman). Copy the letter; never infer it from the first name.",
    "The letter in the column 'Estat civil': S (solter, single), C (casat, married) or V (vidu, widowed). Copy the letter; never infer it from the age or the household.",
    "The occupation in the column 'Professi\u00f3', exactly as written (e.g. 'feines de casa', 'pag\u00e8s'), ditto marks 'id.' included. Empty if the cell is blank; never fill in an occupation that is not written.",
    "The column 'Sap llegir' (can read): 'S' -> yes, 'N' -> no. Empty if the cell is blank; never infer it from the age or the occupation."
  ),
  values = c("", "", "", "", "", "H; D", "S; C; V", "", ""),
  stringsAsFactors = FALSE
)
HOW_TO <- data.frame(
  column = c("variable", "type", "level", "description", "values"),
  what_to_write = c(
    "A short name for the variable, without spaces (use _ instead). It becomes a column of the results.",
    "text (copied exactly as written), integer (a whole number), category (one letter or word from the list in 'values'), or yes/no.",
    "page: one value for the whole page (here the sheet number). row: one value per person listed in the table.",
    "What the model should write: what to copy, an example as printed -> what to return, and what must never happen. The model reads these descriptions as part of its instructions.",
    "Only for category: the allowed values, separated by ;. The package adds a catch-all 'other' so the model is never forced to pick a wrong value."
  ),
  stringsAsFactors = FALSE
)

# libxlsxwriter stamps docProps/core.xml with the clock; rewrite that one file
# with a frozen stamp and re-zip with fixed entry times, keeping the order.
.freeze_xlsx <- function(path) {
  dir <- tempfile("xlsx-")
  on.exit(unlink(dir, recursive = TRUE))
  entries <- utils::unzip(path, list = TRUE)$Name
  utils::unzip(path, exdir = dir)
  core <- file.path(dir, "docProps", "core.xml")
  x <- readChar(core, file.size(core), useBytes = TRUE)
  x <- gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z", STAMP, x, useBytes = TRUE)
  writeChar(x, core, eos = NULL, useBytes = TRUE)
  Sys.setFileTime(file.path(dir, entries), as.POSIXct(paste(MADE, "00:00:00"), tz = "UTC"))
  target <- normalizePath(path, mustWork = FALSE)
  unlink(target)
  zip::zip(target, files = entries, root = dir, mode = "mirror", include_directories = FALSE)
  invisible(path)
}

.write_xlsx <- function(sheets, path) {
  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sheets, tmp)
  .freeze_xlsx(tmp)
  .write_raw_atomic(readBin(tmp, "raw", file.size(tmp)), path)
  unlink(tmp)
}

.write_xlsx(list(codebook = CODEBOOK, how_to = HOW_TO), file.path(OUT, "codebook.xlsx"))
.write_csv(CODEBOOK, file.path(OUT, "codebook.csv"), bom = TRUE)
cb <- suppressMessages(codebook(file.path(OUT, "codebook.xlsx")))
cb_csv <- suppressMessages(codebook(file.path(OUT, "codebook.csv")))
stopifnot(identical(cb$sha12, cb_csv$sha12), identical(cb$variables$variable, CODEBOOK$variable))

# ------- replies --------------------------------------------------------------

.page_json <- function(tab, sheet) {
  rows <- tab[tab$sheet == sheet, ]
  records <- lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    rec <- list(num = r$num, cognoms = r$cognoms, nom = r$nom, edat = r$edat, sexe = r$sexe,
                estat_civil = r$estat_civil)
    rec["professio"] <- list(if (is.na(r$professio)) NULL else r$professio)
    rec$sap_llegir <- r$sap_llegir
    rec
  })
  list(full = as.character(sheet), records = records)
}

schema_json <- .to_json(cb$schema)
prompt_tokens <- 2240L + as.integer(ceiling((nchar(instr$full) + nchar(schema_json)) / 4.5))
stopifnot(prompt_tokens >= 3300, prompt_tokens <= 3500)
replies <- list()
for (s in 1:3) {
  data <- .page_json(model, s)
  text <- .to_json(data)
  problems <- .validate(.parse_json(text), cb$schema)
  if (length(problems)) stop("reply for sheet ", s, " does not fit the schema: ", paste(problems, collapse = "; "))
  replies[[FILES[s]]] <- list(data = data,
                              tokens = list(prompt = prompt_tokens, output = as.integer(ceiling(nchar(text) / 3.5))))
}
.write_json(replies, file.path(OUT, "replies.json"), pretty = TRUE)

# ------- gold -----------------------------------------------------------------

set.seed(SEED)
drawn <- sample(FILES)                     # every page; the draw only fixes the order
bench <- do.call(rbind, lapply(1:3, function(s) {
  rows <- gold[gold$sheet == s, ]
  data.frame(file = FILES[s], row = seq_len(nrow(rows)),
             full = c(as.character(s), rep(NA, nrow(rows) - 1)),
             num = rows$num, cognoms = rows$cognoms, nom = rows$nom, edat = rows$edat,
             sexe = rows$sexe, estat_civil = rows$estat_civil, professio = rows$professio,
             sap_llegir = ifelse(rows$sap_llegir, "yes", "no"), stringsAsFactors = FALSE)
}))
gold_pages <- data.frame(
  file = FILES, done = "yes",
  notes = c("", sprintf("row %d (num %d): the first name is smudged; read as Teresa?",
                        which(gold$num[gold$sheet == 2] == SMUDGED), SMUDGED), ""),
  stringsAsFactors = FALSE
)
about <- data.frame(
  item = c("package", "made", "seed", "n", "method", "drawn", "rows", "prefilled", "codebook_source"),
  value = c(paste("amanuensis", VERSION), MADE, as.character(SEED), "3",
            paste("All 3 pages of the example, in the order given by sample() after set.seed(1930).",
                  "The document is synthetic: this transcription is the generator's own data",
                  "(data-raw/make-example.R), with the smudged first name on page 2 marked as doubtful."),
            paste(drawn, collapse = "; "), NA, "no", "codebook.xlsx"),
  stringsAsFactors = FALSE
)
gold_codebook <- data.frame(
  variable = cb$variables$variable, type = cb$variables$type, level = cb$variables$level,
  description = cb$variables$description,
  values = vapply(cb$variables$values, paste, "", collapse = "; "),
  stringsAsFactors = FALSE
)
gold_how <- data.frame(how_to = c(
  "This is the benchmark (gold standard) of the example project: a careful transcription of every page, made by hand.",
  "Sheet 'benchmark': one line per row of each page's table, in the order of the page. 'file' is the page image and 'row' counts the rows of that page from 1.",
  "The page variable 'full' (the printed sheet number) is filled on the first line of each page only.",
  "Write what is on the page, exactly as written: keep accents, 'l\u00b7l' and ditto marks ('id.'). Add '?' after a doubtful reading and leave a cell empty when it cannot be read or is blank.",
  "Sheet 'pages': only pages marked 'yes' in 'done' are used to score the model.",
  "Sheet 'about' records how the pages were chosen; sheet 'codebook' is the codebook the benchmark follows.",
  "In R: score(\"gold.xlsx\", res) compares this benchmark with the results of transcribe()."
), stringsAsFactors = FALSE)
.write_xlsx(list(benchmark = bench, pages = gold_pages, about = about, codebook = gold_codebook, how_to = gold_how),
            file.path(OUT, "gold.xlsx"))

# ------- checks with the package itself ---------------------------------------

check_dir <- tempfile("example-check-")
pages <- suppressMessages(images(pdf_path, save_to = file.path(check_dir, "pages")))
stopifnot(identical(pages$file, FILES), all(pages$status == "rendered"),
          all(pages$width == W_PX), all(pages$height == H_PX))
res <- suppressMessages(transcribe(pages, file.path(OUT, "instructions.md"), file.path(OUT, "codebook.xlsx"),
                                       model = "example", save_to = file.path(check_dir, "extraction")))
stopifnot(identical(as.vector(table(res$file)), c(14L, 13L, 6L)))
unlink(check_dir, recursive = TRUE)

# ------- report ---------------------------------------------------------------

made <- c(paste0(STEM, ".pdf"), "instructions.md", "codebook.xlsx", "codebook.csv", "replies.json", "gold.xlsx")
cat("\nexample project written to", OUT, "\n")
for (f in made) {
  p <- file.path(OUT, f)
  before <- md5_before[p]
  now <- unname(tools::md5sum(p))
  state <- if (is.na(before)) "new" else if (identical(unname(before), now)) "unchanged" else "CHANGED"
  cat(sprintf("  %-18s %9s bytes  md5 %s  %s\n", f, format(file.size(p), big.mark = ","), now, state))
}
cat(sprintf("\n  gold rows      %s\n  reply rows     %s\n  prompt tokens  %s per page\n  script font    %s\n",
            paste(table(gold$sheet), collapse = " / "), paste(table(model$sheet), collapse = " / "),
            format(prompt_tokens, big.mark = ","), SCRIPT))
