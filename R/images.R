# images(): every source goes through here, even a folder of JPEGs, so that
# each page the model will see is located, checked and listed before any money
# is spent. The rule: rasterise only - no deskew, no contrast, no cropping.
# The only changes made to an image are those needed to send it: decode,
# apply the camera's rotation tag, convert to sRGB, encode as JPEG.
#
# PDFs are rendered with pdftools (poppler) from bytes read once; magick's own
# PDF path needs ghostscript and keeps every page in memory. pdftools 3.6.0
# renders JPEG-compressed scans as blank white pages (its bundled libjpeg is
# the wrong version), hence the version floor in DESCRIPTION and the blank
# check below. Multi-page TIFFs are read one frame at a time ("file.tif[3]"),
# never whole: a 30-frame archive TIFF is 560 MB once decoded.
#
# The folder of page images gets an index, images.csv (file, source,
# source_page, width, height, dpi, source_bytes), so a later session that
# passes only the folder still knows which original file and page each image
# came from. Page numbers are never parsed out of file names. Two sources
# with the same name (IMG_0001.JPG from two camera cards, padro.pdf in a
# folder per year) never share a page image: see .plan_outputs().

.image_exts <- c("pdf", "tif", "tiff", "heic", "heif", "png", "webp", "jp2", "j2k", "jpx",
                 "jpg", "jpeg", "bmp", "gif")
.send_exts <- c("jpg", "jpeg", "png", "webp", "heic", "heif")

# Google accepts at most 20 MB per request, and base64 inflates by 4/3.
.max_jpeg_bytes <- 14 * 1024^2
# Above this a page is rendered at a lower dpi: an A0 sheet at 600 dpi is
# 2 GB of pixels and crashes R, and Google downscales it anyway.
.max_render_pixels <- 60e6

#' Prepare page images
#'
#' Turns your sources into one JPEG image per page, ready for the model:
#' PDFs are rendered page by page, multi-page TIFFs split into pages, and
#' HEIC (iPhone), PNG, WEBP, JPEG 2000, BMP and GIF images converted. JPEGs
#' that are already fine are used as they are. Every file is checked, so a
#' damaged or misnamed file is reported now rather than halfway through a paid
#' run.
#'
#' Use it for every source, even a folder that already holds JPEGs: the
#' result lists every page with its original file and page number, which the
#' later steps carry into your results.
#'
#' Images are not altered beyond what is needed to send them: no cropping, no
#' straightening, no contrast changes. Photos are turned upright according to
#' the rotation recorded by the camera.
#'
#' Running the same line again only does what is missing.
#'
#' @section Choosing `dpi`:
#' `dpi` (dots per inch) only matters for PDFs. 200 suits most manuscripts
#' and ordinary print; use 250-300 for small or worn type. Higher is not
#' always better: Google reads each image within a fixed budget (see
#' `resolution` in [transcribe()]), and a PDF made from 150 dpi scans
#' gains nothing from being rendered at 300 (the function tells you when that
#' happens).
#'
#' @param source A PDF or image file, several files, or a folder (the files
#'   directly inside it are used, in natural order: `p2` before `p10`).
#' @param dpi Resolution for rendering PDF pages. Default 200.
#' @param save_to Folder for the page images. By default a new folder next to
#'   the source (`<folder>/pages` for a folder, `<name>_pages` for a single
#'   file); you are asked before it is created. A folder of JPEGs that need
#'   no conversion is used where it is, and nothing is copied.
#' @param pages For PDFs and multi-page TIFFs, which pages to prepare, e.g.
#'   `1:5` to try a few pages first. Default: all.
#' @param quality JPEG quality, 1-100. Default 85.
#' @param overwrite `FALSE` (default) keeps page images that already exist.
#'   Use `TRUE` after changing `dpi`.
#' @section Files with the same name:
#' Sources from different folders may share a name (camera numbering such as
#' `IMG_0001.JPG` restarts on every card; `1930/padro.pdf` and
#' `1931/padro.pdf`). Every page still gets its own image: the first keeps its
#' name, a later one is named after its folder (`1931_padro_p001.jpg`), and
#' `source` then shows the folder too (`1931/padro.pdf`). A page image already
#' in `save_to` that was made from another file is never replaced.
#'
#' @return A data frame with one row per page image (class
#'   `amanuensis_images`): `image` (full path), `file` (its name), `source`
#'   (the original file), `source_page` (page number within the original
#'   file, counted from 1; this is not the page number printed on the page),
#'   `width` and `height` in pixels, `status` and `note`. Pass it to
#'   [transcribe()].
#' @export
#' @examples
#' ex <- example_project()
#' pages <- images(ex$scans, save_to = file.path(ex$folder, "pages"))
#' pages
images <- function(source, dpi = 200, save_to = NULL, pages = NULL, quality = 85, overwrite = FALSE) {
  if (missing(source) || !is.character(source) || !length(source)) {
    .abort(c("Tell {.fn images} where your scans are.",
             "i" = "For example {.code images(\"scans/book.pdf\")}, or {.code images(\"scans\")} for a folder."),
           class = "amanuensis_error_input")
  }
  .check_number(dpi, min = 30)
  .check_number(quality, min = 1)
  if (!is.null(pages) && (!is.numeric(pages) || anyNA(pages))) .abort("{.arg pages} must be page numbers, e.g. {.code 1:5}.")
  if (dpi > 600) .warn("dpi = {dpi} is very high: pages will be large and slow, and Google reduces them anyway. 200-300 is usually enough.")
  inputs <- .collect_inputs(source)

  in_place <- is.null(save_to) && all(inputs$kind == "jpeg") &&
    length(unique(dirname(inputs$path))) == 1 && .jpegs_ready(inputs$path)
  if (in_place) {
    save_to <- dirname(inputs$path[1])
  } else if (is.null(save_to)) {
    save_to <- if (length(source) == 1 && dir.exists(source)) file.path(source, "pages") else
      if (nrow(inputs) == 1) file.path(dirname(inputs$path[1]), paste0(.stem(inputs$path[1]), "_pages")) else
        file.path(dirname(inputs$path[1]), "pages")
    .confirm_location(save_to, "the page images", "save_to")
  }
  .check_string(save_to, arg = "save_to")
  .refuse_library_path(save_to)
  if (!dir.exists(save_to)) dir.create(save_to, recursive = TRUE)
  save_to <- .show_path(save_to)
  if (in_place) {
    .inform(c("i" = "The JPEGs in {.path {save_to}} are ready to use as they are; nothing is copied."))
  } else {
    .inform(c("i" = "Page images go in {.path {save_to}}."))
  }

  plan <- .plan_outputs(inputs, save_to)
  rows <- vector("list", nrow(inputs))
  pb <- cli::cli_progress_bar("Preparing pages", total = nrow(inputs), clear = TRUE)
  done_ok <- FALSE
  on.exit(if (!done_ok) cli::cli_progress_done(id = pb), add = TRUE)
  for (i in seq_len(nrow(inputs))) {
    path <- inputs$path[i]
    r <- tryCatch(
      switch(inputs$kind[i],
        pdf = .render_pdf(path, save_to, dpi, pages, quality, overwrite, stem = plan$stem[i]),
        tiff = .convert_tiff(path, save_to, plan$name[i], pages, quality, overwrite, stem = plan$stem[i]),
        jpeg = .take_jpeg(path, save_to, plan$name[i], quality, overwrite),
        .convert_image(path, save_to, plan$name[i], quality, overwrite)
      ),
      error = function(e) {
        if (inherits(e, "interrupt")) stop(e)
        .page_row(NA_character_, path, NA_integer_, status = "failed",
                  note = paste("could not be read:", conditionMessage(e)))
      }
    )
    if (!is.null(r) && nrow(r)) {
      r$source <- plan$source[i]
      if (nzchar(plan$note[i])) r$note <- ifelse(nzchar(r$note), paste(r$note, plan$note[i], sep = "; "), plan$note[i])
      r$source_bytes <- file.size(path)
    }
    rows[[i]] <- r
    cli::cli_progress_update(id = pb)
  }
  cli::cli_progress_done(id = pb)
  done_ok <- TRUE
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  if (!in_place) .write_image_index(out, save_to, dpi)
  out$source_bytes <- NULL
  attr(out, "folder") <- save_to
  attr(out, "dpi") <- dpi
  class(out) <- c("amanuensis_images", "data.frame")
  .report_images(out)
  invisible(out)
}

.refuse_library_path <- function(path, arg = "save_to", call = rlang::caller_env()) {
  p <- .show_path(path)
  libs <- .show_path(.libPaths())
  if (any(startsWith(p, paste0(libs, "/")))) {
    .abort(c("{.path {p}} is inside R's package library.",
             "i" = "Choose a folder of your own with {.arg {arg}}."), call = call, class = "amanuensis_error_input")
  }
}

# Expand folders, check existence and formats; returns data.frame(path, kind).
.collect_inputs <- function(source, call = rlang::caller_env()) {
  missing <- source[!file.exists(source)]
  if (length(missing)) {
    .abort(c("Cannot find {.path {(.show_path(missing))}}.",
             "i" = "Check the spelling. Relative paths start from your working folder, {.path {(.show_path(getwd()))}}.",
             "i" = "Use forward slashes {.val /} in paths, also on Windows."),
           call = call, class = "amanuensis_error_input")
  }
  paths <- character()
  skipped <- character()
  for (s in source) {
    if (dir.exists(s)) {
      files <- list.files(s, full.names = TRUE, no.. = TRUE)
      files <- files[!dir.exists(files) & !startsWith(basename(files), ".") & !startsWith(basename(files), "~$")]
      ok <- .ext(files) %in% .image_exts
      skipped <- c(skipped, basename(files[!ok & basename(files) != "images.csv"]))
      files <- files[ok]
      paths <- c(paths, files[.natural_order(basename(files))])
    } else {
      if (!.ext(s) %in% .image_exts) {
        .abort(c("{.file {basename(s)}} is not a PDF or an image file this package reads.",
                 "i" = "Accepted: PDF, TIFF, HEIC, PNG, WEBP, JPEG 2000, JPEG, BMP, GIF."),
               call = call, class = "amanuensis_error_input")
      }
      paths <- c(paths, s)
    }
  }
  if (length(skipped)) {
    .inform(c("i" = "Skipped {length(skipped)} file{?s} that {?is/are} not a PDF or an image: {.file {utils::head(skipped, 5)}}{if (length(skipped) > 5) ' ...' else ''}."))
  }
  if (!length(paths)) {
    .abort(c("No PDF or image files found in {.path {source}}.",
             "i" = "Accepted: PDF, TIFF, HEIC, PNG, WEBP, JPEG 2000, JPEG, BMP, GIF."),
           call = call, class = "amanuensis_error_input")
  }
  paths <- paths[!duplicated(normalizePath(paths))]
  ext <- .ext(paths)
  kind <- ifelse(ext == "pdf", "pdf", ifelse(ext %in% c("tif", "tiff"), "tiff",
                 ifelse(ext %in% c("jpg", "jpeg"), "jpeg", "other")))
  data.frame(path = paths, kind = kind, stringsAsFactors = FALSE)
}

# Cheap check of a set of JPEGs without decoding them: real JPEG bytes, small
# enough, upright according to EXIF.
.jpegs_ready <- function(paths) {
  all(vapply(paths, function(p) {
    info <- .jpeg_header(p)
    info$jpeg && info$bytes <= .max_jpeg_bytes && info$orientation %in% c(NA, 1L)
  }, logical(1)))
}

# Read the first 64 kB of a file: is it a JPEG, its size, and its EXIF
# orientation (parsed from the APP1 segment; NA when there is none).
.jpeg_header <- function(path) {
  size <- file.size(path)
  head <- readBin(path, "raw", n = min(size, 65536))
  jpeg <- length(head) > 3 && identical(head[1:3], as.raw(c(0xff, 0xd8, 0xff)))
  list(jpeg = jpeg, bytes = size, orientation = if (jpeg) .exif_orientation(head) else NA_integer_)
}

.exif_orientation <- function(b) {
  pos <- grepRaw(c(charToRaw("Exif"), as.raw(c(0, 0))), b, fixed = TRUE)
  if (!length(pos)) return(NA_integer_)
  tiff <- pos[1] + 6
  if (tiff + 8 > length(b)) return(NA_integer_)
  little <- identical(b[tiff:(tiff + 1)], charToRaw("II"))
  u16 <- function(at) {
    v <- as.integer(b[at:(at + 1)])
    if (little) v[1] + 256L * v[2] else 256L * v[1] + v[2]
  }
  u32 <- function(at) {
    v <- as.numeric(as.integer(b[at:(at + 3)]))
    if (little) sum(v * 256^(0:3)) else sum(v * 256^(3:0))
  }
  ifd <- tiff + u32(tiff + 4)
  if (ifd + 2 > length(b)) return(NA_integer_)
  n <- u16(ifd)
  for (k in seq_len(min(n, 200))) {
    entry <- ifd + 2 + (k - 1) * 12
    if (entry + 12 > length(b)) break
    if (u16(entry) == 0x0112L) return(as.integer(u16(entry + 8)))
  }
  NA_integer_
}

# A JPEG written by an earlier, interrupted run may be truncated: count it as
# done only if it starts with FFD8 and ends with FFD9.
.jpeg_complete <- function(path) {
  size <- file.size(path)
  if (is.na(size) || size < 4) return(FALSE)
  con <- file(path, "rb")
  on.exit(close(con))
  start <- readBin(con, "raw", n = 2)
  seek(con, size - 2)
  end <- readBin(con, "raw", n = 2)
  identical(start, as.raw(c(0xff, 0xd8))) && identical(end, as.raw(c(0xff, 0xd9)))
}

# Output names for every input, planned before anything is written. Page
# images are found, and their results files named, by file name ignoring
# capitals and the extension, so two sources must never share one. JPEGs keep
# their name; other images become <stem>.jpg, or <stem>_<ext>.jpg beside a
# file of the same stem (a.png and a.tif, Page1.png and page1.jpg); PDF and
# TIFF pages become <stem>_p001.jpg.
#
# Sources with the same name in different folders are normal: camera
# numbering (IMG_0001.JPG) restarts on every card, and archives keep one
# folder per volume with the same file names (1930/padro.pdf, 1931/padro.pdf).
# The first keeps its plain name, so pages and results made earlier stay
# valid; a later one gets its folder's name in front (1931_padro_p001.jpg),
# then a counter if that is taken too. The same inputs always get the same
# names, so a rerun resumes. A name already in the folder is taken when
# images.csv says it was made from another file (another name, or another
# size for the same name), or when it is another source's original: such a
# page is never reported "already there" nor overwritten. Pages without a
# line in images.csv (an interrupted run writes it only at the end) count
# as this source's, except a complete JPEG copy that differs from it.
# Returns data.frame(name, stem, source, note): the single-image name, the
# stem of PDF/TIFF page names, the source as recorded (folder/name when two
# sources share a name) and a note for renamed pages.
.plan_outputs <- function(inputs, folder) {
  paths <- inputs$path
  kind <- inputs$kind
  n <- length(paths)
  ext <- .ext(paths)
  stem <- .safe_stem(.stem(paths))
  norm <- .show_path(paths)
  in_folder <- dirname(norm) == .show_path(folder)
  ready <- rep(FALSE, n)
  ready[kind == "jpeg"] <- vapply(paths[kind == "jpeg"], .jpegs_ready, logical(1))
  in_place <- kind == "jpeg" & in_folder & ready
  single <- ifelse(kind == "jpeg", basename(paths), paste0(stem, ".jpg"))
  upright <- kind == "jpeg" & in_folder & !ready
  single[upright] <- paste0(stem[upright], "_upright.jpg")
  skey <- .page_key(.stem(single))
  for (i in which(kind %in% c("other", "tiff"))) {
    if (any(skey == skey[i] & ext != ext[i] & kind != "pdf")) single[i] <- paste0(stem[i], "_", ext[i], ".jpg")
  }
  multi <- kind %in% c("pdf", "tiff")
  has_single <- kind != "pdf"

  existing <- list.files(folder)
  existing <- existing[.ext(existing) %in% .image_exts & !startsWith(existing, ".")]
  ekey <- .page_key(.stem(existing))
  enorm <- .show_path(file.path(folder, existing))
  idx <- tryCatch(.read_image_index(folder), error = function(e) NULL)
  ikey <- if (is.null(idx)) character() else .page_key(.stem(idx$file))
  same_source <- function(rows, i) {
    bytes <- if ("source_bytes" %in% names(rows)) suppressWarnings(as.numeric(rows$source_bytes)) else NA_real_
    .page_key(basename(rows$source)) == .page_key(basename(paths[i])) &
      (is.na(bytes) | bytes == file.size(paths[i]))
  }
  taken_by_folder <- function(i, name, page_stem) {
    hit <- if (!is.na(name)) which(ekey == .page_key(.stem(name))) else integer()
    if (!is.na(page_stem)) hit <- c(hit, which(.is_page_of(ekey, .page_key(page_stem))))
    if (!length(hit)) return(FALSE)
    if (any(enorm[hit] %in% norm[-i])) return(TRUE)
    hit <- hit[enorm[hit] != norm[i]]
    if (!length(hit)) return(FALSE)
    if (!is.na(name) && any(ekey[hit] == .page_key(.stem(name)) & existing[hit] != name)) return(TRUE)
    rows <- c(if (!is.na(name)) which(ikey == .page_key(.stem(name))),
              if (!is.na(page_stem)) which(.is_page_of(ikey, .page_key(page_stem))))
    if (length(rows)) return(!all(same_source(idx[rows, , drop = FALSE], i)))
    if (kind[i] == "jpeg" && ready[i] && !is.na(name)) {
      f <- file.path(folder, name)
      return(file.exists(f) && .jpeg_complete(f) && !.same_bytes(paths[i], f))
    }
    FALSE
  }

  taken_single <- character()
  taken_multi <- character()
  clashes <- function(s, m) {
    (!is.na(s) && (s %in% taken_single || any(.is_page_of(s, taken_multi)))) ||
      (!is.na(m) && (m %in% taken_multi || any(.is_page_of(taken_single, m))))
  }
  prefix <- .safe_stem(basename(dirname(norm)))
  name <- single
  page_stem <- ifelse(multi, stem, NA_character_)
  renamed <- rep(FALSE, n)
  for (i in c(which(in_place), which(!in_place))) {
    s_stem <- .stem(single[i])
    s_ext <- tools::file_ext(single[i])
    for (k in 0:999) {
      base <- function(x) if (k == 0) x else if (k == 1) paste0(prefix[i], "_", x) else paste0(prefix[i], "_", x, "_", k)
      nm <- if (has_single[i]) paste0(base(s_stem), ".", s_ext) else NA_character_
      ps <- if (multi[i]) base(stem[i]) else NA_character_
      s_key <- if (has_single[i]) .page_key(.stem(nm)) else NA_character_
      m_key <- if (multi[i]) .page_key(ps) else NA_character_
      if (in_place[i] || (!clashes(s_key, m_key) && !taken_by_folder(i, nm, ps))) break
    }
    name[i] <- nm
    page_stem[i] <- ps
    renamed[i] <- k > 0
    if (!is.na(s_key)) taken_single <- c(taken_single, s_key)
    if (!is.na(m_key)) taken_multi <- c(taken_multi, m_key)
  }
  bkey <- .page_key(basename(paths))
  shared <- bkey %in% bkey[duplicated(bkey)] | renamed
  data.frame(name = name, stem = page_stem,
             source = ifelse(shared, paste0(basename(dirname(norm)), "/", basename(paths)), basename(paths)),
             note = ifelse(renamed, "named after its folder: another source has the same file name", ""),
             stringsAsFactors = FALSE)
}

# Is `name` (a stem key) one of the page names <stem>_p001 of `stem`?
.is_page_of <- function(name, stem) {
  if (!length(name) || !length(stem)) return(logical(0))
  pre <- paste0(stem, "_p")
  startsWith(name, pre) & grepl("^[0-9]+$", substring(name, nchar(pre) + 1L))
}

# Two files with the same bytes, judged by size and the first and last 64 kB
# (enough to tell two photographs apart without reading either whole).
.same_bytes <- function(a, b) {
  size <- file.size(a)
  if (is.na(size) || !identical(size, file.size(b))) return(FALSE)
  chunk <- function(p, from_end) {
    con <- file(p, "rb")
    on.exit(close(con))
    k <- min(size, 65536)
    if (from_end) seek(con, size - k)
    readBin(con, "raw", n = k)
  }
  identical(chunk(a, FALSE), chunk(b, FALSE)) && identical(chunk(a, TRUE), chunk(b, TRUE))
}

# Characters Windows cannot have in a file name, and its reserved names.
.safe_stem <- function(x) {
  x <- gsub("[<>:\"/\\\\|?*\u0001-\u001f]", "_", x)
  x <- sub("[. ]+$", "", x)
  reserved <- toupper(x) %in% c("CON", "PRN", "AUX", "NUL", paste0("COM", 1:9), paste0("LPT", 1:9))
  x[reserved] <- paste0(x[reserved], "_")
  x
}

.page_name <- function(path, i, n, stem = .safe_stem(.stem(path))) {
  width <- max(3, nchar(as.character(n)))
  paste0(stem, "_p", formatC(i, width = width, flag = "0"), ".jpg")
}

.page_row <- function(out_path, path, page, img = NULL, status, note = "") {
  w <- h <- NA_integer_
  if (!is.null(img)) {
    info <- magick::image_info(img)
    w <- as.integer(info$width[1]); h <- as.integer(info$height[1])
  } else if (!is.na(out_path) && file.exists(out_path)) {
    info <- tryCatch(magick::image_info(magick::image_read(out_path)), error = function(e) NULL)
    if (!is.null(info)) { w <- as.integer(info$width[1]); h <- as.integer(info$height[1]) }
  }
  data.frame(image = if (is.na(out_path)) NA_character_ else .show_path(out_path),
             file = if (is.na(out_path)) NA_character_ else basename(out_path),
             source = basename(path), source_page = as.integer(page), width = w, height = h,
             status = status, note = note, stringsAsFactors = FALSE)
}

.write_jpeg <- function(img, out_path, quality, dpi = NULL) {
  img <- magick::image_orient(img)
  img <- magick::image_convert(img, colorspace = "sRGB")
  write <- function(q) {
    tmp <- tempfile(pattern = ".amanuensis-", tmpdir = dirname(out_path), fileext = ".jpg")
    if (!is.null(dpi)) {
      magick::image_write(img, tmp, format = "jpeg", quality = q, density = paste0(dpi, "x", dpi))
    } else {
      magick::image_write(img, tmp, format = "jpeg", quality = q)
    }
    on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
    # replace in one step: the old image stays until the new one is in place
    for (try in 1:3) {
      if (.rename_file(tmp, out_path)) return(invisible())
      Sys.sleep(0.2)
    }
    if (!.copy_file(tmp, out_path)) .abort_unwritable(out_path)
  }
  write(quality)
  note <- ""
  if (file.size(out_path) > .max_jpeg_bytes) {
    write(70)
    note <- "saved at JPEG quality 70 to stay under Google's 20 MB request limit"
    if (file.size(out_path) > .max_jpeg_bytes) {
      img <- magick::image_resize(img, "50000000@>")
      write(70)
      note <- "reduced to 50 megapixels to stay under Google's 20 MB request limit"
    }
  }
  list(img = img, note = note)
}

.render_pdf <- function(path, folder, dpi, pages, quality, overwrite, stem = .safe_stem(.stem(path))) {
  raw <- readBin(path, "raw", n = file.size(path))
  n <- tryCatch(pdftools::pdf_info(raw)$pages, error = function(e) {
    stop("it is damaged, password-protected, or not really a PDF (", conditionMessage(e), ")", call. = FALSE)
  })
  todo <- if (is.null(pages)) seq_len(n) else as.integer(pages)
  out_of_range <- todo[todo < 1 | todo > n]
  if (length(out_of_range)) {
    # page numbers are numbers: pluralise on how many there are, not on their value
    .warn("{.file {basename(path)}} has {n} page{?s}; ignoring {cli::qty(length(out_of_range))}page number{?s} {out_of_range}.")
    todo <- todo[todo >= 1 & todo <= n]
  }
  native <- .pdf_native_dpi(raw)
  if (!is.na(native) && dpi > 1.1 * native) {
    .inform(c("i" = "The scans inside {.file {basename(path)}} are about {native} dpi; rendering at {dpi} dpi makes bigger files, not sharper pages ({.code dpi = {native}} would do)."))
  }
  sizes <- tryCatch(pdftools::pdf_pagesize(raw), error = function(e) NULL)
  rows <- vector("list", length(todo))
  blank_pages <- integer()
  for (j in seq_along(todo)) {
    i <- todo[j]
    out_path <- file.path(folder, .page_name(path, i, n, stem))
    if (file.exists(out_path) && !overwrite && .jpeg_complete(out_path)) {
      rows[[j]] <- .page_row(out_path, path, i, status = "already there")
      next
    }
    page_dpi <- dpi
    note <- ""
    if (!is.null(sizes) && i <= nrow(sizes)) {
      px <- (sizes$width[i] / 72 * dpi) * (sizes$height[i] / 72 * dpi)
      if (is.finite(px) && px > .max_render_pixels) {
        page_dpi <- floor(dpi * sqrt(.max_render_pixels / px))
        note <- paste0("very large page: rendered at ", page_dpi, " dpi instead of ", dpi)
      }
    }
    bm <- pdftools::pdf_render_page(raw, page = i, dpi = page_dpi, numeric = FALSE)
    if (.bitmap_is_uniform(bm)) blank_pages <- c(blank_pages, i)
    img <- magick::image_read(bm)
    rm(bm)
    w <- .write_jpeg(img, out_path, quality, page_dpi)
    note <- paste(c(note[nzchar(note)], w$note[nzchar(w$note)],
                    if (i %in% blank_pages) "completely blank"), collapse = "; ")
    rows[[j]] <- .page_row(out_path, path, i, img = w$img, status = "rendered", note = note)
    rm(img, w)
    if (j %% 10 == 0) invisible(gc(verbose = FALSE))
  }
  if (length(blank_pages)) {
    .warn(c("{length(blank_pages)} page{?s} of {.file {basename(path)}} came out completely blank ({cli::qty(length(blank_pages))}page{?s} {blank_pages}).",
            "i" = "If {cli::qty(length(blank_pages))}{?it is/they are} not blank in the PDF, update pdftools ({.code install.packages(\"pdftools\")}), restart R and run with {.code overwrite = TRUE}."))
  }
  do.call(rbind, rows)
}

# A rendered page whose sampled bytes take at most two values (white, or one
# colour plus the opaque alpha byte) is uniform; a real scan never is.
.bitmap_is_uniform <- function(bm) {
  n <- length(bm)
  idx <- seq.int(1, n, by = max(1L, n %/% 200000L))
  length(unique(as.integer(bm[idx]))) <= 2
}

# Best-effort resolution of the scans embedded in a PDF: image XObject
# dictionaries are plain text in the file, so find "/Subtype /Image" and read
# /Width and /Height nearby, then divide by the page size in inches. NA when
# unsure (encrypted files, inline images, vector PDFs). Never turns the whole
# PDF into a string.
.pdf_native_dpi <- function(raw) {
  tryCatch({
    hits <- grepRaw("/Subtype */Image", raw, all = TRUE)
    if (!length(hits)) return(NA_real_)
    hits <- utils::head(hits, 40)
    px <- vapply(hits, function(pos) {
      chunk <- raw[max(1, pos - 400):min(length(raw), pos + 400)]
      chunk[chunk == as.raw(0)] <- as.raw(32)
      s <- rawToChar(chunk)
      Encoding(s) <- "bytes"
      w <- regmatches(s, regexpr("/Width +[0-9]+", s, useBytes = TRUE))
      h <- regmatches(s, regexpr("/Height +[0-9]+", s, useBytes = TRUE))
      if (!length(w) || !length(h)) return(NA_real_)
      max(as.numeric(sub("\\D+", "", w)), as.numeric(sub("\\D+", "", h)))
    }, numeric(1))
    sizes <- pdftools::pdf_pagesize(raw)
    page_in <- stats::median(pmax(sizes$width, sizes$height)) / 72
    m <- stats::median(px, na.rm = TRUE)
    if (!is.finite(m) || !is.finite(page_in) || page_in <= 0) return(NA_real_)
    dpi <- m / page_in
    if (dpi < 50 || dpi > 1200) return(NA_real_)
    round(dpi / 10) * 10
  }, error = function(e) NA_real_)
}

# magick reads "file.tif[3]" as frame 3; on Windows a non-ASCII path is copied
# to an ASCII temporary name first, to be safe with the frame syntax.
.frame_path <- function(path, i) {
  paste0(path, "[", i, "]")
}

.ascii_copy <- function(path) {
  if (.Platform$OS.type != "windows" || !grepl("[^ -~]", path)) return(path)
  tmp <- tempfile(fileext = paste0(".", .ext(path)))
  file.copy(path, tmp)
  tmp
}

.convert_tiff <- function(path, folder, single_name, pages, quality, overwrite, stem = .safe_stem(.stem(path))) {
  src <- .ascii_copy(path)
  if (!identical(src, path)) on.exit(unlink(src), add = TRUE)
  multi <- length(magick::image_read(.frame_path(src, 1))) > 0
  if (!multi) return(.convert_image(path, folder, single_name, quality, overwrite))
  # count frames without decoding them all: stop at the first empty index
  n <- 1L
  while (length(magick::image_read(.frame_path(src, n))) > 0) n <- n + 1L
  todo <- if (is.null(pages)) seq_len(n) else intersect(as.integer(pages), seq_len(n))
  rows <- vector("list", length(todo))
  for (j in seq_along(todo)) {
    i <- todo[j]
    out_path <- file.path(folder, .page_name(path, i, n, stem))
    if (file.exists(out_path) && !overwrite && .jpeg_complete(out_path)) {
      rows[[j]] <- .page_row(out_path, path, i, status = "already there")
      next
    }
    img <- magick::image_read(.frame_path(src, i - 1L))
    w <- .write_jpeg(img, out_path, quality)
    rows[[j]] <- .page_row(out_path, path, i, img = w$img, status = "converted", note = w$note)
    rm(img, w)
    if (j %% 10 == 0) invisible(gc(verbose = FALSE))
  }
  do.call(rbind, rows)
}

.convert_image <- function(path, folder, name, quality, overwrite) {
  out_path <- file.path(folder, name)
  if (file.exists(out_path) && !overwrite && .jpeg_complete(out_path)) {
    return(.page_row(out_path, path, 1, status = "already there"))
  }
  img <- magick::image_read(readBin(path, "raw", n = file.size(path)))
  if (length(img) > 1) img <- img[1]
  w <- .write_jpeg(img, out_path, quality)
  .page_row(out_path, path, 1, img = w$img, status = "converted", note = w$note)
}

# A JPEG that is really a JPEG, upright, RGB or grey and small enough is sent
# as it is (copied byte for byte, or used in place); anything else is
# re-encoded. `name` comes from .plan_outputs(): <stem>_upright.jpg for a
# JPEG re-encoded beside its original, and never a page of another source,
# so replacing a file at that name only ever redoes this source's copy.
.take_jpeg <- function(path, folder, name, quality, overwrite) {
  in_place <- identical(.show_path(dirname(path)), .show_path(folder))
  out_path <- file.path(folder, name)
  hdr <- .jpeg_header(path)
  if (hdr$jpeg && hdr$bytes <= .max_jpeg_bytes && hdr$orientation %in% c(NA, 1L)) {
    if (in_place) return(.page_row(path, path, 1, status = "in place"))
    if (file.exists(out_path) && !overwrite && .same_bytes(path, out_path)) {
      return(.page_row(out_path, path, 1, status = "already there"))
    }
    file.copy(path, out_path, overwrite = TRUE)
    return(.page_row(out_path, path, 1, status = "copied"))
  }
  fixed <- out_path
  if (file.exists(fixed) && !overwrite && .jpeg_complete(fixed)) {
    return(.page_row(fixed, path, 1, status = "already there"))
  }
  img <- magick::image_read(readBin(path, "raw", n = hdr$bytes))
  if (length(img) > 1) img <- img[1]
  info <- magick::image_info(img)
  why <- c(if (!hdr$jpeg) paste0("is really a ", info$format[1], " file"),
           if (!hdr$orientation %in% c(NA, 1L)) "was stored sideways (camera rotation tag)",
           if (hdr$bytes > .max_jpeg_bytes) "was over 14 MB")
  w <- .write_jpeg(img, fixed, quality)
  .page_row(fixed, path, 1, img = w$img, status = "converted",
            note = paste(c(paste0("re-encoded: ", paste(why, collapse = ", ")), if (nzchar(w$note)) w$note), collapse = "; "))
}

# source_bytes (the size of the original file) tells two sources with the
# same name apart in a later session, so a page made from another file is
# never taken for this one's (see .plan_outputs()).
.write_image_index <- function(x, folder, dpi) {
  path <- file.path(folder, "images.csv")
  if (is.null(x$source_bytes)) x$source_bytes <- NA_real_
  new <- x[!is.na(x$image) & dirname(x$image) == .show_path(folder),
           c("file", "source", "source_page", "width", "height", "source_bytes"), drop = FALSE]
  if (!nrow(new)) return(invisible())
  new$dpi <- ifelse(grepl("\\.pdf$", new$source, ignore.case = TRUE), dpi, NA)
  new <- new[, c("file", "source", "source_page", "width", "height", "dpi", "source_bytes"), drop = FALSE]
  old <- tryCatch(.read_image_index(folder), error = function(e) NULL)
  if (!is.null(old)) {
    old <- old[!.page_key(old$file) %in% .page_key(new$file), , drop = FALSE]
    # an index written by an older version has no source_bytes column
    for (col in setdiff(names(new), names(old))) old[[col]] <- rep(NA, nrow(old))
    new <- rbind(old[, names(new)], new)
  }
  new <- new[.natural_order(new$file), , drop = FALSE]
  .write_csv(new, path, bom = FALSE)
  invisible(path)
}

.read_image_index <- function(folder) {
  path <- file.path(folder, "images.csv")
  if (!file.exists(path)) return(NULL)
  idx <- .read_delim_text(path)
  idx$source_page <- as.integer(idx$source_page)
  idx
}

.report_images <- function(x) {
  ok <- x$status != "failed"
  levels <- c("rendered", "converted", "copied", "in place", "already there")
  counts <- table(factor(x$status[ok], levels = levels))
  counts <- counts[counts > 0]
  .inform(c("v" = "{sum(ok)} page image{?s} ready ({paste(names(counts), counts, sep = ': ', collapse = ', ')})."))
  if (any(ok)) {
    mp <- x$width * x$height / 1e6
    if (any(!is.na(mp))) {
      big <- which.max(mp)
      .inform(c("i" = "Largest page: {x$width[big]} x {x$height[big]} pixels ({sprintf('%.1f', mp[big])} megapixels)."))
    }
  }
  if (any(!ok)) {
    .warn(c("{sum(!ok)} file{?s} could not be prepared and will be left out:",
            stats::setNames(.cli_escape(paste0(x$source[!ok], ": ", x$note[!ok])), rep("x", sum(!ok)))))
  }
  flagged <- which(ok & nzchar(x$note))
  if (length(flagged)) {
    .inform(c("!" = "{length(flagged)} page{?s} with a note (see the {.field note} column), e.g. {x$file[flagged[1]]}: {x$note[flagged[1]]}"))
  }
  invisible(x)
}

#' @export
print.amanuensis_images <- function(x, ...) {
  ok <- x$status != "failed"
  cat("Page images: ", sum(ok), " ready", if (any(!ok)) paste0(", ", sum(!ok), " failed"),
      " | folder: ", attr(x, "folder") %||% "-", "\n", sep = "")
  show <- as.data.frame(x)[, intersect(c("file", "source", "source_page", "width", "height", "status", "note"), names(x))]
  if ("note" %in% names(show)) show$note <- ifelse(nchar(show$note) > 40, paste0(substr(show$note, 1, 37), "..."), show$note)
  if (nrow(show) > 10) {
    print(utils::head(show, 10), row.names = FALSE, right = FALSE)
    cat("... and", nrow(show) - 10, "more pages\n")
  } else {
    print(show, row.names = FALSE, right = FALSE)
  }
  invisible(x)
}

#' @export
as.data.frame.amanuensis_images <- function(x, ...) {
  attr(x, "folder") <- NULL
  attr(x, "dpi") <- NULL
  class(x) <- "data.frame"
  x
}

# Accept images() output, a data frame with an `image` column, a folder of
# page images, or image paths; return a clean data frame of pages to send.
# Raw paths get the same cheap checks images() makes (format, size, rotation),
# without writing anything.
.as_images <- function(x, call = rlang::caller_env()) {
  folder <- NULL
  if (is.data.frame(x)) {
    if (!"image" %in% names(x)) {
      .abort("{.arg images} must come from {.fn images}, or be a folder or image files.", call = call,
             class = "amanuensis_error_input")
    }
    folder <- attr(x, "folder")
    df <- as.data.frame(x, stringsAsFactors = FALSE)
    keep <- !is.na(df$image)
    if (!is.null(df$status)) keep <- keep & df$status != "failed"
    df <- df[keep, , drop = FALSE]
    if (is.null(df$file)) df$file <- basename(df$image)
    if (is.null(df$source)) df$source <- df$file
    if (is.null(df$source_page)) df$source_page <- 1L
  } else if (is.character(x)) {
    paths <- character()
    for (s in x) {
      if (dir.exists(s)) {
        if (is.null(folder)) folder <- .show_path(s)
        f <- list.files(s, full.names = TRUE)
        f <- f[.ext(f) %in% .image_exts & !startsWith(basename(f), ".") & !dir.exists(f)]
        paths <- c(paths, f[.natural_order(basename(f))])
      } else if (file.exists(s)) {
        paths <- c(paths, s)
      } else {
        .abort(c("Cannot find {.path {(.show_path(s))}}.",
                 "i" = "Relative paths start from your working folder, {.path {(.show_path(getwd()))}}."),
               call = call, class = "amanuensis_error_input")
      }
    }
    if (!length(paths)) {
      .abort(c("No page images found in {.path {x}}.", "i" = "Run {.fn images} on your scans first."),
             call = call, class = "amanuensis_error_input")
    }
    bad <- paths[!.ext(paths) %in% .send_exts]
    if (length(bad)) {
      .abort(c("{.file {basename(bad)}} must be prepared first (Gemini reads JPEG, PNG, WEBP and HEIC).",
               "i" = "Run {.code pages <- images(\"{x[1]}\")} and pass {.code pages}."), call = call,
             class = "amanuensis_error_input")
    }
    jpg <- .ext(paths) %in% c("jpg", "jpeg")
    not_ready <- paths[jpg][!vapply(paths[jpg], .jpegs_ready, logical(1))]
    if (length(not_ready)) {
      .abort(c("{length(not_ready)} image{?s} need{?s/} preparing first (sideways, too large, or not really JPEG): {.file {basename(utils::head(not_ready, 3))}}.",
               "i" = "Run {.code pages <- images(\"{x[1]}\")} and pass {.code pages}."), call = call,
             class = "amanuensis_error_input")
    }
    df <- data.frame(image = .show_path(paths), file = basename(paths), source = basename(paths),
                     source_page = 1L, stringsAsFactors = FALSE)
    # recover the original file and page from images.csv when there is one
    for (d in unique(dirname(df$image))) {
      idx <- tryCatch(.read_image_index(d), error = function(e) NULL)
      if (is.null(idx)) next
      here <- dirname(df$image) == d
      m <- match(.page_key(df$file[here]), .page_key(idx$file))
      df$source[here][!is.na(m)] <- idx$source[m[!is.na(m)]]
      df$source_page[here][!is.na(m)] <- idx$source_page[m[!is.na(m)]]
    }
  } else {
    .abort("{.arg images} must come from {.fn images}, or be a folder or image files.", call = call,
           class = "amanuensis_error_input")
  }
  if (!nrow(df)) .abort("There are no page images to process.", call = call, class = "amanuensis_error_input")
  # the key results are saved under (.record_file uses the safe stem)
  key <- .page_key(.safe_stem(.stem(df$file)))
  dup <- key[duplicated(key)]
  if (length(dup)) {
    pair <- df$image[key == dup[1]]
    .abort(c("Two images would share one results file: {.path {pair}}.",
             "i" = "Results are saved by file name, ignoring capitals and the extension, so every page image needs its own name.",
             "i" = "Rename one of them, or prepare the pages again with {.fn images} (it names a page after its folder when another source has the same file name)."),
           call = call, class = "amanuensis_error_input")
  }
  missing <- df$image[!file.exists(df$image)]
  if (length(missing)) .abort("Page image{?s} not found: {.path {missing}}.", call = call, class = "amanuensis_error_input")
  attr(df, "folder") <- folder %||% .show_path(dirname(df$image[1]))
  df
}
