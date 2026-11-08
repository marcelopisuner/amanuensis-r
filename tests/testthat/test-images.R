# images(): every kind of source made on the fly with grDevices and magick.

local_images <- function(env = parent.frame()) {
  local_amanuensis(env)
  withr::local_tempdir(.local_envir = env)
}

# A JPEG with an EXIF APP1 segment that says "rotate 90 degrees" (orientation 6).
write_sideways_jpeg <- function(path, width = 40, height = 20) {
  tmp <- tempfile(fileext = ".jpg")
  magick::image_write(magick::image_blank(width, height, "red"), tmp, format = "jpeg")
  b <- readBin(tmp, "raw", file.size(tmp))
  app1 <- as.raw(c(0xff, 0xe1, 0x00, 0x22, charToRaw("Exif"), 0, 0, charToRaw("MM"), 0x00, 0x2a, 0, 0, 0, 8,
                   0, 1, 0x01, 0x12, 0, 3, 0, 0, 0, 1, 0, 6, 0, 0, 0, 0, 0, 0))
  writeBin(c(b[1:2], app1, b[-(1:2)]), path)
  path
}

test_that("a PDF is rendered page by page with numbered names and its source page", {
  dir <- local_images()
  pdf <- make_pdf(file.path(dir, "roll.pdf"), n = 3)
  msgs <- all_condition_text(pg <- images(pdf, save_to = file.path(dir, "pages")))
  expect_s3_class(pg, "amanuensis_images")
  expect_identical(pg$file, c("roll_p001.jpg", "roll_p002.jpg", "roll_p003.jpg"))
  expect_identical(pg$source, rep("roll.pdf", 3))
  expect_identical(pg$source_page, 1:3)
  expect_identical(pg$status, rep("rendered", 3))
  expect_identical(pg$width, rep(800L, 3))     # 4 in x 200 dpi
  expect_identical(pg$height, rep(1000L, 3))
  expect_true(all(file.exists(pg$image)))
  expect_true(all(vapply(pg$image, .jpeg_complete, logical(1))))
  expect_identical(attr(pg, "folder"), .show_path(file.path(dir, "pages")))
  expect_true(any(grepl("3 page images ready \\(rendered: 3\\)", msgs)))
  expect_output(print(pg), "Page images: 3 ready")
})

test_that("running again finds the pages already there, and redoes a truncated one", {
  dir <- local_images()
  pdf <- make_pdf(file.path(dir, "roll.pdf"), n = 3)
  out <- file.path(dir, "pages")
  pg <- quietly(images(pdf, save_to = out))
  again <- quietly(images(pdf, save_to = out))
  expect_identical(again$status, rep("already there", 3))
  expect_identical(again$width, pg$width)
  # an interrupted write leaves a JPEG without its end marker
  p2 <- pg$image[2]
  bytes <- readBin(p2, "raw", file.size(p2))
  writeBin(bytes[seq_len(length(bytes) %/% 2)], p2)
  expect_false(.jpeg_complete(p2))
  third <- quietly(images(pdf, save_to = out))
  expect_identical(third$status, c("already there", "rendered", "already there"))
  expect_true(.jpeg_complete(p2))
  over <- quietly(images(pdf, save_to = out, overwrite = TRUE))
  expect_identical(over$status, rep("rendered", 3))
})

test_that("pages = picks PDF pages; out-of-range pages are ignored with a warning", {
  dir <- local_images()
  pdf <- make_pdf(file.path(dir, "roll.pdf"), n = 3)
  pg <- quietly(images(pdf, save_to = file.path(dir, "pages"), pages = 2:3))
  expect_identical(pg$file, c("roll_p002.jpg", "roll_p003.jpg"))
  expect_identical(pg$source_page, 2:3)
  expect_warning(pg2 <- quietly(images(pdf, save_to = file.path(dir, "p2"), pages = c(1, 9))), "ignoring page number 9")
  expect_identical(pg2$file, "roll_p001.jpg")
  expect_error(images(pdf, pages = "1"), class = "amanuensis_error", regexp = "page numbers")
})

test_that("images.csv indexes the folder, and a later session recovers source and page from it", {
  dir <- local_images()
  pdf <- make_pdf(file.path(dir, "roll.pdf"), n = 2)
  out <- file.path(dir, "pages")
  quietly(images(pdf, save_to = out, dpi = 150))
  idx <- .read_image_index(out)
  expect_identical(idx$file, c("roll_p001.jpg", "roll_p002.jpg"))
  expect_identical(idx$source, c("roll.pdf", "roll.pdf"))
  expect_identical(idx$source_page, 1:2)
  expect_identical(idx$dpi, c("150", "150"))
  df <- .as_images(out)
  expect_identical(df$file, c("roll_p001.jpg", "roll_p002.jpg"))
  expect_identical(df$source, c("roll.pdf", "roll.pdf"))
  expect_identical(df$source_page, 1:2)
  expect_identical(attr(df, "folder"), .show_path(out))
  # images.csv itself is not reported as a skipped file
  msgs <- all_condition_text(images(out, save_to = file.path(dir, "again")))
  expect_false(any(grepl("Skipped", msgs)))
})

test_that("a multi-page TIFF is split frame by frame; a single-page TIFF is one image", {
  dir <- local_images()
  src <- file.path(dir, "src")
  dir.create(src)
  frames <- c(magick::image_blank(40, 30, "red"), magick::image_blank(40, 30, "blue"), magick::image_blank(40, 30, "green"))
  magick::image_write(frames, file.path(src, "box.tif"), format = "tiff")
  magick::image_write(magick::image_blank(50, 20, "white"), file.path(src, "single.tiff"), format = "tiff")
  pg <- quietly(images(src, save_to = file.path(dir, "pages")))
  expect_identical(pg$file, c("box_p001.jpg", "box_p002.jpg", "box_p003.jpg", "single.jpg"))
  expect_identical(pg$source, c("box.tif", "box.tif", "box.tif", "single.tiff"))
  expect_identical(pg$source_page, c(1L, 2L, 3L, 1L))
  expect_identical(pg$status, rep("converted", 4))
  # colours survive: frame 2 is blue
  px <- as.integer(magick::image_data(magick::image_read(pg$image[2]), "rgb"))
  expect_true(mean(px[, , 3]) > 200 && mean(px[, , 1]) < 50)
  sub <- quietly(images(file.path(src, "box.tif"), save_to = file.path(dir, "sub"), pages = 3))
  expect_identical(sub$file, "box_p003.jpg")
})

test_that("PNG and WEBP are converted to JPEG; HEIC too when magick can write one", {
  dir <- local_images()
  src <- file.path(dir, "src")
  dir.create(src)
  magick::image_write(magick::image_blank(30, 20, "orange"), file.path(src, "a.png"), format = "png")
  magick::image_write(magick::image_blank(30, 20, "purple"), file.path(src, "b.webp"), format = "webp")
  pg <- quietly(images(src, save_to = file.path(dir, "pages")))
  expect_identical(pg$file, c("a.jpg", "b.jpg"))
  expect_identical(pg$status, c("converted", "converted"))
  expect_identical(pg$width, c(30L, 30L))
  expect_true(all(vapply(pg$image, function(p) .jpeg_header(p)$jpeg, logical(1))))

  heic <- file.path(src, "photo.heic")
  ok <- tryCatch({
    magick::image_write(magick::image_blank(30, 20, "yellow"), heic, format = "heic")
    file.exists(heic) && file.size(heic) > 0
  }, error = function(e) FALSE)
  skip_if_not(ok, "this ImageMagick cannot write HEIC")
  h <- quietly(images(heic, save_to = file.path(dir, "heic")))
  expect_identical(h$file, "photo.jpg")
  expect_identical(h$status, "converted")
})

test_that("a JPEG stored sideways is turned upright; good JPEGs are copied byte for byte", {
  dir <- local_images()
  src <- file.path(dir, "src")
  dir.create(src)
  write_sideways_jpeg(file.path(src, "photo1.jpg"))
  good <- make_jpegs(src, n = 1, prefix = "scan")
  expect_identical(.jpeg_header(file.path(src, "photo1.jpg"))$orientation, 6L)
  expect_true(is.na(.jpeg_header(good)$orientation))
  msgs <- all_condition_text(pg <- images(src, save_to = file.path(dir, "pages")))
  expect_identical(pg$file, c("photo1.jpg", "scan001.jpg"))
  expect_identical(pg$status, c("converted", "copied"))
  expect_match(pg$note[1], "stored sideways")
  expect_identical(c(pg$width[1], pg$height[1]), c(20L, 40L))
  expect_identical(.md5(pg$image[2]), .md5(good))
  expect_true(any(grepl("1 page with a note", msgs)))
  # the upright copy has no rotation tag left to apply twice
  expect_true(.jpeg_header(pg$image[1])$orientation %in% c(NA, 1L))
  again <- quietly(images(src, save_to = file.path(dir, "pages")))
  expect_identical(again$status, c("already there", "already there"))
})

test_that("a folder of ready JPEGs is used in place: nothing copied, no index written", {
  dir <- local_images()
  src <- file.path(dir, "scans")
  make_jpegs(src, n = 2)
  msgs <- all_condition_text(pg <- images(src))
  expect_identical(pg$status, c("in place", "in place"))
  expect_identical(pg$image, .show_path(file.path(src, c("p001.jpg", "p002.jpg"))))
  expect_identical(attr(pg, "folder"), .show_path(src))
  expect_false(file.exists(file.path(src, "images.csv")))
  expect_false(dir.exists(file.path(src, "pages")))
  expect_true(any(grepl("ready to use as they are", msgs)))
  # a sideways photo among them gets an upright copy beside it
  write_sideways_jpeg(file.path(src, "p003.jpg"))
  pg2 <- quietly(images(src, save_to = src))
  expect_identical(pg2$status, c("in place", "in place", "converted"))
  expect_identical(pg2$file[3], "p003_upright.jpg")
})

test_that("a file named .jpg that is really a PNG is re-encoded", {
  dir <- local_images()
  src <- file.path(dir, "src")
  dir.create(src)
  magick::image_write(magick::image_blank(30, 20, "pink"), file.path(src, "fake.jpg"), format = "png")
  pg <- quietly(images(src, save_to = file.path(dir, "pages")))
  expect_identical(pg$status, "converted")
  expect_match(pg$note, "is really a PNG file")
  expect_true(.jpeg_header(pg$image)$jpeg)
})

test_that("two sources with one name get distinct page images (a.png + a.tif)", {
  dir <- local_images()
  src <- file.path(dir, "src")
  dir.create(src)
  magick::image_write(magick::image_blank(30, 20, "red"), file.path(src, "a.png"), format = "png")
  magick::image_write(magick::image_blank(30, 20, "blue"), file.path(src, "a.tif"), format = "tiff")
  magick::image_write(magick::image_blank(30, 20, "green"), file.path(src, "b.gif"), format = "gif")
  pg <- quietly(images(src, save_to = file.path(dir, "pages")))
  expect_setequal(pg$file, c("a_png.jpg", "a_tif.jpg", "b.jpg"))
  expect_identical(pg$source[pg$file == "a_tif.jpg"], "a.tif")
  dir.create(file.path(dir, "x"))
  magick::image_write(magick::image_blank(30, 20, "red"), file.path(dir, "x", "Page1.png"), format = "png")
  make_jpegs(file.path(dir, "y"), n = 1, prefix = "page")
  file.rename(file.path(dir, "y", "page001.jpg"), file.path(dir, "y", "page1.jpg"))
  pg2 <- quietly(images(file.path(dir, c("x/Page1.png", "y/page1.jpg")), save_to = file.path(dir, "out")))
  expect_identical(pg2$file, c("Page1_png.jpg", "page1.jpg"))
  expect_identical(pg2$source, c("Page1.png", "page1.jpg"))
})

# Two folders holding the same file names, with different pictures (widths).
same_name_folders <- function(dir, names = c("IMG_0001.JPG", "IMG_0002.JPG")) {
  for (k in 1:2) {
    d <- file.path(dir, paste0("visit", k))
    dir.create(d, recursive = TRUE)
    for (j in seq_along(names)) {
      magick::image_write(magick::image_blank(70 + 10 * k, 40, c("red", "blue")[j]), file.path(d, names[j]), format = "jpeg")
    }
  }
  file.path(dir, c("visit1", "visit2"))
}

test_that("JPEGs with the same name in two folders get one page image each (camera numbering restarts)", {
  dir <- local_images()
  src <- same_name_folders(dir)
  out <- file.path(dir, "pages")
  msgs <- all_condition_text(pg <- images(src, save_to = out))
  expect_identical(pg$file, c("IMG_0001.JPG", "IMG_0002.JPG", "visit2_IMG_0001.JPG", "visit2_IMG_0002.JPG"))
  expect_identical(pg$source, c("visit1/IMG_0001.JPG", "visit1/IMG_0002.JPG", "visit2/IMG_0001.JPG", "visit2/IMG_0002.JPG"))
  expect_identical(pg$status, rep("copied", 4))
  expect_identical(pg$width, c(80L, 80L, 90L, 90L))
  expect_match(pg$note[3], "named after its folder")
  # every file on disk is the picture its row says, byte for byte
  expect_identical(unname(.md5(pg$image)), unname(.md5(file.path(rep(src, each = 2), basename(c("IMG_0001.JPG", "IMG_0002.JPG"))))))
  expect_setequal(setdiff(list.files(out), "images.csv"), pg$file)
  idx <- .read_image_index(out)
  expect_identical(nrow(idx), 4L)
  expect_false(anyDuplicated(.page_key(idx$file)) > 0)
  expect_true(any(grepl("4 page images ready \\(copied: 4\\)", msgs)))
  # a rerun finds them all and makes no new names
  again <- quietly(images(src, save_to = out))
  expect_identical(again$file, pg$file)
  expect_identical(again$status, rep("already there", 4))
  expect_identical(length(list.files(out)), 5L)
  # and every page is extracted, from the result or from the folder
  res <- quietly(transcribe(pg, model = "mock", codebook = codebook(free_text = TRUE), save_to = file.path(dir, "res")))
  expect_identical(nrow(.result_info(res)$pages), 4L)
  res2 <- quietly(transcribe(out, model = "mock", codebook = codebook(free_text = TRUE), save_to = file.path(dir, "res2")))
  expect_identical(.result_info(res2)$pages$source, pg$source)
})

test_that("PNGs and PDFs with the same name in two folders are all converted and rendered", {
  dir <- local_images()
  for (k in 1:2) {
    d <- file.path(dir, paste0("v", k))
    dir.create(d)
    magick::image_write(magick::image_blank(70 + 10 * k, 20, "orange"), file.path(d, "IMG_0001.png"), format = "png")
    make_pdf(file.path(d, "padro.pdf"), n = k + 1)
  }
  png <- quietly(images(file.path(dir, c("v1/IMG_0001.png", "v2/IMG_0001.png")), save_to = file.path(dir, "png")))
  expect_identical(png$file, c("IMG_0001.jpg", "v2_IMG_0001.jpg"))
  expect_identical(png$status, c("converted", "converted"))
  expect_identical(png$width, c(80L, 90L))
  expect_identical(png$source, c("v1/IMG_0001.png", "v2/IMG_0001.png"))
  pdf <- quietly(images(file.path(dir, c("v1/padro.pdf", "v2/padro.pdf")), save_to = file.path(dir, "pdf"), dpi = 50))
  expect_identical(pdf$file, c("padro_p001.jpg", "padro_p002.jpg", "v2_padro_p001.jpg", "v2_padro_p002.jpg", "v2_padro_p003.jpg"))
  expect_identical(pdf$status, rep("rendered", 5))
  expect_identical(pdf$source, c(rep("v1/padro.pdf", 2), rep("v2/padro.pdf", 3)))
  expect_identical(pdf$source_page, c(1:2, 1:3))
  again <- quietly(images(file.path(dir, c("v1/padro.pdf", "v2/padro.pdf")), save_to = file.path(dir, "pdf"), dpi = 50))
  expect_identical(again$file, pdf$file)
  expect_identical(again$status, rep("already there", 5))
})

test_that("a page made from another file with the same name is never taken or replaced in a later call", {
  dir <- local_images()
  src <- same_name_folders(dir)
  out <- file.path(dir, "pages")
  first <- quietly(images(src[1], save_to = out))
  before <- .md5(first$image)
  second <- quietly(images(src[2], save_to = out))
  expect_identical(second$file, c("visit2_IMG_0001.JPG", "visit2_IMG_0002.JPG"))
  expect_identical(second$status, c("copied", "copied"))
  expect_identical(second$width, c(90L, 90L))
  expect_identical(.md5(first$image), before)
  # rerunning either call resumes
  expect_identical(quietly(images(src[2], save_to = out))$status, rep("already there", 2))
  expect_identical(quietly(images(src[1], save_to = out))$file, first$file)
  # PDFs: the index records the size of the original
  for (k in 1:2) {
    dir.create(file.path(dir, paste0("y", k)))
    make_pdf(file.path(dir, paste0("y", k), "padro.pdf"), n = k + 1)
  }
  a <- quietly(images(file.path(dir, "y1", "padro.pdf"), save_to = file.path(dir, "pdf"), dpi = 50))
  b <- quietly(images(file.path(dir, "y2", "padro.pdf"), save_to = file.path(dir, "pdf"), dpi = 50))
  expect_identical(b$file, c("y2_padro_p001.jpg", "y2_padro_p002.jpg", "y2_padro_p003.jpg"))
  expect_identical(b$status, rep("rendered", 3))
  expect_true(all(c("source_bytes", "dpi") %in% names(.read_image_index(file.path(dir, "pdf")))))
})

test_that("an original in save_to is never overwritten by another source with its name", {
  dir <- local_images()
  src <- same_name_folders(dir)
  originals <- .md5(file.path(src[1], c("IMG_0001.JPG", "IMG_0002.JPG")))
  pg <- quietly(images(rev(src), save_to = src[1]))
  expect_identical(.md5(file.path(src[1], c("IMG_0001.JPG", "IMG_0002.JPG"))), originals)
  expect_identical(pg$status, c("copied", "copied", "in place", "in place"))
  expect_identical(pg$file, c("visit2_IMG_0001.JPG", "visit2_IMG_0002.JPG", "IMG_0001.JPG", "IMG_0002.JPG"))
})

test_that("hand-made page images that share a name are refused, showing where each one is", {
  dir <- local_images()
  a <- make_jpegs(file.path(dir, "box1"), n = 1, prefix = "IMG_")
  b <- make_jpegs(file.path(dir, "box2"), n = 1, prefix = "img_", width = 70)
  err <- expect_error(.as_images(c(a, b)), class = "amanuensis_error_input", regexp = "share one results file")
  expect_match(conditionMessage(err), "box1")
  expect_match(conditionMessage(err), "box2")
  expect_match(conditionMessage(err), "after its folder")
})

test_that("an older images.csv without source_bytes is still read and extended", {
  dir <- local_images()
  out <- file.path(dir, "pages")
  pdf <- make_pdf(file.path(dir, "roll.pdf"), n = 2)
  quietly(images(pdf, save_to = out, dpi = 50))
  idx <- .read_image_index(out)
  .write_csv(idx[, c("file", "source", "source_page", "dpi", "width", "height")], file.path(out, "images.csv"), bom = FALSE)
  again <- quietly(images(pdf, save_to = out, dpi = 50))
  expect_identical(again$status, rep("already there", 2))
  idx2 <- .read_image_index(out)
  expect_identical(idx2$file, c("roll_p001.jpg", "roll_p002.jpg"))
  expect_false(anyNA(idx2$source_bytes))
})

test_that("a damaged file is listed as failed, warned about, and never dropped", {
  dir <- local_images()
  src <- file.path(dir, "src")
  make_jpegs(src, n = 1)
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47, 1:60)), file.path(src, "broken.png"))
  writeLines("notes", file.path(src, "readme.txt"))
  writeLines("x", file.path(src, ".DS_Store"))
  msgs <- character()
  w <- NULL
  pg <- withCallingHandlers(images(src, save_to = file.path(dir, "pages")),
                            message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") },
                            warning = function(x) { w <<- conditionMessage(x); invokeRestart("muffleWarning") })
  expect_identical(pg$source, c("broken.png", "p001.jpg"))
  expect_identical(pg$status, c("failed", "copied"))
  expect_true(is.na(pg$image[1]))
  expect_match(pg$note[1], "^could not be read")
  expect_match(w, "1 file could not be prepared")
  expect_match(w, "broken.png")
  expect_true(any(grepl("Skipped 1 file that is not a PDF or an image: .readme.txt.", msgs)))
  expect_output(print(pg), "1 failed")
  # transcribe() leaves failed rows out
  expect_identical(.as_images(pg)$file, "p001.jpg")
})

test_that("a damaged PDF fails with a plain reason", {
  dir <- local_images()
  bad <- file.path(dir, "bad.pdf")
  writeLines("%PDF-1.4 this is not really a pdf", bad)
  pg <- suppressWarnings(quietly(images(bad, save_to = file.path(dir, "pages"))))
  expect_identical(pg$status, "failed")
  expect_match(pg$note, "damaged, password-protected, or not really a PDF")
})

test_that("inputs are checked: missing files, other formats, empty folders, odd settings", {
  dir <- local_images()
  expect_error(images(), class = "amanuensis_error_input", regexp = "where your scans are")
  expect_error(images(file.path(dir, "none.pdf")), class = "amanuensis_error_input", regexp = "Cannot find")
  doc <- file.path(dir, "a.docx")
  writeLines("x", doc)
  expect_error(images(doc), class = "amanuensis_error_input", regexp = "not a PDF or an image")
  empty <- file.path(dir, "empty")
  dir.create(empty)
  expect_error(images(empty), class = "amanuensis_error_input", regexp = "No PDF or image files")
  expect_error(images(doc, dpi = 10), class = "amanuensis_error", regexp = "at least 30")
  pdf <- make_pdf(file.path(dir, "r.pdf"), n = 1)
  expect_warning(quietly(images(pdf, dpi = 700, save_to = file.path(dir, "hi"))), "very high")
  expect_error(images(pdf, save_to = file.path(.libPaths()[1], "amanuensis-test")), class = "amanuensis_error_input",
               regexp = "package library")
})

test_that("the default folder sits next to the source", {
  dir <- local_images()
  pdf <- make_pdf(file.path(dir, "roll.pdf"), n = 1)
  pg <- quietly(images(pdf))
  expect_identical(attr(pg, "folder"), .show_path(file.path(dir, "roll_pages")))
  src <- file.path(dir, "src")
  dir.create(src)
  magick::image_write(magick::image_blank(30, 20, "red"), file.path(src, "a.png"), format = "png")
  pg2 <- quietly(images(src))
  expect_identical(attr(pg2, "folder"), .show_path(file.path(src, "pages")))
})

test_that("the native resolution of scans inside a PDF is detected", {
  dir <- local_images()
  f <- file.path(dir, "scan.pdf")
  grDevices::pdf(f, width = 4, height = 4)
  op <- graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(c(0, 1), c(0, 1), xaxs = "i", yaxs = "i")
  m <- matrix(grDevices::gray(rep(seq(0, 1, length.out = 600), 600)), 600, 600)
  graphics::rasterImage(grDevices::as.raster(m), 0, 0, 1, 1, interpolate = FALSE)
  grDevices::dev.off()
  expect_identical(.pdf_native_dpi(readBin(f, "raw", file.size(f))), 150)
  msgs <- all_condition_text(images(f, dpi = 300, save_to = file.path(dir, "pages")))
  expect_true(any(grepl("about 150 dpi; rendering at 300 dpi", msgs)))
  msgs <- all_condition_text(images(f, dpi = 150, save_to = file.path(dir, "pages150")))
  expect_false(any(grepl("about 150 dpi", msgs)))
  # a vector-only PDF has no embedded scans
  vec <- make_pdf(file.path(dir, "vector.pdf"), n = 1)
  expect_true(is.na(.pdf_native_dpi(readBin(vec, "raw", file.size(vec)))))
})

test_that("small image helpers", {
  expect_identical(.page_name("x/My book.pdf", 7, 12), "My book_p007.jpg")
  expect_identical(.page_name("x/b.pdf", 7, 1200), "b_p0007.jpg")
  expect_identical(.safe_stem(c("a:b?c", "CON", "name. ")), c("a_b_c", "CON_", "name"))
  expect_true(.bitmap_is_uniform(as.raw(rep(255, 1000))))
  expect_false(.bitmap_is_uniform(as.raw(rep(0:9, 100))))
  expect_identical(.exif_orientation(charToRaw("no exif here")), NA_integer_)
})
