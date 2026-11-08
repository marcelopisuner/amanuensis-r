# The example project: example_project() copies the bundled files, the
# codebook builds, every prepared reply fits the codebook's schema, images()
# renders the PDF, and model = "example" reproduces exactly the planted errors
# listed in inst/extdata/example/README.md.

EXAMPLE_FILES <- c("padro1930.pdf", "instructions.md", "codebook.xlsx", "codebook.csv", "gold.xlsx")
EXAMPLE_PAGES <- sprintf("padro1930_p%03d.jpg", 1:3)

local_example <- function(env = parent.frame()) {
  local_amanuensis(env = env)
  dir <- file.path(withr::local_tempdir(.local_envir = env), "example")
  quietly(example_project(dir))
}

test_that("example_project() copies every file into a new folder and returns their paths", {
  local_amanuensis()
  dir <- file.path(withr::local_tempdir(), "new", "example")
  expect_message(ex <- example_project(dir), class = "amanuensis_message")
  expect_named(ex, c("folder", "scans", "instructions", "codebook", "codebook_csv", "gold"))
  expect_true(all(file.exists(unlist(ex))))
  expect_identical(ex$folder, normalizePath(dir, winslash = "/"))
  expect_setequal(list.files(dir), EXAMPLE_FILES)
  expect_identical(basename(ex$scans), "padro1930.pdf")
  installed <- system.file("extdata", "example", EXAMPLE_FILES, package = "amanuensis")
  expect_identical(unname(tools::md5sum(file.path(dir, EXAMPLE_FILES))), unname(tools::md5sum(installed)))
})

test_that("the message lists the steps to try", {
  local_amanuensis()
  msgs <- all_condition_text(example_project(file.path(withr::local_tempdir(), "ex")))
  text <- paste(msgs, collapse = "\n")
  for (step in c("images(", "transcribe(", "model = \"example\"", "output(", "benchmark(", "score(")) {
    expect_match(text, step, fixed = TRUE)
  }
})

test_that("files already in the folder are kept unless overwrite = TRUE", {
  local_amanuensis()
  dir <- withr::local_tempdir()
  quietly(example_project(dir))
  writeLines("my own codebook", file.path(dir, "codebook.csv"))
  msgs <- all_condition_text(ex <- example_project(dir))
  expect_identical(readLines(ex$codebook_csv), "my own codebook")
  expect_true(any(grepl("already in", msgs)))

  # a missing file is copied again, the others are kept
  unlink(ex$gold)
  msgs <- all_condition_text(example_project(dir))
  expect_true(file.exists(ex$gold))
  expect_identical(readLines(ex$codebook_csv), "my own codebook")
  expect_true(any(grepl("Kept 4 files", msgs)))

  quietly(example_project(dir, overwrite = TRUE))
  expect_identical(unname(tools::md5sum(ex$codebook_csv)),
                   unname(tools::md5sum(system.file("extdata", "example", "codebook.csv", package = "amanuensis"))))
})

test_that("example_project() refuses the package library, a file, and bad arguments", {
  local_amanuensis()
  expect_error(example_project(file.path(.libPaths()[1], "amanuensis-example")), class = "amanuensis_error_input",
               regexp = "library")
  f <- withr::local_tempfile()
  writeLines("x", f)
  expect_error(example_project(f), class = "amanuensis_error_input", regexp = "not a folder")
  expect_error(example_project(c("a", "b")), class = "amanuensis_error")
  expect_error(example_project(withr::local_tempdir(), overwrite = NA), class = "amanuensis_error_input")
})

test_that("the example codebook builds, from Excel and CSV alike", {
  ex <- local_example()
  cb <- quietly(codebook(ex$codebook))
  expect_identical(cb$kind, "table")
  expect_identical(cb$variables$variable,
                   c("full", "num", "cognoms", "nom", "edat", "sexe", "estat_civil", "professio", "sap_llegir"))
  expect_identical(cb$variables$level, c("page", rep("row", 8)))
  expect_identical(cb$variables$type,
                   c("text", "integer", "text", "text", "integer", "category", "category", "text", "boolean"))
  expect_identical(cb$variables$values[[6]], c("H", "D", "other"))
  expect_identical(cb$variables$values[[7]], c("S", "C", "V", "other"))
  expect_true(all(nzchar(cb$variables$description)))
  expect_identical(quietly(codebook(ex$codebook_csv))$sha12, cb$sha12)
  expect_setequal(readxl::excel_sheets(ex$codebook), c("codebook", "how_to"))
})

test_that("every prepared reply fits the example codebook's schema", {
  ex <- local_example()
  cb <- quietly(codebook(ex$codebook))
  replies <- .read_json(system.file("extdata", "example", "replies.json", package = "amanuensis"))
  expect_named(replies, EXAMPLE_PAGES)
  for (f in names(replies)) {
    expect_identical(.validate(replies[[f]]$data, cb$schema), character(), info = f)
    expect_true(replies[[f]]$tokens$prompt >= 3300 && replies[[f]]$tokens$prompt <= 3500, info = f)
    expect_true(replies[[f]]$tokens$output > 100, info = f)
  }
  expect_identical(unname(vapply(replies, function(r) length(r$data$records), integer(1))), c(14L, 13L, 6L))
  expect_identical(unname(vapply(replies, function(r) r$data$full, "")), c("1", "2", "3"))
})

test_that("the instructions read cleanly and the gold file has the benchmark layout", {
  ex <- local_example()
  expect_no_warning(ins <- instructions(ex$instructions))
  expect_match(ins$text, "^# Role")
  expect_match(ins$text, "Padr\u00f3 municipal d'habitants", fixed = TRUE)
  expect_true(ins$rules)

  expect_setequal(readxl::excel_sheets(ex$gold), c("benchmark", "pages", "about", "codebook", "how_to"))
  bench <- as.data.frame(readxl::read_excel(ex$gold, sheet = "benchmark", col_types = "text"))
  expect_identical(names(bench), c("file", "row", "full", "num", "cognoms", "nom", "edat", "sexe",
                                   "estat_civil", "professio", "sap_llegir"))
  expect_identical(as.vector(table(bench$file)), c(14L, 14L, 6L))
  expect_identical(bench$full[!is.na(bench$full)], c("1", "2", "3"))
  expect_true(all(bench$sap_llegir %in% c("yes", "no")))
  pages <- as.data.frame(readxl::read_excel(ex$gold, sheet = "pages"))
  expect_identical(pages$file, EXAMPLE_PAGES)
  expect_identical(pages$done, rep("yes", 3))
  about <- as.data.frame(readxl::read_excel(ex$gold, sheet = "about", col_types = "text"))
  expect_identical(about$value[about$item == "seed"], "1930")
  expect_setequal(strsplit(about$value[about$item == "drawn"], "; ", fixed = TRUE)[[1]], EXAMPLE_PAGES)
})

test_that("images() renders the example PDF as three pages with the expected names", {
  ex <- local_example()
  pages <- quietly(images(ex$scans, save_to = file.path(ex$folder, "pages")))
  expect_s3_class(pages, "amanuensis_images")
  expect_identical(pages$file, EXAMPLE_PAGES)
  expect_identical(pages$source_page, 1:3)
  expect_identical(pages$status, rep("rendered", 3))
  expect_true(all(file.exists(pages$image)))
  expect_true(all(pages$width > pages$height))        # landscape sheets
  expect_false(any(grepl("blank", pages$note)))
})

test_that("model = 'example' reproduces exactly the planted errors", {
  ex <- local_example()
  pages <- quietly(images(ex$scans, save_to = file.path(ex$folder, "pages")))
  res <- quietly(transcribe(pages, ex$instructions, ex$codebook, model = "example",
                                save_to = file.path(ex$folder, "extraction")))
  info <- .result_info(res)
  expect_identical(info$pages$status, rep("ok", 3))
  expect_identical(as.integer(info$pages$n_rows), c(14L, 13L, 6L))
  expect_identical(as.vector(table(res$file)), c(14L, 13L, 6L))
  expect_true(all(info$pages$input_tokens >= 3300 & info$pages$input_tokens <= 3500))
  expect_identical(as.vector(tapply(res$full, res$file, unique)), c("1", "2", "3"))

  # pair the gold and the reply by the printed row number, and list every difference
  gold <- as.data.frame(readxl::read_excel(ex$gold, sheet = "benchmark", col_types = "text"))
  vars <- c("cognoms", "nom", "edat", "sexe", "estat_civil", "professio", "sap_llegir")
  got <- res
  got$sap_llegir <- ifelse(got$sap_llegir, "yes", "no")
  diffs <- character()
  for (i in seq_len(nrow(gold))) {
    m <- got[got$file == gold$file[i] & got$num == as.integer(gold$num[i]), , drop = FALSE]
    if (!nrow(m)) {
      diffs <- c(diffs, paste(gold$file[i], gold$num[i], "missing"))
      next
    }
    for (v in vars) {
      a <- gold[[v]][i]
      b <- as.character(m[[v]])
      if (!identical(a, if (is.na(b)) NA_character_ else b)) {
        diffs <- c(diffs, paste(gold$file[i], gold$num[i], v, a, "->", b))
      }
    }
  }
  expect_setequal(diffs, c(
    "padro1930_p001.jpg 6 edat 38 -> 36",
    "padro1930_p001.jpg 14 nom Ant\u00f2nia -> Antonia",
    "padro1930_p002.jpg 19 nom Teresa? -> Teresa",
    "padro1930_p002.jpg 20 missing",
    "padro1930_p003.jpg 29 nom Joseph -> Josep",
    "padro1930_p003.jpg 31 estat_civil C -> V",
    "padro1930_p003.jpg 33 cognoms id. -> Mas Riera"
  ))
})

test_that("the example model refuses pages that are not the example's", {
  local_amanuensis()
  dir <- withr::local_tempdir()
  jpgs <- make_jpegs(file.path(dir, "pages"), n = 1)
  expect_error(quietly(transcribe(jpgs, codebook = codebook(free_text = TRUE), model = "example",
                                      save_to = file.path(dir, "out"))),
               class = "amanuensis_error_input", regexp = "example")
})

test_that("score() against the example gold finds the planted errors and only those", {
  ex <- local_example()
  pages <- quietly(images(ex$scans, save_to = file.path(ex$folder, "pages")))
  res <- quietly(transcribe(pages, ex$instructions, ex$codebook, model = "example",
                                save_to = file.path(ex$folder, "extraction")))
  s <- quietly(score(ex$gold, res))
  d <- s$differences
  missing_row <- grepl("missing", d$note)
  expect_identical(unique(d$file[missing_row]), "padro1930_p002.jpg")
  expect_identical(d$gold[missing_row & d$variable == "nom"], "Agust\u00ed")
  wrong <- d[!missing_row, ]
  expect_setequal(paste(wrong$file, wrong$variable, wrong$gold, wrong$output), c(
    "padro1930_p001.jpg edat 38 36",
    "padro1930_p001.jpg nom Ant\u00f2nia Antonia",
    "padro1930_p002.jpg nom Teresa? Teresa",
    "padro1930_p003.jpg nom Joseph Josep",
    "padro1930_p003.jpg estat_civil C V",
    "padro1930_p003.jpg cognoms id. Mas Riera"
  ))
})
