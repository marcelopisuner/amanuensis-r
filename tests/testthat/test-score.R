# score(): the planted-diff protocol. A copy of the benchmark
# serves as fake output, realistic errors are planted in it, and each must be
# caught in the right variable with counts that reconcile by hand.

# ------- fixtures -------------------------------------------------------------

score_cb <- function() {
  suppressMessages(codebook(data.frame(
    variable = c("printed_page", "surname", "name", "age", "civil", "occupation"),
    type = c("text", "text", "text", "integer", "category", "text"),
    values = c("", "", "", "", "single; married; widowed", ""),
    level = c("page", "row", "row", "row", "row", "row"),
    description = "As written.", stringsAsFactors = FALSE
  )))
}

# Three pages: 5, 4 and 2 rows. Row 2 of p002 has empty cells.
score_gold_tab <- function() {
  data.frame(
    file = c(rep("p001.jpg", 5), rep("p002.jpg", 4), rep("p003.jpg", 2)),
    row = c(1:5, 1:4, 1:2),
    printed_page = c("12", NA, NA, NA, NA, "13", NA, NA, NA, "14", NA),
    surname = c("Puig", "Ferr\u00e9", "Soler", "Vidal", "Roca", "Mas", "Font", "Serra", "Pons", "Bosch", "Camps"),
    name = c("Josep", "Maria", "Joan?", "Pere", "Anna", "Jaume", NA, "Rosa", "Llucia", "Marti", "Teresa"),
    age = c(34L, 29L, 41L, 18L, 22L, 50L, NA, 45L, 7L, 60L, 58L),
    civil = c("married", "married", "widowed", "single", "married", "married", NA, "widowed", "single",
              "married", "married"),
    occupation = c("pages", NA, "jornaler", "jornalr", "cosidora", "pag\u00e8s", NA, NA, NA, "ferrer", NA),
    stringsAsFactors = FALSE
  )
}

# The planted errors:
#   p001 row 1  age 34 -> 54                   misread digit
#   p001 row 2  surname Ferre (accent dropped)  1 edit
#   p001 row 3  name "Joan?" -> "Joan"          doubt mark dropped, 1 edit
#   p001 row 4  occupation "jornalr" -> "jornaler"   source typo "corrected", 1 edit
#   p001 row 5  civil married -> single         wrong category
#   p002 row 2  deleted (it has empty cells)
#   p003        printed_page 14 -> 15; a row inserted before Camps
score_output_tab <- function() {
  out <- score_gold_tab()
  out$age[1] <- 54L
  out$surname[2] <- "Ferre"
  out$name[3] <- "Joan"
  out$occupation[4] <- "jornaler"
  out$civil[5] <- "single"
  out <- out[-7, ]
  out$printed_page[out$file == "p003.jpg"][1] <- "15"
  ins <- out[out$file == "p003.jpg", ][1, ]
  ins$printed_page <- NA
  ins$surname <- "Riera"
  ins$name <- "Antoni"
  ins$age <- 3L
  ins$civil <- "single"
  ins$occupation <- NA
  out <- rbind(out[1:9, ], ins, out[10, ])
  out$row <- stats::ave(seq_len(nrow(out)), out$file, FUN = seq_along)
  rownames(out) <- NULL
  out
}

score_units_ok <- function(tab, cb, failed = character()) {
  u <- .units_from_table(tab, cb)
  for (f in names(u)) {
    u[[f]]$status <- if (f %in% failed) "failed" else "ok"
    if (f %in% failed) u[[f]] <- c(.page_unit(NULL, cb), status = "failed")
  }
  u
}

score_gold <- function(tab = score_gold_tab(), cb = score_cb(), done = NULL, rows = NULL, prefilled = FALSE) {
  files <- unique(tab$file)
  pages <- data.frame(file = files, done = if (is.null(done)) TRUE else done,
                      n_rows = as.integer(table(factor(tab$file, levels = files))), stringsAsFactors = FALSE)
  .new_gold(tab, cb, pages, rows = rows, prefilled = prefilled, source = "gold.xlsx")
}

# An extraction result built from a table, as transcribe() would leave
# it: one page record per file with the model's JSON data.
score_result <- function(tab, cb, failed = character(), model = "mock-model") {
  files <- unique(tab$file)
  page_vars <- cb$variables$variable[cb$variables$level == "page"]
  row_vars <- cb$variables$variable[cb$variables$level == "row"]
  recs <- list()
  for (i in seq_along(files)) {
    f <- files[i]
    sub <- tab[tab$file == f, , drop = FALSE]
    ok <- !f %in% failed
    data <- NULL
    if (ok) {
      data <- lapply(page_vars, function(v) {
        x <- sub[[v]][!is.na(sub[[v]])]
        if (length(x)) x[[1]] else NA
      })
      names(data) <- page_vars
      if (length(row_vars)) {
        data$records <- lapply(seq_len(nrow(sub)), function(k) as.list(sub[k, row_vars, drop = FALSE]))
      }
    }
    recs[[.page_key(f)]] <- list(file = f, source = "register.pdf", source_page = i,
                                 status = if (ok) "ok" else "failed",
                                 problem = if (!ok) "truncated", message = if (!ok) "The reply was cut off.",
                                 data = data, model = model, date = "2026-10-20 09:00:00",
                                 settings = list(engine = "sync"))
  }
  info <- new.env(parent = emptyenv())
  info$records <- recs
  info$codebook <- cb
  info$settings <- list(model = model, resolution = "high", thinking = "low", max_tokens = 32768, engine = "sync")
  info$date <- "2026-10-20 09:00:00"
  pages <- data.frame(file = files, source = "register.pdf", source_page = seq_along(files), stringsAsFactors = FALSE)
  info$pages <- .pages_table(pages, recs, cb)
  out <- .flatten_records(Filter(function(r) r$status == "ok", recs), cb)
  .new_result(out, info)
}

measure <- function(s, m) s$summary[s$summary$measure == m, , drop = FALSE]

# ------- the planted-diff protocol -------------------------------------------

test_that("every planted error is caught in the right variable (rows paired by content)", {
  cb <- score_cb()
  s <- .score_units(score_units_ok(score_gold_tab(), cb), score_units_ok(score_output_tab(), cb), cb)
  expect_s3_class(s, "amanuensis_score")

  # by hand: 12 row pairs (5 + 4 with one missing + 3 with one extra); rows
  # right: p001 none, p002 three, p003 two
  expect_equal(measure(s, "rows")$matched, 5)
  expect_equal(measure(s, "rows")$compared, 12)
  # values: 3 page values + 12 pairs x 5 variables = 63; wrong: 5 planted on
  # p001, 5 for the deleted row, 5 for the inserted row, 1 page value
  expect_equal(measure(s, "values")$compared, 63)
  expect_equal(measure(s, "values")$matched, 47)
  # characters (text variables): 155 characters, 19 edits
  #   p001: Ferre 1 + Joan 1 + jornaler 1; p002: deleted "Font" 4;
  #   p003: page 15 1 + inserted "Riera" 5 + "Antoni" 6
  expect_equal(measure(s, "characters")$compared, 155)
  expect_equal(measure(s, "characters")$value, 1 - 19 / 155)
  expect_equal(measure(s, "page values")$matched, 2)
  expect_equal(measure(s, "page values")$compared, 3)
  expect_equal(measure(s, "doubt marks")$matched, 1)
  expect_equal(measure(s, "doubt marks")$compared, 16)

  bv <- s$by_variable
  expect_equal(bv$variable, c("printed_page", "surname", "name", "age", "civil", "occupation"))
  expect_equal(bv$differences, c(1, 3, 3, 3, 3, 3))
  expect_equal(bv$compared, c(3, 12, 12, 12, 12, 12))
  expect_equal(bv$edit_distance, c(1, 10, 7, NA, NA, 1))
  expect_equal(bv$character_match[2], 1 - 10 / 54)
  expect_true(all(is.na(bv$character_match[4:5])))

  d <- s$differences
  expect_equal(nrow(d), 16)
  one <- function(file, variable, row_gold) {
    d[d$file == file & d$variable == variable & d$row_gold %in% row_gold, , drop = FALSE]
  }
  digit <- one("p001.jpg", "age", 1)
  expect_equal(c(digit$gold, digit$output), c("34", "54"))
  expect_true(is.na(digit$edits))
  accent <- one("p001.jpg", "surname", 2)
  expect_equal(accent$output, "Ferre")
  expect_equal(accent$edits, 1)
  doubt <- one("p001.jpg", "name", 3)
  expect_equal(c(doubt$gold, doubt$output, doubt$note), c("Joan?", "Joan", "only a final ?"))
  typo <- one("p001.jpg", "occupation", 4)
  expect_equal(c(typo$gold, typo$output), c("jornalr", "jornaler"))
  expect_equal(typo$edits, 1)
  cat_wrong <- one("p001.jpg", "civil", 5)
  expect_equal(c(cat_wrong$gold, cat_wrong$output), c("married", "single"))

  deleted <- d[d$file == "p002.jpg", ]
  expect_equal(nrow(deleted), 5)
  expect_true(all(deleted$row_gold == 2 & is.na(deleted$row_output)))
  expect_true(all(deleted$note == "row missing from the output"))
  # the deleted row's empty gold cells still count as wrong, with 0 edits
  expect_equal(deleted$edits[deleted$variable %in% c("surname", "name", "occupation")], c(4, 0, 0))

  inserted <- d[d$file == "p003.jpg" & d$variable != "printed_page", ]
  expect_equal(nrow(inserted), 5)
  expect_true(all(is.na(inserted$row_gold) & inserted$row_output == 2))
  expect_true(all(inserted$note == "row not in the benchmark"))
  expect_equal(one("p003.jpg", "printed_page", NA)$output, "15")

  bp <- s$by_page
  expect_equal(bp$alignment, c("position", "content", "content"))
  expect_equal(bp$missing_rows, c(0L, 1L, 0L))
  expect_equal(bp$extra_rows, c(0L, 0L, 1L))
  expect_equal(bp$differences, c(5L, 5L, 6L))
  expect_true(any(grepl("p002.jpg: your benchmark has 4 rows, the output 3", s$notes)))
})

test_that("pairing by position cascades after a deleted row; content pairing does not", {
  cb <- score_cb()
  gu <- score_units_ok(score_gold_tab(), cb)
  ou <- score_units_ok(score_output_tab(), cb)
  pos <- .score_units(gu, ou, cb, align = "position")
  expect_equal(pos$by_page$alignment, rep("position", 3))
  # p002: rows 2 and 3 now meet the next person (4 wrong each, the empty
  # occupation still matching) and row 4 meets nothing (5 wrong)
  expect_equal(pos$by_page$differences, c(5L, 13L, 10L))
  expect_equal(measure(pos, "rows")$matched, 2)
  expect_equal(measure(pos, "values")$matched, 35)
  cont <- .score_units(gu, ou, cb, align = "content")
  expect_equal(cont$by_page$alignment, rep("content", 3))
  expect_equal(cont$by_page$differences, c(5L, 5L, 6L))
  # same counts on a page: auto pairs by position
  auto <- .score_units(gu, ou, cb)
  expect_equal(auto$by_page$alignment[1], "position")
  # two runs give identical results
  expect_identical(.score_units(gu, ou, cb), auto)
})

test_that("content pairing leaves unrelated rows unpaired and keeps the order", {
  pairs <- .dp_align(diag(3), diag(3) >= 0.5)
  expect_equal(pairs$gi, 1:3)
  expect_equal(pairs$oi, 1:3)
  # gold A B C, output A X C: B and X are not alike, so both stay unpaired,
  # the missing benchmark row listed before the extra output row
  sim <- matrix(c(1, 0.1, 0, 0.1, 0.2, 0.1, 0, 0.1, 1), 3, byrow = TRUE)
  pairs <- .dp_align(sim, sim >= 0.5)
  expect_equal(pairs$gi, c(1L, 2L, NA, 3L))
  expect_equal(pairs$oi, c(1L, NA, 2L, 3L))

  cb <- score_cb()
  g <- score_gold_tab()[score_gold_tab()$file == "p003.jpg", ]
  o <- g
  o$surname <- c("Riba", "Prat")
  o$name <- c("Eulalia", "Ramon")
  o$age <- c(3L, 9L)
  o$civil <- c("single", "single")
  o$occupation <- c("teixidor", "mestre")
  o <- rbind(o, o[1, ])
  o$row <- 1:3
  s <- .score_units(score_units_ok(g, cb), score_units_ok(o, cb), cb)
  expect_equal(s$by_page$missing_rows, 2L)
  expect_equal(s$by_page$extra_rows, 3L)
  expect_equal(measure(s, "rows")$compared, 5)
})

test_that("normalize = TRUE adds lenient figures beside the strict ones", {
  cb <- score_cb()
  s <- .score_units(score_units_ok(score_gold_tab(), cb), score_units_ok(score_output_tab(), cb), cb,
                    normalize = TRUE)
  expect_equal(measure(s, "values")$matched, 47)
  # Ferre and Joan become right; the "corrected" typo, the digit and the
  # category stay wrong
  expect_equal(measure(s, "values (lenient)")$matched, 49)
  expect_equal(measure(s, "rows (lenient)")$matched, 7)
  # "Joan?" -> "joan" shortens the pool by one character; two edits forgiven
  expect_equal(measure(s, "characters (lenient)")$value, 1 - 17 / 154)
  bv <- s$by_variable
  expect_equal(bv$lenient_differences, c(1, 2, 2, 3, 3, 3))
  expect_true(all(c("lenient_variable_match", "lenient_character_match") %in% names(bv)))
  d <- s$differences
  expect_equal(d$note[d$variable == "surname" & d$row_gold %in% 2 & d$file == "p001.jpg"],
               "only case, accents, punctuation or spacing")
  expect_equal(d$note[d$variable == "name" & d$row_gold %in% 3 & d$file == "p001.jpg"], "only a final ?")
  # without normalize there are no lenient columns
  plain <- .score_units(score_units_ok(score_gold_tab(), cb), score_units_ok(score_output_tab(), cb), cb)
  expect_false(any(grepl("lenient", c(plain$summary$measure, names(plain$by_variable)))))
})

test_that("the lenient form folds case, accents and punctuation but never digits", {
  expect_equal(.lenient("Ferr\u00e9, Joan?"), "ferre joan")
  expect_equal(.lenient("  PUIG-Ferrer  "), "puig ferrer")
  expect_false(identical(.lenient("1.5"), .lenient("15")))
  expect_equal(.lenient("1.5"), "1.5")
  expect_true(is.na(.lenient("?")))
  expect_equal(.lenient(NA_character_), NA_character_)
})

test_that("the lenient match forgives punctuation inside a word, never a changed number", {
  same <- function(a, b) identical(.lenient_key(.lenient(a)), .lenient_key(.lenient(b)))
  # the Catalan middle dot, an apostrophe, a hyphen, a space
  expect_true(same("Ul\u00b7ldemolins", "Ulldemolins"))
  expect_true(same("Marcel\u00b7l\u00ed", "Marcell\u00ed"))
  expect_true(same("D'Arcy", "DArcy"))
  expect_true(same("Puig-Ferrer", "Puig Ferrer"))
  expect_true(same("Puig Ferrer", "PuigFerrer"))
  expect_false(same("1.5", "15"))
  expect_false(same("1 5", "15"))
  expect_false(same("Puig", "Puig Ferrer"))

  cb <- suppressMessages(codebook(data.frame(variable = c("surname", "name"), type = "text", level = "row",
                                             description = "x", stringsAsFactors = FALSE)))
  g <- data.frame(file = "a.jpg", row = 1:2, surname = c("Ul\u00b7ldemolins Casals", "Puig"),
                  name = c("Marcel\u00b7l\u00ed", "Maria"), stringsAsFactors = FALSE)
  o <- g
  o$surname[1] <- "Ulldemolins Casals"
  o$name[1] <- "Marcell\u00ed"
  s <- .score_units(score_units_ok(g, cb), score_units_ok(o, cb), cb, normalize = TRUE)
  expect_equal(measure(s, "values")$matched, 2)
  expect_equal(measure(s, "values (lenient)")$matched, 4)
  expect_equal(measure(s, "characters (lenient)")$value, 1)
  expect_true(all(s$differences$note == "only case, accents, punctuation or spacing"))
})

test_that("the lenient form does not depend on the computer's language settings", {
  # text R could not translate in a "C" locale (unmarked UTF-8 bytes): with
  # tolower() the accent became its byte codes ("marcell c3 ad")
  x <- "Marcell\u00ed GARC\u00cdA"
  Encoding(x) <- "unknown"
  withr::with_locale(c(LC_CTYPE = "C"), {
    expect_equal(.lenient(x), "marcelli garcia")
    expect_equal(.lenient("Ferr\u00e9, Joan?"), "ferre joan")
  })
  expect_equal(.lenient(x), "marcelli garcia")
})

test_that("values are compared after converting both sides to the codebook type", {
  num <- .score_values(c("34", " 34 ", "34.0", "", NA, "1 069"), "integer")
  expect_equal(num$key, c("34", "34", "34", NA, NA, "1 069"))
  expect_equal(.score_values(c(34L, NA), "integer")$key, c("34", NA))
  bool <- .score_values(c("yes", "FALSE", "", "maybe"), "boolean")
  expect_equal(bool$key, c("TRUE", "FALSE", NA, "maybe"))
  # "" is empty, and NFD text equals its NFC form
  txt <- .score_values(c("", "Jose\u0301", "Jos\u00e9"), "text")
  expect_true(is.na(txt$key[1]))
  expect_identical(txt$key[2], txt$key[3])
})

test_that("an empty cell matches an empty cell, but a missing row matches nothing", {
  cb <- score_cb()
  g <- score_gold_tab()[score_gold_tab()$file == "p002.jpg", ]
  o <- g
  o$name[2] <- ""
  s <- .score_units(score_units_ok(g, cb), score_units_ok(o, cb), cb)
  expect_equal(measure(s, "values")$matched, measure(s, "values")$compared)
  o <- g[-2, ]
  s <- .score_units(score_units_ok(g, cb), score_units_ok(o, cb), cb)
  d <- s$differences
  expect_true(all(c("name", "age", "civil", "occupation") %in% d$variable))
})

# ------- pages failed, not run, not in the output -------------------------------

test_that("a failed page counts as entirely wrong", {
  cb <- score_cb()
  g <- score_gold_tab()[score_gold_tab()$file == "p001.jpg", ]
  s <- .score_units(score_units_ok(g, cb), score_units_ok(g, cb, failed = "p001.jpg"), cb)
  expect_equal(measure(s, "values")$matched, 0)
  expect_equal(measure(s, "values")$compared, 26)
  expect_equal(measure(s, "rows")$matched, 0)
  expect_equal(measure(s, "rows")$compared, 5)
  # every character of the benchmark's text counts as an edit:
  # 2 + (4+5+5) + (5+5+0) + (5+5+8) + (5+4+7) + (4+4+8) = 76
  expect_equal(measure(s, "characters")$compared, 76)
  expect_equal(measure(s, "characters")$value, 0)
  expect_equal(s$by_page$status, "failed")
  expect_equal(s$by_page$alignment, "none")
  expect_equal(s$by_page$missing_rows, 5L)
  expect_true(all(s$differences$note == "page failed"))
  expect_true(any(grepl("failed in the output and counts as entirely wrong", s$notes)))
})

test_that("pages to check are scored with the data the model returned", {
  cb <- score_cb()
  ou <- score_units_ok(score_output_tab(), cb)
  ou[["p001.jpg"]]$status <- "check"
  s <- .score_units(score_units_ok(score_gold_tab(), cb), ou, cb)
  expect_equal(s$by_page$status, c("check", "ok", "ok"))
  expect_equal(measure(s, "values")$matched, 47)
})

test_that("score() puts the notes that matter most first", {
  cb <- score_cb()
  o <- score_output_tab()
  res <- score_result(o[o$file != "p003.jpg", ], cb, failed = "p002.jpg")
  s <- score(score_gold(cb = cb, prefilled = TRUE), res)
  expect_match(s$notes[1], "optimistic")
  expect_match(s$notes[2], "not in the results yet")
  expect_match(s$notes[3], "failed in the output")
  expect_equal(s$not_run, "p003.jpg")
  expect_equal(s$by_page$status, c("ok", "failed", "not run"))
})

test_that("benchmark pages not run are listed and left out of every figure", {
  cb <- score_cb()
  g <- score_gold_tab()[score_gold_tab()$file %in% c("p001.jpg", "p002.jpg"), ]
  o <- score_output_tab()
  o <- o[o$file == "p001.jpg", ]
  s <- .score_units(score_units_ok(g, cb), score_units_ok(o, cb), cb)
  expect_equal(s$not_run, "p002.jpg")
  expect_equal(measure(s, "values")$compared, 26)
  expect_equal(measure(s, "values")$matched, 21)
  expect_equal(s$by_page$status, c("ok", "not run"))
  expect_true(is.na(s$by_page$differences[2]))
  expect_true(any(grepl("only = gold", s$notes)))
  expect_equal(attr(s, "about")$n_pages, 1)
})

test_that("no benchmark page in the output is a plain error suggesting only = gold", {
  cb <- score_cb()
  g <- score_gold_tab()
  o <- g
  o$file <- sub("^p", "q", o$file)
  expect_error(.score_units(score_units_ok(g, cb), score_units_ok(o, cb), cb),
               class = "amanuensis_error_input", regexp = "only = gold")
})

test_that("empty denominators give NA, never 100%", {
  cb2 <- suppressMessages(codebook(data.frame(variable = c("age", "civil"), type = c("integer", "category"),
                                              values = c("", "single; married"), description = "x")))
  g <- data.frame(file = "a.jpg", row = 1:2, age = c(3L, 4L), civil = c("single", "married"), stringsAsFactors = FALSE)
  s <- .score_units(score_units_ok(g, cb2), score_units_ok(g, cb2), cb2)
  expect_true(is.na(measure(s, "characters")$value))
  expect_equal(measure(s, "characters")$compared, 0)
  expect_equal(measure(s, "values")$value, 1)

  # a page with no rows on either side: nothing to compare
  g0 <- list(a.jpg = .page_unit(list(records = list()), cb2))
  o0 <- list(a.jpg = c(.page_unit(list(records = list()), cb2), status = "ok"))
  s0 <- .score_units(g0, o0, cb2)
  expect_true(is.na(measure(s0, "rows")$value))
  expect_true(is.na(measure(s0, "values")$value))
  expect_output(print(s0), "Values right")
  expect_false(any(grepl("100", utils::capture.output(print(s0)))))

  # a codebook of page values only has no row figure
  cb3 <- suppressMessages(codebook(data.frame(variable = c("title", "year"), type = c("text", "integer"),
                                              level = "page", description = "x")))
  g3 <- data.frame(file = c("a.jpg", "b.jpg"), title = c("Padr\u00f3", "Cens"), year = c(1930L, 1931L),
                   stringsAsFactors = FALSE)
  o3 <- g3
  o3$year[2] <- 1937L
  s3 <- .score_units(score_units_ok(g3, cb3), score_units_ok(o3, cb3), cb3)
  expect_true(is.na(measure(s3, "rows")$value))
  expect_equal(measure(s3, "values")$matched, 3)
  expect_equal(measure(s3, "page values")$matched, 1)
  expect_true(all(is.na(s3$by_page$gold_rows)))
})

# ------- free text ------------------------------------------------------------

test_that("free text is scored by characters and words right", {
  cb <- codebook(free_text = TRUE)
  g <- data.frame(file = c("a.jpg", "b.jpg"), text = c("En la villa de Vilanova", "a b c d"), stringsAsFactors = FALSE)
  o <- data.frame(file = c("a.jpg", "b.jpg"), text = c("En la vila de Vilanova", "a c d e f"), stringsAsFactors = FALSE)
  s <- .score_units(score_units_ok(g, cb), score_units_ok(o, cb), cb)
  expect_equal(attr(s, "about")$mode, "text")
  expect_equal(s$summary$measure, c("characters", "words", "pages"))
  # characters: villa/vila 1 edit over 23; "a b c d"/"a c d e f" 5 edits over 9
  expect_equal(utils::adist("a b c d", "a c d e f")[1, 1], 5)
  expect_equal(measure(s, "characters")$value, 1 - 6 / 32)
  # words: 1 replaced of 5; b deleted, e and f inserted = 3 of 5
  expect_equal(measure(s, "words")$value, 1 - 4 / 10)
  expect_equal(measure(s, "pages")$matched, 0)
  out <- utils::capture.output(print(s))
  expect_true(any(grepl("Words right", out)))
  expect_false(any(grepl("Rows with every value", out)))
})

test_that("words are counted whole, whatever their length", {
  expect_equal(.word_lev("the cat sat", "the dog sat"), 1)
  expect_equal(.word_lev("Vilanova de la Riba", "Vilanova de la"), 1)
  expect_equal(.word_lev("a b", ""), 2)
  expect_equal(.word_lev(NA_character_, NA_character_), 0)
  expect_equal(.word_lev("Ferr\u00e9 i Puig", "Ferre i Puig"), 1)
  expect_equal(length(.private_codes(7000)), 7000)
  expect_true(all(.private_codes(7000)[6401:7000] >= 0xF0000))
})

test_that("a table with the page's other text scores that text on its own", {
  cb <- suppressMessages(codebook(data.frame(variable = c("surname", "age"), type = c("text", "integer"),
                                             description = "x"), free_text = TRUE))
  g <- data.frame(file = "a.jpg", row = 1:2, surname = c("Puig", "Mas"), age = c(3L, 4L),
                  text = c("Barri de Sant Pere", NA), stringsAsFactors = FALSE)
  o <- g
  o$text[1] <- "Bari de Sant Pere"
  s <- .score_units(score_units_ok(g, cb), score_units_ok(o, cb), cb)
  expect_equal(measure(s, "values")$compared, 4)
  expect_equal(measure(s, "values")$matched, 4)
  expect_equal(measure(s, "other text: characters")$value, 1 - 1 / 18)
  expect_equal(measure(s, "other text: words")$value, 1 - 1 / 4)
  expect_equal(s$by_variable$differences[s$by_variable$variable == "text"], 1)
})

# ------- score() itself -------------------------------------------------------

test_that("score() reads a benchmark and a result and prints in plain words", {
  cb <- score_cb()
  gold <- score_gold(cb = cb)
  res <- score_result(score_output_tab(), cb)
  s <- score(gold, res)
  expect_s3_class(s, "amanuensis_score")
  expect_equal(measure(s, "values")$matched, 47)
  expect_equal(attr(s, "about")$model, "mock-model")
  expect_named(s, c("summary", "by_variable", "by_page", "differences", "not_run", "notes"))
  expect_named(s$by_page, c("file", "status", "alignment", "gold_rows", "output_rows", "missing_rows", "extra_rows",
                            "differences"))
  out <- utils::capture.output(print(s))
  expect_match(out[1], "Accuracy against your benchmark \\(3 pages, 11 rows, 6 variables; mock-model\\)")
  expect_true(any(grepl("Rows with every value right +5 of 12 +41.7%", out)))
  expect_true(any(grepl("Values right +47 of 63 +74.6%", out)))
  expect_true(any(grepl("1 of the 16 wrong values differ only by a final \"?\"", out, fixed = TRUE)))
  expect_true(any(grepl("^  Weakest: ", out)))
  expect_true(any(grepl("Next: View(s$differences)", out, fixed = TRUE)))
  expect_true(any(grepl("rows paired by content", out)))
})

test_that("score() warns that a pre-filled benchmark is optimistic", {
  cb <- score_cb()
  s <- score(score_gold(cb = cb, prefilled = TRUE), score_result(score_output_tab(), cb))
  expect_match(s$notes[1], "optimistic")
  expect_true(any(grepl("optimistic", utils::capture.output(print(s)))))
})

test_that("score() compares only the pages marked done and the first N rows when asked", {
  cb <- score_cb()
  s <- score(score_gold(cb = cb, done = c(TRUE, FALSE, FALSE)), score_result(score_output_tab(), cb))
  expect_equal(s$by_page$file, "p001.jpg")
  expect_true(any(grepl("not marked as done", s$notes)))
  expect_true(any(grepl("other pages? that", s$notes)))

  g <- score_gold_tab()
  g2 <- g[g$row <= 2, ]
  s2 <- score(score_gold(g2, cb, rows = 2), score_result(score_output_tab(), cb))
  # every page reaches the limit, so every page is paired by content and the
  # output is compared up to the row paired with the benchmark's last one:
  # p001 rows 1-2 (age and surname wrong); p002: Font, deleted by the model,
  # is missing and Serra (row 3 of the page) is beyond the benchmark, not
  # paired with Font; p003: the inserted Riera is an extra row and Camps
  # meets Camps
  expect_equal(s2$by_page$alignment, rep("content", 3))
  expect_equal(s2$by_page$output_rows, c(2L, 1L, 3L))
  expect_equal(s2$by_page$missing_rows, c(0L, 1L, 0L))
  expect_equal(s2$by_page$extra_rows, c(0L, 0L, 1L))
  expect_true(any(grepl("first 2 rows", s2$notes)))
})

# ------- a benchmark of the first N rows ----------------------------------------

rows_cb <- function() {
  suppressMessages(codebook(data.frame(variable = c("name", "age"), type = c("text", "integer"), level = "row",
                                       description = "x", stringsAsFactors = FALSE)))
}

# A page of 30 people; the benchmark has its first 10 (benchmark(rows = 10)).
rows_page <- function() {
  nm <- c("Josep", "Maria", "Joan", "Pere", "Anna", "Jaume", "Rosa", "Llucia", "Marti", "Teresa",
          "Antoni", "Carme", "Ramon", "Pilar", "Jordi", "Nuria", "Enric", "Montse", "Xavier", "Laia",
          "Pau", "Marta", "Oriol", "Eva", "Albert", "Rut", "Lluis", "Clara", "Arnau", "Berta")
  data.frame(file = "p001.jpg", row = 1:30, name = nm, age = 20:49, stringsAsFactors = FALSE)
}

renumber <- function(tab) {
  tab$row <- seq_len(nrow(tab))
  rownames(tab) <- NULL
  tab
}

test_that("rows = N: a row the model skipped in the first N is one missing row, not a cascade", {
  cb <- rows_cb()
  full <- rows_page()
  g <- score_gold(full[1:10, ], cb, rows = 10)
  # the model skipped row 3 and read the other 29: before the fix both sides
  # were cut to 10 rows and paired by position (2 of 10 right), and with
  # align = "content" the correct row 11 counted as an extra error
  s <- score(g, score_result(renumber(full[-3, ]), cb))
  expect_equal(measure(s, "rows")$matched, 9)
  expect_equal(measure(s, "rows")$compared, 10)
  expect_equal(s$by_page$alignment, "content")
  expect_equal(s$by_page$missing_rows, 1L)
  expect_equal(s$by_page$extra_rows, 0L)
  expect_equal(s$by_page$output_rows, 9L)
  expect_equal(unique(s$differences$row_gold), 3L)
  expect_true(any(grepl("first 10 rows of the page, paired by content with the first 9 of the 29", s$notes)))
  s_c <- score(g, score_result(renumber(full[-3, ]), cb), align = "content")
  expect_identical(s_c$summary, s$summary)

  # a spurious row added after row 2: one extra row, nothing missing
  ins <- full[2, ]
  ins$name <- "Quim"
  ins$age <- 3L
  s2 <- score(g, score_result(renumber(rbind(full[1:2, ], ins, full[3:30, ])), cb))
  expect_equal(measure(s2, "rows")$matched, 10)
  expect_equal(measure(s2, "rows")$compared, 11)
  expect_equal(s2$by_page$extra_rows, 1L)
  expect_equal(s2$by_page$missing_rows, 0L)

  # the whole page read right: 10 of 10, with nothing extra
  s3 <- score(g, score_result(full, cb))
  expect_equal(measure(s3, "rows")$value, 1)
  expect_equal(s3$by_page$output_rows, 10L)
  expect_equal(nrow(s3$differences), 0)

  # equal counts prove nothing at the limit: with row 3 skipped, a page of
  # 11 rows gives 10 rows on each side
  s4 <- score(g, score_result(renumber(full[c(1:2, 4:11), ]), cb))
  expect_equal(measure(s4, "rows")$matched, 9)
  expect_equal(s4$by_page$alignment, "content")

  # align = "position" still cuts the output at N rows and pairs in order
  s5 <- score(g, score_result(renumber(full[-3, ]), cb), align = "position")
  expect_equal(s5$by_page$output_rows, 10L)
  expect_equal(measure(s5, "rows")$matched, 2)

  # a page shorter than the limit holds the whole page: extra rows count
  g6 <- score_gold(full[1:7, ], cb, rows = 10)
  s6 <- score(g6, score_result(full[1:9, ], cb))
  expect_equal(s6$by_page$extra_rows, 2L)
  expect_equal(s6$by_page$output_rows, 9L)
})

test_that("rows = N: the benchmark's last row does not pair with a look-alike far down the page", {
  cb <- rows_cb()
  full <- rows_page()
  # the model skipped row 10 (Teresa, 29); row 25 of the output is a
  # Teresa aged 29 too. Pairing them would make 15 extra rows.
  out <- full[-10, ]
  out$name[24] <- "Teresa"
  out$age[24] <- 29L
  s <- score(score_gold(full[1:10, ], cb, rows = 10), score_result(renumber(out), cb))
  expect_equal(s$by_page$missing_rows, 1L)
  expect_equal(s$by_page$extra_rows, 0L)
  expect_equal(measure(s, "rows")$compared, 10)

  # the open end, on a similarity matrix: 2 gold rows, 6 output rows; the
  # second gold row looks like the sixth output row only
  sim <- matrix(c(1, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 1), 2, byrow = TRUE)
  p <- .dp_align(sim, sim >= 0.5, gap = 0.5, open_end = TRUE)
  expect_equal(p$gi, c(1L, 2L))
  expect_equal(p$oi, c(1L, NA))
  # one or two rows the model added before it are still jumped over
  sim2 <- sim[, c(1, 2, 3, 6, 4, 5)]
  p2 <- .dp_align(sim2, sim2 >= 0.5, gap = 0.5, open_end = TRUE)
  expect_equal(p2$gi, c(1L, NA, NA, 2L))
  expect_equal(p2$oi, 1:4)
  # without the open end, as for complete pages, the pairs' total alone wins
  p0 <- .dp_align(sim, sim >= 0.5)
  expect_equal(p0$gi, c(1L, NA, NA, NA, NA, 2L))
  expect_equal(p0$oi, 1:6)
})

# ------- the benchmark table given ------------------------------------------------

test_that("a benchmark cut down after reading scores only what is left", {
  cb <- score_cb()
  res <- score_result(score_output_tab(), cb)
  gold <- score_gold(cb = cb)
  plain <- score(as.data.frame(gold[gold$file != "p002.jpg", ]), res)
  # gold[...] keeps the record that p002 is done: it must not be scored as a
  # page with no rows (every row of the output there counted as wrong)
  s <- score(gold[gold$file != "p002.jpg", ], res)
  expect_equal(s$by_page$file, c("p001.jpg", "p003.jpg"))
  expect_equal(measure(s, "rows")$matched, measure(plain, "rows")$matched)
  expect_equal(measure(s, "rows")$compared, 8)
  expect_equal(measure(s, "values")$matched, measure(plain, "values")$matched)
  expect_equal(measure(s, "characters")$value, measure(plain, "characters")$value)
  expect_true(any(grepl("1 page marked done in your benchmark has no lines in the table you gave and was left out: p002.jpg",
                        s$notes, fixed = TRUE)))
  # subset() keeps the class but drops the record: read as a plain table
  s_sub <- score(subset(gold, file != "p002.jpg"), res)
  expect_equal(measure(s_sub, "values")$matched, measure(plain, "values")$matched)
  # a column taken out is not compared, and listed
  s_col <- score(gold[, c("file", "row", "printed_page", "surname", "name", "age")], res)
  expect_equal(s_col$by_variable$variable, c("printed_page", "surname", "name", "age"))
  expect_true(any(grepl("only in the output\\): civil, occupation", s_col$notes)))
  # rows taken out of a page are said
  s_row <- score(gold[!(gold$file == "p001.jpg" & gold$row > 3), ], res)
  expect_true(any(grepl("fewer rows than your benchmark on p001.jpg (3 of 5)", s_row$notes, fixed = TRUE)))
  # nothing done left in the table
  expect_error(score(gold[0, ], res), "None of the pages marked done", class = "amanuensis_error_input")
})

test_that("a data frame benchmark is read like a file: missing columns are not compared, blank rows numbered", {
  cb <- score_cb()
  res <- score_result(score_output_tab(), cb)
  full <- score(score_gold_tab(), res)
  # civil and occupation never transcribed: listed as not compared, never
  # scored as wrong
  part <- score_gold_tab()[, c("file", "row", "printed_page", "surname", "name", "age")]
  s <- suppressMessages(score(part, res))
  expect_equal(s$by_variable$variable, c("printed_page", "surname", "name", "age"))
  expect_true(any(grepl("Not compared (only in the output): civil, occupation", s$notes, fixed = TRUE)))
  expect_equal(measure(s, "values")$compared, 3 + 12 * 3)
  expect_equal(s$by_variable$differences, c(1, 3, 3, 3))
  # a row column left empty: rows numbered in the order of the lines
  norow <- score_gold_tab()
  norow$row <- NA
  s2 <- suppressMessages(score(norow, res))
  expect_identical(s2$summary, full$summary)
  expect_equal(attr(s2, "about")$gold_rows, 11)
  # no row column at all
  s3 <- suppressMessages(score(score_gold_tab()[, -2], res))
  expect_identical(s3$summary, full$summary)
  # a table with none of the variables
  expect_error(score(data.frame(file = "p001.jpg", x = 1), res), "no column for any variable",
               class = "amanuensis_error_input")
  expect_error(score(data.frame(page = "p001.jpg", surname = "Puig"), res), "needs a .*file", class = "amanuensis_error_input")
})

test_that("yes/no written S and N, as the registers do, is read as yes and no", {
  cb <- suppressMessages(codebook(data.frame(variable = c("name", "reads"), type = c("text", "yes/no"),
                                             level = "row", description = "x", stringsAsFactors = FALSE)))
  out <- data.frame(file = "p001.jpg", row = 1:4, name = c("Josep", "Maria", "Joan", "Pere"),
                    reads = c(TRUE, FALSE, TRUE, FALSE), stringsAsFactors = FALSE)
  g <- out
  g$reads <- c("S", "N", "s", "n")
  s <- suppressMessages(score(g, score_result(out, cb)))
  expect_equal(measure(s, "values")$value, 1)
  expect_equal(nrow(s$differences), 0)
  expect_identical(.score_values(c("S", "N", "S\u00ed", "non", "maybe"), "boolean")$key,
                   c("TRUE", "FALSE", "TRUE", "FALSE", "maybe"))
})

test_that("the page's other text stays apart when the benchmark file is read back without a codebook", {
  local_amanuensis()
  dir <- withr::local_tempdir()
  make_jpegs(file.path(dir, "pages"), n = 2)
  cb <- suppressMessages(codebook(data.frame(variable = c("surname", "age"), type = c("text", "integer"),
                                             description = "x", stringsAsFactors = FALSE), free_text = TRUE))
  f <- file.path(dir, "gold.xlsx")
  quietly(benchmark(f, images = file.path(dir, "pages"), codebook = cb, n = 1, seed = 1930, app = FALSE))
  sheets <- readxl::excel_sheets(f)
  sh <- lapply(stats::setNames(sheets, sheets), function(x) as.data.frame(readxl::read_excel(f, x, col_types = "text")))
  expect_identical(sh$about$value[sh$about$item == "free_text"], "yes")
  fl <- sh$benchmark$file[1]
  long <- "Barri de Sant Pere. Llista dels vehins del carrer Major, any 1930"
  sh$benchmark <- data.frame(file = fl, row = c("1", "2"), text = c(long, NA), surname = c("Puig", "Mas"),
                             age = c("3", "4"), stringsAsFactors = FALSE)
  sh$pages$done <- "yes"
  writexl::write_xlsx(sh, f)
  out <- data.frame(file = fl, row = 1:2, text = c(sub("vehins", "veins", long), NA), surname = c("Puig", "Mas"),
                    age = c(3L, 4L), stringsAsFactors = FALSE)
  res <- score_result(out, cb)
  check <- function(s) {
    expect_equal(measure(s, "values")$matched, 4)
    expect_equal(measure(s, "values")$compared, 4)
    expect_equal(nrow(measure(s, "page values")), 0)
    expect_equal(measure(s, "other text: characters")$compared, nchar(long))
    expect_equal(attr(s, "about")$separate, "text")
  }
  g <- quietly(benchmark(f))
  expect_true(.gold_info(g)$codebook$free_text)
  expect_identical(.gold_info(g)$codebook$variables$variable, c("surname", "age", "text"))
  check(score(g, res))
  check(quietly(score(f, res)))
  # a file written before the choice was recorded: known by the description
  # the package gives the text variable
  sh$about <- sh$about[sh$about$item != "free_text", ]
  writexl::write_xlsx(sh, f)
  expect_true(.gold_info(quietly(benchmark(f)))$codebook$free_text)
  # and whatever the benchmark's codebook says, the results' codebook is
  # enough for score() to keep that text apart
  plain <- .score_codebook_subset(cb, cb$variables$variable)
  plain$free_text <- FALSE
  gtab <- out
  gtab$text[1] <- long
  check(score(score_gold(gtab, plain), res))
})

test_that("score() names itself in the errors of the benchmark file it reads", {
  local_amanuensis()
  dir <- withr::local_tempdir()
  make_jpegs(file.path(dir, "pages"), n = 3)
  cb <- score_cb()
  f <- file.path(dir, "gold.xlsx")
  quietly(benchmark(f, images = file.path(dir, "pages"), codebook = cb, n = 2, seed = 1930, app = FALSE))
  err <- tryCatch(quietly(score(f, score_result(score_gold_tab(), cb))), error = function(e) e)
  expect_s3_class(err, "amanuensis_error_input")
  expect_match(conditionMessage(err), "no values yet")
  expect_identical(rlang::call_name(conditionCall(err)), "score")
})

test_that("score() compares two runs, the first as reference", {
  cb <- score_cb()
  res1 <- score_result(score_gold_tab(), cb, model = "gemini-4-argon")
  res2 <- score_result(score_output_tab(), cb, model = "gemini-3.8-flash")
  s <- score(res1, res2)
  expect_equal(measure(s, "values")$matched, 47)
  expect_equal(attr(s, "about")$reference, "run")
  out <- utils::capture.output(print(s))
  expect_match(out[1], "Agreement with the first run .*gemini-3.8-flash against gemini-4-argon")
  expect_true(any(grepl("Values the same", out)))
  expect_true("row missing from the second run" %in% s$differences$note)
  expect_true("row not in the first run" %in% s$differences$note)
  # a failed page of the reference run is left out, and said so
  res3 <- score_result(score_gold_tab(), cb, failed = "p001.jpg")
  s3 <- score(res3, res2)
  expect_equal(s3$by_page$file, c("p002.jpg", "p003.jpg"))
  expect_true(any(grepl("first run has no data", s3$notes)))
})

test_that("score() accepts a data frame in the layout of the results table", {
  cb <- score_cb()
  s <- score(score_gold_tab(), score_result(score_output_tab(), cb))
  expect_equal(measure(s, "values")$matched, 47)
  expect_equal(measure(s, "rows")$matched, 5)
})

test_that("score() compares the variables both codebooks share and lists the others", {
  cb <- score_cb()
  cb_out <- suppressMessages(codebook(data.frame(
    variable = c("printed_page", "surname", "name", "age", "parish"),
    type = c("text", "text", "text", "number", "text"),
    level = c("page", "row", "row", "row", "row"), description = "x")))
  tab <- score_gold_tab()[, c("file", "row", "printed_page", "surname", "name", "age")]
  tab$parish <- "Sant Pere"
  s <- score(score_gold(cb = cb), score_result(tab, cb_out))
  expect_equal(s$by_variable$variable, c("printed_page", "surname", "name", "age"))
  expect_true(any(grepl("only in your benchmark\\): civil, occupation", s$notes)))
  expect_true(any(grepl("only in the output\\): parish", s$notes)))
  expect_true(any(grepl("age \\(integer, number", s$notes)))
  expect_equal(measure(s, "values")$matched, measure(s, "values")$compared)
})

test_that("score() checks its arguments in plain words", {
  cb <- score_cb()
  gold <- score_gold(cb = cb)
  res <- score_result(score_gold_tab(), cb)
  expect_error(score(res, gold), "goes first", class = "amanuensis_error_input")
  expect_error(score(gold, res, report = "accuracy.pdf"), "HTML", class = "amanuensis_error_input")
  expect_error(score(gold, res, align = "rows"), "align")
  expect_error(score(gold, res, normalize = "yes"), "normalize")
  expect_error(score(gold), "benchmark and the results", class = "amanuensis_error_input")
  expect_error(score(file.path(tempdir(), "no-such-gold.xlsx"), res), "Cannot find", class = "amanuensis_error_input")
  expect_error(score(list(1), res), "must be your benchmark", class = "amanuensis_error_input")
  undone <- score_gold(cb = cb, done = FALSE)
  expect_error(score(undone, res), "marked as done", class = "amanuensis_error_input")
})

test_that("score() works on the result of a real (mock) extraction", {
  withr::local_options(amanuensis.ask = FALSE)
  dir <- withr::local_tempdir()
  pdf <- file.path(dir, "register.pdf")
  grDevices::pdf(pdf, width = 4, height = 4)
  plot(1)
  plot(2)
  grDevices::dev.off()
  cb <- suppressMessages(codebook(data.frame(variable = c("folio", "surname", "age"), type = c("text", "text", "integer"),
                                             level = c("page", "row", "row"), description = "x")))
  pages <- suppressWarnings(suppressMessages(images(pdf, dpi = 40, save_to = file.path(dir, "pages"))))
  folder <- file.path(dir, "extraction")
  res <- suppressMessages(transcribe(pages, codebook = cb, model = "mock", save_to = folder))
  gold <- as.data.frame(res)
  s <- score(gold, res)
  expect_equal(measure(s, "values")$value, 1)
  expect_equal(measure(s, "rows")$value, 1)
  # a results folder works as the output, and as the reference run
  expect_equal(measure(score(gold, folder), "values")$value, 1)
  s_runs <- score(folder, res)
  expect_equal(attr(s_runs, "about")$reference, "run")
  expect_equal(measure(s_runs, "values")$value, 1)
  # plant one misreading in the model's reply of the second page
  info <- .result_info(res)
  key <- .page_key(gold$file[nrow(gold)])
  info$records[[key]]$data$records[[2]]$surname <- "surnam 2"
  s2 <- score(gold, res)
  expect_equal(nrow(s2$differences), 1)
  expect_equal(s2$differences$variable, "surname")
  expect_equal(s2$differences$edits, 1)
})

test_that("score() reads a benchmark file through benchmark()", {
  skip_if_not(exists("benchmark", mode = "function"), "benchmark() is not built yet")
  cb <- score_cb()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "gold.csv")
  .write_csv(score_gold_tab(), path)
  res <- score_result(score_gold_tab(), cb)
  s <- suppressMessages(score(path, res))
  expect_equal(measure(s, "values")$value, 1)
})

# ------- the report -----------------------------------------------------------

test_that("the report is one self-contained, deterministic HTML file", {
  cb <- score_cb()
  s <- score(score_gold(cb = cb), score_result(score_output_tab(), cb), normalize = TRUE)
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a.html")
  b <- file.path(dir, "b.html")
  expect_message(score(score_gold(cb = cb), score_result(score_output_tab(), cb), normalize = TRUE, report = a),
                 "Wrote the report")
  expect_message(.write_score_report(s, b), "Wrote the report")
  expect_equal(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
  expect_message(.write_score_report(s, b), "replaced")
  expect_equal(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
  html <- paste(readLines(b, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_false(grepl("@@", html, fixed = TRUE))
  expect_false(grepl("colspan|rowspan", html))
  expect_false(grepl("<script src|<link|https?://|src=\"", html))
  expect_true(grepl("prefers-color-scheme: dark", html, fixed = TRUE))
  expect_true(grepl("Show only differences", html, fixed = TRUE))
  # no <th> inside a table body
  bodies <- regmatches(html, gregexpr("(?s)<tbody>.*?</tbody>", html, perl = TRUE))[[1]]
  expect_true(length(bodies) > 0)
  expect_false(any(grepl("<th", bodies, fixed = TRUE)))
  # the wrong values are washed red and show the benchmark over the output
  expect_true(grepl("<div class=\"g\">jornalr</div>\n<div class=\"o\">jornal<mark class=\"o\">e</mark>r</div>", html, fixed = TRUE))
  expect_true(grepl("Ferr\u00e9", html, fixed = TRUE) || grepl("Ferr<mark class=\"g\">\u00e9</mark>", html, fixed = TRUE))
})

test_that("the report is built for free text, failed and not-run pages too", {
  dir <- withr::local_tempdir()
  cb <- codebook(free_text = TRUE)
  g <- data.frame(file = c("a.jpg", "b.jpg", "c.jpg"), text = c("En la villa de Vilanova", "a b c d", "x"),
                  stringsAsFactors = FALSE)
  o <- g[1:2, ]
  o$text <- c("En la vila de Vilanova", "a c d e f")
  s <- .score_units(score_units_ok(g, cb), score_units_ok(o, cb, failed = "b.jpg"), cb)
  attr(s, "about")$reference <- "benchmark"
  path <- file.path(dir, "text.html")
  suppressMessages(.write_score_report(s, path))
  html <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_true(grepl("Words right", html, fixed = TRUE))
  expect_true(grepl("<mark class=\"g\">villa</mark>", html, fixed = TRUE))
  expect_true(grepl("not in the results yet", html, fixed = TRUE))
  expect_true(grepl("<span class=\"miss\">missing</span>", html, fixed = TRUE))
  expect_false(grepl("@@", html, fixed = TRUE))
})

test_that("the template check fails loudly on a missing or repeated marker", {
  expect_error(.fill_template("<p>@@A@@ @@B@@</p>", list(A = "x")))
  expect_error(.fill_template("<p>@@A@@ @@A@@</p>", list(A = "x")))
  expect_equal(.fill_template("<p>@@A@@</p>", list(A = .html_escape("a@@b"))), "<p>a&#64;&#64;b</p>")
})
