# The result object and the summary of a run: which page records belong to a
# result, the pages table, and a result rebuilt from its folder.

folder_with <- function(recs, contract) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  .save_contract(dir, contract)
  for (r in recs) .write_record(dir, r)
  dir
}

page_record <- function(file, contract, model = "gemini-3.8-flash", status = "ok", resolution = "ultra_high",
                        date = "2026-10-20T09:00:00Z", name = "Joan Puig") {
  list(file = file, source = file, source_page = 1L, status = status, contract = contract, model = model,
       settings = list(resolution = resolution, thinking = "low", engine = "sync"), date = date,
       cost_usd = 0.001, tokens = list(input = 2500, cached = 0, billed_output = 200),
       data = if (status != "failed") list(heading = "H", records = list(list(name = name, age = 40L, sex = "male"))))
}

test_that("a folder with practice and real results gives only the real ones", {
  local_amanuensis()
  con <- .contract(test_instructions(), roster_codebook())
  dir <- folder_with(list(page_record("p001.jpg", con$id),
                          page_record("p002.jpg", con$id, model = "mock", name = "name 1"),
                          page_record("p003.jpg", con$id, model = "example", name = "name 1")), con)
  msgs <- all_condition_text(res <- .result_from_folder(dir))
  expect_identical(unique(res$file), "p001.jpg")
  p <- .result_info(res)$pages
  expect_identical(p$status, c("ok", "not run", "not run"))
  expect_match(p$message[2], "practice")
  expect_identical(.result_info(res)$settings$model, "gemini-3.8-flash")
  expect_true(any(grepl("2 pages", msgs)))
  # only practice results: those are the result
  dir2 <- folder_with(list(page_record("p001.jpg", con$id, model = "mock")), con)
  expect_identical(.result_info(.result_from_folder(dir2))$pages$status, "ok")
})

test_that("a folder with results made with two settings gives the newest settings only", {
  local_amanuensis()
  con <- .contract(test_instructions(), roster_codebook())
  dir <- folder_with(list(page_record("p001.jpg", con$id, resolution = "high", date = "2026-10-21T09:00:00Z"),
                          page_record("p002.jpg", con$id, date = "2026-10-20T09:00:00Z"),
                          page_record("p003.jpg", con$id, status = "failed", date = "2026-10-19T09:00:00Z")), con)
  msgs <- all_condition_text(res <- .result_from_folder(dir))
  expect_identical(unique(res$file), "p001.jpg")
  p <- .result_info(res)$pages
  expect_identical(p$status, c("ok", "not run", "failed"))
  expect_match(p$message[2], "resolution ultra_high")
  expect_identical(.result_info(res)$settings$resolution, "high")
  expect_true(any(grepl("other settings", msgs)))
})

test_that("practice-only results under other instructions do not block a folder of real ones", {
  local_amanuensis()
  con <- .contract(test_instructions(), roster_codebook())
  old <- .contract(test_instructions("Practice."), roster_codebook())
  dir <- folder_with(list(page_record("p001.jpg", con$id), page_record("p002.jpg", old$id, model = "mock")), con)
  .save_contract(dir, old)
  res <- quietly(.result_from_folder(dir))
  expect_identical(unique(res$file), "p001.jpg")
})

test_that("canonical model names follow the model table's routes", {
  expect_identical(.canonical_model(c("gemini-3.7-flash", "models/gemini-3.5-flash", "gemini-3.8-flash", "mock", "x")),
                   c("gemini-3.8-flash", "gemini-3.6-flash", "gemini-3.8-flash", "mock", "x"))
  expect_identical(.fingerprint("c", "gemini-3.7-flash", "high", "low"), .fingerprint("c", "gemini-3.8-flash", "high", "low"))
})
