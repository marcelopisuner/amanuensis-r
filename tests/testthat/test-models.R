test_that("gemini_models() lists the known models with a checked date and prints", {
  m <- gemini_models()
  expect_s3_class(m, "amanuensis_models")
  expect_identical(attr(m, "checked"), .models_checked)
  expect_true(all(c("gemini-3.8-flash", "gemini-4-argon", "gemini-3.7-flash") %in% m$model))
  expect_true(all(c("model", "status", "thinking", "max_output", "price_input", "price_output",
                    "price_cached", "next_from") %in% names(m)))
  expect_type(m$max_output, "integer")
  out <- capture.output(print(m))
  expect_match(out[1], "checked")
  expect_true(any(grepl("alias of gemini-3.8-flash", out)))
})

test_that(".model_info knows table models and returns NULL for others", {
  expect_identical(.model_info("gemini-4-argon")$max_output, 32768L)
  expect_null(.model_info("gemini-9-ultra"))
  expect_null(.model_info("mock"))
})

test_that(".thinking_for checks the level against the model", {
  expect_identical(.thinking_for("gemini-3.8-flash", "low"), "LOW")
  expect_identical(.thinking_for("gemini-3.6-flash", "minimal"), "MINIMAL")
  expect_error(.thinking_for("gemini-3.8-flash", "minimal"), class = "amanuensis_error",
               regexp = "does not accept")
  expect_error(.thinking_for("gemini-3.8-flash", "huge"), class = "amanuensis_error", regexp = "must be one of")
  # unknown models are not checked
  expect_identical(.thinking_for("gemini-9-ultra", "minimal"), "MINIMAL")
})

test_that(".price_for honours announced price changes, discounts and overrides", {
  before <- .price_for("gemini-3.8-flash", date = "2026-12-31")
  expect_identical(before, list(input = 0.75, output = 3.75, cached = 0.075))
  after <- .price_for("gemini-3.8-flash", date = "2027-01-01")
  expect_identical(after, list(input = 1.50, output = 7.50, cached = 0.15))
  half <- .price_for("gemini-3.8-flash", date = "2026-12-31", discount = TRUE)
  expect_identical(half, list(input = 0.375, output = 1.875, cached = 0.0375))
  # argon changes on its own date
  expect_identical(.price_for("gemini-4-argon", date = "2026-12-01")$input, 4.00)
  # a model with no announced change keeps its price
  expect_identical(.price_for("gemini-3.5-flash-lite", date = "2030-01-01")$input, 0.30)
  # user prices: cached defaults to the input price
  own <- .price_for("anything", prices = c(input = 1, output = 2))
  expect_identical(own, list(input = 1, output = 2, cached = 1))
  expect_identical(.price_for("anything", prices = c(input = 1, output = 2), discount = TRUE)$output, 1)
  expect_null(.price_for("gemini-9-ultra"))
})

test_that(".cost bills cached input at the cached price and thinking as output", {
  p <- list(input = 1, output = 10, cached = 0.1)
  tk <- list(input = 1000, cached = 400, output = 100, thinking = 50)
  expect_equal(.cost(tk, p), (600 * 1 + 400 * 0.1 + 150 * 10) / 1e6)
  expect_identical(.cost(tk, NULL), NA_real_)
  expect_equal(.cost(list(input = 1000), p), 1000 / 1e6)
})

test_that(".check_model explains aliases, previews and unknown models", {
  expect_message(.check_model("gemini-3.7-flash"), "old name.*gemini-3.8-flash")
  expect_message(.check_model("gemini-3.1-pro-preview"), "preview model")
  expect_message(.check_model("gemini-9-ultra"), "not in this package's model table")
  expect_message(.check_model("models/gemini-9-ultra"), "gemini-9-ultra")
  expect_silent(.check_model("gemini-3.8-flash"))
  expect_silent(.check_model("mock"))
  expect_error(.check_model(3), class = "amanuensis_error")
})

test_that("practice models run without the 'not in the model table' warning", {
  # Every practice model, not only "mock": model = "example" is the README's
  # first run, and the warning that costs cannot be estimated contradicted the
  # estimate printed at the end of the same run.
  for (m in .practice_models) {
    expect_silent(.check_model(m))
    expect_null(.model_info(m))
  }
  local_amanuensis()
  ex <- quietly(example_project(file.path(withr::local_tempdir(), "ex")))
  pages <- quietly(images(ex$scans, save_to = file.path(ex$folder, "pages")))
  msgs <- all_condition_text(transcribe(pages, ex$instructions, ex$codebook, model = "example",
                                            save_to = file.path(ex$folder, "extraction")))
  expect_false(any(grepl("model table", msgs)))
  expect_false(any(grepl("costs cannot be estimated", msgs)))
  expect_true(any(grepl("estimated cost", msgs)))
})

test_that("DESCRIPTION asks for an httr2 that has every argument the package uses", {
  # transcribe(workers > 1) calls httr2::req_perform_parallel(max_active =),
  # which httr2 added in 1.1.1; with 1.1.0 every parallel run stopped with
  # "unused argument". check_setup() reads its minimum versions from DESCRIPTION.
  need <- .setup_required_versions()[["httr2"]]
  expect_true(utils::compareVersion(need, "1.1.1") >= 0)
  expect_true("max_active" %in% names(formals(httr2::req_perform_parallel)))
})

test_that("prices without input and output names give no price rather than a broken one", {
  expect_null(.price_for("gemini-3.8-flash", prices = c(1, 2)))
  expect_identical(.price_for("gemini-3.8-flash", prices = c(input = 1, output = 2))$output, 2)
})
