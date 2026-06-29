# ---------------------------------------------------------------------------
# Coverage tests for R/regression_frame_mlogit_betareg.R
#
# Targets branches the Phase 6h tests don't reach:
#   * .check_mlogit_available() / .check_betareg_available() missing-package
#     aborts (mocked spicy_pkg_available -> FALSE)
#   * .betareg_reference_rows() early return when the model has no factor
#     predictors (numeric-only formula -> length(fts) == 0L)
# ---------------------------------------------------------------------------


# ---- 1. Missing-package aborts (mocked availability check) ---------------

test_that(".check_mlogit_available aborts with spicy_missing_pkg when mlogit absent", {
  expect_error(
    testthat::with_mocked_bindings(
      spicy:::.check_mlogit_available(),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    class = "spicy_missing_pkg"
  )
})

test_that(".check_mlogit_available message points at install.packages(mlogit)", {
  err <- tryCatch(
    testthat::with_mocked_bindings(
      spicy:::.check_mlogit_available(),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "mlogit", fixed = TRUE)
  expect_match(conditionMessage(err), "install.packages", fixed = TRUE)
})

test_that(".check_betareg_available aborts with spicy_missing_pkg when betareg absent", {
  expect_error(
    testthat::with_mocked_bindings(
      spicy:::.check_betareg_available(),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    class = "spicy_missing_pkg"
  )
})

test_that(".check_betareg_available message points at install.packages(betareg)", {
  err <- tryCatch(
    testthat::with_mocked_bindings(
      spicy:::.check_betareg_available(),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "betareg", fixed = TRUE)
  expect_match(conditionMessage(err), "install.packages", fixed = TRUE)
})

test_that("as_regression_frame.betareg surfaces the missing-betareg abort message", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg", envir = environment())
  fit <- betareg::betareg(yield ~ temp, data = GasolineYield)
  expect_error(
    testthat::with_mocked_bindings(
      as_regression_frame(fit, model_id = "M1"),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    regexp = "betareg",
    class = "spicy_missing_pkg"
  )
})


# ---- 2. betareg with no factor predictors: empty reference-rows frame ----

test_that("betareg numeric-only formula synthesises no reference rows", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg", envir = environment())
  # yield ~ temp has no factor terms, so .betareg_reference_rows() takes
  # the length(fts) == 0L early return and contributes zero rows.
  fit <- betareg::betareg(yield ~ temp, data = GasolineYield)

  ref_rows <- spicy:::.betareg_reference_rows(fit)
  expect_identical(nrow(ref_rows), 0L)

  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(any(fr$coefs$is_ref))
  expect_setequal(fr$coefs$term, c("(Intercept)", "temp"))
})

test_that("betareg numeric-only frame is still schema-valid", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg", envir = environment())
  fit <- betareg::betareg(yield ~ temp, data = GasolineYield)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})
