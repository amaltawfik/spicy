# ---------------------------------------------------------------------------
# Phase 7c3 tests: ordinal-thresholds footer block for polr / clm.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_polr_th <- function() {
  skip_if_not_installed("MASS")
  data(housing, package = "MASS", envir = environment())
  MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing,
             Hess = TRUE)
}

.fit_clm_th <- function() {
  skip_if_not_installed("ordinal")
  ordinal::clm(rating ~ temp + contact, data = ordinal::wine)
}


# ---- 1. polr: thresholds line --------------------------------------------

test_that("ordinal thresholds footer fires for polr fits", {
  fit <- .fit_polr_th()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_ordinal_thresholds_footer_block_from_frames(list(fr))
  expect_match(out, "^Thresholds:")
  expect_match(out, "Low|Medium", fixed = TRUE)
  expect_match(out, "Medium|High", fixed = TRUE)
})

test_that("ordinal thresholds footer is silent for non-ordinal classes (lm)", {
  fr <- as_regression_frame(lm(mpg ~ wt, data = mtcars), model_id = "M1")
  out <- spicy:::build_ordinal_thresholds_footer_block_from_frames(list(fr))
  expect_null(out)
})


# ---- 2. clm: 4 thresholds inline ----------------------------------------

test_that("ordinal thresholds footer fires for clm fits", {
  fit <- .fit_clm_th()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_ordinal_thresholds_footer_block_from_frames(list(fr))
  expect_match(out, "^Thresholds:")
  expect_match(out, "1|2 =", fixed = TRUE)
  expect_match(out, "4|5 =", fixed = TRUE)
})


# ---- 3. End-to-end through table_regression() ---------------------------

test_that("table_regression() footer carries Thresholds for polr", {
  fit <- .fit_polr_th()
  combined <- paste(capture.output(print(table_regression(fit))),
                    collapse = "\n")
  expect_match(combined, "Thresholds:", fixed = TRUE)
})

test_that("table_regression() footer does NOT carry Thresholds for lm", {
  fit <- lm(mpg ~ wt, data = mtcars)
  combined <- paste(capture.output(print(table_regression(fit))),
                    collapse = "\n")
  expect_false(grepl("Thresholds:", combined, fixed = TRUE))
})


# ---- 4. Empty / multi-model -------------------------------------------

test_that("ordinal thresholds footer is NULL on an empty frames list", {
  expect_null(spicy:::build_ordinal_thresholds_footer_block_from_frames(list()))
})

test_that("ordinal thresholds footer prefixes Model k for 2+ contributing fits", {
  fr1 <- as_regression_frame(.fit_polr_th(), model_id = "M1")
  fr2 <- as_regression_frame(.fit_clm_th(),  model_id = "M2")
  out <- spicy:::build_ordinal_thresholds_footer_block_from_frames(list(fr1, fr2))
  expect_match(out, "Model 1:", fixed = TRUE)
  expect_match(out, "Model 2:", fixed = TRUE)
})

test_that("ordinal thresholds footer skips non-ordinal in mixed lists (no prefix)", {
  fr_polr <- as_regression_frame(.fit_polr_th(), model_id = "M1")
  fr_lm   <- as_regression_frame(lm(mpg ~ wt, data = mtcars), model_id = "M2")
  out <- spicy:::build_ordinal_thresholds_footer_block_from_frames(
    list(fr_polr, fr_lm))
  expect_match(out, "^Thresholds:")
  expect_false(grepl("Model 1:", out, fixed = TRUE))
})
