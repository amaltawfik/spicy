# ---------------------------------------------------------------------------
# Coverage tests for R/regression_frame_estimatr.R.
#
# Targets the regions not exercised by test-regression_frame_estimatr.R:
#   * .check_estimatr_available() missing-pkg abort (lines 70-76)
#   * .estimatr_coefs() CI rebuild when ci_level != engine alpha (lines 122-124)
#   * .estimatr_reference_rows() skip + empty-return paths when a factor
#     term has no dropped reference level (lines 170, 192)
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_lm_robust_cov <- function() {
  skip_if_not_installed("estimatr")
  estimatr::lm_robust(mpg ~ wt + cyl, data = mtcars)
}

# No-intercept factor model: `y ~ 0 + f` fits ALL levels, so the first
# level is NOT dropped. detect_factor_terms() flags reference_dropped =
# FALSE, which drives .estimatr_reference_rows() through the `next` skip
# (line 170) and the empty-return guard (line 192).
.fit_lm_robust_no_intercept_factor <- function() {
  skip_if_not_installed("estimatr")
  d <- mtcars
  d$cyl_f <- factor(d$cyl, labels = c("4cyl", "6cyl", "8cyl"))
  estimatr::lm_robust(mpg ~ 0 + cyl_f, data = d)
}


# ---- 1. Missing-package abort (mocked availability check) ----------------

test_that(".check_estimatr_available aborts with spicy_missing_pkg when absent", {
  expect_error(
    testthat::with_mocked_bindings(
      spicy:::.check_estimatr_available(),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    class = "spicy_missing_pkg"
  )
})

test_that("as_regression_frame.lm_robust surfaces the missing-estimatr abort", {
  fit <- .fit_lm_robust_cov()
  expect_error(
    testthat::with_mocked_bindings(
      as_regression_frame(fit, model_id = "M1"),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    regexp = "estimatr",
    class = "spicy_missing_pkg"
  )
})

test_that("as_regression_frame.iv_robust surfaces the missing-estimatr abort", {
  skip_if_not_installed("estimatr")
  fit <- estimatr::iv_robust(mpg ~ wt | hp, data = mtcars)
  expect_error(
    testthat::with_mocked_bindings(
      as_regression_frame(fit, model_id = "M1"),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    regexp = "estimatr",
    class = "spicy_missing_pkg"
  )
})


# ---- 2. CI rebuild when ci_level differs from the engine alpha -----------

test_that("lm_robust ci_level=0.90 rebuilds CI from est/se/df (Wald-t)", {
  fit <- .fit_lm_robust_cov()
  fr <- as_regression_frame(fit, model_id = "M1", ci_level = 0.90)

  expect_equal(fr$info$ci_level, 0.90)

  sm <- summary(fit)$coefficients
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(sm)) {
    est <- unname(stats::coef(fit)[nm])
    se  <- unname(sm[nm, "Std. Error"])
    df  <- unname(sm[nm, "DF"])
    t_crit <- stats::qt(0.5 + 0.90 / 2, df = df)
    expect_equal(b_rows$ci_lower[b_rows$term == nm], est - t_crit * se,
                 tolerance = 1e-10, info = nm)
    expect_equal(b_rows$ci_upper[b_rows$term == nm], est + t_crit * se,
                 tolerance = 1e-10, info = nm)
  }
})

test_that("lm_robust ci_level=0.90 yields a narrower CI than the engine 95%", {
  fit <- .fit_lm_robust_cov()
  fr90 <- as_regression_frame(fit, model_id = "M1", ci_level = 0.90)

  sm <- summary(fit)$coefficients
  b_rows <- fr90$coefs[!fr90$coefs$is_ref, ]
  for (nm in rownames(sm)) {
    width90 <- b_rows$ci_upper[b_rows$term == nm] -
               b_rows$ci_lower[b_rows$term == nm]
    width95 <- sm[nm, "CI Upper"] - sm[nm, "CI Lower"]
    expect_lt(width90, width95)
  }
})


# ---- 3. Reference rows: factor with no dropped reference -----------------

test_that("lm_robust no-intercept factor synthesises NO reference row", {
  fit <- .fit_lm_robust_no_intercept_factor()

  # detect_factor_terms() must flag the factor as reference_dropped = FALSE,
  # otherwise this test would not exercise the skip + empty-return paths.
  fts <- spicy:::detect_factor_terms(fit)
  expect_true(length(fts) >= 1L)
  expect_false(isTRUE(fts[[1]]$reference_dropped))

  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  # All three levels appear as real coefficients; none is a synthesised ref.
  expect_false(any(fr$coefs$is_ref))
  expect_identical(nrow(fr$coefs), 3L)
})

test_that(".estimatr_reference_rows returns an empty frame for an all-fitted factor", {
  fit <- .fit_lm_robust_no_intercept_factor()
  rows <- spicy:::.estimatr_reference_rows(fit)
  expect_identical(nrow(rows), 0L)
})
