# ---------------------------------------------------------------------------
# Coverage tests for R/regression_frame_flexsurv_selection.R
#
# Targets branches the Phase 6i tests don't reach:
#   * .flexsurv_coefs() non-95% ci_level branch (rebuilds CIs from se/z)
#   * .flexsurv_dist_title() switch arms + default fallback
#   * .check_flexsurv_available() / .check_sampleSelection_available()
#     missing-package aborts (mocked spicy_pkg_available)
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_flexsurv_cov <- function() {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")
  flexsurv::flexsurvreg(survival::Surv(time, status) ~ age + sex,
                        data = survival::lung, dist = "weibull")
}


# ---- 1. flexsurv: non-95% ci_level rebuilds CIs from est/se/z -----------

test_that("flexsurvreg: ci_level != 0.95 rebuilds CIs from est +/- z*se", {
  fit <- .fit_flexsurv_cov()
  fr <- as_regression_frame(fit, model_id = "M1", ci_level = 0.90)
  non_ref <- fr$coefs[!fr$coefs$is_ref, ]
  z_crit <- stats::qnorm(0.5 + 0.90 / 2)
  expect_equal(non_ref$ci_lower,
               non_ref$estimate - z_crit * non_ref$std_error,
               tolerance = 1e-12)
  expect_equal(non_ref$ci_upper,
               non_ref$estimate + z_crit * non_ref$std_error,
               tolerance = 1e-12)
  # A 90% interval is strictly narrower than the engine's hardcoded 95%.
  fr95 <- as_regression_frame(fit, model_id = "M1", ci_level = 0.95)
  nr95 <- fr95$coefs[!fr95$coefs$is_ref, ]
  width90 <- non_ref$ci_upper - non_ref$ci_lower
  width95 <- nr95$ci_upper - nr95$ci_lower
  expect_true(all(width90 < width95))
})

test_that("flexsurvreg: ci_level surfaced unchanged in info", {
  fit <- .fit_flexsurv_cov()
  fr <- as_regression_frame(fit, model_id = "M1", ci_level = 0.99)
  expect_identical(fr$info$ci_level, 0.99)
})


# ---- 2. .flexsurv_dist_title(): switch arms + default --------------------

test_that(".flexsurv_dist_title maps every named distribution", {
  expect_identical(spicy:::.flexsurv_dist_title("weibull"),     "Weibull")
  expect_identical(spicy:::.flexsurv_dist_title("weibullPH"),   "Weibull (PH)")
  expect_identical(spicy:::.flexsurv_dist_title("lognormal"),   "Log-normal")
  expect_identical(spicy:::.flexsurv_dist_title("lnorm"),       "Log-normal")
  expect_identical(spicy:::.flexsurv_dist_title("gompertz"),    "Gompertz")
  expect_identical(spicy:::.flexsurv_dist_title("gamma"),       "Gamma")
  expect_identical(spicy:::.flexsurv_dist_title("exponential"), "Exponential")
  expect_identical(spicy:::.flexsurv_dist_title("exp"),         "Exponential")
  expect_identical(spicy:::.flexsurv_dist_title("llogis"),      "Log-logistic")
  expect_identical(spicy:::.flexsurv_dist_title("gengamma"),    "Generalised gamma")
  expect_identical(spicy:::.flexsurv_dist_title("genf"),        "Generalised F")
})

test_that(".flexsurv_dist_title default capitalises an unknown distribution", {
  expect_identical(spicy:::.flexsurv_dist_title("royston"), "Royston")
  expect_identical(spicy:::.flexsurv_dist_title("spline"),  "Spline")
})


# ---- 3. Missing-package aborts (mocked availability check) ---------------

test_that(".check_flexsurv_available aborts with spicy_missing_pkg when flexsurv absent", {
  expect_error(
    testthat::with_mocked_bindings(
      spicy:::.check_flexsurv_available(),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    class = "spicy_missing_pkg"
  )
})

test_that(".check_sampleSelection_available aborts with spicy_missing_pkg when absent", {
  expect_error(
    testthat::with_mocked_bindings(
      spicy:::.check_sampleSelection_available(),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    class = "spicy_missing_pkg"
  )
})

test_that("as_regression_frame.flexsurvreg surfaces the missing-flexsurv abort message", {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")
  fit <- .fit_flexsurv_cov()
  expect_error(
    testthat::with_mocked_bindings(
      as_regression_frame(fit, model_id = "M1"),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    regexp = "flexsurv",
    class = "spicy_missing_pkg"
  )
})
