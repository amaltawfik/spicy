# ---------------------------------------------------------------------------
# Phase 7c7b tests:
#  - .re_components_on_scale() converts variance <-> SD via Delta-method
#  - Correlation rows (`is_correlation == TRUE`) added for 3 mixed classes
#  - Correlation rows pass through scale conversion unchanged (rho unitless)
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_lmer_slope_7c7b <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

.fit_glmmTMB_slope_7c7b <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

.fit_lme_slope_7c7b <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ age | Subject
  )
}


# ---- 1. .re_components_on_scale() variance -> SD --------------------------

test_that(".re_components_on_scale: sqrt() transform on variance + sd cols", {
  vc <- data.frame(
    group = c("Subject", "Residual"),
    term = c("(Intercept)", ""),
    variance = c(1378.18, 960.46),
    sd = c(37.12, 30.99),
    corr = NA_real_,
    is_correlation = c(FALSE, FALSE),
    std_error = c(505.77, 107.05),
    ci_lower = c(386.89, 750.65),
    ci_upper = c(2369.47, 1170.27),
    ci_method = c("wald", "wald"),
    stringsAsFactors = FALSE
  )
  sd_scaled <- spicy:::.re_components_on_scale(vc, "sd")
  # Estimate column becomes sqrt() of variance
  expect_equal(sd_scaled$variance, sqrt(vc$variance), tolerance = 1e-10)
  expect_equal(sd_scaled$sd, sqrt(vc$variance), tolerance = 1e-10)
  # CI by sqrt (monotonic)
  expect_equal(sd_scaled$ci_lower, sqrt(vc$ci_lower), tolerance = 1e-10)
  expect_equal(sd_scaled$ci_upper, sqrt(vc$ci_upper), tolerance = 1e-10)
  # SE via Delta-method: SE_sd = SE_var / (2 * sd)
  expect_equal(
    sd_scaled$std_error,
    vc$std_error / (2 * sd_scaled$sd),
    tolerance = 1e-10
  )
})

test_that(".re_components_on_scale: variance scale is identity", {
  vc <- data.frame(
    group = "Subject",
    term = "(Intercept)",
    variance = 100,
    sd = 10,
    corr = NA_real_,
    is_correlation = FALSE,
    std_error = 20,
    ci_lower = 60,
    ci_upper = 140,
    ci_method = "wald",
    stringsAsFactors = FALSE
  )
  out <- spicy:::.re_components_on_scale(vc, "variance")
  expect_identical(out, vc)
})

test_that(".re_components_on_scale: correlation rows pass through unchanged", {
  vc <- data.frame(
    group = c("Subject", "Subject"),
    term = c("(Intercept)", "(Intercept), Days"),
    variance = c(612, NA),
    sd = c(24.7, NA),
    corr = c(NA, 0.07),
    is_correlation = c(FALSE, TRUE),
    std_error = c(289, 0.27),
    ci_lower = c(46, -0.49),
    ci_upper = c(1178, 0.59),
    ci_method = c("wald", "wald"),
    stringsAsFactors = FALSE
  )
  out <- spicy:::.re_components_on_scale(vc, "sd")
  # Row 1 (variance) gets converted
  expect_equal(out$sd[1L], sqrt(612), tolerance = 1e-10)
  # Row 2 (correlation) unchanged
  expect_identical(out$corr[2L], vc$corr[2L])
  expect_identical(out$std_error[2L], vc$std_error[2L])
  expect_identical(out$ci_lower[2L], vc$ci_lower[2L])
  expect_identical(out$ci_upper[2L], vc$ci_upper[2L])
})

test_that(".re_components_on_scale: NA-safe for missing SE columns", {
  vc <- data.frame(
    group = "Subject",
    term = "(Intercept)",
    variance = 100,
    sd = 10,
    corr = NA_real_,
    stringsAsFactors = FALSE
  )
  out <- spicy:::.re_components_on_scale(vc, "sd")
  expect_equal(out$variance, 10, tolerance = 1e-10)
})


# ---- 2. Correlation rows: lme ------------------------------------------

test_that("lme random slope: correlation row appended with point estimate", {
  fit <- .fit_lme_slope_7c7b()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  corr_rows <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(corr_rows), 1L)
  expect_match(corr_rows$term, "Intercept", fixed = TRUE)
  expect_match(corr_rows$term, "age", fixed = TRUE)
  expect_true(is.finite(corr_rows$corr))
})

test_that("lme correlation row: SE / CI populated via intervals()", {
  fit <- .fit_lme_slope_7c7b()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  corr_rows <- vc[vc$is_correlation %in% TRUE, ]
  expect_true(is.finite(corr_rows$std_error))
  expect_true(is.finite(corr_rows$ci_lower))
  expect_true(is.finite(corr_rows$ci_upper))
  # Wald CI brackets the point estimate
  expect_true(corr_rows$ci_lower <= corr_rows$corr)
  expect_true(corr_rows$corr <= corr_rows$ci_upper)
})


# ---- 3. Correlation rows: glmmTMB ---------------------------------------

test_that("glmmTMB random slope: correlation row with SE/CI", {
  fit <- .fit_glmmTMB_slope_7c7b()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  corr_rows <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(corr_rows), 1L)
  expect_true(is.finite(corr_rows$corr))
  expect_true(is.finite(corr_rows$std_error))
  expect_true(is.finite(corr_rows$ci_lower))
  expect_true(is.finite(corr_rows$ci_upper))
})


# ---- 4. Correlation rows: lme4 (point estimate only) -------------------

test_that("lme4 random slope: correlation row appended with SE / CI", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_slope_7c7b()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  corr_rows <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(corr_rows), 1L)
  expect_true(is.finite(corr_rows$corr))
  # Phase 7c17: multivariate Delta-method on merDeriv vcov populates
  # SE + CI on the rho row -- the Phase 7c7b NA placeholder is gone.
  expect_true(is.finite(corr_rows$std_error))
  expect_true(is.finite(corr_rows$ci_lower))
  expect_true(is.finite(corr_rows$ci_upper))
  expect_identical(corr_rows$ci_method, "wald")
})


# ---- 5. Variance row SE/CI still populated when correlation rows present

test_that("lme4 random slope: variance rows still have SE/CI from merDeriv", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_slope_7c7b()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  var_rows <- vc[!(vc$is_correlation %in% TRUE), ]
  # 2 group variance rows + 1 residual variance row = 3
  expect_identical(nrow(var_rows), 3L)
  expect_true(all(is.finite(var_rows$std_error)))
})


# ---- 6. Validator accepts the extended schema (is_correlation column) --

test_that("validator accepts schema with is_correlation column", {
  fit <- .fit_lme_slope_7c7b()
  fr <- as_regression_frame(fit)
  expect_invisible(spicy:::validate_regression_frame(fr))
})


# ---- 7. End-to-end scale conversion of a real frame --------------------

test_that("scale conversion applied to a real lme frame matches oracle SD", {
  fit <- .fit_lme_slope_7c7b()
  fr <- as_regression_frame(fit)
  vc_sd <- spicy:::.re_components_on_scale(
    fr$info$random_effects$variance_components,
    "sd"
  )
  ci <- nlme::intervals(fit, which = "var-cov")$reStruct$Subject
  intercept_row <- vc_sd[
    vc_sd$group == "Subject" &
      vc_sd$term == "(Intercept)" &
      !(vc_sd$is_correlation %in% TRUE),
  ]
  expect_equal(
    intercept_row$ci_lower,
    unname(ci["sd((Intercept))", "lower"]),
    tolerance = 1e-10
  )
  expect_equal(
    intercept_row$ci_upper,
    unname(ci["sd((Intercept))", "upper"]),
    tolerance = 1e-10
  )
})
