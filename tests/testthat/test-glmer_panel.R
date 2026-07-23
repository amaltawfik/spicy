# ---------------------------------------------------------------------------
# Phase 7c10 tests: glmer (non-Gaussian) panel + adjusted ICC.
#
# Three coupled fixes for glmer (binomial / Poisson):
#   (1) No "Residual" row in variance_components (lme4::sigma() returns
#       a fixed dispersion of 1 for binomial/poisson; reporting it as
#       a residual variance is misleading).
#   (2) merDeriv positions exclude the residual when family is not
#       Gaussian -- enables the panel to render with SE/CI populated.
#   (3) ICC uses link-scale distribution variance (Nakagawa 2017):
#       pi^2/3 for logit, 1 for probit, pi^2/6 for cloglog, lognormal
#       for Poisson(log).
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_glmer_bernoulli <- function() {
  skip_if_not_installed("lme4")
  set.seed(1)
  n <- 500
  g <- factor(rep(1:25, length.out = n))
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(0.5 + 0.8 * x + rnorm(25)[g]))
  lme4::glmer(y ~ x + (1 | g), family = binomial)
}

.fit_glmer_poisson <- function() {
  skip_if_not_installed("lme4")
  set.seed(2)
  n <- 300
  g <- factor(rep(1:15, length.out = n))
  x <- rnorm(n)
  y <- rpois(n, exp(1 + 0.3 * x + rnorm(15, 0, 0.3)[g]))
  lme4::glmer(y ~ x + (1 | g), family = poisson)
}

.fit_glmer_cbpp <- function() {
  skip_if_not_installed("lme4")
  suppressMessages(suppressWarnings(
    lme4::glmer(
      cbind(incidence, size - incidence) ~ period + (1 | herd),
      data = lme4::cbpp,
      family = binomial
    )
  ))
}

.fit_lmer_baseline <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}


# ---- 1. No Residual row for non-Gaussian merMod -------------------------

test_that("glmer (binomial Bernoulli): variance_components has no Residual row", {
  fit <- .fit_glmer_bernoulli()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  expect_false("Residual" %in% vc$group)
})

test_that("glmer (Poisson): variance_components has no Residual row", {
  fit <- .fit_glmer_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  expect_false("Residual" %in% vc$group)
})

test_that("glmer (cbpp cbind binomial): variance_components has no Residual row", {
  fit <- .fit_glmer_cbpp()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  expect_false("Residual" %in% vc$group)
})

test_that("lmer (Gaussian): variance_components STILL has a Residual row (regression guard)", {
  fit <- .fit_lmer_baseline()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  expect_true("Residual" %in% vc$group)
})


# ---- 2. Panel renders with SE/CI for glmer ------------------------------

test_that("glmer (Bernoulli) panel renders with SE/CI populated", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_glmer_bernoulli()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  # Variance row (intercept) should have finite SE/CI from merDeriv.
  var_rows <- vc[!(vc$is_correlation %in% TRUE), ]
  expect_true(all(is.finite(var_rows$std_error)))
  expect_true(all(var_rows$ci_method == "wald"))
})

test_that("glmer (Bernoulli) table_regression output uses structured panel", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_glmer_bernoulli()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Random effects (ML):", fixed = TRUE)
  expect_match(combined, "σ g (Intercept)", fixed = TRUE)
  # No "residual variance = 1.00" sentence anymore.
  expect_false(grepl("residual variance", combined, fixed = TRUE))
  expect_false(grepl("σ (Residual)", combined, fixed = TRUE))
})

test_that("glmer (cbpp cbind binomial) table_regression output uses structured panel", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_glmer_cbpp()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Random effects (ML)", fixed = TRUE)
  expect_match(combined, "σ herd (Intercept)", fixed = TRUE)
  expect_match(combined, "N (herd)", fixed = TRUE)
  # cbind binomial -> no ICC (gated; per-trial formula not in scope).
  expect_false(grepl("ICC", combined, fixed = TRUE))
})


# ---- 3. Adjusted ICC for non-Gaussian -----------------------------------

test_that("glmer (Bernoulli logit) ICC uses pi^2/3 (matches performance Adjusted ICC)", {
  skip_if_not_installed("performance")
  fit <- .fit_glmer_bernoulli()
  fr <- as_regression_frame(fit, model_id = "M1")
  icc_ours <- fr$info$random_effects$icc
  oracle <- performance::icc(fit)
  # performance$ICC_adjusted matches our formula var_r / (var_r + pi^2/3).
  expect_equal(icc_ours, as.numeric(oracle$ICC_adjusted), tolerance = 1e-6)
})

test_that("glmer (Poisson log) ICC uses lognormal var_d (matches performance Adjusted ICC)", {
  skip_if_not_installed("performance")
  fit <- .fit_glmer_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  icc_ours <- fr$info$random_effects$icc
  oracle <- performance::icc(fit)
  expect_equal(icc_ours, as.numeric(oracle$ICC_adjusted), tolerance = 1e-6)
})

test_that("glmer (cbpp cbind binomial) ICC is NA (gated -- formula deferred)", {
  fit <- .fit_glmer_cbpp()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.na(fr$info$random_effects$icc))
})

test_that("lmer (Gaussian) ICC unchanged: var_r / (var_r + sigma^2)", {
  fit <- .fit_lmer_baseline()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  var_r <- vc$variance[vc$group == "Subject"]
  var_e <- vc$variance[vc$group == "Residual"]
  expect_equal(
    fr$info$random_effects$icc,
    var_r / (var_r + var_e),
    tolerance = 1e-10
  )
})


# ---- 4. End-to-end: ICC row appears in the panel for non-Gaussian -------

test_that("glmer (Bernoulli) panel shows the ICC row", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_glmer_bernoulli()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "ICC", fixed = TRUE)
})

test_that("glmer (Poisson) panel shows the ICC row", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_glmer_poisson()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "ICC", fixed = TRUE)
})


# ---- 5. Link-distribution-variance helper unit tests --------------------

test_that(".merMod_link_distribution_variance returns pi^2/3 for binomial logit", {
  fit <- .fit_glmer_bernoulli()
  v <- spicy:::.merMod_link_distribution_variance(fit, var_random = 1)
  expect_equal(v, pi^2 / 3, tolerance = 1e-10)
})

test_that(".merMod_link_distribution_variance returns NA for cbind binomial", {
  fit <- .fit_glmer_cbpp()
  v <- spicy:::.merMod_link_distribution_variance(fit, var_random = 1)
  expect_true(is.na(v))
})

test_that(".merMod_link_distribution_variance returns NA for Gaussian (handled upstream)", {
  fit <- .fit_lmer_baseline()
  v <- spicy:::.merMod_link_distribution_variance(fit, var_random = 1)
  expect_true(is.na(v)) # Gaussian path uses sigma^2 from vc_df directly
})
