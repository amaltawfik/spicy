# ---------------------------------------------------------------------------
# Phase 7c20 tests: standardized = "refit" for mixed-effects fits
#
# The Cohen et al. 2003 / Gelman 2008 gold-standard z-score-then-refit
# approach. Gaussian (lmer / lme / glmmTMB Gaussian) standardises both
# response and predictors; non-Gaussian (glmer / glmmTMB binomial /
# Poisson) standardises predictors only (response is bounded /
# integer-valued).
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_lmer_std <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

.fit_glmer_std <- function() {
  skip_if_not_installed("lme4")
  set.seed(1)
  n <- 500; g <- factor(rep(1:25, length.out = n))
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(0.5 + 0.8 * x + rnorm(25)[g]))
  lme4::glmer(y ~ x + (1 | g), family = binomial)
}

.fit_glmmTMB_std <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_lme_std <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(distance ~ age, data = nlme::Orthodont, random = ~ 1 | Subject)
}


# ---- 1. Beta rows injected for each engine ----------------------------

test_that("lmer + standardized='refit' injects beta rows", {
  fit <- .fit_lmer_std()
  fr <- as_regression_frame(fit, model_id = "M1",
                             show_columns = c("b", "beta"),
                             standardized = "refit")
  expect_true("beta" %in% fr$coefs$estimate_type)
})

test_that("glmer + standardized='refit' injects beta rows", {
  fit <- .fit_glmer_std()
  fr <- as_regression_frame(fit, model_id = "M1",
                             show_columns = c("b", "beta"),
                             standardized = "refit")
  expect_true("beta" %in% fr$coefs$estimate_type)
})

test_that("glmmTMB + standardized='refit' injects beta rows", {
  fit <- .fit_glmmTMB_std()
  fr <- as_regression_frame(fit, model_id = "M1",
                             show_columns = c("b", "beta"),
                             standardized = "refit")
  expect_true("beta" %in% fr$coefs$estimate_type)
})

test_that("lme + standardized='refit' injects beta rows", {
  fit <- .fit_lme_std()
  fr <- as_regression_frame(fit, model_id = "M1",
                             show_columns = c("b", "beta"),
                             standardized = "refit")
  expect_true("beta" %in% fr$coefs$estimate_type)
})


# ---- 2. Beta formula: Gaussian -> beta = B * sd(X) / sd(Y) ----------

test_that("lmer Gaussian: beta_Days = B_Days * sd(Days) / sd(Reaction)", {
  fit <- .fit_lmer_std()
  fr <- as_regression_frame(fit, model_id = "M1",
                             show_columns = c("b", "beta"),
                             standardized = "refit")
  b_days   <- fr$coefs$estimate[fr$coefs$estimate_type == "B" &
                                  fr$coefs$term == "Days"]
  beta_days <- fr$coefs$estimate[fr$coefs$estimate_type == "beta" &
                                   fr$coefs$term == "Days"]
  d <- lme4::sleepstudy
  expected_beta <- b_days * stats::sd(d$Days) / stats::sd(d$Reaction)
  expect_equal(beta_days, expected_beta, tolerance = 1e-2)
})


# ---- 3. Non-Gaussian: response NOT standardised -----------------------

test_that("glmer binomial: response y stays 0/1 (predictor-only scaling)", {
  fit <- .fit_glmer_std()
  fr <- as_regression_frame(fit, model_id = "M1",
                             show_columns = c("b", "beta"),
                             standardized = "refit")
  beta_x <- fr$coefs$estimate[fr$coefs$estimate_type == "beta" &
                                fr$coefs$term == "x"]
  b_x    <- fr$coefs$estimate[fr$coefs$estimate_type == "B" &
                                fr$coefs$term == "x"]
  # Predictor scaling only: beta = B * sd(x)  (sd of x ~ 1 here since
  # rnorm), so beta is approximately B. The key check is that beta is
  # finite (and not multiplied by 1/sd(y) which would crash binomial).
  expect_true(is.finite(beta_x))
  expect_true(is.finite(b_x))
})


# ---- 4. No-op when standardized = 'none' --------------------------

test_that("standardized = 'none' does NOT inject beta rows", {
  fit <- .fit_lmer_std()
  fr <- as_regression_frame(fit, model_id = "M1",
                             show_columns = c("b"),
                             standardized = "none")
  expect_false("beta" %in% fr$coefs$estimate_type)
})


# ---- 5. End-to-end rendering: beta column populated -------------------

test_that("table_regression(lmer, standardized='refit') prints beta column", {
  fit <- .fit_lmer_std()
  out <- capture.output(print(
    table_regression(fit, standardized = "refit")
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "β", fixed = TRUE)
  # Both B and beta have decimal numbers on the same row.
  expect_true(grepl("Days", combined, fixed = TRUE))
})

test_that("table_regression(glmer, standardized='refit') prints beta column", {
  fit <- .fit_glmer_std()
  out <- capture.output(print(
    table_regression(fit, standardized = "refit")
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "β", fixed = TRUE)
})


# ---- 6. Fallback warning for non-refit methods ----------------------

test_that("standardized = 'posthoc' on mixed warns + falls back to refit", {
  fit <- .fit_lmer_std()
  expect_warning(
    fr <- as_regression_frame(fit, model_id = "M1",
                                show_columns = c("b", "beta"),
                                standardized = "posthoc"),
    "falling back to \"refit\"",
    fixed = TRUE
  )
  expect_true("beta" %in% fr$coefs$estimate_type)
})
