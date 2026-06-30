# ---------------------------------------------------------------------------
# Phase 7c16 tests: exponentiate = TRUE on mixed-effects fits.
# Covers the exp() transform on B / beta rows for non-identity links
# (glmer logit -> OR, glmer Poisson -> IRR, glmmTMB binomial -> OR,
# glmmTMB poisson -> IRR), the column-header rebrand, the footer note,
# and the AME-pass-through semantics (AME rows are already on the
# response scale and must NOT be re-exponentiated).
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_glmer_logit <- function() {
  skip_if_not_installed("lme4")
  set.seed(1)
  n <- 500; g <- factor(rep(1:25, length.out = n))
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(0.5 + 0.8 * x + rnorm(25)[g]))
  lme4::glmer(y ~ x + (1 | g), family = binomial)
}

.fit_glmer_poisson <- function() {
  skip_if_not_installed("lme4")
  set.seed(2)
  n <- 300; g <- factor(rep(1:15, length.out = n))
  x <- rnorm(n)
  y <- rpois(n, exp(1 + 0.3 * x + rnorm(15, 0, 0.3)[g]))
  lme4::glmer(y ~ x + (1 | g), family = poisson)
}

.fit_glmmTMB_logit <- function() {
  skip_if_not_installed("glmmTMB")
  set.seed(1)
  n <- 500; g <- factor(rep(1:25, length.out = n))
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(0.5 + 0.8 * x + rnorm(25)[g]))
  d <- data.frame(y = y, x = x, g = g)
  glmmTMB::glmmTMB(y ~ x + (1 | g), data = d, family = binomial)
}

.fit_lmer_gaussian <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}


# ---- 1. Schema: exp() applied to B-rows for non-identity link -----------

test_that("glmer logit + exponentiate: B-rows are exp() of link-scale coefs", {
  fit <- .fit_glmer_logit()
  fr_raw <- as_regression_frame(fit, model_id = "M1")
  fr_exp <- as_regression_frame(fit, model_id = "M1", exponentiate = TRUE)
  b_rows_raw <- fr_raw$coefs[fr_raw$coefs$estimate_type == "B", ]
  b_rows_exp <- fr_exp$coefs[fr_exp$coefs$estimate_type == "B", ]
  expect_equal(b_rows_exp$estimate, exp(b_rows_raw$estimate), tolerance = 1e-12)
  expect_equal(b_rows_exp$ci_lower, exp(b_rows_raw$ci_lower), tolerance = 1e-12)
  expect_equal(b_rows_exp$ci_upper, exp(b_rows_raw$ci_upper), tolerance = 1e-12)
})

test_that("glmer logit + exponentiate: SE follows delta-method (exp(B) * SE_link)", {
  fit <- .fit_glmer_logit()
  fr_raw <- as_regression_frame(fit, model_id = "M1")
  fr_exp <- as_regression_frame(fit, model_id = "M1", exponentiate = TRUE)
  b_rows_raw <- fr_raw$coefs[fr_raw$coefs$estimate_type == "B", ]
  b_rows_exp <- fr_exp$coefs[fr_exp$coefs$estimate_type == "B", ]
  expected_se <- exp(b_rows_raw$estimate) * b_rows_raw$std_error
  expect_equal(b_rows_exp$std_error, expected_se, tolerance = 1e-12)
})

test_that("glmer Poisson + exponentiate: B-rows are exp() of link-scale coefs", {
  fit <- .fit_glmer_poisson()
  fr_raw <- as_regression_frame(fit, model_id = "M1")
  fr_exp <- as_regression_frame(fit, model_id = "M1", exponentiate = TRUE)
  b_rows_raw <- fr_raw$coefs[fr_raw$coefs$estimate_type == "B", ]
  b_rows_exp <- fr_exp$coefs[fr_exp$coefs$estimate_type == "B", ]
  expect_equal(b_rows_exp$estimate, exp(b_rows_raw$estimate), tolerance = 1e-12)
})

test_that("glmmTMB logit + exponentiate: B-rows are exp() of link-scale coefs", {
  fit <- .fit_glmmTMB_logit()
  fr_raw <- as_regression_frame(fit, model_id = "M1")
  fr_exp <- as_regression_frame(fit, model_id = "M1", exponentiate = TRUE)
  b_rows_raw <- fr_raw$coefs[fr_raw$coefs$estimate_type == "B", ]
  b_rows_exp <- fr_exp$coefs[fr_exp$coefs$estimate_type == "B", ]
  expect_equal(b_rows_exp$estimate, exp(b_rows_raw$estimate), tolerance = 1e-12)
})


# ---- 2. Inference invariants ---------------------------------------------

test_that("exponentiate: z-statistic and p-value unchanged", {
  fit <- .fit_glmer_logit()
  fr_raw <- as_regression_frame(fit, model_id = "M1")
  fr_exp <- as_regression_frame(fit, model_id = "M1", exponentiate = TRUE)
  b_raw <- fr_raw$coefs[fr_raw$coefs$estimate_type == "B", ]
  b_exp <- fr_exp$coefs[fr_exp$coefs$estimate_type == "B", ]
  expect_equal(b_exp$statistic, b_raw$statistic, tolerance = 1e-12)
  expect_equal(b_exp$p_value,   b_raw$p_value,   tolerance = 1e-12)
})


# ---- 3. AME pass-through: AME rows NOT re-exponentiated -----------------

test_that("exponentiate does NOT re-exponentiate AME rows (already response-scale)", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_glmer_logit()
  fr_no_exp <- as_regression_frame(fit, model_id = "M1",
                                    show_columns = c("b", "ame"),
                                    exponentiate = FALSE)
  fr_exp    <- as_regression_frame(fit, model_id = "M1",
                                    show_columns = c("b", "ame"),
                                    exponentiate = TRUE)
  ame_no_exp <- fr_no_exp$coefs[fr_no_exp$coefs$estimate_type == "ame", ]
  ame_exp    <- fr_exp$coefs[fr_exp$coefs$estimate_type == "ame", ]
  expect_equal(ame_exp$estimate,  ame_no_exp$estimate,  tolerance = 1e-12)
  expect_equal(ame_exp$std_error, ame_no_exp$std_error, tolerance = 1e-12)
})


# ---- 4. Info extras: exp_applied + exp_header set when transform fires --

test_that("glmer logit: info$extras$exp_header = 'OR' when exponentiated", {
  fit <- .fit_glmer_logit()
  fr <- as_regression_frame(fit, model_id = "M1", exponentiate = TRUE)
  expect_true(isTRUE(fr$info$extras$exp_applied))
  expect_identical(fr$info$extras$exp_header, "OR")
})

test_that("glmer Poisson: info$extras$exp_header = 'IRR' when exponentiated", {
  fit <- .fit_glmer_poisson()
  fr <- as_regression_frame(fit, model_id = "M1", exponentiate = TRUE)
  expect_true(isTRUE(fr$info$extras$exp_applied))
  expect_identical(fr$info$extras$exp_header, "IRR")
})

test_that("glmmTMB logit: info$extras$exp_header = 'OR'", {
  fit <- .fit_glmmTMB_logit()
  fr <- as_regression_frame(fit, model_id = "M1", exponentiate = TRUE)
  expect_true(isTRUE(fr$info$extras$exp_applied))
  expect_identical(fr$info$extras$exp_header, "OR")
})


# ---- 5. Identity links: exponentiate is a no-op -------------------------

test_that("lmer Gaussian: exponentiate is a no-op on B-rows", {
  fit <- .fit_lmer_gaussian()
  fr_raw <- as_regression_frame(fit, model_id = "M1")
  fr_exp <- as_regression_frame(fit, model_id = "M1", exponentiate = TRUE)
  expect_equal(fr_exp$coefs$estimate,  fr_raw$coefs$estimate,  tolerance = 1e-12)
  expect_equal(fr_exp$coefs$std_error, fr_raw$coefs$std_error, tolerance = 1e-12)
})

test_that("lmer Gaussian: exp_applied stays FALSE (no footer note)", {
  fit <- .fit_lmer_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1", exponentiate = TRUE)
  expect_false(isTRUE(fr$info$extras$exp_applied))
})


# ---- 6. End-to-end: column rebrand + footer note ------------------------

test_that("table_regression(glmer, exponentiate = TRUE) renames B column to OR", {
  fit <- .fit_glmer_logit()
  out <- capture.output(print(
    table_regression(fit, exponentiate = TRUE)
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "OR", fixed = TRUE)
  expect_match(combined, "OR = odds ratio", fixed = TRUE)
  expect_match(combined,
                "Coefficients exponentiated and displayed as OR",
                fixed = TRUE)
})

test_that("table_regression(glmer Poisson, exponentiate = TRUE) renames B to IRR", {
  fit <- .fit_glmer_poisson()
  out <- capture.output(print(
    table_regression(fit, exponentiate = TRUE)
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "IRR",                       fixed = TRUE)
  expect_match(combined, "IRR = incidence rate ratio", fixed = TRUE)
})


# ---- 7. spurious-warn guard ---------------------------------------------

test_that("no spurious 'no effect' warning for non-identity mixed fits", {
  fit <- .fit_glmer_logit()
  expect_no_warning(
    table_regression(fit, exponentiate = TRUE),
    message = "no effect on identity-link"
  )
})

test_that("the 'no effect' warning DOES fire for identity-link mixed fits", {
  fit <- .fit_lmer_gaussian()
  expect_warning(
    table_regression(fit, exponentiate = TRUE),
    "no effect on identity-link"
  )
})
