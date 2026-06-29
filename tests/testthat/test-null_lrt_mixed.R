# ---------------------------------------------------------------------------
# Phase 7c19 tests: LR test vs no-random-effects model
#
# Adds the single line every Stata `mixed` / `melogit` / `mepoisson` output
# ships at the bottom of the panel:
#
#   LR test vs linear regression: chibar^2(q) = 121.07, p < .001
#
# answering the publication-substantive question "are the random effects
# needed at all?". Statistic:
#
#   LRT = 2 * ( logLik(fit_full_ml) - logLik(fit_null) )
#
# where fit_null is an lm / glm (no random effects) fit on the same data.
# p_chibar^2 = 0.5 * pchisq(LRT, q, lower.tail = FALSE) following Stata's
# chi-bar-squared convention (Self & Liang 1987).
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_lmer_lrt <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_lmer_slope_lrt <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

.fit_glmer_lrt <- function() {
  skip_if_not_installed("lme4")
  set.seed(1)
  n <- 500; g <- factor(rep(1:25, length.out = n))
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(0.5 + 0.8 * x + rnorm(25)[g]))
  lme4::glmer(y ~ x + (1 | g), family = binomial)
}

.fit_glmmTMB_lrt <- function() {
  skip_if_not_installed("glmmTMB")
  d <- lme4::sleepstudy
  glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = d)
}

.fit_lme_lrt <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(distance ~ age, data = nlme::Orthodont,
             random = ~ 1 | Subject)
}


# ---- 1. info$random_effects$null_lrt populated per engine -------------

test_that("lmer: null_lrt populated with finite chi^2 + df + p_chibar2", {
  fit <- .fit_lmer_lrt()
  fr <- as_regression_frame(fit, model_id = "M1")
  lrt <- fr$info$random_effects$null_lrt
  expect_true(!is.null(lrt))
  expect_true(is.finite(lrt$chi2))
  expect_identical(as.integer(lrt$df), 1L)  # single variance (1|Subject)
  expect_true(is.finite(lrt$p_chibar2))
  expect_identical(lrt$family_label, "linear regression")
})

test_that("lmer (Days|Subject): df = 3 (2 variances + 1 covariance)", {
  fit <- .fit_lmer_slope_lrt()
  fr <- as_regression_frame(fit, model_id = "M1")
  lrt <- fr$info$random_effects$null_lrt
  expect_identical(as.integer(lrt$df), 3L)
})

test_that("glmer: null_lrt populated, family_label = 'logistic regression'", {
  fit <- .fit_glmer_lrt()
  fr <- as_regression_frame(fit, model_id = "M1")
  lrt <- fr$info$random_effects$null_lrt
  expect_true(!is.null(lrt))
  expect_true(is.finite(lrt$chi2))
  expect_identical(lrt$family_label, "logistic regression")
})

test_that("glmmTMB: null_lrt populated, family_label = 'linear regression'", {
  fit <- .fit_glmmTMB_lrt()
  fr <- as_regression_frame(fit, model_id = "M1")
  lrt <- fr$info$random_effects$null_lrt
  expect_true(!is.null(lrt))
  expect_true(is.finite(lrt$chi2))
  expect_identical(lrt$family_label, "linear regression")
})

test_that("lme: null_lrt populated, family_label = 'linear regression'", {
  fit <- .fit_lme_lrt()
  fr <- as_regression_frame(fit, model_id = "M1")
  lrt <- fr$info$random_effects$null_lrt
  expect_true(!is.null(lrt))
  expect_true(is.finite(lrt$chi2))
  expect_identical(lrt$family_label, "linear regression")
})


# ---- 2. p_chibar2 = 0.5 * pchisq(chi^2, df, upper-tail) ----------------

test_that("p_chibar2 = 0.5 * pchisq(chi2, df, lower.tail = FALSE)", {
  fit <- .fit_lmer_lrt()
  fr <- as_regression_frame(fit, model_id = "M1")
  lrt <- fr$info$random_effects$null_lrt
  expected_p <- 0.5 * stats::pchisq(lrt$chi2, df = lrt$df,
                                       lower.tail = FALSE)
  expect_equal(lrt$p_chibar2, expected_p, tolerance = 1e-12)
})


# ---- 3. lmer chi^2 matches manual 2 * (logLik_full - logLik_null) -----

test_that("lmer (REML) chi^2 matches manual 2*(logLik(refitML) - logLik(lm))", {
  fit <- .fit_lmer_lrt()
  fit_full_ml <- suppressMessages(lme4::refitML(fit))
  fit_null <- lm(Reaction ~ Days, data = lme4::sleepstudy)
  expected_chi2 <- 2 * (as.numeric(stats::logLik(fit_full_ml)) -
                          as.numeric(stats::logLik(fit_null)))
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$random_effects$null_lrt$chi2,
               expected_chi2, tolerance = 1e-10)
})


# ---- 4. End-to-end: LRT line appears at the bottom of the panel -------

test_that("table_regression(lmer) prints LRT line under N (group)", {
  fit <- .fit_lmer_lrt()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "LR test vs linear regression", fixed = TRUE)
  expect_match(combined, "χ̄²", fixed = TRUE)
})

test_that("table_regression(glmer) names 'logistic regression'", {
  fit <- .fit_glmer_lrt()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "LR test vs logistic regression", fixed = TRUE)
})

test_that("table_regression(lme) names 'linear regression'", {
  fit <- .fit_lme_lrt()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "LR test vs linear regression", fixed = TRUE)
})


# ---- 5. show_re = FALSE suppresses the LRT line ----------------------

test_that("show_re = FALSE suppresses both the panel AND the LRT line", {
  fit <- .fit_lmer_lrt()
  out <- capture.output(print(table_regression(fit, show_re = FALSE)))
  combined <- paste(out, collapse = "\n")
  expect_false(grepl("LR test vs", combined, fixed = TRUE))
})
