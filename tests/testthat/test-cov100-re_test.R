# Coverage-gap tests for R/regression_re_test.R: per-term random-effect
# LR-test edge paths the main test-re_test.R suite does not reach --
# refit-failure warnings (lmer + lme), dropping one of several
# intercept-only bars (q = 1, chi2_0 point-mass branch), the nlme
# `random = ~ terms` shorthand, the unsupported-lme-structure guard,
# and the RLRsim missing-package abort.

test_that("lrt: a failing reduced refit warns spicy_fallback and leaves the term untested", {
  skip_if_not_installed("lme4")
  # (0 + Days | Subject): dropping the bar's only slope leaves
  # `(0 | Subject)`, which lme4 cannot fit (no random-effect
  # parameters left) -> the reduced refit fails.
  fit <- lme4::lmer(Reaction ~ Days + (0 + Days | Subject),
                    data = lme4::sleepstudy)
  expect_warning(
    res <- spicy:::.re_term_tests_lrt(fit),
    regexp = "reduced refit for random term `Days` (Subject) failed",
    fixed = TRUE,
    class = "spicy_fallback"
  )
  # the model's only testable term failed -> nothing to report
  expect_null(res)
})

test_that("lrt: dropping one of two intercept-only bars uses q = 1 (chibar2 with chi2_0 mass)", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(diameter ~ 1 + (1 | plate) + (1 | sample),
                    data = lme4::Penicillin)
  res <- spicy:::.re_term_tests_lrt(fit)
  expect_identical(res$group, c("plate", "sample"))
  expect_identical(res$term, c("(Intercept)", "(Intercept)"))
  expect_identical(res$df, c(1, 1))
  # oracle: hand REML LRT against directly refitted one-bar models
  red_plate  <- lme4::lmer(diameter ~ 1 + (1 | sample),
                           data = lme4::Penicillin)
  red_sample <- lme4::lmer(diameter ~ 1 + (1 | plate),
                           data = lme4::Penicillin)
  chi_plate  <- 2 * (as.numeric(stats::logLik(fit)) -
                       as.numeric(stats::logLik(red_plate)))
  chi_sample <- 2 * (as.numeric(stats::logLik(fit)) -
                       as.numeric(stats::logLik(red_sample)))
  expect_equal(res$statistic, c(chi_plate, chi_sample), tolerance = 1e-6)
  # q = 1: p = 0.5 * P(chi2_1 > x) + 0.5 * chi2_0 point mass at 0 (= 0 here)
  expect_equal(
    res$p_value,
    0.5 * stats::pchisq(c(chi_plate, chi_sample), df = 1,
                        lower.tail = FALSE),
    tolerance = 1e-6
  )
})

test_that("lme lrt: a non-formula `random =` structure warns unsupported, rows stay untested", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(distance ~ age, data = nlme::Orthodont,
                   random = list(Subject = ~ 1))
  expect_warning(
    res <- spicy:::.re_term_tests_lrt_lme(fit),
    regexp = "support only a simple",
    class = "spicy_fallback"
  )
  expect_null(res)
})

test_that("lme lrt: the `random = ~ terms` shorthand resolves the group and reuses the null LRT", {
  skip_if_not_installed("nlme")
  # Orthodont is groupedData, so `random = ~ 1` takes the group
  # (Subject) from the fitted object, not the formula.
  fit <- nlme::lme(distance ~ age, data = nlme::Orthodont, random = ~ 1)
  res <- spicy:::.re_term_tests_lrt_lme(fit)
  expect_identical(res$group, "Subject")
  expect_identical(res$term, "(Intercept)")
  expect_identical(res$df, 1)
  # oracle: whole-block LRT = ML refit vs plain lm, chibar2(0, 1) p
  full_ml <- nlme::lme(distance ~ age, data = nlme::Orthodont,
                       random = ~ 1 | Subject, method = "ML")
  null_lm <- stats::lm(distance ~ age, data = nlme::Orthodont)
  chi2_h <- 2 * (as.numeric(stats::logLik(full_ml)) -
                   as.numeric(stats::logLik(null_lm)))
  expect_equal(res$statistic, chi2_h, tolerance = 1e-6)
  expect_equal(
    res$p_value,
    0.5 * stats::pchisq(chi2_h, df = 1, lower.tail = FALSE),
    tolerance = 1e-6
  )
})

test_that("lme lrt: a failing reduced refit warns spicy_fallback and returns nothing", {
  skip_if_not_installed("nlme")
  # The data object exists only inside local(): the reduced refit
  # re-evaluates the captured lme call in the caller's frame, where
  # `dat_hidden` no longer resolves -> the refit fails, the row's
  # test columns stay NA.
  fit <- local({
    dat_hidden <- as.data.frame(nlme::Orthodont)
    nlme::lme(distance ~ age, data = dat_hidden, random = ~ age | Subject)
  })
  expect_warning(
    res <- spicy:::.re_term_tests_lrt_lme(fit),
    regexp = "reduced refit for random term `age` (Subject) failed",
    fixed = TRUE,
    class = "spicy_fallback"
  )
  expect_null(res)
})

test_that("rlrt aborts with spicy_missing_pkg when RLRsim is not installed", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject),
                    data = lme4::sleepstudy)
  testthat::local_mocked_bindings(spicy_pkg_available = function(pkg) FALSE)
  expect_error(
    spicy:::.re_term_tests_rlrt(fit),
    regexp = "requires the RLRsim package",
    fixed = TRUE,
    class = "spicy_missing_pkg"
  )
})
