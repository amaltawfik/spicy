# ---------------------------------------------------------------------------
# Phase 7c11 tests: nested = TRUE for mixed-effects fits. Adds an LRT-based
# nested-comparison path so `table_regression(list(m1, m2, m3), nested=TRUE)`
# emits AIC / BIC / chi^2 / p change rows for the four mixed-effects classes.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_lmer_nested <- function() {
  skip_if_not_installed("lme4")
  list(
    m1 = lme4::lmer(Reaction ~ 1     + (1 | Subject),
                    data = lme4::sleepstudy, REML = FALSE),
    m2 = lme4::lmer(Reaction ~ Days  + (1 | Subject),
                    data = lme4::sleepstudy, REML = FALSE)
  )
}

.fit_glmer_nested <- function() {
  skip_if_not_installed("lme4")
  set.seed(1)
  n <- 500
  g <- factor(rep(1:25, length.out = n))
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(0.5 + 0.8 * x + rnorm(25)[g]))
  list(
    m1 = lme4::glmer(y ~ 1 + (1 | g), family = binomial),
    m2 = lme4::glmer(y ~ x + (1 | g), family = binomial)
  )
}

.fit_lme_nested <- function() {
  skip_if_not_installed("nlme")
  list(
    m1 = nlme::lme(distance ~ 1,   data = nlme::Orthodont,
                    random = ~ 1 | Subject, method = "ML"),
    m2 = nlme::lme(distance ~ age, data = nlme::Orthodont,
                    random = ~ 1 | Subject, method = "ML")
  )
}


# ---- 1. compute_one_pair_mixed returns finite stats ---------------------

test_that("compute_one_pair_mixed populates lrt / aic / bic / p_change for lmer", {
  fits <- .fit_lmer_nested()
  out <- spicy:::compute_one_pair_mixed(fits$m1, fits$m2)
  # Oracle: anova() on the same fits. Both are ML (REML = FALSE) so
  # anova() does not refit -- the table values are exact.
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_equal(out$lrt_change, as.numeric(av[["Chisq"]][2L]),
               tolerance = 1e-10)
  expect_equal(out$aic_change,
               as.numeric(av[["AIC"]][2L] - av[["AIC"]][1L]),
               tolerance = 1e-10)
  expect_equal(out$bic_change,
               as.numeric(av[["BIC"]][2L] - av[["BIC"]][1L]),
               tolerance = 1e-10)
  expect_equal(out$p_change, as.numeric(av[["Pr(>Chisq)"]][2L]),
               tolerance = 1e-10)
  # For ML fits the -2*logLik drop IS the LRT chi-square (algebraic
  # identity), so deviance_change must equal both.
  expect_equal(out$deviance_change,
               -2 * (as.numeric(stats::logLik(fits$m1)) -
                       as.numeric(stats::logLik(fits$m2))),
               tolerance = 1e-10)
  expect_equal(out$deviance_change, out$lrt_change, tolerance = 1e-10)
  # Variance-explained tokens are NA for mixed (F-test framework
  # doesn't apply).
  expect_true(is.na(out$r2_change))
  expect_true(is.na(out$f_change))
  expect_true(is.na(out$f2_change))
  expect_true(is.na(out$aicc_change))
})

test_that("compute_one_pair_mixed populates stats for glmer", {
  fits <- .fit_glmer_nested()
  out <- spicy:::compute_one_pair_mixed(fits$m1, fits$m2)
  # Oracle: anova() on the same (always-ML) glmer fits -- no refit.
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_equal(out$lrt_change, as.numeric(av[["Chisq"]][2L]),
               tolerance = 1e-10)
  expect_equal(out$aic_change,
               as.numeric(av[["AIC"]][2L] - av[["AIC"]][1L]),
               tolerance = 1e-10)
  expect_equal(out$bic_change,
               as.numeric(av[["BIC"]][2L] - av[["BIC"]][1L]),
               tolerance = 1e-10)
  expect_equal(out$p_change, as.numeric(av[["Pr(>Chisq)"]][2L]),
               tolerance = 1e-10)
  expect_equal(out$deviance_change,
               -2 * (as.numeric(stats::logLik(fits$m1)) -
                       as.numeric(stats::logLik(fits$m2))),
               tolerance = 1e-10)
})

test_that("compute_one_pair_mixed populates stats for nlme::lme", {
  fits <- .fit_lme_nested()
  out <- spicy:::compute_one_pair_mixed(fits$m1, fits$m2)
  # Oracle: anova.lme() on the same ML fits (columns L.Ratio / p-value).
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_equal(out$lrt_change, as.numeric(av[["L.Ratio"]][2L]),
               tolerance = 1e-10)
  expect_equal(out$aic_change,
               as.numeric(av[["AIC"]][2L] - av[["AIC"]][1L]),
               tolerance = 1e-10)
  expect_equal(out$bic_change,
               as.numeric(av[["BIC"]][2L] - av[["BIC"]][1L]),
               tolerance = 1e-10)
  expect_equal(out$p_change, as.numeric(av[["p-value"]][2L]),
               tolerance = 1e-10)
  # anova.lme has no deviance column: spicy derives the drop from
  # logLik(), which for ML fits equals the L.Ratio statistic.
  expect_equal(out$deviance_change,
               -2 * (as.numeric(stats::logLik(fits$m1)) -
                       as.numeric(stats::logLik(fits$m2))),
               tolerance = 1e-10)
})


# ---- 2. LRT statistic matches anova() directly --------------------------

test_that("lrt_change matches anova(m1, m2)[['Chisq']][2L] for lmer", {
  fits <- .fit_lmer_nested()
  expected <- suppressWarnings(suppressMessages(
    stats::anova(fits$m1, fits$m2)[["Chisq"]][2L]
  ))
  out <- spicy:::compute_one_pair_mixed(fits$m1, fits$m2)
  expect_equal(out$lrt_change, as.numeric(expected), tolerance = 1e-10)
})

test_that("lrt_change matches anova(m1, m2)[['L.Ratio']][2L] for nlme::lme", {
  fits <- .fit_lme_nested()
  expected <- suppressWarnings(suppressMessages(
    stats::anova(fits$m1, fits$m2)[["L.Ratio"]][2L]
  ))
  out <- spicy:::compute_one_pair_mixed(fits$m1, fits$m2)
  expect_equal(out$lrt_change, as.numeric(expected), tolerance = 1e-10)
})


# ---- 3. Dispatcher detects mixed pairs ----------------------------------

test_that("compute_nested_comparisons dispatches mixed pairs to the mixed branch", {
  fits <- .fit_lmer_nested()
  comp <- spicy:::compute_nested_comparisons(list(fits$m1, fits$m2))
  expect_identical(nrow(comp), 1L)
  # Mixed branch pins the LRT to the anova() chi-square (ML fits, no refit).
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_equal(comp$lrt_change[1L], as.numeric(av[["Chisq"]][2L]),
               tolerance = 1e-10)
  # f_change / r2_change NA -- the lm path would have populated them.
  expect_true(is.na(comp$f_change[1L]))
  expect_true(is.na(comp$r2_change[1L]))
})


# ---- 4. default_nested_tokens returns mixed-specific defaults -----------

test_that("default_nested_tokens returns mixed tokens for all-mixed lists", {
  fits <- .fit_lmer_nested()
  toks <- spicy:::default_nested_tokens(list(fits$m1, fits$m2))
  expect_identical(toks, c("aic_change", "bic_change", "lrt_change", "p_change"))
})

test_that("default_nested_tokens still returns glm tokens for all-glm lists", {
  m1 <- glm(am ~ 1,   data = mtcars, family = binomial)
  m2 <- glm(am ~ mpg, data = mtcars, family = binomial)
  toks <- spicy:::default_nested_tokens(list(m1, m2))
  expect_identical(toks, c("lrt_change", "p_change"))
})

test_that("default_nested_tokens still returns lm tokens for all-lm lists", {
  m1 <- lm(mpg ~ 1,  data = mtcars)
  m2 <- lm(mpg ~ wt, data = mtcars)
  toks <- spicy:::default_nested_tokens(list(m1, m2))
  expect_identical(toks, c("r2_change", "f_change", "p_change"))
})


# ---- 5. End-to-end: table_regression(..., nested = TRUE) ----------------

test_that("table_regression(list, nested = TRUE) renders LRT change rows for lmer", {
  fits <- .fit_lmer_nested()
  out <- capture.output(print(table_regression(list(fits$m1, fits$m2),
                                                nested = TRUE)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Hierarchical", fixed = TRUE)
  expect_match(combined, "ΔAIC",         fixed = TRUE)
  expect_match(combined, "ΔBIC",         fixed = TRUE)
  expect_match(combined, "Δχ²",          fixed = TRUE)
  expect_match(combined, "p (change)",   fixed = TRUE)
  # Rendered cell values pin to the anova() oracle at the default digits
  # (lrt_change -> digits = 2, aic/bic_change -> ic_digits = 1).
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_match(combined,
               spicy:::format_signed(as.numeric(av[["Chisq"]][2L]), 2L),
               fixed = TRUE)
  expect_match(combined,
               spicy:::format_signed(
                 as.numeric(av[["AIC"]][2L] - av[["AIC"]][1L]), 1L),
               fixed = TRUE)
  expect_match(combined,
               spicy:::format_signed(
                 as.numeric(av[["BIC"]][2L] - av[["BIC"]][1L]), 1L),
               fixed = TRUE)
})

test_that("table_regression(list, nested = TRUE) renders LRT change rows for glmer", {
  fits <- .fit_glmer_nested()
  out <- capture.output(print(table_regression(list(fits$m1, fits$m2),
                                                nested = TRUE)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "ΔAIC",       fixed = TRUE)
  expect_match(combined, "Δχ²",        fixed = TRUE)
  expect_match(combined, "p (change)", fixed = TRUE)
  # Rendered chi-square cell pins to the anova() oracle (default digits = 2).
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_match(combined,
               spicy:::format_signed(as.numeric(av[["Chisq"]][2L]), 2L),
               fixed = TRUE)
})

test_that("table_regression(list, nested = TRUE) renders LRT change rows for lme", {
  fits <- .fit_lme_nested()
  out <- capture.output(print(table_regression(list(fits$m1, fits$m2),
                                                nested = TRUE)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "ΔAIC",       fixed = TRUE)
  expect_match(combined, "Δχ²",        fixed = TRUE)
  # Rendered chi-square cell pins to the anova.lme() L.Ratio oracle.
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_match(combined,
               spicy:::format_signed(as.numeric(av[["L.Ratio"]][2L]), 2L),
               fixed = TRUE)
})

test_that("nested = TRUE on REML-fit lmer pair does not error (auto-refit by anova)", {
  skip_if_not_installed("lme4")
  m1 <- lme4::lmer(Reaction ~ 1    + (1 | Subject), data = lme4::sleepstudy)
  m2 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  expect_silent(out <- capture.output(print(
    table_regression(list(m1, m2), nested = TRUE)
  )))
  # Oracle: anova() on the REML pair refits with ML before the LRT --
  # the rendered chi-square must be the ML-refit statistic, not a
  # REML-likelihood difference.
  av <- suppressWarnings(suppressMessages(stats::anova(m1, m2)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined,
               spicy:::format_signed(as.numeric(av[["Chisq"]][2L]), 2L),
               fixed = TRUE)
})


# ---- 6. Regression guard: lm + glm pairs unchanged ----------------------

test_that("lm pair: nested dispatch still routes to compute_one_pair_lm", {
  m1 <- lm(mpg ~ 1,  data = mtcars)
  m2 <- lm(mpg ~ wt, data = mtcars)
  comp <- spicy:::compute_nested_comparisons(list(m1, m2))
  # lm-path oracles: DeltaR^2 from summary(), partial F from anova().
  expect_equal(comp$r2_change[1L],
               unname(summary(m2)$r.squared - summary(m1)$r.squared),
               tolerance = 1e-12)
  av <- stats::anova(m1, m2)
  expect_equal(comp$f_change[1L], as.numeric(av[["F"]][2L]),
               tolerance = 1e-12)
  expect_equal(comp$p_change[1L], as.numeric(av[["Pr(>F)"]][2L]),
               tolerance = 1e-12)
})

test_that("glm pair: nested dispatch still routes to compute_one_pair_glm", {
  m1 <- glm(am ~ 1,   data = mtcars, family = binomial)
  m2 <- glm(am ~ mpg, data = mtcars, family = binomial)
  comp <- spicy:::compute_nested_comparisons(list(m1, m2))
  # glm-path oracle: LRT chi-square + p from anova(test = "LRT").
  av <- stats::anova(m1, m2, test = "LRT")
  expect_equal(comp$lrt_change[1L], as.numeric(av[["Deviance"]][2L]),
               tolerance = 1e-12)
  expect_equal(comp$p_change[1L], as.numeric(av[["Pr(>Chi)"]][2L]),
               tolerance = 1e-12)
  expect_true(is.na(comp$r2_change[1L]))
})
