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

test_that("lmer (REML) chi^2 follows the fit estimator (ranova convention)", {
  # 2026-07-09 (dev/re_lrt_ml_reml_finding.md): the whole-block LRT is
  # computed on the FIT'S OWN likelihood -- REML vs the null model's
  # REML logLik (gls) -- matching lmerTest::ranova / Stata mixed.
  # (Previously: silent refitML under a "(REML)" label.)
  skip_if_not_installed("nlme")
  fit <- .fit_lmer_lrt()
  g0 <- nlme::gls(Reaction ~ Days, data = lme4::sleepstudy,
                  method = "REML")
  expected_chi2 <- 2 * (as.numeric(stats::logLik(fit)) -
                          as.numeric(stats::logLik(g0)))
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$random_effects$null_lrt$chi2,
               expected_chi2, tolerance = 1e-10)
  # External oracle: lmerTest::ranova reproduces the same statistic.
  skip_if_not_installed("lmerTest")
  rv <- lmerTest::ranova(lmerTest::as_lmerModLmerTest(fit))
  expect_equal(fr$info$random_effects$null_lrt$chi2, rv$LRT[2],
               tolerance = 1e-8)
})

test_that("lmer (ML) chi^2 keeps the ML-vs-lm comparison", {
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject),
                    data = lme4::sleepstudy, REML = FALSE)
  fit_null <- lm(Reaction ~ Days, data = lme4::sleepstudy)
  expected_chi2 <- 2 * (as.numeric(stats::logLik(fit)) -
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


# ---- 6. Degradation guards: gls null failure -> no LRT, no error ------

test_that(".null_reml_loglik_lm returns NA when gls fails", {
  skip_if_not_installed("nlme")
  # 0-row data: gls errors; the helper degrades to NA so callers drop
  # the LRT line instead of printing a number under a wrong label.
  expect_identical(
    spicy:::.null_reml_loglik_lm(y ~ 1, data.frame(y = numeric(0))),
    NA_real_
  )
})

test_that("REML fits drop the LRT line (not the table) when the gls null fails", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("nlme")
  testthat::local_mocked_bindings(
    .null_reml_loglik_lm = function(...) NA_real_,
    .package = "spicy"
  )
  expect_null(spicy:::.null_lrt_merMod(.fit_lmer_lrt()))
  expect_null(spicy:::.null_lrt_lme(.fit_lme_lrt()))
})


# ---- 7. Weights and within-group structures reach the null refit ------

test_that("weighted lmer LRT matches ranova exactly (REML and ML)", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("nlme")
  set.seed(7)
  d <- lme4::sleepstudy
  d$w <- runif(nrow(d), 0.5, 2)
  fitw <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), data = d,
                         weights = w)
  out <- spicy:::.null_lrt_merMod(fitw)
  # Oracle 1: lmerTest::ranova() on the weighted fit.
  rv <- lmerTest::ranova(fitw)
  expect_equal(out$chi2, rv$LRT[2], tolerance = 1e-6)
  # Oracle 2 (independent engine): logLik(lm weighted, REML = TRUE).
  lmw <- stats::lm(Reaction ~ Days, data = d, weights = w)
  expect_equal(out$chi2,
               2 * (as.numeric(stats::logLik(fitw)) -
                    as.numeric(stats::logLik(lmw, REML = TRUE))),
               tolerance = 1e-9)
  # ML estimator: identity against the weighted-lm ML null.
  fit_ml <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = d,
                       weights = w, REML = FALSE)
  expect_equal(spicy:::.null_lrt_merMod(fit_ml)$chi2,
               2 * (as.numeric(stats::logLik(fit_ml)) -
                    as.numeric(stats::logLik(lmw))),
               tolerance = 1e-9)
})


test_that("weighted glmer LRT compares against the weighted glm null", {
  skip_if_not_installed("lme4")
  set.seed(11)
  db <- data.frame(g = factor(rep(1:15, each = 12)),
                   x = rnorm(180), w2 = sample(1:3, 180, TRUE))
  db$y <- rbinom(180, 1, stats::plogis(0.3 * db$x +
                                       rep(rnorm(15, 0, 0.8), each = 12)))
  fg <- suppressMessages(
    lme4::glmer(y ~ x + (1 | g), data = db, family = binomial(),
                weights = w2)
  )
  g0 <- stats::glm(y ~ x, data = db, family = binomial(), weights = w2)
  expect_equal(spicy:::.null_lrt_merMod(fg)$chi2,
               2 * (as.numeric(stats::logLik(fg)) -
                    as.numeric(stats::logLik(g0))),
               tolerance = 1e-9)
})


test_that("lme variance/correlation structures survive into the gls null", {
  skip_if_not_installed("nlme")
  # varPower, REML: the null must carry the same variance model, or its
  # improvement is mislabelled as the random-effect test.
  fl <- nlme::lme(distance ~ age, data = nlme::Orthodont,
                  random = ~ 1 | Subject,
                  weights = nlme::varPower(form = ~age), method = "REML")
  gn <- nlme::gls(distance ~ age, data = nlme::Orthodont, method = "REML",
                  weights = nlme::varPower(form = ~age))
  out <- spicy:::.null_lrt_lme(fl)
  expect_equal(out$chi2,
               2 * (as.numeric(stats::logLik(fl)) -
                    as.numeric(stats::logLik(gn))),
               tolerance = 1e-6)
  # nlme has its own LRT of the same comparison: anova(gls, lme).
  a <- stats::anova(gn, fl)
  expect_equal(out$chi2, a$L.Ratio[2], tolerance = 1e-6)

  # corAR1, ML: same contract for correlation structures.
  fo <- nlme::lme(follicles ~ sin(2 * pi * Time), data = nlme::Ovary,
                  random = ~ 1 | Mare, correlation = nlme::corAR1(),
                  method = "ML")
  go <- nlme::gls(follicles ~ sin(2 * pi * Time), data = nlme::Ovary,
                  correlation = nlme::corAR1(form = ~ 1 | Mare),
                  method = "ML")
  expect_equal(spicy:::.null_lrt_lme(fo)$chi2,
               2 * (as.numeric(stats::logLik(fo)) -
                    as.numeric(stats::logLik(go))),
               tolerance = 1e-6)
})


test_that("glmmTMB null is engine-native: weighted, nbinom, unweighted pins", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  set.seed(7)
  d <- lme4::sleepstudy
  d$w <- runif(nrow(d), 0.5, 2)
  # Weighted gaussian REML: glmmTMB weights multiply the log-likelihood
  # (frequency weights), so the null must be glmmTMB too -- the old
  # lm/gls null produced a nonsense negative chi2 here.
  ftw <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = d,
                          weights = w, REML = TRUE)
  n0 <- glmmTMB::glmmTMB(Reaction ~ Days, data = d, weights = w,
                         REML = TRUE)
  out <- spicy:::.null_lrt_glmmTMB(ftw)
  expect_equal(out$chi2,
               2 * (as.numeric(stats::logLik(ftw)) -
                    as.numeric(stats::logLik(n0))),
               tolerance = 1e-6)
  expect_gt(out$chi2, 0)

  # nbinom2: previously NO LRT at all (a glm() null cannot fit nbinom).
  set.seed(5)
  dn <- data.frame(g = factor(rep(1:12, each = 10)), x = rnorm(120))
  dn$y <- stats::rnbinom(120,
                         mu = exp(0.5 + 0.3 * dn$x +
                                  rep(rnorm(12, 0, 0.6), each = 10)),
                         size = 1.5)
  fn <- glmmTMB::glmmTMB(y ~ x + (1 | g), data = dn,
                         family = glmmTMB::nbinom2())
  n0n <- glmmTMB::glmmTMB(y ~ x, data = dn, family = glmmTMB::nbinom2())
  expect_equal(spicy:::.null_lrt_glmmTMB(fn)$chi2,
               2 * (as.numeric(stats::logLik(fn)) -
                    as.numeric(stats::logLik(n0n))),
               tolerance = 1e-6)

  # Unweighted: the engine-native null reproduces the old gls REML null
  # (equivalence cross-checked against the lmer chi2 on the same model).
  ft0 <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject),
                          data = lme4::sleepstudy, REML = TRUE)
  fl0 <- lme4::lmer(Reaction ~ Days + (1 | Subject),
                    data = lme4::sleepstudy)
  expect_equal(spicy:::.null_lrt_glmmTMB(ft0)$chi2,
               spicy:::.null_lrt_merMod(fl0)$chi2, tolerance = 1e-4)
})
