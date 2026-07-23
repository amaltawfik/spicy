# Opt-in per-term significance tests for random-effect variance components
# (re_test = "lrt" / "rlrt"). Never Wald (boundary); the LRT uses the
# chi-bar-squared mixture, the RLRT the exact simulated null. Cross-validated
# to lmerTest::ranova (statistic/df) and RLRsim::exactRLRT (statistic + p).
# Design: dev/mixed_random_effects_rows_spec.md.

.rt_lmer_slope <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}
.rt_lmer_int <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}
.rt_vc <- function(fit, ...) {
  skip_if_not_installed("broom")
  td <- broom::tidy(table_regression(fit, ...))
  td[td$estimate_type == "vc", , drop = FALSE]
}

test_that("lrt: slope test matches lmerTest::ranova statistic and df", {
  skip_if_not_installed("lmerTest")
  fit <- .rt_lmer_slope()
  vc <- .rt_vc(fit, re_test = "lrt")
  tested <- vc[!is.na(vc$p.value), ]
  # only the slope is tested (ranova scheme): 1 row, the Days SD row
  expect_equal(nrow(tested), 1L)
  expect_match(tested$term, "re::Subject::Days", fixed = TRUE)
  expect_identical(tested$test_type, "chibar2")

  rn <- as.data.frame(lmerTest::ranova(lmerTest::as_lmerModLmerTest(fit)))
  expect_equal(tested$statistic, rn$LRT[2L], tolerance = 1e-6)
  expect_equal(tested$df, rn$Df[2L])
  # chi-bar-squared mixture p is smaller than ranova's plain (conservative)
  # chi-square p, and at least half of it
  expect_lt(tested$p.value, rn[["Pr(>Chisq)"]][2L])
  expect_gte(tested$p.value, rn[["Pr(>Chisq)"]][2L] / 2)
})

test_that("lrt: intercept-only model reuses the whole-block null LRT", {
  fit <- .rt_lmer_int()
  vc <- .rt_vc(fit, re_test = "lrt")
  tested <- vc[!is.na(vc$p.value), ]
  expect_equal(nrow(tested), 1L)
  nl <- spicy:::.compute_null_model_lrt(fit)
  expect_equal(tested$statistic, nl$chi2, tolerance = 1e-8)
  expect_equal(tested$p.value, nl$p_chibar2, tolerance = 1e-12)
})

test_that("rlrt matches RLRsim::exactRLRT on a single-component fit", {
  skip_if_not_installed("RLRsim")
  fit <- .rt_lmer_int()
  set.seed(42)
  vc <- .rt_vc(fit, re_test = "rlrt")
  tested <- vc[!is.na(vc$p.value), ]
  expect_equal(nrow(tested), 1L)
  expect_identical(tested$test_type, "rlrt")
  set.seed(42)
  orc <- RLRsim::exactRLRT(fit)
  expect_equal(tested$statistic, unname(orc$statistic), tolerance = 1e-8)
  expect_equal(tested$p.value, unname(orc$p.value), tolerance = 1e-3)
})

test_that("rlrt refuses richer random structures with a pointer to lrt", {
  skip_if_not_installed("RLRsim")
  fit <- .rt_lmer_slope()
  expect_error(
    table_regression(fit, re_test = "rlrt"),
    class = "spicy_unsupported"
  )
})

test_that("re_test validation and guards", {
  fit <- .rt_lmer_int()
  expect_error(
    table_regression(fit, re_test = "wald"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, re_test = "lrt", show_re = FALSE),
    class = "spicy_invalid_input"
  )
})

test_that("lrt works for nlme::lme (random = ~ slope | group)", {
  skip_if_not_installed("nlme")
  ml <- nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ age | Subject
  )
  vc <- .rt_vc(ml, re_test = "lrt")
  tested <- vc[!is.na(vc$p.value), ]
  expect_equal(nrow(tested), 1L)
  expect_match(tested$term, "re::Subject::age", fixed = TRUE)
  # oracle: hand REML LRT vs the intercept-only reduced refit (built
  # directly -- update(lme_fit) needs nlme on the search path)
  ml_red <- nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )
  chi2_h <- 2 *
    (as.numeric(stats::logLik(ml)) -
      as.numeric(stats::logLik(ml_red)))
  expect_equal(tested$statistic, chi2_h, tolerance = 1e-6)
})

test_that("rlrt works for a single-component nlme::lme (matches exactRLRT)", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("RLRsim")
  mi <- nlme::lme(
    distance ~ age,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )
  set.seed(7)
  vc <- .rt_vc(mi, re_test = "rlrt")
  tested <- vc[!is.na(vc$p.value), ]
  expect_equal(nrow(tested), 1L)
  set.seed(7)
  orc <- RLRsim::exactRLRT(mi)
  expect_equal(tested$statistic, unname(orc$statistic), tolerance = 1e-8)
})

test_that("lrt works for glmmTMB and never tests rho / residual", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(
    Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  vc <- .rt_vc(fit, re_test = "lrt")
  expect_true(any(!is.na(vc$p.value)))
  expect_true(all(is.na(vc$p.value[grepl("::cor", vc$term)])))
  expect_true(all(is.na(vc$p.value[grepl("Residual", vc$term)])))
})

test_that("the footer discloses the per-term test", {
  fit <- .rt_lmer_slope()
  out <- paste(
    capture.output(print(table_regression(fit, re_test = "lrt"))),
    collapse = "\n"
  )
  expect_match(out, "chi-bar-squared reference", fixed = TRUE)
})

test_that("stars never land on variance-component rows", {
  fit <- .rt_lmer_slope()
  df <- table_regression(
    fit,
    re_test = "lrt",
    stars = TRUE,
    output = "data.frame"
  )
  b_col <- names(df)[2]
  re_rows <- grepl("Subject", df$Variable, fixed = TRUE)
  expect_false(any(grepl("[*]", df[[b_col]][re_rows])))
  # while the fixed-effect rows do get stars
  expect_true(any(grepl("[*]", df[[b_col]][df$Variable == "Days"])))
})
