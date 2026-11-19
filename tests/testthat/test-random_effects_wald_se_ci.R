# ---------------------------------------------------------------------------
# Phase 7c7a tests: Wald SE + 95% CI on variance scale for random effects
# across the 4 mixed-effects classes (lmer, glmer, glmmTMB, lme).
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_lmer_int <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_lmer_slope <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

.fit_glmer_singular <- function() {
  skip_if_not_installed("lme4")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  suppressMessages(suppressWarnings(
    lme4::glmer(am ~ mpg + (1 | cyl), data = d, family = binomial)
  ))
}

.fit_glmmTMB_int <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_glmmTMB_slope <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

.fit_lme_int <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )
}

.fit_lme_slope <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ age | Subject
  )
}


# ---- 1. Schema: new columns are present ----------------------------------

test_that("variance_components carries Wald SE + CI columns for lmer", {
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(
    c("std_error", "ci_lower", "ci_upper", "ci_method") %in%
      colnames(vc)
  ))
})

test_that("variance_components carries Wald SE + CI columns for glmmTMB", {
  fit <- .fit_glmmTMB_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(
    c("std_error", "ci_lower", "ci_upper", "ci_method") %in%
      colnames(vc)
  ))
})

test_that("variance_components carries Wald SE + CI columns for lme", {
  fit <- .fit_lme_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(
    c("std_error", "ci_lower", "ci_upper", "ci_method") %in%
      colnames(vc)
  ))
})


# ---- 2. lmer: SE + CI populated (non-singular fit) -----------------------

test_that("lmer (random intercept): SE + CI are finite for all rows", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(is.finite(vc$std_error)))
  expect_true(all(is.finite(vc$ci_lower)))
  expect_true(all(is.finite(vc$ci_upper)))
  expect_true(all(vc$ci_method == "wald"))
  # Algebraic identity of the Wald mapping (default ci_level = 0.95),
  # on log(sd): ci = variance * exp(+/- 2 z s), s = SE / (2 variance),
  # so s must be recoverable from the log-ratio of the bounds exactly.
  z <- qnorm(0.975)
  s <- vc$std_error / (2 * vc$variance)
  expect_equal(log(vc$ci_upper / vc$ci_lower) / (4 * z), s, tolerance = 1e-12)
  expect_equal(vc$ci_lower, vc$variance * exp(-2 * z * s), tolerance = 1e-12)
})

test_that("lmer (random intercept): SE is the one lmer, lme and glmmTMB share", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  # SE of the SD (decision 2026-11-19-re-se-delta.md): 6.811906 and
  # 1.727074; the frame stores SE(sd^2) = 2 sd SE(sd).
  expect_equal(
    vc$std_error / (2 * sqrt(vc$variance)),
    c(6.811906, 1.727074),
    tolerance = 1e-6
  )
})

test_that("lmer (random slope): SE/CI populated for intercept + slope + residual", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_slope()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  # Phase 7c7b: 2 group variance rows + 1 correlation row + 1 residual = 4
  expect_identical(nrow(vc), 4L)
  var_rows <- vc[!(vc$is_correlation %in% TRUE), ]
  expect_identical(nrow(var_rows), 3L)
  expect_true(all(is.finite(var_rows$std_error)))
  expect_true(all(var_rows$ci_lower >= 0)) # variance >= 0 enforced

  # Pin the 3 SD-row SEs to the values lmer and glmmTMB share
  # (decision 2026-11-19-re-se-delta.md): 5.8363, 1.2480, 1.5080.
  expect_identical(var_rows$term, c("(Intercept)", "Days", ""))
  sd_se <- var_rows$std_error / (2 * sqrt(var_rows$variance))
  expect_equal(sd_se, c(5.8363, 1.2480, 1.5080), tolerance = 1e-4)
  z <- qnorm(0.975)
  s_log <- var_rows$std_error / (2 * var_rows$variance)
  expect_equal(
    var_rows$ci_lower,
    var_rows$variance * exp(-2 * z * s_log),
    tolerance = 1e-10
  )
  expect_equal(
    var_rows$ci_upper,
    var_rows$variance * exp(2 * z * s_log),
    tolerance = 1e-10
  )

  # Correlation row: SE 0.3250, CI Wald on atanh(rho).
  g_vc <- as.matrix(lme4::VarCorr(fit)$Subject)
  rho <- g_vc[1L, 2L] / sqrt(g_vc[1L, 1L] * g_vc[2L, 2L])
  corr_row <- vc[vc$is_correlation %in% TRUE, ]
  expect_equal(corr_row$corr, rho, tolerance = 1e-10)
  expect_equal(corr_row$std_error, 0.3250, tolerance = 1e-4)
  half <- z * corr_row$std_error / (1 - rho^2)
  expect_equal(corr_row$ci_lower, tanh(atanh(rho) - half), tolerance = 1e-10)
  expect_equal(corr_row$ci_upper, tanh(atanh(rho) + half), tolerance = 1e-10)
})

test_that("lmer Wald CI brackets the point estimate", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(vc$ci_lower <= vc$variance))
  expect_true(all(vc$variance <= vc$ci_upper))
  # Pin the exact bounds on top of the bracket check: Wald CI on
  # log(sd), squared, variance * exp(+/- 2 z s), s = SE / (2 variance),
  # with SE(sd^2) = 2 sd SE(sd) and SE(sd) the values lmer, lme and
  # glmmTMB share. No clamp: the bounds stay above 0.
  se_or <- 2 * sqrt(vc$variance) * c(6.811906, 1.727074)
  z <- qnorm(0.975)
  s <- se_or / (2 * vc$variance)
  expect_identical(vc$group, c("Subject", "Residual"))
  expect_equal(vc$ci_lower, vc$variance * exp(-2 * z * s), tolerance = 1e-6)
  expect_equal(vc$ci_upper, vc$variance * exp(2 * z * s), tolerance = 1e-6)
})


# ---- 3. glmer singular fit: SE/CI = NA (no crash) -----------------------

test_that("glmer singular fit: SE/CI gracefully degrade to NA", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_glmer_singular()
  expect_true(lme4::isSingular(fit))
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(is.na(vc$std_error)))
  expect_true(all(is.na(vc$ci_method)))
})


# ---- 4. glmmTMB: SE/CI from native confint(method="Wald") ---------------

test_that("glmmTMB (random intercept): SE + CI populated for group row", {
  fit <- .fit_glmmTMB_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  group_row <- vc[vc$group != "Residual", ]
  expect_true(is.finite(group_row$std_error))
  expect_identical(group_row$ci_method, "wald")
  # Pin SE to the delta method on glmmTMB's own parameter, log(sd):
  # SE(sd^2) = 2 * sd^2 * SE(theta), SE(theta) read from sdreport.
  sr <- summary(fit$sdr, "fixed")
  th <- sr[rownames(sr) == "theta", , drop = FALSE]
  sd_est <- exp(th[1L, "Estimate"])
  expect_equal(
    group_row$std_error,
    unname(2 * sd_est^2 * th[1L, "Std. Error"]),
    tolerance = 1e-6
  )
})

test_that("glmmTMB residual SE: delta method on its dispersion parameter", {
  fit <- .fit_glmmTMB_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  resid_row <- vc[vc$group == "Residual", ]
  # Gaussian glmmTMB 1.1.15: betadisp = log(sigma).
  sr <- summary(fit$sdr, "fixed")
  bd <- sr[rownames(sr) == "betadisp", , drop = FALSE]
  sig <- exp(bd[1L, "Estimate"])
  expect_equal(resid_row$variance, sig^2, tolerance = 1e-8)
  expect_equal(
    resid_row$std_error,
    unname(2 * sig^2 * bd[1L, "Std. Error"]),
    tolerance = 1e-6
  )
  ci <- confint(fit, parm = "sigma", method = "Wald")
  expect_equal(resid_row$ci_lower, ci[1L, 1L]^2, tolerance = 1e-10)
  expect_equal(resid_row$ci_upper, ci[1L, 2L]^2, tolerance = 1e-10)
  expect_identical(resid_row$ci_method, "wald")
})

test_that("glmmTMB CI on variance = (SD CI)^2 (Delta-method roundtrip)", {
  fit <- .fit_glmmTMB_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  ci_sd <- confint(fit, method = "Wald", parm = "theta_")
  n_checked <- 0L
  for (i in seq_len(nrow(vc))) {
    if (vc$group[i] == "Residual") {
      next
    }
    expect_equal(
      vc$ci_lower[i],
      max(0, ci_sd[1L, "2.5 %"])^2,
      tolerance = 1e-10
    )
    expect_equal(vc$ci_upper[i], ci_sd[1L, "97.5 %"]^2, tolerance = 1e-10)
    n_checked <- n_checked + 1L
  }
  # Skipping the residual row must not amount to skipping every row.
  expect_oracle_covered(n_checked)
})


# ---- 5. lme: SE/CI from nlme::intervals() ------------------------------

test_that("lme (random intercept): SE + CI populated for all rows", {
  fit <- .fit_lme_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(is.finite(vc$std_error)))
  expect_true(all(is.finite(vc$ci_lower)))
  expect_true(all(is.finite(vc$ci_upper)))
  expect_true(all(vc$ci_method == "wald"))
  # Pin SEs to the delta method on nlme's own scale, read from apVar
  # (the covariance of log(sd) and log(sigma)), a route independent of
  # the intervals() the code reads: SE(sd^2) = 2 * sd^2 * SE(log sd).
  s <- sqrt(diag(fit$apVar))
  sds <- exp(attr(fit$apVar, "Pars"))
  expect_equal(
    vc$std_error[vc$group == "Subject"],
    unname(2 * sds[1L]^2 * s[1L]),
    tolerance = 1e-8
  )
  expect_equal(
    vc$std_error[vc$group == "Residual"],
    unname(2 * sds[2L]^2 * s[2L]),
    tolerance = 1e-8
  )
})

test_that("lme (random slope): SE/CI populated for both group rows + residual", {
  fit <- .fit_lme_slope()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  # Phase 7c7b: 2 group variance rows + 1 correlation row + 1 residual = 4
  expect_identical(nrow(vc), 4L)
  expect_true(all(is.finite(vc$std_error)))
  # Pin every row to the delta method on nlme's natural parameters,
  # read from apVar: log(sd) for the SDs and sigma, log((1 + rho) /
  # (1 - rho)) for the correlation. The CI is intervals()' row verbatim.
  ci <- nlme::intervals(fit, which = "var-cov")
  subj <- ci$reStruct$Subject
  s <- unname(sqrt(diag(fit$apVar)))
  pars <- unname(attr(fit$apVar, "Pars"))
  se_int <- 2 * exp(pars[1L])^2 * s[1L]
  se_age <- 2 * exp(pars[2L])^2 * s[2L]
  rho <- tanh(pars[3L] / 2)
  se_rho <- (1 - rho^2) * s[3L] / 2
  expect_equal(
    vc$std_error[
      vc$group == "Subject" &
        vc$term == "(Intercept)"
    ],
    se_int,
    tolerance = 1e-10
  )
  expect_equal(
    vc$std_error[vc$group == "Subject" & vc$term == "age"],
    se_age,
    tolerance = 1e-10
  )
  corr_row <- vc[vc$is_correlation %in% TRUE, ]
  expect_equal(
    corr_row$corr,
    subj["cor((Intercept),age)", "est."],
    tolerance = 1e-10
  )
  expect_equal(corr_row$std_error, se_rho, tolerance = 1e-8)
  expect_equal(
    corr_row$ci_lower,
    subj["cor((Intercept),age)", "lower"],
    tolerance = 1e-10
  )
  expect_equal(
    corr_row$ci_upper,
    subj["cor((Intercept),age)", "upper"],
    tolerance = 1e-10
  )
  se_res <- 2 * exp(pars[4L])^2 * s[4L]
  expect_equal(vc$std_error[vc$group == "Residual"], se_res, tolerance = 1e-8)
})

test_that("lme CI matches intervals() output squared", {
  fit <- .fit_lme_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  ci <- nlme::intervals(fit, which = "var-cov")
  # Subject (Intercept) row
  subj <- vc[vc$group == "Subject", ]
  sd_lower <- ci$reStruct$Subject["sd((Intercept))", "lower"]
  sd_upper <- ci$reStruct$Subject["sd((Intercept))", "upper"]
  expect_equal(subj$ci_lower, max(0, sd_lower)^2, tolerance = 1e-10)
  expect_equal(subj$ci_upper, sd_upper^2, tolerance = 1e-10)
  # Residual row
  res <- vc[vc$group == "Residual", ]
  expect_equal(res$ci_lower, max(0, ci$sigma["lower"])^2, tolerance = 1e-10)
  expect_equal(res$ci_upper, unname(ci$sigma["upper"])^2, tolerance = 1e-10)
})


# ---- 6. Validator accepts the extended schema --------------------------

test_that("validator accepts variance_components with new SE/CI columns", {
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("validator accepts glmmTMB extended schema", {
  fit <- .fit_glmmTMB_int()
  fr <- as_regression_frame(fit)
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("validator accepts lme extended schema", {
  fit <- .fit_lme_int()
  fr <- as_regression_frame(fit)
  expect_invisible(spicy:::validate_regression_frame(fr))
})


# ---- 7. CI bounds are non-negative (variance constraint) ---------------

test_that("ci_lower is clamped at 0 (variance non-negative)", {
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(vc$ci_lower[!is.na(vc$ci_lower)] >= 0))
})
