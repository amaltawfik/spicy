# ---------------------------------------------------------------------------
# Wald SE + 95% CI for the random-effect correlation rows (rho) of the
# random-effects panel. For lme4 (lmer / glmer) the SE comes from the
# Hessian of the deviance in (SD, correlation) and the CI is the Wald
# interval on atanh(rho); the oracle is glmmTMB on the same model.
# glmmTMB and nlme::lme populate rho SE/CI via their native engines
# (confint(method="Wald"), nlme::intervals()); the regression guards
# verify that path still works.
#
# Matches Stata `mixed` / SAS `PROC MIXED` panels which report
# `corr(_cons, slope) Subject | SE | 95% CI`.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_lmer_slope_corr <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

.fit_glmer_slope_corr <- function() {
  skip_if_not_installed("lme4")
  set.seed(42)
  n <- 1000
  g <- factor(rep(1:30, length.out = n))
  x <- rnorm(n)
  b0 <- rnorm(30)
  b1 <- rnorm(30, 0, 0.5)
  y <- rbinom(n, 1, plogis(0.5 + b0[g] + (0.8 + b1[g]) * x))
  lme4::glmer(y ~ x + (x | g), family = binomial)
}

.fit_glmmTMB_slope_corr <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

.fit_lme_slope_corr <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(distance ~ age, data = nlme::Orthodont, random = ~ age | Subject)
}


# ---- 1. lmer: correlation row has finite SE + CI ------------------------

# SE of a two-term glmmTMB correlation: theta = rho / sqrt(1 - rho^2), so
# SE(rho) = SE(theta) (1 + theta^2)^(-3/2) (see test 4 below).
.tmb_rho_se <- function(tm) {
  sdr <- summary(tm$sdr, "fixed")
  th <- sdr[rownames(sdr) == "theta", , drop = FALSE]
  unname(th[3L, "Std. Error"] * (1 + th[3L, "Estimate"]^2)^(-3 / 2))
}

test_that("lmer (Days | Subject): rho row has the Wald SE + CI of glmmTMB", {
  skip_if_not_installed("numDeriv")
  skip_if_not_installed("glmmTMB")
  fit <- .fit_lmer_slope_corr()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  rho_rows <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(rho_rows), 1L)
  expect_true(is.finite(rho_rows$std_error))
  expect_identical(rho_rows$ci_method, "wald")

  # Oracle: glmmTMB on the same REML model. Both SEs are the observed
  # information; the residual drift comes from the two optima.
  tm <- glmmTMB::glmmTMB(
    Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy,
    REML = TRUE
  )
  g_vc <- lme4::VarCorr(fit)[["Subject"]]
  rho_est <- attr(g_vc, "correlation")[1L, 2L]
  expect_equal(rho_rows$corr, rho_est, tolerance = 1e-12)
  expect_equal(rho_rows$std_error, .tmb_rho_se(tm), tolerance = 1e-4)
  # CI: Wald on atanh(rho), back-transformed.
  z <- qnorm(0.975)
  half <- z * rho_rows$std_error / (1 - rho_est^2)
  expect_equal(
    rho_rows$ci_lower,
    tanh(atanh(rho_est) - half),
    tolerance = 1e-12
  )
  expect_equal(
    rho_rows$ci_upper,
    tanh(atanh(rho_est) + half),
    tolerance = 1e-12
  )
})

test_that("lmer rho CI stays inside (-1, 1)", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_slope_corr()
  fr <- as_regression_frame(fit, model_id = "M1")
  rho <- fr$info$random_effects$variance_components
  rho <- rho[rho$is_correlation %in% TRUE, ]
  expect_gt(rho$ci_lower, -1)
  expect_lt(rho$ci_upper, 1)
})


# ---- 2. lmer SE pinned to the value lmer, lme and glmmTMB share ---------

test_that("lmer rho SE is 0.3250 on sleepstudy (REML)", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_slope_corr()
  fr <- as_regression_frame(fit, model_id = "M1")
  rho_row <- fr$info$random_effects$variance_components
  rho_row <- rho_row[rho_row$is_correlation %in% TRUE, ]
  # glmmTMB 0.325024, lme with .relStep = 1e-4 0.325098 (decision
  # 2026-11-19-re-se-delta.md); the Hessian of lme4's deviance 0.325025.
  expect_equal(rho_row$std_error, 0.3250, tolerance = 1e-4)
})


# ---- 2b. THREE-term random block: column-major positions ----------------
# Regression guard for the Phase 7c25 fix. lme4's theta, and the
# parameter vector of the Hessian, lay each random block out COLUMN-MAJOR
# (lower triangle, column by column). Row-major positions coincide with
# column-major only for n <= 2, so a 3-term random block (n = 3) would
# pull SEs from the wrong cells. Cross-validate every SD row and every
# correlation row against glmmTMB, whose correlations are mapped by
# glmmTMB::get_cor().

test_that("lmer 3-term random block: SD + correlation SE are correct (column-major)", {
  skip_if_not_installed("numDeriv")
  skip_if_not_installed("glmmTMB")
  set.seed(123)
  n <- 600
  g <- factor(rep(1:30, length.out = n))
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  b0 <- rnorm(30, 0, 2)
  b1 <- rnorm(30, 0, 1.5)
  b2 <- rnorm(30, 0, 1)
  y <- 1 + 0.5 * x1 - 0.3 * x2 + b0[g] + b1[g] * x1 + b2[g] * x2 + rnorm(n)
  fit <- lme4::lmer(
    y ~ x1 + x2 + (1 + x1 + x2 | g),
    control = lme4::lmerControl(check.conv.singular = "ignore")
  )
  skip_if(lme4::isSingular(fit), "fit was singular this round")
  tm <- glmmTMB::glmmTMB(y ~ x1 + x2 + (1 + x1 + x2 | g), REML = TRUE)

  # glmmTMB: theta = (log SD x 3, correlation parameters x 3); delta
  # method to (SD, correlation) in lower-triangle column-major order:
  # sd1, cor21, cor31, sd2, cor32, sd3.
  est <- tm$sdr$par.fixed
  it <- which(names(est) == "theta")
  h <- function(th) {
    m <- glmmTMB::get_cor(th[4:6], return_val = "mat")
    diag(m) <- exp(th[1:3])
    m[lower.tri(m, diag = TRUE)]
  }
  J <- numDeriv::jacobian(h, est[it])
  se_tmb <- sqrt(diag(J %*% tm$sdr$cov.fixed[it, it] %*% t(J)))

  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  sd_rows <- vc[!(vc$is_correlation %in% TRUE) & vc$group == "g", ]
  expect_identical(sd_rows$term, c("(Intercept)", "x1", "x2"))
  expect_equal(
    sd_rows$std_error / (2 * sqrt(sd_rows$variance)),
    se_tmb[c(1L, 4L, 6L)],
    tolerance = 1e-3
  )
  corr <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(
    corr$term,
    c("(Intercept), x1", "(Intercept), x2", "x1, x2")
  )
  expect_equal(corr$std_error, se_tmb[c(2L, 3L, 5L)], tolerance = 1e-3)
})

test_that(".vech_colmajor_pos returns column-major lower-triangle positions", {
  # n = 3 block: diagonal at 1, 4, 6; off-diag (2,1)=2, (3,1)=3,
  # (3,2)=5 -- the layout of lme4's theta.
  expect_identical(spicy:::.vech_colmajor_pos(3L, 1L, 1L), 1L)
  expect_identical(spicy:::.vech_colmajor_pos(3L, 2L, 2L), 4L)
  expect_identical(spicy:::.vech_colmajor_pos(3L, 3L, 3L), 6L)
  expect_identical(spicy:::.vech_colmajor_pos(3L, 2L, 1L), 2L)
  expect_identical(spicy:::.vech_colmajor_pos(3L, 3L, 1L), 3L)
  expect_identical(spicy:::.vech_colmajor_pos(3L, 3L, 2L), 5L)
  # n = 2 coincides with row-major (why the bug hid): diag at 1, 3.
  expect_identical(spicy:::.vech_colmajor_pos(2L, 1L, 1L), 1L)
  expect_identical(spicy:::.vech_colmajor_pos(2L, 2L, 2L), 3L)
  expect_identical(spicy:::.vech_colmajor_pos(2L, 2L, 1L), 2L)
})


# ---- 3. glmer: rho row has finite SE + CI (no residual -> same path) ----

test_that("glmer (binomial logit) (x | g): rho row has finite Wald SE + CI", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_glmer_slope_corr()
  skip_if(lme4::isSingular(fit), "skip when glmer fit is singular")
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  rho_rows <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(rho_rows), 1L)
  expect_true(is.finite(rho_rows$std_error))
  expect_true(is.finite(rho_rows$ci_lower))
  expect_true(is.finite(rho_rows$ci_upper))
  expect_identical(rho_rows$ci_method, "wald")

  # Oracle: glmmTMB on the same model (the glmer deviance has no
  # residual, the (x | g) block is the whole parameter vector).
  skip_if_not_installed("glmmTMB")
  set.seed(42)
  n <- 1000
  g <- factor(rep(1:30, length.out = n))
  x <- rnorm(n)
  b0 <- rnorm(30)
  b1 <- rnorm(30, 0, 0.5)
  y <- rbinom(n, 1, plogis(0.5 + b0[g] + (0.8 + b1[g]) * x))
  tm <- glmmTMB::glmmTMB(y ~ x + (x | g), family = binomial)
  rho_est <- attr(lme4::VarCorr(fit)[["g"]], "correlation")[1L, 2L]
  expect_equal(rho_rows$corr, rho_est, tolerance = 1e-12)
  expect_equal(rho_rows$std_error, .tmb_rho_se(tm), tolerance = 5e-4)
  z <- qnorm(0.975)
  half <- z * rho_rows$std_error / (1 - rho_est^2)
  expect_equal(
    rho_rows$ci_lower,
    tanh(atanh(rho_est) - half),
    tolerance = 1e-12
  )
  expect_equal(
    rho_rows$ci_upper,
    tanh(atanh(rho_est) + half),
    tolerance = 1e-12
  )
})


# ---- 4. Engine parity: glmmTMB and lme still populate rho SE/CI --------

test_that("glmmTMB (Days | Subject): rho row has finite SE + CI", {
  fit <- .fit_glmmTMB_slope_corr()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  rho_rows <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(rho_rows), 1L)
  expect_true(all(is.finite(rho_rows$std_error)))

  # Oracle: glmmTMB's native Wald confint on the natural rho scale is
  # spicy's CI verbatim. The SE is the delta-method one from the scale
  # glmmTMB estimates a two-term correlation on, theta = rho / sqrt(1 -
  # rho^2): SE(theta) of its sdreport times d rho / d theta.
  ci_sd <- as.matrix(stats::confint(fit, method = "Wald", parm = "theta_"))
  cor_rn <- grep("^Cor\\.", rownames(ci_sd), value = TRUE)
  expect_identical(length(cor_rn), 1L)
  z <- qnorm(0.975)
  expect_equal(
    rho_rows$corr,
    unname(ci_sd[cor_rn, "Estimate"]),
    tolerance = 1e-12
  )
  expect_equal(rho_rows$ci_lower, unname(ci_sd[cor_rn, 1L]), tolerance = 1e-12)
  expect_equal(rho_rows$ci_upper, unname(ci_sd[cor_rn, 2L]), tolerance = 1e-12)
  sdr <- summary(fit$sdr, "fixed")
  th <- sdr[rownames(sdr) == "theta", , drop = FALSE]
  expect_identical(nrow(th), 3L)
  theta <- th[3L, "Estimate"]
  expect_equal(theta / sqrt(1 + theta^2), rho_rows$corr, tolerance = 1e-8)
  expect_equal(
    rho_rows$std_error,
    unname(th[3L, "Std. Error"] * (1 + theta^2)^(-3 / 2)),
    tolerance = 1e-5
  )
  # No longer the half-width of the interval, which is skewed on rho.
  half_width <- unname((ci_sd[cor_rn, 2L] - ci_sd[cor_rn, 1L]) / (2 * z))
  expect_gt(abs(rho_rows$std_error - half_width), 1e-3)
})

test_that("nlme::lme (age | Subject): rho row has finite SE + CI", {
  fit <- .fit_lme_slope_corr()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  rho_rows <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(rho_rows), 1L)
  expect_true(all(is.finite(rho_rows$std_error)))

  # Oracle: nlme::intervals() on the natural rho scale is spicy's CI
  # verbatim. The SE is the delta-method one from the scale nlme
  # estimates a correlation on, log((1 + rho) / (1 - rho)): the SE of
  # that parameter in `apVar` times d rho / d parameter.
  iv <- nlme::intervals(fit, level = 0.95, which = "var-cov")$reStruct$Subject
  cor_rn <- grep("^cor\\(", rownames(iv), value = TRUE)
  expect_identical(length(cor_rn), 1L)
  z <- qnorm(0.975)
  expect_equal(rho_rows$corr, unname(iv[cor_rn, "est."]), tolerance = 1e-12)
  expect_equal(
    rho_rows$ci_lower,
    unname(iv[cor_rn, "lower"]),
    tolerance = 1e-12
  )
  expect_equal(
    rho_rows$ci_upper,
    unname(iv[cor_rn, "upper"]),
    tolerance = 1e-12
  )
  rho <- rho_rows$corr
  se_nat <- sqrt(diag(fit$apVar))
  cor_par <- grep("^reStruct", names(se_nat), value = TRUE)[3L]
  expect_equal(
    rho_rows$std_error,
    unname(se_nat[cor_par] * (1 - rho^2) / 2),
    tolerance = 1e-6
  )
  # No longer the half-width of the interval, which is skewed on rho.
  half_width <- unname((iv[cor_rn, "upper"] - iv[cor_rn, "lower"]) / (2 * z))
  expect_gt(abs(rho_rows$std_error - half_width), 1e-3)
})


# ---- 5. End-to-end rendering: rho row carries SE + CI (rows layout) -----

test_that("table_regression(lmer slope) renders the rho row with SE + CI", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_slope_corr()
  # The printed table: its columns are the displayed cells.
  df <- table_regression(fit, show_columns = c("b", "se", "ci"))
  # the correlation row's label ends in ", Days)"; unique to it
  rho <- df[grepl(", Days)", df$Variable, fixed = TRUE), , drop = FALSE]
  expect_equal(nrow(rho), 1L)
  se_col <- grep("SE", names(df), value = TRUE)[1]
  ci_col <- grep("CI", names(df), value = TRUE)[1]
  expect_true(grepl("[0-9]", rho[[se_col]][1])) # SE populated, no en-dash
  expect_true(grepl("[0-9]", rho[[ci_col]][1])) # CI populated
})


# ---- 6. Single-term random structure: no rho row (regression guard) ----

test_that("lmer (1 | Subject) has NO rho row in variance_components", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  expect_false(any(vc$is_correlation %in% TRUE))
})


# ---- 7. Singular fit: rho SE/CI stays NA (regression guard) -------------

test_that("lme4::isSingular fits leave rho row at NA (no Delta on singular)", {
  skip_if_not_installed("lme4")
  set.seed(2)
  n <- 60
  g <- factor(rep(1:5, length.out = n))
  x <- rnorm(n)
  y <- 1 + 0.5 * x + rnorm(n)
  fit <- suppressMessages(suppressWarnings(
    lme4::lmer(y ~ x + (x | g))
  ))
  skip_if(!lme4::isSingular(fit), "fit happened to be non-singular")
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  rho_rows <- vc[vc$is_correlation %in% TRUE, ]
  if (nrow(rho_rows) > 0L) {
    expect_true(all(is.na(rho_rows$std_error)))
  }
})
