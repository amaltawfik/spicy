# ---------------------------------------------------------------------------
# Phase 7c9a tests: Nakagawa & Schielzeth (2013) marginal / conditional R^2
# for mixed-effects fits. Covers schema population, performance oracle,
# Gaussian closed-form cross-validation, default token injection, and
# end-to-end rendering.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_lmer_ns <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_glmer_ns <- function() {
  skip_if_not_installed("lme4")
  suppressMessages(suppressWarnings(
    lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                 data = lme4::cbpp, family = binomial)
  ))
}

.fit_glmmTMB_ns <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_lme_ns <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(distance ~ age, data = nlme::Orthodont, random = ~ 1 | Subject)
}


# ---- 1. Schema: fit_stats carries r2_marginal + r2_conditional ----------

test_that("lmer fit_stats has r2_marginal and r2_conditional fields", {
  skip_if_not_installed("performance")
  fit <- .fit_lmer_ns()
  fr <- as_regression_frame(fit, model_id = "M1")
  fs <- fr$info$fit_stats
  expect_true("r2_marginal" %in% names(fs))
  expect_true("r2_conditional" %in% names(fs))
  expect_true(is.finite(fs$r2_marginal))
  expect_true(is.finite(fs$r2_conditional))
})

test_that("glmer fit_stats has r2_marginal and r2_conditional fields", {
  skip_if_not_installed("performance")
  fit <- .fit_glmer_ns()
  fr <- as_regression_frame(fit, model_id = "M1")
  fs <- fr$info$fit_stats
  expect_true("r2_marginal" %in% names(fs))
  expect_true("r2_conditional" %in% names(fs))
})

test_that("glmmTMB fit_stats has r2_marginal and r2_conditional fields", {
  skip_if_not_installed("performance")
  fit <- .fit_glmmTMB_ns()
  fr <- as_regression_frame(fit, model_id = "M1")
  fs <- fr$info$fit_stats
  expect_true("r2_marginal" %in% names(fs))
  expect_true("r2_conditional" %in% names(fs))
  expect_true(is.finite(fs$r2_marginal))
  expect_true(is.finite(fs$r2_conditional))
})

test_that("lme fit_stats has r2_marginal and r2_conditional fields", {
  skip_if_not_installed("performance")
  fit <- .fit_lme_ns()
  fr <- as_regression_frame(fit, model_id = "M1")
  fs <- fr$info$fit_stats
  expect_true("r2_marginal" %in% names(fs))
  expect_true("r2_conditional" %in% names(fs))
  expect_true(is.finite(fs$r2_marginal))
  expect_true(is.finite(fs$r2_conditional))
})


# ---- 2. Oracle: matches performance::r2_nakagawa() directly --------------

test_that("lmer R^2 matches performance::r2_nakagawa() to 1e-10", {
  skip_if_not_installed("performance")
  fit <- .fit_lmer_ns()
  fr <- as_regression_frame(fit, model_id = "M1")
  fs <- fr$info$fit_stats
  oracle <- performance::r2_nakagawa(fit)
  expect_equal(fs$r2_marginal,    as.numeric(oracle$R2_marginal),
               tolerance = 1e-10)
  expect_equal(fs$r2_conditional, as.numeric(oracle$R2_conditional),
               tolerance = 1e-10)
})

test_that("glmmTMB R^2 matches performance::r2_nakagawa() to 1e-10", {
  skip_if_not_installed("performance")
  fit <- .fit_glmmTMB_ns()
  fr <- as_regression_frame(fit, model_id = "M1")
  fs <- fr$info$fit_stats
  oracle <- performance::r2_nakagawa(fit)
  expect_equal(fs$r2_marginal,    as.numeric(oracle$R2_marginal),
               tolerance = 1e-10)
  expect_equal(fs$r2_conditional, as.numeric(oracle$R2_conditional),
               tolerance = 1e-10)
})

test_that("lme R^2 matches performance::r2_nakagawa() to 1e-10", {
  skip_if_not_installed("performance")
  fit <- .fit_lme_ns()
  fr <- as_regression_frame(fit, model_id = "M1")
  fs <- fr$info$fit_stats
  oracle <- performance::r2_nakagawa(fit)
  expect_equal(fs$r2_marginal,    as.numeric(oracle$R2_marginal),
               tolerance = 1e-10)
  expect_equal(fs$r2_conditional, as.numeric(oracle$R2_conditional),
               tolerance = 1e-10)
})


# ---- 3. Closed-form: Gaussian sleepstudy intercept-only model -----------

test_that("Gaussian lmer R^2 matches manual variance-decomposition formula", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("performance")
  fit <- .fit_lmer_ns()
  # Manual Nakagawa formula for Gaussian: see Nakagawa & Schielzeth
  # (2013) Eq. 26 + 29. var_f = var(X * beta_fixed); var_g = sum of
  # group variances; var_e = sigma^2.
  X <- lme4::getME(fit, "X")
  betaf <- lme4::fixef(fit)
  var_f <- as.numeric(stats::var(as.vector(X %*% betaf)))
  vc <- as.data.frame(lme4::VarCorr(fit))
  var_g <- sum(vc$vcov[vc$grp != "Residual"])
  var_e <- vc$vcov[vc$grp == "Residual"]
  r2_m <- var_f / (var_f + var_g + var_e)
  r2_c <- (var_f + var_g) / (var_f + var_g + var_e)

  fr <- as_regression_frame(fit, model_id = "M1")
  fs <- fr$info$fit_stats
  expect_equal(fs$r2_marginal,    r2_m, tolerance = 1e-6)
  expect_equal(fs$r2_conditional, r2_c, tolerance = 1e-6)
})


# ---- 4. Token vocabulary accepts the new tokens -------------------------

test_that(".regression_tokens$show_fit_stats includes the two new tokens", {
  toks <- spicy:::.regression_tokens$show_fit_stats
  expect_true("r2_marginal"    %in% toks)
  expect_true("r2_conditional" %in% toks)
})

test_that("validate_show_fit_stats accepts r2_marginal + r2_conditional", {
  expect_silent(
    spicy:::validate_show_fit_stats(c("nobs", "r2_marginal", "r2_conditional"))
  )
})


# ---- 5. Renderer label + precision --------------------------------------

test_that("fit_stat_label() returns the APA-style label for the two tokens", {
  expect_identical(spicy:::fit_stat_label("r2_marginal"),    "R² (marginal)")
  expect_identical(spicy:::fit_stat_label("r2_conditional"), "R² (conditional)")
})


# ---- 6. Default class-aware show_fit_stats picks Nakagawa for mixed -----

test_that("default show_fit_stats for lmer includes Nakagawa R^2 + AIC + BIC", {
  skip_if_not_installed("performance")
  fit <- .fit_lmer_ns()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "n",                fixed = TRUE)
  expect_match(combined, "R² (marginal)",     fixed = TRUE)
  expect_match(combined, "R² (conditional)",  fixed = TRUE)
  expect_match(combined, "AIC",              fixed = TRUE)
  expect_match(combined, "BIC",              fixed = TRUE)
})

test_that("default show_fit_stats for lme includes Nakagawa R^2 + AIC + BIC", {
  skip_if_not_installed("performance")
  fit <- .fit_lme_ns()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "R² (marginal)",     fixed = TRUE)
  expect_match(combined, "R² (conditional)",  fixed = TRUE)
})


# ---- 7. lm / glm do NOT pick up Nakagawa (regression guard) -------------

test_that("lm output still uses classical R^2, NOT Nakagawa", {
  fit <- lm(mpg ~ wt + cyl, data = mtcars)
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "R²",          fixed = TRUE)
  expect_false(grepl("R² (marginal)",     combined, fixed = TRUE))
  expect_false(grepl("R² (conditional)",  combined, fixed = TRUE))
})


# ---- 8. Graceful fallback: NA when performance is unavailable -----------

test_that(".nakagawa_r2 returns NA list when performance is missing", {
  # Simulate performance-missing by stubbing requireNamespace.
  with_mocked_bindings(
    {
      out <- spicy:::.nakagawa_r2(list())  # input doesn't matter
      expect_true(is.list(out))
      expect_true(is.na(out$marginal))
      expect_true(is.na(out$conditional))
    },
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
})


# ---- Self-impl coverage gate --------------------------------------------

test_that("self-impl handles Gaussian lmer without falling through to performance", {
  skip_if_not_installed("lme4")
  fit <- .fit_lmer_ns()
  comps <- spicy:::.nakagawa_components(fit)
  expect_true(!is.null(comps))
  expect_identical(comps$family, "gaussian")
})

test_that("self-impl handles Bernoulli glmer without falling through", {
  skip_if_not_installed("lme4")
  set.seed(1)
  n <- 300; g <- factor(rep(1:15, length.out = n))
  x <- rnorm(n); b0 <- rnorm(15)[g]
  y <- rbinom(n, 1, plogis(0.5 + 0.8 * x + b0))
  fit <- lme4::glmer(y ~ x + (1 | g), family = binomial)
  comps <- spicy:::.nakagawa_components(fit)
  expect_true(!is.null(comps))
  expect_identical(comps$family, "binomial")
  expect_identical(comps$link, "logit")
})

test_that("self-impl refuses cbind() binomial -> falls through to performance", {
  skip_if_not_installed("lme4")
  fit <- .fit_glmer_ns()  # cbpp uses cbind(succ, fail)
  comps <- spicy:::.nakagawa_components(fit)
  expect_null(comps)  # gate rejects it
  # But .nakagawa_r2() still returns finite values via fallback
  skip_if_not_installed("performance")
  out <- spicy:::.nakagawa_r2(fit)
  expect_true(is.finite(out$marginal))
  expect_true(is.finite(out$conditional))
})

test_that("self-impl handles Poisson(log) glmer without falling through", {
  skip_if_not_installed("lme4")
  set.seed(2)
  n <- 300; g <- factor(rep(1:15, length.out = n))
  x <- rnorm(n); b0 <- rnorm(15, 0, 0.3)[g]
  y <- rpois(n, exp(1 + 0.3 * x + b0))
  fit <- lme4::glmer(y ~ x + (1 | g), family = poisson)
  comps <- spicy:::.nakagawa_components(fit)
  expect_true(!is.null(comps))
  expect_identical(comps$family, "poisson")
})

test_that("self-impl matches oracle for Poisson(log) glmer", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("performance")
  set.seed(2)
  n <- 300; g <- factor(rep(1:15, length.out = n))
  x <- rnorm(n); b0 <- rnorm(15, 0, 0.3)[g]
  y <- rpois(n, exp(1 + 0.3 * x + b0))
  fit <- lme4::glmer(y ~ x + (1 | g), family = poisson)
  out <- spicy:::.nakagawa_r2(fit)
  oracle <- performance::r2_nakagawa(fit)
  expect_equal(out$marginal,    as.numeric(oracle$R2_marginal),
               tolerance = 1e-10)
  expect_equal(out$conditional, as.numeric(oracle$R2_conditional),
               tolerance = 1e-10)
})

test_that(".nakagawa_supported_family rejects binomial cbind", {
  skip_if_not_installed("lme4")
  fit <- .fit_glmer_ns()
  expect_false(spicy:::.nakagawa_supported_family(fit))
})

test_that(".nakagawa_supported_family accepts Gaussian + Bernoulli + Poisson(log)", {
  expect_true(spicy:::.nakagawa_supported_family(
    lm(mpg ~ wt, data = mtcars)
  ))  # Gaussian (any LM/GLM with gaussian family)
  expect_true(spicy:::.nakagawa_supported_family(
    glm(am ~ mpg, data = mtcars, family = binomial)
  ))  # Bernoulli
  expect_true(spicy:::.nakagawa_supported_family(
    glm(cyl ~ mpg, data = mtcars, family = poisson)
  ))  # Poisson(log)
})


# ---- 9. User-supplied show_fit_stats overrides the default --------------

test_that("user-supplied show_fit_stats wins over the mixed-default", {
  skip_if_not_installed("lme4")
  fit <- .fit_lmer_ns()
  out <- capture.output(print(
    table_regression(fit, show_fit_stats = c("nobs", "AIC"))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "n",   fixed = TRUE)
  expect_match(combined, "AIC", fixed = TRUE)
  expect_false(grepl("R² (marginal)",    combined, fixed = TRUE))
  expect_false(grepl("R² (conditional)", combined, fixed = TRUE))
})
