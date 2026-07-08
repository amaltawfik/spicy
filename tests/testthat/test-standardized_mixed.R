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
  # Oracle: spicy's beta rows are the fixed effects of an lmer refit on
  # z-scored data -- reproduce that refit here and pin est + SE.
  d <- lme4::sleepstudy
  d$Reaction <- as.numeric(scale(d$Reaction))
  d$Days     <- as.numeric(scale(d$Days))
  refit <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = d)
  be <- fr$coefs[fr$coefs$estimate_type == "beta", ]
  be <- be[match(names(lme4::fixef(refit)), be$term), ]
  expect_equal(be$estimate, unname(lme4::fixef(refit)), tolerance = 1e-10)
  expect_equal(be$std_error,
               unname(sqrt(diag(as.matrix(stats::vcov(refit))))),
               tolerance = 1e-10)
})

test_that("glmer + standardized='refit' injects beta rows", {
  fit <- .fit_glmer_std()
  fr <- as_regression_frame(fit, model_id = "M1",
                             show_columns = c("b", "beta"),
                             standardized = "refit")
  expect_true("beta" %in% fr$coefs$estimate_type)
  # Oracle: refit glmer with x z-scored (predictor-only scaling; the
  # binary response stays 0/1) and pin the beta row's est + SE to it.
  mf <- stats::model.frame(fit)
  dd <- data.frame(y = mf$y, x = as.numeric(scale(mf$x)), g = mf$g)
  refit <- lme4::glmer(y ~ x + (1 | g), data = dd, family = binomial)
  be_x <- fr$coefs[fr$coefs$estimate_type == "beta" & fr$coefs$term == "x", ]
  expect_equal(be_x$estimate, lme4::fixef(refit)[["x"]], tolerance = 1e-10)
  expect_equal(be_x$std_error,
               sqrt(diag(as.matrix(stats::vcov(refit))))[["x"]],
               tolerance = 1e-10)
})

test_that("glmmTMB + standardized='refit' injects beta rows", {
  fit <- .fit_glmmTMB_std()
  fr <- as_regression_frame(fit, model_id = "M1",
                             show_columns = c("b", "beta"),
                             standardized = "refit")
  expect_true("beta" %in% fr$coefs$estimate_type)
  # Oracle: reproduce the z-scored glmmTMB refit and pin est + SE.
  d <- lme4::sleepstudy
  d$Reaction <- as.numeric(scale(d$Reaction))
  d$Days     <- as.numeric(scale(d$Days))
  refit <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = d)
  be_days <- fr$coefs[fr$coefs$estimate_type == "beta" &
                        fr$coefs$term == "Days", ]
  expect_equal(be_days$estimate, glmmTMB::fixef(refit)$cond[["Days"]],
               tolerance = 1e-10)
  expect_equal(be_days$std_error,
               sqrt(diag(as.matrix(stats::vcov(refit)$cond)))[["Days"]],
               tolerance = 1e-10)
})

test_that("lme + standardized='refit' injects beta rows", {
  fit <- .fit_lme_std()
  fr <- as_regression_frame(fit, model_id = "M1",
                             show_columns = c("b", "beta"),
                             standardized = "refit")
  expect_true("beta" %in% fr$coefs$estimate_type)
  # Oracle: reproduce the z-scored lme refit (all numeric columns of
  # getData() are scaled, factors untouched) and pin est + SE.
  d <- nlme::Orthodont
  d$distance <- as.numeric(scale(d$distance))
  d$age      <- as.numeric(scale(d$age))
  refit <- nlme::lme(distance ~ age, data = d, random = ~ 1 | Subject)
  be_age <- fr$coefs[fr$coefs$estimate_type == "beta" &
                       fr$coefs$term == "age", ]
  expect_equal(be_age$estimate, nlme::fixef(refit)[["age"]],
               tolerance = 1e-10)
  expect_equal(be_age$std_error,
               sqrt(diag(as.matrix(stats::vcov(refit))))[["age"]],
               tolerance = 1e-10)
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
  # REML estimates are exactly equivariant under the z-scoring, so the
  # refit reproduces the sd-ratio formula up to optimizer tolerance
  # (observed agreement ~2e-15 relative; 1e-8 leaves optimizer slack).
  expect_equal(beta_days, expected_beta, tolerance = 1e-8)
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
  # B is the original fit's fixed effect, untouched.
  expect_equal(b_x, lme4::fixef(fit)[["x"]], tolerance = 1e-12)
  # Latent-scale oracle: scaling x reparametrises the logistic linear
  # predictor exactly, so beta = B * sd(x). The identity only holds up
  # to glmer's joint (theta, beta) optimizer tolerance across the two
  # parametrisations (observed rel. diff ~4e-6), hence 1e-4 -- still
  # tight enough to catch a spurious 1/sd(y) factor (~2x here). The
  # exact-refit oracle below pins spicy's actual computation at 1e-10.
  mf <- stats::model.frame(fit)
  expect_equal(beta_x, b_x * stats::sd(mf$x), tolerance = 1e-4)
  dd <- data.frame(y = mf$y, x = as.numeric(scale(mf$x)), g = mf$g)
  refit <- lme4::glmer(y ~ x + (1 | g), data = dd, family = binomial)
  expect_equal(beta_x, lme4::fixef(refit)[["x"]], tolerance = 1e-10)
})


# ---- 4. No-op when standardized = 'none' --------------------------

test_that("standardized = 'none' does NOT inject beta rows", {
  fit <- .fit_lmer_std()
  fr <- as_regression_frame(fit, model_id = "M1",
                             show_columns = c("b"),
                             standardized = "none")
  expect_false("beta" %in% fr$coefs$estimate_type)
  # ... and the B rows are the original fit's fixed effects, untouched.
  b_days <- fr$coefs$estimate[fr$coefs$estimate_type == "B" &
                                fr$coefs$term == "Days"]
  expect_equal(b_days, lme4::fixef(fit)[["Days"]], tolerance = 1e-12)
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
  # Pin the printed Days row: B = fixef of the original fit, beta = the
  # sd-ratio oracle, both at the table's 2-decimal formatting.
  d <- lme4::sleepstudy
  b_str    <- sprintf("%.2f", lme4::fixef(fit)[["Days"]])
  beta_str <- sprintf("%.2f", lme4::fixef(fit)[["Days"]] *
                        stats::sd(d$Days) / stats::sd(d$Reaction))
  days_line <- out[grepl("^ Days ", out)]
  expect_length(days_line, 1L)
  expect_match(days_line, b_str, fixed = TRUE)
  expect_match(days_line, beta_str, fixed = TRUE)
})

test_that("table_regression(glmer, standardized='refit') prints beta column", {
  fit <- .fit_glmer_std()
  out <- capture.output(print(
    table_regression(fit, standardized = "refit")
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "β", fixed = TRUE)
  # Pin the printed x row: B from the original fit, beta from the
  # latent-scale oracle beta = B * sd(x), at 2-decimal formatting.
  mf <- stats::model.frame(fit)
  b_str    <- sprintf("%.2f", lme4::fixef(fit)[["x"]])
  beta_str <- sprintf("%.2f", lme4::fixef(fit)[["x"]] * stats::sd(mf$x))
  x_line <- out[grepl("^ x ", out)]
  expect_length(x_line, 1L)
  expect_match(x_line, b_str, fixed = TRUE)
  expect_match(x_line, beta_str, fixed = TRUE)
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

test_that("M1: beta rows inherit the B rows' Satterthwaite reference (lmerTest)", {
  skip_if_not_installed("lmerTest")
  fit <- lmerTest::lmer(Reaction ~ Days + (1 | Subject),
                        data = lme4::sleepstudy)
  fr <- as_regression_frame(fit, standardized = "refit",
                            show_columns = c("b", "beta", "p"))
  cf <- fr$coefs
  b  <- cf[cf$term == "Days" & cf$estimate_type == "B", ]
  be <- cf[cf$term == "Days" & cf$estimate_type == "beta", ]
  # Was: beta carried Wald z (df = Inf) next to B's Satterthwaite t --
  # same statistic, different p in the same table.
  expect_identical(be$test_type, "t")
  expect_equal(be$df, b$df, tolerance = 1e-6)
  expect_equal(be$p_value, b$p_value, tolerance = 1e-6)
  # CI rebuilt from the t critical value
  crit <- qt(0.975, df = be$df)
  expect_equal(be$ci_lower, be$estimate - crit * be$std_error,
               tolerance = 1e-10)
})
