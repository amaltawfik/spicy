# ---------------------------------------------------------------------------
# Phase 1 tests: as_regression_frame() methods for lme4 mixed-effects fits.
#
# Coverage:
#   * lmer (lmerMod / lmerModLmerTest) -- fixed effects extraction,
#     Satterthwaite df via lmerTest when available, Wald fallback;
#     ICC + variance components from VarCorr().
#   * glmer (glmerMod) -- Wald z-asymptotic; family-aware title;
#     binomial / poisson / Gamma supported.
#   * Schema validity in all paths.
#   * Oracle cross-validation against parameters::model_parameters()
#     (skipped if not installed).
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_lmer_basic <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_lmer_factor <- function() {
  skip_if_not_installed("lme4")
  d <- lme4::sleepstudy
  # Inject a 3-level treatment factor so we exercise the reference-row
  # path. The fixed-effect block is `Days + treatment`; the grouping
  # factor stays `Subject`.
  d$treatment <- factor(rep(c("A", "B", "C"), length.out = nrow(d)))
  lme4::lmer(Reaction ~ Days + treatment + (1 | Subject), data = d)
}

.fit_glmer_logit <- function() {
  skip_if_not_installed("lme4")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  lme4::glmer(am ~ mpg + (1 | cyl), data = d, family = binomial(link = "logit"))
}

.fit_glmer_poisson <- function() {
  skip_if_not_installed("lme4")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$counter <- as.integer(d$gear)
  lme4::glmer(counter ~ mpg + (1 | cyl), data = d, family = poisson(link = "log"))
}

# Simulated grouped Bernoulli data with a REAL random-intercept variance.
# The mtcars-based glmer fixtures above are boundary-singular (var_r ~ 0),
# which would make any ICC pin degenerate (0 == 0). Fixed seed; 12 groups
# of 25 so the logit link-scale distribution-variance branch is exercised
# with a non-trivial ICC.
.sim_bernoulli_grouped_data <- function() {
  set.seed(42)
  d <- data.frame(
    g = factor(rep(seq_len(12L), each = 25L)),
    x = rnorm(300)
  )
  u <- rnorm(12L, sd = 1)
  eta <- -0.3 + 0.6 * d$x + u[as.integer(d$g)]
  d$y <- rbinom(300L, 1L, stats::plogis(eta))
  d
}

# Simulated grouped Poisson counts (same rationale). Fixed seed; 10 groups
# of 30; exercises the lognormal-approximation distribution variance.
.sim_poisson_grouped_data <- function() {
  set.seed(7)
  d <- data.frame(
    g = factor(rep(seq_len(10L), each = 30L)),
    x = rnorm(300)
  )
  u <- rnorm(10L, sd = 0.5)
  mu <- exp(0.8 + 0.3 * d$x + u[as.integer(d$g)])
  d$y <- rpois(300L, mu)
  d
}


# ---- 1. lmer: schema validity + core fields -------------------------------

test_that("as_regression_frame.lmerMod produces a schema-valid frame", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("as_regression_frame.lmerMod attaches required attributes", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("lmer: info$class is 'lmerMod' (stripped of lmerTest decoration)", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "lmerMod")
})

test_that("lmer: info$family is gaussian/identity", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "gaussian")
  expect_identical(fr$info$family$link,   "identity")
})

test_that("lmer: info$dv reads the response variable name", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "Reaction")
})

test_that("lmer: info$n_groups returns the per-group counts from lme4::ngrps()", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$n_groups, c(Subject = 18L))
})


# ---- 2. lmer: coef extraction -------------------------------------------

test_that("lmer: coefs estimates match lme4::fixef()", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- lme4::fixef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(b_rows$estimate[b_rows$term == nm],
                 unname(legacy[nm]),
                 tolerance = 1e-10,
                 info = paste("term:", nm))
  }
})

test_that("lmer: coefs SE matches sqrt(diag(vcov))", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  V <- as.matrix(stats::vcov(fit))
  expected_se <- sqrt(diag(V))
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(expected_se)) {
    expect_equal(b_rows$std_error[b_rows$term == nm],
                 unname(expected_se[nm]),
                 tolerance = 1e-10)
  }
})


# ---- 3. lmer: Satterthwaite df via lmerTest -----------------------------

test_that("lmer: lmerTest in NAMESPACE -> ci_method = 'satterthwaite' with finite df", {
  skip_if_not_installed("lmerTest")
  # When lmerTest is loaded, lme4::lmer() returns lmerModLmerTest.
  d <- lme4::sleepstudy
  fit <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), data = d)
  expect_true(inherits(fit, "lmerModLmerTest"))
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "satterthwaite")
  expect_true(all(is.finite(fr$coefs$df)))
  expect_true(all(fr$coefs$test_type == "t"))
  # Pin df / t / p to the lmerTest Satterthwaite summary itself (oracle):
  # the frame must carry the summary values verbatim, not a recomputation.
  smc <- summary(fit)$coefficients
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  idx <- match(rownames(smc), b_rows$term)
  expect_equal(b_rows$df[idx],        unname(smc[, "df"]),
               tolerance = 1e-10)
  expect_equal(b_rows$statistic[idx], unname(smc[, "t value"]),
               tolerance = 1e-10)
  expect_equal(b_rows$p_value[idx],   unname(smc[, "Pr(>|t|)"]),
               tolerance = 1e-10)
})

test_that("lmer: without lmerTest -> ci_method = 'wald', test_type = 'z', df = Inf", {
  # Phase 7c8a: changed from naive-t (df.residual) to Wald-z (df = Inf).
  # Naive-t double-counts the within-cluster correlation and is
  # methodologically wrong; Wald-z matches parameters::model_parameters
  # (ci_method = "wald"), SAS PROC MIXED (ddfm=z), and Stata xtmixed
  # (large-sample default).
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "z"))
  expect_true(all(!is.finite(fr$coefs$df)))
  # Pin the Wald-z algebra (hand-derived identities): z = B / SE,
  # p = 2 * pnorm(-|z|), CI = B +/- qnorm(0.975) * SE at ci_level 0.95.
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  z <- b_rows$estimate / b_rows$std_error
  expect_equal(b_rows$statistic, z,                        tolerance = 1e-12)
  expect_equal(b_rows$p_value,   2 * stats::pnorm(-abs(z)), tolerance = 1e-12)
  expect_equal(b_rows$ci_lower,
               b_rows$estimate - stats::qnorm(0.975) * b_rows$std_error,
               tolerance = 1e-12)
  expect_equal(b_rows$ci_upper,
               b_rows$estimate + stats::qnorm(0.975) * b_rows$std_error,
               tolerance = 1e-12)
})


# ---- 4. lmer: random effects + ICC --------------------------------------

test_that("lmer: random_effects$variance_components has one row per (group, term) + residual", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  # 1 group term (Subject Intercept) + 1 residual
  expect_identical(nrow(vc), 2L)
  expect_setequal(vc$group, c("Subject", "Residual"))
})

test_that("lmer: random_effects$icc is in (0, 1) for a single random-intercept model", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  icc <- fr$info$random_effects$icc
  expect_true(is.finite(icc))
  expect_true(icc > 0 & icc < 1)
  # Pin to the closed form computed from lme4::VarCorr() directly
  # (external oracle, NOT spicy's own variance_components):
  # ICC = var_ranef / (var_ranef + var_resid).
  vc_o <- as.data.frame(lme4::VarCorr(fit))
  var_r <- vc_o$vcov[vc_o$grp == "Subject"]
  var_e <- vc_o$vcov[vc_o$grp == "Residual"]
  expect_equal(icc, var_r / (var_r + var_e), tolerance = 1e-10)
})

test_that("lmer: ICC matches the canonical variance ratio var_random / (var_random + var_resid)", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  var_r <- vc$variance[vc$group == "Subject"]
  var_e <- vc$variance[vc$group == "Residual"]
  # Anchor the components to lme4::VarCorr() (external oracle) first, so
  # the ratio identity below is not self-referential.
  vc_o <- as.data.frame(lme4::VarCorr(fit))
  expect_equal(var_r, vc_o$vcov[vc_o$grp == "Subject"],   tolerance = 1e-12)
  expect_equal(var_e, vc_o$vcov[vc_o$grp == "Residual"],  tolerance = 1e-12)
  expect_equal(vc$sd[vc$group == "Subject"],
               vc_o$sdcor[vc_o$grp == "Subject"],          tolerance = 1e-12)
  expect_equal(var_e, stats::sigma(fit)^2,                 tolerance = 1e-12)
  expect_equal(fr$info$random_effects$icc,
               var_r / (var_r + var_e),
               tolerance = 1e-10)
})

test_that("lmer: ICC matches performance::icc() adjusted ICC (oracle)", {
  skip_if_not_installed("performance")
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  oracle <- performance::icc(fit)
  expect_equal(fr$info$random_effects$icc, oracle$ICC_adjusted,
               tolerance = 1e-10)
})


# ---- 4b. glmer: adjusted ICC via link-scale distribution variance -------

test_that("glmer logit: ICC matches var_r / (var_r + pi^2/3) from lme4::VarCorr (oracle)", {
  skip_if_not_installed("lme4")
  d <- .sim_bernoulli_grouped_data()
  fit <- lme4::glmer(y ~ x + (1 | g), data = d,
                     family = binomial(link = "logit"))
  expect_false(lme4::isSingular(fit))
  fr <- as_regression_frame(fit, model_id = "M1")
  # Nakagawa et al. (2017) adjusted ICC for Bernoulli-logit: the residual
  # term is the logistic distribution variance pi^2 / 3.
  var_r <- lme4::VarCorr(fit)$g["(Intercept)", "(Intercept)"]
  expect_equal(fr$info$random_effects$icc,
               var_r / (var_r + pi^2 / 3),
               tolerance = 1e-10)
})

test_that("glmer poisson: ICC matches the lognormal-approximation oracle (null-model refit)", {
  skip_if_not_installed("lme4")
  d <- .sim_poisson_grouped_data()
  fit <- lme4::glmer(y ~ x + (1 | g), data = d,
                     family = poisson(link = "log"))
  expect_false(lme4::isSingular(fit))
  fr <- as_regression_frame(fit, model_id = "M1")
  # Nakagawa et al. (2017) sec. 3.6, observation-level lognormal
  # approximation: lambda = exp(b0_null + var_g_null / 2) from the NULL
  # model y ~ 1 + (1 | g); distribution variance = log(1 + 1/lambda);
  # ICC = var_r_full / (var_r_full + log1p(1/lambda)). Refit involved,
  # hence the looser tolerance.
  null_fit <- lme4::glmer(y ~ 1 + (1 | g), data = d,
                          family = poisson(link = "log"))
  b0     <- unname(lme4::fixef(null_fit)[1L])
  var_g0 <- lme4::VarCorr(null_fit)$g["(Intercept)", "(Intercept)"]
  lambda <- exp(b0 + 0.5 * var_g0)
  var_r  <- lme4::VarCorr(fit)$g["(Intercept)", "(Intercept)"]
  expect_equal(fr$info$random_effects$icc,
               var_r / (var_r + log1p(1 / lambda)),
               tolerance = 1e-8)
})

test_that("glmer poisson: ICC matches performance::icc() adjusted ICC (oracle)", {
  skip_if_not_installed("performance")
  skip_if_not_installed("lme4")
  d <- .sim_poisson_grouped_data()
  fit <- lme4::glmer(y ~ x + (1 | g), data = d,
                     family = poisson(link = "log"))
  fr <- as_regression_frame(fit, model_id = "M1")
  oracle <- performance::icc(fit)
  expect_equal(fr$info$random_effects$icc, oracle$ICC_adjusted,
               tolerance = 1e-8)
})


# ---- 4c. lmer random slope: correlation row pinned to merDeriv ----------

test_that("lmer random slope: correlation SE matches the merDeriv Delta-method oracle", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("merDeriv")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject),
                    data = lme4::sleepstudy)
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components

  # Point estimate: VarCorr's own correlation attribute (oracle).
  g_vc <- lme4::VarCorr(fit)$Subject
  rho <- attr(g_vc, "correlation")["(Intercept)", "Days"]
  corr_row <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(corr_row), 1L)
  expect_equal(corr_row$corr, rho, tolerance = 1e-12)

  # merDeriv full vcov on the variance scale. Layout for this fit:
  # 2 fixed effects, then the column-major vech of the 2 x 2 Subject
  # block -- var(Int), cov(Int, Days), var(Days) at 3:5 -- and the
  # residual variance at 6 (colnames cov_Subject.*, residual).
  v <- as.matrix(merDeriv::vcov.lmerMod(fit, full = TRUE, ranpar = "var"))
  se_full <- sqrt(diag(v))

  # Variance rows (Subject Intercept, Subject Days, Residual) carry the
  # diagonal SEs of the RE block.
  var_rows <- vc[!(vc$is_correlation %in% TRUE), ]
  expect_equal(var_rows$std_error, unname(se_full[c(3L, 5L, 6L)]),
               tolerance = 1e-10)

  # Delta method for rho = cov / sqrt(var_i * var_j):
  #   d rho / d var_i = -rho / (2 var_i)
  #   d rho / d cov   = 1 / sqrt(var_i * var_j)
  #   d rho / d var_j = -rho / (2 var_j)
  # Var(rho) = grad' Sigma_3 grad on the (var_i, cov, var_j) sub-vcov.
  var_i <- g_vc["(Intercept)", "(Intercept)"]
  var_j <- g_vc["Days", "Days"]
  grad <- c(-rho / (2 * var_i),
            1 / sqrt(var_i * var_j),
            -rho / (2 * var_j))
  se_rho <- sqrt(as.numeric(t(grad) %*% v[3:5, 3:5] %*% grad))
  expect_equal(corr_row$std_error, se_rho, tolerance = 1e-10)
  expect_equal(corr_row$ci_lower,
               max(-1, rho - stats::qnorm(0.975) * se_rho),
               tolerance = 1e-10)
  expect_equal(corr_row$ci_upper,
               min(1, rho + stats::qnorm(0.975) * se_rho),
               tolerance = 1e-10)
})


# ---- 5. lmer: fit statistics --------------------------------------------

test_that("lmer: fit_stats$r_squared is NA (classical R^2 not defined)", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.na(fr$info$fit_stats$r_squared))
  expect_true(is.na(fr$info$fit_stats$adj_r_squared))
})

test_that("lmer: fit_stats$sigma matches lme4::sigma()", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$sigma, stats::sigma(fit),
               tolerance = 1e-10)
})

test_that("lmer: fit_stats$aic / bic / log_lik match stats:: helpers", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$aic, stats::AIC(fit), tolerance = 1e-10)
  expect_equal(fr$info$fit_stats$bic, stats::BIC(fit), tolerance = 1e-10)
  expect_equal(fr$info$fit_stats$log_lik, as.numeric(stats::logLik(fit)),
               tolerance = 1e-10)
})


# ---- 6. lmer: supports capabilities -------------------------------------

test_that("lmer: supports flags are correct", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sp <- fr$info$supports
  expect_true(sp$ame)
  expect_false(sp$partial_effect_size)
  expect_false(sp$classical_r2)
  expect_true(sp$nested_lrt)
  expect_false(sp$exponentiate)
  expect_true(sp$standardise_refit)
})


# ---- 7. lmer with factor predictor: reference rows ----------------------

test_that("lmer: factor predictor produces a reference row + non-ref level rows", {
  fit <- .fit_lmer_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  # `treatment` has 3 levels; 1 ref row + 2 non-ref rows
  treat_rows <- fr$coefs[fr$coefs$parent_var == "treatment", ]
  expect_identical(nrow(treat_rows), 3L)
  expect_identical(sum(treat_rows$is_ref), 1L)
  # Ref row's estimate is NA per the validator's is_ref invariant.
  expect_true(all(is.na(treat_rows$estimate[treat_rows$is_ref])))
})


# ---- 8. glmer: schema validity + class-specific bits --------------------

test_that("as_regression_frame.glmerMod produces a schema-valid frame", {
  fit <- .fit_glmer_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("glmer: info$class is 'glmerMod'", {
  fit <- .fit_glmer_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "glmerMod")
})

test_that("glmer logit: info$family is binomial/logit", {
  fit <- .fit_glmer_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "binomial")
  expect_identical(fr$info$family$link,   "logit")
})

test_that("glmer: ci_method = 'wald' and test_type = 'z' with df = Inf", {
  fit <- .fit_glmer_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "z"))
  expect_true(all(is.infinite(fr$coefs$df)))
  # Pin the Wald-z algebra: the summary-sourced z / p must satisfy
  # z = B / SE and p = 2 * pnorm(-|z|) (same vcov source both sides).
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_equal(b_rows$statistic, b_rows$estimate / b_rows$std_error,
               tolerance = 1e-10)
  expect_equal(b_rows$p_value, 2 * stats::pnorm(-abs(b_rows$statistic)),
               tolerance = 1e-10)
})

test_that("glmer: supports$exponentiate = TRUE (non-identity link)", {
  fit <- .fit_glmer_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})

test_that("glmer poisson: family + title_prefix reflect Poisson", {
  fit <- .fit_glmer_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "poisson")
  expect_identical(fr$info$extras$title_prefix,
                   "Poisson mixed-effects regression")
})


# ---- 9. lmer + glmer: title_prefix reflects the family ------------------

test_that("lmer: title_prefix = 'Linear mixed-effects regression'", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix,
                   "Linear mixed-effects regression")
})

test_that("glmer logit: title_prefix names the Logistic family", {
  fit <- .fit_glmer_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix,
                   "Logistic mixed-effects regression")
})


# ---- 10. Oracle: parameters::model_parameters() ---------------------------

test_that("lmer coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  skip_if_not_installed("lmerTest")
  d <- lme4::sleepstudy
  fit <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), data = d)
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci         = 0.95,
    ci_method  = "satterthwaite",
    test       = NULL,
    effects    = "fixed"
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row  <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(spicy_row$estimate,  oracle_row$Coefficient, tolerance = 1e-7,
                 info = paste("oracle B mismatch on term:", nm))
    expect_equal(spicy_row$std_error, oracle_row$SE,          tolerance = 1e-7,
                 info = paste("oracle SE mismatch on term:", nm))
    expect_equal(spicy_row$p_value,   oracle_row$p,           tolerance = 1e-7,
                 info = paste("oracle p mismatch on term:", nm))
    expect_equal(spicy_row$df,        oracle_row$df_error,    tolerance = 1e-2,
                 info = paste("oracle df mismatch on term:", nm))
  }
})

test_that("glmer coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_glmer_logit()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci         = 0.95,
    ci_method  = "wald",
    test       = NULL,
    effects    = "fixed",
    exponentiate = FALSE
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row  <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(spicy_row$estimate,  oracle_row$Coefficient, tolerance = 1e-6,
                 info = paste("oracle B mismatch on term:", nm))
    expect_equal(spicy_row$std_error, oracle_row$SE,          tolerance = 1e-6,
                 info = paste("oracle SE mismatch on term:", nm))
    expect_equal(spicy_row$p_value,   oracle_row$p,           tolerance = 1e-6,
                 info = paste("oracle p mismatch on term:", nm))
  }
})
