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
})

test_that("lmer: without lmerTest -> ci_method = 'wald'", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "t"))
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
})

test_that("lmer: ICC matches the canonical variance ratio var_random / (var_random + var_resid)", {
  fit <- .fit_lmer_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  var_r <- vc$variance[vc$group == "Subject"]
  var_e <- vc$variance[vc$group == "Residual"]
  expect_equal(fr$info$random_effects$icc,
               var_r / (var_r + var_e),
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
  expect_match(fr$info$extras$title_prefix, "Poisson", fixed = TRUE)
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
  expect_match(fr$info$extras$title_prefix, "Logistic", fixed = TRUE)
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
