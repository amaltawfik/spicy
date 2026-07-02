# ---------------------------------------------------------------------------
# Phase 4a tests: as_regression_frame() method for glmmTMB fits.
#
# Coverage:
#   * Gaussian (Linear mixed) -- vcov, ICC, variance components.
#   * Binomial (Logistic mixed) -- Wald z, exponentiate support.
#   * Poisson (Count mixed).
#   * Zero-inflated Poisson -- zi_coefs / has_zi extras, title suffix.
#   * Factor predictor -- reference-row synthesis.
#   * Schema validity in all paths.
#   * Oracle cross-validation against parameters::model_parameters()
#     with component = "conditional" (skipped if not installed).
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_glmmTMB_gauss <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject),
                   data = lme4::sleepstudy)
}

.fit_glmmTMB_gauss_factor <- function() {
  skip_if_not_installed("glmmTMB")
  d <- lme4::sleepstudy
  d$treatment <- factor(rep(c("A", "B", "C"), length.out = nrow(d)))
  glmmTMB::glmmTMB(Reaction ~ Days + treatment + (1 | Subject), data = d)
}

.fit_glmmTMB_binom <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data   = lme4::cbpp,
    family = binomial
  )
}

.fit_glmmTMB_poisson <- function() {
  skip_if_not_installed("glmmTMB")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$counter <- as.integer(d$gear)
  glmmTMB::glmmTMB(counter ~ mpg + (1 | cyl),
                   data = d, family = poisson(link = "log"))
}

.fit_glmmTMB_zi <- function() {
  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB", envir = environment())
  glmmTMB::glmmTMB(count ~ mined + (1 | site),
                   zi = ~ mined,
                   data = Salamanders,
                   family = poisson)
}


# ---- 1. Gaussian: schema validity + core fields --------------------------

test_that("as_regression_frame.glmmTMB produces a schema-valid frame (Gaussian)", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("glmmTMB: required attributes are attached", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("glmmTMB: info$class is 'glmmTMB'", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "glmmTMB")
})

test_that("glmmTMB Gaussian: info$family is gaussian/identity", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "gaussian")
  expect_identical(fr$info$family$link,   "identity")
})

test_that("glmmTMB: info$dv reads the response variable name", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "Reaction")
})

test_that("glmmTMB Gaussian: info$n_groups reports per-grouping-factor counts", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$n_groups, c(Subject = 18L))
})


# ---- 2. Gaussian: coef extraction ----------------------------------------

test_that("glmmTMB Gaussian: coefs estimates match glmmTMB::fixef(fit)$cond", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- glmmTMB::fixef(fit)$cond
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(b_rows$estimate[b_rows$term == nm],
                 unname(legacy[nm]),
                 tolerance = 1e-10,
                 info = paste("term:", nm))
  }
})

test_that("glmmTMB Gaussian: coefs SE matches sqrt(diag(vcov$cond))", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  V <- as.matrix(stats::vcov(fit)$cond)
  expected_se <- sqrt(diag(V))
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(expected_se)) {
    expect_equal(b_rows$std_error[b_rows$term == nm],
                 unname(expected_se[nm]),
                 tolerance = 1e-10)
  }
})


# ---- 3. Inference: Wald z uniformly --------------------------------------

test_that("glmmTMB Gaussian: ci_method = 'wald'; test_type = 'z'; df = Inf", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "z" | fr$coefs$is_ref))
  expect_true(all(is.infinite(fr$coefs$df) | fr$coefs$is_ref))
})

test_that("glmmTMB Gaussian: p-values match summary(fit)$coefficients$cond", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  smc <- summary(fit)$coefficients$cond
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(smc)) {
    expect_equal(b_rows$p_value[b_rows$term == nm],
                 unname(smc[nm, "Pr(>|z|)"]),
                 tolerance = 1e-10,
                 info = paste("term:", nm))
  }
})


# ---- 4. Gaussian: random effects + ICC -----------------------------------

test_that("glmmTMB Gaussian: random_effects has one row per (group,term) + residual", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  expect_identical(nrow(vc), 2L)
  expect_setequal(vc$group, c("Subject", "Residual"))
})

test_that("glmmTMB Gaussian: ICC is in (0,1) for a single random-intercept fit", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  icc <- fr$info$random_effects$icc
  expect_true(is.finite(icc))
  expect_true(icc > 0 & icc < 1)
})

test_that("glmmTMB Gaussian: ICC matches var_random / (var_random + var_resid)", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  var_r <- vc$variance[vc$group == "Subject"]
  var_e <- vc$variance[vc$group == "Residual"]
  expect_equal(fr$info$random_effects$icc,
               var_r / (var_r + var_e),
               tolerance = 1e-10)
})


# ---- 5. Gaussian: fit statistics -----------------------------------------

test_that("glmmTMB Gaussian: fit_stats$r_squared / adj_r_squared are NA", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.na(fr$info$fit_stats$r_squared))
  expect_true(is.na(fr$info$fit_stats$adj_r_squared))
})

test_that("glmmTMB Gaussian: fit_stats$sigma matches stats::sigma()", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$sigma, stats::sigma(fit),
               tolerance = 1e-10)
})

test_that("glmmTMB: AIC/BIC/logLik/nobs match stats:: helpers", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$aic, stats::AIC(fit),     tolerance = 1e-10)
  expect_equal(fr$info$fit_stats$bic, stats::BIC(fit),     tolerance = 1e-10)
  expect_equal(fr$info$fit_stats$log_lik, as.numeric(stats::logLik(fit)),
               tolerance = 1e-10)
  expect_identical(fr$info$fit_stats$nobs, as.integer(stats::nobs(fit)))
})


# ---- 6. Supports capabilities --------------------------------------------

test_that("glmmTMB Gaussian: supports flags are correct", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  sp <- fr$info$supports
  expect_true(sp$ame)
  expect_false(sp$partial_effect_size)
  expect_false(sp$classical_r2)
  expect_true(sp$nested_lrt)
  expect_false(sp$exponentiate)  # identity link
  expect_true(sp$standardise_refit)
})

test_that("glmmTMB binomial: supports$exponentiate = TRUE (non-identity link)", {
  fit <- .fit_glmmTMB_binom()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})


# ---- 7. Factor predictor: reference rows ---------------------------------

test_that("glmmTMB: factor predictor synthesises a reference row + non-ref rows", {
  fit <- .fit_glmmTMB_gauss_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  treat_rows <- fr$coefs[fr$coefs$parent_var == "treatment", ]
  expect_identical(nrow(treat_rows), 3L)
  expect_identical(sum(treat_rows$is_ref), 1L)
  expect_true(all(is.na(treat_rows$estimate[treat_rows$is_ref])))
})


# ---- 8. Binomial / Poisson families --------------------------------------

test_that("glmmTMB binomial: info$family is binomial/logit; Wald z", {
  fit <- .fit_glmmTMB_binom()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "binomial")
  expect_identical(fr$info$family$link,   "logit")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "z" | fr$coefs$is_ref))
})

test_that("glmmTMB binomial: title_prefix names 'Logistic'", {
  fit <- .fit_glmmTMB_binom()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "Logistic", fixed = TRUE)
  expect_match(fr$info$extras$title_prefix, "glmmTMB",  fixed = TRUE)
})

test_that("glmmTMB poisson: info$family is poisson/log; title 'Poisson'", {
  fit <- .fit_glmmTMB_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "poisson")
  expect_identical(fr$info$family$link,   "log")
  expect_match(fr$info$extras$title_prefix, "Poisson", fixed = TRUE)
})


# ---- 9. Zero-inflation ---------------------------------------------------

test_that("glmmTMB zero-inflated: info$extras$has_zi = TRUE; component block captured", {
  fit <- .fit_glmmTMB_zi()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_true(fr$info$extras$has_zi)
  blocks <- fr$info$extras$component_blocks
  expect_true(is.list(blocks) && length(blocks) >= 1L)
  blk <- blocks[[1L]]
  expect_identical(blk$label, "Zero-inflation")
  expect_identical(blk$link, "logit")
  non_ref <- blk$coefs[!blk$coefs$is_ref, , drop = FALSE]
  expect_setequal(non_ref$term,
                  paste0("zi.", names(glmmTMB::fixef(fit)$zi)))
  expect_true(all(is.finite(non_ref$std_error)))
})

test_that("glmmTMB zero-inflated: title_prefix suffixed '(zero-inflated)'", {
  fit <- .fit_glmmTMB_zi()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix,
               "(zero-inflated)", fixed = TRUE)
})

test_that("glmmTMB non-zi: zi_coefs is NULL, has_zi = FALSE", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_null(fr$info$extras$zi_coefs)
  expect_false(fr$info$extras$has_zi)
})


# ---- 10. Oracle: parameters::model_parameters() --------------------------

test_that("glmmTMB Gaussian coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci         = 0.95,
    ci_method  = "wald",
    effects    = "fixed",
    component  = "conditional"
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

test_that("glmmTMB binomial coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_glmmTMB_binom()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci         = 0.95,
    ci_method  = "wald",
    effects    = "fixed",
    component  = "conditional",
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
