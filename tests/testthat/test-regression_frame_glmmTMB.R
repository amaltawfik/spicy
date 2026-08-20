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
  glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
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
    data = lme4::cbpp,
    family = binomial
  )
}

.fit_glmmTMB_poisson <- function() {
  skip_if_not_installed("glmmTMB")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$counter <- as.integer(d$gear)
  glmmTMB::glmmTMB(
    counter ~ mpg + (1 | cyl),
    data = d,
    family = poisson(link = "log")
  )
}

.fit_glmmTMB_zi <- function() {
  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB", envir = environment())
  glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    zi = ~mined,
    data = Salamanders,
    family = poisson
  )
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
  expect_identical(fr$info$family$link, "identity")
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
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})

test_that("glmmTMB Gaussian: coefs SE matches sqrt(diag(vcov$cond))", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  V <- as.matrix(stats::vcov(fit)$cond)
  expected_se <- sqrt(diag(V))
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(expected_se)) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(expected_se[nm]),
      tolerance = 1e-10
    )
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
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(smc[nm, "Pr(>|z|)"]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
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
  expect_equal(
    fr$info$random_effects$icc,
    var_r / (var_r + var_e),
    tolerance = 1e-10
  )
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
  expect_equal(fr$info$fit_stats$sigma, stats::sigma(fit), tolerance = 1e-10)
})

test_that("glmmTMB: AIC/BIC/logLik/nobs match stats:: helpers", {
  fit <- .fit_glmmTMB_gauss()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$aic, stats::AIC(fit), tolerance = 1e-10)
  expect_equal(fr$info$fit_stats$bic, stats::BIC(fit), tolerance = 1e-10)
  expect_equal(
    fr$info$fit_stats$log_lik,
    as.numeric(stats::logLik(fit)),
    tolerance = 1e-10
  )
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
  expect_false(sp$exponentiate) # identity link
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
  expect_identical(fr$info$family$link, "logit")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "z" | fr$coefs$is_ref))
})

test_that("glmmTMB binomial: title_prefix names 'Logistic'", {
  fit <- .fit_glmmTMB_binom()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "Logistic", fixed = TRUE)
  expect_match(fr$info$extras$title_prefix, "glmmTMB", fixed = TRUE)
})

test_that("glmmTMB poisson: info$family is poisson/log; title 'Poisson'", {
  fit <- .fit_glmmTMB_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "poisson")
  expect_identical(fr$info$family$link, "log")
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
  expect_setequal(non_ref$term, paste0("zi.", names(glmmTMB::fixef(fit)$zi)))
  expect_true(all(is.finite(non_ref$std_error)))
})

test_that("glmmTMB zero-inflated: title_prefix suffixed '(zero-inflated)'", {
  fit <- .fit_glmmTMB_zi()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "(zero-inflated)", fixed = TRUE)
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
    ci = 0.95,
    ci_method = "wald",
    effects = "fixed",
    component = "conditional"
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-6,
      info = paste("oracle B mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-6,
      info = paste("oracle SE mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$p_value,
      oracle_row$p,
      tolerance = 1e-6,
      info = paste("oracle p mismatch on term:", nm)
    )
  }
})

test_that("glmmTMB binomial coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_glmmTMB_binom()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci = 0.95,
    ci_method = "wald",
    effects = "fixed",
    component = "conditional",
    exponentiate = FALSE
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-6,
      info = paste("oracle B mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-6,
      info = paste("oracle SE mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$p_value,
      oracle_row$p,
      tolerance = 1e-6,
      info = paste("oracle p mismatch on term:", nm)
    )
  }
})


# ---- 11. Boundary (singular) fits ----------------------------------------

# glmmTMB has no isSingular() of its own. The criterion is
# det(V / sigma^2) < 1e-5 for any random-effect covariance block, over
# all three components (cond / zi / disp). Being relative, it is
# invariant to the units of the response -- the property that ties the
# verdict to lme4::isSingular() rather than to
# performance::check_singularity.glmmTMB(), which tests absolute
# variances and is pinned as a documented divergence below.

.fit_glmmTMB_singular <- function() {
  skip_if_not_installed("glmmTMB")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  # Three groups, no between-group signal: the random intercept
  # collapses onto 0 (the glmmTMB twin of the classic singular glmer).
  suppressWarnings(glmmTMB::glmmTMB(
    am ~ mpg + (1 | cyl),
    data = d,
    family = binomial
  ))
}

.fit_glmmTMB_singular_zi <- function() {
  skip_if_not_installed("glmmTMB")
  # Healthy random intercept in the conditional model, collapsed one in
  # the zero-inflation model: only the "all components" convention sees it.
  set.seed(2026)
  n_g <- 25L
  d <- data.frame(g = factor(rep(seq_len(n_g), each = 12L)))
  d$x <- rnorm(nrow(d))
  b <- rnorm(n_g, sd = 1.2)
  lambda <- exp(0.4 + 0.3 * d$x + b[as.integer(d$g)])
  d$y <- rpois(nrow(d), lambda) * rbinom(nrow(d), 1L, 0.75)
  suppressWarnings(glmmTMB::glmmTMB(
    y ~ x + (1 | g),
    ziformula = ~ 1 + (1 | g),
    family = poisson,
    data = d
  ))
}

test_that("glmmTMB singular fit sets extras$has_singular (and a healthy one does not)", {
  fit <- .fit_glmmTMB_singular()
  expect_true(spicy:::.glmmTMB_is_singular(fit))
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$extras$has_singular)
  expect_identical(fr$info$extras$singular_terms, character(0))
  expect_invisible(spicy:::validate_regression_frame(fr))

  clean <- .fit_glmmTMB_gauss()
  expect_false(spicy:::.glmmTMB_is_singular(clean))
  expect_false(
    as_regression_frame(clean, model_id = "M1")$info$extras$has_singular
  )
})

test_that("glmmTMB singularity agrees with the oracle where the scale is neutral", {
  skip_if_not_installed("performance")
  # Non-Gaussian families have no residual scale (VarCorr's useSc is
  # FALSE, sigma is 1 or a dispersion parameter), so the criterion
  # reduces to performance::check_singularity.glmmTMB()'s exactly, and
  # the oracle is a genuine one there. The Gaussian fixtures below are
  # included because their sigma leaves the verdict unchanged.
  fits <- list(
    singular = .fit_glmmTMB_singular(),
    singular_zi = .fit_glmmTMB_singular_zi(),
    gauss = .fit_glmmTMB_gauss(),
    binom = .fit_glmmTMB_binom(),
    zi = .fit_glmmTMB_zi()
  )
  for (nm in names(fits)) {
    expect_identical(
      spicy:::.glmmTMB_is_singular(fits[[nm]]),
      isTRUE(performance::check_singularity(fits[[nm]])),
      info = paste("oracle mismatch on fixture:", nm)
    )
  }
})

test_that("glmmTMB singularity is invariant to the scale of the response", {
  skip_if_not_installed("lme4")
  # A healthy Gaussian fit stays healthy when the response is divided by
  # 1000, and a collapsed component stays collapsed when it is multiplied
  # by 100. lme4::isSingular() on the equivalent lmer fit says the same
  # at both scales.
  ss <- lme4::sleepstudy
  ss_small <- ss
  ss_small$Reaction <- ss_small$Reaction / 1000
  for (d in list(ss, ss_small)) {
    fit <- glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject), data = d)
    expect_false(spicy:::.glmmTMB_is_singular(fit))
  }

  set.seed(7)
  n_g <- 15L
  d0 <- data.frame(g = factor(rep(seq_len(n_g), each = 6L)))
  d0$x <- rnorm(nrow(d0))
  d0$y <- 1 + 0.5 * d0$x + rnorm(nrow(d0)) # no between-group variance
  for (k in c(1, 100)) {
    d <- d0
    d$y <- d$y * k
    fit <- suppressWarnings(glmmTMB::glmmTMB(y ~ x + (1 | g), data = d))
    twin <- suppressMessages(suppressWarnings(
      lme4::lmer(y ~ x + (1 | g), data = d)
    ))
    expect_true(
      spicy:::.glmmTMB_is_singular(fit),
      info = paste("collapsed component missed at scale", k)
    )
    expect_true(lme4::isSingular(twin), info = paste("twin at scale", k))
  }
})

test_that("the divergence from performance::check_singularity.glmmTMB is deliberate", {
  skip_if_not_installed("performance")
  skip_if_not_installed("lme4")
  # performance tests absolute variances: a perfectly ordinary fit whose
  # response happens to be small is declared singular. Pinned to OUR
  # verdict, with lme4::isSingular() on the equivalent lmer fit as the
  # tiebreaker.
  d <- lme4::sleepstudy
  d$Reaction <- d$Reaction / 1000
  fit <- glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject), data = d)
  twin <- suppressMessages(suppressWarnings(
    lme4::lmer(Reaction ~ Days + (Days | Subject), data = d)
  ))
  expect_true(performance::check_singularity(fit))
  expect_false(lme4::isSingular(twin))
  expect_false(spicy:::.glmmTMB_is_singular(fit))
})

test_that(".glmmTMB_re_scale2 only divides by a genuine residual scale", {
  skip_if_not_installed("glmmTMB")
  # Gaussian: sigma IS the residual scale of the linear predictor.
  gauss <- glmmTMB::VarCorr(.fit_glmmTMB_gauss())$cond
  expect_true(attr(gauss, "useSc"))
  expect_equal(
    spicy:::.glmmTMB_re_scale2(gauss),
    attr(gauss, "sc")^2,
    tolerance = 1e-12
  )
  # nbinom2: sigma() returns the dispersion parameter, which is NOT a
  # scale for the random effects -- VarCorr says so with useSc = FALSE,
  # and the divisor stays 1.
  data(Salamanders, package = "glmmTMB", envir = environment())
  nb <- glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    data = Salamanders,
    family = glmmTMB::nbinom2
  )
  nb_vc <- glmmTMB::VarCorr(nb)$cond
  expect_false(isTRUE(attr(nb_vc, "useSc")))
  expect_true(abs(stats::sigma(nb) - 1) > 0.1) # would have moved the verdict
  expect_identical(spicy:::.glmmTMB_re_scale2(nb_vc), 1)
  # A modelled dispersion has no single residual scale: sc is NA and the
  # divisor falls back to 1 rather than poisoning det() with NA.
  disp <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    dispformula = ~Days,
    data = lme4::sleepstudy
  )
  disp_vc <- glmmTMB::VarCorr(disp)$cond
  expect_true(isTRUE(attr(disp_vc, "useSc")))
  expect_true(is.na(attr(disp_vc, "sc")))
  expect_identical(spicy:::.glmmTMB_re_scale2(disp_vc), 1)
  expect_false(spicy:::.glmmTMB_is_singular(disp))
})

test_that("glmmTMB singularity covers the zero-inflation component", {
  skip_if_not_installed("performance")
  fit <- .fit_glmmTMB_singular_zi()
  # The oracle's per-term breakdown: conditional healthy, zi collapsed.
  per_term <- performance::check_singularity(fit, check = "terms")
  expect_false(any(per_term$cond))
  expect_true(any(per_term$zi))
  # A conditional-only walk would miss it; the frame does not.
  expect_true(
    as_regression_frame(fit, model_id = "M1")$info$extras$has_singular
  )
})

test_that("glmmTMB singular fit: the boundary VC keeps no Wald SE or CI", {
  fit <- .fit_glmmTMB_singular()
  # Left to itself the Wald machinery answers [0, Inf] with an infinite
  # SE -- the numbers the singular note says are omitted.
  raw <- suppressWarnings(stats::confint(fit, method = "Wald", parm = "theta_"))
  expect_true(any(!is.finite(as.matrix(raw))))

  vc <- as_regression_frame(
    fit,
    model_id = "M1"
  )$info$random_effects$variance_components
  expect_true(all(is.na(vc$std_error)))
  expect_true(all(is.na(vc$ci_lower)))
  expect_true(all(is.na(vc$ci_upper)))
  expect_true(all(is.na(vc$ci_method)))
  # The estimate itself still renders: only its uncertainty is withheld.
  expect_true(all(is.finite(vc$variance)))
})

test_that("glmmTMB singularity tolerance sits between the two regimes", {
  fit <- .fit_glmmTMB_singular()
  vc <- glmmTMB::VarCorr(fit)$cond
  rel <- det(as.matrix(vc[[1L]]) / spicy:::.glmmTMB_re_scale2(vc))
  # Collapsed by orders of magnitude, not by a hair's breadth: the
  # verdict does not hang on the exact tolerance.
  expect_lt(rel, 1e-7)
  clean_vc <- glmmTMB::VarCorr(.fit_glmmTMB_gauss())$cond
  expect_gt(
    det(as.matrix(clean_vc[[1L]]) / spicy:::.glmmTMB_re_scale2(clean_vc)),
    1
  )
  # An explicit tolerance argument is honoured. Only the loosening
  # direction is asserted: how far below the tolerance the optimiser
  # happened to stop is not a property of the fit.
  expect_true(spicy:::.glmmTMB_is_singular(
    .fit_glmmTMB_gauss(),
    tolerance = 1e6
  ))
})

test_that("a non-converged glmmTMB fit prints no NaN uncertainty", {
  skip_if_not_installed("glmmTMB")
  # Stopped after one iteration: the fit sits on its starting values, its
  # information matrix is meaningless and confint() returns NaN. The
  # boundary flag does NOT catch this (the starting variances are 1, not
  # 0) -- the degeneracy guard does, independently.
  fit <- suppressWarnings(glmmTMB::glmmTMB(
    Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy,
    control = glmmTMB::glmmTMBControl(
      optCtrl = list(iter.max = 1L, eval.max = 1L)
    )
  ))
  skip_if(fit$fit$convergence == 0L, "fit converged after all")
  expect_false(spicy:::.glmmTMB_is_singular(fit))
  raw <- suppressWarnings(
    stats::confint(fit, method = "Wald", parm = "theta_")
  )
  # What the guard has to absorb -- IF this platform produces it. The
  # one-iteration stop-point is optimizer- and platform-dependent
  # (macOS lands on an invertible information matrix and returns
  # finite bounds); the guard itself is exercised deterministically on
  # every platform by the synthetic unit test below.
  skip_if(
    !any(is.nan(as.matrix(raw))),
    "this platform's stop-point yields finite Wald bounds"
  )

  # suppressWarnings: glmmTMB's own summary() warns on this fit's
  # degenerate Hessian. That noise is left visible to users -- a broken
  # fit should be loud -- and only muted here.
  vc <- suppressWarnings(
    as_regression_frame(fit, model_id = "M1")
  )$info$random_effects$variance_components
  expect_false(any(is.nan(vc$std_error)))
  expect_false(any(is.nan(vc$ci_lower)))
  expect_false(any(is.nan(vc$ci_upper)))
  expect_true(all(is.na(vc$ci_method[is.na(vc$std_error)])))
})

test_that(".glmmTMB_blank_degenerate_vc drops non-finite Wald quantities", {
  vc <- data.frame(
    group = c("g", "g", "Residual"),
    term = c("(Intercept)", "x", ""),
    variance = c(1, 2, 3),
    std_error = c(NaN, 0.5, Inf),
    ci_lower = c(0, 0.1, 0),
    ci_upper = c(Inf, 0.9, 10),
    ci_method = c("wald", "wald", "wald"),
    stringsAsFactors = FALSE
  )
  out <- spicy:::.glmmTMB_blank_degenerate_vc(vc)
  expect_identical(is.na(out$std_error), c(TRUE, FALSE, TRUE))
  expect_identical(is.na(out$ci_upper), c(TRUE, FALSE, TRUE))
  expect_identical(out$ci_method, c(NA, "wald", NA))
  expect_identical(out$std_error[2L], 0.5)
  expect_identical(out$variance, c(1, 2, 3))
  # Idempotent: a frame that is already clean comes back unchanged.
  expect_identical(spicy:::.glmmTMB_blank_degenerate_vc(out), out)
})
