# ---------------------------------------------------------------------------
# Targeted coverage tests for R/regression_frame_merMod.R.
#
# These exercise branches not hit by the broader merMod suite:
#   * glmer family-title switch arms: Gamma, inverse.gaussian, and the
#     default (negative-binomial) fall-through.
#   * ordered-factor predictor (polynomial contrasts) -> reference_dropped
#     is FALSE, so .merMod_reference_rows() walks every term via `next`
#     and returns the empty-coefs frame.
#   * .merMod_icc() edge cases: empty vc_df, multiple grouping factors.
#   * .merMod_link_distribution_variance(): NULL fit; poisson(log)
#     lognormal-approximation ICC.
#   * .extract_dv_label(): labelled response attribute.
#   * random-slope fit -> correlation-row Wald SE / CI Delta-method path.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.cov_fit_gamma_glmer <- function() {
  skip_if_not_installed("lme4")
  set.seed(1)
  d <- data.frame(
    y = rgamma(60, shape = 2, rate = 0.5),
    x = rnorm(60),
    g = factor(rep(letters[1:6], each = 10))
  )
  suppressWarnings(
    lme4::glmer(y ~ x + (1 | g), data = d, family = Gamma(link = "log"))
  )
}

.cov_fit_invgauss_glmer <- function() {
  skip_if_not_installed("lme4")
  set.seed(1)
  d <- data.frame(
    y = rgamma(60, shape = 2, rate = 0.5),
    x = rnorm(60),
    g = factor(rep(letters[1:6], each = 10))
  )
  suppressWarnings(
    lme4::glmer(
      y ~ x + (1 | g),
      data = d,
      family = inverse.gaussian(link = "log")
    )
  )
}

.cov_fit_poisson_glmer <- function() {
  skip_if_not_installed("lme4")
  set.seed(42)
  n_g <- 20
  n_i <- 15
  g <- factor(rep(seq_len(n_g), each = n_i))
  u <- rnorm(n_g, 0, 0.8)[as.integer(g)]
  x <- rnorm(n_g * n_i)
  eta <- 0.5 + 0.3 * x + u
  y <- rpois(length(eta), exp(eta))
  d <- data.frame(y = y, x = x, g = g)
  suppressWarnings(
    lme4::glmer(y ~ x + (1 | g), data = d, family = poisson(link = "log"))
  )
}

.cov_fit_lmer_slope <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = lme4::sleepstudy)
}


# ---- 1. .merMod_glm_family_title: switch arms ----------------------------

test_that("glmer Gamma: family title is 'Gamma'", {
  fit <- .cov_fit_gamma_glmer()
  # lme4 labels the Wald column "t value" (not "z value") for Gamma /
  # inverse.gaussian glmer, which the current .merMod_coefs() z-branch
  # cannot index -- so we assert via the family-title helper directly
  # rather than the full as_regression_frame() pipeline.
  expect_identical(spicy:::.merMod_glm_family_title(fit), "Gamma")
})

test_that("glmer inverse.gaussian: family title is 'Inverse-Gaussian'", {
  fit <- .cov_fit_invgauss_glmer()
  expect_identical(spicy:::.merMod_glm_family_title(fit), "Inverse-Gaussian")
})

test_that("glmer.nb: family title falls through to the title-cased default", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("MASS")
  set.seed(7)
  n_g <- 8
  n_i <- 12
  g <- factor(rep(seq_len(n_g), each = n_i))
  u <- rnorm(n_g, 0, 0.5)[as.integer(g)]
  x <- rnorm(n_g * n_i)
  y <- MASS::rnegbin(length(x), mu = exp(0.5 + 0.3 * x + u), theta = 2)
  d <- data.frame(y = y, x = x, g = g)
  fit <- tryCatch(
    suppressWarnings(suppressMessages(
      lme4::glmer.nb(y ~ x + (1 | g), data = d)
    )),
    error = function(e) skip("glmer.nb did not converge in this environment")
  )
  # Family string is "Negative Binomial(theta)"; the default switch arm
  # title-cases the first letter (already upper) and returns it verbatim.
  title <- spicy:::.merMod_glm_family_title(fit)
  expect_match(title, "^Negative Binomial")
})


# ---- 2. Ordered (polynomial-contrast) factor: empty reference rows -------

test_that("lmer with ordered factor: no reference rows are emitted", {
  skip_if_not_installed("lme4")
  d <- lme4::sleepstudy
  d$ord <- ordered(
    rep(c("lo", "mid", "hi"), length.out = nrow(d)),
    levels = c("lo", "mid", "hi")
  )
  fit <- lme4::lmer(Reaction ~ Days + ord + (1 | Subject), data = d)

  # detect_factor_terms() classifies `ord` as polynomial -> reference_dropped
  # FALSE, so .merMod_reference_rows() `next`s every term and returns the
  # empty-coefs frame.
  ref <- spicy:::.merMod_reference_rows(fit, est_template = lme4::fixef(fit))
  expect_identical(nrow(ref), 0L)

  # And the public frame carries no is_ref rows for the ordered factor.
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(any(fr$coefs$is_ref))
  expect_invisible(spicy:::validate_regression_frame(fr))
})


# ---- 3. .merMod_icc(): edge cases ----------------------------------------

test_that(".merMod_icc returns NA for an empty variance-components frame", {
  expect_true(is.na(spicy:::.merMod_icc(data.frame())))
})

test_that(".merMod_icc returns NA with more than one grouping factor", {
  skip_if_not_installed("lme4")
  # Crossed random intercepts: two grouping factors -> ICC undefined.
  fit <- lme4::lmer(
    Reaction ~ Days + (1 | Subject) + (1 | Days),
    data = lme4::sleepstudy
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  groups <- setdiff(
    unique(fr$info$random_effects$variance_components$group),
    "Residual"
  )
  expect_gt(length(groups), 1L)
  expect_true(is.na(fr$info$random_effects$icc))
})


# ---- 4. .merMod_link_distribution_variance() -----------------------------

test_that(".merMod_link_distribution_variance returns NA for a NULL fit", {
  expect_true(is.na(spicy:::.merMod_link_distribution_variance(NULL, 1)))
})

test_that("poisson glmer: adjusted ICC uses the lognormal link variance", {
  fit <- .cov_fit_poisson_glmer()
  re <- spicy:::.merMod_random_effects(fit)
  # Single grouping factor + genuine between-group variance -> finite ICC.
  expect_true(is.finite(re$icc))
  expect_gt(re$icc, 0)
  expect_lt(re$icc, 1)

  # The link-scale distribution variance (lognormal approximation) is
  # finite and positive for poisson(log).
  var_r <- re$variance_components$variance[1L]
  ldv <- spicy:::.merMod_link_distribution_variance(fit, var_r)
  expect_true(is.finite(ldv))
  expect_gt(ldv, 0)
})


# ---- 5. .extract_dv_label(): labelled response ---------------------------

test_that(".extract_dv_label returns the response 'label' attribute", {
  skip_if_not_installed("lme4")
  d <- lme4::sleepstudy
  attr(d$Reaction, "label") <- "Reaction time (ms)"
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = d)
  expect_identical(
    spicy:::.extract_dv_label(fit, "Reaction"),
    "Reaction time (ms)"
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv_label, "Reaction time (ms)")
})


# ---- 6. Random-slope fit: correlation-row Wald SE / CI Delta-method ------

test_that("lmer random slope: correlation row gets a Wald SE + CI via merDeriv", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("merDeriv")
  fit <- .cov_fit_lmer_slope()
  skip_if(
    lme4::isSingular(fit),
    "random-slope fit is singular; merDeriv SE path is skipped"
  )
  re <- spicy:::.merMod_random_effects(fit)
  vc <- re$variance_components
  corr_rows <- vc[vc$is_correlation %in% TRUE, , drop = FALSE]
  expect_identical(nrow(corr_rows), 1L)
  # The Phase 7c17 Delta-method populates SE + Wald CI on the rho row.
  expect_true(is.finite(corr_rows$std_error[1L]))
  expect_true(is.finite(corr_rows$ci_lower[1L]))
  expect_true(is.finite(corr_rows$ci_upper[1L]))
  expect_identical(corr_rows$ci_method[1L], "wald")
  # CI is clamped to [-1, 1].
  expect_gte(corr_rows$ci_lower[1L], -1)
  expect_lte(corr_rows$ci_upper[1L], 1)
})

test_that("lmer random slope: variance rows carry merDeriv Wald SE + CI", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("merDeriv")
  fit <- .cov_fit_lmer_slope()
  skip_if(
    lme4::isSingular(fit),
    "random-slope fit is singular; merDeriv SE path is skipped"
  )
  re <- spicy:::.merMod_random_effects(fit)
  vc <- re$variance_components
  var_rows <- vc[!(vc$is_correlation %in% TRUE), , drop = FALSE]
  expect_true(all(is.finite(var_rows$std_error)))
  expect_true(all(var_rows$ci_lower >= 0)) # variance CI lower clamped at 0
  expect_true(all(var_rows$ci_method == "wald"))
})
