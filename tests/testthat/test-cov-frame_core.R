# ---------------------------------------------------------------------------
# Coverage-focused tests for R/regression_frame.R
#
# Targets branches not exercised by the existing test suite:
#   * .validate_info() type / structure error arms
#   * .compute_null_model_lrt() non-mixed fall-through
#   * the Suggests-package-absent (requireNamespace == FALSE) guards in the
#     null-model-LRT and Nakagawa component extractors
#   * .merMod_glm_family_title_safe() fallback label
#   * .nakagawa_distribution_variance() NA arms (non-finite sigma / lambda)
#   * .nakagawa_assemble() degenerate-total arm
#   * .nakagawa_supported_family() family-gate rejections
#   * .nakagawa_components_glmmTMB() dispformula bail
#
# These reach the per-class methods through the internal helpers directly
# (spicy:::) and via crafted component lists, which is the only practical
# way to drive the defensive NA / NULL arms.
# ---------------------------------------------------------------------------


# ---- Helper: minimal valid frame (mirrors test-regression_frame.R) ---------

.cov_valid_frame <- function() {
  coefs <- data.frame(
    term             = "x",
    parent_var       = "x",
    label            = "x",
    factor_level_pos = NA_integer_,
    is_ref           = FALSE,
    estimate_type    = "B",
    estimate         = 1.0,
    std_error        = 0.1,
    ci_lower         = 0.8,
    ci_upper         = 1.2,
    stringsAsFactors = FALSE
  )
  info <- list(
    class        = "lm",
    family       = list(family = "gaussian", link = "identity"),
    dv           = "y",
    n_obs        = 100L,
    weights_kind = "none",
    fit_stats    = list(nobs = 100L),
    vcov_kind    = "model",
    vcov_label   = "OLS",
    ci_level     = 0.95,
    ci_method    = "wald",
    supports     = list(
      ame                 = TRUE,
      partial_effect_size = TRUE,
      classical_r2        = TRUE,
      nested_lrt          = TRUE,
      exponentiate        = FALSE,
      standardise_refit   = TRUE
    ),
    extras       = list()
  )
  out <- list(coefs = coefs, info = info)
  attr(out, "spicy_frame_version") <- spicy:::spicy_frame_version()
  attr(out, "fit") <- list(dummy = TRUE)
  out
}


# ---- .validate_info(): structure + type arms ------------------------------

test_that("validator rejects info that is not a named list (lines 369-372)", {
  frame <- .cov_valid_frame()
  frame$info <- list(1, 2, 3)   # a list, but unnamed
  err <- tryCatch(
    spicy:::validate_regression_frame(frame),
    spicy_invalid_frame = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_frame")
  expect_match(conditionMessage(err), "named list", fixed = TRUE)
})

test_that("validator rejects non-character scalar info fields (lines 396-404)", {
  # vcov_kind must be a length-1 non-NA character.
  frame <- .cov_valid_frame()
  frame$info$vcov_kind <- 42L
  err <- tryCatch(
    spicy:::validate_regression_frame(frame),
    spicy_invalid_frame = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_frame")
  expect_match(conditionMessage(err), "vcov_kind", fixed = TRUE)

  # NA character also fails the same arm.
  frame2 <- .cov_valid_frame()
  frame2$info$ci_method <- NA_character_
  expect_error(
    spicy:::validate_regression_frame(frame2),
    class = "spicy_invalid_frame"
  )

  # length-2 character fails the same arm.
  frame3 <- .cov_valid_frame()
  frame3$info$dv <- c("y1", "y2")
  expect_error(
    spicy:::validate_regression_frame(frame3),
    class = "spicy_invalid_frame"
  )
})

test_that("validator rejects non-list info$supports (lines 478-481)", {
  frame <- .cov_valid_frame()
  frame$info$supports <- "not a list"
  err <- tryCatch(
    spicy:::validate_regression_frame(frame),
    spicy_invalid_frame = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_frame")
  expect_match(conditionMessage(err), "supports", fixed = TRUE)
})


# ---- .compute_null_model_lrt(): non-mixed fall-through (line 586) ----------

test_that(".compute_null_model_lrt returns NULL for a non-mixed fit (line 586)", {
  # Plain lm is neither glmmTMB, merMod, nor lme: hits the final else NULL.
  expect_null(spicy:::.compute_null_model_lrt(lm(mpg ~ wt, data = mtcars)))
  # glm too.
  expect_null(
    spicy:::.compute_null_model_lrt(
      glm(am ~ mpg, data = mtcars, family = binomial)
    )
  )
})


# ---- requireNamespace guards in the null-LRT extractors -------------------

test_that(".null_lrt_merMod returns NULL when lme4 is unavailable (line 614)", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  with_mocked_bindings(
    expect_null(spicy:::.null_lrt_merMod(fit)),
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
})

test_that(".null_lrt_glmmTMB returns NULL when glmmTMB is unavailable (line 675)", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  fit <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject),
                          data = lme4::sleepstudy)
  with_mocked_bindings(
    expect_null(spicy:::.null_lrt_glmmTMB(fit)),
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
})

test_that(".null_lrt_lme returns NULL when nlme is unavailable (line 719)", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(distance ~ age, data = nlme::Orthodont,
                   random = ~ 1 | Subject)
  with_mocked_bindings(
    expect_null(spicy:::.null_lrt_lme(fit)),
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
})


# ---- .merMod_glm_family_title_safe(): fallback label (line 670) -----------

test_that(".merMod_glm_family_title_safe falls back to fam$family (line 670)", {
  # A junk fit makes .merMod_glm_family_title() error (stats::family fails),
  # so the tryCatch yields NA and the helper falls back to fam$family.
  junk <- structure(list(), class = "zzz_no_family_method")
  out <- spicy:::.merMod_glm_family_title_safe(
    junk, list(family = "quasipoisson")
  )
  expect_identical(out, "quasipoisson")

  # When fam$family is also absent, the %||% default "regression" is used.
  out2 <- spicy:::.merMod_glm_family_title_safe(junk, list())
  expect_identical(out2, "regression")
})


# ---- .nakagawa_supported_family(): family gate rejections -----------------

test_that(".nakagawa_supported_family is FALSE when family() errors (line 911)", {
  junk <- structure(list(), class = "zzz_no_family_method")
  expect_false(spicy:::.nakagawa_supported_family(junk))
})

test_that(".nakagawa_supported_family rejects unsupported binomial link (line 914)", {
  # cauchit is not one of logit / probit / cloglog.
  fit <- glm(am ~ mpg, data = mtcars, family = binomial(link = "cauchit"))
  expect_false(spicy:::.nakagawa_supported_family(fit))
})

test_that(".nakagawa_supported_family rejects non-supported families (line 924)", {
  # Gamma is neither gaussian, binomial, nor poisson(log).
  fit_gamma <- glm(mpg ~ wt, data = mtcars, family = Gamma(link = "log"))
  expect_false(spicy:::.nakagawa_supported_family(fit_gamma))
  # poisson with a non-log link also falls through to the final FALSE.
  fit_pois_sqrt <- suppressWarnings(
    glm(cyl ~ mpg, data = mtcars, family = poisson(link = "sqrt"))
  )
  expect_false(spicy:::.nakagawa_supported_family(fit_pois_sqrt))
})


# ---- .nakagawa_distribution_variance(): NA arms ---------------------------

test_that("distribution variance is NA for non-finite Gaussian sigma (line 863)", {
  comps <- list(family = "gaussian", link = "identity", sigma = Inf)
  expect_true(is.na(spicy:::.nakagawa_distribution_variance(comps)))
})

test_that("distribution variance is NA for non-finite Poisson lambda (line 890)", {
  comps <- list(family = "poisson", link = "log",
                null_intercept = Inf, null_var_g = 0)
  expect_true(is.na(spicy:::.nakagawa_distribution_variance(comps)))
})

test_that("distribution variance honours the binomial link switch", {
  # Spot-check the supported binomial links (closed-form constants) plus
  # the NA fall-through for an unsupported link inside the switch.
  expect_equal(
    spicy:::.nakagawa_distribution_variance(
      list(family = "binomial", link = "logit")
    ),
    pi^2 / 3
  )
  expect_equal(
    spicy:::.nakagawa_distribution_variance(
      list(family = "binomial", link = "probit")
    ),
    1
  )
  expect_true(is.na(
    spicy:::.nakagawa_distribution_variance(
      list(family = "binomial", link = "cauchit")
    )
  ))
})


# ---- .nakagawa_assemble(): degenerate total (line 846) --------------------

test_that(".nakagawa_assemble returns NA list when total variance is 0 (line 846)", {
  comps <- list(var_f = 0, var_g = 0, family = "gaussian",
                link = "identity", sigma = 0)
  out <- spicy:::.nakagawa_assemble(comps)
  expect_true(is.list(out))
  expect_true(is.na(out$marginal))
  expect_true(is.na(out$conditional))
})


# ---- requireNamespace guards in the Nakagawa component extractors ---------

test_that(".nakagawa_components_merMod returns NULL without lme4 (line 931)", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  with_mocked_bindings(
    expect_null(spicy:::.nakagawa_components_merMod(fit)),
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
})

test_that(".nakagawa_components_lme returns NULL without nlme (line 1050)", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(distance ~ age, data = nlme::Orthodont,
                   random = ~ 1 | Subject)
  with_mocked_bindings(
    expect_null(spicy:::.nakagawa_components_lme(fit)),
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
})

test_that(".nakagawa_components_glmmTMB returns NULL without glmmTMB (line 1123)", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  fit <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject),
                          data = lme4::sleepstudy)
  with_mocked_bindings(
    expect_null(spicy:::.nakagawa_components_glmmTMB(fit)),
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
})


# ---- .nakagawa_components_glmmTMB(): dispformula bail (line 1141) ----------

test_that(".nakagawa_components_glmmTMB bails on a non-trivial dispformula (line 1141)", {
  skip_if_not_installed("glmmTMB")
  set.seed(11)
  n <- 300
  d <- data.frame(
    yc = rnorm(n),
    x  = rnorm(n),
    g  = factor(rep(seq_len(30), length.out = n))
  )
  # dispformula = ~ x introduces a real dispersion predictor; the Nakagawa
  # self-implementation refuses it (returns NULL) so the caller falls
  # through to performance.
  fit <- glmmTMB::glmmTMB(yc ~ x + (1 | g), dispformula = ~ x, data = d)
  expect_null(spicy:::.nakagawa_components_glmmTMB(fit))
})
