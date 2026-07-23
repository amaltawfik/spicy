# ---------------------------------------------------------------------------
# Coverage tests: residual branches of the misc frame builders
# (lm-legacy helpers, MASS::rlm, survival::coxph penalized fallback,
# flexsurv / sampleSelection, estimatr weights, fixest title prefixes).
# Each test pins engine-oracle values or exact strings for a branch the
# main per-engine frame test files do not reach.
# ---------------------------------------------------------------------------

# ---- lm legacy reshape: zero-coefficient fit ------------------------------

test_that("lm with no coefficients yields a zero-row schema coefs frame", {
  fit <- stats::lm(mpg ~ 0, data = mtcars)
  expect_identical(length(stats::coef(fit)), 0L)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_s3_class(fr, "spicy_regression_frame")
  expect_identical(nrow(fr$coefs), 0L)
  expect_identical(
    names(fr$coefs),
    c(
      "term",
      "parent_var",
      "label",
      "factor_level_pos",
      "is_ref",
      "estimate_type",
      "estimate",
      "std_error",
      "df",
      "statistic",
      "p_value",
      "ci_lower",
      "ci_upper",
      "test_type"
    )
  )
  expect_identical(fr$info$class, "lm")
})


# ---- lm helpers: .scalar_or_na / .vcov_label_from_kind --------------------

test_that(".scalar_or_na maps non-numeric, non-logical input to NA_real_", {
  # Defensive normaliser for malformed fit-stats slots: a character or
  # list slot must degrade to NA_real_, not crash the row build.
  expect_identical(spicy:::.scalar_or_na("abc"), NA_real_)
  expect_identical(spicy:::.scalar_or_na(list(1)), NA_real_)
  # Regular slots pass through: NULL / empty -> NA, numeric -> first,
  # logical -> coerced.
  expect_identical(spicy:::.scalar_or_na(NULL), NA_real_)
  expect_identical(spicy:::.scalar_or_na(numeric(0)), NA_real_)
  expect_identical(spicy:::.scalar_or_na(c(2.5, 9)), 2.5)
  expect_identical(spicy:::.scalar_or_na(TRUE), 1)
})

test_that(".vcov_label_from_kind falls back to the kind name when unknown", {
  expect_identical(spicy:::.vcov_label_from_kind("XYZ"), "XYZ")
  # Known kinds keep their curated labels.
  expect_identical(
    spicy:::.vcov_label_from_kind("HC3"),
    "HC3 heteroskedasticity-consistent"
  )
  expect_identical(
    spicy:::.vcov_label_from_kind("model", is_glm = TRUE),
    "Model-based (asymptotic)"
  )
})


# ---- MASS::rlm ------------------------------------------------------------

test_that("rlm with psi.hampel labels the psi function 'Hampel'", {
  skip_if_not_installed("MASS")
  fit <- MASS::rlm(mpg ~ wt, data = mtcars, psi = MASS::psi.hampel)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$psi_function, "Hampel")
})

test_that(".rlm_psi_label returns NA for a missing or non-function psi", {
  skip_if_not_installed("MASS")
  expect_identical(spicy:::.rlm_psi_label(list(psi = NULL)), NA_character_)
  expect_identical(spicy:::.rlm_psi_label(list(psi = "huber")), NA_character_)
})

test_that("rlm with a no-intercept factor emits no reference rows", {
  skip_if_not_installed("MASS")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  # y ~ 0 + f fits ALL levels: detect_factor_terms() flags the factor
  # with reference_dropped = FALSE, so no synthetic reference row.
  fit <- MASS::rlm(mpg ~ 0 + cyl_f, data = d)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(any(fr$coefs$is_ref))
  expect_identical(fr$coefs$term, c("cyl_f4", "cyl_f6", "cyl_f8"))
  expect_equal(fr$coefs$estimate, unname(stats::coef(fit)))
})


# ---- survival::coxph: penalized fit falls back to est/se ------------------

test_that("coxph penalized (ridge) fit routes through the est/se fallback", {
  skip_if_not_installed("survival")
  # summary.coxph for penalized fits reports Chisq/DF/p columns (no "z"),
  # so .coxph_coefs() falls back to statistic = est/se, Wald-normal p.
  ridge <- survival::ridge
  fit <- survival::coxph(
    survival::Surv(time, status) ~ ridge(age, ph.ecog, theta = 1),
    data = survival::lung
  )
  expect_false("z" %in% colnames(summary(fit)$coefficients))
  fr <- as_regression_frame(fit, model_id = "M1")
  est <- unname(stats::coef(fit))
  se <- unname(sqrt(diag(as.matrix(stats::vcov(fit)))))
  expect_identical(nrow(fr$coefs), 2L)
  expect_equal(fr$coefs$estimate, est)
  expect_equal(fr$coefs$std_error, se)
  expect_equal(fr$coefs$statistic, est / se)
  expect_equal(fr$coefs$p_value, 2 * stats::pnorm(-abs(est / se)))
  expect_identical(unique(fr$coefs$test_type), "z")
})


# ---- estimatr::lm_robust: weighted fit ------------------------------------

test_that("weighted lm_robust surfaces weighted_n = sum(weights)", {
  skip_if_not_installed("estimatr")
  d <- mtcars
  set.seed(1)
  d$w <- stats::runif(nrow(d), 0.5, 2)
  fit <- estimatr::lm_robust(mpg ~ wt, data = d, weights = w)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$extras$has_weights)
  expect_equal(fr$info$extras$weighted_n, sum(d$w))
})


# ---- fixest: family-aware title prefixes ----------------------------------

test_that(".fixest_title_prefix maps GLM families to family-aware titles", {
  # Direct helper calls: through as_regression_frame() these branches are
  # currently unreachable because .fixest_coefs() indexes the "z value"
  # column for every is_glm fit, but fixest reports "t value" columns for
  # free-dispersion families (Gamma / gaussian / inverse.gaussian) -- see
  # the incidental-bug note in the coverage task log.
  expect_identical(
    spicy:::.fixest_title_prefix(list(family = "Gamma", link = "log"), TRUE),
    "Gamma regression (fixed effects)"
  )
  expect_identical(
    spicy:::.fixest_title_prefix(
      list(family = "inverse.gaussian", link = "1/mu^2"),
      TRUE
    ),
    "Inverse-Gaussian regression (fixed effects)"
  )
  # Any other family name falls through to the capitalised default.
  expect_identical(
    spicy:::.fixest_title_prefix(
      list(family = "quasipoisson", link = "log"),
      TRUE
    ),
    "Quasipoisson regression (fixed effects)"
  )
  expect_identical(
    spicy:::.fixest_title_prefix(list(family = "gaussian", link = "log"), TRUE),
    "Gaussian regression (fixed effects)"
  )
})


# ---- flexsurv: polynomial-contrast factor + anc guard ----------------------

test_that("flexsurvreg with an ordered (poly-contrast) factor emits no reference rows", {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")
  ov <- survival::ovarian
  ov$ogrp <- ordered(cut(
    ov$age,
    breaks = c(0, 50, 60, 100),
    labels = c("lo", "mid", "hi")
  ))
  fit <- flexsurv::flexsurvreg(
    survival::Surv(futime, fustat) ~ ogrp,
    data = ov,
    dist = "weibull"
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  # Polynomial contrasts have no reference level: detect_factor_terms()
  # flags reference_dropped = FALSE and no synthetic row is emitted.
  expect_false(any(fr$coefs$is_ref))
  expect_identical(fr$coefs$term, c("ogrp.L", "ogrp.Q"))
  expect_equal(fr$coefs$estimate, unname(fit$res[c("ogrp.L", "ogrp.Q"), "est"]))
})

test_that(".flexsurv_has_anc_covariates is FALSE when the fit has no mx slot", {
  skip_if_not_installed("flexsurv")
  # Guard for fits without the parameter->covariate map: no mx, no anc.
  expect_false(spicy:::.flexsurv_has_anc_covariates(list(mx = NULL)))
  expect_false(spicy:::.flexsurv_has_anc_covariates(list(mx = list())))
})


# ---- sampleSelection: outcome DV name --------------------------------------

test_that("selection frame reads the DV from fit$outcome$formula when present", {
  skip_if_not_installed("sampleSelection")
  data("Mroz87", package = "sampleSelection", envir = environment())
  Mroz87$kids <- Mroz87$kids5 + Mroz87$kids618 > 0
  fit <- sampleSelection::selection(
    lfp ~ age + I(age^2) + faminc + kids + educ,
    wage ~ exper + I(exper^2) + educ + city,
    data = Mroz87,
    method = "2step"
  )
  # Real selection objects carry no $outcome component, so the dv falls
  # back to the generic "outcome" label.
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "outcome")
  # When $outcome$formula is present (the state .selection_info() reads),
  # the outcome-equation response name is surfaced as the dv.
  fit2 <- fit
  fit2$outcome <- list(formula = wage ~ exper + educ)
  fr2 <- as_regression_frame(fit2, model_id = "M1")
  expect_identical(fr2$info$dv, "wage")
  expect_identical(fr2$info$dv_label, "wage")
})
