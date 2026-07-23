# ---------------------------------------------------------------------------
# Coverage: glmmTMB frame method branches not hit by the functional tests.
# Targets the reference-row builder (factor predictors), the random-slope
# correlation rows, the no-factor empty path, and the gls-vs-Gaussian ICC.
# ---------------------------------------------------------------------------

.fit_glmmTMB_factor_cov <- function() {
  skip_if_not_installed("glmmTMB")
  d <- lme4::sleepstudy
  d$grp <- factor(rep(c("a", "b", "c"), length.out = nrow(d)))
  glmmTMB::glmmTMB(Reaction ~ Days + grp + (1 | Subject), data = d)
}

.fit_glmmTMB_slope_cov <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}


# ---- 1. Factor predictor: reference row built ---------------------------

test_that("glmmTMB with a factor predictor emits a reference row", {
  fit <- .fit_glmmTMB_factor_cov()
  fr <- as_regression_frame(fit, model_id = "M1")
  ref <- fr$coefs[fr$coefs$is_ref %in% TRUE, ]
  expect_true(nrow(ref) >= 1L)
  # The reference row has an NA estimate and carries the factor parent_var.
  expect_true(all(is.na(ref$estimate)))
  expect_true("grp" %in% ref$parent_var)
})

test_that("glmmTMB with NO factor predictor has no reference rows", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(any(fr$coefs$is_ref %in% TRUE))
})


# ---- 2. Random-slope: correlation row with SE / CI ----------------------

test_that("glmmTMB random slope carries a correlation row with finite SE/CI", {
  fit <- .fit_glmmTMB_slope_cov()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  corr <- vc[vc$is_correlation %in% TRUE, ]
  expect_true(nrow(corr) >= 1L)
  expect_true(all(is.finite(corr$std_error)))
  expect_true(all(is.finite(corr$ci_lower)))
  expect_true(all(is.finite(corr$ci_upper)))
})


# ---- 3. n_groups extraction from summary()$ngrps ------------------------

test_that("glmmTMB n_groups is pulled from summary()$ngrps$cond", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  ng <- fr$info$n_groups
  expect_true(!is.null(ng))
  expect_identical(unname(ng[["Subject"]]), 18L)
})


# ---- 4. End-to-end render with a factor + slope -------------------------

test_that("table_regression(glmmTMB factor) renders factor block + panel", {
  fit <- .fit_glmmTMB_factor_cov()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "grp:", fixed = TRUE)
  expect_match(combined, "Random effects", fixed = TRUE)
  expect_match(combined, "N (Subject)", fixed = TRUE)
})
