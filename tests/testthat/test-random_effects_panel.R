# ---------------------------------------------------------------------------
# Phase 7c7c tests: structured multi-line random-effects panel in the footer
# Note. Replaces the legacy one-line sentence when SE / CI are populated.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_lmer_int_panel <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_lmer_slope_panel <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

.fit_glmmTMB_slope_panel <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject),
                    data = lme4::sleepstudy)
}

.fit_lme_slope_panel <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(distance ~ age + Sex, data = nlme::Orthodont,
            random = ~ age | Subject)
}


# ---- 1. Panel renders for lmer (random intercept) -----------------------

test_that("lmer random-intercept output has structured 'Random effects' panel", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_int_panel()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Random effects (REML):", fixed = TRUE)
  expect_match(combined, "σ Subject (Intercept)", fixed = TRUE)
  expect_match(combined, "σ (Residual)",         fixed = TRUE)
  expect_match(combined, "ICC",                  fixed = TRUE)
  expect_match(combined, "N (Subject)",          fixed = TRUE)
})


# ---- 2. Panel renders for lmer random slope ----------------------------

test_that("lmer random-slope output includes ρ row", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_slope_panel()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "σ Subject (Intercept)", fixed = TRUE)
  expect_match(combined, "σ Subject Days",        fixed = TRUE)
  expect_match(combined, "ρ Subject",             fixed = TRUE)
})


# ---- 3. glmmTMB panel ---------------------------------------------------

test_that("glmmTMB panel includes σ + ρ with SE/CI", {
  fit <- .fit_glmmTMB_slope_panel()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Random effects",   fixed = TRUE)
  expect_match(combined, "σ Subject",        fixed = TRUE)
  expect_match(combined, "ρ Subject",        fixed = TRUE)
})


# ---- 4. lme panel ------------------------------------------------------

test_that("lme panel renders for nlme::lme", {
  fit <- .fit_lme_slope_panel()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Random effects (REML):", fixed = TRUE)
  expect_match(combined, "σ Subject (Intercept)",  fixed = TRUE)
  expect_match(combined, "σ Subject age",          fixed = TRUE)
  expect_match(combined, "ρ Subject",              fixed = TRUE)
  expect_match(combined, "σ (Residual)",           fixed = TRUE)
})


# ---- 5. Non-mixed classes: NO panel ------------------------------------

test_that("lm output has no Random effects panel", {
  fit <- lm(mpg ~ wt + cyl, data = mtcars)
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_false(grepl("Random effects", combined, fixed = TRUE))
})


# ---- 6. Panel uses SD scale (Gelman default) ---------------------------

test_that("panel values are on SD scale, not variance scale", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_int_panel()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  # SD = sqrt(variance) for the Subject Intercept row
  sd_value <- sqrt(vc$variance[vc$group == "Subject"])
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  # The SD value (e.g. 37.12) should appear in the panel, NOT the
  # variance (1378.18).
  expect_match(combined, sprintf("%.2f", sd_value), fixed = TRUE)
  expect_false(grepl(sprintf("%.2f", vc$variance[vc$group == "Subject"]),
                     combined, fixed = TRUE))
})


# ---- 7. Header carries the estimator label (REML / ML) ----------------

test_that("panel header annotates lmer (REML default) with '(REML)'", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_int_panel()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Random effects (REML):", fixed = TRUE)
})

test_that("panel header annotates lmer (REML=FALSE) with '(ML)'", {
  skip_if_not_installed("merDeriv")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject),
                    data = lme4::sleepstudy, REML = FALSE)
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Random effects (ML):", fixed = TRUE)
})


# ---- 8. NA values render as em-dash -----------------------------------

test_that("lme4 correlation row (SE NA) renders em-dash", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_slope_panel()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  # lme4's correlation row has NA SE / CI (full Delta-method deferred)
  # so the ρ row's SE should render as em-dash.
  expect_match(combined, "ρ Subject", fixed = TRUE)
  expect_match(combined, "—",         fixed = TRUE)
})
