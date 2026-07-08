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
  # Pin the complete lines (panel header, sigma rows, fit stats, LRT footer);
  # σ = sigma, │ = box bar, – = en dash, χ̄² = chibar2.
  expect_match(combined, " Random effects:         │", fixed = TRUE)
  expect_match(combined,
    "   σ Subject (Intercept) │   37.12  6.81  [ 19.67,  48.68]   –",
    fixed = TRUE)
  expect_match(combined,
    "   σ (Residual)          │   30.99  1.73  [ 27.40,  34.21]   –",
    fixed = TRUE)
  expect_match(combined, " ICC                     │    0.59", fixed = TRUE)
  expect_match(combined, " N (Subject)             │   18",    fixed = TRUE)
  # chibar2 built from codepoints: the combining macron is invisible in
  # source and easily normalized away by editors.
  chibar2 <- intToUtf8(c(0x03c7, 0x0304, 0x00b2))
  expect_match(combined,
    paste0("Random effects (REML): LR test vs linear regression, ",
           chibar2, "(1) = 106.21, p < .001."),
    fixed = TRUE)
})


# ---- 2. Panel renders for lmer random slope ----------------------------

test_that("lmer random-slope output includes ρ row", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_slope_panel()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  # Pin the complete σ / ρ rows (label + estimate + SE + CI + dash).
  expect_match(combined,
    "   σ Subject (Intercept)         │   24.74  5.84  [  6.79,  34.32]   –",
    fixed = TRUE)
  expect_match(combined,
    "   σ Subject Days                │    5.92  1.25  [  2.47,   8.00]   –",
    fixed = TRUE)
  expect_match(combined,
    "   ρ Subject ((Intercept), Days) │    0.07  0.33  [ -0.57,   0.70]   –",
    fixed = TRUE)
})


# ---- 3. glmmTMB panel ---------------------------------------------------

test_that("glmmTMB panel includes σ + ρ with SE/CI", {
  fit <- .fit_glmmTMB_slope_panel()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  # Pin the complete panel header, σ / ρ rows, and LRT footer (glmmTMB is ML).
  expect_match(combined, " Random effects:                 │", fixed = TRUE)
  expect_match(combined,
    "   σ Subject (Intercept)         │   23.78  5.78  [ 15.02,  37.66]   –",
    fixed = TRUE)
  expect_match(combined,
    "   σ Subject Days                │    5.72  1.22  [  3.81,   8.59]   –",
    fixed = TRUE)
  expect_match(combined,
    "   ρ Subject ((Intercept), Days) │    0.08  0.27  [ -0.49,   0.59]   –",
    fixed = TRUE)
  chibar2 <- intToUtf8(c(0x03c7, 0x0304, 0x00b2))
  expect_match(combined,
    paste0("Random effects (ML): LR test vs linear regression, ",
           chibar2, "(3) = 148.35, p < .001."),
    fixed = TRUE)
})


# ---- 4. lme panel ------------------------------------------------------

test_that("lme panel renders for nlme::lme", {
  fit <- .fit_lme_slope_panel()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  # Pin the complete σ / ρ rows and the REML LRT footer.
  expect_match(combined,
    "   σ Subject (Intercept)        │   2.80  1.11  [ 1.36,  5.73]   –",
    fixed = TRUE)
  expect_match(combined,
    "   σ Subject age                │   0.23  0.10  [ 0.10,  0.50]   –",
    fixed = TRUE)
  expect_match(combined,
    "   ρ Subject ((Intercept), age) │  -0.77  0.21  [-0.95, -0.14]   –",
    fixed = TRUE)
  expect_match(combined,
    "   σ (Residual)                 │   1.31  0.13  [ 1.08,  1.58]   –",
    fixed = TRUE)
  chibar2 <- intToUtf8(c(0x03c7, 0x0304, 0x00b2))
  expect_match(combined,
    paste0("Random effects (REML): LR test vs linear regression, ",
           chibar2, "(3) = 47.85, p < .001."),
    fixed = TRUE)
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
  # variance (1378.18). Bind the value to its own σ row so a match
  # elsewhere in the table cannot satisfy the check.
  expect_match(combined,
    sprintf("   σ Subject (Intercept) │   %.2f", sd_value),
    fixed = TRUE)
  expect_false(grepl(sprintf("%.2f", vc$variance[vc$group == "Subject"]),
                     combined, fixed = TRUE))
})


# ---- 7. Header carries the estimator label (REML / ML) ----------------

test_that("panel header annotates lmer (REML default) with '(REML)'", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_int_panel()
  out <- capture.output(print(table_regression(fit)))
  # Pin the whole LRT footer line, not just its "(REML):" prefix.
  lrt_line <- grep("^Random effects \\(", out, value = TRUE)
  chibar2 <- intToUtf8(c(0x03c7, 0x0304, 0x00b2))
  expect_identical(
    lrt_line,
    paste0("Random effects (REML): LR test vs linear regression, ",
           chibar2, "(1) = 106.21, p < .001."))
})

test_that("panel header annotates lmer (REML=FALSE) with '(ML)'", {
  skip_if_not_installed("merDeriv")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject),
                    data = lme4::sleepstudy, REML = FALSE)
  out <- capture.output(print(table_regression(fit)))
  # Pin the whole LRT footer line, not just its "(ML):" prefix.
  lrt_line <- grep("^Random effects \\(", out, value = TRUE)
  chibar2 <- intToUtf8(c(0x03c7, 0x0304, 0x00b2))
  expect_identical(
    lrt_line,
    paste0("Random effects (ML): LR test vs linear regression, ",
           chibar2, "(1) = 106.21, p < .001."))
})


# ---- 8. Correlation row shows rho with SE + CI in the rows layout -------

test_that("lme4 correlation row shows rho with SE + CI (rows layout)", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_slope_panel()
  df <- table_regression(fit, show_columns = c("b", "se", "ci"),
                         output = "data.frame")
  # the correlation row's label ends in ", Days)"; unique to it
  rho <- df[grepl(", Days)", df$Variable, fixed = TRUE), , drop = FALSE]
  expect_equal(nrow(rho), 1L)
  # Pin the complete row label (two-space panel indent included).
  expect_identical(rho$Variable[1], "  ρ Subject ((Intercept), Days)")
  se_col <- grep("SE", names(df), value = TRUE)[1]
  ci_col <- grep("CI", names(df), value = TRUE)[1]
  # Phase 7c17: multivariate Delta-method populates SE + CI on the rho row.
  expect_true(grepl("[0-9]", rho[[se_col]][1]))
  expect_true(grepl("[0-9]", rho[[ci_col]][1]))
})
