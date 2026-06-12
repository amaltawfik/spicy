# ---------------------------------------------------------------------------
# Phase 7c7a tests: Wald SE + 95% CI on variance scale for random effects
# across the 4 mixed-effects classes (lmer, glmer, glmmTMB, lme).
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_lmer_int <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_lmer_slope <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

.fit_glmer_singular <- function() {
  skip_if_not_installed("lme4")
  d <- mtcars; d$cyl <- factor(d$cyl)
  suppressMessages(suppressWarnings(
    lme4::glmer(am ~ mpg + (1 | cyl), data = d, family = binomial)
  ))
}

.fit_glmmTMB_int <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject),
                    data = lme4::sleepstudy)
}

.fit_glmmTMB_slope <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject),
                    data = lme4::sleepstudy)
}

.fit_lme_int <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(distance ~ age + Sex, data = nlme::Orthodont,
            random = ~ 1 | Subject)
}

.fit_lme_slope <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(distance ~ age + Sex, data = nlme::Orthodont,
            random = ~ age | Subject)
}


# ---- 1. Schema: new columns are present ----------------------------------

test_that("variance_components carries Wald SE + CI columns for lmer", {
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(c("std_error", "ci_lower", "ci_upper", "ci_method") %in%
                  colnames(vc)))
})

test_that("variance_components carries Wald SE + CI columns for glmmTMB", {
  fit <- .fit_glmmTMB_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(c("std_error", "ci_lower", "ci_upper", "ci_method") %in%
                  colnames(vc)))
})

test_that("variance_components carries Wald SE + CI columns for lme", {
  fit <- .fit_lme_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(c("std_error", "ci_lower", "ci_upper", "ci_method") %in%
                  colnames(vc)))
})


# ---- 2. lmer: SE + CI populated (non-singular fit) -----------------------

test_that("lmer (random intercept): SE + CI are finite for all rows", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(is.finite(vc$std_error)))
  expect_true(all(is.finite(vc$ci_lower)))
  expect_true(all(is.finite(vc$ci_upper)))
  expect_true(all(vc$ci_method == "wald"))
})

test_that("lmer (random intercept): SE matches merDeriv direct output", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  v <- as.matrix(merDeriv::vcov.lmerMod(fit, full = TRUE, ranpar = "var"))
  expected_se <- sqrt(diag(v))
  n_fixed <- length(lme4::fixef(fit))
  # For random intercept only: position 1 of RE block, then residual
  expect_equal(vc$std_error[vc$group == "Subject"],
               unname(expected_se[n_fixed + 1L]),
               tolerance = 1e-10)
  expect_equal(vc$std_error[vc$group == "Residual"],
               unname(expected_se[n_fixed + 2L]),
               tolerance = 1e-10)
})

test_that("lmer (random slope): SE/CI populated for intercept + slope + residual", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_slope()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  # 2 group rows (Intercept var + Days var) + 1 residual = 3 rows
  expect_identical(nrow(vc), 3L)
  expect_true(all(is.finite(vc$std_error)))
  expect_true(all(vc$ci_lower >= 0))  # variance >= 0 enforced
})

test_that("lmer Wald CI brackets the point estimate", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(vc$ci_lower <= vc$variance))
  expect_true(all(vc$variance <= vc$ci_upper))
})


# ---- 3. glmer singular fit: SE/CI = NA (no crash) -----------------------

test_that("glmer singular fit: SE/CI gracefully degrade to NA", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_glmer_singular()
  expect_true(lme4::isSingular(fit))
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(is.na(vc$std_error)))
  expect_true(all(is.na(vc$ci_method)))
})


# ---- 4. glmmTMB: SE/CI from native confint(method="Wald") ---------------

test_that("glmmTMB (random intercept): SE + CI populated for group row", {
  fit <- .fit_glmmTMB_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  group_row <- vc[vc$group != "Residual", ]
  expect_true(is.finite(group_row$std_error))
  expect_identical(group_row$ci_method, "wald")
})

test_that("glmmTMB residual SE = NA (confint doesn't include residual)", {
  fit <- .fit_glmmTMB_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  resid_row <- vc[vc$group == "Residual", ]
  expect_true(is.na(resid_row$std_error))
})

test_that("glmmTMB CI on variance = (SD CI)^2 (Delta-method roundtrip)", {
  fit <- .fit_glmmTMB_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  ci_sd <- confint(fit, method = "Wald", parm = "theta_")
  for (i in seq_len(nrow(vc))) {
    if (vc$group[i] == "Residual") next
    expect_equal(vc$ci_lower[i], max(0, ci_sd[1L, "2.5 %"])^2,
                 tolerance = 1e-10)
    expect_equal(vc$ci_upper[i], ci_sd[1L, "97.5 %"]^2,
                 tolerance = 1e-10)
  }
})


# ---- 5. lme: SE/CI from nlme::intervals() ------------------------------

test_that("lme (random intercept): SE + CI populated for all rows", {
  fit <- .fit_lme_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(is.finite(vc$std_error)))
  expect_true(all(is.finite(vc$ci_lower)))
  expect_true(all(is.finite(vc$ci_upper)))
  expect_true(all(vc$ci_method == "wald"))
})

test_that("lme (random slope): SE/CI populated for both group rows + residual", {
  fit <- .fit_lme_slope()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_identical(nrow(vc), 3L)
  expect_true(all(is.finite(vc$std_error)))
})

test_that("lme CI matches intervals() output squared", {
  fit <- .fit_lme_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  ci <- nlme::intervals(fit, which = "var-cov")
  # Subject (Intercept) row
  subj <- vc[vc$group == "Subject", ]
  sd_lower <- ci$reStruct$Subject["sd((Intercept))", "lower"]
  sd_upper <- ci$reStruct$Subject["sd((Intercept))", "upper"]
  expect_equal(subj$ci_lower, max(0, sd_lower)^2, tolerance = 1e-10)
  expect_equal(subj$ci_upper, sd_upper^2,         tolerance = 1e-10)
  # Residual row
  res <- vc[vc$group == "Residual", ]
  expect_equal(res$ci_lower, max(0, ci$sigma["lower"])^2,
               tolerance = 1e-10)
  expect_equal(res$ci_upper, unname(ci$sigma["upper"])^2,
               tolerance = 1e-10)
})


# ---- 6. Validator accepts the extended schema --------------------------

test_that("validator accepts variance_components with new SE/CI columns", {
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("validator accepts glmmTMB extended schema", {
  fit <- .fit_glmmTMB_int()
  fr <- as_regression_frame(fit)
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("validator accepts lme extended schema", {
  fit <- .fit_lme_int()
  fr <- as_regression_frame(fit)
  expect_invisible(spicy:::validate_regression_frame(fr))
})


# ---- 7. CI bounds are non-negative (variance constraint) ---------------

test_that("ci_lower is clamped at 0 (variance non-negative)", {
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(vc$ci_lower[!is.na(vc$ci_lower)] >= 0))
})
