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
  d <- mtcars
  d$cyl <- factor(d$cyl)
  suppressMessages(suppressWarnings(
    lme4::glmer(am ~ mpg + (1 | cyl), data = d, family = binomial)
  ))
}

.fit_glmmTMB_int <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_glmmTMB_slope <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

.fit_lme_int <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )
}

.fit_lme_slope <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ age | Subject
  )
}


# ---- 1. Schema: new columns are present ----------------------------------

test_that("variance_components carries Wald SE + CI columns for lmer", {
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(
    c("std_error", "ci_lower", "ci_upper", "ci_method") %in%
      colnames(vc)
  ))
})

test_that("variance_components carries Wald SE + CI columns for glmmTMB", {
  fit <- .fit_glmmTMB_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(
    c("std_error", "ci_lower", "ci_upper", "ci_method") %in%
      colnames(vc)
  ))
})

test_that("variance_components carries Wald SE + CI columns for lme", {
  fit <- .fit_lme_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(
    c("std_error", "ci_lower", "ci_upper", "ci_method") %in%
      colnames(vc)
  ))
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
  # Algebraic identity of the Wald mapping (default ci_level = 0.95):
  # ci_upper = variance + z * SE and ci_lower = max(0, variance - z * SE),
  # so the SE must be recoverable from the upper half-width exactly.
  z <- qnorm(0.975)
  expect_equal((vc$ci_upper - vc$variance) / z, vc$std_error, tolerance = 1e-12)
  expect_equal(
    vc$ci_lower,
    pmax(0, vc$variance - z * vc$std_error),
    tolerance = 1e-12
  )
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
  expect_equal(
    vc$std_error[vc$group == "Subject"],
    unname(expected_se[n_fixed + 1L]),
    tolerance = 1e-10
  )
  expect_equal(
    vc$std_error[vc$group == "Residual"],
    unname(expected_se[n_fixed + 2L]),
    tolerance = 1e-10
  )
})

test_that("lmer (random slope): SE/CI populated for intercept + slope + residual", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_slope()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  # Phase 7c7b: 2 group variance rows + 1 correlation row + 1 residual = 4
  expect_identical(nrow(vc), 4L)
  var_rows <- vc[!(vc$is_correlation %in% TRUE), ]
  expect_identical(nrow(var_rows), 3L)
  expect_true(all(is.finite(var_rows$std_error)))
  expect_true(all(var_rows$ci_lower >= 0)) # variance >= 0 enforced

  # Pin the 3 variance-row SEs to the merDeriv diagonal oracle.
  # Column-major vech layout of the 2x2 Subject block puts
  # var(Intercept) at position 1, cov at 2, var(Days) at 3; the
  # residual variance follows at position 4.
  v <- as.matrix(merDeriv::vcov.lmerMod(fit, full = TRUE, ranpar = "var"))
  n_fixed <- length(lme4::fixef(fit))
  re_se <- unname(sqrt(diag(v))[-seq_len(n_fixed)])
  expect_identical(var_rows$term, c("(Intercept)", "Days", ""))
  expect_equal(var_rows$std_error, re_se[c(1L, 3L, 4L)], tolerance = 1e-10)
  z <- qnorm(0.975)
  expect_equal(
    var_rows$ci_lower,
    pmax(0, var_rows$variance - z * re_se[c(1L, 3L, 4L)]),
    tolerance = 1e-10
  )
  expect_equal(
    var_rows$ci_upper,
    var_rows$variance + z * re_se[c(1L, 3L, 4L)],
    tolerance = 1e-10
  )

  # Correlation row: multivariate Delta-method oracle on the 3x3
  # sub-vcov of (var(Int), cov, var(Days)). rho = cov / sqrt(v1 * v2),
  # grad = (-rho/(2 v1), 1/sqrt(v1 v2), -rho/(2 v2)),
  # Var(rho) = grad' Sigma_3 grad; CI = rho +/- z * SE in [-1, 1].
  g_vc <- as.matrix(lme4::VarCorr(fit)$Subject)
  var_i <- g_vc[1L, 1L]
  var_j <- g_vc[2L, 2L]
  cov_ij <- g_vc[1L, 2L]
  rho <- cov_ij / sqrt(var_i * var_j)
  Sigma3 <- v[n_fixed + 1:3, n_fixed + 1:3]
  grad <- c(-rho / (2 * var_i), 1 / sqrt(var_i * var_j), -rho / (2 * var_j))
  se_rho <- sqrt(as.numeric(t(grad) %*% Sigma3 %*% grad))
  corr_row <- vc[vc$is_correlation %in% TRUE, ]
  expect_equal(corr_row$corr, rho, tolerance = 1e-10)
  expect_equal(corr_row$std_error, se_rho, tolerance = 1e-10)
  expect_equal(corr_row$ci_lower, max(-1, rho - z * se_rho), tolerance = 1e-10)
  expect_equal(corr_row$ci_upper, min(1, rho + z * se_rho), tolerance = 1e-10)
})

test_that("lmer Wald CI brackets the point estimate", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_int()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(all(vc$ci_lower <= vc$variance))
  expect_true(all(vc$variance <= vc$ci_upper))
  # Pin the exact bounds on top of the bracket check: Wald CI is
  # variance +/- qnorm(0.975) * SE, with SE the merDeriv observed-
  # information diagonal (RE variance then residual after the fixed
  # effects) and the lower bound clamped at 0.
  v <- as.matrix(merDeriv::vcov.lmerMod(fit, full = TRUE, ranpar = "var"))
  n_fixed <- length(lme4::fixef(fit))
  se_or <- unname(sqrt(diag(v))[n_fixed + c(1L, 2L)])
  z <- qnorm(0.975)
  expect_identical(vc$group, c("Subject", "Residual"))
  expect_equal(vc$ci_lower, pmax(0, vc$variance - z * se_or), tolerance = 1e-10)
  expect_equal(vc$ci_upper, vc$variance + z * se_or, tolerance = 1e-10)
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
  # Pin SE to the Delta-method oracle from glmmTMB's native Wald CI:
  # SE(sd^2) = 2 * sd * SE(sd), with SE(sd) = (upper - lower) / (2 * z).
  ci_sd <- as.matrix(confint(fit, method = "Wald", parm = "theta_"))
  z <- qnorm(0.975)
  se_sd <- (ci_sd[1L, "97.5 %"] - ci_sd[1L, "2.5 %"]) / (2 * z)
  expect_equal(
    group_row$std_error,
    unname(2 * ci_sd[1L, "Estimate"] * se_sd),
    tolerance = 1e-10
  )
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
    if (vc$group[i] == "Residual") {
      next
    }
    expect_equal(
      vc$ci_lower[i],
      max(0, ci_sd[1L, "2.5 %"])^2,
      tolerance = 1e-10
    )
    expect_equal(vc$ci_upper[i], ci_sd[1L, "97.5 %"]^2, tolerance = 1e-10)
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
  # Pin SEs to the nlme::intervals() Delta-method oracle:
  # SE(sd^2) = 2 * sd * SE(sd), with SE(sd) = (upper - lower) / (2 * z).
  ci <- nlme::intervals(fit, which = "var-cov")
  z <- qnorm(0.975)
  subj_ci <- ci$reStruct$Subject["sd((Intercept))", ]
  se_subj <- 2 *
    subj_ci[["est."]] *
    (subj_ci[["upper"]] - subj_ci[["lower"]]) /
    (2 * z)
  expect_equal(vc$std_error[vc$group == "Subject"], se_subj, tolerance = 1e-10)
  se_res <- 2 *
    unname(ci$sigma["est."]) *
    (unname(ci$sigma["upper"]) - unname(ci$sigma["lower"])) /
    (2 * z)
  expect_equal(vc$std_error[vc$group == "Residual"], se_res, tolerance = 1e-10)
})

test_that("lme (random slope): SE/CI populated for both group rows + residual", {
  fit <- .fit_lme_slope()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  # Phase 7c7b: 2 group variance rows + 1 correlation row + 1 residual = 4
  expect_identical(nrow(vc), 4L)
  expect_true(all(is.finite(vc$std_error)))
  # Pin every row to the nlme::intervals() oracle. Variance rows use
  # the Delta method SE(sd^2) = 2 * sd * SE(sd); the correlation row is
  # on the natural rho scale, so SE = (upper - lower) / (2 * z) and the
  # CI is intervals()' cor row verbatim.
  ci <- nlme::intervals(fit, which = "var-cov")
  z <- qnorm(0.975)
  subj <- ci$reStruct$Subject
  se_int <- 2 *
    subj["sd((Intercept))", "est."] *
    (subj["sd((Intercept))", "upper"] -
      subj["sd((Intercept))", "lower"]) /
    (2 * z)
  se_age <- 2 *
    subj["sd(age)", "est."] *
    (subj["sd(age)", "upper"] - subj["sd(age)", "lower"]) /
    (2 * z)
  expect_equal(
    vc$std_error[
      vc$group == "Subject" &
        vc$term == "(Intercept)"
    ],
    se_int,
    tolerance = 1e-10
  )
  expect_equal(
    vc$std_error[vc$group == "Subject" & vc$term == "age"],
    se_age,
    tolerance = 1e-10
  )
  corr_row <- vc[vc$is_correlation %in% TRUE, ]
  expect_equal(
    corr_row$corr,
    subj["cor((Intercept),age)", "est."],
    tolerance = 1e-10
  )
  expect_equal(
    corr_row$std_error,
    (subj["cor((Intercept),age)", "upper"] -
      subj["cor((Intercept),age)", "lower"]) /
      (2 * z),
    tolerance = 1e-10
  )
  expect_equal(
    corr_row$ci_lower,
    subj["cor((Intercept),age)", "lower"],
    tolerance = 1e-10
  )
  expect_equal(
    corr_row$ci_upper,
    subj["cor((Intercept),age)", "upper"],
    tolerance = 1e-10
  )
  se_res <- 2 *
    unname(ci$sigma["est."]) *
    (unname(ci$sigma["upper"]) - unname(ci$sigma["lower"])) /
    (2 * z)
  expect_equal(vc$std_error[vc$group == "Residual"], se_res, tolerance = 1e-10)
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
  expect_equal(subj$ci_upper, sd_upper^2, tolerance = 1e-10)
  # Residual row
  res <- vc[vc$group == "Residual", ]
  expect_equal(res$ci_lower, max(0, ci$sigma["lower"])^2, tolerance = 1e-10)
  expect_equal(res$ci_upper, unname(ci$sigma["upper"])^2, tolerance = 1e-10)
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
