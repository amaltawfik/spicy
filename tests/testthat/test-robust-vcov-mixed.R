# C2 increment 2: cluster-robust (CR*) standard errors for mixed-effects fits.
# The B-row inference is recomputed from clubSandwich::vcovCR via the shared
# .apply_robust_vcov_to_coefs(); cross-checked against clubSandwich directly.

.mixed_b_se <- function(fr) {
  b <- fr$coefs[fr$coefs$estimate_type == "B", ]
  stats::setNames(b$std_error, b$term)
}

test_that("lmer CR2 SE + Satterthwaite df match clubSandwich exactly", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("clubSandwich")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  cl <- lme4::sleepstudy$Subject
  fr <- as_regression_frame(
    fit,
    vcov = "CR2",
    cluster = cl,
    cluster_name = "Subject"
  )
  b <- fr$coefs[fr$coefs$estimate_type == "B", ]
  ct <- clubSandwich::coef_test(
    fit,
    vcov = "CR2",
    cluster = cl,
    test = "Satterthwaite"
  )
  expect_equal(unname(b$std_error), unname(ct$SE), tolerance = 1e-6)
  expect_equal(unname(b$df), unname(ct$df_Satt), tolerance = 1e-6)
  expect_true(all(b$test_type == "t"))
  expect_identical(
    fr$info$vcov_label,
    "cluster-robust (CR2), clusters by Subject"
  )
})

test_that("nlme::lme CR2 SE matches clubSandwich", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("clubSandwich")
  d <- nlme::Orthodont
  fit <- nlme::lme(distance ~ age, random = ~ 1 | Subject, data = d)
  fr <- as_regression_frame(
    fit,
    vcov = "CR2",
    cluster = d$Subject,
    cluster_name = "Subject"
  )
  ct <- clubSandwich::coef_test(
    fit,
    vcov = "CR2",
    cluster = d$Subject,
    test = "Satterthwaite"
  )
  expect_equal(unname(.mixed_b_se(fr)), unname(ct$SE), tolerance = 1e-6)
})

test_that("lmer / lme CR2: factor joint tests and betas use the robust matrix", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("nlme")
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("car")
  d <- as.data.frame(nlme::BodyWeight)
  fits <- list(
    lme4::lmer(weight ~ Time + Diet + (1 | Rat), data = d),
    nlme::lme(weight ~ Time + Diet, random = ~ 1 | Rat, data = d)
  )
  for (fit in fits) {
    V <- clubSandwich::vcovCR(fit, cluster = d$Rat, type = "CR2")
    fr <- as_regression_frame(
      fit,
      vcov = "CR2",
      cluster = d$Rat,
      show_columns = c("b", "partial_chi2"),
      standardized = "refit"
    )
    chi2 <- fr$coefs[fr$coefs$estimate_type == "partial_chi2", ]
    oracle <- car::linearHypothesis(fit, c("Diet2 = 0", "Diet3 = 0"), vcov. = V)
    expect_equal(
      chi2$estimate[chi2$term == "Diet2"],
      oracle$Chisq[2L],
      tolerance = 1e-8
    )
    # The rescaling of the refit leaves the statistics of the robust
    # B rows unchanged.
    b <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
    beta <- fr$coefs[fr$coefs$estimate_type == "beta", ]
    slopes <- c("Time", "Diet2", "Diet3")
    expect_equal(
      beta$statistic[match(slopes, beta$term)],
      b$statistic[match(slopes, b$term)],
      tolerance = 1e-6
    )
  }
})

test_that("glmmTMB CR2 is refused (its own sandwich has one type, CR0)", {
  # clubSandwich has no vcovCR.glmmTMB: the call would dispatch to
  # vcovCR.default, whose result is numerically invalid for glmmTMB
  # (~360x-deflated SEs on a Poisson random-intercept check, 2026-08-05).
  # CR0 comes from glmmTMB's own sandwich (test-vcov_glmmTMB.R); the
  # bias-reduced CR1-CR3 are refused up front.
  suppressWarnings(requireNamespace("glmmTMB", quietly = TRUE))
  skip_if_not_installed("glmmTMB")
  fit <- suppressWarnings(
    glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  )
  expect_error(
    table_regression(
      fit,
      vcov = "CR2",
      cluster = ~Subject,
      output = "data.frame"
    ),
    class = "spicy_unsupported_vcov"
  )
})

test_that("classical (default) mixed SEs are untouched by the robust path", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  se_model <- .mixed_b_se(as_regression_frame(fit, vcov = "model"))
  se_classical <- .mixed_b_se(as_regression_frame(fit, vcov = "classical"))
  expect_equal(se_model, se_classical)
})

test_that("glmer + HC* are refused for mixed fits (fail fast, no silent fallback)", {
  skip_if_not_installed("lme4")
  d <- transform(
    lme4::sleepstudy,
    y = as.integer(Reaction > stats::median(Reaction))
  )
  glmer_fit <- suppressMessages(
    lme4::glmer(y ~ Days + (1 | Subject), d, family = binomial)
  )
  # glmer: clubSandwich::vcovCR errors -> refuse rather than fall back.
  expect_error(
    table_regression(
      glmer_fit,
      vcov = "CR2",
      cluster = ~Subject,
      output = "data.frame"
    ),
    class = "spicy_unsupported_vcov"
  )
  # HC* is an OLS concept, not defined for mixed models.
  lmer_fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  expect_error(
    table_regression(lmer_fit, vcov = "HC3", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
})
