# The SE of a random-effect row is a delta-method one on the scale the
# engine estimates on (decision 2026-11-19, dev/decisions/
# 2026-11-19-re-se-delta.md). On the same model lmer (merDeriv), lme
# (log sd, atanh rho) and glmmTMB (log sd, rho / sqrt(1 - rho^2)) then
# give the same SE. The oracle numbers are lmer's, written here with the
# call that gives them:
#   fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
#   td <- broom::tidy(table_regression(fit))
#   td[td$estimate_type == "vc", c("term", "std.error")]

.re_se_rows <- function(fit, ...) {
  td <- suppressMessages(suppressWarnings(
    broom::tidy(table_regression(fit, ...))
  ))
  vc <- td[td$estimate_type == "vc", ]
  stats::setNames(vc$std.error, vc$term)
}

.re_se_fits <- function(slope, reml) {
  skip_if_not_installed("lme4")
  skip_if_not_installed("merDeriv")
  skip_if_not_installed("nlme")
  skip_if_not_installed("glmmTMB")
  d <- lme4::sleepstudy
  if (slope) {
    list(
      lmer = lme4::lmer(Reaction ~ Days + (Days | Subject), d, REML = reml),
      lme = nlme::lme(
        Reaction ~ Days,
        random = ~ Days | Subject,
        data = d,
        method = if (reml) "REML" else "ML"
      ),
      glmmTMB = glmmTMB::glmmTMB(
        Reaction ~ Days + (Days | Subject),
        d,
        REML = reml
      )
    )
  } else {
    list(
      lmer = lme4::lmer(Reaction ~ Days + (1 | Subject), d, REML = reml),
      lme = nlme::lme(
        Reaction ~ Days,
        random = ~ 1 | Subject,
        data = d,
        method = if (reml) "REML" else "ML"
      ),
      glmmTMB = glmmTMB::glmmTMB(
        Reaction ~ Days + (1 | Subject),
        d,
        REML = reml
      )
    )
  }
}

test_that("random intercept, REML: the three engines give lmer's SE", {
  fits <- .re_se_fits(slope = FALSE, reml = TRUE)
  oracle <- c(
    "re::Subject::(Intercept)" = 6.8119058,
    "re::Residual::" = 1.7270739
  )
  # Measured: lme and glmmTMB within 2e-6 of lmer in relative terms.
  for (nm in names(fits)) {
    expect_equal(.re_se_rows(fits[[nm]]), oracle, tolerance = 1e-5, info = nm)
  }
  # Before 2026-11-19 lme and glmmTMB showed 6.9597 (the half-width of
  # the log-scale interval over z) and glmmTMB no residual SE.
})

test_that("random intercept, ML: the three engines give lmer's SE", {
  fits <- .re_se_fits(slope = FALSE, reml = FALSE)
  oracle <- c(
    "re::Subject::(Intercept)" = 6.4454588,
    "re::Residual::" = 1.7164130
  )
  for (nm in names(fits)) {
    expect_equal(.re_se_rows(fits[[nm]]), oracle, tolerance = 1e-5, info = nm)
  }
})

test_that("random slope, REML: glmmTMB gives lmer's SE, correlation included", {
  fits <- .re_se_fits(slope = TRUE, reml = TRUE)
  oracle <- c(
    "re::Subject::(Intercept)" = 5.83626394,
    "re::Subject::Days" = 1.24803549,
    "re::Subject::(Intercept), Days::cor" = 0.32502323,
    "re::Residual::" = 1.50801102
  )
  # Measured: glmmTMB within 1e-5 of lmer (exact Hessian on both sides).
  expect_equal(.re_se_rows(fits$lmer), oracle, tolerance = 1e-7)
  expect_equal(.re_se_rows(fits$glmmTMB), oracle, tolerance = 1e-4)
  # Same on the variance scale: SE(sigma^2) = 2 sigma^2 SE(log sigma).
  oracle_var <- c(
    "re::Subject::(Intercept)" = 288.78602008,
    "re::Subject::Days" = 14.78207595,
    "re::Subject::(Intercept), Days::cor" = 0.32502323,
    "re::Residual::" = 77.18542019
  )
  expect_equal(
    .re_se_rows(fits$lmer, re_scale = "variance"),
    oracle_var,
    tolerance = 1e-7
  )
  expect_equal(
    .re_se_rows(fits$glmmTMB, re_scale = "variance"),
    oracle_var,
    tolerance = 1e-4
  )
})

test_that("random slope, REML: lme reaches lmer's SE once its Hessian is accurate", {
  fits <- .re_se_fits(slope = TRUE, reml = TRUE)
  # nlme's apVar is a finite-difference Hessian. With the default step
  # (.relStep = eps^(1/3)) it is off on this model: 5.8880 for the
  # intercept SD (+0.9 %), 0.36691 for the correlation (+13 %). spicy
  # reports what nlme estimates, so those numbers are pinned as such.
  expect_equal(
    unname(.re_se_rows(fits$lme)),
    c(5.88803809, 1.25891261, 0.36691166, 1.50997506),
    tolerance = 1e-6
  )
  # A finer step gives lmer's numbers, which shows the delta method is
  # the same on both sides and the gap is the numerical Hessian.
  fit_fine <- nlme::lme(
    Reaction ~ Days,
    random = ~ Days | Subject,
    data = lme4::sleepstudy,
    control = nlme::lmeControl(.relStep = 1e-4)
  )
  expect_equal(
    .re_se_rows(fit_fine),
    c(
      "re::Subject::(Intercept)" = 5.83626394,
      "re::Subject::Days" = 1.24803549,
      "re::Subject::(Intercept), Days::cor" = 0.32502323,
      "re::Residual::" = 1.50801102
    ),
    tolerance = 1e-3
  )
})

test_that("random slope, ML: glmmTMB gives lmer's SE", {
  fits <- .re_se_fits(slope = TRUE, reml = FALSE)
  oracle <- c(
    "re::Subject::(Intercept)" = 5.57730430,
    "re::Subject::Days" = 1.18706812,
    "re::Subject::(Intercept), Days::cor" = 0.32350744,
    "re::Residual::" = 1.50801758
  )
  expect_equal(.re_se_rows(fits$lmer), oracle, tolerance = 1e-7)
  expect_equal(.re_se_rows(fits$glmmTMB), oracle, tolerance = 1e-4)
})

test_that("the delta-method SE leaves the engine intervals unchanged", {
  fits <- .re_se_fits(slope = FALSE, reml = TRUE)
  td <- suppressMessages(broom::tidy(table_regression(fits$lme)))
  vc <- td[td$estimate_type == "vc", ]
  ci <- nlme::intervals(fits$lme, which = "var-cov")
  subj <- ci$reStruct$Subject
  expect_equal(vc$conf.low[1L], subj[1L, "lower"], tolerance = 1e-8)
  expect_equal(vc$conf.high[1L], subj[1L, "upper"], tolerance = 1e-8)
  # The pre-change numbers, for the record: 25.909816 to 53.191367.
  expect_equal(vc$conf.low[1L], 25.909816, tolerance = 1e-7)
  expect_equal(vc$conf.high[1L], 53.191367, tolerance = 1e-7)
})

test_that(".re_delta_se falls back to the half-width off its scale", {
  z <- stats::qnorm(0.975)
  # Centred on log(2): delta method.
  s <- 0.1
  expect_equal(
    .re_delta_se(2, 2 * exp(-z * s), 2 * exp(z * s), z, "log"),
    2 * s
  )
  # Centred on atanh(0.3).
  expect_equal(
    .re_delta_se(
      0.3,
      tanh(atanh(0.3) - z * s),
      tanh(atanh(0.3) + z * s),
      z,
      "atanh"
    ),
    (1 - 0.3^2) * s
  )
  # Not centred on theta: half-width.
  expect_equal(.re_delta_se(0.3, 0.0, 0.5, z, "theta"), 0.5 / (2 * z))
})


# ---- Wald CI of lmer / glmer on log(sd) (decision 2026-11-19) -----------

.re_ci_rows <- function(fit, ...) {
  td <- suppressMessages(suppressWarnings(
    broom::tidy(table_regression(fit, ...))
  ))
  vc <- td[td$estimate_type == "vc" & !grepl("::cor$", td$term), ]
  cbind(low = vc$conf.low, high = vc$conf.high)
}

test_that("random intercept: lmer's Wald CI equals lme's and glmmTMB's", {
  for (reml in c(TRUE, FALSE)) {
    fits <- .re_se_fits(slope = FALSE, reml = reml)
    # Measured: the three engines agree within 1e-6 in relative terms.
    # lme4::lmer(Reaction ~ Days + (1 | Subject), sleepstudy, REML = TRUE):
    # 25.909827 to 53.191344 (Subject), 27.784543 to 34.568018 (residual);
    # before 2026-11-19, 19.669555 to 48.677157 and 27.397915 to 34.209171.
    oracle <- if (reml) {
      rbind(c(25.909827, 53.191344), c(27.784543, 34.568018))
    } else {
      rbind(c(25.357112, 51.144233), c(27.708009, 34.449528))
    }
    for (nm in names(fits)) {
      expect_equal(
        unname(.re_ci_rows(fits[[nm]])),
        oracle,
        tolerance = 1e-5,
        info = paste(nm, reml)
      )
    }
  }
})

test_that("random slope: lmer's Wald CI equals glmmTMB's, on both scales", {
  fits <- .re_se_fits(slope = TRUE, reml = TRUE)
  # lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  oracle <- rbind(
    c(15.581698, 39.283278),
    c(3.918282, 8.950789),
    c(22.800440, 28.724885)
  )
  expect_equal(unname(.re_ci_rows(fits$lmer)), oracle, tolerance = 1e-6)
  expect_equal(unname(.re_ci_rows(fits$glmmTMB)), oracle, tolerance = 1e-4)
  expect_equal(
    unname(.re_ci_rows(fits$lmer, re_scale = "variance")),
    oracle^2,
    tolerance = 1e-6
  )
})

test_that("the log-scale Wald CI is the one closer to the profile CI", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("merDeriv")
  # Why the scale was chosen (dev/decisions/2026-11-19-re-se-delta.md).
  # Dyestuff, six batches, Batch SD: the interval symmetric on the
  # variance gave 0 to 67.62, the profile is 12.20 to 84.06.
  fit <- lme4::lmer(Yield ~ 1 + (1 | Batch), lme4::Dyestuff)
  wald <- .re_ci_rows(fit)[1L, ]
  expect_equal(unname(wald), c(18.948906, 93.094942), tolerance = 1e-6)
  prof <- suppressMessages(
    stats::confint(fit, parm = "sd_(Intercept)|Batch", signames = FALSE)
  )
  expect_equal(unname(prof[1L, ]), c(12.198, 84.063), tolerance = 1e-3)
  old <- c(0, 67.618)
  expect_lt(sum(abs(wald - prof[1L, ])), sum(abs(old - prof[1L, ])))
  # A glmer logit, cbpp: 0 to 0.987 before, profile 0.346 to 1.100.
  gm <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    lme4::cbpp,
    family = binomial
  )
  expect_equal(
    unname(.re_ci_rows(gm)[1L, ]),
    c(0.325406, 1.267680),
    tolerance = 1e-5
  )
})

test_that("a singular lmer keeps its SE and CI omitted, without NaN or Inf", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("merDeriv")
  fit <- suppressMessages(lme4::lmer(Yield ~ 1 + (1 | Batch), lme4::Dyestuff2))
  expect_true(lme4::isSingular(fit))
  vc <- as_regression_frame(fit)$info$random_effects$variance_components
  expect_true(all(is.na(vc$std_error)))
  expect_true(all(is.na(vc$ci_lower)))
  expect_true(all(is.na(vc$ci_upper)))
  expect_false(any(is.nan(c(vc$std_error, vc$ci_lower, vc$ci_upper))))
})

test_that("re_ci = 'profile' is unchanged", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  # Profile bounds, unchanged by the 2026-11-19 lot.
  expect_equal(
    unname(.re_ci_rows(fit, re_ci = "profile")),
    rbind(c(26.007120, 52.935999), c(27.813847, 34.590995)),
    tolerance = 1e-5
  )
})
