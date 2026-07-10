# RMST-difference and risk-difference columns for coxph fits
# (survival phase lot 2; spec dev/survival_estimands_spec.md): native
# g-computation + bootstrap SEs.
#
# Oracle provenance (constants pinned 2026-07-10, lung data with
# status recoded 0/1, complete cases on time/status/age/sex,
# coxph(Surv(time, status01) ~ age + sex)):
#   * dRMST(365) Female - Male = 38.44068
#     == adjustedCurves 0.11.4 adjusted_rmst(method = "direct") on
#     the same fit (286.4828 - 248.0422);
#   * dRisk(365) Female - Male = -0.1825651
#     == riskRegression 2026.3.11 ate(fit, treatment = "sex",
#     times = 365) g-formula difference (-0.18257).
# survRM2 is deliberately NOT an oracle: it estimates per-arm
# Kaplan-Meier RMST, a different (unconstrained) estimand from the
# model-standardized one.

.est_lung <- function() {
  d <- survival::lung
  d$sex <- factor(d$sex, levels = c(1, 2), labels = c("Male", "Female"))
  d[stats::complete.cases(d[, c("time", "status", "age", "sex")]), ]
}


test_that("point estimates match the pinned g-computation oracles", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex,
                         data = d)
  pts <- spicy:::.coxph_estimand_points(
    fit, spicy:::.coxph_estimand_data(fit),
    want_rmst = TRUE, want_risk = TRUE, tau = 365, at_time = 365
  )
  expect_equal(pts$rmst[pts$term == "sexFemale"], 38.44068,
               tolerance = 1e-6)
  expect_equal(pts$risk[pts$term == "sexFemale"], -0.1825651,
               tolerance = 1e-6)

  # Independent in-test recomputation of the whole pipeline (baseline
  # hazard + counterfactual linear predictors + step integral), for
  # every contrast including the continuous +1-unit one.
  bh <- survival::basehaz(fit, centered = FALSE)
  std_curve <- function(newdata) {
    elp <- exp(stats::predict(fit, newdata = newdata, type = "lp",
                              reference = "zero"))
    vapply(bh$hazard, function(h) mean(exp(-h * elp)), numeric(1))
  }
  rmst_of <- function(s) {
    keep <- bh$time <= 365
    sum(diff(c(0, bh$time[keep], 365)) * c(1, s[keep]))
  }
  d_m <- d; d_m$sex <- factor("Male",   levels = levels(d$sex))
  d_f <- d; d_f$sex <- factor("Female", levels = levels(d$sex))
  expect_equal(pts$rmst[pts$term == "sexFemale"],
               rmst_of(std_curve(d_f)) - rmst_of(std_curve(d_m)),
               tolerance = 1e-10)
  d_p <- d; d_p$age <- d$age + 1
  expect_equal(pts$rmst[pts$term == "age"],
               rmst_of(std_curve(d_p)) - rmst_of(std_curve(d)),
               tolerance = 1e-10)
  s_f <- std_curve(d_f); s_m <- std_curve(d_m)
  at <- function(s) s[findInterval(365, bh$time)]
  expect_equal(pts$risk[pts$term == "sexFemale"],
               (1 - at(s_f)) - (1 - at(s_m)), tolerance = 1e-10)
})


test_that("the step-function integral and landmark reader are exact", {
  # S = 1 on [0,2), 0.8 on [2,5), 0.5 on [5,9), 0.2 from 9.
  times <- c(2, 5, 9)
  surv  <- c(0.8, 0.5, 0.2)
  expect_equal(spicy:::.step_rmst(times, surv, 10),
               2 * 1 + 3 * 0.8 + 4 * 0.5 + 1 * 0.2)
  expect_equal(spicy:::.step_rmst(times, surv, 4), 2 * 1 + 2 * 0.8)
  # tau before the first event: survival is still 1 throughout.
  expect_equal(spicy:::.step_rmst(times, surv, 1.5), 1.5)
  expect_identical(spicy:::.step_surv_at(times, surv, 1), 1)
  expect_equal(spicy:::.step_surv_at(times, surv, 5), 0.5)
  expect_equal(spicy:::.step_surv_at(times, surv, 100), 0.2)
})


test_that("the full table renders estimand columns with inference", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex,
                         data = d)
  set.seed(7)
  tr <- table_regression(
    fit,
    show_columns = c("b", "rmst", "rmst_ci", "rmst_p",
                     "risk_diff", "risk_diff_ci"),
    tau = 365, at_time = 365, boot_n = 40
  )
  out <- paste(capture.output(print(tr)), collapse = "\n")
  expect_match(out, "dRMST (365)", fixed = TRUE)
  expect_match(out, "dRisk (365)", fixed = TRUE)
  expect_match(out, "restricted mean survival time over [0, 365]",
               fixed = TRUE)
  expect_match(out, "cumulative incidence at 365", fixed = TRUE)
  expect_match(out, "g-computation", fixed = TRUE)
  expect_match(out, "bootstrap", fixed = TRUE)

  td <- broom::tidy(tr)
  r <- td[td$estimate_type == "rmst" & td$term == "sexFemale", ]
  expect_equal(r$estimate, 38.44068, tolerance = 1e-6)
  expect_true(is.finite(r$std.error) && r$std.error > 0)
  expect_lt(r$conf.low, r$estimate)
  expect_gt(r$conf.high, r$estimate)
  expect_true(r$p.value >= 0 && r$p.value <= 1)
  rd <- td[td$estimate_type == "risk_diff" & td$term == "sexFemale", ]
  expect_equal(rd$estimate, -0.1825651, tolerance = 1e-6)

  # Bootstrap reproducibility under the same seed.
  set.seed(7)
  tr2 <- table_regression(
    fit,
    show_columns = c("b", "rmst", "rmst_ci", "rmst_p",
                     "risk_diff", "risk_diff_ci"),
    tau = 365, at_time = 365, boot_n = 40
  )
  td2 <- broom::tidy(tr2)
  expect_equal(td$std.error, td2$std.error, tolerance = 1e-12)
})


test_that("tau = 'minmax' resolves to the smallest per-group maximum", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex,
                         data = d)
  set.seed(1)
  tr <- table_regression(fit, show_columns = c("b", "rmst"),
                         tau = "minmax", boot_n = 30)
  expected <- min(tapply(d$time, d$sex, max))
  out <- paste(capture.output(print(tr)), collapse = "\n")
  expect_match(out, sprintf("dRMST (%s)", format(expected)),
               fixed = TRUE)
  expect_match(out, sprintf("[0, %s]", format(expected)), fixed = TRUE)
})


test_that("horizons are explicit, mandatory, and refused when unused", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = d)
  expect_error(table_regression(fit, show_columns = c("b", "rmst")),
               class = "spicy_invalid_input")
  expect_error(table_regression(fit, show_columns = c("b", "risk_diff")),
               class = "spicy_invalid_input")
  expect_error(table_regression(fit, show_columns = c("b", "p"),
                                tau = 365),
               class = "spicy_invalid_input")
  expect_error(table_regression(fit, show_columns = c("b", "p"),
                                at_time = 365),
               class = "spicy_invalid_input")
  expect_error(table_regression(fit, show_columns = c("b", "rmst"),
                                tau = -1),
               class = "spicy_invalid_input")
})


test_that("structural gates: class, start-stop, uv screen", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  expect_error(
    table_regression(stats::lm(mpg ~ wt, data = mtcars),
                     show_columns = c("b", "rmst"), tau = 5),
    class = "spicy_invalid_input"
  )
  # Stratified fits are SUPPORTED since the strata lot -- the gate must
  # pass; full coverage lives in test-survival_estimands_strata.R.
  fit_str <- survival::coxph(
    survival::Surv(time, status) ~ age + survival::strata(sex), data = d
  )
  expect_silent(spicy:::.coxph_estimand_gates(fit_str, "M1"))
  d3 <- data.frame(t1 = c(0, 0, 2, 3, 0, 1), t2 = c(2, 3, 5, 8, 4, 6),
                   ev = c(0, 1, 1, 0, 1, 0), x = rnorm(6))
  fit_cp <- suppressWarnings(
    survival::coxph(survival::Surv(t1, t2, ev) ~ x, data = d3)
  )
  expect_error(
    table_regression(fit_cp, show_columns = c("b", "rmst"), tau = 5),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_uv(d, outcome = Surv(time, status),
                        predictors = c(age, sex), method = "coxph",
                        show_columns = c("n", "b", "rmst")),
    class = "spicy_invalid_input"
  )
})


test_that("a bootstrap that mostly fails raises spicy_resampling_failed", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = d)
  testthat::local_mocked_bindings(
    .coxph_refit_on = function(...) stop("boom"),
    .package = "spicy"
  )
  set.seed(2)
  expect_error(
    table_regression(fit, show_columns = c("b", "rmst"), tau = 365,
                     boot_n = 10),
    class = "spicy_resampling_failed"
  )
})
