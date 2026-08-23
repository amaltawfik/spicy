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
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  pts <- spicy:::.coxph_estimand_points(
    fit,
    spicy:::.coxph_estimand_data(fit),
    want_rmst = TRUE,
    want_risk = TRUE,
    tau = 365,
    at_time = 365
  )
  expect_equal(pts$rmst[pts$term == "sexFemale"], 38.44068, tolerance = 1e-6)
  expect_equal(pts$risk[pts$term == "sexFemale"], -0.1825651, tolerance = 1e-6)

  # Independent in-test recomputation of the whole pipeline (baseline
  # hazard + counterfactual linear predictors + step integral), for
  # every contrast including the continuous +1-unit one.
  bh <- survival::basehaz(fit, centered = FALSE)
  std_curve <- function(newdata) {
    elp <- exp(stats::predict(
      fit,
      newdata = newdata,
      type = "lp",
      reference = "zero"
    ))
    vapply(bh$hazard, function(h) mean(exp(-h * elp)), numeric(1))
  }
  rmst_of <- function(s) {
    keep <- bh$time <= 365
    sum(diff(c(0, bh$time[keep], 365)) * c(1, s[keep]))
  }
  d_m <- d
  d_m$sex <- factor("Male", levels = levels(d$sex))
  d_f <- d
  d_f$sex <- factor("Female", levels = levels(d$sex))
  expect_equal(
    pts$rmst[pts$term == "sexFemale"],
    rmst_of(std_curve(d_f)) - rmst_of(std_curve(d_m)),
    tolerance = 1e-10
  )
  d_p <- d
  d_p$age <- d$age + 1
  expect_equal(
    pts$rmst[pts$term == "age"],
    rmst_of(std_curve(d_p)) - rmst_of(std_curve(d)),
    tolerance = 1e-10
  )
  s_f <- std_curve(d_f)
  s_m <- std_curve(d_m)
  at <- function(s) s[findInterval(365, bh$time)]
  expect_equal(
    pts$risk[pts$term == "sexFemale"],
    (1 - at(s_f)) - (1 - at(s_m)),
    tolerance = 1e-10
  )
})


test_that("truncating the baseline grid at the horizon changes nothing", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  dat <- spicy:::.coxph_estimand_data(fit)

  # The engine as it stood before the grid was cut at max(tau, at_time):
  # the whole baseline grid, every point evaluated, the ones past the
  # horizon discarded by .step_rmst() / .step_surv_at() afterwards.
  # Rebuilt from the same helpers, so the comparison is the truncation
  # and nothing else -- and it is `identical()`, not a tolerance: a
  # point beyond the horizon cannot enter either reader, so removing it
  # is exact by construction.
  untruncated <- function(tau, at_time) {
    bl <- spicy:::.coxph_baseline(fit)
    curve_stats <- function(newdata) {
      s <- spicy:::.coxph_standardized_survival(fit, newdata, bl$H0, bl$s_idx)
      c(
        rmst = spicy:::.step_rmst(bl$times, s, tau),
        risk = 1 - spicy:::.step_surv_at(bl$times, s, at_time)
      )
    }
    spicy:::.estimand_contrast_rows(dat, c("age", "sex"), curve_stats)
  }
  shipped <- function(tau, at_time) {
    got <- spicy:::.coxph_estimand_points(
      fit,
      dat,
      want_rmst = TRUE,
      want_risk = TRUE,
      tau = tau,
      at_time = at_time
    )
    attr(got, "skipped_terms") <- NULL
    got
  }

  # (a) the horizon the fixtures use: most of the grid survives.
  expect_identical(shipped(365, 365), untruncated(365, 365))
  # (b) a low horizon: about a quarter of the grid survives.
  q1 <- unname(stats::quantile(d$time, 0.25))
  expect_identical(shipped(q1, q1), untruncated(q1, q1))
  # (c) at_time beyond tau -- the cut is at the MAX of the two, so a
  # landmark past the RMST horizon still reads the right step.
  expect_identical(shipped(200, 700), untruncated(200, 700))
  # (d) the horizon sitting exactly ON a grid point: the cut is `<=`,
  # so that point is kept and the landmark still reads it. (`<` would
  # drop it and .step_surv_at() would fall back to the previous step --
  # invisible on the RMST integral, whose last width is then 0, and
  # visible only here.)
  on_grid <- spicy:::.coxph_baseline(fit)$times[50L]
  expect_identical(shipped(on_grid, on_grid), untruncated(on_grid, on_grid))
  # (e) both horizons before the first event time: the grid empties and
  # the standardized curve is 1 throughout.
  t1 <- min(spicy:::.coxph_baseline(fit)$times)
  expect_identical(shipped(t1 / 2, t1 / 2), untruncated(t1 / 2, t1 / 2))
  expect_true(all(shipped(t1 / 2, t1 / 2)$rmst == 0))

  # Only rmst is asked for: at_time is NULL and must not enter max().
  bl <- spicy:::.coxph_baseline(fit)
  only_rmst <- spicy:::.coxph_estimand_points(
    fit,
    dat,
    want_rmst = TRUE,
    want_risk = FALSE,
    tau = 200,
    at_time = NULL
  )
  expect_identical(only_rmst$rmst, shipped(200, 700)$rmst)
  expect_true(all(is.na(only_rmst$risk)))
  # ... and symmetrically, tau = NULL with only the landmark asked for.
  only_risk <- spicy:::.coxph_estimand_points(
    fit,
    dat,
    want_rmst = FALSE,
    want_risk = TRUE,
    tau = NULL,
    at_time = 700
  )
  expect_identical(only_risk$risk, shipped(200, 700)$risk)
  expect_true(all(is.na(only_risk$rmst)))
})


test_that("the estimand rows' df parameter is inert at its Inf default", {
  skip_if_not_installed("survival")
  # (a) the arithmetic. The whole parameterisation rests on qt/pt at
  # df = Inf being qnorm/pnorm to the last bit -- true, but arithmetic
  # rather than a documented guarantee, so it is pinned rather than
  # assumed. Nine coverage levels x eleven statistics, `identical()`.
  cls <- c(0.5, 0.6, 0.68, 0.75, 0.8, 0.9, 0.95, 0.99, 0.999)
  for (cl in cls) {
    expect_identical(
      stats::qt(1 - (1 - cl) / 2, df = Inf),
      stats::qnorm(0.5 + cl / 2)
    )
  }
  for (s in c(0, 1e-8, 0.001, 0.5, 1, 1.644854, 1.959964, 2.5, 3.5, 7, 40)) {
    expect_identical(
      2 * stats::pt(abs(s), df = Inf, lower.tail = FALSE),
      2 * stats::pnorm(-abs(s))
    )
  }

  # (b) the rows themselves, at the default: the normal-approximation
  # bootstrap layering, recomputed from estimate and std_error.
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  set.seed(11)
  res <- spicy:::.coxph_estimand_rows(
    fit,
    model_id = "M1",
    outcome = NA_character_,
    show_columns = c("rmst", "risk_diff"),
    tau = 365,
    at_time = 365,
    ci_level = 0.9,
    boot_n = 25
  )
  r <- res$rows
  z <- stats::qnorm(0.5 + 0.9 / 2)
  expect_identical(r$ci_lower, r$estimate - z * r$std_error)
  expect_identical(r$ci_upper, r$estimate + z * r$std_error)
  expect_identical(r$p_value, 2 * stats::pnorm(-abs(r$statistic)))
  expect_identical(r$df, rep(Inf, nrow(r)))
  expect_identical(unique(r$test_type), "z")
})


test_that("a finite df turns the estimand rows into a Wald-t", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  hooks <- list(
    gates_fn = spicy:::.coxph_estimand_gates,
    data_fn = spicy:::.coxph_estimand_data,
    points_fn = spicy:::.coxph_estimand_points,
    refit_fn = function(fit, dboot) {
      spicy:::.coxph_refit_on(stats::formula(fit), dboot)
    }
  )
  run <- function(df) {
    set.seed(11)
    do.call(
      spicy:::.survival_estimand_rows,
      c(
        list(
          fit = fit,
          model_id = "M1",
          show_columns = c("rmst", "risk_diff"),
          tau = 365,
          at_time = 365,
          ci_level = 0.9,
          boot_n = 25,
          df = df
        ),
        hooks
      )
    )$rows
  }
  inf_rows <- run(Inf)
  t_rows <- run(12)

  # The point estimates and the bootstrap SEs are the same draw; only
  # the reference distribution moves.
  expect_identical(t_rows$estimate, inf_rows$estimate)
  expect_identical(t_rows$std_error, inf_rows$std_error)
  expect_identical(unique(t_rows$test_type), "t")
  expect_identical(t_rows$df, rep(12, nrow(t_rows)))
  crit <- stats::qt(1 - (1 - 0.9) / 2, df = 12)
  expect_identical(t_rows$ci_lower, t_rows$estimate - crit * t_rows$std_error)
  expect_identical(
    t_rows$p_value,
    2 * stats::pt(abs(t_rows$statistic), df = 12, lower.tail = FALSE)
  )
  # A t at 12 df is wider and less significant than the normal.
  expect_true(all(
    t_rows$ci_upper - t_rows$ci_lower >
      inf_rows$ci_upper -
        inf_rows$ci_lower
  ))
  expect_true(all(t_rows$p_value > inf_rows$p_value))

  # And the default is the Inf branch: the harness called through the
  # coxph entry point lands on the same rows.
  set.seed(11)
  via_coxph <- spicy:::.coxph_estimand_rows(
    fit,
    model_id = "M1",
    outcome = NA_character_,
    show_columns = c("rmst", "risk_diff"),
    tau = 365,
    at_time = 365,
    ci_level = 0.9,
    boot_n = 25
  )$rows
  expect_identical(via_coxph, inf_rows)
})


test_that("the standardization population can be weighted, and NULL is not", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  bl <- spicy:::.coxph_baseline(fit)

  # NULL takes the expression it always took: the plain mean over
  # subjects, recomputed here from predict() alone.
  elp <- exp(stats::predict(fit, newdata = d, type = "lp", reference = "zero"))
  expect_identical(
    spicy:::.coxph_standardized_survival(fit, d, bl$H0, NULL),
    vapply(bl$H0[, 1L], function(h) mean(exp(-h * elp)), numeric(1))
  )
  expect_identical(
    spicy:::.coxph_standardized_survival(fit, d, bl$H0, NULL, w = NULL),
    spicy:::.coxph_standardized_survival(fit, d, bl$H0, NULL)
  )

  # Weighted: integer weights are subject duplication, so the weighted
  # average over `d` must equal the unweighted average over the data
  # `d` expands to. That is the property a design weight has to have,
  # and it is not the formula restated.
  set.seed(4)
  w <- sample(1:4, nrow(d), replace = TRUE)
  expanded <- d[rep(seq_len(nrow(d)), w), , drop = FALSE]
  expect_equal(
    spicy:::.coxph_standardized_survival(fit, d, bl$H0, NULL, w = w),
    spicy:::.coxph_standardized_survival(fit, expanded, bl$H0, NULL),
    tolerance = 1e-12
  )
  # Constant weights are the unweighted average.
  expect_equal(
    spicy:::.coxph_standardized_survival(
      fit,
      d,
      bl$H0,
      NULL,
      w = rep(2, nrow(d))
    ),
    spicy:::.coxph_standardized_survival(fit, d, bl$H0, NULL),
    tolerance = 1e-12
  )
  # A weight that concentrates on one subject gives that subject's own
  # curve, not the sample's.
  one <- c(1, rep(0, nrow(d) - 1L))
  expect_equal(
    spicy:::.coxph_standardized_survival(fit, d, bl$H0, NULL, w = one),
    exp(-bl$H0[, 1L] * elp[[1L]]),
    tolerance = 1e-12
  )
})


test_that("the bootstrap refit carries weights through the same environment", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  f <- survival::Surv(time, status) ~ age + sex
  fit <- survival::coxph(f, data = d)

  # NULL: the call the engine has always built, with no `weights` in
  # it, and a basehaz() that resolves.
  plain <- spicy:::.coxph_refit_on(f, d)
  expect_identical(names(plain$call), c("", "formula", "data"))
  expect_identical(plain$call$data, quote(.spicy_boot_data.))
  expect_identical(unname(stats::coef(plain)), unname(stats::coef(fit)))
  expect_s3_class(survival::basehaz(plain, centered = FALSE), "data.frame")

  # Weighted: the weights are a SECOND slot in the same environment.
  # Replacing the formula's environment is the mechanism, so a weights
  # vector left in the caller's frame is invisible to coxph's own
  # model.frame(), which evaluates the extra arguments there -- see the
  # counter-witness at the end of this test.
  set.seed(5)
  w <- stats::runif(nrow(d), 0.5, 2)
  wt <- spicy:::.coxph_refit_on(f, d, wboot = w)
  expect_identical(names(wt$call), c("", "formula", "data", "weights"))
  expect_identical(wt$call$weights, quote(.spicy_boot_w.))
  ref <- survival::coxph(f, data = d, weights = w)
  expect_equal(
    unname(stats::coef(wt)),
    unname(stats::coef(ref)),
    tolerance = 1e-12
  )
  # The point of the environment: the baseline runs on the replicate.
  bh <- survival::basehaz(wt, centered = FALSE)
  expect_s3_class(bh, "data.frame")
  expect_equal(
    bh$hazard,
    survival::basehaz(ref, centered = FALSE)$hazard,
    tolerance = 1e-12
  )
  # ... and it is a different fit from the unweighted one.
  expect_false(isTRUE(all.equal(
    unname(stats::coef(wt)),
    unname(stats::coef(plain))
  )))

  # The counter-witness: the same refit with the weights left in the
  # caller's frame instead of the environment slot. coxph() evaluates
  # its extra arguments in `environment(formula)`, which the helper has
  # just replaced, so the vector is not found -- at the fit step, not
  # at the baseline step.
  local_weights_refit <- function(f, dboot) {
    env <- new.env(parent = environment(f))
    env$.spicy_boot_data. <- dboot
    wv <- stats::runif(nrow(dboot), 0.5, 2)
    f2 <- f
    environment(f2) <- env
    eval(
      substitute(
        survival::coxph(FF, data = .spicy_boot_data., weights = wv),
        list(FF = f2)
      ),
      env
    )
  }
  expect_error(local_weights_refit(f, d), "wv")
})


test_that("the step-function integral and landmark reader are exact", {
  # S = 1 on [0,2), 0.8 on [2,5), 0.5 on [5,9), 0.2 from 9.
  times <- c(2, 5, 9)
  surv <- c(0.8, 0.5, 0.2)
  expect_equal(
    spicy:::.step_rmst(times, surv, 10),
    2 * 1 + 3 * 0.8 + 4 * 0.5 + 1 * 0.2
  )
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
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  set.seed(7)
  tr <- table_regression(
    fit,
    show_columns = c(
      "b",
      "rmst",
      "rmst_ci",
      "rmst_p",
      "risk_diff",
      "risk_diff_ci"
    ),
    tau = 365,
    at_time = 365,
    boot_n = 40
  )
  out <- paste(capture.output(print(tr)), collapse = "\n")
  expect_match(out, "dRMST (365)", fixed = TRUE)
  expect_match(out, "dRisk (365)", fixed = TRUE)
  expect_match(out, "restricted mean survival time over [0, 365]", fixed = TRUE)
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
    show_columns = c(
      "b",
      "rmst",
      "rmst_ci",
      "rmst_p",
      "risk_diff",
      "risk_diff_ci"
    ),
    tau = 365,
    at_time = 365,
    boot_n = 40
  )
  td2 <- broom::tidy(tr2)
  expect_equal(td$std.error, td2$std.error, tolerance = 1e-12)
})


test_that("tau = 'minmax' resolves to the smallest per-group maximum", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  set.seed(1)
  tr <- table_regression(
    fit,
    show_columns = c("b", "rmst"),
    tau = "minmax",
    boot_n = 30
  )
  expected <- min(tapply(d$time, d$sex, max))
  out <- paste(capture.output(print(tr)), collapse = "\n")
  expect_match(out, sprintf("dRMST (%s)", format(expected)), fixed = TRUE)
  expect_match(out, sprintf("[0, %s]", format(expected)), fixed = TRUE)
})


test_that("horizons are explicit, mandatory, and refused when unused", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = d)
  expect_error(
    table_regression(fit, show_columns = c("b", "rmst")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, show_columns = c("b", "risk_diff")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, show_columns = c("b", "p"), tau = 365),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, show_columns = c("b", "p"), at_time = 365),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, show_columns = c("b", "rmst"), tau = -1),
    class = "spicy_invalid_input"
  )
})


test_that("structural gates: class, start-stop, uv screen", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  expect_error(
    table_regression(
      stats::lm(mpg ~ wt, data = mtcars),
      show_columns = c("b", "rmst"),
      tau = 5
    ),
    class = "spicy_invalid_input"
  )
  # Stratified fits are SUPPORTED since the strata lot -- the gate must
  # pass; full coverage lives in test-survival_estimands_strata.R.
  fit_str <- survival::coxph(
    survival::Surv(time, status) ~ age + survival::strata(sex),
    data = d
  )
  expect_silent(spicy:::.coxph_estimand_gates(fit_str, "M1"))
  d3 <- data.frame(
    t1 = c(0, 0, 2, 3, 0, 1),
    t2 = c(2, 3, 5, 8, 4, 6),
    ev = c(0, 1, 1, 0, 1, 0),
    x = rnorm(6)
  )
  fit_cp <- suppressWarnings(
    survival::coxph(survival::Surv(t1, t2, ev) ~ x, data = d3)
  )
  expect_error(
    table_regression(fit_cp, show_columns = c("b", "rmst"), tau = 5),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_uv(
      d,
      outcome = Surv(time, status),
      predictors = c(age, sex),
      method = "coxph",
      show_columns = c("n", "b", "rmst")
    ),
    class = "spicy_invalid_input"
  )
})


# Phase 3 matrix – vignettes-news:cox-ame-refused (estimand-gate half)
# (lot T4)

test_that("rms::cph and flexsurvreg support neither rmst nor risk_diff", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  skip_if_not_installed("rms")
  fit_cph <- rms::cph(
    survival::Surv(time, status) ~ age + sex,
    data = d,
    x = TRUE,
    y = TRUE
  )
  expect_error(
    table_regression(fit_cph, show_columns = c("b", "rmst"), tau = 365),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(
      fit_cph,
      show_columns = c("b", "risk_diff"),
      at_time = 365
    ),
    class = "spicy_invalid_input"
  )
  skip_if_not_installed("flexsurv")
  fit_fs <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age,
    data = d,
    dist = "weibull"
  )
  expect_error(
    table_regression(fit_fs, show_columns = c("b", "rmst"), tau = 365),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(
      fit_fs,
      show_columns = c("b", "risk_diff"),
      at_time = 365
    ),
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
    table_regression(
      fit,
      show_columns = c("b", "rmst"),
      tau = 365,
      boot_n = 10
    ),
    class = "spicy_resampling_failed"
  )
})


# ============================================================================
# Phase 3 matrix – rd-uv-estimands:estimand-token-families-accepted
# ============================================================================

test_that("the eight estimand tokens render in token order; variants error", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  # The three never-exercised tokens (rmst_se, risk_diff_se,
  # risk_diff_p), interleaved across families: the display order is
  # the token order, with naked SE / p sub-column headers.
  set.seed(13)
  tr <- table_regression(
    fit,
    show_columns = c("risk_diff_se", "rmst", "rmst_se", "risk_diff_p"),
    tau = 365,
    at_time = 365,
    boot_n = 25
  )
  expect_identical(names(tr), c("Variable", "SE", "dRMST (365)", "SE.2", "p"))
  s <- as_structured(tr)
  toks <- vapply(s$col_meta, function(m) m$token, character(1))
  expect_identical(
    unname(toks),
    c("risk_diff_se", "rmst", "rmst_se", "risk_diff_p")
  )
  # All eight tokens together: each family renders its anchor + SE +
  # CI + p, and the SE columns are the bootstrap SDs backing the CIs.
  set.seed(13)
  tr8 <- table_regression(
    fit,
    show_columns = c(
      "rmst",
      "rmst_se",
      "rmst_ci",
      "rmst_p",
      "risk_diff",
      "risk_diff_se",
      "risk_diff_ci",
      "risk_diff_p"
    ),
    tau = 365,
    at_time = 365,
    boot_n = 25
  )
  expect_true(all(c("dRMST (365)", "dRisk (365)") %in% names(tr8)))
  td <- broom::tidy(tr8)
  z <- stats::qnorm(0.975)
  r <- td[td$estimate_type == "rmst" & td$term == "sexFemale", ]
  expect_equal(r$conf.high - r$conf.low, 2 * z * r$std.error, tolerance = 1e-10)
  rd <- td[td$estimate_type == "risk_diff" & td$term == "sexFemale", ]
  expect_equal(
    rd$conf.high - rd$conf.low,
    2 * z * rd$std.error,
    tolerance = 1e-10
  )
  expect_true(rd$p.value >= 0 && rd$p.value <= 1)
  # Unknown variants of the estimand families are refused.
  expect_error(
    table_regression(fit, show_columns = c("b", "rmst_foo"), tau = 365),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, show_columns = c("b", "risk_diff_cl"), at_time = 365),
    class = "spicy_invalid_input"
  )
})

test_that("transformed terms get no orphan estimand row; note + caveat fire", {
  # Spec dev/registre_rendu_estimands_spec.md (phase 4, option a): the
  # g-computation contrast is defined per RAW variable, so I(age/10)
  # used to spawn an orphan `age` row with a +1-YEAR contrast next to a
  # per-decade coefficient. Transformed-only variables are now skipped,
  # the footer discloses them, and an all-transformed fit warns.
  skip_if_not_installed("survival")
  lung2 <- na.omit(survival::lung[, c(
    "time",
    "status",
    "age",
    "sex",
    "ph.ecog"
  )])
  lung2$sex <- factor(lung2$sex, labels = c("Male", "Female"))
  fit <- survival::coxph(
    survival::Surv(time, status) ~ I(age / 10) + sex + ph.ecog,
    data = lung2
  )
  set.seed(7)
  out <- table_regression(
    fit,
    show_columns = c("b", "rmst"),
    tau = 365,
    exponentiate = TRUE,
    boot_n = 20
  )
  df <- as.data.frame(out)
  expect_false(any(trimws(df$Variable) == "age"))
  expect_true(any(trimws(df$Variable) == "I(age/10)"))
  expect_match(
    paste(attr(out, "note"), collapse = " "),
    "Transformed terms (I(age/10))",
    fixed = TRUE
  )
  # Untransformed predictors keep their estimand rows.
  s <- as_structured(out)
  expect_true("dRMST (365)" %in% names(s$body))
  expect_false(all(is.na(s$body[["dRMST (365)"]])))
  # All-transformed fit: classed caveat, no estimand rows, no crash.
  fit2 <- survival::coxph(
    survival::Surv(time, status) ~ I(age / 10),
    data = lung2
  )
  set.seed(7)
  expect_warning(
    table_regression(
      fit2,
      show_columns = c("b", "rmst"),
      tau = 365,
      boot_n = 20
    ),
    class = "spicy_caveat"
  )
})


# The estimand rows are rbind()ed onto coefs BEFORE new_regression_frame()
# runs (regression_frame_survival.R), so "rmst" / "risk_diff" are frame
# estimate_type values like any other -- and the schema validator's
# allowed_types vocabulary has to know them. It did not, so a coxph frame
# carrying estimands failed its own schema check.
test_that("a frame carrying estimand rows passes the schema validator", {
  skip_if_not_installed("survival")
  d <- .est_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  set.seed(4)
  fr <- spicy:::as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "rmst", "risk_diff"),
    tau = 365,
    at_time = 365,
    boot_n = 15
  )
  expect_true(all(c("rmst", "risk_diff") %in% fr$coefs$estimate_type))
  expect_true(spicy:::validate_regression_frame(fr))
})
