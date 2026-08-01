# RMST / risk-difference columns in the coxph univariable screen.
# Each per-predictor bootstrap runs on its own univariable fit; the
# multivariable merge column carries the adjusted estimand from the
# full fit. Point estimates must equal the directly-fit equivalents.

.uv_lung <- function() {
  d <- stats::na.omit(
    survival::lung[, c("time", "status", "age", "sex", "ph.ecog")]
  )
  d$sex <- factor(d$sex, 1:2, c("male", "female"))
  d$ecog <- factor(ifelse(d$ph.ecog >= 1, "ecog1plus", "ecog0"))
  d$ph.ecog <- NULL
  d
}


test_that("uv-screen estimands equal the directly fit models", {
  skip_if_not_installed("survival")
  d <- .uv_lung()
  set.seed(21)
  out <- table_regression_uv(
    d,
    outcome = survival::Surv(time, status),
    predictors = c(age, sex, ecog),
    method = "coxph",
    show_columns = c("b", "rmst"),
    tau = 500,
    boot_n = 30,
    output = "long"
  )
  uv_rows <- out[
    out$model_id == "Univariable" &
      out$estimate_type == "rmst",
  ]

  fit_sex <- survival::coxph(
    survival::Surv(time, status) ~ sex,
    data = d
  )
  direct <- spicy:::.coxph_estimand_points(
    fit_sex,
    spicy:::.coxph_estimand_data(fit_sex),
    want_rmst = TRUE,
    want_risk = FALSE,
    tau = 500,
    at_time = NULL
  )
  expect_equal(
    uv_rows$estimate[uv_rows$term == "sexfemale"],
    direct$rmst,
    tolerance = 1e-10
  )

  # The multivariable column carries the ADJUSTED estimand.
  fit_multi <- survival::coxph(
    survival::Surv(time, status) ~ age + sex + ecog,
    data = d
  )
  direct_m <- spicy:::.coxph_estimand_points(
    fit_multi,
    spicy:::.coxph_estimand_data(fit_multi),
    want_rmst = TRUE,
    want_risk = FALSE,
    tau = 500,
    at_time = NULL
  )
  mv_rows <- out[
    out$model_id == "Multivariable" &
      out$estimate_type == "rmst",
  ]
  expect_equal(
    mv_rows$estimate[mv_rows$term == "sexfemale"],
    direct_m$rmst[direct_m$term == "sexfemale"],
    tolerance = 1e-10
  )
})


test_that("the rendered screen carries the horizon header and one footer", {
  skip_if_not_installed("survival")
  d <- .uv_lung()
  set.seed(5)
  out <- paste(
    capture.output(print(table_regression_uv(
      d,
      outcome = survival::Surv(time, status),
      predictors = c(sex, ecog),
      method = "coxph",
      show_columns = c("b", "rmst"),
      tau = 500,
      boot_n = 30
    ))),
    collapse = "\n"
  )
  expect_match(out, "dRMST (500)", fixed = TRUE)
  expect_match(out, "restricted mean survival time over \\[0, 500\\]")
  # One deduped disclosure line, not one per model group.
  expect_identical(
    lengths(regmatches(out, gregexpr("g-computation", out))),
    1L
  )
})


test_that("minmax and non-coxph screens are refused", {
  skip_if_not_installed("survival")
  d <- .uv_lung()
  expect_error(
    table_regression_uv(
      d,
      outcome = survival::Surv(time, status),
      predictors = c(age),
      method = "coxph",
      show_columns = c("b", "rmst"),
      tau = "minmax"
    ),
    class = "spicy_invalid_input"
  )
  d$dead <- as.integer(d$status == 2)
  expect_error(
    table_regression_uv(
      d,
      outcome = dead,
      predictors = c(age),
      method = "glm",
      show_columns = c("b", "rmst"),
      tau = 500
    ),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# Phase 3 matrix – rd-uv-estimands:uv-estimand-bootstrap-per-fit
# ============================================================================

test_that("each univariable fit runs its own reproducible estimand bootstrap", {
  # rd-uv-estimands:uv-estimand-bootstrap-per-fit – each block's
  # SE/CI must come from a bootstrap of ITS OWN univariable fit, not
  # from one shared bootstrap of the full model.
  skip_if_not_installed("survival")
  d <- .uv_lung()
  set.seed(9)
  scr <- table_regression_uv(
    d,
    outcome = survival::Surv(time, status),
    predictors = c(sex, ecog),
    method = "coxph",
    show_columns = c("b", "rmst", "rmst_se", "rmst_ci"),
    tau = 500,
    boot_n = 25,
    multivariable = FALSE,
    output = "long"
  )
  uv <- scr[scr$estimate_type == "rmst", ]
  # Manual per-predictor bootstraps, replayed in screen order under the
  # same seed: block k reproduces the bootstrap of its own fit exactly.
  fit_sex <- survival::coxph(survival::Surv(time, status) ~ sex, data = d)
  fit_ecog <- survival::coxph(survival::Surv(time, status) ~ ecog, data = d)
  set.seed(9)
  man_sex <- spicy:::.coxph_estimand_rows(
    fit_sex,
    "M1",
    "Surv(time, status)",
    c("rmst", "rmst_se", "rmst_ci"),
    tau = 500,
    boot_n = 25
  )
  man_ecog <- spicy:::.coxph_estimand_rows(
    fit_ecog,
    "M1",
    "Surv(time, status)",
    c("rmst", "rmst_se", "rmst_ci"),
    tau = 500,
    boot_n = 25
  )
  expect_equal(
    uv$std.error[uv$term == "sexfemale"],
    man_sex$rows$std_error[man_sex$rows$term == "sexfemale"],
    tolerance = 1e-12
  )
  expect_equal(
    uv$std.error[uv$term == "ecoge1"],
    man_ecog$rows$std_error[man_ecog$rows$term == "ecoge1"],
    tolerance = 1e-12
  )
  # The two blocks come from different bootstraps.
  expect_false(isTRUE(all.equal(
    uv$std.error[uv$term == "sexfemale"],
    uv$std.error[uv$term == "ecoge1"]
  )))
  # The rendered CI bounds follow est ± z·SE of the per-block SE.
  z <- stats::qnorm(0.975)
  r <- uv[uv$term == "sexfemale", ]
  expect_equal(r$conf.low, r$estimate - z * r$std.error, tolerance = 1e-10)
  expect_equal(r$conf.high, r$estimate + z * r$std.error, tolerance = 1e-10)
  # Same seed, same screen: reproducible inference.
  set.seed(9)
  scr2 <- table_regression_uv(
    d,
    outcome = survival::Surv(time, status),
    predictors = c(sex, ecog),
    method = "coxph",
    show_columns = c("b", "rmst", "rmst_se", "rmst_ci"),
    tau = 500,
    boot_n = 25,
    multivariable = FALSE,
    output = "long"
  )
  expect_identical(scr$std.error, scr2$std.error)
})
