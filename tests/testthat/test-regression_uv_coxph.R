# table_regression_uv(method = "coxph"): the Cox univariable screen
# (survival phase lot 1; spec dev/survival_estimands_spec.md,
# validated 2026-07-09). The estimates are the coxph coefficients
# themselves, so the oracle is the direct per-predictor fit (the
# composition contract); the survival single-model path is already
# oracle-validated elsewhere.

.uv_lung <- function() {
  d <- survival::lung
  d$sex <- factor(d$sex, levels = c(1, 2), labels = c("Male", "Female"))
  d
}


test_that("univariable rows reproduce the per-predictor coxph fits", {
  skip_if_not_installed("survival")
  d <- .uv_lung()
  t_uv <- table_regression_uv(d, outcome = Surv(time, status),
                              predictors = c(age, sex, ph.ecog),
                              method = "coxph", exponentiate = TRUE)
  td <- broom::tidy(t_uv)
  expect_setequal(unique(td$model_id), c("Univariable", "Multivariable"))

  for (v in c("age", "ph.ecog")) {
    f <- survival::coxph(
      stats::as.formula(paste("survival::Surv(time, status) ~", v)),
      data = d
    )
    sm <- summary(f)$coefficients
    row <- td$model_id == "Univariable" & td$term == v
    expect_equal(td$estimate[row], exp(unname(stats::coef(f)[v])),
                 tolerance = 1e-12)
    expect_equal(td$p.value[row], unname(sm[v, "Pr(>|z|)"]),
                 tolerance = 1e-12)
  }
  f_multi <- survival::coxph(
    survival::Surv(time, status) ~ age + sex + ph.ecog, data = d
  )
  for (v in c("age", "sexFemale", "ph.ecog")) {
    row <- td$model_id == "Multivariable" & td$term == v
    expect_equal(td$estimate[row],
                 exp(unname(stats::coef(f_multi)[v])), tolerance = 1e-12)
  }
})


test_that("per-fit N uses the subject count, not the coxph event count", {
  skip_if_not_installed("survival")
  d <- .uv_lung()
  out <- paste(capture.output(print(
    table_regression_uv(d, outcome = Surv(time, status),
                        predictors = c(age, ph.ecog), method = "coxph")
  )), collapse = "\n")
  # age: 228 complete subjects; ph.ecog has one NA -> 227. The events
  # (165 / 164) must NOT appear in the N column (stats::nobs(coxph)
  # returns the event count under censoring -- the caught bug).
  expect_match(out, "228")
  expect_match(out, "227")
  expect_match(out, "N varies by predictor (227-228)", fixed = TRUE)
})


test_that("n_events column: Surv status counts per level and totals", {
  skip_if_not_installed("survival")
  d <- .uv_lung()
  out <- paste(capture.output(print(
    table_regression_uv(d, outcome = Surv(time, status),
                        predictors = c(age, sex), method = "coxph",
                        show_columns = c("n", "n_events", "b", "ci", "p"))
  )), collapse = "\n")
  y <- survival::Surv(d$time, d$status)
  ev <- as.integer(y[, 2L])
  for (lv in levels(d$sex)) {
    sel <- d$sex == lv
    expect_match(out, paste0(sum(ev[sel]), "/", sum(sel)), fixed = TRUE)
  }
  expect_match(out, paste0(sum(ev), "/", length(ev)), fixed = TRUE)
})


test_that("start-stop (counting-process) fits refuse the n_events column", {
  skip_if_not_installed("survival")
  d3 <- data.frame(t1 = c(0, 0, 2, 3), t2 = c(2, 3, 5, 8),
                   ev = c(0, 1, 1, 0), x = c(1, 2, 3, 4))
  # The toy 4-row fit needn't converge; only the gate matters here.
  fit <- suppressWarnings(
    survival::coxph(survival::Surv(t1, t2, ev) ~ x, data = d3)
  )
  expect_error(
    table_regression(fit, show_columns = c("b", "n_events")),
    class = "spicy_invalid_input"
  )
})


test_that("complete_cases = TRUE forces the common Cox sample", {
  skip_if_not_installed("survival")
  d <- .uv_lung()
  t_cc <- table_regression_uv(d, outcome = Surv(time, status),
                              predictors = c(age, ph.ecog),
                              method = "coxph", complete_cases = TRUE,
                              multivariable = FALSE)
  out <- paste(capture.output(print(t_cc)), collapse = "\n")
  n_common <- sum(stats::complete.cases(
    d[, c("time", "status", "age", "ph.ecog")]
  ))
  expect_match(out,
               sprintf("All models fit on the %d common complete cases.",
                       n_common),
               fixed = TRUE)
  dcc <- d[stats::complete.cases(d[, c("time", "status", "age",
                                       "ph.ecog")]), ]
  f <- survival::coxph(survival::Surv(time, status) ~ age, data = dcc)
  td <- broom::tidy(t_cc)
  expect_equal(td$estimate[td$term == "age"],
               unname(stats::coef(f)["age"]), tolerance = 1e-12)
})


test_that("titles, HR note, and the survival footer render", {
  skip_if_not_installed("survival")
  d <- .uv_lung()
  out <- paste(capture.output(print(
    table_regression_uv(d, outcome = Surv(time, status),
                        predictors = c(age, sex), method = "coxph",
                        exponentiate = TRUE)
  )), collapse = "\n")
  expect_match(out,
               "Univariable and multivariable Cox regression: Surv(time, status)",
               fixed = TRUE)
  expect_match(out, "HR = hazard ratio", fixed = TRUE)
  # The multivariable fit contributes its event total + concordance.
  expect_match(out, "N events", fixed = TRUE)
  expect_match(out, "Concordance", fixed = TRUE)
  out2 <- paste(capture.output(print(
    table_regression_uv(d, outcome = Surv(time, status),
                        predictors = age, method = "coxph",
                        multivariable = FALSE)
  )), collapse = "\n")
  expect_match(out2, "Univariable Cox regression screen: Surv(time, status)",
               fixed = TRUE)
})


test_that("coxph screen input validation", {
  skip_if_not_installed("survival")
  d <- .uv_lung()
  expect_error(
    table_regression_uv(d, outcome = Surv(time, status), predictors = age,
                        method = "coxph", family = stats::binomial()),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_uv(d, outcome = age, predictors = sex,
                        method = "coxph"),
    class = "spicy_invalid_input"
  )
  # Surv() referencing nothing in `data`.
  time_vec <- d$time; status_vec <- d$status
  expect_error(
    table_regression_uv(d, outcome = Surv(time_vec2, status_vec2),
                        predictors = age, method = "coxph"),
    class = "spicy_invalid_input"
  )
})


test_that("cluster vector aligns through the Cox screen (Lin-Wei)", {
  skip_if_not_installed("survival")
  d <- .uv_lung()
  cl <- rep_len(1:20, nrow(d))
  t_cr <- table_regression_uv(d, outcome = Surv(time, status),
                              predictors = c(age, ph.ecog),
                              method = "coxph",
                              vcov = "CR0", cluster = cl,
                              multivariable = FALSE)
  td <- broom::tidy(t_cr)
  # Oracle: Lin-Wei grouped-dfbeta sandwich on the ph.ecog fit, cluster
  # subset to ITS estimation sample (one NA dropped).
  cc <- stats::complete.cases(d[, c("time", "status", "ph.ecog")])
  f <- survival::coxph(survival::Surv(time, status) ~ ph.ecog, data = d)
  db <- stats::residuals(f, type = "dfbeta")
  rob <- crossprod(rowsum(as.matrix(db), cl[cc]))
  expect_equal(td$std.error[td$term == "ph.ecog"],
               sqrt(rob[1, 1]), tolerance = 1e-9)
})
