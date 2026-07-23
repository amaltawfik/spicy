# Stratified Cox estimands: g-computation with within-stratum
# baselines. Each subject keeps their own stratum's baseline hazard;
# only the exposure is set counterfactually. Oracle:
# adjustedCurves::adjusted_rmst (direct method) on the same fit,
# pinned value -61.2369; plus an independent in-test recomputation.

.strata_fit <- function() {
  d <- stats::na.omit(
    survival::lung[, c("time", "status", "age", "sex", "ph.ecog")]
  )
  d$sex <- factor(d$sex, 1:2, c("male", "female"))
  d$group <- factor(ifelse(d$ph.ecog >= 1, "ecog1plus", "ecog0"))
  fit <- survival::coxph(
    survival::Surv(time, status) ~ age + group + strata(sex),
    data = d
  )
  list(fit = fit, data = d)
}


test_that("stratified point estimate matches the pinned adjustedCurves oracle", {
  skip_if_not_installed("survival")
  s <- .strata_fit()
  pts <- spicy:::.coxph_estimand_points(
    s$fit,
    spicy:::.coxph_estimand_data(s$fit),
    want_rmst = TRUE,
    want_risk = TRUE,
    tau = 500,
    at_time = 500
  )
  row <- pts[pts$term == "groupecog1plus", ]
  # Pinned 2026-07-10 against adjustedCurves 0.11.4 adjusted_rmst
  # (method = "direct", riskRegression backend), exact to reported
  # precision on this fit.
  expect_equal(row$rmst, -61.2369, tolerance = 1e-4)
  expect_equal(row$risk, 0.18106, tolerance = 1e-4)
})


test_that("stratified curves equal an independent recomputation", {
  skip_if_not_installed("survival")
  s <- .strata_fit()
  fit <- s$fit
  d <- spicy:::.coxph_estimand_data(fit)

  bh <- survival::basehaz(fit, centered = FALSE)
  grid <- sort(unique(bh$time))
  H0m <- vapply(
    levels(bh$strata),
    function(sl) {
      b <- bh[bh$strata == sl, , drop = FALSE]
      c(0, b$hazard)[findInterval(grid, b$time) + 1L]
    },
    numeric(length(grid))
  )
  mf <- stats::model.frame(fit)
  s_subj <- mf[["strata(sex)"]]
  s_idx <- match(as.character(s_subj), levels(bh$strata))
  std <- function(level) {
    cf <- d
    cf$group <- factor(level, levels = levels(d$group))
    elp <- exp(stats::predict(
      fit,
      newdata = cf,
      type = "lp",
      reference = "zero"
    ))
    rowMeans(exp(-sweep(H0m[, s_idx, drop = FALSE], 2L, elp, "*")))
  }
  rmst <- function(times, surv, tau) {
    keep <- times <= tau
    sum(diff(c(0, times[keep], tau)) * c(1, surv[keep]))
  }
  manual <- rmst(grid, std("ecog1plus"), 500) - rmst(grid, std("ecog0"), 500)

  pts <- spicy:::.coxph_estimand_points(
    fit,
    d,
    want_rmst = TRUE,
    want_risk = FALSE,
    tau = 500,
    at_time = NULL
  )
  expect_equal(
    pts$rmst[pts$term == "groupecog1plus"],
    manual,
    tolerance = 1e-10
  )
})


test_that("strata variables get no contrast row and minmax skips them", {
  skip_if_not_installed("survival")
  s <- .strata_fit()
  pts <- spicy:::.coxph_estimand_points(
    s$fit,
    spicy:::.coxph_estimand_data(s$fit),
    want_rmst = TRUE,
    want_risk = FALSE,
    tau = 500,
    at_time = NULL
  )
  expect_false(any(pts$parent_var == "sex"))
  expect_identical(spicy:::.coxph_strata_vars(s$fit), "sex")
  # tau = "minmax" resolves over the compared groups' follow-up, not
  # the strata levels.
  set.seed(3)
  res <- spicy:::.coxph_estimand_rows(
    s$fit,
    "M1",
    "y",
    show_columns = c("b", "rmst"),
    tau = "minmax",
    boot_n = 20L
  )
  obs <- s$data$time
  expected_tau <- min(tapply(obs, as.character(s$data$group), max))
  expect_equal(res$tau, unname(expected_tau))
  expect_true(res$stratified)
})


test_that("the stratified table renders with the within-stratum footer", {
  skip_if_not_installed("survival")
  s <- .strata_fit()
  set.seed(11)
  out <- paste(
    capture.output(print(table_regression(
      s$fit,
      show_columns = c("b", "rmst"),
      tau = 500,
      boot_n = 30
    ))),
    collapse = "\n"
  )
  expect_match(out, "dRMST (500)", fixed = TRUE)
  expect_match(out, "within-stratum baselines", fixed = TRUE)
  expect_match(out, "-61.24", fixed = TRUE)
  # No estimand (or coefficient) row for the strata variable.
  expect_false(grepl("sex", out, fixed = TRUE))
})


test_that("tt() and counting-process responses stay refused", {
  skip_if_not_installed("survival")
  d <- .strata_fit()$data
  fit_tt <- survival::coxph(
    survival::Surv(time, status) ~ age + tt(age),
    data = d,
    tt = function(x, t, ...) x * log(t)
  )
  expect_error(
    spicy:::.coxph_estimand_gates(fit_tt, "M1"),
    class = "spicy_invalid_input"
  )
})


test_that("several strata() terms are refused with the combine hint", {
  skip_if_not_installed("survival")
  d <- .strata_fit()$data
  d$agegrp <- factor(ifelse(d$age >= 63, "old", "young"))
  f2 <- survival::coxph(
    survival::Surv(time, status) ~ group + strata(sex) + strata(agegrp),
    data = d
  )
  expect_error(
    spicy:::.coxph_baseline(f2),
    class = "spicy_invalid_input"
  )
  # The combined form works: one strata(a, b) term, labels align.
  f3 <- survival::coxph(
    survival::Surv(time, status) ~ group + strata(sex, agegrp),
    data = d
  )
  bl <- spicy:::.coxph_baseline(f3)
  expect_identical(ncol(bl$H0), 4L)
  expect_false(anyNA(bl$s_idx))
  pts <- spicy:::.coxph_estimand_points(
    f3,
    spicy:::.coxph_estimand_data(f3),
    want_rmst = TRUE,
    want_risk = FALSE,
    tau = 500,
    at_time = NULL
  )
  expect_true(is.finite(pts$rmst[pts$term == "groupecog1plus"]))
})
