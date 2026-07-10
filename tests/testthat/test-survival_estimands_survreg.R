# Parametric (AFT) estimands: survreg curves are closed-form, so the
# standardized survival is smooth and the RMST integral is numeric on
# a smooth function. Oracles: flexsurv::standsurv (type = "rmst",
# contrast = "difference") on the same Weibull fit, pinned -64.46022;
# and the closed-form exponential RMST at machine precision.

.aft_lung <- function() {
  d <- stats::na.omit(
    survival::lung[, c("time", "status", "age", "sex", "ph.ecog")]
  )
  d$sex <- factor(d$sex, 1:2, c("male", "female"))
  d$ecog <- factor(ifelse(d$ph.ecog >= 1, "ecog1plus", "ecog0"))
  d
}


test_that("Weibull AFT point estimate matches the pinned flexsurv oracle", {
  skip_if_not_installed("survival")
  d <- .aft_lung()
  fit <- survival::survreg(
    survival::Surv(time, status) ~ age + ecog + sex,
    data = d, dist = "weibull"
  )
  pts <- spicy:::.survreg_estimand_points(
    fit, spicy:::.survreg_estimand_data(fit),
    want_rmst = TRUE, want_risk = TRUE, tau = 500, at_time = 500
  )
  # Pinned 2026-07-10 against flexsurv 2.x standsurv(type = "rmst",
  # contrast = "difference") on the same fit.
  expect_equal(pts$rmst[pts$term == "ecogecog1plus"], -64.46022,
               tolerance = 1e-5)

  skip_if_not_installed("flexsurv")
  ff <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age + ecog + sex,
    data = d, dist = "weibull"
  )
  ss <- flexsurv::standsurv(
    ff, type = "rmst",
    at = list(list(ecog = "ecog0"), list(ecog = "ecog1plus")),
    t = 500, contrast = "difference"
  )
  expect_equal(pts$rmst[pts$term == "ecogecog1plus"],
               as.data.frame(ss)$contrast2_1, tolerance = 1e-4)
})


test_that("exponential AFT reproduces the closed-form RMST difference", {
  skip_if_not_installed("survival")
  set.seed(9)
  n <- 1500
  x <- factor(sample(c("a", "b"), n, TRUE))
  tt <- stats::rexp(n, ifelse(x == "b", 0.02, 0.01))
  cens <- stats::runif(n, 0, 200)
  dd <- data.frame(t = pmin(tt, cens), ev = as.integer(tt <= cens),
                   x = x)
  fe <- survival::survreg(survival::Surv(t, ev) ~ x, data = dd,
                          dist = "exponential")
  lam <- function(lev) {
    exp(-stats::predict(fe, newdata = data.frame(x = lev), type = "lp"))
  }
  tau <- 100
  analytic <- (1 - exp(-lam("b") * tau)) / lam("b") -
    (1 - exp(-lam("a") * tau)) / lam("a")
  pts <- spicy:::.survreg_estimand_points(
    fe, spicy:::.survreg_estimand_data(fe),
    want_rmst = TRUE, want_risk = FALSE, tau = tau, at_time = NULL
  )
  expect_equal(pts$rmst[pts$term == "xb"], unname(analytic),
               tolerance = 1e-7)
})


test_that("the survreg table renders TR next to the estimand columns", {
  skip_if_not_installed("survival")
  d <- .aft_lung()
  fit <- survival::survreg(
    survival::Surv(time, status) ~ age + ecog, data = d,
    dist = "weibull"
  )
  set.seed(31)
  out <- paste(capture.output(print(table_regression(
    fit, show_columns = c("b", "rmst"), tau = 500, boot_n = 30,
    exponentiate = TRUE
  ))), collapse = "\n")
  expect_match(out, "dRMST (500)", fixed = TRUE)
  expect_match(out, "g-computation", fixed = TRUE)
  expect_match(out, "TR", fixed = TRUE)
})


test_that("stratified survreg fits are refused", {
  skip_if_not_installed("survival")
  d <- .aft_lung()
  fs <- survival::survreg(
    survival::Surv(time, status) ~ age + strata(sex) + sex, data = d
  )
  expect_error(
    table_regression(fs, show_columns = c("b", "rmst"), tau = 500,
                     boot_n = 10),
    class = "spicy_invalid_input"
  )
})
