# The dependent variable in the title and the Outcome row drops the
# namespace prefixes of the formula, at any depth of the expression
# (`.fit_dv()`): `survival::Surv(time, status)` reads `Surv(time,
# status)`. Nothing else in the expression changes.

test_that("a qualified call loses its prefix, at any depth", {
  dv <- function(f) .fit_dv(NULL, f)
  expect_identical(
    dv(survival::Surv(time, status) ~ x),
    "Surv(time, status)"
  )
  expect_identical(dv(base::log(y) ~ x), "log(y)")
  expect_identical(dv(log(base::abs(y) + 1) ~ x), "log(abs(y) + 1)")
  expect_identical(dv(base:::log(y) ~ x), "log(y)")
  expect_identical(dv(I(food / income) ~ x), "I(food/income)")
  expect_identical(dv(log(wage) ~ x), "log(wage)")
  expect_identical(dv(cbind(s, f) ~ x), "cbind(s, f)")
})

test_that("the formula of a brmsfit is read inside its brmsformula", {
  # formula() of a brmsfit is a list of class brmsformula that holds the
  # formula: the dependent variable is read from that formula.
  bf <- function(f) structure(list(formula = f), class = "brmsformula")
  expect_identical(.fit_dv(list(), bf(y ~ x)), "y")
  expect_identical(.fit_dv(list(), bf(base::log(y) ~ x + z)), "log(y)")
})

test_that("coxph and lm are titled without the prefix", {
  skip_if_not_installed("survival")
  d <- survival::lung
  cx <- survival::coxph(survival::Surv(time, status) ~ age, data = d)
  fr <- as_regression_frame(cx, model_id = "M1")
  expect_identical(fr$info$dv, "Surv(time, status)")
  out <- capture.output(print(table_regression(cx)))
  expect_match(out[1L], ": Surv(time, status)", fixed = TRUE)
  expect_false(any(grepl("survival::", out, fixed = TRUE)))

  m <- stats::lm(base::log(mpg) ~ wt, data = mtcars)
  out <- capture.output(print(table_regression(m)))
  expect_match(out[1L], ": log(mpg)", fixed = TRUE)
  expect_false(any(grepl("base::", out, fixed = TRUE)))
  expect_identical(
    as_regression_frame(stats::lm(I(mpg / wt) ~ hp, data = mtcars))$info$dv,
    "I(mpg/wt)"
  )
})

test_that("svycoxph is titled without the prefix", {
  skip_if_not_installed("survey")
  skip_if_not_installed("survival")
  d <- survival::lung
  d$w <- 1 + (d$sex == 2)
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  sc <- survey::svycoxph(survival::Surv(time, status > 1) ~ age, design = des)
  expect_identical(
    as_regression_frame(sc, model_id = "M1")$info$dv,
    "Surv(time, status > 1)"
  )
  out <- capture.output(print(table_regression(sc)))
  expect_match(out[1L], ": Surv(time, status > 1)", fixed = TRUE)
  expect_false(any(grepl("survival::", out, fixed = TRUE)))
})

test_that("nested = TRUE reads the response of a fixest IV fit, not its formula", {
  skip_if_not_installed("fixest")
  set.seed(2)
  n <- 200
  d <- data.frame(z = rnorm(n), x = rnorm(n))
  d$xen <- d$z + rnorm(n)
  d$yiv <- d$xen + d$x + rnorm(n)
  d$y2 <- d$yiv + rnorm(n)
  m1 <- fixest::feols(yiv ~ 1 | xen ~ z, data = d)
  m2 <- fixest::feols(yiv ~ x | xen ~ z, data = d)
  expect_no_error(validate_nested_alignment(list(m1, m2), TRUE))
  m3 <- fixest::feols(y2 ~ x | xen ~ z, data = d)
  expect_error(
    validate_nested_alignment(list(m1, m3), TRUE),
    "Models have different response variables: yiv, y2.",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
})
