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
