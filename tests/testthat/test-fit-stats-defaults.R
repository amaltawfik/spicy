# Class-aware show_fit_stats default resolution, including the universal
# nobs + AIC safety net for classes not matched by a tailored branch.

fs_labels <- function(fit, ...) {
  df <- table_regression(fit, output = "data.frame", ...)
  trimws(df$Variable)
}

test_that("lm default fit-stats = nobs + R2 + adj_R2 (no AIC)", {
  v <- fs_labels(lm(mpg ~ wt + hp, data = mtcars))
  expect_true(any(v == "n"))
  expect_true(any(grepl("Adj", v)))          # Adj.R2
  expect_false(any(v == "AIC"))
})

test_that("glm default fit-stats = nobs + McFadden + Nagelkerke + AIC", {
  set.seed(1)
  d <- data.frame(x = rnorm(120), y = rbinom(120, 1, 0.5))
  v <- fs_labels(glm(y ~ x, data = d, family = binomial))
  expect_true(any(v == "n"))
  expect_true(any(grepl("McFadden", v)))
  expect_true(any(grepl("Nagelkerke", v)))
  expect_true(any(v == "AIC"))
})

# ---- universal fallback (nobs + AIC) for otherwise-uncovered classes ------

test_that("betareg falls back to nobs + AIC (was a blank block)", {
  skip_if_not_installed("betareg")
  set.seed(1); n <- 150
  d <- data.frame(x = rnorm(n))
  d$yp <- pmin(pmax(plogis(0.5 * d$x + rnorm(n)), 1e-3), 1 - 1e-3)
  v <- fs_labels(betareg::betareg(yp ~ x, data = d))
  expect_true(any(v == "n"))
  expect_true(any(v == "AIC"))
})

test_that("survreg + coxph fall back to nobs + AIC", {
  skip_if_not_installed("survival")
  set.seed(1); n <- 150
  d <- data.frame(x = rnorm(n),
                  time = rexp(n), status = rbinom(n, 1, 0.7))
  for (fit in list(
    survival::survreg(survival::Surv(time, status) ~ x, data = d),
    survival::coxph(survival::Surv(time, status) ~ x, data = d))) {
    v <- fs_labels(fit)
    expect_true(any(v == "n"))
    expect_true(any(v == "AIC"))
  }
})

test_that("multinom falls back to nobs + AIC", {
  skip_if_not_installed("nnet")
  set.seed(1); n <- 180
  d <- data.frame(x = rnorm(n), g = factor(sample(letters[1:3], n, TRUE)))
  v <- fs_labels(nnet::multinom(g ~ x, data = d, trace = FALSE))
  expect_true(any(v == "n"))
  expect_true(any(v == "AIC"))
})

test_that("an explicit show_fit_stats still overrides the fallback", {
  skip_if_not_installed("survival")
  set.seed(1); n <- 120
  d <- data.frame(x = rnorm(n), time = rexp(n), status = rbinom(n, 1, 0.7))
  fit <- survival::coxph(survival::Surv(time, status) ~ x, data = d)
  v <- fs_labels(fit, show_fit_stats = "nobs")
  expect_true(any(v == "n"))
  expect_false(any(v == "AIC"))          # AIC not requested -> absent
  # and FALSE suppresses the whole block
  v0 <- fs_labels(fit, show_fit_stats = FALSE)
  expect_false(any(v0 == "AIC"))
  expect_false(any(v0 == "n"))
})
