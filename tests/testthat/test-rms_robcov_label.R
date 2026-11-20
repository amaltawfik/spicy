# The label of the variance an rms fit carries from rms::robcov(). For an
# ols, robcov() also has method = "efron" (a leverage adjustment close to
# HC3), and the fit does not keep the method: the label names the
# sandwich without its author. The other classes have Huber's only.

robcov_data <- function(seed = 2, n = 200) {
  set.seed(seed)
  d <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    g = factor(sample(20, n, TRUE))
  )
  d$y <- 1 + 0.5 * d$x1 - 0.3 * d$x2 + rnorm(n)
  d$yb <- rbinom(n, 1, plogis(0.4 * d$x1))
  d
}

test_that("an ols passed through robcov() says sandwich, whatever the method", {
  skip_if_not_installed("rms")
  d <- robcov_data()
  o <- rms::ols(y ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  # The method leaves no trace in the fit.
  expect_identical(
    setdiff(names(rms::robcov(o, method = "efron")), names(o)),
    "orig.var"
  )
  for (m in c("huber", "efron")) {
    expect_identical(
      as_regression_frame(rms::robcov(o, method = m))$info$vcov_label,
      "robust (sandwich)"
    )
  }
  expect_identical(
    as_regression_frame(rms::robcov(o, cluster = d$g))$info$vcov_label,
    "cluster-robust (sandwich), clusters by d$g"
  )
  withr::local_options(spicy.language = "fr")
  expect_identical(
    as_regression_frame(rms::robcov(o))$info$vcov_label,
    "robustes (sandwich)"
  )
})

test_that("an lrm passed through robcov() keeps Huber's name", {
  skip_if_not_installed("rms")
  d <- robcov_data()
  f <- rms::lrm(yb ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  expect_identical(
    as_regression_frame(rms::robcov(f))$info$vcov_label,
    "robust (Huber)"
  )
  # spicy's own CR0 of an ols is robcov()'s Huber method.
  o <- rms::ols(y ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  expect_identical(
    as_regression_frame(
      o,
      vcov = "CR0",
      cluster = d$g,
      cluster_name = "g"
    )$info$vcov_label,
    "cluster-robust (Huber), clusters by g"
  )
})

test_that("the refusal of CR1 for an rms fit says where its CR0 comes from", {
  skip_if_not_installed("rms")
  d <- robcov_data()
  f <- rms::lrm(yb ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  err <- expect_error(
    table_regression(f, vcov = "CR1", cluster = d$g),
    class = "spicy_unsupported_vcov"
  )
  expect_match(
    conditionMessage(err),
    paste0(
      "spicy computes the cluster-robust variance of an rms fit with ",
      "`rms::robcov()`, whose Huber estimator is CR0."
    ),
    fixed = TRUE
  )
})
