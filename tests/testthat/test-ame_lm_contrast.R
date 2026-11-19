# The AME of an lm under a cluster-robust matrix, for any formula: the
# contrast c of each AME is the sample average of the derivative of the
# design row (or of its change), so the AME is c'b, its SE sqrt(c'Vc) and
# its df those of clubSandwich::linear_contrast(). Also: the note names
# Satterthwaite only for the rows that took those df, the tobit AME is
# named as computed on the latent outcome, and an rlm fit gets its AME.

.ame_lm_data <- function() {
  set.seed(11)
  n <- 120
  d <- data.frame(
    x = rnorm(n, 10, 3),
    z = runif(n, 1, 5),
    g = factor(sample(c("a", "b", "c"), n, TRUE)),
    b = rbinom(n, 1, 0.4),
    k = sample(c(4, 6, 8), n, TRUE),
    cl = rep(1:6, each = 20)
  )
  d$y <- 1 +
    0.5 * d$x -
    0.03 * d$x^2 +
    log(d$z) +
    (d$g == "b") +
    0.3 * d$b +
    rnorm(n)
  d$w <- runif(n, 0.5, 2)
  d
}

.ame_A <- function(fit, d, type = "slope") {
  spicy:::extract_ame_satterthwaite(
    fit,
    "CR2",
    d$cl,
    0.95,
    "M1",
    "y",
    ame_type = type
  )
}

test_that("an rlm fit gets its AME rows, on the fit's own variance", {
  skip_if_not_installed("marginaleffects")
  d <- .ame_lm_data()
  fit <- MASS::rlm(y ~ x + I(x^2) + g, data = d)
  lo <- as.data.frame(table_regression(
    fit,
    show_columns = c("b", "se", "ame", "ame_se"),
    output = "long"
  ))
  a <- lo[lo$estimate_type == "ame" & !is.na(lo$estimate), ]
  expect_setequal(a$term, c("x", "gb", "gc"))
  me <- as.data.frame(marginaleffects::avg_slopes(fit))
  expect_equal(
    a$estimate[a$term == "x"],
    me$estimate[me$term == "x"],
    tolerance = 1e-8
  )
  expect_equal(
    a$std.error[a$term == "x"],
    me$std.error[me$term == "x"],
    tolerance = 1e-6
  )
  # By hand: c'b and sqrt(c'Vc), c the contrast of x on the design.
  cv <- c(0, 1, 2 * mean(d$x), 0, 0)
  V <- stats::vcov(fit)
  expect_equal(
    a$estimate[a$term == "x"],
    sum(cv * stats::coef(fit)),
    tolerance = 1e-8
  )
  expect_equal(
    a$std.error[a$term == "x"],
    sqrt(drop(t(cv) %*% V %*% cv)),
    tolerance = 1e-6
  )
  expect_true(all(a$test_type == "z"))
})
