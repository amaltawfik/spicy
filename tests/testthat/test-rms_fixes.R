# Corrections to the rms family (ols, lrm, Glm, cph) found on 2026-11-19
# while preparing the article on rms fits.

rmsfx_data <- function(seed = 1, n = 300) {
  set.seed(seed)
  d <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    w = runif(n, 0.5, 3),
    g = factor(sample(15, n, TRUE)),
    sex = factor(sample(c("f", "m"), n, TRUE))
  )
  d$y <- 1 + 0.5 * d$x1 - 0.3 * d$x2^2 + rnorm(n)
  d$yb <- rbinom(n, 1, plogis(0.4 * d$x1 + 0.3 * log(d$w)))
  d$time <- rexp(n, exp(0.3 * d$x1 + 0.4 * log(d$w)))
  d$status <- rbinom(n, 1, 0.8)
  d
}

rmsfx_se <- function(tbl, term) {
  as.numeric(tbl$SE[tbl$variable == term & tbl$.row_role == "coef"])
}


# ---- F1: a robcov() or bootcov() fit names its own variance -----------------

test_that("a fit passed through robcov() or bootcov() names that variance in the note", {
  skip_if_not_installed("rms")
  d <- rmsfx_data()
  m <- rms::lrm(yb ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  g <- d$g
  set.seed(3)
  b <- rms::bootcov(m, B = 20)
  set.seed(3)
  bc <- rms::bootcov(m, cluster = g, B = 20)
  out <- capture.output(print(table_regression(
    list(
      m = m,
      r = rms::robcov(m),
      rc = rms::robcov(m, cluster = g),
      b = b,
      bc = bc
    ),
    show_columns = "se"
  )))
  note <- paste(out, collapse = " ")
  expect_match(note, "m: Wald asymptotic (z)", fixed = TRUE)
  expect_match(note, "r: robust (Huber)", fixed = TRUE)
  expect_match(note, "rc: cluster-robust (Huber), clusters by g", fixed = TRUE)
  expect_match(note, "b: nonparametric bootstrap (20 replicates)", fixed = TRUE)
  expect_match(
    note,
    "bc: cluster bootstrap (20 replicates), clusters by g",
    fixed = TRUE
  )

  o <- rms::ols(y ~ x1, data = d, x = TRUE, y = TRUE)
  out <- capture.output(print(table_regression(rms::robcov(o))))
  expect_match(
    paste(out, collapse = " "),
    "Std. errors: Robust (Huber).",
    fixed = TRUE
  )
  ph <- rms::cph(
    survival::Surv(time, status) ~ x1,
    data = d,
    x = TRUE,
    y = TRUE
  )
  out <- capture.output(print(table_regression(rms::robcov(ph, cluster = g))))
  expect_match(
    paste(out, collapse = " "),
    "cluster-robust (Lin-Wei), clusters by g",
    fixed = TRUE
  )
})

test_that("the bootstrap variance of an lrm marks its nested test as a Wald test", {
  skip_if_not_installed("rms")
  d <- rmsfx_data()
  m <- rms::lrm(yb ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  set.seed(3)
  b <- rms::bootcov(m, B = 20)
  expect_true(nested_own_robust_vcov(b))
})
