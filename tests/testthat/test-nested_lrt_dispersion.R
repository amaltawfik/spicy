# Nested (hierarchical) comparisons for glm families with an ESTIMATED
# dispersion (Gamma, gaussian, inverse.gaussian, Tweedie, quasi*): the
# change test is the F test anova(test = "F") reports, built on the scaled
# deviance difference, Δdev / φ̂, and not on the raw deviance difference
# printed in anova()'s `Deviance` column (#6). For fixed-dispersion
# families (binomial, poisson) φ = 1 and the test stays the LR chi-square.
# See https://github.com/amaltawfik/spicy/issues/6.

nested_gamma_pair <- function() {
  set.seed(3)
  n <- 180
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  g <- factor(sample(c("a", "b"), n, TRUE))
  mu <- exp(1 + 0.4 * x1 + 0.2 * x2 + 0.3 * (g == "b"))
  d <- data.frame(y = rgamma(n, shape = 3, rate = 3 / mu), x1, x2, g)
  list(
    m1 = glm(y ~ x1, family = Gamma(link = "log"), data = d),
    m2 = glm(y ~ x1 + x2 + g, family = Gamma(link = "log"), data = d)
  )
}

test_that("nested Gamma pair: f_change is the F test of anova(test = 'F')", {
  # Since #6 the statistic was the scaled deviance difference on a
  # chi-square. A pair whose dispersion is estimated now reports the F
  # test anova() reports for it: F = (Delta dev / q) / phi on
  # (q, df.residual of the larger model).
  fits <- nested_gamma_pair()
  out <- spicy:::compute_nested_comparisons(list(fits$m1, fits$m2))
  av <- anova(fits$m1, fits$m2, test = "F")
  phi <- summary(fits$m2)$dispersion

  expect_equal(out$f_change[1], av$F[2], tolerance = 1e-10)
  expect_equal(out$p_change[1], av[["Pr(>F)"]][2], tolerance = 1e-10)
  expect_equal(
    out$f_change[1],
    av$Deviance[2] / av$Df[2] / phi,
    tolerance = 1e-10
  )
  expect_true(is.na(out$lrt_change[1]))
  expect_identical(
    spicy:::default_nested_tokens(list(fits$m1, fits$m2)),
    c("f_change", "p_change")
  )
})

test_that("nested pairs with an estimated dispersion take anova()'s F test", {
  mt <- mtcars
  fams <- list(
    gaussian = list(gaussian(), "mpg"),
    inverse.gaussian = list(inverse.gaussian("log"), "mpg"),
    quasipoisson = list(quasipoisson(), "carb"),
    quasibinomial = list(quasibinomial(), "vs")
  )
  if (requireNamespace("statmod", quietly = TRUE)) {
    fams$tweedie <- list(
      statmod::tweedie(var.power = 1.5, link.power = 0),
      "carb"
    )
  }
  for (nm in names(fams)) {
    f <- function(rhs) stats::as.formula(paste(fams[[nm]][[2L]], "~", rhs))
    g0 <- glm(f("wt"), data = mt, family = fams[[nm]][[1L]])
    g1 <- glm(f("wt + hp"), data = mt, family = fams[[nm]][[1L]])
    av <- anova(g0, g1, test = "F")
    out <- as.data.frame(table_regression(list(g0, g1), nested = TRUE))
    got <- function(v) out[["Model 2: B"]][out$variable == v]
    expect_equal(got("f_change"), av$F[2L], tolerance = 1e-10, label = nm)
    expect_equal(got("p_change"), av[["Pr(>F)"]][2L], tolerance = 1e-10)
    expect_false("lrt_change" %in% out$variable, label = nm)
  }
  # lrt_change is refused for such a hierarchy, pointing at f_change.
  g0 <- glm(mpg ~ wt, data = mt, family = Gamma("log"))
  g1 <- glm(mpg ~ wt + hp, data = mt, family = Gamma("log"))
  expect_error(
    table_regression(
      list(g0, g1),
      nested = TRUE,
      show_fit_stats = c("lrt_change", "p_change")
    ),
    "f_change",
    class = "spicy_invalid_input"
  )
})

test_that("an HC1 nested Gamma pair takes the Wald F of lmtest::waldtest()", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  mt <- mtcars
  g0 <- glm(mpg ~ wt, data = mt, family = Gamma("log"))
  g1 <- glm(mpg ~ wt + hp + qsec, data = mt, family = Gamma("log"))
  wt <- lmtest::waldtest(
    g0,
    g1,
    vcov = sandwich::vcovHC(g1, type = "HC1"),
    test = "F"
  )
  out <- as.data.frame(table_regression(
    list(g0, g1),
    nested = TRUE,
    vcov = "HC1"
  ))
  got <- function(v) out[["Model 2: B"]][out$variable == v]
  expect_equal(got("wald_f_change"), wt$F[2L], tolerance = 1e-10)
  expect_equal(got("p_change"), wt[["Pr(>F)"]][2L], tolerance = 1e-10)
  expect_false("f_change" %in% out$variable)
})

test_that("nested binomial pair (fixed dispersion) is unaffected", {
  set.seed(4)
  n <- 300
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  p <- plogis(-0.3 + 0.8 * x1 + 0.5 * x2)
  d <- data.frame(y = rbinom(n, 1, p), x1, x2)
  m1 <- glm(y ~ x1, family = binomial, data = d)
  m2 <- glm(y ~ x1 + x2, family = binomial, data = d)
  out <- spicy:::compute_nested_comparisons(list(m1, m2))
  av <- anova(m1, m2, test = "LRT")

  expect_equal(out$lrt_change[1], av$Deviance[2], tolerance = 1e-6)
  expect_equal(out$p_change[1], av[["Pr(>Chi)"]][2], tolerance = 1e-8)
  expect_equal(
    stats::pchisq(out$lrt_change[1], df = av$Df[2], lower.tail = FALSE),
    out$p_change[1],
    tolerance = 1e-8
  )
})
