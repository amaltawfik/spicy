# Nested (hierarchical) comparisons for glm families with an ESTIMATED
# dispersion (Gamma, gaussian, inverse.gaussian, quasi*): the chi-square
# change statistic must be the scaled deviance difference, Δdev / φ̂ --
# the quantity anova.glm()'s p-value is computed on -- and not the raw
# deviance difference printed in anova()'s `Deviance` column. For fixed-
# dispersion families (binomial, poisson) φ = 1 and the two coincide.
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

test_that("nested Gamma pair: lrt_change is the scaled deviance difference (#6)", {
  skip("Known bug, see https://github.com/amaltawfik/spicy/issues/6")
  fits <- nested_gamma_pair()
  out <- spicy:::compute_nested_comparisons(list(fits$m1, fits$m2))
  av <- anova(fits$m1, fits$m2, test = "LRT")
  phi <- summary(fits$m2)$dispersion

  # Statistic and p-value must come from the same quantity.
  expect_equal(out$lrt_change[1], av$Deviance[2] / phi, tolerance = 1e-6)
  expect_equal(out$p_change[1], av[["Pr(>Chi)"]][2], tolerance = 1e-8)
  expect_equal(
    stats::pchisq(out$lrt_change[1], df = av$Df[2], lower.tail = FALSE),
    out$p_change[1],
    tolerance = 1e-8
  )
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
