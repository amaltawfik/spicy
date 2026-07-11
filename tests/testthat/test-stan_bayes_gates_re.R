# Bayesian gates + multilevel random-effects block (findings a, b, c
# of the Bayesian-vignette reconnaissance). Fits are tiny and seeded;
# skipped where rstanarm is not installed (CI convention of
# test-regression_frame_stan.R).

.tiny_stan_glmer <- function() {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  sh <- sochealth
  suppressWarnings(rstanarm::stan_glmer(
    smoking ~ age + (1 | region), data = sh, family = binomial(),
    iter = 500, chains = 1, refresh = 0, seed = 42
  ))
}


test_that("p_adjust is refused for all-Bayesian tables", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 400, chains = 1, refresh = 0, seed = 1
  ))
  expect_error(
    table_regression(fit, p_adjust = "holm"),
    class = "spicy_invalid_input"
  )
  # Likelihood-based fit stats are refused too (finding b) ...
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "AIC")),
    class = "spicy_invalid_input"
  )
  # ... and the class-aware DEFAULT no longer injects them: the
  # default table renders with n only.
  out <- paste(capture.output(print(table_regression(fit))),
               collapse = "\n")
  expect_false(grepl("AIC", out, fixed = TRUE))
  expect_match(out, "Posterior covariance", fixed = TRUE)
  # Mixed-class tables keep the frequentist columns adjustable/filled.
  gf <- glm(am ~ wt, data = mtcars, family = binomial)
  out2 <- paste(capture.output(print(table_regression(
    list(G = gf, B = fit), show_columns = c("b", "ci")
  ))), collapse = "\n")
  expect_match(out2, "AIC", fixed = TRUE)
})


test_that("stan_glmer renders an RE block, not per-group b[] rows", {
  fit <- .tiny_stan_glmer()
  out <- paste(capture.output(print(table_regression(fit))),
               collapse = "\n")
  expect_false(grepl("b[(Intercept)", out, fixed = TRUE))
  expect_match(out, "Random effects:", fixed = TRUE)
  expect_match(out, "region (Intercept)", fixed = TRUE)
  expect_match(out, "Random effects (MCMC).", fixed = TRUE)
  # No chi-bar-squared LRT line: there is no likelihood-ratio test
  # for a posterior.
  expect_false(grepl("LR test", out, fixed = TRUE))

  # Oracle: the sigma row's estimate and CrI are the posterior median
  # and quantiles of sqrt(Sigma) from the draws themselves.
  dr <- posterior::as_draws_matrix(fit)
  v <- dr[, "Sigma[region:(Intercept),(Intercept)]"]
  sd_draws <- sqrt(pmax(v, 0))
  td <- broom::tidy(table_regression(fit))
  row <- td[grepl("^re::region", td$term), ]
  expect_equal(row$estimate, median(sd_draws), tolerance = 1e-10)
  expect_equal(row$conf.low,
               unname(quantile(sd_draws, 0.025)), tolerance = 1e-10)
  expect_equal(row$conf.high,
               unname(quantile(sd_draws, 0.975)), tolerance = 1e-10)

  # n_groups feeds the N (region) fit-stat machinery.
  fr <- as_regression_frame(fit)
  expect_identical(fr$info$n_groups$region, 6L)
})


test_that("the pd token renders the probability of direction (finding d)", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 500, chains = 1, refresh = 0, seed = 1
  ))
  out <- paste(capture.output(print(table_regression(
    fit, show_columns = c("b", "ci", "pd")
  ))), collapse = "\n")
  expect_match(out, "pd", fixed = TRUE)
  dr <- posterior::as_draws_matrix(fit)[, "wt"]
  oracle <- max(mean(dr > 0), mean(dr < 0))
  fr <- as_regression_frame(fit)
  expect_equal(fr$coefs$pd[fr$coefs$term == "wt"], oracle,
               tolerance = 1e-10)
  # Frequentist fits refuse the token.
  expect_error(
    table_regression(stats::lm(mpg ~ wt, mtcars),
                     show_columns = c("b", "pd")),
    class = "spicy_invalid_input"
  )
})
