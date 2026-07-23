# nnet::multinom pseudo-R^2 fit statistics. The null model is
# intercept-only, whose log-likelihood is the closed-form multinomial
# log-likelihood of the marginal category frequencies (shared with the
# ordinal path). Oracle: an actual intercept-only refit.

.mn_soc <- function() {
  d <- sochealth
  d$education <- factor(
    as.character(d$education),
    levels = c("Lower secondary", "Upper secondary", "Tertiary")
  )
  d
}


test_that("McFadden and Nagelkerke match an intercept-only refit", {
  skip_if_not_installed("nnet")
  d <- .mn_soc()
  fit <- nnet::multinom(
    employment_status ~ age + sex + education,
    data = d,
    trace = FALSE
  )
  cc <- stats::complete.cases(
    d[, c("employment_status", "age", "sex", "education")]
  )
  fit0 <- nnet::multinom(employment_status ~ 1, data = d[cc, ], trace = FALSE)
  ll1 <- as.numeric(stats::logLik(fit))
  ll0 <- as.numeric(stats::logLik(fit0))
  n <- sum(cc)

  fr <- as_regression_frame(fit)
  expect_equal(
    fr$info$fit_stats$pseudo_r2_mcfadden,
    1 - ll1 / ll0,
    tolerance = 1e-8
  )
  cox_snell <- 1 - exp((ll0 - ll1) * 2 / n)
  upper <- 1 - exp(ll0 * 2 / n)
  expect_equal(
    fr$info$fit_stats$pseudo_r2_nagelkerke,
    cox_snell / upper,
    tolerance = 1e-8
  )
  # The closed-form null log-likelihood is the marginal-frequency one.
  expect_equal(spicy:::.ordinal_null_loglik(fit), ll0, tolerance = 1e-8)
})


test_that("the pseudo-R2 rows are class-aware defaults, not silent drops", {
  skip_if_not_installed("nnet")
  d <- .mn_soc()
  fit <- nnet::multinom(employment_status ~ age, data = d, trace = FALSE)
  out <- paste(capture.output(print(table_regression(fit))), collapse = "\n")
  expect_match(out, "R² (McFadden)", fixed = TRUE)
  expect_match(out, "R² (Nagelkerke)", fixed = TRUE)
  # Explicitly requested, the tokens render (they were silently dropped).
  out2 <- paste(
    capture.output(print(
      table_regression(fit, show_fit_stats = c("nobs", "pseudo_r2_mcfadden"))
    )),
    collapse = "\n"
  )
  expect_match(out2, "R² (McFadden)", fixed = TRUE)
  expect_false(grepl("Nagelkerke", out2, fixed = TRUE))
})


test_that("a weighted multinom fit keeps the closed form exact", {
  skip_if_not_installed("nnet")
  d <- .mn_soc()
  cc <- stats::complete.cases(d[, c("employment_status", "age")])
  d2 <- d[cc, ]
  set.seed(4)
  d2$w <- sample(1:3, nrow(d2), replace = TRUE)
  fit <- nnet::multinom(
    employment_status ~ age,
    data = d2,
    weights = w,
    trace = FALSE
  )
  fit0 <- nnet::multinom(
    employment_status ~ 1,
    data = d2,
    weights = w,
    trace = FALSE
  )
  expect_equal(
    spicy:::.ordinal_null_loglik(fit),
    as.numeric(stats::logLik(fit0)),
    tolerance = 1e-6
  )
})
