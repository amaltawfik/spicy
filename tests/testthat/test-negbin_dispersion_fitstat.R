# Opt-in dispersion fit-stat rows for MASS::glm.nb: theta (NB2,
# V = mu + mu^2/theta) and alpha = 1/theta (the Stata nbreg
# convention). Oracle: fit$theta itself.

.nb_fit <- function() {
  data(bioChemists, package = "pscl")
  MASS::glm.nb(art ~ fem + mar + kid5 + ment, data = bioChemists)
}


test_that("theta and alpha render with the fit's own values", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("pscl")
  fit <- .nb_fit()
  out <- paste(capture.output(print(table_regression(
    fit, show_fit_stats = c("nobs", "theta", "alpha", "AIC")
  ))), collapse = "\n")
  expect_match(out, "θ (dispersion)", fixed = TRUE)
  expect_match(out, "α (= 1/θ)", fixed = TRUE)
  expect_match(out, sprintf("%.2f", fit$theta), fixed = TRUE)
  expect_match(out, sprintf("%.2f", 1 / fit$theta), fixed = TRUE)

  fr <- as_regression_frame(fit)
  expect_equal(fr$info$fit_stats$theta, unname(fit$theta),
               tolerance = 1e-12)
  expect_equal(fr$info$fit_stats$alpha, 1 / unname(fit$theta),
               tolerance = 1e-12)
})


test_that("dispersion tokens stay out of the default negbin block", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("pscl")
  out <- paste(capture.output(print(table_regression(.nb_fit()))),
               collapse = "\n")
  expect_false(grepl("dispersion", out, fixed = TRUE))
})


test_that("theta / alpha are refused for non-negbin fits", {
  fit_lm <- stats::lm(mpg ~ wt, data = mtcars)
  expect_error(
    table_regression(fit_lm, show_fit_stats = c("nobs", "theta")),
    class = "spicy_invalid_input"
  )
  fit_glm <- stats::glm(am ~ wt, data = mtcars, family = binomial)
  expect_error(
    table_regression(fit_glm, show_fit_stats = c("nobs", "alpha")),
    class = "spicy_invalid_input"
  )
  # Mixed negbin + glm list: refused too (tokens are all-negbin only).
  skip_if_not_installed("MASS")
  skip_if_not_installed("pscl")
  expect_error(
    table_regression(list(.nb_fit(), fit_glm),
                     show_fit_stats = c("nobs", "theta")),
    class = "spicy_invalid_input"
  )
})
