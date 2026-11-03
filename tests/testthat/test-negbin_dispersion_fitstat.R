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
  out <- paste(
    capture.output(print(table_regression(
      fit,
      show_fit_stats = c("nobs", "theta", "alpha", "aic")
    ))),
    collapse = "\n"
  )
  expect_match(out, "θ (dispersion)", fixed = TRUE)
  expect_match(out, "α (= 1/θ)", fixed = TRUE)
  expect_match(out, sprintf("%.2f", fit$theta), fixed = TRUE)
  expect_match(out, sprintf("%.2f", 1 / fit$theta), fixed = TRUE)

  fr <- as_regression_frame(fit)
  expect_equal(fr$info$fit_stats$theta, unname(fit$theta), tolerance = 1e-12)
  expect_equal(
    fr$info$fit_stats$alpha,
    1 / unname(fit$theta),
    tolerance = 1e-12
  )
})


test_that("dispersion tokens stay out of the default negbin block", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("pscl")
  out <- paste(
    capture.output(print(table_regression(.nb_fit()))),
    collapse = "\n"
  )
  expect_false(grepl("dispersion", out, fixed = TRUE))
})


test_that("theta / alpha are refused when no model in the table is NB2", {
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
  fit_pois <- stats::glm(carb ~ wt, data = mtcars, family = poisson)
  expect_error(
    table_regression(list(fit_pois, fit_pois), show_fit_stats = "theta"),
    "no model in the table",
    class = "spicy_invalid_input"
  )
})


test_that("Poisson beside NB2: theta in the NB2 column, blank for Poisson", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("pscl")
  fit <- .nb_fit()
  fit_pois <- stats::glm(
    art ~ fem + mar + kid5 + ment,
    data = fit$model,
    family = poisson
  )
  tbl <- table_regression(
    list(Poisson = fit_pois, NB = fit),
    show_fit_stats = c("nobs", "theta", "alpha", "sigma")
  )
  df <- as.data.frame(tbl)
  row <- function(tk) df[df$variable == tk & df$.row_role == "fit_stat", ]
  expect_equal(row("theta")[["NB: B"]], unname(fit$theta), tolerance = 1e-12)
  expect_equal(
    row("alpha")[["NB: B"]],
    1 / unname(fit$theta),
    tolerance = 1e-12
  )
  expect_true(is.na(row("theta")[["Poisson: B"]]))
  expect_true(is.na(row("alpha")[["Poisson: B"]]))
  # sigma: the fixed 1 of the Poisson family (summary.glm's dispersion);
  # blank for the negative binomial, whose dispersion is the theta row.
  expect_equal(row("sigma")[["Poisson: B"]], summary(fit_pois)$dispersion)
  expect_true(is.na(row("sigma")[["NB: B"]]))
  out <- paste(utils::capture.output(print(tbl)), collapse = "\n")
  expect_match(out, "θ (dispersion)", fixed = TRUE)
})


test_that("glm.nb: the sigma cell is blank", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("pscl")
  fit <- .nb_fit()
  # The glm path reads 1 from the family's fixed dispersion.
  expect_identical(summary(fit)$dispersion, 1)
  expect_true(is.na(as_regression_frame(fit)$info$fit_stats$sigma))
  # Alone in the table, the sigma row has no value and is dropped.
  out <- paste(
    capture.output(print(table_regression(
      fit,
      show_fit_stats = c("nobs", "sigma")
    ))),
    collapse = "\n"
  )
  expect_false(grepl(fit_stat_label("sigma"), out, fixed = TRUE))
})
