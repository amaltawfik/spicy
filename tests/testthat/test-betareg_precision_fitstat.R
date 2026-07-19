# Opt-in precision fit-stat row for betareg::betareg(): phi
# (Ferrari & Cribari-Neto 2004; Var(y) = mu(1-mu)/(1+phi)).
# Oracle: coef(fit, model = "precision") itself.

.br_fit <- function() {
  data("GasolineYield", package = "betareg")
  betareg::betareg(yield ~ batch + temp, data = GasolineYield)
}


test_that("phi renders with the fit's own precision estimate", {
  skip_if_not_installed("betareg")
  fit <- .br_fit()
  out <- paste(capture.output(print(table_regression(
    fit, show_fit_stats = c("nobs", "phi", "aic")
  ))), collapse = "\n")
  expect_match(out, "φ (precision)", fixed = TRUE)
  oracle <- unname(stats::coef(fit, model = "precision"))
  expect_match(out, sprintf("%.2f", oracle), fixed = TRUE)

  fr <- as_regression_frame(fit)
  expect_equal(fr$info$fit_stats$phi, oracle, tolerance = 1e-12)
  # Multi-model all-betareg tables carry the row for every model.
  fit2 <- betareg::betareg(yield ~ temp, data = get("GasolineYield"))
  out2 <- paste(capture.output(print(table_regression(
    list(A = fit, B = fit2), show_fit_stats = c("nobs", "phi")
  ))), collapse = "\n")
  expect_match(out2, "φ (precision)", fixed = TRUE)
  expect_match(out2,
               sprintf("%.2f",
                       unname(stats::coef(fit2, model = "precision"))),
               fixed = TRUE)
})


test_that("phi stays out of the default betareg block", {
  skip_if_not_installed("betareg")
  out <- paste(capture.output(print(table_regression(.br_fit()))),
               collapse = "\n")
  expect_false(grepl("precision", out, fixed = TRUE))
})


test_that("phi is refused for non-betareg fits", {
  fit_lm <- stats::lm(mpg ~ wt, data = mtcars)
  expect_error(
    table_regression(fit_lm, show_fit_stats = c("nobs", "phi")),
    class = "spicy_invalid_input"
  )
  # Mixed betareg + lm list: refused too (all-betareg only).
  skip_if_not_installed("betareg")
  expect_error(
    table_regression(list(.br_fit(), fit_lm),
                     show_fit_stats = c("nobs", "phi")),
    class = "spicy_invalid_input"
  )
})


test_that("phi is back-transformed from the precision link", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg")
  # One-part formula: identity link, coef IS phi.
  fid <- betareg::betareg(yield ~ temp, data = GasolineYield)
  phi_id <- unname(stats::coef(fid, model = "precision"))
  # Intercept-only two-part formula: betareg silently switches the
  # precision link to log -- the same constant-precision model, but
  # coef() returns log(phi). The 2026-07 adversarial review caught the
  # link-scale value rendering under the phi label.
  f1 <- betareg::betareg(yield ~ temp | 1, data = GasolineYield)
  expect_identical(f1$link$precision$name, "log")
  fr1 <- as_regression_frame(f1)
  expect_equal(fr1$info$fit_stats$phi,
               exp(unname(stats::coef(f1, model = "precision"))),
               tolerance = 1e-12)
  # Link-invariance: constant-phi ML is the same number under
  # identity, log and sqrt parameterizations.
  expect_equal(fr1$info$fit_stats$phi, phi_id, tolerance = 1e-6)
  fsq <- betareg::betareg(yield ~ temp, link.phi = "sqrt",
                          data = GasolineYield)
  frs <- as_regression_frame(fsq)
  expect_equal(frs$info$fit_stats$phi, phi_id, tolerance = 1e-6)
  # And the rendered row shows the response-scale phi.
  out <- paste(capture.output(print(table_regression(
    f1, show_fit_stats = c("nobs", "phi")
  ))), collapse = "\n")
  expect_match(out, sprintf("%.2f", phi_id), fixed = TRUE)
})


test_that("phi is refused when the precision has covariates", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg")
  fitv <- betareg::betareg(yield ~ temp | gravity, data = GasolineYield)
  expect_error(
    table_regression(fitv, show_fit_stats = c("nobs", "phi")),
    "covariates on the precision",
    class = "spicy_invalid_input"
  )
  # The same fit renders fine without the token.
  out <- paste(capture.output(print(table_regression(fitv))),
               collapse = "\n")
  expect_match(out, "Beta regression", fixed = TRUE)
})
