# A robust variance that could not be computed must never come back as the
# classical one (register n. 229).
#
# Until 0.13 both robust branches of compute_model_vcov() warned and
# returned stats::vcov(fit). Nothing downstream learned of the
# substitution: `vcov_kind` is set from the REQUESTED type where the frame
# is built and .robust_vcov_label() formats that same requested type, so
# the footer announced "heteroskedasticity-robust (HC3)" over classical
# standard errors. A console warning does not travel with a saved table,
# an exported Word file or a knitted report; the mislabelled numbers do.

test_that("a failed HC* computation aborts instead of returning classical", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("sandwich")
  fit <- MASS::polr(Sat ~ Infl, data = MASS::housing, weights = Freq, Hess = TRUE)
  # Sanity: the engine really does fail on this fit.
  expect_error(sandwich::vcovHC(fit, type = "HC3"))

  expect_error(
    compute_model_vcov(fit, type = "HC3"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    compute_model_vcov(fit, type = "HC3"),
    "could not be computed for this fit"
  )
  # No warning-and-carry-on: nothing is returned at all, so nothing can be
  # labelled. The old contract returned the classical matrix here.
  got <- tryCatch(
    compute_model_vcov(fit, type = "HC3"),
    error = function(e) e
  )
  expect_s3_class(got, "spicy_error")
  expect_false(is.matrix(got))
})

test_that("a failed CR* computation aborts instead of returning classical", {
  skip_if_not_installed("clubSandwich")
  fit <- stats::nls(
    mpg ~ a * exp(b * wt),
    data = mtcars,
    start = list(a = 40, b = -0.2)
  )
  expect_error(
    clubSandwich::vcovCR(fit, type = "CR1", cluster = mtcars$cyl)
  )
  expect_error(
    compute_model_vcov(fit, type = "CR1", cluster = mtcars$cyl),
    class = "spicy_unsupported_vcov"
  )
  got <- tryCatch(
    compute_model_vcov(fit, type = "CR1", cluster = mtcars$cyl),
    error = function(e) e
  )
  expect_false(is.matrix(got))
})

test_that("the refusal names the failing engine and the honest alternative", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("sandwich")
  fit <- MASS::polr(Sat ~ Infl, data = MASS::housing, weights = Freq, Hess = TRUE)
  msg <- tryCatch(
    compute_model_vcov(fit, type = "HC3"),
    error = function(e) conditionMessage(e)
  )
  expect_match(msg, "sandwich::vcovHC()", fixed = TRUE)
  expect_match(msg, "classical standard errors under a robust label", fixed = TRUE)
  expect_match(msg, "vcov = \"classical\"", fixed = TRUE)
  # The word "robust" appears only in the explanation of what is REFUSED.
  expect_false(grepl("Falling back", msg, fixed = TRUE))
})

test_that("the classical estimator still returns the model variance", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  expect_identical(
    compute_model_vcov(fit, type = "classical"),
    stats::vcov(fit)
  )
  # And a robust estimator that DOES work is untouched.
  skip_if_not_installed("sandwich")
  expect_identical(
    compute_model_vcov(fit, type = "HC3"),
    sandwich::vcovHC(fit, type = "HC3")
  )
})
