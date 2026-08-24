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
  fit <- MASS::polr(
    Sat ~ Infl,
    data = MASS::housing,
    weights = Freq,
    Hess = TRUE
  )
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
  fit <- MASS::polr(
    Sat ~ Infl,
    data = MASS::housing,
    weights = Freq,
    Hess = TRUE
  )
  msg <- tryCatch(
    compute_model_vcov(fit, type = "HC3"),
    error = function(e) conditionMessage(e)
  )
  expect_match(msg, "sandwich::vcovHC()", fixed = TRUE)
  expect_match(
    msg,
    "classical standard errors under a robust label",
    fixed = TRUE
  )
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


# ---- the third engine site, and an invented cluster --------------------

# compute_model_vcov() reaches three cluster-robust backends, and until
# now they disagreed about a missing cluster id. sandwich::vcovCL and
# clubSandwich::vcovCR refuse it (their error arrived bare, unclassed);
# spicy's own Lin-Wei path for coxph sums dfbeta residuals with rowsum(),
# which makes NA its OWN GROUP and returns a variance -- silently, and
# the table rendered it under a "cluster-robust (Lin-Wei)" footer.
test_that("sandwich::vcovCL failures are classed, not bare", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl", envir = environment())
  fit <- pscl::zeroinfl(art ~ fem + mar | 1, data = bioChemists)
  cl <- bioChemists$fem
  # The good case still computes.
  expect_true(is.matrix(compute_model_vcov(fit, "CR0", cluster = cl)))
  cl_na <- cl
  cl_na[1:5] <- NA
  # Sanity: the engine itself refuses, and used to do so unclassed.
  expect_error(suppressWarnings(sandwich::vcovCL(fit, cluster = cl_na)))
  err <- tryCatch(
    compute_model_vcov(fit, "CR0", cluster = cl_na),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_error")
  expect_s3_class(err, "spicy_invalid_input")
})

test_that("coxph no longer invents a cluster out of the missing ids", {
  skip_if_not_installed("survival")
  d <- survival::lung
  d <- d[!is.na(d$ph.ecog) & !is.na(d$wt.loss), ]
  fit <- survival::coxph(
    survival::Surv(time, status) ~ age + ph.ecog,
    data = d
  )
  intact <- compute_model_vcov(fit, "CR0", cluster = d$sex)
  expect_equal(sqrt(intact[1L, 1L]), 0.008065996985, tolerance = 1e-9)

  cl_na <- d$sex
  cl_na[1:5] <- NA
  err <- tryCatch(
    compute_model_vcov(fit, "CR0", cluster = cl_na),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "5 missing value(s)", fixed = TRUE)
  expect_false(is.matrix(err))
})

test_that("the public path refuses rather than rendering an invented cluster", {
  skip_if_not_installed("survival")
  d <- survival::lung
  d <- d[!is.na(d$ph.ecog) & !is.na(d$wt.loss), ]
  fit <- survival::coxph(
    survival::Surv(time, status) ~ age + ph.ecog,
    data = d
  )
  cl_na <- d$sex
  cl_na[1:5] <- NA
  expect_error(
    table_regression(fit, vcov = "CR0", cluster = cl_na),
    class = "spicy_invalid_input"
  )
  # And the intact-cluster table still renders with its robust footer.
  out <- paste(
    capture.output(print(
      table_regression(fit, vcov = "CR0", cluster = d$sex)
    )),
    collapse = "\n"
  )
  expect_match(out, "cluster-robust", fixed = TRUE)
})

test_that(".check_cluster_no_na passes everything that is usable", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  expect_null(.check_cluster_no_na(fit, NULL))
  expect_null(.check_cluster_no_na(fit, mtcars$cyl))
  expect_null(.check_cluster_no_na(fit, list(1, 2))) # not atomic
  expect_error(
    .check_cluster_no_na(fit, c(1, NA, 3)),
    class = "spicy_invalid_input"
  )
})
