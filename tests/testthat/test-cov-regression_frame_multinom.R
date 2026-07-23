# ---------------------------------------------------------------------------
# Coverage top-up for R/regression_frame_multinom.R.
#
# Targets branches the Phase 5b / 7c4 tests don't reach:
#   * Binary multinom -- nnet::multinom() with a 2-level response returns a
#     FLAT named coef vector (not a matrix). Exercises the `else` arm of
#     .multinom_coefs() that promotes the vector to a 1-row matrix and reads
#     SEs from summary()$standard.errors as a bare vector.
#   * Polynomial-contrast factor predictor (ordered factor) -- detect_factor_terms()
#     flags reference_dropped = FALSE, so .multinom_reference_rows() skips it
#     (the `next`) and, with no other dropped factor, returns the empty block.
#   * .check_nnet_available() missing-package abort (mocked spicy_pkg_available).
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_multinom_binary_cov <- function() {
  skip_if_not_installed("nnet")
  d <- iris[iris$Species != "setosa", ]
  d$Species <- droplevels(d$Species)
  nnet::multinom(Species ~ Sepal.Length + Sepal.Width, data = d, trace = FALSE)
}

.fit_multinom_ordered_cov <- function() {
  skip_if_not_installed("nnet")
  d <- iris
  d$sz <- ordered(cut(d$Petal.Width, 3), labels = c("lo", "mid", "hi"))
  nnet::multinom(Species ~ Sepal.Length + sz, data = d, trace = FALSE)
}


# ---- 1. Binary multinom: flat coef vector path ---------------------------

test_that("binary multinom: coef returns a flat vector (not a matrix)", {
  fit <- .fit_multinom_binary_cov()
  # Guards the assumption the `else` arm in .multinom_coefs() depends on.
  expect_false(is.matrix(stats::coef(fit)))
  expect_null(dim(summary(fit)$standard.errors))
})

test_that("binary multinom: frame is schema-valid with one non-ref block", {
  fit <- .fit_multinom_binary_cov()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  non_ref <- fr$coefs[!fr$coefs$is_ref, ]
  # 2-level response -> a single non-reference outcome (virginica),
  # 2 predictors + Intercept = 3 rows.
  expect_identical(nrow(non_ref), 3L)
  expect_identical(unique(non_ref$outcome_level), "virginica")
})

test_that("binary multinom: lone non-ref outcome names every coefs row", {
  fit <- .fit_multinom_binary_cov()
  fr <- as_regression_frame(fit, model_id = "M1")
  # rn <- non_ref when length(non_ref) == 1L: the row is the outcome name,
  # NOT the fallback literal "outcome".
  expect_true(all(fr$coefs$outcome_level == "virginica"))
  expect_true(all(grepl("^virginica: ", fr$coefs$term)))
  expect_identical(fr$info$extras$reference_outcome, "versicolor")
})

test_that("binary multinom: .multinom_coefs() promotes the flat vector directly", {
  fit <- .fit_multinom_binary_cov()
  # Drive the internal builder directly so the flat-vector promotion arm
  # (1-row matrix, outcome name as row, bare-vector SEs) is exercised.
  coefs <- spicy:::.multinom_coefs(fit, ci_level = 0.95)
  expect_identical(unique(coefs$outcome_level), "virginica")
  expect_setequal(
    coefs$term,
    paste0("virginica: ", c("(Intercept)", "Sepal.Length", "Sepal.Width"))
  )
  expect_false(any(coefs$is_ref))
})

test_that("binary multinom: estimates/SE match coef + standard.errors vectors", {
  fit <- .fit_multinom_binary_cov()
  fr <- as_regression_frame(fit, model_id = "M1")
  cf <- stats::coef(fit)
  se <- summary(fit)$standard.errors
  non_ref <- fr$coefs[!fr$coefs$is_ref, ]
  for (term in names(cf)) {
    row <- non_ref[non_ref$term == paste0("virginica: ", term), ]
    expect_equal(row$estimate, unname(cf[term]), tolerance = 1e-10, info = term)
    expect_equal(
      row$std_error,
      unname(se[term]),
      tolerance = 1e-10,
      info = term
    )
  }
})

test_that("binary multinom: coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_multinom_binary_cov()
  fr <- as_regression_frame(fit, model_id = "M1")
  oracle <- parameters::model_parameters(fit, ci = 0.95, exponentiate = FALSE)
  non_ref <- fr$coefs[!fr$coefs$is_ref, ]
  for (i in seq_len(nrow(non_ref))) {
    spicy_row <- non_ref[i, ]
    bare_term <- sub("^virginica: ", "", spicy_row$term)
    orow <- oracle[oracle$Parameter == bare_term, ]
    if (nrow(orow) == 0L) {
      next
    }
    expect_equal(
      spicy_row$estimate,
      orow$Coefficient,
      tolerance = 1e-6,
      info = bare_term
    )
    expect_equal(
      spicy_row$std_error,
      orow$SE,
      tolerance = 1e-6,
      info = bare_term
    )
    expect_equal(spicy_row$p_value, orow$p, tolerance = 1e-6, info = bare_term)
  }
})


# ---- 2. Polynomial-contrast factor: no reference rows --------------------

test_that("ordered-factor predictor uses polynomial contrasts (reference_dropped FALSE)", {
  fit <- .fit_multinom_ordered_cov()
  fts <- spicy:::detect_factor_terms(fit)
  sz <- Filter(function(ft) ft$factor_term == "sz", fts)[[1L]]
  expect_identical(sz$contrast_type, "polynomial")
  expect_false(isTRUE(sz$reference_dropped))
})

test_that("polynomial-contrast factor yields NO reference rows", {
  fit <- .fit_multinom_ordered_cov()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  # .multinom_reference_rows() skips the only factor term (the `next`) and
  # then returns the empty block -- so no is_ref rows survive.
  expect_false(any(fr$coefs$is_ref))
})

test_that(".multinom_reference_rows returns the empty block for polynomial factors", {
  fit <- .fit_multinom_ordered_cov()
  for (out in c("versicolor", "virginica")) {
    rows <- spicy:::.multinom_reference_rows(fit, outcome = out)
    expect_identical(nrow(rows), 0L)
    expect_true("outcome_level" %in% names(rows))
  }
})


# ---- 3. Missing-nnet abort -----------------------------------------------

test_that(".check_nnet_available aborts with spicy_missing_pkg when nnet absent", {
  expect_error(
    testthat::with_mocked_bindings(
      spicy:::.check_nnet_available(),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    class = "spicy_missing_pkg"
  )
})

test_that("as_regression_frame.multinom surfaces the missing-nnet abort", {
  fit <- .fit_multinom_binary_cov()
  expect_error(
    testthat::with_mocked_bindings(
      as_regression_frame(fit, model_id = "M1"),
      spicy_pkg_available = function(pkg) FALSE,
      .package = "spicy"
    ),
    regexp = "nnet",
    class = "spicy_missing_pkg"
  )
})
