# ---------------------------------------------------------------------------
# Phase 7c4 tests: multinom / mlogit body now shows outcome-prefixed rows
# under a predictor section header (instead of duplicating rows that share
# a term across outcomes).
# ---------------------------------------------------------------------------


.fit_multinom_iris_oc <- function() {
  skip_if_not_installed("nnet")
  nnet::multinom(Species ~ Sepal.Length + Sepal.Width,
                 data = iris, trace = FALSE)
}

.fit_mlogit_fishing_oc <- function() {
  skip_if_not_installed("mlogit")
  data("Fishing", package = "mlogit", envir = environment())
  Fish <- mlogit::dfidx(Fishing, varying = 2:9, choice = "mode", shape = "wide")
  mlogit::mlogit(mode ~ price + catch, data = Fish)
}


# ---- 1. multinom term uniqueness ---------------------------------------

test_that("multinom: term is prefixed with outcome (uniqueness preserved)", {
  fit <- .fit_multinom_iris_oc()
  fr <- as_regression_frame(fit, model_id = "M1")
  non_ref <- fr$coefs[!fr$coefs$is_ref, ]
  expect_identical(length(unique(non_ref$term)), nrow(non_ref))
  expect_true(all(grepl("^versicolor:|^virginica:", non_ref$term)))
})


# ---- 2. multinom body output groups by predictor + shows outcome -------

test_that("table_regression() body shows grouped multinom rows with outcome", {
  fit <- .fit_multinom_iris_oc()
  combined <- paste(capture.output(print(table_regression(fit))),
                    collapse = "\n")
  # Predictor section headers
  expect_match(combined, "(Intercept):",  fixed = TRUE)
  expect_match(combined, "Sepal.Length:", fixed = TRUE)
  expect_match(combined, "Sepal.Width:",  fixed = TRUE)
  # Outcome-labelled rows
  expect_match(combined, "versicolor: (Intercept)", fixed = TRUE)
  expect_match(combined, "virginica: (Intercept)",  fixed = TRUE)
})

test_that("multinom body shows DIFFERENT estimates for the two outcomes", {
  fit <- .fit_multinom_iris_oc()
  combined <- paste(capture.output(print(table_regression(fit))),
                    collapse = "\n")
  # versicolor intercept is -92.10; virginica intercept is -105.10.
  # If the body deduped to one row per term, we'd never see both values.
  expect_match(combined, "-92.10",  fixed = TRUE)
  expect_match(combined, "-105.10", fixed = TRUE)
})


# ---- 3. mlogit body shows alt-prefixed labels --------------------------

test_that("table_regression() body shows alt-prefixed mlogit intercepts", {
  fit <- .fit_mlogit_fishing_oc()
  combined <- paste(capture.output(print(table_regression(fit))),
                    collapse = "\n")
  expect_match(combined, "boat: (Intercept)",    fixed = TRUE)
  expect_match(combined, "charter: (Intercept)", fixed = TRUE)
  expect_match(combined, "pier: (Intercept)",    fixed = TRUE)
})
