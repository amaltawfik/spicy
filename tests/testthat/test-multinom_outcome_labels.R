# ---------------------------------------------------------------------------
# Phase 7c4 tests: multinom / mlogit body now shows outcome-prefixed rows
# under a predictor section header (instead of duplicating rows that share
# a term across outcomes).
# ---------------------------------------------------------------------------

.fit_multinom_iris_oc <- function() {
  skip_if_not_installed("nnet")
  nnet::multinom(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    trace = FALSE
  )
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
# (Multi-model tables only: a SINGLE multinom now renders the
# outcome-as-columns layout, tested in test-multinom_columns.R.)

test_that("multi-model multinom body shows grouped rows with outcome", {
  fit <- .fit_multinom_iris_oc()
  fit0 <- nnet::multinom(Species ~ Sepal.Length, data = iris, trace = FALSE)
  combined <- paste(
    capture.output(print(table_regression(list(fit0, fit)))),
    collapse = "\n"
  )
  # Predictor section headers
  expect_match(combined, "(Intercept):", fixed = TRUE)
  expect_match(combined, "Sepal.Length:", fixed = TRUE)
  expect_match(combined, "Sepal.Width:", fixed = TRUE)
  # Under each header, a row per outcome category, named by it alone.
  shown <- trimws(table_regression(list(fit0, fit))$Variable)
  expect_identical(
    shown[which(shown == "(Intercept):") + 1:2],
    c("versicolor", "virginica")
  )
  expect_false(grepl("versicolor: (Intercept)", combined, fixed = TRUE))
})

test_that("a multinomial row beside other models names its category alone", {
  skip_if_not_installed("nnet")
  d <- stats::na.omit(sochealth[, c("education", "age", "sex", "smoking")])
  mn <- nnet::multinom(education ~ age + sex, data = d, trace = FALSE)
  lg <- stats::glm(smoking ~ age + sex, data = d, family = stats::binomial)
  out <- table_regression(
    list(mn, lg),
    labels = c("(Intercept)" = "Constant", age = "Age (years)")
  )
  shown <- trimws(out$Variable)
  cats <- c("Upper secondary", "Tertiary")
  # The header names the term (its label), the rows the categories.
  expect_identical(shown[which(shown == "Constant:") + 1:2], cats)
  expect_identical(shown[which(shown == "Age (years):") + 1:2], cats)
  # A factor level keeps its category: the levels stay apart.
  expect_true(all(paste0(cats, ": Male") %in% shown))
  # The data frame gives each row its category, and inline() reaches it,
  # as it reaches the rows of the logistic model beside it.
  df <- as.data.frame(out)
  expect_identical(
    df$level[df$variable == "age" & df$.row_role == "level"],
    cats
  )
  expect_identical(
    inline(out, age, "Tertiary", model = 1),
    sprintf("%.2f", stats::coef(mn)["Tertiary", "age"])
  )
  expect_identical(
    inline(out, age, model = 2),
    sprintf("%.2f", stats::coef(lg)[["age"]])
  )
  # The machine outputs keep the prefixed term and the category.
  skip_if_not_installed("broom")
  td <- broom::tidy(out)
  expect_true("Tertiary: age" %in% td$term)
  expect_identical(
    unique(td$outcome_level[td$term == "Tertiary: age"]),
    "Tertiary"
  )
})

test_that("multinom shows DIFFERENT estimates for the two outcomes", {
  fit <- .fit_multinom_iris_oc()
  combined <- paste(
    capture.output(print(table_regression(fit))),
    collapse = "\n"
  )
  # versicolor intercept is -92.10; virginica intercept is -105.10 --
  # now side by side in the two category column groups. If the body
  # deduped to one value per term, we'd never see both.
  expect_match(combined, "-92.10", fixed = TRUE)
  expect_match(combined, "-105.10", fixed = TRUE)
})


# ---- 3. mlogit body: two-segment layout (sections per alternative) -----

test_that("table_regression() body shows per-alternative mlogit sections", {
  fit <- .fit_mlogit_fishing_oc()
  combined <- paste(
    capture.output(print(table_regression(fit))),
    collapse = "\n"
  )
  # One labeled section per non-reference alternative, bare rows
  # inside (the Stata asclogit presentation; phase 2 of the
  # categorical layouts, dev/mlogit_two_segment_spec.md).
  expect_match(combined, "boat:", fixed = TRUE)
  expect_match(combined, "charter:", fixed = TRUE)
  expect_match(combined, "pier:", fixed = TRUE)
  expect_false(grepl("boat: (Intercept)", combined, fixed = TRUE))
})
