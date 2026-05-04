# Unit tests for the shared lm-input-resolution helpers in
# `R/lm_helpers.R`. These helpers are reused by `table_continuous_lm()`
# and (in 0.13.0) `table_regression()`, so their contract must hold
# independently of either consumer.

# ---- resolve_covariates_argument ------------------------------------------

test_that("resolve_covariates_argument: NULL returns character(0)", {
  data <- data.frame(a = 1:3, b = 4:6)
  expect_identical(
    spicy:::resolve_covariates_argument(rlang::quo(NULL), data),
    character()
  )
})

test_that("resolve_covariates_argument: tidyselect bare names", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"), bmi = 1:3)
  expect_identical(
    spicy:::resolve_covariates_argument(rlang::quo(c(age, sex)), data),
    c("age", "sex")
  )
})

test_that("resolve_covariates_argument: literal character vector", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  expect_identical(
    spicy:::resolve_covariates_argument(rlang::quo(c("age", "sex")), data),
    c("age", "sex")
  )
})

test_that("resolve_covariates_argument: all_of() selector", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  expect_identical(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::all_of(c("age", "sex"))),
      data
    ),
    c("age", "sex")
  )
})

test_that("resolve_covariates_argument: starts_with() selector", {
  data <- data.frame(bmi_pre = 1:3, bmi_post = 1:3, age = 1:3)
  expect_identical(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::starts_with("bmi")),
      data
    ),
    c("bmi_pre", "bmi_post")
  )
})

test_that("resolve_covariates_argument: where(predicate) selector", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"), w = c(0.5, 1, 1.5))
  expect_identical(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::where(is.numeric)),
      data
    ),
    c("age", "w")
  )
})

test_that("resolve_covariates_argument: empty tidyselect match returns character(0)", {
  data <- data.frame(a = 1:3)
  expect_identical(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::starts_with("zzz_no_match")),
      data
    ),
    character()
  )
})

test_that("resolve_covariates_argument: empty character vector returns character(0)", {
  data <- data.frame(a = 1:3)
  expect_identical(
    spicy:::resolve_covariates_argument(rlang::quo(character()), data),
    character()
  )
})

# ---- supported atomic classes ----------------------------------------------

test_that("resolve_covariates_argument: accepts numeric / integer / logical / factor / character", {
  data <- data.frame(
    n_dbl = c(1.0, 2.0, 3.0),
    n_int = 1:3,
    l     = c(TRUE, FALSE, TRUE),
    f     = factor(c("a", "b", "a")),
    c     = c("x", "y", "z")
  )
  expect_identical(
    spicy:::resolve_covariates_argument(
      rlang::quo(c(n_dbl, n_int, l, f, c)),
      data
    ),
    c("n_dbl", "n_int", "l", "f", "c")
  )
})

# ---- error paths -----------------------------------------------------------

test_that("resolve_covariates_argument: formula syntax rejected (additive)", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(~ age + sex), data),
    class = "spicy_unsupported"
  )
})

test_that("resolve_covariates_argument: formula syntax rejected (interaction)", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(~ age * sex), data),
    class = "spicy_unsupported"
  )
})

test_that("resolve_covariates_argument: formula syntax rejected (transform)", {
  data <- data.frame(age = 1:3)
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(~ I(age^2)), data),
    class = "spicy_unsupported"
  )
})

test_that("resolve_covariates_argument: nonexistent column", {
  data <- data.frame(age = 1:3)
  expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::all_of("nonexistent")),
      data
    ),
    class = "spicy_missing_column"
  )
})

test_that("resolve_covariates_argument: nonexistent column via literal char vec", {
  # The literal-character-vector path bypasses tidyselect's existence
  # check; the explicit re-validation in the helper must catch it.
  data <- data.frame(age = 1:3)
  expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(c("age", "nonexistent")),
      data
    ),
    class = "spicy_missing_column"
  )
})

test_that("resolve_covariates_argument: list-column rejected", {
  data <- data.frame(age = 1:3)
  data$lst <- I(list(1, 2, 3))
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(c(age, lst)), data),
    class = "spicy_invalid_input"
  )
})

test_that("resolve_covariates_argument: complex column rejected", {
  data <- data.frame(age = 1:3)
  data$z <- complex(real = c(1, 2, 3), imaginary = c(0, 1, 0))
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(c(age, z)), data),
    class = "spicy_invalid_input"
  )
})

test_that("resolve_covariates_argument: overlap with `by` rejected", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(c(age, sex)),
      data,
      by_name = "sex"
    ),
    class = "spicy_invalid_input"
  )
})

test_that("resolve_covariates_argument: overlap with `select` (outcomes) rejected", {
  data <- data.frame(bmi = 1:3, age = 1:3, sex = c("F", "M", "F"))
  expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(c(age, bmi)),
      data,
      select_names = "bmi"
    ),
    class = "spicy_invalid_input"
  )
})

test_that("resolve_covariates_argument: where() may overlap with by -> errors", {
  # `where(is.numeric)` would silently grab the predictor if it is
  # numeric; the overlap check forces the user to be explicit.
  data <- data.frame(age = 1:3, weight = c(70, 80, 75))
  expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::where(is.numeric)),
      data,
      by_name = "age"
    ),
    class = "spicy_invalid_input"
  )
})

test_that("resolve_covariates_argument: every error inherits from spicy_error", {
  data <- data.frame(age = 1:3)
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(~ age), data),
    class = "spicy_error"
  )
  expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::all_of("nope")),
      data
    ),
    class = "spicy_error"
  )
})
