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

test_that("resolve_covariates_argument: overlap with `select` is allowed (orchestrator deduplicates)", {
  # By convention the orchestrator silently excludes covariates from
  # the outcome list (mirroring the existing `by` auto-exclusion), so
  # the helper itself does not error on this overlap.
  data <- data.frame(bmi = 1:3, age = 1:3, sex = c("F", "M", "F"))
  expect_identical(
    spicy:::resolve_covariates_argument(
      rlang::quo(c(age, bmi)),
      data,
      select_names = "bmi"
    ),
    c("age", "bmi")
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

# ---- table_continuous_lm() end-to-end with covariates ---------------------

test_that("table_continuous_lm: numeric outcome by 2-level factor adjusted for one covariate", {
  set.seed(1L)
  n <- 80
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    age = rnorm(n, 50, 10),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  out <- table_continuous_lm(
    df,
    select = bmi,
    by = sex,
    covariates = age,
    output = "long"
  )
  expect_equal(nrow(out), 2L) # one row per level of sex
  expect_identical(attr(out, "covariates"), "age")
  # Adjusted emmeans must differ from raw group means (covariate
  # shifts the predicted means away from the marginal averages).
  raw_means <- tapply(df$bmi, df$sex, mean)
  expect_false(isTRUE(all.equal(unname(raw_means), out$emmean)))
})

test_that("table_continuous_lm: covariate auto-excluded from `select = everything()`", {
  set.seed(2L)
  n <- 60
  df <- data.frame(
    bmi = rnorm(n, 25),
    age = rnorm(n, 50),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  out <- table_continuous_lm(
    df,
    select = tidyselect::where(is.numeric),
    by = sex,
    covariates = age,
    output = "long"
  )
  # `age` would normally be in select via where(is.numeric); the
  # orchestrator must auto-exclude it because it is a covariate.
  expect_false("age" %in% out$variable)
  expect_true("bmi" %in% out$variable)
})

test_that("table_continuous_lm: numeric `by` adjusted for a categorical covariate", {
  set.seed(3L)
  n <- 90
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    age = rnorm(n, 50, 10),
    group = factor(rep(c("A", "B", "C"), each = n / 3))
  )
  out <- table_continuous_lm(
    df,
    select = bmi,
    by = age,
    covariates = group,
    output = "long"
  )
  expect_equal(nrow(out), 1L) # numeric by -> single slope row
  expect_identical(attr(out, "covariates"), "group")
  # Slope of age changes once we adjust for group: compare to raw lm
  fit <- stats::lm(bmi ~ age + group, data = df)
  expect_equal(out$estimate[1], unname(stats::coef(fit)["age"]))
})

test_that("table_continuous_lm: 3-level `by` with a covariate uses partial F", {
  set.seed(4L)
  n <- 90
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    age = rnorm(n, 50, 10),
    group = factor(rep(c("A", "B", "C"), each = n / 3))
  )
  out <- table_continuous_lm(
    df,
    select = bmi,
    by = group,
    covariates = age,
    output = "long",
    effect_size = "f2"
  )
  # Partial F via drop1 -- compare to an independent computation.
  fit <- stats::lm(bmi ~ group + age, data = df)
  d1 <- stats::drop1(fit, scope = ~group, test = "F")
  partial_f <- d1[["F value"]][2]
  partial_df1 <- d1[["Df"]][2]
  expected_f2 <- partial_f * partial_df1 / stats::df.residual(fit)
  expect_equal(out$es_value[1], expected_f2, tolerance = 1e-9)
})

test_that("table_continuous_lm: effect_size = \"d\" + covariates errors with spicy_unsupported", {
  set.seed(5L)
  n <- 60
  df <- data.frame(
    bmi = rnorm(n, 25),
    age = rnorm(n, 50),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  expect_error(
    table_continuous_lm(
      df,
      select = bmi,
      by = sex,
      covariates = age,
      effect_size = "d"
    ),
    class = "spicy_unsupported"
  )
})

test_that("table_continuous_lm: effect_size = \"g\" + covariates errors with spicy_unsupported", {
  set.seed(6L)
  n <- 60
  df <- data.frame(
    bmi = rnorm(n, 25),
    age = rnorm(n, 50),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  expect_error(
    table_continuous_lm(
      df,
      select = bmi,
      by = sex,
      covariates = age,
      effect_size = "g"
    ),
    class = "spicy_unsupported"
  )
})

test_that("table_continuous_lm: NA in covariate triggers complete-cases drop", {
  set.seed(7L)
  n <- 60
  df <- data.frame(
    bmi = rnorm(n, 25),
    age = c(NA, NA, NA, rnorm(n - 3L, 50)),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  out <- table_continuous_lm(
    df,
    select = bmi,
    by = sex,
    covariates = age,
    output = "long"
  )
  expect_equal(out$n[1], n - 3L)
})

test_that("table_continuous_lm: covariates attribute round-trips through default print path", {
  set.seed(8L)
  n <- 60
  df <- data.frame(
    bmi = rnorm(n, 25),
    age = rnorm(n, 50),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  out <- table_continuous_lm(
    df,
    select = bmi,
    by = sex,
    covariates = age,
    output = "long"
  )
  # The render layer (Commit C) will read this attribute to emit
  # the "Adjusted for: ..." footer.
  expect_identical(attr(out, "covariates"), "age")
  expect_identical(attr(out, "covariates"), c("age"))
})
