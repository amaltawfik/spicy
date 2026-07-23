# Coverage tests for R/abort.R cluster-resolution helpers.
# These exercise the internal `resolve_cluster()`, `cluster_lookup_data()`,
# `resolve_cluster_arg()` and `extract_arg_column_name()` utilities directly
# with real `lm()` fits, hitting the formula / string / fallback / na.action
# branches that the public `table_regression()` path does not reach on its own.

# ---- extract_arg_column_name: symbol literally named "NULL" -----------------

test_that("extract_arg_column_name returns NA for a symbol named 'NULL'", {
  # A bare `NULL` captured as a symbol must not be mistaken for a column.
  expect_true(is.na(spicy:::extract_arg_column_name(as.symbol("NULL"))))
})


# ---- resolve_cluster: NULL passthrough --------------------------------------

test_that("resolve_cluster returns NULL when cluster is NULL", {
  fit <- lm(mpg ~ wt, data = mtcars)
  expect_null(spicy:::resolve_cluster(NULL, fit))
})


# ---- resolve_cluster: empty formula errors ----------------------------------

test_that("resolve_cluster errors on a formula with no variables", {
  fit <- lm(mpg ~ wt, data = mtcars)
  expect_error(
    spicy:::resolve_cluster(~1, fit),
    class = "spicy_invalid_input"
  )
})


# ---- resolve_cluster + cluster_lookup_data: variable found in model.frame ----

test_that("resolve_cluster finds a formula variable that is in model.frame", {
  fit <- lm(mpg ~ wt + factor(gear), data = mtcars)
  res <- spicy:::resolve_cluster(~wt, fit)
  expect_equal(length(res), nrow(mtcars))
  expect_equal(unname(res), mtcars$wt)
})

test_that("cluster_lookup_data hits model.frame and reports tried_original = FALSE", {
  fit <- lm(mpg ~ wt + factor(gear), data = mtcars)
  src <- spicy:::cluster_lookup_data(fit, "wt")
  expect_length(src$missing, 0L)
  expect_false(src$tried_original)
  expect_true("wt" %in% src$available)
})


# ---- cluster_lookup_data: fallback to fit$call$data -------------------------

test_that("cluster_lookup_data falls back to original data for an off-model column", {
  df <- mtcars
  df$clinic <- factor(rep(letters[1:4], length.out = nrow(df)))
  fit <- lm(mpg ~ wt, data = df) # `clinic` not in the model frame
  src <- spicy:::cluster_lookup_data(fit, "clinic")
  expect_length(src$missing, 0L)
  expect_true(src$tried_original)
  expect_equal(nrow(src$df), nrow(df))
})

test_that("resolve_cluster resolves a string cluster from the original data", {
  df <- mtcars
  df$clinic <- factor(rep(letters[1:4], length.out = nrow(df)))
  fit <- lm(mpg ~ wt, data = df)
  res <- spicy:::resolve_cluster("clinic", fit)
  expect_equal(res, df$clinic)
})


# ---- cluster_lookup_data: na.action row-subsetting --------------------------

test_that("cluster_lookup_data subsets original data via na.action to match nobs", {
  df <- mtcars
  df$clinic <- factor(rep(letters[1:4], length.out = nrow(df)))
  df$wt[c(2, 5)] <- NA # two rows dropped by lm()
  fit <- lm(mpg ~ wt, data = df)
  expect_lt(stats::nobs(fit), nrow(df)) # precondition: rows were dropped
  src <- spicy:::cluster_lookup_data(fit, "clinic")
  expect_length(src$missing, 0L)
  expect_true(src$tried_original)
  expect_equal(nrow(src$df), stats::nobs(fit))
  # The dropped rows (2, 5) are gone.
  expect_equal(src$df$clinic, df$clinic[-c(2, 5)])
})


# ---- cluster_lookup_data: last-resort branch (no attached data) -------------

test_that("cluster_lookup_data reports missing when fit has no data and var absent", {
  # Fit built from loose vectors: fit$call$data is NULL, so the original-data
  # fallback cannot fire and the last-resort model.frame report is used.
  y <- mtcars$mpg
  x <- mtcars$wt
  fit <- lm(y ~ x)
  expect_null(fit$call$data)
  src <- spicy:::cluster_lookup_data(fit, "nope")
  expect_equal(src$missing, "nope")
  expect_false(src$tried_original)
})

test_that("resolve_cluster errors for a string column absent everywhere", {
  y <- mtcars$mpg
  x <- mtcars$wt
  fit <- lm(y ~ x)
  expect_error(
    spicy:::resolve_cluster("nope", fit),
    class = "spicy_invalid_input"
  )
})


# ---- resolve_cluster_arg: per-model list length mismatch --------------------

test_that("resolve_cluster_arg errors when a cluster list length != models length", {
  models <- list(lm(mpg ~ wt, mtcars), lm(mpg ~ hp, mtcars))
  expect_error(
    spicy:::resolve_cluster_arg(list(~gear), models), # 1 cluster, 2 models
    class = "spicy_invalid_input"
  )
})

test_that("resolve_cluster_arg resolves a per-model list of formulas", {
  models <- list(lm(mpg ~ wt, mtcars), lm(mpg ~ hp, mtcars))
  res <- spicy:::resolve_cluster_arg(list(~gear, ~carb), models)
  expect_length(res, 2L)
  expect_equal(length(res[[1]]), nrow(mtcars))
  expect_equal(unname(res[[1]]), mtcars$gear)
  expect_equal(unname(res[[2]]), mtcars$carb)
})
