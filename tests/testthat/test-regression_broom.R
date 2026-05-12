# Tests for table_regression() broom integration (Step 12 / Q8 / Q17).

mt <- mtcars
mt$cyl <- factor(mt$cyl)


# ============================================================================
# tidy.spicy_regression_table
# ============================================================================

test_that("tidy — returns broom-canonical column names", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  td <- broom::tidy(out)

  expected <- c("model_id", "outcome", "term", "estimate_type",
                "estimate", "std.error", "conf.low", "conf.high",
                "statistic", "df", "p.value", "test_type",
                "is_intercept", "factor_term", "factor_level")
  expect_true(all(expected %in% names(td)))
})

test_that("tidy — drops reference rows (no estimable values)", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  td <- broom::tidy(out)
  # cyl4 is the reference level — should not appear in tidy
  expect_false(any(td$term == "cyl4"))
  # But cyl6 and cyl8 should
  expect_true("cyl6" %in% td$term)
  expect_true("cyl8" %in% td$term)
})

test_that("tidy — drops singular coefs (NA estimates)", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  td <- broom::tidy(out)
  expect_false(any(is.na(td$estimate)))
})

test_that("tidy — multi-model: model_id distinguishes rows", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(Crude = m1, Adjusted = m2))
  td <- broom::tidy(out)
  expect_true(all(c("Crude", "Adjusted") %in% td$model_id))
  # Both models contribute a wt row
  wt_rows <- td[td$term == "wt", ]
  expect_equal(nrow(wt_rows), 2L)
  expect_setequal(wt_rows$model_id, c("Crude", "Adjusted"))
})

test_that("tidy — estimate values match the underlying lm fit", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  td <- broom::tidy(out)
  cf <- coef(fit)
  for (nm in names(cf)) {
    row <- td[td$term == nm & td$estimate_type == "B", ]
    expect_equal(row$estimate, unname(cf[nm]), tolerance = 1e-12)
  }
})

test_that("tidy — partial_eta2 rows have NA std.error and finite estimate", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, show_columns = c("b", "partial_eta2"))
  td <- broom::tidy(out)
  pe <- td[td$estimate_type == "partial_eta2", ]
  expect_true(nrow(pe) > 0L)
  expect_true(all(is.na(pe$std.error)))
  expect_true(all(is.finite(pe$estimate)))
})

test_that("tidy — empty input → empty broom-shaped tibble", {
  # Construct a manually-empty spicy_regression_table
  empty <- structure(
    data.frame(Variable = character(0), stringsAsFactors = FALSE),
    title = NULL, note = NULL,
    spicy_long = NULL, spicy_fit_stats = NULL,
    class = c("spicy_regression_table", "spicy_table", "data.frame")
  )
  td <- broom::tidy(empty)
  expect_equal(nrow(td), 0L)
  expect_true(all(c("model_id", "term", "estimate", "p.value")
                  %in% names(td)))
})


# ============================================================================
# glance.spicy_regression_table
# ============================================================================

test_that("glance — returns one row per model", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  g <- broom::glance(out)
  expect_equal(nrow(g), 1L)
})

test_that("glance — multi-model: one row per (model_id, outcome)", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2))
  g <- broom::glance(out)
  expect_equal(nrow(g), 2L)
})

test_that("glance — broom-canonical column names", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  g <- broom::glance(out)
  expected <- c("model_id", "outcome", "nobs", "r.squared",
                "adj.r.squared", "df.residual")
  expect_true(all(expected %in% names(g)))
})

test_that("glance — r.squared / adj.r.squared match summary(fit)", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  g <- broom::glance(out)
  sm <- summary(fit)
  expect_equal(g$r.squared[1], sm$r.squared, tolerance = 1e-12)
  expect_equal(g$adj.r.squared[1], sm$adj.r.squared, tolerance = 1e-12)
})

test_that("glance — df.residual is numeric (not integer) — Satterthwaite-safe", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  g <- broom::glance(out)
  expect_type(g$df.residual, "double")
})


# ============================================================================
# as.data.frame / as_tibble
# ============================================================================

test_that("as.data.frame — strips spicy classes, keeps title/note", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  df <- as.data.frame(out)
  expect_s3_class(df, "data.frame")
  expect_false(inherits(df, "spicy_regression_table"))
  expect_false(inherits(df, "spicy_table"))
  expect_match(attr(df, "title"), "^Linear regression: mpg")
  expect_match(attr(df, "note"), "^Note\\.")
})

test_that("as.data.frame — same row content as default output", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  df <- as.data.frame(out)
  # Both have identical Variable column and B column values
  expect_equal(df$Variable, out$Variable)
  expect_equal(df$B, out$B)
})

test_that("as.data.frame — strips internal spicy_long / spicy_fit_stats attrs", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  df <- as.data.frame(out)
  expect_null(attr(df, "spicy_long"))
  expect_null(attr(df, "spicy_fit_stats"))
  expect_null(attr(df, "col_spec"))
})

test_that("as_tibble — returns tbl_df", {
  skip_if_not_installed("tibble")
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  tb <- tibble::as_tibble(out)
  expect_s3_class(tb, "tbl_df")
})


# ============================================================================
# Round-trip identity: tidy + glance preserve the analytic content
# ============================================================================

test_that("tidy ⇄ raw long: per-coef estimates round-trip", {
  fit <- lm(mpg ~ wt + cyl + am, data = mt)
  out <- table_regression(fit)
  td <- broom::tidy(out)
  raw <- table_regression(fit, output = "long")
  # Each B-row in tidy must match the corresponding raw entry
  for (i in seq_len(nrow(td))) {
    if (td$estimate_type[i] != "B") next
    raw_row <- raw[raw$term == td$term[i] & raw$estimate_type == "B",
                   , drop = FALSE]
    expect_equal(td$estimate[i], raw_row$estimate, tolerance = 1e-12)
    expect_equal(td$std.error[i], raw_row$std.error, tolerance = 1e-12)
  }
})
