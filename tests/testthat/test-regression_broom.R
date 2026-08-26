# Tests for table_regression() broom integration (Step 12 / Q8 / Q17).

mt <- mtcars
mt$cyl <- factor(mt$cyl)


# ============================================================================
# tidy.spicy_regression_table
# ============================================================================

test_that("tidy – returns broom-canonical column names", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  td <- broom::tidy(out)

  expected <- c(
    "model_id",
    "outcome",
    "term",
    "estimate_type",
    "estimate",
    "std.error",
    "conf.low",
    "conf.high",
    "statistic",
    "df",
    "p.value",
    "test_type",
    "is_intercept",
    "factor_term",
    "factor_level"
  )
  expect_true(all(expected %in% names(td)))
})

test_that("tidy – drops reference rows (no estimable values)", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  td <- broom::tidy(out)
  # cyl4 is the reference level – should not appear in tidy
  expect_false(any(td$term == "cyl4"))
  # But cyl6 and cyl8 should
  expect_true("cyl6" %in% td$term)
  expect_true("cyl8" %in% td$term)
})

test_that("tidy – drops singular coefs (NA estimates)", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  td <- broom::tidy(out)
  expect_false(any(is.na(td$estimate)))
})

test_that("tidy – multi-model: model_id distinguishes rows", {
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

test_that("tidy – estimate values match the underlying lm fit", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  td <- broom::tidy(out)
  cf <- coef(fit)
  for (nm in names(cf)) {
    row <- td[td$term == nm & td$estimate_type == "B", ]
    expect_equal(row$estimate, unname(cf[nm]), tolerance = 1e-12)
  }
})

test_that("tidy – partial_eta2 rows have NA std.error and finite estimate", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, show_columns = c("b", "partial_eta2"))
  td <- broom::tidy(out)
  pe <- td[td$estimate_type == "partial_eta2", ]
  expect_true(nrow(pe) > 0L)
  expect_true(all(is.na(pe$std.error)))
  expect_true(all(is.finite(pe$estimate)))
})

test_that("tidy – empty input → empty broom-shaped tibble", {
  # Construct a manually-empty spicy_regression_table
  empty <- structure(
    data.frame(Variable = character(0), stringsAsFactors = FALSE),
    title = NULL,
    note = NULL,
    spicy_long = NULL,
    spicy_fit_stats = NULL,
    class = c("spicy_regression_table", "spicy_table", "data.frame")
  )
  td <- broom::tidy(empty)
  expect_equal(nrow(td), 0L)
  expect_true(all(c("model_id", "term", "estimate", "p.value") %in% names(td)))
})


# ============================================================================
# glance.spicy_regression_table
# ============================================================================

test_that("glance – returns one row per model", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  g <- broom::glance(out)
  expect_equal(nrow(g), 1L)
})

test_that("glance – multi-model: one row per (model_id, outcome)", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2))
  g <- broom::glance(out)
  expect_equal(nrow(g), 2L)
})

test_that("glance – broom-canonical column names", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  g <- broom::glance(out)
  expected <- c(
    "model_id",
    "outcome",
    "nobs",
    "r.squared",
    "adj.r.squared",
    "df.residual"
  )
  expect_true(all(expected %in% names(g)))
})

test_that("glance – r.squared / adj.r.squared match summary(fit)", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  g <- broom::glance(out)
  sm <- summary(fit)
  expect_equal(g$r.squared[1], sm$r.squared, tolerance = 1e-12)
  expect_equal(g$adj.r.squared[1], sm$adj.r.squared, tolerance = 1e-12)
})

test_that("glance – df.residual is numeric (not integer) – Satterthwaite-safe", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  g <- broom::glance(out)
  expect_type(g$df.residual, "double")
})


# ============================================================================
# as.data.frame / as_tibble
# ============================================================================

test_that("as.data.frame – strips spicy classes, keeps title/note", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  df <- as.data.frame(out)
  expect_s3_class(df, "data.frame")
  expect_false(inherits(df, "spicy_regression_table"))
  expect_false(inherits(df, "spicy_table"))
  expect_match(attr(df, "title"), "^Linear regression: mpg")
  expect_match(attr(df, "note"), "^Note\\.")
})

test_that("as.data.frame – same row content as default output", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  df <- as.data.frame(out)
  # Both have identical Variable column and B column values
  expect_equal(df$Variable, out$Variable)
  expect_equal(df$B, out$B)
})

test_that("as.data.frame – strips internal spicy_long / spicy_fit_stats attrs", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  df <- as.data.frame(out)
  expect_null(attr(df, "spicy_long"))
  expect_null(attr(df, "spicy_fit_stats"))
  # `col_spec` is kept: the documented equivalence with
  # `output = "data.frame"` requires both paths to carry the same
  # attribute set (rd-methods:asdf-roundtrip-output-dataframe).
  expect_identical(attr(df, "col_spec"), attr(out, "col_spec"))
})

test_that("as_tibble – returns tbl_df", {
  skip_if_not_installed("tibble")
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  tb <- tibble::as_tibble(out)
  expect_s3_class(tb, "tbl_df")
})


# ============================================================================
# Round-trip identity: tidy + glance preserve the analytic content
# ============================================================================

# ============================================================================
# Phase 3 matrix – critic:pkgrd-broom-columns-stabilising and
# critic:pkgrd-broom-df-types (lot T4)
# ============================================================================

test_that("tidy/glance column sets are frozen (stabilising contract)", {
  # expect_identical on the FULL name vector: any silent rename,
  # removal, or reorder of an existing column is a contract break
  # (?spicy, section 'broom output shape'). Adding optional new
  # columns is allowed -- append them HERE with a NEWS entry.
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  expect_identical(
    names(broom::tidy(out)),
    c(
      "model_id",
      "outcome",
      "outcome_level",
      "term",
      "estimate_type",
      "estimate",
      "std.error",
      "conf.low",
      "conf.high",
      "statistic",
      "df",
      "p.value",
      "test_type",
      "is_intercept",
      "factor_term",
      "factor_level"
    )
  )
  expect_identical(
    names(broom::glance(out)),
    c(
      "model_id",
      "outcome",
      "nobs",
      "weighted_nobs",
      "r.squared",
      "adj.r.squared",
      "omega2",
      "sigma",
      "rmse",
      "f2",
      "AIC",
      "AICc",
      "BIC",
      "deviance",
      "df.residual"
    )
  )
})

test_that("tidy df is numeric double and carries fractional Satterthwaite df verbatim", {
  skip_if_not_installed("clubSandwich")
  fit <- lm(mpg ~ wt, data = mtcars)
  # Classical: double-typed even when the values are whole numbers.
  td0 <- broom::tidy(table_regression(fit))
  expect_type(td0$df, "double")
  # CR2: the per-coefficient Satterthwaite df must arrive verbatim in
  # tidy() -- fractional, matching clubSandwich::coef_test().
  out <- table_regression(fit, vcov = "CR2", cluster = mtcars$cyl)
  td <- broom::tidy(out)
  ct <- clubSandwich::coef_test(fit, vcov = "CR2", cluster = mtcars$cyl)
  expect_type(td$df, "double")
  for (nm in c("(Intercept)", "wt")) {
    expect_equal(
      td$df[td$term == nm],
      ct$df_Satt[ct$Coef == nm],
      tolerance = 1e-8
    )
  }
  expect_false(all(td$df == round(td$df)))
  # glance keeps df.residual double alongside.
  expect_type(broom::glance(out)$df.residual, "double")
})


test_that("tidy ⇄ raw long: per-coef estimates round-trip", {
  fit <- lm(mpg ~ wt + cyl + am, data = mt)
  out <- table_regression(fit)
  td <- broom::tidy(out)
  raw <- table_regression(fit, output = "long")
  # Each B-row in tidy must match the corresponding raw entry
  n_checked <- 0L
  for (i in seq_len(nrow(td))) {
    if (td$estimate_type[i] != "B") {
      next
    }
    raw_row <- raw[
      raw$term == td$term[i] & raw$estimate_type == "B",
      ,
      drop = FALSE
    ]
    expect_equal(td$estimate[i], raw_row$estimate, tolerance = 1e-12)
    expect_equal(td$std.error[i], raw_row$std.error, tolerance = 1e-12)
    n_checked <- n_checked + 1L
  }
  # A tidy frame with no B rows at all would make every iteration skip.
  expect_oracle_covered(n_checked)
})


# ============================================================================
# Phase 3 matrix – rd-methods: broom / data.frame method promises
# ============================================================================

test_that("tidy / glance – extra dots are ignored (broom-generic compat)", {
  # rd-methods:tidy-glance-dots-ignored
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  expect_identical(
    broom::tidy(out),
    broom::tidy(out, conf.int = FALSE, foo = 1)
  )
  expect_identical(broom::glance(out), broom::glance(out, bar = 2))
})

test_that("tidy – exact documented column vector and tbl_df return", {
  # rd-methods:tidy-column-contract
  # rd-methods:tidy-glance-return-type (tibble sits in Imports, so the
  # documented tbl_df branch is the only reachable one)
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  td <- broom::tidy(out)
  expect_identical(
    names(td),
    c(
      "model_id",
      "outcome",
      "outcome_level",
      "term",
      "estimate_type",
      "estimate",
      "std.error",
      "conf.low",
      "conf.high",
      "statistic",
      "df",
      "p.value",
      "test_type",
      "is_intercept",
      "factor_term",
      "factor_level"
    )
  )
  expect_s3_class(td, "tbl_df")
  expect_s3_class(broom::glance(out), "tbl_df")
})

test_that("glance – exact documented column vector on a fitted model", {
  # rd-methods:glance-column-contract (the empty branch is pinned in
  # test-cov-regression_broom.R; this pins the fitted-model branch)
  fit <- lm(mpg ~ wt + cyl, data = mt)
  gl <- broom::glance(table_regression(fit))
  expect_identical(
    names(gl),
    c(
      "model_id",
      "outcome",
      "nobs",
      "weighted_nobs",
      "r.squared",
      "adj.r.squared",
      "omega2",
      "sigma",
      "rmse",
      "f2",
      "AIC",
      "AICc",
      "BIC",
      "deviance",
      "df.residual"
    )
  )
})

test_that("tidy – a truly aliased coefficient is dropped (no NA estimates)", {
  # rd-methods:tidy-drops-reference-and-singular (aliased half; the
  # reference half is pinned in "tidy – drops reference rows" above)
  mt2 <- mt
  mt2$wt2 <- mt2$wt * 2
  fal <- lm(mpg ~ wt + wt2 + cyl, data = mt2)
  expect_true(anyNA(coef(fal))) # wt2 is aliased by construction
  td <- broom::tidy(table_regression(fal))
  expect_false("wt2" %in% td$term)
  expect_false(any(is.na(td$estimate)))
})

test_that("tidy – estimate_type stays in the documented domain", {
  # rd-methods:tidy-estimate-type-domain
  skip_if_not_installed("marginaleffects")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(
    fit,
    standardized = "refit",
    show_columns = c(
      "b",
      "beta",
      "ame",
      "partial_f2",
      "partial_eta2",
      "partial_omega2"
    )
  )
  td <- broom::tidy(out)
  domain <- c(
    "B",
    "beta",
    "ame",
    "partial_f2",
    "partial_eta2",
    "partial_omega2"
  )
  expect_true(all(td$estimate_type %in% domain))
  # Every documented value is exercised, so the domain check bites.
  expect_setequal(unique(td$estimate_type), domain)
})

test_that("tidy – one row per (model_id, term, estimate_type, outcome_level)", {
  # rd-methods:tidy-row-grain
  skip_if_not_installed("marginaleffects")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt, data = mt)
  out <- table_regression(
    list(A = m1, B = m2),
    standardized = "refit",
    show_columns = c("b", "beta", "ame", "p")
  )
  td <- broom::tidy(out)
  keys <- td[, c("model_id", "term", "estimate_type", "outcome_level")]
  expect_false(any(duplicated(keys)))
  expect_gt(nrow(td), 6L)
})

test_that("as.data.frame – row.names and optional are ignored", {
  # rd-methods:asdf-rownames-optional-ignored
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  expect_identical(
    as.data.frame(out),
    as.data.frame(
      out,
      row.names = letters[seq_len(nrow(out))],
      optional = TRUE
    )
  )
})

test_that("as_tibble – keeps title/note and the data.frame cells", {
  # rd-methods:asdf-preserves-title-note (as_tibble half)
  # rd-methods:astibble-returns-tbldf (cell-equality half)
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  tb <- tibble::as_tibble(out)
  expect_s3_class(tb, "tbl_df")
  expect_identical(attr(tb, "title"), attr(out, "title"))
  expect_identical(attr(tb, "note"), attr(out, "note"))
  strip <- function(d) {
    attributes(d) <- attributes(d)[c("names", "row.names", "class")]
    class(d) <- "data.frame"
    d
  }
  expect_identical(strip(as.data.frame(tb)), strip(as.data.frame(out)))
})

test_that("as.data.frame equals output = 'data.frame' cell-for-cell", {
  # rd-methods:asdf-roundtrip-output-dataframe (content contract; the
  # strict attribute-identity half is the FIXME below)
  fit <- lm(mpg ~ wt + cyl, data = mt)
  d1 <- as.data.frame(table_regression(fit))
  d2 <- table_regression(fit, output = "data.frame")
  expect_identical(class(d1), "data.frame")
  expect_identical(class(d2), "data.frame")
  strip <- function(d) {
    attributes(d) <- attributes(d)[c("names", "row.names", "class")]
    class(d) <- "data.frame"
    d
  }
  expect_identical(strip(d1), strip(d2))
  expect_identical(attr(d1, "title"), attr(d2, "title"))
  expect_identical(attr(d1, "note"), attr(d2, "note"))
})

test_that("as.data.frame and output = 'data.frame' are attribute-identical", {
  # rd-methods:asdf-roundtrip-output-dataframe (strict half): the Rd
  # Details say the two paths are equivalent -- identical objects,
  # attributes included. Both carry the provenance pair
  # (model_ids / outcome) and the rendering attributes (col_spec,
  # structured, ...); neither carries spicy_long / spicy_fit_stats.
  fit <- lm(mpg ~ wt + cyl, data = mt)
  d1 <- as.data.frame(table_regression(fit))
  d2 <- table_regression(fit, output = "data.frame")
  expect_identical(d1, d2)
  # Multi-model path too (provenance pair has one entry per model).
  fit2 <- lm(mpg ~ wt + hp, data = mt)
  e1 <- as.data.frame(table_regression(list(A = fit, B = fit2)))
  e2 <- table_regression(list(A = fit, B = fit2), output = "data.frame")
  expect_identical(e1, e2)
  expect_identical(attr(e1, "model_ids"), c("A", "B"))
})
