# Integration tests for table_regression() — Phase 1 end-to-end.

mt <- mtcars
mt$cyl <- factor(mt$cyl)

# ============================================================================
# default output
# ============================================================================

test_that("table_regression — default output: spicy_regression_table class + attrs", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  expect_s3_class(out, "spicy_regression_table")
  expect_s3_class(out, "spicy_table")
  expect_s3_class(out, "data.frame")
  expect_match(attr(out, "title"), "^Regression: mpg")
  expect_match(attr(out, "note"), "^Note\\. ")
})

test_that("table_regression — single fit and 1-list of fits behave the same", {
  fit <- lm(mpg ~ wt, data = mt)
  o1 <- table_regression(fit)
  o2 <- table_regression(list(fit))
  # Same body content (titles/labels may differ trivially)
  expect_equal(unname(unlist(o1$Variable)), unname(unlist(o2$Variable)))
})

test_that("table_regression — factor with reference: ref row em-dashed", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  ref_idx <- grep("\\(ref\\.\\)", out$Variable)
  expect_equal(length(ref_idx), 1L)
  stat_cols <- setdiff(names(out), "Variable")
  expect_true(all(unlist(out[ref_idx, stat_cols]) == "—"))
})


# ============================================================================
# show_columns + standardized
# ============================================================================

test_that("table_regression — standardized != 'none' auto-injects 'beta'", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, standardized = "refit")
  expect_true("β" %in% names(out))
})

test_that("table_regression — 'beta' without standardized errors with spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, show_columns = c("B", "beta")),
    class = "spicy_invalid_input"
  )
})

test_that("table_regression — partial_eta2 cell uses Q19 compact 'value [CI]'", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, show_columns = c("B", "partial_eta2"))
  wt_row <- out[out$Variable == "wt", , drop = FALSE]
  expect_match(wt_row$`η²`, "^[0-9]+\\.[0-9]+ \\[")
})


# ============================================================================
# stars (Q12)
# ============================================================================

test_that("table_regression — stars = TRUE applied to B (no β requested)", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, stars = TRUE)
  wt_row <- out[out$Variable == "wt", , drop = FALSE]
  expect_match(wt_row$B, "\\*\\*\\*$")
})

test_that("table_regression — stars on β when standardized != 'none'", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, standardized = "refit", stars = TRUE)
  wt_row <- out[out$Variable == "wt", , drop = FALSE]
  expect_match(wt_row$β, "\\*\\*\\*$")
  # B should NOT carry the stars when β is shown
  expect_false(grepl("\\*", wt_row$B))
})


# ============================================================================
# Multi-model + nested
# ============================================================================

test_that("table_regression — two models: per-model column groups", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2))
  expect_true(any(grepl("Model 1: B$", names(out))))
  expect_true(any(grepl("Model 2: B$", names(out))))
})

test_that("table_regression — names(list) used as model labels", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(Crude = m1, Adjusted = m2))
  expect_true(any(grepl("^Crude: B$", names(out))))
  expect_true(any(grepl("^Adjusted: B$", names(out))))
})

test_that("table_regression — nested = TRUE adds comparison block to footer", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2), nested = TRUE)
  expect_match(attr(out, "title"), "^Hierarchical regression")
  expect_match(attr(out, "note"), "── Model comparison ──")
  expect_match(attr(out, "note"), "Model 2 vs Model 1")
})


# ============================================================================
# Output dispatch — non-default formats
# ============================================================================

test_that("output = 'data.frame' returns plain data.frame", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, output = "data.frame")
  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "spicy_regression_table"))
})

test_that("output = 'long' returns broom-style long format", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, output = "long")
  expect_s3_class(out, "data.frame")
  expect_true(all(c("model_id", "term", "estimate", "std.error",
                    "conf.low", "conf.high", "p.value")
                  %in% names(out)))
})

test_that("output = 'excel' without path errors with spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, output = "excel"),
    class = "spicy_invalid_input"
  )
})

test_that("output = 'word' without path errors with spicy_invalid_input", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, output = "word"),
    class = "spicy_invalid_input"
  )
})

test_that("output = 'gt' returns a gt_tbl object when gt installed", {
  skip_if_not_installed("gt")
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, output = "gt")
  expect_s3_class(out, "gt_tbl")
})

test_that("output = 'flextable' returns a flextable object when installed", {
  skip_if_not_installed("flextable")
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, output = "flextable")
  expect_s3_class(out, "flextable")
})

test_that("output = 'tinytable' returns a tinytable object when installed", {
  skip_if_not_installed("tinytable")
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, output = "tinytable")
  # tinytable mixes S3 and S4 across versions; inherits() works for both.
  expect_true(inherits(out, "tinytable"))
})


# ============================================================================
# Validation guarantees
# ============================================================================

test_that("table_regression — NULL models errors with spicy_invalid_input", {
  expect_error(
    table_regression(NULL),
    class = "spicy_invalid_input"
  )
})

test_that("table_regression — glm errors with spicy_unsupported (Phase 1 = lm only)", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  expect_error(
    table_regression(fit),
    class = "spicy_unsupported"
  )
})


# ============================================================================
# Print method
# ============================================================================

test_that("print.spicy_regression_table — invisible return + non-empty stdout", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  txt <- capture.output(p <- print(out))
  expect_identical(p, out)            # invisible(x) returns x
  expect_true(any(nzchar(txt)))
  expect_true(any(grepl("Variable", txt)))
})
