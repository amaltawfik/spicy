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
  expect_match(attr(out, "title"), "^Linear regression: mpg")
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
  # trim padding from default decimal alignment before comparison
  expect_true(all(trimws(unlist(out[ref_idx, stat_cols])) == "—"))
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
    table_regression(fit, show_columns = c("b", "beta")),
    class = "spicy_invalid_input"
  )
})

test_that("table_regression — partial_eta2 + partial_eta2_ci render as atomic columns", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit,
                          show_columns = c("b", "partial_eta2",
                                            "partial_eta2_ci"))
  wt_row <- out[out$Variable == "wt", , drop = FALSE]
  # `partial_eta2` is the estimate-only cell (no brackets); the CI
  # is in its own column under "η² 95% CI".
  expect_match(trimws(wt_row$`η²`), "^[0-9]+\\.[0-9]+$")
  expect_match(trimws(wt_row$`η² 95% CI`), "^\\[.*\\]$")
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
  expect_match(attr(out, "title"), "^Hierarchical linear regression")
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

test_that("table_regression — merMod-like errors with spicy_unsupported (mixed-effects on roadmap for 0.16+)", {
  # As of spicy 0.13, lm + glm are supported (Phase 3); mixed-
  # effects models remain rejected with a roadmap pointer.
  fake <- structure(list(), class = c("lmerMod", "merMod"))
  expect_error(
    table_regression(fake),
    class = "spicy_unsupported"
  )
})

test_that("table_regression — binomial glm fits cleanly (Phase 3)", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  out <- table_regression(fit)
  expect_s3_class(out, "spicy_regression_table")
  expect_match(attr(out, "title"), "^Logistic regression: am$")
  td <- broom::tidy(out)
  # z-asymptotic inference per glm convention (summary.glm,
  # parameters::model_parameters, Stata logit, SPSS LOGISTIC)
  expect_true(all(td$test_type == "z"))
  expect_true(all(is.infinite(td$df)))
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


# ============================================================================
# Polish round 2 — Q1/Q2 conflict warnings, stars validation, AME naming
# ============================================================================

test_that("AME — binary numeric var keeps the var name (not '<var>1')", {
  # mtcars$am is 0/1 INTEGER (not factor). marginaleffects returns
  # the contrast string "1 - 0" for binary numerics; we must not
  # mistake that for a factor and concatenate `am1`. The AME row
  # must align with the B coef row, both keyed `am`.
  fit <- lm(mpg ~ wt + am, data = mtcars)
  td <- broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
  ame <- td[td$estimate_type == "AME", ]
  expect_true("am" %in% ame$term)
  expect_false("am1" %in% ame$term)
})

test_that("AME — true factor still gets <var><level> naming", {
  # Sanity: factor predictors must still produce <var><level> AME
  # rows so they align with the B coef rows (cyl6, cyl8).
  fit <- lm(mpg ~ wt + cyl, data = mt)
  td <- broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
  ame_terms <- td$term[td$estimate_type == "AME"]
  expect_true(all(c("cyl6", "cyl8") %in% ame_terms))
})

test_that("Q1 — names(list) + model_labels conflict warns spicy_ignored_arg", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_warning(
    table_regression(list(A = m1, B = m2), model_labels = c("X", "Y")),
    class = "spicy_ignored_arg"
  )
})

test_that("Q1 — names(list) alone (no explicit model_labels): no warning", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_no_warning(
    table_regression(list(A = m1, B = m2)),
    class = "spicy_ignored_arg"
  )
})

test_that("Q2 — show_intercept=FALSE + non-default intercept_position warns", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_warning(
    table_regression(fit, show_intercept = FALSE,
                     intercept_position = "last"),
    class = "spicy_ignored_arg"
  )
})

test_that("Q2 — show_intercept=FALSE + default intercept_position: no warning", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_no_warning(
    table_regression(fit, show_intercept = FALSE),
    class = "spicy_ignored_arg"
  )
})

test_that("stars validation — empty numeric errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit,
                     stars = setNames(numeric(0), character(0))),
    class = "spicy_invalid_input"
  )
})

test_that("stars validation — out-of-range threshold errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, stars = c("*" = 0, "**" = -0.01)),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, stars = c("*" = 1.5)),
    class = "spicy_invalid_input"
  )
})

test_that("stars validation — empty / unnamed name errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, stars = c(0.05, 0.01)),       # no names
    class = "spicy_invalid_input"
  )
  bad <- setNames(c(0.05, 0.01), c("*", ""))
  expect_error(
    table_regression(fit, stars = bad),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# Polish round 3 — full Q21 validation cascade wired into orchestrator
# ============================================================================

test_that("vcov — unknown type errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, vcov = "HC99"),
    class = "spicy_invalid_input"
  )
})

test_that("vcov — CR* without cluster errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, vcov = "CR2"),
    class = "spicy_invalid_input"
  )
})

test_that("vcov — list length mismatch errors spicy_invalid_input", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(list(m1, m2), vcov = list("classical")),
    class = "spicy_invalid_input"
  )
})

test_that("ci_level — out of range errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, ci_level = 1.5),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, ci_level = 0),
    class = "spicy_invalid_input"
  )
})

test_that("show_columns — unknown token errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, show_columns = c("b", "BOGUS")),
    class = "spicy_invalid_input"
  )
})

test_that("show_columns — empty errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, show_columns = character(0)),
    class = "spicy_invalid_input"
  )
})

test_that("show_fit_stats — unknown token errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "nope")),
    class = "spicy_invalid_input"
  )
})

test_that("nested_stats — unknown token errors spicy_invalid_input", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(list(m1, m2), nested = TRUE,
                     nested_stats = c("F", "BOGUS")),
    class = "spicy_invalid_input"
  )
})

test_that("digit args — non-positive integer errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, digits = -1L),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, p_digits = "three"),
    class = "spicy_invalid_input"
  )
})

test_that("decimal_mark — multi-character errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, decimal_mark = "..."),
    class = "spicy_invalid_input"
  )
})

test_that("reference_label — empty string errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(fit, reference_label = ""),
    class = "spicy_invalid_input"
  )
})

test_that("model_labels — length mismatch errors spicy_invalid_input", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(list(m1, m2), model_labels = "only_one"),
    class = "spicy_invalid_input"
  )
})

test_that("outcome_labels — length mismatch errors spicy_invalid_input", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(list(m1, m2), outcome_labels = "only_one"),
    class = "spicy_invalid_input"
  )
})

test_that("labels — unknown predictor key errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit,
                     labels = c("nonexistent_predictor" = "Foo")),
    class = "spicy_invalid_input"
  )
})

test_that("nested = TRUE — different nobs errors spicy_invalid_input", {
  m1 <- lm(mpg ~ wt, data = mt)
  # Drop one row to force nobs mismatch
  m2 <- lm(mpg ~ wt + cyl, data = mt[-1, ])
  expect_error(
    table_regression(list(m1, m2), nested = TRUE),
    class = "spicy_invalid_input"
  )
})

test_that("nested = TRUE — different DV errors spicy_invalid_input", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(hp ~ wt + cyl, data = mt)
  expect_error(
    table_regression(list(m1, m2), nested = TRUE),
    class = "spicy_invalid_input"
  )
})

test_that("output = 'excel' with non-existent dir errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, output = "excel",
                     excel_path = "/no/such/directory/out.xlsx"),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# Polish round 4 — show_fit_stats footer block, labels for coef-style
# names, align argument
# ============================================================================

test_that("show_fit_stats — default tokens (n / R² / Adj.R²) appear in body", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  expect_true("n" %in% out$Variable)
  expect_true("R²" %in% out$Variable)
  expect_true("Adj.R²" %in% out$Variable)
})

test_that("show_fit_stats — custom tokens (omega2, sigma, AIC) appear", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit,
                          show_fit_stats = c("nobs", "omega2",
                                              "sigma", "AIC"))
  expect_true("ω²" %in% out$Variable)
  expect_true("σ̂" %in% out$Variable)
  expect_true("AIC" %in% out$Variable)
})

test_that("show_fit_stats — empty character drops the footer block", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, show_fit_stats = character(0))
  expect_false(any(c("n", "R²", "Adj.R²") %in% out$Variable))
})

test_that("show_fit_stats — multi-model: each model contributes its values", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2),
                          show_fit_stats = c("nobs", "r2"))
  # Both n rows present; R² differs across the two model columns
  expect_true("n" %in% out$Variable)
  expect_true("R²" %in% out$Variable)
  r2_row <- out[out$Variable == "R²", , drop = FALSE]
  m1_first <- trimws(r2_row[["Model 1: B"]])
  m2_first <- trimws(r2_row[["Model 2: B"]])
  expect_true(nzchar(m1_first))
  expect_true(nzchar(m2_first))
  expect_false(identical(m1_first, m2_first))
})

test_that("group_sep_rows attribute marks the body / fit-stats divider", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  sep <- attr(out, "group_sep_rows")
  expect_true(length(sep) == 1L && sep > 0L)
  # Separator points to the row right after the body ends
  expect_equal(out$Variable[sep], "n")
})

test_that("labels — coef-style key (cyl6) renames the contrast row", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit,
                          labels = c("cyl6" = "6 cylinders",
                                      "cyl8" = "8 cylinders"))
  # Indented level rows are renamed
  expect_true(any(grepl("6 cylinders$", out$Variable)))
  expect_true(any(grepl("8 cylinders$", out$Variable)))
  # Factor header still uses the term name "cyl"
  expect_true("cyl:" %in% out$Variable)
})

test_that("labels — mixed term + coef-style keys both honoured", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit,
                          labels = c("cyl" = "Cylinders",
                                      "cyl6" = "Six",
                                      "wt" = "Weight"))
  expect_true("Weight" %in% out$Variable)
  expect_true("Cylinders:" %in% out$Variable)
  expect_true(any(grepl("Six$", out$Variable)))
})

test_that("align — 'decimal' is default; padding applied to numeric cols", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  expect_equal(attr(out, "align"), "decimal")
  # Decimal-aligned strings are pre-padded → cells contain spaces
  b_col <- out$B
  # At least some cells start or end with a space (padding)
  expect_true(any(grepl("^[ ].*", b_col) | grepl(".*[ ]$", b_col)))
})

test_that("align — 'auto' / 'right' / 'center' are accepted (no decimal pad)", {
  fit <- lm(mpg ~ wt, data = mt)
  for (a in c("center", "right", "auto")) {
    out <- table_regression(fit, align = a)
    expect_equal(attr(out, "align"), a)
  }
})

test_that("print — align = 'center' propagates to align_center_cols", {
  # Exercises the data_col_idx branch in print.spicy_regression_table
  # when align is "center" (vs "decimal" / "right" / "auto").
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, align = "center")
  txt <- capture.output(print(out))
  expect_true(any(nzchar(txt)))
})


# ============================================================================
# Final-audit fixes: duplicate-name validation, outcome attr label,
# cluster name detection via NSE
# ============================================================================

test_that("duplicate names in `list(...)` error spicy_invalid_input", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(list(M1 = m1, M1 = m2)),
    class = "spicy_invalid_input"
  )
})

test_that("duplicate values in `model_labels` error spicy_invalid_input", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(list(m1, m2),
                     model_labels = c("Same", "Same")),
    class = "spicy_invalid_input"
  )
})

test_that("DV smart spanner uses the bare variable name (NOT attr('label'))", {
  # With distinct DVs and no explicit labels, the response variable
  # NAME is lifted into the spanner -- not `attr("label")`, which
  # can be a long phrase that would distort column widths. The
  # Outcome body row is suppressed (info is in the header).
  df <- data.frame(y = rnorm(50), x = rnorm(50))
  attr(df$y, "label") <- "Wellbeing score (0-100)"   # NOT used
  fit_a <- lm(y ~ x, data = df)
  fit_b <- lm(x ~ y, data = df)

  out <- table_regression(list(fit_a, fit_b))
  expect_false("Outcome" %in% out$Variable)
  spans <- attr(out, "spanners")
  expect_equal(names(spans), c("y", "x"))
})

test_that("outcome auto-row identical-DV check uses variable name (not label)", {
  # If two fits share the same response variable (same column),
  # the row must still be hidden even if labels happen to differ.
  df <- data.frame(y = rnorm(50), x = rnorm(50))
  attr(df$y, "label") <- "Y"
  fit1 <- lm(y ~ x, data = df)
  fit2 <- lm(y ~ I(x^2), data = df)
  out <- table_regression(list(fit1, fit2))
  expect_false("Outcome" %in% out$Variable)
})

test_that("cluster_name — `df$col` extracted to 'col' for the footer", {
  skip_if_not_installed("clubSandwich")
  set.seed(1)
  df <- data.frame(
    y = rnorm(120), x = rnorm(120),
    region = factor(sample(letters[1:6], 120, replace = TRUE))
  )
  fit <- lm(y ~ x, data = df)
  out <- table_regression(fit, vcov = "CR2", cluster = df$region)
  expect_match(attr(out, "note"), "clusters by region")
  expect_no_match(attr(out, "note"), "cluster vector supplied")
})

test_that("cluster_name — bare symbol extracted as variable name", {
  skip_if_not_installed("clubSandwich")
  set.seed(2)
  df <- data.frame(
    y = rnorm(120), x = rnorm(120),
    region = factor(sample(letters[1:6], 120, replace = TRUE))
  )
  region_vec <- df$region
  fit <- lm(y ~ x, data = df)
  out <- table_regression(fit, vcov = "CR2", cluster = region_vec)
  expect_match(attr(out, "note"), "clusters by region_vec")
})

test_that("cluster_name — list(...) with named elements per model", {
  skip_if_not_installed("clubSandwich")
  set.seed(3)
  df <- data.frame(
    y = rnorm(120), x = rnorm(120),
    region = factor(sample(letters[1:6], 120, replace = TRUE)),
    clinic = factor(sample(LETTERS[1:5], 120, replace = TRUE))
  )
  m1 <- lm(y ~ x, data = df)
  m2 <- lm(y ~ x, data = df)
  out <- table_regression(
    list(m1, m2),
    vcov = list("CR2", "CR2"),
    cluster = list(df$region, df$clinic)
  )
  note <- attr(out, "note")
  expect_match(note, "clusters by region")
  expect_match(note, "clusters by clinic")
})

test_that("extract_arg_column_name — handles all recognised forms", {
  expect_equal(spicy:::extract_arg_column_name(quote(df$col)), "col")
  expect_equal(spicy:::extract_arg_column_name(quote(df[["col"]])), "col")
  expect_equal(spicy:::extract_arg_column_name(quote(mycluster)), "mycluster")
  expect_true(is.na(spicy:::extract_arg_column_name(quote(c(1, 2, 3)))))
  expect_true(is.na(spicy:::extract_arg_column_name(NULL)))
})


# ============================================================================
# Polish round 5 — outcome_labels (Q11b) and reference_style annotation (Q5)
# ============================================================================

test_that("outcome_labels — single model: row never shown (DV is in title)", {
  fit <- lm(mpg ~ wt, data = mt)
  out_null <- table_regression(fit, outcome_labels = NULL)
  out_chr  <- table_regression(fit, outcome_labels = "Custom")
  out_F    <- table_regression(fit, outcome_labels = FALSE)
  expect_false("Outcome" %in% out_null$Variable)
  expect_false("Outcome" %in% out_chr$Variable)
  expect_false("Outcome" %in% out_F$Variable)
})

test_that("outcome_labels — multi-model identical DVs: NULL hides the row", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2))
  expect_false("Outcome" %in% out$Variable)
})

test_that("outcome_labels — multi-model differing DVs: NULL lifts DVs into spanner", {
  # When `outcome_labels = NULL` (default) and DVs differ, the
  # smart default moves the auto-detected DV names into the
  # multi-model spanner and suppresses the body Outcome row.
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp  <- lm(hp  ~ wt, data = mt)
  out <- table_regression(list(m_mpg, m_hp))
  expect_false("Outcome" %in% out$Variable)
  spans <- attr(out, "spanners")
  expect_equal(names(spans), c("mpg", "hp"))
})

test_that("outcome_labels — explicit labels take precedence", {
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp  <- lm(hp  ~ wt, data = mt)
  out <- table_regression(list(m_mpg, m_hp),
                          outcome_labels = c("Fuel economy", "Horsepower"))
  outcome_row <- out[out$Variable == "Outcome", , drop = FALSE]
  expect_true(any(grepl("Fuel economy", outcome_row[, "Model 1: B"])))
  expect_true(any(grepl("Horsepower",   outcome_row[, "Model 2: B"])))
})

test_that("outcome_labels — FALSE suppresses the row even with differing DVs", {
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp  <- lm(hp  ~ wt, data = mt)
  out <- table_regression(list(m_mpg, m_hp), outcome_labels = FALSE)
  expect_false("Outcome" %in% out$Variable)
})

test_that("reference_style = 'annotation' — factor header annotated [ref: <level>]", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, reference_style = "annotation")
  # Header now reads "cyl: [ref: 4]" (or whichever the ref level is)
  expect_true(any(grepl("^cyl: \\[ref: 4\\]$", out$Variable)))
  # And the orphan row "4 (ref.)" must NOT appear (it was dropped)
  expect_false(any(grepl("\\(ref\\.\\)", out$Variable)))
})

test_that("reference_style = 'row' (default) — no factor header annotation", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)   # default = "row"
  expect_true("cyl:" %in% out$Variable)
  expect_false(any(grepl("\\[ref: ", out$Variable)))
  expect_true(any(grepl("\\(ref\\.\\)", out$Variable)))
})

test_that("reference_style = 'annotation' — works with multiple factors", {
  mt2 <- mt
  mt2$gear <- factor(mt2$gear)
  fit <- lm(mpg ~ wt + cyl + gear, data = mt2)
  out <- table_regression(fit, reference_style = "annotation")
  expect_true(any(grepl("^cyl: \\[ref: 4\\]$",  out$Variable)))
  expect_true(any(grepl("^gear: \\[ref: 3\\]$", out$Variable)))
})


# ============================================================================
# Polish round 6 — no-intercept formula edge case (y ~ 0 + x)
# ============================================================================

test_that("no-intercept formula — first level of factor is a real coef, no phantom ref row", {
  fit <- lm(mpg ~ 0 + wt + cyl, data = mt)
  out <- table_regression(fit)
  # No "(ref.)" suffix anywhere — all 3 cyl levels are fitted coefs
  expect_false(any(grepl("\\(ref\\.\\)", out$Variable)))
  # cyl4 / cyl6 / cyl8 all displayed (indented under "cyl:")
  expect_true(any(grepl("^  4$", out$Variable)))
  expect_true(any(grepl("^  6$", out$Variable)))
  expect_true(any(grepl("^  8$", out$Variable)))
  # No standalone "cyl4" row outside the factor group
  expect_false(any(grepl("^cyl4$", out$Variable)))
})

test_that("no-intercept formula — tidy returns all factor coefs", {
  fit <- lm(mpg ~ 0 + wt + cyl, data = mt)
  td <- broom::tidy(table_regression(fit))
  # Expect 4 B rows (wt, cyl4, cyl6, cyl8)
  b_rows <- td[td$estimate_type == "B", ]
  expect_equal(nrow(b_rows), 4L)
  expect_setequal(b_rows$term, c("wt", "cyl4", "cyl6", "cyl8"))
  # cyl4 estimate is real, NOT NA
  expect_true(is.finite(b_rows$estimate[b_rows$term == "cyl4"]))
  # No is_intercept = TRUE row
  expect_false(any(td$is_intercept))
})

test_that("no-intercept formula — alt syntax 'y ~ x - 1' is equivalent", {
  fit_a <- lm(mpg ~ 0 + wt, data = mt)
  fit_b <- lm(mpg ~ wt - 1, data = mt)
  out_a <- table_regression(fit_a)
  out_b <- table_regression(fit_b)
  expect_equal(out_a$B, out_b$B)
})

test_that("no-intercept formula — works with multi-model nested lookalike", {
  m_with_int <- lm(mpg ~ wt + cyl, data = mt)
  m_no_int   <- lm(mpg ~ 0 + wt + cyl, data = mt)
  # Side-by-side display (NOT nested — these aren't nested in the
  # likelihood sense). Validate that the rendering does not crash.
  out <- table_regression(list(m_with_int, m_no_int))
  expect_s3_class(out, "spicy_regression_table")
  # Reference-row from M1 still shown for M2 the same factor group
  expect_true(any(grepl("\\(ref\\.\\)", out$Variable)))
})


# ============================================================================
# Snapshot tests — golden output for the most common rendering paths
# ============================================================================

# Snapshot helper: capture the printed output and normalise trailing
# whitespace + the trailing blank line. spicy_print_table() pads
# every cell to a fixed column width, so harmless changes in the
# longest cell propagate as cascade-y diffs in the snapshot. Trimming
# trailing whitespace per line keeps the snapshot stable as long as
# the SEMANTIC content (cells + alignment) is unchanged.
capture_norm <- function(out) {
  txt <- capture.output(print(out))
  txt <- sub("[ \t]+$", "", txt)         # trim trailing whitespace
  paste(txt, collapse = "\n")
}

test_that("snapshot — single lm default rendering", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  expect_snapshot(cat(capture_norm(out)))
})

test_that("snapshot — multi-model with nested = TRUE comparison footer", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2), nested = TRUE)
  expect_snapshot(cat(capture_norm(out)))
})

test_that("snapshot — standardized + stars + reference annotation", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, standardized = "refit", stars = TRUE,
                          reference_style = "annotation")
  expect_snapshot(cat(capture_norm(out)))
})


# ============================================================================
# Multi-model column spanners (model name above each model's sub-columns)
# ============================================================================

test_that("spanner — single model: no spanner attribute", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  expect_null(attr(out, "spanners"))
})

test_that("spanner — multi-model named list: names become spanner labels", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list("Step 1" = m1, "Step 2" = m2))
  spans <- attr(out, "spanners")
  expect_equal(names(spans), c("Step 1", "Step 2"))
  # Multi-model context-aware default drops CI: each model owns
  # 3 contiguous sub-columns (B / SE / p). Restore CI explicitly
  # via show_columns when needed.
  expect_equal(spans[["Step 1"]], 2:4)
  expect_equal(spans[["Step 2"]], 5:7)
})

test_that("spanner — multi-model unnamed + same DV: 'Model N' labels", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2))
  spans <- attr(out, "spanners")
  expect_equal(names(spans), c("Model 1", "Model 2"))
})

test_that("spanner — multi-model unnamed + distinct DVs: DV smart default", {
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp  <- lm(hp  ~ wt, data = mt)
  out <- table_regression(list(m_mpg, m_hp))
  spans <- attr(out, "spanners")
  expect_equal(names(spans), c("mpg", "hp"))
  # Outcome row is folded into the spanner -> not in the body.
  expect_false("Outcome" %in% out$Variable)
})

test_that("spanner — explicit model_labels override DV smart default", {
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp  <- lm(hp  ~ wt, data = mt)
  out <- table_regression(list(m_mpg, m_hp),
                          model_labels = c("Fuel", "Power"))
  spans <- attr(out, "spanners")
  expect_equal(names(spans), c("Fuel", "Power"))
})

test_that("spanner — explicit outcome_labels keep the row; spanner stays generic", {
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp  <- lm(hp  ~ wt, data = mt)
  out <- table_regression(list(m_mpg, m_hp),
                          outcome_labels = c("Fuel economy", "Horsepower"))
  spans <- attr(out, "spanners")
  expect_equal(names(spans), c("Model 1", "Model 2"))
  expect_true("Outcome" %in% out$Variable)
})

test_that("spanner — multi-model print strips 'Label: ' prefix from headers", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list("A" = m1, "B" = m2))
  txt <- capture.output(print(out))
  joined <- paste(txt, collapse = "\n")
  # The spanner labels appear above the sub-columns.
  expect_match(joined, "A")
  expect_match(joined, "B")
  # The bare sub-column tokens are shown twice (one per model);
  # the "A: B" / "B: B" prefixed form must not appear in the header.
  expect_false(grepl("A: B", joined, fixed = TRUE))
})

test_that("spanner — .validate_spanners catches malformed input", {
  df <- data.frame(a = 1, b = 2, c = 3, d = 4)
  expect_error(
    build_ascii_table(df, spanners = list(2:3)),     # unnamed
    class = "spicy_invalid_input"
  )
  expect_error(
    build_ascii_table(df, spanners = list(g = 5:6)), # out of range
    class = "spicy_invalid_input"
  )
  expect_error(
    build_ascii_table(df, spanners = list(g = c(2, 4))),  # non-contiguous
    class = "spicy_invalid_input"
  )
  expect_error(
    build_ascii_table(df, spanners = list(a = 2:3, b = 3:4)), # overlapping
    class = "spicy_invalid_input"
  )
})

test_that("spanner — gt output applies tab_spanner + cols_label", {
  skip_if_not_installed("gt")
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  g <- table_regression(list("A" = m1, "B" = m2), output = "gt")
  html <- as.character(gt::as_raw_html(g))
  expect_match(html, ">A<")           # spanner label present
  expect_match(html, ">B<")
  # Bare sub-column labels are used (cols_label stripped the prefix).
  # ">B<" matches both the spanner "B" and the bare-token "B"; ensure
  # the prefixed "A: B" form does NOT appear as a rendered label.
  expect_false(grepl(">A: B<", html, fixed = TRUE))
})

test_that("spanner — flextable output adds a header row with spanners", {
  skip_if_not_installed("flextable")
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  f <- table_regression(list("A" = m1, "B" = m2), output = "flextable")
  hdr <- f$header$dataset
  expect_equal(nrow(hdr), 2L)                          # spanner + sub-cols
  expect_true(any(unlist(hdr[1, ]) == "A"))
  expect_true(any(unlist(hdr[1, ]) == "B"))
})

test_that("ordered factor — grouped under header, poly-order, auto footer note", {
  set.seed(1)
  df <- data.frame(
    y   = rnorm(200),
    x   = rnorm(200),
    edu = ordered(sample(c("Low", "Med", "High", "Top"), 200,
                          replace = TRUE),
                  levels = c("Low", "Med", "High", "Top"))
  )
  fit <- lm(y ~ x + edu, df)
  out <- table_regression(fit)
  vars <- trimws(as.data.frame(out, stringsAsFactors = FALSE)$Variable)
  expect_true("edu:" %in% vars)
  # Poly-order: .L < .Q < .C (alphabetical sort would yield .C first).
  l_pos <- which(vars == ".L")
  q_pos <- which(vars == ".Q")
  c_pos <- which(vars == ".C")
  expect_true(l_pos < q_pos)
  expect_true(q_pos < c_pos)
  # No reference row (poly contrasts have none).
  expect_false(any(grepl("(ref.)", vars, fixed = TRUE)))
  # Auto footer carries the compact publication-grade note +
  # suffix legend (no "R contr.poly" leak, no "linear trend" prose
  # -- that explanation lives in the once-per-session inform).
  note <- attr(out, "note")
  expect_match(note, "Ordered factor `edu`: polynomial trends")
  expect_match(note, ".L = linear", fixed = TRUE)
  expect_match(note, ".Q = quadratic", fixed = TRUE)
})

test_that("ordered factor — fitting with factor(ordered = FALSE) restores treatment layout", {
  set.seed(1)
  df <- data.frame(
    y   = rnorm(200),
    edu = ordered(sample(c("Low", "Med", "High"), 200, replace = TRUE),
                  levels = c("Low", "Med", "High"))
  )
  df$edu_t <- factor(df$edu, ordered = FALSE)
  fit <- lm(y ~ edu_t, df)
  out <- table_regression(fit)
  vars <- trimws(as.data.frame(out, stringsAsFactors = FALSE)$Variable)
  expect_true("edu_t:" %in% vars)
  expect_true(any(grepl("Low (ref.)", vars, fixed = TRUE)))
  # No poly footer for this fit.
  note <- attr(out, "note")
  expect_false(grepl("polynomial trends", note))
})


test_that("default show_columns context-aware: single keeps CI, multi drops it", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out1 <- table_regression(m1)
  expect_true(any(grepl("95% CI", names(out1))))
  out2 <- table_regression(list(m1, m2))
  expect_false(any(grepl("95% CI", names(out2))))
  # Explicit user override restores CI even in multi-model.
  out3 <- table_regression(list(m1, m2),
                            show_columns = c("b", "se", "ci", "p"))
  expect_true(any(grepl("95% CI", names(out3))))
})


test_that("outcome_labels — NULL hides the row even when DVs differ + names supplied", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(hp  ~ wt, data = mt)
  out <- table_regression(list("Step 1" = m1, "Step 2" = m2))
  expect_false("Outcome" %in% out$Variable)
  # Explicit opt-in still works.
  out2 <- table_regression(list("Step 1" = m1, "Step 2" = m2),
                            outcome_labels = c("Fuel", "Power"))
  expect_true("Outcome" %in% out2$Variable)
})


test_that("spanner — tinytable output uses group_tt for column groups", {
  skip_if_not_installed("tinytable")
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  tt <- table_regression(list("A" = m1, "B" = m2), output = "tinytable")
  # tinytable renders the spanner row inside the printed markdown.
  txt <- capture.output(print(tt, output = "markdown"))
  joined <- paste(txt, collapse = "\n")
  # The spanner labels appear above the sub-column headers.
  expect_match(joined, "| A ", fixed = TRUE)
  expect_match(joined, "| B ", fixed = TRUE)
})


test_that("reference row always FIRST in its factor group regardless of group_factor_levels", {
  # Reference-row position must be deterministic across the
  # group_factor_levels toggle. Pre-fix, FALSE put the ref AFTER
  # the active dummies (because build_reference_rows appends them
  # at the end of coef order and group_factor_terms was gated on
  # TRUE). Now we always reorder so the ref is first in its group.
  df <- data.frame(
    y   = rnorm(200),
    age = rnorm(200),
    sex = factor(sample(c("Female", "Male"), 200, replace = TRUE),
                 levels = c("Female", "Male"))
  )
  fit <- lm(y ~ age + sex, df)

  out_t <- table_regression(fit, factor_layout = "grouped")
  vars_t <- as.data.frame(out_t, stringsAsFactors = FALSE)$Variable
  ref_t  <- which(grepl("Female (ref.)", vars_t, fixed = TRUE))
  male_t <- which(vars_t == "  Male")
  expect_true(ref_t < male_t)

  out_f <- table_regression(fit, factor_layout = "flat")
  vars_f <- as.data.frame(out_f, stringsAsFactors = FALSE)$Variable
  ref_f  <- which(grepl("sexFemale (ref.)", vars_f, fixed = TRUE))
  male_f <- which(vars_f == "sexMale")
  expect_true(ref_f < male_f)
})


# ============================================================================
# reference_style = "annotation" / "footer" / "none" + factor_layout enum
# ============================================================================

test_that("reference_style = \"annotation\" + factor_layout = \"flat\" inlines [vs <ref>] on 1st dummy only", {
  df <- data.frame(
    y   = rnorm(200),
    sex = factor(sample(c("Female", "Male"), 200, replace = TRUE),
                 levels = c("Female", "Male")),
    edu = factor(sample(c("Lower", "Upper", "Tertiary"), 200, replace = TRUE),
                 levels = c("Lower", "Upper", "Tertiary"))
  )
  fit <- lm(y ~ sex + edu, df)
  out <- table_regression(fit, reference_style = "annotation",
                          factor_layout = "flat")
  vars <- as.data.frame(out, stringsAsFactors = FALSE)$Variable
  # 2-level factor: the single non-ref dummy carries [vs Female]
  expect_true(any(grepl("sexMale [vs Female]", vars, fixed = TRUE)))
  # 3-level factor: FIRST non-ref dummy carries [vs Lower]; second does not
  edu_rows <- grep("^edu", vars, value = TRUE)
  with_marker <- grep("[vs Lower]", edu_rows, fixed = TRUE, value = TRUE)
  expect_length(with_marker, 1L)              # exactly one
  # Reference rows themselves are NOT in the body in annotation mode
  expect_false(any(grepl("Lower (ref.)", vars, fixed = TRUE)))
})

test_that("reference_style = \"footer\" adds a single 'Reference categories: ...' line", {
  df <- data.frame(
    y   = rnorm(200),
    sex = factor(sample(c("Female", "Male"), 200, replace = TRUE),
                 levels = c("Female", "Male")),
    edu = factor(sample(c("Lower", "Upper", "Tertiary"), 200, replace = TRUE),
                 levels = c("Lower", "Upper", "Tertiary"))
  )
  fit <- lm(y ~ sex + edu, df)
  out <- table_regression(fit, reference_style = "footer")
  vars <- as.data.frame(out, stringsAsFactors = FALSE)$Variable
  # Ref rows dropped from body; no inline annotation
  expect_false(any(grepl("(ref.)", vars, fixed = TRUE)))
  expect_false(any(grepl("[vs ", vars, fixed = TRUE)))
  # Footer line lists both factor references
  note <- attr(out, "note")
  expect_match(note, "Reference categories:")
  expect_match(note, "sex = Female")
  expect_match(note, "edu = Lower")
})

test_that("reference_style = \"none\" shows no reference info anywhere", {
  df <- data.frame(
    y   = rnorm(200),
    sex = factor(sample(c("Female", "Male"), 200, replace = TRUE),
                 levels = c("Female", "Male"))
  )
  fit <- lm(y ~ sex, df)
  # Suppress the spicy_inform emitted on flat+none; we test that
  # separately below.
  withCallingHandlers(
    out <- table_regression(fit, reference_style = "none",
                            factor_layout = "flat"),
    spicy_info = function(c) invokeRestart("muffleMessage")
  )
  vars <- as.data.frame(out, stringsAsFactors = FALSE)$Variable
  expect_false(any(grepl("(ref.)", vars, fixed = TRUE)))
  expect_false(any(grepl("[vs ", vars, fixed = TRUE)))
  # Footer carries the regression-type / vcov declaration but
  # nothing about references.
  expect_no_match(attr(out, "note"), "Reference categories")
})

test_that("reference_style = \"none\" + factor_layout = \"flat\" emits spicy_inform once", {
  df <- data.frame(
    y   = rnorm(100),
    sex = factor(sample(c("Female", "Male"), 100, replace = TRUE))
  )
  fit <- lm(y ~ sex, df)
  cnd <- NULL
  withCallingHandlers(
    table_regression(fit, reference_style = "none",
                     factor_layout = "flat"),
    spicy_info = function(c) {
      cnd <<- c
      invokeRestart("muffleMessage")
    }
  )
  expect_s3_class(cnd, "spicy_silent_reference")
  expect_s3_class(cnd, "spicy_info")
  expect_match(conditionMessage(cnd), "reference convention")
})

test_that("reference_style = \"none\" + factor_layout = \"grouped\" does NOT emit the info", {
  # Grouped still shows the `education:` header, so the silent-loss
  # warning is unnecessary -- only the FLAT case loses all visual
  # trace of the factor's existence beyond per-level dummies.
  df <- data.frame(
    y   = rnorm(100),
    sex = factor(sample(c("Female", "Male"), 100, replace = TRUE))
  )
  fit <- lm(y ~ sex, df)
  cnd <- NULL
  withCallingHandlers(
    table_regression(fit, reference_style = "none",
                     factor_layout = "grouped"),
    spicy_info = function(c) {
      cnd <<- c
      invokeRestart("muffleMessage")
    }
  )
  expect_null(cnd)
})

test_that("reference_style = \"none\" with NO factors: no spicy_inform (nothing to lose)", {
  fit <- lm(mpg ~ wt, data = mt)
  cnd <- NULL
  withCallingHandlers(
    table_regression(fit, reference_style = "none",
                     factor_layout = "flat"),
    spicy_info = function(c) {
      cnd <<- c
      invokeRestart("muffleMessage")
    }
  )
  expect_null(cnd)
})

test_that("factor_layout = \"flat\" produces concatenated <var><level> labels", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, factor_layout = "flat")
  vars <- as.data.frame(out, stringsAsFactors = FALSE)$Variable
  # No "cyl:" factor header in flat mode
  expect_false("cyl:" %in% vars)
  # Each level row uses the concatenated `<var><level>` form
  expect_true(any(grepl("^cyl6$", vars)))
  expect_true(any(grepl("^cyl8$", vars)))
})

test_that("factor_layout = \"grouped\" (default) inserts factor header + indents", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)   # default grouped
  vars <- as.data.frame(out, stringsAsFactors = FALSE)$Variable
  expect_true("cyl:" %in% vars)
  expect_true(any(grepl("^  6$", vars)))
  expect_true(any(grepl("^  8$", vars)))
})
