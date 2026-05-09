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


# ============================================================================
# Polish round 2 — Q1/Q2 conflict warnings, stars validation, AME naming
# ============================================================================

test_that("AME — binary numeric var keeps the var name (not '<var>1')", {
  # mtcars$am is 0/1 INTEGER (not factor). marginaleffects returns
  # the contrast string "1 - 0" for binary numerics; we must not
  # mistake that for a factor and concatenate `am1`. The AME row
  # must align with the B coef row, both keyed `am`.
  fit <- lm(mpg ~ wt + am, data = mtcars)
  td <- broom::tidy(table_regression(fit, show_columns = c("B", "AME")))
  ame <- td[td$estimate_type == "AME", ]
  expect_true("am" %in% ame$term)
  expect_false("am1" %in% ame$term)
})

test_that("AME — true factor still gets <var><level> naming", {
  # Sanity: factor predictors must still produce <var><level> AME
  # rows so they align with the B coef rows (cyl6, cyl8).
  fit <- lm(mpg ~ wt + cyl, data = mt)
  td <- broom::tidy(table_regression(fit, show_columns = c("B", "AME")))
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
    table_regression(fit, show_columns = c("B", "BOGUS")),
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

test_that("outcome_labels — multi-model differing DVs: NULL shows the row with auto labels", {
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp  <- lm(hp  ~ wt, data = mt)
  out <- table_regression(list(m_mpg, m_hp))
  expect_true("Outcome" %in% out$Variable)
  outcome_row <- out[out$Variable == "Outcome", , drop = FALSE]
  # Auto labels = variable names; in M1 first col, in M2 first col.
  expect_true(any(grepl("mpg", outcome_row[, "Model 1: B"])))
  expect_true(any(grepl("hp",  outcome_row[, "Model 2: B"])))
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
