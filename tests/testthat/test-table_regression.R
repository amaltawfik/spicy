# Integration tests for table_regression() – Phase 1 end-to-end.

mt <- mtcars
mt$cyl <- factor(mt$cyl)

# ============================================================================
# default output
# ============================================================================

test_that("table_regression – default output: spicy_regression_table class + attrs", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  expect_s3_class(out, "spicy_regression_table")
  expect_s3_class(out, "spicy_table")
  expect_s3_class(out, "data.frame")
  expect_identical(attr(out, "title"), "Linear regression: mpg")
  expect_identical(
    attr(out, "note"),
    "Note. Linear regression.\nStd. errors: classical (OLS)."
  )
})

test_that("table_regression – single fit and 1-list of fits behave the same", {
  fit <- lm(mpg ~ wt, data = mt)
  o1 <- table_regression(fit)
  o2 <- table_regression(list(fit))
  # Same body content (titles/labels may differ trivially)
  expect_equal(unname(unlist(o1$Variable)), unname(unlist(o2$Variable)))
})

test_that("table_regression – factor with reference: ref row en-dashed", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  ref_idx <- grep("\\(ref\\.\\)", out$Variable)
  expect_equal(length(ref_idx), 1L)
  stat_cols <- setdiff(names(out), "Variable")
  # trim padding from default decimal alignment before comparison
  expect_true(all(trimws(unlist(out[ref_idx, stat_cols])) == "–"))
})


# ============================================================================
# show_columns + standardized
# ============================================================================

test_that("table_regression – standardized != 'none' auto-injects 'beta'", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, standardized = "refit")
  expect_true("β" %in% names(out))
})

test_that("table_regression – 'beta' without standardized errors with spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, show_columns = c("b", "beta")),
    class = "spicy_invalid_input"
  )
})

test_that("table_regression – partial_eta2 + partial_eta2_ci render as atomic columns", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(
    fit,
    show_columns = c("b", "partial_eta2", "partial_eta2_ci")
  )
  wt_row <- out[out$Variable == "wt", , drop = FALSE]
  # `partial_eta2` is the estimate-only cell (no brackets); the CI
  # is in its own column under "η² 95% CI".
  expect_match(trimws(wt_row$`η²`), "^[0-9]+\\.[0-9]+$")
  expect_match(trimws(wt_row$`η² 95% CI`), "^\\[.*\\]$")
})


# ============================================================================
# stars (Q12)
# ============================================================================

test_that("table_regression – stars = TRUE applied to B (no β requested)", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, stars = TRUE)
  wt_row <- out[out$Variable == "wt", , drop = FALSE]
  expect_match(wt_row$B, "\\*\\*\\*$")
})

test_that("table_regression – stars on B when both B and β are shown", {
  # Convention aligned with SPSS, Stata `esttab`, SAS, and the R
  # ecosystem (modelsummary, gtsummary, parameters): stars are
  # anchored on the raw coefficient B. β is a deterministic
  # rescaling of B; its p-value is identical, so adding stars on
  # both columns would be redundant. β stays plain, B carries
  # the significance signal.
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, standardized = "refit", stars = TRUE)
  wt_row <- out[out$Variable == "wt", , drop = FALSE]
  expect_match(wt_row$B, "\\*\\*\\*$")
  # β should NOT carry the stars when B is shown.
  expect_false(grepl("\\*", wt_row$β))
})


# ============================================================================
# Multi-model + nested
# ============================================================================

test_that("table_regression – two models: per-model column groups", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2))
  expect_true("Model 1: B" %in% names(out))
  expect_true("Model 2: B" %in% names(out))
})

test_that("table_regression – names(list) used as model labels", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(Crude = m1, Adjusted = m2))
  expect_true(any(grepl("^Crude: B$", names(out))))
  expect_true(any(grepl("^Adjusted: B$", names(out))))
})

test_that("table_regression – nested = TRUE injects change-stat rows + Hierarchical title", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2), nested = TRUE)
  expect_identical(attr(out, "title"), "Hierarchical linear regression: mpg")
  vars <- trimws(as.data.frame(out, stringsAsFactors = FALSE)$Variable)
  expect_true("ΔR²" %in% vars)
  expect_true("F-change" %in% vars)
  expect_true("p (change)" %in% vars)
  # Old footer block is gone.
  expect_no_match(attr(out, "note"), "Model comparison")
})


# ============================================================================
# Output dispatch – non-default formats
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
  expect_true(all(
    c(
      "model_id",
      "term",
      "estimate",
      "std.error",
      "conf.low",
      "conf.high",
      "p.value"
    ) %in%
      names(out)
  ))
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

test_that("table_regression – NULL models errors with spicy_invalid_input", {
  expect_error(
    table_regression(NULL),
    class = "spicy_invalid_input"
  )
})

test_that("table_regression – class without as_regression_frame method errors with spicy_unsupported", {
  # Phase 1-6 added methods for ~35 classes including mixed-effects,
  # survival, ordinal, multinomial, robust, fixed-effects, GAM, rms,
  # Bayesian etc. Genuinely-unsupported off-roadmap classes still
  # error through the validate_models_input() gate.
  fake <- structure(list(), class = "rlmer_robustlmm")
  expect_error(
    table_regression(fake),
    class = "spicy_unsupported"
  )
})

test_that("table_regression – binomial glm fits cleanly (Phase 3)", {
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

test_that("print.spicy_regression_table – invisible return + non-empty stdout", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  txt <- capture.output(p <- print(out))
  expect_identical(p, out) # invisible(x) returns x
  expect_true(any(nzchar(txt)))
  expect_true(any(grepl("Variable", txt)))
})


# ============================================================================
# Polish round 2 – Q1/Q2 conflict warnings, stars validation, AME naming
# ============================================================================

test_that("AME – binary numeric var keeps the var name (not '<var>1')", {
  # mtcars$am is 0/1 INTEGER (not factor). marginaleffects returns
  # the contrast string "1 - 0" for binary numerics; we must not
  # mistake that for a factor and concatenate `am1`. The AME row
  # must align with the B coef row, both keyed `am`.
  fit <- lm(mpg ~ wt + am, data = mtcars)
  td <- broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
  ame <- td[td$estimate_type == "ame", ]
  expect_true("am" %in% ame$term)
  expect_false("am1" %in% ame$term)
})

test_that("AME – true factor still gets <var><level> naming", {
  # Sanity: factor predictors must still produce <var><level> AME
  # rows so they align with the B coef rows (cyl6, cyl8).
  fit <- lm(mpg ~ wt + cyl, data = mt)
  td <- broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
  ame_terms <- td$term[td$estimate_type == "ame"]
  expect_true(all(c("cyl6", "cyl8") %in% ame_terms))
})

test_that("Q1 – names(list) + model_labels conflict warns spicy_ignored_arg", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_warning(
    table_regression(list(A = m1, B = m2), model_labels = c("X", "Y")),
    class = "spicy_ignored_arg"
  )
})

test_that("Q1 – names(list) alone (no explicit model_labels): no warning", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_no_warning(
    table_regression(list(A = m1, B = m2)),
    class = "spicy_ignored_arg"
  )
})

test_that("Q2 – show_intercept=FALSE + non-default intercept_position warns", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_warning(
    table_regression(fit, show_intercept = FALSE, intercept_position = "last"),
    class = "spicy_ignored_arg"
  )
})

test_that("Q2 – show_intercept=FALSE + default intercept_position: no warning", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_no_warning(
    table_regression(fit, show_intercept = FALSE),
    class = "spicy_ignored_arg"
  )
})

test_that("stars validation – empty numeric errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, stars = setNames(numeric(0), character(0))),
    class = "spicy_invalid_input"
  )
})

test_that("stars validation – out-of-range threshold errors", {
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

test_that("stars validation – empty / unnamed name errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, stars = c(0.05, 0.01)), # no names
    class = "spicy_invalid_input"
  )
  bad <- setNames(c(0.05, 0.01), c("*", ""))
  expect_error(
    table_regression(fit, stars = bad),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# Polish round 3 – full Q21 validation cascade wired into orchestrator
# ============================================================================

test_that("vcov – unknown type errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, vcov = "HC99"),
    class = "spicy_invalid_input"
  )
})

test_that("vcov – CR* without cluster errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, vcov = "CR2"),
    class = "spicy_invalid_input"
  )
})

test_that("vcov – list length mismatch errors spicy_invalid_input", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(list(m1, m2), vcov = list("classical")),
    class = "spicy_invalid_input"
  )
})

test_that("ci_level – out of range errors spicy_invalid_input", {
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

test_that("show_columns – unknown token errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, show_columns = c("b", "BOGUS")),
    class = "spicy_invalid_input"
  )
})

test_that("show_columns – empty errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, show_columns = character(0)),
    class = "spicy_invalid_input"
  )
})

test_that("show_fit_stats – unknown token errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "nope")),
    class = "spicy_invalid_input"
  )
})

test_that("show_fit_stats – change tokens accepted under nested = TRUE", {
  # Since 0.12 the `nested_stats` argument was removed: change tokens
  # (`r2_change`, `f_change`, `p_change`, ...) are regular
  # `show_fit_stats` entries. An unknown token raises the same
  # spicy_invalid_input as any other show_fit_stats typo.
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(
      list(m1, m2),
      nested = TRUE,
      show_fit_stats = c("f_change", "BOGUS")
    ),
    class = "spicy_invalid_input"
  )
})

test_that("digit args – non-positive integer errors spicy_invalid_input", {
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

test_that("decimal_mark – multi-character errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, decimal_mark = "..."),
    class = "spicy_invalid_input"
  )
})

test_that("decimal_mark – the CI note quotes the coverage with the mark", {
  # Decision 27, note surface: "97,5% CIs: profile likelihood." -- the
  # coverage in a footer follows the same mark as the header it glosses.
  fit <- suppressWarnings(glm(am ~ wt, data = mt, family = binomial()))
  tbl <- table_regression(
    fit,
    ci_level = 0.975,
    decimal_mark = ",",
    ci_method = "profile"
  )
  note <- attr(tbl, "note")
  expect_match(note, "97,5% CIs: profile likelihood.", fixed = TRUE)
  expect_false(grepl("97.5", note, fixed = TRUE))
  # The period stays the period, exactly as before.
  tbl_dot <- table_regression(fit, ci_level = 0.975, ci_method = "profile")
  expect_match(
    attr(tbl_dot, "note"),
    "97.5% CIs: profile likelihood.",
    fixed = TRUE
  )
})

test_that("reference_label – empty string errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(fit, reference_label = ""),
    class = "spicy_invalid_input"
  )
})

test_that("model_labels – length mismatch errors spicy_invalid_input", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(list(m1, m2), model_labels = "only_one"),
    class = "spicy_invalid_input"
  )
})

test_that("outcome_labels – length mismatch errors spicy_invalid_input", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(list(m1, m2), outcome_labels = "only_one"),
    class = "spicy_invalid_input"
  )
})

test_that("labels – unknown predictor key errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, labels = c("nonexistent_predictor" = "Foo")),
    class = "spicy_invalid_input"
  )
})

test_that("nested = TRUE – different nobs errors spicy_invalid_input", {
  m1 <- lm(mpg ~ wt, data = mt)
  # Drop one row to force nobs mismatch
  m2 <- lm(mpg ~ wt + cyl, data = mt[-1, ])
  expect_error(
    table_regression(list(m1, m2), nested = TRUE),
    class = "spicy_invalid_input"
  )
})

test_that("nested = TRUE – different DV errors spicy_invalid_input", {
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
    table_regression(
      fit,
      output = "excel",
      excel_path = "/no/such/directory/out.xlsx"
    ),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# Polish round 4 – show_fit_stats footer block, labels for coef-style
# names, align argument
# ============================================================================

test_that("show_fit_stats – default tokens (n / R² / Adj. R²) appear in body", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  expect_true("n" %in% out$Variable)
  expect_true("R²" %in% out$Variable)
  expect_true("Adj. R²" %in% out$Variable)
})

test_that("show_fit_stats – custom tokens (omega2, sigma, AIC) appear", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(
    fit,
    show_fit_stats = c("nobs", "omega2", "sigma", "aic")
  )
  expect_true("ω²" %in% out$Variable)
  expect_true("σ̂" %in% out$Variable)
  expect_true("AIC" %in% out$Variable)
})

test_that("show_fit_stats = FALSE drops the footer block", {
  fit <- lm(mpg ~ wt, data = mt)
  # Positive control first: a negative membership on typed-out labels
  # asserts nothing the day one of them is renamed -- it just stops
  # matching and stays green. Pin that these three strings ARE the
  # footer rows before asserting they are gone.
  kept <- table_regression(fit)
  expect_true(all(c("n", "R²", "Adj. R²") %in% kept$Variable))

  out <- table_regression(fit, show_fit_stats = FALSE)
  expect_false(any(c("n", "R²", "Adj. R²") %in% out$Variable))
})

test_that("show_fit_stats – multi-model: each model contributes its values", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2), show_fit_stats = c("nobs", "r2"))
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

test_that("labels – coef-style key (cyl6) renames the contrast row", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(
    fit,
    labels = c("cyl6" = "6 cylinders", "cyl8" = "8 cylinders")
  )
  # Indented level rows are renamed (two-space level indent pinned)
  expect_true("  6 cylinders" %in% out$Variable)
  expect_true("  8 cylinders" %in% out$Variable)
  # Factor header still uses the term name "cyl"
  expect_true("cyl:" %in% out$Variable)
})

test_that("labels – mixed term + coef-style keys both honoured", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(
    fit,
    labels = c("cyl" = "Cylinders", "cyl6" = "Six", "wt" = "Weight")
  )
  expect_true("Weight" %in% out$Variable)
  expect_true("Cylinders:" %in% out$Variable)
  expect_true("  Six" %in% out$Variable)
})

test_that("labels – a key that exists in only ONE model applies there only", {
  # Mundlak-style comparison: the within / between decomposition exists
  # in the second model only. `labels` keys are validated against the
  # UNION of every model's terms, so naming a term absent from model 1
  # is legal, and the label lands on the rows where the term exists.
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + hp, data = mt)
  out <- table_regression(
    list(Naive = m1, Adjusted = m2),
    labels = c(wt = "Weight", hp = "Horsepower")
  )
  expect_true("Weight" %in% out$Variable)
  expect_true("Horsepower" %in% out$Variable)
  # The Horsepower row is empty under the model that lacks the term.
  hp_row <- out[out$Variable == "Horsepower", , drop = FALSE]
  expect_identical(nrow(hp_row), 1L)
  # A key absent from EVERY model is still a hard error.
  expect_error(
    table_regression(list(m1, m2), labels = c(nope = "X")),
    class = "spicy_invalid_input"
  )
})

test_that("align – 'decimal' is default; padding applied to numeric cols", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  expect_equal(attr(out, "align"), "decimal")
  # Decimal-aligned strings are pre-padded → cells contain spaces
  b_col <- out$B
  # At least some cells start or end with a space (padding)
  expect_true(any(grepl("^[ ].*", b_col) | grepl(".*[ ]$", b_col)))
})

test_that("align – 'right' / 'center' are accepted (no decimal pad)", {
  fit <- lm(mpg ~ wt, data = mt)
  for (a in c("center", "right")) {
    out <- table_regression(fit, align = a)
    expect_equal(attr(out, "align"), a)
  }
})

test_that("print – align = 'center' propagates to align_center_cols", {
  # Exercises the data_col_idx branch in print.spicy_regression_table
  # when align is "center" (vs "decimal" / "right").
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
    table_regression(list(m1, m2), model_labels = c("Same", "Same")),
    class = "spicy_invalid_input"
  )
})

test_that("a typed name colliding with an auto-filled label is refused", {
  # The two guards above compare `model_labels` with itself and
  # `names(models)` with itself. Neither compares a typed name with the
  # "Model <position>" label the auto-fill writes into the unnamed
  # slots -- so `list("Model 2" = m1, m2)` used to render two models
  # under one spanner (or, when every label collapsed, under none).
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ hp, data = mt)
  m3 <- lm(mpg ~ drat, data = mt)

  # Total collision: 2 models, 1 label.
  expect_error(
    table_regression(list("Model 2" = m1, m2)),
    class = "spicy_invalid_input"
  )
  # Adjacent collision: models 1 and 2 of 3.
  expect_error(
    table_regression(list(m1, "Model 1" = m2, m3)),
    class = "spicy_invalid_input"
  )
  # Non-adjacent collision: models 1 and 3 of 3. This is the shape that
  # produced a non-contiguous column set in the typed view, which each
  # engine then rendered its own way.
  expect_error(
    table_regression(list(m1, m2, "Model 1" = m3)),
    class = "spicy_invalid_input"
  )

  # Partial naming without a collision keeps working -- the guard must
  # not cost the feature it protects.
  expect_equal(
    names(attr(table_regression(list("Step 1" = m1, m2)), "spanners")),
    c("Step 1", "Model 2")
  )
})

test_that("the collision message names the label and both positions", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ hp, data = mt)
  m3 <- lm(mpg ~ drat, data = mt)

  # The user typed only ONE of the two colliding labels, so the message
  # has to say where the other one came from.
  err <- expect_error(table_regression(list("Model 2" = m1, m2)))
  msg <- conditionMessage(err)
  expect_match(msg, "Model labels must be unique", fixed = TRUE)
  expect_match(msg, '"Model 2" repeats', fixed = TRUE)
  expect_match(msg, "the name of model 1", fixed = TRUE)
  expect_match(msg, "the default label of model 2", fixed = TRUE)
  expect_match(msg, "`model_labels`", fixed = TRUE)

  # Provenance follows the positions, it is not hardcoded in order.
  err2 <- expect_error(table_regression(list(m1, m2, "Model 1" = m3)))
  expect_match(
    conditionMessage(err2),
    "the default label of model 1 and the name of model 3",
    fixed = TRUE
  )
})

test_that("the collision is detected against the registry, not a literal", {
  # The auto-fill writes `spicy_fmt("label_model_name", i)`. Under a
  # localized registry the colliding string is a different one, and the
  # guard must still see it -- proof it compares resolved labels rather
  # than matching "Model %d" by hand.
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ hp, data = mt)
  base_str <- spicy_str
  local_mocked_bindings(
    spicy_str = function(key) {
      if (identical(key, "label_model_name")) "Modele %d" else base_str(key)
    }
  )
  expect_error(
    table_regression(list("Modele 2" = m1, m2)),
    class = "spicy_invalid_input"
  )
  # And the English literal is no longer a collision under that registry.
  expect_equal(
    names(attr(table_regression(list("Model 2" = m1, m2)), "spanners")),
    c("Model 2", "Modele 2")
  )
})

test_that("the collision is refused before any model is extracted", {
  # Fail-fast: the refusal belongs with the other argument validation,
  # not after n expensive extractions.
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ hp, data = mt)
  calls <- 0L
  local_mocked_bindings(
    as_regression_frame.lm = function(fit, ...) {
      calls <<- calls + 1L
      stop("extraction must not be reached")
    }
  )
  # Control: without the collision the same mock IS reached, so the
  # counter is measuring the extraction loop and not a typo.
  expect_error(table_regression(list(m1, m2)), "must not be reached")
  expect_identical(calls, 1L)

  calls <- 0L
  expect_error(
    table_regression(list("Model 2" = m1, m2)),
    class = "spicy_invalid_input"
  )
  expect_identical(calls, 0L)
})

test_that("DV smart spanner uses the bare variable name (NOT attr('label'))", {
  # With distinct DVs and no explicit labels, the response variable
  # NAME is lifted into the spanner -- not `attr("label")`, which
  # can be a long phrase that would distort column widths. The
  # Outcome body row is suppressed (info is in the header).
  df <- data.frame(y = rnorm(50), x = rnorm(50))
  attr(df$y, "label") <- "Wellbeing score (0-100)" # NOT used
  fit_a <- lm(y ~ x, data = df)
  fit_b <- lm(x ~ y, data = df)

  out <- table_regression(list(fit_a, fit_b))
  # Positive control: explicit `outcome_labels` DO produce the row, so
  # this negative membership on a typed-out row label cannot go quiet
  # the day the label is renamed.
  expect_true(
    "Outcome" %in%
      table_regression(
        list(fit_a, fit_b),
        outcome_labels = c("A", "B")
      )$Variable
  )
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
  # Positive control, as elsewhere in this block: the row exists when
  # it is asked for.
  expect_true(
    "Outcome" %in%
      table_regression(
        list(fit1, fit2),
        outcome_labels = c("A", "B")
      )$Variable
  )
  expect_false("Outcome" %in% out$Variable)
})

test_that("cluster_name – `df$col` extracted to 'col' for the footer", {
  skip_if_not_installed("clubSandwich")
  set.seed(1)
  df <- data.frame(
    y = rnorm(120),
    x = rnorm(120),
    region = factor(sample(letters[1:6], 120, replace = TRUE))
  )
  fit <- lm(y ~ x, data = df)
  out <- table_regression(fit, vcov = "CR2", cluster = df$region)
  expect_identical(
    attr(out, "note"),
    "Note. Linear regression.\nStd. errors: cluster-robust (CR2), clusters by region."
  )
  expect_no_match(attr(out, "note"), "cluster vector supplied")
})

test_that("cluster_name – bare symbol extracted as variable name", {
  skip_if_not_installed("clubSandwich")
  set.seed(2)
  df <- data.frame(
    y = rnorm(120),
    x = rnorm(120),
    region = factor(sample(letters[1:6], 120, replace = TRUE))
  )
  region_vec <- df$region
  fit <- lm(y ~ x, data = df)
  out <- table_regression(fit, vcov = "CR2", cluster = region_vec)
  expect_identical(
    attr(out, "note"),
    "Note. Linear regression.\nStd. errors: cluster-robust (CR2), clusters by region_vec."
  )
})

test_that("cluster_name – list(...) with named elements per model", {
  skip_if_not_installed("clubSandwich")
  set.seed(3)
  df <- data.frame(
    y = rnorm(120),
    x = rnorm(120),
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
  expect_identical(
    note,
    paste0(
      "Note. Linear regression models.\nStd. errors:\n",
      "  Model 1: cluster-robust (CR2), clusters by region\n",
      "  Model 2: cluster-robust (CR2), clusters by clinic"
    )
  )
})

test_that("extract_arg_column_name – handles all recognised forms", {
  expect_equal(spicy:::extract_arg_column_name(quote(df$col)), "col")
  expect_equal(spicy:::extract_arg_column_name(quote(df[["col"]])), "col")
  expect_equal(spicy:::extract_arg_column_name(quote(mycluster)), "mycluster")
  expect_true(is.na(spicy:::extract_arg_column_name(quote(c(1, 2, 3)))))
  expect_true(is.na(spicy:::extract_arg_column_name(NULL)))
})


# ============================================================================
# Polish round 5 – outcome_labels (Q11b) and reference_style annotation (Q5)
# ============================================================================

test_that("outcome_labels – single model: row never shown (DV is in title)", {
  fit <- lm(mpg ~ wt, data = mt)
  out_null <- table_regression(fit, outcome_labels = NULL)
  out_chr <- table_regression(fit, outcome_labels = "Custom")
  out_F <- table_regression(fit, outcome_labels = FALSE)
  # Positive control: the row label is real -- a two-model table with
  # explicit labels carries it -- so these three negatives keep biting
  # if it is ever renamed.
  expect_true(
    "Outcome" %in%
      table_regression(
        list(fit, lm(hp ~ wt, data = mt)),
        outcome_labels = c("A", "B")
      )$Variable
  )
  expect_false("Outcome" %in% out_null$Variable)
  expect_false("Outcome" %in% out_chr$Variable)
  expect_false("Outcome" %in% out_F$Variable)
})

test_that("outcome_labels – multi-model identical DVs: NULL hides the row", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2))
  # Positive control: explicit labels put the row back.
  expect_true(
    "Outcome" %in%
      table_regression(
        list(m1, m2),
        outcome_labels = c("A", "B")
      )$Variable
  )
  expect_false("Outcome" %in% out$Variable)
})

test_that("outcome_labels – multi-model differing DVs: NULL lifts DVs into spanner", {
  # When `outcome_labels = NULL` (default) and DVs differ, the
  # smart default moves the auto-detected DV names into the
  # multi-model spanner and suppresses the body Outcome row.
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp <- lm(hp ~ wt, data = mt)
  out <- table_regression(list(m_mpg, m_hp))
  # Positive control: explicit labels put the row back.
  expect_true(
    "Outcome" %in%
      table_regression(
        list(m_mpg, m_hp),
        outcome_labels = c("A", "B")
      )$Variable
  )
  expect_false("Outcome" %in% out$Variable)
  spans <- attr(out, "spanners")
  expect_equal(names(spans), c("mpg", "hp"))
})

test_that("outcome_labels – explicit labels take precedence", {
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp <- lm(hp ~ wt, data = mt)
  out <- table_regression(
    list(m_mpg, m_hp),
    outcome_labels = c("Fuel economy", "Horsepower")
  )
  outcome_row <- out[out$Variable == "Outcome", , drop = FALSE]
  # Cells are decimal-align padded; pin the exact trimmed content.
  expect_identical(trimws(outcome_row[, "Model 1: B"]), "Fuel economy")
  expect_identical(trimws(outcome_row[, "Model 2: B"]), "Horsepower")
})

test_that("outcome_labels – FALSE suppresses the row even with differing DVs", {
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp <- lm(hp ~ wt, data = mt)
  out <- table_regression(list(m_mpg, m_hp), outcome_labels = FALSE)
  # Positive control: the same two fits WITH labels carry the row, so
  # `FALSE` is what suppresses it here.
  expect_true(
    "Outcome" %in%
      table_regression(
        list(m_mpg, m_hp),
        outcome_labels = c("A", "B")
      )$Variable
  )
  expect_false("Outcome" %in% out$Variable)
})

test_that("reference_style = 'annotation' – factor header annotated [ref: <level>]", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, reference_style = "annotation")
  # Header now reads "cyl: [ref: 4]" (or whichever the ref level is)
  expect_true(any(grepl("^cyl: \\[ref: 4\\]$", out$Variable)))
  # And the orphan row "4 (ref.)" must NOT appear (it was dropped)
  expect_false(any(grepl("\\(ref\\.\\)", out$Variable)))
})

test_that("reference_style = 'row' (default) – no factor header annotation", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit) # default = "row"
  expect_true("cyl:" %in% out$Variable)
  expect_false(any(grepl("\\[ref: ", out$Variable)))
  expect_true(any(grepl("\\(ref\\.\\)", out$Variable)))
})

test_that("reference_style = 'annotation' – works with multiple factors", {
  mt2 <- mt
  mt2$gear <- factor(mt2$gear)
  fit <- lm(mpg ~ wt + cyl + gear, data = mt2)
  out <- table_regression(fit, reference_style = "annotation")
  expect_true(any(grepl("^cyl: \\[ref: 4\\]$", out$Variable)))
  expect_true(any(grepl("^gear: \\[ref: 3\\]$", out$Variable)))
})


# ============================================================================
# Polish round 6 – no-intercept formula edge case (y ~ 0 + x)
# ============================================================================

test_that("no-intercept formula – first level of factor is a real coef, no phantom ref row", {
  fit <- lm(mpg ~ 0 + wt + cyl, data = mt)
  out <- table_regression(fit)
  # No "(ref.)" suffix anywhere – all 3 cyl levels are fitted coefs
  expect_false(any(grepl("\\(ref\\.\\)", out$Variable)))
  # cyl4 / cyl6 / cyl8 all displayed (indented under "cyl:")
  expect_true(any(grepl("^  4$", out$Variable)))
  expect_true(any(grepl("^  6$", out$Variable)))
  expect_true(any(grepl("^  8$", out$Variable)))
  # No standalone "cyl4" row outside the factor group
  expect_false(any(grepl("^cyl4$", out$Variable)))
})

test_that("no-intercept formula – tidy returns all factor coefs", {
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

test_that("no-intercept formula – alt syntax 'y ~ x - 1' is equivalent", {
  fit_a <- lm(mpg ~ 0 + wt, data = mt)
  fit_b <- lm(mpg ~ wt - 1, data = mt)
  out_a <- table_regression(fit_a)
  out_b <- table_regression(fit_b)
  expect_equal(out_a$B, out_b$B)
})

test_that("no-intercept formula – works with multi-model nested lookalike", {
  m_with_int <- lm(mpg ~ wt + cyl, data = mt)
  m_no_int <- lm(mpg ~ 0 + wt + cyl, data = mt)
  # Side-by-side display (NOT nested – these aren't nested in the
  # likelihood sense). Validate that the rendering does not crash.
  out <- table_regression(list(m_with_int, m_no_int))
  expect_s3_class(out, "spicy_regression_table")
  # Reference-row from M1 still shown for M2 the same factor group
  expect_true(any(grepl("\\(ref\\.\\)", out$Variable)))
})


# ============================================================================
# Snapshot tests – golden output for the most common rendering paths
# ============================================================================

# Snapshot helper: capture the printed output and normalise trailing
# whitespace + the trailing blank line. spicy_print_table() pads
# every cell to a fixed column width, so harmless changes in the
# longest cell propagate as cascade-y diffs in the snapshot. Trimming
# trailing whitespace per line keeps the snapshot stable as long as
# the SEMANTIC content (cells + alignment) is unchanged.
capture_norm <- function(out) {
  txt <- capture.output(print(out))
  txt <- sub("[ \t]+$", "", txt) # trim trailing whitespace
  paste(txt, collapse = "\n")
}

test_that("snapshot – single lm default rendering", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  expect_snapshot(cat(capture_norm(out)))
})

test_that("snapshot – multi-model with nested = TRUE comparison footer", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2), nested = TRUE)
  expect_snapshot(cat(capture_norm(out)))
})

test_that("snapshot – standardized + stars + reference annotation", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(
    fit,
    standardized = "refit",
    stars = TRUE,
    reference_style = "annotation"
  )
  expect_snapshot(cat(capture_norm(out)))
})


# ============================================================================
# Multi-model column spanners (model name above each model's sub-columns)
# ============================================================================

test_that("spanner – single model: no spanner attribute", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  expect_null(attr(out, "spanners"))
})

test_that("spanner – multi-model named list: names become spanner labels", {
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

test_that("spanner – multi-model unnamed + same DV: 'Model N' labels", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list(m1, m2))
  spans <- attr(out, "spanners")
  expect_equal(names(spans), c("Model 1", "Model 2"))
})

test_that("spanner – multi-model unnamed + distinct DVs: DV smart default", {
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp <- lm(hp ~ wt, data = mt)
  out <- table_regression(list(m_mpg, m_hp))
  spans <- attr(out, "spanners")
  expect_equal(names(spans), c("mpg", "hp"))
  # Outcome row is folded into the spanner -> not in the body.
  # Positive control: explicit labels unfold it again.
  expect_true(
    "Outcome" %in%
      table_regression(
        list(m_mpg, m_hp),
        outcome_labels = c("A", "B")
      )$Variable
  )
  expect_false("Outcome" %in% out$Variable)
})

test_that("spanner – explicit model_labels override DV smart default", {
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp <- lm(hp ~ wt, data = mt)
  out <- table_regression(list(m_mpg, m_hp), model_labels = c("Fuel", "Power"))
  spans <- attr(out, "spanners")
  expect_equal(names(spans), c("Fuel", "Power"))
})

test_that("spanner – explicit outcome_labels keep the row; spanner stays generic", {
  m_mpg <- lm(mpg ~ wt, data = mt)
  m_hp <- lm(hp ~ wt, data = mt)
  out <- table_regression(
    list(m_mpg, m_hp),
    outcome_labels = c("Fuel economy", "Horsepower")
  )
  spans <- attr(out, "spanners")
  expect_equal(names(spans), c("Model 1", "Model 2"))
  expect_true("Outcome" %in% out$Variable)
})

test_that("spanner – multi-model print strips 'Label: ' prefix from headers", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(list("A" = m1, "B" = m2))
  txt <- capture.output(print(out))
  joined <- paste(txt, collapse = "\n")
  # The spanner row is a dedicated line containing ONLY the two
  # model labels, in order ("A" alone would match e.g. "Adj. R2").
  expect_true(any(grepl("^\\s+A\\s+B\\s*$", txt)))
  # The bare sub-column tokens are shown twice (one per model);
  # the "A: B" / "B: B" prefixed form must not appear in the header.
  expect_false(grepl("A: B", joined, fixed = TRUE))
})

test_that("spanner – .validate_spanners catches malformed input", {
  df <- data.frame(a = 1, b = 2, c = 3, d = 4)
  expect_error(
    spicy:::build_ascii_table(df, spanners = list(2:3)), # unnamed
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy:::build_ascii_table(df, spanners = list(g = 5:6)), # out of range
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy:::build_ascii_table(df, spanners = list(g = c(2, 4))), # non-contiguous
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy:::build_ascii_table(df, spanners = list(a = 2:3, b = 3:4)), # overlapping
    class = "spicy_invalid_input"
  )
})

test_that("spanner – gt output applies tab_spanner + cols_label", {
  skip_if_not_installed("gt")
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  g <- table_regression(list("A" = m1, "B" = m2), output = "gt")
  html <- as.character(gt::as_raw_html(g))
  expect_match(html, ">A<") # spanner label present
  expect_match(html, ">B<")
  # Bare sub-column labels are used (cols_label stripped the prefix).
  # ">B<" matches both the spanner "B" and the bare-token "B"; ensure
  # the prefixed "A: B" form does NOT appear as a rendered label.
  expect_false(grepl(">A: B<", html, fixed = TRUE))
})

test_that("spanner – flextable output adds a header row with spanners", {
  skip_if_not_installed("flextable")
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  f <- table_regression(list("A" = m1, "B" = m2), output = "flextable")
  hdr <- f$header$dataset
  # Multi-model header layout (matches gt / tinytable convention):
  #   row 1: model spanner (A / B)
  #   row 2: per-col + CI spanner (B / 95% CI / p / ...) -- only when
  #          the table has a CI pair; the labels stay in the
  #          column-labels row otherwise
  #   row 3: column-labels row (Variable / LL / UL / ...)
  # These models default to B / SE / p, so the console prints two
  # header rows and so does the engine.
  expect_equal(nrow(hdr), 2L)
  expect_true(any(unlist(hdr[1, ]) == "A"))
  expect_true(any(unlist(hdr[1, ]) == "B"))
  f_ci <- table_regression(
    list("A" = m1, "B" = m2),
    show_columns = c("b", "ci", "p"),
    output = "flextable"
  )
  expect_equal(nrow(f_ci$header$dataset), 3L)
})

test_that("ordered factor – grouped under header, poly-order, auto footer note", {
  set.seed(1)
  df <- data.frame(
    y = rnorm(200),
    x = rnorm(200),
    edu = ordered(
      sample(c("Low", "Med", "High", "Top"), 200, replace = TRUE),
      levels = c("Low", "Med", "High", "Top")
    )
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
  expect_match(
    note,
    "Ordered factor `edu`: polynomial trends (.L = linear, .Q = quadratic, .C = cubic).",
    fixed = TRUE
  )
  expect_match(note, ".L = linear", fixed = TRUE)
  expect_match(note, ".Q = quadratic", fixed = TRUE)
})

test_that("ordered factor – fitting with factor(ordered = FALSE) restores treatment layout", {
  set.seed(1)
  df <- data.frame(
    y = rnorm(200),
    edu = ordered(
      sample(c("Low", "Med", "High"), 200, replace = TRUE),
      levels = c("Low", "Med", "High")
    )
  )
  df$edu_t <- factor(df$edu, ordered = FALSE)
  fit <- lm(y ~ edu_t, df)
  out <- table_regression(fit)
  vars <- trimws(as.data.frame(out, stringsAsFactors = FALSE)$Variable)
  expect_true("edu_t:" %in% vars)
  expect_true("Low (ref.)" %in% vars) # vars is trimws()'d above
  # No poly footer for this fit.
  note <- attr(out, "note")
  expect_false(grepl("polynomial trends", note))
})


test_that("default show_columns context-aware: single keeps CI, multi drops it", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out1 <- table_regression(m1)
  expect_true("95% CI" %in% names(out1))
  out2 <- table_regression(list(m1, m2))
  expect_false(any(grepl("95% CI", names(out2))))
  # Explicit user override restores CI even in multi-model.
  out3 <- table_regression(list(m1, m2), show_columns = c("b", "se", "ci", "p"))
  expect_true(all(c("Model 1: 95% CI", "Model 2: 95% CI") %in% names(out3)))
})


test_that("outcome_labels – NULL hides the row even when DVs differ + names supplied", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(hp ~ wt, data = mt)
  out <- table_regression(list("Step 1" = m1, "Step 2" = m2))
  expect_false("Outcome" %in% out$Variable)
  # Explicit opt-in still works.
  out2 <- table_regression(
    list("Step 1" = m1, "Step 2" = m2),
    outcome_labels = c("Fuel", "Power")
  )
  expect_true("Outcome" %in% out2$Variable)
})


test_that("spanner – tinytable output uses group_tt for column groups", {
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
    y = rnorm(200),
    age = rnorm(200),
    sex = factor(
      sample(c("Female", "Male"), 200, replace = TRUE),
      levels = c("Female", "Male")
    )
  )
  fit <- lm(y ~ age + sex, df)

  out_t <- table_regression(fit, factor_layout = "grouped")
  vars_t <- as.data.frame(out_t, stringsAsFactors = FALSE)$Variable
  ref_t <- which(vars_t == "  Female (ref.)")
  male_t <- which(vars_t == "  Male")
  expect_true(ref_t < male_t)

  out_f <- table_regression(fit, factor_layout = "flat")
  vars_f <- as.data.frame(out_f, stringsAsFactors = FALSE)$Variable
  ref_f <- which(vars_f == "sexFemale (ref.)")
  male_f <- which(vars_f == "sexMale")
  expect_true(ref_f < male_f)
})


# ============================================================================
# reference_style = "annotation" / "footer" / "none" + factor_layout enum
# ============================================================================

test_that("reference_style = \"annotation\" + factor_layout = \"flat\" inlines [vs <ref>] on 1st dummy only", {
  df <- data.frame(
    y = rnorm(200),
    sex = factor(
      sample(c("Female", "Male"), 200, replace = TRUE),
      levels = c("Female", "Male")
    ),
    edu = factor(
      sample(c("Lower", "Upper", "Tertiary"), 200, replace = TRUE),
      levels = c("Lower", "Upper", "Tertiary")
    )
  )
  fit <- lm(y ~ sex + edu, df)
  out <- table_regression(
    fit,
    reference_style = "annotation",
    factor_layout = "flat"
  )
  vars <- as.data.frame(out, stringsAsFactors = FALSE)$Variable
  # 2-level factor: the single non-ref dummy carries [vs Female]
  expect_true("sexMale [vs Female]" %in% vars)
  # 3-level factor: FIRST non-ref dummy carries [vs Lower]; second does not
  edu_rows <- grep("^edu", vars, value = TRUE)
  with_marker <- grep("[vs Lower]", edu_rows, fixed = TRUE, value = TRUE)
  expect_identical(with_marker, "eduUpper [vs Lower]") # exactly one, on 1st dummy
  # Reference rows themselves are NOT in the body in annotation mode
  expect_false(any(grepl("Lower (ref.)", vars, fixed = TRUE)))
})

test_that("reference_style = \"footer\" adds a single 'Reference categories: ...' line", {
  df <- data.frame(
    y = rnorm(200),
    sex = factor(
      sample(c("Female", "Male"), 200, replace = TRUE),
      levels = c("Female", "Male")
    ),
    edu = factor(
      sample(c("Lower", "Upper", "Tertiary"), 200, replace = TRUE),
      levels = c("Lower", "Upper", "Tertiary")
    )
  )
  fit <- lm(y ~ sex + edu, df)
  out <- table_regression(fit, reference_style = "footer")
  vars <- as.data.frame(out, stringsAsFactors = FALSE)$Variable
  # Ref rows dropped from body; no inline annotation
  expect_false(any(grepl("(ref.)", vars, fixed = TRUE)))
  expect_false(any(grepl("[vs ", vars, fixed = TRUE)))
  # Footer line lists both factor references
  note <- attr(out, "note")
  expect_match(
    note,
    "Reference categories: sex = Female; edu = Lower.",
    fixed = TRUE
  )
  expect_match(note, "sex = Female")
  expect_match(note, "edu = Lower")
})

test_that("reference_style = \"none\" shows no reference info anywhere", {
  df <- data.frame(
    y = rnorm(200),
    sex = factor(
      sample(c("Female", "Male"), 200, replace = TRUE),
      levels = c("Female", "Male")
    )
  )
  fit <- lm(y ~ sex, df)
  # Suppress the spicy_inform emitted on flat+none; we test that
  # separately below.
  withCallingHandlers(
    out <- table_regression(
      fit,
      reference_style = "none",
      factor_layout = "flat"
    ),
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
    y = rnorm(100),
    sex = factor(sample(c("Female", "Male"), 100, replace = TRUE))
  )
  fit <- lm(y ~ sex, df)
  cnd <- NULL
  withCallingHandlers(
    table_regression(fit, reference_style = "none", factor_layout = "flat"),
    spicy_info = function(c) {
      cnd <<- c
      invokeRestart("muffleMessage")
    }
  )
  expect_s3_class(cnd, "spicy_silent_reference")
  expect_s3_class(cnd, "spicy_info")
  # Pin both full sentences (the second line's leading info glyph is
  # non-ASCII, so match starts after it).
  expect_match(
    conditionMessage(cnd),
    "`reference_style = \"none\"` with `factor_layout = \"flat\"`: reference levels are not displayed anywhere.",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(cnd),
    "State the reference convention in the surrounding text or table caption.",
    fixed = TRUE
  )
})

test_that("reference_style = \"none\" + factor_layout = \"grouped\" does NOT emit the info", {
  # Grouped still shows the `education:` header, so the silent-loss
  # warning is unnecessary -- only the FLAT case loses all visual
  # trace of the factor's existence beyond per-level dummies.
  df <- data.frame(
    y = rnorm(100),
    sex = factor(sample(c("Female", "Male"), 100, replace = TRUE))
  )
  fit <- lm(y ~ sex, df)
  cnd <- NULL
  withCallingHandlers(
    table_regression(fit, reference_style = "none", factor_layout = "grouped"),
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
    table_regression(fit, reference_style = "none", factor_layout = "flat"),
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
  out <- table_regression(fit) # default grouped
  vars <- as.data.frame(out, stringsAsFactors = FALSE)$Variable
  expect_true("cyl:" %in% vars)
  expect_true(any(grepl("^  6$", vars)))
  expect_true(any(grepl("^  8$", vars)))
})


# ============================================================================
# Phase 3 matrix – rd-core: documented promises of table_regression()
# ============================================================================

test_that("show_columns – group tokens expand to their documented fixed vectors", {
  # rd-core:show-columns-group-token-expansions
  expected <- list(
    all_b = c("b", "se", "ci", "p"),
    all_b_compact = c("b", "se", "p"),
    all_b_full = c("b", "se", "ci", "t", "p"),
    all_beta = c("b", "beta", "se", "ci", "p"),
    all_ame = c("ame", "ame_se", "ame_ci", "ame_p"),
    all_ame_compact = c("ame", "ame_p"),
    all_f2 = c("partial_f2", "partial_f2_ci"),
    all_eta2 = c("partial_eta2", "partial_eta2_ci"),
    all_omega2 = c("partial_omega2", "partial_omega2_ci")
  )
  expect_identical(spicy:::.show_columns_groups, expected)
  for (g in names(expected)) {
    expect_identical(spicy:::expand_show_columns(g), expected[[g]])
  }
})

test_that("show_columns – each preset table equals its atomic-token table", {
  # rd-core:show-columns-group-token-expansions (end-to-end column sets)
  fit <- lm(mpg ~ wt + cyl, data = mt)
  pairs <- list(
    list(preset = "all_b_full", atoms = c("b", "se", "ci", "t", "p")),
    list(preset = "all_f2", atoms = c("partial_f2", "partial_f2_ci")),
    list(preset = "all_eta2", atoms = c("partial_eta2", "partial_eta2_ci")),
    list(
      preset = "all_omega2",
      atoms = c("partial_omega2", "partial_omega2_ci")
    )
  )
  for (p in pairs) {
    o1 <- table_regression(fit, show_columns = p$preset)
    o2 <- table_regression(fit, show_columns = p$atoms)
    expect_identical(names(o1), names(o2))
    expect_identical(
      as.data.frame(o1, stringsAsFactors = FALSE),
      as.data.frame(o2, stringsAsFactors = FALSE)
    )
  }
  # all_beta requires a standardisation method
  o1 <- table_regression(fit, show_columns = "all_beta", standardized = "refit")
  o2 <- table_regression(
    fit,
    show_columns = c("b", "beta", "se", "ci", "p"),
    standardized = "refit"
  )
  expect_identical(names(o1), names(o2))
})

test_that("show_columns – AME presets equal their atomic-token tables", {
  # rd-core:show-columns-group-token-expansions (all_ame / all_ame_compact)
  skip_if_not_installed("marginaleffects")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  for (p in list(
    list(preset = "all_ame", atoms = c("ame", "ame_se", "ame_ci", "ame_p")),
    list(preset = "all_ame_compact", atoms = c("ame", "ame_p"))
  )) {
    o1 <- table_regression(fit, show_columns = p$preset)
    o2 <- table_regression(fit, show_columns = p$atoms)
    expect_identical(names(o1), names(o2))
  }
})

test_that("show_columns – token order controls displayed column order", {
  # rd-core:show-columns-dedup-order
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, show_columns = c("p", "b"))
  expect_identical(names(out), c("Variable", "p", "B"))
  # Duplicate after group expansion is deduplicated to a single column.
  out2 <- table_regression(fit, show_columns = c("all_b", "se"))
  expect_identical(sum(names(out2) == "SE"), 1L)
})

test_that("show_columns – beta is auto-injected directly after b", {
  # rd-core:show-columns-beta-autoinject
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(
    fit,
    standardized = "refit",
    show_columns = c("b", "p")
  )
  expect_identical(names(out), c("Variable", "B", "β", "p"))
})

test_that("show_columns – 'p' carries the B p-values, never the AME ones", {
  # rd-core:show-columns-p-is-b-p
  skip_if_not_installed("marginaleffects")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, show_columns = c("b", "ame", "p", "ame_p"))
  s <- as_structured(out)
  toks <- vapply(s$col_meta, function(m) m$token, character(1))
  p_col <- names(toks)[toks == "p"]
  amep_col <- names(toks)[toks == "ame_p"]
  expect_length(p_col, 1L)
  expect_length(amep_col, 1L)
  oracle <- summary(fit)$coefficients[, 4]
  rows <- match(c("(Intercept)", "wt"), s$body$Variable)
  expect_equal(
    s$body[[p_col]][rows],
    unname(oracle[c("(Intercept)", "wt")]),
    tolerance = 1e-12
  )
  # The AME p of the intercept row is NA (no AME for the intercept),
  # while the B p is not – the two columns are distinct quantities.
  expect_true(is.na(s$body[[amep_col]][rows[1]]))
  expect_false(isTRUE(all.equal(
    s$body[[p_col]][rows[2]],
    s$body[[amep_col]][rows[2]],
    tolerance = 1e-12
  )))
})

test_that("show_columns – every base atomic token adds exactly one column", {
  # rd-core:show-columns-atomic-tokens (incl. the never-tested bare 't')
  fit <- lm(mpg ~ wt + cyl, data = mt)
  headers <- c(b = "B", se = "SE", ci = "95% CI", t = "t", p = "p")
  for (tok in names(headers)) {
    out <- table_regression(fit, show_columns = tok)
    expect_identical(names(out), c("Variable", headers[[tok]]))
  }
  # t values match the summary() oracle
  out_t <- table_regression(fit, show_columns = c("b", "t"))
  s <- as_structured(out_t)
  rows <- match(c("(Intercept)", "wt"), s$body$Variable)
  expect_equal(
    s$body$t[rows],
    unname(summary(fit)$coefficients[c("(Intercept)", "wt"), 3]),
    tolerance = 1e-10
  )
})

test_that("boot_n – formals default is 1000L; degenerate values rejected", {
  # rd-core:boot-n-default
  expect_identical(formals(table_regression)$boot_n, 1000L)
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, vcov = "bootstrap", boot_n = 0),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, vcov = "bootstrap", boot_n = c(10, 20)),
    class = "spicy_invalid_input"
  )
})

test_that("ci_method = 'profile' with lm raises spicy_invalid_input", {
  # rd-core:ci-method-profile-lm-refused
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, ci_method = "profile"),
    class = "spicy_invalid_input"
  )
})

test_that("raw formula input is refused – fit-only API", {
  # rd-core:models-fit-only-api
  expect_error(table_regression(mpg ~ wt), class = "spicy_unsupported")
})

test_that("ci_level moves B, beta, AME and partial CI bounds together", {
  # rd-core:ci-level-default-scope
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("broom")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  cols <- c("b", "ci", "ame", "ame_ci", "partial_eta2", "partial_eta2_ci")
  o90 <- table_regression(
    fit,
    ci_level = 0.90,
    standardized = "refit",
    show_columns = cols
  )
  o95 <- table_regression(
    fit,
    ci_level = 0.95,
    standardized = "refit",
    show_columns = cols
  )
  expect_true("90% CI" %in% names(o90))
  s90 <- as_structured(o90)
  s95 <- as_structured(o95)
  wt90 <- s90$body[s90$body$Variable == "wt", ]
  wt95 <- s95$body[s95$body$Variable == "wt", ]
  # B CI matches the confint() oracle at each level
  expect_equal(
    unname(unlist(wt90[, c("90% CI: LL", "90% CI: UL")])),
    unname(confint(fit, level = 0.90)["wt", ]),
    tolerance = 1e-10
  )
  expect_equal(
    unname(unlist(wt95[, c("95% CI: LL", "95% CI: UL")])),
    unname(confint(fit, level = 0.95)["wt", ]),
    tolerance = 1e-10
  )
  # AME + partial eta2 CI bounds move with the level too
  ame_ll90 <- wt90[[grep("CI\\.2: LL$", names(s90$body), value = TRUE)]]
  ame_ll95 <- wt95[[grep("CI\\.2: LL$", names(s95$body), value = TRUE)]]
  expect_gt(ame_ll90, ame_ll95)
  eta_ll90 <- wt90[["η² 90% CI: LL"]]
  eta_ll95 <- wt95[["η² 95% CI: LL"]]
  expect_gt(eta_ll90, eta_ll95)
  # beta CI (carried by tidy) narrows at 90%
  td90 <- broom::tidy(o90)
  td95 <- broom::tidy(o95)
  b90 <- td90[td90$estimate_type == "beta" & td90$term == "wt", ]
  b95 <- td95[td95$estimate_type == "beta" & td95$term == "wt", ]
  expect_gt(b90$conf.low, b95$conf.low)
  expect_lt(b90$conf.high, b95$conf.high)
})

test_that("'n' column is dropped for a plain multivariable fit", {
  # rd-core:show-columns-n-uv-populated (no per-row N data -> no column)
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, show_columns = c("b", "n"))
  expect_identical(names(out), c("Variable", "B"))
})

test_that("partial_f2 / partial_eta2 / partial_omega2 are refused for glm", {
  # rd-core:show-columns-partial-lm-only
  gfit <- glm(am ~ mpg + cyl, data = mt, family = binomial)
  for (tok in c("partial_f2", "partial_eta2", "partial_omega2")) {
    expect_error(
      table_regression(gfit, show_columns = c("b", tok)),
      class = "spicy_invalid_input"
    )
  }
})

test_that("r2 / adj_r2 – lm oracle values; lm-only tokens refused on all-glm", {
  # rd-core:fit-stats-r2-lm-only
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, show_fit_stats = c("r2", "adj_r2"))
  s <- as_structured(out)
  b <- s$body
  expect_equal(
    b$B[b$Variable == "R²"],
    summary(fit)$r.squared,
    tolerance = 1e-10
  )
  expect_equal(
    b$B[b$Variable == "Adj. R²"],
    summary(fit)$adj.r.squared,
    tolerance = 1e-10
  )
  gfit <- glm(am ~ mpg, data = mt, family = binomial)
  for (tok in c("r2", "adj_r2", "omega2")) {
    expect_error(
      table_regression(gfit, show_fit_stats = c("nobs", tok)),
      class = "spicy_invalid_input"
    )
  }
})

test_that("change tokens render under nested = TRUE and are inert without it", {
  # rd-core:fit-stats-change-tokens-nested-only
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  o_nested <- table_regression(
    list(m1, m2),
    nested = TRUE,
    show_fit_stats = c("r2", "r2_change")
  )
  vars_nested <- trimws(
    as.data.frame(o_nested, stringsAsFactors = FALSE)$Variable
  )
  expect_true("ΔR²" %in% vars_nested)
  # Same request without nested = TRUE: the change row is simply absent
  # (inert), the table still renders.
  o_flat <- table_regression(
    list(m1, m2),
    show_fit_stats = c("r2", "r2_change")
  )
  vars_flat <- trimws(as.data.frame(o_flat, stringsAsFactors = FALSE)$Variable)
  expect_false("ΔR²" %in% vars_flat)
  expect_true("R²" %in% vars_flat)
})

test_that("digits controls B/SE/CI/t decimals; p and AIC are untouched", {
  # rd-core:digits-scope-default
  fit <- lm(mpg ~ wt + cyl, data = mt)
  o3 <- table_regression(
    fit,
    digits = 3L,
    show_columns = c("b", "se", "ci", "t", "p"),
    show_fit_stats = c("aic")
  )
  d3 <- as.data.frame(o3, stringsAsFactors = FALSE)
  wt_row <- d3[trimws(d3$Variable) == "wt", ]
  expect_match(trimws(wt_row$B), "^-?[0-9]+\\.[0-9]{3}$")
  expect_match(trimws(wt_row$SE), "^[0-9]+\\.[0-9]{3}$")
  expect_match(trimws(wt_row$t), "^-?[0-9]+\\.[0-9]{3}$")
  expect_match(
    trimws(wt_row$`95% CI`),
    "^\\[-?[0-9]+\\.[0-9]{3}, *-?[0-9]+\\.[0-9]{3}\\]$"
  )
  # p keeps p_digits (3 by default), AIC keeps ic_digits (1)
  expect_identical(trimws(wt_row$p), "<.001")
  aic_cell <- trimws(d3$B[trimws(d3$Variable) == "AIC"])
  expect_match(aic_cell, "^[0-9]+\\.[0-9]$")
})

test_that("p_digits formats rendered p cells APA-strict at any width", {
  # rd-core:p-digits-apa
  fit <- lm(mpg ~ wt + cyl, data = mt)
  d3 <- as.data.frame(table_regression(fit), stringsAsFactors = FALSE)
  d4 <- as.data.frame(
    table_regression(fit, p_digits = 4L),
    stringsAsFactors = FALSE
  )
  keep <- c("(Intercept)", "wt", "6", "8")
  p3 <- trimws(d3$p[trimws(d3$Variable) %in% keep])
  p4 <- trimws(d4$p[trimws(d4$Variable) %in% keep])
  # p_digits = 3 (default): wt is 2.13e-4 -> below threshold
  expect_identical(p3, c("<.001", "<.001", ".005", "<.001"))
  # p_digits = 4: threshold scales to <.0001, wt becomes .0002
  expect_identical(p4, c("<.0001", ".0002", ".0047", ".0010"))
  # APA: no leading zero anywhere
  expect_false(any(startsWith(c(p3, p4), "0.")))
})

test_that("fit_digits controls R² decimals while AIC keeps ic_digits", {
  # rd-core:fit-digits
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(
    fit,
    show_fit_stats = c("r2", "aic"),
    fit_digits = 4L
  )
  d <- as.data.frame(out, stringsAsFactors = FALSE)
  r2_cell <- trimws(d$B[trimws(d$Variable) == "R²"])
  aic_cell <- trimws(d$B[trimws(d$Variable) == "AIC"])
  expect_identical(
    r2_cell,
    format(round(summary(fit)$r.squared, 4), nsmall = 4)
  )
  expect_match(aic_cell, "^[0-9]+\\.[0-9]$")
  # default fit_digits = 2L
  expect_identical(formals(table_regression)$fit_digits, 2L)
  d2 <- as.data.frame(
    table_regression(fit, show_fit_stats = "r2"),
    stringsAsFactors = FALSE
  )
  expect_match(trimws(d2$B[trimws(d2$Variable) == "R²"]), "^0\\.[0-9]{2}$")
})

test_that("effect_size_digits scopes partial_f2 only (B keeps digits)", {
  # rd-core:effect-size-digits
  fit <- lm(mpg ~ wt + cyl, data = mt)
  expect_identical(formals(table_regression)$effect_size_digits, 2L)
  o2 <- table_regression(fit, show_columns = c("b", "partial_f2"))
  d2 <- as.data.frame(o2, stringsAsFactors = FALSE)
  wt2 <- d2[trimws(d2$Variable) == "wt", ]
  expect_match(trimws(wt2$`f²`), "^[0-9]+\\.[0-9]{2}$")
  o3 <- table_regression(
    fit,
    show_columns = c("b", "partial_f2"),
    effect_size_digits = 3L
  )
  d3 <- as.data.frame(o3, stringsAsFactors = FALSE)
  wt3 <- d3[trimws(d3$Variable) == "wt", ]
  expect_match(trimws(wt3$`f²`), "^[0-9]+\\.[0-9]{3}$")
  # B keeps the default 2-decimal `digits`
  expect_match(trimws(wt3$B), "^-?[0-9]+\\.[0-9]{2}$")
})

test_that("default output – documented class vector and rendering attributes", {
  # rd-core:return-class-attributes
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  expect_identical(
    class(out),
    c("spicy_regression_table", "spicy_table", "data.frame")
  )
  expect_type(attr(out, "title"), "character")
  expect_type(attr(out, "note"), "character")
  expect_identical(attr(out, "align"), "decimal")
  expect_identical(attr(out, "padding"), 0L)
})

test_that("provenance attributes outcome / model_ids are carried", {
  # rd-core:return-class-attributes (provenance half)
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  expect_identical(attr(out, "outcome"), "mpg")
  expect_identical(attr(out, "model_ids"), "M1")
  # Multi-model: one id and one outcome per model, in table order.
  fit2 <- lm(hp ~ wt, data = mt)
  out2 <- table_regression(list(A = fit, B = fit2))
  expect_identical(attr(out2, "model_ids"), c("A", "B"))
  expect_identical(attr(out2, "outcome"), c("mpg", "hp"))
  # The attributes agree with the per-row provenance in spicy_long.
  long <- attr(out2, "spicy_long")
  expect_identical(attr(out2, "model_ids"), unique(long$model_id))
})

test_that("title and note attributes are post-processable before printing", {
  # rd-core:i18n-attrs-postprocessable
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  expect_type(attr(out, "title"), "character")
  expect_type(attr(out, "note"), "character")
  attr(out, "title") <- "Titre personnalise"
  attr(out, "note") <- "Note. Remarque personnalisee."
  lines <- capture.output(print(out))
  expect_identical(lines[1], "Titre personnalise")
  expect_true("Note. Remarque personnalisee." %in% lines)
})

test_that("factor_layout grouped applies to character predictors", {
  # rd-core:factor-layout-scope (character half)
  df <- mtcars
  df$gear_chr <- as.character(df$gear)
  fit <- lm(mpg ~ wt + gear_chr, data = df)
  out <- table_regression(fit)
  vars <- as.data.frame(out, stringsAsFactors = FALSE)$Variable
  expect_true("gear_chr:" %in% vars)
  expect_true(any(grepl("^  4$", vars)))
  ref_idx <- grep("\\(ref\\.\\)", vars)
  expect_length(ref_idx, 1L)
  expect_match(vars[ref_idx], "^  3 \\(ref\\.\\)$")
})

test_that("factor_layout grouped applies to logical predictors", {
  # rd-core:factor-layout-scope (logical half)
  df <- mtcars
  df$am_lgl <- df$am == 1
  fit <- lm(mpg ~ wt + am_lgl, data = df)
  out <- table_regression(fit)
  vars <- as.data.frame(out, stringsAsFactors = FALSE)$Variable
  expect_true("am_lgl:" %in% vars)
  expect_true(any(grepl("^  TRUE$", vars)))
  ref_idx <- grep("\\(ref\\.\\)", vars)
  expect_length(ref_idx, 1L)
  expect_match(vars[ref_idx], "^  FALSE \\(ref\\.\\)$")
  # No flat `am_lglTRUE` row survives alongside the grouped layout.
  expect_false("am_lglTRUE" %in% vars)
  # AME rows align on the same grouped rows (no orphan `am_lgl` row).
  skip_if_not_installed("marginaleffects")
  out_ame <- table_regression(fit, show_columns = c("b", "ame"))
  d <- as.data.frame(out_ame, stringsAsFactors = FALSE)
  expect_false("am_lgl" %in% trimws(d$Variable))
  true_row <- d[trimws(d$Variable) == "TRUE", ]
  expect_identical(nrow(true_row), 1L)
  expect_true(nzchar(trimws(true_row$B)))
  expect_true(nzchar(trimws(true_row$AME)))
})

test_that("reference_label defaults to '(ref.)' and only acts in row mode", {
  # rd-core:reference-label-default
  fit <- lm(mpg ~ wt + cyl, data = mt)
  expect_identical(formals(table_regression)$reference_label, "(ref.)")
  o_row <- table_regression(fit, reference_label = "(base)")
  expect_true(any(grepl(
    "(base)",
    as.data.frame(o_row, stringsAsFactors = FALSE)$Variable,
    fixed = TRUE
  )))
  # annotation / footer / none ignore the argument entirely: same
  # output as with the default label.
  for (style in c("annotation", "footer", "none")) {
    o_custom <- suppressMessages(table_regression(
      fit,
      reference_style = style,
      reference_label = "(base)"
    ))
    o_default <- suppressMessages(table_regression(
      fit,
      reference_style = style
    ))
    expect_identical(
      as.data.frame(o_custom, stringsAsFactors = FALSE),
      as.data.frame(o_default, stringsAsFactors = FALSE)
    )
    expect_identical(attr(o_custom, "note"), attr(o_default, "note"))
    expect_false(any(grepl(
      "(base)",
      as.data.frame(o_custom, stringsAsFactors = FALSE)$Variable,
      fixed = TRUE
    )))
  }
})

test_that("align – CI anchors are each column-aligned in decimal mode", {
  # rd-core:align-decimal-ci-anchors (per-anchor positions)
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit)
  ci <- as.data.frame(out, stringsAsFactors = FALSE)$`95% CI`
  ci <- ci[grepl("[0-9]", ci)]
  anchor_pos <- function(cells, pattern, which_match = 1L) {
    vapply(
      cells,
      function(x) {
        hits <- gregexpr(pattern, x, fixed = TRUE)[[1]]
        as.integer(hits[which_match])
      },
      integer(1)
    )
  }
  # Left bracket, LL decimal point, comma, UL decimal point, right
  # bracket: each independently at a fixed character position.
  expect_length(unique(anchor_pos(ci, "[")), 1L)
  expect_length(unique(anchor_pos(ci, ".", 1L)), 1L)
  expect_length(unique(anchor_pos(ci, ",")), 1L)
  expect_length(unique(anchor_pos(ci, ".", 2L)), 1L)
  expect_length(unique(anchor_pos(ci, "]")), 1L)
})

test_that("align – 'right' right-justifies, 'center' centres, in print", {
  # rd-core:align-decimal-ci-anchors ('center' / 'right' behaviour)
  fit <- lm(mpg ~ wt + cyl, data = mt)
  stats <- c("r2", "aic", "nobs")
  last_nonspace <- function(line) max(which(strsplit(line, "")[[1]] != " "))
  first_value_char <- function(line) {
    stat_side <- sub("^[^│]*│", "", line)
    as.integer(regexpr("[^ ]", stat_side))
  }
  fit_stat_lines <- function(o) {
    lines <- capture.output(print(o))
    lines[grepl("^ (R²|AIC|n) ", lines)]
  }
  o_right <- table_regression(fit, align = "right", show_fit_stats = stats)
  o_dec <- table_regression(fit, align = "decimal", show_fit_stats = stats)
  o_ctr <- table_regression(fit, align = "center", show_fit_stats = stats)
  r_lines <- fit_stat_lines(o_right)
  d_lines <- fit_stat_lines(o_dec)
  c_lines <- fit_stat_lines(o_ctr)
  expect_length(r_lines, 3L)
  # right: 0.84 / 156.6 / 32 all end at the same column
  expect_length(unique(vapply(r_lines, last_nonspace, 1L)), 1L)
  # decimal: they align on the decimal mark, so the right edges differ
  expect_gt(length(unique(vapply(d_lines, last_nonspace, 1L))), 1L)
  # center: the narrow 'n' value starts further left than under 'right'
  n_right <- r_lines[grepl("^ n ", r_lines)]
  n_ctr <- c_lines[grepl("^ n ", c_lines)]
  expect_lt(first_value_char(n_ctr), first_value_char(n_right))
})

test_that("weights come from the fit – no weights argument", {
  # rd-core:weights-from-fit (formals half)
  expect_false("weights" %in% names(formals(table_regression)))
  # weighted_nobs = sum of the fit's weights for a weighted lm
  df <- mtcars
  df$w <- seq_len(nrow(df)) / 10
  wfit <- lm(mpg ~ wt, data = df, weights = w)
  out <- table_regression(wfit, show_fit_stats = c("nobs", "weighted_nobs"))
  s <- as_structured(out)
  expect_equal(
    s$body$B[s$body$Variable == "Weighted n"],
    sum(df$w),
    tolerance = 1e-8
  )
})

test_that("AME extraction honours the fit's weights", {
  # rd-core:weights-from-fit (AME half)
  skip_if_not_installed("marginaleffects")
  set.seed(7)
  df <- data.frame(x = rnorm(120), z = rnorm(120))
  df$y <- rbinom(120, 1, plogis(0.4 * df$x - 0.2 * df$z))
  df$w <- ifelse(df$x > 0, 5, 1)
  # glm: response-scale AME = weighted average of the unit slopes.
  gfit <- glm(y ~ x + z, data = df, family = binomial, weights = w)
  s <- as_structured(table_regression(gfit, show_columns = c("b", "ame")))
  orc <- as.data.frame(
    marginaleffects::avg_slopes(gfit, wts = weights(gfit), df = Inf)
  )
  idx <- match(c("x", "z"), s$body$Variable)
  expect_equal(
    s$body$AME[idx],
    orc$estimate[match(c("x", "z"), orc$term)],
    tolerance = 1e-8
  )
  # SE / CI / p of the AME follow the same weighted computation.
  long <- table_regression(
    gfit,
    show_columns = c("b", "ame", "ame_se"),
    output = "long"
  )
  ame_long <- long[long$estimate_type == "ame", ]
  expect_equal(
    ame_long$std.error[match(c("x", "z"), ame_long$term)],
    orc$std.error[match(c("x", "z"), orc$term)],
    tolerance = 1e-8
  )
  # The weighted AME differs from the unweighted average, so the
  # oracle above is discriminating.
  orc_u <- as.data.frame(marginaleffects::avg_slopes(gfit, df = Inf))
  expect_gt(
    abs(orc$estimate[orc$term == "x"] - orc_u$estimate[orc_u$term == "x"]),
    1e-6
  )
  # lm: same contract on the linear path.
  lfit <- lm(y ~ x + z, data = df, weights = w)
  s2 <- as_structured(table_regression(lfit, show_columns = c("b", "ame")))
  orc2 <- as.data.frame(
    marginaleffects::avg_slopes(lfit, wts = weights(lfit))
  )
  expect_equal(
    s2$body$AME[match(c("x", "z"), s2$body$Variable)],
    orc2$estimate[match(c("x", "z"), orc2$term)],
    tolerance = 1e-8
  )
})

test_that("weighted AME identical under na.omit and na.exclude", {
  # Regression (delta review D2): stats::weights() returns the
  # naresid-PADDED vector under na.exclude (NA at dropped rows), which
  # tripped the finite-weights guard and silently reverted the AME to
  # the unweighted average. The helper now strips the padding.
  skip_if_not_installed("marginaleffects")
  df <- mtcars
  set.seed(7)
  df$w <- runif(nrow(df), 0.5, 2)
  df$wt[5] <- NA
  # suppressWarnings: continuous weights trigger the expected
  # "non-integer #successes" binomial fit warning.
  fit_om <- suppressWarnings(glm(
    am ~ wt + hp,
    data = df,
    family = binomial,
    weights = w,
    na.action = na.omit
  ))
  fit_ex <- suppressWarnings(glm(
    am ~ wt + hp,
    data = df,
    family = binomial,
    weights = w,
    na.action = na.exclude
  ))
  w_ex <- spicy:::.spicy_ame_fit_wts(fit_ex)
  expect_identical(w_ex, spicy:::.spicy_ame_fit_wts(fit_om))
  expect_length(w_ex, nrow(df) - 1L)
  s_om <- as_structured(table_regression(fit_om, show_columns = c("b", "ame")))
  s_ex <- as_structured(table_regression(fit_ex, show_columns = c("b", "ame")))
  idx <- match(c("wt", "hp"), s_om$body$Variable)
  expect_equal(s_ex$body$AME[idx], s_om$body$AME[idx], tolerance = 1e-10)
  # The shared value IS the weighted average (discriminating oracle).
  orc <- as.data.frame(
    marginaleffects::avg_slopes(fit_om, wts = stats::weights(fit_om), df = Inf)
  )
  expect_equal(
    s_ex$body$AME[idx],
    orc$estimate[match(c("wt", "hp"), orc$term)],
    tolerance = 1e-8
  )
})

test_that("polr frequency weights reach the per-category AME", {
  # Regression (delta review D4): MASS::polr stores no weights
  # component, so stats::weights() is empty and the per-category AME
  # averaged with equal weights while B / SE were weight-aware. The
  # helper now recovers the frequency weights from the model frame.
  skip_if_not_installed("MASS")
  skip_if_not_installed("marginaleffects")
  # Hess = TRUE avoids the "Re-fitting to get Hessian" warning when
  # the AME path queries the vcov.
  fit <- MASS::polr(
    Sat ~ Infl + Cont,
    weights = Freq,
    data = MASS::housing,
    Hess = TRUE
  )
  w <- spicy:::.spicy_ame_fit_wts(fit)
  expect_equal(w, as.numeric(MASS::housing$Freq))
  ame <- spicy:::.compute_ame_rows_for_frame(fit, ci_level = 0.95)
  orc <- as.data.frame(
    marginaleffects::avg_slopes(fit, wts = MASS::housing$Freq)
  )
  key_ame <- paste(ame$term, ame$outcome_level)
  key_orc <- paste(
    paste0(orc$term, sub(" - .*$", "", orc$contrast)),
    orc$group
  )
  expect_setequal(key_ame, key_orc)
  expect_equal(
    ame$estimate[match(key_orc, key_ame)],
    orc$estimate,
    tolerance = 1e-8
  )
  # Pinned on the audit evidence: the weighted average, not the
  # unweighted one.
  expect_equal(
    ame$estimate[key_ame == "InflHigh Low"],
    -0.266416,
    tolerance = 1e-4
  )
  orc_u <- as.data.frame(marginaleffects::avg_slopes(fit))
  expect_gt(
    abs(
      ame$estimate[key_ame == "InflHigh Low"] -
        orc_u$estimate[
          orc_u$term == "Infl" &
            orc_u$contrast == "High - Low" &
            orc_u$group == "Low"
        ]
    ),
    1e-4
  )
})


# ============================================================================
# Phase 3 matrix – rd-methods / rd-uv-estimands: output contract (lot T3)
# ============================================================================

test_that("side-effect outputs return the table invisibly", {
  # rd-methods:output-class-mapping (invisible(x) half; the gt /
  # flextable / tinytable / data.frame class halves are pinned in the
  # per-output tests above)
  fit <- lm(mpg ~ wt, data = mt)
  skip_if_not_installed("openxlsx2")
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  expect_invisible(table_regression(fit, output = "excel", excel_path = path))
  expect_true(file.exists(path))
})

test_that("output = 'word' returns the table invisibly", {
  # rd-methods:output-class-mapping (invisible(x), word engine)
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  fit <- lm(mpg ~ wt, data = mt)
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  expect_invisible(table_regression(fit, output = "word", word_path = path))
  expect_true(file.exists(path))
})

test_that("output = 'long' returns the documented tbl_df", {
  # rd-methods:output-class-mapping (long half): the Rd \value and the
  # `output` argument docs promise a long-format tibble.
  fit <- lm(mpg ~ wt + cyl, data = mt)
  lo <- table_regression(fit, output = "long")
  expect_s3_class(lo, "tbl_df")
  expect_true(all(
    c("model_id", "term", "estimate_type", "estimate", "std.error") %in%
      names(lo)
  ))
})

test_that("output = 'flextable' carries the spicy_flextable tag", {
  # rd-methods:flextable-output-tagged-class (table_regression half;
  # the table_continuous_lm half is pinned in test-tclm_notes.R)
  skip_if_not_installed("flextable")
  fit <- lm(mpg ~ wt, data = mt)
  ft <- table_regression(fit, output = "flextable")
  expect_identical(class(ft), c("spicy_flextable", "flextable"))
})

test_that("console shows bare deduped labels; data.frame keeps unique names", {
  # rd-methods:print-header-display-labels-deduped
  skip_if_not_installed("marginaleffects")
  fit <- lm(mpg ~ wt + hp, data = mt)
  out <- table_regression(
    fit,
    show_columns = c("b", "ci", "p", "ame", "ame_ci", "ame_p")
  )
  con <- paste(capture.output(print(out)), collapse = "\n")
  expect_false(grepl("CI.2", con, fixed = TRUE))
  expect_false(grepl("p.2", con, fixed = TRUE))
  nms <- names(as.data.frame(out, stringsAsFactors = FALSE))
  expect_true("95% CI.2" %in% nms)
  expect_true("p.2" %in% nms)
  expect_identical(sum(nms == "95% CI.2"), 1L)
})

test_that("print honours the padding attr unless overridden at print time", {
  # rd-methods:print-honors-padding-attr
  fit <- lm(mpg ~ wt, data = mt)
  t0 <- table_regression(fit, padding = 0L)
  t4 <- table_regression(fit, padding = 4L)
  expect_lt(
    max(nchar(capture.output(print(t0)))),
    max(nchar(capture.output(print(t4))))
  )
  # print(x, padding = ) overrides the stored call-site attribute.
  expect_identical(
    capture.output(print(t0, padding = 4L)),
    capture.output(print(t4))
  )
})

test_that("boot_n – full validation domain and the 1000-replicate default", {
  # rd-uv-estimands:boot-n-default-1000-validated (complements
  # rd-core:boot-n-default above: -1 / 2.5 refusals, the resolved
  # default replicate count, and the boot_n = 50 vs 1000 difference)
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, vcov = "bootstrap", boot_n = -1),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, vcov = "bootstrap", boot_n = 2.5),
    class = "spicy_invalid_input"
  )
  set.seed(4)
  b50a <- table_regression(fit, vcov = "bootstrap", boot_n = 50)
  set.seed(4)
  b50b <- table_regression(fit, vcov = "bootstrap", boot_n = 50)
  # Same seed, same boot_n: reproducible SEs; the footer discloses the
  # replicate count actually used.
  expect_identical(as_structured(b50a)$body$SE, as_structured(b50b)$body$SE)
  expect_match(attr(b50a, "note"), "50 replicates", fixed = TRUE)
  # Default boot_n resolves to 1000 replicates end-to-end.
  set.seed(4)
  b1000 <- table_regression(fit, vcov = "bootstrap")
  expect_match(attr(b1000, "note"), "1000 replicates", fixed = TRUE)
  expect_false(isTRUE(all.equal(
    as_structured(b50a)$body$SE,
    as_structured(b1000)$body$SE
  )))
})


# Phase 3 matrix – vignettes-news:align-auto-removed (lot T4)

test_that("align = 'auto' is removed: classed error, not a silent default", {
  fit <- lm(mpg ~ wt, data = mtcars)
  expect_error(
    table_regression(fit, align = "auto"),
    class = "spicy_invalid_input"
  )
})


# Phase 3 matrix – vignettes-news:ci-header-tracks-level (lot T4)

test_that("ci_level relabels the CI header and widens the bounds", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  tbl <- table_regression(fit, ci_level = 0.99)
  out <- paste(capture.output(print(tbl)), collapse = "\n")
  expect_match(out, "99% CI", fixed = TRUE)
  expect_false(grepl("95% CI", out, fixed = TRUE))
  # Bounds are the level-0.99 confint, not the default 0.95 ones.
  td <- broom::tidy(tbl)
  ci99 <- confint(fit, level = 0.99)
  expect_equal(td$conf.low[td$term == "wt"], ci99["wt", 1], tolerance = 1e-10)
  expect_equal(td$conf.high[td$term == "wt"], ci99["wt", 2], tolerance = 1e-10)
  ci95 <- confint(fit, level = 0.95)
  expect_gt(ci95["wt", 1] - ci99["wt", 1], 0)
})


# Phase 3 matrix – vignettes-news:eta2-omega2-shared-steiger-ci and
# vignettes-news:effect-size-broadcast-factors (lot T4)

test_that("eta2 and omega2 CI cells are identical; reference row stays blank", {
  set.seed(7)
  n <- 60
  d <- data.frame(
    A = factor(sample(c("a", "b", "c"), n, TRUE, prob = c(.5, .3, .2))),
    B = factor(sample(c("u", "v"), n, TRUE, prob = c(.6, .4)))
  )
  d$y <- rnorm(n) + as.numeric(d$A) + 0.5 * (d$B == "v")
  fit <- lm(y ~ A + B, data = d)
  tbl <- table_regression(
    fit,
    show_columns = c(
      "b",
      "partial_eta2",
      "partial_eta2_ci",
      "partial_omega2",
      "partial_omega2_ci"
    )
  )
  df <- as.data.frame(tbl)
  eta_ci_col <- grep("η² .*CI", names(df))
  om_ci_col <- grep("ω² .*CI", names(df))
  expect_length(eta_ci_col, 1L)
  expect_length(om_ci_col, 1L)
  # Single Steiger noncentral-F interval shared by both estimands
  # (MBESS convention): the rendered cells are identical row by row.
  expect_identical(df[[eta_ci_col]], df[[om_ci_col]])
  # The k-1 non-reference dummies of A broadcast one joint value.
  eta_col <- setdiff(grep("η²", names(df)), eta_ci_col)
  rows_b <- which(trimws(df$Variable) == "b")
  rows_c <- which(trimws(df$Variable) == "c")
  expect_identical(df[[eta_col]][rows_b], df[[eta_col]][rows_c])
  expect_match(df[[eta_col]][rows_b], "[0-9]")
  # Reference rows leave every effect-size cell blank (the B cell
  # carries the reference dash).
  ref_rows <- grep("(ref.)", df$Variable, fixed = TRUE)
  expect_length(ref_rows, 2L)
  for (cl in c(eta_ci_col, om_ci_col, eta_col)) {
    expect_false(any(grepl("[0-9]", df[[cl]][ref_rows])))
  }
})


# ============================================================================
# the block caption layer
# ============================================================================

test_that("both bodies print the block caption, not the block identity", {
  # At the English default the identity and the caption are the same
  # string, so nothing else in the suite can tell whether either body
  # reads the caption layer at all. Mocking the resolver makes the
  # difference visible -- and it has to be visible in BOTH bodies: the
  # console renders the character body, the six rich engines render the
  # typed one, and those were two independent producers of this cell
  # until `.reg_factor_header_text()`.
  skip_if_not_installed("ordinal")
  d <- data.frame(
    y = factor(
      rep(c("low", "mid", "high"), 30),
      levels = c("low", "mid", "high"),
      ordered = TRUE
    ),
    x = rep(seq(1, 9), 10),
    stringsAsFactors = FALSE
  )
  fit <- ordinal::clm(y ~ x, data = d)
  local_mocked_bindings(.reg_block_label = function(term) {
    paste0("<", term, ">")
  })

  txt <- capture.output(print(table_regression(fit, show_thresholds = TRUE)))
  expect_true(any(grepl("<Thresholds>:", txt, fixed = TRUE)))
  expect_false(any(grepl(" Thresholds:", txt, fixed = TRUE)))

  s <- as_structured(table_regression(fit, show_thresholds = TRUE))
  expect_true(any(s$body$Variable == "<Thresholds>:"))
  # The IDENTITY is untouched: the typed view still publishes the frozen
  # English word, which is what user code matches on.
  expect_true(any(s$body$.variable == "Thresholds"))
})
