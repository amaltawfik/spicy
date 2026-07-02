# ---------------------------------------------------------------------------
# Phase 7c7d tests: user-facing API args
#   show_re      -- TRUE / FALSE  (suppress the whole RE panel)
#   re_scale     -- "sd" (default) / "variance"
#   re_columns   -- subset of c("est", "se", "ci"); "est" is mandatory
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_lmer_api <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}


# ---- 1. show_re = FALSE suppresses the panel ----------------------------

test_that("show_re = FALSE removes the Random effects panel from output", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_api()
  out <- capture.output(print(table_regression(fit, show_re = FALSE)))
  combined <- paste(out, collapse = "\n")
  expect_false(grepl("Random effects", combined, fixed = TRUE))
  expect_false(grepl("Subject (Intercept)", combined, fixed = TRUE))
})

test_that("show_re = TRUE (default) keeps the Random effects panel", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_api()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Random effects", fixed = TRUE)
})

test_that("show_re = FALSE is a no-op on non-mixed fits (no panel anyway)", {
  fit <- lm(mpg ~ wt, data = mtcars)
  out <- capture.output(print(table_regression(fit, show_re = FALSE)))
  combined <- paste(out, collapse = "\n")
  expect_false(grepl("Random effects", combined, fixed = TRUE))
})

test_that("build_random_effects_footer_block_from_frames(show_re=FALSE) -> NULL", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_api()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(
    list(fr), show_re = FALSE)
  expect_null(out)
})


# ---- 2. re_scale = "variance" shows variance values ---------------------

test_that("re_scale = 'variance' shows variance values, not SD", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_api()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  var_value <- vc$variance[vc$group == "Subject"]
  sd_value  <- sqrt(var_value)
  out <- capture.output(print(table_regression(fit, re_scale = "variance")))
  combined <- paste(out, collapse = "\n")
  # Variance value should appear, SD value should not.
  expect_match(combined, sprintf("%.2f", var_value), fixed = TRUE)
  expect_false(grepl(sprintf("\\s%.2f\\s", sd_value), combined))
})

test_that("re_scale = 'sd' (default) shows SD values", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_api()
  fr <- as_regression_frame(fit)
  sd_value <- sqrt(fr$info$random_effects$variance_components$variance[
    fr$info$random_effects$variance_components$group == "Subject"])
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, sprintf("%.2f", sd_value), fixed = TRUE)
})

test_that("invalid re_scale value triggers match.arg error", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_api()
  expect_error(table_regression(fit, re_scale = "nonsense"))
})




# ---- 3. re_columns subsets the RE-row columns (rows layout) -------------
# In the rows layout the SE / CI columns are shared with the fixed effects;
# re_columns em-dashes them on the RE rows only. A cell "has a number" when it
# shows a value, and is em-dashed / blank otherwise -- tested on the body.

.re_cell_has_num <- function(df, col) {
  r <- df[grepl("Subject (Intercept)", df$Variable, fixed = TRUE), ,
          drop = FALSE]
  grepl("[0-9]", r[[col]][1])
}

test_that("re_columns = 'est' em-dashes SE and CI on the RE rows", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_api()
  df <- table_regression(fit, show_columns = c("b", "se", "ci"),
                         re_columns = "est", output = "data.frame")
  se_col <- grep("SE", names(df), value = TRUE)[1]
  ci_col <- grep("CI", names(df), value = TRUE)[1]
  expect_false(.re_cell_has_num(df, se_col))
  expect_false(.re_cell_has_num(df, ci_col))
  # a fixed-effect row keeps its SE (re_columns only touches the RE rows)
  expect_true(grepl("[0-9]", trimws(df[df$Variable == "Days", se_col])))
})

test_that("re_columns = c('est', 'se') keeps SE, em-dashes CI on the RE rows", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_api()
  df <- table_regression(fit, show_columns = c("b", "se", "ci"),
                         re_columns = c("est", "se"), output = "data.frame")
  se_col <- grep("SE", names(df), value = TRUE)[1]
  ci_col <- grep("CI", names(df), value = TRUE)[1]
  expect_true(.re_cell_has_num(df, se_col))
  expect_false(.re_cell_has_num(df, ci_col))
})

test_that("re_columns = c('est', 'ci') keeps CI, em-dashes SE on the RE rows", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_api()
  df <- table_regression(fit, show_columns = c("b", "se", "ci"),
                         re_columns = c("est", "ci"), output = "data.frame")
  se_col <- grep("SE", names(df), value = TRUE)[1]
  ci_col <- grep("CI", names(df), value = TRUE)[1]
  expect_false(.re_cell_has_num(df, se_col))
  expect_true(.re_cell_has_num(df, ci_col))
})

test_that("re_columns default (all three) shows estimate + SE + CI on RE rows", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_api()
  df <- table_regression(fit, show_columns = c("b", "se", "ci"),
                         output = "data.frame")
  se_col <- grep("SE", names(df), value = TRUE)[1]
  ci_col <- grep("CI", names(df), value = TRUE)[1]
  expect_true(.re_cell_has_num(df, se_col))
  expect_true(.re_cell_has_num(df, ci_col))
})


# ---- 4. re_columns must include "est" -----------------------------------

test_that("re_columns must include 'est' (the estimate is mandatory)", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_api()
  expect_error(
    table_regression(fit, re_columns = c("se", "ci")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, re_columns = "se"),
    class = "spicy_invalid_input"
  )
})

test_that("re_columns rejects unknown tokens", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_api()
  expect_error(table_regression(fit, re_columns = c("est", "pvalue")))
  expect_error(table_regression(fit, re_columns = c("est", "junk")))
})


# ---- 5. Helpers in isolation --------------------------------------------

test_that(".validate_re_columns happy path returns input", {
  expect_identical(spicy:::.validate_re_columns(c("est", "se", "ci")),
                   c("est", "se", "ci"))
  expect_identical(spicy:::.validate_re_columns("est"), "est")
  expect_identical(spicy:::.validate_re_columns(c("est", "se")),
                   c("est", "se"))
})

test_that(".validate_re_columns rejects 'est'-missing input", {
  expect_error(spicy:::.validate_re_columns(c("se", "ci")),
               class = "spicy_invalid_input")
})

test_that(".re_components_on_scale('variance') is a no-op", {
  vc <- data.frame(
    group     = c("Subject", "Residual"),
    term      = c("(Intercept)", ""),
    variance  = c(1378.18, 960.46),
    sd        = c(37.12,    30.99),
    corr      = c(NA_real_, NA_real_),
    std_error = c(13.5, 5.1),
    ci_lower  = c(1100, 850),
    ci_upper  = c(1700, 1100),
    stringsAsFactors = FALSE
  )
  out <- spicy:::.re_components_on_scale(vc, "variance")
  expect_equal(out$variance, vc$variance)
  expect_equal(out$std_error, vc$std_error)
})
