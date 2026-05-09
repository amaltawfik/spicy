# Tests for the AME-Satterthwaite path (Q14b — first in R for lm under
# CR* variance). Path A in regression_ame.R: builds the closed-form
# linear contrast representing each AME and inverts it through
# clubSandwich::linear_contrast() with Satterthwaite-corrected df.

skip_if_no_clubsandwich <- function() {
  testthat::skip_if_not_installed("clubSandwich")
}

# ---- Test fixtures -------------------------------------------------------

mk_clustered_data <- function(seed = 1L, n = 200L, n_clusters = 10L) {
  set.seed(seed)
  data.frame(
    y = rnorm(n),
    x = rnorm(n),
    g = factor(sample(letters[1:n_clusters], n, replace = TRUE)),
    cluster = factor(sample(seq_len(n_clusters), n, replace = TRUE))
  )
}


# ============================================================================
# Closed-form contrast builders
# ============================================================================

test_that("build_numeric_ame_contrast — colMeans(model.matrix at v+1 minus at v)", {
  fit <- lm(y ~ x + g, data = mk_clustered_data())
  cv <- spicy:::build_numeric_ame_contrast(fit, "x")
  # Length matches the design matrix
  expect_equal(length(cv), length(coef(fit)))
  # For an additive linear model, the AME of x is just its coefficient
  # → contrast vector has 1 in position "x" and 0 elsewhere.
  expect_equal(cv[["x"]], 1, tolerance = 1e-12)
  for (nm in setdiff(names(cv), "x")) {
    expect_equal(unname(cv[[nm]]), 0, tolerance = 1e-12)
  }
})

test_that("build_factor_ame_contrast — average of (lvl - ref) design rows", {
  fit <- lm(y ~ x + g, data = mk_clustered_data())
  cv <- spicy:::build_factor_ame_contrast(fit, "g", "b", "a")
  # For an additive linear model with reference coding, the AME of
  # level "b" vs "a" is the gb coefficient → 1 in gb, 0 elsewhere.
  expect_equal(unname(cv[["gb"]]), 1, tolerance = 1e-12)
  expect_equal(unname(cv[["x"]]), 0, tolerance = 1e-12)
})

test_that("build_ame_contrasts_for_predictor — numeric: 1 contrast", {
  fit <- lm(y ~ x + g, data = mk_clustered_data())
  out <- spicy:::build_ame_contrasts_for_predictor(fit, "x")
  expect_equal(length(out), 1L)
  expect_equal(out[[1]]$term_id, "x")
})

test_that("build_ame_contrasts_for_predictor — factor: k-1 contrasts", {
  fit <- lm(y ~ x + g, data = mk_clustered_data())
  out <- spicy:::build_ame_contrasts_for_predictor(fit, "g")
  # 10 levels → 9 non-reference contrasts
  expect_equal(length(out), 9L)
  term_ids <- vapply(out, `[[`, character(1), "term_id")
  expect_setequal(term_ids, paste0("g", letters[2:10]))
})


# ============================================================================
# extract_ame_satterthwaite — end-to-end
# ============================================================================

test_that("extract_ame_satterthwaite — produces one row per non-reference contrast", {
  skip_if_no_clubsandwich()
  df <- mk_clustered_data()
  fit <- lm(y ~ x + g, data = df)
  rows <- spicy:::extract_ame_satterthwaite(
    fit, vcov_type = "CR2", cluster = df$cluster, ci_level = 0.95,
    model_id = "M1", outcome = "y"
  )
  # 1 numeric AME (x) + 9 factor contrasts (gb..gj)
  expect_equal(nrow(rows), 10L)
  expect_true(all(rows$estimate_type == "AME"))
  expect_true(all(rows$test_type == "t"))
})

test_that("extract_ame_satterthwaite — t-stat = est/se, p from t-dist + df_Satt", {
  skip_if_no_clubsandwich()
  df <- mk_clustered_data()
  fit <- lm(y ~ x + g, data = df)
  rows <- spicy:::extract_ame_satterthwaite(
    fit, vcov_type = "CR2", cluster = df$cluster, ci_level = 0.95,
    model_id = "M1", outcome = "y"
  )
  # Recompute t and p locally and compare
  for (i in seq_len(nrow(rows))) {
    expect_equal(rows$statistic[i], rows$estimate[i] / rows$se[i],
                 tolerance = 1e-12)
    expect_equal(rows$p_value[i],
                 2 * pt(abs(rows$statistic[i]),
                        df = rows$df[i], lower.tail = FALSE),
                 tolerance = 1e-12)
  }
})

test_that("extract_ame_satterthwaite — matches direct clubSandwich call (oracle)", {
  skip_if_no_clubsandwich()
  df <- mk_clustered_data()
  fit <- lm(y ~ x + g, data = df)
  ours <- spicy:::extract_ame_satterthwaite(
    fit, vcov_type = "CR2", cluster = df$cluster, ci_level = 0.95,
    model_id = "M1", outcome = "y"
  )
  # Direct oracle: build contrasts the same way and call linear_contrast
  # ourselves. Estimates + SE + df + CI must match to machine epsilon
  # because Path A is a thin wrapper.
  contrasts <- list()
  for (v in c("x", "g")) {
    contrasts <- c(contrasts,
                   spicy:::build_ame_contrasts_for_predictor(fit, v))
  }
  cmat <- do.call(rbind, lapply(contrasts, `[[`, "vector"))
  rownames(cmat) <- vapply(contrasts, `[[`, character(1), "term_id")
  oracle <- clubSandwich::linear_contrast(
    fit, vcov = "CR2", cluster = df$cluster,
    contrasts = cmat, test = "Satterthwaite", level = 0.95
  )
  expect_equal(ours$estimate, oracle$Est, tolerance = 1e-12)
  expect_equal(ours$se,       oracle$SE,  tolerance = 1e-12)
  expect_equal(ours$df,       oracle$df,  tolerance = 1e-12)
  expect_equal(ours$ci_low,   oracle$CI_L, tolerance = 1e-12)
  expect_equal(ours$ci_high,  oracle$CI_U, tolerance = 1e-12)
})

test_that("extract_ame_satterthwaite — empty when formula has no main-effect predictors", {
  skip_if_no_clubsandwich()
  df <- mk_clustered_data()
  # Pure interaction term only — no main effects to compute AME for
  fit <- lm(y ~ x:g, data = df)
  rows <- spicy:::extract_ame_satterthwaite(
    fit, vcov_type = "CR2", cluster = df$cluster, ci_level = 0.95,
    model_id = "M1", outcome = "y"
  )
  expect_equal(nrow(rows), 0L)
})

test_that("extract_ame_satterthwaite — errors on function-call predictors", {
  skip_if_no_clubsandwich()
  df <- mk_clustered_data()
  fit <- lm(y ~ poly(x, 2), data = df)
  expect_error(
    spicy:::extract_ame_satterthwaite(
      fit, vcov_type = "CR2", cluster = df$cluster, ci_level = 0.95,
      model_id = "M1", outcome = "y"
    ),
    "function-call predictors"
  )
})


# ============================================================================
# extract_ame_rows — Path A vs Path B dispatcher
# ============================================================================

test_that("extract_ame_rows — Path A taken when use_ame_satterthwaite = TRUE", {
  skip_if_no_clubsandwich()
  df <- mk_clustered_data()
  fit <- lm(y ~ x + g, data = df)
  vc <- clubSandwich::vcovCR(fit, type = "CR2", cluster = df$cluster)
  rows <- spicy:::extract_ame_rows(
    fit, vc = vc, vcov_type = "CR2", cluster = df$cluster,
    ci_level = 0.95, use_ame_satterthwaite = TRUE,
    model_id = "M1", outcome = "y"
  )
  expect_true(nrow(rows) > 0L)
  expect_true(all(rows$test_type == "t"))
  # Path A returns finite Satterthwaite df (typically not the integer
  # df.residual)
  expect_true(all(is.finite(rows$df)))
})

test_that("extract_ame_rows — Path A failure on poly() falls back to Path B", {
  skip_if_no_clubsandwich()
  skip_if_not_installed("marginaleffects")
  df <- mk_clustered_data()
  fit <- lm(y ~ poly(x, 2), data = df)
  vc <- clubSandwich::vcovCR(fit, type = "CR2", cluster = df$cluster)
  # Should warn about Satterthwaite fallback then succeed via Path B
  rows <- suppressWarnings(spicy:::extract_ame_rows(
    fit, vc = vc, vcov_type = "CR2", cluster = df$cluster,
    ci_level = 0.95, use_ame_satterthwaite = TRUE,
    model_id = "M1", outcome = "y"
  ))
  expect_true(nrow(rows) > 0L)
})


# ============================================================================
# Integration — table_regression() with the affirmative footer
# ============================================================================

test_that("table_regression — CR2 + AME triggers the Satterthwaite footer", {
  skip_if_no_clubsandwich()
  df <- mk_clustered_data()
  fit <- lm(y ~ x + g, data = df)
  out <- table_regression(
    fit, vcov = "CR2", cluster = df$cluster,
    show_columns = c("B", "SE", "AME", "p")
  )
  note <- attr(out, "note")
  expect_match(note, "AME inference: t-distribution with Satterthwaite-corrected df")
  expect_match(note, "clubSandwich::linear_contrast")
})
