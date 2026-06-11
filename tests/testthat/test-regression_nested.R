# Tests for the nested-comparison computation layer
# (R/regression_nested.R) -- the pair-wise change stats that
# table_regression() injects as IN-TABLE rows when `nested = TRUE`.

mt <- mtcars
mt$cyl <- factor(mt$cyl)


# ============================================================================
# compute_nested_comparisons() -- per-pair lm + glm + class-aware dispatch
# ============================================================================

test_that("compute_nested_comparisons - empty input returns empty frame", {
  out <- spicy:::compute_nested_comparisons(list())
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("compute_nested_comparisons - single fit returns empty frame", {
  out <- spicy:::compute_nested_comparisons(list(lm(mpg ~ wt, data = mt)))
  expect_equal(nrow(out), 0L)
})

test_that("compute_nested_comparisons - two lm models: one row of change stats", {
  fits <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt))
  out <- spicy:::compute_nested_comparisons(fits)
  expect_equal(nrow(out), 1L)
  expect_true(all(c("r2_change", "adj_r2_change", "f_change",
                    "f2_change", "lrt_change", "aic_change",
                    "aicc_change", "bic_change", "deviance_change",
                    "p_change") %in% names(out)))
})

test_that("compute_nested_comparisons - three lm models: two adjacent pair rows", {
  fits <- list(
    lm(mpg ~ wt, mt),
    lm(mpg ~ wt + cyl, mt),
    lm(mpg ~ wt + cyl + hp, mt)
  )
  out <- spicy:::compute_nested_comparisons(fits)
  expect_equal(nrow(out), 2L)
  expect_equal(out$comparison,
                c("Model 2 vs Model 1", "Model 3 vs Model 2"))
  expect_true(all(out$r2_change > 0))
})

test_that("compute_nested_comparisons - glm pair uses LRT path (variance-explained NA)", {
  fits <- list(
    glm(am ~ mpg, mt, family = binomial),
    glm(am ~ mpg + wt, mt, family = binomial)
  )
  out <- spicy:::compute_nested_comparisons(fits)
  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$r2_change))
  expect_true(is.na(out$f_change))
  expect_true(is.finite(out$lrt_change))
  expect_true(is.finite(out$p_change))
})


# ============================================================================
# compute_one_pair_lm() -- direct unit
# ============================================================================

test_that("compute_one_pair_lm - r2_change matches summary() difference", {
  m1 <- lm(mpg ~ wt, mt)
  m2 <- lm(mpg ~ wt + cyl, mt)
  out <- spicy:::compute_one_pair_lm(m1, m2)
  expect_equal(out$r2_change,
                summary(m2)$r.squared - summary(m1)$r.squared,
                tolerance = 1e-12)
})

test_that("compute_one_pair_lm - f_change + p_change match anova(m1, m2)", {
  m1 <- lm(mpg ~ wt, mt)
  m2 <- lm(mpg ~ wt + cyl, mt)
  out <- spicy:::compute_one_pair_lm(m1, m2)
  av <- stats::anova(m1, m2)
  expect_equal(out$f_change, unname(av$F[2]), tolerance = 1e-10)
  expect_equal(out$p_change, av[["Pr(>F)"]][2], tolerance = 1e-10)
})

test_that("compute_one_pair_lm - degenerate self-pair returns NA fields gracefully", {
  m1 <- lm(mpg ~ wt, mt)
  out <- spicy:::compute_one_pair_lm(m1, m1)
  expect_true(is.na(out$f_change))
  expect_true(is.na(out$p_change))
})


# ============================================================================
# compute_one_pair_glm() -- direct unit
# ============================================================================

test_that("compute_one_pair_glm - lrt_change matches anova(test='LRT')", {
  g1 <- glm(am ~ mpg, mt, family = binomial)
  g2 <- glm(am ~ mpg + wt, mt, family = binomial)
  out <- spicy:::compute_one_pair_glm(g1, g2)
  av <- stats::anova(g1, g2, test = "LRT")
  lrt_col <- intersect(c("Deviance", "scaled dev.", "LRT"), names(av))
  expect_equal(out$lrt_change, unname(av[[lrt_col[1L]]][2L]),
                tolerance = 1e-10)
})

test_that("compute_one_pair_glm - variance-explained tokens are NA", {
  g1 <- glm(am ~ mpg, mt, family = binomial)
  g2 <- glm(am ~ mpg + wt, mt, family = binomial)
  out <- spicy:::compute_one_pair_glm(g1, g2)
  expect_true(is.na(out$r2_change))
  expect_true(is.na(out$adj_r2_change))
  expect_true(is.na(out$f_change))
  expect_true(is.na(out$f2_change))
})


# ============================================================================
# attach_nested_stats_to_frames() -- Model 1 gets NA, M2+ gets pair stats
# Phase 0c sub-step C5: migrated from the deleted
# attach_nested_stats_to_extracts() to its frame-side sibling.
# ============================================================================

test_that("attach_nested_stats_to_frames - Model 1 cells NA, M2+ filled", {
  fits <- list(
    lm(mpg ~ wt, mt),
    lm(mpg ~ wt + cyl, mt),
    lm(mpg ~ wt + cyl + hp, mt)
  )
  frames <- lapply(seq_along(fits), function(i) {
    spicy:::as_regression_frame(fits[[i]], model_id = paste0("M", i))
  })
  out <- spicy:::attach_nested_stats_to_frames(frames, fits)
  for (f in out) {
    expect_true("r2_change" %in% names(f$info$fit_stats))
    expect_true("f_change"  %in% names(f$info$fit_stats))
    expect_true("p_change"  %in% names(f$info$fit_stats))
  }
  expect_true(is.na(out[[1L]]$info$fit_stats$r2_change))
  expect_true(is.na(out[[1L]]$info$fit_stats$f_change))
  expect_true(is.finite(out[[2L]]$info$fit_stats$r2_change))
  expect_true(is.finite(out[[3L]]$info$fit_stats$f_change))
})

test_that("attach_nested_stats_to_frames - single-fit no-op", {
  frames <- list(spicy:::as_regression_frame(lm(mpg ~ wt, mt),
                                              model_id = "M1"))
  out <- spicy:::attach_nested_stats_to_frames(
    frames, list(lm(mpg ~ wt, mt))
  )
  expect_identical(out, frames)
})


# ============================================================================
# default_nested_tokens() -- class-aware default for nested = TRUE
# ============================================================================

test_that("default_nested_tokens - all-lm returns r2_change / f_change / p_change", {
  models <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt))
  expect_equal(spicy:::default_nested_tokens(models),
                c("r2_change", "f_change", "p_change"))
})

test_that("default_nested_tokens - all-glm returns lrt_change / p_change", {
  models <- list(glm(am ~ mpg, mt, family = binomial),
                  glm(am ~ mpg + wt, mt, family = binomial))
  expect_equal(spicy:::default_nested_tokens(models),
                c("lrt_change", "p_change"))
})


# ============================================================================
# format_signed() -- explicit "+" prefix on positive change values
# ============================================================================

test_that("format_signed - explicit '+' on positive, '-' on negative", {
  expect_equal(spicy:::format_signed(0.123, 2L), "+0.12")
  expect_equal(spicy:::format_signed(-0.123, 2L), "-0.12")
  expect_equal(spicy:::format_signed(0, 2L), "0.00")
})


# ============================================================================
# End-to-end: in-table change rows replace the old footer block
# ============================================================================

test_that("table_regression - nested = TRUE injects ΔR² / F-change / p (change) rows", {
  fits <- list("S1" = lm(mpg ~ wt, mt),
                "S2" = lm(mpg ~ wt + cyl, mt))
  out <- table_regression(fits, nested = TRUE)
  vars <- trimws(as.data.frame(out, stringsAsFactors = FALSE)$Variable)
  expect_true("ΔR²" %in% vars)
  expect_true("F-change" %in% vars)
  expect_true("p (change)" %in% vars)
})

test_that("table_regression - nested = TRUE first model has em-dash in change cols", {
  fits <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt))
  out <- table_regression(fits, nested = TRUE)
  body <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  dr2 <- body[trimws(body$Variable) == "ΔR²", , drop = FALSE]
  m1_col <- names(body)[2L]
  m2_col <- names(body)[5L]
  expect_equal(trimws(dr2[[m1_col]]), "—")
  expect_match(trimws(dr2[[m2_col]]), "^[+-]")
})

test_that("table_regression - nested = TRUE no longer emits 'Model comparison' footer block", {
  fits <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt))
  out <- table_regression(fits, nested = TRUE)
  expect_no_match(attr(out, "note"), "Model comparison")
})

test_that("table_regression - nested glm injects Δχ² / p (change) rows", {
  fits <- list(glm(am ~ mpg, mt, family = binomial),
                glm(am ~ mpg + wt, mt, family = binomial))
  out <- table_regression(fits, nested = TRUE)
  vars <- trimws(as.data.frame(out, stringsAsFactors = FALSE)$Variable)
  expect_true("Δχ²" %in% vars)
  expect_true("p (change)" %in% vars)
  expect_false("ΔR²" %in% vars)
  expect_false("F-change" %in% vars)
})

test_that("table_regression - user can override change tokens via show_fit_stats", {
  fits <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt))
  out <- table_regression(fits, nested = TRUE,
                          show_fit_stats = c("nobs", "r2",
                                              "aic_change",
                                              "bic_change",
                                              "p_change"))
  vars <- trimws(as.data.frame(out, stringsAsFactors = FALSE)$Variable)
  expect_true("ΔAIC" %in% vars)
  expect_true("ΔBIC" %in% vars)
  expect_true("p (change)" %in% vars)
  expect_false("ΔR²" %in% vars)
  expect_false("F-change" %in% vars)
})

test_that("table_regression - row order in show_fit_stats controls display order", {
  fits <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt))
  out <- table_regression(fits, nested = TRUE,
                          show_fit_stats = c("p_change", "r2_change",
                                              "nobs"))
  vars <- trimws(as.data.frame(out, stringsAsFactors = FALSE)$Variable)
  fit_vars <- vars[(length(vars) - 2L):length(vars)]
  expect_equal(fit_vars, c("p (change)", "ΔR²", "n"))
})

test_that("table_regression - all-glm with lm-only change tokens rejected", {
  fits <- list(glm(am ~ mpg, mt, family = binomial),
                glm(am ~ mpg + wt, mt, family = binomial))
  expect_error(
    table_regression(fits, nested = TRUE,
                     show_fit_stats = c("nobs", "r2_change", "p_change")),
    class = "spicy_invalid_input"
  )
})
