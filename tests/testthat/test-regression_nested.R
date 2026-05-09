# Tests for table_regression() nested model comparison footer (Step 9 / Q6).

# ---- Test fixtures -------------------------------------------------------

mt <- mtcars

mk_nested_pair_lm <- function() {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  list(m1, m2)
}

mk_nested_triplet_lm <- function() {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  m3 <- lm(mpg ~ wt + cyl + am, data = mt)
  list(m1, m2, m3)
}


# ============================================================================
# compute_nested_comparisons_lm()
# ============================================================================

test_that("nested — empty when fewer than 2 fits", {
  out <- spicy:::compute_nested_comparisons_lm(list())
  expect_equal(nrow(out), 0L)
  expect_true("comparison" %in% names(out))

  out1 <- spicy:::compute_nested_comparisons_lm(list(lm(mpg ~ wt, data = mt)))
  expect_equal(nrow(out1), 0L)
})

test_that("nested — pair: one comparison row, default lm tokens", {
  fits <- mk_nested_pair_lm()
  out <- spicy:::compute_nested_comparisons_lm(fits)
  expect_equal(nrow(out), 1L)
  # Default for lm is c("r2_change", "F", "p")
  expect_equal(names(out), c("comparison", "r2_change", "F", "p"))
  expect_equal(out$comparison, "Model 2 vs Model 1")
})

test_that("nested — triplet: 2 pairwise rows", {
  fits <- mk_nested_triplet_lm()
  out <- spicy:::compute_nested_comparisons_lm(fits)
  expect_equal(nrow(out), 2L)
  expect_equal(out$comparison, c("Model 2 vs Model 1", "Model 3 vs Model 2"))
})

test_that("nested — r2_change matches direct anova / summary computation", {
  fits <- mk_nested_pair_lm()
  out <- spicy:::compute_nested_comparisons_lm(fits, nested_stats = "r2_change")
  expected <- summary(fits[[2]])$r.squared - summary(fits[[1]])$r.squared
  expect_equal(out$r2_change[1], expected, tolerance = 1e-12)
})

test_that("nested — F and p match anova(m1, m2)", {
  fits <- mk_nested_pair_lm()
  av <- anova(fits[[1]], fits[[2]])
  out <- spicy:::compute_nested_comparisons_lm(fits, nested_stats = c("F", "p"))
  expect_equal(out$F[1], av$F[2], tolerance = 1e-12)
  expect_equal(out$p[1], av[["Pr(>F)"]][2], tolerance = 1e-12)
})

test_that("nested — AIC/BIC/AICc/deviance_change deltas correct", {
  fits <- mk_nested_pair_lm()
  out <- spicy:::compute_nested_comparisons_lm(
    fits,
    nested_stats = c("AIC", "BIC", "AICc", "deviance_change")
  )
  expect_equal(out$AIC[1], AIC(fits[[2]]) - AIC(fits[[1]]), tolerance = 1e-12)
  expect_equal(out$BIC[1], BIC(fits[[2]]) - BIC(fits[[1]]), tolerance = 1e-12)
  expect_equal(out$deviance_change[1],
               deviance(fits[[1]]) - deviance(fits[[2]]),
               tolerance = 1e-12)
  # AICc — Hurvich & Tsai
  aicc <- function(fit) {
    k <- length(coef(fit)) + 1L
    n <- nobs(fit)
    AIC(fit) + (2 * k * (k + 1L)) / (n - k - 1L)
  }
  expect_equal(out$AICc[1], aicc(fits[[2]]) - aicc(fits[[1]]),
               tolerance = 1e-12)
})

test_that("nested — LRT matches -2 (logLik_prev - logLik_curr)", {
  fits <- mk_nested_pair_lm()
  out <- spicy:::compute_nested_comparisons_lm(fits, nested_stats = "LRT")
  expected <- -2 * (as.numeric(logLik(fits[[1]])) -
                    as.numeric(logLik(fits[[2]])))
  expect_equal(out$LRT[1], expected, tolerance = 1e-12)
})

test_that("nested — token order in output follows nested_stats order", {
  fits <- mk_nested_pair_lm()
  out <- spicy:::compute_nested_comparisons_lm(
    fits,
    nested_stats = c("p", "F", "AIC")
  )
  # comparison column always first; rest follows nested_stats
  expect_equal(names(out), c("comparison", "p", "F", "AIC"))
})

test_that("nested — f2_change uses R²_full of m_curr in denominator", {
  fits <- mk_nested_pair_lm()
  out <- spicy:::compute_nested_comparisons_lm(fits, nested_stats = "f2_change")
  r2_p <- summary(fits[[1]])$r.squared
  r2_c <- summary(fits[[2]])$r.squared
  expected <- (r2_c - r2_p) / (1 - r2_c)
  expect_equal(out$f2_change[1], expected, tolerance = 1e-12)
})


# ============================================================================
# format_nested_comparison_footer()
# ============================================================================

test_that("footer — NULL when comparisons is empty", {
  expect_null(spicy:::format_nested_comparison_footer(
    spicy:::empty_nested_comparisons()
  ))
  expect_null(spicy:::format_nested_comparison_footer(NULL))
})

test_that("footer — single pair: header + 1 line", {
  fits <- mk_nested_pair_lm()
  comp <- spicy:::compute_nested_comparisons_lm(fits)
  out <- spicy:::format_nested_comparison_footer(comp)
  lines <- strsplit(out, "\n", fixed = TRUE)[[1]]
  expect_equal(length(lines), 2L)
  expect_equal(lines[1], "── Model comparison ──")
  expect_match(lines[2], "^Model 2 vs Model 1: ")
})

test_that("footer — token labels render with the right symbols", {
  fits <- mk_nested_pair_lm()
  comp <- spicy:::compute_nested_comparisons_lm(
    fits,
    nested_stats = c("r2_change", "adj_r2_change", "F", "f2_change",
                     "LRT", "AIC", "AICc", "BIC",
                     "deviance_change", "p")
  )
  out <- spicy:::format_nested_comparison_footer(comp)
  expect_match(out, "ΔR²")
  expect_match(out, "ΔAdj\\.R²")
  expect_match(out, "F = ")
  expect_match(out, "Δf²")
  expect_match(out, "χ²")
  expect_match(out, "ΔAIC")
  expect_match(out, "ΔAICc")
  expect_match(out, "ΔBIC")
  expect_match(out, "Δdev")
  expect_match(out, "p = ")
})

test_that("footer — positive deltas get a leading + sign", {
  fits <- mk_nested_pair_lm()
  comp <- spicy:::compute_nested_comparisons_lm(fits, nested_stats = "r2_change")
  out <- spicy:::format_nested_comparison_footer(comp, fit_digits = 2L)
  # r2_change is positive (m2 better than m1)
  expect_match(out, "ΔR² = \\+0\\.")
})

test_that("footer — negative deltas display the minus", {
  fits <- mk_nested_pair_lm()
  comp <- spicy:::compute_nested_comparisons_lm(fits, nested_stats = "AIC")
  # AIC strictly decreases when adding informative predictors
  out <- spicy:::format_nested_comparison_footer(comp, ic_digits = 1L)
  expect_match(out, "ΔAIC = -")
})

test_that("footer — APA p-value: very small p prints '<.001'", {
  # Synthetic fixture with overwhelming signal so p ≪ .001
  set.seed(42)
  n <- 200L
  df <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  df$y <- 1 + 0.3 * df$x1 + 1.5 * df$x2 + rnorm(n, sd = 0.5)
  m1 <- lm(y ~ x1, data = df)
  m2 <- lm(y ~ x1 + x2, data = df)
  comp <- spicy:::compute_nested_comparisons_lm(list(m1, m2),
                                                 nested_stats = "p")
  expect_lt(comp$p[1], 0.001)   # sanity-check the fixture
  out <- spicy:::format_nested_comparison_footer(comp, p_digits = 3L)
  expect_match(out, "p = <\\.001")
})

test_that("footer — multi-pair: one line per pair", {
  fits <- mk_nested_triplet_lm()
  comp <- spicy:::compute_nested_comparisons_lm(fits)
  out <- spicy:::format_nested_comparison_footer(comp)
  lines <- strsplit(out, "\n", fixed = TRUE)[[1]]
  expect_equal(length(lines), 3L)  # header + 2 pairs
  expect_match(lines[2], "Model 2 vs Model 1")
  expect_match(lines[3], "Model 3 vs Model 2")
})

test_that("footer — NA values render as em-dash", {
  # Force NA via a degenerate pair (anova fails when fits aren't nested).
  # Build a frame to test format_nested_token directly.
  expect_match(
    spicy:::format_nested_token("F", NA_real_, 2L, 3L, 2L, 1L),
    "F = —"
  )
  expect_match(
    spicy:::format_nested_token("p", NA_real_, 2L, 3L, 2L, 1L),
    "p = —"
  )
})


# ============================================================================
# Helper formatters
# ============================================================================

test_that("format_p_apa — APA convention", {
  expect_equal(spicy:::format_p_apa(0.0001, digits = 3L), "<.001")
  expect_equal(spicy:::format_p_apa(0.001, digits = 3L), ".001")
  expect_equal(spicy:::format_p_apa(0.05, digits = 3L), ".050")
  expect_equal(spicy:::format_p_apa(0.5, digits = 3L), ".500")
})

test_that("format_signed — '+' for positives, '-' for negatives, neither for 0", {
  expect_equal(spicy:::format_signed(1.234, 2L), "+1.23")
  expect_equal(spicy:::format_signed(-1.234, 2L), "-1.23")
  expect_equal(spicy:::format_signed(0, 2L), "0.00")
})
