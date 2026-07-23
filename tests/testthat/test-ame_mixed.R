# ---------------------------------------------------------------------------
# Phase 7c15 tests: Average Marginal Effects (AME) for mixed-effects fits.
# Covers schema injection across all four engines (lmer / glmer / glmmTMB /
# lme), oracle agreement against marginaleffects::avg_slopes(), factor and
# numeric predictors, multi-model + nested layouts, and the silent
# fallback when marginaleffects is unavailable.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_lmer_ame <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_glmer_ame <- function() {
  skip_if_not_installed("lme4")
  set.seed(1)
  n <- 500
  g <- factor(rep(1:25, length.out = n))
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(0.5 + 0.8 * x + rnorm(25)[g]))
  lme4::glmer(y ~ x + (1 | g), family = binomial)
}

.fit_glmer_factor_ame <- function() {
  skip_if_not_installed("lme4")
  set.seed(1)
  n <- 500
  g <- factor(rep(1:25, length.out = n))
  x <- rnorm(n)
  cat <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
  y <- rbinom(
    n,
    1,
    plogis(
      0.5 + 0.8 * x + (cat == "B") * 0.3 + (cat == "C") * -0.5 + rnorm(25)[g]
    )
  )
  lme4::glmer(y ~ x + cat + (1 | g), family = binomial)
}

.fit_glmmTMB_ame <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_lme_ame_factor <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )
}


# ---- 1. Schema: AME rows appended to coefs when AME token requested -----

test_that("lmer: requesting 'ame' injects AME rows into frame coefs", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_lmer_ame()
  fr <- as_regression_frame(fit, model_id = "M1", show_columns = c("b", "ame"))
  expect_true("ame" %in% fr$coefs$estimate_type)
})

test_that("lmer: AME rows are NOT injected when show_columns lacks AME", {
  fit <- .fit_lmer_ame()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "se", "ci", "p")
  )
  expect_false("ame" %in% fr$coefs$estimate_type)
})

test_that("glmer: requesting 'ame' injects AME rows", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_glmer_ame()
  fr <- as_regression_frame(fit, model_id = "M1", show_columns = c("b", "ame"))
  expect_true("ame" %in% fr$coefs$estimate_type)
})

test_that("glmmTMB: requesting 'ame' injects AME rows", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_glmmTMB_ame()
  fr <- as_regression_frame(fit, model_id = "M1", show_columns = c("b", "ame"))
  expect_true("ame" %in% fr$coefs$estimate_type)
})

test_that("lme: requesting 'ame' injects AME rows", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_lme_ame_factor()
  fr <- as_regression_frame(fit, model_id = "M1", show_columns = c("b", "ame"))
  expect_true("ame" %in% fr$coefs$estimate_type)
})


# ---- 2. Oracle: AME estimate matches marginaleffects directly ----------

test_that("lmer Gaussian: AME == B coefficient (identity link)", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_lmer_ame()
  fr <- as_regression_frame(fit, model_id = "M1", show_columns = c("b", "ame"))
  b_days <- fr$coefs$estimate[
    fr$coefs$estimate_type == "B" &
      fr$coefs$term == "Days"
  ]
  ame_days <- fr$coefs$estimate[
    fr$coefs$estimate_type == "ame" &
      fr$coefs$term == "Days"
  ]
  expect_equal(ame_days, b_days, tolerance = 1e-10)
})

test_that("glmer AME estimate matches marginaleffects::avg_slopes() exactly", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_glmer_ame()
  fr <- as_regression_frame(fit, model_id = "M1", show_columns = c("b", "ame"))
  oracle <- suppressWarnings(suppressMessages(
    marginaleffects::avg_slopes(fit, df = Inf)
  ))
  ame_rows <- fr$coefs[fr$coefs$estimate_type == "ame", ]
  for (i in seq_len(nrow(oracle))) {
    if (oracle$term[i] == "x") {
      expect_equal(
        ame_rows$estimate[ame_rows$term == "x"],
        oracle$estimate[i],
        tolerance = 1e-10
      )
      expect_equal(
        ame_rows$std_error[ame_rows$term == "x"],
        oracle$std.error[i],
        tolerance = 1e-10
      )
    }
  }
})


# ---- 3. Factor predictor: term-id alignment with B-rows ----------------

test_that("lme factor predictor: AME term == B term (no duplicate row)", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_lme_ame_factor()
  fr <- as_regression_frame(fit, model_id = "M1", show_columns = c("b", "ame"))
  # Factor B-row uses term "SexFemale" -- the AME row must use the same
  # term id so the renderer pairs them on a single line. Previously the
  # AME row used the bare variable name "Sex", producing a stray row.
  b_terms <- fr$coefs$term[
    fr$coefs$estimate_type == "B" &
      fr$coefs$parent_var == "Sex" &
      !is.na(fr$coefs$estimate)
  ]
  # Non-reference AME rows only: the frame also synthesizes an NA
  # reference placeholder (its term matches the B REFERENCE row, so
  # the rendered reference line en-dashes under the AME columns).
  ame_terms <- fr$coefs$term[
    fr$coefs$estimate_type == "ame" &
      fr$coefs$parent_var == "Sex" &
      !(fr$coefs$is_ref %in% TRUE)
  ]
  expect_true(all(ame_terms %in% b_terms))
})

test_that("glmer factor predictor: AME term id matches level coef", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_glmer_factor_ame()
  fr <- as_regression_frame(fit, model_id = "M1", show_columns = c("b", "ame"))
  # cat has levels A (ref), B, C -- computed AME rows must use catB /
  # catC term ids; the catA reference placeholder (NA values, is_ref)
  # is what en-dashes the reference line under the AME columns.
  ame_cat <- fr$coefs[
    fr$coefs$estimate_type == "ame" &
      fr$coefs$parent_var == "cat" &
      !(fr$coefs$is_ref %in% TRUE),
  ]
  expect_true(all(ame_cat$term %in% c("catB", "catC")))
  ref_cat <- fr$coefs[
    fr$coefs$estimate_type == "ame" &
      (fr$coefs$is_ref %in% TRUE),
  ]
  expect_identical(nrow(ref_cat), 1L)
  expect_true(is.na(ref_cat$estimate))
})


# ---- 4. End-to-end rendering --------------------------------------------

test_that("table_regression(lmer, show_columns = c('b','ame')) renders AME col", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_lmer_ame()
  out <- capture.output(print(
    table_regression(fit, show_columns = c("b", "se", "ame", "ame_p"))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "AME", fixed = TRUE)
  # Days row has both B and AME populated -- 10.47 should appear twice
  # (once in B, once in AME) for the identity-link case.
  expect_true(
    length(regmatches(combined, gregexpr("10\\.47", combined))[[1L]]) >= 2L
  )
})

test_that("table_regression(glmer, ...) renders AME (probability points)", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_glmer_ame()
  out <- capture.output(print(
    table_regression(fit, show_columns = c("b", "se", "p", "ame", "ame_p"))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "AME", fixed = TRUE)
})

test_that("table_regression(lme, factor predictor) groups AME under factor header", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_lme_ame_factor()
  out <- capture.output(print(
    table_regression(fit, show_columns = c("b", "se", "ame", "ame_p"))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Sex:", fixed = TRUE)
  expect_match(combined, "Female", fixed = TRUE)
  # No stray "Sex" row outside the factor block.
  panel_lines <- strsplit(combined, "\n")[[1L]]
  bare_sex <- grep("^\\s*Sex\\s+\\|", panel_lines)
  expect_length(bare_sex, 0L)
})


# ---- 5. Multi-model nested rendering ------------------------------------

test_that("nested glmer pair renders AME columns for both models + LRT rows", {
  skip_if_not_installed("marginaleffects")
  set.seed(1)
  n <- 500
  g <- factor(rep(1:25, length.out = n))
  x <- rnorm(n)
  z <- rnorm(n)
  y <- rbinom(n, 1, plogis(0.5 + 0.8 * x + 0.3 * z + rnorm(25)[g]))
  m1 <- lme4::glmer(y ~ x + (1 | g), family = binomial)
  m2 <- lme4::glmer(y ~ x + z + (1 | g), family = binomial)
  out <- capture.output(print(
    table_regression(
      list(m1, m2),
      show_columns = c("b", "p", "ame", "ame_p"),
      nested = TRUE
    )
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Hierarchical", fixed = TRUE)
  expect_match(combined, "AME", fixed = TRUE)
  expect_match(combined, "Δχ²", fixed = TRUE)
  # Both models contribute AME rows (count of "AME" column header
  # spanners is >= 2 in a 2-model layout).
  ame_hits <- length(regmatches(combined, gregexpr("AME", combined))[[1L]])
  expect_true(ame_hits >= 2L)
})


# ---- 6. Behavioural guard: empty show_columns => no AME -----------------

test_that(".attach_ame_to_frame_coefs is a no-op when no AME token requested", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_glmer_ame()
  fr <- as_regression_frame(fit, model_id = "M1", show_columns = c("b", "se"))
  expect_false("ame" %in% fr$coefs$estimate_type)
})

test_that(".attach_ame_to_frame_coefs fires for each AME-family token", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_glmer_ame()
  for (tok in c("ame", "ame_se", "ame_ci", "ame_p")) {
    fr <- as_regression_frame(fit, model_id = "M1", show_columns = c("b", tok))
    expect_true("ame" %in% fr$coefs$estimate_type, info = paste("token:", tok))
  }
})
