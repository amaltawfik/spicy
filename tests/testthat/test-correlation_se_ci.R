# ---------------------------------------------------------------------------
# Phase 7c17 tests: Wald SE + 95% CI for the random-effect correlation rows
# (rho) of the random-effects panel via the multivariate Delta-method on
# the merDeriv vcov for lme4 (lmer / glmer). glmmTMB and nlme::lme already
# populated rho SE/CI via their native engines (confint(method="Wald"),
# nlme::intervals()); the regression guards verify that path still works.
#
# Matches Stata `mixed` / SAS `PROC MIXED` panels which report
# `corr(_cons, slope) Subject | SE | 95% CI`.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_lmer_slope_corr <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

.fit_glmer_slope_corr <- function() {
  skip_if_not_installed("lme4")
  set.seed(42)
  n <- 1000
  g <- factor(rep(1:30, length.out = n))
  x <- rnorm(n)
  b0 <- rnorm(30)
  b1 <- rnorm(30, 0, 0.5)
  y <- rbinom(n, 1, plogis(0.5 + b0[g] + (0.8 + b1[g]) * x))
  lme4::glmer(y ~ x + (x | g), family = binomial)
}

.fit_glmmTMB_slope_corr <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject),
                    data = lme4::sleepstudy)
}

.fit_lme_slope_corr <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(distance ~ age, data = nlme::Orthodont,
             random = ~ age | Subject)
}


# ---- 1. lmer: correlation row has finite SE + CI from Delta method ------

test_that("lmer (Days | Subject): rho row has finite Wald SE + CI", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_slope_corr()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  rho_rows <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(rho_rows), 1L)
  expect_true(is.finite(rho_rows$std_error))
  expect_true(is.finite(rho_rows$ci_lower))
  expect_true(is.finite(rho_rows$ci_upper))
  expect_identical(rho_rows$ci_method, "wald")
})

test_that("lmer rho CI is clamped to [-1, 1] (boundary)", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_slope_corr()
  fr <- as_regression_frame(fit, model_id = "M1")
  rho <- fr$info$random_effects$variance_components
  rho <- rho[rho$is_correlation %in% TRUE, ]
  expect_true(rho$ci_lower >= -1)
  expect_true(rho$ci_upper <=  1)
})


# ---- 2. lmer SE matches a hand-coded Delta-method computation -----------

test_that("lmer rho SE matches the manual Delta-method formula to 1e-10", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_slope_corr()
  fr <- as_regression_frame(fit, model_id = "M1")
  rho_row <- fr$info$random_effects$variance_components
  rho_row <- rho_row[rho_row$is_correlation %in% TRUE, ]

  # Reference computation: pull the 3x3 vcov sub-matrix from merDeriv
  # directly and apply the Delta-method formula in-line.
  g_vc <- as.matrix(lme4::VarCorr(fit)[["Subject"]])
  var_int   <- g_vc[1L, 1L]
  var_slope <- g_vc[2L, 2L]
  cov_ij    <- g_vc[1L, 2L]
  rho_est   <- cov_ij / sqrt(var_int * var_slope)

  v <- as.matrix(merDeriv::vcov.lmerMod(fit, full = TRUE, ranpar = "var"))
  n_fix <- length(lme4::fixef(fit))
  sub <- v[(n_fix + 1L):(n_fix + 3L), (n_fix + 1L):(n_fix + 3L)]
  grad <- c(
    -rho_est / (2 * var_int),
    1 / sqrt(var_int * var_slope),
    -rho_est / (2 * var_slope)
  )
  expected_se <- sqrt(as.numeric(t(grad) %*% sub %*% grad))

  expect_equal(rho_row$std_error, expected_se, tolerance = 1e-10)
})


# ---- 2b. THREE-term random block: column-major vech positions ----------
# Regression guard for the Phase 7c25 fix. merDeriv lays the random
# block out COLUMN-MAJOR (vech); the original row-major position
# formulas coincided with column-major only for n <= 2, so a 3-term
# random block (n = 3) silently pulled SEs from the wrong vcov cells
# (cov(term_3, term_1) mislabelled as var(term_2), etc.). Cross-
# validate every variance-row SE against merDeriv-by-name and every
# correlation-row SE against a numDeriv gradient on the 6x6 sub-vcov.

test_that("lmer 3-term random block: variance + correlation SE are correct (column-major)", {
  skip_if_not_installed("merDeriv")
  skip_if_not_installed("numDeriv")
  set.seed(123)
  n <- 600
  g <- factor(rep(1:30, length.out = n))
  x1 <- rnorm(n); x2 <- rnorm(n)
  b0 <- rnorm(30, 0, 2); b1 <- rnorm(30, 0, 1.5); b2 <- rnorm(30, 0, 1)
  y <- 1 + 0.5 * x1 - 0.3 * x2 + b0[g] + b1[g] * x1 + b2[g] * x2 + rnorm(n)
  fit <- lme4::lmer(
    y ~ x1 + x2 + (1 + x1 + x2 | g),
    control = lme4::lmerControl(check.conv.singular = "ignore")
  )
  skip_if(lme4::isSingular(fit), "fit was singular this round")

  g_vc <- as.matrix(lme4::VarCorr(fit)$g)
  v <- as.matrix(merDeriv::vcov.lmerMod(fit, full = TRUE, ranpar = "var"))
  n_fix <- length(lme4::fixef(fit))
  re_block <- v[(n_fix + 1L):nrow(v), (n_fix + 1L):nrow(v), drop = FALSE]

  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components

  # Variance rows: SE must equal sqrt(diag(merDeriv vcov)) at the
  # column-major diagonal positions (1, 4, 6 for n = 3), which are the
  # entries merDeriv names cov_g.(Intercept) / cov_g.x1 / cov_g.x2.
  diag_names <- c("cov_g.(Intercept)", "cov_g.x1", "cov_g.x2")
  truth_var_se <- sqrt(diag(re_block))[match(diag_names, colnames(re_block))]
  var_rows <- vc[!(vc$is_correlation %in% TRUE) & vc$group == "g", ]
  expect_equal(var_rows$std_error, unname(truth_var_se), tolerance = 1e-8)

  # Correlation rows: SE must equal a numDeriv gradient of
  # rho = cov / sqrt(var_i var_j) quadratic-formed with the 6x6
  # sub-vcov of the column-major vech.
  vech <- c(g_vc[1, 1], g_vc[2, 1], g_vc[3, 1],
            g_vc[2, 2], g_vc[3, 2], g_vc[3, 3])
  re_cov6 <- re_block[1:6, 1:6, drop = FALSE]
  rho_fun <- function(vv, i, j) {
    m <- matrix(0, 3, 3)
    m[1, 1] <- vv[1]; m[2, 1] <- m[1, 2] <- vv[2]; m[3, 1] <- m[1, 3] <- vv[3]
    m[2, 2] <- vv[4]; m[3, 2] <- m[2, 3] <- vv[5]; m[3, 3] <- vv[6]
    m[i, j] / sqrt(m[i, i] * m[j, j])
  }
  corr <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(corr), 3L)
  for (k in seq_len(nrow(corr))) {
    pair <- strsplit(corr$term[k], ", ", fixed = TRUE)[[1L]]
    i <- match(pair[1L], rownames(g_vc))
    j <- match(pair[2L], rownames(g_vc))
    gr <- numDeriv::grad(function(vv) rho_fun(vv, i, j), vech)
    truth <- sqrt(as.numeric(t(gr) %*% re_cov6 %*% gr))
    expect_equal(corr$std_error[k], truth, tolerance = 1e-6,
                 info = paste("corr SE mismatch:", corr$term[k]))
  }
})

test_that(".vech_colmajor_pos returns column-major lower-triangle positions", {
  # n = 3 block: diagonal at 1, 4, 6; off-diag cov(2,1)=2, cov(3,1)=3,
  # cov(3,2)=5 -- the layout merDeriv uses.
  expect_identical(spicy:::.vech_colmajor_pos(3L, 1L, 1L), 1L)
  expect_identical(spicy:::.vech_colmajor_pos(3L, 2L, 2L), 4L)
  expect_identical(spicy:::.vech_colmajor_pos(3L, 3L, 3L), 6L)
  expect_identical(spicy:::.vech_colmajor_pos(3L, 2L, 1L), 2L)
  expect_identical(spicy:::.vech_colmajor_pos(3L, 3L, 1L), 3L)
  expect_identical(spicy:::.vech_colmajor_pos(3L, 3L, 2L), 5L)
  # n = 2 coincides with row-major (why the bug hid): diag at 1, 3.
  expect_identical(spicy:::.vech_colmajor_pos(2L, 1L, 1L), 1L)
  expect_identical(spicy:::.vech_colmajor_pos(2L, 2L, 2L), 3L)
  expect_identical(spicy:::.vech_colmajor_pos(2L, 2L, 1L), 2L)
})


# ---- 3. glmer: rho row has finite SE + CI (no residual -> same path) ----

test_that("glmer (binomial logit) (x | g): rho row has finite Wald SE + CI", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_glmer_slope_corr()
  skip_if(lme4::isSingular(fit), "skip when glmer fit is singular")
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  rho_rows <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(rho_rows), 1L)
  expect_true(is.finite(rho_rows$std_error))
  expect_true(is.finite(rho_rows$ci_lower))
  expect_true(is.finite(rho_rows$ci_upper))
  expect_identical(rho_rows$ci_method, "wald")
})


# ---- 4. Engine parity: glmmTMB and lme still populate rho SE/CI --------

test_that("glmmTMB (Days | Subject): rho row has finite SE + CI", {
  fit <- .fit_glmmTMB_slope_corr()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  rho_rows <- vc[vc$is_correlation %in% TRUE, ]
  expect_true(nrow(rho_rows) >= 1L)
  expect_true(all(is.finite(rho_rows$std_error)))
})

test_that("nlme::lme (age | Subject): rho row has finite SE + CI", {
  fit <- .fit_lme_slope_corr()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  rho_rows <- vc[vc$is_correlation %in% TRUE, ]
  expect_true(nrow(rho_rows) >= 1L)
  expect_true(all(is.finite(rho_rows$std_error)))
})


# ---- 5. End-to-end rendering: rho row prints with bracketed CI ----------

test_that("table_regression(lmer slope) prints rho row with SE + CI brackets", {
  skip_if_not_installed("merDeriv")
  fit <- .fit_lmer_slope_corr()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  # rho row must include parenthesised SE and bracketed CI (no em-dash).
  rho_line <- grep("ρ Subject", strsplit(combined, "\n")[[1L]], value = TRUE)
  expect_length(rho_line, 1L)
  expect_match(rho_line, "\\([0-9]+\\.[0-9]+\\)")  # (SE)
  expect_match(rho_line, "\\[")                    # [CI ...
})


# ---- 6. Single-term random structure: no rho row (regression guard) ----

test_that("lmer (1 | Subject) has NO rho row in variance_components", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  expect_false(any(vc$is_correlation %in% TRUE))
})


# ---- 7. Singular fit: rho SE/CI stays NA (regression guard) -------------

test_that("lme4::isSingular fits leave rho row at NA (no Delta on singular)", {
  skip_if_not_installed("lme4")
  set.seed(2)
  n <- 60
  g <- factor(rep(1:5, length.out = n))
  x <- rnorm(n)
  y <- 1 + 0.5 * x + rnorm(n)
  fit <- suppressMessages(suppressWarnings(
    lme4::lmer(y ~ x + (x | g))
  ))
  skip_if(!lme4::isSingular(fit), "fit happened to be non-singular")
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  rho_rows <- vc[vc$is_correlation %in% TRUE, ]
  if (nrow(rho_rows) > 0L) {
    expect_true(all(is.na(rho_rows$std_error)))
  }
})
