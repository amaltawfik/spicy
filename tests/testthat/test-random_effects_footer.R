# ---------------------------------------------------------------------------
# Phase 7c1 tests: random-effects footer block for mixed-effects fits.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_lmer_re <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_glmmTMB_re <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_lme_re <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(distance ~ age + Sex, data = nlme::Orthodont,
            random = ~ 1 | Subject)
}

.fit_lm_no_re <- function() {
  lm(mpg ~ wt + cyl, data = mtcars)
}


# ---- 1. Single-model footer ---------------------------------------------

# D4 amendment: the footer keeps only the estimation method + the model-level
# chi-bar-squared LR test. The variance components render as table rows; N
# (groups) + ICC render as fit-stat rows.

test_that("random effects footer fires for lmer fits (method + LR test)", {
  fit <- .fit_lmer_re()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_true(is.character(out))
  expect_match(out, "Random effects (REML)", fixed = TRUE)
  expect_match(out, "LR test",               fixed = TRUE)
  # N / ICC moved to fit-stat rows -- no longer in the footer.
  expect_false(grepl("18 Subjects", out, fixed = TRUE))
  expect_false(grepl("ICC", out, fixed = TRUE))
})

test_that("random effects footer fires for glmmTMB Gaussian-identity fits", {
  fit <- .fit_glmmTMB_re()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_true(is.character(out))
  expect_match(out, "Random effects", fixed = TRUE)
  expect_match(out, "LR test",        fixed = TRUE)
})

test_that("random effects footer fires for nlme::lme fits", {
  fit <- .fit_lme_re()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_match(out, "Random effects (REML)", fixed = TRUE)
  expect_match(out, "LR test",               fixed = TRUE)
})


# ---- Phase 7c6: REML / ML estimator label -------------------------------
# Phase 7c7c: estimator label now in the panel header, not the N-groups
# sentence.

test_that("random effects footer annotates lmer (REML default) with '(REML)'", {
  fit <- .fit_lmer_re()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_match(out, "Random effects (REML):", fixed = TRUE)
})

test_that("random effects footer annotates lmer (REML=FALSE) with '(ML)'", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject),
                    data = lme4::sleepstudy, REML = FALSE)
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_match(out, "Random effects (ML):", fixed = TRUE)
})

test_that("random effects footer annotates glmer with '(ML)' (REML undefined for GLMM)", {
  skip_if_not_installed("lme4")
  d <- mtcars; d$cyl <- factor(d$cyl)
  suppressMessages(suppressWarnings(
    fit <- lme4::glmer(am ~ mpg + (1 | cyl), data = d, family = binomial)
  ))
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_match(out, "(ML)", fixed = TRUE)
  expect_false(grepl("(REML)", out, fixed = TRUE))
})

test_that("random effects footer annotates lme (REML default) with '(REML)'", {
  fit <- .fit_lme_re()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_match(out, "Random effects (REML):", fixed = TRUE)
})

test_that("random effects footer annotates lme (method='ML') with '(ML)'", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(distance ~ age + Sex, data = nlme::Orthodont,
                   random = ~ 1 | Subject, method = "ML")
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_match(out, "Random effects (ML):", fixed = TRUE)
})

test_that("random effects footer annotates glmmTMB (default ML) with '(ML)'", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject),
                           data = lme4::sleepstudy)
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_match(out, "Random effects (ML):", fixed = TRUE)
})

test_that("random effects footer annotates glmmTMB (REML=TRUE) with '(REML)'", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject),
                           data = lme4::sleepstudy, REML = TRUE)
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_match(out, "Random effects (REML):", fixed = TRUE)
})


# ---- 2. NOT fired for non-mixed classes ---------------------------------

test_that("random effects footer is NULL for non-mixed-effects fits (lm)", {
  fit <- .fit_lm_no_re()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_null(out)
})

test_that("random effects footer is NULL for an empty frames list", {
  expect_null(spicy:::build_random_effects_footer_block_from_frames(list()))
})


# ---- 3. End-to-end integration via table_regression() -------------------

test_that("table_regression() footer carries the Random effects panel for lmer", {
  fit <- .fit_lmer_re()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  # Phase 7c7c: structured panel format
  expect_match(combined, "Random effects",     fixed = TRUE)
  expect_match(combined, "σ Subject (Intercept)", fixed = TRUE)
  expect_match(combined, "N (Subject)",        fixed = TRUE)
})

test_that("table_regression() footer does NOT carry Random effects for lm", {
  fit <- .fit_lm_no_re()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_false(grepl("Random effects", combined, fixed = TRUE))
})


# ---- 4. Multi-model: per-model prefix -----------------------------------

test_that("random effects footer prefixes per model for multi-model lists", {
  fit_lmer <- .fit_lmer_re()
  fr_lmer <- as_regression_frame(fit_lmer, model_id = "M1")
  fit_lme  <- .fit_lme_re()
  fr_lme  <- as_regression_frame(fit_lme, model_id = "M2")
  out <- spicy:::build_random_effects_footer_block_from_frames(
    list(fr_lmer, fr_lme))
  expect_match(out, "Model 1:", fixed = TRUE)
  expect_match(out, "Model 2:", fixed = TRUE)
})


