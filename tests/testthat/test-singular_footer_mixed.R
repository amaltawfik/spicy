# ---------------------------------------------------------------------------
# Phase 7c21 tests: class-aware singular-fit footer message.
#
# Two distinct singular regimes:
#   * lm / glm: rank-deficient fixed-effect design (coef -> NA)
#   * lmer / glmer / glmmTMB / lme: variance component on the boundary 0
#     (lme4::isSingular = TRUE). The Phase 7c21 footer says "Singular
#     fit: random-effect variance component(s) at the boundary 0 ..."
#     instead of the lm path's "Rank-deficient model" wording.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_singular_glmer <- function() {
  skip_if_not_installed("lme4")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  suppressMessages(suppressWarnings(
    lme4::glmer(am ~ mpg + (1 | cyl), data = d, family = binomial)
  ))
}

.fit_rank_def_lm <- function() {
  d <- mtcars
  d$mpg2 <- d$mpg
  lm(disp ~ mpg + mpg2 + wt, data = d)
}

.fit_clean_lmer <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}


# ---- 1. Singular glmer triggers the mixed-specific message ---------------

test_that("singular glmer footer message names 'random-effect variance component(s) at the boundary 0'", {
  fit <- .fit_singular_glmer()
  skip_if(!lme4::isSingular(fit), "glmer fit was not singular this round")
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Singular fit:", fixed = TRUE)
  expect_match(combined, "boundary 0", fixed = TRUE)
  expect_match(combined, "unreliable at the boundary", fixed = TRUE)
  # The lm rank-deficient phrasing must NOT appear for the mixed case.
  expect_false(grepl("Rank-deficient model", combined, fixed = TRUE))
})


# ---- 2. Rank-deficient lm keeps the original lm-specific message --------

test_that("rank-deficient lm footer keeps 'Rank-deficient model' wording", {
  fit <- .fit_rank_def_lm()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Rank-deficient model", fixed = TRUE)
  # The mixed-specific phrasing must NOT appear for the lm case.
  expect_false(grepl("boundary 0", combined, fixed = TRUE))
})


# ---- 3. Clean (non-singular) lmer fit has no singular footer line --------

test_that("non-singular lmer has no singular footer line", {
  fit <- .fit_clean_lmer()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_false(grepl("Singular fit:",   combined, fixed = TRUE))
  expect_false(grepl("Rank-deficient",  combined, fixed = TRUE))
})


# ---- 4. Helper unit tests ----------------------------------------------

test_that(".is_mixed_frame returns TRUE for the 4 mixed-effects classes", {
  for (cls in c("lmerMod", "lmerModLmerTest", "glmerMod", "glmmTMB", "lme")) {
    frame <- list(info = list(class = cls))
    expect_true(spicy:::.is_mixed_frame(frame),
                info = paste("class:", cls))
  }
})

test_that(".is_mixed_frame returns FALSE for lm / glm / gls", {
  for (cls in c("lm", "glm", "gls", "coxph", "")) {
    frame <- list(info = list(class = cls))
    expect_false(spicy:::.is_mixed_frame(frame),
                 info = paste("class:", cls))
  }
})

test_that(".singular_msg_for_frame branches on is_mixed", {
  frame_lm <- list(info = list(class = "lm"))
  frame_lmer <- list(info = list(class = "lmerMod"))
  msg_lm <- spicy:::.singular_msg_for_frame(frame_lm,   FALSE)
  msg_re <- spicy:::.singular_msg_for_frame(frame_lmer, TRUE)
  expect_match(msg_lm, "Rank-deficient",   fixed = TRUE)
  expect_match(msg_re, "boundary 0",       fixed = TRUE)
})


# ---- 5. Multi-model: per-Model prefix for mixed-singular pair ---------

test_that("multi-model singular fits use per-Model prefix", {
  fit_singular <- .fit_singular_glmer()
  skip_if(!lme4::isSingular(fit_singular), "no singular fit this round")
  fit_clean <- .fit_clean_lmer()
  out <- capture.output(print(
    table_regression(list(fit_clean, fit_singular))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Model 2:", fixed = TRUE)
  expect_match(combined, "boundary 0", fixed = TRUE)
})
