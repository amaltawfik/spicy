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
  out <- capture.output(print(suppressWarnings(table_regression(fit))))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Singular fit:", fixed = TRUE)
  expect_match(combined, "boundary (0)", fixed = TRUE)
  expect_match(
    combined,
    "estimated at the boundary (0); their Wald SE and CI are omitted",
    fixed = TRUE
  )
  # The lm rank-deficient phrasing must NOT appear for the mixed case.
  expect_false(grepl("Rank-deficient model", combined, fixed = TRUE))
})


# ---- 2. Rank-deficient lm keeps the original lm-specific message --------

test_that("rank-deficient lm footer keeps 'Rank-deficient model' wording", {
  fit <- .fit_rank_def_lm()
  out <- capture.output(print(suppressWarnings(table_regression(fit))))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Rank-deficient model", fixed = TRUE)
  # The mixed-specific phrasing must NOT appear for the lm case.
  expect_false(grepl("boundary (0)", combined, fixed = TRUE))
})


# ---- 3. Clean (non-singular) lmer fit has no singular footer line --------

test_that("non-singular lmer has no singular footer line", {
  fit <- .fit_clean_lmer()
  out <- capture.output(print(suppressWarnings(table_regression(fit))))
  combined <- paste(out, collapse = "\n")
  expect_false(grepl("Singular fit:", combined, fixed = TRUE))
  expect_false(grepl("Rank-deficient", combined, fixed = TRUE))
})


# ---- 4. Helper unit tests ----------------------------------------------

test_that(".is_mixed_frame returns TRUE for the 4 mixed-effects classes", {
  for (cls in c("lmerMod", "lmerModLmerTest", "glmerMod", "glmmTMB", "lme")) {
    frame <- list(info = list(class = cls))
    expect_true(spicy:::.is_mixed_frame(frame), info = paste("class:", cls))
  }
})

test_that(".is_mixed_frame returns FALSE for lm / glm / gls", {
  for (cls in c("lm", "glm", "gls", "coxph", "")) {
    frame <- list(info = list(class = cls))
    expect_false(spicy:::.is_mixed_frame(frame), info = paste("class:", cls))
  }
})

test_that(".singular_msg_for_frame branches on is_mixed", {
  frame_lm <- list(info = list(class = "lm"))
  frame_lmer <- list(info = list(class = "lmerMod"))
  msg_lm <- spicy:::.singular_msg_for_frame(frame_lm, FALSE)
  msg_re <- spicy:::.singular_msg_for_frame(frame_lmer, TRUE)
  expect_match(msg_lm, "Rank-deficient", fixed = TRUE)
  expect_match(msg_re, "boundary (0)", fixed = TRUE)
})


# ---- 5. Multi-model: per-Model prefix for mixed-singular pair ---------

test_that("multi-model singular fits use per-Model prefix", {
  fit_singular <- .fit_singular_glmer()
  skip_if(!lme4::isSingular(fit_singular), "no singular fit this round")
  fit_clean <- .fit_clean_lmer()
  out <- capture.output(print(
    suppressWarnings(table_regression(list(fit_clean, fit_singular)))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Model 2:", fixed = TRUE)
  expect_match(combined, "boundary (0)", fixed = TRUE)
})

test_that("singular fit advice arrives as a build-time spicy_caveat warning", {
  skip_if_not_installed("lme4")
  set.seed(2026)
  d <- data.frame(x = rnorm(120), g = factor(rep(1:12, each = 10)))
  d$y <- 2 + 0.5 * d$x + rnorm(120)
  fit <- suppressMessages(suppressWarnings(
    lme4::lmer(y ~ x + (1 | g), data = d)
  ))
  caveat_seen <- FALSE
  out <- withCallingHandlers(
    table_regression(fit),
    spicy_caveat = function(c) {
      caveat_seen <<- TRUE
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning") # lme4 refit noise
  )
  expect_true(caveat_seen)
  # The note keeps only the fact; the advice lives in the warning.
  note <- paste(attr(out, "note"), collapse = "\n")
  expect_false(grepl("consider simplifying", note, ignore.case = TRUE))
})


# ---- 5. The singular warning must not eat `labels =` (register 55) --------

test_that("the singular-fit warning does not swallow `labels =`", {
  # The warning block built its per-model names into a local called
  # `labels`, shadowing the user's per-coefficient label vector for the
  # rest of the pipeline.
  fit <- .fit_singular_glmer()
  skip_if(!lme4::isSingular(fit), "glmer fit was not singular this round")
  out <- suppressWarnings(
    table_regression(fit, labels = c(mpg = "Fuel economy"))
  )
  expect_true("Fuel economy" %in% out$Variable)
  expect_false("mpg" %in% out$Variable)
})


# ---- 6. glmmTMB and lme reach the same footer and warning ---------------

# Neither engine exposes an isSingular(); their frames read the boundary
# off the variance components themselves (see .glmmTMB_is_singular /
# .lme_is_singular). These are the end-to-end witnesses that the footer
# block and the build-time caveat fire for them, exactly as for lmer.

.fit_singular_glmmTMB <- function() {
  skip_if_not_installed("glmmTMB")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  suppressWarnings(glmmTMB::glmmTMB(
    am ~ mpg + (1 | cyl),
    data = d,
    family = binomial
  ))
}

.fit_singular_lme <- function() {
  skip_if_not_installed("nlme")
  d <- mtcars
  d$gearf <- factor(d$gear)
  nlme::lme(mpg ~ wt + hp, data = d, random = ~ 1 | gearf)
}

.fit_clean_glmmTMB <- function() {
  skip_if_not_installed("glmmTMB")
  glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}

.fit_clean_lme <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )
}

test_that("singular glmmTMB gets the mixed-effects singular footer", {
  fit <- .fit_singular_glmmTMB()
  combined <- paste(
    capture.output(print(suppressWarnings(table_regression(fit)))),
    collapse = "\n"
  )
  expect_match(
    combined,
    "estimated at the boundary (0); their Wald SE and CI are omitted",
    fixed = TRUE
  )
  expect_false(grepl("Rank-deficient model", combined, fixed = TRUE))
})

test_that("singular lme gets the mixed-effects singular footer", {
  fit <- .fit_singular_lme()
  combined <- paste(
    capture.output(print(suppressWarnings(table_regression(fit)))),
    collapse = "\n"
  )
  expect_match(
    combined,
    "estimated at the boundary (0); their Wald SE and CI are omitted",
    fixed = TRUE
  )
  expect_false(grepl("Rank-deficient model", combined, fixed = TRUE))
})

test_that("healthy glmmTMB / lme fits stay free of the singular footer", {
  for (fit in list(.fit_clean_glmmTMB(), .fit_clean_lme())) {
    combined <- paste(
      capture.output(print(suppressWarnings(table_regression(fit)))),
      collapse = "\n"
    )
    expect_false(
      grepl("Singular fit:", combined, fixed = TRUE),
      info = paste("class:", class(fit)[1L])
    )
  }
})

test_that("singular glmmTMB / lme raise the build-time caveat warning", {
  for (fit in list(.fit_singular_glmmTMB(), .fit_singular_lme())) {
    caveat_seen <- FALSE
    withCallingHandlers(
      table_regression(fit),
      spicy_caveat = function(c) {
        caveat_seen <<- TRUE
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    )
    expect_true(caveat_seen, info = paste("class:", class(fit)[1L]))
  }
})

test_that("a healthy mixed fit raises no singular caveat", {
  for (fit in list(.fit_clean_glmmTMB(), .fit_clean_lme())) {
    caveat <- NULL
    withCallingHandlers(
      table_regression(fit),
      spicy_caveat = function(c) {
        caveat <<- conditionMessage(c)
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    )
    expect_false(
      isTRUE(grepl("Singular fit", caveat %||% "", fixed = TRUE)),
      info = paste("class:", class(fit)[1L])
    )
  }
})
