# ---------------------------------------------------------------------------
# Phase 0c sub-step C3 tests: byte-equivalence gates for the alignment
# pipeline migration.
#
# The frame-aware align_frames() must produce an aligned object SHAPE-
# IDENTICAL to align_extracts() so the body builder can consume it
# unchanged until C4. We test:
#
#   * coefs_aligned     : every column equal (legacy column names
#                         preserved, including the synthesised
#                         is_intercept / is_singular / factor_term /
#                         factor_level fallbacks)
#   * fit_stats_aligned : same numeric values column-by-column
#   * term_order        : identical character vector
#   * factor_ref_levels : identical named vector
#   * outcome_labels_auto, exp_headers_auto, model_ids, n_models
#     : identical
#
# Also covers attach_nested_stats_to_frames() / compute_canonical_term_order
# _from_frames() siblings since align_frames() uses them.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_lm <- function() {
  lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
}

.fit_lm_factor <- function() {
  d <- sochealth
  d$inc <- factor(
    as.character(d$income_group),
    levels = c("Low", "Lower middle", "Upper middle", "High")
  )
  lm(wellbeing_score ~ inc + sex, data = d)
}

.fit_lm_ordered <- function() {
  lm(wellbeing_score ~ income_group + sex, data = sochealth)
}

.fit_glm_logit <- function() {
  d <- sochealth
  d$smoker_bin <- as.integer(d$smoking == "Yes")
  glm(smoker_bin ~ age + sex + physical_activity,
      data = d, family = binomial(link = "logit"))
}


.legacy_for <- function(fits, model_ids) {
  lapply(seq_along(fits), function(i) {
    extract_lm_phase1(fits[[i]], model_id = model_ids[i])
  })
}

.frames_for <- function(fits, model_ids) {
  lapply(seq_along(fits), function(i) {
    as_regression_frame(fits[[i]], model_id = model_ids[i])
  })
}


# ---- Helper: assert shape equivalence of two aligned objects --------------

.expect_aligned_equal <- function(a, b, info = NULL) {
  # Slot names identical
  expect_setequal(names(a), names(b))

  # Scalar / vector slots
  expect_identical(a$term_order,        b$term_order,        info = info)
  expect_identical(a$factor_ref_levels, b$factor_ref_levels, info = info)
  expect_identical(a$outcome_labels_auto, b$outcome_labels_auto,
                   info = info)
  expect_identical(a$exp_headers_auto,  b$exp_headers_auto,  info = info)
  expect_identical(a$model_ids,         b$model_ids,         info = info)
  expect_identical(a$n_models,          b$n_models,          info = info)

  # coefs_aligned: every column equal. Order of rows is determined by
  # align logic and is identical for both paths.
  expect_setequal(names(a$coefs_aligned), names(b$coefs_aligned))
  for (col in names(a$coefs_aligned)) {
    expect_equal(a$coefs_aligned[[col]], b$coefs_aligned[[col]],
                 info = paste(info, "/ coefs_aligned column:", col))
  }

  # fit_stats_aligned: column-by-column numeric equality (tolerance for
  # floats; identical for ints / chars).
  expect_setequal(names(a$fit_stats_aligned), names(b$fit_stats_aligned))
  for (col in names(a$fit_stats_aligned)) {
    expect_equal(a$fit_stats_aligned[[col]], b$fit_stats_aligned[[col]],
                 tolerance = 1e-10,
                 info = paste(info, "/ fit_stats column:", col))
  }
}


# ---- 1. align_frames() equivalence: single model -------------------------

test_that("align_frames: single lm matches align_extracts", {
  fit <- .fit_lm()
  legacy  <- align_extracts(.legacy_for(list(fit), "M1"))
  via_fr  <- align_frames(.frames_for(list(fit), "M1"), model_ids = "M1")
  .expect_aligned_equal(via_fr, legacy, info = "single lm")
})

test_that("align_frames: single lm with treatment factor matches", {
  fit <- .fit_lm_factor()
  legacy <- align_extracts(.legacy_for(list(fit), "M1"))
  via_fr <- align_frames(.frames_for(list(fit), "M1"), model_ids = "M1")
  .expect_aligned_equal(via_fr, legacy, info = "lm with factor")
})

test_that("align_frames: single lm with ordered factor matches", {
  fit <- .fit_lm_ordered()
  legacy <- align_extracts(.legacy_for(list(fit), "M1"))
  via_fr <- align_frames(.frames_for(list(fit), "M1"), model_ids = "M1")
  .expect_aligned_equal(via_fr, legacy, info = "lm with ordered factor")
})

test_that("align_frames: single glm logistic matches", {
  fit <- .fit_glm_logit()
  legacy <- align_extracts(.legacy_for(list(fit), "M1"))
  via_fr <- align_frames(.frames_for(list(fit), "M1"), model_ids = "M1")
  .expect_aligned_equal(via_fr, legacy, info = "glm logistic")
})


# ---- 2. align_frames() equivalence: multi-model --------------------------

test_that("align_frames: two lms with same DV (identical predictors)", {
  fits <- list(.fit_lm(), .fit_lm())
  legacy <- align_extracts(.legacy_for(fits, c("M1", "M2")))
  via_fr <- align_frames(.frames_for(fits, c("M1", "M2")),
                         model_ids = c("M1", "M2"))
  .expect_aligned_equal(via_fr, legacy, info = "two identical lms")
})

test_that("align_frames: two lms with different predictors (term union)", {
  f1 <- lm(wellbeing_score ~ age,           data = sochealth)
  f2 <- lm(wellbeing_score ~ age + sex,     data = sochealth)
  fits <- list(f1, f2)
  legacy <- align_extracts(.legacy_for(fits, c("M1", "M2")))
  via_fr <- align_frames(.frames_for(fits, c("M1", "M2")),
                         model_ids = c("M1", "M2"))
  .expect_aligned_equal(via_fr, legacy, info = "two lms term union")
})

test_that("align_frames: lm + glm mixed matches", {
  fits <- list(.fit_lm(), .fit_glm_logit())
  legacy <- align_extracts(.legacy_for(fits, c("M1", "M2")))
  via_fr <- align_frames(.frames_for(fits, c("M1", "M2")),
                         model_ids = c("M1", "M2"))
  .expect_aligned_equal(via_fr, legacy, info = "lm + glm mixed")
})


# ---- 3. align_frames() equivalence: argument combinations ----------------

test_that("align_frames: show_intercept = FALSE matches", {
  fit <- .fit_lm()
  legacy <- align_extracts(.legacy_for(list(fit), "M1"),
                            show_intercept = FALSE)
  via_fr <- align_frames(.frames_for(list(fit), "M1"),
                         model_ids = "M1",
                         show_intercept = FALSE)
  .expect_aligned_equal(via_fr, legacy, info = "no intercept")
})

test_that("align_frames: intercept_position = 'last' matches", {
  fit <- .fit_lm()
  legacy <- align_extracts(.legacy_for(list(fit), "M1"),
                            intercept_position = "last")
  via_fr <- align_frames(.frames_for(list(fit), "M1"),
                         model_ids = "M1",
                         intercept_position = "last")
  .expect_aligned_equal(via_fr, legacy, info = "intercept last")
})

test_that("align_frames: reference_style = 'annotation' drops ref rows", {
  fit <- .fit_lm_factor()
  legacy <- align_extracts(.legacy_for(list(fit), "M1"),
                            reference_style = "annotation")
  via_fr <- align_frames(.frames_for(list(fit), "M1"),
                         model_ids = "M1",
                         reference_style = "annotation")
  .expect_aligned_equal(via_fr, legacy, info = "ref-style annotation")
})

test_that("align_frames: reference_style = 'footer' drops ref rows", {
  fit <- .fit_lm_factor()
  legacy <- align_extracts(.legacy_for(list(fit), "M1"),
                            reference_style = "footer")
  via_fr <- align_frames(.frames_for(list(fit), "M1"),
                         model_ids = "M1",
                         reference_style = "footer")
  .expect_aligned_equal(via_fr, legacy, info = "ref-style footer")
})


# ---- 4. align_frames() edge cases ----------------------------------------

test_that("align_frames: empty list returns the empty shape", {
  out <- align_frames(list(), model_ids = character(0))
  expect_identical(out$n_models, 0L)
  expect_identical(nrow(out$coefs_aligned), 0L)
  expect_identical(nrow(out$fit_stats_aligned), 0L)
})


# ---- 5. attach_nested_stats_to_frames() ----------------------------------

test_that("attach_nested_stats_to_frames: short input (<= 1 fit) is identity", {
  fit <- .fit_lm()
  frames <- .frames_for(list(fit), "M1")
  expect_identical(
    attach_nested_stats_to_frames(frames, list(fit)),
    frames
  )
})

test_that("attach_nested_stats_to_frames: change tokens injected into fit_stats", {
  f1 <- lm(wellbeing_score ~ age,           data = sochealth)
  f2 <- lm(wellbeing_score ~ age + sex,     data = sochealth)
  fits   <- list(f1, f2)
  frames <- .frames_for(fits, c("M1", "M2"))
  out <- attach_nested_stats_to_frames(frames, fits)

  # The change tokens are NA in model 1 (no previous) and finite in
  # model 2.
  expect_true(is.na(out[[1]]$info$fit_stats$r2_change))
  expect_true(is.finite(out[[2]]$info$fit_stats$r2_change))
})

test_that("attach_nested_stats: extracts vs frames produce equivalent values", {
  f1 <- lm(wellbeing_score ~ age,           data = sochealth)
  f2 <- lm(wellbeing_score ~ age + sex,     data = sochealth)
  fits <- list(f1, f2)
  legacy   <- attach_nested_stats_to_extracts(.legacy_for(fits, c("M1", "M2")),
                                              fits)
  via_fr   <- attach_nested_stats_to_frames(.frames_for(fits, c("M1", "M2")),
                                            fits)
  # Pick the same change key from each path and assert equality.
  for (i in 1:2) {
    leg_fs <- legacy[[i]]$fit_stats
    fr_fs  <- via_fr[[i]]$info$fit_stats
    for (key in c("r2_change", "f_change", "p_change")) {
      expect_equal(leg_fs[[key]][1L], fr_fs[[key]],
                   tolerance = 1e-10,
                   info = paste("model", i, "/ key", key))
    }
  }
})


# ---- 6. compute_canonical_term_order_from_frames() -----------------------

test_that("compute_canonical_term_order_from_frames matches extracts version", {
  fits <- list(.fit_lm(), .fit_lm_factor())
  extracts <- .legacy_for(fits, c("M1", "M2"))
  frames   <- .frames_for(fits, c("M1", "M2"))
  expect_identical(
    compute_canonical_term_order_from_frames(frames),
    compute_canonical_term_order(extracts)
  )
})

test_that("compute_canonical_term_order_from_frames: empty input -> character(0)", {
  expect_identical(
    compute_canonical_term_order_from_frames(list()),
    character(0)
  )
})


# ---- 7. align_frames + nested change tokens --------------------------------

test_that("align_frames with nested stats matches align_extracts with nested stats", {
  f1 <- lm(wellbeing_score ~ age,           data = sochealth)
  f2 <- lm(wellbeing_score ~ age + sex,     data = sochealth)
  fits <- list(f1, f2)

  legacy_in <- attach_nested_stats_to_extracts(.legacy_for(fits, c("M1", "M2")),
                                                fits)
  frames_in <- attach_nested_stats_to_frames(.frames_for(fits, c("M1", "M2")),
                                              fits)
  legacy_out <- align_extracts(legacy_in)
  via_fr_out <- align_frames(frames_in, model_ids = c("M1", "M2"))

  # The change tokens are part of fit_stats_aligned. Compare those
  # columns explicitly to surface any miswiring.
  for (key in c("r2_change", "f_change", "p_change")) {
    expect_equal(via_fr_out$fit_stats_aligned[[key]],
                 legacy_out$fit_stats_aligned[[key]],
                 tolerance = 1e-10,
                 info = paste("change key:", key))
  }
  # And the full aligned objects.
  .expect_aligned_equal(via_fr_out, legacy_out,
                       info = "nested stats path")
})
