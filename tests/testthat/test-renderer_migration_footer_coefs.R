# ---------------------------------------------------------------------------
# Phase 0c sub-step C2.c tests: byte-equivalence gates for the four
# coefs-touching footer-block builders.
#
# Subtler than C2.a / C2.b because the builders scan the per-row coefs
# data.frame. The column-rename map established in Phase 0b sub-step 2
# is applied:
#   factor_term  -> parent_var      (was NA for non-factor; now == term)
#   factor_level -> label           (was NA for non-factor; now == term)
#   is_reference -> is_ref
#   is_intercept -> derived: term == "(Intercept)"
#
# Builders covered:
#   * build_p_adjust_footer_block
#   * build_polynomial_contrasts_footer_block
#   * build_reference_categories_footer_block
#   * build_standardized_caveat_footer_block
#
# After this commit, 11 / 12 footer-block builders have _from_frames
# siblings. The only block without one is build_nested_footer_block(),
# which takes only `nested` (no extracts arg) and needs no migration.
# C2.last will flip the dispatcher.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fixture_lm <- function() {
  lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
}

.fixture_lm_treatment_factor <- function() {
  d <- sochealth
  d$inc <- factor(
    as.character(d$income_group),
    levels = c("Low", "Lower middle", "Upper middle", "High")
  )
  lm(wellbeing_score ~ inc + sex, data = d)
}

.fixture_lm_ordered_factor <- function() {
  # `income_group` is an ordered factor in sochealth -> polynomial
  # contrasts (.L / .Q / .C).
  lm(wellbeing_score ~ income_group + sex, data = sochealth)
}

.fixture_lm_interaction <- function() {
  lm(wellbeing_score ~ age * sex + smoking, data = sochealth)
}

.fixture_glm_logistic <- function() {
  d <- sochealth
  d$smoker_bin <- as.integer(d$smoking == "Yes")
  glm(smoker_bin ~ age + sex + physical_activity,
      data = d, family = binomial(link = "logit"))
}

.legacy <- function(fit, model_id = "M1") {
  extract_lm_phase1(fit = fit, model_id = model_id)
}

.frame_of <- function(fit, model_id = "M1") {
  as_regression_frame(fit, model_id = model_id)
}


# ---- 1. p_adjust footer block ---------------------------------------------

test_that("p_adjust: p_adjust = 'none' -> NULL via both paths", {
  fit <- .fixture_lm()
  expect_identical(
    build_p_adjust_footer_block_from_frames(list(.frame_of(fit)), "none"),
    build_p_adjust_footer_block(list(.legacy(fit)), "none")
  )
})

test_that("p_adjust: uniform m across single lm matches", {
  fit <- .fixture_lm()
  expect_identical(
    build_p_adjust_footer_block_from_frames(list(.frame_of(fit)), "holm"),
    build_p_adjust_footer_block(list(.legacy(fit)), "holm")
  )
})

test_that("p_adjust: lm with treatment factor (excludes ref rows)", {
  fit <- .fixture_lm_treatment_factor()
  expect_identical(
    build_p_adjust_footer_block_from_frames(list(.frame_of(fit)), "bonferroni"),
    build_p_adjust_footer_block(list(.legacy(fit)), "bonferroni")
  )
})

test_that("p_adjust: two models with different family sizes", {
  f1 <- .fixture_lm()                      # 4 coefs (incl. intercept)
  f2 <- .fixture_lm_treatment_factor()     # more coefs (intercept + 3 inc + sex)
  expect_identical(
    build_p_adjust_footer_block_from_frames(
      list(.frame_of(f1, "M1"), .frame_of(f2, "M2")), "holm"),
    build_p_adjust_footer_block(
      list(.legacy(f1, "M1"), .legacy(f2, "M2")), "holm")
  )
})

test_that("p_adjust: glm logistic matches", {
  fit <- .fixture_glm_logistic()
  expect_identical(
    build_p_adjust_footer_block_from_frames(list(.frame_of(fit)), "holm"),
    build_p_adjust_footer_block(list(.legacy(fit)), "holm")
  )
})

test_that("p_adjust: empty / NULL p_adjust returns NULL", {
  fit <- .fixture_lm()
  expect_null(
    build_p_adjust_footer_block_from_frames(list(.frame_of(fit)), NULL)
  )
  expect_null(
    build_p_adjust_footer_block_from_frames(list(), "holm")
  )
})


# ---- 2. polynomial_contrasts footer block ---------------------------------

test_that("polynomial: lm with ordered factor matches", {
  fit <- .fixture_lm_ordered_factor()
  # The legacy builder calls inform_polynomial_pedagogy() with an rlang
  # one-shot inform. Both paths fire it once per session; we just compare
  # the returned string.
  expect_identical(
    suppressMessages(
      build_polynomial_contrasts_footer_block_from_frames(list(.frame_of(fit)))),
    suppressMessages(
      build_polynomial_contrasts_footer_block(list(.legacy(fit))))
  )
})

test_that("polynomial: lm without ordered factor returns NULL via both paths", {
  fit <- .fixture_lm()
  expect_identical(
    build_polynomial_contrasts_footer_block_from_frames(list(.frame_of(fit))),
    build_polynomial_contrasts_footer_block(list(.legacy(fit)))
  )
  expect_null(
    build_polynomial_contrasts_footer_block_from_frames(list(.frame_of(fit)))
  )
})

test_that("polynomial: two lms with the same ordered factor matches", {
  fit1 <- .fixture_lm_ordered_factor()
  fit2 <- .fixture_lm_ordered_factor()
  expect_identical(
    suppressMessages(
      build_polynomial_contrasts_footer_block_from_frames(list(
        .frame_of(fit1, "M1"), .frame_of(fit2, "M2")))),
    suppressMessages(
      build_polynomial_contrasts_footer_block(list(
        .legacy(fit1, "M1"), .legacy(fit2, "M2"))))
  )
})

test_that("polynomial: empty / non-list returns NULL", {
  expect_null(build_polynomial_contrasts_footer_block_from_frames(list()))
  expect_null(build_polynomial_contrasts_footer_block_from_frames("nope"))
})


# ---- 3. reference_categories footer block ---------------------------------

test_that("reference_categories: reference_style != 'footer' -> NULL", {
  fit <- .fixture_lm_treatment_factor()
  expect_null(
    build_reference_categories_footer_block_from_frames(
      list(.frame_of(fit)), reference_style = "row")
  )
})

test_that("reference_categories: lm with treatment factor matches", {
  fit <- .fixture_lm_treatment_factor()
  expect_identical(
    build_reference_categories_footer_block_from_frames(
      list(.frame_of(fit)), reference_style = "footer"),
    build_reference_categories_footer_block(
      list(.legacy(fit)), reference_style = "footer")
  )
})

test_that("reference_categories: lm without any factor returns NULL", {
  fit <- .fixture_lm()   # only numeric predictors + smoking (binary factor)
  expect_identical(
    build_reference_categories_footer_block_from_frames(
      list(.frame_of(fit)), reference_style = "footer"),
    build_reference_categories_footer_block(
      list(.legacy(fit)), reference_style = "footer")
  )
})

test_that("reference_categories: two lms with same factor (dedup pairs)", {
  fit1 <- .fixture_lm_treatment_factor()
  fit2 <- .fixture_lm_treatment_factor()
  expect_identical(
    build_reference_categories_footer_block_from_frames(
      list(.frame_of(fit1, "M1"), .frame_of(fit2, "M2")),
      reference_style = "footer"),
    build_reference_categories_footer_block(
      list(.legacy(fit1, "M1"), .legacy(fit2, "M2")),
      reference_style = "footer")
  )
})

test_that("reference_categories: empty / non-list returns NULL", {
  expect_null(build_reference_categories_footer_block_from_frames(
    list(), reference_style = "footer"))
  expect_null(build_reference_categories_footer_block_from_frames(
    "nope", reference_style = "footer"))
})


# ---- 4. standardized_caveat footer block ----------------------------------

test_that("standardized_caveat: standardized = 'none' -> NULL via both paths", {
  fit <- .fixture_lm_interaction()
  expect_identical(
    build_standardized_caveat_footer_block_from_frames(
      list(.frame_of(fit)), standardized = "none"),
    build_standardized_caveat_footer_block(
      list(.legacy(fit)), standardized = "none")
  )
})

test_that("standardized_caveat: lm without interaction, standardized = 'refit' -> NULL", {
  fit <- .fixture_lm()
  expect_identical(
    build_standardized_caveat_footer_block_from_frames(
      list(.frame_of(fit)), standardized = "refit"),
    build_standardized_caveat_footer_block(
      list(.legacy(fit)), standardized = "refit")
  )
})

test_that("standardized_caveat: lm WITH interaction, standardized = 'refit' triggers caveat", {
  fit <- .fixture_lm_interaction()
  # Production pipeline (table_regression.R:1234) attaches
  # detect_non_additive_terms(fit) BOTH to extracts[[i]][["non_additive"]]
  # and to frames[[i]]$info$extras$non_additive. Unit-level tests must
  # mirror this attachment to test parity (the orchestrator's invariant).
  non_add <- detect_non_additive_terms(fit)
  leg <- .legacy(fit); leg[["non_additive"]] <- non_add
  fr  <- .frame_of(fit); fr$info$extras$non_additive <- non_add
  expect_identical(
    build_standardized_caveat_footer_block_from_frames(
      list(fr), standardized = "refit"),
    build_standardized_caveat_footer_block(
      list(leg), standardized = "refit")
  )
  # And the caveat is actually emitted (non-NULL) -- confirms the
  # detection logic fires on interaction terms.
  expect_match(
    build_standardized_caveat_footer_block_from_frames(
      list(fr), standardized = "refit"),
    "Standardised", fixed = TRUE
  )
})

test_that("standardized_caveat: empty / non-list returns NULL", {
  expect_null(
    build_standardized_caveat_footer_block_from_frames(
      list(), standardized = "refit")
  )
  expect_null(
    build_standardized_caveat_footer_block_from_frames(
      "nope", standardized = "refit")
  )
})


# ---- Per-field round-trip checks ------------------------------------------

test_that("p_adjust families: B-row count excluding intercept and ref matches", {
  for (fit in list(.fixture_lm(),
                   .fixture_lm_treatment_factor(),
                   .fixture_glm_logistic())) {
    fr  <- .frame_of(fit)
    leg <- .legacy(fit)
    n_frame <- sum(fr$coefs$estimate_type == "B" &
                     fr$coefs$term != "(Intercept)" &
                     !fr$coefs$is_ref &
                     !is.na(fr$coefs$p_value))
    n_leg <- sum(leg$coefs$estimate_type == "B" &
                   !leg$coefs$is_intercept &
                   !leg$coefs$is_reference &
                   !is.na(leg$coefs$p_value))
    expect_identical(n_frame, n_leg,
                     info = paste("class:", class(fit)[1]))
  }
})

test_that("polynomial label round-trip: legacy factor_level == frame label for ordered-factor rows", {
  fit <- .fixture_lm_ordered_factor()
  fr  <- .frame_of(fit)
  leg <- .legacy(fit)
  # Rows where the legacy `factor_level` starts with "." or "^" must
  # match the frame `label` 1:1 on the same row position.
  is_poly_leg <- !is.na(leg$coefs$factor_level) &
    (startsWith(leg$coefs$factor_level, ".") |
       startsWith(leg$coefs$factor_level, "^"))
  is_poly_fr  <- !is.na(fr$coefs$label) &
    (startsWith(fr$coefs$label, ".") |
       startsWith(fr$coefs$label, "^"))
  expect_identical(is_poly_leg, is_poly_fr)
  expect_identical(leg$coefs$factor_level[is_poly_leg],
                   fr$coefs$label[is_poly_fr])
  expect_identical(leg$coefs$factor_term[is_poly_leg],
                   fr$coefs$parent_var[is_poly_fr])
})

test_that("reference row round-trip: is_reference / factor_term / factor_level", {
  fit <- .fixture_lm_treatment_factor()
  fr  <- .frame_of(fit)
  leg <- .legacy(fit)
  # is_ref rows must match the legacy is_reference rows.
  expect_identical(leg$coefs$is_reference, fr$coefs$is_ref)
  # On the is_ref rows the parent_var / label must match the legacy
  # factor_term / factor_level.
  ref_mask <- isTRUE_vec(leg$coefs$is_reference)
  expect_identical(leg$coefs$factor_term[ref_mask],
                   fr$coefs$parent_var[ref_mask])
  expect_identical(leg$coefs$factor_level[ref_mask],
                   fr$coefs$label[ref_mask])
})
