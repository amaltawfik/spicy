# ---------------------------------------------------------------------------
# Phase 0c sub-step C2.a tests: byte-equivalence gates for the frame-aware
# siblings of the simple (scalar-only) footer-block builders.
#
# Each test asserts that the new *_from_frames variant produces output
# byte-identical to the legacy *_from_extracts builder on the same fixture.
# The dispatcher build_regression_footer() still consumes extracts; it
# flips to frames in sub-step C2.last once every block has a sibling.
#
# Builders covered in this commit:
#   * build_regression_type_footer_block
#   * build_vcov_footer_block  (+ format_vcov_label helper)
#   * build_singular_footer_block
#
# Builders deferred to subsequent C2 sub-commits:
#   * build_ame_satterthwaite_footer_block (reads use_ame_satterthwaite, is_glm)
#   * build_exponentiate_footer_block      (reads exp_applied, exp_header)
#   * build_abbreviations_footer_block     (reads exp_applied / exp_header
#                                           + show_columns scan)
#   * build_p_adjust_footer_block          (reads coefs columns)
#   * build_standardized_caveat_footer_block (reads non_additive + fit)
#   * build_polynomial_contrasts_footer_block (reads coefs)
#   * build_reference_categories_footer_block (reads coefs)
#   * build_stars_footer_block, build_nested_footer_block: no extracts arg,
#     not part of the migration
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fixture_lm <- function() {
  lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
}

.fixture_glm_logistic <- function() {
  d <- sochealth
  d$smoker_bin <- as.integer(d$smoking == "Yes")
  glm(smoker_bin ~ age + sex + physical_activity,
      data = d, family = binomial(link = "logit"))
}

.fixture_singular_lm <- function() {
  # Force a rank-deficient model: include a perfectly collinear predictor.
  d <- sochealth
  d$age_twice <- d$age * 2
  d$age_clone <- d$age   # collinear with age
  lm(wellbeing_score ~ age + age_clone + sex, data = d)
}

.legacy <- function(fit, vcov_type = "classical", cluster = NULL,
                    cluster_name = NULL, model_id = "M1") {
  extract_lm_phase1(
    fit          = fit,
    model_id     = model_id,
    vcov_type    = vcov_type,
    cluster      = cluster,
    cluster_name = cluster_name
  )
}

.frame_of <- function(fit, vcov = "classical", cluster = NULL,
                     cluster_name = NULL, model_id = "M1") {
  as_regression_frame(fit,
                      model_id     = model_id,
                      vcov         = vcov,
                      cluster      = cluster,
                      cluster_name = cluster_name)
}


# ---- 1. regression_type footer block --------------------------------------

test_that("regression_type: single lm matches", {
  fit <- .fixture_lm()
  expect_identical(
    build_regression_type_footer_block_from_frames(list(.frame_of(fit))),
    build_regression_type_footer_block(list(.legacy(fit)))
  )
})

test_that("regression_type: single glm logistic matches", {
  fit <- .fixture_glm_logistic()
  expect_identical(
    build_regression_type_footer_block_from_frames(list(.frame_of(fit))),
    build_regression_type_footer_block(list(.legacy(fit)))
  )
})

test_that("regression_type: two lms (uniform family) matches", {
  f1 <- .fixture_lm()
  f2 <- lm(bmi ~ age + sex, data = sochealth)
  expect_identical(
    build_regression_type_footer_block_from_frames(
      list(.frame_of(f1, model_id = "M1"), .frame_of(f2, model_id = "M2"))),
    build_regression_type_footer_block(
      list(.legacy(f1, model_id = "M1"), .legacy(f2, model_id = "M2")))
  )
})

test_that("regression_type: lm + glm (heterogeneous) matches", {
  f1 <- .fixture_lm()
  f2 <- .fixture_glm_logistic()
  expect_identical(
    build_regression_type_footer_block_from_frames(
      list(.frame_of(f1, model_id = "M1"), .frame_of(f2, model_id = "M2"))),
    build_regression_type_footer_block(
      list(.legacy(f1, model_id = "M1"), .legacy(f2, model_id = "M2")))
  )
})

test_that("regression_type: empty / non-list returns NULL through both paths", {
  expect_null(build_regression_type_footer_block_from_frames(list()))
  expect_null(build_regression_type_footer_block_from_frames("nope"))
  expect_identical(
    build_regression_type_footer_block_from_frames(list()),
    build_regression_type_footer_block(list())
  )
})


# ---- 2. vcov footer block (+ format_vcov_label helper) --------------------

test_that("format_vcov_label: classical lm matches", {
  fit <- .fixture_lm()
  expect_identical(
    format_vcov_label_from_frame(.frame_of(fit)),
    format_vcov_label(.legacy(fit))
  )
})

test_that("format_vcov_label: classical glm matches", {
  fit <- .fixture_glm_logistic()
  expect_identical(
    format_vcov_label_from_frame(.frame_of(fit)),
    format_vcov_label(.legacy(fit))
  )
})

test_that("format_vcov_label: HC3 lm matches", {
  fit <- .fixture_lm()
  expect_identical(
    format_vcov_label_from_frame(.frame_of(fit, vcov = "HC3")),
    format_vcov_label(.legacy(fit, vcov_type = "HC3"))
  )
})

test_that("format_vcov_label: bootstrap matches", {
  fit <- .fixture_lm()
  expect_identical(
    format_vcov_label_from_frame(.frame_of(fit, vcov = "bootstrap")),
    format_vcov_label(.legacy(fit, vcov_type = "bootstrap"))
  )
})

test_that("vcov_footer_block: uniform classical matches", {
  fit <- .fixture_lm()
  expect_identical(
    build_vcov_footer_block_from_frames(list(.frame_of(fit))),
    build_vcov_footer_block(list(.legacy(fit)))
  )
})

test_that("vcov_footer_block: uniform HC3 matches", {
  fit <- .fixture_lm()
  expect_identical(
    build_vcov_footer_block_from_frames(list(.frame_of(fit, vcov = "HC3"))),
    build_vcov_footer_block(list(.legacy(fit, vcov_type = "HC3")))
  )
})

test_that("vcov_footer_block: heterogeneous (classical + HC3) matches", {
  f1 <- .fixture_lm()
  f2 <- .fixture_lm()
  expect_identical(
    build_vcov_footer_block_from_frames(list(
      .frame_of(f1, vcov = "classical", model_id = "M1"),
      .frame_of(f2, vcov = "HC3",       model_id = "M2"))),
    build_vcov_footer_block(list(
      .legacy(f1, vcov_type = "classical", model_id = "M1"),
      .legacy(f2, vcov_type = "HC3",       model_id = "M2")))
  )
})

test_that("vcov_footer_block: empty / non-list returns NULL", {
  expect_null(build_vcov_footer_block_from_frames(list()))
  expect_null(build_vcov_footer_block_from_frames("nope"))
})


# ---- 3. singular footer block ---------------------------------------------

test_that("singular_footer_block: no singular -> NULL via both paths", {
  fit <- .fixture_lm()
  expect_identical(
    build_singular_footer_block_from_frames(list(.frame_of(fit))),
    build_singular_footer_block(list(.legacy(fit)))
  )
  expect_null(build_singular_footer_block_from_frames(list(.frame_of(fit))))
})

test_that("singular_footer_block: rank-deficient single model matches", {
  fit <- .fixture_singular_lm()
  expect_identical(
    build_singular_footer_block_from_frames(list(.frame_of(fit))),
    build_singular_footer_block(list(.legacy(fit)))
  )
})

test_that("singular_footer_block: rank-deficient model in a multi-model list", {
  f_clean <- .fixture_lm()
  f_bad   <- .fixture_singular_lm()
  expect_identical(
    build_singular_footer_block_from_frames(list(
      .frame_of(f_clean, model_id = "M1"),
      .frame_of(f_bad,   model_id = "M2"))),
    build_singular_footer_block(list(
      .legacy(f_clean, model_id = "M1"),
      .legacy(f_bad,   model_id = "M2")))
  )
})

test_that("singular_footer_block: empty / non-list returns NULL", {
  expect_null(build_singular_footer_block_from_frames(list()))
  expect_null(build_singular_footer_block_from_frames("nope"))
})


# ---- Per-field round-trip checks ------------------------------------------

test_that("vcov-related fields round-trip from frame back to extract shape", {
  fit <- .fixture_lm()
  for (vt in c("classical", "HC0", "HC1", "HC2", "HC3", "HC4", "HC5",
               "bootstrap", "jackknife")) {
    fr  <- .frame_of(fit, vcov = vt)
    leg <- .legacy(fit, vcov_type = vt)
    expect_identical(fr$info$vcov_kind, leg$vcov_type,
                     info = paste("vcov =", vt))
  }
})

test_that("singular flag round-trips", {
  for (fit in list(.fixture_lm(), .fixture_singular_lm())) {
    fr  <- .frame_of(fit)
    leg <- .legacy(fit)
    expect_identical(isTRUE(fr$info$extras$has_singular),
                     isTRUE(leg$has_singular),
                     info = paste("class:", class(fit)[1]))
  }
})
