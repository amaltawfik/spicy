# ---------------------------------------------------------------------------
# Phase 0c sub-step C2.b tests: byte-equivalence gates for three more
# scalar-reading footer-block builders.
#
# Builders covered in this commit:
#   * build_abbreviations_footer_block       (reads exp_applied, exp_header)
#   * build_ame_satterthwaite_footer_block   (reads use_ame_satterthwaite, is_glm)
#   * build_exponentiate_footer_block        (reads exp_applied, exp_header)
#
# Cumulative: 7 / 12 footer-block builders now have a _from_frames sibling.
# Remaining for C2.c (coefs-touching):
#   * build_p_adjust_footer_block
#   * build_standardized_caveat_footer_block
#   * build_polynomial_contrasts_footer_block
#   * build_reference_categories_footer_block
# (build_stars_footer_block and build_nested_footer_block take no extracts
# arg; nothing to migrate.)
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

.fixture_glm_poisson <- function() {
  d <- sochealth
  d$counter <- as.integer(d$age %% 5)
  glm(counter ~ sex + smoking, data = d, family = poisson(link = "log"))
}

.legacy <- function(fit, exponentiate = FALSE, model_id = "M1") {
  extract_lm_phase1(fit = fit, model_id = model_id,
                    exponentiate = exponentiate)
}

.frame_of <- function(fit, exponentiate = FALSE, model_id = "M1") {
  as_regression_frame(fit, model_id = model_id, exponentiate = exponentiate)
}


# ---- 1. abbreviations footer block ----------------------------------------

test_that("abbreviations: AME token present matches", {
  fit <- .fixture_lm()
  expect_identical(
    build_abbreviations_footer_block_from_frames(
      show_columns = c("b", "ame"),
      frames = list(.frame_of(fit))
    ),
    build_abbreviations_footer_block(
      show_columns = c("b", "ame"),
      extracts = list(.legacy(fit))
    )
  )
})

test_that("abbreviations: standardized = refit triggers beta definition", {
  fit <- .fixture_lm()
  expect_identical(
    build_abbreviations_footer_block_from_frames(
      show_columns = c("b", "beta"),
      frames = list(.frame_of(fit)),
      standardized = "refit"
    ),
    build_abbreviations_footer_block(
      show_columns = c("b", "beta"),
      extracts = list(.legacy(fit)),
      standardized = "refit"
    )
  )
})

test_that("abbreviations: glm exponentiate triggers OR definition", {
  fit <- .fixture_glm_logistic()
  expect_identical(
    build_abbreviations_footer_block_from_frames(
      show_columns = c("b", "ci", "p"),
      frames = list(.frame_of(fit, exponentiate = TRUE))
    ),
    build_abbreviations_footer_block(
      show_columns = c("b", "ci", "p"),
      extracts = list(.legacy(fit, exponentiate = TRUE))
    )
  )
})

test_that("abbreviations: lm + poisson with exponentiate triggers OR / IRR", {
  fit_lm  <- .fixture_lm()
  fit_pos <- .fixture_glm_poisson()
  expect_identical(
    build_abbreviations_footer_block_from_frames(
      show_columns = c("b", "ci", "p"),
      frames = list(
        .frame_of(fit_lm,  model_id = "M1"),
        .frame_of(fit_pos, exponentiate = TRUE, model_id = "M2")
      )
    ),
    build_abbreviations_footer_block(
      show_columns = c("b", "ci", "p"),
      extracts = list(
        .legacy(fit_lm,  model_id = "M1"),
        .legacy(fit_pos, exponentiate = TRUE, model_id = "M2")
      )
    )
  )
})

test_that("abbreviations: partial_f2 in show_columns triggers f² definition", {
  fit <- .fixture_lm()
  expect_identical(
    build_abbreviations_footer_block_from_frames(
      show_columns = c("b", "partial_f2"),
      frames = list(.frame_of(fit))
    ),
    build_abbreviations_footer_block(
      show_columns = c("b", "partial_f2"),
      extracts = list(.legacy(fit))
    )
  )
})

test_that("abbreviations: empty / non-matching show_columns returns NULL", {
  fit <- .fixture_lm()
  expect_null(
    build_abbreviations_footer_block_from_frames(
      show_columns = c("b", "ci", "p"),
      frames = list(.frame_of(fit))
    )
  )
})


# ---- 2. ame_satterthwaite footer block ------------------------------------

test_that("ame_satterthwaite: no AME token in show_columns -> NULL", {
  fit <- .fixture_lm()
  expect_null(
    build_ame_satterthwaite_footer_block_from_frames(
      list(.frame_of(fit)),
      show_columns = c("b", "ci")
    )
  )
})

test_that("ame_satterthwaite: no satt flag -> NULL via both paths", {
  fit <- .fixture_lm()
  expect_identical(
    build_ame_satterthwaite_footer_block_from_frames(
      list(.frame_of(fit)),
      show_columns = c("b", "ame")
    ),
    build_ame_satterthwaite_footer_block(
      list(.legacy(fit)),
      show_columns = c("b", "ame")
    )
  )
})

test_that("ame_satterthwaite: satt flag set, lm only", {
  # extract_lm_phase1() accepts use_ame_satterthwaite = TRUE; mirror that
  # in the frame via .build_info() which reads it from the legacy extract.
  fit <- .fixture_lm()
  expect_identical(
    build_ame_satterthwaite_footer_block_from_frames(
      list(as_regression_frame(fit, use_ame_satterthwaite = TRUE)),
      show_columns = c("b", "ame")
    ),
    build_ame_satterthwaite_footer_block(
      list(extract_lm_phase1(fit, model_id = "M1",
                             use_ame_satterthwaite = TRUE)),
      show_columns = c("b", "ame")
    )
  )
})

test_that("ame_satterthwaite: satt flag set, glm only", {
  fit <- .fixture_glm_logistic()
  expect_identical(
    build_ame_satterthwaite_footer_block_from_frames(
      list(as_regression_frame(fit, use_ame_satterthwaite = TRUE)),
      show_columns = c("b", "ame")
    ),
    build_ame_satterthwaite_footer_block(
      list(extract_lm_phase1(fit, model_id = "M1",
                             use_ame_satterthwaite = TRUE)),
      show_columns = c("b", "ame")
    )
  )
})

test_that("ame_satterthwaite: lm + glm mixed", {
  f1 <- .fixture_lm()
  f2 <- .fixture_glm_logistic()
  expect_identical(
    build_ame_satterthwaite_footer_block_from_frames(
      list(
        as_regression_frame(f1, model_id = "M1", use_ame_satterthwaite = TRUE),
        as_regression_frame(f2, model_id = "M2", use_ame_satterthwaite = TRUE)
      ),
      show_columns = c("b", "ame")
    ),
    build_ame_satterthwaite_footer_block(
      list(
        extract_lm_phase1(f1, model_id = "M1", use_ame_satterthwaite = TRUE),
        extract_lm_phase1(f2, model_id = "M2", use_ame_satterthwaite = TRUE)
      ),
      show_columns = c("b", "ame")
    )
  )
})


# ---- 3. exponentiate footer block -----------------------------------------

test_that("exponentiate: not applied -> NULL via both paths", {
  fit <- .fixture_lm()
  expect_identical(
    build_exponentiate_footer_block_from_frames(list(.frame_of(fit))),
    build_exponentiate_footer_block(list(.legacy(fit)))
  )
})

test_that("exponentiate: glm logistic with exponentiate matches", {
  fit <- .fixture_glm_logistic()
  expect_identical(
    build_exponentiate_footer_block_from_frames(
      list(.frame_of(fit, exponentiate = TRUE))),
    build_exponentiate_footer_block(
      list(.legacy(fit, exponentiate = TRUE)))
  )
})

test_that("exponentiate: poisson with exponentiate matches", {
  fit <- .fixture_glm_poisson()
  expect_identical(
    build_exponentiate_footer_block_from_frames(
      list(.frame_of(fit, exponentiate = TRUE))),
    build_exponentiate_footer_block(
      list(.legacy(fit, exponentiate = TRUE)))
  )
})

test_that("exponentiate: mixed logit + poisson with exponentiate", {
  f1 <- .fixture_glm_logistic()
  f2 <- .fixture_glm_poisson()
  expect_identical(
    build_exponentiate_footer_block_from_frames(list(
      .frame_of(f1, exponentiate = TRUE, model_id = "M1"),
      .frame_of(f2, exponentiate = TRUE, model_id = "M2"))),
    build_exponentiate_footer_block(list(
      .legacy(f1, exponentiate = TRUE, model_id = "M1"),
      .legacy(f2, exponentiate = TRUE, model_id = "M2")))
  )
})

test_that("exponentiate: empty / non-list returns NULL via both paths", {
  expect_null(build_exponentiate_footer_block_from_frames(list()))
  expect_null(build_exponentiate_footer_block_from_frames("nope"))
})


# ---- Per-field round-trip checks ------------------------------------------

test_that("exp_applied / exp_header round-trip through frame for glm logistic", {
  fit <- .fixture_glm_logistic()
  fr  <- .frame_of(fit, exponentiate = TRUE)
  leg <- .legacy(fit, exponentiate = TRUE)
  expect_identical(isTRUE(fr$info$extras$exp_applied),
                   isTRUE(leg$exp_applied))
  expect_identical(fr$info$extras$exp_header, leg$exp_header)
})

test_that("use_ame_satterthwaite round-trips", {
  fit <- .fixture_lm()
  fr  <- as_regression_frame(fit, use_ame_satterthwaite = TRUE)
  leg <- extract_lm_phase1(fit, model_id = "M1", use_ame_satterthwaite = TRUE)
  expect_identical(isTRUE(fr$info$extras$use_ame_satterthwaite),
                   isTRUE(leg$use_ame_satterthwaite))
})
