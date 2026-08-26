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
  nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )
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
  expect_match(out, "LR test", fixed = TRUE)
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
  expect_match(out, "LR test", fixed = TRUE)
})

test_that("random effects footer fires for nlme::lme fits", {
  fit <- .fit_lme_re()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_match(out, "Random effects (REML)", fixed = TRUE)
  expect_match(out, "LR test", fixed = TRUE)
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
  fit <- lme4::lmer(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy,
    REML = FALSE
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_match(out, "Random effects (ML):", fixed = TRUE)
})

test_that("random effects footer annotates glmer with '(ML)' (REML undefined for GLMM)", {
  skip_if_not_installed("lme4")
  d <- mtcars
  d$cyl <- factor(d$cyl)
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
  fit <- nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ 1 | Subject,
    method = "ML"
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_match(out, "Random effects (ML):", fixed = TRUE)
})

test_that("random effects footer annotates glmmTMB (default ML) with '(ML)'", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(list(fr))
  expect_match(out, "Random effects (ML):", fixed = TRUE)
})

test_that("random effects footer annotates glmmTMB (REML=TRUE) with '(REML)'", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy,
    REML = TRUE
  )
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
  expect_match(combined, "Random effects", fixed = TRUE)
  expect_match(combined, "σ Subject (Intercept)", fixed = TRUE)
  expect_match(combined, "N (Subject)", fixed = TRUE)
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
  fit_lme <- .fit_lme_re()
  fr_lme <- as_regression_frame(fit_lme, model_id = "M2")
  out <- spicy:::build_random_effects_footer_block_from_frames(
    list(fr_lmer, fr_lme)
  )
  expect_match(out, "Model 1:", fixed = TRUE)
  expect_match(out, "Model 2:", fixed = TRUE)
})


# ---- 5. The absent ICC row says why it is absent -------------------------
#
# `.merMod_icc()` returns NA for several reasons and the row is simply
# dropped. One of those reasons is a property of the design the reader
# CHOSE -- more than one grouping factor, which defines one ICC per level
# nested and none uniquely when crossed -- and a reader comparing their
# two-factor table against a one-factor example finds a line missing with
# nothing said. The row stays absent; the footer states the reason.

.fit_lmer_crossed <- function() {
  skip_if_not_installed("lme4")
  skip_if_not_installed("nlme")
  suppressWarnings(lme4::lmer(
    distance ~ age + (1 | Subject) + (1 | Sex),
    data = nlme::Orthodont
  ))
}

.fit_lme_nested_icc <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(pixel ~ day, data = nlme::Pixel, random = ~ 1 | Dog / Side)
}

test_that("a crossed lmer says why it reports no ICC", {
  fit <- .fit_lmer_crossed()
  fr <- suppressWarnings(as_regression_frame(fit))
  expect_true(is.na(fr$info$random_effects$icc))
  expect_identical(fr$info$random_effects$icc_omitted, "multi_group")

  out <- paste(
    capture.output(print(suppressWarnings(table_regression(fit)))),
    collapse = "\n"
  )
  # The row is not there ...
  expect_false(grepl("\n ICC ", out, fixed = TRUE))
  # ... and the note says why, in the registry's words.
  expect_match(out, spicy:::spicy_str("note_icc_multi_group"), fixed = TRUE)
})

test_that("a nested lme says why it reports no ICC", {
  fit <- .fit_lme_nested_icc()
  fr <- as_regression_frame(fit)
  expect_true(is.na(fr$info$random_effects$icc))
  expect_identical(fr$info$random_effects$icc_omitted, "multi_group")

  out <- paste(
    capture.output(print(table_regression(fit))),
    collapse = "\n"
  )
  expect_false(grepl("\n ICC ", out, fixed = TRUE))
  expect_match(out, spicy:::spicy_str("note_icc_multi_group"), fixed = TRUE)
})

test_that("a single-factor fit keeps its ICC and stays silent", {
  fit <- .fit_lmer_re()
  fr <- as_regression_frame(fit)
  expect_false(is.na(fr$info$random_effects$icc))
  expect_true(is.na(fr$info$random_effects$icc_omitted))

  out <- paste(capture.output(print(table_regression(fit))), collapse = "\n")
  expect_match(out, "ICC", fixed = TRUE)
  expect_false(grepl(
    spicy:::spicy_str("note_icc_multi_group"),
    out,
    fixed = TRUE
  ))
})

test_that("the note does not claim an omission it cannot explain", {
  # One grouping factor, a random SLOPE: no ICC either, but for a
  # reason this note says nothing about. Silence, not a wrong sentence.
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  fr <- as_regression_frame(fit)
  expect_true(is.na(fr$info$random_effects$icc))
  expect_true(is.na(fr$info$random_effects$icc_omitted))

  out <- paste(capture.output(print(table_regression(fit))), collapse = "\n")
  expect_false(grepl(
    spicy:::spicy_str("note_icc_multi_group"),
    out,
    fixed = TRUE
  ))
})

test_that("the note is gated on the fit-statistic actually being asked for", {
  fit <- .fit_lmer_crossed()
  out <- paste(
    capture.output(print(suppressWarnings(table_regression(
      fit,
      show_fit_stats = c("nobs", "aic")
    )))),
    collapse = "\n"
  )
  expect_false(grepl(
    spicy:::spicy_str("note_icc_multi_group"),
    out,
    fixed = TRUE
  ))
  # And the builder is the thing that refuses, not the caller.
  fr <- suppressWarnings(as_regression_frame(fit))
  expect_null(spicy:::build_icc_omitted_footer_block_from_frames(
    list(fr),
    show_fit_stats = character(0)
  ))
  expect_identical(
    spicy:::build_icc_omitted_footer_block_from_frames(list(fr), "icc"),
    spicy:::spicy_str("note_icc_multi_group")
  )
})

test_that("a mixed list of models attributes the note to the model", {
  fr_one <- as_regression_frame(.fit_lmer_re(), model_id = "M1")
  fr_two <- suppressWarnings(
    as_regression_frame(.fit_lmer_crossed(), model_id = "M2")
  )
  out <- spicy:::build_icc_omitted_footer_block_from_frames(
    list(fr_one, fr_two),
    show_fit_stats = "icc"
  )
  expect_match(out, "Model 2:", fixed = TRUE)
  expect_false(grepl("Model 1:", out, fixed = TRUE))
})

test_that("a non-mixed frame carries the empty icc_omitted contract", {
  fr <- as_regression_frame(.fit_lm_no_re())
  expect_true(is.na(fr$info$random_effects$icc_omitted))
  expect_null(spicy:::build_icc_omitted_footer_block_from_frames(
    list(fr),
    show_fit_stats = "icc"
  ))
})


# ---- An unknown reason token fails loudly, it does not print "NA" ------

test_that("a reason with no sentence is refused, not rendered as NA", {
  # The switch() default used to be NA_character_, and a footer builder
  # that returns NA does not stay silent: with one model the block
  # itself became NA, and with two the composed line read
  # "Model 1: NA". Nothing produces such a token today --
  # .merMod_icc_omitted_reason() emits "multi_group" or NA, and the NA
  # half is filtered out upstream -- so this is a trap set for the next
  # reason that gets added without its sentence.
  fake <- function(reason) {
    list(
      info = list(
        class = "lmerMod",
        random_effects = list(icc_omitted = reason)
      )
    )
  }
  err <- tryCatch(
    spicy:::build_icc_omitted_footer_block_from_frames(
      list(fake("a_reason_with_no_sentence")),
      show_fit_stats = "icc"
    ),
    error = identity
  )
  expect_s3_class(err, "spicy_internal_invariant")
  expect_match(conditionMessage(err), "a_reason_with_no_sentence")
  # The per-model composition path refuses too, not just the shared one.
  expect_error(
    spicy:::build_icc_omitted_footer_block_from_frames(
      list(fake("a_reason_with_no_sentence"), fake("multi_group")),
      show_fit_stats = "icc"
    ),
    class = "spicy_internal_invariant"
  )
  # And the known token still renders, so the guard costs nothing.
  expect_identical(
    spicy:::build_icc_omitted_footer_block_from_frames(
      list(fake("multi_group")),
      show_fit_stats = "icc"
    ),
    spicy:::spicy_str("note_icc_multi_group")
  )
})


# ---- The two mixed engines answer differently, and that is pinned ------

test_that("glmer explains a multi-factor non-Gaussian ICC, glmmTMB stays silent", {
  # Register n. 252, finding F4: a KNOWN asymmetry, pinned rather than
  # harmonised. glmer's ICC kernel runs on non-Gaussian families (the
  # Nakagawa link-scale distribution variance), so on two grouping
  # factors it stops on the multi-factor gate and can name it. glmmTMB
  # runs no kernel at all off Gaussian-identity, so "several grouping
  # factors" would not be the true reason and it says nothing.
  # Harmonising means giving glmmTMB the non-Gaussian ICC, not muting
  # lme4.
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  d <- lme4::cbpp
  d$herd2 <- factor(rep(letters[1:3], length.out = nrow(d)))
  f <- cbind(incidence, size - incidence) ~ period + (1 | herd) + (1 | herd2)

  fr_glmer <- suppressWarnings(as_regression_frame(
    lme4::glmer(f, data = d, family = binomial)
  ))
  expect_true(is.na(fr_glmer$info$random_effects$icc))
  expect_identical(fr_glmer$info$random_effects$icc_omitted, "multi_group")

  fr_tmb <- suppressWarnings(as_regression_frame(
    glmmTMB::glmmTMB(f, data = d, family = binomial)
  ))
  expect_true(is.na(fr_tmb$info$random_effects$icc))
  expect_true(is.na(fr_tmb$info$random_effects$icc_omitted))

  # The consequence for the reader, stated as such: a sentence on one
  # engine, silence on the other, for the same random structure.
  expect_identical(
    spicy:::build_icc_omitted_footer_block_from_frames(
      list(fr_glmer),
      show_fit_stats = "icc"
    ),
    spicy:::spicy_str("note_icc_multi_group")
  )
  expect_null(spicy:::build_icc_omitted_footer_block_from_frames(
    list(fr_tmb),
    show_fit_stats = "icc"
  ))
})
