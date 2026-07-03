# ---------------------------------------------------------------------------
# Phase 7c8a tests: fixed-effect inference (p-values + CI) for mixed-effects
# fits. Two-part assertion:
#   (1) Behavioural -- coefs carry the right test_type / df / p_value per class.
#   (2) Footer -- the annotation line names the inference method.
# ---------------------------------------------------------------------------


# ---- 1. lmer fallback: Wald-z when lmerTest is NOT loaded --------------

test_that("lmer (no lmerTest) -> Wald-z with df = Inf, p from pnorm", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(all(fr$coefs$test_type == "z"))
  expect_true(all(!is.finite(fr$coefs$df)))
  # p-value should be 2 * pnorm(-|t|) with t = est / SE.
  for (i in seq_len(nrow(fr$coefs))) {
    stat <- fr$coefs$estimate[i] / fr$coefs$std_error[i]
    expect_equal(fr$coefs$p_value[i],
                 2 * stats::pnorm(-abs(stat)),
                 tolerance = 1e-12)
  }
})

test_that("lmer (no lmerTest) -> CI uses z-quantile (NOT t with naive df)", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  fr <- as_regression_frame(fit, model_id = "M1")
  z_crit <- stats::qnorm(0.975)
  for (i in seq_len(nrow(fr$coefs))) {
    expect_equal(fr$coefs$ci_lower[i],
                 fr$coefs$estimate[i] - z_crit * fr$coefs$std_error[i],
                 tolerance = 1e-12)
    expect_equal(fr$coefs$ci_upper[i],
                 fr$coefs$estimate[i] + z_crit * fr$coefs$std_error[i],
                 tolerance = 1e-12)
  }
})


# ---- 2. lmerTest path stays Satterthwaite (regression guard) ------------

test_that("lmerTest::lmer -> Satterthwaite t-test, finite df", {
  skip_if_not_installed("lmerTest")
  fit <- lmerTest::lmer(Reaction ~ Days + (1 | Subject),
                         data = lme4::sleepstudy)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(all(fr$coefs$test_type == "t"))
  expect_true(all(is.finite(fr$coefs$df)))
  # Satterthwaite df should be finite and < n (180).
  expect_true(all(fr$coefs$df < stats::nobs(fit)))
})


# ---- 3. Footer annotation: lmerModLmerTest -> Satterthwaite line --------

test_that("footer annotates lmerModLmerTest fits with Satterthwaite line", {
  skip_if_not_installed("lmerTest")
  fit <- lmerTest::lmer(Reaction ~ Days + (1 | Subject),
                         data = lme4::sleepstudy)
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_mixed_inference_footer_block_from_frames(list(fr))
  expect_match(out, "Satterthwaite", fixed = TRUE)
  expect_match(out, "(lmerTest)",    fixed = TRUE)
})


# ---- 4. Footer annotation: lmerMod (no lmerTest) -> Wald-z line --------

test_that("footer annotates lmerMod (no lmerTest) with Wald-z recommendation", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_mixed_inference_footer_block_from_frames(list(fr))
  expect_match(out, "Wald-z",   fixed = TRUE)
  expect_match(out, "lmerTest", fixed = TRUE)  # recommendation
})


# ---- 5. Footer annotation per class -------------------------------------

test_that("footer annotates glmer with Wald-z (lme4)", {
  skip_if_not_installed("lme4")
  d <- mtcars; d$cyl <- factor(d$cyl)
  suppressMessages(suppressWarnings(
    fit <- lme4::glmer(am ~ mpg + (1 | cyl), data = d, family = binomial)
  ))
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_mixed_inference_footer_block_from_frames(list(fr))
  expect_match(out, "Wald-z", fixed = TRUE)
  expect_match(out, "(lme4)", fixed = TRUE)
})

test_that("footer annotates glmmTMB with Wald-z (glmmTMB)", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject),
                           data = lme4::sleepstudy)
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_mixed_inference_footer_block_from_frames(list(fr))
  expect_match(out, "Wald-z",    fixed = TRUE)
  expect_match(out, "(glmmTMB)", fixed = TRUE)
})

test_that("footer annotates lme with containment df (nlme)", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(distance ~ age + Sex, data = nlme::Orthodont,
                    random = ~ 1 | Subject)
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_mixed_inference_footer_block_from_frames(list(fr))
  expect_match(out, "containment", fixed = TRUE)
  expect_match(out, "(nlme)",      fixed = TRUE)
})


# ---- 6. Footer is silent for non-mixed classes --------------------------

test_that("footer returns NULL for lm fits (non-mixed)", {
  fr <- as_regression_frame(lm(mpg ~ wt, data = mtcars), model_id = "M1")
  out <- spicy:::build_mixed_inference_footer_block_from_frames(list(fr))
  expect_null(out)
})

test_that("footer returns NULL for glm fits (non-mixed)", {
  fr <- as_regression_frame(
    glm(am ~ mpg, data = mtcars, family = binomial),
    model_id = "M1"
  )
  out <- spicy:::build_mixed_inference_footer_block_from_frames(list(fr))
  expect_null(out)
})

test_that("footer returns NULL for an empty frame list", {
  expect_null(spicy:::build_mixed_inference_footer_block_from_frames(list()))
})


# ---- 7. Multi-model: per-model prefix -----------------------------------

test_that("footer consolidates identical per-model lines (no 'Model k:' prefix)", {
  # When every model in the list produces the SAME annotation, the
  # builder collapses to a single line -- the per-model "Model k:"
  # prefix would just be three identical copies of the same sentence
  # and is purely noise.
  skip_if_not_installed("lme4")
  fit1 <- lme4::lmer(Reaction ~ 1    + (1 | Subject),
                      data = lme4::sleepstudy)
  fit2 <- lme4::lmer(Reaction ~ Days + (1 | Subject),
                      data = lme4::sleepstudy)
  fr1 <- as_regression_frame(fit1, model_id = "M1")
  fr2 <- as_regression_frame(fit2, model_id = "M2")
  out <- spicy:::build_mixed_inference_footer_block_from_frames(list(fr1, fr2))
  expect_false(grepl("Model 1:", out, fixed = TRUE))
  expect_false(grepl("Model 2:", out, fixed = TRUE))
  expect_match(out, "p-values:", fixed = TRUE)
  expect_match(out, "Wald-z",    fixed = TRUE)
})

test_that("footer prefixes per model in multi-model lists", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("nlme")
  fit_lmer <- lme4::lmer(Reaction ~ Days + (1 | Subject),
                          data = lme4::sleepstudy)
  fit_lme  <- nlme::lme(distance ~ age + Sex, data = nlme::Orthodont,
                         random = ~ 1 | Subject)
  fr1 <- as_regression_frame(fit_lmer, model_id = "M1")
  fr2 <- as_regression_frame(fit_lme,  model_id = "M2")
  out <- spicy:::build_mixed_inference_footer_block_from_frames(list(fr1, fr2))
  expect_match(out, "Model 1:", fixed = TRUE)
  expect_match(out, "Model 2:", fixed = TRUE)
  expect_match(out, "Wald-z",   fixed = TRUE)  # lmer fallback
  expect_match(out, "containment", fixed = TRUE)  # lme
})


# ---- 8. End-to-end: line appears in table_regression() output -----------

test_that("table_regression() footer surfaces the inference annotation", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "p-values:", fixed = TRUE)
  expect_match(combined, "Wald-z",    fixed = TRUE)
})

test_that("orchestrator path keeps the Satterthwaite footer (default ci_method)", {
  skip_if_not_installed("lmerTest")
  # table_regression() always passes its match.arg default "wald" to the
  # frames; that request must not override the Satterthwaite regime the
  # rows actually carry (the footer used to say "Wald-z ... Load
  # lmerTest" over Satterthwaite-t rows).
  fit <- lmerTest::lmer(Reaction ~ Days + (Days | Subject),
                        data = lme4::sleepstudy)
  out <- table_regression(fit)
  note <- paste(attr(out, "note"), collapse = "\n")
  expect_match(note, "Satterthwaite t-test (lmerTest)", fixed = TRUE)
  expect_false(grepl("Load `lmerTest`", note, fixed = TRUE))
})

test_that("CR* on mixed fits attributes the Satterthwaite df to clubSandwich", {
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("clubSandwich")
  fit <- lmerTest::lmer(Reaction ~ Days + (1 | Subject),
                        data = lme4::sleepstudy)
  out <- table_regression(fit, vcov = "CR2", cluster = ~Subject)
  note <- paste(attr(out, "note"), collapse = "\n")
  expect_match(note, "cluster-robust df (clubSandwich)", fixed = TRUE)
  expect_false(grepl("Satterthwaite t-test (lmerTest)", note, fixed = TRUE))
})
