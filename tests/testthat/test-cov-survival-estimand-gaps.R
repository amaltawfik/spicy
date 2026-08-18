# ---------------------------------------------------------------------------
# Coverage closure for the survival-estimand gates and their orchestrator
# arms (companions of test-survival_estimands*.R).
#
# R/regression_survival_estimands.R
#   * .survreg_estimand_gates(): the "needs a named survreg distribution"
#     refusal (lines 375-382), reached with a survreg fit whose `dist` is
#     a distribution LIST -- documented survreg usage, so the arm is live
#     user-facing behaviour, not dead code.
#   * .survreg_estimand_points(): the NULL-rows fallback `data.frame()`
#     (line 478) when no predictor yields a contrast row.
#   * .survival_estimand_rows(): the tau = "minmax" fallback to
#     max(observed time) when no factor / character predictor exists
#     (line 636).
#   * build_survival_estimand_footer_block_from_frames(): the "notes
#     differ across models" arm that prefixes each note with its model
#     reference (lines 838-848).
#
# R/table_regression.R
#   * the multi-model tag in the estimand-capability refusal (line 2488).
#   * the Cox-specific hint in the AME-capability refusal (lines
#     2768-2772).
#   * the "95% HDI" header label under all-posterior_hdi frames
#     (line 3088).
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.gap_lung <- function() {
  d <- survival::lung[, c("time", "status", "age", "sex")]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d$sexf <- factor(d$sex, levels = c(1, 2), labels = c("male", "female"))
  d$old <- d$age > 63
  d
}

# One frame-shaped list carrying only what the estimand footer reads:
# info$model_label (for the per-model reference) and
# info$extras$survival_estimands.
.gap_estimand_frame <- function(
  tau = NULL,
  at_time = NULL,
  boot_n = 500L,
  boot_valid = 500L,
  skipped_terms = character(0),
  model_label = NULL
) {
  list(
    info = list(
      model_label = model_label,
      extras = list(
        survival_estimands = list(
          tau = tau,
          at_time = at_time,
          boot_n = boot_n,
          boot_valid = boot_valid,
          stratified = FALSE,
          skipped_terms = skipped_terms
        )
      )
    )
  )
}


# ---- 1. survreg gate: the distribution must be a NAME --------------------

# survival::survreg() accepts `dist` either as a character name or as a
# distribution LIST (survreg.distributions$weibull). Only the name form
# can be handed to psurvreg() by the g-computation engine, so the list
# form must be refused. The refusal is therefore reachable from ordinary
# user code -- see the note on the misplaced `# nocov` in the report.

test_that(".survreg_estimand_gates refuses a list-valued survreg distribution", {
  skip_if_not_installed("survival")
  d <- .gap_lung()
  fit <- survival::survreg(
    survival::Surv(time, status) ~ age,
    data = d,
    dist = survival::survreg.distributions$weibull
  )
  # Precondition: this is exactly the shape the gate screens for.
  expect_false(is.character(fit$dist))
  expect_error(
    spicy:::.survreg_estimand_gates(fit, "M1"),
    "RMST / risk-difference columns need a named survreg distribution (M1).",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
})

test_that("table_regression() surfaces the survreg distribution-name refusal", {
  skip_if_not_installed("survival")
  d <- .gap_lung()
  fit <- survival::survreg(
    survival::Surv(time, status) ~ age,
    data = d,
    dist = survival::survreg.distributions$weibull
  )
  expect_error(
    table_regression(
      fit,
      show_columns = c("b", "rmst"),
      tau = 500,
      boot_n = 5
    ),
    "need a named survreg distribution",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
  # The scale / strata gate is a different refusal: it must not be the
  # one that fired here.
  err <- tryCatch(
    spicy:::.survreg_estimand_gates(fit, "M1"),
    error = function(e) e
  )
  expect_false(grepl("stratified survreg fit", conditionMessage(err)))
})


# ---- 2. survreg points: no contrastable predictor -> zero rows -----------

# A logical predictor enters the model matrix as a numeric contrast but
# .estimand_contrast_rows() can neither level-contrast nor +1 it, so the
# row list stays empty and rbind() returns NULL: the caller substitutes an
# empty data.frame() so the "skipped_terms" attribute still has a carrier.

test_that(".survreg_estimand_points returns an empty frame when no contrast row exists", {
  skip_if_not_installed("survival")
  d <- .gap_lung()
  fit <- survival::survreg(
    survival::Surv(time, status) ~ old,
    data = d,
    dist = "weibull"
  )
  pts <- spicy:::.survreg_estimand_points(
    fit,
    spicy:::.survreg_estimand_data(fit),
    want_rmst = TRUE,
    want_risk = FALSE,
    tau = 500,
    at_time = NULL
  )
  expect_true(is.data.frame(pts))
  expect_identical(nrow(pts), 0L)
  expect_identical(ncol(pts), 0L)
  # `old` is a bare term label, so nothing was skipped as transformed:
  # the empty result comes from the column TYPE, not from the formula.
  expect_identical(attr(pts, "skipped_terms"), character(0))
})


# ---- 3. tau = "minmax" with no factor predictor --------------------------

# "minmax" is the smallest per-level maximum observed time across factor
# predictors. An all-continuous model has no levels to minimise over, so
# tau falls back to the largest observed time.

test_that("tau = 'minmax' falls back to max(observed time) without a factor predictor", {
  skip_if_not_installed("survival")
  d <- .gap_lung()
  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = d)
  set.seed(1)
  res <- spicy:::.coxph_estimand_rows(
    fit,
    model_id = "M1",
    outcome = NULL,
    show_columns = c("b", "rmst"),
    tau = "minmax",
    at_time = NULL,
    ci_level = 0.95,
    boot_n = 4L
  )
  expect_identical(res$tau, max(d$time))
  expect_identical(res$rows$term, "age")
  expect_identical(unique(res$rows$estimate_type), "rmst")
  # The fallback is the OVERALL maximum, strictly larger than the
  # per-group minmax the same data would give with a factor predictor.
  fit_f <- survival::coxph(survival::Surv(time, status) ~ sexf, data = d)
  set.seed(1)
  res_f <- spicy:::.coxph_estimand_rows(
    fit_f,
    model_id = "M1",
    outcome = NULL,
    show_columns = c("b", "rmst"),
    tau = "minmax",
    at_time = NULL,
    ci_level = 0.95,
    boot_n = 4L
  )
  expect_identical(res_f$tau, min(tapply(d$time, d$sexf, max)))
  expect_lt(res_f$tau, res$tau)
})


# ---- 4. Estimand footer: per-model lines when the notes differ -----------

test_that("the estimand footer prefixes each model when the notes differ", {
  frames <- list(
    .gap_estimand_frame(tau = 365),
    # A frame with no estimand extras contributes nothing and must not
    # shift the numbering of the frames after it.
    list(info = list(extras = list())),
    .gap_estimand_frame(tau = 500)
  )
  out <- spicy:::build_survival_estimand_footer_block_from_frames(frames)
  lines <- strsplit(out, "\n", fixed = TRUE)[[1L]]
  expect_identical(length(lines), 2L)
  expect_match(lines[1L], "^Model 1: dRMST = difference in restricted mean")
  expect_match(lines[1L], "over [0, 365]", fixed = TRUE)
  # Model 3, not Model 2: the index is the frame's own position.
  expect_match(lines[2L], "^Model 3: dRMST = difference in restricted mean")
  expect_match(lines[2L], "over [0, 500]", fixed = TRUE)
  expect_false(grepl("Model 2", out, fixed = TRUE))
})

test_that("identical estimand notes collapse to a single unprefixed line", {
  # Contrast case for the branch above: same tau on both models, so the
  # aggregator returns the shared note with no model reference at all.
  frames <- list(
    .gap_estimand_frame(tau = 365),
    .gap_estimand_frame(tau = 365)
  )
  out <- spicy:::build_survival_estimand_footer_block_from_frames(frames)
  expect_identical(length(strsplit(out, "\n", fixed = TRUE)[[1L]]), 1L)
  expect_match(out, "^dRMST = difference in restricted mean")
  expect_false(grepl("Model 1", out, fixed = TRUE))
})

test_that("differing estimand notes cite user model labels when present", {
  # Same "notes differ" arm, resolved through info$model_label rather
  # than the default "Model <k>" reference.
  frames <- list(
    .gap_estimand_frame(tau = 365, model_label = "Crude"),
    .gap_estimand_frame(
      tau = 365,
      at_time = 200,
      boot_valid = 480L,
      model_label = "Adjusted"
    )
  )
  out <- spicy:::build_survival_estimand_footer_block_from_frames(frames)
  lines <- strsplit(out, "\n", fixed = TRUE)[[1L]]
  expect_identical(length(lines), 2L)
  expect_match(lines[1L], "^Crude: dRMST = difference")
  expect_match(lines[2L], "^Adjusted: dRMST = difference")
  expect_match(
    lines[2L],
    "dRisk = difference in cumulative incidence at 200",
    fixed = TRUE
  )
  # A degraded bootstrap reports the valid range, not the nominal count.
  expect_match(lines[2L], "(480-500 replicates)", fixed = TRUE)
})


# ---- 5. Estimand-capability refusal names the model position ------------

test_that("a multi-model estimand request names the offending model and class", {
  d <- .gap_lung()
  m1 <- stats::lm(time ~ age, data = d)
  m2 <- stats::lm(time ~ sexf, data = d)
  expect_error(
    table_regression(list(m1, m2), show_columns = c("b", "rmst"), tau = 365),
    "RMST / risk-difference columns are not available for model 1 (class lm).",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
})

test_that("the same refusal on a single model names the class only", {
  # Contrast case: n_models == 1 takes the "this model class" tag, so
  # the multi-model tag above is genuinely the multi-model arm.
  d <- .gap_lung()
  expect_error(
    table_regression(
      stats::lm(time ~ age, data = d),
      show_columns = c("b", "rmst"),
      tau = 365
    ),
    "RMST / risk-difference columns are not available for this model class (lm).",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
})

test_that("a Cox model later in a mixed list is still named by position", {
  skip_if_not_installed("survival")
  d <- .gap_lung()
  cx <- survival::coxph(survival::Surv(time, status) ~ age, data = d)
  expect_error(
    table_regression(
      list(cx, stats::lm(time ~ age, data = d)),
      show_columns = c("b", "rmst"),
      tau = 365,
      boot_n = 4
    ),
    "not available for model 2 (class lm)",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
})


# ---- 6. AME refusal: the Cox-specific hint ------------------------------

# validate_class_appropriate_tokens() refuses AME upfront only when ALL
# models are Cox. A mixed set of AME-incapable classes (coxph + nls)
# passes that gate and reaches the frame-level capability guard, whose
# hint points a Cox reader at the absolute-effect estimands instead.

.gap_nls <- function() {
  dn <- data.frame(x = seq(1, 5, length.out = 40))
  dn$y <- 2 * exp(0.3 * dn$x) + rep(c(-0.05, 0.05), length.out = 40)
  stats::nls(y ~ a * exp(b * x), data = dn, start = list(a = 2, b = 0.3))
}

test_that("AME refusal on a coxph + nls table points at the RMST columns", {
  skip_if_not_installed("survival")
  d <- .gap_lung()
  cx <- survival::coxph(survival::Surv(time, status) ~ age, data = d)
  err <- tryCatch(
    table_regression(list(cx, .gap_nls()), show_columns = c("b", "ame")),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "AME columns are not available for `coxph` / `nls`.",
    fixed = TRUE
  )
  expect_match(
    msg,
    "For a Cox model, the absolute-effect columns are the RMST and risk differences:",
    fixed = TRUE
  )
  expect_match(
    msg,
    "`show_columns = c(\"b\", \"rmst\")` with `tau = `",
    fixed = TRUE
  )
  expect_match(msg, "Drop the AME token(s) from `show_columns`.", fixed = TRUE)
})

test_that("the same refusal without a Cox model uses the generic hint", {
  # Contrast case: no coxph in the set, so the Cox branch must NOT fire.
  err <- tryCatch(
    table_regression(.gap_nls(), show_columns = c("b", "ame")),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  msg <- conditionMessage(err)
  expect_match(msg, "AME columns are not available for `nls`.", fixed = TRUE)
  expect_match(
    msg,
    "No average-marginal-effects backend exists for this class",
    fixed = TRUE
  )
  expect_false(grepl("For a Cox model", msg, fixed = TRUE))
})


# ---- 7. Header CI label under all-HDI frames ----------------------------

# The label is a pure function of every frame's info$ci_method, and
# "posterior_hdi" is only ever stamped by the Stan frame builder -- whose
# tests need a sampled fit and are skipped on CI. Mocking the frame
# method keeps the label branch itself under test without sampling.

test_that("the CI header reads '95% HDI' when every frame is a posterior HDI", {
  real_lm_frame <- spicy:::as_regression_frame.lm
  testthat::local_mocked_bindings(
    as_regression_frame.lm = function(x, ...) {
      fr <- real_lm_frame(x, ...)
      fr$info$ci_method <- "posterior_hdi"
      fr
    },
    .package = "spicy"
  )
  out <- paste(
    capture.output(print(
      table_regression(
        stats::lm(mpg ~ wt, data = mtcars),
        show_columns = c("b", "ci")
      )
    )),
    collapse = "\n"
  )
  expect_match(out, "95% HDI", fixed = TRUE)
  expect_false(grepl("95% CI", out, fixed = TRUE))
  expect_false(grepl("95% CrI", out, fixed = TRUE))
})

test_that("the CI header reads '95% CrI' for equal-tailed posterior frames", {
  # Sibling arm of the branch above: quantile intervals keep "CrI".
  real_lm_frame <- spicy:::as_regression_frame.lm
  testthat::local_mocked_bindings(
    as_regression_frame.lm = function(x, ...) {
      fr <- real_lm_frame(x, ...)
      fr$info$ci_method <- "posterior_quantile"
      fr
    },
    .package = "spicy"
  )
  out <- paste(
    capture.output(print(
      table_regression(
        stats::lm(mpg ~ wt, data = mtcars),
        show_columns = c("b", "ci")
      )
    )),
    collapse = "\n"
  )
  expect_match(out, "95% CrI", fixed = TRUE)
  expect_false(grepl("95% HDI", out, fixed = TRUE))
})
