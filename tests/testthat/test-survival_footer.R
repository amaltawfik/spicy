# ---------------------------------------------------------------------------
# Phase 7c2 tests: survival-specific footer block.
# Covers coxph / cph / survreg / flexsurvreg distinct content lines.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_coxph_sf <- function() {
  skip_if_not_installed("survival")
  survival::coxph(survival::Surv(time, status) ~ age + sex,
                  data = survival::lung)
}

.fit_cph_sf <- function() {
  skip_if_not_installed("survival")
  skip_if_not_installed("rms")
  rms::cph(survival::Surv(time, status) ~ age + sex, data = survival::lung)
}

.fit_survreg_sf <- function() {
  skip_if_not_installed("survival")
  survival::survreg(survival::Surv(time, status) ~ age + sex,
                    data = survival::lung, dist = "weibull")
}

.fit_flexsurv_sf <- function() {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")
  flexsurv::flexsurvreg(survival::Surv(time, status) ~ age + sex,
                        data = survival::lung, dist = "weibull")
}


# ---- 1. coxph: events + concordance ---------------------------------------

test_that("survival footer fires for coxph with concordance only", {
  # n / events moved to fit-stat ROWS (n_events token, 2026-07-09);
  # the footer keeps what has no row: the concordance.
  fit <- .fit_coxph_sf()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_survival_footer_block_from_frames(list(fr))
  expect_match(out, "^Concordance C =")
  expect_false(grepl("Events:", out, fixed = TRUE))
})

test_that("table_regression() coxph: events as fit-stat row, C in footer", {
  fit <- .fit_coxph_sf()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  # Field convention (EpiRHandbook survival ch.; Stata stcox): n AND
  # number-of-events rows.
  expect_match(grep("^ n ", out, value = TRUE), "228")
  expect_match(grep("^ N events ", out, value = TRUE), "165")
  expect_match(combined, "Concordance C", fixed = TRUE)
  expect_false(grepl("Events: 165 of 228", combined, fixed = TRUE))
})


# ---- 2. cph: events surface as the n_events fit-stat --------------------

test_that("rms::cph: events move to the n_events fit-stat row", {
  # cph exposes no concordance struct, so with the Events prose gone
  # its survival footer contribution is NULL; the information lives in
  # the N events fit-stat row (fit$stats["Events"]).
  fit <- .fit_cph_sf()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_survival_footer_block_from_frames(list(fr))
  expect_null(out)
  expect_identical(fr$info$fit_stats$n_events, 165L)
  df <- as.data.frame(table_regression(fit))
  ev <- df[trimws(df$Variable) == "N events", -1L, drop = FALSE]
  expect_true(any(grepl("165", unlist(ev))))
})


# ---- 3. survreg: distribution + scale ------------------------------------

test_that("survival footer fires for survreg with distribution + scale", {
  fit <- .fit_survreg_sf()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_survival_footer_block_from_frames(list(fr))
  expect_match(out, "Distribution: Weibull", fixed = TRUE)
  expect_match(out, "scale =", fixed = TRUE)
})


# ---- 4. flexsurv: distribution + aux parameters --------------------------

test_that("survival footer fires for flexsurvreg with shape + scale", {
  fit <- .fit_flexsurv_sf()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_survival_footer_block_from_frames(list(fr))
  expect_match(out, "Distribution: Weibull", fixed = TRUE)
  expect_match(out, "shape =", fixed = TRUE)
  expect_match(out, "scale =", fixed = TRUE)
})


# ---- 5. Non-survival classes are silent ----------------------------------

test_that("survival footer is NULL for lm fits", {
  fr <- as_regression_frame(lm(mpg ~ wt, data = mtcars), model_id = "M1")
  out <- spicy:::build_survival_footer_block_from_frames(list(fr))
  expect_null(out)
})

test_that("survival footer is NULL for an empty frames list", {
  expect_null(spicy:::build_survival_footer_block_from_frames(list()))
})


# ---- 6. Multi-model with mixed survival/non-survival --------------------

test_that("survival footer skips non-survival models in mixed lists", {
  fr_cox <- as_regression_frame(.fit_coxph_sf(), model_id = "M1")
  fr_lm  <- as_regression_frame(lm(mpg ~ wt, data = mtcars), model_id = "M2")
  # Only the coxph entry contributes; the function returns the cox text
  # without a "Model 1:" prefix (no multi-model context with content).
  out <- spicy:::build_survival_footer_block_from_frames(list(fr_cox, fr_lm))
  expect_match(out, "^Concordance C =", fixed = FALSE)
})


# ---- 7. .surv_title_dist normalisation ----------------------------------

test_that(".surv_title_dist normalises lnorm -> 'Log-normal'", {
  expect_identical(spicy:::.surv_title_dist("lnorm"), "Log-normal")
  expect_identical(spicy:::.surv_title_dist("lognormal"), "Log-normal")
  expect_identical(spicy:::.surv_title_dist("weibull"), "Weibull")
  expect_identical(spicy:::.surv_title_dist("gengamma"), "Generalised gamma")
})
