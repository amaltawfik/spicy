# ---------------------------------------------------------------------------
# Coverage top-up (round 2) for R/regression_frame_ordinal.R.
#
# Targets the branches the earlier suites leave uncovered:
#   * as_regression_frame.clm(): the frame-level refusal of a robust vcov for
#     a partial-proportional-odds fit. table_regression()'s validate gate
#     (.clm_robust_vcov_support) normally intercepts first, so the method's
#     own guard only fires on a direct frame call.
#   * .ordinal_maybe_profile_ci(): (a) the early return when no estimated
#     predictor rows exist, (b) the spicy_fallback path when confint()
#     returns a non-matrix that cannot be matched to the coef rows, and
#     (c) the spicy_fallback path when no coef term overlaps the profile
#     matrix rownames -- Wald CIs must be kept untouched in both fallbacks.
#   * .ordinal_nonprop_rows(): a real `nominal = ~ 1` clm, whose alpha.mat
#     carries only the "(Intercept)" baseline row -> no non-proportional
#     rows are synthesised.
#   * Defensive guards of .polr_thresholds / .clm_thresholds /
#     .append_threshold_rows and the .polr_link_title switch fallback.
#   * .ordinal_pseudo_r2 / .ordinal_null_loglik degenerate-input guards.
# ---------------------------------------------------------------------------


# Small, well-behaved synthetic ordinal data: a 3-level ordered response
# driven by x (z is noise), so polr converges cleanly and profiles fast.
.cov100_ord_data <- function(n = 150) {
  set.seed(11)
  x <- stats::rnorm(n)
  z <- stats::rnorm(n)
  lat <- x + stats::rlogis(n)
  y <- cut(lat, breaks = c(-Inf, -0.5, 0.8, Inf),
           labels = c("lo", "mid", "hi"), ordered_result = TRUE)
  data.frame(y = y, x = x, z = z)
}


# ---- 1. Frame-level PPO robust-vcov refusal -------------------------------

test_that("as_regression_frame.clm refuses a robust vcov for a PPO fit", {
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal", envir = environment())
  fit <- ordinal::clm(rating ~ temp, nominal = ~ contact, data = wine)
  # Direct frame call: bypasses table_regression()'s validate gate, so the
  # method's own PPO-specific guard is the one that fires -- its message
  # names the PPO structure, unlike the generic validate-layer message.
  err <- expect_error(
    as_regression_frame(fit, vcov = "CR2"),
    class = "spicy_unsupported_vcov")
  expect_match(conditionMessage(err), "partial-proportional-odds",
               fixed = TRUE)
  expect_match(conditionMessage(err), "CR2", fixed = TRUE)
  expect_match(conditionMessage(err), "nominal", fixed = TRUE)
  # The same fit under the model-based "classical" vcov stays available.
  fr <- as_regression_frame(fit, vcov = "classical")
  expect_identical(fr$info$class, "clm")
  expect_true(any(fr$coefs$parent_var == "Non-proportional effects"))
})


# ---- 2. Profile CI: no estimated predictor rows ---------------------------

test_that("profile CI request with no estimated predictor rows is a no-op", {
  skip_if_not_installed("MASS")
  d <- .cov100_ord_data()
  # Intercept-only polr: zero predictor coefs -> the profile step has no
  # rows to refine and must return the (empty) coefs frame untouched.
  fit0 <- MASS::polr(y ~ 1, data = d, Hess = TRUE)
  fr <- as_regression_frame(fit0, ci_method = "profile")
  expect_identical(nrow(fr$coefs), 0L)
  expect_identical(fr$info$ci_method, "profile")
  # The (k - 1) = 2 thresholds are still extracted.
  expect_identical(nrow(fr$info$extras$thresholds), 2L)
  expect_identical(fr$info$extras$thresholds$term, c("lo|mid", "mid|hi"))
  # Direct guard: a coefs frame with only a reference row (NA estimate)
  # comes back identical -- the fit is never touched (NULL is safe).
  coefs_ref <- data.frame(term = "gLo", is_ref = TRUE, estimate = NA_real_,
                          ci_lower = NA_real_, ci_upper = NA_real_)
  expect_identical(
    spicy:::.ordinal_maybe_profile_ci(coefs_ref, NULL, 0.95, "profile"),
    coefs_ref)
})


# ---- 3. Profile CI: unmatchable confint() shapes fall back to Wald --------

test_that("profile CI falls back to Wald when confint() shape cannot match", {
  skip_if_not_installed("MASS")
  d <- .cov100_ord_data()
  # Single-predictor polr: confint() returns a named length-2 VECTOR. With
  # TWO estimated coef rows the vector cannot be attributed to either row,
  # so pci collapses to NULL and the spicy_fallback warning fires.
  fit1 <- MASS::polr(y ~ x, data = d, Hess = TRUE)
  pci1 <- suppressMessages(stats::confint(fit1))
  expect_false(is.matrix(pci1))   # precondition: the vector shape
  coefs2 <- data.frame(term = c("a", "b"), is_ref = FALSE,
                       estimate = c(0.1, 0.2),
                       ci_lower = c(-1, -1), ci_upper = c(1, 1))
  expect_warning(
    out <- spicy:::.ordinal_maybe_profile_ci(coefs2, fit1, 0.95, "profile"),
    regexp = "Profile-likelihood CI computation failed",
    class = "spicy_fallback")
  # All-or-nothing: the Wald CIs are kept exactly as passed in.
  expect_identical(out, coefs2)
})

test_that("profile CI falls back to Wald when no term matches confint()", {
  skip_if_not_installed("MASS")
  d <- .cov100_ord_data()
  # Two-predictor polr: confint() returns a term-named 2x2 MATRIX; coef rows
  # whose terms are absent from its rownames leave hit all-FALSE, so the
  # spicy_fallback warning fires and the Wald CIs are kept.
  fit2 <- MASS::polr(y ~ x + z, data = d, Hess = TRUE)
  pci2 <- suppressMessages(stats::confint(fit2))
  expect_true(is.matrix(pci2))    # precondition: the matrix arm, not NULL
  expect_identical(rownames(pci2), c("x", "z"))
  coefs3 <- data.frame(term = c("foo", "bar"), is_ref = FALSE,
                       estimate = c(0.1, 0.2),
                       ci_lower = c(-1, -1), ci_upper = c(1, 1))
  expect_warning(
    out <- spicy:::.ordinal_maybe_profile_ci(coefs3, fit2, 0.95, "profile"),
    regexp = "Profile-likelihood CI computation failed",
    class = "spicy_fallback")
  expect_identical(out, coefs3)
})


# ---- 4. nominal = ~ 1: alpha.mat with only the baseline row ---------------

test_that("clm nominal = ~ 1 synthesises no non-proportional rows", {
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal", envir = environment())
  fit <- ordinal::clm(rating ~ temp, nominal = ~ 1, data = wine)
  # Precondition: alpha.mat exists but its only row is the baseline
  # intercept (there is no actual nominal predictor).
  expect_identical(rownames(fit$alpha.mat), "(Intercept)")
  expect_null(spicy:::.ordinal_nonprop_rows(fit, 0.95))
  fr <- as_regression_frame(fit)
  expect_false(any(fr$coefs$parent_var == "Non-proportional effects"))
  # The location effect is still tabulated with the model's own estimate.
  expect_equal(fr$coefs$estimate[fr$coefs$term == "tempwarm"],
               unname(fit$beta["tempwarm"]), tolerance = 1e-12)
  # And the baseline thresholds are the (k - 1) = 4 bare cut-points.
  expect_identical(fr$info$extras$thresholds$term,
                   c("1|2", "2|3", "3|4", "4|5"))
})


# ---- 5. Threshold-extractor and link-title guards -------------------------

test_that("threshold extractors return an empty frame for empty inputs", {
  # Structurally impossible for a real fit (polr needs >= 2 thresholds,
  # clm >= 1), but the guards define the contract: empty in, empty out.
  expect_identical(spicy:::.polr_thresholds(list(zeta = NULL)), data.frame())
  expect_identical(spicy:::.clm_thresholds(list(alpha = NULL)), data.frame())
})

test_that(".polr_link_title falls back to a generic cumulative title", {
  expect_identical(spicy:::.polr_link_title("aranda"), "Cumulative aranda")
})

test_that(".append_threshold_rows is a no-op without a threshold frame", {
  coefs <- data.frame(term = "x", estimate = 1.5)
  expect_identical(spicy:::.append_threshold_rows(coefs, NULL, 0.95), coefs)
  expect_identical(spicy:::.append_threshold_rows(coefs, data.frame(), 0.95),
                   coefs)
  # A non-data.frame thr (e.g. a bare list) is also passed through.
  expect_identical(
    spicy:::.append_threshold_rows(coefs, list(term = "1|2"), 0.95), coefs)
})


# ---- 6. Pseudo-R2 / null log-likelihood degenerate-input guards -----------

test_that("ordinal pseudo-R2 helpers return NA on degenerate inputs", {
  # A bare list: logLik() and model.frame() both fail -> the NA guards.
  expect_identical(spicy:::.ordinal_pseudo_r2(list()),
                   list(mcfadden = NA_real_, nagelkerke = NA_real_))
  expect_identical(spicy:::.ordinal_null_loglik(list()), NA_real_)
  # A model frame WITHOUT a response (model.frame() on a model frame is the
  # identity, so the frame itself stands in for the fit): y is NULL.
  mf_noresp <- stats::model.frame(~ x, data.frame(x = 1:3))
  expect_null(stats::model.response(mf_noresp))
  expect_identical(spicy:::.ordinal_null_loglik(mf_noresp), NA_real_)
  # All-zero prior weights: every category mass is dropped, the total mass
  # W is 0, and the closed-form null log-likelihood is undefined -> NA.
  d0 <- data.frame(y = factor(c("a", "b", "c")), x = 1:3, w = c(0, 0, 0))
  mf_w0 <- stats::model.frame(y ~ x, data = d0, weights = w)
  expect_true(all(stats::model.weights(mf_w0) == 0))
  expect_identical(spicy:::.ordinal_null_loglik(mf_w0), NA_real_)
})
