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
  y <- cut(
    lat,
    breaks = c(-Inf, -0.5, 0.8, Inf),
    labels = c("lo", "mid", "hi"),
    ordered_result = TRUE
  )
  data.frame(y = y, x = x, z = z)
}


# ---- 1. Frame-level PPO robust-vcov refusal -------------------------------

test_that("as_regression_frame.clm refuses a robust vcov for a PPO fit", {
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal", envir = environment())
  fit <- ordinal::clm(rating ~ temp, nominal = ~contact, data = wine)
  # Direct frame call: bypasses table_regression()'s validate gate, so the
  # method's own PPO-specific guard is the one that fires -- its message
  # names the PPO structure, unlike the generic validate-layer message.
  err <- expect_error(
    as_regression_frame(fit, vcov = "CR2"),
    class = "spicy_unsupported_vcov"
  )
  expect_match(conditionMessage(err), "partial-proportional-odds", fixed = TRUE)
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
  coefs_ref <- data.frame(
    term = "gLo",
    is_ref = TRUE,
    estimate = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_
  )
  expect_identical(
    spicy:::.ordinal_maybe_profile_ci(coefs_ref, NULL, 0.95, "profile"),
    coefs_ref
  )
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
  expect_false(is.matrix(pci1)) # precondition: the vector shape
  coefs2 <- data.frame(
    term = c("a", "b"),
    is_ref = FALSE,
    estimate = c(0.1, 0.2),
    ci_lower = c(-1, -1),
    ci_upper = c(1, 1)
  )
  expect_warning(
    out <- spicy:::.ordinal_maybe_profile_ci(coefs2, fit1, 0.95, "profile"),
    regexp = "Profile-likelihood CI computation failed",
    class = "spicy_fallback"
  )
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
  expect_true(is.matrix(pci2)) # precondition: the matrix arm, not NULL
  expect_identical(rownames(pci2), c("x", "z"))
  coefs3 <- data.frame(
    term = c("foo", "bar"),
    is_ref = FALSE,
    estimate = c(0.1, 0.2),
    ci_lower = c(-1, -1),
    ci_upper = c(1, 1)
  )
  expect_warning(
    out <- spicy:::.ordinal_maybe_profile_ci(coefs3, fit2, 0.95, "profile"),
    regexp = "Profile-likelihood CI computation failed",
    class = "spicy_fallback"
  )
  expect_identical(out, coefs3)
})


# ---- 4. nominal = ~ 1: alpha.mat with only the baseline row ---------------

test_that("clm nominal = ~ 1 synthesises no non-proportional rows", {
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal", envir = environment())
  fit <- ordinal::clm(rating ~ temp, nominal = ~1, data = wine)
  # Precondition: alpha.mat exists but its only row is the baseline
  # intercept (there is no actual nominal predictor).
  expect_identical(rownames(fit$alpha.mat), "(Intercept)")
  expect_null(spicy:::.ordinal_nonprop_rows(fit, 0.95))
  fr <- as_regression_frame(fit)
  expect_false(any(fr$coefs$parent_var == "Non-proportional effects"))
  # The location effect is still tabulated with the model's own estimate.
  expect_equal(
    fr$coefs$estimate[fr$coefs$term == "tempwarm"],
    unname(fit$beta["tempwarm"]),
    tolerance = 1e-12
  )
  # And the baseline thresholds are the (k - 1) = 4 bare cut-points.
  expect_identical(
    fr$info$extras$thresholds$term,
    c("1|2", "2|3", "3|4", "4|5")
  )
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
  expect_identical(
    spicy:::.append_threshold_rows(coefs, data.frame(), 0.95),
    coefs
  )
  # A non-data.frame thr (e.g. a bare list) is also passed through.
  expect_identical(
    spicy:::.append_threshold_rows(coefs, list(term = "1|2"), 0.95),
    coefs
  )
})


# ---- 6. Pseudo-R2 / null log-likelihood degenerate-input guards -----------

test_that("ordinal pseudo-R2 helpers return NA on degenerate inputs", {
  # A bare list: logLik() and model.frame() both fail -> the NA guards.
  expect_identical(
    spicy:::.ordinal_pseudo_r2(list()),
    list(mcfadden = NA_real_, nagelkerke = NA_real_)
  )
  expect_identical(spicy:::.ordinal_null_loglik(list()), NA_real_)
  # A model frame WITHOUT a response (model.frame() on a model frame is the
  # identity, so the frame itself stands in for the fit): y is NULL.
  mf_noresp <- stats::model.frame(~x, data.frame(x = 1:3))
  expect_null(stats::model.response(mf_noresp))
  expect_identical(spicy:::.ordinal_null_loglik(mf_noresp), NA_real_)
  # All-zero prior weights: every category mass is dropped, the total mass
  # W is 0, and the closed-form null log-likelihood is undefined -> NA.
  d0 <- data.frame(y = factor(c("a", "b", "c")), x = 1:3, w = c(0, 0, 0))
  mf_w0 <- stats::model.frame(y ~ x, data = d0, weights = w)
  expect_true(all(stats::model.weights(mf_w0) == 0))
  expect_identical(spicy:::.ordinal_null_loglik(mf_w0), NA_real_)
})


test_that("an aliased clm predictor renders undefined instead of erroring", {
  skip_if_not_installed("ordinal")
  # wine's bottle is confounded with temp: bottle8 is aliased. Its NA
  # coefficient stays in fit$beta but vcov() drops the row, and the
  # frame builder used to die on the out-of-bounds index. The row now
  # carries NA everywhere and renders as the en-dash undefined cell,
  # like an aliased lm / glm coefficient.
  data(wine, package = "ordinal", envir = environment())
  fit <- ordinal::clm(rating ~ temp + bottle, data = wine)
  expect_true(any(fit$aliased$beta))
  df <- table_regression(fit, output = "data.frame")
  row8 <- df[trimws(df$Variable) == "8", , drop = FALSE]
  expect_identical(nrow(row8), 1L)
  expect_identical(trimws(row8$B), "\u2013")
  expect_identical(trimws(row8$SE), "\u2013")
  # The estimable coefficients keep exact values (oracle: summary.clm).
  sm <- summary(fit)$coefficients
  fr <- as_regression_frame(fit)
  b7 <- fr$coefs[fr$coefs$term == "bottle7", ]
  expect_equal(b7$estimate, unname(sm["bottle7", "Estimate"]), tolerance = 1e-9)
  expect_equal(
    b7$std_error,
    unname(sm["bottle7", "Std. Error"]),
    tolerance = 1e-9
  )
})


# ---- An intercept-only ordinal fit renders, end to end -----------------
#
# `y ~ 1` is a legal ordinal model, and not an empty one: coef() holds
# the cut-points, and the cut-points ARE its content. It used to abort
# in public, with an untranslated base error, on four separate
# zero-row assumptions between the frame builder and the renderer:
#   * ifelse() returning its empty TEST vector, so `parent_var` and
#     `label` arrived logical(0) instead of character(0);
#   * `coefs$is_threshold <- FALSE`, a length-1 assignment into a
#     zero-row data.frame;
#   * .rbind_union() padding a missing column with a length-1 typed NA;
#   * six optional columns in align_frames() falling back to a scalar
#     NA that data.frame() recycles into rows -- but refuses against
#     none.
# These witnesses drive the PUBLIC call, because the frame being valid
# was never the promise the user cares about.

.m1_ord_fits <- function() {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("survey")
  d <- .cov100_ord_data()
  d$.w <- rep(1, nrow(d))
  d$.id <- seq_len(nrow(d))
  des <- survey::svydesign(id = ~.id, weights = ~.w, data = d)
  list(
    polr = MASS::polr(y ~ 1, data = d, Hess = TRUE),
    clm = ordinal::clm(y ~ 1, data = d),
    svyolr = survey::svyolr(y ~ 1, design = des)
  )
}

test_that("an intercept-only ordinal table renders its cut-points", {
  fits <- .m1_ord_fits()
  for (nm in names(fits)) {
    out <- paste(
      capture.output(print(table_regression(fits[[nm]]))),
      collapse = "\n"
    )
    # The Thresholds block is there, with both cut-points, labelled.
    expect_match(out, "Thresholds:", fixed = TRUE, info = nm)
    expect_match(out, "lo | mid", fixed = TRUE, info = nm)
    expect_match(out, "mid | hi", fixed = TRUE, info = nm)
    # And the fit statistics, so the table is a table and not a header.
    expect_match(out, "150", fixed = TRUE, info = nm)
  }
})

test_that("an intercept-only ordinal frame keeps the schema's column types", {
  fits <- .m1_ord_fits()
  for (nm in names(fits)) {
    fr <- as_regression_frame(fits[[nm]])
    expect_identical(nrow(fr$coefs), 0L, info = nm)
    expect_type(fr$coefs$parent_var, "character")
    expect_type(fr$coefs$label, "character")
    expect_invisible(spicy:::validate_regression_frame(fr))
  }
})

test_that("every public output survives a zero-coefficient ordinal fit", {
  fits <- .m1_ord_fits()
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    df <- table_regression(fit, output = "data.frame")
    expect_s3_class(df, "data.frame")
    expect_gt(nrow(df), 0L)
    lg <- table_regression(fit, output = "long")
    expect_s3_class(lg, "data.frame")
    # The structured view exists -- empty_render_table() has none, so
    # this is the assertion that says the table was really built.
    s <- as_structured(table_regression(fit))
    expect_gt(nrow(s$body), 0L)
    expect_s3_class(broom::tidy(table_regression(fit)), "data.frame")
    expect_s3_class(broom::glance(table_regression(fit)), "data.frame")
    expect_no_error(table_regression(fit, exponentiate = TRUE))
    expect_no_error(table_regression(
      fit,
      show_columns = c("b", "se", "ci", "p")
    ))
  }
})

test_that("a zero-coefficient fit sits beside a normal one in one table", {
  # The alignment layer is where the scalar-NA columns lived: a table
  # that mixes an empty frame with a populated one exercises the union
  # of both row sets, not just the empty case.
  fits <- .m1_ord_fits()
  d <- .cov100_ord_data()
  with_pred <- MASS::polr(y ~ x, data = d, Hess = TRUE)
  out <- paste(
    capture.output(print(table_regression(
      list(M0 = fits$polr, M1 = with_pred)
    ))),
    collapse = "\n"
  )
  expect_match(out, "Thresholds:", fixed = TRUE)
  expect_match(out, "x", fixed = TRUE)
})

test_that("pseudo-R2 is exactly zero when the fit IS its own null", {
  # An intercept-only fit reproduces the marginal frequencies, so both
  # pseudo-R2 are 0. The optimiser stops ~1e-14 short, which rendered as
  # the meaningless "-0.00" -- a negative R2 for a nested null.
  fits <- .m1_ord_fits()
  for (nm in c("polr", "clm")) {
    fs <- as_regression_frame(fits[[nm]])$info$fit_stats
    expect_identical(fs$pseudo_r2_mcfadden, 0, info = nm)
    expect_identical(fs$pseudo_r2_nagelkerke, 0, info = nm)
  }
  out <- paste(
    capture.output(print(table_regression(fits$polr))),
    collapse = "\n"
  )
  expect_false(grepl("-0.00", out, fixed = TRUE))
})

test_that("hiding the only block there is refuses instead of rendering nothing", {
  # `show_thresholds = FALSE` on a fit whose whole content is
  # cut-points leaves no rows at all. build_regression_table() answers
  # that with empty_render_table(): no fit statistics, no note, and no
  # structured view for as_structured() to find -- a bare header, which
  # is worse than either a table or a refusal. Say what emptied it.
  fits <- .m1_ord_fits()
  for (nm in names(fits)) {
    err <- tryCatch(
      table_regression(fits[[nm]], show_thresholds = FALSE),
      error = identity
    )
    expect_s3_class(err, "spicy_empty_table")
    expect_match(conditionMessage(err), "no rows to show", fixed = TRUE)
    expect_match(
      conditionMessage(err),
      "show_thresholds = FALSE",
      fixed = TRUE
    )
  }
  # A fit WITH predictors is untouched by the guard.
  d <- .cov100_ord_data()
  expect_no_error(
    table_regression(
      MASS::polr(y ~ x, data = d, Hess = TRUE),
      show_thresholds = FALSE
    )
  )
})
