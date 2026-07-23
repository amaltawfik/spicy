# ---------------------------------------------------------------------------
# Targeted coverage tests for R/regression_frame_nlme.R.
#
# These exercise branches that the main test-regression_frame_nlme.R suite
# does not reach with its canonical fixtures:
#   * Ordered-factor (polynomial-contrast) predictor -> reference-row
#     synthesis short-circuits (.nlme_reference_rows: skip + empty return).
#   * Nested random effects -> VarCorr sub-header rows that don't parse are
#     skipped in .lme_random_effects().
#   * .lme_attach_wald_se_ci() schema fallback when the input lacks the
#     `is_correlation` column.
#   * .extract_dv_label_nlme(): label-attribute hit + missing-column miss.
#
# Defensive arms (package-not-available guard, VarCorr/intervals failures,
# zero-row var-cov frames) are marked # nocov in the source instead.
# ---------------------------------------------------------------------------

# ---- Ordered-factor predictor: polynomial contrasts ----------------------

# An ordered factor uses contr.poly, so detect_factor_terms() flags the term
# with reference_dropped = FALSE. .nlme_reference_rows() must then (a) hit the
# `next` that skips non-treatment terms and (b) return an empty coefs frame
# because no reference rows were synthesised.

test_that("lme with an ordered-factor predictor synthesises no reference rows", {
  skip_if_not_installed("nlme")
  d <- nlme::Orthodont
  d$agef <- factor(d$age, ordered = TRUE) # contr.poly -> .L/.Q/.C terms
  fit <- nlme::lme(distance ~ agef, data = d, random = ~ 1 | Subject)

  # Internal: reference-row synthesis returns an empty frame (no is_ref rows).
  ref <- spicy:::.nlme_reference_rows(fit)
  expect_identical(nrow(ref), 0L)

  # Public frame: every coef row is a real estimate; none flagged is_ref.
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_false(any(fr$coefs$is_ref))
  # The polynomial-contrast terms are surfaced as ordinary B rows.
  expect_true(all(c("agef.L", "agef.Q", "agef.C") %in% fr$coefs$term))
})


# ---- Nested random effects: unparseable VarCorr sub-header rows ----------

# `random = ~ 1 | Dog/Side` makes nlme::VarCorr() emit "Dog =" / "Side ="
# header rows whose Variance cell is a pdClass label ("pdLogChol(1)") that
# parses to NA. .lme_random_effects() must skip those rows (the `is.na`
# guard) and keep only the numeric variance components + residual.

test_that("lme with nested random effects drops unparseable VarCorr rows", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(pixel ~ day, data = nlme::Pixel, random = ~ 1 | Dog / Side)

  re <- spicy:::.lme_random_effects(fit)
  vc <- re$variance_components

  # Raw VarCorr has 5 rows (2 pdClass headers + 2 intercepts + residual);
  # only the 3 numeric rows survive.
  expect_identical(nrow(vc), 3L)
  expect_true(all(is.finite(vc$variance)))
  expect_true(any(vc$group == "Residual"))

  # Public frame still validates end to end.
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})


# ---- .lme_attach_wald_se_ci(): missing is_correlation column -------------

# In the normal pipeline .lme_append_correlation_rows() always adds the
# is_correlation column before this helper runs. Called in isolation on a
# frame that lacks it, the helper must fall back to treating every row as a
# variance row (the `rep(FALSE, nrow(vc_df))` branch) and still attach the
# Wald SE / CI schema columns.

test_that(".lme_attach_wald_se_ci tolerates a frame without is_correlation", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    distance ~ age,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )

  vc_df <- data.frame(
    group = "Subject",
    term = "(Intercept)",
    variance = 7,
    sd = sqrt(7),
    corr = NA_real_,
    stringsAsFactors = FALSE
  )
  expect_false("is_correlation" %in% names(vc_df))

  out <- spicy:::.lme_attach_wald_se_ci(vc_df, fit)

  # Schema columns added; no error from the missing is_correlation column.
  expect_true(all(
    c("std_error", "ci_lower", "ci_upper", "ci_method") %in%
      names(out)
  ))
  expect_identical(nrow(out), 1L)
  # The random-intercept row gets a finite Wald SE on the variance scale.
  expect_true(is.finite(out$std_error[1]))
  expect_identical(out$ci_method[1], "wald")
})


# ---- .extract_dv_label_nlme(): label hit + missing-column miss -----------

test_that(".extract_dv_label_nlme returns a column 'label' attribute", {
  skip_if_not_installed("nlme")
  d <- nlme::Orthodont
  attr(d$distance, "label") <- "Dental distance (mm)"
  fit <- nlme::lme(distance ~ age, data = d, random = ~ 1 | Subject)

  expect_identical(
    spicy:::.extract_dv_label_nlme(fit, "distance"),
    "Dental distance (mm)"
  )

  # End to end: the labelled DV flows into info$dv_label.
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv_label, "Dental distance (mm)")
})

test_that(".extract_dv_label_nlme falls back to the name for a missing column", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    distance ~ age,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )
  # A dv name that is not a column of getData(fit) -> returns the name.
  expect_identical(
    spicy:::.extract_dv_label_nlme(fit, "not_a_real_column"),
    "not_a_real_column"
  )
})

test_that(".extract_dv_label_nlme returns the name when no label attr exists", {
  skip_if_not_installed("nlme")
  # distance carries no "label" attribute -> the nzchar() guard is FALSE and
  # the helper returns the dv name.
  fit <- nlme::lme(
    distance ~ age,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )
  expect_identical(
    spicy:::.extract_dv_label_nlme(fit, "distance"),
    "distance"
  )
})
