# ---------------------------------------------------------------------------
# Coverage tests for R/regression_partial.R.
#
# Exercises the reachable branches of the partial effect-size machinery
# that the broader suite does not already touch:
#
#   * the lm variance-explained path (partial_f2 / eta2 / omega2) early
#     returns: no active token, intercept-only model, singular coef skip,
#     all-terms-fail -> empty
#   * the glm partial_chi2 path early returns + singular coef skip
#   * the mixed-effects Wald chi^2 path: intercept-only -> NULL, and the
#     frame-attach helper's no-op + extra-column padding branches
#
# These call the internal helpers directly (`spicy:::fn`) where the public
# token validators would otherwise reject the construction (e.g. passing
# a glm with no partial_chi2 token), and via `as_regression_frame()` where
# the public path is reachable.
# ---------------------------------------------------------------------------

# ---- lm path: extract_partial_effect_rows() early returns ----------------

test_that("lm path returns empty when no partial token is active", {
  # `b` alone -> intersect(show_columns, partial_tokens) is empty -> the
  # `length(active) == 0L` branch returns empty_coefs_long().
  m <- lm(mpg ~ wt + hp, data = mtcars)
  out <- spicy:::extract_partial_effect_rows(
    fit = m,
    ci_level = 0.95,
    show_columns = c("b", "se"),
    model_id = "M1",
    outcome = "mpg"
  )
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
})

test_that("lm intercept-only model returns empty (no term labels)", {
  # `term.labels` is length 0 -> the guard on
  # `length(term_labels) == 0L` returns empty_coefs_long().
  m <- lm(mpg ~ 1, data = mtcars)
  out <- spicy:::extract_partial_effect_rows(
    fit = m,
    ci_level = 0.95,
    show_columns = c("partial_f2"),
    model_id = "M1",
    outcome = "mpg"
  )
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
})

test_that("lm with a singular coef skips the NA term but keeps valid terms", {
  # `hp2` is a perfect linear transform of `hp`, so lm aliases it to an
  # NA coefficient. `wt` stays estimable. The NA-coef term is skipped by
  # the `is.na(cf[i])` guard; the valid term still emits a partial row.
  d <- mtcars
  d$hp2 <- d$hp * 2
  m <- lm(mpg ~ wt + hp + hp2, data = d)
  expect_true(any(is.na(stats::coef(m)))) # confirm the singular setup

  out <- spicy:::extract_partial_effect_rows(
    fit = m,
    ci_level = 0.95,
    show_columns = c("partial_f2"),
    model_id = "M1",
    outcome = "mpg"
  )
  expect_gt(nrow(out), 0L)
  # The aliased coefficient never appears as a partial-effect term.
  expect_false("hp2" %in% out$term)
  expect_true("wt" %in% out$term)
})

test_that("lm with every term aliased/failed returns empty", {
  # Both predictors are collinear, so the focal F-stat extraction fails
  # for each term: the eff-is-NULL skip fires for every coef and the
  # final `length(rows) == 0L` guard returns empty_coefs_long().
  d <- mtcars
  d$hp2 <- d$hp * 2
  m <- lm(mpg ~ hp + hp2, data = d)
  out <- spicy:::extract_partial_effect_rows(
    fit = m,
    ci_level = 0.95,
    show_columns = c("partial_f2"),
    model_id = "M1",
    outcome = "mpg"
  )
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
})


# ---- compute_partial_effects_for_term(): NULL on a missing/failed term ---

test_that("compute_partial_effects_for_term returns NULL for an unknown term", {
  # extract_lm_focal_f_stat() returns NULL for a non-existent term label,
  # so the `is.null(fs)` guard returns NULL.
  m <- lm(mpg ~ wt + hp, data = mtcars)
  expect_null(
    spicy:::compute_partial_effects_for_term(m, "no_such_term", 0.95)
  )
})


# ---- glm path: extract_partial_chi2 early returns + singular skip --------

test_that("glm path returns empty when partial_chi2 token absent", {
  # The glm branch only emits rows for `partial_chi2`; without that token
  # it returns empty_coefs_long().
  g <- glm(am ~ hp, data = mtcars, family = binomial)
  out <- spicy:::extract_partial_effect_rows(
    fit = g,
    ci_level = 0.95,
    show_columns = c("b", "se"),
    model_id = "M1",
    outcome = "am"
  )
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
})

test_that("glm partial_chi2 path skips a singular coef but keeps valid terms", {
  # Same aliasing trick as the lm case: `hp2` is collinear with `hp`, so
  # glm leaves it NA. The `is.na(cf[i])` guard skips it; `wt` survives.
  d <- mtcars
  d$hp2 <- d$hp * 2
  g <- glm(am ~ wt + hp + hp2, data = d, family = binomial)
  expect_true(any(is.na(stats::coef(g))))

  out <- spicy:::extract_partial_effect_rows(
    fit = g,
    ci_level = 0.95,
    show_columns = c("partial_chi2"),
    model_id = "M1",
    outcome = "am"
  )
  expect_gt(nrow(out), 0L)
  expect_true(all(out$estimate_type == "partial_chi2"))
  expect_false("hp2" %in% out$term)
})


# ---- mixed-effects Wald chi^2: intercept-only + frame-attach helper ------

test_that("mixed chi^2 helper returns NULL for an intercept-only fit", {
  skip_if_not_installed("lme4")
  # No fixed-effect terms beyond the intercept -> term.labels is empty,
  # so .compute_partial_chi2_rows_for_mixed() returns NULL via the
  # `length(term_labels) == 0L` guard.
  fit <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  # nobars() emits a one-time reformulas deprecation notice; not a failure.
  expect_null(
    suppressWarnings(spicy:::.compute_partial_chi2_rows_for_mixed(fit))
  )
})

test_that("attach helper is a no-op when partial_chi2 token is absent", {
  skip_if_not_installed("lme4")
  # Without `partial_chi2` in show_columns the helper short-circuits and
  # returns the input coefs untouched (no chi^2 computation attempted).
  dd <- lme4::sleepstudy
  dd$period <- factor(rep(c("a", "b", "c"), length.out = nrow(dd)))
  fit <- lme4::lmer(Reaction ~ Days + period + (1 | Subject), data = dd)
  coefs <- spicy:::.merMod_coefs(fit, ci_level = 0.95, family_z = FALSE)
  out <- spicy:::.attach_partial_chi2_to_frame_coefs(
    coefs,
    fit,
    show_columns = c("b", "se", "ci", "p")
  )
  expect_identical(out, coefs)
})

test_that("attach helper is a no-op when the chi^2 rows are NULL", {
  skip_if_not_installed("lme4")
  # An intercept-only fit yields NULL chi^2 rows, so the attach helper
  # returns the input `coefs` unchanged even when partial_chi2 is asked
  # for.
  fit <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  coefs <- spicy:::.merMod_coefs(fit, ci_level = 0.95, family_z = FALSE)
  out <- suppressWarnings(spicy:::.attach_partial_chi2_to_frame_coefs(
    coefs,
    fit,
    show_columns = c("b", "partial_chi2")
  ))
  expect_identical(out, coefs)
})

test_that("attach helper pads coefs-only columns on the chi^2 rows", {
  skip_if_not_installed("lme4")
  # When `coefs` carries a column the freshly built chi^2 rows lack, the
  # padding loop must fill that column with NA on the new rows and align
  # the rbind. Simulate a wider coefs frame (as a future attach step
  # could produce) and confirm the new partial rows are padded.
  dd <- lme4::sleepstudy
  dd$period <- factor(rep(c("a", "b", "c"), length.out = nrow(dd)))
  fit <- lme4::lmer(Reaction ~ Days + period + (1 | Subject), data = dd)

  coefs <- spicy:::.merMod_coefs(fit, ci_level = 0.95, family_z = FALSE)
  coefs$extra_marker <- NA_real_ # a column the chi^2 rows do not build

  out <- spicy:::.attach_partial_chi2_to_frame_coefs(
    coefs,
    fit,
    show_columns = c("b", "partial_chi2")
  )
  # The new partial_chi2 rows were appended ...
  expect_gt(nrow(out), nrow(coefs))
  expect_true("partial_chi2" %in% out$estimate_type)
  # ... and the coefs-only column is NA on every appended chi^2 row.
  pchi <- out[out$estimate_type == "partial_chi2", ]
  expect_true(all(is.na(pchi$extra_marker)))
  # Column ordering matches the original coefs schema.
  expect_identical(colnames(out), colnames(coefs))
})


# ---- Public-path sanity: lm partial tokens render through the frame ------

test_that("as_regression_frame(lm, partial_f2) emits one value per term", {
  # End-to-end check that the happy lm path (factor + numeric terms) is
  # wired through as_regression_frame() and produces partial_f2 rows.
  m <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  fr <- as_regression_frame(m, show_columns = c("b", "partial_f2"))
  pf <- fr$coefs[fr$coefs$estimate_type == "partial_f2", ]
  expect_gt(nrow(pf), 0L)
  expect_true(all(is.finite(pf$estimate)))
  # The 3-level factor shares one joint partial F across its 2 dummies.
  cyl_rows <- pf[grepl("^factor\\(cyl\\)", pf$term), ]
  expect_length(unique(cyl_rows$estimate), 1L)
})
