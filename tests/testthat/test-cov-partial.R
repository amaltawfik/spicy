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


# Phase 3 matrix – vignettes-news:effect-size-type2-anova (lot T4)

.t2_unbalanced <- function() {
  set.seed(7)
  n <- 60
  d <- data.frame(
    A = factor(sample(c("a", "b", "c"), n, TRUE, prob = c(.5, .3, .2))),
    B = factor(sample(c("u", "v"), n, TRUE, prob = c(.6, .4)))
  )
  d$y <- rnorm(n) + as.numeric(d$A) + 0.5 * (d$B == "v")
  d
}

test_that("partial F equals the Type-II ANOVA reference on an unbalanced additive lm", {
  # Manual Type-II oracle (== car::Anova(fit, type = 2) for a model
  # without interactions): SS(term | all other terms) over the full
  # model's MSE. Dependency-free, and discriminating against Type I
  # (the sequential F for A differs on unbalanced data).
  d <- .t2_unbalanced()
  fit <- lm(y ~ A + B, data = d)
  rows <- spicy:::extract_partial_effect_rows(
    fit,
    ci_level = 0.95,
    show_columns = "partial_eta2",
    model_id = "M1",
    outcome = "y"
  )
  rss <- function(f) sum(resid(f)^2)
  mse <- rss(fit) / df.residual(fit)
  f_A <- ((rss(lm(y ~ B, data = d)) - rss(fit)) / 2) / mse
  f_B <- ((rss(lm(y ~ A, data = d)) - rss(fit)) / 1) / mse
  expect_equal(
    unique(rows$statistic[grepl("^A", rows$term)]),
    f_A,
    tolerance = 1e-10
  )
  expect_equal(
    unique(rows$statistic[grepl("^B", rows$term)]),
    f_B,
    tolerance = 1e-10
  )
  # Discriminating: the Type-I sequential F for A is different here.
  f_A_type1 <- anova(fit)[["F value"]][1L]
  expect_gt(abs(f_A - f_A_type1), 1e-4)
  # And the eta2 point estimate is the closed-form of that same F.
  eta_A <- (f_A * 2) / (f_A * 2 + df.residual(fit))
  expect_equal(
    unique(rows$estimate[grepl("^A", rows$term)]),
    eta_A,
    tolerance = 1e-10
  )
})

test_that("with an interaction, the main-effect partial F IS the Type-II reference", {
  # Resolved DISCORDANCE (Phase 3, lot T4): the implementation used to
  # drop a main effect while KEEPING its interaction columns (drop1
  # with a forced scope) -- a Type-III-style, contrast-dependent test.
  # The partial F is now the marginality-respecting Type II: for a
  # focal term T, compare { all terms that do NOT contain T } vs
  # { those terms + T } over the full-model MSE (car::Anova(type = 2)).
  d <- .t2_unbalanced()
  fit <- lm(y ~ A * B, data = d)
  rows <- spicy:::extract_partial_effect_rows(
    fit,
    ci_level = 0.95,
    show_columns = "partial_eta2",
    model_id = "M1",
    outcome = "y"
  )
  rss <- function(f) sum(resid(f)^2)
  mse <- rss(fit) / df.residual(fit)
  # Type II: SS(A | B), interaction excluded from BOTH nested models.
  f_A_type2 <- ((rss(lm(y ~ B, data = d)) - rss(lm(y ~ A + B, data = d))) / 2) /
    mse
  f_B_type2 <- ((rss(lm(y ~ A, data = d)) - rss(lm(y ~ A + B, data = d))) / 1) /
    mse
  expect_equal(
    unique(rows$statistic[rows$term %in% c("Ab", "Ac")]),
    f_A_type2,
    tolerance = 1e-10
  )
  expect_equal(
    unique(rows$statistic[rows$term %in% c("Bv")]),
    f_B_type2,
    tolerance = 1e-10
  )
  # Discriminating against the old forced-scope drop1 (Type-III-style)
  # value for the main effect under treatment coding -- exactly what
  # the previous implementation computed.
  d1 <- drop1(fit, scope = ~A, test = "F")
  expect_gt(abs(f_A_type2 - d1[["F value"]][2L]), 1e-4)
  # The highest-order term is untouched: Type II == drop1 exactly.
  d1_int <- drop1(fit, test = "F")
  expect_equal(
    unique(rows$statistic[grepl(":", rows$term)]),
    d1_int["A:B", "F value"],
    tolerance = 1e-12
  )
  # The eta2 point estimate is the closed form of the Type-II F.
  eta_A <- (f_A_type2 * 2) / (f_A_type2 * 2 + df.residual(fit))
  expect_equal(
    unique(rows$estimate[rows$term %in% c("Ab", "Ac")]),
    eta_A,
    tolerance = 1e-10
  )
})

test_that("Type-II partial F and effect sizes are contrast-invariant", {
  # The decisive property of the Type-II convention: refitting the
  # SAME model under sum (deviation) coding must leave the focal F,
  # the effect-size point estimates, and their CIs IDENTICAL. The old
  # forced-scope drop1 value changed with the contrasts.
  d <- .t2_unbalanced()
  fit_treat <- lm(y ~ A * B, data = d)
  fit_sum <- withr::with_options(
    list(contrasts = c("contr.sum", "contr.poly")),
    lm(y ~ A * B, data = d)
  )
  get_rows <- function(fit) {
    spicy:::extract_partial_effect_rows(
      fit,
      ci_level = 0.95,
      show_columns = c("partial_eta2", "partial_eta2_ci"),
      model_id = "M1",
      outcome = "y"
    )
  }
  rows_treat <- get_rows(fit_treat)
  rows_sum <- get_rows(fit_sum)
  per_term <- function(rows, pattern) {
    sub <- rows[grepl(pattern, rows$term), ]
    vapply(
      c("statistic", "estimate", "ci_low", "ci_high", "p_value"),
      function(col) unique(sub[[col]]),
      numeric(1)
    )
  }
  # Main effects and the interaction, matched by term shape (dummy
  # names differ across codings: Ab/Ac vs A1/A2, Ab:Bv vs A1:B1).
  for (pattern in c("^A[^:]*$", "^B[^:]*$", ":")) {
    expect_equal(
      per_term(rows_treat, pattern),
      per_term(rows_sum, pattern),
      tolerance = 1e-10
    )
  }
})

test_that("effectsize oracle: eta2/f2 derive from the Type-II F under interactions", {
  # effectsize's own lm path is sequential (Type I), so the oracle is
  # built by F_to_eta2 on OUR Type-II F -- the documented convention
  # for deriving the partial effect size from a focal F.
  skip_if_not_installed("effectsize")
  d <- .t2_unbalanced()
  fit <- lm(y ~ A * B, data = d)
  fs <- spicy:::extract_lm_focal_f_stat(fit, "A")
  rows <- spicy:::extract_partial_effect_rows(
    fit,
    ci_level = 0.95,
    show_columns = c("partial_eta2", "partial_f2", "partial_f2_ci"),
    model_id = "M1",
    outcome = "y"
  )
  a_rows <- rows[rows$term %in% c("Ab", "Ac"), ]
  eta_oracle <- effectsize::F_to_eta2(fs$f_obs, fs$df1, fs$df2, ci = 0.95)
  expect_equal(
    unique(a_rows$estimate[a_rows$estimate_type == "partial_eta2"]),
    eta_oracle$Eta2_partial,
    tolerance = 1e-10
  )
  # f2 CI plumbing runs the noncentral inversion on the same Type-II
  # F: check the defining equation pf(F; df1, df2, ncp(bound)) = target
  # with ncp recovered from the bound (f2 = ncp / df2).
  f2_rows <- a_rows[a_rows$estimate_type == "partial_f2", ]
  ci <- c(unique(f2_rows$ci_low), unique(f2_rows$ci_high))
  expect_equal(
    stats::pf(fs$f_obs, fs$df1, fs$df2, ncp = ci[1] * fs$df2),
    0.975,
    tolerance = 1e-6
  )
  expect_equal(
    stats::pf(fs$f_obs, fs$df1, fs$df2, ncp = ci[2] * fs$df2),
    0.025,
    tolerance = 1e-6
  )
})

# ---- glm partial_chi2: Type-II nested LRT under interactions --------------

.t2_unbalanced_glm <- function() {
  set.seed(11)
  n <- 240
  d <- data.frame(
    A = factor(sample(c("a", "b", "c"), n, TRUE, prob = c(.5, .3, .2))),
    B = factor(sample(c("u", "v"), n, TRUE, prob = c(.6, .4)))
  )
  eta <- -0.4 + 0.8 * (d$A == "b") + 0.4 * (d$A == "c") + 0.6 * (d$B == "v")
  d$y <- rbinom(n, 1, plogis(eta))
  d
}

test_that("glm partial_chi2 under an interaction is the Type-II nested LRT", {
  d <- .t2_unbalanced_glm()
  fit <- glm(y ~ A * B, data = d, family = binomial)
  # Nested-deviance oracle: both models exclude the interaction.
  dev_without_A <- deviance(glm(y ~ B, data = d, family = binomial))
  dev_with_A <- deviance(glm(y ~ A + B, data = d, family = binomial))
  out_A <- spicy:::compute_partial_chi2_for_term(fit, "A")
  expect_equal(out_A$chi2, dev_without_A - dev_with_A, tolerance = 1e-10)
  expect_identical(out_A$df, 2L)
  expect_equal(
    out_A$p_value,
    pchisq(dev_without_A - dev_with_A, df = 2, lower.tail = FALSE),
    tolerance = 1e-10
  )
  # Discriminating against the old forced-scope drop1 LRT -- exactly
  # what the previous implementation computed.
  d1 <- drop1(fit, scope = ~A, test = "LRT")
  expect_gt(abs(out_A$chi2 - d1[["LRT"]][2L]), 1e-4)
  # The highest-order term is untouched: Type II == drop1 exactly.
  out_int <- spicy:::compute_partial_chi2_for_term(fit, "A:B")
  d1_int <- drop1(fit, test = "LRT")
  expect_equal(out_int$chi2, d1_int["A:B", "LRT"], tolerance = 1e-10)
  expect_identical(out_int$df, 2L)
})

test_that("glm partial_chi2 is contrast-invariant under interactions", {
  d <- .t2_unbalanced_glm()
  fit_treat <- glm(y ~ A * B, data = d, family = binomial)
  fit_sum <- withr::with_options(
    list(contrasts = c("contr.sum", "contr.poly")),
    glm(y ~ A * B, data = d, family = binomial)
  )
  for (term in c("A", "B", "A:B")) {
    out_treat <- spicy:::compute_partial_chi2_for_term(fit_treat, term)
    out_sum <- spicy:::compute_partial_chi2_for_term(fit_sum, term)
    # IRLS converges on a deviance tolerance (~1e-8 relative), so the
    # invariance holds to that numeric precision, not to 1e-10.
    expect_equal(out_treat$chi2, out_sum$chi2, tolerance = 1e-6)
    expect_identical(out_treat$df, out_sum$df)
  }
})
