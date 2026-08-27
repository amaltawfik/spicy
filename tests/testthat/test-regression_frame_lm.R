# ---------------------------------------------------------------------------
# Phase 0b sub-step 2 tests: as_regression_frame.lm() / .glm().
#
# Validates that the strangler-fig wrappers produce frames that:
#   * pass validate_regression_frame() on the documented schema
#   * preserve the legacy extractor's numeric content (no silent drift)
#   * cross-validate (when packages are installed) against parameters and
#     marginaleffects as oracles, with explicit argument pinning per the
#     audit discipline in dev/design_as_regression_frame.md section 7
# ---------------------------------------------------------------------------

# ---- Shared fixtures ------------------------------------------------------

# `sochealth` is the bundled survey-style data frame in spicy. Using it
# matches the audit-with-real-data rule (memory: audit_real_data_varied_inputs).
.fixture_lm <- function() {
  lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
}

.fixture_lm_multilevel <- function() {
  # Multi-level treatment-coded factor stress test. sochealth ships
  # `income_group` as an ordered factor (polynomial contrasts produce
  # `.L` / `.Q` / `.C` columns instead of per-level coefficients), so
  # we coerce to plain `factor` here to exercise factor_level_pos and
  # the reference-row path under contr.treatment -- the convention
  # `as_regression_frame.lm()` is designed for.
  d <- sochealth
  d$inc <- factor(
    as.character(d$income_group),
    levels = c("Low", "Lower middle", "Upper middle", "High")
  )
  lm(wellbeing_score ~ inc + sex, data = d)
}

.fixture_glm_logistic <- function() {
  # smoking is a binary factor in sochealth; a simple logistic fit
  # exercises the glm path including family / pseudo_r2 / exponentiate.
  d <- sochealth
  d$smoker_bin <- as.integer(d$smoking == "Yes")
  glm(
    smoker_bin ~ age + sex + physical_activity,
    data = d,
    family = binomial(link = "logit")
  )
}


# ---- lm: schema validity --------------------------------------------------

test_that("as_regression_frame.lm() produces a schema-valid frame", {
  fit <- .fixture_lm()
  frame <- as_regression_frame(fit)
  expect_invisible(validate_regression_frame(frame))
})

test_that("as_regression_frame.lm() attaches required attributes", {
  fit <- .fixture_lm()
  frame <- as_regression_frame(fit)
  expect_identical(attr(frame, "spicy_frame_version"), spicy_frame_version())
  # The fit attribute round-trips the original object so downstream AME
  # and refit-standardisation can read it without re-fitting.
  expect_identical(attr(frame, "fit"), fit)
})

test_that("as_regression_frame.lm() populates required info fields correctly", {
  fit <- .fixture_lm()
  frame <- as_regression_frame(fit)
  info <- frame$info

  expect_identical(info$class, "lm")
  expect_identical(info$family$family, "gaussian")
  expect_identical(info$family$link, "identity")
  expect_identical(info$dv, "wellbeing_score")
  expect_identical(as.integer(info$n_obs), as.integer(stats::nobs(fit)))
  expect_identical(info$weights_kind, "none")
  expect_identical(info$random_effects, spicy:::empty_random_effects())
  expect_null(info$n_groups)
  expect_identical(info$ci_level, 0.95)
  expect_identical(info$ci_method, "wald")
  # The canon, not the "classical" synonym the argument still spells:
  # new_regression_frame() normalises on the way in (register n. 228).
  expect_identical(info$vcov_kind, "model")
  expect_identical(info$vcov_label, "OLS")
})

test_that("as_regression_frame.lm() reports lm capabilities", {
  fit <- .fixture_lm()
  frame <- as_regression_frame(fit)
  sp <- frame$info$supports
  expect_true(sp$ame)
  expect_true(sp$partial_effect_size)
  expect_true(sp$classical_r2)
  expect_true(sp$nested_lrt)
  expect_false(sp$exponentiate)
  expect_true(sp$standardise_refit)
})

test_that("as_regression_frame.lm() preserves fit_stats from legacy extractor", {
  fit <- .fixture_lm()
  frame <- as_regression_frame(fit)
  fs <- frame$info$fit_stats

  # Required schema field
  expect_identical(as.integer(fs$nobs), as.integer(stats::nobs(fit)))

  # Schema-named aliases (additive on top of legacy keys; sub-step 4 will
  # pick a canonical naming).
  expect_equal(fs$r_squared, summary(fit)$r.squared, tolerance = 1e-10)
  expect_equal(fs$adj_r_squared, summary(fit)$adj.r.squared, tolerance = 1e-10)
  expect_equal(fs$aic, stats::AIC(fit), tolerance = 1e-10)
  expect_equal(fs$bic, stats::BIC(fit), tolerance = 1e-10)
  expect_equal(fs$log_lik, as.numeric(stats::logLik(fit)), tolerance = 1e-10)

  # Legacy keys still present (sub-step 4 will delete the legacy aliases
  # once downstream consumers all migrate to the schema names).
  expect_true(!is.null(fs$r2))
  expect_true(!is.null(fs$AIC))
})


# ---- lm: coefs reshape ----------------------------------------------------

test_that("coefs has one B row per legacy coefficient (excluding ref rows)", {
  fit <- .fixture_lm()
  frame <- as_regression_frame(fit)
  # Reference rows also carry estimate_type = "B" but they are
  # placeholders (NA estimate, is_ref = TRUE) emitted by
  # `build_reference_rows()` -- they don't correspond to a fitted
  # coefficient, so we exclude them from the count.
  b_rows <- frame$coefs[
    frame$coefs$estimate_type == "B" & !frame$coefs$is_ref,
  ]
  legacy_coefs <- stats::coef(fit)
  expect_identical(nrow(b_rows), length(legacy_coefs))
})

test_that("coefs estimates match coef(fit) for non-factor and factor terms", {
  fit <- .fixture_lm()
  frame <- as_regression_frame(fit)
  b_rows <- frame$coefs[
    frame$coefs$estimate_type == "B" & !frame$coefs$is_ref,
  ]
  legacy_coefs <- stats::coef(fit)
  # Match by term name; numeric agreement to machine precision (we pass
  # through the legacy values, no re-computation).
  for (nm in names(legacy_coefs)) {
    row <- b_rows[b_rows$term == nm, ]
    expect_equal(
      row$estimate,
      unname(legacy_coefs[nm]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})

test_that("coefs SE / CI / p match summary(fit) for non-factor terms", {
  fit <- .fixture_lm()
  frame <- as_regression_frame(fit)
  b_rows <- frame$coefs[
    frame$coefs$estimate_type == "B" & !frame$coefs$is_ref,
  ]
  sm <- summary(fit)$coefficients

  for (nm in rownames(sm)) {
    row <- b_rows[b_rows$term == nm, ]
    expect_equal(
      row$std_error,
      unname(sm[nm, "Std. Error"]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
    expect_equal(
      row$statistic,
      unname(sm[nm, "t value"]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
    expect_equal(
      row$p_value,
      unname(sm[nm, "Pr(>|t|)"]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})

test_that("coefs CI matches confint(fit)", {
  fit <- .fixture_lm()
  frame <- as_regression_frame(fit)
  ci_legacy <- stats::confint(fit, level = 0.95)
  b_rows <- frame$coefs[
    frame$coefs$estimate_type == "B" & !frame$coefs$is_ref,
  ]
  for (nm in rownames(ci_legacy)) {
    row <- b_rows[b_rows$term == nm, ]
    expect_equal(
      row$ci_lower,
      unname(ci_legacy[nm, 1]),
      tolerance = 1e-8,
      info = paste("term:", nm)
    )
    expect_equal(
      row$ci_upper,
      unname(ci_legacy[nm, 2]),
      tolerance = 1e-8,
      info = paste("term:", nm)
    )
  }
})


# ---- lm: factor structure -------------------------------------------------

test_that("factor coefs carry parent_var, label, and factor_level_pos", {
  fit <- .fixture_lm_multilevel()
  frame <- as_regression_frame(fit)

  # Pick the rows that came from the multi-level factor `inc`.
  factor_rows <- frame$coefs[
    !is.na(frame$coefs$factor_level_pos) &
      frame$coefs$parent_var == "inc",
  ]
  expect_gt(nrow(factor_rows), 0L)

  # parent_var is the variable name (not the level concatenation).
  expect_true(all(factor_rows$parent_var == "inc"))

  # label is the bare level name (no variable prefix). Each label
  # appears among the variable's defined levels.
  inc_levels <- c("Low", "Lower middle", "Upper middle", "High")
  expect_true(all(factor_rows$label %in% inc_levels))

  # factor_level_pos is a 1-based index into levels(). Always positive
  # for actual factor coefficients (reference rows handled separately).
  expect_true(all(factor_rows$factor_level_pos >= 1L))
})

test_that("non-factor predictors get fallback parent_var = label = term", {
  fit <- .fixture_lm()
  frame <- as_regression_frame(fit)
  # `age` is numeric -- no factor metadata.
  age_row <- frame$coefs[frame$coefs$term == "age", ]
  expect_identical(age_row$parent_var, "age")
  expect_identical(age_row$label, "age")
  expect_true(is.na(age_row$factor_level_pos))
})

test_that("reference rows are marked is_ref and have NA estimate", {
  fit <- .fixture_lm_multilevel()
  frame <- as_regression_frame(fit)
  ref_rows <- frame$coefs[frame$coefs$is_ref, ]
  # income_group has at least one reference row under contr.treatment.
  expect_gt(nrow(ref_rows), 0L)
  expect_true(all(is.na(ref_rows$estimate)))
  expect_true(all(is.na(ref_rows$std_error)))
  expect_true(all(is.na(ref_rows$ci_lower)))
  expect_true(all(is.na(ref_rows$ci_upper)))
})


# ---- glm: schema validity + class-specific bits ---------------------------

test_that("as_regression_frame.glm() produces a schema-valid frame", {
  fit <- .fixture_glm_logistic()
  frame <- as_regression_frame(fit)
  expect_invisible(validate_regression_frame(frame))
})

test_that("as_regression_frame.glm() populates family from the fit", {
  fit <- .fixture_glm_logistic()
  frame <- as_regression_frame(fit)
  expect_identical(frame$info$class, "glm")
  expect_identical(frame$info$family$family, "binomial")
  expect_identical(frame$info$family$link, "logit")
})

test_that("as_regression_frame.glm() reports glm capabilities", {
  fit <- .fixture_glm_logistic()
  frame <- as_regression_frame(fit)
  sp <- frame$info$supports
  expect_true(sp$ame)
  expect_true(sp$partial_effect_size) # partial chi^2 path
  expect_false(sp$classical_r2) # use pseudo_r2 instead
  expect_true(sp$nested_lrt)
  expect_true(sp$exponentiate) # OR / RR / IRR on response scale
  expect_true(sp$standardise_refit)
})

test_that("as_regression_frame.glm() carries pseudo_r2 with all three methods", {
  fit <- .fixture_glm_logistic()
  frame <- as_regression_frame(fit)
  pr2 <- frame$info$fit_stats$pseudo_r2
  expect_type(pr2, "list")
  expect_named(pr2, c("mcfadden", "nagelkerke", "tjur"), ignore.order = TRUE)
  expect_true(is.finite(pr2$mcfadden))
  expect_true(is.finite(pr2$nagelkerke))
  expect_true(is.finite(pr2$tjur))
})


# ---- vcov: HC* path -------------------------------------------------------

test_that("vcov = 'HC3' is reflected in vcov_kind and vcov_label", {
  fit <- .fixture_lm()
  frame <- as_regression_frame(fit, vcov = "HC3")
  expect_identical(frame$info$vcov_kind, "HC3")
  expect_match(frame$info$vcov_label, "HC3", fixed = TRUE)
})

test_that("HC3 standard errors differ from classical (sanity check)", {
  fit <- .fixture_lm()
  f_classical <- as_regression_frame(fit, vcov = "classical")
  f_hc3 <- as_regression_frame(fit, vcov = "HC3")

  # Pick the same non-reference B row in both frames and confirm the
  # SE moved -- HC3 is a different estimator than the OLS classical
  # one. We don't pin a specific magnitude, just non-equality.
  pick <- function(fr, term) {
    r <- fr$coefs[fr$coefs$term == term & fr$coefs$estimate_type == "B", ]
    r$std_error
  }
  se_classical <- pick(f_classical, "age")
  se_hc3 <- pick(f_hc3, "age")
  expect_false(isTRUE(all.equal(se_classical, se_hc3, tolerance = 1e-6)))
})


# ---- Oracle: parameters::model_parameters() -------------------------------

test_that("lm coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fixture_lm()
  frame <- as_regression_frame(fit)

  # Per audit discipline: pin every argument explicitly so future default
  # drift in parameters does not break this test.
  oracle <- parameters::model_parameters(
    fit,
    ci = 0.95,
    ci_method = "wald",
    standardize = NULL,
    test = NULL
  )

  b_rows <- frame$coefs[
    frame$coefs$estimate_type == "B" & !frame$coefs$is_ref,
  ]
  n_checked <- 0L
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    # Both lookups must hit exactly one row: an unmatched term
    # would otherwise compare a zero-row frame and the counter
    # below would never see it.
    expect_identical(nrow(oracle_row), 1L, info = nm)
    expect_identical(nrow(spicy_row), 1L, info = nm)
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-8,
      info = paste("oracle B mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-8,
      info = paste("oracle SE mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$ci_lower,
      oracle_row$CI_low,
      tolerance = 1e-6,
      info = paste("oracle CI lower mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$ci_upper,
      oracle_row$CI_high,
      tolerance = 1e-6,
      info = paste("oracle CI upper mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$p_value,
      oracle_row$p,
      tolerance = 1e-8,
      info = paste("oracle p mismatch on term:", nm)
    )
    n_checked <- n_checked + 1L
  }
  expect_oracle_covered(n_checked, length(oracle$Parameter))
})

test_that("glm coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fixture_glm_logistic()
  frame <- as_regression_frame(fit)

  oracle <- parameters::model_parameters(
    fit,
    ci = 0.95,
    ci_method = "wald",
    standardize = NULL,
    test = NULL,
    exponentiate = FALSE
  )

  b_rows <- frame$coefs[
    frame$coefs$estimate_type == "B" & !frame$coefs$is_ref,
  ]
  n_checked <- 0L
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    # Both lookups must hit exactly one row: an unmatched term
    # would otherwise compare a zero-row frame and the counter
    # below would never see it.
    expect_identical(nrow(oracle_row), 1L, info = nm)
    expect_identical(nrow(spicy_row), 1L, info = nm)
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-7,
      info = paste("oracle B mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-7,
      info = paste("oracle SE mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$p_value,
      oracle_row$p,
      tolerance = 1e-7,
      info = paste("oracle p mismatch on term:", nm)
    )
    n_checked <- n_checked + 1L
  }
  expect_oracle_covered(n_checked, length(oracle$Parameter))
})


# ---- Unsupported class -- the default fallback still wins ----------------

test_that("a non-lm class falls through to the default error", {
  expect_error(
    as_regression_frame(structure(list(), class = "made_up_fit")),
    class = "spicy_unsupported_class"
  )
})


# ============================================================================
# Phase 3 matrix – rd-core:vcov-matrix-lm-glm-all (lm half)
# ============================================================================

test_that("lm supports every HC* estimator with the sandwich oracle", {
  skip_if_not_installed("sandwich")
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  nm <- names(coef(fit))
  for (hc in paste0("HC", 0:5)) {
    out <- table_regression(fit, vcov = hc)
    s <- as_structured(out)
    rows <- match(nm, s$body$Variable)
    expect_false(anyNA(rows), info = hc)
    oracle <- sqrt(diag(sandwich::vcovHC(fit, type = hc)))
    expect_equal(
      s$body$SE[rows],
      unname(oracle[nm]),
      tolerance = 1e-10,
      info = hc
    )
  }
})

test_that("lm supports every CR* estimator with the clubSandwich oracle", {
  skip_if_not_installed("clubSandwich")
  d <- sochealth
  fit <- lm(wellbeing_score ~ age, data = d)
  cl <- d$region
  nm <- names(coef(fit))
  for (cr in paste0("CR", 0:3)) {
    out <- table_regression(fit, vcov = cr, cluster = cl)
    s <- as_structured(out)
    rows <- match(nm, s$body$Variable)
    expect_false(anyNA(rows), info = cr)
    oracle <- sqrt(diag(as.matrix(
      clubSandwich::vcovCR(fit, cluster = cl, type = cr)
    )))
    expect_equal(
      s$body$SE[rows],
      unname(oracle[seq_along(nm)]),
      tolerance = 1e-8,
      info = cr
    )
  }
})

test_that("lm jackknife matches the manual leave-one-out oracle", {
  mt2 <- mtcars
  mt2$cyl <- factor(mt2$cyl)
  fit <- lm(mpg ~ wt + cyl, data = mt2)
  out <- table_regression(fit, vcov = "jackknife")
  s <- as_structured(out)
  rows <- match(c("(Intercept)", "wt", "6", "8"), trimws(s$body$Variable))
  # Independent oracle: n leave-one-out lm() refits, Tukey scaling
  n <- nrow(mt2)
  beta_loo <- t(vapply(
    seq_len(n),
    function(i) coef(lm(mpg ~ wt + cyl, data = mt2[-i, ])),
    numeric(length(coef(fit)))
  ))
  centered <- sweep(beta_loo, 2L, colMeans(beta_loo))
  vc <- (n - 1L) / n * crossprod(centered)
  expect_equal(
    s$body$SE[rows],
    unname(sqrt(diag(vc))),
    tolerance = 1e-8
  )
})

test_that("lm bootstrap runs and reports finite resampled SEs", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  set.seed(20260801)
  out <- table_regression(fit, vcov = "bootstrap", boot_n = 40L)
  s <- as_structured(out)
  rows <- match(names(coef(fit)), s$body$Variable)
  expect_true(all(is.finite(s$body$SE[rows])))
  expect_match(attr(out, "note"), "bootstrap", ignore.case = TRUE)
})
