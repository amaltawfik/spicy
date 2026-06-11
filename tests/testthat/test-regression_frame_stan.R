# ---------------------------------------------------------------------------
# Phase 3 tests: as_regression_frame() methods for Bayesian fits.
#
# Covers:
#   * stanreg (rstanarm)   -- gated by skip_if_not_installed("rstanarm")
#   * brmsfit (brms)       -- gated by skip_if_not_installed("brms")
#   * Schema validity in all paths, with the new `pd` column populated
#     and `p_value` left NA per Q1.
#   * ci_method = "posterior_quantile" by default; ETI CrI.
#   * Family-aware title_prefix decorated with the posterior engine
#     ("stanreg" / "brmsfit").
#   * Oracle cross-validation against parameters::model_parameters()
#     (skipped if not installed).
#
# Fits are intentionally tiny (1 chain, ~300 iterations) so the suite
# stays fast. Each test creates its own fit because brmsfit objects
# are large enough that caching them across tests can blow up the
# CI memory budget; the marginal MCMC cost per call is small (~2-5s).
# `set.seed()` makes the draws deterministic for assertion stability.
# ---------------------------------------------------------------------------


# ---- Fast-fit helpers ------------------------------------------------------

.fit_brms_basic <- function() {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  skip_if_not_installed("lme4")
  set.seed(1)
  brms::brm(
    Reaction ~ Days,
    data = lme4::sleepstudy,
    chains = 1, iter = 400, refresh = 0, silent = 2, backend = "rstan"
  )
}

.fit_brms_factor <- function() {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  skip_if_not_installed("lme4")
  d <- lme4::sleepstudy
  d$treatment <- factor(rep(c("A", "B", "C"), length.out = nrow(d)))
  set.seed(2)
  brms::brm(
    Reaction ~ Days + treatment,
    data = d,
    chains = 1, iter = 400, refresh = 0, silent = 2, backend = "rstan"
  )
}

.fit_brms_logit <- function() {
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  d <- mtcars
  set.seed(3)
  brms::brm(
    am ~ mpg,
    data = d,
    family = brms::bernoulli(),
    chains = 1, iter = 400, refresh = 0, silent = 2, backend = "rstan"
  )
}

.fit_rstanarm_basic <- function() {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  skip_if_not_installed("lme4")
  set.seed(4)
  rstanarm::stan_glm(
    Reaction ~ Days,
    data = lme4::sleepstudy,
    chains = 1, iter = 400, refresh = 0
  )
}


# ---- 1. brmsfit: schema validity + Bayesian-specific fields --------------

test_that("as_regression_frame.brmsfit produces a schema-valid frame", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("brmsfit: required attributes round-trip", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("brmsfit: info$class = 'brmsfit'", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "brmsfit")
})

test_that("brmsfit: info$ci_method = 'posterior_quantile' by default", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "posterior_quantile")
})

test_that("brmsfit: info$weights_kind = 'none' and random_effects = NULL", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$weights_kind, "none")
  expect_null(fr$info$random_effects)
})


# ---- 2. brmsfit: coefs columns (pd populated, p_value NA per Q1) ---------

test_that("brmsfit: coefs$p_value is NA_real_ for every row (Q1)", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(all(is.na(fr$coefs$p_value)))
})

test_that("brmsfit: coefs$pd populated and in [0.5, 1]", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_true(all(!is.na(b_rows$pd)))
  expect_true(all(b_rows$pd >= 0.5 - 1e-9))
  expect_true(all(b_rows$pd <= 1.0 + 1e-9))
})

test_that("brmsfit: df / statistic / test_type are NA for fixed-effect rows", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_true(all(is.na(b_rows$df)))
  expect_true(all(is.na(b_rows$statistic)))
  expect_true(all(is.na(b_rows$test_type)))
})

test_that("brmsfit: estimate matches posterior median; std_error matches posterior SD", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  # Use posterior::summarise_draws directly on b_* to construct the
  # expected medians / SDs, then compare row-by-row.
  draws <- posterior::as_draws_array(fit)
  b_vars <- grep("^b_", posterior::variables(draws), value = TRUE)
  sm <- posterior::summarise_draws(
    posterior::subset_draws(draws, variable = b_vars),
    "median", "sd"
  )
  # Match by stripping the b_ prefix on summary side.
  human <- ifelse(sm$variable == "b_Intercept", "(Intercept)",
                  sub("^b_", "", sm$variable))
  for (i in seq_along(human)) {
    nm <- human[i]
    row <- fr$coefs[fr$coefs$term == nm & !fr$coefs$is_ref, ]
    expect_equal(row$estimate,  sm$median[i], tolerance = 1e-10,
                 info = paste("term:", nm))
    expect_equal(row$std_error, sm$sd[i],     tolerance = 1e-10,
                 info = paste("term:", nm))
  }
})


# ---- 3. brmsfit: family-aware title_prefix and supports ------------------

test_that("brmsfit gaussian: title_prefix names linear + brmsfit engine", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "linear",  fixed = TRUE)
  expect_match(fr$info$extras$title_prefix, "brmsfit", fixed = TRUE)
})

test_that("brmsfit logit: family = bernoulli/logit; title decorates with logistic", {
  fit <- .fit_brms_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "bernoulli")
  expect_identical(fr$info$family$link,   "logit")
  expect_match(fr$info$extras$title_prefix, "logistic", fixed = TRUE)
})

test_that("brmsfit: supports flags reflect Bayesian conventions", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sp <- fr$info$supports
  expect_true(sp$ame)
  expect_false(sp$partial_effect_size)
  expect_false(sp$classical_r2)
  expect_false(sp$nested_lrt)
  expect_false(sp$exponentiate)   # identity link
  expect_false(sp$standardise_refit)
})

test_that("brmsfit logit: supports$exponentiate = TRUE (non-identity)", {
  fit <- .fit_brms_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})


# ---- 4. brmsfit with factor predictor: reference row --------------------

test_that("brmsfit with treatment factor: produces 1 ref + 2 non-ref rows", {
  fit <- .fit_brms_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  treat_rows <- fr$coefs[fr$coefs$parent_var == "treatment", ]
  expect_identical(nrow(treat_rows), 3L)
  expect_identical(sum(treat_rows$is_ref), 1L)
  # Ref row's estimate is NA per the validator's invariant; also pd NA.
  ref_row <- treat_rows[treat_rows$is_ref, ]
  expect_true(is.na(ref_row$estimate))
  expect_true(is.na(ref_row$pd))
})


# ---- 5. brmsfit: posterior_engine metadata in info$extras ----------------

test_that("brmsfit: info$extras$posterior_engine = 'brmsfit'", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$posterior_engine, "brmsfit")
})


# ---- 6. stanreg (rstanarm): equivalent test battery ---------------------

test_that("as_regression_frame.stanreg produces a schema-valid frame", {
  fit <- .fit_rstanarm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("stanreg: info$class = 'stanreg'; ci_method = posterior_quantile", {
  fit <- .fit_rstanarm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "stanreg")
  expect_identical(fr$info$ci_method, "posterior_quantile")
})

test_that("stanreg: coefs$p_value all NA; pd in [0.5, 1] on non-ref rows", {
  fit <- .fit_rstanarm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(all(is.na(fr$coefs$p_value)))
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_true(all(b_rows$pd >= 0.5 - 1e-9))
  expect_true(all(b_rows$pd <= 1.0 + 1e-9))
})


# ---- 7. Oracle: parameters::model_parameters() --------------------------

test_that("brmsfit coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")

  # parameters::model_parameters() returns the posterior median, MAD-SD,
  # and ETI by default for brmsfit. We pin centrality / ci_method
  # explicitly per the audit discipline.
  oracle <- parameters::model_parameters(
    fit,
    centrality = "median",
    ci         = 0.95,
    ci_method  = "eti",
    test       = NULL,
    effects    = "fixed"
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  # Normalise oracle Parameter names. parameters::model_parameters()
  # for brmsfit returns the draws-side names (`b_Intercept`,
  # `b_Days`, ...). Strip the `b_` prefix and rewrite `b_Intercept`
  # to `(Intercept)` so the intersection on a meaningful set of
  # names always succeeds.
  oracle_terms <- ifelse(oracle$Parameter == "b_Intercept",
                         "(Intercept)",
                         sub("^b_", "", oracle$Parameter))
  # Drop the distributional / sigma rows on the oracle side so the
  # intersection is restricted to true fixed effects.
  shared <- intersect(b_rows$term, oracle_terms)
  expect_gt(length(shared), 0L)
  for (nm in shared) {
    spicy_row  <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle_terms == nm, ]
    oracle_est <- oracle_row$Median %||% oracle_row$Coefficient
    # Posterior comparisons across packages have larger natural
    # tolerance because the underlying draws differ (RNG seed, sample
    # size, package-specific scaling of priors). 1e-3 reflects the
    # audit-discipline guidance in design doc section 7 ("tolerance
    # 1e-3 for posterior summaries").
    expect_equal(spicy_row$estimate, oracle_est,
                 tolerance = 1e-3,
                 info = paste("oracle estimate mismatch on term:", nm))
  }
})
