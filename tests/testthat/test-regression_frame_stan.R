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
# Fits are intentionally tiny (1 chain, 400 iterations) and
# `set.seed()` makes the draws deterministic for assertion stability.
# Each test still asks for its own fit -- holding brmsfit objects in
# memory across a whole file is what would strain the memory budget --
# but the fit comes back from a DISK cache after the first sampling
# run (helper-stan-cache.R), which is neither held in memory nor paid
# for twice. Measured on this file: 1158 s before the cache, and 32 s on
# every run once the entries exist. A run that has to fill them costs
# roughly one sampling pass per fixture on top.
# ---------------------------------------------------------------------------

# ---- Fast-fit helpers ------------------------------------------------------

# Stan model compilation is unreliable on GitHub Actions runners
# (rstan / StanHeaders / C++ toolchain version drift causes random
# compilation failures unrelated to spicy's frame schema). We skip
# the brms / rstanarm fits on CI; local development still runs them
# (every developer has a working Stan toolchain when they install
# brms / rstanarm). CRAN runs each example separately and provides
# Stan, so the help-page examples remain useful.
#
# The fits themselves come through .stan_cached_fit() (see
# helper-stan-cache.R): sampled once, then read back from a
# source-checkout-only disk cache keyed on the R version, the fitting
# package's version and a hash of the model call below. Editing any of
# these bodies -- the seed included -- invalidates the entry on its own.
.fit_brms_basic <- function() {
  skip_on_ci()
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  skip_if_not_installed("lme4")
  .stan_cached_fit("brms_basic", .STAN_CACHE_PKGS_BRMS, function() {
    set.seed(1)
    brms::brm(
      Reaction ~ Days,
      data = lme4::sleepstudy,
      chains = 1,
      iter = 400,
      refresh = 0,
      silent = 2,
      backend = "rstan"
    )
  })
}

.fit_brms_factor <- function() {
  skip_on_ci()
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  skip_if_not_installed("lme4")
  .stan_cached_fit("brms_factor", .STAN_CACHE_PKGS_BRMS, function() {
    d <- lme4::sleepstudy
    d$treatment <- factor(rep(c("A", "B", "C"), length.out = nrow(d)))
    set.seed(2)
    brms::brm(
      Reaction ~ Days + treatment,
      data = d,
      chains = 1,
      iter = 400,
      refresh = 0,
      silent = 2,
      backend = "rstan"
    )
  })
}

.fit_brms_logit <- function() {
  skip_on_ci()
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  .stan_cached_fit("brms_logit", .STAN_CACHE_PKGS_BRMS, function() {
    d <- mtcars
    set.seed(3)
    brms::brm(
      am ~ mpg,
      data = d,
      family = brms::bernoulli(),
      chains = 1,
      iter = 400,
      refresh = 0,
      silent = 2,
      backend = "rstan"
    )
  })
}

.fit_rstanarm_basic <- function() {
  skip_on_ci()
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  skip_if_not_installed("lme4")
  .stan_cached_fit("rstanarm_basic", .STAN_CACHE_PKGS_RSTANARM, function() {
    set.seed(4)
    rstanarm::stan_glm(
      Reaction ~ Days,
      data = lme4::sleepstudy,
      chains = 1,
      iter = 400,
      refresh = 0
    )
  })
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

test_that("brmsfit: info$weights_kind = 'none' and random_effects = empty (canonical)", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$weights_kind, "none")
  expect_identical(fr$info$random_effects, spicy:::empty_random_effects())
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

test_that("brmsfit: estimate matches posterior median; std_error matches posterior MAD SD", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  # Use posterior::summarise_draws directly on b_* to construct the
  # expected medians / MAD SDs (ROS ch. 5 pairing), then compare
  # row-by-row.
  draws <- posterior::as_draws_array(fit)
  b_vars <- grep("^b_", posterior::variables(draws), value = TRUE)
  sm <- posterior::summarise_draws(
    posterior::subset_draws(draws, variable = b_vars),
    "median",
    "mad"
  )
  # Match by stripping the b_ prefix on summary side.
  human <- ifelse(
    sm$variable == "b_Intercept",
    "(Intercept)",
    sub("^b_", "", sm$variable)
  )
  for (i in seq_along(human)) {
    nm <- human[i]
    row <- fr$coefs[fr$coefs$term == nm & !fr$coefs$is_ref, ]
    expect_equal(
      row$estimate,
      sm$median[i],
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
    expect_equal(
      row$std_error,
      sm$mad[i],
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})


# ---- 3. brmsfit: family-aware title_prefix and supports ------------------

test_that("brmsfit gaussian: title_prefix names linear + brmsfit engine", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "linear", fixed = TRUE)
  expect_match(fr$info$extras$title_prefix, "brmsfit", fixed = TRUE)
})

test_that("brmsfit logit: family = bernoulli/logit; title decorates with logistic", {
  fit <- .fit_brms_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "bernoulli")
  expect_identical(fr$info$family$link, "logit")
  expect_match(fr$info$extras$title_prefix, "logistic", fixed = TRUE)
})

test_that("brmsfit: supports flags reflect Bayesian conventions", {
  fit <- .fit_brms_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sp <- fr$info$supports
  # Draws-native AME (finding M2 resolved): posterior median / MAD SD /
  # credible interval of the per-draw avg_slopes(); the empty-column
  # bug that motivated the old refusal is covered by the oracle tests
  # in test-stan_bayes_gates_re.R.
  expect_true(sp$ame)
  expect_false(sp$partial_effect_size)
  expect_false(sp$classical_r2)
  expect_false(sp$nested_lrt)
  expect_false(sp$exponentiate) # identity link
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
    ci = 0.95,
    ci_method = "eti",
    test = NULL,
    effects = "fixed"
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  # Normalise oracle Parameter names. parameters::model_parameters()
  # for brmsfit returns the draws-side names (`b_Intercept`,
  # `b_Days`, ...). Strip the `b_` prefix and rewrite `b_Intercept`
  # to `(Intercept)` so the intersection on a meaningful set of
  # names always succeeds.
  oracle_terms <- ifelse(
    oracle$Parameter == "b_Intercept",
    "(Intercept)",
    sub("^b_", "", oracle$Parameter)
  )
  # Drop the distributional / sigma rows on the oracle side so the
  # intersection is restricted to true fixed effects.
  shared <- intersect(b_rows$term, oracle_terms)
  n_checked <- 0L
  for (nm in shared) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle_terms == nm, ]
    # Both lookups must hit exactly one row: an unmatched term
    # would otherwise compare a zero-row frame and the counter
    # below would never see it.
    expect_identical(nrow(oracle_row), 1L, info = nm)
    expect_identical(nrow(spicy_row), 1L, info = nm)
    oracle_est <- oracle_row$Median %||% oracle_row$Coefficient
    # Posterior comparisons across packages have larger natural
    # tolerance because the underlying draws differ (RNG seed, sample
    # size, package-specific scaling of priors). 1e-3 reflects the
    # audit-discipline guidance in design doc section 7 ("tolerance
    # 1e-3 for posterior summaries").
    expect_equal(
      spicy_row$estimate,
      oracle_est,
      tolerance = 1e-3,
      info = paste("oracle estimate mismatch on term:", nm)
    )
    n_checked <- n_checked + 1L
  }
  # Counted BY the loop, not before it: an empty intersection and a
  # neutered body have to look different to this guard.
  expect_oracle_covered(n_checked, length(shared))
})


## ---- Phase 3 matrix (lot T2) ----------------------------------------------

# Phase 3 matrix: rd-vcov-classes:registry-brmsfit
# Local-only like every brms fixture (skip_on_ci inside the fit helpers):
# Stan compilation is unreliable on CI runners.
test_that("brmsfit: multilevel fits render an RE block, single-level fits do not", {
  fit_plain <- .fit_brms_basic()
  out_plain <- paste(
    capture.output(print(suppressWarnings(table_regression(fit_plain)))),
    collapse = "\n"
  )
  expect_false(grepl("Random effects:", out_plain, fixed = TRUE))
  # AME is draws-native per the registry: the capability is advertised.
  expect_true(isTRUE(
    suppressWarnings(as_regression_frame(fit_plain))$info$supports$ame
  ))

  skip_on_ci()
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  skip_if_not_installed("lme4")
  # Cached like the four named fixtures: this is the slowest fit in the
  # file, and it was the only one still resampled on every run.
  fit_re <- .stan_cached_fit(
    "brms_multilevel",
    .STAN_CACHE_PKGS_BRMS,
    function() {
      set.seed(5)
      brms::brm(
        Reaction ~ Days + (1 | Subject),
        data = lme4::sleepstudy,
        chains = 1,
        iter = 400,
        refresh = 0,
        silent = 2,
        backend = "rstan"
      )
    }
  )
  out_re <- paste(
    capture.output(print(suppressWarnings(table_regression(fit_re)))),
    collapse = "\n"
  )
  expect_match(out_re, "Random effects:", fixed = TRUE)
  expect_match(out_re, "Subject (Intercept)", fixed = TRUE)
  expect_match(out_re, "Random effects (MCMC).", fixed = TRUE)
  fr_re <- suppressWarnings(as_regression_frame(fit_re))
  expect_true(isTRUE(fr_re$info$supports$ame))
  # `info$n_groups` is a named INTEGER VECTOR, the schema's shape and
  # the one lme4::ngrps() gives the merMod builder. brms::ngrps()
  # returns a list and the builder used to pass it straight through.
  expect_type(fr_re$info$n_groups, "integer")
  expect_identical(fr_re$info$n_groups[["Subject"]], 18L)
})


## ---- Delta review D3: draws-native AME honours the fit's prior weights ----

# Local-only like the fixtures above (skip_on_ci): Stan sampling is
# unreliable on CI runners.
test_that("stanreg: weighted fit's AME equals avg_slopes(wts = ) (draws path)", {
  skip_on_ci()
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("collapse")
  d <- mtcars
  set.seed(11)
  d$w <- runif(nrow(d), 0.5, 2)
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt + hp,
    data = d,
    family = binomial(),
    weights = d$w,
    seed = 11,
    chains = 1,
    iter = 500,
    refresh = 0
  ))
  # The helper extracts the prior weights for stanreg fits...
  expect_equal(spicy:::.spicy_ame_fit_wts(fit), d$w, tolerance = 1e-12)
  # ...and the draws-native AME table now passes them through, so the
  # point estimate equals the wts-weighted avg_slopes() median exactly.
  ame <- spicy:::.compute_bayes_ame_table(fit, ci_level = 0.95)
  orc <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(fit, conf_level = 0.95, wts = d$w)
  ))
  idx <- match(c("wt", "hp"), ame$term)
  expect_equal(
    ame$estimate[idx],
    orc$estimate[match(c("wt", "hp"), orc$term)],
    tolerance = 1e-8
  )
  # Discriminating: the weighted average differs from the unweighted one.
  orc_u <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(fit, conf_level = 0.95)
  ))
  expect_gt(
    abs(
      orc$estimate[orc$term == "wt"] - orc_u$estimate[orc_u$term == "wt"]
    ),
    1e-6
  )
})


# ---- Label overrides (register 55) ---------------------------------------

test_that("brmsfit accepts `labels =` keyed on coefficient names", {
  # `stats::terms()` raises on the fit itself; the label validator used
  # to call it unguarded and died before applying anything.
  # Local-only, like every other brms fixture here (skip_on_ci).
  fit <- .fit_brms_basic()
  out <- suppressWarnings(
    table_regression(fit, labels = c(Days = "Days of deprivation"))
  )
  expect_true("Days of deprivation" %in% out$Variable)
  expect_false("Days" %in% out$Variable)
})

test_that("brmsfit accepts `labels =` keyed on a factor term", {
  # The factor header is the one label a coefficient key cannot reach.
  # brmsfit carries its terms on the unwrapped brmsformula: the
  # validator must read them through `.spicy_get_terms()`, like the
  # renderer that displays the `treatment:` header. Local-only.
  fit <- .fit_brms_factor()
  out <- suppressWarnings(
    table_regression(fit, labels = c(treatment = "Arm"))
  )
  expect_true("Arm:" %in% out$Variable)
  expect_false("treatment:" %in% out$Variable)
})


# ---- exponentiate on a non-identity link ---------------------------------

test_that("brmsfit logit: `exponentiate` maps the draws, not the summary", {
  # The link gate runs before any draws work and passes for a logit
  # fit, and the transform is DRAWS-NATIVE: the reported estimate is
  # the median of exp(draws), which for a monotone map is exp of the
  # median of the draws. Pinning that identity is what makes the odds
  # ratio the same number a reader would get from the log-odds column.
  fit <- .fit_brms_logit()
  fr_plain <- suppressWarnings(as_regression_frame(fit, model_id = "M1"))
  fr_exp <- suppressWarnings(
    as_regression_frame(fit, model_id = "M1", exponentiate = TRUE)
  )
  expect_true(isTRUE(fr_exp$info$extras$exp_applied))
  expect_false(isTRUE(fr_plain$info$extras$exp_applied))
  expect_equal(
    fr_exp$coefs$estimate,
    exp(fr_plain$coefs$estimate),
    tolerance = 1e-8
  )
  # The interval travels with it: exponentiating a posterior quantile
  # is the quantile of the exponentiated posterior.
  expect_equal(
    fr_exp$coefs$ci_lower,
    exp(fr_plain$coefs$ci_lower),
    tolerance = 1e-8
  )
  expect_true(all(fr_exp$coefs$estimate > 0))
})


# ---- correlated random slopes --------------------------------------------

test_that("brmsfit: a correlated random slope renders its correlation row", {
  skip_on_ci()
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  skip_if_not_installed("lme4")
  # `(Days | Subject)` is the only fixture that samples a `cor_*`
  # parameter, and the correlation between two random terms is a row of
  # the RE block in its own right -- read off the `cor_<group>__<t1>__<t2>`
  # draws, not derived from the two SDs. Cached like every other fixture.
  fit <- .stan_cached_fit(
    "brms_corr",
    .STAN_CACHE_PKGS_BRMS,
    function() {
      set.seed(6)
      brms::brm(
        Reaction ~ Days + (Days | Subject),
        data = lme4::sleepstudy,
        chains = 1,
        iter = 400,
        refresh = 0,
        silent = 2,
        backend = "rstan"
      )
    }
  )
  re <- suppressWarnings(
    as_regression_frame(fit)
  )$info$random_effects$variance_components
  # Two SD rows and one correlation row for the group, plus Residual.
  sd_rows <- re[!re$is_correlation & re$group == "Subject", ]
  expect_setequal(sd_rows$term, c("(Intercept)", "Days"))
  corr_rows <- re[re$is_correlation, ]
  expect_identical(nrow(corr_rows), 1L)
  expect_identical(corr_rows$group, "Subject")
  # The two terms are named in the reader's words and joined by the
  # multiplication sign, and "Intercept" is spelled the way every other
  # row of the table spells it.
  expect_identical(corr_rows$term, "(Intercept) × Days")
  # A correlation carries no variance / SD, and its interval lies
  # inside [-1, 1].
  expect_true(is.na(corr_rows$variance))
  expect_true(is.na(corr_rows$sd))
  expect_true(corr_rows$corr >= -1 && corr_rows$corr <= 1)
  expect_true(corr_rows$ci_lower >= -1 && corr_rows$ci_upper <= 1)
})


# ---- diagnostics that only a failing computation can show ----------------

test_that("a failed PSIS-LOO / WAIC is announced, never silently absent", {
  # Pre-1.0 policy: a fit statistic the caller ASKED for and that could
  # not be computed must say what failed and what is missing, rather
  # than leaving a row out of the table with no explanation. The
  # underlying error text travels with the message so the reader can
  # act on it.
  fit <- .fit_brms_basic()
  skip_if_not_installed("loo")
  warns <- list()
  fr <- with_mocked_bindings(
    withCallingHandlers(
      as_regression_frame(
        fit,
        model_id = "M1",
        show_fit_stats = c("elpd_loo", "looic", "waic")
      ),
      warning = function(w) {
        warns[[length(warns) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    ),
    loo = function(...) stop("no importance sampling here"),
    waic = function(...) stop("no pointwise likelihood here"),
    .package = "loo"
  )
  msgs <- vapply(warns, conditionMessage, character(1))
  classes <- vapply(
    warns,
    function(w) inherits(w, "spicy_bayes_diagnostics"),
    logical(1)
  )
  expect_true(all(classes))
  expect_true(any(grepl(
    "PSIS-LOO failed for outcome Reaction (no importance sampling here)",
    msgs,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "WAIC failed for outcome Reaction (no pointwise likelihood here)",
    msgs,
    fixed = TRUE
  )))
  expect_true(any(grepl("rows are omitted", msgs, fixed = TRUE)))
  # And the statistics themselves are absent, not zero.
  expect_true(is.na(fr$info$fit_stats$elpd_loo))
  expect_true(is.na(fr$info$fit_stats$looic))
  expect_true(is.na(fr$info$fit_stats$waic))
})


test_that("a low E-BFMI is flagged with the value that failed the guard", {
  # The energy diagnostic (Betancourt 2017) is the one convergence
  # guard the sampled fixtures never trip, so the value comes from
  # rstan's own extractor with a failing posterior standing in. What is
  # pinned is the guard's arithmetic: the MINIMUM E-BFMI is the number
  # reported, and only when it is below 0.2.
  fit <- .fit_brms_basic()
  skip_if_not_installed("rstan")
  flagged <- with_mocked_bindings(
    spicy:::.stan_convergence_diagnostics(fit, "brmsfit"),
    get_bfmi = function(...) c(0.12, 0.44),
    .package = "rstan"
  )
  expect_equal(flagged$bfmi, 0.12)
  expect_match(
    spicy:::.stan_convergence_text(flagged, "."),
    "min E-BFMI = 0.12 (target > 0.2)",
    fixed = TRUE
  )
  # Above the bar the field stays NA: a guard that passed adds no
  # sentence to a publication table.
  clean <- with_mocked_bindings(
    spicy:::.stan_convergence_diagnostics(fit, "brmsfit"),
    get_bfmi = function(...) c(0.9, 0.8),
    .package = "rstan"
  )
  expect_true(is.na(clean$bfmi))
  expect_false(grepl(
    "E-BFMI",
    spicy:::.stan_convergence_text(clean, "."),
    fixed = TRUE
  ))
})
