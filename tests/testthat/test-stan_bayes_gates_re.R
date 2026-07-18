# Bayesian gates + multilevel random-effects block (findings a, b, c
# of the Bayesian-vignette reconnaissance). Fits are tiny and seeded;
# skipped where rstanarm is not installed (CI convention of
# test-regression_frame_stan.R).

.tiny_stan_glmer <- function() {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  sh <- sochealth
  suppressWarnings(rstanarm::stan_glmer(
    smoking ~ age + (1 | region), data = sh, family = binomial(),
    iter = 500, chains = 1, refresh = 0, seed = 42
  ))
}


test_that("p_adjust is refused for all-Bayesian tables", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 400, chains = 1, refresh = 0, seed = 1
  ))
  expect_error(
    table_regression(fit, p_adjust = "holm"),
    class = "spicy_invalid_input"
  )
  # Likelihood-based fit stats are refused too (finding b) ...
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "aic")),
    class = "spicy_invalid_input"
  )
  # ... and the class-aware DEFAULT no longer injects them: the
  # default table renders with n only.
  out <- paste(capture.output(print(table_regression(fit))),
               collapse = "\n")
  expect_false(grepl("AIC", out, fixed = TRUE))
  expect_match(out, "posterior MAD SD", fixed = TRUE)
  # Mixed-class tables keep the frequentist columns adjustable/filled.
  gf <- glm(am ~ wt, data = mtcars, family = binomial)
  out2 <- paste(capture.output(print(table_regression(
    list(G = gf, B = fit), show_columns = c("b", "ci")
  ))), collapse = "\n")
  expect_match(out2, "AIC", fixed = TRUE)
})


test_that("stan_glmer renders an RE block, not per-group b[] rows", {
  fit <- .tiny_stan_glmer()
  out <- paste(capture.output(print(table_regression(fit))),
               collapse = "\n")
  expect_false(grepl("b[(Intercept)", out, fixed = TRUE))
  expect_match(out, "Random effects:", fixed = TRUE)
  expect_match(out, "region (Intercept)", fixed = TRUE)
  expect_match(out, "Random effects (MCMC).", fixed = TRUE)
  # No chi-bar-squared LRT line: there is no likelihood-ratio test
  # for a posterior.
  expect_false(grepl("LR test", out, fixed = TRUE))

  # Oracle: the sigma row's estimate and CrI are the posterior median
  # and quantiles of sqrt(Sigma) from the draws themselves.
  dr <- posterior::as_draws_matrix(fit)
  v <- dr[, "Sigma[region:(Intercept),(Intercept)]"]
  sd_draws <- sqrt(pmax(v, 0))
  td <- broom::tidy(table_regression(fit))
  row <- td[grepl("^re::region", td$term), ]
  expect_equal(row$estimate, median(sd_draws), tolerance = 1e-10)
  expect_equal(row$conf.low,
               unname(quantile(sd_draws, 0.025)), tolerance = 1e-10)
  expect_equal(row$conf.high,
               unname(quantile(sd_draws, 0.975)), tolerance = 1e-10)

  # n_groups feeds the N (region) fit-stat machinery.
  fr <- as_regression_frame(fit)
  expect_identical(fr$info$n_groups$region, 6L)
})


test_that("the pd token renders the probability of direction (finding d)", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 500, chains = 1, refresh = 0, seed = 1
  ))
  out <- paste(capture.output(print(table_regression(
    fit, show_columns = c("b", "ci", "pd")
  ))), collapse = "\n")
  expect_match(out, "pd", fixed = TRUE)
  dr <- posterior::as_draws_matrix(fit)[, "wt"]
  oracle <- max(mean(dr > 0), mean(dr < 0))
  fr <- as_regression_frame(fit)
  expect_equal(fr$coefs$pd[fr$coefs$term == "wt"], oracle,
               tolerance = 1e-10)
  # Frequentist fits refuse the token.
  expect_error(
    table_regression(stats::lm(mpg ~ wt, mtcars),
                     show_columns = c("b", "pd")),
    class = "spicy_invalid_input"
  )
})


test_that("all-Bayesian defaults drop p and label the interval CrI", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 400, chains = 1, refresh = 0, seed = 1
  ))
  out <- paste(capture.output(print(table_regression(fit))),
               collapse = "\n")
  expect_match(out, "95% CrI", fixed = TRUE)
  # No p column in the header (the footer's "p" in "Posterior" aside).
  header <- capture.output(print(table_regression(fit)))[3]
  expect_false(grepl("\bp\b", header))
  # Explicit atomic p refused; the all_b preset expands without it.
  expect_error(
    table_regression(fit, show_columns = c("b", "ci", "p")),
    class = "spicy_invalid_input"
  )
  h2 <- capture.output(print(table_regression(fit,
                                              show_columns = "all_b")))[3]
  expect_false(grepl("\bp\b", h2))
  # Mixed-class tables keep the shared CI label and the p column.
  gf <- glm(am ~ wt, data = mtcars, family = binomial)
  outm <- paste(capture.output(print(table_regression(
    list(G = gf, B = fit), show_columns = c("b", "ci", "p")
  ))), collapse = "\n")
  expect_match(outm, "95% CI", fixed = TRUE)
  expect_false(grepl("95% CrI", outm, fixed = TRUE))
})


test_that("Bayesian fit statistics: r2_bayes default, loo pair opt-in", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  skip_if_not_installed("loo")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 400, chains = 1, refresh = 0, seed = 1
  ))
  out <- paste(capture.output(print(table_regression(fit))),
               collapse = "\n")
  expect_match(out, "R² (Bayes)", fixed = TRUE)
  fr <- as_regression_frame(fit)
  expect_equal(fr$info$fit_stats$r2_bayes,
               median(rstanarm::bayes_R2(fit)), tolerance = 1e-10)
  l <- suppressWarnings(loo::loo(fit))
  fr2 <- as_regression_frame(fit,
    show_fit_stats = c("nobs", "elpd_loo", "looic"))
  expect_equal(fr2$info$fit_stats$elpd_loo,
               unname(l$estimates["elpd_loo", "Estimate"]),
               tolerance = 1e-6)
  expect_equal(fr2$info$fit_stats$looic,
               unname(l$estimates["looic", "Estimate"]),
               tolerance = 1e-6)
  # Refused outside the Bayesian world; mixed default stays legal.
  expect_error(
    table_regression(stats::lm(mpg ~ wt, mtcars),
                     show_fit_stats = c("nobs", "r2_bayes")),
    class = "spicy_invalid_input"
  )
  gf <- glm(am ~ wt, data = mtcars, family = binomial)
  expect_no_error(table_regression(list(G = gf, B = fit),
                                   show_columns = c("b", "ci")))
})


test_that("waic token and the PSIS-LOO SE footer render", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 400, chains = 1, refresh = 0, seed = 1
  ))
  out <- paste(capture.output(print(table_regression(
    fit, show_fit_stats = c("nobs", "elpd_loo", "waic")
  ))), collapse = "\n")
  expect_match(out, "WAIC", fixed = TRUE)
  expect_match(out, "Predictive accuracy by PSIS-LOO / WAIC; SE(ELPD) = ",
               fixed = TRUE)
  expect_match(out, "SE(WAIC) = ", fixed = TRUE)
  fr <- as_regression_frame(fit, show_fit_stats = c("nobs", "waic"))
  w <- suppressWarnings(loo::waic(fit))
  expect_equal(fr$info$fit_stats$waic,
               unname(w$estimates["waic", "Estimate"]), tolerance = 1e-6)
})


test_that("diagnostic columns render and the convergence guard fires", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  # A deliberately under-sampled fit: 1 chain x 400 iter -> 200
  # retained draws, ESS below the 400 target -> guard footer + warning.
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 400, chains = 1, refresh = 0, seed = 1
  ))
  w <- NULL
  out <- withCallingHandlers(
    paste(capture.output(print(table_regression(
      fit, show_columns = c("b", "ci", "rhat", "ess_bulk", "ess_tail")
    ))), collapse = "\n"),
    warning = function(ww) {
      w <<- conditionMessage(ww)
      invokeRestart("muffleWarning")
    }
  )
  expect_match(out, "R-hat", fixed = TRUE)
  expect_match(out, "ESS (bulk)", fixed = TRUE)
  expect_match(out, "Sampler diagnostics:", fixed = TRUE)
  expect_match(out, "min ESS", fixed = TRUE)
  expect_match(w, "sampler problems")
  # Values are the summarise_draws diagnostics, ESS as integers.
  sm <- posterior::summarise_draws(
    posterior::as_draws_array(fit), "rhat", "ess_bulk", "ess_tail")
  expect_match(out,
               format(as.integer(round(sm$ess_bulk[sm$variable == "wt"]))),
               fixed = TRUE)
})


# ---- 2026-07 expert-audit lot ----------------------------------------------

test_that("the Bayesian coefs schema is defined once (CI-runnable)", {
  # The factor-predictor crash: .stan_reference_rows() emitted 15
  # columns while .stan_coefs() emitted 18, and the rbind() stopped
  # every stanreg/brmsfit with a factor covariate. Guard the schema
  # without needing a Stan fit.
  empty <- spicy:::.empty_coefs_frame_with_pd(TRUE)
  expect_identical(names(empty), spicy:::.stan_coefs_schema)
  expect_true(all(c("pd", "rhat", "ess_bulk", "ess_tail")
                  %in% spicy:::.stan_coefs_schema))
})


test_that("uppercase IC tokens raise the 0.13 migration error (CI-runnable)", {
  fit <- lm(mpg ~ wt, mtcars)
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "AIC")),
    "switched the information criteria to lowercase",
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, show_fit_stats = c("BIC", "AICc")),
    class = "spicy_invalid_input"
  )
})


test_that("ci_method = 'hdi' is refused for frequentist fits (CI-runnable)", {
  expect_error(
    table_regression(lm(mpg ~ wt, mtcars), ci_method = "hdi"),
    "defined only for Bayesian fits",
    class = "spicy_invalid_input"
  )
})


test_that("a factor predictor tabulates: schema, MAD SD, reference row", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  fit <- suppressWarnings(rstanarm::stan_glm(
    mpg ~ wt + cyl_f, data = d,
    iter = 500, chains = 1, refresh = 0, seed = 42
  ))
  fr <- as_regression_frame(fit)
  expect_identical(names(fr$coefs), spicy:::.stan_coefs_schema)
  # Reference row present, diagnostics structurally NA.
  ref <- fr$coefs[fr$coefs$is_ref, ]
  expect_identical(nrow(ref), 1L)
  expect_true(is.na(ref$rhat) && is.na(ref$ess_bulk) && is.na(ref$pd))
  # SE = posterior MAD SD (ROS ch. 5; rstanarm print), not the SD.
  drm <- posterior::as_draws_matrix(fit)
  expect_equal(fr$coefs$std_error[fr$coefs$term == "wt"],
               unname(stats::mad(drm[, "wt"])), tolerance = 1e-10)
  expect_equal(fr$coefs$estimate[fr$coefs$term == "wt"],
               unname(stats::median(drm[, "wt"])), tolerance = 1e-10)
  # End-to-end render (was the crash) + footer label.
  out <- paste(capture.output(print(suppressWarnings(
    table_regression(fit)))), collapse = "\n")
  expect_match(out, "(ref.)", fixed = TRUE)
  expect_match(out, "posterior MAD SD", fixed = TRUE)
})


test_that("ci_method = 'hdi' computes the Kruschke HDI, header included", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 500, chains = 1, refresh = 0, seed = 1
  ))
  drm <- posterior::as_draws_matrix(fit)
  fr <- as_regression_frame(fit, ci_method = "hdi")
  expect_identical(fr$info$ci_method, "posterior_hdi")
  oracle <- spicy:::.hdi_interval(as.numeric(drm[, "wt"]), 0.95)
  i <- fr$coefs$term == "wt"
  expect_equal(fr$coefs$ci_lower[i], oracle[1], tolerance = 1e-10)
  expect_equal(fr$coefs$ci_upper[i], oracle[2], tolerance = 1e-10)
  out <- paste(capture.output(print(suppressWarnings(
    table_regression(fit, ci_method = "hdi")))), collapse = "\n")
  expect_match(out, "95% HDI", fixed = TRUE)
  # HDI is not transformation-invariant: under exponentiate the
  # interval is recomputed on the exponentiated draws.
  fr_exp <- suppressWarnings(as_regression_frame(
    fit, ci_method = "hdi", exponentiate = TRUE))
  oracle_exp <- spicy:::.hdi_interval(exp(as.numeric(drm[, "wt"])), 0.95)
  expect_equal(fr_exp$coefs$ci_lower[i], oracle_exp[1], tolerance = 1e-10)
  expect_equal(fr_exp$coefs$ci_upper[i], oracle_exp[2], tolerance = 1e-10)
  # ... and differs from exp of the link-scale HDI bounds.
  expect_false(isTRUE(all.equal(oracle_exp, exp(oracle))))
  # profile / boot_percentile are refused with the Bayesian pointer.
  expect_error(
    table_regression(fit, ci_method = "profile"),
    "not defined for Bayesian fits",
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, ci_method = "boot_percentile"),
    class = "spicy_invalid_input"
  )
})


test_that("exponentiate is draws-native: SE = MAD SD of exp(draws)", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 500, chains = 1, refresh = 0, seed = 1
  ))
  drm <- posterior::as_draws_matrix(fit)
  fr <- suppressWarnings(as_regression_frame(fit, exponentiate = TRUE))
  expect_true(isTRUE(fr$info$extras$exp_applied))
  i <- fr$coefs$term == "wt"
  expect_equal(fr$coefs$std_error[i],
               unname(stats::mad(exp(drm[, "wt"]))), tolerance = 1e-10)
  # Estimate and ETI bounds are exp() of the link-scale values.
  expect_equal(fr$coefs$estimate[i],
               exp(unname(stats::median(drm[, "wt"]))), tolerance = 1e-10)
  out <- paste(capture.output(print(suppressWarnings(table_regression(
    fit, exponentiate = TRUE, show_columns = c("b", "se", "ci")
  )))), collapse = "\n")
  expect_match(out, "posterior MAD SD of the exponentiated draws",
               fixed = TRUE)
  expect_false(grepl("delta method", out, fixed = TRUE))
})


test_that("non-MCMC algorithms are refused with a refit hint", {
  skip_if_not_installed("rstanarm")
  fit_vb <- suppressWarnings(rstanarm::stan_glm(
    mpg ~ wt, data = mtcars, algorithm = "meanfield",
    refresh = 0, seed = 1
  ))
  expect_error(
    table_regression(fit_vb),
    "meanfield",
    class = "spicy_unsupported"
  )
})


test_that("robust vcov on a Bayesian fit gets the principled refusal", {
  skip_if_not_installed("rstanarm")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 400, chains = 1, refresh = 0, seed = 1
  ))
  expect_error(
    table_regression(fit, vcov = "HC3"),
    "no sandwich analogue",
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(fit, vcov = "CR2", cluster = ~cyl),
    "no sandwich analogue",
    class = "spicy_unsupported_vcov"
  )
})


test_that("mixed table: pd dashes, r2_bayes fills per model, CrI footer", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 400, chains = 1, refresh = 0, seed = 1
  ))
  gf <- glm(am ~ wt, data = mtcars, family = binomial)
  out <- paste(capture.output(print(suppressWarnings(table_regression(
    list(G = gf, B = fit), show_columns = c("b", "ci", "p", "pd")
  )))), collapse = "\n")
  # pd allowed with >= 1 Bayesian model; the disclosure line names the
  # Bayesian model's interval; r2_bayes joins the default fit stats.
  expect_match(out, "probability of direction", fixed = TRUE)
  expect_match(out, "equal-tailed posterior credible interval",
               fixed = TRUE)
  expect_match(out, "R² (Bayes)", fixed = TRUE)
  # Bayesian-only fit stats stay refused when NO model is Bayesian ...
  expect_error(
    table_regression(gf, show_fit_stats = c("nobs", "r2_bayes")),
    class = "spicy_invalid_input"
  )
  # ... and per-parameter diagnostics stay all-Bayesian with the
  # guard pointer.
  expect_error(
    table_regression(list(G = gf, B = fit),
                     show_columns = c("b", "rhat")),
    "every model is Bayesian",
    class = "spicy_invalid_input"
  )
})


test_that("Pareto-k and p_waic diagnostics are surfaced, not silenced", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  skip_if_not_installed("loo")
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  # stan_glmer, not stan_lmer: stan_lmer() rewrites its call to
  # stan_glmer() and evaluates it in the CALLER's environment, which
  # fails when rstanarm is not attached (namespace-qualified calls).
  fit <- suppressWarnings(rstanarm::stan_glmer(
    Reaction ~ Days + (Days | Subject), data = sleepstudy,
    iter = 1000, chains = 2, refresh = 0, seed = 11
  ))
  w_classes <- list()
  out <- withCallingHandlers(
    paste(capture.output(print(table_regression(
      fit, show_fit_stats = c("nobs", "elpd_loo", "waic")
    ))), collapse = "\n"),
    warning = function(w) {
      w_classes[[length(w_classes) + 1L]] <<- class(w)
      invokeRestart("muffleWarning")
    }
  )
  # This seeded fit has known-bad Pareto k and p_waic observations:
  # the footer discloses both, alongside both standard errors.
  expect_match(out, "SE(ELPD) = ", fixed = TRUE)
  expect_match(out, "SE(WAIC) = ", fixed = TRUE)
  expect_match(out, "PSIS-LOO unreliable for", fixed = TRUE)
  expect_match(out, "p_waic > 0.4", fixed = TRUE)
  # Every guard warning carries the nested class pair, so
  # spicy_bayes_diagnostics mutes selectively while spicy_caveat
  # handlers still catch it.
  expect_true(length(w_classes) > 0L)
  for (cls in w_classes) {
    if ("spicy_bayes_diagnostics" %in% cls) {
      expect_true("spicy_caveat" %in% cls)
      expect_true("spicy_warning" %in% cls)
    }
  }
  expect_true(any(vapply(w_classes, function(cls) {
    "spicy_bayes_diagnostics" %in% cls
  }, logical(1))))
})


# ---- 2026-07 pre-commit review round 2 --------------------------------------

test_that("brms family spellings map to the shared exp machinery (CI-runnable)", {
  expect_true(spicy:::.exp_gate_allowed("bernoulli", "cloglog"))
  expect_identical(spicy:::spicy_glm_exp_header("bernoulli", "logit"),
                   "OR")
  expect_identical(spicy:::spicy_glm_exp_header("bernoulli", "cloglog"),
                   "HR")
  expect_identical(spicy:::spicy_glm_exp_header("bernoulli", "log"),
                   "RR")
  expect_identical(spicy:::spicy_glm_exp_header("negbinomial", "log"),
                   "IRR")
  expect_identical(spicy:::spicy_glm_exp_header("geometric", "log"),
                   "IRR")
  expect_identical(spicy:::spicy_glm_exp_header("gamma", "log"),
                   "MR")
})


test_that("Bayesian titles are link-aware (CI-runnable)", {
  expect_identical(
    spicy:::.stan_family_title(list(family = "binomial", link = "probit")),
    "probit")
  expect_identical(
    spicy:::.stan_family_title(list(family = "bernoulli", link = "logit")),
    "logistic")
  expect_identical(
    spicy:::.stan_family_title(list(family = "binomial", link = "cloglog")),
    "complementary log-log")
})


test_that("loo tokens without the loo package are refused upfront (CI-runnable)", {
  fake_bayes <- structure(list(), class = c("stanreg", "glm", "lm"))
  testthat::local_mocked_bindings(
    spicy_pkg_available = function(pkg) !identical(pkg, "loo"),
    .package = "spicy"
  )
  expect_error(
    spicy:::validate_class_appropriate_tokens(
      list(fake_bayes), character(0), c("nobs", "elpd_loo")),
    "need the loo package",
    class = "spicy_missing_pkg"
  )
  # r2_bayes does not need loo and still passes this gate.
  expect_no_error(
    spicy:::validate_class_appropriate_tokens(
      list(fake_bayes), character(0), c("nobs", "r2_bayes"))
  )
})


test_that("ci_method = 'hdi' reaches the random-effects block", {
  fit <- .tiny_stan_glmer()
  dr <- posterior::as_draws_matrix(fit)
  sd_draws <- sqrt(pmax(
    as.numeric(dr[, "Sigma[region:(Intercept),(Intercept)]"]), 0))
  h <- spicy:::.hdi_interval(sd_draws, 0.95)
  fr_hdi <- as_regression_frame(fit, ci_method = "hdi")
  vc <- fr_hdi$info$random_effects$variance_components
  i <- which(vc$group == "region")[1L]
  # Storage contract: bounds squared onto the variance scale; the
  # display sqrt-transforms back, so the rendered SD interval is the
  # HDI of the sigma draws exactly.
  expect_equal(vc$ci_lower[i], h[1]^2, tolerance = 1e-10)
  expect_equal(vc$ci_upper[i], h[2]^2, tolerance = 1e-10)
  # ... and differs from the equal-tailed default.
  fr_eti <- as_regression_frame(fit)
  vce <- fr_eti$info$random_effects$variance_components
  expect_false(isTRUE(all.equal(vc$ci_lower[i], vce$ci_lower[i])))
})


test_that("nested grouping factors keep their variance rows (stanreg)", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  set.seed(9)
  d <- data.frame(
    g1 = factor(rep(letters[1:4], each = 12)),
    g2 = factor(rep(1:3, times = 16)),
    x  = rnorm(48)
  )
  d$y <- rnorm(48) + as.numeric(d$g1)
  # stan_glmer, not stan_lmer (see the Pareto-k test note).
  fit <- suppressWarnings(rstanarm::stan_glmer(
    y ~ x + (1 | g1 / g2), data = d,
    iter = 400, chains = 1, refresh = 0, seed = 9
  ))
  re <- suppressWarnings(
    spicy:::.stan_random_effects(fit, "stanreg", 0.95))
  groups <- unique(re$variance_components$group)
  # lme4 names the nested factor "g2:g1": both components must be
  # present (the first-colon parser silently dropped "g2:g1").
  expect_true("g2:g1" %in% groups)
  expect_true("g1" %in% groups)
})


test_that("mixed-table loo note keeps its Model prefix", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("loo")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 400, chains = 1, refresh = 0, seed = 1
  ))
  gf <- glm(am ~ wt, data = mtcars, family = binomial)
  out <- paste(capture.output(print(suppressWarnings(table_regression(
    list(G = gf, B = fit), show_fit_stats = c("nobs", "elpd_loo")
  )))), collapse = "\n")
  expect_match(out, "Model 2: Predictive accuracy by PSIS-LOO",
               fixed = TRUE)
})


test_that("a Bayesian probit is titled probit, not logistic", {
  skip_if_not_installed("rstanarm")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(link = "probit"),
    iter = 400, chains = 1, refresh = 0, seed = 2
  ))
  out <- paste(capture.output(print(suppressWarnings(
    table_regression(fit)))), collapse = "\n")
  expect_match(out, "Bayesian probit regression", fixed = TRUE)
  expect_false(grepl("Bayesian logistic regression", out, fixed = TRUE))
})


test_that("structured outputs carry the CrI / HDI interval label", {
  skip_if_not_installed("rstanarm")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 400, chains = 1, refresh = 0, seed = 1
  ))
  st <- as_structured(suppressWarnings(table_regression(
    fit, show_columns = c("b", "ci"))))
  labs <- vapply(st$ci_pairs, function(p) p$label, character(1))
  expect_true(all(labs == "95% CrI"))
  st_h <- as_structured(suppressWarnings(table_regression(
    fit, show_columns = c("b", "ci"), ci_method = "hdi")))
  labs_h <- vapply(st_h$ci_pairs, function(p) p$label, character(1))
  expect_true(all(labs_h == "95% HDI"))
})


# ---- MCSE column (Bayesian Workflow sec. 11.6) ------------------------------

test_that("the mcse column matches posterior::mcse_median, exp included", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 500, chains = 1, refresh = 0, seed = 1
  ))
  dr <- posterior::as_draws_array(fit)
  fr <- as_regression_frame(fit)
  i <- fr$coefs$term == "wt"
  oracle <- posterior::mcse_median(
    posterior::subset_draws(dr, variable = "wt"))
  expect_equal(fr$coefs$mcse[i], unname(oracle), tolerance = 1e-10)
  # Under exponentiate the MCSE is recomputed on the exp draws (the
  # median MCSE is not transformation-invariant).
  fr_exp <- suppressWarnings(as_regression_frame(fit,
                                                 exponentiate = TRUE))
  oracle_exp <- posterior::mcse_median(
    exp(posterior::subset_draws(dr, variable = "wt")))
  expect_equal(fr_exp$coefs$mcse[i], unname(oracle_exp),
               tolerance = 1e-10)
  expect_false(isTRUE(all.equal(fr$coefs$mcse[i], fr_exp$coefs$mcse[i])))
  # Rendering: MCSE header + the footer abbreviation key.
  out <- paste(capture.output(print(suppressWarnings(table_regression(
    fit, show_columns = c("b", "ci", "mcse")
  )))), collapse = "\n")
  expect_match(out, "MCSE", fixed = TRUE)
  expect_match(out, "MCSE = Monte Carlo standard error of the posterior median",
               fixed = TRUE)
  # All-Bayesian only, same gate as rhat/ess.
  expect_error(
    table_regression(stats::lm(mpg ~ wt, mtcars),
                     show_columns = c("b", "mcse")),
    class = "spicy_invalid_input"
  )
})


# ---- Draws-native Bayesian AME (finding M2 resolved) ------------------------

test_that("Bayesian AME summarizes the avg_slopes draws exactly", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  skip_if_not_installed("marginaleffects")
  # marginaleffects' stanreg prediction path needs collapse (absent
  # from bare CI libraries, hence Suggests).
  skip_if_not_installed("collapse")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt + cyl_f, data = d, family = binomial(),
    iter = 500, chains = 1, refresh = 0, seed = 1
  ))
  s <- suppressWarnings(marginaleffects::avg_slopes(fit,
                                                    conf_level = 0.95))
  pdr <- marginaleffects::posterior_draws(s)
  dw <- pdr$draw[pdr$term == "wt"]
  fr <- suppressWarnings(as_regression_frame(
    fit, show_columns = c("b", "ame", "ame_se", "ame_ci")))
  a <- fr$coefs[fr$coefs$estimate_type == "ame" & fr$coefs$term == "wt", ]
  expect_equal(a$estimate, stats::median(dw), tolerance = 1e-10)
  expect_equal(a$std_error, stats::mad(dw), tolerance = 1e-10)
  expect_equal(a$ci_lower, unname(stats::quantile(dw, 0.025)),
               tolerance = 1e-10)
  expect_equal(a$ci_upper, unname(stats::quantile(dw, 0.975)),
               tolerance = 1e-10)
  # No test, no p, no df: the cells dash like the p column.
  expect_true(is.na(a$statistic) && is.na(a$p_value))
  expect_true(is.na(a$test_type) && is.na(a$df))
  # Factor contrasts ride through (one AME row per non-reference level).
  a6 <- fr$coefs[fr$coefs$estimate_type == "ame" &
                   fr$coefs$term == "cyl_f6", ]
  d6 <- pdr$draw[pdr$contrast == "6 - 4"]
  expect_equal(a6$estimate, stats::median(d6), tolerance = 1e-10)
  # HDI flavor: recomputed on the AME draws, distinct from the ETI.
  fr_h <- suppressWarnings(as_regression_frame(
    fit, ci_method = "hdi", show_columns = c("b", "ame", "ame_ci")))
  ah <- fr_h$coefs[fr_h$coefs$estimate_type == "ame" &
                     fr_h$coefs$term == "wt", ]
  hh <- spicy:::.hdi_interval(dw, 0.95)
  expect_equal(c(ah$ci_lower, ah$ci_upper), hh, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(ah$ci_lower, a$ci_lower)))
})


test_that("Bayesian AME gates: ame_p refused all-Bayes, dashes mixed", {
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("collapse")
  fit <- suppressWarnings(rstanarm::stan_glm(
    am ~ wt, data = mtcars, family = binomial(),
    iter = 400, chains = 1, refresh = 0, seed = 1
  ))
  expect_error(
    table_regression(fit, show_columns = c("b", "ame", "ame_p")),
    class = "spicy_invalid_input"
  )
  # all_ame expands without ame_p for an all-Bayesian table.
  out <- paste(capture.output(print(suppressWarnings(
    table_regression(fit, show_columns = "all_ame")))), collapse = "\n")
  expect_match(out, "AME", fixed = TRUE)
  expect_match(out, "AME = average marginal effect", fixed = TRUE)
  # Mixed table: shared ame_p column, dash on the Bayesian side.
  gf <- glm(am ~ wt, data = mtcars, family = binomial)
  outm <- paste(capture.output(print(suppressWarnings(table_regression(
    list(G = gf, B = fit), show_columns = c("b", "ame", "ame_p")
  )))), collapse = "\n")
  expect_match(outm, "AME", fixed = TRUE)
})
