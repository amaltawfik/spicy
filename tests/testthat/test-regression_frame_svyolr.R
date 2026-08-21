# ---------------------------------------------------------------------------
# as_regression_frame() for survey::svyolr(): a design-weighted
# cumulative-link model.
#
# Three traps this class sets, each of which the borrowed polr path
# would have walked into:
#   * coef(fit) returns the slopes AND the cut-points, so a table built
#     from it publishes two cut-points as slopes;
#   * nobs(fit) is the SUM OF THE WEIGHTS (6194 for 200 schools);
#   * AIC / BIC / logLik / family() all fail -- there is no likelihood.
#
# Oracles are survey's own numbers, pinned at 17 digits; MASS::polr with
# the same weights triangulates the point estimates only (different
# optimiser, and its standard errors are model-based by construction).
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.olr_data <- function() {
  skip_if_not_installed("survey")
  skip_if_not_installed("MASS")
  data(api, package = "survey", envir = environment())
  d <- apistrat
  d$grade <- ordered(cut(d$api00, c(0, 600, 700, 1000)))
  d
}

.olr_design <- function(d = .olr_data()) {
  survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = d,
    fpc = ~fpc
  )
}

.olr_fit <- function() {
  survey::svyolr(grade ~ ell + stype, design = .olr_design())
}

.olr_b <- function(fr) {
  fr$coefs[
    fr$coefs$estimate_type == "B" &
      !(fr$coefs$is_ref %in% TRUE) &
      !(fr$coefs$parent_var %in% spicy:::.REG_BLOCK_THRESH),
  ]
}


# ---- 1. Schema and the slope / cut-point split ----------------------------

test_that("svyolr produces a schema-valid frame", {
  fr <- as_regression_frame(.olr_fit(), model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_identical(fr$info$class, "svyolr")
  expect_identical(attr(fr, "fit"), .olr_fit())
})

test_that("the cut-points never enter the coefficient rows", {
  fit <- .olr_fit()
  fr <- as_regression_frame(fit)
  b <- .olr_b(fr)
  # coef(svyolr) has five entries: three slopes and two cut-points.
  expect_length(stats::coef(fit), 5L)
  expect_identical(b$term, c("ell", "stypeH", "stypeM"))
  # And the cut-points are where they belong.
  th <- fr$info$extras$thresholds
  expect_identical(nrow(th), 2L)
  expect_identical(th$term, names(fit$zeta))
})

test_that("svyolr coefficients and design SEs match survey, pinned", {
  fit <- .olr_fit()
  fr <- as_regression_frame(fit)
  b <- .olr_b(fr)
  expect_equal(
    b$estimate,
    c(-0.093444893105364163, -1.864871214324368, -1.3545190646806959),
    tolerance = 1e-6
  )
  expect_equal(
    b$std_error,
    c(0.013741936286179798, 0.37485217033329765, 0.34278245094454135),
    tolerance = 1e-6
  )
  # The standard errors are the DESIGN's: the naive inverse Hessian gives
  # 0.0020 / 0.083 / 0.072 on the same fit, an order of magnitude apart.
  naive <- sqrt(diag(solve(fit$Hessian)))[seq_len(3L)]
  expect_true(all(b$std_error / naive > 4))
})

test_that("MASS::polr with the same weights reproduces the point estimates", {
  d <- .olr_data()
  fit <- survey::svyolr(grade ~ ell + stype, design = .olr_design(d))
  ref <- suppressWarnings(MASS::polr(
    grade ~ ell + stype,
    data = d,
    weights = d$pw,
    Hess = TRUE
  ))
  # Different optimisers: agreement to ~1.6e-6 relative, not to the bit.
  expect_equal(
    unname(stats::coef(ref)),
    unname(fit$coefficients),
    tolerance = 1e-4
  )
  expect_equal(unname(ref$zeta), unname(fit$zeta), tolerance = 1e-4)
  # And its standard errors are NOT the design's -- that is the point.
  expect_false(isTRUE(all.equal(
    unname(sqrt(diag(stats::vcov(ref)))[1L]),
    unname(sqrt(diag(stats::vcov(fit)))[1L]),
    tolerance = 1e-2
  )))
})


# ---- 2. One reference distribution for the whole table --------------------

test_that("slopes and cut-points are t at the model's degrees of freedom", {
  fit <- .olr_fit()
  fr <- as_regression_frame(fit)
  b <- .olr_b(fr)
  # survey writes degf(design) - length(beta) = 197 - 3.
  expect_equal(stats::df.residual(fit), 194)
  expect_true(all(b$df == 194))
  expect_true(all(b$test_type == "t"))
  th <- fr$info$extras$thresholds
  expect_true(all(th$df == 194))
  expect_true(all(th$test_type == "t"))
  expect_equal(
    b$p_value,
    c(1.2608525863742167e-10, 1.4339335141643623e-06, 1.0867501400278062e-04),
    tolerance = 1e-6
  )
})

test_that("the cut-point p is a t, not a normal left behind by the CI", {
  fr <- as_regression_frame(.olr_fit())
  th <- fr$info$extras$thresholds
  expect_equal(
    th$p_value,
    c(1.0582353820794546e-13, 3.8182570569938606e-07),
    tolerance = 1e-6
  )
  # The normal answer is two orders of magnitude away on the first
  # cut-point: parameterising only the interval would have left a p from
  # one distribution beside a CI from another, in one row.
  expect_gt(th$p_value[1L] / 1.1745772275875659e-15, 50)
})

test_that("the promoted Thresholds rows use the same t for their interval", {
  fit <- .olr_fit()
  out <- table_regression(fit, output = "data.frame")
  fr <- as_regression_frame(fit)
  th <- fr$info$extras$thresholds
  rows <- spicy:::.append_threshold_rows(fr$coefs, th, 0.95)
  thr_rows <- rows[rows$parent_var %in% spicy:::.REG_BLOCK_THRESH, ]
  expect_true(all(thr_rows$test_type == "t"))
  expect_equal(
    thr_rows$ci_lower,
    th$estimate - stats::qt(0.975, df = 194) * th$std_error,
    tolerance = 1e-6
  )
  expect_s3_class(out, "data.frame")
})

test_that("polr and clm cut-points are untouched by the parameterisation", {
  skip_if_not_installed("MASS")
  d <- .olr_data()
  fit <- suppressWarnings(MASS::polr(
    grade ~ ell + stype,
    data = d,
    Hess = TRUE
  ))
  fr <- as_regression_frame(fit)
  th <- fr$info$extras$thresholds
  expect_true(all(is.infinite(th$df)))
  expect_true(all(th$test_type == "z"))
  # qt(p, Inf) IS qnorm(p): the promoted rows keep the historical CI.
  rows <- spicy:::.append_threshold_rows(fr$coefs, th, 0.95)
  thr_rows <- rows[rows$parent_var %in% spicy:::.REG_BLOCK_THRESH, ]
  expect_equal(
    thr_rows$ci_lower,
    th$estimate - stats::qnorm(0.975) * th$std_error,
    tolerance = 1e-15
  )
  expect_equal(
    th$p_value,
    2 * stats::pnorm(-abs(th$estimate / th$std_error)),
    tolerance = 1e-15
  )
})


# ---- 3. Counts and fit statistics -----------------------------------------

test_that("n is the observed count, not the sum of the weights", {
  fit <- .olr_fit()
  fr <- as_regression_frame(fit)
  # survey sets nobs = sum(wt) on this class and ships no nobs.svyolr.
  expect_equal(stats::nobs(fit), 6194, tolerance = 1e-3)
  expect_identical(fr$info$n_obs, 200L)
  expect_identical(fr$info$fit_stats$nobs, 200L)
  expect_equal(
    fr$info$fit_stats$weighted_nobs,
    6193.9999580383301,
    tolerance = 1e-6
  )
})

test_that("no likelihood statistic is invented for a design-weighted fit", {
  fit <- .olr_fit()
  fr <- as_regression_frame(fit)
  fs <- fr$info$fit_stats
  for (k in c("aic", "bic", "log_lik", "deviance", "sigma")) {
    expect_true(is.na(fs[[k]]), info = k)
  }
  expect_null(fs$pseudo_r2)
  # Because survey has none to give.
  expect_error(stats::AIC(fit))
  expect_error(stats::logLik(fit))
  expect_error(stats::family(fit))
  # fit$deviance exists but is the WEIGHTED objective -- it scales with
  # the population, not with the fit -- so it is not published.
  expect_true(is.finite(fit$deviance))
  expect_false(isTRUE(all.equal(fs$deviance, fit$deviance)))
})

test_that("an explicit token cannot republish an absent statistic", {
  fit <- .olr_fit()
  out <- paste(
    utils::capture.output(print(table_regression(
      fit,
      show_fit_stats = c("nobs", "deviance", "aic", "bic")
    ))),
    collapse = "\n"
  )
  expect_match(out, "n ", fixed = TRUE)
  expect_false(grepl("Deviance", out, fixed = TRUE))
  expect_false(grepl("AIC", out, fixed = TRUE))
  expect_false(grepl("BIC", out, fixed = TRUE))
})


# ---- 4. Capabilities, title, variance label -------------------------------

test_that("the title names the link and the parallel-slopes assumption", {
  fr <- as_regression_frame(.olr_fit())
  expect_identical(
    fr$info$extras$title_prefix,
    "Survey-weighted cumulative logit regression (proportional odds)"
  )
  expect_identical(fr$info$family$family, "cumulative")
  expect_identical(fr$info$family$link, "logit")
  expect_identical(fr$info$weights_kind, "sampling")
  expect_identical(fr$info$vcov_label, "Design-based (Taylor linearisation)")
})

test_that("a probit svyolr is not titled logit", {
  d <- .olr_data()
  fit <- survey::svyolr(
    grade ~ ell,
    design = .olr_design(d),
    method = "probit"
  )
  fr <- as_regression_frame(fit)
  expect_match(fr$info$extras$title_prefix, "probit", fixed = TRUE)
  # "proportional odds" only exists under the logit link.
  expect_match(fr$info$extras$title_prefix, "parallel slopes", fixed = TRUE)
  expect_identical(fr$info$family$link, "probit")
})

test_that("the capabilities say what the class can and cannot do", {
  fr <- as_regression_frame(.olr_fit())
  sup <- fr$info$supports
  expect_true(sup$ame)
  expect_true(sup$exponentiate)
  # No likelihood, so no nested LRT: survey's omnibus test is
  # regTermTest(), a Wald test at the same degrees of freedom.
  expect_false(sup$nested_lrt)
  expect_false(sup$classical_r2)
  expect_false(sup$partial_effect_size)
  expect_false(sup$standardise_refit)
})

test_that("exponentiating gives odds ratios and leaves the cut-points alone", {
  fit <- .olr_fit()
  out <- table_regression(fit, exponentiate = TRUE, output = "data.frame")
  expect_s3_class(out, "data.frame")
  txt <- paste(
    utils::capture.output(print(table_regression(fit, exponentiate = TRUE))),
    collapse = "\n"
  )
  expect_match(txt, "OR", fixed = TRUE)
  # exp(-3.68) = 0.025 would appear if the cut-points were exponentiated.
  expect_match(txt, "-3.68", fixed = TRUE)
})


# ---- 5. The design engine, the replicate engine, and missing rows ---------

test_that("a svyolr on a replicate design is still class svyolr", {
  d <- .olr_data()
  # NOT named `rep`: survey's model.frame.svyolr() re-evaluates the
  # `design` argument BY NAME in the formula's environment, where a
  # design called `rep` resolves to base::rep and the call dies on a
  # primitive.
  rep_design <- survey::as.svrepdesign(.olr_design(d), type = "JKn")
  fit <- suppressWarnings(survey::svyolr(
    grade ~ ell + stype,
    design = rep_design
  ))
  # survey has no svrepolr: svyolr.svyrep.design sets class(rval) <-
  # "svyolr" outright.
  expect_identical(class(fit), "svyolr")
  fr <- as_regression_frame(fit)
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_identical(
    fr$info$vcov_label,
    "Design-based (replicate weights, JKn)"
  )
  expect_true(all(.olr_b(fr)$test_type == "t"))
})

test_that("with missing rows, the counts and the df follow the analytic sample", {
  d <- .olr_data()
  d$ell[1:20] <- NA
  fit <- survey::svyolr(grade ~ ell + stype, design = .olr_design(d))
  fr <- as_regression_frame(fit)
  expect_identical(fr$info$n_obs, 180L)
  expect_equal(
    fr$info$fit_stats$weighted_nobs,
    5487.2699661254883,
    tolerance = 1e-6
  )
  # degf of the REDUCED design (177) minus three slopes.
  expect_equal(unique(.olr_b(fr)$df), 174)
})


# ---- 6. AME: weighted by the design, t at the model's df ------------------

test_that("the per-category AME is design-weighted and t", {
  skip_if_not_installed("marginaleffects")
  d <- .olr_data()
  fit <- survey::svyolr(grade ~ ell + meals, design = .olr_design(d))
  w <- .spicy_ame_fit_wts(fit)
  expect_length(w, 200L)
  expect_equal(sum(w), 6193.9999580383301, tolerance = 1e-12)
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  ame <- fr$coefs[fr$coefs$estimate_type == "ame", ]
  expect_gt(nrow(ame), 0L)
  expect_true(all(ame$test_type == "t"))
  expect_true(all(ame$df == stats::df.residual(fit)))
  # One row per (predictor, response category).
  expect_true("outcome_level" %in% names(ame))
  expect_setequal(unique(ame$outcome_level), as.character(fit$lev))
  # Design-weighted, pinned: the unweighted average is 0.00211063677.
  ell_low <- ame$estimate[
    ame$term == "ell" & ame$outcome_level == levels(d$grade)[1L]
  ]
  expect_equal(ell_low, 0.0021969631, tolerance = 1e-6)
})


# ---- 7. The variance gate now reaches this class --------------------------

test_that("a robust vcov request gets the design message, not the generic one", {
  fit <- .olr_fit()
  err <- expect_error(
    table_regression(fit, vcov = "HC3"),
    class = "spicy_unsupported_vcov"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "svyolr", fixed = TRUE)
  expect_match(msg, "svydesign", fixed = TRUE)
  # Before the class had a frame method it was refused earlier still,
  # with "no as_regression_frame() method"; now it reaches the gate.
  expect_false(grepl("as_regression_frame", msg, fixed = TRUE))
  expect_false(grepl("This class supports", msg, fixed = TRUE))
})

test_that("svyolr appears in the supported-models registry", {
  tb <- table_regression_models()
  expect_true("svyolr" %in% tb[[2L]])
  row <- tb[tb[[2L]] == "svyolr", ]
  expect_identical(row[[1L]], "Survey-weighted")
  expect_match(row[[3L]], "svyolr", fixed = TRUE)
})


# ---- 8. What a replicate-design svyolr cannot do --------------------------

test_that("a replicate-design svyolr still counts and weights correctly", {
  d <- .olr_data()
  rep_design <- survey::as.svrepdesign(.olr_design(d), type = "JKn")
  fit <- suppressWarnings(survey::svyolr(
    grade ~ ell + stype,
    design = rep_design
  ))
  # model.frame() is unusable on this engine (survey re-evaluates the
  # `design` argument in an environment that does not hold it), so the
  # counts must not depend on it.
  expect_error(stats::model.frame(fit))
  fr <- as_regression_frame(fit)
  expect_identical(fr$info$n_obs, 200L)
  expect_equal(
    fr$info$fit_stats$weighted_nobs,
    6193.9999580383301,
    tolerance = 1e-6
  )
  expect_length(.spicy_ame_fit_wts(fit), 200L)
})

test_that("an AME request on a replicate-design svyolr degrades, it does not lie", {
  skip_if_not_installed("marginaleffects")
  d <- .olr_data()
  rep_design <- survey::as.svrepdesign(.olr_design(d), type = "JKn")
  fit <- suppressWarnings(survey::svyolr(
    grade ~ ell + stype,
    design = rep_design
  ))
  # marginaleffects cannot recover the data for this engine either. The
  # column en-dashes with a classed warning rather than reporting an
  # unweighted or model-based effect.
  expect_warning(
    fr <- as_regression_frame(fit, show_columns = c("b", "ame")),
    class = "spicy_fallback"
  )
  expect_false(any(fr$coefs$estimate_type == "ame"))
  expect_invisible(spicy:::validate_regression_frame(fr))
})


test_that("the compact footer keeps the cut-points when they are not rows", {
  # show_thresholds = FALSE falls back to a one-line footer gloss, whose
  # builder is guarded BY CLASS: a class missing from that list loses its
  # cut-points entirely instead of moving them.
  d <- .olr_data()
  fit <- survey::svyolr(grade ~ ell, design = .olr_design(d))
  out <- paste(
    utils::capture.output(print(table_regression(
      fit,
      show_thresholds = FALSE
    ))),
    collapse = "\n"
  )
  expect_match(out, "Thresholds: ", fixed = TRUE)
  expect_match(out, "(0,600]|(600,700] = ", fixed = TRUE)
  # And they really left the body.
  expect_false(grepl("(0,600] | (600,700]", out, fixed = TRUE))
})
