# ---------------------------------------------------------------------------
# Phase 5a tests: as_regression_frame() methods for survival fits.
#
# Coverage:
#   * coxph -- partial likelihood Wald z, no intercept row, hazard
#              ratios via exponentiate, n_obs vs n_events distinction,
#              concordance + Cox-Snell pseudo-R2 in extras.
#   * survreg (Weibull / lognormal / gaussian / exponential) -- AFT
#              Wald z, scale parameter excluded from coefs and stashed
#              in extras, family-dependent exponentiate flag.
#   * Factor predictor -- reference-row synthesis.
#   * Schema validity in all paths.
#   * Oracle cross-validation against parameters::model_parameters()
#     (skipped if not installed).
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_coxph_basic <- function() {
  skip_if_not_installed("survival")
  survival::coxph(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung
  )
}

.fit_coxph_factor <- function() {
  skip_if_not_installed("survival")
  d <- survival::lung
  d$ph.ecog <- factor(d$ph.ecog, labels = c("0", "1", "2", "3"))
  survival::coxph(survival::Surv(time, status) ~ age + ph.ecog, data = d)
}

.fit_survreg_weibull <- function() {
  skip_if_not_installed("survival")
  survival::survreg(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung,
    dist = "weibull"
  )
}

.fit_survreg_lognormal <- function() {
  skip_if_not_installed("survival")
  survival::survreg(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung,
    dist = "lognormal"
  )
}

.fit_survreg_gaussian <- function() {
  skip_if_not_installed("survival")
  survival::survreg(
    survival::Surv(time, status) ~ age,
    data = survival::lung,
    dist = "gaussian"
  )
}

.fit_survreg_factor <- function() {
  skip_if_not_installed("survival")
  d <- survival::lung
  d$ph.ecog <- factor(d$ph.ecog, labels = c("0", "1", "2", "3"))
  survival::survreg(
    survival::Surv(time, status) ~ age + ph.ecog,
    data = d,
    dist = "weibull"
  )
}


# ---- 1. coxph: schema validity + core fields -----------------------------

test_that("as_regression_frame.coxph produces a schema-valid frame", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("coxph: required attributes are attached", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("coxph: info$class is 'coxph'", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "coxph")
})

test_that("coxph: info$family is cox/log (hardcoded; no family slot)", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "cox")
  expect_identical(fr$info$family$link, "log")
})

test_that("coxph: info$dv is the full Surv(...) LHS expression", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  # dv echoes the formula LHS exactly, including any namespace prefix
  # the user wrote.
  expect_identical(fr$info$dv, deparse1(stats::formula(fit)[[2L]]))
  expect_match(fr$info$dv, "Surv(time, status)", fixed = TRUE)
})

test_that("coxph: n_obs = fit$n; n_events = fit$nevent (distinct)", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$n_obs, as.integer(fit$n))
  expect_identical(fr$info$extras$n_events, as.integer(fit$nevent))
  # Sanity: events <= subjects (and in lung the two differ).
  expect_true(fr$info$extras$n_events <= fr$info$n_obs)
})


# ---- 2. coxph: no intercept row ------------------------------------------

test_that("coxph: coefs table has no (Intercept) row", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false("(Intercept)" %in% fr$coefs$term)
})


# ---- 3. coxph: coef extraction -------------------------------------------

test_that("coxph: coefs estimates match stats::coef(fit)", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})

test_that("coxph: p-values match summary(fit)$coefficients", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coefficients
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(sm)) {
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(sm[nm, "Pr(>|z|)"]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})


# ---- 4. coxph: inference + supports --------------------------------------

test_that("coxph: Wald z asymptotic (test_type='z', df=Inf, ci_method='wald')", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "z" | fr$coefs$is_ref))
  expect_true(all(is.infinite(fr$coefs$df) | fr$coefs$is_ref))
})

test_that("coxph: supports$exponentiate = TRUE (hazard ratios canonical)", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})

test_that("coxph: supports flags are correct", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sp <- fr$info$supports
  # AME is undefined for Cox PH (ambiguous survival scale, unreliable SEs):
  # report hazard ratios via exponentiate instead.
  expect_false(sp$ame)
  expect_false(sp$partial_effect_size)
  expect_false(sp$classical_r2)
  expect_true(sp$nested_lrt)
})


# ---- 5. coxph: extras + fit stats ----------------------------------------

test_that("coxph: title_prefix = 'Cox proportional hazards regression'", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Cox proportional hazards regression"
  )
})

test_that("coxph: concordance + Cox-Snell pseudo-R2 surfaced in extras / fit_stats", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.numeric(fr$info$extras$concordance$c))
  expect_true(fr$info$extras$concordance$c > 0.5)
  expect_true(is.numeric(fr$info$fit_stats$pseudo_r2$coxsnell))
  expect_true(fr$info$fit_stats$pseudo_r2$coxsnell > 0)
})

test_that("coxph: AIC/BIC/logLik match stats:: helpers", {
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$aic, stats::AIC(fit), tolerance = 1e-10)
  expect_equal(fr$info$fit_stats$bic, stats::BIC(fit), tolerance = 1e-10)
  expect_equal(
    fr$info$fit_stats$log_lik,
    as.numeric(stats::logLik(fit)),
    tolerance = 1e-10
  )
})


# ---- 6. coxph: factor predictor reference rows ---------------------------

test_that("coxph: factor predictor synthesises ref row + (k-1) non-ref rows", {
  fit <- .fit_coxph_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  rows <- fr$coefs[fr$coefs$parent_var == "ph.ecog", ]
  expect_identical(nrow(rows), 4L)
  expect_identical(sum(rows$is_ref), 1L)
  ref <- rows[rows$is_ref, ]
  expect_identical(ref$label, "0")
  expect_identical(ref$factor_level_pos, 1L)
})


# ---- 7. survreg: schema validity + core fields ---------------------------

test_that("as_regression_frame.survreg produces a schema-valid frame (Weibull)", {
  fit <- .fit_survreg_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("survreg: info$class is 'survreg'", {
  fit <- .fit_survreg_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "survreg")
})

test_that("survreg Weibull: info$family$family = 'weibull'; link = 'log'", {
  fit <- .fit_survreg_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "weibull")
  expect_identical(fr$info$family$link, "log")
})

test_that("survreg Gaussian: info$family$link = 'identity'", {
  fit <- .fit_survreg_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "gaussian")
  expect_identical(fr$info$family$link, "identity")
})

test_that("survreg: info$dv is the full Surv(...) LHS expression", {
  fit <- .fit_survreg_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, deparse1(stats::formula(fit)[[2L]]))
  expect_match(fr$info$dv, "Surv(time, status)", fixed = TRUE)
})


# ---- 8. survreg: Log(scale) excluded from coefs --------------------------

test_that("survreg: coefs table excludes the Log(scale) nuisance row", {
  fit <- .fit_survreg_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false("Log(scale)" %in% fr$coefs$term)
  expect_true("(Intercept)" %in% fr$coefs$term)
  expect_true("age" %in% fr$coefs$term)
})

test_that("survreg: scale parameter is stashed in info$extras$scale_parameter", {
  fit <- .fit_survreg_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$extras$scale_parameter, fit$scale, tolerance = 1e-10)
  expect_identical(fr$info$extras$distribution, "weibull")
})


# ---- 9. survreg: coef extraction -----------------------------------------

test_that("survreg Weibull: coefs estimates match stats::coef(fit)", {
  fit <- .fit_survreg_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})

test_that("survreg Weibull: p-values match summary(fit)$table", {
  fit <- .fit_survreg_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$table
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(sm)) {
    if (nm == "Log(scale)") {
      next
    }
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(sm[nm, "p"]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})


# ---- 10. survreg: inference + supports -----------------------------------

test_that("survreg: Wald z asymptotic (test_type='z', df=Inf)", {
  fit <- .fit_survreg_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "z" | fr$coefs$is_ref))
  expect_true(all(is.infinite(fr$coefs$df) | fr$coefs$is_ref))
})

test_that("survreg log-scale dists: supports$exponentiate = TRUE", {
  for (fit in list(.fit_survreg_weibull(), .fit_survreg_lognormal())) {
    fr <- as_regression_frame(fit, model_id = "M1")
    expect_true(fr$info$supports$exponentiate)
  }
})

test_that("survreg gaussian: supports$exponentiate = FALSE", {
  fit <- .fit_survreg_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(fr$info$supports$exponentiate)
})


# ---- 11. survreg: title prefix -------------------------------------------

test_that("survreg: title_prefix names the distribution", {
  expect_identical(
    as_regression_frame(.fit_survreg_weibull())$info$extras$title_prefix,
    "Weibull AFT regression"
  )
  expect_identical(
    as_regression_frame(.fit_survreg_lognormal())$info$extras$title_prefix,
    "Log-normal AFT regression"
  )
  expect_identical(
    as_regression_frame(.fit_survreg_gaussian())$info$extras$title_prefix,
    "Gaussian AFT regression"
  )
})


# ---- 12. survreg: factor predictor reference rows ------------------------

test_that("survreg: factor predictor synthesises a reference row", {
  fit <- .fit_survreg_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  rows <- fr$coefs[fr$coefs$parent_var == "ph.ecog", ]
  expect_identical(nrow(rows), 4L)
  expect_identical(sum(rows$is_ref), 1L)
})


# ---- 13. Oracle: parameters::model_parameters() --------------------------

test_that("coxph coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_coxph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci = 0.95,
    ci_method = "wald",
    exponentiate = FALSE
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-6,
      info = paste("oracle B mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-6,
      info = paste("oracle SE mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$p_value,
      oracle_row$p,
      tolerance = 1e-6,
      info = paste("oracle p mismatch on term:", nm)
    )
  }
})

test_that("survreg Weibull coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_survreg_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci = 0.95,
    ci_method = "wald",
    exponentiate = FALSE
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  # parameters may report Log(scale) or auxiliary rows; only check the
  # rows the frame surfaces.
  for (nm in b_rows$term) {
    oracle_row <- oracle[oracle$Parameter == nm, ]
    if (nrow(oracle_row) == 0L) {
      next
    }
    spicy_row <- b_rows[b_rows$term == nm, ]
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-6,
      info = paste("oracle B mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-6,
      info = paste("oracle SE mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$p_value,
      oracle_row$p,
      tolerance = 1e-6,
      info = paste("oracle p mismatch on term:", nm)
    )
  }
})
