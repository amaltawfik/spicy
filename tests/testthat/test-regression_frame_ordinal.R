# ---------------------------------------------------------------------------
# Phase 5b tests: as_regression_frame() methods for ordinal regression.
#
# Coverage:
#   * polr (MASS) -- predictor coefs only (no intercept; thresholds in
#     extras), Wald z derived from t-value (polr does not compute p).
#   * clm (ordinal) -- predictor coefs only (no intercept; thresholds
#     in extras), Wald z + p from summary natively.
#   * Factor predictor -- reference-row synthesis per factor (NOT per
#     cumulative threshold; PO assumption).
#   * Schema validity in all paths.
#   * Oracle cross-validation against parameters::model_parameters().
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_polr_basic <- function() {
  skip_if_not_installed("MASS")
  MASS::polr(
    Sat ~ Infl + Type + Cont,
    weights = Freq,
    data = MASS::housing,
    Hess = TRUE
  )
}

.fit_clm_basic <- function() {
  skip_if_not_installed("ordinal")
  ordinal::clm(rating ~ temp + contact, data = ordinal::wine)
}


# ---- 1. polr: schema validity + core fields ------------------------------

test_that("as_regression_frame.polr produces a schema-valid frame", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("polr: required attributes are attached", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("polr: info$class is 'polr'", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "polr")
})

test_that("polr (logit): info$family is cumulative/logit", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "cumulative")
  expect_identical(fr$info$family$link, "logit")
})

test_that("polr: info$dv is the response variable", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "Sat")
})

test_that("polr: response levels surfaced in extras", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$response_levels, c("Low", "Medium", "High"))
})


# ---- 2. polr: no intercept in coefs --------------------------------------

test_that("polr: coefs table has no (Intercept) row", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false("(Intercept)" %in% fr$coefs$term)
})


# ---- 3. polr: coef extraction --------------------------------------------

test_that("polr: coefs estimates match stats::coef(fit)", {
  fit <- .fit_polr_basic()
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

test_that("polr: SE matches sqrt(diag(vcov[pred, pred]))", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  V <- as.matrix(stats::vcov(fit))
  preds <- names(stats::coef(fit))
  expected_se <- sqrt(diag(V[preds, preds]))
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in preds) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(expected_se[nm]),
      tolerance = 1e-10
    )
  }
})


# ---- 4. polr: thresholds in extras ---------------------------------------

test_that("polr: thresholds tibble has (k - 1) rows with finite p-values", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  th <- fr$info$extras$thresholds
  expect_s3_class(th, "data.frame")
  expect_identical(nrow(th), length(fit$lev) - 1L)
  expect_setequal(
    colnames(th),
    c(
      "term",
      "estimate",
      "std_error",
      "statistic",
      "p_value",
      "df",
      "test_type"
    )
  )
  expect_true(all(is.finite(th$p_value)))
  # The block carries its own reference distribution: asymptotic normal
  # for a maximum-likelihood cumulative-link fit.
  expect_true(all(is.infinite(th$df)))
  expect_true(all(th$test_type == "z"))
})

test_that("polr: threshold estimates match fit$zeta", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  th <- fr$info$extras$thresholds
  expect_equal(th$estimate, unname(fit$zeta), tolerance = 1e-10)
})


# ---- 5. polr: factor predictor reference rows ----------------------------

test_that("polr: factor predictor synthesises one ref row (PO -- shared)", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  # 3 factor predictors in the fixture -> 3 ref rows.
  expect_identical(sum(fr$coefs$is_ref), 3L)
  # Each factor's rows = (k-1) non-ref + 1 ref
  type_rows <- fr$coefs[fr$coefs$parent_var == "Type", ]
  expect_identical(nrow(type_rows), 4L)
  expect_identical(sum(type_rows$is_ref), 1L)
})


# ---- 6. polr: inference + supports ---------------------------------------

test_that("polr: Wald z asymptotic (test_type='z', df=Inf)", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "z" | fr$coefs$is_ref))
  expect_true(all(is.infinite(fr$coefs$df) | fr$coefs$is_ref))
})

test_that("polr: supports$exponentiate = TRUE (odds ratios)", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})


# ---- 7. polr: title ------------------------------------------------------

test_that("polr (logit): title_prefix names the cumulative logit family", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "Cumulative logit", fixed = TRUE)
  expect_match(fr$info$extras$title_prefix, "proportional odds", fixed = TRUE)
})


# ---- 8. clm: schema validity + core fields -------------------------------

test_that("as_regression_frame.clm produces a schema-valid frame", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("clm: info$class is 'clm'", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "clm")
})

test_that("clm: info$family is cumulative/logit", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "cumulative")
  expect_identical(fr$info$family$link, "logit")
})


# ---- 9. clm: coef extraction matches summary natively --------------------

test_that("clm: coefs estimates match fit$beta", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(fit$beta)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(fit$beta[nm]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})

test_that("clm: p-values match summary(fit)$coefficients byte-for-byte", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coefficients
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(fit$beta)) {
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(sm[nm, "Pr(>|z|)"]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})


# ---- 10. clm: thresholds in extras ---------------------------------------

test_that("clm: thresholds tibble has (k - 1) rows with finite p-values", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  th <- fr$info$extras$thresholds
  expect_s3_class(th, "data.frame")
  expect_identical(nrow(th), length(fit$y.levels) - 1L)
  expect_true(all(is.finite(th$p_value)))
})

test_that("clm: threshold estimates match fit$alpha", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  th <- fr$info$extras$thresholds
  expect_equal(th$estimate, unname(fit$alpha), tolerance = 1e-10)
})


# ---- 11. clm: factor predictor reference rows ----------------------------

test_that("clm: factor predictor synthesises one ref row (PO)", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(sum(fr$coefs$is_ref), 2L) # temp + contact each have ref
})


# ---- 12. Oracle: parameters::model_parameters() --------------------------

test_that("polr coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(fit, ci = 0.95, exponentiate = FALSE)

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  n_checked <- 0L
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
    n_checked <- n_checked + 1L
  }
  expect_oracle_covered(n_checked, nrow(b_rows))
})

test_that("clm coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(fit, ci = 0.95, exponentiate = FALSE)

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  n_checked <- 0L
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
    n_checked <- n_checked + 1L
  }
  expect_oracle_covered(n_checked, nrow(b_rows))
})


test_that("clm cut-points declare the asymptotic normal they are computed under", {
  skip_if_not_installed("ordinal")
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  th <- fr$info$extras$thresholds
  expect_true(all(is.infinite(th$df)))
  expect_true(all(th$test_type == "z"))
  # The declared law and the numbers agree: statistic and p are ordinal's
  # own Wald z, and the promoted rows take their interval from the same
  # distribution.
  expect_equal(
    th$p_value,
    2 * stats::pnorm(-abs(th$statistic)),
    tolerance = 1e-12
  )
  rows <- spicy:::.append_threshold_rows(fr$coefs, th, 0.95)
  thr_rows <- rows[rows$parent_var %in% spicy:::.REG_BLOCK_THRESH, ]
  expect_true(all(thr_rows$test_type == "z"))
  expect_true(all(is.infinite(thr_rows$df)))
  expect_equal(
    thr_rows$ci_upper,
    th$estimate + stats::qnorm(0.975) * th$std_error,
    tolerance = 1e-15
  )
})


# ---- clmm (ordinal::clmm): cumulative-link mixed model ---------------------

.fit_clmm_wine <- function(...) {
  skip_if_not_installed("ordinal")
  ordinal::clmm(
    rating ~ temp + contact + (1 | judge),
    data = ordinal::wine,
    ...
  )
}

.snap_text <- function(out) {
  txt <- capture.output(print(out))
  paste(sub("[ \t]+$", "", txt), collapse = "\n")
}

test_that("as_regression_frame.clmm builds a schema-valid mixed ordinal frame", {
  fit <- .fit_clmm_wine()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_identical(fr$info$class, "clmm")
  expect_identical(fr$info$family, list(family = "cumulative", link = "logit"))
  expect_identical(fr$info$n_groups, c(judge = 9L))
  expect_false("(Intercept)" %in% fr$coefs$term)
  expect_false(fr$info$supports$ame)
  expect_false(fr$info$supports$nested_lrt)
  expect_true(fr$info$supports$exponentiate)
  expect_false(fr$info$extras$has_singular)
  expect_identical(fr$info$extras$response_levels, as.character(1:5))
  expect_identical(fr$info$random_effects$method, "ML")
  expect_true(is.na(fr$info$fit_stats$r2_marginal))
  expect_equal(fr$info$fit_stats$aic, stats::AIC(fit), tolerance = 1e-12)
  expect_equal(
    fr$info$fit_stats$log_lik,
    as.numeric(stats::logLik(fit)),
    tolerance = 1e-12
  )
})

test_that("clmm: B, SE, p and thresholds are the engine's, pinned (oracle)", {
  fit <- .fit_clmm_wine()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- coef(summary(fit))
  b <- fr$coefs[!fr$coefs$is_ref, ]
  th <- fr$info$extras$thresholds
  rows <- rbind(
    data.frame(
      term = b$term,
      est = b$estimate,
      se = b$std_error,
      p = b$p_value
    ),
    data.frame(
      term = th$term,
      est = th$estimate,
      se = th$std_error,
      p = th$p_value
    )
  )
  # ordinal 2026.7.26 on wine (the model of Christensen's clmm tutorial).
  pinned <- data.frame(
    term = c("tempwarm", "contactyes", "1|2", "2|3", "3|4", "4|5"),
    est = c(
      3.062996593,
      1.834884905,
      -1.623666899,
      1.513365161,
      4.228526656,
      6.088772522
    ),
    se = c(
      0.5953802916,
      0.5125316230,
      0.6824434363,
      0.6037582928,
      0.8089748098,
      0.9724633241
    ),
    p = c(
      2.680838763e-07,
      3.435385536e-04,
      1.735043360e-02,
      1.219073530e-02,
      1.722648561e-07,
      3.820635839e-10
    )
  )
  # The engine agreement above is the oracle; the pinned SE and p come
  # from a numerical Hessian and drift at the sixth digit across platforms
  # (Linux CI vs Windows: 3e-6 on an SE), hence the looser tolerances.
  n_checked <- 0L
  for (i in seq_len(nrow(pinned))) {
    tm <- pinned$term[i]
    r <- rows[rows$term == tm, ]
    expect_identical(nrow(r), 1L, info = tm)
    expect_equal(r$est, unname(sm[tm, "Estimate"]), tolerance = 1e-10)
    expect_equal(r$se, unname(sm[tm, "Std. Error"]), tolerance = 1e-10)
    expect_equal(r$p, unname(sm[tm, "Pr(>|z|)"]), tolerance = 1e-10)
    expect_equal(r$est, pinned$est[i], tolerance = 1e-6)
    expect_equal(r$se, pinned$se[i], tolerance = 1e-4)
    expect_equal(r$p, pinned$p[i], tolerance = 1e-3)
    n_checked <- n_checked + 1L
  }
  expect_oracle_covered(n_checked, nrow(rows))
})

test_that("clmm: the random SD is VarCorr()'s, with glmmTMB's Wald interval", {
  fit <- .fit_clmm_wine()
  vc <- as_regression_frame(fit)$info$random_effects$variance_components
  sd_vc <- attr(ordinal::VarCorr(fit)$judge, "stddev")
  expect_equal(vc$sd, unname(sd_vc), tolerance = 1e-12)
  expect_equal(vc$sd, 1.131132565, tolerance = 1e-6)
  # One random term: clmm optimises log(SD), and the ST1 row of vcov() is
  # on that scale.
  se_log <- sqrt(stats::vcov(fit)["ST1", "ST1"])
  z <- stats::qnorm(0.975)
  expect_equal(sqrt(vc$ci_lower), vc$sd * exp(-z * se_log), tolerance = 1e-12)
  expect_equal(sqrt(vc$ci_upper), vc$sd * exp(z * se_log), tolerance = 1e-12)
  expect_identical(vc$ci_method, "wald")
})

test_that("clmm: the footer LR test is against the clm without random effects", {
  fit <- .fit_clmm_wine()
  lrt <- as_regression_frame(fit)$info$random_effects$null_lrt
  fit0 <- ordinal::clm(rating ~ temp + contact, data = ordinal::wine)
  expect_equal(
    lrt$chi2,
    2 * (as.numeric(stats::logLik(fit)) - as.numeric(stats::logLik(fit0))),
    tolerance = 1e-10
  )
  expect_identical(lrt$df, 1L)
  expect_identical(lrt$family_label, "cumulative logit regression")
  expect_equal(
    lrt$p_chibar2,
    0.5 * stats::pchisq(lrt$chi2, 1, lower.tail = FALSE)
  )
  # Prior weights ride into the null.
  w <- rep(c(1, 2), length.out = nrow(ordinal::wine))
  fit_w <- .fit_clmm_wine(weights = w)
  fit0_w <- ordinal::clm(
    rating ~ temp + contact,
    data = ordinal::wine,
    weights = w
  )
  expect_equal(
    as_regression_frame(fit_w)$info$random_effects$null_lrt$chi2,
    2 * (as.numeric(stats::logLik(fit_w)) - as.numeric(stats::logLik(fit0_w))),
    tolerance = 1e-10
  )
})

test_that("clmm: a boundary fit keeps its correlation row and drops the Wald SE", {
  skip_if_not_installed("ordinal")
  fit <- ordinal::clmm(
    rating ~ temp + contact + (1 + contact | judge),
    data = ordinal::wine
  )
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  expect_true(fr$info$extras$has_singular)
  expect_identical(
    vc$term,
    c("(Intercept)", "contactyes", "(Intercept), contactyes")
  )
  expect_identical(vc$is_correlation, c(FALSE, FALSE, TRUE))
  expect_equal(
    vc$corr[3],
    attr(ordinal::VarCorr(fit)$judge, "correlation")[2, 1],
    tolerance = 1e-12
  )
  expect_true(all(is.na(vc$std_error)))
  # Two variances and one covariance in the null LR test.
  expect_identical(fr$info$random_effects$null_lrt$df, 3L)
  # The note names the boundary, not a rank-deficient design.
  expect_warning(out <- table_regression(fit), class = "spicy_caveat")
  expect_match(attr(out, "note"), "Singular fit", fixed = TRUE)
  expect_no_match(attr(out, "note"), "Rank-deficient", fixed = TRUE)
})

test_that("clmm: AME, robust vcov, profile CIs, standardized and nested are refused", {
  fit <- .fit_clmm_wine()
  expect_error(
    as_regression_frame(fit, vcov = "CR2"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(fit, vcov = "CR2", cluster = ~judge),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(fit, show_columns = c("b", "ame")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, ci_method = "profile"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, re_ci = "profile"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, standardized = "refit"),
    class = "spicy_unsupported_standardized"
  )
  fit0 <- ordinal::clmm(rating ~ temp + (1 | judge), data = ordinal::wine)
  expect_error(
    table_regression(list(fit0, fit), nested = TRUE),
    class = "spicy_invalid_input"
  )
})

test_that("clmm: exponentiate gives odds ratios and leaves the thresholds", {
  fit <- .fit_clmm_wine()
  b <- coef(summary(fit))
  txt <- .snap_text(table_regression(fit, exponentiate = TRUE))
  expect_match(
    txt,
    sprintf("%.2f", exp(b["tempwarm", "Estimate"])),
    fixed = TRUE
  )
  expect_match(txt, sprintf("%.2f", b["1|2", "Estimate"]), fixed = TRUE)
  # A "cloglog" clmm is refused: its coefficients are not log hazard
  # ratios (see as_regression_frame.clmm).
  fit_cl <- .fit_clmm_wine(link = "cloglog")
  expect_error(
    table_regression(fit_cl, exponentiate = TRUE),
    class = "spicy_invalid_input"
  )
  expect_s3_class(table_regression(fit_cl), "spicy_regression_table")
})

test_that("clmm: show_thresholds = FALSE folds the cut-points into the note", {
  fit <- .fit_clmm_wine()
  note <- attr(table_regression(fit, show_thresholds = FALSE), "note")
  expect_match(note, "1|2 = -1.62", fixed = TRUE)
})

test_that("clmm: re_test = 'lrt' tests a single random term by the null LR test", {
  fit <- .fit_clmm_wine()
  tt <- spicy:::.compute_re_term_tests(fit, "lrt")
  lrt <- as_regression_frame(fit)$info$random_effects$null_lrt
  expect_equal(tt$statistic, lrt$chi2, tolerance = 1e-12)
  expect_equal(tt$p_value, lrt$p_chibar2, tolerance = 1e-12)
})

test_that("snapshot: clmm table, default and exponentiated", {
  fit <- .fit_clmm_wine()
  expect_snapshot(cat(.snap_text(table_regression(fit))))
  expect_snapshot(cat(.snap_text(table_regression(fit, exponentiate = TRUE))))
})
