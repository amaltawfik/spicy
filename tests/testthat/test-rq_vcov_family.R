# quantreg::rq vcov estimator family. Oracle: summary.rq() itself
# (exact equality), triangulated against parameters:: for the default.
# Design record: dev/rq_se_spec.md (default nid; iid/ker/rank opt-ins;
# native bootstrap incl. Hagemann wild gradient cluster; HC*/CR*/
# jackknife refused).

.rq_engel <- function(tau = 0.5) {
  data("engel", package = "quantreg")
  quantreg::rq(foodexp ~ income, data = engel, tau = tau)
}


test_that("rq default is the nid sandwich, exact vs summary.rq", {
  skip_if_not_installed("quantreg")
  fit <- .rq_engel()
  fr <- as_regression_frame(fit)
  o <- summary(fit, se = "nid", hs = TRUE)$coefficients
  b <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_equal(b$std_error, unname(o[, "Std. Error"]), tolerance = 1e-12)
  expect_equal(b$statistic, unname(o[, "t value"]), tolerance = 1e-12)
  expect_match(fr$info$vcov_label, "nid", fixed = TRUE)
  # t inference with n - p df, byte-matching summary.rq's convention.
  expect_true(all(b$test_type == "t"))
  expect_equal(unique(b$df), length(fit$residuals) - length(stats::coef(fit)))
})


test_that("rq default triangulates against parameters::", {
  skip_if_not_installed("quantreg")
  skip_if_not_installed("parameters")
  fit <- .rq_engel()
  fr <- as_regression_frame(fit)
  pp <- as.data.frame(parameters::model_parameters(fit))
  b <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_equal(
    b$std_error[b$term == "income"],
    pp$SE[pp$Parameter == "income"],
    tolerance = 1e-6
  )
})


test_that("rq iid and ker opt-ins are exact vs summary.rq", {
  skip_if_not_installed("quantreg")
  fit <- .rq_engel()
  for (m in c("iid", "ker")) {
    fr <- as_regression_frame(fit, vcov = m)
    o <- summary(fit, se = m, covariance = TRUE)$coefficients
    b <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
    expect_equal(
      b$std_error,
      unname(o[, "Std. Error"]),
      tolerance = 1e-12,
      info = m
    )
  }
  # iid understates nid on this openly heteroskedastic data (the reason
  # the default moved): pin the ordering, not the exact ratio.
  se_iid <- as_regression_frame(fit, vcov = "iid")$coefs$std_error[2]
  se_nid <- as_regression_frame(fit)$coefs$std_error[2]
  expect_true(se_nid > 1.5 * se_iid)
})


test_that("rq rank inversion renders CIs only, at the requested level", {
  skip_if_not_installed("quantreg")
  fit <- .rq_engel()
  fr <- as_regression_frame(fit, vcov = "rank", ci_level = 0.95)
  b <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_true(all(is.na(b$std_error)) && all(is.na(b$p_value)))
  o <- summary(fit, se = "rank", alpha = 0.05)$coefficients
  expect_equal(b$ci_lower, unname(o[, "lower bd"]), tolerance = 1e-10)
  expect_equal(b$ci_upper, unname(o[, "upper bd"]), tolerance = 1e-10)
  expect_match(fr$info$vcov_label, "Rank inversion", fixed = TRUE)
  # AME needs a vcov matrix: refused under rank with the reason.
  expect_error(
    as_regression_frame(fit, vcov = "rank", show_columns = c("b", "ame")),
    "rank",
    class = "spicy_invalid_input"
  )
})


test_that("rq bootstrap is quantreg-native, z-based, one draw", {
  skip_if_not_installed("quantreg")
  fit <- .rq_engel()
  set.seed(42)
  fr <- as_regression_frame(fit, vcov = "bootstrap", boot_n = 200)
  set.seed(42)
  o <- summary(fit, se = "boot", R = 200, covariance = TRUE)
  b <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_equal(
    b$std_error,
    unname(o$coefficients[, "Std. Error"]),
    tolerance = 1e-12
  )
  # House resampler rule: asymptotic z (summary.rq prints t(n-p); SEs
  # match exactly, the inference frame is disclosed via test_type).
  expect_true(all(b$test_type == "z"))
  expect_true(all(is.infinite(b$df)))
  expect_equal(
    b$p_value,
    2 * stats::pnorm(-abs(b$estimate / b$std_error)),
    tolerance = 1e-12
  )
  # boot_percentile bounds come from the SAME replicate draw ($B).
  set.seed(42)
  fr_p <- as_regression_frame(
    fit,
    vcov = "bootstrap",
    boot_n = 200,
    ci_method = "boot_percentile"
  )
  set.seed(42)
  o2 <- summary(fit, se = "boot", R = 200, covariance = TRUE)
  bp <- fr_p$coefs[fr_p$coefs$estimate_type == "B" & !fr_p$coefs$is_ref, ]
  # House percentile convention (boot::boot.ci type = "perc"), NOT
  # stats::quantile type-7: the same ci_method must mean the same
  # convention for every class (2026-07 review).
  house <- apply(o2$B, 2L, spicy:::.boot_percentile_ci, ci_level = 0.95)
  expect_equal(bp$ci_lower, unname(house[1L, ]), tolerance = 1e-10)
  expect_equal(bp$ci_upper, unname(house[2L, ]), tolerance = 1e-10)
})


test_that("rq internal vcov backend refuses what the gate refuses", {
  skip_if_not_installed("quantreg")
  fit <- .rq_engel()
  # Direct compute_model_vcov() callers must not silently receive the
  # nid matrix under an HC* / jackknife label.
  for (v in c("HC1", "CR2", "jackknife")) {
    expect_error(
      spicy:::compute_model_vcov(fit, type = v),
      class = "spicy_unsupported_vcov"
    )
  }
  # rank has genuine CIs but no matrix: classed refusal with the hint.
  expect_error(
    spicy:::compute_model_vcov(fit, type = "rank"),
    "rank-inversion",
    class = "spicy_unsupported_vcov"
  )
})


test_that("weighted rq fits: disclosed, bootstrap refused", {
  skip_if_not_installed("quantreg")
  data("engel", package = "quantreg")
  set.seed(1)
  w <- runif(nrow(engel), 0.5, 2)
  fit <- quantreg::rq(foodexp ~ income, data = engel, tau = 0.5, weights = w)
  fr <- as_regression_frame(fit)
  expect_identical(fr$info$weights_kind, "case")
  expect_true(fr$info$extras$has_weights)
  expect_error(
    table_regression(fit, vcov = "bootstrap"),
    "weighted",
    class = "spicy_unsupported_vcov"
  )
})


test_that("rq cluster formula survives NA-dropped rows; nested refused", {
  skip_if_not_installed("quantreg")
  data("engel", package = "quantreg")
  eng2 <- engel
  eng2$foodexp[c(3, 7)] <- NA
  eng2$cl <- rep(seq_len(47), each = 5)
  fit <- quantreg::rq(foodexp ~ income, data = eng2, tau = 0.5)
  # The formula form resolves against the ORIGINAL data (235 rows) and
  # must be subset by the fit's na.action to the 233 used rows --
  # summary.rq would re-subset a pre-aligned vector, so the backend
  # calls boot.rq directly on x / y it builds itself.
  tb <- suppressWarnings(table_regression(
    fit,
    vcov = "bootstrap",
    cluster = ~cl,
    boot_n = 50
  ))
  out <- paste(capture.output(print(tb)), collapse = "\n")
  expect_match(out, "wild gradient cluster bootstrap", fixed = TRUE)
})


test_that("nested rq pairs ride anova.rq's Wald-type F", {
  skip_if_not_installed("quantreg")
  data("engel", package = "quantreg")
  f0 <- quantreg::rq(foodexp ~ 1, data = engel, tau = 0.5)
  f1 <- .rq_engel()
  o <- suppressWarnings(stats::anova(f0, f1))$table
  tb <- suppressWarnings(table_regression(
    list(M1 = f0, M2 = f1),
    nested = TRUE
  ))
  out <- paste(capture.output(print(tb)), collapse = "\n")
  # The comparison rows carry the exact anova.rq statistic and p.
  expect_match(out, sprintf("%.2f", o$Tn[1L]), fixed = TRUE)
  cmp <- attr(tb, "nested_comparisons") %||%
    spicy:::compute_nested_comparisons(list(f0, f1))
  expect_equal(cmp$f_change[1L], as.numeric(o$Tn[1L]), tolerance = 1e-12)
  expect_equal(cmp$p_change[1L], as.numeric(o$pvalue[1L]), tolerance = 1e-12)
  # R-squared / likelihood families stay NA (undefined for check loss).
  expect_true(is.na(cmp$r2_change[1L]) && is.na(cmp$lrt_change[1L]))
  # AIC delta from quantreg's own pseudo-likelihood methods.
  expect_equal(
    cmp$aic_change[1L],
    as.numeric(stats::AIC(f1)) - as.numeric(stats::AIC(f0)),
    tolerance = 1e-12
  )
  # Guards: mixed classes and mixed taus are refused with the reason.
  expect_error(
    table_regression(
      list(L = stats::lm(foodexp ~ income, data = engel), Q = f1),
      nested = TRUE
    ),
    "mix quantile regression",
    class = "spicy_invalid_input"
  )
  f75 <- quantreg::rq(foodexp ~ income, data = engel, tau = 0.75)
  expect_error(
    table_regression(list(M1 = f0, M2 = f75), nested = TRUE),
    "SAME tau",
    class = "spicy_invalid_input"
  )
})


test_that("rq cluster works only through the wild gradient bootstrap", {
  skip_if_not_installed("quantreg")
  data("engel", package = "quantreg")
  engel$cl <- rep(seq_len(47), each = 5)
  fit <- quantreg::rq(foodexp ~ income, data = engel, tau = 0.5)
  tb <- suppressWarnings(table_regression(
    fit,
    vcov = "bootstrap",
    cluster = engel$cl,
    boot_n = 100
  ))
  out <- paste(capture.output(print(tb)), collapse = "\n")
  expect_match(out, "wild gradient cluster bootstrap", fixed = TRUE)
  expect_error(
    table_regression(fit, vcov = "nid", cluster = engel$cl),
    "wild gradient",
    class = "spicy_unsupported_vcov"
  )
  # Wrong cluster length caught by the shared length gate.
  expect_error(
    table_regression(fit, vcov = "bootstrap", cluster = engel$cl[-1]),
    class = "spicy_invalid_input"
  )
})


test_that("rq refuses HC*, CR*, jackknife; other classes refuse rq tokens", {
  skip_if_not_installed("quantreg")
  fit <- .rq_engel()
  for (v in c("HC1", "CR2", "jackknife")) {
    expect_error(
      table_regression(
        fit,
        vcov = v,
        cluster = if (v == "CR2") rep(1:47, each = 5)
      ),
      class = "spicy_unsupported_vcov"
    )
  }
  fl <- stats::lm(mpg ~ wt, data = mtcars)
  for (v in c("nid", "rank")) {
    expect_error(
      table_regression(fl, vcov = v),
      class = "spicy_unsupported_vcov"
    )
  }
})


test_that("rq AME rows share the coefficient rows' vcov", {
  skip_if_not_installed("quantreg")
  skip_if_not_installed("marginaleffects")
  fit <- .rq_engel()
  fr <- suppressWarnings(as_regression_frame(
    fit,
    show_columns = c("b", "ame")
  ))
  se_b <- fr$coefs$std_error[
    fr$coefs$estimate_type == "B" &
      fr$coefs$term == "income"
  ]
  se_ame <- fr$coefs$std_error[
    fr$coefs$estimate_type == "ame" &
      fr$coefs$term == "income"
  ]
  # Identity model: AME == slope, so the delta-method SE equals the
  # coefficient SE up to marginaleffects' numeric-jacobian noise
  # (~1e-5 relative) -- far tighter than the iid-vs-nid gap (> 2x),
  # so the tolerance still proves ONE shared matrix.
  expect_equal(se_ame, se_b, tolerance = 1e-4)
  # And an explicit iid request moves BOTH (no silent nid fallback).
  fr_i <- suppressWarnings(as_regression_frame(
    fit,
    vcov = "iid",
    show_columns = c("b", "ame")
  ))
  expect_equal(
    fr_i$coefs$std_error[
      fr_i$coefs$estimate_type == "ame" &
        fr_i$coefs$term == "income"
    ],
    fr_i$coefs$std_error[
      fr_i$coefs$estimate_type == "B" &
        fr_i$coefs$term == "income"
    ],
    tolerance = 1e-4
  )
})


test_that("rq with a factor predictor at a non-central tau stays exact", {
  skip_if_not_installed("quantreg")
  # Real observational data (infert case-control study), factor
  # predictor, upper quartile -- the varied-inputs rule.
  fit <- quantreg::rq(age ~ parity + education, data = infert, tau = 0.75)
  fr <- suppressWarnings(as_regression_frame(fit))
  o <- suppressWarnings(summary(fit, se = "nid", hs = TRUE)$coefficients)
  b <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_equal(
    b$std_error[match(rownames(o), b$term)],
    unname(o[, "Std. Error"]),
    tolerance = 1e-12
  )
  # Reference row for the factor's base level renders as usual.
  expect_true(any(fr$coefs$is_ref))
})


test_that("penalized / external-method rq fits are refused", {
  skip_if_not_installed("quantreg")
  data("engel", package = "quantreg")
  fl <- quantreg::rq(foodexp ~ income, data = engel, method = "lasso")
  expect_error(
    as_regression_frame(fl),
    "lasso",
    class = "spicy_unsupported"
  )
  # scad is penalized too (2026-07 review: it slipped the first guard
  # and rendered nid SEs on a penalized fit).
  fs <- quantreg::rq(foodexp ~ income, data = engel, method = "scad")
  expect_error(
    as_regression_frame(fs),
    "scad",
    class = "spicy_unsupported"
  )
})
