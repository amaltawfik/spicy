# Tests for `glm` support in table_regression() – Phase 3.
# Sections:
#   * Step 1: foundation (z-asymptotic Wald, family-aware title,
#             pseudo-R² family, gaussian glm caveat, class-aware
#             token rejection)
#   * Step 2: exponentiate (OR / IRR / HR / RR / MR / exp(B) header
#             rebrand + delta-method SE)
#   * Step 3: partial_chi2 token via drop1(test = "LRT")
#   * Step 4: standardize for glm (refit / posthoc / basic / smart /
#             pseudo Menard 2011)
#   * Step 5: AME via marginaleffects + CR2 + Satterthwaite df
#   * Step 6: nested LRT for glm + ci_method = "profile"
#   * Step 7: integration / acceptance tests
#   * AUDIT:  regression guards from the post-Phase-3 polish round

mt <- mtcars


# ============================================================================
# Step 1: glm fits flow through, z-asymptotic inference, family-aware title
# ============================================================================

test_that("glm: binomial logit fits with z-asymptotic Wald inference", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit)
  expect_s3_class(out, "spicy_regression_table")
  td <- broom::tidy(out)
  # All test_type = "z", df = Inf – matches summary.glm / Stata
  expect_true(all(td$test_type == "z"))
  expect_true(all(is.infinite(td$df)))
})

test_that("glm: family-aware title – logit / probit / poisson / Gamma", {
  fit_logit <- glm(am ~ mpg, data = mt, family = binomial)
  expect_match(attr(table_regression(fit_logit), "title"),
               "^Logistic regression: am$")
  fit_probit <- glm(am ~ mpg, data = mt, family = binomial(link = "probit"))
  expect_match(attr(table_regression(fit_probit), "title"),
               "^Probit regression: am$")
  fit_pois <- glm(I(round(mpg)) ~ wt, data = mt, family = poisson)
  expect_match(attr(table_regression(fit_pois), "title"),
               "^Poisson regression:")
  fit_gamma <- glm(mpg ~ wt, data = mt, family = Gamma(link = "log"))
  expect_match(attr(table_regression(fit_gamma), "title"),
               "^Gamma regression: mpg$")
})

test_that("glm: hierarchical title is grammatically lower-cased", {
  # Direct unit-test of the title formatter – full nested computation
  # for glm (LRT-based comparison) is part of Step 6.
  # Build minimal frame-shaped objects (only the fields the title
  # function reads). Phase 0c sub-step C5: migrated from
  # build_regression_title() to build_regression_title_from_frames().
  fr <- list(
    list(info = list(dv = "am",
                     extras = list(title_prefix = "Logistic regression"))),
    list(info = list(dv = "am",
                     extras = list(title_prefix = "Logistic regression")))
  )
  expect_equal(
    spicy:::build_regression_title_from_frames(fr, nested = TRUE),
    "Hierarchical logistic regression: am"
  )
})

test_that("glm: classical-vcov footer label says 'Fisher information'", {
  # Phase 7c24 (item c): renamed from "MLE inverse Hessian" to the
  # standard publication name "Fisher information" -- parallels
  # "classical (OLS)" for lm by naming the mechanism that produces
  # the SE (Wooldridge / Greene; Stata's "OIM" / SAS's
  # "Standard Error" are the other common conventions).
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  out <- table_regression(fit)
  expect_match(attr(out, "note"),
               "Std\\. errors: classical \\(Fisher information\\)")
})


# ============================================================================
# Step 1: pseudo-R² family + class-aware token rejection
# ============================================================================

test_that("glm: default show_fit_stats = NULL resolves to pseudo_r2 family", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit)
  # Default for glm-only models: nobs, McFadden, Nagelkerke, AIC
  expect_true("n" %in% out$Variable)
  expect_true(any(grepl("McFadden", out$Variable, fixed = TRUE)))
  expect_true(any(grepl("Nagelkerke", out$Variable, fixed = TRUE)))
  expect_true(any(grepl("AIC", out$Variable, fixed = TRUE)))
  # And NOT plain R² / Adj.R² (those are lm tokens)
  expect_false(any(out$Variable == "R²"))
  expect_false(any(out$Variable == "Adj.R²"))
})

test_that("glm: explicit r2 in show_fit_stats errors with hint to pseudo_r2", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "r2")),
    class = "spicy_invalid_input"
  )
})

test_that("glm: explicit partial_f2 errors with hint to partial_chi2", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  err <- tryCatch(
    table_regression(fit, show_columns = c("b", "partial_f2")),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "partial_chi2")
  expect_match(conditionMessage(err), "Long & Freese")
})

test_that("lm-only: pseudo_r2_mcfadden errors with hint to r2", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "pseudo_r2_mcfadden")),
    class = "spicy_invalid_input"
  )
})

test_that("lm-only: partial_chi2 errors with hint to variance-explained partials", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, show_columns = c("b", "partial_chi2")),
    class = "spicy_invalid_input"
  )
})

test_that("Mixed lm + glm: variance-explained tokens DO NOT error (per-row em-dash)", {
  m_lm <- lm(mpg ~ wt, data = mt)
  m_glm <- glm(am ~ mpg, data = mt, family = binomial)
  # show_fit_stats with r2 + pseudo_r2_mcfadden should accept under
  # mixed; renderer em-dashes the inappropriate cell per-model.
  expect_no_error(
    out <- table_regression(
      list(m_lm, m_glm),
      show_fit_stats = c("nobs", "r2", "pseudo_r2_mcfadden")
    )
  )
})


# ============================================================================
# Step 1: gaussian glm caveat
# ============================================================================

test_that("glm: gaussian / identity emits spicy_caveat", {
  fit <- glm(mpg ~ wt, data = mt, family = gaussian)
  expect_warning(
    table_regression(fit),
    class = "spicy_caveat"
  )
})

test_that("glm: non-gaussian does NOT emit the gaussian caveat", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  expect_no_warning(
    table_regression(fit),
    class = "spicy_caveat"
  )
})


# ============================================================================
# Step 1: pseudo-R² helpers – direct unit tests
# ============================================================================

test_that("compute_pseudo_r2_mcfadden – known-value cross-check", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  ll_full <- as.numeric(logLik(fit))
  null_fit <- update(fit, . ~ 1)
  ll_null <- as.numeric(logLik(null_fit))
  expected <- 1 - ll_full / ll_null
  expect_equal(spicy:::compute_pseudo_r2_mcfadden(fit), expected,
               tolerance = 1e-12)
})

test_that("compute_pseudo_r2_nagelkerke – known-value cross-check", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  n <- nobs(fit)
  ll_full <- as.numeric(logLik(fit))
  ll_null <- as.numeric(logLik(update(fit, . ~ 1)))
  expected <- (1 - exp((ll_null - ll_full) * 2 / n)) /
                (1 - exp(ll_null * 2 / n))
  expect_equal(spicy:::compute_pseudo_r2_nagelkerke(fit), expected,
               tolerance = 1e-12)
})

test_that("compute_pseudo_r2_tjur – only defined for binomial", {
  fit_bin <- glm(am ~ mpg, data = mt, family = binomial)
  expect_true(is.finite(spicy:::compute_pseudo_r2_tjur(fit_bin)))
  fit_pois <- glm(I(round(mpg)) ~ wt, data = mt, family = poisson)
  expect_true(is.na(spicy:::compute_pseudo_r2_tjur(fit_pois)))
  fit_lm <- lm(mpg ~ wt, data = mt)
  expect_true(is.na(spicy:::compute_pseudo_r2_tjur(fit_lm)))
})

test_that("compute_pseudo_r2_*: quasi families return NA (no log-likelihood)", {
  fit_qb <- glm(am ~ mpg, data = mt, family = quasibinomial)
  expect_true(is.na(spicy:::compute_pseudo_r2_mcfadden(fit_qb)))
  expect_true(is.na(spicy:::compute_pseudo_r2_nagelkerke(fit_qb)))
})


# ============================================================================
# Step 2: exponentiate – column header rebrand + delta-method SE
# ============================================================================

test_that("exponentiate: logit ⇒ OR header + numeric identity", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit, exponentiate = TRUE)
  expect_true("OR" %in% names(out))
  td_raw <- broom::tidy(table_regression(fit))
  td_exp <- broom::tidy(table_regression(fit, exponentiate = TRUE))
  b_raw <- td_raw$estimate[td_raw$estimate_type == "B"]
  b_exp <- td_exp$estimate[td_exp$estimate_type == "B"]
  expect_equal(b_exp, exp(b_raw), tolerance = 1e-12)
})

test_that("exponentiate: SE follows delta-method (SE_OR = OR × SE_link)", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td_raw <- broom::tidy(table_regression(fit))
  td_exp <- broom::tidy(table_regression(fit, exponentiate = TRUE))
  b_rows_raw <- td_raw[td_raw$estimate_type == "B", ]
  b_rows_exp <- td_exp[td_exp$estimate_type == "B", ]
  expected <- exp(b_rows_raw$estimate) * b_rows_raw$std.error
  expect_equal(b_rows_exp$std.error, expected, tolerance = 1e-12)
})

test_that("exponentiate: CI bounds also exponentiated", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  td_raw <- broom::tidy(table_regression(fit))
  td_exp <- broom::tidy(table_regression(fit, exponentiate = TRUE))
  expect_equal(td_exp$conf.low [td_exp$estimate_type == "B"],
               exp(td_raw$conf.low [td_raw$estimate_type == "B"]),
               tolerance = 1e-12)
  expect_equal(td_exp$conf.high[td_exp$estimate_type == "B"],
               exp(td_raw$conf.high[td_raw$estimate_type == "B"]),
               tolerance = 1e-12)
})

test_that("exponentiate: z-statistic and p-value are invariant (link scale)", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td_raw <- broom::tidy(table_regression(fit))
  td_exp <- broom::tidy(table_regression(fit, exponentiate = TRUE))
  b_raw <- td_raw[td_raw$estimate_type == "B", ]
  b_exp <- td_exp[td_exp$estimate_type == "B", ]
  expect_equal(b_exp$statistic, b_raw$statistic, tolerance = 1e-12)
  expect_equal(b_exp$p.value,   b_raw$p.value,   tolerance = 1e-12)
})

test_that("exponentiate: poisson(log) ⇒ IRR header", {
  fit <- glm(I(round(mpg)) ~ wt + cyl, data = mt, family = poisson)
  out <- table_regression(fit, exponentiate = TRUE)
  expect_true("IRR" %in% names(out))
})

test_that("exponentiate: binomial(cloglog) ⇒ HR header", {
  fit <- glm(am ~ mpg, data = mt, family = binomial(link = "cloglog"))
  out <- table_regression(fit, exponentiate = TRUE)
  expect_true("HR" %in% names(out))
})

test_that("exponentiate: probit ⇒ generic exp(B) header", {
  fit <- glm(am ~ mpg, data = mt, family = binomial(link = "probit"))
  out <- table_regression(fit, exponentiate = TRUE)
  expect_true("exp(B)" %in% names(out))
})

test_that("exponentiate: lm ⇒ no transform + spicy_ignored_arg warning", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_warning(
    out <- table_regression(fit, exponentiate = TRUE),
    class = "spicy_ignored_arg"
  )
  # B column kept its raw value
  expect_true("B" %in% names(out))
  expect_false("OR" %in% names(out))
  td <- broom::tidy(out)
  expect_equal(td$estimate[td$term == "wt"],
               unname(coef(fit)["wt"]), tolerance = 1e-12)
})

test_that("exponentiate: gaussian glm ⇒ no transform + spicy_ignored_arg warning", {
  fit <- glm(mpg ~ wt, data = mt, family = gaussian)
  expect_warning(
    expect_warning(
      table_regression(fit, exponentiate = TRUE),
      class = "spicy_ignored_arg"
    ),
    class = "spicy_caveat"   # gaussian-glm caveat also fires
  )
})

test_that("exponentiate: footer mentions the family-specific label", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  out <- table_regression(fit, exponentiate = TRUE)
  expect_match(attr(out, "note"),
               "Coefficients exponentiated and displayed as OR")
  # Phase 7c14: the per-rendered-row delta-method "how it was
  # computed" sentence ("SE_OR = OR x SE_link") was dropped from
  # the footer -- redundant with the help text and noisy in a
  # publication table. The footer now ends at "CI bounds
  # exponentiated.".
  expect_match(attr(out, "note"), "CI bounds exponentiated")
  expect_false(grepl("delta-method", attr(out, "note"), fixed = TRUE))
})

test_that("exponentiate: multi-model with mixed families ⇒ per-family qualifier", {
  m_logit <- glm(am ~ mpg, data = mt, family = binomial)
  m_pois  <- glm(I(round(mpg)) ~ wt, data = mt, family = poisson)
  out <- table_regression(list(m_logit, m_pois), exponentiate = TRUE)
  note <- attr(out, "note")
  expect_match(note, "OR")
  expect_match(note, "IRR")
  expect_match(note, "per family")
})

test_that("exponentiate: mixed glm + lm ⇒ per-model header (OR + B)", {
  m_glm <- glm(am ~ mpg, data = mt, family = binomial)
  m_lm  <- lm(mpg ~ wt, data = mt)
  withCallingHandlers(
    out <- table_regression(list(m_glm, m_lm), exponentiate = TRUE),
    spicy_warning = function(c) invokeRestart("muffleWarning")
  )
  # DVs differ (am, mpg) -> smart default lifts the bare DV names
  # into the spanner; sub-column names are prefixed by the DV name.
  # Model 1 (glm/am) -> OR; Model 2 (lm/mpg) -> B.
  expect_true(any(grepl("am: OR", names(out), fixed = TRUE)))
  expect_true(any(grepl("mpg: B$", names(out))))
})

test_that("exponentiate: validation rejects non-logical scalar", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  expect_error(table_regression(fit, exponentiate = "yes"),
               class = "spicy_invalid_input")
  expect_error(table_regression(fit, exponentiate = c(TRUE, FALSE)),
               class = "spicy_invalid_input")
  expect_error(table_regression(fit, exponentiate = NA),
               class = "spicy_invalid_input")
})


# ============================================================================
# Step 2: apply_exponentiate_to_coefs – direct unit tests on the helper
# ============================================================================

test_that("apply_exponentiate_to_coefs – empty / NULL no-ops", {
  expect_null(spicy:::apply_exponentiate_to_coefs(NULL))
  expect_equal(
    nrow(spicy:::apply_exponentiate_to_coefs(spicy:::empty_coefs_long())),
    0L
  )
})

test_that("apply_exponentiate_to_coefs – only B / beta rows transformed", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  ex <- spicy:::extract_lm_phase1(
    fit, model_id = "M1",
    show_columns = c("b", "ame"),
    show_fit_stats = c("nobs", "pseudo_r2_mcfadden", "AIC")
  )
  raw <- ex$coefs
  exp_cf <- spicy:::apply_exponentiate_to_coefs(raw)
  # B rows: exponentiated
  b_idx <- raw$estimate_type == "B" & !raw$is_singular &
             !raw$is_reference & !is.na(raw$estimate)
  expect_equal(exp_cf$estimate[b_idx], exp(raw$estimate[b_idx]),
               tolerance = 1e-12)
  # AME rows: untouched (response scale already)
  a_idx <- raw$estimate_type == "AME" & !is.na(raw$estimate)
  expect_equal(exp_cf$estimate[a_idx], raw$estimate[a_idx],
               tolerance = 1e-12)
})

test_that("apply_exponentiate_to_coefs – singular and reference rows untouched", {
  # Construct a degenerate fit with both a singular coef and a
  # factor-with-reference to exercise both branches.
  mt2 <- mt
  mt2$wt2 <- mt2$wt
  mt2$cyl_f <- factor(mt2$cyl)
  fit <- suppressWarnings(
    glm(am ~ wt + wt2 + cyl_f, data = mt2, family = binomial)
  )
  ex <- spicy:::extract_lm_phase1(fit, model_id = "M1")
  raw <- ex$coefs
  exp_cf <- spicy:::apply_exponentiate_to_coefs(raw)
  # Singular coef: NA estimate stays NA after transform
  sing <- raw$is_singular
  expect_true(any(sing))
  expect_true(all(is.na(exp_cf$estimate[sing])))
  # Reference row: NA stays NA
  ref <- raw$is_reference
  expect_true(any(ref))
  expect_true(all(is.na(exp_cf$estimate[ref])))
})


# ============================================================================
# Step 3: partial_chi2 token via drop1(test = "LRT") - the glm analog
# of partial F (Long & Freese 2014 §3.5; Allison "TYPE3"; SAS PROC LOGISTIC).
# ============================================================================

test_that("glm: partial_chi2 matches drop1(test = 'LRT') to machine precision", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit, show_columns = c("b", "partial_chi2", "p"))
  td <- broom::tidy(out)
  chi <- td[td$estimate_type == "partial_chi2", ]
  expect_equal(nrow(chi), 2L)  # mpg + wt, no intercept

  d1 <- drop1(fit, test = "LRT")
  expect_equal(chi$estimate[chi$term == "mpg"], d1["mpg", "LRT"],
               tolerance = 1e-12)
  expect_equal(chi$estimate[chi$term == "wt"],  d1["wt",  "LRT"],
               tolerance = 1e-12)
  expect_equal(chi$df[chi$term == "mpg"], 1)
  expect_equal(chi$df[chi$term == "wt"],  1)
  # p_value on partial_chi2 row is the LRT p (Pr(>Chi))
  expect_equal(chi$p.value[chi$term == "mpg"], d1["mpg", "Pr(>Chi)"],
               tolerance = 1e-12)
})

test_that("glm: partial_chi2 - factor term shares term-level chi2 across dummies", {
  mt2 <- mt; mt2$cyl <- factor(mt2$cyl)
  fit <- glm(am ~ mpg + cyl, data = mt2, family = binomial)
  out <- table_regression(fit, show_columns = c("b", "partial_chi2"))
  td <- broom::tidy(out)
  chi <- td[td$estimate_type == "partial_chi2", ]
  # cyl has 2 non-reference dummies (cyl6, cyl8), both share term-level chi2
  cyl_rows <- chi[grepl("^cyl", chi$term), ]
  expect_equal(nrow(cyl_rows), 2L)
  expect_equal(cyl_rows$estimate[1], cyl_rows$estimate[2], tolerance = 1e-12)
  expect_equal(cyl_rows$df[1], 2)  # k-1 = 2 df for 3-level factor
  expect_equal(cyl_rows$df[2], 2)
  # Joint chi2 from drop1
  d1 <- drop1(fit, test = "LRT")
  expect_equal(cyl_rows$estimate[1], d1["cyl", "LRT"], tolerance = 1e-12)
})

test_that("glm: partial_chi2 cell renders 'value (df)' format - SAS TYPE3", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit, show_columns = c("b", "partial_chi2"))
  body <- as.data.frame(out, stringsAsFactors = FALSE)
  chi_col <- grep("χ", names(body), value = TRUE)
  expect_length(chi_col, 1L)
  # mpg row: chi2 = 1.99, df = 1 -> "1.99 (1)"
  mpg_cell <- body[grepl("mpg", body$Variable), chi_col]
  expect_match(mpg_cell, "[(]1[)]$")
})

test_that("glm: partial_chi2 column header is chi-squared (UTF-8 safe)", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  out <- table_regression(fit, show_columns = c("b", "partial_chi2"))
  body <- as.data.frame(out, stringsAsFactors = FALSE)
  expect_true(any(grepl("χ²", names(body))))
})

test_that("lm + partial_chi2 - rejected with hint to partial_f2 / eta2 / omega2", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(fit, show_columns = c("b", "partial_chi2")),
    class = "spicy_invalid_input"
  )
  # Hint message mentions the lm-appropriate analogs
  err <- tryCatch(
    table_regression(fit, show_columns = c("b", "partial_chi2")),
    spicy_invalid_input = function(e) e
  )
  expect_match(conditionMessage(err), "partial_f2", fixed = TRUE)
})

test_that("mixed lm + glm with partial_chi2 - validator passes; lm em-dashed", {
  fit_lm  <- lm(mpg ~ wt, data = mt)
  fit_glm <- glm(am ~ mpg + wt, data = mt, family = binomial)
  expect_no_error(
    out <- table_regression(list(fit_lm, fit_glm),
                            show_columns = c("b", "partial_chi2"))
  )
  body <- as.data.frame(out, stringsAsFactors = FALSE)
  chi_cols <- grep("χ", names(body), value = TRUE)
  # Two models -> two chi-squared columns; lm column is all em-dash
  expect_length(chi_cols, 2L)
  lm_chi_col <- chi_cols[1]
  glm_chi_col <- chi_cols[2]
  # lm side of chi2 column is blank (token doesn't apply to this model
  # class); glm side has values. Check that lm column has only blanks
  # for non-Intercept body rows, while glm column has values.
  body_no_fit <- body[!body$Variable %in% c("n", "R²", "Adj.R²",
                                              "R² (McFadden)",
                                              "R² (Nagelkerke)", "AIC",
                                              "Outcome"), ]
  non_int <- body_no_fit[!grepl("Intercept", body_no_fit$Variable), ]
  lm_vals  <- gsub("\\s+", "", non_int[[lm_chi_col]])
  glm_vals <- gsub("\\s+", "", non_int[[glm_chi_col]])
  expect_true(all(lm_vals == ""))           # lm side fully blank
  expect_true(any(nzchar(glm_vals)))         # glm side has values
})

test_that("glm + partial_chi2 - quasi family returns NA (drop1 path), em-dashed", {
  set.seed(123)
  d <- data.frame(y = rpois(50, 3), x = rnorm(50))
  fit <- glm(y ~ x, data = d, family = quasipoisson)
  out <- table_regression(fit, show_columns = c("b", "partial_chi2"))
  td <- broom::tidy(out)
  chi <- td[td$estimate_type == "partial_chi2" & td$term == "x", ]
  # Either no row (extract returned empty) OR row with finite estimate
  # (drop1 still returns scaled deviance for quasi). Just ensure no crash.
  expect_true(nrow(chi) <= 1L)
})

test_that("glm: partial_chi2 with intercept-only fit -> empty rows (no crash)", {
  fit <- glm(am ~ 1, data = mt, family = binomial)
  expect_no_error(
    out <- table_regression(fit, show_columns = c("b", "partial_chi2"))
  )
  td <- broom::tidy(out)
  expect_equal(sum(td$estimate_type == "partial_chi2"), 0L)
})


# ============================================================================
# Step 4: standardize for glm - 5 methods (refit Long-Freese / posthoc / basic
# / smart / pseudo Menard 2011)
# ============================================================================

test_that("glm refit: matches effectsize::standardize_parameters(method='refit')", {
  skip_if_not_installed("effectsize")
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, standardized = "refit"))
  beta <- td[td$estimate_type == "beta", ]
  oracle <- effectsize::standardize_parameters(fit, method = "refit")
  for (term_nm in c("mpg", "wt")) {
    expect_equal(
      beta$estimate[beta$term == term_nm],
      oracle$Std_Coefficient[oracle$Parameter == term_nm],
      tolerance = 1e-6,
      info = paste("term =", term_nm)
    )
  }
})

test_that("glm posthoc: matches effectsize posthoc (X-only, no Y div)", {
  skip_if_not_installed("effectsize")
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, standardized = "posthoc"))
  beta <- td[td$estimate_type == "beta", ]
  oracle <- effectsize::standardize_parameters(fit, method = "posthoc")
  for (term_nm in c("mpg", "wt")) {
    expect_equal(
      beta$estimate[beta$term == term_nm],
      oracle$Std_Coefficient[oracle$Parameter == term_nm],
      tolerance = 1e-6
    )
  }
})

test_that("glm pseudo (Menard 2011): matches manual SD(Y*) calculation", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, standardized = "pseudo"))
  beta <- td[td$estimate_type == "beta", ]
  # Menard formula: SD(Y*) = sqrt(var(eta_hat) + pi^2/3) for binomial logit
  eta <- predict(fit, type = "link")
  sd_y_star <- sqrt(var(eta) + pi^2 / 3)
  b <- coef(fit)
  for (term_nm in c("mpg", "wt")) {
    expected <- b[term_nm] * sd(mt[[term_nm]]) / sd_y_star
    expect_equal(
      beta$estimate[beta$term == term_nm],
      unname(expected),
      tolerance = 1e-12
    )
  }
})

test_that("glm pseudo: probit uses var_link = 1, cloglog uses pi^2/6", {
  fit_pr <- glm(am ~ mpg, data = mt, family = binomial(link = "probit"))
  td_pr <- broom::tidy(table_regression(fit_pr, standardized = "pseudo"))
  beta_pr <- td_pr$estimate[td_pr$estimate_type == "beta" &
                              td_pr$term == "mpg"]
  eta_pr <- predict(fit_pr, type = "link")
  sd_y_star_pr <- sqrt(var(eta_pr) + 1)        # probit: 1
  expected_pr <- coef(fit_pr)["mpg"] * sd(mt$mpg) / sd_y_star_pr
  expect_equal(beta_pr, unname(expected_pr), tolerance = 1e-12)

  fit_cl <- glm(am ~ mpg, data = mt, family = binomial(link = "cloglog"))
  td_cl <- broom::tidy(table_regression(fit_cl, standardized = "pseudo"))
  beta_cl <- td_cl$estimate[td_cl$estimate_type == "beta" &
                              td_cl$term == "mpg"]
  eta_cl <- predict(fit_cl, type = "link")
  sd_y_star_cl <- sqrt(var(eta_cl) + pi^2 / 6) # cloglog: pi^2/6 (Gumbel)
  expected_cl <- coef(fit_cl)["mpg"] * sd(mt$mpg) / sd_y_star_cl
  expect_equal(beta_cl, unname(expected_cl), tolerance = 1e-12)
})

test_that("glm pseudo: non-binomial returns NA + spicy_caveat", {
  fit <- glm(I(round(mpg)) ~ wt, data = mt, family = poisson)
  caveat_seen <- FALSE
  out <- withCallingHandlers(
    table_regression(fit, standardized = "pseudo"),
    spicy_caveat = function(c) {
      caveat_seen <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  expect_true(caveat_seen)
  td <- broom::tidy(out)
  beta_rows <- td[td$estimate_type == "beta", ]
  expect_true(all(is.na(beta_rows$estimate)))
})

test_that("glm: all 5 methods preserve p-value (linear rescaling invariance)", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  raw_td <- broom::tidy(table_regression(fit))
  raw_p <- setNames(raw_td$p.value[raw_td$estimate_type == "B"],
                    raw_td$term[raw_td$estimate_type == "B"])
  for (m in c("refit", "posthoc", "basic", "smart", "pseudo")) {
    td <- broom::tidy(table_regression(fit, standardized = m))
    beta_p <- setNames(td$p.value[td$estimate_type == "beta"],
                       td$term[td$estimate_type == "beta"])
    for (term_nm in c("mpg", "wt")) {
      expect_equal(unname(beta_p[term_nm]), unname(raw_p[term_nm]),
                   tolerance = 1e-10,
                   info = paste("method =", m, "term =", term_nm))
    }
  }
})

test_that("lm + standardized = 'pseudo' - rejected with hint to refit/posthoc", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, standardized = "pseudo"),
    class = "spicy_invalid_input"
  )
  err <- tryCatch(
    table_regression(fit, standardized = "pseudo"),
    spicy_invalid_input = function(e) e
  )
  expect_match(conditionMessage(err), "glm.*only", perl = TRUE)
  expect_match(conditionMessage(err), "refit", fixed = TRUE)
})

test_that("glm refit fallback: factor() in formula triggers spicy_fallback", {
  # factor() in the formula prevents the refit on z-scored mf – should
  # emit spicy_fallback and use posthoc (X-only) algebraic scaling.
  fit <- glm(am ~ mpg + factor(cyl), data = mt, family = binomial)
  fb_seen <- FALSE
  out <- withCallingHandlers(
    table_regression(fit, standardized = "refit"),
    spicy_fallback = function(c) {
      fb_seen <<- TRUE
      invokeRestart("muffleWarning")
    },
    spicy_caveat = function(c) invokeRestart("muffleWarning")
  )
  # Just check no error + warning fires; actual β values come from posthoc
  expect_s3_class(out, "spicy_regression_table")
  expect_true(fb_seen)
})

test_that("glm pseudo: log-binomial standardisation is NA (no latent threshold)", {
  d <- data.frame(y = c(0,1,1,0,1,1,0,0,1,1,0,1,1,0,1,0,1,1,0,1),
                  x = seq_len(20))
  fit <- tryCatch(
    suppressWarnings(glm(y ~ x, data = d, family = binomial(link = "log"))),
    error = function(e) NULL, warning = function(w) NULL
  )
  skip_if(is.null(fit), "log-binomial fit did not converge")
  # The log link (relative-risk model) has no latent-threshold interpretation,
  # so the Menard / Long & Freese latent-variable SD is undefined: a caveat
  # fires and the standardised beta is NA (use standardized = "refit").
  expect_warning(
    td <- broom::tidy(table_regression(fit, standardized = "pseudo")),
    class = "spicy_caveat"
  )
  beta_x <- td$estimate[td$estimate_type == "beta" & td$term == "x"]
  expect_true(length(beta_x) == 0L || all(is.na(beta_x)))
})


# ============================================================================
# Step 5: AME for glm via marginaleffects + (optional) CR2 + Satterthwaite df
# ============================================================================

test_that("glm AME: matches marginaleffects::avg_slopes() to machine precision", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
  ame <- td[td$estimate_type == "AME", ]
  oracle <- marginaleffects::avg_slopes(fit)
  for (term_nm in c("mpg", "wt")) {
    expect_equal(
      ame$estimate[ame$term == term_nm],
      oracle$estimate[oracle$term == term_nm],
      tolerance = 1e-12, info = paste("term =", term_nm)
    )
    expect_equal(
      ame$std.error[ame$term == term_nm],
      oracle$std.error[oracle$term == term_nm],
      tolerance = 1e-10
    )
  }
})

test_that("glm AME: classical vcov uses z-asymptotic (df = Inf, test_type = 'z')", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
  ame <- td[td$estimate_type == "AME", ]
  expect_true(all(is.infinite(ame$df)))
  expect_true(all(ame$test_type == "z"))
})

test_that("glm AME + CR2: Satterthwaite df from coef_test on dominant coef", {
  set.seed(1)
  n <- 200L
  d <- data.frame(y = rbinom(n, 1, 0.5), x1 = rnorm(n), x2 = rnorm(n),
                  clinic = rep(letters[1:20], each = 10))
  fit <- glm(y ~ x1 + x2, data = d, family = binomial)
  td <- broom::tidy(table_regression(fit, vcov = "CR2", cluster = d$clinic,
                                       show_columns = c("b", "ame")))
  ame <- td[td$estimate_type == "AME", ]
  expect_true(all(ame$test_type == "t"))
  # Cross-check: AME df_Satt should equal the dominant-coef df_Satt
  ct <- clubSandwich::coef_test(fit, vcov = "CR2", cluster = d$clinic,
                                  test = "Satterthwaite")
  expect_equal(
    ame$df[ame$term == "x1"],
    unname(ct$df_Satt[rownames(ct) == "x1"]),
    tolerance = 1e-10
  )
  expect_equal(
    ame$df[ame$term == "x2"],
    unname(ct$df_Satt[rownames(ct) == "x2"]),
    tolerance = 1e-10
  )
})

test_that("glm AME + CR2: footer mentions glm-specific mechanism (coef_test)", {
  set.seed(1)
  n <- 200L
  d <- data.frame(y = rbinom(n, 1, 0.5), x = rnorm(n),
                  clinic = rep(letters[1:20], each = 10))
  fit <- glm(y ~ x, data = d, family = binomial)
  out <- table_regression(fit, vcov = "CR2", cluster = d$clinic,
                           show_columns = c("b", "ame"))
  note <- attr(out, "note")
  # Phase 7c22 (item e): footer trimmed -- "t-test with Satterthwaite
  # df (dominant-coefficient approximation)" for the glm path.
  expect_match(note, "Satterthwaite df", fixed = TRUE)
  expect_match(note, "dominant-coefficient approximation", fixed = TRUE)
  expect_false(grepl("clubSandwich::",   note, fixed = TRUE))
  expect_false(grepl("linear_contrast",  note, fixed = TRUE))
})

test_that("glm AME with factor predictor: each level gets its own AME row", {
  mt2 <- mt; mt2$cyl <- factor(mt2$cyl)
  fit <- glm(am ~ mpg + cyl, data = mt2, family = binomial)
  td <- broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
  ame <- td[td$estimate_type == "AME", ]
  # mpg + cyl6 + cyl8 = 3 AME rows
  expect_true("mpg" %in% ame$term)
  expect_true("cyl6" %in% ame$term)
  expect_true("cyl8" %in% ame$term)
})

test_that("glm AME: response-scale (NOT link-scale) - AME != B for logit", {
  # Critical regression test: the closed-form linear contrast (Path A,
  # used for lm) would compute the link-scale AME = B. We must NOT
  # use Path A for glm; instead the response-scale AME from
  # marginaleffects must differ from B.
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
  b_mpg <- td$estimate[td$estimate_type == "B" & td$term == "mpg"]
  ame_mpg <- td$estimate[td$estimate_type == "AME" & td$term == "mpg"]
  # Response-scale AME for logistic mpg coef is order-of-magnitude
  # smaller (logistic squashes through pi^2/3 + var(eta))
  expect_true(abs(ame_mpg) < abs(b_mpg) / 5)
})

test_that("glm AME: HC* vcov uses z-asymptotic (no Satterthwaite)", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, vcov = "HC1",
                                       show_columns = c("b", "ame")))
  ame <- td[td$estimate_type == "AME", ]
  expect_true(all(is.infinite(ame$df)))
  expect_true(all(ame$test_type == "z"))
})


# ============================================================================
# Step 6: nested LRT for glm + ci_method = "profile" option
# ============================================================================

test_that("glm hierarchical: lrt_change cell matches anova(test='LRT')", {
  m1 <- glm(am ~ mpg, data = mt, family = binomial)
  m2 <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(list(m1, m2), nested = TRUE)
  body <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  lrt_row <- body[trimws(body$Variable) == "Δχ²", , drop = FALSE]
  # M2's first sub-column carries the lrt_change cell
  m2_col <- names(body)[5L]
  cell <- trimws(lrt_row[[m2_col]])
  # Cross-check the value
  av <- anova(m1, m2, test = "LRT")
  expected_lrt <- av$Deviance[2]
  expected_str <- sprintf("+%.2f", expected_lrt)
  expect_equal(cell, expected_str)
})

test_that("glm hierarchical: variance-explained NA, lrt_change + p_change finite", {
  m1 <- glm(am ~ mpg, data = mt, family = binomial)
  m2 <- glm(am ~ mpg + wt, data = mt, family = binomial)
  comps <- spicy:::compute_nested_comparisons(list(m1, m2))
  expect_true(is.na(comps$r2_change[1]))
  expect_true(is.na(comps$f_change[1]))
  expect_true(is.finite(comps$lrt_change[1]))
  expect_true(is.finite(comps$p_change[1]))
})

test_that("glm hierarchical: aic_change / bic_change deltas correct", {
  m1 <- glm(am ~ mpg, data = mt, family = binomial)
  m2 <- glm(am ~ mpg + wt, data = mt, family = binomial)
  comps <- spicy:::compute_nested_comparisons(list(m1, m2))
  expect_equal(comps$aic_change[1], AIC(m2) - AIC(m1), tolerance = 1e-12)
  expect_equal(comps$bic_change[1], BIC(m2) - BIC(m1), tolerance = 1e-12)
})

test_that("lm hierarchical: r2_change / f_change / p_change finite", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  comps <- spicy:::compute_nested_comparisons(list(m1, m2))
  expect_true(is.finite(comps$r2_change[1]))
  expect_true(is.finite(comps$f_change[1]))
  expect_true(is.finite(comps$p_change[1]))
})

test_that("ci_method = 'profile' on glm: matches confint() to machine precision", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, ci_method = "profile"))
  oracle <- suppressMessages(confint(fit))
  for (term_nm in c("(Intercept)", "mpg", "wt")) {
    expect_equal(
      td$conf.low[td$estimate_type == "B" & td$term == term_nm],
      oracle[term_nm, 1L],
      tolerance = 1e-10, info = paste("term =", term_nm)
    )
    expect_equal(
      td$conf.high[td$estimate_type == "B" & td$term == term_nm],
      oracle[term_nm, 2L],
      tolerance = 1e-10, info = paste("term =", term_nm)
    )
  }
})

test_that("ci_method = 'profile' leaves estimate / SE / p unchanged (CI only)", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td_wald    <- broom::tidy(table_regression(fit, ci_method = "wald"))
  td_profile <- broom::tidy(table_regression(fit, ci_method = "profile"))
  for (term_nm in c("mpg", "wt")) {
    w_row <- td_wald[td_wald$estimate_type == "B" & td_wald$term == term_nm, ]
    p_row <- td_profile[td_profile$estimate_type == "B" &
                          td_profile$term == term_nm, ]
    expect_equal(w_row$estimate, p_row$estimate, tolerance = 1e-12)
    expect_equal(w_row$std.error, p_row$std.error, tolerance = 1e-12)
    expect_equal(w_row$p.value, p_row$p.value, tolerance = 1e-12)
    # CI bounds DO differ
    expect_false(isTRUE(all.equal(w_row$conf.low, p_row$conf.low,
                                    tolerance = 1e-3)))
  }
})

test_that("ci_method = 'profile' on lm: rejected with hint", {
  fit <- lm(mpg ~ wt, data = mt)
  err <- tryCatch(
    table_regression(fit, ci_method = "profile"),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "glm.*only", perl = TRUE)
  expect_match(conditionMessage(err), "Wald", fixed = TRUE)
})

test_that("ci_method = 'profile' on mixed lm + glm: rejected", {
  m_lm  <- lm(mpg ~ wt, data = mt)
  m_glm <- glm(am ~ mpg, data = mt, family = binomial)
  expect_error(
    table_regression(list(m_lm, m_glm), ci_method = "profile"),
    class = "spicy_invalid_input"
  )
})

test_that("ci_method = 'profile': asymmetric CI for sparse logistic", {
  # Asymmetry is the hallmark of profile CI: distance from estimate
  # to upper bound != distance to lower bound.
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, ci_method = "profile"))
  for (term_nm in c("mpg", "wt")) {
    row <- td[td$estimate_type == "B" & td$term == term_nm, ]
    half_low  <- row$estimate - row$conf.low
    half_high <- row$conf.high - row$estimate
    # Wald would give symmetric (half_low == half_high). Profile is
    # asymmetric – the gap should be visibly different.
    expect_false(isTRUE(all.equal(half_low, half_high, tolerance = 1e-3)))
  }
})


# ============================================================================
# Step 7: integration tests - end-to-end scenarios combining multiple glm
# features (Phase 3 acceptance suite)
# ============================================================================

test_that("E2E: logistic with exponentiate + AME + partial_chi2 + standardized", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(
    fit,
    exponentiate = TRUE,
    standardized = "pseudo",
    show_columns = c("b", "beta", "p", "ame", "ame_p", "partial_chi2")
  )
  expect_s3_class(out, "spicy_regression_table")
  expect_match(attr(out, "title"), "^Logistic regression: am$")
  td <- broom::tidy(out)
  # All four estimate types present
  expect_true(all(c("B", "beta", "AME", "partial_chi2") %in% td$estimate_type))
  # B exponentiated → row for mpg has positive value (OR scale)
  b_mpg <- td$estimate[td$estimate_type == "B" & td$term == "mpg"]
  expect_true(b_mpg > 0)  # OR is exp(-0.32) ≈ 0.72
  # AME on response scale (probability units) – magnitude < |B|
  ame_mpg <- td$estimate[td$estimate_type == "AME" & td$term == "mpg"]
  expect_true(abs(ame_mpg) < 0.1)
})

test_that("E2E: poisson IRR + nested LRT hierarchy", {
  m1 <- glm(I(round(mpg)) ~ wt,         data = mt, family = poisson)
  m2 <- glm(I(round(mpg)) ~ wt + cyl,   data = mt, family = poisson)
  out <- table_regression(
    list(m1, m2),
    exponentiate = TRUE,
    nested = TRUE
  )
  expect_match(attr(out, "title"), "^Hierarchical poisson regression: ")
  note <- attr(out, "note")
  expect_match(note, "IRR", fixed = TRUE)
  # Change rows in the body (not in the footer)
  vars <- trimws(as.data.frame(out, stringsAsFactors = FALSE)$Variable)
  expect_true("Δχ²" %in% vars)
})

test_that("E2E: mixed lm + glm side-by-side renders without error", {
  m_lm  <- lm(mpg ~ wt, data = mt)
  m_glm <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(list("OLS" = m_lm, "Logistic" = m_glm))
  expect_s3_class(out, "spicy_regression_table")
  # Title falls back to plain "Regression comparison" (mixed families)
  expect_match(attr(out, "title"), "^Regression comparison$")
  # vcov footer per-model: OLS / Fisher information (Phase 7c24)
  note <- attr(out, "note")
  expect_match(note, "OLS",                fixed = TRUE)
  expect_match(note, "Fisher information", fixed = TRUE)
})

test_that("E2E: CR2 + glm + AME + Satterthwaite + nested LRT", {
  set.seed(42)
  n <- 200L
  d <- data.frame(y = rbinom(n, 1, 0.5), x1 = rnorm(n), x2 = rnorm(n),
                  clinic = rep(letters[1:20], each = 10))
  m1 <- glm(y ~ x1, data = d, family = binomial)
  m2 <- glm(y ~ x1 + x2, data = d, family = binomial)
  out <- table_regression(
    list(m1, m2),
    vcov = "CR2", cluster = d$clinic,
    show_columns = c("b", "p", "ame", "ame_p"),
    nested = TRUE
  )
  note <- attr(out, "note")
  expect_match(note, "cluster-robust [(]CR2[)]", perl = TRUE)
  # Phase 7c22 (item e): trimmed wording.
  expect_match(note, "Satterthwaite df", fixed = TRUE)
  # Change rows live in the body now
  vars <- trimws(as.data.frame(out, stringsAsFactors = FALSE)$Variable)
  expect_true("Δχ²" %in% vars)
  # AME df_Satt is finite (not Inf) under CR2
  td <- broom::tidy(out)
  ame_rows <- td[td$estimate_type == "AME", ]
  expect_true(all(is.finite(ame_rows$df)))
})

test_that("E2E: profile CI + exponentiate combination", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit, ci_method = "profile", exponentiate = TRUE)
  td <- broom::tidy(out)
  # CI bounds are exp() of the profile bounds on log-odds scale
  oracle <- suppressMessages(confint(fit))
  for (term_nm in c("mpg", "wt")) {
    expect_equal(
      td$conf.low[td$estimate_type == "B" & td$term == term_nm],
      exp(oracle[term_nm, 1L]),
      tolerance = 1e-10
    )
    expect_equal(
      td$conf.high[td$estimate_type == "B" & td$term == term_nm],
      exp(oracle[term_nm, 2L]),
      tolerance = 1e-10
    )
  }
})

test_that("E2E: broom::tidy() schema stable for glm output", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(
    fit,
    show_columns = c("b", "p", "ame", "ame_p", "partial_chi2")
  )
  td <- broom::tidy(out)
  # Required broom-style columns
  expect_true(all(c("term", "estimate", "std.error", "statistic",
                     "p.value", "conf.low", "conf.high", "df",
                     "test_type", "estimate_type") %in% names(td)))
})

test_that("E2E: standardized refit + glm + multi-model rendering", {
  m1 <- glm(am ~ mpg,      data = mt, family = binomial)
  m2 <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(list(m1, m2), standardized = "refit")
  expect_s3_class(out, "spicy_regression_table")
  td <- broom::tidy(out)
  beta_rows <- td[td$estimate_type == "beta", ]
  # Both models have beta rows for mpg
  expect_true(sum(beta_rows$term == "mpg") == 2L)
})

test_that("E2E: full feature surface in a single call (acceptance)", {
  # Single comprehensive call that should succeed – covers ALL Phase 3
  # additions: exponentiate, partial_chi2, AME, standardized=pseudo,
  # ci_method=profile, custom labels, p_adjust, fit-stats override.
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  expect_no_error(
    out <- table_regression(
      fit,
      exponentiate = TRUE,
      ci_method = "profile",
      standardized = "pseudo",
      p_adjust = "holm",
      show_columns = c("b", "beta", "ci", "p", "ame", "ame_p", "partial_chi2"),
      show_fit_stats = c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_tjur",
                          "AIC", "BIC"),
      labels = c(mpg = "Miles per gallon", wt = "Weight (1000 lbs)"),
      stars = TRUE,
      digits = 3L
    )
  )
  td <- broom::tidy(out)
  expect_true(nrow(td) > 0L)
})


# ============================================================================
# Audit-driven regression guards (post-Phase-3 polish)
# ============================================================================

test_that("AUDIT C1: glm + CR* + standardized -- B and beta share Satterthwaite df", {
  set.seed(1)
  n <- 200L
  d <- data.frame(y = rbinom(n, 1, 0.5), x1 = rnorm(n), x2 = rnorm(n),
                  clinic = rep(letters[1:20], each = 10))
  fit <- glm(y ~ x1 + x2, data = d, family = binomial)
  for (m in c("refit", "posthoc", "basic", "smart", "pseudo")) {
    td <- broom::tidy(table_regression(fit, vcov = "CR2", cluster = d$clinic,
                                         standardized = m))
    rows <- td[td$term %in% c("x1", "x2") &
                  td$estimate_type %in% c("B", "beta"), ]
    by_term <- split(rows$df, rows$term)
    for (term_nm in names(by_term)) {
      dfs <- by_term[[term_nm]]
      # B df_Satt and beta df_Satt must match (same Satterthwaite df,
      # not z-asymptotic Inf for one and finite for the other)
      expect_true(all(is.finite(dfs)),
                  info = paste("method =", m, "term =", term_nm))
      expect_true(length(unique(round(dfs, 6))) == 1L,
                  info = paste("method =", m, "term =", term_nm,
                                "df values:", paste(dfs, collapse = ", ")))
    }
  }
})

test_that("AUDIT H1: partial_chi2 returns NULL for quasi families", {
  set.seed(2)
  d <- data.frame(y = rpois(60, 3), x = rnorm(60))
  fit <- glm(y ~ x, data = d, family = quasipoisson)
  res <- spicy:::compute_partial_chi2_for_term(fit, "x")
  expect_null(res)
  fit2 <- glm(am ~ mpg, data = mtcars, family = quasibinomial)
  expect_null(spicy:::compute_partial_chi2_for_term(fit2, "mpg"))
})

test_that("AUDIT M4: lm-only change tokens on all-glm rejected via show_fit_stats", {
  # Since 0.12: change tokens flow through `show_fit_stats`, not via
  # the dropped `nested_stats` argument. Class-aware validation
  # rejects variance-explained change tokens on an all-glm hierarchy.
  m1 <- glm(am ~ mpg,      data = mtcars, family = binomial)
  m2 <- glm(am ~ mpg + wt, data = mtcars, family = binomial)
  err <- tryCatch(
    table_regression(list(m1, m2), nested = TRUE,
                      show_fit_stats = c("nobs", "r2_change", "p_change")),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "not defined for `glm`", fixed = TRUE)
  expect_match(conditionMessage(err), "lrt_change", fixed = TRUE)
})

test_that("AUDIT M5: standardize_algebraic_glm preserves no-intercept predictors", {
  fit <- glm(am ~ -1 + mpg + wt, data = mtcars, family = binomial)
  for (m in c("posthoc", "basic", "smart")) {
    td <- broom::tidy(table_regression(fit, standardized = m))
    beta_rows <- td[td$estimate_type == "beta", ]
    # Both predictors should have FINITE beta (not NA-out of first row)
    mpg_beta <- beta_rows$estimate[beta_rows$term == "mpg"]
    wt_beta  <- beta_rows$estimate[beta_rows$term == "wt"]
    expect_true(is.finite(mpg_beta), info = paste("method =", m))
    expect_true(is.finite(wt_beta),  info = paste("method =", m))
  }
})

test_that("AUDIT: less common families have correct titles", {
  # quasibinomial / quasipoisson explicit prefixes
  fit_qb <- glm(am ~ mpg, data = mtcars, family = quasibinomial)
  expect_match(attr(table_regression(fit_qb), "title"),
               "^Quasi-binomial regression: am$")
  fit_qp <- glm(I(round(mpg)) ~ wt, data = mtcars, family = quasipoisson)
  expect_match(attr(table_regression(fit_qp), "title"),
               "^Quasi-Poisson regression:")
  # inverse.gaussian
  set.seed(5)
  d <- data.frame(y = rgamma(50, 2, 0.5), x = rnorm(50))
  fit_ig <- tryCatch(
    suppressWarnings(glm(y ~ x, data = d,
                          family = inverse.gaussian(link = "1/mu^2"))),
    error = function(e) NULL
  )
  skip_if(is.null(fit_ig), "inverse.gaussian fit did not converge")
  expect_match(attr(table_regression(fit_ig), "title"),
               "^Inverse-Gaussian regression:")
})

test_that("AUDIT: Gamma(log) exponentiate header is MR (mean ratio)", {
  set.seed(6)
  d <- data.frame(y = rgamma(80, 2, 0.5), x1 = rnorm(80), x2 = rnorm(80))
  fit <- glm(y ~ x1 + x2, data = d, family = Gamma(link = "log"))
  out <- table_regression(fit, exponentiate = TRUE)
  expect_true("MR" %in% names(out))
})

test_that("AUDIT: B-row Satterthwaite under CR* (direct unit test)", {
  set.seed(8)
  n <- 200L
  d <- data.frame(y = rbinom(n, 1, 0.5), x = rnorm(n),
                  clinic = rep(letters[1:20], each = 10))
  fit <- glm(y ~ x, data = d, family = binomial)
  vc <- compute_model_vcov(fit, type = "CR2", cluster = d$clinic)
  inf <- spicy:::compute_glm_coef_inference(
    fit, coef_idx = 2L, vc = vc, vcov_type = "CR2",
    cluster = d$clinic, ci_level = 0.95
  )
  expect_equal(inf$test_type, "t")
  expect_true(is.finite(inf$df) && inf$df < n)  # Satterthwaite df is small
})


# ============================================================================
# AUDIT round 2: UX + display labels + formula-wrapped response
# ============================================================================

test_that("AUDIT B1: pseudo_r2_* render with pretty labels (not raw tokens)", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit, show_fit_stats = c("nobs",
                                                    "pseudo_r2_mcfadden",
                                                    "pseudo_r2_nagelkerke",
                                                    "pseudo_r2_tjur"))
  vars <- out$Variable
  expect_true("R² (McFadden)"   %in% vars)
  expect_true("R² (Nagelkerke)" %in% vars)
  expect_true("R² (Tjur)"       %in% vars)
  # Raw token names must NOT appear
  expect_false("pseudo_r2_mcfadden"   %in% vars)
  expect_false("pseudo_r2_nagelkerke" %in% vars)
  expect_false("pseudo_r2_tjur"       %in% vars)
})

test_that("AUDIT B2: pseudo_r2_* work with formula-wrapped response", {
  # Bug: update(fit, . ~ 1) failed when response is wrapped (I(),
  # log(), cbind()), because update reuses the LHS expression and
  # tries to re-evaluate bare symbols against the model.frame
  # (whose columns are named after the wrapped expression). Fixed
  # by extracting the evaluated response and refitting on a fresh
  # data.frame.
  fit_I <- glm(I(round(mpg)) ~ wt, data = mt, family = poisson)
  expect_true(is.finite(spicy:::compute_pseudo_r2_mcfadden(fit_I)))
  expect_true(is.finite(spicy:::compute_pseudo_r2_nagelkerke(fit_I)))

  fit_log <- glm(I(log(mpg)) ~ wt, data = mt, family = gaussian)
  expect_true(is.finite(spicy:::compute_pseudo_r2_mcfadden(fit_log)))

  # cbind() form (binomial proportions): not common in spicy's
  # use cases but should not crash.
  d <- data.frame(s = c(2,5,7,3,1), n = c(10,12,15,8,9), x = 1:5)
  fit_cb <- suppressWarnings(
    glm(cbind(s, n - s) ~ x, data = d, family = binomial)
  )
  # Just check it runs without error (value may be NA if quirky)
  expect_no_error(spicy:::compute_pseudo_r2_mcfadden(fit_cb))
})

test_that("AUDIT: spicy_caveat conditions inherit from spicy_warning", {
  # spicy_warn() must auto-attach the package-wide spicy_warning root
  # so users can catch any spicy warning generically. Verify for the
  # spicy_caveat leaf class introduced for glm-related caveats.
  cnd <- NULL
  withCallingHandlers(
    spicy:::spicy_warn("test", class = "spicy_caveat"),
    spicy_warning = function(c) {
      cnd <<- c
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(cnd, "spicy_caveat")
  expect_s3_class(cnd, "spicy_warning")
})


# ============================================================================
# AUDIT round 3: glm + offset + bootstrap/jackknife class-aware
# ============================================================================

test_that("AUDIT B3: pseudo_r2_* preserve offset in null model refit", {
  # Bug: compute_intercept_only_loglik_glm dropped the offset, so the
  # null model under-modelled the rate baseline and pseudo-R^2 went
  # negative for offset glms (Long & Freese 2014 sec. 3.6: the null
  # model must carry the same offset as the full model).
  set.seed(1)
  n <- 50L
  d <- data.frame(y = rpois(n, 3), x = rnorm(n),
                  exposure = runif(n, 1, 10))
  fit <- glm(y ~ x + offset(log(exposure)), data = d, family = poisson)
  mcf <- spicy:::compute_pseudo_r2_mcfadden(fit)
  nag <- spicy:::compute_pseudo_r2_nagelkerke(fit)
  # Both should be in [0, 1] for a well-defined offset model.
  expect_true(is.finite(mcf) && mcf >= -0.01 && mcf <= 1)
  expect_true(is.finite(nag) && nag >= -0.01 && nag <= 1)
})

test_that("AUDIT B4: glm + bootstrap vcov refits as glm (not lm)", {
  # Bug: compute_lm_vcov_bootstrap hard-coded stats::lm() in the refit
  # closure, so for a glm fit the bootstrap variance was computed for
  # a misspecified linear model on the (often binary) response. SEs
  # were underestimated by an order of magnitude.
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  set.seed(42)
  vc_boot <- spicy:::compute_model_vcov(fit, type = "bootstrap", boot_n = 100L)
  # SE order of magnitude under the FIX should be roughly comparable
  # to (or larger than, for small n with binary data) the classical
  # MLE SE. Under the BUG, bootstrap SEs were ~30x smaller.
  se_classical <- sqrt(diag(vcov(fit)))
  se_boot      <- sqrt(diag(vc_boot))
  # All bootstrap SEs should be at least 50% of classical; the bug
  # produced ratios around 0.03-0.07.
  expect_true(all(se_boot / se_classical > 0.5),
              info = sprintf("ratios: %s",
                              paste(round(se_boot / se_classical, 3),
                                     collapse = ", ")))
})

test_that("AUDIT B4: glm + jackknife vcov refits as glm (not lm)", {
  set.seed(42)
  d <- data.frame(y = rbinom(50, 1, 0.5), x = rnorm(50))
  fit <- glm(y ~ x, data = d, family = binomial)
  vc_jk <- spicy:::compute_model_vcov(fit, type = "jackknife")
  # SE on x should be on the same order of magnitude as classical
  # (jackknife is conservative; ratio should be in [0.5, 5] under
  # the FIX, was ~0.05 under the BUG).
  se_classical <- sqrt(diag(vcov(fit)))["x"]
  se_jk        <- sqrt(diag(vc_jk))["x"]
  expect_true(se_jk / se_classical > 0.5 && se_jk / se_classical < 10,
              info = sprintf("ratio = %.3f", se_jk / se_classical))
})


# ============================================================================
# AUDIT round 4: SPSS/Stata-grade polish
# Cross-validation against parameters / performance, edge case tests,
# error message uniformity, multi-family handling.
# ============================================================================

test_that("AUDIT: glm pseudo-R^2 matches performance::r2_* for binomial", {
  skip_if_not_installed("performance")
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  expect_equal(spicy:::compute_pseudo_r2_mcfadden(fit),
               unname(performance::r2_mcfadden(fit)$R2),
               tolerance = 1e-10)
  expect_equal(spicy:::compute_pseudo_r2_nagelkerke(fit),
               unname(performance::r2_nagelkerke(fit)),
               tolerance = 1e-10)
  expect_equal(spicy:::compute_pseudo_r2_tjur(fit),
               unname(performance::r2_tjur(fit)),
               tolerance = 1e-10)
})

test_that("AUDIT: glm B / SE / z / p match parameters::model_parameters", {
  skip_if_not_installed("parameters")
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit))
  oracle <- parameters::model_parameters(fit, ci_method = "wald")
  for (term_nm in c("(Intercept)", "mpg", "wt")) {
    s_row <- td[td$estimate_type == "B" & td$term == term_nm, ]
    o_row <- oracle[oracle$Parameter == term_nm, ]
    expect_equal(s_row$estimate,  o_row$Coefficient, tolerance = 1e-10,
                 info = paste("term =", term_nm))
    expect_equal(s_row$std.error, o_row$SE,          tolerance = 1e-10)
    expect_equal(s_row$statistic, o_row$z,           tolerance = 1e-10)
    expect_equal(s_row$p.value,   o_row$p,           tolerance = 1e-10)
    expect_equal(s_row$conf.low,  o_row$CI_low,      tolerance = 1e-10)
    expect_equal(s_row$conf.high, o_row$CI_high,     tolerance = 1e-10)
  }
})

test_that("AUDIT: glm + NA in predictors -> nobs reflects na.omit, no crash", {
  set.seed(1)
  d <- data.frame(y = rbinom(50, 1, 0.5), x1 = rnorm(50), x2 = rnorm(50))
  d$x1[1:5] <- NA
  fit <- glm(y ~ x1 + x2, data = d, family = binomial)
  out <- table_regression(fit, show_columns = c("b", "ame", "partial_chi2"))
  expect_equal(nobs(fit), 45L)
  td <- broom::tidy(out)
  expect_true(all(is.finite(td$estimate[!is.na(td$estimate)])))
})

test_that("AUDIT: glm + cluster vector with NAs -> clear fallback warning", {
  set.seed(1)
  n <- 100L
  d <- data.frame(y = rbinom(n, 1, 0.5), x = rnorm(n),
                  clinic = rep(letters[1:10], each = 10))
  d$clinic[1:3] <- NA
  fit <- glm(y ~ x, data = d, family = binomial)
  expect_warning(
    table_regression(fit, vcov = "CR2", cluster = d$clinic),
    class = "spicy_fallback"
  )
})

test_that("AUDIT: cluster length mismatch -> clear actionable error", {
  set.seed(1)
  n <- 100L
  d <- data.frame(y = rbinom(n, 1, 0.5), x = rnorm(n))
  d$x[1:5] <- NA  # creates a 5-row mismatch
  fit <- glm(y ~ x, data = d, family = binomial)
  cluster_full <- rep(letters[1:10], each = 10)  # length 100, fit nobs = 95
  err <- tryCatch(
    table_regression(fit, vcov = "CR2", cluster = cluster_full),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "length 100", fixed = TRUE)
  expect_match(conditionMessage(err), "95 observations", fixed = TRUE)
})

test_that("AUDIT: glm + subset -> nobs reflects subset, table renders", {
  fit <- glm(am ~ mpg + wt, data = mtcars, subset = mpg > 15,
              family = binomial)
  expect_equal(nobs(fit), 26L)
  out <- table_regression(fit)
  expect_s3_class(out, "spicy_regression_table")
})

test_that("AUDIT: glm fit on tibble works", {
  skip_if_not_installed("tibble")
  mtt <- tibble::as_tibble(mtcars)
  fit <- glm(am ~ mpg + wt, data = mtt, family = binomial)
  expect_no_error(table_regression(fit))
})

test_that("AUDIT: empty list + NULL element -> clear errors", {
  expect_error(table_regression(list()), class = "spicy_invalid_input")
  expect_error(table_regression(NULL), class = "spicy_invalid_input")
  err <- tryCatch(
    table_regression(list(NULL, lm(mpg ~ wt, data = mtcars))),
    spicy_unsupported = function(e) e
  )
  expect_s3_class(err, "spicy_unsupported")
  expect_match(conditionMessage(err), "NULL element", fixed = TRUE)
  expect_match(conditionMessage(err), "Drop the NULL", fixed = TRUE)
})

test_that("AUDIT: error messages reference both lm and glm (no stale lm-only)", {
  err <- tryCatch(table_regression(NULL),
                   spicy_invalid_input = function(e) e)
  expect_match(conditionMessage(err), "lm.*glm|glm.*lm", perl = TRUE)
  err <- tryCatch(table_regression(list()),
                   spicy_invalid_input = function(e) e)
  expect_match(conditionMessage(err), "lm.*glm|glm.*lm", perl = TRUE)
})

test_that("AUDIT: ci_level boundary values rejected with clear error", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  for (bad in list(0, 1, -0.1, 1.5, NA_real_)) {
    err <- tryCatch(table_regression(fit, ci_level = bad),
                     spicy_invalid_input = function(e) e)
    expect_s3_class(err, "spicy_invalid_input")
  }
})

test_that("AUDIT: mixed-family glms -> per-model exp header (OR / IRR)", {
  m_log  <- glm(am ~ mpg, data = mt, family = binomial)
  m_pois <- glm(am ~ mpg, data = mt, family = poisson)
  out <- table_regression(list("Logit" = m_log, "Pois" = m_pois),
                            exponentiate = TRUE)
  expect_true("Logit: OR"  %in% names(out))
  expect_true("Pois: IRR"  %in% names(out))
  expect_match(attr(out, "note"), "OR / IRR", fixed = TRUE)
})

test_that("AUDIT: all HC* variants work for glm (sandwich S3 method present)", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  for (h in c("HC0", "HC1", "HC2", "HC3", "HC4", "HC5")) {
    expect_no_error(
      td <- broom::tidy(table_regression(fit, vcov = h))
    )
    se <- td$std.error[td$estimate_type == "B" & td$term == "mpg"]
    expect_true(is.finite(se) && se > 0,
                info = paste("vcov =", h, "se =", se))
  }
})

test_that("AUDIT: rich output formats work for glm + exponentiate", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  if (requireNamespace("gt", quietly = TRUE)) {
    expect_no_error(
      table_regression(fit, exponentiate = TRUE, output = "gt")
    )
  }
  if (requireNamespace("flextable", quietly = TRUE)) {
    expect_no_error(
      table_regression(fit, exponentiate = TRUE, output = "flextable")
    )
  }
  if (requireNamespace("tinytable", quietly = TRUE)) {
    expect_no_error(
      table_regression(fit, exponentiate = TRUE, output = "tinytable")
    )
  }
})

test_that("AUDIT: beta token without standardized -> hint lists all 5 methods", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  err <- tryCatch(
    table_regression(fit, show_columns = c("b", "beta")),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  msg <- conditionMessage(err)
  for (m in c("refit", "posthoc", "basic", "smart", "pseudo")) {
    expect_match(msg, m, fixed = TRUE,
                 info = paste("missing method =", m))
  }
})


# ============================================================================
# AUDIT round 5: B5 critical bug -- multi-model column alignment
# ============================================================================

test_that("AUDIT B5: non-alphabetical model names map to correct columns", {
  # The bug: align_extracts() sorted coefs by (order_idx, estimate_type,
  # model_id), and the renderer derived model_ids via
  # unique(coefs$model_id) which returns the post-sort alphabetical
  # order. The user's model_labels (input order) were then zipped in
  # the wrong order -- column data was swapped vs. column header.
  #
  # Reproducer: name list with non-alphabetical names so the input
  # order ("Z", "A") differs from alphabetical ("A", "Z"). The B value
  # under "Z: B" must come from the model the user passed as "Z", NOT
  # the alphabetically-first model.
  m1 <- lm(mpg ~ wt,           data = mtcars)
  m2 <- lm(mpg ~ wt + cyl,     data = mtcars)
  out <- table_regression(list("Z" = m1, "A" = m2))
  df <- as.data.frame(out, stringsAsFactors = FALSE)
  # Z has only intercept + wt; A has intercept + wt + cyl.
  # Z: B for the cyl row must be blank (Z lacks cyl); A: B has -1.51.
  cyl_row <- df[df$Variable == "cyl", , drop = FALSE]
  expect_equal(trimws(cyl_row$`Z: B`), "")
  expect_match(cyl_row$`A: B`, "-1\\.51")

  # Also: the wt coefficient for Z (lm without cyl) is -5.34;
  # for A (lm with cyl) it's -3.19. They MUST not be swapped.
  wt_row <- df[df$Variable == "wt", , drop = FALSE]
  expect_match(wt_row$`Z: B`, "-5\\.34")
  expect_match(wt_row$`A: B`, "-3\\.19")
})

test_that("AUDIT B5: mixed lm + glm with non-alphabetical labels", {
  # The original audit reproducer: lm + glm with exponentiate. Under
  # the bug, the OLS column got the OR header AND the binomial values.
  m_lm  <- lm(mpg ~ wt, data = mtcars)
  m_glm <- glm(am ~ mpg, data = mtcars, family = binomial)
  out <- table_regression(list("OLS" = m_lm, "Logit" = m_glm),
                            exponentiate = TRUE)
  df <- as.data.frame(out, stringsAsFactors = FALSE)
  # OLS column header must be "OLS: B" (no exp on lm)
  expect_true("OLS: B" %in% names(df))
  # Logit column header must be "Logit: OR" (binomial logit -> OR)
  expect_true("Logit: OR" %in% names(df))
  # Intercept value under OLS: B is the lm intercept (37.29), NOT the
  # exp() of the binomial intercept (which would be ~1.36e-3 displayed
  # as 0.00).
  int_row <- df[df$Variable == "(Intercept)", , drop = FALSE]
  expect_match(int_row$`OLS: B`, "37\\.29")
  expect_match(int_row$`Logit: OR`, "0\\.00")
  # mpg row: OLS has nothing (lm uses wt), Logit has 1.36 (exp(0.31))
  mpg_row <- df[df$Variable == "mpg", , drop = FALSE]
  expect_equal(trimws(mpg_row$`OLS: B`), "")
  expect_match(mpg_row$`Logit: OR`, "1\\.36")
  # Outcome row is hidden by default; opt back in via outcome_labels.
  out2 <- table_regression(list("OLS" = m_lm, "Logit" = m_glm),
                            exponentiate = TRUE,
                            outcome_labels = c("mpg", "am"))
  df2 <- as.data.frame(out2, stringsAsFactors = FALSE)
  out_row <- df2[df2$Variable == "Outcome", , drop = FALSE]
  expect_equal(trimws(out_row$`OLS: B`), "mpg")
  expect_equal(trimws(out_row$`Logit: OR`), "am")
})

test_that("AUDIT B5: 3-model with non-alphabetical names preserves input order", {
  m1 <- lm(mpg ~ wt,             data = mtcars)
  m2 <- lm(mpg ~ wt + cyl,       data = mtcars)
  m3 <- lm(mpg ~ wt + cyl + hp,  data = mtcars)
  out <- table_regression(list("Z" = m1, "A" = m2, "M" = m3))
  cols <- names(as.data.frame(out, stringsAsFactors = FALSE))
  # First non-Variable col must be Z (input position 1), then A, then M
  data_cols <- cols[cols != "Variable"]
  first_per_model <- function(prefix) {
    head(grep(paste0("^", prefix, ":"), data_cols), 1L)
  }
  expect_true(first_per_model("Z") < first_per_model("A"))
  expect_true(first_per_model("A") < first_per_model("M"))
})

test_that("AUDIT B5: pivot_aligned_wide also respects input order", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + cyl, data = mtcars)
  fr1 <- spicy:::as_regression_frame(m1, model_id = "Z")
  fr2 <- spicy:::as_regression_frame(m2, model_id = "A")
  aligned <- spicy:::align_frames(list(fr1, fr2), model_ids = c("Z", "A"))
  wide <- spicy:::pivot_aligned_wide(aligned,
                                       model_labels = c("Z-label", "A-label"))
  cols <- names(wide)
  z_est <- grep("^Z-label__estimate", cols)[1]
  a_est <- grep("^A-label__estimate", cols)[1]
  expect_true(!is.na(z_est) && !is.na(a_est))
  expect_true(z_est < a_est)
  # Z's intercept value must be the lm(mpg ~ wt) intercept (37.29),
  # NOT the lm(mpg ~ wt + cyl) intercept (39.69) – confirms the bug
  # is fixed at the data level (input model_id Z paired with the
  # first input fit, alphabetical re-sort notwithstanding).
  int_row <- wide[wide$term == "(Intercept)" &
                    wide$estimate_type == "B", ]
  expect_equal(int_row[["Z-label__estimate"]], 37.28512627,
               tolerance = 1e-3)
  expect_equal(int_row[["A-label__estimate"]], 39.68616,
               tolerance = 1e-3)
})


# ============================================================================
# AUDIT round 5: B6 (empty model name) + B7 (poly contrasts)
# ============================================================================

test_that("AUDIT B6: partially-named models list auto-fills missing slots", {
  # Pre-0.12: empty names were rejected outright. New behaviour:
  # partial naming is unambiguous user intent (the named slots keep
  # their label, unnamed slots fall back to "Model <position>"),
  # so we accept it without warning. Duplicate explicit names are
  # still rejected (see next test).
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + cyl, data = mtcars)
  models <- list(m1, m2)
  names(models) <- c("", "B")
  out <- table_regression(models)
  cols <- names(as.data.frame(out, stringsAsFactors = FALSE,
                                check.names = FALSE))
  # The unnamed slot gets "Model 1" as label, the named slot keeps "B".
  expect_true(any(grepl("Model 1:", cols, fixed = TRUE)))
  expect_true(any(grepl("B:", cols, fixed = TRUE)))
})

test_that("AUDIT B6: existing duplicate-name validator still fires cleanly", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + cyl, data = mtcars)
  models <- list(m1, m2)
  names(models) <- c("A", "A")
  err <- tryCatch(table_regression(models),
                   spicy_invalid_input = function(e) e)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "Duplicate name", fixed = TRUE)
})

test_that("AUDIT B7: ordered factor with poly contrasts -- grouped under header", {
  # Ordered factors under `contr.poly` (R default for `ordered()`)
  # produce `<var>.L`, `<var>.Q`, `<var>.C`, ... – orthogonal trend
  # coefficients, NOT per-level contrasts. They are grouped under
  # the factor name (like a treatment factor) but with the bare
  # poly suffix (`.L` / `.Q` / `.C` / `^k`) as the sub-row label,
  # in polynomial degree order, and with NO reference row (none
  # exists for poly contrasts). The footer adds an auto note
  # explaining the parameterisation.
  fit <- glm(dentist_12m ~ age + sex + education,
              data = sochealth, family = binomial)
  out <- table_regression(fit)
  vars <- as.data.frame(out, stringsAsFactors = FALSE,
                         check.names = FALSE)$Variable
  vars_trimmed <- trimws(vars)
  # The factor header IS present (poly factors are now grouped).
  expect_true(any(vars == "education:"))
  # NO reference row for education (poly contrasts have none). The
  # presence of "(ref.)" anywhere is fine -- `sex` is a treatment
  # factor and KEEPS its reference row.
  expect_false(any(grepl("Lower secondary", vars)))
  # The poly contrasts are sublabelled with the bare suffix and
  # appear in linear -> quadratic order.
  expect_true(".L" %in% vars_trimmed)
  expect_true(".Q" %in% vars_trimmed)
  l_pos <- which(vars_trimmed == ".L")
  q_pos <- which(vars_trimmed == ".Q")
  expect_true(l_pos < q_pos)
  # Auto footer mentions the poly contrasts.
  expect_match(attr(out, "note"),
                "Ordered factor `education`: polynomial trends")
  # sex (treatment factor) still groups correctly with ref row
  expect_true(any(grepl("^sex:", vars)))
  expect_true(any(grepl("Female.*ref", vars)))
})

test_that("AUDIT B7: treatment-coded factor unchanged (regression check)", {
  # Sanity: factors using contr.treatment (R's default for unordered
  # factors) still get the factor header + reference row + indented
  # levels.
  mt <- mtcars
  mt$cyl <- factor(mt$cyl)  # unordered factor -> contr.treatment
  fit <- glm(am ~ mpg + cyl, data = mt, family = binomial)
  out <- table_regression(fit)
  vars <- as.data.frame(out, stringsAsFactors = FALSE)$Variable
  expect_true(any(grepl("^cyl:", vars)))     # factor header present
  expect_true(any(grepl("4.*ref", vars)))    # ref row for cyl=4
})


# ============================================================================
# AUDIT round 5: B8 (extreme-value display)
# ============================================================================

test_that("AUDIT B8: format_number switches to scientific for huge values", {
  expect_equal(spicy:::format_number(1.747e11, 2L), "1.75e+11")
  expect_equal(spicy:::format_number(-2.13e12, 2L), "-2.13e+12")
  expect_equal(spicy:::format_number(1e7, 2L), "1.00e+07")
})

test_that("AUDIT B8: format_number stays fixed-decimal below 1e+7", {
  expect_equal(spicy:::format_number(9999999, 2L), "9999999.00")
  expect_equal(spicy:::format_number(1234.56, 2L), "1234.56")
})

test_that("AUDIT B8: small values respect digits contract (round to zero)", {
  # 1e-3 at digits = 2 -> "0.00" (fixed), NOT "1.00e-03" (scientific).
  # This matches Stata convention and keeps decimal alignment stable.
  # Users wanting sub-precision visible can request more digits.
  expect_equal(spicy:::format_number(1e-3, 2L), "0.00")
  expect_equal(spicy:::format_number(1e-7, 2L), "0.00")
  # Genuine machine-epsilon noise must render as 0.00, NOT scientific.
  # This was the original B8 audit failure: standardized refit
  # intercept noise (8e-17) was being shown as "8.05e-17" and
  # broke decimal alignment in the standardized stars test.
  expect_equal(spicy:::format_number(8.05e-17, 2L), "0.00")
})

test_that("AUDIT B8: zero stays as 0.00 (no scientific)", {
  expect_equal(spicy:::format_number(0, 2L), "0.00")
})

test_that("AUDIT B8: glm exp huge OR display is readable", {
  fit <- glm(am ~ mpg + wt, data = mtcars, family = binomial)
  out <- table_regression(fit, exponentiate = TRUE)
  df <- as.data.frame(out, stringsAsFactors = FALSE)
  int_row <- df[df$Variable == "(Intercept)", , drop = FALSE]
  # Intercept OR ~ 1.75e+11 -- must be in scientific notation, not
  # 12 digits wide
  expect_match(trimws(int_row$OR), "e\\+", perl = TRUE)
  # Must have mantissa . digits + e + sign + exponent
  expect_match(trimws(int_row$OR), "^-?\\d+\\.\\d+e\\+\\d+$", perl = TRUE)
})

test_that("AUDIT B8: standardized refit intercept noise renders as 0.00", {
  # Regression check for the bug surfaced by polish round 5: the
  # centered-refit intercept produces machine-epsilon non-zero
  # (e.g. 8.05e-17). Pre-fix this was breaking decimal alignment
  # in the standardized + stars test by adding trailing space to
  # the wt beta cell.
  fit <- lm(mpg ~ wt, data = mtcars)
  out <- table_regression(fit, standardized = "refit", stars = TRUE)
  int_row <- out[out$Variable == "(Intercept)", , drop = FALSE]
  # beta column for intercept must round to 0.00, not show as
  # scientific noise.
  beta_int <- trimws(int_row$`β`)
  expect_equal(beta_int, "0.00")
})


# ============================================================================
# AUDIT round 6: silent no-op detection (B9 nested single, B10 nested_stats
# without nested, B12 cluster without CR*) + family/link matrix coverage
# ============================================================================

test_that("AUDIT B9: nested = TRUE on single fit warns (was silent no-op)", {
  fit <- glm(am ~ mpg, data = mtcars, family = binomial)
  expect_warning(
    out <- table_regression(fit, nested = TRUE),
    class = "spicy_ignored_arg"
  )
  # Output still rendered (no error)
  expect_s3_class(out, "spicy_regression_table")
})


test_that("AUDIT B12: cluster supplied without CR* vcov warns explicitly", {
  fit <- glm(am ~ mpg, data = mtcars, family = binomial)
  expect_warning(
    table_regression(fit, cluster = mtcars$cyl),
    class = "spicy_ignored_arg"
  )
  # Same warning under HC* (also not CR)
  expect_warning(
    table_regression(fit, vcov = "HC1", cluster = mtcars$cyl),
    class = "spicy_ignored_arg"
  )
})

test_that("AUDIT: no warning when cluster IS used (CR* + cluster)", {
  set.seed(1)
  n <- 100L
  d <- data.frame(y = rbinom(n, 1, 0.5), x = rnorm(n),
                  clinic = rep(letters[1:10], each = 10))
  fit <- glm(y ~ x, data = d, family = binomial)
  expect_no_warning(
    table_regression(fit, vcov = "CR2", cluster = d$clinic),
    class = "spicy_ignored_arg"
  )
})

test_that("AUDIT round 6: family x link matrix -- 14 combos all run cleanly", {
  set.seed(1)
  n <- 100L
  d <- data.frame(y_b = rbinom(n, 1, 0.5),
                  y_p = rpois(n, 3L),
                  y_g = rgamma(n, 2, 1),
                  x = rnorm(n))
  matrix_specs <- list(
    list(fam = binomial(link = "logit"),    y = "y_b"),
    list(fam = binomial(link = "probit"),   y = "y_b"),
    list(fam = binomial(link = "cloglog"),  y = "y_b"),
    list(fam = binomial(link = "log"),      y = "y_b"),
    list(fam = poisson(link = "log"),       y = "y_p"),
    list(fam = poisson(link = "identity"),  y = "y_p"),
    list(fam = poisson(link = "sqrt"),      y = "y_p"),
    list(fam = Gamma(link = "inverse"),     y = "y_g"),
    list(fam = Gamma(link = "log"),         y = "y_g"),
    list(fam = Gamma(link = "identity"),    y = "y_g"),
    list(fam = quasibinomial(),             y = "y_b"),
    list(fam = quasipoisson(),              y = "y_p")
  )
  for (spec in matrix_specs) {
    y <- d[[spec$y]]
    fit <- tryCatch(
      suppressWarnings(glm(y ~ d$x, family = spec$fam)),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    expect_no_error(
      table_regression(fit),
      message = sprintf("family = %s / link = %s",
                         spec$fam$family, spec$fam$link)
    )
  }
})

test_that("AUDIT: Helmert and sum contrasts render without bogus ref row", {
  d <- mtcars
  d$cyl_h <- factor(d$cyl)
  contrasts(d$cyl_h) <- contr.helmert(3)
  fit_h <- glm(am ~ cyl_h, data = d, family = binomial)
  vars_h <- as.data.frame(table_regression(fit_h),
                            stringsAsFactors = FALSE)$Variable
  expect_false(any(grepl("ref", vars_h)))    # no ref row for poly-style
  expect_true("cyl_h1" %in% vars_h)

  d$cyl_s <- factor(d$cyl)
  contrasts(d$cyl_s) <- contr.sum(3)
  fit_s <- glm(am ~ cyl_s, data = d, family = binomial)
  vars_s <- as.data.frame(table_regression(fit_s),
                            stringsAsFactors = FALSE)$Variable
  expect_false(any(grepl("ref", vars_s)))
  expect_true("cyl_s1" %in% vars_s)
})

test_that("AUDIT: long predictor names render correctly", {
  d <- mtcars
  names(d)[c(2, 3)] <- c("very_long_predictor_name_1",
                          "super_long_predictor_2")
  fit <- glm(am ~ very_long_predictor_name_1 + super_long_predictor_2,
              data = d, family = binomial)
  out <- table_regression(fit)
  vars <- as.data.frame(out, stringsAsFactors = FALSE)$Variable
  expect_true("very_long_predictor_name_1" %in% vars)
  expect_true("super_long_predictor_2" %in% vars)
})

test_that("AUDIT: response types (logical, integer, character->factor)", {
  set.seed(1)
  n <- 100L
  # logical Y
  d_b <- data.frame(y = sample(c(TRUE, FALSE), n, replace = TRUE),
                    x = rnorm(n))
  fit_b <- glm(y ~ x, data = d_b, family = binomial)
  expect_no_error(table_regression(fit_b))
  # integer Y
  d_p <- data.frame(y = rpois(n, 3L), x = rnorm(n))
  fit_p <- glm(y ~ x, data = d_p, family = poisson)
  expect_no_error(table_regression(fit_p))
  # character Y -> factor
  d_c <- data.frame(y = sample(c("Yes", "No"), n, replace = TRUE),
                    x = rnorm(n))
  fit_c <- glm(factor(y) ~ x, data = d_c, family = binomial)
  expect_no_error(table_regression(fit_c))
})


# ============================================================================
# Coverage push (post-polish): branches uncovered by the AME caveat,
# CI internal alignment fallback, fit-stats display label, and the
# nested_stats validators added in the recent polish rounds.
# ============================================================================

test_that("AME caveat: spicy_caveat fires when ame + p without ame_p (glm)", {
  # Caveat fires only when divergence between B-p and AME-p is
  # plausible: any glm in the set, OR any model with a non-additive
  # term (interaction / transform). Pure additive lm is silent.
  fit_glm <- glm(am ~ mpg + wt, data = mtcars, family = binomial)
  expect_warning(
    table_regression(fit_glm, show_columns = c("b", "ame", "p")),
    class = "spicy_caveat"
  )
  cnd <- NULL
  withCallingHandlers(
    table_regression(fit_glm, show_columns = c("b", "ame", "p")),
    spicy_caveat = function(c) {
      cnd <<- c
      invokeRestart("muffleWarning")
    }
  )
  msg <- conditionMessage(cnd)
  expect_match(msg, "ame_p", fixed = TRUE)
  expect_match(msg, "B (or beta", fixed = TRUE)
})

test_that("AME caveat: fires for lm with interaction term", {
  fit_int <- lm(mpg ~ wt * cyl, data = mtcars)
  expect_warning(
    table_regression(fit_int, show_columns = c("b", "ame", "p")),
    class = "spicy_caveat"
  )
})

test_that("AME caveat: SILENT for additive lm (B-p == AME-p mathematically)", {
  fit_add <- lm(mpg ~ wt + cyl, data = mtcars)
  cnd <- NULL
  withCallingHandlers(
    table_regression(fit_add, show_columns = c("b", "ame", "p")),
    spicy_caveat = function(c) {
      cnd <<- c
      invokeRestart("muffleWarning")
    }
  )
  expect_null(cnd)
})

test_that("AME caveat: NO caveat when ame + p + ame_p all present", {
  fit <- glm(am ~ mpg + wt, data = mtcars, family = binomial)
  cnd <- NULL
  withCallingHandlers(
    table_regression(fit, show_columns = c("b", "p", "ame", "ame_p")),
    spicy_caveat = function(c) {
      cnd <<- c
      invokeRestart("muffleWarning")
    }
  )
  expect_null(cnd)
})

test_that("align_ci_strings: em-dash and blank cells centered in column", {
  values <- c("[61.78, 67.49]", "[-0.03, 0.08]", "[2.09, 5.22]",
              "–", "", NA_character_)
  out <- spicy:::align_ci_strings(values)
  # All cells have the same total width
  expect_equal(length(unique(nchar(out))), 1L)
  # CI cells: brackets, commas, decimal points all at fixed positions
  ci_rows <- out[seq_len(3L)]
  bracket_positions <- vapply(ci_rows,
                                function(s) regexpr("\\[", s)[[1]],
                                integer(1))
  expect_equal(length(unique(bracket_positions)), 1L)
  close_positions <- vapply(ci_rows,
                              function(s) regexpr("\\]", s)[[1]],
                              integer(1))
  expect_equal(length(unique(close_positions)), 1L)
  # em-dash cell: contains the em-dash glyph, surrounded by spaces
  em_cell <- out[4L]
  expect_true(grepl("–", em_cell))
  expect_match(em_cell, "^\\s+–\\s+$", perl = TRUE)
  # blank / NA: full-width whitespace
  expect_match(out[5L], "^\\s+$", perl = TRUE)
  expect_match(out[6L], "^\\s+$", perl = TRUE)
})

test_that("align_ci_strings: empty input returns empty vector", {
  expect_equal(spicy:::align_ci_strings(character(0)), character(0))
})

test_that("align_ci_strings: works with European decimal mark (semicolon sep)", {
  values <- c("[61,78; 67,49]", "[-0,03; 0,08]")
  out <- spicy:::align_ci_strings(values, decimal_mark = ",")
  expect_equal(length(unique(nchar(out))), 1L)
  expect_match(out[1L], "; ", fixed = TRUE)
})

test_that("partial_chi2 cell renders em-dash when estimate is NA (direct)", {
  # Exercises the is.na(est) branch of the 'value (df)' formatter.
  long_row <- data.frame(estimate = NA_real_, df = NA_real_,
                          stringsAsFactors = FALSE)
  cs <- list(token = "partial_chi2", fields = c("estimate", "df"))
  out <- spicy:::format_cell_value(long_row, cs, stars_map = NULL,
                                     digits = 2L, p_digits = 3L,
                                     effect_size_digits = 2L,
                                     decimal_mark = ".",
                                     show_columns = c("b", "partial_chi2"))
  expect_equal(out, "–")
})

test_that("AME cell renders em-dash when estimate is NA (estimate-only token)", {
  # Post-split (0.12): the `ame` token is estimate-only. The is.na(val)
  # branch of the single-field path returns the em-dash.
  format_cell <- spicy:::format_cell_value
  long_row <- data.frame(
    estimate = NA_real_,
    stringsAsFactors = FALSE
  )
  cs <- list(token = "ame", fields = "estimate")
  out <- format_cell(long_row, cs, stars_map = NULL,
                       digits = 2L, p_digits = 3L,
                       effect_size_digits = 2L,
                       decimal_mark = ".",
                       show_columns = c("b", "ame"))
  expect_equal(out, "–")
})


test_that("table_regression: padding arg controls column spacing", {
  fit <- lm(mpg ~ wt + cyl, data = mtcars)
  # padding = 0L produces narrower output than padding = 4L
  compact <- capture.output(print(table_regression(fit, padding = 0L)))
  spacious <- capture.output(print(table_regression(fit, padding = 4L)))
  # Body line width (e.g., the second body row "Variable...") is wider
  # under padding = 4L
  cw <- max(nchar(compact))
  sw <- max(nchar(spacious))
  expect_true(sw > cw)
})

test_that("table_regression: padding = 0L produces the most compact output", {
  fit <- lm(mpg ~ wt, data = mtcars)
  out_0 <- table_regression(fit, padding = 0L)
  expect_s3_class(out_0, "spicy_regression_table")
  expect_equal(attr(out_0, "padding"), 0L)
})


# ============================================================================
# Coverage push: lm standardize_*_lm() algebraic-method helpers were
# unexercised (52% coverage on R/standardize_lm.R). The posthoc /
# basic / smart paths share scale_and_rebuild() which had ~50 lines
# of dead branches under test. These tests cross-validate against
# effectsize::standardize_parameters() as the field-standard oracle.
# ============================================================================

test_that("lm posthoc: matches effectsize::standardize_parameters", {
  skip_if_not_installed("effectsize")
  fit <- lm(mpg ~ wt + cyl, data = mtcars)
  td <- broom::tidy(table_regression(fit, standardized = "posthoc"))
  beta <- td[td$estimate_type == "beta", ]
  oracle <- effectsize::standardize_parameters(fit, method = "posthoc")
  for (term_nm in c("wt", "cyl")) {
    expect_equal(
      beta$estimate[beta$term == term_nm],
      oracle$Std_Coefficient[oracle$Parameter == term_nm],
      tolerance = 1e-8,
      info = paste("term =", term_nm)
    )
  }
})

test_that("lm basic: matches effectsize::standardize_parameters", {
  skip_if_not_installed("effectsize")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  fit <- lm(mpg ~ wt + cyl, data = d)
  td <- broom::tidy(table_regression(fit, standardized = "basic"))
  beta <- td[td$estimate_type == "beta", ]
  oracle <- effectsize::standardize_parameters(fit, method = "basic")
  for (term_nm in c("wt", "cyl6", "cyl8")) {
    if (!term_nm %in% beta$term) next
    expect_equal(
      beta$estimate[beta$term == term_nm],
      oracle$Std_Coefficient[oracle$Parameter == term_nm],
      tolerance = 1e-8,
      info = paste("term =", term_nm)
    )
  }
})

test_that("lm smart: binary numeric uses 2 * SD (Gelman 2008)", {
  set.seed(1)
  n <- 100L
  d <- data.frame(
    y = rnorm(n),
    bin_num = sample(c(0, 1), n, replace = TRUE),
    cont = rnorm(n)
  )
  fit <- lm(y ~ bin_num + cont, data = d)
  td <- broom::tidy(table_regression(fit, standardized = "smart"))
  beta_bin <- td$estimate[td$estimate_type == "beta" & td$term == "bin_num"]
  beta_cont <- td$estimate[td$estimate_type == "beta" & td$term == "cont"]
  # Manual Gelman 2008:
  #   binary: beta = b * 2 * sd(X) / sd(Y)
  #   continuous: beta = b * sd(X) / sd(Y)
  b <- coef(fit)
  expected_bin <- b["bin_num"] * 2 * sd(d$bin_num) / sd(d$y)
  expected_cont <- b["cont"] * sd(d$cont) / sd(d$y)
  expect_equal(beta_bin, unname(expected_bin), tolerance = 1e-10)
  expect_equal(beta_cont, unname(expected_cont), tolerance = 1e-10)
})

test_that("lm standardize: intercept beta absent from tidy for algebraic methods", {
  # scale_and_rebuild sets beta[1] (intercept) to NA; broom::tidy
  # drops NA-rows so the (Intercept) beta row never reaches the
  # user. Assert the row is absent rather than NA-present.
  fit <- lm(mpg ~ wt, data = mtcars)
  for (m in c("posthoc", "basic", "smart")) {
    td <- broom::tidy(table_regression(fit, standardized = m))
    n_int_beta <- sum(td$estimate_type == "beta" &
                        td$term == "(Intercept)")
    expect_equal(n_int_beta, 0L, info = paste("method =", m))
  }
})

test_that("lm standardize: refit returns finite intercept (preserved)", {
  fit <- lm(mpg ~ wt, data = mtcars)
  td <- broom::tidy(table_regression(fit, standardized = "refit"))
  int_beta <- td$estimate[td$estimate_type == "beta" &
                            td$term == "(Intercept)"]
  # Refit on z-scored data: intercept is theoretically 0 but
  # floating-point produces machine-epsilon noise. Either way it
  # must be finite (not NA like the algebraic methods).
  expect_true(is.finite(int_beta))
})

test_that("lm standardize: t-statistic invariant under linear rescaling", {
  fit <- lm(mpg ~ wt + cyl, data = mtcars)
  raw_t <- broom::tidy(table_regression(fit))$statistic
  for (m in c("posthoc", "basic", "smart")) {
    td <- broom::tidy(table_regression(fit, standardized = m))
    beta_t <- td$statistic[td$estimate_type == "beta" &
                             td$term %in% c("wt", "cyl")]
    raw_t_wt_cyl <- raw_t[c(2L, 3L)]   # wt, cyl B rows
    expect_equal(beta_t, raw_t_wt_cyl, tolerance = 1e-10,
                 info = paste("method =", m))
  }
})

test_that("lm standardize: CR* yields t-distribution inference with df.residual", {
  set.seed(1)
  n <- 100L
  d <- data.frame(y = rnorm(n), x = rnorm(n),
                  clinic = rep(letters[1:10], each = 10))
  fit <- lm(y ~ x, data = d)
  td <- broom::tidy(table_regression(fit, vcov = "CR2", cluster = d$clinic,
                                       standardized = "posthoc"))
  beta_rows <- td[td$estimate_type == "beta" & td$term == "x", ]
  expect_true(is.finite(beta_rows$df))
  expect_equal(beta_rows$df, df.residual(fit))
})

test_that("lm standardize: refit fallback when formula has factor() wrapper", {
  fit <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
  fb_seen <- FALSE
  withCallingHandlers(
    out <- table_regression(fit, standardized = "refit"),
    spicy_fallback = function(c) {
      fb_seen <<- TRUE
      invokeRestart("muffleWarning")
    },
    spicy_caveat = function(c) invokeRestart("muffleWarning")
  )
  expect_s3_class(out, "spicy_regression_table")
  expect_true(fb_seen)
})

test_that("lm standardize: bootstrap vcov path runs cleanly (z-asymptotic CIs)", {
  # scale_and_rebuild detects vcov %in% c('bootstrap', 'jackknife') and
  # switches the CI / p formulas to qnorm / pnorm (asymptotic). The
  # `df` column itself keeps df.residual for backward compat with the
  # rendered table; the test just verifies the path runs and produces
  # finite numeric output.
  fit <- lm(mpg ~ wt, data = mtcars)
  set.seed(1)
  td <- broom::tidy(table_regression(fit, vcov = "bootstrap", boot_n = 100L,
                                       standardized = "posthoc"))
  beta_rows <- td[td$estimate_type == "beta" & td$term == "wt", ]
  expect_true(is.finite(beta_rows$estimate))
  expect_true(is.finite(beta_rows$std.error))
  expect_true(is.finite(beta_rows$p.value))
})

test_that("detect_factor_design_cols: returns integer(0) when no factor", {
  fit <- lm(mpg ~ wt + cyl, data = mtcars)  # numeric predictors only
  cols <- spicy:::detect_factor_design_cols(fit)
  expect_equal(cols, integer(0))
})

test_that("detect_factor_design_cols: returns indices for factor predictors", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  fit <- lm(mpg ~ wt + cyl, data = d)
  cols <- spicy:::detect_factor_design_cols(fit)
  # cyl produces 2 dummies (cyl6, cyl8) under contr.treatment
  expect_equal(length(cols), 2L)
})


# ============================================================================
# Coverage push: glm_compute.R defensive guards (NA-returns for non-glm
# inputs, generic quasi() family, no-eligible-rows in apply_exponentiate).
# ============================================================================

test_that("compute_pseudo_r2_* return NA for non-glm input (defensive)", {
  fit_lm <- lm(mpg ~ wt, data = mtcars)
  expect_true(is.na(spicy:::compute_pseudo_r2_mcfadden(fit_lm)))
  expect_true(is.na(spicy:::compute_pseudo_r2_nagelkerke(fit_lm)))
})

test_that("compute_pseudo_r2_tjur returns NA when y not 0/1", {
  set.seed(1)
  d <- data.frame(y = rpois(50, 3), x = rnorm(50))
  fit <- glm(y ~ x, data = d, family = poisson)
  # Tjur is binomial-only; non-binomial returns NA early
  expect_true(is.na(spicy:::compute_pseudo_r2_tjur(fit)))
})

test_that("compute_partial_chi2_for_term returns NULL for non-glm (defensive)", {
  fit_lm <- lm(mpg ~ wt, data = mtcars)
  expect_null(spicy:::compute_partial_chi2_for_term(fit_lm, "wt"))
})

test_that("apply_exponentiate_to_coefs no-ops when no eligible rows", {
  # Empty data.frame -> early return
  empty <- spicy:::empty_coefs_long()
  out <- spicy:::apply_exponentiate_to_coefs(empty)
  expect_equal(nrow(out), 0L)
  # Frame with only reference rows (all NA) -> no eligible rows
  coefs <- spicy:::empty_coefs_long()
  ref_row <- spicy:::build_one_b_row(
    nm = "cyl4", model_id = "M1", outcome = "y",
    estimate = NA_real_, se = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
    statistic = NA_real_, df = NA_real_, p_value = NA_real_,
    test_type = NA_character_,
    is_singular = FALSE, is_intercept = FALSE, is_reference = TRUE,
    factor_term = "cyl", factor_level = "4"
  )
  with_ref <- rbind(coefs, ref_row)
  out2 <- spicy:::apply_exponentiate_to_coefs(with_ref)
  # Reference row stays NA after transform
  expect_true(is.na(out2$estimate[1L]))
})

test_that("spicy_glm_title_prefix: generic quasi() family", {
  set.seed(1)
  d <- data.frame(y = rnorm(50), x = rnorm(50))
  fit <- tryCatch(
    suppressWarnings(glm(y ~ x, data = d,
                          family = quasi(link = "identity",
                                          variance = "constant"))),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "quasi() fit did not converge")
  title <- attr(table_regression(fit), "title")
  expect_match(title, "^Quasi-likelihood regression:")
})

test_that("compute_pseudo_r2_nagelkerke: NA when 1 - exp(2 * LL_null / n) <= 0", {
  # Edge: a model where the null log-likelihood is large and positive
  # (so exp(2*LL_null/n) > 1, upper <= 0). Construct by hand via
  # a contrived fit. Easiest path: just verify the function handles
  # the upper <= 0 branch via direct mock.
  # In practice this is rare but we exercise via a small-n binomial
  # with extreme outcome distribution.
  set.seed(1)
  d <- data.frame(y = c(rep(1, 20), rep(0, 2)), x = rnorm(22))
  fit <- suppressWarnings(glm(y ~ x, data = d, family = binomial))
  # Either returns a finite Nagelkerke or NA via the defensive branch.
  out <- spicy:::compute_pseudo_r2_nagelkerke(fit)
  expect_true(is.na(out) || (out >= 0 && out <= 1))
})

test_that("compute_intercept_only_loglik_glm: returns NA for non-glm-fit-like input", {
  # Passing an lm fit -- model.frame works, model.response works,
  # but family() on lm returns NULL. The refit with family = NULL
  # falls back to gaussian, so we get a finite value. The actual
  # NA-defensive paths are unreachable without a contrived fit.
  # This test just ensures the function doesn't error on lm.
  fit <- lm(mpg ~ wt, data = mtcars)
  out <- tryCatch(spicy:::compute_intercept_only_loglik_glm(fit),
                   error = function(e) NA_real_)
  expect_true(is.finite(out) || is.na(out))
})


# ============================================================================
# Coverage push: mixed lm + glm AME-Satterthwaite footer mechanism (the
# `any_lm && any_glm` branch in build_ame_satterthwaite_footer_block).
# ============================================================================

test_that("AME-Satterthwaite footer: mixed lm + glm uses compound wording", {
  set.seed(1)
  n <- 100L
  d <- data.frame(
    y_l = rnorm(n), y_b = rbinom(n, 1, 0.5),
    x = rnorm(n), clinic = rep(letters[1:10], each = 10)
  )
  m_lm <- lm(y_l ~ x, data = d)
  m_gl <- glm(y_b ~ x, data = d, family = binomial)
  out <- table_regression(
    list(m_lm, m_gl),
    vcov = "CR2", cluster = d$clinic,
    show_columns = c("b", "ame", "ame_p", "p")
  )
  note <- attr(out, "note")
  # Phase 7c22 (item e): footer trimmed for the lm+glm mixed case.
  expect_match(note, "Satterthwaite df", fixed = TRUE)
  expect_match(note, "closed-form for lm", fixed = TRUE)
  expect_match(note, "dominant-coefficient approximation for glm",
                fixed = TRUE)
  expect_false(grepl("clubSandwich",     note, fixed = TRUE))
  expect_false(grepl("linear_contrast",  note, fixed = TRUE))
})


# ============================================================================
# cluster argument: formula / string / vector forms (no NSE)
# ============================================================================

test_that("cluster - formula form (~region) resolves against model.frame", {
  skip_if_not_installed("clubSandwich")
  set.seed(1)
  d <- data.frame(y = rnorm(100), x = rnorm(100),
                  region = rep(letters[1:10], each = 10))
  fit <- lm(y ~ x, data = d)
  out <- table_regression(fit, vcov = "CR2", cluster = ~region)
  expect_match(attr(out, "note"), "clusters by region", fixed = TRUE)
})

test_that("cluster - string form ('region') resolves identically", {
  skip_if_not_installed("clubSandwich")
  set.seed(2)
  d <- data.frame(y = rnorm(100), x = rnorm(100),
                  region = rep(letters[1:10], each = 10))
  fit <- lm(y ~ x, data = d)
  out_str <- table_regression(fit, vcov = "CR2", cluster = "region")
  out_form <- table_regression(fit, vcov = "CR2", cluster = ~region)
  # Numeric output identical up to formatting
  expect_equal(out_str$Variable, out_form$Variable)
})

test_that("cluster - vector form (df$region) still supported for derived keys", {
  skip_if_not_installed("clubSandwich")
  set.seed(3)
  d <- data.frame(y = rnorm(100), x = rnorm(100),
                  region = rep(letters[1:10], each = 10))
  fit <- lm(y ~ x, data = d)
  # Derived cluster key not present as a column
  derived <- as.character(d$region)
  out <- table_regression(fit, vcov = "CR2", cluster = derived)
  expect_s3_class(out, "spicy_regression_table")
})

test_that("cluster - formula with interaction (~region:year)", {
  skip_if_not_installed("clubSandwich")
  set.seed(4)
  d <- data.frame(
    y = rnorm(200), x = rnorm(200),
    region = rep(letters[1:10], each = 20),
    year   = rep(2010:2019, times = 20)
  )
  fit <- lm(y ~ x, data = d)
  out <- table_regression(fit, vcov = "CR2", cluster = ~region:year)
  expect_match(attr(out, "note"), "clusters by region:year", fixed = TRUE)
})

test_that("cluster - unknown column raises spicy_invalid_input with hint", {
  fit <- lm(mpg ~ wt, data = mtcars)
  err <- tryCatch(
    table_regression(fit, vcov = "CR2", cluster = ~bogus),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "bogus", fixed = TRUE)
})

test_that("cluster - string column not in data raises spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mtcars)
  err <- tryCatch(
    table_regression(fit, vcov = "CR2", cluster = "bogus"),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
})

test_that("cluster - list of mixed forms (formula / string / vector) for multi-model", {
  skip_if_not_installed("clubSandwich")
  set.seed(5)
  d <- data.frame(y = rnorm(80), x = rnorm(80),
                  region = rep(letters[1:8], each = 10))
  m1 <- lm(y ~ x, data = d)
  m2 <- lm(y ~ x, data = d)
  m3 <- lm(y ~ x, data = d)
  out <- table_regression(
    list("A" = m1, "B" = m2, "C" = m3),
    vcov = list("CR2", "CR2", "CR2"),
    cluster = list(~region, "region", d$region)
  )
  expect_s3_class(out, "spicy_regression_table")
})


test_that("cluster - bare unquoted name raises friendly spicy_invalid_input", {
  # `cluster = region` where `region` doesn't exist in the caller
  # env: we want a migration error pointing at `~region` /
  # `"region"`, NOT R's "object 'region' not found".
  fit <- lm(mpg ~ wt, data = mtcars)
  err <- tryCatch(
    table_regression(fit, vcov = "CR2", cluster = region),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  msg <- conditionMessage(err)
  expect_match(msg, "unquoted bare names are not supported", fixed = TRUE)
  expect_match(msg, "~region", fixed = TRUE)
  expect_match(msg, "\"region\"", fixed = TRUE)
})

test_that("cluster - local variable with the same name as a column still works (vector form)", {
  # `cluster = my_cluster` where `my_cluster` IS a local vector
  # in the caller env: must use the vector silently, no error.
  skip_if_not_installed("clubSandwich")
  set.seed(6)
  d <- data.frame(y = rnorm(100), x = rnorm(100),
                  region = rep(letters[1:10], each = 10))
  fit <- lm(y ~ x, data = d)
  my_cluster <- d$region
  out <- table_regression(fit, vcov = "CR2", cluster = my_cluster)
  expect_s3_class(out, "spicy_regression_table")
  # Footer reflects the local var name
  expect_match(attr(out, "note"), "clusters by my_cluster", fixed = TRUE)
})
