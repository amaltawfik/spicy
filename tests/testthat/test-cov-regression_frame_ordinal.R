# ---------------------------------------------------------------------------
# Coverage top-up for R/regression_frame_ordinal.R.
#
# Targets branches the main test-regression_frame_ordinal.R suite leaves
# uncovered:
#   * Non-logit link arms of .polr_link_short / .polr_link_title
#     (probit / cloglog / loglog / cauchit) via real polr fits.
#   * Non-logit link arms of .clm_link_title (probit / cloglog / loglog /
#     cauchit) plus the switch fallback arm via a flexible "Aranda-Ordaz"
#     clm link, which is NOT in the named switch.
#   * .clm_coefs() empty-beta early return via an intercept-only clm.
#   * .ordinal_reference_rows() with (a) no factor terms (continuous-only
#     predictors) and (b) factor terms whose reference is NOT dropped
#     (ordered predictor -> polynomial contrast), exercising the `next`
#     skip and the final empty-rows return.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_polr_link <- function(method) {
  skip_if_not_installed("MASS")
  data(housing, package = "MASS", envir = environment())
  MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing,
             Hess = TRUE, method = method)
}

.fit_clm_link <- function(link) {
  skip_if_not_installed("ordinal")
  ordinal::clm(rating ~ temp + contact, data = ordinal::wine, link = link)
}


# ---- 1. polr non-logit links: short code + title -------------------------

test_that("polr probit fit reports the probit link and title", {
  fit <- .fit_polr_link("probit")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$link, "probit")
  expect_match(fr$info$extras$title_prefix, "Cumulative probit", fixed = TRUE)
  # A probit model has no odds: the assumption suffix is the
  # link-neutral parallel-slopes name, never "proportional odds".
  expect_identical(fr$info$extras$title_prefix,
                   "Cumulative probit regression (parallel slopes)")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("the assumption suffix is named by the link across engines", {
  # polr logit keeps the canonical proportional-odds name.
  fit_l <- .fit_polr_link("logistic")
  fr_l <- as_regression_frame(fit_l, model_id = "M1")
  expect_identical(fr_l$info$extras$title_prefix,
                   "Cumulative logit regression (proportional odds)")
  # clm shares the mapping: probit -> parallel slopes.
  skip_if_not_installed("ordinal")
  fit_cp <- .fit_clm_link("probit")
  fr_cp <- as_regression_frame(fit_cp, model_id = "M1")
  expect_match(fr_cp$info$extras$title_prefix, "(parallel slopes)",
               fixed = TRUE)
  expect_false(grepl("proportional odds", fr_cp$info$extras$title_prefix,
                     fixed = TRUE))
})

test_that("polr cloglog fit reports the cloglog link and title", {
  fit <- .fit_polr_link("cloglog")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$link, "cloglog")
  expect_match(fr$info$extras$title_prefix, "Cumulative cloglog", fixed = TRUE)
  # Consistency with the sibling probit test: the constructed frame must
  # pass the schema validator (the gap the audit flagged).
  expect_invisible(spicy:::validate_regression_frame(fr))
  # Family is a cumulative-link model; the assumption suffix is named
  # by the link -- the cloglog cumulative model is the grouped
  # proportional-hazards model (McCullagh 1980); "proportional odds"
  # only exists under logit.
  expect_identical(fr$info$family$family, "cumulative")
  expect_identical(fr$info$extras$title_prefix,
                   "Cumulative cloglog regression (proportional hazards)")
  # cloglog is a non-canonical link but exponentiation is still offered
  # (hazard-ratio interpretation), shared by all cumulative-link fits.
  expect_true(isTRUE(fr$info$supports$exponentiate))
  # housing's Sat has 3 ordered levels -> (k - 1) = 2 cumulative
  # thresholds, named for the adjacent-level cutpoints.
  expect_identical(fr$info$extras$response_levels,
                   c("Low", "Medium", "High"))
  expect_identical(nrow(fr$info$extras$thresholds),
                   length(fit$lev) - 1L)
  expect_identical(fr$info$extras$thresholds$term,
                   c("Low|Medium", "Medium|High"))
  # The three factor predictors (Infl, Type, Cont) each contribute one
  # synthesised reference row; structural content, not just a count.
  expect_identical(sum(fr$coefs$is_ref), 3L)
  expect_identical(fr$coefs$term[fr$coefs$is_ref],
                   c("InflLow", "TypeTower", "ContLow"))
  # Reference rows carry NA estimates and z-asymptotic test type for the
  # estimated rows.
  expect_true(all(is.na(fr$coefs$estimate[fr$coefs$is_ref])))
  expect_true(all(fr$coefs$test_type[!fr$coefs$is_ref] == "z"))
})

test_that("polr loglog fit reports the loglog link and title", {
  fit <- .fit_polr_link("loglog")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$link, "loglog")
  expect_match(fr$info$extras$title_prefix, "Cumulative loglog", fixed = TRUE)
  # Consistency with the sibling probit test: the constructed frame must
  # pass the schema validator (the gap the audit flagged).
  expect_invisible(spicy:::validate_regression_frame(fr))
  # Family is a cumulative-link model; loglog has no odds (or hazard)
  # reading, so the suffix falls to the link-neutral parallel-slopes
  # name of the shared restriction.
  expect_identical(fr$info$family$family, "cumulative")
  expect_identical(fr$info$extras$title_prefix,
                   "Cumulative loglog regression (parallel slopes)")
  expect_true(isTRUE(fr$info$supports$exponentiate))
  # housing's Sat has 3 ordered levels -> (k - 1) = 2 cumulative
  # thresholds, named for the adjacent-level cutpoints.
  expect_identical(fr$info$extras$response_levels,
                   c("Low", "Medium", "High"))
  expect_identical(nrow(fr$info$extras$thresholds),
                   length(fit$lev) - 1L)
  expect_identical(fr$info$extras$thresholds$term,
                   c("Low|Medium", "Medium|High"))
  # The three factor predictors (Infl, Type, Cont) each contribute one
  # synthesised reference row; structural content, not just a count.
  expect_identical(sum(fr$coefs$is_ref), 3L)
  expect_identical(fr$coefs$term[fr$coefs$is_ref],
                   c("InflLow", "TypeTower", "ContLow"))
  expect_true(all(is.na(fr$coefs$estimate[fr$coefs$is_ref])))
  expect_true(all(fr$coefs$test_type[!fr$coefs$is_ref] == "z"))
})

test_that("polr cauchit fit reports the cauchit link and title", {
  skip_if_not_installed("MASS")
  set.seed(42)
  df <- data.frame(y = ordered(sample(1:3, 200, replace = TRUE)),
                   x = rnorm(200))
  fit <- MASS::polr(y ~ x, data = df, Hess = TRUE, method = "cauchit")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$link, "cauchit")
  expect_match(fr$info$extras$title_prefix, "Cumulative cauchit", fixed = TRUE)
})


# ---- 2. clm non-logit links: title --------------------------------------

test_that("clm probit fit reports the probit link and title", {
  fit <- .fit_clm_link("probit")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$link, "probit")
  expect_match(fr$info$extras$title_prefix, "Cumulative probit", fixed = TRUE)
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("clm cloglog fit reports the cloglog link and title", {
  fit <- .fit_clm_link("cloglog")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$link, "cloglog")
  expect_match(fr$info$extras$title_prefix, "Cumulative cloglog", fixed = TRUE)
})

test_that("clm loglog fit reports the loglog link and title", {
  fit <- .fit_clm_link("loglog")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$link, "loglog")
  expect_match(fr$info$extras$title_prefix, "Cumulative loglog", fixed = TRUE)
})

test_that("clm cauchit fit reports the cauchit link and title", {
  fit <- .fit_clm_link("cauchit")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$link, "cauchit")
  expect_match(fr$info$extras$title_prefix, "Cumulative cauchit", fixed = TRUE)
})

test_that("clm flexible Aranda-Ordaz link hits the switch fallback title", {
  skip_if_not_installed("ordinal")
  # Aranda-Ordaz is a valid clm link but is NOT in the .clm_link_title
  # switch, so it exercises the paste0() fallback arm.
  fit <- suppressWarnings(
    ordinal::clm(rating ~ temp + contact, data = ordinal::wine,
                 link = "Aranda-Ordaz"))
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$link, "Aranda-Ordaz")
  # A flexible-link family has no odds reading: link-neutral suffix.
  expect_identical(fr$info$extras$title_prefix,
                   "Cumulative Aranda-Ordaz regression (parallel slopes)")
})


# ---- 3. clm intercept-only: empty-beta early return ----------------------

test_that("intercept-only clm yields an empty coefs frame (no predictors)", {
  skip_if_not_installed("ordinal")
  fit <- ordinal::clm(rating ~ 1, data = ordinal::wine)
  expect_length(fit$beta, 0L)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(nrow(fr$coefs), 0L)
  # Schema must still validate and the empty coefs frame must carry the
  # canonical column set.
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_true(all(
    c("term", "parent_var", "label", "estimate", "is_ref") %in%
      colnames(fr$coefs)))
  # Thresholds are still present even with no predictors.
  expect_identical(nrow(fr$info$extras$thresholds),
                   length(fit$y.levels) - 1L)
})


# ---- 3b. clm non-flexible threshold parameterizations: correct SEs -------
#
# Audit hypothesis (carried over from an older `ordinal` internal layout)
# was that threshold = "equidistant" / "symmetric" would name fit$alpha
# with full per-cutpoint tags ("1|2", "2|3", ...) while summary()/vcov()
# used the REDUCED tags ("threshold.1", "spacing", ...), forcing
# .clm_thresholds() into its else-branch and yielding NA SEs.
#
# On the installed `ordinal` (>= 2025.12.29) that premise is FALSE:
# fit$alpha, summary(fit)$coefficients, and vcov(fit) all carry the SAME
# reduced names, so the PRIMARY branch is taken and the reported threshold
# SEs equal the model's own reported threshold SEs (read from summary).
# These tests pin that correct behavior from first principles: the
# extracted threshold SE for parameter p must equal sqrt(vcov(fit)[p, p])
# and must equal summary(fit)$coefficients[p, "Std. Error"], with no NAs.

test_that("clm equidistant thresholds get correct, non-NA SEs", {
  skip_if_not_installed("ordinal")
  fit <- ordinal::clm(rating ~ temp + contact, data = ordinal::wine,
                      threshold = "equidistant")
  th <- spicy:::.clm_thresholds(fit)
  alpha_names <- names(fit$alpha)
  # Equidistant uses the reduced (threshold.1, spacing) parameterization.
  expect_identical(th$term, alpha_names)
  expect_identical(th$term, c("threshold.1", "spacing"))
  expect_false(any(is.na(th$std_error)))
  expect_false(any(is.na(th$statistic)))
  expect_false(any(is.na(th$p_value)))
  # First-principles oracle 1: SE = sqrt of the diagonal of vcov().
  V <- as.matrix(stats::vcov(fit))
  expect_equal(th$std_error, unname(sqrt(diag(V)[alpha_names])))
  # First-principles oracle 2: SE = the model's own reported threshold SE.
  sm <- summary(fit)$coefficients
  expect_equal(th$std_error, unname(sm[alpha_names, "Std. Error"]))
  # statistic = estimate / SE; p = two-sided Wald-z.
  expect_equal(th$statistic, th$estimate / th$std_error)
  expect_equal(th$p_value, 2 * stats::pnorm(-abs(th$statistic)))
})

test_that("clm symmetric thresholds get correct, non-NA SEs", {
  skip_if_not_installed("ordinal")
  fit <- ordinal::clm(rating ~ temp + contact, data = ordinal::wine,
                      threshold = "symmetric")
  th <- spicy:::.clm_thresholds(fit)
  alpha_names <- names(fit$alpha)
  # Symmetric uses the reduced (central.1, central.2, spacing.1) layout.
  expect_identical(th$term, alpha_names)
  expect_identical(th$term, c("central.1", "central.2", "spacing.1"))
  expect_false(any(is.na(th$std_error)))
  V <- as.matrix(stats::vcov(fit))
  expect_equal(th$std_error, unname(sqrt(diag(V)[alpha_names])))
  sm <- summary(fit)$coefficients
  expect_equal(th$std_error, unname(sm[alpha_names, "Std. Error"]))
  expect_equal(th$statistic, th$estimate / th$std_error)
  expect_equal(th$p_value, 2 * stats::pnorm(-abs(th$statistic)))
})

test_that("clm flexible thresholds get correct per-cutpoint SEs", {
  skip_if_not_installed("ordinal")
  fit <- ordinal::clm(rating ~ temp + contact, data = ordinal::wine,
                      threshold = "flexible")
  th <- spicy:::.clm_thresholds(fit)
  # Flexible names cutpoints "1|2", "2|3", ... -- (k - 1) of them.
  expect_identical(th$term, c("1|2", "2|3", "3|4", "4|5"))
  expect_false(any(is.na(th$std_error)))
  sm <- summary(fit)$coefficients
  expect_equal(th$std_error, unname(sm[th$term, "Std. Error"]))
})


# ---- 4. .ordinal_reference_rows(): no factor terms -----------------------

test_that("continuous-only polr fit synthesises no reference rows", {
  skip_if_not_installed("MASS")
  set.seed(1)
  df <- data.frame(y = ordered(sample(1:3, 150, replace = TRUE)),
                   x1 = rnorm(150), x2 = rnorm(150))
  fit <- MASS::polr(y ~ x1 + x2, data = df, Hess = TRUE)
  rr <- spicy:::.ordinal_reference_rows(fit)
  expect_identical(nrow(rr), 0L)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(sum(fr$coefs$is_ref), 0L)
  expect_identical(nrow(fr$coefs), 2L)  # x1, x2 -- no ref rows
})


# ---- 5. .ordinal_reference_rows(): factor without a dropped reference ----

test_that("ordered (polynomial-contrast) predictor synthesises no ref rows", {
  skip_if_not_installed("MASS")
  # An ordered() predictor uses contr.poly: coefs are .L/.Q trends with
  # NO dropped reference level. detect_factor_terms() flags the term with
  # reference_dropped = FALSE, so .ordinal_reference_rows() skips it
  # (the `next` arm) and returns the empty frame.
  set.seed(7)
  n <- 300
  xo <- ordered(sample(c("lo", "mid", "hi"), n, replace = TRUE),
                levels = c("lo", "mid", "hi"))
  df <- data.frame(y = ordered(sample(1:4, n, replace = TRUE)), xo = xo)
  fit <- MASS::polr(y ~ xo, data = df, Hess = TRUE)
  # Confirm the precondition: a factor term with reference_dropped = FALSE.
  fts <- spicy:::detect_factor_terms(fit)
  expect_true(any(vapply(fts, function(f) !isTRUE(f$reference_dropped),
                         logical(1))))
  rr <- spicy:::.ordinal_reference_rows(fit)
  expect_identical(nrow(rr), 0L)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(sum(fr$coefs$is_ref), 0L)
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("ordered (polynomial-contrast) predictor also skips ref rows in clm", {
  skip_if_not_installed("ordinal")
  set.seed(7)
  n <- 300
  xo <- ordered(sample(c("lo", "mid", "hi"), n, replace = TRUE),
                levels = c("lo", "mid", "hi"))
  df <- data.frame(y = ordered(sample(1:4, n, replace = TRUE)), xo = xo)
  fit <- ordinal::clm(y ~ xo, data = df)
  rr <- spicy:::.ordinal_reference_rows(fit)
  expect_identical(nrow(rr), 0L)
})

test_that("has_weights detects non-uniform prior weights for polr AND clm", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  data(housing, package = "MASS")
  # Neither class stores $weights; the previous polr check was always
  # FALSE and clm hardcoded FALSE.
  p_w  <- MASS::polr(Sat ~ Infl, weights = Freq, data = housing, Hess = TRUE)
  p_nw <- MASS::polr(Sat ~ Infl, data = housing, Hess = TRUE)
  c_w  <- ordinal::clm(Sat ~ Infl, weights = Freq, data = housing)
  c_nw <- ordinal::clm(Sat ~ Infl, data = housing)
  expect_true(as_regression_frame(p_w)$info$extras$has_weights)
  expect_false(as_regression_frame(p_nw)$info$extras$has_weights)
  expect_true(as_regression_frame(c_w)$info$extras$has_weights)
  expect_false(as_regression_frame(c_nw)$info$extras$has_weights)
})
