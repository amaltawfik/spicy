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
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("polr cloglog fit reports the cloglog link and title", {
  fit <- .fit_polr_link("cloglog")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$link, "cloglog")
  expect_match(fr$info$extras$title_prefix, "Cumulative cloglog", fixed = TRUE)
})

test_that("polr loglog fit reports the loglog link and title", {
  fit <- .fit_polr_link("loglog")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$link, "loglog")
  expect_match(fr$info$extras$title_prefix, "Cumulative loglog", fixed = TRUE)
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
  expect_identical(fr$info$extras$title_prefix,
                   "Cumulative Aranda-Ordaz regression (proportional odds)")
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
