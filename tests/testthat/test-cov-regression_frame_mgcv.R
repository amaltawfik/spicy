# ---------------------------------------------------------------------------
# Coverage top-up for R/regression_frame_mgcv.R.
#
# Targets the still-uncovered REACHABLE branches:
#   * .gam_coefs() empty-parametric early return  (no-intercept all-smooth fit)
#   * .gam_reference_rows() polynomial-contrast skip + empty return
#       (ordered-factor predictor -> .L/.Q contrasts, no reference row)
#   * .gam_smooth_terms() empty early return       (parametric-only fit)
#   * .gam_coefs() estimated-scale t-branch for NON-gaussian-identity families
#       (Gamma/quasipoisson/gaussian-log/tw): mgcv labels the parametric
#       p.table columns "t value"/"Pr(>|t|)" and references a t-distribution
#       on df.residual(fit). These fits must get real SE/stat/p-values, NOT
#       the all-NA else fallback.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_gam_no_parametric <- function() {
  skip_if_not_installed("mgcv")
  set.seed(1)
  dat <- mgcv::gamSim(1, n = 200, dist = "normal", scale = 2, verbose = FALSE)
  # No intercept + a single smooth: summary(fit)$p.coeff is length 0.
  mgcv::gam(y ~ s(x0) - 1, data = dat)
}

.fit_gam_ordered_factor <- function() {
  skip_if_not_installed("mgcv")
  set.seed(2)
  dat <- mgcv::gamSim(1, n = 200, dist = "normal", scale = 2, verbose = FALSE)
  dat$ogrp <- ordered(rep(c("lo", "mid", "hi"), length.out = nrow(dat)),
                      levels = c("lo", "mid", "hi"))
  # ordered() -> contr.poly: coefs ogrp.L, ogrp.Q (no dropped reference level).
  mgcv::gam(y ~ s(x0) + ogrp, data = dat)
}

.fit_gam_parametric_only <- function() {
  skip_if_not_installed("mgcv")
  set.seed(3)
  dat <- mgcv::gamSim(1, n = 200, dist = "normal", scale = 2, verbose = FALSE)
  # No smooth term: summary(fit)$s.table is NULL.
  mgcv::gam(y ~ x2 + x3, data = dat)
}

# Estimated-scale, NON-gaussian-identity family. summary.gam() labels the
# parametric p.table columns "t value"/"Pr(>|t|)" and references a
# t-distribution on df.residual(fit).
.fit_gam_estimated_scale <- function(family) {
  skip_if_not_installed("mgcv")
  set.seed(4)
  dat <- mgcv::gamSim(1, n = 200, dist = "normal", scale = 2, verbose = FALSE)
  dat$ypos <- dat$y - min(dat$y) + 1            # strictly positive response
  mgcv::gam(ypos ~ x2 + s(x0), data = dat, family = family)
}


# ---- 1. No parametric terms: empty coefs frame --------------------------

test_that("gam with no parametric terms yields a zero-row coefs frame", {
  fit <- .fit_gam_no_parametric()
  expect_length(summary(fit)$p.coeff, 0L)  # precondition for the branch

  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(nrow(fr$coefs), 0L)
  # Empty frame still carries the full schema columns.
  expect_true(all(c("term", "parent_var", "estimate", "std_error",
                    "is_ref", "estimate_type") %in% colnames(fr$coefs)))
})

test_that("gam with no parametric terms still produces a schema-valid frame", {
  fit <- .fit_gam_no_parametric()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  # The single smooth term is still summarised.
  expect_identical(fr$info$extras$n_smooth_terms, 1L)
})


# ---- 2. Ordered factor (polynomial contrasts): no reference row ---------

test_that("gam with ordered factor emits polynomial contrasts and NO reference row", {
  fit <- .fit_gam_ordered_factor()
  fr <- as_regression_frame(fit, model_id = "M1")

  # contr.poly -> .L and .Q coefficient names appear as ordinary B rows.
  poly_terms <- fr$coefs$term[grepl("^ogrp\\.", fr$coefs$term)]
  expect_setequal(poly_terms, c("ogrp.L", "ogrp.Q"))

  # A polynomial-coded factor has no dropped reference level, so the
  # reference-row builder must skip it: no is_ref row anywhere.
  expect_false(any(fr$coefs$is_ref))
  expect_identical(sum(fr$coefs$is_ref), 0L)
})

test_that("gam ordered-factor frame is schema-valid", {
  fit <- .fit_gam_ordered_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})


# ---- 3. Parametric-only fit: empty smooth-term table --------------------

test_that("gam with no smooth terms yields an empty smooth_terms table", {
  fit <- .fit_gam_parametric_only()
  expect_null(summary(fit)$s.table)  # precondition for the branch

  fr <- as_regression_frame(fit, model_id = "M1")
  st <- fr$info$extras$smooth_terms
  expect_s3_class(st, "data.frame")
  expect_identical(nrow(st), 0L)
  expect_identical(fr$info$extras$n_smooth_terms, 0L)
})

test_that("gam parametric-only frame keeps its parametric coefs and validates", {
  fit <- .fit_gam_parametric_only()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  # x2 and x3 parametric estimates are present.
  b_terms <- fr$coefs$term[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref]
  expect_true(all(c("x2", "x3") %in% b_terms))
})


# ---- 4. Estimated-scale family: real (non-NA) t-based inference ---------

test_that("Gamma(log) GAM exposes t-labelled p.table and t-based inference", {
  skip_if_not_installed("mgcv")
  fit <- .fit_gam_estimated_scale(stats::Gamma(link = "log"))

  # Precondition for the branch: family is NOT gaussian-identity, yet the
  # parametric p.table carries "t value"/"Pr(>|t|)" (estimated scale).
  fam <- stats::family(fit)
  expect_false(identical(fam$family, "gaussian") &&
               identical(fam$link, "identity"))
  sm <- summary(fit)
  expect_true(all(c("t value", "Pr(>|t|)") %in% colnames(sm$p.table)))
  expect_false(any(c("z value", "Pr(>|z|)") %in% colnames(sm$p.table)))

  fr <- as_regression_frame(fit, model_id = "M1")
  b  <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]

  # The bug this guards: the else fallback set these all-NA. They must be
  # populated for every parametric term.
  expect_false(any(is.na(b$std_error)))
  expect_false(any(is.na(b$statistic)))
  expect_false(any(is.na(b$p_value)))

  # Inference is the Wald-t reference, so test_type == "t" and df is the
  # residual df, NOT Inf.
  expect_true(all(b$test_type == "t"))
  dfr <- stats::df.residual(fit)
  expect_equal(unique(b$df), dfr)
  expect_true(is.finite(dfr))
})

test_that("Gamma(log) GAM std_error/statistic/p_value match mgcv p.table", {
  skip_if_not_installed("mgcv")
  fit <- .fit_gam_estimated_scale(stats::Gamma(link = "log"))
  sm  <- summary(fit)
  fr  <- as_regression_frame(fit, model_id = "M1")
  b   <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]

  # First-principles oracle: read straight from mgcv's parametric p.table.
  expect_equal(unname(b$std_error),
               unname(sm$p.table[b$term, "Std. Error"]))
  expect_equal(unname(b$statistic),
               unname(sm$p.table[b$term, "t value"]))
  expect_equal(unname(b$p_value),
               unname(sm$p.table[b$term, "Pr(>|t|)"]))

  # CI is estimate +/- qt(0.975, df.residual) * SE (the t-reference CI).
  tcrit <- stats::qt(0.975, df = stats::df.residual(fit))
  expect_equal(b$ci_lower, b$estimate - tcrit * b$std_error)
  expect_equal(b$ci_upper, b$estimate + tcrit * b$std_error)
})

test_that("other estimated-scale families also get non-NA t inference", {
  skip_if_not_installed("mgcv")
  # Use base-stats estimated-scale families: under load_all() the family
  # closures returned by some mgcv specials (e.g. tw()) capture an enclosing
  # environment that cannot resolve internal mgcv helpers, which is a test-
  # harness quirk unrelated to the branch under test. quasipoisson() and
  # gaussian(link = "log") both exercise the same t-labelled p.table path.
  families <- list(
    quasipoisson = stats::quasipoisson(),
    gaussian_log = stats::gaussian(link = "log")
  )
  for (fam in families) {
    fit <- .fit_gam_estimated_scale(fam)
    fr  <- as_regression_frame(fit, model_id = "M1")
    b   <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
    expect_false(any(is.na(b$std_error)))
    expect_false(any(is.na(b$statistic)))
    expect_false(any(is.na(b$p_value)))
    expect_true(all(b$test_type == "t"))
  }
})

test_that("known-scale family (poisson) still routes to the z-branch", {
  skip_if_not_installed("mgcv")
  set.seed(5)
  dat <- mgcv::gamSim(1, n = 200, dist = "poisson", scale = 0.1,
                      verbose = FALSE)
  fit <- mgcv::gam(y ~ x2 + s(x0), data = dat, family = poisson())
  sm  <- summary(fit)
  # Known scale -> z-labelled columns.
  expect_true(all(c("z value", "Pr(>|z|)") %in% colnames(sm$p.table)))

  fr <- as_regression_frame(fit, model_id = "M1")
  b  <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_false(any(is.na(b$std_error)))
  expect_false(any(is.na(b$p_value)))
  expect_true(all(b$test_type == "z"))
  expect_true(all(is.infinite(b$df)))
})
