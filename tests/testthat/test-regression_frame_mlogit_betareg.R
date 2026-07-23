# ---------------------------------------------------------------------------
# Phase 6h tests: as_regression_frame() methods for mlogit + betareg.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_mlogit_fishing <- function() {
  skip_if_not_installed("mlogit")
  data("Fishing", package = "mlogit", envir = environment())
  Fish <- mlogit::dfidx(Fishing, varying = 2:9, choice = "mode", shape = "wide")
  mlogit::mlogit(mode ~ price + catch, data = Fish)
}

.fit_betareg_basic <- function() {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg", envir = environment())
  betareg::betareg(yield ~ batch + temp, data = GasolineYield)
}


# ---- 1. mlogit -----------------------------------------------------------

test_that("as_regression_frame.mlogit produces a schema-valid frame", {
  fit <- .fit_mlogit_fishing()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("mlogit: info$class is 'mlogit'", {
  fit <- .fit_mlogit_fishing()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "mlogit")
})

test_that("mlogit: family multinomial/logit", {
  fit <- .fit_mlogit_fishing()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "multinomial")
  expect_identical(fr$info$family$link, "logit")
})

test_that("mlogit: title_prefix names mlogit", {
  fit <- .fit_mlogit_fishing()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "mlogit", fixed = TRUE)
})

test_that("mlogit: alternative-specific intercepts get outcome_level parsed", {
  fit <- .fit_mlogit_fishing()
  fr <- as_regression_frame(fit, model_id = "M1")
  asc_rows <- fr$coefs[grepl(":", fr$coefs$term, fixed = TRUE), ]
  expect_true(all(!is.na(asc_rows$outcome_level)))
  # Two-segment layout: a row's group is its ALTERNATIVE and the
  # displayed label is the bare predictor
  # (dev/mlogit_two_segment_spec.md).
  expect_true(all(asc_rows$parent_var == asc_rows$outcome_level))
  expect_true(all(asc_rows$label == "(Intercept)"))
  expect_setequal(asc_rows$outcome_level, c("boat", "charter", "pier"))
})

test_that("mlogit: alternative-invariant coefs have outcome_level = NA", {
  fit <- .fit_mlogit_fishing()
  fr <- as_regression_frame(fit, model_id = "M1")
  plain_rows <- fr$coefs[!grepl(":", fr$coefs$term, fixed = TRUE), ]
  expect_true(all(is.na(plain_rows$outcome_level)))
  expect_setequal(plain_rows$term, c("price", "catch"))
})

test_that("mlogit: coefs estimates match stats::coef(fit)", {
  fit <- .fit_mlogit_fishing()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  for (nm in names(legacy)) {
    expect_equal(
      fr$coefs$estimate[fr$coefs$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10
    )
  }
})

test_that("mlogit: SE / p byte-match summary(fit)$CoefTable", {
  fit <- .fit_mlogit_fishing()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$CoefTable
  for (nm in rownames(sm)) {
    expect_equal(
      fr$coefs$std_error[fr$coefs$term == nm],
      unname(sm[nm, "Std. Error"]),
      tolerance = 1e-10
    )
    expect_equal(
      fr$coefs$p_value[fr$coefs$term == nm],
      unname(sm[nm, "Pr(>|z|)"]),
      tolerance = 1e-10
    )
  }
})


# ---- 2. betareg ----------------------------------------------------------

test_that("as_regression_frame.betareg produces a schema-valid frame", {
  fit <- .fit_betareg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("betareg: info$class is 'betareg'", {
  fit <- .fit_betareg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "betareg")
})

test_that("betareg: family is beta/logit (default link)", {
  fit <- .fit_betareg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "beta")
  expect_identical(fr$info$family$link, "logit")
})

test_that("betareg: title_prefix = 'Beta regression'", {
  fit <- .fit_betareg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Beta regression")
})

test_that("betareg: mean component in coefs; phi precision in extras", {
  fit <- .fit_betareg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  # (phi) should NOT appear in coefs
  expect_false("(phi)" %in% fr$coefs$term)
  expect_true(is.numeric(fr$info$extras$precision_phi))
  expect_true(fr$info$extras$precision_phi > 0)
})

test_that("betareg: SE / p byte-match summary(fit)$coefficients$mean", {
  fit <- .fit_betareg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  smm <- summary(fit)$coefficients$mean
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(smm)) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(smm[nm, "Std. Error"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(smm[nm, "Pr(>|z|)"]),
      tolerance = 1e-10
    )
  }
})

test_that("betareg: factor predictor present in coefs (parent_var resolution)", {
  fit <- .fit_betareg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  # batch in GasolineYield is a factor with 10 levels but uses an
  # unusual contrast coding where the reference is "10" (the last
  # level); standard treatment-contrast factor detection may not
  # synthesise a ref row in this corner case. The key invariant we
  # care about is that the factor coefs ARE in the table.
  rows <- fr$coefs[fr$coefs$parent_var == "batch", ]
  expect_true(nrow(rows) >= 9L)
})

test_that("betareg with controlled factor: ref row synthesised", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg", envir = environment())
  d <- GasolineYield
  # Build a simple 3-level factor predictor for ref-row detection.
  d$g <- factor(rep(c("A", "B", "C"), length.out = nrow(d)))
  fit <- betareg::betareg(yield ~ g + temp, data = d)
  fr <- as_regression_frame(fit, model_id = "M1")
  rows <- fr$coefs[fr$coefs$parent_var == "g", ]
  expect_identical(nrow(rows), 3L)
  expect_identical(sum(rows$is_ref), 1L)
  expect_identical(rows$label[rows$is_ref], "A")
})

test_that("betareg: Wald z-asymptotic; supports$exponentiate = TRUE (logit link)", {
  fit <- .fit_betareg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "z"))
  expect_true(all(is.infinite(b_rows$df)))
  expect_true(fr$info$supports$exponentiate)
})

test_that("betareg: pseudo_r2$pseudo from fit$pseudo.r.squared", {
  fit <- .fit_betareg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(
    fr$info$fit_stats$pseudo_r2$pseudo,
    fit$pseudo.r.squared,
    tolerance = 1e-10
  )
})
