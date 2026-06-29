# ---------------------------------------------------------------------------
# Coverage top-up for R/regression_frame_survival.R
#
# Targets branches not exercised by test-regression_frame_survival.R:
#   * .check_survival_available() abort when `survival` is unavailable
#     (mocked probe) -- lines 103-109.
#   * .survival_reference_rows(): the `reference_dropped == FALSE` skip
#     (line 238 `next`) and the resulting empty-rows return (line 260),
#     both driven by an ORDERED factor whose polynomial contrasts have
#     no dropped reference level.
#   * .survreg_dist_title(): the loglogistic / exponential / logistic /
#     Student-t switch arms (lines 426, 427, 429, 430) via real survreg
#     fits, plus the title-case default fallback (line 431) for an
#     unrecognised distribution name.
# ---------------------------------------------------------------------------


# ---- 1. .check_survival_available(): abort when survival is missing -------

test_that(".check_survival_available aborts with spicy_missing_pkg when survival absent", {
  # Mock the availability probe so the guard fires even though survival is
  # installed in the test environment.
  testthat::local_mocked_bindings(spicy_pkg_available = function(pkg) FALSE)
  expect_error(
    .check_survival_available(),
    class = "spicy_missing_pkg"
  )
})

test_that(".check_survival_available message points at install.packages(\"survival\")", {
  testthat::local_mocked_bindings(spicy_pkg_available = function(pkg) FALSE)
  err <- tryCatch(.check_survival_available(), error = function(e) e)
  expect_match(conditionMessage(err), "survival", fixed = TRUE)
  expect_match(conditionMessage(err), "install.packages", fixed = TRUE)
})


# ---- 2. .survival_reference_rows(): ordered factor -> no reference rows ---

# An ordered factor uses polynomial (contr.poly) contrasts, so
# detect_factor_terms() reports reference_dropped = FALSE. The reference-row
# synthesiser must then `next` past it (line 238) and, with no other
# treatment-coded factor present, fall through to the empty-rows return
# (line 260) -- yielding a frame with zero is_ref rows.

.cov_coxph_ordered <- function() {
  skip_if_not_installed("survival")
  d <- survival::lung
  d$ph.ecog <- factor(d$ph.ecog, ordered = TRUE)
  survival::coxph(survival::Surv(time, status) ~ age + ph.ecog, data = d)
}

test_that("coxph ordered (polynomial) factor synthesises NO reference rows", {
  fit <- .cov_coxph_ordered()

  # Sanity on the detection layer: the factor is polynomial-coded and its
  # reference is not dropped, so no synthetic reference row should appear.
  fts <- detect_factor_terms(fit)
  expect_identical(length(fts), 1L)
  expect_identical(fts[[1L]]$contrast_type, "polynomial")
  expect_false(fts[[1L]]$reference_dropped)

  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_identical(sum(fr$coefs$is_ref), 0L)
})

test_that(".survival_reference_rows returns an empty coefs frame for an ordered factor", {
  fit <- .cov_coxph_ordered()
  rows <- spicy:::.survival_reference_rows(fit)
  expect_identical(nrow(rows), 0L)
  # The empty frame still carries the canonical coefs schema columns.
  expect_true(all(c("term", "parent_var", "is_ref", "estimate") %in% names(rows)))
})


# ---- 3. .survreg_dist_title(): per-distribution switch arms --------------

.cov_survreg_dist <- function(dist) {
  skip_if_not_installed("survival")
  survival::survreg(survival::Surv(time, status) ~ age,
                    data = survival::lung, dist = dist)
}

test_that("survreg loglogistic: title_prefix = 'Log-logistic AFT regression'", {
  fit <- .cov_survreg_dist("loglogistic")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Log-logistic AFT regression")
  expect_identical(fr$info$family$family, "loglogistic")
})

test_that("survreg exponential: title_prefix = 'Exponential AFT regression'", {
  fit <- .cov_survreg_dist("exponential")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Exponential AFT regression")
  expect_identical(fr$info$family$family, "exponential")
})

test_that("survreg logistic: title_prefix = 'Logistic AFT regression'", {
  fit <- .cov_survreg_dist("logistic")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Logistic AFT regression")
  expect_identical(fr$info$family$family, "logistic")
})

test_that("survreg t: title_prefix = 'Student-t AFT regression'", {
  fit <- .cov_survreg_dist("t")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_identical(fr$info$extras$title_prefix, "Student-t AFT regression")
})

test_that(".survreg_dist_title title-cases an unrecognised distribution name", {
  # No survreg dist routes to the default switch arm, so exercise the
  # capitalise-first-letter fallback directly. It must upper-case the
  # leading character and preserve the remainder verbatim.
  expect_identical(spicy:::.survreg_dist_title("foobar"), "Foobar")
  expect_identical(spicy:::.survreg_dist_title("rayleigh"), "Rayleigh")
})
