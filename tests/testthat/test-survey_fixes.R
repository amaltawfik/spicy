# ---------------------------------------------------------------------------
# Corrections to the survey-design family of table_regression() (svyglm,
# svyolr, svycoxph), found while writing the survey article (2026-11-19).
# ---------------------------------------------------------------------------

.nhanes_fit <- function() {
  skip_if_not_installed("survey")
  data(nhanes, package = "survey", envir = environment())
  nhanes$race <- factor(
    nhanes$race,
    1:4,
    c("Hispanic", "White", "Black", "Other")
  )
  dn <- survey::svydesign(
    id = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~WTMEC2YR,
    nest = TRUE,
    data = nhanes
  )
  suppressWarnings(survey::svyglm(
    HI_CHOL ~ race,
    design = dn,
    family = stats::quasibinomial
  ))
}

# ---- Counts and fit statistics never turn scientific ----------------------

test_that("a weighted n over 1e7 prints in full in the console and rich outputs", {
  fit <- .nhanes_fit()
  w_n <- formatC(
    sum(stats::weights(fit$survey.design, "sampling")[
      !is.na(fit$survey.design$variables$HI_CHOL)
    ]),
    format = "f",
    digits = 0
  )
  expect_identical(w_n, "255345910")
  fs <- c("nobs", "weighted_nobs", "aic")
  out <- paste(
    capture.output(print(table_regression(fit, show_fit_stats = fs))),
    collapse = "\n"
  )
  expect_match(out, "255345910", fixed = TRUE)
  expect_no_match(out, "e+0", fixed = TRUE)
  skip_if_not_installed("tinytable")
  tt <- table_regression(fit, show_fit_stats = fs, output = "tinytable")
  md <- paste(capture.output(print(tt, "markdown")), collapse = "\n")
  expect_match(md, "255345910", fixed = TRUE)
  expect_no_match(md, "e+0", fixed = TRUE)
})

test_that("fit statistics keep fixed decimals over 1e7 and below it", {
  f <- function(tk, v, dm = ".") {
    format_fit_stat_value(tk, v, 2L, 2L, 1L, decimal_mark = dm)
  }
  expect_identical(f("aic", 12345678.94), "12345678.9")
  expect_identical(f("log_lik", -12345678.94, ","), "-12345678,9")
  expect_identical(f("nobs", 255345910), "255345910")
  expect_identical(f("weighted_nobs", 255345910.14), "255345910")
  expect_identical(f("aic", 5561.035), "5561.0")
  expect_identical(f("weighted_nobs", 6194.3), "6194")
  # A coefficient keeps format_number()'s scientific switch.
  expect_identical(format_number(12345678.94, 2L), "1.23e+07")
})

# ---- The AIC of a design-based fit is named --------------------------------

.api_fits <- function() {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dg <- survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = apistrat,
    fpc = ~fpc
  )
  list(
    fit0 = survey::svyglm(api00 ~ ell, design = dg),
    fit = survey::svyglm(api00 ~ ell + meals, design = dg),
    plain = stats::lm(api00 ~ ell + meals, data = apistrat)
  )
}

.flat <- function(x) {
  gsub("\\s+", " ", paste(utils::capture.output(print(x)), collapse = " "))
}

test_that("the note says the AIC of a design-based fit is the design-based AIC", {
  m <- .api_fits()
  en <- "AIC of a design-based fit: design-based AIC (dAIC)."
  out <- .flat(table_regression(m$fit))
  expect_match(out, en, fixed = TRUE)
  expect_no_match(out, "not comparable", fixed = TRUE)
  # No AIC row, no sentence.
  out <- .flat(table_regression(m$fit, show_fit_stats = "nobs"))
  expect_no_match(out, "dAIC", fixed = TRUE)
  # A table without a design fit never carries it.
  expect_no_match(.flat(table_regression(m$plain)), "dAIC", fixed = TRUE)
  # Beside the likelihood AIC of a fit without a design.
  out <- .flat(table_regression(list(m$fit, m$plain), show_fit_stats = "aic"))
  expect_match(
    out,
    "AIC of a design-based fit: design-based AIC (dAIC), not comparable with the likelihood AIC of the other models.",
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    .flat(table_regression(m$fit)),
    "AIC d'un ajustement sous plan\u00A0: AIC de plan (dAIC).",
    fixed = TRUE
  )
  expect_match(
    .flat(table_regression(list(m$fit, m$plain), show_fit_stats = "aic")),
    paste0(
      "AIC d'un ajustement sous plan\u00A0: AIC de plan (dAIC), non ",
      "comparable \u00E0 l'AIC de vraisemblance des autres mod\u00E8les."
    ),
    fixed = TRUE
  )
})
