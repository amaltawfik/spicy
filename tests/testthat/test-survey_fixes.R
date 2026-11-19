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

# ---- nested = TRUE on a design fit points at survey's tests ----------------

test_that("the nested refusal for a design fit names regTermTest() and anova()", {
  m <- .api_fits()
  err <- expect_error(
    table_regression(list(m$fit0, m$fit), nested = TRUE),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "nested = FALSE", fixed = TRUE)
  expect_match(msg, "survey::regTermTest(fit, ~term)", fixed = TRUE)
  expect_match(msg, "anova(fit0, fit)", fixed = TRUE)
  # The calls the message names run on these fits.
  expect_s3_class(survey::regTermTest(m$fit, ~meals), "regTermTest")
  # anova() refits through the stored call, a bare `svyglm(...)` that
  # finds the function only with survey attached: qualified here instead.
  m$fit0$call[[1L]] <- m$fit$call[[1L]] <- quote(survey::svyglm)
  expect_s3_class(anova(m$fit0, m$fit), "regTermTestLRT")
})

# ---- RMST on a svycoxph: the design refusal comes first --------------------

.scox_fixture <- function() {
  skip_if_not_installed("survey")
  skip_if_not_installed("survival")
  data(pbc, package = "survival", envir = environment())
  pbc$randomized <- with(pbc, !is.na(trt) & trt > 0)
  bias <- stats::glm(
    randomized ~ age * edema,
    data = pbc,
    family = stats::binomial
  )
  pbc$randprob <- stats::fitted(bias)
  des <- survey::svydesign(
    id = ~1,
    prob = ~randprob,
    strata = ~edema,
    data = subset(pbc, randomized)
  )
  survey::svycoxph(
    survival::Surv(time, status > 0) ~ log(bili) + protime,
    design = des
  )
}

test_that("survey 4.5 still has no AIC for svycoxph, which keeps the token refused", {
  fit <- .scox_fixture()
  # extractAIC.svycoxph exists and stops; when survey implements it, the
  # `aic` token can follow.
  expect_error(stats::AIC(fit), "No AIC for survey models", fixed = TRUE)
  expect_error(
    table_regression(fit, show_fit_stats = "aic"),
    class = "spicy_invalid_input"
  )
})

test_that("RMST on a svycoxph is refused for the design before tau is asked", {
  fit <- .scox_fixture()
  head_line <- "RMST / risk-difference columns are not available for a design-based Cox fit."
  for (args in list(list(), list(tau = 1000))) {
    err <- expect_error(
      do.call(
        table_regression,
        c(list(fit, show_columns = c("b", "rmst")), args)
      ),
      class = "spicy_unsupported"
    )
    msg <- conditionMessage(err)
    expect_match(msg, head_line, fixed = TRUE)
    expect_no_match(msg, "explicit horizon", fixed = TRUE)
    expect_no_match(msg, "(M1)", fixed = TRUE)
  }
  # Among several models, the design fit is named as its column is.
  data(pbc, package = "survival", envir = environment())
  plain <- survival::coxph(
    survival::Surv(time, status > 0) ~ log(bili),
    data = pbc
  )
  err <- expect_error(
    table_regression(
      list(Plain = plain, Design = fit),
      show_columns = c("b", "risk_diff"),
      at_time = 1000
    ),
    class = "spicy_unsupported"
  )
  expect_match(conditionMessage(err), "Cox fit (Design).", fixed = TRUE)
})
