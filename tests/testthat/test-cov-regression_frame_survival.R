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
#     unrecognized distribution name.
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
# synthesizer must then `next` past it (line 238) and, with no other
# treatment-coded factor present, fall through to the empty-rows return
# (line 260) -- yielding a frame with zero is_ref rows.

.cov_coxph_ordered <- function() {
  skip_if_not_installed("survival")
  d <- survival::lung
  d$ph.ecog <- factor(d$ph.ecog, ordered = TRUE)
  survival::coxph(survival::Surv(time, status) ~ age + ph.ecog, data = d)
}

test_that("coxph ordered (polynomial) factor synthesizes NO reference rows", {
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
  expect_true(all(
    c("term", "parent_var", "is_ref", "estimate") %in% names(rows)
  ))
})


# ---- 3. .survreg_dist_title(): per-distribution switch arms --------------

.cov_survreg_dist <- function(dist) {
  skip_if_not_installed("survival")
  survival::survreg(
    survival::Surv(time, status) ~ age,
    data = survival::lung,
    dist = dist
  )
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

test_that("survreg logistic: title_prefix = 'Censored regression (logistic)'", {
  fit <- .cov_survreg_dist("logistic")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Censored regression (logistic)"
  )
  expect_identical(fr$info$family$family, "logistic")
  # logistic is identity-scale (models T directly), so link = "identity" and
  # exp(coef) is not a time ratio -> exponentiate unsupported.
  expect_identical(fr$info$family$link, "identity")
  expect_false(fr$info$supports$exponentiate)
})

test_that("survreg t: title_prefix = 'Censored regression (Student t)'", {
  fit <- .cov_survreg_dist("t")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_identical(
    fr$info$extras$title_prefix,
    "Censored regression (Student t)"
  )
  # t is identity-scale like logistic/gaussian.
  expect_identical(fr$info$family$link, "identity")
  expect_false(fr$info$supports$exponentiate)
})

test_that(".survreg_dist_title title-cases an unrecognized distribution name", {
  # No survreg dist routes to the default switch arm, so exercise the
  # capitalize-first-letter fallback directly. It must upper-case the
  # leading character and preserve the remainder verbatim.
  expect_identical(spicy:::.survreg_dist_title("foobar"), "Foobar")
  expect_identical(spicy:::.survreg_dist_title("rayleigh"), "Rayleigh")
})


# ---- 4. survreg link / exponentiate convention by distribution -----------
#
# In survival::survreg.distributions only weibull / exponential / rayleigh /
# lognormal / loglogistic carry trans = log (itrans = exp); they model log(T)
# and so are "log-scale" AFT fits. The remaining built-in dists -- gaussian,
# logistic, extreme, t -- model T directly (identity scale, no log
# transform). Therefore:
#   * family$link must be "identity" for gaussian / logistic / extreme / t,
#     and "log" for weibull / exponential / lognormal / loglogistic.
#   * supports$exponentiate (exp(coef) = time ratio) is meaningful ONLY for
#     the log-scale dists; it must be FALSE for gaussian / logistic /
#     extreme / t.
# Expected values below are derived from this convention, NOT from output.

.cov_survreg_fit <- function(dist) {
  skip_if_not_installed("survival")
  survival::survreg(
    survival::Surv(time, status) ~ age,
    data = survival::lung,
    dist = dist
  )
}

test_that("survreg identity-scale dists report link = 'identity' (gaussian/logistic/extreme/t)", {
  for (d in c("gaussian", "logistic", "extreme", "t")) {
    fr <- as_regression_frame(.cov_survreg_fit(d), model_id = "M1")
    expect_identical(fr$info$family$link, "identity", info = paste("dist =", d))
  }
})

test_that("survreg log-scale dists report link = 'log' (weibull/exponential/lognormal/loglogistic)", {
  for (d in c("weibull", "exponential", "lognormal", "loglogistic")) {
    fr <- as_regression_frame(.cov_survreg_fit(d), model_id = "M1")
    expect_identical(fr$info$family$link, "log", info = paste("dist =", d))
  }
})

test_that("survreg exponentiate is FALSE for identity-scale dists (gaussian/logistic/extreme/t)", {
  for (d in c("gaussian", "logistic", "extreme", "t")) {
    fr <- as_regression_frame(.cov_survreg_fit(d), model_id = "M1")
    expect_false(fr$info$supports$exponentiate, info = paste("dist =", d))
  }
})

test_that("survreg exponentiate is TRUE for log-scale dists (weibull/lognormal)", {
  for (d in c("weibull", "lognormal", "loglogistic", "exponential")) {
    fr <- as_regression_frame(.cov_survreg_fit(d), model_id = "M1")
    expect_true(fr$info$supports$exponentiate, info = paste("dist =", d))
  }
})


# ---- 5. The scale of every survreg distribution, read from survival -------
#
# Two oracles from survival itself, never from spicy: the distribution's
# survreg.distributions entry (a `trans` means the model is fitted on
# log T), and survreg's own predictions (the linear predictor IS the
# predicted time for a distribution that models T itself, and its log
# otherwise).

test_that("a survreg distribution models T itself exactly when its entry has no trans", {
  skip_if_not_installed("survival")
  for (d in names(survival::survreg.distributions)) {
    fit <- .cov_survreg_fit(d)
    raw <- is.null(survival::survreg.distributions[[d]]$trans)
    lp <- stats::predict(fit, type = "lp")
    resp <- stats::predict(fit, type = "response")
    expect_equal(if (raw) lp else exp(lp), resp, info = paste("dist =", d))
    fr <- as_regression_frame(fit, model_id = "M1")
    expect_identical(
      fr$info$family$link,
      if (raw) "identity" else "log",
      info = paste("dist =", d)
    )
    expect_identical(
      fr$info$supports$exponentiate,
      !raw,
      info = paste("dist =", d)
    )
    expect_identical(
      grepl("^Censored regression ", fr$info$extras$title_prefix),
      raw,
      info = paste("dist =", d)
    )
  }
})

test_that("an extreme-value survreg keeps its coefficients under exponentiate = TRUE", {
  skip_if_not_installed("survival")
  skip_if_not_installed("broom")
  fit <- .cov_survreg_fit("extreme")
  # survival models the extreme distribution on T itself: its table
  # entry has no log transformation.
  expect_null(survival::survreg.distributions$extreme$trans)
  w <- NULL
  tbl <- withCallingHandlers(
    table_regression(fit, exponentiate = TRUE),
    spicy_ignored_arg = function(cnd) {
      w <<- conditionMessage(cnd)
      invokeRestart("muffleWarning")
    }
  )
  expect_match(w, "survreg()` model with `dist = \"extreme\"`", fixed = TRUE)
  expect_match(w, "on the time scale, not the log-time scale", fixed = TRUE)
  expect_match(w, "exp(B) is not a time ratio", fixed = TRUE)
  expect_no_match(w, "gaussian)", fixed = TRUE)
  # The same message in a French session: conditions stay English.
  w_fr <- NULL
  withr::with_options(
    list(spicy.language = "fr"),
    withCallingHandlers(
      table_regression(fit, exponentiate = TRUE),
      spicy_ignored_arg = function(cnd) {
        w_fr <<- conditionMessage(cnd)
        invokeRestart("muffleWarning")
      }
    )
  )
  expect_identical(w_fr, w)
  expect_identical(
    attr(tbl, "title"),
    "Censored regression (extreme value): survival::Surv(time, status)"
  )
  expect_no_match(attr(tbl, "note"), "time ratio", fixed = TRUE)
  td <- broom::tidy(tbl)
  expect_equal(td$estimate[td$term == "age"], unname(stats::coef(fit)["age"]))
})

test_that("a censored regression names its distribution alike in title and note", {
  skip_if_not_installed("survival")
  # One spelling per distribution: the word in the title's parentheses is
  # the word of the note, capitalized by position only, in both languages.
  words <- list(
    en = c(
      gaussian = "Gaussian",
      logistic = "logistic",
      extreme = "extreme value",
      t = "Student t"
    ),
    fr = c(
      gaussian = "gaussienne",
      logistic = "logistique",
      extreme = "valeurs extr\u00eames",
      t = "t de Student"
    )
  )
  for (lang in names(words)) {
    withr::local_options(spicy.language = lang)
    for (d in names(words[[lang]])) {
      tbl <- table_regression(.cov_survreg_fit(d))
      word <- words[[lang]][[d]]
      lab <- paste(lang, d)
      expect_match(
        attr(tbl, "title"),
        paste0("(", word, ")"),
        fixed = TRUE,
        label = lab
      )
      note_word <- if (identical(word, "logistic")) "Logistic" else word
      note_word <- if (identical(word, "extreme value")) {
        "Extreme value"
      } else {
        note_word
      }
      expect_match(
        attr(tbl, "note"),
        paste0(
          if (lang == "en") "Distribution: " else "Distribution\u00a0: ",
          note_word,
          if (lang == "en") ";" else "\u00a0;"
        ),
        fixed = TRUE,
        label = lab
      )
    }
  }
})

test_that("a distribution passed as a list follows its own trans", {
  skip_if_not_installed("survival")
  skip_if_not_installed("broom")
  d <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  # An entry of the table, passed as the list itself.
  weib <- survival::survreg(
    survival::Surv(time, status) ~ age,
    data = d,
    dist = survival::survreg.distributions$weibull
  )
  fr <- as_regression_frame(weib, model_id = "M1")
  expect_identical(fr$info$family$link, "log")
  expect_identical(fr$info$extras$title_prefix, "Weibull AFT regression")
  td <- broom::tidy(table_regression(weib, exponentiate = TRUE))
  expect_equal(
    td$estimate[td$term == "age"],
    exp(unname(stats::coef(weib)["age"]))
  )
  # A distribution of the user's own, without trans: named as they named it.
  own <- survival::survreg.distributions$extreme
  own$name <- "My extreme"
  mine <- survival::survreg(
    survival::Surv(time, status) ~ age,
    data = d,
    dist = own
  )
  fr <- as_regression_frame(mine, model_id = "M1")
  expect_identical(fr$info$family$link, "identity")
  expect_false(fr$info$supports$exponentiate)
  expect_identical(
    fr$info$extras$title_prefix,
    "Censored regression (My extreme)"
  )
  expect_match(
    attr(table_regression(mine), "note"),
    "Distribution: My extreme; scale = ",
    fixed = TRUE
  )
})

test_that("AER::tobit keeps its title", {
  skip_if_not_installed("AER")
  data("Affairs", package = "AER", envir = environment())
  for (dist in c("gaussian", "logistic")) {
    fit <- AER::tobit(affairs ~ age + yearsmarried, data = Affairs, dist = dist)
    expect_identical(
      as_regression_frame(fit)$info$extras$title_prefix,
      "Tobit regression",
      info = dist
    )
  }
})
