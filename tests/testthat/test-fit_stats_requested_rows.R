# A `show_fit_stats` token named in the call gives a row for one model of
# the table at least, or the call is refused (`spicy_invalid_input`).
# dev/decisions/2026-11-19-fit-stats-requested-rows.md has the sweep.
#
# The sweep runs on the fixtures of helper-class-fixtures.R and holds the
# rows to the registry: a token gives a row if and only if the column
# `fit_stats` of its class lists it, and the default table shows the rows
# of the column `fit_stats_default`, in their order
# (dev/decisions/2026-11-19-capability-registry.md).

.refusal_reset("fit_stats")

rr_rows <- function(fit, tokens) {
  nrow(suppressWarnings(suppressMessages(
    table_regression(fit, show_fit_stats = tokens, output = "data.frame")
  )))
}

# The registry tokens a fixture does not show, each with its reason. An
# item of a registry cell with a parenthesis holds for some fits of the
# class only: every fixture outside those fits is named here. `rows` is
# for the column `fit_stats`, `default` for `fit_stats_default`.
rr_absent <- list(
  lm = list(
    rows = "weighted_nobs",
    why = "the fit has no weights"
  ),
  glm_gaussian = list(
    rows = c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "pseudo_r2_tjur"),
    default = c("pseudo_r2_tjur", "pseudo_r2_mcfadden"),
    why = "the pseudo-R-squared are for binomial and Poisson families"
  ),
  glm_logit = list(
    rows = "r2_deviance",
    default = "r2_deviance",
    why = "a binomial glm has the pseudo-R-squared instead"
  ),
  glm_probit = list(
    rows = "r2_deviance",
    default = c("pseudo_r2_tjur", "r2_deviance"),
    why = "a binomial glm has the pseudo-R-squared instead; Tjur's is a default of the logit only"
  ),
  glm_poisson = list(
    rows = "pseudo_r2_tjur",
    default = c("pseudo_r2_tjur", "r2_deviance"),
    why = "Tjur's R-squared needs a binary outcome; a count glm defaults to McFadden's"
  ),
  glm_gamma = list(
    rows = c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "pseudo_r2_tjur"),
    default = c("pseudo_r2_tjur", "pseudo_r2_mcfadden"),
    why = "the pseudo-R-squared are for binomial and Poisson families"
  ),
  glm_quasi = list(
    rows = c(
      "pseudo_r2_mcfadden",
      "pseudo_r2_nagelkerke",
      "pseudo_r2_tjur",
      "aic",
      "aicc",
      "bic",
      "log_lik"
    ),
    default = c("pseudo_r2_tjur", "pseudo_r2_mcfadden", "r2_deviance", "aic"),
    why = "a quasi family has no likelihood, and no R-squared by default"
  ),
  lm_robust = list(
    rows = c("fixed_effects", "within_r2", "n_groups"),
    default = "fixed_effects",
    why = "the fit absorbs no fixed effects"
  ),
  feols = list(
    rows = "pseudo_r2_mcfadden",
    default = "pseudo_r2_mcfadden",
    why = "McFadden's R-squared is for the GLM families of fixest"
  ),
  feols_nofe = list(
    rows = c("pseudo_r2_mcfadden", "fixed_effects", "within_r2", "n_groups"),
    default = c("fixed_effects", "within_r2", "pseudo_r2_mcfadden"),
    why = "a linear fit without fixed effects"
  ),
  feols_iv = list(
    rows = c("pseudo_r2_mcfadden", "fixed_effects", "within_r2", "n_groups"),
    default = c(
      "fixed_effects",
      "r2",
      "within_r2",
      "pseudo_r2_mcfadden",
      "aic"
    ),
    why = "an IV fit without fixed effects: no R-squared or AIC by default"
  ),
  fepois = list(
    rows = c("r2", "adj_r2", "within_r2"),
    default = c("r2", "within_r2"),
    why = "a GLM-family fit has McFadden's R-squared instead"
  ),
  fepois_nofe = list(
    rows = c("r2", "adj_r2", "fixed_effects", "within_r2", "n_groups"),
    default = c("fixed_effects", "r2", "within_r2"),
    why = "a GLM-family fit without fixed effects"
  ),
  glmmTMB_ordinal = list(
    rows = c("r2_marginal", "r2_conditional", "icc", "sigma", "deviance"),
    default = c("icc", "r2_marginal", "r2_conditional"),
    why = "the ordinal family has no residual variance and no deviance"
  ),
  glmmTMB_zi = list(
    rows = c("r2_marginal", "r2_conditional", "icc"),
    default = c("icc", "r2_marginal", "r2_conditional"),
    why = "a zero-inflated fit has no R-squared or ICC"
  ),
  glmmTMB_hurdle = list(
    rows = c("r2_conditional", "icc", "deviance"),
    default = c("icc", "r2_conditional"),
    why = "a truncated family has a marginal R-squared only, and no deviance"
  ),
  glmmTMB_disp = list(
    rows = c("r2_marginal", "r2_conditional", "icc", "sigma"),
    default = c("icc", "r2_marginal", "r2_conditional"),
    why = "a dispersion formula gives no single residual variance"
  ),
  zeroinfl = list(
    rows = c("theta", "alpha"),
    why = "a Poisson count part has no dispersion"
  ),
  hurdle = list(
    rows = c("theta", "alpha"),
    why = "a Poisson count part has no dispersion"
  ),
  gam = list(
    rows = c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "pseudo_r2_tjur"),
    default = c("pseudo_r2_tjur", "pseudo_r2_mcfadden"),
    why = "a Gaussian GAM has the deviance R-squared of its glm twin"
  ),
  gam_logit = list(
    rows = "r2_deviance",
    default = "r2_deviance",
    why = "a binomial GAM has the pseudo-R-squared of its glm twin instead"
  ),
  bam = list(
    rows = "pseudo_r2_tjur",
    default = c("pseudo_r2_tjur", "r2_deviance"),
    why = "a Poisson GAM defaults to McFadden's R-squared, as its glm twin"
  ),
  survreg_strata = list(
    rows = "sigma",
    why = "strata give one scale per stratum"
  ),
  lrm_ordinal = list(
    rows = "pseudo_r2_tjur",
    default = "pseudo_r2_tjur",
    why = "Tjur's R-squared needs a binary outcome"
  ),
  rms_Glm = list(
    rows = "pseudo_r2_tjur",
    default = c("pseudo_r2_tjur", "r2_deviance"),
    why = "a Poisson Glm defaults to McFadden's R-squared, as its glm twin"
  ),
  rms_Glm_xy = list(
    rows = "pseudo_r2_tjur",
    default = c("pseudo_r2_tjur", "r2_deviance"),
    why = "a Poisson Glm defaults to McFadden's R-squared, as its glm twin"
  ),
  rms_Glm_logit = list(
    rows = "r2_deviance",
    default = "r2_deviance",
    why = "a binomial Glm has the pseudo-R-squared of its glm twin instead"
  )
)

rr_expected <- function(id, column) {
  reg <- .supported_models_registry()
  cl <- .CLASS_FIXTURES[[id]]$class
  toks <- .reg_cell_tokens(reg[[column]][reg$class == cl])
  key <- if (column == "fit_stats") "rows" else "default"
  setdiff(toks, rr_absent[[id]][[key]])
}

# Every token of the vocabulary, read from the package so that a token
# added later is swept as well: refused, or one row more than "nobs", and
# a row if and only if the registry lists it for the fixture.
rr_sweep <- function(fit, id) {
  base <- rr_rows(fit, "nobs")
  rows <- rr_expected(id, "fit_stats")
  for (tk in setdiff(.regression_tokens$show_fit_stats, "nobs")) {
    k <- tryCatch(
      rr_rows(fit, c("nobs", tk)),
      spicy_invalid_input = function(e) {
        .refusal_note("fit_stats", id, paste0("\"", tk, "\""), e)
        NA_integer_
      }
    )
    label <- paste0(id, " / ", tk)
    if (is.na(k)) {
      expect_false(tk %in% rows, label = paste(label, "refused"))
    } else {
      expect_gt(k, base, label = paste(label, "rows"))
      expect_true(tk %in% rows, label = paste(label, "rendered"))
    }
  }
  tbl <- suppressWarnings(suppressMessages(
    table_regression(fit, output = "data.frame")
  ))
  shown <- unique(tbl$variable[
    tbl$.row_role == "fit_stat" | tbl$variable == "fixed_effects"
  ])
  expect_identical(
    shown,
    rr_expected(id, "fit_stats_default"),
    label = paste(id, "default rows")
  )
}

test_that("every class of the registry has a fixture, and every exception names one", {
  reg <- .supported_models_registry()
  fx_classes <- vapply(.CLASS_FIXTURES, `[[`, character(1), "class")
  expect_setequal(unique(fx_classes), reg$class)
  expect_true(all(names(rr_absent) %in% names(.CLASS_FIXTURES)))
  for (id in names(rr_absent)) {
    cl <- fx_classes[[id]]
    # An exception names tokens of the class cell, never others.
    expect_true(all(
      rr_absent[[id]]$rows %in%
        .reg_cell_tokens(reg$fit_stats[reg$class == cl])
    ))
    expect_true(all(
      rr_absent[[id]]$default %in%
        .reg_cell_tokens(reg$fit_stats_default[reg$class == cl])
    ))
    expect_true(nzchar(rr_absent[[id]]$why))
  }
  vocab <- .regression_tokens$show_fit_stats
  for (cell in c(reg$fit_stats, reg$fit_stats_default)) {
    expect_true(all(.reg_cell_tokens(cell) %in% vocab), label = cell)
  }
})

for (id in names(.CLASS_FIXTURES)) {
  local({
    id <- id
    test_that(
      paste0("every show_fit_stats token renders or is refused: ", id),
      {
        skip_on_cran()
        rr_sweep(.class_fixture(id), id)
        .refusal_ran("fit_stats", id)
      }
    )
  })
}

# The text and the class of every refusal of the sweep, frozen
# (helper-refusal-log.R). The Stan fixtures, swept on local runs only,
# have a snapshot of their own.
test_that("the refusals of the show_fit_stats sweep keep their text and class", {
  skip_on_cran()
  .refusal_snapshot("fit_stats", .refusal_ids())
})

test_that("the refusals of the show_fit_stats sweep keep their text and class: Stan", {
  .skip_unless_local_stan()
  .refusal_snapshot("fit_stats", .refusal_ids(stan = TRUE))
})


# ---- The rule --------------------------------------------------------------

test_that("a token named in the call without a value is refused, naming it", {
  m <- lm(mpg ~ wt, mtcars)
  err <- expect_error(
    table_regression(m, show_fit_stats = c("nobs", "icc")),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "Token `\"icc\"` in `show_fit_stats` has no value",
    fixed = TRUE
  )
  expect_match(msg, "mixed models of lme4, glmmTMB and nlme", fixed = TRUE)
  expect_identical(deparse(conditionCall(err)[[1L]]), "table_regression")
})

test_that("several tokens are named in one message, with their reasons", {
  m <- lm(mpg ~ wt, mtcars)
  err <- expect_error(
    table_regression(
      m,
      show_fit_stats = c("nobs", "n_events", "eff_p", "weighted_nobs")
    ),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "Tokens `\"n_events\"`, `\"eff_p\"` and `\"weighted_nobs\"` in `show_fit_stats` have no value",
    fixed = TRUE
  )
  expect_match(msg, "events of a Cox model", fixed = TRUE)
  expect_match(msg, "`svyglm()` design", fixed = TRUE)
  expect_match(msg, "fitted with weights or on a survey design", fixed = TRUE)
  expect_no_match(msg, "?table_regression", fixed = TRUE)
})

test_that("a token without a domain reason points to the help", {
  skip_if_not_installed("MASS")
  d <- .class_fixture_data()
  fit <- MASS::polr(ord ~ x, d, Hess = TRUE)
  err <- expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "sigma")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "section of ?table_regression lists the models",
    fixed = TRUE
  )
})

test_that("a model of the domain without a value gets no domain reason", {
  d <- .class_fixture_data()
  fit <- lm(y ~ x, d)
  err <- expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "weighted_nobs")),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "fitted with weights", fixed = TRUE)
  wfit <- lm(y ~ x, d, weights = w)
  tbl <- table_regression(
    wfit,
    show_fit_stats = c("nobs", "weighted_nobs"),
    output = "data.frame"
  )
  expect_true("weighted_nobs" %in% tbl$variable)
})

test_that("the defaults stay tolerant of a token without a value", {
  d <- .class_fixture_data()
  # The glm default asks for the AIC, which a quasi family has not.
  q <- glm(cnt ~ x, quasipoisson, d)
  expect_no_error(suppressWarnings(table_regression(q)))
  expect_no_error(table_regression(lm(y ~ x, d), show_fit_stats = FALSE))
  expect_error(
    suppressWarnings(table_regression(q, show_fit_stats = c("nobs", "aic"))),
    class = "spicy_invalid_input"
  )
})

test_that("a token one model shows takes a dash for the others", {
  m1 <- lm(mpg ~ wt, mtcars)
  m2 <- glm(am ~ wt, binomial, mtcars)
  tbl <- table_regression(
    list(m1, m2),
    show_fit_stats = c("nobs", "r2"),
    output = "data.frame"
  )
  r2 <- tbl[tbl$variable %in% "r2", , drop = FALSE]
  expect_identical(nrow(r2), 1L)
  expect_equal(r2[["mpg: B"]], summary(m1)$r.squared)
  expect_true(is.na(r2[["am: B"]]))
})

test_that("a change token needs nested = TRUE and two models", {
  m1 <- lm(mpg ~ wt, mtcars)
  m2 <- lm(mpg ~ wt + hp, mtcars)
  err <- expect_error(
    table_regression(list(m1, m2), show_fit_stats = c("nobs", "r2_change")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "Token `\"r2_change\"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.",
    fixed = TRUE
  )
  expect_identical(deparse(conditionCall(err)[[1L]]), "table_regression")
  err <- expect_error(
    table_regression(m1, show_fit_stats = c("r2_change", "f_change")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "Tokens `\"r2_change\"` and `\"f_change\"`",
    fixed = TRUE
  )
  expect_no_error(table_regression(
    list(m1, m2),
    nested = TRUE,
    show_fit_stats = c("nobs", "r2_change")
  ))
})

test_that("the stacked layout checks the stack as one table", {
  m1 <- lm(mpg ~ wt, mtcars)
  m2 <- lm(mpg ~ wt + hp, mtcars)
  expect_no_error(table_regression(
    list(m1, m2),
    layout = "stacked",
    nested = TRUE,
    show_fit_stats = c("nobs", "r2_change")
  ))
  err <- expect_error(
    table_regression(
      list(m1, m2),
      layout = "stacked",
      show_fit_stats = c("nobs", "r2_change")
    ),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "needs `nested = TRUE`", fixed = TRUE)
  err <- expect_error(
    table_regression(
      list(m1, m2),
      layout = "stacked",
      show_fit_stats = c("nobs", "icc")
    ),
    class = "spicy_invalid_input"
  )
  expect_identical(deparse(conditionCall(err)[[1L]]), "table_regression")
  skip_if_not_installed("lme4")
  m3 <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  m4 <- lm(Reaction ~ Days, lme4::sleepstudy)
  expect_no_error(table_regression(
    list(m3, m4),
    layout = "stacked",
    show_fit_stats = c("nobs", "icc")
  ))
})

test_that("table_regression_uv() refuses a token, naming the screen", {
  d <- .class_fixture_data()
  err <- expect_error(
    table_regression_uv(
      d,
      outcome = y,
      predictors = c(x, z),
      show_fit_stats = c("nobs", "icc")
    ),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "Token `\"icc\"`", fixed = TRUE)
  expect_identical(deparse(conditionCall(err)[[1L]]), "table_regression_uv")
  err <- expect_error(
    table_regression_uv(
      d,
      outcome = y,
      predictors = c(x, z),
      multivariable = FALSE,
      show_fit_stats = c("nobs", "aic")
    ),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "univariable models of a screen show no fit statistic",
    fixed = TRUE
  )
  expect_no_error(table_regression_uv(
    d,
    outcome = y,
    predictors = c(x, z),
    show_fit_stats = c("nobs", "aic")
  ))
})

test_that("a fit-only table that shows the absorbed fixed effects alone is not an empty table", {
  skip_if_not_installed("fixest")
  fit <- fixest::feols(mpg ~ wt | cyl, data = mtcars)
  tbl <- table_regression(
    fit,
    show_columns = "none",
    show_fit_stats = "fixed_effects"
  )
  body <- attr(tbl, "structured")$body
  # The block is the header row and one row per absorbed factor: no
  # fit-statistic row, and still a table.
  expect_identical(body$.row_role, c("factor_header", "level"))
  out <- capture.output(print(tbl))
  expect_true(any(grepl("Fixed effects", out, fixed = TRUE)))
  expect_true(any(
    grepl("cyl", out, fixed = TRUE) & grepl("Yes", out, fixed = TRUE)
  ))
})

test_that("`fixed_effects` asked of a fixest model fitted without fixed effects is refused with the reason", {
  skip_if_not_installed("fixest")
  fit <- fixest::feols(mpg ~ wt, data = mtcars)
  err <- expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "fixed_effects")),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "no model in the table absorbs any", fixed = TRUE)
  # A reason that is given replaces the pointer to the help page.
  expect_false(grepl("lists the models each token", msg, fixed = TRUE))
  # With a fixed effect the same call gives the block.
  with_fe <- fixest::feols(mpg ~ wt | cyl, data = mtcars)
  expect_no_error(
    table_regression(with_fe, show_fit_stats = c("nobs", "fixed_effects"))
  )
})
