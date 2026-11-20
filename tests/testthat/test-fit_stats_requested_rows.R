# A `show_fit_stats` token named in the call gives a row for one model of
# the table at least, or the call is refused (`spicy_invalid_input`).
# dev/decisions/2026-11-19-fit-stats-requested-rows.md has the sweep.
#
# The sweep runs on the fixtures of helper-class-fixtures.R and holds the
# rows to the registry: a token gives a row if and only if the column
# `fit_stats` of its class lists it and the fit meets the condition of
# its parenthesis, and the default table shows the rows of the column
# `fit_stats_default` read the same way, in their order
# (dev/decisions/2026-11-19-capability-registry.md).

.refusal_reset("fit_stats")

rr_rows <- function(fit, tokens) {
  nrow(suppressWarnings(suppressMessages(
    table_regression(fit, show_fit_stats = tokens, output = "data.frame")
  )))
}

# The rows a fixture shows: the tokens of its class cell that the
# registry offers for the fit (.reg_offers(): an item without a
# condition, or one whose condition the predicate of
# .REG_FIT_STATS_CONDITIONS finds in the fit). `column` is `fit_stats`
# or `fit_stats_default`.
rr_expected <- function(fit, id, column) {
  reg <- .supported_models_registry()
  cl <- .CLASS_FIXTURES[[id]]$class
  toks <- .reg_cell_tokens(reg[[column]][reg$class == cl])
  toks[vapply(
    toks,
    function(tk) .reg_offers(fit, tk, column),
    logical(1),
    USE.NAMES = FALSE
  )]
}

# Every token of the vocabulary, read from the package so that a token
# added later is swept as well: refused, or one row more than "nobs", and
# a row if and only if the registry offers it for the fixture.
rr_sweep <- function(fit, id) {
  expect_identical(.reg_registry_class(fit), .CLASS_FIXTURES[[id]]$class)
  base <- rr_rows(fit, "nobs")
  rows <- rr_expected(fit, id, "fit_stats")
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
    rr_expected(fit, id, "fit_stats_default"),
    label = paste(id, "default rows")
  )
}

test_that("every class of the registry has a fixture, and every condition a predicate", {
  reg <- .supported_models_registry()
  fx_classes <- vapply(.CLASS_FIXTURES, `[[`, character(1), "class")
  expect_setequal(unique(fx_classes), reg$class)
  cells <- c(reg$fit_stats, reg$fit_stats_default)
  vocab <- .regression_tokens$show_fit_stats
  for (cell in cells) {
    expect_true(all(.reg_cell_tokens(cell) %in% vocab), label = cell)
    expect_identical(names(.reg_cell_conditions(cell)), .reg_cell_tokens(cell))
  }
  # The texts of the parentheses of both columns and the predicates are
  # one set: each condition has its predicate, and each predicate serves.
  conds <- unlist(lapply(cells, .reg_cell_conditions), use.names = FALSE)
  conds <- unique(conds[!is.na(conds)])
  expect_setequal(names(.REG_FIT_STATS_CONDITIONS), conds)
  expect_false(anyDuplicated(names(.REG_FIT_STATS_CONDITIONS)) > 0L)
  expect_true(all(vapply(.REG_FIT_STATS_CONDITIONS, is.function, logical(1))))
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

# A remedy names a token its receiver accepts (helper-refusal-log.R).
test_that("every remedy of the show_fit_stats sweep is accepted by its receiver", {
  skip_on_cran()
  .refusal_check_remedies("fit_stats", .refusal_ids())
})

test_that("every remedy of the show_fit_stats sweep is accepted by its receiver: Stan", {
  .skip_unless_local_stan()
  .refusal_check_remedies("fit_stats", .refusal_ids(stan = TRUE))
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

# ---- The controls on a group of models ---------------------------------------

# validate_class_appropriate_tokens() refuses a literal list of fit
# statistics when every model of the table is of one group. Each list
# below is the list of such a control, with the test the control makes on
# a fit; no class of the group lists one of its tokens in the column
# `fit_stats` of the registry. The change tokens of these lists, and the
# controls of the rq and rlm hierarchies, are left out: the registry has
# no column for them. So are the column tokens (the AME of Cox and mlogit
# fits, p and t of Bayesian fits, the partial columns of mixed and GEE
# fits), and the pseudo-R-squared refused for a table of GAMs, which
# depends on the family of each GAM (the predicates above).
rr_group_lists <- list(
  "all glm" = list(
    member = function(f) inherits(f, "glm") && !inherits(f, "geeglm"),
    tokens = c("r2", "omega2", "f2")
  ),
  "all glm, not all GAMs" = list(
    member = function(f) {
      inherits(f, "glm") && !inherits(f, c("geeglm", "gam"))
    },
    tokens = "adj_r2"
  ),
  "all lm" = list(
    member = function(f) inherits(f, "lm") && !inherits(f, "glm"),
    tokens = c(
      "pseudo_r2_mcfadden",
      "pseudo_r2_nagelkerke",
      "pseudo_r2_tjur",
      "r2_deviance"
    )
  ),
  "all Bayesian" = list(
    member = function(f) inherits(f, c("stanreg", "brmsfit")),
    tokens = c(
      "aic",
      "aicc",
      "bic",
      "deviance",
      "log_lik",
      "r2",
      "adj_r2",
      "omega2",
      "f2",
      "pseudo_r2_mcfadden",
      "pseudo_r2_nagelkerke",
      "pseudo_r2_tjur",
      "r2_deviance",
      "sigma",
      "rmse"
    )
  ),
  "all GEE" = list(
    member = function(f) inherits(f, "geeglm"),
    tokens = c(
      "r2",
      "adj_r2",
      "omega2",
      "f2",
      "pseudo_r2_mcfadden",
      "pseudo_r2_nagelkerke",
      "pseudo_r2_tjur",
      "r2_deviance",
      "aic",
      "aicc",
      "bic",
      "deviance",
      "log_lik"
    )
  ),
  "all mixed" = list(
    member = function(f) {
      inherits(f, c("merMod", "lmerModLmerTest", "glmmTMB", "lme", "clmm"))
    },
    tokens = c(
      "r2",
      "adj_r2",
      "omega2",
      "f2",
      "pseudo_r2_mcfadden",
      "pseudo_r2_nagelkerke",
      "pseudo_r2_tjur",
      "r2_deviance"
    )
  ),
  "all survey designs" = list(
    member = function(f) .is_design_fit(f),
    tokens = c(
      "deviance",
      "bic",
      "aicc",
      "pseudo_r2_mcfadden",
      "pseudo_r2_nagelkerke",
      "pseudo_r2_tjur",
      "r2_deviance"
    )
  ),
  "all survey designs, no svyglm" = list(
    member = function(f) .is_design_fit(f) && !inherits(f, "svyglm"),
    tokens = "aic"
  )
)

# The class vectors of the Stan fits, which CI does not build: the group
# tests read the classes only.
rr_stan_classes <- list(
  stanreg = c("stanreg", "glm", "lm"),
  brmsfit = "brmsfit"
)

test_that("no class of a group lists a token its group control refuses", {
  skip_on_cran()
  reg <- .supported_models_registry()
  ids <- .refusal_ids()
  fits <- c(
    lapply(stats::setNames(ids, ids), .class_fixture),
    lapply(rr_stan_classes, function(cl) structure(list(), class = cl))
  )
  seen <- character()
  for (id in names(fits)) {
    fit <- fits[[id]]
    cl <- .reg_registry_class(fit)
    listed <- .reg_cell_tokens(reg$fit_stats[reg$class == cl])
    for (g in names(rr_group_lists)) {
      if (!rr_group_lists[[g]]$member(fit)) {
        next
      }
      seen <- union(seen, g)
      expect_identical(
        intersect(rr_group_lists[[g]]$tokens, listed),
        character(),
        label = paste(id, "in", g)
      )
    }
  }
  # Every group has a class of the registry in it.
  expect_setequal(seen, names(rr_group_lists))
})
