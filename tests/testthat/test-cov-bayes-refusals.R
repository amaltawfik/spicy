# Coverage tests for the Bayesian refusal arms that CI never reached
# after the 27 sampling test blocks were put behind skip_on_ci().
#
# Every gate below fires BEFORE any draws work, so a class-only mock
# fit -- structure(list(), class = c("stanreg", "glm", "lm")) -- is
# enough. No Stan, no sampling, no RNG: these run on CI.
#
# Lines closed:
#   R/regression_validate.R  314, 655-674, 1223-1235, 1262-1280,
#                            1287-1300, 1313-1333, 1362-1375, 2336
#   R/table_regression.R     1939-1957, 2194-2205

# A stanreg carries `glm` and `lm` in its class vector, so the mock has
# to as well: the gates that must NOT fire on a Bayesian fit are the
# ones keyed on those parent classes.
fake_stanreg <- function() {
  structure(list(), class = c("stanreg", "glm", "lm"))
}

fake_brmsfit <- function() {
  structure(list(), class = "brmsfit")
}


# ---- regression_validate.R 655-674: robust vcov on a Bayesian fit ----

test_that("a robust `vcov` on a Bayesian fit is refused on principle", {
  # Not "support is pending": a posterior has no sandwich analogue, so
  # the message names the design reason rather than the generic
  # "being added" wording used for classes awaiting support.
  err <- expect_error(
    table_regression(fake_stanreg(), vcov = "HC3"),
    class = "spicy_unsupported_vcov"
  )
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "`vcov = \"HC3\"` is not defined for Bayesian fits (`stanreg`).",
    fixed = TRUE
  )
  expect_match(msg, "no sandwich analogue", fixed = TRUE)
  # The actionable alternative is to MODEL the structure, not to
  # sandwich it after the fact.
  expect_match(msg, "rstanarm::stan_glmer()", fixed = TRUE)

  # The class name in the message is the fit's own, not a hard-coded
  # "stanreg": a brmsfit reports brmsfit.
  err2 <- expect_error(
    spicy:::validate_vcov_cluster_lists("CR2", NULL, list(fake_brmsfit())),
    class = "spicy_unsupported_vcov"
  )
  expect_match(
    conditionMessage(err2),
    "`vcov = \"CR2\"` is not defined for Bayesian fits (`brmsfit`).",
    fixed = TRUE
  )
})


test_that("`vcov = \"classical\"` passes the Bayesian gate", {
  # The refusal is scoped to robust types: "classical" is supported by
  # every class, so a default call must never trip this arm.
  expect_no_error(
    spicy:::validate_vcov_cluster_lists("classical", NULL, list(fake_stanreg()))
  )
  expect_identical(
    spicy:::.robust_vcov_support(fake_stanreg()),
    "classical"
  )
})


# ---- regression_validate.R 1223-1235: Bayesian fit stats, no Bayesian fit ----

test_that("Bayesian-only fit statistics need a Bayesian model", {
  m <- lm(mpg ~ wt, data = mtcars)
  err <- expect_error(
    table_regression(m, show_fit_stats = c("nobs", "r2_bayes")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "Token(s) \"r2_bayes\" in `show_fit_stats` are defined only for Bayesian fits.",
    fixed = TRUE
  )

  # Every member of the quartet is gated, and the message lists them
  # all when several are asked for at once.
  err2 <- expect_error(
    spicy:::validate_class_appropriate_tokens(
      list(m),
      character(0),
      c("nobs", "looic", "waic")
    ),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err2), "\"looic\", \"waic\"", fixed = TRUE)

  # An EMPTY model set takes the same arm (`length(models) == 0L`).
  expect_error(
    spicy:::validate_class_appropriate_tokens(
      list(),
      character(0),
      c("nobs", "elpd_loo")
    ),
    class = "spicy_invalid_input"
  )
})


test_that("a Bayesian fit passes the Bayesian-fit-statistics gate", {
  # Complement of the arm above: with a Bayesian model present the
  # token is legitimate and the gate stays silent. r2_bayes is chosen
  # because it does not additionally require the loo package.
  expect_no_error(
    spicy:::validate_class_appropriate_tokens(
      list(fake_stanreg()),
      character(0),
      c("nobs", "r2_bayes")
    )
  )
})


# ---- regression_validate.R 1262-1280: p / t / ame_p on an all-Bayesian set ----

test_that("p, t and ame_p are refused when every model is Bayesian", {
  err <- expect_error(
    table_regression(fake_stanreg(), show_columns = c("b", "p")),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "Token(s) \"p\" in `show_columns` are not defined for Bayesian fits.",
    fixed = TRUE
  )
  # The hint names the posterior substitute rather than leaving the
  # reader without a next step.
  expect_match(msg, "probability of direction", fixed = TRUE)
  expect_match(msg, "(`\"pd\"`)", fixed = TRUE)

  # All three atomic tokens are gated, and the message enumerates the
  # offending ones in the canonical p / t / ame_p order.
  err2 <- expect_error(
    spicy:::validate_class_appropriate_tokens(
      list(fake_stanreg(), fake_brmsfit()),
      c("b", "ame_p", "t"),
      character(0)
    ),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err2), "\"t\", \"ame_p\"", fixed = TRUE)
})


test_that("a MIXED model set keeps p (the frequentist column still exists)", {
  # Only homogeneous Bayesian sets refuse: in a mixed table the
  # frequentist cells carry a p and the Bayesian cells dash.
  expect_no_error(
    spicy:::validate_class_appropriate_tokens(
      list(fake_stanreg(), lm(mpg ~ wt, data = mtcars)),
      c("b", "p"),
      character(0)
    )
  )
})


# ---- regression_validate.R 1287-1300: pd without any Bayesian fit ----

test_that("the pd column is refused when no model is Bayesian", {
  err <- expect_error(
    table_regression(lm(mpg ~ wt, data = mtcars), show_columns = c("b", "pd")),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "Token \"pd\" in `show_columns` is defined only for Bayesian fits.",
    fixed = TRUE
  )
  expect_match(msg, "share of draws on the dominant side of zero", fixed = TRUE)

  # An empty model set takes the same arm.
  expect_error(
    spicy:::validate_class_appropriate_tokens(
      list(),
      c("b", "pd"),
      character(0)
    ),
    class = "spicy_invalid_input"
  )
})


test_that("pd survives a mixed table (one Bayesian model is enough)", {
  # `pd` is gated on ANY Bayesian model, not ALL: the frequentist
  # cells dash, mirroring how p dashes for the Bayesian side.
  expect_no_error(
    spicy:::validate_class_appropriate_tokens(
      list(lm(mpg ~ wt, data = mtcars), fake_stanreg()),
      c("b", "pd"),
      character(0)
    )
  )
})


# ---- regression_validate.R 1313-1333: sampler diagnostics need ALL Bayesian ----

test_that("sampler-diagnostic columns need every model to be Bayesian", {
  err <- expect_error(
    table_regression(
      lm(mpg ~ wt, data = mtcars),
      show_columns = c("b", "rhat")
    ),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "Token(s) \"rhat\" in `show_columns` are shown only when every model is Bayesian.",
    fixed = TRUE
  )
  expect_match(msg, "sampler diagnostics", fixed = TRUE)
  # The footer already reports per-model sampler problems, so the hint
  # points there instead of suggesting a per-model column.
  expect_match(msg, "automatic convergence guard", fixed = TRUE)

  # A MIXED set is refused too (unlike pd, which any Bayesian model
  # enables): the diagnostics gate is an `all`, not an `any`.
  err2 <- expect_error(
    spicy:::validate_class_appropriate_tokens(
      list(fake_stanreg(), lm(mpg ~ wt, data = mtcars)),
      c("b", "ess_bulk", "mcse"),
      character(0)
    ),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err2), "\"ess_bulk\", \"mcse\"", fixed = TRUE)

  # All Bayesian: the same tokens pass.
  expect_no_error(
    spicy:::validate_class_appropriate_tokens(
      list(fake_stanreg(), fake_brmsfit()),
      c("b", "rhat", "ess_tail"),
      character(0)
    )
  )
})


# ---- regression_validate.R 1362-1375: likelihood fit stats on a Bayesian fit ----

test_that("likelihood-based fit statistics are refused for Bayesian fits", {
  err <- expect_error(
    table_regression(fake_stanreg(), show_fit_stats = c("nobs", "aic")),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "Token(s) \"aic\" in `show_fit_stats` are not defined for Bayesian fits.",
    fixed = TRUE
  )
  # The hint routes to loo rather than to a table column.
  expect_match(msg, "`loo::loo()` / `loo::loo_compare()`", fixed = TRUE)

  # The whole classical family is gated, in the vector's own order.
  err2 <- expect_error(
    spicy:::validate_class_appropriate_tokens(
      list(fake_stanreg()),
      character(0),
      c("nobs", "bic", "sigma", "rmse")
    ),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err2),
    "\"bic\", \"sigma\", \"rmse\"",
    fixed = TRUE
  )

  # `nobs` alone is class-agnostic and must survive.
  expect_no_error(
    spicy:::validate_class_appropriate_tokens(
      list(fake_stanreg()),
      character(0),
      "nobs"
    )
  )
})


# ---- table_regression.R 1939-1957: profile / bootstrap CI on a posterior ----

test_that("profile and bootstrap CIs are refused for Bayesian fits", {
  for (meth in c("profile", "boot_percentile")) {
    err <- expect_error(
      table_regression(fake_stanreg(), ci_method = meth),
      class = "spicy_invalid_input"
    )
    msg <- conditionMessage(err)
    expect_match(
      msg,
      sprintf("`ci_method = \"%s\"` is not defined for Bayesian fits.", meth),
      fixed = TRUE
    )
    expect_match(
      msg,
      "no profile likelihood or bootstrap replicates",
      fixed = TRUE
    )
    # The hint names the two intervals a posterior does have.
    expect_match(msg, "`ci_method = \"hdi\"`", fixed = TRUE)
  }
})


# ---- table_regression.R 2194-2205: p_adjust on an all-Bayesian table ----

test_that("`p_adjust` is refused when every model is Bayesian", {
  # Before this gate the request was a silent no-op (there are no
  # p-values to adjust), which is the failure mode the refusal fixes.
  err <- expect_error(
    table_regression(fake_stanreg(), p_adjust = "BH"),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "`p_adjust` is not available for Bayesian fits: there are no p-values to adjust.",
    fixed = TRUE
  )
  expect_match(msg, "show_columns = \"pd\"", fixed = TRUE)

  # `p_adjust = "none"` is the default and must NOT trip the gate. The
  # mock still fails further downstream (it has no draws to render),
  # but that failure is a bare base-R error whose message is
  # locale-dependent, so the assertion is on the gate's own condition
  # class: catching only `spicy_invalid_input` leaves NULL when the
  # gate stayed silent, and would return the condition if it fired.
  gate_fired <- tryCatch(
    table_regression(fake_stanreg(), p_adjust = "none"),
    spicy_invalid_input = function(e) e,
    error = function(e) NULL
  )
  expect_null(gate_fired)
})


# ---- regression_validate.R 314: the position-less class classifier ----

test_that("classify_unsupported_lm_class() omits the prefix without a position", {
  # Internal callers always pass `position`; the NULL default is the
  # bare-message contract, so it is pinned here directly.
  expect_identical(
    spicy:::classify_unsupported_lm_class(data.frame(x = 1)),
    paste0(
      "data.frame supplied where a model fit is expected. ",
      "Fit a model first: ",
      "`fit <- lm(y ~ x, data = your_data); table_regression(fit)`."
    )
  )
  # Same input WITH a position gains the prefix and nothing else.
  expect_identical(
    spicy:::classify_unsupported_lm_class(data.frame(x = 1), position = 2L),
    paste0(
      "Position 2: ",
      spicy:::classify_unsupported_lm_class(data.frame(x = 1))
    )
  )

  # The other two arms share the same prefix slot.
  expect_match(
    spicy:::classify_unsupported_lm_class(NULL),
    "^NULL element "
  )
  expect_match(
    spicy:::classify_unsupported_lm_class(
      structure(list(), class = "zzz_not_a_model")
    ),
    "^`zzz_not_a_model`"
  )
  # A supported class classifies as NULL (no complaint).
  expect_null(
    spicy:::classify_unsupported_lm_class(lm(mpg ~ wt, data = mtcars))
  )
})


# ---- regression_validate.R 2336: coefficient-name fallback for `labels` ----

test_that("`labels` keys fall back to coef() names when the helper fails", {
  # .spicy_fixed_coef_names() knows each class; the tryCatch around it
  # is the defensive arm for a class it does not know. Forcing the
  # helper to fail is the only way to reach the fallback, since every
  # shipped class it is asked about succeeds.
  m <- lm(mpg ~ wt, data = mtcars)
  testthat::local_mocked_bindings(
    .spicy_fixed_coef_names = function(fit) stop("helper unavailable"),
    .package = "spicy"
  )
  # "(Intercept)" is NOT a formula term label -- it can only become a
  # valid key through names(coef(fit)), so accepting it proves the
  # fallback ran.
  expect_silent(
    spicy:::validate_predictor_labels(c("(Intercept)" = "Constant"), list(m))
  )
  expect_silent(spicy:::validate_predictor_labels(c(wt = "Weight"), list(m)))
  # A key that is neither a term nor a coefficient name still errors.
  expect_error(
    spicy:::validate_predictor_labels(c(zzz = "Nope"), list(m)),
    "not term or coefficient names",
    class = "spicy_invalid_input"
  )
})
