# Bayesian-frame paths that no SAMPLED fixture reaches.
#
# Everything here runs without Stan: the guards fire before any draws
# work, the parsers take a draws matrix `posterior` can build on its
# own, and the vocabulary helpers are pure functions of a family. The
# fits in test-regression_frame_stan.R are `skip_on_ci()`; these are
# not, and they close the arms that sampling would never have reached
# anyway -- a missing dependency, an out-of-scope brms formula, a fit
# that carries no factor metadata, a family the fixtures do not fit.
#
# Lines closed in R/regression_frame_stan.R:
#   218-224, 230-236, 242-248 (missing-dependency refusals)
#   377, 402                  (brms beta-scope verdicts)
#   764-767                   (the draws_var_map invariant)
#   893, 896                  (factor metadata absent -> degrade)
#   1018, 1055                (reference rows: nothing to add)
#   1442-1447                 (family title vocabulary)

fake_stanreg <- function() {
  structure(list(), class = c("stanreg", "glm", "lm"))
}

fake_brmsfit <- function() {
  structure(list(), class = "brmsfit")
}

# A posterior a `posterior` draws object can be built from directly:
# 400 iterations of two parameters, which is what the sampled fixtures
# hand `.stan_coefs()` after `posterior::as_draws_array()`.
draws_matrix_fixture <- function(seed = 11L) {
  withr::local_seed(seed)
  cbind(
    `(Intercept)` = stats::rnorm(400L, 250, 5),
    Days = stats::rnorm(400L, 10, 1)
  )
}


# ---- 218-248: the three missing-dependency refusals ------------------------

test_that("a Bayesian frame without `posterior` names the package to install", {
  # `posterior` supplies every summary the Bayesian frame is made of
  # (median, MAD SD, quantiles, pd, R-hat), so its absence is not a
  # degraded table but a refusal -- and the refusal has to say which
  # package and how to get it, for both engines.
  local_mocked_bindings(spicy_pkg_available = function(pkg) pkg != "posterior")

  err <- expect_error(
    as_regression_frame(fake_brmsfit()),
    class = "spicy_missing_pkg"
  )
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "Cannot extract a regression frame from a Bayesian fit without `posterior`.",
    fixed = TRUE
  )
  expect_match(msg, "install.packages(\"posterior\")", fixed = TRUE)

  # The stanreg method asks the same question first, so the same
  # sentence answers it: the gate is shared, not duplicated per class.
  err2 <- expect_error(
    as_regression_frame(fake_stanreg()),
    class = "spicy_missing_pkg"
  )
  expect_match(
    conditionMessage(err2),
    "without `posterior`",
    fixed = TRUE
  )
})


test_that("each engine's own package is named when it is the missing one", {
  # `posterior` is present here; the ENGINE is not. The two refusals
  # must name their own package, or a stanreg user is told to install
  # brms and vice versa.
  local_mocked_bindings(spicy_pkg_available = function(pkg) pkg != "brms")
  err_brms <- expect_error(
    as_regression_frame(fake_brmsfit()),
    class = "spicy_missing_pkg"
  )
  expect_match(
    conditionMessage(err_brms),
    "Cannot extract a regression frame from a brmsfit without `brms`.",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err_brms),
    "install.packages(\"brms\")",
    fixed = TRUE
  )

  local_mocked_bindings(spicy_pkg_available = function(pkg) pkg != "rstanarm")
  err_arm <- expect_error(
    as_regression_frame(fake_stanreg()),
    class = "spicy_missing_pkg"
  )
  expect_match(
    conditionMessage(err_arm),
    "Cannot extract a regression frame from a stanreg fit without `rstanarm`.",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err_arm),
    "install.packages(\"rstanarm\")",
    fixed = TRUE
  )
})


# ---- 377, 402: the brms beta-scope verdicts -------------------------------

test_that("the brms beta scope names why a formula is out of reach", {
  # The algebraic standardization surface is fixed-effects fits with a
  # standard single formula, whose design matrix insight can rebuild.
  # Each refusal carries its OWN verdict string, because the refusal
  # message the caller composes cites it -- a multivariate fit and a
  # fit whose data is gone are not the same problem for the reader.
  mv <- structure(list(), class = c("mvbrmsformula", "brmsformula"))
  local_mocked_bindings(formula = function(x, ...) mv, .package = "stats")
  expect_identical(spicy:::.brms_beta_scope(fake_brmsfit()), "multivariate")
})


test_that("a brms fit that no longer carries its data is unrecoverable", {
  # Every earlier clause passes -- single formula, no distributional
  # parameters, no bars, no special terms -- and the scope still has to
  # refuse: the design matrix is rebuilt FROM `fit$data`, and a fit
  # stripped of it (saved with `save_pars`, loaded from an old rds)
  # cannot be standardized however simple its formula looks.
  bf <- structure(
    list(formula = Reaction ~ Days, pforms = list(), pfix = list()),
    class = "brmsformula"
  )
  local_mocked_bindings(formula = function(x, ...) bf, .package = "stats")
  expect_identical(spicy:::.brms_beta_scope(fake_brmsfit()), "unrecoverable")
})


# ---- 764-767: the draws_var_map invariant ---------------------------------

test_that("the brms coefficient path refuses to guess its draws names", {
  # brms prefixes every fixed effect with `b_`, so the human-readable
  # names and the draws-side names are different vocabularies and the
  # method passes the map between them. Without the map the function
  # would subset the draws by the DISPLAY names and silently return the
  # wrong parameters; it says so instead.
  skip_if_not_installed("posterior")
  m <- draws_matrix_fixture()
  err <- expect_error(
    spicy:::.stan_coefs(m, colnames(m), 0.95, brms_b_prefix = TRUE),
    class = "spicy_internal"
  )
  expect_match(
    conditionMessage(err),
    "Internal: brmsfit path requires draws_var_map.",
    fixed = TRUE
  )
})


# ---- 893, 896: a posterior with no factor metadata ------------------------

test_that("coefficient rows survive a fit that carries no factor metadata", {
  # Not every Bayesian object can be asked for `xlevels`. When the
  # accessor fails the frame must still be produced, with every
  # predictor treated as non-factor (parent_var = label = term) rather
  # than the whole table lost -- the graceful degradation the comment
  # above the accessor promises. A bare draws matrix IS such an object.
  skip_if_not_installed("posterior")
  m <- draws_matrix_fixture()
  coefs <- spicy:::.stan_coefs(m, colnames(m), 0.95)

  expect_identical(nrow(coefs), 2L)
  expect_identical(coefs$term, c("(Intercept)", "Days"))
  # Degraded metadata: the term stands for itself in all three columns,
  # and nothing is marked as a factor level or a reference.
  expect_identical(coefs$parent_var, coefs$term)
  expect_identical(coefs$label, coefs$term)
  expect_true(all(is.na(coefs$factor_level_pos)))
  expect_false(any(coefs$is_ref))
  # The Bayesian summaries themselves are unaffected.
  expect_equal(coefs$estimate, c(249.84599, 10.07493), tolerance = 1e-5)
  expect_true(all(is.na(coefs$p_value)))
  expect_true(all(coefs$pd >= 0.5 & coefs$pd <= 1))
})


# ---- 1018, 1055: reference rows with nothing to add -----------------------

test_that("a factor whose reference was kept contributes no reference row", {
  # Reference rows exist to restore the level treatment coding DROPPED.
  # A factor term whose reference is still in the design (a no-intercept
  # fit, a contrast that keeps every level) already has its row among
  # the coefficients, and adding a second, all-NA one would double it.
  # With no term left to restore, the block is the empty frame -- with
  # the `pd` column, so the rbind() with the coefficient table cannot
  # drift.
  local_mocked_bindings(
    detect_factor_terms = function(fit) {
      list(list(
        factor_term = "g",
        reference_level = "a",
        levels = c("a", "b"),
        reference_dropped = FALSE
      ))
    }
  )
  out <- spicy:::.stan_reference_rows(fake_brmsfit(), TRUE)
  expect_identical(nrow(out), 0L)
  expect_true("pd" %in% names(out))
  expect_identical(names(out), spicy:::.stan_coefs_schema)
})


# ---- 1442-1447: the family-title vocabulary -------------------------------

test_that("the Bayesian title names the family in the reader's words", {
  # The title prefix reads "Bayesian <family> regression (<engine>)".
  # Only a NON-identity link reaches this helper -- an identity link is
  # "Bayesian linear regression" without asking -- so these are the
  # words a Poisson, a Gamma or an inverse-Gaussian posterior gets.
  title <- function(family, link = "log") {
    spicy:::.stan_family_title(list(family = family, link = link))
  }
  expect_identical(title("poisson"), "Poisson")
  expect_identical(title("Gamma"), "Gamma")
  expect_identical(title("inverse.gaussian"), "inverse-Gaussian")
  expect_identical(title("gaussian"), "linear")
  # An unlisted family keeps its own name, lower-cased at the front so
  # it reads inside the sentence rather than shouting mid-title.
  expect_identical(title("negbinomial"), "negbinomial")
  expect_identical(title("Beta"), "beta")
  # The binomial arm above it stays link-aware, which is the reason the
  # two switches are separate.
  expect_identical(title("binomial", "probit"), "probit")
  expect_identical(title("bernoulli", "logit"), "logistic")
})
