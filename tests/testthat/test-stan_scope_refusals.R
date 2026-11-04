# The Bayesian models outside the scope of table_regression(), refused
# through the public call on real objects: an ordinal rstanarm::stan_polr()
# fit (sampled) and the ordinal and per-category brms families (unfitted
# brmsfit objects, `brm(empty = TRUE)`: the refusal reads the family
# alone). Local only, as every test that builds a Stan object
# (.skip_unless_local_stan(), helper-stan-cache.R). The mock-based unit
# test of the stan_polr refusal is in test-cov-pregel-stan.R.

test_that("an ordinal stan_polr fit is refused by a default table_regression() call", {
  .skip_unless_local_stan()
  skip_if_not_installed("rstanarm")
  fit <- suppressWarnings(rstanarm::stan_polr(
    factor(cyl) ~ wt,
    data = mtcars,
    prior = rstanarm::R2(0.3, what = "mean"),
    iter = 300,
    chains = 1,
    refresh = 0,
    seed = 6
  ))
  # rstanarm fitted the ordinal model: three outcome levels, the link kept
  # as a bare string instead of a family.
  expect_identical(nlevels(factor(mtcars$cyl)), 3L)
  expect_s3_class(fit, "polr")
  expect_true(is.character(fit$family))
  # The refusal comes first, whatever the fit statistics: the default
  # ordinal set was refused by the Bayesian gate instead, naming
  # statistics the user never asked for.
  expect_error(table_regression(fit), "stan_polr", class = "spicy_unsupported")
  expect_error(
    table_regression(fit, show_fit_stats = "aic"),
    "stan_polr",
    class = "spicy_unsupported"
  )
  expect_error(
    table_regression(fit, standardized = "posthoc"),
    "stan_polr",
    class = "spicy_unsupported"
  )
})

test_that("the ordinal and per-category brms families are refused", {
  .skip_unless_local_stan()
  skip_if_not_installed("brms")
  set.seed(1)
  d <- data.frame(x = stats::rnorm(60))
  d$yo <- cut(d$x + stats::rlogis(60), c(-Inf, -0.5, 0.5, Inf), labels = FALSE)
  d$yc <- factor(sample(c("a", "b", "c"), 60, replace = TRUE))
  d$yg <- d$x + stats::rnorm(60)
  for (fam in c("cumulative", "sratio", "cratio", "acat")) {
    m <- suppressMessages(brms::brm(
      yo ~ x,
      data = d,
      family = fam,
      empty = TRUE
    ))
    # The family brms built the model with.
    expect_identical(m$family$family, fam)
    expect_error(
      table_regression(m),
      paste0("Ordinal `brms` models (family `", fam, "`)"),
      fixed = TRUE,
      class = "spicy_unsupported"
    )
  }
  m <- suppressMessages(brms::brm(
    yc ~ x,
    data = d,
    family = brms::categorical(),
    empty = TRUE
  ))
  expect_identical(m$family$family, "categorical")
  expect_error(
    table_regression(m),
    "Categorical `brms` models (family `categorical`)",
    fixed = TRUE,
    class = "spicy_unsupported"
  )
  expect_error(
    as_regression_frame(m),
    "Categorical `brms` models",
    fixed = TRUE,
    class = "spicy_unsupported"
  )
  # The other families with one linear predictor per category: a count
  # matrix (multinomial) or a matrix of proportions (compositional).
  cnt <- t(replicate(60, as.vector(stats::rmultinom(1, 10, c(2, 3, 5)))))
  d$Y <- cnt
  d$size <- 10L
  d$P <- (cnt + 0.5) / rowSums(cnt + 0.5)
  multi <- list(
    multinomial = list(Y | trials(size) ~ x, "Multinomial"),
    dirichlet = list(P ~ x, "Compositional"),
    dirichlet2 = list(P ~ x, "Compositional"),
    logistic_normal = list(P ~ x, "Compositional")
  )
  for (fam in names(multi)) {
    m <- suppressMessages(brms::brm(
      multi[[fam]][[1L]],
      data = d,
      family = brms::brmsfamily(fam),
      empty = TRUE
    ))
    expect_identical(m$family$family, fam)
    expect_error(
      table_regression(m),
      paste0(multi[[fam]][[2L]], " `brms` models (family `", fam, "`)"),
      fixed = TRUE,
      class = "spicy_unsupported"
    )
  }
  # A glm-style family passes the refusal.
  mg <- suppressMessages(brms::brm(
    yg ~ x,
    data = d,
    family = stats::gaussian(),
    empty = TRUE
  ))
  expect_null(spicy:::.brms_refuse_ordinal(mg))
})

test_that("a compositional brms family is refused without a pointer to polr or multinom", {
  # The hint names polr, clm and multinom for an ordinal or a categorical
  # outcome only: none of them models a compositional outcome.
  fake <- function(family) list(family = list(family = family))
  e <- expect_error(
    spicy:::.brms_refuse_ordinal(fake("dirichlet")),
    class = "spicy_unsupported"
  )
  expect_no_match(conditionMessage(e), "multinom|polr|clm")
  e <- expect_error(
    spicy:::.brms_refuse_ordinal(fake("categorical")),
    class = "spicy_unsupported"
  )
  expect_match(conditionMessage(e), "nnet::multinom()", fixed = TRUE)
})
