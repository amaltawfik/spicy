# table_regression_models(): the machine-readable registry of supported
# classes. The same registry renders the documentation table, so this test
# also guards the published list against drift from the actual dispatch
# methods.

test_that("the registry matches the as_regression_frame dispatch methods", {
  reg <- table_regression_models()
  expect_s3_class(reg, "data.frame")
  expect_identical(
    names(reg),
    c("family", "class", "engine", "ame", "exponentiate", "blocks")
  )
  expect_true(all(nzchar(reg$class)))
  expect_false(any(duplicated(reg$class)))

  # Every registry class has a dispatch method...
  dispatched <- sub(
    "^as_regression_frame\\.",
    "",
    utils::methods("as_regression_frame")
  )
  dispatched <- setdiff(dispatched, "default")
  # (registry rows name the user-facing class; a few dispatch classes are
  # umbrella/inherited entries not listed separately)
  umbrella <- c(
    "lmerModLmerTest", # inherits the lmerMod row
    "spicy_uv_screen", # internal bundle built by
    # table_regression_uv(), not a
    # user-fitted model class
    "gee" # refusal stub: gee::gee fits inherit glm and would
    # otherwise silently get naive SEs; the method points at
    # geepack::geeglm (the supported, registry-listed engine)
  )
  expect_true(
    all(reg$class %in% dispatched),
    info = paste(
      "registry classes without a method:",
      paste(setdiff(reg$class, dispatched), collapse = ", ")
    )
  )
  # ...and every dispatch method is in the registry (no silent additions).
  expect_true(
    all(setdiff(dispatched, umbrella) %in% reg$class),
    info = paste(
      "methods missing from the registry:",
      paste(setdiff(setdiff(dispatched, umbrella), reg$class), collapse = ", ")
    )
  )
})

test_that("the rendered documentation table covers every registry row", {
  md <- spicy:::.render_supported_models_md()
  reg <- table_regression_models()
  expect_equal(length(strsplit(md, "\n")[[1]]), nrow(reg) + 2L)
})

## ---- Phase 3 matrix (lot T2) ----------------------------------------------

# Phase 3 matrix: rd-vcov-classes:help-topic-aliases
test_that("the per-family help page is reachable via its 8 documented aliases", {
  wanted <- c(
    "table_regression_models",
    "table_regression_mixed",
    "table_regression_ordinal",
    "table_regression_counts",
    "table_regression_categorical",
    "table_regression_survival",
    "table_regression_robust",
    "table_regression_bayesian"
  )
  rd_aliases <- function(rd) {
    tags <- vapply(
      rd,
      function(x) as.character(attr(x, "Rd_tag")),
      character(1)
    )
    unlist(lapply(rd[tags == "\\alias"], function(x) as.character(x[[1]])))
  }
  # Source tree when available (devtools::test), the built package's Rd
  # database otherwise (R CMD check on the installed copy).
  src <- testthat::test_path("..", "..", "man", "table_regression_models.Rd")
  aliases <- if (file.exists(src)) {
    rd_aliases(tools::parse_Rd(src))
  } else {
    rd_aliases(tools::Rd_db("spicy")[["table_regression_models.Rd"]])
  }
  expect_setequal(aliases, wanted)
})

# Phase 3 matrix: rd-vcov-classes:shared-programmatic-methods
# One representative fit per registry family: tidy() / glance() /
# as_structured() / as.data.frame() all return non-empty objects.
test_that("tidy / glance / as_structured / as.data.frame work for every registry family", {
  skip_if_not_installed("quantreg")
  skip_if_not_installed("lme4")
  skip_if_not_installed("geepack")
  skip_if_not_installed("MASS")
  skip_if_not_installed("nnet")
  skip_if_not_installed("pscl")
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("rms")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  set.seed(7)
  n <- 150
  d <- data.frame(x1 = rnorm(n), g = factor(sample(10, n, TRUE)))
  d$y <- 1 + 0.5 * d$x1 + rnorm(n)
  d$yo <- factor(sample(1:3, n, TRUE), ordered = TRUE)
  d$time <- rexp(n, exp(-0.2 * d$x1))
  d$status <- rbinom(n, 1, 0.7)
  data("engel", package = "quantreg", envir = environment())
  engel <- get("engel", envir = environment())
  data("bioChemists", package = "pscl", envir = environment())
  bioChemists <- get("bioChemists", envir = environment())
  des <- suppressWarnings(survey::svydesign(id = ~1, data = d))
  fits <- list(
    "Linear and generalized linear" = stats::lm(y ~ x1, data = d),
    "Robust, IV, quantile, panel" = quantreg::rq(
      foodexp ~ income,
      data = engel,
      tau = 0.5
    ),
    "Mixed effects" = lme4::lmer(
      Reaction ~ Days + (1 | Subject),
      data = lme4::sleepstudy
    ),
    "Population-averaged (GEE)" = geepack::geeglm(
      y ~ x1,
      id = g,
      data = d,
      corstr = "exchangeable"
    ),
    "Ordinal" = MASS::polr(yo ~ x1, data = d, Hess = TRUE),
    "Categorical" = nnet::multinom(yo ~ x1, data = d, trace = FALSE),
    "Counts, two-part" = pscl::zeroinfl(
      art ~ fem + ment | ment,
      data = bioChemists
    ),
    "Survival" = survival::coxph(
      survival::Surv(time, status) ~ x1,
      data = d
    ),
    "Survey-weighted" = survey::svyglm(y ~ x1, design = des),
    "Additive, proportions, selection" = mgcv::gam(y ~ x1, data = d),
    "rms" = rms::ols(y ~ x1, data = d, x = TRUE, y = TRUE),
    "Bayesian" = suppressWarnings(rstanarm::stan_glm(
      y ~ x1,
      data = d,
      iter = 400,
      chains = 1,
      refresh = 0,
      seed = 1
    ))
  )
  # The sample really spans EVERY registry family.
  expect_setequal(names(fits), unique(table_regression_models()$family))
  for (fam in names(fits)) {
    tbl <- suppressWarnings(table_regression(fits[[fam]]))
    td <- broom::tidy(tbl)
    gl <- broom::glance(tbl)
    st <- as_structured(tbl)
    df <- as.data.frame(tbl)
    expect_s3_class(td, "data.frame")
    expect_gt(nrow(td), 0L)
    expect_identical(nrow(gl), 1L)
    expect_gt(ncol(gl), 0L)
    st_body <- if (is.data.frame(st)) st else st$body
    expect_s3_class(st_body, "data.frame")
    expect_gt(nrow(st_body), 0L)
    expect_gt(nrow(df), 0L)
  }
})
