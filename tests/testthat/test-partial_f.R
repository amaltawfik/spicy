# The term-level test of a glm whose dispersion is estimated (gaussian,
# Gamma, inverse Gaussian, Tweedie, negative.binomial(theta) in glm(), the
# quasi families): the `partial_f` column, the F test that
# anova(reduced, full, test = "F") reports for the Type-II comparison of
# each term,
#
#   F = (Delta D / q) / phi  on (q, df.residual),
#
# phi the dispersion summary() estimates. An lm takes the same column,
# the Type II F of car::Anova(type = 2), which is the F of its gaussian
# glm twin. Oracles independent of spicy: car::Anova(type = 2,
# test.statistic = "F"), the pairwise anova(test = "F") and car::Anova()
# of the lm. A glm whose dispersion is fixed (binomial, Poisson, glm.nb)
# keeps the likelihood-ratio `partial_chi2`.

# The gaussian glm earns the "use lm()" caveat; nothing else is muffled.
.pf_quiet <- function(expr) {
  withCallingHandlers(
    expr,
    spicy_caveat = function(w) invokeRestart("muffleWarning")
  )
}

.pf_rows <- function(tbl, type = "partial_f") {
  td <- broom::tidy(tbl)
  td[td$estimate_type == type, , drop = FALSE]
}

# The row of a term: its own, or the first dummy of a factor or of an
# interaction with a factor (the dummies share their term's test).
.pf_term <- function(rows, term) {
  hit <- if (grepl(":", term, fixed = TRUE)) {
    startsWith(rows$term, term)
  } else {
    rows$term == term | rows$factor_term %in% term
  }
  rows[which(hit)[1L], , drop = FALSE]
}

.pf_data <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d
}

# The console cell expected for an F on (q, df): "7.29 (2, 28)".
.pf_cell <- function(f, q, df, mark = ".") {
  v <- formatC(f, format = "f", digits = 2L)
  if (!identical(mark, ".")) {
    v <- sub(".", mark, v, fixed = TRUE)
  }
  sprintf("%s (%d, %d)", v, as.integer(q), as.integer(df))
}

# The console cell of a row of a single-model table.
.pf_cell_of <- function(tbl, row, col = "F") {
  body <- structure(tbl, class = "data.frame")
  trimws(body[[col]][trimws(body$Variable) == row])
}

# car's default Pearson dispersion is computed at the final fitted means,
# summary()'s from the working weights of the last IRLS iteration. They
# differ by the convergence error: up to 1e-5 (relative) at the default
# epsilon, and still 2e-10 on Linux and macOS at epsilon 1e-20, where
# Windows reaches 1e-14. The car oracle therefore reads summary()'s
# dispersion (error.estimate = "dispersion"), which is exact on every
# platform; the tight control only keeps the Pearson gap small for the
# eye. The Gamma log fit needs no control: its working weights are 1.
.pf_families <- function() {
  tight <- glm.control(epsilon = 1e-20, maxit = 100)
  fams <- list(
    Gamma = list(Gamma("log"), "mpg", glm.control()),
    inverse.gaussian = list(inverse.gaussian("1/mu^2"), "mpg", tight),
    gaussian = list(gaussian(), "mpg", tight),
    quasipoisson = list(quasipoisson(), "carb", tight),
    quasibinomial = list(quasibinomial(), "am", tight)
  )
  if (requireNamespace("statmod", quietly = TRUE)) {
    fams$tweedie <- list(
      statmod::tweedie(var.power = 1.5, link.power = 0),
      "carb",
      tight
    )
  }
  fams
}

.pf_fit <- function(fam, rhs, data = .pf_data()) {
  glm(
    stats::as.formula(paste(fam[[2L]], "~", rhs)),
    data = data,
    family = fam[[1L]],
    control = fam[[3L]]
  )
}


# ---- Oracles --------------------------------------------------------------

test_that("partial_f equals car's Type II F test, additive and with an interaction", {
  skip_on_cran()
  skip_if_not_installed("car")
  fams <- .pf_families()
  n_checked <- 0L
  n_expected <- 0L
  for (rhs in c("wt + cyl", "wt * cyl")) {
    for (nm in names(fams)) {
      fit <- .pf_fit(fams[[nm]], rhs)
      rows <- .pf_rows(.pf_quiet(
        table_regression(fit, show_columns = c("b", "partial_f"))
      ))
      ca <- suppressWarnings(
        car::Anova(
          fit,
          type = 2,
          test.statistic = "F",
          error.estimate = "dispersion"
        )
      )
      terms <- setdiff(rownames(ca), "Residuals")
      n_expected <- n_expected + length(terms)
      for (tm in terms) {
        r <- .pf_term(rows, tm)
        lbl <- paste(nm, rhs, tm)
        expect_equal(
          r$estimate,
          ca[tm, "F value"],
          tolerance = 1e-10,
          label = lbl
        )
        expect_equal(r$statistic, r$estimate)
        expect_equal(
          r$p.value,
          ca[tm, "Pr(>F)"],
          tolerance = 1e-10,
          label = lbl
        )
        expect_equal(r$df, ca[tm, "Df"], label = lbl)
        expect_identical(r$test_type, "F")
        n_checked <- n_checked + 1L
      }
      # A factor of k = 3 levels is tested on q = k - 1 = 2.
      expect_equal(.pf_term(rows, "cyl")$df, 2)
    }
  }
  expect_oracle_covered(n_checked, n_expected)
})

test_that("partial_f of an additive model equals the pairwise anova(test = 'F')", {
  skip_on_cran()
  fams <- .pf_families()
  n_checked <- 0L
  for (nm in names(fams)) {
    fit <- .pf_fit(fams[[nm]], "wt + cyl")
    rows <- .pf_rows(.pf_quiet(
      table_regression(fit, show_columns = c("b", "partial_f"))
    ))
    for (tm in c("wt", "cyl")) {
      without <- .pf_fit(fams[[nm]], setdiff(c("wt", "cyl"), tm))
      av <- anova(without, fit, test = "F")
      r <- .pf_term(rows, tm)
      expect_equal(r$estimate, av$F[2L], tolerance = 1e-10, label = nm)
      expect_equal(r$p.value, av[["Pr(>F)"]][2L], tolerance = 1e-10)
      expect_equal(r$df, av$Df[2L])
      n_checked <- n_checked + 1L
    }
  }
  expect_oracle_covered(n_checked, 2L * length(fams))
})

test_that("the dispersion is summary()'s, at any convergence; drop1() is not the test", {
  skip_on_cran()
  skip_if_not_installed("car")
  d <- .pf_data()
  # Default control: car's estimate from summary()'s dispersion is the
  # same test to the last digit, the pairwise anova() too. Also for
  # negative.binomial(theta) fitted by glm(), whose dispersion summary()
  # estimates as well.
  fams <- list(quasipoisson(), MASS::negative.binomial(2))
  for (fam in fams) {
    fit <- glm(carb ~ wt + cyl, data = d, family = fam)
    rows <- .pf_rows(table_regression(fit, show_columns = c("b", "partial_f")))
    cd <- car::Anova(
      fit,
      type = 2,
      test.statistic = "F",
      error.estimate = "dispersion"
    )
    for (tm in c("wt", "cyl")) {
      expect_equal(
        .pf_term(rows, tm)$estimate,
        cd[tm, "F value"],
        tolerance = 1e-12
      )
      expect_equal(
        .pf_term(rows, tm)$p.value,
        cd[tm, "Pr(>F)"],
        tolerance = 1e-12
      )
    }
    av <- anova(glm(carb ~ cyl, data = d, family = fam), fit, test = "F")
    expect_equal(.pf_term(rows, "wt")$estimate, av$F[2L], tolerance = 1e-10)
  }

  # drop1(test = "F") divides by the deviance dispersion D / df.residual,
  # not by the Pearson one: a different number, in that exact ratio.
  g <- glm(mpg ~ wt + cyl, data = d, family = Gamma("log"))
  rows <- .pf_rows(table_regression(g, show_columns = c("b", "partial_f")))
  d1 <- drop1(g, test = "F")
  phi <- summary(g)$dispersion
  phi_dev <- deviance(g) / df.residual(g)
  for (tm in c("wt", "cyl")) {
    got <- .pf_term(rows, tm)$estimate
    expect_gt(abs(got - d1[tm, "F value"]), 0.1)
    expect_equal(got * phi / phi_dev, d1[tm, "F value"], tolerance = 1e-10)
  }
})

test_that("a main effect under an interaction does not depend on the factor coding", {
  skip_on_cran()
  d <- .pf_data()
  f_trt <- glm(mpg ~ wt * cyl, data = d, family = Gamma("log"))
  f_sum <- glm(
    mpg ~ wt * cyl,
    data = d,
    family = Gamma("log"),
    contrasts = list(cyl = "contr.sum")
  )
  r_trt <- .pf_rows(table_regression(f_trt, show_columns = c("b", "partial_f")))
  r_sum <- .pf_rows(table_regression(f_sum, show_columns = c("b", "partial_f")))
  for (tm in c("wt", "cyl")) {
    expect_equal(
      .pf_term(r_sum, tm)$estimate,
      .pf_term(r_trt, tm)$estimate,
      tolerance = 1e-8
    )
  }
})

test_that("partial_f of an lm is car's Type II F, and its gaussian glm twin prints the same", {
  skip_on_cran()
  skip_if_not_installed("car")
  d <- .pf_data()
  w <- d$hp / 100
  cases <- list(
    additive = list(form = mpg ~ wt + cyl, weights = NULL),
    interaction = list(form = mpg ~ wt * cyl, weights = NULL),
    weighted = list(form = mpg ~ wt + cyl, weights = w)
  )
  n_checked <- 0L
  for (nm in names(cases)) {
    form <- cases[[nm]]$form
    wt <- cases[[nm]]$weights
    l <- if (is.null(wt)) {
      lm(form, data = d)
    } else {
      lm(form, data = d, weights = wt)
    }
    g <- if (is.null(wt)) {
      glm(form, data = d)
    } else {
      glm(form, data = d, weights = wt)
    }
    rows_l <- .pf_rows(table_regression(l, show_columns = c("b", "partial_f")))
    rows_g <- .pf_rows(.pf_quiet(
      table_regression(g, show_columns = c("b", "partial_f"))
    ))
    expect_equal(rows_l$term, rows_g$term)
    expect_equal(
      rows_g$estimate,
      rows_l$estimate,
      tolerance = 1e-12,
      label = nm
    )
    expect_equal(rows_g$p.value, rows_l$p.value, tolerance = 1e-12, label = nm)
    expect_equal(rows_g$df, rows_l$df)
    expect_identical(unique(rows_l$test_type), "F")
    ca <- car::Anova(l, type = 2)
    for (tm in setdiff(rownames(ca), "Residuals")) {
      r <- .pf_term(rows_l, tm)
      lbl <- paste(nm, tm)
      expect_equal(
        r$estimate,
        ca[tm, "F value"],
        tolerance = 1e-12,
        label = lbl
      )
      expect_equal(r$p.value, ca[tm, "Pr(>F)"], tolerance = 1e-12, label = lbl)
      expect_equal(r$df, ca[tm, "Df"])
      n_checked <- n_checked + 1L
    }
  }
  expect_oracle_covered(n_checked, 7L)
})

test_that("an lm asked for partial_f alone skips the noncentral-F intervals", {
  d <- .pf_data()
  fit <- lm(mpg ~ wt + cyl, data = d)
  local_mocked_bindings(
    compute_omega2_ci_lm = function(...) stop("interval computed"),
    compute_f2_ci_lm = function(...) stop("interval computed")
  )
  rows <- .pf_rows(table_regression(fit, show_columns = c("b", "partial_f")))
  expect_identical(nrow(rows), 3L)
  # Any effect size beside it needs them.
  expect_error(
    table_regression(fit, show_columns = c("b", "partial_f", "partial_eta2")),
    "interval computed"
  )
})

test_that("binomial, Poisson and glm.nb keep the likelihood-ratio partial_chi2", {
  skip_on_cran()
  skip_if_not_installed("car")
  d <- .pf_data()
  d$cyl2 <- factor(ifelse(d$cyl == "4", "4", "6-8"))
  fits <- list(
    binomial = glm(am ~ wt * cyl2, data = d, family = binomial),
    poisson = glm(carb ~ wt * cyl, data = d, family = poisson),
    negbin = suppressWarnings(MASS::glm.nb(carb ~ wt * cyl, data = d))
  )
  n_checked <- 0L
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    rows <- .pf_rows(
      suppressWarnings(
        table_regression(fit, show_columns = c("b", "partial_chi2"))
      ),
      "partial_chi2"
    )
    ca <- suppressWarnings(car::Anova(fit, type = 2, test.statistic = "LR"))
    for (tm in rownames(ca)) {
      r <- .pf_term(rows, tm)
      expect_equal(
        r$estimate,
        ca[tm, "LR Chisq"],
        tolerance = 1e-10,
        label = nm
      )
      expect_equal(r$p.value, ca[tm, "Pr(>Chisq)"], tolerance = 1e-10)
      expect_equal(r$df, ca[tm, "Df"])
      expect_identical(r$test_type, "X2")
      n_checked <- n_checked + 1L
    }
    expect_false(any(
      broom::tidy(suppressWarnings(
        table_regression(fit, show_columns = c("b", "partial_chi2"))
      ))$estimate_type ==
        "partial_f"
    ))
  }
  expect_oracle_covered(n_checked, 9L)
})


# ---- Refusals -------------------------------------------------------------

test_that("a term-level token no model of the table carries is refused", {
  d <- .pf_data()
  gam_fit <- glm(mpg ~ wt + cyl, data = d, family = Gamma("log"))
  err <- tryCatch(
    table_regression(gam_fit, show_columns = c("b", "partial_chi2")),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "not defined for the model (`glm`, Gamma)",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "Use `\"partial_f\"`.", fixed = TRUE)

  refused_f <- list(
    poisson = glm(carb ~ wt, data = d, family = poisson),
    glm.nb = suppressWarnings(MASS::glm.nb(carb ~ wt, data = d))
  )
  if (requireNamespace("lme4", quietly = TRUE)) {
    refused_f$lmer <- lme4::lmer(mpg ~ wt + (1 | cyl), data = d)
  }
  for (nm in names(refused_f)) {
    err <- tryCatch(
      suppressWarnings(
        table_regression(refused_f[[nm]], show_columns = c("b", "partial_f"))
      ),
      error = identity
    )
    expect_s3_class(err, "spicy_invalid_input")
    expect_match(
      conditionMessage(err),
      "Use `\"partial_chi2\"`.",
      fixed = TRUE,
      label = nm
    )
    expect_match(
      conditionMessage(err),
      "negative.binomial(theta)",
      fixed = TRUE
    )
  }

  # lm refuses the chi-square, pointing at its F and its partials.
  err <- tryCatch(
    table_regression(
      lm(mpg ~ wt, data = d),
      show_columns = c("b", "partial_chi2")
    ),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "not defined for `lm` models",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "`\"partial_f\"`", fixed = TRUE)
  expect_match(conditionMessage(err), "partial_f2", fixed = TRUE)

  # No carrier beside a model with no term-level test: the column would
  # be blank for every model.
  skip_if_not_installed("survival")
  pois <- glm(carb ~ wt + cyl, data = d, family = poisson)
  cox <- survival::coxph(survival::Surv(mpg, am) ~ wt, data = d)
  for (cols in list(c("b", "partial_f"), c("b", "partial_chi2", "partial_f"))) {
    err <- tryCatch(
      table_regression(list(cox, pois), show_columns = cols),
      error = identity
    )
    expect_s3_class(err, "spicy_invalid_input")
    expect_match(
      conditionMessage(err),
      "Token `\"partial_f\"` in `show_columns` is not defined for model 1 (`coxph`), model 2 (`glm`, poisson).",
      fixed = TRUE
    )
  }
  skip_if_not_installed("survey")
  des <- survey::svydesign(ids = ~1, weights = ~1, data = d)
  svy <- survey::svyglm(mpg ~ wt + cyl, design = des)
  err <- tryCatch(
    suppressWarnings(
      table_regression(list(svy, pois), show_columns = c("b", "partial_f"))
    ),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "model 1 (`svyglm`, gaussian)",
    fixed = TRUE
  )
})

test_that("a token that would leave a model of the table without its test is refused", {
  d <- .pf_data()
  gam_fit <- glm(mpg ~ wt + cyl, data = d, family = Gamma("log"))
  pois <- glm(carb ~ wt + cyl, data = d, family = poisson)
  fl <- lm(mpg ~ wt + cyl, data = d)
  cases <- list(
    list(list(gam_fit, pois), "partial_chi2", "Model 1 (`glm`, Gamma)"),
    list(list(fl, pois), "partial_chi2", "Model 1 (`lm`)"),
    list(list(pois, fl), "partial_f", "Model 1 (`glm`, poisson)")
  )
  if (requireNamespace("lme4", quietly = TRUE)) {
    mer <- lme4::lmer(mpg ~ wt + (1 | cyl), data = d)
    cases <- c(
      cases,
      list(
        list(list(gam_fit, mer), "partial_chi2", "Model 1 (`glm`, Gamma)")
      )
    )
  }
  for (cs in cases) {
    other <- setdiff(c("partial_chi2", "partial_f"), cs[[2L]])
    err <- tryCatch(
      table_regression(cs[[1L]], show_columns = c("b", cs[[2L]])),
      error = identity
    )
    expect_s3_class(err, "spicy_invalid_input")
    expect_match(
      conditionMessage(err),
      sprintf(
        "%s has no `\"%s\"`: its term-level test is `\"%s\"`.",
        cs[[3L]],
        cs[[2L]],
        other
      ),
      fixed = TRUE
    )
    expect_match(
      conditionMessage(err),
      sprintf("add `\"%s\"` to `show_columns`, or drop the model", other),
      fixed = TRUE
    )
  }
  # Both tokens: each model fills its own column, the other stays blank.
  td <- broom::tidy(table_regression(
    list(gam_fit, pois),
    show_columns = c("b", "partial_chi2", "partial_f")
  ))
  ids <- unique(td$model_id)
  expect_identical(
    unique(td$model_id[td$estimate_type == "partial_f"]),
    ids[1L]
  )
  expect_identical(
    unique(td$model_id[td$estimate_type == "partial_chi2"]),
    ids[2L]
  )

  # A variance-explained partial names each model's own test.
  err <- tryCatch(
    table_regression(
      list(gam_fit, pois),
      show_columns = c("b", "partial_eta2")
    ),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "`\"partial_f\"` for model 1 (`glm`, Gamma), `\"partial_chi2\"` for model 2 (`glm`, poisson)",
    fixed = TRUE
  )
  err <- tryCatch(
    table_regression(gam_fit, show_columns = c("b", "partial_eta2")),
    error = identity
  )
  expect_match(
    conditionMessage(err),
    "Replace with `\"partial_f\"`",
    fixed = TRUE
  )
})

test_that("a model with no term-level test stays blank beside one that has it", {
  skip_if_not_installed("geepack")
  d <- .pf_data()
  d <- d[order(d$cyl), ]
  gee <- geepack::geeglm(mpg ~ wt + hp, id = cyl, data = d)
  gam_fit <- glm(mpg ~ wt + cyl, data = d, family = Gamma("log"))
  td <- broom::tidy(table_regression(
    list(gee, gam_fit),
    show_columns = c("b", "partial_f")
  ))
  ids <- unique(td$model_id)
  expect_identical(
    unique(td$model_id[td$estimate_type == "partial_f"]),
    ids[2L]
  )
  # No model carries the chi-square, whatever else is asked.
  for (cols in list(
    c("b", "partial_chi2"),
    c("b", "partial_chi2", "partial_f")
  )) {
    err <- tryCatch(
      table_regression(list(gee, gam_fit), show_columns = cols),
      error = identity
    )
    expect_s3_class(err, "spicy_invalid_input")
    expect_match(conditionMessage(err), "Use `\"partial_f\"`.", fixed = TRUE)
  }
})

test_that("a set of lm and glm models that share a test fills it for each", {
  d <- .pf_data()
  fb <- glm(am ~ wt, data = d, family = binomial)
  fg <- glm(mpg ~ wt, data = d, family = Gamma("log"))
  fl <- lm(mpg ~ wt + cyl, data = d)
  both <- broom::tidy(table_regression(
    list(fl, fg),
    show_columns = c("b", "partial_f")
  ))
  expect_identical(
    sort(unique(both$model_id[both$estimate_type == "partial_f"])),
    sort(unique(both$model_id))
  )
  # Two fixed-dispersion glm: the chi-square for both.
  chi <- broom::tidy(table_regression(
    list(fb, glm(carb ~ wt, data = d, family = poisson)),
    show_columns = c("b", "partial_chi2")
  ))
  expect_identical(
    sort(unique(chi$model_id[chi$estimate_type == "partial_chi2"])),
    sort(unique(chi$model_id))
  )
  # lm + Gamma asked the chi-square: neither carries it.
  expect_error(
    table_regression(list(fl, fg), show_columns = c("b", "partial_chi2")),
    "Use `\"partial_f\"`",
    class = "spicy_invalid_input"
  )
})

test_that("the dispersion is read quietly: no warning beyond the coefficients'", {
  d <- .pf_data()
  w <- rep(1, 32)
  w[1:4] <- 0
  g <- glm(mpg ~ wt + cyl, data = d, family = Gamma("log"), weights = w)
  count <- function(expr) {
    n <- 0L
    withCallingHandlers(expr, warning = function(w) {
      n <<- n + 1L
      invokeRestart("muffleWarning")
    })
    n
  }
  n_b <- count(table_regression(g, show_columns = c("b", "p")))
  n_f <- count(table_regression(g, show_columns = c("b", "p", "partial_f")))
  expect_gt(n_b, 0L)
  expect_identical(n_f, n_b)
})

test_that("compute_partial_f_for_term declines what it cannot test", {
  d <- .pf_data()
  # A fixed dispersion: the term-level test is the chi-square.
  expect_null(spicy:::compute_partial_f_for_term(
    glm(am ~ wt, data = d, family = binomial),
    "wt"
  ))
  # A term the model does not have.
  g <- glm(mpg ~ wt, data = d, family = Gamma("log"))
  expect_null(spicy:::compute_partial_f_for_term(g, "not_a_term"))
  # A perfect gaussian fit: the dispersion is 0 and the F is not finite.
  dperf <- data.frame(y = c(2, 2, 5, 5), x = factor(c("a", "a", "b", "b")))
  gperf <- glm(y ~ x, data = dperf)
  expect_identical(deviance(gperf), 0)
  expect_null(spicy:::compute_partial_f_for_term(gperf, "x"))
  # An intercept-only model has no term: no row, the bare definition.
  tbl <- table_regression(
    glm(mpg ~ 1, data = d, family = Gamma("log")),
    show_columns = c("b", "partial_f")
  )
  expect_false(any(broom::tidy(tbl)$estimate_type == "partial_f"))
  expect_match(attr(tbl, "note"), "F = partial F statistic;", fixed = TRUE)
})


# ---- Robust variance -------------------------------------------------------

test_that("the term-level tests are model-based under every vcov", {
  skip_on_cran()
  skip_if_not_installed("sandwich")
  skip_if_not_installed("clubSandwich")
  # The rule partial_chi2 already followed: the likelihood-ratio and F
  # tests come from the refits and the model's dispersion, whatever the
  # standard errors beside them. Neither is refused under a robust vcov.
  d <- .pf_data()
  cases <- list(
    list(glm(mpg ~ wt + cyl, data = d, family = Gamma("log")), "partial_f"),
    list(lm(mpg ~ wt + cyl, data = d), "partial_f"),
    list(glm(am ~ wt + cyl, data = d, family = binomial), "partial_chi2")
  )
  for (cs in cases) {
    fit <- cs[[1L]]
    tok <- cs[[2L]]
    ref <- .pf_rows(table_regression(fit, show_columns = c("b", tok)), tok)
    expect_identical(nrow(ref), 3L)
    for (v in c("HC3", "CR2", "bootstrap", "jackknife")) {
      args <- list(fit, show_columns = c("b", "se", tok), vcov = v)
      if (v == "CR2") {
        args$cluster <- ~gear
      }
      if (v == "bootstrap") {
        args$boot_n <- 20L
      }
      got <- .pf_rows(suppressWarnings(do.call(table_regression, args)), tok)
      expect_equal(got$estimate, ref$estimate, tolerance = 1e-12, label = v)
      expect_equal(got$p.value, ref$p.value, tolerance = 1e-12, label = v)
    }
  }
})


# ---- Rendering -------------------------------------------------------------

test_that("the F cell is 'value (q, df)', df the residual df of its own model", {
  skip_on_cran()
  d <- .pf_data()
  w <- rep(1, 32)
  w[1:4] <- 0
  fits <- list(
    gamma = glm(mpg ~ wt + cyl, data = d, family = Gamma("log")),
    lm = lm(mpg ~ wt + cyl, data = d),
    # Four zero weights leave the residual df at 24, not 28.
    weighted = lm(mpg ~ wt + cyl, data = d, weights = w)
  )
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    dfr <- df.residual(fit)
    tbl <- table_regression(fit, show_columns = c("b", "partial_f"))
    rows <- .pf_rows(tbl)
    # tidy(): the residual df beside the term's q, on every partial F row.
    expect_equal(rows$df.residual, rep(dfr, nrow(rows)), label = nm)
    for (tm in c("wt", "cyl")) {
      r <- .pf_term(rows, tm)
      expect_identical(
        .pf_cell_of(tbl, if (tm == "cyl") "6" else tm),
        .pf_cell(r$estimate, if (tm == "cyl") 2 else 1, dfr),
        label = paste(nm, tm)
      )
    }
    # The typed view: the residual df in a column of its own, beside q.
    df_tbl <- as.data.frame(tbl)
    at_6 <- df_tbl$variable == "cyl" & df_tbl$level %in% "6"
    expect_equal(df_tbl[["F: df"]][at_6], 2)
    expect_equal(df_tbl[["F: residual df"]][at_6], dfr, label = nm)
    meta <- as_structured(tbl)$col_meta[["F: residual df"]]
    expect_identical(meta$source_field, "df_residual")
    expect_true(meta$is_df)
    expect_identical(meta$precision, 0L)
    # The note defines the statistic only.
    note <- attr(tbl, "note")
    expect_match(note, "F = partial F statistic", fixed = TRUE)
    expect_false(grepl("degree", note, fixed = TRUE))
  }
  expect_identical(df.residual(fits$weighted), 24L)
  # Every row whose test is an F carries its residual df, the effect sizes
  # built on that F included, and the B rows leave it NA. The effect-size
  # cells keep a single number. The chi-square keeps "value (q)".
  es <- c("partial_f2", "partial_eta2", "partial_omega2")
  tbl_es <- table_regression(fits$lm, show_columns = c("b", es, "partial_f"))
  td <- broom::tidy(tbl_es)
  is_f <- td$test_type %in% "F"
  expect_setequal(unique(td$estimate_type[is_f]), c(es, "partial_f"))
  expect_equal(unique(td$df.residual[is_f]), df.residual(fits$lm))
  expect_true(all(is.na(td$df.residual[!is_f])))
  body_es <- structure(tbl_es, class = "data.frame")
  es_cells <- unlist(body_es[
    trimws(body_es$Variable) == "wt",
    setdiff(names(body_es), c("Variable", "B", "F"))
  ])
  expect_length(es_cells, 3L)
  expect_false(any(grepl("(", es_cells, fixed = TRUE)))
  expect_identical(
    grep("residual df", names(as.data.frame(tbl_es)), value = TRUE),
    "F: residual df"
  )
  chi <- table_regression(
    glm(am ~ wt + cyl, data = d, family = binomial),
    show_columns = c("b", "partial_chi2")
  )
  expect_match(
    .pf_cell_of(chi, "6", "\u03C7\u00B2"),
    "^[0-9]+[.][0-9]{2} \\(2\\)$"
  )
  expect_true(all(is.na(.pf_rows(chi, "partial_chi2")$df.residual)))
  expect_false(any(grepl("residual df", names(as.data.frame(chi)))))

  # Two models of the table, two residual df: each cell its own.
  g30 <- glm(mpg ~ wt, data = d, family = Gamma("log"))
  two <- table_regression(
    list(fits$gamma, g30),
    show_columns = c("b", "partial_f")
  )
  r2 <- .pf_rows(two)
  m2 <- r2[r2$model_id == unique(r2$model_id)[2L], ]
  out <- paste(capture.output(print(two)), collapse = "\n")
  expect_match(out, .pf_cell(m2$estimate, 1, df.residual(g30)), fixed = TRUE)
  expect_match(attr(two, "note"), "F = partial F statistic;", fixed = TRUE)

  # A univariable screen: the df of each predictor's own fit.
  uv <- table_regression_uv(
    d,
    outcome = mpg,
    predictors = c(wt, cyl),
    method = "glm",
    family = Gamma("log"),
    show_columns = c("b", "partial_f")
  )
  ruv <- .pf_rows(uv)
  uni <- ruv[ruv$model_id == "Univariable", ]
  df_wt <- df.residual(glm(mpg ~ wt, data = d, family = Gamma("log")))
  df_cyl <- df.residual(glm(mpg ~ cyl, data = d, family = Gamma("log")))
  expect_identical(c(df_wt, df_cyl), c(30L, 29L))
  expect_equal(.pf_term(uni, "wt")$df.residual, df_wt)
  expect_equal(.pf_term(uni, "cyl")$df.residual, df_cyl)
  expect_equal(
    unique(ruv$df.residual[ruv$model_id == "Multivariable"]),
    df.residual(fits$gamma)
  )
  out <- paste(capture.output(print(uv)), collapse = "\n")
  expect_match(
    out,
    .pf_cell(.pf_term(uni, "wt")$estimate, 1, df_wt),
    fixed = TRUE
  )
  expect_match(
    out,
    .pf_cell(.pf_term(uni, "cyl")$estimate, 2, df_cyl),
    fixed = TRUE
  )
  expect_match(attr(uv, "note"), "F = partial F statistic;", fixed = TRUE)

  # A stack of two lm panels with different residual df: each panel's
  # cells carry their own, and the note defines F once.
  la <- lm(mpg ~ wt + cyl, data = d)
  lb <- lm(mpg ~ wt, data = d)
  ta <- table_regression(la, show_columns = c("b", "partial_f"))
  tb <- table_regression(lb, show_columns = c("b", "partial_f"))
  st <- table_stack(A = ta, B = tb)
  out <- paste(capture.output(print(st)), collapse = "\n")
  f_a <- .pf_term(.pf_rows(ta), "wt")$estimate
  f_b <- .pf_term(.pf_rows(tb), "wt")$estimate
  expect_match(out, .pf_cell(f_a, 1, df.residual(la)), fixed = TRUE)
  expect_match(out, .pf_cell(f_b, 1, df.residual(lb)), fixed = TRUE)
  st_df <- as.data.frame(st)
  expect_equal(
    st_df[["F: residual df"]][st_df$panel == "B" & st_df$variable == "wt"],
    df.residual(lb)
  )
  st_note <- attr(st, "note")
  expect_match(st_note, "F = partial F statistic.", fixed = TRUE)
  expect_identical(lengths(regmatches(st_note, gregexpr("F = ", st_note))), 1L)
  expect_false(grepl("degree", st_note, fixed = TRUE))

  # French: the decimal comma in the statistic, the df unchanged.
  withr::local_options(spicy.language = "fr")
  fr <- table_regression(fits$gamma, show_columns = c("b", "partial_f"))
  r <- .pf_term(.pf_rows(fr), "cyl")
  expect_identical(
    .pf_cell_of(fr, "6"),
    .pf_cell(r$estimate, 2, df.residual(fits$gamma), mark = ",")
  )
  expect_match(attr(fr, "note"), "F = statistique F partielle", fixed = TRUE)
  expect_false(grepl("libert", attr(fr, "note"), fixed = TRUE))
})

test_that("a residual df of 1000 or more prints without a separator", {
  skip_on_cran()
  set.seed(20261112)
  n <- 1100L
  big <- data.frame(
    x = rnorm(n),
    g = factor(sample(c("a", "b", "c"), n, replace = TRUE))
  )
  big$y <- 0.3 * big$x + rnorm(n)
  fit <- lm(y ~ x + g, data = big)
  dfr <- df.residual(fit)
  expect_identical(dfr, 1096L)
  tbl <- table_regression(fit, show_columns = c("b", "partial_f"))
  rows <- .pf_rows(tbl)
  f_x <- .pf_term(rows, "x")$estimate
  expect_identical(.pf_cell_of(tbl, "x"), .pf_cell(f_x, 1, dfr))
  expect_identical(
    .pf_cell_of(tbl, "b"),
    .pf_cell(.pf_term(rows, "g")$estimate, 2, dfr)
  )
  # Nor in French, whose thousands separator is a space.
  withr::local_options(spicy.language = "fr")
  fr <- table_regression(fit, show_columns = c("b", "partial_f"))
  expect_identical(.pf_cell_of(fr, "x"), .pf_cell(f_x, 1, dfr, mark = ","))
})

# ---- The rich outputs -------------------------------------------------------
#
# The rich engines (tinytable, gt, flextable, Word, the clipboard) print
# the term-level test as the console does: one column under the
# statistic's header, "18.08 (1, 28)". Excel keeps the statistic a number,
# its df in the number format. The machine outputs keep the three numbers.

# A rendered cell as the console prints it: the engines pad with figure
# spaces, and flextable prints the minus as U+2212.
.pf_norm <- function(x) {
  x <- gsub("\u2212", "-", as.character(x), fixed = TRUE)
  trimws(gsub("[\u2007\u00A0]", " ", x))
}

# The cell of the row labelled `row` in column `col` of an engine's data.
.pf_engine_cell <- function(df, row, col) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  .pf_norm(df[[col]][.pf_norm(df[[1L]]) == row])
}

# The rows of a clipboard payload, as character vectors.
.pf_clip_rows <- function(tbl) {
  strsplit(
    strsplit(spicy:::clipboard_payload(tbl, "\t"), "\r?\n")[[1L]],
    "\t",
    fixed = TRUE
  )
}

# The sheet `output = "excel"` writes: its cells read back, and the
# number format and the type ("" or NA for a number) of a cell.
.pf_xlsx <- function(...) {
  path <- tempfile(fileext = ".xlsx")
  table_regression(..., output = "excel", excel_path = path)
  wb <- openxlsx2::wb_load(path)
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  styles <- wb$styles_mgr$styles
  numfmt <- function(ref) {
    s <- as.integer(cc$c_s[cc$r == ref][1L])
    id <- sub('.*numFmtId="([0-9]+)".*', "\\1", styles$cellXfs[[s + 1L]])
    hit <- grep(paste0('numFmtId="', id, '"'), styles$numFmts, value = TRUE)
    gsub("&quot;", "\"", sub('.*formatCode="([^"]*)".*', "\\1", hit[1L]))
  }
  sheet <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  list(
    sheet = sheet,
    # The Excel row of the row labelled `label`.
    row = function(label) {
      as.integer(rownames(sheet)[which(.pf_norm(sheet[[1L]]) == label)])
    },
    numfmt = numfmt,
    type = function(ref) cc$c_t[cc$r == ref][1L]
  )
}

test_that("tinytable prints the term-level test in one column, as the console", {
  skip_on_cran()
  skip_if_not_installed("tinytable")
  d <- .pf_data()
  fl <- lm(mpg ~ wt + cyl, data = d)
  fb <- glm(am ~ wt + cyl, data = d, family = binomial)
  chi <- "\u03C7\u00B2"
  check <- function(fit, tok, head, cells) {
    tbl <- table_regression(fit, show_columns = c("b", tok))
    tt <- table_regression(
      fit,
      show_columns = c("b", tok),
      output = "tinytable"
    )
    expect_identical(tt@names, c("Variable", "B", head))
    got <- vapply(
      c("wt", "6"),
      function(r) .pf_engine_cell(tt@data, r, 3L),
      "",
      USE.NAMES = FALSE
    )
    expect_identical(got, cells)
    expect_identical(
      got,
      c(.pf_cell_of(tbl, "wt", head), .pf_cell_of(tbl, "6", head))
    )
  }
  check(fl, "partial_f", "F", c("18.08 (1, 28)", "7.29 (2, 28)"))
  check(fb, "partial_chi2", chi, c("19.27 (1)", "4.52 (2)"))
  # French: the console's header and the decimal comma, the df unchanged.
  withr::local_options(spicy.language = "fr")
  check(fl, "partial_f", "F", c("18,08 (1, 28)", "7,29 (2, 28)"))
  check(fb, "partial_chi2", chi, c("19,27 (1)", "4,52 (2)"))
})

test_that("gt, flextable, Word and the clipboard print the same column", {
  skip_on_cran()
  d <- .pf_data()
  fl <- lm(mpg ~ wt + cyl, data = d)
  build <- function(output = "default", ...) {
    table_regression(
      fl,
      show_columns = c("b", "partial_f"),
      output = output,
      ...
    )
  }
  # The clipboard, with an interval after the test and stars: the bounds
  # keep their columns once the df columns are gone, and the starred B
  # keeps its marker beside the test.
  rows <- .pf_clip_rows(
    table_regression(
      fl,
      show_columns = c("b", "partial_f", "ci"),
      stars = TRUE
    )
  )
  expect_true(list(c("Variable", "B", "F", "95% CI", "95% CI")) %in% rows)
  expect_true(list(c("", "", "", "LL", "UL")) %in% rows)
  wt <- Filter(function(r) identical(r[[1L]], "wt"), rows)[[1L]]
  expect_identical(wt[1:3], c("wt", "-3.21***", "18.08 (1, 28)"))
  expect_length(wt, 5L)
  skip_if_not_installed("gt")
  g <- build("gt")
  expect_identical(
    unlist(g[["_spanners"]]$spanner_label),
    c("Variable", "B", "F")
  )
  expect_identical(.pf_engine_cell(g[["_data"]], "wt", 3L), "18.08 (1, 28)")
  skip_if_not_installed("flextable")
  ft <- build("flextable")
  hdr <- ft$header$dataset
  expect_identical(unname(unlist(hdr[nrow(hdr), ])), c("Variable", "B", "F"))
  expect_identical(.pf_engine_cell(ft$body$dataset, "6", 3L), "7.29 (2, 28)")
  skip_if_not_installed("officer")
  path <- tempfile(fileext = ".docx")
  build("word", word_path = path)
  s <- officer::docx_summary(officer::read_docx(path))
  cells <- s[s$content_type == "table cell", ]
  word <- lapply(split(cells, cells$row_id), function(r) {
    .pf_norm(r$text[order(r$cell_id)])
  })
  expect_true(list(c("Variable", "B", "F")) %in% word)
  expect_true(list(c("wt", "-3.21", "18.08 (1, 28)")) %in% word)
})

test_that("Excel keeps the statistic a number, its df in the number format", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  d <- .pf_data()
  fl <- lm(mpg ~ wt + cyl, data = d)
  fb <- glm(am ~ wt + cyl, data = d, family = binomial)
  f_rows <- .pf_rows(table_regression(fl, show_columns = c("b", "partial_f")))
  x <- .pf_xlsx(fl, show_columns = c("b", "partial_f"))
  # Three columns: the df columns are not written.
  expect_identical(ncol(x$sheet), 3L)
  expect_true(
    list(c("Variable", "B", "F")) %in%
      lapply(
        seq_len(nrow(x$sheet)),
        function(i) unname(unlist(x$sheet[i, ]))
      )
  )
  wt <- x$row("wt")
  expect_equal(
    as.numeric(x$sheet[as.character(wt), 3L]),
    .pf_term(f_rows, "wt")$estimate,
    tolerance = 1e-12
  )
  expect_identical(x$numfmt(paste0("C", wt)), "0.00\" (1, 28)\"")
  expect_identical(x$numfmt(paste0("C", x$row("6"))), "0.00\" (2, 28)\"")
  expect_true(x$type(paste0("C", wt)) %in% c(NA, ""))
  # The chi-square: its one df.
  y <- .pf_xlsx(fb, show_columns = c("b", "partial_chi2"))
  expect_identical(ncol(y$sheet), 3L)
  expect_identical(y$numfmt(paste0("C", y$row("6"))), "0.00\" (2)\"")
  # A decimal comma sends the body out as text: the console's cell.
  z <- .pf_xlsx(fl, show_columns = c("b", "partial_f"), decimal_mark = ",")
  expect_identical(
    .pf_norm(z$sheet[as.character(z$row("wt")), 3L]),
    "18,08 (1, 28)"
  )
})

test_that("the machine outputs keep the statistic and its df as numbers", {
  skip_on_cran()
  d <- .pf_data()
  fl <- lm(mpg ~ wt + cyl, data = d)
  tbl <- table_regression(fl, show_columns = c("b", "partial_f"))
  df_tbl <- as.data.frame(tbl)
  expect_identical(
    names(df_tbl)[5:8],
    c("B", "F", "F: df", "F: residual df")
  )
  at_wt <- df_tbl$variable == "wt"
  expect_equal(
    unlist(df_tbl[at_wt, c("F: df", "F: residual df")]),
    c(1, 28),
    ignore_attr = TRUE
  )
  expect_identical(
    df_tbl,
    table_regression(
      fl,
      show_columns = c("b", "partial_f"),
      output = "data.frame"
    )
  )
  td <- .pf_rows(tbl)
  expect_equal(td$df, c(1, 2, 2))
  expect_equal(td$df.residual, rep(28, 3L))
  # The typed view: three numeric columns, the console's cell in
  # `display_cells` of the statistic.
  s <- as_structured(tbl)
  expect_true(all(c("F", "F: df", "F: residual df") %in% names(s$body)))
  expect_true(s$col_meta[["F: residual df"]]$is_df)
  disp <- s$col_meta$F$display_cells
  expect_identical(disp[s$body$.variable == "wt"], "18.08 (1, 28)")
  expect_true(all(is.na(disp[is.na(s$body$F)])))
  # A sentence cites the statistic.
  expect_identical(inline(tbl, wt, column = "partial_f"), "18.08")
})

test_that("each model, panel and screen block has one test column", {
  skip_on_cran()
  skip_if_not_installed("tinytable")
  d <- .pf_data()
  fl <- lm(mpg ~ wt + cyl, data = d)
  three <- table_regression(
    list(fl, fl, fl),
    show_columns = c("b", "partial_f"),
    output = "tinytable"
  )
  expect_identical(three@names, c("Variable", rep(c("B", "F"), 3L)))
  expect_identical(
    vapply(c(3L, 5L, 7L), function(j) .pf_engine_cell(three@data, "wt", j), ""),
    rep("18.08 (1, 28)", 3L)
  )
  # The model spanners cover each model's two columns.
  s3 <- spicy:::.reg_engine_view(
    as_structured(table_regression(
      list(fl, fl, fl),
      show_columns = c("b", "partial_f")
    ))
  )
  expect_identical(unname(s3$spanners), list(2:3, 4:5, 6:7))
  # A univariable screen and a stack, each panel with its own df.
  uv <- table_regression_uv(
    d,
    outcome = mpg,
    predictors = c(wt, cyl),
    method = "lm",
    show_columns = c("b", "partial_f"),
    output = "tinytable"
  )
  expect_identical(uv@names, c("Variable", "B", "F", "B", "F"))
  expect_identical(.pf_engine_cell(uv@data, "wt", 3L), "91.38 (1, 30)")
  tb <- table_regression(
    lm(mpg ~ wt, data = d),
    show_columns = c("b", "partial_f")
  )
  st <- table_stack(
    A = table_regression(fl, show_columns = c("b", "partial_f")),
    B = tb,
    output = "tinytable"
  )
  expect_identical(st@names, c("Variable", "B", "F"))
  expect_identical(
    .pf_engine_cell(st@data, "wt", 3L),
    c("18.08 (1, 28)", "91.38 (1, 30)")
  )
  skip_if_not_installed("openxlsx2")
  path <- tempfile(fileext = ".xlsx")
  table_stack(A = tb, B = tb, output = "excel", excel_path = path)
  expect_identical(ncol(openxlsx2::wb_to_df(path, col_names = FALSE)), 3L)
})

test_that("the test column stays aligned on its decimal mark, digits 2 or 0", {
  skip_on_cran()
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  d <- .pf_data()
  fl <- lm(mpg ~ wt + cyl, data = d)
  # The column of the decimal mark of each cell, or the end of its integer
  # part; the df after the statistic are not read.
  anchor <- function(x) {
    x <- gsub("\u2007", " ", as.character(x), fixed = TRUE)
    core <- sub(" \\([^()]*\\) *$", "", x)
    core <- sub(" +$", "", core)
    pos <- regexpr(".", core, fixed = TRUE)
    out <- ifelse(pos > 0L, pos, nchar(core) + 1L)
    unique(out[grepl("[0-9]", core)])
  }
  for (es in c(2L, 0L)) {
    args <- list(
      fl,
      show_columns = c("b", "partial_f"),
      digits = es,
      effect_size_digits = es
    )
    cell <- .pf_cell_of(do.call(table_regression, args), "6")
    expect_identical(cell, if (es == 2L) "7.29 (2, 28)" else "7 (2, 28)")
    tt <- do.call(table_regression, c(args, output = "tinytable"))
    g <- do.call(table_regression, c(args, output = "gt"))
    expect_length(anchor(tt@data[[3L]]), 1L)
    expect_length(anchor(g[["_data"]][[3L]]), 1L)
  }
})

test_that("output = 'long' names the token, its test, its q and its residual df", {
  d <- .pf_data()
  fit <- glm(mpg ~ wt + cyl, data = d, family = Gamma("log"))
  lo <- table_regression(
    fit,
    show_columns = c("b", "partial_f"),
    output = "long"
  )
  pf_rows <- lo[lo$estimate_type == "partial_f", ]
  expect_identical(pf_rows$term, c("wt", "cyl6", "cyl8"))
  expect_identical(unique(pf_rows$test_type), "F")
  expect_equal(pf_rows$df, c(1, 2, 2))
  expect_equal(pf_rows$df.residual, rep(df.residual(fit), 3L))
})

test_that("console snapshot of the partial F column", {
  skip_on_cran()
  d <- .pf_data()
  fit <- glm(mpg ~ wt + cyl, data = d, family = Gamma("log"))
  expect_snapshot(print(
    table_regression(fit, show_columns = c("b", "se", "p", "partial_f"))
  ))
})
