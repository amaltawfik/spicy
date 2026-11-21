# ---------------------------------------------------------------------------
# Phase 6i tests: as_regression_frame() methods for flexsurv +
# sampleSelection.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_flexsurv_weibull <- function() {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")
  flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung,
    dist = "weibull"
  )
}

.fit_flexsurv_lognormal <- function() {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")
  flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age,
    data = survival::lung,
    dist = "lognormal"
  )
}

.fit_selection_heckman <- function() {
  skip_if_not_installed("sampleSelection")
  data("Mroz87", package = "sampleSelection", envir = environment())
  sampleSelection::selection(
    lfp ~ age + faminc + kids5 + educ,
    wage ~ exper + educ + city,
    data = Mroz87
  )
}


# ---- 1. flexsurv: schema validity + core fields --------------------------

test_that("as_regression_frame.flexsurvreg produces a schema-valid frame", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("flexsurvreg: info$class is 'flexsurvreg'", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "flexsurvreg")
})

test_that("flexsurvreg Weibull: family = weibull/log", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "weibull")
  expect_identical(fr$info$family$link, "log")
})

test_that("flexsurvreg Weibull: title = 'Weibull parametric survival regression'", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Weibull parametric survival regression"
  )
})

test_that("flexsurvreg lognormal: title = 'Log-normal parametric survival regression'", {
  fit <- .fit_flexsurv_lognormal()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Log-normal parametric survival regression"
  )
})


# ---- 2. flexsurv: aux parameters in extras (not coefs) -----------------

test_that("flexsurvreg: shape / scale rows NOT in coefs (stashed in extras)", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false("shape" %in% fr$coefs$term)
  expect_false("scale" %in% fr$coefs$term)
  expect_true("shape" %in% names(fr$info$extras$aux_parameters))
  expect_true("scale" %in% names(fr$info$extras$aux_parameters))
})

test_that("flexsurvreg: distribution name surfaced in extras", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$distribution, "weibull")
})


# ---- 3. flexsurv: coef extraction -------------------------------------

test_that("flexsurvreg: coefs match fit$res for covariate rows", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  cov_names <- c("age", "sex")
  for (nm in cov_names) {
    expect_equal(
      fr$coefs$estimate[fr$coefs$term == nm],
      unname(fit$res[nm, "est"]),
      tolerance = 1e-10
    )
    expect_equal(
      fr$coefs$std_error[fr$coefs$term == nm],
      unname(fit$res[nm, "se"]),
      tolerance = 1e-10
    )
  }
})

test_that("flexsurvreg: Wald z derived from est/se", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  non_ref <- fr$coefs[!fr$coefs$is_ref, ]
  expected_z <- non_ref$estimate / non_ref$std_error
  expect_equal(non_ref$statistic, expected_z, tolerance = 1e-12)
  expected_p <- 2 * stats::pnorm(-abs(expected_z))
  expect_equal(non_ref$p_value, expected_p, tolerance = 1e-12)
})

test_that("flexsurvreg: supports$exponentiate = TRUE; nested_lrt = TRUE", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
  expect_true(fr$info$supports$nested_lrt)
})


# ---- 4. selection: schema validity + core fields ----------------------

test_that("as_regression_frame.selection produces a schema-valid frame", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("selection: info$class is 'selection'", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "selection")
})

test_that("selection: title = 'Heckman selection model'", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Heckman selection model")
})

test_that("selection: family = heckman/identity", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "heckman")
  expect_identical(fr$info$family$link, "identity")
})


# ---- 5. selection: two-block coefs ------------------------------------

test_that("selection: coefs has two outcome_level groups ('selection' + 'outcome')", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_setequal(unique(fr$coefs$outcome_level), c("selection", "outcome"))
})

test_that("selection: each block has the predictor count from the engine", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  sel_rows <- fr$coefs[fr$coefs$outcome_level == "selection", ]
  out_rows <- fr$coefs[fr$coefs$outcome_level == "outcome", ]
  # Selection has Intercept + age + faminc + kids5 + educ = 5
  expect_identical(nrow(sel_rows), 5L)
  # Outcome has Intercept + exper + educ + city = 4
  expect_identical(nrow(out_rows), 4L)
})


# ---- 6. selection: sigma + rho + method in extras -------------------

test_that("selection: sigma + rho surfaced in extras", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.numeric(fr$info$extras$selection_sigma))
  expect_true(is.numeric(fr$info$extras$selection_rho))
})

test_that("selection: estimation_method surfaced in extras", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(
    fr$info$extras$estimation_method %in%
      c("Maximum likelihood", "Heckman two-step")
  )
})


# ---- 7. Phase 7c5: term + label prefixed with block name -----------

test_that("selection: term is prefixed with the block name (uniqueness)", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(any(grepl("^selection: ", fr$coefs$term)))
  expect_true(any(grepl("^outcome: ", fr$coefs$term)))
  # Term-uniqueness within the model (otherwise the body builder
  # collapses the two (Intercept) rows into one).
  expect_identical(length(unique(fr$coefs$term)), nrow(fr$coefs))
})

test_that("table_regression() body shows separate selection vs outcome rows", {
  fit <- .fit_selection_heckman()
  combined <- paste(
    capture.output(print(table_regression(fit))),
    collapse = "\n"
  )
  # `educ` appears in BOTH equations -- the body should show both rows,
  # under its header, each named by its equation, beside another model
  # (alone, the equations are column groups).
  shown <- trimws(table_regression(list(fit, fit))$Variable)
  expect_identical(
    shown[which(shown == "educ:") + 1:2],
    c("selection", "outcome")
  )
  expect_identical(
    shown[which(shown == "(Intercept):") + 1:2],
    c("selection", "outcome")
  )
})


## ---- Phase 3 matrix (lot T2) ----------------------------------------------

# Phase 3 matrix: rd-vcov-classes:registry-flexsurvreg
test_that("flexsurvreg surfaces its distribution parameters in the footer", {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")
  lung2 <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age + sex,
    data = lung2,
    dist = "weibull"
  )
  note <- attr(table_regression(fit), "note")
  expect_match(note, "Distribution: Weibull", fixed = TRUE)
  expect_match(note, "shape = ", fixed = TRUE)
  expect_match(note, "scale = ", fixed = TRUE)
  # The footer values are the fit's own auxiliary parameters.
  fr <- as_regression_frame(fit)
  aux <- fr$info$extras$aux_parameters
  expect_true(all(c("shape", "scale") %in% names(aux)))
  expect_match(note, sprintf("shape = %.2f", aux[["shape"]]), fixed = TRUE)
  expect_match(note, sprintf("scale = %.2f", aux[["scale"]]), fixed = TRUE)
})

test_that("a flexsurv rate below 0.01 keeps two significant digits in the footer", {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")
  lung2 <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex")])
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age + sex,
    data = lung2,
    dist = "exp"
  )
  # The fit's own rate (about 0.0017 per day), rounded by R alone.
  shown <- format(signif(fit$res["rate", "est"], 2L), scientific = FALSE)
  expect_false(grepl("^0[.]0*$", shown))
  expect_match(
    attr(table_regression(fit), "note"),
    paste0("Distribution: Exponential; rate = ", shown, "."),
    fixed = TRUE
  )
  expect_match(
    attr(table_regression(fit, decimal_mark = ","), "note"),
    paste0("rate = ", chartr(".", ",", shown), "."),
    fixed = TRUE
  )
})

# Phase 3 matrix: rd-vcov-classes:registry-selection
test_that("selection advertises no AME and refuses the ame column", {
  skip_if_not_installed("sampleSelection")
  set.seed(11)
  d <- data.frame(z = rnorm(300), x = rnorm(300))
  d$sel <- rbinom(300, 1, plogis(0.5 + d$z)) == 1
  d$w <- ifelse(d$sel, 1 + 0.5 * d$x + rnorm(300), NA)
  fit <- sampleSelection::selection(sel ~ z, w ~ x, data = d)
  expect_false(isTRUE(as_regression_frame(fit)$info$supports$ame))
  expect_error(
    table_regression(fit, show_columns = c("b", "ame"), output = "data.frame"),
    class = "spicy_invalid_input"
  )
})

test_that("selection refuses a robust vcov before it looks up `cluster`", {
  # Even a variable of the data: the cluster lookup does not find it
  # through a selection fit, and used to answer first.
  fit <- .fit_selection_heckman()
  for (cl in list(~age, ~not_a_variable)) {
    err <- expect_error(
      table_regression(fit, vcov = "CR1", cluster = cl),
      class = "spicy_unsupported_vcov"
    )
    expect_match(conditionMessage(err), "`selection` models", fixed = TRUE)
  }
  # The refusal lists what the class supports and promises nothing.
  expect_match(
    conditionMessage(err),
    "This class supports: classical.",
    fixed = TRUE
  )
  expect_no_match(conditionMessage(err), "being added", fixed = TRUE)
})


# ---- Label overrides (register 55) ---------------------------------------

test_that("flexsurvreg accepts `labels =` keyed on coefficient names", {
  # `stats::terms()` raises on the fit itself; the label validator used
  # to die there before any label could be applied.
  fit <- .fit_flexsurv_weibull()
  out <- table_regression(fit, labels = c(age = "Age (years)"))
  expect_true("Age (years)" %in% out$Variable)
  expect_false("age" %in% out$Variable)
})

test_that("flexsurvreg accepts `labels =` keyed on a factor term", {
  # The factor header is the one label a coefficient key cannot reach.
  # flexsurvreg carries its terms on the model.frame, not the fit: the
  # validator must read them through `.spicy_get_terms()` -- the same
  # helper the renderer uses to detect the header it displays --
  # or the displayed `sexf:` header is unreachable by any key.
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")
  d <- survival::lung
  d$sexf <- factor(d$sex, levels = c(1, 2), labels = c("Male", "Female"))
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age + sexf,
    data = d,
    dist = "weibull"
  )
  out <- table_regression(fit, labels = c(sexf = "Sex"))
  expect_true("Sex:" %in% out$Variable)
  expect_false("sexf:" %in% out$Variable)
})

test_that("selection accepts `labels =` keyed on coefficient names", {
  fit <- .fit_selection_heckman()
  out <- table_regression(fit, labels = c(educ = "Years of education"))
  expect_true(any(grepl("Years of education", out$Variable, fixed = TRUE)))
})

test_that("the intercepts of the two Heckman equations are intercepts", {
  fit <- .fit_selection_heckman()
  # The engine's names: one "(Intercept)" per equation.
  expect_identical(sum(names(stats::coef(fit)) == "(Intercept)"), 2L)
  ints <- c("selection: (Intercept)", "outcome: (Intercept)")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$coefs$term[fr$coefs$is_intercept], ints)
  shown <- function(...) trimws(table_regression(...)$Variable)
  # Alone, one row across the two equation groups.
  expect_identical(shown(fit)[[1L]], "(Intercept)")
  # show_intercept = FALSE hides them, alone and beside another model.
  expect_false(any(grepl(
    "(Intercept)",
    shown(fit, show_intercept = FALSE),
    fixed = TRUE
  )))
  data("Mroz87", package = "sampleSelection", envir = environment())
  ols <- stats::lm(wage ~ exper + educ, data = Mroz87)
  expect_false(any(grepl(
    "(Intercept)",
    shown(list(ols, fit), show_intercept = FALSE),
    fixed = TRUE
  )))
  # keep / drop leave them to show_intercept, as they do the intercept
  # of lm().
  expect_identical(
    shown(fit, keep = "educ")[1:2],
    c("(Intercept)", "educ")
  )
  expect_identical(
    shown(list(fit, fit), keep = "educ")[1:3],
    c("(Intercept):", "selection", "outcome")
  )
  expect_true("(Intercept)" %in% shown(ols, keep = "educ"))
})

test_that("p_adjust leaves the intercepts of the two equations out", {
  skip_if_not_installed("broom")
  skip_if_not_installed("nnet")
  fit <- .fit_selection_heckman()
  # Hand count: the slopes of both equations, without the two
  # intercepts and the sigma and rho of the model.
  nm <- names(stats::coef(fit))
  n_slopes <- sum(!nm %in% c("(Intercept)", "invMillsRatio", "sigma", "rho"))
  expect_identical(n_slopes, 7L)
  tab <- table_regression(fit, p_adjust = "holm")
  expect_match(
    attr(tab, "note"),
    sprintf("P-values adjusted for %d tests (Holm method).", n_slopes),
    fixed = TRUE
  )
  # The displayed slope p-values are Holm over those slopes alone, the
  # engine's own p-values (two-step fit, whose standard errors are finite).
  data("Mroz87", package = "sampleSelection", envir = environment())
  fit2 <- sampleSelection::selection(
    lfp ~ age + faminc + kids5 + educ,
    wage ~ exper + educ + city,
    data = Mroz87,
    method = "2step"
  )
  est <- summary(fit2)$estimate
  n_sel <- length(fit2$param$index$betaS)
  n_out <- length(fit2$param$index$betaO)
  keys <- c(
    paste0("selection: ", rownames(est)[seq_len(n_sel)]),
    paste0("outcome: ", rownames(est)[n_sel + seq_len(n_out)])
  )
  p_raw <- est[seq_len(n_sel + n_out), "Pr(>|t|)"]
  slope <- !grepl("(Intercept)", keys, fixed = TRUE)
  td <- as.data.frame(broom::tidy(table_regression(fit2, p_adjust = "holm")))
  expect_equal(
    td$p.value[match(keys[slope], td$term)],
    unname(stats::p.adjust(p_raw[slope], "holm")),
    tolerance = 1e-12
  )
  # A multinomial model follows the same rule.
  m <- nnet::multinom(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    trace = FALSE
  )
  expect_match(
    attr(table_regression(m, p_adjust = "holm"), "note"),
    sprintf(
      "P-values adjusted for %d tests (Holm method).",
      sum(colnames(stats::coef(m)) != "(Intercept)") * nrow(stats::coef(m))
    ),
    fixed = TRUE
  )
})


# ---- A selection model alone: its equations as column groups -------------

# A logical, a two-level and a three-level factor, the fit made inside a
# function: its data are not in the global environment.
.fit_selection_factors <- function(two_step = FALSE) {
  skip_if_not_installed("sampleSelection")
  data("Mroz87", package = "sampleSelection", envir = environment())
  Mroz87$kids <- Mroz87$kids5 + Mroz87$kids618 > 0
  Mroz87$city <- factor(Mroz87$city, labels = c("no", "yes"))
  Mroz87$region <- factor(
    rep(c("north", "south", "west"), length.out = nrow(Mroz87))
  )
  f <- if (two_step) sampleSelection::heckit else sampleSelection::selection
  f(
    lfp ~ age + kids + educ + region,
    wage ~ exper + educ + city,
    data = Mroz87
  )
}

test_that("a selection model alone sets its equations as column groups", {
  skip_if_not_installed("broom")
  for (two_step in c(FALSE, TRUE)) {
    fit <- .fit_selection_factors(two_step)
    tb <- table_regression(fit)
    expect_identical(names(attr(tb, "spanners")), c("Outcome", "Selection"))
    expect_identical(
      names(tb),
      c(
        "Variable",
        paste0("Outcome: ", c("B", "SE", "p")),
        paste0("Selection: ", c("B", "SE", "p"))
      )
    )
    # The predictors of the outcome equation first, then those of the
    # selection equation alone; the error terms, n and AIC once.
    rows <- trimws(tb$Variable)
    expect_identical(
      rows[1:14],
      c(
        "(Intercept)",
        "exper",
        "educ",
        "city:",
        "no (ref.)",
        "yes",
        "age",
        "kids:",
        "FALSE (ref.)",
        "TRUE",
        "region:",
        "north (ref.)",
        "south",
        "west"
      )
    )
    expect_identical(rows[[15]], "Error terms:")
    expect_identical(sum(rows == "n"), 1L)
    # Each cell is the estimate tidy() gives, in its equation's column;
    # a term out of an equation has an empty cell there.
    df <- table_regression(fit, output = "data.frame")
    td <- as.data.frame(broom::tidy(tb))
    for (eq in c("outcome", "selection")) {
      col <- paste0(capitalize_first(eq), ": B")
      sub <- td[td$outcome_level %in% eq, ]
      key <- sub(paste0("^", eq, ": "), "", sub$term)
      key_df <- ifelse(
        df$.row_role == "level",
        paste0(df$variable, df$level),
        df$variable
      )
      expect_equal(df[[col]][match(key, key_df)], sub$estimate)
    }
    expect_true(is.na(df[["Selection: B"]][df$variable == "exper"]))
    expect_true(is.na(df[["Outcome: B"]][df$variable == "age"]))
    # The error terms sit under the outcome group only.
    err <- df$variable == "Error terms" & df$.row_role == "level"
    expect_true(all(!is.na(df[["Outcome: B"]][err])))
    expect_true(all(is.na(df[["Selection: B"]][err])))
  }
})

test_that("the reference rows of a selection frame follow lm()", {
  fit <- .fit_selection_factors(two_step = TRUE)
  cf <- as_regression_frame(fit)$coefs
  ref <- cf[cf$is_ref, ]
  expect_setequal(
    ref$term,
    c("selection: kidsFALSE", "selection: regionnorth", "outcome: cityno")
  )
  expect_true(all(ref$factor_level_pos == 1L))
  lev <- cf[cf$term == "selection: regionwest", ]
  expect_identical(lev$parent_var, "region")
  expect_identical(lev$label, "selection: west")
  expect_identical(lev$factor_level_pos, 3L)
  # A design that does not give the coefficient names back keeps them.
  expect_null(.selection_design(fit, "selection", c("(Intercept)", "x")))
})

test_that("several selection models keep the equations as rows", {
  fit <- .fit_selection_factors()
  two <- .fit_selection_factors(two_step = TRUE)
  rows <- trimws(table_regression(list(ML = fit, "Two-step" = two))$Variable)
  expect_identical(
    rows[which(rows == "region:") + 1:3],
    c("selection: north (ref.)", "selection: south", "selection: west")
  )
  expect_identical(
    rows[which(rows == "educ:") + 1:2],
    c("selection", "outcome")
  )
  # nested = TRUE keeps the rows too.
  data("Mroz87", package = "sampleSelection", envir = environment())
  m0 <- sampleSelection::selection(
    lfp ~ age + educ,
    wage ~ exper,
    data = Mroz87
  )
  m1 <- update(m0, outcome = wage ~ exper + educ)
  rows <- trimws(table_regression(list(m0, m1), nested = TRUE)$Variable)
  expect_true(all(c("selection", "outcome") %in% rows))
})

test_that("labels, keep and outcome_labels on a selection model alone", {
  fit <- .fit_selection_factors()
  tb <- expect_no_warning(table_regression(
    fit,
    labels = c(city = "Lives in a city", cityyes = "Yes", educ = "Education")
  ))
  rows <- trimws(tb$Variable)
  expect_true(all(c("Lives in a city:", "Yes", "Education") %in% rows))
  # keep matches the terms without their equation prefix.
  rows <- trimws(table_regression(fit, keep = "^educ$")$Variable)
  expect_identical(rows[1:2], c("(Intercept)", "educ"))
  tb <- table_regression(fit, outcome_labels = c("Wage", "Participation"))
  expect_identical(names(attr(tb, "spanners")), c("Wage", "Participation"))
  expect_error(
    table_regression(fit, model_labels = "Heckman"),
    class = "spicy_invalid_input"
  )
})

test_that("inline() selects the equation of a selection model alone by model", {
  fit <- .fit_selection_factors()
  tb <- table_regression(fit)
  b <- summary(fit)$estimate
  out <- grep("^educ$", rownames(b))
  expect_identical(
    inline(tb, educ, model = "Outcome"),
    sprintf("%.2f", b[out[2L], "Estimate"])
  )
  expect_identical(
    inline(tb, educ, model = 2),
    sprintf("%.2f", b[out[1L], "Estimate"])
  )
  expect_identical(
    inline(tb, city, "yes", model = "Outcome"),
    sprintf("%.2f", b["cityyes", "Estimate"])
  )
  err <- expect_error(
    inline(tb, educ, level = "outcome"),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "`model = \"Outcome\"`", fixed = TRUE)
  # Beside another model, the equation is a level.
  tbm <- table_regression(list(A = fit, B = fit))
  expect_identical(
    inline(tbm, educ, "outcome", model = "A"),
    sprintf("%.2f", b[out[2L], "Estimate"])
  )
})

test_that("a selection model alone in French and in a stack", {
  fit <- .fit_selection_factors()
  withr::with_options(list(spicy.language = "fr"), {
    tb <- table_regression(fit)
    expect_identical(
      names(attr(tb, "spanners")),
      c("R\u00e9sultat", "S\u00e9lection")
    )
    expect_identical(
      inline(tb, educ, model = "R\u00e9sultat"),
      inline(tb, educ, model = 1)
    )
  })
  data("Mroz87", package = "sampleSelection", envir = environment())
  ols <- stats::lm(wage ~ exper + educ, data = Mroz87, subset = lfp == 1)
  st <- table_stack(
    Heckman = table_regression(fit),
    OLS = table_regression(ols)
  )
  out <- utils::capture.output(print(st))
  expect_true(any(grepl("Outcome", out[1:3], fixed = TRUE)))
  expect_true(any(grepl("Selection", out[1:3], fixed = TRUE)))
})

test_that("inline() reaches a model by its identity, whatever the language", {
  fit <- .fit_selection_factors()
  withr::with_options(list(spicy.language = "fr"), {
    tb <- table_regression(fit)
    expect_identical(
      inline(tb, educ, model = "Outcome"),
      inline(tb, educ, model = 1)
    )
    expect_identical(
      inline(tb, educ, model = "outcome"),
      inline(tb, educ, model = 1)
    )
    expect_identical(
      inline(tb, educ, model = "selection"),
      inline(tb, educ, model = 2)
    )
  })
  # The displayed label wins over an identity: "M2" is the label of the
  # first model here.
  tm <- table_regression(list(
    M2 = stats::lm(mpg ~ wt, data = mtcars),
    stats::lm(mpg ~ wt + hp, data = mtcars)
  ))
  expect_identical(inline(tm, wt, model = "M2"), inline(tm, wt, model = 1))
  expect_identical(inline(tm, wt, model = "M1"), inline(tm, wt, model = 1))
  expect_error(inline(tm, wt, model = "m2x"), class = "spicy_invalid_input")
})

test_that("outcome_labels of the wrong length names the equations", {
  fit <- .fit_selection_factors()
  for (bad in list("Wage", 1:2)) {
    err <- expect_error(
      table_regression(fit, outcome_labels = bad),
      class = "spicy_invalid_input"
    )
    expect_match(
      conditionMessage(err),
      "one label per equation for a selection model alone",
      fixed = TRUE
    )
    expect_no_match(conditionMessage(err), "multinomial", fixed = TRUE)
  }
})


# ---- A selection model beside a model of another class --------------------

.fit_selection_ols <- function() {
  data("Mroz87", package = "sampleSelection", envir = environment())
  Mroz87$city <- factor(Mroz87$city, labels = c("no", "yes"))
  stats::lm(wage ~ exper + educ + city, data = Mroz87, subset = lfp == 1)
}

test_that("beside another class, a selection model shows its equations as groups", {
  skip_if_not_installed("broom")
  fit <- .fit_selection_factors()
  ols <- .fit_selection_ols()
  tb <- table_regression(list(OLS = ols, Heckman = fit))
  groups <- c("OLS", "Heckman: Outcome", "Heckman: Selection")
  expect_identical(names(attr(tb, "spanners")), groups)
  expect_identical(
    names(tb)[-1L],
    paste0(rep(groups, each = 3L), ": ", c("B", "SE", "p"))
  )
  # The rows align by term: educ of the lm and of the outcome equation on
  # one row, each cell the estimate tidy() gives.
  df <- table_regression(list(OLS = ols, Heckman = fit), output = "data.frame")
  td <- as.data.frame(broom::tidy(tb))
  educ <- df[df$variable == "educ", ]
  expect_identical(nrow(educ), 1L)
  b <- function(id, term) td$estimate[td$model_id == id & td$term == term]
  expect_equal(educ[["OLS: B"]], b("OLS", "educ"))
  expect_equal(educ[["Heckman: Outcome: B"]], b("Heckman", "outcome: educ"))
  expect_equal(
    educ[["Heckman: Selection: B"]],
    b("Heckman", "selection: educ")
  )
  expect_true(is.na(df[["OLS: B"]][df$variable == "age"]))
  # tidy() keeps one model per fit and the prefixed terms.
  expect_identical(unique(td$model_id), c("OLS", "Heckman"))
  expect_true("selection: age" %in% td$term)
  # The rows of the whole model, under its first group only.
  err <- df$variable == "Error terms" & df$.row_role == "level"
  expect_true(all(!is.na(df[["Heckman: Outcome: B"]][err])))
  expect_true(all(is.na(df[["Heckman: Selection: B"]][err])))
  n_row <- tb[trimws(tb$Variable) == "n", ]
  expect_identical(trimws(n_row[["Heckman: Outcome: B"]]), "753")
  expect_identical(trimws(n_row[["Heckman: Selection: B"]]), "")
  # The note names the model, never a group.
  note <- attr(tb, "note")
  expect_match(note, "Heckman: Outcome observed for 428 of 753", fixed = TRUE)
  expect_no_match(note, "Heckman: Selection", fixed = TRUE)
})

test_that("default labels, the Outcome row and keep beside a selection model", {
  fit <- .fit_selection_factors()
  ols <- .fit_selection_ols()
  data("Mroz87", package = "sampleSelection", envir = environment())
  pr <- stats::glm(
    lfp ~ age + educ,
    data = Mroz87,
    family = stats::binomial("probit")
  )
  tb <- table_regression(list(ols, fit))
  expect_identical(
    names(attr(tb, "spanners")),
    c("Model 1", "Model 2: Outcome", "Model 2: Selection")
  )
  # The models do not share an outcome: the group of the selection
  # equation has the selection variable.
  outcome_row <- function(tb) {
    out <- trimws(unlist(tb[trimws(tb$Variable) == "Outcome", -1L]))
    unname(out[nzchar(out)])
  }
  tb <- table_regression(list(OLS = ols, Probit = pr, Heckman = fit))
  expect_identical(outcome_row(tb), c("wage", "lfp", "wage", "lfp"))
  tb <- table_regression(
    list(OLS = ols, Probit = pr, Heckman = fit),
    outcome_labels = c("Wage", "Participation", "Wage")
  )
  expect_identical(
    outcome_row(tb),
    c("Wage", "Participation", "Wage", "lfp")
  )
  # keep and labels read the bare terms.
  tb <- expect_no_warning(table_regression(
    list(OLS = ols, Heckman = fit),
    keep = c("^educ$", "^age$"),
    labels = c(educ = "Education")
  ))
  expect_identical(
    trimws(tb$Variable)[1:3],
    c("(Intercept)", "Education", "age")
  )
  long <- table_regression(
    list(OLS = ols, Heckman = fit),
    keep = "^age$",
    show_intercept = FALSE,
    output = "long"
  )
  expect_identical(unique(long$term), "selection: age")
})

test_that("selection models alone keep the rows; beside an lm, groups each", {
  fit <- .fit_selection_factors()
  two <- .fit_selection_factors(two_step = TRUE)
  tb <- table_regression(list(ML = fit, "Two-step" = two))
  expect_identical(names(attr(tb, "spanners")), c("ML", "Two-step"))
  tb <- table_regression(
    list(OLS = .fit_selection_ols(), ML = fit, "Two-step" = two)
  )
  expect_identical(
    names(attr(tb, "spanners")),
    c(
      "OLS",
      "ML: Outcome",
      "ML: Selection",
      "Two-step: Outcome",
      "Two-step: Selection"
    )
  )
  rows <- trimws(tb$Variable)
  expect_true("Error terms:" %in% rows)
  expect_false(any(grepl("^(selection|outcome)", rows)))
})

test_that("inline() picks a group beside another model by label, position or identity", {
  fit <- .fit_selection_factors()
  tb <- table_regression(list(OLS = .fit_selection_ols(), Heckman = fit))
  b <- summary(fit)$estimate
  sel <- sprintf("%.2f", b[grep("^educ$", rownames(b))[1L], "Estimate"])
  expect_identical(inline(tb, educ, model = "Heckman: Selection"), sel)
  expect_identical(inline(tb, educ, model = 3), sel)
  expect_identical(inline(tb, educ, model = "heckman: selection"), sel)
  expect_identical(
    inline(tb, city, "yes", model = "Heckman: Outcome"),
    sprintf("%.2f", b["cityyes", "Estimate"])
  )
  withr::with_options(list(spicy.language = "fr"), {
    tbf <- table_regression(list(OLS = .fit_selection_ols(), Heckman = fit))
    expect_identical(
      names(attr(tbf, "spanners"))[2:3],
      c("Heckman\u00a0: R\u00e9sultat", "Heckman\u00a0: S\u00e9lection")
    )
    # The identity does not depend on the language.
    expect_identical(
      inline(tbf, educ, model = "Heckman: selection"),
      inline(tbf, educ, model = names(attr(tbf, "spanners"))[[3L]])
    )
  })
})

test_that("Excel writes the groups of a selection model beside another model", {
  skip_if_not_installed("openxlsx2")
  fit <- .fit_selection_factors()
  fits <- list(OLS = .fit_selection_ols(), Heckman = fit)
  path <- withr::local_tempfile(fileext = ".xlsx")
  table_regression(fits, output = "excel", excel_path = path)
  x <- openxlsx2::read_xlsx(path, col_names = FALSE, skip_empty_rows = FALSE)
  head_row <- unlist(x[3L, ])
  expect_true(all(
    c("OLS", "Heckman: Outcome", "Heckman: Selection") %in% head_row
  ))
  educ <- unlist(x[which(x[[1L]] == "educ"), ])
  df <- table_regression(fits, output = "data.frame")
  expect_equal(
    as.numeric(educ[which(head_row == "Heckman: Selection")[1L]]),
    df[["Heckman: Selection: B"]][df$variable == "educ"]
  )
})

test_that("the selection variable of the Outcome row is named as any outcome", {
  fit <- .fit_selection_factors()
  ols <- .fit_selection_ols()
  data("Mroz87", package = "sampleSelection", envir = environment())
  po <- stats::glm(kids5 ~ educ, data = Mroz87, family = stats::poisson)
  outcome_row <- function(tb) {
    out <- trimws(unlist(tb[trimws(tb$Variable) == "Outcome", -1L]))
    unname(out[nzchar(out)])
  }
  # A named vector, without the warning of a name that matches nothing.
  tb <- expect_no_warning(table_regression(
    list(OLS = ols, Poisson = po, Heckman = fit),
    labels = c(lfp = "Participation")
  ))
  expect_identical(
    outcome_row(tb),
    c("wage", "kids5", "wage", "Participation")
  )
  # outcome_labels names the models; labels the selection variable.
  tb <- table_regression(
    list(OLS = ols, Poisson = po, Heckman = fit),
    outcome_labels = c("Wage", "Children", "Wage"),
    labels = c(lfp = "Participation")
  )
  expect_identical(
    outcome_row(tb),
    c("Wage", "Children", "Wage", "Participation")
  )
  # The label attribute, on the attribute base only.
  attr(Mroz87$lfp, "label") <- "Labour force participation"
  sel <- sampleSelection::selection(
    lfp ~ age + educ,
    wage ~ exper + educ,
    data = Mroz87
  )
  fits <- list(OLS = ols, Poisson = po, Heckman = sel)
  expect_identical(
    outcome_row(table_regression(fits, labels = TRUE))[[4L]],
    "Labour force participation"
  )
  expect_identical(outcome_row(table_regression(fits))[[4L]], "lfp")
})

test_that("inline() gives the group of a model shown as two equation groups", {
  fit <- .fit_selection_factors()
  tb <- table_regression(list(OLS = .fit_selection_ols(), Heckman = fit))
  err <- expect_error(
    inline(tb, educ, "outcome", model = "Heckman"),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "select one with `model = \"Heckman: Outcome\"`.",
    fixed = TRUE
  )
  err <- expect_error(
    inline(tb, educ, model = "Heckman"),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "`model = \"Heckman: Outcome\"` or `model = \"Heckman: Selection\"`",
    fixed = TRUE
  )
  err <- expect_error(
    inline(tb, educ, model = "Probit"),
    class = "spicy_invalid_input"
  )
  expect_no_match(conditionMessage(err), "column groups", fixed = TRUE)
})
