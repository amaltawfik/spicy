# The table of a sample-selection model (sampleSelection::selection(),
# heckit(), treatReg()): its error terms, its estimation method, its
# reference distribution, and the fits it refuses.
# dev/decisions/2026-11-19-selection-error-terms.md.

.mroz <- function() {
  skip_if_not_installed("sampleSelection")
  e <- new.env()
  utils::data("Mroz87", package = "sampleSelection", envir = e)
  d <- e$Mroz87
  d$kids <- d$kids5 + d$kids618 > 0
  d
}

.sel_ml <- function(d = .mroz()) {
  sampleSelection::selection(
    lfp ~ age + I(age^2) + faminc + kids + educ,
    wage ~ exper + I(exper^2) + educ + city,
    data = d
  )
}

.sel_2s <- function(d = .mroz()) {
  sampleSelection::heckit(
    lfp ~ age + I(age^2) + faminc + kids + educ,
    wage ~ exper + I(exper^2) + educ + city,
    data = d
  )
}

test_that("a tobit-5 fit is refused by name", {
  skip_if_not_installed("sampleSelection")
  skip_if_not_installed("MASS")
  set.seed(1)
  n <- 300
  d <- data.frame(z = rnorm(n), x1 = rnorm(n), x2 = rnorm(n))
  e <- MASS::mvrnorm(
    n,
    c(0, 0, 0),
    matrix(c(1, -0.5, 0.5, -0.5, 1, 0, 0.5, 0, 1), 3)
  )
  d$ys <- d$z + e[, 1] > 0
  d$y1 <- d$x1 + e[, 2]
  d$y2 <- d$x2 + e[, 3]
  t5 <- sampleSelection::selection(ys ~ z, list(y1 ~ x1, y2 ~ x2), data = d)
  err <- expect_error(table_regression(t5), class = "spicy_unsupported_class")
  expect_match(conditionMessage(err), "A tobit-5 `selection` fit", fixed = TRUE)
  expect_error(as_regression_frame(t5), class = "spicy_unsupported_class")
})

# The rows of the table, by term, from the tidy view.
.sel_tidy <- function(tab) {
  skip_if_not_installed("broom")
  as.data.frame(broom::tidy(tab))
}

.sel_note <- function(tab) attr(tab, "note")

# The Greek letters and accented words, kept out of the ASCII source.
.u <- function(...) intToUtf8(c(...))
.sig <- .u(0x03C3)
.rho <- .u(0x03C1)
.lam <- .u(0x03BB)
.e_acute <- .u(0x00E9)
.a_grave <- .u(0x00E0)

test_that("the ML error terms equal summary() and sigma carries no test", {
  fit <- .sel_ml()
  est <- summary(fit)$estimate
  df <- fit$param$df
  td <- .sel_tidy(table_regression(fit))
  r <- td[match(c("Error terms::sigma", "Error terms::rho"), td$term), ]
  expect_equal(r$estimate, unname(est[c("sigma", "rho"), 1]), tolerance = 1e-10)
  expect_equal(
    r$std.error,
    unname(est[c("sigma", "rho"), 2]),
    tolerance = 1e-10
  )
  expect_true(is.na(r$p.value[1L]))
  expect_equal(r$p.value[2L], unname(est["rho", 4]), tolerance = 1e-10)
  q <- stats::qt(0.975, df)
  expect_equal(r$conf.low, r$estimate - q * r$std.error, tolerance = 1e-10)
  expect_equal(r$conf.high, r$estimate + q * r$std.error, tolerance = 1e-10)
  # Greene (2003), example 22.8, as sampleSelection reproduces it.
  expect_equal(r$estimate, c(3.1084, -0.1320), tolerance = 1e-4)
  expect_identical(unique(r$factor_term), "Error terms")
})

test_that("the two-step error terms equal summary(), sigma and rho without SE", {
  fit <- .sel_2s()
  est <- summary(fit)$estimate
  td <- .sel_tidy(table_regression(fit))
  keys <- paste0("Error terms::", c("invMillsRatio", "sigma", "rho"))
  r <- td[match(keys, td$term), ]
  expect_equal(
    r$estimate,
    unname(est[c("invMillsRatio", "sigma", "rho"), 1]),
    tolerance = 1e-10
  )
  expect_equal(
    r$std.error[1L],
    unname(est["invMillsRatio", 2]),
    tolerance = 1e-10
  )
  expect_equal(
    r$p.value[1L],
    unname(est["invMillsRatio", 4]),
    tolerance = 1e-10
  )
  expect_true(all(is.na(r$std.error[2:3])))
  expect_true(all(is.na(r$conf.low[2:3])))
  expect_true(all(is.na(r$p.value[2:3])))
  expect_equal(r$estimate, c(-1.0976, 3.2001, -0.3430), tolerance = 1e-4)
})

test_that("every row tests on summary()'s t with the residual df", {
  for (fit in list(.sel_ml(), .sel_2s())) {
    est <- summary(fit)$estimate
    fr <- as_regression_frame(fit)
    expect_true(all(fr$coefs$test_type == "t"))
    expect_true(all(fr$coefs$df == fit$param$df))
    n <- nrow(fr$coefs)
    expect_equal(
      fr$coefs$p_value,
      unname(est[seq_len(n), "Pr(>|t|)"]),
      tolerance = 1e-10
    )
    q <- stats::qt(0.975, fit$param$df)
    expect_equal(
      fr$coefs$ci_lower,
      unname(est[seq_len(n), 1] - q * est[seq_len(n), 2]),
      tolerance = 1e-10
    )
    expect_match(
      .sel_note(table_regression(fit)),
      "P-values and CIs: t distribution with 740 df.",
      fixed = TRUE
    )
  }
  # The p of age is summary()'s t on 740 df (z would give 0.00521).
  td <- .sel_tidy(table_regression(.sel_ml()))
  expect_equal(
    td$p.value[td$term == "selection: age"],
    0.005345,
    tolerance = 1e-3
  )
})

test_that("the note names the standard errors of each estimation method", {
  ml <- .sel_ml()
  ts <- .sel_2s()
  expect_match(
    .sel_note(table_regression(ml)),
    "Std. errors: classical (Fisher information).",
    fixed = TRUE
  )
  expect_match(
    .sel_note(table_regression(list(ts, ml))),
    paste0(
      "Std. errors: Model 1: classical (two-step), ",
      "Model 2: classical (Fisher information)."
    ),
    fixed = TRUE
  )
})

test_that("the note gives the outcome cases and defines the error terms", {
  note <- .sel_note(table_regression(.sel_ml()))
  expect_match(note, "Outcome observed for 428 of 753.", fixed = TRUE)
  expect_match(
    note,
    paste0(
      "Error terms: ",
      .sig,
      ", standard deviation of the outcome error; ",
      .rho,
      ", correlation of the selection and outcome errors."
    ),
    fixed = TRUE
  )
  note2 <- .sel_note(table_regression(.sel_2s()))
  expect_match(
    note2,
    paste0(
      .lam,
      ", coefficient of the inverse Mills ratio, an estimate of ",
      .rho,
      .sig
    ),
    fixed = TRUE
  )
  expect_match(
    note2,
    paste0(
      "In a two-step fit, ",
      .sig,
      " and ",
      .rho,
      " are derived from the estimates and have no standard error."
    ),
    fixed = TRUE
  )
})

test_that("the block is drawn under its heading, the symbol first", {
  out <- capture.output(print(table_regression(.sel_2s())))
  expect_true(any(grepl("^ Error terms:", out)))
  for (lab in c(
    paste(.lam, "(inverse Mills ratio)"),
    paste(.sig, "(outcome error)"),
    paste(.rho, "(error correlation)")
  )) {
    expect_true(any(grepl(lab, out, fixed = TRUE)), info = lab)
  }
})

test_that("keep and drop select the error terms by their coefficient name", {
  fit <- .sel_ml()
  tab <- table_regression(fit, keep = c("educ", "^rho$"))
  td <- .sel_tidy(tab)
  expect_true("Error terms::rho" %in% td$term)
  expect_false("Error terms::sigma" %in% td$term)
  # The note defines the rows left, and only them.
  expect_false(grepl("standard deviation of the outcome error", .sel_note(tab)))
  expect_match(.sel_note(tab), "correlation of the selection", fixed = TRUE)
  tab <- table_regression(fit, drop = "^(sigma|rho)$")
  expect_false(any(.sel_tidy(tab)$factor_term %in% "Error terms"))
  expect_false(grepl("Error terms:", .sel_note(tab), fixed = TRUE))
  # A predictor pattern leaves them out, as it leaves the other slopes.
  td <- .sel_tidy(table_regression(fit, keep = "educ"))
  expect_false(any(td$factor_term %in% "Error terms"))
  # A pattern on a coefficient name is no dead pattern.
  expect_no_warning(table_regression(fit, keep = "^invMillsRatio$|^rho$"))
})

test_that("show_components = FALSE leaves the block and its definitions out", {
  tab <- table_regression(.sel_ml(), show_components = FALSE)
  expect_false(any(.sel_tidy(tab)$factor_term %in% "Error terms"))
  expect_false(grepl("Error terms:", .sel_note(tab), fixed = TRUE))
})

test_that("the error terms stay out of the p-adjust family", {
  fit <- .sel_ml()
  est <- summary(fit)$estimate
  tab <- table_regression(fit, p_adjust = "holm")
  td <- .sel_tidy(tab)
  expect_equal(
    td$p.value[td$term == "Error terms::rho"],
    unname(est["rho", 4]),
    tolerance = 1e-12
  )
  # The nine slopes of the two equations, without sigma and rho.
  expect_match(.sel_note(tab), "P-values adjusted for 9 tests", fixed = TRUE)
})

test_that("an OLS fit on the selected cases sits beside the selection model", {
  d <- .mroz()
  fit <- .sel_ml(d)
  ols <- stats::lm(
    wage ~ exper + I(exper^2) + educ + city,
    data = d,
    subset = lfp == 1
  )
  tab <- table_regression(list(OLS = ols, ML = fit))
  note <- .sel_note(tab)
  # The t line holds for the selection model only, and says so.
  expect_match(
    note,
    "ML: P-values and CIs: t distribution with 740 df.",
    fixed = TRUE
  )
  expect_match(note, "ML: Outcome observed for 428 of 753.", fixed = TRUE)
  expect_equal(sum(.sel_tidy(tab)$factor_term %in% "Error terms"), 2L)
})

test_that("the title names a binary outcome and a treatment-effects model", {
  d <- .mroz()
  mb <- sampleSelection::selection(
    lfp ~ educ + age + kids5 + kids618 + nwifeinc,
    wage >= 5 ~ educ,
    data = d
  )
  expect_identical(
    as_regression_frame(mb)$info$extras$title_prefix,
    "Heckman selection model (binary outcome)"
  )
  td <- .sel_tidy(table_regression(mb))
  expect_identical(
    td$term[td$factor_term %in% "Error terms"],
    "Error terms::rho"
  )
  expect_equal(
    td$estimate[td$term == "Error terms::rho"],
    unname(summary(mb)$estimate["rho", 1]),
    tolerance = 1e-10
  )

  tr <- sampleSelection::treatReg(
    lfp ~ age + I(age^2) + faminc + kids + educ,
    log(faminc) ~ lfp + educ + age,
    data = d
  )
  fr <- as_regression_frame(tr)
  expect_identical(fr$info$extras$title_prefix, "Treatment-effects model")
  # Its rows are summary()'s, equations and error terms.
  est <- summary(tr)$estimate
  n <- nrow(fr$coefs)
  expect_equal(fr$coefs$estimate, unname(est[seq_len(n), 1]), tolerance = 1e-10)
  expect_equal(
    fr$coefs$std_error,
    unname(est[seq_len(n), 2]),
    tolerance = 1e-10
  )
  td <- .sel_tidy(table_regression(tr))
  r <- td[match(c("Error terms::sigma", "Error terms::rho"), td$term), ]
  expect_equal(r$estimate, unname(est[c("sigma", "rho"), 1]), tolerance = 1e-10)
  expect_equal(
    r$std.error,
    unname(est[c("sigma", "rho"), 2]),
    tolerance = 1e-10
  )
  # Every case is in the outcome equation of a treatment-effects model.
  expect_false(grepl("Outcome observed", .sel_note(table_regression(tr))))
})

test_that("the error terms reach every output", {
  fit <- .sel_2s()
  tab <- table_regression(fit)
  hit <- function(x) any(grepl("inverse Mills ratio", unlist(x), fixed = TRUE))
  expect_true(hit(table_regression(fit, output = "data.frame")))
  long <- table_regression(fit, output = "long")
  expect_true("Error terms::invMillsRatio" %in% long$term)
  expect_true(hit(as_structured(tab)$body))
  skip_if_not_installed("gt")
  expect_true(hit(as.character(gt::as_raw_html(
    table_regression(fit, output = "gt")
  ))))
  skip_if_not_installed("tinytable")
  expect_true(hit(table_regression(fit, output = "tinytable")@data))
  skip_if_not_installed("flextable")
  expect_true(hit(table_regression(fit, output = "flextable")$body$dataset))
  skip_if_not_installed("openxlsx2")
  xl <- withr::local_tempfile(fileext = ".xlsx")
  table_regression(fit, output = "excel", excel_path = xl)
  expect_true(hit(openxlsx2::read_xlsx(xl, col_names = FALSE)))
})

test_that("the French table translates the block, its rows and its notes", {
  withr::local_options(spicy.language = "fr")
  tab <- table_regression(.sel_2s())
  out <- capture.output(print(tab))
  expect_true(any(grepl("Termes d'erreur", out, fixed = TRUE)))
  expect_true(any(grepl(
    paste(.lam, "(ratio de Mills inverse)"),
    out,
    fixed = TRUE
  )))
  note <- .sel_note(tab)
  expect_match(
    note,
    paste0("R", .e_acute, "sultat observ", .e_acute, " pour 428 sur 753."),
    fixed = TRUE
  )
  expect_match(note, paste0("deux ", .e_acute, "tapes"), fixed = TRUE)
  expect_match(note, paste0("loi t ", .a_grave, " 740 degr"), fixed = TRUE)
})

test_that("nested = TRUE compares two ML selection models by their likelihoods", {
  d <- .mroz()
  big <- .sel_ml(d)
  small <- sampleSelection::selection(
    lfp ~ age + I(age^2) + faminc + kids + educ,
    wage ~ exper + I(exper^2) + educ,
    data = d
  )
  lr <- 2 * (as.numeric(stats::logLik(big)) - as.numeric(stats::logLik(small)))
  expect_equal(lr, 1.993, tolerance = 1e-3)
  cmp <- compute_nested_comparisons(list(small, big))
  expect_equal(cmp$lrt_change, lr, tolerance = 1e-10)
  expect_equal(cmp$change_df1, 1)
  expect_equal(
    cmp$p_change,
    stats::pchisq(lr, 1, lower.tail = FALSE),
    tolerance = 1e-10
  )
  expect_equal(cmp$p_change, 0.158, tolerance = 1e-3)
  out <- capture.output(print(table_regression(
    list(small, big),
    nested = TRUE
  )))
  expect_true(any(grepl("1.99", out, fixed = TRUE)))
  expect_true(any(grepl(".158", out, fixed = TRUE)))
})

test_that("nested = TRUE refuses a two-step fit and a mix of classes", {
  d <- .mroz()
  err <- expect_error(
    table_regression(list(.sel_2s(d), .sel_ml(d)), nested = TRUE),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "two-step selection model", fixed = TRUE)
  m0 <- stats::lm(wage ~ educ, data = d)
  err <- expect_error(
    table_regression(list(m0, .sel_ml(d)), nested = TRUE),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "cannot mix a selection model",
    fixed = TRUE
  )
})

test_that("exponentiate = TRUE on a selection model says why it does nothing", {
  fit <- .sel_ml()
  w <- expect_warning(
    table_regression(fit, exponentiate = TRUE),
    class = "spicy_ignored_arg"
  )
  msg <- conditionMessage(w)
  expect_match(msg, "no effect on a sample-selection model", fixed = TRUE)
  expect_match(msg, "probit", fixed = TRUE)
  expect_false(grepl("identity-link", msg, fixed = TRUE))
  # The coefficients are shown on their own scale.
  td0 <- .sel_tidy(table_regression(fit))
  td1 <- .sel_tidy(suppressWarnings(table_regression(fit, exponentiate = TRUE)))
  expect_equal(td1$estimate, td0$estimate, tolerance = 1e-12)
})
