# table_regression(fallback = ): the rows of a class without a table of
# its own, read from broom::tidy() or parameters::model_parameters().

# An invented class, its tidy(), model_parameters() and nobs() methods
# registered for the duration of the test: `rows` is what the tidiers
# return, `n` what nobs() returns.
.fake_fit <- function(rows = NULL, n = 20) {
  rows <- rows %||%
    data.frame(
      term = c("(Intercept)", "x"),
      estimate = c(1, 0.5),
      std.error = c(0.2, 0.1),
      conf.low = c(0.6, 0.3),
      conf.high = c(1.4, 0.7),
      statistic = c(5, 5),
      p.value = c(0.001, 0.002)
    )
  structure(list(rows = rows, n = n), class = "spicy_fake_fit")
}

# A method of the invented class, registered in the namespace of its
# generic and removed when `frame` exits.
.local_fake_method <- function(generic, ns, fn, frame = parent.frame()) {
  env <- asNamespace(ns)
  registerS3method(generic, "spicy_fake_fit", fn, envir = env)
  withr::defer(
    rm(
      list = paste0(generic, ".spicy_fake_fit"),
      envir = get(".__S3MethodsTable__.", envir = env)
    ),
    envir = frame
  )
}

.local_fake_methods <- function(frame = parent.frame()) {
  skip_if_not_installed("broom")
  skip_if_not_installed("parameters")
  .local_fake_method(
    "tidy",
    "generics",
    function(x, ...) {
      if (is.character(x$rows)) {
        stop(x$rows)
      }
      x$rows
    },
    frame
  )
  .local_fake_method(
    "model_parameters",
    "parameters",
    function(model, ...) model$rows,
    frame
  )
  .local_fake_method("nobs", "stats", function(object, ...) object$n, frame)
}

test_that("an unknown class is refused by default, with a line naming fallback", {
  .local_fake_methods()
  expect_error(
    table_regression(.fake_fit()),
    class = "spicy_unsupported"
  )
  expect_snapshot(table_regression(.fake_fit()), error = TRUE)
})

test_that("`fallback` takes NULL, \"broom\" or \"parameters\"", {
  .local_fake_methods()
  for (bad in list("tidy", c("broom", "parameters"), NA_character_, TRUE)) {
    expect_error(
      table_regression(.fake_fit(), fallback = bad),
      class = "spicy_invalid_input"
    )
  }
})

test_that("the rows are the tidier's, under its term names", {
  .local_fake_methods()
  tb <- table_regression(
    .fake_fit(),
    fallback = "broom",
    show_columns = c("b", "se", "ci", "t", "p")
  )
  td <- broom::tidy(tb)
  expect_identical(td$term, c("(Intercept)", "x"))
  expect_equal(td$estimate, c(1, 0.5))
  expect_equal(td$std.error, c(0.2, 0.1))
  expect_equal(td$conf.low, c(0.6, 0.3))
  expect_equal(td$conf.high, c(1.4, 0.7))
  expect_equal(td$statistic, c(5, 5))
  expect_equal(td$p.value, c(0.001, 0.002))
  expect_identical(attr(tb, "title"), "Regression")
  expect_snapshot(print(tb))
})

test_that("a column the tidier does not return gives missing cells", {
  .local_fake_methods()
  fit <- .fake_fit(data.frame(term = c("a", "b"), estimate = c(1, 2)))
  td <- broom::tidy(table_regression(fit, fallback = "broom"))
  expect_equal(td$estimate, c(1, 2))
  expect_true(all(is.na(td$std.error)))
  expect_true(all(is.na(td$p.value)))
})

test_that("components prefix the terms; random-effect rows are dropped", {
  .local_fake_methods()
  rows <- data.frame(
    term = c("(Intercept)", "x", "(Intercept)", "x", "sd__(Intercept)"),
    component = c("count", "count", "zero", "zero", "count"),
    effect = c("fixed", "fixed", "fixed", "fixed", "ran_pars"),
    estimate = c(1, 2, 3, 4, 5)
  )
  tb <- table_regression(.fake_fit(rows), fallback = "broom")
  td <- broom::tidy(tb)
  expect_identical(
    td$term,
    c("count: (Intercept)", "count: x", "zero: (Intercept)", "zero: x")
  )
  expect_match(attr(tb, "note"), "random-effect rows are not shown")
  # A split column with one value says nothing: no prefix.
  rows$component <- "count"
  rows$term[3:4] <- c("z1", "z2")
  td <- broom::tidy(table_regression(.fake_fit(rows), fallback = "broom"))
  expect_identical(td$term, c("(Intercept)", "x", "z1", "z2"))
})

test_that("parameters columns are read under their own names", {
  .local_fake_methods()
  rows <- data.frame(
    Parameter = c("a", "b", "a"),
    Response = c("y1", "y1", "y2"),
    Effects = "fixed",
    Coefficient = c(1, 2, 3),
    SE = c(0.1, 0.2, 0.3),
    CI_low = c(0.8, 1.6, 2.4),
    CI_high = c(1.2, 2.4, 3.6),
    t = c(10, 10, 10),
    df_error = 30,
    p = c(0.01, 0.02, 0.03)
  )
  tb <- table_regression(
    .fake_fit(rows),
    fallback = "parameters",
    show_columns = c("b", "se", "t", "df", "p")
  )
  td <- broom::tidy(tb)
  expect_identical(td$term, c("y1: a", "y1: b", "y2: a"))
  expect_equal(td$df, c(30, 30, 30))
  expect_identical(unique(td$test_type), "t")
  expect_match(attr(tb, "note"), "parameters::model_parameters()", fixed = TRUE)
})

test_that("a tidier that fails, duplicates a term or has no term is refused", {
  .local_fake_methods()
  expect_error(
    table_regression(.fake_fit("no method here"), fallback = "broom"),
    "no method here",
    class = "spicy_unsupported"
  )
  dup <- data.frame(term = c("x", "x"), estimate = 1:2)
  expect_error(
    table_regression(.fake_fit(dup), fallback = "broom"),
    "more than once",
    class = "spicy_unsupported"
  )
  expect_error(
    table_regression(.fake_fit(data.frame(est = 1)), fallback = "broom"),
    "no `term` and `estimate` columns",
    class = "spicy_unsupported"
  )
})

test_that("a fit whose number of observations cannot be read is refused", {
  .local_fake_methods()
  expect_error(
    table_regression(.fake_fit(n = NA), fallback = "broom"),
    "number of observations",
    class = "spicy_unsupported"
  )
})

test_that("the requests that need to know the model are refused", {
  .local_fake_methods()
  fit <- .fake_fit()
  # The variance gate and the standardization gate refuse with their own
  # classes; the others are the refusals of the fallback.
  refused <- list(
    list(list(vcov = "HC3"), "spicy_unsupported_vcov"),
    list(list(standardized = "refit"), "spicy_unsupported_standardized"),
    list(list(cluster = ~g), "spicy_unsupported"),
    list(list(show_columns = c("b", "ame")), "spicy_unsupported"),
    list(list(exponentiate = TRUE), "spicy_unsupported"),
    list(list(show_fit_stats = c("nobs", "r2")), "spicy_unsupported")
  )
  for (a in refused) {
    expect_error(
      do.call(table_regression, c(list(fit, fallback = "broom"), a[[1L]])),
      class = a[[2L]]
    )
  }
  # Beside an lm, the cluster of the lm alone is not refused.
  m <- lm(mpg ~ wt, data = mtcars)
  expect_no_error(table_regression(
    list(m, fit),
    fallback = "broom",
    vcov = list("CR1", "classical"),
    cluster = list(~cyl, NULL)
  ))
  expect_error(
    table_regression(list(fit, fit), fallback = "broom", nested = TRUE),
    class = "spicy_unsupported"
  )
  expect_snapshot(
    table_regression(fit, fallback = "broom", exponentiate = TRUE),
    error = TRUE
  )
})

test_that("a supported class ignores fallback, and warns when no model needs it", {
  fit <- lm(mpg ~ wt, data = mtcars)
  expect_warning(
    tb <- table_regression(fit, fallback = "broom"),
    "`fallback` is ignored",
    class = "spicy_ignored_arg"
  )
  expect_identical(
    attr(tb, "note"),
    attr(table_regression(fit), "note")
  )
})

test_that("a class refused by name stays refused under fallback", {
  skip_if_not_installed("survival")
  # clogit() calls coxph() and strata() by their bare names.
  coxph <- survival::coxph
  strata <- survival::strata
  cl <- survival::clogit(case ~ spontaneous + strata(stratum), data = infert)
  expect_error(
    suppressWarnings(table_regression(cl, fallback = "broom")),
    class = "spicy_unsupported_class"
  )
})

test_that("arima read by broom and by parameters: the numbers are the tidier's", {
  skip_if_not_installed("broom")
  skip_if_not_installed("parameters")
  fit <- stats::arima(datasets::lh, order = c(1, 0, 0))
  tb <- table_regression(fit, fallback = "broom")
  td <- broom::tidy(tb)
  ref <- broom::tidy(fit, conf.int = TRUE)
  expect_identical(td$term, ref$term)
  expect_equal(td$estimate, ref$estimate)
  expect_equal(td$std.error, ref$std.error)
  expect_equal(td$conf.low, ref$conf.low)
  expect_equal(td$conf.high, ref$conf.high)
  expect_equal(attr(tb, "nobs") %||% stats::nobs(fit), 48)

  tp <- table_regression(fit, fallback = "parameters", ci_level = 0.9)
  td <- broom::tidy(tp)
  ref <- as.data.frame(parameters::model_parameters(fit, ci = 0.9))
  expect_identical(td$term, ref$Parameter)
  expect_equal(td$estimate, ref$Coefficient)
  expect_equal(td$std.error, ref$SE)
  expect_equal(td$conf.low, ref$CI_low)
  expect_equal(td$p.value, ref$p)
  expect_snapshot({
    print(tb)
    print(tp)
  })
})

test_that("beside an lm: one note per model, in English and in French", {
  skip_if_not_installed("broom")
  fit <- stats::arima(datasets::lh, order = c(1, 0, 0))
  models <- list(OLS = lm(mpg ~ wt, data = mtcars), AR = fit)
  tb <- table_regression(models, fallback = "broom")
  expect_identical(attr(tb, "title"), "Regression comparison")
  expect_match(
    attr(tb, "note"),
    "Std. errors: OLS: classical (OLS). AR: Estimates, standard errors",
    fixed = TRUE
  )
  expect_identical(inline(tb, "ar1", model = "AR"), "0.57")
  withr::local_options(spicy.language = "fr")
  tf <- table_regression(models, fallback = "broom")
  expect_match(
    attr(tf, "note"),
    "tels que rendus par broom::tidy()",
    fixed = TRUE
  )
  expect_snapshot({
    print(tb)
    print(tf)
  })
})

test_that("rich outputs carry the rows and the note", {
  skip_if_not_installed("broom")
  skip_if_not_installed("tinytable")
  skip_if_not_installed("openxlsx2")
  fit <- stats::arima(datasets::lh, order = c(1, 0, 0))
  tt <- table_regression(fit, fallback = "broom", output = "tinytable")
  expect_s4_class(tt, "tinytable")
  path <- withr::local_tempfile(fileext = ".xlsx")
  table_regression(fit, fallback = "broom", output = "excel", excel_path = path)
  xl <- openxlsx2::read_xlsx(path, col_names = FALSE)
  cells <- unlist(xl, use.names = FALSE)
  expect_true("ar1" %in% cells)
  expect_true(any(grepl("broom::tidy()", cells, fixed = TRUE)))
})

test_that("beside a selection model, the fallback model keeps one column group", {
  skip_if_not_installed("broom")
  skip_if_not_installed("sampleSelection")
  data("Mroz87", package = "sampleSelection", envir = environment())
  sel <- sampleSelection::selection(
    lfp ~ age + kids5,
    wage ~ exper,
    data = Mroz87,
    method = "2step"
  )
  fit <- stats::arima(datasets::lh, order = c(1, 0, 0))
  td <- broom::tidy(table_regression(
    list(Sel = sel, AR = fit),
    fallback = "broom"
  ))
  ar <- td[td$model_id == "AR", ]
  expect_identical(ar$term, c("ar1", "intercept"))
  expect_equal(ar$estimate, broom::tidy(fit)$estimate)
})

test_that("the AME df column beside an lm is the refusal of the fallback", {
  skip_if_not_installed("broom")
  fit <- stats::arima(datasets::lh, order = c(1, 0, 0))
  m <- lm(mpg ~ wt, data = mtcars)
  expect_error(
    table_regression(
      list(m, fit),
      fallback = "broom",
      show_columns = c("b", "ame_df")
    ),
    "AME column `\"ame_df\"` is not available for model 2",
    class = "spicy_unsupported"
  )
})

test_that("the outcome is read from the formula, else from insight", {
  .local_fake_methods()
  .local_fake_method("formula", "stats", function(x, ...) y ~ x)
  tb <- table_regression(.fake_fit(), fallback = "broom")
  expect_identical(attr(tb, "title"), "Regression: y")
})
