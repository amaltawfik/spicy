# ---- structure ----

test_that("table_continuous_lm returns expected raw structure", {
  out <- table_continuous_lm(
    iris,
    select = c(Sepal.Length, Petal.Width),
    by = Species,
    output = "long"
  )

  expect_s3_class(out, "data.frame")
  expect_named(
    out,
    c(
      "variable",
      "label",
      "predictor_type",
      "predictor_label",
      "level",
      "reference",
      "estimate_type",
      "emmean",
      "emmean_se",
      "emmean_ci_lower",
      "emmean_ci_upper",
      "estimate",
      "estimate_se",
      "estimate_ci_lower",
      "estimate_ci_upper",
      "test_type",
      "statistic",
      "df1",
      "df2",
      "p.value",
      "es_type",
      "es_value",
      "es_ci_lower",
      "es_ci_upper",
      "r2",
      "adj_r2",
      "n",
      "weighted_n"
    )
  )
  expect_equal(nrow(out), 6L)
})

test_that("table_continuous_lm default output returns a spicy table", {
  out <- table_continuous_lm(
    iris,
    select = c(Sepal.Length, Petal.Width),
    by = Species
  )

  expect_s3_class(out, "spicy_continuous_lm_table")
  expect_s3_class(out, "spicy_table")
  expect_equal(attr(out, "by_var"), "Species")
  expect_equal(attr(out, "vcov_type"), "classical")
})

# ---- computation ----

test_that("table_continuous_lm numeric predictor slope matches lm", {
  out <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Petal.Width,
    output = "long"
  )
  fit <- lm(Sepal.Length ~ Petal.Width, data = iris)

  expect_equal(out$predictor_type, "continuous")
  expect_equal(out$estimate, unname(coef(fit)[["Petal.Width"]]))
  expect_equal(out$r2, unname(summary(fit)$r.squared))
})

test_that("table_continuous_lm binary factor stores means and auto contrast", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)

  out <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    output = "long"
  )

  expect_equal(nrow(out), 2L)
  expect_equal(out$level, c("setosa", "versicolor"))
  expect_true(is.na(out$estimate[1]))
  expect_equal(
    out$estimate[2],
    mean(df$Sepal.Length[df$Species == "versicolor"]) -
      mean(df$Sepal.Length[df$Species == "setosa"])
  )
  expect_false(is.na(out$p.value[1]))
  expect_true(is.na(out$es_type[1]))
  expect_true(is.na(out$es_value[1]))
})

test_that("table_continuous_lm 3+ level factor reports estimated means", {
  out <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "long"
  )

  expect_equal(nrow(out), 3L)
  expect_true(all(is.na(out$estimate)))
  expect_equal(
    out$emmean,
    as.vector(tapply(iris$Sepal.Length, iris$Species, mean))
  )
  expect_false(is.na(out$statistic[1]))
  expect_true(all(is.na(out$statistic[-1])))
})

test_that("table_continuous_lm contrast none suppresses binary contrasts", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)

  out <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    contrast = "none",
    output = "long"
  )

  expect_true(is.na(out$estimate[1]))
  expect_true(is.na(out$estimate[2]))
  expect_true(all(is.na(out$estimate_type)))
})

test_that("table_continuous_lm weights accept column names", {
  df <- data.frame(
    y = c(1, 2, 3, 10),
    g = factor(c("A", "A", "B", "B")),
    w = c(1, 1, 1, 5)
  )

  out <- table_continuous_lm(
    df,
    select = y,
    by = g,
    weights = w,
    output = "long"
  )

  expect_equal(out$weighted_n[1], sum(df$w))
  expect_equal(
    out$emmean[2],
    weighted.mean(df$y[df$g == "B"], df$w[df$g == "B"])
  )
})

test_that("table_continuous_lm can display weighted n separately from n", {
  df <- data.frame(
    y = c(1, 2, 3, 10),
    g = factor(c("A", "A", "B", "B")),
    w = c(1, 1, 1, 5)
  )

  out <- table_continuous_lm(
    df,
    select = y,
    by = g,
    weights = w,
    show_weighted_n = TRUE,
    output = "data.frame"
  )

  expect_true(all(c("n", "Weighted n") %in% names(out)))
  expect_equal(out$n[1], 4)
  expect_equal(out$`Weighted n`[1], sum(df$w))
  expect_lt(match("n", names(out)), match("Weighted n", names(out)))
})

test_that("table_continuous_lm ignores weighted n when weights are absent", {
  expect_warning(
    out <- table_continuous_lm(
      iris,
      select = Sepal.Length,
      by = Species,
      show_weighted_n = TRUE,
      output = "data.frame"
    ),
    "show_weighted_n"
  )

  expect_false("Weighted n" %in% names(out))
})

test_that("table_continuous_lm HC3 changes standard errors on heteroskedastic data", {
  df <- data.frame(
    y = c(1.0, 1.2, 1.1, 5.0, 7.5, 10.0),
    x = c(0, 0, 0, 1, 1, 1)
  )

  classical <- table_continuous_lm(
    df,
    select = y,
    by = x,
    vcov = "classical",
    output = "long"
  )
  robust <- table_continuous_lm(
    df,
    select = y,
    by = x,
    vcov = "HC3",
    output = "long"
  )

  expect_false(isTRUE(all.equal(classical$estimate_se, robust$estimate_se)))
})

test_that("table_continuous_lm HC4, HC4m, and HC5 use leverage-adjusted SE", {
  df <- data.frame(
    y = c(2, 2.2, 2.1, 2.4, 3.1, 8.5),
    x = c(0, 1, 2, 3, 4, 20)
  )

  manual_se <- function(type) {
    fit <- lm(y ~ x, data = df)
    xmat <- model.matrix(fit)
    e <- residuals(fit)
    xtx_inv <- solve(crossprod(xmat))
    hat <- rowSums((xmat %*% xtx_inv) * xmat)
    n <- nrow(xmat)
    p <- max(1L, as.integer(round(sum(hat))))
    h_adj <- pmax(1 - hat, .Machine$double.eps)
    nh_over_p <- n * hat / p

    scale <- switch(
      type,
      HC4 = {
        delta <- pmin(4, nh_over_p)
        1 / h_adj^delta
      },
      HC4m = {
        delta <- pmin(1, nh_over_p) + pmin(1.5, nh_over_p)
        1 / h_adj^delta
      },
      HC5 = {
        delta <- pmin(
          nh_over_p,
          pmax(4, n * 0.7 * max(hat) / p)
        )
        1 / sqrt(h_adj^delta)
      }
    )

    vc <- xtx_inv %*% crossprod(xmat, (e^2 * scale) * xmat) %*% xtx_inv
    sqrt(diag(vc))[2]
  }

  out_hc4 <- table_continuous_lm(
    df,
    select = y,
    by = x,
    vcov = "HC4",
    output = "long"
  )
  out_hc4m <- table_continuous_lm(
    df,
    select = y,
    by = x,
    vcov = "HC4m",
    output = "long"
  )
  out_hc5 <- table_continuous_lm(
    df,
    select = y,
    by = x,
    vcov = "HC5",
    output = "long"
  )

  expect_equal(out_hc4$estimate_se, unname(manual_se("HC4")))
  expect_equal(out_hc4m$estimate_se, unname(manual_se("HC4m")))
  expect_equal(out_hc5$estimate_se, unname(manual_se("HC5")))
})

# ---- display ----

test_that("table_continuous_lm uses dedicated digits for fit and effect size", {
  out <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = "f2",
    digits = 1,
    fit_digits = 4,
    effect_size_digits = 3
  )

  display <- spicy:::build_wide_display_df_continuous_lm(
    out,
    digits = 1L,
    decimal_mark = ".",
    ci_level = 0.95,
    show_statistic = FALSE,
    show_p_value = TRUE,
    show_n = TRUE,
    effect_size = "f2",
    r2_type = "r2",
    ci = TRUE,
    fit_digits = 4L,
    effect_size_digits = 3L
  )

  expect_equal(attr(out, "fit_digits"), 4L)
  expect_equal(attr(out, "effect_size_digits"), 3L)
  expect_match(display$`R²`[1], "^0\\.[0-9]{4}$")
  expect_match(display$`f²`[1], "^[0-9]+\\.[0-9]{3}$")
})

test_that("table_continuous_lm rejects deprecated contrast value", {
  expect_error(
    table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = education,
      contrast = "reference",
      output = "long"
    ),
    "should be one of"
  )
})

test_that("table_continuous_lm data.frame output puts >2 categorical levels in columns", {
  out <- table_continuous_lm(
    iris,
    select = c(Sepal.Length, Petal.Width),
    by = Species,
    statistic = TRUE,
    output = "data.frame"
  )

  expect_s3_class(out, "data.frame")
  expect_true("M (setosa)" %in% names(out))
  expect_true("M (versicolor)" %in% names(out))
  expect_true("M (virginica)" %in% names(out))
  expect_false(any(grepl("^Δ", names(out))))
  expect_false(any(grepl("CI", names(out))))
  expect_true("F(2, 147)" %in% names(out))
  expect_true("p" %in% names(out))
  expect_false("f²" %in% names(out))
  expect_true("R²" %in% names(out))
  expect_equal(out$Variable[1], "Sepal.Length")
  expect_true(is.numeric(out$p))
})

test_that("table_continuous_lm data.frame output labels binary contrasts explicitly", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)

  out <- table_continuous_lm(
    df,
    select = c(Sepal.Length, Petal.Width),
    by = Species,
    statistic = TRUE,
    output = "data.frame"
  )

  expect_true("M (setosa)" %in% names(out))
  expect_true("M (versicolor)" %in% names(out))
  expect_true("Δ (versicolor - setosa)" %in% names(out))
  expect_true("95% CI LL" %in% names(out))
  expect_true("95% CI UL" %in% names(out))
  expect_true(any(grepl("^t\\([0-9]+\\)$", names(out))))
  expect_true("R²" %in% names(out))
  expect_true(is.numeric(out$`Δ (versicolor - setosa)`))
})

test_that("table_continuous_lm data.frame output honors optional columns", {
  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    statistic = FALSE,
    p_value = FALSE,
    show_n = FALSE,
    effect_size = "none",
    output = "data.frame"
  )

  expect_false("t" %in% names(out))
  expect_false("p" %in% names(out))
  expect_false("n" %in% names(out))
  expect_false("f²" %in% names(out))
  expect_true("R²" %in% names(out))
})

test_that("table_continuous_lm data.frame can hide CI columns", {
  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    ci = FALSE,
    output = "data.frame"
  )

  expect_false(any(grepl("CI", names(out))))
})

test_that("table_continuous_lm data.frame uses R2 by default and can show adjusted R2", {
  out_r2 <- table_continuous_lm(
    iris,
    select = c(Sepal.Length, Petal.Width),
    by = Species,
    output = "data.frame"
  )
  out <- table_continuous_lm(
    iris,
    select = c(Sepal.Length, Petal.Width),
    by = Species,
    r2 = "adj_r2",
    output = "data.frame"
  )

  expect_true("R²" %in% names(out_r2))
  expect_false("Adj. R²" %in% names(out_r2))
  expect_true("Adj. R²" %in% names(out))
  expect_false("R²" %in% names(out))
  expect_true(is.numeric(out$`Adj. R²`))
})

test_that("table_continuous_lm rejects deprecated wide output", {
  expect_error(
    table_continuous_lm(
      sochealth,
      select = c(wellbeing_score, bmi),
      by = sex,
      output = "wide"
    ),
    "should be one of"
  )
})

# ---- validation ----

test_that("table_continuous_lm validates select and weights", {
  df <- data.frame(y = letters[1:3], x = 1:3, w = c(1, -1, 1))

  expect_warning(
    out <- table_continuous_lm(df, select = y, by = x, output = "long"),
    "No numeric outcome columns selected"
  )
  expect_equal(ncol(out), 0L)
  expect_error(
    table_continuous_lm(
      transform(df, y = 1:3),
      select = y,
      by = x,
      weights = w
    ),
    "weights"
  )
})

test_that("table_continuous_lm validates core user arguments", {
  df <- data.frame(y = 1:3, x = 1:3)

  expect_error(
    table_continuous_lm(list(y = 1:3), select = y, by = x, output = "long"),
    "data.frame"
  )
  expect_error(
    table_continuous_lm(df, select = y, by = x, ci_level = 1, output = "long"),
    "ci_level"
  )
  expect_error(
    table_continuous_lm(df, select = y, by = x, digits = -1, output = "long"),
    "digits"
  )
  expect_error(
    table_continuous_lm(
      df,
      select = y,
      by = x,
      fit_digits = -1,
      output = "long"
    ),
    "fit_digits"
  )
  expect_error(
    table_continuous_lm(
      df,
      select = y,
      by = x,
      effect_size_digits = -1,
      output = "long"
    ),
    "effect_size_digits"
  )
  expect_error(
    table_continuous_lm(
      df,
      select = y,
      by = x,
      decimal_mark = ";",
      output = "long"
    ),
    "decimal_mark"
  )
  expect_error(
    table_continuous_lm(
      df,
      select = y,
      by = x,
      labels = c("Y label"),
      output = "long"
    ),
    "named character vector"
  )
  expect_error(
    table_continuous_lm(
      df,
      select = y,
      by = x,
      statistic = NA,
      output = "long"
    ),
    "TRUE/FALSE"
  )
})

test_that("table_continuous_lm validates predictor and regex selection", {
  df <- data.frame(
    y = 1:3,
    z = 4:6,
    bad = I(list(1, 2, 3))
  )

  expect_error(
    table_continuous_lm(df, select = y, by = bad, output = "long"),
    "must be numeric, logical, character, or factor"
  )
  expect_error(
    table_continuous_lm(df, select = c("y", "z"), by = y, regex = TRUE),
    "single regex pattern"
  )
  expect_error(
    table_continuous_lm(df, select = unknown, by = y, output = "long"),
    "select"
  )
})

test_that("table_continuous_lm validates weight edge cases", {
  df <- data.frame(y = 1:3, x = 1:3, w = c(1, Inf, 1), z = c(0, 0, 0))

  err1 <- tryCatch(
    {
      table_continuous_lm(df, select = y, by = x, weights = w, output = "long")
      NULL
    },
    error = identity
  )
  err2 <- tryCatch(
    {
      table_continuous_lm(df, select = y, by = x, weights = z, output = "long")
      NULL
    },
    error = identity
  )

  expect_match(conditionMessage(err1), "finite")
  expect_match(conditionMessage(err2), "at least one positive value")
})

test_that("table_continuous_lm uses labels, exclude, regex, and character by", {
  df <- sochealth
  attr(df$wellbeing_score, "label") <- "Wellbeing attr"

  out <- table_continuous_lm(
    df,
    select = "^well|^bmi$",
    by = "sex",
    regex = TRUE,
    exclude = bmi,
    output = "data.frame"
  )

  expect_equal(out$Variable, "Wellbeing attr")
})

test_that("table_continuous_lm reports ignored non-numeric outcomes in verbose mode", {
  expect_message(
    out <- table_continuous_lm(
      sochealth,
      select = c(wellbeing_score, sex),
      by = age,
      verbose = TRUE,
      output = "data.frame"
    ),
    "Ignoring non-numeric selected outcomes: sex"
  )

  expect_equal(out$Variable, "WHO-5 wellbeing index (0-100)")
})

test_that("table_continuous_lm helper functions cover empty-model cases", {
  num_empty <- spicy:::fit_numeric_predictor_lm_rows(
    y = c(1, 2),
    x = c(1, 1),
    weights = NULL,
    outcome_name = "y",
    outcome_label = "Y",
    predictor_label = "X",
    vcov_type = "classical",
    ci_level = 0.95
  )
  cat_empty <- spicy:::fit_categorical_predictor_lm_rows(
    y = c(1, 2),
    x = factor(c("A", "A")),
    weights = NULL,
    outcome_name = "y",
    outcome_label = "Y",
    predictor_label = "G",
    vcov_type = "classical",
    contrast = "auto",
    ci_level = 0.95
  )

  expect_true(all(is.na(num_empty$estimate)))
  expect_equal(num_empty$predictor_type, "continuous")
  expect_true(all(is.na(cat_empty$emmean)))
  expect_equal(cat_empty$predictor_type, "categorical")
})

test_that("table_continuous_lm helper functions handle coercion and weights detection", {
  expect_true(is.factor(spicy:::coerce_lm_factor(c("a", "b"))))
  expect_true(is.factor(spicy:::coerce_lm_factor(factor("a"))))
  expect_true(spicy:::is_supported_lm_predictor(TRUE))
  expect_false(spicy:::is_supported_lm_predictor(list(1, 2)))

  df <- data.frame(y = 1:3, w = c(1, 2, 3))
  expect_equal(
    spicy:::detect_weights_column_name(rlang::quo(w), df),
    "w"
  )
  expect_equal(
    spicy:::detect_weights_column_name(rlang::quo("w"), df),
    "w"
  )
  expect_null(spicy:::detect_weights_column_name(rlang::quo(c(1, 2, 3)), df))
})

test_that("table_continuous_lm internal builders cover numeric displays", {
  out <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = age,
    statistic = TRUE,
    effect_size = "f2",
    output = "long"
  )

  wide_raw <- spicy:::build_wide_raw_continuous_lm(
    out,
    show_statistic = TRUE,
    show_p_value = TRUE,
    show_n = TRUE,
    effect_size = "f2",
    r2_type = "adj_r2",
    ci = TRUE
  )
  wide_display <- spicy:::build_wide_display_df_continuous_lm(
    out,
    digits = 2L,
    decimal_mark = ".",
    ci_level = 0.95,
    show_statistic = TRUE,
    show_p_value = TRUE,
    show_n = TRUE,
    effect_size = "f2",
    r2_type = "adj_r2",
    ci = TRUE
  )

  expect_true(all(
    c("B", "95% CI LL", "95% CI UL", "p", "Adj. R²", "f²", "n") %in%
      names(wide_raw)
  ))
  expect_lt(match("Adj. R²", names(wide_raw)), match("f²", names(wide_raw)))
  expect_true(any(grepl("^t", names(wide_raw))))
  expect_true(all(
    c("Variable", "B", "95% CI LL", "95% CI UL", "p", "Adj. R²", "f²", "n") %in%
      names(wide_display)
  ))
  expect_lt(
    match("Adj. R²", names(wide_display)),
    match("f²", names(wide_display))
  )
  expect_true(any(grepl("^t", names(wide_display))))
})

test_that("table_continuous_lm internal builders cover binary categorical variants", {
  df <- sochealth
  out <- table_continuous_lm(
    df,
    select = c(wellbeing_score, bmi),
    by = sex,
    statistic = FALSE,
    p_value = FALSE,
    show_n = FALSE,
    effect_size = "none",
    r2 = "none",
    ci = FALSE,
    output = "long"
  )

  wide_raw <- spicy:::build_wide_raw_continuous_lm(
    out,
    show_statistic = FALSE,
    show_p_value = FALSE,
    show_n = FALSE,
    effect_size = "none",
    r2_type = "none",
    ci = FALSE
  )
  wide_display <- spicy:::build_wide_display_df_continuous_lm(
    out,
    digits = 2L,
    decimal_mark = ",",
    ci_level = 0.95,
    show_statistic = FALSE,
    show_p_value = FALSE,
    show_n = FALSE,
    effect_size = "none",
    r2_type = "none",
    ci = FALSE
  )

  expect_true(all(
    c("M (Female)", "M (Male)", "Δ (Male - Female)") %in% names(wide_raw)
  ))
  expect_false(any(
    c("p", "n", "f²", "R²", "Adj. R²", "95% CI LL") %in% names(wide_raw)
  ))
  expect_true(all(
    c("Variable", "M (Female)", "M (Male)", "Δ (Male - Female)") %in%
      names(wide_display)
  ))
  expect_false(any(
    c("p", "n", "f²", "R²", "Adj. R²", "95% CI LL") %in% names(wide_display)
  ))
})

test_that("table_continuous_lm low-level formatting helpers behave as expected", {
  block <- table_continuous_lm(
    iris[iris$Species != "virginica", ],
    select = Sepal.Length,
    by = Species,
    output = "long"
  )

  expect_equal(
    spicy:::rename_ci_cols_lm(
      setNames(
        data.frame(1, 2, check.names = FALSE),
        c("95% CI LL", "95% CI UL")
      ),
      "95% CI LL",
      "95% CI UL"
    ),
    setNames(data.frame(1, 2, check.names = FALSE), c("LL", "UL"))
  )
  hdr <- spicy:::build_header_rows_lm(c("Variable", "LL", "UL"), "95%")
  expect_equal(hdr$top, c("Variable", "95% CI", "95% CI"))
  expect_equal(hdr$bottom, c("", "LL", "UL"))
  expect_match(spicy:::get_delta_label_lm(block), "versicolor - setosa")
  expect_equal(spicy:::get_test_row_index_lm(block), 2L)
  expect_equal(spicy:::get_test_header_lm(block, TRUE, FALSE), "t")
  expect_null(spicy:::get_test_header_lm(block, FALSE, TRUE))
  expect_equal(spicy:::format_effect_size_header_lm("f2"), "f²")
  expect_equal(spicy:::format_r2_header_lm("adj_r2"), "Adj. R²")
  expect_true(is.numeric(spicy:::get_r2_value_lm(block, "r2")))
  expect_equal(spicy:::format_number(c(1.2, NA), 1L, ","), c("1,2", ""))
  expect_equal(spicy:::format_p_value(NA_real_), "")
  expect_equal(spicy:::format_p_value(0.045, ","), ",045")
})

test_that("table_continuous_lm internal covariance helper covers fallback branches", {
  fit <- lm(mpg ~ wt, data = mtcars)
  fit_singular <- lm(mpg ~ wt + I(2 * wt), data = mtcars)

  expect_error(
    spicy:::compute_lm_vcov(fit, "bogus"),
    "Unknown `vcov` type"
  )

  # sandwich::vcovHC handles rank-deficient fits gracefully by
  # returning the (rank x rank) matrix for the identifiable
  # coefficients, without raising a warning.
  vc <- spicy:::compute_lm_vcov(fit_singular, "HC3")
  expect_true(is.matrix(vc))
  expect_equal(dim(vc), c(fit_singular$rank, fit_singular$rank))
  expect_false(anyNA(vc))
})

test_that("table_continuous_lm export helper validates output-specific paths", {
  display_df <- data.frame(
    Variable = "x",
    `M (A)` = "1.00",
    p = ".050",
    check.names = FALSE
  )

  expect_error(
    spicy:::export_continuous_lm_table(
      display_df,
      output = "excel",
      ci_level = 0.95,
      excel_path = NULL,
      excel_sheet = "Sheet1",
      clipboard_delim = "\t",
      word_path = NULL
    ),
    "excel_path"
  )
  expect_error(
    spicy:::export_continuous_lm_table(
      display_df,
      output = "word",
      ci_level = 0.95,
      excel_path = NULL,
      excel_sheet = "Sheet1",
      clipboard_delim = "\t",
      word_path = NULL
    ),
    "word_path"
  )
  expect_error(
    spicy:::export_continuous_lm_table(
      display_df,
      output = "bogus",
      ci_level = 0.95,
      excel_path = NULL,
      excel_sheet = "Sheet1",
      clipboard_delim = "\t",
      word_path = NULL
    ),
    "Unknown output format"
  )
})

# ---- optional outputs ----

test_that("table_continuous_lm tinytable output works", {
  skip_if_not_installed("tinytable")

  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    output = "tinytable"
  )

  expect_true(inherits(out, "tinytable"))
})

test_that("table_continuous_lm gt output works", {
  skip_if_not_installed("gt")

  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    output = "gt"
  )

  expect_s3_class(out, "gt_tbl")
})

test_that("table_continuous_lm gt output includes a CI spanner", {
  skip_if_not_installed("gt")

  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    statistic = TRUE,
    output = "gt"
  )

  spanners <- out[["_spanners"]]
  labels <- unlist(spanners$spanner_label)
  expect_true(any(grepl("CI", labels)))
})

test_that("table_continuous_lm flextable output works", {
  skip_if_not_installed("flextable")

  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    output = "flextable"
  )

  expect_s3_class(out, "flextable")
})

test_that("table_continuous_lm flextable has 2-row header with CI spanner", {
  skip_if_not_installed("flextable")

  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    statistic = TRUE,
    output = "flextable"
  )

  hdr <- out$header$dataset
  expect_equal(nrow(hdr), 2L)
  expect_true(any(grepl("CI", hdr[1, ])))
})

test_that("table_continuous_lm excel output works", {
  skip_if_not_installed("openxlsx2")

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  local_mocked_bindings(
    wb_save = function(wb, file, overwrite = TRUE, ...) {
      invisible(wb)
    },
    .package = "openxlsx2"
  )

  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    output = "excel",
    excel_path = tmp
  )

  expect_equal(out, tmp)
})

test_that("table_continuous_lm word output works", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)

  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    output = "word",
    word_path = tmp
  )

  expect_equal(out, tmp)
  expect_true(file.exists(tmp))
})

# ---- effect sizes ----

test_that("effect_size = 'd' matches beta / sigma from lm for binary by", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  fit <- lm(Sepal.Length ~ Species, data = df)
  expected_d <- unname(coef(fit)[2]) / summary(fit)$sigma

  out <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = "d",
    output = "long"
  )

  expect_equal(out$es_type[1], "d")
  expect_equal(out$es_value[1], expected_d)
})

test_that("effect_size = 'g' applies the Hedges small-sample correction", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  fit <- lm(Sepal.Length ~ Species, data = df)
  d <- unname(coef(fit)[2]) / summary(fit)$sigma
  df_resid <- df.residual(fit)
  expected_g <- (1 - 3 / (4 * df_resid - 1)) * d

  out <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = "g",
    output = "long"
  )

  expect_equal(out$es_type[1], "g")
  expect_equal(out$es_value[1], expected_g)
})

test_that("effect_size = 'd' / 'g' work with weights via the WLS fit", {
  df <- data.frame(
    y = c(1, 2, 3, 5, 6, 8, 10, 12),
    g = factor(c("A", "A", "A", "A", "B", "B", "B", "B")),
    w = c(1, 2, 1, 2, 1, 2, 1, 2)
  )
  fit <- lm(y ~ g, data = df, weights = w)
  expected_d <- unname(coef(fit)[2]) / summary(fit)$sigma
  expected_g <- (1 - 3 / (4 * df.residual(fit) - 1)) * expected_d

  out_d <- table_continuous_lm(
    df,
    select = y,
    by = g,
    weights = w,
    effect_size = "d",
    output = "long"
  )
  out_g <- table_continuous_lm(
    df,
    select = y,
    by = g,
    weights = w,
    effect_size = "g",
    output = "long"
  )

  expect_equal(out_d$es_value[1], expected_d)
  expect_equal(out_g$es_value[1], expected_g)
})

test_that("effect_size = 'd' / 'g' error when by is not a 2-level categorical", {
  expect_error(
    table_continuous_lm(
      iris,
      select = Sepal.Length,
      by = Species,
      effect_size = "d",
      output = "long"
    ),
    "exactly two non-empty levels"
  )
  expect_error(
    table_continuous_lm(
      iris,
      select = Sepal.Length,
      by = Species,
      effect_size = "g",
      output = "long"
    ),
    "exactly two non-empty levels"
  )
  expect_error(
    table_continuous_lm(
      iris,
      select = Sepal.Length,
      by = Petal.Width,
      effect_size = "d",
      output = "long"
    ),
    "exactly two non-empty levels"
  )
})

test_that("effect_size = 'omega2' matches Hays' formula and is truncated at 0", {
  fit <- lm(Sepal.Length ~ Species, data = iris)
  ss_total <- sum((iris$Sepal.Length - mean(iris$Sepal.Length))^2)
  ss_resid <- sum(residuals(fit)^2)
  ss_effect <- ss_total - ss_resid
  df_effect <- length(coef(fit)) - 1L
  df_resid <- df.residual(fit)
  mse <- ss_resid / df_resid
  expected <- max(0, (ss_effect - df_effect * mse) / (ss_total + mse))

  out <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = "omega2",
    output = "long"
  )

  expect_equal(out$es_type[1], "omega2")
  expect_equal(out$es_value[1], expected)

  null_df <- data.frame(
    y = c(1, 2, 3, 1, 2, 3),
    g = factor(c("A", "A", "A", "B", "B", "B"))
  )
  out_null <- table_continuous_lm(
    null_df,
    select = y,
    by = g,
    effect_size = "omega2",
    output = "long"
  )
  expect_equal(out_null$es_value[1], 0)
})

test_that("effect sizes are invariant to the choice of vcov", {
  df <- data.frame(
    y = c(1, 1.2, 1.1, 5, 7.5, 10),
    x = factor(c("A", "A", "A", "B", "B", "B"))
  )

  for (es in c("f2", "d", "g", "omega2")) {
    classical <- table_continuous_lm(
      df,
      select = y,
      by = x,
      vcov = "classical",
      effect_size = es,
      output = "long"
    )
    robust <- table_continuous_lm(
      df,
      select = y,
      by = x,
      vcov = "HC3",
      effect_size = es,
      output = "long"
    )
    expect_equal(classical$es_value[1], robust$es_value[1])
  }
})

test_that("effect_size column header reflects the chosen metric in wide outputs", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)

  out_d <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = "d",
    output = "data.frame"
  )
  out_g <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = "g",
    output = "data.frame"
  )
  out_omega <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = "omega2",
    output = "data.frame"
  )

  expect_true("d" %in% names(out_d))
  expect_true("g" %in% names(out_g))
  expect_true("ω²" %in% names(out_omega))
})

test_that("effect_size_ci computes valid noncentral CIs containing the point estimate", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)

  for (es in c("d", "g")) {
    out <- table_continuous_lm(
      df,
      select = Sepal.Length,
      by = Species,
      effect_size = es,
      effect_size_ci = TRUE,
      output = "long"
    )
    expect_true(is.finite(out$es_ci_lower[1]))
    expect_true(is.finite(out$es_ci_upper[1]))
    expect_lt(out$es_ci_lower[1], out$es_value[1])
    expect_gt(out$es_ci_upper[1], out$es_value[1])
  }

  for (es in c("f2", "omega2")) {
    out <- table_continuous_lm(
      iris,
      select = Sepal.Length,
      by = Species,
      effect_size = es,
      effect_size_ci = TRUE,
      output = "long"
    )
    expect_true(is.finite(out$es_ci_lower[1]))
    expect_true(is.finite(out$es_ci_upper[1]))
    expect_lte(out$es_ci_lower[1], out$es_value[1])
    expect_gte(out$es_ci_upper[1], out$es_value[1])
  }
})

test_that("effect_size_ci CIs match effectsize package for d and g (nct)", {
  skip_if_not_installed("effectsize")
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)

  out_d <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = "d",
    effect_size_ci = TRUE,
    output = "long"
  )
  ed <- effectsize::cohens_d(
    Sepal.Length ~ Species,
    data = df,
    ci_method = "nct"
  )
  expect_equal(abs(out_d$es_value[1]), abs(ed$Cohens_d), tolerance = 1e-6)
  expect_equal(
    sort(c(out_d$es_ci_lower[1], out_d$es_ci_upper[1])),
    sort(abs(c(ed$CI_low, ed$CI_high))),
    tolerance = 1e-4
  )

  out_g <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = "g",
    effect_size_ci = TRUE,
    output = "long"
  )
  eg <- effectsize::hedges_g(
    Sepal.Length ~ Species,
    data = df,
    ci_method = "nct"
  )
  expect_equal(abs(out_g$es_value[1]), abs(eg$Hedges_g), tolerance = 1e-4)
  expect_equal(
    sort(c(out_g$es_ci_lower[1], out_g$es_ci_upper[1])),
    sort(abs(c(eg$CI_low, eg$CI_high))),
    tolerance = 1e-4
  )
})

test_that("effect_size_ci is invariant to vcov choice", {
  df <- data.frame(
    y = c(1, 1.2, 1.1, 5, 7.5, 10),
    x = factor(c("A", "A", "A", "B", "B", "B"))
  )
  for (es in c("f2", "d", "g", "omega2")) {
    classical <- table_continuous_lm(
      df,
      select = y,
      by = x,
      vcov = "classical",
      effect_size = es,
      effect_size_ci = TRUE,
      output = "long"
    )
    robust <- table_continuous_lm(
      df,
      select = y,
      by = x,
      vcov = "HC3",
      effect_size = es,
      effect_size_ci = TRUE,
      output = "long"
    )
    expect_equal(classical$es_ci_lower[1], robust$es_ci_lower[1])
    expect_equal(classical$es_ci_upper[1], robust$es_ci_upper[1])
  }
})

test_that("effect_size_ci = TRUE adds bracket notation in wide display", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  out <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = "g",
    effect_size_ci = TRUE,
    digits = 2L,
    effect_size_digits = 2L,
    output = "long"
  )
  display <- spicy:::build_wide_display_df_continuous_lm(
    out,
    digits = 2L,
    effect_size_digits = 2L,
    decimal_mark = ".",
    ci_level = 0.95,
    show_statistic = FALSE,
    show_p_value = TRUE,
    show_n = TRUE,
    effect_size = "g",
    effect_size_ci = TRUE,
    r2_type = "r2",
    ci = TRUE
  )

  expect_match(display$g[1], "^[0-9.]+ \\[[0-9.]+, [0-9.]+\\]$")
})

test_that("effect_size_ci = TRUE adds numeric LL/UL columns in wide raw", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  out <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = "d",
    effect_size_ci = TRUE,
    output = "data.frame"
  )

  expect_true(all(
    c("d", "effect_size_ci_lower", "effect_size_ci_upper") %in% names(out)
  ))
  expect_type(out$effect_size_ci_lower, "double")
  expect_type(out$effect_size_ci_upper, "double")
  expect_lt(out$effect_size_ci_lower[1], out$d[1])
  expect_gt(out$effect_size_ci_upper[1], out$d[1])
})

test_that("effect_size_ci = TRUE warns and resets when effect_size is none", {
  expect_warning(
    out <- table_continuous_lm(
      iris,
      select = Sepal.Length,
      by = Species,
      effect_size_ci = TRUE,
      output = "long"
    ),
    "effect_size_ci.*is ignored"
  )
  expect_true(all(is.na(out$es_ci_lower)))
  expect_true(all(is.na(out$es_ci_upper)))
})

test_that("effect_size_ci validates as logical scalar", {
  expect_error(
    table_continuous_lm(
      iris,
      select = Sepal.Length,
      by = Species,
      effect_size_ci = NA,
      output = "long"
    ),
    "effect_size_ci.*TRUE/FALSE"
  )
})

test_that("ci_level controls effect-size CI width", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  out_95 <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = "d",
    effect_size_ci = TRUE,
    ci_level = 0.95,
    output = "long"
  )
  out_99 <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = "d",
    effect_size_ci = TRUE,
    ci_level = 0.99,
    output = "long"
  )

  width_95 <- out_95$es_ci_upper[1] - out_95$es_ci_lower[1]
  width_99 <- out_99$es_ci_upper[1] - out_99$es_ci_lower[1]
  expect_gt(width_99, width_95)
})

test_that("degenerate models still preserve the predictor label", {
  df <- data.frame(
    y_ok = c(1, 2, 3, 4, 5, 6),
    y_bad = c(NA, NA, NA, NA, NA, 1),
    g = factor(c("A", "A", "A", "B", "B", "B"))
  )
  attr(df$g, "label") <- "Group label"

  out <- table_continuous_lm(
    df,
    select = c(y_ok, y_bad),
    by = g,
    output = "long"
  )

  expect_true(all(out$predictor_label == "Group label"))
})

test_that("long output uses integer types for n and df1, double for df2", {
  out_num <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Petal.Width,
    output = "long"
  )
  out_cat <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "long"
  )
  out_empty <- spicy:::make_empty_lm_rows("y", "Y", "continuous")

  # n and df1 are always whole numbers -> integer.
  # df2 is double to allow fractional Satterthwaite df under cluster-
  # robust inference (CR2). Whole-number df2 still equal their
  # integer value (e.g., 147.0).
  expect_type(out_num$n, "integer")
  expect_type(out_num$df1, "integer")
  expect_type(out_num$df2, "double")
  expect_type(out_cat$n, "integer")
  expect_type(out_cat$df1, "integer")
  expect_type(out_cat$df2, "double")
  expect_type(out_empty$n, "integer")
  expect_type(out_empty$df1, "integer")
  expect_type(out_empty$df2, "double")
})

test_that("effect_size = 'none' yields NA es_type and es_value in long output", {
  out <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "long"
  )

  expect_true(all(is.na(out$es_type)))
  expect_true(all(is.na(out$es_value)))
})

# ---- coercion + broom integration ----

test_that("as.data.frame.spicy_continuous_lm_table strips spicy classes/attrs", {
  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex
  )
  df <- as.data.frame(out)

  expect_identical(class(df), "data.frame")
  expect_setequal(
    setdiff(names(attributes(df)), c("names", "row.names", "class")),
    "by_var"
  )
  expect_equal(attr(df, "by_var"), "sex")
  # Same content: 28 columns + same number of rows.
  expect_equal(ncol(df), 28L)
  expect_equal(nrow(df), nrow(out))
  # Original object is unchanged.
  expect_true(inherits(out, "spicy_continuous_lm_table"))
  expect_true(!is.null(attr(out, "ci_level")))
})

test_that("as_tibble.spicy_continuous_lm_table returns a tbl_df", {
  skip_if_not_installed("tibble")
  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex
  )
  tib <- tibble::as_tibble(out)
  expect_s3_class(tib, "tbl_df")
  expect_equal(ncol(tib), 28L)
  expect_equal(nrow(tib), nrow(out))
  expect_true(inherits(out, "spicy_continuous_lm_table"))
})

test_that("tidy() returns one row per parameter for binary categorical by", {
  skip_if_not_installed("broom")
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  out <- table_continuous_lm(
    df,
    select = c(Sepal.Length, Petal.Width),
    by = Species
  )
  tidy_out <- broom::tidy(out)

  # Two outcomes x (2 emmeans + 1 contrast) = 6 rows.
  expect_equal(nrow(tidy_out), 6L)
  expect_setequal(
    unique(tidy_out$estimate_type),
    c("emmean", "difference")
  )
  # The difference rows have non-NA p.value; emmeans do not.
  diffs <- tidy_out[tidy_out$estimate_type == "difference", ]
  emmeans <- tidy_out[tidy_out$estimate_type == "emmean", ]
  expect_true(all(!is.na(diffs$p.value)))
  expect_true(all(is.na(emmeans$p.value)))
  # Standard broom columns present.
  expect_true(all(
    c(
      "outcome", "label", "term", "estimate_type", "estimate",
      "std.error", "conf.low", "conf.high", "statistic", "p.value"
    ) %in%
      names(tidy_out)
  ))
})

test_that("tidy() handles 3-level categorical by (no contrast row)", {
  skip_if_not_installed("broom")
  out <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Species
  )
  tidy_out <- broom::tidy(out)

  # 3 emmeans, no difference / slope rows.
  expect_equal(nrow(tidy_out), 3L)
  expect_true(all(tidy_out$estimate_type == "emmean"))
  expect_true(all(is.na(tidy_out$p.value)))
})

test_that("tidy() handles numeric predictor (slope row)", {
  skip_if_not_installed("broom")
  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = age
  )
  tidy_out <- broom::tidy(out)

  expect_equal(nrow(tidy_out), 2L)
  expect_true(all(tidy_out$estimate_type == "slope"))
  expect_true(all(!is.na(tidy_out$p.value)))
  expect_true(all(!is.na(tidy_out$conf.low)))
})

test_that("glance() returns one row per outcome with model-level stats", {
  skip_if_not_installed("broom")
  out <- table_continuous_lm(
    iris,
    select = c(Sepal.Length, Petal.Width),
    by = Species,
    effect_size = "omega2",
    effect_size_ci = TRUE
  )
  glance_out <- broom::glance(out)

  expect_equal(nrow(glance_out), 2L)
  expect_true(all(
    c(
      "outcome", "label", "predictor_type", "test_type", "statistic",
      "df", "df.residual", "p.value", "r.squared", "adj.r.squared",
      "es_type", "es_value", "es_ci_lower", "es_ci_upper", "nobs"
    ) %in%
      names(glance_out)
  ))
  expect_true(all(glance_out$test_type == "F"))
  expect_true(all(!is.na(glance_out$r.squared)))
  expect_true(all(!is.na(glance_out$es_value)))
})

test_that("glance() exposes test_type 't' for numeric predictors", {
  skip_if_not_installed("broom")
  out <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = age
  )
  glance_out <- broom::glance(out)
  expect_equal(glance_out$test_type[1], "t")
})

test_that("coercion is a no-op on the original object", {
  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex
  )
  attr_pre <- names(attributes(out))
  cls_pre <- class(out)
  invisible(as.data.frame(out))
  if (requireNamespace("tibble", quietly = TRUE)) {
    invisible(tibble::as_tibble(out))
  }
  if (requireNamespace("broom", quietly = TRUE)) {
    invisible(broom::tidy(out))
    invisible(broom::glance(out))
  }
  expect_identical(class(out), cls_pre)
  expect_identical(names(attributes(out)), attr_pre)
})

# ---- p_digits ----

test_that("format_p_value derives threshold from digits", {
  expect_equal(spicy:::format_p_value(0.045, ".", 3L), ".045")
  expect_equal(spicy:::format_p_value(0.0008, ".", 3L), "<.001")
  expect_equal(spicy:::format_p_value(0.0451, ".", 4L), ".0451")
  expect_equal(spicy:::format_p_value(0.00008, ".", 4L), "<.0001")
  expect_equal(spicy:::format_p_value(0.05, ".", 2L), ".05")
  expect_equal(spicy:::format_p_value(0.005, ".", 2L), "<.01")
})

test_that("format_p_value respects European decimal mark across digits", {
  expect_equal(spicy:::format_p_value(0.045, ",", 3L), ",045")
  expect_equal(spicy:::format_p_value(0.0008, ",", 3L), "<,001")
  expect_equal(spicy:::format_p_value(0.00008, ",", 4L), "<,0001")
  expect_equal(spicy:::format_p_value(0.005, ",", 2L), "<,01")
})

test_that("format_p_value handles NA and falls back to default for bad digits", {
  expect_equal(spicy:::format_p_value(NA_real_, ".", 3L), "")
  # Non-finite or < 1 digits silently fall back to digits=3.
  expect_equal(spicy:::format_p_value(0.045, ".", 0L), ".045")
  expect_equal(spicy:::format_p_value(0.045, ".", -1L), ".045")
})

test_that("p_digits argument propagates through table_continuous_lm()", {
  out_3 <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = sex,
    output = "default"
  )
  out_4 <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = sex,
    p_digits = 4,
    output = "default"
  )
  out_2 <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = sex,
    p_digits = 2,
    output = "default"
  )

  expect_equal(attr(out_3, "p_digits"), 3L)
  expect_equal(attr(out_4, "p_digits"), 4L)
  expect_equal(attr(out_2, "p_digits"), 2L)
})

test_that("p_digits changes the rendered p value in wide display", {
  long <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = sex,
    output = "long"
  )
  display_3 <- spicy:::build_wide_display_df_continuous_lm(
    long,
    digits = 2L,
    fit_digits = 2L,
    effect_size_digits = 2L,
    p_digits = 3L,
    decimal_mark = ".",
    ci_level = 0.95,
    show_statistic = FALSE,
    show_p_value = TRUE,
    show_n = TRUE,
    effect_size = "none",
    effect_size_ci = FALSE,
    r2_type = "r2",
    ci = TRUE
  )
  display_4 <- spicy:::build_wide_display_df_continuous_lm(
    long,
    digits = 2L,
    fit_digits = 2L,
    effect_size_digits = 2L,
    p_digits = 4L,
    decimal_mark = ".",
    ci_level = 0.95,
    show_statistic = FALSE,
    show_p_value = TRUE,
    show_n = TRUE,
    effect_size = "none",
    effect_size_ci = FALSE,
    r2_type = "r2",
    ci = TRUE
  )

  # The p column is named "p". With p_digits = 3, format is X.XXX or
  # <.001; with p_digits = 4 it is X.XXXX or <.0001.
  expect_match(display_3$p[1], "^[<.0-9]+$")
  expect_match(display_4$p[1], "^[<.0-9]+$")
  # Number of fractional digits in the rendered string should match
  # p_digits, except for "<.0..1" which has digits chars after the dot.
  count_after_dot <- function(s) nchar(sub("^.*\\.", "", s))
  expect_equal(count_after_dot(display_3$p[1]), 3L)
  expect_equal(count_after_dot(display_4$p[1]), 4L)
})

test_that("p_digits validates as a single positive integer", {
  expect_error(
    table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = sex,
      p_digits = 0
    ),
    "positive integer"
  )
  expect_error(
    table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = sex,
      p_digits = -1
    ),
    "positive integer"
  )
  expect_error(
    table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = sex,
      p_digits = c(2, 3)
    ),
    "positive integer"
  )
  expect_error(
    table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = sex,
      p_digits = NA
    ),
    "positive integer"
  )
})

# ---- decimal alignment ----

test_that("decimal_align_strings aligns dots in heterogeneous-magnitude column", {
  out <- spicy:::decimal_align_strings(
    c("1234.56", "0.05", "42.30"),
    decimal_mark = "."
  )
  # All values padded to identical width.
  expect_equal(length(unique(nchar(out))), 1L)
  # Decimal-point positions identical across rows.
  dot_pos <- vapply(
    out,
    function(s) regexpr(".", s, fixed = TRUE),
    integer(1)
  )
  expect_equal(length(unique(dot_pos)), 1L)
})

test_that("decimal_align_strings handles integers (no dot) as if dotted at end", {
  out <- spicy:::decimal_align_strings(
    c("1.23", "100", "12.5"),
    decimal_mark = "."
  )
  expect_equal(length(unique(nchar(out))), 1L)
})

test_that("decimal_align_strings handles blanks and NA as full-width spaces", {
  out <- spicy:::decimal_align_strings(
    c("1.23", "", NA_character_, "42.5"),
    decimal_mark = "."
  )
  expect_equal(length(unique(nchar(out))), 1L)
  # Blank/NA become whitespace-only of the right width.
  expect_true(!nzchar(trimws(out[2])))
  expect_true(!nzchar(trimws(out[3])))
})

test_that("decimal_align_strings respects European decimal mark", {
  out <- spicy:::decimal_align_strings(
    c("1,23", "0,05", "42,30"),
    decimal_mark = ","
  )
  expect_equal(length(unique(nchar(out))), 1L)
  comma_pos <- vapply(
    out,
    function(s) regexpr(",", s, fixed = TRUE),
    integer(1)
  )
  expect_equal(length(unique(comma_pos)), 1L)
})

test_that("decimal_align_strings handles bracket notation like '0.18 [0.07, 0.30]'", {
  out <- spicy:::decimal_align_strings(
    c("0.18 [0.07, 0.30]", "12.34 [10.0, 14.7]"),
    decimal_mark = "."
  )
  expect_equal(length(unique(nchar(out))), 1L)
  # First-dot positions aligned across rows.
  first_dot <- vapply(
    out,
    function(s) regexpr(".", s, fixed = TRUE),
    integer(1)
  )
  expect_equal(length(unique(first_dot)), 1L)
})

test_that("align='decimal' is the default and pads ASCII display values", {
  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    output = "long"
  )
  expect_equal(attr(out, "align"), "decimal")
})

test_that("align='decimal' produces dot-aligned numeric columns", {
  # Test the pre-padding pipeline that the ASCII print method applies:
  # build the display df, then pre-pad numeric columns. All values in
  # each numeric column must end up with identical nchar (and therefore
  # identically-positioned decimal marks).
  long <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    statistic = TRUE,
    effect_size = "g",
    effect_size_ci = TRUE,
    output = "long"
  )
  display <- spicy:::build_wide_display_df_continuous_lm(
    long,
    digits = 2L,
    fit_digits = 2L,
    effect_size_digits = 2L,
    decimal_mark = ".",
    ci_level = 0.95,
    show_statistic = TRUE,
    show_p_value = TRUE,
    show_n = TRUE,
    effect_size = "g",
    effect_size_ci = TRUE,
    r2_type = "r2",
    ci = TRUE
  )
  numeric_cols <- setdiff(seq_along(display), 1L)
  for (j in numeric_cols) {
    display[[j]] <- spicy:::decimal_align_strings(
      display[[j]],
      decimal_mark = "."
    )
  }
  for (j in numeric_cols) {
    vals <- display[[j]]
    if (all(!nzchar(trimws(vals)))) {
      next
    }
    expect_equal(
      length(unique(nchar(vals))),
      1L,
      info = sprintf(
        "Column '%s' has heterogeneous widths after decimal padding.",
        names(display)[j]
      )
    )
  }
})

test_that("align='center' restores the legacy column-by-column rendering", {
  out_decimal <- capture.output(
    table_continuous_lm(
      sochealth,
      select = c(wellbeing_score, bmi),
      by = sex
    )
  )
  out_center <- capture.output(
    table_continuous_lm(
      sochealth,
      select = c(wellbeing_score, bmi),
      by = sex,
      align = "center"
    )
  )
  # Different alignment strategies should produce different output.
  expect_false(identical(out_decimal, out_center))
})

test_that("align argument validates", {
  expect_error(
    table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = sex,
      align = "bogus"
    ),
    "should be one of"
  )
})

test_that("align='decimal' on gt and tinytable returns valid styled objects", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tinytable")
  expect_s3_class(
    table_continuous_lm(
      sochealth,
      select = c(wellbeing_score, bmi),
      by = sex,
      output = "gt"
    ),
    "gt_tbl"
  )
  expect_true(inherits(
    table_continuous_lm(
      sochealth,
      select = c(wellbeing_score, bmi),
      by = sex,
      output = "tinytable"
    ),
    "tinytable"
  ))
})

# ---- wide raw respects ci_level for contrast CI columns ----

test_that("wide raw uses ci_level-derived names for contrast CI columns", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)

  out_95 <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    ci_level = 0.95,
    output = "data.frame"
  )
  out_99 <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    ci_level = 0.99,
    output = "data.frame"
  )

  expect_true(all(c("95% CI LL", "95% CI UL") %in% names(out_95)))
  expect_false(any(c("99% CI LL", "99% CI UL") %in% names(out_95)))
  expect_true(all(c("99% CI LL", "99% CI UL") %in% names(out_99)))
  expect_false(any(c("95% CI LL", "95% CI UL") %in% names(out_99)))

  width_95 <- out_95[["95% CI UL"]][1] - out_95[["95% CI LL"]][1]
  width_99 <- out_99[["99% CI UL"]][1] - out_99[["99% CI LL"]][1]
  expect_gt(width_99, width_95)
})

test_that("wide raw column names match wide display when ci_level is custom", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)

  raw_99 <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    ci_level = 0.99,
    output = "data.frame"
  )
  long <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    ci_level = 0.99,
    output = "long"
  )
  display_99 <- spicy:::build_wide_display_df_continuous_lm(
    long,
    digits = 2L,
    fit_digits = 2L,
    effect_size_digits = 2L,
    decimal_mark = ".",
    ci_level = 0.99,
    show_statistic = FALSE,
    show_p_value = TRUE,
    show_n = TRUE,
    effect_size = "none",
    effect_size_ci = FALSE,
    r2_type = "r2",
    ci = TRUE
  )

  raw_ci_cols <- grep("CI", names(raw_99), value = TRUE)
  display_ci_cols <- grep("CI", names(display_99), value = TRUE)
  expect_setequal(raw_ci_cols, display_ci_cols)
})

# ---- categorical Wald F: tryCatch around solve(vc_sub, beta_sub) ----

test_that("compute_lm_wald_test degrades to NA on a singular submatrix", {
  fit <- stats::lm(Sepal.Length ~ Species, data = iris)
  vc <- stats::vcov(fit)
  vc[, ] <- 0
  result <- spicy:::compute_lm_wald_test(
    fit,
    coef_idx_set = 2:3,
    vc = vc,
    vcov_type = "classical"
  )
  expect_true(is.na(result$statistic))
  expect_true(is.na(result$p.value))
  expect_equal(result$test_type, "F")
})

# ---- bracket separator for European decimal mark ----

test_that("ci_bracket_separator returns ', ' for '.' and '; ' for ','", {
  expect_equal(spicy:::ci_bracket_separator("."), ", ")
  expect_equal(spicy:::ci_bracket_separator(","), "; ")
})

test_that("effect-size CI bracket separator switches with decimal_mark", {
  out_dot <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = sex,
    effect_size = "g",
    effect_size_ci = TRUE,
    decimal_mark = ".",
    output = "default"
  )
  out_comma <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = sex,
    effect_size = "g",
    effect_size_ci = TRUE,
    decimal_mark = ",",
    output = "default"
  )

  display_dot <- spicy:::build_wide_display_df_continuous_lm(
    out_dot,
    digits = 2L,
    fit_digits = 2L,
    effect_size_digits = 2L,
    decimal_mark = ".",
    ci_level = 0.95,
    show_statistic = FALSE,
    show_p_value = TRUE,
    show_n = TRUE,
    effect_size = "g",
    effect_size_ci = TRUE,
    r2_type = "r2",
    ci = TRUE
  )
  display_comma <- spicy:::build_wide_display_df_continuous_lm(
    out_comma,
    digits = 2L,
    fit_digits = 2L,
    effect_size_digits = 2L,
    decimal_mark = ",",
    ci_level = 0.95,
    show_statistic = FALSE,
    show_p_value = TRUE,
    show_n = TRUE,
    effect_size = "g",
    effect_size_ci = TRUE,
    r2_type = "r2",
    ci = TRUE
  )

  expect_match(display_dot$g[1], "^[0-9.]+ \\[[0-9.]+, [0-9.]+\\]$")
  expect_match(display_comma$g[1], "^[0-9,]+ \\[[0-9,]+; [0-9,]+\\]$")
  expect_false(grepl(";", display_dot$g[1], fixed = TRUE))
  expect_false(grepl(", ", display_comma$g[1], fixed = TRUE))
})

# ---- cluster-robust SE ----

test_that("cluster-robust vcov requires cluster + clubSandwich", {
  expect_error(
    table_continuous_lm(
      sleep,
      select = extra,
      by = group,
      vcov = "CR2"
    ),
    "requires `cluster`"
  )
  expect_error(
    table_continuous_lm(
      sleep,
      select = extra,
      by = group,
      vcov = "HC3",
      cluster = ID
    ),
    "cluster.*only used.*CR"
  )
})

test_that("cluster argument resolves columns and vectors consistently", {
  skip_if_not_installed("clubSandwich")

  # Column form via NSE.
  out_nse <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    cluster = ID,
    vcov = "CR2",
    output = "long"
  )
  # Character form.
  out_chr <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    cluster = "ID",
    vcov = "CR2",
    output = "long"
  )
  # Vector form.
  out_vec <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    cluster = sleep$ID,
    vcov = "CR2",
    output = "long"
  )

  expect_equal(out_nse$statistic, out_chr$statistic)
  expect_equal(out_nse$statistic, out_vec$statistic)
  expect_equal(out_nse$p.value, out_chr$p.value)
  expect_equal(out_nse$df2, out_chr$df2)
})

test_that("CR2 matches clubSandwich::coef_test() for binary contrast", {
  skip_if_not_installed("clubSandwich")

  out <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    cluster = ID,
    vcov = "CR2",
    output = "long"
  )
  contrast_row <- out[!is.na(out$estimate), ]

  fit <- stats::lm(extra ~ group, data = sleep)
  ct <- clubSandwich::coef_test(
    fit,
    vcov = "CR2",
    cluster = sleep$ID,
    test = "Satterthwaite"
  )

  expect_equal(contrast_row$estimate, unname(coef(fit)[2]), tolerance = 1e-8)
  expect_equal(contrast_row$estimate_se[1], ct$SE[2], tolerance = 1e-8)
  expect_equal(contrast_row$statistic[1], ct$tstat[2], tolerance = 1e-8)
  expect_equal(contrast_row$df2[1], ct$df_Satt[2], tolerance = 1e-6)
  expect_equal(contrast_row$p.value[1], ct$p_Satt[2], tolerance = 1e-8)
})

test_that("CR2 matches clubSandwich::Wald_test() for k>2 categorical", {
  skip_if_not_installed("clubSandwich")

  # Use a synthetic clustering structure independent of the predictor
  # so the cluster-robust variance estimator is well-defined.
  set.seed(20260418)
  df <- iris
  df$cluster_id <- rep(seq_len(15), each = 10)

  out <- table_continuous_lm(
    df,
    select = Sepal.Length,
    by = Species,
    cluster = cluster_id,
    vcov = "CR2",
    statistic = TRUE,
    output = "long"
  )
  test_row <- out[1L, ]

  fit <- stats::lm(Sepal.Length ~ Species, data = df)
  wt <- clubSandwich::Wald_test(
    fit,
    constraints = clubSandwich::constrain_zero(2:3),
    vcov = "CR2",
    cluster = df$cluster_id,
    test = "HTZ"
  )

  expect_equal(test_row$statistic, wt$Fstat[1], tolerance = 1e-6)
  expect_equal(test_row$df1, as.integer(wt$df_num[1]))
  expect_equal(test_row$df2, wt$df_denom[1], tolerance = 1e-6)
  expect_equal(test_row$p.value, wt$p_val[1], tolerance = 1e-8)
})

test_that("effect sizes are invariant to vcov = 'CR2'", {
  skip_if_not_installed("clubSandwich")
  fit_classical <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    vcov = "classical",
    effect_size = "g",
    effect_size_ci = TRUE,
    output = "long"
  )
  fit_cr2 <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    vcov = "CR2",
    cluster = ID,
    effect_size = "g",
    effect_size_ci = TRUE,
    output = "long"
  )
  # ES point estimate identical (depends only on OLS fit).
  expect_equal(
    fit_classical$es_value[!is.na(fit_classical$es_value)],
    fit_cr2$es_value[!is.na(fit_cr2$es_value)]
  )
  # ES CI also identical (does not depend on vcov).
  expect_equal(
    fit_classical$es_ci_lower[!is.na(fit_classical$es_ci_lower)],
    fit_cr2$es_ci_lower[!is.na(fit_cr2$es_ci_lower)]
  )
})

test_that("CR2 df2 differs from classical df.residual (cluster-aware df)", {
  skip_if_not_installed("clubSandwich")
  long_classical <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    output = "long"
  )
  long_cr2 <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    cluster = ID,
    vcov = "CR2",
    output = "long"
  )
  classical_df2 <- long_classical$df2[!is.na(long_classical$df2)]
  cr2_df2 <- long_cr2$df2[!is.na(long_cr2$df2)]
  # CR2 SW-df is generally smaller than the classical n - p df, since
  # it reflects cluster-level rather than observation-level
  # information.
  expect_true(all(cr2_df2 < classical_df2))
})

test_that("get_test_header_lm formats fractional df with one decimal", {
  long <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    output = "long"
  )
  long$df2 <- 45.32
  long$df1 <- 1L
  hdr <- spicy:::get_test_header_lm(long, show_statistic = TRUE, exact = TRUE)
  expect_equal(hdr, "t(45.3)")

  long$df2 <- 45.0
  hdr_int <- spicy:::get_test_header_lm(long, show_statistic = TRUE, exact = TRUE)
  expect_equal(hdr_int, "t(45)")
})

test_that("cluster validation: must have nrow(data) length, no all-NA, etc.", {
  skip_if_not_installed("clubSandwich")
  expect_error(
    table_continuous_lm(
      sleep,
      select = extra,
      by = group,
      cluster = c(1, 2, 3),
      vcov = "CR2"
    ),
    "Cluster `cluster` must have length"
  )
  # Single distinct cluster value -> error.
  expect_error(
    table_continuous_lm(
      sleep,
      select = extra,
      by = group,
      cluster = rep(1, nrow(sleep)),
      vcov = "CR2"
    ),
    "at least two distinct"
  )
})

# ---- end cluster-robust ----

# ---- bootstrap and jackknife SE ----

test_that("bootstrap produces a finite vcov on a binary predictor", {
  set.seed(20260418)
  out <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    vcov = "bootstrap",
    boot_n = 200,
    output = "long"
  )
  contrast_row <- out[!is.na(out$estimate), ]
  expect_true(is.finite(contrast_row$estimate_se[1]))
  expect_true(is.finite(contrast_row$statistic[1]))
  expect_true(is.finite(contrast_row$p.value[1]))
  expect_equal(contrast_row$test_type[1], "z")
  expect_equal(contrast_row$df2[1], Inf)
})

test_that("cluster bootstrap differs from obs bootstrap on paired data", {
  set.seed(20260418)
  obs_boot <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    vcov = "bootstrap",
    boot_n = 200,
    output = "long"
  )
  set.seed(20260418)
  cl_boot <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    cluster = ID,
    vcov = "bootstrap",
    boot_n = 200,
    output = "long"
  )
  obs_se <- obs_boot$estimate_se[!is.na(obs_boot$estimate_se)]
  cl_se <- cl_boot$estimate_se[!is.na(cl_boot$estimate_se)]
  # Cluster bootstrap accounts for within-subject correlation; on
  # the paired sleep data this typically tightens the contrast SE.
  expect_lt(cl_se, obs_se)
})

test_that("jackknife matches the closed-form formula on a binary predictor", {
  out <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    vcov = "jackknife",
    output = "long"
  )
  contrast_row <- out[!is.na(out$estimate), ]

  # Closed-form jackknife: ((n - 1) / n) * sum((beta_i - beta_bar)^2)
  fit <- stats::lm(extra ~ group, data = sleep)
  beta_jack <- vapply(
    seq_len(nrow(sleep)),
    function(i) {
      stats::coef(stats::lm(extra ~ group, data = sleep[-i, ]))[2]
    },
    numeric(1)
  )
  beta_bar <- mean(beta_jack)
  n <- length(beta_jack)
  jack_var <- ((n - 1) / n) * sum((beta_jack - beta_bar)^2)
  expect_equal(contrast_row$estimate_se[1], sqrt(jack_var), tolerance = 1e-8)
})

test_that("cluster jackknife is leave-one-cluster-out on the paired data", {
  out <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    cluster = ID,
    vcov = "jackknife",
    output = "long"
  )
  contrast_row <- out[!is.na(out$estimate), ]

  unique_ids <- unique(sleep$ID)
  beta_jack <- vapply(
    unique_ids,
    function(g) {
      keep <- sleep$ID != g
      stats::coef(stats::lm(extra ~ group, data = sleep[keep, ]))[2]
    },
    numeric(1)
  )
  beta_bar <- mean(beta_jack)
  G <- length(beta_jack)
  jack_var <- ((G - 1) / G) * sum((beta_jack - beta_bar)^2)
  expect_equal(contrast_row$estimate_se[1], sqrt(jack_var), tolerance = 1e-8)
})

test_that("bootstrap on 3-level categorical produces chi2 header", {
  set.seed(20260418)
  out <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Species,
    vcov = "bootstrap",
    boot_n = 200,
    statistic = TRUE,
    output = "long"
  )
  test_row <- out[1L, ]
  expect_equal(test_row$test_type, "chi2")
  expect_equal(test_row$df1, 2L)
  expect_equal(test_row$df2, Inf)

  hdr <- spicy:::get_test_header_lm(out, show_statistic = TRUE, exact = TRUE)
  expect_match(hdr, "^χ²\\(2\\)$")
})

test_that("bootstrap and jackknife leave the effect sizes unchanged", {
  set.seed(20260418)
  out_class <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    effect_size = "g",
    effect_size_ci = TRUE,
    output = "long"
  )
  set.seed(20260418)
  out_boot <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    vcov = "bootstrap",
    boot_n = 200,
    effect_size = "g",
    effect_size_ci = TRUE,
    output = "long"
  )
  out_jack <- table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    vcov = "jackknife",
    effect_size = "g",
    effect_size_ci = TRUE,
    output = "long"
  )
  expect_equal(out_class$es_value, out_boot$es_value)
  expect_equal(out_class$es_value, out_jack$es_value)
  expect_equal(out_class$es_ci_lower, out_boot$es_ci_lower)
  expect_equal(out_class$es_ci_lower, out_jack$es_ci_lower)
})

test_that("multi-way clustering errors with a clear message", {
  expect_error(
    table_continuous_lm(
      iris,
      select = Sepal.Length,
      by = Species,
      cluster = list(rep(1:3, 50), rep(1:5, 30)),
      vcov = "CR2"
    ),
    "Multi-way clustering"
  )
  expect_error(
    table_continuous_lm(
      iris,
      select = Sepal.Length,
      by = Species,
      cluster = data.frame(c1 = rep(1:3, 50), c2 = rep(1:5, 30)),
      vcov = "CR2"
    ),
    "Multi-way clustering"
  )
})

test_that("boot_n validates as a positive integer >= 50", {
  expect_error(
    table_continuous_lm(
      sleep,
      select = extra,
      by = group,
      vcov = "bootstrap",
      boot_n = 10
    ),
    "boot_n.*>= 50"
  )
  expect_error(
    table_continuous_lm(
      sleep,
      select = extra,
      by = group,
      vcov = "bootstrap",
      boot_n = NA
    ),
    "boot_n"
  )
  expect_error(
    table_continuous_lm(
      sleep,
      select = extra,
      by = group,
      vcov = "bootstrap",
      boot_n = c(100, 200)
    ),
    "boot_n"
  )
})

# ---- end bootstrap and jackknife ----

# ---- additional coverage: SE helpers and edge cases ----

test_that("compute_lm_vcov_bootstrap reproducibility and weighted refit", {
  set.seed(20260418)
  fit <- stats::lm(extra ~ group, data = sleep)
  set.seed(20260418)
  vc1 <- spicy:::compute_lm_vcov_bootstrap(fit, boot_n = 100)
  set.seed(20260418)
  vc2 <- spicy:::compute_lm_vcov_bootstrap(fit, boot_n = 100)
  expect_equal(vc1, vc2)
  expect_true(is.matrix(vc1))
  expect_equal(dim(vc1), c(2L, 2L))

  # Weighted bootstrap path
  w <- stats::runif(nrow(sleep), 0.5, 1.5)
  fit_w <- stats::lm(extra ~ group, data = sleep, weights = w)
  set.seed(20260418)
  vc_w <- spicy:::compute_lm_vcov_bootstrap(fit_w, boot_n = 100, weights = w)
  expect_true(is.matrix(vc_w))
  expect_equal(dim(vc_w), c(2L, 2L))
})

test_that("compute_lm_vcov_jackknife reproduces the leave-one-out variance", {
  fit <- stats::lm(extra ~ group, data = sleep)
  vc <- spicy:::compute_lm_vcov_jackknife(fit)
  expect_true(is.matrix(vc))
  expect_equal(dim(vc), c(2L, 2L))
  # Closed-form jackknife for the slope
  jacks <- vapply(seq_len(nrow(sleep)), function(i) {
    stats::coef(stats::lm(extra ~ group, data = sleep[-i, ]))[2]
  }, numeric(1))
  jack_var <- ((length(jacks) - 1) / length(jacks)) * sum((jacks - mean(jacks))^2)
  expect_equal(vc[2, 2], jack_var, tolerance = 1e-8)
})

test_that("compute_lm_vcov dispatches by type and validates unknowns", {
  fit <- stats::lm(extra ~ group, data = sleep)
  expect_equal(
    spicy:::compute_lm_vcov(fit, "classical"),
    stats::vcov(fit)
  )
  vc_boot <- spicy:::compute_lm_vcov(fit, "bootstrap", boot_n = 100)
  expect_true(is.matrix(vc_boot))
  vc_jack <- spicy:::compute_lm_vcov(fit, "jackknife")
  expect_true(is.matrix(vc_jack))
  expect_error(
    spicy:::compute_lm_vcov(fit, "BOGUS"),
    "Unknown `vcov` type"
  )
  expect_error(
    spicy:::compute_lm_vcov(fit, "CR2"),
    "requires `cluster`"
  )
})

test_that("resolve_cluster_argument rejects malformed input with clear errors", {
  data <- data.frame(y = 1:10, x = 1:10)

  # Multi-way (list / data.frame) rejected.
  expect_error(
    spicy:::resolve_cluster_argument(
      rlang::quo(list(a = 1:10, b = 1:10)),
      data,
      "cluster"
    ),
    "Multi-way clustering"
  )
  # Wrong length.
  expect_error(
    spicy:::resolve_cluster_argument(
      rlang::quo(c(1, 2, 3)),
      data,
      "cluster"
    ),
    "must have length"
  )
  # Non-atomic non-list.
  expect_error(
    spicy:::resolve_cluster_argument(
      rlang::quo(stats::lm(y ~ x, data)),
      data,
      "cluster"
    ),
    "Multi-way clustering|atomic"
  )
})

test_that("get_test_header_lm covers all test_type branches", {
  # numeric predictor with classical vcov -> "t(df)"
  out_num <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = age,
    statistic = TRUE,
    output = "long"
  )
  expect_match(
    spicy:::get_test_header_lm(out_num, TRUE, TRUE),
    "^t\\("
  )
  # numeric predictor with jackknife -> "z"
  out_num_z <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = age,
    vcov = "jackknife",
    statistic = TRUE,
    output = "long"
  )
  expect_equal(spicy:::get_test_header_lm(out_num_z, TRUE, TRUE), "z")

  # k>2 categorical with classical -> "F(df1, df2)"
  out_kgt2 <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Species,
    statistic = TRUE,
    output = "long"
  )
  expect_match(
    spicy:::get_test_header_lm(out_kgt2, TRUE, TRUE),
    "^F\\("
  )

  # k>2 categorical with bootstrap -> "χ²(df1)"
  set.seed(20260418)
  out_kgt2_chi2 <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Species,
    vcov = "bootstrap",
    boot_n = 100,
    statistic = TRUE,
    output = "long"
  )
  expect_match(
    spicy:::get_test_header_lm(out_kgt2_chi2, TRUE, TRUE),
    "^χ²\\("
  )

  # show_statistic = FALSE -> NULL
  expect_null(spicy:::get_test_header_lm(out_kgt2, FALSE, TRUE))
})

test_that("compute_lm_vcov_bootstrap warns on too few valid replicates", {
  # Fit before mocking so the original fit succeeds; the mock then
  # blocks every refit inside compute_lm_vcov_bootstrap.
  fit <- stats::lm(extra ~ group, data = sleep)
  testthat::local_mocked_bindings(
    lm = function(...) stop("synthetic refit failure"),
    .package = "stats"
  )
  msg <- tryCatch(
    spicy:::compute_lm_vcov_bootstrap(fit, boot_n = 100),
    warning = function(w) conditionMessage(w)
  )
  expect_true(is.character(msg))
  expect_match(msg, "replicates were valid")
})

# ---- end additional coverage ----

# ---- coverage: more SE error paths and helper branches ----

test_that("compute_lm_vcov_bootstrap cluster path produces a finite vcov", {
  set.seed(20260418)
  fit <- stats::lm(extra ~ group, data = sleep)
  vc <- spicy:::compute_lm_vcov_bootstrap(
    fit,
    cluster = sleep$ID,
    boot_n = 100
  )
  expect_true(is.matrix(vc))
  expect_equal(dim(vc), c(2L, 2L))
  expect_true(all(is.finite(vc)))
})

test_that("compute_lm_vcov_jackknife cluster path matches leave-one-out", {
  fit <- stats::lm(extra ~ group, data = sleep)
  vc_obs <- spicy:::compute_lm_vcov_jackknife(fit)
  vc_cl <- spicy:::compute_lm_vcov_jackknife(fit, cluster = sleep$ID)
  expect_true(is.matrix(vc_obs))
  expect_true(is.matrix(vc_cl))
  expect_false(isTRUE(all.equal(vc_obs, vc_cl)))
})

test_that("compute_lm_vcov_jackknife falls back when too few replicates", {
  fit <- stats::lm(extra ~ group, data = sleep)
  testthat::local_mocked_bindings(
    lm = function(...) stop("synthetic refit failure"),
    .package = "stats"
  )
  msg <- tryCatch(
    spicy:::compute_lm_vcov_jackknife(fit),
    warning = function(w) conditionMessage(w)
  )
  expect_match(msg, "fewer than 2 valid")
})

test_that("compute_lm_vcov errors for CR* without cluster and clubSandwich", {
  fit <- stats::lm(extra ~ group, data = sleep)
  expect_error(
    spicy:::compute_lm_vcov(fit, type = "CR2"),
    "requires `cluster`"
  )
})

test_that("compute_lm_vcov CR fallback warns when clubSandwich errors", {
  fit <- stats::lm(extra ~ group, data = sleep)
  testthat::local_mocked_bindings(
    vcovCR = function(...) stop("synthetic CR failure"),
    .package = "clubSandwich"
  )
  msg <- tryCatch(
    spicy:::compute_lm_vcov(fit, type = "CR2", cluster = sleep$ID),
    warning = function(w) conditionMessage(w)
  )
  expect_true(is.character(msg))
  expect_match(msg, "Cluster-robust")
  expect_match(msg, "synthetic CR failure")
})

test_that("compute_lm_coef_inference falls back when clubSandwich coef_test errors", {
  fit <- stats::lm(extra ~ group, data = sleep)
  vc <- clubSandwich::vcovCR(fit, type = "CR2", cluster = sleep$ID)
  testthat::local_mocked_bindings(
    coef_test = function(...) stop("synthetic coef_test failure"),
    .package = "clubSandwich"
  )
  out <- spicy:::compute_lm_coef_inference(
    fit,
    coef_idx = 2L,
    vc = vc,
    vcov_type = "CR2",
    cluster = sleep$ID
  )
  # Falls back to df.residual + supplied vcov; test_type stays "t".
  expect_equal(out$test_type, "t")
  expect_true(is.finite(out$estimate))
  expect_true(is.finite(out$se))
})

test_that("compute_lm_wald_test falls back when clubSandwich Wald_test errors", {
  fit <- stats::lm(Sepal.Length ~ Species, data = iris)
  vc <- clubSandwich::vcovCR(fit, type = "CR2", cluster = iris$Species)
  testthat::local_mocked_bindings(
    Wald_test = function(...) stop("synthetic Wald_test failure"),
    .package = "clubSandwich"
  )
  out <- spicy:::compute_lm_wald_test(
    fit,
    coef_idx_set = 2:3,
    vc = vc,
    vcov_type = "CR2",
    cluster = iris$Species
  )
  expect_equal(out$test_type, "F")
  expect_true(is.finite(out$statistic))
})

test_that("compute_lm_wald_test handles q == 0", {
  fit <- stats::lm(extra ~ group, data = sleep)
  out <- spicy:::compute_lm_wald_test(
    fit,
    coef_idx_set = integer(0),
    vc = stats::vcov(fit),
    vcov_type = "classical"
  )
  expect_true(is.na(out$statistic))
  expect_true(is.na(out$df1))
})

test_that("resolve_cluster_argument NULL roundtrip", {
  data <- data.frame(y = 1:5, x = 1:5)
  expect_null(
    spicy:::resolve_cluster_argument(rlang::quo(NULL), data, "cluster")
  )
})

test_that("decimal_align_strings handles empty input and single value", {
  expect_equal(spicy:::decimal_align_strings(character(0)), character(0))
  out <- spicy:::decimal_align_strings("3.14", decimal_mark = ".")
  expect_equal(out, "3.14")
  # All blank
  out_blank <- spicy:::decimal_align_strings(c("", NA, " "))
  expect_equal(length(out_blank), 3L)
})

test_that("table_continuous_lm errors clearly on bad cluster + vcov combinations", {
  expect_error(
    table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = sex,
      cluster = sex,
      vcov = "HC3"
    ),
    "cluster.*only used"
  )
  expect_error(
    table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = sex,
      cluster = rep(1, nrow(sochealth)),
      vcov = "CR2"
    ),
    "at least two distinct"
  )
})

# ---- end coverage extras ----

# ---- coverage: noncentral inversion edge cases and small helpers ----

test_that("find_ncp_t_lm expands the bracket on extreme statistics", {
  # Initial bracket [-50, 50] doesn't contain the root for very large
  # |t|; the helper widens up to 6 times. With t = 200 the root is
  # near 200, so bracket expansion is required.
  ncp <- spicy:::find_ncp_t_lm(t_obs = 200, df = 10000, p = 0.5)
  expect_true(is.finite(ncp))
  # pt is monotonically decreasing in ncp at fixed t_obs, so this
  # inversion converges; verify the relation.
  expect_equal(stats::pt(200, df = 10000, ncp = ncp), 0.5, tolerance = 1e-3)
})

test_that("find_ncp_f_lm returns 0 when ncp = 0 already attains the target tail", {
  # F = 0 -> pf(0, df1, df2, 0) = 0 < p, so the helper returns 0.
  expect_equal(spicy:::find_ncp_f_lm(0, df1 = 2, df2 = 30, p = 0.025), 0)
})

test_that("find_ncp_f_lm widens the bracket on huge F statistics", {
  ncp <- spicy:::find_ncp_f_lm(f_obs = 5000, df1 = 1, df2 = 100, p = 0.5)
  expect_true(is.finite(ncp))
  expect_gte(ncp, 0)
})

test_that("format_p_value covers blanks, NAs, and edge digits", {
  expect_equal(spicy:::format_p_value(NA_real_), "")
  # digits = 1L is admissible
  expect_match(spicy:::format_p_value(0.05, ".", 1L), "^[<.0-9]+$")
})

test_that("get_test_header_lm: show_statistic = FALSE returns NULL early", {
  block <- data.frame(
    test_type = "t",
    df1 = 1L,
    df2 = 30,
    predictor_type = "continuous",
    level = NA_character_,
    estimate = 1
  )
  expect_null(spicy:::get_test_header_lm(block, show_statistic = FALSE))
})

test_that("compute_es_ci_lm fast-paths to NA on effect_size = 'none'", {
  fit <- stats::lm(extra ~ group, data = sleep)
  out <- spicy:::compute_es_ci_lm(fit, "none", 0.95)
  expect_equal(out, c(NA_real_, NA_real_))
})

test_that("compute_lm_omega2 returns NA when sums of squares are degenerate", {
  # Constant outcome -> SS_total = 0 -> omega2 not defined.
  df <- data.frame(y = rep(5, 10), x = factor(rep(c("A", "B"), each = 5)))
  fit <- stats::lm(y ~ x, data = df)
  expect_equal(spicy:::compute_lm_omega2(fit, df_effect = 1L, df_resid = 8L), NA_real_)
  # Negative df_effect rejected.
  expect_equal(spicy:::compute_lm_omega2(fit, df_effect = 0L, df_resid = 8L), NA_real_)
  # Negative df_resid rejected.
  expect_equal(spicy:::compute_lm_omega2(fit, df_effect = 1L, df_resid = 0L), NA_real_)
})

# ---- end coverage edge cases ----

# ---- internal CI helpers ----

test_that("find_ncp_t_lm inverts pt() correctly", {
  ncp <- spicy:::find_ncp_t_lm(t_obs = 2.5, df = 30, p = 0.5)
  expect_equal(stats::pt(2.5, df = 30, ncp = ncp), 0.5, tolerance = 1e-6)

  ncp_lo <- spicy:::find_ncp_t_lm(t_obs = 2.5, df = 30, p = 0.975)
  expect_equal(
    stats::pt(2.5, df = 30, ncp = ncp_lo),
    0.975,
    tolerance = 1e-6
  )

  ncp_hi <- spicy:::find_ncp_t_lm(t_obs = 2.5, df = 30, p = 0.025)
  expect_equal(
    stats::pt(2.5, df = 30, ncp = ncp_hi),
    0.025,
    tolerance = 1e-6
  )
  expect_lt(ncp_lo, ncp_hi)
})

test_that("find_ncp_t_lm returns NA for invalid inputs", {
  expect_true(is.na(spicy:::find_ncp_t_lm(NA_real_, 10, 0.5)))
  expect_true(is.na(spicy:::find_ncp_t_lm(2, NA_real_, 0.5)))
  expect_true(is.na(spicy:::find_ncp_t_lm(2, 0, 0.5)))
  expect_true(is.na(spicy:::find_ncp_t_lm(2, 10, 0)))
  expect_true(is.na(spicy:::find_ncp_t_lm(2, 10, 1)))
})

test_that("find_ncp_f_lm inverts pf() correctly and respects ncp >= 0", {
  ncp <- spicy:::find_ncp_f_lm(f_obs = 5, df1 = 2, df2 = 30, p = 0.5)
  expect_gte(ncp, 0)
  expect_equal(
    stats::pf(5, df1 = 2, df2 = 30, ncp = ncp),
    0.5,
    tolerance = 1e-6
  )

  ncp_hi <- spicy:::find_ncp_f_lm(f_obs = 5, df1 = 2, df2 = 30, p = 0.025)
  expect_gte(ncp_hi, 0)
  expect_equal(
    stats::pf(5, df1 = 2, df2 = 30, ncp = ncp_hi),
    0.025,
    tolerance = 1e-6
  )

  expect_equal(
    spicy:::find_ncp_f_lm(f_obs = 0.001, df1 = 2, df2 = 30, p = 0.975),
    0
  )
})

test_that("find_ncp_f_lm returns NA for invalid inputs", {
  expect_true(is.na(spicy:::find_ncp_f_lm(NA_real_, 2, 30, 0.5)))
  expect_true(is.na(spicy:::find_ncp_f_lm(-1, 2, 30, 0.5)))
  expect_true(is.na(spicy:::find_ncp_f_lm(5, 0, 30, 0.5)))
  expect_true(is.na(spicy:::find_ncp_f_lm(5, 2, 0, 0.5)))
  expect_true(is.na(spicy:::find_ncp_f_lm(5, 2, 30, 0)))
})

test_that("compute_es_ci_lm dispatches and the strict fallback rejects unknown effect_size", {
  fit <- stats::lm(Sepal.Length ~ Species, data = iris)

  expect_equal(
    spicy:::compute_es_ci_lm(fit, "none", 0.95),
    c(NA_real_, NA_real_)
  )
  expect_equal(
    length(spicy:::compute_es_ci_lm(fit, "f2", 0.95)),
    2L
  )
  expect_equal(
    length(spicy:::compute_es_ci_lm(fit, "omega2", 0.95)),
    2L
  )
  expect_error(
    spicy:::compute_es_ci_lm(fit, "bogus", 0.95),
    "Unknown `effect_size`"
  )
})

test_that("pick_es_value_lm fails fast on unknown effect_size", {
  ms <- list(f2 = 1, d = 0.5, g = 0.45, omega2 = 0.2)
  expect_error(
    spicy:::pick_es_value_lm(ms, "bogus"),
    "Unknown `effect_size`"
  )
  expect_equal(spicy:::pick_es_value_lm(ms, "none"), NA_real_)
  expect_equal(spicy:::pick_es_value_lm(ms, "f2"), 1)
  expect_equal(spicy:::pick_es_value_lm(ms, "g"), 0.45)
})

test_that("compute_lm_vcov falls back to classical vcov when sandwich errors", {
  # The defensive fallback in compute_lm_vcov (warn + return classical
  # vcov) is reached only when sandwich::vcovHC() itself errors. We
  # mock a failure to verify the fallback path emits a clear sprintf
  # warning and returns a usable (possibly-NA) matrix.
  testthat::local_mocked_bindings(
    vcovHC = function(x, type, ...) stop("synthetic test failure"),
    .package = "sandwich"
  )

  fit <- stats::lm(mpg ~ wt, data = mtcars)
  msg <- tryCatch(
    spicy:::compute_lm_vcov(fit, "HC4m"),
    warning = function(w) conditionMessage(w)
  )

  expect_true(is.character(msg))
  expect_match(msg, "Robust `vcov = \"HC4m\"`")
  expect_match(msg, "synthetic test failure")
  # cli-style bullet wording: "Falling back to the classical OLS variance"
  expect_match(msg, "Falling back to the classical OLS variance")
})

test_that("compute_lm_vcov matches sandwich::vcovHC numerically", {
  fit <- stats::lm(mpg ~ wt + cyl, data = mtcars)
  for (type in c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")) {
    expect_equal(
      spicy:::compute_lm_vcov(fit, type),
      sandwich::vcovHC(fit, type = type),
      info = paste0("vcov type = ", type)
    )
  }
})

# ---- end internal CI helpers ----

# ---- end effect sizes ----

test_that("table_continuous_lm clipboard output can be exercised with a mocked writer", {
  skip_if_not_installed("clipr")

  captured <- NULL

  local_mocked_bindings(
    write_clip = function(text, ...) {
      captured <<- text
      invisible(text)
    },
    .package = "clipr"
  )

  expect_message(
    out <- table_continuous_lm(
      sochealth,
      select = c(wellbeing_score, bmi),
      by = sex,
      statistic = TRUE,
      output = "clipboard",
      clipboard_delim = ";"
    ),
    "copied to clipboard"
  )

  expect_s3_class(out, "data.frame")
  expect_true(is.character(captured))
  expect_match(
    captured,
    "Variable;M \\(Female\\);M \\(Male\\);Δ \\(Male - Female\\)"
  )
  expect_match(captured, "95% CI;95% CI")
  expect_match(captured, "LL;UL")
  expect_match(captured, "<\\.001|<,001")
})

# ---- coverage: resolve_cluster_argument error paths ----

test_that("resolve_cluster_argument rejects unevaluable expressions", {
  data <- data.frame(y = 1:5, x = 1:5)
  q <- rlang::quo(this_object_does_not_exist_anywhere)
  expect_error(
    spicy:::resolve_cluster_argument(q, data, "cluster"),
    "must be NULL"
  )
})

test_that("resolve_cluster_argument rejects multi-way list / data.frame", {
  data <- data.frame(y = 1:5, x = 1:5, g1 = 1:5, g2 = letters[1:5])
  q <- rlang::quo(list(g1, g2))
  expect_error(
    spicy:::resolve_cluster_argument(q, data, "cluster"),
    "Multi-way clustering"
  )
  q2 <- rlang::quo(data.frame(g1 = 1:5, g2 = 1:5))
  expect_error(
    spicy:::resolve_cluster_argument(q2, data, "cluster"),
    "Multi-way clustering"
  )
})

test_that("resolve_cluster_argument rejects non-atomic resolved value", {
  data <- data.frame(y = 1:5, x = 1:5)
  # An S4 / non-atomic non-list value is unusual, but a function or
  # environment is non-atomic and not a list.
  q <- rlang::quo(stats::median)
  expect_error(
    spicy:::resolve_cluster_argument(q, data, "cluster"),
    "must be NULL|atomic"
  )
})

# ---- coverage: compute_lm_vcov dispatch ----

test_that("compute_lm_vcov errors clearly on unknown vcov type", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  expect_error(
    spicy:::compute_lm_vcov(fit, type = "BOGUS"),
    "Unknown `vcov` type"
  )
})

test_that("compute_lm_vcov simulates clubSandwich missing for CR types", {
  fit <- stats::lm(extra ~ group, data = sleep)
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "clubSandwich")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_error(
    spicy:::compute_lm_vcov(
      fit,
      type = "CR2",
      cluster = sleep$ID
    ),
    "clubSandwich"
  )
})

# ---- coverage: bootstrap warnings ----

test_that("compute_lm_vcov_bootstrap warns when fewer than 10 valid replicates", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  # Force every refit to fail by mocking lm so 0 replicates succeed.
  testthat::local_mocked_bindings(
    lm = function(...) stop("synthetic lm failure"),
    .package = "stats"
  )
  msg <- tryCatch(
    spicy:::compute_lm_vcov_bootstrap(fit, boot_n = 20L),
    warning = function(w) conditionMessage(w)
  )
  expect_match(msg, "only 0 / 20 replicates were valid")
  expect_match(msg, "unreliable")
})

test_that("compute_lm_vcov_bootstrap warns when over half of replicates fail", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  real_lm <- stats::lm
  call_count <- 0L
  # Make ~75% of bootstrap refits fail (still leave > 10 valid so we
  # exercise the n_valid < boot_n/2 warning branch, not the < 10 branch).
  testthat::local_mocked_bindings(
    lm = function(...) {
      call_count <<- call_count + 1L
      if (call_count %% 4L == 0L) {
        return(real_lm(...))
      }
      stop("synthetic lm failure")
    },
    .package = "stats"
  )
  withr::with_seed(123, {
    msg <- tryCatch(
      spicy:::compute_lm_vcov_bootstrap(fit, boot_n = 60L),
      warning = function(w) conditionMessage(w)
    )
  })
  expect_match(msg, "replicates failed")
  expect_match(msg, "60")
})

# ---- coverage: get_test_header_lm edge cases ----

test_that("get_test_header_lm returns NULL when test_type column is all NA", {
  block <- data.frame(
    test_type = NA_character_,
    df1 = NA_integer_,
    df2 = NA_real_,
    predictor_type = "continuous",
    level = NA_character_,
    estimate = NA_real_
  )
  expect_null(spicy:::get_test_header_lm(block))
})

test_that("get_test_header_lm returns plain z / chi^2 / t / F when df not available", {
  # z asymptotic
  block_z <- data.frame(
    test_type = "z", df1 = 1L, df2 = NA_real_,
    predictor_type = "continuous", level = NA_character_,
    estimate = 1
  )
  expect_equal(spicy:::get_test_header_lm(block_z), "z")

  # chi^2 with no df1
  block_c <- data.frame(
    test_type = "chi2", df1 = NA_integer_, df2 = NA_real_,
    predictor_type = "categorical", level = c("a", "b"),
    estimate = c(NA, 1)
  )
  expect_equal(spicy:::get_test_header_lm(block_c), "χ²")

  # chi^2 with exact = FALSE -> bare "χ²"
  block_c2 <- data.frame(
    test_type = "chi2", df1 = 2L, df2 = NA_real_,
    predictor_type = "categorical", level = c("a", "b"),
    estimate = c(NA, 1)
  )
  expect_equal(
    spicy:::get_test_header_lm(block_c2, exact = FALSE),
    "χ²"
  )

  # t with no df2
  block_t <- data.frame(
    test_type = "t", df1 = 1L, df2 = NA_real_,
    predictor_type = "continuous", level = NA_character_,
    estimate = 1
  )
  expect_equal(spicy:::get_test_header_lm(block_t), "t")

  # F with no df1/df2
  block_f <- data.frame(
    test_type = "F", df1 = NA_integer_, df2 = NA_real_,
    predictor_type = "categorical", level = c("a", "b"),
    estimate = c(NA, 1)
  )
  expect_equal(spicy:::get_test_header_lm(block_f), "F")
})

test_that("get_test_header_lm returns the raw test_type for unknown labels", {
  block <- data.frame(
    test_type = "weirdo",
    df1 = NA_integer_, df2 = NA_real_,
    predictor_type = "continuous",
    level = NA_character_,
    estimate = 1
  )
  out <- spicy:::get_test_header_lm(block)
  expect_equal(out, "weirdo")
})

test_that("format_effect_size_header_lm passes through unknown labels", {
  expect_equal(spicy:::format_effect_size_header_lm("custom"), "custom")
  expect_equal(spicy:::format_effect_size_header_lm("d"), "d")
  expect_equal(spicy:::format_effect_size_header_lm("g"), "g")
})

# ---- coverage: omega2 / smd CI degenerate paths ----

test_that("compute_lm_omega2 returns NA when y is non-numeric", {
  # Inject a fake fit where model.response is non-numeric. We do this
  # by mocking model.response.
  fit <- stats::lm(extra ~ group, data = sleep)
  testthat::local_mocked_bindings(
    model.response = function(...) c("a", "b", "c"),
    .package = "stats"
  )
  expect_equal(
    spicy:::compute_lm_omega2(fit, df_effect = 1L, df_resid = 18L),
    NA_real_
  )
})

test_that("compute_smd_ci_lm returns NA when slope is not finite or x not factor", {
  # Numeric predictor (not a factor) -> NA
  fit_num <- stats::lm(mpg ~ wt, data = mtcars)
  out <- spicy:::compute_smd_ci_lm(fit_num, ci_level = 0.95, hedges_correct = FALSE)
  expect_equal(out, c(NA_real_, NA_real_))

  # Factor predictor with > 2 levels -> NA
  fit_multi <- stats::lm(Sepal.Length ~ Species, data = iris)
  out_multi <- spicy:::compute_smd_ci_lm(
    fit_multi,
    ci_level = 0.95,
    hedges_correct = FALSE
  )
  expect_equal(out_multi, c(NA_real_, NA_real_))
})

test_that("extract_lm_f_stat returns NULL for an intercept-only fit", {
  df <- data.frame(y = c(1, 2, 3, 4, 5))
  fit <- stats::lm(y ~ 1, data = df)
  expect_null(spicy:::extract_lm_f_stat(fit))
})

test_that("compute_omega2_ci_lm and compute_f2_ci_lm return NA when F-stat is missing", {
  df <- data.frame(y = c(1, 2, 3, 4, 5))
  fit <- stats::lm(y ~ 1, data = df)
  expect_equal(
    spicy:::compute_omega2_ci_lm(fit, ci_level = 0.95),
    c(NA_real_, NA_real_)
  )
  expect_equal(
    spicy:::compute_f2_ci_lm(fit, ci_level = 0.95),
    c(NA_real_, NA_real_)
  )
})

# ---- coverage: build_wide_display_df NA branches ----

test_that("table_continuous_lm renders empty cells when n / weighted_n are NA", {
  # Trigger empty-row path: an outcome with < 2 valid observations.
  df <- data.frame(
    y_short = c(1, NA, NA, NA, NA, NA),
    g = factor(rep(c("a", "b"), 3))
  )
  out <- table_continuous_lm(
    df,
    select = y_short,
    by = g,
    output = "data.frame",
    show_weighted_n = FALSE
  )
  expect_s3_class(out, "data.frame")
  # n column for empty row is "" rather than a number
  expect_true("n" %in% names(out))
})

test_that("table_continuous_lm with show_weighted_n = TRUE renders blank weighted_n on empty rows", {
  df <- data.frame(
    y_short = c(1, NA, NA, NA, NA, NA),
    g = factor(rep(c("a", "b"), 3)),
    w = c(1, 1, 1, 1, 1, 1)
  )
  out <- table_continuous_lm(
    df,
    select = y_short,
    by = g,
    weights = w,
    output = "data.frame",
    show_weighted_n = TRUE
  )
  expect_s3_class(out, "data.frame")
  expect_true("Weighted n" %in% names(out))
})

# ---- coverage: export engines (skip if Suggests not installed) ----

test_that("table_continuous_lm gt output is rendered (decimal align default)", {
  skip_if_not_installed("gt")
  out <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = sex,
    output = "gt"
  )
  expect_s3_class(out, "gt_tbl")
})

test_that("table_continuous_lm gt output respects align = 'center' / 'right' / 'auto'", {
  skip_if_not_installed("gt")
  for (al in c("center", "right", "auto")) {
    out <- table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = sex,
      output = "gt",
      align = al
    )
    expect_s3_class(out, "gt_tbl")
  }
})

test_that("table_continuous_lm tinytable output respects align = 'center' / 'right' / 'auto'", {
  skip_if_not_installed("tinytable")
  for (al in c("center", "right", "auto")) {
    out <- table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = sex,
      output = "tinytable",
      align = al
    )
    expect_true(inherits(out, "tinytable"))
  }
})

test_that("table_continuous_lm flextable output is rendered (decimal default)", {
  skip_if_not_installed("flextable")
  out <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = sex,
    output = "flextable"
  )
  expect_s3_class(out, "flextable")
})

test_that("table_continuous_lm flextable output respects align = 'center' / 'right' / 'auto'", {
  skip_if_not_installed("flextable")
  for (al in c("center", "right", "auto")) {
    out <- table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = sex,
      output = "flextable",
      align = al
    )
    expect_s3_class(out, "flextable")
  }
})

test_that("table_continuous_lm word output writes a docx file", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)
  out <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = sex,
    output = "word",
    word_path = tmp
  )
  expect_equal(out, tmp)
  expect_true(file.exists(tmp))
})

test_that("table_continuous_lm word output errors when word_path is missing", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  expect_error(
    table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = sex,
      output = "word"
    ),
    "word_path"
  )
})

test_that("table_continuous_lm excel output writes an xlsx file", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  out <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = sex,
    output = "excel",
    excel_path = tmp
  )
  expect_equal(out, tmp)
  expect_true(file.exists(tmp))
})

test_that("table_continuous_lm excel output errors when excel_path is missing", {
  skip_if_not_installed("openxlsx2")
  expect_error(
    table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = sex,
      output = "excel"
    ),
    "excel_path"
  )
})

test_that("resolve_cluster_argument: non-null quo that evaluates to NULL returns NULL", {
  data <- data.frame(y = 1:5, x = 1:5)
  null_fn <- function() NULL
  q <- rlang::new_quosure(rlang::expr(null_fn()), env = environment())
  expect_null(spicy:::resolve_cluster_argument(q, data, "cluster"))
})

test_that("table_continuous_lm: labels argument overrides default outcome label", {
  out <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = sex,
    labels = c(wellbeing_score = "Custom WB"),
    output = "data.frame"
  )
  expect_true("Custom WB" %in% out$Variable)
})

test_that("table_continuous_lm: numeric predictor + weights exercises weighted lm path", {
  df <- sochealth
  df$wt <- runif(nrow(df), 0.5, 1.5)
  out <- table_continuous_lm(
    df,
    select = wellbeing_score,
    by = age,
    weights = wt,
    output = "data.frame"
  )
  expect_s3_class(out, "data.frame")
  # B should be a finite number for the slope of age.
  expect_true(any(grepl("[0-9]", out$B)))
})

test_that("fit_categorical_predictor_lm_rows handles df_resid <= 0 (perfect fit)", {
  # Perfect fit: 3 observations, 3 levels => df.residual = 0; the qnorm
  # fallback at line 1382 fires. Use vcov = "classical" to avoid CR
  # complications; the function still returns rows without erroring.
  out <- spicy:::fit_categorical_predictor_lm_rows(
    y = c(1, 2, 3),
    x = factor(c("a", "b", "c")),
    weights = NULL,
    outcome_name = "y",
    outcome_label = "y",
    predictor_label = "g",
    vcov_type = "classical",
    contrast = "treatment",
    ci_level = 0.95,
    effect_size = "none"
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
})

test_that("compute_lm_vcov HC fallback returns the classical vcov after warning", {
  testthat::local_mocked_bindings(
    vcovHC = function(x, type, ...) stop("synthetic test failure"),
    .package = "sandwich"
  )
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  vc <- suppressWarnings(spicy:::compute_lm_vcov(fit, "HC4m"))
  expect_true(is.matrix(vc))
  expect_equal(vc, stats::vcov(fit))
})

test_that("compute_lm_vcov CR fallback returns the classical vcov after warning", {
  testthat::local_mocked_bindings(
    vcovCR = function(...) stop("synthetic CR failure"),
    .package = "clubSandwich"
  )
  fit <- stats::lm(extra ~ group, data = sleep)
  vc <- suppressWarnings(
    spicy:::compute_lm_vcov(fit, type = "CR2", cluster = sleep$ID)
  )
  expect_true(is.matrix(vc))
  expect_equal(vc, stats::vcov(fit))
})

test_that("compute_lm_vcov_bootstrap fallback returns the classical vcov when 0 valid replicates", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  testthat::local_mocked_bindings(
    lm = function(...) stop("synthetic lm failure"),
    .package = "stats"
  )
  vc <- suppressWarnings(spicy:::compute_lm_vcov_bootstrap(fit, boot_n = 5L))
  expect_equal(vc, stats::vcov(fit))
})

test_that("compute_lm_vcov_jackknife fallback returns the classical vcov when 0 valid replicates", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  testthat::local_mocked_bindings(
    lm = function(...) stop("synthetic lm failure"),
    .package = "stats"
  )
  vc <- suppressWarnings(spicy:::compute_lm_vcov_jackknife(fit))
  expect_equal(vc, stats::vcov(fit))
})

test_that("compute_lm_omega2 returns NA when weights length mismatches y / when sum(w) = 0", {
  fit <- stats::lm(extra ~ group, data = sleep)
  # Weights with mismatched length
  testthat::local_mocked_bindings(
    weights = function(x, ...) c(1, 1, 1),
    .package = "stats"
  )
  expect_equal(
    spicy:::compute_lm_omega2(fit, df_effect = 1L, df_resid = 18L),
    NA_real_
  )
})

test_that("compute_lm_omega2 returns NA when sum of weights is non-positive", {
  fit <- stats::lm(extra ~ group, data = sleep)
  testthat::local_mocked_bindings(
    weights = function(x, ...) rep(0, 20L),
    .package = "stats"
  )
  expect_equal(
    spicy:::compute_lm_omega2(fit, df_effect = 1L, df_resid = 18L),
    NA_real_
  )
})

test_that("compute_lm_coef_inference uses qnorm CI when df is not finite (CR path)", {
  fit <- stats::lm(extra ~ group, data = sleep)
  vc <- clubSandwich::vcovCR(fit, type = "CR2", cluster = sleep$ID)
  testthat::local_mocked_bindings(
    coef_test = function(...) {
      # Return Satterthwaite df = Inf so the qnorm branch fires.
      data.frame(
        Coef = c("(Intercept)", "groupB"),
        SE = c(0.5, 0.85),
        tstat = c(2.5, 1.86),
        df_Satt = c(Inf, Inf),
        p_Satt = c(0.02, 0.06),
        stringsAsFactors = FALSE
      )
    },
    .package = "clubSandwich"
  )
  out <- spicy:::compute_lm_coef_inference(
    fit,
    coef_idx = 2L,
    vc = vc,
    vcov_type = "CR2",
    cluster = sleep$ID
  )
  expect_equal(out$df, Inf)
  expect_true(is.finite(out$ci_lower))
})

test_that("compute_lm_coef_inference uses qnorm CI when df.residual <= 0 (classical fallback)", {
  # Perfect-fit lm: df.residual = 0 -> qnorm critical value branch
  fit <- stats::lm(c(1, 2, 3) ~ factor(c("a", "b", "c")))
  out <- spicy:::compute_lm_coef_inference(
    fit,
    coef_idx = 2L,
    vc = stats::vcov(fit),
    vcov_type = "classical"
  )
  expect_equal(out$df, 0)
})

test_that("compute_lm_wald_test: clubSandwich constrain_zero failure falls through to classical Wald", {
  fit <- stats::lm(Sepal.Length ~ Species, data = iris)
  vc <- clubSandwich::vcovCR(fit, type = "CR2", cluster = iris$Species)
  testthat::local_mocked_bindings(
    constrain_zero = function(...) stop("synthetic constrain_zero failure"),
    .package = "clubSandwich"
  )
  out <- spicy:::compute_lm_wald_test(
    fit,
    coef_idx_set = 2:3,
    vc = vc,
    vcov_type = "CR2",
    cluster = iris$Species
  )
  # Falls back to classical Wald F (test_type = "F", df2 = df.residual)
  expect_equal(out$test_type, "F")
  expect_true(is.finite(out$statistic))
})

test_that("export_continuous_lm_table errors when required Suggests packages are missing", {
  display_df <- data.frame(
    Variable = "x",
    M = "1",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) FALSE,
    .package = "base"
  )
  for (out in c("tinytable", "gt", "flextable", "excel", "clipboard")) {
    args <- list(
      display_df = display_df,
      output = out,
      ci_level = 0.95,
      excel_path = if (identical(out, "excel")) tempfile(fileext = ".xlsx") else NULL,
      excel_sheet = "Sheet1",
      clipboard_delim = "\t",
      word_path = NULL
    )
    expect_error(
      do.call(spicy:::export_continuous_lm_table, args),
      "Install package"
    )
  }
})
