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
      "r2",
      "adj_r2",
      "n",
      "sum_w"
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
  expect_equal(out$es_type[1], "f2")
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

  expect_equal(out$sum_w[1], sum(df$w))
  expect_equal(
    out$emmean[2],
    weighted.mean(df$y[df$g == "B"], df$w[df$g == "B"])
  )
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

test_that("table_continuous_lm display builder formats level blocks", {
  out <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = education,
    decimal_mark = ","
  )

  display <- spicy:::build_display_df_continuous_lm(out, 2L, ",", 0.95)
  expect_equal(display$Variable[1], "WHO-5 wellbeing index (0-100)")
  expect_equal(display$Variable[2], "")
  expect_match(display$Level[1], "\\(ref\\)")
  expect_false("95% CI LL" %in% names(display))
  expect_false("f²" %in% names(display))
})

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
  display <- spicy:::build_display_df_continuous_lm(
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
  expect_true(any(grepl("^t", names(wide_raw))))
  expect_true(all(
    c(
      "Variable",
      "B",
      "95% CI LL",
      "95% CI UL",
      "t",
      "p",
      "f²",
      "Adj. R²",
      "n"
    ) %in%
      names(display)
  ))
  expect_true(all(
    c("Variable", "B", "95% CI LL", "95% CI UL", "p", "f²", "Adj. R²", "n") %in%
      names(wide_display)
  ))
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
  display <- spicy:::build_display_df_continuous_lm(
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
    c("Variable", "Level", "M", "Δ (Male - Female)") %in% names(display)
  ))
  expect_false(any(
    c("p", "n", "f²", "R²", "Adj. R²", "95% CI LL") %in% names(display)
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
  expect_equal(spicy:::format_number_lm(c(1.2, NA), 1L, ","), c("1,2", ""))
  expect_equal(spicy:::format_p_value_lm(NA_real_), "")
  expect_equal(spicy:::format_p_value_lm(0.045, ","), ",045")
  expect_equal(
    spicy:::format_test_value_lm("t", 2.34, 1, 10, 2L, "."),
    "t = 2.34"
  )
  expect_equal(
    spicy:::format_test_value_lm("F", 2.34, 1, 10, 2L, "."),
    "F = 2.34"
  )
  expect_equal(spicy:::format_effect_size_lm("f²", 0.23, 2L, "."), "f² = 0.23")
  expect_equal(
    spicy:::format_effect_size_lm(NA_character_, NA_real_, 2L, "."),
    ""
  )
})

test_that("table_continuous_lm internal covariance helper covers fallback branches", {
  fit <- lm(mpg ~ wt, data = mtcars)
  fit_singular <- lm(mpg ~ wt + I(2 * wt), data = mtcars)

  expect_error(
    spicy:::compute_lm_vcov(fit, "bogus"),
    "Unknown `vcov` type"
  )

  vc <- spicy:::compute_lm_vcov(fit_singular, "HC3")
  expect_true(is.matrix(vc))
  expect_equal(dim(vc), c(3L, 3L))
  expect_true(all(is.na(vc)))
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
