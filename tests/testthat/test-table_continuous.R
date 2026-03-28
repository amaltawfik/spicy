# ---- structure ----

test_that("table_continuous returns correct structure", {
  out <- table_continuous(iris, output = "data.frame")
  expect_s3_class(out, "data.frame")
  expect_named(
    out,
    c(
      "variable",
      "label",
      "mean",
      "sd",
      "min",
      "max",
      "ci_lower",
      "ci_upper",
      "n"
    )
  )
  expect_equal(nrow(out), 4L)
})

test_that("table_continuous returns spicy_continuous_table class when default output", {
  out <- table_continuous(iris, select = c(Sepal.Length))
  expect_s3_class(out, "spicy_continuous_table")
  expect_s3_class(out, "spicy_table")
})

test_that("table_continuous default output object carries correct attributes", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    ci_level = 0.90,
    digits = 3,
    decimal_mark = ","
  )
  expect_equal(attr(out, "ci_level"), 0.90)
  expect_equal(attr(out, "digits"), 3L)
  expect_equal(attr(out, "decimal_mark"), ",")
  expect_null(attr(out, "group_var"))
})

test_that("table_continuous default output with group carries group_var attribute", {
  out <- table_continuous(iris, select = Sepal.Length, by = Species)
  expect_equal(attr(out, "group_var"), "Species")
})

test_that("table_continuous accepts by as a character object without warnings", {
  by_col <- "Species"

  expect_no_warning(
    out <- table_continuous(
      iris,
      select = Sepal.Length,
      by = by_col,
      output = "data.frame"
    )
  )

  expect_true("group" %in% names(out))
  expect_true(all(out$group %in% unique(iris$Species)))
})

# ---- computation ----

test_that("table_continuous computes correct values", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  out <- table_continuous(df, output = "data.frame")
  expect_equal(out$mean, 3)
  expect_equal(out$sd, sd(c(1, 2, 3, 4, 5)))
  expect_equal(out$min, 1)
  expect_equal(out$max, 5)
  expect_equal(out$n, 5L)
})

test_that("table_continuous CI is t-based", {
  df <- data.frame(x = c(10, 20, 30))
  out <- table_continuous(df, ci_level = 0.95, output = "data.frame")
  m <- mean(c(10, 20, 30))
  se <- sd(c(10, 20, 30)) / sqrt(3)
  t_crit <- qt(0.975, df = 2)
  expect_equal(out$ci_lower, m - t_crit * se, tolerance = 1e-10)
  expect_equal(out$ci_upper, m + t_crit * se, tolerance = 1e-10)
})

test_that("table_continuous ci_level affects width", {
  df <- data.frame(x = 1:100)
  out90 <- table_continuous(df, ci_level = 0.90, output = "data.frame")
  out99 <- table_continuous(df, ci_level = 0.99, output = "data.frame")
  expect_gt(
    out99$ci_upper - out99$ci_lower,
    out90$ci_upper - out90$ci_lower
  )
})

test_that("table_continuous handles multiple numeric variables", {
  out <- table_continuous(iris, output = "data.frame")
  expect_equal(nrow(out), 4L)
  expect_equal(
    out$variable,
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
  expect_equal(out$mean[1], mean(iris$Sepal.Length))
  expect_equal(out$mean[4], mean(iris$Petal.Width))
})

# ---- NA handling ----

test_that("table_continuous handles NAs", {
  df <- data.frame(x = c(1, NA, 3, NA, 5))
  out <- table_continuous(df, output = "data.frame")
  expect_equal(out$n, 3L)
  expect_equal(out$mean, mean(c(1, 3, 5)))
})

test_that("table_continuous handles all-NA column", {
  df <- data.frame(x = rep(NA_real_, 5))
  out <- table_continuous(df, output = "data.frame")
  expect_equal(out$n, 0L)
  expect_true(is.na(out$mean))
  expect_true(is.na(out$sd))
  expect_true(is.na(out$min))
  expect_true(is.na(out$max))
  expect_true(is.na(out$ci_lower))
  expect_true(is.na(out$ci_upper))
})

test_that("table_continuous n=1 gives NA for sd and CI", {
  df <- data.frame(x = 42)
  out <- table_continuous(df, output = "data.frame")
  expect_equal(out$n, 1L)
  expect_equal(out$mean, 42)
  expect_equal(out$min, 42)
  expect_equal(out$max, 42)
  expect_true(is.na(out$sd))
  expect_true(is.na(out$ci_lower))
  expect_true(is.na(out$ci_upper))
})

test_that("table_continuous display uses -- for NA values", {
  df <- data.frame(x = 42)
  out <- table_continuous(df)
  display <- spicy:::build_display_df(out, 2L, ".", 0.95)
  expect_equal(display$SD[1], "--")
  expect_equal(display[["95% CI LL"]][1], "--")
  expect_equal(display[["95% CI UL"]][1], "--")
})

# ---- column selection ----

test_that("table_continuous filters non-numeric columns", {
  df <- data.frame(x = 1:5, y = letters[1:5], z = 6:10)
  out <- table_continuous(df, output = "data.frame")
  expect_equal(out$variable, c("x", "z"))
})

test_that("table_continuous select works with tidyselect", {
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Petal.Width),
    output = "data.frame"
  )
  expect_equal(nrow(out), 2L)
  expect_equal(out$variable, c("Sepal.Length", "Petal.Width"))
})

test_that("table_continuous select works with character vector", {
  out <- table_continuous(
    iris,
    select = c("Sepal.Length", "Petal.Width"),
    output = "data.frame"
  )
  expect_equal(nrow(out), 2L)
  expect_equal(out$variable, c("Sepal.Length", "Petal.Width"))
})

test_that("table_continuous select works with tidyselect helpers", {
  out <- table_continuous(
    iris,
    select = starts_with("Sepal"),
    output = "data.frame"
  )
  expect_equal(nrow(out), 2L)
  expect_true(all(grepl("^Sepal", out$variable)))
})

test_that("table_continuous exclude works", {
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width, Petal.Length),
    exclude = "Sepal.Width",
    output = "data.frame"
  )
  expect_equal(nrow(out), 2L)
  expect_false("Sepal.Width" %in% out$variable)
})

test_that("table_continuous exclude works with an unquoted column name", {
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width, Petal.Length),
    exclude = Sepal.Width,
    output = "data.frame"
  )
  expect_equal(nrow(out), 2L)
  expect_false("Sepal.Width" %in% out$variable)
})

test_that("table_continuous exclude works with tidyselect syntax", {
  out <- table_continuous(
    iris,
    select = everything(),
    exclude = c(Sepal.Width, Petal.Width),
    output = "data.frame"
  )
  expect_equal(
    out$variable,
    c("Sepal.Length", "Petal.Length")
  )
})

test_that("table_continuous regex selection works", {
  out <- table_continuous(
    iris,
    select = "^Sepal",
    regex = TRUE,
    output = "data.frame"
  )
  expect_equal(nrow(out), 2L)
  expect_true(all(grepl("^Sepal", out$variable)))
})

test_that("table_continuous regex with default select matches all", {
  out <- table_continuous(iris, regex = TRUE, output = "data.frame")
  expect_equal(nrow(out), 4L)
})

test_that("table_continuous verbose reports ignored columns", {
  df <- data.frame(x = 1:5, y = letters[1:5], z = 6:10)
  expect_message(
    table_continuous(df, output = "data.frame", verbose = TRUE),
    "Ignored non-numeric"
  )
})

# ---- labels ----

test_that("table_continuous uses column names as default labels", {
  df <- data.frame(x = 1:5, y = 6:10)
  out <- table_continuous(df, output = "data.frame")
  expect_equal(out$label, c("x", "y"))
})

test_that("table_continuous accepts custom labels", {
  df <- data.frame(x = 1:5, y = 6:10)
  out <- table_continuous(
    df,
    labels = c(x = "My X", y = "My Y"),
    output = "data.frame"
  )
  expect_equal(out$label, c("My X", "My Y"))
})

test_that("table_continuous custom labels apply only to matching columns", {
  df <- data.frame(x = 1:5, y = 6:10)
  out <- table_continuous(df, labels = c(x = "My X"), output = "data.frame")
  expect_equal(out$label, c("My X", "y"))
})

test_that("table_continuous auto-detects haven labels", {
  df <- data.frame(x = 1:5)
  attr(df$x, "label") <- "A labeled var"
  out <- table_continuous(df, output = "data.frame")
  expect_equal(out$label, "A labeled var")
})

test_that("table_continuous custom labels override haven labels", {
  df <- data.frame(x = 1:5)
  attr(df$x, "label") <- "Haven label"
  out <- table_continuous(df, labels = c(x = "Custom"), output = "data.frame")
  expect_equal(out$label, "Custom")
})

# ---- grouping ----

test_that("table_continuous grouped output has group column", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "data.frame"
  )
  expect_true("group" %in% names(out))
  expect_equal(nrow(out), 3L)
  expect_equal(sort(out$group), c("setosa", "versicolor", "virginica"))
})

test_that("table_continuous grouped stats are correct", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "data.frame"
  )
  setosa_data <- iris$Sepal.Length[iris$Species == "setosa"]
  setosa_row <- out[out$group == "setosa", ]
  expect_equal(setosa_row$mean, mean(setosa_data))
  expect_equal(setosa_row$sd, sd(setosa_data))
  expect_equal(setosa_row$n, length(setosa_data))
})

test_that("table_continuous group_var is excluded from numeric selection", {
  df <- data.frame(g = rep(1:2, each = 5), x = 1:10, y = 11:20)
  out <- table_continuous(df, by = g, output = "data.frame")
  expect_false("g" %in% out$variable)
})

test_that("table_continuous preserves factor level order in group_var", {
  df <- data.frame(
    g = factor(
      rep(c("C", "A", "B"), each = 5),
      levels = c("B", "A", "C")
    ),
    x = 1:15
  )
  out <- table_continuous(df, by = g, output = "data.frame")
  expect_equal(out$group, c("B", "A", "C"))
})

test_that("table_continuous grouped with multiple variables", {
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Petal.Length),
    by = Species,
    output = "data.frame"
  )
  expect_equal(nrow(out), 6L)
  expect_equal(
    out$variable,
    rep(c("Sepal.Length", "Petal.Length"), each = 3L)
  )
})

test_that("table_continuous grouped with character group_var sorts levels", {
  df <- data.frame(g = c("Z", "A", "Z", "A"), x = c(1, 2, 3, 4))
  out <- table_continuous(df, by = g, output = "data.frame")
  expect_equal(out$group, c("A", "Z"))
})

# ---- p_value / statistic ----

test_that("table_continuous p_value adds p column without Test column", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    p_value = TRUE,
    output = "data.frame"
  )
  expect_true("p.value" %in% names(out))
  expect_true("statistic" %in% names(out))
  display <- spicy:::build_display_df(
    out,
    2L,
    ".",
    0.95,
    show_p = TRUE,
    show_statistic = FALSE
  )
  expect_true("p" %in% names(display))
  expect_false("Test" %in% names(display))
})

test_that("table_continuous p_value + statistic adds both columns", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    p_value = TRUE,
    statistic = TRUE,
    output = "data.frame"
  )
  display <- spicy:::build_display_df(
    out,
    2L,
    ".",
    0.95,
    show_p = TRUE,
    show_statistic = TRUE
  )
  expect_true("Test" %in% names(display))
  expect_true("p" %in% names(display))
})

test_that("table_continuous statistic alone shows Test column without p", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    statistic = TRUE,
    output = "data.frame"
  )
  expect_true("statistic" %in% names(out))
  display <- spicy:::build_display_df(
    out,
    2L,
    ".",
    0.95,
    show_p = FALSE,
    show_statistic = TRUE
  )
  expect_true("Test" %in% names(display))
  expect_false("p" %in% names(display))
})

test_that("table_continuous p_value without by warns", {
  expect_warning(
    out <- table_continuous(
      iris,
      select = Sepal.Length,
      p_value = TRUE,
      output = "data.frame"
    ),
    "ignored"
  )
  expect_false("p.value" %in% names(out))
})

test_that("table_continuous statistic without by warns", {
  expect_warning(
    out <- table_continuous(
      iris,
      select = Sepal.Length,
      statistic = TRUE,
      output = "data.frame"
    ),
    "ignored"
  )
  expect_false("statistic" %in% names(out))
})

test_that("table_continuous p_value default output carries show_p attribute", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    p_value = TRUE
  )
  expect_true(attr(out, "show_p"))
  expect_false(attr(out, "show_statistic"))
})

test_that("table_continuous p_value + statistic default output carries both attributes", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    p_value = TRUE,
    statistic = TRUE
  )
  expect_true(attr(out, "show_p"))
  expect_true(attr(out, "show_statistic"))
})

test_that("table_continuous statistic default output carries show_statistic attribute", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    statistic = TRUE
  )
  expect_false(attr(out, "show_p"))
  expect_true(attr(out, "show_statistic"))
})

test_that("table_continuous print works with statistic only", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    statistic = TRUE
  )
  expect_output(print(out))
})

test_that("table_continuous print works with p_value only", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    p_value = TRUE
  )
  expect_output(print(out))
})

test_that("table_continuous print works with p_value + statistic", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    p_value = TRUE,
    statistic = TRUE
  )
  expect_output(print(out))
})

# ---- test method ----

test_that("table_continuous test='welch' is the default (2 groups)", {
  df <- data.frame(g = rep(c("A", "B"), each = 20), x = rnorm(40))
  out <- table_continuous(df, by = g, p_value = TRUE, output = "data.frame")
  expect_equal(out$test_type[1], "welch_t")
})
test_that("table_continuous test='welch' with 3+ groups uses welch_anova", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    p_value = TRUE,
    output = "data.frame"
  )
  expect_equal(out$test_type[1], "welch_anova")
})

test_that("table_continuous test='student' uses student_t for 2 groups", {
  df <- data.frame(g = rep(c("A", "B"), each = 20), x = rnorm(40))
  out <- table_continuous(
    df,
    by = g,
    test = "student",
    p_value = TRUE,
    output = "data.frame"
  )
  expect_equal(out$test_type[1], "student_t")
})

test_that("table_continuous test='student' uses anova for 3+ groups", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    test = "student",
    p_value = TRUE,
    output = "data.frame"
  )
  expect_equal(out$test_type[1], "anova")
  expect_false(is.na(out$df2[1]))
})

test_that("table_continuous test='nonparametric' uses wilcoxon for 2 groups", {
  df <- data.frame(g = rep(c("A", "B"), each = 20), x = rnorm(40))
  out <- table_continuous(
    df,
    by = g,
    test = "nonparametric",
    p_value = TRUE,
    output = "data.frame"
  )
  expect_equal(out$test_type[1], "wilcoxon")
  expect_true(is.na(out$df1[1]))
})

test_that("table_continuous test='nonparametric' uses kruskal for 3+ groups", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    test = "nonparametric",
    p_value = TRUE,
    output = "data.frame"
  )
  expect_equal(out$test_type[1], "kruskal")
  expect_false(is.na(out$df1[1]))
})

test_that("table_continuous test attribute is stored", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    test = "student",
    p_value = TRUE
  )
  expect_equal(attr(out, "test"), "student")
})

test_that("table_continuous test attribute is NA without by", {
  expect_warning(
    out <- table_continuous(iris, select = Sepal.Length, p_value = TRUE),
    "ignored"
  )
  expect_true(is.na(attr(out, "test")))
})

test_that("table_continuous warns when test is set without p_value/statistic", {
  expect_warning(
    table_continuous(
      iris,
      select = Sepal.Length,
      by = Species,
      test = "student"
    ),
    "ignored"
  )
})

test_that("table_continuous nonparametric statistic display uses W and H", {
  df2 <- data.frame(g = rep(c("A", "B"), each = 20), x = rnorm(40))
  out2 <- table_continuous(
    df2,
    by = g,
    test = "nonparametric",
    statistic = TRUE,
    output = "data.frame"
  )
  display2 <- spicy:::build_display_df(
    out2,
    2L,
    ".",
    0.95,
    show_p = FALSE,
    show_statistic = TRUE
  )
  expect_match(display2$Test[1], "^W = ")

  out3 <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    test = "nonparametric",
    statistic = TRUE,
    output = "data.frame"
  )
  display3 <- spicy:::build_display_df(
    out3,
    2L,
    ".",
    0.95,
    show_p = FALSE,
    show_statistic = TRUE
  )
  expect_match(display3$Test[1], "^H\\(")
})

test_that("table_continuous student statistic display uses t and F", {
  df2 <- data.frame(g = rep(c("A", "B"), each = 20), x = rnorm(40))
  out2 <- table_continuous(
    df2,
    by = g,
    test = "student",
    statistic = TRUE,
    output = "data.frame"
  )
  display2 <- spicy:::build_display_df(
    out2,
    2L,
    ".",
    0.95,
    show_p = FALSE,
    show_statistic = TRUE
  )
  expect_match(display2$Test[1], "^t\\(")

  out3 <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    test = "student",
    statistic = TRUE,
    output = "data.frame"
  )
  display3 <- spicy:::build_display_df(
    out3,
    2L,
    ".",
    0.95,
    show_p = FALSE,
    show_statistic = TRUE
  )
  expect_match(display3$Test[1], "^F\\(")
})

test_that("table_continuous test='nonparametric' p-values match base R", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    test = "nonparametric",
    p_value = TRUE,
    output = "data.frame"
  )
  ref <- kruskal.test(Sepal.Length ~ Species, data = iris)
  expect_equal(out$p.value[1], ref$p.value)
})

# ---- formatting ----

test_that("table_continuous decimal_mark attribute is set", {
  out <- table_continuous(iris, select = Sepal.Length, decimal_mark = ",")
  expect_equal(attr(out, "decimal_mark"), ",")
})

test_that("table_continuous decimal_mark comma in display output", {
  df <- data.frame(x = c(1.5, 2.5, 3.5))
  out <- table_continuous(df, decimal_mark = ",")
  display <- spicy:::build_display_df(out, 2L, ",", 0.95)
  expect_true(any(grepl(",", display$M)))
  expect_false(any(grepl("\\.", display$M)))
})

test_that("table_continuous digits parameter controls precision", {
  df <- data.frame(x = c(1.123456, 2.654321, 3.987654))
  d0 <- spicy:::build_display_df(
    table_continuous(df, digits = 0),
    0L,
    ".",
    0.95
  )
  d4 <- spicy:::build_display_df(
    table_continuous(df, digits = 4),
    4L,
    ".",
    0.95
  )
  expect_false(grepl("\\.", d0$M[1]))
  expect_match(d4$M[1], "\\.[0-9]{4}$")
})

# ---- printing ----

test_that("print.spicy_continuous_table produces output", {
  out <- table_continuous(iris, select = c(Sepal.Length, Sepal.Width))
  expect_output(print(out))
})

test_that("print.spicy_continuous_table works with groups", {
  out <- table_continuous(iris, select = Sepal.Length, by = Species)
  expect_output(print(out))
})

test_that("print.spicy_continuous_table returns invisible x", {
  out <- table_continuous(iris, select = Sepal.Length)
  ret <- withVisible(print(out))
  expect_false(ret$visible)
  expect_s3_class(ret$value, "spicy_continuous_table")
})

# ---- validation ----

test_that("table_continuous errors on non-data-frame", {
  expect_error(table_continuous(1:10), "data\\.frame")
})

test_that("table_continuous validates ci_level", {
  df <- data.frame(x = 1:5)
  expect_error(table_continuous(df, ci_level = 2), "ci_level")
  expect_error(table_continuous(df, ci_level = 0), "ci_level")
  expect_error(table_continuous(df, ci_level = -0.5), "ci_level")
  expect_error(table_continuous(df, ci_level = NA), "ci_level")
  expect_error(table_continuous(df, ci_level = "a"), "ci_level")
})

test_that("table_continuous validates digits", {
  df <- data.frame(x = 1:5)
  expect_error(table_continuous(df, digits = -1), "digits")
  expect_error(table_continuous(df, digits = "a"), "digits")
  expect_error(table_continuous(df, digits = NA), "digits")
})

test_that("table_continuous validates decimal_mark", {
  df <- data.frame(x = 1:5)
  expect_error(table_continuous(df, decimal_mark = ";"), "decimal_mark")
})

test_that("table_continuous validates labels", {
  df <- data.frame(x = 1:5)
  expect_error(table_continuous(df, labels = c("a", "b")), "labels")
  expect_error(table_continuous(df, labels = 42), "labels")
})

test_that("table_continuous validates by", {
  df <- data.frame(x = 1:5)
  expect_error(table_continuous(df, by = nonexistent), "by")
})

test_that("table_continuous validates regex select", {
  df <- data.frame(x = 1:5)
  expect_error(
    table_continuous(df, select = c("a", "b"), regex = TRUE),
    "single character pattern"
  )
  expect_error(
    table_continuous(df, select = NA_character_, regex = TRUE),
    "single character pattern"
  )
})

test_that("table_continuous validates logical parameters", {
  df <- data.frame(x = 1:5)
  expect_error(table_continuous(df, p_value = "yes"), "p_value")
  expect_error(table_continuous(df, statistic = 1), "statistic")
  expect_error(table_continuous(df, effect_size = NA), "effect_size")
  expect_error(table_continuous(df, effect_size_ci = NULL), "effect_size_ci")
  expect_error(table_continuous(df, regex = "TRUE"), "regex")
  expect_error(table_continuous(df, verbose = NA), "verbose")
})

test_that("table_continuous warns when NA present in by column", {
  df <- data.frame(
    x = 1:6,
    g = c("A", "A", "B", "B", NA, NA)
  )
  expect_warning(
    table_continuous(df, select = "x", by = "g", output = "data.frame"),
    "2 observation.*excluded"
  )
  out <- suppressWarnings(
    table_continuous(df, select = "x", by = "g", output = "data.frame")
  )
  # Only A and B groups should be present
  expect_equal(sort(unique(out$group)), c("A", "B"))
})

test_that("fmt_p uses non-breaking space in display", {
  set.seed(1)
  df <- data.frame(
    x = c(rnorm(30, 0), rnorm(30, 10)),
    g = rep(c("A", "B"), each = 30)
  )
  out <- table_continuous(df, select = "x", by = "g", p_value = TRUE)
  # build_display_df is used by the print method; call it directly
  display <- spicy:::build_display_df(
    out,
    digits = attr(out, "digits"),
    decimal_mark = attr(out, "decimal_mark"),
    ci_level = attr(out, "ci_level"),
    show_p = TRUE,
    show_statistic = FALSE,
    show_effect_size = FALSE,
    show_effect_size_ci = FALSE
  )
  p_col <- display[["p"]][nzchar(display[["p"]])]
  # p should be very small -> "<\u00A0.001" with non-breaking space
  expect_true(any(grepl("<\u00A0", p_col)))
})

test_that("table_continuous warns on no numeric columns", {
  df <- data.frame(x = letters[1:5])
  expect_warning(table_continuous(df, output = "data.frame"), "No numeric")
})

test_that("table_continuous no-numeric warning returns empty data.frame", {
  df <- data.frame(x = letters[1:5])
  out <- suppressWarnings(table_continuous(df, output = "data.frame"))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

# ---- optional outputs: tinytable ----

test_that("table_continuous tinytable output works", {
  skip_if_not_installed("tinytable")
  out <- table_continuous(iris, select = c(Sepal.Length), output = "tinytable")
  expect_true(methods::is(out, "tinytable"))
})

test_that("table_continuous tinytable with groups works", {
  skip_if_not_installed("tinytable")
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "tinytable"
  )
  expect_true(methods::is(out, "tinytable"))
})

# ---- optional outputs: gt ----

test_that("table_continuous gt output works", {
  skip_if_not_installed("gt")
  out <- table_continuous(iris, select = c(Sepal.Length), output = "gt")
  expect_s3_class(out, "gt_tbl")
})

test_that("table_continuous gt with groups works", {
  skip_if_not_installed("gt")
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "gt"
  )
  expect_s3_class(out, "gt_tbl")
})

test_that("table_continuous gt has spanners for all columns", {
  skip_if_not_installed("gt")
  out <- table_continuous(iris, select = c(Sepal.Length), output = "gt")
  spanners <- out[["_spanners"]]
  labels <- unlist(spanners$spanner_label)
  expect_true("Variable" %in% labels)
  expect_true("M" %in% labels)
  expect_true("SD" %in% labels)
  expect_true("n" %in% labels)
  expect_true(any(grepl("CI", labels)))
})

test_that("table_continuous gt with p_value only has p spanner but not Test", {
  skip_if_not_installed("gt")
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    p_value = TRUE,
    output = "gt"
  )
  spanners <- out[["_spanners"]]
  labels <- unlist(spanners$spanner_label)
  expect_true("p" %in% labels)
  expect_false("Test" %in% labels)
})

test_that("table_continuous gt with p_value + statistic has both spanners", {
  skip_if_not_installed("gt")
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    p_value = TRUE,
    statistic = TRUE,
    output = "gt"
  )
  spanners <- out[["_spanners"]]
  labels <- unlist(spanners$spanner_label)
  expect_true("p" %in% labels)
  expect_true("Test" %in% labels)
})

# ---- optional outputs: flextable ----

test_that("table_continuous flextable output works", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  out <- table_continuous(iris, select = c(Sepal.Length), output = "flextable")
  expect_s3_class(out, "flextable")
})

test_that("table_continuous flextable has 2-row header with CI spanner", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  out <- table_continuous(iris, select = c(Sepal.Length), output = "flextable")
  hdr <- out$header$dataset
  expect_equal(nrow(hdr), 2L)
  expect_true(any(grepl("CI", hdr[1, ])))
})

# ---- optional outputs: excel ----

test_that("table_continuous excel output works", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  out <- table_continuous(
    iris,
    select = c(Sepal.Length),
    output = "excel",
    excel_path = tmp
  )
  expect_true(file.exists(tmp))
})

test_that("table_continuous excel errors without path", {
  expect_error(
    table_continuous(iris, select = c(Sepal.Length), output = "excel"),
    "excel_path"
  )
})

test_that("table_continuous excel errors with empty path", {
  expect_error(
    table_continuous(
      iris,
      select = c(Sepal.Length),
      output = "excel",
      excel_path = ""
    ),
    "excel_path"
  )
})

# ---- optional outputs: word ----

test_that("table_continuous word output writes file", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)
  out <- table_continuous(
    iris,
    select = c(Sepal.Length),
    output = "word",
    word_path = tmp
  )
  expect_true(file.exists(tmp))
})

test_that("table_continuous word errors without path", {
  expect_error(
    table_continuous(iris, select = c(Sepal.Length), output = "word"),
    "word_path"
  )
})

test_that("table_continuous word errors with empty path", {
  expect_error(
    table_continuous(
      iris,
      select = c(Sepal.Length),
      output = "word",
      word_path = ""
    ),
    "word_path"
  )
})

# ---- optional outputs: clipboard ----

test_that("table_continuous clipboard output works", {
  skip_if_not_installed("clipr")
  skip_if_not(clipr::clipr_available(), "Clipboard not available")
  expect_message(
    out <- table_continuous(
      iris,
      select = c(Sepal.Length),
      output = "clipboard"
    ),
    "copied to clipboard"
  )
  expect_s3_class(out, "data.frame")
})

test_that("table_continuous clipboard output can be exercised with a mocked writer", {
  skip_if_not_installed("clipr")

  captured <- NULL

  local_mocked_bindings(
    write_clip = function(text, ...) {
      captured <<- text
      invisible(text)
    },
    clipr_available = function(...) TRUE,
    .package = "clipr"
  )

  expect_message(
    out <- table_continuous(
      iris,
      select = c(Sepal.Length),
      output = "clipboard",
      clipboard_delim = ";"
    ),
    "copied to clipboard"
  )

  expect_s3_class(out, "data.frame")
  expect_true(is.character(captured))
  expect_match(captured, "Variable;M;SD")
  expect_match(captured, "Sepal.Length")
})

# ---- grouped optional outputs ----

test_that("table_continuous flextable with groups works", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "flextable"
  )
  expect_s3_class(out, "flextable")
})

test_that("table_continuous excel with groups works", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "excel",
    excel_path = tmp
  )
  expect_true(file.exists(tmp))
})

test_that("table_continuous word with groups works", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "word",
    word_path = tmp
  )
  expect_true(file.exists(tmp))
})

# ---- display format details ----

test_that("build_display_df ungrouped has correct column names", {
  df <- data.frame(x = 1:10)
  out <- table_continuous(df, ci_level = 0.90)
  display <- spicy:::build_display_df(out, 2L, ".", 0.90)
  expect_true("90% CI LL" %in% names(display))
  expect_true("90% CI UL" %in% names(display))
  expect_false("Group" %in% names(display))
})

test_that("build_display_df grouped has Group column", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
  )
  display <- spicy:::build_display_df(out, 2L, ".", 0.95)
  expect_true("Group" %in% names(display))
  expect_true("95% CI LL" %in% names(display))
  expect_true("95% CI UL" %in% names(display))
})

test_that("table_continuous multiple variables with groups has correct rows", {
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width),
    by = Species,
    output = "data.frame"
  )
  expect_equal(nrow(out), 6L)
  expect_equal(out$n[1], 50L)
})

test_that("table_continuous NAs in grouped data are handled", {
  df <- data.frame(
    g = c("A", "A", "B", "B", "B"),
    x = c(1, NA, 3, NA, 5)
  )
  out <- table_continuous(df, by = g, output = "data.frame")
  a_row <- out[out$group == "A", ]
  b_row <- out[out$group == "B", ]
  expect_equal(a_row$n, 1L)
  expect_equal(b_row$n, 2L)
  expect_equal(b_row$mean, mean(c(3, 5)))
})

test_that("table_continuous gt grouped output has Group column aligned left", {
  skip_if_not_installed("gt")
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "gt"
  )
  dat <- out[["_data"]]
  expect_true("Group" %in% names(dat))
})

test_that("table_continuous tinytable grouped has correct number of rows", {
  skip_if_not_installed("tinytable")
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width),
    by = Species,
    output = "tinytable"
  )
  expect_true(methods::is(out, "tinytable"))
})

# ---- by selecting multiple columns ----

test_that("table_continuous errors when by selects multiple columns", {
  df <- data.frame(a = 1:6, b = rep(1:2, 3), c = rep(1:3, 2), x = 11:16)
  expect_error(table_continuous(df, by = c(b, c), output = "data.frame"))
})


# ---- grouped clipboard ----

test_that("table_continuous clipboard with groups works", {
  skip_if_not_installed("clipr")
  skip_if_not(clipr::clipr_available(), "Clipboard not available")
  expect_message(
    out <- table_continuous(
      iris,
      select = Sepal.Length,
      by = Species,
      output = "clipboard"
    ),
    "copied to clipboard"
  )
  expect_s3_class(out, "data.frame")
  expect_true("Group" %in% names(out))
})

# ---- non-default clipboard_delim ----

test_that("table_continuous clipboard with custom delimiter works", {
  skip_if_not_installed("clipr")
  skip_if_not(clipr::clipr_available(), "Clipboard not available")
  expect_message(
    out <- table_continuous(
      iris,
      select = Sepal.Length,
      output = "clipboard",
      clipboard_delim = ";"
    ),
    "copied to clipboard"
  )
  expect_s3_class(out, "data.frame")
})

# ---- print method %||% fallback branches ----

# ---- effect size ----

test_that("table_continuous effect_size=TRUE adds es columns (welch, 2 groups)", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  out <- table_continuous(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    output = "data.frame"
  )
  expect_true("es_type" %in% names(out))
  expect_true("es_value" %in% names(out))
  expect_true("es_ci_lower" %in% names(out))
  expect_true("es_ci_upper" %in% names(out))
  expect_equal(out$es_type[1], "hedges_g")
  expect_false(is.na(out$es_value[1]))
})

test_that("table_continuous Hedges' g matches manual calculation", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  out <- table_continuous(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    output = "data.frame"
  )
  x1 <- df$Sepal.Length[df$Species == "setosa"]
  x2 <- df$Sepal.Length[df$Species == "versicolor"]
  n1 <- length(x1)
  n2 <- length(x2)
  s_pooled <- sqrt(
    ((n1 - 1) * var(x1) + (n2 - 1) * var(x2)) / (n1 + n2 - 2)
  )
  d <- (mean(x1) - mean(x2)) / s_pooled
  g_manual <- d * (1 - 3 / (4 * (n1 + n2 - 2) - 1))
  expect_equal(out$es_value[1], g_manual)
})

test_that("table_continuous eta-squared for 3+ groups (welch)", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    output = "data.frame"
  )
  expect_equal(out$es_type[1], "eta_sq")
  expect_false(is.na(out$es_ci_lower[1]))
  expect_false(is.na(out$es_ci_upper[1]))
  # Manual eta-squared
  grand_mean <- mean(iris$Sepal.Length)
  ss_between <- sum(tapply(
    iris$Sepal.Length,
    iris$Species,
    \(x) length(x) * (mean(x) - grand_mean)^2
  ))
  ss_total <- sum((iris$Sepal.Length - grand_mean)^2)
  expect_equal(out$es_value[1], ss_between / ss_total)
})

test_that("table_continuous nonparametric 2 groups gives rank-biserial r", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  out <- table_continuous(
    df,
    select = Sepal.Length,
    by = Species,
    test = "nonparametric",
    effect_size = TRUE,
    output = "data.frame"
  )
  expect_equal(out$es_type[1], "r_rb")
  expect_false(is.na(out$es_ci_lower[1]))
  expect_false(is.na(out$es_ci_upper[1]))
})

test_that("table_continuous nonparametric 3+ groups gives epsilon-squared", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    test = "nonparametric",
    effect_size = TRUE,
    output = "data.frame"
  )
  expect_equal(out$es_type[1], "epsilon_sq")
  expect_false(is.na(out$es_ci_lower[1]))
  expect_false(is.na(out$es_ci_upper[1]))
})

test_that("table_continuous effect_size without by warns", {
  expect_warning(
    out <- table_continuous(
      iris,
      select = Sepal.Length,
      effect_size = TRUE,
      output = "data.frame"
    ),
    "ignored"
  )
  expect_false("es_type" %in% names(out))
})

test_that("table_continuous effect_size_ci without effect_size warns and enables it", {
  expect_warning(
    out <- table_continuous(
      iris,
      select = Sepal.Length,
      by = Species,
      effect_size_ci = TRUE,
      output = "data.frame"
    ),
    "effect_size_ci"
  )
  expect_true("es_type" %in% names(out))
})

test_that("table_continuous effect_size default output carries attributes", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    effect_size_ci = TRUE
  )
  expect_true(attr(out, "show_effect_size"))
  expect_true(attr(out, "show_effect_size_ci"))
})

test_that("table_continuous effect_size display shows ES column", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
  )
  display <- spicy:::build_display_df(
    out,
    2L,
    ".",
    0.95,
    show_effect_size = TRUE
  )
  expect_true("ES" %in% names(display))
  expect_match(display$ES[1], "=")
})

test_that("table_continuous effect_size_ci display shows brackets", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    effect_size_ci = TRUE,
  )
  display <- spicy:::build_display_df(
    out,
    2L,
    ".",
    0.95,
    show_effect_size = TRUE,
    show_effect_size_ci = TRUE
  )
  expect_true("ES" %in% names(display))
  expect_match(display$ES[1], "\\[")
})

test_that("table_continuous print works with effect_size", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    effect_size_ci = TRUE
  )
  expect_output(print(out))
})

test_that("table_continuous es values only on first row of each variable block", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    output = "data.frame"
  )
  # 3 groups x 1 var = 3 rows; es_value only on first row
  expect_false(is.na(out$es_value[1]))
  expect_true(is.na(out$es_value[2]))
  expect_true(is.na(out$es_value[3]))
})

test_that("table_continuous gt with effect_size has ES spanner", {
  skip_if_not_installed("gt")
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    output = "gt"
  )
  spanners <- out[["_spanners"]]
  labels <- unlist(spanners$spanner_label)
  expect_true("ES" %in% labels)
})

# ---- effect size: do_es without do_test (no p_value/statistic) ----

test_that("table_continuous effect_size=TRUE alone adds es but not test columns", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    output = "data.frame"
  )
  expect_true("es_type" %in% names(out))
  expect_true("es_value" %in% names(out))
  expect_true("test_type" %in% names(out))
  expect_false(is.na(out$es_value[1]))
  # display should not show Test/p when show_p/show_statistic are FALSE
  display <- spicy:::build_display_df(
    out,
    2L,
    ".",
    0.95,
    show_effect_size = TRUE
  )
  expect_true("ES" %in% names(display))
  expect_false("Test" %in% names(display))
  expect_false("p" %in% names(display))
})

test_that("table_continuous does not warn about test when effect_size uses it", {
  expect_no_warning(
    out <- table_continuous(
      iris,
      select = Sepal.Length,
      by = Species,
      test = "student",
      effect_size = TRUE,
      p_value = FALSE,
      statistic = FALSE,
      output = "data.frame"
    )
  )

  expect_equal(out$es_type[1], "eta_sq")
  expect_equal(out$test_type[1], "anova")
})

# ---- effect size: student test ----

test_that("table_continuous effect_size with test='student' 2 groups gives hedges_g", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  out <- table_continuous(
    df,
    select = Sepal.Length,
    by = Species,
    test = "student",
    effect_size = TRUE,
    output = "data.frame"
  )
  expect_equal(out$es_type[1], "hedges_g")
  expect_equal(out$test_type[1], "student_t")
})

test_that("table_continuous effect_size with test='student' 3+ groups gives eta_sq", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    test = "student",
    effect_size = TRUE,
    output = "data.frame"
  )
  expect_equal(out$es_type[1], "eta_sq")
  expect_equal(out$test_type[1], "anova")
})

# ---- effect size: decimal_mark comma ----

test_that("table_continuous effect_size display uses comma decimal_mark", {
  df <- iris[iris$Species != "virginica", ]
  df$Species <- droplevels(df$Species)
  out <- table_continuous(
    df,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    effect_size_ci = TRUE,
    decimal_mark = ",",
  )
  display <- spicy:::build_display_df(
    out,
    2L,
    ",",
    0.95,
    show_effect_size = TRUE,
    show_effect_size_ci = TRUE
  )
  es_cell <- display$ES[1]
  expect_true(grepl(",", es_cell))
})

# ---- effect size: untestable groups (n=1) ----

test_that("table_continuous effect_size with untestable group gives NA", {
  df <- data.frame(
    g = c("A", "B"),
    x = c(1, 2)
  )
  out <- table_continuous(
    df,
    by = g,
    effect_size = TRUE,
    output = "data.frame"
  )
  expect_true("es_type" %in% names(out))
  expect_true(is.na(out$es_value[1]))
})

# ---- effect size: nonparametric rank-biserial CI small n ----

test_that("table_continuous rank-biserial r_rb has CI with small n", {
  df <- data.frame(
    g = c("A", "A", "B", "B"),
    x = c(1, 2, 3, 4)
  )
  out <- table_continuous(
    df,
    by = g,
    test = "nonparametric",
    effect_size = TRUE,
    output = "data.frame"
  )
  expect_equal(out$es_type[1], "r_rb")
  # n_total = 4 > 3, so CI should be computed
  expect_false(is.na(out$es_ci_lower[1]))
})

# ---- effect size: multiple variables ----

test_that("table_continuous effect_size with multiple variables", {
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width),
    by = Species,
    effect_size = TRUE,
    output = "data.frame"
  )
  sl_es <- out$es_value[out$variable == "Sepal.Length" & !is.na(out$es_value)]
  sw_es <- out$es_value[out$variable == "Sepal.Width" & !is.na(out$es_value)]
  expect_length(sl_es, 1L)
  expect_length(sw_es, 1L)
  expect_false(identical(sl_es, sw_es))
})

# ---- effect size: export formats ----

test_that("table_continuous tinytable with effect_size works", {
  skip_if_not_installed("tinytable")
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    effect_size_ci = TRUE,
    output = "tinytable"
  )
  expect_true(methods::is(out, "tinytable"))
})

test_that("table_continuous flextable with effect_size works", {
  skip_if_not_installed("flextable")
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    output = "flextable"
  )
  expect_s3_class(out, "flextable")
})

test_that("table_continuous excel with effect_size works", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    output = "excel",
    excel_path = tmp
  )
  expect_true(file.exists(tmp))
})

test_that("table_continuous word with effect_size works", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    effect_size_ci = TRUE,
    output = "word",
    word_path = tmp
  )
  expect_true(file.exists(tmp))
})

test_that("table_continuous gt with effect_size_ci has ES spanner", {
  skip_if_not_installed("gt")
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    effect_size_ci = TRUE,
    output = "gt"
  )
  dat <- out[["_data"]]
  expect_true("ES" %in% names(dat))
})

# ---- effect size: epsilon_sq_boot_ci internals ----

test_that("epsilon_sq_boot_ci returns valid CI", {
  set.seed(1)
  x <- c(rnorm(30, 0), rnorm(30, 1), rnorm(30, 2))
  g <- rep(c("A", "B", "C"), each = 30)
  ci <- spicy:::epsilon_sq_boot_ci(x, g, 3L, 0.95)
  expect_length(ci, 2L)
  expect_true(ci[1] < ci[2])
})

test_that("epsilon_sq_boot_ci handles resamples that lose a group", {
  # Small groups: many resamples will miss a group (triggers NA branch)
  x <- c(1, 2, 10, 11, 20, 21)
  g <- rep(c("A", "B", "C"), each = 2)
  ci <- spicy:::epsilon_sq_boot_ci(x, g, 3L, 0.95)
  expect_length(ci, 2L)
  # Still enough valid resamples to produce a CI
  expect_false(is.na(ci[1]))
})

test_that("epsilon_sq_boot_ci returns NA when too few valid resamples", {
  # Many groups with 1 obs each: most resamples miss groups
  x <- 1:8
  g <- paste0("G", 1:8)
  ci <- spicy:::epsilon_sq_boot_ci(x, g, 8L, 0.95)
  expect_length(ci, 2L)
  expect_true(is.na(ci[1]))
  expect_true(is.na(ci[2]))
})

test_that("epsilon_sq_boot_ci does not alter user RNG state", {
  set.seed(123)
  before <- .Random.seed
  x <- c(rnorm(30, 0), rnorm(30, 1), rnorm(30, 2))
  g <- rep(c("A", "B", "C"), each = 30)
  set.seed(99)
  rng_before <- .Random.seed
  spicy:::epsilon_sq_boot_ci(x, g, 3L, 0.95)
  rng_after <- .Random.seed
  # RNG should have advanced (not been reset to a fixed seed)
  expect_false(identical(rng_before, rng_after))
})

test_that("epsilon squared is clamped to 0 when H is small", {
  # Create data where groups are nearly identical -> H ≈ 0 -> raw ε² < 0
  set.seed(1)
  x <- rnorm(90, mean = 5, sd = 10)
  g <- rep(c("A", "B", "C"), each = 30)
  out <- table_continuous(
    data.frame(x = x, g = g),
    select = "x",
    by = "g",
    test = "nonparametric",
    effect_size = TRUE
  )
  eps_vals <- out$es_value[!is.na(out$es_value)]
  expect_true(all(eps_vals >= 0))
})

test_that("eta_sq_ci returns lower = 0 for very small F", {
  # F close to 0: ncp_lower should be 0
  ci <- spicy:::eta_sq_ci(0.5, df1 = 2, df2 = 100, ci_level = 0.95)
  expect_equal(ci[1], 0)
  expect_false(is.na(ci[2]))
})

# ---- effect size: fmt_es empty when NA ----

test_that("table_continuous ES display is empty for subsequent group rows", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
  )
  display <- spicy:::build_display_df(
    out,
    2L,
    ".",
    0.95,
    show_effect_size = TRUE
  )
  expect_equal(display$ES[2], "")
  expect_equal(display$ES[3], "")
})

# ---- effect size: p_value + statistic + effect_size together ----

test_that("table_continuous all columns together: p + stat + es", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    p_value = TRUE,
    statistic = TRUE,
    effect_size = TRUE,
    effect_size_ci = TRUE,
  )
  display <- spicy:::build_display_df(
    out,
    2L,
    ".",
    0.95,
    show_p = TRUE,
    show_statistic = TRUE,
    show_effect_size = TRUE,
    show_effect_size_ci = TRUE
  )
  expect_true(all(c("Test", "p", "ES") %in% names(display)))
  expect_match(display$ES[1], "\\[")
})

test_that("table_continuous print works with all columns", {
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width),
    by = Species,
    p_value = TRUE,
    statistic = TRUE,
    effect_size = TRUE,
    effect_size_ci = TRUE
  )
  expect_output(print(out))
})

# ---- print: auto-compact padding ----

test_that("print auto-selects compact padding for narrow console", {
  out <- table_continuous(iris, select = Sepal.Length, by = Species)
  old_w <- getOption("width")
  on.exit(options(width = old_w), add = TRUE)
  # Normal padding should produce wider output than compact
  options(width = 80)
  expect_output(print(out))
  # Very narrow: still prints without error
  options(width = 40)
  expect_output(print(out))
})

# ---- print method %||% fallback branches ----

test_that("print.spicy_continuous_table uses defaults when attributes are missing", {
  df <- data.frame(x = 1:10)
  out <- table_continuous(df)
  # Strip attributes to test fallback branches
  attr(out, "digits") <- NULL
  attr(out, "decimal_mark") <- NULL
  attr(out, "ci_level") <- NULL
  attr(out, "data_name") <- NULL
  expect_output(print(out))
})

# ---- coverage: multi-variable exports with separator rows ----

test_that("table_continuous gt with multiple variables has separator rows", {
  skip_if_not_installed("gt")
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width),
    by = Species,
    p_value = TRUE,
    statistic = TRUE,
    effect_size = TRUE,
    output = "gt"
  )
  expect_s3_class(out, "gt_tbl")
})

test_that("table_continuous flextable with multiple variables has separator rows", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width),
    by = Species,
    p_value = TRUE,
    output = "flextable"
  )
  expect_s3_class(out, "flextable")
})

test_that("table_continuous excel with multiple variables has separator rows", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width),
    by = Species,
    p_value = TRUE,
    output = "excel",
    excel_path = tmp
  )
  expect_true(file.exists(tmp))
})

test_that("table_continuous tinytable with p + stat + es works", {
  skip_if_not_installed("tinytable")
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width),
    by = Species,
    p_value = TRUE,
    statistic = TRUE,
    effect_size = TRUE,
    output = "tinytable"
  )
  expect_true(methods::is(out, "tinytable"))
})

# ---- coverage: display formatting branches ----

test_that("build_display_df formats p >= 0.001 correctly", {
  df <- data.frame(g = rep(c("A", "B"), each = 30), x = rnorm(60))
  out <- table_continuous(df, by = g, p_value = TRUE)
  display <- spicy:::build_display_df(
    out,
    2L,
    ".",
    0.95,
    show_p = TRUE
  )
  # p might be >= 0.001 for random data, but ensure column exists
  expect_true("p" %in% names(display))
})

test_that("build_display_df fmt_test with decimal comma", {
  df <- data.frame(g = rep(c("A", "B"), each = 20), x = rnorm(40))
  out <- table_continuous(
    df,
    by = g,
    statistic = TRUE,
    decimal_mark = ",",
  )
  display <- spicy:::build_display_df(
    out,
    2L,
    ",",
    0.95,
    show_statistic = TRUE
  )
  # t-test statistic should use comma
  expect_true(grepl(",", display$Test[1]))
})

test_that("build_display_df fmt_p with decimal comma", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    p_value = TRUE,
    decimal_mark = ",",
  )
  display <- spicy:::build_display_df(
    out,
    2L,
    ",",
    0.95,
    show_p = TRUE
  )
  # Very small p: "< ,001"
  expect_match(display$p[1], ",")
})

test_that("build_display_df fmt_p large p with decimal comma", {
  set.seed(123)
  df <- data.frame(
    g = rep(c("A", "B"), each = 20),
    x = rnorm(40, mean = 5, sd = 2)
  )
  out <- table_continuous(
    df,
    by = g,
    p_value = TRUE,
    decimal_mark = ",",
  )
  display <- spicy:::build_display_df(
    out,
    2L,
    ",",
    0.95,
    show_p = TRUE
  )
  # p >= 0.001 should be formatted with comma
  expect_true(grepl(",", display$p[1]))
})

test_that("build_display_df fmt_test F-test with decimal comma", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    statistic = TRUE,
    decimal_mark = ",",
  )
  display <- spicy:::build_display_df(
    out,
    2L,
    ",",
    0.95,
    show_statistic = TRUE
  )
  expect_true(grepl(",", display$Test[1]))
})
