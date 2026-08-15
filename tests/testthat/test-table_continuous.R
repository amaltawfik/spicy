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
      "median",
      "q1",
      "q3",
      "iqr",
      "med_ci_lower",
      "med_ci_upper",
      "n",
      # Stable schema (decision 17): always present, NA without
      # `weights` -- programmatic consumers get one shape, not two.
      "weighted_n"
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

test_that("table_continuous non-factor by groups in order of appearance", {
  # Audit phase 2, finding 17: the family convention (locked for
  # table_categorical()) is order of first appearance for a
  # non-factor `by`; table_continuous() used to sort.
  df <- data.frame(g = c("Z", "A", "Z", "A"), x = c(1, 2, 3, 4))
  out <- table_continuous(df, by = g, output = "data.frame")
  expect_equal(out$group, c("Z", "A"))

  # Numeric by, same convention, matching table_categorical().
  set.seed(7)
  dn <- data.frame(gnum = sample(c(1, 2, 3), 60, TRUE), xx = rnorm(60))
  tc <- table_continuous(dn, select = xx, by = gnum, output = "long")
  dcat <- data.frame(yy = factor(rep(c("u", "v"), 30)), gnum = dn$gnum)
  tcat <- table_categorical(dcat, select = yy, by = gnum, output = "long")
  expect_equal(
    unique(tc$group),
    setdiff(unique(tcat$group), "Total")
  )
})

test_that("the test direction follows the displayed group order", {
  # Companion to finding 17: the formula interface of t.test() would
  # re-sort a bare character `by`; the statistic's sign must match
  # the displayed first-minus-second convention.
  df <- data.frame(
    g = c("Z", "A", "Z", "A", "Z", "A"),
    x = c(1, 2, 3, 4, 5, 6)
  )
  out <- table_continuous(
    df,
    by = g,
    p_value = TRUE,
    statistic = TRUE,
    output = "long"
  )
  expect_equal(unique(out$group), c("Z", "A"))
  tt <- stats::t.test(df$x[df$g == "Z"], df$x[df$g == "A"])
  expect_equal(out$statistic[1], unname(tt$statistic), tolerance = 1e-12)
  expect_equal(out$p.value[1], tt$p.value, tolerance = 1e-12)
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
    statistic = TRUE,
    p_value = FALSE
  )
  expect_false(attr(out, "show_p"))
  expect_true(attr(out, "show_statistic"))
})

test_that("table_continuous p_value auto-shows when by is supplied", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "data.frame"
  )
  expect_true("p.value" %in% names(out))
  expect_true(attr(out, "show_p"))
  expect_false(is.na(out$p.value[1]))
})

test_that("table_continuous p_value stays hidden when by is absent", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    output = "data.frame"
  )
  expect_false("p.value" %in% names(out))
  expect_false(isTRUE(attr(out, "show_p")))
})

test_that("table_continuous p_value = FALSE suppresses column when by is supplied", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    p_value = FALSE,
    output = "data.frame"
  )
  expect_false("p.value" %in% names(out))
  expect_false(attr(out, "show_p"))
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
      test = "student",
      p_value = FALSE
    ),
    "ignored"
  )
})

test_that("the test-ignored warning states the full trigger condition", {
  expect_warning(
    table_continuous(
      iris,
      select = Sepal.Length,
      by = Species,
      test = "student",
      p_value = FALSE,
      statistic = FALSE,
      output = "data.frame"
    ),
    "`p_value`, `statistic`, `effect_size`, and `effect_size_ci`",
    class = "spicy_ignored_arg"
  )
  # An effect-size request keeps `test` active: no warning.
  expect_no_warning(
    table_continuous(
      iris,
      select = Sepal.Length,
      by = Species,
      test = "student",
      p_value = FALSE,
      statistic = FALSE,
      effect_size = "eta_sq",
      output = "data.frame"
    )
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

test_that("table_continuous uses dedicated digits for effect sizes", {
  out <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = TRUE,
    effect_size_ci = TRUE,
    digits = 1,
    effect_size_digits = 3
  )
  display <- spicy:::build_display_df(
    out,
    1L,
    ".",
    0.95,
    show_effect_size = TRUE,
    show_effect_size_ci = TRUE,
    effect_size_digits = 3L
  )

  expect_equal(attr(out, "effect_size_digits"), 3L)
  expect_match(display$M[1], "\\.[0-9]{1}$")
  expect_match(display$ES[1], "= [0-9]+\\.[0-9]{3}")
  expect_match(display$ES[1], "\\[[0-9]+\\.[0-9]{3}, [0-9]+\\.[0-9]{3}\\]")
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
  expect_error(
    table_continuous(df, effect_size_digits = -1),
    "effect_size_digits"
  )
  expect_error(
    table_continuous(df, effect_size_digits = "a"),
    "effect_size_digits"
  )
  expect_error(
    table_continuous(df, effect_size_digits = NA),
    "effect_size_digits"
  )
})

test_that("table_continuous validates decimal_mark", {
  df <- data.frame(x = 1:5)
  expect_error(table_continuous(df, decimal_mark = "--"), "decimal_mark")
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
  expect_error(table_continuous(df, drop_na = "yes"), "drop_na")
  expect_error(table_continuous(df, verbose = NA), "verbose")
})

test_that("table_continuous warns when NA present in by column", {
  df <- data.frame(
    x = 1:6,
    g = c("A", "A", "B", "B", NA, NA)
  )
  expect_warning(
    table_continuous(df, select = "x", by = "g", output = "data.frame"),
    class = "spicy_dropped_na"
  )
  out <- suppressWarnings(
    table_continuous(df, select = "x", by = "g", output = "data.frame")
  )
  # Only A and B groups should be present
  expect_equal(sort(unique(out$group)), c("A", "B"))
})

test_that("table_continuous discloses removed NAs in a table note", {
  df <- data.frame(x = c(1:7, NA), y = c(NA, NA, 3:8))
  out <- table_continuous(df, select = c("x", "y"))
  expect_equal(
    attr(out, "missing_note"),
    "Missing values removed: x (1), y (2)."
  )
  printed <- paste(capture.output(print(out)), collapse = "\n")
  expect_match(printed, "Missing values removed: x (1), y (2).", fixed = TRUE)
  # No NAs -> no note.
  clean <- table_continuous(data.frame(x = 1:5), select = "x")
  expect_null(attr(clean, "missing_note"))
})

test_that("drop_na = TRUE discloses removed missing-by rows in the note", {
  df <- data.frame(
    x = c(1:6, NA),
    g = c("A", "A", "B", "B", NA, NA, "A")
  )
  out <- suppressWarnings(table_continuous(df, select = "x", by = "g"))
  expect_equal(
    attr(out, "missing_note"),
    "Missing values removed: x (1). Rows with missing g removed: 2."
  )
})

test_that("drop_na = FALSE displays a (Missing) group and tests the observed", {
  df <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8),
    g = c("A", "A", "B", "B", "A", "B", NA, NA)
  )
  # No spicy_dropped_na warning: nothing is removed.
  out <- table_continuous(
    df,
    select = "x",
    by = "g",
    drop_na = FALSE,
    output = "data.frame"
  )
  expect_equal(unique(out$group), c("A", "B", "(Missing)"))
  expect_equal(out$n[out$group == "(Missing)"], 2L)
  # Show the missing, test the observed: same p as drop_na = TRUE.
  ref <- suppressWarnings(
    table_continuous(df, select = "x", by = "g", output = "data.frame")
  )
  expect_equal(out$p.value[1], ref$p.value[1])
})

test_that("drop_na = FALSE guards against a real (Missing) group value", {
  df <- data.frame(
    x = 1:6,
    g = c("(Missing)", "(Missing)", "B", "B", NA, NA)
  )
  out <- table_continuous(
    df,
    select = "x",
    by = "g",
    drop_na = FALSE,
    output = "data.frame"
  )
  expect_equal(unique(out$group), c("(Missing)", "B", "(Missing_1)"))
})

test_that("drop_na = FALSE guards against a declared-only (Missing) level", {
  # Audit phase 2 delta (sibling of R2/R3/R8): the collision guard
  # scanned only observed values, so a factor `by` DECLARING an
  # unobserved "(Missing)" level plus real NAs used to display the
  # missing-`by` group twice under the same duplicated label.
  df <- data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    g = factor(c("u", "u", "v", "v", NA, NA), levels = c("u", "v", "(Missing)"))
  )
  out <- table_continuous(
    df,
    select = "y",
    by = "g",
    drop_na = FALSE,
    output = "data.frame"
  )
  expect_equal(unique(out$group), c("u", "v", "(Missing)", "(Missing_1)"))
  expect_identical(out$n[out$group == "(Missing)"], 0L)
  expect_identical(out$n[out$group == "(Missing_1)"], 2L)
})

test_that("drop_na = FALSE without by warns spicy_ignored_arg", {
  df <- data.frame(x = c(1:4, NA))
  expect_warning(
    table_continuous(df, select = "x", drop_na = FALSE, output = "data.frame"),
    class = "spicy_ignored_arg"
  )
})

test_that("table_continuous raw outputs carry the missing_note attribute", {
  # Reversal of the pre-0.13 contract: a raw frame that a pipeline
  # re-renders itself must still be able to state what was removed.
  df <- data.frame(x = c(1:7, NA))
  out <- table_continuous(df, select = "x", output = "data.frame")
  expect_identical(attr(out, "missing_note"), "Missing values removed: x (1).")
  long <- table_continuous(df, select = "x", output = "long")
  expect_identical(attr(long, "missing_note"), "Missing values removed: x (1).")
  # Nothing removed -> nothing to disclose.
  clean <- table_continuous(
    data.frame(x = 1:5),
    select = "x",
    output = "data.frame"
  )
  expect_null(attr(clean, "missing_note"))
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
  # p should be very small -> "<.001" (shared format_p_value helper,
  # no non-breaking space; alignment is now handled by decimal_align).
  expect_true(any(startsWith(p_col, "<")))
  expect_true(any(grepl("\\.001", p_col)))
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
  skip_if_not_installed("openxlsx2")
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
  skip_if_not_installed("openxlsx2")
  expect_error(
    table_continuous(iris, select = c(Sepal.Length), output = "excel"),
    "excel_path"
  )
})

test_that("table_continuous excel errors with empty path", {
  skip_if_not_installed("openxlsx2")
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
  # The writer is mocked, never the real system clipboard: a test run
  # must not overwrite what the user has copied.
  testthat::local_mocked_bindings(
    write_clip = function(text, ...) invisible(text),
    clipr_available = function(...) TRUE,
    .package = "clipr"
  )
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
  skip_if_not_installed("openxlsx2")
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
  testthat::local_mocked_bindings(
    write_clip = function(text, ...) invisible(text),
    clipr_available = function(...) TRUE,
    .package = "clipr"
  )
  expect_message(
    out <- table_continuous(
      iris,
      select = Sepal.Length,
      by = Species,
      output = "clipboard"
    ),
    "copied to clipboard",
    class = "spicy_info"
  )
  expect_s3_class(out, "data.frame")
  expect_true("Group" %in% names(out))
})

# ---- non-default clipboard_delim ----

test_that("table_continuous clipboard with custom delimiter works", {
  skip_if_not_installed("clipr")
  testthat::local_mocked_bindings(
    write_clip = function(text, ...) invisible(text),
    clipr_available = function(...) TRUE,
    .package = "clipr"
  )
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
  skip_if_not_installed("openxlsx2")
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
  skip_if_not_installed("openxlsx2")
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

test_that("effect-size CI bracket separator switches with decimal_mark", {
  df_two <- iris[iris$Species != "virginica", ]
  df_two$Species <- droplevels(df_two$Species)

  raw <- table_continuous(
    df_two,
    select = Sepal.Length,
    by = Species,
    test = "student",
    effect_size = TRUE,
    effect_size_ci = TRUE,
    output = "data.frame"
  )

  display_dot <- spicy:::build_display_df(
    raw,
    digits = 2L,
    decimal_mark = ".",
    ci_level = 0.95,
    show_p = TRUE,
    show_statistic = FALSE,
    show_effect_size = TRUE,
    show_effect_size_ci = TRUE,
    effect_size_digits = 2L
  )
  display_comma <- spicy:::build_display_df(
    raw,
    digits = 2L,
    decimal_mark = ",",
    ci_level = 0.95,
    show_p = TRUE,
    show_statistic = FALSE,
    show_effect_size = TRUE,
    show_effect_size_ci = TRUE,
    effect_size_digits = 2L
  )

  es_dot <- display_dot$ES[1]
  es_comma <- display_comma$ES[1]

  expect_match(es_dot, "\\[\\-?[0-9.]+, \\-?[0-9.]+\\]")
  expect_match(es_comma, "\\[\\-?[0-9,]+; \\-?[0-9,]+\\]")
  expect_false(grepl(";", es_dot, fixed = TRUE))
  expect_false(grepl(", ", es_comma, fixed = TRUE))
})

# ---- harmonisation with table_continuous_lm() (Phase 1) ------------------

test_that("effect_size accepts logical TRUE/FALSE as silent aliases", {
  out_T <- table_continuous(
    sleep,
    select = extra,
    by = group,
    effect_size = TRUE,
    output = "long"
  )
  out_auto <- table_continuous(
    sleep,
    select = extra,
    by = group,
    effect_size = "auto",
    output = "long"
  )
  out_F <- table_continuous(
    sleep,
    select = extra,
    by = group,
    effect_size = FALSE,
    output = "long"
  )
  out_none <- table_continuous(
    sleep,
    select = extra,
    by = group,
    effect_size = "none",
    output = "long"
  )

  # TRUE == "auto" (Hedges' g for two-group parametric)
  expect_equal(unique(out_T$es_type[!is.na(out_T$es_type)]), "hedges_g")
  expect_equal(out_T$es_value, out_auto$es_value)

  # FALSE == "none" (no effect-size columns populated)
  expect_true(all(is.na(out_F$es_type)))
  expect_true(all(is.na(out_none$es_type)))
})

test_that("effect_size character explicit choices dispatch correctly", {
  out_g <- table_continuous(
    sleep,
    select = extra,
    by = group,
    effect_size = "hedges_g",
    output = "long"
  )
  expect_equal(unique(out_g$es_type[!is.na(out_g$es_type)]), "hedges_g")

  out_eta <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    effect_size = "eta_sq",
    output = "long"
  )
  expect_equal(unique(out_eta$es_type[!is.na(out_eta$es_type)]), "eta_sq")

  out_rrb <- table_continuous(
    sleep,
    select = extra,
    by = group,
    test = "nonparametric",
    effect_size = "r_rb",
    output = "long"
  )
  expect_equal(unique(out_rrb$es_type[!is.na(out_rrb$es_type)]), "r_rb")

  out_eps <- table_continuous(
    iris,
    select = Sepal.Length,
    by = Species,
    test = "nonparametric",
    effect_size = "epsilon_sq",
    output = "long"
  )
  expect_equal(unique(out_eps$es_type[!is.na(out_eps$es_type)]), "epsilon_sq")
})

test_that("effect_size mismatched explicit choice errors clearly", {
  # eta_sq requires k > 2; sleep has 2 groups
  expect_error(
    table_continuous(sleep, select = extra, by = group, effect_size = "eta_sq"),
    "requires more than two groups"
  )
  # hedges_g requires k = 2; iris$Species has 3
  expect_error(
    table_continuous(
      iris,
      select = Sepal.Length,
      by = Species,
      effect_size = "hedges_g"
    ),
    "requires exactly two groups"
  )
  # r_rb is nonparametric; with parametric test, error
  expect_error(
    table_continuous(
      sleep,
      select = extra,
      by = group,
      test = "welch",
      effect_size = "r_rb"
    ),
    "nonparametric measure"
  )
  # hedges_g is parametric; with nonparametric test, error
  expect_error(
    table_continuous(
      sleep,
      select = extra,
      by = group,
      test = "nonparametric",
      effect_size = "hedges_g"
    ),
    "parametric measure"
  )
})

test_that("effect_size invalid character value rejected with a classed error", {
  expect_error(
    table_continuous(
      sleep,
      select = extra,
      by = group,
      effect_size = "bogus_metric"
    ),
    "`effect_size` must be one of",
    class = "spicy_invalid_input"
  )
})

test_that("effect_size logical NA / non-scalar errors", {
  expect_error(
    table_continuous(sleep, select = extra, by = group, effect_size = NA),
    "must be a single TRUE/FALSE or character"
  )
  expect_error(
    table_continuous(
      sleep,
      select = extra,
      by = group,
      effect_size = c(TRUE, FALSE)
    ),
    "must be a single TRUE/FALSE or character"
  )
})

test_that("p_digits validates and renders accordingly", {
  expect_error(
    table_continuous(sleep, select = extra, by = group, p_digits = 0),
    "p_digits"
  )
  expect_error(
    table_continuous(sleep, select = extra, by = group, p_digits = NA_integer_),
    "p_digits"
  )

  # p_digits = 4 -> rendered p column should show 4 digits past decimal
  out <- table_continuous(sleep, select = extra, by = group, p_digits = 4)
  expect_s3_class(out, "spicy_continuous_table")
  display <- spicy:::build_display_df(
    out,
    digits = 2,
    decimal_mark = ".",
    ci_level = 0.95,
    show_p = TRUE,
    show_statistic = FALSE,
    show_effect_size = FALSE,
    show_effect_size_ci = FALSE,
    p_digits = 4L
  )
  p_col <- display[["p"]][nzchar(display[["p"]])]
  # Either ".####" with 4 digits or "<.0001"
  expect_true(any(grepl("\\.\\d{4}|<\\.0001", p_col)))
})

test_that("align argument validates and stores the choice", {
  for (a in c("decimal", "center", "right")) {
    out <- table_continuous(sleep, select = extra, by = group, align = a)
    expect_equal(attr(out, "align"), a)
  }
  expect_error(
    table_continuous(sleep, select = extra, by = group, align = "bogus"),
    "`align` must be one of",
    class = "spicy_invalid_input"
  )
})

test_that("show_n = FALSE drops the n column from the rendered display df", {
  out <- table_continuous(
    sleep,
    select = extra,
    by = group,
    show_n = FALSE
  )
  expect_false(attr(out, "show_n"))
  display <- spicy:::build_display_df(
    out,
    digits = 2,
    decimal_mark = ".",
    ci_level = 0.95,
    show_p = TRUE,
    show_statistic = FALSE,
    show_n = FALSE,
    show_ci = TRUE,
    show_effect_size = FALSE,
    show_effect_size_ci = FALSE,
    p_digits = 3L
  )
  expect_false("n" %in% names(display))
})

test_that("ci = FALSE drops the CI columns from the rendered display df", {
  out <- table_continuous(sleep, select = extra, by = group, ci = FALSE)
  expect_false(attr(out, "show_ci"))
  display <- spicy:::build_display_df(
    out,
    digits = 2,
    decimal_mark = ".",
    ci_level = 0.95,
    show_p = TRUE,
    show_statistic = FALSE,
    show_n = TRUE,
    show_ci = FALSE,
    show_effect_size = FALSE,
    show_effect_size_ci = FALSE,
    p_digits = 3L
  )
  expect_false(any(grepl("CI", names(display))))
})

test_that("output = 'long' is a synonym for output = 'data.frame'", {
  out_df <- table_continuous(
    sleep,
    select = extra,
    by = group,
    output = "data.frame"
  )
  out_lg <- table_continuous(sleep, select = extra, by = group, output = "long")
  expect_identical(unclass(out_df), unclass(out_lg))
})

# ---- broom S3 methods -----------------------------------------------------

test_that("as.data.frame() strips spicy classes and rendering attrs", {
  out <- table_continuous(sleep, select = extra, by = group)
  df <- as.data.frame(out)
  expect_true(inherits(df, "data.frame"))
  expect_false("spicy_continuous_table" %in% class(df))
  expect_false("spicy_table" %in% class(df))
  # Rendering-only attributes stripped
  expect_null(attr(df, "digits"))
  expect_null(attr(df, "decimal_mark"))
  expect_null(attr(df, "align"))
  # group_var preserved as provenance
  expect_equal(attr(df, "group_var"), "group")
})

test_that("as_tibble() returns a tbl_df", {
  skip_if_not_installed("tibble")
  out <- table_continuous(sleep, select = extra, by = group)
  tb <- tibble::as_tibble(out)
  expect_s3_class(tb, "tbl_df")
})

test_that("tidy() returns one row per (variable x group) with broom columns", {
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width),
    by = Species
  )
  td <- broom::tidy(out)
  # 2 outcomes x 3 species = 6 rows
  expect_equal(nrow(td), 6L)
  expect_setequal(
    names(td),
    c(
      "outcome",
      "label",
      "group",
      "estimate",
      "std.error",
      "conf.low",
      "conf.high",
      "n",
      "min",
      "max",
      "sd"
    )
  )
  # estimate equals empirical mean
  setosa_sl <- td[td$outcome == "Sepal.Length" & td$group == "setosa", ]
  expect_equal(
    setosa_sl$estimate,
    mean(iris$Sepal.Length[iris$Species == "setosa"])
  )
})

test_that("tidy() works without by (one row per variable)", {
  out <- table_continuous(iris, select = c(Sepal.Length, Sepal.Width))
  td <- broom::tidy(out)
  expect_equal(nrow(td), 2L)
  expect_false("group" %in% names(td))
})

test_that("glance() returns one row per outcome with omnibus test + ES", {
  out <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width),
    by = Species,
    effect_size = "eta_sq"
  )
  gl <- broom::glance(out)
  expect_equal(nrow(gl), 2L)
  expect_setequal(
    names(gl),
    c(
      "outcome",
      "label",
      "test_type",
      "statistic",
      "df",
      "df.residual",
      "p.value",
      "es_type",
      "es_value",
      "es_ci_lower",
      "es_ci_upper",
      "n_total"
    )
  )
  expect_equal(unique(gl$es_type), "eta_sq")
  expect_true(all(gl$n_total == 150L))
})

test_that("glance() without by returns NA test/ES, populated n_total", {
  out <- table_continuous(iris, select = Sepal.Length)
  gl <- broom::glance(out)
  expect_equal(nrow(gl), 1L)
  expect_true(is.na(gl$test_type))
  expect_true(is.na(gl$p.value))
  expect_equal(gl$n_total, 150L)
})

# ---- ci = FALSE / show_n = FALSE: structural omission across engines -----

test_that("ci = FALSE / show_n = FALSE render across every output (smoke matrix)", {
  Sys.setenv(CLIPR_ALLOW = "TRUE")
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(text, ...) invisible(text),
    .package = "clipr"
  )
  cases <- expand.grid(
    ci = c(TRUE, FALSE),
    show_n = c(TRUE, FALSE),
    KEEP.OUT.ATTRS = FALSE
  )
  engines_text <- c("default", "data.frame", "long", "clipboard")
  for (i in seq_len(nrow(cases))) {
    for (eng in engines_text) {
      expect_no_error(
        table_continuous(
          sleep,
          select = extra,
          by = group,
          ci = cases$ci[i],
          show_n = cases$show_n[i],
          output = eng
        )
      )
    }
  }
})

test_that("raw data.frame / long outputs always carry analytic ci_lower / ci_upper / n", {
  # `ci = FALSE` and `show_n = FALSE` only suppress the formatted
  # display columns; the raw analytic data exposed via
  # `output = "data.frame"` / `"long"` always carries the underlying
  # CI bounds and `n` so downstream code (broom::tidy, gtsummary, ...)
  # has access to them.
  out_df <- table_continuous(
    sleep,
    select = extra,
    by = group,
    ci = FALSE,
    show_n = FALSE,
    output = "data.frame"
  )
  out_lg <- table_continuous(
    sleep,
    select = extra,
    by = group,
    ci = FALSE,
    show_n = FALSE,
    output = "long"
  )
  expect_true(all(c("ci_lower", "ci_upper", "n") %in% names(out_df)))
  expect_true(all(c("ci_lower", "ci_upper", "n") %in% names(out_lg)))
})

test_that("ci = FALSE structurally removes CI cols from the build_display_df output", {
  out <- table_continuous(sleep, select = extra, by = group, ci = FALSE)
  display <- spicy:::build_display_df(
    out,
    digits = 2,
    decimal_mark = ".",
    ci_level = 0.95,
    show_p = TRUE,
    show_statistic = FALSE,
    show_n = TRUE,
    show_ci = FALSE,
    show_effect_size = FALSE,
    show_effect_size_ci = FALSE,
    p_digits = 3L
  )
  expect_false(any(grepl("CI", names(display))))
})

test_that("show_n = FALSE structurally removes n col from build_display_df output", {
  out <- table_continuous(sleep, select = extra, by = group, show_n = FALSE)
  display <- spicy:::build_display_df(
    out,
    digits = 2,
    decimal_mark = ".",
    ci_level = 0.95,
    show_p = TRUE,
    show_statistic = FALSE,
    show_n = FALSE,
    show_ci = TRUE,
    show_effect_size = FALSE,
    show_effect_size_ci = FALSE,
    p_digits = 3L
  )
  expect_false("n" %in% names(display))
})

test_that("ci = FALSE renders structurally without CI in tinytable / gt / flextable", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  expect_true(inherits(
    table_continuous(
      sleep,
      select = extra,
      by = group,
      ci = FALSE,
      output = "tinytable"
    ),
    "tinytable"
  ))
  expect_s3_class(
    table_continuous(
      sleep,
      select = extra,
      by = group,
      ci = FALSE,
      output = "gt"
    ),
    "gt_tbl"
  )
  expect_s3_class(
    table_continuous(
      sleep,
      select = extra,
      by = group,
      ci = FALSE,
      output = "flextable"
    ),
    "flextable"
  )
})

test_that("show_n = FALSE renders structurally without n in tinytable / gt / flextable", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  expect_true(inherits(
    table_continuous(
      sleep,
      select = extra,
      by = group,
      show_n = FALSE,
      output = "tinytable"
    ),
    "tinytable"
  ))
  expect_s3_class(
    table_continuous(
      sleep,
      select = extra,
      by = group,
      show_n = FALSE,
      output = "gt"
    ),
    "gt_tbl"
  )
  expect_s3_class(
    table_continuous(
      sleep,
      select = extra,
      by = group,
      show_n = FALSE,
      output = "flextable"
    ),
    "flextable"
  )
})

test_that("ci = FALSE / show_n = FALSE flow to excel and word", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  for (ci in c(TRUE, FALSE)) {
    for (sn in c(TRUE, FALSE)) {
      tmp_xl <- tempfile(fileext = ".xlsx")
      tmp_dx <- tempfile(fileext = ".docx")
      on.exit(unlink(c(tmp_xl, tmp_dx)), add = TRUE)
      table_continuous(
        sleep,
        select = extra,
        by = group,
        ci = ci,
        show_n = sn,
        output = "excel",
        excel_path = tmp_xl
      )
      table_continuous(
        sleep,
        select = extra,
        by = group,
        ci = ci,
        show_n = sn,
        output = "word",
        word_path = tmp_dx
      )
      expect_true(file.exists(tmp_xl))
      expect_true(file.exists(tmp_dx))
    }
  }
})

test_that("clipboard text reflects ci = FALSE structurally", {
  skip_if_not_installed("clipr")
  Sys.setenv(CLIPR_ALLOW = "TRUE")
  captured <- new.env()
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(text, ...) {
      captured$text <- text
      invisible(text)
    },
    .package = "clipr"
  )
  table_continuous(
    sleep,
    select = extra,
    by = group,
    ci = FALSE,
    output = "clipboard"
  )
  expect_false(grepl("CI", captured$text, fixed = TRUE))

  table_continuous(
    sleep,
    select = extra,
    by = group,
    ci = TRUE,
    output = "clipboard"
  )
  expect_true(grepl("CI", captured$text, fixed = TRUE))
})

# ---- requireNamespace() guards: actionable error when Suggests missing ---

test_that("each rendered output errors with an actionable 'Install package' message when its Suggest is missing", {
  # The defensive guards inside `export_desc_table()` (one
  # `requireNamespace()` per engine, plus one for `officer` inside
  # the word path) are not exercised under normal `devtools::test()`
  # because every Suggests package is installed in the test
  # environment. Mocking `base::requireNamespace` to return FALSE
  # for the targeted package surfaces the guard and verifies that
  # the user receives the canonical actionable message
  # (`"Install package 'X'."`). Using `local_mocked_bindings` keeps
  # the mock scoped to this `test_that` block and never leaks.
  cases <- list(
    list(output = "tinytable", pkg = "tinytable"),
    list(output = "gt", pkg = "gt"),
    list(output = "flextable", pkg = "flextable"),
    list(output = "excel", pkg = "openxlsx2"),
    list(output = "clipboard", pkg = "clipr")
  )
  for (c in cases) {
    local_pkg <- c$pkg
    testthat::local_mocked_bindings(
      requireNamespace = function(package, ...) {
        if (identical(package, local_pkg)) FALSE else TRUE
      },
      .package = "base"
    )
    args <- list(
      data = sleep,
      select = quote(extra),
      by = quote(group),
      output = c$output
    )
    if (identical(c$output, "excel")) {
      args$excel_path <- tempfile(fileext = ".xlsx")
    }
    expect_error(
      do.call(table_continuous, args),
      sprintf("Install package '%s'", c$pkg)
    )
  }
})

test_that("every align value renders cleanly across every rendered engine", {
  # Phase 2/3 added "decimal" / "center" / "right" / "auto" branches
  # in each engine's alignment dispatch (gt, tinytable, flextable,
  # word, excel, clipboard). Default tests cover the "decimal"
  # branch; this matrix smoke-tests the three alternatives so a
  # silent regression in any branch is caught.
  Sys.setenv(CLIPR_ALLOW = "TRUE")
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(text, ...) invisible(text),
    .package = "clipr"
  )
  base_args <- list(
    data = sleep,
    select = quote(extra),
    by = quote(group)
  )
  for (al in c("center", "right")) {
    if (requireNamespace("tinytable", quietly = TRUE)) {
      out <- do.call(
        table_continuous,
        c(
          base_args,
          list(
            output = "tinytable",
            align = al
          )
        )
      )
      expect_true(inherits(out, "tinytable"))
    }
    if (requireNamespace("gt", quietly = TRUE)) {
      out <- do.call(
        table_continuous,
        c(
          base_args,
          list(
            output = "gt",
            align = al
          )
        )
      )
      expect_s3_class(out, "gt_tbl")
    }
    if (requireNamespace("flextable", quietly = TRUE)) {
      out <- do.call(
        table_continuous,
        c(
          base_args,
          list(
            output = "flextable",
            align = al
          )
        )
      )
      expect_s3_class(out, "flextable")
    }
    if (requireNamespace("openxlsx2", quietly = TRUE)) {
      tmp <- tempfile(fileext = ".xlsx")
      on.exit(unlink(tmp), add = TRUE)
      do.call(
        table_continuous,
        c(
          base_args,
          list(
            output = "excel",
            align = al,
            excel_path = tmp
          )
        )
      )
      expect_true(file.exists(tmp))
    }
    if (requireNamespace("clipr", quietly = TRUE)) {
      do.call(
        table_continuous,
        c(
          base_args,
          list(
            output = "clipboard",
            align = al
          )
        )
      )
    }
  }
})

test_that("output = 'word' errors when officer is not installed even if flextable is", {
  # `output = "word"` requires both `flextable` (for the table
  # object) and `officer` (for `save_as_docx`). The guard for
  # `officer` is checked inside the flextable / word branch; mock
  # only the `officer` call to surface the actionable message.
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "officer")) FALSE else TRUE
    },
    .package = "base"
  )
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)
  expect_error(
    table_continuous(
      sleep,
      select = extra,
      by = group,
      output = "word",
      word_path = tmp
    ),
    "Install package 'officer'"
  )
})

test_that("a constant column reports exact SD 0 and a degenerate CI", {
  # Audit phase 2, finding 16: pins the documented behavior -- the
  # numbers are exact, "--" is reserved for undefined statistics.
  out <- table_continuous(
    data.frame(k = rep(5, 4)),
    select = k,
    output = "data.frame"
  )
  expect_equal(out$mean, 5)
  expect_equal(out$sd, 0)
  expect_equal(out$ci_lower, 5)
  expect_equal(out$ci_upper, 5)
  # n = 1: SD / CI undefined -> NA (rendered as "--").
  one <- table_continuous(
    data.frame(k = 5),
    select = k,
    output = "data.frame"
  )
  expect_true(is.na(one$sd))
  expect_true(is.na(one$ci_lower))
})

test_that("a degenerate variable degrades per-variable, others survive", {
  # Audit phase 2, finding 27: t.test()'s "data are essentially
  # constant" error used to kill the whole multi-variable table.
  d <- data.frame(
    cst = c(5, 5, 5, 5, 7, 7, 7, 7),
    ok = c(1.2, 2.3, 1.8, 2.9, 5.4, 6.1, 5.8, 6.3),
    g = factor(rep(c("m", "f"), each = 4))
  )
  expect_warning(
    out <- table_continuous(d, select = c(cst, ok), by = g, output = "long"),
    class = "spicy_undefined_stat"
  )
  cst_rows <- out[out$variable == "cst", ]
  ok_rows <- out[out$variable == "ok", ]
  # Descriptives stay exact for the degenerate variable.
  expect_equal(sort(cst_rows$mean), c(5, 7))
  expect_true(all(is.na(cst_rows$p.value)))
  expect_true(all(is.na(cst_rows$test_type)))
  # The healthy variable keeps its test.
  tt <- stats::t.test(ok ~ g, data = d)
  expect_equal(ok_rows$p.value[1], tt$p.value, tolerance = 1e-12)
  # The nonparametric route still works on the same data.
  wp <- suppressWarnings(stats::wilcox.test(cst ~ g, data = d))$p.value
  np <- table_continuous(
    d,
    select = cst,
    by = g,
    test = "nonparametric",
    output = "long"
  )
  expect_equal(np$p.value[1], wp, tolerance = 1e-12)
})

test_that("an undefined effect size degrades to NA with a classed warning", {
  # Companion to finding 27: a zero pooled SD makes Hedges' g
  # non-finite; the cell must be NA, never a printed Inf.
  d <- data.frame(
    cst = c(5, 5, 5, 5, 7, 7, 7, 7),
    g = factor(rep(c("m", "f"), each = 4))
  )
  warns <- character(0)
  out <- withCallingHandlers(
    table_continuous(
      d,
      select = cst,
      by = g,
      effect_size = "hedges_g",
      p_value = FALSE,
      output = "long"
    ),
    spicy_undefined_stat = function(w) {
      warns <<- c(warns, class(w)[1])
      invokeRestart("muffleWarning")
    }
  )
  expect_true(length(warns) >= 1L)
  expect_true(all(is.na(out$es_value)))
  expect_false(any(is.infinite(out$es_value)))
})

test_that("a NaN effect size is blanked with a classed warning", {
  # Audit phase 2 delta, R9: is.na(NaN) is TRUE, so the NA-first
  # non-finite guard used to let a 0/0 Hedges' g (equal group means
  # with zero pooled SD) reach the long output as NaN, unannounced.
  d <- data.frame(cst = rep(5, 8), g = factor(rep(c("m", "f"), each = 4)))
  msgs <- character(0)
  out <- withCallingHandlers(
    table_continuous(
      d,
      select = cst,
      by = g,
      effect_size = "hedges_g",
      p_value = FALSE,
      output = "long"
    ),
    spicy_undefined_stat = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("effect size", msgs)))
  expect_false(any(is.nan(out$es_value)))
  expect_true(all(is.na(out$es_value)))
  expect_false(any(is.nan(out$es_ci_lower)))
  expect_false(any(is.nan(out$es_ci_upper)))
})

test_that("table_continuous() rejects bit64::integer64 columns", {
  i64 <- structure(
    c(4.94e-324, 9.88e-324, 1.48e-323, 1.98e-323),
    class = "integer64"
  )
  d <- data.frame(y = c(1.5, 2.5, 3.5, 4.5), g = factor(c("a", "a", "b", "b")))
  d$score <- i64
  expect_error(
    table_continuous(d, select = score),
    "integer64",
    class = "spicy_invalid_data"
  )
  # The default select sweeps integer64 in (it passes is.numeric()).
  expect_error(table_continuous(d), class = "spicy_invalid_data")
  expect_error(
    table_continuous(d, select = y, by = score),
    class = "spicy_invalid_data"
  )
  # Excluding the integer64 column restores normal computation.
  res <- table_continuous(d, exclude = score, output = "data.frame")
  expect_s3_class(res, "data.frame")
})


# Phase 3 matrix – vignettes-news:align-auto-removed and
# critic:pkgrd-broom-columns-stabilising (lot T4)

test_that("align = 'auto' is removed from table_continuous", {
  expect_error(
    table_continuous(mtcars, select = "mpg", align = "auto"),
    class = "spicy_invalid_input"
  )
})

test_that("tidy/glance column sets are frozen (stabilising contract)", {
  skip_if_not_installed("broom")
  out <- table_continuous(mtcars, select = "mpg", by = "am")
  expect_identical(
    names(broom::tidy(out)),
    c(
      "outcome",
      "label",
      "group",
      "estimate",
      "std.error",
      "conf.low",
      "conf.high",
      "n",
      "min",
      "max",
      "sd"
    )
  )
  expect_identical(
    names(broom::glance(out)),
    c(
      "outcome",
      "label",
      "test_type",
      "statistic",
      "df",
      "df.residual",
      "p.value",
      "es_type",
      "es_value",
      "es_ci_lower",
      "es_ci_upper",
      "n_total"
    )
  )
})

# ---- the disclosure reaches every output route ---------------------------
# dev/notes_perdues_hors_console.md: rows dropped for a missing `by`
# value were signalled by a console warning and a note only print()
# could read -- invisible in any document rendered with
# `warning: false`.

test_that("table_continuous discloses dropped rows on the rich outputs", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  skip_if_not_installed("htmltools")
  d <- sochealth
  d$sex_na <- d$sex
  d$sex_na[1:40] <- NA

  x <- suppressWarnings(
    table_continuous(d, select = age, by = sex_na, output = "tinytable")
  )
  expect_identical(
    paste(unlist(x@notes), collapse = " "),
    "Rows with missing sex_na removed: 40."
  )
  expect_match(
    tinytable::save_tt(x, output = "html"),
    "Rows with missing sex_na removed: 40.",
    fixed = TRUE
  )

  g <- suppressWarnings(
    table_continuous(d, select = age, by = sex_na, output = "gt")
  )
  expect_s3_class(g, "spicy_gt")
  expect_identical(
    attr(g, "spicy_note"),
    "Rows with missing sex_na removed: 40."
  )

  f <- suppressWarnings(
    table_continuous(d, select = age, by = sex_na, output = "flextable")
  )
  expect_identical(nrow(f$footer$dataset), 1L)
  expect_match(
    as.character(flextable::htmltools_value(f)),
    "Rows with missing sex_na removed: 40.",
    fixed = TRUE
  )
})

test_that("table_continuous rich outputs carry no note when nothing is lost", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  df <- data.frame(x = 1:6, g = rep(c("A", "B"), each = 3))

  expect_length(
    table_continuous(df, select = "x", by = "g", output = "tinytable")@notes,
    0L
  )
  g <- table_continuous(df, select = "x", by = "g", output = "gt")
  expect_false(inherits(g, "spicy_gt"))
  f <- table_continuous(df, select = "x", by = "g", output = "flextable")
  expect_identical(nrow(f$footer$dataset), 0L)
})

test_that("an existing table note is joined, never overwritten", {
  # table_continuous_lm() already emits an SE-estimator note; the
  # missing-data disclosure must be added to it, not on top of it.
  skip_if_not_installed("tinytable")
  d <- sochealth
  d$sex_na <- d$sex
  d$sex_na[1:40] <- NA
  z <- suppressWarnings(
    table_continuous_lm(
      d,
      select = age,
      by = sex_na,
      vcov = "HC3",
      output = "tinytable"
    )
  )
  note <- paste(unlist(z@notes), collapse = " ")
  expect_match(note, "HC3", fixed = TRUE)
  expect_match(note, "Rows with missing sex_na removed: 40.", fixed = TRUE)
})

# ---- show_columns: statistic selection -----------------------------------

test_that("show_columns = NULL reproduces the historical display exactly", {
  # The default must not move: the printed table shows M / SD / Min /
  # Max and none of the new statistics, and its note stays empty.
  base_no_by <- capture.output(
    table_continuous(sleep, select = extra)
  )
  expect_true(any(grepl(" M ", base_no_by, fixed = TRUE)))
  expect_false(any(grepl("Med", base_no_by, fixed = TRUE)))
  expect_false(any(grepl("IQR", base_no_by, fixed = TRUE)))

  out <- table_continuous(sleep, select = extra, by = group, output = "long")
  expect_identical(
    attr(out, "show_columns"),
    c("m", "sd", "min", "max", "ci", "n")
  )
  expect_identical(
    attr(
      table_continuous(
        sleep,
        select = extra,
        ci = FALSE,
        show_n = FALSE,
        output = "long"
      ),
      "show_columns"
    ),
    c("m", "sd", "min", "max")
  )
  # No gloss is appended to a default table's note.
  expect_null(attr(out, "missing_note"))
})

test_that("median / quartile columns equal stats::median() and quantile() type 7", {
  set.seed(101)
  d <- data.frame(x = stats::rexp(41, 1 / 10))
  out <- table_continuous(
    d,
    select = x,
    show_columns = c("med", "q1", "q3", "iqr", "med_iqr"),
    output = "long"
  )
  q <- unname(stats::quantile(d$x, probs = c(0.25, 0.75)))
  expect_identical(out$median, stats::median(d$x))
  expect_identical(out$q1, q[1])
  expect_identical(out$q3, q[2])
  expect_identical(out$iqr, q[2] - q[1])
  expect_identical(out$iqr, stats::IQR(d$x))

  # The compact column shows the INTERVAL, `iqr` shows the width.
  printed <- capture.output(
    table_continuous(d, select = x, show_columns = c("med_iqr", "iqr"))
  )
  expect_true(any(grepl("Med [Q1, Q3]", printed, fixed = TRUE)))
  expect_true(any(grepl(
    sprintf("%.2f [%.2f, %.2f]", stats::median(d$x), q[1], q[2]),
    printed,
    fixed = TRUE
  )))
})

test_that("med_ci is the exact order-statistic interval, NA at small n", {
  # Hand-pinned against the sign-test inversion: the tightest
  # [x(k), x(n-k+1)] whose binomial coverage still reaches 95%.
  #   n = 6  -> k = 1  (coverage 0.96875)
  #   n = 10 -> k = 2  (coverage 0.97852)
  #   n = 25 -> k = 8  (coverage 0.95671)
  #   n = 5  -> no k reaches 95%, the interval is undefined
  pinned <- list(c(6, 1, 6), c(10, 2, 9), c(25, 8, 18))
  for (p in pinned) {
    x <- as.numeric(seq_len(p[1]))
    out <- table_continuous(
      data.frame(x = x),
      show_columns = c("med", "med_ci"),
      output = "long"
    )
    expect_identical(out$med_ci_lower, x[p[2]])
    expect_identical(out$med_ci_upper, x[p[3]])
  }
  small <- table_continuous(
    data.frame(x = c(1, 2, 3, 4, 5)),
    show_columns = c("med", "med_ci"),
    output = "long"
  )
  expect_true(is.na(small$med_ci_lower))
  expect_true(is.na(small$med_ci_upper))
  printed <- capture.output(
    table_continuous(
      data.frame(x = c(1, 2, 3, 4, 5)),
      show_columns = c("med", "med_ci")
    )
  )
  expect_true(any(grepl("--", printed, fixed = TRUE)))
  expect_true(any(grepl("too small for this level", printed, fixed = TRUE)))
})

test_that("med_ci matches the exact order-statistic oracle", {
  # Oracle values PINNED from DescTools::MedianCI(method = "exact")
  # (v0.99.60, captured 2026-08-14) on set.seed(202) draws -- pinned,
  # not called live, per the house rule (and so the package declares
  # no DescTools dependency; R CMD check flags undeclared '::' uses).
  set.seed(202)
  oracle <- list(
    `6` = c(-14.31, -1.46),
    `7` = c(-19.33, 11.80),
    `10` = c(-2.58, 16.70),
    `25` = c(-3.59, 5.97),
    `50` = c(-1.63, 2.37)
  )
  for (n in c(6, 7, 10, 25, 50)) {
    x <- round(stats::rnorm(n) * 10, 2)
    out <- table_continuous(
      data.frame(x = x),
      show_columns = c("med", "med_ci"),
      output = "long"
    )
    expect_equal(out$med_ci_lower, oracle[[as.character(n)]][1])
    expect_equal(out$med_ci_upper, oracle[[as.character(n)]][2])
  }
})

test_that("med_ci follows ci_level", {
  x <- as.numeric(1:25)
  # n = 25: k = 6 at 99% -> [6, 20]; k = 8 at 95% -> [8, 18].
  wide <- table_continuous(
    data.frame(x = x),
    show_columns = c("med", "med_ci"),
    ci_level = 0.99,
    output = "long"
  )
  narrow <- table_continuous(
    data.frame(x = x),
    show_columns = c("med", "med_ci"),
    ci_level = 0.95,
    output = "long"
  )
  expect_identical(c(wide$med_ci_lower, wide$med_ci_upper), c(6, 20))
  expect_identical(c(narrow$med_ci_lower, narrow$med_ci_upper), c(8, 18))
})

test_that("a named list gives each variable its own columns, blanks the rest", {
  set.seed(7)
  d <- data.frame(
    mvpa = round(stats::rexp(60, 1 / 300)),
    sitting = round(stats::rexp(60, 1 / 400)),
    age = round(stats::rnorm(60, 22, 3))
  )
  out <- table_continuous(
    d,
    select = c(mvpa, sitting, age),
    show_columns = list(
      mvpa = c("med_iqr", "n"),
      sitting = c("med_iqr", "n"),
      .default = c("m", "sd", "n")
    ),
    output = "long"
  )
  expect_identical(attr(out, "show_columns"), c("m", "sd", "med_iqr", "n"))
  display <- spicy:::build_display_df(
    out,
    digits = 2,
    decimal_mark = ".",
    ci_level = 0.95,
    tokens_union = attr(out, "show_columns"),
    tokens_by_var = attr(out, "show_columns_by_var")
  )
  expect_named(display, c("Variable", "M", "SD", "Med [Q1, Q3]", "n"))
  # Structural blank (""), never the undefined-statistic en-dash.
  expect_identical(display$M[1:2], c("", ""))
  expect_identical(display[[4]][3], "")
  expect_false(any(display$M[1:2] == "--"))
  expect_true(all(nzchar(display$n)))
})

test_that("a variable named in show_columns but absent from the table errors", {
  expect_error(
    table_continuous(sleep, select = extra, show_columns = list(nope = "m")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous(sleep, select = extra, show_columns = list("m")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous(
      sleep,
      select = extra,
      show_columns = list(extra = "m", extra = "sd")
    ),
    class = "spicy_invalid_input"
  )
})

test_that("unknown show_columns tokens error with the valid list", {
  expect_error(
    table_continuous(sleep, select = extra, show_columns = c("m", "mediane")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous(sleep, select = extra, show_columns = c("m", "m")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous(sleep, select = extra, show_columns = character(0)),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous(
      sleep,
      select = extra,
      show_columns = list(extra = c("m", "q4"))
    ),
    class = "spicy_invalid_input"
  )
})

test_that("a variable shown as a median is tested as one, per variable", {
  set.seed(9)
  d <- data.frame(
    skewed = round(stats::rexp(60, 1 / 300)),
    age = round(stats::rnorm(60, 22, 3)),
    g = rep(c("a", "b"), 30)
  )
  out <- table_continuous(
    d,
    select = c(skewed, age),
    by = g,
    show_columns = list(skewed = c("med_iqr", "n"), age = c("m", "sd", "n")),
    output = "long"
  )
  types <- out$test_type[!is.na(out$test_type)]
  names(types) <- out$variable[!is.na(out$test_type)]
  expect_identical(unname(types[["skewed"]]), "wilcoxon")
  expect_identical(unname(types[["age"]]), "welch_t")
  # Oracle: the p-values the stats:: tests give directly.
  expect_equal(
    out$p.value[out$variable == "skewed" & !is.na(out$p.value)],
    stats::wilcox.test(skewed ~ g, data = d)$p.value
  )
  expect_equal(
    out$p.value[out$variable == "age" & !is.na(out$p.value)],
    stats::t.test(age ~ g, data = d)$p.value
  )
  # The note discloses the test per variable when they differ.
  note <- attr(out, "missing_note")
  expect_match(note, "Wilcoxon rank-sum test (skewed)", fixed = TRUE)
  expect_match(note, "Welch t-test (age)", fixed = TRUE)

  # Three groups: Kruskal-Wallis on the median variable.
  d3 <- d
  d3$g3 <- rep(c("a", "b", "c"), 20)
  out3 <- table_continuous(
    d3,
    select = skewed,
    by = g3,
    show_columns = c("med_iqr", "n"),
    output = "long"
  )
  expect_identical(out3$test_type[1], "kruskal")
  expect_match(
    attr(out3, "missing_note"),
    "Group comparison: Kruskal-Wallis test.",
    fixed = TRUE
  )
})

test_that("the rank switch carries the rank effect size", {
  set.seed(19)
  d <- data.frame(
    skewed = round(stats::rexp(60, 1 / 300)),
    g = rep(c("a", "b"), 30)
  )
  out <- table_continuous(
    d,
    select = skewed,
    by = g,
    show_columns = c("med_iqr", "n"),
    effect_size = "auto",
    output = "long"
  )
  expect_identical(out$es_type[1], "r_rb")
})

test_that("an explicit test stays sovereign but raises a caveat", {
  set.seed(21)
  d <- data.frame(
    skewed = round(stats::rexp(60, 1 / 300)),
    g = rep(c("a", "b"), 30)
  )
  expect_warning(
    out <- table_continuous(
      d,
      select = skewed,
      by = g,
      show_columns = c("med_iqr", "n"),
      test = "welch",
      output = "long"
    ),
    class = "spicy_caveat"
  )
  expect_identical(out$test_type[1], "welch_t")
  # Sovereign means uniform: the note glosses the columns but never
  # claims a rank test the table did not run.
  expect_false(grepl(
    "Group comparison",
    attr(out, "missing_note"),
    fixed = TRUE
  ))
})

test_that("ci without m is dropped and points at med_ci; med_ci mirrors it", {
  expect_warning(
    out <- table_continuous(
      sleep,
      select = extra,
      show_columns = c("med", "ci", "n"),
      output = "long"
    ),
    class = "spicy_ignored_arg"
  )
  expect_false("ci" %in% attr(out, "show_columns"))
  expect_warning(
    out2 <- table_continuous(
      sleep,
      select = extra,
      show_columns = c("m", "med_ci"),
      output = "long"
    ),
    class = "spicy_ignored_arg"
  )
  expect_false("med_ci" %in% attr(out2, "show_columns"))
  # med_iqr also displays the median, so it satisfies med_ci.
  out3 <- table_continuous(
    sleep,
    select = extra,
    show_columns = c("med_iqr", "med_ci"),
    output = "long"
  )
  expect_true("med_ci" %in% attr(out3, "show_columns"))
})

test_that("show_columns overrules a contradictory show_n / ci", {
  expect_warning(
    out <- table_continuous(
      sleep,
      select = extra,
      show_columns = c("m", "sd"),
      show_n = TRUE,
      output = "long"
    ),
    class = "spicy_ignored_arg"
  )
  expect_false("n" %in% attr(out, "show_columns"))
  expect_warning(
    table_continuous(
      sleep,
      select = extra,
      show_columns = c("m", "sd"),
      ci = TRUE,
      output = "long"
    ),
    class = "spicy_ignored_arg"
  )
  # A consistent legacy toggle stays silent.
  expect_silent(
    table_continuous(
      sleep,
      select = extra,
      show_columns = c("m", "sd"),
      show_n = FALSE,
      ci = FALSE,
      output = "long"
    )
  )
})

test_that("show_columns reaches every output engine with the same columns", {
  Sys.setenv(CLIPR_ALLOW = "TRUE")
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(text, ...) invisible(text),
    .package = "clipr"
  )
  set.seed(33)
  d <- data.frame(
    skewed = round(stats::rexp(60, 1 / 300)),
    age = round(stats::rnorm(60, 22, 3)),
    g = rep(c("a", "b"), 30)
  )
  spec <- list(
    skewed = c("med", "med_iqr", "med_ci", "n"),
    age = c("m", "sd", "ci", "n")
  )
  # data.frame / long: every statistic present whatever is displayed.
  lg <- table_continuous(
    d,
    select = c(skewed, age),
    by = g,
    show_columns = spec,
    output = "long"
  )
  expect_true(all(
    c("median", "q1", "q3", "iqr", "med_ci_lower", "med_ci_upper") %in%
      names(lg)
  ))
  expect_identical(
    attr(lg, "show_columns"),
    c("m", "sd", "med", "med_iqr", "ci", "med_ci", "n")
  )

  display <- spicy:::build_display_df(
    lg,
    digits = 2,
    decimal_mark = ".",
    ci_level = 0.95,
    show_p = TRUE,
    tokens_union = attr(lg, "show_columns"),
    tokens_by_var = attr(lg, "show_columns_by_var")
  )
  expect_named(
    display,
    c(
      "Variable",
      "Group",
      "M",
      "SD",
      "Med",
      "Med [Q1, Q3]",
      "95% CI LL",
      "95% CI UL",
      "Med 95% CI LL",
      "Med 95% CI UL",
      "n",
      "p"
    )
  )

  for (eng in c("default", "data.frame", "clipboard")) {
    expect_no_error(
      table_continuous(
        d,
        select = c(skewed, age),
        by = g,
        show_columns = spec,
        output = eng
      )
    )
  }

  skip_if_not_installed("tinytable")
  tt <- table_continuous(
    d,
    select = c(skewed, age),
    by = g,
    show_columns = spec,
    output = "tinytable"
  )
  expect_s4_class(tt, "tinytable")

  skip_if_not_installed("flextable")
  ft <- table_continuous(
    d,
    select = c(skewed, age),
    by = g,
    show_columns = spec,
    output = "flextable"
  )
  # Both CI pairs keep their own spanner; the headers do not collide.
  hdr <- unlist(ft$header$dataset, use.names = FALSE)
  expect_true(any(grepl("Med 95% CI", hdr, fixed = TRUE)))
  expect_true(any(hdr == "95% CI"))
})

test_that("the note glosses only the columns actually displayed", {
  d <- data.frame(x = as.numeric(1:40))
  n_iqr <- attr(
    table_continuous(d, show_columns = c("med", "iqr"), output = "long"),
    "missing_note"
  )
  expect_match(n_iqr, "IQR = interquartile range (Q3 - Q1).", fixed = TRUE)
  expect_false(grepl("order-statistic", n_iqr, fixed = TRUE))

  n_full <- attr(
    table_continuous(
      d,
      show_columns = c("med_iqr", "med_ci"),
      output = "long"
    ),
    "missing_note"
  )
  expect_match(n_full, "Med [Q1, Q3] = median", fixed = TRUE)
  expect_match(n_full, "coverage at least 95%", fixed = TRUE)
  expect_false(grepl("IQR = interquartile", n_full, fixed = TRUE))

  # A plain M / SD selection adds nothing to the note.
  expect_null(attr(
    table_continuous(d, show_columns = c("m", "sd"), output = "long"),
    "missing_note"
  ))
})


test_that("the by table titles itself 'Descriptive statistics by <label>'", {
  # Decision 4 (2026-08-13): the grouping variable is stated in the
  # title, label resolved, like the other by tables of the family.
  out <- capture.output(print(
    table_continuous(sochealth, select = bmi, by = sex)
  ))
  expect_true(any(grepl("^Descriptive statistics by Sex$", trimws(out))))
  out1 <- capture.output(print(table_continuous(sochealth, select = bmi)))
  expect_true(any(grepl("^Descriptive statistics$", trimws(out1))))
})
