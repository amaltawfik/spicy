# Continuous summary table

`table_continuous()` computes descriptive statistics for continuous
variables, with optional grouping by a categorical variable, optional
group-comparison tests, and multiple output formats.

## Usage

``` r
table_continuous(
  data,
  select = dplyr::everything(),
  by = NULL,
  exclude = NULL,
  regex = FALSE,
  test = c("welch", "student", "nonparametric"),
  p_value = FALSE,
  statistic = FALSE,
  effect_size = FALSE,
  effect_size_ci = FALSE,
  labels = NULL,
  ci_level = 0.95,
  digits = 2,
  decimal_mark = ".",
  output = c("default", "tinytable", "gt", "flextable", "excel", "clipboard", "word"),
  styled = TRUE,
  excel_path = NULL,
  excel_sheet = "Descriptives",
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  A `data.frame`.

- select:

  Columns to include. If `regex = FALSE`, use tidyselect syntax
  (default:
  [`dplyr::everything()`](https://tidyselect.r-lib.org/reference/everything.html)).
  If `regex = TRUE`, provide a regular expression pattern (character
  string).

- by:

  An optional unquoted column name to group the descriptive statistics
  by. The column does not need to be numeric.

- exclude:

  Columns to exclude. Supports tidyselect syntax and character vectors
  of column names.

- regex:

  Logical. If `FALSE` (the default), uses tidyselect helpers. If `TRUE`,
  the `select` argument is treated as a regular expression.

- test:

  Character. Statistical test to use when comparing groups. One of
  `"welch"` (default), `"student"`, or `"nonparametric"`.

  - `"welch"`: Welch *t*-test (2 groups) or Welch one-way ANOVA (3+
    groups). Does not assume equal variances.

  - `"student"`: Student *t*-test (2 groups) or classic one-way ANOVA
    (3+ groups). Assumes equal variances.

  - `"nonparametric"`: Wilcoxon rank-sum / Mann–Whitney *U* (2 groups)
    or Kruskal–Wallis *H* (3+ groups).

  Used when `by` is supplied together with `p_value = TRUE`,
  `statistic = TRUE`, or `effect_size = TRUE`. Ignored otherwise.

- p_value:

  Logical. If `TRUE` and `by` is used, adds a *p*-value column from the
  test specified by `test`. Defaults to `FALSE`. Ignored when `by` is
  not used.

- statistic:

  Logical. If `TRUE` and `by` is used, the test statistic is shown in an
  additional column (e.g., `t(df) = ...`, `F(df1, df2) = ...`,
  `W = ...`, or `H(df) = ...`). Both `p_value` and `statistic` are
  independent; either or both can be enabled. Defaults to `FALSE`.
  Ignored when `by` is not used.

- effect_size:

  Logical. If `TRUE` and `by` is used, adds an effect-size column
  ("ES"). The measure is chosen automatically:

  - Hedges' *g* (bias-corrected) â€” 2 groups, parametric (CI via Hedges
    & Olkin approximation).

  - Eta-squared (\\\eta^2\\) â€” 3+ groups, parametric (CI via
    noncentral *F* distribution).

  - Rank-biserial *r* (`r_rb`) â€” 2 groups, nonparametric (CI via
    Fisher *z*-transform).

  - Epsilon-squared (\\\varepsilon^2\\) â€” 3+ groups, nonparametric (CI
    via percentile bootstrap, 2 000 replicates).

  Defaults to `FALSE`. Ignored when `by` is not used.

- effect_size_ci:

  Logical. If `TRUE`, appends the confidence interval of the effect size
  in brackets (e.g., `g = 0.45 [0.22, 0.68]`). Implies
  `effect_size = TRUE`. Defaults to `FALSE`.

- labels:

  An optional named character vector of variable labels. Names must
  match column names in `data`. When `NULL` (the default), labels are
  auto-detected from variable attributes (e.g., haven labels); if none
  are found, the column name is used.

- ci_level:

  Confidence level for the mean confidence interval (default: `0.95`).
  Must be between 0 and 1 exclusive.

- digits:

  Number of decimal places for numeric output (default: `2`).

- decimal_mark:

  Character used as decimal separator. Either `"."` (default) or `","`.

- output:

  Output format. One of:

  - `"default"` (a printed ASCII table)

  - `"tinytable"` (requires `tinytable`)

  - `"gt"` (requires `gt`)

  - `"flextable"` (requires `flextable`)

  - `"excel"` (requires `openxlsx`)

  - `"clipboard"` (requires `clipr`)

  - `"word"` (requires `flextable` and `officer`)

- styled:

  Logical. If `TRUE` (the default), returns an S3 object with a custom
  print method. If `FALSE`, returns a plain `data.frame`.

- excel_path:

  File path for `output = "excel"`.

- excel_sheet:

  Sheet name for `output = "excel"` (default: `"Descriptives"`).

- clipboard_delim:

  Delimiter for `output = "clipboard"` (default: `"\t"`).

- word_path:

  File path for `output = "word"`.

- verbose:

  Logical. If `TRUE`, prints messages about excluded non-numeric columns
  (default: `FALSE`).

## Value

When `styled = TRUE` (default) and `output = "default"`, prints a styled
ASCII table and returns the underlying `data.frame` invisibly (with S3
class `"spicy_continuous_table"`). When `styled = FALSE`, returns a
plain `data.frame` with columns: `variable`, `label`, `group` (if `by`
is used), `mean`, `sd`, `min`, `max`, `ci_lower`, `ci_upper`, `n`. When
`by` is used together with `p_value = TRUE`, `statistic = TRUE`, or
`effect_size = TRUE`, columns `test_type`, `statistic`, `df1`, `df2`,
and `p.value` are appended (populated on the first row of each variable
block only). `test_type` records the test that was run (e.g.,
`"welch_t"`, `"welch_anova"`, `"student_t"`, `"anova"`, `"wilcoxon"`,
`"kruskal"`). When `effect_size = TRUE` and `by` is used, columns
`es_type`, `es_value`, `es_ci_lower`, and `es_ci_upper` are appended
(populated on the first row of each variable block only). `es_type`
records the measure used (`"hedges_g"`, `"eta_sq"`, `"r_rb"`, or
`"epsilon_sq"`).

For other `output` values: `"tinytable"`, `"gt"`, and `"flextable"`
return their respective table objects. `"excel"` and `"word"` write to
disk and return the file path invisibly. `"clipboard"` copies the table
and returns the display `data.frame` invisibly.

## Examples

``` r
# Basic usage with all numeric columns
table_continuous(iris, styled = FALSE)
#>       variable        label     mean        sd min max ci_lower ci_upper   n
#> 1 Sepal.Length Sepal.Length 5.843333 0.8280661 4.3 7.9 5.709732 5.976934 150
#> 2  Sepal.Width  Sepal.Width 3.057333 0.4358663 2.0 4.4 2.987010 3.127656 150
#> 3 Petal.Length Petal.Length 3.758000 1.7652982 1.0 6.9 3.473185 4.042815 150
#> 4  Petal.Width  Petal.Width 1.199333 0.7622377 0.1 2.5 1.076353 1.322313 150

# Select specific columns with tidyselect
table_continuous(iris, select = c(Sepal.Length, Petal.Width), styled = FALSE)
#>       variable        label     mean        sd min max ci_lower ci_upper   n
#> 1 Sepal.Length Sepal.Length 5.843333 0.8280661 4.3 7.9 5.709732 5.976934 150
#> 2  Petal.Width  Petal.Width 1.199333 0.7622377 0.1 2.5 1.076353 1.322313 150

# Grouped descriptives
table_continuous(iris, select = c(Sepal.Length, Sepal.Width),
           by = Species, styled = FALSE)
#>       variable        label      group  mean        sd min max ci_lower
#> 1 Sepal.Length Sepal.Length     setosa 5.006 0.3524897 4.3 5.8 4.905824
#> 2 Sepal.Length Sepal.Length versicolor 5.936 0.5161711 4.9 7.0 5.789306
#> 3 Sepal.Length Sepal.Length  virginica 6.588 0.6358796 4.9 7.9 6.407285
#> 4  Sepal.Width  Sepal.Width     setosa 3.428 0.3790644 2.3 4.4 3.320271
#> 5  Sepal.Width  Sepal.Width versicolor 2.770 0.3137983 2.0 3.4 2.680820
#> 6  Sepal.Width  Sepal.Width  virginica 2.974 0.3224966 2.2 3.8 2.882347
#>   ci_upper  n
#> 1 5.106176 50
#> 2 6.082694 50
#> 3 6.768715 50
#> 4 3.535729 50
#> 5 2.859180 50
#> 6 3.065653 50

# Grouped descriptives with p-value
table_continuous(iris, select = c(Sepal.Length, Sepal.Width),
           by = Species, p_value = TRUE, styled = FALSE)
#>       variable        label      group  mean        sd min max ci_lower
#> 1 Sepal.Length Sepal.Length     setosa 5.006 0.3524897 4.3 5.8 4.905824
#> 2 Sepal.Length Sepal.Length versicolor 5.936 0.5161711 4.9 7.0 5.789306
#> 3 Sepal.Length Sepal.Length  virginica 6.588 0.6358796 4.9 7.9 6.407285
#> 4  Sepal.Width  Sepal.Width     setosa 3.428 0.3790644 2.3 4.4 3.320271
#> 5  Sepal.Width  Sepal.Width versicolor 2.770 0.3137983 2.0 3.4 2.680820
#> 6  Sepal.Width  Sepal.Width  virginica 2.974 0.3224966 2.2 3.8 2.882347
#>   ci_upper  n   test_type statistic df1      df2      p.value
#> 1 5.106176 50 welch_anova 138.90829   2 92.21115 1.505059e-28
#> 2 6.082694 50        <NA>        NA  NA       NA           NA
#> 3 6.768715 50        <NA>        NA  NA       NA           NA
#> 4 3.535729 50 welch_anova  45.01204   2 97.40159 1.432735e-14
#> 5 2.859180 50        <NA>        NA  NA       NA           NA
#> 6 3.065653 50        <NA>        NA  NA       NA           NA

# Grouped descriptives with test statistic only
table_continuous(iris, select = c(Sepal.Length, Sepal.Width),
           by = Species, statistic = TRUE, styled = FALSE)
#>       variable        label      group  mean        sd min max ci_lower
#> 1 Sepal.Length Sepal.Length     setosa 5.006 0.3524897 4.3 5.8 4.905824
#> 2 Sepal.Length Sepal.Length versicolor 5.936 0.5161711 4.9 7.0 5.789306
#> 3 Sepal.Length Sepal.Length  virginica 6.588 0.6358796 4.9 7.9 6.407285
#> 4  Sepal.Width  Sepal.Width     setosa 3.428 0.3790644 2.3 4.4 3.320271
#> 5  Sepal.Width  Sepal.Width versicolor 2.770 0.3137983 2.0 3.4 2.680820
#> 6  Sepal.Width  Sepal.Width  virginica 2.974 0.3224966 2.2 3.8 2.882347
#>   ci_upper  n   test_type statistic df1      df2      p.value
#> 1 5.106176 50 welch_anova 138.90829   2 92.21115 1.505059e-28
#> 2 6.082694 50        <NA>        NA  NA       NA           NA
#> 3 6.768715 50        <NA>        NA  NA       NA           NA
#> 4 3.535729 50 welch_anova  45.01204   2 97.40159 1.432735e-14
#> 5 2.859180 50        <NA>        NA  NA       NA           NA
#> 6 3.065653 50        <NA>        NA  NA       NA           NA

# Grouped descriptives with both p-value and test statistic
table_continuous(iris, select = c(Sepal.Length, Sepal.Width),
           by = Species, p_value = TRUE, statistic = TRUE,
           styled = FALSE)
#>       variable        label      group  mean        sd min max ci_lower
#> 1 Sepal.Length Sepal.Length     setosa 5.006 0.3524897 4.3 5.8 4.905824
#> 2 Sepal.Length Sepal.Length versicolor 5.936 0.5161711 4.9 7.0 5.789306
#> 3 Sepal.Length Sepal.Length  virginica 6.588 0.6358796 4.9 7.9 6.407285
#> 4  Sepal.Width  Sepal.Width     setosa 3.428 0.3790644 2.3 4.4 3.320271
#> 5  Sepal.Width  Sepal.Width versicolor 2.770 0.3137983 2.0 3.4 2.680820
#> 6  Sepal.Width  Sepal.Width  virginica 2.974 0.3224966 2.2 3.8 2.882347
#>   ci_upper  n   test_type statistic df1      df2      p.value
#> 1 5.106176 50 welch_anova 138.90829   2 92.21115 1.505059e-28
#> 2 6.082694 50        <NA>        NA  NA       NA           NA
#> 3 6.768715 50        <NA>        NA  NA       NA           NA
#> 4 3.535729 50 welch_anova  45.01204   2 97.40159 1.432735e-14
#> 5 2.859180 50        <NA>        NA  NA       NA           NA
#> 6 3.065653 50        <NA>        NA  NA       NA           NA

# Student t-test / classic ANOVA (assumes equal variances)
table_continuous(iris, select = Sepal.Length, by = Species,
           test = "student", p_value = TRUE, styled = FALSE)
#>       variable        label      group  mean        sd min max ci_lower
#> 1 Sepal.Length Sepal.Length     setosa 5.006 0.3524897 4.3 5.8 4.905824
#> 2 Sepal.Length Sepal.Length versicolor 5.936 0.5161711 4.9 7.0 5.789306
#> 3 Sepal.Length Sepal.Length  virginica 6.588 0.6358796 4.9 7.9 6.407285
#>   ci_upper  n test_type statistic df1 df2      p.value
#> 1 5.106176 50     anova  119.2645   2 147 1.669669e-31
#> 2 6.082694 50      <NA>        NA  NA  NA           NA
#> 3 6.768715 50      <NA>        NA  NA  NA           NA

# Nonparametric test (Kruskal-Wallis for 3+ groups)
table_continuous(iris, select = Sepal.Length, by = Species,
           test = "nonparametric", p_value = TRUE,
           statistic = TRUE, styled = FALSE)
#>       variable        label      group  mean        sd min max ci_lower
#> 1 Sepal.Length Sepal.Length     setosa 5.006 0.3524897 4.3 5.8 4.905824
#> 2 Sepal.Length Sepal.Length versicolor 5.936 0.5161711 4.9 7.0 5.789306
#> 3 Sepal.Length Sepal.Length  virginica 6.588 0.6358796 4.9 7.9 6.407285
#>   ci_upper  n test_type statistic df1 df2      p.value
#> 1 5.106176 50   kruskal  96.93744   2  NA 8.918734e-22
#> 2 6.082694 50      <NA>        NA  NA  NA           NA
#> 3 6.768715 50      <NA>        NA  NA  NA           NA

# Effect size (eta-squared for 3 groups)
table_continuous(iris, select = Sepal.Length, by = Species,
           effect_size = TRUE, styled = FALSE)
#>       variable        label      group  mean        sd min max ci_lower
#> 1 Sepal.Length Sepal.Length     setosa 5.006 0.3524897 4.3 5.8 4.905824
#> 2 Sepal.Length Sepal.Length versicolor 5.936 0.5161711 4.9 7.0 5.789306
#> 3 Sepal.Length Sepal.Length  virginica 6.588 0.6358796 4.9 7.9 6.407285
#>   ci_upper  n   test_type statistic df1      df2      p.value es_type  es_value
#> 1 5.106176 50 welch_anova  138.9083   2 92.21115 1.505059e-28  eta_sq 0.6187057
#> 2 6.082694 50        <NA>        NA  NA       NA           NA    <NA>        NA
#> 3 6.768715 50        <NA>        NA  NA       NA           NA    <NA>        NA
#>   es_ci_lower es_ci_upper
#> 1   0.5203092   0.6844361
#> 2          NA          NA
#> 3          NA          NA

# Effect size with confidence interval
table_continuous(iris, select = Sepal.Length, by = Species,
           p_value = TRUE, effect_size_ci = TRUE,
           styled = FALSE)
#> Warning: `effect_size_ci` implies `effect_size = TRUE`.
#>       variable        label      group  mean        sd min max ci_lower
#> 1 Sepal.Length Sepal.Length     setosa 5.006 0.3524897 4.3 5.8 4.905824
#> 2 Sepal.Length Sepal.Length versicolor 5.936 0.5161711 4.9 7.0 5.789306
#> 3 Sepal.Length Sepal.Length  virginica 6.588 0.6358796 4.9 7.9 6.407285
#>   ci_upper  n   test_type statistic df1      df2      p.value es_type  es_value
#> 1 5.106176 50 welch_anova  138.9083   2 92.21115 1.505059e-28  eta_sq 0.6187057
#> 2 6.082694 50        <NA>        NA  NA       NA           NA    <NA>        NA
#> 3 6.768715 50        <NA>        NA  NA       NA           NA    <NA>        NA
#>   es_ci_lower es_ci_upper
#> 1   0.5203092   0.6844361
#> 2          NA          NA
#> 3          NA          NA

# Nonparametric effect size (epsilon-squared with bootstrap CI)
table_continuous(iris, select = Sepal.Length, by = Species,
           test = "nonparametric", effect_size_ci = TRUE,
           styled = FALSE)
#> Warning: `effect_size_ci` implies `effect_size = TRUE`.
#>       variable        label      group  mean        sd min max ci_lower
#> 1 Sepal.Length Sepal.Length     setosa 5.006 0.3524897 4.3 5.8 4.905824
#> 2 Sepal.Length Sepal.Length versicolor 5.936 0.5161711 4.9 7.0 5.789306
#> 3 Sepal.Length Sepal.Length  virginica 6.588 0.6358796 4.9 7.9 6.407285
#>   ci_upper  n test_type statistic df1 df2      p.value    es_type  es_value
#> 1 5.106176 50   kruskal  96.93744   2  NA 8.918734e-22 epsilon_sq 0.6458329
#> 2 6.082694 50      <NA>        NA  NA  NA           NA       <NA>        NA
#> 3 6.768715 50      <NA>        NA  NA  NA           NA       <NA>        NA
#>   es_ci_lower es_ci_upper
#> 1   0.5462008   0.7279627
#> 2          NA          NA
#> 3          NA          NA

# Hedges' g for 2 groups
table_continuous(iris[iris$Species != "virginica", ],
           select = Sepal.Length, by = Species,
           effect_size_ci = TRUE, styled = FALSE)
#> Warning: `effect_size_ci` implies `effect_size = TRUE`.
#>       variable        label      group  mean        sd min max ci_lower
#> 1 Sepal.Length Sepal.Length     setosa 5.006 0.3524897 4.3 5.8 4.905824
#> 2 Sepal.Length Sepal.Length versicolor 5.936 0.5161711 4.9 7.0 5.789306
#> 3 Sepal.Length Sepal.Length  virginica    NA        NA  NA  NA       NA
#>   ci_upper  n test_type statistic    df1 df2      p.value  es_type  es_value
#> 1 5.106176 50   welch_t -10.52099 86.538  NA 3.746743e-17 hedges_g -2.088053
#> 2 6.082694 50      <NA>        NA     NA  NA           NA     <NA>        NA
#> 3       NA  0      <NA>        NA     NA  NA           NA     <NA>        NA
#>   es_ci_lower es_ci_upper
#> 1   -2.575291   -1.600814
#> 2          NA          NA
#> 3          NA          NA

# Regex column selection
table_continuous(iris, select = "^Sepal", regex = TRUE, styled = FALSE)
#>       variable        label     mean        sd min max ci_lower ci_upper   n
#> 1 Sepal.Length Sepal.Length 5.843333 0.8280661 4.3 7.9 5.709732 5.976934 150
#> 2  Sepal.Width  Sepal.Width 3.057333 0.4358663 2.0 4.4 2.987010 3.127656 150

# Custom labels
table_continuous(iris,
           select = c(Sepal.Length, Petal.Length),
           labels = c(Sepal.Length = "Sepal length (cm)",
                      Petal.Length = "Petal length (cm)"),
           styled = FALSE)
#>       variable             label     mean        sd min max ci_lower ci_upper
#> 1 Sepal.Length Sepal length (cm) 5.843333 0.8280661 4.3 7.9 5.709732 5.976934
#> 2 Petal.Length Petal length (cm) 3.758000 1.7652982 1.0 6.9 3.473185 4.042815
#>     n
#> 1 150
#> 2 150

# \donttest{
# ASCII table (default)
table_continuous(iris, select = starts_with("Sepal"))
#> Descriptive statistics
#> 
#>  Variable     │  M     SD   Min   Max   95% CI LL  95% CI UL    n 
#> ──────────────┼───────────────────────────────────────────────────
#>  Sepal.Length │ 5.84  0.83  4.30  7.90    5.71       5.98     150 
#>  Sepal.Width  │ 3.06  0.44  2.00  4.40    2.99       3.13     150 

# Grouped ASCII table
table_continuous(iris, select = starts_with("Sepal"), by = Species)
#> Descriptive statistics
#> 
#>  Variable     │ Group        M     SD   Min   Max   95% CI LL  95% CI UL   n 
#> ──────────────┼──────────────────────────────────────────────────────────────
#>  Sepal.Length │ setosa      5.01  0.35  4.30  5.80    4.91       5.11     50 
#>               │ versicolor  5.94  0.52  4.90  7.00    5.79       6.08     50 
#>               │ virginica   6.59  0.64  4.90  7.90    6.41       6.77     50 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Sepal.Width  │ setosa      3.43  0.38  2.30  4.40    3.32       3.54     50 
#>               │ versicolor  2.77  0.31  2.00  3.40    2.68       2.86     50 
#>               │ virginica   2.97  0.32  2.20  3.80    2.88       3.07     50 

# tinytable output
if (requireNamespace("tinytable", quietly = TRUE)) {
  table_continuous(iris, output = "tinytable")
  table_continuous(iris, select = starts_with("Sepal"),
             by = Species, output = "tinytable")
}
#> +--------------+------------+------+------+------+------+------+------+----+
#> | Variable     | Group      | M    | SD   | Min  | Max  | 95% CI      | n  |
#> +--------------+------------+------+------+------+------+------+------+----+
#> |              |            |      |      |      |      | LL   | UL   |    |
#> +==============+============+======+======+======+======+======+======+====+
#> | Sepal.Length | setosa     | 5.01 | 0.35 | 4.30 | 5.80 | 4.91 | 5.11 | 50 |
#> +--------------+------------+------+------+------+------+------+------+----+
#> |              | versicolor | 5.94 | 0.52 | 4.90 | 7.00 | 5.79 | 6.08 | 50 |
#> +--------------+------------+------+------+------+------+------+------+----+
#> |              | virginica  | 6.59 | 0.64 | 4.90 | 7.90 | 6.41 | 6.77 | 50 |
#> +--------------+------------+------+------+------+------+------+------+----+
#> | Sepal.Width  | setosa     | 3.43 | 0.38 | 2.30 | 4.40 | 3.32 | 3.54 | 50 |
#> +--------------+------------+------+------+------+------+------+------+----+
#> |              | versicolor | 2.77 | 0.31 | 2.00 | 3.40 | 2.68 | 2.86 | 50 |
#> +--------------+------------+------+------+------+------+------+------+----+
#> |              | virginica  | 2.97 | 0.32 | 2.20 | 3.80 | 2.88 | 3.07 | 50 |
#> +--------------+------------+------+------+------+------+------+------+----+ 

# gt output
if (requireNamespace("gt", quietly = TRUE)) {
  table_continuous(iris, output = "gt")
  table_continuous(iris, select = starts_with("Sepal"),
             by = Species, output = "gt")
}


  



        Variable
      
```

Group

M

SD

Min

Max

95% CI

n

LL

UL

Sepal.Length

setosa

5.01

0.35

4.30

5.80

4.91

5.11

50

versicolor

5.94

0.52

4.90

7.00

5.79

6.08

50

virginica

6.59

0.64

4.90

7.90

6.41

6.77

50

Sepal.Width

setosa

3.43

0.38

2.30

4.40

3.32

3.54

50

versicolor

2.77

0.31

2.00

3.40

2.68

2.86

50

virginica

2.97

0.32

2.20

3.80

2.88

3.07

50

\# flextable output if
([requireNamespace](https://rdrr.io/r/base/ns-load.html)("flextable",
quietly = TRUE)) { table_continuous(iris, output = "flextable")
table_continuous(iris, by = Species, output = "flextable") }

| Variable     | Group      | M    | SD   | Min  | Max  | 95% CI |      | n   |
|--------------|------------|------|------|------|------|--------|------|-----|
|              |            |      |      |      |      | LL     | UL   |     |
| Sepal.Length | setosa     | 5.01 | 0.35 | 4.30 | 5.80 | 4.91   | 5.11 | 50  |
|              | versicolor | 5.94 | 0.52 | 4.90 | 7.00 | 5.79   | 6.08 | 50  |
|              | virginica  | 6.59 | 0.64 | 4.90 | 7.90 | 6.41   | 6.77 | 50  |
| Sepal.Width  | setosa     | 3.43 | 0.38 | 2.30 | 4.40 | 3.32   | 3.54 | 50  |
|              | versicolor | 2.77 | 0.31 | 2.00 | 3.40 | 2.68   | 2.86 | 50  |
|              | virginica  | 2.97 | 0.32 | 2.20 | 3.80 | 2.88   | 3.07 | 50  |
| Petal.Length | setosa     | 1.46 | 0.17 | 1.00 | 1.90 | 1.41   | 1.51 | 50  |
|              | versicolor | 4.26 | 0.47 | 3.00 | 5.10 | 4.13   | 4.39 | 50  |
|              | virginica  | 5.55 | 0.55 | 4.50 | 6.90 | 5.40   | 5.71 | 50  |
| Petal.Width  | setosa     | 0.25 | 0.11 | 0.10 | 0.60 | 0.22   | 0.28 | 50  |
|              | versicolor | 1.33 | 0.20 | 1.00 | 1.80 | 1.27   | 1.38 | 50  |
|              | virginica  | 2.03 | 0.27 | 1.40 | 2.50 | 1.95   | 2.10 | 50  |

\# Word output if
([requireNamespace](https://rdrr.io/r/base/ns-load.html)("flextable",
quietly = TRUE) &&
[requireNamespace](https://rdrr.io/r/base/ns-load.html)("officer",
quietly = TRUE)) { table_continuous(iris, select =
[starts_with](https://tidyselect.r-lib.org/reference/starts_with.html)("Sepal"),
by = Species, output = "word", word_path =
[tempfile](https://rdrr.io/r/base/tempfile.html)(fileext = ".docx")) }
\# Excel output if
([requireNamespace](https://rdrr.io/r/base/ns-load.html)("openxlsx",
quietly = TRUE)) { table_continuous(iris, select =
[starts_with](https://tidyselect.r-lib.org/reference/starts_with.html)("Sepal"),
by = Species, output = "excel", excel_path =
[tempfile](https://rdrr.io/r/base/tempfile.html)(fileext = ".xlsx")) }
\# }
