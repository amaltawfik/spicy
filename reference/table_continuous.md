# Continuous summary table

Computes descriptive statistics (mean, SD, min, max, confidence interval
of the mean, *n*) for one or many continuous variables selected with
tidyselect syntax.

With `by`, produces grouped summaries with optional group-comparison
tests (`test`), *p*-values (`p_value`), test statistics (`statistic`),
and effect sizes (`effect_size` / `effect_size_ci`). Without `by`,
produces one-way descriptive summaries.

Multiple output formats are available via `output`: a printed ASCII
table (`"default"`), a plain numeric `data.frame` (`"data.frame"`), or
publication-ready tables (`"tinytable"`, `"gt"`, `"flextable"`,
`"excel"`, `"clipboard"`, `"word"`).

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
  effect_size_digits = 2,
  decimal_mark = ".",
  output = c("default", "data.frame", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
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

  Columns to include. If `regex = FALSE`, use tidyselect syntax or a
  character vector of column names (default:
  [`dplyr::everything()`](https://tidyselect.r-lib.org/reference/everything.html)).
  If `regex = TRUE`, provide a regular expression pattern (character
  string).

- by:

  Optional grouping column. Accepts an unquoted column name or a single
  character column name. The column does not need to be numeric.

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

  - Hedges' *g* (bias-corrected) - 2 groups, parametric (CI via Hedges &
    Olkin approximation).

  - Eta-squared (\\\eta^2\\) - 3+ groups, parametric (CI via noncentral
    *F* distribution).

  - Rank-biserial *r* (`r_rb`) - 2 groups, nonparametric (CI via Fisher
    *z*-transform).

  - Epsilon-squared (\\\varepsilon^2\\) - 3+ groups, nonparametric (CI
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

  Number of decimal places for descriptive values and test statistics
  (default: `2`).

- effect_size_digits:

  Number of decimal places for effect-size values in formatted displays
  (default: `2`).

- decimal_mark:

  Character used as decimal separator. Either `"."` (default) or `","`.

- output:

  Output format. One of:

  - `"default"` (a printed ASCII table, returned invisibly)

  - `"data.frame"` (a plain numeric `data.frame`)

  - `"tinytable"` (requires `tinytable`)

  - `"gt"` (requires `gt`)

  - `"flextable"` (requires `flextable`)

  - `"excel"` (requires `openxlsx2`)

  - `"clipboard"` (requires `clipr`)

  - `"word"` (requires `flextable` and `officer`)

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

Depends on `output`:

- `"default"`: prints a styled ASCII table and returns the underlying
  `data.frame` invisibly (S3 class `"spicy_continuous_table"`).

- `"data.frame"`: a plain `data.frame` with columns `variable`, `label`,
  `group` (when `by` is used), `mean`, `sd`, `min`, `max`, `ci_lower`,
  `ci_upper`, `n`. When `by` is used together with `p_value = TRUE`,
  `statistic = TRUE`, or `effect_size = TRUE`, additional columns are
  appended (populated on the first row of each variable block only):

  - `test_type` – test identifier (e.g., `"welch_t"`, `"welch_anova"`,
    `"student_t"`, `"anova"`, `"wilcoxon"`, `"kruskal"`).

  - `statistic`, `df1`, `df2`, `p.value` – test results.

  - `es_type` – effect-size identifier (`"hedges_g"`, `"eta_sq"`,
    `"r_rb"`, or `"epsilon_sq"`), when `effect_size = TRUE`.

  - `es_value`, `es_ci_lower`, `es_ci_upper` – effect-size estimate and
    confidence interval bounds.

- `"tinytable"`: a `tinytable` object.

- `"gt"`: a `gt_tbl` object.

- `"flextable"`: a `flextable` object.

- `"excel"` / `"word"`: writes to disk and returns the file path
  invisibly.

- `"clipboard"`: copies the table and returns the display `data.frame`
  invisibly.

## Details

Non-numeric columns are silently dropped (set `verbose = TRUE` to see
which columns were excluded). When a single constant column is passed,
SD and CI are shown as `"--"` in the ASCII table.

Optional output engines require suggested packages:

- tinytable for `output = "tinytable"`

- gt for `output = "gt"`

- flextable for `output = "flextable"`

- flextable + officer for `output = "word"`

- openxlsx2 for `output = "excel"`

- clipr for `output = "clipboard"`

## See also

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
for categorical variables;
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) for
one-way frequency tables;
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
for two-way cross-tabulations.

## Examples

``` r
# Basic usage with all numeric columns
table_continuous(sochealth, output = "data.frame")
#>                  variable                                      label
#> 1                     age                                Age (years)
#> 2                  income             Monthly household income (CHF)
#> 3         wellbeing_score              WHO-5 wellbeing index (0-100)
#> 4                     bmi                            Body mass index
#> 5      political_position  Political position (0 = left, 10 = right)
#> 6         life_sat_health             Satisfaction with health (1-5)
#> 7           life_sat_work               Satisfaction with work (1-5)
#> 8  life_sat_relationships      Satisfaction with relationships (1-5)
#> 9       life_sat_standard Satisfaction with standard of living (1-5)
#> 10                 weight                       Survey design weight
#>            mean          sd      min      max     ci_lower    ci_upper    n
#> 1    49.2641667   14.704293   25.000   75.000   48.4313676   50.096966 1200
#> 2  3832.9958333 1394.584239 1000.000 7388.000 3754.0115203 3911.980146 1200
#> 3    69.0405833   15.620359   18.700  100.000   68.1559015   69.925265 1200
#> 4    25.9314815    3.720186   16.000   38.900   25.7197197   26.143243 1188
#> 5     5.4801688    2.033874    0.000   10.000    5.3642491    5.596088 1185
#> 6     3.5486577    1.250081    1.000    5.000    3.4776199    3.619696 1192
#> 7     3.3783557    1.184975    1.000    5.000    3.3110176    3.445694 1192
#> 8     3.7248322    1.097186    1.000    5.000    3.6624829    3.787182 1192
#> 9     3.3968121    1.160253    1.000    5.000    3.3308789    3.462745 1192
#> 10    0.9970617    0.406257    0.294    3.447    0.9740527    1.020071 1200

# Select specific columns with tidyselect
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  output = "data.frame"
)
#>          variable                         label     mean        sd  min   max
#> 1             bmi               Body mass index 25.93148  3.720186 16.0  38.9
#> 2 wellbeing_score WHO-5 wellbeing index (0-100) 69.04058 15.620359 18.7 100.0
#>   ci_lower ci_upper    n
#> 1 25.71972 26.14324 1188
#> 2 68.15590 69.92527 1200

# Grouped descriptives
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education,
  output = "data.frame"
)
#>          variable                         label           group     mean
#> 1             bmi               Body mass index Lower secondary 28.08731
#> 2             bmi               Body mass index Upper secondary 26.01891
#> 3             bmi               Body mass index        Tertiary 24.39036
#> 4 wellbeing_score WHO-5 wellbeing index (0-100) Lower secondary 57.21571
#> 5 wellbeing_score WHO-5 wellbeing index (0-100) Upper secondary 68.96920
#> 6 wellbeing_score WHO-5 wellbeing index (0-100)        Tertiary 76.85250
#>          sd  min   max ci_lower ci_upper   n
#> 1  3.471744 18.2  38.9 27.66333 28.51129 260
#> 2  3.434736 16.0  37.1 25.72693 26.31090 534
#> 3  3.520150 16.0  33.0 24.04170 24.73901 394
#> 4 15.444587 18.7  97.9 55.33323 59.09819 261
#> 5 13.621193 26.7 100.0 67.81669 70.12172 539
#> 6 13.226818 40.4 100.0 75.55235 78.15265 400

# Grouped descriptives with p-value
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education,
  p_value = TRUE,
  output = "data.frame"
)
#>          variable                         label           group     mean
#> 1             bmi               Body mass index Lower secondary 28.08731
#> 2             bmi               Body mass index Upper secondary 26.01891
#> 3             bmi               Body mass index        Tertiary 24.39036
#> 4 wellbeing_score WHO-5 wellbeing index (0-100) Lower secondary 57.21571
#> 5 wellbeing_score WHO-5 wellbeing index (0-100) Upper secondary 68.96920
#> 6 wellbeing_score WHO-5 wellbeing index (0-100)        Tertiary 76.85250
#>          sd  min   max ci_lower ci_upper   n   test_type statistic df1      df2
#> 1  3.471744 18.2  38.9 27.66333 28.51129 260 welch_anova  87.95902   2 654.4758
#> 2  3.434736 16.0  37.1 25.72693 26.31090 534        <NA>        NA  NA       NA
#> 3  3.520150 16.0  33.0 24.04170 24.73901 394        <NA>        NA  NA       NA
#> 4 15.444587 18.7  97.9 55.33323 59.09819 261 welch_anova 144.35083   2 638.5873
#> 5 13.621193 26.7 100.0 67.81669 70.12172 539        <NA>        NA  NA       NA
#> 6 13.226818 40.4 100.0 75.55235 78.15265 400        <NA>        NA  NA       NA
#>        p.value
#> 1 1.467916e-34
#> 2           NA
#> 3           NA
#> 4 1.888362e-52
#> 5           NA
#> 6           NA

# Grouped descriptives with test statistic only
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education,
  statistic = TRUE,
  output = "data.frame"
)
#>          variable                         label           group     mean
#> 1             bmi               Body mass index Lower secondary 28.08731
#> 2             bmi               Body mass index Upper secondary 26.01891
#> 3             bmi               Body mass index        Tertiary 24.39036
#> 4 wellbeing_score WHO-5 wellbeing index (0-100) Lower secondary 57.21571
#> 5 wellbeing_score WHO-5 wellbeing index (0-100) Upper secondary 68.96920
#> 6 wellbeing_score WHO-5 wellbeing index (0-100)        Tertiary 76.85250
#>          sd  min   max ci_lower ci_upper   n   test_type statistic df1      df2
#> 1  3.471744 18.2  38.9 27.66333 28.51129 260 welch_anova  87.95902   2 654.4758
#> 2  3.434736 16.0  37.1 25.72693 26.31090 534        <NA>        NA  NA       NA
#> 3  3.520150 16.0  33.0 24.04170 24.73901 394        <NA>        NA  NA       NA
#> 4 15.444587 18.7  97.9 55.33323 59.09819 261 welch_anova 144.35083   2 638.5873
#> 5 13.621193 26.7 100.0 67.81669 70.12172 539        <NA>        NA  NA       NA
#> 6 13.226818 40.4 100.0 75.55235 78.15265 400        <NA>        NA  NA       NA
#>        p.value
#> 1 1.467916e-34
#> 2           NA
#> 3           NA
#> 4 1.888362e-52
#> 5           NA
#> 6           NA

# Grouped descriptives with both p-value and test statistic
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education,
  p_value = TRUE,
  statistic = TRUE,
  output = "data.frame"
)
#>          variable                         label           group     mean
#> 1             bmi               Body mass index Lower secondary 28.08731
#> 2             bmi               Body mass index Upper secondary 26.01891
#> 3             bmi               Body mass index        Tertiary 24.39036
#> 4 wellbeing_score WHO-5 wellbeing index (0-100) Lower secondary 57.21571
#> 5 wellbeing_score WHO-5 wellbeing index (0-100) Upper secondary 68.96920
#> 6 wellbeing_score WHO-5 wellbeing index (0-100)        Tertiary 76.85250
#>          sd  min   max ci_lower ci_upper   n   test_type statistic df1      df2
#> 1  3.471744 18.2  38.9 27.66333 28.51129 260 welch_anova  87.95902   2 654.4758
#> 2  3.434736 16.0  37.1 25.72693 26.31090 534        <NA>        NA  NA       NA
#> 3  3.520150 16.0  33.0 24.04170 24.73901 394        <NA>        NA  NA       NA
#> 4 15.444587 18.7  97.9 55.33323 59.09819 261 welch_anova 144.35083   2 638.5873
#> 5 13.621193 26.7 100.0 67.81669 70.12172 539        <NA>        NA  NA       NA
#> 6 13.226818 40.4 100.0 75.55235 78.15265 400        <NA>        NA  NA       NA
#>        p.value
#> 1 1.467916e-34
#> 2           NA
#> 3           NA
#> 4 1.888362e-52
#> 5           NA
#> 6           NA

# Student t-test / classic ANOVA (assumes equal variances)
table_continuous(
  sochealth,
  select = wellbeing_score,
  by = education,
  test = "student",
  p_value = TRUE,
  output = "data.frame"
)
#>          variable                         label           group     mean
#> 1 wellbeing_score WHO-5 wellbeing index (0-100) Lower secondary 57.21571
#> 2 wellbeing_score WHO-5 wellbeing index (0-100) Upper secondary 68.96920
#> 3 wellbeing_score WHO-5 wellbeing index (0-100)        Tertiary 76.85250
#>         sd  min   max ci_lower ci_upper   n test_type statistic df1  df2
#> 1 15.44459 18.7  97.9 55.33323 59.09819 261     anova  157.3698   2 1197
#> 2 13.62119 26.7 100.0 67.81669 70.12172 539      <NA>        NA  NA   NA
#> 3 13.22682 40.4 100.0 75.55235 78.15265 400      <NA>        NA  NA   NA
#>        p.value
#> 1 2.100699e-61
#> 2           NA
#> 3           NA

# Nonparametric test (Kruskal-Wallis for 3+ groups)
table_continuous(
  sochealth,
  select = wellbeing_score,
  by = education,
  test = "nonparametric",
  p_value = TRUE,
  statistic = TRUE,
  output = "data.frame"
)
#>          variable                         label           group     mean
#> 1 wellbeing_score WHO-5 wellbeing index (0-100) Lower secondary 57.21571
#> 2 wellbeing_score WHO-5 wellbeing index (0-100) Upper secondary 68.96920
#> 3 wellbeing_score WHO-5 wellbeing index (0-100)        Tertiary 76.85250
#>         sd  min   max ci_lower ci_upper   n test_type statistic df1 df2
#> 1 15.44459 18.7  97.9 55.33323 59.09819 261   kruskal  233.5302   2  NA
#> 2 13.62119 26.7 100.0 67.81669 70.12172 539      <NA>        NA  NA  NA
#> 3 13.22682 40.4 100.0 75.55235 78.15265 400      <NA>        NA  NA  NA
#>        p.value
#> 1 1.947889e-51
#> 2           NA
#> 3           NA

# Effect size (eta-squared for 3 groups)
table_continuous(
  sochealth,
  select = wellbeing_score,
  by = education,
  effect_size = TRUE,
  output = "data.frame"
)
#>          variable                         label           group     mean
#> 1 wellbeing_score WHO-5 wellbeing index (0-100) Lower secondary 57.21571
#> 2 wellbeing_score WHO-5 wellbeing index (0-100) Upper secondary 68.96920
#> 3 wellbeing_score WHO-5 wellbeing index (0-100)        Tertiary 76.85250
#>         sd  min   max ci_lower ci_upper   n   test_type statistic df1      df2
#> 1 15.44459 18.7  97.9 55.33323 59.09819 261 welch_anova  144.3508   2 638.5873
#> 2 13.62119 26.7 100.0 67.81669 70.12172 539        <NA>        NA  NA       NA
#> 3 13.22682 40.4 100.0 75.55235 78.15265 400        <NA>        NA  NA       NA
#>        p.value es_type es_value es_ci_lower es_ci_upper
#> 1 1.888362e-52  eta_sq 0.208197   0.1690121   0.2461732
#> 2           NA    <NA>       NA          NA          NA
#> 3           NA    <NA>       NA          NA          NA

# Effect size with confidence interval
table_continuous(
  sochealth,
  select = wellbeing_score,
  by = education,
  p_value = TRUE,
  effect_size_ci = TRUE,
  output = "data.frame"
)
#> Warning: `effect_size_ci` implies `effect_size = TRUE`.
#>          variable                         label           group     mean
#> 1 wellbeing_score WHO-5 wellbeing index (0-100) Lower secondary 57.21571
#> 2 wellbeing_score WHO-5 wellbeing index (0-100) Upper secondary 68.96920
#> 3 wellbeing_score WHO-5 wellbeing index (0-100)        Tertiary 76.85250
#>         sd  min   max ci_lower ci_upper   n   test_type statistic df1      df2
#> 1 15.44459 18.7  97.9 55.33323 59.09819 261 welch_anova  144.3508   2 638.5873
#> 2 13.62119 26.7 100.0 67.81669 70.12172 539        <NA>        NA  NA       NA
#> 3 13.22682 40.4 100.0 75.55235 78.15265 400        <NA>        NA  NA       NA
#>        p.value es_type es_value es_ci_lower es_ci_upper
#> 1 1.888362e-52  eta_sq 0.208197   0.1690121   0.2461732
#> 2           NA    <NA>       NA          NA          NA
#> 3           NA    <NA>       NA          NA          NA

# Nonparametric effect size (epsilon-squared with bootstrap CI)
# \donttest{
table_continuous(
  sochealth,
  select = wellbeing_score,
  by = education,
  test = "nonparametric",
  effect_size_ci = TRUE,
  output = "data.frame"
)
#> Warning: `effect_size_ci` implies `effect_size = TRUE`.
#>          variable                         label           group     mean
#> 1 wellbeing_score WHO-5 wellbeing index (0-100) Lower secondary 57.21571
#> 2 wellbeing_score WHO-5 wellbeing index (0-100) Upper secondary 68.96920
#> 3 wellbeing_score WHO-5 wellbeing index (0-100)        Tertiary 76.85250
#>         sd  min   max ci_lower ci_upper   n test_type statistic df1 df2
#> 1 15.44459 18.7  97.9 55.33323 59.09819 261   kruskal  233.5302   2  NA
#> 2 13.62119 26.7 100.0 67.81669 70.12172 539      <NA>        NA  NA  NA
#> 3 13.22682 40.4 100.0 75.55235 78.15265 400      <NA>        NA  NA  NA
#>        p.value    es_type  es_value es_ci_lower es_ci_upper
#> 1 1.947889e-51 epsilon_sq 0.1934254   0.1525021   0.2370707
#> 2           NA       <NA>        NA          NA          NA
#> 3           NA       <NA>        NA          NA          NA
# }

# Hedges' g for 2 groups
table_continuous(
  sochealth,
  select = wellbeing_score,
  by = sex,
  effect_size_ci = TRUE,
  output = "data.frame"
)
#> Warning: `effect_size_ci` implies `effect_size = TRUE`.
#>          variable                         label  group     mean       sd  min
#> 1 wellbeing_score WHO-5 wellbeing index (0-100) Female 67.16194 14.79849 19.6
#> 2 wellbeing_score WHO-5 wellbeing index (0-100)   Male 71.04879 16.22730 18.7
#>   max ci_lower ci_upper   n test_type statistic    df1 df2      p.value
#> 1 100  65.9948 68.32907 620   welch_t -4.326141 1168.7  NA 1.647005e-05
#> 2 100  69.7254 72.37219 580      <NA>        NA     NA  NA           NA
#>    es_type   es_value es_ci_lower es_ci_upper
#> 1 hedges_g -0.2505192  -0.3641835   -0.136855
#> 2     <NA>         NA          NA          NA

# Regex column selection
table_continuous(
  sochealth,
  select = "^life_sat",
  regex = TRUE,
  output = "data.frame"
)
#>                 variable                                      label     mean
#> 1        life_sat_health             Satisfaction with health (1-5) 3.548658
#> 2          life_sat_work               Satisfaction with work (1-5) 3.378356
#> 3 life_sat_relationships      Satisfaction with relationships (1-5) 3.724832
#> 4      life_sat_standard Satisfaction with standard of living (1-5) 3.396812
#>         sd min max ci_lower ci_upper    n
#> 1 1.250081   1   5 3.477620 3.619696 1192
#> 2 1.184975   1   5 3.311018 3.445694 1192
#> 3 1.097186   1   5 3.662483 3.787182 1192
#> 4 1.160253   1   5 3.330879 3.462745 1192

# Custom labels
table_continuous(
  sochealth,
  select = c(bmi, life_sat_health),
  labels = c(
    bmi = "Body mass index",
    life_sat_health = "Satisfaction with health"
  ),
  output = "data.frame"
)
#>          variable                    label      mean       sd min  max ci_lower
#> 1             bmi          Body mass index 25.931481 3.720186  16 38.9 25.71972
#> 2 life_sat_health Satisfaction with health  3.548658 1.250081   1  5.0  3.47762
#>    ci_upper    n
#> 1 26.143243 1188
#> 2  3.619696 1192

# \donttest{
# ASCII table (default)
table_continuous(sochealth, select = starts_with("life_sat"))
#> Descriptive statistics
#> 
#>  Variable                                   │  M     SD   Min   Max   95% CI LL 
#> ────────────────────────────────────────────┼───────────────────────────────────
#>  Satisfaction with health (1-5)             │ 3.55  1.25  1.00  5.00    3.48    
#>  Satisfaction with work (1-5)               │ 3.38  1.18  1.00  5.00    3.31    
#>  Satisfaction with relationships (1-5)      │ 3.72  1.10  1.00  5.00    3.66    
#>  Satisfaction with standard of living (1-5) │ 3.40  1.16  1.00  5.00    3.33    
#> 
#>  Variable                                   │ 95% CI UL     n 
#> ────────────────────────────────────────────┼─────────────────
#>  Satisfaction with health (1-5)             │   3.62     1192 
#>  Satisfaction with work (1-5)               │   3.45     1192 
#>  Satisfaction with relationships (1-5)      │   3.79     1192 
#>  Satisfaction with standard of living (1-5) │   3.46     1192 

# Grouped ASCII table
table_continuous(sochealth, select = starts_with("life_sat"), by = education)
#> Descriptive statistics
#> 
#>  Variable                                   │ Group             M     SD   Min  
#> ────────────────────────────────────────────┼───────────────────────────────────
#>  Satisfaction with health (1-5)             │ Lower secondary  2.71  1.20  1.00 
#>                                             │ Upper secondary  3.53  1.19  1.00 
#>                                             │ Tertiary         4.11  1.04  1.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with work (1-5)               │ Lower secondary  2.57  1.15  1.00 
#>                                             │ Upper secondary  3.42  1.10  1.00 
#>                                             │ Tertiary         3.85  1.03  1.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with relationships (1-5)      │ Lower secondary  3.02  1.23  1.00 
#>                                             │ Upper secondary  3.74  0.96  1.00 
#>                                             │ Tertiary         4.16  0.93  1.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with standard of living (1-5) │ Lower secondary  2.67  1.16  1.00 
#>                                             │ Upper secondary  3.39  1.11  1.00 
#>                                             │ Tertiary         3.89  0.96  1.00 
#> 
#>  Variable                                   │ Group            Max   95% CI LL 
#> ────────────────────────────────────────────┼──────────────────────────────────
#>  Satisfaction with health (1-5)             │ Lower secondary  5.00    2.57    
#>                                             │ Upper secondary  5.00    3.43    
#>                                             │ Tertiary         5.00    4.01    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with work (1-5)               │ Lower secondary  5.00    2.43    
#>                                             │ Upper secondary  5.00    3.33    
#>                                             │ Tertiary         5.00    3.75    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with relationships (1-5)      │ Lower secondary  5.00    2.87    
#>                                             │ Upper secondary  5.00    3.66    
#>                                             │ Tertiary         5.00    4.07    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with standard of living (1-5) │ Lower secondary  5.00    2.52    
#>                                             │ Upper secondary  5.00    3.29    
#>                                             │ Tertiary         5.00    3.79    
#> 
#>  Variable                                   │ Group            95% CI UL    n 
#> ────────────────────────────────────────────┼─────────────────────────────────
#>  Satisfaction with health (1-5)             │ Lower secondary    2.86     259 
#>                                             │ Upper secondary    3.63     534 
#>                                             │ Tertiary           4.21     399 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with work (1-5)               │ Lower secondary    2.71     261 
#>                                             │ Upper secondary    3.52     535 
#>                                             │ Tertiary           3.95     396 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with relationships (1-5)      │ Lower secondary    3.17     260 
#>                                             │ Upper secondary    3.83     534 
#>                                             │ Tertiary           4.25     398 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with standard of living (1-5) │ Lower secondary    2.81     261 
#>                                             │ Upper secondary    3.48     532 
#>                                             │ Tertiary           3.98     399 

# tinytable output
if (requireNamespace("tinytable", quietly = TRUE)) {
  table_continuous(sochealth, output = "tinytable")
  table_continuous(
    sochealth,
    select = starts_with("life_sat"),
    by = education,
    output = "tinytable"
  )
}
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> | Variable                                   | Group           | M    | SD   | Min  | Max  | 95% CI      | n   |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> |                                            |                 |      |      |      |      | LL   | UL   |     |
#> +============================================+=================+======+======+======+======+======+======+=====+
#> | Satisfaction with health (1-5)             | Lower secondary | 2.71 | 1.20 | 1.00 | 5.00 | 2.57 | 2.86 | 259 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> |                                            | Upper secondary | 3.53 | 1.19 | 1.00 | 5.00 | 3.43 | 3.63 | 534 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> |                                            | Tertiary        | 4.11 | 1.04 | 1.00 | 5.00 | 4.01 | 4.21 | 399 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> | Satisfaction with work (1-5)               | Lower secondary | 2.57 | 1.15 | 1.00 | 5.00 | 2.43 | 2.71 | 261 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> |                                            | Upper secondary | 3.42 | 1.10 | 1.00 | 5.00 | 3.33 | 3.52 | 535 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> |                                            | Tertiary        | 3.85 | 1.03 | 1.00 | 5.00 | 3.75 | 3.95 | 396 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> | Satisfaction with relationships (1-5)      | Lower secondary | 3.02 | 1.23 | 1.00 | 5.00 | 2.87 | 3.17 | 260 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> |                                            | Upper secondary | 3.74 | 0.96 | 1.00 | 5.00 | 3.66 | 3.83 | 534 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> |                                            | Tertiary        | 4.16 | 0.93 | 1.00 | 5.00 | 4.07 | 4.25 | 398 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> | Satisfaction with standard of living (1-5) | Lower secondary | 2.67 | 1.16 | 1.00 | 5.00 | 2.52 | 2.81 | 261 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> |                                            | Upper secondary | 3.39 | 1.11 | 1.00 | 5.00 | 3.29 | 3.48 | 532 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+
#> |                                            | Tertiary        | 3.89 | 0.96 | 1.00 | 5.00 | 3.79 | 3.98 | 399 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+ 

# gt output
if (requireNamespace("gt", quietly = TRUE)) {
  table_continuous(sochealth, output = "gt")
  table_continuous(
    sochealth,
    select = starts_with("life_sat"),
    by = education,
    output = "gt"
  )
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

Satisfaction with health (1-5)

Lower secondary

2.71

1.20

1.00

5.00

2.57

2.86

259

Upper secondary

3.53

1.19

1.00

5.00

3.43

3.63

534

Tertiary

4.11

1.04

1.00

5.00

4.01

4.21

399

Satisfaction with work (1-5)

Lower secondary

2.57

1.15

1.00

5.00

2.43

2.71

261

Upper secondary

3.42

1.10

1.00

5.00

3.33

3.52

535

Tertiary

3.85

1.03

1.00

5.00

3.75

3.95

396

Satisfaction with relationships (1-5)

Lower secondary

3.02

1.23

1.00

5.00

2.87

3.17

260

Upper secondary

3.74

0.96

1.00

5.00

3.66

3.83

534

Tertiary

4.16

0.93

1.00

5.00

4.07

4.25

398

Satisfaction with standard of living (1-5)

Lower secondary

2.67

1.16

1.00

5.00

2.52

2.81

261

Upper secondary

3.39

1.11

1.00

5.00

3.29

3.48

532

Tertiary

3.89

0.96

1.00

5.00

3.79

3.98

399

\# flextable output if
([requireNamespace](https://rdrr.io/r/base/ns-load.html)("flextable",
quietly = TRUE)) { table_continuous(sochealth, output = "flextable")
table_continuous(sochealth, by = education, output = "flextable") }

| Variable                                   | Group           | M       | SD     | Min     | Max     | 95% CI  |         | n   |
|--------------------------------------------|-----------------|---------|--------|---------|---------|---------|---------|-----|
|                                            |                 |         |        |         |         | LL      | UL      |     |
| Age (years)                                | Lower secondary | 48.88   | 14.76  | 25.00   | 75.00   | 47.08   | 50.68   | 261 |
|                                            | Upper secondary | 48.70   | 14.46  | 25.00   | 75.00   | 47.47   | 49.92   | 539 |
|                                            | Tertiary        | 50.28   | 14.97  | 25.00   | 75.00   | 48.81   | 51.75   | 400 |
| Monthly household income (CHF)             | Lower secondary | 2330.12 | 815.24 | 1000.00 | 4923.00 | 2230.75 | 2429.49 | 261 |
|                                            | Upper secondary | 3517.80 | 850.28 | 1000.00 | 5993.00 | 3445.85 | 3589.74 | 539 |
|                                            | Tertiary        | 5238.35 | 919.12 | 2351.00 | 7388.00 | 5148.01 | 5328.70 | 400 |
| WHO-5 wellbeing index (0-100)              | Lower secondary | 57.22   | 15.44  | 18.70   | 97.90   | 55.33   | 59.10   | 261 |
|                                            | Upper secondary | 68.97   | 13.62  | 26.70   | 100.00  | 67.82   | 70.12   | 539 |
|                                            | Tertiary        | 76.85   | 13.23  | 40.40   | 100.00  | 75.55   | 78.15   | 400 |
| Body mass index                            | Lower secondary | 28.09   | 3.47   | 18.20   | 38.90   | 27.66   | 28.51   | 260 |
|                                            | Upper secondary | 26.02   | 3.43   | 16.00   | 37.10   | 25.73   | 26.31   | 534 |
|                                            | Tertiary        | 24.39   | 3.52   | 16.00   | 33.00   | 24.04   | 24.74   | 394 |
| Political position (0 = left, 10 = right)  | Lower secondary | 5.40    | 2.01   | 0.00    | 10.00   | 5.15    | 5.64    | 259 |
|                                            | Upper secondary | 5.37    | 2.05   | -0.00   | 10.00   | 5.19    | 5.54    | 531 |
|                                            | Tertiary        | 5.68    | 2.01   | 0.00    | 10.00   | 5.48    | 5.88    | 395 |
| Satisfaction with health (1-5)             | Lower secondary | 2.71    | 1.20   | 1.00    | 5.00    | 2.57    | 2.86    | 259 |
|                                            | Upper secondary | 3.53    | 1.19   | 1.00    | 5.00    | 3.43    | 3.63    | 534 |
|                                            | Tertiary        | 4.11    | 1.04   | 1.00    | 5.00    | 4.01    | 4.21    | 399 |
| Satisfaction with work (1-5)               | Lower secondary | 2.57    | 1.15   | 1.00    | 5.00    | 2.43    | 2.71    | 261 |
|                                            | Upper secondary | 3.42    | 1.10   | 1.00    | 5.00    | 3.33    | 3.52    | 535 |
|                                            | Tertiary        | 3.85    | 1.03   | 1.00    | 5.00    | 3.75    | 3.95    | 396 |
| Satisfaction with relationships (1-5)      | Lower secondary | 3.02    | 1.23   | 1.00    | 5.00    | 2.87    | 3.17    | 260 |
|                                            | Upper secondary | 3.74    | 0.96   | 1.00    | 5.00    | 3.66    | 3.83    | 534 |
|                                            | Tertiary        | 4.16    | 0.93   | 1.00    | 5.00    | 4.07    | 4.25    | 398 |
| Satisfaction with standard of living (1-5) | Lower secondary | 2.67    | 1.16   | 1.00    | 5.00    | 2.52    | 2.81    | 261 |
|                                            | Upper secondary | 3.39    | 1.11   | 1.00    | 5.00    | 3.29    | 3.48    | 532 |
|                                            | Tertiary        | 3.89    | 0.96   | 1.00    | 5.00    | 3.79    | 3.98    | 399 |
| Survey design weight                       | Lower secondary | 0.99    | 0.43   | 0.35    | 3.45    | 0.94    | 1.04    | 261 |
|                                            | Upper secondary | 1.01    | 0.40   | 0.29    | 2.30    | 0.98    | 1.04    | 539 |
|                                            | Tertiary        | 0.98    | 0.40   | 0.35    | 2.51    | 0.95    | 1.02    | 400 |

\# Word output if
([requireNamespace](https://rdrr.io/r/base/ns-load.html)("flextable",
quietly = TRUE) &&
[requireNamespace](https://rdrr.io/r/base/ns-load.html)("officer",
quietly = TRUE)) { table_continuous( sochealth, select =
[starts_with](https://tidyselect.r-lib.org/reference/starts_with.html)("life_sat"),
by = education, output = "word", word_path =
[tempfile](https://rdrr.io/r/base/tempfile.html)(fileext = ".docx") ) }
\# Excel output if
([requireNamespace](https://rdrr.io/r/base/ns-load.html)("openxlsx2",
quietly = TRUE)) { table_continuous( sochealth, select =
[starts_with](https://tidyselect.r-lib.org/reference/starts_with.html)("life_sat"),
by = education, output = "excel", excel_path =
[tempfile](https://rdrr.io/r/base/tempfile.html)(fileext = ".xlsx") ) }
\# }
