# Continuous-outcome linear-model table

Builds APA-style summary tables from a series of simple linear models
for one or many continuous outcomes selected with tidyselect syntax.

A single predictor is supplied with `by`, and each selected numeric
outcome is fit as `lm(outcome ~ by, ...)`. When `by` is categorical, the
function returns a model-based mean-comparison table with estimated
marginal means by level, plus an optional single difference for
dichotomous predictors. When `by` is numeric, the table reports the
slope and its confidence interval.

Multiple output formats are available via `output`: a printed ASCII
table (`"default"`), a plain wide `data.frame` (`"data.frame"`), a raw
long `data.frame` (`"long"`), or rendered outputs (`"tinytable"`,
`"gt"`, `"flextable"`, `"excel"`, `"clipboard"`, `"word"`).

## Usage

``` r
table_continuous_lm(
  data,
  select = dplyr::everything(),
  by,
  exclude = NULL,
  regex = FALSE,
  weights = NULL,
  vcov = c("classical", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5"),
  contrast = c("auto", "none"),
  statistic = FALSE,
  p_value = TRUE,
  show_n = TRUE,
  effect_size = c("none", "f2"),
  r2 = c("r2", "adj_r2", "none"),
  ci = TRUE,
  labels = NULL,
  ci_level = 0.95,
  digits = 2,
  decimal_mark = ".",
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = "Linear models",
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  A `data.frame`.

- select:

  Outcome columns to include. If `regex = FALSE`, use tidyselect syntax
  or a character vector of column names (default:
  [`dplyr::everything()`](https://tidyselect.r-lib.org/reference/everything.html)).
  If `regex = TRUE`, provide a regular expression pattern (character
  string).

- by:

  A single predictor column. Accepts an unquoted column name or a single
  character column name. The predictor can be numeric, logical,
  character, or factor.

- exclude:

  Columns to exclude from `select`. Supports tidyselect syntax and
  character vectors of column names.

- regex:

  Logical. If `FALSE` (the default), uses tidyselect helpers. If `TRUE`,
  the `select` argument is treated as a regular expression.

- weights:

  Optional case weights. Accepts `NULL`, an unquoted numeric column
  name, a single character column name, or a numeric vector of length
  `nrow(data)`.

- vcov:

  Variance estimator used for uncertainty estimates. One of
  `"classical"` (default), `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`,
  `"HC4m"`, or `"HC5"`.

- contrast:

  Contrast display for categorical predictors. One of:

  - `"auto"`: show a single reference contrast only when `by` has
    exactly two levels.

  - `"none"`: suppress difference columns for categorical predictors.

- statistic:

  Logical. If `TRUE`, includes a test-statistic column in the wide and
  rendered outputs. Defaults to `FALSE`.

- p_value:

  Logical. If `TRUE`, includes a `p` column in the wide and rendered
  outputs. Defaults to `TRUE`.

- show_n:

  Logical. If `TRUE`, includes an `n` column in the wide and rendered
  outputs. Defaults to `TRUE`.

- effect_size:

  Character. Effect-size column to include in the wide and rendered
  outputs. One of `"none"` (the default) or `"f2"`.

- r2:

  Character. Fit statistic to include in the wide and rendered outputs.
  One of `"r2"` (the default), `"adj_r2"`, or `"none"`.

- ci:

  Logical. If `TRUE`, includes contrast confidence-interval columns in
  the wide and rendered outputs when a single contrast is shown.
  Defaults to `TRUE`.

- labels:

  An optional named character vector of outcome labels. Names must match
  column names in `data`. When `NULL` (the default), labels are
  auto-detected from variable attributes; if none are found, the column
  name is used.

- ci_level:

  Confidence level for coefficient and estimated-mean intervals
  (default: `0.95`). Must be between 0 and 1 exclusive.

- digits:

  Number of decimal places for numeric output (default: `2`).

- decimal_mark:

  Character used as decimal separator. Either `"."` (default) or `","`.

- output:

  Output format. One of:

  - `"default"`: a printed ASCII table, returned invisibly

  - `"data.frame"`: a plain wide `data.frame`

  - `"long"`: a raw long `data.frame`

  - `"tinytable"` (requires `tinytable`)

  - `"gt"` (requires `gt`)

  - `"flextable"` (requires `flextable`)

  - `"excel"` (requires `openxlsx2`)

  - `"clipboard"` (requires `clipr`)

  - `"word"` (requires `flextable` and `officer`)

- excel_path:

  File path for `output = "excel"`.

- excel_sheet:

  Sheet name for `output = "excel"` (default: `"Linear models"`).

- clipboard_delim:

  Delimiter for `output = "clipboard"` (default: `"\t"`).

- word_path:

  File path for `output = "word"`.

- verbose:

  Logical. If `TRUE`, prints messages about ignored non-numeric selected
  outcomes (default: `FALSE`).

## Value

Depends on `output`:

- `"default"`: prints a styled ASCII table and returns the underlying
  long `data.frame` invisibly, with class `"spicy_continuous_lm_table"`.

- `"data.frame"`: a plain wide `data.frame` with one row per outcome and
  numeric columns for means, optional contrasts, optional test
  statistics, model fit, and sample size.

- `"long"`: a raw `data.frame` with one block per outcome and columns
  describing estimated means, contrasts, test statistics, model fit, and
  sample size.

- `"tinytable"`: a `tinytable` object.

- `"gt"`: a `gt_tbl` object.

- `"flextable"`: a `flextable` object.

- `"excel"` / `"word"`: writes to disk and returns the file path.

- `"clipboard"`: copies the wide table and returns it invisibly.

## Details

`table_continuous_lm()` is designed for article-style bivariate
reporting: a single predictor supplied with `by`, and one simple model
per selected continuous outcome. The model fit is always
`lm(outcome ~ by, ...)`.

Compared with
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
this function is the model-based companion for users who want to report
bivariate mean comparisons in a linear-model framework. In practice, it
is the better choice when you want heteroskedasticity-consistent
standard errors (`vcov = "HC*"`), model fit statistics, or case weights
via `lm(..., weights = ...)`.

Effect size is reported as Cohen's `f2`, computed from the model `R2` as
`R2 / (1 - R2)`. When `vcov != "classical"`, standard errors, confidence
intervals, and test statistics use the requested heteroskedasticity-
consistent estimator, while `R2` and adjusted `R2` remain the ordinary
least-squares fit statistics.

When `weights` is supplied, `table_continuous_lm()` fits weighted linear
models using `lm(..., weights = ...)` and reports weighted estimated
means or slopes accordingly. This is appropriate for case-weighted
analyses and weighted article tables, but it is not a substitute for a
full complex-survey design workflow.

For dichotomous categorical predictors, the wide outputs report means in
reference-level order and labels the contrast column explicitly as
`Delta (level2 - level1)`. For categorical predictors with more than two
levels, no single contrast or contrast confidence interval is shown in
the wide outputs; instead, the table reports level-specific means plus
the overall `F` test when `statistic = TRUE` (or `F(df1, df2)` when the
degrees of freedom are constant across outcomes).

Optional output engines require suggested packages:

- tinytable for `output = "tinytable"`

- gt for `output = "gt"`

- flextable for `output = "flextable"`

- flextable + officer for `output = "word"`

- openxlsx2 for `output = "excel"`

- clipr for `output = "clipboard"`

## See also

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)

## Examples

``` r
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  statistic = TRUE,
  output = "data.frame"
)
#>                                      Variable M (Female) M (Male)
#> wellbeing_score WHO-5 wellbeing index (0-100)   67.16194 71.04879
#> bmi                           Body mass index   25.68506 26.19685
#>                 Δ (Male - Female)  95% CI LL 95% CI UL        t            p
#> wellbeing_score         3.8868576 2.12952733 5.6441879 4.339426 1.548898e-05
#> bmi                     0.5117882 0.08879857 0.9347778 2.373843 1.776254e-02
#>                          R²    n
#> wellbeing_score 0.015475137 1200
#> bmi             0.004728908 1188

table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = age,
  vcov = "HC3",
  ci = FALSE,
  output = "long"
)
#>          variable                         label predictor_type predictor_label
#> 1 wellbeing_score WHO-5 wellbeing index (0-100)     continuous     Age (years)
#> 2             bmi               Body mass index     continuous     Age (years)
#>   level reference estimate_type emmean emmean_se emmean_ci_lower
#> 1  <NA>      <NA>         slope     NA        NA              NA
#> 2  <NA>      <NA>         slope     NA        NA              NA
#>   emmean_ci_upper   estimate estimate_se estimate_ci_lower estimate_ci_upper
#> 1              NA 0.04138096 0.030585451       -0.01862605        0.10138797
#> 2              NA 0.03508525 0.007455753        0.02045732        0.04971319
#>   test_type statistic df1  df2      p.value es_type    es_value          r2
#> 1         t  1.352962   1 1198 1.763230e-01      f2 0.001519732 0.001517426
#> 2         t  4.705796   1 1186 2.826189e-06      f2 0.019622500 0.019244867
#>         adj_r2    n sum_w
#> 1 0.0006839677 1200    NA
#> 2 0.0184179236 1188    NA

# \donttest{
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex
)
#> Continuous outcomes by Sex
#> 
#>  Variable                      │ M (Female)  M (Male)  Δ (Male - Female) 
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   67.16      71.05          3.89        
#>  Body mass index               │   25.69      26.20          0.51        
#> 
#>  Variable                      │ 95% CI LL  95% CI UL      p   R²      n 
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   2.13       5.64     <.001  0.02  1200 
#>  Body mass index               │   0.09       0.93      .018  0.00  1188 

if (requireNamespace("gt", quietly = TRUE)) {
  table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    output = "gt"
  )
}


  



        Variable
      
```

M (Female)

M (Male)

Δ (Male - Female)

95% CI

p

R²

n

LL

UL

WHO-5 wellbeing index (0-100)

67.16

71.05

3.89

2.13

5.64

\<.001

0.02

1200

Body mass index

25.69

26.20

0.51

0.09

0.93

.018

0.00

1188

\# }
