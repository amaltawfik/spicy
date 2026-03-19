# Goodman-Kruskal's Lambda

`lambda_gk()` computes Goodman-Kruskal's Lambda, a proportional
reduction in error (PRE) measure for nominal variables.

## Usage

``` r
lambda_gk(
  x,
  direction = c("symmetric", "row", "column"),
  detail = FALSE,
  conf_level = 0.95,
  .include_se = FALSE
)
```

## Arguments

- x:

  A contingency table (of class `table`).

- direction:

  Direction of prediction: `"symmetric"` (default), `"row"` (column
  predicts row), or `"column"` (row predicts column).

- detail:

  Logical. If `FALSE` (default), return the estimate as a numeric
  scalar. If `TRUE`, return a named numeric vector including confidence
  interval and p-value.

- conf_level:

  Confidence level for the confidence interval (default `0.95`). Only
  used when `detail = TRUE`. Set to `NULL` to omit the confidence
  interval.

- .include_se:

  Internal parameter; do not use.

## Value

Same structure as
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md):
a scalar when `detail = FALSE`, a named vector when `detail = TRUE`.

## Details

Lambda measures how much prediction error is reduced when the
independent variable is used to predict the dependent variable. It
ranges from 0 (no reduction) to 1 (perfect prediction). Lambda can equal
zero even when variables are associated if the modal category dominates
in every column (or row). Standard error formulas follow the DescTools
implementations (Signorell et al., 2024); see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## See also

[`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md),
[`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

## Examples

``` r
data(mtcars)
tab <- table(factor(mtcars$gear), factor(mtcars$cyl))
lambda_gk(tab)
#> [1] 0.4857143
lambda_gk(tab, direction = "row")
#> [1] 0.5294118
lambda_gk(tab, direction = "column", detail = TRUE)
#>    estimate    ci_lower    ci_upper     p_value 
#> 0.444444444 0.163299708 0.725589180 0.001945774 
```
