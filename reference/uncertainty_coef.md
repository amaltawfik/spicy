# Uncertainty Coefficient

`uncertainty_coef()` computes the Uncertainty Coefficient (Theil's U)
for a two-way contingency table, based on information entropy.

## Usage

``` r
uncertainty_coef(
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

  Logical. If `FALSE` (default), return the estimate as a bare scalar.
  If `TRUE`, return a named numeric vector including confidence interval
  and p-value.

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

The uncertainty coefficient measures association using Shannon entropy.
For `direction = "row"`: \\U = (H_X + H_Y - H\_{XY}) / H_X\\, where
\\H_X\\, \\H_Y\\ are the marginal entropies and \\H\_{XY}\\ is the joint
entropy. The symmetric version is \\U = 2 (H_X + H_Y - H\_{XY}) / (H_X +
H_Y)\\. Standard error formulas follow the DescTools implementations
(Signorell et al., 2024); see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## See also

[`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md),
[`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

## Examples

``` r
data(mtcars)
tab <- table(factor(mtcars$gear), factor(mtcars$cyl))
uncertainty_coef(tab)
#> [1] 0.3504372
uncertainty_coef(tab, direction = "row", detail = TRUE)
#>     estimate     ci_lower     ci_upper      p_value 
#> 0.3587708805 0.1755315202 0.5420102407 0.0001243016 
```
