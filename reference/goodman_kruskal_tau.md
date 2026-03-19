# Goodman-Kruskal's Tau

`goodman_kruskal_tau()` computes Goodman-Kruskal's Tau, a proportional
reduction in error (PRE) measure for nominal variables.

## Usage

``` r
goodman_kruskal_tau(
  x,
  direction = c("row", "column"),
  detail = FALSE,
  conf_level = 0.95,
  .include_se = FALSE
)
```

## Arguments

- x:

  A contingency table (of class `table`).

- direction:

  Direction of prediction: `"row"` (default, column predicts row) or
  `"column"` (row predicts column).

- detail:

  Logical. If `FALSE` (default), return the estimate as a numeric
  scalar. If `TRUE`, return a named numeric vector including confidence
  interval and p-value.

- conf_level:

  A number between 0 and 1 giving the confidence level (default `0.95`).
  Only used when `detail = TRUE`. Set to `NULL` to omit the confidence
  interval.

- .include_se:

  Internal parameter; do not use.

## Value

Same structure as
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md):
a scalar when `detail = FALSE`, a named vector when `detail = TRUE`.

## Details

Unlike
[`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md),
Goodman-Kruskal's Tau uses all cell frequencies rather than only the
modal categories, making it more sensitive to association patterns where
lambda may be zero. Standard error formulas follow the DescTools
implementations (Signorell et al., 2024); see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## See also

[`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md),
[`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

## Examples

``` r
tab <- table(sochealth$smoking, sochealth$education)
goodman_kruskal_tau(tab)
#> [1] 0.01840572
goodman_kruskal_tau(tab, direction = "column", detail = TRUE)
#>    estimate    ci_lower    ci_upper     p_value 
#> 0.007609460 0.001118492 0.014100428 0.021579123 
```
