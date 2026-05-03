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
  digits = 3L,
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

  A number between 0 and 1 giving the confidence level (default `0.95`).
  Only used when `detail = TRUE`. Set to `NULL` to omit the confidence
  interval.

- digits:

  Number of decimal places used when printing the result (default `3`).
  Only affects the `detail = TRUE` output.

- .include_se:

  Internal parameter; do not use.

## Value

Same structure as
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md):
a scalar when `detail = FALSE`, a named vector when `detail = TRUE`. The
p-value tests H0: U = 0 (Wald z-test).

## Details

The uncertainty coefficient measures association using Shannon entropy.
For `direction = "row"`: \\U = (H_X + H_Y - H\_{XY}) / H_X\\, where
\\H_X\\, \\H_Y\\ are the marginal entropies and \\H\_{XY}\\ is the joint
entropy. The symmetric version is \\U = 2 (H_X + H_Y - H\_{XY}) / (H_X +
H_Y)\\.

The entropy terms use the standard mathematical convention \\0 \log 0 =
0\\, matching SPSS / PSPP `CROSSTABS` and the definition in Cover &
Thomas (2006). Note that `DescTools::UncertCoef()` applies an additional
Laplace correction (replacing zero cells with \\1/n^2\\) before the
entropy computation, which produces slightly different point estimates
on tables with empty cells; that correction is uncommon in the
information-theory literature and is not used here. The asymptotic
standard errors follow the DescTools delta method; see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## See also

[`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md),
[`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

## Examples

``` r
tab <- table(sochealth$smoking, sochealth$education)
uncertainty_coef(tab)
#> [1] 0.01148762
uncertainty_coef(tab, direction = "row", detail = TRUE)
#> Estimate  CI lower  CI upper     p
#>    0.018     0.003     0.032  .021
```
