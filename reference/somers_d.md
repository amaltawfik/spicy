# Somers' D

`somers_d()` computes Somers' D for a two-way contingency table of
ordinal variables.

## Usage

``` r
somers_d(
  x,
  direction = c("row", "column", "symmetric"),
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

  Direction of prediction: `"row"` (default, column predicts row),
  `"column"` (row predicts column), or `"symmetric"` (average of both
  directions).

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
p-value tests H0: D = 0 (Wald z-test).

## Details

Somers' D is an asymmetric ordinal measure defined as \\d = (C - D) /
(C + D + T)\\, where \\T\\ is the number of pairs tied on the
independent variable. The symmetric version is the harmonic mean of the
two asymmetric values. Standard error formulas follow the DescTools
implementations (Signorell et al., 2024); see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## See also

[`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
[`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

Other association measures:
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md),
[`contingency_coef()`](https://amaltawfik.github.io/spicy/reference/contingency_coef.md),
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
[`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
[`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md),
[`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
[`kendall_tau_c()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_c.md),
[`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md),
[`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md),
[`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md),
[`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md)

## Examples

``` r
tab <- table(sochealth$education, sochealth$self_rated_health)
somers_d(tab, direction = "row")
#> [1] 0.2015369
somers_d(tab, direction = "column", detail = TRUE)
#> Estimate  CI lower  CI upper      p
#>    0.208     0.157     0.258  <.001
```
