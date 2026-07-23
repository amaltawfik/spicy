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
  digits = 3L
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

  A single number strictly between 0 and 1 giving the confidence level
  (default `0.95`). Only used when `detail = TRUE`. Set to `NULL` to
  omit the confidence interval. Any other value – including percentages
  such as `95` – raises a classed error (`spicy_invalid_input`).

- digits:

  Number of decimal places used when printing the result (default `3`).
  Only affects the `detail = TRUE` output.

## Value

Same structure as
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md):
a scalar when `detail = FALSE`, a named vector when `detail = TRUE`. The
p-value tests H0: lambda = 0 (Wald z-test).

## Details

Lambda measures how much prediction error is reduced when the
independent variable is used to predict the dependent variable. It
ranges from 0 (no reduction) to 1 (perfect prediction). Lambda can equal
zero even when variables are associated if the modal category dominates
in every column (or row).

The default `direction = "symmetric"` follows the SPSS and DescTools
convention: symmetric lambda is a standard, well-defined variant with
its own asymptotic standard error.
[`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md)
deliberately differs (its default is `"row"`) because its symmetric form
is a derived quantity without an analytic SE; see its documentation.

Standard error formulas follow the DescTools implementations (Signorell
et al., 2024); see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## References

Goodman, L. A., & Kruskal, W. H. (1954). Measures of association for
cross classifications. *Journal of the American Statistical
Association*, 49(268), 732-764.
[doi:10.2307/2281536](https://doi.org/10.2307/2281536)

## See also

[`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md),
[`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

Other association measures:
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md),
[`contingency_coef()`](https://amaltawfik.github.io/spicy/reference/contingency_coef.md),
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
[`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
[`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md),
[`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
[`kendall_tau_c()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_c.md),
[`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md),
[`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md),
[`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md),
[`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md)

## Examples

``` r
tab <- table(sochealth$smoking, sochealth$education)
lambda_gk(tab)
#> [1] 0
lambda_gk(tab, direction = "row")
#> [1] 0
lambda_gk(tab, direction = "column", detail = TRUE)
#> Estimate     SE  CI lower  CI upper   p
#>    0.000  0.000     0.000     0.000  --
```
