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
p-value tests H0: U = 0 (Wald z-test).

## Details

The uncertainty coefficient measures association using Shannon entropy.
Let \\H_X\\ and \\H_Y\\ be the marginal entropies of the **row** and
**column** variables respectively, and \\H\_{XY}\\ the joint entropy.

- `direction = "row"` (column predicts row): \\U = (H_X + H_Y - H\_{XY})
  / H_X\\.

- `direction = "column"` (row predicts column): \\U = (H_X + H_Y -
  H\_{XY}) / H_Y\\.

- `direction = "symmetric"`: \\U = 2 (H_X + H_Y - H\_{XY}) / (H_X +
  H_Y)\\.

The default `direction = "symmetric"` follows the SPSS and DescTools
convention: the symmetric coefficient is a standard, well-defined
variant with its own asymptotic standard error.
[`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md)
deliberately differs (its default is `"row"`) because its symmetric form
is a derived quantity without an analytic SE; see its documentation.

When the marginal entropy in the denominator is zero (the predicted
variable is constant, e.g. an unused factor level), the coefficient is
the undefined form \\0/0\\: the function returns `NA` with a
`spicy_undefined_stat` warning, like the other measures in the family.
For `direction = "symmetric"` this happens only when both variables are
constant; with a single constant variable the symmetric coefficient is a
well-defined 0.

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

## References

Theil, H. (1970). On the estimation of relationships involving
qualitative variables. *American Journal of Sociology*, 76(1), 103-154.
[doi:10.1086/224909](https://doi.org/10.1086/224909)

## See also

[`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md),
[`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md),
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
[`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md),
[`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md)

## Examples

``` r
tab <- table(sochealth$smoking, sochealth$education)
uncertainty_coef(tab)
#> [1] 0.01148762
uncertainty_coef(tab, direction = "row", detail = TRUE)
#> Estimate     SE  CI lower  CI upper     p
#>    0.018  0.008     0.003     0.032  .021
```
