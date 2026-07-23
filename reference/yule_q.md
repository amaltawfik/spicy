# Yule's Q

`yule_q()` computes Yule's Q coefficient of association for a 2x2
contingency table.

## Usage

``` r
yule_q(x, detail = FALSE, conf_level = 0.95, digits = 3L)
```

## Arguments

- x:

  A contingency table (of class `table`).

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
p-value tests H0: Q = 0 (Wald z-test).

## Details

For a 2x2 table with cells \\a, b, c, d\\, Yule's Q is \\Q = (ad - bc) /
(ad + bc)\\. It is equivalent to the Goodman-Kruskal Gamma for 2x2
tables. The asymptotic standard error is \\SE = 0.5 (1 - Q^2)
\sqrt{1/a + 1/b + 1/c + 1/d}\\.

Edge cases: when `ad + bc = 0`, Q itself is undefined and the function
returns `NA` with a `spicy_undefined_stat` warning. When any cell is
zero (and `ad + bc > 0`), Q is well-defined but the SE formula divides
by zero – the point estimate is returned, and `se`, `ci_lower`,
`ci_upper`, and `p_value` are all `NA`.

Standard error formulas follow the DescTools implementations (Signorell
et al., 2024); see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## References

Yule, G. U. (1900). On the association of attributes in statistics.
*Philosophical Transactions of the Royal Society of London, Series A*,
194, 257-319.
[doi:10.1098/rsta.1900.0019](https://doi.org/10.1098/rsta.1900.0019)

## See also

[`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md),
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
[`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md),
[`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md)

## Examples

``` r
tab <- table(sochealth$smoking, sochealth$sex)
yule_q(tab)
#> [1] -0.02632653
```
