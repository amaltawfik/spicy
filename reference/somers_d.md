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
  digits = 3L
)
```

## Arguments

- x:

  A contingency table (of class `table`).

- direction:

  Direction of prediction: `"row"` (default, column predicts row),
  `"column"` (row predicts column), or `"symmetric"` (harmonic mean of
  both directions).

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
p-value tests H0: D = 0 (Wald z-test).

## Details

Somers' D is an asymmetric ordinal measure defined as \\d = (C - D) /
(C + D + T)\\, where \\T\\ is the number of pairs tied on the
independent variable. The symmetric version (`direction = "symmetric"`)
is the *harmonic* mean of the two asymmetric values, matching the SPSS /
PSPP convention; this is **not** identical to Kendall's Tau-b (which is
the *geometric* mean of the same two quantities), although the two often
agree to two decimals. It is computed via the equivalent closed form
\\2(C - D)\\ divided by the sum of the two asymmetric denominators, so a
table with exactly as many concordant as discordant pairs (e.g. an
independence pattern) yields the well-defined value 0 – the
harmonic-mean form is 0/0 there – as printed by SPSS / PSPP. The
symmetric estimate is `NA` only when one of the asymmetric directions is
itself undefined (with the same `spicy_undefined_stat` warning). No
analytic SE / CI is reported for the symmetric form: its `se` is always
`NA`, matching PSPP, which prints no ASE for it (DescTools offers no
symmetric form at all).

The default `direction = "row"` differs deliberately from
[`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md)
and
[`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md)
(which default to `"symmetric"`): Somers' D is intrinsically asymmetric,
its symmetric form is a derived convention without inference, and the
reference implementation (`DescTools::SomersDelta()`) offers only the
two asymmetric directions, defaulting to `"row"`.

Standard error formulas for the asymmetric directions follow the
DescTools implementations (Signorell et al., 2024); see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## References

Somers, R. H. (1962). A new asymmetric measure of association for
ordinal variables. *American Sociological Review*, 27(6), 799-811.
[doi:10.2307/2090408](https://doi.org/10.2307/2090408)

Brown, M. B., & Benedetti, J. K. (1977). Sampling behavior of tests for
correlation in two-way contingency tables. *Journal of the American
Statistical Association*, 72(358), 309-315.
[doi:10.1080/01621459.1977.10480995](https://doi.org/10.1080/01621459.1977.10480995)

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
#> Estimate     SE  CI lower  CI upper      p
#>    0.208  0.026     0.157     0.258  <.001
```
