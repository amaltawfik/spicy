# Phi coefficient

`phi()` computes the phi coefficient for a 2x2 contingency table.

## Usage

``` r
phi(x, detail = FALSE, conf_level = 0.95, digits = 3L, .include_se = FALSE)
```

## Arguments

- x:

  A contingency table (of class `table`).

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
p-value tests the null hypothesis of no association (Pearson chi-squared
test).

## Details

The phi coefficient is \\\phi = \sqrt{\chi^2 / n}\\. It is equivalent to
Cramer's V for 2x2 tables and equals the Pearson correlation between the
two binary variables. The point estimate matches the DescTools
(Signorell et al., 2024) and SPSS implementations. The confidence
interval uses the Fisher z-transformation on \\\phi\\; see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for the formula and full references.

## See also

[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
[`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

## Examples

``` r
tab <- table(sochealth$smoking, sochealth$sex)
phi(tab)
#> [1] 0.0107495
phi(tab, detail = TRUE)
#> Estimate  CI lower  CI upper     p
#>    0.011     0.000     0.068  .713
```
