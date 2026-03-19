# Kendall's Tau-c (Stuart's Tau-c)

`kendall_tau_c()` computes Stuart's Tau-c (also known as Kendall's
Tau-c) for a two-way contingency table of ordinal variables.

## Usage

``` r
kendall_tau_c(x, detail = FALSE, conf_level = 0.95, .include_se = FALSE)
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

- .include_se:

  Internal parameter; do not use.

## Value

Same structure as
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md):
a scalar when `detail = FALSE`, a named vector when `detail = TRUE`. The
p-value is based on a Wald test.

## Details

Stuart's Tau-c is computed as \\\tau_c = 2m(C - D) / (n^2(m - 1))\\,
where \\m = \min(r, c)\\. It is appropriate for rectangular tables and
is not restricted to the range \\\[-1, 1\]\\ only for square tables.
Standard error formulas follow the DescTools implementations (Signorell
et al., 2024); see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## See also

[`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
[`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
[`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

## Examples

``` r
data(mtcars)
tab <- table(factor(mtcars$gear), factor(mtcars$cyl))
kendall_tau_c(tab)
#> [1] -0.4833984
```
