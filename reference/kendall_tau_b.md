# Kendall's Tau-b

`kendall_tau_b()` computes Kendall's Tau-b for a two-way contingency
table of ordinal variables.

## Usage

``` r
kendall_tau_b(x, detail = FALSE, conf_level = 0.95, .include_se = FALSE)
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

Kendall's Tau-b is computed as \\\tau_b = (C - D) / \sqrt{(n_0 -
n_1)(n_0 - n_2)}\\, where \\n_0 = n(n-1)/2\\, \\n_1\\ is the number of
pairs tied on the row variable, and \\n_2\\ is the number tied on the
column variable. Tau-b corrects for ties and is appropriate for square
tables. Standard error formulas follow the DescTools implementations
(Signorell et al., 2024); see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## See also

[`kendall_tau_c()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_c.md),
[`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
[`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

## Examples

``` r
data(mtcars)
tab <- table(factor(mtcars$gear), factor(mtcars$cyl))
kendall_tau_b(tab)
#> [1] -0.5125435
```
