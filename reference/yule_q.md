# Yule's Q

`yule_q()` computes Yule's Q coefficient of association for a 2x2
contingency table.

## Usage

``` r
yule_q(x, detail = FALSE, conf_level = 0.95, .include_se = FALSE)
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

For a 2x2 table with cells \\a, b, c, d\\, Yule's Q is \\Q = (ad - bc) /
(ad + bc)\\. It is equivalent to the Goodman-Kruskal Gamma for 2x2
tables. The asymptotic standard error is \\SE = 0.5 (1 - Q^2)
\sqrt{1/a + 1/b + 1/c + 1/d}\\. Standard error formulas follow the
DescTools implementations (Signorell et al., 2024); see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## See also

[`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md),
[`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

## Examples

``` r
data(mtcars)
tab <- table(factor(mtcars$vs), factor(mtcars$am))
yule_q(tab)
#> [1] 0.3333333
```
