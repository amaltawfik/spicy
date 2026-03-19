# Pearson's contingency coefficient

`contingency_coef()` computes Pearson's contingency coefficient C for a
two-way contingency table.

## Usage

``` r
contingency_coef(x, detail = FALSE, conf_level = 0.95, .include_se = FALSE)
```

## Arguments

- x:

  A contingency table (of class `table`).

- detail:

  Logical. If `FALSE` (default), return the estimate as a bare scalar.
  If `TRUE`, return a named numeric vector including confidence interval
  and p-value.

- conf_level:

  Confidence level for the confidence interval (default `0.95`). Only
  used when `detail = TRUE`. Set to `NULL` to omit the confidence
  interval.

- .include_se:

  Internal parameter; do not use.

## Value

Same structure as
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md):
a scalar when `detail = FALSE`, a named vector when `detail = TRUE`. CI
values are `NA` because no standard asymptotic SE exists for C.

## Details

The contingency coefficient is \\C = \sqrt{\chi^2 / (\chi^2 + n)}\\. It
ranges from 0 (independence) to a maximum that depends on the table
dimensions. No standard asymptotic standard error exists, so the
confidence interval is not computed.

## See also

[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

## Examples

``` r
data(mtcars)
tab <- table(factor(mtcars$gear), factor(mtcars$cyl))
contingency_coef(tab)
#> [1] 0.6003875
```
