# Phi coefficient

`phi()` computes the phi coefficient for a 2x2 contingency table.

## Usage

``` r
phi(x, detail = FALSE, conf_level = 0.95, .include_se = FALSE)
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
a scalar when `detail = FALSE`, a named vector when `detail = TRUE`. The
p-value comes from the Pearson chi-squared test.

## Details

The phi coefficient is \\\phi = \sqrt{\chi^2 / n}\\. It is equivalent to
Cramer's V for 2x2 tables and equals the Pearson correlation between the
two binary variables. The confidence interval uses the Fisher
z-transformation. Standard error formulas follow the DescTools
implementations (Signorell et al., 2024); see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## See also

[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
[`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

## Examples

``` r
data(mtcars)
tab <- table(factor(mtcars$vs), factor(mtcars$am))
phi(tab)
#> [1] 0.1683451
phi(tab, detail = TRUE)
#>  estimate  ci_lower  ci_upper   p_value 
#> 0.1683451 0.0000000 0.4883712 0.3409429 
```
