# Cramer's V

`cramer_v()` computes Cramer's V for a two-way contingency table,
measuring the strength of association between two categorical variables.

## Usage

``` r
cramer_v(x, detail = FALSE, conf_level = 0.95, .include_se = FALSE)
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

When `detail = FALSE`: a single numeric value (the estimate). When
`detail = TRUE` and `conf_level` is non-`NULL`:
`c(estimate, ci_lower, ci_upper, p_value)`. When `detail = TRUE` and
`conf_level = NULL`: `c(estimate, p_value)`. The p-value tests the null
hypothesis of no association (Pearson chi-squared test).

## Details

Cramer's V is computed as \\V = \sqrt{\chi^2 / (n \cdot (k - 1))}\\,
where \\\chi^2\\ is the Pearson chi-squared statistic, \\n\\ is the
total count, and \\k = \min(r, c)\\. The confidence interval uses the
Fisher z-transformation. Standard error formulas follow the DescTools
implementations (Signorell et al., 2024).

## References

Agresti, A. (2002). *Categorical Data Analysis* (2nd ed.). Wiley.

Liebetrau, A. M. (1983). *Measures of Association*. Sage.

Signorell, A. et al. (2024). *DescTools: Tools for Descriptive
Statistics*. R package.

## See also

[`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md),
[`contingency_coef()`](https://amaltawfik.github.io/spicy/reference/contingency_coef.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

## Examples

``` r
tab <- table(sochealth$smoking, sochealth$education)
cramer_v(tab)
#> [1] 0.1356677
cramer_v(tab, detail = TRUE)
#> Estimate  CI lower  CI upper  p value
#>    0.136     0.079     0.191  < 0.001
cramer_v(tab, detail = TRUE, conf_level = NULL)
#> Estimate  p value
#>    0.136  < 0.001
```
