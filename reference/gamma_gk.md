# Goodman-Kruskal Gamma

`gamma_gk()` computes the Goodman-Kruskal Gamma statistic for a two-way
contingency table of ordinal variables.

## Usage

``` r
gamma_gk(
  x,
  detail = FALSE,
  conf_level = 0.95,
  digits = 3L,
  .include_se = FALSE
)
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
p-value tests H0: gamma = 0 (Wald z-test).

## Details

Gamma is computed as \\\gamma = (C - D) / (C + D)\\, where \\C\\ and
\\D\\ are the numbers of concordant and discordant pairs. It ignores
tied pairs, making it appropriate for ordinal variables with many ties.
Standard error formulas follow the DescTools implementations (Signorell
et al., 2024); see
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
for full references.

## See also

[`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
[`kendall_tau_c()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_c.md),
[`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

## Examples

``` r
tab <- table(sochealth$education, sochealth$self_rated_health)
gamma_gk(tab)
#> [1] 0.3104791
gamma_gk(tab, detail = TRUE)
#> Estimate  CI lower  CI upper      p
#>    0.310     0.238     0.383  <.001
```
