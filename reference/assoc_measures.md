# Association measures summary table

`assoc_measures()` computes a range of association measures for a
two-way contingency table and returns them in a tidy data frame.

## Usage

``` r
assoc_measures(
  x,
  type = c("auto", "nominal", "ordinal", "all"),
  conf_level = 0.95
)
```

## Arguments

- x:

  A contingency table (of class `table`).

- type:

  Which family of measures to compute: `"auto"` (default, all measures),
  `"nominal"`, `"ordinal"`, or `"all"`.

- conf_level:

  Confidence level for the confidence interval (default `0.95`). Set to
  `NULL` to omit the confidence interval.

## Value

A data frame with columns `measure`, `estimate`, `se`, `ci_lower`,
`ci_upper`, and `p_value`.

## Details

`type = "auto"` (the default) returns all nominal and ordinal measures.
Use `type = "nominal"` or `type = "ordinal"` to restrict the output to a
single family.

The nominal family includes
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
[`contingency_coef()`](https://amaltawfik.github.io/spicy/reference/contingency_coef.md),
[`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md),
[`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md),
[`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md),
and (for 2x2 tables)
[`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md) and
[`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md).

The ordinal family includes
[`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
[`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
[`kendall_tau_c()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_c.md),
and
[`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md).

## References

Agresti, A. (2002). *Categorical Data Analysis* (2nd ed.). Wiley.

Liebetrau, A. M. (1983). *Measures of Association*. Sage.

Signorell, A. et al. (2024). DescTools: Tools for Descriptive
Statistics. R package. Standard error formulas follow the DescTools
implementations.

## See also

[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
[`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
[`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md)

## Examples

``` r
data(mtcars)
tab <- table(factor(mtcars$gear), factor(mtcars$cyl))
assoc_measures(tab)
#>                              measure   estimate         se   ci_lower
#> 1                         Cramer's V  0.5308655         NA  0.2235534
#> 2            Contingency Coefficient  0.6003875         NA         NA
#> 3                   Lambda symmetric  0.4857143 0.12920157  0.2324839
#> 4                         Lambda R|C  0.5294118 0.15628498  0.2230988
#> 5                         Lambda C|R  0.4444444 0.14344383  0.1632997
#> 6            Goodman-Kruskal Tau R|C  0.3825603 0.11759802  0.1520724
#> 7            Goodman-Kruskal Tau C|R  0.3386018 0.09944838  0.1436866
#> 8  Uncertainty Coefficient symmetric  0.3504372 0.08940615  0.1752043
#> 9        Uncertainty Coefficient R|C  0.3587709 0.09349119  0.1755315
#> 10       Uncertainty Coefficient C|R  0.3424818 0.08737530  0.1712294
#> 11             Goodman-Kruskal Gamma -0.6573705 0.17278471 -0.9960223
#> 12                   Kendall's Tau-b -0.5125435 0.14889469 -0.8043717
#> 13                    Stuart's Tau-c -0.4833984 0.13209204 -0.7422941
#> 14                     Somers' D R|C -0.5015198 0.14077197 -0.7774278
#> 15                     Somers' D C|R -0.5238095 0.16050578 -0.8383951
#>      ci_upper      p_value
#> 1   0.7421756 1.214066e-03
#> 2          NA 1.214066e-03
#> 3   0.7389447 1.703534e-04
#> 4   0.8357247 7.053865e-04
#> 5   0.7255892 1.945774e-03
#> 6   0.6130482 1.141459e-03
#> 7   0.5335171 6.621256e-04
#> 8   0.5256700 8.869272e-05
#> 9   0.5420102 1.243016e-04
#> 10  0.5137342 8.867261e-05
#> 11 -0.3187187 1.420541e-04
#> 12 -0.2207153 5.767428e-04
#> 13 -0.2245028 2.526509e-04
#> 14 -0.2256118 3.671451e-04
#> 15 -0.2092240 1.100478e-03
assoc_measures(tab, type = "ordinal")
#>                 measure   estimate        se   ci_lower   ci_upper      p_value
#> 1 Goodman-Kruskal Gamma -0.6573705 0.1727847 -0.9960223 -0.3187187 0.0001420541
#> 2       Kendall's Tau-b -0.5125435 0.1488947 -0.8043717 -0.2207153 0.0005767428
#> 3        Stuart's Tau-c -0.4833984 0.1320920 -0.7422941 -0.2245028 0.0002526509
#> 4         Somers' D R|C -0.5015198 0.1407720 -0.7774278 -0.2256118 0.0003671451
#> 5         Somers' D C|R -0.5238095 0.1605058 -0.8383951 -0.2092240 0.0011004782
assoc_measures(tab, type = "all")
#>                              measure   estimate         se   ci_lower
#> 1                         Cramer's V  0.5308655         NA  0.2235534
#> 2            Contingency Coefficient  0.6003875         NA         NA
#> 3                   Lambda symmetric  0.4857143 0.12920157  0.2324839
#> 4                         Lambda R|C  0.5294118 0.15628498  0.2230988
#> 5                         Lambda C|R  0.4444444 0.14344383  0.1632997
#> 6            Goodman-Kruskal Tau R|C  0.3825603 0.11759802  0.1520724
#> 7            Goodman-Kruskal Tau C|R  0.3386018 0.09944838  0.1436866
#> 8  Uncertainty Coefficient symmetric  0.3504372 0.08940615  0.1752043
#> 9        Uncertainty Coefficient R|C  0.3587709 0.09349119  0.1755315
#> 10       Uncertainty Coefficient C|R  0.3424818 0.08737530  0.1712294
#> 11             Goodman-Kruskal Gamma -0.6573705 0.17278471 -0.9960223
#> 12                   Kendall's Tau-b -0.5125435 0.14889469 -0.8043717
#> 13                    Stuart's Tau-c -0.4833984 0.13209204 -0.7422941
#> 14                     Somers' D R|C -0.5015198 0.14077197 -0.7774278
#> 15                     Somers' D C|R -0.5238095 0.16050578 -0.8383951
#>      ci_upper      p_value
#> 1   0.7421756 1.214066e-03
#> 2          NA 1.214066e-03
#> 3   0.7389447 1.703534e-04
#> 4   0.8357247 7.053865e-04
#> 5   0.7255892 1.945774e-03
#> 6   0.6130482 1.141459e-03
#> 7   0.5335171 6.621256e-04
#> 8   0.5256700 8.869272e-05
#> 9   0.5420102 1.243016e-04
#> 10  0.5137342 8.867261e-05
#> 11 -0.3187187 1.420541e-04
#> 12 -0.2207153 5.767428e-04
#> 13 -0.2245028 2.526509e-04
#> 14 -0.2256118 3.671451e-04
#> 15 -0.2092240 1.100478e-03
```
