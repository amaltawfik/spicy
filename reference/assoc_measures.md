# Association measures summary table

`assoc_measures()` computes a range of association measures for a
two-way contingency table and returns them in a tidy data frame.

## Usage

``` r
assoc_measures(x, type = c("all", "nominal", "ordinal"), conf_level = 0.95)
```

## Arguments

- x:

  A contingency table (of class `table`).

- type:

  Which family of measures to compute: `"all"` (default), `"nominal"`,
  or `"ordinal"`.

- conf_level:

  A number between 0 and 1 giving the confidence level (default `0.95`).
  Set to `NULL` to omit the confidence interval.

## Value

A data frame with columns `measure`, `estimate`, `se`, `ci_lower`,
`ci_upper`, and `p_value`.

## Details

`type = "all"` (the default) returns all nominal and ordinal measures.
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

Standard error formulas follow the DescTools implementations (Signorell
et al., 2024).

## References

Agresti, A. (2002). *Categorical Data Analysis* (2nd ed.). Wiley.

Liebetrau, A. M. (1983). *Measures of Association*. Sage.

Signorell, A. et al. (2024). *DescTools: Tools for Descriptive
Statistics*. R package.

## See also

[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
[`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
[`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md)

## Examples

``` r
tab <- table(sochealth$smoking, sochealth$education)
assoc_measures(tab)
#>                              measure     estimate          se     ci_lower
#> 1                         Cramer's V  0.135667681          NA  0.079092643
#> 2            Contingency Coefficient  0.134436125          NA           NA
#> 3                   Lambda symmetric  0.000000000 0.000000000  0.000000000
#> 4                         Lambda R|C  0.000000000 0.000000000  0.000000000
#> 5                         Lambda C|R  0.000000000 0.000000000  0.000000000
#> 6          Goodman-Kruskal's Tau R|C  0.018405720 0.008085719  0.002558001
#> 7          Goodman-Kruskal's Tau C|R  0.007609460 0.003311779  0.001118492
#> 8  Uncertainty Coefficient symmetric  0.011487616 0.004981856  0.001723359
#> 9        Uncertainty Coefficient R|C  0.017512300 0.007583030  0.002649834
#> 10       Uncertainty Coefficient C|R  0.008547169 0.003712463  0.001270875
#> 11             Goodman-Kruskal Gamma -0.268067807 0.056281007 -0.378376554
#> 12                   Kendall's Tau-b -0.126415453 0.027412072 -0.180142125
#> 13                    Stuart's Tau-c -0.116920960 0.025716774 -0.167324910
#> 14                     Somers' D R|C -0.091306679 0.020030266 -0.130565280
#> 15                     Somers' D C|R -0.175024070 0.037817479 -0.249144966
#>       ci_upper      p_value
#> 1   0.19137158 2.012877e-05
#> 2           NA 2.012877e-05
#> 3   0.00000000           NA
#> 4   0.00000000           NA
#> 5   0.00000000           NA
#> 6   0.03425344 2.282660e-02
#> 7   0.01410043 2.157912e-02
#> 8   0.02125187 2.111672e-02
#> 9   0.03237477 2.092103e-02
#> 10  0.01582346 2.131878e-02
#> 11 -0.15775906 1.907128e-06
#> 12 -0.07268878 3.994450e-06
#> 13 -0.06651701 5.454892e-06
#> 14 -0.05204808 5.153607e-06
#> 15 -0.10090317 3.689888e-06
assoc_measures(tab, type = "nominal")
#>                              measure    estimate          se    ci_lower
#> 1                         Cramer's V 0.135667681          NA 0.079092643
#> 2            Contingency Coefficient 0.134436125          NA          NA
#> 3                   Lambda symmetric 0.000000000 0.000000000 0.000000000
#> 4                         Lambda R|C 0.000000000 0.000000000 0.000000000
#> 5                         Lambda C|R 0.000000000 0.000000000 0.000000000
#> 6          Goodman-Kruskal's Tau R|C 0.018405720 0.008085719 0.002558001
#> 7          Goodman-Kruskal's Tau C|R 0.007609460 0.003311779 0.001118492
#> 8  Uncertainty Coefficient symmetric 0.011487616 0.004981856 0.001723359
#> 9        Uncertainty Coefficient R|C 0.017512300 0.007583030 0.002649834
#> 10       Uncertainty Coefficient C|R 0.008547169 0.003712463 0.001270875
#>      ci_upper      p_value
#> 1  0.19137158 2.012877e-05
#> 2          NA 2.012877e-05
#> 3  0.00000000           NA
#> 4  0.00000000           NA
#> 5  0.00000000           NA
#> 6  0.03425344 2.282660e-02
#> 7  0.01410043 2.157912e-02
#> 8  0.02125187 2.111672e-02
#> 9  0.03237477 2.092103e-02
#> 10 0.01582346 2.131878e-02
assoc_measures(tab, type = "ordinal")
#>                 measure    estimate         se   ci_lower    ci_upper
#> 1 Goodman-Kruskal Gamma -0.26806781 0.05628101 -0.3783766 -0.15775906
#> 2       Kendall's Tau-b -0.12641545 0.02741207 -0.1801421 -0.07268878
#> 3        Stuart's Tau-c -0.11692096 0.02571677 -0.1673249 -0.06651701
#> 4         Somers' D R|C -0.09130668 0.02003027 -0.1305653 -0.05204808
#> 5         Somers' D C|R -0.17502407 0.03781748 -0.2491450 -0.10090317
#>        p_value
#> 1 1.907128e-06
#> 2 3.994450e-06
#> 3 5.454892e-06
#> 4 5.153607e-06
#> 5 3.689888e-06
```
