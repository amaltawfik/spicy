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
`ci_upper`, and `p_value`. For nominal measures (Cramer's V, Phi,
Contingency Coef.), the p-value comes from the Pearson chi-squared test
of independence. For all other measures, it is a Wald z-test of H0:
measure = 0.

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
#> Measure                            Estimate     SE  CI lower  CI upper        p 
#> Cramer's V                            0.136     --     0.079     0.191  < 0.001 
#> Contingency Coefficient               0.134     --        --        --  < 0.001 
#> Lambda symmetric                      0.000  0.000     0.000     0.000       -- 
#> Lambda R|C                            0.000  0.000     0.000     0.000       -- 
#> Lambda C|R                            0.000  0.000     0.000     0.000       -- 
#> Goodman-Kruskal's Tau R|C             0.018  0.008     0.003     0.034    0.023 
#> Goodman-Kruskal's Tau C|R             0.008  0.003     0.001     0.014    0.022 
#> Uncertainty Coefficient symmetric     0.011  0.005     0.002     0.021    0.021 
#> Uncertainty Coefficient R|C           0.018  0.008     0.003     0.032    0.021 
#> Uncertainty Coefficient C|R           0.009  0.004     0.001     0.016    0.021 
#> Goodman-Kruskal Gamma                -0.268  0.056    -0.378    -0.158  < 0.001 
#> Kendall's Tau-b                      -0.126  0.027    -0.180    -0.073  < 0.001 
#> Kendall's Tau-c                      -0.117  0.026    -0.167    -0.067  < 0.001 
#> Somers' D R|C                        -0.091  0.020    -0.131    -0.052  < 0.001 
#> Somers' D C|R                        -0.175  0.038    -0.249    -0.101  < 0.001 
assoc_measures(tab, type = "nominal")
#> Measure                            Estimate     SE  CI lower  CI upper        p 
#> Cramer's V                            0.136     --     0.079     0.191  < 0.001 
#> Contingency Coefficient               0.134     --        --        --  < 0.001 
#> Lambda symmetric                      0.000  0.000     0.000     0.000       -- 
#> Lambda R|C                            0.000  0.000     0.000     0.000       -- 
#> Lambda C|R                            0.000  0.000     0.000     0.000       -- 
#> Goodman-Kruskal's Tau R|C             0.018  0.008     0.003     0.034    0.023 
#> Goodman-Kruskal's Tau C|R             0.008  0.003     0.001     0.014    0.022 
#> Uncertainty Coefficient symmetric     0.011  0.005     0.002     0.021    0.021 
#> Uncertainty Coefficient R|C           0.018  0.008     0.003     0.032    0.021 
#> Uncertainty Coefficient C|R           0.009  0.004     0.001     0.016    0.021 
assoc_measures(tab, type = "ordinal")
#> Measure                Estimate     SE  CI lower  CI upper        p 
#> Goodman-Kruskal Gamma    -0.268  0.056    -0.378    -0.158  < 0.001 
#> Kendall's Tau-b          -0.126  0.027    -0.180    -0.073  < 0.001 
#> Kendall's Tau-c          -0.117  0.026    -0.167    -0.067  < 0.001 
#> Somers' D R|C            -0.091  0.020    -0.131    -0.052  < 0.001 
#> Somers' D C|R            -0.175  0.038    -0.249    -0.101  < 0.001 
```
