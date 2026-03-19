# Association measures for contingency tables

``` r
library(spicy)
```

spicy provides a full suite of association measures for contingency
tables. This vignette explains which measure to use depending on the
measurement level of your variables, and how to obtain confidence
intervals and p-values.

## Choosing the right measure

The table below summarizes the recommended measures by variable type.

| Variable types                 | Recommended measure     | Function                                                                                       |
|--------------------------------|-------------------------|------------------------------------------------------------------------------------------------|
| Nominal x Nominal              | Cramer’s V              | [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)                       |
| Nominal x Nominal (2x2)        | Phi                     | [`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md)                                 |
| Ordinal x Ordinal              | Kendall’s Tau-b         | [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md)             |
| Ordinal x Ordinal              | Goodman-Kruskal Gamma   | [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md)                       |
| Ordinal x Ordinal (asymmetric) | Somers’ D               | [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md)                       |
| Nominal (asymmetric, PRE)      | Lambda                  | [`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md)                     |
| Nominal (asymmetric, PRE)      | Goodman-Kruskal Tau     | [`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md) |
| Nominal (asymmetric, PRE)      | Uncertainty Coefficient | [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md)       |
| 2x2 table                      | Yule’s Q                | [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md)                           |

PRE = Proportional Reduction in Error. These measures quantify how much
knowing one variable reduces prediction error for the other.

All functions accept a contingency table (class `table`, typically from
[`xtabs()`](https://rdrr.io/r/stats/xtabs.html) or
[`table()`](https://rdrr.io/r/base/table.html)).

## Quick overview with assoc_measures()

[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
computes all available measures at once:

``` r
tbl <- xtabs(~ smoking + education, data = sochealth)
assoc_measures(tbl)
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
```

This is useful for exploratory analysis. For reporting, pick the measure
that matches your variable types.

## Nominal variables

### Cramer’s V

Cramer’s V measures the strength of association between two nominal
variables. It ranges from 0 (no association) to 1 (perfect association).

``` r
tbl <- xtabs(~ smoking + education, data = sochealth)
cramer_v(tbl)
#> [1] 0.1356677
```

Pass `detail = TRUE` for the confidence interval and p-value. The
p-value tests the null hypothesis of no association using the Pearson
chi-squared test.

``` r
cramer_v(tbl, detail = TRUE)
#>     estimate     ci_lower     ci_upper      p_value 
#> 1.356677e-01 7.909264e-02 1.913716e-01 2.012877e-05
```

### Phi coefficient

For 2x2 tables, Phi is equivalent to Cramer’s V. Unlike V, Phi can be
negative when the table is 2x2, indicating the direction of association.

``` r
tbl_22 <- xtabs(~ smoking + physical_activity, data = sochealth)
phi(tbl_22, detail = TRUE)
#>    estimate    ci_lower    ci_upper     p_value 
#> 0.005910601 0.000000000 0.063077970 0.839443630
```

### Contingency coefficient

The contingency coefficient is an alternative to Cramer’s V. Its upper
bound depends on the table dimensions, which makes it harder to compare
across tables of different sizes.

``` r
contingency_coef(tbl, detail = TRUE)
#>     estimate     ci_lower     ci_upper      p_value 
#> 1.344361e-01           NA           NA 2.012877e-05
```

## Ordinal variables

When both variables are ordinal (ordered factors), measures that account
for the ordering are more appropriate than Cramer’s V.

### Goodman-Kruskal Gamma

Gamma ranges from -1 to +1. It ignores tied pairs, which makes it
sensitive to the direction of association but tends to overestimate
strength when there are many ties.

``` r
tbl_ord <- xtabs(~ self_rated_health + education, data = sochealth)
gamma_gk(tbl_ord, detail = TRUE)
#>     estimate     ci_lower     ci_upper      p_value 
#> 3.104791e-01 2.376258e-01 3.833324e-01 6.667393e-17
```

A positive value means that higher values on one variable tend to occur
with higher values on the other. The p-value tests H0: Gamma = 0 using a
Wald z-test.

### Kendall’s Tau-b

Tau-b adjusts for ties and ranges from -1 to +1. It is generally
preferred over Gamma for square or near-square tables.

``` r
kendall_tau_b(tbl_ord, detail = TRUE)
#>     estimate     ci_lower     ci_upper      p_value 
#> 2.045524e-01 1.552195e-01 2.538854e-01 4.410418e-16
```

### Stuart’s Tau-c

Tau-c is similar to Tau-b but adjusts for rectangular tables where the
number of rows and columns differ.

``` r
kendall_tau_c(tbl_ord, detail = TRUE)
#>     estimate     ci_lower     ci_upper      p_value 
#> 1.996409e-01 1.509952e-01 2.482866e-01 8.720015e-16
```

### Somers’ D

Somers’ D is an asymmetric measure: it distinguishes between a dependent
and an independent variable. By default, the row variable is treated as
dependent (D(R\|C)).

``` r
somers_d(tbl_ord, detail = TRUE)
#>     estimate     ci_lower     ci_upper      p_value 
#> 2.076131e-01 1.573452e-01 2.578810e-01 5.730469e-16
```

## Asymmetric (PRE) measures

These measures answer a specific question: how much does knowing the
column variable reduce our error in predicting the row variable (or vice
versa)?

### Lambda

Lambda measures the proportional reduction in classification error. It
can equal zero even when the variables are associated, if the modal
category does not change across columns.

``` r
tbl <- xtabs(~ employment_status + education, data = sochealth)
lambda_gk(tbl, detail = TRUE)
#> estimate ci_lower ci_upper  p_value 
#>        0        0        0       NA
```

### Goodman-Kruskal Tau

Tau measures the proportional reduction in error when predicting the row
variable from the column variable, using the full distribution (not just
the mode).

``` r
goodman_kruskal_tau(tbl, detail = TRUE)
#>    estimate    ci_lower    ci_upper     p_value 
#> 0.011754531 0.002910365 0.020598697 0.009189229
```

### Uncertainty coefficient

The uncertainty coefficient (Theil’s U) is based on entropy. It measures
how much knowing one variable reduces uncertainty about the other.

``` r
uncertainty_coef(tbl, detail = TRUE)
#>    estimate    ci_lower    ci_upper     p_value 
#> 0.014340586 0.004867184 0.023813987 0.003007778
```

## Yule’s Q

Yule’s Q is defined for 2x2 tables only. It ranges from -1 to +1 and is
equivalent to Gamma for 2x2 tables.

``` r
tbl_22 <- xtabs(~ smoking + physical_activity, data = sochealth)
yule_q(tbl_22, detail = TRUE)
#>    estimate    ci_lower    ci_upper     p_value 
#>  0.01450794 -0.12582192  0.15483780  0.83942419
```

## Automatic selection in cross_tab()

[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
can automatically select an appropriate measure via
`assoc_measure = "auto"` (the default). When both variables are ordered
factors, it picks Kendall’s Tau-b; otherwise it uses Cramer’s V.

``` r
# Nominal: Cramer's V
cross_tab(sochealth, smoking, education)
#> Crosstable: smoking x education (N)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                  179                   415            332 │        926 
#>  Yes         │                   78                   112             59 │        249 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                  257                   527            391 │       1175 
#> 
#> Chi-2(2) = 21.6, p < 0.001
#> Cramer's V = 0.14

# Ordinal: Kendall's Tau-b (automatic)
cross_tab(sochealth, self_rated_health, education)
#> Crosstable: self_rated_health x education (N)
#> 
#>  Values         │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ────────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Poor           │                   28                    28              5 │         61 
#>  Fair           │                   86                   118             62 │        266 
#>  Good           │                  102                   263            193 │        558 
#>  Very good      │                   44                   118            133 │        295 
#> ────────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total          │                  260                   527            393 │       1180 
#> 
#> Chi-2(6) = 73.2, p < 0.001
#> Kendall's Tau-b = 0.20
```

You can override the automatic choice:

``` r
cross_tab(sochealth, self_rated_health, education, assoc_measure = "gamma")
#> Crosstable: self_rated_health x education (N)
#> 
#>  Values         │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ────────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Poor           │                   28                    28              5 │         61 
#>  Fair           │                   86                   118             62 │        266 
#>  Good           │                  102                   263            193 │        558 
#>  Very good      │                   44                   118            133 │        295 
#> ────────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total          │                  260                   527            393 │       1180 
#> 
#> Chi-2(6) = 73.2, p < 0.001
#> Goodman-Kruskal Gamma = 0.31
```

## Confidence intervals

All functions support confidence intervals via `detail = TRUE`. The
confidence level defaults to 95% and can be changed with `conf_level`:

``` r
cramer_v(tbl, detail = TRUE, conf_level = 0.99)
#>     estimate     ci_lower     ci_upper      p_value 
#> 1.269149e-01 5.310210e-02 1.993484e-01 8.352614e-07
```

To get only the estimate and p-value (no CI), pass `conf_level = NULL`:

``` r
cramer_v(tbl, detail = TRUE, conf_level = NULL)
#>     estimate      p_value 
#> 1.269149e-01 8.352614e-07
```
