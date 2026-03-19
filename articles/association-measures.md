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
#>                              measure estimate      se ci_lower ci_upper
#> 1                         Cramer's V  0.13567      NA  0.07909   0.1914
#> 2            Contingency Coefficient  0.13444      NA       NA       NA
#> 3                   Lambda symmetric  0.00000 0.00000  0.00000   0.0000
#> 4                         Lambda R|C  0.00000 0.00000  0.00000   0.0000
#> 5                         Lambda C|R  0.00000 0.00000  0.00000   0.0000
#> 6          Goodman-Kruskal's Tau R|C  0.01841 0.00809  0.00256   0.0343
#> 7          Goodman-Kruskal's Tau C|R  0.00761 0.00331  0.00112   0.0141
#> 8  Uncertainty Coefficient symmetric  0.01149 0.00498  0.00172   0.0213
#> 9        Uncertainty Coefficient R|C  0.01751 0.00758  0.00265   0.0324
#> 10       Uncertainty Coefficient C|R  0.00855 0.00371  0.00127   0.0158
#> 11             Goodman-Kruskal Gamma -0.26807 0.05628 -0.37838  -0.1578
#> 12                   Kendall's Tau-b -0.12642 0.02741 -0.18014  -0.0727
#> 13                    Stuart's Tau-c -0.11692 0.02572 -0.16732  -0.0665
#> 14                     Somers' D R|C -0.09131 0.02003 -0.13057  -0.0520
#> 15                     Somers' D C|R -0.17502 0.03782 -0.24914  -0.1009
#>       p_value
#> 1  0.00002013
#> 2  0.00002013
#> 3          NA
#> 4          NA
#> 5          NA
#> 6  0.02282660
#> 7  0.02157912
#> 8  0.02111672
#> 9  0.02092103
#> 10 0.02131878
#> 11 0.00000191
#> 12 0.00000399
#> 13 0.00000545
#> 14 0.00000515
#> 15 0.00000369
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
#> [1] 0.136
```

Pass `detail = TRUE` for the confidence interval and p-value. The
p-value tests the null hypothesis of no association using the Pearson
chi-squared test.

``` r
cramer_v(tbl, detail = TRUE)
#>  estimate  ci_lower  ci_upper   p_value 
#> 0.1356677 0.0790926 0.1913716 0.0000201
```

### Phi coefficient

For 2x2 tables, Phi is equivalent to Cramer’s V. Unlike V, Phi can be
negative when the table is 2x2, indicating the direction of association.

``` r
tbl_22 <- xtabs(~ smoking + physical_activity, data = sochealth)
phi(tbl_22, detail = TRUE)
#> estimate ci_lower ci_upper  p_value 
#>  0.00591  0.00000  0.06308  0.83944
```

### Contingency coefficient

The contingency coefficient is an alternative to Cramer’s V. Its upper
bound depends on the table dimensions, which makes it harder to compare
across tables of different sizes.

``` r
contingency_coef(tbl, detail = TRUE)
#>  estimate  ci_lower  ci_upper   p_value 
#> 0.1344361        NA        NA 0.0000201
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
#>              estimate              ci_lower              ci_upper 
#> 0.3104790880640124207 0.2376257718815852626 0.3833324042464395509 
#>               p_value 
#> 0.0000000000000000667
```

A positive value means that higher values on one variable tend to occur
with higher values on the other. The p-value tests H0: Gamma = 0 using a
Wald z-test.

### Kendall’s Tau-b

Tau-b adjusts for ties and ranges from -1 to +1. It is generally
preferred over Gamma for square or near-square tables.

``` r
kendall_tau_b(tbl_ord, detail = TRUE)
#>             estimate             ci_lower             ci_upper 
#> 0.204552410773216536 0.155219460176931112 0.253885361369501961 
#>              p_value 
#> 0.000000000000000441
```

### Kendall’s Tau-c (Stuart’s Tau-c)

Tau-c is similar to Tau-b but adjusts for rectangular tables where the
number of rows and columns differ.

``` r
kendall_tau_c(tbl_ord, detail = TRUE)
#>             estimate             ci_lower             ci_upper 
#> 0.199640907785119226 0.150995217413982141 0.248286598156256311 
#>              p_value 
#> 0.000000000000000872
```

### Somers’ D

Somers’ D is an asymmetric measure: it distinguishes between a dependent
and an independent variable. By default, the row variable is treated as
dependent (D(R\|C)).

``` r
somers_d(tbl_ord, detail = TRUE)
#>             estimate             ci_lower             ci_upper 
#> 0.207613076979953431 0.157345200236115784 0.257880953723791106 
#>              p_value 
#> 0.000000000000000573
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
#> estimate ci_lower ci_upper  p_value 
#>  0.01175  0.00291  0.02060  0.00919
```

### Uncertainty coefficient

The uncertainty coefficient (Theil’s U) is based on entropy. It measures
how much knowing one variable reduces uncertainty about the other.

``` r
uncertainty_coef(tbl, detail = TRUE)
#> estimate ci_lower ci_upper  p_value 
#>  0.01434  0.00487  0.02381  0.00301
```

## Yule’s Q

Yule’s Q is defined for 2x2 tables only. It ranges from -1 to +1 and is
equivalent to Gamma for 2x2 tables.

``` r
tbl_22 <- xtabs(~ smoking + physical_activity, data = sochealth)
yule_q(tbl_22, detail = TRUE)
#> estimate ci_lower ci_upper  p_value 
#>   0.0145  -0.1258   0.1548   0.8394
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
#>    estimate    ci_lower    ci_upper     p_value 
#> 0.126914917 0.053102096 0.199348414 0.000000835
```

To get only the estimate and p-value (no CI), pass `conf_level = NULL`:

``` r
cramer_v(tbl, detail = TRUE, conf_level = NULL)
#>    estimate     p_value 
#> 0.126914917 0.000000835
```
