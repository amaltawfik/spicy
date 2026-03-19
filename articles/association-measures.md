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
#>       ci_upper        p_value
#> 1   0.19137158 0.000020128774
#> 2           NA 0.000020128774
#> 3   0.00000000             NA
#> 4   0.00000000             NA
#> 5   0.00000000             NA
#> 6   0.03425344 0.022826600446
#> 7   0.01410043 0.021579122521
#> 8   0.02125187 0.021116720521
#> 9   0.03237477 0.020921026002
#> 10  0.01582346 0.021318784966
#> 11 -0.15775906 0.000001907128
#> 12 -0.07268878 0.000003994450
#> 13 -0.06651701 0.000005454892
#> 14 -0.05204808 0.000005153607
#> 15 -0.10090317 0.000003689888
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
#>      estimate      ci_lower      ci_upper       p_value 
#> 0.13566768051 0.07909264263 0.19137158459 0.00002012877
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
#>      estimate      ci_lower      ci_upper       p_value 
#> 0.13443612479            NA            NA 0.00002012877
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
#>                  estimate                  ci_lower                  ci_upper 
#> 0.31047908806401242065576 0.23762577188158526264594 0.38333240424643955091000 
#>                   p_value 
#> 0.00000000000000006667393
```

A positive value means that higher values on one variable tend to occur
with higher values on the other. The p-value tests H0: Gamma = 0 using a
Wald z-test.

### Kendall’s Tau-b

Tau-b adjusts for ties and ranges from -1 to +1. It is generally
preferred over Gamma for square or near-square tables.

``` r
kendall_tau_b(tbl_ord, detail = TRUE)
#>                 estimate                 ci_lower                 ci_upper 
#> 0.2045524107732165364215 0.1552194601769311121942 0.2538853613695019606489 
#>                  p_value 
#> 0.0000000000000004410418
```

### Stuart’s Tau-c

Tau-c is similar to Tau-b but adjusts for rectangular tables where the
number of rows and columns differ.

``` r
kendall_tau_c(tbl_ord, detail = TRUE)
#>                 estimate                 ci_lower                 ci_upper 
#> 0.1996409077851192259700 0.1509952174139821412524 0.2482865981562563106877 
#>                  p_value 
#> 0.0000000000000008720015
```

### Somers’ D

Somers’ D is an asymmetric measure: it distinguishes between a dependent
and an independent variable. By default, the row variable is treated as
dependent (D(R\|C)).

``` r
somers_d(tbl_ord, detail = TRUE)
#>                 estimate                 ci_lower                 ci_upper 
#> 0.2076130769799534314668 0.1573452002361157842092 0.2578809537237911064800 
#>                  p_value 
#> 0.0000000000000005730469
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
#>        estimate        ci_lower        ci_upper         p_value 
#> 0.1269149168801 0.0531020955399 0.1993484144163 0.0000008352614
```

To get only the estimate and p-value (no CI), pass `conf_level = NULL`:

``` r
cramer_v(tbl, detail = TRUE, conf_level = NULL)
#>        estimate         p_value 
#> 0.1269149168801 0.0000008352614
```
