# Cramer's V, Phi, and association measures

``` r

library(spicy)
```

spicy provides a coherent set of effect size and association measures
for contingency tables, covering nominal and ordinal variables. This
vignette explains which measure to use depending on the measurement
level of your variables, and how to obtain confidence intervals and
p-values for chi-squared-based and rank-based statistics.

## Choosing the right measure

The table below summarizes the recommended measures by variable type.

| Variable types | Recommended measure | Function |
|----|----|----|
| Nominal x Nominal | Cramer’s V | [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md) |
| Nominal x Nominal | Contingency Coefficient | [`contingency_coef()`](https://amaltawfik.github.io/spicy/reference/contingency_coef.md) |
| Nominal x Nominal (2x2) | Phi | [`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md) |
| Ordinal x Ordinal | Kendall’s Tau-b | [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md) |
| Ordinal x Ordinal (rectangular) | Stuart’s Tau-c | [`kendall_tau_c()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_c.md) |
| Ordinal x Ordinal | Goodman-Kruskal Gamma | [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md) |
| Ordinal x Ordinal (asymmetric) | Somers’ D | [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md) |
| Nominal (asymmetric, PRE) | Lambda | [`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md) |
| Nominal (asymmetric, PRE) | Goodman-Kruskal Tau | [`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md) |
| Nominal (asymmetric, PRE) | Uncertainty Coefficient | [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md) |
| 2x2 table | Yule’s Q | [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md) |

PRE = Proportional Reduction in Error. These measures quantify how much
knowing one variable reduces prediction error for the other.

Cramer’s V and the contingency coefficient are defined for tables of any
dimension. For a nominal-by-ordinal pair they are the standard choice:
the ordering is simply ignored, because the rank-based measures require
both variables to be ordinal.

All functions accept a contingency table (class `table`, typically from
[`xtabs()`](https://rdrr.io/r/stats/xtabs.html) or
[`table()`](https://rdrr.io/r/base/table.html)).

## Quick overview with assoc_measures()

[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
computes all available measures at once:

``` r

tbl <- xtabs(~ smoking + education, data = sochealth)
assoc_measures(tbl)
#> Measure                            Estimate     SE  CI lower  CI upper      p 
#> Cramer's V                            0.136     --     0.079     0.191  <.001 
#> Contingency Coefficient               0.134     --        --        --  <.001 
#> Lambda symmetric                      0.000  0.000     0.000     0.000     -- 
#> Lambda R|C                            0.000  0.000     0.000     0.000     -- 
#> Lambda C|R                            0.000  0.000     0.000     0.000     -- 
#> Goodman-Kruskal's Tau R|C             0.018  0.008     0.003     0.034   .023 
#> Goodman-Kruskal's Tau C|R             0.008  0.003     0.001     0.014   .022 
#> Uncertainty Coefficient symmetric     0.011  0.005     0.002     0.021   .021 
#> Uncertainty Coefficient R|C           0.018  0.008     0.003     0.032   .021 
#> Uncertainty Coefficient C|R           0.009  0.004     0.001     0.016   .021 
#> Goodman-Kruskal Gamma                -0.268  0.056    -0.378    -0.158  <.001 
#> Kendall's Tau-b                      -0.126  0.027    -0.180    -0.073  <.001 
#> Stuart's Tau-c                       -0.117  0.026    -0.167    -0.067  <.001 
#> Somers' D R|C                        -0.091  0.020    -0.131    -0.052  <.001 
#> Somers' D C|R                        -0.175  0.038    -0.249    -0.101  <.001
```

Directional variants are labelled `R|C` (the row variable is treated as
dependent, i.e. predicted from the column variable) and `C|R` (the
reverse); `symmetric` variants single out no dependent variable.

[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
computes every measure the table dimensions allow, regardless of
measurement level: the ordinal rows above treat the factor level order
of `smoking` (No \< Yes) as a substantive ordering. This is useful for
exploratory analysis, but read only the rows that match your variable
types. For reporting, pick the measure that matches them.

## Nominal variables

Chi-squared-based measures ignore any ordering of the categories, which
makes them the reference choice when at least one of the two variables
is nominal.

### Cramer’s V

Cramer’s V measures the strength of association between two categorical
variables. It ranges from 0 (no association) to 1 (perfect association).

``` r

tbl <- xtabs(~ smoking + education, data = sochealth)
cramer_v(tbl)
#> [1] 0.1356677
```

In this pair, `smoking` is nominal and `education` is ordinal: Cramer’s
V treats `education` as an unordered set of categories, which is the
standard treatment for a nominal-by-ordinal pair. When *both* variables
are ordinal, the rank-based measures presented below use more of the
information in the data.

Pass `detail = TRUE` for the confidence interval and p-value. The
p-value tests the null hypothesis of no association using the Pearson
chi-squared test. The chi-squared-based measures (Cramer’s V, Phi, the
contingency coefficient) have no standard asymptotic standard error, so
the `SE` column prints `--` for them; the rank-based measures later in
this vignette report their asymptotic SE in that column.

``` r

cramer_v(tbl, detail = TRUE)
#> Estimate  SE  CI lower  CI upper      p
#>    0.136  --     0.079     0.191  <.001
```

### Phi coefficient

For 2x2 tables, Phi is equivalent to Cramer’s V. spicy implements Phi as
\\\sqrt{\chi^2 / n}\\, matching the DescTools and PSPP conventions – the
value is always non-negative and reports only the *strength* of
association, not its direction. SPSS itself signs Phi on 2x2 tables
(there it equals the Pearson correlation between the two variables coded
0/1), so SPSS output can show a negative value of the same magnitude. To
recover the signed direction, compute the Pearson correlation between
the two binary variables explicitly (e.g., `cor(x, y)` after coding both
0/1). The p-value tests H0: no association (Pearson chi-squared test).

``` r

tbl_22 <- xtabs(~ smoking + physical_activity, data = sochealth)
phi(tbl_22, detail = TRUE)
#> Estimate  SE  CI lower  CI upper     p
#>    0.006  --     0.000     0.063  .839
```

### Contingency coefficient

The contingency coefficient is an alternative to Cramer’s V. Its upper
bound depends on the table dimensions, which makes it harder to compare
across tables of different sizes. The p-value tests H0: no association
(Pearson chi-squared test).

``` r

contingency_coef(tbl, detail = TRUE)
#> Estimate  SE  CI lower  CI upper      p
#>    0.134  --        --        --  <.001
```

## Ordinal variables

When both variables are ordinal (ordered factors), measures that account
for the ordering are more appropriate than Cramer’s V. Note that these
measures are not on a common scale: on the health-by-education table
used throughout this section, Cramer’s V is 0.176, Tau-b is 0.205 and
Gamma is 0.310 for the very same association. Compare magnitudes within
a measure, never across measures.

### Goodman-Kruskal Gamma

Gamma ranges from -1 to +1. It ignores tied pairs, which tends to
overestimate the strength of association when there are many ties: on
any table, \|Gamma\| is at least as large as \|Tau-b\|. Like the other
measures built on the difference between concordant and discordant pairs
(Tau-b, Tau-c, Somers’ D), its sign carries the direction of
association.

``` r

tbl_ord <- xtabs(~ self_rated_health + education, data = sochealth)
gamma_gk(tbl_ord, detail = TRUE)
#> Estimate     SE  CI lower  CI upper      p
#>    0.310  0.037     0.238     0.383  <.001
```

A positive value means that higher values on one variable tend to occur
with higher values on the other. The p-value tests H0: Gamma = 0 using a
Wald z-test.

### Kendall’s Tau-b

Tau-b adjusts for ties and ranges from -1 to +1. It is generally
preferred over Gamma for square or near-square tables. The p-value tests
H0: Tau-b = 0 (Wald z-test).

``` r

kendall_tau_b(tbl_ord, detail = TRUE)
#> Estimate     SE  CI lower  CI upper      p
#>    0.205  0.025     0.155     0.254  <.001
```

### Stuart’s Tau-c

Stuart’s Tau-c – the label spicy shares with SAS, after Stuart (1953) –
is also known as Kendall’s Tau-c, the name SPSS and PSPP print for the
same statistic. It is similar to Tau-b but adjusts for rectangular
tables where the number of rows and columns differ. The p-value tests
H0: Tau-c = 0 (Wald z-test).

``` r

kendall_tau_c(tbl_ord, detail = TRUE)
#> Estimate     SE  CI lower  CI upper      p
#>    0.200  0.025     0.151     0.248  <.001
```

### Somers’ D

Somers’ D is an asymmetric measure: it distinguishes between a dependent
and an independent variable. By default, the row variable is treated as
dependent (D(R\|C)). The p-value tests H0: D = 0 (Wald z-test). One
disclosure worth knowing: the association line printed below the table
by
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
reports the *symmetric* variant of Somers’ D, while the standalone
[`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md)
defaults to the row-dependent D(R\|C) — the two values differ by
construction.

``` r

somers_d(tbl_ord, detail = TRUE)
#> Estimate     SE  CI lower  CI upper      p
#>    0.208  0.026     0.157     0.258  <.001
```

### How the p-values compare with SPSS and PSPP

spicy’s Wald z-tests divide each estimate by the displayed asymptotic
standard error, which is estimated *without* assuming independence. SPSS
and PSPP test the ordinal measures differently: they use the standard
error computed *under* the independence hypothesis (which is why their
“Approx. T” column shows the same value for Gamma, Tau-b, Tau-c and
Somers’ D), and SPSS refers Goodman-Kruskal’s Tau to a chi-squared
distribution rather than a z-test. Estimates and standard errors match
SPSS and PSPP; the p-values can differ, especially near the null. For
the non-negative PRE measures below (Lambda, Goodman-Kruskal’s Tau, the
uncertainty coefficient) the Wald z-test is approximate in a further
way: H0 places the parameter on the boundary of its range, so read those
p-values as indicative rather than exact.

## Asymmetric (PRE) measures

These measures answer a specific question: how much does knowing the
column variable reduce our error in predicting the row variable (or vice
versa)? Each function takes a `direction` argument.
[`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md)
and
[`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md)
default to `direction = "symmetric"`, which combines the two prediction
directions rather than singling out a dependent variable;
[`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md)
defaults to `direction = "row"` (the column variable predicts the row
variable). Pass `direction = "row"` or `direction = "column"` explicitly
when your question is directional.

### Lambda

Lambda measures the proportional reduction in classification error. The
default is the symmetric variant; here we also show the row-dependent
variant, where education (the column variable) predicts self-rated
health (the row variable):

``` r

lambda_gk(tbl_ord, detail = TRUE)
#> Estimate     SE  CI lower  CI upper     p
#>    0.012  0.014     0.000     0.039  .389
lambda_gk(tbl_ord, direction = "row", detail = TRUE)
#> Estimate     SE  CI lower  CI upper   p
#>    0.000  0.000     0.000     0.000  --
```

The row-dependent result illustrates a well-known caveat: Lambda can
equal zero even when the variables are associated, if the modal category
of the dependent variable does not change across the categories of the
predictor. That is exactly what happens here – “Good” is the modal
health category at every education level, so the row-dependent Lambda is
exactly 0 even though the same table shows a clear association (Cramer’s
V = 0.176, chi-squared p \< .001). When the estimate is zero its
asymptotic SE is zero as well, and no test is printed (`--`). Otherwise
the p-value tests H0: Lambda = 0 (Wald z-test).

### Goodman-Kruskal Tau

Tau measures the proportional reduction in error when predicting the row
variable from the column variable, using the full distribution (not just
the mode). This matches its default `direction = "row"`; pass
`direction = "column"` for the reverse prediction. The p-value tests H0:
Tau = 0 (Wald z-test).

``` r

goodman_kruskal_tau(tbl_ord, detail = TRUE)
#> Estimate     SE  CI lower  CI upper      p
#>    0.017  0.005     0.008     0.026  <.001
```

### Uncertainty coefficient

The uncertainty coefficient (Theil’s U) is based on entropy. It measures
how much knowing one variable reduces uncertainty about the other. By
default it reports the symmetric coefficient; pass `direction = "row"`
or `direction = "column"` for the asymmetric variants. The p-value tests
H0: U = 0 (Wald z-test).

``` r

uncertainty_coef(tbl_ord, detail = TRUE)
#> Estimate     SE  CI lower  CI upper      p
#>    0.028  0.006     0.016     0.040  <.001
```

## Yule’s Q

Yule’s Q is defined for 2x2 tables only. It ranges from -1 to +1 and is
equivalent to Gamma for 2x2 tables. The p-value tests H0: Q = 0 (Wald
z-test).

``` r

tbl_22 <- xtabs(~ smoking + physical_activity, data = sochealth)
yule_q(tbl_22, detail = TRUE)
#> Estimate     SE  CI lower  CI upper     p
#>    0.015  0.072    -0.126     0.155  .839
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
#>  Values   │   Lower secondary    Upper secondary    Tertiary │   Total 
#> ──────────┼──────────────────────────────────────────────────┼─────────
#>  No       │               179                415         332 │     926 
#>  Yes      │                78                112          59 │     249 
#> ──────────┼──────────────────────────────────────────────────┼─────────
#>  Total    │               257                527         391 │    1175 
#> 
#> Chi-2(2) = 21.6, p <.001
#> Cramer's V = 0.14
#> Missing values removed: smoking (25).

# Ordinal: Kendall's Tau-b (automatic)
cross_tab(sochealth, self_rated_health, education)
#> Crosstable: self_rated_health x education (N)
#> 
#>  Values      │   Lower secondary    Upper secondary    Tertiary │   Total 
#> ─────────────┼──────────────────────────────────────────────────┼─────────
#>  Poor        │                28                 28           5 │      61 
#>  Fair        │                86                118          62 │     266 
#>  Good        │               102                263         193 │     558 
#>  Very good   │                44                118         133 │     295 
#> ─────────────┼──────────────────────────────────────────────────┼─────────
#>  Total       │               260                527         393 │    1180 
#> 
#> Chi-2(6) = 73.2, p <.001
#> Kendall's Tau-b = 0.20
#> Missing values removed: self_rated_health (20).
```

You can override the automatic choice:

``` r

cross_tab(sochealth, self_rated_health, education, assoc_measure = "gamma")
#> Crosstable: self_rated_health x education (N)
#> 
#>  Values      │   Lower secondary    Upper secondary    Tertiary │   Total 
#> ─────────────┼──────────────────────────────────────────────────┼─────────
#>  Poor        │                28                 28           5 │      61 
#>  Fair        │                86                118          62 │     266 
#>  Good        │               102                263         193 │     558 
#>  Very good   │                44                118         133 │     295 
#> ─────────────┼──────────────────────────────────────────────────┼─────────
#>  Total       │               260                527         393 │    1180 
#> 
#> Chi-2(6) = 73.2, p <.001
#> Goodman-Kruskal Gamma = 0.31
#> Missing values removed: self_rated_health (20).
```

## Confidence intervals

Most functions report a confidence interval via `detail = TRUE`, with
two exceptions:
[`contingency_coef()`](https://amaltawfik.github.io/spicy/reference/contingency_coef.md)
never reports one (no standard asymptotic SE exists for the contingency
coefficient), and `somers_d(direction = "symmetric")` reports the
estimate only. Cramer’s V and Phi also omit the interval silently in
degenerate cases (estimate exactly 0, or n \<= 3). The confidence level
defaults to 95% and can be changed with `conf_level` – here both levels
on the smoking-by-education table from the beginning of the vignette:

``` r

cramer_v(tbl, detail = TRUE)
#> Estimate  SE  CI lower  CI upper      p
#>    0.136  --     0.079     0.191  <.001
cramer_v(tbl, detail = TRUE, conf_level = 0.99)
#> Estimate  SE  CI lower  CI upper      p
#>    0.136  --     0.061     0.209  <.001
```

To drop the confidence interval, pass `conf_level = NULL`; the result
keeps the estimate, the SE column, and the p-value:

``` r

cramer_v(tbl, detail = TRUE, conf_level = NULL)
#> Estimate  SE      p
#>    0.136  --  <.001
```

## Controlling decimal places

When `detail = FALSE` (the default), functions return a plain numeric
scalar, so R’s own formatting rules apply. When `detail = TRUE`, the
result uses a custom print method that defaults to 3 decimal places.
Pass `digits` to change the precision of the estimate, SE, and CI
columns. The p-value follows APA-style formatting independent of
`digits`: 3 decimal places with the leading zero stripped (`.045`) or
`<.001` below the threshold:

``` r

cramer_v(tbl, detail = TRUE, digits = 4)
#> Estimate  SE  CI lower  CI upper      p
#>   0.1357  --    0.0791    0.1914  <.001
```

The same `digits` argument works for
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md):

``` r

assoc_measures(tbl, digits = 2)
#> Measure                            Estimate    SE  CI lower  CI upper      p 
#> Cramer's V                             0.14    --      0.08      0.19  <.001 
#> Contingency Coefficient                0.13    --        --        --  <.001 
#> Lambda symmetric                       0.00  0.00      0.00      0.00     -- 
#> Lambda R|C                             0.00  0.00      0.00      0.00     -- 
#> Lambda C|R                             0.00  0.00      0.00      0.00     -- 
#> Goodman-Kruskal's Tau R|C              0.02  0.01      0.00      0.03   .023 
#> Goodman-Kruskal's Tau C|R              0.01  0.00      0.00      0.01   .022 
#> Uncertainty Coefficient symmetric      0.01  0.00      0.00      0.02   .021 
#> Uncertainty Coefficient R|C            0.02  0.01      0.00      0.03   .021 
#> Uncertainty Coefficient C|R            0.01  0.00      0.00      0.02   .021 
#> Goodman-Kruskal Gamma                 -0.27  0.06     -0.38     -0.16  <.001 
#> Kendall's Tau-b                       -0.13  0.03     -0.18     -0.07  <.001 
#> Stuart's Tau-c                        -0.12  0.03     -0.17     -0.07  <.001 
#> Somers' D R|C                         -0.09  0.02     -0.13     -0.05  <.001 
#> Somers' D C|R                         -0.18  0.04     -0.25     -0.10  <.001
```

You can also store a result and re-display it with a different precision
without recalculating:

``` r

res <- cramer_v(tbl, detail = TRUE)
print(res, digits = 5)
#> Estimate  SE  CI lower  CI upper      p
#>  0.13567  --   0.07909   0.19137  <.001
```

## See also

- See
  [`vignette("frequency-tables")`](https://amaltawfik.github.io/spicy/articles/frequency-tables.md)
  for
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
  which reports these measures inline through its `assoc_measure`
  argument.
- See
  [`vignette("table-categorical")`](https://amaltawfik.github.io/spicy/articles/table-categorical.md)
  for association measures inside APA-style categorical summary tables.

## References

- Agresti, A. (2002). *Categorical Data Analysis* (2nd ed.). Wiley.
- Brown, M. B., & Benedetti, J. K. (1977). Sampling behavior of tests
  for correlation in two-way contingency tables. *Journal of the
  American Statistical Association*, 72(358), 309–315.
- Goodman, L. A., & Kruskal, W. H. (1954). Measures of association for
  cross classifications. *Journal of the American Statistical
  Association*, 49(268), 732–764.
- Kendall, M. G. (1938). A new measure of rank correlation.
  *Biometrika*, 30(1–2), 81–93.
- Liebetrau, A. M. (1983). *Measures of Association*. Sage.
- Somers, R. H. (1962). A new asymmetric measure of association for
  ordinal variables. *American Sociological Review*, 27(6), 799–811.
- Stuart, A. (1953). The estimation and comparison of strengths of
  association in contingency tables. *Biometrika*, 40(1–2), 105–110.
- Theil, H. (1970). On the estimation of relationships involving
  qualitative variables. *American Journal of Sociology*, 76(1),
  103–154.
- Yule, G. U. (1900). On the association of attributes in statistics.
  *Philosophical Transactions of the Royal Society of London, Series A*,
  194, 257–319.
