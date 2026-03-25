# Getting started with spicy

``` r
library(spicy)
```

spicy is an R package for descriptive statistics and data analysis,
designed for data science and survey research workflows. It covers
variable inspection, frequency tables, cross-tabulations with
chi-squared tests and effect sizes, and publication-ready summary
tables, offering functionality similar to Stata or SPSS but within a
tidyverse-friendly R environment. This vignette walks through the core
workflow using the bundled `sochealth` dataset, a simulated
social-health survey with 1 200 respondents and 24 variables.

## Inspect your data

[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
(or its shortcut
[`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md)) gives
a compact overview of every variable in a data frame: name, label,
representative values, class, number of distinct values, valid
observations, and missing values. In RStudio or Positron, calling
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
without arguments opens an interactive viewer - this is the most common
usage in practice. Here we use `tbl = TRUE` to produce static output for
the vignette:

``` r
varlist(sochealth, tbl = TRUE)
#> # A tibble: 24 × 7
#>    Variable          Label                 Values Class N_distinct N_valid   NAs
#>    <chr>             <chr>                 <chr>  <chr>      <int>   <int> <int>
#>  1 sex               Sex                   Femal… fact…          2    1200     0
#>  2 age               Age (years)           25, 2… nume…         51    1200     0
#>  3 age_group         Age group             25-34… orde…          4    1200     0
#>  4 education         Highest education le… Lower… orde…          3    1200     0
#>  5 social_class      Subjective social cl… Lower… orde…          5    1200     0
#>  6 region            Region of residence   Centr… fact…          6    1200     0
#>  7 employment_status Employment status     Emplo… fact…          4    1200     0
#>  8 income_group      Household income gro… Low, … orde…          4    1182    18
#>  9 income            Monthly household in… 1000,… nume…       1052    1200     0
#> 10 smoking           Current smoker        No, Y… fact…          2    1175    25
#> # ℹ 14 more rows
```

You can also select specific columns with tidyselect syntax:

``` r
varlist(sochealth, starts_with("bmi"), income, weight, tbl = TRUE)
#> # A tibble: 4 × 7
#>   Variable     Label                       Values Class N_distinct N_valid   NAs
#>   <chr>        <chr>                       <chr>  <chr>      <int>   <int> <int>
#> 1 bmi          Body mass index             16, 1… nume…        177    1188    12
#> 2 bmi_category BMI category                Norma… orde…          3    1188    12
#> 3 income       Monthly household income (… 1000,… nume…       1052    1200     0
#> 4 weight       Survey design weight        0.294… nume…        794    1200     0
```

## Frequency tables

[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
produces frequency tables with counts, percentages, and (optionally)
valid and cumulative percentages.

``` r
freq(sochealth, education)
#> Frequency table: education
#> 
#>  Category │ Values           Freq.  Percent 
#> ──────────┼─────────────────────────────────
#>  Valid    │ Lower secondary    261     21.8 
#>           │ Upper secondary    539     44.9 
#>           │ Tertiary           400     33.3 
#> ──────────┼─────────────────────────────────
#>  Total    │                   1200    100.0 
#> 
#> Label: Highest education level
#> Class: ordered, factor
#> Data: sochealth
```

Weighted frequencies use the `weights` argument. With `rescale = TRUE`,
the total weighted N matches the unweighted N:

``` r
freq(sochealth, education, weights = weight, rescale = TRUE)
#> Frequency table: education
#> 
#>  Category │ Values            Freq.  Percent 
#> ──────────┼──────────────────────────────────
#>  Valid    │ Lower secondary  258.62     21.6 
#>           │ Upper secondary  546.40     45.5 
#>           │ Tertiary         394.99     32.9 
#> ──────────┼──────────────────────────────────
#>  Total    │                    1200    100.0 
#> 
#> Label: Highest education level
#> Class: ordered, factor
#> Data: sochealth
#> Weight: weight (rescaled)
```

## Cross-tabulations

[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
crosses two categorical variables. By default it shows counts, a
chi-squared test, and Cramer’s V:

``` r
cross_tab(sochealth, smoking, education)
#> Crosstable: smoking x education (N)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                  179                   415            332 
#>  Yes         │                   78                   112             59 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  257                   527            391 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        926 
#>  Yes         │        249 
#> ─────────────┼────────────
#>  Total       │       1175 
#> 
#> Chi-2(2) = 21.6, p < 0.001
#> Cramer's V = 0.14
```

Add percentages with `percent`:

``` r
cross_tab(sochealth, smoking, education, percent = "col")
#> Crosstable: smoking x education (Column %)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                 69.6                  78.7           84.9 
#>  Yes         │                 30.4                  21.3           15.1 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                100.0                 100.0          100.0 
#>  N           │                  257                   527            391 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │       78.8 
#>  Yes         │       21.2 
#> ─────────────┼────────────
#>  Total       │      100.0 
#>  N           │       1175 
#> 
#> Chi-2(2) = 21.6, p < 0.001
#> Cramer's V = 0.14
```

Group by a third variable with `by`:

``` r
cross_tab(sochealth, smoking, education, by = sex)
#> Crosstable: smoking x education (N) | sex = Female
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   95                   220            160 
#>  Yes         │                   38                    62             31 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  133                   282            191 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        475 
#>  Yes         │        131 
#> ─────────────┼────────────
#>  Total       │        606 
#> 
#> Chi-2(2) = 7.1, p = 0.029
#> Cramer's V = 0.11
#> 
#> Crosstable: smoking x education (N) | sex = Male
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   84                   195            172 
#>  Yes         │                   40                    50             28 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  124                   245            200 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        451 
#>  Yes         │        118 
#> ─────────────┼────────────
#>  Total       │        569 
#> 
#> Chi-2(2) = 15.6, p < 0.001
#> Cramer's V = 0.17
```

When both variables are ordered factors,
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
automatically selects an ordinal measure (Kendall’s Tau-b) instead of
Cramer’s V:

``` r
cross_tab(sochealth, self_rated_health, education)
#> Crosstable: self_rated_health x education (N)
#> 
#>  Values         │      Lower secondary       Upper secondary       Tertiary 
#> ────────────────┼───────────────────────────────────────────────────────────
#>  Poor           │                   28                    28              5 
#>  Fair           │                   86                   118             62 
#>  Good           │                  102                   263            193 
#>  Very good      │                   44                   118            133 
#> ────────────────┼───────────────────────────────────────────────────────────
#>  Total          │                  260                   527            393 
#> 
#>  Values         │      Total 
#> ────────────────┼────────────
#>  Poor           │         61 
#>  Fair           │        266 
#>  Good           │        558 
#>  Very good      │        295 
#> ────────────────┼────────────
#>  Total          │       1180 
#> 
#> Chi-2(6) = 73.2, p < 0.001
#> Kendall's Tau-b = 0.20
```

## Association measures

For a quick overview of all available association statistics, pass a
contingency table to
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md):

``` r
tbl <- xtabs(~ smoking + education, data = sochealth)
assoc_measures(tbl)
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
```

Individual functions such as
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
[`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
or
[`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md)
return a scalar by default. Pass `detail = TRUE` for the confidence
interval and p-value:

``` r
cramer_v(tbl, detail = TRUE)
#> Estimate  CI lower  CI upper        p
#>    0.136     0.079     0.191  < 0.001
```

## Summary tables

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
covers grouped or one-way summary tables for categorical variables:

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education,
  output = "tinytable"
)
```

| Variable          | Lower secondary |      | Upper secondary |      | Tertiary |      | Total |      | p       | Cramer's V |
|-------------------|-----------------|------|-----------------|------|----------|------|-------|------|---------|------------|
|                   | n               | %    | n               | %    | n        | %    | n     | %    |         |            |
| smoking           |                 |      |                 |      |          |      |       |      | \< .001 | .14        |
|     No            | 179             | 69.6 | 415             | 78.7 | 332      | 84.9 | 926   | 78.8 |         |            |
|     Yes           | 78              | 30.4 | 112             | 21.3 | 59       | 15.1 | 249   | 21.2 |         |            |
| physical_activity |                 |      |                 |      |          |      |       |      | \< .001 | .21        |
|     No            | 177             | 67.8 | 310             | 57.5 | 163      | 40.8 | 650   | 54.2 |         |            |
|     Yes           | 84              | 32.2 | 229             | 42.5 | 237      | 59.2 | 550   | 45.8 |         |            |
| dentist_12m       |                 |      |                 |      |          |      |       |      | \< .001 | .22        |
|     No            | 113             | 43.3 | 174             | 32.3 | 67       | 16.8 | 354   | 29.5 |         |            |
|     Yes           | 148             | 56.7 | 365             | 67.7 | 333      | 83.2 | 846   | 70.5 |         |            |

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
summarizes continuous variables, either overall or by a categorical `by`
variable, and can also add group-comparison tests:

``` r
table_continuous(
  sochealth,
  select = c(bmi, life_sat_health),
  by = education
)
#> Descriptive statistics
#> 
#>  Variable                       │ Group              M     SD    Min    Max  
#> ────────────────────────────────┼────────────────────────────────────────────
#>  Body mass index                │ Lower secondary  28.09  3.47  18.20  38.90 
#>                                 │ Upper secondary  26.02  3.43  16.00  37.10 
#>                                 │ Tertiary         24.39  3.52  16.00  33.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary  2.71   1.20  1.00   5.00  
#>                                 │ Upper secondary  3.53   1.19  1.00   5.00  
#>                                 │ Tertiary         4.11   1.04  1.00   5.00  
#> 
#>  Variable                       │ Group            95% CI LL  95% CI UL    n 
#> ────────────────────────────────┼────────────────────────────────────────────
#>  Body mass index                │ Lower secondary    27.66      28.51    260 
#>                                 │ Upper secondary    25.73      26.31    534 
#>                                 │ Tertiary           24.04      24.74    394 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary    2.57       2.86     259 
#>                                 │ Upper secondary    3.43       3.63     534 
#>                                 │ Tertiary           4.01       4.21     399
```

For detailed guidance, see the dedicated articles on
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
and the final reporting overview for APA-style summary tables.

## Row-wise summaries

[`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
[`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md), and
[`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
compute row-wise statistics across selected columns, with automatic
handling of missing values.

``` r
sochealth |>
  dplyr::mutate(
    mean_sat  = mean_n(select = starts_with("life_sat")),
    sum_sat   = sum_n(select = starts_with("life_sat"), min_valid = 2),
    n_missing = count_n(select = starts_with("life_sat"), special = "NA")
  ) |>
  dplyr::select(starts_with("life_sat"), mean_sat, sum_sat, n_missing) |>
  head() |>
  as.data.frame()
#>   life_sat_health life_sat_work life_sat_relationships life_sat_standard
#> 1               5             3                      5                 5
#> 2               4             4                      5                 5
#> 3               3             2                      5                 3
#> 4               3             4                      3                 2
#> 5               4             5                      4                 4
#> 6               5             5                      5                 3
#>   mean_sat sum_sat n_missing
#> 1     4.50      18         0
#> 2     4.50      18         0
#> 3     3.25      13         0
#> 4     3.00      12         0
#> 5     4.25      17         0
#> 6     4.50      18         0
```

## Learn more

- See
  [`?varlist`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  to inspect variables, labels, values, and missing data.
- See
  [`?cross_tab`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  for the full list of arguments (weights, simulation, association
  measures).
- See
  [`?table_categorical`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  for grouped or one-way categorical tables.
- See
  [`?table_continuous`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  for continuous summaries and group comparisons.
- See
  [`?assoc_measures`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
  for the complete list of association statistics.
- See
  [`?code_book`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  to generate an interactive HTML codebook.
