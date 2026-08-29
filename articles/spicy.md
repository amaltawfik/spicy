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
social-health survey with 1200 respondents and 24 variables.

## Inspect your data

[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
(or its shortcut
[`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md)) gives
a compact overview of every variable in a data frame: name, label,
representative values, class, number of distinct values, valid
observations, and missing values. In RStudio or Positron, calling
`varlist(mydata)` with the default `tbl = FALSE` opens an interactive
viewer - this is the most common usage in practice. Here we use
`tbl = TRUE` to produce static output for the vignette:

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
produces frequency tables with counts, percentages, valid percentages
(shown by default whenever missing values are present), and optionally
cumulative percentages.

``` r

freq(sochealth, education)
#> Frequency table: education
#> 
#>  Category   │ Values               Freq.    Percent 
#> ────────────┼───────────────────────────────────────
#>  Valid      │ Lower secondary        261       21.8 
#>             │ Upper secondary        539       44.9 
#>             │ Tertiary               400       33.3 
#> ────────────┼───────────────────────────────────────
#>  Total      │                       1200      100.0 
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
#>  Category   │ Values               Freq.    Percent 
#> ────────────┼───────────────────────────────────────
#>  Valid      │ Lower secondary        259       21.6 
#>             │ Upper secondary        546       45.5 
#>             │ Tertiary               395       32.9 
#> ────────────┼───────────────────────────────────────
#>  Total      │                       1200      100.0 
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
```

Add percentages with `percent`:

``` r

cross_tab(sochealth, smoking, education, percent = "column")
#> Crosstable: smoking x education (Column %)
#> 
#>  Values   │   Lower secondary    Upper secondary    Tertiary │   Total 
#> ──────────┼──────────────────────────────────────────────────┼─────────
#>  No       │              69.6               78.7        84.9 │    78.8 
#>  Yes      │              30.4               21.3        15.1 │    21.2 
#> ──────────┼──────────────────────────────────────────────────┼─────────
#>  Total    │             100.0              100.0       100.0 │   100.0 
#>  N        │               257                527         391 │    1175 
#> 
#> Chi-2(2) = 21.6, p <.001
#> Cramer's V = 0.14
#> Missing values removed: smoking (25).
```

Group by a third variable with `by`:

``` r

cross_tab(sochealth, smoking, education, by = sex)
#> Crosstable: smoking x education (N) | sex = Female
#> 
#>  Values   │   Lower secondary    Upper secondary    Tertiary │   Total 
#> ──────────┼──────────────────────────────────────────────────┼─────────
#>  No       │                95                220         160 │     475 
#>  Yes      │                38                 62          31 │     131 
#> ──────────┼──────────────────────────────────────────────────┼─────────
#>  Total    │               133                282         191 │     606 
#> 
#> Chi-2(2) = 7.1, p = .029
#> Cramer's V = 0.11
#> Missing values removed: smoking (14).
#> 
#> Crosstable: smoking x education (N) | sex = Male
#> 
#>  Values   │   Lower secondary    Upper secondary    Tertiary │   Total 
#> ──────────┼──────────────────────────────────────────────────┼─────────
#>  No       │                84                195         172 │     451 
#>  Yes      │                40                 50          28 │     118 
#> ──────────┼──────────────────────────────────────────────────┼─────────
#>  Total    │               124                245         200 │     569 
#> 
#> Chi-2(2) = 15.6, p <.001
#> Cramer's V = 0.17
#> Missing values removed: smoking (11).
```

When both variables are ordered factors,
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
automatically selects an ordinal measure (Kendall’s Tau-b) instead of
Cramer’s V:

``` r

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

## Association measures

For a quick overview of all available association statistics, pass a
contingency table to
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md):

``` r

tbl <- xtabs(~ smoking + education, data = sochealth)
assoc_measures(tbl)
#> Measure                            Estimate     SE  CI lower  CI upper      p 
#> Cramer's V                            0.136      –     0.079     0.191  <.001 
#> Contingency Coefficient               0.134      –         –         –  <.001 
#> Lambda symmetric                      0.000  0.000     0.000     0.000      – 
#> Lambda R|C                            0.000  0.000     0.000     0.000      – 
#> Lambda C|R                            0.000  0.000     0.000     0.000      – 
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

Individual functions such as
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
[`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
or
[`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md)
return a scalar by default. Pass `detail = TRUE` for the confidence
interval and p-value:

``` r

cramer_v(tbl, detail = TRUE)
#> Estimate  SE  CI lower  CI upper      p
#>    0.136   –     0.079     0.191  <.001
```

## Summary tables

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
covers grouped or one-way summary tables for categorical variables:

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education
)
#> Categorical table by education
#> 
#>  Variable                        │ Lower secondary n  Lower secondary % 
#> ─────────────────────────────────┼──────────────────────────────────────
#>  Current smoker                  │                                      
#>    No                            │        179               68.6        
#>    Yes                           │         78               29.9        
#>    (Missing)                     │          4                1.5        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity       │                                      
#>    No                            │        177               67.8        
#>    Yes                           │         84               32.2        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist visit in last 12 months │                                      
#>    No                            │        113               43.3        
#>    Yes                           │        148               56.7        
#> 
#>  Variable                        │ Upper secondary n  Upper secondary % 
#> ─────────────────────────────────┼──────────────────────────────────────
#>  Current smoker                  │                                      
#>    No                            │        415               77.0        
#>    Yes                           │        112               20.8        
#>    (Missing)                     │         12                2.2        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity       │                                      
#>    No                            │        310               57.5        
#>    Yes                           │        229               42.5        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist visit in last 12 months │                                      
#>    No                            │        174               32.3        
#>    Yes                           │        365               67.7        
#> 
#>  Variable                        │ Tertiary n  Tertiary %  Total n  Total % 
#> ─────────────────────────────────┼──────────────────────────────────────────
#>  Current smoker                  │                                          
#>    No                            │    332         83.0       926     77.2   
#>    Yes                           │     59         14.8       249     20.8   
#>    (Missing)                     │      9          2.2        25      2.1   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity       │                                          
#>    No                            │    163         40.8       650     54.2   
#>    Yes                           │    237         59.2       550     45.8   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist visit in last 12 months │                                          
#>    No                            │     67         16.8       354     29.5   
#>    Yes                           │    333         83.2       846     70.5   
#> 
#>  Variable                        │   p    Cramer's V 
#> ─────────────────────────────────┼───────────────────
#>  Current smoker                  │ <.001     .14     
#>    No                            │                   
#>    Yes                           │                   
#>    (Missing)                     │                   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity       │ <.001     .21     
#>    No                            │                   
#>    Yes                           │                   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist visit in last 12 months │ <.001     .22     
#>    No                            │                   
#>    Yes                           │
```

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
summarizes continuous variables, either overall or by a categorical `by`
variable; when `by` is supplied it also reports a group-comparison test
(a Welch test by default):

``` r

table_continuous(
  sochealth,
  select = c(bmi, life_sat_health),
  by = education
)
#> Descriptive statistics by Highest education level
#> 
#>  Variable                       │ Group              M     SD    Min    Max  
#> ────────────────────────────────┼────────────────────────────────────────────
#>  Body mass index                │ Lower secondary  28.09  3.47  18.20  38.90 
#>                                 │ Upper secondary  26.02  3.43  16.00  37.10 
#>                                 │ Tertiary         24.39  3.52  16.00  33.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary   2.71  1.20   1.00   5.00 
#>                                 │ Upper secondary   3.53  1.19   1.00   5.00 
#>                                 │ Tertiary          4.11  1.04   1.00   5.00 
#> 
#>  Variable                       │ Group            95% CI LL  95% CI UL   n  
#> ────────────────────────────────┼────────────────────────────────────────────
#>  Body mass index                │ Lower secondary    27.66      28.51    260 
#>                                 │ Upper secondary    25.73      26.31    534 
#>                                 │ Tertiary           24.04      24.74    394 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary     2.57       2.86    259 
#>                                 │ Upper secondary     3.43       3.63    534 
#>                                 │ Tertiary            4.01       4.21    399 
#> 
#>  Variable                       │ Group              p   
#> ────────────────────────────────┼────────────────────────
#>  Body mass index                │ Lower secondary  <.001 
#>                                 │ Upper secondary        
#>                                 │ Tertiary               
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary  <.001 
#>                                 │ Upper secondary        
#>                                 │ Tertiary               
#> 
#> Missing values removed: bmi (12), life_sat_health (8).
```

[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
covers the same reporting territory when you want to stay in a
linear-model framework, for example with robust or cluster-robust
standard errors, case weights, or additive covariate adjustment:

``` r

table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  vcov = "HC3"
)
#> Continuous outcomes by Sex
#> 
#>  Variable                      │ M (Female)  M (Male)  Δ (Male - Female) 
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   67.16      71.05          3.89        
#>  Body mass index               │   25.69      26.20          0.51        
#> 
#>  Variable                      │ 95% CI LL  95% CI UL    p     R²    n   
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   2.12       5.65     <.001  0.02  1200 
#>  Body mass index               │   0.09       0.93      .018  0.00  1188 
#> 
#> Note. Std. errors: heteroskedasticity-robust (HC3).
#> Missing values removed: bmi (12).
```

By default every `table_*` helper formats to APA conventions. To match a
journal instead, pass `style =` — `"jama"`, `"nejm"`, `"lancet"`,
`"annals"`, `"apa"`, `"aer"`;
[`spicy_style_names()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
lists them all. Set it once for a whole document with
`options(spicy.style = "nejm")`, or compose a custom variant with
[`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md),
for example `spicy_style("lancet", ci_sep = " to ")`. The same styles
apply to the regression tables below. French typographic conventions are
not a journal style: they come with the language, through
`options(spicy.language = "fr")`.

For detailed guidance, see the dedicated articles on
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
and
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
and the final reporting overview tying the summary tables together.

## Regression tables

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reports the full coefficient table for one or several fitted models —
more than thirty classes, from [`lm()`](https://rdrr.io/r/stats/lm.html)
/ [`glm()`](https://rdrr.io/r/stats/glm.html) to mixed-effects, ordinal,
survival and Bayesian engines (the full map is the [Supported
models](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.html)
article) — with APA formatting by default and the same journal styles on
demand (`style = "jama"`, `"nejm"`, `"lancet"` and more), factor
grouping with reference rows, robust variance, standardised
coefficients, average marginal effects, hierarchical comparisons, and
side-by-side multi-model layouts:

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(fit)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.66  [61.95, 68.45]  <.001 
#>  age             │    0.05  0.03  [-0.01,  0.11]   .130 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    3.86  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │     –     –          –          –    
#>    Yes           │   -1.72  1.11  [-3.89,  0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj. R²         │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
```

For detailed guidance, start with the core
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
article; the Learn more section below lists the companion articles by
model family.

## Row-wise summaries

[`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
[`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md), and
[`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
compute row-wise statistics across selected columns, with explicit
control over missing values. By default, a row mean or sum requires
every selected value to be valid; `min_valid` relaxes this to a minimum
count (or proportion) of valid values. The rows selected below include
respondents with missing satisfaction items so the difference is
visible: `mean_sat` (no `min_valid`) turns `NA` as soon as one item is
missing, `sum_sat` is computed from the valid items as long as at least
three are present (and turns `NA` in the last row, which has only two),
and `count_n(special = "NA")` counts the missing items themselves:

``` r

sochealth |>
  dplyr::mutate(
    mean_sat  = mean_n(select = starts_with("life_sat")),
    sum_sat   = sum_n(select = starts_with("life_sat"), min_valid = 3),
    n_missing = count_n(select = starts_with("life_sat"), special = "NA")
  ) |>
  dplyr::select(starts_with("life_sat"), mean_sat, sum_sat, n_missing) |>
  dplyr::slice(c(1, 2, 43, 82, 455)) |>
  as.data.frame()
#>   life_sat_health life_sat_work life_sat_relationships life_sat_standard
#> 1               5             3                      5                 5
#> 2               4             4                      5                 5
#> 3               2            NA                      3                 4
#> 4               1            NA                      2                 2
#> 5               5            NA                      4                NA
#>   mean_sat sum_sat n_missing
#> 1      4.5      18         0
#> 2      4.5      18         0
#> 3       NA       9         1
#> 4       NA       5         1
#> 5       NA      NA         2
```

## Learn more

This is the vignette the package ships. Every walk-through below is an
article on the [spicy website](https://amaltawfik.github.io/spicy/),
kept alongside the reference pages and rebuilt with each release.

**Explore and tabulate**

- [Explore variables and build
  codebooks](https://amaltawfik.github.io/spicy/articles/variable-exploration.html)
  — inspect variables, labels and missingness
  ([`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md),
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md),
  [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)).
- [Frequency tables and
  cross-tabulations](https://amaltawfik.github.io/spicy/articles/frequency-tables.html)
  — [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  in depth (weights, simulation, labelled data).
- [Cramer’s V, Phi, and association
  measures](https://amaltawfik.github.io/spicy/articles/association-measures.html)
  — choosing the right effect size for a contingency table.

**Summary tables**

- [Categorical summary
  tables](https://amaltawfik.github.io/spicy/articles/table-categorical.html)
  and [Continuous summary
  tables](https://amaltawfik.github.io/spicy/articles/table-continuous.html)
  — the Table 1 / 2 builders (APA by default, other journal styles via
  `style =`).
- [One outcome across several
  groupings](https://amaltawfik.github.io/spicy/articles/table-outcome.html)
  — one continuous outcome across several groupings, one block of rows
  per grouping.
- [Model-based continuous summary
  tables](https://amaltawfik.github.io/spicy/articles/table-continuous-lm.html)
  — model-based group comparisons (robust SE, weights, covariate
  adjustment).
- [Summary tables from a survey
  design](https://amaltawfik.github.io/spicy/articles/survey-tables.html)
  — the same two tables from a `survey` design: design-based standard
  errors, degrees of freedom and tests
  ([`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md),
  [`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)).

**Regression tables**

- [Publication-ready regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression.html)
  — the core guide, including the univariable screen
  ([`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)).
- [Supported
  models](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.html)
  — the class-by-class capability map (more than thirty model classes).
- [The structured
  view](https://amaltawfik.github.io/spicy/articles/as-structured.html)
  — the typed view behind every regression table: filter, aggregate, or
  build a custom renderer.
- [Mixed-effects regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.html)
  — mixed-effects (multilevel) models, random effects as table rows,
  ICC.
- [GEE regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression-gee.html)
  — GEE (population-averaged) models, working-correlation choice via
  QIC, sandwich inference.
- [Count and two-part regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression-counts.html)
  — Poisson, negative-binomial, zero-inflated and hurdle models.
- [Survival regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression-survival.html)
  — Cox hazard ratios and accelerated failure time models.
- [Ordinal regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.html)
  — proportional-odds models, thresholds, per-category marginal effects.
- [Multinomial regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression-multinomial.html)
  — multinomial logit with outcome categories as columns.
- [Bayesian regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression-bayesian.html)
  — posterior medians, credible intervals, convergence checks.
- [Categorical
  predictors](https://amaltawfik.github.io/spicy/articles/categorical-predictors.html)
  — reference levels, joint tests, contrasts — across all of the above.

**Putting it together**

- [Summary tables for
  reporting](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.html)
  — the end-to-end reporting sequence tying the four `table_*` helpers
  together.
- [`?mean_n`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  [`?sum_n`](https://amaltawfik.github.io/spicy/reference/sum_n.md),
  [`?count_n`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  — row-wise summaries with minimum-valid-values rules.
