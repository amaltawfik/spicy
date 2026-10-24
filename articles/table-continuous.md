# Continuous summary tables

``` r

library(spicy)
```

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
summarizes continuous variables either overall or by a categorical
grouping variable. It is designed for readable summary tables in the
console and for publication-ready outputs in rendered documents. When
`by` is supplied, it can also add group-comparison tests, test
statistics, and effect sizes. Formatting follows APA conventions by
default; `style = "jama"` (or `"nejm"`, `"lancet"`, `"annals"`, `"aer"`)
switches the whole table to that journal’s published rules — the *House
styles* section of [*Summary tables for
reporting*](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md)
shows each style and the guideline sentence behind every rule.

## Basic usage

Use `select` to choose the continuous variables you want to summarize:

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health)
)
#> Descriptive statistics
#> 
#>  Variable        │   M     SD     Min    Max    95% CI LL  95% CI UL   n   
#> ─────────────────┼─────────────────────────────────────────────────────────
#>  bmi             │ 25.93   3.72  16.00   38.90    25.72      26.14    1188 
#>  wellbeing_score │ 69.04  15.62  18.70  100.00    68.16      69.93    1200 
#>  life_sat_health │  3.55   1.25   1.00    5.00     3.48       3.62    1192 
#> 
#> Note. Missing values removed: bmi (12), life_sat_health (8). CI = confidence interval; LL = lower limit; UL = upper limit.
```

If you omit `select`,
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
scans the data frame and keeps numeric columns. Non-numeric columns are
dropped silently; set `verbose = TRUE` to list them in a message:

``` r

table_continuous(sochealth)
#> Descriptive statistics
#> 
#>  Variable               │    M       SD       Min      Max    95% CI LL 
#> ────────────────────────┼───────────────────────────────────────────────
#>  age                    │   49.26    14.70    25.00    75.00     48.43  
#>  income                 │ 3833.00  1394.58  1000.00  7388.00   3754.01  
#>  wellbeing_score        │   69.04    15.62    18.70   100.00     68.16  
#>  bmi                    │   25.93     3.72    16.00    38.90     25.72  
#>  political_position     │    5.48     2.03     0.00    10.00      5.36  
#>  life_sat_health        │    3.55     1.25     1.00     5.00      3.48  
#>  life_sat_work          │    3.38     1.18     1.00     5.00      3.31  
#>  life_sat_relationships │    3.72     1.10     1.00     5.00      3.66  
#>  life_sat_standard      │    3.40     1.16     1.00     5.00      3.33  
#>  weight                 │    1.00     0.41     0.29     3.45      0.97  
#> 
#>  Variable               │ 95% CI UL   n   
#> ────────────────────────┼─────────────────
#>  age                    │    50.10   1200 
#>  income                 │  3911.98   1200 
#>  wellbeing_score        │    69.93   1200 
#>  bmi                    │    26.14   1188 
#>  political_position     │     5.60   1185 
#>  life_sat_health        │     3.62   1192 
#>  life_sat_work          │     3.45   1192 
#>  life_sat_relationships │     3.79   1192 
#>  life_sat_standard      │     3.46   1192 
#>  weight                 │     1.02   1200 
#> 
#> Note. Missing values removed: bmi (12), political_position (15), life_sat_health (8), life_sat_work (8), life_sat_relationships (8), life_sat_standard (8). CI = confidence interval; LL = lower limit; UL = upper limit.
```

## Grouped summaries

Add `by` to summarize the same variables across categories. When `by` is
supplied, a Welch-test *p*-value column is added automatically:

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education
)
#> Descriptive statistics by education
#> 
#>  Variable        │ Group              M     SD     Min    Max    95% CI LL 
#> ─────────────────┼─────────────────────────────────────────────────────────
#>  bmi             │ Lower secondary  28.09   3.47  18.20   38.90    27.66   
#>                  │ Upper secondary  26.02   3.43  16.00   37.10    25.73   
#>                  │ Tertiary         24.39   3.52  16.00   33.00    24.04   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary  57.22  15.44  18.70   97.90    55.33   
#>                  │ Upper secondary  68.97  13.62  26.70  100.00    67.82   
#>                  │ Tertiary         76.85  13.23  40.40  100.00    75.55   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_health │ Lower secondary   2.71   1.20   1.00    5.00     2.57   
#>                  │ Upper secondary   3.53   1.19   1.00    5.00     3.43   
#>                  │ Tertiary          4.11   1.04   1.00    5.00     4.01   
#> 
#>  Variable        │ Group            95% CI UL   n     p   
#> ─────────────────┼────────────────────────────────────────
#>  bmi             │ Lower secondary    28.51    260  <.001 
#>                  │ Upper secondary    26.31    534        
#>                  │ Tertiary           24.74    394        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary    59.10    261  <.001 
#>                  │ Upper secondary    70.12    539        
#>                  │ Tertiary           78.15    400        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_health │ Lower secondary     2.86    259  <.001 
#>                  │ Upper secondary     3.63    534        
#>                  │ Tertiary            4.21    399        
#> 
#> Note. Missing values removed: bmi (12), life_sat_health (8). P-values: Welch one-way ANOVA. CI = confidence interval; LL = lower limit; UL = upper limit.
```

This is the main pattern for reporting continuous variables across
groups such as education, sex, treatment arm, or survey wave. Pass
`p_value = FALSE` to suppress the test column and keep the output
strictly descriptive. When a journal asks for adjusted *p*-values,
`p_adjust = "holm"` (or `"BH"`) adjusts all the tests of the table
together and the note says so.

If you want the same outcomes reported in a linear-model workflow, with
heteroskedasticity-consistent or cluster-robust standard errors, case
weights, or additive covariate adjustment, use
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
instead:

``` r

table_continuous_lm(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  vcov = "HC3"
)
#> Continuous outcomes by education
#> 
#>                                        M                                   
#>                    ──────────────────────────────────────────              
#>  Variable        │ Lower secondary  Upper secondary  Tertiary    p     R²  
#> ─────────────────┼─────────────────────────────────────────────────────────
#>  bmi             │      28.09            26.02        24.39    <.001  0.13 
#>  wellbeing_score │      57.22            68.97        76.85    <.001  0.21 
#>  life_sat_health │       2.71             3.53         4.11    <.001  0.16 
#> 
#>  Variable        │  n   
#> ─────────────────┼──────
#>  bmi             │ 1188 
#>  wellbeing_score │ 1200 
#>  life_sat_health │ 1192 
#> 
#> Note. Missing values removed: bmi (12), life_sat_health (8). P-values: F-test from the linear model. Std. errors: heteroskedasticity-robust (HC3).
```

## Add test statistics and effect sizes

Grouped tables can also report the test statistic and an effect size
alongside the default *p*-value column:

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  statistic = TRUE,
  effect_size = "auto",
  effect_size_ci = TRUE
)
#> Descriptive statistics by education
#> 
#>  Variable        │ Group              M     SD     Min    Max    95% CI LL 
#> ─────────────────┼─────────────────────────────────────────────────────────
#>  bmi             │ Lower secondary  28.09   3.47  18.20   38.90    27.66   
#>                  │ Upper secondary  26.02   3.43  16.00   37.10    25.73   
#>                  │ Tertiary         24.39   3.52  16.00   33.00    24.04   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary  57.22  15.44  18.70   97.90    55.33   
#>                  │ Upper secondary  68.97  13.62  26.70  100.00    67.82   
#>                  │ Tertiary         76.85  13.23  40.40  100.00    75.55   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_health │ Lower secondary   2.71   1.20   1.00    5.00     2.57   
#>                  │ Upper secondary   3.53   1.19   1.00    5.00     3.43   
#>                  │ Tertiary          4.11   1.04   1.00    5.00     4.01   
#> 
#>  Variable        │ Group            95% CI UL   n           Test          
#> ─────────────────┼────────────────────────────────────────────────────────
#>  bmi             │ Lower secondary    28.51    260  F(2, 654.48) = 87.96  
#>                  │ Upper secondary    26.31    534                        
#>                  │ Tertiary           24.74    394                        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary    59.10    261  F(2, 638.59) = 144.35 
#>                  │ Upper secondary    70.12    539                        
#>                  │ Tertiary           78.15    400                        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_health │ Lower secondary     2.86    259  F(2, 652.08) = 118.74 
#>                  │ Upper secondary     3.63    534                        
#>                  │ Tertiary            4.21    399                        
#> 
#>  Variable        │ Group              p         ES [95% CI]       
#> ─────────────────┼────────────────────────────────────────────────
#>  bmi             │ Lower secondary  <.001  η² = 0.13 [0.10, 0.17] 
#>                  │ Upper secondary                                
#>                  │ Tertiary                                       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary  <.001  η² = 0.21 [0.17, 0.25] 
#>                  │ Upper secondary                                
#>                  │ Tertiary                                       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_health │ Lower secondary  <.001  η² = 0.16 [0.13, 0.20] 
#>                  │ Upper secondary                                
#>                  │ Tertiary                                       
#> 
#> Note. Missing values removed: bmi (12), life_sat_health (8). P-values: Welch one-way ANOVA. CI = confidence interval; LL = lower limit; UL = upper limit; ES = effect size.
```

Use `test = "student"` for equal-variance parametric tests or
`test = "nonparametric"` for rank-based comparisons:

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education,
  test = "nonparametric",
  statistic = TRUE,
  effect_size = "auto"
)
#> Descriptive statistics by education
#> 
#>  Variable        │ Group              M     SD     Min    Max    95% CI LL 
#> ─────────────────┼─────────────────────────────────────────────────────────
#>  bmi             │ Lower secondary  28.09   3.47  18.20   38.90    27.66   
#>                  │ Upper secondary  26.02   3.43  16.00   37.10    25.73   
#>                  │ Tertiary         24.39   3.52  16.00   33.00    24.04   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary  57.22  15.44  18.70   97.90    55.33   
#>                  │ Upper secondary  68.97  13.62  26.70  100.00    67.82   
#>                  │ Tertiary         76.85  13.23  40.40  100.00    75.55   
#> 
#>  Variable        │ Group            95% CI UL   n       Test         p   
#> ─────────────────┼───────────────────────────────────────────────────────
#>  bmi             │ Lower secondary    28.51    260  H(2) = 144.63  <.001 
#>                  │ Upper secondary    26.31    534                       
#>                  │ Tertiary           24.74    394                       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary    59.10    261  H(2) = 233.53  <.001 
#>                  │ Upper secondary    70.12    539                       
#>                  │ Tertiary           78.15    400                       
#> 
#>  Variable        │ Group               ES     
#> ─────────────────┼────────────────────────────
#>  bmi             │ Lower secondary  ε² = 0.12 
#>                  │ Upper secondary            
#>                  │ Tertiary                   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary  ε² = 0.19 
#>                  │ Upper secondary            
#>                  │ Tertiary                   
#> 
#> Note. Missing values removed: bmi (12). P-values: Kruskal-Wallis test. CI = confidence interval; LL = lower limit; UL = upper limit; ES = effect size.
```

`effect_size = "auto"` selects the canonical measure for the chosen
`test` and number of groups: Hedges’s *g* (parametric, 2 groups),
eta-squared (parametric, 3+ groups), rank-biserial *r* (nonparametric, 2
groups), or epsilon-squared (nonparametric, 3+ groups). To pick a
specific measure explicitly, pass its character name:

``` r

table_continuous(
  sochealth,
  select = wellbeing_score,
  by = sex,
  effect_size = "hedges_g",
  effect_size_ci = TRUE
)
#> Descriptive statistics by sex
#> 
#>  Variable        │ Group     M     SD     Min    Max    95% CI LL  95% CI UL 
#> ─────────────────┼───────────────────────────────────────────────────────────
#>  wellbeing_score │ Female  67.16  14.80  19.60  100.00    65.99      68.33   
#>                  │ Male    71.05  16.23  18.70  100.00    69.73      72.37   
#> 
#>  Variable        │ Group    n     p          ES [95% CI]        
#> ─────────────────┼──────────────────────────────────────────────
#>  wellbeing_score │ Female  620  <.001  g = -0.25 [-0.36, -0.14] 
#>                  │ Male    580                                  
#> 
#> Note. P-value: Welch t-test. CI = confidence interval; LL = lower limit; UL = upper limit; ES = effect size.
```

Allowed values are `"none"` (default), `"auto"`, `"hedges_g"`, `"eta2"`,
`"r_rb"`, and `"epsilon2"`. Incompatible explicit choices (e.g. `"eta2"`
with two groups, or `"hedges_g"` with `test = "nonparametric"`) trigger
an actionable error.

When you need the numbers for further processing, use
`output = "data.frame"` for the table as it is displayed, one row per
printed row: the key columns `variable`, `label`, `group` and
`.row_role`, then the printed columns. A cell that prints several
numbers keeps the others beside it: the test statistic `Test` its
degrees of freedom `df` and `df.residual`:

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education,
  statistic = TRUE,
  effect_size = "auto",
  output = "data.frame"
)
#>          variable           label           group .row_role        M        SD
#> 1             bmi             bmi Lower secondary     group 28.08731  3.471744
#> 2             bmi             bmi Upper secondary     group 26.01891  3.434736
#> 3             bmi             bmi        Tertiary     group 24.39036  3.520150
#> 4 wellbeing_score wellbeing_score Lower secondary     group 57.21571 15.444587
#> 5 wellbeing_score wellbeing_score Upper secondary     group 68.96920 13.621193
#> 6 wellbeing_score wellbeing_score        Tertiary     group 76.85250 13.226818
#>    Min   Max 95% CI LL 95% CI UL   n      Test df df.residual            p
#> 1 18.2  38.9  27.66333  28.51129 260  87.95902  2    654.4758 1.467916e-34
#> 2 16.0  37.1  25.72693  26.31090 534        NA NA          NA           NA
#> 3 16.0  33.0  24.04170  24.73901 394        NA NA          NA           NA
#> 4 18.7  97.9  55.33323  59.09819 261 144.35083  2    638.5873 1.888362e-52
#> 5 26.7 100.0  67.81669  70.12172 539        NA NA          NA           NA
#> 6 40.4 100.0  75.55235  78.15265 400        NA NA          NA           NA
#>          ES
#> 1 0.1307679
#> 2        NA
#> 3        NA
#> 4 0.2081970
#> 5        NA
#> 6        NA
```

`output = "long"` gives every statistic the table computes, whatever it
displays, one row per variable and group, in the vocabulary of `broom`
(`statistic`, `df`, `df.residual`, `p.value`, `es_type`, `es_value`).

## Balance: the standardized mean difference

A baseline table comparing two arms is usually read for *balance*, not
for significance. `smd = TRUE` adds the standardized mean difference of
the Table 1 literature, and the idiomatic balance table drops the *p*
column at the same time:

``` r

table_continuous(
  sochealth,
  select = c(age, bmi, wellbeing_score),
  by = sex,
  smd = TRUE,
  p_value = FALSE
)
#> Descriptive statistics by sex
#> 
#>  Variable        │ Group     M     SD     Min    Max    95% CI LL  95% CI UL 
#> ─────────────────┼───────────────────────────────────────────────────────────
#>  age             │ Female  49.38  14.91  25.00   75.00    48.20      50.55   
#>                  │ Male    49.14  14.50  25.00   75.00    47.96      50.32   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  bmi             │ Female  25.69   3.78  16.00   38.90    25.39      25.98   
#>                  │ Male    26.20   3.64  16.00   37.70    25.90      26.50   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Female  67.16  14.80  19.60  100.00    65.99      68.33   
#>                  │ Male    71.05  16.23  18.70  100.00    69.73      72.37   
#> 
#>  Variable        │ Group    n    SMD  
#> ─────────────────┼────────────────────
#>  age             │ Female  620   0.02 
#>                  │ Male    580        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  bmi             │ Female  616  -0.14 
#>                  │ Male    572        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Female  620  -0.25 
#>                  │ Male    580        
#> 
#> Note. Missing values removed: bmi (12). SMD direction: Female − Male. The usual imbalance threshold is |SMD| > 0.1. CI = confidence interval; LL = lower limit; UL = upper limit; SMD = standardized mean difference.
```

The two arguments are independent — turning the SMD on turns nothing
else off, so you have to write both. The rule of thumb the table note
quotes is \|SMD\| \> 0.1; spicy prints the number and never highlights
it.

The column is **signed**, group 1 minus group 2 in the order the table
displays the groups, so the direction of an imbalance is readable
without going back to the two means. It requires exactly two groups: a
`by` with three or more is refused rather than averaged over pairs,
because an average over pairs has no published reading and can sit under
0.1 while one pair sits well over it.

### It is not the effect size

The SMD and Hedges’s *g* look alike and are never the same number. On
`sochealth` the two agree to two decimals, which hides the point, so
here they are on a small unbalanced sample where they cannot hide:

``` r

small <- sochealth[c(1:6, 601:604), c("sex", "bmi")]
table_continuous(
  small,
  select = bmi,
  by = sex,
  smd = TRUE,
  effect_size = "hedges_g",
  effect_size_digits = 4
)
#> Descriptive statistics by sex
#> 
#>  Variable │ Group     M     SD    Min    Max   95% CI LL  95% CI UL  n   p   
#> ──────────┼──────────────────────────────────────────────────────────────────
#>  bmi      │ Female  25.03  3.74  19.50  29.10    21.57      28.49    7  .072 
#>           │ Male    30.20  3.03  27.80  33.60    22.68      37.72    3       
#> 
#>  Variable │ Group       ES         SMD   
#> ──────────┼──────────────────────────────
#>  bmi      │ Female  g = -1.3057  -1.5194 
#>           │ Male                         
#> 
#> Note. P-value: Welch t-test. SMD direction: Female − Male. The usual imbalance threshold is |SMD| > 0.1. CI = confidence interval; LL = lower limit; UL = upper limit; ES = effect size; SMD = standardized mean difference.
```

Both standardize a mean difference, but by different denominators.
Austin’s SMD divides by the root **mean of the two group variances**;
*g* divides by the degrees-of-freedom pooled SD and then multiplies by
the small-sample correction *J*. Two consequences, both worth knowing:

- At **equal** group sizes the two denominators coincide, so the SMD
  *is* Cohen’s *d*. At unequal sizes it is not.
- *g* is never the SMD, at any sample size. With equal groups the ratio
  *g* / SMD is exactly *J*: 0.80 at *n* = 3 per group, 0.96 at *n* = 10,
  approaching 1 only as the sample grows. Reading one for the other
  costs 20% on a small trial.

Read each for what it is: the SMD is a balance diagnostic, *g* is an
effect size. It also follows that the SMD carries **no confidence
interval and no *p*-value** — attaching one would put back the test
reasoning the balance literature asks the reader to drop.

## Selecting variables

`select` supports tidyselect helpers:

``` r

table_continuous(
  sochealth,
  select = starts_with("life_sat"),
  by = sex
)
#> Descriptive statistics by sex
#> 
#>  Variable               │ Group    M     SD   Min   Max   95% CI LL  95% CI UL 
#> ────────────────────────┼──────────────────────────────────────────────────────
#>  life_sat_health        │ Female  3.51  1.25  1.00  5.00    3.41       3.61    
#>                         │ Male    3.59  1.25  1.00  5.00    3.49       3.69    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_work          │ Female  3.32  1.17  1.00  5.00    3.23       3.41    
#>                         │ Male    3.44  1.20  1.00  5.00    3.34       3.54    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_relationships │ Female  3.71  1.09  1.00  5.00    3.62       3.79    
#>                         │ Male    3.74  1.10  1.00  5.00    3.65       3.83    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_standard      │ Female  3.37  1.16  1.00  5.00    3.28       3.46    
#>                         │ Male    3.42  1.17  1.00  5.00    3.33       3.52    
#> 
#>  Variable               │ Group    n    p   
#> ────────────────────────┼───────────────────
#>  life_sat_health        │ Female  616  .267 
#>                         │ Male    576       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_work          │ Female  615  .073 
#>                         │ Male    577       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_relationships │ Female  615  .570 
#>                         │ Male    577       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_standard      │ Female  615  .453 
#>                         │ Male    577       
#> 
#> Note. Missing values removed: life_sat_health (8), life_sat_work (8), life_sat_relationships (8), life_sat_standard (8). P-values: Welch t-test. CI = confidence interval; LL = lower limit; UL = upper limit.
```

For more programmatic selection, set `regex = TRUE`:

``` r

table_continuous(
  sochealth,
  select = "^life_sat",
  regex = TRUE,
  by = education,
  output = "data.frame"
)
#>                  variable                  label           group .row_role
#> 1         life_sat_health        life_sat_health Lower secondary     group
#> 2         life_sat_health        life_sat_health Upper secondary     group
#> 3         life_sat_health        life_sat_health        Tertiary     group
#> 4           life_sat_work          life_sat_work Lower secondary     group
#> 5           life_sat_work          life_sat_work Upper secondary     group
#> 6           life_sat_work          life_sat_work        Tertiary     group
#> 7  life_sat_relationships life_sat_relationships Lower secondary     group
#> 8  life_sat_relationships life_sat_relationships Upper secondary     group
#> 9  life_sat_relationships life_sat_relationships        Tertiary     group
#> 10      life_sat_standard      life_sat_standard Lower secondary     group
#> 11      life_sat_standard      life_sat_standard Upper secondary     group
#> 12      life_sat_standard      life_sat_standard        Tertiary     group
#>           M        SD Min Max 95% CI LL 95% CI UL   n            p
#> 1  2.714286 1.2021575   1   5  2.567189  2.861382 259 1.063917e-44
#> 2  3.533708 1.1853493   1   5  3.432943  3.634473 534           NA
#> 3  4.110276 1.0432216   1   5  4.007602  4.212950 399           NA
#> 4  2.570881 1.1467994   1   5  2.431102  2.710660 261 1.398117e-40
#> 5  3.422430 1.1037312   1   5  3.328691  3.516169 535           NA
#> 6  3.851010 1.0314174   1   5  3.749112  3.952909 396           NA
#> 7  3.023077 1.2268891   1   5  2.873246  3.172908 260 1.969764e-32
#> 8  3.743446 0.9645227   1   5  3.661453  3.825439 534           NA
#> 9  4.158291 0.9322485   1   5  4.066423  4.250159 398           NA
#> 10 2.666667 1.1635489   1   5  2.524846  2.808487 261 5.105889e-39
#> 11 3.387218 1.1065913   1   5  3.292970  3.481466 532           NA
#> 12 3.887218 0.9588582   1   5  3.792847  3.981589 399           NA
```

Use `exclude` when you want a broad selection with one or two explicit
removals:

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health, life_sat_work),
  exclude = "life_sat_work",
  by = sex
)
#> Descriptive statistics by sex
#> 
#>  Variable        │ Group     M     SD     Min    Max    95% CI LL  95% CI UL 
#> ─────────────────┼───────────────────────────────────────────────────────────
#>  bmi             │ Female  25.69   3.78  16.00   38.90    25.39      25.98   
#>                  │ Male    26.20   3.64  16.00   37.70    25.90      26.50   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Female  67.16  14.80  19.60  100.00    65.99      68.33   
#>                  │ Male    71.05  16.23  18.70  100.00    69.73      72.37   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_health │ Female   3.51   1.25   1.00    5.00     3.41       3.61   
#>                  │ Male     3.59   1.25   1.00    5.00     3.49       3.69   
#> 
#>  Variable        │ Group    n     p   
#> ─────────────────┼────────────────────
#>  bmi             │ Female  616   .018 
#>                  │ Male    572        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Female  620  <.001 
#>                  │ Male    580        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_health │ Female  616   .267 
#>                  │ Male    576        
#> 
#> Note. Missing values removed: bmi (12), life_sat_health (8). P-values: Welch t-test. CI = confidence interval; LL = lower limit; UL = upper limit.
```

## Handling missing values

Missing values can never enter a mean, so they are always excluded from
each variable’s statistics. The table discloses the exclusion in a note
rather than staying silent:

``` r

table_continuous(
  sochealth,
  select = c(bmi, life_sat_health)
)
#> Descriptive statistics
#> 
#>  Variable        │   M     SD    Min    Max   95% CI LL  95% CI UL   n   
#> ─────────────────┼───────────────────────────────────────────────────────
#>  bmi             │ 25.93  3.72  16.00  38.90    25.72      26.14    1188 
#>  life_sat_health │  3.55  1.25   1.00   5.00     3.48       3.62    1192 
#> 
#> Note. Missing values removed: bmi (12), life_sat_health (8). CI = confidence interval; LL = lower limit; UL = upper limit.
```

Missing values in `by` are removed by default (`drop_na = TRUE`), and
the removal is again disclosed rather than silent, in the note under the
table:

``` r

table_continuous(
  sochealth,
  select = bmi,
  by = income_group
)
#> Descriptive statistics by income_group
#> 
#>  Variable │ Group           M     SD    Min    Max   95% CI LL  95% CI UL   n  
#> ──────────┼────────────────────────────────────────────────────────────────────
#>  bmi      │ Low           26.58  3.94  16.00  38.90    26.08      27.07    246 
#>           │ Lower middle  26.19  3.47  16.00  37.30    25.84      26.53    385 
#>           │ Upper middle  25.66  3.89  16.00  37.70    25.24      26.09    325 
#>           │ High          25.15  3.43  16.00  35.00    24.68      25.61    214 
#> 
#>  Variable │ Group           p   
#> ──────────┼─────────────────────
#>  bmi      │ Low           <.001 
#>           │ Lower middle        
#>           │ Upper middle        
#>           │ High                
#> 
#> Note. Missing values removed: bmi (12), income_group (18). P-value: Welch one-way ANOVA. CI = confidence interval; LL = lower limit; UL = upper limit.
```

Set `drop_na = FALSE` to display those rows as a dedicated “(Missing)”
group instead. The group-comparison test and effect size still cover the
observed groups only, matching
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md):

``` r

table_continuous(
  sochealth,
  select = bmi,
  by = income_group,
  drop_na = FALSE
)
#> Descriptive statistics by income_group
#> 
#>  Variable │ Group           M     SD    Min    Max   95% CI LL  95% CI UL   n  
#> ──────────┼────────────────────────────────────────────────────────────────────
#>  bmi      │ Low           26.58  3.94  16.00  38.90    26.08      27.07    246 
#>           │ Lower middle  26.19  3.47  16.00  37.30    25.84      26.53    385 
#>           │ Upper middle  25.66  3.89  16.00  37.70    25.24      26.09    325 
#>           │ High          25.15  3.43  16.00  35.00    24.68      25.61    214 
#>           │ (Missing)     25.83  4.08  18.50  31.80    23.80      27.86     18 
#> 
#>  Variable │ Group           p   
#> ──────────┼─────────────────────
#>  bmi      │ Low           <.001 
#>           │ Lower middle        
#>           │ Upper middle        
#>           │ High                
#>           │ (Missing)           
#> 
#> Note. Missing values removed: bmi (12). P-value: Welch one-way ANOVA. CI = confidence interval; LL = lower limit; UL = upper limit.
```

## Weights

Survey data usually comes with case weights, and `sochealth` carries
one. Passing it weights every displayed statistic — mean, SD, quantiles,
extremes, and the CI of the mean — and the table says so in its note:

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  weights = weight,
  rescale = TRUE
)
#> Descriptive statistics
#> 
#>  Variable        │   M     SD     Min    Max    95% CI LL  95% CI UL   n   
#> ─────────────────┼─────────────────────────────────────────────────────────
#>  bmi             │ 25.72   3.69  16.00   38.90    25.51      25.94    1188 
#>  wellbeing_score │ 68.78  15.54  18.70  100.00    67.90      69.66    1200 
#> 
#> Note. Statistics weighted by weight. Missing values removed: bmi (12). CI = confidence interval; LL = lower limit; UL = upper limit.
```

Two conventions coexist for weighted statistics, and spicy makes the
choice explicit instead of silent. Without `rescale`, weights are taken
as *frequencies*: integer weights reproduce, exactly, the statistics of
the data with each row repeated `w` times — the reading of SPSS’s
`WEIGHT BY` and Stata’s `fweight`. With `rescale = TRUE`, weights are
first normalized to sum to the number of observations — the
*sampling-weights* reading, invariant to the scale of the weights, whose
SD matches Stata’s `aweight` and
[`survey::svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html).
For a survey weight like this one, `rescale = TRUE` is the reading you
want. The mean is the same under both (it is a ratio); the SD and the
`weighted_n` column differ. The exact formulas and the cross-software
correspondence are in the Weights section of
[`?table_continuous`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).

Two things are deliberately refused under weights rather than silently
wrong: the median confidence interval (an order-statistic interval with
no weighted version), and the group tests and effect sizes — a *t*-test
printed next to weighted descriptives would itself be unweighted. For
weighted group comparisons,
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
takes the same `weights` argument and fits them properly:

``` r

# Weighted descriptives by group: turn the test columns off
table_continuous(
  sochealth,
  select = bmi,
  by = sex,
  weights = weight,
  rescale = TRUE,
  p_value = FALSE
)
# Weighted group comparison: the model-based tool
table_continuous_lm(sochealth, select = bmi, by = sex, weights = weight)
```

The standardized mean difference is the exception, and passes under
weights:

``` r

table_continuous(
  sochealth,
  select = c(age, bmi),
  by = sex,
  weights = weight,
  rescale = TRUE,
  smd = TRUE,
  p_value = FALSE
)
#> Descriptive statistics by sex
#> 
#>  Variable │ Group     M     SD     Min    Max   95% CI LL  95% CI UL   n  
#> ──────────┼───────────────────────────────────────────────────────────────
#>  age      │ Female  45.14  14.48  25.00  75.00    44.03      46.25    620 
#>           │ Male    44.86  13.96  25.00  75.00    43.68      46.03    580 
#> ╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  bmi      │ Female  25.51   3.75  16.00  38.90    25.22      25.80    616 
#>           │ Male    25.98   3.61  16.00  37.70    25.67      26.29    572 
#> 
#>  Variable │ Group    SMD  
#> ──────────┼───────────────
#>  age      │ Female   0.02 
#>           │ Male          
#> ╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  bmi      │ Female  -0.13 
#>           │ Male          
#> 
#> Note. Statistics weighted by weight. Missing values removed: bmi (12). SMD direction: Female − Male. The usual imbalance threshold is |SMD| > 0.1. CI = confidence interval; LL = lower limit; UL = upper limit; SMD = standardized mean difference.
```

It is a descriptive balance diagnostic with no *p* and no interval —
which is precisely why the balance literature substitutes it for the
test — and it is computed from the same weighted means and variances the
M and SD columns display, so it cannot be unweighted inference sitting
next to weighted descriptives. One consequence of that shared producer:
without `rescale`, the weighted SMD inherits the frequency reading and
is *not* invariant to the scale of the weights, exactly as the SD column
is not. With `rescale = TRUE` it is. For a survey weight, use
`rescale = TRUE`.

## Custom labels

By default,
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
labels each variable with its column name, which is why the tables above
read `bmi`. `labels = TRUE` uses the label attribute when one is present
(e.g. data imported with `haven`), and the column name otherwise. A
named character vector keyed by column name sets the labels you publish:
the listed columns take them, and the others follow
`options(spicy.var_labels)` (their name unless it is `TRUE`):

``` r

table_continuous(
  sochealth,
  select = c(wellbeing_score, life_sat_health),
  by = sex,
  labels = c(wellbeing_score = "Well-being score (0-100)")
)
#> Descriptive statistics by sex
#> 
#>  Variable                 │ Group     M     SD     Min    Max    95% CI LL 
#> ──────────────────────────┼────────────────────────────────────────────────
#>  Well-being score (0-100) │ Female  67.16  14.80  19.60  100.00    65.99   
#>                           │ Male    71.05  16.23  18.70  100.00    69.73   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_health          │ Female   3.51   1.25   1.00    5.00     3.41   
#>                           │ Male     3.59   1.25   1.00    5.00     3.49   
#> 
#>  Variable                 │ Group   95% CI UL   n     p   
#> ──────────────────────────┼───────────────────────────────
#>  Well-being score (0-100) │ Female    68.33    620  <.001 
#>                           │ Male      72.37    580        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_health          │ Female     3.61    616   .267 
#>                           │ Male       3.69    576        
#> 
#> Note. Missing values removed: life_sat_health (8). P-values: Welch t-test. CI = confidence interval; LL = lower limit; UL = upper limit.
```

[`var_labels()`](https://amaltawfik.github.io/spicy/reference/var_labels.md)
renames the same way while the other columns, and the `by` variable in
the title, keep their label attribute:

``` r

table_continuous(
  sochealth,
  select = c(wellbeing_score, life_sat_health),
  by = sex,
  labels = var_labels(wellbeing_score = "Well-being score (0-100)")
)
#> Descriptive statistics by Sex
#> 
#>  Variable                       │ Group     M     SD     Min    Max   
#> ────────────────────────────────┼─────────────────────────────────────
#>  Well-being score (0-100)       │ Female  67.16  14.80  19.60  100.00 
#>                                 │ Male    71.05  16.23  18.70  100.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Female   3.51   1.25   1.00    5.00 
#>                                 │ Male     3.59   1.25   1.00    5.00 
#> 
#>  Variable                       │ Group   95% CI LL  95% CI UL   n     p   
#> ────────────────────────────────┼──────────────────────────────────────────
#>  Well-being score (0-100)       │ Female    65.99      68.33    620  <.001 
#>                                 │ Male      69.73      72.37    580        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Female     3.41       3.61    616   .267 
#>                                 │ Male       3.49       3.69    576        
#> 
#> Note. Missing values removed: Satisfaction with health (1-5) (8). P-values: Welch t-test. CI = confidence interval; LL = lower limit; UL = upper limit.
```

## Display options

The printed ASCII table and every rendered output share the same
formatting vocabulary as
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md);
`align`, `p_digits`, and `decimal_mark` are also shared with
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md):

- `align = "decimal"` (default) aligns numeric columns on the decimal
  mark, matching SPSS / SAS / LaTeX `siunitx` conventions. `"center"`
  and `"right"` are the alternatives.
- `p_digits = 3` (default, the APA standard) drives both the displayed
  precision of the `p` column and the small-*p* threshold: with
  `p_digits = 4`, the chunk below shows `.0176` for BMI and `<.0001` for
  the well-being score.
- `show_n = FALSE` and `ci = FALSE` drop the corresponding columns (and
  the CI spanner / borders) structurally from every output; the
  underlying `n` and `ci_lower` / `ci_upper` are always present in
  `output = "long"` for downstream code.

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = sex,
  ci = FALSE,
  show_n = FALSE,
  p_digits = 4
)
#> Descriptive statistics by sex
#> 
#>  Variable          │ Group        M       SD       Min      Max        p     
#> ───────────────────┼─────────────────────────────────────────────────────────
#>  bmi               │ Female     25.69     3.78    16.00     38.90     .0176  
#>                    │ Male       26.20     3.64    16.00     37.70            
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score   │ Female     67.16    14.80    19.60    100.00    <.0001  
#>                    │ Male       71.05    16.23    18.70    100.00            
#> 
#> Note. Missing values removed: bmi (12). P-values: Welch t-test.
```

## Tidying for downstream pipelines

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
returns an object that can be coerced to a plain `data.frame` / `tbl_df`
(the displayed table, as with `output = "data.frame"`) or piped into
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) /
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
for any downstream tidyverse stats workflow:

``` r

out <- table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = sex
)

# Long descriptive rows: one per (variable x group) with broom-style
# columns (variable, label, group, .group_role, estimate = mean,
# std.error, conf.low / conf.high, n, weighted_n, median, q1, q3, min,
# max, sd).
broom::tidy(out)
#> # A tibble: 4 × 16
#>   variable   label group .group_role estimate std.error conf.low conf.high     n
#>   <chr>      <chr> <chr> <chr>          <dbl>     <dbl>    <dbl>     <dbl> <int>
#> 1 bmi        bmi   Fema… group           25.7     0.152     25.4      26.0   616
#> 2 bmi        bmi   Male  group           26.2     0.152     25.9      26.5   572
#> 3 wellbeing… well… Fema… group           67.2     0.594     66.0      68.3   620
#> 4 wellbeing… well… Male  group           71.0     0.674     69.7      72.4   580
#> # ℹ 7 more variables: weighted_n <dbl>, median <dbl>, q1 <dbl>, q3 <dbl>,
#> #   min <dbl>, max <dbl>, sd <dbl>

# One row per variable with the omnibus test + effect-size summary
# (test_type, statistic, df, df.residual, p.value, es_type, es_value,
# es_ci_lower / es_ci_upper, smd_type, smd_value, nobs).
broom::glance(out)
#> # A tibble: 2 × 14
#>   variable  label test_type statistic    df df.residual p.value es_type es_value
#>   <chr>     <chr> <chr>         <dbl> <dbl>       <dbl>   <dbl> <chr>      <dbl>
#> 1 bmi       bmi   welch_t       -2.38 1184.          NA 1.76e-2 NA            NA
#> 2 wellbein… well… welch_t       -4.33 1169.          NA 1.65e-5 NA            NA
#> # ℹ 5 more variables: es_ci_lower <dbl>, es_ci_upper <dbl>, smd_type <chr>,
#> #   smd_value <dbl>, nobs <int>

# Or the displayed table as a plain data.frame
head(as.data.frame(out))
#>          variable           label  group .row_role        M        SD  Min
#> 1             bmi             bmi Female     group 25.68506  3.781113 16.0
#> 2             bmi             bmi   Male     group 26.19685  3.638092 16.0
#> 3 wellbeing_score wellbeing_score Female     group 67.16194 14.798488 19.6
#> 4 wellbeing_score wellbeing_score   Male     group 71.04879 16.227304 18.7
#>     Max 95% CI LL 95% CI UL   n            p
#> 1  38.9  25.38588  25.98425 616 1.760093e-02
#> 2  37.7  25.89808  26.49563 572           NA
#> 3 100.0  65.99480  68.32907 620 1.647005e-05
#> 4 100.0  69.72540  72.37219 580           NA
```

## Output formats

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
supports the same reporting-oriented outputs as
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md):

| `output` value | Returned object |
|:---|:---|
| `"default"` | Styled ASCII console table |
| `"data.frame"` | Plain `data.frame`: the displayed table, with its numbers |
| `"long"` | Plain `data.frame`: every statistic, one row per variable and group |
| `"tinytable"` | Formatted tinytable |
| `"gt"` | Formatted gt table |
| `"flextable"` | Formatted flextable |
| `"excel"` | Written `.xlsx` file |
| `"clipboard"` | Copied text table |
| `"word"` | Written `.docx` file |

`output = "gt"` produces a formatted gt table with APA-style borders and
column spanners:

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  output = "gt"
)
```

[TABLE]

*Note.* Missing values removed: bmi (12), life_sat_health (8). P-values:
Welch one-way ANOVA. CI = confidence interval; LL = lower limit; UL =
upper limit.

`output = "tinytable"` works well in Quarto and R Markdown documents:

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  output = "tinytable"
)
```

| Variable | Group | M | SD | Min | Max | 95% CI |  | n | p |
|----|----|----|----|----|----|----|----|----|----|
|  |  |  |  |  |  | LL | UL |  |  |
| bmi | Lower secondary | 28.09 |  3.47 | 18.20 |  38.90 | 27.66 | 28.51 | 260 | \<.001 |
|  | Upper secondary | 26.02 |  3.43 | 16.00 |  37.10 | 25.73 | 26.31 | 534 |       |
|  | Tertiary | 24.39 |  3.52 | 16.00 |  33.00 | 24.04 | 24.74 | 394 |       |
| wellbeing_score | Lower secondary | 57.22 | 15.44 | 18.70 |  97.90 | 55.33 | 59.10 | 261 | \<.001 |
|  | Upper secondary | 68.97 | 13.62 | 26.70 | 100.00 | 67.82 | 70.12 | 539 |       |
|  | Tertiary | 76.85 | 13.23 | 40.40 | 100.00 | 75.55 | 78.15 | 400 |       |
| life_sat_health | Lower secondary |  2.71 |  1.20 |  1.00 |   5.00 |  2.57 |  2.86 | 259 | \<.001 |
|  | Upper secondary |  3.53 |  1.19 |  1.00 |   5.00 |  3.43 |  3.63 | 534 |       |
|  | Tertiary |  4.11 |  1.04 |  1.00 |   5.00 |  4.01 |  4.21 | 399 |       |
| *Note.* Missing values removed: bmi (12), life_sat_health (8). P-values: Welch one-way ANOVA. CI = confidence interval; LL = lower limit; UL = upper limit. |  |  |  |  |  |  |  |  |  |

Descriptive statistics by education {#tinytable_17x18ugxkltggwgxbyw6
.table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

## Export to Excel or Word

Use the same function to export the table directly:

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  output = "excel",
  excel_path = "table_continuous.xlsx"
)

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  output = "word",
  word_path = "table_continuous.docx"
)
```

## See also

- See [*Categorical summary
  tables*](https://amaltawfik.github.io/spicy/articles/table-categorical.md)
  for grouped tables of categorical variables.
- See [*One outcome across several
  groupings*](https://amaltawfik.github.io/spicy/articles/table-outcome.md)
  for the transposed shape: ONE continuous outcome described across the
  levels of SEVERAL groupings, one block of rows per grouping. Several
  outcomes across one grouping is this article; one outcome across one
  or more groupings is that one.
- See [*Model-based continuous summary
  tables*](https://amaltawfik.github.io/spicy/articles/table-continuous-lm.md)
  for model-based continuous summary tables with robust standard errors
  or case weights.
- See [*Publication-ready regression
  tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
  for full regression tables from one or more fitted models (APA Table
  3).
- See [*Summary tables for
  reporting*](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md)
  for a cross-function reporting workflow that ties the summary-table
  functions together along the APA Table 1 / 2 / 3 sequence.
