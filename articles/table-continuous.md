# Continuous summary tables with table_continuous()

``` r
library(spicy)
```

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
summarizes continuous variables either overall or by a categorical
grouping variable. It is designed for readable summary tables in the
console and for publication-ready outputs in rendered documents. When
`by` is supplied, it can also add group-comparison tests, test
statistics, and effect sizes.

## Basic usage

Use `select` to choose the continuous variables you want to summarize:

``` r
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health)
)
#> Descriptive statistics
#> 
#>  Variable                       │   M     SD     Min    Max    95% CI LL 
#> ────────────────────────────────┼────────────────────────────────────────
#>  Body mass index                │ 25.93  3.72   16.00  38.90     25.72   
#>  WHO-5 wellbeing index (0-100)  │ 69.04  15.62  18.70  100.00    68.16   
#>  Satisfaction with health (1-5) │ 3.55   1.25   1.00    5.00     3.48    
#> 
#>  Variable                       │ 95% CI UL     n 
#> ────────────────────────────────┼─────────────────
#>  Body mass index                │   26.14    1188 
#>  WHO-5 wellbeing index (0-100)  │   69.93    1200 
#>  Satisfaction with health (1-5) │   3.62     1192
```

If you omit `select`,
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
scans the data frame and keeps numeric columns:

``` r
table_continuous(sochealth)
#> Descriptive statistics
#> 
#>  Variable                                   │    M       SD       Min   
#> ────────────────────────────────────────────┼───────────────────────────
#>  Age (years)                                │  49.26    14.70    25.00  
#>  Monthly household income (CHF)             │ 3833.00  1394.58  1000.00 
#>  WHO-5 wellbeing index (0-100)              │  69.04    15.62    18.70  
#>  Body mass index                            │  25.93    3.72     16.00  
#>  Political position (0 = left, 10 = right)  │  5.48     2.03     0.00   
#>  Satisfaction with health (1-5)             │  3.55     1.25     1.00   
#>  Satisfaction with work (1-5)               │  3.38     1.18     1.00   
#>  Satisfaction with relationships (1-5)      │  3.72     1.10     1.00   
#>  Satisfaction with standard of living (1-5) │  3.40     1.16     1.00   
#>  Survey design weight                       │  1.00     0.41     0.29   
#> 
#>  Variable                                   │   Max    95% CI LL  95% CI UL 
#> ────────────────────────────────────────────┼───────────────────────────────
#>  Age (years)                                │  75.00     48.43      50.10   
#>  Monthly household income (CHF)             │ 7388.00   3754.01    3911.98  
#>  WHO-5 wellbeing index (0-100)              │ 100.00     68.16      69.93   
#>  Body mass index                            │  38.90     25.72      26.14   
#>  Political position (0 = left, 10 = right)  │  10.00     5.36       5.60    
#>  Satisfaction with health (1-5)             │  5.00      3.48       3.62    
#>  Satisfaction with work (1-5)               │  5.00      3.31       3.45    
#>  Satisfaction with relationships (1-5)      │  5.00      3.66       3.79    
#>  Satisfaction with standard of living (1-5) │  5.00      3.33       3.46    
#>  Survey design weight                       │  3.45      0.97       1.02    
#> 
#>  Variable                                   │    n 
#> ────────────────────────────────────────────┼──────
#>  Age (years)                                │ 1200 
#>  Monthly household income (CHF)             │ 1200 
#>  WHO-5 wellbeing index (0-100)              │ 1200 
#>  Body mass index                            │ 1188 
#>  Political position (0 = left, 10 = right)  │ 1185 
#>  Satisfaction with health (1-5)             │ 1192 
#>  Satisfaction with work (1-5)               │ 1192 
#>  Satisfaction with relationships (1-5)      │ 1192 
#>  Satisfaction with standard of living (1-5) │ 1192 
#>  Survey design weight                       │ 1200
```

## Grouped summaries

Add `by` to summarize the same variables across categories:

``` r
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education
)
#> Descriptive statistics
#> 
#>  Variable                       │ Group              M     SD     Min    Max   
#> ────────────────────────────────┼──────────────────────────────────────────────
#>  Body mass index                │ Lower secondary  28.09  3.47   18.20  38.90  
#>                                 │ Upper secondary  26.02  3.43   16.00  37.10  
#>                                 │ Tertiary         24.39  3.52   16.00  33.00  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100)  │ Lower secondary  57.22  15.44  18.70  97.90  
#>                                 │ Upper secondary  68.97  13.62  26.70  100.00 
#>                                 │ Tertiary         76.85  13.23  40.40  100.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary  2.71   1.20   1.00    5.00  
#>                                 │ Upper secondary  3.53   1.19   1.00    5.00  
#>                                 │ Tertiary         4.11   1.04   1.00    5.00  
#> 
#>  Variable                       │ Group            95% CI LL  95% CI UL    n 
#> ────────────────────────────────┼────────────────────────────────────────────
#>  Body mass index                │ Lower secondary    27.66      28.51    260 
#>                                 │ Upper secondary    25.73      26.31    534 
#>                                 │ Tertiary           24.04      24.74    394 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100)  │ Lower secondary    55.33      59.10    261 
#>                                 │ Upper secondary    67.82      70.12    539 
#>                                 │ Tertiary           75.55      78.15    400 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary    2.57       2.86     259 
#>                                 │ Upper secondary    3.43       3.63     534 
#>                                 │ Tertiary           4.01       4.21     399
```

This is the main pattern for reporting continuous variables across
groups such as education, sex, treatment arm, or survey wave.

## Add group-comparison results

Grouped tables can include inferential results directly in the same
table. The default test is Welch’s test:

``` r
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  p_value = TRUE,
  statistic = TRUE,
  effect_size_ci = TRUE
)
#> Warning: `effect_size_ci` implies `effect_size = TRUE`.
#> Descriptive statistics
#> 
#>  Variable                       │ Group              M     SD     Min    Max   
#> ────────────────────────────────┼──────────────────────────────────────────────
#>  Body mass index                │ Lower secondary  28.09  3.47   18.20  38.90  
#>                                 │ Upper secondary  26.02  3.43   16.00  37.10  
#>                                 │ Tertiary         24.39  3.52   16.00  33.00  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100)  │ Lower secondary  57.22  15.44  18.70  97.90  
#>                                 │ Upper secondary  68.97  13.62  26.70  100.00 
#>                                 │ Tertiary         76.85  13.23  40.40  100.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary  2.71   1.20   1.00    5.00  
#>                                 │ Upper secondary  3.53   1.19   1.00    5.00  
#>                                 │ Tertiary         4.11   1.04   1.00    5.00  
#> 
#>  Variable                       │ Group            95% CI LL  95% CI UL    n 
#> ────────────────────────────────┼────────────────────────────────────────────
#>  Body mass index                │ Lower secondary    27.66      28.51    260 
#>                                 │ Upper secondary    25.73      26.31    534 
#>                                 │ Tertiary           24.04      24.74    394 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100)  │ Lower secondary    55.33      59.10    261 
#>                                 │ Upper secondary    67.82      70.12    539 
#>                                 │ Tertiary           75.55      78.15    400 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary    2.57       2.86     259 
#>                                 │ Upper secondary    3.43       3.63     534 
#>                                 │ Tertiary           4.01       4.21     399 
#> 
#>  Variable                       │ Group                    Test          
#> ────────────────────────────────┼────────────────────────────────────────
#>  Body mass index                │ Lower secondary  F(2, 654.48) = 87.96  
#>                                 │ Upper secondary                        
#>                                 │ Tertiary                               
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100)  │ Lower secondary  F(2, 638.59) = 144.35 
#>                                 │ Upper secondary                        
#>                                 │ Tertiary                               
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary  F(2, 652.08) = 118.74 
#>                                 │ Upper secondary                        
#>                                 │ Tertiary                               
#> 
#>  Variable                       │ Group                 p 
#> ────────────────────────────────┼─────────────────────────
#>  Body mass index                │ Lower secondary  < .001 
#>                                 │ Upper secondary         
#>                                 │ Tertiary                
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100)  │ Lower secondary  < .001 
#>                                 │ Upper secondary         
#>                                 │ Tertiary                
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary  < .001 
#>                                 │ Upper secondary         
#>                                 │ Tertiary                
#> 
#>  Variable                       │ Group                      ES           
#> ────────────────────────────────┼─────────────────────────────────────────
#>  Body mass index                │ Lower secondary  η² = 0.13 [0.10, 0.17] 
#>                                 │ Upper secondary                         
#>                                 │ Tertiary                                
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100)  │ Lower secondary  η² = 0.21 [0.17, 0.25] 
#>                                 │ Upper secondary                         
#>                                 │ Tertiary                                
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary  η² = 0.16 [0.13, 0.20] 
#>                                 │ Upper secondary                         
#>                                 │ Tertiary
```

Use `test = "student"` for equal-variance parametric tests or
`test = "nonparametric"` for rank-based comparisons:

``` r
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education,
  test = "nonparametric",
  p_value = TRUE,
  statistic = TRUE,
  effect_size = TRUE
)
#> Descriptive statistics
#> 
#>  Variable                      │ Group              M     SD     Min    Max   
#> ───────────────────────────────┼──────────────────────────────────────────────
#>  Body mass index               │ Lower secondary  28.09  3.47   18.20  38.90  
#>                                │ Upper secondary  26.02  3.43   16.00  37.10  
#>                                │ Tertiary         24.39  3.52   16.00  33.00  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100) │ Lower secondary  57.22  15.44  18.70  97.90  
#>                                │ Upper secondary  68.97  13.62  26.70  100.00 
#>                                │ Tertiary         76.85  13.23  40.40  100.00 
#> 
#>  Variable                      │ Group            95% CI LL  95% CI UL    n 
#> ───────────────────────────────┼────────────────────────────────────────────
#>  Body mass index               │ Lower secondary    27.66      28.51    260 
#>                                │ Upper secondary    25.73      26.31    534 
#>                                │ Tertiary           24.04      24.74    394 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100) │ Lower secondary    55.33      59.10    261 
#>                                │ Upper secondary    67.82      70.12    539 
#>                                │ Tertiary           75.55      78.15    400 
#> 
#>  Variable                      │ Group                Test            p 
#> ───────────────────────────────┼────────────────────────────────────────
#>  Body mass index               │ Lower secondary  H(2) = 144.63  < .001 
#>                                │ Upper secondary                        
#>                                │ Tertiary                               
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100) │ Lower secondary  H(2) = 233.53  < .001 
#>                                │ Upper secondary                        
#>                                │ Tertiary                               
#> 
#>  Variable                      │ Group               ES     
#> ───────────────────────────────┼────────────────────────────
#>  Body mass index               │ Lower secondary  ε² = 0.12 
#>                                │ Upper secondary            
#>                                │ Tertiary                   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100) │ Lower secondary  ε² = 0.19 
#>                                │ Upper secondary            
#>                                │ Tertiary
```

When you need the underlying columns for further processing, set
`styled = FALSE`:

``` r
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education,
  p_value = TRUE,
  statistic = TRUE,
  effect_size = TRUE,
  styled = FALSE
)
#>          variable                         label           group     mean
#> 1             bmi               Body mass index Lower secondary 28.08731
#> 2             bmi               Body mass index Upper secondary 26.01891
#> 3             bmi               Body mass index        Tertiary 24.39036
#> 4 wellbeing_score WHO-5 wellbeing index (0-100) Lower secondary 57.21571
#> 5 wellbeing_score WHO-5 wellbeing index (0-100) Upper secondary 68.96920
#> 6 wellbeing_score WHO-5 wellbeing index (0-100)        Tertiary 76.85250
#>          sd  min   max ci_lower ci_upper   n   test_type statistic df1      df2
#> 1  3.471744 18.2  38.9 27.66333 28.51129 260 welch_anova  87.95902   2 654.4758
#> 2  3.434736 16.0  37.1 25.72693 26.31090 534        <NA>        NA  NA       NA
#> 3  3.520150 16.0  33.0 24.04170 24.73901 394        <NA>        NA  NA       NA
#> 4 15.444587 18.7  97.9 55.33323 59.09819 261 welch_anova 144.35083   2 638.5873
#> 5 13.621193 26.7 100.0 67.81669 70.12172 539        <NA>        NA  NA       NA
#> 6 13.226818 40.4 100.0 75.55235 78.15265 400        <NA>        NA  NA       NA
#>        p.value es_type  es_value es_ci_lower es_ci_upper
#> 1 1.467916e-34  eta_sq 0.1307679  0.09667861   0.1654516
#> 2           NA    <NA>        NA          NA          NA
#> 3           NA    <NA>        NA          NA          NA
#> 4 1.888362e-52  eta_sq 0.2081970  0.16901207   0.2461732
#> 5           NA    <NA>        NA          NA          NA
#> 6           NA    <NA>        NA          NA          NA
```

## Selecting variables

`select` supports tidyselect helpers:

``` r
table_continuous(
  sochealth,
  select = starts_with("life_sat"),
  by = sex
)
#> Descriptive statistics
#> 
#>  Variable                                   │ Group    M     SD   Min   Max  
#> ────────────────────────────────────────────┼────────────────────────────────
#>  Satisfaction with health (1-5)             │ Female  3.51  1.25  1.00  5.00 
#>                                             │ Male    3.59  1.25  1.00  5.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with work (1-5)               │ Female  3.32  1.17  1.00  5.00 
#>                                             │ Male    3.44  1.20  1.00  5.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with relationships (1-5)      │ Female  3.71  1.09  1.00  5.00 
#>                                             │ Male    3.74  1.10  1.00  5.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with standard of living (1-5) │ Female  3.37  1.16  1.00  5.00 
#>                                             │ Male    3.42  1.17  1.00  5.00 
#> 
#>  Variable                                   │ Group   95% CI LL  95% CI UL    n 
#> ────────────────────────────────────────────┼───────────────────────────────────
#>  Satisfaction with health (1-5)             │ Female    3.41       3.61     616 
#>                                             │ Male      3.49       3.69     576 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with work (1-5)               │ Female    3.23       3.41     615 
#>                                             │ Male      3.34       3.54     577 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with relationships (1-5)      │ Female    3.62       3.79     615 
#>                                             │ Male      3.65       3.83     577 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with standard of living (1-5) │ Female    3.28       3.46     615 
#>                                             │ Male      3.33       3.52     577
```

For more programmatic selection, set `regex = TRUE`:

``` r
table_continuous(
  sochealth,
  select = "^life_sat",
  regex = TRUE,
  by = education,
  styled = FALSE
)
#>                  variable                                      label
#> 1         life_sat_health             Satisfaction with health (1-5)
#> 2         life_sat_health             Satisfaction with health (1-5)
#> 3         life_sat_health             Satisfaction with health (1-5)
#> 4           life_sat_work               Satisfaction with work (1-5)
#> 5           life_sat_work               Satisfaction with work (1-5)
#> 6           life_sat_work               Satisfaction with work (1-5)
#> 7  life_sat_relationships      Satisfaction with relationships (1-5)
#> 8  life_sat_relationships      Satisfaction with relationships (1-5)
#> 9  life_sat_relationships      Satisfaction with relationships (1-5)
#> 10      life_sat_standard Satisfaction with standard of living (1-5)
#> 11      life_sat_standard Satisfaction with standard of living (1-5)
#> 12      life_sat_standard Satisfaction with standard of living (1-5)
#>              group     mean        sd min max ci_lower ci_upper   n
#> 1  Lower secondary 2.714286 1.2021575   1   5 2.567189 2.861382 259
#> 2  Upper secondary 3.533708 1.1853493   1   5 3.432943 3.634473 534
#> 3         Tertiary 4.110276 1.0432216   1   5 4.007602 4.212950 399
#> 4  Lower secondary 2.570881 1.1467994   1   5 2.431102 2.710660 261
#> 5  Upper secondary 3.422430 1.1037312   1   5 3.328691 3.516169 535
#> 6         Tertiary 3.851010 1.0314174   1   5 3.749112 3.952909 396
#> 7  Lower secondary 3.023077 1.2268891   1   5 2.873246 3.172908 260
#> 8  Upper secondary 3.743446 0.9645227   1   5 3.661453 3.825439 534
#> 9         Tertiary 4.158291 0.9322485   1   5 4.066423 4.250159 398
#> 10 Lower secondary 2.666667 1.1635489   1   5 2.524846 2.808487 261
#> 11 Upper secondary 3.387218 1.1065913   1   5 3.292970 3.481466 532
#> 12        Tertiary 3.887218 0.9588582   1   5 3.792847 3.981589 399
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
#> Descriptive statistics
#> 
#>  Variable                       │ Group     M     SD     Min    Max   
#> ────────────────────────────────┼─────────────────────────────────────
#>  Body mass index                │ Female  25.69  3.78   16.00  38.90  
#>                                 │ Male    26.20  3.64   16.00  37.70  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100)  │ Female  67.16  14.80  19.60  100.00 
#>                                 │ Male    71.05  16.23  18.70  100.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Female  3.51   1.25   1.00    5.00  
#>                                 │ Male    3.59   1.25   1.00    5.00  
#> 
#>  Variable                       │ Group   95% CI LL  95% CI UL    n 
#> ────────────────────────────────┼───────────────────────────────────
#>  Body mass index                │ Female    25.39      25.98    616 
#>                                 │ Male      25.90      26.50    572 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100)  │ Female    65.99      68.33    620 
#>                                 │ Male      69.73      72.37    580 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Female    3.41       3.61     616 
#>                                 │ Male      3.49       3.69     576
```

## Labels and output formats

Use `labels` to replace technical variable names with reporting labels:

``` r
pkgdown_dark_gt(
  table_continuous(
    sochealth,
    select = c(bmi, wellbeing_score, life_sat_health),
    by = education,
    labels = c(
      bmi = "Body mass index",
      wellbeing_score = "Well-being score",
      life_sat_health = "Satisfaction with health"
    ),
    output = "gt"
  )
)
```

[TABLE]

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
supports the same reporting-oriented outputs as
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md):

| `output` value | Returned object            |
|:---------------|:---------------------------|
| `"default"`    | Styled ASCII console table |
| `"tinytable"`  | Formatted tinytable        |
| `"gt"`         | Formatted gt table         |
| `"flextable"`  | Formatted flextable        |
| `"excel"`      | Written `.xlsx` file       |
| `"clipboard"`  | Copied text table          |
| `"word"`       | Written `.docx` file       |

For example, `tinytable` works well in Quarto and R Markdown documents:

``` r
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  output = "tinytable"
)
```

| Variable                       | Group           | M     | SD    | Min   | Max    | 95% CI |       | n   |
|--------------------------------|-----------------|-------|-------|-------|--------|--------|-------|-----|
|                                |                 |       |       |       |        | LL     | UL    |     |
| Body mass index                | Lower secondary | 28.09 | 3.47  | 18.20 | 38.90  | 27.66  | 28.51 | 260 |
|                                | Upper secondary | 26.02 | 3.43  | 16.00 | 37.10  | 25.73  | 26.31 | 534 |
|                                | Tertiary        | 24.39 | 3.52  | 16.00 | 33.00  | 24.04  | 24.74 | 394 |
| WHO-5 wellbeing index (0-100)  | Lower secondary | 57.22 | 15.44 | 18.70 | 97.90  | 55.33  | 59.10 | 261 |
|                                | Upper secondary | 68.97 | 13.62 | 26.70 | 100.00 | 67.82  | 70.12 | 539 |
|                                | Tertiary        | 76.85 | 13.23 | 40.40 | 100.00 | 75.55  | 78.15 | 400 |
| Satisfaction with health (1-5) | Lower secondary | 2.71  | 1.20  | 1.00  | 5.00   | 2.57   | 2.86  | 259 |
|                                | Upper secondary | 3.53  | 1.19  | 1.00  | 5.00   | 3.43   | 3.63  | 534 |
|                                | Tertiary        | 4.11  | 1.04  | 1.00  | 5.00   | 4.01   | 4.21  | 399 |

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

- See
  [`vignette("table-categorical", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/table-categorical.md)
  for grouped tables of categorical variables.
- See
  [`vignette("summary-tables-reporting", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md)
  for a cross-function reporting workflow using both summary-table
  helpers.
