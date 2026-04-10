# Model-based continuous summary tables in R

``` r
library(spicy)
```

[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
is the model-based companion to
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).
It fits one linear model per selected continuous outcome using
`lm(outcome ~ by, ...)`, then returns a compact reporting table. This
makes it the better choice when you want to stay in a linear-model
workflow, add heteroskedasticity-consistent standard errors, or apply
case weights.

## Basic usage

Use `select` for one or more continuous outcomes and `by` for the single
predictor:

``` r
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi, life_sat_health),
  by = sex
)
#> Continuous outcomes by Sex
#> 
#>  Variable                       │ M (Female)  M (Male)  Δ (Male - Female) 
#> ────────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100)  │   67.16      71.05          3.89        
#>  Body mass index                │   25.69      26.20          0.51        
#>  Satisfaction with health (1-5) │    3.51       3.59          0.08        
#> 
#>  Variable                       │ 95% CI LL  95% CI UL      p   R²      n 
#> ────────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100)  │   2.13       5.64     <.001  0.02  1200 
#>  Body mass index                │   0.09       0.93      .018  0.00  1188 
#>  Satisfaction with health (1-5) │   -0.06      0.22      .267  0.00  1192
```

For categorical predictors, the table reports estimated means by level.
When the predictor is dichotomous, it can also show a single mean
difference and confidence interval.

## Robust standard errors

Use `vcov = "HC*"` when you want heteroskedasticity-consistent standard
errors and tests:

``` r
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  vcov = "HC3",
  statistic = TRUE,
  output = "data.frame"
)
#>                                      Variable M (Female) M (Male)
#> wellbeing_score WHO-5 wellbeing index (0-100)   67.16194 71.04879
#> bmi                           Body mass index   25.68506 26.19685
#>                 Δ (Male - Female)  95% CI LL 95% CI UL        t            p
#> wellbeing_score         3.8868576 2.12265210 5.6510631 4.322515 1.670572e-05
#> bmi                     0.5117882 0.08904596 0.9345305 2.375233 1.769614e-02
#>                          R²    n
#> wellbeing_score 0.015475137 1200
#> bmi             0.004728908 1188
```

## Case weights

Use `weights` when you want weighted estimated means or slopes in the
same model-based table:

``` r
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = education,
  weights = weight,
  output = "data.frame"
)
#>                                      Variable M (Lower secondary)
#> wellbeing_score WHO-5 wellbeing index (0-100)            67.55044
#> bmi                           Body mass index            25.95611
#>                 M (Upper secondary) M (Tertiary)            p        R²    n
#> wellbeing_score            80.87817     66.51913 2.919348e-56 0.1923742 1200
#> bmi                        23.39124     26.15834 1.455685e-35 0.1266195 1188
```

This is often the most natural summary-table function when your
reporting workflow already relies on weighted linear models.

## Numeric predictors

If `by` is numeric,
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
reports the slope rather than group means:

``` r
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = age,
  vcov = "HC3",
  output = "long"
)
#>          variable                         label predictor_type predictor_label
#> 1 wellbeing_score WHO-5 wellbeing index (0-100)     continuous     Age (years)
#> 2             bmi               Body mass index     continuous     Age (years)
#>   level reference estimate_type emmean emmean_se emmean_ci_lower
#> 1  <NA>      <NA>         slope     NA        NA              NA
#> 2  <NA>      <NA>         slope     NA        NA              NA
#>   emmean_ci_upper   estimate estimate_se estimate_ci_lower estimate_ci_upper
#> 1              NA 0.04138096 0.030585451       -0.01862605        0.10138797
#> 2              NA 0.03508525 0.007455753        0.02045732        0.04971319
#>   test_type statistic df1  df2      p.value es_type    es_value          r2
#> 1         t  1.352962   1 1198 1.763230e-01      f2 0.001519732 0.001517426
#> 2         t  4.705796   1 1186 2.826189e-06      f2 0.019622500 0.019244867
#>         adj_r2    n sum_w
#> 1 0.0006839677 1200    NA
#> 2 0.0184179236 1188    NA
```

## Publication-ready output

The function supports the same output formats as the other summary-table
helpers, including `tinytable`, `gt`, `flextable`, `excel`, `word`, and
`clipboard`.

``` r
pkgdown_dark_gt(
  table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi, life_sat_health),
    by = sex,
    vcov = "HC3",
    statistic = TRUE,
    output = "gt"
  )
)
```

[TABLE]

## See also

- See
  [`vignette("table-continuous", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/table-continuous.md)
  for descriptive continuous summary tables with classical
  group-comparison tests.
- See
  [`vignette("summary-tables-reporting", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md)
  for a cross-function reporting workflow using the summary-table
  helpers.
