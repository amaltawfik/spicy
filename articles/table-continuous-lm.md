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
  statistic = TRUE
)
#> Continuous outcomes by Sex
#> 
#>  Variable                      │ M (Female)  M (Male)  Δ (Male - Female) 
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   67.16      71.05          3.89        
#>  Body mass index               │   25.69      26.20          0.51        
#> 
#>  Variable                      │ 95% CI LL  95% CI UL   t        p   R²      n 
#> ───────────────────────────────┼───────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   2.12       5.65     4.32  <.001  0.02  1200 
#>  Body mass index               │   0.09       0.93     2.38   .018  0.00  1188
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
  show_weighted_n = TRUE
)
#> Continuous outcomes by Highest education level
#> 
#>  Variable                      │ M (Lower secondary)  M (Upper secondary) 
#> ───────────────────────────────┼──────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │        67.55                80.88        
#>  Body mass index               │        25.96                23.39        
#> 
#>  Variable                      │ M (Tertiary)      p   R²      n  Weighted n 
#> ───────────────────────────────┼─────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │    66.52      <.001  0.19  1200     1196.47 
#>  Body mass index               │    26.16      <.001  0.13  1188     1183.32
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
  vcov = "HC3"
)
#> Continuous outcomes by Age (years)
#> 
#>  Variable                      │  B    95% CI LL  95% CI UL      p   R²      n 
#> ───────────────────────────────┼───────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │ 0.04    -0.02      0.10      .176  0.00  1200 
#>  Body mass index               │ 0.04    0.02       0.05     <.001  0.02  1188
```

When you need the underlying returned data for further processing, use
`output = "data.frame"` for the wide raw table or `output = "long"` for
the analytic long table.

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
