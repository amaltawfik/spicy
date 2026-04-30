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
#>  Variable                       │ 95% CI LL  95% CI UL    p     R²    n   
#> ────────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100)  │    2.13      5.64     <.001  0.02  1200 
#>  Body mass index                │    0.09      0.93      .018  0.00  1188 
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
#>  Variable                      │ 95% CI LL  95% CI UL   t      p     R²    n   
#> ───────────────────────────────┼───────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   2.12       5.65     4.32  <.001  0.02  1200 
#>  Body mass index               │   0.09       0.93     2.38   .018  0.00  1188
```

The `HC*` family is computed via \[sandwich::vcovHC()\] and includes
`"HC0"`, `"HC1"`, `"HC2"`, `"HC3"` (default for small / moderate
samples), `"HC4"`, `"HC4m"`, and `"HC5"`.

## Cluster-robust standard errors

When observations are not independent (repeated measurements per
individual, students nested in classes, panel data), classical and `HC*`
standard errors are biased downward. Use the `CR*` family together with
`cluster = id_var` to get cluster-robust inference, dispatched to
`clubSandwich`:

``` r

table_continuous_lm(
  sleep,
  select = extra,
  by = group,
  vcov = "CR2",
  cluster = ID,
  statistic = TRUE
)
#> Continuous outcomes by group
#> 
#>  Variable │ M (1)  M (2)  Δ (2 - 1)  95% CI LL  95% CI UL  t(9)   p     R²   n  
#> ──────────┼─────────────────────────────────────────────────────────────────────
#>  extra    │ 0.75   2.33     1.58       0.70       2.46     4.06  .003  0.16  20
```

`"CR2"` is the recommended default (Bell & McCaffrey 2002; Pustejovsky &
Tipton 2018). It produces fractional Satterthwaite degrees of freedom,
rendered in the displayed test header as e.g. `t(8.7)` or `F(2, 12.4)`.
`"CR1"` matches Stata’s `vce(cluster id)` default.

## Bootstrap and jackknife

For situations where the residual distribution is non-standard or the
sample is small, `vcov = "bootstrap"` and `vcov = "jackknife"` provide
resampling-based variance estimators in pure base R (no dependency
added):

``` r

table_continuous_lm(
  sochealth,
  select = wellbeing_score,
  by = sex,
  vcov = "bootstrap",
  boot_n = 1000  # default
)
```

When `cluster` is supplied, bootstrap switches to a cluster bootstrap
(Cameron, Gelbach & Miller 2008) and jackknife to leave-one-cluster-out
(Quenouille 1956). Both estimators use asymptotic inference: `z` for
single-coefficient contrasts and `chi^2(q)` for the global Wald test on
`k > 2` categorical predictors, rendered in the displayed test header.

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
#>  Variable                      │ M (Tertiary)    p     R²    n    Weighted n 
#> ───────────────────────────────┼─────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │    66.52      <.001  0.19  1200   1196.47   
#>  Body mass index               │    26.16      <.001  0.13  1188   1183.32
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
#>  Variable                      │  B    95% CI LL  95% CI UL    p     R²    n   
#> ───────────────────────────────┼───────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │ 0.04    -0.02      0.10      .176  0.00  1200 
#>  Body mass index               │ 0.04     0.02      0.05     <.001  0.02  1188
```

When you need the underlying returned data for further processing, use
`output = "data.frame"` for the wide raw table or `output = "long"` for
the analytic long table.

## Effect sizes

[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
supports four effect sizes via the `effect_size` argument. All are
derived from the **same fitted model** as the displayed coefficients and
`R²`, so the table stays internally consistent — and all are **invariant
to `vcov`** (the choice of classical or `HC*` standard errors changes
the contrast SE, CI, and test statistic but not the standardized effect
size itself).

Cohen’s *d* and Hedges’ *g* are the conventions for two-group
comparisons (Cohen 1988; Hedges and Olkin 1985; APA 2020) and require
`by` to have exactly two non-empty levels:

``` r

table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = smoking,
  effect_size = "d"
)
#> Continuous outcomes by Current smoker
#> 
#>  Variable                      │ M (No)  M (Yes)  Δ (Yes - No)  95% CI LL 
#> ───────────────────────────────┼──────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │ 69.36    67.65      -1.72        -3.91   
#>  Body mass index               │ 25.96    25.93      -0.03        -0.55   
#> 
#>  Variable                      │ 95% CI UL   p     R²     d     n   
#> ───────────────────────────────┼────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   0.47     .124  0.00  -0.11  1175 
#>  Body mass index               │   0.49     .905  0.00  -0.01  1163
```

Hedges’ *g* applies the small-sample correction
`J = 1 - 3/(4·df_resid - 1)`. It is generally preferred over raw *d* in
published reports (Goulet-Pelletier and Cousineau 2018):

``` r

table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = smoking,
  effect_size = "g"
)
#> Continuous outcomes by Current smoker
#> 
#>  Variable                      │ M (No)  M (Yes)  Δ (Yes - No)  95% CI LL 
#> ───────────────────────────────┼──────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │ 69.36    67.65      -1.72        -3.91   
#>  Body mass index               │ 25.96    25.93      -0.03        -0.55   
#> 
#>  Variable                      │ 95% CI UL   p     R²     g     n   
#> ───────────────────────────────┼────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   0.47     .124  0.00  -0.11  1175 
#>  Body mass index               │   0.49     .905  0.00  -0.01  1163
```

For categorical predictors with three or more levels (or numeric
predictors), use Hays’ `omega²` for a less biased estimate of the
population variance explained (Hays 1963; Olejnik and Algina 2003;
Lakens 2013):

``` r

table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = education,
  effect_size = "omega2"
)
#> Continuous outcomes by Highest education level
#> 
#>  Variable                      │ M (Lower secondary)  M (Upper secondary) 
#> ───────────────────────────────┼──────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │        67.68                81.56        
#>  Body mass index               │        26.17                23.55        
#> 
#>  Variable                      │ M (Tertiary)    p     R²    ω²    n   
#> ───────────────────────────────┼───────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │    66.10      <.001  0.21  0.21  1200 
#>  Body mass index               │    26.35      <.001  0.13  0.13  1188
```

Cohen’s `f²` (= `R² / (1 - R²)`) is the effect size familiar from
power-analysis frameworks (e.g. G\*Power) and is defined for any
predictor type:

``` r

table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = age,
  effect_size = "f2"
)
#> Continuous outcomes by Age (years)
#> 
#>  Variable                      │  B    95% CI LL  95% CI UL    p     R²    f²  
#> ───────────────────────────────┼───────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │ 0.04    -0.02      0.10      .177  0.00  0.00 
#>  Body mass index               │ 0.04     0.02      0.05     <.001  0.02  0.02 
#> 
#>  Variable                      │  n   
#> ───────────────────────────────┼──────
#>  WHO-5 wellbeing index (0-100) │ 1200 
#>  Body mass index               │ 1188
```

## Confidence intervals for effect sizes

Setting `effect_size_ci = TRUE` adds a confidence interval for the
effect size. The implementation uses the modern noncentral-distribution
inversion approach — the consensus standard in commercial statistical
software (Stata `esize` / `estat esize`, SAS `PROC TTEST` and
`PROC GLM EFFECTSIZE`) and in mainstream R packages (`effectsize`,
`MOTE`, `TOSTER`, `effsize`):

- Noncentral *t* inversion for `"d"` and `"g"` (Steiger and Fouladi
  1997; Goulet-Pelletier and Cousineau 2018; Cousineau and
  Goulet-Pelletier 2021), which has nominal coverage across sample sizes
  — unlike the older Hedges-Olkin normal approximation that is biased
  for small samples.
- Noncentral *F* inversion for `"omega2"` and `"f2"` (Steiger 2004;
  Smithson 2003).

In the printed and rendered outputs, the effect size is displayed with
the bracketed CI (article-ready):

``` r

table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = smoking,
  effect_size = "g",
  effect_size_ci = TRUE
)
#> Continuous outcomes by Current smoker
#> 
#>  Variable                      │ M (No)  M (Yes)  Δ (Yes - No)  95% CI LL 
#> ───────────────────────────────┼──────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │ 69.36    67.65      -1.72        -3.91   
#>  Body mass index               │ 25.96    25.93      -0.03        -0.55   
#> 
#>  Variable                      │ 95% CI UL   p     R²            g          
#> ───────────────────────────────┼────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   0.47     .124  0.00  -0.11 [-0.25, 0.03] 
#>  Body mass index               │   0.49     .905  0.00  -0.01 [-0.15, 0.13] 
#> 
#>  Variable                      │  n   
#> ───────────────────────────────┼──────
#>  WHO-5 wellbeing index (0-100) │ 1175 
#>  Body mass index               │ 1163
```

The CI level follows `ci_level` (default `0.95`). For programmatic
access, the wide raw output exposes separate numeric columns, and the
long output always includes them:

``` r

table_continuous_lm(
  sochealth,
  select = wellbeing_score,
  by = smoking,
  effect_size = "g",
  effect_size_ci = TRUE,
  output = "data.frame"
)
#>                                      Variable   M (No)  M (Yes) Δ (Yes - No)
#> wellbeing_score WHO-5 wellbeing index (0-100) 69.36317 67.64538    -1.717793
#>                 95% CI LL 95% CI UL         p          R²          g
#> wellbeing_score -3.906437 0.4708504 0.1238546 0.002017481 -0.1098571
#>                 effect_size_ci_lower effect_size_ci_upper    n
#> wellbeing_score           -0.2497291           0.03006164 1175
```

``` r

out <- table_continuous_lm(
  sochealth,
  select = wellbeing_score,
  by = smoking,
  effect_size = "g",
  effect_size_ci = TRUE,
  output = "long"
)
out[, c("variable", "es_type", "es_value", "es_ci_lower", "es_ci_upper")]
#>          variable es_type   es_value es_ci_lower es_ci_upper
#> 1 wellbeing_score       g -0.1098571  -0.2497291  0.03006164
#> 2 wellbeing_score    <NA>         NA          NA          NA
```

When `weights` is supplied, all four effect sizes (and their CIs) are
computed from the weighted least-squares fit, keeping them consistent
with the weighted contrast and its CI (DuMouchel and Duncan 1983). This
is the right convention for case-weighted reporting; for
propensity-score balance assessment (Austin and Stuart 2015) or
complex-survey designs, dedicated packages (`cobalt::bal.tab()` and
`survey`) are more appropriate.

## Article-style polish

Pretty outcome labels and a comma decimal separator (useful for European
reporting):

``` r

table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  labels = c(
    wellbeing_score = "WHO-5 wellbeing (0-100)",
    bmi = "Body-mass index (kg/m²)"
  ),
  effect_size = "g",
  effect_size_ci = TRUE,
  decimal_mark = ","
)
#> Continuous outcomes by Sex
#> 
#>  Variable                │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ─────────────────────────┼────────────────────────────────────────────────────
#>  WHO-5 wellbeing (0-100) │   67,16      71,05          3,89           2,13    
#>  Body-mass index (kg/m²) │   25,69      26,20          0,51           0,09    
#> 
#>  Variable                │ 95% CI UL    p     R²           g           n   
#> ─────────────────────────┼─────────────────────────────────────────────────
#>  WHO-5 wellbeing (0-100) │   5,64     <,001  0,02  0,25 [0,14; 0,36]  1200 
#>  Body-mass index (kg/m²) │   0,93      ,018  0,00  0,14 [0,02; 0,25]  1188
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

## Tidying for downstream pipelines

[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
returns an object that can be coerced to a plain `data.frame` / `tbl_df`
(stripping the spicy formatting attributes) or piped into
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) /
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
for use with `gtsummary`, `modelsummary`, `parameters`, or any other
tidyverse-stats workflow:

``` r

out <- table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  effect_size = "g",
  effect_size_ci = TRUE
)
#> Continuous outcomes by Sex
#> 
#>  Variable                      │ M (Female)  M (Male)  Δ (Male - Female) 
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   67.16      71.05          3.89        
#>  Body mass index               │   25.69      26.20          0.51        
#> 
#>  Variable                      │ 95% CI LL  95% CI UL    p     R²  
#> ───────────────────────────────┼───────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   2.13       5.64     <.001  0.02 
#>  Body mass index               │   0.09       0.93      .018  0.00 
#> 
#>  Variable                      │         g           n   
#> ───────────────────────────────┼─────────────────────────
#>  WHO-5 wellbeing index (0-100) │ 0.25 [0.14, 0.36]  1200 
#>  Body mass index               │ 0.14 [0.02, 0.25]  1188

# One row per estimated parameter: emmean per level, contrast for
# binary predictors, slope for numeric predictors.
broom::tidy(out)
#> # A tibble: 6 × 10
#>   outcome        label term  estimate_type estimate std.error conf.low conf.high
#>   <chr>          <chr> <chr> <chr>            <dbl>     <dbl>    <dbl>     <dbl>
#> 1 wellbeing_sco… WHO-… Fema… emmean          67.2       0.623  65.9       68.4  
#> 2 wellbeing_sco… WHO-… Male  emmean          71.0       0.644  69.8       72.3  
#> 3 wellbeing_sco… WHO-… Male… difference       3.89      0.896   2.13       5.64 
#> 4 bmi            Body… Fema… emmean          25.7       0.150  25.4       26.0  
#> 5 bmi            Body… Male  emmean          26.2       0.155  25.9       26.5  
#> 6 bmi            Body… Male… difference       0.512     0.216   0.0888     0.935
#> # ℹ 2 more variables: statistic <dbl>, p.value <dbl>

# One row per outcome with model-level statistics: r.squared,
# adj.r.squared, F / t, df, p.value, nobs, weighted_n, plus the
# effect-size summary.
broom::glance(out)
#> # A tibble: 2 × 16
#>   outcome     label predictor_type test_type statistic    df df.residual p.value
#>   <chr>       <chr> <chr>          <chr>         <dbl> <int>       <int>   <dbl>
#> 1 wellbeing_… WHO-… categorical    F             18.8      1        1198 1.55e-5
#> 2 bmi         Body… categorical    F              5.64     1        1186 1.78e-2
#> # ℹ 8 more variables: r.squared <dbl>, adj.r.squared <dbl>, es_type <chr>,
#> #   es_value <dbl>, es_ci_lower <dbl>, es_ci_upper <dbl>, nobs <int>,
#> #   weighted_n <dbl>
```

## See also

- See
  [`vignette("table-continuous", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/table-continuous.md)
  for descriptive continuous summary tables with classical
  group-comparison tests.
- See
  [`vignette("summary-tables-reporting", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md)
  for a cross-function reporting workflow using the summary-table
  helpers.
