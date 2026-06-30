# Ordinal regression tables

``` r

library(spicy)
library(MASS)        # polr()
```

This vignette covers **ordinal regression** — models for an *ordered*
categorical response such as a Poor \< Fair \< Good \< Very good health
rating or a Likert agreement scale. The companion vignette
[*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
covers the shared mechanics (`vcov`, `ci_level`, output formats,
multi-model layouts, broom integration); here we focus on what is
*specific* to ordinal fits.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
supports both ordinal engines:

- **[`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html)** —
  proportional-odds cumulative logit (also probit / cloglog / cauchit
  via `method =`);
- **[`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html)** — the
  more flexible cumulative link model, with optional scale and
  partial-proportional-odds (nominal) effects.

The model used throughout is a self-rated-health rating regressed on
age, sex, smoking, and physical activity, using the bundled `sochealth`
survey data
([`?sochealth`](https://amaltawfik.github.io/spicy/reference/sochealth.md)).
`self_rated_health` is an ordered factor:

``` r

levels(sochealth$self_rated_health)
#> [1] "Poor"      "Fair"      "Good"      "Very good"
```

## The proportional-odds model in one paragraph

A cumulative logit model with \\K\\ ordered response categories
estimates **one slope per predictor** (shared across all \\K-1\\
cumulative splits — the *proportional-odds assumption*) plus \\K-1\\
ordered **thresholds** (cut-points). A positive slope means that higher
values of the predictor push the response toward *higher* categories.
There is deliberately **no per-category coefficient**: that is the whole
point of the proportional-odds restriction. The per-category structure
reappears only in the *marginal effects* (below), where a one-unit
change has a different effect on the probability of each category.

## Basic table

Pass a fitted [`polr()`](https://rdrr.io/pkg/MASS/man/polr.html) object.
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
renders the shared slope coefficients with a Wald-\\z\\ inference
regime, and prints the ordered thresholds in the footer (they are model
parameters, not predictor effects, so they do not belong in the
coefficient body):

``` r

fit <- polr(
  self_rated_health ~ age + sex + smoking + physical_activity,
  data = sochealth, Hess = TRUE
)
table_regression(fit)
#> Cumulative logit regression (proportional odds): self_rated_health
#> 
#>  Variable           │   B     SE       95% CI        p   
#> ────────────────────┼────────────────────────────────────
#>  age                │ -0.00  0.00  [-0.01,  0.01]   .831 
#>  sex:               │                                    
#>    Female (ref.)    │   –     –          –          –    
#>    Male             │  0.02  0.11  [-0.20,  0.23]   .874 
#>  smoking:           │                                    
#>    No (ref.)        │   –     –          –          –    
#>    Yes              │ -0.27  0.14  [-0.53, -0.00]   .047 
#>  physical_activity: │                                    
#>    No (ref.)        │   –     –          –          –    
#>    Yes              │  0.03  0.11  [-0.19,  0.24]   .794 
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: Poor|Fair = -2.98, Fair|Good = -1.02, Good|Very good = 1.04.
```

Reading the table:

- Each predictor has **one** coefficient row (proportional odds); factor
  predictors are grouped under their parent variable with the reference
  level carrying `(ref.)` and an em-dash.
- Inference is **Wald-\\z\\** (`df = Inf`): ordinal MLE has no residual
  degrees of freedom, matching `summary.polr()`, Stata `ologit`, and
  SPSS PLUM.
- The footer reports the **thresholds** (`Poor|Fair`, `Fair|Good`,
  `Good|Very good`). They locate the cut-points on the latent logit
  scale and replace the single intercept of a binary logit.

## Odds ratios: `exponentiate = TRUE`

On the logit scale a coefficient is a log cumulative-odds ratio.
`exponentiate = TRUE` reports **odds ratios** instead, exponentiating
the estimate and its CI bounds and rebranding the column header:

``` r

table_regression(fit, exponentiate = TRUE)
#> Cumulative logit regression (proportional odds): self_rated_health
#> 
#>  Variable           │  OR    SE      95% CI       p   
#> ────────────────────┼─────────────────────────────────
#>  age                │ 1.00  0.00  [0.99, 1.01]   .831 
#>  sex:               │                                 
#>    Female (ref.)    │  –     –         –         –    
#>    Male             │ 1.02  0.11  [0.82, 1.26]   .874 
#>  smoking:           │                                 
#>    No (ref.)        │  –     –         –         –    
#>    Yes              │ 0.76  0.10  [0.59, 1.00]   .047 
#>  physical_activity: │                                 
#>    No (ref.)        │  –     –         –         –    
#>    Yes              │ 1.03  0.11  [0.83, 1.28]   .794 
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: Poor|Fair = -2.98, Fair|Good = -1.02, Good|Very good = 1.04.
#> OR = odds ratio.
#> Coefficients exponentiated and displayed as OR; CI bounds exponentiated.
```

An OR above 1 raises the odds of being in a *higher* health category;
below 1 lowers them. Smoking’s OR here is interpretable as “smokers have
lower odds of better self-rated health.”

## Average marginal effects: a probability matrix

Odds ratios are multiplicative and live on the latent scale. For a
reader-friendly, **probability-scale** summary, request average marginal
effects (AME). Because an ordinal model predicts a probability for
*every* response category, each predictor has **one AME per category** —
the effect of the predictor on the probability of that category.
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
lays these out as the field-standard **matrix**: predictors in rows,
response categories in columns (Long & Freese 2014; Williams 2012).

``` r

table_regression(fit, show_columns = c("b", "ame"))
#> Cumulative logit regression (proportional odds): self_rated_health
#> 
#>  Variable           │   B    AME Poor  AME Fair  AME Good  AME Very good 
#> ────────────────────┼────────────────────────────────────────────────────
#>  age                │ -0.00      0.00      0.00     -0.00          -0.00 
#>  sex:               │                                                    
#>    Female (ref.)    │   –                                                
#>    Male             │  0.02     -0.00     -0.00      0.00           0.00 
#>  smoking:           │                                                    
#>    No (ref.)        │   –                                                
#>    Yes              │ -0.27      0.01      0.04     -0.01          -0.05 
#>  physical_activity: │                                                    
#>    No (ref.)        │   –                                                
#>    Yes              │  0.03     -0.00     -0.00      0.00           0.01 
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: Poor|Fair = -2.98, Fair|Good = -1.02, Good|Very good = 1.04.
#> AME = average marginal effect on the probability of each response category (per predictor these sum to ≈ 0 across categories; a change of 0.07 = 7 percentage points, not 7%).
```

How to read it, with the `smoking = Yes` row:

- The four `AME ...` columns are the average change in the probability
  of each health category for a smoker versus a non-smoker, holding the
  other predictors at their observed values.
- The effects **sum to ≈ 0 across the row**: a predictor *redistributes*
  probability mass between categories (the total probability is always
  1). Here smoking moves mass *out of* the better categories and *into*
  the worse ones.
- Cells are **probabilities** (the 0–1 scale, matching
  [`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
  and the binary-`glm` AME). An AME of `0.04` is a change of **4
  percentage points** — *not* 4 percent. The footer note states this;
  reserve “percent” for multiplicative quantities such as odds ratios.

Request only the AME matrix (drop the coefficient column) with
`show_columns = "ame"`, or add the AME standard errors / CIs / p-values
with `"ame_se"`, `"ame_ci"`, `"ame_p"` (or the `"all_ame"` bundle):

``` r

table_regression(fit, show_columns = "ame")
#> Cumulative logit regression (proportional odds): self_rated_health
#> 
#>  Variable           │ AME Poor  AME Fair  AME Good  AME Very good 
#> ────────────────────┼─────────────────────────────────────────────
#>  age                │     0.00      0.00     -0.00          -0.00 
#>  sex:               │                                             
#>    Female (ref.)    │                                             
#>    Male             │    -0.00     -0.00      0.00           0.00 
#>  smoking:           │                                             
#>    No (ref.)        │                                             
#>    Yes              │     0.01      0.04     -0.01          -0.05 
#>  physical_activity: │                                             
#>    No (ref.)        │                                             
#>    Yes              │    -0.00     -0.00      0.00           0.01 
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: Poor|Fair = -2.98, Fair|Good = -1.02, Good|Very good = 1.04.
#> AME = average marginal effect on the probability of each response category (per predictor these sum to ≈ 0 across categories; a change of 0.07 = 7 percentage points, not 7%).
```

Marginal effects are computed with
[`marginaleffects::avg_slopes()`](https://marginaleffects.com) on the
response (probability) scale, then cross-validated to it to machine
precision. For a single-outcome model (binary `glm`, `lm`, mixed) the
AME stays a single column — the per-category matrix appears only when
the response has more than two categories.

## Cluster-robust standard errors

Ordinal fits honour the cluster-robust `vcov` family (`"CR0"`–`"CR3"`)
via
[`sandwich::vcovCL()`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html).
Pass the cluster as a formula, a column name, or a vector (see the main
vignette, *How to specify `cluster`*):

``` r

table_regression(
  fit, vcov = "CR2", cluster = ~region,
  show_columns = c("b", "ame")
)
#> Registered S3 method overwritten by 'clubSandwich':
#>   method    from    
#>   bread.mlm sandwich
#> Cumulative logit regression (proportional odds): self_rated_health
#> 
#>  Variable           │   B    AME Poor  AME Fair  AME Good  AME Very good 
#> ────────────────────┼────────────────────────────────────────────────────
#>  age                │ -0.00      0.00      0.00     -0.00          -0.00 
#>  sex:               │                                                    
#>    Female (ref.)    │   –                                                
#>    Male             │  0.02     -0.00     -0.00      0.00           0.00 
#>  smoking:           │                                                    
#>    No (ref.)        │   –                                                
#>    Yes              │ -0.27      0.01      0.04     -0.01          -0.05 
#>  physical_activity: │                                                    
#>    No (ref.)        │   –                                                
#>    Yes              │  0.03     -0.00     -0.00      0.00           0.01 
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: cluster-robust (CL), clusters by region.
#> Thresholds: Poor|Fair = -2.98, Fair|Good = -1.02, Good|Very good = 1.04.
#> AME = average marginal effect on the probability of each response category (per predictor these sum to ≈ 0 across categories; a change of 0.07 = 7 percentage points, not 7%).
```

The footer switches to name the estimator and the clustering variable.
Heteroskedasticity-consistent (`HC*`) and the `bootstrap` / `jackknife`
resamplers are *not* defined for ordinal fits and are refused with a
clear `spicy_unsupported_vcov` error rather than a silent fallback.

## The `ordinal::clm()` engine

[`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) is rendered
through the same code path and supports the same columns. It is the
engine to reach for when you need flexible link functions, scale
effects, or a relaxation of proportional odds.

``` r

clm_fit <- ordinal::clm(
  self_rated_health ~ age + sex + smoking + physical_activity,
  data = sochealth
)
table_regression(clm_fit, show_columns = c("b", "ame"))
#> Cumulative logit regression (proportional odds): self_rated_health
#> 
#>  Variable           │   B    AME Poor  AME Fair  AME Good  AME Very good 
#> ────────────────────┼────────────────────────────────────────────────────
#>  age                │ -0.00      0.00      0.00     -0.00          -0.00 
#>  sex:               │                                                    
#>    Female (ref.)    │   –                                                
#>    Male             │  0.02     -0.00     -0.00      0.00           0.00 
#>  smoking:           │                                                    
#>    No (ref.)        │   –                                                
#>    Yes              │ -0.27      0.01      0.04     -0.01          -0.05 
#>  physical_activity: │                                                    
#>    No (ref.)        │   –                                                
#>    Yes              │  0.03     -0.00     -0.00      0.00           0.01 
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: Poor|Fair = -2.98, Fair|Good = -1.02, Good|Very good = 1.04.
#> AME = average marginal effect on the probability of each response category (per predictor these sum to ≈ 0 across categories; a change of 0.07 = 7 percentage points, not 7%).
```

### Scale effects and partial proportional odds

`clm` offers two relaxations of the basic model.

A **scale** component (`scale = ~`) lets the latent dispersion depend on
covariates. The location coefficients remain a single shared block, so
the table renders normally. Cluster-robust SEs are not available for
scale fits (`sandwich` has no estimating-functions method for them), so
`CR*` is refused up front with a clear `spicy_unsupported_vcov` error:

``` r

clm_scale <- ordinal::clm(
  self_rated_health ~ age + smoking, scale = ~ smoking,
  data = sochealth
)
table_regression(clm_scale)             # classical: renders normally
#> Cumulative logit regression (proportional odds): self_rated_health
#> 
#>  Variable    │   B     SE      95% CI        p   
#> ─────────────┼───────────────────────────────────
#>  age         │ -0.00  0.00  [-0.01, 0.01]   .884 
#>  smoking:    │                                   
#>    No (ref.) │   –     –          –         –    
#>    Yes       │ -0.27  0.14  [-0.55, 0.01]   .060 
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: Poor|Fair = -3.06, Fair|Good = -1.05, Good|Very good = 1.05.
```

``` r

table_regression(clm_scale, vcov = "CR2", cluster = ~region)
#> Error in `validate_vcov_cluster_lists()`:
#> ! `vcov = "CR2"` is not available for `clm` models.
#> ℹ This class supports: classical. Robust standard errors for more model classes are being added; see ?table_regression.
```

A **nominal** component (`nominal = ~`, *partial* proportional odds)
estimates a separate coefficient **per threshold** for the nominal terms
— a structure a single-block ordinal table cannot represent.
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
refuses these fits with a clear error rather than mis-tabulating them:

``` r

clm_npo <- ordinal::clm(
  self_rated_health ~ age + smoking, nominal = ~ smoking,
  data = sochealth
)
table_regression(clm_npo)
#> Error in `as_regression_frame()`:
#> ! table_regression() does not support partial-proportional-odds `clm` fits (`nominal = ~ ...`).
#> ℹ The nominal terms carry a separate coefficient per threshold, which a single-block ordinal table cannot represent. Drop `nominal` for a proportional-odds model, or use `scale = ~` for scale effects.
```

## Several models side by side

The same fit can appear several times with different estimators, or
different fits can be compared, with `table_regression(list(...))`.
Per-model `vcov` is supplied as a list:

``` r

fit_min <- polr(self_rated_health ~ smoking, data = sochealth, Hess = TRUE)
table_regression(
  list(Unadjusted = fit_min, Adjusted = fit),
  show_columns = c("b", "ame_p")
)
#> Cumulative logit regression (proportional odds) comparison: self_rated_health
#> 
#>                                       Unadjusted                  Adjus 
#>                       ──────────────────────────────────────────  ───── 
#>  Variable           │   B    p Poor  p Fair  p Good  p Very good    B   
#> ────────────────────┼───────────────────────────────────────────────────
#>  smoking:           │                                                   
#>    No (ref.)        │   –                                           –   
#>    Yes              │ -0.27    .067    .050    .218         .037  -0.27 
#>  age                │                                             -0.00 
#>  sex:               │                                                   
#>    Female (ref.)    │                                               –   
#>    Male             │                                              0.02 
#>  physical_activity: │                                                   
#>    No (ref.)        │                                               –   
#>    Yes              │                                              0.03 
#> 
#>                                    Adjusted               
#>                       ─────────────────────────────────── 
#>  Variable           │ p Poor  p Fair  p Good  p Very good 
#> ────────────────────┼─────────────────────────────────────
#>  smoking:           │                                     
#>    No (ref.)        │                                     
#>    Yes              │   .069    .052    .221         .038 
#>  age                │   .831    .831    .833         .831 
#>  sex:               │                                     
#>    Female (ref.)    │                                     
#>    Male             │   .874    .874    .875         .875 
#>  physical_activity: │                                     
#>    No (ref.)        │                                     
#>    Yes              │   .794    .794    .794         .794 
#> 
#> Note. Cumulative logit regression (proportional odds) models.
#> Std. errors: Wald asymptotic (z).
#> Model 1: Thresholds: Poor|Fair = -2.97, Fair|Good = -1.01, Good|Very good = 1.06.
#> Model 2: Thresholds: Poor|Fair = -2.98, Fair|Good = -1.02, Good|Very good = 1.04.
#> AME = average marginal effect on the probability of each response category (per predictor these sum to ≈ 0 across categories; a change of 0.07 = 7 percentage points, not 7%).
```

## Output formats

The default console table shown above is one of several targets. The
`output` argument also produces a raw data frame, a long broom-style
tibble, and — with the corresponding Suggests package — rich `gt`,
`flextable`, `tinytable`, Excel, or Word tables. The structure (the
per-category AME matrix included) carries through to every format.

``` r

head(table_regression(fit, show_columns = c("b", "ame"), output = "data.frame"))
#>          Variable     B AME Poor AME Fair AME Good AME Very good
#> 1             age -0.00     0.00     0.00    -0.00         -0.00
#> 2            sex:                                               
#> 3   Female (ref.)   –                                           
#> 4            Male  0.02    -0.00    -0.00     0.00          0.00
#> 5        smoking:                                               
#> 6       No (ref.)   –
```

``` r

table_regression(fit, show_columns = c("b", "ame"), output = "gt")
```

[TABLE]

*Note.* Cumulative logit regression (proportional odds). Std. errors:
Wald asymptotic (z). Thresholds: Poor\|Fair = -2.98, Fair\|Good = -1.02,
Good\|Very good = 1.04. AME = average marginal effect on the probability
of each response category (per predictor these sum to ≈ 0 across
categories; a change of 0.07 = 7 percentage points, not 7%).

[`broom::tidy()`](https://broom.tidymodels.org) returns the long frame,
one row per `(term, estimate_type)`; per-category AME rows carry the
response category in the `outcome_level`-derived structure:

``` r

broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
#> # A tibble: 20 × 15
#>    model_id outcome    term  estimate_type estimate std.error conf.low conf.high
#>    <chr>    <chr>      <chr> <chr>            <dbl>     <dbl>    <dbl>     <dbl>
#>  1 M1       self_rate… age   ame            3.91e-5 0.000183  -3.20e-4   3.99e-4
#>  2 M1       self_rate… age   ame            1.20e-4 0.000565  -9.86e-4   1.23e-3
#>  3 M1       self_rate… age   ame           -1.20e-5 0.0000570 -1.24e-4   9.97e-5
#>  4 M1       self_rate… age   ame           -1.47e-4 0.000692  -1.50e-3   1.21e-3
#>  5 M1       self_rate… age   B             -7.94e-4 0.00372   -8.09e-3   6.50e-3
#>  6 M1       self_rate… sexM… ame           -8.52e-4 0.00539   -1.14e-2   9.72e-3
#>  7 M1       self_rate… sexM… ame           -2.62e-3 0.0166    -3.52e-2   2.99e-2
#>  8 M1       self_rate… sexM… ame            2.62e-4 0.00166   -3.00e-3   3.52e-3
#>  9 M1       self_rate… sexM… ame            3.21e-3 0.0204    -3.67e-2   4.31e-2
#> 10 M1       self_rate… sexM… B              1.73e-2 0.110     -1.97e-1   2.32e-1
#> 11 M1       self_rate… smok… ame            1.41e-2 0.00778   -1.12e-3   2.94e-2
#> 12 M1       self_rate… smok… ame            4.16e-2 0.0214    -3.01e-4   8.36e-2
#> 13 M1       self_rate… smok… ame           -7.88e-3 0.00644   -2.05e-2   4.74e-3
#> 14 M1       self_rate… smok… ame           -4.79e-2 0.0231    -9.32e-2  -2.56e-3
#> 15 M1       self_rate… smok… B             -2.68e-1 0.135     -5.33e-1  -3.34e-3
#> 16 M1       self_rate… phys… ame           -1.42e-3 0.00542   -1.20e-2   9.20e-3
#> 17 M1       self_rate… phys… ame           -4.36e-3 0.0167    -3.71e-2   2.84e-2
#> 18 M1       self_rate… phys… ame            4.30e-4 0.00165   -2.80e-3   3.66e-3
#> 19 M1       self_rate… phys… ame            5.35e-3 0.0205    -3.48e-2   4.55e-2
#> 20 M1       self_rate… phys… B              2.88e-2 0.110     -1.87e-1   2.45e-1
#> # ℹ 7 more variables: statistic <dbl>, df <dbl>, p.value <dbl>,
#> #   test_type <chr>, is_intercept <lgl>, factor_term <chr>, factor_level <chr>
```

## References

- Agresti, A. (2010). *Analysis of Ordinal Categorical Data* (2nd ed.).
  Wiley.
- Long, J. S., & Freese, J. (2014). *Regression Models for Categorical
  Dependent Variables Using Stata* (3rd ed.). Stata Press.
- Williams, R. (2012). Using the margins command to estimate and
  interpret adjusted predictions and marginal effects. *The Stata
  Journal*, 12(2), 308–331.
- Arel-Bundock, V., Greifer, N., & Heiss, A. (2024). How to Interpret
  Statistical Models Using marginaleffects in R and Python. *Journal of
  Statistical Software*.
