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
regime, followed by a subordinate **`Thresholds`** block listing the
ordered cut-points with their own B / SE / CI / p (the field convention:
`summary.polr()`, SPSS PLUM, SAS, Stata `ologit`, Bender & Grouven
1997):

``` r

fit <- polr(
  self_rated_health ~ age + sex + smoking + physical_activity,
  data = sochealth, Hess = TRUE
)
table_regression(fit)
#> Cumulative logit regression (proportional odds): self_rated_health
#> 
#>  Variable           │    B      SE       95% CI        p   
#> ────────────────────┼──────────────────────────────────────
#>  age                │   -0.00  0.00  [-0.01,  0.01]   .831 
#>  sex:               │                                      
#>    Female (ref.)    │     –     –          –          –    
#>    Male             │    0.02  0.11  [-0.20,  0.23]   .874 
#>  smoking:           │                                      
#>    No (ref.)        │     –     –          –          –    
#>    Yes              │   -0.27  0.14  [-0.53, -0.00]   .047 
#>  physical_activity: │                                      
#>    No (ref.)        │     –     –          –          –    
#>    Yes              │    0.03  0.11  [-0.19,  0.24]   .794 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:        │                                      
#>    Poor | Fair      │   -2.98  0.24  [-3.45, -2.52]  <.001 
#>    Fair | Good      │   -1.02  0.21  [-1.43, -0.62]  <.001 
#>    Good | Very good │    1.04  0.21  [ 0.64,  1.45]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │ 1156                                 
#>  R² (McFadden)      │    0.00                              
#>  R² (Nagelkerke)    │    0.00                              
#>  AIC                │ 2761.2                               
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: latent-scale category cut-points.
```

Reading the table:

- Each predictor has **one** coefficient row (proportional odds); factor
  predictors are grouped under their parent variable with the reference
  level carrying `(ref.)` and an em-dash.
- Inference is **Wald-\\z\\** (`df = Inf`): ordinal MLE has no residual
  degrees of freedom, matching `summary.polr()`, Stata `ologit`, and
  SPSS PLUM.
- A subordinate **`Thresholds`** block lists the cut-points
  (`Poor | Fair`, `Fair | Good`, `Good | Very good`) with B / SE / CI /
  p, like the predictor rows. They locate the category boundaries on the
  latent logit scale and replace the single intercept of a binary logit.
  They are reported on the log-odds scale and are **never
  exponentiated**. The \\z\\-test against zero is rarely of interest (it
  only asks whether the baseline cumulative split sits at 50/50), so do
  not over-read a “significant” threshold – the substantive headline is
  the odds ratios and marginal effects below. Hide the block with
  `show_thresholds = FALSE` to fall back to a compact footer line.
- Below the fit-stats rule, the model-fit block reports **N**, two
  pseudo-\\R^2\\ (**McFadden**, the Stata `ologit` default, and
  **Nagelkerke**, the SPSS PLUM default), and **AIC**. Override with
  `show_fit_stats` (e.g. `show_fit_stats = c("nobs", "AIC", "BIC")`).

## Odds ratios: `exponentiate = TRUE`

On the logit scale a coefficient is a log cumulative-odds ratio.
`exponentiate = TRUE` reports **odds ratios** instead, exponentiating
the estimate and its CI bounds and rebranding the column header:

``` r

table_regression(fit, exponentiate = TRUE)
#> Cumulative logit regression (proportional odds): self_rated_health
#> 
#>  Variable           │   OR      SE       95% CI        p   
#> ────────────────────┼──────────────────────────────────────
#>  age                │    1.00  0.00  [ 0.99,  1.01]   .831 
#>  sex:               │                                      
#>    Female (ref.)    │     –     –          –          –    
#>    Male             │    1.02  0.11  [ 0.82,  1.26]   .874 
#>  smoking:           │                                      
#>    No (ref.)        │     –     –          –          –    
#>    Yes              │    0.76  0.10  [ 0.59,  1.00]   .047 
#>  physical_activity: │                                      
#>    No (ref.)        │     –     –          –          –    
#>    Yes              │    1.03  0.11  [ 0.83,  1.28]   .794 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:        │                                      
#>    Poor | Fair      │   -2.98  0.24  [-3.45, -2.52]  <.001 
#>    Fair | Good      │   -1.02  0.21  [-1.43, -0.62]  <.001 
#>    Good | Very good │    1.04  0.21  [ 0.64,  1.45]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │ 1156                                 
#>  R² (McFadden)      │    0.00                              
#>  R² (Nagelkerke)    │    0.00                              
#>  AIC                │ 2761.2                               
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: latent-scale category cut-points (log-odds scale, not exponentiated).
#> OR = odds ratio.
#> Coefficients exponentiated and displayed as OR; CI bounds exponentiated.
```

An OR above 1 raises the odds of being in a *higher* health category;
below 1 lowers them. Smoking’s OR here is interpretable as “smokers have
lower odds of better self-rated health.” The `Thresholds` rows stay on
the **log-odds scale** (a cut-point is not an odds ratio, so it is never
exponentiated); only the predictor coefficients become ORs, and the
footer flags this.

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
#>  Variable           │    B     AME Poor  AME Fair  AME Good  AME Very good 
#> ────────────────────┼──────────────────────────────────────────────────────
#>  age                │   -0.00      0.00      0.00     -0.00          -0.00 
#>  sex:               │                                                      
#>    Female (ref.)    │     –                                                
#>    Male             │    0.02     -0.00     -0.00      0.00           0.00 
#>  smoking:           │                                                      
#>    No (ref.)        │     –                                                
#>    Yes              │   -0.27      0.01      0.04     -0.01          -0.05 
#>  physical_activity: │                                                      
#>    No (ref.)        │     –                                                
#>    Yes              │    0.03     -0.00     -0.00      0.00           0.01 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:        │                                                      
#>    Poor | Fair      │   -2.98                                              
#>    Fair | Good      │   -1.02                                              
#>    Good | Very good │    1.04                                              
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │ 1156                                                 
#>  R² (McFadden)      │    0.00                                              
#>  R² (Nagelkerke)    │    0.00                                              
#>  AIC                │ 2761.2                                               
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: latent-scale category cut-points.
#> AME = average marginal effect on a response-category probability.
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
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │  1156                                       
#>  R² (McFadden)      │     0.00                                    
#>  R² (Nagelkerke)    │     0.00                                    
#>  AIC                │  2761.2                                     
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: Poor|Fair = -2.98, Fair|Good = -1.02, Good|Very good = 1.04.
#> AME = average marginal effect on a response-category probability.
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
#>  Variable           │    B     AME Poor  AME Fair  AME Good  AME Very good 
#> ────────────────────┼──────────────────────────────────────────────────────
#>  age                │   -0.00      0.00      0.00     -0.00          -0.00 
#>  sex:               │                                                      
#>    Female (ref.)    │     –                                                
#>    Male             │    0.02     -0.00     -0.00      0.00           0.00 
#>  smoking:           │                                                      
#>    No (ref.)        │     –                                                
#>    Yes              │   -0.27      0.01      0.04     -0.01          -0.05 
#>  physical_activity: │                                                      
#>    No (ref.)        │     –                                                
#>    Yes              │    0.03     -0.00     -0.00      0.00           0.01 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:        │                                                      
#>    Poor | Fair      │   -2.98                                              
#>    Fair | Good      │   -1.02                                              
#>    Good | Very good │    1.04                                              
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │ 1156                                                 
#>  R² (McFadden)      │    0.00                                              
#>  R² (Nagelkerke)    │    0.00                                              
#>  AIC                │ 2761.2                                               
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: cluster-robust (CL), clusters by region.
#> Thresholds: latent-scale category cut-points.
#> AME = average marginal effect on a response-category probability.
```

The footer switches to name the estimator and the clustering variable.
Heteroskedasticity-consistent (`HC*`) and the `bootstrap` / `jackknife`
resamplers are *not* defined for ordinal fits and are refused with a
clear `spicy_unsupported_vcov` error rather than a silent fallback.

## Standard errors and confidence intervals

The three inference regimes differ in **how the standard error and the
confidence interval relate**:

- **Wald** (default): both come from the model information matrix. The
  CI is `estimate ± z × SE` (symmetric) and the *p*-value uses the same
  SE — SE, CI and *p* are one coherent set.
- **Robust / cluster-robust** (`vcov = "CR*"`): the whole set switches
  to the sandwich estimator — SE, CI (`± z × SE_robust`) and *p* shift
  together, still coupled.
- **Profile likelihood** (`ci_method = "profile"`): the CI is inverted
  from the likelihood-ratio statistic and is **asymmetric** — *not*
  `estimate ± z × SE`. Profile is a **CI-only refinement**: the
  estimate, SE, statistic and *p*-value stay Wald; only the CI changes.
  It covers the predictor coefficients (via
  [`confint()`](https://rdrr.io/r/stats/confint.html)); the thresholds
  stay Wald. A robust `vcov` takes precedence (profile is model-based),
  so requesting both uses the robust Wald CIs.

Because a profile CI cannot be reconstructed from the SE, the footer
**discloses** it (`95% CIs: profile likelihood.`) alongside the SE
method — following APA 7 / SAMPL / STROBE and matching
[`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html):

``` r

table_regression(fit, ci_method = "profile", show_columns = c("b", "ci", "p"))
#> Cumulative logit regression (proportional odds): self_rated_health
#> 
#>  Variable           │    B         95% CI        p   
#> ────────────────────┼────────────────────────────────
#>  age                │   -0.00  [-0.01,  0.01]   .831 
#>  sex:               │                                
#>    Female (ref.)    │     –          –          –    
#>    Male             │    0.02  [-0.20,  0.23]   .874 
#>  smoking:           │                                
#>    No (ref.)        │     –          –          –    
#>    Yes              │   -0.27  [-0.53, -0.00]   .047 
#>  physical_activity: │                                
#>    No (ref.)        │     –          –          –    
#>    Yes              │    0.03  [-0.19,  0.24]   .794 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:        │                                
#>    Poor | Fair      │   -2.98  [-3.45, -2.52]  <.001 
#>    Fair | Good      │   -1.02  [-1.43, -0.62]  <.001 
#>    Good | Very good │    1.04  [ 0.64,  1.45]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │ 1156                           
#>  R² (McFadden)      │    0.00                        
#>  R² (Nagelkerke)    │    0.00                        
#>  AIC                │ 2761.2                         
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> 95% CIs: profile likelihood.
#> Thresholds: latent-scale category cut-points.
```

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
#>  Variable           │    B     AME Poor  AME Fair  AME Good  AME Very good 
#> ────────────────────┼──────────────────────────────────────────────────────
#>  age                │   -0.00      0.00      0.00     -0.00          -0.00 
#>  sex:               │                                                      
#>    Female (ref.)    │     –                                                
#>    Male             │    0.02     -0.00     -0.00      0.00           0.00 
#>  smoking:           │                                                      
#>    No (ref.)        │     –                                                
#>    Yes              │   -0.27      0.01      0.04     -0.01          -0.05 
#>  physical_activity: │                                                      
#>    No (ref.)        │     –                                                
#>    Yes              │    0.03     -0.00     -0.00      0.00           0.01 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:        │                                                      
#>    Poor | Fair      │   -2.98                                              
#>    Fair | Good      │   -1.02                                              
#>    Good | Very good │    1.04                                              
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │ 1156                                                 
#>  R² (McFadden)      │    0.00                                              
#>  R² (Nagelkerke)    │    0.00                                              
#>  AIC                │ 2761.2                                               
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: latent-scale category cut-points.
#> AME = average marginal effect on a response-category probability.
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
#>  Variable           │    B      SE       95% CI        p   
#> ────────────────────┼──────────────────────────────────────
#>  age                │   -0.00  0.00  [-0.01,  0.01]   .884 
#>  smoking:           │                                      
#>    No (ref.)        │     –     –          –          –    
#>    Yes              │   -0.27  0.14  [-0.55,  0.01]   .060 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:        │                                      
#>    Poor | Fair      │   -3.06  0.24  [-3.53, -2.60]  <.001 
#>    Fair | Good      │   -1.05  0.20  [-1.44, -0.66]  <.001 
#>    Good | Very good │    1.05  0.20  [ 0.66,  1.45]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │ 1156                                 
#>  R² (McFadden)      │    0.00                              
#>  R² (Nagelkerke)    │    0.01                              
#>  AIC                │ 2757.8                               
#> 
#> Note. Cumulative logit regression (proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: latent-scale category cut-points.
```

``` r

table_regression(clm_scale, vcov = "CR2", cluster = ~region)
#> Error in `validate_vcov_cluster_lists()`:
#> ! `vcov = "CR2"` is not available for `clm` models.
#> ℹ This class supports: classical. Robust standard errors for more model classes are being added; see ?table_regression.
```

A **nominal** component (`nominal = ~`, *partial* proportional odds)
lets a predictor’s effect vary across the cut-points: it estimates a
separate coefficient **per cut-point** for the nominal terms. These
render as a labelled **`Non-proportional effects`** block — one row per
cut-point — between the proportional coefficients and the thresholds:

``` r

clm_npo <- ordinal::clm(
  self_rated_health ~ age, nominal = ~ smoking,
  data = sochealth
)
table_regression(clm_npo)
#> Cumulative logit regression (partial proportional odds): self_rated_health
#> 
#>  Variable                        │    B      SE       95% CI        p   
#> ─────────────────────────────────┼──────────────────────────────────────
#>  age                             │   -0.00  0.00  [-0.01,  0.01]   .844 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Non-proportional effects:       │                                      
#>    smokingYes @ Poor | Fair      │    0.82  0.28  [ 0.28,  1.37]   .003 
#>    smokingYes @ Fair | Good      │    0.22  0.16  [-0.09,  0.53]   .160 
#>    smokingYes @ Good | Very good │    0.24  0.17  [-0.10,  0.58]   .165 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:                     │                                      
#>    Poor | Fair                   │   -3.17  0.25  [-3.65, -2.69]  <.001 
#>    Fair | Good                   │   -1.03  0.20  [-1.42, -0.65]  <.001 
#>    Good | Very good              │    1.03  0.20  [ 0.64,  1.41]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                               │ 1156                                 
#>  R² (McFadden)                   │    0.00                              
#>  R² (Nagelkerke)                 │    0.01                              
#>  AIC                             │ 2756.1                               
#> 
#> Note. Cumulative logit regression (partial proportional odds).
#> Std. errors: Wald asymptotic (z).
#> Thresholds: latent-scale category cut-points.
```

Two caveats for the non-proportional terms, both surfaced by the table:
robust / cluster SEs are **not available** (`sandwich` has no
estimating-functions method, so a robust `vcov` is refused), and
`ci_method = "profile"` covers the proportional coefficients only (the
non-proportional and threshold rows stay Wald). Under
`exponentiate = TRUE` each non-proportional coefficient becomes an odds
ratio for its cut-point.

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
#>                                        Unadjusted                   Adjuste 
#>                       ────────────────────────────────────────────  ─────── 
#>  Variable           │    B     p Poor  p Fair  p Good  p Very good     B    
#> ────────────────────┼───────────────────────────────────────────────────────
#>  smoking:           │                                                       
#>    No (ref.)        │     –                                             –   
#>    Yes              │   -0.27    .067    .050    .218         .037    -0.27 
#>  age                │                                                 -0.00 
#>  sex:               │                                                       
#>    Female (ref.)    │                                                   –   
#>    Male             │                                                  0.02 
#>  physical_activity: │                                                       
#>    No (ref.)        │                                                   –   
#>    Yes              │                                                  0.03 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:        │                                                       
#>    Poor | Fair      │   -2.97                                         -2.98 
#>    Fair | Good      │   -1.01                                         -1.02 
#>    Good | Very good │    1.06                                          1.04 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │ 1156                                          1156    
#>  R² (McFadden)      │    0.00                                          0.00 
#>  R² (Nagelkerke)    │    0.00                                          0.00 
#>  AIC                │ 2755.3                                        2761.2  
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
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:        │                                     
#>    Poor | Fair      │                                     
#>    Fair | Good      │                                     
#>    Good | Very good │                                     
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │                                     
#>  R² (McFadden)      │                                     
#>  R² (Nagelkerke)    │                                     
#>  AIC                │                                     
#> 
#> Note. Cumulative logit regression (proportional odds) models.
#> Std. errors: Wald asymptotic (z).
#> Thresholds: latent-scale category cut-points.
#> AME = average marginal effect on a response-category probability.
```

## Output formats

The default console table shown above is one of several targets. The
`output` argument also produces a raw data frame, a long broom-style
tibble, and — with the corresponding Suggests package — rich `gt`,
`flextable`, `tinytable`, Excel, or Word tables. The structure (the
per-category AME matrix included) carries through to every format.

``` r

head(table_regression(fit, show_columns = c("b", "ame"), output = "data.frame"))
#>          Variable       B AME Poor AME Fair AME Good AME Very good
#> 1             age   -0.00     0.00     0.00    -0.00         -0.00
#> 2            sex:                                                 
#> 3   Female (ref.)     –                                           
#> 4            Male    0.02    -0.00    -0.00     0.00          0.00
#> 5        smoking:                                                 
#> 6       No (ref.)     –
```

``` r

table_regression(fit, show_columns = c("b", "ame"), output = "gt")
```

[TABLE]

*Note.* Cumulative logit regression (proportional odds). Std. errors:
Wald asymptotic (z). Thresholds: latent-scale category cut-points. AME =
average marginal effect on a response-category probability.

[`broom::tidy()`](https://broom.tidymodels.org) returns the long frame,
one row per `(term, estimate_type)`; per-category AME rows carry the
response category in the `outcome_level`-derived structure:

``` r

broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
#> # A tibble: 23 × 15
#>    model_id outcome    term  estimate_type estimate std.error conf.low conf.high
#>    <chr>    <chr>      <chr> <chr>            <dbl>     <dbl>    <dbl>     <dbl>
#>  1 M1       self_rate… age   ame            3.91e-5 0.000183  -3.20e-4 0.000399 
#>  2 M1       self_rate… age   ame            1.20e-4 0.000565  -9.86e-4 0.00123  
#>  3 M1       self_rate… age   ame           -1.20e-5 0.0000570 -1.24e-4 0.0000997
#>  4 M1       self_rate… age   ame           -1.47e-4 0.000692  -1.50e-3 0.00121  
#>  5 M1       self_rate… age   B             -7.94e-4 0.00372   -8.09e-3 0.00650  
#>  6 M1       self_rate… sexM… ame           -8.52e-4 0.00539   -1.14e-2 0.00972  
#>  7 M1       self_rate… sexM… ame           -2.62e-3 0.0166    -3.52e-2 0.0299   
#>  8 M1       self_rate… sexM… ame            2.62e-4 0.00166   -3.00e-3 0.00352  
#>  9 M1       self_rate… sexM… ame            3.21e-3 0.0204    -3.67e-2 0.0431   
#> 10 M1       self_rate… sexM… B              1.73e-2 0.110     -1.97e-1 0.232    
#> # ℹ 13 more rows
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
