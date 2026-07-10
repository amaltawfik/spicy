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

Check this before fitting anything.
[`polr()`](https://rdrr.io/pkg/MASS/man/polr.html) and `clm()` estimate
on the factor’s level order *as given* — they do not warn when the
outcome is a plain (unordered) factor, or when the levels sit in the
wrong order. Data read from text files is particularly exposed:
[`factor()`](https://rdrr.io/r/base/factor.html) defaults to
alphabetical levels (here `Fair, Good, Poor, Very good`), which would
silently fit a nonsensical scale. And reversing the order negates every
coefficient and marginal effect. Every interpretation below hinges on
knowing the direction of the scale — here worst to best (Long & Freese
2014).

## The proportional-odds model

A cumulative logit model with \\K\\ ordered response categories
estimates **one slope per predictor** (shared across all \\K-1\\
cumulative splits — the *proportional-odds assumption*) plus \\K-1\\
ordered **thresholds** (cut-points). A positive location
(proportional-odds) slope means that higher values of the predictor push
the response toward *higher* categories. There is deliberately **no
per-category coefficient**: the proportional-odds restriction collapses
the effect into a single slope per predictor, trading per-category
detail for one interpretable number. The per-category structure
reappears in the *marginal effects* (below), where a one-unit change has
a different effect on the probability of each category — and in the
relaxations of the shared-slope restriction that
[`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) offers
(tested and demonstrated below).

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
  exponentiated**. The \\z\\-test against zero is rarely of interest —
  it only asks whether the cumulative split sits at 50/50 at the profile
  where *every* predictor equals zero, here a respondent aged 0, not a
  meaningful case — so do not over-read a “significant” threshold; the
  substance lies in the odds ratios and marginal effects below. Hide the
  block with `show_thresholds = FALSE` to fall back to a compact footer
  line.
- Below the fit-stats rule, the model-fit block reports **N**, two
  pseudo-\\R^2\\ (**McFadden**, the Stata `ologit` default, and
  **Nagelkerke**, the SPSS PLUM default), and **AIC**. Both compare the
  fitted log-likelihood to the intercept-only model’s, and both round to
  0.00 here: these four predictors explain almost none of the variation
  in self-rated health. This survey model therefore illustrates table
  mechanics and interpretation *wording*; read the exemplary sentences
  below as templates, not as substantive findings. Override the block
  with `show_fit_stats`
  (e.g. `show_fit_stats = c("nobs", "AIC", "BIC")`).

## Odds ratios: `exponentiate = TRUE`

On the logit scale a coefficient is a log cumulative-odds ratio.
`exponentiate = TRUE` reports **odds ratios** instead, exponentiating
the estimate and its CI bounds and relabelling the column header
accordingly. This is link-specific: under `method = "cloglog"` the
exponentiated coefficient is a **hazard ratio** (the grouped-time
proportional-hazards reading) and the header and footer relabel to HR;
under probit or cauchit the exponential has no ratio interpretation, and
`exponentiate = TRUE` is refused with a clear error.

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
#> Coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric).
```

The OR here is a *cumulative* odds ratio: it multiplies the odds of
scoring **above** any given cut-point rather than at or below it, and —
because the model is proportional-odds — the same ratio applies at every
cut-point (`Poor | Fair`, `Fair | Good`, `Good | Very good`). It is
*not* the odds of landing in one specific category; that per-category
detail is what the AME matrix below provides. For a location
(proportional) coefficient, an OR above 1 raises the cumulative odds of
being in a *higher* health category and an OR below 1 lowers them — the
non-proportional block later in this vignette follows the opposite
convention. Exact reading for the one borderline effect here: smoking’s
OR of 0.76 means that, at every cut-point, smokers have about 24% lower
odds of being in a higher health category rather than a lower one,
adjusting for age, sex, and physical activity. The CI upper bound prints
as 1.00 but is 0.997 before rounding; with p = .047 this is a borderline
association, so treat the sentence as a wording template. The
`Thresholds` rows stay on the **log-odds scale** (a cut-point is not an
odds ratio, so it is never exponentiated); only the predictor
coefficients become ORs, and the footer flags this.

## Average marginal effects: a probability matrix

Odds ratios are multiplicative and describe the (cumulative) odds, not
probabilities directly. For a reader-friendly, **probability-scale**
summary, request average marginal effects (AME). Because an ordinal
model predicts a probability for *every* response category, each
predictor has **one AME per category** — the effect of the predictor on
the probability of that category.
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
  1). Here smoking moves mass out of the better categories and into the
  worse ones — about 4.8 points out of `Very good`, 4.2 into `Fair`.
- Cells are **probabilities** (the 0–1 scale, matching
  [`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
  and the binary-`glm` AME). An AME of `0.04` is a change of **4
  percentage points** — *not* 4 percent. The footer note states this;
  reserve “percent” for multiplicative quantities such as odds ratios.

An AME is the single-number summary of a fuller object: the predicted
probability of each category as the predictor varies. When the story
deserves more than one number per category — or to check that an average
is not masking a non-monotone pattern — tabulate those probabilities
directly:

``` r

marginaleffects::avg_predictions(fit, by = "smoking")
#> 
#>      Group smoking Estimate Std. Error     z Pr(>|z|)     S  2.5 % 97.5 %
#>  Poor          No    0.0490    0.00635  7.71   <0.001  46.2 0.0365 0.0614
#>  Poor          Yes   0.0632    0.00995  6.35   <0.001  32.1 0.0437 0.0827
#>  Fair          No    0.2189    0.01276 17.15   <0.001 216.7 0.1939 0.2440
#>  Fair          Yes   0.2608    0.02172 12.01   <0.001 107.9 0.2182 0.3033
#>  Good          No    0.4746    0.01473 32.22   <0.001 754.4 0.4457 0.5034
#>  Good          Yes   0.4666    0.01562 29.87   <0.001 648.7 0.4360 0.4972
#>  Very good     No    0.2575    0.01397 18.44   <0.001 249.8 0.2301 0.2849
#>  Very good     Yes   0.2094    0.02128  9.84   <0.001  73.5 0.1677 0.2511
#> 
#> Type: probs
```

Smokers’ expected distribution sits lower on the scale than non-smokers’
— `Very good` 20.9% versus 25.8%, `Fair` 26.1% versus 21.9% — and the
smoking AME row above is exactly the difference between these two
profiles. For predicted-probability curves along a continuous predictor,
[`marginaleffects::plot_predictions()`](https://rdrr.io/pkg/marginaleffects/man/plot_predictions.html)
plots the same quantities.

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

Marginal effects are the response-scale (probability) output of
[`marginaleffects::avg_slopes()`](https://marginaleffects.com). For a
single-outcome model (binary `glm`, `lm`, mixed) the AME stays a single
column — the per-category matrix appears only when the response has more
than two categories.

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
Heteroskedasticity-consistent (`HC*`) estimators and the `bootstrap` /
`jackknife` resamplers are *not* defined for ordinal fits and are
refused with a clear `spicy_unsupported_vcov` error rather than a silent
fallback.

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

### Testing proportional odds

The shared-slope restriction is a testable hypothesis, and the workflow
is: fit, **test**, then relax only what fails (Long & Freese 2014).
`ordinal` provides a likelihood-ratio test per predictor:
`nominal_test()` refits the model freeing each predictor across the
cut-points and compares, and `scale_test()` does the same for a scale
(dispersion) effect. For `polr` there is no built-in equivalent — the
classic logit-specific check is Brant’s (1990) test (package `brant`);
`clm`’s tests are the likelihood-based generalisation.

``` r

ordinal::nominal_test(clm_fit)
#> Tests of nominal effects
#> 
#> formula: self_rated_health ~ age + sex + smoking + physical_activity
#>                   Df  logLik    AIC    LRT Pr(>Chi)  
#> <none>               -1373.6 2761.2                  
#> age                2 -1373.6 2765.1 0.0526  0.97407  
#> sex                2 -1373.2 2764.4 0.7847  0.67547  
#> smoking            2 -1371.0 2760.0 5.1474  0.07625 .
#> physical_activity  2 -1373.2 2764.4 0.8015  0.66980  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
ordinal::scale_test(clm_fit)
#> Tests of scale effects
#> 
#> formula: self_rated_health ~ age + sex + smoking + physical_activity
#>                   Df  logLik    AIC     LRT Pr(>Chi)
#> <none>               -1373.6 2761.2                 
#> age                1 -1373.6 2763.2 0.02185   0.8825
#> sex                1 -1373.5 2763.1 0.13106   0.7173
#> smoking            1 -1372.8 2761.6 1.55287   0.2127
#> physical_activity  1 -1373.5 2762.9 0.28934   0.5906
```

Each `nominal_test` row asks: does freeing this predictor across the
\\K-1\\ cut-points (here 2 extra parameters) improve the fit? No term
clearly fails. Smoking is the one borderline case (LRT = 5.15, df = 2, p
= .076); age, sex and physical activity are firmly consistent with
proportional odds (p \> .66), and no term shows a scale effect (p \>
.21). The decision rule is to relax *only* the terms that fail — here
none clearly does, so the proportional-odds table above is adequate as
the primary analysis. Two caveats temper the rule: at moderate sample
sizes these per-term tests have limited power, and one test per
predictor invites multiplicity — so a borderline result argues for
reporting the relaxed fit as a *sensitivity analysis*, not for switching
automatically. In large samples the opposite failure dominates: the
tests reject routinely — Long & Freese (2014) report rejection in the
majority of real applications — while also being sensitive to other
kinds of misspecification, and the freed model’s *predictions* often
barely differ from the proportional fit’s. When a test rejects, compare
predicted probabilities across the two fits before restructuring the
model. And a violated assumption is never a rationale for linear
regression on the ordinal outcome, whose assumptions are stronger still
(Long & Freese 2014). The next two sections demonstrate the two
relaxations, using smoking precisely because it is the borderline term.

### Partial proportional odds: `nominal = ~`

A **nominal** component frees a predictor’s effect across the
cut-points: a separate coefficient *per cut-point* instead of one shared
slope — Peterson & Harrell’s (1990) *partial* proportional odds. These
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

The freed structure is read *within* the block: the smoking effect is
concentrated at the lowest split (`Poor | Fair`: B = 0.82, p = .003) and
near-null at the two higher splits (B = 0.22 and 0.24, p \> .16). That
heterogeneity is what the shared slope averaged away, and what relaxing
proportional odds buys.

One trap, and it matters: **nominal coefficients carry the opposite sign
to location coefficients.** `clm` folds nominal terms into the threshold
side of the model — they shift the cut-points \\\theta_j\\ rather than
enter the slope — so a *positive* nominal coefficient raises the
cumulative odds of being **at or below** that cut-point, pushing toward
the *lower* categories there. Under `exponentiate = TRUE` the smoking
row at `Poor | Fair` becomes OR = 2.28: a smoker’s odds of rating health
*Poor* (rather than Fair or better) are 2.28 times a non-smoker’s. That
is the same worsening of health the proportional fit expressed as OR =
0.76 (\< 1) on the higher-category side — reverse polarity, because the
nominal term enters the threshold, not the slope. Do not apply the
earlier location-coefficient sign rules to this block.

Two caveats for the non-proportional terms, both surfaced by the table:
robust / cluster SEs are **not available** (`sandwich` has no
estimating-functions method, so a robust `vcov` is refused), and
`ci_method = "profile"` covers the proportional coefficients only (the
non-proportional and threshold rows stay Wald).

### Scale effects: `scale = ~`

A **scale** component lets the *dispersion* of the latent response
depend on covariates, relaxing the constant-variance assumption of the
basic model. The motivating question is about spread, not location: is
latent health more *variable* among smokers than non-smokers? Test it
like any nested pair:

``` r

clm_loc <- ordinal::clm(
  self_rated_health ~ age + smoking,
  data = sochealth
)
clm_scale <- ordinal::clm(
  self_rated_health ~ age + smoking, scale = ~ smoking,
  data = sochealth
)
anova(clm_loc, clm_scale)
#> Likelihood ratio tests of cumulative link models:
#>  
#>           formula:                          scale:   link: threshold:
#> clm_loc   self_rated_health ~ age + smoking ~1       logit flexible  
#> clm_scale self_rated_health ~ age + smoking ~smoking logit flexible  
#> 
#>           no.par    AIC  logLik LR.stat df Pr(>Chisq)
#> clm_loc        5 2757.3 -1373.6                      
#> clm_scale      6 2757.8 -1372.9   1.506  1     0.2197
```

No evidence here (LR = 1.51, df = 1, p = .220). The scale coefficient
lives on the **log standard-deviation** metric: the estimate of 0.09
says smokers’ latent SD is exp(0.09) ≈ 1.10 — about 10% larger — but the
CI includes 0 on the log scale, so the data cannot distinguish it from
equal spread. The table renders the location coefficients, a subordinate
**`Scale effects`** block for the scale coefficients, and the
thresholds; the footer states the log-SD scale. Scale rows are never
exponentiated — their exponential is a ratio of latent standard
deviations, not an odds ratio:

``` r

table_regression(clm_scale)
#> Cumulative logit regression (proportional odds): self_rated_health
#> 
#>  Variable           │    B      SE       95% CI        p   
#> ────────────────────┼──────────────────────────────────────
#>  age                │   -0.00  0.00  [-0.01,  0.01]   .884 
#>  smoking:           │                                      
#>    No (ref.)        │     –     –          –          –    
#>    Yes              │   -0.27  0.14  [-0.55,  0.01]   .060 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Scale effects:     │                                      
#>    smokingYes       │    0.09  0.08  [-0.06,  0.24]   .224 
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
#> Scale effects: covariate effects on the log standard deviation of the latent response.
```

Cluster-robust SEs are not available for scale fits (`sandwich` has no
estimating-functions method for them), so `CR*` is refused up front with
a clear `spicy_unsupported_vcov` error:

``` r

table_regression(clm_scale, vcov = "CR2", cluster = ~region)
#> Error in `validate_vcov_cluster_lists()`:
#> ! `vcov = "CR2"` is not available for `clm` models.
#> ℹ This class supports: classical. Robust standard errors for more model classes are being added; see ?table_regression.
```

### Choosing an ordinal specification

The driving question is what to do when proportional odds fails — or was
never plausible.

- When the tests pass, the shared-slope cumulative model is the most
  parsimonious choice: one slope per predictor, one table row each.
- When a *few* predictors fail, free just those with `nominal = ~` and
  report the freed fit, at least as a sensitivity analysis.
- When the assumption fails *broadly*, or the ordering of the response
  is itself doubtful, abandon ordinality for **multinomial logit** — the
  standard unconstrained alternative (Long & Freese 2014), compared to
  the ordinal fit by AIC — covered in the [multinomial
  vignette](https://amaltawfik.github.io/spicy/articles/table-regression-multinomial.md).
- For asymmetric responses and grouped survival times, the **cloglog**
  link (`method = "cloglog"` in `polr`, `link = "cloglog"` in `clm`)
  replaces the odds-ratio reading with a hazard-ratio one, connecting
  the link menu above to a modelling rationale.

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
#>                                        Unadjusted                   Adjust… 
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
one row per `(term, estimate_type, outcome_level)`; each per-category
AME row is labelled by its response category in the `outcome_level`
column:

``` r

broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
#> # A tibble: 23 × 16
#>    model_id outcome         outcome_level term  estimate_type estimate std.error
#>    <chr>    <chr>           <chr>         <chr> <chr>            <dbl>     <dbl>
#>  1 M1       self_rated_hea… Poor          age   ame            3.91e-5 0.000183 
#>  2 M1       self_rated_hea… Fair          age   ame            1.20e-4 0.000565 
#>  3 M1       self_rated_hea… Good          age   ame           -1.20e-5 0.0000571
#>  4 M1       self_rated_hea… Very good     age   ame           -1.47e-4 0.000691 
#>  5 M1       self_rated_hea… NA            age   B             -7.94e-4 0.00372  
#>  6 M1       self_rated_hea… Poor          sexM… ame           -8.52e-4 0.00539  
#>  7 M1       self_rated_hea… Fair          sexM… ame           -2.62e-3 0.0166   
#>  8 M1       self_rated_hea… Good          sexM… ame            2.62e-4 0.00166  
#>  9 M1       self_rated_hea… Very good     sexM… ame            3.21e-3 0.0204   
#> 10 M1       self_rated_hea… NA            sexM… B              1.73e-2 0.110    
#> # ℹ 13 more rows
#> # ℹ 9 more variables: conf.low <dbl>, conf.high <dbl>, statistic <dbl>,
#> #   df <dbl>, p.value <dbl>, test_type <chr>, is_intercept <lgl>,
#> #   factor_term <chr>, factor_level <chr>
```

## References

- Agresti, A. (2010). *Analysis of Ordinal Categorical Data* (2nd ed.).
  Wiley.
- Brant, R. (1990). Assessing proportionality in the proportional odds
  model for ordinal logistic regression. *Biometrics*, 46(4), 1171–1178.
- Long, J. S., & Freese, J. (2014). *Regression Models for Categorical
  Dependent Variables Using Stata* (3rd ed.). Stata Press.
- Peterson, B., & Harrell, F. E. (1990). Partial proportional odds
  models for ordinal response variables. *Journal of the Royal
  Statistical Society: Series C (Applied Statistics)*, 39(2), 205–217.
- Williams, R. (2012). Using the margins command to estimate and
  interpret adjusted predictions and marginal effects. *The Stata
  Journal*, 12(2), 308–331.
- Arel-Bundock, V., Greifer, N., & Heiss, A. (2024). How to Interpret
  Statistical Models Using marginaleffects in R and Python. *Journal of
  Statistical Software*.
