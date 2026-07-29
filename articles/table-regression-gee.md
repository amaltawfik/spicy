# GEE regression tables (population-averaged models)

``` r

library(spicy)
library(geepack) # geeglm()
```

This vignette covers **generalized estimating equations** (GEE) –
marginal models for clustered and longitudinal data, fitted with
[`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html)
(Liang & Zeger, 1986; Halekoh, Højsgaard & Yan, 2006). The companion
vignette [*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
covers the shared mechanics (output engines, labels,
[`tidy()`](https://generics.r-lib.org/reference/tidy.html));
[*Mixed-effects regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md)
shows the population-averaged and subject-specific answers side by side
in a single table.

## When GEE: the population-averaged question

Clustered data can be modelled to answer two different questions. A
mixed model answers the **subject-specific** (conditional) one: what
happens to *this* subject’s outcome when their predictor changes,
holding their random effect fixed. GEE answers the
**population-averaged** (marginal) one: how does the *average* outcome
differ between populations that differ in the predictor (Zeger, Liang &
Albert, 1988). The marginal estimand is the natural one for policy and
epidemiology – a treatment’s effect on a population’s risk, not on one
patient’s odds (Hubbard et al., 2010).

For a linear model the two coincide. On a logit link they do not: with a
random-intercept variance \\\sigma^2\\, the population-averaged
coefficient is attenuated by approximately \\1/\sqrt{1 +
0.346\\\sigma^2}\\ relative to the subject-specific one (Zeger et al.,
1988), so the conditional odds ratio is always farther from 1. Neither
is “biased” – they answer different questions. The side-by-side table in
the [mixed-effects
vignette](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md)
shows the gap on real data.

Two properties make GEE attractive when the marginal question is the
right one. The coefficients stay consistent even if the assumed
within-cluster correlation is wrong, and the standard errors are
computed with a cluster-robust sandwich estimator *by construction* –
robustness is not an option added after the fact, it is the method
(Liang & Zeger, 1986).

## A first table

`geepack`’s `dietox` data follow the weight of 72 pigs over 12 weeks. A
gaussian GEE with an exchangeable working correlation:

``` r

data(dietox, package = "geepack")

fit <- geeglm(
  Weight ~ Time + Cu,
  id     = Pig,
  data   = dietox,
  family = gaussian,
  corstr = "exchangeable"
)
table_regression(fit)
#> Population-averaged linear regression (GEE): Weight
#> 
#>  Variable         │   B      SE       95% CI        p   
#> ──────────────────┼─────────────────────────────────────
#>  (Intercept)      │  15.42  1.03  [13.41, 17.43]  <.001 
#>  Time             │   6.94  0.08  [ 6.79,  7.10]  <.001 
#>  Cu:              │                                     
#>    Cu000 (ref.)   │    –     –          –          –    
#>    Cu035          │  -0.84  1.56  [-3.90,  2.23]   .593 
#>    Cu175          │   1.77  1.88  [-1.90,  5.45]   .345 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                │ 861                                 
#>  N (Pig)          │  72                                 
#>  Max cluster size │  12                                 
#> 
#> Note. Population-averaged linear regression (GEE).
#> Std. errors: Robust sandwich (GEE), clusters by Pig.
#> GEE working correlation: exchangeable (alpha = 0.77).
```

Three things to read off this table:

- **The title** says *Population-averaged linear regression (GEE)* – the
  estimand is named, not implied.
- **The fit statistics** describe the sample the way the GEE literature
  (and Stata’s `xtgee` header) does: the number of observations, the
  number of clusters (`N (Pig)`), and the largest cluster. These are the
  clusters the sandwich variance is actually computed over – see the
  section on unsorted data below.
- **The footer** carries two disclosures.
  `Std. errors: Robust sandwich (GEE), clusters by Pig` names the
  inference: the fit’s own sandwich estimator, clustered on its `id =`
  variable. `GEE working correlation: exchangeable (alpha = ...)` names
  the working correlation structure with its estimated parameter – two
  GEE fits with the same formula but different structures are different
  models, so the structure is model-defining and always disclosed.

## Choosing the working correlation

The working correlation encodes how observations within a cluster are
assumed to correlate: `"independence"` (none), `"exchangeable"` (all
pairs equally), `"ar1"` (decaying with distance), or `"unstructured"`.
Point estimates are consistent under any of them; the choice affects
efficiency. Because GEE has no likelihood, AIC is undefined – the
comparison tool is Pan’s (2001) quasi-likelihood information criterion,
available as opt-in fit statistics:

``` r

fit_ind <- update(fit, corstr = "independence")
fit_ar1 <- update(fit, corstr = "ar1")

table_regression(
  list(
    Independence = fit_ind,
    Exchangeable = fit,
    "AR(1)"      = fit_ar1
  ),
  show_fit_stats = c("nobs", "n_groups", "qic", "qicu")
)
#> Population-averaged linear regression (GEE) comparison: Weight
#> 
#>                       Independence           Exchangeable           AR(1)      
#>                   ─────────────────────  ─────────────────────  ────────────── 
#>  Variable       │    B       SE     p       B       SE     p       B       SE  
#> ────────────────┼──────────────────────────────────────────────────────────────
#>  (Intercept)    │    15.42  1.03  <.001     15.42  1.03  <.001     18.13  0.94 
#>  Time           │     6.95  0.08  <.001      6.94  0.08  <.001      6.73  0.08 
#>  Cu:            │                                                              
#>    Cu000 (ref.) │      –     –     –          –     –     –          –     –   
#>    Cu035        │    -0.86  1.57   .583     -0.84  1.56   .593     -0.47  1.44 
#>    Cu175        │     1.76  1.88   .350      1.77  1.88   .345      1.21  1.79 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n              │   861                    861                    861          
#>  N (Pig)        │    72                     72                     72          
#>  QIC            │ 43158.6                43158.7                45131.0        
#>  QICu           │ 43107.2                43107.6                45087.0        
#> 
#>                   AR(1) 
#>                   ───── 
#>  Variable       │   p   
#> ────────────────┼───────
#>  (Intercept)    │ <.001 
#>  Time           │ <.001 
#>  Cu:            │       
#>    Cu000 (ref.) │  –    
#>    Cu035        │  .744 
#>    Cu175        │  .500 
#> 
#> Note. Population-averaged linear regression (GEE) models.
#> Std. errors: Robust sandwich (GEE), clusters by Pig.
#> Model 1: GEE working correlation: independence.
#> Model 2: GEE working correlation: exchangeable (alpha = 0.77).
#> Model 3: GEE working correlation: ar1 (alpha = 0.94).
```

Lower QIC is better for choosing the working correlation; QICu is the
variant for comparing mean models under a fixed structure (Pan, 2001).
The footer discloses each model’s structure on its own line. Note that
the estimates barely move across columns – that is the GEE consistency
property at work – while the standard errors differ, which is exactly
what the working correlation is for.

`qic` and `qicu` are computed only when you ask for them:
[`geepack::QIC()`](https://rdrr.io/pkg/geepack/man/QIC.html) silently
refits the independence model, and the default table should not pay that
cost for a number it does not display.

## The displayed clusters are the clusters of the inference

`geepack` defines clusters as **consecutive runs** of identical `id =`
values. If the data are not sorted by `id`, a subject whose rows are
scattered becomes many small “clusters”, and the sandwich variance is
computed over those fragments – a classic and silent GEE pitfall:

``` r

set.seed(3)
panel <- data.frame(id = rep(1:30, each = 4), x = rnorm(120))
panel$y <- panel$x + rep(rnorm(30), each = 4) + rnorm(120)
shuffled <- panel[sample(nrow(panel)), ]

fit_shuffled <- geeglm(y ~ x, id = id, data = shuffled, family = gaussian)
table_regression(fit_shuffled, show_fit_stats = c("nobs", "n_groups", "max_cluster_size"))
#> Population-averaged linear regression (GEE): y
#> 
#>  Variable         │   B      SE      95% CI        p   
#> ──────────────────┼────────────────────────────────────
#>  (Intercept)      │  -0.14  0.15  [-0.43, 0.15]   .341 
#>  x                │   1.12  0.18  [ 0.77, 1.48]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                │ 120                                
#>  N (id)           │ 118                                
#>  Max cluster size │   2                                
#> 
#> Note. Population-averaged linear regression (GEE).
#> Std. errors: Robust sandwich (GEE), clusters by id.
#> GEE working correlation: independence.
```

The table reports well over a hundred clusters for what the analyst
knows to be 30 subjects – because that is what the model actually used.
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
deliberately reads the cluster structure from the fit (`geese$clusz`,
what [`summary()`](https://rdrr.io/r/base/summary.html) reports as
*Number of clusters*) rather than counting unique `id` values, so the
table *diagnoses* the mistake instead of papering over it. The remedy is
to sort by `id` before fitting (and to use geeglm’s `waves =` argument
when series are incomplete):

``` r

sorted <- shuffled[order(shuffled$id), ]
fit_sorted <- geeglm(y ~ x, id = id, data = sorted, family = gaussian)
table_regression(fit_sorted, show_fit_stats = c("nobs", "n_groups", "max_cluster_size"))
#> Population-averaged linear regression (GEE): y
#> 
#>  Variable         │   B      SE      95% CI        p   
#> ──────────────────┼────────────────────────────────────
#>  (Intercept)      │  -0.14  0.23  [-0.59, 0.31]   .540 
#>  x                │   1.12  0.19  [ 0.76, 1.49]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                │ 120                                
#>  N (id)           │  30                                
#>  Max cluster size │   4                                
#> 
#> Note. Population-averaged linear regression (GEE).
#> Std. errors: Robust sandwich (GEE), clusters by id.
#> GEE working correlation: independence.
```

## Few clusters: the jackknife variants

The sandwich estimator is asymptotic in the number of clusters; with
only a few dozen, it tends to be anti-conservative. `geeglm` offers
jackknife approximations as alternatives (`std.err = "jack"`, `"j1s"`,
`"fij"`; Halekoh et al., 2006). The estimator choice lives on the fit,
and the table reads and names whatever the fit computed:

``` r

fit_j1s <- geeglm(
  Weight ~ Time + Cu,
  id      = Pig,
  data    = dietox,
  family  = gaussian,
  corstr  = "exchangeable",
  std.err = "j1s"
)
table_regression(fit_j1s)
#> Population-averaged linear regression (GEE): Weight
#> 
#>  Variable         │   B      SE       95% CI        p   
#> ──────────────────┼─────────────────────────────────────
#>  (Intercept)      │  15.42  1.02  [13.42, 17.43]  <.001 
#>  Time             │   6.94  0.08  [ 6.79,  7.09]  <.001 
#>  Cu:              │                                     
#>    Cu000 (ref.)   │    –     –          –          –    
#>    Cu035          │  -0.84  1.56  [-3.90,  2.23]   .593 
#>    Cu175          │   1.77  1.88  [-1.90,  5.45]   .344 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                │ 861                                 
#>  N (Pig)          │  72                                 
#>  Max cluster size │  12                                 
#> 
#> Note. Population-averaged linear regression (GEE).
#> Std. errors: One-step jackknife (GEE), clusters by Pig.
#> GEE working correlation: exchangeable (alpha = 0.77).
```

The footer now reads *One-step jackknife (GEE), clusters by Pig* – the
displayed uncertainty is always the fit’s own, never a spicy
recomputation.

## A binomial GEE end to end

The `respiratory` trial (111 patients, four visits) illustrates the full
workflow for a binary outcome. `exponentiate = TRUE` displays
population-averaged odds ratios, and the opt-in `"n_events"` column adds
events/N per group – computed on the estimation sample:

``` r

data(respiratory, package = "geepack")
respiratory$outcome <- as.integer(respiratory$outcome)
respiratory$subject <- interaction(respiratory$center, respiratory$id)
# Placebo as the reference level, so the displayed row is the
# active-treatment odds ratio.
respiratory$treat <- relevel(respiratory$treat, ref = "P")

fit_resp <- geeglm(
  outcome ~ treat + age + baseline,
  id     = subject,
  data   = respiratory,
  family = binomial,
  corstr = "exchangeable"
)
table_regression(
  fit_resp,
  exponentiate = TRUE,
  show_columns = c("n_events", "b", "ci", "p")
)
#> Population-averaged logistic regression (GEE): outcome
#> 
#>  Variable         │ Events/N   OR      95% CI        p   
#> ──────────────────┼──────────────────────────────────────
#>  (Intercept)      │  248/444  0.45  [0.18,  1.10]   .080 
#>  treat:           │                                      
#>    P (ref.)       │  101/228   –          –         –    
#>    A              │  147/216  3.44  [1.81,  6.54]  <.001 
#>  age              │  248/444  0.99  [0.97,  1.01]   .338 
#>  baseline         │  248/444  7.26  [3.86, 13.64]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                │      444                             
#>  N (subject)      │      111                             
#>  Max cluster size │        4                             
#> 
#> Note. Population-averaged logistic regression (GEE).
#> Std. errors: Robust sandwich (GEE), clusters by subject.
#> GEE working correlation: exchangeable (alpha = 0.35).
#> OR = odds ratio.
#> Coefficients exponentiated and displayed as OR; CI bounds exponentiated.
```

This odds ratio is marginal: it compares the odds of a positive outcome
between the treated and untreated *populations*. The subject-specific
odds ratio from the matching mixed model is larger – the attenuation of
the first section – and the [mixed-effects
vignette](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md)
shows the two side by side.

Average marginal effects translate the odds ratio onto the probability
scale, and for a GEE fit their uncertainty inherits the sandwich
covariance automatically:

``` r

table_regression(
  fit_resp,
  show_columns = c("b", "p", "ame", "ame_ci")
)
#> Warning: `"ame"` and `"p"` shown without `"ame_p"`: the `p` column is for B (or beta), not the AME. They can differ under non-linear links or interactions.
#> ℹ Add `"ame_p"` to display the AME-specific p-value.
#> Population-averaged logistic regression (GEE): outcome
#> 
#>  Variable         │   B       p     AME      95% CI     
#> ──────────────────┼─────────────────────────────────────
#>  (Intercept)      │  -0.80   .080                       
#>  treat:           │                                     
#>    P (ref.)       │    –     –       –          –       
#>    A              │   1.23  <.001   0.24  [ 0.12, 0.36] 
#>  age              │  -0.01   .338  -0.00  [-0.01, 0.00] 
#>  baseline         │   1.98  <.001   0.41  [ 0.30, 0.53] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                │ 444                                 
#>  N (subject)      │ 111                                 
#>  Max cluster size │   4                                 
#> 
#> Note. Population-averaged logistic regression (GEE).
#> Std. errors: Robust sandwich (GEE), clusters by subject.
#> GEE working correlation: exchangeable (alpha = 0.35).
#> AME = average marginal effect.
```

The AME column reads as percentage-point differences in the
population-averaged probability of a positive outcome – for the
treatment row, between the treated and placebo populations.

## What spicy refuses for GEE, and why

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
treats requests that GEE cannot honour as hard, explained errors rather
than silent approximations:

- **`vcov = "HC1"` / `"CR2"` and `cluster =`.** GEE inference is robust
  by construction: the sandwich, clustered on the model’s own `id =`,
  *is* the default display. There is nothing for spicy’s robust-variance
  machinery to add; to change the estimator, refit with geeglm’s
  `std.err =` option, and to change the clustering, refit with a
  different `id =`.
- **`standardized`.** There is no established convention for
  standardizing population-averaged coefficients: the response SD mixes
  within- and between-cluster variance. Standardize predictors before
  fitting if magnitudes must be compared.
- **`nested = TRUE` and likelihood-based statistics.** GEE is estimated
  by quasi-likelihood: there is no likelihood, hence no likelihood-ratio
  change test, no AIC and no pseudo-R². Compare working models with QIC,
  or use `geepack`’s [`anova()`](https://rdrr.io/r/stats/anova.html)
  Wald tests outside the table.

Each refusal names its reason and its remedy in the error message – in a
table meant for publication, a wrong convention silently applied is
worse than an error.

``` r

table_regression(fit, vcov = "HC3")
#> Error in `.gee_refuse_vcov()`:
#> ! `vcov = "HC3"` is not available for `geeglm` models.
#> ℹ GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
#> ℹ To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
```

## Presentation-ready output

Everything above renders identically through the rich output engines. A
final example with `flextable`, ready for a Word manuscript:

``` r

table_regression(
  fit_resp,
  exponentiate = TRUE,
  show_columns = c("n_events", "b", "ci", "p"),
  output = "flextable"
)
```

| Variable         | Events/N | OR   | 95% CI |       | p      |
|------------------|----------|------|--------|-------|--------|
|                  |          |      | LL     | UL    |        |
| (Intercept)      | 248.00   | 0.45 | 0.18   |  1.10 |  .080  |
| treat:           |          |      |        |       |        |
|  P (ref.)        |   –      | –    | –      |  –    | –      |
|  A               | 147.00   | 3.44 | 1.81   |  6.54 | \<.001 |
| age              | 248.00   | 0.99 | 0.97   |  1.01 |  .338  |
| baseline         | 248.00   | 7.26 | 3.86   | 13.64 | \<.001 |
| n                | 444      |      |        |       |        |
| N (subject)      | 111      |      |        |       |        |
| Max cluster size |   4      |      |        |       |        |

Population-averaged logistic regression (GEE): outcome {.table
.cl-403d88b0 quarto-disable-processing="true"}

*Note.* Population-averaged logistic regression (GEE). Std. errors:
Robust sandwich (GEE), clusters by subject. GEE working correlation:
exchangeable (alpha = 0.35). OR = odds ratio. Coefficients exponentiated
and displayed as OR; CI bounds exponentiated.

## References

- Halekoh, U., Højsgaard, S., & Yan, J. (2006). The R package geepack
  for generalized estimating equations. *Journal of Statistical
  Software*, 15(2), 1–11.
- Hubbard, A. E., Ahern, J., Fleischer, N. L., Van der Laan, M.,
  Lippman, S. A., Jewell, N., Bruckner, T., & Satariano, W. A. (2010).
  To GEE or not to GEE: Comparing population average and mixed models
  for estimating the associations between neighborhood risk factors and
  health. *Epidemiology*, 21(4), 467–474.
- Liang, K.-Y., & Zeger, S. L. (1986). Longitudinal data analysis using
  generalized linear models. *Biometrika*, 73(1), 13–22.
- Pan, W. (2001). Akaike’s information criterion in generalized
  estimating equations. *Biometrics*, 57(1), 120–125.
- Zeger, S. L., Liang, K.-Y., & Albert, P. S. (1988). Models for
  longitudinal data: A generalized estimating equation approach.
  *Biometrics*, 44(4), 1049–1060.
