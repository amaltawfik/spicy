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
in a single table; the class-by-class map is [*Supported
models*](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.md).

## Why these models, and on what grounds

**The statistical trigger is the design.** Repeated measures on the same
subjects, patients within clinics, residents within neighbourhoods:
observations within a cluster are correlated, and an ordinary GLM that
assumes independence gets its coefficients roughly right but their
*variances* wrong – the casualty is every standard error, confidence
interval, and p-value in the table (Hubbard et al., 2010). Whenever the
data are clustered and the within-cluster association is non-negligible,
*some* cluster-aware model is required. That much a statistic can tell
you.

**The choice among cluster-aware models is not a statistical test.**
Once clustering is acknowledged, two families answer two different
questions. A mixed model is **subject-specific** (conditional): what
happens to *this* subject’s outcome when their predictor changes,
holding their random effect fixed. GEE fits a marginal model and is
**population-averaged**: how does the *average* outcome differ between
populations that differ in the predictor (Zeger, Liang & Albert, 1988).
Zeger and colleagues’ own example makes the contrast concrete: with
smoking as the exposure and respiratory infection as the outcome, the
population-averaged model estimates the difference in infection *rates*
between smokers and non-smokers, while the subject-specific model
estimates the change in an *individual’s* probability of infection if
they took up smoking. No index computed from the data arbitrates between
those two targets. As Fitzmaurice, Laird and Ware (2011, p. 342) put it,
the choice “cannot be made through any automatic procedure. Rather, the
choice must be made on subject-matter grounds.” State the scientific
question, and the estimand – hence the model family – follows (Hubbard
et al., 2010). Fitzmaurice and colleagues’ own shorthand for the
contrast: a physician weighing a treatment for the patient in front of
them wants the subject-specific effect; a public-health researcher
weighing the same treatment for a population wants the
population-averaged one. And when both questions matter, “there is no
contradiction in reporting estimates of both” (Fitzmaurice et al., 2011,
Ch. 16) – which is precisely what a side-by-side table does.

For a linear model the two coincide, so the stakes are low. On a logit
link they do not: with a random-intercept variance \\\sigma^2\\, the
population-averaged coefficient is attenuated by approximately
\\1/\sqrt{1 + 0.346\\\sigma^2}\\ relative to the subject-specific one
(Zeger et al., 1988; the constant is \\c^2\\ with \\c =
16\sqrt{3}/(15\pi)\\), so the conditional odds ratio is always farther
from 1. The magnitudes are worth knowing: at \\\sigma^2 = 3.5\\ the
subject-specific coefficient is about 1.5 times the marginal one, at
\\\sigma^2 = 9\\ about twice (Fitzmaurice et al., 2011). The log link is
the notable exception – with a random intercept, every coefficient
except the intercept coincides, so conditional and marginal rate ratios
are directly comparable. Neither estimand is “biased”: they answer
different questions. The side-by-side table in the [mixed-effects
vignette](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md)
shows the gap on real data.

**What GEE buys.** When the marginal question is the right one, GEE has
three properties that likelihood-based alternatives cannot match. First,
it needs no distributional assumptions: for discrete longitudinal data
there is no convenient analogue of the multivariate normal – a full
joint distribution for ten binary repeated measures involves over a
thousand association parameters (Fitzmaurice et al., 2011) – and GEE
sidesteps the problem by modelling only the mean, the variance function,
and the pairwise association – while remaining, in many longitudinal
designs, nearly as efficient as maximum likelihood (Fitzmaurice et al.,
2011). Second, the coefficient estimates are consistent even when the
assumed within-cluster correlation structure is wrong (Liang & Zeger,
1986). Third, the standard errors come from a cluster-robust sandwich
estimator *by construction* – robustness is not an option bolted on
after the fact, it is the method. A mixed model, by contrast, leans on
an assumed random-effects distribution that the data cannot verify
(Hubbard et al., 2010).

**What GEE costs – the criteria that argue against it.** Three
conditions are worth checking before committing. *Efficiency*: with a
badly misspecified working correlation, coefficient estimation can lose
substantial efficiency relative to the correct structure (Pan, 2001) –
the QIC section below is the remedy. *Missing data*: GEE estimates are
consistent only when missingness is completely at random (MCAR, in
Rubin’s 1976 sense; Halekoh, Højsgaard & Yan, 2006; Fitzmaurice et al.,
2011); when dropout depends on the observed history (MAR), the remedies
are inverse-probability-weighted GEE or a likelihood-based mixed model.
*Clusters*: the sandwich is asymptotic in the number of clusters – see
the jackknife section below for the small-\\K\\ remedy. Finally,
marginal models assume the current response depends only on current
covariates; a time-varying covariate that responds to earlier outcomes
(exercise adjusted after a bad glucose reading) violates that assumption
and calls for greater care (Fitzmaurice et al., 2011).

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
#>  Variable       │ p (B) 
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

The division of labor between the two criteria is Pan’s (2001): pick the
working correlation by the smallest **QIC**; use **QICu** only for
comparing *mean models* (covariate sets) under a fixed structure – it
cannot select the correlation. Read the rule against the table honestly:
here QIC rejects AR(1) decisively but scores independence and
exchangeable a tenth of a point apart – a meaningless margin. A near-tie
means the data cannot distinguish the structures, and the choice falls
back on substantive grounds (repeated weight measurements of the same
pig are exchangeable-shaped); the rule earns its keep on the decisions
it *can* make, like ruling out AR(1) here. The stake is efficiency, not
consistency: all three columns estimate the same population quantity and
converge to it under any structure, but in a finite sample the weighting
differs – compare the `Cu` rows under AR(1) with the other two columns –
and a badly chosen structure can cost a substantial share of the
precision the data could deliver (Pan, 2001). The footer discloses each
model’s structure on its own line.

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
to sort by `id` before fitting – and, when series are incomplete or
unequally spaced, to pass geeglm’s `waves =` argument so that
time-dependent structures such as `"ar1"` know each observation’s
position (Halekoh et al., 2006). Incomplete series also raise the
missing-data question from the first section: GEE remains valid only
when the missing observations are MCAR.

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

The sandwich estimator is asymptotic in the number of clusters: with few
clusters it is biased *downward* – the nominal standard errors are too
small, and the estimator itself becomes unstable (Fitzmaurice et al.,
2011). At 30 clusters or fewer (the threshold the geepack authors cite,
following Paik, 1988) the jackknife variance estimators are the
recommended alternative (Halekoh et al., 2006). `geeglm` offers three
(`std.err = "jack"`, `"j1s"`, `"fij"`); the approximate and one-step
versions are far cheaper than the fully iterated one and agree well with
it in simulations (Halekoh et al., 2006). The estimator choice lives on
the fit, and the table reads and names whatever the fit computed:

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
  show_columns = c("b", "ame", "ame_ci", "ame_p")
)
#> Population-averaged logistic regression (GEE): outcome
#> 
#>  Variable         │   B      AME      95% CI        p   
#> ──────────────────┼─────────────────────────────────────
#>  (Intercept)      │  -0.80                              
#>  treat:           │                                     
#>    P (ref.)       │    –      –          –         –    
#>    A              │   1.23   0.24  [ 0.12, 0.36]  <.001 
#>  age              │  -0.01  -0.00  [-0.01, 0.00]   .332 
#>  baseline         │   1.98   0.41  [ 0.30, 0.53]  <.001 
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

The AME column reads on the probability scale: 0.24 for the treatment
row means a 24-percentage-point higher population-averaged probability
of a positive outcome in the treated population than under placebo.

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
| (Intercept)      | 248/444  | 0.45 | 0.18   |  1.10 |  .080  |
| treat:           |          |      |        |       |        |
| P (ref.)         | 101/228  | –    | –      |  –    | –      |
| A                | 147/216  | 3.44 | 1.81   |  6.54 | \<.001 |
| age              | 248/444  | 0.99 | 0.97   |  1.01 |  .338  |
| baseline         | 248/444  | 7.26 | 3.86   | 13.64 | \<.001 |
| n                |     444  |      |        |       |        |
| N (subject)      |     111  |      |        |       |        |
| Max cluster size |       4  |      |        |       |        |

Population-averaged logistic regression (GEE): outcome {.table
.cl-c452b3fe quarto-disable-processing="true"}

*Note.* Population-averaged logistic regression (GEE). Std. errors:
Robust sandwich (GEE), clusters by subject. GEE working correlation:
exchangeable (alpha = 0.35). OR = odds ratio. Coefficients exponentiated
and displayed as OR; CI bounds exponentiated.

## References

- Fitzmaurice, G. M., Laird, N. M., & Ware, J. H. (2011). *Applied
  Longitudinal Analysis* (2nd ed.). Wiley. (Chapters 12, 13, and 16:
  marginal models, GEE estimation, and the contrast with mixed effects
  models.)
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
- Paik, M. C. (1988). Repeated measurement analysis for nonnormal data
  in small samples. *Communications in Statistics — Simulation and
  Computation*, 17(4), 1155–1171.
- Pan, W. (2001). Akaike’s information criterion in generalized
  estimating equations. *Biometrics*, 57(1), 120–125.
- Rubin, D. B. (1976). Inference and missing data. *Biometrika*, 63(3),
  581–592.
- Zeger, S. L., Liang, K.-Y., & Albert, P. S. (1988). Models for
  longitudinal data: A generalized estimating equation approach.
  *Biometrics*, 44(4), 1049–1060.
