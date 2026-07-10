# Survival regression tables

``` r

library(spicy)
library(survival)
```

This vignette covers **survival (time-to-event) regression** — models
for the **time until an event occurs**, with censoring for the subjects
whose event is not observed within follow-up. The companion vignette
[*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
covers the shared mechanics (`vcov`, `ci_level`, output formats,
multi-model layouts, broom integration); here we focus on what is
specific to survival fits: hazard ratios and time ratios, event counts
and concordance, and the variance estimators of multi-centre studies.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
supports four survival engines:

- **[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)**
  — the semiparametric Cox proportional hazards model, the field
  default;
- **[`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html)**
  — parametric accelerated failure time (AFT) models;
- **[`rms::cph()`](https://rdrr.io/pkg/rms/man/cph.html)** — the Cox
  model in the `rms` ecosystem;
- **[`flexsurv::flexsurvreg()`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md)**
  — fully parametric models over a wide family of distributions.

The running example is
[`survival::lung`](https://rdrr.io/pkg/survival/man/lung.html): survival
of 228 patients with advanced lung cancer, with age, sex, and the ECOG
performance score (0 = fully active, higher is worse) as predictors. One
row lacks `ph.ecog` and one lacks the enrolling institution, so we
declare the analytic sample explicitly — the same 226 rows serve every
model and, later, supply the cluster variable:

``` r

lung2 <- na.omit(lung[, c("time", "status", "age", "sex",
                          "ph.ecog", "inst")])
lung2$sex <- factor(lung2$sex, levels = 1:2,
                    labels = c("Male", "Female"))
nrow(lung2)
#> [1] 226
```

## The Cox model in one paragraph

The Cox model works on the **hazard** — the instantaneous event rate
among those still at risk. A one-unit increase in a predictor multiplies
that hazard by a constant factor — the exponentiated coefficient — at
every point in time (the **proportional-hazards assumption**), and the
baseline hazard itself is left completely unspecified: that is the
semiparametric trick that made the model ubiquitous in applied work (Cox
1972; Therneau & Grambsch 2000). Everything below also assumes censoring
is **uninformative**: given the covariates, subjects censored at time
*t* must be representative of those still at risk at *t* — an assumption
the observed data cannot verify. Two consequences shape the table. There
is **no intercept row** — the baseline hazard absorbs it — and there is
no residual variance, so the model-fit block reports what a survival
reader expects instead: the **number of events** and the
**concordance**.

## Basic table: hazard ratios

``` r

cx <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung2)
table_regression(cx, exponentiate = TRUE)
#> Cox proportional hazards regression: Surv(time, status)
#> 
#>  Variable      │   HR      SE      95% CI       p   
#> ───────────────┼────────────────────────────────────
#>  age           │    1.01  0.01  [0.99, 1.03]   .225 
#>  sex:          │                                    
#>    Male (ref.) │     –     –         –         –    
#>    Female      │    0.57  0.10  [0.41, 0.80]  <.001 
#>  ph.ecog       │    1.60  0.18  [1.28, 2.00]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │  226                               
#>  N events      │  163                               
#>  AIC           │ 1454.2                             
#> 
#> Note. Cox proportional hazards regression.
#> Std. errors: Wald asymptotic (z).
#> Concordance C = 0.64 (SE = 0.03).
#> HR = hazard ratio.
#> Coefficients exponentiated and displayed as HR; SE on the HR scale (delta method); CI bounds exponentiated (asymmetric).
```

Reading the table:

- `exponentiate = TRUE` turns each log-hazard coefficient into a
  **hazard ratio**. Women’s hazard is 0.57 times men’s — a 43% lower
  hazard at any given moment, all else equal; each ECOG point multiplies
  the hazard by 1.60. Leave `exponentiate` off for the log-hazard `B`
  column.
- Inference is **Wald-z** on the log scale, matching `summary.coxph()`
  and Stata `stcox`; the statistic and p-value are invariant under the
  exponentiation.
- The footer reports **events** (163 deaths among 226 patients — the
  precision and power of a survival model are governed chiefly by its
  event count, not its n) and the **concordance** C = 0.64: the
  probability that, of two comparable patients, the one predicted to
  fail earlier actually does. 0.5 is a coin flip; 0.64 is modest
  discrimination (Harrell 2015).

The table reports the model, not its assumption: check proportionality
with
[`survival::cox.zph()`](https://rdrr.io/pkg/survival/man/cox.zph.html)
before publishing (Therneau & Grambsch 2000):

``` r

cox.zph(cx)
#>         chisq df    p
#> age     0.244  1 0.62
#> sex     2.475  1 0.12
#> ph.ecog 2.353  1 0.13
#> GLOBAL  4.975  3 0.17
```

Each covariate gets its own test of the proportional-hazards assumption,
plus a global test. The smallest p here is .12 (sex) and the global test
gives p = .17 — little evidence against proportionality. When the test
rejects, the two standard remedies flow through the same table without
ceremony: stratify the offending categorical factor
([`strata()`](https://rdrr.io/pkg/survival/man/strata.html) terms are
absorbed into the baseline hazard, no coefficient row) or give the
covariate a time-varying effect via counting-process data
(`Surv(tstart, tstop, event)`); both render like any other fit.

## Why there is no AME column

An `"ame"` request on a Cox fit is refused with an explanation rather
than silently honoured: an average marginal effect needs a response
scale, and a Cox model deliberately does not commit to one — a
per-coefficient “effect on survival” would be ambiguous (at what time?
on which scale?), and the delta-method standard errors the generic AME
machinery would attach to it are unreliable for Cox fits. The hazard
ratio *is* the per-coefficient summary of this model. Absolute-scale
summaries exist, but they require the analyst to choose a **time
horizon** first — which is exactly what the next section does, with
bootstrap inference in place of the delta method.

## Absolute effects: RMST and risk differences

A hazard ratio answers “at what relative rate?”, never “how much
longer?” or “how many fewer?”. Women’s hazard is 0.57 times men’s — but
how many extra months of life is that, and how many fewer deaths by the
end of the first year? Those are the questions patients and policy
makers actually ask, and the methodological literature increasingly
calls for these answers to be reported alongside the hazard ratio
(Royston & Parmar 2013; Uno et al. 2014). They also settle a workflow
question the assumption check left open: when
[`cox.zph()`](https://rdrr.io/pkg/survival/man/cox.zph.html) rejects
proportionality, the hazard ratio degrades into a follow-up-weighted
average of a changing effect, while the RMST difference over `[0, tau]`
remains a well-defined estimand — in that case the absolute summaries
lead the report rather than accompany it. Two absolute summaries do the
job:

- the **restricted mean survival time (RMST) difference**: the
  difference in mean event-free time over a fixed window `[0, tau]` —
  the area between the two survival curves up to `tau`;
- the **risk difference**: the difference in the probability of having
  had the event by a landmark time.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
computes both from the fitted Cox model by **g-computation** (regression
standardization): every patient’s survival curve is predicted twice,
once under each level of the predictor, keeping their other covariates
as observed; the curves are averaged, and the two standardized curves
are integrated to `tau` (RMST) or evaluated at `at_time` and
complemented (risk = 1 − survival). Standard errors come from a
nonparametric bootstrap, so expect the chunk to take a few seconds; the
200 replicates below keep the vignette light, and the default
`boot_n = 1000` is the better choice for a publication table.

``` r

set.seed(7)
table_regression(cx,
                 show_columns = c("b", "rmst", "rmst_ci",
                                  "risk_diff", "risk_diff_ci"),
                 tau = 365, at_time = 365,
                 exponentiate = TRUE, boot_n = 200)
#> Cox proportional hazards regression: Surv(time, status)
#> 
#>  Variable      │   HR     dRMST (365)       95% CI       dRisk (365) 
#> ───────────────┼─────────────────────────────────────────────────────
#>  age           │    1.01        -0.84  [ -2.27,   0.60]         0.00 
#>  sex:          │                                                     
#>    Male (ref.) │     –                                               
#>    Female      │    0.57        40.50  [ 16.65,  64.35]        -0.19 
#>  ph.ecog       │    1.60       -38.21  [-58.73, -17.70]         0.15 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │  226                                                
#>  N events      │  163                                                
#>  AIC           │ 1454.2                                              
#> 
#>  Variable      │     95% CI     
#> ───────────────┼────────────────
#>  age           │ [-0.00,  0.01] 
#>  sex:          │                
#>    Male (ref.) │                
#>    Female      │ [-0.30, -0.08] 
#>  ph.ecog       │ [ 0.08,  0.22] 
#> 
#> Note. Cox proportional hazards regression.
#> Std. errors: Wald asymptotic (z).
#> Concordance C = 0.64 (SE = 0.03).
#> dRMST = difference in restricted mean survival time over [0, 365]; dRisk = difference in cumulative incidence at 365; adjusted by g-computation from the fitted model, SEs by nonparametric bootstrap (200 replicates).
#> HR = hazard ratio.
#> Coefficients exponentiated and displayed as HR; CI bounds exponentiated.
```

The absolute columns re-express the same model on interpretable scales,
each averaged over the sample’s covariate mix. That cuts both ways:
computed from the fit, they inherit its assumptions — with
proportionality in doubt, estimate the RMST nonparametrically (per-arm
Kaplan–Meier, as in `survRM2`) rather than standardizing a misspecified
model. Stratified fits work too: standardization keeps each subject’s
own stratum baseline — only the exposure is set counterfactually — and
the note discloses it; the strata variable itself gets no contrast row,
exactly as it has no hazard ratio. Being female adds an average of
**40.5 event-free days** over the first year (95% CI 17 to 64) and
lowers the one-year risk of death by **19 percentage points** (95% CI 8
to 30); each additional ECOG point costs 38 days and adds 15 percentage
points to one-year risk. And the age row teaches that the contrast unit
matters as much as the scale: per **year** of age the effect is
invisible (HR 1.01, under a day of RMST), but per **decade** — the
clinically natural unit — the same fit implies HR 1.12 and about nine
fewer event-free days. Rescale the predictor when the per-unit contrast
is not the meaningful one.

Two design decisions shape these columns. First, **the horizon is
mandatory**: an RMST without its `tau` is not an estimand, so
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
refuses to guess — pass a clinically meaningful time, or
`tau = "minmax"` for the largest window every compared group can support
(the value used is disclosed in the table note). Second, the contrast is
level against reference for factor predictors and a +1-unit shift for
continuous predictors, mirroring the AME convention for other model
families — so the columns read the same way across your tables.

## Multi-centre data: cluster-robust variance

The `lung` patients were enrolled by 18 institutions, and outcomes
within an institution may correlate. The `CR*` request routes to the
**Lin & Wei (1989)** robust variance — the grouped score (dfbeta)
sandwich that `coxph(..., cluster = )` and Stata `stcox, vce(cluster)`
use:

``` r

table_regression(cx, vcov = "CR0", cluster = ~inst,
                 exponentiate = TRUE)
#> Registered S3 method overwritten by 'clubSandwich':
#>   method    from    
#>   bread.mlm sandwich
#> Cox proportional hazards regression: Surv(time, status)
#> 
#>  Variable      │   HR      SE      95% CI       p   
#> ───────────────┼────────────────────────────────────
#>  age           │    1.01  0.01  [1.00, 1.03]   .103 
#>  sex:          │                                    
#>    Male (ref.) │     –     –         –         –    
#>    Female      │    0.57  0.06  [0.46, 0.71]  <.001 
#>  ph.ecog       │    1.60  0.19  [1.27, 2.01]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │  226                               
#>  N events      │  163                               
#>  AIC           │ 1454.2                             
#> 
#> Note. Cox proportional hazards regression.
#> Std. errors: cluster-robust (Lin-Wei), clusters by inst.
#> Concordance C = 0.64 (SE = 0.03).
#> HR = hazard ratio.
#> Coefficients exponentiated and displayed as HR; SE on the HR scale (delta method); CI bounds exponentiated (asymmetric).
```

Two lessons here. First, the mechanics: the cluster variable must
provide **one value per row of the model data** — declaring the analytic
sample up front (as we did) keeps the cluster aligned when predictors
carry missing values. Second, the direction: robust standard errors are
not always larger — the sex SE *shrinks* here. Clustering corrects the
variance in whichever direction the within-centre correlation points;
treating it as a conservative inflation ritual misreads it. The robust
variance corrects the standard errors while leaving the estimates
marginal (population-averaged); the modeling alternative is a shared
**frailty** term —
`coxph(Surv(time, status) ~ age + sex + ph.ecog + frailty(inst), data = lung2)`
— whose hazard ratios are instead conditional on the centre effect
(Therneau & Grambsch 2000, ch. 9). Which estimand the analysis needs
decides between them.

## Hierarchical Cox models

Does adding the performance score improve on the age-and-sex model?
`nested = TRUE` compares adjacent models by the **partial-likelihood
ratio test** (the `anova.coxph()` convention) and appends the change
rows:

``` r

cx0 <- coxph(Surv(time, status) ~ age + sex, data = lung2)
table_regression(list(cx0, cx), nested = TRUE, exponentiate = TRUE,
                 show_columns = c("b", "p"))
#> Hierarchical Cox proportional hazards regression: Surv(time, status)
#> 
#>                     Model 1          Model 2     
#>                  ──────────────  ─────────────── 
#>  Variable      │   HR       p       HR       p   
#> ───────────────┼─────────────────────────────────
#>  age           │    1.02   .062     1.01    .225 
#>  sex:          │                                 
#>    Male (ref.) │     –     –         –      –    
#>    Female      │    0.60   .003     0.57   <.001 
#>  ph.ecog       │                    1.60   <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │  226             226            
#>  N events      │  163             163            
#>  AIC           │ 1469.0          1454.2          
#>  Δχ²           │     –            +16.78         
#>  p (change)    │     –              <.001        
#> 
#> Note. Cox proportional hazards regression models.
#> Std. errors: Wald asymptotic (z).
#> Model 1: Concordance C = 0.60 (SE = 0.03).
#> Model 2: Concordance C = 0.64 (SE = 0.03).
#> HR = hazard ratio.
#> Coefficients exponentiated and displayed as HR; CI bounds exponentiated.
```

Adding `ph.ecog` buys a chi-squared change of 16.78 on one degree of
freedom (p \< .001), an AIC drop of 14.8, and a rise in concordance from
0.60 to 0.64 — three readings of the same improvement, on the
likelihood, information, and discrimination scales respectively.

## The univariable screen

Epidemiological analyses often begin one step earlier: before any
multivariable model, each candidate predictor is examined in its own Cox
model, and the two stages are reported side by side.
[`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)
runs that workflow in one call — `outcome` takes the
[`Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) expression, and
each column group is fitted, and its n and events counted, on its own
estimation sample:

``` r

table_regression_uv(lung2, outcome = Surv(time, status),
                    predictors = c(age, sex, ph.ecog),
                    method = "coxph", exponentiate = TRUE,
                    show_columns = c("n", "n_events", "b", "ci", "p"))
#> Univariable and multivariable Cox regression: Surv(time, status)
#> 
#>                                Univariable                   Multivariable   
#>                  ────────────────────────────────────────  ───────────────── 
#>  Variable      │  N   Events/N   HR      95% CI       p     Events/N     HR  
#> ───────────────┼─────────────────────────────────────────────────────────────
#>  age           │ 226   163/226  1.02  [1.00, 1.04]   .041      163/226  1.01 
#>  sex:          │                                                             
#>    Male (ref.) │   –   110/136   –         –         –         110/136   –   
#>    Female      │ 226     53/90  0.59  [0.43, 0.82]   .002        53/90  0.57 
#>  ph.ecog       │ 226   163/226  1.61  [1.29, 2.02]  <.001      163/226  1.60 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │                                                 226         
#>  N events      │                                                 163         
#>  AIC           │                                                1454.2       
#> 
#>                     Multivariable    
#>                  ─────────────────── 
#>  Variable      │    95% CI       p   
#> ───────────────┼─────────────────────
#>  age           │ [0.99, 1.03]   .225 
#>  sex:          │                     
#>    Male (ref.) │      –         –    
#>    Female      │ [0.41, 0.80]  <.001 
#>  ph.ecog       │ [1.28, 2.00]  <.001 
#> 
#> Note. Cox proportional hazards regression models.
#> Std. errors: Wald asymptotic (z).
#> Concordance C = 0.64 (SE = 0.03).
#> HR = hazard ratio.
#> Coefficients exponentiated and displayed as HR; CI bounds exponentiated.
```

The `Events/N` column carries the information a survival reader checks
first — 53 of the 90 women died against 110 of the 136 men — and the
paired columns support the screen’s real question: **does adjustment
move an estimate?** Age is the instructive row. Alone it looks
significant (HR 1.02, p = .041); adjusted for sex and performance status
it is not (p = .225) — its univariable signal was partly carried by the
worse performance scores of the older patients. Reading the two columns
together is what protects the final model from both false leads and
premature exclusions: a predictor should not enter or leave on its
univariable p-value alone. The screen also takes the estimand columns:
with `show_columns = c("b", "rmst")` and one shared numeric `tau`, each
predictor’s unadjusted RMST difference sits next to the
covariate-adjusted one from the multivariable fit — the same
univariable-versus-adjusted reading, on the absolute scale.

Two cautions complete the reading. The comparison is clean here only
because the analytic sample was declared up front: every model uses the
same 226 rows, so movement between the columns is adjustment, not sample
change (with missing predictor values, each univariable model fits on
its own larger complete cases — the `N` column and the table note
disclose exactly that). And when the estimate is a hazard ratio,
movement under adjustment is not by itself proof of confounding: hazard
ratios are **non-collapsible**, so adding a strongly prognostic
covariate shifts the estimate even when it is unrelated to the
predictor.

## Accelerated failure time: `survreg()`

The parametric alternative models **time itself** (Wei 1992): covariates
stretch or compress survival time by a constant factor. When is it the
better tool? When the audience needs the time-scale reading, when
proportionality is in doubt — the AFT assumption of a constant time
ratio is a *different* assumption, not a weaker one — or when the
analysis needs a parametric baseline, for instance to extrapolate beyond
the follow-up window. Under `exponentiate = TRUE` the coefficients
become **time ratios** (TR):

``` r

sr <- survreg(Surv(time, status) ~ age + sex + ph.ecog, data = lung2,
              dist = "weibull")
table_regression(sr, exponentiate = TRUE)
#> Weibull AFT regression: Surv(time, status)
#> 
#>  Variable      │   TR       SE         95% CI          p   
#> ───────────────┼───────────────────────────────────────────
#>  (Intercept)   │  800.93  343.75  [345.36, 1857.47]  <.001 
#>  age           │    0.99    0.01  [  0.98,    1.01]   .261 
#>  sex:          │                                           
#>    Male (ref.) │     –       –            –           –    
#>    Female      │    1.50    0.19  [  1.18,    1.92]   .001 
#>  ph.ecog       │    0.71    0.06  [  0.60,    0.84]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │  226                                      
#>  AIC           │ 2261.7                                    
#> 
#> Note. Weibull AFT regression.
#> Std. errors: Wald asymptotic (z).
#> Distribution: Weibull; scale = 0.73.
#> Coefficients exponentiated and displayed as TR; SE on the TR scale (delta method); CI bounds exponentiated (asymmetric).
```

The intercept is back — a parametric model must anchor the time scale,
and exp(intercept) ≈ 801 days is that anchor (the extrapolated
characteristic time at the reference levels and age 0), not a ratio
despite the column header.

Set the TR next to the Cox HR and the numbers *look* contradictory:
women have TR 1.50 but HR 0.57. They are the same finding on opposite
scales — a protective effect **multiplies survival time up** (50%
longer) and **multiplies the hazard down** (43% lower). Neither
direction is more correct; the Weibull is the one family that is
simultaneously AFT and proportional-hazards, and the two scales are
linked by an exact identity within the fit: HR = TR^(−1/scale) =
1.50^(−1/0.73) ≈ 0.574, with the scale parameter read from the footer —
agreeing with the semiparametric Cox estimate (0.573 at full precision,
0.57 as the tables display). The footer names the distribution because
the interpretation depends on it — `exponentiate` produces time ratios
only for log-scale distributions (Weibull, exponential, lognormal,
loglogistic); for an identity-scale distribution (`dist = "gaussian"`)
the request is skipped with a warning, since the coefficients already
act on the time scale.

## `rms::cph()`

Analysts working in the `rms` ecosystem (Harrell 2015) — for its
validation, calibration, and nomogram tooling — fit the Cox model with
[`rms::cph()`](https://rdrr.io/pkg/rms/man/cph.html), and
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
renders it through the same layout. One requirement carried over from
`rms` itself: cluster-robust variance uses
[`rms::robcov()`](https://rdrr.io/pkg/rms/man/robcov.html), which needs
the model matrix and response stored at fit time — fit with
`x = TRUE, y = TRUE` or the robust request fails with exactly that
instruction:

``` r

cph_fit <- rms::cph(Surv(time, status) ~ age + sex, data = lung2,
                    x = TRUE, y = TRUE)
table_regression(cph_fit, vcov = "CR0", cluster = ~inst,
                 exponentiate = TRUE)
#> Cox proportional hazards regression (rms): Surv(time, status)
#> 
#>  Variable      │   HR      SE      95% CI       p   
#> ───────────────┼────────────────────────────────────
#>  age           │    1.02  0.01  [1.00, 1.03]   .018 
#>  sex:          │                                    
#>    Male (ref.) │     –     –         –         –    
#>    Female      │    0.60  0.08  [0.47, 0.78]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │  226                               
#>  N events      │  163                               
#>  AIC           │ 1469.0                             
#> 
#> Note. Cox proportional hazards regression (rms).
#> Std. errors: cluster-robust (Lin-Wei), clusters by inst.
#> HR = hazard ratio.
#> Coefficients exponentiated and displayed as HR; SE on the HR scale (delta method); CI bounds exponentiated (asymmetric).
```

## Fully parametric: `flexsurv`

[`flexsurv::flexsurvreg()`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md)
(Jackson 2016) fits parametric survival models over a wide family of
distributions (and the Royston–Parmar 2002 spline models). The location
coefficients are exponentiated under a generic `exp(B)` header — the
substantive reading (time ratio, hazard ratio) depends on the
distribution, which the footer names together with its ancillary
parameters:

``` r

fs <- flexsurv::flexsurvreg(Surv(time, status) ~ age + sex,
                            data = lung2, dist = "weibull")
table_regression(fs, exponentiate = TRUE)
#> Weibull parametric survival regression: Surv(time, status)
#> 
#>  Variable      │ exp(B)    SE      95% CI       p   
#> ───────────────┼────────────────────────────────────
#>  age           │    0.99  0.01  [0.97, 1.00]   .075 
#>  sex:          │                                    
#>    Male (ref.) │     –     –         –         –    
#>    Female      │    1.46  0.19  [1.13, 1.87]   .003 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │  226                               
#>  AIC           │ 2276.5                             
#> 
#> Note. Weibull parametric survival regression.
#> Std. errors: Wald asymptotic (z).
#> Distribution: Weibull; shape = 1.33, scale = 792.70.
#> exp(B) = exponentiated coefficient.
#> Coefficients exponentiated and displayed as exp(B); SE on the exp(B) scale (delta method); CI bounds exponentiated (asymmetric).
```

Two guardrails apply. A spline model on the `"normal"` scale places its
coefficients on a probit-like scale whose exponential has no meaning, so
`exponentiate = TRUE` is refused there; and covariates on ancillary
parameters (`anc =`) act on their own scales, so `exponentiate = TRUE`
is refused for the whole fit rather than printing meaningless ratios for
the ancillary rows.

## Output formats

Everything above used the default console output. The same table renders
as a raw data frame, a long broom-style tibble, or — with the
corresponding Suggests package — a rich `gt`, `flextable`, `tinytable`,
Excel, or Word table:

``` r

table_regression(cx, exponentiate = TRUE, output = "data.frame")
#>        Variable      HR   SE       95% CI     p
#> 1           age    1.01 0.01 [0.99, 1.03]  .225
#> 2          sex:                                
#> 3   Male (ref.)     –    –        –        –   
#> 4        Female    0.57 0.10 [0.41, 0.80] <.001
#> 5       ph.ecog    1.60 0.18 [1.28, 2.00] <.001
#> 6             n  226                           
#> 7      N events  163                           
#> 8           AIC 1454.2
```

``` r

table_regression(cx, exponentiate = TRUE, output = "gt")
```

[TABLE]

*Note.* Cox proportional hazards regression. Std. errors: Wald
asymptotic (z). Concordance C = 0.64 (SE = 0.03). HR = hazard ratio.
Coefficients exponentiated and displayed as HR; SE on the HR scale
(delta method); CI bounds exponentiated (asymmetric).

[`broom::tidy()`](https://broom.tidymodels.org) returns the long frame;
with `exponentiate = TRUE` the estimates and CI bounds are on the ratio
scale, as displayed:

``` r

broom::tidy(table_regression(cx, exponentiate = TRUE))
#> # A tibble: 3 × 16
#>   model_id outcome outcome_level term  estimate_type estimate std.error conf.low
#>   <chr>    <chr>   <chr>         <chr> <chr>            <dbl>     <dbl>    <dbl>
#> 1 M1       Surv(t… NA            age   B                1.01    0.00937    0.993
#> 2 M1       Surv(t… NA            sexF… B                0.573   0.0963     0.412
#> 3 M1       Surv(t… NA            ph.e… B                1.60    0.183      1.28 
#> # ℹ 8 more variables: conf.high <dbl>, statistic <dbl>, df <dbl>,
#> #   p.value <dbl>, test_type <chr>, is_intercept <lgl>, factor_term <chr>,
#> #   factor_level <chr>
```

## References

- Cox, D. R. (1972). Regression models and life-tables. *Journal of the
  Royal Statistical Society, Series B*, 34(2), 187–220.
- Harrell, F. E. (2015). *Regression Modeling Strategies* (2nd ed.).
  Springer.
- Jackson, C. H. (2016). `flexsurv`: A platform for parametric survival
  modeling in R. *Journal of Statistical Software*, 70(8).
- Lin, D. Y., & Wei, L. J. (1989). The robust inference for the Cox
  proportional hazards model. *Journal of the American Statistical
  Association*, 84(408), 1074–1078.
- Royston, P., & Parmar, M. K. B. (2002). Flexible parametric
  proportional-hazards and proportional-odds models for censored
  survival data. *Statistics in Medicine*, 21(15), 2175–2197.
- Royston, P., & Parmar, M. K. B. (2013). Restricted mean survival time:
  an alternative to the hazard ratio for the design and analysis of
  randomized trials with a time-to-event outcome. *BMC Medical Research
  Methodology*, 13, 152.
- Uno, H., Claggett, B., Tian, L., et al. (2014). Moving beyond the
  hazard ratio in quantifying the between-group difference in survival
  analysis. *Journal of Clinical Oncology*, 32(22), 2380–2385.
- Therneau, T. M., & Grambsch, P. M. (2000). *Modeling Survival Data:
  Extending the Cox Model*. Springer.
- Wei, L. J. (1992). The accelerated failure time model: a useful
  alternative to the Cox regression model in survival analysis.
  *Statistics in Medicine*, 11(14–15), 1871–1879.
