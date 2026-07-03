# Survival regression tables

``` r

library(spicy)
library(survival)
```

This vignette covers **survival (time-to-event) regression** — models
for how long until an event occurs, with censoring for the subjects who
never reach it. The companion vignette [*Publication-ready regression
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
model and, later, the cluster variable:

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
among those still at risk. Each coefficient multiplies that hazard by a
constant factor at every point in time (the **proportional-hazards
assumption**), and the baseline hazard itself is left completely
unspecified: that is the semiparametric trick that made the model
universal (Cox 1972; Therneau & Grambsch 2000). Two consequences shape
the table. There is **no intercept row** — the baseline hazard absorbs
it — and there is no residual variance, so the model-fit block reports
what a survival reader expects instead: the **number of events** and the
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
#>  AIC           │ 1454.2                             
#> 
#> Note. Cox proportional hazards regression.
#> Std. errors: Wald asymptotic (z).
#> Events: 163 of 226; Concordance C = 0.64 (SE = 0.03).
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

The table reports the model, not its assumption: test proportionality
with
[`survival::cox.zph()`](https://rdrr.io/pkg/survival/man/cox.zph.html)
before reporting (here the global test gives p = .17 — no evidence
against it; Therneau & Grambsch 2000).

## Why there is no AME column

Requesting an `"ame"` column on a Cox fit is refused with an explanation
rather than answered: an average marginal effect needs a response scale,
and a Cox model deliberately does not commit to one — a per-coefficient
“effect on survival” would be ambiguous (at what time? on which scale?)
with unreliable standard errors. The hazard ratio *is* the
per-coefficient summary of this model. Absolute-scale summaries (risk
differences at a horizon, restricted mean survival time) are a different
estimand family, not a per-coefficient column.

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
#>  AIC           │ 1454.2                             
#> 
#> Note. Cox proportional hazards regression.
#> Std. errors: cluster-robust (Lin-Wei), clusters by inst.
#> Events: 163 of 226; Concordance C = 0.64 (SE = 0.03).
#> HR = hazard ratio.
#> Coefficients exponentiated and displayed as HR; SE on the HR scale (delta method); CI bounds exponentiated (asymmetric).
```

Two lessons here. First, the mechanics: the cluster variable must
provide **one value per row of the model data** — declaring the analytic
sample up front (as we did) keeps the cluster aligned when predictors
carry missing values. Second, the direction: robust standard errors are
not always larger — the sex SE *shrinks* here. Clustering corrects the
variance in whichever direction the within-centre correlation points;
treating it as a conservative inflation ritual misreads it.

## Hierarchical Cox models

Does the performance score improve on age and sex? `nested = TRUE`
compares adjacent models by the **partial-likelihood ratio test** (the
`anova.coxph()` convention) and appends the change rows:

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
#>  AIC           │ 1469.0          1454.2          
#>  Δχ²           │     –            +16.78         
#>  p (change)    │     –              <.001        
#> 
#> Note. Cox proportional hazards regression models.
#> Std. errors: Wald asymptotic (z).
#> Model 1: Events: 163 of 226; Concordance C = 0.60 (SE = 0.03).
#> Model 2: Events: 163 of 226; Concordance C = 0.64 (SE = 0.03).
#> HR = hazard ratio.
#> Coefficients exponentiated and displayed as HR; CI bounds exponentiated.
```

Adding `ph.ecog` buys a chi-squared change of 16.78 on one degree of
freedom (p \< .001), an AIC drop of 14.8, and lifts the concordance from
0.60 to 0.64 — three readings of the same improvement, on the
likelihood, information, and discrimination scales respectively.

## Accelerated failure time: `survreg()`

The parametric alternative models **time itself** (Wei 1992): covariates
stretch or compress survival time by a constant factor. Under
`exponentiate = TRUE` the coefficients become **time ratios** (TR):

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
direction is more correct; the Weibull family is the one distribution
that is simultaneously AFT and proportional-hazards, and the two scales
are linked by an exact identity within the fit: HR = TR^(−1/scale) =
1.50^(−1/0.73) ≈ 0.57, with the scale parameter read from the footer —
matching the semiparametric Cox estimate to displayed precision (0.575
vs 0.573). The footer names the distribution because the interpretation
depends on it — `exponentiate` produces time ratios only for log-scale
distributions (Weibull, exponential, lognormal, loglogistic); for an
identity-scale distribution (`dist = "gaussian"`) the request is skipped
with a warning, since the coefficients already act on the time scale.

## `rms::cph()`

The `rms` Cox engine renders through the same layout. One requirement
carried over from `rms` itself: cluster-robust variance uses
[`rms::robcov()`](https://rdrr.io/pkg/rms/man/robcov.html), which needs
the design matrices stored at fit time — fit with `x = TRUE, y = TRUE`
or the robust request errors with that exact instruction:

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
#>  AIC           │ 1469.0                             
#> 
#> Note. Cox proportional hazards regression (rms).
#> Std. errors: cluster-robust (Lin-Wei), clusters by inst.
#> Events: 163 of 226.
#> HR = hazard ratio.
#> Coefficients exponentiated and displayed as HR; SE on the HR scale (delta method); CI bounds exponentiated (asymmetric).
```

## Fully parametric: `flexsurv`

[`flexsurv::flexsurvreg()`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md)
(Jackson 2016) fits parametric survival models over a wide family of
distributions (and the Royston–Parmar 2002 spline models). The location
coefficients exponentiate to the generic `exp(B)` — the substantive
reading (time ratio, hazard ratio) depends on the distribution, which
the footer names together with its ancillary parameters:

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
parameters (`anc =`) act on their own scales, so they refuse
exponentiation as a whole rather than print meaningless ratios.

Two Cox extensions flow through the same table without ceremony:
stratified models
([`strata()`](https://rdrr.io/pkg/survival/man/strata.html) terms are
absorbed into the baseline, no coefficient row) and counting-process
data (`Surv(tstart, tstop, event)` for time-varying covariates) render
like any other fit.

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
#> 7           AIC 1454.2
```

``` r

table_regression(cx, exponentiate = TRUE, output = "gt")
```

[TABLE]

*Note.* Cox proportional hazards regression. Std. errors: Wald
asymptotic (z). Events: 163 of 226; Concordance C = 0.64 (SE = 0.03). HR
= hazard ratio. Coefficients exponentiated and displayed as HR; SE on
the HR scale (delta method); CI bounds exponentiated (asymmetric).

[`broom::tidy()`](https://broom.tidymodels.org) returns the long frame;
with `exponentiate = TRUE` the estimates and CI bounds are on the ratio
scale, as displayed:

``` r

broom::tidy(table_regression(cx, exponentiate = TRUE))
#> # A tibble: 3 × 15
#>   model_id outcome     term  estimate_type estimate std.error conf.low conf.high
#>   <chr>    <chr>       <chr> <chr>            <dbl>     <dbl>    <dbl>     <dbl>
#> 1 M1       Surv(time,… age   B                1.01    0.00937    0.993     1.03 
#> 2 M1       Surv(time,… sexF… B                0.573   0.0963     0.412     0.797
#> 3 M1       Surv(time,… ph.e… B                1.60    0.183      1.28      2.00 
#> # ℹ 7 more variables: statistic <dbl>, df <dbl>, p.value <dbl>,
#> #   test_type <chr>, is_intercept <lgl>, factor_term <chr>, factor_level <chr>
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
- Therneau, T. M., & Grambsch, P. M. (2000). *Modeling Survival Data:
  Extending the Cox Model*. Springer.
- Wei, L. J. (1992). The accelerated failure time model: a useful
  alternative to the Cox regression model in survival analysis.
  *Statistics in Medicine*, 11(14–15), 1871–1879.
