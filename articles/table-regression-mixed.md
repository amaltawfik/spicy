# Mixed-effects regression tables

``` r

library(spicy)
library(lmerTest)   # masks lme4::lmer() with the Satterthwaite-aware version
```

This vignette covers **mixed-effects (multilevel) regression** — models
for data with a grouping structure, such as repeated measurements nested
in subjects or respondents nested in regions. The companion vignette
[*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
covers the shared mechanics (`vcov`, `ci_level`, output formats,
multi-model layouts, broom integration); here we focus on what is
*specific* to mixed fits — above all, how the **random effects** are
reported.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
supports four mixed engines:

- **[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html)** — linear
  mixed models. Load **`lmerTest`** before fitting to get Satterthwaite
  degrees of freedom; plain `lme4` fits fall back to a large-sample
  Wald-z (the footer says which you got).
- **[`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html)** —
  generalized linear mixed models (Wald-z).
- **[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html)**
  — a wider family space, plus zero-inflation and dispersion submodels.
- **[`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html)** — the
  classical engine (containment-df t-tests).

The running example is
[`lme4::sleepstudy`](https://rdrr.io/pkg/lme4/man/sleepstudy.html):
reaction times of 18 subjects measured on 10 consecutive days of a
sleep-restriction study. Each subject contributes 10 rows, so
observations are not independent — the canonical case for a random
intercept and slope.

## The mixed model in one paragraph

A mixed model splits the coefficients in two. **Fixed effects** are the
population-level slopes you would report from any regression. **Random
effects** let selected coefficients vary across groups:
`(Days | Subject)` gives every subject their own intercept and their own
`Days` slope, drawn from a common distribution. What the model
*estimates* for the random part is not 18 pairs of coefficients but the
**variance components** of that distribution — how much subjects differ
at baseline, how much their slopes differ, and how the two go together.
A regression table therefore needs two kinds of rows, and that is
exactly what
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
prints.

## Basic table

``` r

fit <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
table_regression(fit)
#> Linear mixed-effects regression: Reaction
#> 
#>  Variable                        │    B      SE        95% CI         p   
#> ─────────────────────────────────┼────────────────────────────────────────
#>  (Intercept)                     │  251.41  6.82  [237.01, 265.80]  <.001 
#>  Days                            │   10.47  1.55  [  7.21,  13.73]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                 │                                        
#>    σ Subject (Intercept)         │   24.74  5.84  [  6.79,  34.32]   –    
#>    σ Subject Days                │    5.92  1.25  [  2.47,   8.00]   –    
#>    ρ Subject ((Intercept), Days) │    0.07  0.33  [ -0.57,   0.70]   –    
#>    σ (Residual)                  │   25.59  1.51  [ 22.44,  28.39]   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                               │  180                                   
#>  N (Subject)                     │   18                                   
#>  R² (marginal)                   │    0.28                                
#>  R² (conditional)                │    0.80                                
#>  AIC                             │ 1755.6                                 
#>  BIC                             │ 1774.8                                 
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Satterthwaite t-test (lmerTest).
#> Random effects (REML): LR test vs linear regression, χ̄²(3) = 148.35, p < .001.
```

Reading the table, top to bottom:

- **Fixed effects** first, as in any regression table. With `lmerTest`
  loaded, inference is a **t-test with Satterthwaite degrees of
  freedom** — the `lmerTest` default (Kuznetsova et al. 2017), offered
  by SAS PROC MIXED as `DDFM=SATTERTHWAITE` — and the footer says so.
- The **`Random effects:`** block reports the variance components as
  rows, each with its own SE and CI: the between-subject SD of the
  intercept and of the `Days` slope (`σ`), their correlation (`ρ`), and
  the residual SD. The SD scale is the default because it is in the
  units of the outcome (Gelman & Hill 2007) — sleep deprivation adds
  about 10.5 ms per day on average (fixed `Days`), while individual
  slopes spread around that mean with an SD of about 5.9 ms.
- The p column of the random-effect rows shows a **dash, by design** —
  see the next section.
- Below the rule: `n` (observations), **`N (Subject)`** (groups), and
  the **Nakagawa marginal / conditional R²** — the variance explained by
  the fixed effects alone versus fixed and random together (Nakagawa &
  Schielzeth 2013). The gap between the two (0.28 vs 0.80 here) is
  itself a summary of how much the grouping structure matters.
- The footer closes with the **likelihood-ratio test of the whole random
  part** against the same model without random effects, referred to a
  **chi-bar-squared** mixture distribution.

## Why the variance components carry no p-value

A variance cannot be negative, so the null hypothesis “this variance
component is zero” sits **on the boundary** of the parameter space.
There, the usual Wald z-test is invalid — its reference distribution is
wrong, and the resulting p-value is conservative in an unknown degree
(Self & Liang 1987). Printing a Wald p next to each `σ` would look
rigorous and be meaningless. None of the dedicated mixed-model engines
prints one by default — `lme4`, `nlme`, Stata `mixed`, and MLwiN all
decline; SAS PROC MIXED offers a Wald Z only behind its `COVTEST`
option, with documented cautions.

What *is* valid is a **likelihood-ratio test with a boundary-corrected
reference**: a 50:50 mixture of chi-squared distributions
(chi-bar-squared; Self & Liang 1987; Stram & Lee 1994). The footer
reports such a test for the random part as a whole — across its several
parameters jointly it follows Stata’s conservative halved-p convention,
while the per-term tests below use the exact mixture. Here
chi-bar-squared(3) = 148.35, p \< .001: the random structure earns its
place.

## When the fit is singular

The boundary is not only a testing problem — estimates land *on* it.
When a variance component is estimated at exactly zero (`lme4` calls
this a **singular fit**), its Wald SE and CI are meaningless, so
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
omits them and says why in the footer:

``` r

set.seed(2026)
d <- data.frame(x = rnorm(120), g = factor(rep(1:12, each = 10)))
d$y <- 2 + 0.5 * d$x + rnorm(120)   # no group effect at all
sfit <- lmer(y ~ x + (1 | g), data = d)
table_regression(sfit)
#> Linear mixed-effects regression: y
#> 
#>  Variable          │   B      SE      95% CI       p   
#> ───────────────────┼───────────────────────────────────
#>  (Intercept)       │   2.15  0.09  [1.98, 2.32]  <.001 
#>  x                 │   0.51  0.09  [0.34, 0.68]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:   │                                   
#>    σ g (Intercept) │   0.00   –         –         –    
#>    σ (Residual)    │   0.96   –         –         –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                 │ 120                               
#>  N (g)             │  12                               
#>  ICC               │   0.00                            
#>  R² (marginal)     │   0.23                            
#>  R² (conditional)  │   0.23                            
#>  AIC               │ 342.0                             
#>  BIC               │ 353.1                             
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Satterthwaite t-test (lmerTest).
#> Random effects (REML): LR test vs linear regression, χ̄²(1) = -0.00, p = 0.500.
#> Singular fit: random-effect variance component(s) at the boundary 0. Wald SE and CI on the variance components are unreliable at the boundary and have been omitted; consider simplifying the random structure or refitting on the affected grouping factor.
```

The chi-bar-squared test reads p = 0.500 — the boundary value when the
data show no group variance at all. The footer’s advice is the
substantive one: simplify the random structure rather than report a zero
variance with invented uncertainty.

## Testing individual components: `re_test`

When a reviewer asks about one *specific* component — “do you really
need the random slope?” — request an opt-in per-term test.
`re_test = "lrt"` refits the model without each testable component and
fills the p column of its row with the boundary-corrected
likelihood-ratio test (the same statistic as
[`lmerTest::ranova()`](https://rdrr.io/pkg/lmerTest/man/ranova.html)):

``` r

table_regression(fit, re_test = "lrt")
#> Linear mixed-effects regression: Reaction
#> 
#>  Variable                        │    B      SE        95% CI         p   
#> ─────────────────────────────────┼────────────────────────────────────────
#>  (Intercept)                     │  251.41  6.82  [237.01, 265.80]  <.001 
#>  Days                            │   10.47  1.55  [  7.21,  13.73]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                 │                                        
#>    σ Subject (Intercept)         │   24.74  5.84  [  6.79,  34.32]   –    
#>    σ Subject Days                │    5.92  1.25  [  2.47,   8.00]  <.001 
#>    ρ Subject ((Intercept), Days) │    0.07  0.33  [ -0.57,   0.70]   –    
#>    σ (Residual)                  │   25.59  1.51  [ 22.44,  28.39]   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                               │  180                                   
#>  N (Subject)                     │   18                                   
#>  R² (marginal)                   │    0.28                                
#>  R² (conditional)                │    0.80                                
#>  AIC                             │ 1755.6                                 
#>  BIC                             │ 1774.8                                 
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Satterthwaite t-test (lmerTest).
#> Random effects (REML): LR test vs linear regression, χ̄²(3) = 148.35, p < .001.
#> Random-effect p-values: LR test vs the reduced random structure, chi-bar-squared reference.
```

Only the `Days` slope row gains a p-value: following the marginality
rule of
[`lmerTest::ranova()`](https://rdrr.io/pkg/lmerTest/man/ranova.html), an
intercept is tested only when it is the bar’s sole term, and a
correlation is not removable on its own. The footer names the test.

For a model with a **single** variance component — here the
random-intercept-only baseline, which returns in the comparison below —
`re_test = "rlrt"` uses the exact restricted likelihood-ratio test of
Crainiceanu & Ruppert (2004) instead, with a simulated null distribution
(via `RLRsim`) — preferable in small samples where the chi-bar-squared
approximation is at its weakest:

``` r

fit_ri <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
```

``` r

table_regression(fit_ri, re_test = "rlrt")
#> Linear mixed-effects regression: Reaction
#> 
#>  Variable                │    B      SE        95% CI         p   
#> ─────────────────────────┼────────────────────────────────────────
#>  (Intercept)             │  251.41  9.75  [231.23, 271.58]  <.001 
#>  Days                    │   10.47  0.80  [  8.88,  12.06]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:         │                                        
#>    σ Subject (Intercept) │   37.12  6.81  [ 19.67,  48.68]  <.001 
#>    σ (Residual)          │   30.99  1.73  [ 27.40,  34.21]   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                       │  180                                   
#>  N (Subject)             │   18                                   
#>  ICC                     │    0.59                                
#>  R² (marginal)           │    0.28                                
#>  R² (conditional)        │    0.70                                
#>  AIC                     │ 1794.5                                 
#>  BIC                     │ 1807.2                                 
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Satterthwaite t-test (lmerTest).
#> Random effects (REML): LR test vs linear regression, χ̄²(1) = 106.21, p < .001.
#> Random-effect p-value: exact restricted LRT (simulated null distribution).
```

## The random-effects block on your terms

Three arguments control the block without touching the estimates.
`re_scale = "variance"` switches the rows from SD (`σ`) to variance
(`σ²`); `re_columns` restricts which quantities the variance-component
rows display (the SE and CI on a variance component come from `merDeriv`
and are themselves boundary-fragile — dropping them is a defensible
editorial choice); `show_re = FALSE` removes the block and all
random-effects footer lines, leaving a fixed-effects-only table (the
`N (groups)` and R² fit-stat rows remain):

``` r

table_regression(fit, re_scale = "variance", re_columns = "est")
#> Linear mixed-effects regression: Reaction
#> 
#>  Variable                        │    B      SE        95% CI         p   
#> ─────────────────────────────────┼────────────────────────────────────────
#>  (Intercept)                     │  251.41  6.82  [237.01, 265.80]  <.001 
#>  Days                            │   10.47  1.55  [  7.21,  13.73]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                 │                                        
#>    σ² Subject (Intercept)        │  612.10   –           –           –    
#>    σ² Subject Days               │   35.07   –           –           –    
#>    ρ Subject ((Intercept), Days) │    0.07   –           –           –    
#>    σ² (Residual)                 │  654.94   –           –           –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                               │  180                                   
#>  N (Subject)                     │   18                                   
#>  R² (marginal)                   │    0.28                                
#>  R² (conditional)                │    0.80                                
#>  AIC                             │ 1755.6                                 
#>  BIC                             │ 1774.8                                 
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Satterthwaite t-test (lmerTest).
#> Random effects (REML): LR test vs linear regression, χ̄²(3) = 148.35, p < .001.
```

Whatever the display,
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) always
carries the full set (estimate, SE, CI) for the variance-component rows
— display and data are decoupled.

## Random intercept or random slope? Models side by side

Model comparison is where the row layout pays off: the variance
components align across columns like any coefficient, and each model’s
chi-bar-squared test gets its own footer line.

``` r

table_regression(
  list("Intercept only" = fit_ri, "+ random slope" = fit),
  show_columns = c("b", "se", "p")
)
#> Linear mixed-effects regression comparison: Reaction
#> 
#>                                       Intercept only        + random slope    
#>                                    ────────────────────  ──────────────────── 
#>  Variable                        │    B      SE     p       B      SE     p   
#> ─────────────────────────────────┼────────────────────────────────────────────
#>  (Intercept)                     │  251.41  9.75  <.001   251.41  6.82  <.001 
#>  Days                            │   10.47  0.80  <.001    10.47  1.55  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                 │                                            
#>    σ Subject (Intercept)         │   37.12  6.81   –       24.74  5.84   –    
#>    σ Subject Days                │                          5.92  1.25   –    
#>    ρ Subject ((Intercept), Days) │                          0.07  0.33   –    
#>    σ (Residual)                  │   30.99  1.73   –       25.59  1.51   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                               │  180                   180                 
#>  N (Subject)                     │   18                    18                 
#>  ICC                             │    0.59                                    
#>  R² (marginal)                   │    0.28                  0.28              
#>  R² (conditional)                │    0.70                  0.80              
#>  AIC                             │ 1794.5                1755.6               
#>  BIC                             │ 1807.2                1774.8               
#> 
#> Note. Linear mixed-effects regression models.
#> Std. errors: Wald (model-based).
#> p-values: Satterthwaite t-test (lmerTest).
#> Model 1: Random effects (REML): LR test vs linear regression, χ̄²(1) = 106.21, p < .001.
#> Model 2: Random effects (REML): LR test vs linear regression, χ̄²(3) = 148.35, p < .001.
```

Two details of this table repay attention. The **ICC** row (0.59)
appears only for the intercept-only model: with a random slope, the
correlation between two observations from the same subject depends on
*when* they were taken, so a single ICC no longer exists — printing one
would be wrong, so spicy does not. And the fixed `Days` SE **doubles**
(0.80 → 1.55) once slopes are allowed to vary: the intercept-only model
was overstating the precision of the average slope by ignoring
between-subject slope variation — the statistical argument for the
random slope, visible in the table itself.

## Generalized linear mixed models

[`glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) fits render
identically, with two family-driven changes: inference is **Wald-z** (no
Satterthwaite for GLMMs), and `exponentiate = TRUE` is link-gated
exactly as for `glm` — odds ratios under logit, rate ratios under log,
and a hard error on links whose exponential has no meaning (probit). The
example is [`lme4::cbpp`](https://rdrr.io/pkg/lme4/man/cbpp.html),
herd-level incidence of a cattle disease across four periods:

``` r

gfit <- glmer(
  cbind(incidence, size - incidence) ~ period + (1 | herd),
  data = cbpp, family = binomial()
)
table_regression(gfit, exponentiate = TRUE)
#> Logistic mixed-effects regression: incidence
#> 
#>  Variable             │   OR     SE      95% CI       p   
#> ──────────────────────┼───────────────────────────────────
#>  (Intercept)          │   0.25  0.06  [0.16, 0.39]  <.001 
#>  period:              │                                   
#>    1 (ref.)           │    –     –         –         –    
#>    2                  │   0.37  0.11  [0.20, 0.67]   .001 
#>    3                  │   0.32  0.10  [0.17, 0.61]  <.001 
#>    4                  │   0.21  0.09  [0.09, 0.47]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:      │                                   
#>    σ herd (Intercept) │   0.64  0.22  [0.00, 0.99]   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                    │  56                               
#>  N (herd)             │  15                               
#>  R² (marginal)        │   0.35                            
#>  R² (conditional)     │   0.78                            
#>  AIC                  │ 194.1                             
#>  BIC                  │ 204.2                             
#> 
#> Note. Logistic mixed-effects regression.
#> Std. errors: Wald asymptotic (z).
#> p-values: Wald-z asymptotic (lme4).
#> Random effects (ML): LR test vs logistic regression, χ̄²(1) = 14.01, p < .001.
#> OR = odds ratio.
#> Coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric).
```

The random-effect row is now on the **log-odds scale** (the linear
predictor’s scale), not the outcome scale; σ = 0.64 means herds differ
substantially in baseline incidence. The footer’s chi-bar-squared test
(14.01 on 1 df, p \< .001) confirms the herd effect, and the Nakagawa R²
pair carries over unchanged. No ICC row appears — for a trial-weighted
[`cbind()`](https://rdrr.io/r/base/cbind.html) binomial response the
single-trial latent-scale ICC is not defined, so none is printed; a
Bernoulli 0/1 `glmer` shows one.

## `glmmTMB`: beyond `lme4`

`glmmTMB` fits reach the same table through the same code path, and
bring two extra submodels. A zero-inflation (`ziformula =`) or
dispersion (`dispformula =`) component renders as its own labelled block
of rows, with a footer line stating what the component models and on
which scale — here with the `Salamanders` count data:

``` r

data("Salamanders", package = "glmmTMB")
zfit <- glmmTMB::glmmTMB(
  count ~ mined + (1 | site),
  ziformula = ~ mined,
  family = poisson(), data = Salamanders
)
table_regression(zfit, exponentiate = TRUE)
#> Poisson mixed-effects regression (glmmTMB) (zero-inflated): count
#> 
#>  Variable             │   IRR     SE      95% CI       p   
#> ──────────────────────┼────────────────────────────────────
#>  (Intercept)          │    1.09  0.25  [0.69, 1.72]   .706 
#>  mined:               │                                    
#>    yes (ref.)         │     –     –         –         –    
#>    no                 │    3.13  0.77  [1.93, 5.08]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Zero-inflation:      │                                    
#>    (Intercept)        │    3.12  0.73  [1.97, 4.95]  <.001 
#>    mined: yes (ref.)  │     –     –         –         –    
#>    mined: no          │    0.18  0.05  [0.11, 0.29]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:      │                                    
#>    σ site (Intercept) │    0.28  0.10  [0.14, 0.55]   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                    │  644                               
#>  N (site)             │   23                               
#>  R² (marginal)        │    0.29                            
#>  R² (conditional)     │    0.36                            
#>  AIC                  │ 1908.5                             
#>  BIC                  │ 1930.8                             
#> 
#> Note. Poisson mixed-effects regression (glmmTMB) (zero-inflated).
#> Std. errors: Wald asymptotic (z).
#> p-values: Wald-z asymptotic (glmmTMB).
#> Random effects (ML): LR test vs poisson regression, χ̄²(1) = 400.03, p < .001.
#> Zero-inflation component: log-odds of a structural (excess) zero. Coefficients exponentiated and displayed as odds ratios.
#> IRR = incidence rate ratio.
#> Coefficients exponentiated and displayed as IRR; SE on the IRR scale (delta method); CI bounds exponentiated (asymmetric).
```

Note the per-block exponentiation: the count coefficients become
**IRR**, while the logit zero-inflation coefficients become **odds
ratios of a structural zero**. Component blocks are covered in depth in
the counts documentation
([`?table_regression_counts`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md));
opt out with `show_components = FALSE`.

## `nlme::lme`

The classical engine renders through the same layout; its fixed-effect
inference is the **containment-df t-test** native to `nlme`, and the
footer says so:

``` r

lfit <- nlme::lme(Reaction ~ Days, random = ~ 1 | Subject,
                  data = sleepstudy)
table_regression(lfit)
#> Linear mixed-effects regression (nlme): Reaction
#> 
#>  Variable                │    B      SE        95% CI         p   
#> ─────────────────────────┼────────────────────────────────────────
#>  (Intercept)             │  251.41  9.75  [232.16, 270.65]  <.001 
#>  Days                    │   10.47  0.80  [  8.88,  12.06]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:         │                                        
#>    σ Subject (Intercept) │   37.12  6.96  [ 25.91,  53.19]   –    
#>    σ (Residual)          │   30.99  1.73  [ 27.78,  34.57]   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                       │  180                                   
#>  N (Subject)             │   18                                   
#>  ICC                     │    0.59                                
#>  R² (marginal)           │    0.28                                
#>  R² (conditional)        │    0.70                                
#>  AIC                     │ 1794.5                                 
#>  BIC                     │ 1807.2                                 
#> 
#> Note. Linear mixed-effects regression (nlme).
#> Std. errors: Wald (model-based).
#> p-values: t-test with containment df (nlme).
#> Random effects (REML): LR test vs linear regression, χ̄²(1) = 106.21, p < .001.
```

## Cluster-robust and other variance estimators

Mixed fits honour the cluster-robust family (`"CR0"`–`"CR3"`) through
`clubSandwich`, with Satterthwaite small-sample degrees of freedom
computed from the robust covariance — the footer attributes them
accordingly. Reach for `CR*` when you suspect the random-effects
structure does not fully capture the within-group dependence:

``` r

table_regression(fit_ri, vcov = "CR2", cluster = ~Subject)
#> Linear mixed-effects regression: Reaction
#> 
#>  Variable                │    B      SE        95% CI         p   
#> ─────────────────────────┼────────────────────────────────────────
#>  (Intercept)             │  251.41  6.82  [237.01, 265.80]  <.001 
#>  Days                    │   10.47  1.55  [  7.21,  13.73]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:         │                                        
#>    σ Subject (Intercept) │   37.12  6.81  [ 19.67,  48.68]   –    
#>    σ (Residual)          │   30.99  1.73  [ 27.40,  34.21]   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                       │  180                                   
#>  N (Subject)             │   18                                   
#>  ICC                     │    0.59                                
#>  R² (marginal)           │    0.28                                
#>  R² (conditional)        │    0.70                                
#>  AIC                     │ 1794.5                                 
#>  BIC                     │ 1807.2                                 
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: cluster-robust (CR2), clusters by Subject.
#> p-values: Satterthwaite t-test, cluster-robust df (clubSandwich).
#> Random effects (REML): LR test vs linear regression, χ̄²(1) = 106.21, p < .001.
```

Requests that have no valid backend are refused with a clear error
rather than silently ignored: `HC*` (an OLS concept, undefined for mixed
models), `CR*` on `glmer` (no `clubSandwich` method), and the resampling
estimators. For `glmmTMB`, `CR*` covers the conditional component only,
and the footer discloses it.

## Standardized coefficients

Mixed fits support `standardized = "refit"` — the model is refit on
z-scored data, the gold standard under correlated predictors (Cohen et
al. 2003). The β rows inherit the same Satterthwaite reference
distribution as their unstandardized counterparts, so B and β carry one
consistent p per term:

``` r

table_regression(fit, standardized = "refit", show_columns = c("b", "beta", "p"))
#> Linear mixed-effects regression: Reaction
#> 
#>  Variable                        │    B      β      p   
#> ─────────────────────────────────┼──────────────────────
#>  (Intercept)                     │  251.41  0.00  <.001 
#>  Days                            │   10.47  0.54  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                 │                      
#>    σ Subject (Intercept)         │   24.74         –    
#>    σ Subject Days                │    5.92         –    
#>    ρ Subject ((Intercept), Days) │    0.07         –    
#>    σ (Residual)                  │   25.59         –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                               │  180                 
#>  N (Subject)                     │   18                 
#>  R² (marginal)                   │    0.28              
#>  R² (conditional)                │    0.80              
#>  AIC                             │ 1755.6               
#>  BIC                             │ 1774.8               
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Satterthwaite t-test (lmerTest).
#> Random effects (REML): LR test vs linear regression, χ̄²(3) = 148.35, p < .001.
#> β = standardised coefficient.
```

The algebraic shortcuts (`"posthoc"`, `"basic"`, `"smart"`) are not
defined for mixed models — the marginal SD(y) has no unique
decomposition across levels — and fall back to `"refit"` with a warning.
Formulas with inline transforms (`log(x)`,
[`poly()`](https://rdrr.io/r/stats/poly.html)) decline the refit and
omit the β rows, with a warning; pre-compute the transformed column in
`data` for an exact refit.

## Output formats

Everything above used the default console output. The same table renders
as a raw data frame, a long broom-style tibble, or — with the
corresponding Suggests package — a rich `gt`, `flextable`, `tinytable`,
Excel, or Word table; the random-effects block carries through to every
format.

``` r

head(table_regression(fit, output = "data.frame"), 8)
#>                          Variable       B   SE           95% CI     p
#> 1                     (Intercept)  251.41 6.82 [237.01, 265.80] <.001
#> 2                            Days   10.47 1.55 [  7.21,  13.73] <.001
#> 3                 Random effects:                                    
#> 4           σ Subject (Intercept)   24.74 5.84 [  6.79,  34.32]  –   
#> 5                  σ Subject Days    5.92 1.25 [  2.47,   8.00]  –   
#> 6   ρ Subject ((Intercept), Days)    0.07 0.33 [ -0.57,   0.70]  –   
#> 7                    σ (Residual)   25.59 1.51 [ 22.44,  28.39]  –   
#> 8                               n  180
```

``` r

table_regression(fit, output = "gt")
```

[TABLE]

*Note.* Linear mixed-effects regression. Std. errors: Wald
(model-based). p-values: Satterthwaite t-test (lmerTest). Random effects
(REML): LR test vs linear regression, χ̄²(3) = 148.35, p \< .001.

In [`broom::tidy()`](https://broom.tidymodels.org) the
variance-component rows are marked `estimate_type = "vc"` and their
terms are prefixed `re::` (correlations carry a `::cor` suffix), so they
filter cleanly:

``` r

td <- broom::tidy(table_regression(fit))
td[td$estimate_type == "vc", c("term", "estimate", "std.error", "conf.low", "conf.high")]
#> # A tibble: 4 × 5
#>   term                                estimate std.error conf.low conf.high
#>   <chr>                                  <dbl>     <dbl>    <dbl>     <dbl>
#> 1 re::Subject::(Intercept)             24.7        5.84     6.79     34.3  
#> 2 re::Subject::Days                     5.92       1.25     2.47      8.00 
#> 3 re::Subject::(Intercept), Days::cor   0.0656     0.325   -0.571     0.703
#> 4 re::Residual::                       25.6        1.51    22.4      28.4
```

## References

- Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting
  linear mixed-effects models using lme4. *Journal of Statistical
  Software*, 67(1).
- Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). *Applied
  Multiple Regression/Correlation Analysis for the Behavioral Sciences*
  (3rd ed.). Lawrence Erlbaum.
- Crainiceanu, C. M., & Ruppert, D. (2004). Likelihood ratio tests in
  linear mixed models with one variance component. *Journal of the Royal
  Statistical Society, Series B*, 66(1), 165–185.
- Gelman, A., & Hill, J. (2007). *Data Analysis Using Regression and
  Multilevel/Hierarchical Models*. Cambridge University Press.
- Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017).
  lmerTest package: Tests in linear mixed effects models. *Journal of
  Statistical Software*, 82(13).
- Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for
  obtaining R² from generalized linear mixed-effects models. *Methods in
  Ecology and Evolution*, 4(2), 133–142.
- Self, S. G., & Liang, K.-Y. (1987). Asymptotic properties of maximum
  likelihood estimators and likelihood ratio tests under nonstandard
  conditions. *JASA*, 82(398), 605–610.
- Stram, D. O., & Lee, J. W. (1994). Variance components testing in the
  longitudinal mixed effects model. *Biometrics*, 50(4), 1171–1177.
