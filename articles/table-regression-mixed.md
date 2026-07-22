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
multi-model layouts, broom integration; the class-by-class map is
[*Supported
models*](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.md));
here we focus on what is *specific* to mixed fits — above all, how the
**random effects** are reported.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
supports four mixed engines:

- **[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html)** — linear
  mixed models. Load **`lmerTest`** before fitting to get Satterthwaite
  degrees of freedom; plain `lme4` fits fall back to a large-sample
  Wald-z (the footer says which you got).
- **[`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html)** —
  generalized linear mixed models (Wald-z).
- **[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html)**
  — a wider family space, plus zero-inflation and dispersion components.
- **[`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html)** — the
  classical engine (containment-df t-tests).

The Bayesian mixed engines
([`stan_lmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html)
/
[`stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html),
`brm()` with grouping terms) render the same *Random effects* block from
their posterior draws — see
[`vignette("table-regression-bayesian")`](https://amaltawfik.github.io/spicy/articles/table-regression-bayesian.md).

Two datasets carry the main narrative. The table anatomy is shown on
[`lme4::sleepstudy`](https://rdrr.io/pkg/lme4/man/sleepstudy.html) —
reaction times of 18 subjects measured on 10 consecutive days of a
sleep-restriction study, the canonical case for a random intercept and
slope. The model-*building* sequence — from a naive OLS to cross-level
interactions — then runs on the High School & Beyond data of the
multilevel textbooks (7,185 pupils in 160 schools; the data ship with
`nlme`, but the models are
[`lmer()`](https://rdrr.io/pkg/lmerTest/man/lmer.html) fits).

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
#> Random effects (REML): LR test vs linear regression, χ̄²(3) = 150.04, p < .001.
```

Reading the table, top to bottom:

- **Fixed effects** first, as in any regression table. With `lmerTest`
  loaded, inference is a **t-test with Satterthwaite degrees of
  freedom** — the `lmerTest` default (Kuznetsova et al. 2017), offered
  by SAS PROC MIXED as `DDFM=SATTERTHWAITE`, and the method the
  small-sample comparison of Luke (2017) recommends when groups are few
  — and the footer says so.
- The **`Random effects:`** block reports the variance components as
  rows, each with its own SE and CI: the between-subject SD of the
  intercept and of the `Days` slope (`σ`), their correlation (`ρ`), and
  the residual SD. The SD scale is the default because each SD is on the
  same scale as the coefficient it modifies (Gelman & Hill 2007) — ms
  for the intercept, ms per day for the `Days` slope: sleep deprivation
  adds about 10.5 ms per day on average (fixed `Days`), while individual
  slopes spread around that mean with an SD of about 5.9 ms per day.
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
There, the sampling distribution of a variance estimate is skewed and
truncated at zero, so the Wald z has no valid normal reference and its
p-value is unreliable. The likelihood-ratio test misbehaves too:
referred to a plain chi-squared, it is conservative at the boundary
(Self & Liang 1987) — which is what motivates the corrected reference
below. Printing a Wald p next to each `σ` would look rigorous and be
meaningless. None of the dedicated mixed-model engines prints one by
default — `lme4`, `nlme`, Stata `mixed`, and MLwiN all decline; SAS PROC
MIXED offers a Wald Z only behind its `COVTEST` option, with documented
cautions.

What *is* valid is a **likelihood-ratio test with a boundary-corrected
reference**: a mixture of chi-squared distributions (chi-bar-squared;
Self & Liang 1987; Stram & Lee 1994) — for a single tested component, a
50:50 mixture of adjacent degrees of freedom. The footer reports such a
test for the random part as a whole. Across several parameters jointly
the mixture weights depend on the model’s geometry, so the footer falls
back on a pragmatic halved chi-squared; Stata’s `mixed` approaches the
same case from the opposite side, printing the chi-squared without
halving, noting that the test “is conservative and provided only for
reference”. The per-term tests below each constrain a single component
and refer the statistic to the 50:50 mixture itself. Here
chi-bar-squared(3) = 150.04, p \< .001: the random structure earns its
place.

## From OLS to a multilevel model, step by step

The sections above dissect one fitted table. This one walks the
model-building path most multilevel analyses follow — the sequence
codified by Raudenbush & Bryk (2002) and taught across the multilevel
literature (Hox et al. 2018; Snijders & Bosker 2012; Bressoux 2010) — on
the data those books made canonical: the High School & Beyond sample of
7,185 pupils in 160 U.S. schools, modelling mathematics achievement from
pupil socio-economic status (SES) and school sector. SES is a
standardized composite of parental education, occupation, and income
(sample SD 0.78, so a one-point difference is a large, roughly 1.3-SD
contrast). Each step adds one ingredient, and one table shows what it
changes.

Two derived predictors do a lot of work below: the school mean of SES,
and the pupil’s SES *centered within the school*. Keeping the two apart
is what lets a multilevel model estimate separate within- and
between-school effects (step 3):

``` r

data("MathAchieve", package = "nlme")
data("MathAchSchool", package = "nlme")
hsb <- merge(MathAchieve[, c("School", "SES", "MathAch")],
             MathAchSchool[, c("School", "Sector")], by = "School")
hsb$School  <- factor(hsb$School)
hsb$meanses <- ave(hsb$SES, hsb$School)   # school mean of SES
hsb$cses    <- hsb$SES - hsb$meanses      # SES centered within school
```

### Step 0: the regression you would have fitted anyway

``` r

ols <- lm(MathAch ~ SES, data = hsb)
table_regression(ols, show_columns = c("b", "se", "p"))
#> Linear regression: MathAch
#> 
#>  Variable    │    B      SE     p   
#> ─────────────┼──────────────────────
#>  (Intercept) │   12.75  0.08  <.001 
#>  SES         │    3.18  0.10  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 7185                 
#>  R²          │    0.13              
#>  Adj.R²      │    0.13              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
```

A one-point difference in SES is associated with 3.18 more achievement
points (SE 0.10; the same column returns in the step-2 comparison). The
price is one silent assumption: that 7,185 pupils are 7,185 independent
observations. They are not; pupils share schools, teachers, and
neighbourhoods. The next steps make the grouping structure part of the
model.

### Step 1: the empty model — how much do schools matter?

The multilevel analysis starts with a model containing *no predictors at
all*, only a school random intercept. Its job is a single number:

``` r

# (the accompanying console warning is suppressed in this rendering)
m_empty <- lmer(MathAch ~ 1 + (1 | School), data = hsb)
table_regression(m_empty)
#> Linear mixed-effects regression: MathAch
#> 
#>  Variable               │    B       SE       95% CI        p   
#> ────────────────────────┼───────────────────────────────────────
#>  (Intercept)            │    12.64  0.24  [12.15, 13.12]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:        │                                       
#>    σ School (Intercept) │     2.93   –          –          –    
#>    σ (Residual)         │     6.26   –          –          –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                      │  7185                                 
#>  N (School)             │   160                                 
#>  ICC                    │     0.18                              
#>  R² (marginal)          │     0.00                              
#>  R² (conditional)       │     0.18                              
#>  AIC                    │ 47122.8                               
#>  BIC                    │ 47143.4                               
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Satterthwaite t-test (lmerTest).
#> Random effects (REML): LR test vs linear regression, χ̄²(1) = 986.12, p < .001.
#> Random-effect variance components: SE and CI not computed (n = 7,185 exceeds the spicy.re_se_max_n cap).
```

The **ICC of 0.18** is nothing but the two σ rows combined: 2.93² /
(2.93² + 6.26²) ≈ 0.18. Eighteen percent of the achievement variance
lies *between* schools — the variance partition that justifies
everything that follows (and matches the number Raudenbush & Bryk report
for these data). With 160 schools, the design sits comfortably above the
roughly 50 groups Maas & Hox (2005) find necessary for reliable standard
errors and interval coverage of the variance components — the point
estimates themselves hold up with fewer.

One display note, visible here for the first time: at this sample size
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
skips the Wald SE and CI of the variance components because their
computation grows superlinearly with n. The table note states the
omission, and the build-time warning names the override
(`options(spicy.re_se_max_n = )`). Estimates, ICC, R², and the LR test
are unaffected; the random-part inference below runs through `re_test`,
and `re_ci = "profile"` restores boundary-respecting profile-likelihood
CIs at any sample size (a couple of seconds per variance parameter — the
route `lme4` itself recommends over SEs). The same note (and suppressed
warning) accompanies every HSB table in this sequence.

### Step 2: a random intercept, next to the OLS

``` r

m_ri <- lmer(MathAch ~ SES + (1 | School), data = hsb)
table_regression(list(OLS = ols, Multilevel = m_ri),
                 show_columns = c("b", "se", "p"))
#> Regression comparison: MathAch
#> 
#>                                    OLS                Multilevel       
#>                           ─────────────────────  ───────────────────── 
#>  Variable               │    B       SE     p       B       SE     p   
#> ────────────────────────┼──────────────────────────────────────────────
#>  (Intercept)            │    12.75  0.08  <.001     12.66  0.19  <.001 
#>  SES                    │     3.18  0.10  <.001      2.39  0.11  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:        │                                              
#>    σ School (Intercept) │                            2.18   –     –    
#>    σ (Residual)         │                            6.09   –     –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                      │  7185                   7185                 
#>  R²                     │     0.13                                     
#>  Adj.R²                 │     0.13                                     
#>  N (School)             │                          160                 
#>  ICC                    │                            0.11              
#>  R² (marginal)          │                            0.08              
#>  R² (conditional)       │                            0.18              
#>  AIC                    │ 47103.9                46653.2               
#>  BIC                    │ 47124.6                46680.7               
#> 
#> Note. Model 1: linear regression; Model 2: linear mixed-effects regression.
#> Std. errors:
#>   Model 1: classical (OLS)
#>   Model 2: Wald (model-based)
#> p-values: Satterthwaite t-test (lmerTest).
#> Random effects (REML): LR test vs linear regression, χ̄²(1) = 458.92, p < .001.
#> Model 2: Random-effect variance components: SE and CI not computed (n = 7,185 exceeds the spicy.re_se_max_n cap).
```

Reading the two columns against each other, five things change:

- The **intercept SE more than doubles** (0.08 → 0.19). OLS treats 7,185
  pupils as independent evidence about school-shared quantities; the
  multilevel model counts the 160 schools it actually has. The OLS
  precision was borrowed, not earned.
- The **SES coefficient itself moves** (3.18 → 2.39): a raw SES slope
  mixes two sources of information — pupils compared within schools, and
  schools compared with schools — and the multilevel fit reweights them
  toward the within-school comparison. Step 3 separates the two
  outright.
- The **ICC row reads 0.11**, not step 1’s 0.18: it partitions the
  variance that SES leaves unexplained — not a contradiction, a
  conditional version of the same quantity.
- The footer’s chi-bar-squared test (458.9 on 1 df) says the school
  effect is not optional.
- **One pair of rows not to read across:** the OLS AIC rests on the full
  ML likelihood, the mixed model’s on the REML likelihood — different
  objectives, not a common scale. The valid whole-model comparison is
  the chi-bar-squared test above; ML-refit information criteria are
  `nested = TRUE`’s job.
- And the **marginal R² (0.08) sits below the OLS R² (0.13)** not
  because the model fits worse, but because the fixed part now carries a
  smaller, within-school-weighted slope while the between-school
  variance moves to the random part, where the Nakagawa denominator
  still counts it.

### Step 3: one variable, two effects — the contextual split

A pupil’s SES and a school’s SES composition are different variables
with different effects, and entering raw `SES` estimates an
uninterpretable blend of the two. The standard decomposition enters the
within-school part and the school mean separately (Enders & Tofighi
2007; Snijders & Bosker 2012, §4.6):

``` r

m_wb <- lmer(MathAch ~ cses + meanses + (1 | School), data = hsb)
table_regression(m_wb, show_columns = c("b", "se", "p"))
#> Linear mixed-effects regression: MathAch
#> 
#>  Variable               │    B       SE     p   
#> ────────────────────────┼───────────────────────
#>  (Intercept)            │    12.68  0.15  <.001 
#>  cses                   │     2.19  0.11  <.001 
#>  meanses                │     5.87  0.36  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:        │                       
#>    σ School (Intercept) │     1.64   –     –    
#>    σ (Residual)         │     6.08   –     –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                      │  7185                 
#>  N (School)             │   160                 
#>  ICC                    │     0.07              
#>  R² (marginal)          │     0.17              
#>  R² (conditional)       │     0.22              
#>  AIC                    │ 46578.6               
#>  BIC                    │ 46613.0               
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Satterthwaite t-test (lmerTest).
#> Random effects (REML): LR test vs linear regression, χ̄²(1) = 265.12, p < .001.
#> Random-effect variance components: SE and CI not computed (n = 7,185 exceeds the spicy.re_se_max_n cap).
```

Within a school, a one-point difference in SES is associated with 2.19
more points of achievement (`cses`); *between* schools, a one-point
difference in composition is associated with 5.87 (`meanses`) — more
than twice the individual effect. The **contextual effect** proper —
what better-off schoolmates add for the *same* pupil, over and above
their own SES — is the difference, 5.87 − 2.19 = 3.68 (Raudenbush & Bryk
2002). To give it its own inference, refit with raw `SES` in place of
`cses` — an equivalent reparameterization of the same model — and the
`meanses` coefficient *is* the contextual effect: 3.68 (SE 0.38, p \<
.001). The OLS slope of 3.18 in step 0 was a weighted blend of the
within and between effects, answering neither question.

### Step 4: does the SES slope vary across schools?

So far every school shares one SES slope. Freeing it — `(cses | School)`
— asks whether SES matters *more in some schools than others*, and
`re_test = "lrt"` attaches a p-value to the answer with a
boundary-corrected likelihood-ratio test: the model is refit without the
varying slope, and the REML deviance difference is referred to the
chi-bar-squared mixture (the mechanics — and which components are
testable at all — are dissected in *Testing individual components*
below):

``` r

m_rs <- lmer(MathAch ~ cses + meanses + (cses | School), data = hsb)
table_regression(m_rs, re_test = "lrt", show_columns = c("b", "p"))
#> Linear mixed-effects regression: MathAch
#> 
#>  Variable                       │    B        p   
#> ────────────────────────────────┼─────────────────
#>  (Intercept)                    │    12.68  <.001 
#>  cses                           │     2.19  <.001 
#>  meanses                        │     5.90  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                │                 
#>    σ School (Intercept)         │     1.64   –    
#>    σ School cses                │     0.83   .003 
#>    ρ School ((Intercept), cses) │    -0.19   –    
#>    σ (Residual)                 │     6.06   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                              │  7185           
#>  N (School)                     │   160           
#>  R² (marginal)                  │     0.17        
#>  R² (conditional)               │     0.23        
#>  AIC                            │ 46571.7         
#>  BIC                            │ 46619.8         
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Satterthwaite t-test (lmerTest).
#> Random effects (REML): LR test vs linear regression, χ̄²(3) = 276.05, p < .001.
#> Random-effect p-values: LR test vs the reduced random structure, chi-bar-squared reference.
#> Random-effect variance components: SE and CI not computed (n = 7,185 exceeds the spicy.re_se_max_n cap).
```

The slope SD is 0.83 achievement points around the average slope of
2.19, and its test reads p = .003 — solid evidence that the slope
genuinely varies across schools — so the random slope stays. Taking the
school slopes as roughly normal, 95% of them lie within 2.19 ± 1.96 ×
0.83 — about 0.6 to 3.8, the plausible value range of Raudenbush & Bryk
(2002): SES pays off in virtually every school, but six to seven times
more in the steepest than in the flattest. (The full anatomy of this
decision — what the side-by-side comparison does and does not test, how
the R² pair moves, what happens to the fixed SE — is dissected on
`sleepstudy` in *Random intercept or random slope?* below.)

### Step 5: why do slopes differ? A cross-level interaction

A varying slope is a question, not an answer: what is it *about a
school* that flattens or steepens its SES gradient? Interacting the
pupil-level slope with a school-level predictor — a **cross-level
interaction** — is the multilevel move that answers it, here with school
sector:

``` r

m_cl <- lmer(MathAch ~ cses * Sector + meanses + (cses | School),
             data = hsb)
table_regression(m_cl, show_columns = c("b", "se", "p"))
#> Linear mixed-effects regression: MathAch
#> 
#>  Variable                       │    B       SE     p   
#> ────────────────────────────────┼───────────────────────
#>  (Intercept)                    │    12.12  0.20  <.001 
#>  cses                           │     2.79  0.16  <.001 
#>  Sector:                        │                       
#>    Public (ref.)                │      –     –     –    
#>    Catholic                     │     1.25  0.31  <.001 
#>  meanses                        │     5.25  0.37  <.001 
#>  cses:SectorCatholic            │    -1.35  0.23  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                │                       
#>    σ School (Intercept)         │     1.54   –     –    
#>    σ School cses                │     0.52   –     –    
#>    ρ School ((Intercept), cses) │     0.24   –     –    
#>    σ (Residual)                 │     6.06   –     –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                              │  7185                 
#>  N (School)                     │   160                 
#>  R² (marginal)                  │     0.18              
#>  R² (conditional)               │     0.23              
#>  AIC                            │ 46532.5               
#>  BIC                            │ 46594.5               
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Satterthwaite t-test (lmerTest).
#> Random effects (REML): LR test vs linear regression, χ̄²(3) = 221.24, p < .001.
#> Random-effect variance components: SE and CI not computed (n = 7,185 exceeds the spicy.re_se_max_n cap).
```

In public schools the within-school SES slope is 2.79; in Catholic
schools it is 2.79 − 1.35 = 1.44 — half as steep, while their intercept
sits 1.25 points higher (both conditional on school composition, since
`meanses` stays in the model). This is the classic finding of Raudenbush
& Bryk’s own analysis of these data: Catholic schooling weakens the link
between a pupil’s background and achievement. The interaction row’s p
tests the *difference* between the two slopes, not the Catholic slope
itself; 1.44 as printed carries no standard error — refit with `Sector`
releveled (`relevel(hsb$Sector, "Catholic")`) and it becomes a first-row
coefficient with its own inference (1.44, SE 0.18). Note where the
answer lives: the interaction is a *fixed* effect, carried by its own
row and Satterthwaite p; the slope heterogeneity it accounts for shows
in the σ `cses` row shrinking from 0.83 to 0.52.

**The likelihood rule this sequence obeyed.**
[`lmer()`](https://rdrr.io/pkg/lmerTest/man/lmer.html) estimates by REML
by default, and a REML likelihood is only comparable between models with
the *same fixed part* — which is why the random-structure decisions
above could read REML deviances and AICs directly (steps 3 and 4:
46,578.6 → 46,571.7). Comparing fixed parts requires ML: `nested = TRUE`
performs that refit for you, silently, as `lme4::anova()` does. The
footer’s `(REML)`/`(ML)` tag on the random-effects test line always
states which likelihood the fitted model — and hence that test — used.

**Before reporting any of these tables.** The sequence above runs
estimation, testing, and interpretation; every textbook it follows puts
three checks before the reporting step. Confirm that the fit converged
without warnings and was not singular (see *When the fit is singular*
below); inspect the level-1 residuals for pattern (`plot(fit)`); and
check that the estimated random effects look roughly normal, with no
lone school driving the variance
([`qqnorm()`](https://rdrr.io/r/stats/qqnorm.html) on `ranef(fit)`). The
table reports what the model claims, not whether the model is adequate
(Snijders & Bosker 2012, ch. 10);
[`performance::check_model()`](https://easystats.github.io/performance/reference/check_model.html)
runs the battery in one call.

## Three levels, and crossed designs

Nothing above is limited to two levels. A third level enters through the
grouping formula — `(1 | batch/cask)` reads “casks nested in batches” —
and the table gains one variance row and one group count per level:

``` r

p3 <- lmer(strength ~ 1 + (1 | batch/cask), data = Pastes)
table_regression(p3, show_columns = c("b", "se"))
#> Linear mixed-effects regression: strength
#> 
#>  Variable                   │   B      SE  
#> ────────────────────────────┼──────────────
#>  (Intercept)                │  60.05  0.68 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:            │              
#>    σ cask:batch (Intercept) │   2.90  0.48 
#>    σ batch (Intercept)      │   1.29  0.91 
#>    σ (Residual)             │   0.82  0.11 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                          │  60          
#>  N (cask:batch)             │  30          
#>  N (batch)                  │  10          
#>  R² (marginal)              │   0.00       
#>  R² (conditional)           │   0.94       
#>  AIC                        │ 255.0        
#>  BIC                        │ 263.4        
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Satterthwaite t-test (lmerTest).
#> Random effects (REML): LR test vs linear regression, χ̄²(2) = 63.19, p < .001.
```

Both levels of nesting get their σ row, one `N (cask:batch)` /
`N (batch)` row counts each level, and the footer’s chi-bar-squared test
covers the random part jointly (2 df here); at n = 60, far below the
size cap, the Wald SE of the variance components is back — and the CI
with it, trimmed from this display by `show_columns`. Random slopes can
sit at any level — `(x | school) + (1 | class)` renders each block under
its grouping factor — and **crossed** (non-nested) structures such as
`(1 | plate) + (1 | sample)` flow through the same machinery: one block
per factor, no extra syntax.

## When the fit is singular

The boundary is not only a testing problem — estimates land *on* it.
When a variance component is estimated at exactly zero (`lme4` calls
this a **singular fit**), its Wald SE and CI are meaningless, so
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
omits them and says why in the footer:

``` r

# (the accompanying console warning is suppressed in this rendering)
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
#> Random effects (REML): LR test vs linear regression, χ̄²(1) = 0.00, p = 1.000.
#> Singular fit: random-effect variance component(s) estimated at the boundary (0); their Wald SE and CI are omitted.
```

The chi-bar-squared test reads p = 1.000 — the boundary case. The
likelihood-ratio statistic is exactly zero, and half of the
chi-bar-squared null distribution is a point mass *at* zero, so the
observed value carries no evidence whatever against the no-group-effect
null (Stata prints `Prob >= chibar2 = 1.0000` in the same situation).
The note states the fact for the table’s reader; the actionable advice
arrives as a console warning when the table is built: simplify the
random structure, or test the component with `re_test = "lrt"`, rather
than report a zero variance with invented uncertainty.

## Testing individual components: `re_test`

When a reviewer asks about one *specific* component — “do you really
need the random slope?” — request an opt-in per-term test.
`re_test = "lrt"` refits the model without each testable component and
fills the p column of its row with the boundary-corrected
likelihood-ratio test. The statistic is the one
[`lmerTest::ranova()`](https://rdrr.io/pkg/lmerTest/man/ranova.html)
computes (a REML deviance difference); `ranova` refers it to a plain χ²,
which is conservative at the boundary, so spicy applies the
chi-bar-squared mixture instead:

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
#> Random effects (REML): LR test vs linear regression, χ̄²(3) = 150.04, p < .001.
#> Random-effect p-values: LR test vs the reduced random structure, chi-bar-squared reference.
```

Only the `Days` slope row gains a p-value: following the marginality
rule of
[`lmerTest::ranova()`](https://rdrr.io/pkg/lmerTest/man/ranova.html), an
intercept is tested only when it stands alone on the left of the `|` —
as in `(1 | g)` — and a correlation is not removable on its own. The
footer names the test.

For a model with a **single** variance component — here the
random-intercept-only baseline, which returns in the comparison below —
`re_test = "rlrt"` instead uses the exact restricted likelihood-ratio
test of Crainiceanu & Ruppert (2004), with the null distribution
simulated by
[`RLRsim::exactRLRT()`](https://rdrr.io/pkg/RLRsim/man/exactRLRT.html).
This exact test is preferable in small samples, where the
chi-bar-squared approximation is at its weakest:

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
#> Random effects (REML): LR test vs linear regression, χ̄²(1) = 107.20, p < .001.
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
per-factor `N (<factor>)` and R² fit-stat rows remain):

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
#> Random effects (REML): LR test vs linear regression, χ̄²(3) = 150.04, p < .001.
```

Where those SEs come from depends on the engine: `lmer` / `glmer` rows
use the `merDeriv` Hessian (Wang & Merkle 2018; the cells fall back to
NA when the package is absent or the fit is singular), `glmmTMB` its
native Wald [`confint()`](https://rdrr.io/r/stats/confint.html), and
[`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) its `intervals()`.
Correlation (`ρ`) rows carry a multivariate delta-method SE for `lmer`,
native intervals elsewhere. Switching `re_scale` transports SE and CI
between the SD and variance scales by the delta method
(`SE(σ) = SE(σ²) / 2σ`), so the displayed values agree with
[`VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html) on either scale
up to rounding.

Whatever the display,
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) always
carries the full set (estimate, SE, CI) for the variance-component rows
— display and data are decoupled.

## Random intercept or random slope? Models side by side

The HSB sequence above made this decision on real school data with one
`re_test` call; this section dissects its full anatomy on `sleepstudy`,
an example small enough that every quantity can be checked by hand.
Model comparison is where the row layout pays off: the variance
components align across columns like any coefficient, and each model
gets its own footer line.

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
#> Model 1: Random effects (REML): LR test vs linear regression, χ̄²(1) = 107.20, p < .001.
#> Model 2: Random effects (REML): LR test vs linear regression, χ̄²(3) = 150.04, p < .001.
```

**What is — and is not — being tested here.** Each footer’s
chi-bar-squared line answers one question only: *does this model need
its random part at all?* — the fit against a plain linear regression.
Neither footer compares the two columns to each other. That comparison —
*does the slope earn its place on top of the intercept?* — is the
per-term test of the previous section: `re_test = "lrt"` on the slope
model refits exactly the intercept-only model of column 1 and refers the
REML deviance difference (42.8) to the 50:50 mixture of χ²(1) and χ²(2),
p ≈ 3 × 10⁻¹⁰ — the textbook procedure for retaining a random slope
(Snijders & Bosker 2012, §6.2; Stram & Lee 1994). AIC and BIC agree
(1794.5 → 1755.6; 1807.2 → 1774.8). For hierarchies that grow the
*fixed* part instead, `nested = TRUE` adds chi-squared, AIC, and BIC
change rows — an ML-refit test with a plain χ² reference, appropriate
there but conservative for random-structure changes, which is why the
slope decision belongs to `re_test`.

**Reading the two R² rows — without over-reading them.** The Nakagawa
pair is descriptive, and each member answers a different question.
*Marginal* R² is the share of variance explained by the fixed effects
alone: it reads 0.28 in both columns — 0.280 versus 0.279 before
rounding — because the two models share a fixed part and the balanced
design leaves the numerator identical. The near-equality is a property
of this fit, not a theorem: the random-effect variances enter the
denominator of the marginal R², so it can shift when the random
structure changes. *Conditional* R² adds the random effects: it rises
(0.70 → 0.80) because individual slopes let each subject’s own
trajectory absorb variance the first model left in the residual. The
trap: a conditional R² can hardly go *down* when the random structure
grows, so its increase is **not** evidence that the slope is needed — it
measures how much the grouping structure explains, never whether a
component earns its place. Selection belongs to the boundary-corrected
test and the information criteria; the R² pair then describes the model
you selected. (Where the numbers come from: spicy computes the pair
natively from the closed-form decomposition for Gaussian, single-trial
binomial — logit, probit, cloglog — and Poisson log fits,
cross-validated against
[`performance::r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.html)
to 10⁻¹⁰, and delegates the remaining families — multi-trial binomial,
negative binomial, beta, zero-inflation — to `performance`, a Suggests
dependency: absent, the R² rows render as NA and the rest of the table
is unaffected.)

**The ICC row is an interpretation, not a test.** For the intercept-only
model it comes straight from the two σ rows above it: ICC = σ²(Subject)
/ (σ²(Subject) + σ²(Residual)) = 37.12² / (37.12² + 30.99²) ≈ 0.59 —
squares, because the block displays standard deviations
(`re_scale = "variance"` shows the variances directly). Read it two
equivalent ways: 59% of the variance remaining after the fixed effects
lies *between* subjects rather than within them — the variance partition
coefficient of the multilevel textbooks (Snijders & Bosker 2012, §3.3) —
and, equivalently, two observations from the same subject are expected
to correlate at 0.59. It appears only for the intercept-only model: with
a random slope, the correlation between two observations from the same
subject depends on *when* they were taken, so a single ICC no longer
exists — none is printed.

One further detail repays attention: the fixed `Days` SE **nearly
doubles** (0.80 → 1.55). A model without the random slope overstates the
precision of the average slope by ignoring between-subject slope
variation (Schielzeth & Forstmeier 2009) — the inferential stake of the
decision, visible in the table itself.

Whether to *test* the slope at all is its own debate. The design-driven
position includes every slope the design permits (Barr et al. 2013);
parsimony advocates prune components the data cannot support (Matuschek
et al. 2017); Gelman & Hill (2007) would rather estimate every component
and let partial pooling do the pruning: when a variance is near zero,
the group-level coefficients shrink almost entirely toward the common
one, and the model collapses to the simpler fit on its own. The table
serves all three positions: the components are always displayed with
their uncertainty, and the test stays opt-in.

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
predictor’s scale), not the outcome scale: σ = 0.64 means a herd one SD
above the average carries about e^0.64 ≈ 1.9 times the baseline *odds*
of infection, so between-herd variation in susceptibility is far from
negligible. Incidence falls across the trial — relative to period 1, the
odds of new infection are 0.37 (period 2), 0.32 (period 3), and 0.21
(period 4). The footer’s chi-bar-squared test (14.01 on 1 df, p \< .001)
confirms the herd effect, and the Nakagawa R² pair carries over
unchanged.

No ICC row appears. The latent-scale formula σ² / (σ² + π²/3) describes
a *single* Bernoulli trial, and it would still return a number here
(0.11). But an aggregated [`cbind()`](https://rdrr.io/r/base/cbind.html)
count is not a single trial: with denominators of varying size, the
correlation between two rows no longer matches that single-trial
quantity, so one printed ICC would be ambiguous and none is shown. A
Bernoulli 0/1 `glmer`, where every observation *is* one trial, shows it
— computed on the latent logit scale, whose residual variance is fixed
at π²/3. The same adjusted-ICC rule covers the other single-trial links
— σ²_d = 1 under probit, π²/6 under cloglog — and Poisson log fits,
where σ²_d = log(1 + 1/λ) with λ taken from the null model (Nakagawa,
Johnson & Schielzeth 2017); the printed value matches
[`performance::icc()`](https://easystats.github.io/performance/reference/icc.html)’s
adjusted ICC to 10⁻⁶.

## Average marginal effects

An odds ratio answers “by what factor do the odds change”; the question
a substantive reader more often asks is “by how many percentage points
does the probability change”. The `"ame"` / `"ame_se"` / `"ame_ci"` /
`"ame_p"` tokens of `show_columns` work for mixed-effects fits exactly
as for `glm`, delegated to
[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html),
always on the **response scale**:

- `glmer` binomial: **probability scale** (percentage points), not
  log-odds;
- `glmer` / `glmmTMB` Poisson: **count scale** (units of the outcome),
  not log-rate;
- Gaussian fits (identity link): the AME equals the `B` coefficient —
  the column is filled but redundant.

``` r

set.seed(1)
n <- 500
g <- factor(rep(1:25, length.out = n))
x <- rnorm(n)
cat <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
y <- rbinom(n, 1, plogis(0.5 + 0.8 * x + (cat == "B") * 0.3 +
                           (cat == "C") * -0.5 + rnorm(25)[g]))
afit <- lme4::glmer(y ~ x + cat + (1 | g), family = binomial)
table_regression(afit, show_columns = c("b", "se", "p", "ame", "ame_p"))
#> Logistic mixed-effects regression: y
#> 
#>  Variable          │   B      SE     p     AME     p   
#> ───────────────────┼───────────────────────────────────
#>  (Intercept)       │   0.25  0.34   .470               
#>  x                 │   0.85  0.12  <.001   0.15  <.001 
#>  cat:              │                                   
#>    A (ref.)        │    –     –     –       –     –    
#>    B               │   0.46  0.28   .107   0.08   .106 
#>    C               │  -0.30  0.27   .264  -0.05   .262 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:   │                                   
#>    σ g (Intercept) │   1.40  0.23   –                  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                 │ 500                               
#>  N (g)             │  25                               
#>  ICC               │   0.37                            
#>  R² (marginal)     │   0.14                            
#>  R² (conditional)  │   0.46                            
#>  AIC               │ 562.4                             
#>  BIC               │ 583.5                             
#> 
#> Note. Logistic mixed-effects regression.
#> Std. errors: Wald asymptotic (z).
#> p-values: Wald-z asymptotic (lme4).
#> Random effects (ML): LR test vs logistic regression, χ̄²(1) = 84.11, p < .001.
#> AME = average marginal effect.
```

Factor predictors are handled level-by-level: AME rows align under the
same factor header (`cat: A (ref.) / B / C`), each level sharing a row
with its `B` coefficient — for a factor level the AME is the average
*discrete* change against the reference, not a derivative.

Inference on the AME rows is Wald-z asymptotic (`df = Inf`) with the
model-based vcov. That matches the B-row inference for `glmer` and
`glmmTMB`, which are Wald-z themselves; it does *not* match the t-based
B rows of `lmerTest` fits (Satterthwaite df) or
[`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) (containment df) —
there the AME row keeps its large-sample z while the B row keeps its t,
so under an identity link the same estimate can carry two different
p-values in the same row. Treat the AME p as a large-sample
approximation for those classes.

## `glmmTMB`: beyond `lme4`

`glmmTMB` fits reach the same table through the same code path, and
bring two extra components. A zero-inflation (`ziformula =`) or
dispersion (`dispformula =`) component renders as its own labelled block
of rows, with a footer line stating what the component models and on
which scale — here with the `Salamanders` count data. Sixty percent of
these stream surveys count no salamanders, and the zeros cluster in
mined sites (84% zero, against 38% elsewhere): some are ordinary
sampling zeros, others sites that mining has rendered uninhabitable. The
`ziformula` models the probability of that structural absence separately
from abundance where salamanders can occur:

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
#> Random effects (ML): LR test vs poisson regression, χ̄²(1) = 19.46, p < .001.
#> Zero-inflation component: log-odds of a structural (excess) zero. Coefficients exponentiated and displayed as odds ratios.
#> IRR = incidence rate ratio.
#> Coefficients exponentiated and displayed as IRR; SE on the IRR scale (delta method); CI bounds exponentiated (asymmetric).
```

Note the per-block exponentiation: the count coefficients become
**IRR**, while the logit zero-inflation coefficients become **odds
ratios of a structural zero**. Read one number from each: relative to
mined streams, unmined ones carry about three times the abundance (IRR
3.13) where salamanders occur at all, and about a fifth of the odds of
structural absence (OR 0.18). Component blocks are covered in depth in
[`vignette("table-regression-counts")`](https://amaltawfik.github.io/spicy/articles/table-regression-counts.md);
opt out with `show_components = FALSE`.

## `nlme::lme`

Beyond legacy code, `lme()` remains the engine of choice when the
residuals need structure `lme4` cannot fit — heteroscedasticity across
groups (`weights = varIdent()`) or serial correlation within them
(`correlation = corAR1()`). Such fits flow through the same layout, and
the footer’s boundary-corrected test refits the null carrying the same
residual structure. Its fixed-effect inference is the **containment-df
t-test** native to `nlme`, and the footer says so:

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
#> Random effects (REML): LR test vs linear regression, χ̄²(1) = 107.20, p < .001.
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
#> Random effects (REML): LR test vs linear regression, χ̄²(1) = 107.20, p < .001.
```

Requests that have no valid backend are refused with a clear error
rather than silently ignored: `HC*` (a single-level estimator whose
sandwich assumes independent observations — the very dependence the
random effects exist to model; use `CR*` instead), `CR*` on `glmer` (no
`clubSandwich` method), and the resampling estimators. For `glmmTMB`,
`CR*` covers the conditional component only, and the footer discloses
it.

## Standardized coefficients

Mixed fits support `standardized = "refit"` — the model is refit on
z-scored data (Cohen et al. 2003), for mixed models the only well-posed
approach, since the marginal SD(y) has no unique decomposition across
levels. The β rows inherit the same Satterthwaite reference distribution
as their unstandardized counterparts, so B and β carry one consistent p
per term:

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
#> Random effects (REML): LR test vs linear regression, χ̄²(3) = 150.04, p < .001.
#> β = standardised coefficient.
```

The algebraic shortcuts (`"posthoc"`, `"basic"`, `"smart"`) are
therefore not defined for mixed models and fall back to `"refit"` with a
warning. Formulas with inline transforms (`log(x)`,
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
(REML): LR test vs linear regression, χ̄²(3) = 150.04, p \< .001.

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

## Where next

- [`vignette("table-regression")`](https://amaltawfik.github.io/spicy/articles/table-regression.md)
  — the shared mechanics: output formats, multi-model layouts,
  `p_adjust`, broom methods.
- [`vignette("table-regression-counts")`](https://amaltawfik.github.io/spicy/articles/table-regression-counts.md)
  — zero-inflation and dispersion component blocks in depth.
- [`vignette("table-regression-bayesian")`](https://amaltawfik.github.io/spicy/articles/table-regression-bayesian.md)
  — the same *Random effects* block from posterior draws (`stan_glmer`,
  `brm`).
- [`vignette("table-regression-supported-models")`](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.md)
  — the class-by-class capability map.

## References

- Barr, D. J., Levy, R., Scheepers, C., & Tily, H. J. (2013). Random
  effects structure for confirmatory hypothesis testing: Keep it
  maximal. *Journal of Memory and Language*, 68(3), 255–278.
- Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting
  linear mixed-effects models using lme4. *Journal of Statistical
  Software*, 67(1).
- Bressoux, P. (2010). *Modélisation statistique appliquée aux sciences
  sociales* (2nd ed.). De Boeck.
- Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). *Applied
  Multiple Regression/Correlation Analysis for the Behavioral Sciences*
  (3rd ed.). Lawrence Erlbaum.
- Crainiceanu, C. M., & Ruppert, D. (2004). Likelihood ratio tests in
  linear mixed models with one variance component. *Journal of the Royal
  Statistical Society, Series B*, 66(1), 165–185.
- Enders, C. K., & Tofighi, D. (2007). Centering predictor variables in
  cross-sectional multilevel models: A new look at an old issue.
  *Psychological Methods*, 12(2), 121–138.
- Gelman, A., & Hill, J. (2007). *Data Analysis Using Regression and
  Multilevel/Hierarchical Models*. Cambridge University Press.
- Hox, J. J., Moerbeek, M., & van de Schoot, R. (2018). *Multilevel
  Analysis: Techniques and Applications* (3rd ed.). Routledge.
- Luke, S. G. (2017). Evaluating significance in linear mixed-effects
  models in R. *Behavior Research Methods*, 49(4), 1494-1502.
- Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017).
  lmerTest package: Tests in linear mixed effects models. *Journal of
  Statistical Software*, 82(13).
- Maas, C. J. M., & Hox, J. J. (2005). Sufficient sample sizes for
  multilevel modeling. *Methodology*, 1(3), 86–92.
- Matuschek, H., Kliegl, R., Vasishth, S., Baayen, H., & Bates, D.
  (2017). Balancing Type I error and power in linear mixed models.
  *Journal of Memory and Language*, 94, 305–315.
- Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for
  obtaining R² from generalized linear mixed-effects models. *Methods in
  Ecology and Evolution*, 4(2), 133–142.
- Raudenbush, S. W., & Bryk, A. S. (2002). *Hierarchical Linear Models:
  Applications and Data Analysis Methods* (2nd ed.). Sage.
- Schielzeth, H., & Forstmeier, W. (2009). Conclusions beyond support:
  overconfident estimates in mixed models. *Behavioral Ecology*, 20(2),
  416–420.
- Self, S. G., & Liang, K.-Y. (1987). Asymptotic properties of maximum
  likelihood estimators and likelihood ratio tests under nonstandard
  conditions. *JASA*, 82(398), 605–610.
- Snijders, T. A. B., & Bosker, R. J. (2012). *Multilevel Analysis: An
  Introduction to Basic and Advanced Multilevel Modeling* (2nd ed.).
  Sage.
- Stram, D. O., & Lee, J. W. (1994). Variance components testing in the
  longitudinal mixed effects model. *Biometrics*, 50(4), 1171–1177.
- Wang, T., & Merkle, E. C. (2018). merDeriv: Derivative computations
  for linear mixed effects models with application to robust standard
  errors. *Journal of Statistical Software*, 87(Code Snippet 1), 1-16.
