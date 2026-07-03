# spicy: frequency tables, cross-tabulations, summary and regression tables in R

spicy is an R package for frequency tables, cross-tabulations,
association measures, categorical and continuous summary tables,
publication-ready regression tables for 30+ model classes, and labelled
survey data workflows.

## Features

Every spicy table prints as readable text in the console and exports to
gt, tinytable, flextable, Excel, Word, or the clipboard, following APA
conventions. Around the tables, spicy provides the survey-data tools
that feed them: variable inspection, codebooks, label extraction, and
row-wise summaries.

- **Frequency tables** with
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).
- **Cross-tabulations** with
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md):
  percentages, weights, chi-squared tests, and effect sizes.
- **Association measures**:
  [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
  [`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md),
  [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
  [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
  [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md)
  and 6 other coefficients;
  [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
  computes the full set at once.
- **Categorical and continuous summary tables** with
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  and
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
  overall or by group.
- **Model-based continuous summary tables** with
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  for linear-model reporting: classical / HC\* / cluster-robust /
  bootstrap / jackknife variance, four effect-size families (f², Cohen’s
  d, Hedges’ g, Hays’ omega²) with noncentral CIs, additive covariate
  adjustment with G-computation (Stata `margins` style) or equal-weight
  (`emmeans` style) marginal means, and weighted comparisons.
- **Regression tables** with
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  for one or more fitted models side by side, across 30+ model classes
  (see *Supported models* below): classical / heteroskedasticity-robust
  / cluster-robust / bootstrap / jackknife variance with each class’s
  field-standard backend, standardised coefficients, family-aware
  `exponentiate` (OR / IRR / HR / RR / MR, link-gated), Wald or
  profile-likelihood CIs, average marginal effects (per-category for
  ordinal and multinomial models), partial *f²* / *η²* / *ω²* / *χ²*
  effect sizes, class-aware fit statistics (pseudo-*R²*, Nakagawa
  marginal / conditional *R²*, ICC), hierarchical model comparisons with
  the correct nested test per class, and multiple-comparison adjustment.
  Mixed models report their random effects as table rows with an
  optional boundary-correct per-term test; ordinal models report their
  thresholds; zero-inflated and hurdle models report every model
  component.
- **Variable inspection** with
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  and [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md):
  names, labels, values, classes, and missing data.
- **Codebooks** with
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md):
  interactive and exportable, for labelled and survey-style datasets.
- **Label extraction** with
  [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md),
  including LimeSurvey-style headers.
- **Row-wise summaries** with
  [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md),
  and
  [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md),
  with explicit control over missing values.

Works with `labelled`, `factor`, `ordered`, `Date`, `POSIXct`, and other
common variable types. For a full introduction, see [Getting started
with spicy](https://amaltawfik.github.io/spicy/articles/spicy.html).

## Supported models

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
accepts a single fit or a list of fits from any of these classes, and
renders them with the conventions of each model family:

| Family | Engines |
|----|----|
| Linear and generalized linear | [`stats::lm()`](https://rdrr.io/r/stats/lm.html), [`stats::glm()`](https://rdrr.io/r/stats/glm.html), [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html), [`MASS::rlm()`](https://rdrr.io/pkg/MASS/man/rlm.html), [`stats::nls()`](https://rdrr.io/r/stats/nls.html) |
| Robust, IV, quantile, panel | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html), [`estimatr::iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html), [`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html), [`AER::tobit()`](https://rdrr.io/pkg/AER/man/tobit.html), [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html), [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html), [`fixest::feglm()`](https://lrberge.github.io/fixest/reference/feglm.html), [`fixest::fepois()`](https://lrberge.github.io/fixest/reference/feglm.html), [`fixest::fenegbin()`](https://lrberge.github.io/fixest/reference/femlm.html) |
| Mixed effects | [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html), [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html), [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html), [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html), [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) |
| Ordinal and categorical | [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html), [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) (incl. partial proportional odds), [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html), [`mlogit::mlogit()`](https://rdrr.io/pkg/mlogit/man/mlogit.html) |
| Counts, two-part models | [`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html), [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html), [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) (zero-inflation and dispersion components) |
| Survival | [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html), [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html), [`rms::cph()`](https://rdrr.io/pkg/rms/man/cph.html), [`flexsurv::flexsurvreg()`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md) |
| Survey-weighted | [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) (design-based SEs) |
| Additive, proportions, selection | [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html), [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html), [`betareg::betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html), [`sampleSelection::selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html) |
| rms | [`rms::ols()`](https://rdrr.io/pkg/rms/man/ols.html), [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html), [`rms::Glm()`](https://rdrr.io/pkg/rms/man/Glm.html) |
| Bayesian | [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html), [`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html), [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) (posterior median, credible intervals, no p-values) |

Class-specific structure renders as labelled blocks in the same table:
random effects (with SE and CI on each variance component, and an
optional boundary-correct per-term likelihood-ratio test), ordinal
thresholds, non-proportional effects, zero-inflation and dispersion
components. Robust variance requests use each class’s field-standard
backend and are refused with a clear message when no valid backend
exists.

## Installation

Install the current CRAN release, recommended for most users:

``` r

install.packages("spicy")
```

Install the latest [r-universe](https://amaltawfik.r-universe.dev/spicy)
build:

``` r

install.packages(
  "spicy",
  repos = c(
    "https://amaltawfik.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

This installs `spicy` from r-universe when available; CRAN is included
only as a fallback for dependencies. The r-universe build may be newer
than the current CRAN release.

Install the development version from GitHub with `pak`:

``` r

# install.packages("pak")
pak::pak("amaltawfik/spicy")
```

------------------------------------------------------------------------

## Quick tour

The examples below use the bundled `sochealth` dataset.

### Inspect variables

![varlist demo with labelled
data](reference/figures/animation_varlist.gif)

``` r

varlist(sochealth, tbl = TRUE)
#> # A tibble: 24 × 7
#>    Variable          Label                 Values Class N_distinct N_valid   NAs
#>    <chr>             <chr>                 <chr>  <chr>      <int>   <int> <int>
#>  1 sex               Sex                   Femal… fact…          2    1200     0
#>  2 age               Age (years)           25, 2… nume…         51    1200     0
#>  3 age_group         Age group             25-34… orde…          4    1200     0
#>  4 education         Highest education le… Lower… orde…          3    1200     0
#>  5 social_class      Subjective social cl… Lower… orde…          5    1200     0
#>  6 region            Region of residence   Centr… fact…          6    1200     0
#>  7 employment_status Employment status     Emplo… fact…          4    1200     0
#>  8 income_group      Household income gro… Low, … orde…          4    1182    18
#>  9 income            Monthly household in… 1000,… nume…       1052    1200     0
#> 10 smoking           Current smoker        No, Y… fact…          2    1175    25
#> # ℹ 14 more rows
```

``` r

code_book(
  sochealth,
  starts_with("bmi"),
  values = TRUE,
  include_na = TRUE
)
```

See [Explore variables and build
codebooks](https://amaltawfik.github.io/spicy/articles/variable-exploration.html)
for more on
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md),
[`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md), and
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md).

### Frequency tables and cross-tabulations

``` r

freq(sochealth, income_group)
#> Frequency table: income_group
#> 
#>  Category   │ Values            Freq.    Percent    Valid Percent 
#> ────────────┼─────────────────────────────────────────────────────
#>  Valid      │ Low                 247       20.6             20.9 
#>             │ Lower middle        388       32.3             32.8 
#>             │ Upper middle        328       27.3             27.7 
#>             │ High                219       18.2             18.5 
#>  Missing    │ NA                   18        1.5                  
#> ────────────┼─────────────────────────────────────────────────────
#>  Total      │                    1200      100.0            100.0 
#> 
#> Label: Household income group
#> Class: ordered, factor
#> Data: sochealth

cross_tab(sochealth, smoking, education, percent = "col")
#> Crosstable: smoking x education (Column %)
#> 
#>  Values   │   Lower secondary    Upper secondary    Tertiary │   Total 
#> ──────────┼──────────────────────────────────────────────────┼─────────
#>  No       │              69.6               78.7        84.9 │    78.8 
#>  Yes      │              30.4               21.3        15.1 │    21.2 
#> ──────────┼──────────────────────────────────────────────────┼─────────
#>  Total    │             100.0              100.0       100.0 │   100.0 
#>  N        │               257                527         391 │    1175 
#> 
#> Chi-2(2) = 21.6, p <.001
#> Cramer's V = 0.14
```

See [Frequency tables and
cross-tabulations](https://amaltawfik.github.io/spicy/articles/frequency-tables.html)
for [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
percentages, weights, and tests.

### Association measures

``` r

tbl <- xtabs(~ self_rated_health + education, data = sochealth)

# Quick scalar estimate
cramer_v(tbl)
#> [1] 0.1761697

# Detailed result with CI and p-value
cramer_v(tbl, detail = TRUE)
#> Estimate  CI lower  CI upper      p
#>    0.176     0.120     0.231  <.001
```

See [Cramer’s V, Phi, and association
measures](https://amaltawfik.github.io/spicy/articles/association-measures.html)
for a guide on choosing the right measure.

### Summary tables

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  labels = c(
    smoking           = "Current smoker",
    physical_activity = "Physical activity"
  )
)
#> Categorical table
#> 
#>  Variable            │   n      %    
#> ─────────────────────┼───────────────
#>  Current smoker      │               
#>    No                │  926    78.8  
#>    Yes               │  249    21.2  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity   │               
#>    No                │  650    54.2  
#>    Yes               │  550    45.8
```

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c(
    smoking           = "Current smoker",
    physical_activity = "Physical activity"
  )
)
#> Categorical table by education
#> 
#>  Variable          │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ───────────────────┼─────────────────────────────────────────────────────────
#>  Current smoker    │                                                         
#>    No              │        179               69.6                415        
#>    Yes             │         78               30.4                112        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity │                                                         
#>    No              │        177               67.8                310        
#>    Yes             │         84               32.2                229        
#> 
#>  Variable          │ Upper secondary %  Tertiary n  Tertiary %  Total n 
#> ───────────────────┼────────────────────────────────────────────────────
#>  Current smoker    │                                                    
#>    No              │       78.7            332         84.9       926   
#>    Yes             │       21.3             59         15.1       249   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity │                                                    
#>    No              │       57.5            163         40.8       650   
#>    Yes             │       42.5            237         59.2       550   
#> 
#>  Variable          │ Total %    p    Cramer's V 
#> ───────────────────┼────────────────────────────
#>  Current smoker    │          <.001     .14     
#>    No              │  78.8                      
#>    Yes             │  21.2                      
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity │          <.001     .21     
#>    No              │  54.2                      
#>    Yes             │  45.8
```

``` r

table_continuous(
  sochealth,
  select = c(bmi, life_sat_health)
)
#> Descriptive statistics
#> 
#>  Variable                       │   M     SD    Min    Max   95% CI LL 
#> ────────────────────────────────┼──────────────────────────────────────
#>  Body mass index                │ 25.93  3.72  16.00  38.90    25.72   
#>  Satisfaction with health (1-5) │  3.55  1.25   1.00   5.00     3.48   
#> 
#>  Variable                       │ 95% CI UL   n   
#> ────────────────────────────────┼─────────────────
#>  Body mass index                │   26.14    1188 
#>  Satisfaction with health (1-5) │    3.62    1192
```

``` r

table_continuous(
  sochealth,
  select = c(bmi, life_sat_health),
  by = education
)
#> Descriptive statistics
#> 
#>  Variable                       │ Group              M     SD    Min    Max  
#> ────────────────────────────────┼────────────────────────────────────────────
#>  Body mass index                │ Lower secondary  28.09  3.47  18.20  38.90 
#>                                 │ Upper secondary  26.02  3.43  16.00  37.10 
#>                                 │ Tertiary         24.39  3.52  16.00  33.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary   2.71  1.20   1.00   5.00 
#>                                 │ Upper secondary   3.53  1.19   1.00   5.00 
#>                                 │ Tertiary          4.11  1.04   1.00   5.00 
#> 
#>  Variable                       │ Group            95% CI LL  95% CI UL   n  
#> ────────────────────────────────┼────────────────────────────────────────────
#>  Body mass index                │ Lower secondary    27.66      28.51    260 
#>                                 │ Upper secondary    25.73      26.31    534 
#>                                 │ Tertiary           24.04      24.74    394 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary     2.57       2.86    259 
#>                                 │ Upper secondary     3.43       3.63    534 
#>                                 │ Tertiary            4.01       4.21    399 
#> 
#>  Variable                       │ Group              p   
#> ────────────────────────────────┼────────────────────────
#>  Body mass index                │ Lower secondary  <.001 
#>                                 │ Upper secondary        
#>                                 │ Tertiary               
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary  <.001 
#>                                 │ Upper secondary        
#>                                 │ Tertiary
```

``` r

table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  vcov = "HC3",
  output = "data.frame"
)
#>                                      Variable M (Female) M (Male)
#> wellbeing_score WHO-5 wellbeing index (0-100)   67.16194 71.04879
#> bmi                           Body mass index   25.68506 26.19685
#>                 Δ (Male - Female)  95% CI LL 95% CI UL            p          R²
#> wellbeing_score         3.8868576 2.12265210 5.6510631 1.670572e-05 0.015475137
#> bmi                     0.5117882 0.08904596 0.9345305 1.769614e-02 0.004728908
#>                    n
#> wellbeing_score 1200
#> bmi             1188
```

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(fit)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.66  [61.95, 68.45]  <.001 
#>  age             │    0.05  0.03  [-0.01,  0.11]   .130 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    3.86  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │     –     –          –          –    
#>    Yes           │   -1.72  1.11  [-3.89,  0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
```

Regression tables cover 30+ model classes with the conventions of each
family. A mixed-effects fit, for example, reports its random effects as
rows (SD, correlation, residual – each with SE and CI), the ICC and
group sizes as fit statistics, and the likelihood-ratio test of the
random part with the boundary-correct chi-bar-squared p-value:

``` r

library(lme4)
fit_mixed <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
table_regression(fit_mixed)
#> Linear mixed-effects regression: Reaction
#> 
#>  Variable                        │    B      SE        95% CI         p   
#> ─────────────────────────────────┼────────────────────────────────────────
#>  (Intercept)                     │  251.41  6.82  [238.03, 264.78]  <.001 
#>  Days                            │   10.47  1.55  [  7.44,  13.50]  <.001 
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
#> p-values: Wald-z, large-sample approximation. Load `lmerTest` for Satterthwaite t-tests.
#> Random effects (REML): LR test vs linear regression, χ̄²(3) = 148.35, p < .001.
```

See [Categorical summary
tables](https://amaltawfik.github.io/spicy/articles/table-categorical.html)
for categorical summaries, [Continuous summary
tables](https://amaltawfik.github.io/spicy/articles/table-continuous.html)
for continuous summaries and group comparisons, [Model-based continuous
summary
tables](https://amaltawfik.github.io/spicy/articles/table-continuous-lm.html)
for weighted or robust linear-model reporting, [Regression coefficient
tables](https://amaltawfik.github.io/spicy/articles/table-regression.html)
for regression tables across model families, and [Summary tables for
APA-style
reporting](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.html)
for an overview of summary tables.

### Row-wise summaries

``` r

df <- data.frame(
  x1 = c(10, NA, 30, 40, 50),
  x2 = c(5, NA, 15, NA, 25),
  x3 = c(NA, 30, 20, 50, 10)
)

mean_n(df)
#> [1]       NA       NA 21.66667       NA 28.33333
sum_n(df, min_valid = 2)
#> [1] 15 NA 65 90 85
count_n(df, special = "NA")
#> [1] 1 2 0 1 0
```

See [Getting started with
spicy](https://amaltawfik.github.io/spicy/articles/spicy.html) for a
longer workflow using
[`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
[`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md), and
[`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md).

### Label extraction

``` r

# LimeSurvey-style headers: "code. label"
df <- tibble::tibble(
  "age. Age of respondent" = c(25, 30),
  "score. Total score" = c(12, 14)
)
out <- label_from_names(df)
labelled::var_label(out)
#> $age
#> [1] "Age of respondent"
#> 
#> $score
#> [1] "Total score"
```

See [Explore variables and build
codebooks](https://amaltawfik.github.io/spicy/articles/variable-exploration.html)
for more on
[`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md),
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md),
and
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md).

------------------------------------------------------------------------

## Documentation

Each workflow has a dedicated vignette:

- [Getting started with
  spicy](https://amaltawfik.github.io/spicy/articles/spicy.html)
- [Explore variables and build
  codebooks](https://amaltawfik.github.io/spicy/articles/variable-exploration.html)
- [Frequency tables and
  cross-tabulations](https://amaltawfik.github.io/spicy/articles/frequency-tables.html)
- [Cramer’s V, Phi, and association
  measures](https://amaltawfik.github.io/spicy/articles/association-measures.html)
- [Categorical summary
  tables](https://amaltawfik.github.io/spicy/articles/table-categorical.html)
- [Continuous summary
  tables](https://amaltawfik.github.io/spicy/articles/table-continuous.html)
- [Model-based continuous summary
  tables](https://amaltawfik.github.io/spicy/articles/table-continuous-lm.html)
- [Regression coefficient
  tables](https://amaltawfik.github.io/spicy/articles/table-regression.html)
- [Mixed-effects regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.html)
- [Count and two-part regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression-counts.html)
- [Ordinal regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.html)
- [Summary tables for APA-style
  reporting](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.html)

Key reference pages:
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.html),
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.html),
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.html),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.html),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.html),
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.html),
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.html),
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.html),
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.html),
[`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.html),
[`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.html),
[`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.html),
and
[`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.html)
– see the [full function
index](https://amaltawfik.github.io/spicy/reference/index.html) for
everything else.

------------------------------------------------------------------------

## Citation

To cite spicy in a publication or teaching material:

- Use `citation("spicy")` to generate the current BibTeX entry.
- Package DOI: <https://doi.org/10.32614/CRAN.package.spicy>.
- Source citation file:
  <https://github.com/amaltawfik/spicy/blob/main/inst/CITATION>

------------------------------------------------------------------------

## License

MIT. See [`LICENSE`](https://amaltawfik.github.io/spicy/LICENSE) for
details.
