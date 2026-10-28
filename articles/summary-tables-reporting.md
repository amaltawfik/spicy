# Summary tables for reporting

``` r

library(spicy)
```

spicy’s reporting helpers cover the full APA Manual 7 table sequence
used in empirical articles:

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  and
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  build **Table 1** (sample characteristics) and **Table 2** (group
  comparisons);
- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  extends Table 2 to the linear-model regime when group means need
  robust or cluster-robust SE, covariate adjustment, or group tests
  under case weights — the descriptive families take `weights =`
  natively for the estimates themselves, and
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  points here the moment weighted *means* are to be tested;
- [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
  and
  [`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
  build the same Tables 1 and 2 from a
  [`survey::svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html)
  object, when the sample is stratified, clustered, or carried by
  replicate weights — design-based SEs and CIs, design degrees of
  freedom, design tests (see [*Summary tables from a survey
  design*](https://amaltawfik.github.io/spicy/articles/survey-tables.md));
- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  builds **Table 3** (the coefficient table) from one or several fitted
  models — [`lm()`](https://rdrr.io/r/stats/lm.html) /
  [`glm()`](https://rdrr.io/r/stats/glm.html) through the 39 supported
  classes listed by
  [`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md).

These functions share the same output grammar — the same `output`
formats (`default` console ASCII, `gt`, `tinytable`, `flextable`,
`word`, `excel`, `clipboard`), the same `decimal_mark`, `p_digits`,
`labels`, and `align` arguments, and the same `digits` control for
numeric cells (the categorical table’s cells are percentages, so it
spells the argument `percent_digits`) — so a single reporting workflow
can move smoothly from descriptive to inferential without juggling
different APIs. This article focuses on that shared logic; the
function-specific articles cover the methodological options in depth.

## Choose the right function

Use the function that matches the unit you want to report:

| Function | Reports | Selection grammar | Typical additions |
|:---|:---|:---|:---|
| [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md) | Categorical variables (factors, labelled) | `select`, `by` | Chi-squared or Fisher’s exact test, association measure (`phi`, `cramer_v`, `tau_b`, …), confidence interval |
| [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md) | Numeric / continuous variables | `select`, `by` | Group-comparison test (Student / Welch *t*, Wilcoxon, ANOVA, Kruskal–Wallis), effect size (Hedges’s *g*, η², rank-biserial *r*, ε²) |
| [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md) | Numeric outcomes through one linear model per outcome | `select`, `by` (single predictor) | Robust / cluster-robust / bootstrap / jackknife SE, case weights, additive covariate adjustment, four effect-size measures with noncentral CIs |
| [`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md) | *One* numeric outcome across the levels of *several* categorical variables | `outcome` (one), `select` (many) | One group comparison per block, an `Overall` marginal row, the same statistic tokens as [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md) |
| [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md) (experimental) | A “check all that apply” question, one row per item | `select` (the items), `by` | Chi-squared or Fisher’s exact test per item with a Holm adjustment, a global test that allows for several answers per respondent |
| [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md), [`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md) | The same descriptive tables from a [`survey::svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html) (stratified, clustered, replicate-weight samples) | design-first: `design`, then `select`, `by` | Design-based SE and CI, design df, Rao–Scott and design *t*/*F* tests, design effects, observed and weighted counts |
| [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md) | One or several fitted models — 39 classes, from [`lm()`](https://rdrr.io/r/stats/lm.html) / [`glm()`](https://rdrr.io/r/stats/glm.html) to mixed, ordinal, survival and Bayesian engines (see [*Supported models*](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.md)) | Fit-first: pass the model object(s) directly, no `select` / `by` | APA-aligned coefficient table with `B`, `β`, `95% CI`, `p`, AME, robust variance, side-by-side and hierarchical layouts |

In practice, follow the APA sequence:

- start with
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  for smoking, education, or activity — APA Table 1 categorical
  descriptors;
- use
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  for BMI, well-being, or income — Table 1 continuous descriptors and
  Table 2 unadjusted group comparisons;
- transpose to
  [`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md)
  when the question is about *one* outcome and many groupings rather
  than many outcomes and one grouping – a well-being score described by
  sex, by education and by region, block after block;
- move to
  [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
  /
  [`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
  when the data are a survey design rather than a plain sample – the
  numbers then come from the survey package, with the design’s SEs,
  degrees of freedom and tests;
- switch to
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  when the same comparison must account for case weights, robust SE, or
  covariate adjustment;
- finish with
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  once the substantive model is fitted — APA Table 3 with all
  predictors, factor groupings, reference rows, and (optionally)
  standardized coefficients, marginal effects, or nested model
  comparisons.

The descriptive functions share one selection grammar — on a data frame,
or on a
[`survey::svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html) for
the `_svy` twins;
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
is **fit-first** — you build the model the usual R way
([`lm()`](https://rdrr.io/r/stats/lm.html),
[`glm()`](https://rdrr.io/r/stats/glm.html), or any other supported
engine) and hand the object in. All of them share the post-construction
grammar (`output`, `labels`, `decimal_mark`, `align`, and the digits
controls), so swapping functions never breaks your rendering pipeline.

## A shared interface

The examples below use `sochealth`, the dataset bundled with spicy: a
simulated social-health survey of 1200 respondents and 29 variables,
every one of them carrying a variable label (see
[`?sochealth`](https://amaltawfik.github.io/spicy/reference/sochealth.md)).

The descriptive functions share the same core arguments:

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c(
    smoking           = "Smoking status",
    physical_activity = "Regular physical activity"
  )
)
#> Categorical table by education
#> 
#>  Variable                  │ Lower secondary n  Lower secondary % 
#> ───────────────────────────┼──────────────────────────────────────
#>  Smoking status            │                                      
#>    No                      │        179               69.6        
#>    Yes                     │         78               30.4        
#>    (Missing)               │          4                           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity │                                      
#>    No                      │        177               67.8        
#>    Yes                     │         84               32.2        
#> 
#>  Variable                  │ Upper secondary n  Upper secondary %  Tertiary n 
#> ───────────────────────────┼──────────────────────────────────────────────────
#>  Smoking status            │                                                  
#>    No                      │        415               78.7            332     
#>    Yes                     │        112               21.3             59     
#>    (Missing)               │         12                                 9     
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity │                                                  
#>    No                      │        310               57.5            163     
#>    Yes                     │        229               42.5            237     
#> 
#>  Variable                  │ Tertiary %  Total n  Total %    p     V  
#> ───────────────────────────┼──────────────────────────────────────────
#>  Smoking status            │                               <.001  .14 
#>    No                      │    84.9       926     78.8               
#>    Yes                     │    15.1       249     21.2               
#>    (Missing)               │                25                        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity │                               <.001  .21 
#>    No                      │    40.8       650     54.2               
#>    Yes                     │    59.2       550     45.8               
#> 
#> Note. N = 1200: Lower secondary (n = 261), Upper secondary (n = 539), Tertiary (n = 400). Percentages are column percentages. P-values: Pearson chi-squared test. V = Cramer's V.
```

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  labels = c(
    bmi = "Body mass index",
    wellbeing_score = "Well-being score",
    life_sat_health = "Satisfaction with health"
  )
)
#> Descriptive statistics by education
#> 
#>  Variable                 │ Group              M     SD     Min    Max   
#> ──────────────────────────┼──────────────────────────────────────────────
#>  Body mass index          │ Lower secondary  28.09   3.47  18.20   38.90 
#>                           │ Upper secondary  26.02   3.43  16.00   37.10 
#>                           │ Tertiary         24.39   3.52  16.00   33.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Well-being score         │ Lower secondary  57.22  15.44  18.70   97.90 
#>                           │ Upper secondary  68.97  13.62  26.70  100.00 
#>                           │ Tertiary         76.85  13.23  40.40  100.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health │ Lower secondary   2.71   1.20   1.00    5.00 
#>                           │ Upper secondary   3.53   1.19   1.00    5.00 
#>                           │ Tertiary          4.11   1.04   1.00    5.00 
#> 
#>  Variable                 │ Group            95% CI LL  95% CI UL   n     p   
#> ──────────────────────────┼───────────────────────────────────────────────────
#>  Body mass index          │ Lower secondary    27.66      28.51    260  <.001 
#>                           │ Upper secondary    25.73      26.31    534        
#>                           │ Tertiary           24.04      24.74    394        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Well-being score         │ Lower secondary    55.33      59.10    261  <.001 
#>                           │ Upper secondary    67.82      70.12    539        
#>                           │ Tertiary           75.55      78.15    400        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health │ Lower secondary     2.57       2.86    259  <.001 
#>                           │ Upper secondary     3.43       3.63    534        
#>                           │ Tertiary            4.01       4.21    399        
#> 
#> Note. Missing values removed: Body mass index (12), Satisfaction with health (8). P-values: Welch one-way ANOVA. CI = confidence interval; LL = lower limit; UL = upper limit.
```

``` r

table_continuous_lm(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  weights = weight,
  vcov = "HC3"
)
#> Continuous outcomes by education
#> 
#>                                        M                                   
#>                    ──────────────────────────────────────────              
#>  Variable        │ Lower secondary  Upper secondary  Tertiary    p     R²  
#> ─────────────────┼─────────────────────────────────────────────────────────
#>  bmi             │      27.85            25.79        24.23    <.001  0.13 
#>  wellbeing_score │      57.71            68.39        76.55    <.001  0.19 
#>  life_sat_health │       2.75             3.50         4.09    <.001  0.15 
#> 
#>  Variable        │  n   
#> ─────────────────┼──────
#>  bmi             │ 1188 
#>  wellbeing_score │ 1200 
#>  life_sat_health │ 1192 
#> 
#> Note. Statistics weighted by weight. Missing values removed: bmi (12), life_sat_health (8). P-values: F-test from the linear model. Std. errors: heteroskedasticity-robust (HC3).
```

Two words on the weighted example. `weights` supplies **case weights**,
passed to `lm(weights = )` — appropriate for weighted article tables,
but not a substitute for a full complex-survey design (strata, clusters,
calibration), which is the `survey` package’s domain. And because
`sochealth$weight` holds calibrated sampling weights, the example pairs
them with a heteroskedasticity-robust variance (`vcov = "HC3"`): the
default `"classical"` WLS variance would treat the weights as precision
weights, which sampling weights are not.

The same argument pattern is used in all three cases:

- `select` chooses the reported variables;
- `by` defines the grouping structure;
- `labels` cleans up the row labels;
- `output` decides how the result is rendered or exported.

For model-based continuous tables, the same pattern applies, but `by`
must be a single predictor because one linear model is fit per outcome.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
joins the same `labels` / `output` / `decimal_mark` / `digits` grammar
but is **fit-first**: rather than expressing model structure inline
through `select` and `by`, you pass one or several already-fitted
[`lm()`](https://rdrr.io/r/stats/lm.html) or
[`glm()`](https://rdrr.io/r/stats/glm.html) objects:

``` r

fit <- lm(
  wellbeing_score ~ age + sex + smoking + physical_activity,
  data = sochealth
)
table_regression(
  fit,
  labels = c(
    age               = "Age (years)",
    sex               = "Sex",
    smoking           = "Smoking status",
    physical_activity = "Regular physical activity"
  )
)
#> Linear regression: wellbeing_score
#> 
#>  Variable                   │    B      SE       95% CI        p   
#> ────────────────────────────┼──────────────────────────────────────
#>  (Intercept)                │   64.18  1.69  [60.87, 67.49]  <.001 
#>  Age (years)                │    0.04  0.03  [-0.02,  0.10]   .171 
#>  Sex:                       │                                      
#>    Female (ref.)            │     –     –          –          –    
#>    Male                     │    3.88  0.90  [ 2.11,  5.65]  <.001 
#>  Smoking status:            │                                      
#>    No (ref.)                │     –     –          –          –    
#>    Yes                      │   -1.73  1.10  [-3.90,  0.43]   .117 
#>  Regular physical activity: │                                      
#>    No (ref.)                │     –     –          –          –    
#>    Yes                      │    2.70  0.91  [ 0.93,  4.48]   .003 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                          │ 1175                                 
#>  R²                         │    0.03                              
#>  Adj. R²                    │    0.02                              
#> 
#> Note. Linear regression. Std. errors: classical (OLS). CI = confidence interval.
```

This split is intentional. The descriptive functions report the *data* —
`select` and `by` describe what you want to see.
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reports the *model* — the model formula has already declared which
predictors, interactions, polynomials, transformations, splines, and
contrasts to report, so passing those again through `select` / `by`
would duplicate the model object’s information and risk diverging from
it.

## A practical reporting sequence

A common report contains both table types, often with the same grouping
variable. For example, you might first summarize categorical health
behaviors, then summarize continuous well-being indicators.

### Categorical variables

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education,
  labels = c(
    smoking           = "Smoking status",
    physical_activity = "Regular physical activity",
    dentist_12m       = "Visited a dentist in the last 12 months"
  )
)
#> Categorical table by education
#> 
#>  Variable                                │ Lower secondary n  Lower secondary % 
#> ─────────────────────────────────────────┼──────────────────────────────────────
#>  Smoking status                          │                                      
#>    No                                    │        179               69.6        
#>    Yes                                   │         78               30.4        
#>    (Missing)                             │          4                           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity               │                                      
#>    No                                    │        177               67.8        
#>    Yes                                   │         84               32.2        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Visited a dentist in the last 12 months │                                      
#>    No                                    │        113               43.3        
#>    Yes                                   │        148               56.7        
#> 
#>  Variable                                │ Upper secondary n  Upper secondary % 
#> ─────────────────────────────────────────┼──────────────────────────────────────
#>  Smoking status                          │                                      
#>    No                                    │        415               78.7        
#>    Yes                                   │        112               21.3        
#>    (Missing)                             │         12                           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity               │                                      
#>    No                                    │        310               57.5        
#>    Yes                                   │        229               42.5        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Visited a dentist in the last 12 months │                                      
#>    No                                    │        174               32.3        
#>    Yes                                   │        365               67.7        
#> 
#>  Variable                                │ Tertiary n  Tertiary %  Total n 
#> ─────────────────────────────────────────┼─────────────────────────────────
#>  Smoking status                          │                                 
#>    No                                    │    332         84.9       926   
#>    Yes                                   │     59         15.1       249   
#>    (Missing)                             │      9                     25   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity               │                                 
#>    No                                    │    163         40.8       650   
#>    Yes                                   │    237         59.2       550   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Visited a dentist in the last 12 months │                                 
#>    No                                    │     67         16.8       354   
#>    Yes                                   │    333         83.2       846   
#> 
#>  Variable                                │ Total %    p     V  
#> ─────────────────────────────────────────┼─────────────────────
#>  Smoking status                          │          <.001  .14 
#>    No                                    │  78.8               
#>    Yes                                   │  21.2               
#>    (Missing)                             │                     
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity               │          <.001  .21 
#>    No                                    │  54.2               
#>    Yes                                   │  45.8               
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Visited a dentist in the last 12 months │          <.001  .22 
#>    No                                    │  29.5               
#>    Yes                                   │  70.5               
#> 
#> Note. N = 1200: Lower secondary (n = 261), Upper secondary (n = 539), Tertiary (n = 400). Percentages are column percentages. P-values: Pearson chi-squared test. V = Cramer's V.
```

### Continuous variables

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  labels = c(
    bmi = "Body mass index",
    wellbeing_score = "Well-being score",
    life_sat_health = "Satisfaction with health"
  ),
  p_value = TRUE,
  effect_size = "auto"
)
#> Descriptive statistics by education
#> 
#>  Variable                 │ Group              M     SD     Min    Max   
#> ──────────────────────────┼──────────────────────────────────────────────
#>  Body mass index          │ Lower secondary  28.09   3.47  18.20   38.90 
#>                           │ Upper secondary  26.02   3.43  16.00   37.10 
#>                           │ Tertiary         24.39   3.52  16.00   33.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Well-being score         │ Lower secondary  57.22  15.44  18.70   97.90 
#>                           │ Upper secondary  68.97  13.62  26.70  100.00 
#>                           │ Tertiary         76.85  13.23  40.40  100.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health │ Lower secondary   2.71   1.20   1.00    5.00 
#>                           │ Upper secondary   3.53   1.19   1.00    5.00 
#>                           │ Tertiary          4.11   1.04   1.00    5.00 
#> 
#>  Variable                 │ Group            95% CI LL  95% CI UL   n     p   
#> ──────────────────────────┼───────────────────────────────────────────────────
#>  Body mass index          │ Lower secondary    27.66      28.51    260  <.001 
#>                           │ Upper secondary    25.73      26.31    534        
#>                           │ Tertiary           24.04      24.74    394        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Well-being score         │ Lower secondary    55.33      59.10    261  <.001 
#>                           │ Upper secondary    67.82      70.12    539        
#>                           │ Tertiary           75.55      78.15    400        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health │ Lower secondary     2.57       2.86    259  <.001 
#>                           │ Upper secondary     3.43       3.63    534        
#>                           │ Tertiary            4.01       4.21    399        
#> 
#>  Variable                 │ Group               ES     
#> ──────────────────────────┼────────────────────────────
#>  Body mass index          │ Lower secondary  η² = 0.13 
#>                           │ Upper secondary            
#>                           │ Tertiary                   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Well-being score         │ Lower secondary  η² = 0.21 
#>                           │ Upper secondary            
#>                           │ Tertiary                   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health │ Lower secondary  η² = 0.16 
#>                           │ Upper secondary            
#>                           │ Tertiary                   
#> 
#> Note. Missing values removed: Body mass index (12), Satisfaction with health (8). P-values: Welch one-way ANOVA. CI = confidence interval; LL = lower limit; UL = upper limit; ES = effect size.
```

This keeps the reporting structure consistent while still using the
function that fits each variable type.

### Model-based continuous variables

``` r

table_continuous_lm(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = sex,
  vcov = "HC3",
  statistic = TRUE
)
#> Continuous outcomes by sex
#> 
#>                          M                                                      
#>                    ─────────────                                                
#>  Variable        │ Female  Male    Δ    95% CI LL  95% CI UL   t      p     R²  
#> ─────────────────┼──────────────────────────────────────────────────────────────
#>  bmi             │ 25.69   26.20  0.51     0.09      0.93     2.38   .018  0.00 
#>  wellbeing_score │ 67.16   71.05  3.89     2.12      5.65     4.32  <.001  0.02 
#>  life_sat_health │  3.51    3.59  0.08    -0.06      0.22     1.11   .267  0.00 
#> 
#>  Variable        │  n   
#> ─────────────────┼──────
#>  bmi             │ 1188 
#>  wellbeing_score │ 1200 
#>  life_sat_health │ 1192 
#> 
#> Note. Missing values removed: bmi (12), life_sat_health (8). P-values: t-test from the linear model. Std. errors: heteroskedasticity-robust (HC3). Δ = Male − Female. CI = confidence interval; LL = lower limit; UL = upper limit.
```

This is the better summary-table path when the article is already
organized around simple linear models, weighted analyses, or robust
standard errors.

### A balance table instead of a significance table

When the two groups are trial arms or a treated / control contrast, the
baseline table is read for *balance*, and the convention of that
literature is the standardized mean difference rather than the
*p*-value. Both descriptive families take `smd = TRUE`, with the same
meaning and the same refusals:

``` r

table_continuous(
  sochealth,
  select = c(age, bmi, wellbeing_score),
  by = sex,
  smd = TRUE,
  p_value = FALSE
)
#> Descriptive statistics by sex
#> 
#>  Variable        │ Group     M     SD     Min    Max    95% CI LL  95% CI UL 
#> ─────────────────┼───────────────────────────────────────────────────────────
#>  age             │ Female  49.38  14.91  25.00   75.00    48.20      50.55   
#>                  │ Male    49.14  14.50  25.00   75.00    47.96      50.32   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  bmi             │ Female  25.69   3.78  16.00   38.90    25.39      25.98   
#>                  │ Male    26.20   3.64  16.00   37.70    25.90      26.50   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Female  67.16  14.80  19.60  100.00    65.99      68.33   
#>                  │ Male    71.05  16.23  18.70  100.00    69.73      72.37   
#> 
#>  Variable        │ Group    n    SMD  
#> ─────────────────┼────────────────────
#>  age             │ Female  620   0.02 
#>                  │ Male    580        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  bmi             │ Female  616  -0.14 
#>                  │ Male    572        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Female  620  -0.25 
#>                  │ Male    580        
#> 
#> Note. Missing values removed: bmi (12). SMD direction: Female − Male. The usual imbalance threshold is |SMD| > 0.1. CI = confidence interval; LL = lower limit; UL = upper limit; SMD = standardized mean difference.
```

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  smd = TRUE,
  p_value = FALSE
)
#> Categorical table by sex
#> 
#>  Variable          │ Female n  Female %  Male n  Male %  Total n  Total %   φ  
#> ───────────────────┼───────────────────────────────────────────────────────────
#>  smoking           │                                                       .01 
#>    No              │   475       78.4     451     79.3     926     78.8        
#>    Yes             │   131       21.6     118     20.7     249     21.2        
#>    (Missing)       │    14                 11               25                 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │                                                       .01 
#>    No              │   334       53.9     316     54.5     650     54.2        
#>    Yes             │   286       46.1     264     45.5     550     45.8        
#> 
#>  Variable          │ SMD  
#> ───────────────────┼──────
#>  smoking           │ 0.02 
#>    No              │      
#>    Yes             │      
#>    (Missing)       │      
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌
#>  physical_activity │ 0.01 
#>    No              │      
#>    Yes             │      
#> 
#> Note. N = 1200: Female (n = 620), Male (n = 580). Percentages are column percentages. SMD direction: Female − Male. The usual imbalance threshold is |SMD| > 0.1. SMD = standardized mean difference.
```

Exactly two groups, no confidence interval and no *p*-value on the
column itself, and the usual rule of thumb (\|SMD\| \> 0.1) quoted in
the table note without any cell being highlighted. Both calls drop the
*p* column with `p_value = FALSE`.

### The coefficient table

Once the substantive model is fitted,
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
produces the APA Table 3 coefficient summary. The same `output` argument
controls rendering, so the regression table sits in the same reporting
pipeline as the descriptive ones above:

``` r

fit <- lm(
  wellbeing_score ~ age + sex + smoking + physical_activity,
  data = sochealth
)
table_regression(
  fit,
  standardized = "refit",
  show_columns = c("b", "beta", "ci", "p"),
  vcov = "HC3"
)
#> Linear regression: wellbeing_score
#> 
#>  Variable           │    B       β        95% CI        p   
#> ────────────────────┼───────────────────────────────────────
#>  (Intercept)        │   64.18  -0.18  [60.95, 67.42]  <.001 
#>  age                │    0.04   0.04  [-0.02,  0.10]   .169 
#>  sex:               │                                       
#>    Female (ref.)    │     –      –          –          –    
#>    Male             │    3.88   0.25  [ 2.10,  5.66]  <.001 
#>  smoking:           │                                       
#>    No (ref.)        │     –      –          –          –    
#>    Yes              │   -1.73  -0.11  [-3.92,  0.45]   .120 
#>  physical_activity: │                                       
#>    No (ref.)        │     –      –          –          –    
#>    Yes              │    2.70   0.17  [ 0.93,  4.48]   .003 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │ 1175                                  
#>  R²                 │    0.03                               
#>  Adj. R²            │    0.02                               
#> 
#> Note. Linear regression. Std. errors: heteroskedasticity-robust (HC3). β = standardized coefficient ("refit": outcome and numeric predictors z-scored, factor dummies on 0/1); CI = confidence interval.
```

The default footer documents the variance estimator, flags standardized
coefficients (`β = standardized coefficient`), and reports any
multiplicity correction, so the inferential regime is visible without
leaving the table. One thing it does not carry: the *name* of the
standardization method. Here `standardized = "refit"` produced the β
column, but the footer would read the same under any of the five
methods, so record the method in the table note or the text of the
article.

Side-by-side reporting of competing specifications (e.g., unadjusted
vs. covariate-adjusted, or `lm` vs. `glm`) is supported by passing a
list of fits:

``` r

fit_unadj <- lm(wellbeing_score ~ smoking, data = sochealth)
fit_adj   <- lm(
  wellbeing_score ~ smoking + age + sex + physical_activity,
  data = sochealth
)
table_regression(
  list("Unadjusted" = fit_unadj, "Adjusted" = fit_adj),
  show_columns = c("b", "ci", "p")
)
#> Linear regression comparison: wellbeing_score
#> 
#>                                 Unadjusted                   Adjusted         
#>                       ──────────────────────────────  ─────────────────────── 
#>  Variable           │    B         95% CI        p       B         95% CI     
#> ────────────────────┼─────────────────────────────────────────────────────────
#>  (Intercept)        │   69.36  [68.36, 70.37]  <.001    64.18  [60.87, 67.49] 
#>  smoking:           │                                                         
#>    No (ref.)        │     –          –          –         –          –        
#>    Yes              │   -1.72  [-3.91,  0.47]   .124    -1.73  [-3.90,  0.43] 
#>  age                │                                    0.04  [-0.02,  0.10] 
#>  sex:               │                                                         
#>    Female (ref.)    │                                     –          –        
#>    Male             │                                    3.88  [ 2.11,  5.65] 
#>  physical_activity: │                                                         
#>    No (ref.)        │                                     –          –        
#>    Yes              │                                    2.70  [ 0.93,  4.48] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │ 1175                            1175                    
#>  R²                 │    0.00                            0.03                 
#>  Adj. R²            │    0.00                            0.02                 
#> 
#>                       Adju… 
#>                       ───── 
#>  Variable           │ p (B) 
#> ────────────────────┼───────
#>  (Intercept)        │ <.001 
#>  smoking:           │       
#>    No (ref.)        │  –    
#>    Yes              │  .117 
#>  age                │  .171 
#>  sex:               │       
#>    Female (ref.)    │  –    
#>    Male             │ <.001 
#>  physical_activity: │       
#>    No (ref.)        │  –    
#>    Yes              │  .003 
#> 
#> Note. Linear regression models. Std. errors: classical (OLS). CI = confidence interval.
```

For binary or count outcomes, swap
[`lm()`](https://rdrr.io/r/stats/lm.html) for
[`glm()`](https://rdrr.io/r/stats/glm.html) and request response-scale
reporting (odds ratios, incidence rate ratios, etc.):

``` r

fit_glm <- glm(
  smoking ~ age + sex + physical_activity,
  data = sochealth,
  family = binomial()
)
table_regression(
  fit_glm,
  exponentiate = TRUE,
  show_columns = c("b", "ci", "p", "ame", "ame_ci", "ame_p")
)
#> Logistic regression: smoking
#> 
#>  Variable           │   OR        95% CI       p     AME      95% CI        p   
#> ────────────────────┼───────────────────────────────────────────────────────────
#>  (Intercept)        │    0.21  [0.13, 0.36]  <.001                              
#>  age                │    1.01  [1.00, 1.01]   .298   0.00  [-0.00, 0.00]   .297 
#>  sex:               │                                                           
#>    Female (ref.)    │     –         –         –       –          –         –    
#>    Male             │    0.95  [0.72, 1.26]   .723  -0.01  [-0.06, 0.04]   .723 
#>  physical_activity: │                                                           
#>    No (ref.)        │     –         –         –       –          –         –    
#>    Yes              │    1.02  [0.77, 1.35]   .883   0.00  [-0.04, 0.05]   .883 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │ 1175                                                      
#>  R² (McFadden)      │    0.00                                                   
#>  R² (Nagelkerke)    │    0.00                                                   
#>  AIC                │ 1220.5                                                    
#> 
#> Note. Logistic regression. Coefficients exponentiated and displayed as OR; CI bounds exponentiated. Std. errors: classical (Fisher information). CI = confidence interval; AME = average marginal effect; AIC = Akaike information criterion.
```

Average marginal effects (`ame`) are useful next to the odds ratio
because they report a probability-scale change for each predictor — the
quantity most reviewers want to interpret directly. Note the two `p`
columns: the first tests the coefficient (the log odds ratio), `ame_p`
tests the average marginal effect itself. The two can differ under
non-linear links or interactions, which is why
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
warns if you request `ame` alongside `p` without also requesting
`ame_p`.

For the epidemiological variant of Table 2 — a univariable screen of
every candidate predictor set against the multivariable model —
[`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)
builds the whole two-part layout in one call; see the *Univariable
screening* section of [*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md).

## Choose the output format

All of these functions support the same reporting formats:

| Output        | Best use                                     |
|:--------------|:---------------------------------------------|
| `"default"`   | Quick console review in plain ASCII          |
| `"tinytable"` | Quarto or R Markdown documents               |
| `"gt"`        | HTML output with styled reporting tables     |
| `"flextable"` | Office-first workflows; also renders in HTML |
| `"excel"`     | Spreadsheet handoff or downstream editing    |
| `"word"`      | Direct `.docx` export                        |
| `"clipboard"` | Fast pasting into another application        |

Pick the output based on where the table is going, not on the analysis
itself. The underlying selection and grouping pattern stays the same.

If you want an object that fits naturally into Word and PowerPoint
workflows but can also be rendered in HTML documents, `flextable` is a
good choice:

``` r

if (requireNamespace("flextable", quietly = TRUE)) {
  table_continuous(
    sochealth,
    select = c(bmi, wellbeing_score, life_sat_health),
    by = education,
    output = "flextable"
  )
}
```

## House styles

Everything above uses spicy’s defaults: two decimals, APA-style *p*
values (leading zero dropped, `< .001` floor), confidence intervals in
brackets. Those defaults are one house style among several.
[`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
names the others — each rule sourced from the journal’s published author
guidelines:

``` r

table_categorical(sochealth, select = smoking, by = sex, style = "jama")
#> Categorical table by sex
#> 
#>  Variable    │ Female n  Female %  Male n  Male %  Total n  Total %   p    φ  
#> ─────────────┼────────────────────────────────────────────────────────────────
#>  smoking     │                                                       .71  .01 
#>    No        │   475       78.4     451     79.3     926     78.8             
#>    Yes       │   131       21.6     118     20.7     249     21.2             
#>    (Missing) │    14                 11               25                      
#> 
#> Abbreviation: φ, phi coefficient.
#> N = 1200: Female (n = 620), Male (n = 580). Percentages are column percentages. P-value: Pearson chi-squared test.
```

Two things change here. JAMA rounds *p* values to two decimals (`.71`),
where the default reports three (`.713`), and the note opens without the
word “Note.”, as the AMA manual sets table notes.
`spicy_style(note_label = )` chooses that word for any style: “Note.”
(APA), “Note:” (Chicago), or none.

Two properties make styles safe to adopt. A style only moves *display*
defaults — decimals, *p* notation, interval punctuation, the word that
opens the note — never the statistics underneath; and an argument you
set explicitly always wins over it, so `style = "jama"` with
`p_digits = 3` keeps your three decimals. The style travels with the
table: the same call rendered to Word, Excel, or HTML follows it, and
`options(spicy.style = "lancet")` sets one for a whole document.
[`?spicy_style`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
lists every style and every rule with the sentence it comes from.

## Table language

A style is what a journal asks for. A language is what your reader
reads. `options(spicy.language = "fr")` prints the table in French —
headers, row labels, titles and table notes, and the numbers with them:

``` r

options(spicy.language = "fr")
table_continuous(sochealth, select = bmi, by = sex)
#> Statistiques descriptives selon sex
#> 
#>  Variable │ Groupe    M     ET    Min    Max   IC à 95 % LI  IC à 95 % LS   n  
#> ──────────┼────────────────────────────────────────────────────────────────────
#>  bmi      │ Female  25,69  3,78  16,00  38,90     25,39         25,98      616 
#>           │ Male    26,20  3,64  16,00  37,70     25,90         26,50      572 
#> 
#>  Variable │ Groupe    p   
#> ──────────┼───────────────
#>  bmi      │ Female  0,018 
#>           │ Male          
#> 
#> Note. Valeurs manquantes retirées : bmi (12). Valeur p : test t de Welch. IC = intervalle de confiance ; LI = limite inférieure ; LS = limite supérieure.
```

One gesture, one coherent table. A language brings its typography with
it: the decimal comma, and the leading zero French typography keeps on a
*p* value (`0,003` where the English default writes `.003`). The
language of a report is a property of the report, not of a call, so it
is set once in the setup chunk. `"en"` is the default and is unchanged
by any of this. The language reaches every table, the exploration pair
included:
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
take the comma from it too, and an argument you type wins. One boundary
is worth knowing: figures are frozen when a table is built, words when
it is printed, which is one more reason the language belongs in the
setup chunk.

A journal style composes with the language rather than replacing it. A
theme encodes only what its own author guidelines state, so whatever it
leaves open the language still fills — JAMA fixes no decimal mark, and a
French JAMA table keeps the comma:

``` r

table_continuous(sochealth, select = bmi, by = sex, style = "jama")
#> Statistiques descriptives selon sex
#> 
#>  Variable │ Groupe    M     ET    Min    Max   IC à 95 % LI  IC à 95 % LS   n  
#> ──────────┼────────────────────────────────────────────────────────────────────
#>  bmi      │ Female  25,69  3,78  16,00  38,90     25,39         25,98      616 
#>           │ Male    26,20  3,64  16,00  37,70     25,90         26,50      572 
#> 
#>  Variable │ Groupe   p  
#> ──────────┼─────────────
#>  bmi      │ Female  ,02 
#>           │ Male        
#> 
#> Abréviations : LI, limite inférieure ; LS, limite supérieure.
#> Valeurs manquantes retirées : bmi (12). Valeur p : test t de Welch.
```

Where the two do meet, the theme wins — you asked for it by name. JAMA’s
own rule drops the leading zero of a *p* value, so the table above
writes `,02` rather than `0,02`; `style = "lancet"` keeps the journal’s
midline decimal point instead of the comma. To keep a theme’s other
rules and restore the zero, compose the variant:
`style = spicy_style("jama", p_style = "standard")`.

An argument you type wins over both, which is the escape hatch for a
bilingual table — French words, decimal point. It moves only the mark:
the *p* value keeps its French leading zero (`0.018`), since the
leading-zero rule is a style lever, not an argument:

``` r

table_continuous(sochealth, select = bmi, by = sex, decimal_mark = ".")
#> Statistiques descriptives selon sex
#> 
#>  Variable │ Groupe    M     ET    Min    Max   IC à 95 % LI  IC à 95 % LS   n  
#> ──────────┼────────────────────────────────────────────────────────────────────
#>  bmi      │ Female  25.69  3.78  16.00  38.90     25.39         25.98      616 
#>           │ Male    26.20  3.64  16.00  37.70     25.90         26.50      572 
#> 
#>  Variable │ Groupe    p   
#> ──────────┼───────────────
#>  bmi      │ Female  0.018 
#>           │ Male          
#> 
#> Note. Valeurs manquantes retirées : bmi (12). Valeur p : test t de Welch. IC = intervalle de confiance ; LI = limite inférieure ; LS = limite supérieure.
```

When one word has to change and a language does not — a questionnaire
where a missing category means a refusal, not an absent value —
`options(spicy.labels = )` overrides labels one at a time, on top of
whatever language is in force:

``` r

options(spicy.labels = list(row_missing_level = "(No answer)"))
table_categorical(sochealth, select = sex, by = smoking)
#> Categorical table by smoking
#> 
#>  Variable │ No n  No %  Yes n  Yes %  (No answer) n  (No answer) %  Total n 
#> ──────────┼─────────────────────────────────────────────────────────────────
#>  sex      │                                                                 
#>    Female │ 475   51.3   131   52.6        14            56.0         620   
#>    Male   │ 451   48.7   118   47.4        11            44.0         580   
#> 
#>  Variable │ Total %   p     φ  
#> ──────────┼────────────────────
#>  sex      │          .713  .01 
#>    Female │  51.7              
#>    Male   │  48.3              
#> 
#> Note. N = 1200: No (n = 926), Yes (n = 249), (No answer) (n = 25). Percentages are column percentages. P-value: Pearson chi-squared test.
options(spicy.labels = NULL)
```

[`spicy_labels()`](https://amaltawfik.github.io/spicy/reference/spicy_labels.md)
is how you find the key for a label you want to change: it returns every
key with the text it currently resolves to.

Two things a language deliberately does not move. The column names of
the exported frames are a contract your code indexes into, so
`out[["Yes %"]]` resolves whatever the language — spicy translates its
own vocabulary, never your data. And errors and warnings stay in
English, because they are read by developers and quoted in bug reports.

The one column name that *does* follow the language comes from the same
rule read the other way: a column named after a level of `by` takes that
level’s spelling, and spicy’s own missing category is a level. So
`table_categorical(by = )` on a variable with missing values gives
`(Missing) n` in English and `(Manquant) n` in French. If your code
selects that column, build its name from
`spicy_labels()[["row_missing_level"]]` rather than typing it.

## Citing table values in the text

The number a sentence quotes should be the number the table prints —
retyping it is how a manuscript ends up saying 3.9 where the table says
3.90, or keeping a *p* value a revision changed.
[`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md)
returns one cell of a spicy table as text, formatted by the same
machinery that formatted the table:

``` r

fit <- lm(wellbeing_score ~ age + sex, data = sochealth)
tbl <- table_regression(fit)
inline(tbl, sex, "Male", "b")
#> [1] "3.90"
```

so in Quarto you write `` `r inline(tbl, sex, "Male", "b")` `` inside
the sentence. A `{token}` pattern quotes a full fragment in one call:

``` r

inline(tbl, sex, "Male", "{b} ({ci_label} {ci}; p {p})")
#> [1] "3.90 (95% CI [2.14, 5.65]; p <.001)"
```

Two properties carry the guarantee. The text follows the table: under
`style = "jama"`, `decimal_mark = ","` or a table language, the cited
string changes with the printed one. And the addressing survives
relabeling: rows are found by the source variable and level — not by the
displayed label — so `labels = c(sex = "Administrative sex")` changes
the table, not your calls. Misaddressing never fails silently: an
unknown variable, level, or column token errors with the list of
available choices, and a reference or non-estimable cell refuses with
its reason instead of pasting a dash into a sentence.

## Post-process the returned table object

All the table functions return regular `gt`, `tinytable`, or `flextable`
objects, so you can keep styling them with the native package API. This
includes
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
nothing about the fit-first interface changes what the rendering engine
produces.

Use `gt::` functions when you want to keep the `gt` workflow:

``` r

tab <- pkgdown_dark_gt(table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c(
    smoking           = "Smoking status",
    physical_activity = "Regular physical activity"
  ),
  output = "gt"
))

tab |>
  gt::tab_header(
    title = "Health behaviors by education",
    subtitle = "Categorical summary table"
  ) |>
  gt::tab_source_note(
    gt::md("*Percentages are computed within each education group.*")
  )
```

[TABLE]

*Note.* N = 1200: Lower secondary (n = 261), Upper secondary (n = 539),
Tertiary (n = 400). Percentages are column percentages. P-values:
Pearson chi-squared test. V = Cramer’s V.

Use `tinytable::` functions when you want lightweight table-specific
styling:

``` r

tab <- table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c(
    smoking           = "Smoking status",
    physical_activity = "Regular physical activity"
  ),
  output = "tinytable"
)

tab |>
  tinytable::style_tt(
    i = 2:3,
    j = 2:5,
    background = "red",
    color = "white",
    bold = TRUE
  )
```

| Variable | Lower secondary |  | Upper secondary |  | Tertiary |  | Total |  | p | V |
|----|----|----|----|----|----|----|----|----|----|----|
|  | n | % | n | % | n | % | n | % |  |  |
| Smoking status |     |      |     |      |     |      |     |      | \<.001 | .14 |
|     No | 179 | 69.6 | 415 | 78.7 | 332 | 84.9 | 926 | 78.8 |       |     |
|     Yes |  78 | 30.4 | 112 | 21.3 |  59 | 15.1 | 249 | 21.2 |       |     |
|     (Missing) |   4 |      |  12 |      |   9 |      |  25 |      |       |     |
| Regular physical activity |     |      |     |      |     |      |     |      | \<.001 | .21 |
|     No | 177 | 67.8 | 310 | 57.5 | 163 | 40.8 | 650 | 54.2 |       |     |
|     Yes |  84 | 32.2 | 229 | 42.5 | 237 | 59.2 | 550 | 45.8 |       |     |
| *Note.* N = 1200: Lower secondary (n = 261), Upper secondary (n = 539), Tertiary (n = 400). Percentages are column percentages. P-values: Pearson chi-squared test. V = Cramer's V. |  |  |  |  |  |  |  |  |  |  |

Categorical table by education {#tinytable_t19rr4d03acyhdbzki2i .table
.tinytable style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

Use `flextable::` functions when you want to keep working toward Office
or HTML document output. The example is shown as code here because the
dark pkgdown theme is not a reliable preview of the final `flextable`
HTML rendering:

``` r

if (requireNamespace("flextable", quietly = TRUE)) {
  tab <- table_continuous(
    sochealth,
    select = c(bmi, wellbeing_score),
    by = education,
    output = "flextable"
  )

  tab |>
    flextable::theme_booktabs() |>
    flextable::autofit() |>
    flextable::fontsize(size = 10, part = "all")
}
```

## Keep the detailed options in the function-specific articles

The dedicated articles go deeper into each function:

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  covers missing values, level filtering, association measures, and
  one-way frequency-style tables.
- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  covers grouped descriptive statistics, parametric and nonparametric
  tests, and effect sizes.
- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  covers estimated marginal means or slopes from linear models, robust /
  cluster-robust / bootstrap / jackknife variance, case weights,
  additive covariate adjustment (G-computation or equal-weight), and
  four effect-size measures with noncentral CIs.
- [`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md)
  covers the outcome-first layout: block structure, the `Overall` row,
  per-block comparisons and effect sizes, and the comparison with
  `gtsummary::tbl_continuous()`.
- [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
  and
  [`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
  cover the survey-design regime: delegation to the survey package,
  design degrees of freedom, quantile rules, and the weights-vs-design
  estimand boundary.
- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  covers single- and multi-model regression tables across 39 model
  classes (the map is [*Supported
  models*](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.md)),
  five standardization methods (four for linear models, plus the
  glm-only pseudo-standardization), partial effect sizes with
  noncentral-F CIs, average marginal effects, hierarchical
  (`nested = TRUE`) comparisons, multiplicity correction, and
  response-scale reporting for GLMs.

Use this article as the final reporting overview, then consult the
function-specific articles when you need the detailed controls.
