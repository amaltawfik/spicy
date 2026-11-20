# Regression tables for survey designs

``` r

library(spicy)
library(survey) # svydesign(), svyglm(), svyolr(), svycoxph()
```

This article covers regression on data from a complex survey: models
fitted with the survey package (Lumley, 2004) on a declared design,
[`svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html),
[`svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) and
[`svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html). The
descriptive side of the same designs is in [*Summary tables from a
survey
design*](https://amaltawfik.github.io/spicy/articles/survey-tables.md).
The companion article [*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
covers the shared mechanics (output engines, labels,
[`tidy()`](https://generics.r-lib.org/reference/tidy.html)), and the
class-by-class map is [*Supported
models*](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.md).

## Why a design, and what it changes

A survey sample is rarely a simple random sample. Units are drawn with
unequal probabilities, within strata, and often in clusters: schools
within districts, households within census tracts. Design-based
inference takes that selection mechanism as the source of randomness. In
Binder’s words, “the probabilities used to select the sample are then
used as the basis for statistical inference, and such inference refers
back to the finite population from which the random sample was selected”
(Binder, 2008, p. 191).

For a regression table, this has consequences in three places.

The coefficients solve weighted estimating equations. The estimator is a
pseudo-maximum likelihood estimator (Binder, 1992, p. 141): it has the
form of a likelihood equation, but the sample has no likelihood under
the design. Everything that needs one is therefore missing, the
likelihood-ratio test and the usual AIC first.

The variance comes from the design. Strata, clusters and a finite
population correction all enter it, by linearization or by replicate
weights. The weights alone do not determine it.

The reference distribution of the tests comes from the design too. Its
degrees of freedom are counted on the primary sampling units, not on the
rows.

Whether a regression should be weighted at all is a modelling question
that no table settles. When the sampling is informative, model-based
estimates “can be severely biased” (Binder, 2008, p. 193), and “the
appropriateness of the weighted regression estimate depends on which
model is chosen” (DuMouchel & Duncan, 1983, p. 535). This article starts
after that decision: the analysis is design-based, and the question is
how to report it.

## The designs used here

The examples use the data that ship with survey. The `api` data hold the
Academic Performance Index of Californian schools, with several samples
of the same population: `apiclus1` is a cluster sample of 15 school
districts, `apistrat` a sample stratified by school type. The `nhanes`
data are an extract of NHANES 2009-2010 on high cholesterol, with the
strata, the primary sampling units and the examination weights of the
survey.

``` r

data(api)
data(nhanes)

dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

nhanes$race <- factor(
  nhanes$race,
  levels = 1:4,
  labels = c("Hispanic", "White", "Black", "Other")
)
nhanes$sex <- factor(
  nhanes$RIAGENDR,
  levels = 1:2,
  labels = c("Male", "Female")
)
dnh <- svydesign(
  id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
  nest = TRUE, data = nhanes
)
dnh
#> Stratified 1 - level Cluster Sampling design (with replacement)
#> With (31) clusters.
#> svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, 
#>     nest = TRUE, data = nhanes)
```

## A first table

A linear model of the school’s score in 2000 on three characteristics of
its pupils: the percentage of English language learners (`ell`), the
percentage eligible for subsidized meals (`meals`), and the percentage
in their first year at the school (`mobility`).

``` r

gl <- svyglm(api00 ~ ell + meals + mobility, design = dclus1)
table_regression(gl)
#> Survey-weighted linear regression: api00
#> 
#>  Variable    │    B      SE         95% CI         p   
#> ─────────────┼─────────────────────────────────────────
#>  (Intercept) │  819.28  21.39  [772.20, 866.36]  <.001 
#>  ell         │   -0.52   0.32  [ -1.23,   0.20]   .139 
#>  meals       │   -3.12   0.28  [ -3.74,  -2.51]  <.001 
#>  mobility    │   -0.17   0.44  [ -1.15,   0.81]   .711 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │  183                                    
#>  Weighted n  │ 6194                                    
#>  AIC         │ 2004.7                                  
#> 
#> Note. Survey-weighted linear regression. Design: cluster (dnum), 15 PSUs, with finite population correction; 11 residual degrees of freedom. AIC of a design-based fit: design-based AIC (dAIC). Std. errors: design-based (Taylor linearization). CI = confidence interval; AIC = Akaike information criterion.
```

The coefficients, the standard errors and the p-values are those of
`summary(gl)`:
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reads a design-based fit and computes none of its inference. What it
adds is in the fit statistics and the note.

- `n` is the number of schools in the sample. `Weighted n` is the sum of
  their design weights, the 6,194 schools of the population the
  estimates describe.
- The design line of the note states what was declared, here a cluster
  sample of 15 PSUs with a finite population correction, and the degrees
  of freedom the tests of the table use.
- `Std. errors: design-based (Taylor linearization)` names the variance
  estimator. Linearization “uses the linear terms of a Taylor series
  expansion to approximate the estimator by a linear function of the
  observed data” (Williams, 2008, p. 876).

On the substance, a school loses 3.1 points of API for each additional
percentage point of pupils eligible for subsidized meals, with a 95%
interval from 2.5 to 3.7 points. The two other coefficients are not
distinguishable from zero in this sample.

## Degrees of freedom come from the PSUs

The table above rests on 183 schools and tests with 11 degrees of
freedom.

``` r

c(rows = nrow(apiclus1), design = degf(dclus1), model = df.residual(gl))
#>   rows design  model 
#>    183     14     11
```

The sample is a sample of 15 districts, and the schools of a district
were taken together. Clustering “results in fewer degrees of freedom in
the sense that the sample now focuses on a smaller number of sampling
units, that is, the first-stage clusters” (Lavrakas, 2008, p. 787). The
design has 15 PSUs in one stratum, hence 14 degrees of freedom, and
survey takes the residual degrees of freedom of the model by
subtraction: its help page says the rule “is correct for PSU-level
covariates but is potentially very conservative for individual-level
covariates” ([`?svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html),
survey 4.5). Three predictors leave 11.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reads that number on the fit and never derives it: the engines of survey
do not share one rule, and a Cox fit ends one degree of freedom above
what the same arithmetic gives for a
[`svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html).
[`survey::regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html)
takes the same number as the denominator of its F test.

The consequence is visible in every interval of the table. With 11
degrees of freedom the t quantile is 12% larger than the normal one:

``` r

qt(0.975, df.residual(gl)) / qnorm(0.975)
#> [1] 1.122972
```

A large file does not protect against this. The NHANES extract has 7,846
respondents with a cholesterol measure, drawn from 31 PSUs in 15 strata:
16 degrees of freedom for the design, and each coefficient of a model
takes one of them.

## Weights are not a design

[`lm()`](https://rdrr.io/r/stats/lm.html) and
[`glm()`](https://rdrr.io/r/stats/glm.html) accept a `weights` argument,
and passing the sampling weights there gives the right coefficients. It
does not give the right inference. The same model, both ways:

``` r

lw <- lm(api00 ~ ell + meals + mobility, data = apiclus1, weights = pw)
table_regression(
  list("lm(weights = pw)" = lw, "svyglm(design)" = gl),
  show_columns = c("b", "se", "p")
)
#> Regression comparison: api00
#> 
#>                  lm(weights = pw)        svyglm(design)     
#>                ─────────────────────  ───────────────────── 
#>  Variable    │    B      SE      p       B      SE      p   
#> ─────────────┼──────────────────────────────────────────────
#>  (Intercept) │  819.28  10.45  <.001   819.28  21.39  <.001 
#>  ell         │   -0.52   0.33   .116    -0.52   0.32   .139 
#>  meals       │   -3.12   0.21  <.001    -3.12   0.28  <.001 
#>  mobility    │   -0.17   0.39   .663    -0.17   0.44   .711 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │  183                    183                  
#>  Weighted n  │ 6194                   6194                  
#>  R²          │    0.72                    –                 
#>  Adj. R²     │    0.71                    –                 
#>  AIC         │ 2002.9                 2004.7                
#> 
#> Note. lm(weights = pw): linear regression; svyglm(design): survey-weighted linear regression. svyglm(design): Design: cluster (dnum), 15 PSUs, with finite population correction; 11 residual degrees of freedom. AIC of a design-based fit: design-based AIC (dAIC), not comparable with the likelihood AIC of the other models. Std. errors: lm(weights = pw): classical (OLS), svyglm(design): design-based (Taylor linearization). AIC = Akaike information criterion.
```

The two columns of coefficients are identical. The standard error of the
intercept doubles under the design, from 10.45 to 21.39, and that of
`meals` goes from 0.21 to 0.28.
[`lm()`](https://rdrr.io/r/stats/lm.html) treats the 183 schools as
independent draws, and reads each weight as a precision. The `R²` row is
filled for the [`lm()`](https://rdrr.io/r/stats/lm.html) only: spicy
reports no R² for a design-based fit.

The error does not always run in the same direction. A logistic model of
high cholesterol on NHANES, with the weights rescaled to average 1 for
[`glm()`](https://rdrr.io/r/stats/glm.html):

``` r

fit <- svyglm(
  HI_CHOL ~ agecat + race + sex,
  design = dnh,
  family = quasibinomial
)

nh <- subset(nhanes, !is.na(HI_CHOL))
nh$w <- nh$WTMEC2YR / mean(nh$WTMEC2YR)
naive <- glm(
  HI_CHOL ~ agecat + race + sex,
  data = nh,
  family = quasibinomial,
  weights = w
)

table_regression(
  list("glm(weights = )" = naive, "svyglm(design)" = fit),
  show_columns = c("b", "se", "p")
)
#> Regression comparison: HI_CHOL
#> 
#>                        glm(weights = )          svyglm(design)       
#>                      ────────────────────  ───────────────────────── 
#>  Variable          │    B      SE     p         B         SE     p   
#> ───────────────────┼─────────────────────────────────────────────────
#>  (Intercept)       │   -4.74  0.30  <.001         -4.74  0.32  <.001 
#>  agecat:           │                                                 
#>    (0,19] (ref.)   │     –     –     –              –     –     –    
#>    (19,39]         │    2.28  0.29  <.001          2.28  0.33  <.001 
#>    (39,59]         │    3.21  0.29  <.001          3.21  0.36  <.001 
#>    (59,Inf]        │    3.03  0.29  <.001          3.03  0.35  <.001 
#>  race:             │                                                 
#>    Hispanic (ref.) │     –     –     –              –     –     –    
#>    White           │   -0.08  0.11   .437         -0.08  0.08   .316 
#>    Black           │   -0.43  0.16   .007         -0.43  0.15   .019 
#>    Other           │   -0.15  0.18   .405         -0.15  0.34   .674 
#>  sex:              │                                                 
#>    Male (ref.)     │     –     –     –              –     –     –    
#>    Female          │    0.21  0.07   .004          0.21  0.08   .033 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                 │ 7846                       7846                 
#>  Weighted n        │ 7846                  255345910                 
#>  AIC               │     –                      5166.0               
#> 
#> Note. glm(weights = ): quasi-binomial regression; svyglm(design): survey-weighted logistic regression. svyglm(design): Design: stratified (SDMVSTRA), cluster (SDMVPSU), 31 PSUs; 9 residual degrees of freedom. AIC of a design-based fit: design-based AIC (dAIC). Std. errors: glm(weights = ): classical (Fisher information), svyglm(design): design-based (Taylor linearization). AIC = Akaike information criterion.
```

``` r

round(SE(fit) / sqrt(diag(vcov(naive))), 2)
#>    (Intercept)  agecat(19,39]  agecat(39,59] agecat(59,Inf]      raceWhite 
#>           1.08           1.12           1.24           1.21           0.73 
#>      raceBlack      raceOther      sexFemale 
#>           0.94           1.92           1.15
```

The design-based standard errors range from 0.73 to 1.92 times those of
the weighted [`glm()`](https://rdrr.io/r/stats/glm.html), and the
p-values move with them: `Female` from .004 to .033, `Black` from .007
to .019, `White` from .437 to .316. The weighted
[`glm()`](https://rdrr.io/r/stats/glm.html) is neither conservative nor
liberal, so it cannot serve as a bound. Its tests also refer to 7,838
residual degrees of freedom, where the design has 9.

Both fits use `family = quasibinomial`. For
[`svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) this is the
convention of survey: the quasi families “give the same point estimates
and standard errors” as `binomial` and avoid a warning about non-integer
numbers of successes
([`?svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html)).

## A logistic model on NHANES

``` r

table_regression(fit, exponentiate = TRUE)
#> Survey-weighted logistic regression: HI_CHOL
#> 
#>  Variable          │      OR        SE       95% CI        p   
#> ───────────────────┼───────────────────────────────────────────
#>  (Intercept)       │         0.01  0.00  [ 0.00,  0.02]  <.001 
#>  agecat:           │                                           
#>    (0,19] (ref.)   │          –     –          –          –    
#>    (19,39]         │         9.77  3.20  [ 4.66, 20.48]  <.001 
#>    (39,59]         │        24.84  8.84  [11.10, 55.56]  <.001 
#>    (59,Inf]        │        20.70  7.26  [ 9.36, 45.74]  <.001 
#>  race:             │                                           
#>    Hispanic (ref.) │          –     –          –          –    
#>    White           │         0.92  0.07  [ 0.77,  1.10]   .316 
#>    Black           │         0.65  0.10  [ 0.46,  0.91]   .019 
#>    Other           │         0.86  0.29  [ 0.40,  1.85]   .674 
#>  sex:              │                                           
#>    Male (ref.)     │          –     –          –          –    
#>    Female          │         1.24  0.10  [ 1.02,  1.50]   .033 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                 │      7846                                 
#>  Weighted n        │ 255345910                                 
#>  AIC               │      5166.0                               
#> 
#> Note. Survey-weighted logistic regression. Design: stratified (SDMVSTRA), cluster (SDMVPSU), 31 PSUs; 9 residual degrees of freedom. AIC of a design-based fit: design-based AIC (dAIC). Coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric). The (Intercept) row estimates the baseline odds. Std. errors: design-based (Taylor linearization). CI = confidence interval; AIC = Akaike information criterion.
```

With `exponentiate = TRUE` the coefficients are odds ratios, the
standard errors are on the odds-ratio scale by the delta method, and the
intervals are the exponentiated bounds of the intervals on the log-odds
scale, which is why they are not symmetric. The note says all three.

Age dominates. Compared with respondents of 19 or younger, the odds of
high cholesterol are about 10 times higher between 20 and 39, and 20 to
25 times higher after 40. The intervals are wide, from 4.7 to 20.5 for
the first of these odds ratios: they carry the 9 degrees of freedom of
the design. Women have higher odds than men, 1.24 times, with an
interval that starts at 1.02.

## Average marginal effects under the design

An odds ratio of 1.24 says nothing about how many more women than men
have high cholesterol. The average marginal effect answers on the
probability scale.

``` r

table_regression(fit, show_columns = c("b", "ame", "ame_ci", "ame_p"))
#> Survey-weighted logistic regression: HI_CHOL
#> 
#>  Variable          │      B         AME       95% CI        p   
#> ───────────────────┼────────────────────────────────────────────
#>  (Intercept)       │        -4.74                               
#>  agecat:           │                                            
#>    (0,19] (ref.)   │          –      –          –          –    
#>    (19,39]         │         2.28   0.07  [ 0.05,  0.09]  <.001 
#>    (39,59]         │         3.21   0.17  [ 0.14,  0.20]  <.001 
#>    (59,Inf]        │         3.03   0.14  [ 0.11,  0.18]  <.001 
#>  race:             │                                            
#>    Hispanic (ref.) │          –      –          –          –    
#>    White           │        -0.08  -0.01  [-0.03,  0.01]   .313 
#>    Black           │        -0.43  -0.04  [-0.07, -0.01]   .011 
#>    Other           │        -0.15  -0.01  [-0.09,  0.06]   .660 
#>  sex:              │                                            
#>    Male (ref.)     │          –      –          –          –    
#>    Female          │         0.21   0.02  [ 0.00,  0.04]   .029 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                 │      7846                                  
#>  Weighted n        │ 255345910                                  
#>  AIC               │      5166.0                                
#> 
#> Note. Survey-weighted logistic regression. Design: stratified (SDMVSTRA), cluster (SDMVPSU), 31 PSUs; 9 residual degrees of freedom. AIC of a design-based fit: design-based AIC (dAIC). Std. errors: design-based (Taylor linearization). AME = average marginal effect; CI = confidence interval; AIC = Akaike information criterion.
```

At the same age group and race, the probability of high cholesterol is
on average 2 percentage points higher for women than for men, with an
interval from 0 to 4 points. Between 40 and 59 it is 17 points above
that of the youngest group.

Two things make these effects design-based. The average is taken with
the design weights, so it describes the population and not the sample.
The interval uses the variance of the design and the same t reference
with 9 degrees of freedom. The p-values of the two columns are close
without being equal (.019 and .011 for `Black`): a coefficient and its
average marginal effect are two different statistics.

## Testing a term: Wald and Rao-Scott

`race` enters the model with three coefficients, and one of them,
`Black`, has p = .019. That is not a test of race. With four groups, one
comparison in three below .05 is a weak basis, and the comparison
depends on the reference category. The question “does race belong in
this model” is a test of the three coefficients together.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
has no term-level test column for a design-based fit. survey provides
the tests:

``` r

regTermTest(fit, ~race)
#> Wald test for race
#>  in svyglm(formula = HI_CHOL ~ agecat + race + sex, design = dnh, 
#>     family = quasibinomial)
#> F =  3.105771  on  3  and  9  df: p= 0.081573
regTermTest(fit, ~race, method = "LRT")
#> Working (Rao-Scott+F) LRT for race
#>  in svyglm(formula = HI_CHOL ~ agecat + race + sex, design = dnh, 
#>     family = quasibinomial)
#> Working 2logLR =  4.144661 p= 0.27411 
#> (scale factors:  2.3 0.43 0.25 );  denominator df= 9
```

The first is a Wald test, F = 3.11 on 3 and 9 degrees of freedom, p =
.082. The second is the Rao-Scott test: the likelihood-ratio statistic
of the working model “where the observations are independent and the
weights are frequency weights”, referred to a distribution that accounts
for the design
([`?regTermTest`](https://rdrr.io/pkg/survey/man/regTermTest.html)). It
gives p = .274. The two differ in size and agree on the reading: the
data do not establish a difference between the groups, whatever the
single row says.

The result of such a test can be reported under the table with
`note_add`:

``` r

table_regression(
  fit,
  exponentiate = TRUE,
  show_columns = c("b", "ci", "p"),
  note_add = "Race: Wald F(3, 9) = 3.11, p = .082."
)
#> Survey-weighted logistic regression: HI_CHOL
#> 
#>  Variable          │      OR           95% CI        p   
#> ───────────────────┼─────────────────────────────────────
#>  (Intercept)       │         0.01  [ 0.00,  0.02]  <.001 
#>  agecat:           │                                     
#>    (0,19] (ref.)   │          –          –          –    
#>    (19,39]         │         9.77  [ 4.66, 20.48]  <.001 
#>    (39,59]         │        24.84  [11.10, 55.56]  <.001 
#>    (59,Inf]        │        20.70  [ 9.36, 45.74]  <.001 
#>  race:             │                                     
#>    Hispanic (ref.) │          –          –          –    
#>    White           │         0.92  [ 0.77,  1.10]   .316 
#>    Black           │         0.65  [ 0.46,  0.91]   .019 
#>    Other           │         0.86  [ 0.40,  1.85]   .674 
#>  sex:              │                                     
#>    Male (ref.)     │          –          –          –    
#>    Female          │         1.24  [ 1.02,  1.50]   .033 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                 │      7846                           
#>  Weighted n        │ 255345910                           
#>  AIC               │      5166.0                         
#> 
#> Note. Survey-weighted logistic regression. Design: stratified (SDMVSTRA), cluster (SDMVPSU), 31 PSUs; 9 residual degrees of freedom. AIC of a design-based fit: design-based AIC (dAIC). Coefficients exponentiated and displayed as OR; CI bounds exponentiated. The (Intercept) row estimates the baseline odds. Std. errors: design-based (Taylor linearization). CI = confidence interval; AIC = Akaike information criterion. Race: Wald F(3, 9) = 3.11, p = .082.
```

## Comparing models without a likelihood

Two models side by side, with and without race:

``` r

fit0 <- svyglm(HI_CHOL ~ agecat + sex, design = dnh, family = quasibinomial)

table_regression(
  list("Age, sex" = fit0, "+ race" = fit),
  exponentiate = TRUE,
  show_columns = c("b", "ci", "p"),
  show_fit_stats = c("nobs", "aic", "eff_p")
)
#> Survey-weighted logistic regression comparison: HI_CHOL
#> 
#>                                    Age, sex                     + race          
#>                         ──────────────────────────────  ─────────────────────── 
#>  Variable             │   OR         95% CI        p      OR         95% CI     
#> ──────────────────────┼─────────────────────────────────────────────────────────
#>  (Intercept)          │    0.01  [ 0.00,  0.01]  <.001     0.01  [ 0.00,  0.02] 
#>  agecat:              │                                                         
#>    (0,19] (ref.)      │     –          –          –         –          –        
#>    (19,39]            │    9.78  [ 4.76, 20.07]  <.001     9.77  [ 4.66, 20.48] 
#>    (39,59]            │   24.83  [11.39, 54.12]  <.001    24.84  [11.10, 55.56] 
#>    (59,Inf]           │   20.82  [ 9.71, 44.63]  <.001    20.70  [ 9.36, 45.74] 
#>  sex:                 │                                                         
#>    Male (ref.)        │     –          –          –         –          –        
#>    Female             │    1.23  [ 1.02,  1.48]   .035     1.24  [ 1.02,  1.50] 
#>  race:                │                                                         
#>    Hispanic (ref.)    │                                     –          –        
#>    White              │                                    0.92  [ 0.77,  1.10] 
#>    Black              │                                    0.65  [ 0.46,  0.91] 
#>    Other              │                                    0.86  [ 0.40,  1.85] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                    │ 7846                            7846                    
#>  AIC                  │ 5162.1                          5166.0                  
#>  Effective parameters │    6.60                           12.90                 
#> 
#>                      + race 
#>                      ────── 
#>  Variable          │ p (OR) 
#> ───────────────────┼────────
#>  (Intercept)       │  <.001 
#>  agecat:           │        
#>    (0,19] (ref.)   │   –    
#>    (19,39]         │  <.001 
#>    (39,59]         │  <.001 
#>    (59,Inf]        │  <.001 
#>  sex:              │        
#>    Male (ref.)     │   –    
#>    Female          │   .033 
#>  race:             │        
#>    Hispanic (ref.) │   –    
#>    White           │   .316 
#>    Black           │   .019 
#>    Other           │   .674 
#> 
#> Note. Survey-weighted logistic regression models. Design: stratified (SDMVSTRA), cluster (SDMVPSU), 31 PSUs; residual degrees of freedom: 12 (Age, sex), 9 (+ race). AIC of a design-based fit: design-based AIC (dAIC). Coefficients exponentiated and displayed as OR; CI bounds exponentiated. The (Intercept) row estimates the baseline odds. Std. errors: design-based (Taylor linearization). CI = confidence interval; AIC = Akaike information criterion.
```

The `AIC` row of a
[`svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) fit is not the
AIC of a [`glm()`](https://rdrr.io/r/stats/glm.html). survey defines it
“using the Rao-Scott approximation to the weighted loglikelihood (Lumley
and Scott, 2015). It replaces the usual penalty term p \[…\] by the
trace of the generalised design effect matrix”
([`?anova.svyglm`](https://rdrr.io/pkg/survey/man/anova.svyglm.html)).
`show_fit_stats = "eff_p"` displays that penalty as
`Effective parameters`. Here the model with race has seven coefficients
beyond the intercept and 12.9 effective parameters: under this design, a
coefficient costs more than one unit.

The criterion is lower without race, 5162.1 against 5166.0, which is the
reading of the two tests of the previous section. The design-based AIC
compares design-based fits of the same data. It should not be set
against the AIC of a [`glm()`](https://rdrr.io/r/stats/glm.html) shown
in another column.

survey also compares the two fits directly, with the same Rao-Scott test
as above:

``` r

anova(fit0, fit)
#> Working (Rao-Scott+F) LRT for race
#>  in svyglm(formula = HI_CHOL ~ agecat + race + sex, design = dnh, 
#>     family = quasibinomial)
#> Working 2logLR =  4.144661 p= 0.27411 
#> (scale factors:  2.3 0.43 0.25 );  denominator df= 9
```

`nested = TRUE`, which adds the change statistics of a hierarchy to the
table, is refused for a design-based fit:

``` r

table_regression(list(fit0, fit), nested = TRUE)
#> Error in `table_regression()`:
#> ! `nested = TRUE` is not available for `svyglm`.
#> ℹ The change statistics compare each model with the one before it, and this class has no test of two nested fits here. One model that cannot be compared removes the comparison for the whole table.
#> ℹ Render the models side by side with `nested = FALSE`.
#> ℹ For a design-based fit, test a term with `survey::regTermTest(fit, ~term)`. Two nested `svyglm()` or `svycoxph()` fits are compared by `anova(fit0, fit)`.
```

## Subpopulations

An analysis of a subgroup is a domain analysis, and the subgroup is
taken on the design, not on the data. “Estimates for subpopulations or
domains are handled by setting to zero the observed values for all units
that fall outside of the domain” (Binder, 2008, p. 192):
[`subset()`](https://rdrr.io/r/base/subset.html) on a design does this,
“keeping the original design information about number of clusters,
strata”
([`?subset.survey.design`](https://rdrr.io/pkg/survey/man/subset.survey.design.html)).

``` r

older <- subset(dnh, agecat == "(59,Inf]")
fit_old <- svyglm(HI_CHOL ~ race + sex, design = older, family = quasibinomial)
table_regression(fit_old, exponentiate = TRUE)
#> Survey-weighted logistic regression: HI_CHOL
#> 
#>  Variable          │     OR        SE      95% CI       p   
#> ───────────────────┼────────────────────────────────────────
#>  (Intercept)       │        0.12  0.02  [0.08, 0.18]  <.001 
#>  race:             │                                        
#>    Hispanic (ref.) │         –     –         –         –    
#>    White           │        0.95  0.14  [0.69, 1.32]   .760 
#>    Black           │        0.73  0.21  [0.39, 1.38]   .309 
#>    Other           │        0.95  0.38  [0.40, 2.25]   .905 
#>  sex:              │                                        
#>    Male (ref.)     │         –     –         –         –    
#>    Female          │        2.31  0.42  [1.56, 3.42]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                 │     1880                               
#>  Weighted n        │ 51225892                               
#>  AIC               │     1609.5                             
#> 
#> Note. Survey-weighted logistic regression. Design: stratified (SDMVSTRA), cluster (SDMVPSU), 31 PSUs; 12 residual degrees of freedom. AIC of a design-based fit: design-based AIC (dAIC). Coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric). The (Intercept) row estimates the baseline odds. Std. errors: design-based (Taylor linearization). CI = confidence interval; AIC = Akaike information criterion.
```

Among respondents of 60 and over, the odds ratio of women is 2.31, with
an interval from 1.56 to 3.42, against 1.24 in the whole sample. The `n`
row counts the 1,880 respondents of the domain, and the note counts the
PSUs that hold one of them, here all 31.

Subsetting the data first and declaring the design afterwards breaks the
design. For the smallest group, the design is declared without complaint
and the model stops:

``` r

other_data <- subset(nhanes, race == "Other")
d_other <- svydesign(
  id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR,
  nest = TRUE, data = other_data
)
svyglm(HI_CHOL ~ sex, design = d_other, family = quasibinomial)
#> Error in `onestrat()`:
#> ! Stratum (75) has only one PSU at stage 1
```

One stratum has respondents of this group in a single PSU, and a
variance cannot be computed from one unit. That stratum has two PSUs in
the survey. Taken on the design, the same subgroup gives a model:

``` r

fit_other <- svyglm(
  HI_CHOL ~ sex,
  design = subset(dnh, race == "Other"),
  family = quasibinomial
)
table_regression(fit_other, exponentiate = TRUE, show_columns = c("b", "ci", "p"))
#> Survey-weighted logistic regression: HI_CHOL
#> 
#>  Variable      │     OR          95% CI       p   
#> ───────────────┼──────────────────────────────────
#>  (Intercept)   │        0.13  [0.06, 0.26]  <.001 
#>  sex:          │                                  
#>    Male (ref.) │         –         –         –    
#>    Female      │        0.75  [0.31, 1.83]   .504 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │      458                         
#>  Weighted n    │ 18199566                         
#>  AIC           │      301.2                       
#> 
#> Note. Survey-weighted logistic regression. Design: stratified (SDMVSTRA), cluster (SDMVPSU), 29 PSUs; 13 residual degrees of freedom. AIC of a design-based fit: design-based AIC (dAIC). Coefficients exponentiated and displayed as OR; CI bounds exponentiated. The (Intercept) row estimates the baseline odds. Std. errors: design-based (Taylor linearization). CI = confidence interval; AIC = Akaike information criterion.
```

The note counts 29 PSUs, those that hold a respondent of this group with
a cholesterol measure. In 15 strata they leave 14 degrees of freedom for
the design, and 13 for the tests of a model with one predictor.

The error is not always that visible. For the high schools of the
cluster sample, present in 8 of the 15 districts, both routes give a
model, and their standard errors differ by 4%:

``` r

high_design <- svyglm(api00 ~ ell + meals, design = subset(dclus1, stype == "H"))
high_data <- svyglm(
  api00 ~ ell + meals,
  design = svydesign(
    id = ~dnum, weights = ~pw, fpc = ~fpc,
    data = subset(apiclus1, stype == "H")
  )
)
table_regression(
  list("subset(design)" = high_design, "subset(data)" = high_data),
  show_columns = c("b", "se", "p")
)
#> Survey-weighted linear regression comparison: api00
#> 
#>                   subset(design)         subset(data)     
#>                ────────────────────  ──────────────────── 
#>  Variable    │   B      SE      p      B      SE      p   
#> ─────────────┼────────────────────────────────────────────
#>  (Intercept) │ 733.33  32.75  <.001  733.33  33.98  <.001 
#>  ell         │  -4.32   1.69   .051   -4.32   1.75   .057 
#>  meals       │  -1.33   1.12   .289   -1.33   1.17   .305 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │  14                    14                  
#>  Weighted n  │ 474                   474                  
#>  AIC         │ 153.1                 153.5                
#> 
#> Note. Survey-weighted linear regression models. Design: cluster (dnum), 8 PSUs, with finite population correction; 5 residual degrees of freedom. AIC of a design-based fit: design-based AIC (dAIC). Std. errors: design-based (Taylor linearization). AIC = Akaike information criterion.
```

The size of the error depends on the design, and nothing in the output
of the wrong route warns of it. Subsetting the design costs nothing and
is right in every case.

## Replicate weights

A design can be declared by replicate weights in place of strata and
clusters. “Some recent large-scale surveys specify replication weights
rather than the sampling design (partly for privacy reasons)”
([`?svrepdesign`](https://rdrr.io/pkg/survey/man/svrepdesign.html)). The
functions are the same, and so are the coefficients, which depend on the
weights alone. The variance is computed from the estimates obtained with
each set of replicate weights.

``` r

rclus1 <- as.svrepdesign(dclus1)
gl_rep <- svyglm(api00 ~ ell + meals + mobility, design = rclus1)

table_regression(
  list("Linearization" = gl, "Jackknife (JK1)" = gl_rep),
  show_columns = c("b", "se", "ci")
)
#> Survey-weighted linear regression comparison: api00
#> 
#>                         Linearization            Jackknife (JK1) 
#>                ────────────────────────────────  ─────────────── 
#>  Variable    │    B      SE         95% CI           B      SE   
#> ─────────────┼───────────────────────────────────────────────────
#>  (Intercept) │  819.28  21.39  [772.20, 866.36]    819.28  22.98 
#>  ell         │   -0.52   0.32  [ -1.23,   0.20]     -0.52   0.35 
#>  meals       │   -3.12   0.28  [ -3.74,  -2.51]     -3.12   0.30 
#>  mobility    │   -0.17   0.44  [ -1.15,   0.81]     -0.17   0.54 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │  183                                183           
#>  Weighted n  │ 6194                               6194           
#>  AIC         │ 2004.7                             2007.1         
#> 
#>                Jackknife (JK1)  
#>                ──────────────── 
#>  Variable    │    95% CI (B)    
#> ─────────────┼──────────────────
#>  (Intercept) │ [768.70, 869.85] 
#>  ell         │ [ -1.29,   0.26] 
#>  meals       │ [ -3.78,  -2.47] 
#>  mobility    │ [ -1.35,   1.02] 
#> 
#> Note. Survey-weighted linear regression models. Linearization: Design: cluster (dnum), 15 PSUs, with finite population correction; 11 residual degrees of freedom. Jackknife (JK1): Design: replicate weights (JK1), 15 replicates; 11 residual degrees of freedom. AIC of a design-based fit: design-based AIC (dAIC). Std. errors: Linearization: design-based (Taylor linearization); Jackknife (JK1): design-based (replicate weights, JK1). CI = confidence interval; AIC = Akaike information criterion.
```

The note names the design of each model: the PSUs of the linearization,
the scheme and the number of replicates of the jackknife. The jackknife
and the linearization estimators “are approximately the same for point
estimates for which they are valid on large samples” (Lee, 2008,
p. 404). With 15 PSUs the sample of PSUs is not large, and the two
differ by 7% for the intercept and by 21% for `mobility`.

## Ordinal outcomes: `svyolr()`

[`svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) fits a
cumulative logit model under the design. The API score cut into three
bands, on the stratified sample:

``` r

apistrat$grade <- cut(
  apistrat$api00,
  c(0, 600, 700, 1000),
  labels = c("low", "mid", "high"),
  ordered_result = TRUE
)
dgrade <- svydesign(
  id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc
)
ol <- svyolr(grade ~ ell + meals + stype, design = dgrade)
table_regression(ol, exponentiate = TRUE)
#> Survey-weighted cumulative logit regression (proportional odds): grade
#> 
#>  Variable     │   OR      SE       95% CI         p   
#> ──────────────┼───────────────────────────────────────
#>  ell          │    0.97  0.02  [  0.94,  1.01]   .173 
#>  meals        │    0.90  0.02  [  0.87,  0.93]  <.001 
#>  stype:       │                                       
#>    E (ref.)   │     –     –           –          –    
#>    H          │    0.02  0.01  [  0.01,  0.05]  <.001 
#>    M          │    0.11  0.05  [  0.05,  0.27]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:  │                                       
#>    low | mid  │   -8.33  0.92  [-10.15, -6.51]  <.001 
#>    mid | high │   -5.27  0.69  [ -6.63, -3.91]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n            │  200                                  
#>  Weighted n   │ 6194                                  
#> 
#> Note. Survey-weighted cumulative logit regression (proportional odds). Design: stratified (stype), with finite population correction; 193 residual degrees of freedom. Coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric). Std. errors: design-based (Taylor linearization). Thresholds: latent-scale category cut-points (log-odds scale, not exponentiated). CI = confidence interval.
```

Under proportional odds one ratio applies to every cut of the scale:
each additional percentage point of pupils eligible for subsidized meals
multiplies the odds of a higher band by 0.90. The cut-points are in a
`Thresholds` block, on the log-odds scale and never exponentiated, which
the note states. There is no `AIC` row: survey has no such criterion for
a [`svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) fit.

The average marginal effects of an ordinal model come one column per
category of the outcome, and those of a predictor sum to zero across the
categories:

``` r

table_regression(ol, show_columns = c("b", "ame"))
#> Survey-weighted cumulative logit regression (proportional odds): grade
#> 
#>  Variable     │    B     AME low  AME mid  AME high 
#> ──────────────┼─────────────────────────────────────
#>  ell          │   -0.03     0.00     0.00     -0.00 
#>  meals        │   -0.10     0.01     0.00     -0.01 
#>  stype:       │                                     
#>    E (ref.)   │     –        –        –         –   
#>    H          │   -4.12     0.33     0.03     -0.36 
#>    M          │   -2.16     0.16     0.04     -0.20 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:  │                                     
#>    low | mid  │   -8.33                             
#>    mid | high │   -5.27                             
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n            │  200                                
#>  Weighted n   │ 6194                                
#> 
#> Note. Survey-weighted cumulative logit regression (proportional odds). Design: stratified (stype), with finite population correction; 193 residual degrees of freedom. Std. errors: design-based (Taylor linearization). AME of a continuous predictor: average slope (dY/dX). Thresholds: latent-scale category cut-points. AME = average marginal effect on a response-category probability.
```

## Cox models: `svycoxph()`

The example of
[`?svycoxph`](https://rdrr.io/pkg/survey/man/svycoxph.html), which the
help page itself introduces as a “somewhat unrealistic example of
nonresponse bias”: the patients of the Mayo Clinic data on primary
biliary cholangitis who were randomized, weighted by the inverse of an
estimated probability of being randomized.

``` r

data(pbc, package = "survival")
pbc$randomized <- with(pbc, !is.na(trt) & trt > 0)
bias <- glm(randomized ~ age * edema, data = pbc, family = binomial)
pbc$randprob <- fitted(bias)

dpbc <- svydesign(
  id = ~1, prob = ~randprob, strata = ~edema,
  data = subset(pbc, randomized)
)
cx <- svycoxph(
  survival::Surv(time, status > 0) ~ log(bili) + protime + albumin,
  design = dpbc
)
table_regression(
  cx,
  exponentiate = TRUE,
  show_fit_stats = c("nobs", "weighted_nobs", "n_events", "concordance")
)
#> Survey-weighted Cox proportional hazards regression: Surv(time, status > 0)
#> 
#>  Variable    │   HR     SE      95% CI       p   
#> ─────────────┼───────────────────────────────────
#>  log(bili)   │   2.43  0.22  [2.03, 2.90]  <.001 
#>  protime     │   1.28  0.10  [1.09, 1.50]   .003 
#>  albumin     │   0.35  0.07  [0.24, 0.53]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 312                               
#>  Weighted n  │ 418                               
#>  N events    │ 144                               
#>  Concordance │   0.81                            
#> 
#> Note. Survey-weighted Cox proportional hazards regression. Design: stratified (edema); 307 residual degrees of freedom. Coefficients exponentiated and displayed as HR; SE on the HR scale (delta method); CI bounds exponentiated (asymmetric). Std. errors: design-based (Taylor linearization). HR = hazard ratio; CI = confidence interval.
```

`prob` takes the probability, and survey inverts it: the weighted n is
the 418 patients of the cohort, for 312 in the analysis. The standard
errors are the design-based ones of survey, and the tests refer to a t
distribution with the residual degrees of freedom of the note. A list of
tokens in `show_fit_stats` replaces the default rows, so
`"weighted_nobs"` has to be named to stay in the table.

## What spicy refuses for survey fits, and why

A request that the design cannot honor gets an error that says why.

- `vcov =`. The variance of the design is the robust variance for the
  declared design. To change the estimator, change the design and refit.
  A `cluster =` is ignored with a warning that says the same.
- Statistics of a likelihood: `"bic"`, `"aicc"`, `"deviance"`,
  `"log_lik"` and the pseudo-R² tokens. The exception is `"aic"` for
  [`svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html), the
  design-based criterion of the previous sections.
- The AME columns of a
  [`svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html) model, as
  for any Cox model.
- `nested = TRUE` and the term-level test columns. The tests exist in
  survey,
  [`regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html) and
  [`anova()`](https://rdrr.io/r/stats/anova.html), and their result can
  be reported with `note_add`.
- `standardized`. Average marginal effects are the way to compare
  predictors on a common scale here.
- `ci_method = "profile"`, which needs a likelihood to profile.
- The RMST and risk-difference columns of a Cox model. Their uncertainty
  comes from resampling subjects, which ignores the strata and the
  clusters of the design.
  [`survey::svykm()`](https://rdrr.io/pkg/survey/man/svykm.html) gives a
  marginal survival curve under the design.
- [`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)
  on a design object. Fit each model with
  [`svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) and pass the
  fits to
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

``` r

table_regression(fit, vcov = "HC3")
#> Error in `table_regression()`:
#> ! `vcov = "HC3"` is not available for `svyglm` models.
#> ℹ The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
#> ℹ To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
```

The refusal of the RMST and risk-difference columns rests on a
measurement. On simulated data, over five designs and three seeds, a
bootstrap of the rows gave a contrast between clusters a standard error
too small by a factor of 1.5 to 6, depending on the number and the size
of the clusters (about 3 for 30 clusters of 25 units with a moderate
intra-cluster correlation). A contrast within clusters came out about
right, and so did a design without intra-cluster correlation. The ratios
are the same for the Cox coefficients themselves: this is the ordinary
design effect, carried onto the RMST and the risk difference.

## Presentation-ready output

Everything above renders through the rich output engines. The comparison
of the two NHANES models as a `gt` table:

``` r

pkgdown_dark_gt(
  table_regression(
    list("Age, sex" = fit0, "+ race" = fit),
    exponentiate = TRUE,
    show_columns = c("b", "ci", "p"),
    output = "gt"
  )
)
```

[TABLE]

## References

- Binder, D. A. (1992). Fitting Cox’s proportional hazards models from
  survey data. *Biometrika*, 79(1), 139–147.
- Binder, D. A. (2008). Design-based estimation. In P. J. Lavrakas
  (Ed.), *Encyclopedia of survey research methods* (pp. 191–193). SAGE
  Publications.
- DuMouchel, W. H., & Duncan, G. J. (1983). Using sample survey weights
  in multiple regression analyses of stratified samples. *Journal of the
  American Statistical Association*, 78(383), 535–543.
- Lavrakas, P. J. (Ed.). (2008). *Encyclopedia of survey research
  methods*. SAGE Publications. (Entry “Sampling error”, p. 787.)
- Lee, H. (2008). Jackknife variance estimation. In P. J. Lavrakas
  (Ed.), *Encyclopedia of survey research methods* (p. 404). SAGE
  Publications.
- Lumley, T. (2004). Analysis of complex survey samples. *Journal of
  Statistical Software*, 9(8), 1–19.
- Lumley, T., & Scott, A. (2015). AIC and BIC for modeling with complex
  survey data. *Journal of Survey Statistics and Methodology*, 3(1),
  1–18.
- Williams, R. L. (2008). Taylor series linearization (TSL). In P. J.
  Lavrakas (Ed.), *Encyclopedia of survey research methods*
  (pp. 876–877). SAGE Publications.
