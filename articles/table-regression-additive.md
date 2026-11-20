# Additive, beta and selection regression tables

``` r

library(spicy)
library(mgcv)            # gam(), bam()
library(betareg)         # betareg()
library(sampleSelection) # selection(), heckit(), treatReg()
```

This article covers three kinds of models that estimate something beside
their regression coefficients: the smooth terms of a generalized
additive model, the precision submodel of a beta regression, and the
error terms of a sample-selection model.
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
shows each of them in a labeled block of rows under the coefficients.
The companion article [*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
covers the shared mechanics (output engines, labels, `tidy()`), and the
class-by-class map is [*Supported
models*](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.md).

## Generalized additive models

A generalized additive model replaces the straight line of a predictor
by a curve estimated from the data. The example is the one of
[*Regression tables for rms
fits*](https://amaltawfik.github.io/spicy/articles/table-regression-rms.md):
diabetes among 532 women of Pima heritage aged 21 or more
([`MASS::Pima.tr`](https://rdrr.io/pkg/MASS/man/Pima.tr.html) and
`Pima.te`), and the shape of its relation with age. The first model is
linear in age. The second gives age a smooth term.

``` r

pima <- rbind(MASS::Pima.tr, MASS::Pima.te)
fit_glm <- glm(type ~ age + bmi + glu + npreg, family = binomial,
               data = pima)
fit_gam <- gam(type ~ s(age) + bmi + glu + npreg, family = binomial,
               data = pima, method = "REML")
table_regression(list(Linear = fit_glm, GAM = fit_gam))
#> Regression comparison: type
#> 
#>                              Linear                 GAM         
#>                        ───────────────────  ─────────────────── 
#>  Variable            │   B      SE     p      B      SE     p   
#> ─────────────────────┼──────────────────────────────────────────
#>  (Intercept)         │  -9.34  0.87  <.001   -8.13  0.84  <.001 
#>  age                 │   0.03  0.01   .044                      
#>  bmi                 │   0.09  0.02  <.001    0.08  0.02  <.001 
#>  glu                 │   0.04  0.00  <.001    0.04  0.00  <.001 
#>  npreg               │   0.11  0.04   .008    0.05  0.05   .244 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Smooth terms (edf): │                                          
#>    s(age)            │                        3.63   –     .002 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                   │ 532                  532                 
#>  R² (Tjur)           │   0.34                 0.36              
#>  R² (McFadden)       │   0.29                 0.32              
#>  AIC                 │ 491.1                479.2               
#> 
#> Note. Linear: logistic regression; GAM: logistic GAM. Std. errors: Linear: classical (Fisher information), GAM: Bayesian (posterior covariance). Smooth terms: effective degrees of freedom (edf) and approximate test of each term. AIC = Akaike information criterion.
summary(fit_gam)$s.table
#>             edf   Ref.df   Chi.sq     p-value
#> s(age) 3.630771 4.561711 19.24352 0.001747507
```

A curve has no single coefficient, so `s(age)` has no row among the
coefficients. It has a row in the `Smooth terms (edf)` block, and what
stands in the `B` column there is the number of effective degrees of
freedom of the term. One degree of freedom is a straight line. The curve
of age takes 3.63, the value of
[`summary()`](https://rdrr.io/r/base/summary.html).

The p-value of that row belongs to the test that the term is zero. mgcv
computes it, and its help says what it is worth: “Be warned, these are
only approximate”
([`?summary.gam`](https://rdrr.io/pkg/mgcv/man/summary.gam.html)). The
standard error and the interval have no meaning for a number of degrees
of freedom and stay empty.

The standard errors of the two columns do not come from the same place,
and the note says so. Those of the GAM are taken from the Bayesian
posterior covariance matrix of mgcv, as in its
[`summary()`](https://rdrr.io/r/base/summary.html).

### Is the effect of age linear?

The linear model is the additive model whose curve is a line. The two
can be compared, and `nested = TRUE` adds the test to the table.

``` r

table_regression(list(Linear = fit_glm, GAM = fit_gam), nested = TRUE)
#> Hierarchical regression: type
#> 
#>                              Linear                    GAM            
#>                        ───────────────────  ───────────────────────── 
#>  Variable            │   B      SE     p         B         SE     p   
#> ─────────────────────┼────────────────────────────────────────────────
#>  (Intercept)         │  -9.34  0.87  <.001   -8.13        0.84  <.001 
#>  age                 │   0.03  0.01   .044                            
#>  bmi                 │   0.09  0.02  <.001    0.08        0.02  <.001 
#>  glu                 │   0.04  0.00  <.001    0.04        0.00  <.001 
#>  npreg               │   0.11  0.04   .008    0.05        0.05   .244 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Smooth terms (edf): │                                                
#>    s(age)            │                        3.63         –     .002 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                   │ 532                  532                       
#>  R² (Tjur)           │   0.34                 0.36                    
#>  R² (McFadden)       │   0.29                 0.32                    
#>  AIC                 │ 491.1                479.2                     
#>  Δχ²                 │    –                  19.01 (2.6)              
#>  p (change)          │    –                   <.001                   
#> 
#> Note. Linear: logistic regression; GAM: logistic GAM. Std. errors: Linear: classical (Fisher information), GAM: Bayesian (posterior covariance). Change test of an additive model: approximate, on the difference in effective degrees of freedom. Smooth terms: effective degrees of freedom (edf) and approximate test of each term. AIC = Akaike information criterion.
anova(fit_glm, fit_gam, test = "Chisq")
#> Analysis of Deviance Table
#> 
#> Model 1: type ~ age + bmi + glu + npreg
#> Model 2: type ~ s(age) + bmi + glu + npreg
#>   Resid. Df Resid. Dev     Df Deviance  Pr(>Chi)    
#> 1    527.00     481.12                              
#> 2    524.37     462.11 2.6308   19.006 0.0001749 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The deviance falls by 19.01 for 2.6 more effective degrees of freedom, p
\< .001: the relation with age is not a line. The numbers are those of
[`anova()`](https://rdrr.io/r/stats/anova.html). The degrees of freedom
are not whole, since the test takes the difference in effective degrees
of freedom of the two models, and the note recalls that the test is
approximate. The help of mgcv says that these p-values “must be used
with care”
([`?anova.gam`](https://rdrr.io/pkg/mgcv/man/anova.gam.html)).

### Several smooth terms

A model can be made of smooth terms only. Each has its row.

``` r

fit_gam3 <- gam(type ~ s(age) + s(glu) + te(bmi, ped), family = binomial,
                data = pima, method = "REML")
table_regression(fit_gam3)
#> Logistic GAM: type
#> 
#>  Variable            │   B      SE       95% CI        p   
#> ─────────────────────┼─────────────────────────────────────
#>  (Intercept)         │  -1.09  0.14  [-1.36, -0.83]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Smooth terms (edf): │                                     
#>    s(age)            │   3.54   –          –         <.001 
#>    s(glu)            │   1.00   –          –         <.001 
#>    te(bmi,ped)       │   8.21   –          –         <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                   │ 532                                 
#>  R² (Tjur)           │   0.40                              
#>  R² (McFadden)       │   0.36                              
#>  AIC                 │ 465.7                               
#> 
#> Note. Logistic GAM. Std. errors: Bayesian (posterior covariance). Smooth terms: effective degrees of freedom (edf) and approximate test of each term. CI = confidence interval; AIC = Akaike information criterion.
summary(fit_gam3)$s.table
#>                  edf    Ref.df   Chi.sq      p-value
#> s(age)      3.544690  4.444677 37.53214 5.616457e-07
#> s(glu)      1.000150  1.000299 64.78000 0.000000e+00
#> te(bmi,ped) 8.209722 10.422729 41.38410 1.621210e-05
```

The smooth of glucose has 1.00 effective degree of freedom: the penalty
has brought it back to a straight line. `te(bmi, ped)` is a surface in
two predictors, with 8.21.

### Selecting and labeling rows

The smooth terms are predictors, and `keep`, `drop` and `labels` treat
them as such, under the name mgcv gives them.

``` r

table_regression(fit_gam, drop = "^s\\(")
#> Logistic GAM: type
#> 
#>  Variable      │   B      SE       95% CI        p   
#> ───────────────┼─────────────────────────────────────
#>  (Intercept)   │  -8.13  0.84  [-9.77, -6.49]  <.001 
#>  bmi           │   0.08  0.02  [ 0.04,  0.11]  <.001 
#>  glu           │   0.04  0.00  [ 0.03,  0.04]  <.001 
#>  npreg         │   0.05  0.05  [-0.04,  0.14]   .244 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │ 532                                 
#>  R² (Tjur)     │   0.36                              
#>  R² (McFadden) │   0.32                              
#>  AIC           │ 479.2                               
#> 
#> Note. Logistic GAM. Std. errors: Bayesian (posterior covariance). CI = confidence interval; AIC = Akaike information criterion.
table_regression(fit_gam, keep = "s(age)")
#> Logistic GAM: type
#> 
#>  Variable            │   B      SE       95% CI        p   
#> ─────────────────────┼─────────────────────────────────────
#>  (Intercept)         │  -8.13  0.84  [-9.77, -6.49]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Smooth terms (edf): │                                     
#>    s(age)            │   3.63   –          –          .002 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                   │ 532                                 
#>  R² (Tjur)           │   0.36                              
#>  R² (McFadden)       │   0.32                              
#>  AIC                 │ 479.2                               
#> 
#> Note. Logistic GAM. Std. errors: Bayesian (posterior covariance). Smooth terms: effective degrees of freedom (edf) and approximate test of each term. CI = confidence interval; AIC = Akaike information criterion.
table_regression(fit_gam, labels = c("s(age)" = "Age (smooth)"))
#> Logistic GAM: type
#> 
#>  Variable            │   B      SE       95% CI        p   
#> ─────────────────────┼─────────────────────────────────────
#>  (Intercept)         │  -8.13  0.84  [-9.77, -6.49]  <.001 
#>  bmi                 │   0.08  0.02  [ 0.04,  0.11]  <.001 
#>  glu                 │   0.04  0.00  [ 0.03,  0.04]  <.001 
#>  npreg               │   0.05  0.05  [-0.04,  0.14]   .244 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Smooth terms (edf): │                                     
#>    Age (smooth)      │   3.63   –          –          .002 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                   │ 532                                 
#>  R² (Tjur)           │   0.36                              
#>  R² (McFadden)       │   0.32                              
#>  AIC                 │ 479.2                               
#> 
#> Note. Logistic GAM. Std. errors: Bayesian (posterior covariance). Smooth terms: effective degrees of freedom (edf) and approximate test of each term. CI = confidence interval; AIC = Akaike information criterion.
```

`drop` and `keep` take regular expressions, and also the exact name of a
term, written as it is: `"s(age)"` needs no escaping. When no smooth row
is left, the block and its sentence in the note are left out.

### Cluster-robust variance

`vcov = "CR1"` with a `cluster` gives the parametric rows a sandwich
variance. These data have no cluster, and the number of pregnancies
stands in for one here, only to show what the table does.

``` r

table_regression(fit_gam, vcov = "CR1", cluster = ~npreg)
#> Logistic GAM: type
#> 
#>  Variable            │   B      SE       95% CI        p   
#> ─────────────────────┼─────────────────────────────────────
#>  (Intercept)         │  -8.13  0.78  [-9.65, -6.61]  <.001 
#>  bmi                 │   0.08  0.02  [ 0.04,  0.12]  <.001 
#>  glu                 │   0.04  0.00  [ 0.03,  0.04]  <.001 
#>  npreg               │   0.05  0.05  [-0.04,  0.14]   .240 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Smooth terms (edf): │                                     
#>    s(age)            │   3.63   –          –          –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                   │ 532                                 
#>  R² (Tjur)           │   0.36                              
#>  R² (McFadden)       │   0.32                              
#>  AIC                 │ 479.2                               
#> 
#> Note. Logistic GAM. Std. errors: cluster-robust (CR1), clusters by npreg. Smooth terms: effective degrees of freedom (edf). The test of the smooth terms is not computed under a robust variance. CI = confidence interval; AIC = Akaike information criterion.
```

The test of a smooth term rests on the covariance matrix of mgcv. It is
not computed under a robust variance: the row keeps its degrees of
freedom, and its p-value is empty.

### Average marginal effects

The table has given age a shape and a test, and no direction yet. The
average marginal effect does: it is the slope of the predicted
probability with respect to age, averaged over the sample.

``` r

table_regression(fit_gam, show_columns = c("b", "p", "ame", "ame_p"))
#> Logistic GAM: type
#> 
#>  Variable            │   B       p    AME     p   
#> ─────────────────────┼────────────────────────────
#>  (Intercept)         │  -8.13  <.001              
#>  bmi                 │   0.08  <.001  0.01  <.001 
#>  glu                 │   0.04  <.001  0.01  <.001 
#>  npreg               │   0.05   .244  0.01   .242 
#>  age                 │                0.01  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Smooth terms (edf): │                            
#>    s(age)            │   3.63   .002              
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                   │ 532                        
#>  R² (Tjur)           │   0.36                     
#>  R² (McFadden)       │   0.32                     
#>  AIC                 │ 479.2                      
#> 
#> Note. Logistic GAM. Std. errors: Bayesian (posterior covariance). AME of a continuous predictor: average slope (dY/dX). Smooth terms: effective degrees of freedom (edf) and approximate test of each term. AME = average marginal effect; AIC = Akaike information criterion.
```

One more year of age goes with a probability of diabetes higher by about
one percentage point on average. The row of age comes after the
parametric terms, with an empty `B`, since the curve has no coefficient.
The p-value that follows the `AME` column is that of the AME.

### The long format

`tidy()` returns the smooth rows with `estimate_type = "edf"`. The
reference degrees of freedom are in `df`, the test in `statistic` and
`p.value`.

``` r

td <- broom::tidy(table_regression(fit_gam))
td[, c("term", "estimate_type", "estimate", "df", "statistic", "p.value")]
#> # A tibble: 5 × 6
#>   term        estimate_type estimate     df statistic  p.value
#>   <chr>       <chr>            <dbl>  <dbl>     <dbl>    <dbl>
#> 1 (Intercept) B              -8.13   Inf        -9.71 2.85e-22
#> 2 bmi         B               0.0796 Inf         4.41 1.05e- 5
#> 3 glu         B               0.0354 Inf         8.51 1.79e-17
#> 4 npreg       B               0.0532 Inf         1.16 2.44e- 1
#> 5 s(age)      edf             3.63     4.56     19.2  1.75e- 3
```

### Large data: `bam()`

[`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) fits the same models on
large data. The example takes the 28,155 men of
[`AER::CPS1988`](https://rdrr.io/pkg/AER/man/CPS1988.html) and the
logarithm of their wage, with a smooth of experience.

``` r

data("CPS1988", package = "AER")
fit_bam <- bam(log(wage) ~ s(experience) + education + ethnicity + smsa +
                 region + parttime, data = CPS1988)
table_regression(fit_bam)
#> Generalized additive model (GAM): log(wage)
#> 
#>  Variable            │    B       SE       95% CI        p   
#> ─────────────────────┼───────────────────────────────────────
#>  (Intercept)         │     5.07  0.02  [ 5.04,  5.11]  <.001 
#>  education           │     0.09  0.00  [ 0.08,  0.09]  <.001 
#>  ethnicity:          │                                       
#>    cauc (ref.)       │      –     –          –          –    
#>    afam              │    -0.23  0.01  [-0.25, -0.21]  <.001 
#>  smsa:               │                                       
#>    no (ref.)         │      –     –          –          –    
#>    yes               │     0.16  0.01  [ 0.15,  0.18]  <.001 
#>  region:             │                                       
#>    northeast (ref.)  │      –     –          –          –    
#>    midwest           │    -0.05  0.01  [-0.07, -0.03]  <.001 
#>    south             │    -0.10  0.01  [-0.12, -0.08]  <.001 
#>    west              │    -0.05  0.01  [-0.06, -0.03]  <.001 
#>  parttime:           │                                       
#>    no (ref.)         │      –     –          –          –    
#>    yes               │    -0.84  0.01  [-0.87, -0.82]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Smooth terms (edf): │                                       
#>    s(experience)     │     8.19   –          –         <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                   │ 28155                                 
#>  R² (deviance)       │     0.46                              
#>  AIC                 │ 43582.2                               
#> 
#> Note. Generalized additive model (GAM). Std. errors: Bayesian (posterior covariance). Smooth terms: effective degrees of freedom (edf) and approximate test of each term. CI = confidence interval; AIC = Akaike information criterion.
summary(fit_bam)$s.table
#>                    edf   Ref.df        F p-value
#> s(experience) 8.188189 8.693827 837.2108       0
```

The table reads like that of
[`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html). The title keeps the
outcome as the formula writes it, `log(wage)`. For a Gaussian model the
test of a smooth term is an F test, and the fit statistic is the R² of
the deviance.

### What is refused

[`gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html) estimates an additive
model as a mixed model and returns two parts, `lme` and `gam`. Neither
is read, and the error says why.

``` r

fit_gamm <- gamm(glu ~ s(age) + bmi, data = pima)
table_regression(fit_gamm$gam)
#> Error in `table_regression()`:
#> ! The `gam` part of a `mgcv::gamm()` fit is not supported by `table_regression()`.
#> ℹ Its standard errors come from the mixed model of the `lme` part, and its likelihood and information criteria are not those of the model.
#> ℹ Fit the model with `mgcv::gam()`, a random intercept written as `s(g, bs = "re")`, which is supported, or report `summary(fit$gam)`.
```

## Beta regression

A beta regression is a model for a proportion that stays strictly
between 0 and 1. The example is the one of Ferrari and Cribari-Neto
(2004): the share of income that 38 households spend on food
([`betareg::FoodExpenditure`](https://rdrr.io/pkg/betareg/man/FoodExpenditure.html)),
by income and by number of persons in the household.

The model has two parameters. The mean of the share depends on the
predictors through a link, the logit by default. The precision φ says
how tightly the shares gather around that mean: for a given mean, a
larger φ is a smaller variance. The first model below holds the
precision constant. The second lets it depend on the size of the
household, with the formula `y ~ x | z`. The coefficient of income is
small, and the tables of this section show three decimals.

``` r

data("FoodExpenditure", package = "betareg")
FoodExpenditure$share <- FoodExpenditure$food / FoodExpenditure$income
fit_beta  <- betareg(share ~ income + persons, data = FoodExpenditure)
fit_beta2 <- betareg(share ~ income + persons | persons,
                     data = FoodExpenditure)
table_regression(list("Constant precision" = fit_beta,
                      "Precision ~ persons" = fit_beta2), digits = 3)
#> Beta regression comparison: share
#> 
#>                   Constant precision     Precision ~ persons  
#>                  ─────────────────────  ───────────────────── 
#>  Variable      │    B      SE      p       B      SE      p   
#> ───────────────┼──────────────────────────────────────────────
#>  (Intercept)   │  -0.623  0.224   .005   -0.783  0.178  <.001 
#>  income        │  -0.012  0.003  <.001   -0.008  0.002  <.001 
#>  persons       │   0.118  0.035  <.001    0.093  0.035   .008 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Precision:    │                                              
#>    (Intercept) │                          5.504  0.533  <.001 
#>    persons     │                         -0.484  0.133  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │  38                     38                   
#>  AIC           │ -82.7                  -88.4                 
#> 
#> Note. Beta regression models. Std. errors: classical (Fisher information). Precision component: log scale. AIC = Akaike information criterion.
```

The coefficients of the first column are those of Table 2 of Ferrari and
Cribari-Neto. In the second, the submodel of the precision has its own
block, `Precision`, with a coefficient, a standard error and a test for
each of its terms. They are on the scale of the link of the precision,
the logarithm. The coefficient of `persons` is negative: the shares of
the larger households are more dispersed.

[`summary()`](https://rdrr.io/r/base/summary.html) gives the same
numbers.

``` r

summary(fit_beta)$coefficients
#> $mean
#>                Estimate  Std. Error   z value     Pr(>|z|)
#> (Intercept) -0.62254806 0.223853539 -2.781051 5.418326e-03
#> income      -0.01229884 0.003035585 -4.051556 5.087819e-05
#> persons      0.11846210 0.035340667  3.352005 8.022853e-04
#> 
#> $precision
#>       Estimate Std. Error  z value     Pr(>|z|)
#> (phi) 35.60975   8.079598 4.407366 1.046351e-05
summary(fit_beta2)$coefficients
#> $mean
#>                 Estimate  Std. Error   z value     Pr(>|z|)
#> (Intercept) -0.783081551 0.177708226 -4.406558 1.050263e-05
#> income      -0.008216837 0.002410524 -3.408734 6.526502e-04
#> persons      0.092553806 0.034821371  2.657960 7.861526e-03
#> 
#> $precision
#>               Estimate Std. Error   z value     Pr(>|z|)
#> (Intercept)  5.5043097  0.5333500 10.320258 5.706974e-25
#> persons     -0.4835233  0.1334643 -3.622866 2.913567e-04
```

A constant precision is one number, and it has no block: it is the
`"phi"` fit statistic, shown below.

### The mean submodel alone

`show_components = FALSE` leaves the block out.

``` r

table_regression(fit_beta2, show_components = FALSE, digits = 3)
#> Beta regression: share
#> 
#>  Variable    │    B      SE         95% CI         p   
#> ─────────────┼─────────────────────────────────────────
#>  (Intercept) │  -0.783  0.178  [-1.131, -0.435]  <.001 
#>  income      │  -0.008  0.002  [-0.013, -0.003]  <.001 
#>  persons     │   0.093  0.035  [ 0.024,  0.161]   .008 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │  38                                     
#>  AIC         │ -88.4                                   
#> 
#> Note. Beta regression. Std. errors: classical (Fisher information). CI = confidence interval; AIC = Akaike information criterion.
```

### Odds ratios

With the logit link, a coefficient of the mean submodel is a log odds
ratio of the expected proportion, and `exponentiate = TRUE` shows the
odds ratio.

``` r

table_regression(fit_beta2, exponentiate = TRUE, digits = 3)
#> Beta regression: share
#> 
#>  Variable      │   OR      SE         95% CI         p   
#> ───────────────┼─────────────────────────────────────────
#>  (Intercept)   │   0.457  0.081  [ 0.323,  0.647]  <.001 
#>  income        │   0.992  0.002  [ 0.987,  0.997]  <.001 
#>  persons       │   1.097  0.038  [ 1.025,  1.174]   .008 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Precision:    │                                         
#>    (Intercept) │   5.504  0.533  [ 4.459,  6.550]  <.001 
#>    persons     │  -0.484  0.133  [-0.745, -0.222]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │  38                                     
#>  AIC           │ -88.4                                   
#> 
#> Note. Beta regression. Coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric). The (Intercept) row estimates the baseline odds. Std. errors: classical (Fisher information). Precision component: log scale. The component rows stay on the link scale (not exponentiated). CI = confidence interval; AIC = Akaike information criterion.
```

One more person in the household multiplies the odds of the expected
food share by 1.10, 95% CI \[1.02, 1.17\]. The precision block is not
exponentiated: its coefficients are not log odds, and its rows stay on
the scale of their link.

### Other links and fit statistics

[`betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html) takes other
links for the mean. The note names a link other than the logit, and so
does the title of a table of one model.

``` r

fit_probit <- update(fit_beta, link = "probit")
table_regression(list(Logit = fit_beta, Probit = fit_probit), digits = 3,
                 show_fit_stats = c("nobs", "phi", "pseudo_r2_ferrari", "aic"))
#> Beta regression comparison: share
#> 
#>                                        Logit                 Probit         
#>                                ─────────────────────  ───────────────────── 
#>  Variable                    │    B      SE      p       B      SE      p   
#> ─────────────────────────────┼──────────────────────────────────────────────
#>  (Intercept)                 │  -0.623  0.224   .005   -0.389  0.136   .004 
#>  income                      │  -0.012  0.003  <.001   -0.007  0.002  <.001 
#>  persons                     │   0.118  0.035  <.001    0.070  0.021   .001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                           │  38                     38                   
#>  φ (precision)               │  35.61                  35.13                
#>  R² (Ferrari & Cribari-Neto) │   0.39                   0.40                
#>  AIC                         │ -82.7                  -82.2                 
#> 
#> Note. Logit: beta regression; Probit: probit beta regression. Std. errors: classical (Fisher information). AIC = Akaike information criterion.
table_regression(fit_probit, digits = 3)
#> Probit beta regression: share
#> 
#>  Variable    │    B      SE         95% CI         p   
#> ─────────────┼─────────────────────────────────────────
#>  (Intercept) │  -0.389  0.136  [-0.655, -0.123]   .004 
#>  income      │  -0.007  0.002  [-0.011, -0.004]  <.001 
#>  persons     │   0.070  0.021  [ 0.028,  0.111]   .001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │  38                                     
#>  AIC         │ -82.2                                   
#> 
#> Note. Probit beta regression. Std. errors: classical (Fisher information). CI = confidence interval; AIC = Akaike information criterion.
c(logit = fit_beta$pseudo.r.squared, probit = fit_probit$pseudo.r.squared)
#>     logit    probit 
#> 0.3878327 0.3989190
```

Two fit statistics are specific to these models. `"phi"` is the constant
precision. `"pseudo_r2_ferrari"` is the pseudo-R² that
[`summary()`](https://rdrr.io/r/base/summary.html) prints, the squared
correlation between the linear predictor and the outcome on the scale of
the link. Neither is in the default rows, which are the number of
observations and the AIC.

### Does the precision vary?

The two models of the first table are nested, and `nested = TRUE` tests
the precision submodel by a likelihood-ratio test.

``` r

table_regression(list("Constant precision" = fit_beta,
                      "Precision ~ persons" = fit_beta2),
                 nested = TRUE, digits = 3)
#> Hierarchical beta regression: share
#> 
#>                   Constant precision       Precision ~ persons    
#>                  ─────────────────────  ───────────────────────── 
#>  Variable      │    B      SE      p         B        SE      p   
#> ───────────────┼──────────────────────────────────────────────────
#>  (Intercept)   │  -0.623  0.224   .005   -0.783      0.178  <.001 
#>  income        │  -0.012  0.003  <.001   -0.008      0.002  <.001 
#>  persons       │   0.118  0.035  <.001    0.093      0.035   .008 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Precision:    │                                                  
#>    (Intercept) │                          5.504      0.533  <.001 
#>    persons     │                         -0.484      0.133  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │  38                     38                       
#>  AIC           │ -82.7                  -88.4                     
#>  Δχ²           │    –                     7.703 (1)               
#>  p (change)    │    –                      .006                   
#> 
#> Note. Beta regression models. Std. errors: classical (Fisher information). Precision component: log scale. AIC = Akaike information criterion.
lmtest::lrtest(fit_beta, fit_beta2)
#> Likelihood ratio test
#> 
#> Model 1: share ~ income + persons
#> Model 2: share ~ income + persons | persons
#>   #Df LogLik Df  Chisq Pr(>Chisq)   
#> 1   4 45.334                        
#> 2   5 49.185  1 7.7029   0.005513 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The test is that of
[`lmtest::lrtest()`](https://rdrr.io/pkg/lmtest/man/lrtest.html): 7.70
on 1 degree of freedom, p = .006. The precision depends on the size of
the household.

### Effects on the proportion

Coefficients on the logit scale do not say by how much the share
changes. The average marginal effects do, on the scale of the
proportion.

``` r

table_regression(fit_beta2, show_columns = c("b", "p", "ame", "ame_p"),
                 digits = 4)
#> Beta regression: share
#> 
#>  Variable      │    B        p      AME      p   
#> ───────────────┼─────────────────────────────────
#>  (Intercept)   │  -0.7831  <.001                 
#>  income        │  -0.0082  <.001  -0.0017  <.001 
#>  persons       │   0.0926   .008   0.0187   .010 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Precision:    │                                 
#>    (Intercept) │   5.5043  <.001                 
#>    persons     │  -0.4835  <.001                 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │  38                             
#>  AIC           │ -88.4                           
#> 
#> Note. Beta regression. Std. errors: classical (Fisher information). AME of a continuous predictor: average slope (dY/dX). Precision component: log scale. AME = average marginal effect; AIC = Akaike information criterion.
```

One more person goes with an expected food share higher by 1.9
percentage points on average. The precision block has no AME: the
effects are those of the mean submodel.

## Sample-selection models

A sample-selection model has two equations. The first, a probit, says
who is observed. The second is the regression of interest, estimated on
those who are. The two error terms may be correlated, and the model
estimates that correlation. The example is the one of the help of
[`selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html):
the wage of 753 married women (`Mroz87`), of whom 428 were in the labor
force and have a wage.

[`selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html)
estimates the model by maximum likelihood,
[`heckit()`](https://rdrr.io/pkg/sampleSelection/man/selection.html) by
the two-step method. Family income is counted in thousands of dollars
here, and the tables show three decimals.

``` r

data("Mroz87", package = "sampleSelection")
Mroz87$kids <- Mroz87$kids5 + Mroz87$kids618 > 0
Mroz87$faminc <- Mroz87$faminc / 1000
fit_ml <- selection(lfp ~ age + I(age^2) + faminc + kids + educ,
                    wage ~ exper + I(exper^2) + educ + city, data = Mroz87)
fit_2s <- heckit(lfp ~ age + I(age^2) + faminc + kids + educ,
                 wage ~ exper + I(exper^2) + educ + city, data = Mroz87)
table_regression(fit_ml, digits = 3)
#> Heckman selection model: wage
#> 
#>                                   Outcome               Selection       
#>                            ──────────────────────  ──────────────────── 
#>  Variable                │    B       SE      p      B      SE      p   
#> ─────────────────────────┼──────────────────────────────────────────────
#>  (Intercept)             │   -1.963  1.198   .102  -4.120  1.401   .003 
#>  exper                   │    0.028  0.062   .651                       
#>  I(exper^2)              │   -0.000  0.002   .955                       
#>  educ                    │    0.457  0.073  <.001   0.095  0.023  <.001 
#>  city                    │    0.447  0.316   .158                       
#>  age                     │                          0.184  0.066   .005 
#>  I(age^2)                │                         -0.002  0.001   .002 
#>  faminc                  │                          0.006  0.004   .199 
#>  kids:                   │                                              
#>    FALSE (ref.)          │                           –      –      –    
#>    TRUE                  │                         -0.451  0.130  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Error terms:            │                                              
#>    σ (outcome error)     │    3.108  0.114   –                          
#>    ρ (error correlation) │   -0.132  0.165   .424                       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                       │  753                                         
#>  AIC                     │ 3188.5                                       
#> 
#> Note. Heckman selection model. Outcome observed for 428 of 753. Std. errors: classical (Fisher information). P-values and CIs: t distribution with 740 df. Error terms: σ, standard deviation of the outcome error; ρ, correlation of the selection and outcome errors. AIC = Akaike information criterion.
```

A model alone sets its two equations side by side: the wage equation
under `Outcome`, then the probit of participation under `Selection`. A
variable fills the cells of the equations it enters. Education is in the
two: it raises the probability of working and, among the women who work,
the wage. `kids` is a logical variable and has a row for each of its
values, the reference first. `outcome_labels` renames the two groups.

The `Error terms` block holds what the model estimates beside its
coefficients, and the note defines each of them. σ is the standard
deviation of the error of the wage equation. ρ is the correlation
between the errors of the two equations. If it is zero, the women who
work are not selected on what the model does not observe, and a
regression on them alone would do. Here ρ is −0.13, p = .424: these data
give no sign of such a selection.

Beside another model, the equations go back to rows: each variable has a
`selection` row, an `outcome` row, or both.

``` r

table_regression(list(Likelihood = fit_ml, Heckit = fit_2s), digits = 3)
#> Heckman selection model comparison: wage
#> 
#>                                    Likelihood               Heckit         
#>                              ──────────────────────  ───────────────────── 
#>  Variable                  │    B       SE      p       B      SE      p   
#> ───────────────────────────┼───────────────────────────────────────────────
#>  (Intercept):              │                                               
#>    selection               │   -4.120  1.401   .003   -4.157  1.402   .003 
#>    outcome                 │   -1.963  1.198   .102   -0.971  2.059   .637 
#>  age:                      │                                               
#>    selection               │    0.184  0.066   .005    0.185  0.066   .005 
#>  I(age^2):                 │                                               
#>    selection               │   -0.002  0.001   .002   -0.002  0.001   .002 
#>  faminc:                   │                                               
#>    selection               │    0.006  0.004   .199    0.005  0.004   .277 
#>  kids:                     │                                               
#>    selection: FALSE (ref.) │     –      –      –        –      –      –    
#>    selection: TRUE         │   -0.451  0.130  <.001   -0.449  0.131  <.001 
#>  educ:                     │                                               
#>    selection               │    0.095  0.023  <.001    0.098  0.023  <.001 
#>    outcome                 │    0.457  0.073  <.001    0.417  0.100  <.001 
#>  exper:                    │                                               
#>    outcome                 │    0.028  0.062   .651    0.021  0.062   .736 
#>  I(exper^2):               │                                               
#>    outcome                 │   -0.000  0.002   .955    0.000  0.002   .942 
#>  city:                     │                                               
#>    outcome                 │    0.447  0.316   .158    0.444  0.316   .160 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Error terms:              │                                               
#>    σ (outcome error)       │    3.108  0.114   –       3.200   –      –    
#>    λ (inverse Mills ratio) │                          -1.098  1.266   .386 
#>    ρ (error correlation)   │   -0.132  0.165   .424   -0.343   –      –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                         │  753                    753                   
#>  AIC                       │ 3188.5                     –                  
#> 
#> Note. Heckman selection models. Outcome observed for 428 of 753. Std. errors: Likelihood: classical (Fisher information), Heckit: classical (two-step). P-values and CIs: t distribution with 740 df. Error terms: λ, coefficient of the inverse Mills ratio, an estimate of ρσ; σ, standard deviation of the outcome error; ρ, correlation of the selection and outcome errors. In a two-step fit, σ and ρ are derived from the estimates and have no standard error. AIC = Akaike information criterion.
```

The two-step method does not estimate ρ directly. It adds a regressor to
the wage equation, the inverse Mills ratio, whose coefficient λ
estimates ρσ, and it is the test of λ that plays this role (p = .386). σ
and ρ are then derived from the estimates, without a standard error. A
two-step fit has no likelihood, hence no AIC.

The note also gives the number of observations whose outcome is
observed, the standard errors of each model, and the reference
distribution of the tests.
[`summary()`](https://rdrr.io/r/base/summary.html) reports the same
values.

``` r

summary(fit_ml)$estimate[c("educ", "sigma", "rho"), ]
#>         Estimate Std. Error    t value      Pr(>|t|)
#> educ   0.0952808 0.02315342  4.1151936  4.303375e-05
#> sigma  3.1083762 0.11383277 27.3065142 4.201283e-114
#> rho   -0.1319586 0.16512710 -0.7991335  4.244694e-01
summary(fit_2s)$estimate[c("educ", "invMillsRatio", "sigma", "rho"), ]
#>                  Estimate Std. Error    t value     Pr(>|t|)
#> educ           0.09818228 0.02298412  4.2717441 2.192529e-05
#> invMillsRatio -1.09761942 1.26598561 -0.8670078 3.862188e-01
#> sigma          3.20006428         NA         NA           NA
#> rho           -0.34299918         NA         NA           NA
```

The help of
[`selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html)
states what these models assume: “a known (multivariate normal)
distribution of error terms”. It adds that “standard errors tend to be
large without a strong exclusion restriction”, that is, without a
variable that enters the selection equation and not the outcome
equation.

### Comparing two selection models

Two models estimated by maximum likelihood are compared by a
likelihood-ratio test.

``` r

fit_ml0 <- selection(lfp ~ age + I(age^2) + faminc + kids + educ,
                     wage ~ exper + I(exper^2) + educ, data = Mroz87)
table_regression(list("Without city" = fit_ml0, "With city" = fit_ml),
                 nested = TRUE, show_columns = c("b", "se", "p"), digits = 3)
#> Hierarchical Heckman selection model: wage
#> 
#>                                   Without city               With city          
#>                              ──────────────────────  ────────────────────────── 
#>  Variable                  │    B       SE      p         B         SE      p   
#> ───────────────────────────┼────────────────────────────────────────────────────
#>  (Intercept):              │                                                    
#>    selection               │   -4.122  1.400   .003    -4.120      1.401   .003 
#>    outcome                 │   -1.875  1.195   .117    -1.963      1.198   .102 
#>  age:                      │                                                    
#>    selection               │    0.184  0.066   .005     0.184      0.066   .005 
#>  I(age^2):                 │                                                    
#>    selection               │   -0.002  0.001   .002    -0.002      0.001   .002 
#>  faminc:                   │                                                    
#>    selection               │    0.006  0.004   .193     0.006      0.004   .199 
#>  kids:                     │                                                    
#>    selection: FALSE (ref.) │     –      –      –         –          –      –    
#>    selection: TRUE         │   -0.451  0.130  <.001    -0.451      0.130  <.001 
#>  educ:                     │                                                    
#>    selection               │    0.095  0.023  <.001     0.095      0.023  <.001 
#>    outcome                 │    0.471  0.073  <.001     0.457      0.073  <.001 
#>  exper:                    │                                                    
#>    outcome                 │    0.032  0.062   .605     0.028      0.062   .651 
#>  I(exper^2):               │                                                    
#>    outcome                 │   -0.000  0.002   .908    -0.000      0.002   .955 
#>  city:                     │                                                    
#>    outcome                 │                            0.447      0.316   .158 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Error terms:              │                                                    
#>    σ (outcome error)       │    3.116  0.114   –        3.108      0.114   –    
#>    ρ (error correlation)   │   -0.135  0.163   .409    -0.132      0.165   .424 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                         │  753                     753                       
#>  AIC                       │ 3188.5                  3188.5                     
#>  Δχ²                       │     –                      1.993 (1)               
#>  p (change)                │     –                       .158                   
#> 
#> Note. Heckman selection models. Outcome observed for 428 of 753. Std. errors: classical (Fisher information). P-values and CIs: t distribution; degrees of freedom: 741 (Without city), 740 (With city). Error terms: σ, standard deviation of the outcome error; ρ, correlation of the selection and outcome errors. AIC = Akaike information criterion.
```

Living in a city does not improve the wage equation: 1.99 on 1 degree of
freedom, p = .158. The comparison is refused for a two-step fit, which
has no likelihood.

### A binary outcome

When the outcome is binary, its equation is a probit too, which the
title and the note state, and the model has no σ.

``` r

fit_bin <- selection(lfp ~ educ + age + kids5 + kids618 + nwifeinc,
                     wage >= 5 ~ educ, data = Mroz87)
table_regression(fit_bin)
#> Heckman selection model (binary outcome): wage >= 5
#> 
#>                                  Outcome             Selection      
#>                            ────────────────────  ────────────────── 
#>  Variable                │    B      SE     p      B     SE     p   
#> ─────────────────────────┼──────────────────────────────────────────
#>  (Intercept)             │   -4.52  0.56  <.001   0.43  0.48   .366 
#>  educ                    │    0.29  0.04  <.001   0.16  0.02  <.001 
#>  age                     │                       -0.03  0.01  <.001 
#>  kids5                   │                       -0.89  0.11  <.001 
#>  kids618                 │                       -0.04  0.04   .332 
#>  nwifeinc                │                       -0.02  0.00  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Error terms:            │                                          
#>    ρ (error correlation) │    0.12  0.27   .667                     
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                       │  753                                     
#>  AIC                     │ 1324.4                                   
#> 
#> Note. Heckman selection model (binary outcome). Outcome observed for 428 of 753. Outcome equation: probit. Std. errors: classical (Fisher information). P-values and CIs: t distribution with 744 df. Error terms: ρ, correlation of the selection and outcome errors. AIC = Akaike information criterion.
```

### Treatment-effects models

[`treatReg()`](https://rdrr.io/pkg/sampleSelection/man/selection.html)
estimates the effect of a binary treatment that the units choose. The
outcome is observed for everyone, and the equation under `Selection` is
that of the treatment. The example takes the college attendance of the
wife and the logarithm of the family income.

``` r

Mroz87$college <- trimws(Mroz87$wifecoll) == "TRUE"
fit_tr <- treatReg(college ~ motheduc + fatheduc + age,
                   log(faminc) ~ college + huseduc + age + city,
                   data = Mroz87)
table_regression(fit_tr)
#> Treatment-effects model: log(faminc)
#> 
#>                                  Outcome             Selection      
#>                            ────────────────────  ────────────────── 
#>  Variable                │    B      SE     p      B     SE     p   
#> ─────────────────────────┼──────────────────────────────────────────
#>  (Intercept)             │    1.88  0.13  <.001  -2.73  0.37  <.001 
#>  college:                │                                          
#>    FALSE (ref.)          │     –     –     –                        
#>    TRUE                  │    0.30  0.09  <.001                     
#>  huseduc                 │    0.05  0.01  <.001                     
#>  age                     │    0.01  0.00   .001   0.01  0.01   .434 
#>  city                    │    0.19  0.04  <.001                     
#>  motheduc                │                        0.10  0.02  <.001 
#>  fatheduc                │                        0.10  0.02  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Error terms:            │                                          
#>    σ (outcome error)     │    0.45  0.01   –                        
#>    ρ (error correlation) │   -0.15  0.12   .195                     
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                       │  753                                     
#>  AIC                     │ 1711.5                                   
#> 
#> Note. Treatment-effects model. Std. errors: classical (Fisher information). P-values and CIs: t distribution with 742 df. Error terms: σ, standard deviation of the outcome error; ρ, correlation of the selection and outcome errors. AIC = Akaike information criterion.
```

### Dropping an error term

The error terms are coefficients of the fit, and `keep` and `drop`
select them by their name in
[`coef()`](https://rdrr.io/r/stats/coef.html): `"sigma"`, `"rho"`,
`"invMillsRatio"`.

``` r

table_regression(fit_ml, drop = "sigma", show_columns = c("b", "se", "p"),
                 digits = 3)
#> Heckman selection model: wage
#> 
#>                                   Outcome               Selection       
#>                            ──────────────────────  ──────────────────── 
#>  Variable                │    B       SE      p      B      SE      p   
#> ─────────────────────────┼──────────────────────────────────────────────
#>  (Intercept)             │   -1.963  1.198   .102  -4.120  1.401   .003 
#>  exper                   │    0.028  0.062   .651                       
#>  I(exper^2)              │   -0.000  0.002   .955                       
#>  educ                    │    0.457  0.073  <.001   0.095  0.023  <.001 
#>  city                    │    0.447  0.316   .158                       
#>  age                     │                          0.184  0.066   .005 
#>  I(age^2)                │                         -0.002  0.001   .002 
#>  faminc                  │                          0.006  0.004   .199 
#>  kids:                   │                                              
#>    FALSE (ref.)          │                           –      –      –    
#>    TRUE                  │                         -0.451  0.130  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Error terms:            │                                              
#>    ρ (error correlation) │   -0.132  0.165   .424                       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                       │  753                                         
#>  AIC                     │ 3188.5                                       
#> 
#> Note. Heckman selection model. Outcome observed for 428 of 753. Std. errors: classical (Fisher information). P-values and CIs: t distribution with 740 df. Error terms: ρ, correlation of the selection and outcome errors. AIC = Akaike information criterion.
```

### What is refused

A tobit-5 model has two outcome equations, one for each regime. A table
of one outcome cannot show it, and it is refused.

``` r

set.seed(1)
n <- 500
sim <- data.frame(z = rnorm(n), x1 = rnorm(n), x2 = rnorm(n))
e <- MASS::mvrnorm(n, c(0, 0, 0),
                   matrix(c(1, -0.5, 0.5, -0.5, 1, 0, 0.5, 0, 1), 3))
sim$s  <- sim$z + e[, 1] > 0
sim$y1 <- sim$x1 + e[, 2]
sim$y2 <- sim$x2 + e[, 3]
fit_t5 <- selection(s ~ z, list(y1 ~ x1, y2 ~ x2), data = sim)
table_regression(fit_t5)
#> Error in `table_regression()`:
#> ! A tobit-5 `selection` fit is not supported by `table_regression()`.
#> ℹ A tobit-5 (switching regression) fit has two outcome equations, one per regime; the table shows the sample-selection (tobit-2) model and the treatment-effects model of `treatReg()`.
```

## Tables in another language

The blocks and their notes follow `options(spicy.language = )`.

``` r

options(spicy.language = "fr", width = 92)
table_regression(list(Linear = fit_glm, GAM = fit_gam), nested = TRUE)
#> Régression — modèles emboîtés : type
#> 
#>                                           Linear                    GAM             
#>                                    ────────────────────  ────────────────────────── 
#>  Variable                        │   B      ES     p          B         ES     p    
#> ─────────────────────────────────┼──────────────────────────────────────────────────
#>  (Intercept)                     │  -9,34  0,87  <0,001   -8,13        0,84  <0,001 
#>  age                             │   0,03  0,01   0,044                             
#>  bmi                             │   0,09  0,02  <0,001    0,08        0,02  <0,001 
#>  glu                             │   0,04  0,00  <0,001    0,04        0,00  <0,001 
#>  npreg                           │   0,11  0,04   0,008    0,05        0,05   0,244 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Termes lissés (ddl effectifs) : │                                                  
#>    s(age)                        │                         3,63         –     0,002 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                               │ 532                   532                        
#>  R² (Tjur)                       │   0,34                  0,36                     
#>  R² (McFadden)                   │   0,29                  0,32                     
#>  AIC                             │ 491,1                 479,2                      
#>  Δχ²                             │    –                   19,01 (2,6)               
#>  p (variation)                   │    –                   <0,001                    
#> 
#> Note. Linear : régression logistique ; GAM : GAM logistique. Erreurs standards : Linear : classiques (information de Fisher), GAM : bayésiennes (covariance a posteriori). Test de variation d'un modèle additif : approché, sur la différence des degrés de liberté effectifs. Termes lissés : degrés de liberté effectifs et test approché de chaque terme. AIC = critère d'information d'Akaike.
options(spicy.language = NULL, width = 80)
```

## Presentation-ready output

The blocks are part of the table in every output engine. Here the first
table as a gt object, with odds ratios.

``` r

pkgdown_dark_gt(table_regression(list(Linear = fit_glm, GAM = fit_gam),
                                 exponentiate = TRUE,
                                 show_intercept = FALSE, output = "gt"))
```

[TABLE]

## References

- Ferrari, S., & Cribari-Neto, F. (2004). Beta regression for modelling
  rates and proportions. *Journal of Applied Statistics*, 31(7),
  799–815. <https://doi.org/10.1080/0266476042000214501>
