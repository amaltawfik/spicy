# Regression tables for rms fits

``` r

library(spicy)
library(rms) # lrm(), ols(), Glm(), cph(), rcs(), datadist()
library(survival) # Surv(), pbc
```

This article covers models fitted with the rms package (Harrell, 2015):
[`lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) for logistic regression,
[`ols()`](https://rdrr.io/pkg/rms/man/ols.html) for the linear model,
[`Glm()`](https://rdrr.io/pkg/rms/man/Glm.html) for generalized linear
models and [`cph()`](https://rdrr.io/pkg/rms/man/cph.html) for the Cox
model. The companion article [*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
covers the shared mechanics (output engines, labels,
[`tidy()`](https://generics.r-lib.org/reference/tidy.html)). An
[`lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) fit of an ordinal
outcome is a proportional-odds model, and its table has the thresholds
block described in [*Ordinal regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.md).
The class-by-class map is [*Supported
models*](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.md).

## What an rms fit is for

The fitting functions of rms estimate the same models as
[`lm()`](https://rdrr.io/r/stats/lm.html),
[`glm()`](https://rdrr.io/r/stats/glm.html) and
[`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html). An analyst
chooses them for what comes with the fit: spline terms declared in the
formula, [`anova()`](https://rdrr.io/r/stats/anova.html) tests that pool
all the terms of a predictor,
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`Predict()`](https://rdrr.io/pkg/rms/man/Predict.html) for effects over
a chosen range, and the validation, calibration and nomogram tools of
the package.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reads the fit as it is and builds the table a paper needs. The estimates
and their standard errors are those of rms.

## The data

The examples use the Pima diabetes data of MASS (Venables & Ripley,
2002): 532 women of Pima heritage aged 21 or more, tested for diabetes,
of whom 177 are diabetic. The training and the test halves of MASS are
stacked. One question runs through the article: is age associated with
diabetes at equal glucose, body mass index and number of pregnancies,
and is that association linear?

``` r

pima <- rbind(MASS::Pima.tr, MASS::Pima.te)
dd <- datadist(pima)
options(datadist = "dd")
c(n = nrow(pima), diabetic = sum(pima$type == "Yes"))
#>        n diabetic 
#>      532      177
```

[`datadist()`](https://rdrr.io/pkg/rms/man/datadist.html) is a
requirement of rms’s own
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`Predict()`](https://rdrr.io/pkg/rms/man/Predict.html).
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
does not need it.

## A first table

``` r

fit_lin <- lrm(type ~ age + bmi + glu + npreg, data = pima)
table_regression(fit_lin)
#> Logistic regression: type
#> 
#>  Variable      │   B      SE       95% CI         p   
#> ───────────────┼──────────────────────────────────────
#>  (Intercept)   │  -9.34  0.87  [-11.05, -7.63]  <.001 
#>  age           │   0.03  0.01  [  0.00,  0.05]   .044 
#>  bmi           │   0.09  0.02  [  0.05,  0.12]  <.001 
#>  glu           │   0.04  0.00  [  0.03,  0.04]  <.001 
#>  npreg         │   0.11  0.04  [  0.03,  0.20]   .008 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │ 532                                  
#>  R² (Tjur)     │   0.34                               
#>  R² (McFadden) │   0.29                               
#>  AIC           │ 491.1                                
#> 
#> Note. Logistic regression. Std. errors: classical (Fisher information). CI = confidence interval; AIC = Akaike information criterion.
```

The coefficients and the standard errors are those of `print(fit_lin)`,
rounded: 0.0264 (0.0131) for age prints as 0.03 (0.01). The tests are
Wald z tests, as in rms.

The fit statistics differ from rms’s printout. The table reports Tjur’s
(2009) and McFadden’s R² and the AIC, which
[`print()`](https://rdrr.io/r/base/print.html) does not show. It leaves
out the likelihood-ratio χ² of the model and the indexes of
discrimination and accuracy (C, D_(xy), the Brier score), which have no
token. A later section carries two of them, the C index and the Brier
score, in the note.

## The same model as `glm()`

``` r

fit_glm <- glm(type ~ age + bmi + glu + npreg, data = pima, family = binomial)

table_regression(
  list(glm = fit_glm, lrm = fit_lin),
  digits = 4,
  show_columns = c("b", "se", "p")
)
#> Logistic regression comparison: type
#> 
#>                            glm                      lrm           
#>                  ───────────────────────  ─────────────────────── 
#>  Variable      │    B        SE      p       B        SE      p   
#> ───────────────┼──────────────────────────────────────────────────
#>  (Intercept)   │  -9.3388  0.8716  <.001   -9.3388  0.8716  <.001 
#>  age           │   0.0264  0.0131   .044    0.0264  0.0131   .044 
#>  bmi           │   0.0873  0.0178  <.001    0.0873  0.0178  <.001 
#>  glu           │   0.0352  0.0041  <.001    0.0352  0.0041  <.001 
#>  npreg         │   0.1137  0.0426   .008    0.1137  0.0426   .008 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │ 532                      532                     
#>  R² (Tjur)     │   0.34                     0.34                  
#>  R² (McFadden) │   0.29                     0.29                  
#>  AIC           │ 491.1                    491.1                   
#> 
#> Note. Logistic regression models. Std. errors: classical (Fisher information). AIC = Akaike information criterion.
```

The two columns agree to four decimals, and so do the fit statistics:
[`lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) and
[`glm()`](https://rdrr.io/r/stats/glm.html) maximize the same
likelihood. A table can therefore hold fits of both engines when an
analysis used each for a part of its models.

## Relaxing linearity: restricted cubic splines

The linear model assumes that each year of age shifts the log-odds by
the same amount. Harrell advises against assuming it for a continuous
predictor without a reason: “Relationships among variables are seldom
linear” (Harrell, 2015, p. 18). A restricted cubic spline lets the data
choose the shape. It is a piecewise cubic function, constrained to be
linear beyond the outer knots, and it costs k − 1 coefficients for k
knots (p. 24).

``` r

fit_rcs <- lrm(
  type ~ rcs(age, 4) + bmi + glu + npreg,
  data = pima, x = TRUE, y = TRUE
)
fit_rcs$Design$parms$age
#> [1] 21 25 33 54
table_regression(fit_rcs)
#> Logistic regression: type
#> 
#>  Variable      │   B      SE       95% CI         p   
#> ───────────────┼──────────────────────────────────────
#>  (Intercept)   │ -13.76  2.80  [-19.24, -8.27]  <.001 
#>  age           │   0.21  0.11  [ -0.01,  0.43]   .059 
#>  age'          │  -0.85  1.19  [ -3.18,  1.49]   .476 
#>  age''         │   1.05  1.89  [ -2.64,  4.75]   .576 
#>  bmi           │   0.08  0.02  [  0.04,  0.11]  <.001 
#>  glu           │   0.04  0.00  [  0.03,  0.04]  <.001 
#>  npreg         │   0.06  0.04  [ -0.03,  0.15]   .170 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │ 532                                  
#>  R² (Tjur)     │   0.36                               
#>  R² (McFadden) │   0.31                               
#>  AIC           │ 480.7                                
#> 
#> Note. Logistic regression. Std. errors: classical (Fisher information). CI = confidence interval; AIC = Akaike information criterion.
```

With four knots, rms places them at the 5th, 35th, 65th and 95th
percentiles of age: 21, 25, 33 and 54 years. `x = TRUE, y = TRUE` stores
the design matrix and the response in the fit, for the robust variance
and the penalized fit of later sections.

The table has three rows for age, named as rms names them: `age`, `age'`
and `age''`. None of them reads alone. They are the coefficients of
three basis functions, and the p-value of one row tests one basis
function with the two others kept in the model, which answers no
question about age. Two questions do have an answer: is age associated
with diabetes at all, and does the association depart from a line?

## Testing a predictor as a whole

Each question compares two nested models, and `nested = TRUE` puts the
comparison in the table. Harrell gives the order: “all terms involving
the predictor should be tested simultaneously to check whether the
factor is associated with the outcome. This test should precede the test
for linearity” (p. 32).

``` r

fit_noage <- lrm(type ~ bmi + glu + npreg, data = pima)

table_regression(
  list("Without age" = fit_noage, "Spline" = fit_rcs),
  nested = TRUE,
  show_columns = c("b", "p"),
  show_fit_stats = c("nobs", "aic", "lrt_change", "p_change")
)
#> Hierarchical logistic regression: type
#> 
#>                 Without age        Spline       
#>                ─────────────  ───────────────── 
#>  Variable    │   B       p        B         p   
#> ─────────────┼──────────────────────────────────
#>  (Intercept) │  -8.83  <.001  -13.76      <.001 
#>  bmi         │   0.09  <.001    0.08      <.001 
#>  glu         │   0.04  <.001    0.04      <.001 
#>  npreg       │   0.17  <.001    0.06       .170 
#>  age         │                  0.21       .059 
#>  age'        │                 -0.85       .476 
#>  age''       │                  1.05       .576 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 532            532               
#>  AIC         │ 493.2          480.7             
#>  Δχ²         │    –            18.57 (3)        
#>  p (change)  │    –             <.001           
#> 
#> Note. Logistic regression models. Std. errors: classical (Fisher information). AIC = Akaike information criterion.

table_regression(
  list(Linear = fit_lin, Spline = fit_rcs),
  nested = TRUE,
  show_columns = c("b", "p"),
  show_fit_stats = c("nobs", "aic", "lrt_change", "p_change")
)
#> Hierarchical logistic regression: type
#> 
#>                   Linear           Spline       
#>                ─────────────  ───────────────── 
#>  Variable    │   B       p        B         p   
#> ─────────────┼──────────────────────────────────
#>  (Intercept) │  -9.34  <.001  -13.76      <.001 
#>  age         │   0.03   .044    0.21       .059 
#>  bmi         │   0.09  <.001    0.08      <.001 
#>  glu         │   0.04  <.001    0.04      <.001 
#>  npreg       │   0.11   .008    0.06       .170 
#>  age'        │                 -0.85       .476 
#>  age''       │                  1.05       .576 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 532            532               
#>  AIC         │ 491.1          480.7             
#>  Δχ²         │    –            14.46 (2)        
#>  p (change)  │    –             <.001           
#> 
#> Note. Logistic regression models. Std. errors: classical (Fisher information). AIC = Akaike information criterion.
```

The first table tests the three age coefficients together: Δχ² = 18.57
on 3 degrees of freedom, p \< .001. Age is associated with diabetes. The
second tests the two nonlinear coefficients: Δχ² = 14.46 on 2 degrees of
freedom, p \< .001. The association is not linear.

Both are likelihood-ratio tests. `anova(fit_rcs)` of rms gives the Wald
version of the same two hypotheses, 17.60 and 13.64 here, with the same
conclusions.

## How many knots

Harrell reports that the fit depends on the number of knots much more
than on their position, and that four knots suit many data sets (p. 26,
28). The AIC can arbitrate.

``` r

sapply(3:5, function(k) {
  f <- lrm(type ~ rcs(age, k) + bmi + glu + npreg, data = pima)
  c(knots = k, AIC = round(AIC(f), 1))
})
#>        [,1]  [,2]  [,3]
#> knots   3.0   4.0   5.0
#> AIC   479.4 480.7 482.7
```

Three knots have the lowest AIC, four are about one point above, and a
fifth adds a coefficient for nothing. The article keeps four.

## Reading a nonlinear effect

A table holds one number per predictor, and a curve does not fit in one
number. The average marginal effect is the closest summary: the average,
over the women of the sample, of the slope of the predicted probability
with respect to age.

``` r

table_regression(
  fit_rcs,
  show_columns = c("b", "ame", "ame_ci", "ame_p"),
  digits = 3
)
#> Logistic regression: type
#> 
#>  Variable      │    B      AME       95% CI         p   
#> ───────────────┼────────────────────────────────────────
#>  (Intercept)   │ -13.758                                
#>  age           │   0.213  0.012  [ 0.006, 0.018]  <.001 
#>  age'          │  -0.848                                
#>  age''         │   1.054                                
#>  bmi           │   0.080  0.012  [ 0.007, 0.017]  <.001 
#>  glu           │   0.036  0.005  [ 0.005, 0.006]  <.001 
#>  npreg         │   0.062  0.009  [-0.004, 0.022]   .167 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │ 532                                    
#>  R² (Tjur)     │   0.36                                 
#>  R² (McFadden) │   0.31                                 
#>  AIC           │ 480.7                                  
#> 
#> Note. Logistic regression. Std. errors: classical (Fisher information). AME of a continuous predictor: average slope (dY/dX). AME = average marginal effect; CI = confidence interval; AIC = Akaike information criterion.
```

The AME of age appears once, on the first of its three rows, and it
takes the whole spline into account: 0.012, that is 1.2 percentage
points per year of age, 95% CI \[0.006, 0.018\]. The linear model gave
0.004.

The average hides the shape.
[`Predict()`](https://rdrr.io/pkg/rms/man/Predict.html) of rms shows it,
for a woman with the median values of the other predictors:

``` r

Predict(fit_rcs, age = c(25, 30, 35, 40, 45, 50, 55, 60), fun = plogis)
#>   age  bmi glu npreg      yhat     lower     upper
#> 1  25 32.8 115     2 0.1602981 0.1154194 0.2183201
#> 2  30 32.8 115     2 0.2713254 0.1996928 0.3571855
#> 3  35 32.8 115     2 0.3437858 0.2467948 0.4558263
#> 4  40 32.8 115     2 0.3714586 0.2487193 0.5133782
#> 5  45 32.8 115     2 0.3621554 0.2360631 0.5105844
#> 6  50 32.8 115     2 0.3297544 0.2075319 0.4803291
#> 7  55 32.8 115     2 0.2893544 0.1646189 0.4569101
#> 8  60 32.8 115     2 0.2511214 0.1205894 0.4505569
#> 
#> Response variable (y):  
#> 
#> Adjust to: bmi=32.8 glu=115 npreg=2  
#> 
#> Limits are 0.95 confidence limits
```

The predicted probability rises from 0.16 at 25 to 0.37 at 40, then
falls back to 0.25 at 60. A positive average slope summarizes a rise
followed by a decline, which is why the curve of a spline effect belongs
in the paper next to the table.

## Why not cut age into classes

Cutting a continuous predictor into classes is the common alternative,
and Harrell’s judgment of it is blunt: “Nothing could be more
disastrous” (p. 19). Classes assume that the risk is flat within each
interval and jumps at its limits.

``` r

pima$age_group <- cut(pima$age, c(20, 30, 40, 50, 90), right = FALSE)
fit_cat <- lrm(type ~ age_group + bmi + glu + npreg, data = pima)

table_regression(
  list(Linear = fit_lin, Categories = fit_cat, Spline = fit_rcs),
  show_columns = c("b", "p"),
  show_fit_stats = c("nobs", "pseudo_r2_nagelkerke", "aic")
)
#> Logistic regression comparison: type
#> 
#>                        Linear       Categories       Spline     
#>                     ─────────────  ─────────────  ───────────── 
#>  Variable         │   B       p      B       p      B       p   
#> ──────────────────┼─────────────────────────────────────────────
#>  (Intercept)      │  -9.34  <.001   -8.79  <.001  -13.76  <.001 
#>  age              │   0.03   .044                   0.21   .059 
#>  bmi              │   0.09  <.001    0.09  <.001    0.08  <.001 
#>  glu              │   0.04  <.001    0.04  <.001    0.04  <.001 
#>  npreg            │   0.11   .008    0.07   .089    0.06   .170 
#>  age_group:       │                                             
#>    [20,30) (ref.) │                   –     –                   
#>    [30,40)        │                  0.78   .011                
#>    [40,50)        │                  1.22   .002                
#>    [50,90)        │                  0.95   .034                
#>  age'             │                                -0.85   .476 
#>  age''            │                                 1.05   .576 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                │ 532            532            532           
#>  R² (Nagelkerke)  │   0.43           0.44           0.45        
#>  AIC              │ 491.1          487.7          480.7         
#> 
#> Note. Logistic regression models. Std. errors: classical (Fisher information). AIC = Akaike information criterion.
```

Four classes and a spline with four knots both spend three coefficients
on age. The classes reach an AIC of 487.7, the spline 480.7.

## Fit statistics

By default the table of an
[`lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) fit has the number of
observations, Tjur’s R² for a binary outcome, McFadden’s R² and the AIC.
The R² that rms prints is Nagelkerke’s (1991), available as
`"pseudo_r2_nagelkerke"`.

``` r

table_regression(
  fit_rcs,
  show_columns = c("b", "ci", "p"),
  show_fit_stats = c(
    "nobs", "pseudo_r2_nagelkerke", "pseudo_r2_tjur",
    "aic", "bic", "deviance", "log_lik"
  )
)
#> Logistic regression: type
#> 
#>  Variable        │    B         95% CI         p   
#> ─────────────────┼─────────────────────────────────
#>  (Intercept)     │  -13.76  [-19.24, -8.27]  <.001 
#>  age             │    0.21  [ -0.01,  0.43]   .059 
#>  age'            │   -0.85  [ -3.18,  1.49]   .476 
#>  age''           │    1.05  [ -2.64,  4.75]   .576 
#>  bmi             │    0.08  [  0.04,  0.11]  <.001 
#>  glu             │    0.04  [  0.03,  0.04]  <.001 
#>  npreg           │    0.06  [ -0.03,  0.15]   .170 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │  532                            
#>  R² (Nagelkerke) │    0.45                         
#>  R² (Tjur)       │    0.36                         
#>  AIC             │  480.7                          
#>  BIC             │  510.6                          
#>  Deviance        │  466.7                          
#>  Log-likelihood  │ -233.3                          
#> 
#> Note. Logistic regression. Std. errors: classical (Fisher information). CI = confidence interval; AIC = Akaike information criterion; BIC = Bayesian information criterion.
```

rms also computes indexes of discrimination and accuracy, kept in
`fit$stats`: the C index, Somers’ D_(xy), the Brier score. They have no
token in `show_fit_stats`, and `note_add` carries them under the table.

``` r

table_regression(
  fit_rcs,
  show_fit_stats = c("nobs", "pseudo_r2_nagelkerke", "aic"),
  note_add = sprintf(
    "C index = %.3f; Brier score = %.3f (apparent, not validated).",
    fit_rcs$stats[["C"]], fit_rcs$stats[["Brier"]]
  )
)
#> Logistic regression: type
#> 
#>  Variable        │   B      SE       95% CI         p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │ -13.76  2.80  [-19.24, -8.27]  <.001 
#>  age             │   0.21  0.11  [ -0.01,  0.43]   .059 
#>  age'            │  -0.85  1.19  [ -3.18,  1.49]   .476 
#>  age''           │   1.05  1.89  [ -2.64,  4.75]   .576 
#>  bmi             │   0.08  0.02  [  0.04,  0.11]  <.001 
#>  glu             │   0.04  0.00  [  0.03,  0.04]  <.001 
#>  npreg           │   0.06  0.04  [ -0.03,  0.15]   .170 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 532                                  
#>  R² (Nagelkerke) │   0.45                               
#>  AIC             │ 480.7                                
#> 
#> Note. Logistic regression. Std. errors: classical (Fisher information). CI = confidence interval; AIC = Akaike information criterion. C index = 0.857; Brier score = 0.143 (apparent, not validated).
```

These indexes are apparent: computed on the data that fitted the model,
they are optimistic.
[`rms::validate()`](https://rdrr.io/pkg/rms/man/validate.html) estimates
the optimism by bootstrap, and the corrected values are the ones to
report.

## Odds ratios

`exponentiate = TRUE` turns the coefficients into odds ratios. The rows
of the spline are best left out: the exponential of a basis coefficient
is the odds ratio of nothing.

``` r

table_regression(
  fit_rcs,
  exponentiate = TRUE,
  show_columns = c("b", "ci", "p"),
  drop = "age"
)
#> Logistic regression: type
#> 
#>  Variable      │   OR       95% CI       p   
#> ───────────────┼─────────────────────────────
#>  (Intercept)   │   0.00  [0.00, 0.00]  <.001 
#>  bmi           │   1.08  [1.05, 1.12]  <.001 
#>  glu           │   1.04  [1.03, 1.04]  <.001 
#>  npreg         │   1.06  [0.97, 1.16]   .170 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │ 532                         
#>  R² (Tjur)     │   0.36                      
#>  R² (McFadden) │   0.31                      
#>  AIC           │ 480.7                       
#> 
#> Note. Logistic regression. Coefficients exponentiated and displayed as OR; CI bounds exponentiated. The (Intercept) row estimates the baseline odds. Std. errors: classical (Fisher information). CI = confidence interval; AIC = Akaike information criterion.
```

For age, [`summary()`](https://rdrr.io/r/base/summary.html) of rms
compares two values of the predictor, by default its quartiles.

``` r

summary(fit_rcs)
#>              Effects              Response : type 
#> 
#>  Factor      Low    High   Diff.  Effect  S.E.    Lower 0.95 Upper 0.95
#>  age         23.000  38.00 15.000 1.48670 0.36696  0.76751   2.2060    
#>   Odds Ratio 23.000  38.00 15.000 4.42270      NA  2.15440   9.0791    
#>  bmi         27.875  36.90  9.025 0.71829 0.16298  0.39886   1.0377    
#>   Odds Ratio 27.875  36.90  9.025 2.05090      NA  1.49010   2.8228    
#>  glu         98.750 141.25 42.500 1.51240 0.17717  1.16510   1.8596    
#>   Odds Ratio 98.750 141.25 42.500 4.53740      NA  3.20630   6.4213    
#>  npreg        1.000   5.00  4.000 0.24659 0.17981 -0.10583   0.5990    
#>   Odds Ratio  1.000   5.00  4.000 1.27960      NA  0.89957   1.8203
```

The quartiles of age are 23 and 38 years. The odds of diabetes at 38 are
4.42 times those at 23, 95% CI \[2.15, 9.08\]. `note_add` can carry that
sentence under the table.

## Robust and bootstrap variance

rms computes the Huber sandwich with
[`robcov()`](https://rdrr.io/pkg/rms/man/robcov.html) and a bootstrap
covariance matrix with
[`bootcov()`](https://rdrr.io/pkg/rms/man/bootcov.html), each with
optional clusters (Harrell, 2015, p. 198). There are two ways to bring
them into the table.

The first is to ask
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).
`vcov = "CR0"` with a `cluster` calls
[`robcov()`](https://rdrr.io/pkg/rms/man/robcov.html) on the fit, which
must have been fitted with `x = TRUE, y = TRUE`. With one woman per
cluster, the result is the heteroskedasticity-robust sandwich.

``` r

pima$id <- seq_len(nrow(pima))

table_regression(
  fit_rcs,
  vcov = "CR0",
  cluster = ~id,
  show_columns = c("b", "se", "p")
)
#> Logistic regression: type
#> 
#>  Variable      │   B      SE     p   
#> ───────────────┼─────────────────────
#>  (Intercept)   │ -13.76  2.64  <.001 
#>  age           │   0.21  0.11   .051 
#>  age'          │  -0.85  1.18   .471 
#>  age''         │   1.05  1.86   .572 
#>  bmi           │   0.08  0.02  <.001 
#>  glu           │   0.04  0.00  <.001 
#>  npreg         │   0.06  0.05   .194 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │ 532                 
#>  R² (Tjur)     │   0.36              
#>  R² (McFadden) │   0.31              
#>  AIC           │ 480.7               
#> 
#> Note. Logistic regression. Std. errors: cluster-robust (Huber), clusters by id. AIC = Akaike information criterion.
```

The second is to pass a fit that already carries its variance.
[`robcov()`](https://rdrr.io/pkg/rms/man/robcov.html) and
[`bootcov()`](https://rdrr.io/pkg/rms/man/bootcov.html) return the fit
with a new covariance matrix. The table reports that matrix, and the
note names the estimator of each column.

``` r

set.seed(2026)
fit_boot <- bootcov(fit_rcs, B = 200)

table_regression(
  list(Model = fit_rcs, Sandwich = robcov(fit_rcs), Bootstrap = fit_boot),
  show_columns = c("b", "se"),
  drop = "age"
)
#> Logistic regression comparison: type
#> 
#>                     Model        Sandwich     Bootstrap   
#>                  ────────────  ────────────  ──────────── 
#>  Variable      │   B      SE     B      SE     B      SE  
#> ───────────────┼──────────────────────────────────────────
#>  (Intercept)   │ -13.76  2.80  -13.76  2.64  -13.76  2.56 
#>  bmi           │   0.08  0.02    0.08  0.02    0.08  0.02 
#>  glu           │   0.04  0.00    0.04  0.00    0.04  0.00 
#>  npreg         │   0.06  0.04    0.06  0.05    0.06  0.05 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │ 532           532           532          
#>  R² (Tjur)     │   0.36          0.36          0.36       
#>  R² (McFadden) │   0.31          0.31          0.31       
#>  AIC           │ 480.7         480.7         480.7        
#> 
#> Note. Logistic regression models. Std. errors: Model: classical (Fisher information), Sandwich: robust (Huber), Bootstrap: nonparametric bootstrap (200 replicates). AIC = Akaike information criterion.
```

The sandwich of [`robcov()`](https://rdrr.io/pkg/rms/man/robcov.html) is
the Huber estimator, which is `CR0`. The other tokens of `vcov` are
refused, and the error says why.

``` r

table_regression(fit_rcs, vcov = "CR2", cluster = ~id)
#> Error in `table_regression()`:
#> ! `vcov = "CR2"` is not available for `lrm` models.
#> ℹ spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
#> ℹ This class supports: classical, CR0.
```

## Penalized estimation

When a model has many coefficients for its sample, rms can shrink them
by penalized maximum likelihood.
[`pentrace()`](https://rdrr.io/pkg/rms/man/pentrace.html) searches the
penalty that optimizes an information criterion, and
[`update()`](https://rdrr.io/r/stats/update.html) refits with it.

``` r

pen <- pentrace(fit_rcs, seq(0, 20, by = 2))
pen$penalty
#> [1] 2
fit_pen <- update(fit_rcs, penalty = pen$penalty)

table_regression(
  list(MLE = fit_rcs, PMLE = fit_pen),
  show_columns = c("b", "se", "p"),
  show_fit_stats = c("nobs", "pseudo_r2_nagelkerke", "aic")
)
#> Logistic regression comparison: type
#> 
#>                            MLE                 PMLE         
#>                    ───────────────────  ─────────────────── 
#>  Variable        │   B      SE     p      B      SE     p   
#> ─────────────────┼──────────────────────────────────────────
#>  (Intercept)     │ -13.76  2.80  <.001  -10.99  1.08  <.001 
#>  age             │   0.21  0.11   .059    0.11  0.03  <.001 
#>  age'            │  -0.85  1.19   .476   -0.05  0.08   .551 
#>  age''           │   1.05  1.89   .576   -0.13  0.13   .299 
#>  bmi             │   0.08  0.02  <.001    0.08  0.02  <.001 
#>  glu             │   0.04  0.00  <.001    0.03  0.00  <.001 
#>  npreg           │   0.06  0.04   .170    0.08  0.04   .073 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 532                  532                 
#>  R² (Nagelkerke) │   0.45                 0.44              
#>  AIC             │ 480.7                479.6               
#> 
#> Note. Logistic regression models. PMLE: Penalized estimates (penalty: 2). Fit statistics of the penalized model: log-likelihood without the penalty term, effective degrees of freedom. Std. errors: classical (Fisher information). AIC = Akaike information criterion.
```

The penalty chosen is 2. The second column holds the penalized maximum
likelihood estimates (PMLE), and the note says so. Its standard errors
and p-values come from the covariance matrix that rms stores for a
penalized fit. Harrell warns about them: “Penalization will bias
estimates of β, so hypothesis tests and confidence intervals using β̂^(P)
may not have a simple interpretation” (p. 211). The penalized column
serves prediction. Tests are read in the unpenalized one.

The AIC of the penalized column is computed on the log-likelihood
without the penalty term, with the effective degrees of freedom, as the
note says. This is the criterion
[`pentrace()`](https://rdrr.io/pkg/rms/man/pentrace.html) optimizes,
written on the usual scale, where the lower value is the better one:
479.6 for the penalized model against 480.7. `AIC(fit_pen)` of rms gives
another number, because it keeps the penalty term in the likelihood.

## `ols()` and `Glm()`

[`ols()`](https://rdrr.io/pkg/rms/man/ols.html) is the linear model. Its
table has the layout of an [`lm()`](https://rdrr.io/r/stats/lm.html)
table, with t tests on the residual degrees of freedom, and a hierarchy
of [`ols()`](https://rdrr.io/pkg/rms/man/ols.html) fits is compared by
the F test of the change, as for
[`lm()`](https://rdrr.io/r/stats/lm.html).

``` r

fit_ols_lin <- ols(bmi ~ age + npreg, data = pima)
fit_ols <- ols(bmi ~ rcs(age, 4) + npreg, data = pima)

table_regression(
  list(Linear = fit_ols_lin, Spline = fit_ols),
  nested = TRUE,
  show_columns = c("b", "p")
)
#> Hierarchical linear regression: bmi
#> 
#>                   Linear              Spline         
#>                ─────────────  ────────────────────── 
#>  Variable    │   B       p           B           p   
#> ─────────────┼───────────────────────────────────────
#>  (Intercept) │  31.04  <.001   10.29            .071 
#>  age         │   0.07   .041    0.95           <.001 
#>  npreg       │  -0.14   .248   -0.23            .058 
#>  age'        │                 -7.07            .011 
#>  age''       │                 10.49            .019 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 532            532                    
#>  R²          │   0.01           0.05                 
#>  ΔR²         │    –            +0.04                 
#>  F-change    │    –            11.28 (2, 527)        
#>  p (change)  │    –             <.001                
#> 
#> Note. Linear regression models. Std. errors: classical (OLS).
```

The body mass index is not linear in age either: F = 11.28 on 2 and 527
degrees of freedom, the value `anova(fit_ols)` gives for the nonlinear
terms.

[`Glm()`](https://rdrr.io/pkg/rms/man/Glm.html) is
[`glm()`](https://rdrr.io/r/stats/glm.html) with the formula language of
rms. A Poisson model of the number of pregnancies, with incidence rate
ratios:

``` r

fit_G <- Glm(npreg ~ age + bmi, data = pima, family = poisson)
table_regression(fit_G, exponentiate = TRUE)
#> Poisson regression: npreg
#> 
#>  Variable      │   IRR     SE      95% CI       p   
#> ───────────────┼────────────────────────────────────
#>  (Intercept)   │    0.85  0.11  [0.65, 1.11]   .225 
#>  age           │    1.04  0.00  [1.04, 1.05]  <.001 
#>  bmi           │    1.00  0.00  [0.99, 1.01]   .698 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │  532                               
#>  R² (McFadden) │    0.19                            
#>  AIC           │ 2460.1                             
#> 
#> Note. Poisson regression. Coefficients exponentiated and displayed as IRR; SE on the IRR scale (delta method); CI bounds exponentiated (asymmetric). The (Intercept) row estimates the baseline rate. Std. errors: classical (Fisher information). IRR = incidence rate ratio; CI = confidence interval; AIC = Akaike information criterion.
```

## `cph()`

The Cox model of rms takes the same spline terms. The example uses the
312 randomized patients of the Mayo Clinic trial in primary biliary
cholangitis
([`survival::pbc`](https://rdrr.io/pkg/survival/man/pbc.html)), with
death as the event.

``` r

pbc2 <- subset(pbc, !is.na(trt))
pbc2$death <- as.integer(pbc2$status == 2)
pbc2$edema <- factor(pbc2$edema)
ddp <- datadist(pbc2)
options(datadist = "ddp")

fit_cph <- cph(
  Surv(time, death) ~ rcs(age, 4) + log(bili) + albumin + edema,
  data = pbc2, x = TRUE, y = TRUE
)
table_regression(fit_cph, exponentiate = TRUE)
#> Cox proportional hazards regression: Surv(time, death)
#> 
#>  Variable   │   HR      SE      95% CI       p   
#> ────────────┼────────────────────────────────────
#>  age        │    1.03  0.05  [0.94, 1.13]   .535 
#>  age'       │    1.06  0.14  [0.83, 1.36]   .638 
#>  age''      │    0.78  0.32  [0.35, 1.74]   .542 
#>  log(bili)  │    2.56  0.26  [2.11, 3.12]  <.001 
#>  albumin    │    0.39  0.09  [0.24, 0.62]  <.001 
#>  edema:     │                                    
#>    0 (ref.) │     –     –         –         –    
#>    0.5      │    1.20  0.33  [0.70, 2.07]   .504 
#>    1        │    3.05  0.93  [1.67, 5.56]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n          │  312                               
#>  N events   │  125                               
#>  AIC        │ 1100.4                             
#> 
#> Note. Cox proportional hazards regression. Coefficients exponentiated and displayed as HR; SE on the HR scale (delta method); CI bounds exponentiated (asymmetric). Std. errors: classical (Fisher information). HR = hazard ratio; CI = confidence interval; AIC = Akaike information criterion.
```

The row of bilirubin is labelled `log(bili)`, the term of the formula,
where rms’s own printout says `bili`. Its hazard ratio is per unit of
log bilirubin, that is for a bilirubin multiplied by 2.72.

Age is close to linear here: `anova(fit_cph)` gives a Wald χ² of 0.94 on
2 degrees of freedom for its nonlinear terms, p = .63. A single linear
term for age is then the model to report.

[*Survival regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-survival.md)
covers the Cox table itself, the cluster-robust variance of
[`cph()`](https://rdrr.io/pkg/rms/man/cph.html) and the absolute
effects, which are computed for
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) fits
only.

## What spicy refuses for rms fits

A request the fit cannot honor is an explained error.

- Fitters outside the four above:
  [`orm()`](https://rdrr.io/pkg/rms/man/orm.html),
  [`psm()`](https://rdrr.io/pkg/rms/man/psm.html),
  [`Gls()`](https://rdrr.io/pkg/rms/man/Gls.html),
  [`Rq()`](https://rdrr.io/pkg/rms/man/Rq.html) and a formula-less
  [`lrm.fit()`](https://rdrr.io/pkg/rms/man/lrm.fit.html). The message
  names the supported twin where there is one,
  [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html)
  for [`psm()`](https://rdrr.io/pkg/rms/man/psm.html).
- The term-level test columns, `"partial_chi2"` and `"partial_f"`. The
  joint test of a predictor goes through `nested = TRUE`, as above, or
  through [`anova()`](https://rdrr.io/r/stats/anova.html) of rms.
- `"concordance"` in `show_fit_stats`, defined for
  [`coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) fits. The
  indexes of an rms fit are in `fit$stats`.
- The RMST and risk-difference columns of a Cox model, and average
  marginal effects for [`cph()`](https://rdrr.io/pkg/rms/man/cph.html).
- `standardized`. Average marginal effects compare predictors on a
  common scale for [`lrm()`](https://rdrr.io/pkg/rms/man/lrm.html),
  [`ols()`](https://rdrr.io/pkg/rms/man/ols.html) and
  [`Glm()`](https://rdrr.io/pkg/rms/man/Glm.html).
- Every `vcov` but `"classical"` and `"CR0"`.

``` r

table_regression(orm(glu ~ age + bmi, data = pima))
#> Error in `table_regression()`:
#> ! Some `models` are not supported by `table_regression()`.
#> Position 1: `orm` – not a supported model class. If support would be useful, please open an issue: https://github.com/amaltawfik/spicy/issues
#> ℹ Supported model classes: lm, glm, negbin, rlm, nls, lm_robust, iv_robust, ivreg, tobit, rq, fixest, lmerMod, glmerMod, glmmTMB, lme, gls, polr, clm, clmm, multinom, mlogit, zeroinfl, hurdle, coxph, survreg, cph, flexsurvreg, svyglm, svyolr, svycoxph, geeglm, gam, betareg, selection, ols, lrm, Glm, stanreg, brmsfit. `table_regression_models()` lists them with their engines.
#> ℹ `fallback = "broom"` or `fallback = "parameters"` builds the rows from what `broom::tidy()` or `parameters::model_parameters()` returns.
```

## Presentation-ready output

Everything above renders through the rich output engines. The three
models of age as a `gt` table:

``` r

pkgdown_dark_gt(
  table_regression(
    list(Linear = fit_lin, Categories = fit_cat, Spline = fit_rcs),
    show_columns = c("b", "ci", "p"),
    show_fit_stats = c("nobs", "pseudo_r2_nagelkerke", "aic"),
    output = "gt"
  )
)
```

[TABLE]

## References

- Harrell, F. E. (2015). *Regression modeling strategies: With
  applications to linear models, logistic and ordinal regression, and
  survival analysis* (2nd ed.). Springer.
- Nagelkerke, N. J. D. (1991). A note on a general definition of the
  coefficient of determination. *Biometrika*, 78(3), 691–692.
- Tjur, T. (2009). Coefficients of determination in logistic regression
  models—A new proposal: The coefficient of discrimination. *The
  American Statistician*, 63(4), 366–372.
- Venables, W. N., & Ripley, B. D. (2002). *Modern applied statistics
  with S* (4th ed.). Springer.
