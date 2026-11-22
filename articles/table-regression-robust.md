# Robust, IV, quantile and panel regression tables

``` r

library(spicy)
library(AER) # ivreg(), tobit(), bptest() and the three data sets
library(fixest) # feols(), fepois(), fenegbin()
library(quantreg) # rq()
library(estimatr) # lm_robust(), iv_robust()
```

This article covers robust and clustered standard errors on an
[`lm()`](https://rdrr.io/r/stats/lm.html), their counterparts in
[`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html),
fixed-effects models with fixest, instrumental variables, the tobit
model and quantile regression. The companion article [*Publication-ready
regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
covers the shared mechanics (output engines, labels,
[`tidy()`](https://generics.r-lib.org/reference/tidy.html)), and the
class-by-class map is [*Supported
models*](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.md).

## Two kinds of trouble

The estimators of this article answer two different problems.

In the first, the coefficients of least squares are fine and their
standard errors are not. The errors have unequal variances, or the
observations come in groups whose members resemble each other. A robust
variance repairs the standard errors, the intervals and the tests, and
leaves the coefficients as they are.

In the second, the coefficient itself is in question. A predictor is
correlated with what the model leaves out, the outcome is censored, or
the effect differs between the bottom and the top of the distribution.
The answer is another estimator: fixed effects, instrumental variables,
tobit, quantile regression.

A table has to say which estimator and which variance it reports.
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
writes both under the table, in every case below.

## The data

Three data sets of the AER package carry the examples. `CPS1988` is a
cross-section of 28,155 employed American men, with wage, education,
experience and ethnicity. `Fatalities` is a panel of the 48 contiguous
states from 1982 to 1988, with traffic deaths and the tax on beer.
`PSID1976` holds 753 married women, their hours of paid work, and for
the 428 who work, their wage.

``` r

data("CPS1988", package = "AER")
data("Fatalities", package = "AER")
data("PSID1976", package = "AER")

Fatalities$frate <- with(Fatalities, fatal / pop * 10000) # deaths per 10,000
PSID1976$nwincome <- with(PSID1976, (fincome - hours * wage) / 1000)
working <- subset(PSID1976, participation == "yes")
```

## Heteroskedasticity-robust standard errors

A wage equation on the CPS: the log wage on experience, its square,
education and ethnicity.

``` r

fit_wage <- lm(
  log(wage) ~ experience + I(experience^2) + education + ethnicity,
  data = CPS1988
)
bptest(fit_wage)
#> 
#>  studentized Breusch-Pagan test
#> 
#> data:  fit_wage
#> BP = 615.86, df = 4, p-value < 2.2e-16
```

The Breusch-Pagan test leaves no doubt about the unequal variances.
`vcov =` asks for a robust estimator, and a list of fits with a list of
estimators puts the two sets of standard errors side by side.

``` r

table_regression(
  list(Classical = fit_wage, HC3 = fit_wage),
  vcov = list("classical", "HC3"),
  show_columns = c("b", "se", "ci"),
  digits = 3
)
#> Linear regression comparison: log(wage)
#> 
#>                                Classical                              HC3                 
#>                    ──────────────────────────────────  ────────────────────────────────── 
#>  Variable        │     B       SE         95% CI           B       SE         95% CI      
#> ─────────────────┼────────────────────────────────────────────────────────────────────────
#>  (Intercept)     │     4.321  0.019  [ 4.284,  4.359]      4.321  0.021  [ 4.281,  4.362] 
#>  experience      │     0.077  0.001  [ 0.076,  0.079]      0.077  0.001  [ 0.075,  0.079] 
#>  I(experience^2) │    -0.001  0.000  [-0.001, -0.001]     -0.001  0.000  [-0.001, -0.001] 
#>  education       │     0.086  0.001  [ 0.083,  0.088]      0.086  0.001  [ 0.083,  0.088] 
#>  ethnicity:      │                                                                        
#>    cauc (ref.)   │      –      –            –               –      –            –         
#>    afam          │    -0.243  0.013  [-0.269, -0.218]     -0.243  0.013  [-0.269, -0.218] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 28155                               28155                              
#>  R²              │     0.33                                0.33                           
#>  Adj. R²         │     0.33                                0.33                           
#> 
#> Note. Linear regression models. Std. errors: Classical: classical (OLS), HC3: heteroskedasticity-robust (HC3). CI = confidence interval.
```

The coefficients are the same in both columns. The robust standard
errors are larger, by 24% for the square of experience and by 8% for
education.

`"HC0"` to `"HC3"` differ by how they weight the squared residuals when
the sample is small. Zeileis (2004, p. 4) reports the conclusion of the
simulations of Long and Ervin: HC3 “provides the best performance in
small samples as it gives less weight to influential observations”. With
28,155 rows the four estimators agree to three digits, and the choice no
longer matters.
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
keeps the classical variance as its default and computes a robust one
only on request.

## `vcov =` or `lm_robust()`

[`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html)
chooses the variance at fit time. The table reports the standard errors
of the fit, and they are those that `vcov =` gives on an
[`lm()`](https://rdrr.io/r/stats/lm.html).

``` r

fit_wage_rob <- lm_robust(
  log(wage) ~ experience + I(experience^2) + education + ethnicity,
  data = CPS1988, se_type = "HC3"
)

table_regression(
  list("lm(), HC3" = fit_wage, "lm_robust(), HC3" = fit_wage_rob),
  vcov = list("HC3", "classical"),
  show_columns = c("b", "se", "p"),
  digits = 4
)
#> Regression comparison: log(wage)
#> 
#>                            lm(), HC3              lm_robust(), HC3      
#>                    ─────────────────────────  ───────────────────────── 
#>  Variable        │     B         SE      p        B         SE      p   
#> ─────────────────┼──────────────────────────────────────────────────────
#>  (Intercept)     │     4.3214  0.0206  <.001      4.3214  0.0206  <.001 
#>  experience      │     0.0775  0.0010  <.001      0.0775  0.0010  <.001 
#>  I(experience^2) │    -0.0013  0.0000  <.001     -0.0013  0.0000  <.001 
#>  education       │     0.0857  0.0014  <.001      0.0857  0.0014  <.001 
#>  ethnicity:      │                                                      
#>    cauc (ref.)   │      –       –       –          –       –       –    
#>    afam          │    -0.2434  0.0131  <.001     -0.2434  0.0131  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 28155                      28155                     
#>  R²              │     0.33                       0.33                  
#>  Adj. R²         │     0.33                       0.33                  
#> 
#> Note. lm(), HC3: linear regression; lm_robust(), HC3: linear regression (robust SE). Std. errors: heteroskedasticity-robust (HC3).
```

For a fit that carries its own variance, `"classical"` means the
variance of the fit. One difference deserves attention: without
`se_type`, estimatr computes HC2, where an
[`lm()`](https://rdrr.io/r/stats/lm.html) table is classical. The note
says which one is in the table.

``` r

table_regression(lm_robust(frate ~ beertax, data = Fatalities))
#> Linear regression (robust SE): frate
#> 
#>  Variable    │   B      SE      95% CI       p   
#> ─────────────┼───────────────────────────────────
#>  (Intercept) │   1.85  0.05  [1.76, 1.95]  <.001 
#>  beertax     │   0.36  0.05  [0.26, 0.47]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 336                               
#>  R²          │   0.09                            
#>  Adj. R²     │   0.09                            
#> 
#> Note. Linear regression (robust SE). Std. errors: heteroskedasticity-robust (HC2). CI = confidence interval.
```

## Clustered standard errors

The 336 rows of `Fatalities` are 48 states observed seven times. Two
years of the same state resemble each other more than two states do, and
a variance that treats the rows as independent is too small. A
cluster-robust variance takes the state as the unit.

``` r

pooled <- lm(frate ~ beertax, data = Fatalities)

table_regression(
  list(Classical = pooled, HC3 = pooled, CR1 = pooled, CR2 = pooled),
  vcov = list("classical", "HC3", "CR1", "CR2"),
  cluster = list(NULL, NULL, ~state, ~state),
  show_columns = c("b", "se", "ci", "p")
)
#> Linear regression comparison: frate
#> 
#>                            Classical                             HC3                     CR1      
#>                ─────────────────────────────────  ─────────────────────────────────  ──────────── 
#>  Variable    │   B      SE      95% CI       p      B      SE      95% CI       p      B      SE  
#> ─────────────┼────────────────────────────────────────────────────────────────────────────────────
#>  (Intercept) │   1.85  0.04  [1.77, 1.94]  <.001    1.85  0.05  [1.76, 1.95]  <.001    1.85  0.12 
#>  beertax     │   0.36  0.06  [0.24, 0.49]  <.001    0.36  0.05  [0.26, 0.47]  <.001    0.36  0.12 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 336                                336                                336          
#>  R²          │   0.09                               0.09                               0.09       
#>  Adj. R²     │   0.09                               0.09                               0.09       
#> 
#>                        CR1                         CR2                
#>                ───────────────────  ───────────────────────────────── 
#>  Variable    │  95% CI (B)   p (B)    B      SE      95% CI       p   
#> ─────────────┼────────────────────────────────────────────────────────
#>  (Intercept) │ [1.61, 2.09]  <.001    1.85  0.12  [1.61, 2.10]  <.001 
#>  beertax     │ [0.08, 0.65]   .019    0.36  0.13  [0.03, 0.69]   .036 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │                      336                               
#>  R²          │                        0.09                            
#>  Adj. R²     │                        0.09                            
#> 
#> Note. Linear regression models. Std. errors: Classical: classical (OLS); HC3: heteroskedasticity-robust (HC3); CR1: cluster-robust (CR1), clusters by state; CR2: cluster-robust (CR2), clusters by state. CR1, CR2: P-values: Satterthwaite t-test, cluster-robust df. CI = confidence interval.
```

The coefficient of the beer tax keeps its value, and its p-value moves
from below .001 to .019 under CR1 and .036 under CR2. Correcting for
heteroskedasticity alone (HC3) changes little: the trouble here is the
dependence within states.

`"CR1"` is the sandwich with the correction G / (G − 1) for G clusters,
`"CR1S"` the variant of Stata, and `"CR2"` the adjustment of Bell and
McCaffrey (2002). Under `"CR1"` and `"CR2"` the tests are t tests with
Satterthwaite degrees of freedom, computed for each coefficient.
[`lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html)
with `clusters =` gives the same numbers:

``` r

table_regression(
  list(
    "lm(), CR2" = pooled,
    "lm_robust(), CR2" = lm_robust(
      frate ~ beertax,
      data = Fatalities, clusters = state, se_type = "CR2"
    )
  ),
  vcov = list("CR2", "classical"),
  cluster = list(~state, NULL),
  show_columns = c("b", "se", "df", "p"),
  digits = 3
)
#> Regression comparison: frate
#> 
#>                         lm(), CR2                lm_robust(), CR2       
#>                ───────────────────────────  ─────────────────────────── 
#>  Variable    │    B      SE     df     p       B      SE     df     p   
#> ─────────────┼──────────────────────────────────────────────────────────
#>  (Intercept) │   1.853  0.120  26.6  <.001    1.853  0.120  26.6  <.001 
#>  beertax     │   0.365  0.130   5.2   .036    0.365  0.130   5.2   .036 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 336                          336                         
#>  R²          │   0.09                         0.09                      
#>  Adj. R²     │   0.09                         0.09                      
#> 
#> Note. lm(), CR2: linear regression; lm_robust(), CR2: linear regression (robust SE). Std. errors: cluster-robust (CR2), clusters by state. P-values: Satterthwaite t-test, cluster-robust df.
```

The `df` column prints the degrees of freedom of each test.

## Few clusters

A cluster-robust variance is justified when the clusters are many.
Cameron, Gelbach and Miller (2008) show that the usual tests reject too
often with few clusters, five to thirty in their study. The number of
clusters is not the whole story either. Bell and McCaffrey (2002,
p. 169) note that the bias is large “when the number of PSUs is small or
a coefficient depends primarily on data from a small number of PSUs”. In
the previous table, the beer tax has 5.2 degrees of freedom under CR2,
with 48 states. Imbens and Kolesár (2016) trace such losses to a
regressor whose distribution is skewed across clusters.

The CPS has four regions. Clustering a wage equation on them shows what
four clusters can and cannot support. The example draws 2,000 men, since
CR2 forms one matrix per cluster and four clusters of 7,000 rows take
minutes.

``` r

set.seed(1988)
cps <- CPS1988[sample(nrow(CPS1988), 2000), ]
fit_cps <- lm(
  log(wage) ~ experience + I(experience^2) + education + ethnicity,
  data = cps
)

table_regression(
  list(Classical = fit_cps, CR1 = fit_cps, CR1S = fit_cps, CR2 = fit_cps),
  vcov = list("classical", "CR1", "CR1S", "CR2"),
  cluster = list(NULL, ~region, ~region, ~region),
  keep = c("education", "ethnicity"),
  show_columns = c("b", "se", "ci", "p"),
  digits = 3
)
#> Linear regression comparison: log(wage)
#> 
#>                                 Classical                                    CR1                       CR1S   
#>                  ────────────────────────────────────────  ────────────────────────────────────────  ──────── 
#>  Variable      │    B       SE         95% CI         p       B       SE         95% CI         p       B     
#> ───────────────┼──────────────────────────────────────────────────────────────────────────────────────────────
#>  (Intercept)   │    4.308  0.075  [ 4.162,  4.455]  <.001     4.308  0.059  [ 4.116,  4.501]  <.001     4.308 
#>  education     │    0.088  0.005  [ 0.078,  0.098]  <.001     0.088  0.004  [ 0.074,  0.101]  <.001     0.088 
#>  ethnicity:    │                                                                                              
#>    cauc (ref.) │     –      –            –           –         –      –            –           –         –    
#>    afam        │   -0.217  0.054  [-0.322, -0.111]  <.001    -0.217  0.055  [-0.420, -0.013]   .044    -0.217 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │ 2000                                      2000                                      2000     
#>  R²            │    0.30                                      0.30                                      0.30  
#>  Adj. R²       │    0.30                                      0.30                                      0.30  
#> 
#>                               CR1S                                  CR2                   
#>                  ───────────────────────────────  ─────────────────────────────────────── 
#>  Variable      │ SE (B)     95% CI (B)     p (B)     B       SE        95% CI         p   
#> ───────────────┼──────────────────────────────────────────────────────────────────────────
#>  (Intercept)   │  0.059  [ 4.120,  4.497]  <.001     4.308  0.063  [ 4.098, 4.519]  <.001 
#>  education     │  0.004  [ 0.075,  0.101]  <.001     0.088  0.004  [ 0.073, 0.103]  <.001 
#>  ethnicity:    │                                                                          
#>    cauc (ref.) │   –            –           –         –      –            –          –    
#>    afam        │  0.055  [-0.391, -0.042]   .029    -0.217  0.071  [-0.539, 0.106]   .099 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │                                  2000                                    
#>  R²            │                                     0.30                                 
#>  Adj. R²       │                                     0.30                                 
#> 
#> Note. Linear regression models. Std. errors: Classical: classical (OLS); CR1: cluster-robust (CR1), clusters by region; CR1S: cluster-robust (CR1S, Stata vce(cluster), t(G−1)), clusters by region; CR2: cluster-robust (CR2), clusters by region. CR1, CR2: P-values: Satterthwaite t-test, cluster-robust df. CI = confidence interval.
```

For the wage gap of African-American men, the three clustered columns
give p = .044, .029 and .099. They differ by their standard error and by
their reference distribution: 2.4 Satterthwaite degrees of freedom under
CR1, G − 1 = 3 under CR1S, 1.9 under CR2. Imbens and Kolesár (2016) and
Pustejovsky and Tipton (2018) recommend CR2 with its degrees of freedom
as the routine choice. With four clusters, that choice rests on fewer
than two degrees of freedom: these data cannot settle the question at
the level of the region.

## Panel data: the within estimator

In the pooled regression above, states with a higher beer tax have more
traffic deaths. States differ in many ways that the model ignores, and a
panel allows a comparison of each state with itself. Fixed effects for
state and year do that, either as dummy variables in an
[`lm()`](https://rdrr.io/r/stats/lm.html) or absorbed by
[`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html).

``` r

lsdv <- lm(frate ~ beertax + state + year, data = Fatalities)
within <- feols(frate ~ beertax | state + year, data = Fatalities)

table_regression(
  list(Pooled = pooled, "Dummies (LSDV)" = lsdv, "Within (feols)" = within),
  keep = "beertax",
  show_intercept = FALSE
)
#> Linear regression comparison: frate
#> 
#>                         Pooled           Dummies (LSDV)       Within (feols)    
#>                   ───────────────────  ───────────────────  ─────────────────── 
#>  Variable       │   B      SE     p       B      SE    p       B      SE    p   
#> ────────────────┼───────────────────────────────────────────────────────────────
#>  beertax        │   0.36  0.06  <.001    -0.64  0.20  .001    -0.64  0.20  .001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Fixed effects: │                                                               
#>    state        │                                              Yes              
#>    year         │                                              Yes              
#>  n              │ 336                   336                  336                
#>  R²             │   0.09                  0.91                 0.91             
#>  Adj. R²        │   0.09                  0.89                 0.89             
#>  R² (within)    │    –                     –                   0.04             
#>  AIC            │ 548.1                -118.1               -118.1              
#> 
#> Note. Linear regression models. Std. errors: classical (OLS). AIC = Akaike information criterion.
```

The sign reverses: 0.36 in the pooled model, −0.64 within states. The
dummy-variable model and
[`feols()`](https://lrberge.github.io/fixest/reference/feols.html) are
one estimator computed two ways, and they agree on the coefficient and
on its classical standard error. The fixest column carries a block that
says which effects were absorbed. `keep` selects the rows of the
predictors, and `show_intercept` governs the intercept apart.

The price of the within estimator is known. Because the state effects
“account for all the higher-level variance, no other variables measured
at the higher level can be identified” (Bell, Fairbrother & Jones, 2019,
p. 1060): a characteristic of a state that does not change over the
seven years has no coefficient.

`show_fit_stats` adds the statistics specific to these models, the
number of groups and the within R².

``` r

table_regression(
  within,
  show_fit_stats = c("fixed_effects", "nobs", "n_groups", "r2", "within_r2")
)
#> Linear regression (fixed effects): frate
#> 
#>  Variable       │   B      SE       95% CI       p   
#> ────────────────┼────────────────────────────────────
#>  beertax        │  -0.64  0.20  [-1.03, -0.25]  .001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Fixed effects: │                                    
#>    state        │   Yes                              
#>    year         │   Yes                              
#>  n              │ 336                                
#>  N (state)      │  48                                
#>  N (year)       │   7                                
#>  R²             │   0.91                             
#>  R² (within)    │   0.04                             
#> 
#> Note. Linear regression (fixed effects). Std. errors: classical (OLS). CI = confidence interval.
```

The model explains 91% of the variance of the death rate, almost all of
it through the fixed effects: the beer tax accounts for 4% of the
variation left within states and years.

## The variance of a fixest fit

Like estimatr, fixest sets the variance in the fit, with `vcov =` or
`cluster =` in
[`feols()`](https://lrberge.github.io/fixest/reference/feols.html). The
table reports it and names it.

``` r

within_cl <- feols(
  frate ~ beertax | state + year,
  data = Fatalities, cluster = ~state
)
within_tw <- feols(
  frate ~ beertax | state + year,
  data = Fatalities, vcov = "twoway"
)
within_dk <- feols(
  frate ~ beertax | state + year,
  data = Fatalities, panel.id = ~ state + year, vcov = "DK"
)

table_regression(
  list(
    IID = within, "Cluster state" = within_cl,
    "Two-way" = within_tw, "Driscoll-Kraay" = within_dk
  ),
  show_columns = c("b", "se", "p")
)
#> Linear regression (fixed effects) comparison: frate
#> 
#>                           IID             Cluster state           Two-way          Driscoll-Kraay    
#>                   ───────────────────  ───────────────────  ───────────────────  ─────────────────── 
#>  Variable       │    B      SE    p       B      SE    p       B      SE    p       B      SE    p   
#> ────────────────┼────────────────────────────────────────────────────────────────────────────────────
#>  beertax        │   -0.64  0.20  .001    -0.64  0.36  .080    -0.64  0.32  .092    -0.64  0.18  .012 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Fixed effects: │                                                                                    
#>    state        │    Yes                  Yes                  Yes                  Yes              
#>    year         │    Yes                  Yes                  Yes                  Yes              
#>  n              │  336                  336                  336                  336                
#>  R²             │    0.91                 0.91                 0.91                 0.91             
#>  R² (within)    │    0.04                 0.04                 0.04                 0.04             
#>  AIC            │ -118.1               -118.1               -118.1               -118.1              
#> 
#> Note. Linear regression (fixed effects) models. Std. errors: IID: classical (OLS); Cluster state: cluster-robust, clusters by state; Two-way: cluster-robust, clusters by state & year; Driscoll-Kraay: Driscoll-Kraay (L=1). AIC = Akaike information criterion.
```

The coefficient is the same in the four columns, and its p-value is
.001, .080, .092 and .012. The estimators of
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
are refused for a fixest fit, with the way to obtain them in fixest:

``` r

table_regression(within, vcov = "HC3")
#> Error in `table_regression()`:
#> ! `vcov = "HC3"` is not available for `fixest` models.
#> ℹ fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
#> ℹ Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
```

Clustered standard errors of the same model do not agree to the last
digit across software, because the small-sample corrections differ. The
dummy-variable [`lm()`](https://rdrr.io/r/stats/lm.html) under three
estimators, next to fixest:

``` r

table_regression(
  list(
    "LSDV, CR1" = lsdv, "LSDV, CR1S" = lsdv, "LSDV, CR2" = lsdv,
    "feols, cluster" = within_cl
  ),
  vcov = list("CR1", "CR1S", "CR2", "classical"),
  cluster = list(~state, ~state, ~state, NULL),
  keep = "beertax",
  show_columns = c("b", "se", "ci", "p"),
  digits = 3
)
#> Linear regression comparison: frate
#> 
#>                                  LSDV, CR1                               LSDV, CR1S                 LSDV, CR2 
#>                   ───────────────────────────────────────  ───────────────────────────────────────  ───────── 
#>  Variable       │    B       SE        95% CI         p       B       SE        95% CI         p        B     
#> ────────────────┼─────────────────────────────────────────────────────────────────────────────────────────────
#>  (Intercept)    │    3.511  0.590  [ 2.165, 4.858]  <.001     3.511  0.644  [ 2.216, 4.806]  <.001      3.511 
#>  beertax        │   -0.640  0.353  [-1.445, 0.166]   .105    -0.640  0.386  [-1.416, 0.136]   .104     -0.640 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Fixed effects: │                                                                                             
#>    state        │                                                                                             
#>    year         │                                                                                             
#>  n              │  336                                      336                                       336     
#>  R²             │    0.91                                     0.91                                      0.91  
#>  Adj. R²        │    0.89                                     0.89                                      0.89  
#>  R² (within)    │     –                                        –                                         –    
#>  AIC            │ -118.1                                   -118.1                                    -118.1   
#> 
#>                             LSDV, CR2                         feols, cluster             
#>                   ──────────────────────────────  ────────────────────────────────────── 
#>  Variable       │ SE (B)    95% CI (B)     p (B)     B       SE        95% CI        p   
#> ────────────────┼────────────────────────────────────────────────────────────────────────
#>  (Intercept)    │  0.625  [ 2.045, 4.977]  <.001                                         
#>  beertax        │  0.375  [-1.517, 0.237]   .129    -0.640  0.357  [-1.358, 0.078]  .080 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Fixed effects: │                                                                        
#>    state        │                                    Yes                                 
#>    year         │                                    Yes                                 
#>  n              │                                  336                                   
#>  R²             │                                    0.91                                
#>  Adj. R²        │                                    0.89                                
#>  R² (within)    │                                    0.04                                
#>  AIC            │                                 -118.1                                 
#> 
#> Note. Linear regression models. Std. errors: LSDV, CR1: cluster-robust (CR1), clusters by state; LSDV, CR1S: cluster-robust (CR1S, Stata vce(cluster), t(G−1)), clusters by state; LSDV, CR2: cluster-robust (CR2), clusters by state; feols, cluster: cluster-robust, clusters by state. LSDV, CR1; LSDV, CR2: P-values: Satterthwaite t-test, cluster-robust df. CI = confidence interval; AIC = Akaike information criterion.
```

fixest counts fewer parameters than Stata’s formula applied to the
dummy-variable model, since it leaves out the fixed effects nested in
the clusters
([`?fixest::ssc`](https://lrberge.github.io/fixest/reference/ssc.html)).
With `ssc(K.fixef = "full")` it returns the CR1S column. The four
p-values lie between .08 and .13. The classical one was .001.

## Fixed-effects count models

Deaths are counts, and the population at risk differs widely between
states. A Poisson model with the population as exposure and the same two
fixed effects is
[`fepois()`](https://lrberge.github.io/fixest/reference/feglm.html), and
[`fenegbin()`](https://lrberge.github.io/fixest/reference/femlm.html)
relaxes the equality of mean and variance.

``` r

fp <- fepois(
  fatal ~ beertax | state + year,
  offset = ~ log(pop), data = Fatalities, cluster = ~state
)
fnb <- fenegbin(
  fatal ~ beertax | state + year,
  offset = ~ log(pop), data = Fatalities, cluster = ~state
)

table_regression(
  list(Poisson = fp, "Negative binomial" = fnb),
  exponentiate = TRUE,
  show_fit_stats = c("fixed_effects", "nobs", "theta", "pseudo_r2_mcfadden", "aic")
)
#> Regression comparison: fatal
#> 
#>                         Poisson         Negative binomial  
#>                   ───────────────────  ─────────────────── 
#>  Variable       │   IRR     SE    p      IRR     SE    p   
#> ────────────────┼──────────────────────────────────────────
#>  beertax        │    0.71  0.11  .027     0.77  0.10  .055 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Fixed effects: │                                          
#>    state        │    Yes                  Yes              
#>    year         │    Yes                  Yes              
#>  n              │  336                  336                
#>  θ (dispersion) │     –                 290.34             
#>  R² (McFadden)  │    0.98                 0.33             
#>  AIC            │ 4279.4               3644.8              
#> 
#> Note. Poisson: Poisson regression (fixed effects); Negative binomial: negative-binomial regression (fixed effects). Coefficients exponentiated and displayed as IRR; SE on the IRR scale (delta method). Std. errors: cluster-robust, clusters by state. IRR = incidence rate ratio; AIC = Akaike information criterion.
```

`exponentiate = TRUE` gives incidence rate ratios: one more unit of the
tax on a case of beer goes with a death rate multiplied by 0.71 in the
Poisson model, 95% CI \[0.52, 0.96\].

Average marginal effects are available for
[`feols()`](https://lrberge.github.io/fixest/reference/feols.html) and
for a GLM without fixed effects. For a GLM with fixed effects they are
refused, and the message gives the reason.

``` r

table_regression(fp, show_columns = c("b", "ame"))
#> Error in `table_regression()`:
#> ! AME columns are not available for `fixest`.
#> ℹ Model 1: marginaleffects, which computes the AME, refuses a fixest GLM with fixed effects (`feglm()`, `fepois()`, `femlm()`, `fenegbin()`, whatever the family): it cannot take into account the uncertainty of the fixed effects, which fixest does not estimate. The AME is available for `feols()` and for a GLM without fixed effects.
#> ℹ Drop the AME token(s) from `show_columns`.
```

## Instrumental variables

The return to a year of education is the textbook case of a coefficient
in doubt: ability raises both schooling and wages, and least squares
credits schooling with both. An instrument is a variable that moves
education and has no other path to the wage. The example takes the
education of the father and of the mother, for the 428 working women of
the PSID.

Of the assumptions behind an instrument, one can be checked. For Gelman,
Hill and Vehtari (2020, p. 431), the correlation of the instrument with
the treatment “is the only assumption that can be directly tested”, and
the first-stage model “should be examined closely to assess the strength
of the instrument”. The first stage is an ordinary
[`lm()`](https://rdrr.io/r/stats/lm.html), and its table comes first.

``` r

first <- lm(
  education ~ feducation + meducation + experience + I(experience^2),
  data = working
)
table_regression(
  first,
  vcov = "HC1",
  keep = c("feducation", "meducation"),
  digits = 3
)
#> Linear regression: education
#> 
#>  Variable    │    B      SE        95% CI        p   
#> ─────────────┼───────────────────────────────────────
#>  (Intercept) │   9.103  0.424  [8.269, 9.936]  <.001 
#>  feducation  │   0.190  0.032  [0.126, 0.253]  <.001 
#>  meducation  │   0.158  0.035  [0.088, 0.227]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 428                                   
#>  R²          │   0.21                                
#>  Adj. R²     │   0.20                                
#> 
#> Note. Linear regression. Std. errors: heteroskedasticity-robust (HC1). CI = confidence interval.
```

Both parents’ education predict the daughter’s. The tests of weak
instruments, of endogeneity and of overidentification have no place in
the table. They come from
[`summary()`](https://rdrr.io/r/base/summary.html) of the fit, and
`note_add` can carry them.

``` r

iv <- ivreg(
  log(wage) ~ education + experience + I(experience^2) |
    feducation + meducation + experience + I(experience^2),
  data = working
)
summary(iv, diagnostics = TRUE)$diagnostics
#>                  df1 df2  statistic      p-value
#> Weak instruments   2 423 55.4003004 4.268909e-22
#> Wu-Hausman         1 423  2.7925919 9.544055e-02
#> Sargan             1  NA  0.3780715 5.386372e-01
```

The second stage is not an [`lm()`](https://rdrr.io/r/stats/lm.html) on
the fitted values of the first: “the standard-error calculation is
complicated because we cannot simply look at the second regression in
isolation” (Gelman et al., 2020, p. 429).
[`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html),
[`estimatr::iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html)
and
[`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html)
do the calculation, and their fits go in one table with the
least-squares model.

``` r

ols_w <- lm(
  log(wage) ~ education + experience + I(experience^2),
  data = working
)
iv_rob <- iv_robust(
  log(wage) ~ education + experience + I(experience^2) |
    feducation + meducation + experience + I(experience^2),
  data = working, se_type = "HC1"
)
iv_fe <- feols(
  log(wage) ~ experience + I(experience^2) |
    education ~ feducation + meducation,
  data = working, vcov = "hetero"
)

table_regression(
  list(OLS = ols_w, "ivreg()" = iv, "iv_robust()" = iv_rob, "feols()" = iv_fe),
  vcov = list("HC1", "classical", "classical", "classical"),
  show_columns = c("b", "se", "p"),
  show_fit_stats = "nobs",
  digits = 3
)
#> Regression comparison: log(wage)
#> 
#>                             OLS                 ivreg()             iv_robust()             feols()        
#>                    ─────────────────────  ────────────────────  ────────────────────  ──────────────────── 
#>  Variable        │    B      SE      p       B      SE     p       B      SE     p       B      SE     p   
#> ─────────────────┼─────────────────────────────────────────────────────────────────────────────────────────
#>  (Intercept)     │  -0.522  0.202   .010    0.048  0.400  .904    0.048  0.430  .911    0.048  0.430  .911 
#>  education       │   0.107  0.013  <.001    0.061  0.031  .051    0.061  0.033  .066    0.061  0.033  .066 
#>  experience      │   0.042  0.015   .007    0.044  0.013  .001    0.044  0.016  .005    0.044  0.016  .005 
#>  I(experience^2) │  -0.001  0.000   .054   -0.001  0.000  .026   -0.001  0.000  .037   -0.001  0.000  .037 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 428                    428                   428                   428                  
#> 
#> Note. OLS: linear regression; ivreg(), feols(): IV regression (2SLS); iv_robust(): IV regression (robust SE). Std. errors: OLS, iv_robust(): heteroskedasticity-robust (HC1); ivreg(): classical (2SLS); feols(): heteroskedasticity-robust.
```

The return to a year of education falls from 0.107 to 0.061, and its
standard error more than doubles: an instrument uses only a part of the
variation of education. The three implementations give the same
coefficient.

An [`ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html) fit takes
`vcov = "HC0"` and `"HC1"`, and the cluster-robust estimators `"CR0"` to
`"CR3"` and `"CR1S"` with a `cluster`. The values are those of
[`iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html).

``` r

table_regression(
  list("ivreg(), HC1" = iv, "iv_robust(), HC1" = iv_rob),
  vcov = list("HC1", "classical"),
  show_columns = c("b", "se", "p"),
  show_fit_stats = "nobs",
  digits = 3
)
#> Regression comparison: log(wage)
#> 
#>                        ivreg(), HC1        iv_robust(), HC1   
#>                    ────────────────────  ──────────────────── 
#>  Variable        │    B      SE     p       B      SE     p   
#> ─────────────────┼────────────────────────────────────────────
#>  (Intercept)     │   0.048  0.430  .911    0.048  0.430  .911 
#>  education       │   0.061  0.033  .066    0.061  0.033  .066 
#>  experience      │   0.044  0.016  .005    0.044  0.016  .005 
#>  I(experience^2) │  -0.001  0.000  .037   -0.001  0.000  .037 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 428                   428                  
#> 
#> Note. ivreg(), HC1: IV regression (2SLS); iv_robust(), HC1: IV regression (robust SE). Std. errors: heteroskedasticity-robust (HC1).
```

`"HC2"` to `"HC5"` are refused. They weight each residual by its
leverage, which AER and estimatr do not define the same way for
two-stage least squares, and the error says so.

``` r

table_regression(iv, vcov = "HC3")
#> Error in `table_regression()`:
#> ! `vcov = "HC3"` is not available for `ivreg` models.
#> ℹ HC2-HC5 weight each residual by a leverage, which two-stage least squares does not define uniquely: `AER::ivreg()` and `estimatr::iv_robust()` use different hat values. For the second, refit with `estimatr::iv_robust(..., se_type = "HC2")` or `se_type = "HC3"`.
#> ℹ This class supports: classical, HC0, HC1, CR0, CR1, CR2, CR3, CR1S. See ?table_regression_robust.
```

## Censored outcomes: tobit

Of the 753 women, 325 did not work for pay in 1975: their hours are
zero. Least squares on all the rows treats these zeros as ordinary
values. The tobit model posits a latent variable, here the desired
hours, that is observed only when it is positive (Gelman et al., 2020,
p. 284).

``` r

tob <- tobit(
  hours ~ nwincome + education + experience + I(experience^2) + age +
    youngkids + oldkids,
  data = PSID1976
)
ols_h <- lm(
  hours ~ nwincome + education + experience + I(experience^2) + age +
    youngkids + oldkids,
  data = PSID1976
)

table_regression(
  list("OLS (zeros included)" = ols_h, Tobit = tob),
  show_columns = c("b", "se", "p"),
  show_fit_stats = c("nobs", "sigma", "aic")
)
#> Regression comparison: hours
#> 
#>                     OLS (zeros included)            Tobit          
#>                    ───────────────────────  ────────────────────── 
#>  Variable        │    B        SE      p       B       SE      p   
#> ─────────────────┼─────────────────────────────────────────────────
#>  (Intercept)     │  1330.48  270.78  <.001   965.31  446.44   .031 
#>  nwincome        │    -3.45    2.54   .176    -8.81    4.46   .048 
#>  education       │    28.76   12.95   .027    80.65   21.58  <.001 
#>  experience      │    65.67    9.96  <.001   131.56   17.28  <.001 
#>  I(experience^2) │    -0.70    0.32   .031    -1.86    0.54  <.001 
#>  age             │   -30.51    4.36  <.001   -54.41    7.42  <.001 
#>  youngkids       │  -442.09   58.85  <.001  -894.02  111.88  <.001 
#>  oldkids         │   -32.78   23.18   .158   -16.22   38.64   .675 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │   753                     753                   
#>  σ̂               │   750.18                 1122.02                
#>  AIC             │ 12117.1                  7656.2                 
#> 
#> Note. OLS (zeros included): linear regression; Tobit: Tobit regression. Std. errors: OLS (zeros included): classical (OLS), Tobit: classical (Fisher information). AIC = Akaike information criterion.
```

The tobit coefficient of education is 80.65 hours per year of schooling,
against 28.76 by least squares. The two do not measure the same thing. A
tobit coefficient is a slope of the latent variable. The slope of the
observed hours is smaller, since part of the women stay at zero whatever
the change.

The same holds for the average marginal effects of a tobit fit: they are
average slopes of the latent variable, and the note under the table says
so.

``` r

table_regression(
  tob,
  show_columns = c("b", "ame", "ame_ci"),
  keep = c("education", "experience", "youngkids")
)
#> Tobit regression: hours
#> 
#>  Variable        │    B       AME          95% CI        
#> ─────────────────┼───────────────────────────────────────
#>  (Intercept)     │  965.31                               
#>  education       │   80.65    80.65  [   38.35,  122.95] 
#>  experience      │  131.56    91.93  [   76.26,  107.60] 
#>  I(experience^2) │   -1.86                               
#>  youngkids       │ -894.02  -894.02  [-1113.30, -674.75] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │  753                                  
#>  AIC             │ 7656.2                                
#> 
#> Note. Tobit regression. Std. errors: classical (Fisher information). AME of a continuous predictor: average slope (dY/dX). AME of a tobit model: computed on the latent (uncensored) outcome. AME = average marginal effect; CI = confidence interval; AIC = Akaike information criterion.
```

For education the AME repeats the coefficient, and for experience it
takes the squared term into account. `vcov = "CR1"` with a `cluster` is
the one robust estimator of a tobit fit.

## Quantile regression

Least squares models the mean of the outcome. Quantile regression models
a quantile, and several quantiles show whether a predictor acts in the
same way at the bottom and at the top of the distribution.
[`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) fits one
quantile at a time. A table of several quantiles takes a list of fits,
one per `tau`.

``` r

q10 <- rq(
  log(wage) ~ experience + I(experience^2) + education + ethnicity,
  tau = 0.10, data = CPS1988
)
q50 <- update(q10, tau = 0.50)
q90 <- update(q10, tau = 0.90)

table_regression(
  list(Mean = fit_wage, Q10 = q10, Median = q50, Q90 = q90),
  vcov = list("HC3", "classical", "classical", "classical"),
  show_columns = c("b", "se"),
  digits = 3
)
#> Regression comparison: log(wage)
#> 
#>                          Mean              Q10              Median             Q90        
#>                    ────────────────  ────────────────  ────────────────  ──────────────── 
#>  Variable        │     B       SE        B       SE        B       SE        B       SE   
#> ─────────────────┼────────────────────────────────────────────────────────────────────────
#>  (Intercept)     │     4.321  0.021      3.482  0.042      4.279  0.021      5.019  0.026 
#>  experience      │     0.077  0.001      0.102  0.002      0.076  0.001      0.056  0.001 
#>  I(experience^2) │    -0.001  0.000     -0.002  0.000     -0.001  0.000     -0.001  0.000 
#>  education       │     0.086  0.001      0.082  0.003      0.093  0.001      0.093  0.002 
#>  ethnicity:      │                                                                        
#>    cauc (ref.)   │      –      –          –      –          –      –          –      –    
#>    afam          │    -0.243  0.013     -0.283  0.028     -0.251  0.015     -0.207  0.017 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 28155             28155             28155             28155            
#>  R²              │     0.33               –                 –                 –           
#>  Adj. R²         │     0.33               –                 –                 –           
#> 
#> Note. Mean: linear regression; Q10: quantile regression (τ = 0.10); Median: quantile regression (τ = 0.50); Q90: quantile regression (τ = 0.90). Std. errors: Mean: heteroskedasticity-robust (HC3); Q10, Median, Q90: quantile sandwich (nid, Hall-Sheather).
```

The wage gap of African-American men is −0.283 at the first decile and
−0.207 at the ninth: it narrows toward the top. The return to experience
falls from 0.102 to 0.056. The return to education changes little, from
0.082 to 0.093. The standard error of the gap is nearly twice as large
at the first decile as at the median: outer quantiles are harder to
estimate (Harrell, 2015, p. 360).

The variance of a quantile regression has its own estimators, since it
depends on the density of the outcome around the quantile. The tokens
are those of quantreg. For an `rq` fit, `"classical"` is `"nid"`, the
sandwich that allows unequal densities.

``` r

q50_cps <- rq(
  log(wage) ~ experience + I(experience^2) + education + ethnicity,
  tau = 0.5, data = cps
)

set.seed(2)
table_regression(
  list(nid = q50_cps, iid = q50_cps, ker = q50_cps, bootstrap = q50_cps),
  vcov = list("nid", "iid", "ker", "bootstrap"),
  boot_n = 500,
  keep = c("education", "ethnicity"),
  show_columns = c("b", "se", "ci"),
  digits = 3
)
#> Quantile regression (τ = 0.50) comparison: log(wage)
#> 
#>                                 nid                                iid                       ker       
#>                  ─────────────────────────────────  ─────────────────────────────────  ─────────────── 
#>  Variable      │    B       SE         95% CI          B       SE         95% CI          B       SE   
#> ───────────────┼───────────────────────────────────────────────────────────────────────────────────────
#>  (Intercept)   │    4.282  0.086  [ 4.114,  4.451]     4.282  0.085  [ 4.116,  4.448]     4.282  0.093 
#>  education     │    0.093  0.005  [ 0.082,  0.103]     0.093  0.006  [ 0.082,  0.104]     0.093  0.006 
#>  ethnicity:    │                                                                                       
#>    cauc (ref.) │     –      –            –              –      –            –              –      –    
#>    afam        │   -0.190  0.057  [-0.301, -0.078]    -0.190  0.061  [-0.309, -0.070]    -0.190  0.067 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │ 2000                               2000                               2000            
#>  AIC           │ 3616.4                             3616.4                             3616.4          
#> 
#>                        ker                     bootstrap             
#>                  ────────────────  ───────────────────────────────── 
#>  Variable      │    95% CI (B)        B       SE         95% CI      
#> ───────────────┼─────────────────────────────────────────────────────
#>  (Intercept)   │ [ 4.100,  4.464]     4.282  0.101  [ 4.085,  4.479] 
#>  education     │ [ 0.081,  0.105]     0.093  0.007  [ 0.080,  0.105] 
#>  ethnicity:    │                                                     
#>    cauc (ref.) │        –              –      –            –         
#>    afam        │ [-0.321, -0.058]    -0.190  0.064  [-0.315, -0.064] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n             │                   2000                              
#>  AIC           │                   3616.4                            
#> 
#> Note. Quantile regression (τ = 0.50) models. Std. errors: nid: quantile sandwich (nid, Hall-Sheather); iid: quantile (iid); ker: quantile kernel (ker); bootstrap: bootstrap (xy pairs, 500 replicates). CI = confidence interval; AIC = Akaike information criterion.
```

[`summary()`](https://rdrr.io/r/base/summary.html) of quantreg does not
default to `"nid"` on a small sample: below 1,001 rows it inverts a rank
test
([`?summary.rq`](https://rdrr.io/pkg/quantreg/man/summary.rq.html)).
That method gives confidence intervals without standard errors, and
`vcov = "rank"` reproduces it: the standard errors and the p-values of
the table stay empty.

For clustered data, `vcov = "bootstrap"` with a `cluster` runs the wild
gradient bootstrap of Hagemann (2017). The median death rate of the
panel, clustered by state:

``` r

q_frate <- rq(frate ~ beertax + year, tau = 0.5, data = Fatalities)

set.seed(3)
table_regression(
  list(nid = q_frate, "Cluster bootstrap" = q_frate),
  vcov = list("nid", "bootstrap"),
  cluster = list(NULL, ~state),
  boot_n = 500,
  keep = "beertax",
  show_columns = c("b", "se", "ci", "p"),
  digits = 3
)
#> Quantile regression (τ = 0.50) comparison: frate
#> 
#>                                 nid                             Cluster bootstrap           
#>                ─────────────────────────────────────  ───────────────────────────────────── 
#>  Variable    │    B      SE        95% CI        p       B      SE        95% CI        p   
#> ─────────────┼──────────────────────────────────────────────────────────────────────────────
#>  (Intercept) │   1.654  0.116  [1.426, 1.882]  <.001    1.654  0.154  [1.353, 1.955]  <.001 
#>  beertax     │   0.424  0.060  [0.306, 0.542]  <.001    0.424  0.202  [0.028, 0.820]   .036 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 336                                    336                                   
#>  AIC         │ 550.8                                  550.8                                 
#> 
#> Note. Quantile regression (τ = 0.50) models. Std. errors: nid: quantile sandwich (nid, Hall-Sheather); Cluster bootstrap: wild gradient cluster bootstrap (500 replicates), clusters by state. CI = confidence interval; AIC = Akaike information criterion.
```

The standard error of the beer tax is more than three times larger once
the states are taken as clusters. The method assumes many clusters.
Hagemann (2017) reports that with very few of them, inference from
bootstrap standard errors rejects too often.

One argument can mislead a user of quantreg. In
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
`tau` is the time horizon of the survival columns. The quantile of a fit
is set in `rq(tau = )`.

## Presentation-ready output

Everything above renders through the rich output engines. The three
models of the beer tax, with standard errors clustered by state, as a
`gt` table:

``` r

pkgdown_dark_gt(
  table_regression(
    list(Pooled = pooled, "Dummies (LSDV)" = lsdv, "Within" = within_cl),
    vcov = list("CR2", "CR2", "classical"),
    cluster = list(~state, ~state, NULL),
    keep = "beertax",
    output = "gt"
  )
)
```

[TABLE]

## References

- Bell, A., Fairbrother, M., & Jones, K. (2019). Fixed and random
  effects models: Making an informed choice. *Quality & Quantity*,
  53(2), 1051–1074.
- Bell, R. M., & McCaffrey, D. F. (2002). Bias reduction in standard
  errors for linear regression with multi-stage samples. *Survey
  Methodology*, 28(2), 169–181.
- Cameron, A. C., Gelbach, J. B., & Miller, D. L. (2008).
  Bootstrap-based improvements for inference with clustered errors.
  *Review of Economics and Statistics*, 90(3), 414–427.
- Gelman, A., Hill, J., & Vehtari, A. (2020). *Regression and other
  stories*. Cambridge University Press.
- Hagemann, A. (2017). Cluster-robust bootstrap inference in quantile
  regression models. *Journal of the American Statistical Association*,
  112(517), 446–456.
- Harrell, F. E. (2015). *Regression modeling strategies: With
  applications to linear models, logistic and ordinal regression, and
  survival analysis* (2nd ed.). Springer.
- Imbens, G. W., & Kolesár, M. (2016). Robust standard errors in small
  samples: Some practical advice. *Review of Economics and Statistics*,
  98(4), 701–712.
- Pustejovsky, J. E., & Tipton, E. (2018). Small-sample methods for
  cluster-robust variance estimation and hypothesis testing in fixed
  effects models. *Journal of Business & Economic Statistics*, 36(4),
  672–683.
- Zeileis, A. (2004). Econometric computing with HC and HAC covariance
  matrix estimators. *Journal of Statistical Software*, 11(10), 1–17.
