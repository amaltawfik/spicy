# Summary tables from a survey design

A sample that is stratified, clustered, weighted to a population or
carried by replicate weights is not a simple random sample. Its mean is
still a weighted mean. The standard error depends on the strata and the
clusters, the degrees of freedom count sampling units and not rows, and
the tests are corrected for the design. A column of weights does not
carry that information.

spicy has three functions for these samples:
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md),
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
and
[`table_battery_svy()`](https://amaltawfik.github.io/spicy/reference/table_battery_svy.md).
They compute nothing themselves. Every number comes from the survey
package (Lumley, 2004, 2010), and spicy adds what its other tables have:
the same columns, the same outputs and the same typed view, with a note
that says which design produced the numbers.

``` r

library(spicy)
library(survey)
data(api)
```

Three designs from survey’s own `api` data, used throughout:

``` r

dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
dstrat <- svydesign(
  id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc
)
rclus1 <- as.svrepdesign(dclus1)
```

## Continuous variables

``` r

table_continuous_svy(dclus1, select = c(api00, api99))
#> Descriptive statistics
#> 
#>                                                             95% CI              
#>                                                       ──────────────────        
#>  Variable   │    M         SD       Min       Max        LL        UL       n   
#> ────────────┼───────────────────────────────────────────────────────────────────
#>  api00      │  644.17    105.75    411.00    905.00    593.68    694.66    183  
#>  api99      │  606.98    112.85    365.00    890.00    555.02    658.94    183  
#> 
#> Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSUs, with finite population correction; 14 degrees of freedom. Std. errors: design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. CI = confidence interval; LL = lower limit; UL = upper limit.
```

After the sample size, three sentences of the note describe the design.
It is a one-stage cluster sample of 15 school districts with a finite
population correction. The standard errors come from Taylor
linearization. The interval is a *t* interval on 14 degrees of freedom,
the number of clusters minus one. The 183 schools do not enter that
count.

[`confint()`](https://rdrr.io/r/stats/confint.html) on a survey object
uses the normal distribution by default:

``` r

m <- svymean(~api00, dclus1)
confint(m)                    # survey's default: normal
#>          2.5 %   97.5 %
#> api00 598.0275 690.3113
confint(m, df = degf(dclus1)) # what the table reports
#>          2.5 %   97.5 %
#> api00 593.6763 694.6625
```

[`svyciprop()`](https://rdrr.io/pkg/survey/man/svyciprop.html),
[`svyquantile()`](https://rdrr.io/pkg/survey/man/svyquantile.html),
[`svyttest()`](https://rdrr.io/pkg/survey/man/svyttest.html) and
[`regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html) take
their degrees of freedom from the design, and so do the tables. Each
spends them as its statistic requires:
[`svyttest()`](https://rdrr.io/pkg/survey/man/svyttest.html) uses one on
the estimated difference, so its *t* has `degf(design) - 1` degrees of
freedom, 13 here. The note gives the number.

### Choosing the statistics

`show_columns` takes the tokens of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
with `"se"` (the design-based standard error) and `"deff"` (the design
effect) added and `"med_ci"` removed. The columns follow the order of
the tokens:

``` r

table_continuous_svy(
  dclus1,
  select = api00,
  show_columns = c("m", "se", "ci", "med_iqr", "n", "weighted_n", "deff"),
  deff = TRUE
)
#> Descriptive statistics
#> 
#>                                95% CI                                   
#>                            ──────────────                               
#>  Variable │   M      SE      LL      UL         Mdn [Q1, Q3]         n  
#> ──────────┼─────────────────────────────────────────────────────────────
#>  api00    │ 644.17  23.54  593.68  694.66  652.00 [552.00, 719.00]  183 
#> 
#>  Variable │ Weighted n  DEff 
#> ──────────┼──────────────────
#>  api00    │  6194.00    9.35 
#> 
#> Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSUs, with finite population correction; 14 degrees of freedom. Quantiles: Hyndman-Fan type 1. Std. errors: design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. SE = design-based standard error of the mean; CI = confidence interval; LL = lower limit; UL = upper limit; Q1 = first quartile; Q3 = third quartile; DEff = design effect.
```

The design effect is 9.35: the variance of this cluster sample is nine
times that of a simple random sample of 183 schools. A table that
ignored the design would report a standard error three times too small.

`"med_ci"` is refused. The exact interval of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
inverts a binomial sign test, and its coverage rests on independent
observations, which a clustered or stratified sample does not have. For
a design-based interval on the population median, call
`survey::svyquantile(interval.type = )` on the design.

### Groups

`by =` cuts one domain per group. survey recomputes the degrees of
freedom on the sampling units each domain retains, so a grouped table
generally carries a different *df* per row, and the note gives their
range:

``` r

table_continuous_svy(dclus1, select = api00, by = stype, statistic = TRUE)
#> Descriptive statistics by stype
#> 
#>                                                        95% CI          
#>                                                    ──────────────      
#>  Variable │ Group    M       SD     Min     Max      LL      UL     n  
#> ──────────┼────────────────────────────────────────────────────────────
#>  api00    │ E      648.87  106.16  436.00  905.00  600.91  696.83  144 
#>           │ H      618.57   96.74  443.00  724.00  528.67  708.48   14 
#>           │ M      631.44  109.06  411.00  847.00  561.87  701.01   25 
#> 
#>  Variable │ Group       Test         p   
#> ──────────┼──────────────────────────────
#>  api00    │ E      F(2, 12) = 1.28  .314 
#>           │ H                            
#>           │ M                            
#> 
#> Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSUs, with finite population correction; degrees of freedom vary by group (7–14). Std. errors: design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. P-value: design-based Wald test. The group comparison uses 12 degrees of freedom (observed groups only). CI = confidence interval; LL = lower limit; UL = upper limit.
```

The comparison is a single design-based test on the whole design:
[`svyttest()`](https://rdrr.io/pkg/survey/man/svyttest.html) with two
observed groups, the Wald *F* of
[`regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html) with
three or more, and
[`svyranktest()`](https://rdrr.io/pkg/survey/man/svyranktest.html) under
`test = "nonparametric"`. Under a design the variance is that of the
design, and the distinction between the Welch and the Student test does
not exist. `test = "student"` warns and behaves like the default.

The intervals use those degrees of freedom. `svyby(vartype = "ci")`
builds its interval with
[`confint.default()`](https://rdrr.io/r/stats/confint.html), on a normal
quantile, and returns a narrower interval than the tables here. The two
differ by the ratio of `qt(0.975, degf(design))` to `qnorm(0.975)`: the
interval of the table is 9% wider on the 14 degrees of freedom of
`dclus1`.

## Categorical variables

``` r

table_categorical_svy(dclus1, select = c(stype, awards))
#> Categorical table
#> 
#>  Variable   │   n      %    
#> ────────────┼───────────────
#>  stype      │               
#>    E        │  144    78.7  
#>    H        │   14     7.7  
#>    M        │   25    13.7  
#> ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  awards     │               
#>    No       │   53    29.0  
#>    Yes      │  130    71.0  
#> 
#> Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSUs, with finite population correction; 14 degrees of freedom. Std. errors: design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. % = estimated percentage. n = observed (unweighted) count.
```

`n` is the observed count: 144 schools of type E were sampled. The
`78.7%` beside it is an estimate of the share in the population. The
table shows both, and the note gives the estimated population (6194)
beside the sample size (183).

``` r

table_categorical_svy(
  dclus1,
  select = stype,
  proportion_ci = TRUE,
  deff = TRUE
)
#> Categorical table
#> 
#>                                  95% CI             
#>                              ──────────────         
#>  Variable   │   n      %       LL      UL     DEff  
#> ────────────┼───────────────────────────────────────
#>  stype      │                                       
#>    E        │  144    78.7    67.1    87.0    2.40  
#>    H        │   14     7.7     3.5    15.8    1.91  
#>    M        │   25    13.7     8.4    21.3    1.40  
#> 
#> Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSUs, with finite population correction; 14 degrees of freedom. Std. errors: design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. Percentage CIs: Wald on the logit scale (logistic regression). % = estimated percentage. n = observed (unweighted) count. CI = confidence interval; LL = lower limit; UL = upper limit; DEff = design effect.
```

The percentage comes from
[`svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html) and the
interval from
[`svyciprop()`](https://rdrr.io/pkg/survey/man/svyciprop.html).
[`svyciprop()`](https://rdrr.io/pkg/survey/man/svyciprop.html) estimates
on the transformed scale of its method, and a percentage taken from it
would change with `ci_method`, in the thirteenth decimal. `ci_method` is
a property of the interval, and the proportion should not depend on it.

``` r

table_categorical_svy(dclus1, select = stype, by = sch.wide)
#> Categorical table by sch.wide
#> 
#>                    No            Yes           Total             
#>               ────────────  ─────────────  ─────────────         
#>  Variable   │  n      %       n      %       n      %       p    
#> ────────────┼────────────────────────────────────────────────────
#>  stype      │                                              .022  
#>    E        │  12    52.2    132    82.5    144    78.7          
#>    H        │   3    13.0     11     6.9     14     7.7          
#>    M        │   8    34.8     17    10.6     25    13.7          
#> 
#> Note. N = 183 (weighted 6194). Percentages are column percentages. Design: cluster (dnum), 15 PSUs, with finite population correction; degrees of freedom vary by group (9–14). Std. errors: design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. P-value: design-based Pearson chi-squared (Rao-Scott second-order correction). % = estimated percentage. n = observed (unweighted) count.
```

The `p` is [`svychisq()`](https://rdrr.io/pkg/survey/man/svychisq.html):
Pearson’s chi-square with the Rao-Scott second-order correction,
referred to *F*(ndf, `degf(design)`). `chisq_statistic` offers
`"Chisq"`, `"Wald"`, `"adjWald"` and `"saddlepoint"` as well. Two of
survey’s seven are refused: `"lincom"`, whose numerical integration
[`?pchisqsum`](https://rdrr.io/pkg/survey/man/pchisqsum.html) documents
as failing when the upper tail approaches machine epsilon, and
`"wls-score"`, which has no reporting convention here.

The association measures of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
(Cramér’s *V*, phi, tau-b and tau-c, gamma, Somers’ *D*, lambda) are not
offered. None has an established design-based variance, and the
intervals
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
gives them assume simple random sampling. The table reports the
design-based test in the `p` column. For an effect size, fit a model:
`table_regression(survey::svyglm(...))`.

## Item batteries

[`table_battery_svy()`](https://amaltawfik.github.io/spicy/reference/table_battery_svy.md)
is the design twin of
[`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md):
several items answered on one scale, one row per item and one column per
level.

``` r

table_battery_svy(
  dclus1,
  select = c(awards, sch.wide, comp.imp, both),
  by = stype
)
#> Item battery by stype
#> 
#>                    E               H               M              
#>             ───────────────  ──────────────  ──────────────       
#>  Variable │  No   Yes    n    No   Yes   n    No   Yes   n    p   
#> ──────────┼───────────────────────────────────────────────────────
#>  awards   │ 22.9  77.1  144  57.1  42.9  14  48.0  52.0  25  .075 
#>  sch.wide │  8.3  91.7  144  21.4  78.6  14  32.0  68.0  25  .075 
#>  comp.imp │ 22.2  77.8  144  50.0  50.0  14  44.0  56.0  25  .123 
#>  both     │ 22.2  77.8  144  50.0  50.0  14  44.0  56.0  25  .123 
#> 
#> Note. Design: cluster (dnum), 15 PSUs, with finite population correction; 14 degrees of freedom. Percentages are row percentages estimated under the design. Base: respondents who answered the item. Item p-values, each item by group: design-based Pearson chi-squared (Rao-Scott second-order correction). Adjusted for 4 tests (Holm method). n = observed (unweighted) count.
```

The base of a row is the schools that answered the item, `n` their
observed count, and each percentage is
[`svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html) on that
domain. Each item is tested against `stype` by
[`svychisq()`](https://rdrr.io/pkg/survey/man/svychisq.html), Rao-Scott
corrected, and the four p-values are adjusted together by Holm’s method.

## A stratum with one sampling unit

A stratum with a single primary sampling unit has no variance to
estimate, and under its default `options(survey.lonely.psu = "fail")`
survey stops there, point estimate included. The table is still
produced, with a dash in each cell survey could not estimate and a
sentence of the note that says what the dash means. The function also
warns, once, quoting survey’s message. Here the one school left in
stratum H falls in the `No` group of `sch.wide`, so the other group is
estimated as usual:

``` r

keep <- c(which(apistrat$stype != "H"), which(apistrat$stype == "H")[1])
lonely <- svydesign(
  id = ~1, strata = ~stype, weights = ~pw, data = apistrat[keep, ]
)
table_continuous_svy(lonely, select = api00, by = sch.wide)
#> Warning in table_continuous_svy(lonely, select = api00, by = sch.wide): Some cells could not be estimated:
#> • `survey::svyttest()`, `survey::svymean()`, `survey::svyvar()` stopped: "Stratum (H) has only one PSU at stage 1"
#> ℹ The cells that could not be estimated show a dash.
#> ℹ A stratum has a single primary sampling unit, which stops survey under its default `options(survey.lonely.psu = "fail")`: "adjust", "average", "remove" or "certainty" estimate these cells (see `?survey::surveyoptions`).
#> Descriptive statistics by sch.wide
#> 
#>                                                        95% CI             
#>                                                    ──────────────         
#>  Variable │ Group    M       SD     Min     Max      LL      UL     n   p 
#> ──────────┼───────────────────────────────────────────────────────────────
#>  api00    │ No        –       –    398.00  778.00     –       –     25  – 
#>           │ Yes    678.42  121.28  421.00  893.00  655.85  700.99  126    
#> 
#> Note. N = 151 (weighted 5454). Design: stratified (stype); degrees of freedom vary by group (22–124). A dash marks an estimate the design could not provide: a stratum has a single primary sampling unit. Std. errors: design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. CI = confidence interval; LL = lower limit; UL = upper limit.
```

The values `"adjust"`, `"average"`, `"remove"` and `"certainty"` of that
option let survey estimate these cells, each by its own convention
([`?survey::surveyoptions`](https://rdrr.io/pkg/survey/man/surveyoptions.html)),
and the table then follows them. A warning of survey’s own, such as the
one on a stratum with a single primary sampling unit in a domain under
`options(survey.adjust.domain.lonely = TRUE)`, is said once per table,
with the number of times survey raised it.

## Replicate weights

A replicate-weights design goes through the same functions. The point
estimates depend on the weights alone and are identical. The standard
errors come from the replicates and differ:

``` r

table_continuous_svy(rclus1, select = api00)
#> Descriptive statistics
#> 
#>                                                             95% CI              
#>                                                       ──────────────────        
#>  Variable   │    M         SD       Min       Max        LL        UL       n   
#> ────────────┼───────────────────────────────────────────────────────────────────
#>  api00      │  644.17    105.75    411.00    905.00    587.70    700.64    183  
#> 
#> Note. N = 183 (weighted 6194). Design: replicate weights (JK1), 15 replicates; 14 degrees of freedom. Std. errors: design-based (replicate weights, JK1). Confidence intervals and tests use the design degrees of freedom. CI = confidence interval; LL = lower limit; UL = upper limit.
```

## Weights, or a design?

`table_continuous(weights = )` and
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
read a weight differently, and they do not estimate the same standard
deviation.

`table_continuous(weights = )` follows the frequency-expansion
convention: a weight is a number of copies, and `SD` has denominator
`sum(w) - 1`. With integer weights the statistics are those of the data
repeated that many times.

[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
follows the sampling-weight convention: a weight is a number of units
represented, and `SD` is `sqrt(survey::svyvar())`, whose denominator is
`n - 1` on weights normalized to sum to `n`.

`rescale = TRUE` makes the two agree, by an identity. With
`w' = w * n / sum(w)`, so that `sum(w') = n`:

    sum(w' (x - xbar)^2) / (sum(w') - 1)
      = (n / sum(w)) * sum(w (x - xbar)^2) / (n - 1)
      = n / (n - 1) * sum(w (x - xbar)^2) / sum(w)

and the right-hand side is what
[`survey::svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
computes. On a design that declares nothing but weights, the two tables
agree:

``` r

d_iid <- svydesign(id = ~1, weights = ~pw, data = apiclus1)

svy <- table_continuous_svy(d_iid, select = api00, output = "long")
wtd <- table_continuous(
  apiclus1, select = api00, weights = pw, rescale = TRUE, output = "long"
)
c(svy = svy$sd, weighted_rescaled = wtd$sd)
#>               svy weighted_rescaled 
#>          105.7489          105.7489
```

Under the default `rescale = FALSE` they differ, each following its
convention:

``` r

expanded <- table_continuous(
  apiclus1, select = api00, weights = pw, output = "long"
)
c(svy = svy$sd, weighted_default = expanded$sd)
#>              svy weighted_default 
#>         105.7489         105.4681
```

The mean is the same under both conventions: `sum(w x) / sum(w)` does
not change when the weights are rescaled.

A design carries more than weights. On the cluster design, the same mean
has a standard error that the weights alone do not give:

``` r

c(
  iid_design = table_continuous_svy(d_iid, select = api00, output = "long")$se,
  cluster_design = table_continuous_svy(dclus1, select = api00, output = "long")$se
)
#>     iid_design cluster_design 
#>       7.817181      23.542241
```

Passing a design to
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
or a data frame to
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md),
is an error that names the other function.

## Regression under a design

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reads a fit of
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html),
[`survey::svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) or
[`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html), on
a linearized or a replicate-weights design. The coefficients, the
variance and the degrees of freedom all come from survey.

``` r

gl <- svyglm(api00 ~ ell + meals + stype, design = dstrat)
table_regression(gl)
#> Survey-weighted linear regression: api00
#> 
#>  Variable    │    B      SE          95% CI          p   
#> ─────────────┼───────────────────────────────────────────
#>  (Intercept) │  865.99   8.54  [ 849.16,  882.83]  <.001 
#>  ell         │   -0.56   0.34  [  -1.24,    0.12]   .104 
#>  meals       │   -3.43   0.24  [  -3.90,   -2.96]  <.001 
#>  stype:      │                                           
#>    E (ref.)  │     –      –            –            –    
#>    H         │ -128.29  10.40  [-148.81, -107.78]  <.001 
#>    M         │  -60.46   9.76  [ -79.72,  -41.20]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │  200                                      
#>  Weighted n  │ 6194                                      
#>  AIC         │ 2197.8                                    
#> 
#> Note. Survey-weighted linear regression. Design: stratified (stype), with finite population correction; 193 residual degrees of freedom. AIC of a design-based fit: design-based AIC (dAIC). Std. errors: design-based (Taylor linearization). CI = confidence interval; AIC = Akaike information criterion.
```

The note states the design and the residual degrees of freedom of the
tests, and it names the variance estimator. The fit statistics give the
observed `n` and the `Weighted n` the estimates describe, the pair the
descriptive tables print in their note.

[*Regression tables for survey
designs*](https://amaltawfik.github.io/spicy/articles/table-regression-survey.md)
covers these models: the degrees of freedom, weights compared with a
declared design, odds ratios and average marginal effects, the tests of
a term, subpopulations, replicate weights, ordinal and Cox fits, and
what a design-based fit refuses.

## Quantiles

`qrule` chooses the rule, and the note names the one in force. The
default `"math"` estimates the quantile of the population,
`inf{x : F(x) >= p}`. `"spicy"` switches to the type-7 interpolation of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
for a reader who needs the two tables to agree cell for cell. Any other
value, a function included, is passed to
[`survey::svyquantile()`](https://rdrr.io/pkg/survey/man/svyquantile.html)
as it is.

``` r

q <- function(rule) {
  table_continuous_svy(
    dclus1, select = api00, qrule = rule,
    show_columns = c("med", "q1", "q3"), output = "long"
  )[, c("q1", "median", "q3")]
}
rbind(math = q("math"), spicy = q("spicy"), hf7 = q("hf7"))
#>          q1 median  q3
#> math  552.0    652 719
#> spicy 552.0    652 719
#> hf7   552.5    652 718
```

## Downstream

These tables carry the typed view the rest of the package uses, so
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
and [`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md)
work as they do elsewhere:

``` r

tbl <- table_categorical_svy(dclus1, select = stype, by = sch.wide)
s <- as_structured(tbl)
s$body[, c("Variable", "Yes n", "Yes %", "p", ".row_role")]
#>   Variable Yes n  Yes %          p     .row_role
#> 1    stype    NA     NA 0.02174746 factor_header
#> 2        E   132 82.500         NA         level
#> 3        H    11  6.875         NA         level
#> 4        M    17 10.625         NA         level
names(s$spanners)
#> [1] "No"    "Yes"   "Total"
```

``` r

inline(tbl, "stype", level = "E", column = "pct", model = "Yes")
#> [1] "82.5"
inline(tbl, "stype", column = "p")
#> [1] ".022"
```

## Rendering

Every engine of the family is available: `"tinytable"`, `"gt"`,
`"flextable"`, `"word"`, `"excel"`, `"clipboard"`, plus `"data.frame"`
for the displayed table with its numbers and `"long"` for one row per
variable, level and group.

``` r

table_continuous_svy(
  dstrat,
  select = c(api00, api99),
  by = stype,
  output = "gt"
)
```

[TABLE]

## What is not here yet

Two-phase, pps, database-backed and multiframe designs are refused with
a classed error. The delegation to survey was verified on
[`svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html) and
[`as.svrepdesign()`](https://rdrr.io/pkg/survey/man/as.svrepdesign.html)
designs only. Call survey directly for the others. If one of them
matters for your work, open an issue on the tracker of the package and
name the design.

## References

- Lumley, T. (2004). Analysis of complex survey samples. *Journal of
  Statistical Software*, 9(8), 1–19.
- Lumley, T. (2010). *Complex surveys: A guide to analysis using R*.
  John Wiley & Sons.
