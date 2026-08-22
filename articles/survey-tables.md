# Summary tables from a survey design

When a sample is not a simple random sample – when it is stratified,
clustered, weighted to a population, or carried by replicate weights –
the mean is still a weighted mean, but everything around it changes. The
standard error depends on the strata and the clusters, the degrees of
freedom are a count of sampling units rather than of rows, and the tests
are corrected for the design. None of that can be recovered from a
column of weights.

spicy has two functions for that case, and they compute nothing
themselves:
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
and
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
delegate every number to the **survey** package (Lumley, 2004, 2010) and
put spicy’s restitution layer around it – the same columns, the same
nine output routes, the same typed view, plus a footer that says what
design produced the numbers.

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
#>  Variable │   M       SD     Min     Max    95% CI LL  95% CI UL   n  
#> ──────────┼───────────────────────────────────────────────────────────
#>  api00    │ 644.17  105.75  411.00  905.00   593.68     694.66    183 
#>  api99    │ 606.98  112.85  365.00  890.00   555.02     658.94    183 
#> 
#> N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Standard errors: Taylor linearisation (survey). Confidence intervals and tests use the design degrees of freedom.
```

Three sentences under the table, and each is there because the number
above it cannot be read without them. The design is a one-stage cluster
sample of 15 school districts with a finite population correction; the
standard errors come from Taylor linearisation; the interval is a *t*
interval on 14 degrees of freedom – the number of clusters minus one,
not the 183 schools.

That last point is worth dwelling on.
[`confint()`](https://rdrr.io/r/stats/confint.html) on a survey object
uses the *normal* distribution by default:

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
[`regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html) all
take their degrees of freedom from the design, not from the row count,
and so do the tables – everywhere. (Each spends them as its statistic
requires: [`svyttest()`](https://rdrr.io/pkg/survey/man/svyttest.html)
uses one on the estimated difference, so its *t* sits on
`degf(design) - 1` – 13 here.) The footer says so rather than leaving
the reader to find out.

### Choosing the statistics

`show_columns` takes the tokens of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
plus `"se"` (the design-based standard error) and `"deff"` (the design
effect), and minus `"med_ci"`:

``` r

table_continuous_svy(
  dclus1,
  select = api00,
  show_columns = c("m", "se", "ci", "med_iqr", "n", "weighted_n", "deff"),
  deff = TRUE
)
#> Descriptive statistics
#> 
#>  Variable │   M      SE         Med [Q1, Q3]        95% CI LL  95% CI UL   n  
#> ──────────┼───────────────────────────────────────────────────────────────────
#>  api00    │ 644.17  23.54  652.00 [552.00, 719.00]   593.68     694.66    183 
#> 
#>  Variable │ Weighted n  DEff 
#> ──────────┼──────────────────
#>  api00    │  6194.00    9.35 
#> 
#> N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Standard errors: Taylor linearisation (survey). Confidence intervals and tests use the design degrees of freedom. Quantiles: qrule = "math" (survey). Med [Q1, Q3] = median [first quartile, third quartile]. DEff = design effect (design-based variance / simple-random-sample variance at the same n). SE = design-based standard error of the mean.
```

The design effect of 9.35 is the point of the whole exercise: the
variance of this cluster sample is nine times what a simple random
sample of 183 schools would have given. A table that ignored the design
would report a standard error three times too small.

`"med_ci"` is refused rather than approximated. The exact interval
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
reports inverts a binomial sign test, and its coverage guarantee rests
on independent observations – which a clustered or stratified sample is
not. The estimand (the population median) is not the problem; the
interval construction is. For a design-based interval on that same
estimand, call `survey::svyquantile(interval.type = )` on the design
itself.

### Groups

`by =` cuts one domain per group. survey recomputes the degrees of
freedom on the sampling units each domain retains, so a grouped table
generally carries a different *df* per row, and the footer gives the
span:

``` r

table_continuous_svy(dclus1, select = api00, by = stype, statistic = TRUE)
#> Descriptive statistics by stype
#> 
#>  Variable │ Group    M       SD     Min     Max    95% CI LL  95% CI UL   n  
#> ──────────┼──────────────────────────────────────────────────────────────────
#>  api00    │ E      648.87  106.16  436.00  905.00   600.91     696.83    144 
#>           │ H      618.57   96.74  443.00  724.00   528.67     708.48     14 
#>           │ M      631.44  109.06  411.00  847.00   561.87     701.01     25 
#> 
#>  Variable │ Group       Test         p   
#> ──────────┼──────────────────────────────
#>  api00    │ E      F(2, 12) = 1.28  .314 
#>           │ H                            
#>           │ M                            
#> 
#> N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; degrees of freedom vary by group (7 to 14). Standard errors: Taylor linearisation (survey). Confidence intervals and tests use the design degrees of freedom. Group comparison: design-based Wald test. The group comparison uses 12 degrees of freedom (observed groups only).
```

The comparison is one design-based test on the whole design, not a set
of pairwise ones:
[`svyttest()`](https://rdrr.io/pkg/survey/man/svyttest.html) with two
observed groups, the Wald *F* of
[`regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html) with
three or more, and
[`svyranktest()`](https://rdrr.io/pkg/survey/man/svyranktest.html) under
`test = "nonparametric"`. Under a design there is no Welch / Student
distinction – the variance is the design’s – so `test = "student"` warns
and behaves like the default.

The intervals are referred to those degrees of freedom, which is worth
saying because survey’s own shortcut is not: `svyby(vartype = "ci")`
builds its interval with `confint.default`, i.e. a normal quantile, and
so returns a slightly narrower interval than the tables here. The two
differ by the ratio of `qnorm(0.975)` to `qt(0.975, degf(design))` –
about 3% on the 14 degrees of freedom of `dclus1`. Neither is a mistake;
they answer to different reference distributions, and this family
answers to the design’s.

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
#> N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Standard errors: Taylor linearisation (survey). Confidence intervals and tests use the design degrees of freedom. % = estimated percentage within the column (survey::svymean). n = observed (unweighted) count.
```

`n` is the *observed* count: 144 schools of type E were sampled. The
`78.7%` beside it is an *estimate* of the population share, and the two
answer different questions – which is why the table shows both and the
footer gives the estimated population (6194) beside the sample size
(183).

``` r

table_categorical_svy(
  dclus1,
  select = stype,
  proportion_ci = TRUE,
  deff = TRUE
)
#> Categorical table
#> 
#>  Variable   │   n      %      95% CI LL    95% CI UL    DEff  
#> ────────────┼─────────────────────────────────────────────────
#>  stype      │                                                 
#>    E        │  144    78.7      67.1         87.0       2.40  
#>    H        │   14     7.7       3.5         15.8       1.91  
#>    M        │   25    13.7       8.4         21.3       1.40  
#> 
#> N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Standard errors: Taylor linearisation (survey). Confidence intervals and tests use the design degrees of freedom. Percentage CIs: logit (survey::svyciprop). % = estimated percentage within the column (survey::svymean). n = observed (unweighted) count. DEff = design effect (design-based variance / simple-random-sample variance at the same n).
```

The percentage always comes from
[`svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html) and the
interval from
[`svyciprop()`](https://rdrr.io/pkg/survey/man/svyciprop.html). That is
deliberate:
[`svyciprop()`](https://rdrr.io/pkg/survey/man/svyciprop.html) estimates
on the transformed scale its method names, so a percentage taken from
there would move – in the thirteenth decimal – with `ci_method`, which
is a property of the interval and not of the proportion.

``` r

table_categorical_svy(dclus1, select = stype, by = sch.wide)
#> Categorical table by sch.wide
#> 
#>  Variable   │  No n    No %    Yes n    Yes %    Total n    Total %     p    
#> ────────────┼────────────────────────────────────────────────────────────────
#>  stype      │                                                          .022  
#>    E        │   12     52.2     132     82.5       144       78.7            
#>    H        │    3     13.0      11      6.9        14        7.7            
#>    M        │    8     34.8      17     10.6        25       13.7            
#> 
#> N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; degrees of freedom vary by group (9 to 14). Standard errors: Taylor linearisation (survey). Confidence intervals and tests use the design degrees of freedom. Group comparison: design-based Pearson chi-square (Rao-Scott second-order correction). % = estimated percentage within the column (survey::svymean). n = observed (unweighted) count.
```

The `p` is [`svychisq()`](https://rdrr.io/pkg/survey/man/svychisq.html):
Pearson’s chi-square with the Rao-Scott second-order correction,
referred to *F*(ndf, `degf(design)`). `chisq_statistic` offers
`"Chisq"`, `"Wald"`, `"adjWald"` and `"saddlepoint"` as well. Two of
survey’s seven are refused: `"lincom"`, whose numerical integration
[`?pchisqsum`](https://rdrr.io/pkg/survey/man/pchisqsum.html) documents
as failing when the upper tail approaches machine epsilon, and
`"wls-score"`, which has no reporting convention here.

Association measures – Cramer’s *V*, phi, tau-b/c, gamma, Somers’ *D*,
lambda – are absent, and that absence is a decision. None has an
established design-based variance, and the intervals
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
gives them assume simple random sampling. The design-based measure of
association is the test in the `p` column; for an effect size, model it:
`table_regression(survey::svyglm(...))`.

## Replicate weights

A replicate-weights design goes through the same functions. The point
estimates depend on the weights alone and are therefore identical; the
standard errors are not, and should not be:

``` r

table_continuous_svy(rclus1, select = api00)
#> Descriptive statistics
#> 
#>  Variable │   M       SD     Min     Max    95% CI LL  95% CI UL   n  
#> ──────────┼───────────────────────────────────────────────────────────
#>  api00    │ 644.17  105.75  411.00  905.00   587.70     700.64    183 
#> 
#> N = 183 (weighted 6194). Design: replicate weights (JK1), 15 replicates; 14 degrees of freedom. Standard errors: replicate weights (survey). Confidence intervals and tests use the design degrees of freedom.
```

## Weights, or a design?

This is the question the two families answer differently, and the
difference is an estimand rather than an approximation.

`table_continuous(weights = )` implements the **frequency-expansion**
convention of decision 17: a weight is a number of copies, and `SD` has
denominator `sum(w) - 1`. With integer weights the statistics are
exactly those of the data repeated that many times.

[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
implements the **sampling-weight** convention: a weight is a number of
units represented, and `SD` is `sqrt(survey::svyvar())`, whose
denominator is `n - 1` on weights normalised to sum to `n`.

`rescale = TRUE` is the bridge between them, and it is an identity.
Writing `w' = w * n / sum(w)`, so that `sum(w') = n`:

    sum(w' (x - xbar)^2) / (sum(w') - 1)
      = (n / sum(w)) * sum(w (x - xbar)^2) / (n - 1)
      = n / (n - 1) * sum(w (x - xbar)^2) / sum(w)

and the right-hand side is what
[`survey::svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
computes. So on a design that declares nothing but weights, the two
tables agree:

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

Under the default `rescale = FALSE` they do not, and that is the
estimand boundary, not a bug:

``` r

expanded <- table_continuous(
  apiclus1, select = api00, weights = pw, output = "long"
)
c(svy = svy$sd, weighted_default = expanded$sd)
#>              svy weighted_default 
#>         105.7489         105.4681
```

The *mean* is continuous across both regimes: `sum(w x) / sum(w)` does
not move when the weights are rescaled.

And the whole point of the design functions is that a design carries
more than weights. On the real cluster design, the same mean has a
standard error the weights alone cannot produce:

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
is an error naming the other function. There is no silent coercion in
either direction.

## Regression under a design

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reads a design-based fit the same way, and delegates the same way: the
coefficients, the variance and the degrees of freedom all come from
survey.

Three model classes are supported, on a linearised and on a
replicate-weights design alike:
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html),
[`survey::svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) for a
cumulative-link model, and
[`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html) for
a design-weighted Cox model.

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
#> Note. Survey-weighted linear regression.
#> Design: stratified (stype), with finite population correction; 193 residual degrees of freedom.
#> Std. errors: Design-based (Taylor linearisation).
```

Three things in that footer are worth naming.

The design line says what the design is and how many degrees of freedom
the table’s own tests use. That number is the *model’s* residual df –
what survey writes on the fit, and what
[`survey::regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html)
takes as its denominator – not the design’s own. It is read, never
re-derived: the engines of survey do not share one expression, and a Cox
fit ends one degree of freedom above what the same arithmetic would give
for a `svyglm`. For this linear fit:

``` r

c(design = degf(dstrat), model = df.residual(gl))
#> design  model 
#>    197    193
```

The variance label names the estimator the design actually uses. On a
replicate design there is no linearisation anywhere, and the footer says
so:

``` r

table_regression(
  svyglm(api00 ~ ell, design = rclus1),
  show_columns = c("b", "ci", "p")
)
#> Survey-weighted linear regression: api00
#> 
#>  Variable    │    B          95% CI         p   
#> ─────────────┼──────────────────────────────────
#>  (Intercept) │  746.93  [684.12, 809.73]  <.001 
#>  ell         │   -3.72  [ -4.77,  -2.67]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │  183                             
#>  Weighted n  │ 6194                             
#>  AIC         │ 2150.5                           
#> 
#> Note. Survey-weighted linear regression.
#> Design: replicate weights (JK1), 15 replicates; 13 residual degrees of freedom.
#> Std. errors: Design-based (replicate weights, JK1).
```

And the counts are both there: the observed `n` and the `Weighted n` the
estimates describe, the same pair the descriptive tables print in their
header.

### Ordinal and Cox fits

A [`svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) fit gets its
cut-points as a Thresholds block, and its average marginal effects one
column per response category – averaged over the population, not over
the sample:

``` r

apistrat$grade <- ordered(cut(apistrat$api00, c(0, 600, 700, 1000)))
dg <- svydesign(
  id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc
)
table_regression(svyolr(grade ~ ell + stype, design = dg))
#> Survey-weighted cumulative logit regression (proportional odds): grade
#> 
#>  Variable                  │    B      SE       95% CI        p   
#> ───────────────────────────┼──────────────────────────────────────
#>  ell                       │   -0.09  0.01  [-0.12, -0.07]  <.001 
#>  stype:                    │                                      
#>    E (ref.)                │     –     –          –          –    
#>    H                       │   -1.86  0.37  [-2.60, -1.13]  <.001 
#>    M                       │   -1.35  0.34  [-2.03, -0.68]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:               │                                      
#>    (0,600] | (600,700]     │   -3.68  0.46  [-4.59, -2.78]  <.001 
#>    (600,700] | (700,1e+03] │   -1.82  0.35  [-2.51, -1.14]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                         │  200                                 
#>  Weighted n                │ 6194                                 
#> 
#> Note. Survey-weighted cumulative logit regression (proportional odds).
#> Design: stratified (stype), with finite population correction; 194 residual degrees of freedom.
#> Std. errors: Design-based (Taylor linearisation).
#> Thresholds: latent-scale category cut-points.
```

A [`svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html) fit
reports hazard ratios, the events, and the concordance:

``` r

data(pbc, package = "survival")
pbc$randomized <- with(pbc, !is.na(trt) & trt > 0)
bias <- glm(randomized ~ age * edema, data = pbc, family = binomial)
pbc$sw <- 1 / predict(bias, type = "response")
dpbc <- svydesign(
  id = ~1, prob = ~sw, strata = ~edema, data = subset(pbc, randomized)
)
cx <- svycoxph(
  survival::Surv(time, status > 0) ~ log(bili) + protime + albumin,
  design = dpbc
)
table_regression(cx, exponentiate = TRUE, show_columns = c("b", "ci", "p"))
#> Survey-weighted Cox proportional hazards regression: survival::Surv(time, status > 0)
#> 
#>  Variable   │   HR       95% CI       p   
#> ────────────┼─────────────────────────────
#>  log(bili)  │   2.43  [2.04, 2.90]  <.001 
#>  protime    │   1.30  [1.10, 1.52]   .002 
#>  albumin    │   0.34  [0.22, 0.52]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n          │ 312                         
#>  Weighted n │ 235                         
#>  N events   │ 144                         
#> 
#> Note. Survey-weighted Cox proportional hazards regression.
#> Design: stratified (edema); 307 residual degrees of freedom.
#> Std. errors: Design-based (Taylor linearisation).
#> Concordance C = 0.81 (SE = 0.02).
#> HR = hazard ratio.
#> Coefficients exponentiated and displayed as HR; CI bounds exponentiated.
```

### What a design fit refuses

Statistics that need a likelihood are absent rather than approximated:
[`AIC()`](https://rdrr.io/r/stats/AIC.html),
[`BIC()`](https://rdrr.io/r/stats/AIC.html),
[`logLik()`](https://rdrr.io/r/stats/logLik.html) and the
pseudo-R-squared family are not defined for a design-weighted fit, and
asking for them by token prints nothing rather than a number without an
interpretation. The one exception is `svyglm`, whose `AIC` row is the
design-based criterion of Lumley & Scott – and
`show_fit_stats = "eff_p"` reports the effective number of parameters of
the design beside it.

A robust variance is refused for every design class: the design is the
variance authority, and the way to change the estimator is to change the
design.

``` r

table_regression(gl, vcov = "HC3")
#> Error in `validate_vcov_cluster_lists()`:
#> ! `vcov = "HC3"` is not available for `svyglm` models.
#> ℹ The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
#> ℹ To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
```

The RMST and risk-difference columns are refused for a design-based Cox
fit. The estimands are not what is in question – they are validated
against exact oracles for an unweighted Cox model – but their
uncertainty comes from resampling subjects, and a row bootstrap ignores
the strata and the clusters the design declares.

How much that costs depends on the design and on the contrast, so no
single figure describes it. Measured over five designs and three data
seeds: a contrast **between** clusters has its standard error
understated by roughly 1.5x-6x depending on cluster count and size (~3x
on a 30 x 25 design with moderate intra-cluster correlation), while a
contrast **within** clusters, and any design without clustering, come
out about right. Nothing here is peculiar to these columns – the
estimand-level ratios track the coefficient-level ones design by design,
so this is the ordinary design effect of the Cox coefficient carried
onto them.

Putting the design’s own replicate weights in place of the row bootstrap
is on the roadmap. Until then, use
[`survey::svykm()`](https://rdrr.io/pkg/survey/man/svykm.html) for a
marginal survival curve, and
[`survey::regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html)
to test a term.

Average marginal effects are refused for a Cox fit, design-based or not:
a proportional-hazards model has no natural response scale to average an
effect on.

## Quantiles

`qrule` chooses the rule, and the footer always names the one in force.
The default `"math"` estimates the quantile of the *population*,
`inf{x : F(x) >= p}`. `"spicy"` switches to the type-7 interpolation
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
uses, for a reader who needs the two tables to agree cell for cell;
anything else – including a function – is handed to
[`survey::svyquantile()`](https://rdrr.io/pkg/survey/man/svyquantile.html)
untouched.

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

Both tables carry the typed view the rest of the package uses, so
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
`"flextable"`, `"word"`, `"excel"`, `"clipboard"`, plus `"data.frame"` /
`"long"` for the raw frame.

``` r

table_continuous_svy(
  dstrat,
  select = c(api00, api99),
  by = stype,
  output = "gt"
)
```

[TABLE]

N = 200 (weighted 6194). Design: stratified (stype), with finite
population correction; degrees of freedom vary by group (49 to 99).
Standard errors: Taylor linearisation (survey). Confidence intervals and
tests use the design degrees of freedom. Group comparison: design-based
Wald test. The group comparison uses 195 degrees of freedom (observed
groups only).

## What is not here yet

Two-phase, pps, database-backed and multiframe designs are refused with
a classed error rather than approximated: the delegation map was
measured on
[`svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html) and
[`as.svrepdesign()`](https://rdrr.io/pkg/survey/man/as.svrepdesign.html)
designs, and a plausible wrong number is worse than a refusal. Call
survey directly for those – and if one of them matters for your work, an
issue on the package’s tracker saying which design you need is the
fastest way to move it up the roadmap.

## References

- Lumley, T. (2004). Analysis of complex survey samples. *Journal of
  Statistical Software*, 9(1), 1–19.
- Lumley, T. (2010). *Complex surveys: A guide to analysis using R*.
  John Wiley & Sons.
- Lumley, T., & Scott, A. (2015). AIC and BIC for modeling with complex
  survey data. *Journal of Survey Statistics and Methodology*, 3(1),
  1–18.
