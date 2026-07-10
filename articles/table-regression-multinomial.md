# Multinomial regression tables

``` r

library(spicy)
library(nnet)
```

This vignette covers **multinomial logistic regression** — models for a
*nominal* outcome, a categorical response whose categories have no
natural order: employment status, party choice, transport mode. The
companion vignette [*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
covers the shared mechanics (`vcov`, `ci_level`, output formats,
multi-model layouts, broom integration), and [*Ordinal regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.md)
covers the case where the categories *are* ordered. Order alone does not
decide the model: the ordinal model buys parsimony — one slope per
predictor instead of \\J-1\\ — at the price of the parallel-regressions
constraint, and when that constraint is implausible, the multinomial
model is the standard unconstrained alternative (Long & Freese 2014), at
the cost of extra coefficients and category-by-category interpretation.
An approximate check compares the two fits directly by likelihood ratio
or AIC. Here we focus on what is specific to nominal outcomes: one
column of effects per outcome category, the choice of reference
category, and why a coefficient’s sign need not be the direction of its
effect on a probability.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
supports two engines:

- **[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)** —
  the baseline-category logit model on characteristics of the *person*
  (case-specific covariates); the social-science standard.
- **[`mlogit::mlogit()`](https://rdrr.io/pkg/mlogit/man/mlogit.html)** —
  McFadden’s conditional logit on characteristics of the *alternatives*
  (discrete-choice data).

The running example is employment status in the bundled `sochealth`
survey
([`?sochealth`](https://amaltawfik.github.io/spicy/reference/sochealth.md))
— four categories with `Employed` by far the most frequent:

``` r

d <- sochealth
table(d$employment_status)
#> 
#>   Employed    Student Unemployed   Inactive 
#>        762        143        174        121
```

One recoding first. `education` is stored as an *ordered* factor, and R
gives ordered factors polynomial contrasts by default — the model would
estimate `.L` (linear) and `.Q` (quadratic) components that fit
identically but are awkward to report. Applied tables conventionally
show treatment dummies against a reference level (Fox & Weisberg 2019),
so we convert to an unordered factor, preserving the level sequence:

``` r

d$education <- factor(as.character(d$education),
                      levels = c("Lower secondary", "Upper secondary",
                                 "Tertiary"))
```

## The baseline-category logit model in one paragraph

For an outcome with \\J\\ categories, the model fits \\J-1\\ logit
equations simultaneously by maximum likelihood, each comparing one
category against a common reference (the *baseline*): \\\log\[P(Y =
j)/P(Y = J)\] = \alpha_j + x^\top\beta_j\\ (Agresti 2013, ch. 8). Two
consequences shape the table. Each predictor gets **one coefficient per
non-reference outcome** — there is no single “effect of education”, only
its effect on each comparison — and the \\J-1\\ fitted equations
determine the logit for *every* pair of categories, so nothing is lost
by picking one baseline. Fitting separate binary logits instead of the
simultaneous model is consistent but less efficient; the loss is minor
when the baseline is the most frequent category (Begg & Gray 1984). In
the simultaneous fit the baseline changes nothing about the model, but
it does set which contrasts the table displays — and contrasts against a
well-populated category are precisely estimated, which is why Stata’s
`mlogit` defaults to the most frequent outcome.
[`multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) uses the
factor’s first level — here `Employed`, which is also the most frequent,
so R’s positional default coincides with the frequency-based choice:
large, so the displayed contrasts are precise, and substantively
meaningful.

## Basic table

`trace = FALSE` only silences the optimizer’s iteration log; unlike
`polr()`, [`multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)
needs no `Hess = TRUE`.

``` r

fit <- multinom(employment_status ~ age + sex + education,
                data = d, trace = FALSE)
table_regression(fit)
#> Multinomial logistic regression: employment_status
#> 
#>                                   Student             Unemployed           Inactive      
#>                             ────────────────────  ──────────────────  ────────────────── 
#>  Variable                 │    B      SE     p      B     SE     p      B     SE     p   
#> ──────────────────────────┼──────────────────────────────────────────────────────────────
#>  (Intercept)              │   -1.43  0.39  <.001  -0.72  0.33   .030  -1.75  0.40  <.001 
#>  age                      │   -0.01  0.01   .097  -0.00  0.01   .562   0.00  0.01   .719 
#>  sex:                     │                                                              
#>    Female (ref.)          │     –     –     –       –     –     –       –     –     –    
#>    Male                   │    0.12  0.18   .520   0.23  0.17   .172   0.14  0.20   .464 
#>  education:               │                                                              
#>    Lower secondary (ref.) │     –     –     –       –     –     –       –     –     –    
#>    Upper secondary        │    0.32  0.26   .224  -0.80  0.20  <.001  -0.37  0.25   .145 
#>    Tertiary               │    0.14  0.28   .614  -1.23  0.23  <.001  -0.35  0.26   .186 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1200                                                         
#>  R² (McFadden)            │    0.02                                                      
#>  R² (Nagelkerke)          │    0.04                                                      
#>  AIC                      │ 2515.8                                                       
#> 
#> Note. Multinomial logistic regression.
#> Std. errors: Wald asymptotic (z).
#> Reference outcome: Employed.
```

This is the layout multinomial results are published in: predictors as
rows, **one column group per non-reference outcome category**, so a
predictor’s effects on the \\J-1\\ comparisons sit side by side on its
row (Long & Freese 2014) — the arrangement most journals print. Raw
`mlogit` and NOMREG output instead stack the same \\J-1\\ equations as
row blocks; side by side is the publication convention. Reading it:

- Each column group is one **equation against the reference**: the
  `Tertiary` cell under `Unemployed` is the tertiary-education
  coefficient in the *Unemployed-versus-Employed* logit. The footer
  names the baseline once — `Reference outcome: Employed.` — Stata’s
  “base outcome” line; every cell in the table answers “compared to
  `Employed`”.
- The table is wide by construction, so the default trims each group to
  **B, SE, p** — the same compaction applied to multi-model tables.
  Atomic tokens restore anything, e.g.
  `show_columns = c("b", "ci", "p")` for CIs.
- To label the comparisons explicitly — the style some journals ask for
  — relabel the spanners:
  `outcome_labels = c("Student vs Employed", "Unemployed vs Employed", "Inactive vs Employed")`.
- The `(Intercept)` row is one intercept per equation — the log-odds of
  each category against `Employed` at the reference levels and age 0. It
  anchors the equations rather than carrying substantive interest.
- Inference is **Wald-z** (`df = Inf`), matching
  [`summary()`](https://rdrr.io/r/base/summary.html)-based practice for
  ML fits and Stata `mlogit`; `nnet` itself prints no p-values.
- The model-fit block reports **n** and **AIC**, once, under the first
  group — they belong to the model, not to an equation.

Before reading any cell, the whole model earns its keep: against the
intercept-only fit, the likelihood-ratio chi-squared is 41.9 on 12
degrees of freedom (p \< .001) — the test Stata prints in its `mlogit`
header and the first number Long & Freese read. McFadden’s pseudo-R² is
1 − (−1242.9)/(−1263.8) = 0.017: a comparative index, not a share of
explained variance (Long & Freese 2014) — the contrast between the tiny
value and the decisive test is exactly why it should not be read as one.

One scope note: `multinom` fits report **classical Wald-z inference
only** — a robust or cluster-robust `vcov` request is refused with an
explanatory error rather than silently ignored.

## Odds ratios: `exponentiate = TRUE`

`exponentiate = TRUE` puts the estimates and CI bounds on the ratio
scale:

``` r

table_regression(fit, exponentiate = TRUE, show_columns = c("b", "ci", "p"))
#> Multinomial logistic regression: employment_status
#> 
#>                                       Student                    Unemployed                  Inactive          
#>                             ────────────────────────────  ─────────────────────────  ───────────────────────── 
#>  Variable                 │   OR        95% CI       p     OR      95% CI       p     OR      95% CI       p   
#> ──────────────────────────┼────────────────────────────────────────────────────────────────────────────────────
#>  (Intercept)              │    0.24  [0.11, 0.51]  <.001  0.49  [0.25, 0.93]   .030  0.17  [0.08, 0.38]  <.001 
#>  age                      │    0.99  [0.98, 1.00]   .097  1.00  [0.99, 1.01]   .562  1.00  [0.99, 1.02]   .719 
#>  sex:                     │                                                                                    
#>    Female (ref.)          │     –         –         –      –         –         –      –         –         –    
#>    Male                   │    1.12  [0.79, 1.61]   .520  1.26  [0.90, 1.76]   .172  1.15  [0.79, 1.70]   .464 
#>  education:               │                                                                                    
#>    Lower secondary (ref.) │     –         –         –      –         –         –      –         –         –    
#>    Upper secondary        │    1.38  [0.82, 2.31]   .224  0.45  [0.30, 0.66]  <.001  0.69  [0.42, 1.13]   .145 
#>    Tertiary               │    1.15  [0.67, 1.98]   .614  0.29  [0.19, 0.46]  <.001  0.71  [0.42, 1.18]   .186 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1200                                                                               
#>  R² (McFadden)            │    0.02                                                                            
#>  R² (Nagelkerke)          │    0.04                                                                            
#>  AIC                      │ 2515.8                                                                             
#> 
#> Note. Multinomial logistic regression.
#> Std. errors: Wald asymptotic (z).
#> OR = odds ratio.
#> Coefficients exponentiated and displayed as OR; CI bounds exponentiated.
#> Reference outcome: Employed.
```

Each predictor cell is an **odds ratio against the reference outcome**
(the exponentiated intercepts are baseline *odds* — each category’s odds
versus `Employed` at the reference levels and age 0 — not ratios):
compared with lower-secondary graduates of the same age and sex,
tertiary graduates’ odds of being unemployed rather than employed are
0.29 times as large — a 71% reduction in those odds, the education
gradient the table is really about. Formally it is the odds ratio of the
*conditional* distribution restricted to the two categories in the
comparison (Agresti 2013).

One terminology note, because readers will meet it: Stata’s
`mlogit, rrr` labels these same numbers **relative-risk ratios** —
\\e^\beta\\ is also the multiplicative change in the probability ratio
of outcome \\j\\ versus the baseline — while SAS prints “Odds Ratio
Estimates” for its generalized logits. Same quantity, two readings;
`spicy` labels it `OR`, the reading that generalizes from binary
logistic regression.

## Changing the reference category

The reference is a *presentation* choice, not a modeling one: refit with
another baseline and it is the same model, reparameterized — same
likelihood, same AIC, same fitted probabilities; only the comparisons
displayed change.

``` r

d2 <- d
d2$employment_status <- relevel(d2$employment_status, ref = "Unemployed")
fit_unemp <- multinom(employment_status ~ age + sex + education,
                      data = d2, trace = FALSE)
c(AIC(fit), AIC(fit_unemp))
#> [1] 2515.786 2515.786
table_regression(fit_unemp, exponentiate = TRUE, show_columns = c("b", "p"))
#> Multinomial logistic regression: employment_status
#> 
#>                                Employed       Student     Inactive   
#>                             ──────────────  ───────────  ─────────── 
#>  Variable                 │   OR       p     OR     p     OR     p   
#> ──────────────────────────┼──────────────────────────────────────────
#>  (Intercept)              │    2.06   .030  0.49   .127  0.36   .030 
#>  age                      │    1.00   .562  0.99   .369  1.01   .476 
#>  sex:                     │                                          
#>    Female (ref.)          │     –     –      –     –      –     –    
#>    Male                   │    0.79   .172  0.89   .613  0.91   .706 
#>  education:               │                                          
#>    Lower secondary (ref.) │     –     –      –     –      –     –    
#>    Upper secondary        │    2.23  <.001  3.08  <.001  1.55   .129 
#>    Tertiary               │    3.43  <.001  3.94  <.001  2.43   .005 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1200                                     
#>  R² (McFadden)            │    0.02                                  
#>  R² (Nagelkerke)          │    0.04                                  
#>  AIC                      │ 2515.8                                   
#> 
#> Note. Multinomial logistic regression.
#> Std. errors: Wald asymptotic (z).
#> OR = odds ratio.
#> Coefficients exponentiated and displayed as OR; CI bounds exponentiated.
#> Reference outcome: Unemployed.
```

Against `Employed`, education looked irrelevant for the `Student`
contrast; against `Unemployed`, the whole `Tertiary` row is strong (OR
3.94, p \< .001 in the `Student` group; even the `Inactive` group
reaches p = .005; only `Upper secondary` in the `Inactive` group stays
inconclusive). The p-values moved because the *questions* changed, not
the model — a multinomial table only ever shows \\J-1\\ of the
\\J(J-1)/2\\ pairwise comparisons, and any of the others can be
recovered by releveling (Agresti 2013; Long 1997). Report the baseline
that makes your substantive comparisons direct.

## Does a predictor matter at all?

The per-cell p-values just moved with the baseline, so none of them
answers the question a reviewer asks first. The baseline-invariant
answer is the **joint likelihood-ratio test** that all \\J-1\\
coefficients of a predictor are zero — the first testing step of Long &
Freese (2014):

``` r

fit_noeduc <- multinom(employment_status ~ age + sex, data = d,
                       trace = FALSE)
anova(fit_noeduc, fit)
#>                   Model Resid. df Resid. Dev   Test    Df LR stat.     Pr(Chi)
#> 1             age + sex      3591   2522.079           NA       NA          NA
#> 2 age + sex + education      3585   2485.786 1 vs 2     6 36.29259 2.41817e-06
```

Education matters decisively — chi-squared 36.29 on 6 degrees of freedom
(2 parameters in each of 3 equations), p \< .001 — even though its
`Student`-equation cells looked unconvincing one table ago. The trap
runs in both directions: sex shows one tempting cell (`Unemployed`, p =
.172) but is jointly null (chi-squared 2.26 on 3 df, p = .520).
Individually weak cells can be jointly overwhelming, and one suggestive
cell among \\J-1\\ can be noise. `car::Anova(fit)` runs this test for
every predictor in one call — the equivalent of Stata’s `mlogtest, lr`.

## Can two categories be combined?

Before deciding how to report \\J\\ categories, the textbook workflow
asks whether all \\J\\ are distinguishable at all (Long & Freese 2014):
if no covariate separates outcomes \\m\\ and \\n\\, the data cannot tell
them apart, and the model may be estimating noise between them. The
likelihood-ratio version restricts the sample to the two categories and
tests a binary logit against the intercept-only model (Cramer & Ridder
1991):

``` r

sub <- droplevels(subset(d, employment_status %in% c("Student", "Inactive")))
y <- as.integer(sub$employment_status == "Student")
anova(glm(y ~ 1, binomial(), sub),
      glm(y ~ age + sex + education, binomial(), sub), test = "LRT")
#> Analysis of Deviance Table
#> 
#> Model 1: y ~ 1
#> Model 2: y ~ age + sex + education
#>   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#> 1       263     364.15                     
#> 2       259     357.20  4   6.9505   0.1385
```

`Student` and `Inactive` are not separated — chi-squared 6.95 on 4
degrees of freedom, p = .139. Running all six pairs tells the honest
story of these data: only `Unemployed` is sharply distinguished from the
rest (versus `Employed`, chi-squared 33.6; versus `Student`, 22.6; both
p \< .001), while `Employed`, `Student`, and `Inactive` are not mutually
separated by age, sex, and education. Two cautions close the step:
failing to reject does not prove interchangeability — the smaller cells
give the test little power — and combining is ultimately a substantive
decision: employment states remain distinct constructs even when these
covariates do not separate them.

## The sign trap, and marginal effects

The most common misreading of a multinomial table is to treat a
coefficient’s sign as the direction of the effect on that category’s
*probability*. The two need not agree: for a continuous \\x_k\\, the
marginal effect on \\P(Y=m)\\ is \\P_m(\beta\_{k,m} - \sum\_{j=1}^{J}
\beta\_{k,j} P_j)\\, with \\\beta\_{k,J} = 0\\ for the baseline so the
sum runs over all \\J\\ categories — the coefficient *minus a
probability-weighted average of every category’s coefficient* — so its
sign can differ from the coefficient’s, and can even change across the
range of \\x_k\\ (Long & Freese 2014; Wulff 2015). For a factor, the AME
below is instead the average difference in predicted probabilities
between levels; the same competition logic applies. The categories
compete for probability mass.

The probability-scale summary is the per-category **average marginal
effect** (AME): one AME column per outcome category, next to that
category’s coefficients:

``` r

table_regression(fit, show_columns = c("b", "ame"))
#> Multinomial logistic regression: employment_status
#> 
#>                                Student       Unemployed     Inactive    Employed 
#>                             ──────────────  ────────────  ────────────  ──────── 
#>  Variable                 │    B      AME     B     AME     B     AME   B   AME  
#> ──────────────────────────┼──────────────────────────────────────────────────────
#>  (Intercept)              │   -1.43         -0.72         -1.75                  
#>  age                      │   -0.01  -0.00  -0.00  -0.00   0.00   0.00      0.00 
#>  sex:                     │                                                      
#>    Female (ref.)          │     –             –             –                    
#>    Male                   │    0.12   0.01   0.23   0.02   0.14   0.01     -0.04 
#>  education:               │                                                      
#>    Lower secondary (ref.) │     –             –             –                    
#>    Upper secondary        │    0.32   0.05  -0.80  -0.12  -0.37  -0.02      0.09 
#>    Tertiary               │    0.14   0.04  -1.23  -0.16  -0.35  -0.01      0.14 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1200                                                 
#>  R² (McFadden)            │    0.02                                              
#>  R² (Nagelkerke)          │    0.04                                              
#>  AIC                      │ 2515.8                                               
#> 
#> Note. Multinomial logistic regression.
#> Std. errors: Wald asymptotic (z).
#> AME = average marginal effect on a response-category probability.
#> Reference outcome: Employed.
```

Three things to read off, using the `Tertiary` row:

- The **reference category is back** — as a last, AME-only column group.
  `Employed` has no coefficients (it is the baseline of every equation,
  so its `B` cells are empty), yet it carries the largest *positive*
  probability effect in the table (+0.14 for `Tertiary`, second in
  magnitude only to `Unemployed`’s −0.16). The log-odds side never shows
  this; the probability side must, because the effects on all \\J\\
  categories **sum to exactly zero** across each row (the \\J\\
  probabilities sum to 1, so whatever probability a predictor moves out
  of one category must land in the others; displayed values can miss 0
  only by rounding) — the mass that tertiary education moves out of
  `Unemployed` lands mainly in `Employed`.
- Cells are **on the probability scale**: −0.16 is a drop of 16
  **percentage points** in the probability of unemployment for tertiary
  relative to lower-secondary education, averaged over the observed
  covariates
  ([`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html);
  Williams 2012). Add `"ame_se"`, `"ame_ci"`, or `"ame_p"` to
  `show_columns` for delta-method inference on these effects.
- Here every AME happens to share its coefficient’s sign; do not count
  on it. When an outcome’s coefficient sits close to the
  probability-weighted average of all the coefficients, the probability
  effect can flip sign — the reason Wulff (2015) recommends never
  interpreting a multinomial model from its coefficient signs alone.

An AME is an average of *changes*; the concrete interpretation device of
Long & Freese (2014) is the **predicted probabilities** those changes
move between, evaluated at named profiles:

``` r

profiles <- data.frame(
  education = levels(d$education),
  sex       = "Female",
  age       = mean(d$age[!is.na(d$employment_status)])
)
round(predict(fit, newdata = profiles, type = "probs"), 3)
#>   Employed Student Unemployed Inactive
#> 1    0.571   0.082      0.235    0.112
#> 2    0.659   0.130      0.121    0.089
#> 3    0.703   0.116      0.084    0.097
```

Read the `Unemployed` column down the education gradient: 0.235, 0.121,
0.084. For this profile — a woman of average age — the −0.16 AME is a
fall from roughly one in four to under one in ten; the baseline matters
as much as the change. (The profile’s own gap, −0.151, sits close to the
sample-averaged −0.160 — they answer slightly different questions.) Base
[`predict()`](https://rdrr.io/r/stats/predict.html) returns no standard
errors for these predictions;
[`marginaleffects::avg_predictions()`](https://rdrr.io/pkg/marginaleffects/man/predictions.html)
supplies delta-method inference on the same quantities.

## An ordinal predictor: scores or dummies?

`education` is ordered — Lower secondary \< Upper secondary \< Tertiary
— and the dummy coding above ignores that. Whether to enter an ordinal
predictor as **numeric scores** (one slope) or as dummies is a general
model-building decision, not a multinomial one: the workflow — fit both
codings, compare with `nested = TRUE`, and keep the scores only when the
likelihood-ratio test finds no departure from a linear trend — is
developed with its caveats in
[`vignette("categorical-predictors")`](https://amaltawfik.github.io/spicy/articles/categorical-predictors.md).
One point *is* multinomial-specific: each freed coding costs one
parameter **per equation**, so the test’s degrees of freedom multiply by
the number of non-reference outcomes:

``` r

d$educ_score <- as.numeric(d$education)   # 1 / 2 / 3
fit_lin <- multinom(employment_status ~ age + sex + educ_score,
                    data = d, trace = FALSE)
table_regression(list(fit_lin, fit), nested = TRUE,
                 show_columns = c("b", "p"))
#> Hierarchical multinomial logistic regression: employment_status
#> 
#>                                            Model 1          Model 2     
#>                                         ──────────────  ─────────────── 
#>  Variable                             │    B       p       B        p   
#> ──────────────────────────────────────┼─────────────────────────────────
#>  (Intercept):                         │                                 
#>    Student: (Intercept)               │   -1.27   .002    -1.43   <.001 
#>    Unemployed: (Intercept)            │   -0.16   .663    -0.72    .030 
#>    Inactive: (Intercept)              │   -1.71  <.001    -1.75   <.001 
#>  age:                                 │                                 
#>    Student: age                       │   -0.01   .090    -0.01    .097 
#>    Unemployed: age                    │   -0.00   .581    -0.00    .562 
#>    Inactive: age                      │    0.00   .697     0.00    .719 
#>  sex:                                 │                                 
#>    Student: Female (ref.)             │     –     –         –      –    
#>    Unemployed: Female (ref.)          │     –     –         –      –    
#>    Inactive: Female (ref.)            │     –     –         –      –    
#>    Student: Male                      │    0.11   .549     0.12    .520 
#>    Unemployed: Male                   │    0.24   .160     0.23    .172 
#>    Inactive: Male                     │    0.15   .444     0.14    .464 
#>  educ_score:                          │                                 
#>    Student: educ_score                │    0.03   .843                  
#>    Unemployed: educ_score             │   -0.64  <.001                  
#>    Inactive: educ_score               │   -0.16   .239                  
#>  education:                           │                                 
#>    Student: Lower secondary (ref.)    │                     –      –    
#>    Unemployed: Lower secondary (ref.) │                     –      –    
#>    Inactive: Lower secondary (ref.)   │                     –      –    
#>    Student: Upper secondary           │                    0.32    .224 
#>    Unemployed: Upper secondary        │                   -0.80   <.001 
#>    Inactive: Upper secondary          │                   -0.37    .145 
#>    Student: Tertiary                  │                    0.14    .614 
#>    Unemployed: Tertiary               │                   -1.23   <.001 
#>    Inactive: Tertiary                 │                   -0.35    .186 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                                    │ 1200            1200            
#>  R² (McFadden)                        │    0.01            0.02         
#>  R² (Nagelkerke)                      │    0.03            0.04         
#>  AIC                                  │ 2514.3          2515.8          
#>  Δχ²                                  │     –             +4.52         
#>  p (change)                           │     –               .211        
#> 
#> Note. Multinomial logistic regression models.
#> Std. errors: Wald asymptotic (z).
#> Reference outcome: Employed.
```

Freeing education from the linear trend buys a chi-squared of 4.52 on 3
degrees of freedom (one per equation), p = .211 — no evidence the
dummies improve on the scores, and AIC agrees (2514.3 vs 2515.8). On
Agresti’s (2007, sec. 4.4.3) grounds the scores model is then
preferable: simpler to report, and more powerful against a genuine trend
— an advantage that reverses if the true pattern is non-monotone. See
the categorical-predictors vignette for the caveats that keep this
decision honest (equal spacing, pre-testing, and when to simply keep the
dummies).

## Independence of irrelevant alternatives, briefly

The multinomial logit assumes the odds between any two categories do not
depend on what other alternatives exist — the *independence of
irrelevant alternatives* (IIA), of red-bus/blue-bus fame (the critique
goes back to Debreu 1960). Formal tests exist (Hausman & McFadden 1984;
Small & Hsiao 1985), but simulation evidence shows them to be
unsatisfactory for applied work — different tests reach conflicting
verdicts on the same data (Cheng & Long 2007) — and Long & Freese (2014)
do not recommend them. The working advice remains McFadden’s (1974): use
the model when the alternatives are “distinct and weighed independently”
— not close substitutes. For a fixed, exhaustive set like employment
status, where no alternative is ever added or removed, IIA is rarely the
practical concern it is in choice modeling; where alternatives genuinely
substitute (two bus lines), the remedy is a different model (nested or
mixed logit), not a mechanical test.

## Discrete choice: `mlogit`

[`multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) models the
chooser; McFadden’s **conditional logit** models the *choices*. When
covariates describe the alternatives — the price of each travel mode,
the catch rate of each fishing mode — the coefficient is one per
*attribute*, shared across alternatives (McFadden 1974; Croissant 2020).
The `mlogit` engine renders both kinds of covariate in one table. The
classic `Fishing` data: 1182 anglers choose among four modes; `price`
and `catch` vary by alternative, while `income_k` (income in thousands
of dollars) describes the angler:

``` r

library(mlogit)
data("Fishing", package = "mlogit")
Fishing$income_k <- Fishing$income / 1000
Fish <- dfidx(Fishing, varying = 2:9, choice = "mode", shape = "wide")

fit_choice <- mlogit(mode ~ price + catch | income_k, data = Fish)
table_regression(fit_choice, exponentiate = TRUE,
                 show_columns = c("b", "ci", "p"))
#> Discrete-choice multinomial logit (mlogit): mode
#> 
#>  Variable               │   OR        95% CI       p   
#> ────────────────────────┼──────────────────────────────
#>  Alternative-invariant: │                              
#>    price                │    0.98  [0.97, 0.98]  <.001 
#>    catch                │    1.43  [1.15, 1.77]   .001 
#>  boat:                  │                              
#>    (Intercept)          │    1.69  [1.09, 2.62]   .018 
#>    income_k             │    1.09  [0.99, 1.21]   .074 
#>  charter:               │                              
#>    (Intercept)          │    5.44  [3.51, 8.44]  <.001 
#>    income_k             │    0.97  [0.88, 1.07]   .508 
#>  pier:                  │                              
#>    (Intercept)          │    2.18  [1.41, 3.35]  <.001 
#>    income_k             │    0.88  [0.80, 0.97]   .012 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                      │ 1182                         
#>  AIC                    │ 2446.3                       
#> 
#> Note. Discrete-choice multinomial logit (mlogit).
#> Std. errors: Wald asymptotic (z).
#> OR = odds ratio.
#> Coefficients exponentiated and displayed as OR; CI bounds exponentiated.
#> Reference alternative: beach.
```

The table comes in **two segments**, the presentation Stata’s `asclogit`
prints. The `Alternative-invariant` section holds the model’s
centerpiece: the attribute coefficients (`price`, `catch`), one row
each, shared across alternatives — one more dollar on a mode’s own price
multiplies the odds of choosing that mode over any other alternative by
0.98, other modes’ prices held fixed, *whichever mode it is*. Then one
section per non-reference alternative (`boat`, `charter`, `pier`)
collects that alternative’s intercept and its **case-specific**
coefficients: `income_k` enters as it would under
[`multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html), one
coefficient per comparison against the reference — the footer’s
`Reference alternative: beach.` names it, so each section reads “this
alternative versus beach”. The footer’s `n` counts **choice situations**
(1182 anglers), not the 4728 rows of the long-format data: each angler
contributes one choice, and that is the model’s sample size.

Three guardrails: average marginal effects are refused for `mlogit`
(`marginaleffects` has no slopes method for its one-row-per-choice
structure); the robust `vcov` family is `CR0`–`CR3` only, with one
cluster value per choice situation, not per long-format row; and `HC*`
is refused outright —
[`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)
mis-scales the sandwich for mlogit’s per-choice-situation scores.

## Several models side by side

Multinomial fits can be compared like fits of any other model class —
here the education gradient before and after adjustment. With several
models the column groups are needed for the *models*, so the table falls
back to the one-row-per-comparison layout (`Student: Tertiary` rows), as
does `nested = TRUE` above:

``` r

fit_min <- multinom(employment_status ~ education, data = d, trace = FALSE)
table_regression(list(Unadjusted = fit_min, Adjusted = fit),
                 exponentiate = TRUE, show_columns = c("b", "p"))
#> Multinomial logistic regression comparison: employment_status
#> 
#>                                           Unadjusted       Adjusted    
#>                                         ──────────────  ────────────── 
#>  Variable                             │   OR       p      OR       p   
#> ──────────────────────────────────────┼────────────────────────────────
#>  (Intercept):                         │                                
#>    Student: (Intercept)               │    0.15  <.001     0.24  <.001 
#>    Unemployed: (Intercept)            │    0.46  <.001     0.49   .030 
#>    Inactive: (Intercept)              │    0.21  <.001     0.17  <.001 
#>  education:                           │                                
#>    Student: Lower secondary (ref.)    │     –     –         –     –    
#>    Unemployed: Lower secondary (ref.) │     –     –         –     –    
#>    Inactive: Lower secondary (ref.)   │     –     –         –     –    
#>    Student: Upper secondary           │    1.38   .225     1.38   .224 
#>    Unemployed: Upper secondary        │    0.45  <.001     0.45  <.001 
#>    Inactive: Upper secondary          │    0.69   .141     0.69   .145 
#>    Student: Tertiary                  │    1.14   .640     1.15   .614 
#>    Unemployed: Tertiary               │    0.29  <.001     0.29  <.001 
#>    Inactive: Tertiary                 │    0.71   .196     0.71   .186 
#>  age:                                 │                                
#>    Student: age                       │                    0.99   .097 
#>    Unemployed: age                    │                    1.00   .562 
#>    Inactive: age                      │                    1.00   .719 
#>  sex:                                 │                                
#>    Student: Female (ref.)             │                     –     –    
#>    Unemployed: Female (ref.)          │                     –     –    
#>    Inactive: Female (ref.)            │                     –     –    
#>    Student: Male                      │                    1.12   .520 
#>    Unemployed: Male                   │                    1.26   .172 
#>    Inactive: Male                     │                    1.15   .464 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                                    │ 1200            1200           
#>  R² (McFadden)                        │    0.01            0.02        
#>  R² (Nagelkerke)                      │    0.03            0.04        
#>  AIC                                  │ 2509.4          2515.8         
#> 
#> Note. Multinomial logistic regression models.
#> Std. errors: Wald asymptotic (z).
#> OR = odds ratio.
#> Coefficients exponentiated and displayed as OR; CI bounds exponentiated.
#> Reference outcome: Employed.
```

Adjustment changes nothing here: the education block is identical to two
decimals across the columns — `Tertiary` is OR 0.29 in the
`Unemployed`-versus-`Employed` equation both times — because age and
sex, at best weakly related to employment status in this sample (their
smallest p is .097), have nothing to confound the education gradient
with.

## Output formats

Everything above used the default console output. The same table renders
as a raw data frame, a long broom-style tibble, or — with the
corresponding Suggests package — a rich `gt`, `flextable`, `tinytable`,
Excel, or Word table:

``` r

head(table_regression(fit, exponentiate = TRUE, output = "data.frame"))
#>          Variable Student: OR Student: SE Student: p Unemployed: OR Unemployed: SE Unemployed: p Inactive: OR Inactive: SE
#> 1     (Intercept)        0.24        0.09      <.001           0.49           0.16          .030         0.17         0.07
#> 2             age        0.99        0.01       .097           1.00           0.01          .562         1.00         0.01
#> 3            sex:                                                                                                         
#> 4   Female (ref.)         –           –         –               –              –            –             –            –  
#> 5            Male        1.12        0.21       .520           1.26           0.22          .172         1.15         0.23
#> 6      education:                                                                                                         
#>   Inactive: p
#> 1       <.001
#> 2        .719
#> 3            
#> 4        –   
#> 5        .464
#> 6
```

``` r

table_regression(fit, show_columns = c("b", "ame"), output = "gt")
```

[TABLE]

*Note.* Multinomial logistic regression. Std. errors: Wald asymptotic
(z). AME = average marginal effect on a response-category probability.
Reference outcome: Employed.

[`broom::tidy()`](https://broom.tidymodels.org) always returns the long
frame, one row per `(term, estimate_type)`, whatever the display layout
— the per-outcome structure is carried in the term labels
(`"Student: age"`), and AME rows cover all \\J\\ categories including
the reference:

``` r

broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
#> # A tibble: 31 × 16
#>    model_id outcome      outcome_level term  estimate_type estimate std.error conf.low conf.high statistic    df p.value test_type
#>    <chr>    <chr>        <chr>         <chr> <chr>            <dbl>     <dbl>    <dbl>     <dbl>     <dbl> <dbl>   <dbl> <chr>    
#>  1 M1       employment_… Student       Stud… B             -1.43e+0  0.386    -2.18e+0 -0.672       -3.70    Inf 2.13e-4 z        
#>  2 M1       employment_… Unemployed    Unem… B             -7.23e-1  0.333    -1.38e+0 -0.0716      -2.18    Inf 2.96e-2 z        
#>  3 M1       employment_… Inactive      Inac… B             -1.75e+0  0.400    -2.53e+0 -0.965       -4.37    Inf 1.23e-5 z        
#>  4 M1       employment_… Student       Stud… ame           -1.06e-3  0.000641 -2.32e-3  0.000199    -1.65    Inf 9.89e-2 z        
#>  5 M1       employment_… Student       Stud… B             -1.04e-2  0.00627  -2.27e-2  0.00188     -1.66    Inf 9.70e-2 z        
#>  6 M1       employment_… Unemployed    Unem… ame           -2.72e-4  0.000683 -1.61e-3  0.00107     -0.398   Inf 6.91e-1 z        
#>  7 M1       employment_… Unemployed    Unem… B             -3.38e-3  0.00583  -1.48e-2  0.00804     -0.580   Inf 5.62e-1 z        
#>  8 M1       employment_… Inactive      Inac… ame            3.91e-4  0.000591 -7.67e-4  0.00155      0.661   Inf 5.08e-1 z        
#>  9 M1       employment_… Inactive      Inac… B              2.40e-3  0.00667  -1.07e-2  0.0155       0.360   Inf 7.19e-1 z        
#> 10 M1       employment_… Employed      Empl… ame            9.39e-4  0.000941 -9.06e-4  0.00278      0.998   Inf 3.18e-1 z        
#> # ℹ 21 more rows
#> # ℹ 3 more variables: is_intercept <lgl>, factor_term <chr>, factor_level <chr>
```

## References

- Agresti, A. (2007). *An Introduction to Categorical Data Analysis*
  (2nd ed.). Wiley.
- Agresti, A. (2013). *Categorical Data Analysis* (3rd ed.). Wiley.
- Begg, C. B., & Gray, R. (1984). Calculation of polychotomous logistic
  regression parameters using individualized regressions. *Biometrika*,
  71(1), 11–18.
- Cheng, S., & Long, J. S. (2007). Testing for IIA in the multinomial
  logit model. *Sociological Methods & Research*, 35(4), 583–600.
- Croissant, Y. (2020). Estimation of random utility models in R: The
  mlogit package. *Journal of Statistical Software*, 95(11), 1–41.
- Debreu, G. (1960). Review of R. D. Luce, *Individual Choice Behavior*.
  *American Economic Review*, 50(1), 186–188.
- Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression*
  (3rd ed.). Sage.
- Hausman, J. A., & McFadden, D. (1984). Specification tests for the
  multinomial logit model. *Econometrica*, 52(5), 1219–1240.
- Long, J. S. (1997). *Regression Models for Categorical and Limited
  Dependent Variables*. Sage.
- Long, J. S., & Freese, J. (2014). *Regression Models for Categorical
  Dependent Variables Using Stata* (3rd ed.). Stata Press.
- McFadden, D. (1974). Conditional logit analysis of qualitative choice
  behavior. In P. Zarembka (Ed.), *Frontiers in Econometrics*
  (pp. 105–142). Academic Press.
- Small, K. A., & Hsiao, C. (1985). Multinomial logit specification
  tests. *International Economic Review*, 26(3), 619–627.
- Williams, R. (2012). Using the margins command to estimate and
  interpret adjusted predictions and marginal effects. *The Stata
  Journal*, 12(2), 308–331.
- Wulff, J. N. (2015). Interpreting results from the multinomial logit
  model: Demonstrated by foreign market entry. *Organizational Research
  Methods*, 18(2), 300–325.
