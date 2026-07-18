# Count and two-part regression tables

``` r

library(spicy)
library(pscl)     # zeroinfl(), hurdle()
```

This vignette covers **count regression** — models for a non-negative
integer outcome such as number of publications, doctor visits, or
species counts. The companion vignette [*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
covers the shared mechanics (`vcov`, `ci_level`, output formats,
multi-model layouts, broom integration); here we focus on what is
specific to counts: rate ratios, overdispersion, and above all the
**two-part models** whose zero component
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
renders as its own labelled block.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
supports the full count-model escalation:

- **`stats::glm(family = poisson())`** — the Poisson baseline;
- **[`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html)** —
  negative binomial, for overdispersion;
- **[`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html)**
  and **[`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html)** —
  the two-part models (Zeileis, Kleiber & Jackman 2008);
- **[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html)**
  — all of the above with random effects.

The running example is
[`pscl::bioChemists`](https://rdrr.io/pkg/pscl/man/bioChemists.html)
(Long 1990): the number of articles published by 915 biochemistry PhD
students in the last three years of their doctorate, with gender,
marital status, children under six, and the mentor’s article count as
predictors. Thirty percent of the students published nothing — a
striking share that on its own proves nothing: whether those zeros are
*excess* is a question about a fitted count model, not about the raw
percentage (Long 1997). The model-comparison section below answers it.

``` r

data("bioChemists", package = "pscl")
mean(bioChemists$art == 0)
#> [1] 0.3005464
```

## Count models in one paragraph

A Poisson regression models the *rate* of events through a log link, so
`exponentiate = TRUE` turns each coefficient into an **incidence rate
ratio** (IRR) — a multiplicative effect on the expected count. Real
count data usually violate the Poisson assumption in one or both of two
ways: the conditional variance exceeds the conditional mean
(**overdispersion** — the negative binomial’s job), and there are more
zeros than the count process predicts (**excess zeros** — the job of the
zero-inflated and hurdle models, which add a second, binary submodel for
the zeros). The two are entangled: unmodelled dispersion surfaces as
apparent excess zeros, so the model comparison at the end of this
vignette, not the raw zero count, arbitrates. Each extension changes
what the table must show, and the sections below follow that escalation.

## Poisson baseline

``` r

fit_pois <- glm(art ~ fem + mar + kid5 + ment, data = bioChemists,
                family = poisson())
table_regression(fit_pois, exponentiate = TRUE)
#> Poisson regression: art
#> 
#>  Variable        │   IRR     SE      95% CI       p   
#> ─────────────────┼────────────────────────────────────
#>  (Intercept)     │    1.41  0.08  [1.26, 1.59]  <.001 
#>  fem:            │                                    
#>    Men (ref.)    │     –     –         –         –    
#>    Women         │    0.80  0.04  [0.72, 0.89]  <.001 
#>  mar:            │                                    
#>    Single (ref.) │     –     –         –         –    
#>    Married       │    1.16  0.07  [1.03, 1.31]   .013 
#>  kid5            │    0.83  0.03  [0.77, 0.90]  <.001 
#>  ment            │    1.03  0.00  [1.02, 1.03]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │  915                               
#>  R² (McFadden)   │    0.05                            
#>  R² (Nagelkerke) │    0.19                            
#>  AIC             │ 3312.3                             
#> 
#> Note. Poisson regression.
#> Std. errors: classical (Fisher information).
#> IRR = incidence rate ratio.
#> Coefficients exponentiated and displayed as IRR; SE on the IRR scale (delta method); CI bounds exponentiated (asymmetric).
```

Each IRR multiplies the expected article count, holding the other
predictors constant: the expected count for women is about 20% lower
than for otherwise-similar men (IRR 0.80), and each additional mentor
publication raises a student’s expected count by 2.6% (IRR 1.03 as
displayed; exp(0.0256) = 1.026). Inference is Wald-z on the link scale;
the footer records the SE and CI conventions.

One remark for rate data: the observation window here is a fixed three
years for everyone, so no exposure adjustment is needed. With unequal
exposure — person-years, plot area, time at risk — add
`offset(log(exposure))` to the formula as usual; the offset is absorbed
silently (no spurious coefficient row). The model then targets the rate
per unit of exposure — articles per person-year, counts per hectare —
and the exponentiated coefficients are still rate ratios, read exactly
as before.

## Overdispersion: the negative binomial

Poisson regression assumes the **conditional** variance equals the
conditional mean. A raw comparison of the marginal moments does not by
itself establish otherwise — covariates alone push the marginal variance
above the marginal mean — so the check belongs to the fitted model: the
Pearson dispersion statistic of the Poisson fit is 1.82, where
equidispersion would give roughly 1.

``` r

c(mean = mean(bioChemists$art), var = var(bioChemists$art))
#>     mean      var 
#> 1.692896 3.709742
sum(residuals(fit_pois, "pearson")^2) / df.residual(fit_pois)
#> [1] 1.824161
```

A Poisson model answers such overdispersion by **understating the
standard errors**.
[`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) fits the
NB2 model, adding a dispersion parameter \\\theta\\ with \\V(Y \mid x) =
\mu + \mu^2/\theta\\ — smaller \\\theta\\ means stronger overdispersion,
and the Poisson is the limiting case as \\\theta\\ grows without bound:

``` r

fit_nb <- MASS::glm.nb(art ~ fem + mar + kid5 + ment, data = bioChemists)
table_regression(fit_nb, exponentiate = TRUE)
#> Negative-binomial regression: art
#> 
#>  Variable        │   IRR     SE      95% CI       p   
#> ─────────────────┼────────────────────────────────────
#>  (Intercept)     │    1.35  0.11  [1.15, 1.59]  <.001 
#>  fem:            │                                    
#>    Men (ref.)    │     –     –         –         –    
#>    Women         │    0.81  0.06  [0.70, 0.93]   .003 
#>  mar:            │                                    
#>    Single (ref.) │     –     –         –         –    
#>    Married       │    1.16  0.09  [0.99, 1.36]   .072 
#>  kid5            │    0.84  0.04  [0.76, 0.93]  <.001 
#>  ment            │    1.03  0.00  [1.02, 1.04]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │  915                               
#>  R² (McFadden)   │    0.03                            
#>  R² (Nagelkerke) │    0.11                            
#>  AIC             │ 3134.1                             
#> 
#> Note. Negative-binomial regression.
#> Std. errors: Model-based (asymptotic).
#> IRR = incidence rate ratio.
#> Coefficients exponentiated and displayed as IRR; SE on the IRR scale (delta method); CI bounds exponentiated (asymmetric).
```

Here `summary(fit_nb)` reports \\\hat\theta\\ = 2.26 (SE 0.27); readers
trained on Stata know its reciprocal, \\\alpha = 1/\theta\\ = 0.44 —
both available as opt-in fit-stat rows via
`show_fit_stats = c("nobs", "theta", "alpha", "aic")`, so the table can
carry the dispersion estimate itself. The likelihood-ratio test of the
Poisson restriction — a boundary test, so its p-value is halved —
rejects decisively (LR = 180.3, p \< .001).
[`glmmTMB::nbinom2`](https://rdrr.io/pkg/glmmTMB/man/nbinom2.html), used
in the mixed-model section below, is the same NB2 parameterization.

The IRRs barely move, but the inference does: the evidence for marriage
weakens (p = .013 under Poisson, p = .072 here) — the Poisson
significance was an artifact of ignored overdispersion. AIC falls from
3312.3 to 3134.1, and that comparison returns below.

## Excess zeros: zero-inflation and hurdle

Two-part models tell two different stories about the zeros, and the
difference matters for interpretation:

- A **zero-inflated** model
  ([`zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html);
  Lambert 1992) says some zeros are *structural*: they come from a
  latent group that would never publish, regardless of the count
  process. Its zero component models the probability of being such a
  **structural zero**.
- A **hurdle** model
  ([`hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html); Mullahy 1986)
  says all zeros have a single source, the hurdle: first you publish at
  all or you do not; a truncated count process then decides how many.
  Its zero component models the probability of a **nonzero count** — the
  *opposite direction*.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
renders the count coefficients first and the zero component as a
labelled block, with a footer line stating exactly what the block
models. Under `exponentiate = TRUE` the exponentiation is **per block**:
count coefficients become IRRs for the underlying count process — for
`zeroinfl`, the rate among units not in the structural-zero class; for
`hurdle`, the latent untruncated rate — not multiplicative effects on
the overall expected count E(Y); the zero component’s logit coefficients
become odds ratios. (For a single number on the E(Y) scale, see the
combined AME below.)

In the two-part formula the segment after `|` specifies the zero model.
Here all four covariates drive how much publishers publish, while only
the mentor’s productivity governs membership in the never-publishing
class — a deliberate restriction of the full inflation equation (Zeileis
et al. 2008). The inflation equation is a substantive hypothesis to
specify, not a default to accept.

``` r

fit_zip <- zeroinfl(art ~ fem + mar + kid5 + ment | ment,
                    data = bioChemists)
table_regression(fit_zip, exponentiate = TRUE)
#> Poisson zero-inflated regression: art
#> 
#>  Variable        │   IRR     SE      95% CI       p   
#> ─────────────────┼────────────────────────────────────
#>  (Intercept)     │    1.84  0.12  [1.61, 2.10]  <.001 
#>  fem:            │                                    
#>    Men (ref.)    │     –     –         –         –    
#>    Women         │    0.80  0.05  [0.72, 0.90]  <.001 
#>  mar:            │                                    
#>    Single (ref.) │     –     –         –         –    
#>    Married       │    1.14  0.08  [1.01, 1.30]   .041 
#>  kid5            │    0.85  0.04  [0.78, 0.93]  <.001 
#>  ment            │    1.02  0.00  [1.01, 1.02]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Zero-inflation: │                                    
#>    (Intercept)   │    0.50  0.10  [0.34, 0.75]  <.001 
#>    ment          │    0.88  0.04  [0.81, 0.95]   .001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │  915                               
#>  AIC             │ 3225.5                             
#> 
#> Note. Poisson zero-inflated regression.
#> Std. errors: Wald asymptotic (z).
#> Zero-inflation component: log-odds of a structural (excess) zero. Coefficients exponentiated and displayed as odds ratios.
#> IRR = incidence rate ratio.
#> Coefficients exponentiated and displayed as IRR; SE on the IRR scale (delta method); CI bounds exponentiated (asymmetric).
```

``` r

fit_hur <- hurdle(art ~ fem + mar + kid5 + ment | ment,
                  data = bioChemists)
table_regression(fit_hur, exponentiate = TRUE)
#> Poisson hurdle regression: art
#> 
#>  Variable        │   IRR     SE      95% CI       p   
#> ─────────────────┼────────────────────────────────────
#>  (Intercept)     │    1.88  0.13  [1.63, 2.16]  <.001 
#>  fem:            │                                    
#>    Men (ref.)    │     –     –         –         –    
#>    Women         │    0.80  0.05  [0.70, 0.90]  <.001 
#>  mar:            │                                    
#>    Single (ref.) │     –     –         –         –    
#>    Married       │    1.10  0.08  [0.96, 1.27]   .169 
#>  kid5            │    0.87  0.04  [0.79, 0.95]   .003 
#>  ment            │    1.02  0.00  [1.01, 1.02]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Zero hurdle:    │                                    
#>    (Intercept)   │    1.28  0.14  [1.04, 1.58]   .021 
#>    ment          │    1.08  0.01  [1.06, 1.11]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │  915                               
#>  AIC             │ 3233.8                             
#> 
#> Note. Poisson hurdle regression.
#> Std. errors: Wald asymptotic (z).
#> Zero hurdle component: log-odds of a nonzero count. Coefficients exponentiated and displayed as odds ratios.
#> IRR = incidence rate ratio.
#> Coefficients exponentiated and displayed as IRR; SE on the IRR scale (delta method); CI bounds exponentiated (asymmetric).
```

Compare the `ment` row in the two zero blocks — the same predictor, the
same substantive finding, and *opposite-looking* odds ratios:

- Zero-inflation block: OR 0.88 — each mentor publication **lowers the
  odds of being a structural zero**.
- Zero hurdle block: OR 1.08 — each mentor publication **raises the odds
  of publishing at all**.

Both blocks say the same thing: a productive mentor makes an observed
zero less likely. Reading either number without its block label invites
a sign error; the block labels and their footer lines exist to prevent
it.

As for choosing between the two: AIC leans toward the zero-inflated
variant here (3225.5 vs 3233.8; the two models have the same number of
parameters, so this is a pure likelihood comparison) — though both
Poisson two-part models still trail the plain negative binomial of the
previous section, a comparison the next section takes up. The real
choice is substantive, not statistical: no information criterion can
tell you whether the zeros are structural or sampling zeros.
Zero-inflation fits when some units plausibly *can never* experience the
event (a latent never-publisher class); a hurdle fits when no one is
immune — all zeros arise from a single all-or-nothing first stage, one
process, with its own covariates, deciding whether anything is published
at all. One mechanical difference also separates them: zero inflation
can only *add* zeros to the count process, whereas the hurdle frees the
zero probability in either direction — it alone can accommodate *fewer*
zeros than the count process implies (Cameron & Trivedi 2013).

Zero-component coefficients test substantive hypotheses: they take
significance stars, and `p_adjust` corrections treat them as part of the
same family as the count-component coefficients. To display only the
count part, set `show_components = FALSE` (the model is still estimated
in full).

## Which count model? Side by side

Before comparing likelihoods, run the diagnostic the zero question
actually asks: are there more zeros than a fitted count model implies?
Compare the observed zero share with the share each model predicts:

``` r

c(observed = mean(bioChemists$art == 0),
  poisson  = mean(dpois(0, fitted(fit_pois))),
  negbin   = mean(dnbinom(0, mu = fitted(fit_nb), size = fit_nb$theta)))
#>  observed   poisson    negbin 
#> 0.3005464 0.2090480 0.3035314
```

The Poisson underpredicts the zeros badly (0.21 against 0.30 observed),
but the negative binomial reproduces them almost exactly (0.30) —
*without any zero component at all*. The apparent excess of zeros was
overdispersion. This is the point Long (1997) and Cameron & Trivedi
(2013) make, and the AIC row now confirms it. (The once-standard Vuong
test is not used here: it is no longer recommended for testing
zero-inflation — Wilson 2015 — so information criteria and substantive
reasoning carry the comparison.)

``` r

fit_zinb <- zeroinfl(art ~ fem + mar + kid5 + ment | ment,
                     data = bioChemists, dist = "negbin")
table_regression(
  list(Poisson = fit_pois, ZIP = fit_zip, NB = fit_nb, ZINB = fit_zinb),
  show_columns = "b", exponentiate = TRUE
)
#> Regression comparison: art
#> 
#>                    Poisson    ZIP      NB      ZINB   
#>                    ───────  ───────  ───────  ─────── 
#>  Variable        │   IRR      IRR      IRR      IRR   
#> ─────────────────┼────────────────────────────────────
#>  (Intercept)     │    1.41     1.84     1.35     1.51 
#>  fem:            │                                    
#>    Men (ref.)    │     –        –        –        –   
#>    Women         │    0.80     0.80     0.81     0.81 
#>  mar:            │                                    
#>    Single (ref.) │     –        –        –        –   
#>    Married       │    1.16     1.14     1.16     1.15 
#>  kid5            │    0.83     0.85     0.84     0.85 
#>  ment            │    1.03     1.02     1.03     1.02 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Zero-inflation: │                                    
#>    (Intercept)   │             0.50              0.45 
#>    ment          │             0.88              0.54 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │  915      915      915      915    
#>  R² (McFadden)   │    0.05              0.03          
#>  R² (Nagelkerke) │    0.19              0.11          
#>  AIC             │ 3312.3   3225.5   3134.1   3122.5  
#> 
#> Note. Model 1: Poisson regression; Model 2: Poisson zero-inflated regression; Model 3: negative-binomial regression; Model 4: negative-binomial zero-inflated regression.
#> Std. errors:
#>   Model 1: classical (Fisher information)
#>   Model 2: Wald asymptotic (z)
#>   Model 3: Model-based (asymptotic)
#>   Model 4: Wald asymptotic (z)
#> Zero-inflation component: log-odds of a structural (excess) zero. Coefficients exponentiated and displayed as odds ratios.
#> IRR = incidence rate ratio.
#> Coefficients exponentiated and displayed as IRR; CI bounds exponentiated.
```

Read the AIC row across. The ZIP’s entire inflation component buys less
than the negative binomial’s single dispersion parameter (3225.5 against
3134.1): modelling the zeros directly is the *worse* answer to
overdispersion here. On top of the negative binomial, the inflation part
adds only a modest further gain (3134.1 → 3122.5). The hurdle (AIC
3233.8) sits with the ZIP, and the previous section argued that choosing
between those two is substantive, not fit-based. The zero block simply
stays empty in the columns of models that have none, and substantively
the IRRs agree across all four — what changes is the inference and the
account of the zeros.

## One number per predictor: the combined AME

Rate ratios live inside one component. For a single response-scale
summary that spans *both* parts, request average marginal effects: the
AME of a two-part model is the effect on the **overall expected count**
E(Y), combining the count and zero processes (computed by
[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
on the full model):

``` r

table_regression(fit_zip, show_columns = c("b", "ame", "p"))
#> Poisson zero-inflated regression: art
#> 
#>  Variable        │    B      AME     p   
#> ─────────────────┼───────────────────────
#>  (Intercept)     │    0.61         <.001 
#>  fem:            │                       
#>    Men (ref.)    │     –      –     –    
#>    Women         │   -0.22  -0.36  <.001 
#>  mar:            │                       
#>    Single (ref.) │     –      –     –    
#>    Married       │    0.13   0.22   .041 
#>  kid5            │   -0.16  -0.28  <.001 
#>  ment            │    0.02   0.06  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Zero-inflation: │                       
#>    (Intercept)   │   -0.69         <.001 
#>    ment          │   -0.13          .001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │  915                  
#>  AIC             │ 3225.5                
#> 
#> Note. Poisson zero-inflated regression.
#> Std. errors: Wald asymptotic (z).
#> Zero-inflation component: log-odds of a structural (excess) zero.
#> AME = average marginal effect.
```

Averaged over the sample’s covariate values, women are expected to
publish 0.36 fewer articles over the three years than comparable men
(AME −0.36) — one number combining both components; the zero-component
rows carry no AME of their own because the combined effect already
includes them.

For the remaining standard interpretive devices — predicted counts, or
the probability of specific counts such as Pr(Y = 0), at substantively
chosen covariate values — go back to the fitted model:
`predict(fit_zip, newdata = ..., type = "prob")`, or
[`marginaleffects::predictions()`](https://rdrr.io/pkg/marginaleffects/man/predictions.html)
with `datagrid()`.

## Cluster-robust variance

Students cluster in labs, cohorts, mentor groups. The `CR*` family
covers **both components with one estimator** —
[`sandwich::vcovCL()`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html)
on the full score matrix, so the count and zero rows shift together. For
two-part models every `CR*` variant maps to that same estimator, and the
footer records it as `CL`:

``` r

set.seed(1)
bioChemists$lab <- factor(sample(1:60, nrow(bioChemists), replace = TRUE))
table_regression(fit_zip, vcov = "CR0", cluster = bioChemists$lab,
                 show_columns = c("b", "se", "p"))
#> Poisson zero-inflated regression: art
#> 
#>  Variable        │    B      SE     p   
#> ─────────────────┼──────────────────────
#>  (Intercept)     │    0.61  0.09  <.001 
#>  fem:            │                      
#>    Men (ref.)    │     –     –     –    
#>    Women         │   -0.22  0.07  <.001 
#>  mar:            │                      
#>    Single (ref.) │     –     –     –    
#>    Married       │    0.13  0.09   .147 
#>  kid5            │   -0.16  0.07   .013 
#>  ment            │    0.02  0.00  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Zero-inflation: │                      
#>    (Intercept)   │   -0.69  0.26   .010 
#>    ment          │   -0.13  0.05   .017 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │  915                 
#>  AIC             │ 3225.5               
#> 
#> Note. Poisson zero-inflated regression.
#> Std. errors: cluster-robust (CL), clusters by lab.
#> Zero-inflation component: log-odds of a structural (excess) zero.
```

Do not read the shift here as the effect of clustering. The labs are
assigned at random, so they carry no real dependence: most of the
movement in marriage (p = .041 → .147) comes from the sandwich estimator
no longer trusting the ZIP’s model-based variance — unclustered robust
SEs give p ≈ .11 — and the rest is noise in the synthetic labels. What
the example demonstrates is mechanical: both components shift together
under one `CL` estimator. With genuinely clustered data, positive
within-cluster dependence widens the intervals further, and borderline
effects are typically the first casualties. `HC*` and the resampling
estimators have no two-part backend and are refused with a clear error.

## Mixed counts: `glmmTMB`

When counts are also grouped, `glmmTMB` (Brooks et al. 2017) combines
everything above with random effects: the zero-inflation block, the
random-effects block, and per-block exponentiation appear in one table —
here salamander counts in mined and unmined streams (Price et al. 2016):

``` r

data("Salamanders", package = "glmmTMB")
fit_mix <- glmmTMB::glmmTMB(
  count ~ mined + (1 | site),
  ziformula = ~ mined,
  family = glmmTMB::nbinom2, data = Salamanders
)
table_regression(fit_mix, exponentiate = TRUE)
#> Negative-binomial mixed-effects regression (glmmTMB) (zero-inflated): count
#> 
#>  Variable             │   IRR     SE      95% CI       p   
#> ──────────────────────┼────────────────────────────────────
#>  (Intercept)          │    0.57  0.21  [0.28, 1.19]   .133 
#>  mined:               │                                    
#>    yes (ref.)         │     –     –         –         –    
#>    no                 │    4.34  1.57  [2.14, 8.81]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Zero-inflation:      │                                    
#>    (Intercept)        │    1.27  0.62  [0.49, 3.30]   .621 
#>    mined: yes (ref.)  │     –     –         –         –    
#>    mined: no          │    0.10  0.08  [0.02, 0.49]   .004 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:      │                                    
#>    σ site (Intercept) │    0.37  0.17  [0.16, 0.81]   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                    │  644                               
#>  N (site)             │   23                               
#>  R² (marginal)        │    0.44                            
#>  R² (conditional)     │    0.56                            
#>  AIC                  │ 1741.4                             
#>  BIC                  │ 1768.2                             
#> 
#> Note. Negative-binomial mixed-effects regression (glmmTMB) (zero-inflated).
#> Std. errors: Wald asymptotic (z).
#> p-values: Wald-z asymptotic (glmmTMB).
#> Random effects (ML): LR test vs nbinom2 regression, χ̄²(1) = 5.37, p = 0.010.
#> Zero-inflation component: log-odds of a structural (excess) zero. Coefficients exponentiated and displayed as odds ratios.
#> IRR = incidence rate ratio.
#> Coefficients exponentiated and displayed as IRR; SE on the IRR scale (delta method); CI bounds exponentiated (asymmetric).
```

Among streams not in the structural-zero state, unmined ones host
salamanders at over four times the rate of mined ones (IRR 4.34); they
are also far less likely to be structural zeros in the first place (OR
0.10). Because the two components compound, the overall abundance gap is
larger than either alone — the model-implied expected count is about 8.7
times higher in unmined streams — one coherent ecological story told in
two parts. The random-effects block and its conventions are covered in
the companion vignette [*Mixed-effects regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md).

## Output formats

Everything above used the default console output. The same structure —
zero block included, as the last rows below — carries to a raw data
frame, a long broom-style tibble, and the rich `gt`, `flextable`,
`tinytable`, Excel, or Word targets:

``` r

table_regression(fit_zip, output = "data.frame")
#>           Variable       B   SE         95% CI     p
#> 1      (Intercept)    0.61 0.07 [ 0.48,  0.74] <.001
#> 2             fem:                                  
#> 3       Men (ref.)     –    –         –         –   
#> 4            Women   -0.22 0.06 [-0.33, -0.10] <.001
#> 5             mar:                                  
#> 6    Single (ref.)     –    –         –         –   
#> 7          Married    0.13 0.07 [ 0.01,  0.26]  .041
#> 8             kid5   -0.16 0.04 [-0.25, -0.08] <.001
#> 9             ment    0.02 0.00 [ 0.01,  0.02] <.001
#> 10 Zero-inflation:                                  
#> 11     (Intercept)   -0.69 0.21 [-1.09, -0.28] <.001
#> 12            ment   -0.13 0.04 [-0.21, -0.05]  .001
#> 13               n  915                             
#> 14             AIC 3225.5
```

``` r

table_regression(fit_zip, exponentiate = TRUE, output = "gt")
```

[TABLE]

*Note.* Poisson zero-inflated regression. Std. errors: Wald asymptotic
(z). Zero-inflation component: log-odds of a structural (excess) zero.
Coefficients exponentiated and displayed as odds ratios. IRR = incidence
rate ratio. Coefficients exponentiated and displayed as IRR; SE on the
IRR scale (delta method); CI bounds exponentiated (asymmetric).

In [`broom::tidy()`](https://broom.tidymodels.org) the zero-component
terms carry a `zero_` prefix, so the two parts separate cleanly:

``` r

td <- broom::tidy(table_regression(fit_zip))
td[, c("term", "estimate_type", "estimate", "p.value")]
#> # A tibble: 7 × 4
#>   term             estimate_type estimate  p.value
#>   <chr>            <chr>            <dbl>    <dbl>
#> 1 (Intercept)      B               0.609  2.37e-19
#> 2 femWomen         B              -0.218  2.05e- 4
#> 3 marMarried       B               0.135  4.07e- 2
#> 4 kid5             B              -0.163  1.75e- 4
#> 5 ment             B               0.0182 1.91e-16
#> 6 zero_(Intercept) B              -0.686  8.47e- 4
#> 7 zero_ment        B              -0.130  1.22e- 3
```

## References

- Brooks, M. E., Kristensen, K., van Benthem, K. J., Magnusson, A.,
  Berg, C. W., Nielsen, A., Skaug, H. J., Mächler, M., & Bolker, B. M.
  (2017). glmmTMB balances speed and flexibility among packages for
  zero-inflated generalized linear mixed modeling. *The R Journal*,
  9(2), 378–400.
- Cameron, A. C., & Trivedi, P. K. (2013). *Regression Analysis of Count
  Data* (2nd ed.). Cambridge University Press.
- Lambert, D. (1992). Zero-inflated Poisson regression, with an
  application to defects in manufacturing. *Technometrics*, 34(1), 1–14.
- Long, J. S. (1990). The origins of sex differences in science. *Social
  Forces*, 68(4), 1297–1316.
- Long, J. S. (1997). *Regression Models for Categorical and Limited
  Dependent Variables*. Sage.
- Mullahy, J. (1986). Specification and testing of some modified count
  data models. *Journal of Econometrics*, 33(3), 341–365.
- Price, S. J., Muncy, B. L., Bonner, S. J., Drayer, A. N., &
  Barton, C. D. (2016). Effects of mountaintop removal mining and valley
  filling on the occupancy and abundance of stream salamanders. *Journal
  of Applied Ecology*, 53(2), 459–468.
- Wilson, P. (2015). The misuse of the Vuong test for non-nested models
  to test for zero-inflation. *Economics Letters*, 127, 51–53.
- Zeileis, A., Kleiber, C., & Jackman, S. (2008). Regression models for
  count data in R. *Journal of Statistical Software*, 27(8).
