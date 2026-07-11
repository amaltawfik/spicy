# Bayesian regression tables

``` r

library(spicy)
library(rstanarm)
```

This vignette covers **Bayesian regression tables** — the same
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
call on a posterior instead of a maximum-likelihood fit. The companion
vignette [*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
covers the shared mechanics; here we focus on what genuinely changes
when the estimates are draws from a posterior distribution: what the
columns mean, which familiar columns are *deliberately absent*, and what
to check before reading anything.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
supports two engines:

- **`rstanarm`**
  ([`stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html),
  [`stan_lmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html),
  [`stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html))
  — ships precompiled models, so fits run in seconds and every chunk in
  this vignette executes live;
- **`brms`** (`brm()`) — a wider model space, at the cost of compiling
  each model; shown at the end (the code is identical in spirit).

The running example is the logistic regression of `smoking` from the
main vignette’s glm section — same data (`sochealth`), same predictors —
so the Bayesian table can be read against its frequentist twin.

``` r

sh <- sochealth
sh$education <- factor(sh$education, ordered = FALSE)
fit <- stan_glm(smoking ~ sex + age + education, data = sh,
                family = binomial(),
                iter = 1000, chains = 2, seed = 123, refresh = 0)
```

The sampler settings are trimmed so this vignette builds in seconds —
for real work, keep rstanarm’s defaults (four chains of 2,000
iterations): R-hat compares chains, so it needs several to compare, and
credible bounds are tail quantiles, estimated exactly where draws are
scarcest (Vehtari et al. 2021).

## What changes in a Bayesian table

``` r

table_regression(fit)
#> Bayesian logistic regression (stanreg): smoking
#> 
#>  Variable                 │    B      SE      95% CrI     
#> ──────────────────────────┼───────────────────────────────
#>  (Intercept)              │   -1.11  0.28  [-1.66, -0.55] 
#>  sex:                     │                               
#>    Female (ref.)          │     –     –          –        
#>    Male                   │   -0.04  0.15  [-0.34,  0.25] 
#>  age                      │    0.01  0.00  [-0.00,  0.02] 
#>  education:               │                               
#>    Lower secondary (ref.) │     –     –          –        
#>    Upper secondary        │   -0.49  0.17  [-0.81, -0.16] 
#>    Tertiary               │   -0.91  0.19  [-1.31, -0.56] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                          
#> 
#> Note. Bayesian logistic regression (stanreg).
#> Std. errors: Posterior covariance.
```

Reading the columns against their frequentist counterparts:

- **B is the posterior median** — the 50% quantile of each coefficient’s
  draws, robust to skewed posteriors (the `rstanarm` convention). It is
  not a maximum-likelihood point estimate, although under the default
  weakly informative priors and this sample size the two are nearly
  identical.
- **SE is the posterior standard deviation**, and the footer says so
  (`Std. errors: Posterior covariance.`) rather than borrowing the
  frequentist term.
- **The interval is a credible interval**, and the header says so:
  `95% CrI`, the equal-tailed 2.5% and 97.5% posterior quantiles. Its
  reading is the direct probability statement that confidence intervals
  are so often misread as making: *given model, priors, and data, the
  coefficient lies in \[-1.31, -0.56\] with 95% probability* (the
  `Tertiary` row). No appeal to repeated sampling is required.
- **There is no p column, by design.** A posterior has no p-value, and
  the Bayesian Analysis Reporting Guidelines (Kruschke 2021) ask that
  any decision rule be stated explicitly and kept separate from the
  posterior summary itself — so the default table reports the posterior
  and leaves decisions to the reader (requesting `"p"` explicitly is
  refused; in a mixed frequentist–Bayesian table the shared p column
  simply dashes the Bayesian rows). The closest posterior summary — the
  **probability of direction** (`pd`, the share of draws on the dominant
  side of zero; Makowski et al. 2019) — is opt-in:

``` r

table_regression(fit, show_columns = c("b", "ci", "pd"))
#> Bayesian logistic regression (stanreg): smoking
#> 
#>  Variable                 │    B        95% CrI       pd  
#> ──────────────────────────┼───────────────────────────────
#>  (Intercept)              │   -1.11  [-1.66, -0.55]  1.00 
#>  sex:                     │                               
#>    Female (ref.)          │     –          –          –   
#>    Male                   │   -0.04  [-0.34,  0.25]  0.62 
#>  age                      │    0.01  [-0.00,  0.02]  0.89 
#>  education:               │                               
#>    Lower secondary (ref.) │     –          –          –   
#>    Upper secondary        │   -0.49  [-0.81, -0.16]  1.00 
#>    Tertiary               │   -0.91  [-1.31, -0.56]  1.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                          
#> 
#> Note. Bayesian logistic regression (stanreg).
#> Std. errors: Posterior covariance.
```

Read against the table: `Male`, at 0.62, is barely better than a coin
flip on sign; `age`, at 0.89, leans positive without settling the
direction; the education contrasts display 1.00 — essentially every
retained draw is negative (all 1,000 for `Tertiary`; 999 of 1,000 for
`Upper secondary`, which rounds up). A `pd` of 0.50 would say the draws
split evenly, with zero at the posterior median. It reads as evidence of
*direction*, never of practical size — and one disclosure keeps this
section honest: under weak priors `pd` maps almost one-to-one onto the
frequentist one-sided p-value (two-sided p ≈ 2(1 − pd), so `age`’s 0.89
corresponds to p ≈ 0.23; Makowski et al. 2019). `pd` is not a route
around the p-value’s pathologies; it is the same directional information
stated as a posterior probability.

## Check convergence before reading anything

A frequentist table is wrong only if the model is wrong; a Bayesian
table can additionally be wrong because the *sampler* has not converged
— the draws then misrepresent the very posterior the columns summarize.
The check comes first, and it is three numbers per parameter (Vehtari et
al. 2021): R-hat, which compares chains (at convergence, 1.00; worry
above roughly 1.01), and two effective sample sizes that discount
autocorrelation — bulk ESS, which governs point estimates such as the
posterior median, and tail ESS, which governs the credible-interval
bounds:

``` r

posterior::summarise_draws(
  posterior::subset_draws(posterior::as_draws_array(fit),
                          variable = names(fit$coefficients))
)[, c("variable", "rhat", "ess_bulk", "ess_tail")]
#> # A tibble: 5 × 4
#>   variable                  rhat ess_bulk ess_tail
#>   <chr>                    <dbl>    <dbl>    <dbl>
#> 1 (Intercept)               1.01    1301.     689.
#> 2 sexMale                   1.00    1292.     743.
#> 3 age                       1.00    1270.     788.
#> 4 educationUpper secondary  1.00    1064.     816.
#> 5 educationTertiary         1.00    1133.     797.
```

Read the output rather than glossing it. R-hat sits at 1.00 for every
slope but prints 1.01 for the intercept — exactly the alert level, and
exactly what a deliberately short two-chain run invites; a manuscript
fit would simply use the defaults. Bulk effective sizes exceed the 1,000
retained draws — legitimate, since Hamiltonian Monte Carlo can produce
anticorrelated draws that beat independence — while the tail effective
sizes, near 700–800, are what guard the 2.5% and 97.5% bounds the CI
column reports: comfortable for two-decimal reporting. When these
diagnostics genuinely fail, the remedy is more iterations or a
reparameterized model — never a more optimistic reading of the table.

## Priors are part of the model — disclose them

The posterior blends data with priors, so a reported Bayesian model is
incompletely specified until the priors are stated. `rstanarm`’s
defaults are weakly informative (centred at zero, scaled to the data),
which is why the table above sits so close to its frequentist twin — but
that is a property to *verify and report*, not assume:

``` r

prior_summary(fit)
#> Priors for model 'fit' 
#> ------
#> Intercept (after predictors centered)
#>  ~ normal(location = 0, scale = 2.5)
#> 
#> Coefficients
#>   Specified prior:
#>     ~ normal(location = [0,0,0,...], scale = [2.5,2.5,2.5,...])
#>   Adjusted prior:
#>     ~ normal(location = [0,0,0,...], scale = [5.00,0.17,5.02,...])
#> ------
#> See help('prior_summary.stanreg') for more details
```

The *Adjusted prior* line is the disclosure that matters: `rstanarm`
rescales the default `normal(0, 2.5)` by each predictor’s spread (scale
2.5/sd(x)), so the priors actually applied here are normal(0, 5.00) on
the sex contrast and normal(0, 0.17) on age — equivalent to placing
normal(0, 2.5) on the coefficients of *standardized* predictors. For a
manuscript, one sentence suffices (“rstanarm defaults: independent
normal(0, 2.5) priors, autoscaled by each predictor’s SD — the *Adjusted
prior* scales above”), and a sensitivity re-fit with wider or
substantive priors belongs in the appendix when the priors could
plausibly matter.

## Odds ratios: `exponentiate = TRUE`

The ratio scale works exactly as for `glm`, with one pleasant
difference: the credible bounds are the exponentiated *quantiles*, so
they are exact posterior statements about the odds ratio itself — not a
delta-method approximation. The SE column is the exception: as the
footer discloses, it crosses to the ratio scale by the delta method
(SE_OR = OR × SE_log-odds) — one more reason to read the interval and
never to reconstruct one as B ± 2 SE:

``` r

table_regression(fit, exponentiate = TRUE)
#> Bayesian logistic regression (stanreg): smoking
#> 
#>  Variable                 │   OR      SE     95% CrI    
#> ──────────────────────────┼─────────────────────────────
#>  (Intercept)              │    0.33  0.09  [0.19, 0.57] 
#>  sex:                     │                             
#>    Female (ref.)          │     –     –         –       
#>    Male                   │    0.96  0.14  [0.71, 1.28] 
#>  age                      │    1.01  0.00  [1.00, 1.02] 
#>  education:               │                             
#>    Lower secondary (ref.) │     –     –         –       
#>    Upper secondary        │    0.61  0.10  [0.44, 0.85] 
#>    Tertiary               │    0.40  0.08  [0.27, 0.57] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                        
#> 
#> Note. Bayesian logistic regression (stanreg).
#> Std. errors: Posterior covariance.
#> OR = odds ratio.
#> Coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric).
```

Reading the `Tertiary` row with the factor-change template: holding sex
and age constant, the odds of smoking for a tertiary-educated respondent
are 0.40 times the odds for one with lower secondary education — and
given model, priors, and data, that odds ratio lies between 0.27 and
0.57 with 95% probability.

## The frequentist twin, side by side

Because the layout machinery is shared, the comparison table is one
[`list()`](https://rdrr.io/r/base/list.html) away — and with weak priors
and n = 1,175 it mostly documents agreement, which is itself worth
showing to a skeptical reader:

``` r

gf <- glm(smoking ~ sex + age + education, data = sh,
          family = binomial)
table_regression(list("ML (glm)" = gf, "Bayes (stan_glm)" = fit),
                 show_columns = c("b", "ci"))
#> Regression comparison: smoking
#> 
#>                                    ML (glm)             Bayes (stan_glm)     
#>                             ───────────────────────  ─────────────────────── 
#>  Variable                 │    B         95% CI         B         95% CI     
#> ──────────────────────────┼──────────────────────────────────────────────────
#>  (Intercept)              │   -1.11  [-1.67, -0.55]    -1.11  [-1.66, -0.55] 
#>  sex:                     │                                                  
#>    Female (ref.)          │     –          –             –          –        
#>    Male                   │   -0.04  [-0.32,  0.25]    -0.04  [-0.34,  0.25] 
#>  age                      │    0.01  [-0.00,  0.02]     0.01  [-0.00,  0.02] 
#>  education:               │                                                  
#>    Lower secondary (ref.) │     –          –             –          –        
#>    Upper secondary        │   -0.48  [-0.82, -0.14]    -0.49  [-0.81, -0.16] 
#>    Tertiary               │   -0.91  [-1.29, -0.52]    -0.91  [-1.31, -0.56] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                     1175                    
#>  R² (McFadden)            │    0.02                                          
#>  R² (Nagelkerke)          │    0.03                                          
#>  AIC                      │ 1200.9                                           
#> 
#> Note. Model 1: logistic regression; Model 2: Bayesian logistic regression (stanreg).
#> Std. errors:
#>   Model 1: classical (Fisher information)
#>   Model 2: Posterior covariance
```

The intervals differ in *reading*, not much in width: the frequentist
column’s CI is a coverage statement about the procedure, the Bayesian
column’s CrI a probability statement about the parameter (the shared
header stays `95% CI` here — the honest common label; the all-Bayesian
table above shows `95% CrI`). In the fit-statistics block, `n` fills
both columns, but the likelihood-based rows — the two pseudo-R² and AIC
— fill only the frequentist one, because a posterior has none of them.
Which brings us to what the table refuses.

## What is refused, and why

Several familiar requests are meaningless for a posterior, and spicy
refuses them with an explanatory error rather than rendering something
silently wrong:

- `p_adjust` — there are no p-values to adjust;
- likelihood-based fit statistics (`"AIC"`, `"BIC"`, pseudo-R²) —
  compare Bayesian models with
  [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) and
  [`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
  outside the table (Vehtari, Gelman & Gabry 2017); explained variance
  has a draws-based analog,
  [`rstanarm::bayes_R2()`](https://mc-stan.org/rstantools/reference/bayes_R2.html)
  (Gelman et al. 2019), reported in the text;
- robust / cluster-robust `vcov` — a sandwich estimator corrects a
  misspecified likelihood’s standard errors; nothing standard plays that
  role for a posterior covariance (misspecification-robust Bayesian
  procedures exist but remain research-grade), so model the clustering
  instead (next section);
- `ci_method = "profile"` and the AME family — profile likelihood is a
  frequentist construction, and a draws-based AME needs its own design
  (planned).

``` r

table_regression(fit, p_adjust = "holm")
#> Error in `table_regression()`:
#> ! `p_adjust` is not available for Bayesian fits: there are no p-values to adjust.
#> ℹ Bayesian tables report posterior medians and credible intervals; the probability-of-direction column (`show_columns = "pd"`) is the closest posterior summary.
```

## Multilevel models: random effects from the draws

Clustered data is where the Bayesian machinery earns its keep — the
priors regularize the group-level variance exactly where a
likelihood-based fit struggles (few groups, boundary estimates). The
random effects render as the same block as for `lmer`, with every number
taken directly from the posterior draws: the estimate is the posterior
median of each SD, the interval its posterior quantiles — and there is
no boundary-corrected test because nothing is being tested — the
chi-bar-squared machinery exists only because the frequentist null σ = 0
sits on the edge of the parameter space, a situation a posterior
quantile never encounters:

``` r

mfit <- stan_lmer(Reaction ~ Days + (Days | Subject),
                  data = lme4::sleepstudy,
                  iter = 1000, chains = 2, seed = 7, refresh = 0)
table_regression(mfit)
#> Bayesian linear regression (stanreg): Reaction
#> 
#>  Variable                         │   B      SE       95% CrI      
#> ──────────────────────────────────┼────────────────────────────────
#>  (Intercept)                      │ 251.23  6.88  [237.54, 264.18] 
#>  Days                             │  10.38  1.90  [  6.58,  13.95] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                  │                                
#>    σ Subject (Intercept)          │  21.99  6.00  [ 12.68,  35.63] 
#>    σ Subject Days                 │   6.53  1.47  [  4.37,  10.18] 
#>    ρ Subject (Days × (Intercept)) │   0.11  0.29  [ -0.42,   0.70] 
#>    σ (Residual)                   │  25.99  1.57  [ 23.14,  29.28] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                                │ 180                            
#> 
#> Note. Bayesian linear regression (stanreg).
#> Std. errors: Posterior covariance.
#> Random effects (MCMC).
```

Compare this block with the `lmer` table of the [mixed-effects
vignette](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md):
same layout, but the credible interval on the `Days` slope SD — the
`σ Subject Days` row — is a genuine posterior statement — no
chi-bar-squared correction, no profile refit — and the footer reads
`Random effects (MCMC)`. The per-group deviations themselves (one per
subject) are deliberately *not* listed as coefficient rows; they are
shrinkage estimates, and their summary is the block’s SD.

## `brms`

`brms` reaches the same table through the same code path; the only
practical difference is that each model compiles before sampling
(minutes, and a C++ toolchain), which is why this chunk is shown without
being evaluated. Run it locally to see the table — it matches the
`rstanarm` one above row for row, as the package’s shared frame tests
verify:

``` r

bfit <- brms::brm(Reaction ~ Days + (Days | Subject),
                  data = lme4::sleepstudy,
                  chains = 2, iter = 1000, seed = 7, refresh = 0,
                  silent = 2)
table_regression(bfit)
```

Coefficients are posterior medians with equal-tailed credible intervals,
the random-effect block is built from the `sd_*` / `cor_*` draws, and
the refusals above apply identically. What `brms` buys is model space:
distributional regression, non-linear formulas, and families neither
`glm` nor `rstanarm` covers — all rendered through the same table.

## Output formats

The default console table is one of several targets. The `output`
argument also produces a raw data frame, a long broom-style tibble, and
— with the corresponding Suggests package — rich `gt`, `flextable`,
`tinytable`, Excel, or Word tables. The Bayesian structure carries
through: the p column stays a dash, and the posterior summaries keep
full precision in the tidy frame. In
[`broom::tidy()`](https://broom.tidymodels.org) the `estimate` column is
the posterior median and `conf.low` / `conf.high` the credible bounds;
`p.value`, `statistic`, and `df` are `NA` — pipelines that filter on
`p.value` must switch to the interval (or `pd`):

``` r

td <- broom::tidy(table_regression(fit))
td[, c("term", "estimate", "std.error", "conf.low", "conf.high",
       "p.value")]
#> # A tibble: 5 × 6
#>   term                     estimate std.error conf.low conf.high p.value
#>   <chr>                       <dbl>     <dbl>    <dbl>     <dbl>   <dbl>
#> 1 (Intercept)              -1.11      0.284   -1.66      -0.554       NA
#> 2 sexMale                  -0.0380    0.148   -0.337      0.249       NA
#> 3 age                       0.00614   0.00485 -0.00326    0.0152      NA
#> 4 educationUpper secondary -0.487     0.169   -0.811     -0.164       NA
#> 5 educationTertiary        -0.909     0.194   -1.31      -0.559       NA
```

``` r

table_regression(fit, exponentiate = TRUE, output = "gt")
```

[TABLE]

*Note.* Bayesian logistic regression (stanreg). Std. errors: Posterior
covariance. OR = odds ratio. Coefficients exponentiated and displayed as
OR; SE on the OR scale (delta method); CI bounds exponentiated
(asymmetric).

## See also

- [`vignette("table-regression", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/table-regression.md)
  for the shared mechanics (columns, layouts, filtering).
- [`vignette("table-regression-mixed", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md)
  for the frequentist multilevel counterpart of the random-effects
  block.

## References

- Gelman, A., Hill, J., & Vehtari, A. (2020). *Regression and Other
  Stories*. Cambridge University Press.
- Gelman, A., Goodrich, B., Gabry, J., & Vehtari, A. (2019). R-squared
  for Bayesian regression models. *The American Statistician*, 73(3),
  307–309.
- Kruschke, J. K. (2021). Bayesian analysis reporting guidelines.
  *Nature Human Behaviour*, 5(10), 1282–1291.
- Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., & Lüdecke, D.
  (2019). Indices of effect existence and significance in the Bayesian
  framework. *Frontiers in Psychology*, 10, 2767.
- Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model
  evaluation using leave-one-out cross-validation and WAIC. *Statistics
  and Computing*, 27(5), 1413–1432.
- Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C.
  (2021). Rank-normalization, folding, and localization: An improved
  R-hat for assessing convergence of MCMC. *Bayesian Analysis*, 16(2),
  667–718.
