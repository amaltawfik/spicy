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
#>  (Intercept)              │   -1.11  0.29  [-1.66, -0.55] 
#>  sex:                     │                               
#>    Female (ref.)          │     –     –          –        
#>    Male                   │   -0.04  0.15  [-0.34,  0.25] 
#>  age                      │    0.01  0.01  [-0.00,  0.02] 
#>  education:               │                               
#>    Lower secondary (ref.) │     –     –          –        
#>    Upper secondary        │   -0.49  0.16  [-0.81, -0.16] 
#>    Tertiary               │   -0.91  0.20  [-1.31, -0.56] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                          
#>  R² (Bayes)               │    0.02                       
#> 
#> Note. Bayesian logistic regression (stanreg).
#> Std. errors: posterior MAD SD (scaled median absolute deviation).
```

Reading the columns against their frequentist counterparts:

- **B is the posterior median** — the 50% quantile of each coefficient’s
  draws, robust to skewed posteriors (the `rstanarm` convention). It is
  not a maximum-likelihood point estimate, although under the default
  weakly informative priors and this sample size the two are nearly
  identical.
- **SE is the posterior MAD SD** — the median absolute deviation of the
  draws, rescaled so it equals the standard deviation for a normal
  posterior — and the footer says so
  (`Std. errors: posterior MAD SD (scaled median absolute deviation).`)
  rather than borrowing the frequentist term. Median + MAD SD is the
  robust pairing of *Regression and Other Stories* (§5.3) and of
  `rstanarm`’s own print: both members resist skewed posteriors, where a
  mean + SD pair can be dominated by one heavy tail. As Gelman, Hill &
  Vehtari put it, “for convenience we sometimes call the posterior mad
  sd the ‘standard error’”.
- **The interval is a credible interval**, and the header says so:
  `95% CrI`, the equal-tailed 2.5% and 97.5% posterior quantiles. Its
  reading is the direct probability statement that confidence intervals
  are so often misread as making: *given model, priors, and data, the
  coefficient lies in \[-1.31, -0.56\] with 95% probability* (the
  `Tertiary` row). No appeal to repeated sampling is required. The
  equal-tailed interval is the default because it is
  transformation-invariant (which is what makes the exact odds-ratio
  bounds below possible) and matches `rstanarm`; the Kruschke
  tradition’s **highest-density interval** is one argument away —
  `ci_method = "hdi"` computes the shortest interval containing 95% of
  the draws (Kruschke 2015) and relabels the header `95% HDI`. For these
  near-symmetric posteriors the two differ only in the second decimal;
  they part ways on skewed posteriors, where the HDI shifts toward the
  mode.
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
#>  R² (Bayes)               │    0.02                       
#> 
#> Note. Bayesian logistic regression (stanreg).
#> Std. errors: posterior MAD SD (scaled median absolute deviation).
#> pd = probability of direction (share of the posterior on the dominant side of zero; Makowski et al. 2019).
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

For a claim of practical size, the Kruschke tradition asks how much of
the posterior falls inside a **region of practical equivalence** (ROPE)
around zero. spicy deliberately renders no ROPE column: the reporting
guidelines require the equivalence limits to be *justified* (BARG step
4.C), and limits are substantive, per-coefficient claims — ±0.05
log-odds may be negligible for one predictor and consequential for
another — that a package cannot default on the analyst’s behalf. When
you can defend limits, compute the shares outside the table with
`bayestestR::rope(fit, range = c(-0.18, 0.18))` (or
[`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html)),
state the limits and their justification in the text, and keep the
table’s posterior summaries as they are.

## Check convergence before reading anything

A frequentist table is wrong only if the model is wrong; a Bayesian
table can additionally be wrong because the *sampler* has not converged
— the draws then misrepresent the very posterior the columns summarize.
spicy therefore checks every Bayesian fit before rendering: when any
sampled parameter misses its target — R-hat at or above 1.01, effective
sample size below 100 per chain (never less than 400, so fewer chains do
not weaken the bar), any divergent transition, E-BFMI below 0.2 (Vehtari
et al. 2021) — the table gains a `Sampler diagnostics:` footer line
naming the failure and a warning fires (classed
`spicy_bayes_diagnostics`, so scripts can catch or mute this guard
selectively with
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)). A
clean fit prints nothing: the publication table stays lean, and silence
means the checks passed. The guard reads *all* sampled parameters —
including the group-level ones a multilevel table summarizes — not just
the displayed coefficients.

For parameter-level triage the same three diagnostics are available as
opt-in columns in all-Bayesian tables, together with a fourth, `mcse` —
the Monte Carlo standard error of the displayed posterior median:

``` r

table_regression(fit, show_columns = c("b", "ci", "rhat", "mcse"))
#> Bayesian logistic regression (stanreg): smoking
#> 
#>  Variable                 │    B        95% CrI      R-hat   MCSE   
#> ──────────────────────────┼─────────────────────────────────────────
#>  (Intercept)              │   -1.11  [-1.66, -0.55]  1.007  0.014   
#>  sex:                     │                                         
#>    Female (ref.)          │     –          –          –      –      
#>    Male                   │   -0.04  [-0.34,  0.25]  1.000  0.0050  
#>  age                      │    0.01  [-0.00,  0.02]  1.002  0.00015 
#>  education:               │                                         
#>    Lower secondary (ref.) │     –          –          –      –      
#>    Upper secondary        │   -0.49  [-0.81, -0.16]  1.002  0.0071  
#>    Tertiary               │   -0.91  [-1.31, -0.56]  1.002  0.0097  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                                    
#>  R² (Bayes)               │    0.02                                 
#> 
#> Note. Bayesian logistic regression (stanreg).
#> Std. errors: posterior MAD SD (scaled median absolute deviation).
#> MCSE = Monte Carlo standard error of the posterior median (Vehtari et al. 2021).
```

The `mcse` column answers a question the other diagnostics do not: *how
many of the printed digits are real?* MCMC output is stochastic — rerun
the sampler with another seed and the third significant digit of most
estimates will move. The working rule (Gelman, Vehtari, McElreath et
al. 2026, §11.6): a displayed digit is Monte-Carlo stable when **twice
the MCSE stays below one unit of that digit**. Here twice the MCSE of
the coefficients is on the order of their second decimal: that digit
sits near the stability edge — honest to report, and a reminder that a
two-decimal table is close to the resolution limit of this deliberately
short chain. Most of the time two *significant* digits are all a
posterior summary can support, halving the MCSE costs four times the
iterations, and tail quantities (the CrI bounds) are noisier than the
median. When the displayed digits matter — a boundary against a clinical
threshold, say — check `mcse` before trusting the last one.

Under the hood these are three numbers per parameter (Vehtari et al.
2021): R-hat, which compares chains (at convergence, 1.00; worry above
roughly 1.01), and two effective sample sizes that discount
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
difference: every displayed quantity is computed from the exponentiated
draws themselves, so all of them are exact posterior statements about
the odds ratio — the credible bounds are the exponentiated quantiles,
and the SE is the posterior MAD SD of `exp(draws)` rather than the
delta-method approximation (SE_OR = OR × SE_log-odds), which understates
the ratio-scale spread severalfold when the posterior is wide.
Ratio-scale posteriors are right-tail-heavy, so the interval remains the
primary uncertainty summary — never reconstruct one as B ± 2 SE:

``` r

table_regression(fit, exponentiate = TRUE)
#> Bayesian logistic regression (stanreg): smoking
#> 
#>  Variable                 │   OR      SE     95% CrI    
#> ──────────────────────────┼─────────────────────────────
#>  (Intercept)              │    0.33  0.09  [0.19, 0.57] 
#>  sex:                     │                             
#>    Female (ref.)          │     –     –         –       
#>    Male                   │    0.96  0.15  [0.71, 1.28] 
#>  age                      │    1.01  0.01  [1.00, 1.02] 
#>  education:               │                             
#>    Lower secondary (ref.) │     –     –         –       
#>    Upper secondary        │    0.61  0.10  [0.44, 0.85] 
#>    Tertiary               │    0.40  0.08  [0.27, 0.57] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                        
#>  R² (Bayes)               │    0.02                     
#> 
#> Note. Bayesian logistic regression (stanreg).
#> Std. errors: posterior MAD SD (scaled median absolute deviation).
#> OR = odds ratio.
#> Coefficients exponentiated and displayed as OR; SE on the OR scale (posterior MAD SD of the exponentiated draws); CI bounds exponentiated (asymmetric).
```

Reading the `Tertiary` row with the factor-change template: holding sex
and age constant, the odds of smoking for a tertiary-educated respondent
are 0.40 times the odds for one with lower secondary education — and
given model, priors, and data, that odds ratio lies between 0.27 and
0.57 with 95% probability.

## Marginal effects, from the draws

Odds ratios answer on the odds scale; the **average marginal effect**
answers on the probability scale — here, percentage points of
P(smoking). For a posterior, the AME is computed *per draw*
([`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)),
and the table summarizes those draws under the same conventions as every
other column: posterior median, MAD SD, and a credible interval that
follows `ci_method` (equal-tailed by default, HDI on request):

``` r

table_regression(fit, show_columns = c("b", "ame", "ame_ci"))
#> Warning: AME computation via `marginaleffects::avg_slopes()` failed.
#> ✖ Reason: Unable to compute predicted values with this model. This error can arise when `insight::get_data()` is unable to extract the dataset from the model object, or when the original data frame (usually in the global environment) was modified since fitting the model. Make sure the original data object is unchanged and/or try to supply a different dataset to the `newdata` argument.
#> 
#> In addition, this error message was raised:
#> there is no package called 'collapse'
#> 
#> Bug Tracker: https://github.com/vincentarelbundock/marginaleffects/issues
#> ℹ AME column will be em-dashed in the displayed table.
#> Bayesian logistic regression (stanreg): smoking
#> 
#>  Variable                 │    B     AME  95% CrI 
#> ──────────────────────────┼───────────────────────
#>  (Intercept)              │   -1.11               
#>  sex:                     │                       
#>    Female (ref.)          │     –                 
#>    Male                   │   -0.04               
#>  age                      │    0.01               
#>  education:               │                       
#>    Lower secondary (ref.) │     –                 
#>    Upper secondary        │   -0.49               
#>    Tertiary               │   -0.91               
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                  
#>  R² (Bayes)               │    0.02               
#> 
#> Note. Bayesian logistic regression (stanreg).
#> Std. errors: posterior MAD SD (scaled median absolute deviation).
#> AME = average marginal effect.
```

Reading the `Tertiary` row: averaged over the sample, tertiary education
shifts the probability of smoking by the displayed amount — given model,
priors, and data, that shift lies in its credible interval with 95%
probability, a statement no delta-method AME can make exactly. There is
no `ame_p`, for the same reason there is no p column: the preset
`"all_ame"` expands without it, the atomic token is refused, and in a
mixed frequentist–Bayesian table the shared `ame_p` column dashes the
Bayesian rows.

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
#>  R² (Bayes)               │                             0.02                 
#> 
#> Note. Model 1: logistic regression; Model 2: Bayesian logistic regression (stanreg).
#> Std. errors:
#>   Model 1: classical (Fisher information)
#>   Model 2: posterior MAD SD (scaled median absolute deviation)
#> Model 2: 95% CI is an equal-tailed posterior credible interval.
```

The intervals differ in *reading*, not much in width: the frequentist
column’s CI is a coverage statement about the procedure, the Bayesian
column’s CrI a probability statement about the parameter. The shared
header stays `95% CI` here — the honest common label — and the footer
discloses per model that the Bayesian column is an equal-tailed
posterior credible interval (the all-Bayesian table above relabels the
header itself, `95% CrI`). In the fit-statistics block, `n` and
`R² (Bayes)` fill per model, but the likelihood-based rows — the two
pseudo-R² and AIC — fill only the frequentist one, because a posterior
has none of them. Which brings us to what the table refuses.

## What is refused, and why

Several familiar requests are meaningless for a posterior, and spicy
refuses them with an explanatory error rather than rendering something
silently wrong:

- `p_adjust` — there are no p-values to adjust;
- likelihood-based fit statistics (`"aic"`, `"bic"`, pseudo-R²) — the
  Bayesian table has its own instead: the default block reports
  `R² (Bayes)`, the posterior median of the draws-based R² (Gelman et
  al. 2019), and
  `show_fit_stats = c("nobs", "r2_bayes", "elpd_loo", "looic")` adds the
  PSIS-LOO expected log predictive density and its deviance-scale twin,
  with the elpd standard error disclosed in the footer (Vehtari, Gelman
  & Gabry 2017; a few seconds per model). Prefer the log scale: the −2
  of LOOIC persists for historical reasons only (Gelman, Vehtari,
  McElreath et al. 2026, App. B). A `"waic"` token exists too — its SE
  is disclosed the same way — though PSIS-LOO is generally preferred.
  These estimates come with their own reliability diagnostics, and spicy
  never mutes them: when PSIS-LOO flags observations with Pareto k above
  the sample-size-specific threshold — min(1 − 1/log10(S), 0.7), the
  same bound [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html)
  itself prints (Vehtari et al. 2024) — or WAIC flags p_waic above 0.4,
  the footer says the estimate is unreliable and a
  `spicy_bayes_diagnostics` warning fires. The remediation ladder
  (Gelman, Vehtari, McElreath et al. 2026, ch. 24):
  [`loo::loo_moment_match()`](https://mc-stan.org/loo/reference/loo_moment_match.html)
  first, then refitting the flagged folds (`k_threshold = 0.7`, brms
  `reloo = TRUE`), then K-fold cross-validation. Model *comparison*
  belongs to
  [`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
  outside the table: the comparison uncertainty is the standard error of
  the elpd *difference* (not the per-model SEs shown here), and
  differences smaller than about 4 carry no practical meaning (Gelman,
  Vehtari, McElreath et al. 2026, ch. 9). The canonical fit *check* is
  the graphical posterior predictive check (`pp_check(fit)`; Gelman et
  al. 2013, ch. 6) — Bayes factors stay out by design (Gelman et
  al. 2013, §7.4);
- robust / cluster-robust `vcov` — a sandwich estimator corrects a
  misspecified likelihood’s standard errors; nothing standard plays that
  role for a posterior (misspecification-robust Bayesian procedures
  exist but remain research-grade), so model the clustering instead
  (next section);
- `ci_method = "profile"` / `"boot_percentile"` — profile likelihood and
  bootstrap replicates are frequentist constructions with no posterior
  analogue (the credible interval is computed from the draws; `"hdi"` is
  the one alternative flavor);
- `standardized` — the refit flavors would re-run MCMC on z-scored data
  inside a table call (minutes per model), and the post-hoc flavors
  rescale a Wald machinery the posterior does not have; standardize
  predictors before fitting (Gelman, Hill & Vehtari 2020, ch. 12) if
  standardized coefficients are the goal;
- variational and optimizing fits
  (`stan_glm(..., algorithm = "meanfield")`, `"optimizing"`) — they
  carry an *approximate* posterior on which the MCMC diagnostics above
  would be vacuous; refit with `algorithm = "sampling"`;
- a ROPE column — deliberately absent, as discussed in the `pd` section:
  equivalence limits are substantive claims the analyst must justify per
  coefficient
  ([`bayestestR::rope()`](https://easystats.github.io/bayestestR/reference/rope.html)
  renders them once justified).

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
#> Warning: Bayesian fit (outcome: Reaction) shows sampler problems -- Sampler
#> diagnostics: max R-hat = 1.010 (target < 1.01); min ESS = 253 (target > 400).
#> Do not report as-is; run longer or reparameterize (Vehtari et al. 2021).
#> Bayesian linear regression (stanreg): Reaction
#> 
#>  Variable                         │   B      SE       95% CrI      
#> ──────────────────────────────────┼────────────────────────────────
#>  (Intercept)                      │ 251.23  6.53  [237.54, 264.18] 
#>  Days                             │  10.38  1.82  [  6.58,  13.95] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                  │                                
#>    σ Subject (Intercept)          │  21.99  5.52  [ 12.68,  35.63] 
#>    σ Subject Days                 │   6.53  1.33  [  4.37,  10.18] 
#>    ρ Subject (Days × (Intercept)) │   0.11  0.30  [ -0.42,   0.70] 
#>    σ (Residual)                   │  25.99  1.63  [ 23.14,  29.28] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                                │ 180                            
#>  R² (Bayes)                       │   0.79                         
#> 
#> Note. Bayesian linear regression (stanreg).
#> Std. errors: posterior MAD SD (scaled median absolute deviation).
#> Random effects (MCMC).
#> Sampler diagnostics: max R-hat = 1.010 (target < 1.01); min ESS = 253 (target > 400). Do not report as-is; run longer or reparameterize (Vehtari et al. 2021).
```

Note the guard from the convergence section doing its job: this
deliberately trimmed run (two short chains, a correlated random slope)
misses the R-hat and ESS targets, so the table says so in its footer and
a `spicy_bayes_diagnostics` warning fires — a manuscript fit with
rstanarm’s default sampler settings renders this same table clean.

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
the posterior median, `std.error` the posterior MAD SD, and `conf.low` /
`conf.high` the credible bounds; `p.value`, `statistic`, and `df` are
`NA` — pipelines that filter on `p.value` must switch to the interval
(or `pd`):

``` r

td <- broom::tidy(table_regression(fit))
td[, c("term", "estimate", "std.error", "conf.low", "conf.high",
       "p.value")]
#> # A tibble: 5 × 6
#>   term                     estimate std.error conf.low conf.high p.value
#>   <chr>                       <dbl>     <dbl>    <dbl>     <dbl>   <dbl>
#> 1 (Intercept)              -1.11      0.287   -1.66      -0.554       NA
#> 2 sexMale                  -0.0380    0.153   -0.337      0.249       NA
#> 3 age                       0.00614   0.00504 -0.00326    0.0152      NA
#> 4 educationUpper secondary -0.487     0.165   -0.811     -0.164       NA
#> 5 educationTertiary        -0.909     0.197   -1.31      -0.559       NA
```

``` r

table_regression(fit, exponentiate = TRUE, output = "gt")
```

[TABLE]

*Note.* Bayesian logistic regression (stanreg). Std. errors: posterior
MAD SD (scaled median absolute deviation). OR = odds ratio. Coefficients
exponentiated and displayed as OR; SE on the OR scale (posterior MAD SD
of the exponentiated draws); CI bounds exponentiated (asymmetric).

## See also

- [`vignette("table-regression", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/table-regression.md)
  for the shared mechanics (columns, layouts, filtering).
- [`vignette("table-regression-mixed", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md)
  for the frequentist multilevel counterpart of the random-effects
  block.

## References

- Gelman, A., Hill, J., & Vehtari, A. (2020). *Regression and Other
  Stories*. Cambridge University Press.
- Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., &
  Rubin, D. B. (2013). *Bayesian Data Analysis* (3rd ed.). CRC Press.
- Gelman, A., Goodrich, B., Gabry, J., & Vehtari, A. (2019). R-squared
  for Bayesian regression models. *The American Statistician*, 73(3),
  307–309.
- Gelman, A., Vehtari, A., McElreath, R., et al. (2026). *Bayesian
  Workflow*. Chapman & Hall / CRC Press.
- Kruschke, J. K. (2015). *Doing Bayesian Data Analysis* (2nd ed.).
  Academic Press.
- Kruschke, J. K. (2021). Bayesian analysis reporting guidelines.
  *Nature Human Behaviour*, 5(10), 1282–1291.
- Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., & Lüdecke, D.
  (2019). Indices of effect existence and significance in the Bayesian
  framework. *Frontiers in Psychology*, 10, 2767.
- Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model
  evaluation using leave-one-out cross-validation and WAIC. *Statistics
  and Computing*, 27(5), 1413–1432.
- Vehtari, A., Simpson, D., Gelman, A., Yao, Y., & Gabry, J. (2024).
  Pareto smoothed importance sampling. *Journal of Machine Learning
  Research*, 25(72), 1–58.
- Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C.
  (2021). Rank-normalization, folding, and localization: An improved
  R-hat for assessing convergence of MCMC. *Bayesian Analysis*, 16(2),
  667–718.
