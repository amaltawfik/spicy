# Supported models and per-family behavior of table_regression()

`table_regression_models()` returns the registry of model classes
supported by
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
one row per engine, with each class's family, average-marginal-effects
estimand, `exponentiate` semantics, and labeled table blocks. The same
registry drives this page's table, so the published list cannot drift
from the code.

This page is also the reference for **per-family behavior** (the
sections below). It is reachable as `?table_regression_models`,
`?table_regression_mixed`, `?table_regression_ordinal`,
`?table_regression_counts`, `?table_regression_categorical`,
`?table_regression_survival`, `?table_regression_robust`, or
`?table_regression_bayesian`.

If a class is not listed: fit the model and call `table_regression(fit)`
anyway – unsupported classes error with a clear message naming the
supported set. Feature requests are welcome on the issue tracker.

## Usage

``` r
table_regression_models()
```

## Value

A data frame with one row per supported engine and columns `family`,
`class`, `engine`, `ame`, `exponentiate`, `blocks`.

## Supported classes

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Family | Class | Engine | AME | Exponentiate | Blocks |
| Linear and generalized linear | `lm` | [`stats::lm()`](https://rdrr.io/r/stats/lm.html) | yes | \- | \- |
| Linear and generalized linear | `glm` | [`stats::glm()`](https://rdrr.io/r/stats/glm.html) | yes | OR / IRR / RR / MR / HR (link) | \- |
| Linear and generalized linear | `negbin` | [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) | yes | IRR | \- |
| Linear and generalized linear | `rlm` | [`MASS::rlm()`](https://rdrr.io/pkg/MASS/man/rlm.html) | yes | \- | \- |
| Linear and generalized linear | `nls` | [`stats::nls()`](https://rdrr.io/r/stats/nls.html) | no | \- | \- |
| Robust, IV, quantile, panel | `lm_robust` | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html) | yes | \- | \- |
| Robust, IV, quantile, panel | `iv_robust` | [`estimatr::iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html) | yes | \- | \- |
| Robust, IV, quantile, panel | `ivreg` | [`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html) | yes | \- | \- |
| Robust, IV, quantile, panel | `tobit` | [`AER::tobit()`](https://rdrr.io/pkg/AER/man/tobit.html) | yes | \- | \- |
| Robust, IV, quantile, panel | `rq` | [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) | yes | \- | \- |
| Robust, IV, quantile, panel | `fixest` | [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html), [`fixest::feglm()`](https://lrberge.github.io/fixest/reference/feglm.html), [`fixest::fepois()`](https://lrberge.github.io/fixest/reference/feglm.html), [`fixest::fenegbin()`](https://lrberge.github.io/fixest/reference/femlm.html) | yes | `feglm`: OR / IRR | \- |
| Mixed effects | `lmerMod` | [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) | yes | \- | Random effects |
| Mixed effects | `glmerMod` | [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) | yes | OR / IRR (link) | Random effects |
| Mixed effects | `glmmTMB` | [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) | yes (no for the ordinal family) | link-dependent (IRR for count families, OR for ordinal logit) | Random effects; Zero-inflation; Dispersion; Thresholds (ordinal family) |
| Mixed effects | `lme` | [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) | yes | \- | Random effects |
| Mixed effects | `gls` | [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) | yes | \- | \- |
| Population-averaged (GEE) | `geeglm` | [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html) | yes | OR / IRR / RR / MR / HR (link) | \- |
| Ordinal | `polr` | [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) | per category | OR (logit) | Thresholds |
| Ordinal | `clm` | [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) | per category | OR (logit) | Thresholds; Non-proportional effects |
| Ordinal | `clmm` | [`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html) | no | OR (logit) | Thresholds; Random effects |
| Categorical | `multinom` | [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) | per outcome | OR | per-outcome blocks |
| Categorical | `mlogit` | [`mlogit::mlogit()`](https://rdrr.io/pkg/mlogit/man/mlogit.html) | no | OR | per-alternative rows |
| Counts, two-part | `zeroinfl` | [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero-inflation |
| Counts, two-part | `hurdle` | [`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero hurdle |
| Survival | `coxph` | [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) | RMST / risk diff | HR | \- |
| Survival | `survreg` | [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html) | yes + RMST / risk diff | TR (log-scale distributions) | \- |
| Survival | `cph` | [`rms::cph()`](https://rdrr.io/pkg/rms/man/cph.html) | no | HR | \- |
| Survival | `flexsurvreg` | [`flexsurv::flexsurvreg()`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md) | no | TR / HR (dist) | distribution parameters |
| Survey-weighted | `svyglm` | [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) | yes (design-based) | OR / IRR | \- |
| Survey-weighted | `svyolr` | [`survey::svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) | per category (design-based) | OR (logit) | Thresholds |
| Survey-weighted | `svycoxph` | [`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html) | no | HR | \- |
| Additive, proportions, selection | `gam` | [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html), [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) | yes | OR / IRR (link) | \- |
| Additive, proportions, selection | `betareg` | [`betareg::betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html) | yes | OR (mean link) | \- |
| Additive, proportions, selection | `selection` | [`sampleSelection::selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html) | no | \- | selection component |
| rms | `ols` | [`rms::ols()`](https://rdrr.io/pkg/rms/man/ols.html) | yes | \- | \- |
| rms | `lrm` | [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) | yes | OR | \- |
| rms | `Glm` | [`rms::Glm()`](https://rdrr.io/pkg/rms/man/Glm.html) | yes | link-dependent | \- |
| Bayesian | `stanreg` | [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html), [`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html) | yes (draws) | link-dependent | Random effects (if multilevel) |
| Bayesian | `brmsfit` | [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) | yes (draws) | link-dependent | Random effects (if multilevel) |

## Shared semantics (all classes)

- A robust `vcov` request is honored through the class's field-standard
  backend, or **refused with a clear error** naming the supported set;
  the footer always names the estimator actually applied.

- `exponentiate = TRUE` is link-gated: it produces a labeled ratio (OR /
  IRR / HR / RR / MR / TR) only where the link warrants one.
  Identity-link fits warn and are left untouched; non-ratio links
  (probit, cauchit, inverse, ...) are **refused with a clear error**.

- Class-specific structure renders as labeled subordinate blocks of rows
  in the same table, each explained by a footer line.

- Fit statistics default to the family's field standard
  (`show_fit_stats` overrides; class-inappropriate tokens are rejected
  with a pointer to the right ones).

- Everything is available programmatically:
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html),
  `glance()`,
  [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md),
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Mixed effects

Fixed effects: Satterthwaite t (`lmer` + lmerTest), Wald z (`glmer`,
`glmmTMB`), containment-df t (`lme`). Random effects render as a
`Random effects` block of rows (SD / correlation / residual with SE and
CI; `re_scale`, `re_columns`), deliberately with **no per-row p-value**
(boundary-invalid Wald; Self & Liang 1987) – the footer carries the
chi-bar-squared LR test of the whole random part, and `re_test = "lrt"`
/ `"rlrt"` adds an opt-in boundary-correct per-term test. `N (groups)`
and `ICC` are fit-stat rows; Nakagawa marginal / conditional R-squared
are the default R-squared family. `CR*` cluster-robust standard errors
are available for `lmer` and `lme` fits, via clubSandwich with
Satterthwaite degrees of freedom. `glmer` and `glmmTMB` fits keep their
model-based standard errors: a `CR*` request on them is refused with a
clear error.

## Population-averaged (GEE) models

[`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html) fits
are read on their own terms: the sandwich standard errors the fit
computed (its `std.err =` option, clustered on its `id =`) are the
displayed inference – GEE is robust by construction, so spicy's `vcov` /
`cluster` arguments are refused with a pointer to the fit options.
Coefficients are population-averaged (marginal) effects; the footer
discloses the working correlation structure with its estimated alpha.
Wald z inference; `exponentiate` follows the usual link gates (OR / IRR
/ RR). Default fit statistics report the cluster structure (n,
`N (<id>)`, largest cluster); the quasi-likelihood information criteria
`"qic"` / `"qicu"` (Pan 2001) and the `"scale"` (dispersion) parameter
are opt-in – there is no likelihood, so AIC, pseudo-R-squared,
`nested = TRUE`, and `standardized` are refused. See the
population-averaged section of the [Mixed-effects regression
tables](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.html)
article for the contrast with subject-specific mixed models.

## Ordinal models

Cut-points render as a `Thresholds` block (log-odds scale, never
exponentiated; `show_thresholds`). Partial-proportional-odds `clm` terms
render as a `Non-proportional effects` block, one coefficient per
cut-point. `exponentiate` yields proportional odds ratios under logit;
`ci_method = "profile"` profiles the predictor coefficients. AME is
per-category (the marginal effect on each P(Y = k)). Defaults include
McFadden and Nagelkerke pseudo-R-squared. See the [Ordinal regression
tables](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.html)
article.

Cumulative-link mixed models
([`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html), and
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html)
with `family = glmmTMB::ordinal()`) add a `Random effects` block and the
mixed-model fit statistics (n, groups, AIC, BIC; no R-squared). Their
inference is Wald only: AME, a robust `vcov`, `ci_method = "profile"`
and `standardized` are refused, as is `nested = TRUE` for `clmm`.
`exponentiate` follows the `clm` rule (odds ratios under logit, hazard
ratios under cloglog), except for a `clmm` fit with the cloglog link,
where it is refused: on the current `ordinal`, the cloglog and loglog
links of `clmm()` do not match those of `clm()` and of `glmmTMB()`.

## Counts and two-part models

Two-part models show their full model: the zero component renders as a
`Zero-inflation` block (`zeroinfl`, glmmTMB `ziformula`: probability of
a structural zero) or a `Zero hurdle` block (`hurdle`: probability of a
nonzero count – the opposite direction, hence the distinct label), and a
`Dispersion` block when `dispformula` has covariates. Component
coefficients join the `p_adjust` family and take stars; a zero component
is exponentiated only under a logit link (odds ratio). AME is the
combined-response effect on E(Y). `CR*` for `pscl` fits covers both
components via
[`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html).
Opt out with `show_components = FALSE`.

## Categorical outcomes

`multinom` renders per non-reference outcome; `exponentiate` yields odds
ratios of each outcome against the reference outcome – the
baseline-category logits are log-odds (Agresti; SAS prints "Odds Ratio
Estimates" under its generalized-logit link; Stata's `mlogit, rrr`
labels the same quantity a relative-risk ratio). AME is per-outcome.
`nested = TRUE` compares nested `multinom` fits by likelihood-ratio test
(the `anova.multinom()` convention). Cluster-robust `CR*` is available
(one cluster value per observation; sandwich \>= 3.1-2) and the AME
columns honor it; `HC*` is refused – a multi-equation model has no
working residuals. `mlogit` renders per-alternative rows; AME is refused
(no `slopes()` method exists for its data format). `CR*` is available
with one cluster value per choice situation, and `n` counts choice
situations; `HC*` is refused
([`sandwich::vcovHC()`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html)
mis-scales the meat for mlogit's per-chooser score structure).

## Survival models

Cox models exponentiate to hazard ratios; `survreg` log-scale
distributions to time ratios (identity-scale distributions are left
untouched). AME is refused for Cox fits (no marginal-probability effect
on the hazard scale); their absolute-effect columns are the `"rmst"` and
`"risk_diff"` families instead – covariate-adjusted RMST and
cumulative-incidence differences by g-computation, with the mandatory
`tau` / `at_time` horizons. For `coxph`: right-censored single-record
fits, `strata()` supported (within-stratum baselines), `tt()` refused.
For `survreg`: the closed-form AFT curves are standardized directly
(stratified `survreg` refused). `CR*` uses the Lin-Wei grouped-dfbeta
sandwich (`coxph`) or
[`rms::robcov()`](https://rdrr.io/pkg/rms/man/robcov.html) (`cph`, needs
`x = TRUE, y = TRUE`). `nested = TRUE` compares nested Cox fits by
likelihood-ratio test.

## Survey-design models

Fits from a
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
or
[`survey::as.svrepdesign()`](https://rdrr.io/pkg/survey/man/as.svrepdesign.html)
design – `svyglm` (and its replicate sibling `svrepglm`), `svyolr`,
`svycoxph` (and `svrepcoxph`) – are read as design-based throughout: the
coefficients, the variance and the reference distribution all come from
survey.

Inference is Wald **t** at the degrees of freedom survey writes on the
FIT – `df.residual` for `svyglm` / `svyolr`, `degf.resid` or
`degf.residual` for the two Cox engines – which is what
[`survey::regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html)
takes as its denominator. It is not `survey::degf(design)`, and it is
not re-derived here: the six engines of survey do not share one
expression and are not harmonized (a Cox fit carries
`degf(design) - p + 1` although it has no intercept for the `+ 1` to
cancel, so it ends one above the two other classes). The value is read
off the object. The footer names the design and prints the number, and
the average marginal effects answer to the same distribution as the
coefficient rows.

The average marginal effect is the Horvitz-Thompson estimator: the mean
unit-level effect weighted by the sampling weights of the analytic
sample, with its variance from the delta method on the design vcov.

Counts are both reported: the observed `n` and the `Weighted n` the
estimates describe. `svycoxph` adds the number of events, and its
concordance goes to the footer.

What is refused, and why: every model-derived variance (`HC*`, `CR*`,
bootstrap, jackknife) – the design is the variance authority, and the
way to change the estimator is to change the design; every likelihood
statistic (AIC, BIC, logLik, deviance, pseudo-R-squared) for `svyolr`
and `svycoxph` – there is no likelihood, and survey's own
[`deviance()`](https://rdrr.io/r/stats/deviance.html) returns a
sign-flipped likelihood-ratio statistic on one Cox engine and a bare
zero on the other; the AME for `svycoxph`, on the same ground as for a
plain Cox fit; and the `"rmst"` / `"risk_diff"` columns for `svycoxph`,
whose uncertainty comes from resampling subjects and so ignores the
strata and clusters the design declares (use
[`survey::svykm()`](https://rdrr.io/pkg/survey/man/svykm.html) for a
marginal curve).

`nested = TRUE` is refused for a design-based table: there is no
likelihood to compare, so every change statistic would be empty, and a
block of empty rows reads like an answer. Put the models side by side
with `nested = FALSE`, and test a term under the design with
[`survey::regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html).

`svyglm` keeps an `AIC` row: survey's `extractAIC.svyglm` computes the
design-based AIC of Lumley & Scott (2015), the one information criterion
published for this class, and `show_fit_stats = "eff_p"` reports the
effective number of design parameters beside it. `BIC.svyglm` requires a
maximal model and has no default, so it stays blank. See the [Summary
tables from a survey
design](https://amaltawfik.github.io/spicy/articles/survey-tables.html)
article.

## Robust, IV, quantile and panel models

`estimatr` fits keep their own robust SEs (never overwritten);
[`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) defaults to
the heteroskedasticity-robust `"nid"` sandwich (quantreg's own
large-sample default), with `"iid"`, `"ker"`, `"rank"` (CIs only) and a
native `"bootstrap"` – clustered via the wild gradient bootstrap – as
`vcov` options (the footer names the estimator); `fixest` fits – and
`estimatr` fits built with `fixed_effects =` – disclose their absorbed
fixed effects as a default-on `Fixed effects:` block (one Yes / No row
per factor; varying-slope-only factors are not absorbed intercepts and
read No), with per-factor `N (<factor>)` counts via the opt-in
`n_groups` token and the within R-squared in the default fit statistics
for `fixest` (opt-in via `within_r2` for `estimatr`).

## Bayesian models

Posterior median, posterior MAD SD, and equal-tailed credible intervals
(`ci_method = "hdi"` opts into the highest-density interval);
deliberately no p-value column and no stars – the probability of
direction (`"pd"`) is the opt-in posterior summary. A
sampler-diagnostics guard checks every fit (R-hat, ESS, divergences,
E-BFMI) and per-coefficient `"rhat"` / `"ess_bulk"` / `"ess_tail"` /
`"mcse"` columns are available. The AME columns are draws-native
(posterior median, MAD SD and credible interval of the per-draw
`avg_slopes()`; no `"ame_p"`), and so are the standardized betas
(`"posthoc"` / `"basic"` / `"smart"`, exact affine rescales of the
draws) on fixed-effects fits: `stan_glm`-style models and
standard-formula `brm()` models, whose design matrix is recovered
through insight. Multilevel fits, `stan_polr` / `stan_betareg`, brms
formulas with distributional or special terms, and `"refit"` /
`"pseudo"` are refused with a pre-standardization hint. Multilevel fits
(`stan_glmer`, `brm` with grouping terms) report their random effects as
a block – posterior median SD and credible interval per component, from
the draws – with no likelihood-ratio line. `p_adjust` and
likelihood-based fit-statistic tokens are refused (no p-values, no
likelihood-based information criteria in a posterior); `"r2_bayes"` is
in the default fit statistics and `"elpd_loo"` / `"looic"` / `"waic"`
are opt-in, with standard errors and reliability caveats in the footer;
compare models with
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
outside the table.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
the [Publication-ready regression
tables](https://amaltawfik.github.io/spicy/articles/table-regression.html)
and [Ordinal regression
tables](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.html)
articles.

## Examples

``` r
table_regression_models()
#>                              family       class
#> 1     Linear and generalized linear          lm
#> 2     Linear and generalized linear         glm
#> 3     Linear and generalized linear      negbin
#> 4     Linear and generalized linear         rlm
#> 5     Linear and generalized linear         nls
#> 6       Robust, IV, quantile, panel   lm_robust
#> 7       Robust, IV, quantile, panel   iv_robust
#> 8       Robust, IV, quantile, panel       ivreg
#> 9       Robust, IV, quantile, panel       tobit
#> 10      Robust, IV, quantile, panel          rq
#> 11      Robust, IV, quantile, panel      fixest
#> 12                    Mixed effects     lmerMod
#> 13                    Mixed effects    glmerMod
#> 14                    Mixed effects     glmmTMB
#> 15                    Mixed effects         lme
#> 16                    Mixed effects         gls
#> 17        Population-averaged (GEE)      geeglm
#> 18                          Ordinal        polr
#> 19                          Ordinal         clm
#> 20                          Ordinal        clmm
#> 21                      Categorical    multinom
#> 22                      Categorical      mlogit
#> 23                 Counts, two-part    zeroinfl
#> 24                 Counts, two-part      hurdle
#> 25                         Survival       coxph
#> 26                         Survival     survreg
#> 27                         Survival         cph
#> 28                         Survival flexsurvreg
#> 29                  Survey-weighted      svyglm
#> 30                  Survey-weighted      svyolr
#> 31                  Survey-weighted    svycoxph
#> 32 Additive, proportions, selection         gam
#> 33 Additive, proportions, selection     betareg
#> 34 Additive, proportions, selection   selection
#> 35                              rms         ols
#> 36                              rms         lrm
#> 37                              rms         Glm
#> 38                         Bayesian     stanreg
#> 39                         Bayesian     brmsfit
#>                                                                    engine
#> 1                                                             stats::lm()
#> 2                                                            stats::glm()
#> 3                                                          MASS::glm.nb()
#> 4                                                             MASS::rlm()
#> 5                                                            stats::nls()
#> 6                                                   estimatr::lm_robust()
#> 7                                                   estimatr::iv_robust()
#> 8                                                            AER::ivreg()
#> 9                                                            AER::tobit()
#> 10                                                         quantreg::rq()
#> 11 fixest::feols(), fixest::feglm(), fixest::fepois(), fixest::fenegbin()
#> 12                                                           lme4::lmer()
#> 13                                                          lme4::glmer()
#> 14                                                     glmmTMB::glmmTMB()
#> 15                                                            nlme::lme()
#> 16                                                            nlme::gls()
#> 17                                                      geepack::geeglm()
#> 18                                                           MASS::polr()
#> 19                                                         ordinal::clm()
#> 20                                                        ordinal::clmm()
#> 21                                                       nnet::multinom()
#> 22                                                       mlogit::mlogit()
#> 23                                                       pscl::zeroinfl()
#> 24                                                         pscl::hurdle()
#> 25                                                      survival::coxph()
#> 26                                                    survival::survreg()
#> 27                                                             rms::cph()
#> 28                                                flexsurv::flexsurvreg()
#> 29                                                       survey::svyglm()
#> 30                                                       survey::svyolr()
#> 31                                                     survey::svycoxph()
#> 32                                               mgcv::gam(), mgcv::bam()
#> 33                                                     betareg::betareg()
#> 34                                           sampleSelection::selection()
#> 35                                                             rms::ols()
#> 36                                                             rms::lrm()
#> 37                                                             rms::Glm()
#> 38                           rstanarm::stan_glm(), rstanarm::stan_glmer()
#> 39                                                            brms::brm()
#>                                ame
#> 1                              yes
#> 2                              yes
#> 3                              yes
#> 4                              yes
#> 5                               no
#> 6                              yes
#> 7                              yes
#> 8                              yes
#> 9                              yes
#> 10                             yes
#> 11                             yes
#> 12                             yes
#> 13                             yes
#> 14 yes (no for the ordinal family)
#> 15                             yes
#> 16                             yes
#> 17                             yes
#> 18                    per category
#> 19                    per category
#> 20                              no
#> 21                     per outcome
#> 22                              no
#> 23         yes (combined response)
#> 24         yes (combined response)
#> 25                RMST / risk diff
#> 26          yes + RMST / risk diff
#> 27                              no
#> 28                              no
#> 29              yes (design-based)
#> 30     per category (design-based)
#> 31                              no
#> 32                             yes
#> 33                             yes
#> 34                              no
#> 35                             yes
#> 36                             yes
#> 37                             yes
#> 38                     yes (draws)
#> 39                     yes (draws)
#>                                                     exponentiate
#> 1                                                              -
#> 2                                 OR / IRR / RR / MR / HR (link)
#> 3                                                            IRR
#> 4                                                              -
#> 5                                                              -
#> 6                                                              -
#> 7                                                              -
#> 8                                                              -
#> 9                                                              -
#> 10                                                             -
#> 11                                             `feglm`: OR / IRR
#> 12                                                             -
#> 13                                               OR / IRR (link)
#> 14 link-dependent (IRR for count families, OR for ordinal logit)
#> 15                                                             -
#> 16                                                             -
#> 17                                OR / IRR / RR / MR / HR (link)
#> 18                                                    OR (logit)
#> 19                                                    OR (logit)
#> 20                                                    OR (logit)
#> 21                                                            OR
#> 22                                                            OR
#> 23                            IRR (count) + OR (logit zero part)
#> 24                            IRR (count) + OR (logit zero part)
#> 25                                                            HR
#> 26                                  TR (log-scale distributions)
#> 27                                                            HR
#> 28                                                TR / HR (dist)
#> 29                                                      OR / IRR
#> 30                                                    OR (logit)
#> 31                                                            HR
#> 32                                               OR / IRR (link)
#> 33                                                OR (mean link)
#> 34                                                             -
#> 35                                                             -
#> 36                                                            OR
#> 37                                                link-dependent
#> 38                                                link-dependent
#> 39                                                link-dependent
#>                                                                     blocks
#> 1                                                                        -
#> 2                                                                        -
#> 3                                                                        -
#> 4                                                                        -
#> 5                                                                        -
#> 6                                                                        -
#> 7                                                                        -
#> 8                                                                        -
#> 9                                                                        -
#> 10                                                                       -
#> 11                                                                       -
#> 12                                                          Random effects
#> 13                                                          Random effects
#> 14 Random effects; Zero-inflation; Dispersion; Thresholds (ordinal family)
#> 15                                                          Random effects
#> 16                                                                       -
#> 17                                                                       -
#> 18                                                              Thresholds
#> 19                                    Thresholds; Non-proportional effects
#> 20                                              Thresholds; Random effects
#> 21                                                      per-outcome blocks
#> 22                                                    per-alternative rows
#> 23                                                          Zero-inflation
#> 24                                                             Zero hurdle
#> 25                                                                       -
#> 26                                                                       -
#> 27                                                                       -
#> 28                                                 distribution parameters
#> 29                                                                       -
#> 30                                                              Thresholds
#> 31                                                                       -
#> 32                                                                       -
#> 33                                                                       -
#> 34                                                     selection component
#> 35                                                                       -
#> 36                                                                       -
#> 37                                                                       -
#> 38                                          Random effects (if multilevel)
#> 39                                          Random effects (if multilevel)

# All engines of one family:
subset(table_regression_models(), family == "Mixed effects")
#>           family    class             engine                             ame
#> 12 Mixed effects  lmerMod       lme4::lmer()                             yes
#> 13 Mixed effects glmerMod      lme4::glmer()                             yes
#> 14 Mixed effects  glmmTMB glmmTMB::glmmTMB() yes (no for the ordinal family)
#> 15 Mixed effects      lme        nlme::lme()                             yes
#> 16 Mixed effects      gls        nlme::gls()                             yes
#>                                                     exponentiate
#> 12                                                             -
#> 13                                               OR / IRR (link)
#> 14 link-dependent (IRR for count families, OR for ordinal logit)
#> 15                                                             -
#> 16                                                             -
#>                                                                     blocks
#> 12                                                          Random effects
#> 13                                                          Random effects
#> 14 Random effects; Zero-inflation; Dispersion; Thresholds (ordinal family)
#> 15                                                          Random effects
#> 16                                                                       -
```
