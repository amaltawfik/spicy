# Supported models and per-family behaviour of table_regression()

`table_regression_models()` returns the registry of model classes
supported by
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
one row per engine, with each class's family, average-marginal-effects
estimand, `exponentiate` semantics, and labelled table blocks. The same
registry drives this page's table, so the published list cannot drift
from the code.

This page is also the reference for **per-family behaviour** (the
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
| Linear and generalized linear | `glm` | [`stats::glm()`](https://rdrr.io/r/stats/glm.html) | yes | OR / IRR / RR (link) | \- |
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
| Mixed effects | `glmmTMB` | [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) | yes | link-dependent (IRR for count families) | Random effects; Zero-inflation; Dispersion |
| Mixed effects | `lme` | [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) | yes | \- | Random effects |
| Mixed effects | `gls` | [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) | yes | \- | \- |
| Ordinal | `polr` | [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) | per category | OR (logit) | Thresholds |
| Ordinal | `clm` | [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) | per category | OR (logit) | Thresholds; Non-proportional effects |
| Categorical | `multinom` | [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) | per outcome | OR | per-outcome blocks |
| Categorical | `mlogit` | [`mlogit::mlogit()`](https://rdrr.io/pkg/mlogit/man/mlogit.html) | no | OR | per-alternative rows |
| Counts, two-part | `zeroinfl` | [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero-inflation |
| Counts, two-part | `hurdle` | [`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero hurdle |
| Survival | `coxph` | [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) | RMST / risk diff | HR | \- |
| Survival | `survreg` | [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html) | yes + RMST / risk diff | TR (log-scale distributions) | \- |
| Survival | `cph` | [`rms::cph()`](https://rdrr.io/pkg/rms/man/cph.html) | no | HR | \- |
| Survival | `flexsurvreg` | [`flexsurv::flexsurvreg()`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md) | no | TR / HR (dist) | distribution parameters |
| Survey-weighted | `svyglm` | [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) | yes (design-based) | OR / IRR | \- |
| Additive, proportions, selection | `gam` | [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html), [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) | yes | OR / IRR (link) | \- |
| Additive, proportions, selection | `betareg` | [`betareg::betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html) | yes | OR (mean link) | \- |
| Additive, proportions, selection | `selection` | [`sampleSelection::selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html) | no | \- | selection component |
| rms | `ols` | [`rms::ols()`](https://rdrr.io/pkg/rms/man/ols.html) | yes | \- | \- |
| rms | `lrm` | [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) | yes | OR | \- |
| rms | `Glm` | [`rms::Glm()`](https://rdrr.io/pkg/rms/man/Glm.html) | yes | link-dependent | \- |
| Bayesian | `stanreg` | [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html), [`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html) | yes (draws) | link-dependent | Random effects (if multilevel) |
| Bayesian | `brmsfit` | [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) | yes (draws) | link-dependent | Random effects (if multilevel) |

## Shared semantics (all classes)

- A robust `vcov` request is honoured through the class's field-standard
  backend, or **refused with a clear error** naming the supported set;
  the footer always names the estimator actually applied.

- `exponentiate = TRUE` is link-gated: it produces a labelled ratio (OR
  / IRR / HR / RR / MR / TR) only where the link warrants one.
  Identity-link fits warn and are left untouched; non-ratio links
  (probit, cauchit, inverse, ...) are **refused with a clear error**.

- Class-specific structure renders as labelled subordinate blocks of
  rows in the same table, each explained by a footer line.

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
are the default R-squared family. `CR*` robust via clubSandwich
(glmmTMB: conditional part only, disclosed).

## Ordinal models

Cut-points render as a `Thresholds` block (log-odds scale, never
exponentiated; `show_thresholds`). Partial-proportional-odds `clm` terms
render as a `Non-proportional effects` block, one coefficient per
cut-point. `exponentiate` yields proportional odds ratios under logit;
`ci_method = "profile"` profiles the predictor coefficients. AME is
per-category (the marginal effect on each P(Y = k)). Defaults include
McFadden and Nagelkerke pseudo-R-squared. See
[`vignette("table-regression-ordinal")`](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.md).

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
[`sandwich::vcovCL()`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html).
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
columns honour it; `HC*` is refused – a multi-equation model has no
working residuals. `mlogit` renders per-alternative rows; AME is refused
(no `slopes()` method exists for its data format). `CR*` is available
with one cluster value per choice situation, and `n` counts choice
situations; `HC*` is refused
([`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)
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

## Robust, IV, quantile and panel models

`estimatr` fits keep their own robust SEs (never overwritten);
[`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) honours its
`se =` method, including rank-inversion CIs; `fixest` fits report their
fixed-effects structure in the footer.

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
draws; fixed-effects `stan_glm`-style fits only – multilevel and
`stan_polr` / `stan_betareg` fits are refused, `brmsfit` carries no
recoverable design matrix; `"refit"` / `"pseudo"` refused). Multilevel
fits (`stan_glmer`, `brm` with grouping terms) report their random
effects as a block – posterior median SD and credible interval per
component, from the draws – with no likelihood-ratio line. `p_adjust`
and likelihood-based fit-statistic tokens are refused (no p-values, no
likelihood-based information criteria in a posterior); `"r2_bayes"` is
in the default fit statistics and `"elpd_loo"` / `"looic"` / `"waic"`
are opt-in, with standard errors and reliability caveats in the footer;
compare models with
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
outside the table.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
[`vignette("table-regression")`](https://amaltawfik.github.io/spicy/articles/table-regression.md)
and
[`vignette("table-regression-ordinal")`](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.md).

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
#> 17                          Ordinal        polr
#> 18                          Ordinal         clm
#> 19                      Categorical    multinom
#> 20                      Categorical      mlogit
#> 21                 Counts, two-part    zeroinfl
#> 22                 Counts, two-part      hurdle
#> 23                         Survival       coxph
#> 24                         Survival     survreg
#> 25                         Survival         cph
#> 26                         Survival flexsurvreg
#> 27                  Survey-weighted      svyglm
#> 28 Additive, proportions, selection         gam
#> 29 Additive, proportions, selection     betareg
#> 30 Additive, proportions, selection   selection
#> 31                              rms         ols
#> 32                              rms         lrm
#> 33                              rms         Glm
#> 34                         Bayesian     stanreg
#> 35                         Bayesian     brmsfit
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
#> 17                                                           MASS::polr()
#> 18                                                         ordinal::clm()
#> 19                                                       nnet::multinom()
#> 20                                                       mlogit::mlogit()
#> 21                                                       pscl::zeroinfl()
#> 22                                                         pscl::hurdle()
#> 23                                                      survival::coxph()
#> 24                                                    survival::survreg()
#> 25                                                             rms::cph()
#> 26                                                flexsurv::flexsurvreg()
#> 27                                                       survey::svyglm()
#> 28                                               mgcv::gam(), mgcv::bam()
#> 29                                                     betareg::betareg()
#> 30                                           sampleSelection::selection()
#> 31                                                             rms::ols()
#> 32                                                             rms::lrm()
#> 33                                                             rms::Glm()
#> 34                           rstanarm::stan_glm(), rstanarm::stan_glmer()
#> 35                                                            brms::brm()
#>                        ame                            exponentiate
#> 1                      yes                                       -
#> 2                      yes                    OR / IRR / RR (link)
#> 3                      yes                                     IRR
#> 4                      yes                                       -
#> 5                       no                                       -
#> 6                      yes                                       -
#> 7                      yes                                       -
#> 8                      yes                                       -
#> 9                      yes                                       -
#> 10                     yes                                       -
#> 11                     yes                       `feglm`: OR / IRR
#> 12                     yes                                       -
#> 13                     yes                         OR / IRR (link)
#> 14                     yes link-dependent (IRR for count families)
#> 15                     yes                                       -
#> 16                     yes                                       -
#> 17            per category                              OR (logit)
#> 18            per category                              OR (logit)
#> 19             per outcome                                      OR
#> 20                      no                                      OR
#> 21 yes (combined response)      IRR (count) + OR (logit zero part)
#> 22 yes (combined response)      IRR (count) + OR (logit zero part)
#> 23        RMST / risk diff                                      HR
#> 24  yes + RMST / risk diff            TR (log-scale distributions)
#> 25                      no                                      HR
#> 26                      no                          TR / HR (dist)
#> 27      yes (design-based)                                OR / IRR
#> 28                     yes                         OR / IRR (link)
#> 29                     yes                          OR (mean link)
#> 30                      no                                       -
#> 31                     yes                                       -
#> 32                     yes                                      OR
#> 33                     yes                          link-dependent
#> 34             yes (draws)                          link-dependent
#> 35             yes (draws)                          link-dependent
#>                                        blocks
#> 1                                           -
#> 2                                           -
#> 3                                           -
#> 4                                           -
#> 5                                           -
#> 6                                           -
#> 7                                           -
#> 8                                           -
#> 9                                           -
#> 10                                          -
#> 11                                          -
#> 12                             Random effects
#> 13                             Random effects
#> 14 Random effects; Zero-inflation; Dispersion
#> 15                             Random effects
#> 16                                          -
#> 17                                 Thresholds
#> 18       Thresholds; Non-proportional effects
#> 19                         per-outcome blocks
#> 20                       per-alternative rows
#> 21                             Zero-inflation
#> 22                                Zero hurdle
#> 23                                          -
#> 24                                          -
#> 25                                          -
#> 26                    distribution parameters
#> 27                                          -
#> 28                                          -
#> 29                                          -
#> 30                        selection component
#> 31                                          -
#> 32                                          -
#> 33                                          -
#> 34             Random effects (if multilevel)
#> 35             Random effects (if multilevel)

# All engines of one family:
subset(table_regression_models(), family == "Mixed effects")
#>           family    class             engine ame
#> 12 Mixed effects  lmerMod       lme4::lmer() yes
#> 13 Mixed effects glmerMod      lme4::glmer() yes
#> 14 Mixed effects  glmmTMB glmmTMB::glmmTMB() yes
#> 15 Mixed effects      lme        nlme::lme() yes
#> 16 Mixed effects      gls        nlme::gls() yes
#>                               exponentiate
#> 12                                       -
#> 13                         OR / IRR (link)
#> 14 link-dependent (IRR for count families)
#> 15                                       -
#> 16                                       -
#>                                        blocks
#> 12                             Random effects
#> 13                             Random effects
#> 14 Random effects; Zero-inflation; Dispersion
#> 15                             Random effects
#> 16                                          -
```
