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
| Linear and generalized linear | `lm` | [`stats::lm`](https://rdrr.io/r/stats/lm.html) | yes | \- | \- |
| Linear and generalized linear | `glm` | [`stats::glm`](https://rdrr.io/r/stats/glm.html) | yes | OR / IRR / RR (link) | \- |
| Linear and generalized linear | `negbin` | [`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html) | yes | IRR | \- |
| Linear and generalized linear | `rlm` | [`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html) | yes | \- | \- |
| Linear and generalized linear | `nls` | [`stats::nls`](https://rdrr.io/r/stats/nls.html) | no | \- | \- |
| Robust, IV, quantile, panel | `lm_robust` | [`estimatr::lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.html) | yes | \- | \- |
| Robust, IV, quantile, panel | `iv_robust` | [`estimatr::iv_robust`](https://declaredesign.org/r/estimatr/reference/iv_robust.html) | yes | \- | \- |
| Robust, IV, quantile, panel | `ivreg` | [`AER::ivreg`](https://rdrr.io/pkg/AER/man/ivreg.html) | yes | \- | \- |
| Robust, IV, quantile, panel | `tobit` | [`AER::tobit`](https://rdrr.io/pkg/AER/man/tobit.html) | yes | \- | \- |
| Robust, IV, quantile, panel | `rq` | [`quantreg::rq`](https://rdrr.io/pkg/quantreg/man/rq.html) | yes | \- | \- |
| Robust, IV, quantile, panel | `fixest` | `fixest::feols / feglm / fepois / fenegbin` | yes | `feglm`: OR / IRR | \- |
| Mixed effects | `lmerMod` | [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) | yes | \- | Random effects |
| Mixed effects | `glmerMod` | [`lme4::glmer`](https://rdrr.io/pkg/lme4/man/glmer.html) | yes | OR / IRR (link) | Random effects |
| Mixed effects | `glmmTMB` | [`glmmTMB::glmmTMB`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) | yes | link-dependent (IRR for count families) | Random effects; Zero-inflation; Dispersion |
| Mixed effects | `lme` | [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) | yes | \- | Random effects |
| Mixed effects | `gls` | [`nlme::gls`](https://rdrr.io/pkg/nlme/man/gls.html) | yes | \- | \- |
| Ordinal | `polr` | [`MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html) | per category | OR (logit) | Thresholds |
| Ordinal | `clm` | [`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html) | per category | OR (logit) | Thresholds; Non-proportional effects |
| Categorical | `multinom` | [`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html) | per outcome | RRR | per-outcome blocks |
| Categorical | `mlogit` | [`mlogit::mlogit`](https://rdrr.io/pkg/mlogit/man/mlogit.html) | no | OR | per-alternative rows |
| Counts, two-part | `zeroinfl` | [`pscl::zeroinfl`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero-inflation |
| Counts, two-part | `hurdle` | [`pscl::hurdle`](https://rdrr.io/pkg/pscl/man/hurdle.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero hurdle |
| Survival | `coxph` | [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) | no | HR | \- |
| Survival | `survreg` | [`survival::survreg`](https://rdrr.io/pkg/survival/man/survreg.html) | yes | TR (log-scale distributions) | \- |
| Survival | `cph` | [`rms::cph`](https://rdrr.io/pkg/rms/man/cph.html) | no | HR | \- |
| Survival | `flexsurvreg` | [`flexsurv::flexsurvreg`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md) | yes | TR / HR (dist) | distribution parameters |
| Survey-weighted | `svyglm` | [`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html) | yes (design-based) | OR / IRR | \- |
| Additive, proportions, selection | `gam` | `mgcv::gam / bam` | yes | OR / IRR (link) | \- |
| Additive, proportions, selection | `betareg` | [`betareg::betareg`](https://rdrr.io/pkg/betareg/man/betareg.html) | yes | OR (mean link) | \- |
| Additive, proportions, selection | `selection` | [`sampleSelection::selection`](https://rdrr.io/pkg/sampleSelection/man/selection.html) | no | \- | selection component |
| rms | `ols` | [`rms::ols`](https://rdrr.io/pkg/rms/man/ols.html) | yes | \- | \- |
| rms | `lrm` | [`rms::lrm`](https://rdrr.io/pkg/rms/man/lrm.html) | yes | OR | \- |
| rms | `Glm` | [`rms::Glm`](https://rdrr.io/pkg/rms/man/Glm.html) | yes | link-dependent | \- |
| Bayesian | `stanreg` | `rstanarm::stan_glm / stan_glmer / ...` | yes | link-dependent | Random effects (if multilevel) |
| Bayesian | `brmsfit` | [`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html) | yes | link-dependent | Random effects (if multilevel) |

## Shared semantics (all classes)

- A robust `vcov` request is honoured through the class's field-standard
  backend, or **refused with a clear error** naming the supported set –
  never silently ignored; the footer always names the estimator actually
  applied.

- `exponentiate = TRUE` is link-gated: it produces a labelled ratio (OR
  / IRR / HR / RR / MR / RRR / TR) only where the link warrants one;
  identity-link fits warn and are left untouched.

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
[`sandwich::vcovCL`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html).
Opt out with `show_components = FALSE`.

## Categorical outcomes

`multinom` renders per non-reference outcome; `exponentiate` yields
relative-risk ratios (RRR); AME is per-outcome. `mlogit` renders
per-alternative rows; AME is refused (no `slopes()` method exists for
its data format); `HC*` and `CR*` are available.

## Survival models

Cox models exponentiate to hazard ratios; `survreg` log-scale
distributions to time ratios (identity-scale distributions are left
untouched). AME is refused for Cox fits (no marginal-probability effect
on the hazard scale). `CR*` uses the Lin-Wei grouped-dfbeta sandwich
(`coxph`) or [`rms::robcov`](https://rdrr.io/pkg/rms/man/robcov.html)
(`cph`, needs `x = TRUE, y = TRUE`). `nested = TRUE` compares nested Cox
fits by likelihood-ratio test.

## Robust, IV, quantile and panel models

`estimatr` fits keep their own robust SEs (never overwritten);
[`quantreg::rq`](https://rdrr.io/pkg/quantreg/man/rq.html) honours its
`se =` method, including rank-inversion CIs; `fixest` fits report their
fixed-effects structure in the footer.

## Bayesian models

Posterior median, posterior SD, and equal-tailed credible intervals;
deliberately no p-value column and no stars.

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
#>                                       engine                     ame
#> 1                                  stats::lm                     yes
#> 2                                 stats::glm                     yes
#> 3                               MASS::glm.nb                     yes
#> 4                                  MASS::rlm                     yes
#> 5                                 stats::nls                      no
#> 6                        estimatr::lm_robust                     yes
#> 7                        estimatr::iv_robust                     yes
#> 8                                 AER::ivreg                     yes
#> 9                                 AER::tobit                     yes
#> 10                              quantreg::rq                     yes
#> 11 fixest::feols / feglm / fepois / fenegbin                     yes
#> 12                                lme4::lmer                     yes
#> 13                               lme4::glmer                     yes
#> 14                          glmmTMB::glmmTMB                     yes
#> 15                                 nlme::lme                     yes
#> 16                                 nlme::gls                     yes
#> 17                                MASS::polr            per category
#> 18                              ordinal::clm            per category
#> 19                            nnet::multinom             per outcome
#> 20                            mlogit::mlogit                      no
#> 21                            pscl::zeroinfl yes (combined response)
#> 22                              pscl::hurdle yes (combined response)
#> 23                           survival::coxph                      no
#> 24                         survival::survreg                     yes
#> 25                                  rms::cph                      no
#> 26                     flexsurv::flexsurvreg                     yes
#> 27                            survey::svyglm      yes (design-based)
#> 28                           mgcv::gam / bam                     yes
#> 29                          betareg::betareg                     yes
#> 30                sampleSelection::selection                      no
#> 31                                  rms::ols                     yes
#> 32                                  rms::lrm                     yes
#> 33                                  rms::Glm                     yes
#> 34     rstanarm::stan_glm / stan_glmer / ...                     yes
#> 35                                 brms::brm                     yes
#>                               exponentiate
#> 1                                        -
#> 2                     OR / IRR / RR (link)
#> 3                                      IRR
#> 4                                        -
#> 5                                        -
#> 6                                        -
#> 7                                        -
#> 8                                        -
#> 9                                        -
#> 10                                       -
#> 11                       `feglm`: OR / IRR
#> 12                                       -
#> 13                         OR / IRR (link)
#> 14 link-dependent (IRR for count families)
#> 15                                       -
#> 16                                       -
#> 17                              OR (logit)
#> 18                              OR (logit)
#> 19                                     RRR
#> 20                                      OR
#> 21      IRR (count) + OR (logit zero part)
#> 22      IRR (count) + OR (logit zero part)
#> 23                                      HR
#> 24            TR (log-scale distributions)
#> 25                                      HR
#> 26                          TR / HR (dist)
#> 27                                OR / IRR
#> 28                         OR / IRR (link)
#> 29                          OR (mean link)
#> 30                                       -
#> 31                                       -
#> 32                                      OR
#> 33                          link-dependent
#> 34                          link-dependent
#> 35                          link-dependent
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
#>           family    class           engine ame
#> 12 Mixed effects  lmerMod       lme4::lmer yes
#> 13 Mixed effects glmerMod      lme4::glmer yes
#> 14 Mixed effects  glmmTMB glmmTMB::glmmTMB yes
#> 15 Mixed effects      lme        nlme::lme yes
#> 16 Mixed effects      gls        nlme::gls yes
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
