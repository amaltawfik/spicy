# Supported models of table_regression()

`table_regression_models()` returns the registry of model classes
supported by
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
one row per engine, with each class's family, average-marginal-effects
estimand, `exponentiate` semantics, labeled table blocks, accepted
`vcov` values and fit statistics. The same registry drives the table of
this page and those of the family pages, so the published list cannot
drift from the code.

This page is the overview: the table of every supported class, the rules
all classes share, and the page of each family, which gives its
arguments, the values the shared arguments take for it, and what it
refuses.

If a class is not listed: fit the model and call `table_regression(fit)`
anyway – unsupported classes error with a clear message naming the
supported set. Feature requests are welcome on the issue tracker.

A class listed here does not cover the objects that inherit from it
without being that model. These are refused by name: `clogit`,
multi-state `coxph`, `coxph` with a `pspline()` or a non-sparse
`frailty()` term, `svysurvreg`, `mlm` (an
[`lm()`](https://rdrr.io/r/stats/lm.html) with a matrix response),
`aov`, `nlme`, `gnls`, the `lme` and `gam` parts of a
[`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html) fit, and the
[`rms::Rq()`](https://rdrr.io/pkg/rms/man/Rq.html) and
[`rms::Gls()`](https://rdrr.io/pkg/rms/man/Gls.html) wrappers.

## Usage

``` r
table_regression_models()
```

## Value

A data frame with one row per supported engine and columns `family`,
`class`, `engine`, `ame`, `exponentiate`, `blocks`, `vcov` (the values
of `vcov` the class accepts), `fit_stats_default` (the fit-statistic
rows of a one-model table under the default `show_fit_stats`, in their
order) and `fit_stats` (the tokens of `show_fit_stats` that give a row
for one model; the change tokens, which need `nested = TRUE`, are not
listed). A parenthesis in a cell gives the fits an item is limited to.

## Supported classes

|  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|
| Family | Class | Engine | AME | Exponentiate | Blocks | vcov |
| Linear and generalized linear | `lm` | [`stats::lm()`](https://rdrr.io/r/stats/lm.html) | yes | \- | \- | classical, HC0-HC5, CR0-CR3, CR1S, bootstrap, jackknife |
| Linear and generalized linear | `glm` | [`stats::glm()`](https://rdrr.io/r/stats/glm.html) | yes | OR / IRR / RR / MR / HR (link) | \- | classical, HC0-HC5, CR0-CR3, bootstrap, jackknife |
| Linear and generalized linear | `negbin` | [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) | yes | IRR | \- | classical, HC0-HC5, CR0-CR3, bootstrap, jackknife |
| Linear and generalized linear | `rlm` | [`MASS::rlm()`](https://rdrr.io/pkg/MASS/man/rlm.html) | yes | \- | \- | classical |
| Linear and generalized linear | `nls` | [`stats::nls()`](https://rdrr.io/r/stats/nls.html) | no | \- | \- | classical |
| Robust, IV, quantile, panel | `lm_robust` | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html) | yes | \- | \- | classical (the fit's own) |
| Robust, IV, quantile, panel | `iv_robust` | [`estimatr::iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html) | yes | \- | \- | classical (the fit's own) |
| Robust, IV, quantile, panel | `ivreg` | [`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html) | yes | \- | \- | classical, HC0, HC1, CR0-CR3, CR1S |
| Robust, IV, quantile, panel | `tobit` | [`AER::tobit()`](https://rdrr.io/pkg/AER/man/tobit.html) | yes (latent outcome) | \- | \- | classical, CR1 |
| Robust, IV, quantile, panel | `rq` | [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) | yes | \- | \- | classical (= nid), nid, iid, ker, rank, bootstrap |
| Robust, IV, quantile, panel | `fixest` | [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html), [`fixest::feglm()`](https://lrberge.github.io/fixest/reference/feglm.html), [`fixest::fepois()`](https://lrberge.github.io/fixest/reference/feglm.html), [`fixest::femlm()`](https://lrberge.github.io/fixest/reference/femlm.html), [`fixest::fenegbin()`](https://lrberge.github.io/fixest/reference/femlm.html) | yes (no for a GLM with fixed effects or an IV fit without exogenous regressor) | OR / IRR (link) | \- | classical (the fit's own) |
| Mixed effects | `lmerMod` | [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) | yes | \- | Random effects | classical, CR0-CR3 |
| Mixed effects | `glmerMod` | [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) | yes | OR / IRR (link) | Random effects | classical |
| Mixed effects | `glmmTMB` | [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) | yes (no for the ordinal family or a REML fit) | link-dependent (IRR for count families, OR for ordinal logit) | Random effects; Zero-inflation (ziformula); Zero hurdle, Pr(y = 0) for a zero-truncated family, ziGamma, lognormal or beta_family; Dispersion (dispformula); Precision (dispformula of beta_family); Thresholds (ordinal family) | classical, CR0, CR1 (classical only for REML or the ordinal family) |
| Mixed effects | `lme` | [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) | yes | \- | Random effects | classical, CR0-CR3 (classical only with contrasts other than the session's) |
| Mixed effects | `gls` | [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) | yes | \- | \- | classical |
| Ordinal | `polr` | [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) | per category | OR (logit) | Thresholds | classical, CR1 |
| Ordinal | `clm` | [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) | per category | OR (logit) | Thresholds; Non-proportional effects (nominal =); Scale effects (scale =) | classical, CR1 (classical only with scale or nominal terms) |
| Ordinal | `clmm` | [`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html) | no | OR (logit) | Thresholds; Random effects | classical |
| Categorical | `multinom` | [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) | per outcome | OR | per-outcome blocks | classical, CR1 |
| Categorical | `mlogit` | [`mlogit::mlogit()`](https://rdrr.io/pkg/mlogit/man/mlogit.html) | no | OR | per-alternative rows | classical, CR1 |
| Counts, two-part | `zeroinfl` | [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero-inflation | classical, CR1 (classical only for a negative-binomial part) |
| Counts, two-part | `hurdle` | [`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero hurdle | classical, CR1 (classical only for a negative-binomial part) |
| Survival | `coxph` | [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) | RMST / risk diff | HR | \- | classical, CR0 |
| Survival | `survreg` | [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html) | yes + RMST / risk diff | TR (log-scale distributions) | \- | classical, CR1 |
| Survival | `cph` | [`rms::cph()`](https://rdrr.io/pkg/rms/man/cph.html) | no | HR | \- | classical, CR0 (classical only without x = TRUE, y = TRUE) |
| Survival | `flexsurvreg` | [`flexsurv::flexsurvreg()`](https://chjackson.github.io/flexsurv/reference/flexsurvreg.html) | no | TR / HR (dist) | distribution parameters | classical |
| Survey-weighted | `svyglm` | [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) | yes (design-based) | OR / IRR | \- | classical (design-based) |
| Survey-weighted | `svyolr` | [`survey::svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) | per category (design-based) | OR (logit) | Thresholds | classical (design-based) |
| Survey-weighted | `svycoxph` | [`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html) | no | HR | \- | classical (design-based) |
| Population-averaged (GEE) | `geeglm` | [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html) | yes | OR / IRR / RR / MR / HR (link) | \- | classical (the fit's own) |
| Additive, proportions, selection | `gam` | [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html), [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) | yes | OR / IRR (link) | Smooth terms | classical, CR1 (classical only for an mgcv extended family other than nb) |
| Additive, proportions, selection | `betareg` | [`betareg::betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html) | yes | OR (mean link) | Precision (y ~ x | z\) |
| Additive, proportions, selection | `selection` | [`sampleSelection::selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html) | no | \- | Error terms | classical |
| rms | `ols` | [`rms::ols()`](https://rdrr.io/pkg/rms/man/ols.html) | yes | \- | \- | classical, CR0 (classical only without x = TRUE, y = TRUE) |
| rms | `lrm` | [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) | yes (per category for an ordinal outcome) | OR | Thresholds (ordinal outcome) | classical, CR0 (classical only without x = TRUE, y = TRUE) |
| rms | `Glm` | [`rms::Glm()`](https://rdrr.io/pkg/rms/man/Glm.html) | yes | link-dependent | \- | classical, CR0 (classical only without x = TRUE, y = TRUE) |
| Bayesian | `stanreg` | [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html), [`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html) | yes (draws) | link-dependent | Random effects (if multilevel) | classical (the posterior) |
| Bayesian | `brmsfit` | [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) | yes (draws) | link-dependent | Random effects (if multilevel) | classical (the posterior) |

AME: whether the average-marginal-effects columns are available, and
what they estimate (`per category` for ordinal models, `per outcome` for
multinomial models, `yes (draws)` for Bayesian fits). Exponentiate: what
`exponentiate = TRUE` gives. Blocks: the labeled blocks of rows the
class adds below its coefficients. vcov: the values of `vcov` the class
accepts; see *Robust SE availability by model class* in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

## Pages by model family

- [table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md):
  `lmer`, `glmer`, `glmmTMB`, `lme` and `gls` fits.

- [table_regression_survival](https://amaltawfik.github.io/spicy/reference/table_regression_survival.md):
  `coxph`, `survreg`, `cph` and `flexsurvreg` fits.

- [table_regression_ordinal](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md):
  `polr`, `clm` and `clmm` fits.

- [table_regression_categorical](https://amaltawfik.github.io/spicy/reference/table_regression_categorical.md):
  `multinom` and `mlogit` fits.

- [table_regression_counts](https://amaltawfik.github.io/spicy/reference/table_regression_counts.md):
  `zeroinfl` and `hurdle` fits, and the zero-inflation, hurdle and
  dispersion blocks of `glmmTMB`.

- [table_regression_survey](https://amaltawfik.github.io/spicy/reference/table_regression_survey.md):
  `svyglm`, `svyolr` and `svycoxph` fits.

- [table_regression_bayesian](https://amaltawfik.github.io/spicy/reference/table_regression_bayesian.md):
  `stanreg` and `brmsfit` fits.

- [table_regression_robust](https://amaltawfik.github.io/spicy/reference/table_regression_robust.md):
  `lm_robust`, `iv_robust`, `ivreg`, `tobit`, `rq` and `fixest` fits.

The linear and generalized linear models are documented on
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
itself. GEE fits are described below. The additive, beta-regression,
sample-selection and `rms` fits follow the rules of
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
and the table above. The tokens of `show_columns` and `show_fit_stats`,
and the fit statistics of each class, are on
[table_regression_tokens](https://amaltawfik.github.io/spicy/reference/table_regression_tokens.md).

A sample-selection fit
([`sampleSelection::selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html),
`heckit()`, `treatReg()`) alone shows its two equations as column
groups, `Outcome` then `Selection`; beside other models, or under
`nested = TRUE`, each variable has its `selection` and `outcome` rows. A
factor or a logical predictor has a header, its levels and a reference
row. The `Error terms` block follows: \\\sigma\\ and \\\rho\\ for a
maximum-likelihood fit (\\\rho\\ alone for a binary outcome), the
inverse Mills ratio \\\lambda\\ followed by \\\sigma\\ and \\\rho\\
without standard errors for a two-step fit. Tests and intervals use the
t distribution with the residual df that
[`summary()`](https://rdrr.io/r/base/summary.html) uses. The note names
the standard errors, "classical (Fisher information)" for a
maximum-likelihood fit and "classical (two-step)" for a two-step fit,
and the number of cases with an observed outcome. A tobit-5 (switching
regression) fit is refused.

## Shared semantics (all classes)

- A robust `vcov` request is honored through the class's backend, or
  refused with an error that names the supported set; the footer names
  the estimator applied.

- `exponentiate = TRUE` is link-gated: it produces a labeled ratio (OR /
  IRR / HR / RR / MR / TR) only where the link warrants one.
  Identity-link fits warn and are left untouched; non-ratio links
  (probit, cauchit, inverse, ...) are refused with an error.

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

## Population-averaged (GEE) models

[`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html) fits
are read on their own terms: the sandwich standard errors the fit
computed (its `std.err =` option, clustered on its `id =`) are the
displayed inference – GEE is robust by construction, so spicy's `HC*`
and `CR*` tokens, the resamplers and `cluster` are refused with a
pointer to the fit options. Coefficients are population-averaged
(marginal) effects; the footer discloses the working correlation
structure with its estimated alpha. Wald z inference; `exponentiate`
follows the usual link gates (OR / IRR / RR). Default fit statistics
report the cluster structure (n, `N (<id>)`, largest cluster); the
quasi-likelihood information criteria `"qic"` / `"qicu"` (Pan 2001) and
the `"scale"` (dispersion) parameter are opt-in – there is no
likelihood, so AIC, pseudo-R-squared, `nested = TRUE`, and
`standardized` are refused. See the [GEE regression
tables](https://amaltawfik.github.io/spicy/articles/table-regression-gee.html)
article, and the population-averaged section of the [Mixed-effects
regression
tables](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.html)
article for the contrast with subject-specific mixed models.

## References

Pan, W. (2001). Akaike's information criterion in generalized estimating
equations. *Biometrics*, 57(1), 120-125.
[doi:10.1111/j.0006-341X.2001.00120.x](https://doi.org/10.1111/j.0006-341X.2001.00120.x)

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
the family pages listed under *Pages by model family*;
[table_regression_tokens](https://amaltawfik.github.io/spicy/reference/table_regression_tokens.md)
for the tokens of `show_columns` and `show_fit_stats`; the [Supported
models](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.html)
and [Publication-ready regression
tables](https://amaltawfik.github.io/spicy/articles/table-regression.html)
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
#> 17                          Ordinal        polr
#> 18                          Ordinal         clm
#> 19                          Ordinal        clmm
#> 20                      Categorical    multinom
#> 21                      Categorical      mlogit
#> 22                 Counts, two-part    zeroinfl
#> 23                 Counts, two-part      hurdle
#> 24                         Survival       coxph
#> 25                         Survival     survreg
#> 26                         Survival         cph
#> 27                         Survival flexsurvreg
#> 28                  Survey-weighted      svyglm
#> 29                  Survey-weighted      svyolr
#> 30                  Survey-weighted    svycoxph
#> 31        Population-averaged (GEE)      geeglm
#> 32 Additive, proportions, selection         gam
#> 33 Additive, proportions, selection     betareg
#> 34 Additive, proportions, selection   selection
#> 35                              rms         ols
#> 36                              rms         lrm
#> 37                              rms         Glm
#> 38                         Bayesian     stanreg
#> 39                         Bayesian     brmsfit
#>                                                                                     engine
#> 1                                                                              stats::lm()
#> 2                                                                             stats::glm()
#> 3                                                                           MASS::glm.nb()
#> 4                                                                              MASS::rlm()
#> 5                                                                             stats::nls()
#> 6                                                                    estimatr::lm_robust()
#> 7                                                                    estimatr::iv_robust()
#> 8                                                                             AER::ivreg()
#> 9                                                                             AER::tobit()
#> 10                                                                          quantreg::rq()
#> 11 fixest::feols(), fixest::feglm(), fixest::fepois(), fixest::femlm(), fixest::fenegbin()
#> 12                                                                            lme4::lmer()
#> 13                                                                           lme4::glmer()
#> 14                                                                      glmmTMB::glmmTMB()
#> 15                                                                             nlme::lme()
#> 16                                                                             nlme::gls()
#> 17                                                                            MASS::polr()
#> 18                                                                          ordinal::clm()
#> 19                                                                         ordinal::clmm()
#> 20                                                                        nnet::multinom()
#> 21                                                                        mlogit::mlogit()
#> 22                                                                        pscl::zeroinfl()
#> 23                                                                          pscl::hurdle()
#> 24                                                                       survival::coxph()
#> 25                                                                     survival::survreg()
#> 26                                                                              rms::cph()
#> 27                                                                 flexsurv::flexsurvreg()
#> 28                                                                        survey::svyglm()
#> 29                                                                        survey::svyolr()
#> 30                                                                      survey::svycoxph()
#> 31                                                                       geepack::geeglm()
#> 32                                                                mgcv::gam(), mgcv::bam()
#> 33                                                                      betareg::betareg()
#> 34                                                            sampleSelection::selection()
#> 35                                                                              rms::ols()
#> 36                                                                              rms::lrm()
#> 37                                                                              rms::Glm()
#> 38                                            rstanarm::stan_glm(), rstanarm::stan_glmer()
#> 39                                                                             brms::brm()
#>                                                                               ame
#> 1                                                                             yes
#> 2                                                                             yes
#> 3                                                                             yes
#> 4                                                                             yes
#> 5                                                                              no
#> 6                                                                             yes
#> 7                                                                             yes
#> 8                                                                             yes
#> 9                                                            yes (latent outcome)
#> 10                                                                            yes
#> 11 yes (no for a GLM with fixed effects or an IV fit without exogenous regressor)
#> 12                                                                            yes
#> 13                                                                            yes
#> 14                                  yes (no for the ordinal family or a REML fit)
#> 15                                                                            yes
#> 16                                                                            yes
#> 17                                                                   per category
#> 18                                                                   per category
#> 19                                                                             no
#> 20                                                                    per outcome
#> 21                                                                             no
#> 22                                                        yes (combined response)
#> 23                                                        yes (combined response)
#> 24                                                               RMST / risk diff
#> 25                                                         yes + RMST / risk diff
#> 26                                                                             no
#> 27                                                                             no
#> 28                                                             yes (design-based)
#> 29                                                    per category (design-based)
#> 30                                                                             no
#> 31                                                                            yes
#> 32                                                                            yes
#> 33                                                                            yes
#> 34                                                                             no
#> 35                                                                            yes
#> 36                                      yes (per category for an ordinal outcome)
#> 37                                                                            yes
#> 38                                                                    yes (draws)
#> 39                                                                    yes (draws)
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
#> 11                                               OR / IRR (link)
#> 12                                                             -
#> 13                                               OR / IRR (link)
#> 14 link-dependent (IRR for count families, OR for ordinal logit)
#> 15                                                             -
#> 16                                                             -
#> 17                                                    OR (logit)
#> 18                                                    OR (logit)
#> 19                                                    OR (logit)
#> 20                                                            OR
#> 21                                                            OR
#> 22                            IRR (count) + OR (logit zero part)
#> 23                            IRR (count) + OR (logit zero part)
#> 24                                                            HR
#> 25                                  TR (log-scale distributions)
#> 26                                                            HR
#> 27                                                TR / HR (dist)
#> 28                                                      OR / IRR
#> 29                                                    OR (logit)
#> 30                                                            HR
#> 31                                OR / IRR / RR / MR / HR (link)
#> 32                                               OR / IRR (link)
#> 33                                                OR (mean link)
#> 34                                                             -
#> 35                                                             -
#> 36                                                            OR
#> 37                                                link-dependent
#> 38                                                link-dependent
#> 39                                                link-dependent
#>                                                                                                                                                                                                                              blocks
#> 1                                                                                                                                                                                                                                 -
#> 2                                                                                                                                                                                                                                 -
#> 3                                                                                                                                                                                                                                 -
#> 4                                                                                                                                                                                                                                 -
#> 5                                                                                                                                                                                                                                 -
#> 6                                                                                                                                                                                                                                 -
#> 7                                                                                                                                                                                                                                 -
#> 8                                                                                                                                                                                                                                 -
#> 9                                                                                                                                                                                                                                 -
#> 10                                                                                                                                                                                                                                -
#> 11                                                                                                                                                                                                                                -
#> 12                                                                                                                                                                                                                   Random effects
#> 13                                                                                                                                                                                                                   Random effects
#> 14 Random effects; Zero-inflation (ziformula); Zero hurdle, Pr(y = 0) for a zero-truncated family, ziGamma, lognormal or beta_family; Dispersion (dispformula); Precision (dispformula of beta_family); Thresholds (ordinal family)
#> 15                                                                                                                                                                                                                   Random effects
#> 16                                                                                                                                                                                                                                -
#> 17                                                                                                                                                                                                                       Thresholds
#> 18                                                                                                                                                        Thresholds; Non-proportional effects (nominal =); Scale effects (scale =)
#> 19                                                                                                                                                                                                       Thresholds; Random effects
#> 20                                                                                                                                                                                                               per-outcome blocks
#> 21                                                                                                                                                                                                             per-alternative rows
#> 22                                                                                                                                                                                                                   Zero-inflation
#> 23                                                                                                                                                                                                                      Zero hurdle
#> 24                                                                                                                                                                                                                                -
#> 25                                                                                                                                                                                                                                -
#> 26                                                                                                                                                                                                                                -
#> 27                                                                                                                                                                                                          distribution parameters
#> 28                                                                                                                                                                                                                                -
#> 29                                                                                                                                                                                                                       Thresholds
#> 30                                                                                                                                                                                                                                -
#> 31                                                                                                                                                                                                                                -
#> 32                                                                                                                                                                                                                     Smooth terms
#> 33                                                                                                                                                                                                            Precision (y ~ x | z)
#> 34                                                                                                                                                                                                                      Error terms
#> 35                                                                                                                                                                                                                                -
#> 36                                                                                                                                                                                                     Thresholds (ordinal outcome)
#> 37                                                                                                                                                                                                                                -
#> 38                                                                                                                                                                                                   Random effects (if multilevel)
#> 39                                                                                                                                                                                                   Random effects (if multilevel)
#>                                                                           vcov
#> 1                      classical, HC0-HC5, CR0-CR3, CR1S, bootstrap, jackknife
#> 2                            classical, HC0-HC5, CR0-CR3, bootstrap, jackknife
#> 3                            classical, HC0-HC5, CR0-CR3, bootstrap, jackknife
#> 4                                                                    classical
#> 5                                                                    classical
#> 6                                                    classical (the fit's own)
#> 7                                                    classical (the fit's own)
#> 8                                           classical, HC0, HC1, CR0-CR3, CR1S
#> 9                                                               classical, CR1
#> 10                           classical (= nid), nid, iid, ker, rank, bootstrap
#> 11                                                   classical (the fit's own)
#> 12                                                          classical, CR0-CR3
#> 13                                                                   classical
#> 14         classical, CR0, CR1 (classical only for REML or the ordinal family)
#> 15 classical, CR0-CR3 (classical only with contrasts other than the session's)
#> 16                                                                   classical
#> 17                                                              classical, CR1
#> 18                 classical, CR1 (classical only with scale or nominal terms)
#> 19                                                                   classical
#> 20                                                              classical, CR1
#> 21                                                              classical, CR1
#> 22                classical, CR1 (classical only for a negative-binomial part)
#> 23                classical, CR1 (classical only for a negative-binomial part)
#> 24                                                              classical, CR0
#> 25                                                              classical, CR1
#> 26                  classical, CR0 (classical only without x = TRUE, y = TRUE)
#> 27                                                                   classical
#> 28                                                    classical (design-based)
#> 29                                                    classical (design-based)
#> 30                                                    classical (design-based)
#> 31                                                   classical (the fit's own)
#> 32   classical, CR1 (classical only for an mgcv extended family other than nb)
#> 33                                                              classical, CR1
#> 34                                                                   classical
#> 35                  classical, CR0 (classical only without x = TRUE, y = TRUE)
#> 36                  classical, CR0 (classical only without x = TRUE, y = TRUE)
#> 37                  classical, CR0 (classical only without x = TRUE, y = TRUE)
#> 38                                                   classical (the posterior)
#> 39                                                   classical (the posterior)
#>                                                                                                                                                                                                                              fit_stats_default
#> 1                                                                                                                                                                                                                             nobs, r2, adj_r2
#> 2                                                                                                nobs, pseudo_r2_tjur (binomial logit, binary outcome), pseudo_r2_mcfadden (binomial, Poisson), r2_deviance (Gaussian, Gamma), aic (not quasi)
#> 3                                                                                                                                                                                                                nobs, pseudo_r2_mcfadden, aic
#> 4                                                                                                                                                                                                                                         nobs
#> 5                                                                                                                                                                                                                                    nobs, aic
#> 6                                                                                                                                                                                       fixed_effects (with fixed_effects =), nobs, r2, adj_r2
#> 7                                                                                                                                                                                                                                         nobs
#> 8                                                                                                                                                                                                                                         nobs
#> 9                                                                                                                                                                                                                                    nobs, aic
#> 10                                                                                                                                                                                                                                   nobs, aic
#> 11                                                                                   fixed_effects (with fixed effects), nobs, r2 (feols, not IV), within_r2 (feols with fixed effects, not IV), pseudo_r2_mcfadden (GLM family), aic (not IV)
#> 12                                                                                                                                                                                  nobs, n_groups, icc, r2_marginal, r2_conditional, aic, bic
#> 13                                                                                                                                                     nobs, n_groups, icc (binomial or Poisson family), r2_marginal, r2_conditional, aic, bic
#> 14 nobs, n_groups, icc (Gaussian, no dispersion formula), r2_marginal (no ordinal or beta family, zero-inflation or dispersion formula), r2_conditional (no ordinal, beta or truncated family, zero-inflation or dispersion formula), aic, bic
#> 15                                                                                                                                                                                  nobs, n_groups, icc, r2_marginal, r2_conditional, aic, bic
#> 16                                                                                                                                                                                                                                   nobs, aic
#> 17                                                                                                                                                                                                               nobs, pseudo_r2_mcfadden, aic
#> 18                                                                                                                                                                                                               nobs, pseudo_r2_mcfadden, aic
#> 19                                                                                                                                                                                                                    nobs, n_groups, aic, bic
#> 20                                                                                                                                                                                                               nobs, pseudo_r2_mcfadden, aic
#> 21                                                                                                                                                                                                                                   nobs, aic
#> 22                                                                                                                                                                                                                                   nobs, aic
#> 23                                                                                                                                                                                                                                   nobs, aic
#> 24                                                                                                                                                                                                                         nobs, n_events, aic
#> 25                                                                                                                                                                                                                                   nobs, aic
#> 26                                                                                                                                                                                                                         nobs, n_events, aic
#> 27                                                                                                                                                                                                                                   nobs, aic
#> 28                                                                                                                                                                                                                    nobs, weighted_nobs, aic
#> 29                                                                                                                                                                                                                         nobs, weighted_nobs
#> 30                                                                                                                                                                                                               nobs, weighted_nobs, n_events
#> 31                                                                                                                                                                                                            nobs, n_groups, max_cluster_size
#> 32                                                                   nobs, pseudo_r2_tjur (binomial logit, binary outcome), pseudo_r2_mcfadden (binomial, Poisson, negative binomial), r2_deviance (Gaussian, Gamma, Tweedie), aic (not quasi)
#> 33                                                                                                                                                                                                                                   nobs, aic
#> 34                                                                                                                                                                                                                                   nobs, aic
#> 35                                                                                                                                                                                                                                    nobs, r2
#> 36                                                                                                                                                                              nobs, pseudo_r2_tjur (binary outcome), pseudo_r2_mcfadden, aic
#> 37                                                                                                           nobs, pseudo_r2_tjur (binomial logit, binary outcome), pseudo_r2_mcfadden (binomial, Poisson), r2_deviance (Gaussian, Gamma), aic
#> 38                                                                                                                                                                                                                              nobs, r2_bayes
#> 39                                                                                                                                                                                                                              nobs, r2_bayes
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      fit_stats
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                   nobs, weighted_nobs (weighted fit), r2, adj_r2, omega2, sigma, rmse, f2, aic, aicc, bic, deviance, log_lik
#> 2                                                                                                                                                                                                                                                                                                                                nobs, pseudo_r2_mcfadden (binomial, Poisson), pseudo_r2_nagelkerke (binomial, Poisson), pseudo_r2_tjur (binomial), r2_deviance (not binomial), sigma, rmse, aic (not quasi), aicc (not quasi), bic (not quasi), deviance, log_lik (not quasi)
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                           nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke, r2_deviance, theta, alpha, rmse, aic, aicc, bic, deviance, log_lik
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         nobs, sigma, aic, aicc, bic, log_lik
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               nobs, sigma, aic, aicc, bic, deviance, log_lik
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                    nobs, r2, adj_r2, fixed_effects (with fixed_effects =), within_r2 (with fixed_effects =), n_groups (with fixed_effects =)
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             nobs, r2, adj_r2
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             nobs, r2, adj_r2
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         nobs, sigma, aic, aicc, bic, log_lik
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    nobs, aic, aicc, log_lik
#> 11                                                                                                                                                                                                                                                                          nobs, r2 (feols), adj_r2 (feols), pseudo_r2_mcfadden (GLM family), theta (negative binomial), alpha (negative binomial), fixed_effects (with fixed effects), within_r2 (feols with fixed effects), n_groups (with fixed effects), sigma (not negative binomial), aic, aicc, bic, deviance, log_lik
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  nobs, r2_marginal, r2_conditional, n_groups, icc, sigma, aic, aicc, bic, deviance, log_lik
#> 13                                                                                                                                                                                                                                                                                                                                                                       nobs, theta (negative binomial), alpha (negative binomial), r2_marginal, r2_conditional, n_groups, icc (binomial or Poisson family), sigma (not negative binomial), aic, aicc, bic, deviance, log_lik
#> 14 nobs, theta (NB2 family, no dispersion formula), alpha (NB2 family, no dispersion formula), delta (NB1 family, no dispersion formula), phi (beta family, no dispersion formula), r2_marginal (no ordinal or beta family, zero-inflation or dispersion formula), r2_conditional (no ordinal, beta or truncated family, zero-inflation or dispersion formula), n_groups, icc (Gaussian, no dispersion formula), sigma (Gaussian, Gamma, Poisson, binomial or Tweedie family, no dispersion formula), aic, aicc, bic, deviance (no ordinal, truncated or beta family), log_lik
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            nobs, r2_marginal, r2_conditional, n_groups, icc, sigma, aic, aicc, bic, log_lik
#> 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        nobs, sigma, aic, aicc, bic, log_lik
#> 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke, aic, aicc, bic, deviance, log_lik
#> 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke, aic, aicc, bic, log_lik
#> 19                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           nobs, n_groups, aic, aicc, bic, deviance, log_lik
#> 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke, aic, aicc, bic, deviance, log_lik
#> 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               nobs, aic, aicc, bic, log_lik
#> 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         nobs, theta (negative binomial), alpha (negative binomial), aic, aicc, bic, log_lik
#> 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         nobs, theta (negative binomial), alpha (negative binomial), aic, aicc, bic, log_lik
#> 24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        nobs, n_events, aic, aicc, bic, log_lik, concordance
#> 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            nobs, sigma (no strata), aic, aicc, bic, log_lik
#> 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     nobs, n_events, aic, aicc, bic, log_lik
#> 27                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               nobs, aic, aicc, bic, log_lik
#> 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             nobs, weighted_nobs, aic, eff_p
#> 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         nobs, weighted_nobs
#> 30                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  nobs, weighted_nobs, n_events, concordance
#> 31                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          nobs, qic, qicu, scale, max_cluster_size, n_groups
#> 32                                                                                                                                                                                                                                                nobs, adj_r2, pseudo_r2_mcfadden (binomial, Poisson, negative binomial), pseudo_r2_nagelkerke (binomial, Poisson, negative binomial), pseudo_r2_tjur (binomial), r2_deviance (nb, tw or a glm family but binomial), theta (nb()), alpha (nb()), phi (betar()), sigma (glm family or tw()), aic, aicc, bic, deviance, log_lik
#> 33                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       nobs, pseudo_r2_ferrari, phi, aic, aicc, bic, log_lik
#> 34                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               nobs, aic, aicc, bic, log_lik
#> 35                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          nobs, r2, sigma, aic, aicc, bic, deviance, log_lik
#> 36                                                                                                                                                                                                                                                                                                                                                                                                                                                          nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke, pseudo_r2_tjur (binary outcome), aic, aicc, bic, deviance, log_lik
#> 37                                                                                                                                                                                                                                                                                                                                                                                            nobs, pseudo_r2_mcfadden (binomial, Poisson), pseudo_r2_nagelkerke (binomial, Poisson), pseudo_r2_tjur (binomial), r2_deviance (not binomial), aic, aicc, bic, deviance, log_lik
#> 38                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       nobs, r2_bayes, elpd_loo, looic, waic
#> 39                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       nobs, r2_bayes, elpd_loo, looic, waic

# All engines of one family:
subset(table_regression_models(), family == "Mixed effects")
#>           family    class             engine
#> 12 Mixed effects  lmerMod       lme4::lmer()
#> 13 Mixed effects glmerMod      lme4::glmer()
#> 14 Mixed effects  glmmTMB glmmTMB::glmmTMB()
#> 15 Mixed effects      lme        nlme::lme()
#> 16 Mixed effects      gls        nlme::gls()
#>                                              ame
#> 12                                           yes
#> 13                                           yes
#> 14 yes (no for the ordinal family or a REML fit)
#> 15                                           yes
#> 16                                           yes
#>                                                     exponentiate
#> 12                                                             -
#> 13                                               OR / IRR (link)
#> 14 link-dependent (IRR for count families, OR for ordinal logit)
#> 15                                                             -
#> 16                                                             -
#>                                                                                                                                                                                                                              blocks
#> 12                                                                                                                                                                                                                   Random effects
#> 13                                                                                                                                                                                                                   Random effects
#> 14 Random effects; Zero-inflation (ziformula); Zero hurdle, Pr(y = 0) for a zero-truncated family, ziGamma, lognormal or beta_family; Dispersion (dispformula); Precision (dispformula of beta_family); Thresholds (ordinal family)
#> 15                                                                                                                                                                                                                   Random effects
#> 16                                                                                                                                                                                                                                -
#>                                                                           vcov
#> 12                                                          classical, CR0-CR3
#> 13                                                                   classical
#> 14         classical, CR0, CR1 (classical only for REML or the ordinal family)
#> 15 classical, CR0-CR3 (classical only with contrasts other than the session's)
#> 16                                                                   classical
#>                                                                                                                                                                                                                              fit_stats_default
#> 12                                                                                                                                                                                  nobs, n_groups, icc, r2_marginal, r2_conditional, aic, bic
#> 13                                                                                                                                                     nobs, n_groups, icc (binomial or Poisson family), r2_marginal, r2_conditional, aic, bic
#> 14 nobs, n_groups, icc (Gaussian, no dispersion formula), r2_marginal (no ordinal or beta family, zero-inflation or dispersion formula), r2_conditional (no ordinal, beta or truncated family, zero-inflation or dispersion formula), aic, bic
#> 15                                                                                                                                                                                  nobs, n_groups, icc, r2_marginal, r2_conditional, aic, bic
#> 16                                                                                                                                                                                                                                   nobs, aic
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      fit_stats
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  nobs, r2_marginal, r2_conditional, n_groups, icc, sigma, aic, aicc, bic, deviance, log_lik
#> 13                                                                                                                                                                                                                                                                                                                                                                       nobs, theta (negative binomial), alpha (negative binomial), r2_marginal, r2_conditional, n_groups, icc (binomial or Poisson family), sigma (not negative binomial), aic, aicc, bic, deviance, log_lik
#> 14 nobs, theta (NB2 family, no dispersion formula), alpha (NB2 family, no dispersion formula), delta (NB1 family, no dispersion formula), phi (beta family, no dispersion formula), r2_marginal (no ordinal or beta family, zero-inflation or dispersion formula), r2_conditional (no ordinal, beta or truncated family, zero-inflation or dispersion formula), n_groups, icc (Gaussian, no dispersion formula), sigma (Gaussian, Gamma, Poisson, binomial or Tweedie family, no dispersion formula), aic, aicc, bic, deviance (no ordinal, truncated or beta family), log_lik
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            nobs, r2_marginal, r2_conditional, n_groups, icc, sigma, aic, aicc, bic, log_lik
#> 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        nobs, sigma, aic, aicc, bic, log_lik
```
