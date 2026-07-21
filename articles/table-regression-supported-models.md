# Supported models

``` r

library(spicy)
```

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
takes fitted model objects – never raw data plus a formula – and renders
them as publication-ready coefficient tables. “Supported” is a
commitment, not a list of classes that happen to run: every number a
supported class produces is validated against a field reference (the
model’s own [`summary()`](https://rdrr.io/r/base/summary.html),
`sandwich`, `clubSandwich`, `marginaleffects`, `effectsize`,
`performance`, Stata or SPSS conventions), and every request a class
cannot honour is **refused with a classed error** that names what is
available – never rendered as a silently empty or approximate column.

This article is the map. Each family below links to a dedicated article
that walks through its behaviour in depth.

## The registry

The table is generated from the same internal registry that the package
itself uses, so it cannot drift from the code. Call
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
to get it as a data frame.

| Family | Class | Engine | AME | Exponentiate | Blocks |
|:---|:---|:---|:---|:---|:---|
| Linear and generalized linear | `lm` | [`stats::lm()`](https://rdrr.io/r/stats/lm.html) | yes | \- | \- |
|  | `glm` | [`stats::glm()`](https://rdrr.io/r/stats/glm.html) | yes | OR / IRR / RR (link) | \- |
|  | `negbin` | [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) | yes | IRR | \- |
|  | `rlm` | [`MASS::rlm()`](https://rdrr.io/pkg/MASS/man/rlm.html) | yes | \- | \- |
|  | `nls` | [`stats::nls()`](https://rdrr.io/r/stats/nls.html) | no | \- | \- |
| Robust, IV, quantile, panel | `lm_robust` | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html) | yes | \- | \- |
|  | `iv_robust` | [`estimatr::iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html) | yes | \- | \- |
|  | `ivreg` | [`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html) | yes | \- | \- |
|  | `tobit` | [`AER::tobit()`](https://rdrr.io/pkg/AER/man/tobit.html) | yes | \- | \- |
|  | `rq` | [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) | yes | \- | \- |
|  | `fixest` | [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html), [`fixest::feglm()`](https://lrberge.github.io/fixest/reference/feglm.html), [`fixest::fepois()`](https://lrberge.github.io/fixest/reference/feglm.html), [`fixest::fenegbin()`](https://lrberge.github.io/fixest/reference/femlm.html) | yes | `feglm`: OR / IRR | \- |
| Mixed effects | `lmerMod` | [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) | yes | \- | Random effects |
|  | `glmerMod` | [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) | yes | OR / IRR (link) | Random effects |
|  | `glmmTMB` | [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) | yes | link-dependent (IRR for count families) | Random effects; Zero-inflation; Dispersion |
|  | `lme` | [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) | yes | \- | Random effects |
|  | `gls` | [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) | yes | \- | \- |
| Ordinal | `polr` | [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) | per category | OR (logit) | Thresholds |
|  | `clm` | [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) | per category | OR (logit) | Thresholds; Non-proportional effects |
| Categorical | `multinom` | [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) | per outcome | OR | per-outcome blocks |
|  | `mlogit` | [`mlogit::mlogit()`](https://rdrr.io/pkg/mlogit/man/mlogit.html) | no | OR | per-alternative rows |
| Counts, two-part | `zeroinfl` | [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero-inflation |
|  | `hurdle` | [`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero hurdle |
| Survival | `coxph` | [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) | RMST / risk diff | HR | \- |
|  | `survreg` | [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html) | yes + RMST / risk diff | TR (log-scale distributions) | \- |
|  | `cph` | [`rms::cph()`](https://rdrr.io/pkg/rms/man/cph.html) | no | HR | \- |
|  | `flexsurvreg` | [`flexsurv::flexsurvreg()`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md) | no | TR / HR (dist) | distribution parameters |
| Survey-weighted | `svyglm` | [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) | yes (design-based) | OR / IRR | \- |
| Additive, proportions, selection | `gam` | [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html), [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) | yes | OR / IRR (link) | \- |
|  | `betareg` | [`betareg::betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html) | yes | OR (mean link) | \- |
|  | `selection` | [`sampleSelection::selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html) | no | \- | selection component |
| rms | `ols` | [`rms::ols()`](https://rdrr.io/pkg/rms/man/ols.html) | yes | \- | \- |
|  | `lrm` | [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) | yes | OR | \- |
|  | `Glm` | [`rms::Glm()`](https://rdrr.io/pkg/rms/man/Glm.html) | yes | link-dependent | \- |
| Bayesian | `stanreg` | [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html), [`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html) | yes (draws) | link-dependent | Random effects (if multilevel) |
|  | `brmsfit` | [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) | yes (draws) | link-dependent | Random effects (if multilevel) |

How to read the columns:

- **AME** – what `show_columns = c("b", "ame")` adds. `yes` is an
  average marginal effect on the response scale (a probability or rate
  effect for GLM families, the slope itself under identity).
  `per category` (ordinal) is the effect on each P(Y = k); `per outcome`
  (multinomial) is one effect per non-reference outcome.
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html)
  refuses AME – the hazard scale has no marginal-probability effect –
  and provides covariate-adjusted `rmst` and `risk_diff` columns instead
  (also available for `survreg`;
  [`rms::cph`](https://rdrr.io/pkg/rms/man/cph.html) and `flexsurvreg`
  support neither AME nor the estimand columns). `yes (draws)` means the
  effect is computed per posterior draw and summarized as a posterior
  median with MAD SD and credible interval.
- **Exponentiate** – the labelled ratio `exponentiate = TRUE` produces.
  The label follows the link: OR under logit, IRR for count log-links,
  HR for proportional hazards, TR (time ratio) for
  accelerated-failure-time models. Identity-link fits warn and stay
  untouched; links whose exponential is not a ratio (probit, cauchit,
  inverse) are refused.
- **Blocks** – labelled subordinate row blocks rendered inside the same
  table (random effects, thresholds, zero components, per-outcome
  segments), each explained by a footer line.

## Cross-cutting arguments

The same arguments work across families, each through the family’s
field-standard backend – or a clear refusal.

**Robust and cluster-robust standard errors** (`vcov`, `cluster`). `lm`
and `glm` take `HC0`–`HC5` (`sandwich`) and cluster-robust `CR0`–`CR3`
(`clubSandwich`, bias-reduced with Satterthwaite df), plus `"bootstrap"`
/ `"jackknife"` resampling estimators. Among the mixed engines, `lmer`,
[`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) and `glmmTMB` take
`CR*` via `clubSandwich` (for `glmmTMB` the sandwich covers the
conditional part only, and the footer says so); `glmer` and `gls` are
model-based only – `clubSandwich` has no working backend for them.
Ordinal models take `CR0`–`CR3` but no `HC*`, and the cut-point
thresholds are reweighted from the same clustered vcov (a `clm` with a
scale or nominal partial-proportional-odds component is model-based
only). `multinom` takes `CR*` (one cluster value per observation) and
`mlogit` takes `CR*` (one per choice situation) – both refuse `HC*`,
which has no valid working-residual form for multi-equation models.
Quantile regression (`rq`) uses its own estimator family – `"classical"`
resolves to the robust `nid` sandwich, `iid` / `ker` / `rank` are
opt-ins, and clustering goes through its native wild gradient bootstrap
(`vcov = "bootstrap"` + `cluster`; `HC*` / `CR*` are refused). Cox
models use the Lin-Wei grouped-dfbeta sandwich, and the `rms` fits take
`CR*` via [`rms::robcov()`](https://rdrr.io/pkg/rms/man/robcov.html)
(refit with `x = TRUE, y = TRUE`); `survreg`, `gam` / `bam` and
`betareg` take `CR*` via
[`sandwich::vcovCL()`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html);
`pscl` two-part fits cluster both components. `estimatr` fits keep their
own robust SEs; `svyglm` is design-based by default and additionally
accepts design-aware `CR0`–`CR3`. Bayesian fits refuse `vcov` – nothing
standard plays the sandwich role for a posterior. Whatever the backend,
the footer names the estimator actually applied, and a robust vcov also
flows into the AME uncertainty.

**Standardized coefficients** (`standardized`). Available for `lm`,
`glm` (including
[`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html)), the mixed
engines (`lmer` / `glmer` / `glmmTMB` /
[`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html)), and fixed-effects
`stan_glm`-style Bayesian fits, where `"posthoc"`, `"basic"` and
`"smart"` are exact affine rescales of the posterior draws. Other
classes – including multilevel Bayesian fits and `brmsfit` – refuse with
a hint to standardize predictors before fitting.

**Confidence intervals** (`ci_method`). Wald everywhere by default;
`"profile"` (profile likelihood) for `glm`, `polr` and `clm`;
`"boot_percentile"` (with `vcov = "bootstrap"`) replaces the bounds with
equal-tailed percentile intervals of the bootstrap replicates; `"hdi"`
(highest-density interval) for Bayesian fits, which otherwise report
equal-tailed credible intervals.

**Model comparison and multiplicity.** `nested = TRUE` compares nested
fits by the family’s change-test convention: Delta R-squared with the
partial F test for `lm`, the likelihood-ratio test for `glm`, mixed,
`multinom` and Cox models, and `anova.rq`’s Wald-type F for quantile
regressions (all fits at one tau). `p_adjust` applies a multiplicity
correction across the displayed p-values – and is refused for Bayesian
tables, which carry no p-values at all.

## The families in brief

**Linear and generalized linear.** The core engines: `lm`, `glm` (with
profile CIs on request),
[`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html) (with opt-in
`theta` / `alpha` dispersion statistics),
[`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html),
[`stats::nls`](https://rdrr.io/r/stats/nls.html). Start with
[`vignette("table-regression")`](https://amaltawfik.github.io/spicy/articles/table-regression.md)
and
[`vignette("categorical-predictors")`](https://amaltawfik.github.io/spicy/articles/categorical-predictors.md).

**Robust, IV, quantile, panel.**
[`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html)
/ `iv_robust()`,
[`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html) and
[`AER::tobit()`](https://rdrr.io/pkg/AER/man/tobit.html),
[`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) (defaulting
to the heteroskedasticity-robust `nid` sandwich – quantreg’s own
large-sample default – with `iid`, `ker`, `rank` CIs and a native
clustered bootstrap as `vcov` options), and the `fixest` estimators,
whose absorbed fixed effects render as a default-on `Fixed effects:`
block – one Yes / No row per factor, blank for non-fixest models in a
mixed table – with the within R-squared among the default fit
statistics.

**Mixed effects.** `lmer` (Satterthwaite t via `lmerTest`), `glmer`,
`glmmTMB` (with zero-inflation and dispersion blocks),
[`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) and
[`nlme::gls`](https://rdrr.io/pkg/nlme/man/gls.html). Random effects
render as rows – SD, correlations, residual – deliberately without
per-row p-values; the footer carries the boundary-correct
chi-bar-squared test, and `re_test = "lrt"` / `"rlrt"` adds per-term
tests. See
[`vignette("table-regression-mixed")`](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md).

**Ordinal.** [`MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html) and
[`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html): proportional
odds ratios, a `Thresholds` block for the cut-points (log-odds scale,
never exponentiated), partial-proportional-odds terms as a
`Non-proportional effects` block, and per-category AME. See
[`vignette("table-regression-ordinal")`](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.md).

**Categorical.**
[`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html) renders
outcome categories as columns with per-outcome AME;
[`mlogit::mlogit`](https://rdrr.io/pkg/mlogit/man/mlogit.html) renders
per-alternative rows for discrete-choice designs. See
[`vignette("table-regression-multinomial")`](https://amaltawfik.github.io/spicy/articles/table-regression-multinomial.md).

**Counts and two-part.** Poisson and negative binomial through `glm` /
`glm.nb` / `glmmTMB`, plus
[`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) and
[`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) with their
zero components as labelled blocks and a combined-response AME. See
[`vignette("table-regression-counts")`](https://amaltawfik.github.io/spicy/articles/table-regression-counts.md).

**Survival.**
[`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) and
[`rms::cph`](https://rdrr.io/pkg/rms/man/cph.html) (hazard ratios,
`strata()` supported),
[`survival::survreg`](https://rdrr.io/pkg/survival/man/survreg.html)
(time ratios) and
[`flexsurv::flexsurvreg`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md).
Absolute effects come as covariate-adjusted RMST and risk differences by
g-computation for `coxph` and `survreg` fits. See
[`vignette("table-regression-survival")`](https://amaltawfik.github.io/spicy/articles/table-regression-survival.md).

**Survey-weighted.**
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html):
design-based inference, weighted and unweighted n.

**Additive, proportions, selection.**
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) / `bam()`,
[`betareg::betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html)
(odds ratios on the logit mean link under `exponentiate`; the precision
is the opt-in `phi` fit statistic), and
[`sampleSelection::selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html)
with its selection component as a block.

**rms.** `ols`, `lrm`, `Glm` and `cph` are first-class citizens, so
Harrell-style workflows drop in directly.

**Bayesian.** `rstanarm` and `brms` fits are summarized from their
posterior draws: posterior median, MAD SD, credible intervals,
draws-native exponentiation and AME, sampler diagnostics checked on
every fit. No p-values, by design. See
[`vignette("table-regression-bayesian")`](https://amaltawfik.github.io/spicy/articles/table-regression-bayesian.md).

## When a class is not supported

An unsupported class fails fast with a classed error
(`spicy_unsupported`):

``` r

fit <- loess(dist ~ speed, data = cars)
table_regression(fit)
#> Error in `validate_models_input()`:
#> ! Some `models` are not supported by `table_regression()`.
#> Position 1: `loess` – no `as_regression_frame()` method registered. If support would be useful, please open an issue: https://github.com/amaltawfik/spicy/issues
#> ℹ Run `methods('as_regression_frame')` to see all currently supported model classes.
```

The same contract applies inside a family: a request a class cannot
honour – `HC*` for `multinom`, AME for `mlogit`, `exponentiate` on a
probit link, `p_adjust` on a Bayesian table – is refused with the reason
and the supported alternative, never silently degraded.

## Programmatic access

[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
returns the registry as a plain data frame – convenient to filter, join,
or cite:

``` r

subset(table_regression_models(), family == "Survival")
#>      family       class                  engine                    ame                 exponentiate
#> 23 Survival       coxph       survival::coxph()       RMST / risk diff                           HR
#> 24 Survival     survreg     survival::survreg() yes + RMST / risk diff TR (log-scale distributions)
#> 25 Survival         cph              rms::cph()                     no                           HR
#> 26 Survival flexsurvreg flexsurv::flexsurvreg()                     no               TR / HR (dist)
#>                     blocks
#> 23                       -
#> 24                       -
#> 25                       -
#> 26 distribution parameters
```

The per-family reference sections live on its help page:
[`?table_regression_models`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
(also reachable as
[`?table_regression_mixed`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md),
[`?table_regression_ordinal`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md),
[`?table_regression_survival`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md),
and the other family aliases).
