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

## Choosing a model

Most readers arrive with the opposite question: not “what does spicy
support?” but “I have this outcome – which model do I fit?”. The table
below answers it for the situations applied work meets most often. It
*recommends and explains*; it never chooses for you – model choice
depends on your design and your estimand, and spicy will render
whichever defensible model you fit.

| Your outcome | The situation | Reach for | Notes |
|----|----|----|----|
| Continuous | roughly symmetric errors, independent observations | [`lm()`](https://rdrr.io/r/stats/lm.html) | the default screen; `beta` for standardized effects |
| Continuous | positive and right-skewed | `glm(family = Gamma("log"))` | `exponentiate = TRUE` gives mean ratios (MR) |
| Continuous | censored at a bound (floor/ceiling) | [`AER::tobit()`](https://rdrr.io/pkg/AER/man/tobit.html) |  |
| Continuous | outlying responses distort the fit | [`MASS::rlm()`](https://rdrr.io/pkg/MASS/man/rlm.html) | M-estimation: resistant estimates, not just fixed SEs |
| Continuous | the median or another quantile is the estimand | [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) | one model per quantile; effects are quantile-specific |
| Binary | independent observations | `glm(family = binomial())` | OR via `exponentiate`; add `"ame"` for probability effects |
| Binary | overdispersed grouped binomial data | `glm(family = quasibinomial())` |  |
| Ordered categories | a Likert scale, severity grades | [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) or [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) | thresholds render as a labelled block; AME per category; `clm()` also fits partial proportional odds |
| Unordered categories | 3+ nominal outcomes | [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html); [`mlogit::mlogit()`](https://rdrr.io/pkg/mlogit/man/mlogit.html) for alternative-specific predictors | outcome categories render as column groups |
| Count | mean roughly equal to variance | `glm(family = poisson())` | IRR via `exponentiate`; rates via an [`offset()`](https://rdrr.io/r/stats/offset.html) |
| Count | variance well above the mean | [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) | models the overdispersion; `quasipoisson` merely widens the SEs |
| Count | more zeros than the count part explains | [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) / [`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) | both components render as labelled blocks |
| Proportion in (0, 1) | rates, indices, shares | [`betareg::betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html) |  |
| Time-to-event | independent observations | [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) | HR – and adjusted RMST / risk-difference columns beyond it |
| Time-to-event | a parametric survival-time model | [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html) | time ratios (TR) via `exponentiate` |
| Any of the above | clustered or repeated measures | [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) / [`glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) **or** [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html) | *the distinction below matters* |
| Any of the above | many absorbed fixed effects (panel) | [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html) / `feglm()` | the absorbed factors render as a Yes/No block |
| Any of the above | a complex survey design | [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) | design-based SEs are the inference |
| Any of the above | smooth non-linear terms | [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) / `bam()` |  |
| Any of the above | a Bayesian analysis | [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html) / [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) | posterior medians, MAD SD, credible intervals – no p-values |
| Any of the above | an endogenous predictor, an instrument | [`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html) / [`estimatr::iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html) |  |

Three distinctions the table cannot compress:

**Clustered data: marginal or conditional?** For repeated or clustered
measurements, [`glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) and
[`geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html) answer
*different questions*. A mixed model is *subject-specific*: its odds
ratio compares outcomes within the same cluster. GEE is
*population-averaged*: its odds ratio compares whole subpopulations –
usually the public-health question. Under a logit link the two genuinely
differ – the population-averaged OR is attenuated toward the null
relative to the subject-specific one – so the choice is about your
estimand, not about taste. spicy renders both with their own correct
inference (boundary-corrected random-effect tests for the former, the
native sandwich SEs and working correlation for the latter).

**Binary outcomes: OR, RR, or a probability effect?** The odds ratio is
the default because the logit is; it is also routinely misread as a risk
ratio. When the RR is the estimand, fit the log link (`binomial("log")`)
and spicy labels the exponentiated coefficient RR – knowing that
log-binomial models can fail to converge on common data, which is a
property of the model, not of the table. When what you need is “how many
percentage points does this predictor change the probability”, add
`show_columns = c("b", "ame")` – the average marginal effect is the
quantity most readers actually want, it sidesteps both ratio debates,
and for ordinal and multinomial models spicy gives it per outcome
category.

**Survival: beyond the hazard ratio.** The HR is non-collapsible –
adjusting for a prognostic covariate changes it even without any
confounding – and when proportional hazards fail, the single reported HR
is an average whose weights depend on censoring and follow-up, not a
stable quantity. spicy’s `rmst` and `risk_diff` columns give
covariate-adjusted differences in restricted mean survival time and in
cumulative incidence by g-computation – absolute, time-anchored
quantities a reader can act on – for Cox and parametric AFT fits alike,
in single tables and in the univariable screen.

And when you fit something the guide does not cover: if the class is
supported, the registry below renders it; if it is not, spicy refuses
with a classed error that names the nearest supported route (see *When a
class is not supported*) – it never renders an approximation of a model
it does not understand.

## The registry

The table is generated from the same internal registry that the package
itself uses, so it cannot drift from the code. Call
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
to get it as a data frame.

| Family | Class | Engine | AME | Exponentiate | Blocks |
|:---|:---|:---|:---|:---|:---|
| Linear and generalized linear | `lm` | [`stats::lm()`](https://rdrr.io/r/stats/lm.html) | yes | \- | \- |
|  | `glm` | [`stats::glm()`](https://rdrr.io/r/stats/glm.html) | yes | OR / IRR / RR / MR / HR (link) | \- |
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
| Population-averaged (GEE) | `geeglm` | [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html) | yes | OR / IRR / RR / MR / HR (link) | \- |
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
|  | `svyolr` | [`survey::svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) | per category (design-based) | OR (logit) | Thresholds |
|  | `svycoxph` | [`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html) | no | HR | \- |
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
  RR for the binomial log link, MR (mean ratio) for Gamma log links, HR
  for proportional hazards, TR (time ratio) for accelerated-failure-time
  models. Identity-link fits warn and stay untouched; links whose
  exponential is not a ratio (probit, cauchit, inverse) are refused.
- **Blocks** – labelled subordinate row blocks rendered inside the same
  table (random effects, thresholds, zero components, per-outcome
  segments), each explained by a footer line.

## Cross-cutting arguments

The same arguments work across families, each through the family’s
field-standard backend – or a clear refusal.

**Robust and cluster-robust standard errors** (`vcov`, `cluster`).
Family by family:

- **`lm`, `glm`** – `HC0`–`HC5` (`sandwich`) and `CR0`–`CR3`
  (`clubSandwich`, bias-reduced with Satterthwaite df), plus
  `"bootstrap"` / `"jackknife"` resampling estimators.
- **Mixed effects** – `lmer` and
  [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) take `CR*` via
  `clubSandwich`; `glmer`, `glmmTMB` and `gls` are model-based only
  (`clubSandwich` has no working backend for the first two, and its
  `gls` backend is not yet wired and validated in spicy).
- **Ordinal (`polr`, `clm`)** – `CR0`–`CR3`, no `HC*`; the cut-point
  thresholds are reweighted from the same clustered vcov. A `clm` with a
  scale or nominal partial-proportional-odds component is model-based
  only.
- **Categorical** – `multinom` takes `CR*` (one cluster value per
  observation), `mlogit` takes `CR*` (one per choice situation). Both
  refuse `HC*`: `multinom` has no working-residual form for a
  multi-equation model, and for `mlogit`,
  [`sandwich::vcovHC()`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html)
  computes a result but silently mis-scales the meat for its per-chooser
  score structure.
- **Quantile (`rq`)** – its own estimator family: `"classical"` resolves
  to the robust `nid` sandwich, `iid` / `ker` / `rank` are opt-ins, and
  clustering goes through the native wild gradient bootstrap
  (`vcov = "bootstrap"` + `cluster`). `HC*` / `CR*` are refused.
- **Survival** – Cox models use the Lin-Wei grouped-dfbeta sandwich; the
  `rms` fits take `CR*` via
  [`rms::robcov()`](https://rdrr.io/pkg/rms/man/robcov.html) (refit with
  `x = TRUE, y = TRUE`); `survreg` takes `CR*` via
  [`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html).
- **`gam` / `bam`, `betareg`, `pscl` two-part** – `CR*` via
  [`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html);
  zero-inflated and hurdle fits cluster both components.
- **Own-estimator classes** – `estimatr` fits keep the robust SEs they
  were computed with; `fixest` fits keep their estimator (the footer
  carries fixest’s own label – clustered, Newey-West, Conley, …;
  fixest’s “IID” is normalised to “Classical”). spicy’s `HC*` / `CR*`
  tokens are refused for both.
- **Robust by construction** – `svyglm` is design-based (Taylor /
  replicate): the design variance *is* the robust variance, and
  clustering belongs in the design itself (`survey::svydesign(ids = )`).
  `geeglm` displays the sandwich SEs the fit computed over its own
  `id =` clusters; change the estimator by refitting with `std.err =`.
  Both refuse spicy’s `HC*` / `CR*` tokens and `cluster`.
- **Bayesian** – `vcov` is refused: nothing standard plays the sandwich
  role for a posterior.

Whatever the backend, the footer names the estimator actually applied,
and a robust vcov also flows into the AME uncertainty.

**Standardized coefficients** (`standardized`). Available for `lm`,
`glm` (including
[`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html)), the mixed
engines (`lmer` / `glmer` / `glmmTMB` /
[`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html)), and fixed-effects
Bayesian fits – `stan_glm`-style models and standard-formula `brm()`
models – where `"posthoc"`, `"basic"` and `"smart"` are exact affine
rescales of the posterior draws. Other classes refuse: the Bayesian
refusals (multilevel fits, brms formulas with distributional or special
terms) hint to standardize predictors before fitting, and the
frequentist ones point to the AME columns instead.

**Confidence intervals** (`ci_method`).

- Wald everywhere by default;
- `"profile"` (profile likelihood) for `glm`, `polr` and `clm`;
- `"boot_percentile"` (with `vcov = "bootstrap"`) replaces the bounds
  with equal-tailed percentile intervals of the bootstrap replicates;
- `"hdi"` (highest-density interval) for Bayesian fits, which otherwise
  report equal-tailed credible intervals.

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
mixed table – with the within R-squared among the default fit statistics
and per-factor `N (<factor>)` counts through the opt-in `"n_groups"`
token.

**Mixed effects.** `lmer` (Satterthwaite t via `lmerTest`), `glmer`,
`glmmTMB` (with zero-inflation and dispersion blocks),
[`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) and
[`nlme::gls`](https://rdrr.io/pkg/nlme/man/gls.html). Random effects
render as rows – SD, correlations, residual – deliberately without
per-row p-values; the footer carries the boundary-correct
chi-bar-squared test, and `re_test = "lrt"` / `"rlrt"` adds per-term
tests. See
[`vignette("table-regression-mixed")`](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md).

**Population-averaged (GEE).**
[`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html):
marginal (population-averaged) coefficients with the fit’s own sandwich
SEs as the default inference, the working correlation structure
disclosed in the footer (with its estimated alpha), cluster-structure
fit statistics, and opt-in QIC / QICu. See
[`vignette("table-regression-gee")`](https://amaltawfik.github.io/spicy/articles/table-regression-gee.md)
for the full workflow;
[`vignette("table-regression-mixed")`](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md)
contrasts GEE with subject-specific mixed models in a single table.

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
[`strata()`](https://rdrr.io/pkg/survival/man/strata.html) supported),
[`survival::survreg`](https://rdrr.io/pkg/survival/man/survreg.html)
(time ratios) and
[`flexsurv::flexsurvreg`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md).
Absolute effects come as covariate-adjusted RMST and risk differences by
g-computation for `coxph` and `survreg` fits. See
[`vignette("table-regression-survival")`](https://amaltawfik.github.io/spicy/articles/table-regression-survival.md).

**Survey-weighted.**
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html):
design-based inference; the unweighted n is a default fit statistic and
the sum of design weights is available as
`show_fit_stats = "weighted_nobs"`.

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
every fit (with opt-in `pd`, `rhat`, `ess_bulk` / `ess_tail` and `mcse`
columns). No p-values, by design. See
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
#> 24 Survival       coxph       survival::coxph()       RMST / risk diff                           HR
#> 25 Survival     survreg     survival::survreg() yes + RMST / risk diff TR (log-scale distributions)
#> 26 Survival         cph              rms::cph()                     no                           HR
#> 27 Survival flexsurvreg flexsurv::flexsurvreg()                     no               TR / HR (dist)
#>                     blocks
#> 24                       -
#> 25                       -
#> 26                       -
#> 27 distribution parameters
```

The per-family reference sections live on its help page:
[`?table_regression_models`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
(also reachable as
[`?table_regression_mixed`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md),
[`?table_regression_ordinal`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md),
[`?table_regression_survival`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md),
and the other family aliases).
