# Supported models

``` r

library(spicy)
```

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
takes fitted model objects – never raw data plus a formula – and renders
them as publication-ready regression tables. “Supported” is a
commitment, not a list of classes that happen to run: every number a
supported class produces is validated against a field reference (the
model’s own [`summary()`](https://rdrr.io/r/base/summary.html),
`sandwich`, `clubSandwich`, `marginaleffects`, `effectsize`,
`performance`, Stata or SPSS conventions), and every request a class
cannot honor is **refused with a classed error** that names what is
available – never rendered as a silently empty or approximate column.

This article is the map. Each family below links to a dedicated article
that walks through its behavior in depth.

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
| Ordered categories | a Likert scale, severity grades | [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) or [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) | thresholds render as a labeled block; AME per category; `clm()` also fits partial proportional odds |
| Ordered categories | clustered or repeated measures | [`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html) or `glmmTMB::glmmTMB(family = glmmTMB::ordinal())` | thresholds and random effects as labeled blocks; Wald only, no AME |
| Unordered categories | 3+ nominal outcomes | [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html); [`mlogit::mlogit()`](https://rdrr.io/pkg/mlogit/man/mlogit.html) for alternative-specific predictors | outcome categories render as column groups |
| Count | mean roughly equal to variance | `glm(family = poisson())` | IRR via `exponentiate`; rates via an [`offset()`](https://rdrr.io/r/stats/offset.html) |
| Count | variance well above the mean | [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) | models the overdispersion; `quasipoisson` merely widens the SEs |
| Count | more zeros than the count part explains | [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) / [`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) | both components render as labeled blocks |
| Proportion in (0, 1) | rates, indices, shares | [`betareg::betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html) |  |
| Time-to-event | independent observations | [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) | HR – and adjusted RMST / risk-difference columns beyond it |
| Time-to-event | a parametric survival-time model | [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html) | time ratios (TR) via `exponentiate` |
| Any of the above | clustered or repeated measures | [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) / [`glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) **or** [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html) | *the distinction below matters* |
| Any of the above | many absorbed fixed effects (panel) | [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html) / [`feglm()`](https://lrberge.github.io/fixest/reference/feglm.html) | the absorbed factors render as a Yes/No block |
| Any of the above | a complex survey design | [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) | design-based SEs are the inference |
| Any of the above | smooth non-linear terms | [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) / [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) | smooth terms render as a labeled block (edf and test) |
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
with a classed error that lists the supported classes and points to
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
(see *When a class is not supported*) – it never renders an
approximation of a model it does not understand.

## The registry

The table is generated from the same internal registry that the package
itself uses, so it cannot drift from the code. Call
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
to get it as a data frame.

| Family | Class | Engine | AME | Exponentiate | Blocks | vcov |
|:---|:---|:---|:---|:---|:---|:---|
| Linear and generalized linear | `lm` | [`stats::lm()`](https://rdrr.io/r/stats/lm.html) | yes | \- | \- | classical, HC0-HC5, CR0-CR3, CR1S, bootstrap, jackknife |
|  | `glm` | [`stats::glm()`](https://rdrr.io/r/stats/glm.html) | yes | OR / IRR / RR / MR / HR (link) | \- | classical, HC0-HC5, CR0-CR3, bootstrap, jackknife |
|  | `negbin` | [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) | yes | IRR | \- | classical, HC0-HC5, CR0-CR3, bootstrap, jackknife |
|  | `rlm` | [`MASS::rlm()`](https://rdrr.io/pkg/MASS/man/rlm.html) | yes | \- | \- | classical |
|  | `nls` | [`stats::nls()`](https://rdrr.io/r/stats/nls.html) | no | \- | \- | classical |
| Robust, IV, quantile, panel | `lm_robust` | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html) | yes | \- | \- | classical (the fit’s own) |
|  | `iv_robust` | [`estimatr::iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html) | yes | \- | \- | classical (the fit’s own) |
|  | `ivreg` | [`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html) | yes | \- | \- | classical, HC0, HC1, CR0-CR3, CR1S |
|  | `tobit` | [`AER::tobit()`](https://rdrr.io/pkg/AER/man/tobit.html) | yes (latent outcome) | \- | \- | classical, CR1 |
|  | `rq` | [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) | yes | \- | \- | classical (= nid), nid, iid, ker, rank, bootstrap |
|  | `fixest` | [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html), [`fixest::feglm()`](https://lrberge.github.io/fixest/reference/feglm.html), [`fixest::fepois()`](https://lrberge.github.io/fixest/reference/feglm.html), [`fixest::femlm()`](https://lrberge.github.io/fixest/reference/femlm.html), [`fixest::fenegbin()`](https://lrberge.github.io/fixest/reference/femlm.html) | yes (no for a GLM with fixed effects or an IV fit without exogenous regressor) | OR / IRR (link) | \- | classical (the fit’s own) |
| Mixed effects | `lmerMod` | [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) | yes | \- | Random effects | classical, CR0-CR3 |
|  | `glmerMod` | [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) | yes | OR / IRR (link) | Random effects | classical |
|  | `glmmTMB` | [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) | yes (no for the ordinal family or a REML fit) | link-dependent (IRR for count families, OR for ordinal logit) | Random effects; Zero-inflation (ziformula); Zero hurdle, Pr(y = 0) for a zero-truncated family, ziGamma, lognormal or beta_family; Dispersion (dispformula); Precision (dispformula of beta_family); Thresholds (ordinal family) | classical, CR0, CR1 (classical only for REML or the ordinal family) |
|  | `lme` | [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) | yes | \- | Random effects | classical, CR0-CR3 (classical only with contrasts other than the session’s) |
|  | `gls` | [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) | yes | \- | \- | classical |
| Ordinal | `polr` | [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) | per category | OR (logit) | Thresholds | classical, CR1 |
|  | `clm` | [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) | per category | OR (logit) | Thresholds; Non-proportional effects (nominal =); Scale effects (scale =) | classical, CR1 (classical only with scale or nominal terms) |
|  | `clmm` | [`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html) | no | OR (logit) | Thresholds; Random effects | classical |
| Categorical | `multinom` | [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) | per outcome | OR | per-outcome blocks | classical, CR1 |
|  | `mlogit` | [`mlogit::mlogit()`](https://rdrr.io/pkg/mlogit/man/mlogit.html) | no | OR | per-alternative rows | classical, CR1 |
| Counts, two-part | `zeroinfl` | [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero-inflation | classical, CR1 (classical only for a negative-binomial part) |
|  | `hurdle` | [`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero hurdle | classical, CR1 (classical only for a negative-binomial part) |
| Survival | `coxph` | [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) | RMST / risk diff | HR | \- | classical, CR0 |
|  | `survreg` | [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html) | yes + RMST / risk diff | TR (log-scale distributions) | \- | classical, CR1 |
|  | `cph` | [`rms::cph()`](https://rdrr.io/pkg/rms/man/cph.html) | no | HR | \- | classical, CR0 (classical only without x = TRUE, y = TRUE) |
|  | `flexsurvreg` | [`flexsurv::flexsurvreg()`](https://chjackson.github.io/flexsurv/reference/flexsurvreg.html) | no | TR / HR (dist) | distribution parameters | classical |
| Survey-weighted | `svyglm` | [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) | yes (design-based) | OR / IRR | \- | classical (design-based) |
|  | `svyolr` | [`survey::svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) | per category (design-based) | OR (logit) | Thresholds | classical (design-based) |
|  | `svycoxph` | [`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html) | no | HR | \- | classical (design-based) |
| Population-averaged (GEE) | `geeglm` | [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html) | yes | OR / IRR / RR / MR / HR (link) | \- | classical (the fit’s own) |
| Additive, proportions, selection | `gam` | [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html), [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) | yes | OR / IRR (link) | Smooth terms | classical, CR1 (classical only for an mgcv extended family other than nb) |
|  | `betareg` | [`betareg::betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html) | yes | OR (mean link) | Precision (y ~ x \| z) | classical, CR1 |
|  | `selection` | [`sampleSelection::selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html) | no | \- | Error terms | classical |
| rms | `ols` | [`rms::ols()`](https://rdrr.io/pkg/rms/man/ols.html) | yes | \- | \- | classical, CR0 (classical only without x = TRUE, y = TRUE) |
|  | `lrm` | [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) | yes (per category for an ordinal outcome) | OR | Thresholds (ordinal outcome) | classical, CR0 (classical only without x = TRUE, y = TRUE) |
|  | `Glm` | [`rms::Glm()`](https://rdrr.io/pkg/rms/man/Glm.html) | yes | link-dependent | \- | classical, CR0 (classical only without x = TRUE, y = TRUE) |
| Bayesian | `stanreg` | [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html), [`rstanarm::stan_glmer()`](https://mc-stan.org/rstanarm/reference/stan_glmer.html) | yes (draws) | link-dependent | Random effects (if multilevel) | classical (the posterior) |
|  | `brmsfit` | [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) | yes (draws) | link-dependent | Random effects (if multilevel) | classical (the posterior) |

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
  median with MAD SD and credible interval. Under every entry, the AME
  of a continuous predictor is the average slope by default;
  `ame_type = "unit"` or `"sd"` reports the average change for one unit
  or one standard deviation instead.
- **Exponentiate** – the labeled ratio `exponentiate = TRUE` produces.
  The label follows the link: OR under logit, IRR for count log-links,
  RR for the binomial log link, MR (mean ratio) for Gamma log links, HR
  for proportional hazards, TR (time ratio) for accelerated-failure-time
  models. Identity-link fits warn and stay untouched; links whose
  exponential is not a ratio (probit, cauchit, inverse) are refused. An
  exponentiated intercept is not a ratio either, and the note says what
  it estimates: the baseline odds, risk, rate or mean, or a quantile of
  the survival time for an AFT model.
- **Blocks** – labeled subordinate row blocks rendered inside the same
  table (smooth terms, random effects, thresholds, zero components,
  per-outcome segments), each explained by a footer line.
- **vcov** – the values of `vcov` the class accepts; see the next
  section.

## Cross-cutting arguments

The same arguments work across families, each through the family’s
field-standard backend – or a clear refusal.

**Robust and cluster-robust standard errors** (`vcov`, `cluster`). The
values of `vcov` each class accepts come from the same registry:

| Family | Class | vcov |
|:---|:---|:---|
| Linear and generalized linear | `lm` | classical, HC0-HC5, CR0-CR3, CR1S, bootstrap, jackknife |
|  | `glm` | classical, HC0-HC5, CR0-CR3, bootstrap, jackknife |
|  | `negbin` | classical, HC0-HC5, CR0-CR3, bootstrap, jackknife |
|  | `rlm` | classical |
|  | `nls` | classical |
| Robust, IV, quantile, panel | `lm_robust` | classical (the fit’s own) |
|  | `iv_robust` | classical (the fit’s own) |
|  | `ivreg` | classical, HC0, HC1, CR0-CR3, CR1S |
|  | `tobit` | classical, CR1 |
|  | `rq` | classical (= nid), nid, iid, ker, rank, bootstrap |
|  | `fixest` | classical (the fit’s own) |
| Mixed effects | `lmerMod` | classical, CR0-CR3 |
|  | `glmerMod` | classical |
|  | `glmmTMB` | classical, CR0, CR1 (classical only for REML or the ordinal family) |
|  | `lme` | classical, CR0-CR3 (classical only with contrasts other than the session’s) |
|  | `gls` | classical |
| Ordinal | `polr` | classical, CR1 |
|  | `clm` | classical, CR1 (classical only with scale or nominal terms) |
|  | `clmm` | classical |
| Categorical | `multinom` | classical, CR1 |
|  | `mlogit` | classical, CR1 |
| Counts, two-part | `zeroinfl` | classical, CR1 (classical only for a negative-binomial part) |
|  | `hurdle` | classical, CR1 (classical only for a negative-binomial part) |
| Survival | `coxph` | classical, CR0 |
|  | `survreg` | classical, CR1 |
|  | `cph` | classical, CR0 (classical only without x = TRUE, y = TRUE) |
|  | `flexsurvreg` | classical |
| Survey-weighted | `svyglm` | classical (design-based) |
|  | `svyolr` | classical (design-based) |
|  | `svycoxph` | classical (design-based) |
| Population-averaged (GEE) | `geeglm` | classical (the fit’s own) |
| Additive, proportions, selection | `gam` | classical, CR1 (classical only for an mgcv extended family other than nb) |
|  | `betareg` | classical, CR1 |
|  | `selection` | classical |
| rms | `ols` | classical, CR0 (classical only without x = TRUE, y = TRUE) |
|  | `lrm` | classical, CR0 (classical only without x = TRUE, y = TRUE) |
|  | `Glm` | classical, CR0 (classical only without x = TRUE, y = TRUE) |
| Bayesian | `stanreg` | classical (the posterior) |
|  | `brmsfit` | classical (the posterior) |

A request the class cannot honor is refused with a classed error that
gives the reason and the estimators the class supports. The help page of
each family gives the backend of each estimator and the reason of each
refusal: [mixed
effects](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md),
[survival](https://amaltawfik.github.io/spicy/reference/table_regression_survival.md),
[ordinal](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md),
[categorical](https://amaltawfik.github.io/spicy/reference/table_regression_categorical.md),
[counts and
two-part](https://amaltawfik.github.io/spicy/reference/table_regression_counts.md),
[survey
designs](https://amaltawfik.github.io/spicy/reference/table_regression_survey.md),
[Bayesian](https://amaltawfik.github.io/spicy/reference/table_regression_bayesian.md)
and [robust, IV, quantile and
panel](https://amaltawfik.github.io/spicy/reference/table_regression_robust.md)
models; `lm`, `glm` and the remaining classes are covered on
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
GEE on
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md).

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
regressions (all fits at one tau). Under a robust or cluster-robust
`vcov`, each added block is tested by a Wald test on that matrix
instead: a Wald F-change for `lm`, a Wald chi-squared for `glm` and
`coxph`. `p_adjust` applies a multiplicity correction across the
coefficient p-values of each model, the intercept excepted, rows hidden
by `keep` or `drop` included – and is refused for Bayesian tables, which
carry no p-values at all.

## The families in brief

**Linear and generalized linear.** The core engines: `lm`, `glm` (with
profile CIs on request),
[`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html) (with opt-in
`theta` / `alpha` dispersion statistics),
[`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html),
[`stats::nls`](https://rdrr.io/r/stats/nls.html). Start with
[*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
and [*Categorical
predictors*](https://amaltawfik.github.io/spicy/articles/categorical-predictors.md).

**Robust, IV, quantile, panel.**
[`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html)
/
[`iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html),
[`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html) and
[`AER::tobit()`](https://rdrr.io/pkg/AER/man/tobit.html),
[`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) (defaulting
to the heteroskedasticity-robust `nid` sandwich – quantreg’s own
large-sample default – with `iid`, `ker`, `rank` CIs and a native
clustered bootstrap as `vcov` options), and the `fixest` estimators.
Absorbed fixed effects – a `fixest` fit, or an `estimatr` fit built with
`fixed_effects =` – render as a default-on `Fixed effects:` block – one
Yes / No row per factor, blank for models without the concept in a mixed
table – with per-factor `N (<factor>)` counts through the opt-in
`"n_groups"` token and the within R-squared through `"within_r2"` (a
`fixest` default). See [*Robust, IV, quantile and panel regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-robust.md).

**Mixed effects.** `lmer` (Satterthwaite t via `lmerTest`), `glmer`,
`glmmTMB` (with zero-inflation and dispersion blocks, and a `Thresholds`
block for the ordinal family),
[`ordinal::clmm`](https://rdrr.io/pkg/ordinal/man/clmm.html),
[`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) and
[`nlme::gls`](https://rdrr.io/pkg/nlme/man/gls.html). Random effects
render as rows – SD, correlations, residual – deliberately without
per-row p-values; the footer carries the boundary-correct
chi-bar-squared test, and `re_test = "lrt"` / `"rlrt"` adds per-term
tests. See [*Mixed-effects regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md).

**Population-averaged (GEE).**
[`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html):
marginal (population-averaged) coefficients with the fit’s own sandwich
SEs as the default inference, the working correlation structure
disclosed in the footer (with its estimated alpha), cluster-structure
fit statistics, and opt-in QIC / QICu. See [*GEE regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-gee.md)
for the full workflow; [*Mixed-effects regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.md)
contrasts GEE with subject-specific mixed models in a single table.

**Ordinal.** [`MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html) and
[`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html): proportional
odds ratios, a `Thresholds` block for the cut-points (log-odds scale,
never exponentiated), partial-proportional-odds terms as a
`Non-proportional effects` block, and per-category AME. Cumulative-link
mixed models,
[`ordinal::clmm`](https://rdrr.io/pkg/ordinal/man/clmm.html) and
[`glmmTMB::glmmTMB`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) with
`family = glmmTMB::ordinal()`, add the `Random effects` block and keep
the `Thresholds` block; they are Wald only, without AME. See [*Ordinal
regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.md).

**Categorical.**
[`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html) renders
outcome categories as columns with per-outcome AME;
[`mlogit::mlogit`](https://rdrr.io/pkg/mlogit/man/mlogit.html) renders
per-alternative rows for discrete-choice designs. See [*Multinomial
regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-multinomial.md).

**Counts and two-part.** Poisson and negative binomial through `glm` /
`glm.nb` / `glmmTMB`, plus
[`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) and
[`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) with their
zero components as labeled blocks and a combined-response AME. See
[*Count and two-part regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-counts.md).

**Survival.**
[`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) and
[`rms::cph`](https://rdrr.io/pkg/rms/man/cph.html) (hazard ratios,
[`strata()`](https://rdrr.io/pkg/survival/man/strata.html) supported),
[`survival::survreg`](https://rdrr.io/pkg/survival/man/survreg.html)
(time ratios) and
[`flexsurv::flexsurvreg`](https://chjackson.github.io/flexsurv/reference/flexsurvreg.html).
Absolute effects come as covariate-adjusted RMST and risk differences by
g-computation for `coxph` and `survreg` fits. The baseline hazard those
estimands standardize follows the tie-handling convention of the fit,
exactly as [`survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
and [`basehaz()`](https://rdrr.io/pkg/survival/man/basehaz.html) do – a
`ties = "breslow"` fit gives a Breslow baseline, the default Efron fit
an Efron one – so the hazard-ratio column and the dRMST column always
come from the same likelihood. See [*Survival regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-survival.md).

**Survey-weighted.**
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html),
[`svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) and
[`svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html):
design-based inference, with the unweighted n and the sum of the design
weights (`Weighted n`) as default fit statistics. See [*Regression
tables for survey
designs*](https://amaltawfik.github.io/spicy/articles/table-regression-survey.md).

**Additive, proportions, selection.**
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) /
[`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) (a GAM shows the R²
rows of the [`glm()`](https://rdrr.io/r/stats/glm.html) of its family
and link, and its smooth terms as a block: the effective degrees of
freedom and the p-value of the approximate test of each term),
[`betareg::betareg()`](https://rdrr.io/pkg/betareg/man/betareg.html)
(odds ratios on the logit mean link under `exponentiate`; a precision
submodel as a `Precision` block, a constant precision as the opt-in
`phi` fit statistic), and
[`sampleSelection::selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html),
[`heckit()`](https://rdrr.io/pkg/sampleSelection/man/selection.html) and
[`treatReg()`](https://rdrr.io/pkg/sampleSelection/man/selection.html)
(a row per equation under each variable, and σ, ρ and the inverse Mills
ratio in an `Error terms` block). See [*Additive, beta and selection
regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-additive.md).

**rms.** `ols`, `lrm`, `Glm` and `cph` fits, with their restricted cubic
splines, the variance of
[`robcov()`](https://rdrr.io/pkg/rms/man/robcov.html) and
[`bootcov()`](https://rdrr.io/pkg/rms/man/bootcov.html), and penalized
estimation. See [*Regression tables for rms
fits*](https://amaltawfik.github.io/spicy/articles/table-regression-rms.md).

**Bayesian.** `rstanarm` and `brms` fits are summarized from their
posterior draws: posterior median, MAD SD, credible intervals,
draws-native exponentiation and AME, sampler diagnostics checked on
every fit (with opt-in `pd`, `rhat`, `ess_bulk` / `ess_tail` and `mcse`
columns). No p-values, by design. See [*Bayesian regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression-bayesian.md).

## When a class is not supported

An unsupported class fails fast with a classed error
(`spicy_unsupported`):

``` r

fit <- loess(dist ~ speed, data = cars)
table_regression(fit)
#> Error in `table_regression()`:
#> ! Some `models` are not supported by `table_regression()`.
#> Position 1: `loess` – not a supported model class. If support would be useful, please open an issue: https://github.com/amaltawfik/spicy/issues
#> ℹ Supported model classes: lm, glm, negbin, rlm, nls, lm_robust, iv_robust, ivreg, tobit, rq, fixest, lmerMod, glmerMod, glmmTMB, lme, gls, polr, clm, clmm, multinom, mlogit, zeroinfl, hurdle, coxph, survreg, cph, flexsurvreg, svyglm, svyolr, svycoxph, geeglm, gam, betareg, selection, ols, lrm, Glm, stanreg, brmsfit. `table_regression_models()` lists them with their engines.
```

The same contract applies inside a family: a request a class cannot
honor – `HC*` for `multinom`, AME for `mlogit`, `exponentiate` on a
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
#>                     blocks                                                       vcov   fit_stats_default
#> 24                       -                                             classical, CR0 nobs, n_events, aic
#> 25                       -                                             classical, CR1           nobs, aic
#> 26                       - classical, CR0 (classical only without x = TRUE, y = TRUE) nobs, n_events, aic
#> 27 distribution parameters                                                  classical           nobs, aic
#>                                               fit_stats
#> 24 nobs, n_events, aic, aicc, bic, log_lik, concordance
#> 25     nobs, sigma (no strata), aic, aicc, bic, log_lik
#> 26              nobs, n_events, aic, aicc, bic, log_lik
#> 27                        nobs, aic, aicc, bic, log_lik
```

Each family has a help page of its own, listed on
[`?table_regression_models`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md):
[`?table_regression_mixed`](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md),
[`?table_regression_survival`](https://amaltawfik.github.io/spicy/reference/table_regression_survival.md),
[`?table_regression_ordinal`](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md),
and the others.
