# Supported model classes in table_regression()

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
accepts a single fitted model or a list of fitted models from 35+
classes, and renders each with the reporting conventions of its model
family. This page is the map; the per-family pages listed below document
the class-specific behaviour in detail.

## Supported classes by family

- Linear and generalized linear:

  [`stats::lm`](https://rdrr.io/r/stats/lm.html),
  [`stats::glm`](https://rdrr.io/r/stats/glm.html),
  [`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html),
  [`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html),
  [`stats::nls`](https://rdrr.io/r/stats/nls.html).

- Robust, IV, quantile, panel:

  [`estimatr::lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.html)
  / `iv_robust`, [`AER::ivreg`](https://rdrr.io/pkg/AER/man/ivreg.html)
  / `tobit`, [`quantreg::rq`](https://rdrr.io/pkg/quantreg/man/rq.html),
  [`fixest::feols`](https://lrberge.github.io/fixest/reference/feols.html)
  / `feglm` / `fepois` / `fenegbin`. See
  [table_regression_robust](https://amaltawfik.github.io/spicy/reference/table_regression_robust.md).

- Mixed effects:

  [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) / `glmer`,
  [`glmmTMB::glmmTMB`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html),
  [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) / `gls`. See
  [table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).

- Ordinal:

  [`MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html),
  [`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html) (including
  partial proportional odds via `nominal = ~`). See
  [table_regression_ordinal](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md).

- Categorical:

  [`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html),
  [`mlogit::mlogit`](https://rdrr.io/pkg/mlogit/man/mlogit.html). See
  [table_regression_categorical](https://amaltawfik.github.io/spicy/reference/table_regression_categorical.md).

- Counts and two-part models:

  [`pscl::hurdle`](https://rdrr.io/pkg/pscl/man/hurdle.html) /
  `zeroinfl`, and `glmmTMB` zero-inflation / dispersion components. See
  [table_regression_counts](https://amaltawfik.github.io/spicy/reference/table_regression_counts.md).

- Survival:

  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) /
  `survreg`, [`rms::cph`](https://rdrr.io/pkg/rms/man/cph.html),
  [`flexsurv::flexsurvreg`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md).
  See
  [table_regression_survival](https://amaltawfik.github.io/spicy/reference/table_regression_survival.md).

- Survey-weighted:

  [`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html)
  (design-based SEs are the default and never overwritten).

- Additive, proportions, selection:

  [`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html) / `bam`,
  [`betareg::betareg`](https://rdrr.io/pkg/betareg/man/betareg.html),
  [`sampleSelection::selection`](https://rdrr.io/pkg/sampleSelection/man/selection.html).

- rms:

  [`rms::ols`](https://rdrr.io/pkg/rms/man/ols.html) / `lrm` / `Glm`
  (cluster-robust via
  [`rms::robcov`](https://rdrr.io/pkg/rms/man/robcov.html)).

- Bayesian:

  `rstanarm`, `brms`. See
  [table_regression_bayesian](https://amaltawfik.github.io/spicy/reference/table_regression_bayesian.md).

## Shared semantics

Whatever the class, the same rules hold:

- A robust `vcov` request is honoured through the class's field-standard
  backend, or **refused with a clear error** naming the supported set –
  never silently ignored, and the footer always names the estimator
  actually applied.

- `exponentiate = TRUE` is link-gated: it produces a labelled ratio (OR
  / IRR / HR / RR / MR / RRR / TR) only where the link warrants one;
  identity-link fits warn and are left untouched.

- Class-specific structure renders as labelled subordinate blocks of
  rows in the same table (ordinal `Thresholds`, mixed `Random effects`,
  two-part `Zero-inflation` / `Zero hurdle` / `Dispersion`), with a
  footer line explaining each block.

- Fit statistics default to the family's field standard and can be
  overridden with `show_fit_stats`; class-inappropriate tokens are
  rejected with a pointer to the right ones.

- Everything is available programmatically:
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html),
  `glance()`,
  [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md),
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
for the full argument reference;
[`vignette("table-regression")`](https://amaltawfik.github.io/spicy/articles/table-regression.md)
for a narrative introduction.
