# Count and two-part models in table_regression()

Reporting behaviour for
[`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) /
[`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) and for
`glmmTMB` fits with `ziformula =` / `dispformula =` components.

## Component blocks

Two-part models show their FULL model. The count / conditional
coefficients render as the main block; the other components render as
labelled subordinate blocks, with full Wald inference from the
per-component covariance:

- `Zero-inflation` (`zeroinfl`, `glmmTMB` zi): the model for the
  probability of a **structural (excess) zero**.

- `Zero hurdle` (`hurdle`): the model for the probability of a **nonzero
  count** – the opposite direction of zero-inflation. The distinct
  labels and the footer gloss keep the two apart (they are conflated
  under one header in some other packages).

- `Dispersion` (`glmmTMB`): only when dispersion was actually modelled
  (`dispformula` with covariates); log scale.

Component coefficients are substantive hypotheses: they join the
`p_adjust` family and take significance stars. Opt out of the blocks
with `show_components = FALSE`.

## Exponentiate

The count component exponentiates to incidence-rate ratios (IRR). A zero
component is exponentiated **only when its link makes the result an odds
ratio** (the default logit); probit / cauchit / cloglog zero links,
count-type hurdle zero parts, and the dispersion model stay on the link
scale, disclosed in the footer. (Stata's `irr` similarly leaves the
inflate equation untransformed.)

## Average marginal effects

The AME column reports the combined-response marginal effect on E(Y),
flowing through both components (the
[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
default) – the policy-relevant quantity for a two-part model.

## Robust variance

`CR*` for `zeroinfl` / `hurdle` via
[`sandwich::vcovCL`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html),
covering **both components** with one estimator. `HC*` is refused (no
hatvalues machinery). For `glmmTMB`, see
[table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
[table_regression_models](https://amaltawfik.github.io/spicy/reference/table_regression_models.md).
