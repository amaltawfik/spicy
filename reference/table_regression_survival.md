# Survival models in table_regression()

Reporting behaviour for
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) /
`survreg()`, [`rms::cph()`](https://rdrr.io/pkg/rms/man/cph.html), and
[`flexsurv::flexsurvreg()`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md)
fits.

## Semantics

Cox models report hazard ratios under `exponentiate = TRUE` (the
canonical report; the footer carries the event count and concordance).
`survreg` accelerated-failure-time fits exponentiate to time ratios for
log-scale distributions; identity-scale distributions (gaussian,
logistic, t) are left untouched. AME is refused for Cox fits (no single
marginal-probability effect exists on the hazard scale; an
RMST-difference estimand is on the roadmap).

## Robust variance and nested comparison

`coxph`: `CR*` via the Lin-Wei (1989) grouped-dfbeta sandwich, the
field-standard Cox robust SE, with subject-level cluster length checks
under censoring. [`rms::cph`](https://rdrr.io/pkg/rms/man/cph.html)
routes through [`rms::robcov`](https://rdrr.io/pkg/rms/man/robcov.html)
(requires `x = TRUE, y = TRUE`). `nested = TRUE` compares nested Cox
models with the likelihood-ratio test (matching `anova.coxph`);
R-squared-family change statistics are not defined and stay empty.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
[table_regression_models](https://amaltawfik.github.io/spicy/reference/table_regression_models.md).
