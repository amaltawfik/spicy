# Categorical (multinomial) models in table_regression()

Reporting behaviour for
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) and
[`mlogit::mlogit()`](https://rdrr.io/pkg/mlogit/man/mlogit.html) fits.

## Layout and semantics

`multinom` coefficients render per non-reference outcome (one labelled
block per outcome level); `exponentiate = TRUE` yields relative-risk
ratios (RRR, the Stata `mlogit, rrr` convention). AME reports the
marginal effect on each outcome's probability (per-outcome columns).
`mlogit` renders per-alternative rows; AME is refused (marginaleffects
has no `slopes()` method for its one-row-per-alternative data format).

## Robust variance

`multinom`: classical only. `mlogit`: `HC*` (it provides `estfun`) and
`CR*` via
[`sandwich::vcovCL`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html);
the cluster vector must be at the choice-situation level (one entry per
individual), and a long-format cluster errors with a clear message.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
[table_regression_models](https://amaltawfik.github.io/spicy/reference/table_regression_models.md).
