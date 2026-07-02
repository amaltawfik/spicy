# Bayesian models in table_regression()

Reporting behaviour for rstanarm (`stanreg`) and brms (`brmsfit`) fits.

## Semantics

Coefficient rows report the posterior median, the posterior SD in the SE
column, and equal-tailed credible intervals at `ci_level`. There is
deliberately **no p-value column** (no null-hypothesis test statistic is
computed); significance stars do not apply. `exponentiate = TRUE`
follows the model's link, as for frequentist fits. Random-effect /
hierarchical structure reporting follows the mixed-models conventions
where applicable.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
[table_regression_models](https://amaltawfik.github.io/spicy/reference/table_regression_models.md).
