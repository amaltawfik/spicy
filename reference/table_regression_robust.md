# Robust, IV, quantile and panel models in table_regression()

Reporting behaviour for
[`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html)
/ `iv_robust()`,
[`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html) / `tobit()`,
[`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html), and
[`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html)
/ `feglm()` / `fepois()` / `fenegbin()` fits.

## Semantics

- `estimatr` fits carry their own robust SEs (including per-coefficient
  CR2 Satterthwaite df); spicy reports them as fitted and never
  overwrites them.

- [`quantreg::rq`](https://rdrr.io/pkg/quantreg/man/rq.html) honours the
  `se =` method used at summary time; rank-inversion CIs (`se = "rank"`)
  are carried into the CI columns.

- `fixest` fits report their fixed-effects structure in the footer;
  `feglm` / `fepois` / `fenegbin` exponentiate to OR / IRR.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
[table_regression_models](https://amaltawfik.github.io/spicy/reference/table_regression_models.md).
