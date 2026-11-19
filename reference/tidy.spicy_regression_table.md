# Tidy / glance methods for `spicy_regression_table`

Standard
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) /
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
interfaces for an object returned by
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).
They re-shape the underlying long-format data into the two canonical
broom views so the table can be consumed by any downstream
tidyverse-stats pipeline.

## Usage

``` r
# S3 method for class 'spicy_regression_table'
tidy(x, ...)

# S3 method for class 'spicy_regression_table'
glance(x, ...)
```

## Arguments

- x:

  A `spicy_regression_table` returned by
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

- ...:

  Currently ignored. Present for compatibility with the
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) /
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  generics.

## Value

A `tbl_df`.

## Details

`tidy()` returns one row per
`(model_id, term, estimate_type, outcome_level)` combination, with
`estimate_type` in
`c("B", "beta", "ame", "partial_f2", "partial_eta2", "partial_omega2", "partial_chi2", "partial_f")`;
the two term-level tests carry their term's df in `df` and their test in
`test_type` (`"X2"`, `"F"`). `df.residual` holds the residual df, the
second df of the test, on the rows whose test is an F (the partial F and
the effect sizes built on it), and is `NA` except on those rows.
`partial_type` names the hypothesis of the partial rows' test, `"II"` or
`"III"` (the `partial_type` of
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)),
and is `NA` on every other row. `outcome_level` names the response
category of per-category rows (ordinal / multinomial average marginal
effects) and is `NA` for single-outcome models. Reference-row
placeholders (factor reference levels) and singular coefficients (NA
estimates) are dropped. Columns:
`model_id, outcome, outcome_level, term, estimate_type, estimate, std.error, conf.low, conf.high, statistic, df, df.residual, p.value, test_type, is_intercept, factor_term, factor_level, partial_type`.

`glance()` returns one row per `(model_id, outcome)` with model-level
statistics. Columns:
`model_id, outcome, nobs, weighted_nobs, r.squared, adj.r.squared, omega2, sigma, rmse, f2, AIC, AICc, BIC, deviance, df.residual`
(numeric – Satterthwaite-safe),
`logLik, concordance, std.error.concordance`. `logLik` is the value of
[`stats::logLik()`](https://rdrr.io/r/stats/logLik.html), the number of
the `log_lik` row of
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
and `NA` for a model without a likelihood (quasi families, survey
designs, GEE, Bayesian fits). `concordance` is Harrell's C of a
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html) or
[`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html)
model and `std.error.concordance` its standard error, as in the note of
the table; both are `NA` for other models.

## See also

[`as.data.frame.spicy_regression_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_regression_table.md)
for the wide raw view.
