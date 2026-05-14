# Tidy / glance methods for `spicy_regression_table`

Standard
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) /
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
interfaces for an object returned by
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).
They re-shape the underlying long-format data into the two canonical
broom views so the table can be consumed by `gtsummary`, `modelsummary`,
`parameters`, and any tidyverse-stats pipeline.

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

A `tbl_df` (when `tibble` is installed) or a plain `data.frame`.

## Details

`tidy()` returns one row per `(model_id, term, estimate_type)` triplet,
with `estimate_type` in
`c("B", "beta", "AME", "partial_f2", "partial_eta2", "partial_omega2")`.
Reference-row placeholders (factor reference levels) and singular
coefficients (NA estimates) are dropped. Columns:
`model_id, outcome, term, estimate_type, estimate, std.error, conf.low, conf.high, statistic, df, p.value, test_type, is_intercept, factor_term, factor_level`.

`glance()` returns one row per `(model_id, outcome)` with model-level
statistics. Columns:
`model_id, outcome, nobs, weighted_nobs, r.squared, adj.r.squared, omega2, sigma, rmse, f2, AIC, AICc, BIC, deviance, df.residual`
(numeric – Satterthwaite-safe).

## See also

[`as.data.frame.spicy_regression_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_regression_table.md)
for the wide raw view.
