# Terms method for univariable screen bundles

Returns the [`stats::terms()`](https://rdrr.io/r/stats/terms.html)
object of the formula `outcome ~ predictor_1 + ... + predictor_k`
spanning every predictor screened by
[`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md).
Non-syntactic column names are backtick-quoted, so the terms are valid
whatever the input names. Used internally by the label validator, which
reads term labels off every model in a table.

## Usage

``` r
# S3 method for class 'spicy_uv_screen'
terms(x, ...)
```

## Arguments

- x:

  A `spicy_uv_screen` bundle (the internal object wrapping the
  univariable fits of
  [`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)).

- ...:

  Additional arguments (currently ignored).

## Value

A `terms` object for `outcome ~ all screened predictors`.

## See also

[`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)
