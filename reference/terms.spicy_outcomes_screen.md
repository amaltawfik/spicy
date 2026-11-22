# Terms method for bundles of one model per outcome

Returns the [`stats::terms()`](https://rdrr.io/r/stats/terms.html)
object of the formula `outcome_1 ~ exposure + covariates`, the
right-hand side every model fitted by
[`table_regression_outcomes()`](https://amaltawfik.github.io/spicy/reference/table_regression_outcomes.md)
shares. Non-syntactic column names are backtick-quoted. Used internally
to read the term labels a `labels` entry can name, off every model in a
table.

## Usage

``` r
# S3 method for class 'spicy_outcomes_screen'
terms(x, ...)
```

## Arguments

- x:

  A `spicy_outcomes_screen` bundle (the internal object wrapping the
  models of
  [`table_regression_outcomes()`](https://amaltawfik.github.io/spicy/reference/table_regression_outcomes.md)).

- ...:

  Additional arguments (currently ignored).

## Value

A `terms` object.

## See also

[`table_regression_outcomes()`](https://amaltawfik.github.io/spicy/reference/table_regression_outcomes.md)
