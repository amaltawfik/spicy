# Convert a `spicy_regression_table` to a plain data.frame / tibble

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the table
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
displays, with its numbers: exactly what `output = "data.frame"`
returns. One row per printed row – the coefficients, the header and the
levels of each factor, the reference levels, the fit statistics – with
the key columns `variable` (the variable of the row: the term of a
coefficient, the factor of a level, the token of a fit statistic such as
`"nobs"` or `"r2"`), `label` (the label displayed for that variable, or
the text of a fit-statistic row), `level` (the level of a factor row)
and `.row_role` (`"coef"`, `"factor_header"`, `"level"`, `"reference"`,
`"fit_stat"`, ..., the roles of
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)),
then the printed columns under their names, numeric: the interval of a
coefficient is split into its two bounds (`"95\% CI: LL"`,
`"95\% CI: UL"`), a fit statistic sits in the first column of its model,
and a cell of events over a total (`n_events`) keeps the events in its
column and the total beside it (`"<column>: N"`). With several models
each column name starts with the model's label. The note, as printed, is
kept in `attr(, "note")`. `as_tibble()` returns the same table as a
tibble.

## Usage

``` r
# S3 method for class 'spicy_regression_table'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spicy_regression_table'
as_tibble(x, ...)
```

## Arguments

- x:

  A `spicy_regression_table` returned by
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

- row.names, optional:

  Standard
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  arguments (currently ignored – the table's row layout is preserved).

- ...:

  Currently ignored.

## Value

A plain `data.frame` (for
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)) or a
`tbl_df` (for `as_tibble()`), identical to the `output = "data.frame"`
of the same call.

## Details

For one row per coefficient with the broom names, use `output = "long"`
or
[`tidy.spicy_regression_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_regression_table.md).

## See also

[`tidy.spicy_regression_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_regression_table.md),
[`glance.spicy_regression_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_regression_table.md)
for broom-canonical long views.
