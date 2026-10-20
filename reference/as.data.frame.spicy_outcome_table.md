# Coerce a `spicy_outcome_table` to a plain data frame or tibble

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the table
[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md)
displays, with its numbers: exactly what `output = "data.frame"`
returns. One row per printed row – the `Overall` row, then for each
grouping its header row, which carries the block's comparison, and its
levels – with the key columns `variable` (the outcome on the `Overall`
row, the grouping elsewhere), `label`, `level` and `.row_role`
(`"summary"`, `"factor_header"`, `"level"`, or `"missing"`), then the
printed columns under their names (`"M"`, `"SD"`, ..., `"n"`, `"p"`). A
cell that prints several numbers keeps the others beside it, as in
[`as.data.frame.spicy_continuous_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_table.md).
The note, as printed, is kept in `attr(, "note")`. `as_tibble()` returns
the same table as a tibble. The original `x` is unaffected, and
`print(x)` continues to render the formatted table.

## Usage

``` r
# S3 method for class 'spicy_outcome_table'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spicy_outcome_table'
as_tibble(x, ...)
```

## Arguments

- x:

  A `spicy_outcome_table` returned by
  [`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md).

- row.names, optional:

  Standard
  [`base::as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  arguments, currently ignored.

- ...:

  Further arguments passed to
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  (for the tibble method) or ignored.

## Value

A plain `data.frame` (or `tbl_df`), identical to the
`output = "data.frame"` of the same call.

## Details

For every statistic the table computes, use `output = "long"` or
[`tidy.spicy_outcome_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_outcome_table.md).

## See also

[`tidy.spicy_outcome_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_outcome_table.md),
[`glance.spicy_outcome_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_outcome_table.md).
