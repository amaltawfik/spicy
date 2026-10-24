# Coerce a `spicy_multiresponse_table` to a plain data frame or tibble

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the table
[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)
displays, with its numbers: exactly what `output = "data.frame"`
returns. One row per printed row – the items, the rows of `none_row`,
then the base – with the key columns `variable` (the item's column, or
the token of a row the table adds: `"none"`, `"no_data"`, `"nobs"` for
the base), `label` (the label displayed) and `.row_role` (`"summary"`
for an item, `"none"`, `"missing"`, or `"fit_stat"` for the base), then
the printed columns under their names (`"<group> n"`, `"<group> %"`,
..., `"Test"` with `df` beside it under `statistic = TRUE`, `"p"`). The
`"note"` and `"global_test"` attributes are kept. `as_tibble()` returns
the same table as a tibble. The original `x` is unaffected, and
`print(x)` continues to render the formatted table.

## Usage

``` r
# S3 method for class 'spicy_multiresponse_table'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spicy_multiresponse_table'
as_tibble(x, ...)
```

## Arguments

- x:

  A `spicy_multiresponse_table` returned by
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md).

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

## See also

[`tidy.spicy_multiresponse_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_multiresponse_table.md),
[`glance.spicy_multiresponse_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_multiresponse_table.md).
