# Coerce a `spicy_categorical_table` to a plain data frame or tibble

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the table
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
displays, with its numbers: exactly what `output = "data.frame"`
returns. One row per printed row – the header row of each variable,
which carries its *p*-value and its association measure, then one row
per level – and the printed columns under their names (`"Female n"`,
`"Female %"`, ..., `"p"`, the measure), plus the key columns `variable`
(the column name), `label` (the label displayed for it), `level`, and
`.row_role` (`"factor_header"`, `"level"`, `"missing"` for the row of
missing values the table adds, or `"total"` for the Total row of
`total_row = TRUE`). The note, as printed, is kept in `attr(, "note")`.
`as_tibble()` returns the same table as a tibble.

## Usage

``` r
# S3 method for class 'spicy_categorical_table'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spicy_categorical_table'
as_tibble(x, ...)
```

## Arguments

- x:

  A `spicy_categorical_table` returned by
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).

- row.names, optional:

  Standard
  [`base::as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  arguments. Currently ignored.

- ...:

  Further arguments passed to
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  (for the tibble method) or ignored (for the
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  method).

## Value

A plain `data.frame` (or `tbl_df`), identical to the
`output = "data.frame"` of the same call.

## Details

For one row per variable, level and group, use `output = "long"` or
[`tidy.spicy_categorical_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_table.md).
The original `x` is unaffected, and `print(x)` continues to render the
formatted ASCII table.

## See also

[`tidy.spicy_categorical_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_table.md),
[`glance.spicy_categorical_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_table.md).
