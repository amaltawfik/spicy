# Coerce a `spicy_battery_table` to a plain data frame or tibble

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the table
[`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md)
displays, with its numbers: exactly what `output = "data.frame"`
returns. One row per item, with the columns `variable` (the item's
column), `label` (the label displayed), then one column per displayed
column (one per level shown, `"(Missing)"`, the summary column of `top`,
`"n"`, then `"p"`). Under `by` each name joins the group printed above
the column to the header printed under it (`"Female n"`). The `"note"`
attribute is kept. `as_tibble()` returns the same table as a tibble. The
original `x` is unaffected, and `print(x)` continues to render the
formatted table.

## Usage

``` r
# S3 method for class 'spicy_battery_table'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spicy_battery_table'
as_tibble(x, ...)

# S3 method for class 'spicy_battery_svy_table'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spicy_battery_svy_table'
as_tibble(x, ...)
```

## Arguments

- x:

  A `spicy_battery_table` returned by
  [`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md),
  or a `spicy_battery_svy_table` returned by
  [`table_battery_svy()`](https://amaltawfik.github.io/spicy/reference/table_battery_svy.md).

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
