# Coerce a `spicy_continuous_svy_table` to a plain data frame or tibble

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the table
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
displays, with its numbers: exactly what `output = "data.frame"`
returns, the shape of
[`as.data.frame.spicy_continuous_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_table.md)
(the key columns `variable`, `label` and, with `by`, `group` and
`.row_role`, then the printed columns under their names). The note, as
printed, is kept in `attr(, "note")`. `as_tibble()` returns the same
table as a tibble.

## Usage

``` r
# S3 method for class 'spicy_continuous_svy_table'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spicy_continuous_svy_table'
as_tibble(x, ...)
```

## Arguments

- x:

  A `spicy_continuous_svy_table` returned by
  [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md).

- row.names, optional, ...:

  Passed on for method compatibility; ignored.

## Value

A plain `data.frame` (or a `tbl_df` for `as_tibble()`), identical to the
`output = "data.frame"` of the same call.

## See also

[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md).
