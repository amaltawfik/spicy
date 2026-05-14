# Convert a `spicy_regression_table` to a plain data.frame / tibble

Strips the `spicy_regression_table` / `spicy_table` classes and the
`col_spec` rendering metadata, returning the wide character display as a
plain `data.frame` (or `tbl_df` via `as_tibble()`). The `title` and
`note` attributes are preserved.

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
`tbl_df` (for `as_tibble()`).

## Details

Equivalent to passing `output = "data.frame"` to
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

## See also

[`tidy.spicy_regression_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_regression_table.md),
[`glance.spicy_regression_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_regression_table.md)
for broom-canonical long views.
