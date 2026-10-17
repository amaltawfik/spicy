# Coerce a `spicy_multiresponse_table` to a plain data frame or tibble

These S3 methods strip the `"spicy_multiresponse_table"` /
`"spicy_table"` classes and the rendering-only attributes from an object
returned by
[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md),
so the underlying wide data can be manipulated with downstream tools
under the standard `data.frame` / `tbl_df` contract. The `"note"` and
`"global_test"` attributes are kept. The original `x` is unaffected, and
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

A plain `data.frame` (or `tbl_df`).

## Details

The returned data is identical to what `output = "data.frame"` returns
directly from
[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md):
one row per item, then the base.

## See also

[`tidy.spicy_multiresponse_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_multiresponse_table.md),
[`glance.spicy_multiresponse_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_multiresponse_table.md).
