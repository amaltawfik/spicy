# Coerce a `spicy_categorical_table` to a plain data frame or tibble

These S3 methods strip the `"spicy_categorical_table"` / `"spicy_table"`
classes and the rendering-only attributes (`display_df`, `indent_text`,
`align`, `decimal_mark`, `long_data`, ...) from an object returned by
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
so the underlying wide-format data can be manipulated with downstream
tools (`dplyr`, `tidyr`, etc.) under the standard `data.frame` /
`tbl_df` contract. The single attribute `"group_var"` is preserved as a
lightweight provenance marker; all other spicy attributes are dropped.
The original `x` is unaffected, and `print(x)` continues to render the
formatted ASCII table.

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

A plain `data.frame` (or `tbl_df`) with the same rows and columns as the
wide raw output of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).

## Details

The returned data is the wide raw representation (one row per
`(variable x level)`, group columns side by side). For the tidy long
format – one row per `(variable x level x group)` – use
[`tidy.spicy_categorical_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_table.md)
or call
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
directly with `output = "long"`.

## See also

[`tidy.spicy_categorical_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_table.md),
[`glance.spicy_categorical_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_table.md).
