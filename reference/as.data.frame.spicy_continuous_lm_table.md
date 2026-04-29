# Coerce a `spicy_continuous_lm_table` to a plain data frame or tibble

These S3 methods strip the `"spicy_continuous_lm_table"` /
`"spicy_table"` classes and the rendering-only attributes (`digits`,
`decimal_mark`, `ci_level`, ...) from an object returned by
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
so the underlying long-format data can be manipulated with downstream
tools (`dplyr`, `tidyr`, etc.) under the standard `data.frame` /
`tbl_df` contract. The single attribute `"by_var"` is preserved as a
lightweight provenance marker; all other spicy attributes are dropped.
The original `x` is unaffected, and `print(x)` continues to render the
formatted ASCII table.

## Usage

``` r
# S3 method for class 'spicy_continuous_lm_table'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spicy_continuous_lm_table'
as_tibble(x, ...)
```

## Arguments

- x:

  A `spicy_continuous_lm_table` returned by
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).

- row.names, optional:

  Standard
  [`base::as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  arguments. Currently ignored: the long format already carries integer
  row names and explicit columns.

- ...:

  Further arguments passed to
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  (for the tibble method) or ignored (for the
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  method).

## Value

A plain `data.frame` (or `tbl_df`) with the same rows and columns as the
long output of
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).

## See also

[`tidy.spicy_continuous_lm_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_lm_table.md),
[`glance.spicy_continuous_lm_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_lm_table.md)
for cleaner broom-style pivots tailored to downstream pipelines.
