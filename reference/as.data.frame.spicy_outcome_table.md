# Coerce a `spicy_outcome_table` to a plain data frame or tibble

These S3 methods strip the `"spicy_outcome_table"` class and the
rendering-only attributes from an object returned by
[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md),
so the underlying long-format data can be manipulated with downstream
tools under the standard `data.frame` / `tbl_df` contract. The
`"outcome"` and `"by"` attributes are kept as lightweight provenance
markers. The original `x` is unaffected, and `print(x)` continues to
render the formatted table.

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

A plain `data.frame` (or `tbl_df`), one row per displayed row of the
table.

## Details

The returned data is identical to what `output = "long"` (or
`output = "data.frame"`) returns directly from
[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md).

## See also

[`tidy.spicy_outcome_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_outcome_table.md),
[`glance.spicy_outcome_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_outcome_table.md).
