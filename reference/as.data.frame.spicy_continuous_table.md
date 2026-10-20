# Coerce a `spicy_continuous_table` to a plain data frame or tibble

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the table
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
displays, with its numbers: exactly what `output = "data.frame"`
returns. One row per printed row, the key columns `variable` (the column
name), `label` (the label displayed for it) and, with `by`, `group` and
`.row_role` (`"group"`, or `"missing"` for the group of missing `by`
values), then the printed columns under their names (`"M"`, `"SD"`,
`"95\% CI LL"`, ..., `"n"`, `"p"`). A cell that prints several numbers
keeps the first in its column and the others beside it under their names
in the long output: `df` and `df.residual` after the test statistic
`"Test"`, `es_ci_lower` and `es_ci_upper` after the effect size `"ES"`
when its interval is printed, `q1` and `q3` after `"Med [Q1, Q3]"`. The
note, as printed, is kept in `attr(, "note")`. `as_tibble()` returns the
same table as a tibble.

## Usage

``` r
# S3 method for class 'spicy_continuous_table'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spicy_continuous_table'
as_tibble(x, ...)
```

## Arguments

- x:

  A `spicy_continuous_table` returned by
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).

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

For every statistic the table computes, whatever it displays, use
`output = "long"` or
[`tidy.spicy_continuous_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_table.md).
The original `x` is unaffected, and `print(x)` continues to render the
formatted ASCII table.

## See also

[`tidy.spicy_continuous_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_table.md),
[`glance.spicy_continuous_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_table.md)
for cleaner broom-style pivots tailored to downstream pipelines.
