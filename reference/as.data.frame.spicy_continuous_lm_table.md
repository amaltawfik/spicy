# Coerce a `spicy_continuous_lm_table` to a plain data frame or tibble

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the table
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
displays, with its numbers: exactly what `output = "data.frame"`
returns. One row per outcome, the key columns `variable` (the column
name of the outcome) and `label` (the label displayed for it), then the
printed columns under their names (`"M (<level>)"`, the contrast, the
interval bounds, the test statistic, `"p"`, the fit statistic, the
effect size, `"n"`, `"Weighted n"`); an effect size printed with its
interval keeps the bounds beside it as `es_ci_lower` and `es_ci_upper`.
The note, as printed, is kept in `attr(, "note")`. `as_tibble()` returns
the same table as a tibble. The original `x` is unaffected, and
`print(x)` continues to render the formatted ASCII table.

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

For one row per level of `by` with every estimate, use
`output = "long"`,
[`tidy.spicy_continuous_lm_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_lm_table.md)
or
[`glance.spicy_continuous_lm_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_lm_table.md).

## See also

[`tidy.spicy_continuous_lm_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_lm_table.md),
[`glance.spicy_continuous_lm_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_lm_table.md)
for cleaner broom-style pivots tailored to downstream pipelines.
