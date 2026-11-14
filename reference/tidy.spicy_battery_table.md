# Tidying methods for a `spicy_battery_table`

Standard
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
interfaces for an object returned by
[`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md)
or
[`table_battery_svy()`](https://amaltawfik.github.io/spicy/reference/table_battery_svy.md).

## Usage

``` r
# S3 method for class 'spicy_battery_table'
tidy(x, ...)

# S3 method for class 'spicy_battery_table'
glance(x, ...)

# S3 method for class 'spicy_battery_svy_table'
tidy(x, ...)

# S3 method for class 'spicy_battery_svy_table'
glance(x, ...)
```

## Arguments

- x:

  A `spicy_battery_table` returned by
  [`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md),
  or a `spicy_battery_svy_table` returned by
  [`table_battery_svy()`](https://amaltawfik.github.io/spicy/reference/table_battery_svy.md).

- ...:

  Ignored, for S3 compatibility.

## Value

A `tbl_df` (or `data.frame` when tibble is not installed).

## Details

`tidy()` returns one row per item, group and level: the long reading of
the table, the rows of `output = "long"`, with the percentage as a
proportion. Columns: `variable` (the item's column), `label` (the label
the row displays), `level` (the level, or the label of the column of
missing answers), `.row_role` (`"level"`, or `"missing"` on the missing
answers of `drop_na = FALSE`), `group` (the `by` level, `NA` without
`by`), `n` (the count: weighted under the `weights` of
[`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md),
observed in
[`table_battery_svy()`](https://amaltawfik.github.io/spicy/reference/table_battery_svy.md)),
`proportion` (between 0 and 1, the share of the item's base, estimated
under the design in
[`table_battery_svy()`](https://amaltawfik.github.io/spicy/reference/table_battery_svy.md);
`NA` on the missing answers under `percent_base = "valid"`, which leaves
them out of the base), and the test of the item: `test_type`,
`statistic` and `df` (`NA` for an exact test, and in
[`table_battery_svy()`](https://amaltawfik.github.io/spicy/reference/table_battery_svy.md),
whose long output does not carry them), `p.value` (adjusted under
`p_adjust`) and `p_unadjusted`. The summary column of `top` has no row
of its own: its proportion is the sum of those of the levels it names.

`glance()` returns one row per item: `variable`, `label`, `test_type`,
`statistic`, `df`, `p.value`, `p_unadjusted`, `nobs`, `nobs_valid`,
`weighted_nobs` and `weighted_nobs_valid`. The counts are rows, never
weights, a row of zero weight left out, summed over the groups:

- `nobs` counts the rows of the item in the table, the base under
  `percent_base = "total"`: its answers on the levels shown, and its
  missing answers under `drop_na = FALSE`;

- `nobs_valid` counts its answers on the levels shown, the base under
  `percent_base = "valid"`; it is `nobs` under `drop_na = TRUE`;

- `weighted_nobs` and `weighted_nobs_valid` are the weighted counts of
  the same rows (under the `weights` of
  [`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md),
  the sum of the sampling weights in
  [`table_battery_svy()`](https://amaltawfik.github.io/spicy/reference/table_battery_svy.md),
  `NA` otherwise).

Both column sets are fixed: a test the table did not run is present and
`NA`. Index by name rather than by position.
