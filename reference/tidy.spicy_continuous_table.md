# Tidying methods for a `spicy_continuous_table`

Standard
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
interfaces for an object returned by
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).
They re-shape the underlying long-format data into the two canonical
broom views so the descriptive table can be consumed by any downstream
tidyverse-stats pipeline.

## Usage

``` r
# S3 method for class 'spicy_continuous_table'
tidy(x, ...)

# S3 method for class 'spicy_continuous_table'
glance(x, ...)
```

## Arguments

- x:

  A `spicy_continuous_table` returned by
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).

- ...:

  Currently ignored. Present for compatibility with the
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) /
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  generics.

## Value

A `tbl_df`.

## Details

`tidy()` returns one row per `(variable x group)` (or per `variable`
when `by` is not used) with broom-conventional columns: `variable` (the
column name), `label` (the label the rows display for it, see `labels`),
`group` (`NA` without `by`), `.group_role` (the role of `group`:
`"group"`, or `"missing"` for the group of missing `by` values; `NA`
without `by`), `estimate` (the empirical mean), `std.error`
(`sd / sqrt(n)`), `conf.low`, `conf.high` (the mean confidence interval
at `ci_level`), `n`, `weighted_n` (the sum of the weights, `NA` without
`weights`), `median`, `q1`, `q3`, `min`, `max`, `sd`: the columns of
[`tidy.spicy_continuous_svy_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_svy_table.md),
the design ones (`df`, `deff`) aside.

`glance()` returns one row per variable with the omnibus group
comparison (when `by` is used), the requested effect size and the
balance diagnostic. Columns: `variable`, `label`, `test_type`,
`statistic`, `df`, `df.residual`, `p.value`, `es_type`, `es_value`,
`es_ci_lower`, `es_ci_upper`, `smd_type`, `smd_value`, `nobs`. Without
`by`, only `variable`, `label`, and `nobs` are populated; the other
columns are `NA`. The schema is fixed: `smd_type` / `smd_value` are
present whether or not `smd = TRUE`, like every other comparison column
here, and are `NA` when the table carries no standardized mean
difference, as in
[`glance.spicy_categorical_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_table.md).
Index this frame by NAME rather than by position.

## See also

[`as.data.frame.spicy_continuous_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_table.md)
for the displayed table as a data frame, and `output = "long"` of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
for the long format;
[`tidy.spicy_continuous_lm_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_lm_table.md)
for the model-based companion.
