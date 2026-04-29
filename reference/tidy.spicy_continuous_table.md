# Tidying methods for a `spicy_continuous_table`

Standard
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)
and
[`broom::glance()`](https://broom.tidymodels.org/reference/reexports.html)
interfaces for an object returned by
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).
They re-shape the underlying long-format data into the two canonical
broom views so the descriptive table can be consumed by `gtsummary`,
`modelsummary`, `parameters`, and any other tidyverse-stats pipeline.

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
  [`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)
  /
  [`broom::glance()`](https://broom.tidymodels.org/reference/reexports.html)
  generics.

## Value

A `tbl_df` (when `tibble` is installed) or a plain `data.frame`.

## Details

`tidy()` returns one row per `(variable x group)` (or per `variable`
when `by` is not used) with broom-conventional columns: `outcome`,
`label`, `group` (when applicable), `estimate` (the empirical mean),
`std.error` (`sd / sqrt(n)`), `conf.low`, `conf.high` (the mean
confidence interval at `ci_level`), `n`, `min`, `max`, `sd`. The
`outcome` column carries the variable name and `label` the
human-readable label.

`glance()` returns one row per `variable` with the omnibus group
comparison (when `by` is used) and the requested effect size: `outcome`,
`label`, `test_type`, `statistic`, `df`, `df.residual`, `p.value`,
`es_type`, `es_value`, `es_ci_lower`, `es_ci_upper`, `n_total`. Without
`by`, only `outcome`, `label`, and `n_total` are populated; the other
columns are `NA`.

## See also

[`as.data.frame.spicy_continuous_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_table.md)
for the raw long-format access;
[`tidy.spicy_continuous_lm_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_lm_table.md)
for the model-based companion.
