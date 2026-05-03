# Tidying methods for a `spicy_continuous_lm_table`

Standard
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
interfaces for an object returned by
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
They re-shape the underlying long-format data into the two canonical
broom views so the table can be consumed by `gtsummary`, `modelsummary`,
`parameters`, and any other tidyverse-stats pipeline.

## Usage

``` r
# S3 method for class 'spicy_continuous_lm_table'
tidy(x, ...)

# S3 method for class 'spicy_continuous_lm_table'
glance(x, ...)
```

## Arguments

- x:

  A `spicy_continuous_lm_table` returned by
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).

- ...:

  Currently ignored. Present for compatibility with the
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) /
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  generics.

## Value

A `tbl_df` (when `tibble` is installed) or a plain `data.frame`.

## Details

`tidy()` returns one row per **estimated parameter** across all
outcomes:

- One row per fitted level mean (`estimate_type = "emmean"`) for
  categorical predictors, with the level name in `term`.

- One row per **contrast** (`estimate_type = "difference"`) when a
  binary contrast is shown, with `term = "<level2> - <level1>"`.

- One row per **slope** (`estimate_type = "slope"`) for numeric
  predictors, with `term = predictor_label`.

Standard broom columns: `outcome`, `label`, `term`, `estimate_type`,
`estimate`, `std.error`, `conf.low`, `conf.high`, `statistic`,
`p.value`. The `outcome` column carries the original variable name;
`label` carries the human-readable label.

`glance()` returns one row per outcome with model-level statistics:
`r.squared`, `adj.r.squared`, `statistic`, `df`, `df.residual`,
`p.value`, `nobs`, `weighted_n`, plus the effect-size summary `es_type`,
`es_value`, `es_ci_lower`, `es_ci_upper`, and the test type used for
`statistic` (`"F"` for categorical predictors, `"t"` for numeric ones).

## See also

[`as.data.frame.spicy_continuous_lm_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_lm_table.md)
for the raw long-format access.
