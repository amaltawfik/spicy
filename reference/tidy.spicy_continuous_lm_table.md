# Tidying methods for a `spicy_continuous_lm_table`

Standard
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
interfaces for an object returned by
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
They re-shape the underlying long-format data into the two canonical
broom views so the table can be consumed by any downstream
tidyverse-stats pipeline.

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

A `tbl_df`.

## Details

`tidy()` returns one row per **estimated parameter** across all
outcomes:

- One row per fitted level mean (`estimate_type = "emmean"`) for
  categorical predictors, with the level name in `term`, `NA` where no
  mean is estimated (a level in which the outcome has no data, an
  outcome whose model could not be fit).

- One row per **contrast** (`estimate_type = "difference"`) when a
  binary contrast is shown, with `term = "<level2> - <level1>"`.

- One row per **slope** (`estimate_type = "slope"`) for numeric
  predictors, with `term = predictor_label`.

Standard broom columns: `variable`, `label`, `term`, `estimate_type`,
`estimate`, `std.error`, `conf.low`, `conf.high`, `statistic`,
`p.value`. The `variable` column carries the column name of the outcome;
`label` carries the label the rows display for it (see `labels`).

`glance()` returns one row per outcome with model-level statistics.
Columns: `variable`, `label`, `predictor_type` (`"categorical"` or
`"continuous"`), `test_type` (the test the table displays: `"t"` for a
numeric `by` and for the contrast of a two-level `by`, `"F"` for a `by`
of three or more levels or under `contrast = "none"`, and `"z"` or
`"chi2"` in their place under `vcov = "bootstrap"` or `"jackknife"`),
`statistic`, `df`, `df.residual` (the degrees of freedom follow the
statistic: an *F* has both, a *t* has its own in `df` and none in
`df.residual`), `p.value`, `r.squared`, `adj.r.squared`, `es_type`,
`es_value`, `es_ci_lower`, `es_ci_upper`, `nobs`, `weighted_nobs` (the
sum of the weights, `NA` without `weights`). Beside the contrast of a
two-level `by`, the *F* of the model, the square of its *t*, is in the
long output only.

## See also

[`as.data.frame.spicy_continuous_lm_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_lm_table.md)
for the displayed table as a data frame, and `output = "long"` of
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
for every estimate in long format.
