# Tidying methods for a `spicy_categorical_svy_table`

Standard
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
interfaces for an object returned by
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md).

## Usage

``` r
# S3 method for class 'spicy_categorical_svy_table'
tidy(x, ...)

# S3 method for class 'spicy_categorical_svy_table'
glance(x, ...)
```

## Arguments

- x:

  A `spicy_categorical_svy_table` returned by
  [`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md).

- ...:

  Ignored, for S3 compatibility.

## Value

A `tbl_df` (or `data.frame` when tibble is not installed).

## Details

`tidy()` returns one row per (variable x level x column block), which is
the LONG reading of a table whose blocks are columns, in the order of
the table (variable, then level, then block). Columns: `variable`,
`label`, `level`, `.row_role` (the role of `level`: `"missing"` on the
row of missing values the table adds, `"level"` otherwise), `group` (the
`by` level, or the margin, `NA` without `by`), `.group_role` (the role
of `group`: `"group"`, `"missing"` for the column of missing `by`
values, `"total"` for the margin, `NA` without `by`), `total` (whether
that block is the margin), `n` (observed), `proportion` (the estimated
proportion, between 0 and 1), `conf.low`, `conf.high` (its interval, on
the same scale), `deff`. The header rows carry no level statistic and do
not appear; their p-value is what `glance()` is for.

`glance()` returns one row per variable: `variable`, `label`,
`n_levels`, `test_type` (the `svychisq()` statistic of the test,
`chisq_statistic`; `NA` where no test ran), `p.value`, `degf` (the
design's own), `nobs`, `nobs_valid`, `weighted_nobs` and
`weighted_nobs_valid`. The four counts leave out the rows of zero
weight, and `levels_keep` changes none of them:

- `nobs` counts the rows of the variable in the table, unweighted, the
  base of the estimates under `percent_base = "total"`: every row under
  `drop_na = FALSE`, those of the `(Missing)` row included, and the
  observed values under `drop_na = TRUE`;

- `nobs_valid` counts the rows with an observed value, the base under
  `percent_base = "valid"`; it is `nobs` under `drop_na = TRUE`;

- `weighted_nobs` and `weighted_nobs_valid` are the sums of the sampling
  weights of the same rows.

`n_levels` counts the levels the TABLE displays, so a `(Missing)`
display level counts; the test behind `p.value` runs on the complete
cases and the observed levels only, as it does in
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
