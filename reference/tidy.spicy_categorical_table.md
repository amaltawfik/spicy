# Tidying methods for a `spicy_categorical_table`

Standard
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
interfaces for an object returned by
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
They re-shape the underlying long-format data (stored on the object as
the `"long_data"` attribute) into the two canonical broom views so the
table can be consumed by any downstream tidyverse-stats pipeline.

## Usage

``` r
# S3 method for class 'spicy_categorical_table'
tidy(x, ...)

# S3 method for class 'spicy_categorical_table'
glance(x, ...)
```

## Arguments

- x:

  A `spicy_categorical_table` returned by
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).

- ...:

  Currently ignored. Present for compatibility with the
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) /
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  generics.

## Value

A `tbl_df`.

## Details

`tidy()` returns one row per `(variable x level x group)`, the groups of
`by` and the `Total` margin included, with columns `variable` (the
column name), `label` (the label the rows display for it, see `labels`),
`level`, `.row_role` (the role of `level`: `"missing"` on the row of
missing values the table adds, `"total"` on the Total row of
`total_row = TRUE`, `"level"` otherwise), `group` (`NA` without `by`),
`.group_role` (the role of `group`: `"missing"` on the column of missing
`by` values, `"total"` on the margin, `"group"` otherwise, `NA` without
`by`), `total` (`TRUE` on the rows of the margin and of the Total row),
`n`, and `proportion` (between 0 and 1, the share the table displays:
within the group, or within the level under `percent = "row"`, as
`attr(x, "percent")` records). Filter on the role columns and `total`
rather than on the displayed labels, which follow the display language.

`glance()` returns one row per variable, in the order of `select`, with
its test (when `by` is used), the requested association measure and the
standardized mean difference: `variable`, `label`, `test_type`
(`"chi_squared"`, `"n1_chi_squared"`, `"fisher_exact"`, `"fisher_midp"`,
or `"fisher_exact_mc"`, as in the long output of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)),
`statistic` (the chi-squared, Pearson's or the N-1, `NA` for an exact
test), `df`, `p.value`, `es_type` (the key of the measure the variable
carries: `"phi"`, `"cramer_v"`, ...), `es_value`, `es_ci_lower`,
`es_ci_upper`, `smd_type`, `smd_value`, and `nobs` (the observations of
the variable, the margin and the Total row not counted twice). Without
`by`, only `variable`, `label` and `nobs` are populated; the other
columns are `NA`. The interval bounds are `NA` unless the table was
built with `assoc_ci = TRUE`, and the SMD columns unless it was built
with `smd = TRUE`.

## See also

[`as.data.frame.spicy_categorical_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_categorical_table.md)
for the raw wide-format access;
[`tidy.spicy_continuous_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_table.md)
for the continuous-descriptive companion.
