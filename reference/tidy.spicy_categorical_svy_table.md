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
the LONG reading of a table whose blocks are columns. Columns:
`variable`, `label`, `level`, `group` (the `by` level, or the margin,
`NA` without `by`), `total` (whether that block is the margin), `n`
(observed), `estimate` (the estimated percentage), `conf.low`,
`conf.high`, `deff`. The header rows carry no level statistic and do not
appear; their p-value is what `glance()` is for.

`glance()` returns one row per variable: `variable`, `label`,
`n_levels`, `p.value`, `statistic_type` (the `svychisq()` statistic
asked for), `degf` (the design's own), `nobs`, `weighted.nobs`.

`n_levels` counts the levels the TABLE displays, so a `(Missing)`
display level counts; the test behind `p.value` runs on the complete
cases and the observed levels only, as it does in
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
