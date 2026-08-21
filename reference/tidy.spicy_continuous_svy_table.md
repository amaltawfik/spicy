# Tidying methods for a `spicy_continuous_svy_table`

Standard
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
interfaces for an object returned by
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md).

## Usage

``` r
# S3 method for class 'spicy_continuous_svy_table'
tidy(x, ...)

# S3 method for class 'spicy_continuous_svy_table'
glance(x, ...)
```

## Arguments

- x:

  A `spicy_continuous_svy_table` returned by
  [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md).

- ...:

  Ignored, for S3 compatibility.

## Value

A `tbl_df` (or `data.frame` when tibble is not installed).

## Details

`tidy()` returns one row per displayed row: one per variable, or one per
(variable x group) with `by`. Columns: `variable`, `label`, `group`
(`NA` without `by`), `estimate` (the mean), `std.error` (the
design-based standard error, from
[`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
and never recomputed from `sd / sqrt(n)` – under a design those are
different quantities), `conf.low`, `conf.high`, `df` (the degrees of
freedom the interval used), `n` (observed), `weighted.n` (the sum of the
sampling weights), `median`, `q1`, `q3`, `min`, `max`, `sd`, `deff`.

`glance()` returns one row per variable with its group comparison:
`variable`, `label`, `n_groups`, `test_type`, `statistic`, `df`,
`df.residual`, `p.value`, `degf` (the design's own), `nobs`,
`weighted.nobs`. One row per variable even without `by`, where the
comparison columns are `NA` – a fixed schema a pipeline can index into
by NAME.
