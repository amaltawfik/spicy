# Tidying methods for a `spicy_outcome_table`

Standard
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
interfaces for an object returned by
[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md).

## Usage

``` r
# S3 method for class 'spicy_outcome_table'
tidy(x, ...)

# S3 method for class 'spicy_outcome_table'
glance(x, ...)
```

## Arguments

- x:

  A `spicy_outcome_table` returned by
  [`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md).

- ...:

  Ignored, for S3 compatibility.

## Value

A `tbl_df` (or `data.frame` when tibble is not installed).

## Details

`tidy()` returns the DESCRIBED rows: the marginal Overall row and one
row per (grouping x level). Columns: `outcome` (the outcome name,
constant down the frame), `variable` (the grouping, or the outcome
itself on the marginal row), `label`, `level` (`NA` on the marginal
row), `estimate` (the mean), `std.error` (`sd / sqrt(n)`), `conf.low`,
`conf.high`, `n`, `min`, `max`, `sd`.

Two identity columns where the sibling has one, and deliberately: here
the outcome is fixed and the variable changes, so a single `outcome`
column would have to mean two different things down the frame. The
schema reads without knowing which function produced it.

`glance()` returns one row per grouping – one BLOCK – with that block's
own comparison. Columns: `outcome`, `variable`, `label`, `n_levels`,
`test_type`, `statistic`, `df`, `df.residual`, `p.value`, `es_type`,
`es_value`, `es_ci_lower`, `es_ci_upper`, `smd_type`, `smd_value`,
`n_total`.

`n_levels` counts the levels the TABLE displays, so a missing-value
display level counts; the comparison behind `test_type` runs on the
observed levels only, as it does everywhere in the family.

The schema is FIXED: `smd_type` / `smd_value` are present and `NA` from
the first version, so the day a standardized mean difference enters this
table it cannot break a pipeline that indexes the frame. Index by NAME
rather than by position.
