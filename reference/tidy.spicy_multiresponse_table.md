# Tidying methods for a `spicy_multiresponse_table`

Standard
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
interfaces for an object returned by
[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md).

## Usage

``` r
# S3 method for class 'spicy_multiresponse_table'
tidy(x, ...)

# S3 method for class 'spicy_multiresponse_table'
glance(x, ...)
```

## Arguments

- x:

  A `spicy_multiresponse_table` returned by
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md).

- ...:

  Ignored, for S3 compatibility.

## Value

A `tbl_df` (or `data.frame` when tibble is not installed).

## Details

`tidy()` returns one row per (item x group), the margin included and
flagged, and the items only: the rows of `none_row` and the base row are
left out. Columns: `variable` (the item's column), `label` (its
displayed label), `group` (the `by` level or the margin, `NA` without
`by`), `.group_role` (the role of `group`: `"group"` or `"total"`, `NA`
without `by`), `total` (whether the row is the margin), `n` (the
respondents who chose the item), `base` (the respondents of the group's
base), `proportion_cases` (`n / base`, the share of respondents),
`proportion_responses` (`n` over the choices made in the group), and the
item's test: `test_type` (`"chi_squared"`, `"n1_chi_squared"`,
`"fisher_exact"`, `"fisher_midp"` or `"fisher_exact_mc"`), `statistic`
(the chi-squared, Pearson's or the N-1, `NA` for an exact test), `df`,
`p.value` (adjusted under `p_adjust`) and `p_unadjusted`.

`glance()` returns one row for the question. Columns: `n_items`,
`n_items_tested` (the items the global test kept), `n_groups` (the
groups of `by`, `NA` without it), `nobs` (the respondents of the base),
`test_type` (`"rs2"` or `"bootstrap"`, from `global_test`), `statistic`,
`df`, `p.value` and `boot_n` of the global test, `rs_cv` (the \\\hat a\\
of the second-order Rao-Scott correction, see *Tests* in
[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)),
and `p_adjust`, the adjustment of the item p-values.

Both column sets are FIXED: a column the table does not show (a test it
did not run, an adjustment it did not make) is present and `NA`. Index
by name rather than by position.
