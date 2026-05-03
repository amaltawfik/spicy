# Tidying methods for a `spicy_categorical_table`

Standard
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
interfaces for an object returned by
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
They re-shape the underlying long-format data (stored on the object as
the `"long_data"` attribute) into the two canonical broom views so the
table can be consumed by `gtsummary`, `modelsummary`, `parameters`, and
any other tidyverse-stats pipeline.

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

A `tbl_df` (when `tibble` is installed) or a plain `data.frame`.

## Details

`tidy()` returns one row per `(variable x level)` – or per
`(variable x level x group)` when `by` is used – with broom-conventional
columns: `outcome`, `level`, `group` (when applicable), `n`,
`proportion` (the percentage divided by 100).

`glance()` returns one row per outcome with the omnibus chi-squared test
(when `by` is used) and the requested association measure: `outcome`,
`test_type` (`"chi_squared"`), `statistic` (chi-squared), `df`,
`p.value`, `assoc_type`, `assoc_value`, `assoc_ci_lower`,
`assoc_ci_upper`, `n_total`. Without `by`, only `outcome` and `n_total`
are populated; the other columns are `NA`.

## See also

[`as.data.frame.spicy_categorical_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_categorical_table.md)
for the raw wide-format access;
[`tidy.spicy_continuous_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_table.md)
for the continuous-descriptive companion.
