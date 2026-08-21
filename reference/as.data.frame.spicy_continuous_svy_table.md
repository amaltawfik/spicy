# Coerce a `spicy_continuous_svy_table` to a plain data frame or tibble

These S3 methods strip the `"spicy_continuous_svy_table"` class and the
rendering-only attributes, keeping the long compute frame and the three
provenance markers (`group_var`, `design_meta`, `note`).

## Usage

``` r
# S3 method for class 'spicy_continuous_svy_table'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spicy_continuous_svy_table'
as_tibble(x, ...)
```

## Arguments

- x:

  A `spicy_continuous_svy_table` returned by
  [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md).

- row.names, optional, ...:

  Passed on for method compatibility; ignored.

## Value

A plain `data.frame` (or a `tbl_df` for `as_tibble()`).

## See also

[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md).
