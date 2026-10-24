# Coerce a `spicy_categorical_svy_table` to a data frame or tibble

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the table
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
displays, with its numbers: exactly what `output = "data.frame"`
returns. One row per printed row – the header row of each variable,
which carries its *p*-value, then one row per level – and the printed
columns under their names (`"Female n"`, `"Female %"`, the interval
bounds `"Female 95% CI LL"` and `"Female 95% CI UL"`, `"Female DEff"`,
..., `"p"`), plus the key columns `variable`, `label`, `level` and
`.row_role` (`"factor_header"`, `"level"`, or `"missing"`). The note, as
printed, is kept in `attr(, "note")`. `as_tibble()` returns the same
table as a tibble.

## Usage

``` r
# S3 method for class 'spicy_categorical_svy_table'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spicy_categorical_svy_table'
as_tibble(x, ...)
```

## Arguments

- x:

  A `spicy_categorical_svy_table` returned by
  [`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md).

- row.names, optional, ...:

  Passed on for method compatibility; ignored.

## Value

A plain `data.frame` (or a `tbl_df` for `as_tibble()`), identical to the
`output = "data.frame"` of the same call.

## See also

[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md).
