# Print method for regression tables

Formats and prints a `spicy_regression_table` object as a styled ASCII
table: title banner, spanner row for multi-model tables, decimal-aligned
body with factor grouping and reference rows, fit-statistics block, and
footer note.

## Usage

``` r
# S3 method for class 'spicy_regression_table'
print(x, ...)
```

## Arguments

- x:

  A `data.frame` of class `"spicy_regression_table"` as returned by
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  with `output = "default"`.

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
