# Print method for descriptive statistics tables

Formats and prints a `spicy_desc_table` object as a styled ASCII table
using
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md).

## Usage

``` r
# S3 method for class 'spicy_desc_table'
print(x, ...)
```

## Arguments

- x:

  A `data.frame` of class `"spicy_desc_table"` as returned by
  [`table_desc()`](https://amaltawfik.github.io/spicy/reference/table_desc.md).

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[`table_desc()`](https://amaltawfik.github.io/spicy/reference/table_desc.md),
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
