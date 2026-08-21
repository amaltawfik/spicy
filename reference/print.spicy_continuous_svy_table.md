# Print method for continuous survey-design tables

Formats and prints a `spicy_continuous_svy_table` object as a styled
ASCII table using
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md).

## Usage

``` r
# S3 method for class 'spicy_continuous_svy_table'
print(x, ...)
```

## Arguments

- x:

  A `data.frame` of class `"spicy_continuous_svy_table"` as returned by
  [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md).

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md),
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
