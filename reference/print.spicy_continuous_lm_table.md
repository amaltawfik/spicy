# Print method for bivariate linear-model tables

Formats and prints a `spicy_continuous_lm_table` object as a styled
ASCII table using
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md).

## Usage

``` r
# S3 method for class 'spicy_continuous_lm_table'
print(x, ...)
```

## Arguments

- x:

  A `data.frame` of class `"spicy_continuous_lm_table"` as returned by
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
