# Print method for spicy_cross_table objects

Prints a formatted SPSS-like crosstable created by
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).

## Usage

``` r
# S3 method for class 'spicy_cross_table'
print(x, digits = NULL, decimal_mark = NULL, ...)
```

## Arguments

- x:

  A `spicy_cross_table` object.

- digits:

  Optional integer; number of decimal places to display for cell values.
  Defaults to the value stored in the object.

- decimal_mark:

  Optional character (`"."` or `","`) used as the decimal mark. Defaults
  to the value stored in the object.

- ...:

  Additional arguments passed to internal formatting functions.
