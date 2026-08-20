# Print method for outcome tables

Formats and prints a `spicy_outcome_table` object as a styled ASCII
table using
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md).

## Usage

``` r
# S3 method for class 'spicy_outcome_table'
print(x, ...)
```

## Arguments

- x:

  A `data.frame` of class `"spicy_outcome_table"` as returned by
  [`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md).

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md),
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
