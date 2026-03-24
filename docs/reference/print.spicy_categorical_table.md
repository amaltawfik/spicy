# Print method for categorical summary tables

Formats and prints a `spicy_categorical_table` object as a styled ASCII
table using
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md).

## Usage

``` r
# S3 method for class 'spicy_categorical_table'
print(x, ...)
```

## Arguments

- x:

  A `data.frame` of class `"spicy_categorical_table"` as returned by
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  with `output = "default"` and `styled = TRUE`.

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
