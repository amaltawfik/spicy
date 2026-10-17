# Print method for multiple-response tables

Formats and prints a `spicy_multiresponse_table` object as a styled
ASCII table using
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md).

## Usage

``` r
# S3 method for class 'spicy_multiresponse_table'
print(x, ...)
```

## Arguments

- x:

  A `data.frame` of class `"spicy_multiresponse_table"` as returned by
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)
  with `output = "default"`.

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md),
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
