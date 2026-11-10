# Print method for item battery tables

Formats and prints a `spicy_battery_table` object as a styled ASCII
table using
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md).
A table wider than the console is printed in panels, the item labels
repeated in each.

## Usage

``` r
# S3 method for class 'spicy_battery_table'
print(x, ...)
```

## Arguments

- x:

  A `data.frame` of class `"spicy_battery_table"` as returned by
  [`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md)
  with `output = "default"`.

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md),
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
