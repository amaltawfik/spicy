# Print method for stacked tables

Prints a `spicy_stack_table` returned by
[`table_stack()`](https://amaltawfik.github.io/spicy/reference/table_stack.md)
or by `table_regression(layout = "stacked")` as an ASCII table: the
title, each panel opened by its label row (and by its own header rows
when the panels differ in their columns), and the merged note.

## Usage

``` r
# S3 method for class 'spicy_stack_table'
print(x, ...)
```

## Arguments

- x:

  A `spicy_stack_table`.

- ...:

  Passed to
  [`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
  (e.g. `padding`).

## Value

Invisibly returns `x`.

## See also

[`table_stack()`](https://amaltawfik.github.io/spicy/reference/table_stack.md)
