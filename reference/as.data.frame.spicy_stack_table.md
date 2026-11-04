# Convert a `spicy_stack_table` to a data frame

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the data frames of the panels of a stacked table
([`as.data.frame.spicy_regression_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_regression_table.md))
bound by rows, with a leading `panel` column holding the label of the
panel: exactly what `output = "data.frame"` returns. The columns are the
union of the panels'; the note, as printed, is kept in `attr(, "note")`.
`as_tibble()` returns the same table as a tibble.

## Usage

``` r
# S3 method for class 'spicy_stack_table'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'spicy_stack_table'
as_tibble(x, ...)
```

## Arguments

- x:

  A `spicy_stack_table` returned by
  [`table_stack()`](https://amaltawfik.github.io/spicy/reference/table_stack.md).

- row.names, optional:

  Standard
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  arguments (ignored).

- ...:

  Ignored.

## Value

A `data.frame`
([`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)) or a
`tbl_df` (`as_tibble()`).

## See also

[`tidy.spicy_stack_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_stack_table.md)
