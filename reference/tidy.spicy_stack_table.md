# Tidy / glance methods for `spicy_stack_table`

The [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
outputs of the models of a stacked table
([`tidy.spicy_regression_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_regression_table.md)),
with a leading `panel` column holding the label of the panel of each
model.

## Usage

``` r
# S3 method for class 'spicy_stack_table'
tidy(x, ...)

# S3 method for class 'spicy_stack_table'
glance(x, ...)
```

## Arguments

- x:

  A `spicy_stack_table` returned by
  [`table_stack()`](https://amaltawfik.github.io/spicy/reference/table_stack.md).

- ...:

  Ignored.

## Value

A `tbl_df`.

## Details

For a stack of
[`table_stack()`](https://amaltawfik.github.io/spicy/reference/table_stack.md),
the outputs of the panels bound by rows: a row is keyed by `panel`,
`model_id` (the identifier of the model inside its panel), `term` and
`estimate_type`. For `table_regression(layout = "stacked")`, the outputs
of the same models side by side, `panel` aside: `model_id` is the
identifier of the model in the list, and a row is keyed by `model_id`,
`term` and `estimate_type`, as in
[`tidy.spicy_regression_table()`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_regression_table.md).
A multinomial model under `keep` or `drop` returns the rows its panel
shows, selected by its terms as in its table alone.
