# Generate an interactive variable codebook

`code_book()` creates an interactive and exportable codebook summarizing
variables of a data frame. It builds upon
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
to provide an overview of variable names, labels, classes, and
representative values in a sortable, searchable table.

The output is displayed as an interactive
[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) in the
Viewer pane, allowing searching, sorting, and export (copy, print, CSV,
Excel, PDF) directly.

## Usage

``` r
code_book(
  x,
  values = FALSE,
  include_na = FALSE,
  title = "Codebook",
  factor_levels = c("all", "observed"),
  ...
)
```

## Arguments

- x:

  A data frame or tibble.

- values:

  Logical. If `FALSE` (the default), displays a compact summary of the
  variable's values. For numeric, character, date/time, labelled, and
  factor variables, up to four unique non-missing values are shown: the
  first three values, followed by an ellipsis (`...`), and the last
  value. Values are sorted when appropriate (e.g., numeric, character,
  date). For factors, `factor_levels` controls whether observed or all
  declared levels are shown; level order is preserved. For labelled
  variables, prefixed labels are displayed via
  `labelled::to_factor(levels = "prefixed")`. If `TRUE`, all unique
  non-missing values are displayed.

- include_na:

  Logical. If `TRUE`, unique missing value markers (`<NA>`, `<NaN>`) are
  appended at the end of the `Values` summary when present in the
  variable. This applies to all variable types. Literal strings `"NA"`,
  `"NaN"`, and `""` are quoted to distinguish them from missing markers.
  If `FALSE` (the default), missing values are omitted from `Values` but
  still counted in the `NAs` column.

- title:

  Optional character string displayed as the table title in the Viewer.
  Defaults to `"Codebook"`. Set to `NULL` to remove the title
  completely.

- factor_levels:

  Character. Controls how factor values are displayed in `Values`.
  `"all"` (the default) shows all declared levels, including unused
  levels. `"observed"` shows only levels present in the data, preserving
  factor level order.

- ...:

  Additional arguments (currently unused).

## Value

A [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) object.

## Details

- The interactive `datatable` supports column sorting, global searching,
  and client-side export to various formats.

- All exports occur client-side through the Viewer or Tab.

## Dependencies

Requires the following package:

- **DT**

## See also

[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
for generating the underlying variable summaries.

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("DT", quietly = TRUE)) {
  code_book(sochealth)
}
} # }
```
