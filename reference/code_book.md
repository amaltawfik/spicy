# Generate an interactive variable codebook

`code_book()` creates an interactive and exportable codebook summarizing
selected variables of a data frame. It builds upon
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
to provide an overview of variable names, labels, classes, and
representative values in a sortable, searchable table.

The output is displayed as an interactive
[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) in the
Viewer pane (for example in RStudio or Positron), allowing searching,
sorting, and export (copy, print, CSV, Excel, PDF) directly.

## Usage

``` r
code_book(
  x,
  ...,
  values = FALSE,
  include_na = FALSE,
  title = "Codebook",
  filename = NULL,
  factor_levels = c("all", "observed")
)
```

## Arguments

- x:

  A data frame or tibble.

- ...:

  Optional tidyselect-style column selectors (e.g. `starts_with("var")`,
  `where(is.numeric)`, etc.). Columns can be selected or reordered, but
  renaming selections is not supported.

- values:

  Logical. If `FALSE` (the default), displays a compact summary of the
  variable's values. For numeric, character, date/time, labelled, and
  factor variables, all unique non-missing values are shown when there
  are at most four; otherwise the first three values, an ellipsis
  (`...`), and the last value are shown. Values are sorted when
  appropriate (e.g., numeric, character, date). For factors,
  `factor_levels` controls whether observed or all declared levels are
  shown; level order is preserved. For labelled variables, prefixed
  labels are displayed via `labelled::to_factor(levels = "prefixed")`.
  If `TRUE`, all unique non-missing values are displayed.

- include_na:

  Logical. If `TRUE`, unique missing value markers (`<NA>`, `<NaN>`) are
  appended at the end of the `Values` summary when present in the
  variable. This applies to all variable types. Literal strings `"NA"`,
  `"NaN"`, and `""` are quoted to distinguish them from missing markers.
  If `FALSE` (the default), missing values are omitted from `Values` but
  still counted in the `NAs` column.

- title:

  Optional character string displayed as the table caption. Defaults to
  `"Codebook"`. Set to `NULL` to remove the title completely. When
  `filename = NULL`, the title is also used as the base for export
  filenames after conversion to a portable ASCII name.

- filename:

  Optional character string used as the base for exported CSV, Excel,
  and PDF filenames. If `NULL` (the default), a portable filename is
  derived from `title`, falling back to `"Codebook"` when needed. File
  extensions are added by the browser/export engine.

- factor_levels:

  Character. Controls how factor values are displayed in `Values`.
  `"all"` (the default;
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  uses `"observed"`) shows all declared levels, including unused levels.
  `"observed"` shows only levels present in the data, preserving factor
  level order.

## Value

A [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) object.

## Details

- The interactive `datatable` supports column sorting, global searching,
  and client-side export to various formats.

- Variable selection uses the same tidyselect interface as
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md).

- By default, factor variables document all declared levels, including
  unused levels — appropriate for a schema-oriented codebook. This
  differs from
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md),
  which defaults to `"observed"` to summarize observed data only. Pass
  `factor_levels = "observed"` to mirror
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)'s
  default.

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
  code_book(sochealth, starts_with("bmi"))
  code_book(sochealth, starts_with("bmi"), values = TRUE, include_na = TRUE)

  factors <- data.frame(
    group = factor(c("A", "B", NA), levels = c("A", "B", "C"))
  )
  code_book(
    factors,
    values = TRUE,
    include_na = TRUE,
    factor_levels = "observed"
  )

  code_book(
    sochealth,
    starts_with("bmi"),
    title = "BMI codebook",
    filename = "bmi_codebook"
  )
}
} # }
```
