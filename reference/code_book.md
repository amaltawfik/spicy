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
  factor_levels = c("all", "observed"),
  user_na = TRUE
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
  explicitly appended at the end of the `Values` summary when present in
  the variable. This applies to all variable types. Literal strings
  `"NA"`, `"NaN"`, and `""` are quoted to distinguish them from missing
  markers. If `FALSE` (the default), missing values are omitted from
  `Values` but still counted in the `NAs` column.

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

- user_na:

  Logical. If `TRUE` (the default), declared missing values count as
  missing in `N_valid`, `NAs`, and `N_distinct` (all three columns share
  one missing definition). If `FALSE`, they count as valid. Either way,
  the declared codes remain listed in `Values` (with their value labels
  when declared) – a codebook documents the full coding scheme. See the
  "Declared missing values" section of
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).

## Value

A [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) object.

## Details

- The interactive `datatable` supports column sorting, global searching,
  and client-side export (copy, print, CSV, Excel, PDF) directly from
  the Viewer.

- Variable selection uses the same tidyselect interface as
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md);
  the underlying summary tibble is built by
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  with `tbl = TRUE`.

## Dependencies

Requires the following package:

- **DT**

## Declared missing values

Survey files imported with **haven** often carry *declared missing
values*: codes such as `8 = Don't know` or `9 = Refused` that the source
file marks as missing while keeping them distinct from a plain `NA`. Two
kinds of declaration exist: `na_values` / `na_range` metadata on
[`haven::labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html)
vectors, and tagged missing values created by
[`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
(the Stata `.a`, `.b`, ... convention).

spicy honors the declaration by default (`user_na = TRUE`): declared
missing values are excluded from every statistic exactly like `NA` –
valid percentages, means, chi-squared tests, association measures,
row-wise summaries, and group definitions – but they are not erased from
display.
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) lists
each observed declared value as its own row of the Missing block, with
its value label;
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
and
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
disclose the exclusion in the table note
(`Declared missing values removed: x (2).`);
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
and `code_book()` count them as missing in `N_valid` / `NAs` /
`N_distinct` while still listing the declared codes in `Values`.

Every function involved offers the same escape hatch: set
`user_na = FALSE` to ignore the declaration and treat the declared codes
as valid values (the behavior of spicy before 0.13.0). Tagged missing
values are genuine `NA`s either way; for them, `user_na = FALSE` only
collapses the per-tag breakdown back into the regular `NA` count.

## See also

[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
for generating the underlying variable summaries.

Other variable inspection:
[`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md),
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)

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
