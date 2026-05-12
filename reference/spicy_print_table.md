# Print a spicy-formatted ASCII table

User-facing helper that prints a spicy-styled ASCII table to the console
with optional title and note, table-type-aware alignment defaults, and
automatic horizontal panelling when the table is wider than the console.
Wraps the internal renderer
[`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md).

## Usage

``` r
spicy_print_table(
  x,
  title = attr(x, "title"),
  note = attr(x, "note"),
  padding = 2L,
  first_column_line = TRUE,
  row_total_line = TRUE,
  column_total_line = TRUE,
  bottom_line = FALSE,
  lines_color = "darkgrey",
  align_left_cols = NULL,
  align_center_cols = integer(0),
  center_headers = FALSE,
  spanners = NULL,
  group_sep_rows = integer(0),
  total_row_idx = attr(x, "total_row_idx"),
  ...
)
```

## Arguments

- x:

  A `spicy_table` or `data.frame` to be printed.

- title:

  Optional title displayed above the table. Defaults to the `"title"`
  attribute of `x` if present.

- note:

  Optional note displayed below the table. Defaults to the `"note"`
  attribute of `x` if present.

- padding:

  Non-negative integer giving the number of extra characters added to
  each column's auto-computed width (max of cell-content width and
  header width). Defaults to `2L`. See
  [`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md)
  for the precise formula and the migration note from the pre-0.11.0
  string enum.

- first_column_line:

  Logical. If `TRUE` (the default), adds a vertical separator after the
  first column.

- row_total_line, column_total_line, bottom_line:

  Logical flags controlling the presence of horizontal lines before
  total rows/columns or at the bottom of the table. Both
  `row_total_line` and `column_total_line` default to `TRUE`;
  `bottom_line` defaults to `FALSE`.

- lines_color:

  Character. Color for table separators. Defaults to `"darkgrey"`. Only
  applied if the output supports ANSI colors (see
  [`crayon::has_color()`](http://r-lib.github.io/crayon/reference/has_color.md)).

- align_left_cols:

  Integer vector of column indices to left-align. If `NULL` (the
  default), alignment is auto-detected based on `x`:

  - For `freq` tables -\> `c(1, 2)`

  - For `cross` tables -\> `1`

- align_center_cols:

  Integer vector of column indices to center-align. Defaults to
  `integer(0)`.

- center_headers:

  Logical. When `TRUE`, column headers are centered above their column
  content even when the data itself is right-aligned. Passed through to
  [`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md).
  Defaults to `FALSE`.

- spanners:

  Optional named list of column-group labels (label -\> integer column
  indices). Passed through to
  [`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md);
  when the table is split into horizontal panels each panel keeps only
  the spanners whose columns are fully contained in it. Defaults to
  `NULL` (no spanner row).

- group_sep_rows:

  Integer vector of row indices before which a light dashed separator
  line is drawn. Defaults to `integer(0)`.

- total_row_idx:

  Optional integer vector of 1-based row indices identifying the totals
  rows; defaults to the `"total_row_idx"` attribute of `x` (set by
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)).
  See
  [`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md).

- ...:

  Additional arguments passed to
  [`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md).

## Value

Invisibly returns `x`, after printing the formatted ASCII table to the
console.

## Details

Table type is auto-detected from `x` and drives the default alignment
when `align_left_cols = NULL`:

- **frequency table** (a `Category` column is present): the first two
  columns (`Category`, `Values`) are left-aligned.

- **cross table** (otherwise): only the first column (row variable) is
  left-aligned.

If the table is wider than the console, it is split into stacked
horizontal panels with the left-most identifier columns repeated on each
panel. Unicode line-drawing characters are used by default; coloured
separators are drawn when the terminal supports ANSI colour
([`crayon::has_color()`](http://r-lib.github.io/crayon/reference/has_color.md))
and fall back to monochrome otherwise.

## See also

[`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md)
for the underlying text rendering engine.
[`print.spicy_freq_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_freq_table.md)
for the specialized printing method used by
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).

## Examples

``` r
# Simple demonstration
df <- data.frame(
  Category = c("Valid", "", "Missing", "Total"),
  Values = c("Yes", "No", "NA", ""),
  Freq. = c(12, 8, 1, 21),
  Percent = c(57.1, 38.1, 4.8, 100.0)
)

spicy_print_table(df,
  title = "Frequency table: Example",
  note = "Class: data.frame\nData: demo"
)
#> Frequency table: Example
#> 
#>  Category   │ Values      Freq.    Percent 
#> ────────────┼──────────────────────────────
#>  Valid      │ Yes            12       57.1 
#>             │ No              8       38.1 
#>  Missing    │ NA              1        4.8 
#> ────────────┼──────────────────────────────
#>  Total      │                21        100 
#> 
#> Class: data.frame
#> Data: demo
```
