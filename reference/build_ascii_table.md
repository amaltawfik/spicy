# Build a formatted ASCII table

Low-level internal function that constructs a visually aligned ASCII
table from a `data.frame`. It supports Unicode characters, ANSI colors,
dynamic width adjustment, left/right alignment, and spacing control.
This function is primarily used internally by higher-level wrappers such
as
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
or
[`print.spicy_freq_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_freq_table.md).

## Usage

``` r
build_ascii_table(
  x,
  padding = c("compact", "normal", "wide"),
  first_column_line = TRUE,
  row_total_line = TRUE,
  column_total_line = TRUE,
  bottom_line = FALSE,
  lines_color = "darkgrey",
  align_left_cols = c(1L, 2L),
  align_center_cols = integer(0),
  group_sep_rows = integer(0),
  ...
)
```

## Arguments

- x:

  A `data.frame` or `spicy_table` object containing the table to format.
  Typically, this includes columns such as *Category*, *Values*,
  *Freq.*, *Percent*, etc.

- padding:

  Character string controlling horizontal spacing between columns:

  - `"compact"` — minimal spacing

  - `"normal"` — moderate spacing (default)

  - `"wide"` — extra spacing (for large displays or wide content)

- first_column_line:

  Logical. If `TRUE` (the default), a vertical separator is drawn after
  the first column (useful for separating categories from data).

- row_total_line, column_total_line:

  Logical. Control horizontal rules before total rows or columns. Both
  default to `TRUE`.

- bottom_line:

  Logical. If `FALSE` (the default), no closing line is drawn. If
  `TRUE`, draws a closing line at the bottom of the table.

- lines_color:

  Character. Color used for table separators. Defaults to `"darkgrey"`.
  The color is applied only when ANSI color support is available (see
  [`crayon::has_color()`](http://r-lib.github.io/crayon/reference/has_color.md)).

- align_left_cols:

  Integer vector of column indices to left-align. Defaults to `c(1, 2)`
  for frequency tables (Category + Values).

- align_center_cols:

  Integer vector of column indices to center-align. Defaults to
  `integer(0)` (no centered columns). Columns not in `align_left_cols`
  or `align_center_cols` are right-aligned.

- ...:

  Additional arguments (currently ignored).

## Value

A single character string containing the full ASCII-formatted table,
suitable for direct printing with
[`cat()`](https://rdrr.io/r/base/cat.html).

## Details

`build_ascii_table()` is the rendering engine that produces the aligned
text layout of **spicy-formatted tables**. It automatically detects cell
widths (including colored text), inserts Unicode separators, and applies
padding for different display modes (`"compact"`, `"normal"`, `"wide"`).

For most users, this function should not be called directly. Instead,
use
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
which adds headers, notes, and alignment logic automatically.

## See also

[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
for a user-facing wrapper that adds titles and notes.

## Examples

``` r
# Internal usage example (for developers)
df <- data.frame(
  Category = c("Valid", "", "Missing", "Total"),
  Values = c("Yes", "No", "NA", ""),
  Freq. = c(12, 8, 1, 21),
  Percent = c(57.1, 38.1, 4.8, 100.0)
)

cat(build_ascii_table(df, padding = "compact"))
#>  Category │ Values  Freq.  Percent 
#> ──────────┼────────────────────────
#>  Valid    │ Yes        12     57.1 
#>           │ No          8     38.1 
#>  Missing  │ NA          1      4.8 
#> ──────────┼────────────────────────
#>  Total    │            21      100 
```
