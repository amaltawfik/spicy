# Print a spicy-formatted ASCII table

User-facing helper that prints a visually aligned, spicy-styled ASCII
table created by functions such as
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) or
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
It automatically adjusts column alignment, spacing, and separators for
improved readability in console outputs.

This function wraps the internal renderer
[`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md),
adding optional titles, notes, and automatic alignment rules depending
on the type of table.

## Usage

``` r
spicy_print_table(
  x,
  title = attr(x, "title"),
  note = attr(x, "note"),
  padding = c("compact", "normal", "wide"),
  first_column_line = TRUE,
  row_total_line = TRUE,
  column_total_line = TRUE,
  bottom_line = FALSE,
  lines_color = "darkgrey",
  align_left_cols = NULL,
  align_center_cols = integer(0),
  group_sep_rows = integer(0),
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

  Character string controlling horizontal spacing between columns:

  - `"compact"` - minimal spacing

  - `"normal"` - moderate spacing (default)

  - `"wide"` - extra spacing (for wide displays)

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

- group_sep_rows:

  Integer vector of row indices before which a light dashed separator
  line is drawn. Defaults to `integer(0)`.

- ...:

  Additional arguments passed to
  [`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md).

## Value

Invisibly returns `x`, after printing the formatted ASCII table to the
console.

## Details

`spicy_print_table()` detects whether the table represents frequencies
(`freq`-style) or cross-tabulations (`cross`-style) and adjusts
formatting accordingly:

- For **frequency tables**, the first two columns (*Category* and
  *Values*) are left-aligned.

- For **cross tables**, only the first column (row variable) is
  left-aligned.

The function supports Unicode line-drawing characters and colored
separators using the **crayon** package, with graceful fallback to
monochrome output when color is not supported.

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
#>  Category │ Values  Freq.  Percent 
#> ──────────┼────────────────────────
#>  Valid    │ Yes        12     57.1 
#>           │ No          8     38.1 
#>  Missing  │ NA          1      4.8 
#> ──────────┼────────────────────────
#>  Total    │            21      100 
#> 
#> Class: data.frame
#> Data: demo
```
