# Copy data to the clipboard

`copy_clipboard()` copies a data frame, matrix, array (2D or higher),
table or vector to the clipboard. You can paste the result into a text
editor (e.g. Notepad++, Sublime Text), a spreadsheet (e.g. Excel,
LibreOffice Calc), or a word processor (e.g. Word).

## Usage

``` r
copy_clipboard(
  x,
  row.names.as.col = FALSE,
  row.names = TRUE,
  col.names = TRUE,
  message = TRUE,
  quiet = FALSE,
  ...
)
```

## Arguments

- x:

  A data frame, matrix, 2D array, 3D array, table, or atomic vector to
  be copied.

- row.names.as.col:

  Logical or character. If `FALSE` (default), row names are not added as
  a column. If `TRUE`, a column named `"rownames"` is prepended. If a
  character string is supplied, it is used as the column name for row
  names.

- row.names:

  Logical. If `TRUE` (default), includes row names in the clipboard
  output. If `FALSE`, row names are omitted.

- col.names:

  Logical. If `TRUE` (default), includes column names in the clipboard
  output. If `FALSE`, column names are omitted.

- message:

  Logical. If `TRUE` (default), displays a success message after
  copying. If `FALSE`, no success message is printed.

- quiet:

  Logical. If `TRUE`, suppresses all messages, including success,
  coercion notices, and warnings. If `FALSE` (default), messages are
  shown.

- ...:

  Additional arguments passed to
  [`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md).

## Value

Invisibly returns the object `x`. The main purpose is the side effect of
copying data to the clipboard.

## Details

Note: Objects that are not data frames or 2D matrices (e.g. atomic
vectors, arrays, tables) are automatically converted to character when
copied to the clipboard, as required by
[`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md).
The original object in R remains unchanged.

For multidimensional arrays (e.g. 3D arrays), the entire array is
flattened into a 1D character vector, with each element on a new line.
To preserve a tabular structure, you should extract a 2D slice before
copying. For example: `copy_clipboard(my_array[, , 1])`.

## Examples

``` r
# \donttest{
if (clipr::clipr_available()) {
  # Data frame
  copy_clipboard(mtcars)

  # Data frame with row names as column
  copy_clipboard(mtcars, row.names.as.col = "car")

  # Matrix
  mat <- matrix(1:6, nrow = 2)
  copy_clipboard(mat)

  # Table
  tbl <- table(iris$Species)
  copy_clipboard(tbl)

  # Array (3D) — flattened to character
  arr <- array(1:8, dim = c(2, 2, 2))
  copy_clipboard(arr)

  # Recommended: copy 2D slice for tabular layout
  copy_clipboard(arr[, , 1])

  # Numeric vector
  copy_clipboard(c(3.14, 2.71, 1.618))

  # Character vector
  copy_clipboard(c("apple", "banana", "cherry"))

  # Quiet mode (no messages shown)
  copy_clipboard(mtcars, quiet = TRUE)
}
# }
```
