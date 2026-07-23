# Copy data to the clipboard

Copies a `data.frame`, matrix, 2D or higher array, table, or atomic
vector to the system clipboard, ready to paste into a text editor,
spreadsheet, or word processor. Wraps
[`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md)
(a Suggests dependency); requires `clipr` to be installed and a
clipboard backend to be available on the platform.

## Usage

``` r
copy_clipboard(
  x,
  row_names_as_col = FALSE,
  row_names = TRUE,
  col_names = TRUE,
  show_message = TRUE,
  quiet = FALSE,
  ...
)
```

## Arguments

- x:

  A `data.frame`, matrix, 2D array, 3D array, table, or atomic vector to
  be copied.

- row_names_as_col:

  Logical or character. If `FALSE` (the default), row names are not
  added as a column. If `TRUE`, a column named `"rownames"` is
  prepended. If a character string, it is used as the column name for
  the promoted row names. Ignored (with a warning) when `x` is neither a
  `data.frame` nor a strict matrix.

- row_names:

  Logical. If `TRUE` (the default), row names are included in the
  clipboard output; `FALSE` omits them.

- col_names:

  Logical. If `TRUE` (the default), column names are included in the
  clipboard output; `FALSE` omits them.

- show_message:

  Logical. If `TRUE` (the default), prints a success message after
  copying.

- quiet:

  Logical. If `FALSE` (the default), messages are shown. If `TRUE`,
  suppresses all messages, including the success message, coercion
  notices, and warnings.

- ...:

  Additional arguments passed to
  [`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md).
  The pre-0.13.0 dot.case argument names (`row.names.as.col`,
  `row.names`, `col.names`) are trapped here and raise an error naming
  their snake_case replacements.

## Value

Invisibly returns the object as it was sent to the clipboard: identical
to `x` by default, but reflecting the `row_names_as_col` transformation
when one was requested (e.g. a matrix comes back as a `data.frame` with
the promoted row-name column). The function is called for its clipboard
side effect.

## Details

Objects that are not `data.frame`s or 2D matrices (atomic vectors,
arrays, tables) are automatically coerced to character on the way to the
clipboard, as required by
[`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md).
The caller's object is never modified in place; transformations happen
on a local copy (see the return value).

Multidimensional arrays (3D and higher) are flattened to a 1D character
vector with one element per line. To preserve a tabular layout, extract
a 2D slice first, e.g. `copy_clipboard(my_array[, , 1])`.

Messages and warnings raised by the clipboard backend are re-emitted as
regular R conditions, so
[`suppressMessages()`](https://rdrr.io/r/base/message.html) /
[`suppressWarnings()`](https://rdrr.io/r/base/warning.html) work as
usual; `quiet = TRUE` silences them all at once.

## Examples

``` r
# \donttest{
if (clipr::clipr_available()) {
  # Data frame
  copy_clipboard(sochealth)

  # Data frame with row names as column
  copy_clipboard(head(sochealth), row_names_as_col = "id")

  # Matrix
  mat <- matrix(1:6, nrow = 2)
  copy_clipboard(mat)

  # Table
  tbl <- table(sochealth$education)
  copy_clipboard(tbl)

  # Array (3D) -- flattened to character
  arr <- array(1:8, dim = c(2, 2, 2))
  copy_clipboard(arr)

  # Recommended: copy 2D slice for tabular layout
  copy_clipboard(arr[, , 1])

  # Numeric vector
  copy_clipboard(c(3.14, 2.71, 1.618))

  # Character vector
  copy_clipboard(c("apple", "banana", "cherry"))

  # Quiet mode (no messages shown)
  copy_clipboard(sochealth, quiet = TRUE)
}
# }
```
