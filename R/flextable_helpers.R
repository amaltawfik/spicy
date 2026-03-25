# Build a flextable/officer-compatible border object without forcing
# a hard dependency on officer for plain flextable output.
spicy_fp_border <- function(color = "black", width = 1, style = "solid") {
  structure(
    list(
      width = width,
      color = color,
      style = style
    ),
    class = "fp_border"
  )
}
