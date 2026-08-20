# Engine parity for `table_outcome()`.
#
# Everything the console prints must reach the rendering engines: the
# same cells, the same title, the same note, and the block geometry --
# a rule above every block INCLUDING the first, and indentation on the
# level rows and only those.
#
# Engines are compared on CONTENT: column names differ by design (each
# engine moves the interval label into its own spanner row) and the
# decimal padding is per engine.

.tor_quiet <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(expr)))
  res
}

.tor_tbl <- function(...) {
  .tor_quiet(table_outcome(as.data.frame(spicy::sochealth), ...))
}

# Figure spaces (U+2007) are the decimal padding; non-breaking spaces
# (U+00A0) are the HTML indentation. Parity is on the content.
.tor_norm <- function(x) {
  trimws(gsub("[  ]", " ", as.character(x)))
}

.tor_cells <- function(df) {
  unname(lapply(df, .tor_norm))
}

test_that("tinytable and gt render the console body", {
  tbl <- .tor_tbl(bmi, by = c(sex, smoking), statistic = TRUE)
  console <- attr(tbl, "display_df")
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  tt <- .tor_tbl(
    bmi,
    by = c(sex, smoking),
    statistic = TRUE,
    output = "tinytable"
  )
  g <- .tor_tbl(bmi, by = c(sex, smoking), statistic = TRUE, output = "gt")
  expect_identical(.tor_cells(tt@data), .tor_cells(console))
  expect_identical(.tor_cells(g[["_data"]]), .tor_cells(console))
})

test_that("the typed view renders the same body as the engines", {
  tbl <- .tor_tbl(bmi, by = c(sex, region), effect_size = "auto")
  s <- as_structured(tbl)
  typed <- spicy:::.format_structured_to_string_body(s)
  skip_if_not_installed("gt")
  g <- .tor_tbl(bmi, by = c(sex, region), effect_size = "auto", output = "gt")
  expect_identical(.tor_cells(g[["_data"]]), .tor_cells(typed))
})

test_that("only the level rows are indented", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tinytable")
  tbl <- .tor_tbl(bmi, by = c(sex, smoking))
  indent <- spicy:::.struct_indent_rows(as_structured(tbl))
  expect_identical(indent, c(3L, 4L, 6L, 7L, 8L))

  for (out in c("gt", "tinytable")) {
    rendered <- .tor_tbl(bmi, by = c(sex, smoking), output = out)
    col <- if (identical(out, "gt")) {
      rendered[["_data"]][[1L]]
    } else {
      rendered@data[[1L]]
    }
    # Indented rows carry the non-breaking prefix; the marginal row and
    # the block headers do not. `Overall` is the witness that matters:
    # a predicate reading "not a header" would indent it.
    expect_true(all(startsWith(col[indent], strrep(" ", 4L))))
    expect_false(any(startsWith(col[-indent], " ")))
    expect_identical(col[[1L]], "Overall")
  }
})

test_that("the block rules are drawn above every block", {
  skip_if_not_installed("gt")
  tbl <- .tor_tbl(bmi, by = c(sex, smoking))
  sep <- spicy:::.struct_block_sep_rows(as_structured(tbl))
  expect_identical(sep, c(2L, 5L))
  g <- .tor_tbl(bmi, by = c(sex, smoking), output = "gt")
  styles <- g[["_styles"]]
  body <- styles[styles$locname == "data", , drop = FALSE]
  light <- body$rownum[vapply(
    body$styles,
    function(s) identical(s$cell_border_bottom$color, "#cccccc"),
    logical(1)
  )]
  # The renderer draws the rule BELOW the row above each block opening.
  expect_true(all((sep - 1L) %in% light))
})

test_that("the engines carry the title and the note", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tinytable")
  tbl <- .tor_tbl(bmi, by = sex)
  title <- spicy:::.outcome_title(attr(tbl, "outcome_label"))
  note <- attr(tbl, "note")

  g <- .tor_tbl(bmi, by = sex, output = "gt")
  expect_identical(as.character(g[["_heading"]]$title), title)
  expect_identical(attr(g, "spicy_note", exact = TRUE), note)

  tt <- .tor_tbl(bmi, by = sex, output = "tinytable")
  expect_identical(tt@caption, title)
  expect_identical(unname(unlist(tt@notes)), note)
})

test_that("show_columns reaches the engines", {
  skip_if_not_installed("gt")
  tbl <- .tor_tbl(bmi, by = sex, show_columns = c("med_iqr", "n"))
  g <- .tor_tbl(bmi, by = sex, show_columns = c("med_iqr", "n"), output = "gt")
  expect_identical(
    .tor_cells(g[["_data"]]),
    .tor_cells(attr(tbl, "display_df"))
  )
})
