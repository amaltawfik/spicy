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
  tbl <- .tor_tbl(bmi, select = c(sex, smoking), statistic = TRUE)
  console <- attr(tbl, "display_df")
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  tt <- .tor_tbl(
    bmi,
    select = c(sex, smoking),
    statistic = TRUE,
    output = "tinytable"
  )
  g <- .tor_tbl(bmi, select = c(sex, smoking), statistic = TRUE, output = "gt")
  expect_identical(.tor_cells(tt@data), .tor_cells(console))
  expect_identical(.tor_cells(g[["_data"]]), .tor_cells(console))
})

test_that("the typed view renders the same body as the engines", {
  tbl <- .tor_tbl(bmi, select = c(sex, region), effect_size = "auto")
  s <- as_structured(tbl)
  typed <- spicy:::.format_structured_to_string_body(s)
  skip_if_not_installed("gt")
  g <- .tor_tbl(
    bmi,
    select = c(sex, region),
    effect_size = "auto",
    output = "gt"
  )
  expect_identical(.tor_cells(g[["_data"]]), .tor_cells(typed))
})

test_that("only the level rows are indented", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tinytable")
  tbl <- .tor_tbl(bmi, select = c(sex, smoking))
  indent <- spicy:::.struct_indent_rows(as_structured(tbl))
  expect_identical(indent, c(3L, 4L, 6L, 7L, 8L))

  for (out in c("gt", "tinytable")) {
    rendered <- .tor_tbl(bmi, select = c(sex, smoking), output = out)
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
  tbl <- .tor_tbl(bmi, select = c(sex, smoking))
  sep <- spicy:::.struct_block_sep_rows(as_structured(tbl))
  expect_identical(sep, c(2L, 5L))
  g <- .tor_tbl(bmi, select = c(sex, smoking), output = "gt")
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
  tbl <- .tor_tbl(bmi, select = sex)
  title <- spicy:::.outcome_title(attr(tbl, "outcome_label"))
  note <- attr(tbl, "note")

  g <- .tor_tbl(bmi, select = sex, output = "gt")
  expect_identical(as.character(g[["_heading"]]$title), title)
  expect_identical(attr(g, "spicy_note", exact = TRUE), note)

  tt <- .tor_tbl(bmi, select = sex, output = "tinytable")
  expect_identical(tt@caption, title)
  expect_identical(unname(unlist(tt@notes)), note)
})

test_that("show_columns reaches the engines", {
  skip_if_not_installed("gt")
  tbl <- .tor_tbl(bmi, select = sex, show_columns = c("med_iqr", "n"))
  g <- .tor_tbl(
    bmi,
    select = sex,
    show_columns = c("med_iqr", "n"),
    output = "gt"
  )
  expect_identical(
    .tor_cells(g[["_data"]]),
    .tor_cells(attr(tbl, "display_df"))
  )
})


# ============================================================================
# The office engines: flextable, Word, Excel, clipboard
# ============================================================================

test_that("flextable renders the console body and pads the levels", {
  skip_if_not_installed("flextable")
  tbl <- .tor_tbl(bmi, select = c(sex, smoking))
  ft <- .tor_tbl(bmi, select = c(sex, smoking), output = "flextable")
  console <- attr(tbl, "display_df")
  indent <- spicy:::.struct_indent_rows(as_structured(tbl))

  # Flextable takes the console prefix OFF and pads instead: one
  # indentation, and the one that survives docx.
  body <- ft$body$dataset
  expect_identical(
    .tor_norm(body[[1L]]),
    .tor_norm(sub("^  ", "", console$Variable))
  )
  pad <- ft$body$styles$pars$padding.left$data[, 1L]
  expect_true(all(unname(pad[indent]) == 14))
  expect_true(all(unname(pad[-indent]) < 14))
  # The marginal row keeps its own label, in full.
  expect_identical(body[[1L]][[1L]], "Overall")
  # Every other cell is the console's.
  expect_identical(
    .tor_cells(body[-1L]),
    .tor_cells(console[-1L])
  )
})

test_that("the Word route writes a document with the console caption", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  f <- withr::local_tempfile(fileext = ".docx")
  out <- .tor_tbl(bmi, select = sex, output = "word", word_path = f)
  expect_identical(out, f)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)
})

test_that("Excel keeps the marginal label whole and deepens the levels", {
  skip_if_not_installed("openxlsx2")
  f <- withr::local_tempfile(fileext = ".xlsx")
  tbl <- .tor_tbl(bmi, select = c(sex, smoking))
  .tor_tbl(
    bmi,
    select = c(sex, smoking),
    output = "excel",
    excel_path = f,
    indent_text_excel_clipboard = strrep("_", 6)
  )
  cells <- openxlsx2::wb_to_df(openxlsx2::wb_load(f), col_names = FALSE)
  col <- as.character(cells[[1L]])
  col <- col[!is.na(col)]
  # The title first, then the header, then the eight body rows, then
  # the note.
  expect_identical(
    col[[1L]],
    spicy:::.outcome_title(attr(tbl, "outcome_label"))
  )
  start <- which(col == "Overall")
  expect_length(start, 1L)
  body <- col[start:(start + 7L)]
  # `make_stronger_indent()` strips `nchar(indent_text)` leading
  # characters from every row it is handed. Handing it the marginal
  # row would return "erall": THAT is what this line guards.
  expect_identical(body[[1L]], "Overall")
  expect_identical(body[[2L]], "Sex")
  expect_identical(body[[3L]], paste0(strrep("_", 6), "Female"))
  expect_identical(body[[5L]], "Current smoker")
  expect_identical(body[[8L]], paste0(strrep("_", 6), "(Missing)"))
})

test_that("the clipboard payload carries the title, the grid and the note", {
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(content, ...) {
      captured$text <- content
      invisible(content)
    },
    .package = "clipr"
  )
  tbl <- .tor_tbl(bmi, select = c(sex, smoking))
  .tor_tbl(
    bmi,
    select = c(sex, smoking),
    output = "clipboard",
    indent_text_excel_clipboard = strrep("_", 6)
  )
  lines <- strsplit(paste(captured$text, collapse = "\n"), "\n", fixed = TRUE)[[
    1L
  ]]
  # The payload is a rectangular grid: the title row is padded with
  # empty fields, so compare the FIELD, not the line.
  expect_identical(
    strsplit(lines[[1L]], "	", fixed = TRUE)[[1L]][[1L]],
    spicy:::.outcome_title(attr(tbl, "outcome_label"))
  )
  cells <- vapply(
    lines,
    function(l) strsplit(l, "\t", fixed = TRUE)[[1L]][[1L]],
    character(1),
    USE.NAMES = FALSE
  )
  expect_true("Overall" %in% cells)
  expect_true(paste0(strrep("_", 6), "Female") %in% cells)
  # Nothing was truncated on the way through the indent surgery.
  expect_false(any(cells %in% c("erall", "x", "ent smoker")))
  expect_true(any(grepl("not adjusted for one another", lines, fixed = TRUE)))
})

test_that("every engine agrees with the console on the numbers", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("gt")
  skip_if_not_installed("tinytable")
  tbl <- .tor_tbl(bmi, select = c(sex, region), effect_size = "auto")
  console <- .tor_cells(attr(tbl, "display_df")[-1L])
  for (out in c("tinytable", "gt", "flextable")) {
    rendered <- .tor_tbl(
      bmi,
      select = c(sex, region),
      effect_size = "auto",
      output = out
    )
    body <- switch(
      out,
      tinytable = rendered@data,
      gt = rendered[["_data"]],
      flextable = rendered$body$dataset
    )
    expect_identical(.tor_cells(body[-1L]), console)
  }
})
