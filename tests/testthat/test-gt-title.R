# The gt engine of every descriptive family carries the title the
# console prints.
#
# gt was the last of the six engines to draw none: tinytable takes it
# as `caption=`, flextable and Word as a caption paragraph, Excel
# writes it in the first row and the clipboard payload opens on it.
# The witnesses below pin two things per family:
#
#   * the title STRING, byte for byte against the console producer
#     (`.continuous_title()` / `.categorical_title()` /
#     `.continuous_lm_title()`) -- not against a literal, so a
#     translated registry moves both sides at once;
#   * the APA layout gt does NOT do by default -- flush left, and no
#     bottom border under the title (gt's own would double the rule
#     the spanner row already draws).
#
# Plus: the table note still travels as the `spicy_gt` attribute, so
# adding a header did not disturb the note route.

skip_if_no_gt <- function() {
  testthat::skip_if_not_installed("gt")
}

# The heading gt stores, as the caller wrote it.
.gt_title <- function(tbl) {
  as.character(tbl[["_heading"]]$title)
}

# TRUE when the title carries the two APA overrides: left alignment
# and a transparent, zero-weight bottom border. Read off the style
# ledger rather than the rendered HTML -- the ledger is what
# `tab_style()` writes and what a mutation would drop.
.gt_title_apa <- function(tbl) {
  styles <- tbl[["_styles"]]
  rows <- styles[styles$locname == "title", , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(list(align = NA_character_, border = NA_character_))
  }
  cells <- rows$styles
  align <- NULL
  border_color <- NULL
  border_weight <- NULL
  for (cell in cells) {
    if (!is.null(cell$cell_text$align)) {
      align <- cell$cell_text$align
    }
    if (!is.null(cell$cell_border_bottom$color)) {
      border_color <- cell$cell_border_bottom$color
    }
    if (!is.null(cell$cell_border_bottom$width)) {
      border_weight <- cell$cell_border_bottom$width
    }
  }
  list(
    align = align %||% NA_character_,
    border_color = border_color %||% NA_character_,
    border_weight = border_weight %||% NA_character_
  )
}

test_that("the one-way continuous gt table carries the console title", {
  skip_if_no_gt()
  d <- spicy::sochealth
  tbl <- table_continuous(d, select = c("bmi", "age"), output = "gt")
  expect_identical(.gt_title(tbl), spicy:::.continuous_title(NULL))

  apa <- .gt_title_apa(tbl)
  expect_identical(apa$align, "left")
  expect_identical(apa$border_color, "transparent")
  expect_identical(apa$border_weight, "0px")
})

test_that("the grouped continuous gt table names its grouping variable", {
  skip_if_no_gt()
  d <- spicy::sochealth
  tbl <- table_continuous(d, select = "bmi", by = "sex", output = "gt")
  expect_identical(.gt_title(tbl), spicy:::.continuous_title("Sex"))
  # The note route is untouched: it still travels as an attribute,
  # never as a gt source note.
  expect_true(inherits(tbl, "spicy_gt"))
  expect_true(nzchar(attr(tbl, "spicy_note", exact = TRUE)))
})

test_that("the one-way categorical gt table carries the console title", {
  skip_if_no_gt()
  d <- spicy::sochealth
  tbl <- table_categorical(d, select = c("sex", "smoking"), output = "gt")
  expect_identical(.gt_title(tbl), spicy:::.categorical_title(NULL))
  apa <- .gt_title_apa(tbl)
  expect_identical(apa$align, "left")
  expect_identical(apa$border_color, "transparent")
})

test_that("the grouped categorical gt table names its grouping variable", {
  skip_if_no_gt()
  d <- spicy::sochealth
  tbl <- table_categorical(
    d,
    select = c("smoking", "education"),
    by = "sex",
    output = "gt"
  )
  expect_identical(.gt_title(tbl), spicy:::.categorical_title("sex"))
  apa <- .gt_title_apa(tbl)
  expect_identical(apa$align, "left")
  expect_identical(apa$border_color, "transparent")
})

test_that("the bivariate linear-model gt table carries the console title", {
  skip_if_no_gt()
  d <- spicy::sochealth
  tbl <- table_continuous_lm(
    d,
    select = c("bmi", "age"),
    by = "sex",
    output = "gt"
  )
  expect_identical(.gt_title(tbl), spicy:::.continuous_lm_title("Sex"))
  apa <- .gt_title_apa(tbl)
  expect_identical(apa$align, "left")
  expect_identical(apa$border_color, "transparent")
})

test_that("the gt title equals the title the console prints", {
  skip_if_no_gt()
  d <- spicy::sochealth
  # Byte identity against the CONSOLE, not against the producer: the
  # console header is the first non-empty line of the printed table.
  console_first_line <- function(x) {
    lines <- utils::capture.output(print(x))
    lines <- lines[nzchar(trimws(lines))]
    lines[[1L]]
  }
  con <- table_continuous(d, select = "bmi", by = "sex")
  gt_con <- table_continuous(d, select = "bmi", by = "sex", output = "gt")
  expect_identical(.gt_title(gt_con), console_first_line(con))

  cat_tbl <- table_categorical(d, select = "smoking", by = "sex")
  gt_cat <- table_categorical(
    d,
    select = "smoking",
    by = "sex",
    output = "gt"
  )
  expect_identical(.gt_title(gt_cat), console_first_line(cat_tbl))
})
