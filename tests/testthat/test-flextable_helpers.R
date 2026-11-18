test_that("spicy_fp_border returns an fp_border-compatible object", {
  border <- spicy:::spicy_fp_border(color = "black", width = 1)

  expect_s3_class(border, "fp_border")
  expect_equal(border$color, "black")
  expect_equal(border$width, 1)
  expect_equal(border$style, "solid")
})

test_that("a note of several lines takes one flextable footer line each", {
  skip_if_not_installed("flextable")
  ft <- flextable::flextable(data.frame(a = 1))
  ft <- spicy:::.spicy_ft_attach_note(ft, "Note. First line.\nSecond line.")
  expect_identical(flextable::nrow_part(ft, "footer"), 2L)
  expect_identical(ft$footer$dataset[[2L, 1L]], "Second line.")
})

test_that("the gt minus sign rewrites the text columns and only them", {
  skip_if_not_installed("gt")
  # No text column after the stub: the table is left to gt.
  tbl <- gt::gt(data.frame(Variable = "a", b = -1.5))
  expect_identical(spicy:::.spicy_gt_minus_cells(tbl), tbl)
  # A text column gets the typographic minus in HTML.
  tbl <- gt::gt(data.frame(Variable = "a", b = "-1.5"))
  html <- gt::as_raw_html(
    spicy:::.spicy_gt_minus_cells(tbl),
    inline_css = FALSE
  )
  expect_match(html, "\u{2212}1.5", fixed = TRUE)
})
