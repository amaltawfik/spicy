test_that("spicy_fp_border returns an fp_border-compatible object", {
  border <- spicy:::spicy_fp_border(color = "black", width = 1)

  expect_s3_class(border, "fp_border")
  expect_equal(border$color, "black")
  expect_equal(border$width, 1)
  expect_equal(border$style, "solid")
})
