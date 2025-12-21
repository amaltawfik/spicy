test_that("cramer_v computes Cramer's V correctly with mtcars example", {
  data(mtcars)

  mtcars$gear <- as.factor(mtcars$gear)
  mtcars$cyl <- as.factor(mtcars$cyl)

  tab <- table(mtcars$gear, mtcars$cyl)

  result <- suppressWarnings(cramer_v(tab))

  expect_type(result, "double")
  expect_length(result, 1)
  expect_gte(result, 0)
  expect_lte(result, 1)
})

test_that("cramer_v fails gracefully with non-table input", {
  expect_error(
    cramer_v(iris),
    "`x` must be a contingency table \\(class `table`\\)\\."
  )

  expect_error(
    cramer_v(1:10),
    "`x` must be a contingency table \\(class `table`\\)\\."
  )
})
