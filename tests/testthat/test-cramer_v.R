test_that("cramer_v returns scalar by default", {
  data(mtcars)

  mtcars$gear <- as.factor(mtcars$gear)
  mtcars$cyl <- as.factor(mtcars$cyl)

  tab <- table(mtcars$gear, mtcars$cyl)

  result <- suppressWarnings(cramer_v(tab))

  expect_type(result, "double")
  expect_length(result, 1)
  expect_null(names(result))
  expect_gte(result, 0)
  expect_lte(result, 1)
})

test_that("cramer_v with detail = TRUE returns 4-element vector", {
  data(mtcars)
  tab <- table(factor(mtcars$gear), factor(mtcars$cyl))
  result <- suppressWarnings(cramer_v(tab, detail = TRUE))
  expect_length(result, 4)
  expect_named(result, c("estimate", "ci_lower", "ci_upper", "p_value"))
})

test_that("cramer_v with detail = TRUE, conf_level = NULL returns 2 elements", {
  data(mtcars)
  tab <- table(factor(mtcars$gear), factor(mtcars$cyl))
  result <- suppressWarnings(cramer_v(tab, detail = TRUE, conf_level = NULL))
  expect_length(result, 2)
  expect_named(result, c("estimate", "p_value"))
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

test_that("cramer_v rejects degenerate 1-row tables", {
  tab <- table(factor(c("A", "A")), factor(c("X", "Y")))
  expect_error(cramer_v(tab), "at least 2x2")
})
