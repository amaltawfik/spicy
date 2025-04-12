test_that("cross_tab computes simple 2D crosstab correctly", {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$gear <- factor(mtcars$gear)

  tab <- cross_tab(mtcars, cyl, gear)

  expect_s3_class(tab, "spicy")
  expect_true("Values" %in% colnames(tab))
  expect_true(any(grepl("Chi-2", attr(tab, "note"))))
})

test_that("cross_tab works with row percentages", {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$gear <- factor(mtcars$gear)

  tab <- cross_tab(mtcars, cyl, gear, rowprct = TRUE)

  expect_s3_class(tab, "spicy")
  expect_true("Row_Total" %in% colnames(tab))
})

test_that("cross_tab works with weights and rescaling", {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$gear <- factor(mtcars$gear)

  unweighted <- cross_tab(mtcars, cyl, gear)
  weighted <- cross_tab(mtcars, cyl, gear, weights = mtcars$mpg)
  rescaled <- cross_tab(mtcars, cyl, gear, weights = mtcars$mpg, rescale_weights = TRUE)

  expect_s3_class(weighted, "spicy")
  expect_s3_class(rescaled, "spicy")
})

test_that("cross_tab works with group 'by' and combine = TRUE", {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$gear <- factor(mtcars$gear)
  mtcars$am <- factor(mtcars$am, labels = c("auto", "manual"))

  combined <- cross_tab(mtcars, cyl, gear, by = am, combine = TRUE)

  expect_s3_class(combined, "spicy")
  expect_true("am" %in% colnames(combined))
  expect_true(any(grepl("Chi-2", attr(combined, "note"))))
})

test_that("cross_tab returns list if combine = FALSE with 'by'", {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$gear <- factor(mtcars$gear)
  mtcars$am <- factor(mtcars$am, labels = c("auto", "manual"))

  result_list <- cross_tab(mtcars, cyl, gear, by = am, combine = FALSE)

  expect_type(result_list, "list")
  expect_s3_class(result_list[[1]], "spicy")
})

test_that("cross_tab works with 1D (no y) input", {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl)

  result <- cross_tab(mtcars, cyl)

  expect_s3_class(result, "spicy")
  expect_true("Values" %in% colnames(result))
})

test_that("cross_tab errors with invalid weights or mismatched length", {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$gear <- factor(mtcars$gear)

  expect_error(cross_tab(mtcars, cyl, gear, weights = "wrong"), "must be a column name")
  expect_error(cross_tab(mtcars[1:10, ], cyl, gear, weights = rep(1, 5)), "must match length")
})

test_that("cross_tab warns with by having only one level or all NA", {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$gear <- factor(mtcars$gear)

  mtcars$one_level <- rep("only", nrow(mtcars))
  mtcars$all_na <- NA

  expect_warning(
    cross_tab(mtcars, cyl, gear, by = one_level),
    "only one unique level"
  )

  expect_error(
    cross_tab(mtcars, cyl, gear, by = all_na),
    "All values in `by` are NA"
  )
})

test_that("cross_tab handles interaction in 'by'", {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl)
  mtcars$gear <- factor(mtcars$gear)
  mtcars$am <- factor(mtcars$am, labels = c("auto", "manual"))
  mtcars$vs <- factor(mtcars$vs, labels = c("V", "S"))

  out <- cross_tab(mtcars, cyl, gear, by = interaction(am, vs), combine = TRUE)

  expect_s3_class(out, "spicy")
  expect_true("interaction(am, vs)" %in% colnames(out))
})
