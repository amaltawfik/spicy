test_that("varlist() returns a dataframe", {
  expect_s3_class(varlist(mtcars, tbl = T), "data.frame")
})

test_that("varlist()$Variable returns a character vector", {
  expect_type(varlist(mtcars, tbl = T)$Variable, "character")
})

test_that("varlist() returns correct name of columns", {
  expect_named(varlist(mtcars, tbl = T), c("Variable",
                                           "Label",
                                           "Values",
                                           "Class",
                                           "Ndist_val",
                                           "N_valid",
                                           "NAs"))
})

