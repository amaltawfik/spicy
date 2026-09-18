# The CRAN tier of tests/testthat.R is an allow-list of test files. A listed
# file that is renamed or deleted would shrink what CRAN checks without a
# word, so the list is held against the files that exist.

test_that("every file of the CRAN tier exists", {
  entry <- test_path("..", "testthat.R")
  skip_if_not(file.exists(entry), "tests/testthat.R is not reachable from here")
  exprs <- parse(entry, keep.source = FALSE)
  is_tier <- vapply(
    exprs,
    function(e) {
      is.call(e) &&
        identical(e[[1L]], as.name("<-")) &&
        identical(e[[2L]], as.name("cran_tier"))
    },
    logical(1)
  )
  expect_identical(sum(is_tier), 1L)
  tier <- eval(exprs[[which(is_tier)]][[3L]])

  expect_type(tier, "character")
  expect_identical(anyDuplicated(tier), 0L)
  files <- file.path(test_path(), paste0("test-", tier, ".R"))
  missing <- tier[!file.exists(files)]
  expect_identical(missing, character(0))
})

test_that("the CRAN tier stays a tier, not the suite", {
  entry <- test_path("..", "testthat.R")
  skip_if_not(file.exists(entry), "tests/testthat.R is not reachable from here")
  all_files <- list.files(test_path(), pattern = "^test-.*[.]R$")
  exprs <- parse(entry, keep.source = FALSE)
  tier <- NULL
  for (e in exprs) {
    if (is.call(e) && identical(e[[1L]], as.name("<-")) &&
        identical(e[[2L]], as.name("cran_tier"))) {
      tier <- eval(e[[3L]])
    }
  }
  # Growing the tier is a decision about CRAN check time: past a quarter of
  # the suite, re-measure with devtools::check(env_vars = c(NOT_CRAN = "false")).
  expect_lt(length(tier), length(all_files) / 4)
})
