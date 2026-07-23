# table_regression_models(): the machine-readable registry of supported
# classes. The same registry renders the documentation table, so this test
# also guards the published list against drift from the actual dispatch
# methods.

test_that("the registry matches the as_regression_frame dispatch methods", {
  reg <- table_regression_models()
  expect_s3_class(reg, "data.frame")
  expect_identical(
    names(reg),
    c("family", "class", "engine", "ame", "exponentiate", "blocks")
  )
  expect_true(all(nzchar(reg$class)))
  expect_false(any(duplicated(reg$class)))

  # Every registry class has a dispatch method...
  dispatched <- sub(
    "^as_regression_frame\\.",
    "",
    utils::methods("as_regression_frame")
  )
  dispatched <- setdiff(dispatched, "default")
  # (registry rows name the user-facing class; a few dispatch classes are
  # umbrella/inherited entries not listed separately)
  umbrella <- c(
    "lmerModLmerTest", # inherits the lmerMod row
    "spicy_uv_screen"
  ) # internal bundle built by
  # table_regression_uv(), not a
  # user-fitted model class
  expect_true(
    all(reg$class %in% dispatched),
    info = paste(
      "registry classes without a method:",
      paste(setdiff(reg$class, dispatched), collapse = ", ")
    )
  )
  # ...and every dispatch method is in the registry (no silent additions).
  expect_true(
    all(setdiff(dispatched, umbrella) %in% reg$class),
    info = paste(
      "methods missing from the registry:",
      paste(setdiff(setdiff(dispatched, umbrella), reg$class), collapse = ", ")
    )
  )
})

test_that("the rendered documentation table covers every registry row", {
  md <- spicy:::.render_supported_models_md()
  reg <- table_regression_models()
  expect_equal(length(strsplit(md, "\n")[[1]]), nrow(reg) + 2L)
})
