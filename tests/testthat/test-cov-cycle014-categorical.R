# table_categorical(): the console alignment, and tidy() of a table whose
# long data carries no group roles.

test_that("print() centers the cells under align = \"center\"", {
  centered <- table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    align = "center"
  )
  right <- table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    align = "right"
  )
  expect_false(identical(
    capture.output(print(centered)),
    capture.output(print(right))
  ))
  seen <- NULL
  local_mocked_bindings(
    spicy_print_table = function(x, ...) {
      seen <<- list(cells = x, center = list(...)$align_center_cols)
      invisible(NULL)
    }
  )
  print(centered)
  shown <- attr(centered, "display_df")
  # Centered as they are: the cells are not padded to a decimal mark.
  expect_identical(seen$center, seq_along(shown)[-1L])
  expect_identical(seen$cells, shown)
  print(right)
  expect_identical(seen$center, integer(0))
})

test_that("tidy() rebuilds the group roles from `total` when the long data lacks them", {
  skip_if_not_installed("broom")
  tbl <- table_categorical(sochealth, select = smoking, by = sex)
  legacy <- tbl
  long <- attr(legacy, "long_data")
  long$.group_role <- NULL
  attr(legacy, "long_data") <- long
  out <- broom::tidy(legacy)
  expect_identical(out$.group_role, ifelse(out$total, "total", "group"))
  expect_identical(out, broom::tidy(tbl))
})
