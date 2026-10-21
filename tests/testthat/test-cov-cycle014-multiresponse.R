# table_multiresponse(): the refusals on empty data, the `counted` value a
# column declares, the undefined Rao-Scott correction, and the console
# alignment.

test_that("zero-row data with a character `by` is a `spicy_invalid_data` error", {
  d <- data.frame(a = logical(0), b = logical(0), g = character(0))
  expect_error(
    table_multiresponse(d, select = c(a, b), by = g),
    class = "spicy_invalid_data"
  )
})

test_that("`counted` on columns that hold no value says so", {
  d <- data.frame(a = c(NA, NA), b = c(NA, NA))
  err <- expect_error(
    table_multiresponse(d, select = c(a, b), counted = "Yes"),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "`counted = \"Yes\"` matches no value of the selected columns.",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err),
    "The selected columns hold no value at all.",
    fixed = TRUE
  )
})

test_that("a `counted` code the value labels declare is valid when nobody chose it", {
  nobody <- labelled::labelled(c(0, 0, 0, 0), labels = c(No = 0, Yes = 1))
  d <- data.frame(a = nobody, b = nobody)
  by_code <- as.data.frame(table_multiresponse(
    d,
    select = c(a, b),
    counted = 1
  ))
  by_label <- as.data.frame(
    table_multiresponse(d, select = c(a, b), counted = "Yes")
  )
  items <- by_code$.row_role == "summary"
  expect_identical(by_code$variable[items], c("a", "b"))
  expect_equal(by_code$n[items], c(0, 0))
  expect_equal(by_label$n, by_code$n)
  # The same zeros with no value labels: a `counted` that matches nothing.
  expect_error(
    table_multiresponse(
      data.frame(a = c(0, 0), b = c(0, 0)),
      select = c(a, b),
      counted = 1
    ),
    class = "spicy_invalid_input"
  )
})

test_that("the Rao-Scott correction is undefined when no group varies", {
  # Each group answers each item with one voice.
  d <- data.frame(
    a = rep(c(TRUE, FALSE), each = 10),
    b = rep(c(FALSE, TRUE), each = 10),
    g = rep(c("x", "y"), each = 10)
  )
  w <- expect_warning(
    tbl <- table_multiresponse(d, select = c(a, b), by = g),
    class = "spicy_undefined_stat"
  )
  expect_match(
    conditionMessage(w),
    "The Rao-Scott correction is undefined: the indicators do not vary within any group",
    fixed = TRUE
  )
  expect_null(attr(tbl, "global_test"))
  # The item tests stand.
  p <- as.data.frame(tbl)$p
  expect_true(all(p[!is.na(p)] < 0.001))
  expect_length(p[!is.na(p)], 2L)
})

test_that("print() hands the console the alignment `align` asks for", {
  d <- data.frame(
    a = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
    b = c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
    g = c("x", "y", "x", "y", "x", "y")
  )
  centered <- table_multiresponse(d, select = c(a, b), by = g, align = "center")
  right <- table_multiresponse(d, select = c(a, b), by = g, align = "right")
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
