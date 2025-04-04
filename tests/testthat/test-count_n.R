test_that("count_n basic value counting works", {
  df <- tibble::tibble(
    a = c("a", "b", "b"),
    b = c("b", "c", "b"),
    c = c("c", "b", "b")
  )

  expect_equal(count_n(df, count = "b"), c(1, 2, 3))
  expect_equal(count_n(df, count = c("b", "c")), c(2, 3, 3))
})

test_that("count_n works with ignore_case = TRUE", {
  df <- tibble::tibble(x = c("A", "b", "B"), y = c("a", "B", "b"))
  expect_equal(count_n(df, count = "b", ignore_case = TRUE), c(0, 2, 2))
})

test_that("count_n handles strict comparison with allow_coercion = FALSE", {
  df <- tibble::tibble(x = c(1, 2, 2), y = c("2", 2, 3))
  expect_equal(count_n(df, count = 2, allow_coercion = FALSE), c(0, 1, 1))
})

test_that("count_n handles multiple values with allow_coercion = FALSE", {
  df <- tibble::tibble(x = c(1, 2, 2), y = c("2", 2, 3))

  # Same type (character)
  expect_equal(count_n(df, count = c("2", "3"), allow_coercion = FALSE), c(1, 1, 1))

  # Same type (numeric)
  expect_equal(count_n(df, count = c(1, 2), allow_coercion = FALSE), c(1, 1, 1))

})

test_that("count_n handles special = NA, NaN, Inf, -Inf", {
  df <- tibble::tibble(
    a = c(NA, 1, NaN, Inf, -Inf),
    b = c(NaN, Inf, NA, NA, -Inf),
    c = 1:5
  )

  expect_equal(count_n(df, special = "NA"), c(2, 0, 2, 1, 0))
  expect_equal(count_n(df, special = "NaN"), c(1, 0, 1, 0, 0))
  expect_equal(count_n(df, special = "Inf"), c(0, 1, 0, 1, 0))
  expect_equal(count_n(df, special = "-Inf"), c(0, 0, 0, 0, 2))
  expect_equal(count_n(df, special = c("NA", "NaN")), c(2, 0, 2, 1, 0))
  expect_equal(count_n(df, special = "all"), c(2, 1, 2, 2, 2))
})

test_that("count_n handles factor variables including ignore_case", {
  df <- tibble::tibble(
    x = factor(c("a", "b", "c")),        # levels: a, b, c
    y = factor(c("b", "B", "a"))         # levels: a, b, B
  )

  # Default coercion: character "b" matches in both x and y
  expect_equal(count_n(df, count = "b"), c(1, 1, 0))

  # Strict mode: character input fails (character ≠ factor)
  expect_equal(count_n(df, count = "b", allow_coercion = FALSE), c(0, 0, 0))

  # Strict mode: with factor value having matching levels (for x)
  expect_equal(count_n(df, count = factor("b", levels = levels(df$x)), allow_coercion = FALSE), c(0, 1, 0))

  # Using a value from the data (ensures type + levels)
  expect_equal(count_n(df, count = df$x[2], allow_coercion = FALSE), c(0, 1, 0))

  # Case-insensitive match: works for both x and y after conversion to character
  expect_equal(count_n(df, count = "b", ignore_case = TRUE), c(1, 2, 0))
  expect_equal(count_n(df, count = "B", ignore_case = TRUE), c(1, 2, 0))

  # Case-insensitive + strict: internally uses character comparison, not identical()
  expect_equal(count_n(df, count = "b", ignore_case = TRUE, allow_coercion = FALSE), c(1, 2, 0))
  expect_equal(count_n(df, count = factor("b", levels = levels(df$x)), ignore_case = TRUE, allow_coercion = FALSE), c(1, 2, 0))
})



test_that("count_n handles labelled variables from haven", {
  skip_if_not_installed("haven")

  df <- tibble::tibble(
    lab = haven::labelled(
      c(1, 2, 1, 2, NA),
      labels = c(No = 1, Yes = 2)
    ),
    other = c(2, 2, 1, 1, 2)
  )

  expect_equal(count_n(df, count = 2), c(1, 2, 0, 1, 1))
  expect_equal(count_n(df, count = 2, allow_coercion = FALSE), c(1, 1, 0, 0, 1))
})

test_that("count_n handles selection and exclusion", {
  df <- tibble::tibble(x1 = "a", x2 = "b", x3 = "b", skip = "b")
  expect_equal(count_n(df, count = "b", select = starts_with("x")), 2)
  expect_equal(count_n(df, count = "b", select = everything(), exclude = "skip"), 2)
})

test_that("count_n works with regex selection", {
  df <- tibble::tibble(a_1 = "a", a_2 = "b", b_1 = "b")
  expect_equal(count_n(df, count = "b", select = "^a_", regex = TRUE), 1)
})

test_that("count_n returns unnamed numeric vector", {
  df <- tibble::tibble(x = c("a", "b", "b"))
  result <- count_n(df, count = "b")
  expect_true(is.numeric(result))
  expect_null(names(result))
})

test_that("count_n works inside mutate()", {
  df <- tibble::tibble(x = c("a", "b", "c"), y = c("b", "b", "b"))
  out <- df |> dplyr::mutate(n_b = count_n(count = "b"))
  expect_equal(out$n_b, c(1, 2, 1))
})

test_that("count_n ignores incompatible columns safely", {
  df <- tibble::tibble(
    x = c("a", "b"),
    y = as.Date(c("2022-01-01", "2022-01-02")),
    z = list(1, 2)
  )
  expect_equal(count_n(df, count = "b"), c(0, 1))
})

test_that("count_n returns 0 for empty or incompatible selection", {
  df <- tibble::tibble(x = 1:3, y = letters[1:3])
  expect_equal(count_n(df, count = "z", select = "x"), c(0, 0, 0))
  expect_equal(count_n(df, count = "z", select = starts_with("not_here")), c(0, 0, 0))
})

test_that("count_n throws error if neither count nor special is specified", {
  df <- tibble::tibble(x = 1:3, y = letters[1:3])
  expect_error(
    count_n(df),
    "You must specify either `count` or `special`"
  )
})
