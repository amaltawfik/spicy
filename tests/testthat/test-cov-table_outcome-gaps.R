# Coverage of the arms the behaviour tests do not reach: the scalar
# validators, the ignored-argument warnings, and the two fallbacks
# that only fire on an object rebuilt by hand.
#
# Each block says which arm it covers, so a future refactor can tell
# a real assertion from a coverage visit.

.cto_quiet <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(expr)))
  res
}

.cto_d <- function() {
  data.frame(
    y = c(1, 2, 3, 4, 10, 20, 30, 40),
    g = c("a", "a", "a", "a", "b", "b", "b", "b"),
    stringsAsFactors = FALSE
  )
}

test_that("the scalar validators refuse, one message each", {
  d <- .cto_d()
  bad <- function(...) tryCatch(table_outcome(...), error = identity)

  expect_s3_class(bad("not a frame", y, by = g), "spicy_invalid_data")
  expect_s3_class(bad(d, y, by = g, ci_level = 2), "spicy_invalid_input")
  expect_s3_class(bad(d, y, by = g, ci_level = "x"), "spicy_invalid_input")
  expect_s3_class(bad(d, y, by = g, digits = -1), "spicy_invalid_input")
  expect_s3_class(
    bad(d, y, by = g, effect_size_digits = -1),
    "spicy_invalid_input"
  )
  expect_s3_class(bad(d, y, by = g, p_digits = 0), "spicy_invalid_input")
  expect_s3_class(bad(d, y, by = g, decimal_mark = ".."), "spicy_invalid_input")
  expect_s3_class(bad(d, y, by = g, labels = "no names"), "spicy_invalid_input")
  expect_s3_class(bad(d, y, by = g, overall = NA), "spicy_invalid_input")
  expect_s3_class(bad(d, y, by = g, drop_na = 1), "spicy_invalid_input")
  expect_s3_class(bad(d, y, by = g, user_na = NULL), "spicy_invalid_input")
  expect_s3_class(bad(d, y, by = g, p_value = "yes"), "spicy_invalid_input")
  expect_s3_class(
    bad(d, y, by = g, effect_size = c(TRUE, FALSE)),
    "spicy_invalid_input"
  )
  expect_s3_class(bad(d, y, by = g, rescale = NA), "spicy_invalid_input")
})

test_that("`effect_size` accepts the legacy logical form", {
  d <- .cto_d()
  logical_on <- .cto_quiet(table_outcome(d, y, by = g, effect_size = TRUE))
  named_on <- .cto_quiet(table_outcome(d, y, by = g, effect_size = "auto"))
  expect_identical(
    attr(logical_on, "display_df"),
    attr(named_on, "display_df")
  )
  logical_off <- .cto_quiet(table_outcome(d, y, by = g, effect_size = FALSE))
  expect_false("ES" %in% names(attr(logical_off, "display_df")))
})

test_that("the ignored-argument warnings fire and say what won", {
  d <- .cto_d()
  # `show_columns` is sovereign over `show_n` and `ci`.
  expect_warning(
    table_outcome(d, y, by = g, show_n = TRUE, show_columns = c("m", "sd")),
    class = "spicy_ignored_arg"
  )
  expect_warning(
    table_outcome(d, y, by = g, ci = TRUE, show_columns = c("m", "sd")),
    class = "spicy_ignored_arg"
  )
  # A `test` nobody will run.
  expect_warning(
    table_outcome(d, y, by = g, test = "student", p_value = FALSE),
    class = "spicy_ignored_arg"
  )
  # An interval implies the measure it bounds.
  expect_warning(
    table_outcome(d, y, by = g, effect_size_ci = TRUE),
    class = "spicy_ignored_arg"
  )
})

test_that("an integer64 column is refused before anything reads it", {
  skip_if_not_installed("bit64")
  d <- .cto_d()
  d$big <- bit64::as.integer64(seq_len(nrow(d)))
  err <- tryCatch(table_outcome(d, big, by = g), error = identity)
  expect_s3_class(err, "spicy_invalid_data")
  expect_match(conditionMessage(err), "integer64")
})

test_that("the membership guard names several missing columns at once", {
  d <- .cto_d()
  err <- tryCatch(
    table_outcome(d, y, by = c("nope", "alsonope")),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "are not columns")
  expect_match(conditionMessage(err), "alsonope")
})

test_that("declared missing values are honoured, and disclosed", {
  skip_if_not_installed("haven")
  d <- .cto_d()
  d$y <- haven::labelled_spss(d$y, na_values = 40)
  tbl <- .cto_quiet(table_outcome(d, y, by = g))
  expect_identical(tbl$n[tbl$.row_role == "summary"], 7L)
  expect_match(
    attr(tbl, "note"),
    "Declared missing values removed: y (1).",
    fixed = TRUE
  )
  # `user_na = FALSE` drops the declaration and keeps the code.
  kept <- .cto_quiet(table_outcome(d, y, by = g, user_na = FALSE))
  expect_identical(kept$n[kept$.row_role == "summary"], 8L)
})

test_that("the print method rebuilds a display frame it was not given", {
  # A table that travelled through `structure()` or a `[` that dropped
  # the cached frame still prints: the fallback recomputes it from the
  # attributes, which is why they are all stored.
  tbl <- .cto_quiet(table_outcome(.cto_d(), y, by = g))
  cached <- attr(tbl, "display_df")
  stripped <- tbl
  attr(stripped, "display_df") <- NULL
  expect_identical(spicy:::.outcome_rendered_df(stripped), cached)
  expect_output(print(stripped), "Overall")
})

test_that("the broom helpers fall back without tibble", {
  # `tibble` is a Suggests: the frames must still come back, as plain
  # data.frames, when it is absent -- and the coercion must say which
  # package to install.
  tbl <- .cto_quiet(table_outcome(.cto_d(), y, by = g))
  td <- with_mocked_bindings(
    spicy:::tidy.spicy_outcome_table(tbl),
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  expect_identical(class(td), "data.frame")
  gl <- with_mocked_bindings(
    spicy:::glance.spicy_outcome_table(tbl),
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  expect_identical(class(gl), "data.frame")
  err <- tryCatch(
    with_mocked_bindings(
      spicy:::as_tibble.spicy_outcome_table(tbl),
      requireNamespace = function(...) FALSE,
      .package = "base"
    ),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_missing_pkg")
  expect_match(conditionMessage(err), "Install package 'tibble'.", fixed = TRUE)
})
