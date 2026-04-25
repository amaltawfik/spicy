empty_code_book_data <- function() {
  data.frame(
    Variable = character(),
    Label = character(),
    Values = character(),
    Class = character(),
    N_distinct = integer(),
    N_valid = integer(),
    NAs = integer()
  )
}


code_book_columns <- function() {
  c(
    "Variable",
    "Label",
    "Values",
    "Class",
    "N_distinct",
    "N_valid",
    "NAs"
  )
}


code_book_filenames <- function(cb) {
  vapply(
    cb$x$options$buttons[[3]]$buttons,
    function(button) button$filename,
    character(1)
  )
}


code_book_export_extends <- function(cb) {
  vapply(
    cb$x$options$buttons[[3]]$buttons,
    function(button) button$extend,
    character(1)
  )
}


test_that("code_book() runs without error on a simple data frame", {
  skip_if_not_installed("DT")

  df <- head(mtcars)

  expect_silent({
    cb <- suppressMessages(code_book(df))
  })

  expect_s3_class(cb, "datatables")
  expect_true(inherits(cb, "htmlwidget"))
  expect_named(cb$x$data, code_book_columns())
  expect_equal(cb$x$data$Variable, names(df))
  expect_equal(cb$x$options$dom, "Bfrtip")
  expect_equal(cb$x$options$pageLength, 10)
  expect_true(cb$x$options$colReorder)
  expect_true(cb$x$options$fixedHeader)
  expect_equal(
    unlist(cb$x$extensions, use.names = FALSE),
    c("Buttons", "ColReorder", "FixedHeader")
  )
})

test_that("code_book() works with values = TRUE", {
  skip_if_not_installed("DT")
  df <- data.frame(x = letters[1:6])

  cb <- suppressMessages(code_book(df, values = TRUE))

  expect_s3_class(cb, "datatables")
  expect_equal(cb$x$data$Values, "a, b, c, d, e, f")
})

test_that("code_book() works with include_na = TRUE", {
  skip_if_not_installed("DT")
  df <- data.frame(x = c(1, NA, 3), y = c("a", "b", NA))

  cb <- suppressMessages(code_book(df, include_na = TRUE))

  expect_s3_class(cb, "datatables")
  expect_match(cb$x$data$Values[[1]], "<NA>", fixed = TRUE)
  expect_match(cb$x$data$Values[[2]], "<NA>", fixed = TRUE)
})

test_that("code_book() selects and reorders variables like varlist()", {
  skip_if_not_installed("DT")
  df <- data.frame(a = 1:3, b = 4:6, c = letters[1:3])

  cb <- suppressMessages(code_book(df, b, a))
  expect_equal(cb$x$data$Variable, c("b", "a"))

  cb <- suppressMessages(code_book(df, where(is.character)))
  expect_equal(cb$x$data$Variable, "c")
})

test_that("code_book() returns the same data as varlist()", {
  skip_if_not_installed("DT")
  df <- data.frame(
    a = c(3, 1, NA),
    b = letters[1:3],
    c = factor(c("yes", "no", "yes"), levels = c("no", "yes", "missing"))
  )

  expected <- varlist(
    df,
    c,
    a,
    values = TRUE,
    include_na = TRUE,
    factor_levels = "all",
    tbl = TRUE
  )
  cb <- suppressMessages(code_book(
    df,
    c,
    a,
    values = TRUE,
    include_na = TRUE,
    factor_levels = "all"
  ))

  expect_equal(cb$x$data, as.data.frame(expected))
})

test_that("code_book() accepts custom title", {
  skip_if_not_installed("DT")

  cb <- suppressMessages(code_book(head(mtcars), title = "My Codebook"))

  expect_s3_class(cb, "datatables")
  expect_equal(cb$x$caption, "<caption>My Codebook</caption>")
  expect_equal(code_book_filenames(cb), rep("My_Codebook", 3))
})

test_that("code_book() configures export buttons consistently", {
  skip_if_not_installed("DT")

  cb <- suppressMessages(code_book(head(mtcars), title = "My Codebook"))
  buttons <- cb$x$options$buttons
  download_buttons <- buttons[[3]]$buttons

  expect_equal(buttons[[1]], "copy")
  expect_equal(buttons[[2]], "print")
  expect_equal(buttons[[3]]$extend, "collection")
  expect_equal(buttons[[3]]$text, "Download")
  expect_equal(code_book_export_extends(cb), c("csv", "excel", "pdf"))
  expect_true(all(vapply(
    download_buttons,
    function(button) is.null(button$title),
    logical(1)
  )))
  expect_equal(code_book_filenames(cb), rep("My_Codebook", 3))
})

test_that("code_book() accepts title = NULL", {
  skip_if_not_installed("DT")

  cb <- suppressMessages(code_book(head(mtcars), title = NULL))

  expect_s3_class(cb, "datatables")
  expect_null(cb$x$caption)
  expect_equal(cb$x$options$buttons[[3]]$buttons[[1]]$filename, "Codebook")
})

test_that("code_book() accepts explicit export filenames", {
  skip_if_not_installed("DT")

  cb <- suppressMessages(code_book(
    head(mtcars),
    title = "Codebook: BMI / smoking?",
    filename = "bmi smoking review"
  ))

  expect_equal(cb$x$caption, "<caption>Codebook: BMI / smoking?</caption>")
  expect_equal(code_book_filenames(cb), rep("bmi_smoking_review", 3))

  cb <- suppressMessages(code_book(
    head(mtcars),
    title = NULL,
    filename = "exports/final:codebook"
  ))

  expect_null(cb$x$caption)
  expect_equal(code_book_filenames(cb), rep("exports_final_codebook", 3))
})

test_that("code_book() sanitizes export filenames", {
  skip_if_not_installed("DT")

  cb <- suppressMessages(code_book(
    head(mtcars),
    title = "Codebook: BMI / smoking?"
  ))
  expect_equal(cb$x$caption, "<caption>Codebook: BMI / smoking?</caption>")
  expect_equal(code_book_filenames(cb), rep("Codebook_BMI_smoking", 3))

  cb <- suppressMessages(code_book(head(mtcars), title = "***"))
  expect_equal(code_book_filenames(cb), rep("Codebook", 3))

  cb <- suppressMessages(code_book(
    head(mtcars),
    title = "\u00c2ge & sant\u00e9"
  ))
  expect_equal(cb$x$caption, "<caption>\u00c2ge &amp; sant\u00e9</caption>")
  expect_equal(code_book_filenames(cb), rep("Age_sante", 3))
})

test_that("code_book() works with labelled data", {
  skip_if_not_installed("DT")
  skip_if_not_installed("labelled")
  df <- data.frame(x = labelled::labelled(1:3, labels = c(A = 1, B = 2, C = 3)))
  cb <- suppressMessages(code_book(df))
  expect_s3_class(cb, "datatables")
})

test_that("code_book() errors on non-data.frame", {
  expect_error(code_book(1:10), "`x` must be a data frame or tibble")
})

test_that("code_book() errors when DT is not available", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "DT") FALSE else TRUE,
    .package = "base"
  )
  expect_error(code_book(mtcars), "Package 'DT' is required")
})

test_that("code_book() passes arguments and selectors to varlist", {
  skip_if_not_installed("DT")
  captured <- list()

  local_mocked_bindings(
    varlist = function(x, ..., values, include_na, factor_levels, tbl) {
      captured <<- list(
        values = values,
        include_na = include_na,
        factor_levels = factor_levels,
        tbl = tbl,
        dots = as.list(substitute(list(...)))[-1]
      )
      empty_code_book_data()
    },
    .package = "spicy"
  )

  suppressMessages(code_book(
    mtcars,
    cyl,
    values = TRUE,
    include_na = TRUE,
    factor_levels = "observed"
  ))

  expect_true(captured$values)
  expect_true(captured$include_na)
  expect_equal(captured$factor_levels, "observed")
  expect_true(captured$tbl)
  expect_identical(captured$dots[[1]], quote(cyl))
})

test_that("code_book() validates factor_levels", {
  expect_error(
    code_book(mtcars, factor_levels = "bad"),
    '`factor_levels` must be "observed" or "all"'
  )
})

test_that("code_book() validates logical and title arguments", {
  expect_error(
    code_book(mtcars, values = "yes"),
    "`values` must be TRUE or FALSE"
  )
  expect_error(
    code_book(mtcars, include_na = NA),
    "`include_na` must be TRUE or FALSE"
  )
  expect_error(
    code_book(mtcars, title = NA_character_),
    "`title` must be NULL or a single non-empty character string"
  )
  expect_error(
    code_book(mtcars, title = ""),
    "`title` must be NULL or a single non-empty character string"
  )
})

test_that("code_book() validates filename arguments", {
  expect_error(
    code_book(mtcars, filename = NA_character_),
    "`filename` must be NULL or a single non-empty character string"
  )
  expect_error(
    code_book(mtcars, filename = ""),
    "`filename` must be NULL or a single non-empty character string"
  )
  expect_error(
    code_book(mtcars, filename = c("a", "b")),
    "`filename` must be NULL or a single non-empty character string"
  )
  expect_error(
    code_book(mtcars, filename = "***"),
    "`filename` must contain at least one letter"
  )
})

test_that("code_book() rejects option-like partial matches in dots", {
  expect_error(
    code_book(mtcars, value = TRUE),
    "`value` was supplied through `\\.\\.\\.`"
  )
  expect_error(
    code_book(mtcars, inc = TRUE),
    "`inc` was supplied through `\\.\\.\\.`"
  )
  expect_error(
    code_book(mtcars, tit = "x"),
    "`tit` was supplied through `\\.\\.\\.`"
  )
  expect_error(
    code_book(mtcars, fac = "observed"),
    "`fac` was supplied through `\\.\\.\\.`"
  )
  expect_error(
    code_book(mtcars, fil = "x"),
    "`fil` was supplied through `\\.\\.\\.`"
  )
})

test_that("code_book() rejects renamed selections", {
  skip_if_not_installed("DT")
  df <- data.frame(x = 1:3, y = 4:6)

  expect_error(
    code_book(df, selected = x),
    "`\\.\\.\\.` can select columns but cannot rename them"
  )
})

test_that("code_book() handles empty selections", {
  skip_if_not_installed("DT")
  df <- data.frame(x = 1:3)

  expect_warning(
    cb <- suppressMessages(code_book(df, starts_with("missing"))),
    "No columns selected"
  )

  expect_s3_class(cb, "datatables")
  expect_named(cb$x$data, code_book_columns())
  expect_equal(nrow(cb$x$data), 0L)
})

test_that("code_book() handles data frames with no columns", {
  skip_if_not_installed("DT")
  df <- data.frame()

  expect_warning(
    cb <- suppressMessages(code_book(df)),
    "No columns selected"
  )

  expect_s3_class(cb, "datatables")
  expect_named(cb$x$data, code_book_columns())
  expect_equal(nrow(cb$x$data), 0L)
  expect_equal(cb$x$caption, "<caption>Codebook</caption>")
  expect_equal(code_book_export_extends(cb), c("csv", "excel", "pdf"))
  expect_equal(code_book_filenames(cb), rep("Codebook", 3))
})

test_that("code_book() defaults to all factor levels", {
  skip_if_not_installed("DT")

  captured <- character()

  local_mocked_bindings(
    varlist = function(..., factor_levels, tbl) {
      captured <<- c(captured, factor_levels)
      empty_code_book_data()
    },
    .package = "spicy"
  )

  suppressMessages(code_book(mtcars))
  suppressMessages(code_book(mtcars, factor_levels = "observed"))

  expect_equal(captured, c("all", "observed"))
})

test_that("code_book() lets varlist() errors surface directly", {
  skip_if_not_installed("DT")

  local_mocked_bindings(
    varlist = function(...) stop("bad varlist", call. = FALSE),
    .package = "spicy"
  )

  expect_error(
    code_book(mtcars),
    "^bad varlist$"
  )
})

test_that("code_book() lets varlist() column name errors surface directly", {
  skip_if_not_installed("DT")
  df <- data.frame(x = 1:3, y = 4:6)
  names(df) <- c("x", "x")

  expect_error(
    code_book(df),
    "`x` must have unique column names"
  )
})

test_that("code_book() errors when varlist() does not return a data frame", {
  skip_if_not_installed("DT")

  local_mocked_bindings(
    varlist = function(...) list(x = 1),
    .package = "spicy"
  )

  expect_error(
    code_book(mtcars),
    "`varlist\\(\\)` did not return a data frame"
  )
})
