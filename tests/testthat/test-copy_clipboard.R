with_mocked_clipr <- function(
  code,
  clipr_available = function() TRUE,
  write_clip = function(...) NULL
) {
  ns <- asNamespace("clipr")
  old_available <- get("clipr_available", envir = ns)
  old_write <- get("write_clip", envir = ns)

  unlockBinding("clipr_available", ns)
  unlockBinding("write_clip", ns)
  assign("clipr_available", clipr_available, envir = ns)
  assign("write_clip", write_clip, envir = ns)
  lockBinding("clipr_available", ns)
  lockBinding("write_clip", ns)

  on.exit(
    {
      unlockBinding("clipr_available", ns)
      unlockBinding("write_clip", ns)
      assign("clipr_available", old_available, envir = ns)
      assign("write_clip", old_write, envir = ns)
      lockBinding("clipr_available", ns)
      lockBinding("write_clip", ns)
    },
    add = TRUE
  )

  eval(substitute(code), envir = parent.frame())
}

test_that("copy_clipboard() works silently for different structures", {
  skip_if_not_installed("clipr")
  skip_if_not(clipr::clipr_available())

  df <- data.frame(
    col1 = c("A", "B", "C"),
    col2 = c(1, 2, 3)
  )

  mat <- matrix(
    1:9,
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      c("Row1", "Row2", "Row3"),
      c("Col1", "Col2", "Col3")
    )
  )

  tab <- table(
    gender = c("Male", "Female", "Female", "Male", "Male", "Female"),
    city = c("Paris", "London", "Paris", "London", "Paris", "London")
  )

  arr <- array(1:8, dim = c(2, 2, 2))

  vec_num <- c(5.5, 6.6, 7.7)
  vec_chr <- c("alpha", "beta", "gamma")

  # Silent execution
  expect_silent(copy_clipboard(df, quiet = TRUE))
  expect_silent(copy_clipboard(df, row.names.as.col = TRUE, quiet = TRUE))
  expect_silent(copy_clipboard(df, col.names = FALSE, quiet = TRUE))

  expect_silent(copy_clipboard(mat, quiet = TRUE))
  expect_silent(copy_clipboard(mat, row.names.as.col = TRUE, quiet = TRUE))
  expect_silent(copy_clipboard(mat, col.names = FALSE, quiet = TRUE))

  expect_silent(copy_clipboard(tab, quiet = TRUE))
  expect_silent(copy_clipboard(tab, row.names.as.col = TRUE, quiet = TRUE))

  expect_silent(copy_clipboard(arr, quiet = TRUE))

  expect_silent(copy_clipboard(vec_num, quiet = TRUE))
  expect_silent(copy_clipboard(vec_chr, quiet = TRUE))
})

test_that("copy_clipboard() copies expected content", {
  skip_if_not_installed("clipr")
  skip_if_not(clipr::clipr_available())

  # Data frame
  df <- data.frame(
    name = c("Alice", "Bob"),
    score = c(10, 15)
  )

  copy_clipboard(df, quiet = TRUE)
  clip <- clipr::read_clip()

  expect_length(clip, 3)
  expect_true(grepl("name\\tscore", clip[1]))
  expect_true(grepl("Alice\\t10", clip[2]))
  expect_true(grepl("Bob\\t15", clip[3]))

  # Matrix
  mat <- matrix(
    1:4,
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("r1", "r2"), c("c1", "c2"))
  )

  copy_clipboard(mat, quiet = TRUE)
  mat_clip <- clipr::read_clip()
  expect_length(mat_clip, 3)
  expect_true(grepl("c1\\tc2", mat_clip[1]))

  # Numeric vector
  vec_num <- c(3.14, 2.71, 1.618)
  copy_clipboard(vec_num, quiet = TRUE)
  num_clip <- clipr::read_clip()
  expect_equal(num_clip, as.character(vec_num))

  # Character vector
  vec_chr <- c("apple", "banana", "cherry")
  copy_clipboard(vec_chr, quiet = TRUE)
  chr_clip <- clipr::read_clip()
  expect_equal(chr_clip, vec_chr)
})

test_that("copy_clipboard validates availability and row.names.as.col", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  with_mocked_clipr(
    clipr_available = function() FALSE,
    {
      expect_error(
        copy_clipboard(data.frame(x = 1)),
        "Clipboard is not available on this system."
      )
    }
  )

  with_mocked_clipr({
    expect_error(
      copy_clipboard(data.frame(x = 1), row.names.as.col = 1),
      "row.names.as.col"
    )
  })
})

test_that("copy_clipboard errors when clipr is unavailable", {
  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )

  expect_error(
    copy_clipboard(data.frame(x = 1)),
    "Package 'clipr' is required"
  )
})

test_that("copy_clipboard captures write_clip messages and warnings", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  clip_payload <- NULL
  out <- capture.output(
    ret <- with_mocked_clipr(
      {
        copy_clipboard(
          data.frame(x = 1:2),
          show_message = TRUE,
          quiet = FALSE
        )
      },
      write_clip = function(x, ...) {
        clip_payload <<- x
        message("mock message")
        warning("mock warning")
        invisible(NULL)
      }
    )
  )

  expect_s3_class(ret, "data.frame")
  expect_true(any(grepl("Data successfully copied to clipboard!", out)))
  expect_true(any(grepl("Message: mock message", out)))
  expect_true(any(grepl("Warning: mock warning", out)))
  expect_equal(clip_payload$x, 1:2)
})

test_that("copy_clipboard warns when row.names.as.col is irrelevant", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  out <- capture.output(
    with_mocked_clipr({
      copy_clipboard(
        c("a", "b"),
        row.names.as.col = TRUE,
        quiet = FALSE,
        show_message = FALSE
      )
    })
  )

  expect_true(any(grepl("has no effect", out)))

  out_chr <- capture.output(
    with_mocked_clipr({
      copy_clipboard(
        table(c("a", "b")),
        row.names.as.col = "id",
        quiet = FALSE,
        show_message = FALSE
      )
    })
  )

  expect_true(any(grepl("is ignored because", out_chr)))
})

test_that("copy_clipboard rejects multi-element / NA / empty `row.names.as.col`", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  with_mocked_clipr({
    msg <- "must be either FALSE, TRUE, or a single non-empty character string"
    expect_error(
      copy_clipboard(data.frame(x = 1), row.names.as.col = c("id", "extra")),
      msg
    )
    expect_error(
      copy_clipboard(data.frame(x = 1), row.names.as.col = c(TRUE, FALSE)),
      msg
    )
    expect_error(
      copy_clipboard(data.frame(x = 1), row.names.as.col = NA),
      msg
    )
    expect_error(
      copy_clipboard(data.frame(x = 1), row.names.as.col = NA_character_),
      msg
    )
    expect_error(
      copy_clipboard(data.frame(x = 1), row.names.as.col = ""),
      msg
    )
  })
})

test_that("copy_clipboard accumulates multiple captured messages / warnings", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  out <- capture.output(
    with_mocked_clipr(
      copy_clipboard(
        data.frame(x = 1L),
        show_message = FALSE,
        quiet = FALSE
      ),
      write_clip = function(x, ...) {
        message("first message")
        message("second message")
        warning("first warning")
        warning("second warning")
        invisible(NULL)
      }
    )
  )

  # All four are surfaced, none silently dropped.
  expect_true(any(grepl("Message: first message", out, fixed = TRUE)))
  expect_true(any(grepl("Message: second message", out, fixed = TRUE)))
  expect_true(any(grepl("Warning: first warning", out, fixed = TRUE)))
  expect_true(any(grepl("Warning: second warning", out, fixed = TRUE)))
})

test_that("copy_clipboard `Warning:` prefix is consistent for the row.names.as.col warning", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  out <- capture.output(
    with_mocked_clipr({
      copy_clipboard(
        c("a", "b"),
        row.names.as.col = TRUE,
        show_message = FALSE,
        quiet = FALSE
      )
    })
  )

  # The needs_warning message now uses the same "Warning: " prefix as
  # the captured-warning path (consistency polish).
  expect_true(any(grepl("Warning: `row.names.as.col = TRUE` has no effect", out, fixed = TRUE)))
})

test_that("copy_clipboard adds row names column for data frames and matrices", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  captured_df <- NULL
  with_mocked_clipr(
    {
      df <- data.frame(value = c(10, 20), row.names = c("a", "b"))
      copy_clipboard(df, row.names.as.col = TRUE, quiet = TRUE)
    },
    write_clip = function(x, ...) {
      captured_df <<- x
      invisible(NULL)
    }
  )

  expect_equal(names(captured_df)[1], "rownames")
  expect_equal(captured_df$rownames, c("a", "b"))

  captured_mat <- NULL
  with_mocked_clipr(
    {
      mat <- matrix(
        1:4,
        nrow = 2,
        dimnames = list(c("r1", "r2"), c("c1", "c2"))
      )
      copy_clipboard(mat, row.names.as.col = "id", quiet = TRUE)
    },
    write_clip = function(x, ...) {
      captured_mat <<- x
      invisible(NULL)
    }
  )

  expect_equal(names(captured_mat)[1], "id")
  expect_equal(captured_mat$id, c("r1", "r2"))
})
