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
  expect_silent(copy_clipboard(df, row_names_as_col = TRUE, quiet = TRUE))
  expect_silent(copy_clipboard(df, col_names = FALSE, quiet = TRUE))

  expect_silent(copy_clipboard(mat, quiet = TRUE))
  expect_silent(copy_clipboard(mat, row_names_as_col = TRUE, quiet = TRUE))
  expect_silent(copy_clipboard(mat, col_names = FALSE, quiet = TRUE))

  expect_silent(copy_clipboard(tab, quiet = TRUE))
  expect_silent(copy_clipboard(tab, row_names_as_col = TRUE, quiet = TRUE))

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

test_that("copy_clipboard hard-errors on the pre-0.13.0 dot.case argument names", {
  skip_if_not_installed("clipr")

  df <- data.frame(x = 1)

  # The rename trap fires before the clipr guards, so no mocking is
  # needed and the error is reachable on any platform.
  expect_error(
    copy_clipboard(df, row.names.as.col = TRUE),
    class = "spicy_invalid_input"
  )
  expect_error(
    copy_clipboard(df, row.names = FALSE),
    class = "spicy_invalid_input"
  )
  expect_error(
    copy_clipboard(df, col.names = FALSE),
    class = "spicy_invalid_input"
  )

  # The message names the exact replacement for every legacy name used.
  err <- tryCatch(
    copy_clipboard(df, row.names = FALSE, col.names = FALSE),
    error = function(e) e
  )
  msg <- paste(conditionMessage(err), collapse = "\n")
  expect_match(msg, "`row.names` is now `row_names`.", fixed = TRUE)
  expect_match(msg, "`col.names` is now `col_names`.", fixed = TRUE)
})

test_that("copy_clipboard validates availability and row_names_as_col", {
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
      copy_clipboard(data.frame(x = 1), row_names_as_col = 1),
      "row_names_as_col"
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

test_that("copy_clipboard re-emits write_clip messages and warnings as conditions", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  clip_payload <- NULL
  msgs <- character()
  warns <- character()
  out <- capture.output(
    withCallingHandlers(
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
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  )

  expect_s3_class(ret, "data.frame")
  # The success banner stays on stdout; the backend conditions are
  # real R conditions, no longer cat() text.
  expect_true(any(grepl("Data successfully copied to clipboard!", out)))
  expect_false(any(grepl("mock message", out)))
  expect_false(any(grepl("mock warning", out)))
  expect_true(any(grepl("mock message", msgs)))
  expect_true(any(grepl("mock warning", warns)))
  expect_equal(clip_payload$x, 1:2)
})

test_that("suppressMessages() / suppressWarnings() silence the re-emitted conditions", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  out <- capture.output(
    expect_silent(
      suppressWarnings(suppressMessages(
        with_mocked_clipr(
          copy_clipboard(
            data.frame(x = 1L),
            show_message = FALSE,
            quiet = FALSE
          ),
          write_clip = function(x, ...) {
            message("mock message")
            warning("mock warning")
            invisible(NULL)
          }
        )
      ))
    )
  )
  expect_false(any(grepl("mock", out)))
})

test_that("copy_clipboard warns (classed) when row_names_as_col is irrelevant", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  with_mocked_clipr({
    expect_warning(
      copy_clipboard(
        c("a", "b"),
        row_names_as_col = TRUE,
        quiet = FALSE,
        show_message = FALSE
      ),
      class = "spicy_ignored_arg"
    )
    expect_warning(
      copy_clipboard(
        table(c("a", "b")),
        row_names_as_col = "id",
        quiet = FALSE,
        show_message = FALSE
      ),
      class = "spicy_ignored_arg"
    )
    # And suppressWarnings() can mute it (a real R condition).
    expect_silent(
      suppressWarnings(
        copy_clipboard(
          c("a", "b"),
          row_names_as_col = TRUE,
          quiet = FALSE,
          show_message = FALSE
        )
      )
    )
  })
})

test_that("copy_clipboard rejects multi-element / NA / empty `row_names_as_col`", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  with_mocked_clipr({
    msg <- "must be either FALSE, TRUE, or a single non-empty character string"
    expect_error(
      copy_clipboard(data.frame(x = 1), row_names_as_col = c("id", "extra")),
      msg
    )
    expect_error(
      copy_clipboard(data.frame(x = 1), row_names_as_col = c(TRUE, FALSE)),
      msg
    )
    expect_error(
      copy_clipboard(data.frame(x = 1), row_names_as_col = NA),
      msg
    )
    expect_error(
      copy_clipboard(data.frame(x = 1), row_names_as_col = NA_character_),
      msg
    )
    expect_error(
      copy_clipboard(data.frame(x = 1), row_names_as_col = ""),
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

  msgs <- character()
  warns <- character()
  withCallingHandlers(
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
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # All four are surfaced, none silently dropped.
  expect_true(any(grepl("first message", msgs, fixed = TRUE)))
  expect_true(any(grepl("second message", msgs, fixed = TRUE)))
  expect_true(any(grepl("first warning", warns, fixed = TRUE)))
  expect_true(any(grepl("second warning", warns, fixed = TRUE)))
})

test_that("copy_clipboard quiet = TRUE swallows backend conditions entirely", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  expect_silent(
    with_mocked_clipr(
      copy_clipboard(data.frame(x = 1L), quiet = TRUE),
      write_clip = function(x, ...) {
        message("mock message")
        warning("mock warning")
        invisible(NULL)
      }
    )
  )
})

test_that("copy_clipboard adds row names column for data frames and matrices", {
  skip_if_not_installed("clipr")

  local_mocked_bindings(
    requireNamespace = function(...) TRUE,
    .package = "base"
  )

  captured_df <- NULL
  ret_df <- NULL
  with_mocked_clipr(
    {
      df <- data.frame(value = c(10, 20), row.names = c("a", "b"))
      ret_df <- copy_clipboard(df, row_names_as_col = TRUE, quiet = TRUE)
    },
    write_clip = function(x, ...) {
      captured_df <<- x
      invisible(NULL)
    }
  )

  expect_equal(names(captured_df)[1], "rownames")
  expect_equal(captured_df$rownames, c("a", "b"))
  # The invisible return is the transformed payload (what was sent to
  # the clipboard), as documented.
  expect_identical(ret_df, captured_df)

  captured_mat <- NULL
  with_mocked_clipr(
    {
      mat <- matrix(
        1:4,
        nrow = 2,
        dimnames = list(c("r1", "r2"), c("c1", "c2"))
      )
      copy_clipboard(mat, row_names_as_col = "id", quiet = TRUE)
    },
    write_clip = function(x, ...) {
      captured_mat <<- x
      invisible(NULL)
    }
  )

  expect_equal(names(captured_mat)[1], "id")
  expect_equal(captured_mat$id, c("r1", "r2"))
})
