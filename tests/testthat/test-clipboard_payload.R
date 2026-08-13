# The clipboard payload is the only engine whose output is TEXT the
# user's next application has to parse. Three rules make that text
# survive the trip, and they are pinned here for every table family:
#
#   * the grid survives any delimiter -- a cell holding the delimiter,
#     a quote or a line break is quoted RFC 4180-style, so a label
#     with a comma (or `decimal_mark = ","`) no longer shifts every
#     following value one column to the right;
#   * cells are plain -- no U+2007 decimal padding (not whitespace to
#     a parser: a padded number pastes as text beside an unpadded
#     number) and no Excel text formula (`="..."`, verbatim garbage
#     in a text editor or a word processor);
#   * the table names itself and discloses itself -- the title and the
#     notes the console prints travel with the body.
#
# Nothing here touches the system clipboard: the payload builders are
# called directly, or `clipr::write_clip()` is mocked. A test run must
# never overwrite what the user has copied.

.clip_capture <- function() {
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(content, ...) {
      captured$text <- content
      invisible(content)
    },
    .package = "clipr",
    .env = parent.frame()
  )
  captured
}

.clip_lines <- function(txt) strsplit(txt, "\n", fixed = TRUE)[[1L]]

.clip_cells <- function(txt, delim = "\t") {
  unlist(strsplit(.clip_lines(txt), delim, fixed = TRUE), use.names = FALSE)
}

# Round-trip through a real delimited-text parser: the payload must
# come back as a rectangular grid whatever the delimiter.
.clip_parse <- function(txt, delim) {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  writeLines(txt, tf, useBytes = TRUE)
  utils::read.delim(
    tf,
    sep = delim,
    header = FALSE,
    colClasses = "character",
    check.names = FALSE,
    na.strings = character()
  )
}


# ---- the shared cell / row helpers ---------------------------------------

test_that("a cell is escaped only when it would break the grid", {
  # Untouched: nothing to escape.
  expect_identical(
    spicy:::.spicy_clip_cell(c("Male", "3.65", ""), "\t"),
    c("Male", "3.65", "")
  )
  # NA is a hole in the grid, not the string "NA".
  expect_identical(spicy:::.spicy_clip_cell(NA, "\t"), "")
  # The delimiter, a quote, a newline and a carriage return each
  # force quoting; embedded quotes are doubled.
  expect_identical(
    spicy:::.spicy_clip_cell("left, right", ","),
    "\"left, right\""
  )
  expect_identical(
    spicy:::.spicy_clip_cell("said \"hi\"", "\t"),
    "\"said \"\"hi\"\"\""
  )
  expect_identical(spicy:::.spicy_clip_cell("a\nb", "\t"), "\"a\nb\"")
  expect_identical(spicy:::.spicy_clip_cell("a\rb", "\t"), "\"a\rb\"")
  # A comma is harmless under the default tab delimiter.
  expect_identical(spicy:::.spicy_clip_cell("left, right", "\t"), "left, right")
  # No delimiter to collide with: cells pass through unquoted.
  expect_identical(spicy:::.spicy_clip_cell("a,b", ""), "a,b")
  expect_identical(spicy:::.spicy_clip_cell("a,b", NA_character_), "a,b")
})

test_that("a cell sheds its decimal padding but keeps its indentation", {
  expect_identical(
    spicy:::.spicy_clip_cell(
      c("\u2007\u20073.80", "38.90\u2007", "\u2007\u2007"),
      "\t"
    ),
    c("3.80", "38.90", "")
  )
  # U+00A0 is the row-label indent of table_categorical(), not
  # padding: a spreadsheet discards leading ASCII spaces, so the
  # indent has to be non-breaking and has to survive.
  expect_identical(
    spicy:::.spicy_clip_cell("\u00a0\u00a0Lower secondary", "\t"),
    "\u00a0\u00a0Lower secondary"
  )
})

test_that("rows, text rows and the payload assemble as a rectangle", {
  expect_identical(spicy:::.spicy_clip_payload(list(), "\t"), "")
  expect_identical(spicy:::.spicy_clip_rows(data.frame()[0, ]), list())
  expect_identical(spicy:::.spicy_clip_text_rows(NULL, 3L), list())
  expect_identical(spicy:::.spicy_clip_text_rows("", 3L), list())
  expect_identical(
    spicy:::.spicy_clip_text_rows("one\ntwo", 3L),
    list(c("one", "", ""), c("two", "", ""))
  )
  expect_identical(
    spicy:::.spicy_clip_payload(list(c("a", "b"), c("1", "2")), "\t"),
    "a\tb\n1\t2"
  )
  # Title and note rows are padded to the full grid.
  mat <- rbind(c("Variable", "n"), c("age", "12"))
  txt <- spicy:::.clipboard_payload_desc(
    mat,
    "\t",
    title = "A title",
    note = "A note.\nA second line."
  )
  expect_identical(
    .clip_lines(txt),
    c("A title\t", "Variable\tn", "age\t12", "A note.\t", "A second line.\t")
  )
})


# ---- table_regression: the delimiter no longer shreds the grid ------------

test_that("a label holding the delimiter keeps the columns aligned", {
  d <- as.data.frame(sochealth)
  d[["Political position (0 = left, 10 = right)"]] <- d$age
  fit <- stats::lm(
    wellbeing_score ~ `Political position (0 = left, 10 = right)` + sex,
    data = d
  )
  tbl <- table_regression(fit, show_columns = c("b", "se", "p"))
  payload <- spicy:::clipboard_payload(tbl, ",")
  grid <- .clip_parse(payload, ",")
  expect_equal(ncol(grid), 4L)
  expect_true(any(grepl(
    "Political position (0 = left, 10 = right)",
    grid[[1L]],
    fixed = TRUE
  )))
})

test_that("decimal_mark and clipboard_delim may be the same character", {
  fit <- stats::lm(wellbeing_score ~ age + sex, data = sochealth)
  tbl <- table_regression(
    fit,
    decimal_mark = ",",
    show_columns = c("b", "se", "p")
  )
  payload <- spicy:::clipboard_payload(tbl, ",")
  grid <- .clip_parse(payload, ",")
  expect_equal(ncol(grid), 4L)
  # Every numeric cell arrives whole, comma included.
  b_col <- grid[[2L]]
  expect_true(any(grepl("^\\d+,\\d+$", b_col)))
})

test_that("the default tab payload needs no quoting and no padding", {
  fit <- stats::lm(wellbeing_score ~ age + sex, data = sochealth)
  payload <- spicy:::clipboard_payload(table_regression(fit), "\t")
  expect_false(grepl("\"", payload, fixed = TRUE))
  expect_false(grepl("\u2007", payload, fixed = TRUE))
  # Level indentation is ASCII and is deliberately kept.
  expect_true(any(grepl("^  Male", .clip_lines(payload))))
})

test_that("the per-row N of a univariable screen stays an integer", {
  tbl <- table_regression_uv(
    sochealth,
    outcome = "wellbeing_score",
    predictors = c("age", "sex")
  )
  cells <- .clip_cells(spicy:::clipboard_payload(tbl, "\t"))
  expect_true("1200" %in% cells)
  expect_false("1200.00" %in% cells)
})


# ---- descriptive tables: title, notes, plain cells ------------------------

test_that("table_continuous clipboard carries the title and the note", {
  skip_if_not_installed("clipr")
  captured <- .clip_capture()

  d <- as.data.frame(sochealth)
  d$bmi[1:57] <- NA

  suppressMessages(table_continuous(
    d,
    select = c(bmi, wellbeing_score),
    by = sex,
    output = "clipboard"
  ))
  lines <- .clip_lines(captured$text)
  # The by table states its grouping variable in the title
  # (decision 4, 2026-08-13), label resolved ("Sex", not "sex").
  expect_identical(
    strsplit(lines[1L], "\t", fixed = TRUE)[[1L]][1L],
    "Descriptive statistics by Sex"
  )
  expect_match(lines[length(lines)], "^Missing values removed: bmi \\(68\\)\\.")
  expect_false(grepl("\u2007", captured$text, fixed = TRUE))
  # The sub-label row carries LL / UL under the CI spanner.
  expect_true("LL" %in% .clip_cells(captured$text))
})

test_that("table_continuous drops the sub-label row when there is no CI", {
  skip_if_not_installed("clipr")
  captured <- .clip_capture()

  suppressMessages(
    table_continuous(sochealth, select = bmi, ci = FALSE, output = "clipboard")
  )
  lines <- .clip_lines(captured$text)
  # Title, header, one body row, note -- no blank line between the
  # header and the body.
  expect_length(lines, 4L)
  expect_match(lines[2L], "^Variable\t")
  expect_match(lines[3L], "^Body mass index\t")
  expect_match(lines[4L], "^Missing values removed: bmi \\(12\\)\\.")
})

test_that("table_continuous_lm clipboard carries the title, without a blank row", {
  skip_if_not_installed("clipr")
  captured <- .clip_capture()

  suppressMessages(table_continuous_lm(
    sochealth,
    select = c(bmi, wellbeing_score),
    by = education,
    output = "clipboard"
  ))
  lines <- .clip_lines(captured$text)
  expect_identical(
    strsplit(lines[1L], "\t", fixed = TRUE)[[1L]][1L],
    "Continuous outcomes by Highest education level"
  )
  expect_match(lines[2L], "^Variable\t")
  # Line 3 used to be an all-blank second header row.
  expect_match(lines[3L], "^Body mass index\t")
  expect_false(grepl("\u2007", captured$text, fixed = TRUE))
})

test_that("a descriptive payload parses as a grid under a comma delimiter", {
  skip_if_not_installed("clipr")
  captured <- .clip_capture()

  d <- as.data.frame(sochealth)
  levels(d$education) <- c("Lower secondary, or less", "Upper", "Tertiary")

  suppressMessages(table_categorical(
    d,
    select = education,
    by = sex,
    output = "clipboard",
    clipboard_delim = ","
  ))
  grid <- .clip_parse(captured$text, ",")
  expect_equal(ncol(grid), 9L)
  expect_true(any(grepl("Lower secondary, or less", grid[[1L]], fixed = TRUE)))

  suppressMessages(table_continuous(
    d,
    select = c(bmi, wellbeing_score),
    by = sex,
    output = "clipboard",
    clipboard_delim = ",",
    decimal_mark = ","
  ))
  grid2 <- .clip_parse(captured$text, ",")
  expect_equal(ncol(grid2), 10L)
})
