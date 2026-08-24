# inline() refusals that used to be dead ends (register n. 226(b) /
# n. 227(a)). The tests live in their own file because test-inline.R is
# owned by another branch.

.inline_msg_data <- function(n = 60) {
  set.seed(4)
  data.frame(
    bmi = stats::rnorm(n, 25, 4),
    chol = stats::rnorm(n, 5, 1),
    sex = factor(rep(c("Female", "Male"), n / 2)),
    stringsAsFactors = FALSE
  )
}


# ---- n. 227(a): contrast = "none" ----------------------------------------

# `table_continuous_lm(contrast = "none")` still prints the delta column,
# empty, and `delta` is the token a BARE inline(x, var) asks for. The
# refusal used to stop at "The "delta" cell of this row is empty in the
# table." -- true, and a dead end: it did not say the column is empty
# throughout, and it did not point at the group means beside it.
test_that("an empty delta cell names the table-wide emptiness and the remedy", {
  d <- .inline_msg_data()
  tbl <- table_continuous_lm(d, c(bmi, chol), by = sex, contrast = "none")
  err <- tryCatch(inline(tbl, bmi), error = function(e) e)
  expect_s3_class(err, "spicy_invalid_input")
  msg <- conditionMessage(err)
  expect_match(msg, "The \"delta\" cell of this row is empty", fixed = TRUE)
  expect_match(msg, "empty on EVERY row", fixed = TRUE)
  expect_match(msg, "level = \"Female\", column = \"emmean\"", fixed = TRUE)
  expect_match(msg, "Available levels: \"Female\", \"Male\".", fixed = TRUE)
})

test_that("the same hints reach an explicitly addressed delta", {
  d <- .inline_msg_data()
  tbl <- table_continuous_lm(d, c(bmi, chol), by = sex, contrast = "none")
  msg <- tryCatch(
    inline(tbl, bmi, column = "delta"),
    error = function(e) conditionMessage(e)
  )
  expect_match(msg, "empty on EVERY row", fixed = TRUE)
  expect_match(msg, "column = \"emmean\"", fixed = TRUE)
})

test_that("the remedy the message names actually works", {
  d <- .inline_msg_data()
  tbl <- table_continuous_lm(d, c(bmi, chol), by = sex, contrast = "none")
  expect_identical(
    inline(tbl, bmi, level = "Female", column = "emmean"),
    "26.59"
  )
  expect_identical(inline(tbl, bmi, level = "Male", column = "emmean"), "25.22")
})

test_that("a table that HAS a contrast is unaffected", {
  d <- .inline_msg_data()
  tbl <- table_continuous_lm(d, c(bmi, chol), by = sex)
  expect_identical(inline(tbl, bmi), "-1.37")
})


# ---- n. 226(b) / n. 227(b): the "Available: ." dead end -------------------

# The register recorded inline(tbl, bmi, "Female", "p") on a
# table_continuous() as still answering "Available: .". It no longer does:
# the p is addressable with the level of its own row, and the ambiguity
# refusal that fires without a level lists the levels, not nothing.
test_that("a p addressed by level resolves on table_continuous", {
  d <- .inline_msg_data()
  tbl <- table_continuous(d, c(bmi, chol), by = sex)
  expect_identical(inline(tbl, bmi, "Female", "p"), ".146")
  expect_identical(inline(tbl, chol, "Female", "p"), ".247")
})

test_that("no refusal on this table lists an empty set of alternatives", {
  d <- .inline_msg_data()
  tbl <- table_continuous(d, c(bmi, chol), by = sex)
  msgs <- c(
    tryCatch(inline(tbl, bmi, column = "p"), error = function(e) {
      conditionMessage(e)
    }),
    tryCatch(inline(tbl, bmi, "Nope", "p"), error = function(e) {
      conditionMessage(e)
    }),
    tryCatch(inline(tbl, bmi, "Female", "mean"), error = function(e) {
      conditionMessage(e)
    })
  )
  for (m in msgs) {
    expect_false(grepl("Available: .", m, fixed = TRUE))
    expect_match(m, "Available")
  }
  expect_match(msgs[1L], "pick one with `level`", fixed = TRUE)
  expect_match(msgs[1L], "\"Female\", \"Male\"", fixed = TRUE)
})
