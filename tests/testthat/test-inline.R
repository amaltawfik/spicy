# inline(): one formatted cell of a spicy table, for running text.
#
# The guarantee under test: the returned string IS the displayed cell
# -- same formatter, same style, same precision -- and rows are
# addressed by identity, so custom labels or a journal style never
# change a call.

.il_fit <- function() {
  lm(wellbeing_score ~ age + sex, data = sochealth)
}

.il_quiet <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(expr)))
  res
}

test_that("regression cells match the displayed table exactly", {
  tbl <- .il_quiet(table_regression(.il_fit()))
  s <- as_structured(tbl)
  formatted <- spicy:::.format_structured_to_string_body(s)
  age_row <- which(s$body$.variable == "age")
  expect_identical(inline(tbl, age, column = "b"), trimws(formatted$B[age_row]))
  expect_identical(
    inline(tbl, age, column = "se"),
    trimws(formatted$SE[age_row])
  )
  expect_identical(inline(tbl, sex, "Male", "p"), "<.001")
  # The interval composes with the style's brackets and separator.
  expect_match(inline(tbl, age, column = "ci"), "^\\[.+, .+\\]$")
})

test_that("patterns compose cells and the interval label", {
  tbl <- .il_quiet(table_regression(.il_fit()))
  out <- inline(tbl, sex, "Male", "{b} ({ci_label} {ci}; p {p})")
  expect_match(out, "^3.90 \\(95% CI \\[2.14, 5.65\\]; p <.001\\)$")
})

test_that("fit statistics are addressed by token", {
  tbl <- .il_quiet(table_regression(.il_fit()))
  expect_identical(inline(tbl, "n"), "1200")
})

test_that("addressing is by identity: custom labels do not move a call", {
  tbl1 <- .il_quiet(table_regression(.il_fit()))
  tbl2 <- .il_quiet(table_regression(
    .il_fit(),
    labels = c(sex = "Administrative sex")
  ))
  expect_identical(
    inline(tbl2, sex, "Male", "b"),
    inline(tbl1, sex, "Male", "b")
  )
  # And the displayed-label convenience still finds the row.
  expect_identical(
    inline(tbl2, "Administrative sex", "Male", "b"),
    inline(tbl1, sex, "Male", "b")
  )
})

test_that("the cited text follows the style and the decimal mark", {
  tbl <- .il_quiet(table_regression(.il_fit(), decimal_mark = ","))
  expect_identical(inline(tbl, sex, "Male", "b"), "3,90")
  tbl_j <- .il_quiet(table_regression(.il_fit(), style = "jama"))
  # JAMA rounds p to two decimals.
  expect_identical(inline(tbl_j, age, column = "p"), ".16")
})

test_that("categorical and continuous families answer by token", {
  d <- as.data.frame(sochealth)
  tc <- .il_quiet(table_categorical(d, select = smoking, by = sex))
  expect_identical(inline(tc, smoking, "Yes", "n", model = "Female"), "131")
  expect_identical(inline(tc, smoking, "Yes", "pct", model = "Total"), "20.8")
  tw <- .il_quiet(table_continuous(d, select = bmi))
  expect_identical(inline(tw, bmi, column = "m"), "25.93")
  tl <- .il_quiet(table_continuous_lm(d, select = bmi, by = sex))
  expect_match(inline(tl, bmi, column = "delta"), "^[0-9.]+$")
})

test_that("misaddressing errors are classed and list the choices", {
  tbl <- .il_quiet(table_regression(.il_fit()))
  err <- tryCatch(inline(tbl, nope, column = "b"), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "age")
  err <- tryCatch(inline(tbl, sex, column = "b"), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "Male")
  err <- tryCatch(inline(tbl, sex, "Nope", "b"), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  err <- tryCatch(inline(tbl, age, column = "zzz"), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "\"b\"")
})

test_that("reference and undefined cells refuse with the reason", {
  tbl <- .il_quiet(table_regression(.il_fit()))
  err <- tryCatch(inline(tbl, sex, "Female", "b"), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "reference")
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal", envir = environment())
  fit <- ordinal::clm(rating ~ temp + bottle, data = wine)
  tclm <- .il_quiet(table_regression(fit))
  err <- tryCatch(inline(tclm, bottle, "8", "b"), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "undefined|not estimable")
})

test_that("a multi-model table requires and honours `model`", {
  d <- as.data.frame(sochealth)
  m1 <- lm(wellbeing_score ~ age, data = d)
  m2 <- lm(wellbeing_score ~ age + sex, data = d)
  tbl <- .il_quiet(table_regression(list(A = m1, B = m2)))
  err <- tryCatch(inline(tbl, age, column = "b"), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "\"A\"")
  s <- as_structured(tbl)
  formatted <- spicy:::.format_structured_to_string_body(s)
  age_row <- which(s$body$.variable == "age")
  b_a <- inline(tbl, age, column = "b", model = "A")
  b_b <- inline(tbl, age, column = "b", model = "B")
  expect_identical(b_a, trimws(formatted[["A: B"]][age_row]))
  expect_identical(b_b, trimws(formatted[["B: B"]][age_row]))
  expect_identical(inline(tbl, age, column = "b", model = 1), b_a)
})

test_that("the missing category is addressed by role, not label", {
  d <- as.data.frame(sochealth)
  d$smoking <- as.character(d$smoking)
  d$smoking[d$smoking == "Yes"] <- "(Missing)"
  tc <- .il_quiet(table_categorical(d, select = smoking, drop_na = FALSE))
  # The real missing row (displayed "(Missing_1)") answers to the key.
  out <- inline(tc, smoking, "(Missing)", "n")
  s <- as_structured(tc)
  miss_row <- which(s$body$.row_role == "missing")
  expect_identical(out, as.character(s$body$n[miss_row]))
})
