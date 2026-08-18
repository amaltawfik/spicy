# Coverage tests for R/inline.R, closing the refusal arms that
# test-inline.R does not reach: the `model` guards of
# .inline_model_cols() (lines 236-239, 242, 246, 249-259), the
# "no default column" abort of .inline_default_token() (279-289),
# the final abort of .inline_ci_pair() (451-454) and the identically
# worded abort of .inline_ci_label() (521-524).
#
# Idiom mirrors test-inline.R: real tables built from `sochealth`,
# printed output captured, every refusal asserted as a classed
# condition with its exact message text.

.cig_quiet <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(expr)))
  res
}

.cig_fit <- function() {
  lm(wellbeing_score ~ age + sex, data = sochealth)
}


# ============================================================================
# .inline_model_cols(): the `model` guards (236-239, 242, 246, 249-259)
# ============================================================================

test_that("`model` on a table without spanners is refused", {
  # Lines 236-239: a single-model table carries `spanners == NULL`, so
  # `model` addresses nothing at all -- a different mistake from naming
  # a model that does not exist, and worded as such.
  tbl <- .cig_quiet(table_regression(.cig_fit()))
  expect_null(as_structured(tbl)$spanners)
  expect_error(
    inline(tbl, age, column = "b", model = 1),
    "`model` was supplied but this table has no model spanners.",
    class = "spicy_invalid_input",
    fixed = TRUE
  )
  # A label rather than a position takes the same arm.
  expect_error(
    inline(tbl, age, column = "b", model = "Model 1"),
    "no model spanners",
    class = "spicy_invalid_input"
  )
})

test_that("an unknown `model` lists the available spanners", {
  # Lines 242 / 246 both set `pick` to NULL -- a position outside
  # seq_along(spanners), or a label none of them carries -- and 249-259
  # is the shared abort, which must name the choices the user can make.
  d <- as.data.frame(sochealth)
  mm <- .cig_quiet(table_regression(list(
    A = lm(wellbeing_score ~ age, data = d),
    B = lm(wellbeing_score ~ age + sex, data = d)
  )))
  expect_identical(names(as_structured(mm)$spanners), c("A", "B"))

  # Line 242, upper side: a position past the last model.
  err <- tryCatch(
    inline(mm, age, column = "b", model = 99),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), 'Unknown model "99".', fixed = TRUE)
  expect_match(conditionMessage(err), 'Available: "A", "B".', fixed = TRUE)

  # Line 242, lower side: `model = 0` is not "the model before the
  # first", it is out of range like any other.
  expect_error(
    inline(mm, age, column = "b", model = 0),
    'Unknown model "0".',
    class = "spicy_invalid_input",
    fixed = TRUE
  )

  # Line 246: a character `model` that matches no spanner label.
  expect_error(
    inline(mm, age, column = "b", model = "Nope"),
    'Unknown model "Nope".',
    class = "spicy_invalid_input",
    fixed = TRUE
  )

  # Control: an in-range position still resolves, so the guards above
  # are refusing the wrong input, not every input.
  expect_identical(
    inline(mm, age, column = "b", model = 2),
    inline(mm, age, column = "b", model = "B")
  )
})


# ============================================================================
# .inline_default_token(): no estimate-like token (279-289)
# ============================================================================

test_that("a row with no estimate-like token refuses a bare inline()", {
  # Lines 279-289: `column = NULL` asks the row for its single
  # estimate-like token; a table showing only a median and an SD has
  # none of "or"/"irr"/"hr"/"rr"/"mr"/"exp"/"b"/"n"/"mean", so the
  # refusal must list what the table does carry.
  d <- as.data.frame(sochealth)
  tw <- .cig_quiet(table_continuous(
    d,
    select = bmi,
    show_columns = c("med", "sd")
  ))
  err <- tryCatch(inline(tw, bmi), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "No default column for this row: pick one with `column`.",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err),
    'Available tokens: "sd", "med".',
    fixed = TRUE
  )
  # Naming the column explicitly is the documented remedy, and works.
  expect_match(inline(tw, bmi, column = "med"), "^[0-9.]+$")
})


# ============================================================================
# .inline_ci_pair(): the final abort (451-454)
# ============================================================================

test_that(".inline_ci_pair() refuses when the bounds are not a pair", {
  # Lines 451-454. This arm is DEFENSIVE: .inline_cell() only calls
  # .inline_ci_pair() when at least two columns share the token and all
  # of them carry a non-empty `ci_role`, and the package only ever
  # emits "LL" / "UL" -- so two-or-more such columns always land on the
  # (1, 1) return or on the "Several confidence intervals" abort above.
  # The helper is therefore called DIRECTLY here, with a hand-built
  # `s`, which is also what keeps this test distinct from the
  # identically worded abort in .inline_ci_label() (521-524).
  s_none <- list(
    col_meta = list(
      A = list(token = "ci"),
      B = list(token = "ci")
    )
  )
  expect_error(
    spicy:::.inline_ci_pair(s_none, c("A", "B")),
    "This table displays no confidence interval.",
    class = "spicy_invalid_input",
    fixed = TRUE
  )

  # Same arm from the other shape: a lower bound with no upper bound is
  # not a pair either, and must not fall through to the "pick a
  # `model`" message, which would name a remedy that cannot help.
  s_half <- list(
    col_meta = list(
      LL = list(token = "ci", ci_role = "LL")
    )
  )
  err <- tryCatch(
    spicy:::.inline_ci_pair(s_half, "LL"),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_identical(
    conditionMessage(err),
    "This table displays no confidence interval."
  )

  # Control: a genuine pair is returned lower bound first.
  s_pair <- list(
    col_meta = list(
      lo = list(token = "ci", ci_role = "LL"),
      hi = list(token = "ci", ci_role = "UL")
    )
  )
  expect_identical(
    spicy:::.inline_ci_pair(s_pair, c("hi", "lo")),
    c("lo", "hi")
  )
})


# ============================================================================
# .inline_ci_label(): the same wording, a different helper (521-524)
# ============================================================================

test_that("{ci_label} on a table with no interval refuses", {
  # Lines 521-524. Distinct from 451-454 above even though the message
  # is word for word the same: the pattern cites no interval token, so
  # .inline_ci_pair() is never called -- .inline_ci_label() scans the
  # col_meta for a `ci_label`, finds none, and aborts on its own.
  tbl <- .cig_quiet(table_regression(
    .cig_fit(),
    show_columns = c("b", "p")
  ))
  s <- as_structured(tbl)
  # The premise: this table really carries no interval at all.
  expect_null(unlist(lapply(s$col_meta, function(m) m$ci_label)))

  expect_error(
    inline(tbl, age, column = "{b} {ci_label}"),
    "This table displays no confidence interval.",
    class = "spicy_invalid_input",
    fixed = TRUE
  )

  # And directly, so the line is pinned to THIS helper rather than to
  # whichever one the public path happened to route through.
  cols <- setdiff(names(s$col_meta), "Variable")
  expect_error(
    spicy:::.inline_ci_label(s, cols, character(0)),
    "This table displays no confidence interval.",
    class = "spicy_invalid_input",
    fixed = TRUE
  )
  # A requested-but-absent interval token takes the same exit: the
  # `want` loop finds nothing, then the plain scan finds nothing.
  expect_error(
    spicy:::.inline_ci_label(s, cols, "med_ci"),
    "This table displays no confidence interval.",
    class = "spicy_invalid_input",
    fixed = TRUE
  )
})
