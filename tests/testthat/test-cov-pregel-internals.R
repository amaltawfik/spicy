# Two contracts that only a shape no fixture produces can state.
#
# Lines closed:
#   R/tt_theme.R           141, 148, 152
#   R/regression_nested.R  621, 771

# ---- tt_theme.R 141, 148, 152: the shapes `tt(notes = )` accepts --------

test_that("the tinytable note reader keeps the text of every note shape", {
  # `tinytable::tt(notes = )` accepts a string, a (possibly named) list
  # of strings, or a list of `list(text =, i =, j =)` cell entries, and
  # the slot keeps whichever shape it was given. The HTML finaliser
  # escapes each note by EXACT SUBSTITUTION, so it has to recover the
  # text of all three -- a shape it cannot read is a note that reaches
  # the `<tfoot>` unescaped.
  texts <- spicy:::.spicy_note_texts

  # No notes at all: nothing to substitute, and the caller's `for` loop
  # must get a zero-length character vector rather than NULL.
  expect_identical(texts(NULL), character(0))

  # A bare string, and a (named) list of strings.
  expect_identical(texts("plain"), "plain")
  expect_identical(texts(list(a = "one", b = "two")), c("one", "two"))

  # A positioned entry: only the TEXT is of interest, never `i` / `j`.
  expect_identical(texts(list(text = "cell note", i = 1, j = 1)), "cell note")
  expect_identical(
    texts(list(list(text = "x", i = 1), list(text = "y", i = 2))),
    c("x", "y")
  )

  # A leaf that is neither text nor a list carries no note.
  expect_identical(texts(42), character(0))
})


test_that("a positioned note is escaped in the rendered <tfoot>", {
  # The end of the same path: a note whose text arrives as a
  # `list(text = )` entry must still be escaped where it lands, or a
  # label shaped like a tag reaches the reader as markup. The escape is
  # engine-aware and happens here, in the HTML branch only.
  html <- paste0(
    "<table><tbody><tr><td>1</td></tr></tbody>",
    "<tfoot><tr><td colspan=\"2\">a <b> note</td></tr></tfoot></table>"
  )
  out <- spicy:::.spicy_tt_note_tfoot(html, list(text = "a <b> note"))
  expect_match(out, "a &lt;b&gt; note", fixed = TRUE)
  expect_false(grepl("<td colspan=\"2\">a <b>", out, fixed = TRUE))
  # The note cell also picks up the family's note styling.
  expect_match(out, "font-size", fixed = TRUE)

  # Without the notes the finaliser cannot know which text is a note,
  # so it styles the cell and substitutes nothing -- the default
  # argument is a real path, not a placeholder.
  bare <- spicy:::.spicy_tt_note_tfoot(html)
  expect_match(bare, "font-size", fixed = TRUE)
  expect_false(grepl("&lt;b&gt;", bare, fixed = TRUE))
})


# ---- regression_nested.R 621, 771: the all-NA contract -------------------

test_that("an LRT pair whose sample sizes cannot be established reports NA", {
  # Likelihoods fitted on different samples are not comparable, so the
  # nested block settles the question BEFORE reading anova(). A fit
  # whose number of observations cannot be established at all is not
  # evidence that the samples agree: when the engine also declines the
  # comparison, the pair gets the all-NA contract rather than a
  # plausible-looking number computed from log-likelihoods that may
  # describe different data.
  m1 <- stats::glm(am ~ 1, data = mtcars, family = stats::binomial())
  m2 <- stats::glm(am ~ wt, data = mtcars, family = stats::binomial())

  # Sanity: with the real accessors the pair does produce a change row,
  # so the NA below is the guard's doing and not an inert call.
  real <- spicy:::compute_one_pair_lrt(m1, m2)
  expect_true(is.finite(real$lrt_change))

  out <- with_mocked_bindings(
    spicy:::compute_one_pair_lrt(m1, m2),
    nested_lrt_anova = function(...) NULL,
    .spicy_nobs = function(fit) NA_real_
  )
  expect_true(all(vapply(out, function(v) is.na(v), logical(1))))
  expect_setequal(names(out), names(real))
})


test_that("a mixed pair whose engine refuses anova() reports NA, not a row", {
  skip_if_not_installed("lme4")
  # Same rule one family over: the mixed block reads AIC, BIC, deviance
  # and the chi-square off ONE anova table, positionally. If the engine
  # declines to build it there is nothing to read, and every column of
  # the change row goes NA together -- never some numbers from the
  # table and some from elsewhere.
  f1 <- lme4::lmer(
    Reaction ~ 1 + (1 | Subject),
    data = lme4::sleepstudy,
    REML = FALSE
  )
  f2 <- lme4::lmer(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy,
    REML = FALSE
  )

  real <- spicy:::compute_one_pair_mixed(f1, f2)
  expect_true(is.finite(real$lrt_change))
  expect_true(is.finite(real$aic_change))

  out <- with_mocked_bindings(
    spicy:::compute_one_pair_mixed(f1, f2),
    anova = function(...) stop("engine refuses this comparison"),
    .package = "stats"
  )
  expect_true(all(vapply(out, function(v) is.na(v), logical(1))))
  expect_setequal(names(out), names(real))
})
