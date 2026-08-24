# ---------------------------------------------------------------------------
# "Residual" is a reserved key of the variance-components frame.
#
# Every mixed engine writes the residual row as group = "Residual",
# term = "", and every consumer finds the residual by reading that
# string back: the nlme and glmmTMB SE / CI walks, the ICC kernel, the
# block sort, the footer panel, the null-model LR test. A grouping
# factor a user named `Residual` is therefore not merely confusable with
# the residual -- it is indistinguishable from it, and the frame carried
# the damage silently.
#
# Measured before the guard, on `lme(distance ~ age, random = ~ 1 |
# Residual)` over Orthodont: the group's variance row (4.47) came back
# with the residual's interval, [1.50, 2.79] -- an interval that does not
# contain its own estimate -- and the ICC went NA with nothing said.
#
# The frame refuses instead, at the constructor every engine passes
# through, because disambiguating means migrating five engines and seven
# readers onto an internal key: a contract change, not a guard.
# ---------------------------------------------------------------------------

.orthodont_residual_named <- function() {
  skip_if_not_installed("nlme")
  d <- nlme::Orthodont
  d$Residual <- d$Subject
  d
}


# ---- 1. nlme: the engine whose SE / CI the collision corrupted ----------

test_that("an lme grouping factor named Residual is refused, with the reason", {
  d <- .orthodont_residual_named()
  fit <- nlme::lme(distance ~ age, data = d, random = ~ 1 | Residual)

  expect_error(
    table_regression(fit),
    class = "spicy_unsupported"
  )
  err <- tryCatch(table_regression(fit), error = function(e) e)
  msg <- paste(conditionMessage(err), collapse = "\n")
  # It names the word that is taken, the fit's own class, and the move.
  expect_match(msg, "\"Residual\"", fixed = TRUE)
  expect_match(msg, "lme", fixed = TRUE)
  expect_match(msg, "Rename the grouping variable and refit.", fixed = TRUE)
})


# ---- 2. lme4: the same reserved word, the same refusal -----------------

test_that("an lmer grouping factor named Residual is refused too", {
  skip_if_not_installed("lme4")
  d <- .orthodont_residual_named()
  fit <- lme4::lmer(distance ~ age + (1 | Residual), data = d)

  expect_error(
    spicy:::as_regression_frame(fit),
    class = "spicy_unsupported"
  )
})


# ---- 3. Controls: the residual row itself is not the collision ---------

test_that("an ordinary mixed fit still builds", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  fr <- expect_no_error(spicy:::as_regression_frame(fit))
  vc <- fr$info$random_effects$variance_components
  # The residual row is there, and it is what the empty term marks.
  expect_true(any(vc$group == "Residual" & !nzchar(vc$term)))
})

test_that("a non-mixed fit is not asked about a block it has not got", {
  fr <- expect_no_error(spicy:::as_regression_frame(lm(mpg ~ wt, mtcars)))
  expect_identical(nrow(fr$info$random_effects$variance_components), 0L)
})

test_that("the guard reads the term, not merely the group name", {
  # A frame whose ONLY Residual row is the residual (empty term) passes;
  # one extra row under that group with a term of its own does not.
  ok <- list(
    variance_components = data.frame(
      group = c("Subject", "Residual"),
      term = c("(Intercept)", ""),
      stringsAsFactors = FALSE
    )
  )
  expect_null(spicy:::.assert_no_reserved_residual_group(
    ok,
    lm(mpg ~ wt, mtcars)
  ))

  bad <- list(
    variance_components = data.frame(
      group = c("Residual", "Residual"),
      term = c("(Intercept)", ""),
      stringsAsFactors = FALSE
    )
  )
  expect_error(
    spicy:::.assert_no_reserved_residual_group(bad, lm(mpg ~ wt, mtcars)),
    class = "spicy_unsupported"
  )
})
