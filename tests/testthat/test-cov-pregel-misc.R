# Coverage tests for a scatter of arms no other test file reaches: the
# generic half of the empty-table refusal, the SMD cell's decimal mark,
# four of `inline()`'s address-resolution fallbacks, the non-HTML knit
# safety net, the template-hole checker's float and positional dummies,
# two `copy_clipboard()` arms, the non-finite partial F, the weighted
# SMD's empty group, two title/footer degradations, two hint wordings
# and the nlme variance block without intervals.
#
# Nothing here is a touch-the-line test: every block asserts the
# user-visible behaviour the line implements, and most pin the
# complementary arm beside it so the pair cannot drift apart.
#
# Lines closed:
#   R/table_regression.R        2877, 2882
#   R/table_categorical.R       1546
#   R/inline.R                  308, 386, 449, 666
#   R/regression_dispatch.R     1344-1347
#   R/i18n.R                    866, 876
#   R/copy_clipboard.R          183, 233
#   R/lm_compute.R              435
#   R/smd.R                     228
#   R/regression_titlefooter.R  83, 1522
#   R/regression_validate.R     707, 1180
#   R/regression_frame_nlme.R   881

# The clipboard is a shared resource: a test run must never overwrite
# what the user has copied. Same shape as the helper in
# test-copy_clipboard.R, copied because a helper defined inside a test
# file is not visible from another one.
.pregel_mocked_clipr <- function(
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

# knitr's pandoc target, restored on exit. Same shape as
# `.with_pandoc_to()` in test-quarto_word_rendering.R.
.pregel_with_pandoc_to <- function(to, code) {
  old <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  knitr::opts_knit$set(rmarkdown.pandoc.to = to)
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old), add = TRUE)
  force(code)
}


# ---- table_regression.R 2877, 2882: the GENERIC empty-table advice ----

test_that("a fit with no coefficients is refused with the generic advice", {
  # The refusal has two voices. An intercept-only ordinal whose
  # cut-points were switched off is told about `show_thresholds`; every
  # other empty fit gets the generic pair of bullets. `mpg ~ 0` has an
  # empty coefficient vector and no thresholds at all, so it takes the
  # generic arm -- the ordinal advice would name an argument that has
  # nothing to re-enable here.
  err <- expect_error(
    table_regression(lm(mpg ~ 0, data = mtcars)),
    class = "spicy_empty_table"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "Model 1 has no rows to show.", fixed = TRUE)
  expect_match(
    msg,
    "Every row the model could contribute has been switched off.",
    fixed = TRUE
  )
  expect_match(
    msg,
    paste0(
      "Re-enable one of the row blocks (`show_thresholds`, `show_re`, ",
      "`show_components`)."
    ),
    fixed = TRUE
  )
  # And NOT the cut-point wording: this fit has no cut-points to hide.
  expect_false(grepl("cut-points", msg, fixed = TRUE))
})


# ---- table_categorical.R 1546: the SMD cell takes the table's mark ----

test_that("the SMD cell follows the table's decimal mark", {
  # The SMD is a number in the table like any other, so it prints under
  # the mark the table was asked for -- a French reader must not find
  # one full stop among the commas. Fixture built by hand (no RNG) so
  # the value is pinned, not merely comma-shaped.
  d <- data.frame(
    grp = factor(rep(c("c", "t"), each = 30)),
    trait = factor(c(
      rep("p", 8),
      rep("q", 16),
      rep("r", 6),
      rep("p", 13),
      rep("q", 10),
      rep("r", 7)
    ))
  )
  tc <- table_categorical(d, trait, by = grp, smd = TRUE, decimal_mark = ",")

  # `inline()` reads the same formatted cell the printed table shows.
  cell <- inline(tc, trait, column = "{smd}")
  expect_identical(cell, "0,43")
  expect_false(grepl(".", cell, fixed = TRUE))

  # The default mark is unchanged: same data, same value, a full stop.
  tc_dot <- table_categorical(d, trait, by = grp, smd = TRUE)
  expect_identical(inline(tc_dot, trait, column = "{smd}"), "0.43")
})


# ---- inline.R 308: the missing category answers to its English name ----

test_that("the missing category answers to `(Missing)` in every language", {
  withr::local_options(spicy.language = "fr")
  d <- data.frame(trait = factor(c(rep("a", 20), rep("b", 15), rep(NA, 5))))
  tb <- table_categorical(d, trait, drop_na = FALSE)

  # The row's own `.level` is the translated label, so the French
  # spelling is an IDENTITY match and never reaches the role fallback.
  s <- as_structured(tb)
  miss <- s$body$.row_role == "missing"
  expect_identical(s$body$.level[miss], "(Manquant)")

  # `?inline` documents "(Missing)", and that spelling has to keep
  # working once the table speaks another language: it matches no
  # `.level` here, so the role fallback resolves it to the missing row.
  expect_identical(inline(tb, trait, level = "(Missing)"), "5")
  expect_identical(inline(tb, trait, level = "(Manquant)"), "5")

  # A level that is neither a spelling of the missing row nor a real
  # level is still refused.
  expect_error(
    inline(tb, trait, level = "(Absent)"),
    class = "spicy_invalid_input"
  )
})


# ---- inline.R 386: a token that does not spread keeps `level` as a row ----

test_that("`level` addresses a column only for a token that spreads", {
  d <- mtcars
  d$grp <- factor(ifelse(d$am == 1, "manual", "auto"))
  tl <- table_continuous_lm(d, c(mpg, hp), by = grp)

  # The group means DO spread sideways -- one column per `by` level --
  # so there `level` is a column address and picks one of them.
  expect_identical(
    inline(tl, mpg, column = "{emmean}", level = "manual"),
    "24.39"
  )
  expect_identical(
    inline(tl, mpg, column = "{emmean}", level = "auto"),
    "17.15"
  )
  # Without one, the ambiguity is named.
  expect_error(
    inline(tl, mpg, column = "{emmean}"),
    "matches 2 columns",
    class = "spicy_invalid_input"
  )

  # The p belongs to the comparison, not to a group: it occupies ONE
  # column, so the column rule does not engage and `level` keeps meaning
  # the row it has always meant -- of which this table has none.
  expect_identical(inline(tl, mpg, column = "{p}"), "<.001")
  err <- expect_error(
    inline(tl, mpg, column = "{p}", level = "manual"),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "No level \"manual\" for \"mpg\".",
    fixed = TRUE
  )
})


# ---- inline.R 449: a token the table does not carry falls through ----

test_that("a token the table does not carry gets the message it always got", {
  d <- data.frame(
    grp = factor(rep(c("c", "t"), each = 15)),
    trait = factor(rep(c("p", "q", "r"), 10))
  )
  tc <- table_categorical(d, trait, by = grp)

  # The header rule exists so a block statistic can be cited without a
  # level: the p lives on the variable's own row and resolves there.
  expect_identical(inline(tc, trait, column = "{p}"), "1.000")

  # `smd` is not in this table (it was built without `smd = TRUE`), so
  # the rule has no columns to reason about and declines to rule. The
  # caller then gets the plain "pick a level" refusal -- the message
  # that was there before the header rule existed -- rather than a
  # header-flavoured one about a statistic the table never had.
  err <- expect_error(
    inline(tc, trait, column = "{smd}"),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "\"trait\" has levels: pick one with `level`.",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err),
    "Available: \"p\", \"q\", \"r\".",
    fixed = TRUE
  )
})


# ---- inline.R 666: the empty-cell refusal without a structured table ----

test_that("the empty-cell refusal works without any table context", {
  # The context arguments are optional, and the promise attached to
  # that default is a BARE refusal: the head sentence and nothing else.
  # No in-package caller omits them, so the promise is tested here.
  err <- expect_error(
    spicy:::.inline_refuse_empty("", "delta"),
    class = "spicy_invalid_input"
  )
  expect_identical(
    conditionMessage(err),
    "The \"delta\" cell of this row is empty in the table."
  )
  # One sentence: no bullet was appended.
  expect_false(grepl("\n", conditionMessage(err), fixed = TRUE))
  expect_identical(
    spicy:::.inline_empty_cell_hints(NULL, "delta", NULL, NULL),
    character(0)
  )

  # With a real table the SAME refusal earns its hints -- which is what
  # makes the bare form a fallback rather than the norm.
  d <- mtcars
  d$grp <- factor(ifelse(d$am == 1, "manual", "auto"))
  tl <- table_continuous_lm(d, mpg, by = grp, contrast = "none")
  err2 <- expect_error(inline(tl, mpg), class = "spicy_invalid_input")
  expect_match(
    conditionMessage(err2),
    "The \"delta\" column is empty on EVERY row",
    fixed = TRUE
  )

  # A non-empty cell is not refused at all.
  expect_null(spicy:::.inline_refuse_empty("1.23", "delta"))
})


# ---- regression_dispatch.R 1344-1347: the docx source-note safety net ----

test_that("a note-less gt gains the spicy note as a source note off HTML", {
  skip_if_not_installed("gt")
  skip_if_not_installed("knitr")
  # pandoc drops raw HTML, so on a Word / PDF target the note has to
  # travel as a native gt source note. Every spicy builder now attaches
  # it at build time; this arm is the safety net for a tagged gt that
  # reached the knit path without one, so the fixture is built by hand.
  g <- gt::gt(data.frame(a = 1:2, b = c("x", "y")))
  expect_length(g[["_source_notes"]], 0L)
  attr(g, "spicy_note") <- "First line.\nSecond line."
  class(g) <- c("spicy_gt", class(g))

  handed_on <- NULL
  out <- .pregel_with_pandoc_to(
    "docx",
    testthat::with_mocked_bindings(
      spicy:::knit_print.spicy_gt(g),
      knit_print = function(x, ...) {
        handed_on <<- x
        "delegated"
      },
      .package = "knitr"
    )
  )

  expect_identical(out, "delegated")
  # The object handed to gt's own knit_print carries the note once, and
  # is a plain gt again (the spicy tag is off).
  expect_false(inherits(handed_on, "spicy_gt"))
  expect_length(handed_on[["_source_notes"]], 1L)
  # Newlines flattened to spaces: a source note is one run of text.
  expect_identical(
    as.character(handed_on[["_source_notes"]][[1L]]),
    "First line. Second line."
  )
})


# ---- i18n.R 866, 876: float and positional template holes ----

test_that("the hole checker types float holes and counts positional ones", {
  compatible <- spicy:::.spicy_holes_compatible

  # A `%f` hole is filled with a DOUBLE, so precision is the label
  # author's business and a replacement may change it.
  expect_true(compatible("Mean %.2f", "Mean %.1f"))
  expect_true(compatible("Moyenne %s", "Mean %.1f"))
  # Dropping the hole loses the number the table meant to show
  # (sprintf warns); adding one leaves it unfillable (sprintf errors).
  expect_false(compatible("Mean", "Mean %.1f"))
  expect_false(compatible("Mean %.1f (%s)", "Mean %.1f"))
  # The holes are typed positionally, not counted: a template whose
  # float and string holes are swapped takes the wrong arguments.
  expect_true(compatible("Score of %s: %.2f", "%s scored %.1f"))
  expect_false(compatible("%.1f scored %s", "%s scored %.1f"))

  # A positional reference is checked at its own arity, not at the
  # number of holes it spells: `note_gloss_smd` writes five holes for
  # four arguments (`%1$s` twice).
  ref <- spicy:::.spicy_strings[["note_gloss_smd"]]
  expect_match(ref, "%1$s", fixed = TRUE)
  expect_true(compatible("%1$s: %2$s vs %3$s, threshold %4$s (|%1$s|)", ref))
  expect_false(compatible("%1$s only", ref))
  # Reordering is free; reaching past the arity is not.
  expect_true(compatible("%2$d for %1$s", "%1$s of %2$d"))
  expect_false(compatible("%1$s %2$d %3$s", "%1$s of %2$d"))
  # Arity is the HIGHEST position cited, so a template that skips one
  # still asks for an argument there; the gap is filled with a string,
  # and a replacement that uses the skipped position is accepted.
  expect_true(compatible("%1$s and %2$s", "%2$s alone"))
  expect_false(compatible("%1$s and %2$d", "%2$s alone"))

  # The public consequence: an override that drops the holes is refused
  # at the `options()` line rather than inside the next table call.
  withr::local_options(spicy.labels = c(note_gloss_smd = "SMD only"))
  err <- expect_error(spicy_labels(), class = "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "does not fit what spicy fills it with",
    fixed = TRUE
  )
})


# ---- copy_clipboard.R 183: a data frame's row names, named by the caller ----

test_that("copy_clipboard() names the row-names column as asked for a data frame", {
  skip_if_not_installed("clipr")
  captured <- NULL
  ret <- NULL
  .pregel_mocked_clipr(
    {
      df <- data.frame(value = c(10, 20), row.names = c("a", "b"))
      ret <- copy_clipboard(df, row_names_as_col = "id", quiet = TRUE)
    },
    write_clip = function(x, ...) {
      captured <<- x
      invisible(NULL)
    }
  )

  # `TRUE` names the column "rownames"; a string names it itself, and
  # the row names travel as its values -- for a data frame exactly as
  # for a matrix.
  expect_identical(names(captured)[1L], "id")
  expect_identical(captured$id, c("a", "b"))
  expect_identical(captured$value, c(10, 20))
  # The invisible return is the transformed payload, as documented.
  expect_identical(ret, captured)
})


# ---- copy_clipboard.R 233: the success banner in colour ----

test_that("copy_clipboard() prints its success banner when the console has colour", {
  skip_if_not_installed("clipr")
  skip_if_not_installed("crayon")
  # Two levers, because testthat turns colour OFF for reproducible
  # output and `crayon.enabled` alone does not always win: crayon reads
  # `cli.num_colors` first, and returns it unconditionally. Setting both
  # makes the coloured arm deterministic wherever this runs.
  withr::local_options(crayon.enabled = TRUE, cli.num_colors = 8)
  expect_true(crayon::has_color())

  out <- NULL
  .pregel_mocked_clipr({
    out <- capture.output(
      copy_clipboard(data.frame(a = 1), quiet = FALSE, show_message = TRUE)
    )
  })
  banner <- out[grepl(
    "Data successfully copied to clipboard!",
    out,
    fixed = TRUE
  )]
  expect_length(banner, 1L)
  # And it is GREEN: the style is built lazily, only when the banner is
  # actually printed, so stripping the styling has to change the line.
  expect_false(identical(banner, crayon::strip_style(banner)))
  expect_identical(
    trimws(crayon::strip_style(banner)),
    "Data successfully copied to clipboard!"
  )

  # `quiet = TRUE` suppresses it, colour or no colour.
  out_quiet <- NULL
  .pregel_mocked_clipr({
    out_quiet <- capture.output(
      copy_clipboard(data.frame(a = 1), quiet = TRUE, show_message = TRUE)
    )
  })
  expect_length(out_quiet, 0L)
})


# ---- lm_compute.R 435: an F that is not finite yields no effect size ----

test_that("a perfectly fitted model reports no partial effect size", {
  # A response with no variation at all is fitted exactly: the residual
  # sum of squares is 0, so the partial F's denominator is 0 and the
  # ratio is not a number. An effect size is then absent, never Inf.
  n <- 24
  d <- data.frame(
    cov_z = as.numeric(seq_len(n)),
    grp = factor(rep(c("a", "b", "c"), length.out = n)),
    outcome = 0
  )
  fit <- lm(outcome ~ grp + cov_z, data = d)
  expect_identical(stats::deviance(fit), 0)
  expect_null(spicy:::compute_lm_type2_f_stat(fit, "grp"))
  expect_null(spicy:::extract_lm_focal_f_stat(fit, "grp"))

  # The public consequence: the adjusted table asks for f^2 and gets NA
  # rather than an infinite number in the cell.
  tl <- suppressWarnings(
    table_continuous_lm(
      d,
      outcome,
      by = grp,
      covariates = cov_z,
      effect_size = "f2"
    )
  )
  tidy <- as.data.frame(tl)
  expect_identical(tidy$es_type[[1L]], "f2")
  expect_true(is.na(tidy$es_value[[1L]]))

  # A model that is NOT perfectly fitted still gets its number, so the
  # guard is about the degenerate case and nothing else.
  d$outcome <- as.numeric(seq_len(n)) + rep(c(0, 1, 2), length.out = n)
  fs <- spicy:::compute_lm_type2_f_stat(
    lm(outcome ~ grp + cov_z, data = d),
    "grp"
  )
  expect_true(is.finite(fs$f_obs))
})


# ---- smd.R 228: a weighted group with no usable observation ----

test_that("a weighted group with nothing to average yields a bare NA SMD", {
  # The weighted branch answers what the unweighted one answers: a
  # group with no observation left after the keep filter has no mean
  # and no variance, so the SMD is NA rather than a number computed
  # from nothing.
  expect_identical(
    spicy:::.smd_moments_base(c(NA_real_, NA_real_), w = c(1, 2)),
    c(NA_real_, NA_real_)
  )
  expect_identical(
    spicy:::.smd_moments_base(c(1, 2), w = c(0, 0)),
    c(NA_real_, NA_real_)
  )
  expect_identical(
    spicy:::.smd_moments_base(c(1, 2), w = c(NA_real_, NA_real_)),
    c(NA_real_, NA_real_)
  )
  # Same answer as the unweighted twin.
  expect_identical(
    spicy:::.smd_moments_base(c(NA_real_, NA_real_)),
    c(NA_real_, NA_real_)
  )

  # Through a real table: one group is entirely missing on the variable,
  # so the weighted moments of that group do not exist and the SMD cell
  # is NA.
  d <- data.frame(
    grp = factor(rep(c("c", "t"), each = 6)),
    score = c(rep(NA_real_, 6), 1, 2, 3, 4, 5, 6),
    wgt = rep(c(1, 2, 3), 4)
  )
  tb <- table_continuous(
    d,
    score,
    by = grp,
    smd = TRUE,
    weights = wgt,
    drop_na = FALSE,
    p_value = FALSE,
    statistic = FALSE,
    effect_size = "none"
  )
  tidy <- as.data.frame(tb)
  expect_true(all(is.na(tidy$smd_value)))
})


# ---- regression_titlefooter.R 83: a translated prefix, no outcome to name ----

test_that("a translated title falls back to the bare prefix without an outcome", {
  # `build_regression_title_from_frames()` names the outcome after the
  # prefix. A frame that cannot name one -- a class whose builder found
  # no DV -- gets the prefix alone, and that has to hold in the
  # TRANSLATED arm too, not only in the English one: half a French
  # title over a missing outcome is the hybrid decision 42 forbids.
  fr <- list(
    info = list(dv = NULL, extras = list(title_prefix = "Linear regression"))
  )
  expect_identical(
    spicy:::build_regression_title_from_frames(list(fr)),
    "Linear regression"
  )
  withr::local_options(spicy.language = "fr")
  fr_title <- spicy:::build_regression_title_from_frames(list(fr))
  # Translated -- and the translated prefix is the WHOLE title: nothing
  # is appended, so no colon and no outcome clause follow it.
  expect_identical(fr_title, spicy:::.title_prefix_display("Linear regression"))
  expect_false(identical(fr_title, "Linear regression"))
  expect_false(grepl(":", fr_title, fixed = TRUE))

  # An NA outcome is the same "cannot be named" case as a NULL one.
  fr_na <- list(
    info = list(
      dv = NA_character_,
      extras = list(title_prefix = "Logistic regression")
    )
  )
  expect_identical(
    spicy:::build_regression_title_from_frames(list(fr_na)),
    spicy:::.title_prefix_display("Logistic regression")
  )

  # With an outcome the translated template puts it back, after the
  # very same prefix -- which is what makes the bare form a fallback.
  fr_dv <- list(
    info = list(dv = "mpg", extras = list(title_prefix = "Linear regression"))
  )
  with_dv <- spicy:::build_regression_title_from_frames(list(fr_dv))
  expect_match(with_dv, "mpg", fixed = TRUE)
  expect_true(startsWith(with_dv, fr_title))
  expect_gt(nchar(with_dv), nchar(fr_title))
})


# ---- regression_titlefooter.R 1522: a frame with no fit-statistics list ----

test_that("the ML-refit note needs a frame that actually carries change stats", {
  # The note annotates change ROWS, so it is only earned when a frame
  # holds a finite change statistic. A frame whose `fit_stats` is not a
  # list never went through attach_nested_stats_to_frames() and cannot
  # claim one -- it contributes FALSE rather than erroring on a missing
  # key.
  bare <- list(
    info = list(
      class = "lmerMod",
      fit_stats = NA,
      random_effects = list(method = "REML")
    )
  )
  bare2 <- list(
    info = list(
      class = "lmerMod",
      fit_stats = NULL,
      random_effects = list(method = "REML")
    )
  )
  expect_null(
    spicy:::build_nested_ml_refit_footer_block_from_frames(
      list(bare, bare2),
      nested = TRUE,
      show_fit_stats = c("nobs", "aic_change")
    )
  )

  # One frame with a real change statistic beside the bare one is
  # enough: the scan is an `any`, and the note appears.
  attached <- list(
    info = list(
      class = "lmerMod",
      fit_stats = list(aic_change = -3.5),
      random_effects = list(method = "REML")
    )
  )
  expect_match(
    spicy:::build_nested_ml_refit_footer_block_from_frames(
      list(bare, attached),
      nested = TRUE,
      show_fit_stats = c("nobs", "aic_change")
    ),
    "maximum-likelihood refits",
    fixed = TRUE
  )
})


# ---- regression_validate.R 707: the multi-model wording of the vcov hint ----

test_that("the unknown-`vcov` hint says \"these models\" for a model set", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)

  # With several models the valid types are the UNION of what they can
  # each compute, so the hint cannot name one class and says so.
  err <- expect_error(
    table_regression(list(m1, m2), vcov = "nonsense"),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "Unknown `vcov` type(s): \"nonsense\".", fixed = TRUE)
  expect_match(msg, "Valid types for these models:", fixed = TRUE)
  expect_false(grepl("Valid types for `lm`", msg, fixed = TRUE))

  # A single model can be named, and is: the pair is pinned together so
  # neither wording drifts into the other's case.
  err1 <- expect_error(
    table_regression(m1, vcov = "nonsense"),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err1), "Valid types for `lm`:", fixed = TRUE)
  expect_false(grepl("these models", conditionMessage(err1), fixed = TRUE))
})


# ---- regression_validate.R 1180: the iv_robust cluster hint ----

test_that("the estimatr cluster hint names the function that fitted the model", {
  skip_if_not_installed("estimatr")
  # estimatr clusters at fit time, so the generic "set `vcov` to CR*"
  # advice would send the user into the hard refusal Step 6c raises.
  # The hint names the fitting call instead -- and it has to name the
  # RIGHT one: an instrumental-variables fit is refitted with
  # iv_robust(), never with lm_robust().
  d <- data.frame(
    z = rep(c(-1, 1), 30),
    cl = factor(rep(1:6, each = 10))
  )
  d$x <- d$z + rep(c(0.5, -0.5, 0.25, -0.25, 0, 1), 10)
  d$y <- d$x + rep(c(0.1, -0.1), 30)
  iv <- estimatr::iv_robust(y ~ x | z, data = d)

  w <- expect_warning(
    table_regression(iv, cluster = d$cl),
    class = "spicy_ignored_arg"
  )
  msg <- conditionMessage(w)
  expect_match(msg, "estimatr::iv_robust(..., clusters = <var>", fixed = TRUE)
  expect_false(grepl("estimatr::lm_robust", msg, fixed = TRUE))

  # The sibling keeps its own name.
  lr <- estimatr::lm_robust(y ~ x, data = d)
  w2 <- expect_warning(
    table_regression(lr, cluster = d$cl),
    class = "spicy_ignored_arg"
  )
  expect_match(
    conditionMessage(w2),
    "estimatr::lm_robust(..., clusters = <var>",
    fixed = TRUE
  )
})


# ---- regression_frame_nlme.R 881: variance components without intervals ----

test_that("nlme variance components stay listed when intervals cannot be had", {
  skip_if_not_installed("nlme")
  fm <- nlme::lme(
    distance ~ age,
    random = ~ 1 | Subject,
    data = nlme::Orthodont
  )

  # Normally the block carries a Wald SE and CI derived from
  # nlme::intervals().
  body_ok <- as_structured(table_regression(fm))$body
  vc_ok <- body_ok[body_ok$.row_role == "vc", , drop = FALSE]
  expect_gt(nrow(vc_ok), 0L)
  expect_true(all(is.finite(vc_ok$SE)))

  # When intervals() cannot produce them -- and it does fail on fits
  # that are not singular -- the rows STAY: the estimate is still the
  # model's, only its uncertainty is undefined. Dropping the block
  # would hide variance components the fit really has.
  tb <- testthat::with_mocked_bindings(
    table_regression(fm),
    intervals = function(...) stop("no intervals here"),
    .package = "nlme"
  )
  body_na <- as_structured(tb)$body
  vc_na <- body_na[body_na$.row_role == "vc", , drop = FALSE]
  expect_identical(nrow(vc_na), nrow(vc_ok))
  expect_identical(vc_na$Variable, vc_ok$Variable)
  expect_identical(vc_na$B, vc_ok$B)
  expect_true(all(is.na(vc_na$SE)))
  expect_true(all(is.na(vc_na[["95% CI: LL"]])))
  expect_true(all(is.na(vc_na[["95% CI: UL"]])))

  # The fixed effects are untouched: only the variance block degrades.
  coef_na <- body_na[body_na$.row_role == "coef", , drop = FALSE]
  expect_true(all(is.finite(coef_na$SE)))
})
