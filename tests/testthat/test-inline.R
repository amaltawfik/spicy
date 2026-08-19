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

test_that("`model = k` addresses the k-th model, by identity", {
  # `model = k` used to resolve to `names(spans)[[k]]` and then re-enter
  # the list BY NAME. That is the k-th model only while no two labels
  # coincide -- and `[[` by name returns the first match, so when they
  # did, a sentence quoted a different model's coefficient with no
  # warning. Position is now kept, and this pins it against the model
  # identity rather than against the spanner order.
  # Three specifications whose `wt` coefficient separates at the
  # displayed precision (-5.34 / -3.88 / -5.05), so no k can pass by
  # landing on a neighbour that happens to print the same.
  d <- mtcars
  fits <- list(
    lm(mpg ~ wt, data = d),
    lm(mpg ~ wt + hp, data = d),
    lm(mpg ~ wt + qsec, data = d)
  )
  tbl <- .il_quiet(table_regression(fits))
  s <- as_structured(tbl)
  ids <- vapply(
    s$col_meta,
    function(e) e$model_id %||% NA_character_,
    character(1)
  )
  model_ids <- unique(stats::na.omit(unname(ids)))
  expect_length(model_ids, 3L)

  formatted <- spicy:::.format_structured_to_string_body(s)
  wt_row <- which(s$body$.variable == "wt")

  for (k in seq_along(model_ids)) {
    # The columns the k-th model owns, straight from the public
    # per-column identity -- never from the spanner label.
    cols <- names(ids)[!is.na(ids) & ids == model_ids[[k]]]
    b_col <- cols[vapply(
      cols,
      function(cn) identical(s$col_meta[[cn]]$token, "b"),
      logical(1)
    )]
    expect_length(b_col, 1L)
    expect_identical(
      inline(tbl, wt, column = "b", model = k),
      trimws(formatted[[b_col]][wt_row])
    )
  }

  # The three values are genuinely different, so the loop above cannot
  # pass by every k returning the same cell.
  vals <- vapply(
    seq_along(model_ids),
    function(k) inline(tbl, wt, column = "b", model = k),
    character(1)
  )
  expect_identical(anyDuplicated(vals), 0L)

  # Addressing by label still works and agrees with the position.
  spans <- attr(tbl, "spanners")
  for (k in seq_along(spans)) {
    expect_identical(
      inline(tbl, wt, column = "b", model = names(spans)[[k]]),
      vals[[k]]
    )
  }
})

test_that("`model = k` does not go out to the label and back", {
  # The property, isolated. `table_regression()` now refuses a table
  # whose models would share a label, so the failure this guards cannot
  # be reached through the front door -- which is exactly why the guard
  # has to be tested at the seam. Rename the spanners of a real
  # three-model table so that 1 and 3 collide, and ask for the third:
  # resolving `names(spans)[[3]]` and then `spans[[that]]` returns the
  # FIRST match, i.e. model 1's columns. Position cannot.
  d <- mtcars
  tbl <- .il_quiet(table_regression(list(
    lm(mpg ~ wt, data = d),
    lm(mpg ~ wt + hp, data = d),
    lm(mpg ~ wt + qsec, data = d)
  )))
  s <- as_structured(tbl)
  expect_length(s$spanners, 3L)
  names(s$spanners) <- c("Same", "Other", "Same")

  cols <- lapply(1:3, function(k) spicy:::.inline_model_cols(s, k))
  # Three models, three disjoint column sets -- k picks the k-th.
  expect_length(unique(unlist(cols)), length(unlist(cols)))
  expect_false(identical(cols[[1L]], cols[[3L]]))
  # And each set is the one the structured spanner at that POSITION
  # names, mapped out of the Variable-counting index space.
  body_cols <- names(spicy:::.struct_display_body(s$body))
  for (k in 1:3) {
    expect_identical(cols[[k]], body_cols[s$spanners[[k]]])
  }
})

test_that("an out-of-range or unknown `model` is refused, unchanged", {
  d <- as.data.frame(sochealth)
  tbl <- .il_quiet(table_regression(list(
    lm(wellbeing_score ~ age, data = d),
    lm(wellbeing_score ~ age + sex, data = d)
  )))
  for (bad in list(0, 3, "Nope")) {
    err <- tryCatch(
      inline(tbl, age, column = "b", model = bad),
      error = identity
    )
    expect_s3_class(err, "spicy_invalid_input")
    expect_match(conditionMessage(err), "Unknown model")
    expect_match(
      conditionMessage(err),
      "\"Model 1\", \"Model 2\"",
      fixed = TRUE
    )
  }
  # A single-model table has no spanners: `model` does not apply at all,
  # and that refusal is a different one.
  one <- .il_quiet(table_regression(lm(wellbeing_score ~ age, data = d)))
  err <- tryCatch(inline(one, age, column = "b", model = 1), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "no model spanners")
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


test_that("inline() refuses an interval whose bounds have no value", {
  # `?inline` promises that a cell the table shows as undefined refuses
  # with the reason rather than pasting a placeholder into a sentence.
  # The scalar columns kept that promise; the interval did not -- it
  # composed the two placeholder cells and returned "[-, -]", which is
  # exactly the sentence `inline()` exists to prevent. Both bounds are
  # checked, with the same classed error and the same wording as the
  # scalar path.
  tr <- table_regression(
    lm(wellbeing_score ~ age + sex, data = spicy::sochealth),
    factor_layout = "grouped",
    reference_style = "row"
  )
  for (col in c("b", "se", "p", "ci")) {
    expect_error(
      inline(tr, sex, "Female", column = col),
      "reference category",
      class = "spicy_invalid_input"
    )
  }

  tb <- suppressWarnings(table_continuous(
    data.frame(x = 42),
    show_columns = c("m", "sd", "ci", "n")
  ))
  for (col in c("sd", "ci")) {
    expect_error(
      inline(tb, x, column = col),
      "undefined",
      class = "spicy_invalid_input"
    )
  }
  # The guard is on the CELL, not on the column: a row that does have an
  # interval still returns it.
  expect_match(inline(tr, age, column = "ci"), "^\\[.*\\]$")
})

test_that("an interval token names its own bounds, not a rival estimand", {
  # "ci", "med_ci", "ame_ci" and "assoc_ci" are different estimands. The
  # bound lookup used to ignore the token and scan every LL / UL column,
  # so a table carrying two intervals reported an ambiguity and told the
  # reader to "pick a `model`" -- on tables that have no models, with an
  # empty list of choices ("Available: ."). Selecting by token first
  # makes each interval addressable and leaves the model message for the
  # one case where it is true.
  d <- spicy::sochealth
  two <- suppressWarnings(table_continuous(
    d,
    select = bmi,
    show_columns = c("m", "ci", "med", "med_ci")
  ))
  ci <- inline(two, bmi, column = "ci")
  med_ci <- inline(two, bmi, column = "med_ci")
  expect_match(ci, "^\\[.*\\]$")
  expect_match(med_ci, "^\\[.*\\]$")
  expect_false(identical(ci, med_ci))

  # Same shape on the regression side: the coefficient interval and the
  # AME interval are both addressable in one single-model table.
  g <- suppressWarnings(table_regression(
    glm(I(bmi > 30) ~ age + sex, data = d, family = stats::binomial()),
    show_columns = c("b", "ci", "p", "ame", "ame_ci")
  ))
  expect_match(inline(g, age, column = "ci"), "^\\[.*\\]$")
  expect_match(inline(g, age, column = "ame_ci"), "^\\[.*\\]$")

  # A table with no "ci" token now says so, and lists what it does
  # have, instead of quietly composing the association interval of a
  # row that has none.
  cat_tbl <- suppressWarnings(table_categorical(
    d,
    select = sex,
    by = smoking,
    assoc_ci = TRUE
  ))
  expect_error(
    inline(cat_tbl, sex, "Male", column = "ci"),
    "No column with token",
    class = "spicy_invalid_input"
  )

  # The genuine ambiguity survives, and it still names the models --
  # this is the only shape where `model` is the remedy, and the only one
  # where the spanner labels exist to be listed.
  mm <- table_regression(
    list(
      lm(bmi ~ age, data = d),
      lm(bmi ~ age + sex, data = d)
    ),
    show_columns = c("b", "ci")
  )
  expect_error(
    inline(mm, age, column = "ci"),
    "Model 1",
    class = "spicy_invalid_input"
  )
  expect_match(inline(mm, age, column = "ci", model = "Model 2"), "^\\[")
})


test_that("{ci_label} names the interval the pattern quotes", {
  # Residue of the lot D fix, one layer up: the bound lookup learned to
  # select by token, but the LABEL still returned the first `ci_label`
  # in column order. A table showing both intervals heads them "95% CI"
  # and "Med 95% CI" -- so a sentence quoting the median interval was
  # given the mean's label, contradicting the table three lines above it.
  d <- spicy::sochealth
  two <- suppressWarnings(table_continuous(
    d,
    select = bmi,
    show_columns = c("m", "ci", "med", "med_ci")
  ))
  s <- as_structured(two)
  labels <- unique(unlist(lapply(s$col_meta, function(m) m$ci_label)))
  # The premise: this table really does carry two different labels.
  expect_setequal(labels, c("95% CI", "Med 95% CI"))

  expect_match(
    inline(two, bmi, column = "{m} ({ci_label} {ci})"),
    "^[0-9.]+ \\(95% CI \\["
  )
  expect_match(
    inline(two, bmi, column = "{med} ({ci_label} {med_ci})"),
    "^[0-9.]+ \\(Med 95% CI \\["
  )
  # Two intervals in one sentence: the first one cited takes the label,
  # which is the one a reader pairs it with.
  expect_match(
    inline(two, bmi, column = "{med} ({ci_label} {med_ci}), M {m} {ci}"),
    "^[0-9.]+ \\(Med 95% CI \\["
  )
  # A pattern citing no interval keeps the plain scan.
  expect_identical(inline(two, bmi, column = "{ci_label}"), "95% CI")

  # One interval, one label: unchanged.
  tbl <- .il_quiet(table_regression(.il_fit()))
  expect_match(
    inline(tbl, sex, "Male", "{b} ({ci_label} {ci})"),
    "^3.90 \\(95% CI \\[2.14, 5.65\\]\\)$"
  )
})


test_that("{ci_label} follows the table's decimal_mark (decision 27)", {
  # The cited label is the table's label: a comma table says
  # "97,5% CI" in its header, so the sentence quoting it does too.
  d <- spicy::sochealth
  tc <- suppressWarnings(table_continuous(
    d,
    select = bmi,
    ci_level = 0.975,
    decimal_mark = ",",
    show_columns = c("m", "ci")
  ))
  expect_identical(inline(tc, bmi, column = "{ci_label}"), "97,5% CI")
  # The fractional-period case stays as lot F pinned it.
  tc_dot <- suppressWarnings(table_continuous(
    d,
    select = bmi,
    ci_level = 0.975,
    show_columns = c("m", "ci")
  ))
  expect_identical(inline(tc_dot, bmi, column = "{ci_label}"), "97.5% CI")
})


test_that("an interval with blank bounds refuses like the scalar does", {
  d <- spicy::sochealth
  ct <- suppressWarnings(table_categorical(
    d,
    select = sex,
    by = smoking,
    assoc_ci = TRUE
  ))
  # The association measure and its interval sit on the VARIABLE row;
  # a level row carries neither. `{assoc}` refused already, because the
  # scalar path checks for a blank cell -- `{assoc_ci}` composed the
  # brackets around two blanks and returned "[, ]". The lot D status
  # guard could not catch it: a blank bound carries no status at all.
  err <- tryCatch(
    inline(ct, sex, "Male", column = "assoc_ci"),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "The \"assoc_ci\" cell of this row is empty in the table.",
    fixed = TRUE
  )
  # Word for word the scalar refusal on the same row: one producer.
  err_scalar <- tryCatch(
    inline(ct, sex, "Male", column = "assoc"),
    error = function(e) e
  )
  expect_identical(
    sub("assoc_ci", "assoc", conditionMessage(err), fixed = TRUE),
    conditionMessage(err_scalar)
  )
  # In a pattern too, where the dash used to be pasted into a sentence.
  expect_error(
    inline(ct, sex, "Male", "{assoc} ({assoc_ci})"),
    class = "spicy_invalid_input"
  )

  # A populated interval is untouched, on both families.
  tbl <- .il_quiet(table_regression(.il_fit()))
  expect_match(inline(tbl, age, column = "ci"), "^[[]")
  two <- suppressWarnings(table_continuous(
    d,
    select = bmi,
    show_columns = c("m", "ci", "med", "med_ci")
  ))
  expect_match(inline(two, bmi, column = "med_ci"), "^[[]")
})


test_that("a multi-valued `model` is refused, not vectorised into `if`", {
  d <- as.data.frame(sochealth)
  tbl <- .il_quiet(table_regression(list(
    A = lm(wellbeing_score ~ age, d),
    B = lm(wellbeing_score ~ age + sex, d)
  )))
  err <- tryCatch(
    inline(tbl, age, column = "b", model = c("A", "B")),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "single value")
  err <- tryCatch(
    inline(tbl, age, column = "b", model = 1:2),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
})


test_that("inline() cites a continuous SMD cell, byte for byte", {
  # The mechanism is generic: `inline()` builds the typed view and
  # derives its vocabulary from `col_meta$token`, with no hard-coded
  # list of columns -- so a new column becomes citable with no work,
  # provided its cell is NOT a display override. The SMD's is a bare
  # number, so the string quoted in a sentence IS the string in the
  # table.
  d <- data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    x = c(1, 2, 4, 5, 2, 3, 8)
  )
  tbl <- .il_quiet(table_continuous(
    d,
    select = x,
    by = g,
    smd = TRUE,
    p_value = FALSE
  ))
  s <- as_structured(tbl)
  formatted <- spicy:::.format_structured_to_string_body(s)
  expect_identical(inline(tbl, x, "A", column = "smd"), formatted$SMD[[1L]])
  expect_identical(inline(tbl, x, "A", column = "smd"), "-0.51")

  # Non-regression: `"smd"` is not in the default column preference,
  # and must not be. A bare `inline(tbl, x, "A")` cites the MEAN --
  # asserted against the value, not against another inline() call:
  # comparing two inline() calls agrees with itself whatever the
  # default resolves to, which is how "n" passed for the mean here.
  expect_identical(inline(tbl, x, "A"), "3.00")
  expect_identical(inline(tbl, x, "A"), inline(tbl, x, "A", column = "m"))
  expect_false(identical(
    inline(tbl, x, "A"),
    inline(tbl, x, "A", column = "n")
  ))

  # KNOWN FAMILY LIMIT, pinned so it is not mistaken for a regression:
  # in a categorical table the SMD -- like `p` and the association
  # measure, neither of which is citable today either -- lives only on
  # the `factor_header` row, whose `.level` is NA, and
  # `.inline_resolve_row()` refuses `level = NULL` as soon as the
  # variable has levels. Unblocking it would unblock all three at once
  # and is a lot of its own.
  dc <- data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B")),
    bin = factor(c("no", "no", "no", "yes", "yes", "no", "yes"))
  )
  ct <- .il_quiet(table_categorical(dc, select = bin, by = g, smd = TRUE))
  expect_error(inline(ct, bin, column = "smd"), "pick one with")
  expect_error(inline(ct, bin, "no", column = "smd"), "empty")
  expect_error(inline(ct, bin, column = "assoc"), "pick one with")
})


test_that("a bare inline() cites the family's primary estimate", {
  # The documented default (`?inline`, `column = NULL`): the coefficient
  # for the two model families, the mean for the continuous one, the
  # count for the categorical one. Asserted against the VALUES, per
  # family, so no family can drift onto a neighbouring column unnoticed
  # -- which is exactly what happened to the continuous one, whose mean
  # token is "m" while the preference list looked for "mean" and found
  # "n" first.
  d <- data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    x = c(1, 2, 4, 5, 2, 3, 8)
  )
  dc <- data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    bin = factor(c("no", "no", "no", "yes", "yes", "no", "yes"))
  )

  # Continuous: the mean (M = 3.00 in group A), not the group's n (4).
  tc <- .il_quiet(table_continuous(d, select = x, by = g, p_value = FALSE))
  expect_identical(inline(tc, x, "A"), "3.00")
  expect_identical(inline(tc, x, "A"), inline(tc, x, "A", column = "m"))
  expect_identical(inline(tc, x, "A", column = "n"), "4")

  # And with no `by`: still the mean over all seven observations.
  tc1 <- .il_quiet(table_continuous(d, select = x))
  expect_identical(inline(tc1, x), inline(tc1, x, column = "m"))
  expect_false(identical(inline(tc1, x), inline(tc1, x, column = "n")))

  # Categorical: the count. (With `by`, "n" spans the groups, so the
  # bare call asks for a `model` rather than guessing one.)
  tk1 <- .il_quiet(table_categorical(dc, select = bin))
  expect_identical(inline(tk1, bin, "no"), inline(tk1, bin, "no", column = "n"))
  expect_identical(inline(tk1, bin, "no"), "4")
  tk <- .il_quiet(table_categorical(dc, select = bin, by = g))
  expect_error(inline(tk, bin, "no"), "pick one with `model`")

  # Regression: the coefficient.
  tr <- table_regression(lm(mpg ~ wt, data = mtcars))
  expect_identical(inline(tr, wt), inline(tr, wt, column = "b"))
  expect_identical(inline(tr, wt), "-5.34")

  # Linear-model descriptive table: the coefficient, not its n.
  tl <- .il_quiet(table_continuous_lm(mtcars, select = "mpg", by = "am"))
  expect_identical(inline(tl, mpg), inline(tl, mpg, column = "b"))
  expect_false(identical(inline(tl, mpg), inline(tl, mpg, column = "n")))
})
