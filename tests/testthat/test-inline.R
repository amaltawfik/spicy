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

test_that("a cited cell carries the style levers that have no formal", {
  # Half a style travels in the typed contract, because `digits`,
  # `p_digits` and `decimal_mark` are formals and are baked into
  # `col_meta` at build time. The other half -- `p_sigfig`, `p_bands`,
  # `p_floor`, `ci_sep`, `ci_brackets` -- has no formal and only ever
  # lived in the call-scoped format context, which is gone by the time
  # a sentence cites a cell. So `inline()` re-formatted under spicy's
  # defaults: a Lancet p came out at four decimals where the table
  # printed two significant figures, the floor lost its leading zero,
  # and the interval closed with the default comma instead of the
  # journal's en dash. Asserted against the table's OWN rendered cell
  # wherever the object carries one, and against the values otherwise.
  dot <- "\u00b7"
  dash <- "\u2013"
  tl <- .il_quiet(table_regression(.il_fit(), style = "lancet"))
  s <- as_structured(tl)
  age_row <- which(s$body$.variable == "age")
  int_row <- which(s$body$.variable == "(Intercept)")
  expect_identical(inline(tl, age, column = "p"), trimws(tl$p[age_row]))
  expect_identical(inline(tl, age, column = "p"), paste0("0", dot, "16"))
  expect_identical(inline(tl, "(Intercept)", column = "p"), trimws(tl$p[int_row]))
  expect_identical(
    inline(tl, "(Intercept)", column = "p"),
    paste0("<0", dot, "0001")
  )
  expect_identical(
    inline(tl, age, column = "ci"),
    paste0("[-0", dot, "02", dash, "0", dot, "10]")
  )

  # Both interval levers, hand-composed.
  ts <- .il_quiet(table_regression(
    .il_fit(),
    style = spicy_style(ci_brackets = c("(", ")"), ci_sep = " to ")
  ))
  expect_identical(inline(ts, age, column = "ci"), "(-0.02 to 0.10)")

  # And the descriptive families, which reach `inline()` by the same
  # road: the block p of a categorical / outcome table, and a floor
  # that parts ways with the decimals (JAMA: two decimals, floor .001).
  d <- as.data.frame(sochealth)
  tk <- .il_quiet(table_categorical(d, select = smoking, by = sex, style = "lancet"))
  expect_identical(inline(tk, smoking, column = "p"), paste0("0", dot, "71"))
  to <- .il_quiet(table_outcome(d, bmi, by = sex, style = "lancet"))
  expect_identical(inline(to, sex, column = "p"), paste0("0", dot, "018"))
  tj <- .il_quiet(table_continuous(
    d,
    select = wellbeing_score,
    by = sex,
    style = "jama"
  ))
  expect_identical(inline(tj, wellbeing_score, "Female", "p"), "<.001")
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

test_that("a real level named (Missing) wins over the role shortcut", {
  # This block used to assert the opposite, and it was pinning a bug.
  # With a REAL level literally named "(Missing)", the missing category
  # auto-renames itself "(Missing_1)" and the table shows both rows;
  # resolving "(Missing)" by role handed back the OTHER row's number,
  # silently. Identity now comes first, so the address a reader types
  # reaches the row a reader sees.
  d <- as.data.frame(sochealth)
  d$smoking <- as.character(d$smoking)
  d$smoking[d$smoking == "Yes"] <- "(Missing)"
  tc <- .il_quiet(table_categorical(d, select = smoking, drop_na = FALSE))
  s <- as_structured(tc)
  real_row <- which(s$body$.level == "(Missing)")
  miss_row <- which(s$body$.row_role == "missing")
  # The fixture is only worth anything if the two rows differ.
  expect_length(real_row, 1L)
  expect_length(miss_row, 1L)
  expect_false(identical(real_row, miss_row))
  expect_false(identical(s$body$n[real_row], s$body$n[miss_row]))

  expect_identical(
    inline(tc, smoking, "(Missing)", "n"),
    as.character(s$body$n[real_row])
  )
  # The auto-renamed missing row keeps its own address.
  expect_identical(
    inline(tc, smoking, "(Missing_1)", "n"),
    as.character(s$body$n[miss_row])
  )
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

  # A statistic that lives on the BLOCK is addressed without a level:
  # in a categorical table the SMD, the association measure and the
  # group comparison's `p` all sit on the `factor_header` row, whose
  # `.level` is NA. `level = NULL` resolves there, because those tokens
  # are filled on the header and blank on every level row of the block.
  #
  # The middle line is the witness that this did not spill over: an
  # EXPLICIT level still addresses the level row, where the SMD is
  # blank, and still refuses.
  dc <- data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B")),
    bin = factor(c("no", "no", "no", "yes", "yes", "no", "yes"))
  )
  ct <- .il_quiet(table_categorical(dc, select = bin, by = g, smd = TRUE))
  expect_identical(inline(ct, bin, column = "smd"), "-0.92")
  expect_error(inline(ct, bin, "no", column = "smd"), "empty")
  expect_identical(inline(ct, bin, column = "assoc"), ".42")
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

test_that("a bare inline() on a median-only table cites the median, not n", {
  # Register 55's failure mode in its non-parametric costume: a
  # median-only row carries no "m", and the preference list used to
  # fall through to the count. The median tokens now sit between "m"
  # and "n" -- bare before bracketed, so an explicit `med` column wins
  # over its `med_iqr` companion when the table shows both.
  d <- data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    x = c(1, 2, 4, 5, 2, 3, 8)
  )
  tm <- .il_quiet(table_continuous(
    d,
    select = x,
    by = g,
    show_columns = c("n", "med"),
    p_value = FALSE
  ))
  expect_identical(inline(tm, x, "A"), inline(tm, x, "A", column = "med"))
  expect_identical(inline(tm, x, "A"), "3.00")
  expect_identical(inline(tm, x, "A", column = "n"), "4")

  tq <- .il_quiet(table_continuous(
    d,
    select = x,
    by = g,
    show_columns = c("n", "med_iqr"),
    p_value = FALSE
  ))
  expect_identical(
    inline(tq, x, "A"),
    inline(tq, x, "A", column = "med_iqr")
  )
  expect_false(identical(inline(tq, x, "A"), "4"))
})

test_that("an exponentiated table's bare inline() still finds token 'b'", {
  # The OR header does not rename the token: an exponentiated
  # regression column carries "b" whatever its header says, and the
  # preference list holds no scale-named entries ("or", "irr", "hr")
  # because no table ever emits them.
  tr <- table_regression(
    glm(am ~ wt, data = mtcars, family = binomial),
    exponentiate = TRUE
  )
  expect_identical(inline(tr, wt), inline(tr, wt, column = "b"))
  expect_error(inline(tr, wt, column = "or"), "No column with token")
})

test_that("a block statistic is cited without a level", {
  # `table_categorical()` puts the statistics OF THE VARIABLE on the
  # block's header row: the group comparison's `p`, the association
  # measure, the SMD. Those rows carry no `.level`, so they can only be
  # addressed by leaving `level` out.
  dc <- data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B")),
    bin = factor(c("no", "no", "no", "yes", "yes", "no", "yes"))
  )
  ct <- .il_quiet(table_categorical(dc, select = bin, by = g))
  s <- as_structured(ct)
  formatted <- spicy:::.format_structured_to_string_body(s)
  hdr <- which(s$body$.row_role == "factor_header")

  # The cell, byte for byte against the table's own header row.
  expect_identical(inline(ct, bin, column = "p"), trimws(formatted$p[hdr]))
  # A pattern is a set of tokens, and the rule reads all of them.
  expect_identical(
    inline(ct, bin, column = "{p}"),
    trimws(formatted$p[hdr])
  )
  # A level statistic is NOT reachable that way: `n` is filled on every
  # level row, so the address stays ambiguous and the refusal stands --
  # the LEVEL refusal, listing the levels.
  err <- tryCatch(inline(ct, bin, column = "n"), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "pick one with `level`")
  expect_match(conditionMessage(err), "\"no\"")
  # A level still addresses its own row.
  expect_identical(inline(ct, bin, "no", column = "n", model = "A"), "3")
})

test_that("a mixed pattern does not resolve to the block header", {
  # `{p}` lives on the header, `{n}` on the levels: no single row
  # answers the whole pattern, so the address stays ambiguous.
  dc <- data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B")),
    bin = factor(c("no", "no", "no", "yes", "yes", "no", "yes"))
  )
  ct <- .il_quiet(table_categorical(dc, select = bin, by = g))
  expect_error(inline(ct, bin, column = "{p} ({n})"), "pick one with")
})

test_that("an empty block statistic refuses with its own reason", {
  # A single-level variable gives the chi-squared nothing to compare:
  # its `p` is blank on the header AND on the level. Pointing at the
  # levels would name a remedy that cannot help, so the refusal says
  # what actually happened.
  dc <- data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B")),
    one = factor(rep("yes", 7L))
  )
  ct <- .il_quiet(table_categorical(dc, select = one, by = g))
  err <- tryCatch(inline(ct, one, column = "p"), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "empty for")
  expect_match(conditionMessage(err), "group comparison did not run")
})

test_that("the header rule needs the statistic blank on every level", {
  # Both halves of the rule are load-bearing, and only one of them can
  # be witnessed through a real table (the other needs a body where a
  # token is filled on the header AND on a level -- no family emits
  # one today). Feed the predicate that body directly.
  dc <- data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B")),
    bin = factor(c("no", "no", "no", "yes", "yes", "no", "yes"))
  )
  ct <- .il_quiet(table_categorical(dc, select = bin, by = g))
  s <- as_structured(ct)
  formatted <- spicy:::.format_structured_to_string_body(s)
  cols <- spicy:::.inline_model_cols(s, NULL)
  rows <- which(s$body$.variable == "bin")
  hdr <- rows[s$body$.row_role[rows] == "factor_header"]

  # As the table stands, `p` resolves to the header.
  expect_identical(
    spicy:::.inline_header_row(s, formatted, rows, "bin", "p", cols),
    hdr
  )
  # Fill `p` on a level row too: the header is no longer the only
  # possible answer, so the rule must decline and let the caller be
  # told to pick a level.
  spilled <- formatted
  spilled$p[setdiff(rows, hdr)[1L]] <- ".270"
  expect_null(
    spicy:::.inline_header_row(s, spilled, rows, "bin", "p", cols)
  )
})


test_that("a real level named (Missing) is not shadowed by the role", {
  # F1, adversarial review of the table-outcome train. The role
  # shortcut used to run FIRST, so a variable carrying a real level
  # literally named "(Missing)" -- which makes the missing category
  # auto-rename itself "(Missing_1)" -- returned the OTHER row's
  # number. Silently: no error, no warning, and the table on screen
  # showed both rows with different values.
  #
  # Pinned across all three families that share `.inline_resolve_row()`,
  # and on VALUES that differ between the two rows, so a regression
  # cannot pass by agreeing with itself.
  g <- factor(
    c(rep("(Missing)", 5L), rep("a", 3L), rep(NA, 2L)),
    levels = c("(Missing)", "a")
  )
  d <- data.frame(g = g, y = seq_along(g))

  to <- .il_quiet(table_outcome(d, y, by = g))
  shown <- attr(to, "display_df")
  expect_identical(
    trimws(shown$Variable),
    c("Overall", "g", "(Missing)", "a", "(Missing_1)")
  )
  expect_identical(trimws(shown$n), c("10", "", "5", "3", "2"))
  expect_identical(inline(to, g, "(Missing)", "n"), "5")
  expect_identical(inline(to, g, "(Missing_1)", "n"), "2")

  ct <- .il_quiet(table_categorical(d, select = g))
  expect_identical(inline(ct, g, "(Missing)", "n"), "5")
  expect_identical(inline(ct, g, "(Missing_1)", "n"), "2")

  cn <- .il_quiet(table_continuous(
    d,
    select = y,
    by = g,
    p_value = FALSE,
    drop_na = FALSE
  ))
  expect_identical(inline(cn, y, "(Missing)", "n"), "5")
  expect_identical(inline(cn, y, "(Missing_1)", "n"), "2")
})

test_that("the missing category is still addressed by its documented name", {
  # The other half: with no collision, `level = "(Missing)"` reaches
  # the missing row exactly as `?inline` promises -- the exact match
  # and the role shortcut agree there, because the missing row's
  # `.level` IS its displayed label.
  d <- data.frame(
    g = factor(c("a", "a", "a", "b", "b", NA, NA)),
    y = c(1, 2, 3, 10, 20, 30, 40)
  )
  to <- .il_quiet(table_outcome(d, y, by = g))
  expect_identical(inline(to, g, "(Missing)", "n"), "2")
  ct <- .il_quiet(table_categorical(d, select = g))
  expect_identical(inline(ct, g, "(Missing)", "n"), "2")
  cn <- .il_quiet(table_continuous(
    d,
    select = y,
    by = g,
    p_value = FALSE,
    # This family drops the missing group by default; the address
    # under test only exists when the row is shown.
    drop_na = FALSE
  ))
  expect_identical(inline(cn, y, "(Missing)", "n"), "2")
})

test_that("the role fallback survives a translated missing label", {
  # Why the role shortcut exists at all: under a translated registry
  # the displayed label is no longer "(Missing)", and the documented
  # address must still work. Exercised through the predicate, which is
  # the only thing the registry moves.
  expect_true(spicy:::.inline_addresses_missing("(Missing)"))
  expect_true(spicy:::.inline_addresses_missing(spicy_str("row_missing_level")))
  expect_false(spicy:::.inline_addresses_missing("a"))
  expect_false(spicy:::.inline_addresses_missing("(Missing_1)"))
})
