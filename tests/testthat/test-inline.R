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
  expect_identical(
    inline(tl, "(Intercept)", column = "p"),
    trimws(tl$p[int_row])
  )
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
  tk <- .il_quiet(table_categorical(
    d,
    select = smoking,
    by = sex,
    style = "lancet"
  ))
  expect_identical(inline(tk, smoking, column = "p"), paste0("0", dot, "71"))
  to <- .il_quiet(table_outcome(d, bmi, select = sex, style = "lancet"))
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

test_that("a multi-model table requires and honors `model`", {
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
  # landing on a neighbor that happens to print the same.
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
  # With a REAL level literally named "(Missing)" beside the missing
  # category (relabeled here, since the tables refuse the two under one
  # label), resolving "(Missing)" by role handed back the OTHER row's
  # number, silently. Identity now comes first, so the address a reader
  # types reaches the row a reader sees.
  d <- as.data.frame(sochealth)
  d$smoking <- as.character(d$smoking)
  d$smoking[d$smoking == "Yes"] <- "(Missing)"
  withr::local_options(
    spicy.labels = list(row_missing_level = "(Not stated)")
  )
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
  # The relabeled missing row keeps its own address.
  expect_identical(
    inline(tc, smoking, "(Not stated)", "n"),
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
  skip_if_not_installed("marginaleffects")
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


test_that("a multi-valued `model` is refused, not vectorized into `if`", {
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
  # family, so no family can drift onto a neighboring column unnoticed
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

test_that("a bare inline() on a factor `by` cites the contrast, not n", {
  # Register 55's failure mode in its third costume, and the one the
  # function is named for. `table_continuous_lm()` emits token "b" only
  # across a NUMERIC `by` -- a slope; `mtcars$am` is 0/1, which is why
  # the block above passes. Across the levels of a FACTOR the estimate
  # is the contrast, token "delta", and the table carries no "b" at
  # all: the preference list fell through to "n", so a bare call quoted
  # the sample size (1188) where the sentence meant the mean difference
  # (0.51). Asserted on the values, per shape, so neither can drift
  # onto a neighboring column by agreeing with itself.
  d <- as.data.frame(sochealth)
  tf <- .il_quiet(table_continuous_lm(d, select = bmi, by = sex))
  expect_identical(inline(tf, bmi), inline(tf, bmi, column = "delta"))
  expect_identical(inline(tf, bmi), "0.51")
  expect_identical(inline(tf, bmi, column = "n"), "1188")
  # The numeric-`by` shape keeps its slope.
  tn <- .il_quiet(table_continuous_lm(mtcars, select = "mpg", by = "am"))
  expect_identical(inline(tn, mpg), inline(tn, mpg, column = "b"))
  expect_error(inline(tn, mpg, column = "delta"), "No column with token")
})

test_that("`level` cites a group column of table_continuous_lm()", {
  # This family lays its groups out SIDEWAYS: one row per outcome, the
  # `by` levels in the headers ("M (Female)", "M (Male)"), with the
  # level carried as data in `col_meta$level` precisely so a consumer
  # never parses the header back. `inline()` read row identity only, so
  # the two marginal-mean columns were an ambiguity it told the caller
  # to settle with `model` -- on a table with no spanners, listing
  # nothing ("Available: ."), while `model` itself refused with "this
  # table has no model spanners". Both remedies were dead ends and
  # `"emmean"`, a token of the published contract, was unreachable.
  d <- as.data.frame(sochealth)
  tl <- .il_quiet(table_continuous_lm(d, select = bmi, by = sex))
  s <- as_structured(tl)
  lv <- vapply(
    s$col_meta,
    function(m) m$level %||% NA_character_,
    character(1)
  )
  expect_setequal(stats::na.omit(unname(lv)), c("Female", "Male"))

  expect_identical(inline(tl, bmi, "Female", "emmean"), "25.69")
  expect_identical(inline(tl, bmi, "Male", "emmean"), "26.20")
  expect_identical(
    inline(tl, bmi, "Female", "{emmean} (n = {n})"),
    "25.69 (n = 1188)"
  )
  # A group the table does not have refuses with the groups it has --
  # the message that used to name `model` and list nothing.
  err <- tryCatch(inline(tl, bmi, "Nope", "emmean"), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "No group \"Nope\"", fixed = TRUE)
  expect_match(conditionMessage(err), "\"Female\", \"Male\"", fixed = TRUE)
  # Omitting it names `level`, not `model`, and lists the groups.
  err <- tryCatch(inline(tl, bmi, column = "emmean"), error = identity)
  expect_match(conditionMessage(err), "pick one with `level`", fixed = TRUE)
  expect_match(conditionMessage(err), "\"Female\", \"Male\"", fixed = TRUE)

  # The columns that belong to no group are cited without one, and the
  # rule does not spill onto the families whose levels are rows.
  expect_identical(inline(tl, bmi, column = "delta"), "0.51")
  expect_identical(inline(tl, bmi, column = "p"), ".018")
  tc <- .il_quiet(table_continuous(d, select = bmi, by = sex))
  expect_identical(inline(tc, bmi, "Female", "m"), "25.69")
  tr <- .il_quiet(table_regression(lm(wellbeing_score ~ age + sex, data = d)))
  expect_identical(inline(tr, sex, "Male", "b"), "3.90")
  expect_error(
    inline(tr, age, "Male", "b"),
    "No level",
    class = "spicy_invalid_input"
  )
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
  # literally named "(Missing)" beside the missing category returned the
  # OTHER row's number. Silently: no error, no warning, and the table on
  # screen showed both rows with different values. The tables now refuse
  # the two under one label; relabeled, the missing row still answers to
  # `level = "(Missing)"` by role, and the real level must win.
  #
  # Pinned across all three families that share `.inline_resolve_row()`,
  # and on VALUES that differ between the two rows, so a regression
  # cannot pass by agreeing with itself.
  g <- factor(
    c(rep("(Missing)", 5L), rep("a", 3L), rep(NA, 2L)),
    levels = c("(Missing)", "a")
  )
  d <- data.frame(g = g, y = seq_along(g))
  withr::local_options(
    spicy.labels = list(row_missing_level = "(Not stated)")
  )

  to <- .il_quiet(table_outcome(d, y, select = g))
  shown <- attr(to, "display_df")
  expect_identical(
    trimws(shown$Variable),
    c("Overall", "g", "(Missing)", "a", "(Not stated)")
  )
  expect_identical(trimws(shown$n), c("10", "", "5", "3", "2"))
  expect_identical(inline(to, g, "(Missing)", "n"), "5")
  expect_identical(inline(to, g, "(Not stated)", "n"), "2")

  ct <- .il_quiet(table_categorical(d, select = g))
  expect_identical(inline(ct, g, "(Missing)", "n"), "5")
  expect_identical(inline(ct, g, "(Not stated)", "n"), "2")

  cn <- .il_quiet(table_continuous(
    d,
    select = y,
    by = g,
    p_value = FALSE,
    drop_na = FALSE
  ))
  expect_identical(inline(cn, y, "(Missing)", "n"), "5")
  expect_identical(inline(cn, y, "(Not stated)", "n"), "2")
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
  to <- .il_quiet(table_outcome(d, y, select = g))
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
  withr::local_options(
    spicy.labels = list(row_missing_level = "(Not stated)")
  )
  expect_true(spicy:::.inline_addresses_missing("(Not stated)"))
})


# ---- table_multiresponse() ------------------------------------------------

# Three yes/no items of `sochealth` read as one "check all that apply"
# question.
.il_mr <- function(...) {
  .il_quiet(table_multiresponse(
    sochealth,
    select = c(smoking, physical_activity, dentist_12m),
    counted = "Yes",
    ...
  ))
}

test_that("a multiple-response table cites its items by token", {
  # One row per item, keyed by its column name. A percentage answers to
  # the token of what it is a percentage of, whatever its header ("%",
  # "% of cases", "% of responses").
  tbl <- .il_mr()
  expect_identical(inline(tbl, smoking), "249")
  expect_identical(inline(tbl, smoking), inline(tbl, smoking, column = "n"))
  expect_identical(inline(tbl, smoking, column = "pct"), "20.8")
  expect_error(
    inline(tbl, smoking, column = "pct_responses"),
    "No column with token",
    class = "spicy_invalid_input"
  )
  both <- .il_mr(percent = "both")
  expect_identical(inline(both, smoking, column = "pct"), "20.8")
  expect_identical(inline(both, smoking, column = "pct_responses"), "15.1")
  expect_identical(
    inline(both, dentist_12m, column = "pct_responses"),
    trimws(attr(both, "display_df")[["% of responses"]][[3L]])
  )
  resp <- .il_mr(percent = "responses")
  expect_identical(inline(resp, smoking, column = "pct_responses"), "15.1")
  expect_error(
    inline(resp, smoking, column = "pct"),
    "No column with token",
    class = "spicy_invalid_input"
  )
})

test_that("a multiple-response table by group cites a group, the margin and the item test", {
  # The groups are spanners, as in table_categorical(): `model` selects
  # one, the margin included. The item test belongs to no group, and its
  # p is the adjusted one the table shows (Holm; the raw p is .309).
  tbl <- .il_mr(by = region, statistic = TRUE)
  expect_identical(inline(tbl, smoking, column = "n", model = "Central"), "55")
  expect_identical(
    inline(tbl, smoking, column = "pct", model = "Central"),
    "22.4"
  )
  expect_identical(inline(tbl, smoking, column = "n", model = "Total"), "249")
  expect_identical(
    inline(tbl, smoking, column = "pct", model = "Total"),
    "20.8"
  )
  expect_error(
    inline(tbl, smoking),
    "pick one with `model`",
    class = "spicy_invalid_input"
  )
  expect_identical(inline(tbl, smoking, column = "p"), ".926")
  expect_identical(
    inline(tbl, smoking, column = "p"),
    trimws(attr(tbl, "display_df")$p[[1L]])
  )
  expect_identical(
    inline(tbl, smoking, column = "statistic"),
    "\u03c7\u00b2(5) = 5.98"
  )
  expect_identical(
    inline(tbl, smoking, column = "{statistic}, p = {p}"),
    "\u03c7\u00b2(5) = 5.98, p = .926"
  )
})

test_that("a multiple-response table without p-values has no p to cite", {
  # `p_value = FALSE` removes the column, so the address is refused like
  # any token the table does not carry, with the head the other families
  # use -- not with a level hint: an item row has no level.
  tbl <- .il_mr(by = region, p_value = FALSE)
  err <- expect_error(
    inline(tbl, smoking, column = "p"),
    "No column with token \"p\" in this table.",
    class = "spicy_invalid_input",
    fixed = TRUE
  )
  expect_false(grepl("level", conditionMessage(err), fixed = TRUE))
  tc <- .il_quiet(table_continuous(
    sochealth,
    select = bmi,
    by = region,
    p_value = FALSE
  ))
  err_tc <- expect_error(
    inline(tc, bmi, "Central", column = "p"),
    class = "spicy_invalid_input"
  )
  expect_identical(
    strsplit(conditionMessage(err), "\n", fixed = TRUE)[[1L]][[1L]],
    strsplit(conditionMessage(err_tc), "\n", fixed = TRUE)[[1L]][[1L]]
  )
  # `statistic` does not depend on `p_value`: shown alone, cited alone.
  st <- .il_mr(by = region, p_value = FALSE, statistic = TRUE)
  expect_identical(
    inline(st, smoking, column = "statistic"),
    "\u03c7\u00b2(5) = 5.98"
  )
  expect_error(
    inline(st, smoking, column = "p"),
    "No column with token",
    class = "spicy_invalid_input"
  )
})

test_that("the rows of none_row leave the items and the base row addressable", {
  # The "(No item chosen)" row sits between the items and the base row
  # (every respondent answered one of the three items). The items keep
  # their column names, the base its "nobs" token and its label, and the
  # row answers to its `.variable` token.
  tbl <- .il_mr(by = sex, base = "all")
  ref <- .il_mr(by = sex, base = "all", none_row = FALSE)
  s <- as_structured(tbl)
  expect_identical(s$body$.row_role[4:5], c("none", "fit_stat"))
  for (m in c("Female", "Total")) {
    expect_identical(
      inline(tbl, smoking, column = "{n} ({pct}%)", model = m),
      inline(ref, smoking, column = "{n} ({pct}%)", model = m)
    )
    expect_identical(
      inline(tbl, "nobs", model = m),
      inline(ref, "nobs", model = m)
    )
  }
  expect_identical(inline(tbl, "Respondents", model = "Total"), "1200")
  expect_identical(
    inline(tbl, smoking, column = "p"),
    inline(ref, smoking, column = "p")
  )
  expect_identical(inline(tbl, "none", model = "Total"), "166")
  expect_identical(inline(tbl, "(No item chosen)", model = "Total"), "166")
})

test_that("`model` keeps the columns that belong to no group", {
  # The `p` and the association measure of a `by` table, and the item
  # test of a multiple-response table, sit under no spanner. Selecting a
  # group used to drop them, and the table's own `p` was refused as a
  # token the table does not carry.
  tc <- .il_quiet(table_categorical(sochealth, select = smoking, by = sex))
  expect_identical(inline(tc, smoking, column = "p", model = "Female"), ".713")
  expect_identical(
    inline(tc, smoking, column = "p {p}, {assoc}", model = "Female"),
    "p .713, .01"
  )
  expect_identical(
    inline(tc, smoking, "Yes", "{n} ({pct}%)", model = "Female"),
    "131 (21.1%)"
  )
  mr <- .il_mr(by = sex, statistic = TRUE)
  expect_identical(
    inline(mr, smoking, column = "p", model = "Female"),
    inline(mr, smoking, column = "p")
  )
  expect_identical(
    inline(mr, smoking, column = "{n} ({pct}%), p {p}", model = "Female"),
    "131 (21.1%), p 1.000"
  )
  expect_identical(
    inline(mr, smoking, column = "statistic", model = "Male"),
    inline(mr, smoking, column = "statistic")
  )
})

test_that("a column the table does not carry is named before any level hint", {
  # On a block table the refusal used to be a level hint ("has levels:
  # pick one with `level`"), which cannot help when the column itself is
  # missing. It now names the column, with the head every family uses.
  tc <- .il_quiet(table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    p_value = FALSE
  ))
  err <- expect_error(
    inline(tc, smoking, column = "p"),
    "No column with token \"p\" in this table.",
    class = "spicy_invalid_input",
    fixed = TRUE
  )
  expect_false(grepl("level", conditionMessage(err), fixed = TRUE))
  na <- .il_quiet(table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    assoc_measure = "none"
  ))
  expect_error(
    inline(na, smoking, column = "assoc"),
    "No column with token \"assoc\" in this table.",
    class = "spicy_invalid_input",
    fixed = TRUE
  )
  # The same head on table_continuous(), with or without a level, and in
  # a pattern whose other token exists.
  head_of <- function(expr) {
    msg <- conditionMessage(expect_error(expr, class = "spicy_invalid_input"))
    strsplit(msg, "\n", fixed = TRUE)[[1L]][[1L]]
  }
  tn <- .il_quiet(table_continuous(
    sochealth,
    select = bmi,
    by = sex,
    p_value = FALSE
  ))
  expect_identical(
    head_of(inline(tn, bmi, column = "p")),
    head_of(inline(tc, smoking, column = "p"))
  )
  expect_identical(
    head_of(inline(tn, bmi, "Female", column = "p")),
    head_of(inline(tc, smoking, "Yes", "{n}, p {p}", model = "Female"))
  )
})

test_that("the base row of a multiple-response table is the fit statistic nobs", {
  # The row's `.variable` is the token of table_regression()'s number of
  # observations, so the address does not move with the translated label
  # ("Respondents"), which is accepted as a convenience.
  one <- .il_mr()
  expect_identical(inline(one, "nobs"), "1200")
  expect_identical(inline(one, "Respondents"), "1200")
  tbl <- .il_mr(by = region)
  expect_identical(inline(tbl, "nobs", model = "Central"), "245")
  expect_identical(inline(tbl, "nobs", model = "Total"), "1200")
  expect_error(
    inline(tbl, "nobs"),
    "pick one with `model`",
    class = "spicy_invalid_input"
  )
  # The base row carries counts only.
  expect_error(
    inline(tbl, "nobs", column = "pct", model = "Total"),
    "empty in the table",
    class = "spicy_invalid_input"
  )
})

test_that("a French multiple-response table cites its cells as displayed", {
  # The language sets the decimal comma, and the French p keeps its
  # leading zero. A cell carries no no-break space, as in the other
  # families: French typography puts them in the note.
  withr::local_options(spicy.language = "fr")
  tbl <- .il_mr(by = region, statistic = TRUE)
  disp <- attr(tbl, "display_df")
  expect_identical(
    inline(tbl, smoking, column = "pct", model = "Central"),
    "22,4"
  )
  expect_identical(
    inline(tbl, smoking, column = "pct", model = "Central"),
    trimws(disp[["Central %"]][[1L]])
  )
  expect_identical(inline(tbl, smoking, column = "p"), "0,926")
  expect_identical(
    inline(tbl, smoking, column = "statistic"),
    "\u03c7\u00b2(5) = 5,98"
  )
  expect_false(grepl(
    "\u00a0",
    inline(tbl, smoking, column = "{statistic}, p = {p}"),
    fixed = TRUE
  ))
  expect_identical(inline(tbl, "nobs", model = "Central"), "245")
  expect_identical(
    inline(tbl, "Personnes", model = "Central"),
    "245"
  )
})

test_that("a multiple-response item renamed by `labels` keeps its address", {
  # As in the other families, the row is found by identity: the column
  # name survives any label, and the displayed label is accepted as a
  # convenience. `sort = "-"` moves the rows, never the address.
  plain <- .il_mr(by = sex)
  named <- .il_mr(by = sex, labels = c(smoking = "Current smoker"))
  expect_identical(inline(named, smoking, column = "n", model = "Male"), "118")
  expect_identical(
    inline(named, smoking, column = "n", model = "Male"),
    inline(plain, smoking, column = "n", model = "Male")
  )
  expect_identical(
    inline(named, "Current smoker", column = "n", model = "Male"),
    "118"
  )
  from_attr <- .il_mr(by = sex, labels = TRUE)
  expect_identical(
    attr(from_attr, "display_df")$Variable[[3L]],
    "Dentist visit in last 12 months"
  )
  expect_identical(
    inline(from_attr, dentist_12m, column = "pct", model = "Total"),
    "70.5"
  )
  expect_identical(
    inline(.il_mr(sort = "-"), smoking, column = "pct"),
    inline(.il_mr(), smoking, column = "pct")
  )
})

test_that("a group given as `level` is pointed to `model`", {
  # An item row has no level, and the groups of a `by` table are
  # spanners: `level = "Female"` is the address a reader tries first, and
  # its refusal used to list nothing ("Available: ."). It now says why,
  # and names the argument that selects the group.
  tbl <- .il_mr(by = sex)
  err <- expect_error(
    inline(tbl, smoking, "Female", "n"),
    "No level \"Female\" for \"smoking\".",
    class = "spicy_invalid_input",
    fixed = TRUE
  )
  msg <- conditionMessage(err)
  expect_false(grepl("Available: .", msg, fixed = TRUE))
  expect_match(
    msg,
    "\"smoking\" has no levels: leave `level` out.",
    fixed = TRUE
  )
  expect_match(msg, "select it with `model = \"Female\"`.", fixed = TRUE)
  expect_identical(inline(tbl, smoking, column = "n", model = "Female"), "131")
  # The same hints on the other families: a categorical variable lists
  # its levels beside the group, a numeric term with no group says only
  # that `level` does not apply.
  tc <- .il_quiet(table_categorical(sochealth, select = smoking, by = sex))
  msg_tc <- conditionMessage(expect_error(
    inline(tc, smoking, "Female", "n"),
    class = "spicy_invalid_input"
  ))
  expect_match(
    msg_tc,
    "Available: \"No\", \"Yes\", \"(Missing)\".",
    fixed = TRUE
  )
  expect_match(msg_tc, "`model = \"Female\"`", fixed = TRUE)
  tr <- .il_quiet(table_regression(.il_fit()))
  msg_tr <- conditionMessage(expect_error(
    inline(tr, age, "Male", "b"),
    class = "spicy_invalid_input"
  ))
  expect_match(
    msg_tr,
    "\"age\" has no levels: leave `level` out.",
    fixed = TRUE
  )
  expect_false(grepl("model", msg_tr, fixed = TRUE))
})

test_that("an untested item and an empty group refuse with the reason", {
  # A dash in the table is a cell with no value: an item nobody chose has
  # no test, and a declared group with no respondent has no percentage.
  # Both refuse rather than quote the dash; the count of the empty group
  # is a real zero.
  d <- data.frame(
    g = factor(c("a", "a", "a", "b", "b", "b", "b"), levels = c("a", "b", "c")),
    x1 = c(1, 0, 1, 0, 0, 1, 1),
    x3 = c(0, 0, 0, 0, 0, 0, 0)
  )
  # `test = "chisq"`: x1 keeps its chi-squared statistic and p-value; under
  # "auto" its small groups take Fisher's exact test, with no statistic.
  tbl <- .il_quiet(table_multiresponse(
    d,
    c(x1, x3),
    by = g,
    test = "chisq",
    statistic = TRUE
  ))
  for (col in c("p", "statistic")) {
    expect_error(
      inline(tbl, x3, column = col),
      "undefined",
      class = "spicy_invalid_input"
    )
  }
  expect_error(
    inline(tbl, x1, column = "pct", model = "c"),
    "undefined",
    class = "spicy_invalid_input"
  )
  expect_identical(inline(tbl, x1, column = "n", model = "c"), "0")
  expect_identical(inline(tbl, x1, column = "p"), ".659")
})


# ---- identity first: tokens, labels, and their collisions -----------------

# Two items, the second named `nm`: x1 = 4, the item = 3, "(No item
# chosen)" = 2, "(No data for the question)" = 1, "Respondents" = 7. In
# group "a" (rows 1 to 3), one respondent chose no item.
.il_token_item <- function(nm, ...) {
  d <- data.frame(
    x1 = c(1, 0, NA, 1, 0, 1, 1),
    tok = c(0, 0, NA, 1, 0, 1, 1),
    g = c("a", "a", "a", "b", "b", "b", "b")
  )
  names(d)[[2L]] <- nm
  table_multiresponse(d, select = c("x1", nm), base = "all", ...)
}

test_that("an item column named like a generated row's token keeps both rows", {
  # The item and the generated row shared the token, so the label of the
  # generated row led back to it and returned the item's count. The
  # column now owns the token; the generated row keeps its label.
  generated <- c(none = "2", no_data = "1", nobs = "7")
  for (nm in names(generated)) {
    tbl <- .il_token_item(nm)
    expect_identical(inline(tbl, !!nm), "3")
    for (other in setdiff(names(generated), nm)) {
      expect_identical(inline(tbl, !!other), generated[[other]])
    }
    expect_identical(inline(tbl, "(No item chosen)"), "2")
    expect_identical(inline(tbl, "(No data for the question)"), "1")
    expect_identical(inline(tbl, "Respondents"), "7")
  }
  # The token names the item, which has no level.
  expect_error(
    inline(.il_token_item("no_data"), "no_data", "(Missing)"),
    "No level",
    class = "spicy_invalid_input"
  )
  by_g <- .il_token_item("none", by = g)
  expect_identical(inline(by_g, none, model = "Total"), "3")
  expect_identical(inline(by_g, "(No item chosen)", model = "Total"), "2")
  expect_identical(inline(by_g, "(No item chosen)", model = "a"), "1")
})

test_that("an item labeled like a generated row's token is told apart", {
  # The label of x1 and the token of "(No item chosen)" are both exact
  # matches of "none"; the token used to win.
  tbl <- .il_token_item("x2", labels = c(x1 = "none"))
  msg <- conditionMessage(expect_error(
    inline(tbl, "none"),
    class = "spicy_invalid_input"
  ))
  expect_match(msg, "\"x1\": the variable shown as \"none\".", fixed = TRUE)
  expect_match(
    msg,
    "\"(No item chosen)\": the row shown as \"(No item chosen)\".",
    fixed = TRUE
  )
  expect_identical(inline(tbl, x1), "4")
  expect_identical(inline(tbl, "(No item chosen)"), "2")
})

test_that("an exact label wins over a fit statistic's label read regardless of case", {
  d <- data.frame(
    x1 = c(1, 0, NA, 1, 0, 1, 1),
    x2 = c(0, 0, NA, 1, 0, 1, 1)
  )
  tbl <- table_multiresponse(
    d,
    c(x1, x2),
    base = "all",
    labels = c(x1 = "respondents")
  )
  expect_identical(inline(tbl, "respondents"), "4")
  expect_identical(inline(tbl, "Respondents"), "7")
  # Both labels fold to the request: refused, not settled for the base.
  expect_error(inline(tbl, "RESPONDENTS"), class = "spicy_invalid_input")
  plain <- table_multiresponse(d, c(x1, x2), base = "all")
  expect_identical(inline(plain, "RESPONDENTS"), "7")
  # The same order on a released family: a term labeled "N" (nitrogen)
  # was answered with the number of observations, "n".
  tr <- table_regression(lm(mpg ~ wt + hp, data = mtcars), labels = c(wt = "N"))
  expect_identical(inline(tr, "N"), "-3.88")
  expect_identical(inline(tr, "n"), "32")
  expect_identical(inline(tr, "nobs"), "32")
  # A column wins over the token or label of a fit statistic.
  tc <- table_regression(lm(
    mpg ~ wt + nobs,
    data = transform(mtcars, nobs = qsec)
  ))
  expect_identical(inline(tc, nobs), "0.93")
  expect_identical(inline(tc, "n"), "32")
})

test_that("a variable that matches two rows is refused with an address for each", {
  tbl <- table_regression(
    lm(mpg ~ wt + hp, data = mtcars),
    show_fit_stats = c("nobs", "aic"),
    labels = c(wt = "AIC", hp = "n")
  )
  msg <- conditionMessage(expect_error(
    inline(tbl, "AIC"),
    class = "spicy_invalid_input"
  ))
  expect_match(
    msg,
    "\"AIC\" matches 2 rows of this table: address the one you mean.",
    fixed = TRUE
  )
  expect_match(msg, "\"wt\": the variable shown as \"AIC\".", fixed = TRUE)
  expect_match(
    msg,
    "\"aic\": the fit statistic shown as \"AIC\".",
    fixed = TRUE
  )
  msg <- conditionMessage(expect_error(
    inline(tbl, "n"),
    class = "spicy_invalid_input"
  ))
  expect_match(msg, "\"hp\": the variable shown as \"n\".", fixed = TRUE)
  expect_match(msg, "\"nobs\": the fit statistic shown as \"n\".", fixed = TRUE)
  # The addresses it names reach one row each.
  expect_identical(inline(tbl, "wt"), "-3.88")
  expect_identical(inline(tbl, "aic"), "156.7")
  expect_identical(inline(tbl, "hp"), "-0.03")
  expect_identical(inline(tbl, "nobs"), "32")
  # A fit statistic whose token is a column name and whose label is
  # another term's has no address of its own, and the error says so.
  shadowed <- table_regression(
    lm(mpg ~ wt + aic, data = transform(mtcars, aic = qsec)),
    show_fit_stats = c("nobs", "aic"),
    labels = c(wt = "AIC")
  )
  expect_error(
    inline(shadowed, "AIC"),
    "No address of its own for the fit statistic shown as \"AIC\"",
    class = "spicy_invalid_input",
    fixed = TRUE
  )
  # Two variables showing one label: the descriptive families refuse
  # alike. A generated row whose label a variable would show is refused
  # by the table itself; relabeled, each row keeps an address.
  two <- table_continuous(
    mtcars,
    c(mpg, hp),
    labels = c(mpg = "Same", hp = "Same")
  )
  msg <- conditionMessage(expect_error(
    inline(two, "Same"),
    class = "spicy_invalid_input"
  ))
  expect_match(msg, "\"mpg\": the variable shown as \"Same\".", fixed = TRUE)
  expect_match(msg, "\"hp\": the variable shown as \"Same\".", fixed = TRUE)
  d <- data.frame(y = c(1, 2, 3, 4, 5, 6), g = c("a", "a", "b", "b", "a", "b"))
  withr::local_options(spicy.labels = list(row_overall = "Whole sample"))
  to <- .il_quiet(table_outcome(d, y, select = g, labels = c(g = "Overall")))
  expect_identical(inline(to, "Whole sample"), "3.50")
  expect_identical(inline(to, "Overall", "a"), "2.67")
  expect_identical(inline(to, y), "3.50")
  expect_identical(inline(to, g, "a"), "2.67")
})

test_that("a level's label leads to its variable only when one variable has it", {
  # The fallback reads the indented rows after the variables' own labels:
  # a variable labeled "u" wins over a level "u" of another variable.
  one <- .il_quiet(table_categorical(sochealth, select = smoking))
  expect_identical(inline(one, "Yes", "Yes"), "249")
  dc <- data.frame(
    a = factor(c("u", "v", "u", "v", "u")),
    b = factor(c("v", "v", "u", "u", "u"))
  )
  tu <- .il_quiet(table_categorical(dc, select = c(a, b), labels = c(a = "u")))
  expect_identical(inline(tu, "u", "v"), inline(tu, a, "v"))
  # Two variables with a level "Yes": the first one was cited.
  two <- .il_quiet(table_categorical(
    sochealth,
    select = c(smoking, physical_activity)
  ))
  msg <- conditionMessage(expect_error(
    inline(two, "Yes", "Yes"),
    class = "spicy_invalid_input"
  ))
  expect_match(
    msg,
    "\"physical_activity\": the variable shown as \"physical_activity\".",
    fixed = TRUE
  )
})

test_that("the p of a by-group continuous table is cited without `level`", {
  # The p-value sits on the first group's row only: that row alone carries
  # it, so it is the address, as `?inline` says ("leave `level` out").
  x <- table_continuous(sochealth, select = bmi, by = sex)
  expect_identical(
    inline(x, bmi, column = "p"),
    inline(x, bmi, "Female", column = "p")
  )
  # A statistic every group carries still needs its group.
  expect_error(inline(x, bmi, column = "m"), class = "spicy_invalid_input")
  expect_false(identical(
    inline(x, bmi, "Male", column = "m"),
    inline(x, bmi, "Female", column = "m")
  ))
  skip_if_not_installed("survey")
  des <- survey::svydesign(ids = ~1, weights = ~weight, data = sochealth)
  xs <- table_continuous_svy(des, select = bmi, by = sex)
  expect_identical(
    inline(xs, bmi, column = "p"),
    inline(xs, bmi, "Female", column = "p")
  )
})
