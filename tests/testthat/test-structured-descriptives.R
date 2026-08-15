# The structured (typed) view of the DESCRIPTIVE families.
#
# One contract, four families: what `as_structured()` returns for
# `table_categorical()` / `table_continuous()` /
# `table_continuous_lm()` is the same v3 object it returns for
# `table_regression()`. Three properties are pinned here:
#
#   1. IDENTITY -- every row says what it is (`.variable`, `.level`,
#      `.row_role`, `.indent`), read off the compute frames, never
#      parsed back from a display string.
#   2. FIDELITY -- rendering the typed body through the shared
#      string formatter reproduces the console's `display_df` cell
#      for cell. This is what makes the typed numbers safe to use:
#      they ARE the printed numbers, at the precision `col_meta`
#      declares.
#   3. GUARDS -- the version guard and the row-role vocabulary apply
#      to descriptive objects exactly as they do to regression ones.

quiet <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(expr)))
  res
}

# The frame the print method renders, rebuilt from the object's own
# attributes -- the reference the typed body is checked against.
cat_display <- function(x) attr(x, "display_df")

con_display <- function(x) {
  build_display_df(
    x,
    digits = attr(x, "digits"),
    effect_size_digits = attr(x, "effect_size_digits"),
    p_digits = attr(x, "p_digits"),
    decimal_mark = attr(x, "decimal_mark"),
    ci_level = attr(x, "ci_level"),
    show_p = isTRUE(attr(x, "show_p")),
    show_statistic = isTRUE(attr(x, "show_statistic")),
    show_n = attr(x, "show_n"),
    show_ci = attr(x, "show_ci"),
    show_effect_size = isTRUE(attr(x, "show_effect_size")),
    show_effect_size_ci = isTRUE(attr(x, "show_effect_size_ci")),
    tokens_union = attr(x, "show_columns"),
    tokens_by_var = attr(x, "show_columns_by_var")
  )
}

lm_display <- function(x) {
  build_wide_display_df_continuous_lm(
    x,
    digits = attr(x, "digits"),
    fit_digits = attr(x, "fit_digits"),
    effect_size_digits = attr(x, "effect_size_digits"),
    p_digits = attr(x, "p_digits"),
    decimal_mark = attr(x, "decimal_mark"),
    ci_level = attr(x, "ci_level"),
    show_statistic = attr(x, "show_statistic"),
    show_p_value = attr(x, "show_p_value"),
    show_n = attr(x, "show_n"),
    show_weighted_n = attr(x, "show_weighted_n"),
    effect_size = attr(x, "effect_size"),
    effect_size_ci = attr(x, "show_effect_size_ci"),
    r2_type = attr(x, "r2_type"),
    ci = attr(x, "show_ci")
  )
}

# The typed body, rendered through the SHARED string formatter every
# engine uses, must equal the console frame column for column. Label
# columns the typed body does not carry (`Group`, which names the row
# and travels in `.level`) are excluded from the comparison.
#
# Column matching goes through `col_meta$display_label` (decision 13):
# the structured column NAME is the frozen programmatic key, the
# label is what the console prints. The two are identical strings
# today, so this comparison is byte-for-byte the old one -- but the
# helper is ready for the day a label is translated while the key
# stays put.
expect_faithful <- function(struct, display, skip_cols = character(0)) {
  formatted <- spicy:::.format_structured_to_string_body(struct)
  shown <- vapply(
    names(formatted),
    function(nm) {
      lbl <- struct$col_meta[[nm]]$display_label
      if (is.null(lbl)) nm else lbl
    },
    character(1)
  )
  expect_identical(
    unname(shown),
    setdiff(names(display), skip_cols)
  )
  for (i in seq_along(formatted)) {
    expect_identical(
      formatted[[i]],
      display[[shown[[i]]]],
      info = names(formatted)[[i]]
    )
  }
}

# Every descriptive view answers the same schema questions.
expect_v3_shape <- function(struct) {
  expect_identical(struct$version, 3L)
  expect_null(struct$stars)
  expect_true(all(
    spicy:::.STRUCT_META_COLS %in% names(struct$body)
  ))
  expect_true(is.character(struct$body$Variable))
  expect_true(is.integer(struct$body$.indent))
  expect_true(all(struct$body$.row_role %in% spicy:::.STRUCT_ROW_ROLES))
  for (nm in spicy:::.struct_value_cols(struct$body)) {
    expect_true(is.numeric(struct$body[[nm]]), info = nm)
    expect_false(is.null(struct$col_meta[[nm]]), info = nm)
  }
  expect_silent(spicy:::.validate_structured(struct))
}

sh_desc <- function() {
  data(sochealth, package = "spicy", envir = environment())
  sochealth
}


# ---- table_categorical() --------------------------------------------------

test_that("as_structured() types a one-way categorical table", {
  sh <- sh_desc()
  tbl <- quiet(table_categorical(sh, c(sex, smoking)))
  s <- as_structured(tbl)

  expect_v3_shape(s)
  expect_identical(spicy:::.struct_value_cols(s$body), c("n", "%"))
  expect_identical(s$col_meta$n$token, "n")
  expect_identical(s$col_meta$n$precision, 0L)
  expect_identical(s$col_meta[["%"]]$token, "pct")
  expect_identical(s$col_meta[["%"]]$precision, 1L)
  expect_null(s$spanners)
  expect_identical(s$ci_pairs, list())

  # Identity: a header row per variable, then one row per level; the
  # source variable is the COLUMN NAME, not the display label.
  expect_identical(
    s$body$.row_role,
    c(
      "factor_header",
      "level",
      "level",
      "factor_header",
      "level",
      "level",
      "missing"
    )
  )
  expect_identical(unique(s$body$.variable), c("sex", "smoking"))
  expect_identical(s$body$.level[1L], NA_character_)
  expect_identical(s$body$.indent, c(0L, 1L, 1L, 0L, 1L, 1L, 1L))

  # The raw numbers, unrounded, addressable by (variable, level).
  # `which()` because `.level` is NA on the header rows -- the same
  # idiom `?as_structured` documents for the regression body.
  female <- s$body[
    which(s$body$.variable == "sex" & s$body$.level == "Female"),
  ]
  expect_identical(female$n, 620)
  expect_equal(female[["%"]], 100 * 620 / 1200)

  expect_faithful(s, cat_display(tbl))
})

test_that("the (Missing) category is identified by role, not by label", {
  sh <- sh_desc()
  # A real category literally named "(Missing)" pushes the display
  # label of the missing one to "(Missing_1)". The ROLE must follow
  # the key, not the string -- the property an i18n pass depends on.
  sh$smoking <- as.character(sh$smoking)
  sh$smoking[sh$smoking == "Yes"] <- "(Missing)"
  tbl <- quiet(table_categorical(sh, smoking, drop_na = FALSE))
  s <- as_structured(tbl)

  expect_true("(Missing_1)" %in% s$body$.level)
  expect_identical(
    s$body$.row_role[which(s$body$.level == "(Missing_1)")],
    "missing"
  )
  # The decoy keeps the ordinary level role.
  expect_identical(
    s$body$.row_role[which(s$body$.level == "(Missing)")],
    "level"
  )
  expect_faithful(s, cat_display(tbl))
})

test_that("as_structured() types a categorical table by a group", {
  sh <- sh_desc()
  tbl <- quiet(table_categorical(sh, c(sex, smoking), by = education))
  s <- as_structured(tbl)

  expect_v3_shape(s)
  expect_identical(
    spicy:::.struct_value_cols(s$body),
    c(
      "Lower secondary n",
      "Lower secondary %",
      "Upper secondary n",
      "Upper secondary %",
      "Tertiary n",
      "Tertiary %",
      "Total n",
      "Total %",
      "p",
      "Cramer's V"
    )
  )
  # One spanner per group, on the (n, %) pair -- body column indices,
  # `Variable` counted.
  expect_identical(
    s$spanners,
    list(
      `Lower secondary` = c(2L, 3L),
      `Upper secondary` = c(4L, 5L),
      Tertiary = c(6L, 7L),
      Total = c(8L, 9L)
    )
  )
  # The margin is flagged, never matched on its label.
  expect_true(isTRUE(s$col_meta[["Total n"]]$total))
  expect_null(s$col_meta[["Tertiary n"]]$total)
  expect_identical(s$col_meta[["Tertiary n"]]$group, "Tertiary")

  # The test and the measure sit on the variable HEADER row, as they
  # print; the level rows carry the counts.
  hdr <- which(s$body$.row_role == "factor_header")
  expect_false(anyNA(s$body$p[hdr]))
  expect_true(all(is.na(s$body$p[-hdr])))
  expect_identical(s$col_meta$p$p_style, "apa")
  expect_equal(s$col_meta$p$threshold, 1e-3)
  expect_identical(s$col_meta[["Cramer's V"]]$token, "assoc")
  expect_equal(s$col_meta[["Cramer's V"]]$value_range, c(-1, 1))

  expect_faithful(s, cat_display(tbl))
})

test_that("a signed association measure is not read as a bad p-value", {
  sh <- sh_desc()
  tbl <- quiet(table_categorical(
    sh,
    self_rated_health,
    by = education,
    assoc_measure = "tau_b",
    assoc_ci = TRUE
  ))
  s <- as_structured(tbl)

  # The APA leading-zero strip applies (it is a bounded measure), but
  # the validated range is [-1, 1]: a negative tau is data, not a
  # broken p-value.
  expect_identical(s$col_meta[["Kendall's Tau-b"]]$p_style, "apa")
  expect_silent(spicy:::.validate_structured(s))
  s_bad <- s
  s_bad$body[["Kendall's Tau-b"]][1L] <- -1.5
  expect_warning(
    spicy:::.validate_structured(s_bad),
    class = "spicy_internal_invariant"
  )

  expect_identical(
    s$ci_pairs,
    list(list(
      label = "95% CI",
      cols = spicy:::.desc_col_index(
        spicy:::.struct_value_cols(s$body),
        c("CI lower", "CI upper")
      )
    ))
  )
  expect_faithful(s, cat_display(tbl))
})

test_that("weights, no margin and levels_keep keep the typed view faithful", {
  sh <- sh_desc()
  set.seed(11)
  sh$w <- stats::runif(nrow(sh), 0.5, 2)

  weighted <- quiet(table_categorical(sh, sex, by = education, weights = w))
  sw <- as_structured(weighted)
  expect_v3_shape(sw)
  # Weighted counts are fractional in the typed body and integer on
  # screen (the SPSS Crosstabs convention `precision = 0` states).
  expect_false(all(sw$body[["Total n"]] %% 1 == 0, na.rm = TRUE))
  expect_identical(sw$col_meta[["Total n"]]$precision, 0L)
  expect_faithful(sw, cat_display(weighted))

  no_total <- quiet(table_categorical(
    sh,
    sex,
    by = education,
    include_total = FALSE
  ))
  snt <- as_structured(no_total)
  expect_false(any(vapply(
    snt$col_meta,
    function(m) isTRUE(m$total),
    logical(1)
  )))
  expect_faithful(snt, cat_display(no_total))

  kept <- quiet(table_categorical(sh, c(sex, smoking), levels_keep = "Female"))
  sk <- as_structured(kept)
  # `smoking` matches nothing and is dropped from the table; the typed
  # view drops the same block.
  expect_identical(unique(sk$body$.variable), "sex")
  expect_identical(sk$body$.level, c(NA_character_, "Female"))
  expect_faithful(sk, cat_display(kept))
})

test_that("the typed roles reproduce the console's block geometry", {
  sh <- sh_desc()
  tbl <- quiet(table_categorical(sh, c(sex, smoking), by = education))
  s <- as_structured(tbl)

  # The legacy string derivation reads the label back
  # (`.categorical_var_sep_rows()` tests it for the indent prefix);
  # the typed body says it outright, and since the row_role migration
  # every route reads the typed answer. On sane labels the two agree
  # -- this pin keeps the retired derivation as an independent oracle.
  expect_identical(
    spicy:::.categorical_var_sep_rows(attr(tbl, "display_df")$Variable, "  "),
    which(s$body$.row_role == "factor_header")[-1L]
  )

  # Same table, exotic indent: the labels move, the identity does not.
  wide <- quiet(table_categorical(
    sh,
    c(sex, smoking),
    by = education,
    indent_text = ">> "
  ))
  sw <- as_structured(wide)
  expect_identical(sw$body$.row_role, s$body$.row_role)
  expect_identical(sw$body$.level, s$body$.level)
  expect_identical(sw$body$.indent, s$body$.indent)
  expect_faithful(sw, cat_display(wide))
})

test_that("a label that starts with the indent string keeps its geometry", {
  skip_if_not_installed("tinytable")
  sh <- sh_desc()
  # A variable label crafted to LOOK like an indented level row. The
  # retired string derivation misread the header as a level -- no rule
  # above the block, label re-indented as a level name -- because it
  # keyed on the indent prefix. The typed roles cannot be fooled.
  tricky <- "  Sex (label starts with the indent)"
  tbl <- quiet(table_categorical(
    sh,
    c(sex, smoking),
    by = education,
    labels = c(sex = tricky)
  ))
  s <- as_structured(tbl)
  expect_identical(s$body$.row_role[1L], "factor_header")

  tt <- quiet(table_categorical(
    sh,
    c(sex, smoking),
    by = education,
    labels = c(sex = tricky),
    output = "tinytable"
  ))
  # The engine keeps the header label verbatim: it is not re-indented
  # into a level and its block still opens where the roles say.
  expect_identical(tt@data[[1L]][1L], tricky)
})

test_that("an empty categorical table yields an empty typed body", {
  df <- data.frame(
    a = factor(c("x", "y", "x")),
    b = factor(c("p", "q", "p"))
  )
  tbl <- quiet(table_categorical(df, c(a, b), levels_keep = "zzz"))
  s <- as_structured(tbl)

  expect_identical(nrow(s$body), 0L)
  expect_identical(spicy:::.struct_value_cols(s$body), c("n", "%"))
  expect_true(is.character(s$body$Variable))
  expect_true(is.integer(s$body$.indent))
  expect_identical(s$cell_status, list())
  expect_silent(spicy:::.validate_structured(s))
})


# ---- table_continuous() ---------------------------------------------------

test_that("as_structured() types a one-way continuous table", {
  sh <- sh_desc()
  tbl <- quiet(table_continuous(sh, c(age, bmi)))
  s <- as_structured(tbl)

  expect_v3_shape(s)
  expect_identical(
    spicy:::.struct_value_cols(s$body),
    c("M", "SD", "Min", "Max", "95% CI LL", "95% CI UL", "n")
  )
  # One `summary` row per variable: no sub-key, so `.level` is NA.
  expect_identical(s$body$.row_role, c("summary", "summary"))
  expect_identical(s$body$.variable, c("age", "bmi"))
  expect_true(all(is.na(s$body$.level)))
  expect_identical(s$body$.indent, c(0L, 0L))

  # Typed values are the raw statistics, not the rounded display.
  expect_equal(s$body$M[1L], mean(sh$age, na.rm = TRUE))
  expect_identical(s$col_meta$M$token, "m")
  expect_identical(s$col_meta$n$precision, 0L)
  expect_identical(s$col_meta[["95% CI LL"]]$ci_role, "LL")
  expect_identical(s$col_meta[["95% CI LL"]]$ci_pair, "95% CI UL")
  expect_identical(s$ci_pairs[[1L]]$label, "95% CI")

  expect_faithful(s, con_display(tbl))
})

test_that("as_structured() types a continuous table by a group", {
  sh <- sh_desc()
  tbl <- quiet(table_continuous(
    sh,
    c(age, bmi),
    by = sex,
    statistic = TRUE,
    effect_size = "auto",
    effect_size_ci = TRUE
  ))
  s <- as_structured(tbl)

  expect_v3_shape(s)
  # The `by` level names the row and travels in `.level`; the console's
  # second label column is not a cell of the table.
  expect_identical(s$body$.row_role, rep("group", 4L))
  expect_identical(s$body$.level, c("Female", "Male", "Female", "Male"))
  expect_identical(s$body$.variable, c("age", "age", "bmi", "bmi"))

  # Composite cells: the body keeps the statistic, the override the
  # string the console prints.
  expect_identical(s$col_meta$Test$token, "statistic")
  expect_true(grepl("=", s$col_meta$Test$display_cells[1L], fixed = TRUE))
  expect_true(is.na(s$col_meta$Test$display_cells[2L]))
  expect_true(grepl("[", s$col_meta$ES$display_cells[1L], fixed = TRUE))
  expect_identical(s$col_meta$p$p_style, "apa")

  expect_faithful(s, con_display(tbl), skip_cols = "Group")
})

test_that("the missing-by group is identified by role", {
  sh <- sh_desc()
  tbl <- quiet(table_continuous(sh, age, by = smoking, drop_na = FALSE))
  s <- as_structured(tbl)

  expect_identical(s$body$.row_role, c("group", "group", "missing"))
  expect_identical(s$body$.level[3L], "(Missing)")
  expect_faithful(s, con_display(tbl), skip_cols = "Group")
})

test_that("show_columns tokens drive the typed columns", {
  sh <- sh_desc()
  tbl <- quiet(table_continuous(
    sh,
    c(age, bmi),
    show_columns = c("med", "iqr", "med_iqr", "q1", "q3", "med_ci", "n")
  ))
  s <- as_structured(tbl)

  expect_v3_shape(s)
  expect_identical(
    vapply(s$col_meta, `[[`, character(1), "token"),
    c(
      Med = "med",
      IQR = "iqr",
      `Med [Q1, Q3]` = "med_iqr",
      Q1 = "q1",
      Q3 = "q3",
      `Med 95% CI LL` = "med_ci",
      `Med 95% CI UL` = "med_ci",
      n = "n"
    )
  )
  # The compact median cell is composite: the body keeps the median,
  # the override the "Med [Q1, Q3]" string.
  expect_equal(
    s$body[["Med [Q1, Q3]"]],
    s$body[["Med"]]
  )
  expect_false(anyNA(s$col_meta[["Med [Q1, Q3]"]]$display_cells))
  expect_identical(s$ci_pairs[[1L]]$label, "Med 95% CI")

  expect_faithful(s, con_display(tbl))
})

test_that("a statistic another variable displays is absent, not undefined", {
  sh <- sh_desc()
  tbl <- quiet(table_continuous(
    sh,
    c(age, bmi),
    show_columns = list(age = c("m", "sd"), bmi = c("med", "iqr"))
  ))
  s <- as_structured(tbl)

  # `age` has no median row: the cell is NA with NO status (absent,
  # displayed blank), never "undefined".
  expect_true(is.na(s$body$Med[1L]))
  expect_identical(spicy:::.struct_cell_status(s, "Med")[1L], "")
  expect_false(is.na(s$body$Med[2L]))
  expect_faithful(s, con_display(tbl))
})

test_that("a statistic that applies but has no value is `undefined`", {
  df <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5))
  tbl <- quiet(table_continuous(df, v, by = g))
  s <- as_structured(tbl)

  # The one-observation group has no SD and no interval: the console
  # prints "--", the contract says `undefined`, and the override keeps
  # the console's own glyph.
  expect_identical(spicy:::.struct_cell_status(s, "SD"), c("", "undefined"))
  expect_identical(s$col_meta$SD$display_cells, c(NA, "--"))
  expect_identical(
    spicy:::.struct_cell_status(s, "95% CI LL"),
    c("", "undefined")
  )
  expect_faithful(s, con_display(tbl), skip_cols = "Group")
})


# ---- table_continuous_lm() ------------------------------------------------

test_that("as_structured() types a two-level bivariate model table", {
  sh <- sh_desc()
  tbl <- quiet(table_continuous_lm(
    sh,
    c(age, bmi),
    by = sex,
    statistic = TRUE,
    effect_size = "g",
    effect_size_ci = TRUE
  ))
  s <- as_structured(tbl)

  expect_v3_shape(s)
  expect_identical(s$body$.row_role, c("summary", "summary"))
  expect_identical(s$body$.variable, c("age", "bmi"))
  expect_true(all(is.na(s$body$.level)))

  # The `by` levels are COLUMNS here; each marginal-mean column says
  # which level it reports, so nothing has to parse "M (Male)".
  expect_identical(s$col_meta[["M (Female)"]]$token, "emmean")
  expect_identical(s$col_meta[["M (Female)"]]$level, "Female")
  expect_identical(s$col_meta[["M (Male)"]]$level, "Male")
  expect_identical(s$col_meta[["Δ (Male - Female)"]]$token, "delta")
  expect_identical(s$col_meta$t$token, "statistic")
  expect_identical(s$col_meta[["R²"]]$token, "r2")
  expect_identical(s$col_meta$n$precision, 0L)
  expect_identical(
    s$ci_pairs[[1L]]$cols,
    spicy:::.desc_col_index(
      spicy:::.struct_value_cols(s$body),
      c("95% CI LL", "95% CI UL")
    )
  )
  # The inlined effect-size interval is a composite cell.
  expect_identical(s$col_meta$g$token, "es")
  expect_true(grepl("[", s$col_meta$g$display_cells[1L], fixed = TRUE))

  expect_faithful(s, lm_display(tbl))
})

test_that("a numeric predictor types a slope column", {
  sh <- sh_desc()
  tbl <- quiet(table_continuous_lm(sh, c(bmi, wellbeing_score), by = age))
  s <- as_structured(tbl)

  expect_v3_shape(s)
  expect_identical(s$col_meta$B$token, "b")
  expect_identical(s$body$.variable, c("bmi", "wellbeing_score"))
  expect_faithful(s, lm_display(tbl))
})

test_that("a three-level predictor has marginal means and no contrast", {
  sh <- sh_desc()
  tbl <- quiet(table_continuous_lm(sh, c(age, bmi), by = education))
  s <- as_structured(tbl)

  expect_v3_shape(s)
  tokens <- vapply(s$col_meta, `[[`, character(1), "token")
  expect_identical(sum(tokens == "emmean"), 3L)
  expect_false("delta" %in% tokens)
  expect_false("ci" %in% tokens)
  expect_faithful(s, lm_display(tbl))
})

test_that("weighted n, no R2 and no effect size stay faithful", {
  sh <- sh_desc()
  set.seed(12)
  sh$w <- stats::runif(nrow(sh), 0.5, 2)
  tbl <- quiet(table_continuous_lm(
    sh,
    bmi,
    by = sex,
    weights = w,
    show_weighted_n = TRUE,
    r2 = "none",
    effect_size = "none"
  ))
  s <- as_structured(tbl)

  expect_v3_shape(s)
  tokens <- vapply(s$col_meta, `[[`, character(1), "token")
  expect_true("weighted_n" %in% tokens)
  expect_false("r2" %in% tokens)
  expect_false("es" %in% tokens)
  # A sum of weights is not a count: it keeps the table's precision.
  expect_identical(s$col_meta[["Weighted n"]]$precision, 2L)
  expect_faithful(s, lm_display(tbl))
})


# ---- the contract applies to descriptive objects too ----------------------

test_that("as_structured() names the four supported families", {
  expect_error(as_structured(mtcars), class = "spicy_invalid_input")
  expect_error(as_structured(mtcars), "spicy_categorical_table")
  expect_error(as_structured(mtcars), "spicy_continuous_lm_table")
})

test_that("a descriptive table with no view is refused by its own builder", {
  sh <- sh_desc()
  tbl <- quiet(table_continuous(sh, age))
  attr(tbl, "structured") <- NULL
  expect_error(as_structured(tbl), "table_continuous\\(\\)")
  expect_error(as_structured(tbl), class = "spicy_invalid_input")
})

test_that("the version guard applies to descriptive views", {
  sh <- sh_desc()
  tbl <- quiet(table_categorical(sh, sex))

  older <- tbl
  s_old <- attr(older, "structured")
  s_old$version <- 2L
  attr(older, "structured") <- s_old
  expect_error(as_structured(older), class = "spicy_structured_version")
  expect_error(as_structured(older), "table_categorical\\(\\)")

  newer <- tbl
  s_new <- attr(newer, "structured")
  s_new$version <- 99L
  attr(newer, "structured") <- s_new
  expect_error(as_structured(newer), class = "spicy_invalid_input")
})

test_that("an unknown row role is refused on a descriptive body", {
  sh <- sh_desc()
  s <- as_structured(quiet(table_continuous(sh, age)))
  s$body$.row_role[1L] <- "whatever"
  expect_warning(
    spicy:::.validate_structured(s),
    class = "spicy_internal_invariant"
  )

  # And the three descriptive roles ARE part of the vocabulary.
  expect_true(all(
    c("summary", "group", "missing") %in% spicy:::.STRUCT_ROW_ROLES
  ))
})

test_that("the descriptive views declare a usable format spec", {
  sh <- sh_desc()
  views <- list(
    categorical = as_structured(quiet(table_categorical(
      sh,
      sex,
      by = education
    ))),
    continuous = as_structured(quiet(table_continuous(sh, age, by = sex))),
    lm = as_structured(quiet(table_continuous_lm(sh, age, by = sex)))
  )
  for (nm in names(views)) {
    fs <- views[[nm]]$format_spec
    expect_identical(fs$decimal_mark, ".", info = nm)
    expect_identical(fs$p_style, "apa", info = nm)
    expect_true(is.numeric(fs$digits), info = nm)
    expect_identical(fs$ci_level, 0.95, info = nm)
    expect_identical(views[[nm]]$outcome_labels_by_col, character(0), info = nm)
  }
})

test_that("the decimal mark of the typed view follows the table", {
  sh <- sh_desc()
  tbl <- quiet(table_continuous(sh, age, by = sex, decimal_mark = ","))
  s <- as_structured(tbl)
  expect_identical(s$format_spec$decimal_mark, ",")
  expect_faithful(s, con_display(tbl), skip_cols = "Group")

  cat_tbl <- quiet(table_categorical(
    sh,
    sex,
    by = education,
    decimal_mark = ","
  ))
  sc <- as_structured(cat_tbl)
  expect_identical(sc$format_spec$decimal_mark, ",")
  expect_faithful(sc, cat_display(cat_tbl))
})


test_that("descriptive col_meta carries display_label, defaulting to the key", {
  # Decision 13 / i18n stage 1.5 lot 0: the column NAME is the frozen
  # programmatic key; display_label is what engines will render. The
  # three descriptive families all funnel through .desc_assemble(),
  # which fills the identity default.
  sh <- sh_desc()
  s_cat <- as_structured(quiet(table_categorical(sh, sex, by = education)))
  s_con <- as_structured(quiet(table_continuous(sh, select = bmi)))
  s_lm <- as_structured(quiet(table_continuous_lm(sh, select = bmi, by = sex)))
  for (s in list(s_cat, s_con, s_lm)) {
    labels <- vapply(
      names(s$col_meta),
      function(nm) s$col_meta[[nm]]$display_label,
      character(1)
    )
    expect_identical(unname(labels), names(s$col_meta))
  }
})
