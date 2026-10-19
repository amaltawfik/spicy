collect_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

test_that("table_categorical returns expected long raw structure", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non"),
    v2 = c("Oui", "Oui", "Non", "Non", "Oui", "Non")
  )

  out <- table_categorical(
    data = df,
    select = c(v1, v2),
    by = grp,
    labels = c(v1 = "Var 1", v2 = "Var 2"),
    include_total = TRUE,
    simulate_p = FALSE,
    output = "long"
  )

  expect_s3_class(out, "data.frame")
  # The long measure column has a stable name; the measure it holds is
  # a key in `effect_size_type`. 2x2 + 2x2 -> auto rule picks phi (was
  # cramer_v before 0.11.0; see NEWS).
  expect_true(all(
    c(
      "variable",
      "level",
      "group",
      "n",
      "pct",
      "p",
      "effect_size",
      "effect_size_type"
    ) %in%
      names(out)
  ))
  expect_identical(unique(out$effect_size_type), "phi")
  expect_true(nrow(out) > 0)
})

test_that("table_categorical accepts weights as column name or numeric vector", {
  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non"),
    w = c(1, 2, 1, 3, 2, 1)
  )

  out_col <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = c(v1 = "Var 1"),
    weights = "w",
    simulate_p = FALSE,
    output = "long"
  )

  out_vec <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = c(v1 = "Var 1"),
    weights = df$w,
    simulate_p = FALSE,
    output = "long"
  )

  expect_equal(out_col$n, out_vec$n)
  expect_equal(out_col$pct, out_vec$pct)
})

test_that("table_categorical accepts weights as an unquoted column name", {
  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non"),
    w = c(1, 2, 1, 3, 2, 1)
  )

  expect_no_warning(
    out_bare <- table_categorical(
      data = df,
      select = "v1",
      by = "grp",
      labels = c(v1 = "Var 1"),
      weights = w,
      simulate_p = FALSE,
      output = "long"
    )
  )

  out_char <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = c(v1 = "Var 1"),
    weights = "w",
    simulate_p = FALSE,
    output = "long"
  )

  expect_equal(out_bare$n, out_char$n)
  expect_equal(out_bare$pct, out_char$pct)
})

test_that("table_categorical accepts tidyselect-style select and unquoted by", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    v1 = c("Oui", "Non", "Oui", "Non"),
    v2 = c("Oui", "Oui", "Non", "Non")
  )

  out <- table_categorical(
    data = df,
    select = tidyselect::starts_with("v"),
    by = grp,
    output = "data.frame"
  )

  expect_true(all(c("v1", "v2") %in% out$Variable))
})

test_that("table_categorical accepts by as a character object", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    v1 = c("Oui", "Non", "Oui", "Non")
  )

  by_col <- "grp"
  expect_no_warning(
    out <- table_categorical(
      data = df,
      select = "v1",
      by = by_col,
      output = "data.frame"
    )
  )

  expect_true("A n" %in% names(out))
  expect_true("B %" %in% names(out))
})

test_that("table_categorical validates by and select branches", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    v1 = c("Oui", "Non", "Oui", "Non"),
    v2 = c("x", "y", "x", "y")
  )

  expect_error(
    table_categorical(df, select = "v1", by = c(grp, v2)),
    "by"
  )

  expect_error(
    table_categorical(df, select = tidyselect::starts_with("zzz"), by = grp),
    "select"
  )
})

test_that("table_categorical works without by in long raw output", {
  df <- data.frame(
    v1 = c("Oui", "Non", "Oui", NA),
    v2 = c("A", "A", "B", "B")
  )

  out <- table_categorical(
    data = df,
    select = c(v1, v2),
    drop_na = FALSE,
    output = "long"
  )

  expect_true(all(c("variable", "level", "n", "pct") %in% names(out)))
  expect_false("group" %in% names(out))
  expect_true(any(grepl("Missing", out$level)))
})

test_that("table_categorical renames generated missing labels when needed", {
  df <- data.frame(v1 = c("(Missing)", NA, "Yes"))

  out <- table_categorical(
    data = df,
    select = v1,
    drop_na = FALSE,
    output = "long"
  )

  expect_true("(Missing)" %in% out$level)
  expect_true("(Missing_1)" %in% out$level)
})

test_that("de-duplication leaves each Missing row with its own count", {
  # The names were pinned; the VALUES were not. `.add_missing_level()`
  # appends the RESOLVED label, so the literal level keeps "(Missing)"
  # and the real NAs take "(Missing_1)" -- and that direction is what a
  # values witness has to fix. The three counts are deliberately
  # different, so a regression that swapped the two rows cannot pass by
  # having them agree with each other.
  df <- data.frame(
    x = factor(
      c("(Missing)", "(Missing)", "(Missing)", "Yes", "Yes", NA),
      levels = c("(Missing)", "Yes")
    )
  )
  out <- table_categorical(df, select = x, drop_na = FALSE, output = "long")
  expect_identical(out$n[out$level == "(Missing)"], 3)
  expect_identical(out$n[out$level == "(Missing_1)"], 1)
  expect_identical(out$n[out$level == "Yes"], 2)
})

test_that("a declared-but-unobserved '(Missing)' level does not crash", {
  # Audit phase 2 delta, R2/R3/R8: the collision guard scanned only
  # observed values, so a factor DECLARING a "(Missing)" level with no
  # observations plus a real NA used to kill the whole table with a
  # raw "factor level [4] is duplicated" error.
  dx <- data.frame(
    x = factor(c("A", "B", NA), levels = c("A", "B", "(Missing)"))
  )
  lg <- table_categorical(dx, select = x, output = "long")
  expect_identical(lg$level, c("A", "B", "(Missing_1)"))
  expect_identical(lg$n[lg$level == "(Missing_1)"], 1)
  lg2 <- table_categorical(dx, select = x, drop_na = TRUE, output = "long")
  expect_identical(lg2$level, c("A", "B"))

  # Same declaration on the `by` side.
  db <- data.frame(
    x = factor(c("A", "B", "A", "B")),
    g = factor(c("u", "v", NA, "u"), levels = c("u", "v", "(Missing)"))
  )
  lgb <- table_categorical(db, select = x, by = g, output = "long")
  expect_identical(lgb$n[lgb$level == "A" & lgb$group == "(Missing_1)"], 1)
  # The declared-but-unobserved "(Missing)" level stays a zero group.
  expect_identical(lgb$n[lgb$level == "A" & lgb$group == "(Missing)"], 0)
  lgb2 <- table_categorical(
    db,
    select = x,
    by = g,
    drop_na = TRUE,
    output = "long"
  )
  expect_false("(Missing_1)" %in% lgb2$group)
})

test_that("table_categorical handles one-way empty results after dropping missing", {
  df <- data.frame(v1 = c(NA, NA))

  out_long <- table_categorical(
    data = df,
    select = v1,
    drop_na = TRUE,
    output = "long"
  )
  out_wide <- table_categorical(
    data = df,
    select = v1,
    drop_na = TRUE,
    output = "data.frame"
  )

  expect_equal(nrow(out_long), 0L)
  expect_equal(nrow(out_wide), 0L)
  expect_named(out_wide, c("Variable", "Level", "n", "%"))
})

test_that("table_categorical handles grouped empty results after dropping missing", {
  df <- data.frame(
    grp = c("A", "B"),
    v1 = c(NA, NA)
  )

  out_long <- table_categorical(
    data = df,
    select = v1,
    by = grp,
    drop_na = TRUE,
    output = "long"
  )
  out_wide <- table_categorical(
    data = df,
    select = v1,
    by = grp,
    drop_na = TRUE,
    output = "data.frame"
  )
  out_default <- table_categorical(
    data = df,
    select = v1,
    by = grp,
    drop_na = TRUE,
    output = "data.frame"
  )

  expect_equal(nrow(out_long), 0L)
  expect_equal(nrow(out_wide), 0L)
  expect_equal(nrow(out_default), 0L)
  expect_true("Variable" %in% names(out_wide))
  expect_true("Variable" %in% names(out_default))
})

test_that("table_categorical warns about ignored grouped options without by", {
  df <- data.frame(v1 = c("Oui", "Non", "Oui"))

  res <- collect_warnings(
    table_categorical(
      data = df,
      select = "v1",
      include_total = FALSE,
      correct = TRUE,
      simulate_p = TRUE,
      assoc_measure = "phi",
      assoc_ci = TRUE,
      output = "long"
    )
  )

  expect_true(any(grepl("include_total", res$warnings)))
  expect_true(any(grepl("correct", res$warnings)))
  expect_true(any(grepl("simulate_p", res$warnings)))
  expect_true(any(grepl("assoc_measure", res$warnings)))
  expect_true(any(grepl("assoc_ci", res$warnings)))
  expect_s3_class(res$value, "data.frame")
})

test_that("table_categorical default output renders ASCII and returns styled object", {
  # Assigning is silent (decision 47): the ASCII rendering comes from
  # the print method, which is what the auto-print of a bare call
  # dispatches to.
  out <- table_categorical(
    sochealth,
    select = smoking,
    output = "default"
  )
  printed <- capture.output(print(out))

  expect_true(length(printed) > 0)
  expect_s3_class(out, "spicy_categorical_table")
})

test_that("table_categorical returns visibly and is silent when assigned", {
  # A bare call renders (capture.output() evaluates as at top level, so
  # the visible return auto-prints); an assignment writes nothing.
  expect_visible(table_categorical(sochealth, select = smoking))
  bare <- capture.output(table_categorical(sochealth, select = smoking))
  expect_true(length(bare) > 0)

  assigned <- capture.output(
    tc <- table_categorical(sochealth, select = smoking)
  )
  expect_identical(assigned, character(0))
  expect_s3_class(tc, "spicy_categorical_table")
  expect_identical(capture.output(print(tc)), bare)

  # Same on the by-group branch (the second `output == "default"`
  # return of the function).
  expect_visible(table_categorical(sochealth, select = smoking, by = sex))
  bare_by <- capture.output(
    table_categorical(sochealth, select = smoking, by = sex)
  )
  expect_true(length(bare_by) > 0)
  assigned_by <- capture.output(
    tb <- table_categorical(sochealth, select = smoking, by = sex)
  )
  expect_identical(assigned_by, character(0))
  expect_identical(capture.output(print(tb)), bare_by)
})

test_that("table_categorical default output with output = 'data.frame' returns wide raw data", {
  out <- table_categorical(
    sochealth,
    select = smoking,
    output = "data.frame"
  )

  expect_s3_class(out, "data.frame")
  expect_true(all(c("Variable", "Level", "n", "%") %in% names(out)))
})

test_that("table_categorical validates weights and simulate_B", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    v1 = c("Oui", "Non", "Oui", "Non")
  )

  expect_error(
    table_categorical(
      data = df,
      select = "v1",
      by = "grp",
      labels = c(v1 = "Var 1"),
      weights = c(1, 2),
      output = "long"
    ),
    "Numeric `weights` must have length `nrow(data)`.",
    fixed = TRUE
  )

  expect_error(
    table_categorical(
      data = df,
      select = "v1",
      by = "grp",
      labels = c(v1 = "Var 1"),
      simulate_B = 0,
      output = "long"
    ),
    "`simulate_B` must be a positive integer.",
    fixed = TRUE
  )
})

test_that("table_categorical keeps missing values as explicit levels when drop_na is FALSE", {
  df <- data.frame(
    grp = c("A", "A", "B", NA),
    v1 = c("Oui", NA, "Non", "Oui"),
    stringsAsFactors = FALSE
  )

  out_keep <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = c(v1 = "Var 1"),
    drop_na = FALSE,
    simulate_p = FALSE,
    output = "long"
  )

  out_drop <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = c(v1 = "Var 1"),
    drop_na = TRUE,
    simulate_p = FALSE,
    output = "long"
  )

  expect_true(any(grepl("^\\(Missing", out_keep$level)))
  expect_true(any(grepl("^\\(Missing", out_keep$group)))
  expect_false(any(grepl("^\\(Missing", out_drop$level)))
  expect_false(any(grepl("^\\(Missing", out_drop$group)))
})

test_that("table_categorical returns tinytable object when requested", {
  skip_if_not_installed("tinytable")

  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )

  tt <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = c(v1 = "Var 1"),
    simulate_p = FALSE,
    output = "tinytable"
  )

  expect_true(methods::is(tt, "tinytable"))
})

test_that("table_categorical returns one-way rendered objects when requested", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("clipr")

  tt <- table_categorical(
    sochealth,
    select = smoking,
    output = "tinytable"
  )
  expect_true(methods::is(tt, "tinytable"))

  gt_tbl <- table_categorical(
    sochealth,
    select = smoking,
    output = "gt"
  )
  expect_s3_class(gt_tbl, "gt_tbl")

  ft <- table_categorical(
    sochealth,
    select = smoking,
    output = "flextable"
  )
  expect_s3_class(ft, "flextable")

  tmp_xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_xlsx), add = TRUE)
  expect_invisible(
    table_categorical(
      sochealth,
      select = smoking,
      output = "excel",
      excel_path = tmp_xlsx
    )
  )
  expect_true(file.exists(tmp_xlsx))

  tmp_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp_docx), add = TRUE)
  expect_identical(
    table_categorical(
      sochealth,
      select = smoking,
      output = "word",
      word_path = tmp_docx
    ),
    invisible(tmp_docx)
  )
  expect_true(file.exists(tmp_docx))

  # Never the real clipboard, and clipr_available too: under
  # R CMD check the session is non-interactive with no CLIPR_ALLOW,
  # so the pre-flight would refuse before write_clip is reached.
  clip_text <- NULL
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(x, ...) {
      clip_text <<- x
      invisible(NULL)
    },
    .package = "clipr"
  )

  expect_message(
    txt <- table_categorical(
      sochealth,
      select = smoking,
      output = "clipboard"
    ),
    "copied to clipboard",
    class = "spicy_info"
  )
  expect_type(txt, "character")
  expect_identical(txt, invisible(txt))
  expect_match(clip_text, "Variable")
})

test_that("table_categorical returns gt object when requested", {
  skip_if_not_installed("gt")

  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )

  gt_tbl <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = c(v1 = "Var 1"),
    simulate_p = FALSE,
    output = "gt"
  )

  expect_s3_class(gt_tbl, "gt_tbl")
})

# ---- Dynamic association measure column ----------------------------------

test_that("table_categorical default column is Cramer's V", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )
  out <- table_categorical(
    df,
    "v1",
    "grp",
    labels = c(v1 = "Var 1"),
    output = "long"
  )
  # 2x2 -> auto rule now picks phi (see NEWS for 0.11.0).
  expect_true("effect_size" %in% names(out))
  expect_identical(unique(out$effect_size_type), "phi")
  # The wide output still names the column after the measure.
  wide <- table_categorical(
    df,
    "v1",
    "grp",
    labels = c(v1 = "Var 1"),
    output = "data.frame"
  )
  expect_true("Phi" %in% names(wide))
})

test_that("table_categorical auto-rule picks Phi for 2x2, Cramer's V otherwise (mixed -> Effect size header)", {
  # smoking: binary, education: 4-cat nominal, sex: binary
  # auto-rule: smoking -> phi (2x2), education -> cramer_v (not 2x2)
  out <- table_categorical(
    sochealth,
    select = c(smoking, education),
    by = sex,
    output = "long"
  )
  # Long: one stable column, the per-row measure keys beside it.
  expect_true("effect_size" %in% names(out))
  expect_setequal(unique(out$effect_size_type), c("phi", "cramer_v"))
  # The long output's column names do not depend on the measure at all,
  # so a negative membership on "Phi" / "Cramer's V" over `names(out)`
  # could not fail at any setting. The WIDE output is where a measure
  # names a column -- single-measure tables carry the measure's own
  # name, and only the mixed case collapses to the generic header --
  # so that is where both halves of this assertion belong.
  expect_true(
    "Phi" %in%
      names(
        table_categorical(
          sochealth,
          select = smoking,
          by = sex,
          output = "data.frame"
        )
      )
  )
  expect_true(
    "Cramer's V" %in%
      names(
        table_categorical(
          sochealth,
          select = smoking,
          by = education,
          output = "data.frame"
        )
      )
  )
  # Wide: the mixed case collapses to the generic header.
  wide <- table_categorical(
    sochealth,
    select = c(smoking, education),
    by = sex,
    output = "data.frame"
  )
  expect_false(any(c("Phi", "Cramer's V") %in% names(wide)))
  expect_true("Effect size" %in% names(wide))
})

test_that("table_categorical accepts a named per-variable `assoc_measure`", {
  # Same data, but explicit override per variable
  out_default <- table_categorical(
    sochealth,
    select = c(smoking, education),
    by = sex,
    output = "default"
  )
  # The note lists display labels: the column names by default. The
  # attribute is the statistics part of the note, without its "Note.".
  expect_match(
    attr(out_default, "assoc_note"),
    "^Phi: smoking\\. Cramer's V: education\\."
  )

  # Force uniform Cramer's V via single-string -> no measure note; the
  # sentence naming the test is all that is left.
  out_uniform <- table_categorical(
    sochealth,
    select = c(smoking, education),
    by = sex,
    assoc_measure = "cramer_v",
    output = "default"
  )
  expect_identical(
    attr(out_uniform, "assoc_note"),
    "Group comparison: Pearson chi-squared test."
  )
})

test_that("table_categorical accepts unnamed positional `assoc_measure` and validates length", {
  # Positional, length matches select -> works
  out <- table_categorical(
    sochealth,
    select = c(smoking, education),
    by = sex,
    assoc_measure = c("phi", "cramer_v"),
    output = "long"
  )
  expect_true("effect_size" %in% names(out))
  expect_setequal(unique(out$effect_size_type), c("phi", "cramer_v"))

  # Length mismatch (positional vec longer than select) -> actionable error.
  # NB: length-1 unnamed is treated as a uniform single-string application,
  # not as a positional vector, so we use length 3 vs select 2 here.
  expect_error(
    table_categorical(
      sochealth,
      select = c(smoking, education),
      by = sex,
      assoc_measure = c("phi", "cramer_v", "tau_b"),
      output = "long"
    ),
    "Unnamed `assoc_measure` has length 3 but `select` chose 2 variables"
  )
})

test_that("table_categorical errors when `assoc_measure = 'phi'` requested on non-2x2", {
  expect_error(
    table_categorical(
      sochealth,
      select = education,
      by = sex,
      assoc_measure = "phi"
    ),
    "requires a 2x2 table"
  )

  # Same via named per-variable form
  expect_error(
    table_categorical(
      sochealth,
      select = c(smoking, education),
      by = sex,
      assoc_measure = c(education = "phi")
    ),
    "education.+requires a 2x2 table"
  )
})

test_that("table_categorical rejects unknown `assoc_measure` values and bad keys", {
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      assoc_measure = "not_a_measure"
    ),
    "is not one of"
  )
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      assoc_measure = c(no_such_var = "phi")
    ),
    "keys not found in `select`"
  )
})

test_that("table_categorical drops association column when assoc_measure is none", {
  out_long <- table_categorical(
    sochealth,
    select = smoking,
    by = education,
    assoc_measure = "none",
    output = "long"
  )

  out_wide <- table_categorical(
    sochealth,
    select = smoking,
    by = education,
    assoc_measure = "none",
    output = "data.frame"
  )

  expect_false("Cramer's V" %in% names(out_long))
  expect_false(any(grepl("Cramer's V", names(out_wide), fixed = TRUE)))
  expect_true("p" %in% names(out_long))
  expect_true("p" %in% names(out_wide))
})

test_that("table_categorical uses dynamic column name with assoc_measure = 'gamma'", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )
  out <- table_categorical(
    df,
    "v1",
    "grp",
    labels = c(v1 = "Var 1"),
    assoc_measure = "gamma",
    output = "long"
  )
  expect_identical(unique(out$effect_size_type), "gamma")
  # As above: the long output never names a column after the measure,
  # so the exclusion has to be asserted on the wide one, where the
  # default measure WOULD have produced "Cramer's V". The positive
  # control is the same table's own column name.
  expect_true(
    "Cramer's V" %in%
      names(
        table_categorical(
          sochealth,
          select = smoking,
          by = education,
          output = "data.frame"
        )
      )
  )
  wide <- table_categorical(
    df,
    "v1",
    "grp",
    labels = c(v1 = "Var 1"),
    assoc_measure = "gamma",
    output = "data.frame"
  )
  expect_false("Cramer's V" %in% names(wide))
  expect_true("Goodman-Kruskal Gamma" %in% names(wide))
})

test_that("assoc_ci adds CI columns in wide raw output", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "data.frame",
    assoc_ci = TRUE
  )
  expect_true("CI lower" %in% names(out))
  expect_true("CI upper" %in% names(out))
  expect_true(is.numeric(out[["CI lower"]]))
  expect_true(all(!is.na(out[["CI lower"]])))
})

test_that("assoc_ci = FALSE omits CI columns in wide raw output", {
  # Positive control: the two bound headers ARE column names with
  # `assoc_ci = TRUE`, so the negatives below keep biting if either is
  # renamed instead of silently ceasing to match.
  with_ci <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "data.frame",
    assoc_ci = TRUE
  )
  expect_true(all(c("CI lower", "CI upper") %in% names(with_ci)))

  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "data.frame",
    assoc_ci = FALSE
  )
  expect_false("CI lower" %in% names(out))
  expect_false("CI upper" %in% names(out))
})

test_that("the association CI label ignores decimal_mark at its integer coverage", {
  # Decision 27 witness, categorical family: the association interval is
  # pinned at 95% upstream, an integer percentage with no decimal point,
  # so the label is byte-identical under any mark.
  s_of <- function(mk) {
    as_structured(table_categorical(
      sochealth,
      "smoking",
      "education",
      assoc_ci = TRUE,
      decimal_mark = mk
    ))
  }
  lab_of <- function(s) {
    unique(unlist(lapply(s$col_meta, function(m) m$ci_label)))
  }
  expect_identical(lab_of(s_of(",")), "95% CI")
  expect_identical(lab_of(s_of(",")), lab_of(s_of(".")))
})

test_that("assoc_ci adds CI columns in long raw output", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "long",
    assoc_ci = TRUE
  )
  expect_true("ci_lower" %in% names(out))
  expect_true("ci_upper" %in% names(out))
  expect_true(is.numeric(out$ci_lower))
})

test_that("assoc_ci shows inline CI in rendered formats", {
  skip_if_not_installed("gt")
  gt_out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "gt",
    assoc_ci = TRUE
  )
  dat <- gt_out[["_data"]]
  expect_match(dat$assoc_col[1], "\\[")
  # Positive control: the bound header is a real column name on the
  # data.frame route, so this negative is about the gt route inlining
  # the interval -- not about a header that has quietly been renamed.
  expect_true(
    "CI lower" %in%
      names(
        table_categorical(
          sochealth,
          "smoking",
          "education",
          output = "data.frame",
          assoc_ci = TRUE
        )
      )
  )
  expect_false("CI lower" %in% names(dat))
})
# ---- levels_keep ---------------------------------------------------------

test_that("table_categorical levels_keep filters and reorders levels", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    levels_keep = c("Yes"),
    output = "data.frame"
  )
  expect_true(all(out$Level == "Yes", na.rm = TRUE))
})

test_that("table_categorical levels_keep with (Missing)", {
  out <- table_categorical(
    sochealth,
    "income_group",
    "education",
    drop_na = FALSE,
    levels_keep = c("Low", "High", "(Missing)"),
    output = "data.frame"
  )
  lvls <- out$Level[!is.na(out$Level) & out$Level != ""]
  expect_equal(lvls, c("Low", "High", "(Missing)"))
})

test_that("table_categorical levels_keep no-match warns and names the levels", {
  # Labelled column: internal level strings are the "[code] label"
  # display form, so the bare label text can never match -- the
  # variable used to vanish silently (audit finding
  # levels-keep-labelled-silent-empty).
  d <- data.frame(i = 1:20)
  d$smoke <- labelled::labelled(
    rep(c(1, 2), 10),
    labels = c(Smoker = 1, `Non-smoker` = 2),
    label = "Smoking"
  )
  cnd <- testthat::capture_warning(
    table_categorical(
      d,
      select = smoke,
      levels_keep = c("Smoker", "Non-smoker"),
      output = "data.frame"
    )
  )
  expect_s3_class(cnd, "spicy_no_selection")
  expect_match(conditionMessage(cnd), "smoke", fixed = TRUE)
  expect_match(
    conditionMessage(cnd),
    "\"[1] Smoker\", \"[2] Non-smoker\"",
    fixed = TRUE
  )
})

test_that("table_categorical levels_keep no-match keeps the matching variable", {
  # Multi-variable select: only the variable with zero matches is
  # dropped (with a warning); the matching one still renders.
  df <- data.frame(
    a = factor(c("x", "y", "x", "y")),
    b = factor(c("p", "q", "p", "q"))
  )
  expect_warning(
    out <- table_categorical(
      df,
      select = c(a, b),
      levels_keep = c("x", "y"),
      output = "data.frame"
    ),
    class = "spicy_no_selection"
  )
  expect_true(all(out$Level %in% c("x", "y")))
  expect_true(nrow(out) > 0L)
})

test_that("table_categorical grouped levels_keep no-match warns per variable", {
  df <- data.frame(
    grp = factor(c("A", "B", "A", "B")),
    v = factor(c("x", "y", "x", "y"))
  )
  cnd <- testthat::capture_warning(
    table_categorical(
      df,
      select = v,
      by = grp,
      levels_keep = c("bogus"),
      output = "long"
    )
  )
  expect_s3_class(cnd, "spicy_no_selection")
  expect_match(conditionMessage(cnd), "\"x\", \"y\"", fixed = TRUE)
})

test_that(".warn_levels_keep_no_match truncates long level listings", {
  cnd <- testthat::capture_warning(
    .warn_levels_keep_no_match("v", as.character(1:12))
  )
  expect_s3_class(cnd, "spicy_no_selection")
  expect_match(conditionMessage(cnd), "\"10\", ...", fixed = TRUE)
  expect_no_match(conditionMessage(cnd), "\"11\"", fixed = TRUE)

  # Defensive degenerate input: nothing available to list.
  cnd_none <- testthat::capture_warning(
    .warn_levels_keep_no_match("v", NA_character_)
  )
  expect_match(conditionMessage(cnd_none), "(none)", fixed = TRUE)
})
# ---- blank_na_wide -------------------------------------------------------

test_that("table_categorical blank_na_wide replaces NA with empty strings", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "data.frame",
    blank_na_wide = TRUE
  )
  chr_cols <- vapply(out, is.character, logical(1))
  if (any(chr_cols)) {
    expect_false(any(is.na(out[chr_cols])))
  }
})

# ---- Validation errors ---------------------------------------------------

test_that("table_categorical validates data argument", {
  expect_error(
    table_categorical("not_df", "v1", "grp"),
    "`data` must be a data.frame"
  )
})

test_that("table_categorical validates select", {
  df <- data.frame(g = 1, v = 1)
  expect_error(
    table_categorical(df, character(0), "g"),
    "`select` must select at least one column"
  )
  expect_error(
    table_categorical(df, "missing", "g"),
    "Some `select` columns are missing"
  )
})

test_that("table_categorical validates by", {
  df <- data.frame(g = 1, v = 1)
  expect_error(
    table_categorical(df, "v", "missing"),
    "`by` must select exactly one column"
  )
})

test_that("table_categorical rejects unnamed labels vectors", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_categorical(df, "v", "g", labels = c("a", "b")),
    class = "spicy_invalid_input"
  )
})

test_that("table_categorical validates boolean arguments", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_categorical(df, "v", "g", include_total = NA),
    "`include_total` must be"
  )
  expect_error(
    table_categorical(df, "v", "g", drop_na = "yes"),
    "`drop_na` must be"
  )
  expect_error(
    table_categorical(df, "v", "g", rescale = NA),
    "`rescale` must be"
  )
  expect_error(
    table_categorical(df, "v", "g", correct = NA),
    "`correct` must be"
  )
  expect_error(
    table_categorical(df, "v", "g", simulate_p = NA),
    "`simulate_p` must be"
  )
  expect_error(
    table_categorical(df, "v", "g", blank_na_wide = NA),
    "`blank_na_wide` must be"
  )
  expect_error(
    table_categorical(df, "v", "g", add_multilevel_header = NA),
    "`add_multilevel_header` must be"
  )
})

test_that("table_categorical validates decimal_mark", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_categorical(df, "v", "g", decimal_mark = "--"),
    "`decimal_mark` must be"
  )
})

test_that("table_categorical validates weights type", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_categorical(df, "v", "g", weights = TRUE),
    "`weights` must be NULL"
  )
})

test_that("table_categorical validates weights column name", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_categorical(df, "v", "g", weights = "nonexistent"),
    "column name in `data`"
  )
})

test_that("table_categorical warns when rescale = TRUE without weights", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_warning(
    table_categorical(df, "v", "g", rescale = TRUE, output = "data.frame"),
    "rescale = TRUE.*no effect"
  )
})

test_that("table_categorical honors options(spicy.rescale)", {
  df <- data.frame(v = rep(c("a", "b"), 5), w = rep(2, 10))
  withr::local_options(spicy.rescale = TRUE)
  out <- table_categorical(df, "v", weights = "w", output = "data.frame")
  expect_equal(sum(out$n), 10)
})

test_that("explicit rescale overrides options(spicy.rescale)", {
  df <- data.frame(v = rep(c("a", "b"), 5), w = rep(2, 10))
  withr::local_options(spicy.rescale = TRUE)
  out <- table_categorical(
    df,
    "v",
    weights = "w",
    rescale = FALSE,
    output = "data.frame"
  )
  expect_equal(sum(out$n), 20)
})

test_that("table_categorical rejects p_digits < 1 with a classed error", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_categorical(df, "v", "g", p_digits = 0, output = "long"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical(df, "v", "g", p_digits = 0, output = "long"),
    "integer >= 1"
  )
  # percent_digits / v_digits keep their >= 0 bound: 0 is legitimate.
  expect_silent(
    table_categorical(
      df,
      "v",
      "g",
      percent_digits = 0,
      v_digits = 0,
      output = "long"
    )
  )
})

# ---- Multiple select -----------------------------------------------------

test_that("table_categorical handles multiple select in wide output", {
  out <- table_categorical(
    sochealth,
    c(smoking, physical_activity),
    education,
    output = "data.frame"
  )
  expect_true(all(c("smoking", "physical_activity") %in% out$Variable))
})

# ---- include_total = FALSE -----------------------------------------------

test_that("table_categorical include_total = FALSE omits Total column", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    include_total = FALSE,
    output = "data.frame"
  )
  expect_false(any(grepl("^Total", names(out))))
})

# ---- Flextable output ----------------------------------------------------

test_that("table_categorical returns flextable object when requested", {
  skip_if_not_installed("flextable")
  ft <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "flextable"
  )
  expect_s3_class(ft, "flextable")
})

test_that("table_categorical grouped word and clipboard outputs work", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  skip_if_not_installed("clipr")

  tmp_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp_docx), add = TRUE)
  path <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "word",
    word_path = tmp_docx
  )
  expect_identical(path, invisible(tmp_docx))
  expect_true(file.exists(tmp_docx))

  # Never the real clipboard; clipr_available mocked too so the
  # pre-flight passes in the non-interactive R CMD check session.
  clip_text <- NULL
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(x, ...) {
      clip_text <<- x
      invisible(NULL)
    },
    .package = "clipr"
  )

  txt <- suppressMessages(table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "clipboard",
    assoc_ci = TRUE
  ))
  expect_type(txt, "character")
  expect_match(clip_text, "Cramer's V")
  expect_match(clip_text, "CI lower")
})

test_that("table_categorical requires file paths for word and excel outputs", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  skip_if_not_installed("openxlsx2")

  expect_error(
    table_categorical(sochealth, "smoking", output = "word"),
    "word_path"
  )
  expect_error(
    table_categorical(sochealth, "smoking", output = "excel"),
    "excel_path"
  )
})

# ---- Excel output --------------------------------------------------------

test_that("table_categorical writes excel file", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "excel",
    excel_path = tmp
  )
  expect_true(file.exists(tmp))
})

# ---- assoc_measure = "none" ----------------------------------------------

test_that("table_categorical assoc_measure = 'none' omits association column", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    assoc_measure = "none",
    output = "long"
  )
  # This used to assert that a column named after the measure was
  # absent. The long output names its association column `effect_size`
  # whatever the measure is, so no such column can ever exist and the
  # assertion was vacuous by construction, not merely fragile to a
  # rename. The columns the output CAN carry are what carries it now,
  # with a positive control on the same call minus `assoc_measure`.
  kept <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "long"
  )
  expect_true(all(c("effect_size", "effect_size_type") %in% names(kept)))
  expect_false(any(c("effect_size", "effect_size_type") %in% names(out)))
})
test_that("table_categorical with assoc_ci includes CI columns in raw long", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    assoc_ci = TRUE,
    output = "long"
  )
  expect_true("CI lower" %in% names(out) || "ci_lower" %in% names(out))
})

test_that("table_categorical simulate_p works in long output", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    simulate_p = TRUE,
    output = "long"
  )
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
})

test_that("table_categorical with drop_na = FALSE includes Missing level", {
  df <- sochealth
  df$smoking[1:5] <- NA
  out <- table_categorical(
    df,
    "smoking",
    "education",
    drop_na = FALSE,
    output = "long"
  )
  expect_true(any(grepl("Missing", out$level)))
})

# ---- Digit validation ----------------------------------------------------

test_that("table_categorical rejects invalid digit arguments", {
  df <- data.frame(
    grp = c("A", "B", "A", "B"),
    v1 = c("x", "y", "x", "y")
  )

  expect_error(
    table_categorical(df, "v1", "grp", percent_digits = -1, output = "long"),
    "percent_digits"
  )
  expect_error(
    table_categorical(df, "v1", "grp", p_digits = "a", output = "long"),
    "p_digits"
  )
  expect_error(
    table_categorical(df, "v1", "grp", v_digits = NA, output = "long"),
    "v_digits"
  )
})

# ---- Level ordering ------------------------------------------------------

test_that("table_categorical preserves factor level order in row variables", {
  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = factor(
      c("Low", "High", "Medium", "Low", "High", "Medium"),
      levels = c("Low", "Medium", "High")
    )
  )
  out <- table_categorical(
    df,
    "v1",
    "grp",
    include_total = FALSE,
    output = "long"
  )
  lvs <- unique(out$level)
  expect_equal(lvs, c("Low", "Medium", "High"))
})

test_that("table_categorical places (Missing) at end when drop_na = FALSE", {
  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = factor(
      c("Yes", NA, "No", "Yes", NA, "No"),
      levels = c("Yes", "No")
    )
  )
  out <- table_categorical(
    df,
    "v1",
    "grp",
    drop_na = FALSE,
    include_total = FALSE,
    output = "long"
  )
  lvs <- unique(out$level)
  expect_equal(lvs, c("Yes", "No", "(Missing)"))
})

test_that("table_categorical rescale warning includes call. = FALSE", {
  df <- data.frame(
    grp = c("A", "B", "A", "B"),
    v1 = c("x", "y", "x", "y")
  )
  w <- tryCatch(
    table_categorical(
      df,
      "v1",
      "grp",
      rescale = TRUE,
      output = "long"
    ),
    warning = function(w) w
  )
  # Spicy classed warnings inherit from `rlang_warning` (a wider class
  # than base `simpleWarning`); `expect_s3_class` matches either.
  expect_s3_class(w, "warning")
  expect_null(w$call)
})

# ---- grouped empty data returns character columns, not logical ----

test_that("grouped table with empty data returns character(0) columns", {
  df <- data.frame(
    x = factor(levels = c("a", "b")),
    g = factor(levels = c("A", "B")),
    stringsAsFactors = FALSE
  )
  out <- table_categorical(df, select = "x", by = "g", output = "data.frame")
  # All columns should be character, not logical
  col_types <- vapply(out, typeof, character(1))
  expect_true(all(col_types == "character"))
})

test_that("table_categorical gt output omits association header when assoc_measure = 'none'", {
  skip_if_not_installed("gt")

  gt_tbl <- table_categorical(
    sochealth,
    "smoking",
    "education",
    assoc_measure = "none",
    output = "gt"
  )

  boxhead <- gt_tbl[["_boxhead"]]
  spanners <- gt_tbl[["_spanners"]]

  expect_false(any(boxhead$column_label == "Cramer's V"))
  expect_false(any(boxhead$var == "assoc_col"))
  expect_false(any(spanners$spanner_id == "spn_assoc"))
  expect_false(any(spanners$spanner_label == "Cramer's V"))
})

# --- Coverage tests: uncovered paths ---

test_that("table_categorical errors when select matches no columns", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_error(
    table_categorical(df, select = c(nonexistent_col)),
    "select"
  )
})

test_that("table_categorical one-way with weights", {
  df <- data.frame(
    x = factor(c("A", "B", "A", "B", "A")),
    w = c(2, 1, 3, 1, 2)
  )
  out <- table_categorical(df, select = x, weights = w, output = "data.frame")
  expect_s3_class(out, "data.frame")
  expect_true(all(c("Variable", "Level", "n", "%") %in% names(out)))
  # Weighted n should reflect weights
  expect_equal(sum(out$n), sum(df$w))
})

test_that("table_categorical one-way with levels_keep filters levels", {
  df <- data.frame(
    x = factor(c("A", "B", "C", "A", "B"), levels = c("A", "B", "C"))
  )
  out <- table_categorical(
    df,
    select = x,
    levels_keep = c("B", "A"),
    output = "data.frame"
  )
  expect_equal(as.character(out$Level), c("B", "A"))
})

test_that("table_categorical one-way with levels_keep and long output", {
  df <- data.frame(
    x = factor(c("A", "B", "C", "A", "B"), levels = c("A", "B", "C"))
  )
  out <- table_categorical(
    df,
    select = x,
    levels_keep = c("B", "C"),
    output = "long"
  )
  expect_true(all(out$level %in% c("B", "C")))
})

test_that("table_categorical one-way with decimal_mark comma", {
  df <- data.frame(x = factor(c("A", "B", "A", "B", "A")))
  out <- table_categorical(
    df,
    select = x,
    decimal_mark = ",",
    output = "default"
  )
  disp <- attr(out, "display_df")
  # Percentages should use comma as decimal separator
  pct_col <- disp[["%"]]
  expect_true(any(grepl(",", pct_col)))
})

test_that("table_categorical one-way with blank_na_wide", {
  df <- data.frame(
    x = factor(c("A", NA, "B", NA)),
    y = factor(c("C", "D", NA, NA))
  )
  out <- table_categorical(
    df,
    select = c(x, y),
    drop_na = TRUE,
    blank_na_wide = TRUE,
    output = "data.frame"
  )
  expect_s3_class(out, "data.frame")
})

test_that("table_categorical one-way empty after dropping NA produces 0-row data.frame", {
  df <- data.frame(x = factor(c(NA, NA, NA)))
  out_wide <- table_categorical(
    df,
    select = x,
    drop_na = TRUE,
    output = "data.frame"
  )
  expect_equal(nrow(out_wide), 0L)
  out_long <- table_categorical(df, select = x, drop_na = TRUE, output = "long")
  expect_equal(nrow(out_long), 0L)
})

test_that("table_categorical grouped default output returns a styled object", {
  out <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex
  )
  expect_s3_class(out, "spicy_categorical_table")
  expect_equal(attr(out, "group_var"), "sex")
})

test_that("table_categorical grouped with levels_keep filters levels", {
  out <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    levels_keep = c("Yes"),
    output = "data.frame"
  )
  out_long <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    levels_keep = c("Yes"),
    output = "long"
  )
  expect_true(all(out_long$level == "Yes"))
})

test_that("table_categorical grouped with assoc_measure = none", {
  out <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    assoc_measure = "none",
    output = "long"
  )
  # No association measure column. As in the test above, the long
  # output never names one after the measure, so the stable column
  # names are what carries this; the positive control is the same call
  # with the default measure.
  kept <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    output = "long"
  )
  expect_true(all(c("effect_size", "effect_size_type") %in% names(kept)))
  expect_false(any(c("effect_size", "effect_size_type") %in% names(out)))
})

test_that("table_categorical grouped empty after dropping NA", {
  df <- data.frame(
    grp = factor(c("A", "B")),
    v = factor(c(NA, NA))
  )
  out_wide <- table_categorical(
    df,
    select = v,
    by = grp,
    drop_na = TRUE,
    output = "data.frame"
  )
  expect_equal(nrow(out_wide), 0L)
  out_long <- table_categorical(
    df,
    select = v,
    by = grp,
    drop_na = TRUE,
    output = "long"
  )
  expect_equal(nrow(out_long), 0L)
})

test_that("table_categorical one-way weighted with rescale", {
  df <- data.frame(
    x = factor(c("A", "B", "A", "B", "A")),
    w = c(10, 5, 10, 5, 10)
  )
  out <- table_categorical(
    df,
    select = x,
    weights = w,
    rescale = TRUE,
    output = "data.frame"
  )
  # After rescaling, total n should equal nrow(df)
  expect_equal(sum(out$n), nrow(df), tolerance = 0.01)
})

test_that("table_categorical handles Missing_ label collision", {
  df <- data.frame(
    x = factor(c("(Missing)", "(Missing_1)", NA, "B")),
    stringsAsFactors = FALSE
  )
  out <- table_categorical(df, select = x, drop_na = FALSE, output = "long")
  expect_true(any(grepl("Missing", out$level)))
  # Should not have duplicate level names
  expect_equal(length(unique(out$level)), nrow(out))
})

test_that("table_categorical grouped with levels_keep and default output", {
  out <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    levels_keep = c("Yes")
  )
  expect_s3_class(out, "spicy_categorical_table")
  disp <- attr(out, "display_df")
  # Only "Yes" level should appear in indented rows
  indented <- disp$Variable[startsWith(disp$Variable, "  ")]
  expect_true(all(trimws(indented) == "Yes"))
})

test_that("table_categorical grouped empty via levels_keep with non-matching levels", {
  df <- data.frame(
    grp = factor(c("A", "B", "A", "B")),
    v = factor(c("x", "y", "x", "y"))
  )
  expect_warning(
    out <- table_categorical(
      df,
      select = v,
      by = grp,
      levels_keep = c("nonexistent"),
      output = "data.frame"
    ),
    class = "spicy_no_selection"
  )
  expect_equal(nrow(out), 0L)
  expect_warning(
    out_long <- table_categorical(
      df,
      select = v,
      by = grp,
      levels_keep = c("nonexistent"),
      output = "long"
    ),
    class = "spicy_no_selection"
  )
  expect_equal(nrow(out_long), 0L)
  # Also with assoc_measure = "none" to cover L1196
  expect_warning(
    out_none <- table_categorical(
      df,
      select = v,
      by = grp,
      levels_keep = c("nonexistent"),
      assoc_measure = "none",
      output = "long"
    ),
    class = "spicy_no_selection"
  )
  expect_equal(nrow(out_none), 0L)
})

test_that("table_categorical grouped with assoc_measure = none and default output", {
  out <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    assoc_measure = "none"
  )
  expect_s3_class(out, "spicy_categorical_table")
})

test_that("table_categorical one-way with levels_keep that filters some levels", {
  df <- data.frame(
    x = factor(c("A", "B", "C", "D"), levels = c("A", "B", "C", "D")),
    y = factor(c("P", "Q", "P", "Q"))
  )
  out <- table_categorical(
    df,
    select = c(x, y),
    levels_keep = c("A", "C", "P"),
    output = "data.frame"
  )
  # Should only have matching levels
  expect_true(all(out$Level %in% c("A", "C", "P")))
})

test_that("table_categorical grouped with decimal_mark comma and default output", {
  out <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    decimal_mark = ","
  )
  expect_s3_class(out, "spicy_categorical_table")
  disp <- attr(out, "display_df")
  # p-value and percentage columns should use comma
  p_col <- disp$p
  non_empty_p <- p_col[nzchar(p_col)]
  if (length(non_empty_p) > 0) {
    expect_true(any(grepl(",", non_empty_p)) || any(grepl("<", non_empty_p)))
  }
})

test_that("table_categorical grouped tinytable with assoc_measure = none", {
  skip_if_not_installed("tinytable")
  tt <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    assoc_measure = "none",
    output = "tinytable"
  )
  expect_true(inherits(tt, "tinytable"))
})

test_that("table_categorical grouped excel with assoc_ci", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  path <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    assoc_ci = TRUE,
    output = "excel",
    excel_path = tmp
  )
  expect_true(file.exists(path))
})

test_that("table_categorical grouped excel with assoc_measure = none", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  path <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    assoc_measure = "none",
    output = "excel",
    excel_path = tmp
  )
  expect_true(file.exists(path))
})

test_that("table_categorical one-way all-NA renders empty default table", {
  df <- data.frame(x = factor(c(NA, NA, NA)))
  out <- table_categorical(df, select = x, drop_na = TRUE, output = "default")
  expect_s3_class(out, "spicy_categorical_table")
  expect_equal(nrow(out), 0L)
})

test_that("table_categorical grouped all-NA renders empty default table", {
  df <- data.frame(
    grp = factor(c("A", "B", "A")),
    v = factor(c(NA, NA, NA))
  )
  out <- table_categorical(
    df,
    select = v,
    by = grp,
    drop_na = TRUE,
    output = "default"
  )
  expect_s3_class(out, "spicy_categorical_table")
})

test_that("table_categorical one-way levels_keep with no match returns empty", {
  df <- data.frame(x = factor(c("A", "B"), levels = c("A", "B", "C")))
  expect_warning(
    out <- table_categorical(
      df,
      select = x,
      levels_keep = c("nonexistent"),
      output = "data.frame"
    ),
    class = "spicy_no_selection"
  )
  expect_equal(nrow(out), 0L)
  # levels_keep includes "C" which exists in factor levels but has 0 obs
  # -> covers the `next` at match(lv, vals) returning NA; partial
  # matches keep their intersect semantics with no warning
  expect_silent(
    out2 <- table_categorical(
      df,
      select = x,
      levels_keep = c("A", "C"),
      output = "data.frame"
    )
  )
  expect_equal(nrow(out2), 1L)
  expect_equal(as.character(out2$Level), "A")
  # Also test default output path (covers make_report_wide_oneway empty path)
  expect_warning(
    out3 <- table_categorical(
      df,
      select = x,
      levels_keep = c("nonexistent"),
      output = "default"
    ),
    class = "spicy_no_selection"
  )
  expect_s3_class(out3, "spicy_categorical_table")
})

test_that("table_categorical errors for missing tinytable package", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "tinytable") {
        return(FALSE)
      }
      TRUE
    },
    .package = "base"
  )
  expect_error(
    table_categorical(sochealth, select = smoking, output = "tinytable"),
    "tinytable"
  )
})

test_that("table_categorical errors for missing gt package", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "gt") {
        return(FALSE)
      }
      TRUE
    },
    .package = "base"
  )
  expect_error(
    table_categorical(sochealth, select = smoking, output = "gt"),
    "gt"
  )
})

test_that("table_categorical errors for missing flextable package", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "flextable") {
        return(FALSE)
      }
      TRUE
    },
    .package = "base"
  )
  expect_error(
    table_categorical(sochealth, select = smoking, output = "flextable"),
    "flextable",
    class = "spicy_missing_pkg"
  )
})

test_that("table_categorical errors for missing openxlsx2 package", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "openxlsx2") {
        return(FALSE)
      }
      TRUE
    },
    .package = "base"
  )
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      output = "excel",
      excel_path = tempfile(fileext = ".xlsx")
    ),
    "openxlsx2"
  )
})

test_that("table_categorical errors for missing clipr package", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "clipr") {
        return(FALSE)
      }
      TRUE
    },
    .package = "base"
  )
  expect_error(
    table_categorical(sochealth, select = smoking, output = "clipboard"),
    "clipr"
  )
})

test_that("table_categorical one-way word output errors for missing officer package", {
  skip("Cannot mock officer requireNamespace without recursion")
})

test_that("table_categorical word output errors when word_path is missing", {
  expect_error(
    table_categorical(sochealth, select = smoking, by = sex, output = "word"),
    "word_path"
  )
})

test_that("table_categorical excel output errors when excel_path is missing", {
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      output = "excel"
    ),
    "excel_path"
  )
})

test_that("table_categorical grouped errors for missing tinytable", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "tinytable") {
        return(FALSE)
      }
      TRUE
    },
    .package = "base"
  )
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      output = "tinytable"
    ),
    "tinytable"
  )
})

test_that("table_categorical grouped errors for missing gt", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "gt") {
        return(FALSE)
      }
      TRUE
    },
    .package = "base"
  )
  expect_error(
    table_categorical(sochealth, select = smoking, by = sex, output = "gt"),
    "gt"
  )
})

test_that("table_categorical grouped errors for missing flextable", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "flextable") {
        return(FALSE)
      }
      TRUE
    },
    .package = "base"
  )
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      output = "flextable"
    ),
    "flextable",
    class = "spicy_missing_pkg"
  )
})

test_that("table_categorical grouped errors for missing openxlsx2", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "openxlsx2") {
        return(FALSE)
      }
      TRUE
    },
    .package = "base"
  )
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      output = "excel",
      excel_path = tempfile(fileext = ".xlsx")
    ),
    "openxlsx2"
  )
})

test_that("table_categorical grouped errors for missing clipr", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "clipr") {
        return(FALSE)
      }
      TRUE
    },
    .package = "base"
  )
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      output = "clipboard"
    ),
    "clipr"
  )
})

test_that("table_categorical grouped word errors for missing officer", {
  skip("Cannot mock officer requireNamespace without recursion")
})

# ---- harmonization with table_continuous() / _lm() (Phase 2) -------------

test_that("align argument validates and is stored as attribute", {
  for (a in c("decimal", "center", "right")) {
    out <- table_categorical(sochealth, select = smoking, by = sex, align = a)
    expect_equal(attr(out, "align"), a)
  }
  expect_error(
    table_categorical(sochealth, select = smoking, by = sex, align = "bogus"),
    "`align` must be one of",
    class = "spicy_invalid_input"
  )
})

test_that("align defaults to 'decimal' on the printed object", {
  out <- table_categorical(sochealth, select = smoking, by = sex)
  expect_equal(attr(out, "align"), "decimal")
  out_ow <- table_categorical(sochealth, select = smoking)
  expect_equal(attr(out_ow, "align"), "decimal")
})

test_that("align = 'decimal' produces gt and tinytable outputs", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tinytable")
  out_gt <- table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    output = "gt",
    align = "decimal"
  )
  expect_s3_class(out_gt, "gt_tbl")

  out_tt <- table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    output = "tinytable",
    align = "decimal"
  )
  expect_true(inherits(out_tt, "tinytable"))
})

test_that("align = 'center' / 'right' all render gt + tinytable", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tinytable")
  for (a in c("center", "right")) {
    expect_s3_class(
      table_categorical(
        sochealth,
        select = smoking,
        by = sex,
        output = "gt",
        align = a
      ),
      "gt_tbl"
    )
    expect_true(inherits(
      table_categorical(
        sochealth,
        select = smoking,
        by = sex,
        output = "tinytable",
        align = a
      ),
      "tinytable"
    ))
  }
})

test_that("align = 'decimal' / 'center' / 'right' all render flextable", {
  skip_if_not_installed("flextable")
  for (a in c("decimal", "center", "right")) {
    expect_s3_class(
      table_categorical(
        sochealth,
        select = smoking,
        by = sex,
        output = "flextable",
        align = a
      ),
      "flextable"
    )
    expect_s3_class(
      table_categorical(
        sochealth,
        select = smoking,
        output = "flextable",
        align = a
      ),
      "flextable"
    )
  }
})

test_that("align flows to word output (cross-tab + oneway)", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  for (a in c("decimal", "center")) {
    tmp <- tempfile(fileext = ".docx")
    on.exit(unlink(tmp), add = TRUE)
    res <- table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      output = "word",
      word_path = tmp,
      align = a
    )
    expect_equal(res, tmp)
    expect_true(file.exists(tmp))

    tmp2 <- tempfile(fileext = ".docx")
    on.exit(unlink(tmp2), add = TRUE)
    res2 <- table_categorical(
      sochealth,
      select = smoking,
      output = "word",
      word_path = tmp2,
      align = a
    )
    expect_equal(res2, tmp2)
    expect_true(file.exists(tmp2))
  }
})

test_that("align flows to excel output (cross-tab + oneway, all values)", {
  skip_if_not_installed("openxlsx2")
  for (a in c("decimal", "center", "right")) {
    tmp <- tempfile(fileext = ".xlsx")
    on.exit(unlink(tmp), add = TRUE)
    res <- table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      output = "excel",
      excel_path = tmp,
      align = a
    )
    expect_equal(res, tmp)
    expect_true(file.exists(tmp))
  }
})

test_that("align = 'decimal' pads numeric clipboard cells (oneway + cross-tab)", {
  skip_if_not_installed("clipr")
  captured <- new.env()
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(text, ...) {
      captured$text <- text
      invisible(text)
    },
    .package = "clipr"
  )

  # Oneway: the payload is the same text whatever `align` asks for.
  # Alignment is a fixed-width-rendering concern; a delimited payload
  # is parsed, and the U+2007 pad character is not whitespace to a
  # parser (a padded number would paste as text).
  table_categorical(
    sochealth,
    select = smoking,
    output = "clipboard",
    align = "decimal"
  )
  txt_dec <- captured$text
  table_categorical(
    sochealth,
    select = smoking,
    output = "clipboard",
    align = "center"
  )
  txt_center <- captured$text
  expect_identical(txt_dec, txt_center)
  expect_false(grepl("\u2007", txt_dec, fixed = TRUE))

  # Cross-tab: p / association cells travel as the plain strings the
  # console prints -- no Excel text formula (`="..."`), which is
  # meaningless in a text editor or a word processor, and no padding.
  table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    output = "clipboard",
    align = "decimal"
  )
  txt_ct_dec <- captured$text
  cells <- unlist(strsplit(txt_ct_dec, "[\t\n]"))
  expect_false(any(grepl("=\"", cells, fixed = TRUE)))
  expect_false(any(grepl("\u2007", cells, fixed = TRUE)))
  expect_true(".713" %in% cells)
  # Cells that carry nothing are empty, not runs of padding.
  expect_true(any(!nzchar(cells)))
})

test_that("clipboard payload carries the console title and notes", {
  skip_if_not_installed("clipr")
  captured <- new.env()
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(text, ...) {
      captured$text <- text
      invisible(text)
    },
    .package = "clipr"
  )

  d <- sochealth
  d$smoking[1:40] <- NA

  table_categorical(
    d,
    select = c(smoking, education),
    by = sex,
    drop_na = TRUE,
    output = "clipboard"
  )
  lines <- strsplit(captured$text, "\n", fixed = TRUE)[[1L]]
  cells <- strsplit(lines, "\t", fixed = TRUE)

  # Title on the first row, disclosure notes on the last rows: what
  # left the table, then which measure each row's effect size is.
  expect_identical(cells[[1L]][1L], "Categorical table by sex")
  expect_match(
    lines[length(lines) - 1L],
    "^Note\\. Missing values removed: smoking \\(65\\)\\."
  )
  expect_match(
    lines[length(lines)],
    "^Phi: smoking\\. Cramer's V: education\\."
  )
  # Title / note rows are padded to the full grid, so every line of
  # the payload holds the same number of fields (trailing empty
  # fields make delimiter counting the honest measure here).
  n_delims <- vapply(
    gregexpr("\t", lines, fixed = TRUE),
    length,
    integer(1)
  )
  expect_length(unique(n_delims), 1L)

  # One-way tables name themselves too.
  table_categorical(sochealth, select = smoking, output = "clipboard")
  first_line <- strsplit(captured$text, "\n", fixed = TRUE)[[1L]][1L]
  expect_identical(
    strsplit(first_line, "\t", fixed = TRUE)[[1L]][1L],
    "Categorical table"
  )
})


# ---- broom S3 methods -----------------------------------------------------

test_that("as.data.frame() strips spicy classes and rendering attrs", {
  out <- table_categorical(sochealth, select = smoking, by = sex)
  df <- as.data.frame(out)
  expect_true(inherits(df, "data.frame"))
  expect_false("spicy_categorical_table" %in% class(df))
  expect_false("spicy_table" %in% class(df))
  expect_null(attr(df, "display_df"))
  expect_null(attr(df, "long_data"))
  expect_null(attr(df, "align"))
  # group_var preserved as provenance
  expect_equal(attr(df, "group_var"), "sex")
})

test_that("as_tibble() returns a tbl_df", {
  skip_if_not_installed("tibble")
  out <- table_categorical(sochealth, select = smoking, by = sex)
  tb <- tibble::as_tibble(out)
  expect_s3_class(tb, "tbl_df")
})

test_that("tidy() returns long-format with broom-conventional columns (cross-tab)", {
  out <- table_categorical(
    sochealth,
    select = c(smoking, physical_activity),
    by = sex
  )
  td <- broom::tidy(out)
  expect_setequal(
    names(td),
    c("outcome", "level", "group", "n", "proportion")
  )
  expect_true(all(td$proportion >= 0 & td$proportion <= 1))
  # `outcome` carries the display labels: the column names by default.
  expect_equal(unique(td$outcome), c("smoking", "physical_activity"))
  # Real groups appear; the synthetic "Total" marginal is excluded
  # by `tidy()` (one row per real group, broom convention).
  expect_setequal(unique(td$group), c("Female", "Male"))
})

test_that("tidy() returns no group column without by", {
  out <- table_categorical(sochealth, select = smoking)
  td <- broom::tidy(out)
  expect_false("group" %in% names(td))
  expect_setequal(names(td), c("outcome", "level", "n", "proportion"))
})

test_that("glance() returns chi-squared test + association measure (cross-tab)", {
  out <- table_categorical(
    sochealth,
    select = c(smoking, physical_activity),
    by = sex
  )
  gl <- broom::glance(out)
  expect_setequal(
    names(gl),
    c(
      "outcome",
      "test_type",
      "statistic",
      "df",
      "p.value",
      "assoc_type",
      "assoc_value",
      "assoc_ci_lower",
      "assoc_ci_upper",
      "n_total"
    )
  )
  expect_equal(nrow(gl), 2L)
  expect_true(all(gl$test_type == "chi_squared"))
  expect_true(all(is.finite(gl$statistic)))
  expect_true(all(gl$df >= 1L))
  expect_true(all(gl$p.value >= 0 & gl$p.value <= 1))
  # smoking and physical_activity are both binary, sex is binary -> 2x2
  # auto-rule picks Phi (see NEWS for 0.11.0).
  expect_true(all(gl$assoc_type == "Phi"))
})

test_that("glance() returns NA test/ES, populated n_total without by", {
  out <- table_categorical(sochealth, select = smoking)
  gl <- broom::glance(out)
  expect_equal(nrow(gl), 1L)
  expect_true(is.na(gl$test_type))
  expect_true(is.na(gl$statistic))
  expect_true(is.na(gl$p.value))
  # Default drop_na = FALSE (0.13.0): the tabulated sample includes the
  # displayed "(Missing)" level, so n_total is the full 1200; opting
  # into drop_na = TRUE recovers the observed-only 1175.
  expect_equal(gl$n_total, 1200L)
  out_cc <- table_categorical(sochealth, select = smoking, drop_na = TRUE)
  expect_equal(broom::glance(out_cc)$n_total, 1175L)
})

test_that("a row whose variable asked for no measure has no effect_size_type", {
  # The only route to the NA arm: a mixed vector where one variable opts
  # out. Its rows still sit in the shared `effect_size` column -- empty --
  # but they name no measure.
  out <- table_categorical(
    sochealth,
    select = c(smoking, education),
    by = sex,
    labels = c(smoking = "Smoking", education = "Education"),
    assoc_measure = c(smoking = "none", education = "cramer_v"),
    output = "long"
  )
  expect_true(all(c("effect_size", "effect_size_type") %in% names(out)))
  expect_true(all(is.na(out$effect_size_type[out$variable == "Smoking"])))
  expect_true(all(is.na(out$effect_size[out$variable == "Smoking"])))
  expect_identical(
    unique(out$effect_size_type[out$variable == "Education"]),
    "cramer_v"
  )
  expect_false(any(is.na(out$effect_size[out$variable == "Education"])))
})

test_that("print() survives a typed view that outlived its display frame", {
  # The shape guard on `display_labels`, not merely `is.null(structured)`:
  # here the typed view is PRESENT while `display_df` is gone, so the
  # printer falls back to the raw wide frame -- which carries `Level`,
  # `Chi2` and `df` the typed view does not. Handing that frame the typed
  # labels aborts in spicy_print_table(); reverting the guard to
  # `is.null(s)` alone fails this test.
  out <- table_categorical(sochealth, select = smoking, by = education)
  degraded <- out
  attr(degraded, "display_df") <- NULL
  expect_no_error(txt <- capture.output(print(degraded)))
  expect_true(any(grepl("Chi2", txt, fixed = TRUE)))
})

test_that("an empty display cell does not break the width decision", {
  # The width decision measures printed width, so a missing cell is two
  # columns wide, not an NA that turns `if (width > console)` into an
  # error.
  out <- table_categorical(
    sochealth,
    select = smoking,
    by = education,
    align = "right"
  )
  dd <- attr(out, "display_df")
  dd[[2L]][1L] <- NA_character_
  attr(out, "display_df") <- dd
  expect_no_error(capture.output(print(out)))
})

test_that("glance() names the measure column from the typed view", {
  out <- table_categorical(sochealth, select = smoking, by = education)
  expect_identical(broom::glance(out)$assoc_type, "Cramer's V")
  # Stripped of its typed view, the exclusion rule answers the same.
  bare <- out
  attr(bare, "structured") <- NULL
  expect_identical(broom::glance(bare)$assoc_type, "Cramer's V")
  # A table with no measure has no assoc column to find on either route.
  none <- table_categorical(
    sochealth,
    select = smoking,
    by = education,
    assoc_measure = "none"
  )
  expect_true(is.na(broom::glance(none)$assoc_type))
})

test_that("glance() picks up assoc CIs when assoc_ci = TRUE", {
  out <- table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    assoc_ci = TRUE
  )
  gl <- broom::glance(out)
  expect_true(is.finite(gl$assoc_ci_lower))
  expect_true(is.finite(gl$assoc_ci_upper))
  expect_true(gl$assoc_ci_lower <= gl$assoc_value)
  expect_true(gl$assoc_ci_upper >= gl$assoc_value)
})

test_that("glance() reports no interval when assoc_ci = FALSE", {
  # The attached long data carries the bounds whatever `assoc_ci` says,
  # and glance() used to publish them for a table that showed none.
  skip_if_not_installed("broom")
  off <- broom::glance(table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    assoc_ci = FALSE
  ))
  on <- broom::glance(table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    assoc_ci = TRUE
  ))
  expect_true(is.finite(off$assoc_value))
  expect_true(is.na(off$assoc_ci_lower))
  expect_true(is.na(off$assoc_ci_upper))
  expect_true(is.finite(on$assoc_ci_lower))
  expect_true(is.finite(on$assoc_ci_upper))
  expect_identical(off$assoc_value, on$assoc_value)
})

# ---- audit fixes (n_total / Total filtering / p_digits threshold) --------

test_that("glance() n_total excludes the synthetic 'Total' group", {
  # smoking x sex with include_total = TRUE (default) should not
  # double-count: the underlying analytic sample is the count of
  # observations with non-NA smoking (1175 in sochealth), NOT
  # 2 * 1175 (which is what summing across Female + Male + Total
  # would give).
  out <- table_categorical(sochealth, select = smoking, by = sex)
  gl <- broom::glance(out)
  # Displayed sample under the drop_na = FALSE default: 1200 (incl. the
  # "(Missing)" level) -- and NOT 2 * 1200, which is what summing across
  # Female + Male + Total would give.
  expect_equal(gl$n_total, nrow(sochealth))
  out_cc <- table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    drop_na = TRUE
  )
  expect_equal(broom::glance(out_cc)$n_total, sum(!is.na(sochealth$smoking)))

  # Triple group setting with iris: 150 observations, three Species
  # plus a Total marginal -> n_total must remain 150, not 4 * 50.
  iris2 <- iris
  iris2$pet_size <- factor(iris2$Petal.Length > 4, labels = c("small", "large"))
  out_iris <- table_categorical(iris2, select = pet_size, by = Species)
  gl_iris <- broom::glance(out_iris)
  expect_equal(gl_iris$n_total, 150L)
})

test_that("glance() n_total stays correct when include_total = FALSE", {
  out <- table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    include_total = FALSE
  )
  gl <- broom::glance(out)
  expect_equal(gl$n_total, nrow(sochealth))
  out_cc <- table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    include_total = FALSE,
    drop_na = TRUE
  )
  expect_equal(broom::glance(out_cc)$n_total, sum(!is.na(sochealth$smoking)))
})

test_that("tidy() drops the synthetic 'Total' group", {
  out <- table_categorical(sochealth, select = smoking, by = sex)
  td <- broom::tidy(out)
  expect_false("Total" %in% td$group)
  expect_setequal(unique(td$group), c("Female", "Male"))
})

test_that("tidy() respects include_total = FALSE without spurious Total rows", {
  out <- table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    include_total = FALSE
  )
  td <- broom::tidy(out)
  # Positive control: "Total" IS a group of the long view when the
  # margin is kept, so this negative cannot go quiet the day the margin
  # label is renamed.
  expect_true(
    "Total" %in%
      table_categorical(
        sochealth,
        select = smoking,
        by = sex,
        output = "long"
      )$group
  )
  expect_false("Total" %in% td$group)
  expect_setequal(unique(td$group), c("Female", "Male"))
})

test_that("p_digits drives the small-p threshold in table_categorical()", {
  # With a strong association, the chi-squared p-value falls well
  # below 1e-4. p_digits = 4 -> the rendered p column should show
  # `<.0001`, not `<.001` (which would be the legacy hardcoded
  # threshold). Use the wide raw `data.frame` output and inspect the
  # rendered display via the same code path the printed and gt
  # outputs use.
  out_default <- table_categorical(
    sochealth,
    select = smoking,
    by = education
  )
  out_p4 <- table_categorical(
    sochealth,
    select = smoking,
    by = education,
    p_digits = 4
  )
  # Both objects expose `display_df` as an attribute; the `p` column
  # is rendered in the report-wide form.
  disp_default <- attr(out_default, "display_df")
  disp_p4 <- attr(out_p4, "display_df")
  p_default <- disp_default[["p"]][nzchar(disp_default[["p"]])]
  p_p4 <- disp_p4[["p"]][nzchar(disp_p4[["p"]])]
  # Default: any small p prints as `<.001`
  expect_true(any(grepl("^<\\.001$", p_default)))
  # p_digits = 4: same small p prints as `<.0001`
  expect_true(any(grepl("^<\\.0001$", p_p4)))
})

test_that("p_digits = 4 respects decimal_mark = ','", {
  out <- table_categorical(
    sochealth,
    select = smoking,
    by = education,
    p_digits = 4,
    decimal_mark = ","
  )
  disp <- attr(out, "display_df")
  p_col <- disp[["p"]][nzchar(disp[["p"]])]
  expect_true(any(grepl("^<0,0001$", p_col)))
})

test_that("table_categorical does not over-truncate p in (10^-p_digits, 0.001)", {
  # Regression: parse_stats() previously hardcoded `p_op = "<"` for any
  # p < 0.001, which caused fmt_p() to render "<.0001" for a true p
  # like 0.000108 even though that value is *greater* than the
  # p_digits = 4 threshold. The correct rendering is the rounded
  # numeric form (".0001"), reserving "<.0001" for p < 1e-4.
  set.seed(1)
  n <- 60
  df <- tibble::tibble(
    x = factor(c(rep("A", n), rep("B", n))),
    y = factor(c(rep("yes", 50), rep("no", 10), rep("yes", 30), rep("no", 30)))
  )
  ct <- cross_tab(df, x, y)
  p <- attr(ct, "p_value")
  expect_true(p > 1e-4 && p < 1e-3) # sanity: p sits in the bug zone
  out <- table_categorical(df, select = y, by = x, p_digits = 4)
  disp <- attr(out, "display_df")
  p_col <- disp[["p"]][nzchar(disp[["p"]])]
  # The rendered p must NOT be "<.0001" -- the true p > 1e-4.
  expect_false(any(grepl("^<\\.0001$", p_col)))
  # It should be the four-decimal rounded form, e.g. ".0001".
  expect_true(any(grepl("^\\.0001$", p_col)))
})

# ---- labels: column names by default, the attribute on request ------------

test_that("labels rejects the legacy positional character vector", {
  err <- tryCatch(
    table_categorical(
      sochealth,
      select = c(smoking, physical_activity),
      labels = c("Current smoker", "Physical activity"),
      output = "long"
    ),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    paste(conditionMessage(err), collapse = "\n"),
    "a named character vector"
  )
})

test_that("labels rejects partially named vectors", {
  expect_error(
    table_categorical(
      sochealth,
      select = c(smoking, physical_activity),
      labels = c(smoking = "Current smoker", "Physical activity"),
      output = "long"
    ),
    class = "spicy_invalid_input"
  )
})

test_that("labels: names by default, the label attribute under TRUE", {
  df <- data.frame(
    smk = structure(
      c("No", "Yes", "No", "Yes"),
      label = "Current smoker"
    ),
    act = c("Low", "High", "Low", "High"),
    na_lab = structure(c("a", "b", "a", "b"), label = NA_character_),
    empty = structure(c("u", "v", "u", "v"), label = ""),
    stringsAsFactors = FALSE
  )
  rows <- function(...) {
    out <- table_categorical(
      df,
      select = c(smk, act, na_lab, empty),
      output = "long",
      ...
    )
    as.character(unique(out$variable))
  }
  expect_identical(rows(), c("smk", "act", "na_lab", "empty"))
  # No attribute, an NA one and an empty one fall back to the name.
  expect_identical(
    rows(labels = TRUE),
    c("Current smoker", "act", "na_lab", "empty")
  )
  # A named vector sets the columns it names; the others follow the
  # option (names when unset); a name matching no column is ignored.
  expect_identical(
    rows(labels = c(smk = "Override", bogus = "X")),
    c("Override", "act", "na_lab", "empty")
  )
  withr::local_options(spicy.var_labels = TRUE)
  expect_identical(rows(), c("Current smoker", "act", "na_lab", "empty"))
  expect_identical(
    rows(labels = c(act = "Activity")),
    c("Current smoker", "Activity", "na_lab", "empty")
  )
  expect_identical(rows(labels = FALSE), c("smk", "act", "na_lab", "empty"))
})

test_that("labels refuses every other form with one classed error", {
  bad <- list(NULL, NA, c(TRUE, FALSE), 123, list(smoking = "Smoker"), mean)
  for (b in bad) {
    expect_error(
      table_categorical(sochealth, select = smoking, labels = b),
      class = "spicy_invalid_input"
    )
  }
  withr::local_options(spicy.var_labels = "yes")
  err <- expect_error(
    table_categorical(sochealth, select = smoking),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "spicy.var_labels", fixed = TRUE)
  expect_error(
    table_categorical(sochealth, select = smoking, labels = c(smoking = "S")),
    class = "spicy_invalid_input"
  )
  # TRUE and FALSE typed in the call win over the option.
  out <- table_categorical(
    sochealth,
    select = smoking,
    labels = TRUE,
    output = "long"
  )
  expect_identical(as.character(unique(out$variable)), "Current smoker")
})

test_that("labels accepts a named character vector keyed by column name", {
  out <- table_categorical(
    sochealth,
    select = c(smoking, physical_activity),
    labels = c(
      smoking = "Current smoker",
      physical_activity = "Physical activity"
    ),
    output = "long"
  )
  expect_setequal(
    unique(out$variable),
    c("Current smoker", "Physical activity")
  )
})

test_that("named labels relabel a subset; the others keep their names", {
  out <- table_categorical(
    sochealth,
    select = c(smoking, physical_activity),
    labels = c(smoking = "CS only"),
    output = "long"
  )
  expect_setequal(unique(out$variable), c("CS only", "physical_activity"))
})

test_that("named labels with unknown names are ignored", {
  out <- table_categorical(
    sochealth,
    select = smoking,
    labels = c(bogus = "X"),
    output = "long"
  )
  expect_identical(as.character(unique(out$variable)), "smoking")
})

test_that("non-character labels rejected at boundary", {
  expect_error(
    table_categorical(sochealth, select = smoking, labels = 123),
    class = "spicy_invalid_input"
  )
})

test_that("the by title and the missing-values notes follow labels", {
  d <- sochealth
  d$sex_na <- d$sex
  d$sex_na[1:40] <- NA
  build <- function(...) {
    table_categorical(d, select = smoking, by = sex_na, drop_na = TRUE, ...)
  }
  check <- function(out, by, row) {
    expect_identical(attr(out, "group_label"), by)
    expect_identical(
      attr(out, "missing_note"),
      sprintf(
        "Missing values removed: %s (25). Rows with missing %s removed: 40.",
        row,
        by
      )
    )
  }
  check(build(), "sex_na", "smoking")
  check(build(labels = TRUE), "Sex", "Current smoker")
  check(
    build(labels = c(sex_na = "Gender", smoking = "Smoker")),
    "Gender",
    "Smoker"
  )
  withr::local_options(spicy.var_labels = TRUE)
  check(build(labels = c(smoking = "Smoker")), "Sex", "Smoker")
  check(build(labels = FALSE), "sex_na", "smoking")
})

test_that("var_labels() renames on the attribute base, whatever the option", {
  build <- function() {
    table_categorical(
      sochealth,
      select = c(smoking, physical_activity),
      by = sex,
      labels = var_labels(smoking = "Fumeur")
    )
  }
  check <- function(out) {
    expect_identical(attr(out, "group_label"), "Sex")
    expect_identical(
      unique(as.data.frame(out)$Variable),
      c("Fumeur", "Regular physical activity")
    )
  }
  check(build())
  withr::local_options(spicy.var_labels = FALSE)
  check(build())
})

# ---- select: optional, defaults to eligible categorical columns -----------

test_that("select-less call tabulates every eligible categorical column", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B")),
    fct = factor(c("x", "y", "x", "y")),
    chr = c("u", "v", "u", "v"),
    lgl = c(TRUE, FALSE, TRUE, FALSE),
    num = c(1.5, 2.5, 3.5, 4.5),
    stringsAsFactors = FALSE
  )
  out <- table_categorical(df, output = "long")
  # factor / character / logical columns are in; numeric is out.
  expect_setequal(unique(out$variable), c("grp", "fct", "chr", "lgl"))

  # With `by`, the grouping column is excluded from the rows.
  out_by <- table_categorical(df, by = grp, output = "long")
  expect_setequal(unique(out_by$variable), c("fct", "chr", "lgl"))
})

test_that("select-less call keeps labelled (haven-style) columns", {
  df <- data.frame(x = 1:4)
  df$lab <- labelled::labelled(
    c(1, 2, 1, 2),
    labels = c(Low = 1, High = 2)
  )
  out <- table_categorical(df, output = "long")
  expect_identical(unique(out$variable), "lab")
})

test_that("select-less call with no eligible column warns spicy_no_selection", {
  df <- data.frame(a = 1:4, b = rnorm(4))
  expect_warning(
    res <- table_categorical(df),
    class = "spicy_no_selection"
  )
  expect_identical(suppressWarnings(table_categorical(df)), data.frame())
  expect_identical(res, data.frame())
})

test_that("explicit select is taken verbatim, numeric columns included", {
  df <- data.frame(num = c(1, 2, 1, 2))
  out <- table_categorical(df, select = num, output = "long")
  expect_setequal(unique(out$level), c("1", "2"))
})


# ---- drop_na default flip + disclosure (0.13.0, EpiRHandbook batch) -------

test_that("default drop_na = FALSE shows the (Missing) level", {
  out <- table_categorical(sochealth, select = income_group)
  df <- as.data.frame(attr(out, "display_df"))
  expect_true(any(grepl("(Missing)", df$Variable, fixed = TRUE)))
  # No disclosure note under the default: nothing was removed.
  expect_null(attr(out, "missing_note"))
})

test_that("drop_na = TRUE discloses removals in the missing_note", {
  out <- table_categorical(
    sochealth,
    select = c(income_group, smoking),
    labels = TRUE,
    drop_na = TRUE
  )
  note <- attr(out, "missing_note")
  # Each variable is named by its row label (here its `label` attribute).
  expect_identical(
    note,
    "Missing values removed: Household income group (18), Current smoker (25)."
  )
  printed <- paste(
    capture.output(print(out)),
    collapse = "
"
  )
  expect_match(
    printed,
    "Missing values removed: Household income group (18)",
    fixed = TRUE
  )
  relabelled <- table_categorical(
    sochealth,
    select = smoking,
    labels = c(smoking = "Smoker"),
    drop_na = TRUE
  )
  expect_identical(
    attr(relabelled, "missing_note"),
    "Missing values removed: Smoker (25)."
  )
})

test_that("the by title names the grouping variable by its label", {
  title_has <- function(out, text) {
    any(grepl(text, capture.output(print(out)), fixed = TRUE))
  }
  out <- table_categorical(sochealth, select = smoking, by = sex)
  expect_true(title_has(out, "Categorical table by sex"))
  out <- table_categorical(sochealth, select = smoking, by = sex, labels = TRUE)
  expect_true(title_has(out, "Categorical table by Sex"))
  d <- sochealth
  attr(d$sex, "label") <- NULL
  plain <- table_categorical(d, select = smoking, by = sex, labels = TRUE)
  expect_true(title_has(plain, "Categorical table by sex"))
})

test_that("drop_na = TRUE with by-NAs discloses both removals", {
  d <- sochealth
  d$sex_na <- d$sex
  d$sex_na[1:40] <- NA
  out <- table_categorical(d, select = smoking, by = sex_na, drop_na = TRUE)
  expect_identical(
    attr(out, "missing_note"),
    "Missing values removed: smoking (25). Rows with missing sex_na removed: 40."
  )
})

test_that("association stats ignore the (Missing) display level", {
  # Show the missing, test the observed (gtsummary / SPSS convention):
  # the chi-square / Phi / p must be IDENTICAL whether the missing are
  # displayed (default) or dropped (drop_na = TRUE).
  d <- sochealth
  d$sex_na <- d$sex
  d$sex_na[1:40] <- NA
  gl_show <- broom::glance(
    table_categorical(d, select = smoking, by = sex_na)
  )
  gl_drop <- broom::glance(
    table_categorical(d, select = smoking, by = sex_na, drop_na = TRUE)
  )
  expect_equal(gl_show$statistic, gl_drop$statistic, tolerance = 1e-12)
  expect_equal(gl_show$p.value, gl_drop$p.value, tolerance = 1e-12)
  expect_equal(gl_show$assoc_value, gl_drop$assoc_value, tolerance = 1e-12)
  # And the displayed table nevertheless carries the (Missing) column.
  df <- as.data.frame(table_categorical(d, select = smoking, by = sex_na))
  expect_true(any(grepl("(Missing)", names(df), fixed = TRUE)))
})


test_that("drop_na = TRUE disclosure note names the dropped counts", {
  # The published contract (NEWS 0.13 dev): opting back into silent-drop
  # is DISCLOSED -- a reader can always see what left the table.
  df <- data.frame(
    v1 = factor(c("a", "b", NA, "a", NA)),
    grp = factor(c("A", "B", "A", NA, "B"))
  )
  out <- table_categorical(df, select = v1, drop_na = TRUE, output = "default")
  expect_identical(attr(out, "missing_note"), "Missing values removed: v1 (2).")
  printed <- paste(capture.output(print(out)), collapse = "\n")
  expect_match(printed, "Missing values removed: v1 (2).", fixed = TRUE)

  out_by <- table_categorical(
    df,
    select = v1,
    by = grp,
    drop_na = TRUE,
    output = "default"
  )
  note <- attr(out_by, "missing_note")
  expect_match(note, "Missing values removed: v1 (2).", fixed = TRUE)
  expect_match(note, "Rows with missing grp removed: 1.", fixed = TRUE)

  # Nothing dropped -> no note at all (nothing to disclose).
  df_full <- data.frame(v1 = factor(c("a", "b", "a")))
  out_full <- table_categorical(
    df_full,
    select = v1,
    drop_na = TRUE,
    output = "default"
  )
  expect_null(attr(out_full, "missing_note"))
})

test_that("the note opens with \"Note.\" once, drop_na disclosure first or not", {
  by_sex <- function(...) {
    table_categorical(
      sochealth,
      select = c(smoking, education),
      by = sex,
      ...
    )
  }
  # drop_na = TRUE: the missing-values sentence comes first, and the
  # prefix opens the note there, never between its two parts (issue #11).
  tbl <- by_sex(drop_na = TRUE)
  printed <- utils::capture.output(print(tbl))
  expect_identical(sum(grepl("Note.", printed, fixed = TRUE)), 1L)
  expect_identical(
    printed[grepl("Note.", printed, fixed = TRUE)],
    "Note. Missing values removed: smoking (25)."
  )
  expect_true(
    "Phi: smoking. Cramer's V: education. Group comparison: Pearson chi-squared test." %in%
      printed
  )
  # The two attributes are the parts, without the prefix; the assembled
  # note is what every engine carries.
  expect_identical(
    attr(tbl, "missing_note"),
    "Missing values removed: smoking (25)."
  )
  expect_false(grepl("Note.", attr(tbl, "assoc_note"), fixed = TRUE))
  skip_if_not_installed("gt")
  expect_identical(
    attr(by_sex(drop_na = TRUE, output = "gt"), "spicy_note"),
    paste0(
      "Note. Missing values removed: smoking (25).\n",
      "Phi: smoking. Cramer's V: education. Group comparison: Pearson chi-squared test."
    )
  )
  # Without drop_na, the statistics part opens the note.
  printed <- utils::capture.output(print(by_sex()))
  expect_true(
    "Note. Phi: smoking. Cramer's V: education. Group comparison: Pearson chi-squared test." %in%
      printed
  )
  # One-way, drop_na alone; nothing to say, no note.
  printed <- utils::capture.output(print(
    table_categorical(sochealth, select = smoking, drop_na = TRUE)
  ))
  expect_identical(sum(grepl("Note.", printed, fixed = TRUE)), 1L)
  expect_true("Note. Missing values removed: smoking (25)." %in% printed)
  expect_false(any(grepl(
    "Note.",
    utils::capture.output(print(table_categorical(
      sochealth,
      select = smoking
    ))),
    fixed = TRUE
  )))
  withr::local_options(spicy.language = "fr")
  printed <- utils::capture.output(print(by_sex(drop_na = TRUE)))
  expect_identical(sum(grepl("Note.", printed, fixed = TRUE)), 1L)
  expect_true(
    "Note. Valeurs manquantes retir\u00e9es\u00a0: smoking (25)." %in% printed
  )
})

# ---- audit Phase 2: level order, labels, margins, full precision ----------

test_that("ordinal measures respect the declared order under default drop_na", {
  # PSPP 2.0 oracle (CROSSTABS /STATISTICS=BTAU on education x
  # self_rated_health): tau-b = .2045524, chi2 = 73.2444141. The
  # pre-fix as.character() round-trip re-sorted both ordered factors
  # alphabetically and returned 0.0200 for the same table.
  out <- table_categorical(
    sochealth,
    select = education,
    by = self_rated_health,
    output = "long"
  )
  expect_equal(unique(out$effect_size), 0.2045524108, tolerance = 1e-9)
  expect_identical(unique(out$effect_size_type), "tau_b")
  expect_equal(unique(out$chi2), 73.2444140723, tolerance = 1e-9)
})

test_that("ordinal measures are drop_na-invariant on NA-free ordered data", {
  # Ordinal order != alphabetical order in BOTH variables; with no NA
  # anywhere, drop_na = FALSE and drop_na = TRUE tabulate the same
  # cells, so all four order-sensitive measures must agree exactly.
  d <- data.frame(
    xo = factor(
      rep(c("Low", "Low", "Mid", "Mid", "High", "High"), 5),
      levels = c("Low", "Mid", "High"),
      ordered = TRUE
    ),
    yo = factor(
      rep(c("Worse", "Same", "Same", "Better", "Better", "Better"), 5),
      levels = c("Worse", "Same", "Better"),
      ordered = TRUE
    )
  )
  for (m in c("tau_b", "tau_c", "gamma", "somers_d")) {
    l_show <- table_categorical(
      d,
      select = xo,
      by = yo,
      assoc_measure = m,
      output = "long"
    )
    l_drop <- table_categorical(
      d,
      select = xo,
      by = yo,
      assoc_measure = m,
      drop_na = TRUE,
      output = "long"
    )
    expect_identical(unique(l_show$effect_size), unique(l_drop$effect_size))
    expect_identical(unique(l_show$effect_size_type), m)
  }
  # And the auto tau-b matches the integer-rank oracle.
  l_auto <- table_categorical(d, select = xo, by = yo, output = "long")
  expect_equal(
    unique(l_auto$effect_size),
    stats::cor(as.integer(d$xo), as.integer(d$yo), method = "kendall"),
    tolerance = 1e-12
  )
})

test_that("wide data.frame output carries Chi2 and df matching long/glance", {
  wd <- table_categorical(
    sochealth,
    select = smoking,
    by = education,
    output = "data.frame"
  )
  lg <- table_categorical(
    sochealth,
    select = smoking,
    by = education,
    output = "long"
  )
  expect_true(all(c("Chi2", "df") %in% names(wd)))
  expect_equal(unique(wd$Chi2), unique(lg$chi2))
  expect_equal(unique(wd$df), unique(lg$df))
  # Complete-case oracle: chisq.test on the observed 2x3 table.
  oracle <- suppressWarnings(stats::chisq.test(
    table(sochealth$smoking, sochealth$education),
    correct = FALSE
  ))
  expect_equal(unique(wd$Chi2), as.numeric(oracle$statistic), tolerance = 1e-9)
  expect_equal(unique(wd$df), as.numeric(oracle$parameter))
})

test_that("ignored `correct` warns once, with the tested-table dimensions", {
  # smoking has NAs, so the internal display passes see a 3x3 table
  # with the (Missing) column; the warning must fire once, from the
  # complete-case stats pass, with the 3x2 dimensions actually tested.
  cnds <- list()
  withCallingHandlers(
    invisible(table_categorical(
      sochealth,
      select = education,
      by = smoking,
      correct = TRUE,
      output = "data.frame"
    )),
    spicy_ignored_arg = function(w) {
      cnds[[length(cnds) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(cnds, 1L)
  expect_s3_class(cnds[[1L]], "spicy_ignored_arg")
  expect_match(conditionMessage(cnds[[1L]]), "3x2", fixed = TRUE)
})

test_that("weighted grouped table is exact in machine outputs, coherent on display", {
  d <- data.frame(
    x = factor(c("A", "A", "B", "C", NA, NA, "C", "A")),
    g = factor(c("g1", "g1", "g2", "g2", "g1", NA, "g2", "g1")),
    w = c(1.5, 2, 0.5, 1, 2, 1, 3, 0.25)
  )
  # `test = "chisq"`: the point is the weighted counts, and the exact test
  # has no counts to condition on in this fractional table.
  lg <- table_categorical(
    d,
    select = x,
    by = g,
    weights = w,
    test = "chisq",
    output = "long"
  )
  # Exact weighted counts (oracle xtabs), Total included.
  a <- lg[lg$level == "A", ]
  expect_identical(a$n[a$group == "g1"], 3.75)
  expect_identical(a$n[a$group == "Total"], 3.75)
  expect_equal(
    a$pct[a$group == "g1"],
    3.75 / 5.75 * 100,
    tolerance = 1e-12
  )
  b <- lg[lg$level == "B", ]
  expect_identical(b$n[b$group == "g2"], 0.5)
  # Display: the SPSS convention -- integers everywhere, cells and
  # Total from the same fmt_n, so displayed rows sum.
  out <- table_categorical(d, select = x, by = g, weights = w, test = "chisq")
  disp <- attr(out, "display_df")
  ra <- disp[trimws(disp$Variable) == "A", ]
  expect_identical(ra[["g1 n"]], "4")
  expect_identical(ra[["Total n"]], "4")
  rb <- disp[trimws(disp$Variable) == "B", ]
  expect_identical(rb[["g2 n"]], "0")
  expect_identical(rb[["Total n"]], "0")
})

test_that("weighted real-data counts match the xtabs oracle exactly", {
  lg <- table_categorical(
    sochealth,
    select = education,
    by = sex,
    weights = weight,
    output = "long"
  )
  oracle <- stats::xtabs(weight ~ education + sex, data = sochealth)
  low_f <- lg$n[lg$level == "Lower secondary" & lg$group == "Female"]
  low_m <- lg$n[lg$level == "Lower secondary" & lg$group == "Male"]
  low_t <- lg$n[lg$level == "Lower secondary" & lg$group == "Total"]
  expect_equal(
    low_f,
    oracle["Lower secondary", "Female"],
    tolerance = 1e-9,
    ignore_attr = TRUE
  )
  expect_equal(
    low_m,
    oracle["Lower secondary", "Male"],
    tolerance = 1e-9,
    ignore_attr = TRUE
  )
  expect_equal(low_t, low_f + low_m, tolerance = 1e-12)
  # Full-precision column percent (oracle prop.table), not 1-decimal.
  pct_f <- lg$pct[lg$level == "Lower secondary" & lg$group == "Female"]
  expect_equal(
    pct_f,
    100 * prop.table(oracle, 2)["Lower secondary", "Female"],
    tolerance = 1e-9,
    ignore_attr = TRUE
  )
})

test_that("labelled columns display value labels under the drop_na default", {
  skip_if_not_installed("haven")
  d <- data.frame(id = 1:10)
  d$xl <- haven::labelled_spss(
    c(1, 2, 1, 3, 9, 9, 2, 1, NA, 8),
    labels = c(Agree = 1, Neutral = 2, Disagree = 3, DK = 8, Refusal = 9),
    na_values = c(8, 9)
  )
  lg <- table_categorical(d, select = xl, output = "long")
  expect_identical(
    lg$level,
    c("[1] Agree", "[2] Neutral", "[3] Disagree", "(Missing)")
  )
  expect_identical(lg$n, c(3, 2, 1, 4))
  # Same rendering with a grouping variable.
  d$g <- factor(rep(c("a", "b"), 5))
  lg2 <- table_categorical(d, select = xl, by = g, output = "long")
  expect_identical(
    unique(lg2$level),
    c("[1] Agree", "[2] Neutral", "[3] Disagree", "(Missing)")
  )
})

test_that("a by-level literally named 'Total' keeps both group and margin", {
  d1 <- data.frame(
    x = factor(c("A", "B", "A", "B", "A")),
    g = factor(c("Total", "Total", "g2", "g2", "Total"))
  )
  cnds <- list()
  lg <- withCallingHandlers(
    table_categorical(d1, select = x, by = g, output = "long"),
    spicy_renamed_column = function(w) {
      cnds[[length(cnds) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(cnds, 1L)
  # The user's "Total" group survives with its own counts...
  expect_identical(lg$n[lg$level == "A" & lg$group == "Total"], 2)
  expect_identical(lg$n[lg$level == "B" & lg$group == "Total"], 1)
  # ...and the true margin is always present, under the renamed key.
  expect_identical(lg$n[lg$level == "A" & lg$group == "Total_1"], 3)
  expect_identical(lg$n[lg$level == "B" & lg$group == "Total_1"], 2)
  wd <- suppressWarnings(
    table_categorical(d1, select = x, by = g, output = "data.frame")
  )
  expect_true(all(c("Total n", "Total_1 n") %in% names(wd)))
  expect_identical(wd[["Total_1 n"]], c(3, 2))
})

test_that("a declared-but-unobserved 'Total' by level keeps a real margin", {
  # Audit phase 2 delta, R7: cross_tab() renames its margin only on an
  # OBSERVED collision, so with a declared-only "Total" level the last
  # column is literally named "Total" and the name-based group read
  # used to hand the margin counts to the user's zero-observation
  # group -- the margin appeared twice, once under each key.
  d1 <- data.frame(
    x = factor(c("A", "B", "A", "B", "A")),
    g = factor(c("g1", "g1", "g2", "g2", "g1"), levels = c("g1", "g2", "Total"))
  )
  cnds <- list()
  lg <- withCallingHandlers(
    table_categorical(d1, select = x, by = g, output = "long"),
    spicy_renamed_column = function(w) {
      cnds[[length(cnds) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(cnds, 1L)
  # The declared-but-unobserved user group is a zero group...
  expect_identical(lg$n[lg$level == "A" & lg$group == "Total"], 0)
  expect_identical(lg$n[lg$level == "B" & lg$group == "Total"], 0)
  # ...and the true margin lives under the renamed key only.
  expect_identical(lg$n[lg$level == "A" & lg$group == "Total_1"], 3)
  expect_identical(lg$n[lg$level == "B" & lg$group == "Total_1"], 2)
  wd <- suppressWarnings(
    table_categorical(d1, select = x, by = g, output = "data.frame")
  )
  expect_true(all(c("Total n", "Total_1 n") %in% names(wd)))
  expect_identical(wd[["Total n"]], c(0, 0))
  expect_identical(wd[["Total_1 n"]], c(3, 2))
  # No margin displayed -> no rename to disclose, zero group intact.
  cnds2 <- list()
  lg2 <- withCallingHandlers(
    table_categorical(
      d1,
      select = x,
      by = g,
      include_total = FALSE,
      output = "long"
    ),
    spicy_renamed_column = function(w) {
      cnds2[[length(cnds2) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(cnds2, 0L)
  expect_setequal(unique(lg2$group), c("g1", "g2", "Total"))
  expect_identical(lg2$n[lg2$level == "A" & lg2$group == "Total"], 0)
})

test_that("include_total = FALSE keeps a user group named 'Total'", {
  d1 <- data.frame(
    x = factor(c("A", "B", "A", "B", "A")),
    g = factor(c("Total", "Total", "g2", "g2", "Total"))
  )
  cnds <- list()
  lg <- withCallingHandlers(
    table_categorical(
      d1,
      select = x,
      by = g,
      include_total = FALSE,
      output = "long"
    ),
    spicy_renamed_column = function(w) {
      cnds[[length(cnds) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  # No margin displayed -> no rename to disclose.
  expect_length(cnds, 0L)
  expect_setequal(unique(lg$group), c("g2", "Total"))
  expect_identical(lg$n[lg$level == "A" & lg$group == "Total"], 2)
})

test_that("tidy() drops the margin, not a user group named 'Total'", {
  d1 <- data.frame(
    x = factor(c("A", "B", "A", "B", "A")),
    g = factor(c("Total", "Total", "g2", "g2", "Total"))
  )
  suppressWarnings(capture.output(
    out <- table_categorical(d1, select = x, by = g)
  ))
  td <- broom::tidy(out)
  expect_setequal(unique(td$group), c("g2", "Total"))
  expect_identical(sum(td$n), 5L)
  gl <- broom::glance(out)
  expect_identical(gl$n_total, 5L)
})

test_that("percent_digits >= 2 renders true decimals with by", {
  d <- data.frame(
    x = factor(c("A", "A", "B", "C", NA, NA, "C", "A")),
    g = factor(c("g1", "g1", "g2", "g2", "g1", NA, "g2", "g1"))
  )
  out <- table_categorical(d, select = x, by = g, percent_digits = 2)
  disp <- attr(out, "display_df")
  rb <- disp[trimws(disp$Variable) == "B", ]
  rc <- disp[trimws(disp$Variable) == "C", ]
  expect_identical(rb[["g2 %"]], "33.33")
  expect_identical(rc[["g2 %"]], "66.67")
})

test_that("a declared-but-unobserved by level is a zero column in wide and long", {
  # Audit phase 2, finding 29: the wide output used to carry NA cells
  # under the empty group's columns while the long output omitted the
  # group entirely.
  d <- data.frame(
    x = factor(c("a", "b", "a", "a", "b", "b", "a", "b", "a", "b")),
    g = factor(rep(c("m", "f"), 5), levels = c("m", "f", "ghost"))
  )
  wide <- table_categorical(d, select = x, by = g, output = "data.frame")
  expect_equal(wide[["ghost n"]], c(0, 0))
  expect_equal(wide[["ghost %"]], c(0, 0))
  long <- table_categorical(d, select = x, by = g, output = "long")
  ghost <- long[long$group == "ghost", ]
  expect_equal(nrow(ghost), 2L)
  expect_equal(ghost$n, c(0, 0))
  expect_equal(ghost$pct, c(0, 0))
  # Observed cells and the margin are unchanged (oracle table()).
  expect_equal(
    long$n[long$group == "m" & long$level == "a"],
    unname(table(d$x, d$g)["a", "m"])
  )
  expect_equal(long$n[long$group == "Total" & long$level == "a"], 5)
  # Same contract under drop_na = TRUE.
  wide_dn <- table_categorical(
    d,
    select = x,
    by = g,
    drop_na = TRUE,
    output = "data.frame"
  )
  expect_equal(wide_dn[["ghost n"]], c(0, 0))
})

test_that("user_na = FALSE surfaces phi's hard error instead of a silent NA", {
  # Audit phase 2, finding 31: with declared na_values kept as valid
  # codes, the table is 3x2; phi's documented spicy_unsupported error
  # used to be swallowed into an all-NA column.
  skip_if_not_installed("haven")
  d <- data.frame(g = factor(rep(c("u", "v"), 5)))
  d$xs <- haven::labelled_spss(
    c(1, 2, 1, 9, 1, 2, 2, 1, 9, 2),
    labels = c(Low = 1, High = 2, Refused = 9),
    na_values = 9
  )
  expect_error(
    table_categorical(
      d,
      select = xs,
      by = g,
      user_na = FALSE,
      assoc_measure = "phi",
      output = "long"
    ),
    class = "spicy_unsupported"
  )
  # auto now dispatches on the levels the table actually has (3x2 ->
  # Cramer's V), instead of choosing phi from the 2 non-declared codes.
  auto <- table_categorical(
    d,
    select = xs,
    by = g,
    user_na = FALSE,
    output = "long"
  )
  expect_identical(unique(auto$effect_size_type), "cramer_v")
  expect_false(any(is.na(auto$effect_size)))
  # With user_na = TRUE the table is a true 2x2: phi works and is
  # exact against the chi-squared oracle.
  ok <- table_categorical(
    d,
    select = xs,
    by = g,
    assoc_measure = "phi",
    output = "long"
  )
  codes <- as.numeric(unclass(d$xs))
  keep <- codes != 9
  chi <- suppressWarnings(
    stats::chisq.test(
      table(codes[keep], as.character(d$g)[keep]),
      correct = FALSE
    )
  )
  expect_identical(unique(ok$effect_size_type), "phi")
  expect_equal(
    ok$effect_size[1],
    unname(sqrt(chi$statistic / sum(keep))),
    tolerance = 1e-10
  )
})

test_that("table_categorical() rejects bit64::integer64 columns", {
  # Manually classed vector: inherits() is all the guard needs, and
  # this is the shape a bare integer64 column has when bit64 is not
  # loaded (raw int64 bit patterns in a double payload).
  i64 <- structure(
    c(4.94e-324, 9.88e-324, 1.48e-323, 1.98e-323),
    class = "integer64"
  )
  d <- data.frame(g = factor(c("a", "b", "a", "b")))
  d$code <- i64
  expect_error(
    table_categorical(d, select = code),
    "integer64",
    class = "spicy_invalid_data"
  )
  expect_error(
    table_categorical(d, select = g, by = code),
    class = "spicy_invalid_data"
  )
  expect_error(
    table_categorical(d, select = g, weights = code),
    class = "spicy_invalid_data"
  )
  # The select-less default excludes integer64 columns via the
  # categorical-eligibility filter, so the call still works.
  res <- table_categorical(d, output = "data.frame")
  expect_s3_class(res, "data.frame")
  expect_false(any(grepl("code", res$Variable, fixed = TRUE)))
})


# Phase 3 matrix – vignettes-news:align-auto-removed and
# critic:pkgrd-broom-columns-stabilising (lot T4)

test_that("align = 'auto' is removed from table_categorical", {
  expect_error(
    table_categorical(mtcars, select = "cyl", align = "auto"),
    class = "spicy_invalid_input"
  )
})

test_that("tidy/glance column sets are frozen (stabilizing contract)", {
  skip_if_not_installed("broom")
  out <- table_categorical(mtcars, select = "cyl", by = "am")
  expect_identical(
    names(broom::tidy(out)),
    c("outcome", "level", "group", "n", "proportion")
  )
  expect_identical(
    names(broom::glance(out)),
    c(
      "outcome",
      "test_type",
      "statistic",
      "df",
      "p.value",
      "assoc_type",
      "assoc_value",
      "assoc_ci_lower",
      "assoc_ci_upper",
      "n_total"
    )
  )
})

test_that("each variable keeps its own level order in multi-variable tables", {
  # dev/bug_missing_order_multivar.md: the global level factor imposed
  # the FIRST variable's ordering on every later block -- "(Missing)"
  # jumped to the head from the second variable on, and homonymous
  # levels were reordered. Ordering is now per variable.
  d <- data.frame(
    a = factor(c("x", "y", NA, "x", "y", "x")),
    s = factor(c("F", "M", "F", NA, NA, "F")),
    e = factor(c("O", "N", NA, "O", "O", "N"))
  )
  out <- table_categorical(
    d,
    select = c(a, s, e),
    drop_na = FALSE,
    output = "data.frame"
  )
  lv <- trimws(out$Level)
  block <- cumsum(nzchar(trimws(out$Variable)))
  for (b in split(seq_along(lv), block)) {
    m <- which(lv[b] == "(Missing)")
    if (length(m)) expect_identical(max(m), length(b))
  }
  # Homonymous levels with opposite declared orders stay per-variable.
  d2 <- data.frame(
    u = factor(c("Oui", "Non", "Oui"), levels = c("Oui", "Non")),
    v = factor(c("Non", "Oui", "Non"), levels = c("Non", "Oui"))
  )
  o2 <- table_categorical(d2, select = c(u, v), output = "data.frame")
  expect_identical(trimws(o2$Level), c("Oui", "Non", "Non", "Oui"))
  # The by path orders identically.
  outb <- table_categorical(
    d,
    select = c(a, s),
    by = e,
    drop_na = FALSE,
    output = "data.frame"
  )
  lvb <- trimws(outb$Level)
  blockb <- cumsum(nzchar(trimws(outb$Variable)))
  for (b in split(seq_along(lvb), blockb)) {
    m <- which(lvb[b] == "(Missing)")
    if (length(m)) expect_identical(max(m), length(b))
  }
})

test_that("spicy tinytables keep tinytable's format finalizers alive", {
  # dev/theme_empty_efface_les_finaliseurs.md: theme_empty() wiped
  # lazy_finalize, killing options(tinytable_typst_multipage) -- and
  # any future format hook -- on every spicy table. .spicy_tt_bare()
  # strips appearance slots only.
  skip_if_not_installed("tinytable")
  op <- options(tinytable_typst_multipage = TRUE)
  on.exit(options(op), add = TRUE)
  x <- table_categorical(sochealth, select = smoking, output = "tinytable")
  expect_match(
    tinytable::save_tt(x, output = "typst"),
    "breakable: true",
    fixed = TRUE
  )
  y <- table_regression(
    lm(wellbeing_score ~ age, sochealth),
    output = "tinytable"
  )
  expect_match(
    tinytable::save_tt(y, output = "typst"),
    "breakable: true",
    fixed = TRUE
  )
})

# ---- the disclosure reaches every output route ---------------------------
# dev/notes_perdues_hors_console.md: the drop_na ledger was built and
# then read by print() alone, so a Quarto report with `warning: false`
# showed a table computed on fewer people than it announced.

test_that("the missing disclosure reaches tinytable, one-way and by", {
  skip_if_not_installed("tinytable")
  d <- sochealth
  d$sex_na <- d$sex
  d$sex_na[1:40] <- NA

  x <- table_categorical(
    d,
    select = c(income_group, smoking),
    drop_na = TRUE,
    output = "tinytable"
  )
  expect_identical(
    paste(unlist(x@notes), collapse = " "),
    "Note. Missing values removed: income_group (18), smoking (25)."
  )
  # And it survives rendering, not just the slot.
  expect_match(
    tinytable::save_tt(x, output = "html"),
    "Missing values removed",
    fixed = TRUE
  )

  x_by <- table_categorical(
    d,
    select = smoking,
    by = sex_na,
    drop_na = TRUE,
    output = "tinytable"
  )
  note <- paste(unlist(x_by@notes), collapse = " ")
  expect_match(
    note,
    "Missing values removed: smoking (25).",
    fixed = TRUE
  )
  expect_match(note, "Rows with missing sex_na removed: 40.", fixed = TRUE)

  # Nothing removed -> no note at all.
  clean <- table_categorical(
    data.frame(v = factor(c("a", "b", "a"))),
    select = v,
    drop_na = TRUE,
    output = "tinytable"
  )
  expect_length(clean@notes, 0L)
})

test_that("the missing disclosure reaches gt, one-way and by", {
  skip_if_not_installed("gt")
  d <- sochealth
  d$sex_na <- d$sex
  d$sex_na[1:40] <- NA

  g <- table_categorical(
    d,
    select = c(income_group, smoking),
    drop_na = TRUE,
    output = "gt"
  )
  expect_s3_class(g, "spicy_gt")
  expect_identical(
    attr(g, "spicy_note"),
    "Note. Missing values removed: income_group (18), smoking (25)."
  )

  g_by <- table_categorical(
    d,
    select = smoking,
    by = sex_na,
    drop_na = TRUE,
    output = "gt"
  )
  expect_match(
    attr(g_by, "spicy_note"),
    "Rows with missing sex_na removed: 40.",
    fixed = TRUE
  )

  # Nothing removed -> the gt object stays untagged.
  clean <- table_categorical(
    data.frame(v = factor(c("a", "b", "a"))),
    select = v,
    drop_na = TRUE,
    output = "gt"
  )
  expect_false(inherits(clean, "spicy_gt"))
  expect_null(attr(clean, "spicy_note"))
})

test_that("the missing disclosure reaches flextable, one-way and by", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("htmltools")
  d <- sochealth
  d$sex_na <- d$sex
  d$sex_na[1:40] <- NA

  f <- table_categorical(
    d,
    select = c(income_group, smoking),
    drop_na = TRUE,
    output = "flextable"
  )
  expect_identical(nrow(f$footer$dataset), 1L)
  expect_match(
    as.character(flextable::htmltools_value(f)),
    "Missing values removed: income_group (18), smoking (25).",
    fixed = TRUE
  )

  f_by <- table_categorical(
    d,
    select = smoking,
    by = sex_na,
    drop_na = TRUE,
    output = "flextable"
  )
  expect_match(
    as.character(flextable::htmltools_value(f_by)),
    "Rows with missing sex_na removed: 40.",
    fixed = TRUE
  )

  # Nothing removed -> no footer row.
  clean <- table_categorical(
    data.frame(v = factor(c("a", "b", "a"))),
    select = v,
    drop_na = TRUE,
    output = "flextable"
  )
  expect_identical(nrow(clean$footer$dataset), 0L)
})

test_that("the missing disclosure rides along on output = 'data.frame'", {
  d <- sochealth
  d$sex_na <- d$sex
  d$sex_na[1:40] <- NA

  out <- table_categorical(
    d,
    select = c(income_group, smoking),
    drop_na = TRUE,
    output = "data.frame"
  )
  expect_identical(
    attr(out, "note"),
    "Note. Missing values removed: income_group (18), smoking (25)."
  )
  expect_null(attr(out, "missing_note"))

  out_by <- table_categorical(
    d,
    select = smoking,
    by = sex_na,
    drop_na = TRUE,
    output = "data.frame"
  )
  expect_match(
    attr(out_by, "note"),
    "Rows with missing sex_na removed: 40.",
    fixed = TRUE
  )

  clean <- table_categorical(
    data.frame(v = factor(c("a", "b", "a"))),
    select = v,
    drop_na = TRUE,
    output = "data.frame"
  )
  expect_null(attr(clean, "note"))
})


test_that("the association CI separator follows the decimal mark", {
  skip_if_not_installed("tinytable")
  d <- transform(mtcars, vs = factor(vs), am = factor(am))
  tt <- table_categorical(
    d,
    vs,
    by = am,
    assoc_measure = "cramer_v",
    assoc_ci = TRUE,
    decimal_mark = ",",
    output = "tinytable"
  )
  cells <- unlist(tt@data)
  merged <- grep("[[]", cells, value = TRUE)
  expect_true(length(merged) >= 1L)
  # Bounds separated by '; ' under a comma mark -- the site used to
  # hardcode ', ', printing the ambiguous `0,45 [0,31, 0,59]`.
  expect_true(any(grepl("; ", merged, fixed = TRUE)))
  expect_false(any(grepl("[0-9], [0-9]", merged)))
})


# ---- gt: the group-column CSS selector escapes its ids -------------------

test_that("a quote in a `by` level no longer breaks the gt render", {
  skip_if_not_installed("gt")
  d <- data.frame(
    sex = factor(rep(c("F", "M"), 20)),
    grp = factor(rep(c('a"b', "plain"), each = 20)),
    stringsAsFactors = FALSE
  )
  g <- table_categorical(d, select = sex, by = grp, output = "gt")
  # The APA intermediate rule is addressed by a `th[id="..."]` attribute
  # selector built from the group columns, which are named after the
  # `by` levels. Unescaped, the quote closed the CSS string and gt's
  # sass compiler aborted the whole render.
  html <- expect_no_error(
    as.character(gt::as_raw_html(g, inline_css = FALSE))
  )
  # The rule reaches the quote-bearing column. The quote never gets as
  # far as the selector any more: `.gt_safe_ids()` takes it out of the
  # gt id before the table is built (register n. 73), so both sides of
  # the match read the sanitized id. The `by` level itself is untouched
  # -- it is the spanner LABEL, which gt escapes.
  expect_true(grepl("th[id=\"a_b_n\"]", html, fixed = TRUE))
  expect_true(grepl('scope="col" id="a_b_n"', html, fixed = TRUE))
  expect_true(grepl(">a\"b<", html, fixed = TRUE))
})

test_that(".css_escape_string leaves an ordinary label untouched", {
  expect_identical(
    spicy:::.css_escape_string(c("plain_n", "Total_pct", "\u00e9t\u00e9_n")),
    c("plain_n", "Total_pct", "\u00e9t\u00e9_n")
  )
  expect_identical(spicy:::.css_escape_string('a"b'), 'a\\"b')
  expect_identical(spicy:::.css_escape_string("a\\b"), "a\\\\b")
  expect_identical(spicy:::.css_escape_string("a\nb"), "a\\00000Ab")
})


# ---- by with no non-missing level -----------------------------------------

test_that("an all-missing `by` is refused instead of growing phantom columns", {
  d <- data.frame(
    x = factor(rep(c("a", "b"), 10)),
    g = rep(NA_character_, 20),
    stringsAsFactors = FALSE
  )
  err <- tryCatch(
    table_categorical(
      d,
      select = x,
      by = g,
      drop_na = TRUE,
      include_total = FALSE
    ),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_data")
  msg <- conditionMessage(err)
  expect_match(msg, "`by = g` has no level to tabulate.", fixed = TRUE)
  expect_match(msg, "Every observation is missing it.", fixed = TRUE)
  expect_match(msg, "drop_na = FALSE", fixed = TRUE)
  # Same refusal with the margin on: the margin is not a group.
  expect_error(
    table_categorical(d, select = x, by = g, drop_na = TRUE),
    class = "spicy_invalid_data"
  )
  # A factor with no levels left takes the same route.
  d_f <- d
  d_f$g <- factor(d$g)
  expect_error(
    table_categorical(
      d_f,
      select = x,
      by = g,
      drop_na = TRUE,
      include_total = FALSE
    ),
    class = "spicy_invalid_data"
  )

  # The remedy the message names really works, and the columns it
  # produces are named -- the defect was `paste0(character(0), " n")`
  # returning " n", so the table grew a ' n' / ' %' pair with no rows.
  keep <- suppressWarnings(
    table_categorical(d, select = x, by = g, drop_na = FALSE)
  )
  expect_true(all(nzchar(trimws(names(keep)))))
  expect_identical(nrow(keep), 2L)
})

test_that("the refusal states the reason the DATA gives, not a generic one", {
  # Two shapes reach the same refusal and the message must fit both.
  # Zero rows: nothing is missing `by`, there is simply nothing -- and
  # `drop_na = FALSE` is NOT a remedy here (it raises the same error),
  # so the message must not offer it.
  d0 <- data.frame(
    x = factor(character(0), levels = c("a", "b")),
    g = character(0),
    stringsAsFactors = FALSE
  )
  err0 <- tryCatch(
    table_categorical(d0, select = x, by = g, drop_na = TRUE),
    error = function(e) e
  )
  expect_s3_class(err0, "spicy_invalid_data")
  msg0 <- conditionMessage(err0)
  expect_match(msg0, "The data has no rows.", fixed = TRUE)
  expect_false(grepl("Every observation is missing it.", msg0, fixed = TRUE))
  expect_false(grepl("drop_na = FALSE", msg0, fixed = TRUE))
  # And it really is not a remedy.
  expect_error(
    table_categorical(d0, select = x, by = g, drop_na = FALSE),
    class = "spicy_invalid_data"
  )
})

# ---- p_adjust ---------------------------------------------------------------

test_that("p_adjust adjusts the one test of each variable, together", {
  raw <- table_categorical(mtcars, c(cyl, gear, vs), by = am, output = "long")
  raw_p <- raw$p[!duplicated(raw$variable)]
  for (method in c("holm", "BH", "bonferroni")) {
    adj <- table_categorical(
      mtcars,
      c(cyl, gear, vs),
      by = am,
      p_adjust = method,
      output = "long"
    )
    expect_equal(
      adj$p[!duplicated(adj$variable)],
      stats::p.adjust(raw_p, method = method)
    )
    # Every row of a variable carries its variable's adjusted p-value.
    expect_equal(
      adj$p,
      stats::p.adjust(raw_p, method = method)[match(
        adj$variable,
        unique(adj$variable)
      )]
    )
    expect_identical(adj$p_unadjusted, raw$p)
  }
})

test_that("p_adjust: the displayed p, the object and the note agree", {
  raw <- table_categorical(mtcars, c(cyl, gear, vs), by = am)
  tbl <- table_categorical(mtcars, c(cyl, gear, vs), by = am, p_adjust = "holm")
  raw_p <- raw$p[!duplicated(raw$Variable)]
  adj_p <- stats::p.adjust(raw_p, method = "holm")

  # The display formats the adjusted value, "<.001" included.
  shown <- attr(tbl, "display_df")$p
  expect_identical(
    shown[nzchar(trimws(shown))],
    vapply(adj_p, format_p_value, character(1), decimal_mark = ".")
  )
  # The wide frame keeps the raw value beside the adjusted one.
  expect_identical(tbl$p_unadjusted, raw$p)
  expect_identical(names(tbl)[match("p", names(tbl)) + 1L], "p_unadjusted")

  # The typed view: adjusted cells, raw values, method and family size.
  # The family is every displayed test, and nothing else.
  s <- as_structured(tbl)
  meta <- s$col_meta$p
  tested <- !is.na(s$body$p)
  expect_equal(s$body$p[tested], adj_p)
  expect_equal(meta$p_unadjusted[tested], raw_p)
  expect_true(all(is.na(meta$p_unadjusted[!tested])))
  expect_identical(meta$p_adjust, "holm")
  expect_identical(meta$p_adjust_m, sum(tested))
  expect_identical(meta$p_adjust_m, 3L)

  expect_match(
    attr(tbl, "assoc_note"),
    'P-values adjusted for 3 tests (Holm method).',
    fixed = TRUE
  )
})

test_that("p_adjust finds the raw p-values of labelled variables", {
  raw <- table_categorical(mtcars, c(cyl, gear), by = am)
  tbl <- table_categorical(
    mtcars,
    c(cyl, gear),
    by = am,
    labels = c(cyl = "Cylinders", gear = "Gears"),
    p_adjust = "holm"
  )
  s <- as_structured(tbl)
  tested <- !is.na(s$body$p)
  expect_identical(s$body$.variable[tested], c("cyl", "gear"))
  expect_equal(
    s$col_meta$p$p_unadjusted[tested],
    raw$p[!duplicated(raw$Variable)]
  )
})

test_that("p_adjust = \"none\" leaves the table as it was", {
  a <- table_categorical(mtcars, c(cyl, gear), by = am)
  b <- table_categorical(mtcars, c(cyl, gear), by = am, p_adjust = "none")
  expect_identical(a, b)
  expect_false("p_unadjusted" %in% names(a))
  expect_null(as_structured(a)$col_meta$p$p_adjust)
})

test_that("p_adjust needs `by`, and validates its method", {
  expect_error(
    table_categorical(mtcars, c(cyl, gear), p_adjust = "holm"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical(mtcars, c(cyl, gear), by = am, p_adjust = "tukey"),
    class = "spicy_invalid_input"
  )
  # "none" without `by` is the default, not a request.
  expect_no_error(table_categorical(mtcars, c(cyl, gear), p_adjust = "none"))
})

test_that("p_adjust: the note reaches the gt and flextable engines", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  note <- 'P-values adjusted for 3 tests (Benjamini-Hochberg method).'
  g <- table_categorical(
    mtcars,
    c(cyl, gear, vs),
    by = am,
    p_adjust = "BH",
    output = "gt"
  )
  expect_match(attr(g, "spicy_note"), note, fixed = TRUE)
  # The whole statistics note, as the other routes print it: before,
  # the gt route dropped the measure note of a mixed table.
  expect_match(
    attr(g, "spicy_note"),
    "Cramer's V: cyl, gear. Phi: vs.",
    fixed = TRUE
  )
  ft <- table_categorical(
    mtcars,
    c(cyl, gear, vs),
    by = am,
    p_adjust = "BH",
    output = "flextable"
  )
  # The footer is a paragraph: an italic "Note." chunk, then the rest.
  expect_match(
    paste(ft$footer$content$data[[1L, 1L]]$txt, collapse = ""),
    note,
    fixed = TRUE
  )
})

test_that("p_adjust under holm is pinned, in English and in French", {
  withr::local_options(width = 100)
  expect_snapshot(print(table_categorical(
    mtcars,
    c(cyl, gear, vs),
    by = am,
    p_adjust = "holm"
  )))
  withr::local_options(spicy.language = "fr")
  expect_snapshot(print(table_categorical(
    mtcars,
    c(cyl, gear, vs),
    by = am,
    p_adjust = "holm"
  )))
})

# ---- p_value (issue #9) ------------------------------------------------------

.pv_tbl <- function(...) {
  table_categorical(sochealth, c(smoking, education), by = sex, ...)
}

test_that("p_value defaults to the test with `by`, as in the siblings", {
  ref <- .pv_tbl()
  expect_true(all(c("Chi2", "df", "p") %in% names(ref)))
  expect_identical(.pv_tbl(p_value = TRUE), ref)
  expect_true(all(c("chi2", "df", "p") %in% names(.pv_tbl(output = "long"))))
  expect_true("p" %in% names(as_structured(ref)$col_meta))
})

test_that("p_value = FALSE: the data frames are the default minus the test", {
  ref <- .pv_tbl(output = "data.frame")
  out <- .pv_tbl(p_value = FALSE, output = "data.frame")
  expect_identical(
    names(out),
    setdiff(names(ref), c("test_type", "Chi2", "df", "p"))
  )
  # The note loses the test too.
  expect_identical(
    attr(out, "note"),
    "Note. Phi: smoking. Cramer's V: education."
  )
  expect_identical(structure(out, note = NULL), ref[names(out)])

  ref_l <- .pv_tbl(output = "long")
  out_l <- .pv_tbl(p_value = FALSE, output = "long")
  expect_identical(
    names(out_l),
    setdiff(names(ref_l), c("test_type", "chi2", "df", "p"))
  )
  expect_identical(attr(out_l, "note"), attr(out, "note"))
  expect_identical(structure(out_l, note = NULL), ref_l[names(out_l)])
})

test_that("p_value = FALSE: the console table, its typed view and glance()", {
  ref <- .pv_tbl()
  tbl <- .pv_tbl(p_value = FALSE)
  expect_false(any(c("test_type", "Chi2", "df", "p") %in% names(tbl)))
  expect_false(any(
    c("test_type", "chi2", "df", "p", "p_op") %in%
      names(attr(tbl, "long_data"))
  ))

  disp <- attr(tbl, "display_df")
  expect_identical(disp, attr(ref, "display_df")[names(disp)])
  expect_identical(setdiff(names(attr(ref, "display_df")), names(disp)), "p")

  s <- as_structured(tbl)
  s_ref <- as_structured(ref)
  expect_false("p" %in% names(s$col_meta))
  expect_false("p" %in% names(s$body))
  expect_false("p" %in% vapply(s$col_meta, `[[`, "", "token"))
  expect_identical(s$spanners, s_ref$spanners)
  expect_identical(s$body$.row_role, s_ref$body$.row_role)

  # The measure note is the measure's: it stays. The sentence naming the
  # test leaves with the test.
  measure_note <- "Phi: smoking. Cramer's V: education."
  expect_identical(attr(tbl, "assoc_note"), measure_note)
  expect_identical(
    attr(ref, "assoc_note"),
    paste(measure_note, "Group comparison: Pearson chi-squared test.")
  )
  printed <- utils::capture.output(print(tbl))
  header <- printed[grepl("Variable", printed, fixed = TRUE)][[1L]]
  expect_match(header, "Female n", fixed = TRUE)
  expect_match(header, "Total %", fixed = TRUE)
  expect_false(grepl("\\bp\\b", header))

  gl <- broom::glance(tbl)
  expect_true(all(is.na(gl$test_type)))
  expect_true(all(is.na(gl$statistic)))
  expect_true(all(is.na(gl$p.value)))
  expect_false(anyNA(gl$assoc_value))
})

test_that("p_value = FALSE removes the p column from every rendered engine", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  skip_if_not_installed("openxlsx2")

  tt <- .pv_tbl(p_value = FALSE, output = "tinytable")
  expect_identical(
    ncol(tt@data),
    ncol(.pv_tbl(output = "tinytable")@data) - 1L
  )
  md <- strsplit(tinytable::save_tt(tt, output = "markdown"), "\n")[[1L]]
  top <- md[grepl("Variable", md, fixed = TRUE)][[1L]]
  expect_match(top, "Female", fixed = TRUE)
  expect_match(top, "Total", fixed = TRUE)
  expect_false(grepl("| p ", top, fixed = TRUE))

  g <- .pv_tbl(p_value = FALSE, output = "gt")
  g_ref <- .pv_tbl(output = "gt")
  expect_false("p" %in% names(g[["_data"]]))
  expect_false("spn_p" %in% g[["_spanners"]]$spanner_id)
  expect_identical(
    setdiff(g_ref[["_spanners"]]$spanner_id, g[["_spanners"]]$spanner_id),
    "spn_p"
  )
  expect_identical(
    attr(g, "spicy_note"),
    sub(
      " Group comparison: Pearson chi-squared test.",
      "",
      attr(g_ref, "spicy_note"),
      fixed = TRUE
    )
  )

  ft <- .pv_tbl(p_value = FALSE, output = "flextable")
  expect_identical(
    ft$col_keys,
    setdiff(.pv_tbl(output = "flextable")$col_keys, "p")
  )
  ft_top <- unlist(ft$header$dataset[1L, ], use.names = FALSE)
  expect_false("p" %in% ft_top)
  expect_true(all(c("Female", "Male", "Total") %in% ft_top))

  docx <- tempfile(fileext = ".docx")
  xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(docx, xlsx)), add = TRUE)
  .pv_tbl(p_value = FALSE, output = "word", word_path = docx)
  cells <- officer::docx_summary(officer::read_docx(docx))
  cells <- cells[cells$content_type == "table cell", ]
  wd_top <- trimws(cells$text[cells$row_id == min(cells$row_id)])
  expect_false("p" %in% wd_top)
  expect_true(all(c("Female", "Male", "Total") %in% wd_top))

  .pv_tbl(p_value = FALSE, output = "excel", excel_path = xlsx)
  grid <- as.matrix(openxlsx2::wb_to_df(
    openxlsx2::wb_load(xlsx),
    col_names = FALSE
  ))
  xl_top <- grid[3L, ]
  expect_false("p" %in% xl_top)
  expect_true(all(c("Female", "Male", "Total") %in% xl_top))
  expect_identical(ncol(grid), 8L)

  clip <- NULL
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(x, ...) {
      clip <<- x
      invisible(NULL)
    },
    .package = "clipr"
  )
  suppressMessages(.pv_tbl(p_value = FALSE, output = "clipboard"))
  clip_top <- strsplit(strsplit(clip, "\n", fixed = TRUE)[[1L]][[2L]], "\t")[[
    1L
  ]]
  expect_false("p" %in% clip_top)
  expect_true(all(c("Female", "Male", "Total") %in% clip_top))
})

test_that("p_value = FALSE with assoc_measure = \"none\": a purely descriptive table", {
  tbl <- .pv_tbl(p_value = FALSE, assoc_measure = "none")
  s <- as_structured(tbl)
  expect_identical(
    unique(vapply(s$col_meta, `[[`, "", "token")),
    c("n", "pct")
  )
  # No measure, so no measure note either.
  expect_null(attr(tbl, "assoc_note"))
  expect_identical(
    names(.pv_tbl(
      p_value = FALSE,
      assoc_measure = "none",
      output = "data.frame"
    )),
    c(
      "Variable",
      "Level",
      "Female n",
      "Female %",
      "Male n",
      "Male %",
      "Total n",
      "Total %"
    )
  )
  expect_identical(
    names(.pv_tbl(p_value = FALSE, assoc_measure = "none", output = "long")),
    c("variable", "level", "group", "n", "pct")
  )
  withr::local_options(width = 100)
  expect_snapshot(print(tbl))
})

test_that("a descriptive table leaves the engines no statistic column", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  skip_if_not_installed("openxlsx2")
  tt <- .pv_tbl(p_value = FALSE, assoc_measure = "none", output = "tinytable")
  expect_identical(ncol(tt@data), 7L)
  g <- .pv_tbl(p_value = FALSE, assoc_measure = "none", output = "gt")
  expect_null(attr(g, "spicy_note"))
  expect_identical(
    g[["_spanners"]]$spanner_id,
    c("spn_variable", paste0("spn_", c("Female", "Male", "Total"), "_n"))
  )
  ft <- .pv_tbl(p_value = FALSE, assoc_measure = "none", output = "flextable")
  expect_identical(nrow(ft$footer$dataset), 0L)
  # With no statistic left, the count of text columns taken from the
  # right is zero: the sheet stops at the last percentage (column G)
  # instead of styling an empty column beyond it.
  xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(xlsx), add = TRUE)
  .pv_tbl(
    p_value = FALSE,
    assoc_measure = "none",
    output = "excel",
    excel_path = xlsx
  )
  wb <- openxlsx2::wb_load(xlsx)
  expect_false(any(grepl("^H", wb$worksheets[[1L]]$sheet_data$cc$r)))
  expect_identical(ncol(openxlsx2::wb_to_df(wb, col_names = FALSE)), 7L)
})

test_that("p_value = FALSE keeps the SMD, which has its own switch", {
  # The multinomial SMD of `smoking` (three levels) needs MASS, a
  # suggestion: the no-Suggests check mode must skip, not fail.
  skip_if_not_installed("MASS")
  tbl <- .pv_tbl(p_value = FALSE, smd = TRUE)
  ref <- .pv_tbl(smd = TRUE)
  expect_false("p" %in% names(tbl))
  expect_identical(tbl$SMD, ref$SMD)
  long <- .pv_tbl(p_value = FALSE, smd = TRUE, output = "long")
  expect_true(all(c("smd", "smd_type") %in% names(long)))
  expect_false("p" %in% names(long))
  expect_identical(
    as_structured(tbl)$col_meta$SMD,
    as_structured(ref)$col_meta$SMD
  )
})

test_that("p_adjust is refused when p_value = FALSE removes the p column", {
  expect_error(
    .pv_tbl(p_value = FALSE, p_adjust = "holm"),
    class = "spicy_invalid_input"
  )
  expect_no_error(.pv_tbl(p_value = FALSE, p_adjust = "none"))
})

test_that("p_value without `by` is ignored, with a warning when TRUE", {
  ref <- table_categorical(sochealth, c(smoking, education))
  expect_warning(
    out <- table_categorical(sochealth, c(smoking, education), p_value = TRUE),
    class = "spicy_ignored_arg"
  )
  expect_identical(out, ref)
  expect_no_warning(
    out <- table_categorical(sochealth, c(smoking, education), p_value = FALSE)
  )
  expect_identical(out, ref)
})

test_that("p_value is validated", {
  for (bad in list(NA, "no", c(TRUE, FALSE), 1)) {
    expect_error(.pv_tbl(p_value = bad), class = "spicy_invalid_input")
  }
})

test_that("correct and simulate_p are ignored without the test", {
  ref <- .pv_tbl(p_value = FALSE)
  expect_warning(
    out <- .pv_tbl(p_value = FALSE, correct = TRUE),
    "`correct` is ignored",
    class = "spicy_ignored_arg"
  )
  expect_identical(out, ref)
  # No Monte Carlo draw for a p-value nobody sees: the session's RNG is
  # where it was.
  set.seed(1)
  expect_warning(
    out <- .pv_tbl(p_value = FALSE, simulate_p = TRUE),
    "`simulate_p` is ignored",
    class = "spicy_ignored_arg"
  )
  after <- stats::runif(1L)
  set.seed(1)
  expect_identical(stats::runif(1L), after)
  expect_identical(out, ref)
})

# ---- test: chi-squared or Fisher's exact test (decision of 2026-10-18) ------------
#
# Oracle: gtsummary 2.6.1, not a dependency, run once on 2026-10-18:
#   tbl_summary(d[c(vars, by)], by = all_of(by), missing = "no") |> add_p()
# reading `test_name` and `p.value` of the label rows, with the label
# attributes of sochealth removed. Its rule (R/assign_tests.R, lines
# 330-357): "chisq.test.no.correct" when every expected count is at least
# 5, "fisher.test" otherwise. Every p-value below agreed with spicy's to
# the last digit printed (15 significant).

.gts_sh <- function(rows = NULL) {
  d <- as.data.frame(sochealth)
  if (is.null(rows)) d else d[rows, ]
}

.first_rows <- function(long) long[!duplicated(long$variable), ]

test_that("test = \"auto\": dense tables take the chi-squared test, as in gtsummary", {
  out <- .first_rows(table_categorical(
    .gts_sh(),
    c(smoking, education, physical_activity),
    by = sex,
    output = "long"
  ))
  expect_identical(out$test_type, rep("chi_squared", 3L))
  expect_equal(
    out$p,
    c(0.712519570026406, 0.343778176322083, 0.831676321336384),
    tolerance = 1e-10
  )
  # The chi-squared of chisq.test(correct = FALSE), complete cases.
  sh <- .gts_sh()
  ref <- stats::chisq.test(table(sh$smoking, sh$sex), correct = FALSE)
  expect_equal(out$chi2[[1L]], unname(ref$statistic), tolerance = 1e-12)
  expect_equal(out$p[[1L]], ref$p.value, tolerance = 1e-12)
})

test_that("test = \"auto\": an expected count below 5 takes Fisher's exact test, as in gtsummary", {
  d <- .gts_sh(1:40)
  tbl <- table_categorical(
    d,
    c(smoking, dentist_12m, physical_activity),
    by = sex
  )
  out <- .first_rows(table_categorical(
    d,
    c(smoking, dentist_12m, physical_activity),
    by = sex,
    output = "long"
  ))
  # Minimum expected counts: 3.6 (smoking), 5.85, 8.55.
  expect_identical(
    out$test_type,
    c("fisher_exact", "chi_squared", "chi_squared")
  )
  expect_equal(
    out$p,
    c(0.257949447423132, 0.564090263272185, 0.726308097640108),
    tolerance = 1e-10
  )
  expect_equal(
    out$p[[1L]],
    stats::fisher.test(table(d$smoking, d$sex))$p.value,
    tolerance = 1e-12
  )
  # A Fisher row has no chi-squared statistic and no degrees of freedom;
  # its association measure is unchanged.
  expect_true(is.na(out$chi2[[1L]]) && is.na(out$df[[1L]]))
  ref <- .first_rows(table_categorical(
    d,
    c(smoking, dentist_12m, physical_activity),
    by = sex,
    test = "chisq",
    output = "long"
  ))
  expect_identical(out$effect_size, ref$effect_size)
  # The note names the test of every variable, the chi-squared first.
  expect_identical(
    attr(tbl, "assoc_note"),
    paste(
      "Group comparison: Pearson chi-squared test (dentist_12m,",
      "physical_activity); Fisher exact test (smoking)."
    )
  )
  wide <- table_categorical(
    d,
    c(smoking, dentist_12m, physical_activity),
    by = sex,
    output = "data.frame"
  )
  expect_identical(
    names(wide)[names(wide) %in% c("test_type", "Chi2", "df", "p")],
    c("test_type", "Chi2", "df", "p")
  )
  expect_identical(
    wide$test_type,
    rep(c("fisher_exact", "chi_squared", "chi_squared"), each = 2L)
  )
  # The typed view and glance() carry the same.
  s <- as_structured(tbl)
  hdr <- s$body$.row_role == "factor_header"
  expect_identical(
    s$col_meta$p$test_type[hdr],
    c("fisher_exact", "chi_squared", "chi_squared")
  )
  expect_true(all(is.na(s$col_meta$p$test_type[!hdr])))
  skip_if_not_installed("broom")
  gl <- broom::glance(tbl)
  expect_identical(gl$test_type[gl$outcome == "smoking"], "fisher_exact")
  expect_true(is.na(gl$statistic[gl$outcome == "smoking"]))
  expect_identical(gl$test_type[gl$outcome == "dentist_12m"], "chi_squared")
})

test_that("Fisher's exact test of an r x c table reproduces gtsummary", {
  d <- .gts_sh(1:100)
  out <- .first_rows(table_categorical(
    d,
    c(self_rated_health, employment_status),
    by = education,
    output = "long"
  ))
  # 4 x 3 tables, minimum expected counts 1.40 and 1.68.
  expect_identical(out$test_type, rep("fisher_exact", 2L))
  expect_equal(out$p, c(0.1666469610989, 0.0490952340252815), tolerance = 1e-10)
  expect_equal(
    out$p[[1L]],
    stats::fisher.test(table(d$self_rated_health, d$education))$p.value,
    tolerance = 1e-12
  )
  # A 3 x 4 table.
  out34 <- .first_rows(table_categorical(
    d,
    bmi_category,
    by = age_group,
    output = "long"
  ))
  expect_identical(out34$test_type, "fisher_exact")
  expect_equal(out34$p, 0.601196474720587, tolerance = 1e-10)
})

test_that("a table the exact enumeration cannot handle takes the Monte Carlo test", {
  d <- .gts_sh(1:300)
  d$sex_age <- interaction(d$sex, d$age_group, sep = " ")
  tab <- table(d$region, d$sex_age)
  # A 6 x 8 table, minimum expected count 3.28: FEXACT gives up.
  expect_error(stats::fisher.test(tab))
  set.seed(20261018)
  tbl <- table_categorical(d, region, by = sex_age)
  set.seed(20261018)
  long <- table_categorical(d, region, by = sex_age, output = "long")
  expect_identical(unique(long$test_type), "fisher_exact_mc")
  expect_true(all(is.na(long$chi2)))
  # The same draws as a direct call under the same seed, and within
  # Monte Carlo error of a 100 000-replicate estimate (0.3036).
  set.seed(20261018)
  direct <- stats::fisher.test(tab, simulate.p.value = TRUE, B = 2000)$p.value
  expect_identical(long$p[[1L]], direct)
  set.seed(1)
  big <- stats::fisher.test(tab, simulate.p.value = TRUE, B = 1e5)$p.value
  expect_lt(abs(long$p[[1L]] - big), 4 * sqrt(big * (1 - big) / 2000))
  expect_identical(
    attr(tbl, "assoc_note"),
    "Group comparison: Fisher exact test (Monte Carlo, 2000 replicates)."
  )
  # `simulate_B` sets the replicates, and the note says how many.
  set.seed(5)
  b500 <- table_categorical(d, region, by = sex_age, simulate_B = 500)
  set.seed(5)
  expect_identical(
    b500$p[[1L]],
    stats::fisher.test(tab, simulate.p.value = TRUE, B = 500)$p.value
  )
  expect_match(
    attr(b500, "assoc_note"),
    "(Monte Carlo, 500 replicates)",
    fixed = TRUE
  )
})

test_that("test = \"chisq\" gives the 0.13.0 p-values, test = \"fisher\" tests every variable exactly", {
  d <- .gts_sh(1:40)
  vars <- c("smoking", "dentist_12m", "physical_activity")
  chisq <- .first_rows(table_categorical(
    d,
    vars,
    by = sex,
    test = "chisq",
    output = "long"
  ))
  fisher <- .first_rows(table_categorical(
    d,
    vars,
    by = sex,
    test = "fisher",
    output = "long"
  ))
  expect_identical(chisq$test_type, rep("chi_squared", 3L))
  expect_identical(fisher$test_type, rep("fisher_exact", 3L))
  for (j in seq_along(vars)) {
    tab <- table(d[[vars[[j]]]], d$sex)
    expect_equal(
      chisq$p[[j]],
      suppressWarnings(stats::chisq.test(tab, correct = FALSE))$p.value,
      tolerance = 1e-12
    )
    expect_equal(
      fisher$p[[j]],
      stats::fisher.test(tab)$p.value,
      tolerance = 1e-12
    )
  }
  expect_identical(
    attr(table_categorical(d, vars, by = sex, test = "fisher"), "assoc_note"),
    "Group comparison: Fisher exact test."
  )
  expect_error(
    table_categorical(d, smoking, by = sex, test = "exact"),
    class = "spicy_invalid_input"
  )
})

test_that("p_adjust adjusts the p-values the tests produced", {
  d <- .gts_sh(1:40)
  out <- .first_rows(table_categorical(
    d,
    c(smoking, dentist_12m, physical_activity),
    by = sex,
    p_adjust = "holm",
    output = "long"
  ))
  expect_equal(out$p_unadjusted[[1L]], 0.257949447423132, tolerance = 1e-10)
  expect_equal(out$p, stats::p.adjust(out$p_unadjusted, "holm"))
})

test_that("correct and simulate_p reach only the variables the chi-squared tests", {
  d <- .gts_sh(1:40)
  vars <- c("smoking", "dentist_12m", "physical_activity")
  # Yates on the two chi-squared 2 x 2 tables; smoking keeps its exact p.
  expect_no_warning(
    yates <- table_categorical(d, vars, by = sex, correct = TRUE)
  )
  long <- .first_rows(table_categorical(
    d,
    vars,
    by = sex,
    correct = TRUE,
    output = "long"
  ))
  expect_equal(long$p[[1L]], 0.257949447423132, tolerance = 1e-10)
  expect_equal(
    long$p[[2L]],
    stats::chisq.test(table(d$dentist_12m, d$sex), correct = TRUE)$p.value,
    tolerance = 1e-12
  )
  expect_match(
    attr(yates, "assoc_note"),
    "Fisher exact test (smoking). Yates continuity correction applied.",
    fixed = TRUE
  )
  # A simulated chi-squared is named as such; inside the list, where the
  # variables follow in parentheses, the qualifier takes a comma.
  set.seed(2)
  sim <- table_categorical(d, vars, by = sex, simulate_p = TRUE)
  expect_match(
    attr(sim, "assoc_note"),
    "Pearson chi-squared test, simulated (dentist_12m, physical_activity)",
    fixed = TRUE
  )
  # Alone, it keeps cross_tab()'s wording.
  set.seed(2)
  expect_identical(
    attr(
      table_categorical(d, dentist_12m, by = sex, simulate_p = TRUE),
      "assoc_note"
    ),
    "Group comparison: Pearson chi-squared test (simulated)."
  )
  # Under test = "fisher" neither reaches a variable.
  ref <- table_categorical(d, vars, by = sex, test = "fisher")
  expect_warning(
    out <- table_categorical(
      d,
      vars,
      by = sex,
      test = "fisher",
      correct = TRUE
    ),
    "`correct` is ignored when `test = \"fisher\"`",
    class = "spicy_ignored_arg"
  )
  expect_identical(out, ref)
  expect_warning(
    table_categorical(d, vars, by = sex, test = "fisher", simulate_p = TRUE),
    "`simulate_p` is ignored when `test = \"fisher\"`",
    class = "spicy_ignored_arg"
  )
})

test_that("test is ignored without `by` or without the p column", {
  ref <- table_categorical(sochealth, c(smoking, education))
  expect_warning(
    out <- table_categorical(sochealth, c(smoking, education), test = "fisher"),
    "`test` is ignored when `by` is not used",
    class = "spicy_ignored_arg"
  )
  expect_identical(out, ref)
  expect_no_warning(table_categorical(
    sochealth,
    c(smoking, education),
    test = "auto"
  ))
  expect_warning(
    out <- .pv_tbl(p_value = FALSE, test = "fisher"),
    "`test` is ignored when `p_value = FALSE`",
    class = "spicy_ignored_arg"
  )
  expect_identical(out, .pv_tbl(p_value = FALSE))
})

test_that("Fisher's exact test runs on whole-number weights and not on fractional ones", {
  d <- data.frame(
    x = factor(c("a", "a", "b", "b", "a", "b")),
    g = factor(c("u", "v", "u", "v", "v", "u")),
    w = c(5, 1, 1, 6, 2, 1)
  )
  # Frequency weights: the weighted table is the table of counts.
  out <- .first_rows(table_categorical(
    d,
    x,
    by = g,
    weights = w,
    output = "long"
  ))
  expanded <- d[rep(seq_len(nrow(d)), d$w), ]
  expect_identical(out$test_type, "fisher_exact")
  expect_equal(
    out$p,
    stats::fisher.test(table(expanded$x, expanded$g))$p.value,
    tolerance = 1e-12
  )
  # Fractional weights: "auto" keeps the chi-squared, with a warning, and
  # "fisher" is an error.
  d$w <- d$w / 3
  expect_warning(
    frac <- table_categorical(d, x, by = g, weights = w, output = "long"),
    class = "spicy_fallback"
  )
  expect_identical(unique(frac$test_type), "chi_squared")
  expect_equal(
    frac$p,
    table_categorical(
      d,
      x,
      by = g,
      weights = w,
      test = "chisq",
      output = "long"
    )$p
  )
  expect_error(
    table_categorical(d, x, by = g, weights = w, test = "fisher"),
    class = "spicy_unsupported"
  )
})

test_that("the test reads the observed table: no (Missing) level, no empty level", {
  d <- .gts_sh(1:40)
  # drop_na = FALSE shows a (Missing) level; the test is the complete
  # cases' all the same.
  d$smoking[1:3] <- NA
  shown <- .first_rows(table_categorical(d, smoking, by = sex, output = "long"))
  cc <- d[!is.na(d$smoking), ]
  expect_identical(shown$test_type, "fisher_exact")
  expect_equal(
    shown$p,
    stats::fisher.test(table(cc$smoking, cc$sex))$p.value,
    tolerance = 1e-12
  )
  # A declared level nobody holds has no expected count to be small: the
  # dense table keeps the chi-squared (gtsummary's table() keeps the empty
  # row and would take the exact test).
  sh <- .gts_sh()
  sh$sex <- factor(sh$sex, levels = c("Female", "Male", "Other"))
  dense <- .first_rows(table_categorical(
    sh,
    smoking,
    by = sex,
    output = "long"
  ))
  expect_identical(dense$test_type, "chi_squared")
  expect_equal(dense$p, 0.712519570026406, tolerance = 1e-10)
})

test_that("a variable with nothing to test carries no test type", {
  d <- data.frame(
    x = factor(c("a", "a", "a", "a")),
    y = factor(c("a", "b", "a", "b")),
    g = factor(c("u", "v", "u", "v"))
  )
  out <- .first_rows(table_categorical(d, c(x, y), by = g, output = "long"))
  expect_true(is.na(out$test_type[[1L]]) && is.na(out$p[[1L]]))
  expect_identical(out$test_type[[2L]], "fisher_exact")
  # One test ran: the note names it alone, with no list of variables.
  expect_match(
    attr(table_categorical(d, c(x, y), by = g), "assoc_note"),
    "Group comparison: Fisher exact test.",
    fixed = TRUE
  )
  # No test ran: no sentence, and no test type in the typed view.
  none <- table_categorical(d, x, by = g)
  expect_null(attr(none, "assoc_note"))
  expect_true(all(is.na(as_structured(none)$col_meta$p$test_type)))
})

test_that("the test helpers read the table cross_tab() tests", {
  set.seed(3)
  d <- data.frame(
    x = factor(sample(c("a", "b", "c"), 200, replace = TRUE)),
    g = factor(sample(c("u", "v"), 200, replace = TRUE)),
    w = stats::runif(200, 0.2, 3)
  )
  d$w[c(2, 9)] <- NA
  tab <- spicy:::.cat_test_table(d$x, d$g, d$w, rescale = TRUE)
  ct <- suppressWarnings(spicy::cross_tab(
    d$x,
    d$g,
    weights = d$w,
    rescale = TRUE
  ))
  expect_equal(
    unname(suppressWarnings(stats::chisq.test(tab, correct = FALSE))$statistic),
    attr(ct, "chi2"),
    tolerance = 1e-12
  )
  expect_equal(sum(tab), 198)
  expect_identical(spicy:::.cat_resolve_test(tab, "chisq"), "chisq")
  expect_identical(spicy:::.cat_resolve_test(tab, "fisher"), "fisher")
  expect_identical(
    spicy:::.cat_resolve_test(matrix(c(5, 5, 5, 5), 2), "auto"),
    "chisq"
  )
  expect_identical(
    spicy:::.cat_resolve_test(matrix(c(5, 5, 5, 4), 2), "auto"),
    "fisher"
  )
})

test_that("the note names the tests in French", {
  withr::local_options(spicy.language = "fr")
  d <- .gts_sh(1:40)
  expect_identical(
    attr(
      table_categorical(
        d,
        c(smoking, dentist_12m, physical_activity),
        by = sex
      ),
      "assoc_note"
    ),
    paste0(
      "Comparaison des groupes\u00a0: test du khi-deux de Pearson ",
      "(dentist_12m, physical_activity)\u00a0; test exact de Fisher (smoking)."
    )
  )
  d300 <- .gts_sh(1:300)
  d300$sex_age <- interaction(d300$sex, d300$age_group, sep = " ")
  set.seed(1)
  expect_identical(
    attr(table_categorical(d300, region, by = sex_age), "assoc_note"),
    "Comparaison des groupes\u00a0: test exact de Fisher (Monte-Carlo, 2000 r\u00e9plications)."
  )
})

test_that("the Monte Carlo exact test takes a comma inside a list of tests", {
  # The 20 x 2 table of x overflows FEXACT; y's does not.
  k <- c(6, 5, 7, 4, 6, 8, 5, 6, 7, 3, 6, 5, 4, 7, 6, 5, 8, 6, 5, 4)
  d <- data.frame(
    g = factor(rep(sprintf("g%02d", 1:20), each = 200L)),
    x = factor(unlist(lapply(k, function(m) {
      c(rep("yes", m), rep("no", 200 - m))
    }))),
    y = factor(rep(c("yes", "no"), 2000L))
  )
  set.seed(9)
  expect_identical(
    attr(table_categorical(d, c(x, y), by = g, test = "fisher"), "assoc_note"),
    paste(
      "Group comparison: Fisher exact test (y); Fisher exact test, Monte",
      "Carlo with 2000 replicates (x)."
    )
  )
  withr::local_options(spicy.language = "fr")
  set.seed(9)
  expect_identical(
    attr(table_categorical(d, c(x, y), by = g, test = "fisher"), "assoc_note"),
    paste0(
      "Comparaison des groupes\u00a0: test exact de Fisher (y)\u00a0; test ",
      "exact de Fisher, Monte-Carlo avec 2000 r\u00e9plications (x)."
    )
  )
})

# smoking takes Fisher's exact test, the two others the chi-squared; smoking
# loses one value, dentist_12m two.
.comma_label_tbl <- function(label) {
  d <- .gts_sh(1:40)
  d$smoking[3] <- NA
  d$dentist_12m[1:2] <- NA
  table_categorical(
    d,
    c(smoking, dentist_12m, physical_activity),
    by = sex,
    drop_na = TRUE,
    labels = c(dentist_12m = label)
  )
}

test_that("a label with a comma turns its list in a note to semicolons", {
  assoc <- function() {
    attr(
      table_categorical(
        .gts_sh(),
        c(smoking, education, self_rated_health),
        by = sex,
        labels = c(education = "Education, highest level")
      ),
      "assoc_note"
    )
  }
  out <- .comma_label_tbl("Dentist, past 12 months")
  expect_identical(
    attr(out, "assoc_note"),
    paste(
      "Group comparison: Pearson chi-squared test (Dentist, past 12 months;",
      "physical_activity); Fisher exact test (smoking)."
    )
  )
  expect_identical(
    attr(out, "missing_note"),
    "Missing values removed: smoking (1); Dentist, past 12 months (2)."
  )
  expect_match(
    assoc(),
    "Phi: smoking. Cramer's V: Education, highest level; self_rated_health.",
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  out <- .comma_label_tbl("Dentist, past 12 months")
  expect_identical(
    attr(out, "assoc_note"),
    paste0(
      "Comparaison des groupes\u00a0: test du khi-deux de Pearson (Dentist, ",
      "past 12 months\u00a0; physical_activity)\u00a0; test exact de Fisher ",
      "(smoking)."
    )
  )
  expect_identical(
    attr(out, "missing_note"),
    paste0(
      "Valeurs manquantes retir\u00e9es\u00a0: smoking (1)\u00a0; Dentist, ",
      "past 12 months (2)."
    )
  )
  expect_match(
    assoc(),
    paste0(
      "Phi\u00a0: smoking. V de Cram\u00e9r\u00a0: Education, ",
      "highest level\u00a0; self_rated_health."
    ),
    fixed = TRUE
  )
})

test_that("labels without a comma keep the comma join in the notes", {
  out <- .comma_label_tbl("Dentist in the past 12 months")
  expect_identical(
    attr(out, "assoc_note"),
    paste(
      "Group comparison: Pearson chi-squared test (Dentist in the past 12",
      "months, physical_activity); Fisher exact test (smoking)."
    )
  )
  expect_identical(
    attr(out, "missing_note"),
    "Missing values removed: smoking (1), Dentist in the past 12 months (2)."
  )
  withr::local_options(spicy.language = "fr")
  out <- .comma_label_tbl("Dentist in the past 12 months")
  expect_identical(
    attr(out, "assoc_note"),
    paste0(
      "Comparaison des groupes\u00a0: test du khi-deux de Pearson (Dentist ",
      "in the past 12 months, physical_activity)\u00a0; test exact de Fisher ",
      "(smoking)."
    )
  )
  expect_identical(
    attr(out, "missing_note"),
    paste0(
      "Valeurs manquantes retir\u00e9es\u00a0: smoking (1), Dentist in the ",
      "past 12 months (2)."
    )
  )
})

# ---- test = "midp" and "n1": the tests of a 2 x 2 table ---------------------------
#
# Complement of the decision of 2026-10-18. Oracles: the research memo of
# that day, its values reproduced in base R and against exact2x2, epitools,
# DescTools (the N-1 statistic, to 1e-12) and contingencytables, none of them
# a dependency. The mid-p is the probability-ordered one, ties at half weight
# (Fagerland, Lydersen & Laake 2017, Eq. 1.12 and section 4.4.6; Lydersen,
# Fagerland & Laake 2009, Eq. 12); the N-1 statistic is Pearson's times
# (N - 1) / N (Campbell 2007).

# A data frame whose table of `g` (rows) by `x` (columns) is a b / c d.
.mp_2x2 <- function(a, b, c, d) {
  data.frame(
    g = factor(rep(c("g1", "g1", "g2", "g2"), c(a, b, c, d)), c("g1", "g2")),
    x = factor(
      rep(c("yes", "no", "yes", "no"), c(a, b, c, d)),
      c("yes", "no")
    )
  )
}

# The test row of that table under `test`.
.mp_row <- function(v, test) {
  table_categorical(
    .mp_2x2(v[[1L]], v[[2L]], v[[3L]], v[[4L]]),
    x,
    by = g,
    test = test,
    output = "long"
  )[1L, ]
}

# A binary variable, a dense and a sparse three-level one, two groups of 30.
.mp_mixed <- function() {
  data.frame(
    g = factor(rep(c("a", "b"), each = 30L)),
    x = factor(rep(c("yes", "no", "yes", "no"), c(12, 18, 5, 25))),
    z = factor(rep(c("u", "v", "w", "u", "v", "w"), c(10, 10, 10, 8, 12, 10))),
    w = factor(rep(c("r", "s", "t", "r", "s", "t"), c(1, 2, 27, 0, 3, 27)))
  )
}

test_that("test = \"midp\" reproduces the published Fisher mid-p values", {
  pins <- list(
    # Fagerland et al. 2017: tea (p. 100), epinephrine (Table 4.12), GADA
    # (p. 102), CHRNA4 (Table 4.15); the last two have unequal margins.
    list(c(3, 1, 1, 3), 0.2571428571),
    list(c(1, 33, 7, 27), 0.02965625843),
    list(c(9, 4, 4, 10), 0.03914177672),
    list(c(0, 16, 15, 57), 0.0446596912),
    # Campbell 2007, Table I(b), and a dense table.
    list(c(4, 16, 1, 21), 0.1118332887),
    list(c(30, 20, 18, 32), 0.01805544078),
    # The observed table is the most probable one.
    list(c(5, 5, 5, 5), 0.8281408993)
  )
  for (pin in pins) {
    out <- .mp_row(pin[[1L]], "midp")
    info <- paste(pin[[1L]], collapse = " ")
    expect_identical(out$test_type, "fisher_midp", info = info)
    expect_equal(out$p, pin[[2L]], tolerance = 1e-9, info = info)
    # An exact test: no chi-squared statistic, no degrees of freedom.
    expect_true(is.na(out$chi2) && is.na(out$df), info = info)
  }
  expect_identical(
    attr(
      table_categorical(.mp_2x2(3, 1, 1, 3), x, by = g, test = "midp"),
      "assoc_note"
    ),
    "Group comparison: Fisher mid-p test."
  )
})

test_that("the mid-p reads ties within fisher.test()'s tolerance: a table and its mirror agree", {
  # 2 36 / 8 30 and its mirror are as probable as each other: an exact
  # comparison gives 0.0680 and 0.0320 (contingencytables 3.1.0).
  p <- .mp_row(c(2, 36, 8, 30), "midp")$p
  expect_equal(p, 0.0499865594, tolerance = 1e-9)
  expect_identical(.mp_row(c(8, 30, 2, 36), "midp")$p, p)
  # Transposed: the groups as the variable, the variable as the groups.
  tr <- table_categorical(
    .mp_2x2(2, 36, 8, 30),
    g,
    by = x,
    test = "midp",
    output = "long"
  )
  expect_equal(tr$p[[1L]], p, tolerance = 1e-12)
  for (v in list(c(9, 4, 4, 10), c(0, 16, 15, 57), c(4, 16, 1, 21))) {
    tab <- matrix(v, 2L, byrow = TRUE)
    ref <- spicy:::.cat_midp_p(tab)
    expect_equal(spicy:::.cat_midp_p(t(tab)), ref, tolerance = 1e-12)
    expect_equal(spicy:::.cat_midp_p(tab[2:1, ]), ref, tolerance = 1e-12)
  }
})

test_that("the central mid-p coincides at equal margins and nowhere else", {
  # Twice the smaller one-sided mid-p (exact2x2, epitools; Agresti 2019,
  # p. 49): documented, not computed by spicy.
  central <- function(v) {
    tab <- matrix(v, 2L, byrow = TRUE)
    m <- sum(tab[, 1L])
    n <- sum(tab[, 2L])
    k <- sum(tab[1L, ])
    a <- tab[1L, 1L]
    f <- stats::dhyper(a, m, n, k)
    lower <- stats::phyper(a, m, n, k) - 0.5 * f
    upper <- stats::phyper(a - 1, m, n, k, lower.tail = FALSE) - 0.5 * f
    min(1, 2 * min(lower, upper))
  }
  # Equal row totals: tea, epinephrine, the dense table, the tie table.
  equal <- list(
    c(3, 1, 1, 3),
    c(1, 33, 7, 27),
    c(30, 20, 18, 32),
    c(2, 36, 8, 30)
  )
  for (v in equal) {
    expect_equal(.mp_row(v, "midp")$p, central(v), tolerance = 1e-12)
  }
  # Unequal margins: GADA 0.0391 against 0.0468, CHRNA4 0.0447 against
  # 0.0364.
  expect_equal(central(c(9, 4, 4, 10)), 0.04678786338, tolerance = 1e-9)
  expect_equal(central(c(0, 16, 15, 57)), 0.03641394686, tolerance = 1e-9)
  expect_gt(abs(.mp_row(c(9, 4, 4, 10), "midp")$p - 0.04678786338), 0.007)
  expect_gt(abs(.mp_row(c(0, 16, 15, 57), "midp")$p - 0.03641394686), 0.008)
  # Equal margins, the most probable table: 0.828 against 1.
  expect_equal(central(c(5, 5, 5, 5)), 1, tolerance = 1e-12)
  expect_equal(.mp_row(c(5, 5, 5, 5), "midp")$p, 0.8281408993, tolerance = 1e-9)
})

test_that("test = \"n1\" is the N-1 chi-squared of Campbell (2007)", {
  # Campbell's Table I(b): X2 = 2.3858, N-1 p = 0.1270 (Busing et al. 2016).
  out <- .mp_row(c(4, 16, 1, 21), "n1")
  expect_identical(out$test_type, "n1_chi_squared")
  expect_equal(out$chi2, 2.329041769, tolerance = 1e-9)
  expect_identical(out$df, 1)
  expect_equal(out$p, 0.1269800217, tolerance = 1e-9)
  expect_equal(
    .mp_row(c(30, 20, 18, 32), "n1")$p,
    0.0168537629,
    tolerance = 1e-9
  )
  expect_equal(.mp_row(c(9, 4, 4, 10), "n1")$p, 0.03815109231, tolerance = 1e-9)
  # Pearson's statistic times (N - 1) / N, and the Mantel-Haenszel form
  # (n11 - E)^2 / V of a single 2 x 2 table.
  tab <- matrix(c(4, 16, 1, 21), 2L, byrow = TRUE)
  pearson <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  expect_equal(out$chi2, unname(pearson$statistic) * 41 / 42, tolerance = 1e-12)
  r <- rowSums(tab)
  s <- colSums(tab)
  n <- sum(tab)
  mh <- (tab[1L, 1L] - r[[1L]] * s[[1L]] / n)^2 /
    (prod(r, s) / (n^2 * (n - 1)))
  expect_equal(out$chi2, mh, tolerance = 1e-12)
  expect_identical(
    attr(
      table_categorical(.mp_2x2(4, 16, 1, 21), x, by = g, test = "n1"),
      "assoc_note"
    ),
    "Group comparison: N\u22121 chi-squared test."
  )
})

test_that("Campbell's rule sends a table with an expected count below 1 to Fisher's exact test", {
  # 2 1 / 1 20: smallest expected count 0.375.
  out <- .mp_row(c(2, 1, 1, 20), "n1")
  expect_identical(out$test_type, "fisher_exact")
  expect_equal(out$p, 0.03162055336, tolerance = 1e-9)
  expect_true(is.na(out$chi2) && is.na(out$df))
  # One table, two variables: x's smallest expected count is 2.38, y's
  # 0.95. Each takes its test, and the note names both.
  d <- .mp_2x2(4, 16, 1, 21)
  d$y <- factor(rep(c("yes", "no", "no"), c(2, 18, 22)), c("yes", "no"))
  tbl <- table_categorical(d, c(x, y), by = g, test = "n1")
  long <- .first_rows(table_categorical(
    d,
    c(x, y),
    by = g,
    test = "n1",
    output = "long"
  ))
  expect_identical(long$test_type, c("n1_chi_squared", "fisher_exact"))
  expect_equal(
    long$p[[2L]],
    stats::fisher.test(table(d$y, d$g))$p.value,
    tolerance = 1e-12
  )
  expect_identical(
    attr(tbl, "assoc_note"),
    "Group comparison: N\u22121 chi-squared test (x); Fisher exact test (y)."
  )
  # The rule at its boundary: an expected count of exactly 1 keeps the
  # N-1 test.
  expect_identical(
    spicy:::.cat_resolve_test(matrix(c(1, 1, 9, 9), 2L), "n1"),
    "n1"
  )
  expect_identical(
    spicy:::.cat_resolve_test(matrix(c(1, 0, 9, 10), 2L), "n1"),
    "fisher"
  )
})

test_that("midp and n1 test 2 x 2 tables only; a larger table follows auto", {
  d <- .mp_mixed()
  auto <- .first_rows(table_categorical(
    d,
    c(x, z, w),
    by = g,
    output = "long"
  ))
  # z is dense (expected counts 9 to 11), w sparse (0.5).
  expect_identical(
    auto$test_type,
    c("chi_squared", "chi_squared", "fisher_exact")
  )
  midp <- .first_rows(table_categorical(
    d,
    c(x, z, w),
    by = g,
    test = "midp",
    output = "long"
  ))
  n1 <- .first_rows(table_categorical(
    d,
    c(x, z, w),
    by = g,
    test = "n1",
    output = "long"
  ))
  expect_identical(
    midp$test_type,
    c("fisher_midp", "chi_squared", "fisher_exact")
  )
  expect_identical(
    n1$test_type,
    c("n1_chi_squared", "chi_squared", "fisher_exact")
  )
  expect_identical(midp$p[2:3], auto$p[2:3])
  expect_identical(n1$p[2:3], auto$p[2:3])
  # The note names the test of each variable (the measure, Phi or
  # Cramer's V, would add a sentence of its own).
  expect_identical(
    attr(
      table_categorical(
        d,
        c(x, z, w),
        by = g,
        test = "midp",
        assoc_measure = "none"
      ),
      "assoc_note"
    ),
    paste(
      "Group comparison: Pearson chi-squared test (z); Fisher exact test",
      "(w); Fisher mid-p test (x)."
    )
  )
  expect_identical(
    attr(
      table_categorical(
        d,
        c(x, z),
        by = g,
        test = "n1",
        assoc_measure = "none"
      ),
      "assoc_note"
    ),
    "Group comparison: Pearson chi-squared test (z); N\u22121 chi-squared test (x)."
  )
  # A binary variable by three groups is not 2 x 2 either.
  d3 <- data.frame(
    g = factor(rep(c("a", "b", "c"), each = 20L)),
    x = factor(rep(c("yes", "no"), 30L))
  )
  three <- table_categorical(d3, x, by = g, test = "midp", output = "long")
  expect_identical(unique(three$test_type), "chi_squared")
  expect_identical(
    three$p,
    table_categorical(d3, x, by = g, output = "long")$p
  )
})

test_that("a table with an empty row or column gets no test under midp and n1", {
  # x is observed at one level: its table has an empty row.
  d <- data.frame(
    g = factor(rep(c("a", "b"), each = 5L)),
    x = factor(rep("yes", 10L), levels = c("yes", "no")),
    y = factor(rep(c("yes", "no"), 5L))
  )
  for (tt in c("midp", "n1")) {
    out <- .first_rows(table_categorical(
      d,
      c(x, y),
      by = g,
      test = tt,
      output = "long"
    ))
    expect_true(is.na(out$p[[1L]]) && is.na(out$test_type[[1L]]), info = tt)
    expect_false(is.na(out$p[[2L]]), info = tt)
  }
  # The helper refuses such a table whatever the test: the mid-p would
  # report 0.5 there, and Fisher's exact test 1.
  tab <- matrix(c(0, 0, 5, 7), 2L)
  for (kind in c("midp", "n1", "fisher")) {
    res <- spicy:::.cat_run_test(tab, kind, 2000L)
    expect_true(is.na(res$p) && is.na(res$type), info = kind)
  }
})

test_that("p_adjust adjusts the mid-p values", {
  d <- .mp_2x2(9, 4, 4, 10)
  # A second variable on the same 13 + 14 respondents.
  d$y <- factor(rep(c("yes", "no", "yes", "no"), c(3, 10, 1, 13)))
  out <- .first_rows(table_categorical(
    d,
    c(x, y),
    by = g,
    test = "midp",
    p_adjust = "holm",
    output = "long"
  ))
  expect_equal(out$p_unadjusted[[1L]], 0.03914177672, tolerance = 1e-9)
  expect_equal(out$p, stats::p.adjust(out$p_unadjusted, "holm"))
  expect_true(out$p[[1L]] > out$p_unadjusted[[1L]])
  expect_match(
    attr(
      table_categorical(d, c(x, y), by = g, test = "midp", p_adjust = "holm"),
      "assoc_note"
    ),
    "Fisher mid-p test. P-values adjusted for 2 tests (Holm method).",
    fixed = TRUE
  )
})

test_that("correct is ignored under midp and n1; simulate_p reaches a larger table", {
  d <- .mp_mixed()
  for (tt in c("midp", "n1")) {
    ref <- table_categorical(d, c(x, z), by = g, test = tt)
    expect_warning(
      out <- table_categorical(d, c(x, z), by = g, test = tt, correct = TRUE),
      sprintf("`correct` is ignored when `test = \"%s\"`", tt),
      class = "spicy_ignored_arg"
    )
    expect_identical(out, ref)
  }
  # The Monte Carlo p-value of the chi-squared still reaches z, named with
  # a comma inside the list.
  set.seed(4)
  sim <- table_categorical(
    d,
    c(x, z),
    by = g,
    test = "midp",
    simulate_p = TRUE,
    assoc_measure = "none"
  )
  expect_identical(
    attr(sim, "assoc_note"),
    paste(
      "Group comparison: Pearson chi-squared test, simulated (z); Fisher",
      "mid-p test (x)."
    )
  )
})

test_that("the mid-p needs counts; under weights Campbell's rule gives way to the N-1 test", {
  d <- data.frame(
    x = factor(c("a", "a", "b", "b", "a", "b")),
    g = factor(c("u", "v", "u", "v", "v", "u")),
    w = c(5, 1, 1, 6, 2, 1)
  )
  # Frequency weights: the mid-p of the expanded data.
  expanded <- d[rep(seq_len(nrow(d)), d$w), ]
  expect_equal(
    table_categorical(
      d,
      x,
      by = g,
      weights = w,
      test = "midp",
      output = "long"
    )$p,
    table_categorical(expanded, x, by = g, test = "midp", output = "long")$p,
    tolerance = 1e-12
  )
  # Fractional weights: the mid-p has no counts to condition on.
  d$w <- d$w / 3
  expect_error(
    table_categorical(d, x, by = g, weights = w, test = "midp"),
    class = "spicy_unsupported"
  )
  # Fractional weights and a smallest expected count of 0.76: Fisher's
  # exact test is out of reach, so the N-1 test stays, with a warning.
  f <- data.frame(
    x = factor(c("a", "a", "b", "b")),
    g = factor(c("u", "v", "u", "v")),
    w = c(2.5, 0.2, 0.3, 7)
  )
  expect_warning(
    out <- table_categorical(
      f,
      x,
      by = g,
      weights = w,
      test = "n1",
      output = "long"
    ),
    "N-1 chi-squared test although an expected count is below 1",
    class = "spicy_fallback"
  )
  tab <- matrix(c(2.5, 0.3, 0.2, 7), 2L)
  x2 <- unname(
    suppressWarnings(stats::chisq.test(tab, correct = FALSE))$statistic
  )
  expect_identical(unique(out$test_type), "n1_chi_squared")
  expect_equal(unique(out$chi2), x2 * 9 / 10, tolerance = 1e-12)
})

test_that("the notes name the 2 x 2 tests in French", {
  withr::local_options(spicy.language = "fr")
  d <- .mp_2x2(4, 16, 1, 21)
  d$y <- factor(rep(c("yes", "no", "no"), c(2, 18, 22)), c("yes", "no"))
  expect_identical(
    attr(table_categorical(d, c(x, y), by = g, test = "n1"), "assoc_note"),
    paste0(
      "Comparaison des groupes\u00a0: test du khi-deux N\u22121 (x)\u00a0; ",
      "test exact de Fisher (y)."
    )
  )
  expect_identical(
    attr(
      table_categorical(.mp_2x2(3, 1, 1, 3), x, by = g, test = "midp"),
      "assoc_note"
    ),
    "Comparaison des groupes\u00a0: test mid-p de Fisher."
  )
  set.seed(4)
  expect_identical(
    attr(
      table_categorical(
        .mp_mixed(),
        c(x, z),
        by = g,
        test = "midp",
        simulate_p = TRUE,
        assoc_measure = "none"
      ),
      "assoc_note"
    ),
    paste0(
      "Comparaison des groupes\u00a0: test du khi-deux de Pearson, ",
      "simul\u00e9 (z)\u00a0; test mid-p de Fisher (x)."
    )
  )
})
