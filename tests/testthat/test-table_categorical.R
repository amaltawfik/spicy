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

test_that("table_categorical default output prints ASCII and returns styled object", {
  printed <- capture.output(
    out <- table_categorical(
      sochealth,
      select = smoking,
      output = "default"
    )
  )

  expect_true(length(printed) > 0)
  expect_s3_class(out, "spicy_categorical_table")
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
  # The note lists display labels; sochealth ships label attributes,
  # picked up by the 0.13.0 attribute fallback.
  expect_match(
    attr(out_default, "assoc_note"),
    "Note\\. Phi: Current smoker; Cramer's V: Highest education level\\."
  )

  # Force uniform Cramer's V via single-string -> no note
  out_uniform <- table_categorical(
    sochealth,
    select = c(smoking, education),
    by = sex,
    assoc_measure = "cramer_v",
    output = "default"
  )
  expect_null(attr(out_uniform, "assoc_note"))
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
  # sochealth ships label attributes, picked up by the fallback.
  expect_true(all(
    c("Current smoker", "Regular physical activity") %in% out$Variable
  ))
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

test_that("table_categorical grouped default output prints and returns invisibly", {
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
    "flextable"
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
    "flextable"
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

# ---- harmonisation with table_continuous() / _lm() (Phase 2) -------------

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
    "^Missing values removed: smoking \\(65\\)\\."
  )
  expect_match(
    lines[length(lines)],
    "^Note\\. Phi: Current smoker; Cramer's V: Highest education level\\."
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
  # `outcome` carries the display labels; sochealth ships label
  # attributes, picked up by the 0.13.0 attribute fallback.
  expect_equal(
    unique(td$outcome),
    c("Current smoker", "Regular physical activity")
  )
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
  expect_true(any(grepl("^<,0001$", p_col)))
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

# ---- labels: named-only contract + attribute fallback (0.13.0) ------------

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
    "must be a named character vector"
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

test_that("labels fall back to the label attribute (haven-style)", {
  df <- data.frame(
    smk = structure(
      c("No", "Yes", "No", "Yes"),
      label = "Current smoker"
    ),
    act = c("Low", "High", "Low", "High"),
    stringsAsFactors = FALSE
  )
  out <- table_categorical(df, select = c(smk, act), output = "long")
  expect_setequal(unique(out$variable), c("Current smoker", "act"))

  # An explicit named label overrides the attribute.
  out2 <- table_categorical(
    df,
    select = c(smk, act),
    labels = c(smk = "Override"),
    output = "long"
  )
  expect_setequal(unique(out2$variable), c("Override", "act"))
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

test_that("named labels relabel a subset; others fall back to the attribute label", {
  out <- table_categorical(
    sochealth,
    select = c(smoking, physical_activity),
    labels = c(smoking = "CS only"),
    output = "long"
  )
  # physical_activity keeps its sochealth label attribute.
  expect_setequal(
    unique(out$variable),
    c("CS only", "Regular physical activity")
  )
})

test_that("named labels with unknown names error clearly", {
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      labels = c(bogus = "X")
    ),
    "Names in `labels` not found in `data`"
  )
})

test_that("non-character labels rejected at boundary", {
  expect_error(
    table_categorical(sochealth, select = smoking, labels = 123),
    "must be a named character vector"
  )
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
    drop_na = TRUE
  )
  note <- attr(out, "missing_note")
  expect_identical(
    note,
    "Missing values removed: income_group (18), smoking (25)."
  )
  printed <- paste(
    capture.output(print(out)),
    collapse = "
"
  )
  expect_match(
    printed,
    "Missing values removed: income_group (18)",
    fixed = TRUE
  )
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
  lg <- table_categorical(d, select = x, by = g, weights = w, output = "long")
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
  out <- table_categorical(d, select = x, by = g, weights = w)
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

test_that("tidy/glance column sets are frozen (stabilising contract)", {
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
    "Missing values removed: income_group (18), smoking (25)."
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
  expect_match(note, "Missing values removed: smoking (25).", fixed = TRUE)
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
    "Missing values removed: income_group (18), smoking (25)."
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
    attr(out, "missing_note"),
    "Missing values removed: income_group (18), smoking (25)."
  )

  out_by <- table_categorical(
    d,
    select = smoking,
    by = sex_na,
    drop_na = TRUE,
    output = "data.frame"
  )
  expect_match(
    attr(out_by, "missing_note"),
    "Rows with missing sex_na removed: 40.",
    fixed = TRUE
  )

  clean <- table_categorical(
    data.frame(v = factor(c("a", "b", "a"))),
    select = v,
    drop_na = TRUE,
    output = "data.frame"
  )
  expect_null(attr(clean, "missing_note"))
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
  # The rule reaches the quote-bearing column: sass renormalises the
  # escaped double quote to a single-quoted CSS string, which matches
  # the id gt wrote into the DOM.
  expect_true(grepl("th[id='a\"b_n']", html, fixed = TRUE))
  expect_true(grepl('scope="col" id="a&quot;b_n"', html, fixed = TRUE))
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
