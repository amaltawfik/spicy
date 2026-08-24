# table_outcome(): one continuous outcome across the levels of several
# categorical variables, one block per variable.

test_that("the title names the outcome and nothing else", {
  # Decision 32: the grouping variables ARE the rows, so a title that
  # listed them would repeat the stub.
  expect_identical(
    spicy:::.outcome_title("Body mass index"),
    "Descriptive statistics of Body mass index"
  )
  # One `select` or six, the title is the same: the geometry is what
  # changes, not the subject of the table.
  expect_identical(
    spicy:::.outcome_title("Age (years)"),
    "Descriptive statistics of Age (years)"
  )
})

test_that("the Excel sheet name is resolved from the registry", {
  # Decision 16: `excel_sheet = NULL` keeps the \usage line clean.
  expect_identical(spicy:::.outcome_excel_sheet(NULL), "Outcome")
  expect_identical(spicy:::.outcome_excel_sheet("Mine"), "Mine")
})

test_that("the structure notes say what the table does not adjust", {
  notes <- spicy:::.outcome_structure_notes("Body mass index", TRUE, TRUE)
  expect_length(notes, 2L)
  expect_match(notes[[1L]], "Body mass index", fixed = TRUE)
  expect_match(notes[[1L]], "not adjusted for one another", fixed = TRUE)
  expect_match(notes[[2L]], "whole analytic sample", fixed = TRUE)

  # Owed only when there is something to disclose.
  expect_identical(
    spicy:::.outcome_structure_notes("x", FALSE, TRUE),
    spicy_str("note_outcome_overall")
  )
  expect_identical(
    spicy:::.outcome_structure_notes("x", FALSE, FALSE),
    NULL
  )
})

test_that("Overall is not the word Total, and that is deliberate", {
  # Decision 32bis. "Total" is the word of a COUNT margin, where
  # frequencies add up; this row is the whole analytic sample, where a
  # mean is recomputed and nothing is added.
  expect_identical(spicy:::.outcome_overall_label(), "Overall")
  expect_identical(spicy_str("label_total"), "Total")
  expect_false(identical(
    spicy_str("row_overall"),
    spicy_str("header_margin_total")
  ))
})


# ============================================================================
# The compute frame: one block per `select` variable
# ============================================================================

# The worked example of the spec: eight observations, a two-level
# grouping and a three-level one, chosen so every statistic separates.
.to_y <- function() c(1, 2, 3, 4, 10, 20, 30, 40)
.to_g1 <- function() c("a", "a", "a", "a", "b", "b", "b", "b")
.to_g2 <- function() c("x", "x", "y", "y", "z", "z", "x", "y")

.to_compute <- function(..., y = .to_y()) {
  suppressWarnings(spicy:::.outcome_compute(
    outcome = y,
    select_list = list(g1 = .to_g1(), g2 = .to_g2()),
    select_labels = c(g1 = "G one", g2 = "G two"),
    outcome_name = "y",
    outcome_label = "Y",
    ...
  ))
}

test_that("the frame lays the blocks out in display order", {
  r <- .to_compute()
  expect_identical(
    r$.row_role,
    c(
      "summary",
      "factor_header",
      "level",
      "level",
      "factor_header",
      "level",
      "level",
      "level"
    )
  )
  expect_identical(r$variable, c("y", rep("g1", 3L), rep("g2", 4L)))
  expect_identical(r$level, c(NA, NA, "a", "b", NA, "x", "y", "z"))
  # The marginal row is the OUTCOME's row: it carries its name and its
  # label, not a grouping's.
  expect_identical(r$label[[1L]], "Y")
  # A header row carries no descriptive cell, a level row no block
  # statistic: two structural blanks, not two undefined values.
  expect_true(all(is.na(r$mean[r$.row_role == "factor_header"])))
  expect_true(all(is.na(r$p.value[r$.row_role != "factor_header"])))
})

test_that("the descriptive cells match hand-computed values", {
  r <- .to_compute()
  # Marginal row.
  expect_equal(r$mean[[1L]], 13.75, tolerance = 1e-12)
  expect_equal(r$sd[[1L]], 14.723644735109383, tolerance = 1e-12)
  expect_identical(r$n[[1L]], 8L)
  expect_equal(r$ci_lower[[1L]], 1.4407249588054114, tolerance = 1e-12)
  expect_equal(r$ci_upper[[1L]], 26.059275041194589, tolerance = 1e-12)
  expect_equal(r$median[[1L]], 7, tolerance = 1e-12)
  expect_equal(r$q1[[1L]], 2.75, tolerance = 1e-12)
  expect_equal(r$q3[[1L]], 22.5, tolerance = 1e-12)
  expect_equal(r$iqr[[1L]], 19.75, tolerance = 1e-12)

  # Level "a": four observations 1..4.
  a <- which(r$variable == "g1" & r$level == "a")
  expect_equal(r$mean[a], 2.5, tolerance = 1e-12)
  expect_equal(r$sd[a], 1.2909944487358056, tolerance = 1e-12)
  expect_equal(r$min[a], 1, tolerance = 1e-12)
  expect_equal(r$max[a], 4, tolerance = 1e-12)
  expect_identical(r$n[a], 4L)
  expect_equal(r$ci_lower[a], 0.44573974323947940, tolerance = 1e-12)
  expect_equal(r$ci_upper[a], 4.5542602567605206, tolerance = 1e-12)

  # Level "b".
  b <- which(r$variable == "g1" & r$level == "b")
  expect_equal(r$mean[b], 25, tolerance = 1e-12)
  expect_equal(r$sd[b], 12.909944487358056, tolerance = 1e-12)
})

test_that("the order-statistic median interval is undefined at n = 4", {
  # Distribution-free coverage is discrete: with four observations even
  # the full range does not reach 95%, so the interval is NA rather
  # than a false one. The marginal row's eight observations do reach it.
  r <- .to_compute()
  expect_equal(r$med_ci_lower[[1L]], 1, tolerance = 1e-12)
  expect_equal(r$med_ci_upper[[1L]], 40, tolerance = 1e-12)
  a <- which(r$variable == "g1" & r$level == "a")
  expect_true(is.na(r$med_ci_lower[a]))
  expect_true(is.na(r$med_ci_upper[a]))
})

test_that("each block carries its own one-way comparison", {
  r <- .to_compute(do_test = TRUE, do_es = TRUE, effect_size = "auto")
  h1 <- which(r$variable == "g1" & r$.row_role == "factor_header")
  expect_identical(r$test_type[h1], "welch_t")
  expect_equal(r$statistic[h1], -3.4683862198862792, tolerance = 1e-12)
  expect_equal(r$df1[h1], 3.0599940005999398, tolerance = 1e-12)
  expect_true(is.na(r$df2[h1]))
  expect_equal(r$p.value[h1], 0.039167916188933373, tolerance = 1e-12)
  expect_identical(r$es_type[h1], "hedges_g")
  expect_equal(r$es_value[h1], -2.1326255790048378, tolerance = 1e-12)

  # Three levels: the same `test = "welch"` becomes the Welch one-way
  # ANOVA, and "auto" follows it to eta-squared.
  h2 <- which(r$variable == "g2" & r$.row_role == "factor_header")
  expect_identical(r$test_type[h2], "welch_anova")
  expect_equal(r$statistic[h2], 0.063490520518212523, tolerance = 1e-12)
  expect_equal(r$df1[h2], 2, tolerance = 1e-12)
  expect_equal(r$df2[h2], 3.2843793820103295, tolerance = 1e-12)
  expect_equal(r$p.value[h2], 0.93960667111306229, tolerance = 1e-12)
  expect_identical(r$es_type[h2], "eta_sq")
  expect_equal(r$es_value[h2], 0.024272377814387697, tolerance = 1e-12)
})

test_that("the rank family answers on the same blocks", {
  r <- .to_compute(
    do_test = TRUE,
    do_es = TRUE,
    effect_size = "auto",
    test = "nonparametric"
  )
  h2 <- which(r$variable == "g2" & r$.row_role == "factor_header")
  expect_identical(r$test_type[h2], "kruskal")
  expect_equal(r$statistic[h2], 1.1388888888888893, tolerance = 1e-12)
  expect_equal(r$df1[h2], 2, tolerance = 1e-12)
  expect_equal(r$p.value[h2], 0.56583970678742257, tolerance = 1e-12)
  expect_identical(r$es_type[h2], "epsilon_sq")
})

test_that("the block counts sum to the marginal count", {
  # The invariant that makes the marginal row a legitimate denominator,
  # under the condition that makes it exact: `drop_na = FALSE`, so the
  # levels of a block PARTITION the outcome-complete sample -- the
  # missing display level included.
  y <- c(1, 2, NA, 4, 10, NA, 30, 40)
  g_na <- c("a", "a", "a", NA, "b", "b", NA, "b")
  r <- suppressWarnings(spicy:::.outcome_compute(
    outcome = y,
    select_list = list(g = g_na, h = .to_g2()),
    select_labels = c(g = "G", h = "H"),
    outcome_name = "y",
    outcome_label = "Y",
    drop_na = FALSE
  ))
  overall_n <- r$n[r$.row_role == "summary"]
  expect_identical(overall_n, 6L)
  for (v in c("g", "h")) {
    expect_identical(
      sum(r$n[r$variable == v & r$.row_role %in% c("level", "missing")]),
      overall_n
    )
  }
  # The missing `select` values are a display level of their own, and the
  # role is the KEY -- not the label.
  miss <- r[r$variable == "g" & r$.row_role == "missing", ]
  expect_identical(nrow(miss), 1L)
  expect_identical(miss$level, "(Missing)")
  # Two rows have a missing `select` value and an observed outcome; both
  # count, which is the point of showing the level.
  expect_identical(miss$n, 2L)
})

test_that("drop_na = TRUE removes the missing rows block by block", {
  y <- c(1, 2, 3, 4, 10, 20, 30, 40)
  g_na <- c("a", "a", "a", NA, "b", "b", NA, "b")
  r <- suppressWarnings(spicy:::.outcome_compute(
    outcome = y,
    select_list = list(g = g_na),
    select_labels = c(g = "G"),
    outcome_name = "y",
    outcome_label = "Y",
    drop_na = TRUE
  ))
  expect_identical(r$n[r$.row_role == "summary"], 8L)
  expect_identical(sum(r$n[r$.row_role == "level"]), 6L)
  expect_false(any(r$.row_role == "missing"))
  expect_identical(unname(attr(r, "select_na_dropped")[["g"]]), 2L)
})

test_that("a block too thin to compare degrades on its own", {
  # One block cannot be tested (a single observed level), the other
  # can. The thin one keeps NA test columns; the other is untouched.
  r <- suppressWarnings(spicy:::.outcome_compute(
    outcome = .to_y(),
    select_list = list(one = rep("only", 8L), g1 = .to_g1()),
    select_labels = c(one = "One", g1 = "G one"),
    outcome_name = "y",
    outcome_label = "Y",
    do_test = TRUE
  ))
  h_one <- which(r$variable == "one" & r$.row_role == "factor_header")
  h_g1 <- which(r$variable == "g1" & r$.row_role == "factor_header")
  expect_true(is.na(r$p.value[h_one]))
  expect_false(is.na(r$p.value[h_g1]))
  expect_true(is.na(attr(r, "test_used")[["one"]]))
  expect_identical(unname(attr(r, "test_used")[["g1"]]), "welch")
})

test_that("a declared but empty level keeps its row", {
  # A level nobody chose is information about the instrument, so a
  # factor keeps its declared order and its empty levels.
  g <- factor(
    c("a", "a", "a", "a", "b", "b", "b", "b"),
    levels = c("b", "a", "never")
  )
  r <- suppressWarnings(spicy:::.outcome_compute(
    outcome = .to_y(),
    select_list = list(g = g),
    select_labels = c(g = "G"),
    outcome_name = "y",
    outcome_label = "Y"
  ))
  levels_shown <- r$level[r$.row_role == "level"]
  expect_identical(levels_shown, c("b", "a", "never"))
  empty <- which(r$level == "never")
  expect_identical(r$n[empty], 0L)
  expect_true(is.na(r$mean[empty]))
})

test_that("a single observation leaves the SD undefined, not zero", {
  r <- suppressWarnings(spicy:::.outcome_compute(
    outcome = c(1, 2, 3, 4, 10, 20, 30, 40),
    select_list = list(g = c("a", "a", "a", "a", "a", "a", "a", "solo")),
    select_labels = c(g = "G"),
    outcome_name = "y",
    outcome_label = "Y"
  ))
  solo <- which(r$level == "solo")
  expect_identical(r$n[solo], 1L)
  expect_equal(r$mean[solo], 40, tolerance = 1e-12)
  expect_true(is.na(r$sd[solo]))
  expect_true(is.na(r$ci_lower[solo]))
})

test_that("a `select` with no observed level yields no level rows", {
  r <- suppressWarnings(spicy:::.outcome_compute(
    outcome = .to_y(),
    select_list = list(
      g = factor(rep(NA_character_, 8L), levels = character(0))
    ),
    select_labels = c(g = "G"),
    outcome_name = "y",
    outcome_label = "Y",
    drop_na = TRUE
  ))
  expect_identical(sum(r$.row_role == "factor_header"), 1L)
  expect_identical(sum(r$.row_role %in% c("level", "missing")), 0L)
})

test_that("overall = FALSE drops the marginal row and nothing else", {
  with_overall <- .to_compute()
  without <- .to_compute(overall = FALSE)
  expect_identical(nrow(without), nrow(with_overall) - 1L)
  expect_false(any(without$.row_role == "summary"))
  expect_equal(
    without$mean,
    with_overall$mean[-1L],
    tolerance = 1e-12
  )
})


# ============================================================================
# The display frame and the console
# ============================================================================

.to_sh <- function() as.data.frame(spicy::sochealth)

.to_quiet <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(expr)))
  res
}

test_that("the console lays out one stub column and indented levels", {
  tbl <- .to_quiet(table_outcome(.to_sh(), bmi, select = c(sex, smoking)))
  df <- attr(tbl, "display_df")
  expect_identical(names(df)[[1L]], "Variable")
  expect_identical(
    df$Variable,
    c(
      "Overall",
      "Sex",
      "  Female",
      "  Male",
      "Current smoker",
      "  No",
      "  Yes",
      "  (Missing)"
    )
  )
  # The marginal row is NOT indented: it is the whole sample, not a
  # level of the block below it.
  expect_false(startsWith(df$Variable[[1L]], " "))
})

test_that("a statistic sits on the row it belongs to", {
  tbl <- .to_quiet(table_outcome(
    .to_sh(),
    bmi,
    select = c(sex, smoking),
    statistic = TRUE,
    effect_size = "auto"
  ))
  df <- attr(tbl, "display_df")
  header <- which(tbl$.row_role == "factor_header")
  levels_i <- which(tbl$.row_role %in% c("level", "missing"))
  overall <- which(tbl$.row_role == "summary")

  # Block statistics: on the header rows, and blank everywhere else --
  # the marginal row included, which is not a group comparison.
  expect_true(all(nzchar(df$p[header])))
  expect_true(all(!nzchar(df$p[levels_i])))
  expect_true(all(!nzchar(df$p[overall])))
  expect_true(all(nzchar(df$Test[header])))
  expect_true(all(nzchar(df$ES[header])))

  # Outcome statistics: on the level rows and the marginal row, blank
  # on the headers -- a structural blank, NOT the undefined dash.
  expect_true(all(nzchar(df$M[levels_i])))
  expect_true(all(nzchar(df$M[overall])))
  expect_true(all(identical(unique(df$M[header]), "")))
  expect_true(all(identical(unique(df$n[header]), "")))
  expect_false(any(grepl(
    spicy_str("cell_undefined"),
    df$M[header],
    fixed = TRUE
  )))
})

test_that("a rule opens every block, the first one included", {
  tbl <- .to_quiet(table_outcome(.to_sh(), bmi, select = c(sex, smoking)))
  geom <- spicy:::.outcome_body_geometry(tbl)
  # Rows 2 and 5 open a block. The rule above row 2 is the one a
  # rank-based predicate would drop -- the table opens on a `summary`
  # row, not on a header.
  expect_identical(
    spicy:::.struct_block_sep_rows(list(body = geom)),
    c(2L, 5L)
  )
  expect_identical(
    spicy:::.struct_indent_rows(list(body = geom)),
    c(3L, 4L, 6L, 7L, 8L)
  )
})

test_that("the note discloses the comparison, the missing and the shape", {
  tbl <- .to_quiet(table_outcome(.to_sh(), bmi, select = c(sex, region)))
  note <- attr(tbl, "note")
  # The outcome's own missing values, once, globally -- this is the
  # sentence that reconciles the Overall count with the raw data.
  expect_match(note, "Missing values removed: bmi (12).", fixed = TRUE)
  # Two blocks, two different tests: the note names both.
  expect_match(note, "Welch one-way ANOVA (region)", fixed = TRUE)
  expect_match(note, "Welch t-test (sex)", fixed = TRUE)
  # The honest sentence about what the table does NOT do.
  expect_match(note, "not adjusted for one another", fixed = TRUE)
  expect_match(note, "Overall = the whole analytic sample", fixed = TRUE)
})

test_that("the disclosures follow the arguments that cause them", {
  d <- .to_sh()
  # `drop_na = TRUE`: one sentence per `select` variable that lost rows.
  tbl <- .to_quiet(table_outcome(d, bmi, select = smoking, drop_na = TRUE))
  expect_match(attr(tbl, "note"), "Rows with missing smoking removed: 25.")
  # `overall = FALSE`: no sentence about a row that is not there.
  tbl2 <- .to_quiet(table_outcome(d, bmi, select = sex, overall = FALSE))
  expect_false(grepl("whole analytic sample", attr(tbl2, "note"), fixed = TRUE))
  # No comparison: no sentence about blocks that compare nothing.
  tbl3 <- .to_quiet(table_outcome(d, bmi, select = sex, p_value = FALSE))
  expect_false(grepl("not adjusted", attr(tbl3, "note"), fixed = TRUE))
  expect_false(grepl("Group comparison", attr(tbl3, "note"), fixed = TRUE))
})

test_that("the table tests what it shows", {
  # A median without a mean switches the default to the rank family,
  # globally -- there is one outcome, so this is a scalar decision.
  tbl <- .to_quiet(table_outcome(
    .to_sh(),
    bmi,
    select = sex,
    show_columns = c("med_iqr", "n")
  ))
  expect_match(attr(tbl, "note"), "Wilcoxon rank-sum test", fixed = TRUE)
  # An explicit `test` is sovereign, and says so.
  expect_warning(
    table_outcome(
      .to_sh(),
      bmi,
      select = sex,
      show_columns = c("med_iqr", "n"),
      test = "welch"
    ),
    class = "spicy_caveat"
  )
})

test_that("refusals are classed and name the cause", {
  d <- .to_sh()
  err <- tryCatch(table_outcome(d, sex, select = smoking), error = identity)
  # NOT `invalid_input`: a categorical outcome is a shape the API must
  # keep room for, not a mistake.
  expect_s3_class(err, "spicy_not_implemented")
  expect_match(conditionMessage(err), "table_categorical()", fixed = TRUE)

  err <- tryCatch(table_outcome(d, c(bmi, age), select = sex), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "table_continuous(select = , by = )",
    fixed = TRUE
  )

  # The membership guard: without it a typo travels as a NULL column
  # and fails far from its cause.
  err <- tryCatch(table_outcome(d, bmi, select = c("sexe")), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "sexe", fixed = TRUE)
  expect_match(conditionMessage(err), "Available:", fixed = TRUE)

  err <- tryCatch(table_outcome(d, bmi, select = NULL), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "at least one column", fixed = TRUE)

  err <- tryCatch(table_outcome(d, bmi, select = c(sex, bmi)), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "cannot contain the outcome",
    fixed = TRUE
  )

  err <- tryCatch(
    table_outcome(d, bmi, select = sex, show_columns = list(bmi = "m")),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "character vector", fixed = TRUE)

  err <- tryCatch(
    table_outcome(d, bmi, select = sex, show_columns = c("m", "weighted_n")),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
})

test_that("the renamed `by` errors with the migration hint", {
  # Decision 39. `by` was the third formal until 0.13.0; it now names
  # nothing, and it comes back in a later release as the COMPARED
  # group. A silent alias would mean two readings of one argument, so
  # the old spelling is a hard, classed error -- never R's bare
  # "unused argument".
  d <- .to_sh()
  err <- tryCatch(
    table_outcome(d, bmi, by = c(sex, smoking)),
    error = identity
  )
  expect_s3_class(err, "spicy_defunct")
  expect_s3_class(err, "spicy_invalid_input")
  expect_s3_class(err, "spicy_error")
  expect_match(conditionMessage(err), "renamed `select`", fixed = TRUE)
  expect_match(
    conditionMessage(err),
    "table_outcome(data, outcome, select = ...)",
    fixed = TRUE
  )

  # It fires BEFORE any other validation, so the migration message is
  # what the user sees even when the rest of the call is also wrong.
  err2 <- tryCatch(
    table_outcome(d, bmi, by = c(sex), ci_level = 95),
    error = identity
  )
  expect_s3_class(err2, "spicy_defunct")

  # `select` is the only spelling; the third position still binds it.
  named <- .to_quiet(table_outcome(d, bmi, select = c(sex, smoking)))
  positional <- .to_quiet(table_outcome(d, bmi, c(sex, smoking)))
  expect_identical(
    attr(named, "display_df"),
    attr(positional, "display_df")
  )
  expect_identical(attr(named, "select"), c("sex", "smoking"))
})

test_that("a high-cardinality grouping is announced, never refused", {
  # The family refuses no numeric `select`; it says what it sees. The
  # threshold is arbitrary and the Rd says so.
  expect_warning(
    tbl <- table_outcome(.to_sh(), bmi, select = age),
    class = "spicy_caveat"
  )
  expect_s3_class(tbl, "spicy_outcome_table")
})

test_that("the raw outputs are the plain compute frame", {
  d <- .to_sh()
  raw <- .to_quiet(table_outcome(d, bmi, select = sex, output = "long"))
  expect_s3_class(raw, "data.frame")
  expect_false(inherits(raw, "spicy_outcome_table"))
  expect_true(all(
    c("variable", "label", "level", "mean", "sd", "n", "p.value") %in%
      names(raw)
  ))
  # The schema is FIXED: the SMD columns are present and NA from the
  # first version, so adding the statistic later cannot break a
  # pipeline that indexes into the frame.
  expect_true(all(c("smd_type", "smd_value") %in% names(raw)))
  expect_true(all(is.na(raw$smd_value)))
  expect_identical(
    .to_quiet(table_outcome(d, bmi, select = sex, output = "data.frame")),
    raw
  )
})

test_that("the console shape is pinned", {
  skip_on_cran()
  d <- .to_sh()
  expect_snapshot(print(suppressWarnings(
    table_outcome(d, bmi, select = c(sex, smoking))
  )))
  expect_snapshot(print(suppressWarnings(
    table_outcome(
      d,
      bmi,
      select = c(sex, region),
      statistic = TRUE,
      effect_size = "auto"
    )
  )))
  expect_snapshot(print(suppressWarnings(
    table_outcome(
      d,
      bmi,
      select = sex,
      overall = FALSE,
      show_columns = c("med_iqr", "n")
    )
  )))
})


# ============================================================================
# State and disclosure: what the object prints, and what the note promises
# ============================================================================

test_that("printing a subset prints the subset", {
  # `[.data.frame` copies every attribute onto the result, including
  # the cached display frame. `print(x[1:4, ])` used to render the
  # eight ORIGINAL rows -- and the block rules were recomputed from the
  # four-row subset, so the printed body did not even agree with its
  # own rules.
  x <- .to_quiet(table_outcome(.to_sh(), bmi, select = c(sex, smoking)))
  body_lines <- function(obj) {
    lines <- utils::capture.output(print(obj))
    # Body rows are the ones with the stub separator, minus the header.
    sum(grepl("\u2502", lines, fixed = TRUE)) - 1L
  }
  expect_identical(body_lines(x), 8L)
  expect_identical(body_lines(x[1:4, ]), 4L)
  expect_identical(body_lines(utils::head(x, 3L)), 3L)
  # The rebuilt frame is the subset's own.
  expect_identical(nrow(spicy:::.outcome_rendered_df(x[1:4, ])), 4L)
  # And an untouched object still uses its cache.
  expect_identical(
    spicy:::.outcome_rendered_df(x),
    attr(x, "display_df", exact = TRUE)
  )
})

test_that("the block note is owed only when a block compared", {
  # `.outcome_test_note()` reads the RESULT; this half read the
  # REQUEST, so a table whose every block was too thin to compare
  # printed an empty `p` column under a note promising that "each
  # block compares ...".
  d <- data.frame(
    score = c(4, 5, 6, 7, 9, 11, 12, 14, 20),
    arm = c("A", "A", "A", "A", "B", "B", "B", "B", "C"),
    stringsAsFactors = FALSE
  )
  thin <- .to_quiet(table_outcome(d, score, select = arm))
  # Nothing was compared: the p column is empty top to bottom.
  expect_true(all(!nzchar(attr(thin, "display_df")$p)))
  expect_false(grepl("Each block compares", attr(thin, "note"), fixed = TRUE))
  expect_false(grepl("Group comparison", attr(thin, "note"), fixed = TRUE))
  # The marginal-row gloss is a different sentence and stays.
  expect_match(attr(thin, "note"), "whole analytic sample", fixed = TRUE)

  # A table that did compare still says so.
  real <- .to_quiet(table_outcome(.to_sh(), bmi, select = sex))
  expect_match(attr(real, "note"), "Each block compares", fixed = TRUE)
  expect_match(attr(real, "note"), "Group comparison", fixed = TRUE)
})


# ---- A missing `outcome` / `select` is refused in spicy's own words -----

test_that("a missing `select` is a classed refusal, not a missingArgError", {
  # Neither formal has a default: one names the variable described, the
  # other the blocks it is described across. Left to lazy evaluation the
  # call died deep inside tidyselect on base R's own
  # `missingArgError` -- "argument \"expr\" is missing", translated by
  # the session's locale and naming a variable that appears nowhere in
  # the user's call.
  d <- .to_sh()
  err <- tryCatch(table_outcome(d, bmi), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_s3_class(err, "spicy_error")
  expect_false(inherits(err, "missingArgError"))
  expect_match(conditionMessage(err), "`select` is required", fixed = TRUE)
  expect_match(conditionMessage(err), "select = c(sex, smoking)", fixed = TRUE)
})

test_that("a missing `outcome` is refused the same way", {
  d <- .to_sh()
  err <- tryCatch(table_outcome(d, select = sex), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_false(inherits(err, "missingArgError"))
  expect_match(conditionMessage(err), "`outcome` is required", fixed = TRUE)

  # Both missing: the outcome is named first, because it is the subject.
  err_both <- tryCatch(table_outcome(d), error = identity)
  expect_match(conditionMessage(err_both), "`outcome` is required", fixed = TRUE)
})

test_that("the `by` migration still fires before the missing-arg guards", {
  d <- .to_sh()
  err <- tryCatch(table_outcome(d, by = c(sex)), error = identity)
  expect_s3_class(err, "spicy_defunct")
})
