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
  expect_identical(spicy_str("row_overall"), "Overall")
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
  expect_identical(r$es_type[h2], "eta2")
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
  expect_identical(r$es_type[h2], "epsilon2")
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

test_that("the object comes back visibly, and assigning it says nothing", {
  # Decision 47, the family invariant: a bare call renders through the
  # print method (capture.output() evaluates as at top level, so the
  # visible return auto-prints), an assignment writes nothing. This
  # family was already conformant; the witness pins it.
  d <- .to_sh()
  bare <- suppressWarnings(
    utils::capture.output(table_outcome(d, bmi, select = c(sex, smoking)))
  )
  expect_true(length(bare) > 0)

  assigned <- suppressWarnings(
    utils::capture.output(
      tbl <- table_outcome(d, bmi, select = c(sex, smoking))
    )
  )
  expect_identical(assigned, character(0))
  expect_s3_class(tbl, "spicy_outcome_table")
  expect_identical(utils::capture.output(print(tbl)), bare)
})

test_that("the console lays out one stub column and indented levels", {
  tbl <- .to_quiet(table_outcome(.to_sh(), bmi, select = c(sex, smoking)))
  df <- attr(tbl, "display_df")
  expect_identical(names(df)[[1L]], "Variable")
  expect_identical(
    df$Variable,
    c(
      "Overall",
      "sex",
      "  Female",
      "  Male",
      "smoking",
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
  expect_match(
    note,
    "Missing values removed: bmi (12).",
    fixed = TRUE
  )
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
  expect_match(
    attr(tbl, "note"),
    "Rows with missing smoking removed: 25."
  )
  # `overall = FALSE`: no sentence about a row that is not there.
  tbl2 <- .to_quiet(table_outcome(d, bmi, select = sex, overall = FALSE))
  expect_false(grepl("whole analytic sample", attr(tbl2, "note"), fixed = TRUE))
  # No comparison: no sentence about blocks that compare nothing.
  tbl3 <- .to_quiet(table_outcome(d, bmi, select = sex, p_value = FALSE))
  expect_false(grepl("not adjusted", attr(tbl3, "note"), fixed = TRUE))
  expect_false(grepl("Group comparison", attr(tbl3, "note"), fixed = TRUE))
})

test_that("a select displayed as Overall is refused, with both ways out", {
  d <- data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    g = c("a", "a", "b", "b", "a", "b")
  )
  err <- expect_error(
    table_outcome(d, y, select = g, labels = c(g = "Overall")),
    class = "spicy_label_collision"
  )
  expect_s3_class(err, "spicy_invalid_input")
  msg <- conditionMessage(err)
  expect_match(msg, "The label \"Overall\" of `g`", fixed = TRUE)
  expect_match(msg, "the row spicy adds for the whole sample", fixed = TRUE)
  expect_match(msg, "Choose another label in `labels`", fixed = TRUE)
  expect_match(msg, "row_overall = \"Whole sample\"", fixed = TRUE)
  # A column named Overall collides the same way, and without the row
  # nothing does.
  named <- data.frame(y = d$y, Overall = d$g)
  expect_error(
    table_outcome(named, y, select = Overall),
    class = "spicy_label_collision"
  )
  expect_identical(
    attr(
      .to_quiet(table_outcome(named, y, select = Overall, overall = FALSE)),
      "display_df"
    )$Variable,
    c("Overall", "  a", "  b")
  )

  # Way out 1: another label in `labels`.
  ok <- .to_quiet(table_outcome(d, y, select = g, labels = c(g = "Group")))
  expect_identical(
    attr(ok, "display_df")$Variable,
    c("Overall", "Group", "  a", "  b")
  )

  # Way out 2: another label for the row. The note names the row as the
  # table shows it.
  withr::with_options(
    list(spicy.labels = list(row_overall = "Whole sample")),
    {
      tbl <- .to_quiet(table_outcome(
        d,
        y,
        select = g,
        labels = c(g = "Overall")
      ))
      expect_identical(
        attr(tbl, "display_df")$Variable,
        c("Whole sample", "Overall", "  a", "  b")
      )
      expect_match(
        attr(tbl, "note"),
        "Whole sample = the whole analytic sample.",
        fixed = TRUE
      )
      expect_identical(inline(tbl, "Whole sample"), inline(tbl, y))
      expect_identical(inline(tbl, "Overall", "a"), inline(tbl, g, "a"))
    }
  )
  # An override of the fixed sentence, written for 0.13.0, still applies,
  # with or without a relabeled row.
  withr::with_options(
    list(spicy.labels = list(note_outcome_overall = "Overall: everyone.")),
    expect_match(
      attr(.to_quiet(table_outcome(d, y, select = g)), "note"),
      "Overall: everyone.",
      fixed = TRUE
    )
  )
  withr::with_options(
    list(
      spicy.labels = list(
        row_overall = "All",
        note_outcome_overall = "All: everyone."
      )
    ),
    expect_match(
      attr(.to_quiet(table_outcome(d, y, select = g)), "note"),
      "All: everyone.",
      fixed = TRUE
    )
  )

  withr::local_options(spicy.language = "fr")
  expect_error(
    table_outcome(d, y, select = g, labels = c(g = "Ensemble")),
    class = "spicy_label_collision"
  )
  # The English label is not displayed in French: nothing collides.
  fr <- .to_quiet(table_outcome(d, y, select = g, labels = c(g = "Overall")))
  expect_identical(
    attr(fr, "display_df")$Variable,
    c("Ensemble", "Overall", "  a", "  b")
  )
  expect_match(attr(fr, "note"), "Ensemble = l'", fixed = TRUE)
  withr::local_options(spicy.labels = list(row_overall = "Tous"))
  fr2 <- .to_quiet(table_outcome(d, y, select = g, labels = c(g = "Ensemble")))
  expect_match(attr(fr2, "note"), " Tous = l'", fixed = TRUE)
})

test_that("a level displayed beside the row of missing values is refused", {
  # Every declared level of a block is displayed, so a declared
  # "(Missing)" collides too.
  for (g in list(
    c("(Missing)", "a", NA, "a", "b", "b"),
    factor(c("b", "a", NA, "a", "b", "b"), levels = c("a", "b", "(Missing)"))
  )) {
    d <- data.frame(y = 1:6, g = g)
    err <- expect_error(
      table_outcome(d, y, select = g),
      class = "spicy_label_collision"
    )
    expect_match(
      conditionMessage(err),
      "The level \"(Missing)\" of `g` has the label of the row spicy adds",
      fixed = TRUE
    )
    expect_s3_class(
      .to_quiet(table_outcome(d, y, select = g, drop_na = TRUE)),
      "spicy_outcome_table"
    )
    withr::with_options(
      list(spicy.labels = list(row_missing_level = "(Not stated)")),
      {
        tbl <- .to_quiet(table_outcome(d, y, select = g))
        lv <- tbl$level[tbl$.row_role %in% c("level", "missing")]
        expect_true(all(c("(Missing)", "(Not stated)") %in% lv))
        expect_identical(
          tbl$.row_role[tbl$level %in% "(Not stated)"],
          "missing"
        )
      }
    )
  }
})

test_that("the note opens with \"Note.\" once, on the attribute and in print", {
  tbl <- .to_quiet(table_outcome(.to_sh(), bmi, select = c(sex, region)))
  note <- attr(tbl, "note")
  expect_true(startsWith(note, "Note. Missing values removed: bmi (12). "))
  expect_identical(lengths(gregexpr("Note.", note, fixed = TRUE)), 1L)
  printed <- utils::capture.output(print(tbl))
  expect_identical(sum(grepl("Note.", printed, fixed = TRUE)), 1L)
  expect_identical(sum(startsWith(printed, "Note. ")), 1L)
  withr::local_options(spicy.language = "fr")
  expect_true(startsWith(
    attr(.to_quiet(table_outcome(.to_sh(), bmi, select = sex)), "note"),
    "Note. Valeurs manquantes retir\u00e9es\u00a0: bmi (12). "
  ))
})

# ---- no data in a level (lot N, R1) -------------------------------------------

# The outcome is not observed in the level "high" of `edu` (a question
# not asked there); `sex` crosses it, so each of its levels has data.
.to_nodata <- function() {
  data.frame(
    y = c(2, 4, 6, 8, 1, 3, 5, 7, NA, NA, NA, NA),
    edu = factor(
      rep(c("low", "mid", "high"), each = 4),
      levels = c("low", "mid", "high")
    ),
    sex = rep(c("F", "M"), 6)
  )
}

test_that("a level without data shows the mark, n included", {
  long <- table_outcome(.to_nodata(), y, select = c(edu, sex), output = "long")
  high <- long$variable == "edu" & long$level %in% "high"
  expect_identical(long$n[high], NA_integer_)
  expect_true(is.na(long$mean[high]))
  # The Overall row stays the whole analytic sample.
  expect_identical(long$n[long$.row_role == "summary"], 8L)
  expect_identical(
    long$n[long$variable == "sex" & !is.na(long$level)],
    c(4L, 4L)
  )

  tbl <- table_outcome(.to_nodata(), y, select = c(edu, sex))
  row <- which(tbl$variable == "edu" & tbl$level %in% "high")
  st <- as_structured(tbl)
  expect_true(is.na(st$body$n[[row]]))
  expect_identical(st$cell_status$n[[row]], "undefined")
  expect_identical(
    attr(tbl, "display_df")$n[[row]],
    spicy_str("cell_undefined")
  )
})

test_that("the block compares the levels with data, and the note says why", {
  d <- .to_nodata()
  long <- table_outcome(d, y, select = c(edu, sex), output = "long")
  headers <- long[long$.row_role == "factor_header", ]
  # Two levels with data in `edu`: the two-group test.
  expect_identical(headers$test_type, c("welch_t", "welch_t"))
  expect_equal(
    headers$p.value[[1L]],
    stats::t.test(y ~ edu, data = droplevels(d[d$edu != "high", ]))$p.value,
    tolerance = 1e-12
  )
  # The four rows of "high" are not missing answers: no missing-value
  # sentence, in any block, and the no-data sentence instead.
  tbl <- table_outcome(d, y, select = c(edu, sex))
  expect_identical(
    attr(tbl, "note"),
    paste(
      "Note. No data for y in high.",
      "Group comparison: Welch t-test.",
      "Each block compares y across the levels of one variable;",
      "blocks are not adjusted for one another.",
      "Overall = the whole analytic sample."
    )
  )
  # A real missing value elsewhere is still counted.
  d$y[[1L]] <- NA
  expect_match(
    attr(table_outcome(d, y, select = c(edu, sex)), "note"),
    "Note. Missing values removed: y (1). No data for y in high.",
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    attr(table_outcome(.to_nodata(), y, select = c(edu, sex)), "note"),
    "Aucune donn\u00e9e pour y dans high.",
    fixed = TRUE
  )
})

test_that("a block with one level of data has no test, and names none", {
  d <- data.frame(
    y = c(1, 2, 3, 4, NA, NA, NA, NA),
    g = rep(c("a", "b"), each = 4),
    h = rep(c("u", "v"), 4)
  )
  long <- table_outcome(d, y, select = c(g, h), output = "long")
  headers <- long[long$.row_role == "factor_header", ]
  expect_true(is.na(headers$p.value[[1L]]))
  expect_false(is.na(headers$p.value[[2L]]))
  expect_identical(
    attr(table_outcome(d, y, select = c(g, h)), "note"),
    paste(
      "Note. No data for y in b.",
      "Group comparison: Welch t-test.",
      "Each block compares y across the levels of one variable;",
      "blocks are not adjusted for one another.",
      "Overall = the whole analytic sample."
    )
  )
})

test_that("the weights open the note", {
  d <- .to_nodata()
  d$w <- rep(c(1, 2), 6)
  note <- attr(
    table_outcome(d, y, select = edu, weights = w, p_value = FALSE),
    "note"
  )
  expect_true(startsWith(
    note,
    "Note. Statistics weighted by w. No data for y in high."
  ))
})

test_that("the object carries the title it prints", {
  tbl <- table_outcome(sochealth, bmi, select = sex)
  expect_identical(attr(tbl, "title"), "Descriptive statistics of bmi")
  expect_identical(utils::capture.output(print(tbl))[[1L]], attr(tbl, "title"))
})


test_that("labels names the outcome and the blocks: title, rows, notes", {
  d <- .to_sh()
  d$plain <- as.character(d$sex)
  d$na_lab <- structure(as.character(d$sex), label = NA_character_)
  d$empty <- structure(as.character(d$sex), label = "")
  build <- function(...) {
    .to_quiet(table_outcome(
      d,
      bmi,
      select = c(smoking, plain, na_lab, empty),
      drop_na = TRUE,
      ...
    ))
  }
  check <- function(tbl, outcome, block) {
    expect_identical(attr(tbl, "outcome_label"), outcome)
    expect_true(any(grepl(
      paste("Descriptive statistics of", outcome),
      utils::capture.output(print(tbl)),
      fixed = TRUE
    )))
    expect_identical(
      tbl$label[tbl$.row_role == "factor_header"],
      c(block, "plain", "na_lab", "empty")
    )
    expect_match(
      attr(tbl, "note"),
      sprintf("Missing values removed: %s (12).", outcome),
      fixed = TRUE
    )
    expect_match(
      attr(tbl, "note"),
      sprintf("Rows with missing %s removed: 25.", block),
      fixed = TRUE
    )
  }
  check(build(), "bmi", "smoking")
  # No attribute, an NA one and an empty one fall back to the name.
  check(build(labels = TRUE), "Body mass index", "Current smoker")
  check(build(labels = c(bmi = "BMI", nope = "X")), "BMI", "smoking")
  withr::local_options(spicy.var_labels = TRUE)
  check(build(labels = c(smoking = "Smoker")), "Body mass index", "Smoker")
  check(build(labels = FALSE), "bmi", "smoking")
  for (b in list(NULL, NA, c("a", "b"), list(bmi = "BMI"))) {
    expect_error(
      table_outcome(d, bmi, select = smoking, labels = b),
      class = "spicy_invalid_input"
    )
  }
  withr::local_options(spicy.var_labels = 1)
  expect_error(
    table_outcome(d, bmi, select = smoking),
    class = "spicy_invalid_input"
  )
})

test_that("var_labels() renames on the attribute base, whatever the option", {
  build <- function() {
    .to_quiet(table_outcome(
      .to_sh(),
      bmi,
      select = c(sex, smoking),
      labels = var_labels(bmi = "IMC")
    ))
  }
  check <- function(tbl) {
    expect_identical(attr(tbl, "outcome_label"), "IMC")
    expect_identical(
      tbl$label[tbl$.row_role == "factor_header"],
      c("Sex", "Current smoker")
    )
  }
  check(build())
  withr::local_options(spicy.var_labels = FALSE)
  check(build())
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
  # and fails far from its cause. The family's one class and wording.
  err <- tryCatch(table_outcome(d, bmi, select = c("sexe")), error = identity)
  expect_s3_class(err, "spicy_missing_column")
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "\"sexe\"", fixed = TRUE)

  err <- tryCatch(table_outcome(d, bmi, select = NULL), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "selects no column", fixed = TRUE)

  err <- tryCatch(table_outcome(d, bmi, select = c(sex, bmi)), error = identity)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "cannot contain the `outcome` column",
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

test_that("the long output is the plain compute frame", {
  d <- .to_sh()
  raw <- .to_quiet(table_outcome(d, bmi, select = sex, output = "long"))
  expect_s3_class(raw, "data.frame")
  expect_false(inherits(raw, "spicy_outcome_table"))
  expect_true(all(
    c(
      "variable",
      "label",
      "level",
      ".row_role",
      "mean",
      "sd",
      "n",
      "df",
      "df.residual",
      "p.value"
    ) %in%
      names(raw)
  ))
  # The family's vocabulary (ADR of 2026-10-20): broom's names for the
  # degrees of freedom, and no SMD columns -- this table computes none
  # (decision 9), so the two always-NA columns are gone.
  expect_false(any(c("df1", "df2", "smd_type", "smd_value") %in% names(raw)))
  # The note is the one attribute kept.
  expect_identical(
    setdiff(names(attributes(raw)), c("names", "row.names", "class")),
    "note"
  )
  # `output = "data.frame"` is no synonym: it is the displayed table,
  # one row per row of the compute frame.
  df <- .to_quiet(table_outcome(d, bmi, select = sex, output = "data.frame"))
  expect_false(identical(df, raw))
  expect_identical(nrow(df), nrow(raw))
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
  expect_match(
    conditionMessage(err_both),
    "`outcome` is required",
    fixed = TRUE
  )
})

test_that("the `by` migration still fires before the missing-arg guards", {
  d <- .to_sh()
  err <- tryCatch(table_outcome(d, by = c(sex)), error = identity)
  expect_s3_class(err, "spicy_defunct")
})


# ---- "data.frame" is the displayed table, "long" the compute frame -------

test_that("output = 'data.frame' is the displayed table, 'long' every statistic", {
  # Decision 5 of the ADR of 2026-10-20: the two tokens are no longer
  # synonyms. "data.frame" is what as.data.frame() of the printed object
  # returns -- the displayed columns under their keys, with numbers --
  # and "long" the compute frame, with every statistic.
  d <- .to_sh()
  tbl <- .to_quiet(table_outcome(d, bmi, select = c(sex, smoking)))
  as_df <- .to_quiet(table_outcome(
    d,
    bmi,
    select = c(sex, smoking),
    output = "data.frame"
  ))
  as_long <- .to_quiet(table_outcome(
    d,
    bmi,
    select = c(sex, smoking),
    output = "long"
  ))
  expect_identical(as_df, as.data.frame(tbl))
  expect_false(identical(as_df, as_long))
  # Both are long in rows: one row per printed row, which `.row_role`
  # marks.
  expect_identical(
    names(as_df)[1:4],
    c("variable", "label", "level", ".row_role")
  )
  expect_identical(as_df$.row_role, as_long$.row_role)
  expect_true(all(c("level", "summary") %in% as_df$.row_role))
  # The displayed columns hold the numbers of the compute frame.
  expect_identical(
    names(as_df)[-(1:4)],
    c("M", "SD", "Min", "Max", "95% CI LL", "95% CI UL", "n", "p")
  )
  expect_type(as_df$M, "double")
  expect_equal(as_df$M, as_long$mean, tolerance = 1e-12)
  expect_equal(as_df$p, as_long$p.value, tolerance = 1e-12)
  expect_false("mean" %in% names(as_df))
})

# ---- p_adjust ---------------------------------------------------------------

test_that("p_adjust adjusts the one test of each block, together", {
  raw <- table_outcome(mtcars, mpg, select = c(am, vs, cyl), output = "long")
  raw_p <- raw$p.value[!is.na(raw$p.value)]
  expect_length(raw_p, 3L)
  for (method in c("holm", "BH", "bonferroni")) {
    adj <- table_outcome(
      mtcars,
      mpg,
      select = c(am, vs, cyl),
      p_adjust = method,
      output = "long"
    )
    expect_equal(
      adj$p.value[!is.na(adj$p.value)],
      stats::p.adjust(raw_p, method = method)
    )
    expect_identical(adj$p_unadjusted, raw$p.value)
  }
})

test_that("p_adjust: the typed view, the note and an engine agree", {
  raw <- table_outcome(mtcars, mpg, select = c(am, vs, cyl), output = "long")
  raw_p <- raw$p.value[!is.na(raw$p.value)]
  tbl <- table_outcome(mtcars, mpg, select = c(am, vs, cyl), p_adjust = "BH")
  s <- as_structured(tbl)
  meta <- s$col_meta$p
  tested <- !is.na(s$body$p)
  expect_equal(s$body$p[tested], stats::p.adjust(raw_p, method = "BH"))
  expect_equal(meta$p_unadjusted[tested], raw_p)
  expect_identical(meta$p_adjust, "BH")
  expect_identical(meta$p_adjust_m, 3L)
  note <- 'P-values adjusted for 3 tests (Benjamini-Hochberg method).'
  expect_match(attr(tbl, "note"), note, fixed = TRUE)

  skip_if_not_installed("gt")
  g <- table_outcome(
    mtcars,
    mpg,
    select = c(am, vs, cyl),
    p_adjust = "BH",
    output = "gt"
  )
  expect_match(attr(g, "spicy_note"), note, fixed = TRUE)
})

test_that("p_adjust: the adjustment follows the tests, before the blocks", {
  note <- attr(
    table_outcome(mtcars, mpg, select = c(am, vs, cyl), p_adjust = "BH"),
    "note"
  )
  expect_match(
    note,
    paste0(
      "Welch t-test (am, vs); Welch one-way ANOVA (cyl). ",
      'P-values adjusted for 3 tests (Benjamini-Hochberg method). ',
      "Each block compares mpg"
    ),
    fixed = TRUE
  )
})

test_that("p_adjust needs the p-value column", {
  expect_error(
    table_outcome(
      mtcars,
      mpg,
      select = c(am, vs),
      p_value = FALSE,
      p_adjust = "holm"
    ),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_outcome(mtcars, mpg, select = c(am, vs), p_adjust = "Holm"),
    class = "spicy_invalid_input"
  )
})

test_that("p_value = FALSE leaves no p-value and no empty test column (issue #9)", {
  # The compute frame, under the family's names (`output = "long"`).
  test_cols <- c("test_type", "statistic", "df", "df.residual", "p.value")
  build <- function(...) {
    .to_quiet(table_outcome(.to_sh(), bmi, select = c(sex, smoking), ...))
  }
  ref <- build(output = "long")
  # Absent, not NA columns; the rest of the frame is the default's.
  out <- build(p_value = FALSE, output = "long")
  expect_identical(names(out), setdiff(names(ref), test_cols))
  expect_identical(as.list(out)[names(out)], as.list(ref)[names(out)])
  # The statistic and an effect size keep the test they show or come
  # from; the p-value still goes.
  for (st in list(
    build(p_value = FALSE, statistic = TRUE, output = "long"),
    build(p_value = FALSE, effect_size = "auto", output = "long")
  )) {
    expect_identical(
      intersect(test_cols, names(st)),
      setdiff(test_cols, "p.value")
    )
    expect_identical(st$statistic, ref$statistic)
  }
  # The displayed table loses its p column the same way.
  expect_true("p" %in% names(build(output = "data.frame")))
  expect_false("p" %in% names(build(p_value = FALSE, output = "data.frame")))
  # glance() keeps its fixed schema, NA where the frame has no column.
  gl <- broom::glance(build(p_value = FALSE, statistic = TRUE))
  gl0 <- broom::glance(build(p_value = FALSE))
  expect_identical(names(gl0), names(gl))
  expect_true(all(c("statistic", "df", "p.value") %in% names(gl)))
  expect_true(all(is.na(gl$p.value)))
  expect_false(anyNA(gl$statistic))
  expect_true(all(is.na(gl0$statistic)))
})

test_that("the outcome named in `select` is refused; a helper skips it", {
  d <- data.frame(
    y = c(1, 2, 3, 4, 10, 20, 30, 40),
    g = factor(rep(c("a", "b"), each = 4)),
    h = factor(rep(c("u", "v"), 4))
  )
  err <- expect_error(
    table_outcome(d, y, select = c(g, y)),
    class = "spicy_invalid_input"
  )
  expect_false(inherits(err, "spicy_missing_column"))
  out <- table_outcome(d, y, select = tidyselect::everything())
  expect_identical(attr(out, "select"), c("g", "h"))
  expect_error(
    table_outcome(d, y, select = c(g, nope)),
    class = "spicy_missing_column"
  )
  expect_error(
    table_outcome(d, nope, select = g),
    class = "spicy_missing_column"
  )
})

test_that("an outcome with no variance has no test, whichever test is chosen", {
  # wilcox.test() answers p = 1 and kruskal.test() NaN on a constant,
  # where t.test() stops: every test leaves the block's cells blank.
  d <- data.frame(
    k = rep(5, 12),
    g2 = factor(rep(c("a", "b"), each = 6)),
    g3 = factor(rep(c("a", "b", "c"), each = 4))
  )
  for (grp in c("g2", "g3")) {
    expect_warning(
      out <- table_outcome(
        d,
        k,
        select = tidyselect::all_of(grp),
        test = "nonparametric",
        output = "long"
      ),
      sprintf("`%s` (the outcome has no variance)", grp),
      fixed = TRUE,
      class = "spicy_undefined_stat"
    )
    expect_true(all(is.na(out$p.value)))
  }
})

test_that("the note names a block's test only when it ran", {
  # A constant outcome makes every test fail: the note must not name one.
  d <- data.frame(y = rep(5, 8), g = factor(rep(c("a", "b"), each = 4)))
  out <- suppressWarnings(table_outcome(d, y, select = g, output = "long"))
  expect_true(all(is.na(out$p.value)))
  expect_false(grepl(
    "Group comparison",
    attr(out, "note") %||% "",
    fixed = TRUE
  ))
})

test_that("`p_value = NULL`, the default, shows the p-value", {
  d <- data.frame(
    y = c(1, 2, 3, 4, 10, 20, 30, 40),
    g = factor(rep(c("a", "b"), each = 4))
  )
  expect_identical(
    table_outcome(d, y, select = g, p_value = NULL, output = "long"),
    table_outcome(d, y, select = g, p_value = TRUE, output = "long")
  )
})
