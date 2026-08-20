# table_outcome(): one continuous outcome across the levels of several
# categorical variables, one block per variable.

test_that("the title names the outcome and nothing else", {
  # Decision 32: the grouping variables ARE the rows, so a title that
  # listed them would repeat the stub.
  expect_identical(
    spicy:::.outcome_title("Body mass index"),
    "Descriptive statistics of Body mass index"
  )
  # One `by` or six, the title is the same: the geometry is what
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
# The compute frame: one block per `by` variable
# ============================================================================

# The worked example of the spec: eight observations, a two-level
# grouping and a three-level one, chosen so every statistic separates.
.to_y <- function() c(1, 2, 3, 4, 10, 20, 30, 40)
.to_g1 <- function() c("a", "a", "a", "a", "b", "b", "b", "b")
.to_g2 <- function() c("x", "x", "y", "y", "z", "z", "x", "y")

.to_compute <- function(..., y = .to_y()) {
  suppressWarnings(spicy:::.outcome_compute(
    outcome = y,
    by_list = list(g1 = .to_g1(), g2 = .to_g2()),
    by_labels = c(g1 = "G one", g2 = "G two"),
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
    by_list = list(g = g_na, h = .to_g2()),
    by_labels = c(g = "G", h = "H"),
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
  # The missing `by` values are a display level of their own, and the
  # role is the KEY -- not the label.
  miss <- r[r$variable == "g" & r$.row_role == "missing", ]
  expect_identical(nrow(miss), 1L)
  expect_identical(miss$level, "(Missing)")
  # Two rows have a missing `by` value and an observed outcome; both
  # count, which is the point of showing the level.
  expect_identical(miss$n, 2L)
})

test_that("drop_na = TRUE removes the missing rows block by block", {
  y <- c(1, 2, 3, 4, 10, 20, 30, 40)
  g_na <- c("a", "a", "a", NA, "b", "b", NA, "b")
  r <- suppressWarnings(spicy:::.outcome_compute(
    outcome = y,
    by_list = list(g = g_na),
    by_labels = c(g = "G"),
    outcome_name = "y",
    outcome_label = "Y",
    drop_na = TRUE
  ))
  expect_identical(r$n[r$.row_role == "summary"], 8L)
  expect_identical(sum(r$n[r$.row_role == "level"]), 6L)
  expect_false(any(r$.row_role == "missing"))
  expect_identical(unname(attr(r, "by_na_dropped")[["g"]]), 2L)
})

test_that("a block too thin to compare degrades on its own", {
  # One block cannot be tested (a single observed level), the other
  # can. The thin one keeps NA test columns; the other is untouched.
  r <- suppressWarnings(spicy:::.outcome_compute(
    outcome = .to_y(),
    by_list = list(one = rep("only", 8L), g1 = .to_g1()),
    by_labels = c(one = "One", g1 = "G one"),
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
    by_list = list(g = g),
    by_labels = c(g = "G"),
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
    by_list = list(g = c("a", "a", "a", "a", "a", "a", "a", "solo")),
    by_labels = c(g = "G"),
    outcome_name = "y",
    outcome_label = "Y"
  ))
  solo <- which(r$level == "solo")
  expect_identical(r$n[solo], 1L)
  expect_equal(r$mean[solo], 40, tolerance = 1e-12)
  expect_true(is.na(r$sd[solo]))
  expect_true(is.na(r$ci_lower[solo]))
})

test_that("a `by` with no observed level yields no level rows", {
  r <- suppressWarnings(spicy:::.outcome_compute(
    outcome = .to_y(),
    by_list = list(g = factor(rep(NA_character_, 8L), levels = character(0))),
    by_labels = c(g = "G"),
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
