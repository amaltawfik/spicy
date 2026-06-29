# Coverage tests for R/table_categorical.R
# These exercise paths not hit by tests/testthat/test-table_categorical.R:
#   - .assoc_label() ordinal labels + identity passthrough
#   - .resolve_assoc_measures() NULL / non-character / bad-value branches
#   - parse_stats() note-text fallback for a single-level select variable
#     (fmt_p / fmt_v NA cells)
#   - one-way gt center/right alignment branches
#   - flextable output that also writes a docx via `word_path`
#   - make_report_wide* skip-empty-label paths

# ---- .assoc_label(): ordinal labels and identity fallback -----------------

test_that(".assoc_label returns the documented ASCII labels", {
  expect_equal(spicy:::.assoc_label("tau_c"), "Stuart's Tau-c")
  expect_equal(spicy:::.assoc_label("somers_d"), "Somers' D")
  expect_equal(spicy:::.assoc_label("lambda"), "Lambda")
  # Unknown key falls through to the identity arm (display-only helper;
  # the public function validates measures before this is reached).
  expect_equal(spicy:::.assoc_label("not_a_measure"), "not_a_measure")
})

test_that("ordinal assoc_measure values produce their column names end-to-end", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )
  expect_true("Stuart's Tau-c" %in% names(
    table_categorical(df, "v1", "grp", labels = "V1",
                      assoc_measure = "tau_c", output = "long")
  ))
  expect_true("Somers' D" %in% names(
    table_categorical(df, "v1", "grp", labels = "V1",
                      assoc_measure = "somers_d", output = "long")
  ))
  expect_true("Lambda" %in% names(
    table_categorical(df, "v1", "grp", labels = "V1",
                      assoc_measure = "lambda", output = "long")
  ))
})

# ---- .resolve_assoc_measures(): input-shape branches ----------------------

test_that("assoc_measure = NULL defaults to auto resolution", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )
  out <- table_categorical(df, "v1", "grp", labels = "V1",
                           assoc_measure = NULL, output = "long")
  # 2x2 -> auto picks Phi
  expect_true("Phi" %in% names(out))
})

test_that("non-character assoc_measure is rejected", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B")),
    v1 = c("x", "y", "x", "y")
  )
  expect_error(
    table_categorical(df, "v1", "grp", assoc_measure = 1, output = "long"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical(df, "v1", "grp", assoc_measure = 1, output = "long"),
    "must be a character string"
  )
})

test_that("named assoc_measure with an unrecognised value errors clearly", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B")),
    v1 = c("x", "y", "x", "y")
  )
  expect_error(
    table_categorical(df, "v1", "grp",
                      assoc_measure = c(v1 = "bogus"), output = "long"),
    "value\\(s\\) not recognised"
  )
})

test_that("unnamed positional assoc_measure with a bad value errors clearly", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B")),
    v1 = c("x", "y", "x", "y"),
    v2 = c("p", "q", "p", "q")
  )
  # length matches select (2) so it takes the positional branch, then the
  # bad value is caught.
  expect_error(
    table_categorical(df, c("v1", "v2"), "grp",
                      assoc_measure = c("phi", "bogus"), output = "long"),
    "value\\(s\\) not recognised"
  )
})

# ---- parse_stats() note fallback: single-level select variable ------------

test_that("single-level select variable yields NA stats (note fallback)", {
  # A select variable with a single observed level makes the internal
  # cross_tab() omit the numeric p_value / assoc_value attrs, so
  # parse_stats() falls through to the note-text fallback. With no note
  # to parse, p and the association measure resolve to NA.
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = factor(rep("only", 6))
  )
  out <- table_categorical(df, "v1", "grp", labels = "V1", output = "long")
  expect_true(all(is.na(out$p)))
  expect_true("Cramer's V" %in% names(out))
  expect_true(all(is.na(out[["Cramer's V"]])))
})

test_that("single-level select variable renders blank p / assoc cells (default)", {
  # Exercises fmt_p(NA) -> "" and fmt_v(NA) -> "" in the report-wide path.
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = factor(rep("only", 6))
  )
  out <- table_categorical(df, "v1", "grp", labels = "V1", output = "default")
  expect_s3_class(out, "spicy_categorical_table")
  disp <- attr(out, "display_df")
  expect_true(all(disp$p == ""))
  expect_true(all(disp[["Cramer's V"]] == ""))
})

# ---- one-way gt alignment branches ----------------------------------------

test_that("one-way gt output honours align = 'center' and 'right'", {
  skip_if_not_installed("gt")
  g_center <- table_categorical(sochealth, select = smoking,
                                output = "gt", align = "center")
  g_right <- table_categorical(sochealth, select = smoking,
                               output = "gt", align = "right")
  expect_s3_class(g_center, "gt_tbl")
  expect_s3_class(g_right, "gt_tbl")
})

# ---- flextable output with word_path also writes a docx -------------------

test_that("one-way flextable output with word_path returns ft and writes docx", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)
  ft <- table_categorical(sochealth, select = smoking,
                          output = "flextable", word_path = tmp)
  expect_s3_class(ft, "flextable")
  expect_true(file.exists(tmp))
})

test_that("cross-tab flextable output with word_path returns ft and writes docx", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)
  ft <- table_categorical(sochealth, select = smoking, by = sex,
                          output = "flextable", word_path = tmp)
  expect_s3_class(ft, "flextable")
  expect_true(file.exists(tmp))
})

# ---- make_report_wide*: skip a label with no rows -------------------------

test_that("one-way default output skips an all-NA variable label", {
  # The all-NA variable produces no long rows, so its label is skipped in
  # make_report_wide_oneway() (the `next` at the empty-subset guard).
  df <- data.frame(
    good = factor(c("a", "b", "a", "b")),
    allna = factor(rep(NA_character_, 4), levels = c("x", "y"))
  )
  out <- table_categorical(df, select = c(good, allna),
                           drop_na = TRUE, output = "default")
  expect_s3_class(out, "spicy_categorical_table")
  vars <- trimws(attr(out, "display_df")$Variable)
  expect_true("good" %in% vars)
  expect_false("allna" %in% vars)
})

test_that("grouped default output skips an all-NA variable label", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B")),
    good = factor(c("a", "b", "a", "b")),
    allna = factor(rep(NA_character_, 4), levels = c("x", "y"))
  )
  out <- table_categorical(df, select = c(good, allna), by = grp,
                           drop_na = TRUE, output = "default")
  expect_s3_class(out, "spicy_categorical_table")
  vars <- trimws(attr(out, "display_df")$Variable)
  expect_true("good" %in% vars)
  expect_false("allna" %in% vars)
})

test_that("one-way default output with levels_keep reorders modalities", {
  # Exercises the levels_keep branch inside make_report_wide_oneway().
  df <- data.frame(
    good = factor(c("a", "b", "a", "b"), levels = c("a", "b"))
  )
  out <- table_categorical(df, select = good,
                           levels_keep = c("b", "a"), output = "default")
  expect_s3_class(out, "spicy_categorical_table")
  disp <- attr(out, "display_df")
  modal <- trimws(disp$Variable[startsWith(disp$Variable, "  ")])
  expect_equal(modal, c("b", "a"))
})
