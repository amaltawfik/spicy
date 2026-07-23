# Ensure global spicy options don't trigger unintended behavior
old_opts <- options(spicy.rescale = FALSE, spicy.simulate_p = FALSE)
on.exit(options(old_opts)) # restore previous user options after tests

test_that("cross_tab basic two-way table works", {
  data <- mtcars
  res <- cross_tab(data, cyl, gear, output = "data.frame")

  expect_s3_class(res, "data.frame")
  expect_true("Values" %in% names(res))

  # The plain payload has no margins and no metadata attributes: the
  # cell counts alone sum to N.
  num_cols <- vapply(res, is.numeric, logical(1))
  expect_equal(sum(as.matrix(res[, num_cols])), nrow(data))
  expect_equal(attr(cross_tab(data, cyl, gear), "n_total"), nrow(data))
})


test_that("cross_tab supports grouping with by", {
  data <- mtcars
  res <- cross_tab(data, cyl, gear, by = am, output = "data.frame")

  expect_type(res, "list")
  expect_length(res, length(unique(data$am)))
  expect_true(all(vapply(res, inherits, logical(1), "data.frame")))
})

test_that("cross_tab supports interaction() in by", {
  data <- mtcars
  res <- cross_tab(
    data,
    cyl,
    gear,
    by = interaction(vs, am),
    output = "data.frame"
  )

  expect_type(res, "list")
  expect_length(res, length(unique(interaction(data$vs, data$am))))
})

test_that("cross_tab handles weights and rescale properly", {
  data <- mtcars

  # Without rescale: sum(weights) ≠ N
  res1 <- cross_tab(
    data,
    cyl,
    gear,
    weights = mpg,
    rescale = FALSE
  )
  total1 <- attr(res1, "n_total")

  # With rescale: sum(weights) == N
  res2 <- cross_tab(
    data,
    cyl,
    gear,
    weights = mpg,
    rescale = TRUE
  )
  total2 <- attr(res2, "n_total")

  expect_false(isTRUE(all.equal(total1, nrow(data))))
  expect_true(isTRUE(all.equal(round(total2), nrow(data))))
})

test_that("cross_tab automatically ignores NA values", {
  df <- data.frame(
    x = c("A", "B", NA, "A", "B", NA),
    y = c("Yes", "No", "Yes", "No", "Yes", NA)
  )

  # xtabs() ignores missing values automatically (the exclusion is
  # disclosed in the table note since 0.13.0; see the F02 tests below)
  res <- cross_tab(df, x, y)

  complete_n <- sum(stats::complete.cases(df[, c("x", "y")]))
  total_tab <- attr(res, "n_total")

  expect_equal(total_tab, complete_n)
})


test_that("cross_tab respects global options spicy.simulate_p and spicy.rescale", {
  data <- mtcars

  # Backup current options
  old_opts <- options()

  options(spicy.simulate_p = TRUE, spicy.rescale = TRUE)
  res <- cross_tab(data, cyl, gear, weights = mpg)

  # Verify attributes and global option effect. The simulated p-value
  # varies run to run, so pin the deterministic parts of the note.
  note <- attr(res, "note")
  expect_match(note, "Chi-2(NA) = 17.7, p", fixed = TRUE)
  expect_match(note, "(simulated)", fixed = TRUE)
  expect_match(note, "Weight: mpg (rescaled)", fixed = TRUE)
  expect_true(isTRUE(all.equal(round(attr(res, "n_total")), nrow(data))))

  # Restore options
  options(old_opts)
})

test_that("cross_tab returns spicy_cross_table or list with the default output", {
  data <- mtcars
  res1 <- cross_tab(data, cyl, gear)
  res2 <- cross_tab(data, cyl, gear, by = am)

  expect_s3_class(res1, "spicy_cross_table")
  expect_s3_class(res2, "spicy_cross_table_list")
})

test_that("cross_tab accepts labelled vectors in vector mode", {
  skip_if_not_installed("haven")
  x <- haven::labelled(
    c(1, 2, 1, 2, 1, 2),
    labels = c(Non = 1, Oui = 2)
  )
  y <- factor(c("BFH", "BFH", "HESAV", "HESAV", "ZHAW", "ZHAW"))

  res <- cross_tab(x, y, percent = "c")

  expect_s3_class(res, "data.frame")
  expect_true("Values" %in% names(res))
  expect_identical(attr(res, "title"), "Crosstable: x x y (Column %)")
})

test_that("cross_tab keeps column names with $ vector calls", {
  d <- data.frame(
    pasemploiraison_1 = c("Non", "Oui", "Non", "Oui"),
    hes = c("BFH", "BFH", "HESAV", "HESAV")
  )

  res <- cross_tab(d$pasemploiraison_1, d$hes, percent = "c")

  expect_identical(
    attr(res, "title"),
    "Crosstable: pasemploiraison_1 x hes (Column %)"
  )
})

test_that("cross_tab validates weights length in data.frame and vector modes", {
  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c("Yes", "No", "Yes", "No")
  )

  expect_error(
    cross_tab(df, x, y, weights = c(1, 2), output = "data.frame"),
    "`weights` must have the same length as the number of rows.",
    fixed = TRUE,
    class = "spicy_invalid_data"
  )

  expect_error(
    cross_tab(df$x, df$y, weights = c(1, 2), output = "data.frame"),
    "`weights` must have the same length as `x` and `y` in vector mode.",
    fixed = TRUE,
    class = "spicy_invalid_data"
  )
})

test_that("cross_tab rejects rescale when weight sum is zero", {
  df <- data.frame(
    x = c("A", "B"),
    y = c("Yes", "No"),
    w = c(0, 0)
  )

  expect_error(
    cross_tab(df, x, y, weights = w, rescale = TRUE, output = "data.frame"),
    "`rescale = TRUE` requires a strictly positive sum of weights.",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
})

test_that("cross_tab fails early when y is explicitly NULL", {
  expect_error(
    cross_tab(mtcars, cyl, y = NULL, output = "data.frame"),
    "You must specify a `y` variable",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
})

test_that("cross_tab computes by-group stats on non-empty margins", {
  df <- data.frame(
    g = c(rep("A", 6), rep("B", 8)),
    x = c("a", "a", "b", "b", "c", "c", "u", "u", "u", "u", "v", "v", "v", "v"),
    y = c("k", "l", "k", "l", "k", "l", "k", "k", "l", "l", "k", "k", "l", "l")
  )

  # Group A is 3x2, so `correct = TRUE` fires the new "Yates ignored"
  # warning; group B is 2x2 and gets Yates applied. The warning itself
  # is asserted in the dedicated test below.
  out <- suppressWarnings(
    cross_tab(df, x, y, by = g, correct = TRUE, output = "default")
  )
  note_b <- attr(out[["B"]], "note")

  # Pin the complete group-B note (fully deterministic).
  expect_identical(
    note_b,
    paste(
      "Chi-2(1) = 0.0, p = 1.000",
      "Cramer's V = 0.00",
      "Yates continuity correction applied.",
      "Stats computed on 2x2 sub-table after dropping empty rows / columns.",
      paste0(
        "Warning: 4 expected cells < 5 (100%). Minimum expected = 2. ",
        "Consider `simulate_p = TRUE` or set globally via ",
        "`options(spicy.simulate_p = TRUE)`."
      ),
      sep = "\n"
    )
  )
  expect_false(grepl("p = NA", note_b, fixed = TRUE))
})

test_that("cross_tab warns when `correct = TRUE` is ignored on non-2x2 sub-tables", {
  df <- data.frame(
    g = c(rep("A", 6), rep("B", 8)),
    x = c("a", "a", "b", "b", "c", "c", "u", "u", "u", "u", "v", "v", "v", "v"),
    y = c("k", "l", "k", "l", "k", "l", "k", "k", "l", "l", "k", "k", "l", "l")
  )
  expect_warning(
    cross_tab(df, x, y, by = g, correct = TRUE, output = "default"),
    "Yates continuity correction only applies to 2x2 tables"
  )
})

# ── Association measure features ───────────────────────────────────────────

test_that("cross_tab stores numeric attributes", {
  res <- cross_tab(mtcars, cyl, gear)
  expect_true(is.numeric(attr(res, "chi2")))
  expect_true(is.numeric(attr(res, "df")))
  expect_true(is.numeric(attr(res, "p_value")))
  expect_equal(attr(res, "assoc_measure"), "Cramer's V")
  expect_true(is.numeric(attr(res, "assoc_value")))
  expect_true(length(attr(res, "assoc_result")) == 5)
})

test_that("cross_tab auto-detects ordinal variables", {
  mt <- mtcars
  mt$cyl <- ordered(mt$cyl)
  mt$gear <- ordered(mt$gear)
  res <- cross_tab(mt, cyl, gear)
  expect_equal(attr(res, "assoc_measure"), "Kendall's Tau-b")
})

test_that("cross_tab assoc_ci adds CI to note", {
  res <- cross_tab(mtcars, cyl, gear, assoc_ci = TRUE)
  expect_match(
    attr(res, "note"),
    "Cramer's V = 0.53, 95% CI [0.22, 0.74]",
    fixed = TRUE
  )
})

test_that("cross_tab assoc_measure = 'none' omits coefficient line", {
  res <- cross_tab(mtcars, cyl, gear, assoc_measure = "none")
  expect_false(grepl("Cramer", attr(res, "note")))
  expect_match(attr(res, "note"), "Chi-2(4) = 18.0, p = .001", fixed = TRUE)
})

test_that("cross_tab note uses new Chi-2(df) format", {
  res <- cross_tab(mtcars, cyl, gear)
  expect_true(grepl("Chi-2\\(\\d+\\) =", attr(res, "note")))
  expect_match(attr(res, "note"), "Chi-2(4) = 18.0, p = .001", fixed = TRUE)
})

# ── Percentage modes ─────────────────────────────────────────────────────

test_that("cross_tab row percent shows row percentages", {
  res <- cross_tab(mtcars, cyl, gear, percent = "row")
  expect_true("Values" %in% names(res))
  expect_identical(attr(res, "title"), "Crosstable: cyl x gear (Row %)")
})

test_that("cross_tab column percent shows column percentages", {
  res <- cross_tab(mtcars, cyl, gear, percent = "column")
  expect_identical(attr(res, "title"), "Crosstable: cyl x gear (Column %)")
})

test_that("cross_tab default row percent includes Total and N rows", {
  res <- cross_tab(mtcars, cyl, gear, percent = "row", output = "default")
  expect_true("N" %in% names(res))
  vals <- res$Values
  expect_true("Total" %in% vals)
})

test_that("cross_tab default column percent includes Total and N rows", {
  res <- cross_tab(mtcars, cyl, gear, percent = "column", output = "default")
  vals <- res$Values
  expect_true("Total" %in% vals)
  expect_true("N" %in% vals)
})

test_that("cross_tab show_n = FALSE omits N row/column", {
  res <- cross_tab(
    mtcars,
    cyl,
    gear,
    percent = "row",
    show_n = FALSE,
    output = "default"
  )
  expect_false("N" %in% names(res))
  res2 <- cross_tab(
    mtcars,
    cyl,
    gear,
    percent = "column",
    show_n = FALSE,
    output = "default"
  )
  expect_false("N" %in% res2$Values)
})

# ── Association measures ────────────────────────────────────────────────

test_that("cross_tab assoc_measure = 'phi' works", {
  res <- cross_tab(mtcars, am, vs, assoc_measure = "phi")
  expect_equal(attr(res, "assoc_measure"), "Phi")
})

test_that("cross_tab assoc_measure = 'gamma' works", {
  mt <- mtcars
  mt$cyl <- ordered(mt$cyl)
  mt$gear <- ordered(mt$gear)
  res <- cross_tab(mt, cyl, gear, assoc_measure = "gamma")
  expect_equal(attr(res, "assoc_measure"), "Goodman-Kruskal Gamma")
})

test_that("cross_tab assoc_measure = 'tau_c' works", {
  mt <- mtcars
  mt$cyl <- ordered(mt$cyl)
  mt$gear <- ordered(mt$gear)
  res <- cross_tab(mt, cyl, gear, assoc_measure = "tau_c")
  expect_equal(attr(res, "assoc_measure"), "Kendall's Tau-c")
})

test_that("cross_tab assoc_measure = 'somers_d' works", {
  mt <- mtcars
  mt$cyl <- ordered(mt$cyl)
  mt$gear <- ordered(mt$gear)
  res <- cross_tab(mt, cyl, gear, assoc_measure = "somers_d")
  expect_equal(attr(res, "assoc_measure"), "Somers' D")
})

test_that("requested Somers' D line survives a C == D (independence) table", {
  # Symmetric Somers' d used to be a silent NA when concordant and
  # discordant pairs were equal, so the association line the user
  # asked for vanished from the printed note without a trace. It is
  # now the correct 0 (PSPP prints .000).
  x <- factor(rep(c("a", "a", "b", "b"), 5))
  y <- factor(rep(c("u", "v", "u", "v"), 5))
  res <- cross_tab(x, y, assoc_measure = "somers_d")
  expect_identical(attr(res, "assoc_value"), 0)
  out <- capture.output(print(res))
  expect_true(any(grepl("Somers' D", out, fixed = TRUE)))
})

test_that("cross_tab assoc_measure = 'lambda' works", {
  res <- cross_tab(mtcars, cyl, gear, assoc_measure = "lambda")
  expect_equal(attr(res, "assoc_measure"), "Lambda")
})

# ── Weight note ──────────────────────────────────────────────────────────

test_that("cross_tab adds weight note", {
  res <- cross_tab(mtcars, cyl, gear, weights = mpg)
  # Pin the complete note (fully deterministic).
  expect_identical(
    attr(res, "note"),
    "Chi-2(4) = 355.1, p <.001\nCramer's V = 0.53\nWeight: mpg"
  )
})

test_that("cross_tab adds rescaled note", {
  res <- cross_tab(
    mtcars,
    cyl,
    gear,
    weights = mpg,
    rescale = TRUE
  )
  expect_match(attr(res, "note"), "Weight: mpg (rescaled)", fixed = TRUE)
})

# ── Small expected cells warning ─────────────────────────────────────────

test_that("cross_tab warns about small expected cells", {
  df <- data.frame(
    x = c("A", "A", "A", "B", "B", "C"),
    y = c("X", "X", "Y", "X", "Y", "Y")
  )
  res <- cross_tab(df, x, y)
  # Pin the complete warning sentence.
  expect_match(
    attr(res, "note"),
    paste0(
      "Warning: 6 expected cells < 5 (100%). 2 expected cells < 1. ",
      "Minimum expected = 0.5. Consider `simulate_p = TRUE` or set ",
      "globally via `options(spicy.simulate_p = TRUE)`."
    ),
    fixed = TRUE
  )
})

# ── Simulate p ───────────────────────────────────────────────────────────

test_that("cross_tab simulate_p adds (simulated) to note", {
  res <- cross_tab(mtcars, cyl, gear, simulate_p = TRUE)
  # The simulated p-value varies run to run; pin the deterministic
  # prefix (df becomes NA under simulation) and the suffix marker.
  expect_match(attr(res, "note"), "Chi-2(NA) = 18.0, p", fixed = TRUE)
  expect_match(attr(res, "note"), "(simulated)", fixed = TRUE)
})

# ── include_stats = FALSE ────────────────────────────────────────────────

test_that("cross_tab include_stats = FALSE omits note", {
  res <- cross_tab(mtcars, cyl, gear, include_stats = FALSE)
  expect_null(attr(res, "note"))
})

# ── Validation ───────────────────────────────────────────────────────────

test_that("cross_tab errors when data is missing", {
  expect_error(
    cross_tab(),
    "must provide a dataset",
    class = "spicy_invalid_input"
  )
})

test_that("cross_tab errors when x is missing for data.frame", {
  expect_error(
    cross_tab(mtcars),
    "must specify at least one variable",
    class = "spicy_invalid_input"
  )
})

test_that("cross_tab errors with non-numeric weights", {
  expect_error(
    cross_tab(mtcars, cyl, gear, weights = "a"),
    "`weights` must be a numeric or logical vector",
    class = "spicy_invalid_input"
  )
})

test_that("cross_tab errors with negative weights", {
  df <- data.frame(x = c("A", "B"), y = c("C", "D"), w = c(-1, 1))
  expect_error(
    cross_tab(df, x, y, weights = w),
    "non-negative",
    class = "spicy_invalid_input"
  )
})

test_that("cross_tab errors with infinite weights", {
  df <- data.frame(x = c("A", "B"), y = c("C", "D"), w = c(Inf, 1))
  expect_error(
    cross_tab(df, x, y, weights = w),
    "finite",
    class = "spicy_invalid_input"
  )
})

# ── New 0.11.0 audit-fix coverage ────────────────────────────────────────────

test_that("cross_tab warns when `weights` contain NA values", {
  df <- data.frame(
    x = c("A", "B", "A", "B", "A"),
    y = c("X", "Y", "X", "Y", "X"),
    w = c(1, 2, NA, NA, 1)
  )
  expect_warning(
    cross_tab(df, x, y, weights = w, output = "data.frame"),
    "NA value.+in `weights`"
  )
  # The two NA-weighted rows are excluded -> n_total = 3
  res <- suppressWarnings(cross_tab(df, x, y, weights = w))
  expect_equal(attr(res, "n_total"), 4) # sum of c(1, 2, 1) = 4
})

test_that("cross_tab `decimal_mark = ','` propagates to all printed numbers", {
  data <- mtcars
  res <- cross_tab(
    data,
    cyl,
    gear,
    assoc_ci = TRUE,
    decimal_mark = ","
  )
  note <- attr(res, "note")
  # Chi-2 value should use comma decimal
  expect_match(note, "Chi-2\\(\\d+\\) = \\d+,\\d")
  # Cramer's V estimate
  expect_match(note, "Cramer's V = \\d+,\\d{2}")
  # CI uses ";" separator under decimal_mark = ","
  expect_match(note, "95% CI \\[\\d+,\\d{2}; \\d+,\\d{2}\\]")
})

test_that("cross_tab `p_digits` controls p-value precision and threshold", {
  # mtcars cyl x gear has p ~ 0.0014 (chi-2 = 18.04, df=4) -> with
  # p_digits = 3, the value is above 0.001 and prints `p = .001`;
  # with p_digits = 2, the value is below 0.01 and prints `p <.01`.
  res3 <- cross_tab(mtcars, cyl, gear, p_digits = 3L)
  res2 <- cross_tab(mtcars, cyl, gear, p_digits = 2L)
  # APA format strips the leading zero from the p-value.
  expect_match(attr(res3, "note"), "p = \\.\\d{3}\\b")
  expect_match(attr(res2, "note"), "p <\\.\\d{2}\\b")
})

test_that("cross_tab validates decimal_mark, p_digits and simulate_B", {
  expect_error(
    cross_tab(mtcars, cyl, gear, decimal_mark = ";"),
    "decimal_mark",
    class = "spicy_invalid_input"
  )
  expect_error(
    cross_tab(mtcars, cyl, gear, p_digits = 0L),
    "p_digits",
    class = "spicy_invalid_input"
  )
  expect_error(
    cross_tab(mtcars, cyl, gear, simulate_B = 0),
    "simulate_B",
    class = "spicy_invalid_input"
  )
  expect_error(
    cross_tab(mtcars, cyl, gear, simulate_B = -10),
    "simulate_B",
    class = "spicy_invalid_input"
  )
})

test_that("cross_tab adds a note when stats are computed on a pruned sub-table", {
  # `make_levels()` strips unused factor levels, so the by-mode (where
  # `factor(levels = full_x_levels)` puts in zero-rows for groups that
  # don't observe every x level) is the natural way to surface pruning.
  df <- data.frame(
    g = c("g1", "g1", "g1", "g1", "g2", "g2", "g2", "g2"),
    x = c("A", "A", "B", "B", "C", "C", "D", "D"),
    y = c("X", "Y", "X", "Y", "X", "Y", "X", "Y")
  )
  res <- suppressWarnings(cross_tab(df, x, y, by = g))
  pruned_line <- "Stats computed on 2x2 sub-table after dropping empty rows / columns."
  expect_match(attr(res[["g1"]], "note"), pruned_line, fixed = TRUE)
  expect_match(attr(res[["g2"]], "note"), pruned_line, fixed = TRUE)
})

test_that("cross_tab marks N row via attribute (robust to user level 'N')", {
  # User data with a level literally called "N" -- the old fragile detection
  # would have treated it as the totals row.
  df <- data.frame(
    x = c("Yes", "No", "Yes", "No", "Yes"),
    y = c("A", "B", "A", "B", "B")
  )
  res <- cross_tab(df, x, y, percent = "column", output = "default")
  # The N row attribute should point to the LAST row (after Total)
  expect_equal(attr(res, "n_row_idx"), nrow(res))
  expect_identical(attr(res, "n_col_name"), NA_character_)
  # And the user's "No" row should NOT be confused with the N row in the print
  out <- capture.output(print(res))
  expect_true(any(grepl("\\bNo\\b", out)))
  expect_true(any(grepl("\\bN\\b", out)))
})

test_that("cross_tab marks N column via attribute in row-percent mode", {
  df <- data.frame(
    x = c("A", "B", "A", "B", "A"),
    y = c("X", "Y", "X", "Y", "Y")
  )
  res <- cross_tab(df, x, y, percent = "row", output = "default")
  expect_identical(attr(res, "n_col_name"), "N")
  expect_true(is.na(attr(res, "n_row_idx")))
})

test_that("cross_tab decimal_mark / p_digits round-trip through print method", {
  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c("X", "Y", "X", "Y")
  )
  res <- cross_tab(
    df,
    x,
    y,
    percent = "column",
    decimal_mark = ",",
    output = "default"
  )
  out <- capture.output(print(res))
  expect_true(any(grepl(",", out)))
  expect_false(any(grepl("\\d+\\.\\d+", out)))
})

# ── Print methods ────────────────────────────────────────────────────────

test_that("print.spicy_cross_table produces output", {
  res <- cross_tab(mtcars, cyl, gear)
  expect_output(print(res))
})

test_that("print.spicy_cross_table_list produces output", {
  res <- cross_tab(mtcars, cyl, gear, by = am)
  expect_output(print(res))
})

# ── Vector input paths ──────────────────────────────────────────────────

test_that("cross_tab vector input with by works", {
  x <- c("A", "B", "A", "B")
  y <- c("X", "Y", "X", "Y")
  by <- c("G1", "G1", "G2", "G2")
  res <- cross_tab(x, y, by = by, output = "data.frame")
  expect_type(res, "list")
  expect_length(res, 2)
})

test_that("cross_tab vector input by length mismatch errors", {
  expect_error(
    cross_tab(c("A", "B"), c("X", "Y"), by = c("G1"), output = "data.frame"),
    "same length",
    class = "spicy_invalid_data"
  )
})

test_that("cross_tab vector input x/y length mismatch errors", {
  expect_error(
    cross_tab(c("A", "B", "C"), c("X", "Y"), output = "data.frame"),
    "same length",
    class = "spicy_invalid_data"
  )
})

test_that("cross_tab rescale without weights warns", {
  expect_warning(
    cross_tab(mtcars, cyl, gear, rescale = TRUE, output = "data.frame"),
    "no effect"
  )
})

test_that("cross_tab 1x1 table omits chi-2 note", {
  df <- data.frame(x = c("A", "A"), y = c("X", "X"))
  res <- cross_tab(df, x, y)
  expect_null(attr(res, "note"))
})

test_that("cross_tab percent = 'none' shows N in title", {
  res <- cross_tab(mtcars, cyl, gear, percent = "none")
  expect_identical(attr(res, "title"), "Crosstable: cyl x gear (N)")
})

test_that("cross_tab default percent = 'none' with digits", {
  res <- cross_tab(mtcars, cyl, gear, percent = "none", output = "default")
  expect_output(print(res))
})

test_that("print.spicy_cross_table formats N row in column percent", {
  res <- cross_tab(mtcars, cyl, gear, percent = "column", output = "default")
  out <- capture.output(print(res))
  # Pin the full N row (│ is the table's vertical rule); anchored
  # so a wrong label or wrong counts cannot slip through.
  n_line <- out[grepl("^ N ", out)]
  expect_length(n_line, 1L)
  expect_match(n_line, "^ N +\u2502 +15 +12 +5 \u2502 +32 $")
})

test_that("print.spicy_cross_table formats N column in row percent", {
  res <- cross_tab(mtcars, cyl, gear, percent = "row", output = "default")
  out <- capture.output(print(res))
  # Pin the full header row: the N column must sit after Total.
  header <- out[grepl("^ Values ", out)]
  expect_length(header, 1L)
  expect_match(header, "^ Values +\u2502 +3 +4 +5 \u2502 +Total +N $")
})

test_that("cross_tab vector mode errors when only one vector given", {
  expect_error(
    cross_tab(c("A", "B")),
    "must provide both",
    class = "spicy_invalid_input"
  )
})

test_that("cross_tab vector mode with interaction by", {
  x <- c("A", "B", "A", "B", "A", "B")
  y <- c("X", "Y", "X", "Y", "X", "Y")
  g1 <- c("M", "M", "F", "F", "M", "F")
  g2 <- c("Y", "Y", "Y", "O", "O", "O")
  res <- cross_tab(x, y, by = interaction(g1, g2), output = "data.frame")
  expect_type(res, "list")
})

test_that("cross_tab with [[ extraction preserves var name", {
  d <- data.frame(a = c("X", "Y", "X"), b = c("M", "F", "M"))
  res <- cross_tab(d[["a"]], d[["b"]], output = "data.frame")
  expect_s3_class(res, "data.frame")
})

test_that("cross_tab vector mode rejects non-numeric weights", {
  x <- c("A", "B", "A")
  y <- c("X", "Y", "X")
  expect_error(
    cross_tab(x, y, weights = c("a", "b", "c")),
    "numeric",
    class = "spicy_invalid_input"
  )
})

test_that("cross_tab vector mode rejects mismatched by length", {
  x <- c("A", "B", "A")
  y <- c("X", "Y", "X")
  expect_error(
    cross_tab(x, y, by = c("G1", "G2")),
    "same length",
    class = "spicy_invalid_data"
  )
})

test_that("cross_tab with $ accessor extracts var name", {
  d <- data.frame(aa = c("X", "Y", "X"), bb = c("M", "F", "M"))
  res <- cross_tab(d$aa, d$bb, output = "data.frame")
  expect_s3_class(res, "data.frame")
})

test_that("cross_tab skips stats when single non-empty column", {
  d <- data.frame(
    x = c("A", "A", "B", "B"),
    y = c("X", "X", "X", "X")
  )
  out <- capture.output(cross_tab(d, x, y))
  expect_false(any(grepl("Chi-2", out)))
})

test_that("cross_tab weighted without stats shows weight note alone", {
  d <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c("X", "Y", "X", "Y"),
    w = c(2, 3, 1, 4)
  )
  out <- capture.output(cross_tab(d, x, y, weights = w, include_stats = FALSE))
  # Pin the exact weight note line printed below the table.
  expect_true("Weight: w" %in% out)
})

test_that("cross_tab percent column default output", {
  out <- capture.output(cross_tab(mtcars, cyl, gear, percent = "column"))
  expect_identical(out[1], "Crosstable: cyl x gear (Column %)")
  expect_true(any(grepl("%", out)))
})

test_that("cross_tab percent row default output", {
  out <- capture.output(cross_tab(mtcars, cyl, gear, percent = "row"))
  expect_identical(out[1], "Crosstable: cyl x gear (Row %)")
  expect_true(any(grepl("%", out)))
})

test_that("cross_tab invalid assoc_measure errors with a classed condition", {
  expect_error(
    cross_tab(mtcars, cyl, gear, assoc_measure = "invalid"),
    class = "spicy_invalid_input"
  )
  expect_error(
    cross_tab(mtcars, cyl, gear, percent = "bogus"),
    class = "spicy_invalid_input"
  )
})

test_that("print.spicy_cross_table validates digits", {
  ct <- cross_tab(mtcars, cyl, am)
  expect_error(print(ct, digits = -1), class = "spicy_invalid_input")
  expect_error(print(ct, digits = 1.5), class = "spicy_invalid_input")
  expect_error(print(ct, digits = c(1, 2)), class = "spicy_invalid_input")
  expect_output(print(ct, digits = 2), "Crosstable")
})

test_that("cross_tab tryCatch fallback for complex x/y expressions", {
  d <- data.frame(a = c("X", "Y", "X"), b = c("M", "F", "M"))
  res <- cross_tab(d, a, b, output = "data.frame")
  expect_s3_class(res, "data.frame")
})

test_that("cross_tab print without Values column", {
  d <- data.frame(x = c("A", "B"), y = c("X", "Y"))
  res <- cross_tab(d, x, y, output = "data.frame")
  names(res)[names(res) == "Values"] <- "Category"
  class(res) <- c("spicy_cross_table", "spicy_table", "data.frame")
  attr(res, "title") <- "Test (N)"
  out <- capture.output(print(res))
  expect_true(length(out) > 0)
})

test_that("cross_tab print uses digits=1 for percent titles", {
  d <- data.frame(x = c("A", "B", "A"), y = c("X", "Y", "X"))
  res <- cross_tab(d, x, y, percent = "row", output = "data.frame")
  class(res) <- c("spicy_cross_table", "spicy_table", class(res))
  attr(res, "title") <- "Table (Row %)"
  attr(res, "digits") <- NULL
  out <- capture.output(print(res))
  expect_true(length(out) > 0)
})

test_that("cross_tab vector mode with [[ symbol index extracts name", {
  d <- data.frame(aa = c("X", "Y", "X"), bb = c("M", "F", "M"))
  col <- "aa"
  res <- cross_tab(d[[col]], d[["bb"]], output = "data.frame")
  expect_s3_class(res, "data.frame")
})

test_that("cross_tab vector mode with function-wrapped expression", {
  d <- data.frame(aa = c("x", "y", "x"), bb = c("M", "F", "M"))
  res <- cross_tab(toupper(d$aa), d$bb, output = "data.frame")
  expect_s3_class(res, "data.frame")
})

test_that("cross_tab handles all-NA vector in make_levels", {
  res <- cross_tab(c(NA, NA), c("A", "B"), output = "data.frame")
  expect_s3_class(res, "data.frame")
})

test_that("cross_tab DF mode with complex x/y expressions triggers tryCatch fallback", {
  d <- data.frame(
    a = c("X", "Y", "X", "Y"),
    b = c("M", "F", "M", "F"),
    c = c("P", "Q", "P", "Q")
  )
  res <- cross_tab(d, interaction(a, b), c, output = "data.frame")
  expect_s3_class(res, "data.frame")
  res2 <- cross_tab(d, c, interaction(a, b), output = "data.frame")
  expect_s3_class(res2, "data.frame")
})

test_that("cross_tab print uses digits=0 for count titles", {
  d <- data.frame(x = c("A", "B", "A"), y = c("X", "Y", "X"))
  res <- cross_tab(d, x, y, output = "data.frame")
  class(res) <- c("spicy_cross_table", "spicy_table", class(res))
  attr(res, "title") <- "Table (N)"
  attr(res, "digits") <- NULL
  out <- capture.output(print(res))
  expect_true(length(out) > 0)
})

test_that("cross_tab auto-renames internal Total/N margin columns on collision", {
  # Anti-regression for a silent data-corruption bug: when a y
  # level was literally named "N" or "Total", `cross_tab(percent
  # = "row")` and `percent = "column"` overwrote that user column
  # with row totals, producing a plausible-looking but corrupt
  # table. Now the function auto-picks a non-conflicting margin
  # column name (e.g. "N_1", "Total_1") and emits a
  # `spicy_renamed_column` warning, leaving the user's data
  # intact.

  # Y/N coding: the user's "N" level (% of "No") is preserved;
  # the sample-size column gets renamed to "N_1".
  df_yn <- data.frame(
    group = c("A", "A", "A", "B", "B", "B"),
    answer = c("Y", "N", "Y", "N", "Y", "N")
  )
  expect_warning(
    res <- cross_tab(df_yn, group, answer, percent = "row"),
    class = "spicy_renamed_column"
  )
  expect_true("N" %in% names(res)) # user data preserved
  expect_true("N_1" %in% names(res)) # margin column renamed
  expect_true("Total" %in% names(res)) # default margin name kept
  # User's "N" column holds the % of "No" answers (not row totals).
  # Group A: 1 N out of 3 -> 33.3%; Group B: 2 N out of 3 -> 66.7%.
  expect_equal(res$N[res$Values == "A"], 33.3)
  expect_equal(res$N[res$Values == "B"], 66.7)
  # Sample-size column ("N_1") holds the row counts.
  expect_equal(res$N_1[res$Values == "A"], 3)
  expect_equal(res$N_1[res$Values == "B"], 3)

  # User-level "Total": the user's Total level data preserved;
  # the margin column gets renamed to "Total_1".
  df_total <- data.frame(
    group = c("A", "A", "B", "B"),
    answer = c("Sub", "Total", "Sub", "Total")
  )
  expect_warning(
    res2 <- cross_tab(df_total, group, answer, percent = "column"),
    class = "spicy_renamed_column"
  )
  expect_true("Total" %in% names(res2)) # user data preserved
  expect_true("Total_1" %in% names(res2)) # margin renamed

  # `percent = "none"` does not touch the user's columns.
  expect_no_warning(
    cross_tab(df_yn, group, answer, percent = "none")
  )

  # `percent = "column"` with a Y/N coding does NOT trigger a
  # rename: only "Total" is reserved as a column there (the "N"
  # for sample size is a row label in this layout, not a column).
  expect_no_warning(
    cross_tab(df_yn, group, answer, percent = "column")
  )

  # User-level "Values" collides with the row-identifier column.
  # Without the fix, R's data.frame() auto-renamed the user's
  # column to "Values.1" silently AND make_named_row() then
  # overwrote the totals row's identifier label with the
  # percentage value of the y-level (because both keyed on
  # "Values"). Now the row-identifier is renamed to "Values_1"
  # explicitly and the user's "Values" column carries the
  # correct percentages.
  df_values <- data.frame(
    group = c("A", "A", "A", "B", "B", "B"),
    answer = c("Yes", "Values", "Yes", "Values", "Yes", "Values")
  )
  expect_warning(
    res3 <- cross_tab(df_values, group, answer, percent = "row"),
    class = "spicy_renamed_column"
  )
  expect_true("Values" %in% names(res3)) # user's y-level data preserved
  expect_true("Values_1" %in% names(res3)) # row identifier renamed
  # User's "Values" column holds the % of "Values" answers per row.
  expect_equal(res3$Values[res3$Values_1 == "A"], 33.3)
  expect_equal(res3$Values[res3$Values_1 == "B"], 66.7)
  # Row identifier column carries the x-levels and the "Total"
  # summary label, untouched by the y-level data.
  expect_equal(res3$Values_1, c("A", "B", "Total"))
})

# ── 0.13.0 API audit (Lot 2) ─────────────────────────────────────────────

test_that("cross_tab discloses NA rows dropped from x and y", {
  df <- data.frame(
    x = c("A", "B", NA, "A", "B", NA),
    y = c("Y", "N", "Y", "N", "Y", NA)
  )
  res <- cross_tab(df, x, y)
  note <- attr(res, "note")
  # Rows 3 and 6 carry NAs (row 6 on both variables): the per-variable
  # counts overlap, so the note discloses the deduplicated row total.
  expect_match(
    note,
    "Missing values removed: x (2), y (1); 2 rows in total.",
    fixed = TRUE
  )
  # The disclosure does not change the tabulation itself: counts still
  # cover the complete cases only.
  expect_equal(attr(res, "n_total"), sum(stats::complete.cases(df)))
})

test_that("cross_tab NA disclosure names only variables that lost rows", {
  df <- data.frame(
    x = c("A", "B", "A", "B", "A", "B"),
    y = c("Y", "N", NA, "N", "Y", "N")
  )
  res <- cross_tab(df, x, y)
  note <- attr(res, "note")
  expect_match(note, "Missing values removed: y (1).", fixed = TRUE)
  expect_false(grepl("x (", note, fixed = TRUE))

  # Complete data: no disclosure line at all.
  res2 <- cross_tab(mtcars, cyl, gear)
  expect_false(grepl(
    "Missing values removed",
    attr(res2, "note"),
    fixed = TRUE
  ))
})

test_that("cross_tab NA disclosure stands alone without a stats note", {
  df <- data.frame(x = c("A", NA, "B", "A"), y = c("Y", "N", "N", "Y"))
  res <- cross_tab(df, x, y, include_stats = FALSE)
  expect_identical(attr(res, "note"), "Missing values removed: x (1).")
})

test_that("cross_tab discloses rows dropped for a missing `by`", {
  df <- data.frame(
    x = c("A", "B", "A", "B", "A", NA),
    y = c("Y", "N", "Y", "N", "Y", "N"),
    g = c("g1", "g1", "g2", "g2", NA, "g1")
  )
  res <- suppressWarnings(cross_tab(df, x, y, by = g))
  for (nm in names(res)) {
    expect_match(
      attr(res[[nm]], "note"),
      "Rows with missing g removed: 1.",
      fixed = TRUE,
      info = paste("group", nm)
    )
  }
  # The x NA lives in group g1, so only g1 reports it.
  expect_match(
    attr(res[["g1"]], "note"),
    "Missing values removed: x (1).",
    fixed = TRUE
  )
  expect_false(
    grepl("Missing values removed", attr(res[["g2"]], "note"), fixed = TRUE)
  )
})

test_that("cross_tab warns when a third positional argument is given in vector mode", {
  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c("X", "Y", "X", "Y"),
    z = c("u", "v", "u", "v")
  )
  expect_warning(
    cross_tab(df$x, df$y, df$z, output = "data.frame"),
    class = "spicy_ignored_arg"
  )
  # The result is the x-by-y table: `z` plays no role.
  expect_identical(
    suppressWarnings(cross_tab(df$x, df$y, df$z, output = "data.frame")),
    cross_tab(df$x, df$y, output = "data.frame")
  )
})

test_that("cross_tab vector-mode `y` warning never forces the promise", {
  x <- c("A", "B", "A", "B")
  y <- c("X", "Y", "X", "Y")
  forced <- FALSE
  res <- suppressWarnings(
    cross_tab(
      x,
      y,
      y = {
        forced <- TRUE
        stop("never evaluated")
      },
      output = "data.frame"
    )
  )
  expect_false(forced)
  expect_s3_class(res, "data.frame")
})

test_that("cross_tab vector mode without a third argument stays silent", {
  expect_no_warning(
    cross_tab(c("A", "B", "A"), c("X", "Y", "X"), output = "data.frame")
  )
  expect_no_warning(
    cross_tab(
      c("A", "B", "A"),
      c("X", "Y", "X"),
      y = NULL,
      output = "data.frame"
    )
  )
})

test_that("cross_tab accepts logical weights in both modes (parity with freq)", {
  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c("X", "Y", "X", "Y"),
    keep = c(TRUE, FALSE, TRUE, TRUE)
  )
  df$keep_num <- as.numeric(df$keep)

  res_lgl <- cross_tab(df, x, y, weights = keep, output = "data.frame")
  res_num <- cross_tab(df, x, y, weights = keep_num, output = "data.frame")
  expect_identical(res_lgl, res_num)

  res_vec <- cross_tab(df$x, df$y, weights = df$keep, output = "data.frame")
  res_vec_num <- cross_tab(
    df$x,
    df$y,
    weights = df$keep_num,
    output = "data.frame"
  )
  expect_identical(res_vec, res_vec_num)

  # Same weighted total as freq() on the margin.
  f <- freq(df$x, weights = df$keep, output = "data.frame")
  expect_equal(sum(f$n), sum(df$keep_num))
  expect_equal(
    attr(cross_tab(df, x, y, weights = keep), "n_total"),
    sum(df$keep_num)
  )

  # Everything else non-numeric is still rejected.
  expect_error(
    cross_tab(df, x, y, weights = factor(c("a", "b", "a", "b"))),
    class = "spicy_invalid_input"
  )
})

test_that("cross_tab output = 'data.frame' returns a genuinely plain payload", {
  res <- cross_tab(mtcars, cyl, gear, output = "data.frame")
  expect_setequal(names(attributes(res)), c("names", "row.names", "class"))
  expect_identical(class(res), "data.frame")

  res_by <- cross_tab(mtcars, cyl, gear, by = am, output = "data.frame")
  for (el in res_by) {
    expect_setequal(names(attributes(el)), c("names", "row.names", "class"))
    expect_identical(class(el), "data.frame")
  }
})

test_that("cross_tab validates output with a classed error", {
  expect_error(
    cross_tab(mtcars, cyl, gear, output = "console"),
    class = "spicy_invalid_input"
  )
  expect_error(
    cross_tab(mtcars, cyl, gear, output = TRUE),
    class = "spicy_invalid_input"
  )
  # table_*()-only rendered engines are refused, not silently mapped.
  expect_error(
    cross_tab(mtcars, cyl, gear, output = "flextable"),
    class = "spicy_invalid_input"
  )
})

test_that("cross_tab styled is defunct with a migration error", {
  expect_error(
    cross_tab(mtcars, cyl, gear, styled = TRUE),
    class = "spicy_defunct"
  )
  expect_error(
    cross_tab(mtcars, cyl, gear, styled = FALSE),
    class = "spicy_defunct"
  )
})

test_that("cross_tab validates `digits` like freq()", {
  for (bad in list(-1, 1.5, "a", c(1, 2), NA, NaN, Inf)) {
    expect_error(
      cross_tab(mtcars, cyl, gear, digits = bad),
      "non-negative integer",
      class = "spicy_invalid_input",
      info = paste("digits =", deparse(bad))
    )
  }
  # NULL keeps the context-dependent default; valid integers pass.
  expect_s3_class(
    cross_tab(mtcars, cyl, gear, digits = NULL, output = "data.frame"),
    "data.frame"
  )
  expect_s3_class(
    cross_tab(
      mtcars,
      cyl,
      gear,
      digits = 2,
      percent = "row",
      output = "data.frame"
    ),
    "data.frame"
  )
})
