# Ensure global spicy options don't trigger unintended behavior
old_opts <- options(spicy.rescale = FALSE, spicy.simulate_p = FALSE)
on.exit(options(old_opts)) # restore previous user options after tests

test_that("cross_tab basic two-way table works", {
  data <- mtcars
  res <- cross_tab(data, cyl, gear, styled = FALSE)

  expect_s3_class(res, "data.frame")
  expect_true("Values" %in% names(res))

  if ("Total" %in% names(res)) {
    expect_equal(sum(res$Total, na.rm = TRUE), nrow(data))
  } else {
    expect_equal(attr(res, "n_total"), nrow(data))
  }
})


test_that("cross_tab supports grouping with by", {
  data <- mtcars
  res <- cross_tab(data, cyl, gear, by = am, styled = FALSE)

  expect_type(res, "list")
  expect_length(res, length(unique(data$am)))
  expect_true(all(vapply(res, inherits, logical(1), "data.frame")))
})

test_that("cross_tab supports interaction() in by", {
  data <- mtcars
  res <- cross_tab(data, cyl, gear, by = interaction(vs, am), styled = FALSE)

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
    rescale = FALSE,
    styled = FALSE
  )
  total1 <- attr(res1, "n_total")

  # With rescale: sum(weights) == N
  res2 <- cross_tab(
    data,
    cyl,
    gear,
    weights = mpg,
    rescale = TRUE,
    styled = FALSE
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

  # xtabs() ignores missing values automatically
  res <- cross_tab(df, x, y, styled = FALSE)

  complete_n <- sum(stats::complete.cases(df[, c("x", "y")]))
  total_tab <- attr(res, "n_total")

  expect_equal(total_tab, complete_n)
})


test_that("cross_tab respects global options spicy.simulate_p and spicy.rescale", {
  data <- mtcars

  # Backup current options
  old_opts <- options()

  options(spicy.simulate_p = TRUE, spicy.rescale = TRUE)
  res <- cross_tab(data, cyl, gear, weights = mpg, styled = FALSE)

  # Verify attributes and global option effect
  expect_true(grepl("Chi-2\\(", attr(res, "note")))
  expect_true(isTRUE(all.equal(round(attr(res, "n_total")), nrow(data))))

  # Restore options
  options(old_opts)
})

test_that("cross_tab returns spicy_cross_table or list when styled = TRUE", {
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

  res <- cross_tab(x, y, percent = "c", styled = FALSE)

  expect_s3_class(res, "data.frame")
  expect_true("Values" %in% names(res))
  expect_match(attr(res, "title"), "x x y")
})

test_that("cross_tab keeps column names with $ vector calls", {
  d <- data.frame(
    pasemploiraison_1 = c("Non", "Oui", "Non", "Oui"),
    hes = c("BFH", "BFH", "HESAV", "HESAV")
  )

  res <- cross_tab(d$pasemploiraison_1, d$hes, percent = "c", styled = FALSE)

  expect_match(attr(res, "title"), "pasemploiraison_1 x hes", fixed = TRUE)
})

test_that("cross_tab validates weights length in data.frame and vector modes", {
  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c("Yes", "No", "Yes", "No")
  )

  expect_error(
    cross_tab(df, x, y, weights = c(1, 2), styled = FALSE),
    "`weights` must have the same length as the number of rows.",
    fixed = TRUE
  )

  expect_error(
    cross_tab(df$x, df$y, weights = c(1, 2), styled = FALSE),
    "`weights` must have the same length as `x` and `y` in vector mode.",
    fixed = TRUE
  )
})

test_that("cross_tab rejects rescale when weight sum is zero", {
  df <- data.frame(
    x = c("A", "B"),
    y = c("Yes", "No"),
    w = c(0, 0)
  )

  expect_error(
    cross_tab(df, x, y, weights = w, rescale = TRUE, styled = FALSE),
    "`rescale = TRUE` requires a strictly positive sum of weights.",
    fixed = TRUE
  )
})

test_that("cross_tab fails early when y is explicitly NULL", {
  expect_error(
    cross_tab(mtcars, cyl, y = NULL, styled = FALSE),
    "You must specify a `y` variable",
    fixed = TRUE
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
    cross_tab(df, x, y, by = g, correct = TRUE, styled = TRUE)
  )
  note_b <- attr(out[["B"]], "note")

  expect_true(grepl(
    "Yates continuity correction applied.",
    note_b,
    fixed = TRUE
  ))
  expect_false(grepl("p = NA", note_b, fixed = TRUE))
})

test_that("cross_tab warns when `correct = TRUE` is ignored on non-2x2 sub-tables", {
  df <- data.frame(
    g = c(rep("A", 6), rep("B", 8)),
    x = c("a", "a", "b", "b", "c", "c", "u", "u", "u", "u", "v", "v", "v", "v"),
    y = c("k", "l", "k", "l", "k", "l", "k", "k", "l", "l", "k", "k", "l", "l")
  )
  expect_warning(
    cross_tab(df, x, y, by = g, correct = TRUE, styled = TRUE),
    "Yates continuity correction only applies to 2x2 tables"
  )
})

# ── Association measure features ───────────────────────────────────────────

test_that("cross_tab stores numeric attributes", {
  res <- cross_tab(mtcars, cyl, gear, styled = FALSE)
  expect_true(is.numeric(attr(res, "chi2")))
  expect_true(is.numeric(attr(res, "df")))
  expect_true(is.numeric(attr(res, "p_value")))
  expect_equal(attr(res, "assoc_measure"), "Cramer's V")
  expect_true(is.numeric(attr(res, "assoc_value")))
  expect_true(length(attr(res, "assoc_result")) == 4)
})

test_that("cross_tab auto-detects ordinal variables", {
  mt <- mtcars
  mt$cyl <- ordered(mt$cyl)
  mt$gear <- ordered(mt$gear)
  res <- cross_tab(mt, cyl, gear, styled = FALSE)
  expect_equal(attr(res, "assoc_measure"), "Kendall's Tau-b")
})

test_that("cross_tab assoc_ci adds CI to note", {
  res <- cross_tab(mtcars, cyl, gear, assoc_ci = TRUE, styled = FALSE)
  expect_true(grepl("95% CI", attr(res, "note")))
})

test_that("cross_tab assoc_measure = 'none' omits coefficient line", {
  res <- cross_tab(mtcars, cyl, gear, assoc_measure = "none", styled = FALSE)
  expect_false(grepl("Cramer", attr(res, "note")))
  expect_true(grepl("Chi-2", attr(res, "note")))
})

test_that("cross_tab note uses new Chi-2(df) format", {
  res <- cross_tab(mtcars, cyl, gear, styled = FALSE)
  expect_true(grepl("Chi-2\\(\\d+\\) =", attr(res, "note")))
})

# ── Percentage modes ─────────────────────────────────────────────────────

test_that("cross_tab row percent shows row percentages", {
  res <- cross_tab(mtcars, cyl, gear, percent = "row", styled = FALSE)
  expect_true("Values" %in% names(res))
  expect_match(attr(res, "title"), "Row %")
})

test_that("cross_tab column percent shows column percentages", {
  res <- cross_tab(mtcars, cyl, gear, percent = "column", styled = FALSE)
  expect_match(attr(res, "title"), "Column %")
})

test_that("cross_tab styled row percent includes Total and N rows", {
  res <- cross_tab(mtcars, cyl, gear, percent = "row", styled = TRUE)
  expect_true("N" %in% names(res))
  vals <- res$Values
  expect_true("Total" %in% vals)
})

test_that("cross_tab styled column percent includes Total and N rows", {
  res <- cross_tab(mtcars, cyl, gear, percent = "column", styled = TRUE)
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
    styled = TRUE
  )
  expect_false("N" %in% names(res))
  res2 <- cross_tab(
    mtcars,
    cyl,
    gear,
    percent = "column",
    show_n = FALSE,
    styled = TRUE
  )
  expect_false("N" %in% res2$Values)
})

# ── Association measures ────────────────────────────────────────────────

test_that("cross_tab assoc_measure = 'phi' works", {
  res <- cross_tab(mtcars, am, vs, assoc_measure = "phi", styled = FALSE)
  expect_equal(attr(res, "assoc_measure"), "Phi")
})

test_that("cross_tab assoc_measure = 'gamma' works", {
  mt <- mtcars
  mt$cyl <- ordered(mt$cyl)
  mt$gear <- ordered(mt$gear)
  res <- cross_tab(mt, cyl, gear, assoc_measure = "gamma", styled = FALSE)
  expect_equal(attr(res, "assoc_measure"), "Goodman-Kruskal Gamma")
})

test_that("cross_tab assoc_measure = 'tau_c' works", {
  mt <- mtcars
  mt$cyl <- ordered(mt$cyl)
  mt$gear <- ordered(mt$gear)
  res <- cross_tab(mt, cyl, gear, assoc_measure = "tau_c", styled = FALSE)
  expect_equal(attr(res, "assoc_measure"), "Kendall's Tau-c")
})

test_that("cross_tab assoc_measure = 'somers_d' works", {
  mt <- mtcars
  mt$cyl <- ordered(mt$cyl)
  mt$gear <- ordered(mt$gear)
  res <- cross_tab(mt, cyl, gear, assoc_measure = "somers_d", styled = FALSE)
  expect_equal(attr(res, "assoc_measure"), "Somers' D")
})

test_that("cross_tab assoc_measure = 'lambda' works", {
  res <- cross_tab(mtcars, cyl, gear, assoc_measure = "lambda", styled = FALSE)
  expect_equal(attr(res, "assoc_measure"), "Lambda")
})

# ── Weight note ──────────────────────────────────────────────────────────

test_that("cross_tab adds weight note", {
  res <- cross_tab(mtcars, cyl, gear, weights = mpg, styled = FALSE)
  expect_match(attr(res, "note"), "Weight: mpg")
})

test_that("cross_tab adds rescaled note", {
  res <- cross_tab(
    mtcars,
    cyl,
    gear,
    weights = mpg,
    rescale = TRUE,
    styled = FALSE
  )
  expect_match(attr(res, "note"), "rescaled")
})

# ── Small expected cells warning ─────────────────────────────────────────

test_that("cross_tab warns about small expected cells", {
  df <- data.frame(
    x = c("A", "A", "A", "B", "B", "C"),
    y = c("X", "X", "Y", "X", "Y", "Y")
  )
  res <- cross_tab(df, x, y, styled = FALSE)
  expect_match(attr(res, "note"), "expected cell")
})

# ── Simulate p ───────────────────────────────────────────────────────────

test_that("cross_tab simulate_p adds (simulated) to note", {
  res <- cross_tab(mtcars, cyl, gear, simulate_p = TRUE, styled = FALSE)
  expect_match(attr(res, "note"), "simulated")
})

# ── include_stats = FALSE ────────────────────────────────────────────────

test_that("cross_tab include_stats = FALSE omits note", {
  res <- cross_tab(mtcars, cyl, gear, include_stats = FALSE, styled = FALSE)
  expect_null(attr(res, "note"))
})

# ── Validation ───────────────────────────────────────────────────────────

test_that("cross_tab errors when data is missing", {
  expect_error(cross_tab(), "must provide a dataset")
})

test_that("cross_tab errors when x is missing for data.frame", {
  expect_error(cross_tab(mtcars), "must specify at least one variable")
})

test_that("cross_tab errors with non-numeric weights", {
  expect_error(
    cross_tab(mtcars, cyl, gear, weights = "a"),
    "`weights` must be numeric"
  )
})

test_that("cross_tab errors with negative weights", {
  df <- data.frame(x = c("A", "B"), y = c("C", "D"), w = c(-1, 1))
  expect_error(cross_tab(df, x, y, weights = w), "non-negative")
})

test_that("cross_tab errors with infinite weights", {
  df <- data.frame(x = c("A", "B"), y = c("C", "D"), w = c(Inf, 1))
  expect_error(cross_tab(df, x, y, weights = w), "finite")
})

# ── New 0.11.0 audit-fix coverage ────────────────────────────────────────────

test_that("cross_tab warns when `weights` contain NA values", {
  df <- data.frame(
    x = c("A", "B", "A", "B", "A"),
    y = c("X", "Y", "X", "Y", "X"),
    w = c(1, 2, NA, NA, 1)
  )
  expect_warning(
    cross_tab(df, x, y, weights = w, styled = FALSE),
    "NA value.+in `weights`"
  )
  # The two NA-weighted rows are excluded -> n_total = 3
  res <- suppressWarnings(cross_tab(df, x, y, weights = w, styled = FALSE))
  expect_equal(attr(res, "n_total"), 4) # sum of c(1, 2, 1) = 4
})

test_that("cross_tab `decimal_mark = ','` propagates to all printed numbers", {
  data <- mtcars
  res <- cross_tab(
    data,
    cyl,
    gear,
    assoc_ci = TRUE,
    decimal_mark = ",",
    styled = FALSE
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
  res3 <- cross_tab(mtcars, cyl, gear, p_digits = 3L, styled = FALSE)
  res2 <- cross_tab(mtcars, cyl, gear, p_digits = 2L, styled = FALSE)
  # APA format strips the leading zero from the p-value.
  expect_match(attr(res3, "note"), "p = \\.\\d{3}\\b")
  expect_match(attr(res2, "note"), "p <\\.\\d{2}\\b")
})

test_that("cross_tab validates decimal_mark, p_digits and simulate_B", {
  expect_error(cross_tab(mtcars, cyl, gear, decimal_mark = ";"), "decimal_mark")
  expect_error(cross_tab(mtcars, cyl, gear, p_digits = 0L), "p_digits")
  expect_error(cross_tab(mtcars, cyl, gear, simulate_B = 0), "simulate_B")
  expect_error(cross_tab(mtcars, cyl, gear, simulate_B = -10), "simulate_B")
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
  res <- suppressWarnings(cross_tab(df, x, y, by = g, styled = FALSE))
  expect_match(attr(res[["g1"]], "note"), "sub-table after dropping empty rows / columns")
  expect_match(attr(res[["g2"]], "note"), "sub-table after dropping empty rows / columns")
})

test_that("cross_tab marks N row via attribute (robust to user level 'N')", {
  # User data with a level literally called "N" -- the old fragile detection
  # would have treated it as the totals row.
  df <- data.frame(
    x = c("Yes", "No", "Yes", "No", "Yes"),
    y = c("A", "B", "A", "B", "B")
  )
  res <- cross_tab(df, x, y, percent = "column", styled = TRUE)
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
  res <- cross_tab(df, x, y, percent = "row", styled = TRUE)
  expect_identical(attr(res, "n_col_name"), "N")
  expect_true(is.na(attr(res, "n_row_idx")))
})

test_that("cross_tab decimal_mark / p_digits round-trip through print method", {
  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c("X", "Y", "X", "Y")
  )
  res <- cross_tab(df, x, y, percent = "column", decimal_mark = ",", styled = TRUE)
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
  res <- cross_tab(x, y, by = by, styled = FALSE)
  expect_type(res, "list")
  expect_length(res, 2)
})

test_that("cross_tab vector input by length mismatch errors", {
  expect_error(
    cross_tab(c("A", "B"), c("X", "Y"), by = c("G1"), styled = FALSE),
    "same length"
  )
})

test_that("cross_tab vector input x/y length mismatch errors", {
  expect_error(
    cross_tab(c("A", "B", "C"), c("X", "Y"), styled = FALSE),
    "same length"
  )
})

test_that("cross_tab rescale without weights warns", {
  expect_warning(
    cross_tab(mtcars, cyl, gear, rescale = TRUE, styled = FALSE),
    "no effect"
  )
})

test_that("cross_tab 1x1 table omits chi-2 note", {
  df <- data.frame(x = c("A", "A"), y = c("X", "X"))
  res <- cross_tab(df, x, y, styled = FALSE)
  expect_null(attr(res, "note"))
})

test_that("cross_tab percent = 'none' shows N in title", {
  res <- cross_tab(mtcars, cyl, gear, percent = "none", styled = FALSE)
  expect_match(attr(res, "title"), "(N)", fixed = TRUE)
})

test_that("cross_tab styled percent = 'none' with digits", {
  res <- cross_tab(mtcars, cyl, gear, percent = "none", styled = TRUE)
  expect_output(print(res))
})

test_that("print.spicy_cross_table formats N row in column percent", {
  res <- cross_tab(mtcars, cyl, gear, percent = "column", styled = TRUE)
  out <- capture.output(print(res))
  expect_true(any(grepl("N", out)))
})

test_that("print.spicy_cross_table formats N column in row percent", {
  res <- cross_tab(mtcars, cyl, gear, percent = "row", styled = TRUE)
  out <- capture.output(print(res))
  expect_true(any(grepl("N", out)))
})

test_that("cross_tab vector mode errors when only one vector given", {
  expect_error(
    cross_tab(c("A", "B")),
    "must provide both"
  )
})

test_that("cross_tab vector mode with interaction by", {
  x <- c("A", "B", "A", "B", "A", "B")
  y <- c("X", "Y", "X", "Y", "X", "Y")
  g1 <- c("M", "M", "F", "F", "M", "F")
  g2 <- c("Y", "Y", "Y", "O", "O", "O")
  res <- cross_tab(x, y, by = interaction(g1, g2), styled = FALSE)
  expect_type(res, "list")
})

test_that("cross_tab with [[ extraction preserves var name", {
  d <- data.frame(a = c("X", "Y", "X"), b = c("M", "F", "M"))
  res <- cross_tab(d[["a"]], d[["b"]], styled = FALSE)
  expect_s3_class(res, "data.frame")
})

test_that("cross_tab vector mode rejects non-numeric weights", {
  x <- c("A", "B", "A")
  y <- c("X", "Y", "X")
  expect_error(cross_tab(x, y, weights = c("a", "b", "c")), "numeric")
})

test_that("cross_tab vector mode rejects mismatched by length", {
  x <- c("A", "B", "A")
  y <- c("X", "Y", "X")
  expect_error(cross_tab(x, y, by = c("G1", "G2")), "same length")
})

test_that("cross_tab with $ accessor extracts var name", {
  d <- data.frame(aa = c("X", "Y", "X"), bb = c("M", "F", "M"))
  res <- cross_tab(d$aa, d$bb, styled = FALSE)
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
  expect_true(any(grepl("Weight:", out)))
})

test_that("cross_tab percent column styled output", {
  out <- capture.output(cross_tab(mtcars, cyl, gear, percent = "column"))
  expect_true(any(grepl("%", out)))
})

test_that("cross_tab percent row styled output", {
  out <- capture.output(cross_tab(mtcars, cyl, gear, percent = "row"))
  expect_true(any(grepl("%", out)))
})

test_that("cross_tab invalid assoc_measure errors", {
  expect_error(cross_tab(mtcars, cyl, gear, assoc_measure = "invalid"))
})

test_that("cross_tab tryCatch fallback for complex x/y expressions", {
  d <- data.frame(a = c("X", "Y", "X"), b = c("M", "F", "M"))
  res <- cross_tab(d, a, b, styled = FALSE)
  expect_s3_class(res, "data.frame")
})

test_that("cross_tab print without Values column", {
  d <- data.frame(x = c("A", "B"), y = c("X", "Y"))
  res <- cross_tab(d, x, y, styled = FALSE)
  names(res)[names(res) == "Values"] <- "Category"
  class(res) <- c("spicy_cross_table", "spicy_table", "data.frame")
  attr(res, "title") <- "Test (N)"
  out <- capture.output(print(res))
  expect_true(length(out) > 0)
})

test_that("cross_tab print uses digits=1 for percent titles", {
  d <- data.frame(x = c("A", "B", "A"), y = c("X", "Y", "X"))
  res <- cross_tab(d, x, y, percent = "row", styled = FALSE)
  class(res) <- c("spicy_cross_table", "spicy_table", class(res))
  attr(res, "title") <- "Table (Row %)"
  attr(res, "digits") <- NULL
  out <- capture.output(print(res))
  expect_true(length(out) > 0)
})

test_that("cross_tab vector mode with [[ symbol index extracts name", {
  d <- data.frame(aa = c("X", "Y", "X"), bb = c("M", "F", "M"))
  col <- "aa"
  res <- cross_tab(d[[col]], d[["bb"]], styled = FALSE)
  expect_s3_class(res, "data.frame")
})

test_that("cross_tab vector mode with function-wrapped expression", {
  d <- data.frame(aa = c("x", "y", "x"), bb = c("M", "F", "M"))
  res <- cross_tab(toupper(d$aa), d$bb, styled = FALSE)
  expect_s3_class(res, "data.frame")
})

test_that("cross_tab handles all-NA vector in make_levels", {
  res <- cross_tab(c(NA, NA), c("A", "B"), styled = FALSE)
  expect_s3_class(res, "data.frame")
})

test_that("cross_tab DF mode with complex x/y expressions triggers tryCatch fallback", {
  d <- data.frame(
    a = c("X", "Y", "X", "Y"),
    b = c("M", "F", "M", "F"),
    c = c("P", "Q", "P", "Q")
  )
  res <- cross_tab(d, interaction(a, b), c, styled = FALSE)
  expect_s3_class(res, "data.frame")
  res2 <- cross_tab(d, c, interaction(a, b), styled = FALSE)
  expect_s3_class(res2, "data.frame")
})

test_that("cross_tab print uses digits=0 for count titles", {
  d <- data.frame(x = c("A", "B", "A"), y = c("X", "Y", "X"))
  res <- cross_tab(d, x, y, styled = FALSE)
  class(res) <- c("spicy_cross_table", "spicy_table", class(res))
  attr(res, "title") <- "Table (N)"
  attr(res, "digits") <- NULL
  out <- capture.output(print(res))
  expect_true(length(out) > 0)
})
