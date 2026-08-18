# Coverage-gap tests for the descriptive / core-helper family.
#
# Lines closed here:
#   R/weighted_stats.R       49, 65-67
#   R/count_n.R              393-400
#   R/cross_tab.R            371, 963-964, 966, 1194, 1490
#   R/user_na.R              100, 111
#   R/table_categorical.R    2298, 2362, 3324-3328
#   R/abort.R                348, 366
#   R/selection_helpers.R    52
#   R/regression_transform.R 150
#
# Every test pins an exact value, an exact condition class or an exact
# output string -- never bare execution.

# ---- weighted_stats.R line 49: .wtd_sd() with sum(w) <= 1 -----------------

test_that(".wtd_sd returns NA when the expanded sample has < 2 observations", {
  # sum(w) - 1 is the frequency denominator: it is 0 for exactly one
  # expanded observation and negative below that, so the SD is undefined
  # and the guard returns NA_real_ rather than NaN / a negative sqrt().
  expect_identical(spicy:::.wtd_sd(c(1, 2), c(0.5, 0.5)), NA_real_)
  expect_identical(spicy:::.wtd_sd(3, 1), NA_real_)
  expect_identical(spicy:::.wtd_sd(c(1, 2, 3), c(0.2, 0.2, 0.1)), NA_real_)
  # Contrast: above the guard the frequency convention holds exactly --
  # with integer weights the result is sd(rep(x, w)) byte for byte.
  x <- c(1, 2, 2, 5)
  w <- c(2, 1, 3, 1)
  expect_equal(spicy:::.wtd_sd(x, w), stats::sd(rep(x, w)))
})


# ---- weighted_stats.R lines 65-67: .wtd_quantile7() tie collapsing --------

test_that(".wtd_quantile7 sums the weights of duplicated x values", {
  # x has ties, so the anyDuplicated() branch fires and collapses them:
  #   sorted x    = 1, 1, 2, 2, 3, 4     sorted w    = 2, 1, 1, 3, 1, 2
  #   collapsed x = 1, 2, 3, 4           collapsed w = 3, 4, 1, 2
  #
  # Oracle hand-computed here from the Hmisc::wtd.quantile(type =
  # "quantile") algorithm the file transcribes (no Hmisc dependency is
  # added). W = 10, cumsum(w) = 3, 7, 8, 10; the position
  # 1 + (W - 1) p is looked up as a right-continuous step (approx
  # method "constant", f = 1) and blended linearly:
  #   p = .25 -> pos = 3.25, low = 3, high = 4, frac = .25
  #              step(3) = 1, step(4) = 2 -> .75 * 1 + .25 * 2 = 1.25
  #   p = .50 -> pos = 5.50, low = 5, high = 6, frac = .50
  #              step(5) = step(6) = 2                       -> 2.00
  #   p = .75 -> pos = 7.75, low = 7, high = 8, frac = .75
  #              step(7) = 2, step(8) = 3 -> .25 * 2 + .75 * 3 = 2.75
  x <- c(2, 1, 2, 3, 1, 4)
  w <- c(1, 2, 3, 1, 1, 2)
  probs <- c(0.25, 0.5, 0.75)
  expect_gt(anyDuplicated(x), 0L) # precondition: the branch is reached
  got <- spicy:::.wtd_quantile7(x, w, probs)
  expect_equal(got, c(1.25, 2, 2.75))
  # Independent oracle: integer weights are frequency weights, so the
  # answer must equal the unweighted type-7 quantile of rep(x, w).
  expect_equal(
    got,
    stats::quantile(rep(x, w), probs, type = 7, names = FALSE)
  )
  # The collapse is exactly "sort, then sum the weights of equal x":
  # feeding the already-collapsed pair (no tie branch) returns the same
  # numbers, which is what lines 65-67 claim to produce.
  expect_equal(
    spicy:::.wtd_quantile7(c(1, 2, 3, 4), c(3, 4, 1, 2), probs),
    got
  )
})


# ---- count_n.R lines 393-400: no column survives the comparison -----------

test_that("count_n() warns and returns all NA when no column can be compared", {
  # A declared-UTF-8 string carrying an invalid byte makes tolower()
  # error inside compare_fun(). Here EVERY selected column is such a
  # string, so `results` is empty: the classed no-selection warning
  # fires and the result is a pure NA placeholder of nrow(data).
  bad <- rawToChar(as.raw(0xe9))
  Encoding(bad) <- "UTF-8"
  df <- data.frame(x = rep(bad, 3), stringsAsFactors = FALSE)
  df$y <- rep(bad, 3)
  cond <- tryCatch(
    count_n(df, count = "a", ignore_case = TRUE),
    warning = function(w) w
  )
  expect_s3_class(cond, "spicy_no_selection")
  expect_match(
    conditionMessage(cond),
    "No selected column could be compared with `count`",
    fixed = TRUE
  )
  res <- suppressWarnings(count_n(df, count = "a", ignore_case = TRUE))
  expect_identical(res, rep(NA_real_, 3))
})


# ---- cross_tab.R line 371: `df[, "col"]` names the variable ---------------

test_that("cross_tab() reads the column name out of a `df[, \"col\"]` call", {
  d <- data.frame(
    aa = factor(rep(c("p", "q", "r"), each = 10)),
    bb = factor(rep(c("u", "v"), 15))
  )
  # Vector mode: the captured expressions are `d[, "aa"]` / `d[, "bb"]`,
  # whose last argument is a string literal naming the column.
  ct <- cross_tab(d[, "aa"], d[, "bb"], include_stats = FALSE)
  expect_identical(attr(ct, "title"), "Crosstable: aa x bb (N)")
  # Contrast: an inline literal vector carries no column name, so the
  # neutral placeholders are used instead of a deparsed data value.
  ct0 <- cross_tab(
    factor(rep(c("p", "q"), 15)),
    factor(rep(c("u", "v"), 15)),
    include_stats = FALSE
  )
  expect_identical(attr(ct0, "title"), "Crosstable: x x y (N)")
})


# ---- cross_tab.R lines 963-964: classed measure error is re-raised --------

test_that("cross_tab() re-raises a classed error from an association measure", {
  # phi() refuses anything that is not 2x2. That refusal is the
  # measure's documented contract, so it must reach the caller instead
  # of degrading to a silent all-NA association column.
  d <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 30)),
    y = factor(rep(c("u", "v"), 45))
  )
  err <- tryCatch(
    cross_tab(d, x, y, assoc_measure = "phi"),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_unsupported")
  expect_s3_class(err, "spicy_error")
  expect_match(
    conditionMessage(err),
    "must be a 2x2 table for the phi coefficient",
    fixed = TRUE
  )
})


# ---- cross_tab.R line 966: unclassed measure error degrades to NULL -------

test_that("cross_tab() drops the association line on an unclassed error", {
  d <- data.frame(
    x = factor(rep(c("a", "b", "c"), each = 30)),
    y = factor(rep(c("u", "v"), 45))
  )
  # Baseline: the measure normally contributes its own note line.
  ok <- cross_tab(d, x, y, assoc_measure = "cramer_v")
  expect_match(attr(ok, "note"), "Cramer", fixed = TRUE)
  # An error with no spicy class is not the measure's contract, so it
  # degrades to "no association line" while the chi-square line stays.
  out <- with_mocked_bindings(
    cross_tab(d, x, y, assoc_measure = "cramer_v"),
    cramer_v = function(...) stop("plain unclassed failure"),
    .package = "spicy"
  )
  expect_match(attr(out, "note"), "Chi-2(2)", fixed = TRUE)
  expect_false(grepl("Cramer", attr(out, "note"), fixed = TRUE))
  expect_true(all(is.na(attr(out, "assoc_estimate") %||% NA_real_)))
})


# ---- cross_tab.R line 1194: declared-missing note with no prior note ------

test_that("cross_tab() creates the note from the declared-missing line", {
  skip_if_not_installed("haven")
  d <- data.frame(id = 1:6)
  d$xx <- haven::labelled_spss(
    c(1, 2, 1, 2, 9, 9),
    labels = c(No = 1, Yes = 2, Refused = 9),
    na_values = 9
  )
  d$yy <- factor(c("u", "v", "u", "v", "u", "v"))
  # `include_stats = FALSE` leaves no chi-square note, there are no
  # weights and no regular NAs: the declared-missing line is the FIRST
  # note fragment, so it is assigned rather than appended.
  ct <- cross_tab(d, xx, yy, include_stats = FALSE)
  expect_identical(
    attr(ct, "note"),
    "Declared missing values removed: xx (2)."
  )
  expect_identical(attr(ct, "n_total"), 4)
  # Contrast: with the statistics on, the same line is appended after
  # the chi-square block instead of replacing it.
  ct2 <- cross_tab(d, xx, yy, include_stats = TRUE)
  expect_match(attr(ct2, "note"), "Chi-2(1)", fixed = TRUE)
  expect_match(
    attr(ct2, "note"),
    "\nDeclared missing values removed: xx (2).",
    fixed = TRUE
  )
})


# ---- cross_tab.R line 1490: print() digits derived from percent_mode ------

test_that("print.spicy_cross_table() derives digits from percent_mode", {
  d <- data.frame(
    a = factor(rep(c("p", "q"), each = 40)),
    b = factor(rep(c("u", "v"), 40))
  )
  row_tab <- cross_tab(d, a, b, percent = "row", include_stats = FALSE)
  count_tab <- cross_tab(d, a, b, percent = "none", include_stats = FALSE)
  # Simulate an object rebuilt from the plain-data.frame payload, which
  # keeps `percent_mode` but has lost the stored `digits`.
  attr(row_tab, "digits") <- NULL
  attr(count_tab, "digits") <- NULL
  expect_identical(attr(row_tab, "percent_mode"), "row")
  expect_identical(attr(count_tab, "percent_mode"), "none")

  row_out <- utils::capture.output(print(row_tab))
  count_out <- utils::capture.output(print(count_tab))
  # percent_mode != "none" -> one decimal.
  expect_true(any(grepl("50\\.0", row_out)))
  expect_true(any(grepl("100\\.0", row_out)))
  # percent_mode == "none" -> raw counts, no decimal anywhere.
  expect_false(any(grepl("[0-9]\\.[0-9]", count_out)))
  expect_true(any(grepl("20", count_out, fixed = TRUE)))
  expect_true(any(grepl("80", count_out, fixed = TRUE)))
})


# ---- user_na.R lines 100, 111: non-double codes, no value labels ----------

test_that(".user_na_info handles an integer vector with no value labels", {
  skip_if_not_installed("haven")
  # Integer storage: is.double(x) is FALSE, so the tagged-NA probe is
  # short-circuited to an all-FALSE mask (tagged NAs are a double-only
  # feature). No `labels` attribute: every code resolves to NA and is
  # displayed as the bare code in every labelled_levels mode.
  v <- haven::labelled_spss(
    c(1L, 2L, 9L, 9L, 8L),
    labels = NULL,
    na_values = c(8L, 9L)
  )
  expect_false(is.double(v))
  expect_null(attr(v, "labels", exact = TRUE))
  mask <- spicy:::.user_na_mask(v)
  expect_identical(mask, c(FALSE, FALSE, TRUE, TRUE, TRUE))

  info <- spicy:::.user_na_info(v[mask])
  expect_identical(info$value, c("8", "9"))
  expect_equal(info$n, c(1, 2))
  # Weighted counts sum the weights of the declared-missing subset.
  winfo <- spicy:::.user_na_info(v[mask], weights = c(2, 3, 1))
  expect_identical(winfo$value, c("8", "9"))
  expect_equal(winfo$n, c(1, 5))
  # No label exists, so "labels" mode cannot fall back to anything but
  # the bare code either.
  linfo <- spicy:::.user_na_info(v[mask], labelled_levels = "labels")
  expect_identical(linfo$value, c("8", "9"))
})


# ---- table_categorical.R line 2298: margin key collision loop ------------

test_that("table_categorical() skips past an occupied Total_1 margin key", {
  # `by` carries levels literally named "Total" AND "Total_1", so the
  # auto-rename loop must iterate before it finds a free key.
  d <- data.frame(
    v1 = factor(rep(c("a", "b"), 18)),
    g = factor(rep(c("Total", "Total_1", "Z"), each = 12))
  )
  cond <- tryCatch(
    utils::capture.output(table_categorical(d, select = v1, by = g)),
    warning = function(w) w
  )
  expect_s3_class(cond, "spicy_renamed_column")
  expect_match(conditionMessage(cond), "\"Total_2\"", fixed = TRUE)

  suppressWarnings(
    utils::capture.output(res <- table_categorical(d, select = v1, by = g))
  )
  expect_identical(attr(res, "total_group"), "Total_2")
  # The user's own "Total" / "Total_1" groups keep their names and the
  # true margin is present under the free key.
  expect_true(all(
    c("Total n", "Total_1 n", "Z n", "Total_2 n") %in%
      names(as.data.frame(res))
  ))
})


# ---- table_categorical.R line 2362: declared-missing ledger, drop_na ------

test_that("table_categorical() discloses declared missing under drop_na", {
  skip_if_not_installed("haven")
  d <- data.frame(g = factor(rep(c("A", "B"), each = 4)))
  d$v1 <- haven::labelled_spss(
    c(1, 2, 9, 1, 2, 1, 9, 2),
    labels = c(No = 1, Yes = 2, Refused = 9),
    na_values = 9
  )
  # drop_na = TRUE removes the two declared-missing rows; the grouped
  # path records them in the per-variable ledger, which becomes the
  # "Declared missing values removed" note.
  suppressWarnings(
    utils::capture.output(
      res <- table_categorical(d, select = v1, by = g, drop_na = TRUE)
    )
  )
  expect_identical(
    attr(res, "missing_note"),
    "Declared missing values removed: v1 (2)."
  )
  df <- as.data.frame(res)
  expect_identical(as.character(df$Level), c("[1] No", "[2] Yes"))
  expect_equal(df[["Total n"]], c(3, 3))
})


# ---- table_categorical.R lines 3324-3328: gt block separators -------------

test_that("table_categorical(output = 'gt') draws the light block separators", {
  skip_if_not_installed("gt")
  d <- data.frame(
    g = factor(rep(c("A", "B"), each = 6)),
    v1 = factor(rep(c("a", "b", "c"), 4)),
    v2 = factor(rep(c("x", "y"), 6))
  )
  # Two variables -> one separator, drawn on the body row just above the
  # second variable's header row (v1 header + 3 levels = rows 1-4, so
  # the rule sits on row 4).
  tbl <- suppressWarnings(
    table_categorical(d, select = c(v1, v2), by = g, output = "gt")
  )
  expect_s3_class(tbl, "gt_tbl")
  styles <- tbl[["_styles"]]
  is_light <- vapply(
    styles$styles,
    function(s) any(grepl("#cccccc", unlist(s), fixed = TRUE)),
    logical(1)
  )
  expect_true(any(is_light))
  expect_identical(unique(styles$locname[is_light]), "data")
  expect_identical(unique(styles$rownum[is_light]), 4L)
  flat <- unlist(styles$styles[is_light][[1]])
  expect_identical(unname(flat[["cell_border_bottom.side"]]), "bottom")
  expect_identical(unname(flat[["cell_border_bottom.width"]]), "0.5px")
})


# ---- abort.R line 348: formula cluster, nothing available to suggest -----

test_that("resolve_cluster ends the hint when no columns are available", {
  # A fit whose model.frame() cannot be built and that carries no `data`
  # leaves cluster_lookup_data() with nothing to offer, so the hint
  # stops after "Looked in: model.frame(fit)." instead of listing
  # available columns.
  fake <- structure(list(), class = "spicy_fake_fit_for_cluster")
  src <- spicy:::cluster_lookup_data(fake, "region")
  expect_identical(src$available, character(0)) # precondition
  expect_identical(src$missing, "region")
  err <- tryCatch(
    spicy:::resolve_cluster(~region, fake),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "formula references unknown variable(s): \"region\".",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err),
    "Looked in: model.frame(fit).",
    fixed = TRUE
  )
  expect_false(
    grepl("Available there", conditionMessage(err), fixed = TRUE)
  )
})


# ---- abort.R line 366: string cluster, no data attached to the fit -------

test_that("resolve_cluster names the no-data case for a string cluster", {
  fake <- structure(list(), class = "spicy_fake_fit_for_cluster")
  err <- tryCatch(
    spicy:::resolve_cluster("region", fake),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "`cluster = \"region\"`: column not found.",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err),
    "Available: <no data attached to the fit>.",
    fixed = TRUE
  )
  # Contrast: a real fit lists what it does have (here the model's own
  # `data`, so every column of mtcars).
  fit <- lm(mpg ~ wt, data = mtcars)
  err2 <- tryCatch(
    spicy:::resolve_cluster("region", fit, arg_label = "cluster"),
    error = function(e) e
  )
  expect_match(
    conditionMessage(err2),
    "Available: \"mpg\", \"cyl\", \"disp\"",
    fixed = TRUE
  )
})


# ---- selection_helpers.R line 52: tidyselect helper resolves to a name ----

test_that("resolve_single_column_selection returns the tidyselect match name", {
  d <- data.frame(grp = c("a", "b"), v1 = c(1, 2), v2 = c(3, 4))
  # A tidyselect helper is neither a bare column symbol nor a character
  # scalar, and it cannot be evaluated outside a selecting context, so
  # it falls through to eval_select(); a single match returns its name.
  expect_identical(
    spicy:::resolve_single_column_selection(
      rlang::quo(tidyselect::starts_with("gr")),
      d,
      "by"
    ),
    "grp"
  )
  # Two matches are refused with the classed error.
  err <- tryCatch(
    spicy:::resolve_single_column_selection(
      rlang::quo(tidyselect::starts_with("v")),
      d,
      "by"
    ),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_missing_column")
  expect_match(
    conditionMessage(err),
    "`by` must select exactly one column in `data`.",
    fixed = TRUE
  )
})


# ---- regression_transform.R line 150: aligned frame without is_intercept --

test_that("apply_keep_drop_filter falls back when is_intercept is absent", {
  # `is_intercept` is the column the intercept exemption reads. A frame
  # built without it falls back to an all-FALSE vector, so no row is
  # exempt and the intercept obeys `keep` like any other term.
  fr <- list(spicy:::as_regression_frame(
    lm(mpg ~ wt + hp, data = mtcars),
    model_id = "M1"
  ))
  aligned <- spicy:::align_frames(fr, model_ids = "M1")
  expect_true("is_intercept" %in% names(aligned$coefs_aligned))
  # With the column present the intercept is exempt and survives.
  kept <- spicy:::apply_keep_drop_filter(aligned, keep = "^wt$")
  expect_setequal(unique(kept$coefs_aligned$term), c("(Intercept)", "wt"))
  # Without it, only the matching term survives.
  stripped <- aligned
  stripped$coefs_aligned$is_intercept <- NULL
  out <- spicy:::apply_keep_drop_filter(stripped, keep = "^wt$")
  expect_identical(out$coefs_aligned$term, "wt")
  expect_identical(out$term_order, "wt")
})
