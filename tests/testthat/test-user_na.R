# ---------------------------------------------------------------------------
# Declared missing values (user_na), package-wide.
#
# Cross-software oracle: PSPP 2.0 (SPSS clone) run on 2026-07-23 with
# tools/validation/usermissing.sps (frozen output:
# tools/validation/usermissing_pspp.csv). The 60-case dataset declares
# y codes 8 (DK) and 9 (Refused) as missing via MISSING VALUES y (8, 9):
#   FREQUENCIES  -> Valid N = 44, Missing = 16, Valid Percent
#                   31.8 / 45.5 / 22.7, DK / Refused / sysmis listed in
#                   the Missing block with their labels.
#   CROSSTABS    -> N = 44, Pearson chi-squared = 4.8906241, df = 2,
#                   p = .087.
# The row-wise pins reproduce the PSPP MEAN.n / SUM.n / NMISS oracle of
# the Phase-1 audit (MISSING VALUES b (-99)).
# ---------------------------------------------------------------------------

.una_y <- function() {
  haven::labelled_spss(
    c(rep(1, 14), rep(2, 20), rep(3, 10), rep(8, 6), rep(9, 7), rep(NA, 3)),
    labels = c(Agree = 1, Neutral = 2, Disagree = 3, DK = 8, Refused = 9),
    na_values = c(8, 9)
  )
}

.una_g <- function() {
  haven::labelled(
    c(
      rep(1, 10),
      rep(2, 4),
      rep(1, 8),
      rep(2, 12),
      rep(1, 3),
      rep(2, 7),
      rep(1, 4),
      rep(2, 2),
      rep(1, 5),
      rep(2, 2),
      rep(1, 2),
      rep(2, 1)
    ),
    labels = c(Low = 1, High = 2)
  )
}

test_that("PSPP oracle: freq() valid percents honor declared missing values", {
  skip_if_not_installed("haven")
  f <- freq(.una_y(), output = "data.frame")

  expect_equal(
    f$value,
    c("[1] Agree", "[2] Neutral", "[3] Disagree", "[8] DK", "[9] Refused", NA)
  )
  expect_equal(f$n, c(14, 20, 10, 6, 7, 3))
  # PSPP FREQUENCIES Valid Percent: 31.8 / 45.5 / 22.7 (exact 14/44 etc.)
  expect_equal(
    f$valid_prop,
    c(14 / 44, 20 / 44, 10 / 44, NA, NA, NA)
  )
  expect_equal(f$prop, c(14, 20, 10, 6, 7, 3) / 60)

  fd <- freq(.una_y())
  expect_identical(attr(fd, "n_total"), 60L)
  expect_identical(attr(fd, "n_valid"), 44L)
  expect_identical(attr(fd, "user_na_rows"), 4:5)
})

test_that("PSPP oracle: cross_tab() chi-squared matches SPSS semantics", {
  skip_if_not_installed("haven")
  y <- .una_y()
  g <- .una_g()
  ct <- cross_tab(y, g)

  expect_equal(attr(ct, "n_total"), 44)
  expect_equal(attr(ct, "chi2"), 4.8906241, tolerance = 1e-7)
  expect_equal(attr(ct, "df"), 2)
  expect_equal(attr(ct, "p_value"), 0.0866991, tolerance = 1e-6)
  note <- attr(ct, "note")
  expect_match(note, "Missing values removed: y (3).", fixed = TRUE)
  expect_match(
    note,
    "Declared missing values removed: y (13).",
    fixed = TRUE
  )
})

test_that("freq() prints declared missing values inside the Missing block", {
  skip_if_not_installed("haven")
  out <- capture.output(print(freq(.una_y())))
  missing_line <- grep("^ Missing", out, value = TRUE)
  expect_length(missing_line, 1L)
  expect_match(missing_line, "[8] DK", fixed = TRUE)
  expect_true(any(grepl("[9] Refused", out, fixed = TRUE)))
})

test_that("freq(user_na = FALSE) restores the pre-0.13.0 numbers", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(
    c(1, 1, 2, 2, 2, 9, 9, NA),
    labels = c(No = 1, Yes = 2, Refused = 9),
    na_values = 9
  )
  f_old <- freq(x, user_na = FALSE, output = "data.frame")
  expect_equal(f_old$value, c("[1] No", "[2] Yes", "[9] Refused", NA))
  expect_equal(f_old$valid_prop, c(2 / 7, 3 / 7, 2 / 7, NA))

  f_new <- freq(x, output = "data.frame")
  expect_equal(f_new$valid_prop, c(2 / 5, 3 / 5, NA, NA))
  expect_identical(attr(freq(x), "n_valid"), 5L)
  expect_identical(attr(freq(x, user_na = FALSE), "n_valid"), 7L)
})

test_that("freq() handles na_range declarations, including unlabelled codes", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(
    c(1, 2, 2, 97, 98, 99, NA),
    labels = c(Low = 1, High = 2, DK = 97),
    na_range = c(97, 99)
  )
  f <- freq(x, output = "data.frame")
  expect_equal(f$value, c("[1] Low", "[2] High", "[97] DK", "98", "99", NA))
  expect_equal(f$n, c(1, 2, 1, 1, 1, 1))
  expect_equal(f$valid_prop[1:2], c(1 / 3, 2 / 3))
  expect_true(all(is.na(f$valid_prop[3:6])))
})

test_that("freq() breaks tagged NAs out with their value labels", {
  skip_if_not_installed("haven")
  z <- haven::labelled(
    c(
      1,
      2,
      2,
      haven::tagged_na("a"),
      haven::tagged_na("a"),
      haven::tagged_na("b"),
      NA
    ),
    labels = c(
      Yes = 1,
      No = 2,
      Refused = haven::tagged_na("a"),
      `Not applicable` = haven::tagged_na("b")
    )
  )
  f <- freq(z, output = "data.frame")
  expect_equal(
    f$value,
    c("[1] Yes", "[2] No", "[NA(a)] Refused", "[NA(b)] Not applicable", NA)
  )
  expect_equal(f$n, c(1, 2, 2, 1, 1))
  expect_equal(f$valid_prop[1:2], c(1 / 3, 2 / 3))

  # user_na = FALSE collapses the breakdown back into the NA row
  # (tagged NAs stay missing: they are genuine NA).
  f_off <- freq(z, user_na = FALSE, output = "data.frame")
  expect_equal(f_off$value, c("[1] Yes", "[2] No", NA))
  expect_equal(f_off$n, c(1, 2, 4))
})

test_that("freq() shows bare tags for tagged NAs in a plain double", {
  skip_if_not_installed("haven")
  p <- c(1, 2, 2, haven::tagged_na("a"), NA)
  f <- freq(p, output = "data.frame")
  expect_equal(f$value, c("1", "2", "NA(a)", NA))
  expect_equal(f$n, c(1, 2, 1, 1))
})

test_that("freq() labelled_levels applies to declared-missing rows", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(
    c(1, 2, 9, NA),
    labels = c(No = 1, Yes = 2, Refused = 9),
    na_values = 9
  )
  f_lab <- freq(x, labelled_levels = "labels", output = "data.frame")
  expect_equal(f_lab$value, c("No", "Yes", "Refused", NA))
  f_val <- freq(x, labelled_levels = "values", output = "data.frame")
  expect_equal(f_val$value, c("1", "2", "9", NA))
})

test_that("freq() weighted counts include declared-missing weights", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(
    c(1, 1, 2, 9, NA),
    labels = c(No = 1, Yes = 2, Refused = 9),
    na_values = 9
  )
  w <- c(2, 1, 1, 3, 2)
  f <- freq(x, weights = w, output = "data.frame")
  expect_equal(f$value, c("[1] No", "[2] Yes", "[9] Refused", NA))
  expect_equal(f$n, c(3, 1, 3, 2))
  expect_equal(f$valid_prop, c(3 / 4, 1 / 4, NA, NA))
  expect_identical(attr(freq(x, weights = w), "n_total"), 9)
})

test_that("freq() sort and cum keep declared-missing rows in place", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(
    c(1, 2, 2, 2, 9, 9, NA),
    labels = c(No = 1, Yes = 2, Refused = 9),
    na_values = 9
  )
  f <- freq(x, sort = "+", cum = TRUE, output = "data.frame")
  expect_equal(f$value, c("[1] No", "[2] Yes", "[9] Refused", NA))
  expect_equal(f$cum_prop, cumsum(f$n) / 7)
  expect_equal(f$cum_valid_prop, c(1 / 4, 1, NA, NA))
})

test_that("freq() factor_levels = 'all' does not resurface declared labels", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(
    c(1, 1, 9, NA),
    labels = c(No = 1, Yes = 2, Refused = 9),
    na_values = 9
  )
  f <- freq(x, factor_levels = "all", output = "data.frame")
  # Unused VALID label kept with n = 0; the declared label appears only
  # once, in the Missing block.
  expect_equal(f$value, c("[1] No", "[2] Yes", "[9] Refused", NA))
  expect_equal(f$n, c(2, 0, 1, 1))
})

test_that("freq() handles an all-declared-missing vector", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(
    c(9, 9, 9),
    labels = c(Refused = 9),
    na_values = 9
  )
  f <- freq(x, output = "data.frame")
  expect_equal(f$value, "[9] Refused")
  expect_equal(f$n, 3)
  expect_equal(f$prop, 1)
  expect_identical(attr(freq(x), "n_valid"), 0L)
})

test_that("freq() leaves plain labelled vectors untouched", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, 2, 3, NA), labels = c(A = 1, B = 2, C = 3))
  expect_identical(
    freq(x, output = "data.frame"),
    freq(x, user_na = FALSE, output = "data.frame")
  )
  expect_identical(attr(freq(x), "n_valid"), 3L)
})

test_that("cross_tab(user_na = FALSE) tabulates declared codes as categories", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(
    c(1, 1, 2, 2, 2, 9, 9, NA),
    labels = c(No = 1, Yes = 2, Refused = 9),
    na_values = 9
  )
  y <- haven::labelled(c(1, 2, 1, 2, 1, 2, 1, 2), labels = c(Low = 1, High = 2))

  ct_on <- cross_tab(x, y, include_stats = FALSE)
  expect_equal(attr(ct_on, "n_total"), 5)
  expect_equal(ct_on$Values[seq_len(2)], c("1", "2"))

  ct_off <- cross_tab(x, y, include_stats = FALSE, user_na = FALSE)
  expect_equal(attr(ct_off, "n_total"), 7)
  expect_equal(ct_off$Values[seq_len(3)], c("1", "2", "9"))
  note_off <- attr(ct_off, "note")
  expect_false(grepl(
    "Declared missing",
    if (is.null(note_off)) "" else note_off
  ))
})

test_that("cross_tab() by honors user_na for declared-missing group values", {
  skip_if_not_installed("haven")
  d <- data.frame(id = 1:12)
  d$x <- factor(rep(c("a", "b"), 6))
  d$y <- factor(rep(c("u", "v"), each = 6))
  d$byv <- haven::labelled_spss(
    c(1, 1, 1, 1, 2, 2, 2, 2, 9, 9, 9, NA),
    labels = c(M = 1, F = 2, Ref = 9),
    na_values = 9
  )
  res <- cross_tab(d, x, y, by = byv, include_stats = FALSE)
  expect_named(res, c("1", "2"))
  expect_match(
    attr(res[[1]], "note"),
    "Rows with missing byv removed: 4.",
    fixed = TRUE
  )

  res_off <- cross_tab(
    d,
    x,
    y,
    by = byv,
    include_stats = FALSE,
    user_na = FALSE
  )
  expect_named(res_off, c("1", "2", "9"))
})

test_that("cross_tab() note discloses declared missing on both variables", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(
    c(1, 1, 2, 2, 2, 9, 9, NA),
    labels = c(No = 1, Yes = 2, Refused = 9),
    na_values = 9
  )
  y2 <- haven::labelled_spss(
    c(1, 2, 8, 2, 1, 2, NA, 2),
    labels = c(Low = 1, High = 2, DK = 8),
    na_values = 8
  )
  note <- attr(cross_tab(x, y2, include_stats = FALSE), "note")
  expect_match(
    note,
    "Missing values removed: x (1), y2 (1); 2 rows in total.",
    fixed = TRUE
  )
  expect_match(
    note,
    "Declared missing values removed: x (2), y2 (1); 3 rows in total.",
    fixed = TRUE
  )
})

test_that("table_categorical() folds declared missing into (Missing)", {
  skip_if_not_installed("haven")
  d <- data.frame(id = 1:8)
  d$y <- haven::labelled_spss(
    c(1, 2, 2, 8, 8, 9, 9, NA),
    labels = c(Agree = 1, Neutral = 2, DK = 8, Refused = 9),
    na_values = c(8, 9)
  )
  tc <- table_categorical(d, select = y, output = "data.frame")
  expect_equal(tc$Level, c("1", "2", "(Missing)"))
  expect_equal(tc$n, c(1, 2, 5))

  tc_off <- table_categorical(
    d,
    select = y,
    user_na = FALSE,
    output = "data.frame"
  )
  expect_equal(tc_off$Level, c("1", "2", "8", "9", "(Missing)"))
  expect_equal(tc_off$n, c(1, 2, 2, 2, 1))
})

test_that("table_categorical(drop_na = TRUE) discloses declared removals", {
  skip_if_not_installed("haven")
  d <- data.frame(id = 1:8)
  d$y <- haven::labelled_spss(
    c(1, 2, 2, 8, 8, 9, 9, NA),
    labels = c(Agree = 1, Neutral = 2, DK = 8, Refused = 9),
    na_values = c(8, 9)
  )
  out <- capture.output(table_categorical(d, select = y, drop_na = TRUE))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "Missing values removed: y (1).", fixed = TRUE)
  expect_match(txt, "Declared missing values removed: y (4).", fixed = TRUE)
})

test_that("table_categorical() grouped honors user_na in by", {
  skip_if_not_installed("haven")
  d <- data.frame(id = 1:12)
  d$x <- factor(rep(c("a", "b"), 6))
  d$byv <- haven::labelled_spss(
    c(1, 1, 1, 1, 2, 2, 2, 2, 9, 9, 9, NA),
    labels = c(M = 1, F = 2, Ref = 9),
    na_values = 9
  )
  tc <- table_categorical(d, select = x, by = byv, output = "data.frame")
  expect_true("(Missing) n" %in% names(tc))
  expect_equal(sum(tc[["(Missing) n"]]), 4)

  tc_drop <- table_categorical(
    d,
    select = x,
    by = byv,
    drop_na = TRUE,
    output = "data.frame"
  )
  expect_false("(Missing) n" %in% names(tc_drop))
  expect_false("9 n" %in% names(tc_drop))

  tc_off <- table_categorical(
    d,
    select = x,
    by = byv,
    user_na = FALSE,
    output = "data.frame"
  )
  expect_true("9 n" %in% names(tc_off))
})

test_that("table_continuous() excludes declared missing from summaries", {
  skip_if_not_installed("haven")
  da <- data.frame(i = 1:6)
  da$a <- haven::labelled_spss(
    c(20, 30, 40, 999, 999, NA),
    labels = c(Refused = 999),
    na_values = 999
  )
  tc <- table_continuous(da, select = a, output = "data.frame")
  expect_equal(tc$mean, 30)
  expect_equal(tc$n, 3L)

  out <- capture.output(table_continuous(da, select = a))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "Missing values removed: a (1).", fixed = TRUE)
  expect_match(txt, "Declared missing values removed: a (2).", fixed = TRUE)

  tc_off <- table_continuous(
    da,
    select = a,
    user_na = FALSE,
    output = "data.frame"
  )
  expect_equal(tc_off$mean, mean(c(20, 30, 40, 999, 999)))
  expect_equal(tc_off$n, 5L)
})

test_that("table_continuous() declared-missing by values form no group", {
  skip_if_not_installed("haven")
  da <- data.frame(a = c(10, 20, 30, 40, 50, 60))
  da$byv <- haven::labelled_spss(
    c(1, 1, 2, 2, 9, NA),
    labels = c(M = 1, F = 2, Ref = 9),
    na_values = 9
  )
  tc <- suppressWarnings(
    table_continuous(da, select = a, by = byv, output = "data.frame")
  )
  expect_equal(unique(tc$group), c("1", "2"))

  tc_keep <- table_continuous(
    da,
    select = a,
    by = byv,
    drop_na = FALSE,
    output = "data.frame"
  )
  expect_true("(Missing)" %in% tc_keep$group)
  expect_equal(tc_keep$n[tc_keep$group == "(Missing)"], 2L)
})

test_that("table_continuous_lm() honors user_na in outcomes", {
  skip_if_not_installed("haven")
  da <- data.frame(g = factor(rep(c("x", "y"), 3)))
  da$a <- haven::labelled_spss(
    c(20, 30, 40, 999, 999, NA),
    labels = c(Refused = 999),
    na_values = 999
  )
  lm_on <- table_continuous_lm(da, select = a, by = g, output = "data.frame")
  expect_equal(lm_on$n, 3)
  lm_off <- table_continuous_lm(
    da,
    select = a,
    by = g,
    user_na = FALSE,
    output = "data.frame"
  )
  expect_equal(lm_off$n, 5)
})

test_that("PSPP oracle: mean_n() / sum_n() honor declared missing values", {
  skip_if_not_installed("haven")
  d <- data.frame(a = c(1, 1, 1, NA, 1, 4), c = c(3, NA, NA, NA, 3, NA))
  d$b <- haven::labelled_spss(
    c(2, 2, NA, NA, -99, -99),
    labels = c(Refused = -99),
    na_values = -99
  )
  d <- d[, c("a", "b", "c")]

  # PSPP: MEAN.2 = 2, 1.5, SYSMIS, SYSMIS, 2, SYSMIS
  expect_equal(mean_n(d, min_valid = 2), c(2, 1.5, NA, NA, 2, NA))
  # PSPP: SUM.1 = 6, 3, 1, SYSMIS, 4, 4
  expect_equal(sum_n(d, min_valid = 1), c(6, 3, 1, NA, 4, 4))
  # PSPP: NMISS = 0 1 2 3 1 2
  expect_equal(count_n(d, special = "NA"), c(0, 1, 2, 3, 1, 2))

  # min_valid gate: row 6 has one SPSS-valid value only.
  expect_true(is.na(mean_n(d, min_valid = 2)[6]))

  # Escape hatch restores the raw-codes numbers.
  expect_equal(
    round(mean_n(d, min_valid = 2, user_na = FALSE), 2),
    c(2, 1.5, NA, NA, -31.67, -47.5)
  )
  expect_equal(
    sum_n(d, min_valid = 1, user_na = FALSE),
    c(6, 3, 1, NA, -95, -95)
  )
})

test_that("count_n() user_na toggles the special = 'NA' definition only", {
  skip_if_not_installed("haven")
  d <- data.frame(a = c(1, NA, 3))
  d$b <- haven::labelled_spss(
    c(-99, 2, -99),
    labels = c(Refused = -99),
    na_values = -99
  )
  expect_equal(count_n(d, special = "NA"), c(1, 1, 1))
  expect_equal(count_n(d, special = "NA", user_na = FALSE), c(0, 1, 0))
  # count = matches the underlying code in both modes.
  expect_equal(count_n(d, count = -99), c(1, 0, 1))
  expect_equal(count_n(d, count = -99, user_na = FALSE), c(1, 0, 1))
})

test_that("varlist() count columns share one missing definition", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(
    c(1, 2, 8, 9, 1, NA),
    labels = c(Agree = 1, Disagree = 2, DK = 8, Refused = 9),
    na_values = c(8, 9)
  )
  v <- varlist(data.frame(x = x), tbl = TRUE)
  expect_identical(unname(v$N_distinct), 2L)
  expect_identical(unname(v$N_valid), 3L)
  expect_identical(unname(v$NAs), 3L)

  v_off <- varlist(data.frame(x = x), tbl = TRUE, user_na = FALSE)
  expect_identical(unname(v_off$N_distinct), 4L)
  expect_identical(unname(v_off$N_valid), 5L)
  expect_identical(unname(v_off$NAs), 1L)
})

test_that("varlist() na_range codes stay visible in Values", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(
    c(1, 2, 97, 98, 99, NA),
    labels = c(Low = 1, High = 2),
    na_range = c(97, 99)
  )
  v <- varlist(
    data.frame(x = x),
    tbl = TRUE,
    values = TRUE,
    include_na = TRUE
  )
  expect_match(v$Values, "[97] 97", fixed = TRUE)
  expect_match(v$Values, "[98] 98", fixed = TRUE)
  expect_match(v$Values, "[99] 99", fixed = TRUE)
  expect_identical(unname(v$N_distinct), 2L)
  expect_identical(unname(v$N_valid), 2L)
  expect_identical(unname(v$NAs), 4L)
})

test_that("varlist() surfaces tagged-NA value labels in Values", {
  skip_if_not_installed("haven")
  x <- haven::labelled(
    c(1, 2, 1, haven::tagged_na("a"), haven::tagged_na("b"), NA),
    labels = c(
      Yes = 1,
      No = 2,
      Refused = haven::tagged_na("a"),
      DontKnow = haven::tagged_na("b")
    )
  )
  v <- varlist(data.frame(x = x), tbl = TRUE, values = TRUE)
  expect_match(v$Values, "[NA(a)] Refused", fixed = TRUE)
  expect_match(v$Values, "[NA(b)] DontKnow", fixed = TRUE)
  expect_identical(unname(v$N_distinct), 2L)
  expect_identical(unname(v$N_valid), 3L)
  expect_identical(unname(v$NAs), 3L)
})

test_that("varlist() N_distinct uses is.na() on list and POSIXlt columns", {
  d <- data.frame(id = 1:3)
  d$lst <- list(1, 1, NA)
  expect_identical(unname(varlist(d, tbl = TRUE)$N_distinct[2]), 1L)

  t <- tibble::tibble(
    lt = as.POSIXlt(
      c("2020-01-01 10:00:00", "2020-01-01 10:00:00", NA),
      tz = "UTC"
    )
  )
  expect_identical(unname(varlist(t, tbl = TRUE)$N_distinct), 1L)
})

test_that("code_book() passes user_na through to varlist()", {
  skip_if_not_installed("haven")
  skip_if_not_installed("DT")
  x <- haven::labelled_spss(
    c(1, 2, 8, 9, 1, NA),
    labels = c(Agree = 1, Disagree = 2, DK = 8, Refused = 9),
    na_values = c(8, 9)
  )
  d <- data.frame(x = x)
  cb <- code_book(d, user_na = FALSE)
  payload <- cb$x$data
  ref <- varlist(d, tbl = TRUE, user_na = FALSE, factor_levels = "all")
  expect_identical(unname(payload$N_valid), unname(ref$N_valid))
  expect_identical(unname(payload$N_distinct), unname(ref$N_distinct))
})

test_that("user_na argument is validated across the family", {
  df <- data.frame(x = 1:3)
  expect_error(freq(df$x, user_na = "yes"), class = "spicy_invalid_input")
  expect_error(
    cross_tab(df$x, df$x, user_na = NA),
    class = "spicy_invalid_input"
  )
  expect_error(mean_n(df, user_na = 1), class = "spicy_invalid_input")
  expect_error(
    count_n(df, count = 1, user_na = "no"),
    class = "spicy_invalid_input"
  )
  expect_error(
    varlist(df, tbl = TRUE, user_na = NA),
    class = "spicy_invalid_input"
  )
})

test_that("user_na internals handle non-atomic and empty inputs", {
  expect_identical(spicy:::.user_na_mask(list(1, 2)), c(FALSE, FALSE))
  expect_false(spicy:::.has_user_na(c(1, 2, NA)))
  info <- spicy:::.user_na_info(double(0))
  expect_identical(nrow(info), 0L)
  # Pass-through for vectors without declarations.
  expect_identical(spicy:::.user_na_to_na(1:3), 1:3)
  expect_identical(spicy:::.user_na_zap(letters), letters)
})
