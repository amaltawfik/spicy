# `smd = TRUE` in table_continuous() -- the Table 1 balance column.
#
# The kernels and their oracles live in test-smd.R; this file pins the
# COLUMN: what it publishes, where it appears, what it refuses, and what
# it leaves untouched when it is off (the default).

.smd_tc_data <- function() {
  data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    x = c(1, 2, 4, 5, 2, 3, 8),
    w = c(1, 2, 1, 3, 2, 1, 1),
    stringsAsFactors = FALSE
  )
}

test_that("the SMD column carries Austin's value, not the effect size's", {
  d <- .smd_tc_data()
  r <- table_continuous(
    d,
    select = x,
    by = g,
    smd = TRUE,
    p_value = FALSE,
    output = "long"
  )
  # tableone 0.13.2 `ExtractSmd` -> 0.5100613704506704593200. The sign
  # is ours: group 1 minus group 2 in DISPLAY order (cobalt's
  # `col_w_smd(abs = FALSE)` returns +0.51..., i.e. B - A, because it
  # guesses the second level as "treated"; do not align to it).
  expect_equal(r$smd_value[[1L]], -0.51006137045067045932, tolerance = 1e-15)
  expect_identical(r$smd_type[[1L]], "continuous")
  # Only the first group row of a variable block carries it.
  expect_true(is.na(r$smd_value[[2L]]))
  expect_true(is.na(r$smd_type[[2L]]))
  # And it is NOT Hedges' g, which the same table can show beside it:
  # Austin's denominator is the mean of the two group variances, g's is
  # the degrees-of-freedom pooled SD. They part company here because
  # n1 = 4 != n2 = 3.
  g_only <- table_continuous(
    d,
    select = x,
    by = g,
    effect_size = "hedges_g",
    output = "long"
  )
  expect_equal(
    g_only$es_value[[1L]],
    -0.4533742333121392742434,
    tolerance = 1e-15
  )
  expect_false(isTRUE(all.equal(r$smd_value[[1L]], g_only$es_value[[1L]])))
})

test_that("the SMD cell, the typed body and the note agree", {
  d <- .smd_tc_data()
  tbl <- table_continuous(d, select = x, by = g, smd = TRUE, p_value = FALSE)
  txt <- paste(capture.output(print(tbl)), collapse = "\n")
  expect_match(txt, "SMD", fixed = TRUE)
  expect_match(txt, "-0.51", fixed = TRUE)
  expect_match(
    txt,
    "SMD = standardized mean difference (A - B); |SMD| > 0.1 is the usual imbalance threshold.",
    fixed = TRUE
  )
  st <- as_structured(tbl)
  expect_true(.CON_KEY_SMD %in% names(st$body))
  expect_equal(
    st$body[[.CON_KEY_SMD]][[1L]],
    -0.51006137045067045932,
    tolerance = 1e-15
  )
  meta <- st$col_meta[[.CON_KEY_SMD]]
  expect_identical(meta$token, "smd")
  expect_identical(meta$display_label, "SMD")
  expect_identical(meta$precision, 2L)
  # A k-level SMD is unbounded (1.11, 2.45 on the pinned fixtures), so
  # neither the APA leading-zero strip nor the [-1, 1] range check the
  # association column carries may apply here.
  expect_null(meta$p_style)
  expect_null(meta$value_range)
  # Not composite: the cell is the bare number, so `inline()` cites the
  # very string the table prints.
  expect_identical(inline(tbl, x, "A", column = "smd"), "-0.51")
})

test_that("the threshold in the gloss follows decimal_mark", {
  # Decision 29-C: the threshold is a displayed NUMBER inside a label,
  # so it takes the table's mark -- a comma under `decimal_mark = ","`,
  # the midline dot under the Lancet style.
  d <- .smd_tc_data()
  fr <- paste(
    capture.output(print(table_continuous(
      d,
      select = x,
      by = g,
      smd = TRUE,
      p_value = FALSE,
      decimal_mark = ","
    ))),
    collapse = "\n"
  )
  expect_match(fr, "|SMD| > 0,1 is the usual", fixed = TRUE)
  expect_false(grepl("> 0.1 is", fr, fixed = TRUE))
  lancet <- paste(
    capture.output(print(table_continuous(
      d,
      select = x,
      by = g,
      smd = TRUE,
      p_value = FALSE,
      style = "lancet"
    ))),
    collapse = "\n"
  )
  # U+00B7 written as an escape so this file stays pure ASCII.
  expect_match(
    lancet,
    paste0("|SMD| > 0", "\u00b7", "1 is the usual"),
    fixed = TRUE
  )
})

test_that("the SMD is computed on the weighted moments the table displays", {
  d <- .smd_tc_data()
  r <- table_continuous(
    d,
    select = x,
    by = g,
    smd = TRUE,
    weights = w,
    p_value = FALSE,
    output = "long"
  )
  # Frequency convention (decision 17): the SMD of the expanded data.
  expect_equal(
    r$smd_value[[1L]],
    -0.1358139271300619344007,
    tolerance = 1e-15
  )
  dup <- d[rep(seq_len(nrow(d)), d$w), , drop = FALSE]
  rd <- table_continuous(
    dup,
    select = x,
    by = g,
    smd = TRUE,
    p_value = FALSE,
    output = "long"
  )
  expect_equal(r$smd_value[[1L]], rd$smd_value[[1L]], tolerance = 1e-15)
  # `rescale = TRUE` restores scale invariance and is the recommended
  # form for sampling weights until the `_svy` twins land.
  rr <- table_continuous(
    d,
    select = x,
    by = g,
    smd = TRUE,
    weights = w,
    rescale = TRUE,
    p_value = FALSE,
    output = "long"
  )
  expect_equal(rr$smd_value[[1L]], -0.12392769374077624, tolerance = 1e-15)
  # ... and without it the weights carry their own scale, on purpose.
  d10 <- d
  d10$w <- d$w * 10
  r10 <- table_continuous(
    d10,
    select = x,
    by = g,
    smd = TRUE,
    weights = w,
    p_value = FALSE,
    output = "long"
  )
  expect_equal(r10$smd_value[[1L]], -0.15225682298376567, tolerance = 1e-15)
})

test_that("the SMD refuses more than two groups, on the REAL groups only", {
  d <- .smd_tc_data()
  d3 <- rbind(d, data.frame(g = "C", x = 4, w = 1))
  d3$g <- factor(d3$g, levels = c("A", "B", "C"))
  expect_error(
    table_continuous(d3, select = x, by = g, smd = TRUE),
    class = "spicy_not_implemented"
  )
  msg <- tryCatch(
    table_continuous(d3, select = x, by = g, smd = TRUE),
    spicy_not_implemented = function(e) conditionMessage(e)
  )
  expect_match(
    msg,
    "requires exactly two groups in `by` (found 3)",
    fixed = TRUE
  )
  # tableone answers a three-group `by` with the AVERAGE of the pairwise
  # SMDs; there is no published reading of that number, and an average
  # can sit under 0.1 while one pair sits at 0.3 -- the table would call
  # a frank imbalance balanced. Hence the classed refusal.
  expect_match(msg, "two-group balance diagnostic", fixed = TRUE)

  # ... and the "(Missing)" group is not a real group. A single missing
  # `by` value must not turn a two-group table into a refused
  # three-group one, in either `drop_na` mode.
  dna <- d
  dna$g <- factor(
    c("A", "A", "A", NA, "B", "B", "B"),
    levels = c("A", "B")
  )
  keep <- table_continuous(
    dna,
    select = x,
    by = g,
    smd = TRUE,
    drop_na = FALSE,
    p_value = FALSE,
    output = "long"
  )
  expect_false(is.na(keep$smd_value[[1L]]))
  expect_true("(Missing)" %in% keep$group)
  # The missing group never carries a value, and the value is the
  # complete-case one either way.
  expect_true(all(is.na(keep$smd_value[keep$group == "(Missing)"])))
  drop <- suppressWarnings(table_continuous(
    dna,
    select = x,
    by = g,
    smd = TRUE,
    drop_na = TRUE,
    p_value = FALSE,
    output = "long"
  ))
  expect_identical(keep$smd_value[[1L]], drop$smd_value[[1L]])
  # No row of role `missing` carries an SMD in the typed view either.
  st <- as_structured(table_continuous(
    dna,
    select = x,
    by = g,
    smd = TRUE,
    drop_na = FALSE,
    p_value = FALSE
  ))
  miss_rows <- which(st$body$.row_role == "missing")
  expect_gt(length(miss_rows), 0L)
  expect_true(all(is.na(st$body[[.CON_KEY_SMD]][miss_rows])))
})

test_that("`smd` without `by` is a request for nothing", {
  d <- .smd_tc_data()
  expect_warning(
    table_continuous(d, select = x, smd = TRUE, output = "long"),
    class = "spicy_ignored_arg"
  )
  r <- suppressWarnings(table_continuous(
    d,
    select = x,
    smd = TRUE,
    output = "long"
  ))
  # A one-way frame has no comparison columns at all -- not the test's,
  # not the effect size's, not this one.
  expect_false("smd_value" %in% names(r))
  expect_error(
    table_continuous(d, select = x, by = g, smd = "yes"),
    class = "spicy_invalid_input"
  )
})

test_that("an undefined SMD is disclosed and NA, never a number", {
  # Both groups constant at DIFFERENT values: the standardized distance
  # is infinite, and the cell must say so rather than print anything.
  d <- data.frame(
    g = factor(c("A", "A", "A", "B", "B")),
    x = c(2, 2, 2, 5, 5)
  )
  expect_warning(
    table_continuous(d, select = x, by = g, smd = TRUE, p_value = FALSE),
    class = "spicy_undefined_stat"
  )
  r <- suppressWarnings(table_continuous(
    d,
    select = x,
    by = g,
    smd = TRUE,
    p_value = FALSE,
    output = "long"
  ))
  expect_true(is.na(r$smd_value[[1L]]))
  txt <- paste(
    capture.output(suppressWarnings(print(table_continuous(
      d,
      select = x,
      by = g,
      smd = TRUE,
      p_value = FALSE
    )))),
    collapse = "\n"
  )
  expect_match(txt, spicy_str("cell_undefined"), fixed = TRUE)
  # Same value on both sides is perfect balance, not an undefined cell.
  d0 <- data.frame(g = factor(c("A", "A", "B", "B")), x = c(2, 2, 2, 2))
  r0 <- table_continuous(
    d0,
    select = x,
    by = g,
    smd = TRUE,
    p_value = FALSE,
    output = "long"
  )
  expect_identical(r0$smd_value[[1L]], 0)
  # A group of one has no variance to standardise by: silent NA, the
  # neighbouring SD cell already discloses it.
  d1 <- data.frame(g = factor(c("A", "B", "B")), x = c(1, 2, 3))
  expect_no_warning(
    r1 <- table_continuous(
      d1,
      select = x,
      by = g,
      smd = TRUE,
      p_value = FALSE,
      output = "long"
    )
  )
  expect_true(is.na(r1$smd_value[[1L]]))
})

test_that("`smd = FALSE` changes nothing, and `smd = TRUE` only appends", {
  d <- .smd_tc_data()
  off <- capture.output(print(table_continuous(d, select = x, by = g)))
  on <- capture.output(print(table_continuous(
    d,
    select = x,
    by = g,
    smd = TRUE
  )))
  # The column goes LAST -- a constraint, not a preference: the console
  # re-labels an orphaned companion column by looking LEFT for its
  # carrier, so an SMD inserted between `Test` and `p` would re-label an
  # orphaned `p` as "p (SMD)".
  keys_off <- names(
    as_structured(table_continuous(
      d,
      select = x,
      by = g
    ))$col_meta
  )
  keys_on <- names(
    as_structured(table_continuous(
      d,
      select = x,
      by = g,
      smd = TRUE
    ))$col_meta
  )
  expect_identical(keys_on, c(keys_off, .CON_KEY_SMD))
  # One column added, one note line added, nothing else: the body rows
  # differ only by the cells of that column.
  expect_length(setdiff(off, on), 4L) # header, rule, two data rows
  expect_true(any(grepl(
    "SMD = standardized mean difference",
    on,
    fixed = TRUE
  )))
  expect_false(any(grepl("SMD", off, fixed = TRUE)))
  expect_identical(
    trimws(sub("SMD *$", "", setdiff(on, off)[[1L]])),
    trimws(setdiff(off, on)[[1L]])
  )
  # The Excel right-hand rule is untouched: the SMD lands to the RIGHT
  # of `n` and `p`, so the indices it returns cannot move.
  expect_identical(
    .continuous_right_cols(keys_off),
    .continuous_right_cols(keys_on)
  )
})
