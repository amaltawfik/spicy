# Decision 17 for `table_outcome()`: the frequency-expansion
# convention, its two acceptance invariants, and what it refuses.
#
# The invariants are what make "weighted" mean something checkable:
#
#   * all weights 1 reproduces the unweighted table, to the byte;
#   * integer weights reproduce the table of the data duplicated that
#     many times.
#
# Everything numeric comes from `.prep_variable_weights()` and
# `.continuous_compute_one()`, the same producers the sibling reads,
# so these invariants are inherited rather than re-implemented -- and
# pinned here because inheritance is exactly what a refactor can break.

.tow_quiet <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(expr)))
  res
}

.tow_data <- function() {
  data.frame(
    y = c(1, 2, 3, 4, 10, 20, 30, 40, 5, 6),
    g = c("a", "a", "a", "a", "b", "b", "b", "b", "a", "b"),
    h = c("x", "x", "y", "y", "z", "z", "x", "y", "z", "x"),
    w1 = rep(1, 10L),
    wi = c(2, 1, 3, 1, 1, 2, 1, 1, 2, 1),
    stringsAsFactors = FALSE
  )
}

test_that("all weights 1 reproduces the unweighted table", {
  d <- .tow_data()
  plain <- .tow_quiet(table_outcome(d, y, by = c(g, h), p_value = FALSE))
  weighted <- .tow_quiet(table_outcome(
    d,
    y,
    by = c(g, h),
    p_value = FALSE,
    weights = w1
  ))
  expect_identical(
    attr(weighted, "display_df"),
    attr(plain, "display_df")
  )
  for (nm in c("mean", "sd", "min", "max", "ci_lower", "ci_upper", "median")) {
    expect_equal(weighted[[nm]], plain[[nm]], tolerance = 1e-12)
  }
  expect_identical(weighted$n, plain$n)
})

test_that("integer weights reproduce the duplicated data", {
  d <- .tow_data()
  expanded <- d[rep(seq_len(nrow(d)), d$wi), , drop = FALSE]
  weighted <- .tow_quiet(table_outcome(
    d,
    y,
    by = c(g, h),
    p_value = FALSE,
    weights = wi
  ))
  duplicated_tbl <- .tow_quiet(table_outcome(
    expanded,
    y,
    by = c(g, h),
    p_value = FALSE
  ))
  for (nm in c("mean", "sd", "min", "max", "ci_lower", "ci_upper", "median")) {
    expect_equal(weighted[[nm]], duplicated_tbl[[nm]], tolerance = 1e-12)
  }
  # The weighted count IS the duplicated count; the raw `n` counts the
  # rows that carried the weights, which is the honest other number.
  expect_equal(
    weighted$weighted_n,
    as.numeric(duplicated_tbl$n),
    tolerance = 1e-12
  )
})

test_that("rescale normalises over the whole sample, never per level", {
  # A per-level rescale would destroy the relative weights across
  # levels -- the entire information a sampling weight carries into
  # this table. Rescaling multiplies every weight by ONE constant, read
  # off the whole surviving sample: the means are therefore unchanged,
  # while the SDs move, because their denominator is the Bessel-style
  # `sum(w) - 1` and the sum of weights is what rescaling changes.
  #
  # Both halves are checked against the SIBLING, which is the family's
  # published behaviour: same numbers, per level, to 1e-12.
  d <- .tow_data()
  raw <- .tow_quiet(table_outcome(d, y, by = g, p_value = FALSE, weights = wi))
  scaled <- .tow_quiet(table_outcome(
    d,
    y,
    by = g,
    p_value = FALSE,
    weights = wi,
    rescale = TRUE,
    show_columns = c("m", "sd", "n", "weighted_n")
  ))
  expect_equal(scaled$mean, raw$mean, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(scaled$sd, raw$sd)))

  sib_raw <- .tow_quiet(table_continuous(
    d,
    select = y,
    by = g,
    weights = wi,
    p_value = FALSE
  ))
  sib_scaled <- .tow_quiet(table_continuous(
    d,
    select = y,
    by = g,
    weights = wi,
    rescale = TRUE,
    p_value = FALSE
  ))
  lv <- raw$.row_role == "level"
  expect_equal(raw$sd[lv], sib_raw$sd, tolerance = 1e-12)
  expect_equal(scaled$sd[lv], sib_scaled$sd, tolerance = 1e-12)
  expect_equal(raw$mean[lv], sib_raw$mean, tolerance = 1e-12)

  levels_i <- scaled$.row_role == "level"
  expect_equal(
    sum(scaled$weighted_n[levels_i]),
    scaled$weighted_n[scaled$.row_role == "summary"],
    tolerance = 1e-12
  )
  expect_equal(
    scaled$weighted_n[scaled$.row_role == "summary"],
    nrow(d),
    tolerance = 1e-12
  )
})

test_that("the weighted table says it is weighted", {
  d <- .tow_data()
  tbl <- .tow_quiet(table_outcome(d, y, by = g, p_value = FALSE, weights = wi))
  expect_match(attr(tbl, "note"), "Statistics weighted by wi.", fixed = TRUE)
})

test_that("weights refuse the inference, not the description", {
  d <- .tow_data()
  # The refusal protects INFERENCE: a p-value or an effect size read
  # against an interval must not be silently unweighted beside
  # weighted descriptives.
  err <- tryCatch(
    table_outcome(d, y, by = g, weights = w1),
    error = identity
  )
  expect_s3_class(err, "spicy_not_implemented")
  expect_match(
    conditionMessage(err),
    "table_continuous_lm(weights = )",
    fixed = TRUE
  )
  err <- tryCatch(
    table_outcome(
      d,
      y,
      by = g,
      weights = w1,
      p_value = FALSE,
      effect_size = "auto"
    ),
    error = identity
  )
  expect_s3_class(err, "spicy_not_implemented")
  # The description itself is fine.
  expect_s3_class(
    .tow_quiet(table_outcome(d, y, by = g, weights = w1, p_value = FALSE)),
    "spicy_outcome_table"
  )
})

test_that("the two token guards of decision 17 hold", {
  d <- .tow_data()
  # The order-statistic median interval has no weighted version.
  err <- tryCatch(
    table_outcome(
      d,
      y,
      by = g,
      weights = w1,
      p_value = FALSE,
      show_columns = c("med", "med_ci")
    ),
    error = identity
  )
  expect_s3_class(err, "spicy_not_implemented")
  expect_match(conditionMessage(err), "order-statistic", fixed = TRUE)
  # And a weighted count has nothing to show without weights.
  err <- tryCatch(
    table_outcome(d, y, by = g, show_columns = c("m", "weighted_n")),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
})

test_that("invalid weights are refused, and NA weights are disclosed", {
  d <- .tow_data()
  d$bad <- d$w1
  d$bad[[1L]] <- Inf
  err <- tryCatch(
    table_outcome(d, y, by = g, weights = bad, p_value = FALSE),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  d$neg <- d$w1
  d$neg[[1L]] <- -1
  err <- tryCatch(
    table_outcome(d, y, by = g, weights = neg, p_value = FALSE),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  d$zero <- 0
  err <- tryCatch(
    table_outcome(d, y, by = g, weights = zero, p_value = FALSE),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")

  # An NA weight is legal: the row leaves the sample and the note says
  # how many did.
  d$some_na <- d$wi
  d$some_na[[2L]] <- NA
  tbl <- .tow_quiet(table_outcome(
    d,
    y,
    by = g,
    weights = some_na,
    p_value = FALSE
  ))
  expect_match(
    attr(tbl, "note"),
    "Rows with missing some_na removed: 1.",
    fixed = TRUE
  )
})

test_that("rescale without weights is announced and ignored", {
  d <- .tow_data()
  expect_warning(
    table_outcome(d, y, by = g, rescale = TRUE, p_value = FALSE),
    class = "spicy_ignored_arg"
  )
})
