# `table_outcome()` against `gtsummary::tbl_continuous()`.
#
#   tbl_continuous(data, variable = Y, include = c(A, B))
#     ==  table_outcome(data, outcome = Y, by = c(A, B))
#
# Same geometry -- one stub column, a header row per grouping, indented
# levels -- so the two are directly comparable, and every place they
# differ is a decision one of us made rather than an accident.
#
# Oracle provenance, pinned as CONSTANTS so this file needs no
# dependency: gtsummary 2.5.1 (cards layer, `t$cards[[1]]`), R 4.6.1,
# captured 2026-08-20 on `spicy::sochealth` and on `gtsummary::trial`.
# The reproducer:
#
#   t <- gtsummary::tbl_continuous(
#     as.data.frame(spicy::sochealth), variable = bmi,
#     include = c(sex, smoking))
#   subset(as.data.frame(t$cards[[1]]), context == "summary")
#
# SEVEN divergences are pinned below. Six are deliberate on one side or
# the other. The seventh is a QUANTILE DEFINITION, and it is the reason
# this file exists: the two packages do not compute the same quartiles.

.too_sh <- function() as.data.frame(spicy::sochealth)

.too_quiet <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(expr)))
  res
}

# ---------------------------------------------------------------------------
# 1. Where the two agree: the median and the count
# ---------------------------------------------------------------------------

test_that("the median and the count match gtsummary exactly", {
  # gtsummary 2.5.1 cards, `bmi` by `sex` and by `smoking`.
  gts_median <- c(
    sex.Female = 25.699999999999999,
    sex.Male = 26.100000000000001,
    smoking.No = 25.800000000000001,
    smoking.Yes = 26.199999999999999
  )
  gts_n <- c(
    sex.Female = 616L,
    sex.Male = 572L,
    smoking.No = 915L,
    smoking.Yes = 248L
  )

  tbl <- .too_quiet(table_outcome(.too_sh(), bmi, by = c(sex, smoking)))
  key <- paste(tbl$variable, tbl$level, sep = ".")
  for (k in names(gts_median)) {
    i <- which(key == k)
    expect_length(i, 1L)
    expect_equal(tbl$median[i], gts_median[[k]], tolerance = 1e-9)
    expect_identical(tbl$n[i], gts_n[[k]])
  }
})

# ---------------------------------------------------------------------------
# 2. Divergence 1 -- the quantile definition, and it is not cosmetic
# ---------------------------------------------------------------------------

test_that("the quartiles follow R's default, gtsummary's follow type 2", {
  # spicy documents that `med` / `q1` / `q3` equal `stats::median()` /
  # `stats::quantile()` on the same vector: R's default, type 7.
  # gtsummary 2.5.1 computes type 2 -- the averaged-order-statistic
  # definition SAS and SPSS use.
  #
  # The two coincide whenever both land on an observed order
  # statistic, which is why a display at zero decimals can make them
  # look identical. They do not coincide here.
  gts_p25 <- c(sex.Female = 23.100000000000001, sex.Male = 23.850000000000001)
  gts_p75 <- c(sex.Female = 28.600000000000001, sex.Male = 28.649999999999999)

  d <- .too_sh()
  tbl <- .too_quiet(table_outcome(d, bmi, by = sex))
  for (lv in c("Female", "Male")) {
    i <- which(tbl$level == lv)
    x <- d$bmi[d$sex == lv]
    x <- x[!is.na(x)]
    # Ours IS R's default.
    expect_equal(
      c(tbl$q1[i], tbl$q3[i]),
      unname(stats::quantile(x, c(0.25, 0.75), type = 7)),
      tolerance = 1e-12
    )
    # Theirs IS type 2, reproduced here rather than taken on trust.
    expect_equal(
      c(gts_p25[[paste0("sex.", lv)]], gts_p75[[paste0("sex.", lv)]]),
      unname(stats::quantile(x, c(0.25, 0.75), type = 2)),
      tolerance = 1e-9
    )
  }
  # And on this corpus the two genuinely differ, so the pin above is
  # not agreeing with itself.
  male <- which(tbl$level == "Male")
  expect_false(isTRUE(all.equal(
    tbl$q1[male],
    gts_p25[["sex.Male"]],
    tolerance = 1e-9
  )))

  # The median is the one quantile they always share: at p = 0.5 both
  # definitions return the standard median.
  x <- d$bmi[d$sex == "Male"]
  x <- x[!is.na(x)]
  expect_equal(
    unname(stats::quantile(x, 0.5, type = 2)),
    unname(stats::quantile(x, 0.5, type = 7)),
    tolerance = 1e-12
  )
})

# ---------------------------------------------------------------------------
# 3. Divergence 2 -- rounding: two mechanisms, and it bites at digits = 2
# ---------------------------------------------------------------------------

test_that("the two rounding mechanisms differ, at the default digits", {
  # `formatC()` (C sprintf) rounds the BINARY value actually stored:
  # 2.675 is 2.67499... in a double, so it prints 2.67. That is not
  # round-half-to-even. `gtsummary::style_number()` rounds half UP on
  # the decimal scale, so it prints 2.68.
  #
  # gtsummary's side is PINNED (2.5.1, R 4.6.1, measured through
  # `gtsummary::style_number()`): reimplementing its rounding here
  # would only test the reimplementation. Ours is computed live,
  # because ours is what a regression would move.
  cases <- data.frame(
    value = c(0.125, 2.675, 1.005, 0.375),
    digits = c(2L, 2L, 2L, 2L),
    gtsummary = c("0.13", "2.68", "1.01", "0.38"),
    ours = c("0.12", "2.67", "1.00", "0.38"),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(cases))) {
    expect_identical(
      formatC(cases$value[i], format = "f", digits = cases$digits[i]),
      cases$ours[i]
    )
  }
  # Three of the four DIFFER, at the default `digits = 2`. Only the
  # last agrees, and it agrees because 0.375 is exact in binary.
  expect_identical(cases$ours != cases$gtsummary, c(TRUE, TRUE, TRUE, FALSE))

  # The living case in gtsummary's own headline corpus: `trial$age` by
  # `grade`, level II, median 48.5 EXACTLY, which it displays as "49".
  expect_identical(formatC(48.5, format = "f", digits = 0L), "48")
  # And on 47.5 the two agree, so the divergence is not "every half":
  # this is a binary-representation effect, not round-half-to-even.
  expect_identical(formatC(47.5, format = "f", digits = 0L), "48")
})

# ---------------------------------------------------------------------------
# 4. Divergences 3 to 6 -- what each table chooses to show
# ---------------------------------------------------------------------------

test_that("we show the missing levels, gtsummary hides them", {
  # `smoking` carries 25 missing values. gtsummary 2.5.1 at its
  # defaults renders TWO levels, No and Yes; there is no Unknown row in
  # its cards. We render three, and the third is keyed by its role.
  d <- .too_sh()
  tbl <- .too_quiet(table_outcome(d, bmi, by = smoking))
  shown <- tbl$level[tbl$.row_role %in% c("level", "missing")]
  expect_identical(shown, c("No", "Yes", "(Missing)"))
  expect_identical(sum(tbl$.row_role == "missing"), 1L)
  # And the counts add up to the marginal one, which is the whole
  # point of showing the level.
  expect_identical(
    sum(tbl$n[tbl$.row_role %in% c("level", "missing")]),
    tbl$n[tbl$.row_role == "summary"]
  )
})

test_that("we open on a marginal row, gtsummary has none", {
  tbl <- .too_quiet(table_outcome(.too_sh(), bmi, by = sex))
  expect_identical(tbl$.row_role[[1L]], "summary")
  expect_identical(
    attr(tbl, "display_df")$Variable[[1L]],
    spicy:::.outcome_overall_label()
  )
})

test_that("we name the outcome, and we carry a p by default", {
  # gtsummary's header reads "N = 200": the outcome appears nowhere in
  # the table. Ours is in the title, on every engine.
  tbl <- .too_quiet(table_outcome(.too_sh(), bmi, by = sex))
  expect_identical(
    spicy:::.outcome_title(attr(tbl, "outcome_label")),
    "Descriptive statistics of Body mass index"
  )
  # gtsummary needs `add_p()`; we show the comparison by default.
  expect_true("p" %in% names(attr(tbl, "display_df")))
  expect_false(is.na(tbl$p.value[tbl$.row_role == "factor_header"]))
})

test_that("the composite cell is ours, punctuation included", {
  # "Med [Q1, Q3]" against gtsummary's "median (p25, p75)": frozen
  # brackets (decision 25), and a semicolon separator under a comma
  # decimal mark, where a comma would serve two roles at once.
  tbl <- .too_quiet(table_outcome(
    .too_sh(),
    bmi,
    by = sex,
    show_columns = c("med_iqr", "n")
  ))
  cell <- attr(tbl, "display_df")[["Med [Q1, Q3]"]][[3L]]
  expect_match(cell, "^[0-9.]+ \\[[0-9.]+, [0-9.]+\\]$")
  tbl_eu <- .too_quiet(table_outcome(
    .too_sh(),
    bmi,
    by = sex,
    show_columns = c("med_iqr", "n"),
    decimal_mark = ","
  ))
  cell_eu <- attr(tbl_eu, "display_df")[["Med [Q1, Q3]"]][[3L]]
  expect_match(cell_eu, "^[0-9,]+ \\[[0-9,]+; [0-9,]+\\]$")
})
