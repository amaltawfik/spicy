# table_battery(): a battery of items rated on one scale (issue #13; ADR
# dev/decisions/2026-11-10-table-battery.md).
#
# Oracles, all independent of spicy: `prop.table(table())` for the row
# percentages of each item, `xtabs()` for the weighted ones,
# `chisq.test(table(item, group))` for the item tests, Holm's adjustment
# written out by hand, and `freq()` as a cross-check of one item. The data
# are the four life_sat_* items of sochealth, rated 1 to 5, given the
# labels of a satisfaction scale, and small frames built for the edges.

.bat_fig <- intToUtf8(0x2007L) # figure space: the decimal-alignment padding
.bat_bar <- intToUtf8(0x2502L) # the rule after the stub of the console

.BAT_SL <- c(
  "Very dissatisfied",
  "Dissatisfied",
  "Neutral",
  "Satisfied",
  "Very satisfied"
)
.BAT_ITEMS <- c(
  "life_sat_health",
  "life_sat_work",
  "life_sat_relationships",
  "life_sat_standard"
)

.bat_sat <- function() {
  d <- as.data.frame(sochealth[c("sex", "age_group", "weight")])
  for (v in .BAT_ITEMS) {
    d[[v]] <- factor(sochealth[[v]], levels = 1:5, labels = .BAT_SL)
  }
  d
}

.bat_tbl <- function(...) {
  table_battery(.bat_sat(), tidyselect::all_of(.BAT_ITEMS), ...)
}

# Holm's step-down adjustment, written out.
.bat_holm <- function(p) {
  m <- length(p)
  o <- order(p)
  adj <- pmin(1, cummax((m - seq_len(m) + 1) * p[o]))
  out <- numeric(m)
  out[o] <- adj
  out
}

# A small battery with a level nobody chose, a missing group and an item
# without data in one group.
.bat_small <- function() {
  lv <- c("Agree", "Neutral", "Disagree", "Unused")
  data.frame(
    g = factor(c(rep("A", 6), rep("B", 6), NA)),
    q1 = factor(
      c(
        "Agree",
        "Agree",
        "Neutral",
        "Disagree",
        "Agree",
        NA,
        "Disagree",
        "Disagree",
        "Neutral",
        "Agree",
        "Disagree",
        "Disagree",
        "Agree"
      ),
      levels = lv
    ),
    q2 = factor(
      c(
        "Neutral",
        "Agree",
        "Agree",
        "Agree",
        "Neutral",
        "Agree",
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        "Agree"
      ),
      levels = lv
    ),
    stringsAsFactors = FALSE
  )
}

# ---- the numbers ---------------------------------------------------------------

test_that("the row percentages are those of prop.table(table()) for each item", {
  d <- .bat_sat()
  df <- .bat_tbl(output = "data.frame")
  expect_identical(df$variable, .BAT_ITEMS)
  for (i in seq_along(.BAT_ITEMS)) {
    x <- d[[.BAT_ITEMS[[i]]]]
    oracle <- 100 * as.vector(prop.table(table(x)))
    expect_equal(unname(unlist(df[i, .BAT_SL])), oracle)
    expect_identical(df$n[[i]], as.numeric(sum(!is.na(x))))
  }
})

test_that("one item agrees with freq()", {
  d <- .bat_sat()
  fr <- freq(d, life_sat_work, output = "data.frame")
  fr <- fr[!is.na(fr$value), ]
  df <- table_battery(d, life_sat_work, output = "data.frame")
  expect_equal(unname(unlist(df[1L, fr$value])), 100 * fr$valid_prop)
})

test_that("weights give the percentages of xtabs()", {
  d <- .bat_sat()
  df <- .bat_tbl(weights = weight, output = "data.frame")
  for (i in seq_along(.BAT_ITEMS)) {
    tab <- stats::xtabs(
      stats::as.formula(paste("weight ~", .BAT_ITEMS[[i]])),
      data = d
    )
    expect_equal(
      unname(unlist(df[i, .BAT_SL])),
      100 * as.vector(prop.table(tab))
    )
    expect_equal(df$n[[i]], sum(tab))
  }
  expect_match(attr(df, "note"), "Statistics weighted by weight.", fixed = TRUE)
})

test_that("by: the percentages of each group, a chi-squared per item, Holm by hand", {
  d <- .bat_sat()
  df <- .bat_tbl(by = sex, output = "data.frame")
  long <- .bat_tbl(by = sex, output = "long")
  raw <- vapply(
    .BAT_ITEMS,
    function(v) {
      suppressWarnings(
        stats::chisq.test(table(d[[v]], d$sex), correct = FALSE)$p.value
      )
    },
    numeric(1)
  )
  expect_equal(df$p, unname(.bat_holm(raw)))
  expect_equal(df$p, unname(stats::p.adjust(raw, "holm")))
  first <- long[!duplicated(long$variable), ]
  expect_equal(first$p_unadjusted, unname(raw))
  expect_identical(unique(long$test_type), "chi_squared")
  stat <- vapply(
    .BAT_ITEMS,
    function(v) {
      unname(stats::chisq.test(table(d[[v]], d$sex), correct = FALSE)$statistic)
    },
    numeric(1)
  )
  expect_equal(first$statistic, unname(stat))
  expect_identical(first$df, rep(4, 4L))
  for (g in c("Female", "Male")) {
    x <- d$life_sat_health[d$sex == g]
    expect_equal(
      unname(unlist(df[1L, paste(g, .BAT_SL)])),
      100 * as.vector(prop.table(table(x)))
    )
    expect_identical(df[[paste(g, "n")]][[1L]], as.numeric(sum(!is.na(x))))
  }
  expect_match(
    attr(df, "note"),
    "Adjusted for 4 tests (Holm method).",
    fixed = TRUE
  )
})

test_that("p_adjust = \"none\" leaves the p-values raw", {
  d <- .bat_sat()
  df <- .bat_tbl(by = sex, p_adjust = "none", output = "data.frame")
  raw <- vapply(
    .BAT_ITEMS,
    function(v) {
      stats::chisq.test(table(d[[v]], d$sex), correct = FALSE)$p.value
    },
    numeric(1)
  )
  expect_equal(df$p, unname(raw))
  expect_false(grepl("Adjusted", attr(df, "note"), fixed = TRUE))
})

test_that("top adds up its levels, and with by keeps only itself and n", {
  df <- .bat_tbl(top = c("Very satisfied", "Satisfied"), output = "data.frame")
  top_col <- "Very satisfied + Satisfied"
  expect_identical(
    names(df),
    c("variable", "label", .BAT_SL, top_col, "n")
  )
  expect_equal(df[[top_col]], df[["Very satisfied"]] + df[["Satisfied"]])
  by_df <- .bat_tbl(
    by = sex,
    top = c("Very satisfied", "Satisfied"),
    output = "data.frame"
  )
  expect_identical(
    names(by_df),
    c(
      "variable",
      "label",
      paste("Female", c(top_col, "n")),
      paste("Male", c(top_col, "n")),
      "p"
    )
  )
  full <- .bat_tbl(by = sex, output = "data.frame")
  expect_equal(
    by_df[[paste("Female", top_col)]],
    full[["Female Very satisfied"]] + full[["Female Satisfied"]]
  )
  expect_equal(by_df$p, full$p)
})

test_that("drop_na = FALSE adds the column of missing answers to the base", {
  d <- .bat_sat()
  df <- .bat_tbl(drop_na = FALSE, output = "data.frame")
  expect_identical(names(df), c("variable", "label", .BAT_SL, "(Missing)", "n"))
  for (i in seq_along(.BAT_ITEMS)) {
    x <- d[[.BAT_ITEMS[[i]]]]
    oracle <- 100 * as.vector(prop.table(table(x, useNA = "always")))
    expect_equal(unname(unlist(df[i, c(.BAT_SL, "(Missing)")])), oracle)
    expect_identical(df$n[[i]], as.numeric(nrow(d)))
  }
  expect_match(attr(df, "note"), "all respondents", fixed = TRUE)
})

test_that("levels leaves a level out of the percentages and the base", {
  d <- .bat_sat()
  shown <- .BAT_SL[-3L]
  df <- .bat_tbl(levels = shown, output = "data.frame")
  expect_identical(names(df), c("variable", "label", shown, "n"))
  x <- d$life_sat_health
  x <- droplevels(x[!is.na(x) & x != "Neutral"])
  expect_equal(
    unname(unlist(df[1L, shown])),
    100 * as.vector(prop.table(table(x)))
  )
  expect_identical(df$n[[1L]], as.numeric(length(x)))
  expect_match(
    attr(df, "note"),
    "Answers left out of the percentages and the base: Neutral.",
    fixed = TRUE
  )
  # The order of `levels` is the order of the columns.
  rev_df <- .bat_tbl(levels = rev(shown), output = "data.frame")
  expect_identical(names(rev_df)[3:6], rev(shown))
})

test_that("a level nobody chose keeps its column, at 0", {
  df <- table_battery(.bat_small(), c(q1, q2), output = "data.frame")
  expect_identical(df$Unused, c(0, 0))
  long <- table_battery(.bat_small(), c(q1, q2), output = "long")
  expect_identical(long$n[long$level == "Unused"], c(0, 0))
  printed <- attr(table_battery(.bat_small(), c(q1, q2)), "display_df")
  expect_identical(printed$Unused, c("0.0", "0.0"))
})

test_that("the machine outputs: data.frame, long, as.data.frame()", {
  tbl <- .bat_tbl(by = sex)
  df <- .bat_tbl(by = sex, output = "data.frame")
  expect_identical(as.data.frame(tbl), df)
  expect_identical(class(df), "data.frame")
  long <- .bat_tbl(by = sex, output = "long")
  expect_identical(
    names(long),
    c(
      "variable",
      "label",
      "group",
      "level",
      ".row_role",
      "n",
      "pct",
      "test_type",
      "statistic",
      "df",
      "p.value",
      "p_unadjusted"
    )
  )
  expect_identical(nrow(long), 4L * 2L * 5L)
  expect_identical(unique(long$.row_role), "level")
  # Without `by` and tests, the long output has no group and no test.
  plain <- .bat_tbl(output = "long", drop_na = FALSE)
  expect_identical(
    names(plain),
    c("variable", "label", "level", ".row_role", "n", "pct")
  )
  expect_identical(
    unique(plain$.row_role[plain$level == "(Missing)"]),
    "missing"
  )
  expect_s3_class(tbl, "spicy_battery_table")
  s <- as_structured(tbl)
  expect_identical(s$body$.row_role, rep("summary", 4L))
  expect_identical(s$body$.variable, .BAT_ITEMS)
  expect_named(s$spanners, c("Female", "Male"))
  expect_identical(s$col_meta[["Female Neutral"]]$scale_level, "Neutral")
  expect_identical(s$col_meta$p$p_adjust, "holm")
})

test_that("the default note says what the percentages and the base are", {
  tbl <- .bat_tbl()
  expect_identical(
    attr(tbl, "note"),
    "Note. Percentages are row percentages. Base: respondents who answered the item."
  )
  expect_identical(attr(tbl, "title"), "Item battery")
  expect_identical(attr(.bat_tbl(by = sex), "title"), "Item battery by sex")
})

test_that("the French table translates its title, the missing column and the note", {
  withr::local_options(spicy.language = "fr")
  tbl <- .bat_tbl(by = sex, drop_na = FALSE)
  expect_identical(attr(tbl, "title"), "Batterie d'items selon sex")
  expect_true("Female (Manquant)" %in% names(attr(tbl, "display_df")))
  expect_match(attr(tbl, "note"), "Pourcentages en ligne.", fixed = TRUE)
  expect_match(
    attr(tbl, "note"),
    "y compris celles qui n'ont pas",
    fixed = TRUE
  )
  expect_match(
    attr(tbl, "note"),
    "de chaque item selon le groupe",
    fixed = TRUE
  )
})

# ---- the refusals ----------------------------------------------------------------

test_that("items whose levels differ are refused, the item named", {
  d <- .bat_sat()
  d$life_sat_work <- factor(d$life_sat_work, levels = c(.BAT_SL, "Other"))
  expect_error(
    table_battery(d, tidyselect::all_of(.BAT_ITEMS)),
    "`life_sat_work` does not have the levels of `life_sat_health`",
    class = "spicy_invalid_input"
  )
  # The same set in another order is accepted, in the order of the first.
  d2 <- .bat_sat()
  d2$life_sat_work <- factor(d2$life_sat_work, levels = rev(.BAT_SL))
  df <- table_battery(d2, tidyselect::all_of(.BAT_ITEMS), output = "data.frame")
  expect_identical(names(df)[3:7], .BAT_SL)
})

test_that("a top level outside the levels shown, or a bad sort, is refused", {
  expect_error(
    .bat_tbl(top = "Nope"),
    "`top` names",
    class = "spicy_invalid_input"
  )
  expect_error(
    .bat_tbl(levels = .BAT_SL[-5L], top = c("Very satisfied", "Satisfied")),
    "not among the levels shown",
    class = "spicy_invalid_input"
  )
  expect_error(
    .bat_tbl(sort = "bad"),
    "is not a way to sort",
    class = "spicy_invalid_input"
  )
  expect_error(
    .bat_tbl(sort = "-Neutral"),
    "is not a way to sort",
    class = "spicy_invalid_input"
  )
  expect_error(
    .bat_tbl(sort = "top+"),
    "needs `top`",
    class = "spicy_invalid_input"
  )
  # A level or a keyword without its direction has two readings.
  expect_error(
    .bat_tbl(sort = "Neutral"),
    "two readings: \"Neutral+\" (increasing) or \"Neutral-\" (decreasing)",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
  expect_error(
    .bat_tbl(sort = "name"),
    "two readings",
    class = "spicy_invalid_input"
  )
  expect_error(.bat_tbl(sort = c("a", "b")), class = "spicy_invalid_input")
})

test_that("a level named like a column the table keeps for itself is refused", {
  for (nm in c(
    "Variable",
    "variable",
    "label",
    ".variable",
    ".row_role",
    "n"
  )) {
    d <- data.frame(
      a = factor(c(nm, "other")),
      b = factor(c("other", nm))
    )
    expect_error(
      table_battery(d, c(a, b)),
      "has the name of a column the table adds",
      class = "spicy_label_collision"
    )
  }
  # `n` is free once the table shows no `n` column.
  d <- data.frame(a = factor(c("n", "y")), b = factor(c("y", "n")))
  expect_identical(
    names(table_battery(d, c(a, b), show_n = FALSE, output = "data.frame")),
    c("variable", "label", "n", "y")
  )
})

# ---- the scale -----------------------------------------------------------------

test_that("items without declared levels take the union of their values", {
  # Numbers sorted: the value 4, which item a never holds, is a column at 0.
  d <- data.frame(a = c(1L, 2L, 5L, 3L), b = c(5L, 4L, 4L, 1L))
  df <- table_battery(d, c(a, b), output = "data.frame")
  expect_identical(names(df)[3:7], as.character(1:5))
  expect_equal(
    unname(unlist(df[1L, 3:7])),
    100 * as.vector(prop.table(table(factor(d$a, levels = 1:5))))
  )
  # Text in the order of first appearance: nobody answered "SD" to b.
  ch <- data.frame(a = c("A", "SA", "D", "SD"), b = c("A", "A", "SA", NA))
  df <- table_battery(ch, c(a, b), output = "data.frame")
  expect_identical(names(df)[3:6], c("A", "SA", "D", "SD"))
  expect_identical(df$SD, c(25, 0))
  expect_identical(df$n, c(4, 3))
})

test_that("levels is the scale: a level nobody holds at 0, other answers left out", {
  # A numeric item with a value nobody chose.
  d <- data.frame(a = c(1, 2, 2, 7), b = c(2, 2, 1, 1))
  # 3, 4 and 5 lie within the values held (1 to 7): points of the scale
  # nobody chose, kept at 0 without a warning.
  expect_no_warning(
    df <- table_battery(d, c(a, b), levels = 1:5, output = "data.frame")
  )
  expect_identical(names(df)[3:7], as.character(1:5))
  expect_equal(df[["1"]], c(100 / 3, 50))
  expect_identical(df[["5"]], c(0, 0))
  expect_identical(df$n, c(3, 4))
  expect_match(
    attr(df, "note"),
    "Answers left out of the percentages and the base: 7.",
    fixed = TRUE
  )
  # A factor's declared levels outside `levels` are left out the same way.
  f <- data.frame(
    a = factor(c("x", "y", "z"), levels = c("x", "y", "z", "w")),
    b = factor(c("x", "x", "y"), levels = c("x", "y"))
  )
  df <- table_battery(f, c(a, b), levels = c("x", "y"), output = "data.frame")
  expect_identical(df$n, c(2, 3))
  expect_match(attr(df, "note"), "the base: z, w.", fixed = TRUE)
})

test_that("a levels entry no item declares or holds warns; a declared one does not", {
  d <- data.frame(a = c("A", "B", "A"), b = c("B", "B", "A"))
  expect_warning(
    df <- table_battery(
      d,
      c(a, b),
      levels = c("A", "B", "Bee"),
      output = "data.frame"
    ),
    "`levels` has an entry that no item declares or holds: \"Bee\"",
    class = "spicy_unknown_name"
  )
  expect_identical(df$Bee, c(0, 0))
  # A number outside the values the numeric items hold warns.
  n <- data.frame(a = c(1, 2, 5), b = c(2, 3, 3))
  expect_warning(
    df <- table_battery(n, c(a, b), levels = 0:5, output = "data.frame"),
    "no item declares or holds: \"0\".",
    fixed = TRUE,
    class = "spicy_unknown_name"
  )
  expect_identical(df[["0"]], c(0, 0))
  expect_no_warning(table_battery(n, c(a, b), levels = 1:5))
  # Declared by a factor, chosen by nobody: a modality offered, no warning.
  f <- data.frame(
    a = factor(c("A", "B"), levels = c("A", "B", "C")),
    b = factor(c("B", "B"), levels = c("A", "B", "C"))
  )
  expect_no_warning(
    df <- table_battery(
      f,
      c(a, b),
      levels = c("A", "B", "C"),
      output = "data.frame"
    )
  )
  expect_identical(df$C, c(0, 0))
})

test_that("factor and non-factor items are refused together, unless levels says the scale", {
  d <- data.frame(a = factor(c("x", "y")), b = c("x", "y"))
  expect_error(
    table_battery(d, c(a, b)),
    "`b` declares no levels, but `a` does",
    class = "spicy_invalid_input"
  )
  df <- table_battery(d, c(a, b), levels = c("x", "y"), output = "data.frame")
  expect_identical(df$x, c(50, 50))
})

# ---- beyond the contract# ---- beyond the contract -------------------------------------------------------------

test_that("sort orders the rows on top, a level or the labels", {
  skip_on_cran()
  df <- .bat_tbl(output = "data.frame")
  top <- df[["Very satisfied"]] + df[["Satisfied"]]
  sorted <- function(...) {
    .bat_tbl(
      top = c("Very satisfied", "Satisfied"),
      ...,
      output = "data.frame"
    )$variable
  }
  expect_identical(sorted(sort = "top-"), df$variable[order(-top)])
  expect_identical(sorted(sort = "top+"), df$variable[order(top)])
  # "+" and "-" alone sort on the summary column, or on the first level.
  expect_identical(sorted(sort = "-"), df$variable[order(-top)])
  expect_identical(sorted(sort = "+"), df$variable[order(top)])
  first <- df[["Very dissatisfied"]]
  expect_identical(
    .bat_tbl(sort = "-", output = "data.frame")$variable,
    df$variable[order(-first)]
  )
  expect_identical(
    .bat_tbl(sort = "Neutral-", output = "data.frame")$variable,
    df$variable[order(-df$Neutral)]
  )
  expect_identical(
    .bat_tbl(sort = "Very dissatisfied+", output = "data.frame")$variable,
    df$variable[order(first)]
  )
  expect_identical(
    .bat_tbl(sort = "name-", output = "data.frame")$variable,
    sort(.BAT_ITEMS, decreasing = TRUE, method = "radix")
  )
})

test_that("name+ sorts on the labels shown, and the keywords win over a level", {
  d <- .bat_sat()
  labs <- c(
    life_sat_health = "D health",
    life_sat_work = "A work",
    life_sat_relationships = "C relationships",
    life_sat_standard = "B standard"
  )
  df <- table_battery(
    d,
    tidyselect::all_of(.BAT_ITEMS),
    labels = labs,
    sort = "name+",
    output = "data.frame"
  )
  expect_identical(df$label, sort(unname(labs)))
  expect_identical(df$variable, names(sort(labs)))
  printed <- attr(
    table_battery(d, tidyselect::all_of(.BAT_ITEMS), labels = labs),
    "display_df"
  )
  expect_identical(printed$Variable, unname(labs))
  # A level called "name": "name+" is the keyword, the labels.
  k <- data.frame(
    b = factor(c("name", "x", "x"), levels = c("name", "x")),
    a = factor(c("name", "name", "x"), levels = c("name", "x"))
  )
  expect_identical(
    table_battery(k, c(b, a), sort = "name+", output = "data.frame")$variable,
    c("a", "b")
  )
})

test_that("a level sorts on all the groups together under by", {
  # Group A puts q1 first, the groups together put q2 first; the bases of
  # the items in group A differ, so only the pooled bases give the order.
  d <- data.frame(
    g = c(rep("A", 50), rep("B", 100)),
    q1 = c("Yes", "Yes", rep(NA, 48), rep(c("Yes", "No"), c(10, 90))),
    q2 = c(rep("No", 50), rep(c("Yes", "No"), c(50, 50)))
  )
  df <- table_battery(
    d,
    c(q1, q2),
    by = g,
    sort = "Yes-",
    output = "data.frame"
  )
  pooled <- c(
    q1 = mean(d$q1 == "Yes", na.rm = TRUE),
    q2 = mean(d$q2 == "Yes", na.rm = TRUE)
  )
  expect_identical(df$variable, names(sort(pooled, decreasing = TRUE)))
  expect_identical(df$variable, c("q2", "q1"))
})

test_that("auto takes Fisher's exact test where an expected count is below 5", {
  skip_on_cran()
  d <- .bat_small()
  df <- table_battery(
    d,
    c(q1, q2),
    by = g,
    p_adjust = "none",
    output = "data.frame"
  )
  tab <- table(d$q1, d$g)
  tab <- tab[rowSums(tab) > 0, ]
  expect_equal(df$p[[1L]], stats::fisher.test(tab)$p.value)
  long <- table_battery(d, c(q1, q2), by = g, output = "long")
  expect_identical(long$test_type[long$variable == "q1"][[1L]], "fisher_exact")
  # q2 has no answer in group B: its p cell is blank, it is not adjusted.
  expect_true(is.na(df$p[[2L]]))
  printed <- attr(table_battery(d, c(q1, q2), by = g), "display_df")
  expect_identical(printed$p[[2L]], "")
  note <- attr(df, "note")
  expect_match(note, "Missing values removed: g (1).", fixed = TRUE)
  expect_match(note, "No data for q2 in B.", fixed = TRUE)
  # One p-value shown: the sentence of one item.
  expect_match(note, "Item p-value: Fisher exact test by group.", fixed = TRUE)
})

test_that("test = \"chisq\", \"midp\" and \"n1\" on a two-level item", {
  skip_on_cran()
  d <- .bat_sat()
  d$yes <- factor(
    d$life_sat_health %in% .BAT_SL[4:5],
    c(FALSE, TRUE),
    c("No", "Yes")
  )
  d$yes2 <- factor(
    d$life_sat_work %in% .BAT_SL[4:5],
    c(FALSE, TRUE),
    c("No", "Yes")
  )
  tab <- table(d$yes, d$sex)
  n1 <- table_battery(
    d,
    c(yes, yes2),
    by = sex,
    test = "n1",
    p_adjust = "none",
    output = "long"
  )
  x2 <- unname(stats::chisq.test(tab, correct = FALSE)$statistic)
  n <- sum(tab)
  expect_equal(n1$statistic[[1L]], x2 * (n - 1) / n)
  expect_identical(n1$test_type[[1L]], "n1_chi_squared")
  midp <- table_battery(
    d,
    c(yes, yes2),
    by = sex,
    test = "midp",
    p_adjust = "none",
    output = "long"
  )
  expect_identical(midp$test_type[[1L]], "fisher_midp")
  expect_true(is.na(midp$statistic[[1L]]))
  fisher <- table_battery(
    d,
    c(yes, yes2),
    by = sex,
    test = "fisher",
    p_adjust = "none",
    output = "long"
  )
  expect_equal(fisher$p.value[[1L]], stats::fisher.test(tab)$p.value)
})

test_that("fractional weights refuse the exact test asked for and fall back otherwise", {
  skip_on_cran()
  d <- .bat_small()
  d$w <- c(1.25, 0.5, 1, 2, 1, 1, 0.5, 1, 1, 2, 1.5, 1, 1)
  expect_error(
    table_battery(d, c(q1, q2), by = g, weights = w, test = "fisher"),
    "needs counts",
    class = "spicy_unsupported"
  )
  expect_warning(
    long <- table_battery(d, q1, by = g, weights = w, output = "long"),
    class = "spicy_fallback"
  )
  expect_identical(long$test_type[[1L]], "chi_squared")
  tab <- stats::xtabs(w ~ q1 + g, data = d)
  tab <- tab[rowSums(tab) > 0, ]
  expect_equal(
    long$statistic[[1L]],
    unname(suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic))
  )
})

test_that("an item without variation shows a dash and is named in the note", {
  skip_on_cran()
  d <- .bat_small()
  d$q3 <- factor(ifelse(is.na(d$g), "Neutral", "Agree"), levels = levels(d$q1))
  tbl <- table_battery(d, c(q1, q3), by = g)
  expect_identical(attr(tbl, "display_df")$p[[2L]], spicy_str("cell_undefined"))
  expect_match(attr(tbl, "note"), "Not tested (no variation", fixed = TRUE)
})

test_that("an item with no data shows the dash, and the note says so", {
  skip_on_cran()
  d <- .bat_small()
  d$q3 <- factor(NA, levels = levels(d$q1))
  tbl <- table_battery(d, c(q1, q3))
  dash <- spicy_str("cell_undefined")
  expect_identical(attr(tbl, "display_df")$Agree[[2L]], dash)
  expect_identical(attr(tbl, "display_df")$n[[2L]], "0")
  expect_match(attr(tbl, "note"), "No data for q3.", fixed = TRUE)
  expect_identical(
    as_structured(tbl)$cell_status$Agree,
    c("", "undefined")
  )
})

test_that("labelled and character items take the levels freq() gives them", {
  skip_on_cran()
  lab <- data.frame(
    a = labelled::labelled(c(1, 2, 2, 1, 9), c(Yes = 1, No = 2, DK = 9)),
    b = labelled::labelled(c(2, 2, 1, NA, 1), c(Yes = 1, No = 2, DK = 9))
  )
  df <- table_battery(lab, c(a, b), output = "data.frame")
  expect_identical(names(df)[3:5], c("[1] Yes", "[2] No", "[9] DK"))
  expect_equal(df[["[9] DK"]], c(20, 0))
  df <- table_battery(
    lab,
    c(a, b),
    levels = c("[1] Yes", "[2] No"),
    output = "data.frame"
  )
  expect_equal(df[["[1] Yes"]], c(50, 50))
  expect_identical(df$n, c(4, 4))
  ch <- data.frame(a = c("x", "y", "x"), b = c("x", "x", "x"))
  expect_identical(
    table_battery(ch, c(a, b), output = "data.frame")$y,
    c(100 / 3, 0)
  )
  ch$b <- c("y", "x", "x")
  expect_identical(
    names(table_battery(ch, c(a, b), output = "data.frame"))[3:4],
    c("x", "y")
  )
})

test_that("show_n = FALSE, p_value = FALSE and the ignored arguments", {
  skip_on_cran()
  expect_false("n" %in% names(.bat_tbl(show_n = FALSE, output = "data.frame")))
  df <- .bat_tbl(by = sex, p_value = FALSE, output = "data.frame")
  expect_false("p" %in% names(df))
  expect_false(any(
    c("test_type", "p.value") %in%
      names(.bat_tbl(by = sex, p_value = FALSE, output = "long"))
  ))
  expect_warning(.bat_tbl(p_value = TRUE), class = "spicy_ignored_arg")
  expect_warning(.bat_tbl(test = "fisher"), class = "spicy_ignored_arg")
  expect_error(.bat_tbl(p_adjust = "BH"), class = "spicy_invalid_input")
  expect_error(
    .bat_tbl(by = sex, p_value = FALSE, p_adjust = "BH"),
    class = "spicy_invalid_input"
  )
  expect_error(.bat_tbl(drop_na = NA), class = "spicy_invalid_input")
  expect_error(
    .bat_tbl(levels = c("Neutral", "Neutral")),
    class = "spicy_invalid_input"
  )
  expect_error(table_battery(.bat_sat()), class = "spicy_invalid_input")
  expect_error(
    .bat_tbl(top = "Neutral"),
    "one level",
    class = "spicy_invalid_input"
  )
})

test_that("title and note can be replaced, and the note removed", {
  skip_on_cran()
  tbl <- .bat_tbl(
    title = "Satisfaction",
    note = "Source: sochealth.",
    note_add = "Second."
  )
  expect_identical(attr(tbl, "title"), "Satisfaction")
  expect_identical(attr(tbl, "note"), "Note. Source: sochealth. Second.")
  expect_null(attr(.bat_tbl(note = FALSE), "note"))
  expect_null(attr(.bat_tbl(title = FALSE), "title"))
})

test_that("the console splits a wide table into panels", {
  skip_on_cran()
  withr::local_options(width = 70)
  out <- capture.output(print(.bat_tbl(by = sex)))
  expect_gt(sum(grepl("Variable", out, fixed = TRUE)), 1L)
  expect_true(all(
    nchar(out[grepl(.bat_bar, out, fixed = TRUE)], type = "width") <= 70L
  ))
  withr::local_options(width = 200)
  expect_identical(
    sum(grepl(
      "Variable",
      capture.output(print(.bat_tbl())),
      fixed = TRUE
    )),
    1L
  )
})

test_that("gt, tinytable and flextable receive the cells of the typed view", {
  skip_on_cran()
  skip_if_not_installed("gt")
  skip_if_not_installed("tinytable")
  skip_if_not_installed("flextable")
  shown <- attr(.bat_tbl(by = sex), "display_df")
  g <- .bat_tbl(by = sex, output = "gt")
  expect_identical(names(g[["_data"]]), names(shown))
  expect_identical(
    trimws(gsub(.bat_fig, "", g[["_data"]][["Female Neutral"]])),
    shown[["Female Neutral"]]
  )
  expect_true(all(
    c("Female", "Male") %in% unlist(g[["_spanners"]]$spanner_label)
  ))
  expect_match(attr(g, "spicy_note"), "Holm method", fixed = TRUE)
  tt <- .bat_tbl(
    by = sex,
    top = c("Very satisfied", "Satisfied"),
    output = "tinytable"
  )
  expect_identical(dim(tt@data), c(4L, 6L))
  ft <- .bat_tbl(output = "flextable")
  expect_identical(ft$col_keys, c("Variable", .BAT_SL, "n"))
})

test_that("Excel and Word read back the table", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  xlsx <- tempfile(fileext = ".xlsx")
  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(c(xlsx, docx)), add = TRUE)
  shown <- attr(.bat_tbl(by = sex), "display_df")
  .bat_tbl(by = sex, output = "excel", excel_path = xlsx)
  grid <- as.matrix(openxlsx2::wb_to_df(
    openxlsx2::wb_load(xlsx),
    col_names = FALSE
  ))
  expect_identical(unname(grid[1L, 1L]), "Item battery by sex")
  expect_identical(unname(grid[3L, 2L]), "Female")
  expect_identical(unname(grid[4L, 2:6]), .BAT_SL)
  expect_identical(unname(grid[5L, 1L]), "life_sat_health")
  expect_equal(
    as.numeric(grid[5L, 2:6]),
    round(as.numeric(shown[1L, 2:6]), 1)
  )
  sheets <- openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(xlsx))
  expect_identical(unname(sheets), "Item battery")

  .bat_tbl(
    top = c("Very satisfied", "Satisfied"),
    output = "word",
    word_path = docx
  )
  cells <- officer::docx_summary(officer::read_docx(docx))
  cells <- cells[cells$content_type == "table cell", ]
  head_row <- trimws(cells$text[cells$row_id == min(cells$row_id)])
  expect_identical(
    head_row,
    c("Variable", .BAT_SL, "Very satisfied + Satisfied", "n")
  )
  body1 <- trimws(gsub(
    .bat_fig,
    "",
    cells$text[cells$row_id == min(cells$row_id) + 1L]
  ))
  shown_top <- attr(
    .bat_tbl(top = c("Very satisfied", "Satisfied")),
    "display_df"
  )
  expect_identical(body1, unname(unlist(shown_top[1L, ])))
  expect_true(any(grepl(
    "Percentages are row percentages",
    officer::docx_summary(officer::read_docx(docx))$text,
    fixed = TRUE
  )))
})

test_that("the clipboard carries the displayed table", {
  skip_on_cran()
  skip_if_not_installed("clipr")
  clip <- NULL
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(x, ...) {
      clip <<- x
      invisible(NULL)
    },
    .package = "clipr"
  )
  suppressMessages(.bat_tbl(output = "clipboard"))
  lines <- strsplit(clip, "\n", fixed = TRUE)[[1L]]
  expect_true(any(grepl("Very satisfied", lines, fixed = TRUE)))
  expect_true(any(startsWith(lines, "life_sat_health")))
})

test_that("a survey design is refused, with the reason", {
  skip_on_cran()
  skip_if_not_installed("survey")
  des <- survey::svydesign(ids = ~1, weights = ~weight, data = .bat_sat())
  expect_error(
    table_battery(des, tidyselect::all_of(.BAT_ITEMS)),
    class = "spicy_wrong_regime"
  )
})

test_that("a subset of the table prints as the data frame it is", {
  skip_on_cran()
  tbl <- .bat_tbl()
  expect_no_error(capture.output(print(tbl[1:2, ])))
})

# ---- the remaining contract -------------------------------------------------------

test_that("the long pct is the share of the base, as prop.table() gives it", {
  d <- .bat_sat()
  long <- .bat_tbl(output = "long")
  for (v in .BAT_ITEMS) {
    expect_equal(
      long$pct[long$variable == v],
      100 * as.vector(prop.table(table(d[[v]])))
    )
  }
})

test_that("decimal_mark writes the percentages with its mark", {
  df <- .bat_tbl(output = "data.frame")
  shown <- attr(.bat_tbl(decimal_mark = ","), "display_df")
  expect_identical(
    shown$Neutral,
    sub(".", ",", formatC(df$Neutral, format = "f", digits = 1), fixed = TRUE)
  )
})

test_that("note_labels names an item in the note, labels in the rows", {
  d <- .bat_small()
  tbl <- table_battery(
    d,
    c(q1, q2),
    by = g,
    note_labels = c(q2 = "the second question", g = "the group")
  )
  expect_match(
    attr(tbl, "note"),
    "No data for the second question in B.",
    fixed = TRUE
  )
  expect_identical(attr(tbl, "title"), "Item battery by the group")
  expect_identical(attr(tbl, "display_df")$Variable, c("q1", "q2"))
})

test_that("drop_na = FALSE says an item has no data in a group, and the base it keeps", {
  d <- .bat_small()
  tbl <- table_battery(d, c(q1, q2), by = g, drop_na = FALSE)
  expect_match(attr(tbl, "note"), "No data for q2 in B.", fixed = TRUE)
  left <- table_battery(
    .bat_sat(),
    tidyselect::all_of(.BAT_ITEMS),
    levels = .BAT_SL[-3L],
    drop_na = FALSE
  )
  note <- attr(left, "note")
  expect_match(
    note,
    "Base: all respondents but those whose answer is left out, including those who did not answer the item.",
    fixed = TRUE
  )
  expect_false(grepl("Base: all respondents, including", note, fixed = TRUE))
  withr::local_options(spicy.language = "fr")
  fr <- table_battery(
    .bat_sat(),
    tidyselect::all_of(.BAT_ITEMS),
    levels = .BAT_SL[-3L],
    drop_na = FALSE
  )
  expect_match(
    attr(fr, "note"),
    "toutes les personnes sauf celles dont la",
    fixed = TRUE
  )
})
