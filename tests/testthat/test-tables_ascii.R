test_that("build_ascii_table produces aligned ASCII output", {
  df <- data.frame(A = 1:2, B = c("x", "yy"))
  txt <- spicy:::build_ascii_table(df)

  expect_type(txt, "character")
  expect_true(any(grepl("\u2502", txt))) # has vertical bars
  expect_true(any(grepl("\u2500", txt))) # has horizontal lines
  expect_no_error(spicy:::build_ascii_table(df))
})


test_that("spicy_print_table prints and returns invisibly", {
  df <- data.frame(Category = "Valid", Values = "A", Freq. = 1)

  # Test invisibility directly
  expect_invisible(spicy_print_table(df, title = "Test Title"))

  # Test capture of printed output (for robustness)
  output <- capture.output(spicy_print_table(df, title = "Test Title"))
  expect_true(any(grepl("Test Title", output)))
})


test_that("spicy_print_table aligns Category and Values left", {
  df <- data.frame(
    Category = c("Valid", "Total"),
    Values = c("A", "B"),
    Freq. = c(1, 2)
  )
  output <- capture.output(spicy_print_table(df))

  # Rough alignment test (spaces after "Valid")
  expect_true(any(grepl("^ Valid", output)))
})

test_that("build_ascii_table accepts a numeric `padding` of any non-negative value", {
  df <- data.frame(A = 1:2, B = c("x", "y"))
  txt0 <- spicy:::build_ascii_table(df, padding = 0L)
  txt2 <- spicy:::build_ascii_table(df, padding = 2L)
  txt9 <- spicy:::build_ascii_table(df, padding = 9L)
  expect_type(txt0, "character")
  expect_type(txt2, "character")
  expect_type(txt9, "character")
  # Larger padding -> longer lines
  width0 <- max(nchar(strsplit(txt0, "\n", fixed = TRUE)[[1]]))
  width2 <- max(nchar(strsplit(txt2, "\n", fixed = TRUE)[[1]]))
  width9 <- max(nchar(strsplit(txt9, "\n", fixed = TRUE)[[1]]))
  expect_lt(width0, width2)
  expect_lt(width2, width9)
})

test_that("build_ascii_table rejects the legacy string `padding` choices with a migration error", {
  df <- data.frame(A = 1:2, B = c("x", "y"))
  expect_error(
    spicy:::build_ascii_table(df, padding = "compact"),
    "non-negative integer.+removed in spicy 0\\.11\\.0"
  )
  expect_error(
    spicy:::build_ascii_table(df, padding = "normal"),
    "non-negative integer.+removed in spicy 0\\.11\\.0"
  )
  expect_error(
    spicy:::build_ascii_table(df, padding = "wide"),
    "non-negative integer.+removed in spicy 0\\.11\\.0"
  )
})

test_that("build_ascii_table rejects negative or non-finite `padding`", {
  df <- data.frame(A = 1:2, B = c("x", "y"))
  expect_error(
    spicy:::build_ascii_table(df, padding = -1L),
    "non-negative integer"
  )
  expect_error(
    spicy:::build_ascii_table(df, padding = NA_integer_),
    "non-negative integer"
  )
  expect_error(
    spicy:::build_ascii_table(df, padding = c(2L, 3L)),
    "non-negative integer"
  )
})

test_that("spicy_print_table forwards the new `padding` semantics", {
  df <- data.frame(A = 1:2, B = c("x", "y"))
  expect_no_error(spicy_print_table(df, padding = 0L))
  expect_no_error(spicy_print_table(df, padding = 5L))
  expect_error(
    spicy_print_table(df, padding = "normal"),
    "non-negative integer.+removed in spicy 0\\.11\\.0"
  )
})

test_that("build_ascii_table supports bottom_line", {
  df <- data.frame(A = 1:2, B = c("x", "y"))
  txt <- spicy:::build_ascii_table(df, bottom_line = TRUE)
  expect_true(grepl("\u2534", txt))
})

test_that("print.spicy_categorical_table falls back to x when display_df is absent", {
  x <- data.frame(
    Variable = c("Smoking", "  Yes"),
    n = c("10", "10"),
    check.names = FALSE
  )
  class(x) <- c("spicy_categorical_table", "spicy_table", "data.frame")
  attr(x, "data_name") <- "demo"
  attr(x, "indent_text") <- "  "

  output <- capture.output(print(x))

  expect_true(any(grepl("Categorical table", output, fixed = TRUE)))
})

test_that("print.spicy_categorical_table uses grouped title and compact padding", {
  withr::local_options(list(width = 18))

  x <- data.frame(
    Variable = c("Var 1", "  Yes", "Var 2"),
    n = c("12", "8", "9"),
    check.names = FALSE
  )
  class(x) <- c("spicy_categorical_table", "spicy_table", "data.frame")
  attr(x, "display_df") <- x
  attr(x, "data_name") <- "demo"
  attr(x, "group_var") <- "education"
  attr(x, "indent_text") <- "  "

  output <- capture.output(print(x))

  expect_true(any(grepl(
    "Categorical table by education",
    output,
    fixed = TRUE
  )))
})

test_that("build_ascii_table honors `total_row_idx` and supports `group_sep_rows`", {
  df <- data.frame(
    Group = c("A1", "A2", "B1", "B2"),
    Count = c("10", "20", "5", "15")
  )
  # Explicit total_row_idx places the rule before row 4
  txt <- spicy:::build_ascii_table(
    df,
    padding = 0L,
    total_row_idx = 4L,
    group_sep_rows = 3L
  )
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  # group_sep_rows = 3 inserts a light dashed rule before row 3
  expect_true(any(grepl("╌", lines)))
  # total_row_idx = 4 inserts a heavy rule before row 4 (the bottom group)
  rule_positions <- grep("^[─╌┼│\\[]+$", lines)
  expect_gte(length(rule_positions), 2L) # header rule + group rule + total rule
})

test_that("`total_row_idx = integer(0)` suppresses the regex fallback", {
  # When the user provides `total_row_idx` explicitly (even as empty),
  # the grep fallback never runs, so a category literally named "Total"
  # cannot trigger a stray separator line.
  df <- data.frame(
    Item = c("Sub Total", "Real total here"),
    Count = c("5", "10")
  )
  txt <- spicy:::build_ascii_table(df, padding = 0L, total_row_idx = integer(0))
  expect_type(txt, "character")
  # The output has the header rule but no body separator rules
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  body_rule_count <- sum(grepl("^[─┼]+$", lines))
  expect_equal(body_rule_count, 1L) # header rule only
})

test_that("spicy_print_table reads `total_row_idx` from the input attribute", {
  df <- data.frame(
    Item = c("a", "b", "Total"),
    Count = c("1", "2", "3")
  )
  attr(df, "total_row_idx") <- 3L
  out <- capture.output(spicy_print_table(df))
  expect_type(out, "character")
  # Reading via attr should still draw the rule before row 3 ("Total")
  expect_true(any(grepl("^[─┼]+$", out)))
})

test_that("build_ascii_table handles single-column input", {
  df <- data.frame(Only = c("a", "b", "c"))
  txt <- spicy:::build_ascii_table(df)
  expect_type(txt, "character")
  expect_no_error(spicy_print_table(df))
})

test_that("spicy_print_table splits wide tables into stacked panels", {
  withr::local_options(list(width = 26))

  df <- data.frame(
    Variable = c("Smoking", "Activity"),
    Group = c("Women", "Men"),
    Count = c("120", "98"),
    Percent = c("52.3", "47.7"),
    check.names = FALSE
  )

  output <- capture.output(
    spicy_print_table(
      df,
      title = NULL,
      padding = 0L,
      align_left_cols = c(1L, 2L)
    )
  )

  expect_gt(sum(grepl("Variable", output, fixed = TRUE)), 1L)
  expect_true(any(grepl("Count", output, fixed = TRUE)))
  expect_true(any(grepl("Percent", output, fixed = TRUE)))
})

test_that("build_ascii_table / spicy_print_table validate inputs with classed errors", {
  expect_error(spicy:::build_ascii_table(1:3), class = "spicy_invalid_data")
  expect_error(spicy_print_table(1:3), class = "spicy_invalid_data")

  df <- data.frame(a = 1:2, b = 3:4)
  expect_error(
    spicy:::build_ascii_table(df, display_labels = "only one"),
    "one label per column",
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy_print_table(df, display_labels = "only one"),
    "one label per column",
    class = "spicy_invalid_input"
  )
})

test_that("continuation panels name the estimand of an orphaned companion column", {
  # dev/registre_rendu_estimands_spec.md (phase 4): a companion column
  # (SE / p / CI label) split away from its carrier repeated only its
  # generic header, ambiguous when the main panel already shows another
  # 95% CI. The orphan now reads "95% CI (<carrier>)"; a companion
  # whose carrier shares the panel keeps the short header.
  x <- data.frame(
    Variable = c("aaaa", "bbbb"),
    HR = c("1.1", "2.2"),
    "dRMST (365)" = c("-3", "4"),
    ci1 = c("[1.23, 2.34]", "[3.45, 4.56]"),
    "dRisk (365)" = c("0.11", "0.22"),
    ci2 = c("[5.67, 6.78]", "[7.89, 8.90]"),
    check.names = FALSE
  )
  names(x)[c(4L, 6L)] <- c("95% CI", "95% CI")
  op <- options(width = 66)
  on.exit(options(op), add = TRUE)
  qp <- function() {
    capture.output(spicy_print_table(
      x,
      align_left_cols = 1L,
      qualify_companions = TRUE
    ))
  }
  out <- qp()
  expect_true(any(grepl("95% CI (dRisk (365))", out, fixed = TRUE)))
  # Off by default: a layout whose `p` is a block omnibus has no
  # carrier to name, so the caller has to say it has one.
  bare <- capture.output(spicy_print_table(x, align_left_cols = 1L))
  expect_false(any(grepl("95% CI (dRisk", bare, fixed = TRUE)))
  expect_true(any(grepl("95% CI", bare, fixed = TRUE)))
  # Carrier traveling WITH its companion: short header preserved.
  options(width = 58)
  out2 <- qp()
  hdr2 <- grep("dRisk", out2, value = TRUE)
  expect_true(any(grepl("dRisk (365)", hdr2, fixed = TRUE)))
  expect_false(any(grepl("95% CI (dRisk", out2, fixed = TRUE)))
})

test_that("a block omnibus p is not attributed to its neighbor", {
  # Register n. 89. The carrier search is "the nearest data column on my
  # left", which is the estimate only in a coefficient layout. In the
  # descriptive families the `p` is the test of the whole block, and a
  # width split handed it whatever column happened to precede it: the
  # console said "p (Total %)" of a chi-squared test, "p (Test)" of a
  # Kruskal-Wallis, "p (Total DEff)" of a Rao-Scott -- each a false
  # claim about what was compared.
  skip_if_not_installed("survey")
  d <- mtcars
  for (v in c("cyl", "gear", "am", "carb")) {
    d[[v]] <- factor(d[[v]])
  }
  hdr <- function(expr, w) {
    op <- options(width = w)
    on.exit(options(op), add = TRUE)
    out <- capture.output(print(expr))
    paste(grep("Variable", out, value = TRUE), collapse = "\n")
  }

  # One width per family, each of them a width that used to compose.
  cat_h <- hdr(table_categorical(d, c(cyl, gear), by = carb), 100)
  con_h <- hdr(
    table_continuous(
      d,
      c(mpg, disp, hp, wt),
      by = carb,
      show_columns = c("n", "m", "sd", "ci", "med", "iqr"),
      p_value = TRUE,
      statistic = TRUE,
      effect_size = "auto"
    ),
    82
  )
  out_h <- hdr(
    table_outcome(
      d,
      mpg,
      c(cyl, gear, carb),
      show_columns = c("n", "m", "sd", "ci", "med", "iqr"),
      p_value = TRUE,
      statistic = TRUE,
      effect_size = "auto"
    ),
    90
  )
  lm_h <- hdr(table_continuous_lm(d, c(mpg, disp, hp), by = carb), 62)

  for (h in list(cat_h, con_h, out_h, lm_h)) {
    # The orphan reads "p", and the panel it landed on still shows it.
    expect_false(grepl("p (", h, fixed = TRUE))
    expect_match(h, "p", fixed = TRUE)
  }
  expect_false(grepl("p (Total %)", cat_h, fixed = TRUE))
  expect_false(grepl("p (Test)", con_h, fixed = TRUE))
  expect_false(grepl("p (Test)", out_h, fixed = TRUE))

  data(api, package = "survey", envir = environment())
  des <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
  svy_h <- hdr(
    suppressWarnings(table_categorical_svy(
      des,
      c(stype, awards),
      by = sch.wide,
      deff = TRUE,
      proportion_ci = TRUE
    )),
    102
  )
  expect_false(grepl("p (", svy_h, fixed = TRUE))

  # And the coefficient table, which asked for the qualification,
  # still gets it: there `p` really does follow its estimate.
  reg_h <- hdr(
    table_regression(lm(mpg ~ wt + hp + disp + drat, data = mtcars)),
    46
  )
  expect_true(grepl("p (B)", reg_h, fixed = TRUE))
})


test_that("an orphaned interval names its carrier at a fractional coverage", {
  # The pattern that recognizes a companion header interpolated `[0-9]+`
  # for the coverage, so it matched "95% CI" and missed "97.5% CI": at a
  # fractional `ci_level` the orphan silently kept its bare header and
  # the reader lost which estimand it belonged to.
  set.seed(1)
  d <- data.frame(
    bmi = rnorm(60, 25, 3),
    a_very_long_predictor_name_indeed = rnorm(60)
  )
  fit <- stats::lm(bmi ~ a_very_long_predictor_name_indeed, data = d)
  op <- options(width = 46)
  on.exit(options(op), add = TRUE)
  panel_header <- function(level, mark = ".") {
    out <- capture.output(print(table_regression(
      fit,
      ci_level = level,
      decimal_mark = mark,
      show_columns = c("b", "ci")
    )))
    grep("CI", out, value = TRUE)
  }
  expect_true(any(grepl("95% CI (B)", panel_header(0.95), fixed = TRUE)))
  expect_true(any(grepl("97.5% CI (B)", panel_header(0.975), fixed = TRUE)))
  # Decision 27: the header now carries the display mark, and the
  # pattern must keep recognizing it -- the orphan names its carrier
  # under the comma exactly as it does under the period.
  expect_true(any(grepl(
    "97,5% CI (B)",
    panel_header(0.975, ","),
    fixed = TRUE
  )))
  # And the pattern itself, against the whole of what
  # `.ci_pct_display()` can emit: the producer has no scientific
  # branch, so every coverage is digits with at most one decimal mark
  # (period, comma, or the Lancet midline dot).
  rx <- spicy:::.companion_header_pattern()
  expect_true(grepl(rx, "95% CI"))
  expect_true(grepl(rx, "97.5% CI"))
  expect_true(grepl(rx, "97,5% CI"))
  expect_true(grepl(rx, "97·5% CI"))
  expect_false(grepl(rx, "95% CI (B)"))
  levels <- c(0.5, 0.9, 0.95, 0.975, 0.99, 0.999, 0.29, 1e-3, 1e-5, 1e-6, 1e-9)
  pct <- vapply(levels, spicy:::.ci_pct_str, character(1))
  expect_true(all(grepl("^[0-9]+([.][0-9]+)?$", pct)))
  expect_true(all(grepl(rx, paste0(pct, "% CI"))))
  for (mk in c(",", "·")) {
    pct_mk <- vapply(
      levels,
      function(l) spicy:::.ci_pct_display(l, mk),
      character(1)
    )
    expect_true(all(grepl(rx, paste0(pct_mk, "% CI"))))
  }
  # The level that used to escape into scientific notation.
  expect_identical(spicy:::.ci_pct_str(1e-6), "0.0001")
  # ... and the ordinary levels are untouched.
  expect_identical(
    vapply(c(0.9, 0.95, 0.975, 0.99, 0.29), spicy:::.ci_pct_str, character(1)),
    c("90", "95", "97.5", "99", "29")
  )
})


test_that("a missing cell renders blank without desyncing the table", {
  # `stringr::str_pad(NA, w)` returns NA rather than a padded blank, and
  # `build_line()` adds that NA to its running bar position, so ONE
  # missing value used to leave its row unpadded and put every separator
  # of the table at NA -- a header rule with two crossings. Missing is
  # normalized to empty at the door of the renderer, for every family.
  df <- data.frame(
    Category = c("one", "two"),
    Values = c("a", NA_character_),
    Freq. = c("1", "2"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  txt <- strsplit(spicy:::build_ascii_table(df), "\n", fixed = TRUE)[[1L]]

  expect_false(any(grepl("NA", txt, fixed = TRUE)))
  expect_length(gregexpr("┼", txt[2L], fixed = TRUE)[[1L]], 1L)
  expect_length(unique(crayon::col_nchar(txt, type = "width")), 1L)
})


test_that("a missing column name renders blank without desyncing the table", {
  # Same defect, the header lane: the column name is padded by the same
  # `str_pad()` as the cells. Two routes reach it -- `names(x)` and the
  # `display_labels` override -- and both used to put the rule at NA.
  df <- data.frame(a = c("1", "2"), b = c("3", "4"), stringsAsFactors = FALSE)

  named_na <- df
  names(named_na) <- c(NA_character_, "b")
  txt <- strsplit(spicy:::build_ascii_table(named_na), "\n", fixed = TRUE)[[1L]]
  expect_false(any(grepl("NA", txt, fixed = TRUE)))
  expect_length(gregexpr("┼", txt[2L], fixed = TRUE)[[1L]], 1L)
  expect_length(unique(crayon::col_nchar(txt, type = "width")), 1L)

  txt2 <- strsplit(
    spicy:::build_ascii_table(df, display_labels = c(NA_character_, "B")),
    "\n",
    fixed = TRUE
  )[[1L]]
  expect_false(any(grepl("NA", txt2, fixed = TRUE)))
  expect_length(gregexpr("┼", txt2[2L], fixed = TRUE)[[1L]], 1L)
  expect_length(unique(crayon::col_nchar(txt2, type = "width")), 1L)

  # The panel splitter goes through the same door, so every panel of a
  # wide table is in register too.
  wide <- as.data.frame(
    matrix(as.character(1:40), nrow = 2L),
    stringsAsFactors = FALSE
  )
  names(wide) <- c(NA_character_, paste0("col_long_name_", 2:20))
  out <- capture.output(spicy_print_table(wide, max_width = 60))
  expect_false(any(grepl("NA", out, fixed = TRUE)))
  rules <- grep("┼", out, fixed = TRUE, value = TRUE)
  expect_true(length(rules) > 1L)
  for (r in rules) {
    expect_length(gregexpr("┼", r, fixed = TRUE)[[1L]], 1L)
  }
})

# Column groups laid out as units (`whole_spanners`), the console header
# of the descriptive tables under `by`.
.spanner_df <- function() {
  data.frame(
    V = c("a", "b"),
    A = c("1.0", "2.0"),
    B = c("3.0", "4.0"),
    C = c("5", "6"),
    D = c("7", "8")
  )
}

test_that("whole_spanners widens a group to its label instead of truncating", {
  m <- .spanner_df()
  lbl <- "A rather long group label"
  names_lbl <- stats::setNames(list(2:3), lbl)
  wide <- spicy:::build_ascii_table(
    m,
    align_left_cols = 1L,
    spanners = names_lbl,
    whole_spanners = TRUE
  )
  lines <- strsplit(wide, "\n", fixed = TRUE)[[1]]
  expect_match(lines[[1]], lbl, fixed = TRUE)
  expect_length(unique(nchar(lines, type = "width")), 1L)
  # The default keeps the truncation of the regression tables.
  cut <- spicy:::build_ascii_table(
    m,
    align_left_cols = 1L,
    spanners = names_lbl
  )
  expect_false(grepl(lbl, cut, fixed = TRUE))
  expect_match(cut, "…", fixed = TRUE)
})

test_that("spanner labels with wide or combining characters stay aligned", {
  m <- .spanner_df()
  row_of <- function(first) {
    out <- spicy:::build_ascii_table(
      m,
      align_left_cols = 1L,
      spanners = stats::setNames(list(2:3, 4:5), c(first, "Osaka"))
    )
    strsplit(out, "\n", fixed = TRUE)[[1]]
  }
  ascii <- row_of("ABCD")
  # Two wide characters fill the four columns of "ABCD", and a decomposed
  # accent adds none: "Osaka" stays where it is.
  for (first in c("東京", "étéA")) {
    lines <- row_of(first)
    expect_length(unique(nchar(lines, type = "width")), 1L)
    expect_identical(
      sub("^.*Osaka", "Osaka", lines[[1]]),
      sub("^.*Osaka", "Osaka", ascii[[1]])
    )
    expect_identical(
      nchar(sub("Osaka.*$", "", lines[[1]]), type = "width"),
      nchar(sub("Osaka.*$", "", ascii[[1]]), type = "width")
    )
  }
  # A truncated label is cut by console columns too.
  cut <- spicy:::build_ascii_table(
    m,
    align_left_cols = 1L,
    padding = 0L,
    spanners = list("東京都の男性" = 2:3)
  )
  expect_length(
    unique(nchar(strsplit(cut, "\n", fixed = TRUE)[[1]], type = "width")),
    1L
  )
  expect_identical(spicy:::ascii_trunc_width("東京都", 5L), "東京")
  expect_identical(spicy:::ascii_trunc_width("été", 2L), "ét")
})

test_that("a split into panels keeps each group whole", {
  df <- data.frame(
    V = "row",
    a1 = "11.1",
    a2 = "22.2",
    b1 = "33.3",
    b2 = "44.4",
    c1 = "55.5",
    c2 = "66.6"
  )
  groups <- list(A = 2:3, B = 4:5, C = 6:7)
  panels <- function(width, groups) {
    spicy:::ascii_table_panels(
      df,
      console_width = width,
      padding = 2L,
      first_column_line = TRUE,
      row_total_line = FALSE,
      sticky_cols = 1L,
      groups = groups
    )
  }
  # Room for one group and a half: the columns alone split inside B.
  expect_identical(panels(32, NULL), list(1:4, c(1L, 5:7)))
  expect_identical(panels(32, groups), list(1:3, c(1L, 4:5), c(1L, 6:7)))
  # A group wider than a panel by itself splits as single columns do.
  expect_identical(panels(20, groups), panels(20, NULL))
  # Printed: each panel repeats the label of the groups it holds.
  out <- withr::with_options(
    list(width = 32),
    capture.output(spicy_print_table(
      df,
      align_left_cols = 1L,
      spanners = groups,
      whole_spanners = TRUE
    ))
  )
  expect_identical(sum(grepl("^ V ", out)), 3L)
  expect_identical(sum(grepl("\\bA\\b|\\bB\\b|\\bC\\b", out)), 3L)
})

test_that("whole_spanners = \"fit\" widens a group only where the panel fits", {
  m <- .spanner_df()
  lbl <- "A rather long group label"
  print_at <- function(width, whole) {
    withr::with_options(
      list(width = width),
      capture.output(spicy_print_table(
        m,
        align_left_cols = 1L,
        spanners = stats::setNames(list(2:3), lbl),
        whole_spanners = whole
      ))
    )
  }
  # Room for the widened group: the label is whole, as under TRUE.
  expect_identical(print_at(80, "fit"), print_at(80, TRUE))
  expect_true(any(grepl(lbl, print_at(80, "fit"), fixed = TRUE)))
  # No room: the label is cut as under FALSE, and the table keeps to the
  # console.
  narrow <- print_at(24, "fit")
  expect_identical(narrow, print_at(24, FALSE))
  expect_true(any(grepl("…", narrow, fixed = TRUE)))
  expect_true(all(nchar(narrow, type = "width") <= 24L))
})

test_that("ascii_group_header heads the columns of a group", {
  labels <- c("Variable", "F n", "F %", "M n", "M %", "x 1", "p")
  groups <- list(
    list(key = "Variable", label = "Variable", cols = 1L),
    list(key = "F", label = "F", cols = 2:3, bounds = c("n", "%")),
    list(key = "M", label = "M", cols = 4:5, bounds = c("n", "%")),
    list(key = "x", label = "x", cols = 6L, bounds = "1"),
    list(key = "p", label = "p", cols = 7L)
  )
  hdr <- spicy:::ascii_group_header(labels, groups)
  expect_identical(hdr$labels, c("Variable", "n", "%", "n", "%", "x", "p"))
  expect_identical(hdr$spanners, list(F = 2:3, M = 4:5))
  # A label that cannot name a spanner leaves the full headers.
  groups[[3]]$label <- "F"
  groups[[2]]$label <- ""
  hdr <- spicy:::ascii_group_header(labels, groups)
  expect_identical(hdr$labels[2:5], c("F n", "F %", "n", "%"))
  expect_identical(hdr$spanners, list(F = 4:5))
  groups[[2]]$label <- "F"
  hdr <- spicy:::ascii_group_header(labels, groups)
  expect_identical(hdr$labels[2:5], c("n", "%", "M n", "M %"))
  expect_null(spicy:::ascii_group_header(labels, NULL)$spanners)
})
