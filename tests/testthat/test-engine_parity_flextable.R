# Parity of the flextable engine with the console, on the residuals
# the audit left once the structured contract was repaired:
#
#   * the header -- a table without a CI column emitted a wholly empty
#     row of column labels under its spanners, and the rules landed a
#     row below where the console draws them (HTML and .docx alike,
#     since `output_word()` builds on `output_flextable()`);
#   * the spanner runs -- neighbouring models sharing a label were
#     merged into ONE header cell straddling the model boundary: two
#     CI pairs became a single 4-wide "95% CI", two "B" a single
#     2-wide "B";
#   * the separators -- only the first rule the console draws between
#     blocks reached the table, so a `Thresholds:` block opened
#     without one;
#   * the title of the descriptive families, which reached the .docx
#     but not the HTML;
#   * the indentation of a level row in the descriptive families,
#     which was applied twice (indent string + engine padding).
#
# Oracles are the console rendering / attributes of the same object;
# the evidence is the rendered HTML (what `print()` hands the viewer)
# and, for the Word cross-check, the file read back with
# `officer::docx_summary()`.

.pf_data <- function() {
  d <- as.data.frame(sochealth)
  d$educ <- factor(as.character(d$education), levels = levels(d$education))
  d$srh <- d$self_rated_health
  d
}

# The rendered HTML of a spicy flextable, before note post-processing.
.pf_html <- function(ft) {
  class(ft) <- setdiff(class(ft), "spicy_flextable")
  as.character(flextable::htmltools_value(x = ft))
}

# One string per <thead> row; each cell as "[label]", a spanning cell
# as "[label x<colspan>]".
.pf_thead <- function(html) {
  inner <- sub("</thead>.*$", "", sub("^.*<thead>", "", html))
  trs <- regmatches(inner, gregexpr("<tr[^>]*>.*?</tr>", inner))[[1L]]
  vapply(
    trs,
    function(tr) {
      cells <- regmatches(tr, gregexpr("<t[hd][^>]*>.*?</t[hd]>", tr))[[1L]]
      paste0(
        vapply(
          cells,
          function(cell) {
            cs <- regmatches(cell, regexpr("colspan=\"[0-9]+\"", cell))
            n <- if (length(cs)) as.integer(gsub("\\D", "", cs)) else 1L
            txt <- gsub("<[^>]*>", "", cell)
            txt <- gsub("&nbsp;| | ", " ", txt)
            paste0("[", trimws(txt), if (n > 1L) paste0(" x", n) else "", "]")
          },
          character(1),
          USE.NAMES = FALSE
        ),
        collapse = ""
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

.pf_caption <- function(html) {
  m <- regmatches(html, regexpr("<caption[^>]*>.*?</caption>", html))
  if (!length(m)) NA_character_ else trimws(gsub("<[^>]*>", "", m))
}

# Bottom-border width of every body row, read on the first column.
.pf_body_rules <- function(ft) {
  as.numeric(ft$body$styles$cells$border.width.bottom$data[, 1L])
}


# ---- header structure -----------------------------------------------------

test_that("a table with no CI column rules its header like one with", {
  skip_if_not_installed("flextable")
  d <- .pf_data()
  m1 <- stats::lm(wellbeing_score ~ age + sex, data = d)
  m2 <- stats::glm(
    dentist_12m ~ age + sex,
    data = d,
    family = stats::binomial()
  )
  rows <- .pf_thead(.pf_html(table_regression(
    list(OLS = m1, Logistic = m2),
    show_columns = c("b", "se", "p"),
    output = "flextable"
  )))
  # Two header rows, as in the console: model names, then column
  # labels. The third, wholly empty row is the defect this pins.
  expect_length(rows, 2L)
  expect_identical(rows[1L], "[][OLS x3][Logistic x3]")
  expect_identical(rows[2L], "[Variable][B][SE][p][B][SE][p]")
  expect_false(any(grepl("^(\\[\\])+$", rows)))
})

test_that("a single-model table with no CI has one header row", {
  skip_if_not_installed("flextable")
  d <- .pf_data()
  m1 <- stats::lm(wellbeing_score ~ age + sex, data = d)
  rows <- .pf_thead(.pf_html(table_regression(
    m1,
    show_columns = c("b", "se", "p"),
    output = "flextable"
  )))
  expect_length(rows, 1L)
  expect_identical(rows[1L], "[Variable][B][SE][p]")
})

test_that("the CI header keeps its spanner row and its LL/UL row", {
  skip_if_not_installed("flextable")
  d <- .pf_data()
  m1 <- stats::lm(wellbeing_score ~ age + sex, data = d)
  rows <- .pf_thead(.pf_html(table_regression(
    m1,
    show_columns = c("b", "ci", "p"),
    output = "flextable"
  )))
  expect_length(rows, 2L)
  expect_identical(rows[1L], "[Variable][B][95% CI x2][p]")
  expect_identical(rows[2L], "[][][LL][UL][]")
  # Multi-model: the model row sits on top of those two.
  m2 <- stats::lm(wellbeing_score ~ age + educ, data = d)
  rows2 <- .pf_thead(.pf_html(table_regression(
    list(M1 = m1, M2 = m2),
    show_columns = c("b", "ci", "p"),
    output = "flextable"
  )))
  expect_length(rows2, 3L)
  expect_identical(rows2[1L], "[][M1 x4][M2 x4]")
  expect_identical(rows2[3L], "[][][LL][UL][][][LL][UL][]")
})

test_that("a header cell never straddles two models", {
  skip_if_not_installed("flextable")
  d <- .pf_data()
  m1 <- stats::lm(wellbeing_score ~ age + sex, data = d)
  m2 <- stats::lm(wellbeing_score ~ age + educ, data = d)
  # Two CI pairs: one "95% CI" each, never a single 4-wide cell.
  rows <- .pf_thead(.pf_html(table_regression(
    list(M1 = m1, M2 = m2),
    show_columns = "ci",
    output = "flextable"
  )))
  expect_identical(rows[2L], "[Variable][95% CI x2][95% CI x2]")
  expect_false(grepl("95% CI x4", rows[2L], fixed = TRUE))
  # Same for a shared 1-wide label.
  rows_b <- .pf_thead(.pf_html(table_regression(
    list(M1 = m1, M2 = m2),
    show_columns = "b",
    output = "flextable"
  )))
  expect_identical(rows_b[2L], "[Variable][B][B]")
})


# ---- separators -----------------------------------------------------------

test_that("every console separator is drawn, not only the first", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("MASS")
  d <- .pf_data()
  fit <- MASS::polr(srh ~ age + sex, data = d, Hess = TRUE)
  tbl <- table_regression(fit, output = "data.frame")
  group_sep <- as.integer(attr(tbl, "group_sep_rows"))
  section_sep <- as.integer(attr(tbl, "section_sep_rows"))
  expect_true(length(group_sep) == 1L && length(section_sep) == 1L)
  w <- .pf_body_rules(table_regression(fit, output = "flextable"))
  # A light rule closes the row ABOVE each separator: the one that
  # opens the Thresholds block and the one that opens the fit stats.
  expect_equal(w[section_sep - 1L], 0.5)
  expect_equal(w[group_sep - 1L], 0.5)
  expect_equal(sum(w == 0.5), 2L)
})


# ---- captions -------------------------------------------------------------

test_that("the descriptive families carry their title as an HTML caption", {
  skip_if_not_installed("flextable")
  d <- .pf_data()
  expect_identical(
    .pf_caption(.pf_html(table_categorical(
      d,
      select = c("sex", "smoking"),
      output = "flextable"
    ))),
    "Categorical table"
  )
  expect_identical(
    .pf_caption(.pf_html(table_categorical(
      d,
      select = c("srh", "smoking"),
      by = "sex",
      output = "flextable"
    ))),
    "Categorical table by sex"
  )
  expect_identical(
    .pf_caption(.pf_html(table_continuous(
      d,
      select = c("age", "bmi"),
      by = "sex",
      output = "flextable"
    ))),
    # Decision 4 (2026-08-13): the by table states its grouping
    # variable, label resolved.
    "Descriptive statistics by Sex"
  )
  expect_identical(
    .pf_caption(.pf_html(table_continuous_lm(
      d,
      select = c("wellbeing_score", "bmi"),
      by = "education",
      output = "flextable"
    ))),
    "Continuous outcomes by Highest education level"
  )
})

test_that("the caption is the title the console prints, in the table font", {
  skip_if_not_installed("flextable")
  d <- .pf_data()
  m1 <- stats::lm(wellbeing_score ~ age + sex, data = d)
  html <- .pf_html(table_regression(m1, output = "flextable"))
  expect_identical(.pf_caption(html), "Linear regression: wellbeing_score")
  # A table asked for no title carries no caption at all.
  expect_true(is.na(.pf_caption(.pf_html(table_regression(
    m1,
    title = FALSE,
    output = "flextable"
  )))))
})


# ---- indentation ----------------------------------------------------------

test_that("descriptive level rows are indented once, by the engine", {
  skip_if_not_installed("flextable")
  d <- .pf_data()
  for (ft in list(
    table_categorical(d, select = c("sex", "smoking"), output = "flextable"),
    table_categorical(
      d,
      select = c("srh", "smoking"),
      by = "sex",
      output = "flextable"
    )
  )) {
    labels <- ft$body$dataset[[1L]]
    pad <- as.numeric(ft$body$styles$pars$padding.left$data[, 1L])
    level <- pad > 5
    expect_true(any(level))
    # The engine's padding carries the indent; the cell text does not.
    expect_false(any(grepl("^[ \t]", labels)))
    expect_true(all(pad[level] == 14))
  }
})


# ---- HTML post-processing -------------------------------------------------

test_that("the header CSS cannot erase the rule of a single header row", {
  skip_if_not_installed("flextable")
  d <- .pf_data()
  m1 <- stats::lm(wellbeing_score ~ age + sex, data = d)
  ft <- table_regression(
    m1,
    show_columns = c("b", "se", "p"),
    output = "flextable"
  )
  html <- .spicy_ft_html_postprocess(.pf_html(ft), attr(ft, "spicy_note"))
  # This table's header IS a single row, so the rule that strips the
  # border under the first header row must not reach it.
  expect_length(.pf_thead(html), 1L)
  expect_false(grepl(
    "thead tr:first-child th { border-bottom: 0 none",
    html,
    fixed = TRUE
  ))
  expect_true(grepl(
    "thead tr:first-child:not(:last-child) th",
    html,
    fixed = TRUE
  ))
  expect_true(grepl(
    "thead tr:first-child:not(:last-child) th[colspan]",
    html,
    fixed = TRUE
  ))
})


# ---- the Word deliverable rides on the same builder -----------------------

test_that("no empty header row reaches the .docx either", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  d <- .pf_data()
  m1 <- stats::lm(wellbeing_score ~ age + sex, data = d)
  m2 <- stats::glm(
    dentist_12m ~ age + sex,
    data = d,
    family = stats::binomial()
  )
  path <- tempfile(fileext = ".docx")
  table_regression(
    list(OLS = m1, Logistic = m2),
    show_columns = c("b", "se", "p"),
    output = "word",
    word_path = path
  )
  s <- officer::docx_summary(officer::read_docx(path))
  cells <- s[s$content_type == "table cell", ]
  rows <- lapply(
    split(cells, cells$row_id),
    function(r) trimws(r$text[order(r$cell_id)])
  )
  expect_false(any(vapply(rows, function(r) all(!nzchar(r)), logical(1))))
  # Word collapses the merged spanner cells, so the model row reads
  # as three cells: the empty one above "Variable", then one per model.
  expect_identical(rows[[1L]], c("", "OLS", "Logistic"))
  expect_identical(rows[[2L]], c("Variable", "B", "SE", "p", "B", "SE", "p"))
})
