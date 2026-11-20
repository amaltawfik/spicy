# Quarto re-parses an HTML table that does not opt out of its processing:
# it stretched a gt table of the package to the width of the page, with
# columns of equal widths and Bootstrap's stripes. Every gt table opts
# out (`.spicy_gt()`), except in a knit to a format Quarto writes from
# gt's HTML (Typst), where it reads the table (`knit_print.spicy_gt()`).

.gtq_tables <- function() {
  list(
    regression = table_regression(
      list(
        A = stats::lm(bmi ~ age + sex, data = sochealth),
        B = stats::lm(wellbeing_score ~ age + sex, data = sochealth)
      ),
      output = "gt"
    ),
    categorical = table_categorical(
      sochealth,
      select = c(smoking, physical_activity),
      by = sex,
      output = "gt"
    ),
    continuous = table_continuous(
      sochealth,
      select = c(age, bmi),
      by = sex,
      output = "gt"
    )
  )
}

.gtq_option <- function(g) {
  opt <- g[["_options"]]
  opt$value[[which(opt$parameter == "quarto_disable_processing")]]
}

.gtq_knit <- function(g, to) {
  old <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  knitr::opts_knit$set(rmarkdown.pandoc.to = to)
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old), add = TRUE)
  as.character(knitr::knit_print(g))
}

test_that("every gt table opts out of Quarto's processing in HTML", {
  skip_if_not_installed("gt")
  for (g in .gtq_tables()) {
    expect_true(.gtq_option(g))
    flag <- "data-quarto-disable-processing=\"true\""
    expect_match(as.character(knit_print.spicy_gt(g)), flag, fixed = TRUE)
    expect_match(.gtq_knit(g, "html"), flag, fixed = TRUE)
    expect_match(
      as.character(gt::as_raw_html(g, inline_css = FALSE)),
      flag,
      fixed = TRUE
    )
  }
})

test_that("a knit to Typst writes no row of empty column labels", {
  skip_if_not_installed("gt")
  # One outcome: no Outcome level over column 1.
  fits <- list(
    A = stats::lm(bmi ~ age, data = sochealth),
    B = stats::lm(bmi ~ age + sex, data = sochealth)
  )
  head_rows <- function(html) {
    thead <- regmatches(html, regexpr("<thead>.*?</thead>", html))
    regmatches(
      thead,
      gregexpr("<tr class=\"gt_col_headings[^\"]*\">.*?</tr>", thead)
    )[[1L]]
  }
  label_cells <- function(row) {
    regmatches(
      row,
      gregexpr("<th class=\"gt_col_heading[^>]*>[^<]*</th>", row)
    )[[1L]]
  }
  # Two models: the Model row, then Variable, B, SE, p as column labels.
  html <- gsub(
    "\n\\s*",
    "",
    .gtq_knit(
      table_regression(fits, output = "gt", show_columns = c("b", "se", "p")),
      "typst"
    )
  )
  rows <- head_rows(html)
  expect_length(rows, 2L)
  labels <- label_cells(paste(rows, collapse = ""))
  expect_identical(
    sub(".*>([^<]*)</th>$", "\\1", labels),
    c("Variable", "B", "SE", "p", "B", "SE", "p")
  )
  # Variable is at the top of the header: it takes the top rule.
  expect_match(labels[[1L]], "border-top-width: 1px", fixed = TRUE)
  expect_no_match(labels[[2L]], "border-top-width: 1px", fixed = TRUE)
  # One model: one row, the top rule over every label.
  html <- gsub(
    "\n\\s*",
    "",
    .gtq_knit(
      table_regression(
        fits["A"],
        output = "gt",
        show_columns = c("b", "se", "p")
      ),
      "typst"
    )
  )
  rows <- head_rows(html)
  expect_length(rows, 1L)
  labels <- label_cells(rows)
  expect_length(labels, 4L)
  expect_true(all(grepl("border-top-width: 1px", labels, fixed = TRUE)))
  expect_true(all(grepl("border-bottom-width: 1px", labels, fixed = TRUE)))
  # A CI keeps its LL and UL row, and the spanners above them.
  ci <- .gtq_knit(table_regression(fits["A"], output = "gt"), "typst")
  expect_match(ci, "id=\"col_span_1_Variable\"", fixed = TRUE)
  # HTML is untouched.
  html <- .gtq_knit(
    table_regression(
      fits["A"],
      output = "gt",
      show_columns = c("b", "se", "p")
    ),
    "html"
  )
  expect_match(html, "id=\"col_span_1_Variable\"", fixed = TRUE)
})

test_that("the Typst header edit gives back a table of another shape", {
  skip_if_not_installed("gt")
  g <- table_regression(
    stats::lm(bmi ~ age, data = sochealth),
    output = "gt",
    show_columns = c("b", "se", "p")
  )
  no_spanners <- g
  no_spanners[["_spanners"]] <- NULL
  expect_identical(.spicy_gt_typst_labels(no_spanners), no_spanners)
  renamed <- g
  names(renamed)[names(renamed) == "_spanners"] <- "_spanners_x"
  expect_identical(.spicy_gt_typst_labels(renamed), renamed)
  no_boxhead <- g
  no_boxhead[["_boxhead"]] <- NULL
  expect_identical(.spicy_gt_typst_labels(no_boxhead), no_boxhead)
  renamed <- g
  names(renamed)[names(renamed) == "_boxhead"] <- "_boxhead_x"
  expect_identical(.spicy_gt_typst_labels(renamed), renamed)
  # A table whose spanners lack a column.
  odd <- g
  odd[["_spanners"]][["spanner_level"]] <- NULL
  expect_identical(.spicy_gt_typst_labels(odd), odd)
  # A call of gt that fails or warns: the table as it came, silently.
  local_mocked_bindings(.spicy_gt_typst_relabel = function(x) stop("gt"))
  expect_no_condition(out <- .spicy_gt_typst_labels(g))
  expect_identical(out, g)
  local_mocked_bindings(.spicy_gt_typst_relabel = function(x) warning("gt"))
  expect_no_condition(out <- .spicy_gt_typst_labels(g))
  expect_identical(out, g)
})

test_that("a knit to Typst rules each model spanner, and only there", {
  skip_if_not_installed("gt")
  # With a CI, the labels lifted into one-column spanners stay spanners.
  g <- table_regression(
    list(
      A = stats::lm(bmi ~ age + sex, data = sochealth),
      B = stats::lm(wellbeing_score ~ age + sex, data = sochealth)
    ),
    output = "gt",
    show_columns = c("b", "ci", "p")
  )
  spanner_cells <- function(html) {
    regmatches(
      html,
      gregexpr("<th [^>]*id=\"[a-z]+_span_[^\"]*\">", html)
    )[[1L]]
  }
  rule <- paste(
    "border-bottom-width: 1px; border-bottom-style: solid;",
    "border-bottom-color: #333333;"
  )
  typst <- .gtq_knit(g, "typst")
  model <- grep("id=\"model_span_", spanner_cells(typst), value = TRUE)
  expect_length(model, 2L)
  expect_true(all(grepl(rule, model, fixed = TRUE)))
  # The labels lifted into one-column spanners take no rule.
  lifted <- grep("id=\"col_span_", spanner_cells(typst), value = TRUE)
  expect_gt(length(lifted), 0L)
  expect_false(any(grepl(rule, lifted, fixed = TRUE)))
  # HTML and LaTeX keep their own rules: the edit is not made there.
  html_model <- grep(
    "id=\"model_span_",
    spanner_cells(.gtq_knit(g, "html")),
    value = TRUE
  )
  expect_length(html_model, 2L)
  expect_false(any(grepl(rule, html_model, fixed = TRUE)))
  local({
    local_mocked_bindings(
      .spicy_gt_typst_spanner_rules = function(x) stop("Typst only")
    )
    expect_no_error(.gtq_knit(g, "html"))
    expect_no_error(.gtq_knit(g, "latex"))
  })
  # A table without model spanners, or a call of gt that fails or warns:
  # the table as it came, silently.
  one <- table_regression(stats::lm(bmi ~ age, data = sochealth), output = "gt")
  expect_identical(.spicy_gt_typst_spanner_rules(one), one)
  local_mocked_bindings(tab_style = function(...) stop("gt"), .package = "gt")
  expect_no_condition(out <- .spicy_gt_typst_spanner_rules(g))
  expect_identical(out, g)
  local_mocked_bindings(
    tab_style = function(...) warning("gt"),
    .package = "gt"
  )
  expect_no_condition(out <- .spicy_gt_typst_spanner_rules(g))
  expect_identical(out, g)
})

test_that("a knit to Typst sizes the columns on their content", {
  skip_if_not_installed("gt")
  cols <- function(html) {
    w <- regmatches(html, gregexpr("<col style=\"width:[0-9.]+%;\"/>", html))
    as.numeric(gsub("[^0-9.]", "", w[[1L]]))
  }
  for (g in .gtq_tables()) {
    w <- cols(.gtq_knit(g, "typst"))
    expect_length(w, nrow(g[["_boxhead"]]))
    expect_equal(sum(w), 100, tolerance = 0.001)
    # The column of labels is the widest: "(Intercept)", "Variable".
    expect_identical(which.max(w), 1L)
    # HTML and LaTeX keep their own widths.
    expect_length(cols(.gtq_knit(g, "html")), 0L)
    expect_no_match(.gtq_knit(g, "latex"), "%;", fixed = TRUE)
  }
  # The widest text of a column, plus two characters of insets; a label
  # lifted into a spanner over the column alone counts.
  g <- gt::tab_spanner(
    gt::gt(data.frame(a = c("x", "y"), b = c("1", "22"))),
    "Variable",
    columns = "a"
  )
  w <- .spicy_gt_typst_widths(g)[["_boxhead"]]$column_width
  expect_identical(unlist(w), c("71.43%", "28.57%"))
  # A call of gt that fails or warns: the table as it came, silently.
  local_mocked_bindings(cols_width = function(...) stop("gt"), .package = "gt")
  expect_no_condition(out <- .spicy_gt_typst_widths(g))
  expect_identical(out, g)
})

test_that("a knit to Typst leaves the table to Quarto's processing", {
  skip_if_not_installed("gt")
  for (g in .gtq_tables()) {
    out <- .gtq_knit(g, "typst")
    expect_match(out, "data-quarto-disable-processing=\"false\"", fixed = TRUE)
    expect_no_match(
      out,
      "data-quarto-disable-processing=\"true\"",
      fixed = TRUE
    )
  }
})
