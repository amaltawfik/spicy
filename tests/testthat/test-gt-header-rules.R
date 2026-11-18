# The rules of the header of a gt table (`.spicy_gt_header_css()`) sit in
# the CSS `.spicy_gt()` sets, within the id of the table, so that every
# route that writes HTML -- `gtsave()`, `as_raw_html()` with or without
# inlined CSS, `knit_print()` -- draws the same header.

.gth_tables <- function() {
  list(
    regression = table_regression(
      list(
        stats::lm(mpg ~ wt + hp, data = mtcars),
        stats::lm(mpg ~ wt + hp + qsec, data = mtcars)
      ),
      show_columns = c("b", "ci", "p"),
      output = "gt"
    ),
    categorical = table_categorical(
      sochealth,
      select = c(smoking, physical_activity),
      by = sex,
      output = "gt"
    )
  )
}

.gth_id <- function(g) {
  g[["_options"]]$value[[which(g[["_options"]]$parameter == "table_id")]]
}

test_that("the header rules are in the style sheet, within the table's id", {
  skip_if_not_installed("gt")
  for (nm in names(.gth_tables())) {
    g <- .gth_tables()[[nm]]
    id <- .gth_id(g)
    # Whitespace normalized: the line breaks of gt's style sheet.
    page <- gsub(
      "\\s+",
      " ",
      as.character(gt::as_raw_html(g, inline_css = FALSE))
    )
    s <- paste0("#", id, " ")
    expect_match(
      page,
      paste0(
        s,
        ".gt_col_heading { border-bottom: 1px solid #333333 !important;"
      ),
      fixed = TRUE
    )
    expect_match(
      page,
      paste0(s, ".gt_column_spanner { border-bottom-width: 0px !important;"),
      fixed = TRUE
    )
    expect_match(page, paste0(s, ".gt_spanner_row {"), fixed = TRUE)
    # The label of the first column starts 5px in, as the body labels do.
    expect_match(
      page,
      paste0(
        s,
        ".gt_column_spanner_outer:first-child { padding-left: 5px !important; }"
      ),
      fixed = TRUE
    )
    expect_match(page, paste0(s, ".gt_table tbody .gt_row {"), fixed = TRUE)
    expect_match(
      page,
      paste0(s, "th[id^=\"model_span_\"] > .gt_column_spanner {"),
      fixed = TRUE
    )
    # No pseudo-element, which an inlined copy would lose, and no wrapper.
    expect_no_match(page, "::after", fixed = TRUE)
    expect_no_match(page, "spicy-gt-outer", fixed = TRUE)
    # The knit to HTML writes the same table.
    expect_match(
      gsub("\\s+", " ", as.character(knit_print.spicy_gt(g))),
      paste0(s, ".gt_col_heading {"),
      fixed = TRUE
    )
  }
})

test_that("the header rules survive inlined CSS", {
  skip_if_not_installed("gt")
  tabs <- .gth_tables()
  for (g in tabs) {
    inline <- as.character(gt::as_raw_html(g, inline_css = TRUE))
    heads <- regmatches(
      inline,
      gregexpr("<th class=\"gt_col_heading[^>]*>", inline)
    )[[1L]]
    expect_gt(length(heads), 0L)
    expect_true(all(grepl("border-bottom: 1px solid #333333;", heads)))
    rows <- regmatches(
      inline,
      gregexpr("<td[^>]*class=\"gt_row[^>]*>", inline)
    )[[1L]]
    expect_true(all(grepl("border-top-width: 0px;", rows, fixed = TRUE)))
    spans <- regmatches(
      inline,
      gregexpr("<div class=\"gt_column_spanner\"[^>]*>", inline)
    )[[1L]]
    expect_true(all(grepl(
      "border-bottom-width: 0px; border-bottom-color: transparent;",
      spans,
      fixed = TRUE
    )))
  }
  # The rule under each model spanner is the bottom border of the label,
  # inset by its margins: a pseudo-element would not be inlined.
  inline <- as.character(gt::as_raw_html(tabs$regression, inline_css = TRUE))
  model <- regmatches(
    inline,
    gregexpr(
      "id=\"model_span_[0-9]+\"[^>]*>\\s*<div class=\"gt_column_spanner\"[^>]*>",
      inline
    )
  )[[1L]]
  expect_length(model, 2L)
  expect_true(all(grepl("border-bottom: 1px solid #333333;", model)))
  expect_true(all(grepl(
    "margin-left: calc(0.5em - 4px);",
    model,
    fixed = TRUE
  )))
  expect_match(model[[1L]], "margin-right: calc(0.5em - 4px);", fixed = TRUE)
  expect_match(model[[2L]], "margin-right: 0.5em;", fixed = TRUE)
  # The categorical table has no model spanner.
  cat_inline <- as.character(
    gt::as_raw_html(tabs$categorical, inline_css = TRUE)
  )
  expect_no_match(cat_inline, "margin-left: calc(0.5em - 4px)", fixed = TRUE)
})

test_that("the header rules stay out of the outputs that are not HTML", {
  skip_if_not_installed("gt")
  for (g in .gth_tables()) {
    x <- g
    class(x) <- setdiff(class(x), "spicy_gt")
    for (out in list(gt::as_latex(x), gt::as_rtf(x), gt::as_word(x))) {
      out <- paste(as.character(out), collapse = "\n")
      expect_no_match(out, "!important", fixed = TRUE)
      expect_no_match(out, "gt_column_spanner", fixed = TRUE)
      expect_no_match(out, "model_span_", fixed = TRUE)
    }
  }
})
