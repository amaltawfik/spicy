# Parity of the tinytable engine with the console, on the four
# artefacts the audit found missing from the rendered table:
#
#   * the APA rule structure of the header -- a top rule above the
#     outermost header row, a trimmed rule under each model spanner, a
#     rule under the column labels -- which used to be drawn one row
#     too low on every table without a CI column, below an empty
#     header strip the engine also emitted;
#   * every rule the console draws between blocks (fit-stats,
#     Thresholds, Random effects, variable blocks);
#   * the auto-documenting material of the descriptive families
#     (title as caption, association gloss as note);
#   * the note itself: one disclosure per line, subordinated by size.
#
# Oracles are the console attributes and the rendered LaTeX / Typst /
# HTML strings.

.pt_data <- function() {
  d <- as.data.frame(sochealth)
  d$educ <- factor(as.character(d$education), levels = levels(d$education))
  d
}

.pt_latex <- function(tt) tinytable::save_tt(tt, output = "latex")
.pt_html <- function(tt) tinytable::save_tt(tt, output = "html")
.pt_typst <- function(tt) tinytable::save_tt(tt, output = "typst")

# Body rows of the rendered LaTeX table (header rows included, in the
# order tabularray counts them for `hline{i}`).
.pt_rows <- function(latex) {
  ln <- strsplit(latex, "\n", fixed = TRUE)[[1L]]
  ln[grepl("\\\\\\\\$", ln)]
}

# The `i` of every `hline{i}` carrying `width`.
.pt_hlines <- function(latex, width) {
  m <- gregexpr(
    paste0("hline\\{([0-9]+)\\}=\\{[^}]*\\}\\{[^}]*", width, "em"),
    latex
  )
  hits <- regmatches(latex, m)[[1L]]
  sort(unique(as.integer(sub("^hline\\{([0-9]+)\\}.*$", "\\1", hits))))
}


# ---- header rules ---------------------------------------------------------

test_that("a table with no CI column rules its header like one with", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  m1 <- stats::lm(wellbeing_score ~ age + sex, data = d)
  m2 <- stats::glm(
    dentist_12m ~ age + sex,
    data = d,
    family = stats::binomial()
  )
  tt <- table_regression(
    list(OLS = m1, Logistic = m2),
    show_columns = c("b", "se", "p"),
    output = "tinytable"
  )
  tex <- .pt_latex(tt)
  rows <- .pt_rows(tex)
  # Two header rows, as in the console: model names, then column
  # labels. The third, wholly empty row is the defect this pins.
  expect_match(rows[1L], "OLS")
  expect_match(rows[2L], "Variable & B & SE & p & B & SE & p")
  expect_false(grepl("^(\\s*&)+\\s*\\\\\\\\$", rows[3L]))
  # Top rule above the model row (1), rule under the labels (3).
  full <- .pt_hlines(tex, "0.06")
  expect_true(1L %in% full)
  expect_true(3L %in% full)
  # Trimmed per-model rules under the MODEL names (row 2's top edge),
  # not under the column labels.
  expect_match(
    tex,
    "hline\\{2\\}=\\{[0-9,-]+\\}\\{solid, black, 0.06em, l=-0.5\\}"
  )
})

test_that("a single-model table with no CI opens with a rule", {
  skip_if_not_installed("tinytable")
  fit <- stats::lm(wellbeing_score ~ age + sex, data = .pt_data())
  tex <- .pt_latex(table_regression(
    fit,
    show_columns = c("b", "se", "p"),
    output = "tinytable"
  ))
  expect_true(1L %in% .pt_hlines(tex, "0.06"))
  rows <- .pt_rows(tex)
  expect_match(rows[1L], "Variable & B & SE & p")
  expect_match(rows[2L], "(Intercept)", fixed = TRUE)
})

test_that("the CI header keeps its three rows and its bracket rule", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  m1 <- stats::lm(wellbeing_score ~ age + sex, data = d)
  m2 <- stats::glm(
    dentist_12m ~ age + sex,
    data = d,
    family = stats::binomial()
  )
  tex <- .pt_latex(table_regression(
    list(OLS = m1, Logistic = m2),
    show_columns = c("b", "ci", "p"),
    output = "tinytable"
  ))
  rows <- .pt_rows(tex)
  expect_match(rows[1L], "OLS")
  expect_match(rows[2L], "95\\\\% CI")
  expect_match(rows[3L], "LL & UL")
  full <- .pt_hlines(tex, "0.06")
  expect_true(1L %in% full) # top rule above the model row
  expect_true(4L %in% full) # rule under LL / UL
})


# ---- separators between blocks -------------------------------------------

test_that("every console separator is drawn, not only the first", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("MASS")
  d <- .pt_data()
  fit <- MASS::polr(self_rated_health ~ age + educ, data = d, Hess = TRUE)
  tbl <- table_regression(fit)
  # Console oracle: a rule before "Thresholds:" and one before the
  # fit-stats block.
  sec <- as.integer(attr(tbl, "section_sep_rows"))
  grp <- as.integer(attr(tbl, "group_sep_rows"))
  expect_length(sec, 1L)
  expect_length(grp, 1L)
  tex <- .pt_latex(table_regression(fit, output = "tinytable"))
  light <- .pt_hlines(tex, "0.03")
  # `hline{i}` rules the TOP of table row i, and the two header rows
  # (CI bracket) sit above the body.
  header_rows <- 2L
  expect_setequal(light, c(sec, grp) + header_rows)
})

test_that("mixed models rule off their random-effects block", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("lme4")
  d <- .pt_data()
  fit <- suppressMessages(suppressWarnings(
    lme4::lmer(wellbeing_score ~ age + sex + (1 | region), data = d)
  ))
  tbl <- suppressWarnings(table_regression(fit))
  expect_length(as.integer(attr(tbl, "section_sep_rows")), 1L)
  tex <- .pt_latex(suppressWarnings(
    table_regression(fit, output = "tinytable")
  ))
  expect_length(.pt_hlines(tex, "0.03"), 2L)
})

test_that("table_categorical separates its variable blocks", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  tex <- .pt_latex(table_categorical(d, c(sex, smoking), output = "tinytable"))
  # One rule, above the second variable block.
  expect_length(.pt_hlines(tex, "0.03"), 1L)
  tex2 <- .pt_latex(table_categorical(
    d,
    c(sex, smoking, employment_status),
    by = education,
    output = "tinytable"
  ))
  expect_length(.pt_hlines(tex2, "0.03"), 2L)
})


# ---- descriptive families: caption + note --------------------------------

test_that("the descriptive families carry their title as a caption", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  expect_match(
    .pt_html(table_categorical(d, c(sex, smoking), output = "tinytable")),
    "<caption>Categorical table</caption>",
    fixed = TRUE
  )
  expect_match(
    .pt_html(table_categorical(d, smoking, by = sex, output = "tinytable")),
    "<caption>Categorical table by sex</caption>",
    fixed = TRUE
  )
  expect_match(
    .pt_html(table_continuous(
      d,
      c(age, wellbeing_score),
      output = "tinytable"
    )),
    "<caption>Descriptive statistics</caption>",
    fixed = TRUE
  )
})

test_that("the association gloss reaches the by-group table", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  # Two measures in one table (2x2 -> Phi, 3x2 -> Cramer's V) is what
  # makes the console print the gloss.
  tbl <- table_categorical(d, c(smoking, educ), by = sex)
  gloss <- attr(tbl, "assoc_note")
  skip_if(is.null(gloss))
  html <- .pt_html(table_categorical(
    d,
    c(smoking, educ),
    by = sex,
    output = "tinytable"
  ))
  expect_match(html, gloss, fixed = TRUE)
})

test_that("the one-way table labels its first column like the console", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  tt <- table_categorical(d, c(sex, smoking), output = "tinytable")
  expect_match(.pt_html(tt), ">Variable</th>", fixed = TRUE)
})


# ---- notes: one disclosure per line, one size down ------------------------

.pt_multiline_note <- function(o) {
  d <- .pt_data()
  m1 <- stats::lm(wellbeing_score ~ age, data = d)
  m2 <- stats::glm(dentist_12m ~ age, data = d, family = stats::binomial())
  table_regression(list(OLS = m1, Logistic = m2), output = o)
}

test_that("a multi-line note keeps its lines in HTML and Typst", {
  skip_if_not_installed("tinytable")
  note <- attr(.pt_multiline_note("default"), "note")
  expect_match(note, "\n", fixed = TRUE) # oracle: the console breaks lines
  tt <- .pt_multiline_note("tinytable")
  html <- .pt_html(tt)
  expect_match(html, "Std. errors:<br>", fixed = TRUE)
  # The console indents the per-model attributions; the indent survives
  # as no-break spaces.
  expect_match(html, "<br>&nbsp;&nbsp;Model 1:", fixed = TRUE)
  # Typst collapses a bare newline exactly as HTML does: the line
  # ends with a forced break, the indent becomes no-break spaces.
  expect_match(.pt_typst(tt), "Std. errors: \\\n~~Model 1:", fixed = TRUE)
})

test_that("notes are subordinated by size, in black, by default", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  fit <- stats::lm(wellbeing_score ~ age, data = d)
  tt <- table_regression(fit, output = "tinytable")
  expect_match(.pt_html(tt), "font-size: 0.9em;", fixed = TRUE)
  expect_match(.pt_typst(tt), "text(size: 0.9em, [Note.", fixed = TRUE)
  # The descriptive families keep tinytable's own <tfoot> cell: it is
  # restyled in place.
  cat_tt <- table_categorical(
    d,
    c(sex, smoking),
    drop_na = TRUE,
    output = "tinytable"
  )
  expect_match(
    .pt_html(cat_tt),
    "<td colspan='3' style=\"font-size: 0.9em; \">",
    fixed = TRUE
  )
  # No colour is imposed: the note inherits the document's text colour.
  note_cell <- grep(
    "table.cell(align: left, colspan",
    strsplit(.pt_typst(tt), "\n", fixed = TRUE)[[1L]],
    fixed = TRUE,
    value = TRUE
  )
  expect_length(note_cell, 1L)
  expect_no_match(note_cell, "fill:", fixed = TRUE)
})

test_that("options(spicy.note_style) opts out or adds Typst styling", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  fit <- stats::lm(wellbeing_score ~ age, data = d)

  withr::with_options(list(spicy.note_style = "none"), {
    tt <- table_regression(fit, output = "tinytable")
    expect_no_match(.pt_html(tt), "font-size: 0.9em", fixed = TRUE)
    expect_match(.pt_typst(tt), "text([Note.", fixed = TRUE)
    cat_tt <- table_categorical(d, sex, drop_na = TRUE, output = "tinytable")
    expect_no_match(.pt_html(cat_tt), "font-size", fixed = TRUE)
  })

  withr::with_options(list(spicy.note_style = "fill: luma(89)"), {
    tt <- table_regression(fit, output = "tinytable")
    expect_match(
      .pt_typst(tt),
      "text(size: 0.9em, fill: luma(89), [Note.",
      fixed = TRUE
    )
    # Typst-only: the HTML note keeps the plain size.
    expect_match(.pt_html(tt), "font-size: 0.9em;", fixed = TRUE)
  })

  withr::with_options(list(spicy.note_style = 0.8), {
    expect_error(
      table_regression(fit, output = "tinytable"),
      class = "spicy_invalid_input"
    )
  })
})

test_that("a note the user styled through tinytable is left alone", {
  skip_if_not_installed("tinytable")
  fit <- stats::lm(wellbeing_score ~ age, data = .pt_data())
  tt <- table_regression(fit, output = "tinytable")
  tt <- tinytable::style_tt(tt, i = "notes", fontsize = 1.4)
  typ <- .pt_typst(tt)
  expect_match(typ, "size: 1.4em", fixed = TRUE)
  expect_no_match(typ, "size: 0.9em", fixed = TRUE)
})


test_that("the note rewriters leave a string they do not recognise alone", {
  # A note cell whose closing `])),` never comes (a tinytable template
  # change would look like this): the rewriter must not invent one.
  broken <- paste(
    "    table.cell(align: left, colspan: 6, text([Note. One",
    "still going",
    sep = "\n"
  )
  expect_identical(spicy:::.spicy_tt_note_typst(broken), broken)
  # Nothing to wrap in a table that carries no note.
  expect_identical(spicy:::.spicy_tt_note_typst("#table()"), "#table()")
  # Nothing to restyle in a table that carries no note.
  expect_identical(
    spicy:::.spicy_tt_note_tfoot("<table><tr><td>1</td></tr></table>"),
    "<table><tr><td>1</td></tr></table>"
  )
  # A single-line note is passed through untouched.
  expect_identical(spicy:::.spicy_note_html_lines("one line"), "one line")
})


# ---- indentation ----------------------------------------------------------

test_that("factor levels are indented once, by the engine", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  fit <- stats::lm(wellbeing_score ~ age + sex, data = d)
  tbl <- table_regression(fit)
  # Console oracle: the display body carries the two-space indent.
  expect_true(any(grepl("^  Male$", tbl$Variable)))
  tt <- table_regression(fit, output = "tinytable")
  # The cell text is clean; the indent is a style, so it survives every
  # backend instead of collapsing in HTML.
  expect_true("Male" %in% tt@data[[1L]])
  expect_false(any(grepl("^\\s", tt@data[[1L]])))
  expect_match(.pt_latex(tt), "preto={\\hspace{1em}}", fixed = TRUE)
  expect_match(.pt_html(tt), "padding-left: 1.4em;", fixed = TRUE)
})
