# Parity of the tinytable engine with the console, on the four
# artifacts the audit found missing from the rendered table:
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
# The markdown grid as a document gets it, while knitr runs.
.pt_markdown <- function(tt) {
  withr::with_options(
    list(knitr.in.progress = TRUE),
    tinytable::save_tt(tt, output = "markdown")
  )
}

# Body rows of the rendered LaTeX table (header rows included, in the
# order tabularray counts them for `hline{i}`). Read below the inner
# specification: the note above it breaks its lines with `\\` as well.
.pt_rows <- function(latex) {
  ln <- strsplit(latex, "\n", fixed = TRUE)[[1L]]
  ln <- ln[-seq_len(grep("tabularray inner close", ln, fixed = TRUE)[1L])]
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

test_that("a one-way continuous table draws no inter-variable rule; by-group does", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  # One-way: each variable block is a single row, so a rule between the
  # blocks is ink without information -- decision 37 draws none, in the
  # engines exactly as on the console.
  tex_ow <- .pt_latex(table_continuous(
    d,
    c(age, wellbeing_score),
    output = "tinytable"
  ))
  expect_length(.pt_hlines(tex_ow, "0.03"), 0L)
  # By-group: the blocks are multi-row and the rule separates variables,
  # so it is kept (one rule, above the second variable's block).
  tex_bg <- .pt_latex(table_continuous(
    d,
    c(age, wellbeing_score),
    by = sex,
    output = "tinytable"
  ))
  expect_length(.pt_hlines(tex_bg, "0.03"), 1L)
})

test_that("the survey twin follows the same one-way rule policy", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  des <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
  tex_ow <- .pt_latex(table_continuous_svy(
    des,
    select = c(api00, api99),
    output = "tinytable"
  ))
  expect_length(.pt_hlines(tex_ow, "0.03"), 0L)
  tex_bg <- .pt_latex(table_continuous_svy(
    des,
    select = c(api00, api99),
    by = stype,
    output = "tinytable"
  ))
  expect_true(length(.pt_hlines(tex_bg, "0.03")) >= 1L)
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

# A note of two lines: the general note, one paragraph, then the legend
# of the stars on a line of its own.
.pt_multiline_note <- function(o) {
  d <- .pt_data()
  m1 <- stats::lm(wellbeing_score ~ age, data = d)
  m2 <- stats::glm(dentist_12m ~ age, data = d, family = stats::binomial())
  table_regression(list(OLS = m1, Logistic = m2), stars = TRUE, output = o)
}

test_that("a multi-line note keeps its lines in HTML and Typst", {
  skip_if_not_installed("tinytable")
  note <- attr(.pt_multiline_note("default"), "note")
  # Oracle: the console breaks the line before the legend, and only there.
  expect_length(strsplit(note, "\n", fixed = TRUE)[[1L]], 2L)
  tt <- .pt_multiline_note("tinytable")
  html <- .pt_html(tt)
  expect_match(html, "criterion.<br>*** p &lt; .001", fixed = TRUE)
  # Typst collapses a bare newline exactly as HTML does: the line
  # ends with a forced break.
  expect_match(.pt_typst(tt), "criterion. \\\n\\*\\*\\* p", fixed = TRUE)
})

test_that("notes are subordinated by size, in black, by default", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  fit <- stats::lm(wellbeing_score ~ age, data = d)
  tt <- table_regression(fit, output = "tinytable")
  expect_match(.pt_html(tt), "font-size: 0.9em;", fixed = TRUE)
  expect_match(
    .pt_typst(tt),
    "text(size: 0.9em, [#emph[Note.]",
    fixed = TRUE
  )
  expect_match(.pt_latex(tt), "note{}={\\small \\emph{Note.}", fixed = TRUE)
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
  # No color is imposed: the note inherits the document's text color.
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
    expect_match(.pt_typst(tt), "text([#emph[Note.]", fixed = TRUE)
    expect_match(.pt_latex(tt), "note{}={\\emph{Note.}", fixed = TRUE)
    cat_tt <- table_categorical(d, sex, drop_na = TRUE, output = "tinytable")
    expect_no_match(.pt_html(cat_tt), "font-size", fixed = TRUE)
  })

  withr::with_options(list(spicy.note_style = "fill: luma(89)"), {
    tt <- table_regression(fit, output = "tinytable")
    expect_match(
      .pt_typst(tt),
      "text(size: 0.9em, fill: luma(89), [#emph[Note.]",
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


test_that("the note rewriters leave a string they do not recognize alone", {
  # A note cell whose closing `])),` never comes (a tinytable template
  # change would look like this): the rewriter must not invent one.
  broken <- paste(
    "    table.cell(align: left, colspan: 6, text([Note. One",
    "still going",
    sep = "\n"
  )
  expect_identical(spicy:::.spicy_tt_note_typst(broken), broken)
  # Not even to set its prefix in italics, when handed the note.
  expect_identical(
    spicy:::.spicy_tt_note_typst(broken, "Note. One\nstill going"),
    broken
  )
  # Nothing to wrap in a table that carries no note.
  expect_identical(spicy:::.spicy_tt_note_typst("#table()"), "#table()")
  expect_identical(
    spicy:::.spicy_tt_note_latex("\\begin{tblr}", "Note. x"),
    "\\begin{tblr}"
  )
  # Nothing to restyle in a table that carries no note.
  expect_identical(
    spicy:::.spicy_tt_note_tfoot("<table><tr><td>1</td></tr></table>"),
    "<table><tr><td>1</td></tr></table>"
  )
  # A single-line note is passed through untouched.
  expect_identical(spicy:::.spicy_note_lines("one line"), "one line")
})


# ---- the note prefix in italics, in every output --------------------------
#
# APA Manual 7 Section 7.14: the note opens with "Note." in italics. HTML
# had it; Typst, LaTeX and markdown printed it in roman. The prefix is
# read from the registry, so the expectations are too.

.pt_mr <- function(o = "tinytable") {
  table_multiresponse(
    .pt_data(),
    select = c(info_doctor, info_pharmacist, info_internet),
    by = sex,
    output = o
  )
}

test_that("the note prefix is in italics in Typst, LaTeX and markdown", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  fit <- stats::lm(wellbeing_score ~ age + sex, data = d)
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    p <- spicy_str("note_prefix_emphasis")
    tables <- list(
      multiresponse = .pt_mr(),
      continuous = table_continuous(
        d,
        c(bmi, wellbeing_score),
        output = "tinytable"
      ),
      continuous_by = table_continuous(
        d,
        c(bmi, life_sat_health),
        by = sex,
        output = "tinytable"
      ),
      regression = table_regression(fit, output = "tinytable")
    )
    for (nm in names(tables)) {
      tt <- tables[[nm]]
      info <- paste(lang, nm)
      expect_match(
        .pt_typst(tt),
        paste0("[#emph[", p, "]"),
        fixed = TRUE,
        info = info
      )
      expect_match(
        .pt_latex(tt),
        paste0("\\emph{", p, "}"),
        fixed = TRUE,
        info = info
      )
      expect_match(
        .pt_markdown(tt),
        paste0("| *", p, "* "),
        fixed = TRUE,
        info = info
      )
      expect_match(
        .pt_html(tt),
        paste0("<em>", p, "</em>"),
        fixed = TRUE,
        info = info
      )
    }
  }
})

test_that("the markdown note is one paragraph, inside its grid", {
  skip_if_not_installed("tinytable")
  tt <- .pt_multiline_note("tinytable")
  for (md in list(
    .pt_markdown(tt),
    tinytable::save_tt(tt, output = "markdown")
  )) {
    grid <- strsplit(md, "\n", fixed = TRUE)[[1L]]
    grid <- grid[nzchar(grid) & !startsWith(grid, "Table:")]
    # Every line opens and closes on the border, the note's lines too: a
    # newline written into the grid broke it, and pandoc dropped the note
    # from a Word document.
    expect_true(all(grepl("^[|+].*[|+]$", trimws(grid, "right"))))
    expect_length(unique(nchar(trimws(grid, "right"))), 1L)
  }
  expect_match(
    .pt_markdown(tt),
    "| *Note.* OLS: linear regression;",
    fixed = TRUE
  )
})

test_that("a tinytable printed in a terminal keeps its note plain", {
  # The markdown grid is the console print of a tinytable as well as the
  # table of a Word document: the emphasis is markup for pandoc, and a
  # terminal printed it as two stars.
  skip_if_not_installed("tinytable")
  fit <- stats::lm(wellbeing_score ~ age, data = .pt_data())
  tt <- table_regression(fit, output = "tinytable")
  withr::local_options(knitr.in.progress = NULL)
  md <- tinytable::save_tt(tt, output = "markdown")
  expect_match(md, "| Note. Linear regression.", fixed = TRUE)
  expect_no_match(md, "*", fixed = TRUE)
  expect_match(.pt_markdown(tt), "| *Note.* Linear regression.", fixed = TRUE)
})

test_that("a typed note opens with the lead-in of the style, in italics", {
  skip_if_not_installed("tinytable")
  fit <- stats::lm(wellbeing_score ~ age, data = .pt_data())
  tt <- table_regression(fit, note = "Source: a survey.", output = "tinytable")
  expect_match(
    .pt_typst(tt),
    "text(size: 0.9em, [#emph[Note.] Source: a survey.])",
    fixed = TRUE
  )
  expect_match(.pt_latex(tt), "\\emph{Note.} Source: a survey.", fixed = TRUE)
  expect_match(.pt_markdown(tt), "| *Note.* Source: a survey.", fixed = TRUE)
  expect_match(.pt_html(tt), "<em>Note.</em> Source: a survey.", fixed = TRUE)
})

test_that("a note that does not open with the prefix is left as typed", {
  skip_if_not_installed("tinytable")
  fit <- stats::lm(wellbeing_score ~ age, data = .pt_data())
  # No lead-in under `note_label = ""`: the typed note stands alone.
  tt <- table_regression(
    fit,
    note = "Source: a survey.",
    style = spicy_style(note_label = ""),
    output = "tinytable"
  )
  expect_match(
    .pt_typst(tt),
    "text(size: 0.9em, [Source: a survey.])",
    fixed = TRUE
  )
  expect_match(
    .pt_latex(tt),
    "note{}={\\small Source: a survey.}",
    fixed = TRUE
  )
  expect_match(.pt_markdown(tt), "| Source: a survey.", fixed = TRUE)
  expect_no_match(.pt_typst(tt), "#emph[", fixed = TRUE)
  expect_no_match(.pt_latex(tt), "\\emph{", fixed = TRUE)
  expect_no_match(.pt_html(tt), "<em>", fixed = TRUE)
})

# A French source with its no-break spaces made visible: in the file they
# look like any other space, and a snapshot must show which one it is.
.pt_nbsp <- function(x) gsub("\u00A0", "<U+00A0>", x, fixed = TRUE)

test_that("the Typst source of a French note opened by the colon lead-in", {
  skip_if_not_installed("tinytable")
  withr::local_options(spicy.language = "fr")
  set.seed(6)
  d <- data.frame(
    x = factor(sample(c("oui", "non"), 40L, TRUE)),
    g = factor(rep(c("A", "B"), 20L))
  )
  expect_snapshot(cat(.pt_nbsp(.pt_typst(table_categorical(
    d,
    x,
    by = g,
    style = spicy_style(note_label = "Note:"),
    output = "tinytable"
  )))))
})

test_that("the Typst source of a note that defines every abbreviation", {
  skip_if_not_installed("tinytable")
  withr::local_options(spicy.language = "fr")
  set.seed(8)
  d <- data.frame(
    y = round(stats::rnorm(30, 50, 10), 1),
    g = factor(rep(c("A", "B", "C"), 10L))
  )
  expect_snapshot(cat(.pt_nbsp(.pt_typst(table_continuous_lm(
    d,
    y,
    by = g,
    effect_size = "omega2",
    style = spicy_style(abbreviations = "all"),
    output = "tinytable"
  )))))
})

test_that("the LaTeX note is escaped like the cells around it", {
  skip_if_not_installed("tinytable")
  # The multiple-response note says "more than 100%": unescaped, the
  # `%` commented out the rest of the note and its closing brace.
  tex <- .pt_latex(.pt_mr())
  expect_match(tex, "more than 100\\%.", fixed = TRUE)
  # A variable name in the missing-values note: an unescaped `_` stopped
  # the compilation.
  tex <- .pt_latex(table_continuous(
    .pt_data(),
    c(bmi, life_sat_health),
    by = sex,
    output = "tinytable"
  ))
  expect_match(tex, "life\\_sat\\_health (8)", fixed = TRUE)
  # Every character of the set, in one note.
  fit <- stats::lm(wellbeing_score ~ age, data = .pt_data())
  tt <- table_regression(
    fit,
    note = "Note. 5% & a_b #1 $2 ~3 ^4 {5} \\6",
    output = "tinytable"
  )
  expect_match(
    .pt_latex(tt),
    paste0(
      "\\emph{Note.} 5\\% \\& a\\_b \\#1 \\$2 \\textasciitilde{}3 ",
      "\\textasciicircum{}4 \\{5\\} \\textbackslash{}6}"
    ),
    fixed = TRUE
  )
})

test_that("the Typst note is escaped like the cells around it", {
  skip_if_not_installed("tinytable")
  fit <- stats::lm(wellbeing_score ~ age + sex, data = .pt_data())
  # The significance legend: read as markup, its stars turned to bold and
  # vanished, and Typst warned of a strong emphasis with no text.
  typ <- .pt_typst(table_regression(fit, stars = TRUE, output = "tinytable"))
  expect_match(
    typ,
    "\\*\\*\\* p \\< .001, \\*\\* p \\< .01, \\* p \\< .05.",
    fixed = TRUE
  )
  # A URL, whose `//` opened a Typst comment and broke the compilation,
  # and every other character of tinytable's Typst set, in one note.
  tt <- table_regression(
    fit,
    note = "Note. See https://x.org/a_b #1 $2 <x> @y [z] `c` ~d \"e\" f-g h=i j+k \\l",
    output = "tinytable"
  )
  expect_match(
    .pt_typst(tt),
    paste0(
      "[#emph[Note.] See https:\\/\\/x.org\\/a\\_b \\#1 \\$2 \\<x\\> \\@y ",
      "\\[z\\] \\`c\\` \\~d \\\"e\\\" f\\-g h\\=i j\\+k \\\\l])"
    ),
    fixed = TRUE
  )
})

test_that("Typst sizes the columns with the inset the table is set with", {
  # tinytable measured each column with Typst's default 5pt inset, the
  # table took the document's: Quarto sets 6pt, so every column came out
  # 2pt short and its widest cell wrapped ("Fe-male"). And Quarto
  # justifies paragraphs, which hyphenates them: the table is set ragged
  # right, so a cell wraps between words, never inside one.
  skip_if_not_installed("tinytable")
  typ <- .pt_typst(table_continuous(
    .pt_data(),
    c(bmi, life_sat_health),
    by = sex,
    output = "tinytable"
  ))
  expect_match(
    typ,
    "measure(grid(columns: 1, inset: table.inset, ..col))",
    fixed = TRUE
  )
  expect_no_match(typ, "inset: 5pt", fixed = TRUE)
  expect_match(
    typ,
    "// start block\n  #set par(justify: false)\n",
    fixed = TRUE
  )
  # Substitutions on tinytable's template: a string without its lines
  # comes back as it went in.
  expect_identical(spicy:::.spicy_tt_typst_layout("#table()"), "#table()")
})

test_that("Typst widens the columns under a group label wider than them", {
  # The natural widths tinytable measures read each column's cells and
  # name, not the group labels above them: three `by` levels over narrow
  # n and % columns ran into each other ("EmployeursParticipants").
  skip_if_not_installed("tinytable")
  d <- .pt_data()[1:90, ]
  roles <- c("Employeurs", "Participants", "Etudiants")
  d$role <- factor(roles[as.integer(d$education)], levels = roles)
  typ <- .pt_typst(table_categorical(d, sex, by = role, output = "tinytable"))
  expect_match(
    typ,
    paste0(
      "for (spicy_from, spicy_to, spicy_label) in ((0, 0, [Variable]), ",
      "(1, 2, [Employeurs]), (3, 4, [Participants]), (5, 6, [Etudiants]), ",
      "(7, 8, [Total]), (9, 9, [p]), (10, 10, [V]),) {"
    ),
    fixed = TRUE
  )
  # Measured before the total width is taken from the columns.
  expect_lt(
    as.integer(regexpr("spicy_label) in", typ, fixed = TRUE)),
    as.integer(regexpr("let tinytable-total", typ, fixed = TRUE))
  )
  # No note, no measure: tinytable gives Typst `auto` columns, which
  # Typst widens for a spanning cell itself. The note says N whenever the
  # table builds it, so `note = FALSE` takes it away.
  bare <- .pt_typst(table_categorical(
    d,
    sex,
    by = role,
    p_value = FALSE,
    assoc_measure = "none",
    note = FALSE,
    output = "tinytable"
  ))
  expect_match(bare, "columns: (auto,", fixed = TRUE)
  expect_no_match(bare, "spicy_label", fixed = TRUE)
  expect_identical(spicy:::.spicy_tt_typst_spans("x", NULL), "x")
})

test_that("a LaTeX table wider than the text block is scaled down to it", {
  skip_if_not_installed("tinytable")
  tt <- table_categorical(
    .pt_data(),
    c(sex, smoking),
    by = educ,
    output = "tinytable"
  )
  tex <- .pt_latex(tt)
  # Scaled to the line width only when wider: a narrow table keeps its
  # size. The note of a talltblr is inside the box, so it follows.
  expect_match(
    tex,
    paste0(
      "\\resizebox{\\ifdim\\width>\\linewidth\\linewidth\\else\\width\\fi}",
      "{!}{%\n\\begin{talltblr}"
    ),
    fixed = TRUE
  )
  expect_match(tex, "\\end{talltblr}%\n}", fixed = TRUE)
  # A table sent across pages is not one box: the conversion to a
  # longtblr, asked for after spicy built the table, is left unscaled.
  tex <- .pt_latex(tinytable::theme_latex(tt, multipage = TRUE))
  expect_match(tex, "\\begin{longtblr}", fixed = TRUE)
  expect_no_match(tex, "\\resizebox", fixed = TRUE)
  # A resize the user asked for is left as it is, not doubled.
  tex <- .pt_latex(tinytable::theme_latex(tt, resize_direction = "both"))
  expect_identical(
    lengths(regmatches(tex, gregexpr("\\resizebox", tex, fixed = TRUE))),
    1L
  )
})

test_that("the LaTeX note keeps one disclosure per line, as HTML and Typst", {
  skip_if_not_installed("tinytable")
  tex <- .pt_latex(.pt_multiline_note("tinytable"))
  expect_match(tex, "criterion.\\\\\n*** p < .001", fixed = TRUE)
})

test_that("LaTeX output sets the characters spicy writes in every engine", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  cp <- function(x) intToUtf8(x)
  # The chi-squared of the multiple-response note, in math mode: Latin
  # Modern, the default font of xelatex and lualatex, has no Greek.
  tex <- .pt_latex(.pt_mr())
  expect_match(tex, "\\ensuremath{\\chi^{2}}(", fixed = TRUE)
  expect_no_match(tex, cp(0x03C7), fixed = TRUE)
  # The figure space of the decimal alignment, as a phantom digit.
  fit <- stats::lm(wellbeing_score ~ age + sex, data = d)
  tex <- .pt_latex(table_regression(fit, output = "tinytable"))
  expect_match(tex, "\\hphantom{0}", fixed = TRUE)
  expect_no_match(tex, cp(0x2007), fixed = TRUE)
  # The Delta of the linear-model family's difference column.
  tex <- .pt_latex(table_continuous_lm(
    d,
    select = wellbeing_score,
    by = sex,
    output = "tinytable"
  ))
  expect_match(tex, "\\ensuremath{\\Delta}", fixed = TRUE)
  expect_no_match(tex, cp(0x0394), fixed = TRUE)
  # The mapping itself: accented letters first, squared letters as a
  # superscript, every other character left to the engine.
  map <- spicy:::.spicy_latex_unicode
  expect_identical(
    map(cp(c(0x03C3, 0x0302))),
    "\\ensuremath{\\hat{\\sigma}}"
  )
  expect_identical(
    map(cp(c(0x03C7, 0x0304, 0x00B2))),
    "\\ensuremath{\\bar{\\chi}^{2}}"
  )
  expect_identical(map(cp(0x2212)), "\\ensuremath{-}")
  # The signs and the letters of a formula in a note, and the gamma of an
  # association header: math in every engine. The plus-minus sign, which
  # every engine sets, takes the font of the minus sign beside it.
  expect_identical(map(cp(0x2264)), "\\ensuremath{\\leq}")
  expect_identical(map(cp(0x00B1)), "\\ensuremath{\\pm}")
  expect_identical(map(cp(0x03B6)), "\\ensuremath{\\zeta}")
  expect_identical(map(cp(0x03B3)), "\\ensuremath{\\gamma}")
  # The en dash of a range or of an undefined cell is left to the engine,
  # which sets it under pdflatex, xelatex and lualatex alike.
  expect_identical(
    map(cp(c(0x00E9, 0x2013, 0x00A0))),
    cp(c(0x00E9, 0x2013, 0x00A0))
  )
  # Cohen's f2 is set as math like the squared Greek letters, wherever it
  # stands on its own; the R2 of a fit statistic and a squared letter
  # ending a word are left to the engine.
  f2 <- paste0("f", cp(0x00B2))
  expect_identical(map(f2), "\\ensuremath{f^{2}}")
  expect_identical(
    map(paste0("Cohen's partial ", f2, " [95% CI]")),
    "Cohen's partial \\ensuremath{f^{2}} [95% CI]"
  )
  expect_identical(map(paste0("R", cp(0x00B2))), paste0("R", cp(0x00B2)))
  expect_identical(map(paste0("coef", cp(0x00B2))), paste0("coef", cp(0x00B2)))
  # A table that shows the f2 column writes no raw superscript two.
  fit <- stats::lm(wellbeing_score ~ age + sex, data = d)
  tex <- .pt_latex(table_regression(
    fit,
    show_columns = c("b", "p", "partial_f2"),
    output = "tinytable"
  ))
  expect_match(tex, "\\ensuremath{f^{2}}", fixed = TRUE)
  expect_no_match(tex, f2, fixed = TRUE)
  # The formula of the hazard ratio of a cumulative cloglog model.
  skip_if_not_installed("MASS")
  set.seed(5)
  dd <- data.frame(x = stats::rnorm(300))
  lat <- 0.6 * dd$x + log(stats::rexp(300))
  dd$y <- cut(
    lat,
    c(-Inf, stats::quantile(lat, c(0.3, 0.6)), Inf),
    ordered_result = TRUE
  )
  fit <- MASS::polr(y ~ x, data = dd, method = "cloglog", Hess = TRUE)
  tex <- .pt_latex(table_regression(
    fit,
    exponentiate = TRUE,
    output = "tinytable"
  ))
  expect_match(
    tex,
    paste0(
      "exp(\\ensuremath{-}B): the cumulative parametrization cloglog ",
      "P(Y \\ensuremath{\\leq} j) = \\ensuremath{\\zeta}\\_j \\ensuremath{-} ",
      "x\\ensuremath{\\beta}"
    ),
    fixed = TRUE
  )
})

test_that("LaTeX merges a spanner across its columns, as HTML and Typst do", {
  skip_if_not_installed("tinytable")
  d <- .pt_data()
  fit <- stats::lm(wellbeing_score ~ age + sex, data = d)
  # "95% CI" over LL / UL: column 4, two wide.
  tex <- .pt_latex(table_regression(fit, output = "tinytable"))
  expect_match(tex, "cell{1}{4}={c=2}", fixed = TRUE)
  # Each group over its n and %.
  tex <- .pt_latex(.pt_mr())
  expect_match(tex, "cell{1}{2}={c=2}", fixed = TRUE)
  expect_match(tex, "cell{1}{4}={c=2}", fixed = TRUE)
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

# A level label long enough to wrap, in a table with a note (Typst
# measures its columns) and two header rows.
.pt_long_level <- function() {
  long <- "At least one parent holds a university degree"
  data.frame(
    parents = factor(
      rep(c(long, "None", long, "None"), c(14L, 6L, 10L, 10L)),
      levels = c(long, "None")
    ),
    grp = factor(rep(c("A", "B"), each = 20L))
  )
}

# The rows (Typst `y`) whose first cell has an indent in its style.
.pt_typst_indent_rows <- function(typ) {
  ln <- strsplit(typ, "\n", fixed = TRUE)[[1L]]
  from <- grep("tinytable cell style after", ln, fixed = TRUE) + 1L
  to <- from + which(!startsWith(ln[-seq_len(from - 1L)], "    ("))[1L] - 2L
  ids <- grep("indent:", ln[from:to], fixed = TRUE) - 1L
  dict <- ln[grep("tinytable style-dict after", ln, fixed = TRUE) + 1L]
  keys <- regmatches(dict, gregexpr("\"[0-9]+_0\": [0-9]+", dict))[[1L]]
  rows <- as.integer(sub("^\"([0-9]+)_0\".*$", "\\1", keys))
  sort(rows[as.integer(sub("^.*: ", "", keys)) %in% ids])
}

test_that("Typst indents a level row by its cell, not by spaces", {
  skip_if_not_installed("tinytable")
  d <- .pt_long_level()
  long <- levels(d$parents)[1L]
  nbsp4 <- strrep(intToUtf8(0x00A0), 4L)
  tt <- table_categorical(d, parents, by = grp, output = "tinytable")
  typ <- .pt_typst(tt)
  # A wrapped label started its next lines at the cell's indent, left of
  # the first, which four spaces had pushed right. The label now opens
  # the cell, and the indent is the style of the rows the console
  # indents, two header rows down, and of no other cell.
  lv <- as_structured(table_categorical(d, parents, by = grp))$body$.indent
  expect_match(typ, paste0("\n[", long, "], "), fixed = TRUE)
  expect_match(typ, "\n[None], ", fixed = TRUE)
  expect_no_match(typ, paste0("[", nbsp4), fixed = TRUE)
  expect_identical(.pt_typst_indent_rows(typ), which(lv > 0L) + 1L)
  expect_match(
    typ,
    "\n[Variable], table.cell(colspan: 2, align: center)[A], ",
    fixed = TRUE
  )
  expect_match(typ, "text(size: 0.9em, [#emph[Note.]", fixed = TRUE)
  # The natural widths are measured with the same indent.
  expect_match(typ, "get-style(spicy_x, spicy_k + 1)", fixed = TRUE)
  # The other outputs keep the spaces.
  for (out in list(.pt_latex(tt), .pt_html(tt), .pt_markdown(tt))) {
    expect_match(out, paste0(nbsp4, long), fixed = TRUE)
  }
  # A table with no indented row is measured as before.
  plain <- .pt_typst(table_continuous(
    data.frame(y = c(1, 4, 2, 8, 5, 7), g = rep(c("A", "B"), 3L)),
    y,
    by = g,
    output = "tinytable"
  ))
  expect_match(plain, "tinytable-coldata.map(", fixed = TRUE)
  expect_no_match(plain, "spicy_style", fixed = TRUE)
  expect_identical(spicy:::.spicy_tt_typst_indents("x", NULL), "x")
})

test_that("the Typst source of a table with a long level label", {
  skip_if_not_installed("tinytable")
  expect_snapshot(cat(.pt_typst(table_categorical(
    .pt_long_level(),
    parents,
    by = grp,
    output = "tinytable"
  ))))
})


# ---- group rows -----------------------------------------------------------

# A variable label wider than its two short levels, by two groups, in a
# table with a note (Typst measures its columns).
.pt_long_group <- function() {
  data.frame(
    parents = factor(
      rep(
        c("At least one", "None", "At least one", "None"),
        c(14L, 6L, 10L, 10L)
      ),
      levels = c("At least one", "None")
    ),
    grp = factor(rep(c("A", "B"), each = 20L))
  )
}
.pt_group_label <- "Parents holding a university degree"

test_that("Typst spans a variable's label over the columns of its groups", {
  skip_if_not_installed("tinytable")
  long <- .pt_group_label
  d <- .pt_long_group()
  d$none <- NA_character_
  tt <- table_categorical(
    d,
    c(parents, none),
    by = grp,
    labels = c(parents = long),
    output = "tinytable"
  )
  typ <- .pt_typst(tt)
  # The label was a cell of the first column, which wrapped it at the
  # width of the levels. It spans the stub and the n and % of A, B and
  # Total, seven columns; the p and the Phi keep their own. A variable
  # with no data carries the mark of each group on its row, which keeps
  # its cells.
  expect_match(
    typ,
    paste0(
      "\ntable\\.cell\\(colspan: 7\\)\\[",
      long,
      "\\], \\[[^]]*\\.197\\], \\[[^]]*\\.20\\],\n"
    )
  )
  expect_match(typ, "\n[none], [", fixed = TRUE)
  expect_length(gregexpr("table.cell(colspan: 7)", typ, fixed = TRUE)[[1L]], 1L)
  # The natural widths leave the label out of the first column, before
  # they are measured, and nothing gives it room: it sizes no column.
  blank <- "if (spicy_x, spicy_k) in ((0, 1),) { [] } else { spicy_cell }"
  expect_match(typ, blank, fixed = TRUE)
  expect_lt(
    as.integer(regexpr(blank, typ, fixed = TRUE)),
    as.integer(regexpr("let tinytable-naturals", typ, fixed = TRUE))
  )
  # The levels keep their indent; the spanning label carries none.
  lv <- as_structured(table_categorical(d, c(parents, none), by = grp))
  expect_identical(.pt_typst_indent_rows(typ), which(lv$body$.indent > 0L) + 1L)
  # The other outputs keep their cells, and a one-way table its rows.
  expect_no_match(.pt_html(tt), "colspan='7'", fixed = TRUE)
  expect_no_match(.pt_latex(tt), "c=7", fixed = TRUE)
  expect_no_match(
    .pt_typst(table_categorical(d, parents, output = "tinytable")),
    "table.cell(colspan",
    fixed = TRUE
  )
  # table_outcome() spans the columns of the outcome's statistics, the
  # stub to n, eight columns; its p keeps its own.
  o <- data.frame(
    y = rep(c(48, 52, 50, 55, 45), 8L),
    g = factor(rep(c("Yes", "No"), 20L))
  )
  expect_match(
    .pt_typst(table_outcome(
      o,
      y,
      select = g,
      labels = c(g = long),
      output = "tinytable"
    )),
    paste0("\ntable\\.cell\\(colspan: 8\\)\\[", long, "\\], \\[[^]]*\\],\n")
  )
  # Strings without the measure, tables without a span: left as they are.
  expect_identical(spicy:::.spicy_tt_typst_colspans("x", NULL), "x")
  expect_null(spicy:::.spicy_tt_colspan_cells(NULL))
  expect_null(spicy:::.spicy_tt_colspan_cells(
    data.frame(i = c(0, NA), j = 1, colspan = 2)
  ))
  bare <- tinytable::tt(data.frame(a = c("x", "y"), b = c("1", "")))
  expect_identical(spicy:::.spicy_tt_span_label(bare, 1L, 2L), bare)
  expect_identical(spicy:::.spicy_tt_span_label(bare, 2L, 1L), bare)
})

test_that("the design table spans its labels like the plain one", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("survey")
  long <- .pt_group_label
  d <- .pt_long_group()
  d$w <- rep(c(1, 2), 20L)
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  typ <- function(...) {
    .pt_typst(table_categorical_svy(
      des,
      parents,
      labels = c(parents = long),
      output = "tinytable",
      ...
    ))
  }
  expect_match(
    typ(by = grp),
    paste0("\ntable\\.cell\\(colspan: 7\\)\\[", long, "\\], \\[[^]]*\\],\n")
  )
  # Without `by`, the row carries no statistic of a crossing: as before.
  expect_no_match(typ(), "table.cell(colspan", fixed = TRUE)
})

test_that("the Typst source of a table whose variable label spans its groups", {
  skip_if_not_installed("tinytable")
  expect_snapshot(cat(.pt_typst(table_categorical(
    .pt_long_group(),
    parents,
    by = grp,
    labels = c(parents = .pt_group_label),
    output = "tinytable"
  ))))
})


# ---- the words of a header and of a note ----------------------------------

test_that("the Typst source of a difference column, its order in the note", {
  skip_if_not_installed("tinytable")
  set.seed(42)
  d <- data.frame(
    g = factor(rep(c("A", "B"), each = 10)),
    y = round(stats::rnorm(20, 50, 10))
  )
  expect_snapshot(cat(.pt_typst(table_continuous_lm(
    d,
    y,
    by = g,
    ci = FALSE,
    r2 = "none",
    show_n = FALSE,
    output = "tinytable"
  ))))
})

# The minimal example of the decked heads of `table_continuous_lm()`: two
# groups labelled with 12 and 14 characters, two outcomes with 30.
.pt_decked_labels <- c(
  wellbeing = "Well-being score (WHO-5 index)",
  stress = "Perceived stress, PSS-10 score"
)
.pt_decked <- function(...) {
  set.seed(20261024)
  smoking <- factor(
    rep(c("Never smoker", "Current smoker"), each = 40L),
    levels = c("Never smoker", "Current smoker")
  )
  d <- data.frame(
    smoking = smoking,
    wellbeing = round(
      60 + 5 * (smoking == "Current smoker") + stats::rnorm(80L, 0, 12),
      1
    ),
    stress = round(
      18 - 2 * (smoking == "Current smoker") + stats::rnorm(80L, 0, 6),
      1
    )
  )
  table_continuous_lm(
    d,
    select = c(wellbeing, stress),
    by = smoking,
    labels = .pt_decked_labels,
    ...
  )
}

test_that("the Typst source of decked heads, M over the groups and the difference alone", {
  skip_if_not_installed("tinytable")
  expect_snapshot(cat(.pt_typst(.pt_decked(output = "tinytable"))))
})

test_that("the Typst source of an association measure headed by its symbol", {
  skip_if_not_installed("tinytable")
  set.seed(5)
  d <- data.frame(
    x = factor(sample(c("lo", "mid", "hi"), 40L, TRUE)),
    g = factor(rep(c("A", "B"), 20L))
  )
  expect_snapshot(cat(.pt_typst(table_categorical(
    d,
    x,
    by = g,
    assoc_measure = "cramer_v",
    output = "tinytable"
  ))))
})

test_that("the Typst source of the sample size, the base and the Total rule", {
  # The note opens on N and the group sizes, then the direction of the
  # percentages; the `(Missing)` row, outside the base, leaves its
  # percentage cells blank, and a thin rule sits above the Total row.
  skip_if_not_installed("tinytable")
  set.seed(6)
  d <- data.frame(
    x = factor(sample(c("no", "yes"), 30L, TRUE)),
    g = factor(rep(c("A", "B"), 15L))
  )
  d$x[c(2L, 9L, 20L)] <- NA
  typ <- .pt_typst(table_categorical(
    d,
    x,
    by = g,
    p_value = FALSE,
    assoc_measure = "none",
    total_row = TRUE,
    output = "tinytable"
  ))
  expect_match(
    typ,
    "table.hline(y: 6, start: 0, end: 7, stroke: 0.03em)",
    fixed = TRUE
  )
  expect_snapshot(cat(typ))
})

test_that("the Typst source of a note naming the test of its one p-value", {
  skip_if_not_installed("tinytable")
  set.seed(9)
  d <- data.frame(
    y = round(stats::rnorm(24, 50, 10)),
    g = factor(rep(c("A", "B"), 12L))
  )
  expect_snapshot(cat(.pt_typst(table_continuous(
    d,
    y,
    by = g,
    show_columns = c("m", "sd", "n"),
    output = "tinytable"
  ))))
})

test_that("the Typst source of a note listing a missing `by` last", {
  skip_if_not_installed("tinytable")
  set.seed(42)
  d <- data.frame(
    y = round(stats::rnorm(20, 50, 10)),
    g = factor(rep(c("A", "B"), each = 10))
  )
  d$y[1:3] <- NA
  d$g[c(4, 15)] <- NA
  expect_snapshot(cat(.pt_typst(table_continuous(
    d,
    y,
    by = g,
    show_columns = c("m", "sd", "n"),
    p_value = FALSE,
    output = "tinytable"
  ))))
})


# ---- the sentences a call adds to the note, and the labels of the note ---

.pt_note_add_fit <- function() {
  set.seed(15)
  d <- data.frame(x = stats::rnorm(50), g = factor(rep(c("a", "b"), 25L)))
  d$y <- 0.5 * d$x + stats::rnorm(50)
  stats::lm(y ~ x + g, data = d)
}

test_that("the sentences of `note_add` come before the stars legend in Typst and Word", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  fit <- .pt_note_add_fit()
  add <- "Data: simulated for this example."
  lines <- strsplit(
    attr(table_regression(fit, stars = TRUE, note_add = add), "note"),
    "\n",
    fixed = TRUE
  )[[1L]]
  expect_length(lines, 2L)
  expect_true(endsWith(lines[[1L]], paste0("interval. ", add)))
  expect_true(startsWith(lines[[2L]], "*** p"))
  typ <- .pt_typst(table_regression(
    fit,
    stars = TRUE,
    note_add = add,
    output = "tinytable"
  ))
  # Typst escapes the stars of the legend.
  at_add <- regexpr(add, typ, fixed = TRUE)
  at_stars <- regexpr("\\*\\*\\* p", typ, fixed = TRUE)
  expect_true(at_add > 0L && at_stars > at_add)
  docx <- withr::local_tempfile(fileext = ".docx")
  table_regression(
    fit,
    stars = TRUE,
    note_add = add,
    output = "word",
    word_path = docx
  )
  word <- officer::docx_summary(officer::read_docx(docx))$text
  at_add <- grep(add, word, fixed = TRUE)
  at_stars <- grep("*** p", word, fixed = TRUE)
  expect_length(at_add, 1L)
  expect_true(endsWith(word[[at_add]], add))
  expect_identical(at_stars, at_add + 1L)
})

test_that("the Typst source of a note with a sentence added before the stars legend", {
  skip_if_not_installed("tinytable")
  expect_snapshot(cat(.pt_typst(table_regression(
    .pt_note_add_fit(),
    stars = TRUE,
    note_add = "Data: simulated for this example.",
    output = "tinytable"
  ))))
})

test_that("the Typst source of a note naming a variable by its note label", {
  skip_if_not_installed("tinytable")
  set.seed(4)
  d <- data.frame(
    bmi = round(stats::rnorm(20, 24, 4), 1),
    sex = factor(rep(c("Female", "Male"), 10L))
  )
  d$bmi[1:2] <- NA
  d$sex[3] <- NA
  expect_snapshot(cat(.pt_typst(table_continuous(
    d,
    bmi,
    by = sex,
    labels = c(
      bmi = "Body mass index computed from self-reported height and weight",
      sex = "Which sex were you assigned at birth?"
    ),
    note_labels = c(bmi = "BMI", sex = "sex at birth"),
    show_columns = c("m", "sd", "n"),
    p_value = FALSE,
    output = "tinytable"
  ))))
})


# ---- decimal places per variable ------------------------------------------

test_that("the Typst source of a table with decimal places per variable", {
  skip_if_not_installed("tinytable")
  set.seed(23)
  d <- data.frame(
    x = round(stats::rnorm(20, 170, 8)),
    y = stats::rnorm(20, 23, 3),
    g = rep(c("A", "B"), 10L)
  )
  expect_snapshot(cat(.pt_typst(table_continuous(
    d,
    select = c(x, y),
    by = g,
    digits = c(x = 0, y = 2),
    output = "tinytable"
  ))))
})

# ---- the minus sign of negative numbers ----------------------------------

test_that("a negative number takes the minus sign, every other hyphen stays", {
  cp <- function(...) intToUtf8(c(...))
  rule <- spicy:::.spicy_minus_sign
  esc <- spicy:::.spicy_typst_escape
  signed <- c(
    "-0.8",
    "[-0.8, 0.3]",
    "(-1.2 to 0.4)",
    # A difference padded for the decimal alignment, an interval under
    # the Lancet's en dash and under a comma mark, no leading zero.
    paste0(cp(0x2007), "-0.08"),
    paste0("[-5.17", cp(0x2013), "-2.58]"),
    "[-0,61; -0,10]",
    "-.45"
  )
  kept <- c("COVID-19", "1-5", "2026-10-24", "1e-05", "Non-smoker", "Q3 - Q1")
  minus <- gsub("-", cp(0x2212), signed, fixed = TRUE)
  # LaTeX gets the cells as typed, Typst escaped (`\-`).
  expect_identical(rule(c(signed, kept), "latex"), c(minus, kept))
  expect_identical(rule(esc(c(signed, kept)), "typst"), esc(c(minus, kept)))
  # A discretionary hyphen of LaTeX is not a sign.
  expect_identical(rule("x \\-5", "latex"), "x \\-5")
  # Before a mark other than the point and the comma, the sign is read
  # with the table's own mark.
  dot <- paste0("-", cp(0x00B7), "25")
  expect_identical(rule(dot, "latex"), dot)
  expect_identical(
    rule(dot, "latex", decimal_mark = cp(0x00B7)),
    paste0(cp(0x2212), cp(0x00B7), "25")
  )
})

# A difference and a lower bound below zero, an upper bound above it.
.pt_negative <- function(...) {
  set.seed(3)
  d <- data.frame(
    g = factor(rep(c("A", "B"), each = 20L)),
    y = c(stats::rnorm(20L, 5, 1), stats::rnorm(20L, 4.7, 1))
  )
  table_continuous_lm(d, select = y, by = g, ...)
}

# The lines of a Typst table between two of tinytable's markers.
.pt_typst_lines <- function(typ, from, to) {
  ln <- strsplit(typ, "\n", fixed = TRUE)[[1L]]
  a <- grep(from, ln, fixed = TRUE)[1L]
  b <- grep(to, ln, fixed = TRUE)[1L]
  ln[seq.int(a + 1L, b - 1L)]
}

test_that("Typst and LaTeX print a negative statistic with the minus sign", {
  skip_if_not_installed("tinytable")
  minus <- intToUtf8(0x2212)
  tt <- .pt_negative(output = "tinytable")
  ascii <- .pt_negative(
    output = "tinytable",
    style = spicy_style(minus_sign = "ascii")
  )
  # The difference and the lower bound, in the table and in the natural
  # widths Typst measures its columns with: the same character in both.
  typ <- .pt_typst(tt)
  expect_match(
    typ,
    paste0("\n[y], [4.83], [4.75], [", minus, "0.08], [", minus, "0.61], "),
    fixed = TRUE
  )
  expect_match(typ, paste0("\n    ([], [", minus, "0.08]),"), fixed = TRUE)
  expect_match(typ, paste0("\n    ([LL], [", minus, "0.61]),"), fixed = TRUE)
  cells <- c(
    .pt_typst_lines(typ, "cell content after", "footer after"),
    .pt_typst_lines(typ, "tinytable-coldata = (", "#context layout(")
  )
  expect_false(any(grepl("-[0-9]", cells)))
  # Nothing else moves -- the headers, the note, the code around the
  # cells -- and `minus_sign = "ascii"` gives the table as it was.
  expect_identical(
    typ,
    gsub("[\\-", paste0("[", minus), .pt_typst(ascii), fixed = TRUE)
  )
  tex <- .pt_latex(tt)
  expect_match(
    tex,
    "\ny & 4.83 & 4.75 & \\ensuremath{-}0.08 & \\ensuremath{-}0.61 & 0.46 & ",
    fixed = TRUE
  )
  expect_identical(
    tex,
    gsub("& -", "& \\ensuremath{-}", .pt_latex(ascii), fixed = TRUE)
  )
  expect_match(.pt_latex(ascii), "& -0.08 & -0.61 &", fixed = TRUE)
  # The option carries the lever, as the argument does.
  withr::with_options(
    list(spicy.style = spicy_style(minus_sign = "ascii")),
    expect_identical(
      .pt_typst(.pt_negative(output = "tinytable")),
      .pt_typst(ascii)
    )
  )
  # The outputs whose text is copied or recomputed keep the hyphen of a
  # number; the subtraction sign of the header is not a number (point 2).
  for (out in list(.pt_html(tt), .pt_markdown(tt))) {
    expect_match(out, "-0.08", fixed = TRUE)
    expect_no_match(out, paste0(minus, "0."), fixed = TRUE)
  }
  console <- utils::capture.output(print(.pt_negative()))
  expect_true(any(grepl("-0.08", console, fixed = TRUE)))
  expect_false(any(grepl(paste0(minus, "0."), console, fixed = TRUE)))
  df <- .pt_negative(output = "data.frame")
  expect_true(any(unlist(Filter(is.numeric, df)) < 0))
  expect_false(any(grepl(minus, unlist(df), fixed = TRUE)))
})

test_that("Word and Excel keep the hyphen of a negative statistic", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  skip_if_not_installed("openxlsx2")
  minus <- intToUtf8(0x2212)
  docx <- withr::local_tempfile(fileext = ".docx")
  .pt_negative(output = "word", word_path = docx)
  word <- officer::docx_summary(officer::read_docx(docx))$text
  xlsx <- withr::local_tempfile(fileext = ".xlsx")
  .pt_negative(output = "excel", excel_path = xlsx)
  excel <- unlist(openxlsx2::wb_to_df(
    openxlsx2::wb_load(xlsx),
    col_names = FALSE
  ))
  for (text in list(word, excel)) {
    expect_true(all(c("-0.08", "-0.61") %in% text))
    expect_false(any(grepl(paste0(minus, "0."), text, fixed = TRUE)))
  }
})

test_that("the labels keep the hyphen the user typed", {
  skip_if_not_installed("tinytable")
  minus <- intToUtf8(0x2212)
  set.seed(3)
  d <- data.frame(
    g = factor(rep(c("-1", "1"), each = 20L)),
    y = stats::rnorm(40L, -5, 1)
  )
  # A level "-1" in the group column of a descriptive table and in the
  # stub of a regression table stays as typed; the statistics beside it
  # take the sign.
  typ <- .pt_typst(table_continuous(d, y, by = g, output = "tinytable"))
  expect_match(typ, paste0("\n[y], [\\-1], [", minus, "5."), fixed = TRUE)
  fit <- stats::lm(y ~ g, data = d)
  typ <- .pt_typst(table_regression(fit, output = "tinytable"))
  expect_match(typ, paste0("\n[(Intercept)], [", minus, "5."), fixed = TRUE)
  expect_match(typ, "\n[\\-1 (ref.)], ", fixed = TRUE)
})

test_that("the Typst source of a table with negative statistics", {
  skip_if_not_installed("tinytable")
  expect_snapshot(cat(.pt_typst(.pt_negative(output = "tinytable"))))
})


# ---- compiled documents (local only) --------------------------------------
#
# The sources above are the contract; a compiled document is the proof.
# Quarto renders the same tables to a PDF through LaTeX (its default
# engine, lualatex) and through Typst, and the PDF is read back: the font
# of every "Note." word, and the text of every note, which a `%` or a
# glyph missing from the font would cut short. Local only: they need the
# Quarto CLI, a LaTeX distribution with tabularray, and about a minute.

.pt_compile <- function(format, env = parent.frame()) {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("tinytable")
  skip_if_not_installed("quarto")
  skip_if_not_installed("pdftools")
  skip_if(is.null(quarto::quarto_path()), "the Quarto CLI is not installed")
  if (identical(format, "pdf")) {
    skip_if(!nzchar(Sys.which("lualatex")), "lualatex is not installed")
  }
  dir <- withr::local_tempdir(.local_envir = env)
  # The spicy under test: the sources under devtools::test(), the
  # installed copy under R CMD check.
  pkg <- normalizePath(find.package("spicy"), winslash = "/")
  load <- if (file.exists(file.path(pkg, "R", "tt_theme.R"))) {
    sprintf("pkgload::load_all('%s', quiet = TRUE)", pkg)
  } else {
    sprintf("library(spicy, lib.loc = '%s')", dirname(pkg))
  }
  # The lead-in each table opens its note with: the APA default, none
  # (`jama`), Chicago's. The fifth table is wider than the text block;
  # in the eighth, three group labels are wider than the two narrow
  # columns under each of them.
  calls <- c(
    "Note." = "table_multiresponse(d, select = c(info_doctor, info_pharmacist, info_internet), by = sex, output = 'tinytable')",
    "Note." = "table_continuous(d, c(bmi, life_sat_health), by = sex, output = 'tinytable')",
    "Note." = "table_regression(fit, stars = TRUE, output = 'tinytable')",
    "Note." = "withr::with_options(list(spicy.language = 'fr'), table_regression(fit, output = 'tinytable'))",
    "Note." = "table_categorical(d, c(sex, smoking, income_group), by = education, output = 'tinytable')",
    " " = "table_regression(fit, style = 'jama', output = 'tinytable')",
    "Note:" = "table_regression(fit, style = spicy_style(note_label = 'Note:'), output = 'tinytable')",
    "Note." = "table_categorical(small, c(sex, smoking), by = role, output = 'tinytable')"
  )
  chunk <- function(...) c("```{r}", ..., "```", "")
  qmd <- c(
    "---",
    "format:",
    if (identical(format, "pdf")) {
      c("  pdf:", "    latex-auto-install: false", "    latex-clean: false")
    } else {
      "  typst: default"
    },
    "execute:",
    "  echo: false",
    "---",
    "",
    chunk(
      "#| include: false",
      load,
      "d <- as.data.frame(sochealth)",
      "fit <- stats::lm(wellbeing_score ~ age + sex, data = d)",
      "small <- d[1:90, ]",
      "roles <- c('Employeurs', 'Participants', 'Etudiants')",
      "small$role <- factor(roles[as.integer(small$education)], levels = roles)",
      sprintf(
        "saveRDS(lapply(list(%s), function(x) unlist(x@notes)), 'notes.rds')",
        paste(calls, collapse = ", ")
      )
    ),
    unlist(lapply(calls, chunk))
  )
  writeLines(qmd, file.path(dir, "notes.qmd"), useBytes = TRUE)
  withr::local_envvar(QUARTO_R = R.home("bin"))
  withr::with_dir(dir, quarto::quarto_render("notes.qmd", quiet = TRUE))
  pdf <- file.path(dir, "notes.pdf")
  expect_true(file.exists(pdf))
  list(
    pdf = pdf,
    notes = readRDS(file.path(dir, "notes.rds")),
    leads = trimws(names(calls)),
    log = file.path(dir, "notes.log")
  )
}

# Every lead-in of the PDF is set in an italic font, one per table that
# has one, and every note is there in full, with no word broken in the
# middle and no line running past the text block.
.pt_expect_compiled_notes <- function(out) {
  words <- do.call(rbind, pdftools::pdf_data(out$pdf, font_info = TRUE))
  for (lead in c("Note.", "Note:")) {
    prefix <- words[words$text == lead, ]
    expect_identical(nrow(prefix), sum(out$leads == lead), info = lead)
    expect_true(all(grepl("Italic|Oblique", prefix$font_name)), info = lead)
  }
  # Typst marks the break of a hyphenated word with a soft hyphen.
  expect_false(any(endsWith(words$text, intToUtf8(0x00AD))))
  # Group labels wider than their columns: they ran into each other
  # ("EmployeursParticipants") until the columns made room for them.
  expect_true(all(c("Employeurs", "Participants", "Etudiants") %in% words$text))
  if (file.exists(out$log)) {
    log <- readLines(out$log, warn = FALSE)
    over <- regmatches(log, regexpr("Overfull \\\\hbox \\([0-9.]+pt", log))
    over <- as.numeric(sub("^.*\\(([0-9.]+)pt$", "\\1", over))
    expect_true(all(over < 1), info = paste(over, collapse = ", "))
  }
  # Text compared with every kind of space, hyphen and quote folded, the
  # ligatures unbound, and the math-italic chi of LaTeX, with its
  # superscript set as a plain 2, read as the letter it stands for.
  fold <- function(x) {
    cp <- function(...) intToUtf8(c(...), multiple = TRUE)
    map <- c("2", "'", "ff", "fi", "fl", "ffi", "ffl", cp(0x03C7))
    names(map) <- cp(
      0x00B2,
      0x2019,
      0xFB00,
      0xFB01,
      0xFB02,
      0xFB03,
      0xFB04,
      0x1D712
    )
    for (k in names(map)) {
      x <- gsub(k, map[[k]], x, fixed = TRUE)
    }
    blank <- paste(cp(0x00A0, 0x202F, 0x00AD), collapse = "")
    gsub(paste0("[[:space:]", blank, "-]"), "", x, perl = TRUE)
  }
  text <- fold(paste(pdftools::pdf_text(out$pdf), collapse = " "))
  for (n in out$notes) {
    expect_true(grepl(fold(n), text, fixed = TRUE), info = n)
  }
}

test_that("a PDF compiled through LaTeX sets every note prefix in italics", {
  .pt_expect_compiled_notes(.pt_compile("pdf"))
})

test_that("a PDF compiled through Typst sets every note prefix in italics", {
  .pt_expect_compiled_notes(.pt_compile("typst"))
})

test_that("a wrapped level label lines up under its first line in a PDF", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("tinytable")
  skip_if_not_installed("quarto")
  skip_if_not_installed("pdftools")
  skip_if(is.null(quarto::quarto_path()), "the Quarto CLI is not installed")
  d <- .pt_long_level()
  typ <- .pt_typst(table_categorical(
    d,
    parents,
    by = grp,
    output = "tinytable"
  ))
  words <- strsplit(levels(d$parents)[1L], " ", fixed = TRUE)[[1L]]
  # The x of the first word of each line of the long label, and of the
  # variable's own label above it, on a page `width` wide.
  starts <- function(width) {
    dir <- withr::local_tempdir()
    src <- file.path(dir, "t.typ")
    pdf <- file.path(dir, "t.pdf")
    page <- sprintf("#set page(width: %s, height: auto, margin: 0.5cm)", width)
    writeLines(c(page, typ), src, useBytes = TRUE)
    system2(
      quarto::quarto_path(),
      c("typst", "compile", shQuote(src), shQuote(pdf)),
      stdout = FALSE,
      stderr = FALSE
    )
    w <- pdftools::pdf_data(pdf)[[1L]]
    row <- w$y >= w$y[w$text == "At"] & w$y < w$y[w$text == "None"]
    lab <- w[row & w$text %in% words, ]
    list(lines = tapply(lab$x, lab$y, min), header = w$x[w$text == "parents"])
  }
  narrow <- starts("7cm")
  expect_gt(length(narrow$lines), 1L)
  expect_true(all(narrow$lines == narrow$lines[[1L]]))
  expect_gt(narrow$lines[[1L]], narrow$header)
  # Measured with its indent, the label fits a column wide enough for it.
  expect_length(starts("20cm")$lines, 1L)
})

test_that("a variable label spans the groups and sizes no column in a PDF", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("tinytable")
  skip_if_not_installed("quarto")
  skip_if_not_installed("pdftools")
  skip_if(is.null(quarto::quarto_path()), "the Quarto CLI is not installed")
  d <- .pt_long_group()
  # On a page `width` wide, the table set flush left: the lines of the
  # label and the x of every word of the first level row.
  layout <- function(label, width, ...) {
    typ <- .pt_typst(table_categorical(
      d,
      parents,
      by = grp,
      labels = c(parents = label),
      output = "tinytable",
      ...
    ))
    dir <- withr::local_tempdir()
    src <- file.path(dir, "t.typ")
    pdf <- file.path(dir, "t.pdf")
    page <- c(
      sprintf("#set page(width: %s, height: auto, margin: 0.5cm)", width),
      "#show figure: set align(left)"
    )
    writeLines(c(page, typ), src, useBytes = TRUE)
    system2(
      quarto::quarto_path(),
      c("typst", "compile", shQuote(src), shQuote(pdf)),
      stdout = FALSE,
      stderr = FALSE
    )
    w <- pdftools::pdf_data(pdf)[[1L]]
    heads <- w$y[w$text == "n"][1L]
    level <- w$y[w$text == "At"][1L]
    words <- strsplit(label, " ", fixed = TRUE)[[1L]]
    lab <- w[w$text %in% words & w$y > heads & w$y < level, ]
    list(lines = length(unique(lab$y)), level = w$x[w$y == level])
  }
  # tinytable measured the label as a cell of the first column, which
  # wrapped it there at 12cm. It spans the groups on one line, and every
  # word of the level row sits where it sits under a one-word label: the
  # first column keeps the width of the levels.
  long <- layout(.pt_group_label, "12cm")
  short <- layout("Parents", "12cm")
  expect_identical(long$lines, 1L)
  expect_identical(long$level, short$level)
  # Longer than all the columns of the groups, the label wraps inside
  # them, as a section head does: it widens none of them.
  wide <- "Parents holding a university or an applied sciences degree"
  long <- layout(wide, "16cm", include_total = FALSE)
  short <- layout("Parents", "16cm", include_total = FALSE)
  expect_gt(long$lines, 1L)
  expect_identical(long$level, short$level)
})

test_that("a PDF compiled through Typst prints the minus sign", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("tinytable")
  skip_if_not_installed("quarto")
  skip_if_not_installed("pdftools")
  skip_if(is.null(quarto::quarto_path()), "the Quarto CLI is not installed")
  dir <- withr::local_tempdir()
  src <- file.path(dir, "t.typ")
  pdf <- file.path(dir, "t.pdf")
  typ <- .pt_typst(.pt_negative(output = "tinytable"))
  writeLines(typ, src, useBytes = TRUE)
  system2(
    quarto::quarto_path(),
    c("typst", "compile", shQuote(src), shQuote(pdf)),
    stdout = FALSE,
    stderr = FALSE
  )
  words <- pdftools::pdf_data(pdf)[[1L]]$text
  minus <- intToUtf8(0x2212)
  expect_true(all(paste0(minus, c("0.08", "0.61")) %in% words))
  expect_false(any(grepl("^-[0-9]", words)))
})

test_that("decked heads give the stub the width the difference took, in a PDF", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("tinytable")
  skip_if_not_installed("quarto")
  skip_if_not_installed("pdftools")
  skip_if(is.null(quarto::quarto_path()), "the Quarto CLI is not installed")
  typ <- .pt_typst(.pt_decked(output = "tinytable"))
  # The words of the page, the table compiled on a text block `width`
  # centimeters wide.
  words_at <- function(width) {
    dir <- withr::local_tempdir()
    src <- file.path(dir, "t.typ")
    pdf <- file.path(dir, "t.pdf")
    page <- sprintf(
      "#set page(width: %scm, height: auto, margin: 0.5cm)",
      width + 1
    )
    writeLines(c(page, typ), src, useBytes = TRUE)
    system2(
      quarto::quarto_path(),
      c("typst", "compile", shQuote(src), shQuote(pdf)),
      stdout = FALSE,
      stderr = FALSE
    )
    pdftools::pdf_data(pdf)[[1L]]
  }
  # The lines of the label of the first outcome, in the stub.
  stub_lines <- function(w) {
    lab <- strsplit(.pt_decked_labels[["wellbeing"]], " ", fixed = TRUE)[[1L]]
    from <- w$y[w$text == "Well-being"][1L]
    to <- w$y[w$text == "Perceived"][1L]
    length(unique(w$y[w$text %in% lab & w$y >= from & w$y < to]))
  }
  # On a 16 cm block every number is a word of its own: the two bounds
  # and the p ran into one another ("2.3813.44.006") while the header of
  # the difference held 118 of the 454 points.
  w16 <- words_at(16)
  body <- w16[w16$y >= w16$y[w16$text == "Well-being"][1L], ]
  expect_false(any(grepl("[0-9][.][0-9]{2}[^ ]*[0-9][.][0-9]", body$text)))
  # The stub label holds on one line from a 17.3 cm block, where the
  # composite headers needed 23.4 cm.
  expect_identical(stub_lines(words_at(18)), 1L)
})
