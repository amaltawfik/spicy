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
  # as no-break spaces. Named models: the line cites the spanner label.
  expect_match(html, "<br>&nbsp;&nbsp;OLS:", fixed = TRUE)
  # Typst collapses a bare newline exactly as HTML does: the line
  # ends with a forced break, the indent becomes no-break spaces.
  expect_match(.pt_typst(tt), "Std. errors: \\\n~~OLS:", fixed = TRUE)
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
        tinytable::save_tt(tt, output = "markdown"),
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
  md <- tinytable::save_tt(.pt_multiline_note("tinytable"), output = "markdown")
  grid <- strsplit(md, "\n", fixed = TRUE)[[1L]]
  grid <- grid[nzchar(grid) & !startsWith(grid, "Table:")]
  # Every line opens and closes on the border, the note's lines too: a
  # newline written into the grid broke it, and pandoc dropped the note
  # from a Word document.
  expect_true(all(grepl("^[|+].*[|+]$", trimws(grid, "right"))))
  expect_length(unique(nchar(trimws(grid, "right"))), 1L)
  expect_match(md, "| *Note.* OLS: linear regression;", fixed = TRUE)
})

test_that("a note that does not open with the prefix is left as typed", {
  skip_if_not_installed("tinytable")
  fit <- stats::lm(wellbeing_score ~ age, data = .pt_data())
  tt <- table_regression(fit, note = "Source: a survey.", output = "tinytable")
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
  expect_match(
    tinytable::save_tt(tt, output = "markdown"),
    "| Source: a survey.",
    fixed = TRUE
  )
  expect_no_match(.pt_typst(tt), "#emph[", fixed = TRUE)
  expect_no_match(.pt_latex(tt), "\\emph{", fixed = TRUE)
  expect_no_match(.pt_html(tt), "<em>", fixed = TRUE)
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

test_that("the LaTeX note keeps one disclosure per line, as HTML and Typst", {
  skip_if_not_installed("tinytable")
  tex <- .pt_latex(.pt_multiline_note("tinytable"))
  expect_match(tex, "\\\\\nStd. errors:\\\\\n~~OLS:", fixed = TRUE)
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
  expect_identical(
    map(cp(c(0x00E9, 0x2013, 0x00A0))),
    cp(c(0x00E9, 0x2013, 0x00A0))
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
  calls <- c(
    "table_multiresponse(d, select = c(info_doctor, info_pharmacist, info_internet), by = sex, output = 'tinytable')",
    "table_continuous(d, c(bmi, life_sat_health), by = sex, output = 'tinytable')",
    "table_regression(fit, output = 'tinytable')",
    "withr::with_options(list(spicy.language = 'fr'), table_regression(fit, output = 'tinytable'))"
  )
  chunk <- function(...) c("```{r}", ..., "```", "")
  qmd <- c(
    "---",
    "format:",
    if (identical(format, "pdf")) {
      c("  pdf:", "    latex-auto-install: false")
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
  list(pdf = pdf, notes = readRDS(file.path(dir, "notes.rds")))
}

# Every "Note." of the PDF is set in an italic font, one per table, and
# every note is there in full.
.pt_expect_compiled_notes <- function(out) {
  words <- do.call(rbind, pdftools::pdf_data(out$pdf, font_info = TRUE))
  prefix <- words[words$text == "Note.", ]
  expect_identical(nrow(prefix), length(out$notes))
  expect_true(all(grepl("Italic|Oblique", prefix$font_name)))
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
