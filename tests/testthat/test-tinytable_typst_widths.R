# Typst widths of the tinytable tables with a note, and the size of the
# note in every tinytable output (R/tt_theme.R).

.tw_typst <- function(tt) tinytable::save_tt(tt, output = "typst")
.tw_html <- function(tt) tinytable::save_tt(tt, output = "html")
.tw_latex <- function(tt) tinytable::save_tt(tt, output = "latex")

# Nine items on a common five-level scale, labels of 40 to 55
# characters, one population.
.tw_battery <- function(fontsize = NULL) {
  niv <- c(
    "À renforcer",
    "À maintenir tel quel",
    "À réduire",
    "À retirer",
    "Pas d'avis"
  )
  lab <- c(
    "Organiser la conciliation entre travail, études et vie privée",
    "Acquérir des méthodes de travail et d'apprentissage",
    "Se préparer à endosser le rôle d'étudiant·e",
    "Connaître HESAV, son fonctionnement et ses ressources",
    "Développer un regard critique sur sa propre pratique",
    "Se familiariser avec les savoirs théoriques infirmiers",
    "Améliorer l'expression écrite et la prise de parole",
    "Chercher de la documentation et citer ses sources",
    "Clarifier son projet professionnel et sa motivation"
  )
  cnt <- rbind(
    c(13, 8, 0, 0, 2),
    c(11, 10, 1, 0, 1),
    c(10, 10, 0, 0, 3),
    c(10, 8, 2, 0, 3),
    c(10, 12, 0, 0, 1),
    c(9, 11, 1, 0, 2),
    c(8, 12, 2, 0, 1),
    c(6, 15, 1, 0, 1),
    c(4, 16, 2, 0, 1)
  )
  d <- as.data.frame(lapply(seq_len(nrow(cnt)), function(i) {
    factor(rep(niv, times = cnt[i, ]), levels = niv)
  }))
  names(d) <- paste0("item_", seq_along(lab))
  x <- table_battery(
    d,
    select = tidyselect::everything(),
    sort = "À renforcer-",
    percent_digits = 0,
    labels = stats::setNames(lab, names(d)),
    title = FALSE,
    note = paste0(
      "Pourcentages en ligne : chaque ligne totalise ",
      "100 % aux arrondis près. n = nombre de réponses."
    ),
    output = "tinytable"
  )
  if (!is.null(fontsize)) {
    x <- tinytable::style_tt(x, fontsize = fontsize)
  }
  tinytable::style_tt(x, i = 0, alignv = "b")
}

# The same shape, invented data, with no-break spaces after "À" and in
# "tel quel": only "À maintenir tel quel" and "Pas d'avis" can break,
# each into two lines.
.tw_battery_nbsp <- function() {
  nb <- " "
  niv <- c(
    paste0("À", nb, "renforcer"),
    paste0("À", nb, "maintenir tel", nb, "quel"),
    paste0("À", nb, "réduire"),
    paste0("À", nb, "retirer"),
    "Pas d'avis"
  )
  lab <- c(
    "Planifier les semaines de stage avec l'équipe de soins",
    "Préparer les entretiens d'accueil des nouveaux patients",
    "Rédiger les transmissions écrites de fin de journée",
    "Organiser le matériel de la salle de soins partagée",
    "Coordonner les visites avec les médecins de garde",
    "Évaluer la douleur avec les échelles du service",
    "Suivre les protocoles d'hygiène des mains au chevet",
    "Accompagner les familles lors des visites du soir",
    "Transmettre les consignes au collègue de nuit"
  )
  cnt <- rbind(
    c(12, 10, 3, 2, 4),
    c(9, 14, 2, 1, 5),
    c(15, 9, 3, 1, 3),
    c(11, 11, 4, 2, 3),
    c(8, 15, 3, 2, 3),
    c(13, 9, 5, 1, 3),
    c(10, 12, 3, 3, 3),
    c(7, 16, 3, 1, 4),
    c(12, 13, 2, 1, 3)
  )
  d <- as.data.frame(lapply(seq_len(nrow(cnt)), function(i) {
    factor(rep(niv, times = cnt[i, ]), levels = niv)
  }))
  names(d) <- paste0("q", seq_along(lab))
  table_battery(
    d,
    select = tidyselect::everything(),
    percent_digits = 0,
    labels = stats::setNames(lab, names(d)),
    title = FALSE,
    note = "Pourcentages en ligne. n = nombre de réponses.",
    output = "tinytable"
  )
}

.tw_fit <- function() stats::lm(wellbeing_score ~ age, data = sochealth)

# The `style` slot of a tinytable when it is finalized, and its size.
.tw_finalized <- function(tt) {
  out <- new.env()
  tt <- tinytable::style_tt(tt, finalize = function(tbl) {
    out$style <- tbl@style
    out$nrow <- tbl@nrow
    out$ncol <- tbl@ncol
    tbl
  })
  tinytable::save_tt(tt, output = "typst")
  as.list(out)
}

.tw_scale <- function(tt) {
  f <- .tw_finalized(tt)
  spicy:::.spicy_tt_note_scale(f$style, f$nrow, f$ncol)
}

# ---- Typst sources ----------------------------------------------------------

test_that("the Typst source of a battery, with and without a cell size", {
  skip_if_not_installed("tinytable")
  expect_snapshot(cat(.tw_typst(.tw_battery())))
  expect_snapshot(cat(.tw_typst(.tw_battery(fontsize = 0.75))))
})

test_that("the Typst source of a categorical table by a group", {
  skip_if_not_installed("tinytable")
  expect_snapshot(cat(.tw_typst(table_categorical(
    sochealth,
    select = c(smoking, education),
    by = sex,
    output = "tinytable"
  ))))
})

test_that("the Typst source of a multiple-response table", {
  skip_if_not_installed("tinytable")
  expect_snapshot(cat(.tw_typst(table_multiresponse(
    sochealth,
    select = starts_with("info_"),
    output = "tinytable"
  ))))
})

test_that("the Typst source of two models with their CI", {
  skip_if_not_installed("tinytable")
  m <- list(
    "Model A" = stats::lm(
      wellbeing_score ~ age + sex + education,
      data = sochealth
    ),
    "Model B" = stats::lm(bmi ~ age + sex + education, data = sochealth)
  )
  # `education` is an ordered factor: the message on its contrasts is
  # shown once per session, so it depends on the files run before this one.
  typ <- suppressMessages(.tw_typst(table_regression(
    m,
    show_columns = c("b", "ci", "p"),
    output = "tinytable"
  )))
  expect_snapshot(cat(typ))
})

test_that("the Typst source of a continuous table by a group (two label columns)", {
  skip_if_not_installed("tinytable")
  typ <- .tw_typst(table_continuous(
    sochealth,
    select = c(age, wellbeing_score),
    by = employment_status,
    output = "tinytable"
  ))
  expect_match(
    typ,
    "let spicy_stub = if 2 > 0 and 2 < spicy_n { 2 } else { 0 }",
    fixed = TRUE
  )
  expect_snapshot(cat(typ))
})

# ---- the layout functions ---------------------------------------------------

test_that("the layout leaves a string off tinytable's template unchanged", {
  groups <- data.frame(a = "A", b = "")
  expect_identical(spicy:::.spicy_tt_typst_indent_rule("x"), "x")
  expect_identical(spicy:::.spicy_tt_typst_measure("x", groups), "x")
  expect_identical(spicy:::.spicy_tt_typst_spans("x", groups), "x")
  expect_identical(spicy:::.spicy_tt_typst_shrink("x"), "x")
  expect_identical(spicy:::.spicy_tt_typst_layout("#table()"), "#table()")
  # The labels and the shrink need the measure.
  total <- spicy:::.spicy_tt_typst_total
  expect_identical(spicy:::.spicy_tt_typst_spans(total, groups), total)
  expect_identical(spicy:::.spicy_tt_typst_shrink(total), total)
  # No label to measure: no group, or only blank or broken labels.
  s <- paste0("    let spicy_blank = 0pt\n", total)
  expect_identical(spicy:::.spicy_tt_typst_spans(s, NULL), s)
  expect_identical(spicy:::.spicy_tt_typst_spans(s, groups[0, ]), s)
  expect_identical(
    spicy:::.spicy_tt_typst_spans(s, data.frame(a = NA, b = " ", c = "x\ny")),
    s
  )
})

test_that("the measure places each column under the rows of groups", {
  s <- spicy:::.spicy_tt_typst_naturals
  expect_match(
    spicy:::.spicy_tt_typst_measure(s),
    "y: spicy_k + 0, spicy_cell",
    fixed = TRUE
  )
  expect_match(
    spicy:::.spicy_tt_typst_measure(s, data.frame(a = 1:2)),
    "y: spicy_k + 2, spicy_cell",
    fixed = TRUE
  )
  # Each label at its header row, its first column, its span.
  m <- spicy:::.spicy_tt_typst_measure(
    paste0(s, "\n", spicy:::.spicy_tt_typst_total),
    NULL
  )
  groups <- data.frame(
    a = c(NA, "Top"),
    b = c("A", ""),
    c = c("", NA),
    d = c("B", "")
  )
  expect_match(
    spicy:::.spicy_tt_typst_spans(m, groups),
    "let spicy_spans = ((1, 2, 0, [A]), (3, 3, 0, [B]), (0, 1, 1, [Top]),)",
    fixed = TRUE
  )
})

test_that("the shrink reads the label columns, and the labels when there are some", {
  base <- spicy:::.spicy_tt_typst_measure(
    paste0(
      spicy:::.spicy_tt_typst_naturals,
      "\n",
      spicy:::.spicy_tt_typst_total
    )
  )
  stub <- function(x) {
    regmatches(x, regexpr("let spicy_stub = [^\n]*", x))
  }
  expect_identical(
    stub(spicy:::.spicy_tt_typst_shrink(base, 0L)),
    "let spicy_stub = if 0 > 0 and 0 < spicy_n { 0 } else { 0 }"
  )
  for (bad in list(NA, "a", -1L, 1:2)) {
    expect_identical(
      stub(spicy:::.spicy_tt_typst_shrink(base, bad)),
      "let spicy_stub = if 1 > 0 and 1 < spicy_n { 1 } else { 0 }"
    )
  }
  # Three lines for the labels before a header gives a line.
  out <- spicy:::.spicy_tt_typst_shrink(base)
  expect_match(
    out,
    "if spicy_room - spicy_nat_d >= spicy_three_s {",
    fixed = TRUE
  )
  # The data give up to `two` for labels at `three`, and below `two` with
  # the labels between `three` and `two` when the labels cannot have `two`.
  expect_match(
    out,
    "} else if spicy_room - spicy_two_d >= spicy_three_s {",
    fixed = TRUE
  )
  expect_match(
    out,
    "} else if spicy_room - spicy_floor_d >= spicy_three_s {",
    fixed = TRUE
  )
  expect_no_match(out, "// tier", fixed = TRUE)
  # The floors are raised by the column-group labels only when there are.
  expect_no_match(out, "spicy_span_width", fixed = TRUE)
  with_spans <- spicy:::.spicy_tt_typst_shrink(
    spicy:::.spicy_tt_typst_spans(base, data.frame(a = "A"))
  )
  expect_match(
    with_spans,
    "spicy_span_width(spicy_from, spicy_to, spicy_row, spicy_label, true)",
    fixed = TRUE
  )
})

test_that("after steps 2 to 4 the data columns keep only the width of their lines", {
  shrink <- function(groups = NULL) {
    spicy:::.spicy_tt_typst_shrink(spicy:::.spicy_tt_typst_measure(
      paste0(
        spicy:::.spicy_tt_typst_naturals,
        "\n",
        spicy:::.spicy_tt_typst_total
      ),
      groups
    ))
  }
  out <- shrink()
  # Only for steps 2 to 4: neither the natural widths nor the floors.
  expect_match(
    out,
    paste0(
      "if spicy_room - spicy_nat_d < spicy_three_s and ",
      "spicy_room - spicy_floor_d >= spicy_three_s {"
    ),
    fixed = TRUE
  )
  # Each column measured at its place, at a fixed width, under the rows
  # of groups.
  expect_match(
    out,
    "table.cell(x: spicy_k, y: spicy_i + 0, spicy_cell)))).height",
    fixed = TRUE
  )
  expect_match(
    shrink(data.frame(a = 1:2)),
    "table.cell(x: spicy_k, y: spicy_i + 2, spicy_cell)))).height",
    fixed = TRUE
  )
  # Down to the floor at most, to 0.5pt; the room freed to the labels, up
  # to their natural widths; what is left only where it removes a line.
  expect_match(
    out,
    "spicy_least(spicy_k, spicy_floor.at(spicy_k), spicy_w.at(spicy_k),",
    fixed = TRUE
  )
  expect_match(out, "while spicy_hi - spicy_lo > 0.5pt {", fixed = TRUE)
  expect_match(
    out,
    "spicy_w = spicy_fit(spicy_s, spicy_room - spicy_sum(spicy_d, spicy_w), spicy_w, spicy_nat)",
    fixed = TRUE
  )
  expect_match(out, "spicy_height(spicy_k, spicy_w) - 1pt", fixed = TRUE)
  expect_match(out, "tinytable-naturals = spicy_w\n", fixed = TRUE)
})

test_that("a table without a note keeps tinytable's Typst", {
  skip_if_not_installed("tinytable")
  typ <- .tw_typst(table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    p_value = FALSE,
    assoc_measure = "none",
    note = FALSE,
    output = "tinytable"
  ))
  expect_match(typ, "columns: (auto,", fixed = TRUE)
  expect_no_match(typ, "spicy_", fixed = TRUE)
  expect_no_match(typ, "it.body != []", fixed = TRUE)
})

# ---- the size of the note ---------------------------------------------------

test_that(".spicy_tt_note_scale() reads the size every body cell shares", {
  skip_if_not_installed("tinytable")
  x <- table_regression(.tw_fit(), output = "tinytable")
  n <- x@nrow
  expect_identical(.tw_scale(x), 1)
  expect_identical(.tw_scale(tinytable::style_tt(x, fontsize = 0.8)), 0.8)
  expect_identical(
    .tw_scale(tinytable::style_tt(x, i = seq_len(n), fontsize = 0.8)),
    0.8
  )
  # The last size set on a cell.
  expect_identical(
    .tw_scale(tinytable::style_tt(
      tinytable::style_tt(x, fontsize = 0.8),
      fontsize = 0.7
    )),
    0.7
  )
  # Some cells only, or sizes that differ: no common size.
  expect_identical(
    .tw_scale(tinytable::style_tt(x, i = 1, fontsize = 0.8)),
    1
  )
  expect_identical(.tw_scale(tinytable::style_tt(x, j = 2, fontsize = 0.8)), 1)
  expect_identical(
    .tw_scale(tinytable::style_tt(
      tinytable::style_tt(x, fontsize = 0.8),
      j = 2,
      fontsize = 0.7
    )),
    1
  )
  # The header and the note are not body cells.
  expect_identical(.tw_scale(tinytable::style_tt(x, i = 0, fontsize = 0.8)), 1)
  expect_identical(
    .tw_scale(tinytable::style_tt(x, i = "notes", fontsize = 1.4)),
    1
  )
  # Styles that cannot be read.
  expect_identical(spicy:::.spicy_tt_note_scale(NULL, 2, 2), 1)
  expect_identical(spicy:::.spicy_tt_note_scale(data.frame(i = 1), 2, 2), 1)
  st <- data.frame(i = NA, j = NA, fontsize = 0.8)
  expect_identical(spicy:::.spicy_tt_note_scale(st, 2, 2), 0.8)
  expect_identical(spicy:::.spicy_tt_note_scale(st, 0, 2), 1)
  expect_identical(spicy:::.spicy_tt_note_scale(st, NA, 2), 1)
  expect_identical(spicy:::.spicy_tt_note_scale(st, 1:2, 2), 1)
  expect_identical(
    spicy:::.spicy_tt_note_scale(
      data.frame(i = NA, j = NA, fontsize = 0),
      2,
      2
    ),
    1
  )
  expect_identical(spicy:::.spicy_note_em(), "0.9")
  expect_identical(spicy:::.spicy_note_em(0.75), "0.675")
})

test_that("the note is set at 0.9 of the size the body cells share", {
  skip_if_not_installed("tinytable")
  x <- table_regression(.tw_fit(), output = "tinytable")
  sized <- tinytable::style_tt(x, fontsize = 0.8)
  # Typst
  expect_match(.tw_typst(x), "text(size: 0.9em, [#emph[Note.]", fixed = TRUE)
  expect_match(
    .tw_typst(sized),
    "text(size: 0.72em, [#emph[Note.]",
    fixed = TRUE
  )
  # HTML
  td <- "<td colspan='6' style=\"font-size: %sem; line-height: 1.25; \">"
  expect_match(.tw_html(x), sprintf(td, "0.9"), fixed = TRUE)
  expect_match(.tw_html(sized), sprintf(td, "0.72"), fixed = TRUE)
  # LaTeX, as tinytable writes the size of a cell
  expect_match(.tw_latex(x), "note{}={\\small \\emph{Note.}", fixed = TRUE)
  expect_match(
    .tw_latex(sized),
    "note{}={\\fontsize{0.72em}{1.02em}\\selectfont \\emph{Note.}",
    fixed = TRUE
  )
  # The paragraph ends inside the size group, so the note's lines take
  # its leading, not the document's.
  expect_match(.tw_latex(sized), "\\endgraf},", fixed = TRUE)
  expect_match(.tw_latex(x), "\\endgraf},", fixed = TRUE)
  # Only part of the body at that size: the note keeps 0.9em.
  part <- tinytable::style_tt(x, i = 1, fontsize = 0.8)
  expect_match(.tw_typst(part), "text(size: 0.9em, [", fixed = TRUE)
  expect_match(.tw_html(part), sprintf(td, "0.9"), fixed = TRUE)
  expect_match(.tw_latex(part), "note{}={\\small ", fixed = TRUE)
  # A note styled itself keeps its style in every output.
  styled <- tinytable::style_tt(sized, i = "notes", fontsize = 1.4)
  typ <- .tw_typst(styled)
  expect_match(typ, "size: 1.4em", fixed = TRUE)
  expect_no_match(typ, "0.72em", fixed = TRUE)
  html <- .tw_html(styled)
  expect_match(html, sprintf(td, "0.9"), fixed = TRUE)
  expect_no_match(html, "0.72em", fixed = TRUE)
  tex <- .tw_latex(styled)
  expect_match(tex, "\\fontsize{1.4em}{1.4em}\\selectfont Note.", fixed = TRUE)
  expect_no_match(tex, "0.72em", fixed = TRUE)
  # No intervention asked: nothing in any output.
  withr::local_options(spicy.note_style = "none")
  expect_match(.tw_typst(sized), "text([#emph[Note.]", fixed = TRUE)
  expect_no_match(.tw_html(sized), "0.72em", fixed = TRUE)
  expect_match(.tw_latex(sized), "note{}={\\emph{Note.}", fixed = TRUE)
  expect_no_match(.tw_latex(sized), "\\endgraf", fixed = TRUE)
})

test_that("the Typst note keeps its lead-in after a note of several lines", {
  s <- paste(
    "    table.cell(align: left, colspan: 3, text([Note. One",
    "  two])),",
    "    table.cell(align: left, colspan: 3, text([Note. Three])),",
    sep = "\n"
  )
  out <- spicy:::.spicy_tt_note_typst(
    s,
    c("Note. One\n  two", "Note. Three"),
    lead = "Note."
  )
  expect_match(out, "text(size: 0.9em, [#emph[Note.] One \\", fixed = TRUE)
  expect_match(out, "\n~~two]))", fixed = TRUE)
  expect_match(out, "text(size: 0.9em, [#emph[Note.] Three]))", fixed = TRUE)
})

# ---- a compiled document ----------------------------------------------------

test_that("the battery compiles in Typst with every header on one line", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("tinytable")
  skip_if_not_installed("pdftools")
  quarto <- Sys.which("quarto")
  skip_if(!nzchar(quarto), "the Quarto CLI is not installed")
  dir <- withr::local_tempdir()
  typ <- file.path(dir, "battery.typ")
  pdf <- file.path(dir, "battery.pdf")
  # Typst's own font: the table is wider than the text block, and the
  # labels can take three lines while the data keep their widths.
  writeLines(
    enc2utf8(c(
      "#set page(paper: \"a4\", margin: (x: 3.5cm, y: 2.5cm))",
      "#set text(size: 11pt, lang: \"fr\")",
      "",
      .tw_typst(.tw_battery(fontsize = 0.75))
    )),
    typ,
    useBytes = TRUE
  )
  out <- system2(
    quarto,
    c("typst", "compile", shQuote(typ), shQuote(pdf)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(out, "status"))
  expect_true(file.exists(pdf))
  words <- pdftools::pdf_data(pdf)[[1L]]
  top <- words$y[words$text == "Variable"]
  expect_length(top, 1L)
  for (w in c("renforcer", "maintenir", "tel", "quel", "réduire", "retirer")) {
    expect_identical(unique(words$y[words$text == w]), top, info = w)
  }
  expect_identical(unique(words$y[startsWith(words$text, "Pas")]), top)
})

test_that("a data column keeps only the width of its lines (body 9.5pt, text 379.8pt)", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("tinytable")
  skip_if_not_installed("pdftools")
  quarto <- Sys.which("quarto")
  skip_if(!nzchar(quarto), "the Quarto CLI is not installed")
  dir <- withr::local_tempdir()
  typ <- file.path(dir, "nbsp.typ")
  pdf <- file.path(dir, "nbsp.pdf")
  # The widths after the rule, printed above the table: only steps 2 to 4
  # reach this line.
  show <- function(x) {
    sprintf(
      "%s #%s.map(w => str(calc.round(w.pt(), digits: 3))).join(\" \") ",
      toupper(x),
      x
    )
  }
  src <- sub(
    "tinytable-naturals = spicy_w\n",
    paste0(
      "tinytable-naturals = spicy_w\n[",
      show("spicy_w"),
      show("spicy_nat"),
      show("spicy_floor"),
      "END]\n"
    ),
    .tw_typst(.tw_battery_nbsp()),
    fixed = TRUE
  )
  # Typst's own font, the body at 9.5pt, a text block of 379.8pt.
  writeLines(
    enc2utf8(c(
      "#set page(width: 379.8pt + 4cm, height: auto, margin: 2cm)",
      "#set text(size: 11pt, lang: \"fr\")",
      "#show table: set text(9.5pt)",
      "",
      src
    )),
    typ,
    useBytes = TRUE
  )
  out <- system2(
    quarto,
    c("typst", "compile", shQuote(typ), shQuote(pdf)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(out, "status"))
  txt <- paste(pdftools::pdf_text(pdf), collapse = " ")
  read <- function(x) {
    m <- regmatches(txt, regexpr(paste0(x, "[0-9.[:space:]]+"), txt))
    as.numeric(strsplit(trimws(sub(x, "", m)), "[[:space:]]+")[[1L]])
  }
  w <- read("SPICY_W")
  nat <- read("SPICY_NAT")
  floor <- read("SPICY_FLOOR")
  expect_length(w, 7L)
  # The two headers that break keep their two lines at their floor, the
  # narrowest width they can have; the other columns keep their width.
  expect_true(all(abs(w[c(3, 6)] - floor[c(3, 6)]) <= 0.5))
  expect_equal(w[c(2, 4, 5, 7)], nat[c(2, 4, 5, 7)])
  # The labels take the room freed, within the text block.
  expect_lt(w[[1L]], nat[[1L]])
  expect_lte(abs(sum(w) - 379.8), 0.05)
  words <- pdftools::pdf_data(pdf)[[1L]]
  top <- words$y[words$text == "Variable"]
  expect_length(top, 1L)
  for (h in c("renforcer", "maintenir", "réduire", "retirer", "Pas")) {
    expect_identical(unique(words$y[words$text == h]), top, info = h)
  }
  expect_gt(unique(words$y[words$text == "tel"]), top)
  expect_gt(unique(words$y[endsWith(words$text, "avis")]), top)
})
