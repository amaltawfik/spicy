# ---------------------------------------------------------------------------
# Rich outputs in non-HTML knit targets (Quarto / R Markdown -> Word).
# Bug reported from a real Quarto -> docx workflow (the stays project,
# 2026-07-08; dev/quarto_word_rendering_spec.md): knit_print for
# spicy_flextable / spicy_gt ALWAYS emitted raw HTML, which pandoc
# drops in docx -- the table silently disappeared. The methods are now
# format-aware: HTML targets keep the styled-note post-processing,
# every other target delegates to the engine's native rendering (the
# flextable already carries the note in its footer; gt gets it
# attached via tab_source_note at delegation time).
# ---------------------------------------------------------------------------

.with_pandoc_to <- function(to, code) {
  old <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  knitr::opts_knit$set(rmarkdown.pandoc.to = to)
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old), add = TRUE)
  force(code)
}

.fit_qw <- function() lm(mpg ~ wt + hp, data = mtcars)


test_that("docx target: spicy_flextable delegates to native openxml", {
  skip_if_not_installed("flextable")
  ft <- table_regression(.fit_qw(), output = "flextable")
  out <- .with_pandoc_to("docx", knitr::knit_print(ft))
  s <- paste(as.character(out), collapse = "")
  # flextable's own knit_print emits a raw openxml block -- the thing
  # pandoc actually embeds in a Word document.
  expect_match(s, "{=openxml}", fixed = TRUE)
  # And NOT the HTML-only styled note div (dropped by pandoc).
  expect_false(grepl("spicy-ft-note", s, fixed = TRUE))
})

test_that("docx target: spicy_gt carries the note and skips raw HTML", {
  skip_if_not_installed("gt")
  gtb <- table_regression(.fit_qw(), output = "gt")
  # The regression builder puts the note on the object itself, so it
  # reaches every gt verb, not only this knit path.
  expect_length(gtb[["_source_notes"]], 1L)
  out <- .with_pandoc_to("docx", knitr::knit_print(gtb))
  s <- paste(as.character(out), collapse = "")
  # The note -- HTML-injected as a div in HTML targets -- must reach
  # the Word rendering through gt's native source note, exactly once.
  expect_match(s, "Std. errors", fixed = TRUE)
  expect_length(gregexpr("Std. errors", s, fixed = TRUE)[[1L]], 1L)
  expect_false(grepl("spicy-gt-note", s, fixed = TRUE))
})

test_that("docx target: a descriptive gt gains the note at knit time", {
  skip_if_not_installed("gt")
  g <- table_continuous(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    output = "gt"
  )
  # The descriptive builders carry the note natively (a saved gt must
  # not lose it -- the review's F4); the knit path must therefore NOT
  # add a second copy. Once, on every pandoc target, is the contract.
  expect_length(g[["_source_notes"]], 1L)
  out <- .with_pandoc_to("docx", knitr::knit_print(g))
  s <- paste(as.character(out), collapse = "")
  expect_identical(
    length(gregexpr("Missing values removed", s, fixed = TRUE)[[1L]]),
    1L
  )
  s2 <- paste(
    as.character(.with_pandoc_to("html", knitr::knit_print(g))),
    collapse = ""
  )
  expect_identical(
    length(gregexpr("Missing values removed", s2, fixed = TRUE)[[1L]]),
    1L
  )
})

test_that("HTML target keeps the styled-note post-processing", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("gt")
  ft <- table_regression(.fit_qw(), output = "flextable")
  gtb <- table_regression(.fit_qw(), output = "gt")
  s_ft <- .with_pandoc_to(
    "html",
    paste(as.character(knitr::knit_print(ft)), collapse = "")
  )
  s_gt <- .with_pandoc_to(
    "html",
    paste(as.character(knitr::knit_print(gtb)), collapse = "")
  )
  expect_match(s_ft, "spicy-ft-note", fixed = TRUE)
  expect_match(s_gt, "spicy-gt-note", fixed = TRUE)
})

test_that("docx target: table_continuous_lm flextable renders too", {
  # The class tags are shared, so the shared knit_print fix must cover
  # the TCLM rich path as well.
  skip_if_not_installed("flextable")
  tcl <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = physical_activity,
    covariates = c(age, sex),
    output = "flextable"
  )
  out <- .with_pandoc_to("docx", knitr::knit_print(tcl))
  s <- paste(as.character(out), collapse = "")
  expect_match(s, "{=openxml}", fixed = TRUE)
})


test_that("as_flextable() returns the clean engine object, note intact", {
  skip_if_not_installed("flextable")
  ft <- table_regression(.fit_qw(), output = "flextable")
  clean <- flextable::as_flextable(ft)
  expect_s3_class(clean, "flextable")
  expect_false(inherits(clean, "spicy_flextable"))
  # The note lives in the flextable footer natively (added at build
  # time), so nothing is lost by untagging.
  foot_txt <- paste(
    unlist(clean$footer$content$data),
    collapse = " "
  )
  expect_match(foot_txt, "Std. errors", fixed = TRUE)
})


test_that("end-to-end: rmarkdown -> Word document contains the table", {
  # The decisive integration proof for the reported bug: knit a real
  # Rmd to docx and assert the table cells AND the note text are in
  # the document body.
  skip_on_cran()
  skip_if_not_installed("flextable")
  skip_if_not_installed("rmarkdown")
  skip_if(!rmarkdown::pandoc_available("2.0"), "pandoc >= 2.0 not available")

  td <- withr::local_tempdir()
  rmd <- file.path(td, "qw.Rmd")
  writeLines(
    c(
      "---",
      "output: word_document",
      "---",
      "",
      "```{r, echo=FALSE}",
      "fit <- lm(mpg ~ wt + hp, data = mtcars)",
      "spicy::table_regression(fit, output = \"flextable\")",
      "```"
    ),
    rmd
  )
  docx <- file.path(td, "qw.docx")
  suppressWarnings(suppressMessages(
    rmarkdown::render(
      rmd,
      output_file = docx,
      quiet = TRUE,
      envir = new.env(parent = globalenv())
    )
  ))
  expect_true(file.exists(docx))
  xml <- local({
    exdir <- file.path(td, "unz")
    utils::unzip(docx, files = "word/document.xml", exdir = exdir)
    readChar(file.path(exdir, "word", "document.xml"), 5e6, useBytes = TRUE)
  })
  # A REAL native Word table node, with or without attributes on the
  # opening tag (flextable's fragment carries xmlns declarations, so
  # matching the literal "<w:tbl>" alone would miss it -- the exact
  # measurement mistake behind the retracted pandoc#11772 report).
  expect_match(xml, "<w:tbl[ >]")
  # Table content present (predictor rows + a fitted value).
  expect_match(xml, ">wt<", fixed = TRUE)
  expect_match(xml, ">hp<", fixed = TRUE)
  # The note survived into the Word body via the native footer.
  expect_match(xml, "Std. errors", fixed = TRUE)
})


# Phase 3 matrix – vignettes-news:rich-outputs-word-ppt-pdf (lot T4)

test_that("pptx target: spicy_flextable delegates to native openxml", {
  skip_if_not_installed("flextable")
  ft <- table_regression(.fit_qw(), output = "flextable")
  out <- .with_pandoc_to("pptx", knitr::knit_print(ft))
  s <- paste(as.character(out), collapse = "")
  # flextable's PowerPoint path emits a raw openxml graphic frame --
  # what pandoc embeds in a pptx slide -- not the HTML fallback.
  expect_match(s, "{=openxml}", fixed = TRUE)
  expect_match(s, "graphicFrame", fixed = TRUE)
  expect_false(grepl("spicy-ft-note", s, fixed = TRUE))
})

test_that("latex target: spicy_flextable delegates to native LaTeX", {
  skip_if_not_installed("flextable")
  ft <- table_regression(.fit_qw(), output = "flextable")
  out <- suppressWarnings(.with_pandoc_to("latex", knitr::knit_print(ft)))
  s <- paste(as.character(out), collapse = "")
  expect_match(s, "{=latex}", fixed = TRUE)
  expect_false(grepl("spicy-ft-note", s, fixed = TRUE))
})

test_that("latex target: spicy_gt writes a LaTeX every engine compiles", {
  skip_if_not_installed("gt")
  gtb <- table_regression(.fit_qw(), output = "gt")
  out <- .with_pandoc_to("latex", knitr::knit_print(gtb))
  s <- paste(as.character(out), collapse = "")
  # The prefix in italics, through gt's own markdown rendering.
  expect_match(s, "\\emph{Note.}", fixed = TRUE)
  # The figure space of the decimal alignment, which pdflatex refused
  # and Latin Modern lacks, as a phantom digit.
  expect_match(s, "\\hphantom{0}", fixed = TRUE)
  expect_false(grepl(intToUtf8(0x2007), s, fixed = TRUE))
  # gt's LaTeX packages still travel with the table.
  expect_true(length(attr(out, "knit_meta")) > 0L)
})

test_that("latex target: spicy_gt titles at the table's size, apostrophes curl", {
  skip_if_not_installed("gt")
  gtb <- table_regression(
    .fit_qw(),
    note = "Note. The model's fit.",
    output = "gt"
  )
  s <- paste(
    as.character(.with_pandoc_to("latex", knitr::knit_print(gtb))),
    collapse = ""
  )
  # gt read the 100% of the HTML title against 16pt and set the title at
  # 16pt (20pt in the descriptive tables), over a table set at 12pt.
  expect_match(s, "\\caption*{\n{\\fontsize{12}{15}\\selectfont", fixed = TRUE)
  # The note goes through markdown, whose escaped apostrophe gt set as
  # the straight `\textquotesingle{}`; LaTeX's own `'` is typographic.
  expect_match(s, "The model's fit.", fixed = TRUE)
  expect_no_match(s, "textquotesingle", fixed = TRUE)
})

test_that("latex target: spicy_gt matches its HTML header and fits the line", {
  skip_if_not_installed("gt")
  skip_if_not_installed("rmarkdown")
  gtb <- table_regression(.fit_qw(), output = "gt")
  out <- .with_pandoc_to("latex", knitr::knit_print(gtb))
  s <- paste(as.character(out), collapse = "")
  # The rules under the one-column label spanners (Variable, B, SE, p),
  # which HTML hides, are gone; the one under "95% CI" stays.
  expect_no_match(s, "\\\\cmidrule\\(lr\\)\\{([0-9]+)-\\1\\}")
  expect_match(s, "\\cmidrule(lr){4-5}", fixed = TRUE)
  # The title flush left, as in HTML, through the caption package gt
  # loads itself.
  expect_match(
    s,
    "\\captionsetup{justification=raggedright,singlelinecheck=false}",
    fixed = TRUE
  )
  pkgs <- vapply(attr(out, "knit_meta"), function(m) m$name, character(1))
  expect_true(all(c("caption", "graphicx") %in% pkgs))
  # Measured at its natural width, scaled down to the line only when
  # wider, set as gt wrote it otherwise.
  expect_match(s, "\\setbox0\\hbox{\\begin{tabular}{@{}lccccc}", fixed = TRUE)
  expect_match(
    s,
    "\\ifdim\\wd0>\\linewidth\\noindent\\resizebox{\\linewidth}{!}{\\box0}",
    fixed = TRUE
  )
  expect_match(
    s,
    "\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}lccccc}",
    fixed = TRUE
  )
  # A model spanner keeps its rule even over one column: the rows above
  # the lowest spanner row are left as gt wrote them.
  two <- table_regression(
    list(A = .fit_qw(), B = .fit_qw()),
    show_columns = "b",
    output = "gt"
  )
  s2 <- paste(
    as.character(.with_pandoc_to("latex", knitr::knit_print(two))),
    collapse = ""
  )
  expect_match(s2, "\n\\cmidrule(lr){2-2} \\cmidrule(lr){3-3}\n", fixed = TRUE)
  expect_no_match(s2, "\\cmidrule(lr){1-1}", fixed = TRUE)
})

test_that("latex target: spicy_gt prints a negative statistic with the minus sign", {
  skip_if_not_installed("gt")
  # A difference and a lower bound below zero, an upper bound above it.
  set.seed(3)
  d <- data.frame(
    g = factor(rep(c("A", "B"), each = 20L)),
    y = c(stats::rnorm(20L, 5, 1), stats::rnorm(20L, 4.7, 1))
  )
  latex <- function(gtb) {
    paste(
      as.character(.with_pandoc_to("latex", knitr::knit_print(gtb))),
      collapse = ""
    )
  }
  s <- latex(table_continuous_lm(d, select = y, by = g, output = "gt"))
  ascii <- latex(table_continuous_lm(
    d,
    select = y,
    by = g,
    output = "gt",
    style = spicy_style(minus_sign = "ascii")
  ))
  # The difference and the lower bound, in both copies of the body: the box
  # measured at its natural width and the table as gt set it.
  row <- paste0(
    "{y} & {4.83} & {4.75} & {\\ensuremath{-}0.08} & ",
    "{\\ensuremath{-}0.61} & {0.46} & "
  )
  expect_length(gregexpr(row, s, fixed = TRUE)[[1L]], 2L)
  # Nothing else moves, and "ascii" leaves the hyphen gt writes. The
  # subtraction the note states keeps its minus sign under both.
  expect_identical(
    s,
    gsub("\\{-(?=[0-9])", "{\\\\ensuremath{-}", ascii, perl = TRUE)
  )
  expect_match(
    ascii,
    "{y} & {4.83} & {4.75} & {-0.08} & {-0.61} &",
    fixed = TRUE
  )
  # The label columns keep the hyphen the user typed: a group "-1".
  d$lvl <- factor(rep(c("-1", "1"), 20L))
  d$z <- d$y - 6
  s <- latex(table_continuous(d, z, by = lvl, output = "gt"))
  expect_match(s, "\nz & -1 & \\ensuremath{-}1.", fixed = TRUE)
})
