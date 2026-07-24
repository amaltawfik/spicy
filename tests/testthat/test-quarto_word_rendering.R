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
  out <- .with_pandoc_to("docx", knitr::knit_print(gtb))
  s <- paste(as.character(out), collapse = "")
  # The note -- HTML-injected as a div in HTML targets -- must reach
  # the Word rendering through gt's native source note.
  expect_match(s, "Std. errors", fixed = TRUE)
  expect_false(grepl("spicy-gt-note", s, fixed = TRUE))
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
