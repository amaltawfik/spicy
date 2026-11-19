# The note of a gt table does not widen the table in HTML: its source
# notes take a set width (`.spicy_gt_note_css()`), within the id the
# table is built with (`.spicy_gt()`).

.gtw_long <- paste(
  "A long note added to the table so that it runs well past the width of",
  "the body: the sample, the weighting, the missing values and the source."
)

.gtw_tables <- function() {
  list(
    regression = table_regression(
      list(
        stats::lm(mpg ~ wt + hp, data = mtcars),
        stats::lm(mpg ~ wt + hp + qsec, data = mtcars)
      ),
      output = "gt",
      note_add = .gtw_long
    ),
    categorical = table_categorical(
      sochealth,
      select = c(smoking, physical_activity),
      by = sex,
      output = "gt",
      note_add = .gtw_long
    )
  )
}

test_that("the source notes of a gt table take a set width in its HTML", {
  skip_if_not_installed("gt")
  for (g in .gtw_tables()) {
    id <- g[["_options"]]$value[[which(
      g[["_options"]]$parameter == "table_id"
    )]]
    expect_match(id, "^spicy-gt-[A-Za-z0-9]+-[0-9]+$")
    rule <- paste0(
      "#",
      id,
      " \\.gt_sourcenote \\{\\s*width: 1px;\\s*line-height: 1\\.25;"
    )
    # A page with its style sheet (gtsave() to .html and the display
    # methods) ...
    page <- as.character(gt::as_raw_html(g, inline_css = FALSE))
    expect_match(page, paste0("<div id=\"", id, "\""), fixed = TRUE)
    expect_match(page, rule, perl = TRUE)
    expect_match(page, "<td class=\"gt_sourcenote\"", fixed = TRUE)
    # ... and the inlined HTML of as_raw_html(), where the rules sit on
    # the cell of the note.
    inline <- as.character(gt::as_raw_html(g))
    cell <- regmatches(
      inline,
      regexpr("<td class=\"gt_sourcenote\"[^>]*>", inline)
    )
    expect_match(cell, "width: 1px; line-height: 1.25;", fixed = TRUE)
    expect_match(cell, "padding-top: 0.5rem; padding-bottom: 0.2rem;")
    # Its text starts 5px in, as the text of a body cell does.
    expect_match(cell, "padding-left: 5px;", fixed = TRUE)
  }
})

test_that("a padding of the note set by tab_options() wins over spicy's", {
  skip_if_not_installed("gt")
  g <- .gtw_tables()$regression
  cell <- function(x) {
    inline <- as.character(gt::as_raw_html(x))
    regmatches(inline, regexpr("<td class=\"gt_sourcenote\"[^>]*>", inline))
  }
  expect_match(
    cell(g),
    "padding-top: 0.5rem; padding-bottom: 0.2rem;",
    fixed = TRUE
  )
  set <- cell(gt::tab_options(g, source_notes.padding = gt::px(12)))
  expect_match(set, "padding-top: 12px; padding-bottom: 12px;", fixed = TRUE)
  expect_no_match(set, "rem;", fixed = TRUE)
  wide <- cell(gt::tab_options(g, source_notes.padding.horizontal = gt::px(20)))
  expect_match(wide, "padding-left: 20px; padding-right: 20px;", fixed = TRUE)
  expect_no_match(wide, "padding: 0", fixed = TRUE)
})

test_that("each gt table has an id of its own, drawn without the RNG", {
  skip_if_not_installed("gt")
  set.seed(1)
  seed <- .Random.seed
  a <- .spicy_gt(data.frame(x = 1))
  b <- .spicy_gt(data.frame(x = 1))
  expect_identical(.Random.seed, seed)
  id <- function(g) {
    g[["_options"]]$value[[which(g[["_options"]]$parameter == "table_id")]]
  }
  expect_false(identical(id(a), id(b)))
  # The name of the temporary directory sets the session apart, so that
  # a cached chunk and a fresh one never share an id.
  session <- gsub("[^A-Za-z0-9]", "", basename(tempdir()))
  expect_match(id(a), paste0("^spicy-gt-", session, "-[0-9]+$"))
})

test_that("the rules of the note leave the LaTeX, RTF and Word outputs alone", {
  skip_if_not_installed("gt")
  df <- data.frame(x = c(1.5, -2), y = c("a", "b"))
  note <- gt::md("*Note.* A note, long enough to wrap.")
  plain <- gt::tab_source_note(gt::gt(df), note)
  ours <- gt::tab_source_note(.spicy_gt(df), note)
  expect_identical(
    as.character(gt::as_latex(ours)),
    as.character(gt::as_latex(plain))
  )
  expect_identical(gt::as_rtf(ours), gt::as_rtf(plain))
  expect_identical(gt::as_word(ours), gt::as_word(plain))
})

test_that("gtsave() writes a PNG as wide as the table without its note", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("gt")
  skip_if_not_installed("webshot2")
  dir <- withr::local_tempdir()
  # The width of a PNG, from its header.
  png_width <- function(path) {
    con <- file(path, "rb")
    on.exit(close(con))
    b <- readBin(con, "raw", 24L)
    sum(as.integer(b[17:20]) * 256^(3:0))
  }
  tabs <- .gtw_tables()
  for (nm in names(tabs)) {
    g <- tabs[[nm]]
    class(g) <- setdiff(class(g), "spicy_gt")
    bare <- g
    bare[["_source_notes"]] <- list()
    f1 <- file.path(dir, paste0(nm, "_note.png"))
    f0 <- file.path(dir, paste0(nm, "_bare.png"))
    suppressMessages(gt::gtsave(g, f1, zoom = 1, expand = 0))
    suppressMessages(gt::gtsave(bare, f0, zoom = 1, expand = 0))
    expect_identical(png_width(f1), png_width(f0), label = nm)
  }
})
