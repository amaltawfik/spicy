# Level labels, variable labels and captions are USER DATA. An engine
# that passes them through unescaped does not merely look wrong: a
# label holding `</td></tr><tr><td>` closes its own cell and row, so
# the rendered table has MORE rows than the object, with the
# statistics silently redistributed across them -- two engines
# disagreeing about the geometry of the same object. A label holding
# `<script>` is emitted live into the document.
#
# gt and flextable escape on their own. tinytable does not, and every
# descriptive family fed it raw. `.spicy_tt_escape()` is the recipe
# `table_regression()` already used.
#
# The witness that matters is the ROW COUNT, compared across engines:
# it is the property that cannot be satisfied by escaping "somewhere".

.esc_hostile <- function() {
  data.frame(
    g = factor(c(
      rep("</td></tr><tr><td>PWNED", 4L),
      rep("<script>alert(1)</script>", 4L)
    )),
    y = c(1, 2, 3, 4, 10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
}

.esc_quiet <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(expr)))
  res
}

# Body rows of a rendered HTML table.
.esc_body_rows <- function(html) {
  body <- sub("</tbody>.*", "", sub(".*<tbody[^>]*>", "", html))
  length(gregexpr("<tr", body, fixed = TRUE)[[1L]])
}

.esc_tt_html <- function(x) {
  f <- withr::local_tempfile(fileext = ".html")
  suppressMessages(tinytable::save_tt(x, f, overwrite = TRUE))
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# The families that put the `by` LEVELS in rows. Those are the ones a
# tag-shaped level can split, and the ones this fix is about.
#
# `table_continuous_lm()` is deliberately absent: it puts the levels in
# COLUMN labels ("M (Male)"), a different surface with its own,
# separate, still-open leak on gt -- pre-existing, out of this fix's
# scope, and recorded in the incident register rather than half-fixed
# here. Its row cells are escaped by the same recipe and are covered by
# the tinytable-only witness below.
.esc_builders <- function() {
  list(
    table_outcome = function(out) {
      .esc_quiet(table_outcome(.esc_hostile(), y, by = g, output = out))
    },
    table_continuous = function(out) {
      .esc_quiet(table_continuous(
        .esc_hostile(),
        select = y,
        by = g,
        p_value = FALSE,
        output = out
      ))
    },
    table_categorical = function(out) {
      .esc_quiet(table_categorical(.esc_hostile(), select = g, output = out))
    }
  )
}

test_that("a tag-shaped level does not change the rendered row count", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  for (nm in names(.esc_builders())) {
    build <- .esc_builders()[[nm]]
    tt_rows <- .esc_body_rows(.esc_tt_html(build("tinytable")))
    gt_rows <- .esc_body_rows(as.character(gt::as_raw_html(build("gt"))))
    expect_identical(
      tt_rows,
      gt_rows,
      info = paste(nm, "tinytable", tt_rows, "vs gt", gt_rows)
    )
  }
})

test_that("no engine emits a live script element", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  for (nm in names(.esc_builders())) {
    build <- .esc_builders()[[nm]]
    tt_html <- .esc_tt_html(build("tinytable"))
    gt_html <- as.character(gt::as_raw_html(build("gt")))
    expect_false(
      grepl("<script>alert(1)</script>", tt_html, fixed = TRUE),
      info = paste(nm, "tinytable")
    )
    expect_false(
      grepl("<script>alert(1)</script>", gt_html, fixed = TRUE),
      info = paste(nm, "gt")
    )
    # And the label is still THERE, escaped -- escaping must not be
    # deletion.
    expect_true(
      grepl("&lt;script&gt;", tt_html, fixed = TRUE),
      info = paste(nm, "tinytable keeps the label")
    )
  }
})

test_that("the three rich engines carry the same label content", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  d <- .esc_hostile()
  console <- attr(.esc_quiet(table_outcome(d, y, by = g)), "display_df")
  stub <- trimws(console$Variable)
  norm <- function(x) trimws(gsub("[\u2007\u00A0]", " ", as.character(x)))

  tt <- .esc_quiet(table_outcome(d, y, by = g, output = "tinytable"))
  g_tbl <- .esc_quiet(table_outcome(d, y, by = g, output = "gt"))
  ft <- .esc_quiet(table_outcome(d, y, by = g, output = "flextable"))

  # The escaping happens at RENDER: every engine still holds the label
  # the console holds, tag characters included.
  expect_identical(norm(tt@data[[1L]]), stub)
  expect_identical(norm(g_tbl[["_data"]][[1L]]), stub)
  expect_identical(norm(ft$body$dataset[[1L]]), sub("^  ", "", stub))
})

test_that("a tag-shaped caption is escaped, and the note is not", {
  skip_if_not_installed("tinytable")
  d <- .esc_hostile()
  tt <- .esc_quiet(table_outcome(
    d,
    y,
    by = g,
    labels = c(y = "BMI <sub>kg</sub>"),
    output = "tinytable"
  ))
  html <- .esc_tt_html(tt)
  caption <- regmatches(html, regexpr("<caption>.*?</caption>", html))
  expect_length(caption, 1L)
  expect_true(grepl("&lt;sub&gt;", caption, fixed = TRUE))
  expect_false(grepl("<sub>kg</sub>", caption, fixed = TRUE))

  # The NOTE is deliberately outside the escape set, family-wide:
  # escaping it broke Typst compilation (the typst escape set covers
  # the brackets a `#text(8pt)[...]` note needs), and the note is
  # package prose. It does interpolate the outcome label, so a
  # tag-shaped label still reaches the footer raw -- pinned here as a
  # KNOWN residual rather than left to be discovered, and harmless to
  # the geometry because `<tfoot>` sits outside `<tbody>`.
  foot <- sub(".*<tfoot>", "", html)
  expect_true(grepl("<sub>kg</sub>", foot, fixed = TRUE))
  expect_identical(
    .esc_body_rows(html),
    .esc_body_rows(.esc_tt_html(.esc_quiet(
      table_outcome(d, y, by = g, labels = c(y = "BMI"), output = "tinytable")
    )))
  )
})

test_that("the linear-model family escapes its tinytable cells too", {
  # Its levels live in COLUMN labels, so it is not part of the
  # cross-engine row-count comparison above -- but its body cells go
  # through the same tinytable branch and must be escaped like the
  # rest.
  skip_if_not_installed("tinytable")
  d <- .esc_hostile()
  d$lab <- NULL
  tt <- .esc_quiet(table_continuous_lm(
    d,
    select = y,
    by = g,
    labels = c(y = "<b>bold</b>"),
    output = "tinytable"
  ))
  html <- .esc_tt_html(tt)
  body <- sub("</tbody>.*", "", sub(".*<tbody[^>]*>", "", html))
  expect_true(grepl("&lt;b&gt;bold", body, fixed = TRUE))
  expect_false(grepl("<b>bold</b>", body, fixed = TRUE))
})
