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
# separate leak on gt -- not a split row, a broken `headers=` attribute,
# and answered at the end of this file by the id witnesses. Its row
# cells are escaped by the same recipe and are covered by the
# tinytable-only witness below.
.esc_builders <- function() {
  list(
    table_outcome = function(out) {
      .esc_quiet(table_outcome(.esc_hostile(), y, select = g, output = out))
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
  console <- attr(.esc_quiet(table_outcome(d, y, select = g)), "display_df")
  stub <- trimws(console$Variable)
  norm <- function(x) trimws(gsub("[\u2007\u00A0]", " ", as.character(x)))

  tt <- .esc_quiet(table_outcome(d, y, select = g, output = "tinytable"))
  g_tbl <- .esc_quiet(table_outcome(d, y, select = g, output = "gt"))
  ft <- .esc_quiet(table_outcome(d, y, select = g, output = "flextable"))

  # The escaping happens at RENDER: every engine still holds the label
  # the console holds, tag characters included.
  expect_identical(norm(tt@data[[1L]]), stub)
  expect_identical(norm(g_tbl[["_data"]][[1L]]), stub)
  expect_identical(norm(ft$body$dataset[[1L]]), sub("^  ", "", stub))
})

test_that("a tag-shaped caption and the note are both escaped", {
  skip_if_not_installed("tinytable")
  d <- .esc_hostile()
  tt <- .esc_quiet(table_outcome(
    d,
    y,
    select = g,
    labels = c(y = "BMI <sub>kg</sub>"),
    output = "tinytable"
  ))
  html <- .esc_tt_html(tt)
  caption <- regmatches(html, regexpr("<caption>.*?</caption>", html))
  expect_length(caption, 1L)
  expect_true(grepl("&lt;sub&gt;", caption, fixed = TRUE))
  expect_false(grepl("<sub>kg</sub>", caption, fixed = TRUE))

  # The note is package prose, but it INTERPOLATES the outcome label,
  # so a tag-shaped label used to reach the `<tfoot>` live. It is now
  # escaped on the HTML branch of the finalizer only -- see the Typst
  # counterpart below, which must stay untouched.
  foot <- sub(".*<tfoot>", "", html)
  expect_false(grepl("<sub>kg</sub>", foot, fixed = TRUE))
  expect_true(grepl("&lt;sub&gt;kg&lt;/sub&gt;", foot, fixed = TRUE))
  expect_identical(
    .esc_body_rows(html),
    .esc_body_rows(.esc_tt_html(.esc_quiet(
      table_outcome(
        d,
        y,
        select = g,
        labels = c(y = "BMI"),
        output = "tinytable"
      )
    )))
  )
})

test_that("no live script survives in the note", {
  skip_if_not_installed("tinytable")
  d <- .esc_hostile()
  html <- .esc_tt_html(.esc_quiet(table_outcome(
    d,
    y,
    select = g,
    labels = c(y = "<script>alert(1)</script>"),
    output = "tinytable"
  )))
  foot <- sub(".*<tfoot>", "", html)
  expect_false(grepl("<script>", foot, fixed = TRUE))
  # Escaping is not deletion: the label is still legible in the note.
  expect_true(grepl(
    "&lt;script&gt;alert(1)&lt;/script&gt;",
    foot,
    fixed = TRUE
  ))
})

test_that("a note carrying a cell boundary does not survive the escape", {
  # The escape substitutes the note the table was BUILT with, not the
  # rendered cell text. A label carrying `</td>` is why: it is
  # indistinguishable from the real cell boundary the styling pass
  # splits on, so a text-level escape would hand it back untouched as
  # the separator that re-joins the pieces.
  skip_if_not_installed("tinytable")
  d <- .esc_hostile()
  html <- .esc_tt_html(.esc_quiet(table_outcome(
    d,
    y,
    select = g,
    labels = c(y = "</td></tr><tr><td>PWNED"),
    output = "tinytable"
  )))
  foot <- regmatches(
    html,
    regexpr("<tfoot>[\\s\\S]*?</tfoot>", html, perl = TRUE)
  )
  expect_length(foot, 1L)
  expect_false(grepl("<tr>", sub("^<tfoot><tr>", "", foot), fixed = TRUE))
  expect_true(grepl("&lt;/td&gt;&lt;/tr&gt;", foot, fixed = TRUE))
})

test_that("the note escape is engine-aware: Typst output is untouched", {
  # Escaping the note upstream, where it is composed, is not an option:
  # the same string feeds Typst, whose escape set covers the brackets a
  # `#text(8pt)[...]` note needs. The escape therefore lives in the
  # finalizer branch that only sees HTML.
  skip_if_not_installed("tinytable")
  d <- .esc_hostile()
  obj <- .esc_quiet(table_outcome(
    d,
    y,
    select = g,
    labels = c(y = "BMI [kg] <sub>x</sub>"),
    output = "tinytable"
  ))
  # One object, two backends: the same note comes out raw in Typst and
  # escaped in HTML. The Typst note is the `table.cell(align: left,
  # colspan: N, text([...]))` line -- read it alone, because the CELLS
  # around it carry the same label and ARE escaped.
  typ <- strsplit(
    as.character(tinytable::save_tt(obj, "typst")),
    "\n",
    fixed = TRUE
  )[[1L]]
  note_line <- grep("table.cell(align: left, colspan:", typ, fixed = TRUE)
  expect_length(note_line, 1L)
  note_typ <- typ[[note_line]]
  expect_true(grepl("<sub>x</sub>", note_typ, fixed = TRUE))
  expect_false(grepl("&lt;", note_typ, fixed = TRUE))
  # The brackets are the reason the note stays out of tinytable's own
  # escape set: `\[` leaves a Typst `text()` without a body.
  expect_true(grepl("BMI [kg]", note_typ, fixed = TRUE))
  expect_false(grepl("\\[kg\\]", note_typ, fixed = TRUE))
  foot <- sub(".*<tfoot>", "", .esc_tt_html(obj))
  expect_true(grepl("&lt;sub&gt;x&lt;/sub&gt;", foot, fixed = TRUE))
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

# ---------------------------------------------------------------------------
# The gt column id (register n. 73).
#
# The leak the header of this file records as "still open" on the
# `table_continuous_lm()` COLUMN labels. gt escapes every label a reader
# sees, but the id it writes into the `headers="..."` attribute of every
# body cell is the data frame's column NAME, taken raw. Four families
# build that name out of user data -- a `by` level ("Q\"x_n", "M (Q\"x)")
# or a model name ("M\"1: B") -- so a level holding a double quote closed
# the attribute early and the remainder was re-parsed as bare
# attributes: `M (Q"x)` rendered as `headers="M (Q" x)`. The same raw id
# reached the `th[id="%s"]` CSS selectors, where the quote aborted gt's
# own sass compiler and took the whole survey-categorical render down.
#
# `<script>` in a column name was never live -- it is an attribute
# value, not a text node. The damage is malformed markup and a lost
# render, which is why the witness is the ATTRIBUTE, not the absence of
# a script element.
#
# The public column name does not move: it is what `output =
# "data.frame"` publishes and what user code indexes by. Only gt's
# internal id is sanitised, and only for the characters that cannot
# survive an HTML attribute or a CSS string -- so a table of ordinary
# names renders exactly as it did.

# A `by` level carrying every character the id sanitiser answers for.
.esc_id_level <- "Q\"<b>\\x"

.esc_id_data <- function(levs = c("Lo", .esc_id_level)) {
  set.seed(11)
  n <- 40L
  data.frame(
    y = rnorm(n),
    z = rnorm(n),
    f1 = factor(rep(c("alpha", "beta"), length.out = n)),
    g = factor(rep(levs, each = n / 2L), levels = levs),
    w = runif(n, 1, 3),
    stringsAsFactors = FALSE
  )
}

# Every `headers="..."` value the rendered table carries. A truncated
# attribute yields a value that is NOT one of the table's column ids,
# which is the whole defect.
.esc_header_attrs <- function(html) {
  hits <- unlist(regmatches(html, gregexpr("headers=\"[^\"]*\"", html)))
  unique(substr(hits, 10L, nchar(hits) - 1L))
}

.esc_id_builders <- function() {
  list(
    table_continuous_lm = function(d) {
      .esc_quiet(table_continuous_lm(
        d,
        select = c(y, z),
        by = g,
        output = "gt"
      ))
    },
    table_categorical = function(d) {
      .esc_quiet(table_categorical(d, select = f1, by = g, output = "gt"))
    },
    table_categorical_svy = function(d) {
      .esc_quiet(table_categorical_svy(
        survey::svydesign(ids = ~1, weights = ~w, data = d),
        select = f1,
        by = g,
        output = "gt"
      ))
    },
    table_regression = function(d) {
      .esc_quiet(table_regression(
        stats::setNames(
          list(lm(y ~ g, data = d), lm(y ~ g + z, data = d)),
          c(levels(d$g)[2L], "M2")
        ),
        output = "gt"
      ))
    }
  )
}

test_that("a hostile column name leaves every gt headers= attribute intact", {
  skip_if_not_installed("gt")
  skip_if_not_installed("survey")
  d <- .esc_id_data()
  for (nm in names(.esc_id_builders())) {
    tbl <- .esc_id_builders()[[nm]](d)
    ids <- names(tbl[["_data"]])
    # The id layer answers for the four characters that break an HTML
    # attribute value or a CSS string.
    expect_false(
      any(grepl("[\"<>\\\\]", ids)),
      info = paste(nm, "ids:", paste(ids, collapse = " | "))
    )
    html <- as.character(gt::as_raw_html(tbl))
    attrs <- .esc_header_attrs(html)
    # Before the fix this failed on the truncated half of the name.
    expect_true(
      all(attrs %in% ids),
      info = paste(nm, "stray:", paste(setdiff(attrs, ids), collapse = " | "))
    )
  }
})

test_that("the hostile name still reads verbatim in the rendered header", {
  skip_if_not_installed("gt")
  d <- .esc_id_data()
  html <- as.character(gt::as_raw_html(
    .esc_id_builders()$table_continuous_lm(d)
  ))
  # Sanitising the ID must not touch the LABEL: gt escapes the angle
  # brackets, leaves the quote and the backslash as themselves, and the
  # reader sees the level exactly as it stands in the data.
  expect_true(grepl("M (Q\"&lt;b&gt;\\x)", html, fixed = TRUE))
  expect_false(grepl("M (Q\"<b>\\x)", html, fixed = TRUE))
})

test_that("a quoted by level no longer aborts the survey gt render", {
  # `.gt_safe_ids()` also empties the `th[id="%s"]` CSS selector these
  # branches build: the quote used to close the selector string and
  # gt's sass compiler refused the stylesheet ("unterminated attribute
  # selector for id"), losing the table, not just its borders.
  skip_if_not_installed("gt")
  skip_if_not_installed("survey")
  d <- .esc_id_data()
  expect_no_error(as.character(gt::as_raw_html(
    .esc_id_builders()$table_categorical_svy(d)
  )))
})

test_that("the gt id layer leaves the public column names alone", {
  skip_if_not_installed("gt")
  d <- .esc_id_data()
  # The data frame contract: user code indexes by these, so the
  # hostile level is still in them, character for character.
  df <- .esc_quiet(table_continuous_lm(
    d,
    select = c(y, z),
    by = g,
    output = "data.frame"
  ))
  expect_true(paste0("M (", .esc_id_level, ")") %in% names(df))
  reg <- .esc_quiet(table_regression(
    stats::setNames(
      list(lm(y ~ g, data = d), lm(y ~ g + z, data = d)),
      c(.esc_id_level, "M2")
    )
  ))
  expect_true(paste0(.esc_id_level, ": B") %in% names(as.data.frame(reg)))
})

test_that("an ordinary column name reaches gt untouched", {
  # The control for the witnesses above: the sanitiser is the IDENTITY
  # on every name that carries none of those four characters, so an
  # ordinary table renders byte for byte as it did before the id layer
  # existed. Pinned on the ids gt receives, which is what it builds the
  # rendered `headers=` and `id=` attributes from.
  skip_if_not_installed("gt")
  d <- .esc_id_data(c("Lo", "Hi"))
  expect_identical(
    names(.esc_id_builders()$table_continuous_lm(d)[["_data"]]),
    c(
      "Variable",
      "M (Lo)",
      "M (Hi)",
      "\u0394 (Hi - Lo)",
      "LL",
      "UL",
      "p",
      "R\u00b2",
      "n"
    )
  )
  expect_identical(
    names(.esc_id_builders()$table_categorical(d)[["_data"]]),
    c(
      "Variable",
      "Lo_n",
      "Lo_pct",
      "Hi_n",
      "Hi_pct",
      "Total_n",
      "Total_pct",
      "p",
      "assoc_col"
    )
  )
  expect_identical(
    names(.esc_id_builders()$table_regression(d)[["_data"]]),
    c("Variable", "Hi: B", "Hi: SE", "Hi: p", "M2: B", "M2: SE", "M2: p")
  )
  keys <- c("Variable", "M (Hi)", "95% CI LL", "p", "R\u00b2", "n", "Step 1: B")
  expect_identical(.gt_safe_ids(keys), stats::setNames(keys, keys))
  # And a tie the replacement could create is broken, not collided:
  # gt aborts on two columns sharing an id.
  expect_identical(
    unname(.gt_safe_ids(c("a\"b", "a<b"))),
    c("a_b", "a_b_1")
  )
})
