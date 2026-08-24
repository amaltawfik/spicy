# `export_desc_table()`: the one exporter of the descriptive families.
#
# It used to read two facts off its caller -- a `has_group` flag and
# the raw compute frame it pulled the title from. Both are now
# arguments, because a family whose stub is one column and whose rows
# are indented blocks has neither.
#
# Pinned here, directly on the exporter rather than through a family:
#
#   * `stub_keys` drives all seven places the old flag reached,
#     including the three gt addresses by frozen NAME (the column
#     alignment, the numeric-column complement, the left spanner id);
#   * `indent_rows` is inert when empty and produces, per engine, the
#     indentation that engine can render;
#   * the title belongs to the caller, and its absence is refused.

.edt_quiet <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(expr)))
  res
}

# The spanner ids gt was told to align LEFT -- the machine state the
# stub keys write, read back through the style ledger rather than
# through the spanner list.
#
# `tbl[["_spanners"]]` cannot answer the question at all.
# `desc_spanner_groups()` emits one group per COLUMN, so every column
# carries a one-wide `spn_<key>` spanner -- stub columns included, even
# when there is only one of them -- and the gt arm then puts the header
# text INTO the spanner and leaves the column label empty. The list
# therefore looks identical over a stub and over a value column; only
# `_styles` (locname == "columns_groups") records which spanners were
# aligned left. Every spanner also carries the top rule, so the list
# would not separate them on that either.
.edt_left_spanners <- function(tbl) {
  styles <- tbl[["_styles"]]
  rows <- styles[styles$locname == "columns_groups", , drop = FALSE]
  keep <- vapply(
    rows$styles,
    function(s) identical(s$cell_text$align, "left"),
    logical(1)
  )
  rows$grpname[keep]
}

# The display frame of a grouped continuous table, plus its stub keys.
.edt_frame <- function() {
  d <- as.data.frame(spicy::sochealth)
  tbl <- .edt_quiet(table_continuous(d, select = bmi, by = sex))
  build_display_df(
    tbl,
    digits = 2L,
    decimal_mark = ".",
    ci_level = 0.95,
    show_p = TRUE
  )
}

test_that("export_desc_table() refuses to invent a title", {
  df <- .edt_frame()
  err <- tryCatch(
    export_desc_table(
      df,
      output = "tinytable",
      ci_level = 0.95,
      stub_keys = c(.CON_KEY_VARIABLE, .CON_KEY_GROUP),
      excel_path = NULL,
      excel_sheet = "x",
      clipboard_delim = "\t",
      word_path = NULL
    ),
    error = identity
  )
  expect_s3_class(err, "spicy_internal_invariant")
  expect_match(conditionMessage(err), "title")
})

test_that("stub_keys drives the gt stub, by name", {
  skip_if_not_installed("gt")
  df <- .edt_frame()
  keys <- c(.CON_KEY_VARIABLE, .CON_KEY_GROUP)
  tbl <- export_desc_table(
    df,
    output = "gt",
    ci_level = 0.95,
    stub_keys = keys,
    title = "T",
    excel_path = NULL,
    excel_sheet = "x",
    clipboard_delim = "\t",
    word_path = NULL
  )
  # Both stub columns are left-aligned, and nothing else is.
  boxh <- tbl[["_boxhead"]]
  left <- boxh$var[vapply(boxh$column_align, identical, logical(1), "left")]
  expect_setequal(left, keys)
  # The left SPANNERS are addressed by id, built from the same keys.
  expect_setequal(.edt_left_spanners(tbl), paste0("spn_", keys))
})

test_that("a one-column stub leaves the group column to the numbers", {
  # The other half of the same property: with one stub key, the second
  # column is a value column -- centred with the rest, and absent from
  # the LEFT-ALIGNED spanner ids. It still has a spanner of its own:
  # every column does. See `.edt_left_spanners()` above.
  skip_if_not_installed("gt")
  df <- .edt_frame()
  tbl <- export_desc_table(
    df,
    output = "gt",
    ci_level = 0.95,
    stub_keys = .CON_KEY_VARIABLE,
    title = "T",
    excel_path = NULL,
    excel_sheet = "x",
    clipboard_delim = "\t",
    word_path = NULL
  )
  boxh <- tbl[["_boxhead"]]
  left <- boxh$var[vapply(boxh$column_align, identical, logical(1), "left")]
  expect_identical(left, .CON_KEY_VARIABLE)
  expect_identical(
    .edt_left_spanners(tbl),
    paste0("spn_", .CON_KEY_VARIABLE)
  )
})

test_that("indent_rows is inert when empty and indents when given", {
  skip_if_not_installed("tinytable")
  df <- .edt_frame()
  keys <- c(.CON_KEY_VARIABLE, .CON_KEY_GROUP)
  build <- function(rows) {
    export_desc_table(
      df,
      output = "tinytable",
      ci_level = 0.95,
      stub_keys = keys,
      indent_rows = rows,
      indent_text = "",
      title = "T",
      excel_path = NULL,
      excel_sheet = "x",
      clipboard_delim = "\t",
      word_path = NULL
    )
  }
  plain <- build(integer(0))
  indented <- build(2L)
  # The label of the untouched row is the same on both.
  expect_identical(plain@data[[1L]][1L], indented@data[[1L]][1L])
  # The indented row gained the four non-breaking spaces.
  expect_identical(
    indented@data[[1L]][2L],
    paste0(strrep("\u00A0", 4), plain@data[[1L]][2L])
  )
})

test_that("the office engines indent the way each of them can", {
  df <- .edt_frame()
  keys <- c(.CON_KEY_VARIABLE, .CON_KEY_GROUP)
  # Flextable takes the prefix OFF and pads instead: one indentation,
  # and the one that survives docx.
  skip_if_not_installed("flextable")
  ft <- export_desc_table(
    df,
    output = "flextable",
    ci_level = 0.95,
    stub_keys = keys,
    indent_rows = 2L,
    indent_text = "  ",
    title = "T",
    excel_path = NULL,
    excel_sheet = "x",
    clipboard_delim = "\t",
    word_path = NULL
  )
  expect_identical(
    ft$body$dataset[[1L]][2L],
    substring(df[[1L]][2L], 3L)
  )
  expect_identical(unname(ft$body$styles$pars$padding.left$data[2L, 1L]), 14)

  # The clipboard swaps the console prefix for the wider one.
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(content, ...) {
      captured$text <- content
      invisible(content)
    },
    .package = "clipr"
  )
  .edt_quiet(export_desc_table(
    df,
    output = "clipboard",
    ci_level = 0.95,
    stub_keys = keys,
    indent_rows = 2L,
    indent_text = "",
    indent_text_excel_clipboard = strrep("_", 6),
    title = "T",
    excel_path = NULL,
    excel_sheet = "x",
    clipboard_delim = "\t",
    word_path = NULL
  ))
  lines <- strsplit(captured$text, "\n", fixed = TRUE)[[1L]]
  expect_true(any(startsWith(lines, strrep("_", 6))))
})

test_that("excel writes the stronger indent into the label", {
  skip_if_not_installed("openxlsx2")
  df <- .edt_frame()
  f <- withr::local_tempfile(fileext = ".xlsx")
  export_desc_table(
    df,
    output = "excel",
    ci_level = 0.95,
    stub_keys = c(.CON_KEY_VARIABLE, .CON_KEY_GROUP),
    indent_rows = 2L,
    indent_text = "",
    indent_text_excel_clipboard = strrep("_", 6),
    title = "T",
    excel_path = f,
    excel_sheet = "Sheet",
    clipboard_delim = "\t",
    word_path = NULL
  )
  cells <- openxlsx2::wb_to_df(
    openxlsx2::wb_load(f),
    col_names = FALSE
  )
  expect_true(any(startsWith(
    as.character(cells[[1L]])[!is.na(cells[[1L]])],
    strrep("_", 6)
  )))
})


# ---- the one-way rule policy, at the layer every engine reads -------------
#
# Decision 37 gates the inter-variable rule of the continuous families on
# `by`. The gate lives in the CALLER, in the `sep_rows` it hands the
# exporter, so every engine -- tinytable, gt, flextable, Word, Excel --
# inherits it from one place. tinytable and flextable pin the rendered
# consequence in their own parity files; this pins the contract itself,
# which is what the three engines with no cheap structural oracle rely
# on.

# The `sep_rows` the family hands the exporter for a given call.
.edt_sep_rows_for <- function(expr) {
  seen <- NULL
  testthat::local_mocked_bindings(
    export_desc_table = function(display_df, ..., sep_rows = integer(0)) {
      seen <<- sep_rows
      invisible(NULL)
    }
  )
  .edt_quiet(expr)
  seen
}

test_that("table_continuous() sends no sep_rows without `by`, and some with", {
  d <- as.data.frame(sochealth)
  for (engine in c("tinytable", "gt", "flextable")) {
    expect_length(
      .edt_sep_rows_for(
        table_continuous(d, c(age, wellbeing_score), output = engine)
      ),
      0L
    )
    expect_length(
      .edt_sep_rows_for(
        table_continuous(d, c(age, wellbeing_score), by = sex, output = engine)
      ),
      1L
    )
  }
})

test_that("table_continuous_svy() sends no sep_rows without `by`", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  des <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
  expect_length(
    .edt_sep_rows_for(
      table_continuous_svy(des, select = c(api00, api99), output = "gt")
    ),
    0L
  )
  expect_true(length(.edt_sep_rows_for(
    table_continuous_svy(
      des,
      select = c(api00, api99),
      by = stype,
      output = "gt"
    )
  )) >= 1L)
})
