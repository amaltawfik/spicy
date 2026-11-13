# Stacked tables: `table_stack()` and `table_regression(layout = "stacked")`.
#
# The oracle is the one the decision of 2026-10-16 names
# (dev/stacked_layout_spec.md): each panel of a stack is, cell for cell,
# the table it came from, rendered by the same engine -- the note apart,
# since the stack merges the notes. Each battery builds the stack and each
# panel's table alone, then holds the rows of every panel against its
# table: in the console body, the merged formatter, the typed views and
# the clipboard payload, and, outside the CRAN tier, in gt, tinytable,
# flextable, Excel and Word.
#
# A stack lines up the decimals of a column across its panels, so the
# padding is the one thing a panel need not share with its table: the
# comparisons read the content of each cell (`.st_norm()`), as the engine
# parity batteries do (test-engine_parity_contract.R, whose helpers these
# mirror under their own names).

# ---- helpers -----------------------------------------------------------------

.st_fig <- intToUtf8(0x2007L) # figure space: the decimal-alignment padding
.st_nbsp <- intToUtf8(0x00A0L) # no-break space
.st_minus <- intToUtf8(0x2212L) # the minus sign of the printed engines

# The content of a cell: the minus sign read as the console's hyphen, the
# padding gone -- figure spaces, the spaces around the cell, and the runs
# of spaces that align the two bounds inside the brackets of an interval
# ("[ 0.02,  0.05]" and "[0.02, 0.05]" are the same cell).
.st_norm <- function(x) {
  x <- as.character(x)
  x <- gsub(.st_minus, "-", x, fixed = TRUE)
  x <- gsub(.st_fig, " ", x, fixed = TRUE)
  x <- gsub(" {2,}", " ", x)
  x <- gsub("[ ", "[", x, fixed = TRUE)
  x <- gsub(" ]", "]", x, fixed = TRUE)
  trimws(x)
}

.st_df <- function(x) {
  out <- as.data.frame(lapply(x, .st_norm), stringsAsFactors = FALSE)
  names(out) <- names(x)
  rownames(out) <- NULL
  out
}

# The body each engine lays out, as the content of its cells.
.st_console <- function(tbl) .st_df(structure(tbl, class = "data.frame"))
.st_view <- function(s) .st_df(spicy:::.format_structured_to_string_body(s))
.st_structured <- function(tbl) .st_view(as_structured(tbl))
.st_gt <- function(g) {
  .st_df(as.data.frame(g[["_data"]], stringsAsFactors = FALSE))
}
.st_tt <- function(tt) .st_df(as.data.frame(tt@data, stringsAsFactors = FALSE))
.st_ft <- function(ft) {
  .st_df(as.data.frame(ft$body$dataset, stringsAsFactors = FALSE))
}

# The labels a table prints over its value columns.
.st_labels <- function(tbl) {
  meta <- attr(tbl, "structured")$col_meta
  unname(vapply(
    names(tbl)[-1L],
    function(nm) spicy:::.lookup_display_label(nm, meta),
    character(1)
  ))
}

.st_count <- function(x, pattern) {
  lengths(regmatches(x, gregexpr(pattern, x, fixed = TRUE)))
}

.st_blank <- function(x) all(is.na(x) | !nzchar(x))

# The parts of a table's note, which the stack merges: the arguments of
# the note composer, kept on the table under an internal attribute.
.st_parts <- function(tbl) attr(tbl, "spicy_note_parts", exact = TRUE)

# The note of the same models side by side, with the columns of a model
# alone, which is what a stack of single-model panels shows: the merged
# note of the stack words its themes as this note does.
.st_side_note <- function(models, ...) {
  attr(
    table_regression(models, show_columns = c("b", "se", "ci", "p"), ...),
    "note"
  )
}

# The conditions an expression signals, as "class: message" strings (the
# leaf class, the message as printed), its value aside.
.st_conditions <- function(expr) {
  out <- character(0)
  leaf <- function(cnd) {
    setdiff(
      class(cnd),
      c(
        "spicy_error",
        "spicy_warning",
        "spicy_info",
        "rlang_error",
        "rlang_warning",
        "rlang_message",
        "error",
        "warning",
        "message",
        "condition"
      )
    )[[1L]]
  }
  record <- function(cnd) {
    out <<- c(out, paste0(leaf(cnd), ": ", conditionMessage(cnd)))
  }
  withCallingHandlers(
    tryCatch(expr, error = function(e) record(e)),
    warning = function(w) {
      record(w)
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      record(m)
      invokeRestart("muffleMessage")
    }
  )
  out
}

# The column ranges a stack merges on body row `row` (`header_spans`, in
# the columns of the engines' body, the label column first).
.st_spans_at <- function(st, row) {
  spans <- attr(st, "header_spans")
  at <- Filter(function(h) identical(as.integer(h$row), as.integer(row)), spans)
  lapply(at, function(h) as.integer(h$cols))
}

# A panel's warning about its own arguments, which a stack silences in
# favor of the one it emits for the list (`exponentiate` on an lm).
.st_quiet <- function(expr) {
  withCallingHandlers(
    expr,
    spicy_ignored_arg = function(w) invokeRestart("muffleWarning")
  )
}

# The cells of a body, its column names set aside: engines name their
# columns differently by design. Positional names rather than none, so a
# failure reads as a cell ("actual$V2[11]") instead of an error in the
# comparison of two nameless data frames.
.st_cells <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- paste0("V", seq_along(df))
  rownames(df) <- NULL
  df
}

# Rows `rows` of a stack, cut to the width of a panel's table, are that
# table; the cells the stack has beyond that width are blank on them.
.st_expect_panel <- function(st_df, panel_df, rows, info) {
  w <- ncol(panel_df)
  if (!isTRUE(ncol(st_df) >= w)) {
    expect_true(ncol(st_df) >= w, info = info)
    return(invisible())
  }
  got <- st_df[rows, seq_len(w), drop = FALSE]
  expect_equal(.st_cells(got), .st_cells(panel_df), info = info)
  if (ncol(st_df) > w) {
    rest <- unlist(st_df[rows, -seq_len(w), drop = FALSE], use.names = FALSE)
    expect_true(.st_blank(rest), info = info)
  }
}

# A written table as a list of rows, one character vector of cells each:
# the fields of the clipboard payload, the cells of the Word table.
.st_clip_rows <- function(tbl) {
  payload <- spicy:::clipboard_payload(tbl, "\t")
  strsplit(strsplit(payload, "\n", fixed = TRUE)[[1L]], "\t", fixed = TRUE)
}

.st_wd_rows <- function(path) {
  s <- officer::docx_summary(officer::read_docx(path))
  cells <- s[s$content_type == "table cell", , drop = FALSE]
  lapply(split(cells, cells$row_id), function(r) r$text[order(r$cell_id)])
}

.st_wd_trim <- function(x) {
  pad <- paste0("[ \t", .st_fig, .st_nbsp, "]+")
  gsub(paste0("^", pad, "|", pad, "$"), "", x)
}

# Panel k of the written stack opens on the row whose first cell is its
# section label; its table, written alone, opens on the row whose first
# cell is its first label. From there the rows run one for one. `w` is
# the width of the panel's table (its label column and value columns).
.st_expect_rows <- function(rows_st, rows_k, st, tbl, k, w, trim, info) {
  first <- function(rows) {
    vapply(
      rows,
      function(r) if (length(r)) .st_norm(trim(r[[1L]])) else "",
      character(1)
    )
  }
  sec <- attr(st, "section_rows")[[k]]
  own <- attr(st, "panel_rows")[[k]]
  at <- which(first(rows_st) == .st_norm(st[[1L]][sec]))
  at_k <- which(first(rows_k) == .st_norm(tbl[[1L]][1L]))
  found <- length(at) == 1L &&
    length(at_k) >= 1L &&
    max(at + own - sec) <= length(rows_st)
  expect_true(found, info = info)
  if (!found) {
    return(invisible())
  }
  pad <- function(r, n) c(r, rep("", max(0L, n - length(r))))
  got <- lapply(at + own - sec, function(i) trim(pad(rows_st[[i]], w)))
  want <- lapply(
    at_k[1L] + seq_len(nrow(tbl)) - 1L,
    function(i) trim(pad(rows_k[[i]], w))
  )
  expect_identical(lapply(got, `[`, seq_len(w)), want, info = info)
  rest <- unlist(lapply(got, function(r) r[-seq_len(w)]))
  expect_true(.st_blank(rest), info = info)
}

# An Excel sheet: its grid of values (row names are the Excel row
# numbers, column names the letters), and the type and number format of a
# cell as the file stores them (the chain of test-engine_parity_xlsx.R,
# read once per file).
.st_xl_read <- function(path) {
  wb <- openxlsx2::wb_load(path)
  m <- as.matrix(openxlsx2::wb_to_df(wb, col_names = FALSE))
  storage.mode(m) <- "character"
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  xfs <- wb$styles_mgr$styles$cellXfs
  fmts <- wb$styles_mgr$styles$numFmts
  type <- function(ref) {
    t <- cc$c_t[cc$r == ref]
    if (length(t) == 0L) {
      return(NA_character_)
    }
    if (is.na(t[1L])) {
      return("number")
    }
    switch(t[1L], "inlineStr" = "text", "e" = "error", "str" = "text", "number")
  }
  numfmt <- function(ref) {
    s <- cc$c_s[cc$r == ref]
    if (length(s) == 0L || is.na(s[1L]) || !nzchar(s[1L])) {
      return(NA_character_)
    }
    xf <- xfs[[as.integer(s[1L]) + 1L]]
    id <- sub(".*numFmtId=\"([0-9]+)\".*", "\\1", xf)
    hit <- grep(paste0("numFmtId=\"", id, "\""), fmts, value = TRUE)
    if (length(hit) == 0L) {
      return(NA_character_)
    }
    sub(".*formatCode=\"([^\"]*)\".*", "\\1", hit[1L])
  }
  list(grid = m, type = type, numfmt = numfmt)
}

# Excel, cell for cell: the value, the type (number or text) and the
# number format of every cell of panel k against its table written alone.
.st_expect_excel <- function(st, tabs, build_st, build_k) {
  sec <- attr(st, "section_rows")
  own <- attr(st, "panel_rows")
  p <- withr::local_tempfile(fileext = ".xlsx")
  # The path of the file comes back, invisibly.
  res <- withVisible(build_st("excel", excel_path = p))
  expect_identical(res, list(value = p, visible = FALSE))
  xs <- .st_xl_read(p)
  a <- .st_norm(xs$grid[, "A"])
  for (k in seq_along(tabs)) {
    info <- sprintf("excel, panel %d", k)
    pk <- withr::local_tempfile(fileext = ".xlsx")
    build_k[[k]]("excel", excel_path = pk)
    xk <- .st_xl_read(pk)
    at <- which(a == .st_norm(st[[1L]][sec[[k]]]))
    at_k <- which(.st_norm(xk$grid[, "A"]) == .st_norm(tabs[[k]][[1L]][1L]))
    expect_true(length(at) == 1L && length(at_k) >= 1L, info = info)
    if (length(at) != 1L || length(at_k) == 0L) {
      next
    }
    rows_st <- as.integer(rownames(xs$grid)[at]) - sec[[k]] + own[[k]]
    rows_k <- as.integer(rownames(xk$grid)[at_k[1L]]) -
      1L +
      seq_len(nrow(tabs[[k]]))
    cols <- colnames(xk$grid)[seq_len(ncol(.st_structured(tabs[[k]])))]
    expect_identical(
      unname(xs$grid[as.character(rows_st), cols, drop = FALSE]),
      unname(xk$grid[as.character(rows_k), cols, drop = FALSE]),
      info = info
    )
    refs <- function(rows) {
      as.vector(t(outer(rows, cols, function(r, cl) paste0(cl, r))))
    }
    expect_identical(
      vapply(refs(rows_st), xs$type, character(1), USE.NAMES = FALSE),
      vapply(refs(rows_k), xk$type, character(1), USE.NAMES = FALSE),
      info = paste(info, "(types)")
    )
    expect_identical(
      vapply(refs(rows_st), xs$numfmt, character(1), USE.NAMES = FALSE),
      vapply(refs(rows_k), xk$numfmt, character(1), USE.NAMES = FALSE),
      info = paste(info, "(number formats)")
    )
  }
}

# The oracle. `st` is the stack (`output = "default"`) and `tabs` its
# panels built alone; `build_st(o, ...)` and `build_k[[k]](o, ...)` make
# the same calls with `output = o`. The console, the merged formatter, the
# typed views and the clipboard always run; `engines` adds the rich ones
# ("tinytable", "flextable", "gt", "excel", "word"). An engine whose
# package is missing is passed over rather than skipped, so the others
# still run.
.st_expect_oracle <- function(
  st,
  tabs,
  build_st = NULL,
  build_k = NULL,
  engines = character(0)
) {
  own <- attr(st, "panel_rows")
  sec <- attr(st, "section_rows")
  hdr <- attr(st, "header_rows") %||% integer(0)
  n <- length(tabs)
  expect_identical(length(own), n)
  expect_identical(length(sec), n)
  expect_identical(names(own), names(attr(st, "panels")))
  expect_equal(
    unname(lengths(own)),
    unname(vapply(tabs, nrow, integer(1)))
  )
  # Every row of the stack is a section row, a header row or a row of one
  # panel, once.
  expect_equal(
    sort(c(sec, hdr, unlist(own, use.names = FALSE))),
    seq_len(nrow(st))
  )
  # The typed body runs row for row with the printed one: section rows
  # are block headers, header rows say so, and a panel's rows keep the
  # identity they have in its table.
  b <- attr(st, "structured")$body
  expect_identical(nrow(b), nrow(st))
  expect_true(all(b$.row_role[sec] == "factor_header"))
  expect_true(all(b$.indent[sec] == 0L))
  expect_true(all(b$.row_role[hdr] == "header"))
  views <- lapply(tabs, as_structured)
  ids <- c(".variable", ".level", ".row_role", ".indent")
  for (k in seq_len(n)) {
    got <- b[own[[k]], ids, drop = FALSE]
    rownames(got) <- NULL
    want <- views[[k]]$body[, ids, drop = FALSE]
    rownames(want) <- NULL
    expect_equal(got, want, info = sprintf("identity, panel %d", k))
  }

  # Console.
  sc <- .st_console(st)
  for (k in seq_len(n)) {
    .st_expect_panel(
      sc,
      .st_console(tabs[[k]]),
      own[[k]],
      sprintf("console, panel %d", k)
    )
  }
  # The formatter every string-driven engine reads, on the merged view.
  ss <- .st_view(attr(st, "structured"))
  for (k in seq_len(n)) {
    .st_expect_panel(
      ss,
      .st_view(views[[k]]),
      own[[k]],
      sprintf("formatter, panel %d", k)
    )
  }
  # Typed views: the list of the panels' own.
  s <- as_structured(st)
  expect_type(s, "list")
  expect_identical(names(s), names(attr(st, "panels")))
  for (k in seq_len(n)) {
    expect_identical(s[[k]], views[[k]], info = sprintf("typed, panel %d", k))
    expect_identical(
      s[[k]],
      as_structured(attr(st, "panels")[[k]]),
      info = sprintf("typed (stored panel), panel %d", k)
    )
  }
  # Clipboard.
  rows_st <- .st_clip_rows(st)
  for (k in seq_len(n)) {
    .st_expect_rows(
      rows_st,
      .st_clip_rows(tabs[[k]]),
      st,
      tabs[[k]],
      k,
      ncol(.st_view(views[[k]])),
      identity,
      sprintf("clipboard, panel %d", k)
    )
  }

  # Rich engines: the body each one writes, from the same call.
  readers <- list(gt = .st_gt, tinytable = .st_tt, flextable = .st_ft)
  for (eng in intersect(engines, names(readers))) {
    if (!requireNamespace(eng, quietly = TRUE)) {
      next
    }
    obj <- build_st(eng)
    whole <- readers[[eng]](obj)
    for (k in seq_len(n)) {
      obj_k <- build_k[[k]](eng)
      if (k == 1L) {
        expect_identical(class(obj), class(obj_k), info = eng)
      }
      .st_expect_panel(
        whole,
        readers[[eng]](obj_k),
        own[[k]],
        sprintf("%s, panel %d", eng, k)
      )
    }
  }
  if ("excel" %in% engines && requireNamespace("openxlsx2", quietly = TRUE)) {
    .st_expect_excel(st, tabs, build_st, build_k)
  }
  if (
    "word" %in%
      engines &&
      requireNamespace("flextable", quietly = TRUE) &&
      requireNamespace("officer", quietly = TRUE)
  ) {
    p <- withr::local_tempfile(fileext = ".docx")
    res <- withVisible(build_st("word", word_path = p))
    expect_identical(res, list(value = p, visible = FALSE))
    rows_st <- .st_wd_rows(p)
    for (k in seq_len(n)) {
      pk <- withr::local_tempfile(fileext = ".docx")
      build_k[[k]]("word", word_path = pk)
      .st_expect_rows(
        rows_st,
        .st_wd_rows(pk),
        st,
        tabs[[k]],
        k,
        ncol(.st_view(views[[k]])),
        .st_wd_trim,
        sprintf("word, panel %d", k)
      )
    }
  }
  invisible(st)
}

.st_all_engines <- c("tinytable", "flextable", "gt", "excel", "word")

# ---- fixtures ----------------------------------------------------------------

.st_data <- function() as.data.frame(sochealth)

.st_lm <- function() stats::lm(bmi ~ age + sex, data = .st_data())

.st_glm <- function() {
  stats::glm(
    dentist_12m ~ age + sex,
    family = stats::binomial(),
    data = .st_data()
  )
}

.st_cox <- function() {
  survival::coxph(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung
  )
}

.st_mn <- function() {
  nnet::multinom(
    employment_status ~ age + sex,
    data = .st_data(),
    trace = FALSE
  )
}

# One model, one sample per sex: the subgroup stack.
.st_subgroups <- function() {
  d <- .st_data()
  women <- d[d$sex == "Female", ]
  men <- d[d$sex == "Male", ]
  list(
    Female = stats::lm(bmi ~ age + smoking, data = women),
    Male = stats::lm(bmi ~ age + smoking, data = men)
  )
}

# A hierarchy of least-squares fits on one outcome.
.st_hierarchy <- function() {
  list(
    stats::lm(mpg ~ wt, data = mtcars),
    stats::lm(mpg ~ wt + hp, data = mtcars),
    stats::lm(mpg ~ wt + hp + qsec, data = mtcars)
  )
}

# The builders of a list of models, each alone, with the same arguments.
.st_alone <- function(models, ...) {
  args <- list(...)
  lapply(models, function(m) {
    force(m)
    function(o = "default", ...) {
      .st_quiet(do.call(
        table_regression,
        c(list(m), args, list(output = o), list(...))
      ))
    }
  })
}


# ---- 1. table_stack(): one header, one note ----------------------------------

test_that("table_stack() stacks two tables under one header", {
  fits <- .st_subgroups()
  tf <- table_regression(fits$Female)
  tm <- table_regression(fits$Male)
  st <- table_stack(Female = tf, Male = tm)

  expect_identical(
    class(st),
    c("spicy_stack_table", "spicy_table", "data.frame")
  )
  # The panels are the tables passed, named by their labels.
  expect_identical(names(attr(st, "panels")), c("Female", "Male"))
  expect_identical(attr(st, "panels")[["Female"]], tf)
  expect_identical(attr(st, "panels")[["Male"]], tm)

  # A section row opens each panel, the panels sharing one header.
  sec <- attr(st, "section_rows")
  expect_equal(sec, c(1L, 2L + nrow(tf)))
  expect_identical(
    .st_console(st)[[1L]][sec],
    spicy_fmt("label_block_header", c("Female", "Male"))
  )
  expect_length(attr(st, "header_rows"), 0L)
  expect_equal(
    attr(st, "panel_rows"),
    list(
      Female = 1L + seq_len(nrow(tf)),
      Male = sec[[2L]] + seq_len(nrow(tm))
    )
  )
  expect_identical(nrow(st), 2L + nrow(tf) + nrow(tm))

  # A rule opens the second panel, and each panel keeps the rule above
  # its fit statistics. Every engine draws a rule above each row of the
  # two attributes alike, so the test reads their union.
  rules <- c(attr(st, "group_sep_rows"), attr(st, "section_sep_rows"))
  expect_true(sec[[2L]] %in% rules)
  own <- attr(st, "panel_rows")
  expect_true(all(
    c(
      own$Female[attr(tf, "group_sep_rows")],
      own$Male[attr(tm, "group_sep_rows")]
    ) %in%
      rules
  ))
  # No rule inside a panel that its table does not draw.
  inner <- unlist(lapply(
    list(list(own$Female, tf), list(own$Male, tm)),
    function(p) {
      p[[1L]][setdiff(
        seq_along(p[[1L]]),
        c(attr(p[[2L]], "group_sep_rows"), attr(p[[2L]], "section_sep_rows"))
      )]
    }
  ))
  expect_false(any(inner %in% rules))

  # The two tables share their title. Their note is the note of the two
  # models side by side: one family ("Linear regression models."), one
  # estimator, each said once.
  expect_identical(attr(st, "title"), attr(tf, "title"))
  expect_identical(attr(st, "note"), .st_side_note(unname(fits)))

  # The panel oracle runs on this stack in the next block, engines
  # included, outside the CRAN tier.

  # Printed: the title, one header line, a section row per panel, the
  # note; the stack itself comes back, invisibly.
  out <- utils::capture.output(res <- print(st))
  expect_identical(res, st)
  txt <- paste(out, collapse = "\n")
  expect_match(txt, attr(st, "title"), fixed = TRUE)
  expect_match(txt, substr(attr(st, "note"), 1L, 40L), fixed = TRUE)
  rows <- trimws(out[grepl(intToUtf8(0x2502L), out, fixed = TRUE)])
  for (lab in spicy_fmt("label_block_header", c("Female", "Male"))) {
    expect_identical(sum(startsWith(rows, lab)), 1L)
  }
  expect_identical(sum(startsWith(rows, spicy_str("header_variable"))), 1L)
})

test_that("table_stack() reaches every rich engine, each panel its table", {
  skip_on_cran()
  fits <- .st_subgroups()
  build_k <- .st_alone(fits)
  build_st <- function(o = "default", ...) {
    table_stack(
      Female = build_k$Female(),
      Male = build_k$Male(),
      output = o,
      ...
    )
  }
  st <- build_st()
  tabs <- lapply(build_k, function(b) b())
  .st_expect_oracle(st, tabs, build_st, build_k, engines = .st_all_engines)
})


# ---- 2. layout = "stacked": one panel per model ------------------------------

test_that("layout = 'stacked' gives one panel per model, the model alone", {
  models <- list(BMI = .st_lm(), Dentist = .st_glm())
  st <- table_regression(models, layout = "stacked")
  expect_s3_class(st, "spicy_stack_table")
  expect_identical(names(attr(st, "panels")), c("BMI", "Dentist"))
  # lm and glm print the same columns: one header.
  expect_length(attr(st, "header_rows"), 0L)
  # The title is the one the models side by side would carry.
  expect_identical(
    attr(st, "title"),
    attr(table_regression(models), "title")
  )
  tabs <- lapply(.st_alone(models), function(b) b())
  .st_expect_oracle(st, tabs)

  # The two doors build the same stack: the panels, their cells and the
  # merged note are those of table_stack() on the tables built alone.
  alt <- table_stack(!!!tabs)
  expect_identical(.st_console(st), .st_console(alt))
  expect_identical(attr(st, "note"), attr(alt, "note"))
  expect_identical(as_structured(st), as_structured(alt))
})

test_that("arguments reach every panel; a vcov list goes model by model", {
  skip_on_cran()
  f1 <- .st_lm()
  f2 <- stats::lm(wellbeing_score ~ age + sex, data = .st_data())
  # One `show_columns` for every panel.
  models <- list(A = f1, B = .st_glm())
  st <- table_regression(models, show_columns = c("b", "p"), layout = "stacked")
  .st_expect_oracle(
    st,
    lapply(.st_alone(models, show_columns = c("b", "p")), function(b) b())
  )
  # A list of vcov specifications, one per model.
  st2 <- table_regression(
    list(A = f1, B = f2),
    vcov = list("HC3", "classical"),
    layout = "stacked"
  )
  .st_expect_oracle(
    st2,
    list(table_regression(f1, vcov = "HC3"), table_regression(f2))
  )
})

test_that("a cluster vector keeps its name in every panel of the stack", {
  skip_on_cran()
  skip_if_not_installed("sandwich")
  # Complete cases: one cluster value per observation of both models.
  d <- stats::na.omit(
    .st_data()[, c("bmi", "wellbeing_score", "age", "sex", "region")]
  )
  f1 <- stats::lm(bmi ~ age + sex, data = d)
  f2 <- stats::lm(wellbeing_score ~ age + sex, data = d)
  st <- table_regression(
    list(A = f1, B = f2),
    vcov = "CR1",
    cluster = d$region,
    layout = "stacked"
  )
  tabs <- list(
    table_regression(f1, vcov = "CR1", cluster = d$region),
    table_regression(f2, vcov = "CR1", cluster = d$region)
  )
  .st_expect_oracle(st, tabs)
  # The note names the clusters as each table alone does, and as the two
  # models side by side do, once.
  expect_match(attr(tabs[[1L]], "note"), "region", fixed = TRUE)
  expect_identical(.st_count(attr(st, "note"), "region"), 1L)
  expect_identical(
    attr(st, "note"),
    .st_side_note(list(A = f1, B = f2), vcov = "CR1", cluster = d$region)
  )
})

test_that("one model with layout = 'stacked' is a stack of one panel", {
  skip_on_cran()
  f <- .st_lm()
  st <- table_regression(f, layout = "stacked")
  expect_s3_class(st, "spicy_stack_table")
  expect_length(attr(st, "panels"), 1L)
  .st_expect_oracle(st, list(table_regression(f)))
})

test_that("lm, glm and coxph stack under one header, in every engine", {
  skip_on_cran()
  skip_if_not_installed("survival")
  models <- list(BMI = .st_lm(), Dentist = .st_glm(), Survival = .st_cox())
  build_st <- function(o = "default", ...) {
    table_regression(models, layout = "stacked", output = o, ...)
  }
  build_k <- .st_alone(models)
  st <- build_st()
  tabs <- lapply(build_k, function(b) b())
  expect_identical(names(attr(st, "panels")), names(models))
  expect_length(attr(st, "header_rows"), 0L)
  .st_expect_oracle(st, tabs, build_st, build_k, engines = .st_all_engines)

  # One column, one decimal anchor: the decimal marks of the estimate
  # column sit at one position, whichever panel a row belongs to.
  cells <- as.character(st[[2L]])[unlist(attr(st, "panel_rows"))]
  dot <- regexpr(".", cells, fixed = TRUE)
  expect_length(unique(dot[dot > 0L]), 1L)
})


# ---- 3. a header per panel ---------------------------------------------------

test_that("panels with other columns open with a header of their own", {
  skip_on_cran()
  skip_if_not_installed("survival")
  models <- list(BMI = .st_lm(), Dentist = .st_glm(), Survival = .st_cox())
  build_st <- function(o = "default", ...) {
    table_regression(
      models,
      exponentiate = TRUE,
      layout = "stacked",
      output = o,
      ...
    )
  }
  build_k <- .st_alone(models, exponentiate = TRUE)
  # exp() applies to two of the three models: no warning, as side by side.
  expect_no_warning(st <- build_st(), class = "spicy_ignored_arg")
  tabs <- lapply(build_k, function(b) b())

  sec <- attr(st, "section_rows")
  hr <- attr(st, "header_rows")
  own <- attr(st, "panel_rows")
  # Panel 1's header heads the table; the OR and HR panels open with a
  # header of their own under their section row, as the header of their
  # table: the row of their labels, then the bounds of the interval.
  lab_rows <- sec[2:3] + 1L
  ci_rows <- sec[2:3] + 2L
  expect_equal(hr, sort(c(lab_rows, ci_rows)))
  expect_equal(c(own[[2L]][1L], own[[3L]][1L]), sec[2:3] + 3L)
  sc <- .st_console(st)
  expect_identical(sc[lab_rows, 2L], c("OR", "HR"))
  for (k in 2:3) {
    lab <- .st_labels(tabs[[k]])
    expect_identical(
      unlist(sc[lab_rows[[k - 1L]], 1L + seq_along(lab)], use.names = FALSE),
      lab
    )
  }
  # The interval label spans its two bounds in the engines' body, as in
  # the header of the panel's table.
  for (k in 2:3) {
    expect_identical(
      .st_spans_at(st, lab_rows[[k - 1L]]),
      list(as.integer(as_structured(tabs[[k]])$ci_pairs[[1L]]$cols))
    )
  }
  # The bounds: "LL" and "UL" under the interval (columns 4 and 5 of the
  # engines' body: Variable, OR, SE, LL, UL, p) and nothing elsewhere, in
  # the formatter every string engine reads, the clipboard, gt and Excel.
  # The console prints an interval in one column: the row is blank in its
  # body and left out when printed.
  bounds <- c("", "", "", "LL", "UL", "")
  ss <- .st_view(attr(st, "structured"))
  for (r in ci_rows) {
    expect_identical(unlist(ss[r, ], use.names = FALSE), bounds)
  }
  expect_true(.st_blank(unlist(sc[ci_rows, ], use.names = FALSE)))
  printed <- capture.output(print(st))
  expect_false(any(grepl("\\bLL\\b|\\bUL\\b", printed)))
  at <- grep("^ Dentist:", printed)
  expect_match(printed[[at + 1L]], "OR")
  expect_true(startsWith(printed[[at + 2L]], intToUtf8(0x254CL)))
  clip <- .st_clip_rows(st)
  first <- vapply(clip, function(r) if (length(r)) r[[1L]] else "", "")
  for (k in 2:3) {
    at <- which(first == st[[1L]][sec[[k]]])
    got <- clip[[at + 2L]][seq_along(bounds)]
    got[is.na(got)] <- ""
    expect_identical(got, bounds)
  }
  skip_if_not_installed("gt")
  g <- build_st("gt")
  gd <- .st_gt(g)
  for (r in ci_rows) {
    expect_identical(unlist(gd[r, ], use.names = FALSE), bounds)
  }
  skip_if_not_installed("openxlsx2")
  p <- withr::local_tempfile(fileext = ".xlsx")
  build_st("excel", excel_path = p)
  xs <- .st_xl_read(p)
  a <- .st_norm(xs$grid[, "A"])
  for (k in 2:3) {
    at <- which(a == .st_norm(st[[1L]][sec[[k]]]))
    got <- .st_norm(xs$grid[at + 2L, seq_along(bounds)])
    got[is.na(got)] <- ""
    expect_identical(unname(got), bounds)
  }

  .st_expect_oracle(st, tabs, build_st, build_k, engines = .st_all_engines)
})


# ---- 4. multinomial: alone in its panel, or beside other models --------------

test_that("a multinomial model alone in its panel keeps its categories", {
  skip_on_cran()
  skip_if_not_installed("nnet")
  models <- list(Health = .st_lm(), Employment = .st_mn())
  build_st <- function(o = "default", ...) {
    table_regression(models, layout = "stacked", output = o, ...)
  }
  build_k <- .st_alone(models)
  st <- build_st()
  tabs <- lapply(build_k, function(b) b())

  # Two header rows open the multinomial panel: its categories, then the
  # labels of their columns.
  sec <- attr(st, "section_rows")
  hr <- attr(st, "header_rows")
  expect_equal(hr, sec[[2L]] + 1:2)
  sc <- .st_console(st)
  expect_true(all(
    names(attr(tabs$Employment, "spanners")) %in%
      unlist(sc[hr[[1L]], ], use.names = FALSE)
  ))
  lab_mn <- .st_labels(tabs$Employment)
  expect_identical(
    unlist(sc[hr[[2L]], 1L + seq_along(lab_mn)], use.names = FALSE),
    lab_mn
  )
  # Each category spans its columns in the engines' body, as in the
  # header of the multinomial table.
  expect_setequal(
    .st_spans_at(st, hr[[1L]]),
    lapply(unname(as_structured(tabs$Employment)$spanners), as.integer)
  )
  # The top header is the first panel's, and the columns it does not have
  # are headed by nothing: their label is empty in the typed view, and
  # only gt, which would print the column key under an empty label,
  # writes a no-break space there.
  lab_lm <- .st_labels(tabs$Health)
  top <- unname(attr(st, "display_labels"))
  n_top <- 1L + length(lab_lm)
  expect_identical(top[seq_len(n_top)], c(spicy_str("header_variable"), lab_lm))
  expect_length(top, 1L + length(lab_mn))
  expect_true(.st_blank(top[-seq_len(n_top)]))
  vcols <- function(s) spicy:::.struct_value_cols(s$body)
  merged <- vcols(attr(st, "structured"))
  n_lm <- length(vcols(as_structured(tabs$Health)))
  n_extra <- length(merged) - n_lm
  expect_length(merged, length(vcols(as_structured(tabs$Employment))))
  meta <- attr(st, "structured")$col_meta
  expect_identical(
    unname(vapply(
      merged[-seq_len(n_lm)],
      function(k) meta[[k]]$display_label,
      character(1)
    )),
    rep("", n_extra)
  )
  # The written header: Excel and the clipboard leave those cells empty,
  # flextable writes empty labels; gt labels them with a no-break space.
  clip_top <- .st_clip_rows(st)[[1L]]
  extra <- length(clip_top) - n_extra + seq_len(n_extra)
  expect_identical(clip_top[extra], rep("", n_extra))
  if (requireNamespace("openxlsx2", quietly = TRUE)) {
    p <- withr::local_tempfile(fileext = ".xlsx")
    build_st("excel", excel_path = p)
    xs <- .st_xl_read(p)
    hdr_row <- which(xs$grid[, "A"] == spicy_str("header_variable"))[[1L]]
    cells <- xs$grid[hdr_row + 0:1, ncol(xs$grid) - n_extra + seq_len(n_extra)]
    expect_true(all(is.na(cells) | cells == ""))
  }
  if (requireNamespace("flextable", quietly = TRUE)) {
    ic <- flextable::information_data_chunk(build_st("flextable"))
    ic <- ic[ic$.part == "header", , drop = FALSE]
    expect_identical(unique(ic$txt[ic$.col_id %in% merged[-seq_len(n_lm)]]), "")
  }
  if (requireNamespace("gt", quietly = TRUE)) {
    sp <- build_st("gt")[["_spanners"]]
    lab <- vapply(
      sp$spanner_label[sp$spanner_level == 1L],
      function(l) paste(as.character(l), collapse = ""),
      ""
    )
    expect_identical(sum(lab == .st_nbsp), n_extra)
  }

  .st_expect_oracle(st, tabs, build_st, build_k, engines = .st_all_engines)

  # `nested = FALSE` spelled out builds the same stack.
  expect_identical(
    table_regression(models, layout = "stacked", nested = FALSE),
    st
  )
})

test_that("a multinomial first panel heads the table with its categories", {
  skip_on_cran()
  skip_if_not_installed("nnet")
  models <- list(Employment = .st_mn(), Health = .st_lm())
  st <- table_regression(models, layout = "stacked")
  tabs <- lapply(.st_alone(models), function(b) b())
  expect_identical(attr(st, "spanners"), attr(tabs$Employment, "spanners"))
  # The lm panel opens with its label row and the row of the bounds of
  # its interval: it has no categories.
  hr <- attr(st, "header_rows")
  expect_equal(hr, attr(st, "section_rows")[[2L]] + 1:2)
  lab_lm <- .st_labels(tabs$Health)
  expect_identical(
    unlist(
      .st_console(st)[hr[[1L]], 1L + seq_along(lab_lm)],
      use.names = FALSE
    ),
    lab_lm
  )
  .st_expect_oracle(st, tabs)
})

test_that("a multinomial model beside another keeps its long form", {
  skip_on_cran()
  skip_if_not_installed("nnet")
  f_lm <- .st_lm()
  f_mn <- .st_mn()
  build_k <- list(
    A = function(o = "default", ...) {
      table_regression(list(f_lm, f_mn), output = o, ...)
    },
    B = function(o = "default", ...) table_regression(f_lm, output = o, ...)
  )
  build_st <- function(o = "default", ...) {
    table_stack(A = build_k$A(), B = build_k$B(), output = o, ...)
  }
  st <- build_st()
  tabs <- lapply(build_k, function(b) b())
  # Panel A is the side-by-side table, one row per category and term.
  expect_true(
    "Student: age" %in% .st_console(st)[[1L]][attr(st, "panel_rows")$A]
  )
  # Panel B has other columns: its label row and the bounds of its
  # interval open it.
  expect_equal(attr(st, "header_rows"), attr(st, "section_rows")[[2L]] + 1:2)
  .st_expect_oracle(st, tabs, build_st, build_k, engines = .st_all_engines)
})

# The lines of a printed stack, cut into the chunks of a width split: each
# chunk opens on its column header ("Variable").
.st_chunks <- function(out) {
  starts <- grep("^ Variable ", out)
  blank <- which(!nzchar(out))
  ends <- vapply(starts, function(a) min(blank[blank > a]) - 1L, 1L)
  Map(function(a, b) out[a:b], starts, ends)
}

# The runs of a character in a printed line, as their first and last
# character positions.
.st_runs <- function(line, ch) {
  hit <- strsplit(line, "", fixed = TRUE)[[1L]] == ch
  r <- rle(hit)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  cbind(start = starts[r$values], end = ends[r$values])
}

test_that("a panel's column groups are drawn and ruled as in its table", {
  skip_on_cran()
  skip_if_not_installed("nnet")
  models <- list(Health = .st_lm(), Employment = .st_mn())
  build_st <- function(o = "default", ...) {
    table_regression(models, layout = "stacked", output = o, ...)
  }
  st <- build_st()
  # The groups: each outcome category but the reference, over its B, SE
  # and p (columns 2-4, 5-7 and 8-10 of the engines' body).
  cats <- levels(.st_data()$employment_status)[-1L]
  spans <- stats::setNames(list(2:4, 5:7, 8:10), cats)
  row <- attr(st, "header_rows")[[1L]]
  rules <- attr(st, "header_rules")
  expect_identical(lapply(rules, `[[`, "cols"), unname(spans))
  expect_true(all(vapply(rules, `[[`, 1L, "row") == row))

  # The console: each label centered over its columns, a rule under them,
  # as over the categories of the multinomial table alone.
  withr::local_options(width = 200)
  out <- capture.output(print(st))
  at <- grep(paste(cats, collapse = ".*"), out)
  expect_length(at, 1L)
  runs <- .st_runs(out[[at + 1L]], intToUtf8(0x2500L))
  expect_identical(nrow(runs), length(cats))
  for (k in seq_along(cats)) {
    pos <- regexpr(cats[[k]], out[[at]], fixed = TRUE)
    mid_label <- pos + (nchar(cats[[k]]) - 1L) / 2
    mid_run <- (runs[k, "start"] + runs[k, "end"]) / 2
    expect_lte(abs(mid_label - mid_run), 1)
  }
  # The labels leave the line of their cells: nothing else on it.
  rest <- out[[at]]
  for (cat in cats) {
    rest <- sub(cat, "", rest, fixed = TRUE)
  }
  expect_identical(trimws(gsub(intToUtf8(0x2502L), "", rest)), "")
  # Under the top header the same, from the multinomial table alone.
  alone <- capture.output(print(table_regression(models$Employment)))
  at1 <- grep(paste(cats, collapse = ".*"), alone)
  expect_identical(nrow(.st_runs(alone[[at1 + 1L]], intToUtf8(0x2500L))), 3L)

  # The rich engines draw the rule under each group's columns only.
  if (requireNamespace("gt", quietly = TRUE)) {
    g <- build_st("gt")
    s <- g[["_styles"]]
    nm <- names(g[["_data"]])
    for (cols in spans) {
      hit <- s[
        s$locname == "data" & s$rownum == row & s$colname %in% nm[cols],
      ]
      has_bottom <- vapply(
        hit$styles,
        function(x) {
          b <- x$cell_border_bottom
          !is.null(b) && identical(tolower(b$color), "#333333")
        },
        logical(1)
      )
      expect_setequal(hit$colname[has_bottom], nm[cols])
    }
  }
  if (requireNamespace("flextable", quietly = TRUE)) {
    bw <- build_st("flextable")$body$styles$cells$border.width.bottom$data
    expect_true(all(bw[row, unlist(spans)] > 0))
    expect_identical(unname(bw[row, 1L]), 0)
  }
  if (requireNamespace("tinytable", quietly = TRUE)) {
    calls <- build_st("tinytable")@lazy_style
    lines <- Filter(
      function(cl) identical(cl$line, "b") && identical(cl$i, row),
      calls
    )
    expect_setequal(
      lapply(lines, function(cl) as.integer(eval(cl$j))),
      unname(spans)
    )
  }
  if (requireNamespace("openxlsx2", quietly = TRUE)) {
    p <- withr::local_tempfile(fileext = ".xlsx")
    build_st("excel", excel_path = p)
    wb <- openxlsx2::wb_load(p)
    xs <- .st_xl_read(p)
    xr <- which(.st_norm(xs$grid[, "A"]) == "Employment:") + 1L
    xr <- rownames(xs$grid)[xr]
    cc <- wb$worksheets[[1L]]$sheet_data$cc
    bottom <- function(ref) {
      s <- cc$c_s[cc$r == ref]
      xf <- wb$styles_mgr$styles$cellXfs[[as.integer(s) + 1L]]
      id <- as.integer(sub(".*borderId=\"([0-9]+)\".*", "\\1", xf))
      grepl("<bottom style=", wb$styles_mgr$styles$borders[[id + 1L]])
    }
    letters_at <- LETTERS[unlist(spans)]
    expect_true(all(vapply(paste0(letters_at, xr), bottom, logical(1))))
    expect_false(bottom(paste0("A", xr)))
  }
})

test_that("a panel's column groups keep their place with wide or combining names", {
  skip_on_cran()
  # The console places a group label by console column, in the header and
  # in a panel's own header row alike: a wide character (CJK) or a
  # decomposed accent in a model or a panel name moves no label. Names of
  # the same display width as the reference start where its names start.
  d <- sochealth
  f1 <- lm(bmi ~ age + sex, data = d)
  f2 <- lm(bmi ~ age, data = d)
  g1 <- glm(dentist_12m ~ age + sex, data = d, family = binomial)
  g2 <- glm(dentist_12m ~ age, data = d, family = binomial)
  col_of <- function(line, lbl) {
    nchar(
      substr(line, 1L, regexpr(lbl, line, fixed = TRUE) - 1L),
      type = "width"
    )
  }
  layout_of <- function(m1, m2, p1, p2) {
    st <- do.call(
      table_stack,
      stats::setNames(
        list(
          table_regression(stats::setNames(list(f1, f2), c(m1, m2))),
          table_regression(
            stats::setNames(list(g1, g2), c(m1, m2)),
            exponentiate = TRUE
          )
        ),
        c(p1, p2)
      )
    )
    out <- capture.output(print(st))
    at <- which(grepl(m1, out, fixed = TRUE))
    expect_length(at, 2L)
    c(
      vapply(out[at], col_of, 1L, lbl = m1, USE.NAMES = FALSE),
      vapply(out[at], col_of, 1L, lbl = m2, USE.NAMES = FALSE),
      nchar(out[at], type = "width"),
      nchar(out[at + 1L], type = "width")
    )
  }
  withr::local_options(width = 200)
  ref <- layout_of("ModelA", "ModelB", "BMI", "Dentist")
  expect_identical(
    layout_of(
      "\u6a21\u578b\u7532",
      "\u6a21\u578b\u4e59",
      "\u4f53\u91cd",
      "\u7259\u79d1"
    ),
    ref
  )
  expect_identical(
    layout_of("Mode\u0300lA", "Mode\u0300lB", "BMI\u0301", "De\u0301ntist"),
    ref
  )
})

test_that("an interval label in a panel's header is ruled over its bounds", {
  skip_on_cran()
  models <- list(BMI = .st_lm(), Dentist = .st_glm())
  st <- table_regression(models, exponentiate = TRUE, layout = "stacked")
  lab_row <- attr(st, "section_rows")[[2L]] + 1L
  ci <- as.integer(
    as_structured(attr(st, "panels")$Dentist)$ci_pairs[[1L]]$cols
  )
  expect_identical(ci, 4:5)
  rules <- attr(st, "header_rules")
  expect_identical(rules, list(list(row = lab_row, cols = ci, kind = "ci")))
  skip_if_not_installed("flextable")
  bw <- table_regression(
    models,
    exponentiate = TRUE,
    layout = "stacked",
    output = "flextable"
  )$body$styles$cells$border.width.bottom$data
  expect_true(all(bw[lab_row, ci] > 0))
  expect_true(all(bw[lab_row, -ci] == 0))
})

test_that("panels with other decimals keep their own padding in the console", {
  skip_on_cran()
  d <- .st_data()
  fit <- stats::lm(bmi ~ age + sex, data = d)
  t1 <- table_regression(fit)
  t2 <- table_regression(
    stats::glm(dentist_12m ~ age + sex, family = stats::binomial(), data = d),
    digits = 3
  )
  st <- table_stack(A = t1, B = t2)
  own <- attr(st, "panel_rows")
  # Each panel's cells are its table's, padding included: the bounds of an
  # interval are never padded inside its brackets.
  for (k in 1:2) {
    tab <- list(t1, t2)[[k]]
    got <- structure(st, class = "data.frame")[own[[k]], seq_along(tab)]
    expect_identical(
      unname(lapply(got, as.character)),
      unname(lapply(structure(tab, class = "data.frame"), as.character))
    )
  }
  ci <- unlist(structure(st, class = "data.frame")[unlist(own), 4L])
  expect_false(any(grepl(" ,| \\]", ci)))
  # The interval of the intercept, from the model.
  b <- stats::confint(fit)[1L, ]
  expect_identical(
    trimws(ci[[1L]]),
    sprintf("[%.2f, %.2f]", b[[1L]], b[[2L]])
  )
})

test_that("a stack split by width repeats a group and drops empty rows", {
  skip_on_cran()
  skip_if_not_installed("nnet")
  st <- table_regression(
    list(Health = .st_lm(), Employment = .st_mn()),
    layout = "stacked"
  )
  cats <- levels(.st_data()$employment_status)[-1L]
  withr::local_options(width = 60)
  chunks <- .st_chunks(capture.output(print(st)))
  expect_length(chunks, 2L)
  second <- chunks[[2L]]
  # The lm panel has its four columns in the first chunk: neither its rows
  # nor the fit statistics of the multinomial panel, whose values sit in
  # its first column, return in the second.
  expect_false(any(grepl("Health:", second, fixed = TRUE)))
  expect_false(any(grepl("^ (n|AIC|Adj\\.) ", second)))
  rows <- second[grepl(intToUtf8(0x2502L), second, fixed = TRUE)]
  labels <- trimws(sub(paste0(intToUtf8(0x2502L), ".*"), "", rows))
  expect_identical(
    labels[nzchar(labels)],
    c(
      "Variable",
      "Employment:",
      "(Intercept)",
      "age",
      "sex:",
      "Female (ref.)",
      "Male"
    )
  )
  # Every column of the second chunk sits under the rule of its group,
  # the group cut by the split included, its label repeated over it.
  at <- which(startsWith(trimws(second), "Employment:")) + 1L
  rule <- .st_runs(second[[at + 1L]], intToUtf8(0x2500L))
  head_line <- second[[at + 2L]]
  bar <- regexpr(intToUtf8(0x2502L), head_line)
  chars <- strsplit(head_line, "", fixed = TRUE)[[1L]]
  filled <- which(chars != " " & seq_along(chars) > bar)
  covered <- unlist(lapply(seq_len(nrow(rule)), function(i) {
    rule[i, "start"]:rule[i, "end"]
  }))
  expect_true(all(filled %in% covered))
  expect_true(any(grepl(cats[[2L]], second[[at]], fixed = TRUE)))
  expect_true(any(grepl(cats[[3L]], second[[at]], fixed = TRUE)))
})

# ---- 5. nested = TRUE, stacked -----------------------------------------------

test_that("nested = TRUE writes each change into the panel of its model", {
  skip_on_cran()
  m <- .st_hierarchy()
  side <- table_regression(m, nested = TRUE)
  st <- table_regression(m, nested = TRUE, layout = "stacked")
  labs <- spicy_fmt("label_model_name", 1:3)
  expect_identical(names(attr(st, "panels")), labs)
  expect_identical(attr(st, "title"), attr(side, "title"))

  tokens <- c("r2_change", "f_change", "p_change")
  change <- c(
    spicy:::.NESTED_CHANGE_TOKENS,
    "wald_f_change",
    "wald_chi2_change"
  )
  s_side <- as_structured(side)
  i_side <- match(tokens, s_side$body$.variable)
  base <- .st_norm(s_side$body$Variable[i_side])
  fs <- attr(side, "spicy_fit_stats")
  side_c <- .st_console(side)
  s <- as_structured(st)
  sc <- .st_console(st)
  own <- attr(st, "panel_rows")

  # Panel 1 compares with nothing: it is the first model alone.
  t1 <- table_regression(m[[1L]])
  expect_false(any(s[[1L]]$body$.variable %in% change))
  .st_expect_panel(sc, .st_console(t1), own[[1L]], "panel 1")
  expect_identical(s[[1L]], as_structured(t1))

  for (k in 2:3) {
    info <- sprintf("panel %d", k)
    b <- s[[k]]$body
    i <- match(tokens, b$.variable)
    expect_false(anyNA(i), info = info)
    if (anyNA(i)) {
      next
    }
    expect_identical(b$.row_role[i], rep("fit_stat", 3L), info = info)
    # Labeled against the panel before it, in the typed view and printed.
    vs <- spicy_fmt("fitstat_change_vs", base, labs[[k - 1L]])
    expect_identical(.st_norm(b$Variable[i]), vs, info = info)
    expect_identical(sc[own[[k]][i], 1L], vs, info = info)
    # The values of the side-by-side table, typed and printed.
    v <- spicy:::.struct_value_cols(b)[[1L]]
    expect_equal(
      b[[v]][i],
      c(fs$r2_change[[k]], fs$f_change[[k]], fs$p_change[[k]]),
      tolerance = 1e-10,
      info = info
    )
    expect_identical(
      sc[own[[k]][i], 2L],
      side_c[i_side, paste0("Model ", k, ": B")],
      info = info
    )
    # Without its change rows, panel k is model k alone.
    keep <- !(b$.variable %in% change & b$.row_role == "fit_stat")
    tk <- .st_console(table_regression(m[[k]]))
    got <- sc[own[[k]][keep], seq_len(ncol(tk)), drop = FALSE]
    expect_equal(.st_cells(got), .st_cells(tk), info = info)
  }

  # The change is labeled with the name of the panel before it.
  named <- table_regression(
    list(Base = m[[1L]], `Plus hp` = m[[2L]]),
    nested = TRUE,
    layout = "stacked"
  )
  b2 <- as_structured(named)[[2L]]$body
  expect_identical(
    .st_norm(b2$Variable[match(tokens, b2$.variable)]),
    spicy_fmt("fitstat_change_vs", base, "Base")
  )
})

test_that("a stacked glm hierarchy reports its likelihood-ratio changes", {
  skip_on_cran()
  m <- list(
    stats::glm(am ~ wt, family = stats::binomial(), data = mtcars),
    stats::glm(am ~ wt + hp, family = stats::binomial(), data = mtcars)
  )
  side <- table_regression(m, nested = TRUE)
  st <- table_regression(m, nested = TRUE, layout = "stacked")
  expect_identical(attr(st, "title"), attr(side, "title"))
  tokens <- c("lrt_change", "p_change")
  fs <- attr(side, "spicy_fit_stats")
  b <- as_structured(st)[[2L]]$body
  i <- match(tokens, b$.variable)
  expect_false(anyNA(i))
  v <- spicy:::.struct_value_cols(b)[[1L]]
  expect_equal(
    b[[v]][i],
    c(fs$lrt_change[[2L]], fs$p_change[[2L]]),
    tolerance = 1e-10
  )
  s_side <- as_structured(side)
  base <- .st_norm(s_side$body$Variable[match(tokens, s_side$body$.variable)])
  expect_identical(
    .st_norm(b$Variable[i]),
    spicy_fmt("fitstat_change_vs", base, spicy_fmt("label_model_name", 1L))
  )
  expect_false(any(as_structured(st)[[1L]]$body$.variable %in% tokens))
})

test_that("a stacked hierarchy refuses a class with no nested test", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  m <- list(
    MASS::rlm(mpg ~ wt, data = mtcars),
    MASS::rlm(mpg ~ wt + hp, data = mtcars)
  )
  expect_error(
    table_regression(m, nested = TRUE, layout = "stacked"),
    class = "spicy_invalid_input"
  )
})

test_that("a stacked mixed hierarchy compares maximum-likelihood refits", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  m <- list(
    lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy),
    lme4::lmer(
      Reaction ~ Days + I(Days^2) + (1 | Subject),
      data = lme4::sleepstudy
    )
  )
  side <- suppressMessages(table_regression(m, nested = TRUE))
  st <- suppressMessages(table_regression(m, nested = TRUE, layout = "stacked"))
  # The change rows of panel 2 carry the side-by-side values, computed
  # from the refits of both models.
  fs <- attr(side, "spicy_fit_stats")
  b <- as_structured(st)[[2L]]$body
  tokens <- c("aic_change", "bic_change", "lrt_change", "p_change")
  i <- match(tokens, b$.variable)
  expect_false(anyNA(i))
  v <- spicy:::.struct_value_cols(b)[[1L]]
  expect_equal(
    b[[v]][i],
    vapply(tokens, function(tk) fs[[tk]][[2L]], numeric(1), USE.NAMES = FALSE),
    tolerance = 1e-8
  )
  # And the note says how, as the side-by-side note does.
  side_note <- attr(side, "note")
  refit <- regmatches(
    side_note,
    regexpr("[^.]*maximum-likelihood refits[^.]*\\.", side_note)
  )
  expect_length(refit, 1L)
  expect_match(attr(st, "note"), trimws(refit), fixed = TRUE)
})


# ---- 5b. the messages of a stacked layout ------------------------------------

# The oracle of the messages: for the same call, a stacked table signals
# what the side-by-side table signals, class and text -- a model named by
# its name or position in the list, a message about several models given
# once with all of them, a message about the arguments given once.
.st_expect_same_conditions <- function(models, ..., info = "") {
  side <- .st_conditions(table_regression(models, ...))
  stacked <- .st_conditions(table_regression(models, ..., layout = "stacked"))
  expect_identical(stacked, side, info = info)
  invisible(stacked)
}

test_that("a stacked table names each model as the side-by-side table does", {
  skip_on_cran()
  d <- .st_data()
  f_lm <- .st_lm()
  f_gg <- stats::glm(bmi ~ age, family = stats::gaussian(), data = d)
  # A message about one model: its position in the list, not "Model 1".
  got <- .st_expect_same_conditions(list(LM = f_lm, GG = f_gg), info = "gg")
  expect_match(got, "Model 2 is a gaussian / identity glm.", fixed = TRUE)
  .st_expect_same_conditions(list(f_lm, f_gg), info = "gg unnamed")
  # A message about several models: once, with every one of them.
  ga <- .st_glm()
  gb <- stats::glm(dentist_12m ~ sex, family = stats::binomial(), data = d)
  got <- .st_expect_same_conditions(
    list(A = ga, B = gb),
    ci_method = "profile",
    vcov = "HC3",
    info = "profile"
  )
  expect_length(got, 1L)
  expect_match(got, "`vcov`: A, B.", fixed = TRUE)
  # A message about the arguments: once.
  got <- .st_expect_same_conditions(
    list(f_lm, ga),
    show_intercept = FALSE,
    intercept_position = "last",
    info = "Q2"
  )
  expect_length(got, 1L)
  # A message, too: the reference levels shown nowhere, said once.
  got <- .st_expect_same_conditions(
    list(f_lm, ga),
    reference_style = "none",
    factor_layout = "flat",
    info = "silent reference"
  )
  expect_length(got, 1L)
  expect_match(got, "^spicy_silent_reference:")
  # `boot_n` that no model uses warns once; one that a model uses, never.
  got <- .st_expect_same_conditions(list(f_lm, ga), boot_n = 50, info = "boot")
  expect_length(got, 1L)
  skip_if_not_installed("sandwich")
  expect_identical(
    .st_conditions(table_regression(
      list(f_lm, f_lm),
      vcov = list("bootstrap", "classical"),
      boot_n = 20,
      layout = "stacked"
    )),
    character(0)
  )
  # A `cluster` the estimator ignores: once per model, numbered.
  got <- .st_expect_same_conditions(
    list(f_lm, ga),
    cluster = ~region,
    info = "cluster"
  )
  expect_length(got, 2L)
  # A warning a model gives in words that do not name it: once per
  # panel, as once per model side by side -- one panel never hides
  # another.
  got <- .st_expect_same_conditions(
    list(
      stats::lm(bmi ~ log(age) + sex, data = d),
      stats::lm(wellbeing_score ~ log(age) + sex, data = d)
    ),
    standardized = "refit",
    info = "same words"
  )
  expect_identical(sum(grepl("^spicy_fallback:", got)), 2L)
  # Per-model lines of one warning, numbered in the list.
  .st_expect_same_conditions(
    list(
      stats::lm(bmi ~ age * sex, data = d),
      f_lm,
      stats::lm(bmi ~ age * smoking, data = d)
    ),
    standardized = "refit",
    info = "standardized caveat"
  )
  # An error about one model.
  skip_if_not_installed("survival")
  got <- .st_expect_same_conditions(
    list(f_lm, .st_cox()),
    standardized = "refit",
    info = "standardized error"
  )
  expect_match(got, "model 2 (class coxph)", fixed = TRUE)
})

test_that("singular fits and empty models are named together, once", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("MASS")
  d <- .st_data()
  # lme4's once-a-session note about its p-values, out of the way.
  l1 <- suppressMessages(lme4::lmer(bmi ~ age + (1 | region), data = d))
  l2 <- suppressMessages(lme4::lmer(bmi ~ age + sex + (1 | region), data = d))
  suppressMessages(suppressWarnings(table_regression(l1)))
  got <- .st_expect_same_conditions(
    list(l1, l2),
    nested = TRUE,
    info = "singular"
  )
  expect_identical(sum(grepl("^spicy_caveat: Singular fit", got)), 1L)
  expect_match(
    got,
    "Singular fit (Model 1, Model 2)",
    fixed = TRUE,
    all = FALSE
  )
  g1 <- suppressMessages(lme4::glmer(
    dentist_12m ~ age + sex + (1 | region),
    data = d,
    family = stats::binomial()
  ))
  got <- .st_expect_same_conditions(
    list(LM = .st_lm(), GLMER = g1),
    info = "glmer"
  )
  expect_match(got, "Singular fit (GLMER)", fixed = TRUE, all = FALSE)
  # A model with no row: the error names every such model.
  p0 <- MASS::polr(self_rated_health ~ 1, data = d, Hess = TRUE)
  got <- .st_expect_same_conditions(
    list(LM = .st_lm(), P0 = p0),
    show_thresholds = FALSE,
    info = "empty"
  )
  expect_match(got, "^spicy_empty_table: P0 has no rows to show.")
  got <- .st_expect_same_conditions(
    list(P1 = p0, P0 = p0),
    show_thresholds = FALSE,
    info = "empty x2"
  )
  expect_match(got, "^spicy_empty_table: P1, P0 have no rows to show.")
})

test_that("keep, drop: an emptied panel stops; patterns checked once", {
  skip_on_cran()
  d <- .st_data()
  f_lm <- .st_lm()
  f_sex <- stats::glm(dentist_12m ~ sex, family = stats::binomial(), data = d)
  # A pattern that matches in one panel only: no warning, as side by side.
  expect_no_warning(
    st <- table_regression(list(f_lm, f_sex), keep = "age", layout = "stacked")
  )
  expect_true("age" %in% as_structured(st)[[1L]]$body$.variable)
  # A pattern that matches no model: one warning, the one of the columns.
  .st_expect_same_conditions(list(f_lm, f_sex), keep = "zzz", info = "zzz")
  w <- .st_conditions(table_regression(
    list(f_lm, f_sex),
    drop = "zzz",
    layout = "stacked"
  ))
  expect_length(w, 1L)
  expect_match(w, "^spicy_no_match:")
  # A panel the row selection empties is no table: the error names it.
  e <- expect_error(
    table_regression(
      list(A = f_lm, B = f_sex),
      drop = "sex",
      show_intercept = FALSE,
      layout = "stacked"
    ),
    class = "spicy_empty_table"
  )
  expect_match(conditionMessage(e), "B has no rows to show.", fixed = TRUE)
  expect_match(conditionMessage(e), "`drop` and `show_intercept = FALSE`")
  expect_match(conditionMessage(e), "stacked table", fixed = TRUE)
  skip_if_not_installed("survival")
  cox <- survival::coxph(
    survival::Surv(time, status) ~ sex,
    data = survival::lung
  )
  e <- expect_error(
    table_regression(
      list(LM = stats::lm(mpg ~ wt + hp, data = mtcars), COX = cox),
      keep = "wt",
      layout = "stacked"
    ),
    class = "spicy_empty_table"
  )
  expect_match(conditionMessage(e), "COX has no rows to show.", fixed = TRUE)
  # Both panels emptied: one error, both named, after the warning about a
  # pattern no model matches.
  expect_warning(
    e <- expect_error(
      table_regression(
        list(A = cox, B = cox),
        keep = "wt",
        layout = "stacked"
      ),
      class = "spicy_empty_table"
    ),
    class = "spicy_no_match"
  )
  expect_match(conditionMessage(e), "A, B have no rows to show.", fixed = TRUE)
})


# ---- 6. one language, one decimal mark, one style ----------------------------

test_that("panels built in another language, mark or style are refused", {
  f <- .st_lm()
  t_en <- table_regression(f)
  t_fr <- withr::with_options(
    list(spicy.language = "fr"),
    table_regression(f)
  )
  t_dm <- table_regression(f, decimal_mark = ",")
  t_jama <- table_regression(f, style = "jama")
  # Every table records the language it was built in.
  expect_identical(attr(t_en, "language"), "en")
  expect_identical(attr(t_fr, "language"), "fr")

  msg <- character(0)
  for (other in list(t_fr, t_dm, t_jama)) {
    expect_error(
      table_stack(A = t_en, B = other),
      class = "spicy_stack_mismatch"
    )
    e <- expect_error(
      table_stack(A = t_en, B = other),
      class = "spicy_invalid_input"
    )
    msg <- c(msg, conditionMessage(e))
  }
  # Each message names what differs, and the panel.
  expect_match(msg[[1L]], "\\bfr\\b")
  expect_match(msg[[2L]], "decimal", ignore.case = TRUE)
  expect_match(msg[[3L]], "style", ignore.case = TRUE)
  expect_identical(anyDuplicated(msg), 0L)
  for (x in msg) {
    expect_match(x, "panel 2", ignore.case = TRUE)
  }
  # Whichever panel comes first.
  expect_error(table_stack(A = t_dm, B = t_en), class = "spicy_stack_mismatch")

  # The alignment: a centered panel would be printed decimal-aligned.
  t_center <- table_regression(f, align = "center")
  e <- expect_error(
    table_stack(A = t_en, B = t_center),
    class = "spicy_stack_mismatch"
  )
  expect_match(conditionMessage(e), "`align`", fixed = TRUE)
  expect_match(conditionMessage(e), "\"decimal\"", fixed = TRUE)
  expect_match(conditionMessage(e), "\"center\"", fixed = TRUE)
})

test_that("styles that print alike stack; differing levers are named", {
  skip_on_cran()
  f <- .st_lm()
  g <- .st_glm()
  # No style and "apa" print the same table: they stack.
  t_def <- table_regression(f)
  t_apa <- table_regression(g, style = "apa")
  expect_s3_class(table_stack(A = t_def, B = t_apa), "spicy_stack_table")
  # One theme, `p_digits` typed for one table only: the p-values of that
  # table follow `p_digits`, the other's the theme's, and the stack says
  # which lever differs and which argument sets it.
  t_jama <- table_regression(f, style = "jama")
  t_jama3 <- table_regression(g, style = "jama", p_digits = 3)
  e <- expect_error(
    table_stack(A = t_jama, B = t_jama3),
    class = "spicy_stack_mismatch"
  )
  msg <- conditionMessage(e)
  expect_match(msg, "p-value format: panel 1", fixed = TRUE)
  expect_match(msg, "panel 2 the decimals of `p_digits`", fixed = TRUE)
  expect_match(msg, "`style`", fixed = TRUE)
  expect_match(msg, "`p_digits`", fixed = TRUE)
  # `p_digits` typed for both: one format.
  expect_s3_class(
    table_stack(
      A = table_regression(f, style = "jama", p_digits = 3),
      B = t_jama3
    ),
    "spicy_stack_table"
  )
  # Two styles: each lever that differs is named, with both values.
  e <- expect_error(
    table_stack(A = t_def, B = t_jama),
    class = "spicy_stack_mismatch"
  )
  msg <- conditionMessage(e)
  expect_match(
    msg,
    "note lead-in: panel 1 \"Note.\", panel 2 \"\"",
    fixed = TRUE
  )
  expect_match(
    msg,
    "abbreviation rule: panel 1 \"apa\", panel 2 \"jama\"",
    fixed = TRUE
  )
  expect_false(grepl("interval separator", msg, fixed = TRUE))
  # A theme of significant figures (its mid-dot decimal mark set aside),
  # and a lever set by hand.
  msg <- conditionMessage(expect_error(
    table_stack(
      A = t_def,
      B = table_regression(g, style = "lancet", decimal_mark = ".")
    ),
    class = "spicy_stack_mismatch"
  ))
  expect_match(msg, "2 significant figures", fixed = TRUE)
  msg <- conditionMessage(expect_error(
    table_stack(
      A = t_def,
      B = table_regression(g, style = spicy_style(abbreviations = "all"))
    ),
    class = "spicy_stack_mismatch"
  ))
  expect_match(
    msg,
    "abbreviations: panel 1 those of its rule, panel 2 \"all\"",
    fixed = TRUE
  )
})

test_that("two French panels stack in French", {
  fits <- .st_subgroups()
  fr <- withr::with_options(
    list(spicy.language = "fr"),
    lapply(fits, table_regression)
  )
  # Stacked from an English session: the stack speaks the panels'
  # language.
  st <- table_stack(Femmes = fr$Female, Hommes = fr$Male)
  sec_fr <- withr::with_options(
    list(spicy.language = "fr"),
    spicy_fmt("label_block_header", c("Femmes", "Hommes"))
  )
  expect_identical(.st_console(st)[[1L]][attr(st, "section_rows")], sec_fr)
  # The French note of the two models side by side.
  expect_identical(
    attr(st, "note"),
    withr::with_options(
      list(spicy.language = "fr"),
      .st_side_note(unname(fits))
    )
  )
  expect_false(identical(attr(st, "note"), .st_side_note(unname(fits))))
  .st_expect_oracle(st, unname(fr))
})

test_that("layout = 'stacked' built in French labels its panels in French", {
  skip_on_cran()
  fits <- .st_subgroups()
  st <- withr::with_options(
    list(spicy.language = "fr"),
    table_regression(
      list(Femmes = fits$Female, Hommes = fits$Male),
      layout = "stacked"
    )
  )
  sec_fr <- withr::with_options(
    list(spicy.language = "fr"),
    spicy_fmt("label_block_header", c("Femmes", "Hommes"))
  )
  expect_identical(.st_console(st)[[1L]][attr(st, "section_rows")], sec_fr)
  expect_identical(attr(attr(st, "panels")[[1L]], "language"), "fr")
})

test_that("a French stack of two families is French, title and note", {
  skip_on_cran()
  fr <- function(expr) withr::with_options(list(spicy.language = "fr"), expr)
  models <- list(IMC = .st_lm(), Dentiste = .st_glm())
  st <- fr(table_regression(models, layout = "stacked"))
  # The generic French title, not the English one the side-by-side table
  # keeps over its English list of families (decision 42).
  expect_identical(
    attr(st, "title"),
    fr(spicy_fmt(
      "title_regression_comparison",
      spicy:::.title_prefix_display("Regression")
    ))
  )
  expect_false(identical(attr(st, "title"), "Regression comparison"))
  # Each panel gives its type in French, in a sentence of its own, in
  # lower case after the label, as side by side.
  types <- vapply(
    models,
    function(m) .st_parts(fr(table_regression(m)))$type,
    character(1)
  )
  for (k in seq_along(types)) {
    expect_match(
      attr(st, "note"),
      fr(spicy_fmt(
        "note_model_line",
        names(models)[[k]],
        spicy:::lowercase_first(types[[k]])
      )),
      fixed = TRUE
    )
  }
})


# ---- 7. naming the panels ----------------------------------------------------

test_that("panels take names, labels, titles or Panel k; the stack a title", {
  skip_on_cran()
  t1 <- table_regression(.st_lm())
  t2 <- table_regression(.st_glm())
  lab <- function(st) names(attr(st, "panels"))
  expect_identical(lab(table_stack(A = t1, B = t2)), c("A", "B"))
  expect_identical(
    lab(table_stack(t1, t2, panel_labels = c("L1", "L2"))),
    c("L1", "L2")
  )
  # A name in `...` wins over `panel_labels`, panel by panel, and the
  # label it replaces is named in a warning.
  w <- expect_warning(
    st <- table_stack(A = t1, t2, panel_labels = c("L1", "L2")),
    class = "spicy_ignored_arg"
  )
  expect_identical(lab(st), c("A", "L2"))
  expect_match(conditionMessage(w), "\"L1\"", fixed = TRUE)
  expect_false(grepl("\"L2\"", conditionMessage(w), fixed = TRUE))
  # Every table named: `panel_labels` serves no panel, and says so.
  w <- expect_warning(
    st <- table_stack(A = t1, B = t2, panel_labels = c("X", "Y")),
    class = "spicy_ignored_arg"
  )
  expect_identical(lab(st), c("A", "B"))
  expect_match(conditionMessage(w), "every table is named", fixed = TRUE)
  # A named vector is a variable-label slip (`labels` of the table_*
  # builders): refused, with the way to name the panels.
  e <- expect_error(
    table_stack(t1, t2, panel_labels = c(age = "Age", sex = "Sex")),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(e), "table_stack(Women = t1", fixed = TRUE)

  # The title spicy builds describes the model, not the panel: tables of
  # different outcomes are labeled by their outcome, as the stacked
  # layout labels them...
  expect_false(isTRUE(attr(t1, "spicy_title_typed")))
  expect_identical(lab(table_stack(t1, t2)), c("bmi", "dentist_12m"))
  t2_untitled <- table_regression(.st_glm(), title = FALSE)
  expect_null(attr(t2_untitled, "title"))
  expect_identical(
    lab(table_stack(t1, t2_untitled)),
    c("bmi", "dentist_12m")
  )
  # ... by their outcome as the table names it...
  t1_lab <- table_regression(.st_lm(), labels = c(bmi = "Body mass index"))
  expect_identical(
    lab(table_stack(t1_lab, t2)),
    c("Body mass index", "dentist_12m")
  )
  # ... by a title typed for them...
  t1_typed <- table_regression(.st_lm(), title = "Body mass")
  expect_true(attr(t1_typed, "spicy_title_typed"))
  expect_identical(
    lab(table_stack(t1_typed, t2)),
    c("Body mass", "dentist_12m")
  )
  # ... else by their position: tables of one outcome.
  fits <- .st_subgroups()
  tf <- table_regression(fits$Female)
  tm <- table_regression(fits$Male)
  expect_identical(
    lab(table_stack(tf, tm)),
    spicy_fmt("label_panel_name", 1:2)
  )
  # A label is not closed twice: "Women:" stays "Women:".
  st <- table_stack(`Women:` = tf, `Men:` = tm)
  expect_identical(
    .st_console(st)[[1L]][attr(st, "section_rows")],
    c("Women:", "Men:")
  )
  # A list of tables splices.
  expect_identical(
    table_stack(!!!list(A = t1, B = t2)),
    table_stack(A = t1, B = t2)
  )

  # Two panels cannot share a label; the error says how to tell them
  # apart.
  e <- expect_error(
    table_stack(t1_typed, t1_typed),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(e), "name", ignore.case = TRUE)
  expect_error(table_stack(A = t1, A = t2), class = "spicy_invalid_input")
  expect_error(
    table_stack(t1, t2, panel_labels = c("X", "X")),
    class = "spicy_invalid_input"
  )
  # `panel_labels`: one non-empty string per table.
  for (bad in list("A", c("A", "B", "C"), c("A", NA), c("A", ""), 1:2)) {
    expect_error(
      table_stack(t1, t2, panel_labels = bad),
      class = "spicy_invalid_input"
    )
  }

  # The title: the panels' common one, else none; FALSE removes it, a
  # string sets it.
  expect_null(attr(table_stack(A = t1, B = t2), "title"))
  expect_identical(
    attr(table_stack(A = t1, B = t2, title = "Two models"), "title"),
    "Two models"
  )
  expect_identical(
    attr(table_stack(A = tf, B = tm), "title"),
    attr(tf, "title")
  )
  expect_null(attr(table_stack(A = tf, B = tm, title = FALSE), "title"))
  expect_error(table_stack(A = t1, title = 1), class = "spicy_invalid_input")
})

test_that("layout = 'stacked' names panels as the column groups are named", {
  skip_on_cran()
  f_lm <- .st_lm()
  f_glm <- .st_glm()
  lab <- function(st) names(attr(st, "panels"))
  # model_labels, over names (with the warning of the side-by-side table).
  expect_warning(
    st <- table_regression(
      list(X = f_lm, Y = f_glm),
      model_labels = c("A", "B"),
      layout = "stacked"
    ),
    class = "spicy_ignored_arg"
  )
  expect_identical(lab(st), c("A", "B"))
  expect_identical(
    .st_console(st)[[1L]][attr(st, "section_rows")],
    spicy_fmt("label_block_header", c("A", "B"))
  )
  # Names, an unnamed slot filled as side by side.
  expect_identical(
    lab(table_regression(list(X = f_lm, Y = f_glm), layout = "stacked")),
    c("X", "Y")
  )
  expect_identical(
    lab(table_regression(list(X = f_lm, f_glm), layout = "stacked")),
    c("X", spicy_fmt("label_model_name", 2L))
  )
  # Distinct outcomes name their panels as they head the column groups.
  expect_identical(
    lab(table_regression(list(f_lm, f_glm), layout = "stacked")),
    names(attr(table_regression(list(f_lm, f_glm)), "spanners"))
  )
  expect_identical(
    lab(table_regression(list(f_lm, f_glm), labels = TRUE, layout = "stacked")),
    names(attr(table_regression(list(f_lm, f_glm), labels = TRUE), "spanners"))
  )
  # One outcome: "Model k".
  expect_identical(
    lab(table_regression(.st_hierarchy()[1:2], layout = "stacked")),
    spicy_fmt("label_model_name", 1:2)
  )
  # A typed name met by an auto-filled one.
  expect_error(
    table_regression(list("Model 2" = f_lm, f_glm), layout = "stacked"),
    class = "spicy_invalid_input"
  )
})


# ---- 8. inputs ---------------------------------------------------------------

test_that("table_stack() takes regression tables built for the console", {
  skip_on_cran()
  t1 <- table_regression(.st_lm())
  t2 <- table_regression(.st_glm())
  expect_error(table_stack(), class = "spicy_invalid_input")
  expect_error(table_stack(mtcars), class = "spicy_invalid_input")
  expect_error(table_stack(A = t1, B = mtcars), class = "spicy_invalid_input")
  expect_error(
    table_stack(A = t1, B = table_regression(.st_glm(), output = "data.frame")),
    class = "spicy_invalid_input"
  )
  # A stack is no panel.
  st <- table_stack(A = t1, B = t2)
  expect_error(table_stack(A = st, B = t1), class = "spicy_invalid_input")
  # The descriptive tables are not panels yet.
  tc <- table_categorical(.st_data(), c(smoking))
  expect_error(table_stack(A = t1, B = tc), class = "spicy_not_implemented")
  expect_error(table_stack(A = t1, B = tc), class = "spicy_unsupported")
  # A table from a spicy that stored no note parts or no language.
  for (a in c("spicy_note_parts", "language")) {
    old <- t2
    attr(old, a) <- NULL
    e <- expect_error(
      table_stack(A = t1, B = old),
      class = "spicy_invalid_input"
    )
    expect_match(conditionMessage(e), "rebuild", ignore.case = TRUE)
  }
  # A list of tables passed as it is: the tables are right, the list
  # wants splicing.
  e <- expect_error(
    table_stack(list(A = t1, B = t2)),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(e), "table_stack(!!!tables)", fixed = TRUE)
  # A table whose rows were taken out after it was built: its typed view
  # no longer runs row for row with it.
  e <- expect_error(
    table_stack(A = t1[t1$Variable != "(Intercept)", ], B = t2),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(e), "Table 1 was modified", fixed = TRUE)
  expect_match(conditionMessage(e), "`keep`, `drop` or `show_intercept`")
  # A table without rows (a spicy before the one that refuses to build
  # it): no rows to show, not "an older spicy".
  e <- expect_error(
    table_stack(A = t1, B = t2[0, ]),
    class = "spicy_empty_table"
  )
  expect_match(
    conditionMessage(e),
    "Table 2 has no rows to show.",
    fixed = TRUE
  )
  expect_false(grepl("older", conditionMessage(e), fixed = TRUE))
  # The arguments of the stack itself.
  expect_error(
    table_stack(A = t1, B = t2, output = "bogus"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_stack(A = t1, B = t2, note = FALSE, note_add = "More."),
    class = "spicy_invalid_input"
  )
  # inline() cites one table: a stack is refused, a panel is not.
  expect_error(inline(st, age), class = "spicy_invalid_input")
  expect_type(inline(attr(st, "panels")[["A"]], age), "character")
})

test_that("a table of table_regression_uv() is a panel", {
  skip_on_cran()
  d <- .st_data()
  uv <- table_regression_uv(d, outcome = bmi, predictors = c(age, sex))
  t_glm <- table_regression(.st_glm())
  st <- table_stack(Screen = uv, Dentist = t_glm)
  expect_identical(attr(st, "panels")$Screen, uv)
  .st_expect_oracle(st, list(uv, t_glm))
  # The title the screen builds is not a label; its outcome is.
  expect_false(attr(uv, "spicy_title_typed"))
  expect_identical(
    names(attr(table_stack(uv, t_glm), "panels")),
    c("bmi", "dentist_12m")
  )
  # A title typed for the screen labels its panel.
  uv_typed <- table_regression_uv(
    d,
    outcome = bmi,
    predictors = c(age, sex),
    title = "Screen of BMI"
  )
  expect_true(attr(uv_typed, "spicy_title_typed"))
  expect_identical(
    names(attr(table_stack(uv_typed, t_glm), "panels"))[[1L]],
    "Screen of BMI"
  )
})

test_that("table_stack() refuses a table built for another engine", {
  skip_on_cran()
  t1 <- table_regression(.st_lm())
  for (o in c("gt", "tinytable", "flextable")) {
    if (!requireNamespace(o, quietly = TRUE)) {
      next
    }
    e <- expect_error(
      table_stack(A = t1, B = table_regression(.st_glm(), output = o)),
      class = "spicy_invalid_input"
    )
    expect_match(conditionMessage(e), "default", fixed = TRUE)
  }
})

test_that("layout is validated; a stacked table has no Outcome row", {
  f <- .st_lm()
  expect_error(
    table_regression(f, layout = "x"),
    "`layout` must be one of",
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(
      list(f, .st_glm()),
      outcome_labels = c("BMI", "Dentist"),
      layout = "stacked"
    ),
    class = "spicy_invalid_input"
  )
  # The univariable screen has no stacked layout.
  expect_error(
    table_regression_uv(
      .st_data(),
      outcome = bmi,
      predictors = c(age, sex),
      layout = "stacked"
    ),
    class = "spicy_invalid_input"
  )
})


# ---- 9. machine outputs ------------------------------------------------------

test_that("machine outputs bind the panels' own, with a panel column", {
  t1 <- table_regression(.st_lm())
  t2 <- table_regression(.st_glm())
  st <- table_stack(A = t1, B = t2)
  tabs <- list(A = t1, B = t2)

  # Every panel's rows, under the panel's own columns; the columns of the
  # result are the union, opened by `panel`.
  expect_panels <- function(out, parts, what) {
    out <- as.data.frame(out)
    expect_identical(names(out)[[1L]], "panel", info = what)
    expect_type(out$panel, "character")
    expect_setequal(
      names(out),
      c("panel", unique(unlist(lapply(parts, names))))
    )
    expect_identical(
      out$panel,
      rep(names(parts), vapply(parts, nrow, integer(1))),
      info = what
    )
    for (k in names(parts)) {
      want <- as.data.frame(parts[[k]])
      attr(want, "note") <- NULL
      rownames(want) <- NULL
      got <- out[out$panel == k, names(want), drop = FALSE]
      attr(got, "note") <- NULL
      rownames(got) <- NULL
      expect_equal(got, want, info = paste(what, k))
    }
  }

  df <- as.data.frame(st)
  expect_identical(class(df), "data.frame")
  expect_setequal(
    names(attributes(df)),
    c("names", "row.names", "class", "note")
  )
  expect_identical(attr(df, "note"), attr(st, "note"))
  expect_panels(df, lapply(tabs, as.data.frame), "as.data.frame")
  expect_identical(table_stack(A = t1, B = t2, output = "data.frame"), df)
  expect_equal(
    as.data.frame(tibble::as_tibble(st)),
    as.data.frame(df),
    ignore_attr = TRUE
  )

  lg <- table_stack(A = t1, B = t2, output = "long")
  expect_s3_class(lg, "tbl_df")
  expect_identical(attr(lg, "note"), attr(st, "note"))
  expect_panels(
    lg,
    list(
      A = table_regression(.st_lm(), output = "long"),
      B = table_regression(.st_glm(), output = "long")
    ),
    "long"
  )

  skip_if_not_installed("broom")
  expect_panels(broom::tidy(st), lapply(tabs, broom::tidy), "tidy")
  expect_panels(broom::glance(st), lapply(tabs, broom::glance), "glance")
})

test_that("layout = 'stacked' keeps the machine outputs of the columns", {
  skip_on_cran()
  skip_if_not_installed("broom")
  # tidy(), glance() and the long output of a stacked table are those of
  # the same models side by side -- the rows, their order, their key
  # `(model_id, term, estimate_type)` -- with a leading `panel` column.
  bare <- function(x) {
    x <- as.data.frame(x)
    attributes(x) <- attributes(x)[c("names", "row.names", "class")]
    x
  }
  expect_side <- function(models, ..., info) {
    st <- table_regression(models, ..., layout = "stacked")
    co <- table_regression(models, ...)
    long_st <- table_regression(
      models,
      ...,
      layout = "stacked",
      output = "long"
    )
    long_co <- table_regression(models, ..., output = "long")
    pairs <- list(
      tidy = list(broom::tidy(st), broom::tidy(co)),
      glance = list(broom::glance(st), broom::glance(co)),
      long = list(long_st, long_co)
    )
    for (nm in names(pairs)) {
      got <- pairs[[nm]][[1L]]
      expect_identical(names(got)[[1L]], "panel", info = paste(info, nm))
      expect_identical(
        bare(got[, -1L]),
        bare(pairs[[nm]][[2L]]),
        info = paste(info, nm)
      )
      # Each row names the panel of its model.
      labs <- names(attr(st, "panels"))
      ids <- spicy:::.reg_model_ids(models)
      expect_identical(
        got$panel,
        labs[match(got$model_id, ids)],
        info = paste(info, nm)
      )
    }
    expect_identical(attr(broom::tidy(st), "note"), attr(st, "note"))
    st
  }
  st <- expect_side(list(.st_lm(), .st_glm()), info = "lm + glm")
  expect_identical(
    unique(broom::tidy(st)$model_id),
    c("M1", "M2")
  )
  st <- expect_side(list(A = .st_lm(), B = .st_glm()), info = "named")
  expect_identical(broom::glance(st)$model_id, c("A", "B"))
  expect_identical(broom::glance(st)$panel, c("A", "B"))
  expect_side(.st_hierarchy(), nested = TRUE, info = "nested")
  # The displayed table keeps its own rows, panel by panel.
  expect_identical(
    table_regression(
      list(A = .st_lm(), B = .st_glm()),
      layout = "stacked",
      output = "data.frame"
    ),
    as.data.frame(st)
  )
  # A stack of tables built apart keys its rows by panel, then by the
  # model inside its panel.
  ts <- table_stack(
    A = table_regression(.st_lm()),
    B = table_regression(.st_glm())
  )
  td <- broom::tidy(ts)
  expect_identical(unique(td$model_id), "M1")
  expect_identical(unique(td$panel), c("A", "B"))
  expect_identical(
    anyDuplicated(td[c("panel", "model_id", "term", "estimate_type")]),
    0L
  )
})


# ---- 10. the merged note -----------------------------------------------------

test_that("a multinomial model beside others keeps or hides its intercepts", {
  skip_on_cran()
  skip_if_not_installed("nnet")
  skip_if_not_installed("broom")
  mn <- .st_mn()
  models <- list(Health = .st_lm(), Employment = mn)
  cf <- stats::coef(mn)
  cats <- rownames(cf)
  side_td <- function(...) {
    as.data.frame(broom::tidy(table_regression(models, ...)))
  }
  stacked_td <- function(...) {
    as.data.frame(broom::tidy(table_regression(
      models,
      ...,
      layout = "stacked"
    )))[, -1L]
  }
  # keep = "age": `keep` selects predictors, and each model keeps its
  # intercepts beside the others, the multinomial model as in its table
  # alone and with its own estimates.
  side <- side_td(keep = "age")
  emp <- side[side$model_id == "Employment", , drop = FALSE]
  want <- paste0(rep(cats, 2L), ": ", rep(c("(Intercept)", "age"), each = 3L))
  expect_setequal(emp$term, want)
  expect_setequal(
    emp$term,
    broom::tidy(table_regression(mn, keep = "age"))$term
  )
  expect_equal(
    emp$estimate[match(want, emp$term)],
    unname(c(cf[, "(Intercept)"], cf[, "age"]))
  )
  expect_true("(Intercept)" %in% side$term[side$model_id == "Health"])
  # The table still names each intercept by its outcome, indented under
  # "(Intercept):".
  body <- attr(table_regression(models), "structured")$body
  ic <- body[
    body$.variable == "(Intercept)" & body$.row_role == "level",
    ,
    drop = FALSE
  ]
  expect_identical(ic$Variable, paste0("  ", cats, ": (Intercept)"))
  expect_identical(ic$.indent, rep(1L, length(cats)))
  # show_intercept = FALSE: neither model keeps an intercept.
  noint <- side_td(show_intercept = FALSE)
  expect_setequal(
    noint$term[noint$model_id == "Employment"],
    paste0(
      rep(cats, ncol(cf) - 1L),
      ": ",
      rep(colnames(cf)[-1L], each = length(cats))
    )
  )
  expect_false("(Intercept)" %in% noint$term)
  # The stacked layout returns the rows of the models side by side.
  expect_equal(stacked_td(keep = "age"), side, ignore_attr = TRUE)
  expect_equal(stacked_td(show_intercept = FALSE), noint, ignore_attr = TRUE)
  expect_equal(stacked_td(), side_td(), ignore_attr = TRUE)
  # A pattern written for the terms of the table alone ("^age$") meets,
  # side by side, the terms named with their outcome ("Student: age"):
  # the multinomial model keeps only its intercepts there, while its
  # panel shows age too, the documented difference.
  st <- table_regression(models, keep = "^age$", layout = "stacked")
  p <- attr(st, "panels")$Employment
  expect_identical(
    attr(p, "structured")$body$.variable[
      attr(p, "structured")$body$.row_role != "fit_stat"
    ],
    c("(Intercept)", "age")
  )
  td <- as.data.frame(broom::tidy(st))
  expect_setequal(td$term[td$panel == "Employment"], want)
  side <- suppressWarnings(side_td(keep = "^age$"))
  expect_setequal(
    side$term[side$model_id == "Employment"],
    paste0(cats, ": (Intercept)")
  )
})

test_that("note, note_add: removed, replaced, completed", {
  t1 <- table_regression(.st_lm())
  t2 <- table_regression(.st_glm())
  expect_null(attr(table_stack(A = t1, B = t2, note = FALSE), "note"))
  # A typed note replaces the merged one, under the lead-in.
  expect_identical(
    attr(table_stack(A = t1, B = t2, note = "Custom."), "note"),
    attr(table_regression(.st_lm(), note = "Custom."), "note")
  )
  # `note_add` closes the general note.
  merged <- attr(table_stack(A = t1, B = t2), "note")
  added <- attr(table_stack(A = t1, B = t2, note_add = "Data: X."), "note")
  expect_true(endsWith(added, " Data: X."))
  expect_identical(added, paste(merged, "Data: X."))
})

test_that("the notes merge as the note of the models side by side", {
  skip_on_cran()
  t_lm <- table_regression(.st_lm())
  t_glm <- table_regression(.st_glm())
  note <- attr(table_stack(A = t_lm, B = t_glm), "note")
  expect_true(startsWith(note, "Note. "))
  # The type and the estimator of each model, worded as side by side:
  # one sentence each, the panels named in it.
  lines <- c(
    "A: linear regression; B: logistic regression.",
    "Std. errors: A: classical (OLS), B: classical (Fisher information)."
  )
  for (s in lines) {
    expect_identical(.st_count(note, s), 1L, info = s)
  }
  pos <- vapply(lines, function(s) regexpr(s, note, fixed = TRUE)[[1L]], 1L)
  expect_true(all(diff(pos) > 0L))
  # The abbreviations of both panels, defined once, close the note.
  expect_identical(.st_count(note, "CI = confidence interval"), 1L)
  expect_true(endsWith(
    note,
    "CI = confidence interval; AIC = Akaike information criterion."
  ))

  # One family, two estimators.
  t_hc3 <- table_regression(.st_lm(), vcov = "HC3")
  note2 <- attr(table_stack(A = t_lm, B = t_hc3), "note")
  expect_identical(.st_count(note2, "Linear regression models."), 1L)
  expect_match(
    note2,
    "Std. errors: A: classical (OLS), B: heteroskedasticity-robust (HC3).",
    fixed = TRUE
  )
  # Three panels, two families: each named, as side by side.
  f_glm2 <- stats::glm(
    physical_activity ~ age + sex,
    family = stats::binomial(),
    data = .st_data()
  )
  note3 <- attr(
    table_stack(A = t_lm, B = t_glm, C = table_regression(f_glm2)),
    "note"
  )
  expect_identical(
    note3,
    .st_side_note(list(A = .st_lm(), B = .st_glm(), C = f_glm2))
  )

  # Any other sentence: the ones every panel carries first, then the
  # others with the labels of their panels, panel after panel. Two Cox
  # panels share the exponentiation sentence and differ by concordance.
  skip_if_not_installed("survival")
  cox <- list(
    survival::coxph(survival::Surv(time, status) ~ age, data = survival::lung),
    survival::coxph(
      survival::Surv(time, status) ~ age + sex,
      data = survival::lung
    )
  )
  note4 <- attr(
    table_regression(
      cox,
      nested = TRUE,
      exponentiate = TRUE,
      layout = "stacked"
    ),
    "note"
  )
  shared <- regexpr("Coefficients exponentiated", note4, fixed = TRUE)
  own <- gregexpr("Model [12]: Concordance", note4)[[1L]]
  expect_length(own, 2L)
  expect_true(shared > 0L && all(shared < own))
  expect_identical(.st_count(note4, "Coefficients exponentiated"), 1L)
  # Under the type, the side-by-side sentence of one family.
  expect_true(startsWith(
    note4,
    "Note. Cox proportional hazards regression models."
  ))
})

test_that("the stacked note is the note of the models side by side", {
  skip_on_cran()
  # The oracle of the merge: with the columns of a model alone in both
  # layouts, the note of `layout = "stacked"` is the note of `layout =
  # "columns"`, theme by theme.
  models <- list(A = .st_lm(), B = .st_glm())
  st <- table_regression(models, layout = "stacked")
  expect_identical(attr(st, "note"), .st_side_note(models))
  # By default the side-by-side table shows no interval: the one sentence
  # it lacks is the definition of the CI the panels show.
  side <- attr(table_regression(models), "note")
  expect_false(grepl("CI = confidence interval", side, fixed = TRUE))
  expect_identical(
    sub("CI = confidence interval; ", "", attr(st, "note"), fixed = TRUE),
    side
  )
  # A hierarchy.
  h <- .st_hierarchy()
  expect_identical(
    attr(table_regression(h, nested = TRUE, layout = "stacked"), "note"),
    .st_side_note(h, nested = TRUE)
  )
  # A reference outcome, which only the multinomial panel carries.
  skip_if_not_installed("nnet")
  mn <- list(Health = .st_lm(), Employment = .st_mn())
  note_mn <- attr(table_regression(mn, layout = "stacked"), "note")
  expect_identical(note_mn, .st_side_note(mn))
  expect_match(note_mn, "Employment: Reference outcome:", fixed = TRUE)
})

test_that("the legend of the stars is printed once when the panels share it", {
  skip_on_cran()
  t_star <- table_regression(.st_lm(), stars = TRUE)
  legend <- utils::tail(strsplit(attr(t_star, "note"), "\n")[[1L]], 1L)
  # One panel with stars: its legend, once, last.
  note <- attr(table_stack(A = t_star, B = table_regression(.st_glm())), "note")
  expect_identical(utils::tail(strsplit(note, "\n")[[1L]], 1L), legend)
  expect_identical(.st_count(note, "p < .001"), 1L)
  # Two panels with the same stars: still once.
  note2 <- attr(
    table_stack(A = t_star, B = table_regression(.st_glm(), stars = TRUE)),
    "note"
  )
  expect_identical(.st_count(note2, "p < .001"), 1L)
  # Two legends: each opens with its panel.
  note3 <- attr(
    table_stack(
      A = t_star,
      B = table_regression(.st_glm(), stars = c("+" = 0.1))
    ),
    "note"
  )
  expect_match(note3, "A: *** p < .001", fixed = TRUE)
  expect_match(note3, "B: + p < .10", fixed = TRUE)
})

test_that("a panel's removed or typed note is what it brings to the stack", {
  skip_on_cran()
  t_glm <- table_regression(.st_glm())
  # A panel built with note = FALSE brings no sentence.
  note <- attr(
    table_stack(A = table_regression(.st_lm(), note = FALSE), B = t_glm),
    "note"
  )
  expect_false(grepl("Linear regression.", note, fixed = TRUE))
  # Panel A says nothing: B's sentences are B's, and say so.
  expect_match(note, "B: logistic regression.", fixed = TRUE)
  # A panel built with a typed note brings that note.
  note2 <- attr(
    table_stack(A = table_regression(.st_lm(), note = "Typed A."), B = t_glm),
    "note"
  )
  expect_match(note2, "A: Typed A.", fixed = TRUE)
  expect_false(grepl("Linear regression.", note2, fixed = TRUE))
})

test_that("a table's note is the composition of its stored parts", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  compose <- function(tbl) {
    pushed <- spicy:::.style_restore(tbl)
    on.exit(spicy:::.style_end(pushed), add = TRUE)
    withr::with_options(
      list(spicy.language = attr(tbl, "language")),
      spicy:::.compose_note_parts(.st_parts(tbl))
    )
  }
  f <- .st_lm()
  tabs <- list(
    lm = table_regression(f),
    glm = table_regression(.st_glm(), stars = TRUE, note_add = "Data: X."),
    polr = table_regression(MASS::polr(
      self_rated_health ~ age + sex,
      data = .st_data(),
      Hess = TRUE
    )),
    jama = table_regression(f, style = "jama"),
    fr = withr::with_options(
      list(spicy.language = "fr"),
      table_regression(f)
    )
  )
  for (nm in names(tabs)) {
    expect_false(is.null(.st_parts(tabs[[nm]])), info = nm)
    expect_identical(compose(tabs[[nm]]), attr(tabs[[nm]], "note"), info = nm)
  }
})


# ---- 11. merged fit statistics -----------------------------------------------

test_that("a panel's merged fit statistics stay that panel's", {
  skip_on_cran()
  t_merged <- table_regression(.st_lm(), fit_stats_layout = "merged")
  t_glm <- table_regression(.st_glm())
  st <- table_stack(A = t_merged, B = t_glm)
  expect_identical(attr(st, "fit_stats_layout"), "merged")
  own <- attr(st, "panel_rows")
  fit_a <- own$A[seq.int(
    attr(t_merged, "group_sep_rows")[[1L]],
    nrow(t_merged)
  )]
  fit_b <- own$B[seq.int(attr(t_glm, "group_sep_rows")[[1L]], nrow(t_glm))]
  rows <- vapply(
    attr(st, "fit_merge_specs"),
    function(sp) as.numeric(sp$row),
    numeric(1)
  )
  expect_setequal(rows, fit_a)

  skip_if_not_installed("flextable")
  ft <- table_stack(A = t_merged, B = t_glm, output = "flextable")
  spans <- ft$body$spans$rows
  expect_true(all(spans[fit_a, 2L] > 1L))
  expect_true(all(spans[fit_b, ] == 1L))

  skip_if_not_installed("gt")
  expect_warning(
    table_stack(A = t_merged, B = t_glm, output = "gt"),
    class = "spicy_ignored_arg"
  )
})


# ---- 12. a panel named Total -------------------------------------------------

test_that("a panel named Total is ruled as a section, not as a total", {
  skip_on_cran()
  st <- table_stack(
    Overall = table_regression(.st_lm()),
    Total = table_regression(.st_glm())
  )
  out <- utils::capture.output(print(st))
  body <- grepl(intToUtf8(0x2502L), out, fixed = TRUE)
  i <- which(
    body & startsWith(trimws(out), spicy_fmt("label_block_header", "Total"))
  )
  expect_length(i, 1L)
  above <- out[i - 1L]
  # The light rule that opens a panel, not the solid rule of a total.
  expect_true(grepl(intToUtf8(0x254CL), above, fixed = TRUE))
  expect_false(grepl(intToUtf8(0x2500L), above, fixed = TRUE))
})


# ---- 13. console snapshots ---------------------------------------------------

test_that("stacked lm, glm and coxph print as pinned", {
  skip_on_cran()
  skip_if_not_installed("survival")
  models <- list(BMI = .st_lm(), Dentist = .st_glm(), Survival = .st_cox())
  expect_snapshot(print(table_regression(models, layout = "stacked")))
})

test_that("a stack with a header per panel prints as pinned", {
  skip_on_cran()
  skip_if_not_installed("survival")
  models <- list(BMI = .st_lm(), Dentist = .st_glm(), Survival = .st_cox())
  expect_snapshot(print(table_regression(
    models,
    exponentiate = TRUE,
    layout = "stacked"
  )))
})

test_that("a stacked hierarchy prints as pinned", {
  skip_on_cran()
  expect_snapshot(print(table_regression(
    .st_hierarchy(),
    nested = TRUE,
    layout = "stacked"
  )))
})

test_that("a stack with column groups, split by width, prints as pinned", {
  skip_on_cran()
  skip_if_not_installed("nnet")
  withr::local_options(width = 60)
  expect_snapshot(print(table_regression(
    list(Health = .st_lm(), Employment = .st_mn()),
    layout = "stacked"
  )))
})


# ---- 10. arguments, refusals and notes, after review -------------------------

test_that("a named value in `...` is an argument, not a table", {
  skip_on_cran()
  f <- .st_lm()
  t1 <- table_regression(f)
  t2 <- table_regression(.st_glm())
  # `labels` labels the terms in every table_* function: the panels take
  # `panel_labels`.
  e <- expect_error(
    table_stack(t1, t2, labels = c("A", "B")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(e),
    "`labels` is not an argument of `table_stack()`.",
    fixed = TRUE
  )
  expect_match(conditionMessage(e), "`panel_labels`", fixed = TRUE)
  expect_no_match(conditionMessage(e), "Table 3", fixed = TRUE)
  # A formal typed in part is named in full.
  expect_true("title" %in% names(formals(table_stack)))
  e <- expect_error(
    table_stack(t1, t2, titl = "T"),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(e), "Did you mean `title`?", fixed = TRUE)
  # Any other name: still no table.
  e <- expect_error(table_stack(t1, t2, foo = 1), class = "spicy_invalid_input")
  expect_match(conditionMessage(e), "`foo` is not an argument", fixed = TRUE)
  # A model is no table: its class is named.
  e <- expect_error(table_stack(t1, f), class = "spicy_invalid_input")
  expect_match(conditionMessage(e), "Table 2 is a `lm` object", fixed = TRUE)
})

test_that("a style that sets the decimal mark is refused as a style", {
  skip_on_cran()
  f <- .st_lm()
  t_lancet <- table_regression(f, style = "lancet")
  # The Lancet style moves the decimal mark the user did not type.
  expect_false(identical(attr(t_lancet, "decimal_mark"), "."))
  e <- expect_error(
    table_stack(table_regression(f), t_lancet),
    class = "spicy_stack_mismatch"
  )
  expect_match(conditionMessage(e), "Panels differ by style", fixed = TRUE)
  expect_no_match(conditionMessage(e), "decimal mark", fixed = TRUE)
  # A decimal mark typed alone is named as such, not as the interval
  # separator it changes.
  e <- expect_error(
    table_stack(table_regression(f), table_regression(f, decimal_mark = ",")),
    class = "spicy_stack_mismatch"
  )
  expect_match(
    conditionMessage(e),
    "Panels differ by decimal mark",
    fixed = TRUE
  )
})

test_that("beside a panel with note = FALSE, the sentences keep their label", {
  skip_on_cran()
  ta <- table_regression(.st_lm())
  tb <- table_regression(.st_glm(), note = FALSE)
  # Panel A alone: a linear model, classical standard errors.
  expect_identical(
    attr(ta, "note"),
    "Note. Linear regression. Std. errors: classical (OLS). CI = confidence interval."
  )
  # B, a logistic model, shares none of A's sentences: each opens with A.
  expect_identical(
    attr(table_stack(A = ta, B = tb), "note"),
    "Note. A: linear regression. A: Std. errors: classical (OLS). CI = confidence interval."
  )
  expect_match(
    attr(table_stack(B = tb, A = ta), "note"),
    "^Note\\. A: linear regression\\."
  )
})

test_that("a type after a panel label goes on in lower case, as side by side", {
  skip_on_cran()
  f_lm <- .st_lm()
  f_glm <- .st_glm()
  # The side-by-side note writes a type after a label in lower case.
  expect_match(
    attr(table_regression(list(W = f_lm, M = f_glm)), "note"),
    "W: linear regression; M: logistic regression.",
    fixed = TRUE
  )
  # A panel of two linear models beside a logistic one.
  expect_match(
    attr(
      table_stack(
        W = table_regression(list(f_lm, f_lm)),
        M = table_regression(f_glm)
      ),
      "note"
    ),
    "W: linear regression models. M: logistic regression.",
    fixed = TRUE
  )
  # French: one sentence per panel, lower case after the colon.
  fr <- withr::with_options(
    list(spicy.language = "fr"),
    attr(
      table_stack(
        BMI = table_regression(f_lm),
        Dentist = table_regression(f_glm)
      ),
      "note"
    )
  )
  expect_match(fr, "Dentist[^A-Za-z]+r\u00e9gression logistique\\.")
  expect_no_match(fr, "R\u00e9gression", fixed = TRUE)
  # The scope of exponentiated coefficients, as side by side.
  models <- list(BMI = f_lm, Dentist = f_glm)
  side <- attr(table_regression(models, exponentiate = TRUE), "note")
  st <- attr(
    table_regression(models, exponentiate = TRUE, layout = "stacked"),
    "note"
  )
  expect_match(side, "Dentist: coefficients exponentiated", fixed = TRUE)
  expect_match(st, "Dentist: coefficients exponentiated", fixed = TRUE)
})

test_that("an adjustment of the p-values is said within each model", {
  skip_on_cran()
  a <- stats::lm(mpg ~ wt + hp + qsec, data = mtcars)
  b <- stats::lm(mpg ~ wt + cyl + am, data = mtcars)
  c2 <- stats::lm(mpg ~ wt + cyl, data = mtcars)
  # The family of a model: its coefficients, the intercept apart.
  m <- function(f) length(stats::coef(f)) - 1L
  expect_identical(c(m(a), m(b), m(c2)), c(3L, 3L, 2L))
  sentence <- function(x) {
    regmatches(x, regexpr("P-values adjusted[^.]*\\.", x))
  }
  equal <- list(A = a, B = b)
  unequal <- list(A = a, B = c2)
  expect_identical(
    sentence(attr(
      table_regression(equal, p_adjust = "holm", layout = "stacked"),
      "note"
    )),
    "P-values adjusted within each model for 3 tests (Holm method)."
  )
  expect_identical(
    sentence(attr(
      table_regression(unequal, p_adjust = "holm", layout = "stacked"),
      "note"
    )),
    "P-values adjusted within each model (Holm method), for 3 and 2 tests respectively."
  )
  # The sentence of the models side by side, for table_stack() too.
  for (pair in list(equal, unequal)) {
    side <- attr(table_regression(pair, p_adjust = "holm"), "note")
    st <- table_stack(
      A = table_regression(pair$A, p_adjust = "holm"),
      B = table_regression(pair$B, p_adjust = "holm")
    )
    expect_identical(sentence(attr(st, "note")), sentence(side))
  }
})

test_that("rarer paths: typed notes, titles, cluster names, labels", {
  skip_on_cran()
  m1 <- stats::lm(mpg ~ wt + hp, data = mtcars)
  m2 <- stats::lm(qsec ~ wt + hp, data = mtcars)
  # A typed note that opens with the lead-in brings its text, once led.
  note <- attr(
    table_stack(
      A = table_regression(m1, note = "Note. Custom panel note."),
      B = table_regression(m2)
    ),
    "note"
  )
  expect_match(note, "A: Custom panel note.", fixed = TRUE)
  expect_identical(lengths(gregexpr("Note.", note, fixed = TRUE)), 1L)
  # The title of a stacked layout: removed, or typed.
  expect_null(attr(
    table_regression(list(m1, m2), layout = "stacked", title = FALSE),
    "title"
  ))
  expect_identical(
    attr(
      table_regression(list(m1, m2), layout = "stacked", title = "T"),
      "title"
    ),
    "T"
  )
  # A hierarchy whose fit statistics leave out the changes: each panel
  # has its coefficients and the statistics asked for, no change row.
  h2 <- stats::lm(mpg ~ wt + hp, data = mtcars)
  h1 <- stats::lm(mpg ~ wt, data = mtcars)
  st <- table_regression(
    list(h1, h2),
    nested = TRUE,
    layout = "stacked",
    show_fit_stats = c("nobs", "r2")
  )
  rows <- function(f) c(NA, names(stats::coef(f)), "nobs", "r2")
  expect_identical(
    attr(st, "structured")$body$.variable,
    c(rows(h1), rows(h2))
  )
  # A list of cluster vectors keeps the name of each.
  skip_if_not_installed("sandwich")
  st <- table_regression(
    list(m1, m2),
    layout = "stacked",
    vcov = "CR1",
    cluster = list(mtcars$gear, mtcars$carb)
  )
  expect_match(attr(st, "note"), "clusters by gear", fixed = TRUE)
  expect_match(attr(st, "note"), "clusters by carb", fixed = TRUE)
  # A label naming nothing in any panel: one warning, as side by side.
  w <- .st_conditions(table_regression(
    list(m1, m2),
    layout = "stacked",
    labels = c(wt = "Weight", zz = "Z")
  ))
  expect_identical(sum(grepl("^spicy_unknown_name:", w)), 1L)
  expect_match(w[grepl("^spicy_unknown_name:", w)], "\"zz\"", fixed = TRUE)
  # Outcomes shown alike: the panels take the names of the outcomes.
  dv <- vapply(list(m1, m2), function(f) all.vars(stats::formula(f))[[1L]], "")
  st <- table_regression(
    list(m1, m2),
    layout = "stacked",
    labels = c(mpg = "Same", qsec = "Same")
  )
  expect_identical(names(attr(st, "panels")), dv)
})

test_that("panel cells of their own reach the stack: counts, outcomes, Excel", {
  skip_on_cran()
  d <- .st_data()
  # The events/N cells of a binomial model are composite cells.
  g <- stats::glm(dentist_12m ~ sex, family = stats::binomial(), data = d)
  tg <- table_regression(g, show_columns = c("n_events", "b", "p"))
  st <- table_stack(A = tg, B = tg)
  ev <- as.integer(table(g$model$sex[g$model$dentist_12m == "Yes"]))
  n <- as.integer(table(g$model$sex))
  want <- sprintf("%d/%d", ev, n)
  rows <- attr(st, "panel_rows")$B
  got <- .st_console(st)[rows, 2L]
  expect_true(all(vapply(
    want,
    function(x) any(grepl(x, got, fixed = TRUE)),
    TRUE
  )))
  # The Outcome row of a side-by-side panel names its outcomes.
  m1 <- stats::lm(mpg ~ wt, data = mtcars)
  m2 <- stats::lm(qsec ~ wt, data = mtcars)
  side <- table_regression(list(m1, m2), outcome_labels = c("MPG", "QSEC"))
  st <- table_stack(A = side, B = side)
  body <- .st_view(attr(st, "structured"))
  orow <- which(attr(st, "structured")$body$.row_role == "outcome")
  expect_length(orow, 2L)
  for (r in orow) {
    expect_identical(
      setdiff(unlist(body[r, -1L], use.names = FALSE), ""),
      c("MPG", "QSEC")
    )
  }
  # The Yes of a fixed effect is text in Excel, as in its table alone.
  skip_if_not_installed("fixest")
  skip_if_not_installed("openxlsx2")
  fx <- fixest::feols(mpg ~ wt + hp | cyl, data = mtcars)
  expect_identical(names(fixest::fixef(fx)), "cyl")
  p <- withr::local_tempfile(fileext = ".xlsx")
  table_stack(
    A = table_regression(fx),
    B = table_regression(m1),
    output = "excel",
    excel_path = p
  )
  wb <- openxlsx2::wb_load(p)
  xl <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  at <- which(xl[[1L]] == "cyl")
  expect_length(at, 1L)
  expect_identical(xl[[2L]][[at]], "Yes")
})

test_that("output = 'clipboard' writes the stack's payload", {
  skip_on_cran()
  skip_if_not_installed("clipr")
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(content, ...) {
      captured$text <- content
      invisible(content)
    },
    .package = "clipr"
  )
  t1 <- table_regression(.st_lm())
  t2 <- table_regression(.st_glm())
  suppressMessages(table_stack(A = t1, B = t2, output = "clipboard"))
  lines <- strsplit(captured$text, "\n", fixed = TRUE)[[1L]]
  first <- vapply(strsplit(lines, "\t", fixed = TRUE), `[`, "", 1L)
  expect_true(all(spicy_fmt("label_block_header", c("A", "B")) %in% first))
  expect_match(lines[[length(lines)]], "^Note\\. ")
})

test_that("a continuation that leaves out a group keeps the rule of its total", {
  skip_on_cran()
  withr::local_options(width = 40)
  x <- data.frame(
    Variable = c("a", "b", "c", "Total"),
    A = c("1.11111", "2.22222", "3.33333", "6.66666"),
    B = c("1.11111", "2.22222", "3.33333", "6.66666"),
    C = c("", "", "3.33333", "6.66666"),
    D = c("", "", "3.33333", "6.66666"),
    stringsAsFactors = FALSE
  )
  out <- utils::capture.output(spicy_print_table(
    x,
    title = NULL,
    note = NULL,
    align_left_cols = 1L,
    total_row_idx = 4L,
    blank_row_groups = list(1:2, 3L, 4L)
  ))
  # The second part holds C and D, where rows a and b have no cell: it
  # keeps c and Total, the rule of the total above its second row.
  part <- out[seq(which(!nzchar(trimws(out)))[[1L]] + 1L, length(out))]
  rows <- trimws(sub(intToUtf8(0x2502L), "", part, fixed = TRUE))
  expect_false(any(grepl("^(a|b) ", rows)))
  rule <- grepl(paste0("^", intToUtf8(0x2500L), "+"), part)
  expect_identical(which(grepl("^ *Total", part)) - 1L, which(rule)[[2L]])
  expect_identical(sum(rule), 2L)
})

test_that("each panel's standard-error label opens in lower case after its name", {
  skip_on_cran()
  skip_if_not_installed("fixest")
  skip_if_not_installed("estimatr")
  d <- .st_data()
  fe <- fixest::feols(bmi ~ age + sex, data = d)
  rb <- estimatr::lm_robust(bmi ~ age + sex, data = d, se_type = "HC1")
  ols <- .st_lm()
  # Each model alone writes its label at the head of the sentence.
  expect_match(
    attr(table_regression(fe), "note"),
    "Std. errors: Classical.",
    fixed = TRUE
  )
  expect_match(
    attr(table_regression(rb), "note"),
    "Std. errors: Robust (HC1).",
    fixed = TRUE
  )
  want <- "Std. errors: A: classical, B: robust (HC1), C: classical (OLS)."
  models <- list(A = fe, B = rb, C = ols)
  expect_match(attr(table_regression(models), "note"), want, fixed = TRUE)
  expect_match(
    attr(table_regression(models, layout = "stacked"), "note"),
    want,
    fixed = TRUE
  )
  expect_match(
    attr(
      table_stack(
        A = table_regression(fe),
        B = table_regression(rb),
        C = table_regression(ols)
      ),
      "note"
    ),
    want,
    fixed = TRUE
  )
  # French: a surname keeps its capital ("Wald"), a common noun does not.
  nb <- intToUtf8(0xa0)
  withr::with_options(list(spicy.language = "fr"), {
    cox <- .st_cox()
    expect_match(
      attr(table_regression(cox), "note"),
      paste0("Erreurs standards", nb, ": Wald asymptotique (z)."),
      fixed = TRUE
    )
    expect_match(
      attr(table_regression(fe), "note"),
      paste0("Erreurs standards", nb, ": classiques."),
      fixed = TRUE
    )
    expect_match(
      attr(
        table_regression(list(A = fe, B = cox), layout = "stacked"),
        "note"
      ),
      paste0(
        "Erreurs standards",
        nb,
        ": A",
        nb,
        ": classiques, B",
        nb,
        ": Wald asymptotique (z)."
      ),
      fixed = TRUE
    )
  })
})


# ---- 14. Excel signs and p-value families, with main's rules -----------------

test_that("a stacked nested table signs the differences in Excel, not the F-change", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  models <- .st_hierarchy()[1:2]
  p <- withr::local_tempfile(fileext = ".xlsx")
  table_regression(
    models,
    nested = TRUE,
    layout = "stacked",
    show_fit_stats = c(
      "nobs",
      "r2",
      "aic",
      "r2_change",
      "f_change",
      "p_change",
      "aic_change"
    ),
    output = "excel",
    excel_path = p
  )
  xl <- .st_xl_read(p)
  delta <- intToUtf8(0x0394L)
  row_of <- function(label) {
    r <- which(xl$grid[, 1L] %in% paste(label, "vs Model 1"))
    expect_length(r, 1L)
    r
  }
  s1 <- summary(models[[1L]])
  s2 <- summary(models[[2L]])
  cells <- list(
    list(
      label = paste0(delta, "R", intToUtf8(0x00B2L)),
      value = s2$r.squared - s1$r.squared,
      fmt = "+0.00;-0.00;0.00"
    ),
    list(
      label = paste0(delta, "AIC"),
      value = stats::AIC(models[[2L]]) - stats::AIC(models[[1L]]),
      fmt = "+0.0;-0.0;0.0"
    )
  )
  # A test statistic is unsigned and carries its df: a number whose
  # format ends with the console's df, '0.00" (1, 29)"'.
  av <- stats::anova(models[[1L]], models[[2L]])
  ref <- paste0("B", row_of("F-change"))
  expect_identical(xl$type(ref), "number")
  expect_equal(
    as.numeric(xl$grid[as.integer(sub("B", "", ref)), 2L]),
    av$F[[2L]],
    tolerance = 1e-10
  )
  expect_identical(
    gsub("&quot;", "\"", xl$numfmt(ref), fixed = TRUE),
    sprintf(
      "0.00\" (%d, %d)\"",
      as.integer(av$Df[[2L]]),
      as.integer(av$Res.Df[[2L]])
    )
  )
  for (cl in cells) {
    ref <- paste0("B", row_of(cl$label))
    # A number, with the sign in its format (decimals as the token's).
    expect_identical(xl$type(ref), "number", info = cl$label)
    expect_equal(
      as.numeric(xl$grid[as.integer(sub("B", "", ref)), 2L]),
      cl$value,
      tolerance = 1e-10,
      info = cl$label
    )
    expect_identical(xl$numfmt(ref), cl$fmt, info = cl$label)
  }
  # The difference is negative for the AIC, positive for the R-squared.
  expect_lt(cells[[2L]]$value, 0)
  expect_gt(cells[[1L]]$value, 0)
})

test_that("a stacked note counts the coefficient and AME families as side by side", {
  skip_on_cran()
  skip_if_not_installed("marginaleffects")
  models <- list(
    stats::lm(mpg ~ wt + I(wt^2), data = mtcars),
    stats::lm(mpg ~ wt + I(wt^2) + hp, data = mtcars)
  )
  sc <- c("b", "se", "ci", "p", "ame", "ame_p")
  # The hand count: the coefficients but the intercept, and one AME per
  # predictor (wt^2 is wt's).
  n_coef <- vapply(models, function(m) length(stats::coef(m)) - 1L, integer(1))
  n_ame <- vapply(
    models,
    function(m) length(all.vars(stats::formula(m))[-1L]),
    integer(1)
  )
  expect_identical(n_coef, c(2L, 3L))
  expect_identical(n_ame, c(1L, 2L))
  sentence <- paste0(
    sprintf(
      "P-values adjusted within each model (Holm method), for %d and %d tests respectively.",
      n_coef[[1L]],
      n_coef[[2L]]
    ),
    " ",
    sprintf(
      "AME p-values adjusted within each model (Holm method), for %d and %d tests respectively.",
      n_ame[[1L]],
      n_ame[[2L]]
    )
  )
  side <- attr(
    table_regression(models, p_adjust = "holm", show_columns = sc),
    "note"
  )
  st <- attr(
    table_regression(
      models,
      layout = "stacked",
      p_adjust = "holm",
      show_columns = sc
    ),
    "note"
  )
  expect_match(side, sentence, fixed = TRUE)
  expect_match(st, sentence, fixed = TRUE)
  # Each panel's own sentence, a table of one model, is replaced.
  expect_no_match(st, "for 2 tests (", fixed = TRUE)
  expect_no_match(st, "Model 1: P-values", fixed = TRUE)
})

test_that("a stacked note names the models of a family as side by side", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  skip_if_not_installed("marginaleffects")
  set.seed(1)
  d <- data.frame(
    x1 = stats::rnorm(220),
    smoke = factor(sample(c("no", "yes"), 220, TRUE)),
    yc = factor(
      sample(c("Poor", "Fair", "Good", "Very good"), 220, TRUE),
      levels = c("Poor", "Fair", "Good", "Very good"),
      ordered = TRUE
    )
  )
  # A Cox panel refuses the AME columns in a stack: the panel between
  # without an AME is an intercept-only model.
  models <- list(
    MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE),
    stats::glm(I(yc >= "Good") ~ 1, data = d, family = stats::binomial),
    stats::glm(I(yc >= "Good") ~ x1 + smoke, data = d, family = stats::binomial)
  )
  # Hand count: 2 slopes x 4 categories for the polr, 2 for the last glm.
  n_ame <- c(2L * nlevels(d$yc), 2L)
  note_of <- function(sc, ...) {
    attr(
      suppressWarnings(table_regression(
        models,
        p_adjust = "holm",
        show_columns = sc,
        ...
      )),
      "note"
    )
  }
  sentences <- list(
    with_p = paste(
      "P-values adjusted within each model for 2 and 2 tests",
      "(Model 1 and Model 3, Holm method).",
      sprintf(
        paste0(
          "AME p-values adjusted within each model for %d and %d tests ",
          "(Model 1 and Model 3, Holm method)."
        ),
        n_ame[[1L]],
        n_ame[[2L]]
      )
    ),
    ame_only = sprintf(
      paste0(
        "P-values adjusted within each model for %d and %d tests ",
        "(Model 1 and Model 3, Holm method)."
      ),
      n_ame[[1L]],
      n_ame[[2L]]
    )
  )
  cols <- list(
    with_p = c("b", "p", "ame", "ame_p"),
    ame_only = c("b", "ame", "ame_p")
  )
  for (k in names(cols)) {
    side <- note_of(cols[[k]])
    st <- note_of(cols[[k]], layout = "stacked")
    expect_match(side, sentences[[k]], fixed = TRUE, label = k)
    expect_match(st, sentences[[k]], fixed = TRUE, label = k)
    expect_false(grepl("Model 1: P-values", st, fixed = TRUE), label = k)
  }
})

test_that("a stacked table warns of an interaction term with several rows", {
  skip_on_cran()
  set.seed(2)
  d <- data.frame(
    age = stats::rnorm(120, 40, 10),
    wc = factor(sample(c("no", "yes"), 120, TRUE)),
    g3 = factor(sample(c("a", "b", "c"), 120, TRUE))
  )
  d$y <- d$age / 10 + stats::rnorm(120)
  models <- list(
    stats::lm(y ~ age * wc, data = d),
    stats::lm(y ~ wc * g3, data = d)
  )
  # The engine's names: "wc:g3" is a term of two coefficients.
  expect_identical(
    grep(":", names(stats::coef(models[[2L]])), fixed = TRUE, value = TRUE),
    c("wcyes:g3b", "wcyes:g3c")
  )
  side <- expect_warning(
    table_regression(models, labels = c("wc:g3" = "WC x G3")),
    class = "spicy_unknown_name"
  )
  st <- expect_warning(
    out <- table_regression(
      models,
      layout = "stacked",
      labels = c("wc:g3" = "WC x G3", "age:wc" = "Age x WC")
    ),
    class = "spicy_unknown_name"
  )
  expect_match(
    conditionMessage(st),
    '"wc:g3" names "wcyes:g3b", "wcyes:g3c".',
    fixed = TRUE
  )
  expect_identical(conditionMessage(st), conditionMessage(side))
  # The term with one row labels it in its panel.
  expect_true("Age x WC" %in% trimws(out$Variable))
  expect_false("WC x G3" %in% trimws(out$Variable))
})

test_that("an intercept-only model between: the note names the models tested", {
  skip_on_cran()
  models <- list(
    stats::lm(mpg ~ wt + hp, data = mtcars),
    stats::lm(mpg ~ 1, data = mtcars),
    stats::lm(mpg ~ wt, data = mtcars)
  )
  # Hand count: the coefficients but the intercept.
  n_coef <- vapply(models, function(m) length(stats::coef(m)) - 1L, integer(1))
  expect_identical(n_coef, c(2L, 0L, 1L))
  note_of <- function(...) {
    attr(table_regression(models, p_adjust = "holm", ...), "note")
  }
  en <- sprintf(
    paste0(
      "P-values adjusted within each model for %d and %d tests ",
      "(Model 1 and Model 3, Holm method)."
    ),
    n_coef[[1L]],
    n_coef[[3L]]
  )
  expect_match(note_of(), en, fixed = TRUE)
  expect_match(note_of(layout = "stacked"), en, fixed = TRUE)
  expect_false(grepl("respectively", note_of(), fixed = TRUE))
  withr::local_options(spicy.language = "fr")
  fr <- sprintf(
    paste0(
      "Valeurs p ajust\u00e9es dans chaque mod\u00e8le pour %d et %d tests ",
      "(Mod\u00e8le 1 et Mod\u00e8le 3, m\u00e9thode de Holm)."
    ),
    n_coef[[1L]],
    n_coef[[3L]]
  )
  expect_match(note_of(), fr, fixed = TRUE)
  expect_match(note_of(layout = "stacked"), fr, fixed = TRUE)
})

test_that("a stacked table refuses fixest models with fixed effects only", {
  skip_on_cran()
  skip_if_not_installed("fixest")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  fe_only <- fixest::feols(Ozone ~ 1 | Month, data = d)
  full <- fixest::feols(Ozone ~ Wind | Month, data = d)
  # The engine estimates no coefficient for it.
  expect_length(stats::coef(fe_only), 0L)
  err <- expect_error(
    table_regression(list(fe_only, full, fe_only), layout = "stacked"),
    class = "spicy_empty_table"
  )
  expect_identical(
    strsplit(conditionMessage(err), "\n", fixed = TRUE)[[1L]][1L],
    "Model 1, Model 3 have no rows to show: they have fixed effects only."
  )
})
