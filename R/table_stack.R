#' Stack tables one under the other
#'
#' @description
#' `table_stack()` sets regression tables one under the other. Each table
#' becomes a **panel**, opened by a row with its label, and keeps its own
#' columns, rows, fit statistics and number formats: the cells of a panel
#' have the content of the table passed, in every engine. The panels
#' share one column header when they have the same columns, and each opens
#' with its own header otherwise. Their notes are merged into one.
#'
#' Build each table with its own arguments (`vcov`, `cluster`,
#' `exponentiate`, `p_adjust`, `show_columns`, ...), then stack them: a
#' grid of models is `table_stack(table_regression(list(m1, m2)),
#' table_regression(list(m3, m4)))`. For one panel per model of a list,
#' `table_regression(models, layout = "stacked")` is the shortcut.
#'
#' @param ... Tables returned by [table_regression()] or
#'   [table_regression_uv()] with `output = "default"`, one per panel, in
#'   the order of the panels. A name given here labels its panel:
#'   `table_stack(Women = t1, Men = t2)`. A list of tables is spliced with
#'   `!!!`: `table_stack(!!!tables)`.
#' @param panel_labels Character vector, one non-empty label per table, in
#'   the order of the tables, unnamed: the labels of the panels not named
#'   in `...` (a name given there takes precedence, with a warning). `NULL`
#'   (default) labels such a panel by the title you typed for its table,
#'   else by its outcome when the panels have different outcomes, else
#'   "Panel k". Two panels cannot share a label.
#' @param title Title of the stacked table. `NULL` (default) keeps the
#'   title of the tables when they all have the same one, and gives none
#'   otherwise; `FALSE` gives none; a string sets it.
#' @param note `NULL` (default) merges the notes of the panels; `FALSE`
#'   removes the note; a string replaces the general note, printed after
#'   the lead-in ("Note."), with the legend of the stars kept below it.
#' @param note_add Sentences added at the end of the general note.
#' @param output Output format, as in [table_regression()]: `"default"`
#'   (a printable data frame), `"data.frame"`, `"long"`, `"tinytable"`,
#'   `"gt"`, `"flextable"`, `"excel"`, `"clipboard"` or `"word"`.
#' @inheritParams table_regression
#'
#' @details
#' # Panels
#'
#' A row with the label of the panel opens each panel ("Women:"), in the
#' typography of the block headers. The label of a panel is its name in
#' `...`, else its entry in `panel_labels`, else the title you typed for
#' its table (`title = "..."`), else its outcome when every panel has an
#' outcome of its own, else "Panel k". A title spicy built is not a label:
#' it describes the model ("Linear regression: bmi"), not the panel. The
#' rows of a panel follow, in the order of its table: coefficients, blocks
#' (thresholds, random effects, components), fit statistics.
#'
#' When every panel has the same column headers, one header heads the
#' whole table. Otherwise the header of the first panel heads it and each
#' later panel opens with its own header rows, below its label row:
#' an `exponentiate = TRUE` glm panel reads "OR" where an lm panel reads
#' "B", and a multinomial panel keeps its outcome categories in columns.
#' A panel narrower than the widest one leaves the extra columns blank.
#' The header of a panel has the rows of the header of its table: its
#' column groups, ruled under, its labels, and the bounds of its interval
#' ("LL", "UL"), which the console leaves out since it prints an interval
#' in one column.
#'
#' In the console, the columns are matched by position and every panel
#' shares their widths: a panel with other columns can widen a column of
#' the first. When the panels have the same columns and decimals, the
#' decimals line up from one panel to the next, only the padding of a
#' cell changing; otherwise each panel keeps the padding of its table.
#' When the console splits a wide stack by width, a column group cut by
#' the split keeps its label over each part, and a continuation leaves
#' out the panels with no cell in it. The other engines write each cell
#' as its table does.
#'
#' A table of [table_regression_uv()] is a panel like any other, its
#' univariable models and their own sample sizes included.
#'
#' # Notes
#'
#' The notes are merged sentence by sentence. The type of each model and
#' its standard errors are worded as in a table of the models side by
#' side: "Linear regression models.", or "Women: linear regression; Men:
#' logistic regression." (in English; another language gives each panel
#' its own sentence, as below), and "Std. errors: classical (OLS)." or
#' one estimator per panel. So is an adjustment of the p-values ("P-values
#' adjusted within each model ..."). Any other sentence that every panel
#' carries is printed once, first; one that only some panels carry follows,
#' opening with their labels ("Women, Men: ..."), panel after panel. The
#' abbreviations are defined once. The legend of the stars is printed
#' once, and when panels have different legends, each opens with the
#' labels of its panels. A panel built with `note = FALSE` contributes no
#' sentence, and one built with a typed `note` contributes that note.
#'
#' # Compatibility
#'
#' The panels must be built in the same language
#' (`options(spicy.language)`), with the same `decimal_mark` and `align`,
#' and under styles that format a cell alike: the stacked table has one of
#' each. Two styles that print the same table are compatible (no style
#' and `style = "apa"`); a lever that differs (the decimals of a p-value,
#' the separator or brackets of an interval, the minus sign, the lead-in
#' of the note, the abbreviations) is not, nor is `p_digits` typed for one
#' table of a theme that sets its own p-values. Otherwise `table_stack()`
#' stops with `spicy_stack_mismatch`, naming what differs.
#'
#' Only the tables of [table_regression()] and [table_regression_uv()]
#' can be panels for now; the descriptive tables will follow.
#'
#' @return With `output = "default"`, a `spicy_stack_table` (classes
#'   `c("spicy_stack_table", "spicy_table", "data.frame")`): the cells as
#'   printed, one row per printed row, plus a blank row for the bounds of
#'   an interval in a panel's header. Its attributes `panels` (the tables
#'   stacked, named by their labels), `panel_rows` (the rows of each panel
#'   in it, its label and header rows excluded), `title` and `note` are
#'   documented; the others are internal. [as_structured()] returns the
#'   typed views of the panels, as a named list.
#'
#'   With `output = "data.frame"` (and from `as.data.frame()`), the data
#'   frames of the panels bound by rows; with `output = "long"` (and from
#'   `broom::tidy()` and `broom::glance()`), the outputs of their models.
#'   Each opens with a `panel` column holding the label of the panel; the
#'   columns are the union of the panels' and the note, as printed, is the
#'   `note` attribute. In the output of `table_stack()`, a row is keyed by
#'   `panel`, `model_id` (the model inside its panel), `term` and
#'   `estimate_type`. With `table_regression(layout = "stacked")`, these
#'   three outputs are those of the same models side by side, `panel`
#'   aside: one row per `(model_id, term, estimate_type)`, `model_id`
#'   being the identifier of the model in the list. With `"tinytable"`,
#'   `"gt"` or `"flextable"`, the object of that package; with `"excel"`
#'   or `"word"`, the path of the file, invisibly; with `"clipboard"`, the
#'   stacked table, invisibly.
#'
#' @section Classed conditions:
#' Errors carry `spicy_invalid_input` (no table, an object that is not a
#' table built with `output = "default"`, a stack passed as a panel, a
#' table whose rows were changed after it was built, invalid or
#' duplicated labels), `spicy_empty_table` (a table without rows),
#' `spicy_not_implemented` with `spicy_unsupported` (a descriptive table)
#' and `spicy_stack_mismatch` with `spicy_invalid_input` (panels built in
#' different languages, with different decimal marks or alignments, or
#' under styles that format a cell differently). Warnings carry
#' `spicy_ignored_arg` (an entry of `panel_labels` overridden by a name in
#' `...`).
#'
#' @seealso [table_regression()], whose `layout = "stacked"` stacks one
#'   panel per model; [as_structured()].
#'
#' @examples
#' women <- sochealth[sochealth$sex == "Female", ]
#' men <- sochealth[sochealth$sex == "Male", ]
#' table_stack(
#'   Women = table_regression(lm(bmi ~ age + smoking, data = women)),
#'   Men = table_regression(lm(bmi ~ age + smoking, data = men))
#' )
#'
#' # Panels with their own columns: the second opens with its header.
#' fit_bmi <- lm(bmi ~ age + sex, data = sochealth)
#' fit_dentist <- glm(dentist_12m ~ age + sex, data = sochealth,
#'                    family = binomial)
#' table_stack(
#'   BMI = table_regression(fit_bmi),
#'   Dentist = table_regression(fit_dentist, exponentiate = TRUE)
#' )
#'
#' # One panel per model.
#' table_regression(list(BMI = fit_bmi, Dentist = fit_dentist),
#'                  layout = "stacked")
#'
#' @export
table_stack <- function(
  ...,
  panel_labels = NULL,
  title = NULL,
  note = NULL,
  note_add = NULL,
  output = c(
    "default",
    "data.frame",
    "long",
    "tinytable",
    "gt",
    "flextable",
    "excel",
    "clipboard",
    "word"
  ),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  word_template = NULL
) {
  output <- spicy_match_arg(output)
  validate_caption_arg(title, "title")
  .check_note_args(note, note_add)
  .warn_ignored_output_args(output, names(match.call()))
  validate_output_resources(output, excel_path, word_path)
  tables <- rlang::list2(...)
  .stack_check_tables(tables)
  .stack_check_compatible(tables)
  .table_stack_impl(
    unname(tables),
    .stack_panel_labels(tables, names(tables), panel_labels),
    title = .stack_title(tables, title),
    note = note,
    note_add = note_add,
    note_extra = NULL,
    output = output,
    excel_path = excel_path,
    excel_sheet = excel_sheet,
    clipboard_delim = clipboard_delim,
    word_path = word_path,
    word_template = word_template
  )
}

# Every element of `...` a table that can be a panel: a table_regression()
# or table_regression_uv() table built with `output = "default"`, as built,
# by a spicy that records what the merge needs (its language, the parts of
# its note).
.stack_check_tables <- function(tables, call = rlang::caller_env()) {
  if (length(tables) == 0L) {
    spicy_abort(
      c(
        "`table_stack()` needs at least one table.",
        "i" = "Pass the tables to stack: `table_stack(t1, t2)`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  rebuild <- "Build each table with `output = \"default\"` (the default)."
  rows_hint <- paste0(
    "Rebuild it, choosing its rows with `keep`, `drop` or ",
    "`show_intercept`."
  )
  # The builders of the tables that carry a typed view, regression apart:
  # their tables cannot be panels yet.
  descriptive <- .STRUCTURED_TABLE_BUILDERS[
    names(.STRUCTURED_TABLE_BUILDERS) != "spicy_regression_table"
  ]
  for (k in seq_along(tables)) {
    x <- tables[[k]]
    if (inherits(x, "spicy_stack_table")) {
      spicy_abort(
        c(
          sprintf("Table %d is a stacked table; a stack cannot be a panel.", k),
          "i" = "Pass its panels instead: `!!!attr(stack, \"panels\")`."
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    if (inherits(x, "spicy_regression_table")) {
      if (nrow(x) == 0L) {
        spicy_abort(
          c(sprintf("Table %d has no rows to show.", k), "i" = rows_hint),
          class = "spicy_empty_table",
          call = call
        )
      }
      st <- attr(x, "structured", exact = TRUE)
      if (
        is.null(st) ||
          is.null(attr(x, "spicy_note_parts", exact = TRUE)) ||
          is.null(attr(x, "language", exact = TRUE))
      ) {
        spicy_abort(
          c(
            sprintf(
              "Table %d was built by an older spicy, which records too little to stack it.",
              k
            ),
            "i" = "Rebuild it with `table_regression()`."
          ),
          class = "spicy_invalid_input",
          call = call
        )
      }
      # Rows taken out of a table (`t[t$Variable != "(Intercept)", ]`)
      # leave its typed view whole: the two no longer run row for row.
      if (nrow(x) != nrow(st$body)) {
        spicy_abort(
          c(
            sprintf("Table %d was modified after it was built.", k),
            "i" = rows_hint
          ),
          class = "spicy_invalid_input",
          call = call
        )
      }
      next
    }
    desc <- intersect(names(descriptive), class(x))
    if (length(desc)) {
      spicy_abort(
        c(
          sprintf(
            "Table %d was built by %s: stacking descriptive tables is not implemented yet.",
            k,
            descriptive[[desc[[1L]]]]
          ),
          "i" = "Only `table_regression()` tables can be panels."
        ),
        class = "spicy_not_implemented",
        call = call
      )
    }
    # A list of tables passed as it is: the tables are fine, the list
    # only needs splicing.
    if (
      is.list(x) &&
        !is.data.frame(x) &&
        length(x) > 0L &&
        all(vapply(x, inherits, logical(1), "spicy_table"))
    ) {
      spicy_abort(
        c(
          sprintf("Table %d is a list of tables, not a table.", k),
          "i" = "Splice the list into the tables to stack: `table_stack(!!!tables)`."
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    # A named value that is no table: an argument of another function
    # (`labels`), or a mistyped one of ours, which `...` took as a table.
    nm <- names(tables)[k] %||% ""
    if (!is.na(nm) && nzchar(nm) && (is.atomic(x) || is.null(x))) {
      formal <- setdiff(names(formals(table_stack)), "...")
      cand <- formal[startsWith(formal, nm)]
      spicy_abort(
        c(
          sprintf("`%s` is not an argument of `table_stack()`.", nm),
          "i" = if (nm %in% c("labels", "label")) {
            paste0(
              "Label the panels with `panel_labels`, or by naming the ",
              "tables; label the terms in the call that builds each table."
            )
          } else if (length(cand) == 1L) {
            sprintf(
              "Did you mean `%s`? Arguments are matched by their full name.",
              cand
            )
          } else {
            "A named argument in `...` is a table, its name the label of its panel."
          }
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    what <- if (inherits(x, "gt_tbl")) {
      "a gt table"
    } else if (inherits(x, "flextable")) {
      "a flextable"
    } else if (inherits(x, "tinytable")) {
      "a tinytable"
    } else if (is.data.frame(x)) {
      "a plain data frame"
    } else {
      sprintf("a `%s` object", class(x)[[1L]])
    }
    spicy_abort(
      c(
        sprintf("Table %d is %s, not a spicy regression table.", k, what),
        "i" = rebuild
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# One language, one decimal mark, one alignment and one style for the
# whole stack (decision 1 of dev/stacked_layout_spec.md for the first two).
# The alignment and the style join them because the engines read them
# when they write a cell: panels built under two could not each be shown
# as built.
.stack_check_compatible <- function(tables, call = rlang::caller_env()) {
  check_attr <- function(ck) {
    ref <- attr(tables[[1L]], ck$attr, exact = TRUE)
    for (k in seq_along(tables)[-1L]) {
      cur <- attr(tables[[k]], ck$attr, exact = TRUE)
      if (identical(cur, ref)) {
        next
      }
      spicy_abort(
        c(
          sprintf(
            "Panels differ by %s: panel 1 has %s, panel %d %s.",
            ck$what,
            .quote_val(ref %||% "none"),
            k,
            .quote_val(cur %||% "none")
          ),
          "i" = ck$hint
        ),
        class = "spicy_stack_mismatch",
        call = call
      )
    }
  }
  check_attr(list(
    attr = "language",
    what = "language",
    hint = "Build every table under the same `options(spicy.language)`."
  ))
  # The style, lever by lever, as the writers read it: two styles that
  # print the same cells (no style and "apa") stack. It comes before the
  # decimal mark, which a style sets too: the user typed the style. The
  # levers are read under one decimal mark, so that a decimal mark typed
  # alone is reported as such.
  dm <- attr(tables[[1L]], "decimal_mark", exact = TRUE)
  ref <- .stack_style_levers(tables[[1L]], dm)
  for (k in seq_along(tables)[-1L]) {
    cur <- .stack_style_levers(tables[[k]], dm)
    differ <- names(ref)[
      !vapply(names(ref), function(l) identical(ref[[l]], cur[[l]]), logical(1))
    ]
    if (!length(differ)) {
      next
    }
    spicy_abort(
      c(
        sprintf(
          "Panels differ by style: panel 1 and panel %d format their cells differently.",
          k
        ),
        stats::setNames(
          sprintf(
            "%s: panel 1 %s, panel %d %s.",
            .STACK_STYLE_LEVERS[differ],
            unlist(ref[differ], use.names = FALSE),
            k,
            unlist(cur[differ], use.names = FALSE)
          ),
          rep("*", length(differ))
        ),
        "i" = paste0(
          "Build every table with the same `style`",
          if ("p_format" %in% differ) {
            paste0(
              ", and type `p_digits` for all of them or for none: a typed ",
              "`p_digits` replaces the p-values of a theme"
            )
          },
          "."
        )
      ),
      class = "spicy_stack_mismatch",
      call = call
    )
  }
  check_attr(list(
    attr = "decimal_mark",
    what = "decimal mark",
    hint = "Build every table with the same `decimal_mark`."
  ))
  check_attr(list(
    attr = "align",
    what = "`align`",
    hint = "Build every table with the same `align`."
  ))
  invisible(NULL)
}

# The levers of a style that a stack reads when it writes its cells or
# composes its note, by the name its errors give them. The others (the
# floor and the leading zero of a p-value, the headers) are fixed in the
# cells of each panel when its table is built.
.STACK_STYLE_LEVERS <- c(
  p_format = "p-value format",
  ci_sep = "interval separator",
  ci_brackets = "interval brackets",
  minus_sign = "minus sign",
  note_label = "note lead-in",
  abbreviations = "abbreviations",
  abbrev_rule = "abbreviation rule"
)

# Each lever of `.STACK_STYLE_LEVERS` for the style `tbl` was built under,
# as the writers read it: the levers its stamp sets (`style_fmt`, the
# style over the typography of its language) over spicy's defaults. The
# stamp is pushed alone, so that no style in force where the stack is
# built leaks in. Each value is written as the error prints it, under the
# decimal mark `dm`, which sets the default interval separator.
.stack_style_levers <- function(tbl, dm = NULL) {
  .style_push(attr(tbl, "style_fmt", exact = TRUE) %||% list())
  on.exit(.style_pop(), add = TRUE)
  fmt <- .style_fmt()
  dm <- dm %||% "."
  list(
    p_format = if (!is.null(fmt$p_bands)) {
      .style_fmt_lever(fmt$p_bands)
    } else if (!is.null(fmt$p_sigfig)) {
      sprintf("%d significant figures", as.integer(fmt$p_sigfig))
    } else {
      "the decimals of `p_digits`"
    },
    ci_sep = .quote_val(.style_ci_sep(if (identical(dm, ",")) "; " else ", ")),
    ci_brackets = .quote_val(paste(.style_ci_brackets(), collapse = "")),
    minus_sign = .quote_val(.style_minus_sign()),
    note_label = .quote_val(fmt$note_label %||% "Note."),
    abbreviations = if (is.null(fmt$abbreviations)) {
      "those of its rule"
    } else {
      .quote_val(fmt$abbreviations)
    },
    abbrev_rule = .quote_val(.abbrev_rule())
  )
}

# Sets `options(spicy.language)` to the language the panels were built in
# and returns the previous options, for `options()` to restore. English
# is the option unset, spicy's fast path.
.stack_language_options <- function(tables) {
  lang <- attr(tables[[1L]], "language", exact = TRUE)
  options(spicy.language = if (identical(lang, "en")) NULL else lang)
}

# The label of each panel: its name in `...`, else its entry in
# `panel_labels`, else the title typed for its table, else its outcome
# when every panel has one of its own, else "Panel k" in the language of
# the panels. Two panels cannot share one.
.stack_panel_labels <- function(
  tables,
  dot_names,
  panel_labels,
  call = rlang::caller_env()
) {
  n <- length(tables)
  if (!is.null(panel_labels) && !is.null(names(panel_labels))) {
    spicy_abort(
      c(
        "`panel_labels` must be unnamed: one label per table, in the order of the tables.",
        "i" = paste0(
          "To label the panels by name, name the tables in `...`: ",
          "`table_stack(Women = t1, Men = t2)`."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (
    !is.null(panel_labels) &&
      (!is.character(panel_labels) ||
        length(panel_labels) != n ||
        anyNA(panel_labels) ||
        !all(nzchar(panel_labels)))
  ) {
    spicy_abort(
      sprintf(
        "`panel_labels` must be NULL or a character vector of %d non-empty label%s, one per table.",
        n,
        if (n == 1L) "" else "s"
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  dot_names <- dot_names %||% character(n)
  dot_names[is.na(dot_names)] <- ""
  named <- nzchar(dot_names)
  # A label of `panel_labels` that a name in `...` replaces is said.
  if (!is.null(panel_labels) && any(named)) {
    spicy_warn(
      if (all(named)) {
        c(
          "`panel_labels` is ignored: every table is named in `...`.",
          "i" = "Drop `panel_labels`, or the names in `...`."
        )
      } else {
        c(
          "Some labels of `panel_labels` are ignored: a name in `...` takes precedence.",
          stats::setNames(
            sprintf(
              "%s replaces %s (table %d).",
              .quote_val(dot_names[named]),
              .quote_val(panel_labels[named]),
              which(named)
            ),
            rep("*", sum(named))
          )
        )
      },
      class = "spicy_ignored_arg"
    )
  }
  old <- .stack_language_options(tables)
  on.exit(options(old), add = TRUE)
  outcomes <- .stack_panel_outcomes(tables)
  out <- vapply(
    seq_len(n),
    function(k) {
      title_k <- attr(tables[[k]], "title", exact = TRUE)
      if (named[[k]]) {
        dot_names[[k]]
      } else if (!is.null(panel_labels)) {
        panel_labels[[k]]
      } else if (
        isTRUE(attr(tables[[k]], "spicy_title_typed", exact = TRUE)) &&
          is.character(title_k) &&
          length(title_k) == 1L &&
          nzchar(title_k)
      ) {
        title_k
      } else if (!is.null(outcomes)) {
        outcomes[[k]]
      } else {
        spicy_fmt("label_panel_name", k)
      }
    },
    character(1)
  )
  if (anyDuplicated(out)) {
    dupe <- out[[anyDuplicated(out)]]
    spicy_abort(
      c(
        sprintf("Panel labels must be unique; %s repeats.", .quote_val(dupe)),
        "i" = paste0(
          "Name the tables (`table_stack(A = t1, B = t2)`) or pass ",
          "`panel_labels`."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  out
}

# The outcome each panel would be labeled by: of a table of one outcome,
# that outcome as the table names it (`spicy_outcome_display`), the rule
# of `.reg_stacked_panel_labels()` deciding between displays and names;
# NULL unless every panel has an outcome of its own.
.stack_panel_outcomes <- function(tables) {
  one <- function(t, what) {
    v <- unique(attr(t, what, exact = TRUE))
    if (length(v) == 1L) v else NA_character_
  }
  .stack_distinct_outcomes(
    vapply(tables, one, character(1), what = "outcome"),
    vapply(tables, one, character(1), what = "spicy_outcome_display")
  )
}

# The title of the stack: none under `FALSE`, the string given, else the
# title every panel shares (none when they differ).
.stack_title <- function(tables, title) {
  if (isFALSE(title)) {
    return(NULL)
  }
  if (is.character(title)) {
    return(title)
  }
  titles <- lapply(tables, attr, which = "title", exact = TRUE)
  if (any(vapply(titles, is.null, logical(1)))) {
    return(NULL)
  }
  titles <- unique(unlist(titles, use.names = FALSE))
  if (length(titles) == 1L) titles else NULL
}

# The path both doors take: table_stack() and table_regression(layout =
# "stacked"). The stack is built and rendered in the language and under
# the style of its panels, whatever is in force when it is called.
# `machine`, from the stacked layout only: the `model_id` of each model in
# the side-by-side table (`ids`) and the order of its terms
# (`term_order`), with which the machine outputs keep its key.
.table_stack_impl <- function(
  tables,
  panel_labels,
  title,
  note,
  note_add,
  note_extra,
  output,
  excel_path,
  excel_sheet,
  clipboard_delim,
  word_path,
  word_template,
  machine = NULL
) {
  old <- .stack_language_options(tables)
  on.exit(options(old), add = TRUE)
  pushed <- .style_restore(tables[[1L]])
  on.exit(.style_end(pushed), add = TRUE)
  note_txt <- .stack_note(tables, panel_labels, note, note_add, note_extra)
  st <- .stack_build(tables, panel_labels, title, note_txt)
  attr(st, "spicy_model_ids") <- machine$ids
  attr(st, "spicy_term_order") <- machine$term_order
  .stack_dispatch(
    st,
    output,
    excel_path = excel_path,
    excel_sheet = excel_sheet,
    clipboard_delim = clipboard_delim,
    word_path = word_path,
    word_template = word_template
  )
}

.stack_dispatch <- function(
  st,
  output,
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  word_template = NULL
) {
  switch(
    output,
    default = st,
    data.frame = as.data.frame(st),
    long = .stack_long(st),
    tinytable = output_tinytable(st),
    gt = output_gt(st),
    flextable = output_flextable(st),
    excel = output_excel(st, excel_path, excel_sheet),
    clipboard = output_clipboard(st, clipboard_delim),
    word = output_word(st, word_path, word_template)
  )
}

# ---- The note ----------------------------------------------------------------

# The note of the stack, merged from the parts each panel recorded
# (`attr(, "spicy_note_parts")`, the arguments of `.compose_note()`), kind
# by kind in the order of the note.
#
# The two themes that speak of each model, its type and its standard
# errors, are worded as the side-by-side table words them
# (`build_regression_type_footer_block_from_frames()`,
# `.note_std_errors()`), each panel named by its label, when every panel
# holds one model and brings the sentences spicy wrote. A mixed type list
# is English in that table (decision 42): under another language, each
# panel keeps its own type sentence instead, in the language of the
# stack. Any other sentence that every panel carries is printed once,
# first in its kind; one only some panels carry follows, opening with
# their labels, panel after panel. The abbreviations are defined once
# over the panels, the legends of the stars likewise. A panel built with
# `note = FALSE` contributes nothing; one built with a typed note
# contributes it in place of its own sentences. `extra` holds sentences
# about the whole stack (`inference`), printed bare.
.stack_note <- function(tables, panel_labels, note, note_add, extra = NULL) {
  if (isFALSE(note)) {
    return(NULL)
  }
  parts <- lapply(tables, attr, which = "spicy_note_parts", exact = TRUE)
  live <- !vapply(parts, function(p) isTRUE(p$suppressed), logical(1))
  typed <- vapply(parts, function(p) !is.null(p$typed), logical(1))
  auto <- live & !typed
  sep <- spicy_str("note_panels_sep")
  # One kind: a list with the sentences of each panel (NULL for a panel
  # that contributes none). The sentences every panel carries come first;
  # then the others, each opening with the labels of its panels, in the
  # order of the first panel that carries it. A panel built with
  # `note = FALSE` carries none: the sentences of the others keep their
  # labels. After a label, a sentence for which `lower` is TRUE (a type,
  # the scope of exponentiated coefficients) goes on in lower case, as
  # side by side.
  merge_kind <- function(by_panel, lower = function(x) FALSE) {
    s <- unique(unlist(by_panel[live], use.names = FALSE))
    if (!length(s)) {
      return(character(0))
    }
    has <- lapply(s, function(x) {
      live & vapply(by_panel, function(b) x %in% b, logical(1))
    })
    shared <- vapply(has, all, logical(1))
    first <- vapply(has, function(h) which(h)[[1L]], integer(1))
    vapply(
      order(!shared, ifelse(shared, 0L, first)),
      function(i) {
        if (shared[[i]]) {
          s[[i]]
        } else {
          spicy_fmt(
            "note_model_line",
            paste(panel_labels[has[[i]]], collapse = sep),
            if (lower(s[[i]])) lowercase_first(s[[i]]) else s[[i]]
          )
        }
      },
      character(1)
    )
  }
  kind <- function(name, from = auto) {
    lapply(seq_along(parts), function(i) {
      if (from[[i]]) {
        x <- unlist(parts[[i]][name], use.names = FALSE)
        x[!is.na(x) & nzchar(x)]
      }
    })
  }
  # The two themes of each model, when every live panel holds one model
  # and brings its own sentences.
  per_model <- any(auto) &&
    all(auto) &&
    all(vapply(
      parts[auto],
      function(p) length(p$models$type) == 1L,
      logical(1)
    ))
  type_txt <- NULL
  vcov_txt <- NULL
  if (per_model) {
    refs <- panel_labels[auto]
    types <- vapply(parts[auto], function(p) p$models$type, character(1))
    if (
      length(unique(types)) == 1L || identical(.spicy_language_code(), "en")
    ) {
      type_txt <- build_regression_type_footer_block_from_frames(
        Map(
          function(type, ref) {
            list(
              info = list(extras = list(title_prefix = type), model_label = ref)
            )
          },
          types,
          refs
        ),
        decimal_mark = attr(tables[[1L]], "decimal_mark", exact = TRUE)
      )
    }
    vcov_txt <- .note_std_errors(
      vapply(parts[auto], function(p) p$models$vcov, character(1)),
      refs
    )
  }
  # The adjustment of the p-values, its family each model: worded as side
  # by side ("within each model"), each panel's own sentence, that of a
  # table of one model, replaced.
  if (per_model) {
    adj <- lapply(parts[auto], function(p) p$models$p_adjust)
    method <- unique(unlist(lapply(adj, `[[`, "method")))
    m <- unlist(lapply(adj, `[[`, "m"))
    if (length(method) == 1L && length(m) == sum(auto) && all(m > 0L)) {
      whole <- .p_adjust_sentence(method, m)
      for (j in which(auto)) {
        own <- .p_adjust_sentence(method, parts[[j]]$models$p_adjust$m)
        inf <- parts[[j]]$inference
        parts[[j]]$inference[inf %in% own] <- whole
      }
    }
  }
  typed_txt <- lapply(seq_along(parts), function(i) {
    if (live[[i]] && typed[[i]] && nzchar(parts[[i]]$typed)) {
      .stack_strip_lead(parts[[i]]$typed)
    }
  })
  # The legend of the stars explains the markers wherever they are: one
  # legend is printed once, bare; several each open with their panels.
  legends <- kind("probability", live)
  probability <- if (length(unique(unlist(legends, use.names = FALSE))) <= 1L) {
    unique(unlist(legends, use.names = FALSE))
  } else {
    merge_kind(legends)
  }
  types_own <- unlist(kind("type"), use.names = FALSE)
  exp_scope <- spicy_str("note_exp_scope_all")
  built <- .compose_note(
    what = c(
      type_txt,
      merge_kind(
        kind(if (is.null(type_txt)) c("type", "what") else "what"),
        lower = function(x) x %in% types_own || startsWith(x, exp_scope)
      )
    ),
    missing = merge_kind(kind("missing")),
    inference = c(
      vcov_txt,
      merge_kind(kind(
        if (is.null(vcov_txt)) c("vcov", "inference") else "inference"
      )),
      extra$inference
    ),
    definitions = merge_kind(kind("definitions")),
    abbreviations = .note_abbreviations(unlist(
      lapply(parts[auto], function(p) p$abbreviation_items),
      recursive = FALSE
    )),
    probability = probability,
    add = c(merge_kind(typed_txt), merge_kind(kind("add")), note_add)
  )
  .table_note(built, note, note_add, probability = probability)
}

# A typed note of a panel without the lead-in it may open with: the note
# of the stack has its own.
.stack_strip_lead <- function(text) {
  if (!nzchar(text) || !.note_opens_with_lead(text)) {
    return(text)
  }
  split <- .note_prefix_split(text, .note_prefix_text())
  if (is.null(split)) text else trimws(split$rest)
}

# ---- Machine outputs ---------------------------------------------------------

# The output `fun` gives each panel, bound by rows under a leading `panel`
# column with the label of the panel; the columns are the union of the
# panels', and the note of the stack is the one attribute.
.stack_rbind_panels <- function(x, fun) {
  panels <- attr(x, "panels", exact = TRUE)
  dfs <- Map(
    function(p, lab) {
      d <- as.data.frame(fun(p), stringsAsFactors = FALSE)
      cbind(
        data.frame(panel = rep(lab, nrow(d)), stringsAsFactors = FALSE),
        d
      )
    },
    panels,
    names(panels)
  )
  out <- Reduce(.rbind_union, unname(dfs))
  rownames(out) <- NULL
  attributes(out) <- attributes(out)[c("names", "row.names", "class")]
  attr(out, "note") <- attr(x, "note", exact = TRUE)
  out
}

# The output `fun` gives the models of a stack (`tidy()`, `glance()`, the
# long rows), under a leading `panel` column. A stack of
# `table_regression(layout = "stacked")` keeps the key of the models side
# by side: their rows, each model under its `model_id` in the list, in
# the order of that table (`.stack_side_rows()`). Any other stack binds
# the outputs of its panels (`.stack_rbind_panels()`).
.stack_machine <- function(x, fun) {
  ids <- attr(x, "spicy_model_ids", exact = TRUE)
  if (is.null(ids)) {
    return(.stack_rbind_panels(x, fun))
  }
  panels <- attr(x, "panels", exact = TRUE)
  side <- structure(
    list(),
    spicy_long = .stack_side_rows(x, "spicy_long"),
    spicy_fit_stats = .stack_side_rows(x, "spicy_fit_stats")
  )
  d <- as.data.frame(fun(side), stringsAsFactors = FALSE)
  out <- cbind(
    data.frame(
      panel = names(panels)[match(d$model_id, ids)],
      stringsAsFactors = FALSE
    ),
    d
  )
  rownames(out) <- NULL
  attributes(out) <- attributes(out)[c("names", "row.names", "class")]
  attr(out, "note") <- attr(x, "note", exact = TRUE)
  out
}

# The rows `what` of the panels of a stacked layout -- their long
# coefficients ("spicy_long") or their fit statistics ("spicy_fit_stats")
# -- as the models side by side hold them: each model under its
# `model_id` in the list, the coefficients in the order of the terms of
# that table, then of the estimate type and of the model (the order of
# `align_frames()`).
.stack_side_rows <- function(x, what) {
  panels <- attr(x, "panels", exact = TRUE)
  ids <- attr(x, "spicy_model_ids", exact = TRUE)
  rows <- Map(
    function(p, id) {
      d <- attr(p, what, exact = TRUE)
      d$model_id <- rep(id, nrow(d))
      d
    },
    panels,
    ids
  )
  out <- Reduce(.rbind_union, unname(rows))
  if (identical(what, "spicy_long")) {
    out$order_idx <- match(out$term, attr(x, "spicy_term_order", exact = TRUE))
    out <- out[
      order(out$order_idx, out$estimate_type, out$model_id),
      ,
      drop = FALSE
    ]
  }
  rownames(out) <- NULL
  out
}

.stack_long <- function(st) {
  out <- maybe_as_tibble(.stack_machine(st, function(p) {
    output_long(list(coefs_aligned = attr(p, "spicy_long", exact = TRUE)))
  }))
  attr(out, "note") <- attr(st, "note", exact = TRUE)
  out
}

#' Print method for stacked tables
#'
#' @description
#' Prints a `spicy_stack_table` returned by [table_stack()] or by
#' `table_regression(layout = "stacked")` as an ASCII table: the title,
#' each panel opened by its label row (and by its own header rows when the
#' panels differ in their columns), and the merged note.
#'
#' @param x A `spicy_stack_table`.
#' @param ... Passed to [spicy_print_table()] (e.g. `padding`).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_stack()]
#' @keywords internal
#' @export
print.spicy_stack_table <- function(x, ...) {
  shown <- structure(x, class = "data.frame")
  shown[] <- lapply(shown, function(v) {
    v <- as.character(v)
    v[is.na(v)] <- ""
    v
  })
  hr <- as.integer(attr(x, "header_rows", exact = TRUE) %||% integer(0))
  # The column groups of a panel's header are drawn over their columns,
  # with a rule under them, as those of the top header: their labels
  # leave the cells.
  bs <- attr(x, "body_spanners", exact = TRUE) %||% list()
  bs_rows <- vapply(bs, function(b) as.integer(b$row), integer(1))
  shown[bs_rows, -1L] <- ""
  # The row of the bounds of an interval ("LL", "UL") is blank here: the
  # console prints an interval in one column.
  blank <- hr[vapply(
    hr,
    function(r) !any(nzchar(trimws(unlist(shown[r, ], use.names = FALSE)))),
    logical(1)
  )]
  blank <- setdiff(blank, bs_rows)
  # The other header rows are centered over their columns, as the top
  # header is, rather than right-aligned like the numbers under them.
  lr <- setdiff(hr, c(bs_rows, blank))
  if (length(lr)) {
    for (j in seq_along(shown)[-1L]) {
      v <- shown[[j]]
      w <- max(0L, nchar(v, type = "width"))
      pad <- pmax(0L, w - nchar(v[lr], type = "width"))
      v[lr] <- paste0(
        strrep(" ", pad %/% 2L),
        v[lr],
        strrep(" ", pad - pad %/% 2L)
      )
      shown[[j]] <- v
    }
  }
  kept <- setdiff(seq_len(nrow(x)), blank)
  at <- function(r) {
    r <- as.integer(r)
    match(r[r %in% kept], kept)
  }
  body <- shown[kept, , drop = FALSE]
  rownames(body) <- NULL
  for (a in c("title", "note", "align", "padding", "spanners")) {
    attr(body, a) <- attr(x, a, exact = TRUE)
  }
  attr(body, "group_sep_rows") <- at(attr(x, "group_sep_rows", exact = TRUE))
  attr(body, "section_sep_rows") <- at(
    attr(x, "section_sep_rows", exact = TRUE)
  )
  # When the console splits the table by width, a continuation leaves
  # out each panel that has no cell in it, and each fit statistic whose
  # value sits in an earlier one, as a table split alone does.
  role <- attr(x, "structured", exact = TRUE)$body$.row_role
  groups <- c(
    Map(
      function(s, own) seq.int(s, max(own)),
      attr(x, "section_rows", exact = TRUE),
      attr(x, "panel_rows", exact = TRUE)
    ),
    as.list(which(role == "fit_stat"))
  )
  .print_regression_like(
    body,
    display_labels = attr(x, "display_labels", exact = TRUE),
    fit_stats_start = NULL,
    body_spanners = lapply(bs, function(b) {
      list(row = at(b$row), spans = b$spans)
    }),
    blank_row_groups = unname(lapply(groups, at)),
    ...
  )
  invisible(x)
}

#' Convert a `spicy_stack_table` to a data frame
#'
#' `as.data.frame()` returns the data frames of the panels of a stacked
#' table ([as.data.frame.spicy_regression_table()]) bound by rows, with a
#' leading `panel` column holding the label of the panel: exactly what
#' `output = "data.frame"` returns. The columns are the union of the
#' panels'; the note, as printed, is kept in `attr(, "note")`.
#' `as_tibble()` returns the same table as a tibble.
#'
#' @param x A `spicy_stack_table` returned by [table_stack()].
#' @param row.names,optional Standard `as.data.frame()` arguments
#'   (ignored).
#' @param ... Ignored.
#'
#' @return A `data.frame` (`as.data.frame()`) or a `tbl_df`
#'   (`as_tibble()`).
#'
#' @seealso [tidy.spicy_stack_table()]
#' @name as.data.frame.spicy_stack_table
#' @keywords internal
NULL

#' @rdname as.data.frame.spicy_stack_table
#' @exportS3Method base::as.data.frame
as.data.frame.spicy_stack_table <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  .stack_rbind_panels(x, as.data.frame)
}

#' @rdname as.data.frame.spicy_stack_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_stack_table <- function(x, ...) {
  out <- tibble::as_tibble(.stack_rbind_panels(x, as.data.frame), ...)
  attr(out, "note") <- attr(x, "note", exact = TRUE)
  out
}

#' Tidy / glance methods for `spicy_stack_table`
#'
#' The [broom::tidy()] and [broom::glance()] outputs of the models of a
#' stacked table ([tidy.spicy_regression_table()]), with a leading `panel`
#' column holding the label of the panel of each model.
#'
#' For a stack of [table_stack()], the outputs of the panels bound by
#' rows: a row is keyed by `panel`, `model_id` (the identifier of the
#' model inside its panel), `term` and `estimate_type`. For
#' `table_regression(layout = "stacked")`, the outputs of the same models
#' side by side, `panel` aside: `model_id` is the identifier of the model
#' in the list, and a row is keyed by `model_id`, `term` and
#' `estimate_type`, as in [tidy.spicy_regression_table()]. A multinomial
#' model under `keep` or `drop` returns the rows its panel shows,
#' selected by its terms as in its table alone.
#'
#' @param x A `spicy_stack_table` returned by [table_stack()].
#' @param ... Ignored.
#'
#' @return A `tbl_df`.
#'
#' @name tidy.spicy_stack_table
#' @keywords internal
NULL

#' @rdname tidy.spicy_stack_table
#' @exportS3Method broom::tidy
tidy.spicy_stack_table <- function(x, ...) {
  out <- maybe_as_tibble(.stack_machine(x, tidy.spicy_regression_table))
  attr(out, "note") <- attr(x, "note", exact = TRUE)
  out
}

#' @rdname tidy.spicy_stack_table
#' @exportS3Method broom::glance
glance.spicy_stack_table <- function(x, ...) {
  out <- maybe_as_tibble(.stack_machine(x, glance.spicy_regression_table))
  attr(out, "note") <- attr(x, "note", exact = TRUE)
  out
}
