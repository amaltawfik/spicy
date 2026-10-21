# inline(): cite one formatted table cell in running text.
#
# The whole design rests on two pillars of the structured contract
# (see ?as_structured):
#   * ADDRESSING BY IDENTITY -- rows are found through `.variable` /
#     `.level` / `.row_role`, never by parsing display labels, so a
#     call survives custom `labels`, journal styles, and translation;
#   * THE SHARED FORMATTER -- the cell text comes from
#     .format_structured_to_string_body(), the exact function the
#     fidelity tests pin against the console, so a number quoted in
#     the text can never drift from the number printed in the table.
#
# The second pillar needs the table's STYLE back in force. Half a
# style travels in the typed contract (`digits`, `p_digits`,
# `decimal_mark` are formals, so they are already baked into
# `col_meta`); the other half -- `p_bands`, `p_sigfig`, `p_floor`,
# `ci_sep`, `ci_brackets` -- has no formal and lives in the
# call-scoped format context, which is long gone by the time a
# sentence cites a cell. `.style_restore()` puts it back for the
# length of the call.

#' Cite a table cell in inline text
#'
#' @description
#' Returns one cell of a spicy table as a character scalar, formatted
#' exactly as the table displays it -- same decimals, same *p* style,
#' same interval punctuation, same journal style. Designed for inline
#' R chunks in Quarto / R Markdown:
#'
#' ```
#' Smokers had higher odds (`r inline(tbl, smoking, "Yes", "or")`).
#' ```
#'
#' @details
#' # Addressing
#'
#' The row is found by **identity**, not by display text: `variable`
#' names the source column (`.variable` in the typed body), `level`
#' the level (`.level`). Custom `labels`, a `style`, or a translated
#' display never change the call. As a convenience, a `variable`
#' that matches no source column is looked up among the displayed
#' labels before erroring. The missing-value category is addressed by
#' `level = "(Missing)"` whatever its displayed (possibly translated or
#' relabeled) label, through its row role. Fit statistics are addressed
#' by their token as `variable` (`inline(tbl, "nobs")`,
#' `inline(tbl, "r2")`) or by their label (`inline(tbl, "n")`). The token
#' is the one `show_fit_stats` takes (`"r2"`, `"adj_r2"`, `"aic"`, ...),
#' the key of the row in the `variable` column of
#' `output = "data.frame"`. `glance()` and the long output name the same
#' statistic as broom does (`r.squared`, `adj.r.squared`), a name
#' `inline()` does not read. Identity comes first: a source column
#' wins over a token or a label of the same name, and an exact label over
#' a fit statistic's label read regardless of case. A `variable` that
#' still matches the rows of two variables or statistics is refused, and
#' the error names an address for each.
#'
#' A statistic that belongs to a whole variable rather than to one of
#' its levels sits on the variable's own row: the *p* of a
#' `table_categorical()` block, its association measure, its SMD. Leave
#' `level` out to cite it (`inline(tbl, smoking, column = "p")`). The
#' same call cites the *p* of a by-group [table_continuous()] or
#' [table_continuous_svy()], which sits on the first group's row: when
#' one row of the variable alone carries the statistic, that row is the
#' address.
#'
#' # Groups and models
#'
#' `model` selects a group of `by` in every table, by its label or its
#' position, wherever the table sets the groups out: the column
#' spanners of [table_categorical()], [table_categorical_svy()] and
#' [table_multiresponse()], the margin included
#' (`inline(tbl, smoking, "Yes", "pct", model = "Female")`), the
#' per-group columns of [table_continuous_lm()], headed `M (Female)`,
#' `M (Male)` (`inline(tbl, bmi, column = "emmean", model = "Female")`),
#' and the rows of [table_continuous()] and [table_continuous_svy()],
#' one per group (`inline(tbl, bmi, model = "Female")`). The group of
#' missing `by` values is `model = "(Missing)"`. In a multi-model table,
#' `model` selects the model the same way. The cells that belong to no
#' group (a *p*, the contrast of [table_continuous_lm()] and its
#' interval, an association measure) are cited without `model`. In
#' [table_continuous()] and [table_continuous_svy()], the test, its *p*,
#' the effect size and the SMD are statistics of the variable, printed
#' once on the row of the first group: `inline(tbl, bmi, column = "p")`.
#' Asked with `model`, whichever group it names, they are an error (class
#' `spicy_invalid_input`) that gives that call.
#'
#' `level` names a category shown as a row: the level of a variable,
#' its row of missing values. Until spicy 0.14.0 it also selected the
#' groups of [table_continuous()], [table_continuous_svy()] and
#' [table_continuous_lm()]: that use is an error (class `spicy_defunct`)
#' whose message gives the call to use instead.
#'
#' [table_multiresponse()] has one row per item, addressed by its column
#' name, then the rows of `none_row`, addressed as `"none"` and
#' `"no_data"`, and the base row, the number of respondents, addressed as
#' the fit statistic `"nobs"`: `inline(tbl, "nobs", model = "Total")`.
#' The item `p` and `statistic` belong to no group and are cited without
#' `model` (`inline(tbl, info_internet, column = "p")`).
#'
#' # Columns
#'
#' The column is a **token** of the typed contract (`"b"`, `"se"`,
#' `"p"`, `"ci"`, `"or"`, `"ame"`, `"n"`, `"pct"`, `"m"`, ... -- see
#' `as_structured()`'s `col_meta`), never a display header. `"ci"`
#' composes the interval with the style's brackets and separator, and
#' so does every other interval token the table carries (`"med_ci"`,
#' `"ame_ci"`, `"assoc_ci"`): each addresses its own bounds.
#'
#' # Patterns
#'
#' A `column` containing `{` is a pattern: each `{token}` is replaced
#' by the corresponding cell, so one call quotes a full sentence
#' fragment:
#'
#' ```
#' inline(tbl, smoking, "Yes", "{or} ({ci_label} {ci}; p = {p})")
#' ```
#'
#' `{ci_label}` inserts the interval label of the interval the pattern
#' cites (`95% CI`, or `Med 95% CI` in a pattern quoting `{med_ci}`) --
#' the first one when it cites several, the table's first when it cites
#' none. Note that `{p}` carries the floor operator when the table does
#' (`<.001`), so write `p {p}` rather than `p = {p}` in patterns that
#' may hit the floor.
#'
#' # Errors
#'
#' Every misaddressing is a classed error that lists the available
#' choices: unknown variables list the variables, missing levels list
#' the levels, unknown tokens list the table's tokens, ambiguous
#' models and groups list them. A `level` that names a group of a
#' `by` table points to `model`. A cell the table itself displays
#' as undefined (an aliased coefficient's en-dash) refuses with the
#' reason rather than pasting a dash into a sentence.
#'
#' @param x A table returned by [table_regression()],
#'   [table_categorical()], [table_continuous()],
#'   [table_continuous_lm()], [table_outcome()],
#'   [table_categorical_svy()], [table_continuous_svy()] or
#'   [table_multiresponse()] (default output) -- every family
#'   [as_structured()] accepts.
#' @param variable The source variable, unquoted or as a string; or a
#'   fit-statistic token (`"nobs"`, `"r2"`, ...) or label (`"n"`).
#' @param level For a factor variable, the level, as a string: a
#'   category shown as a row. `"(Missing)"` addresses the missing-value
#'   category by role. A group of `by` is selected with `model`.
#' @param column A column token, or a `{token}` pattern. `NULL` (the
#'   default) returns the estimate-like column of the row when it is
#'   unambiguous: the family's primary estimate. That is the
#'   coefficient for [table_regression()] -- always token `"b"`: an
#'   exponentiated table changes its header to OR, IRR or HR, never
#'   its token -- the contrast (`"delta"`) or, across a numeric `by`,
#'   the slope (`"b"`) for [table_continuous_lm()], the mean (`"m"`)
#'   or, on a median-only table, the median (`"med"`, `"med_iqr"`) for
#'   [table_continuous()], and the count (`"n"`) for
#'   [table_categorical()] and [table_multiresponse()]. A row carrying
#'   none of them refuses and lists its tokens. A [table_continuous_lm()]
#'   row that shows the mean of each group and no contrast (a `by` of
#'   three or more levels, or `contrast = "none"`) has no default: it
#'   refuses and gives the address of a mean.
#' @param model A group of `by`, or in a multi-model table the model:
#'   its label (as displayed) or its position. See *Groups and models*.
#'
#' @return A character scalar.
#'
#' @seealso [as_structured()] for the typed contract behind the
#'   addressing.
#' @export
#'
#' @examples
#' fit <- lm(wellbeing_score ~ age + sex, data = sochealth)
#' tbl <- table_regression(fit)
#' inline(tbl, age, column = "b")
#' inline(tbl, sex, "Male", "{b} ({ci_label} {ci}; p {p})")
#' inline(tbl, "n")
#'
#' # A group of `by` is a `model`, whether the table sets it out as
#' # columns or as rows.
#' ct <- table_categorical(sochealth, select = smoking, by = sex)
#' inline(ct, smoking, "Yes", "pct", model = "Female")
#' cn <- table_continuous(sochealth, select = bmi, by = sex)
#' inline(cn, bmi, model = "Female")
#' inline(cn, bmi, column = "p")
inline <- function(
  x,
  variable,
  level = NULL,
  column = NULL,
  model = NULL
) {
  # The table's own style, back in force for the length of this call --
  # otherwise the sentence re-formats under spicy's defaults and quotes
  # a number the table never printed: a Lancet p-value at four decimals
  # instead of two significant figures, an interval closed with the
  # default comma instead of the journal's en dash.
  .style_pushed <- .style_restore(x)
  on.exit(.style_end(.style_pushed), add = TRUE)

  s <- as_structured(x)
  formatted <- .format_structured_to_string_body(s)

  var_chr <- .inline_variable_chr(rlang::enquo(variable))
  # Two stages, and the order matters. The variable is resolved FIRST,
  # so an unknown one still refuses before anything else; the ROW is
  # resolved last, because on a block table which row answers depends
  # on the column asked for (a `p` sits on the block header, a mean on
  # a level), and `column` is only known once its default is filled in.
  rows <- .inline_variable_rows(s, var_chr)
  # `model` selects a group of `by` in every table, whether the table
  # sets its groups out as columns (the spanners of a categorical table,
  # the per-group columns of `table_continuous_lm()`) or as rows
  # (`table_continuous()`, `table_continuous_svy()`); `level` names a
  # category shown as a row (decision 10 of the ADR of 2026-10-20). A
  # `level` that names a group is refused with the `model` call.
  .inline_check_level_not_group(s, rows, level, var_chr, column)
  group_rows <- .inline_group_rows(s, rows)
  if (length(group_rows) > 0L && !is.null(model)) {
    rows <- .inline_pick_group_row(s, group_rows, model)
    cols <- .inline_model_cols(s, NULL)
    .inline_check_variable_stat(
      s,
      formatted,
      rows,
      group_rows,
      var_chr,
      column,
      cols
    )
  } else {
    cols <- .inline_model_cols(s, model)
  }

  if (is.null(column)) {
    column <- .inline_default_token(s, cols)
  }
  # A token the table does not carry is refused before any row is
  # resolved: on a block table the row stage would otherwise answer with
  # a level hint, which cannot help when the column itself is missing.
  for (tk in .inline_requested_tokens(column)) {
    if (length(.inline_token_cols(s, cols, tk)) == 0L) {
      .inline_abort_no_token(s, tk)
    }
  }

  row <- .inline_resolve_row(s, formatted, rows, var_chr, level, column, cols)
  if (grepl("{", column, fixed = TRUE)) {
    return(.inline_pattern(s, formatted, row, column, cols))
  }
  .inline_cell(s, formatted, row, column, cols)
}

# The variable argument as a string: a bare name is captured, a string
# passes through.
.inline_variable_chr <- function(quo) {
  expr <- rlang::quo_get_expr(quo)
  if (rlang::is_string(expr)) {
    return(expr)
  }
  rlang::as_name(quo)
}

# The body rows of one variable, found by IDENTITY first. Three tiers,
# and the first that matches anything decides (`.inline_match_rows()`):
#
#   1. a source column name, which wins outright: it is the identity;
#   2. a displayed label, or the token of a row the table GENERATES (a
#      fit statistic's "nobs", the "none" and "no_data" rows of
#      `table_multiresponse()`). A token reaches its row only when no
#      column carries the same name, which tier 1 has settled;
#   3. a fit statistic's label whatever its case ("N" for "n").
#
# A tier that matches rows of DIFFERENT identities refuses. The resolver
# used to take the first match, and to try the fit statistics, case
# folded, before any label: an item labeled "respondents" was answered
# with the base row ("Respondents"), and the label "(No item chosen)" led
# to its token, "none", which an item column of that name shared, so the
# item's count came back. Another row's number, with no error.
#
# `call` is the frame of `inline()`, which the errors name; every helper of
# this file that aborts takes it and hands it on.
.inline_variable_rows <- function(s, var_chr, call = rlang::caller_env()) {
  body <- s$body
  generated <- .inline_generated_rows(body)
  id <- .inline_row_identity(body, generated)
  ids <- unique(id[.inline_match_rows(body, var_chr, generated)])
  if (length(ids) > 1L) {
    .inline_abort_ambiguous(body, var_chr, ids, id, generated, call = call)
  }
  rows <- which(id %in% ids)
  if (length(rows) == 0L) {
    spicy_abort(
      c(
        sprintf("No variable %s in this table.", .quote_val(var_chr)),
        "i" = paste0(
          "Available: ",
          paste(
            .quote_val(unique(stats::na.omit(s$body$.variable))),
            collapse = ", "
          ),
          "."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  rows
}

# Rows the table GENERATES rather than reads from a source variable: the
# fit statistics (a regression's, and the base row of
# `table_multiresponse()`) and the rows of `none_row`, whose "missing" row
# carries no level because it belongs to no variable. Their `.variable` is
# a token of spicy's, which a source column may also be called.
.inline_generated_rows <- function(body) {
  role <- body$.row_role
  role %in% c("fit_stat", "none") | (role %in% "missing" & is.na(body$.level))
}

# The identity of each row: its variable, kept apart from a generated row
# that carries the same token. NA for a row no variable owns (the outcome
# row of a multi-outcome regression), which no address reaches.
.inline_row_identity <- function(body, generated) {
  ifelse(
    is.na(body$.variable),
    NA_character_,
    paste(ifelse(generated, "generated", "variable"), body$.variable)
  )
}

# The displayed labels, stripped of indentation and of the suffix of a
# block header ("sex:").
#
# The suffix is DERIVED from the very template that wrote it
# (`label_block_header`, filled with an empty caption), never typed out:
# French puts a no-break space before the colon, and a hardcoded ":$"
# would leave that space behind -- `shown` would then equal no caption at
# all and addressing a variable by its displayed label would abort with
# "no such variable", silently, in one language only.
.inline_shown_labels <- function(body) {
  blk_suffix <- .escape_regex(spicy_fmt("label_block_header", ""))
  sub(paste0(blk_suffix, "$"), "", trimws(body$Variable))
}

# The rows `var_chr` matches, in the first tier that matches any (see
# `.inline_variable_rows()`). A label is first read on the rows that name
# a variable, a fit statistic or a generated row, at indent 0; the label of
# an indented row, a level, is a fallback the resolver has always offered.
# Case folding applies to the fit statistics only, and when it reaches
# one, every row whose label folds to the same text is a match as well,
# so "RESPONDENTS" is not settled in favor of the base row over an item
# labeled "respondents".
.inline_match_rows <- function(body, var_chr, generated) {
  known <- !is.na(body$.variable)
  hits <- which(known & !generated & body$.variable == var_chr)
  if (length(hits) > 0L) {
    return(hits)
  }
  shown <- .inline_shown_labels(body)
  top <- is.na(body$.indent) | body$.indent == 0L
  hits <- which(
    known &
      ((top & shown == var_chr) | (generated & body$.variable == var_chr))
  )
  if (length(hits) > 0L) {
    return(hits)
  }
  hits <- which(known & shown == var_chr)
  if (length(hits) > 0L) {
    return(hits)
  }
  folded <- known & top & tolower(shown) == tolower(var_chr)
  if (any(folded & body$.row_role == "fit_stat", na.rm = TRUE)) {
    return(which(folded))
  }
  integer(0)
}

# The refusal of a request that matches rows of different identities. It
# names, for each, an address that matches that row alone: the column
# name of a variable, and the token or the displayed label of a generated
# row, each checked against the resolver itself. A generated row can be
# left with none (a regression term named like a fit statistic's token
# and another labeled like its label), and the message says so.
.inline_abort_ambiguous <- function(
  body,
  var_chr,
  ids,
  id,
  generated,
  call = rlang::caller_env()
) {
  shown <- .inline_shown_labels(body)
  top <- is.na(body$.indent) | body$.indent == 0L
  lines <- vapply(
    ids,
    function(k) {
      rows <- which(id == k)
      gen <- generated[[rows[[1L]]]]
      what <- if (!gen) {
        "the variable"
      } else if (body$.row_role[[rows[[1L]]]] == "fit_stat") {
        "the fit statistic"
      } else {
        "the row"
      }
      labels <- shown[rows][
        top[rows] & !is.na(shown[rows]) & nzchar(shown[rows])
      ]
      label <- c(labels, body$.variable[[rows[[1L]]]])[[1L]]
      candidates <- unique(c(body$.variable[[rows[[1L]]]], if (gen) labels))
      own <- Filter(
        function(a) {
          identical(unique(id[.inline_match_rows(body, a, generated)]), k)
        },
        candidates
      )
      if (length(own) == 0L) {
        return(sprintf(
          "No address of its own for %s shown as %s: give the other row another label.",
          what,
          .quote_val(label)
        ))
      }
      sprintf(
        "%s: %s shown as %s.",
        .quote_val(own[[1L]]),
        what,
        .quote_val(label)
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
  spicy_abort(
    c(
      sprintf(
        "%s matches %d rows of this table: address the one you mean.",
        .quote_val(var_chr),
        length(ids)
      ),
      stats::setNames(lines, rep("i", length(lines)))
    ),
    class = "spicy_invalid_input",
    call = call
  )
}

# Pick THE row among a variable's rows, from `level` and -- when no
# level is given -- from the column the caller asked for.
.inline_resolve_row <- function(
  s,
  formatted,
  rows,
  var_chr,
  level,
  column,
  cols,
  call = rlang::caller_env()
) {
  body <- s$body

  has_levels <- any(!is.na(body$.level[rows]))
  if (is.null(level)) {
    if (has_levels && length(rows) > 1L) {
      hdr <- .inline_header_row(
        s,
        formatted,
        rows,
        var_chr,
        .inline_requested_tokens(column),
        cols,
        call = call
      )
      if (!is.null(hdr)) {
        return(hdr)
      }
      # A statistic of the whole variable that a table without a header
      # row prints on one row of the block only (the `p` of a by-group
      # `table_continuous()`, on its first group): that row is the
      # address, as `?inline` promises ("leave `level` out").
      carriers <- .inline_rows_carrying(
        s,
        formatted,
        rows,
        .inline_requested_tokens(column),
        cols
      )
      if (length(carriers) == 1L) {
        return(carriers)
      }
      # The rows of the groups of `by` are selected with `model`, the
      # levels of a variable with `level`.
      by_groups <- length(.inline_group_rows(s, rows)) > 0L
      spicy_abort(
        c(
          if (by_groups) {
            sprintf(
              "%s has a row per group of `by`: pick one with `model`.",
              .quote_val(var_chr)
            )
          } else {
            sprintf(
              "%s has levels: pick one with `level`.",
              .quote_val(var_chr)
            )
          },
          "i" = paste0(
            "Available: ",
            paste(
              .quote_val(stats::na.omit(body$.level[rows])),
              collapse = ", "
            ),
            "."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    return(rows[1L])
  }
  # IDENTITY FIRST, role second. The role shortcut exists because the
  # missing category's displayed label can be translated, and a caller
  # writing `level = "(Missing)"` should still reach it. But taken
  # first it SHADOWS a real level literally named "(Missing)" beside a
  # missing row relabeled through `options(spicy.labels)` (the tables
  # refuse the two under one label), and `inline(tbl, g, "(Missing)")`
  # quietly returned the other row's number -- the one thing this
  # function must never do.
  #
  # So: match `.level` exactly, and fall back to the role only when
  # nothing carries that level literally. With no collision the two
  # agree (the missing row's `.level` IS its displayed label), and
  # with a collision the reader's own reading wins.
  hit <- rows[!is.na(body$.level[rows]) & body$.level[rows] == level]
  if (length(hit) != 1L && .inline_addresses_missing(level)) {
    hit <- rows[body$.row_role[rows] == "missing"]
  }
  if (length(hit) != 1L) {
    spicy_abort(
      c(
        sprintf("No level %s for %s.", .quote_val(level), .quote_val(var_chr)),
        .inline_level_hints(s, rows, var_chr, level)
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  hit
}

# What a refused `level` can do instead -- never an empty list. A
# variable with levels lists them. One without any (a numeric term, an
# item of `table_multiresponse()`) used to answer "Available: .", a dead
# end; it now says that `level` does not apply. And a `level` that names
# a column group -- a `by` group of a categorical or multiple-response
# table, a spanner there -- is pointed to `model`, the argument that
# selects it: on a multiple-response table, `level = "Female"` is the
# address a reader tries first.
.inline_level_hints <- function(s, rows, var_chr, level) {
  # Rows that are the groups of `by` carry no level `level` can name:
  # they are selected with `model`.
  if (length(.inline_group_rows(s, rows)) > 0L) {
    return(c(
      "i" = sprintf(
        "%s has no levels: its rows are the groups of `by`, selected with `model`.",
        .quote_val(var_chr)
      ),
      "i" = paste0(
        "Available: ",
        paste(.quote_val(stats::na.omit(s$body$.level[rows])), collapse = ", "),
        "."
      )
    ))
  }
  known <- stats::na.omit(s$body$.level[rows])
  hints <- if (length(known) > 0L) {
    c(
      "i" = paste0(
        "Available: ",
        paste(.quote_val(known), collapse = ", "),
        "."
      )
    )
  } else {
    c(
      "i" = sprintf(
        "%s has no levels: leave `level` out.",
        .quote_val(var_chr)
      )
    )
  }
  if (rlang::is_string(level) && level %in% names(s$spanners)) {
    hints <- c(
      hints,
      "i" = sprintf(
        "%s is a column group of this table: select it with `model = %s`.",
        .quote_val(level),
        .quote_val(level)
      )
    )
  }
  hints
}

# Is `level` an address for the missing-value category?
#
# Two spellings: the one `?inline` documents, and whatever the
# registry currently displays. The second is what makes the role
# fallback survive translation; the first is what a reader types.
.inline_addresses_missing <- function(level) {
  identical(level, "(Missing)") ||
    identical(level, spicy_str("row_missing_level"))
}

# The tokens a `column` argument asks for. A plain token asks for
# itself; a pattern asks for every `{token}` it cites. `{ci_label}` is
# not one: it names an interval, it addresses no cell.
.inline_requested_tokens <- function(column) {
  if (!grepl("{", column, fixed = TRUE)) {
    return(column)
  }
  braced <- regmatches(column, gregexpr("\\{[^{}]+\\}", column))[[1L]]
  setdiff(unique(substring(braced, 2L, nchar(braced) - 1L)), "ci_label")
}

# ---- groups of `by` -------------------------------------------------------
#
# `model` addresses a group of `by` in every table (decision 10 of the
# ADR of 2026-10-20). Three layouts carry the groups:
#   * spanners over the columns (`table_categorical()`, its survey twin,
#     `table_multiresponse()`), read by `.inline_model_cols()`;
#   * per-group columns with no spanner (`table_continuous_lm()`, whose
#     headers read "M (Female)", "M (Male)"), the group carried as data in
#     `col_meta$level` so that no consumer parses a header back;
#   * one row per group (`table_continuous()`, `table_continuous_svy()`),
#     rows of role "group", and "missing" for the group of missing `by`
#     values.
# Until 0.14.0, `level` reached the groups of the last two layouts, and
# `level = "(Missing)"` the missing group: that use is refused with the
# `model` call (`.inline_check_level_not_group()`), with no alias.

# The groups of the per-group columns, in column order (empty when the
# table has none).
.inline_column_groups <- function(s) {
  lv <- vapply(
    setdiff(names(s$col_meta), .REG_KEY_VARIABLE),
    function(nm) s$col_meta[[nm]]$level %||% NA_character_,
    character(1)
  )
  unique(stats::na.omit(lv))
}

# The rows of `rows` that are groups of `by`, the missing group included
# (empty when the variable's rows are not groups).
.inline_group_rows <- function(s, rows) {
  role <- s$body$.row_role[rows]
  if (!any(role %in% "group")) {
    return(integer(0))
  }
  rows[role %in% c("group", "missing")]
}

# The row of the group `model` names, by its label or its position, the
# missing group by the address `?inline` documents for it. `call` is the
# frame of `inline()`, which the errors name.
.inline_pick_group_row <- function(
  s,
  group_rows,
  model,
  call = rlang::caller_env()
) {
  if (length(model) != 1L) {
    spicy_abort(
      sprintf("`model` must be a single value, not length %d.", length(model)),
      class = "spicy_invalid_input",
      call = call
    )
  }
  lv <- s$body$.level[group_rows]
  hit <- if (is.numeric(model)) {
    if (model >= 1L && model <= length(group_rows)) {
      group_rows[[as.integer(model)]]
    }
  } else {
    m <- group_rows[!is.na(lv) & lv == model]
    if (length(m) != 1L && .inline_addresses_missing(model)) {
      m <- group_rows[s$body$.row_role[group_rows] == "missing"]
    }
    if (length(m) == 1L) m
  }
  if (is.null(hit)) {
    .inline_abort_no_group(model, lv, call = call)
  }
  hit
}

# The statistics of the group comparison -- the test, its p-value, the
# effect size and the SMD -- belong to the variable, not to a group: a
# table whose groups are rows prints them once, on the row of the first
# group. Asked of a group through `model`, whichever row prints them,
# they are refused with the call that cites them (rather than "the cell
# is empty" on the other rows). A statistic the table leaves blank on
# every row of the variable falls through to the refusals that say so.
# `row` is the group row `model` picked.
.INLINE_VARIABLE_TOKENS <- c("statistic", "p", "es", "smd")

.inline_check_variable_stat <- function(
  s,
  formatted,
  row,
  group_rows,
  var_chr,
  column,
  cols,
  call = rlang::caller_env()
) {
  if (is.null(column)) {
    return(invisible(NULL))
  }
  tokens <- intersect(.inline_requested_tokens(column), .INLINE_VARIABLE_TOKENS)
  for (tk in tokens) {
    hits <- .inline_token_cols(s, cols, tk)
    if (length(hits) == 0L) {
      next
    }
    filled <- function(i) {
      any(nzchar(trimws(vapply(
        hits,
        function(nm) as.character(formatted[[nm]][i]),
        character(1)
      ))))
    }
    if (!any(vapply(group_rows, filled, logical(1)))) {
      next
    }
    var_txt <- if (identical(make.names(var_chr), var_chr)) {
      var_chr
    } else {
      .quote_val(var_chr)
    }
    spicy_abort(
      c(
        sprintf(
          "`column = %s` is a statistic of the variable %s, not of a group of `by`: the table prints it once, on the row of the first group.",
          .quote_val(tk),
          .quote_val(var_chr)
        ),
        "i" = sprintf(
          "Cite it without `model`: `inline(x, %s, column = %s)`.",
          var_txt,
          .quote_val(tk)
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

.inline_abort_no_group <- function(model, groups, call = rlang::caller_env()) {
  spicy_abort(
    c(
      sprintf("No group %s in this table.", .quote_val(as.character(model))),
      "i" = paste0(
        "Available: ",
        paste(.quote_val(unique(stats::na.omit(groups))), collapse = ", "),
        "."
      )
    ),
    class = "spicy_invalid_input",
    call = call
  )
}

# `level` naming a group of `by`: refused, with the call that reaches it.
# A group of the per-group columns is one only when the variable's own
# rows carry no level (so that `level` cannot be a row address there); a
# group of the rows is one of the `.level` values of those rows. `call` is
# the frame of `inline()`, which the error names.
.inline_check_level_not_group <- function(
  s,
  rows,
  level,
  var_chr,
  column,
  call = rlang::caller_env()
) {
  if (!rlang::is_string(level)) {
    return(invisible(NULL))
  }
  group_rows <- .inline_group_rows(s, rows)
  is_group <- if (length(group_rows) > 0L) {
    lv <- s$body$.level[group_rows]
    any(!is.na(lv) & lv == level) ||
      (.inline_addresses_missing(level) &&
        any(s$body$.row_role[group_rows] == "missing"))
  } else {
    all(is.na(s$body$.level[rows])) &&
      level %in% .inline_column_groups(s)
  }
  if (!is_group) {
    return(invisible(NULL))
  }
  var_txt <- if (identical(make.names(var_chr), var_chr)) {
    var_chr
  } else {
    .quote_val(var_chr)
  }
  col_txt <- if (is.null(column)) {
    ""
  } else {
    paste0(", column = ", .quote_val(column))
  }
  # A statistic of the variable on a table whose groups are rows (the
  # test, its p-value, the effect size) is cited without any group.
  var_stat <- if (length(group_rows) > 0L && !is.null(column)) {
    carried <- Filter(
      function(tk) {
        length(.inline_token_cols(
          s,
          .inline_model_cols(s, NULL, call = call),
          tk
        )) >
          0L
      },
      intersect(.inline_requested_tokens(column), .INLINE_VARIABLE_TOKENS)
    )
    if (length(carried)) carried[[1L]]
  }
  hint <- if (is.null(var_stat)) {
    sprintf(
      "Select it with `model`: `inline(x, %s, model = %s%s)`.",
      var_txt,
      .quote_val(level),
      col_txt
    )
  } else {
    sprintf(
      "`column = %s` is a statistic of the variable, not of a group: cite it without `level`: `inline(x, %s, column = %s)`.",
      .quote_val(var_stat),
      var_txt,
      .quote_val(var_stat)
    )
  }
  spicy_abort(
    c(
      sprintf(
        "`level` no longer selects a group of `by`: %s is a group of this table.",
        .quote_val(level)
      ),
      "i" = hint
    ),
    class = c("spicy_defunct", "spicy_invalid_input"),
    call = call
  )
}

# The columns of `cols` carrying `token` (a pair for an interval).
.inline_token_cols <- function(s, cols, token) {
  cols[
    vapply(
      cols,
      function(nm) identical(s$col_meta[[nm]]$token %||% "", token),
      logical(1)
    )
  ]
}

# The block HEADER row, when the caller's tokens can only be meant for
# it -- or NULL when the question stays ambiguous.
#
# A block table (`table_categorical()`, `table_outcome()`) puts the
# statistics OF THE VARIABLE on its `factor_header` row: the group
# comparison's `p`, the association measure, the SMD. Those rows carry
# no `.level`, so a caller who wants the block's `p` has no level to
# name and used to be told to pick one -- a remedy that cannot help,
# since no level row carries a `p` at all.
#
# The rule, and it is deliberately narrow: resolve to the header only
# when EVERY token asked for is filled on the header AND blank on every
# level row of the block. Both halves are load-bearing. Without the
# second, a mean (filled on the levels too) would silently pick the
# header instead of refusing; without the first, a block whose test did
# not run would return the header's empty cell instead of saying so.
#
# A token the table does not carry is not this rule's business: it
# falls through, and the caller gets the message they got before.
.inline_header_row <- function(
  s,
  formatted,
  rows,
  var_chr,
  tokens,
  cols,
  call = rlang::caller_env()
) {
  body <- s$body
  hdr <- rows[body$.row_role[rows] == "factor_header"]
  if (length(hdr) != 1L || length(tokens) == 0L) {
    return(NULL)
  }
  level_rows <- setdiff(rows, hdr)
  on_header <- logical(length(tokens))
  off_levels <- logical(length(tokens))
  for (k in seq_along(tokens)) {
    hits <- .inline_token_cols(s, cols, tokens[[k]])
    if (length(hits) == 0L) {
      return(NULL)
    }
    cells <- function(i) {
      trimws(vapply(
        hits,
        function(nm) as.character(formatted[[nm]][i]),
        character(1)
      ))
    }
    on_header[[k]] <- all(nzchar(cells(hdr)))
    off_levels[[k]] <- all(vapply(
      level_rows,
      function(i) !any(nzchar(cells(i))),
      logical(1)
    ))
  }
  if (all(on_header) && all(off_levels)) {
    return(hdr)
  }
  # The statistic belongs to the header and is empty there: the block
  # comparison did not run. Say that, rather than pointing at levels
  # that carry the statistic even less.
  if (!any(on_header) && all(off_levels)) {
    spicy_abort(
      c(
        sprintf(
          "The %s cell is empty for %s.",
          .quote_val(tokens[[1L]]),
          .quote_val(var_chr)
        ),
        "i" = paste0(
          "That statistic belongs to the variable's block and the ",
          "group comparison did not run for it, so there is no ",
          "value to cite."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  NULL
}

# The rows of `rows` whose cells are filled for every token asked for.
.inline_rows_carrying <- function(s, formatted, rows, tokens, cols) {
  keep <- rep(length(tokens) > 0L, length(rows))
  for (tk in tokens) {
    hits <- .inline_token_cols(s, cols, tk)
    keep <- keep &
      length(hits) > 0L &
      vapply(
        rows,
        function(i) {
          all(nzchar(trimws(vapply(
            hits,
            function(nm) as.character(formatted[[nm]][i]),
            character(1)
          ))))
        },
        logical(1)
      )
  }
  rows[keep]
}

# The candidate columns after the `model` filter: NULL model on a
# single-model table keeps every column; on a multi-model table the
# choice is required as soon as a token is ambiguous across models.
.inline_model_cols <- function(s, model, call = rlang::caller_env()) {
  all_cols <- setdiff(names(s$col_meta), .REG_KEY_VARIABLE)
  spans <- s$spanners
  if (is.null(model)) {
    return(all_cols)
  }
  if (length(model) != 1L) {
    spicy_abort(
      sprintf("`model` must be a single value, not length %d.", length(model)),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (length(spans) == 0L) {
    # The per-group columns of `table_continuous_lm()`: the group's own
    # columns, and those of no group (`p`, `n`, the contrast and its
    # interval), which stay addressable.
    groups <- .inline_column_groups(s)
    if (length(groups) == 0L) {
      spicy_abort(
        "`model` was supplied but this table has no model or group to select.",
        class = "spicy_invalid_input",
        call = call
      )
    }
    pick <- if (is.numeric(model)) {
      if (model >= 1L && model <= length(groups)) groups[[as.integer(model)]]
    } else if (model %in% groups) {
      model
    }
    if (is.null(pick)) {
      .inline_abort_no_group(model, groups, call = call)
    }
    lv <- vapply(
      all_cols,
      function(nm) s$col_meta[[nm]]$level %||% NA_character_,
      character(1)
    )
    return(all_cols[is.na(lv) | lv == pick])
  }
  # `model = k` addresses the k-th MODEL. Since `.model_spanner_ranges()`
  # emits one entry per model, in model order, the k-th entry of `spans`
  # IS the k-th model -- so resolve the POSITION and keep it. Going out
  # to the label and back (`names(spans)[[k]]`, then `spans[[label]]`)
  # re-entered the list by name, and `[[` by name returns the FIRST
  # match: on a table whose labels collided it handed back a different
  # model's columns, silently, in running text.
  pick <- if (is.numeric(model)) {
    if (model < 1L || model > length(spans)) NULL else as.integer(model)
  } else {
    m <- match(model, names(spans))
    if (is.na(m)) NULL else m
  }
  if (is.null(pick)) {
    spicy_abort(
      c(
        sprintf("Unknown model %s.", .quote_val(as.character(model))),
        "i" = paste0(
          "Available: ",
          paste(.quote_val(names(spans)), collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  # Spanner indices count the Variable column; col_meta keys do not.
  body_cols <- names(.struct_display_body(s$body))
  # The columns under no spanner belong to no group -- the `p` and the
  # association measure of a `by` table, the item test of a
  # multiple-response table -- so they stay addressable whichever group
  # `model` picks. A multi-model table has none.
  c(body_cols[spans[[pick]]], setdiff(all_cols, body_cols[unlist(spans)]))
}

# The default column for a bare inline(tbl, var, level): the row's
# single estimate-like token when unambiguous.
#
# The order is a PREFERENCE over estimate-like tokens, most specific
# first: the regression coefficient (an exponentiated table still
# carries the token "b" -- only its HEADER says OR, IRR or HR, so
# scale-named tokens would be dead entries), then the descriptive
# center (mean before median, bare median before its bracketed
# variant), and only then the count -- the estimate of a categorical
# table and the fallback of everything else.
#
# `"m"`, not `"mean"`: the continuous family's mean column carries the
# token "m" (see the `show_columns` table in `?table_continuous`), so
# the old "mean" entry matched nothing and "n" -- one place earlier --
# won instead. A bare `inline()` on a descriptive table quoted the
# group's N where the sentence meant its mean.
#
# `"delta"` sits beside `"b"` for the same reason, one family over.
# `table_continuous_lm()` emits `"b"` only when `by` is NUMERIC (a
# slope); across the levels of a FACTOR -- the form the function is
# named for -- the estimate is the contrast, token `"delta"`, and the
# table carries no `"b"` at all. Without this entry the preference
# fell through to `"n"`, and a bare `inline(tbl, outcome)` quoted the
# sample size where the sentence meant the mean difference.
#
# A row with the means of the groups and no contrast (a `by` of three or
# more levels, or `contrast = "none"`) has no single estimate: falling
# through to `"n"` would quote the sample size again, so it is refused
# with the address of a mean.
.inline_default_token <- function(s, cols, call = rlang::caller_env()) {
  tokens <- unique(vapply(
    cols[cols %in% names(s$col_meta)],
    function(nm) s$col_meta[[nm]]$token %||% "",
    character(1)
  ))
  if ("emmean" %in% tokens && !any(c("b", "delta") %in% tokens)) {
    groups <- .inline_column_groups(s)
    spicy_abort(
      c(
        "No default column for this row: the table shows the mean of each group, with no contrast.",
        "i" = sprintf(
          "Cite a mean with `model`: `model = %s, column = \"emmean\"`.",
          .quote_val(groups[[1L]])
        ),
        "i" = paste0(
          "Available groups: ",
          paste(.quote_val(groups), collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  for (cand in c("b", "delta", "m", "med", "med_iqr", "n")) {
    if (cand %in% tokens) {
      return(cand)
    }
  }
  spicy_abort(
    c(
      "No default column for this row: pick one with `column`.",
      "i" = paste0(
        "Available tokens: ",
        paste(.quote_val(setdiff(tokens, "")), collapse = ", "),
        "."
      )
    ),
    class = "spicy_invalid_input",
    call = call
  )
}

# Refuse a cell the table itself says carries no value. `inline()`
# exists to keep a number quoted in running text in step with the table,
# so pasting the placeholder of a reference or undefined cell into a
# sentence is the one thing it must not do -- which is what `?inline`
# promises. Shared by the scalar path and by BOTH bounds of an interval:
# the interval path used to skip this and render "[<dash>, <dash>]".
.inline_refuse_status <- function(status, token, call = rlang::caller_env()) {
  if (identical(status, "undefined")) {
    spicy_abort(
      sprintf(
        "The %s cell of this row is undefined in the table (not estimable).",
        .quote_val(token)
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (identical(status, "reference")) {
    spicy_abort(
      sprintf(
        "This row is the reference category: it has no %s value.",
        .quote_val(token)
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# Refuse a cell the table leaves BLANK -- no number, and no status
# saying why. The scalar path always had this guard; the interval path
# did not, so a token whose bounds are empty on this row composed the
# brackets around nothing and returned "[, ]". One producer, so the two
# paths cannot word the same refusal differently.
#
# The refusal used to stop at the period, a dead end: the reader was told
# the cell is empty and nothing else -- not that the column is empty
# throughout, and not that the group means beside it are addressable.
# (`table_continuous_lm(contrast = "none")` printed an empty contrast
# column, the case that exposed it; the column is now left out.) The
# context arguments (the table and the row) are optional so a caller
# without a structured table still gets the bare refusal.
.inline_refuse_empty <- function(
  txt,
  token,
  s = NULL,
  formatted = NULL,
  cols = NULL,
  row = NULL,
  call = rlang::caller_env()
) {
  if (nzchar(txt)) {
    return(invisible(NULL))
  }
  spicy_abort(
    c(
      sprintf(
        "The %s cell of this row is empty in the table.",
        .quote_val(token)
      ),
      .inline_empty_cell_hints(s, token, formatted, cols, row)
    ),
    class = "spicy_invalid_input",
    call = call
  )
}

# The two hints the empty-cell refusal can earn from the table itself.
#
# (1) The column is empty on EVERY row, not just this one -- the table
#     carries no value for the token at all. Stated as a property of the
#     table, never by naming the argument that produced it: `inline()`
#     reads the structured view and has no sight of the call.
# (2) Some other token spreads over columns that carry a group. Those
#     ARE addressable, and `model` is the argument that reaches them --
#     the remedy the reader needs and could not guess. Only the groups
#     whose own cell on this `row` carries a value are named: a level
#     with no data shows the undefined mark there, and a model that could
#     not be fit leaves the means blank, so naming them would send the
#     reader to one more refusal.
.inline_empty_cell_hints <- function(s, token, formatted, cols, row) {
  if (is.null(s) || is.null(formatted) || is.null(cols) || is.null(row)) {
    return(character(0))
  }
  hits <- Filter(
    function(nm) identical(s$col_meta[[nm]]$token %||% "", token),
    cols
  )
  hints <- character(0)
  all_blank <- length(hits) > 0L &&
    all(vapply(
      hits,
      function(nm) !any(nzchar(trimws(formatted[[nm]]))),
      logical(1)
    ))
  if (all_blank) {
    hints <- c(
      hints,
      "i" = sprintf(
        paste0(
          "The %s column is empty on EVERY row: this table carries no ",
          "value for that token."
        ),
        .quote_val(token)
      )
    )
  }
  levels_by_token <- list()
  for (nm in setdiff(cols, hits)) {
    lv <- s$col_meta[[nm]]$level %||% NA_character_
    tk <- s$col_meta[[nm]]$token %||% ""
    filled <- nzchar(trimws(formatted[[nm]][row])) &&
      !nzchar(.struct_cell_status(s, nm)[row])
    if (!is.na(lv) && nzchar(tk) && filled) {
      levels_by_token[[tk]] <- unique(c(levels_by_token[[tk]], lv))
    }
  }
  if (length(levels_by_token) > 0L) {
    tk <- names(levels_by_token)[1L]
    hints <- c(
      hints,
      "i" = sprintf(
        "Per-group columns ARE addressable: `model = %s, column = %s`.",
        .quote_val(levels_by_token[[tk]][1L]),
        .quote_val(tk)
      )
    )
    hints <- c(
      hints,
      "i" = paste0(
        "Available groups: ",
        paste(.quote_val(levels_by_token[[tk]]), collapse = ", "),
        "."
      )
    )
  }
  hints
}

# The refusal of a token the table does not carry. One producer, for the
# up-front check in `inline()` and for `.inline_cell()`, so the message
# reads the same on every family.
.inline_abort_no_token <- function(s, token, call = rlang::caller_env()) {
  tokens <- unique(vapply(
    setdiff(names(s$col_meta), .REG_KEY_VARIABLE),
    function(nm) s$col_meta[[nm]]$token %||% "",
    character(1)
  ))
  spicy_abort(
    c(
      sprintf("No column with token %s in this table.", .quote_val(token)),
      "i" = paste0(
        "Available: ",
        paste(.quote_val(sort(setdiff(tokens, ""))), collapse = ", "),
        "."
      )
    ),
    class = "spicy_invalid_input",
    call = call
  )
}

# One formatted cell by (row, token), the interval composed like the
# console composes it.
.inline_cell <- function(
  s,
  formatted,
  row,
  token,
  cols,
  call = rlang::caller_env()
) {
  hits <- cols[
    vapply(
      cols,
      function(nm) {
        identical(s$col_meta[[nm]]$token %||% "", token)
      },
      logical(1)
    )
  ]
  if (length(hits) == 0L) {
    .inline_abort_no_token(s, token, call = call)
  }
  # An interval token names its two BOUNDS, not two competing columns.
  # Selecting by token FIRST is what keeps "ci" apart from "ame_ci",
  # "med_ci" and "assoc_ci": those are different estimands, not
  # different models, and the old role-only scan reported them as an
  # ambiguity `model` could settle -- on tables that have no models.
  roles <- vapply(
    hits,
    function(nm) s$col_meta[[nm]]$ci_role %||% "",
    character(1)
  )
  if (length(hits) >= 2L && all(nzchar(roles))) {
    pair <- .inline_ci_pair(s, hits, call = call)
    for (nm in pair) {
      .inline_refuse_status(.struct_cell_status(s, nm)[row], token, call = call)
    }
    lo <- trimws(formatted[[pair[1L]]][row])
    hi <- trimws(formatted[[pair[2L]]][row])
    # A blank bound carries no status to refuse on, so the status guard
    # above lets it through: an association interval on a LEVEL row of
    # `table_categorical()` (the measure sits on the variable row) used
    # to compose as "[, ]".
    .inline_refuse_empty(lo, token, s, formatted, cols, row, call = call)
    .inline_refuse_empty(hi, token, s, formatted, cols, row, call = call)
    br <- .style_ci_brackets()
    sep <- .style_ci_sep(
      ci_bracket_separator(s$format_spec$decimal_mark)
    )
    return(paste0(br[[1L]], lo, sep, hi, br[[2L]]))
  }
  if (length(hits) > 1L) {
    # `model` settles it: the columns of a group of `by` carry it in
    # `col_meta$level` (the per-group columns of `table_continuous_lm()`),
    # those of a spanner are the spanners' (a multi-model or a by table).
    groups <- unique(stats::na.omit(vapply(
      hits,
      function(nm) s$col_meta[[nm]]$level %||% NA_character_,
      character(1)
    )))
    choices <- if (length(groups) > 1L) groups else names(s$spanners)
    spicy_abort(
      c(
        sprintf(
          "Token %s matches %d columns: pick one with `model`.",
          .quote_val(token),
          length(hits)
        ),
        "i" = paste0(
          "Available: ",
          paste(.quote_val(choices), collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  .inline_refuse_status(.struct_cell_status(s, hits)[row], token, call = call)
  out <- trimws(formatted[[hits]][row])
  .inline_refuse_empty(out, token, s, formatted, cols, row, call = call)
  out
}

# The two bounds among `cols`, which the caller has already narrowed to
# ONE interval token. A remaining ambiguity is therefore a genuine
# multi-model (or multi-group) one, and `model` is the remedy the
# message names -- `s$spanners` is populated exactly on those tables.
.inline_ci_pair <- function(s, cols, call = rlang::caller_env()) {
  roles <- vapply(
    cols,
    function(nm) s$col_meta[[nm]]$ci_role %||% "",
    character(1)
  )
  lo <- cols[roles == .REG_CI_ROLE_LL]
  hi <- cols[roles == .REG_CI_ROLE_UL]
  if (length(lo) == 1L && length(hi) == 1L) {
    return(c(lo, hi))
  }
  if (length(lo) > 1L || length(hi) > 1L) {
    spicy_abort(
      c(
        "Several confidence intervals match: pick a `model`.",
        "i" = paste0(
          "Available: ",
          paste(.quote_val(names(s$spanners)), collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  spicy_abort(
    "This table displays no confidence interval.",
    class = "spicy_invalid_input",
    call = call
  )
}

# Render a "{token} ... {token}" pattern.
.inline_pattern <- function(
  s,
  formatted,
  row,
  pattern,
  cols,
  call = rlang::caller_env()
) {
  tokens <- regmatches(
    pattern,
    gregexpr("\\{[^{}]+\\}", pattern)
  )[[1L]]
  names_cited <- substring(tokens, 2L, nchar(tokens) - 1L)
  # The interval tokens this pattern cites, IN PATTERN ORDER, so
  # `{ci_label}` can name the interval the sentence is actually
  # quoting.
  cited_intervals <- unique(names_cited[
    vapply(names_cited, .inline_is_interval, logical(1), s = s, cols = cols)
  ])
  out <- pattern
  for (tk in unique(tokens)) {
    name <- substring(tk, 2L, nchar(tk) - 1L)
    value <- if (identical(name, "ci_label")) {
      .inline_ci_label(s, cols, cited_intervals, call = call)
    } else {
      .inline_cell(s, formatted, row, name, cols, call = call)
    }
    out <- gsub(tk, value, out, fixed = TRUE)
  }
  out
}

# Does `token` address an interval (a pair of bounds) in this table?
.inline_is_interval <- function(token, s, cols) {
  for (nm in cols) {
    m <- s$col_meta[[nm]]
    if (identical(m$token %||% "", token) && nzchar(m$ci_role %||% "")) {
      return(TRUE)
    }
  }
  FALSE
}

# The displayed interval label ("95% CI"), read from the col_meta of
# the CI bounds.
#
# A table can carry two intervals with DIFFERENT labels: `show_columns =
# c("m", "ci", "med", "med_ci")` heads them "95% CI" and "Med 95% CI",
# and the console says so. The label therefore belongs to the interval
# the pattern quotes, not to whichever bound comes first in the column
# order -- the same token-blindness the interval lookup itself carried
# until 7fe6fd94, one layer up. `want` is the pattern's interval tokens
# in the order it cites them; the first one wins, which is the interval
# a reader pairs with the label. A pattern citing none (`"{b}
# {ci_label}"`) keeps the plain scan.
.inline_ci_label <- function(
  s,
  cols,
  want = character(0),
  call = rlang::caller_env()
) {
  for (tk in want) {
    for (nm in cols) {
      m <- s$col_meta[[nm]]
      if (identical(m$token %||% "", tk) && !is.null(m$ci_label)) {
        return(m$ci_label)
      }
    }
  }
  for (nm in cols) {
    lbl <- s$col_meta[[nm]]$ci_label
    if (!is.null(lbl)) {
      return(lbl)
    }
  }
  spicy_abort(
    "This table displays no confidence interval.",
    class = "spicy_invalid_input",
    call = call
  )
}
