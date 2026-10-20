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
#' relabeled) label, through its row role. Fit statistics are addressed by their
#' token as `variable` (`inline(tbl, "nobs")`, `inline(tbl, "r2")`) or by
#' their label (`inline(tbl, "n")`). Identity comes first: a source column
#' wins over a token or a label of the same name, and an exact label over
#' a fit statistic's label read regardless of case. A `variable` that
#' still matches the rows of two variables or statistics is refused, and
#' the error names an address for each.
#'
#' A statistic that belongs to a whole variable rather than to one of
#' its levels sits on the variable's own row: the *p* of a
#' `table_categorical()` block, its association measure, its SMD. Leave
#' `level` out to cite it (`inline(tbl, smoking, column = "p")`).
#'
#' [table_continuous_lm()] lays its groups out sideways -- one row per
#' outcome, the `by` levels as columns (`M (Female)`, `M (Male)`) --
#' so there `level` names the group whose column you want:
#' `inline(tbl, bmi, "Female", "emmean")`. The columns that belong to
#' no group (the contrast, its interval, `p`, `n`) are cited without
#' one, as before.
#'
#' [table_multiresponse()] has one row per item, addressed by its column
#' name, then the rows of `none_row`, addressed as `"none"` and
#' `"no_data"`, and the base row, the number of respondents, addressed as
#' the fit statistic `"nobs"`. Its groups are spanners, as in
#' [table_categorical()], so `model` selects one, the margin included:
#' `inline(tbl, smoking, column = "pct", model = "Female")`,
#' `inline(tbl, "nobs", model = "Total")`. The item `p` and `statistic`
#' belong to no group and are cited without `model`
#' (`inline(tbl, smoking, column = "p")`).
#'
#' The column is a **token** of the typed contract (`"b"`, `"se"`,
#' `"p"`, `"ci"`, `"or"`, `"ame"`, `"n"`, `"pct"`, `"m"`, ... -- see
#' `as_structured()`'s `col_meta`), never a display header. `"ci"`
#' composes the interval with the style's brackets and separator, and
#' so does every other interval token the table carries (`"med_ci"`,
#' `"ame_ci"`, `"assoc_ci"`): each addresses its own bounds. In
#' a multi-model table, `model` selects the model by its spanner
#' label or position; in a `by` table, the spanners are the groups,
#' so `model` selects the group the same way.
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
#' models list the spanner labels. A `level` that names a group of a
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
#' @param level For a factor variable, the level, as a string.
#'   `"(Missing)"` addresses the missing-value category by role. On
#'   [table_continuous_lm()], whose groups are columns rather than
#'   rows, it names the group.
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
#'   none of them refuses and lists its tokens.
#' @param model In a multi-model table, the model: its label (as
#'   displayed in the column spanners) or its position.
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
  cols <- .inline_model_cols(s, model)

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
  # One family lays its groups out sideways, so `level` addresses a
  # COLUMN there rather than a row.
  narrowed <- .inline_level_cols(s, rows, level, column, cols)
  cols <- narrowed$cols
  level <- narrowed$level

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
.inline_variable_rows <- function(s, var_chr) {
  body <- s$body
  generated <- .inline_generated_rows(body)
  id <- .inline_row_identity(body, generated)
  ids <- unique(id[.inline_match_rows(body, var_chr, generated)])
  if (length(ids) > 1L) {
    .inline_abort_ambiguous(body, var_chr, ids, id, generated)
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
      class = "spicy_invalid_input"
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
.inline_abort_ambiguous <- function(body, var_chr, ids, id, generated) {
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
    class = "spicy_invalid_input"
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
  cols
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
        cols
      )
      if (!is.null(hdr)) {
        return(hdr)
      }
      spicy_abort(
        c(
          sprintf("%s has levels: pick one with `level`.", .quote_val(var_chr)),
          "i" = paste0(
            "Available: ",
            paste(
              .quote_val(stats::na.omit(body$.level[rows])),
              collapse = ", "
            ),
            "."
          )
        ),
        class = "spicy_invalid_input"
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
      class = "spicy_invalid_input"
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

# `level` as a COLUMN address, on the one family that lays its groups
# out sideways.
#
# `table_continuous_lm()` prints one row per outcome and puts the `by`
# levels in the HEADERS ("M (Female)", "M (Male)"), with the level
# carried as data in `col_meta$level` precisely so a consumer never
# parses the header back. `inline()` reads row identity only, so those
# two columns were an ambiguity it asked the caller to settle with
# `model` -- on a table that has no spanners, listing nothing
# ("Available: ."), while `model` itself refused with "this table has
# no model spanners". Both remedies it named were dead ends, and a
# token of the published contract was unreachable.
#
# The rule is narrow, and both halves are load-bearing: it fires only
# when the variable's own rows carry no `.level` (so `level` cannot be
# a row address) AND the token asked for really does spread over
# several columns that carry one. Everywhere else `level` keeps
# meaning the row it has always meant.
.inline_level_cols <- function(s, rows, level, column, cols) {
  unchanged <- list(cols = cols, level = level)
  if (is.null(level) || any(!is.na(s$body$.level[rows]))) {
    return(unchanged)
  }
  lv <- vapply(
    cols,
    function(nm) s$col_meta[[nm]]$level %||% NA_character_,
    character(1)
  )
  if (all(is.na(lv))) {
    return(unchanged)
  }
  spread <- FALSE
  for (tk in .inline_requested_tokens(column)) {
    same <- .inline_token_cols(s, cols, tk)
    if (length(same) > 1L && any(!is.na(lv[same]))) {
      spread <- TRUE
    }
  }
  if (!spread) {
    return(unchanged)
  }
  pick <- !is.na(lv) & lv == level
  if (!any(pick)) {
    spicy_abort(
      c(
        sprintf("No group %s in this table.", .quote_val(level)),
        "i" = paste0(
          "Available: ",
          paste(.quote_val(unique(stats::na.omit(lv))), collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  # The columns that carry no level at all (`p`, `n`, the contrast and
  # its interval) belong to no group and stay addressable.
  list(cols = cols[is.na(lv) | pick], level = NULL)
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
.inline_header_row <- function(s, formatted, rows, var_chr, tokens, cols) {
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
      class = "spicy_invalid_input"
    )
  }
  NULL
}

# The candidate columns after the `model` filter: NULL model on a
# single-model table keeps every column; on a multi-model table the
# choice is required as soon as a token is ambiguous across models.
.inline_model_cols <- function(s, model) {
  all_cols <- setdiff(names(s$col_meta), .REG_KEY_VARIABLE)
  spans <- s$spanners
  if (is.null(model)) {
    return(all_cols)
  }
  if (length(model) != 1L) {
    spicy_abort(
      sprintf("`model` must be a single value, not length %d.", length(model)),
      class = "spicy_invalid_input"
    )
  }
  if (is.null(spans)) {
    spicy_abort(
      "`model` was supplied but this table has no model spanners.",
      class = "spicy_invalid_input"
    )
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
      class = "spicy_invalid_input"
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
.inline_default_token <- function(s, cols) {
  tokens <- unique(vapply(
    cols[cols %in% names(s$col_meta)],
    function(nm) s$col_meta[[nm]]$token %||% "",
    character(1)
  ))
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
    class = "spicy_invalid_input"
  )
}

# Refuse a cell the table itself says carries no value. `inline()`
# exists to keep a number quoted in running text in step with the table,
# so pasting the placeholder of a reference or undefined cell into a
# sentence is the one thing it must not do -- which is what `?inline`
# promises. Shared by the scalar path and by BOTH bounds of an interval:
# the interval path used to skip this and render "[<dash>, <dash>]".
.inline_refuse_status <- function(status, token) {
  if (identical(status, "undefined")) {
    spicy_abort(
      sprintf(
        "The %s cell of this row is undefined in the table (not estimable).",
        .quote_val(token)
      ),
      class = "spicy_invalid_input"
    )
  }
  if (identical(status, "reference")) {
    spicy_abort(
      sprintf(
        "This row is the reference category: it has no %s value.",
        .quote_val(token)
      ),
      class = "spicy_invalid_input"
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
# The refusal used to stop at the period, which made it a dead end on
# the one table where it is REACHED BY DEFAULT: `table_continuous_lm(
# contrast = "none")` still prints the delta column, empty, and `delta`
# is the token a bare `inline(x, var)` asks for. The reader was told the
# cell is empty and nothing else -- not that the column is empty
# throughout, and not that the group means beside it are addressable.
# The context arguments are optional so a caller without a structured
# table still gets the bare refusal.
.inline_refuse_empty <- function(
  txt,
  token,
  s = NULL,
  formatted = NULL,
  cols = NULL
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
      .inline_empty_cell_hints(s, token, formatted, cols)
    ),
    class = "spicy_invalid_input"
  )
}

# The two hints the empty-cell refusal can earn from the table itself.
#
# (1) The column is empty on EVERY row, not just this one -- the table
#     carries no value for the token at all. Stated as a property of the
#     table, never by naming the argument that produced it: `inline()`
#     reads the structured view and has no sight of the call.
# (2) Some other token spreads over columns that carry a group. Those
#     ARE addressable, and `level` is the argument that reaches them --
#     the remedy the reader needs and could not guess.
.inline_empty_cell_hints <- function(s, token, formatted, cols) {
  if (is.null(s) || is.null(formatted) || is.null(cols)) {
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
    if (!is.na(lv) && nzchar(tk)) {
      levels_by_token[[tk]] <- unique(c(levels_by_token[[tk]], lv))
    }
  }
  if (length(levels_by_token) > 0L) {
    tk <- names(levels_by_token)[1L]
    hints <- c(
      hints,
      "i" = sprintf(
        "Per-group columns ARE addressable: `level = %s, column = %s`.",
        .quote_val(levels_by_token[[tk]][1L]),
        .quote_val(tk)
      )
    )
    hints <- c(
      hints,
      "i" = paste0(
        "Available levels: ",
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
.inline_cell <- function(s, formatted, row, token, cols) {
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
    .inline_abort_no_token(s, token)
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
    pair <- .inline_ci_pair(s, hits)
    for (nm in pair) {
      .inline_refuse_status(.struct_cell_status(s, nm)[row], token)
    }
    lo <- trimws(formatted[[pair[1L]]][row])
    hi <- trimws(formatted[[pair[2L]]][row])
    # A blank bound carries no status to refuse on, so the status guard
    # above lets it through: an association interval on a LEVEL row of
    # `table_categorical()` (the measure sits on the variable row) used
    # to compose as "[, ]".
    .inline_refuse_empty(lo, token, s, formatted, cols)
    .inline_refuse_empty(hi, token, s, formatted, cols)
    br <- .style_ci_brackets()
    sep <- .style_ci_sep(
      ci_bracket_separator(s$format_spec$decimal_mark)
    )
    return(paste0(br[[1L]], lo, sep, hi, br[[2L]]))
  }
  if (length(hits) > 1L) {
    # Which argument settles this one? Columns that carry a group in
    # `col_meta$level` are told apart by `level`; anything else is a
    # multi-model table, where the spanner labels are the choices.
    groups <- unique(stats::na.omit(vapply(
      hits,
      function(nm) s$col_meta[[nm]]$level %||% NA_character_,
      character(1)
    )))
    arg <- if (length(groups) > 1L) "level" else "model"
    choices <- if (length(groups) > 1L) groups else names(s$spanners)
    spicy_abort(
      c(
        sprintf(
          "Token %s matches %d columns: pick one with `%s`.",
          .quote_val(token),
          length(hits),
          arg
        ),
        "i" = paste0(
          "Available: ",
          paste(.quote_val(choices), collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  .inline_refuse_status(.struct_cell_status(s, hits)[row], token)
  out <- trimws(formatted[[hits]][row])
  .inline_refuse_empty(out, token, s, formatted, cols)
  out
}

# The two bounds among `cols`, which the caller has already narrowed to
# ONE interval token. A remaining ambiguity is therefore a genuine
# multi-model (or multi-group) one, and `model` is the remedy the
# message names -- `s$spanners` is populated exactly on those tables.
.inline_ci_pair <- function(s, cols) {
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
      class = "spicy_invalid_input"
    )
  }
  spicy_abort(
    "This table displays no confidence interval.",
    class = "spicy_invalid_input"
  )
}

# Render a "{token} ... {token}" pattern.
.inline_pattern <- function(s, formatted, row, pattern, cols) {
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
      .inline_ci_label(s, cols, cited_intervals)
    } else {
      .inline_cell(s, formatted, row, name, cols)
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
.inline_ci_label <- function(s, cols, want = character(0)) {
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
    class = "spicy_invalid_input"
  )
}
