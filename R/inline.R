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
#' `level = "(Missing)"` whatever its displayed (possibly deduplicated)
#' label, through its row role. Fit statistics are addressed by their
#' token as `variable` (`inline(tbl, "n")`, `inline(tbl, "r2")`).
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
#' models list the spanner labels. A cell the table itself displays
#' as undefined (an aliased coefficient's en-dash) refuses with the
#' reason rather than pasting a dash into a sentence.
#'
#' @param x A table returned by [table_regression()],
#'   [table_categorical()], [table_continuous()], or
#'   [table_continuous_lm()] (default output).
#' @param variable The source variable, unquoted or as a string; or a
#'   fit-statistic token (`"n"`, `"r2"`, ...).
#' @param level For a factor variable, the level, as a string.
#'   `"(Missing)"` addresses the missing-value category by role.
#' @param column A column token, or a `{token}` pattern. `NULL` (the
#'   default) returns the estimate-like column of the row when it is
#'   unambiguous: the family's primary estimate. That is the
#'   coefficient (`"b"`, or the exponentiated scale the table displays
#'   -- `"or"`, `"irr"`, `"hr"`, ...) for [table_regression()] and
#'   [table_continuous_lm()], the mean (`"m"`) for
#'   [table_continuous()], and the count (`"n"`) for
#'   [table_categorical()]. A row carrying none of them refuses and
#'   lists its tokens.
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
  s <- as_structured(x)
  formatted <- .format_structured_to_string_body(s)

  var_chr <- .inline_variable_chr(rlang::enquo(variable))
  row <- .inline_resolve_row(s, var_chr, level)
  cols <- .inline_model_cols(s, model)

  if (is.null(column)) {
    column <- .inline_default_token(s, row, cols)
  }
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

# Resolve the body row by identity, with the documented conveniences.
.inline_resolve_row <- function(s, var_chr, level) {
  body <- s$body
  rows <- which(body$.variable == var_chr)

  if (length(rows) == 0L) {
    # Fit statistic by token-as-variable: "n", "r2", ...
    fit_rows <- which(body$.row_role == "fit_stat")
    fit_hit <- fit_rows[
      vapply(
        fit_rows,
        function(i) {
          identical(body$.variable[i], var_chr) ||
            identical(tolower(body$Variable[i]), tolower(var_chr))
        },
        logical(1)
      )
    ]
    if (length(fit_hit) == 1L) {
      return(fit_hit)
    }
    # Convenience: match the displayed label (the console suffixes
    # factor headers with a colon; strip it before comparing).
    shown <- sub(":$", "", trimws(body$Variable))
    lbl_rows <- which(
      shown == var_chr & body$.row_role == "factor_header"
    )
    if (length(lbl_rows) == 0L) {
      lbl_rows <- which(shown == var_chr)
    }
    if (length(lbl_rows) >= 1L) {
      var_chr <- body$.variable[lbl_rows[1L]]
      rows <- which(body$.variable == var_chr)
    }
  }
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

  has_levels <- any(!is.na(body$.level[rows]))
  if (is.null(level)) {
    if (has_levels && length(rows) > 1L) {
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
  hit <- if (identical(level, "(Missing)")) {
    # By ROLE: the displayed label may be deduplicated or translated.
    rows[body$.row_role[rows] == "missing"]
  } else {
    rows[!is.na(body$.level[rows]) & body$.level[rows] == level]
  }
  if (length(hit) != 1L) {
    spicy_abort(
      c(
        sprintf("No level %s for %s.", .quote_val(level), .quote_val(var_chr)),
        "i" = paste0(
          "Available: ",
          paste(.quote_val(stats::na.omit(body$.level[rows])), collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  hit
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
  body_cols[spans[[pick]]]
}

# The default column for a bare inline(tbl, var, level): the row's
# single estimate-like token when unambiguous.
#
# The order is a PREFERENCE over estimate-like tokens, most specific
# first: the exponentiated regression scales, then the coefficient,
# then the descriptive mean, and only then the count -- which is the
# estimate of a categorical table and the fallback of everything else.
#
# `"m"`, not `"mean"`: the continuous family's mean column carries the
# token "m" (see the `show_columns` table in `?table_continuous`), so
# the old "mean" entry matched nothing and "n" -- one place earlier --
# won instead. A bare `inline()` on a descriptive table quoted the
# group's N where the sentence meant its mean.
.inline_default_token <- function(s, row, cols) {
  tokens <- unique(vapply(
    cols[cols %in% names(s$col_meta)],
    function(nm) s$col_meta[[nm]]$token %||% "",
    character(1)
  ))
  for (cand in c("or", "irr", "hr", "rr", "mr", "exp", "b", "m", "n")) {
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
.inline_refuse_empty <- function(txt, token) {
  if (!nzchar(txt)) {
    spicy_abort(
      sprintf(
        "The %s cell of this row is empty in the table.",
        .quote_val(token)
      ),
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
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
          paste(
            .quote_val(sort(setdiff(tokens, ""))),
            collapse = ", "
          ),
          "."
        )
      ),
      class = "spicy_invalid_input"
    )
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
    .inline_refuse_empty(lo, token)
    .inline_refuse_empty(hi, token)
    br <- .style_ci_brackets()
    sep <- .style_ci_sep(
      ci_bracket_separator(s$format_spec$decimal_mark)
    )
    return(paste0(br[[1L]], lo, sep, hi, br[[2L]]))
  }
  if (length(hits) > 1L) {
    spicy_abort(
      c(
        sprintf(
          "Token %s matches %d columns: pick one with `model`.",
          .quote_val(token),
          length(hits)
        ),
        "i" = paste0(
          "Available: ",
          paste(.quote_val(names(s$spanners)), collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  .inline_refuse_status(.struct_cell_status(s, hits)[row], token)
  out <- trimws(formatted[[hits]][row])
  .inline_refuse_empty(out, token)
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
