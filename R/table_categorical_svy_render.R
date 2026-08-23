# Rendering of `table_categorical_svy()`: the display frame, the header
# layer the shared exporter is parameterised with, the typed view and
# the note.
#
# The compute frame this file reads is already keyed by the FROZEN
# display keys (`"E n"`, `"E %"`, `"E % CI lower"`, ...), so nothing
# below re-derives a column name: it formats, labels and groups the
# columns that are there.

# The statistic columns of the frame, in display order, with the block
# and the base each belongs to. One walk, read by the display frame,
# the header layout and the typed view alike.
.cat_svy_columns <- function(x) {
  blocks <- attr(x, "blocks", exact = TRUE)
  proportion_ci <- isTRUE(attr(x, "proportion_ci", exact = TRUE))
  deff_on <- !isFALSE(attr(x, "deff", exact = TRUE))
  out <- list()
  for (b in blocks) {
    entries <- list(
      list(key = .cat_svy_key_n(b), token = "n", base = "n"),
      list(key = .cat_svy_key_pct(b), token = "pct", base = "pct")
    )
    if (proportion_ci) {
      entries <- c(
        entries,
        list(
          list(key = .cat_key_prop_ci_ll(b), token = "prop_ci", base = "ll"),
          list(key = .cat_key_prop_ci_ul(b), token = "prop_ci", base = "ul")
        )
      )
    }
    if (deff_on) {
      entries <- c(
        entries,
        list(list(key = .cat_key_deff(b), token = "deff", base = "deff"))
      )
    }
    for (e in entries) {
      e$block <- b
      out[[length(out) + 1L]] <- e
    }
  }
  if (isTRUE(attr(x, "show_p", exact = TRUE))) {
    out[[length(out) + 1L]] <- list(
      key = .CAT_KEY_P,
      token = "p",
      base = "p",
      block = NA_character_
    )
  }
  out
}

# The header a reader sees over one statistic column, WITHOUT its
# block: the block name is the spanner above it.
#
# Passing `ci_level` asks for the STANDALONE form, for a column that
# carries no spanner over it: an interval bound then has to name its
# own coverage ("95% CI LL"), because "LL" alone over a lone column
# names nothing.
.cat_svy_base_label <- function(base, ci_level = NULL, decimal_mark = ".") {
  bound <- function(role_key) {
    role <- spicy_str(role_key)
    if (is.null(ci_level)) {
      return(role)
    }
    spicy_fmt(
      "header_ci_bound",
      .continuous_ci_label(ci_level, decimal_mark),
      role
    )
  }
  switch(
    base,
    n = spicy_str("header_n_lower"),
    pct = spicy_str("header_percent_symbol"),
    ll = bound("header_ci_ll"),
    ul = bound("header_ci_ul"),
    deff = spicy_str("header_deff"),
    p = spicy_str("header_p")
  )
}

# The display frame: one stub column, then the statistic columns.
#
# The block's own statistic (the p) belongs to the HEADER row, the
# level statistics to the level rows, and the other place is a
# structural blank -- an absence, never the undefined dash, which means
# "applies here but has no value".
.cat_svy_display_df <- function(x) {
  percent_digits <- attr(x, "percent_digits") %||% 1L
  p_digits <- attr(x, "p_digits") %||% 3L
  decimal_mark <- attr(x, "decimal_mark") %||% "."
  indent_text <- attr(x, "indent_text") %||% "  "
  is_header <- x$.row_role == "factor_header"

  df <- stats::setNames(
    data.frame(
      ifelse(is_header, x$label, paste0(indent_text, x$level)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    .CAT_KEY_VARIABLE
  )
  fmt <- function(v, d) {
    out <- formatC(v, format = "f", digits = d)
    if (decimal_mark != ".") {
      out <- sub("\\.", decimal_mark, out)
    }
    ifelse(is.na(v), spicy_str("cell_undefined"), out)
  }
  for (e in .cat_svy_columns(x)) {
    v <- x[[e$key]]
    cell <- switch(
      e$base,
      n = as.character(v),
      p = vapply(
        v,
        function(p) format_p_value(p, decimal_mark, digits = p_digits),
        character(1)
      ),
      deff = fmt(v, 2L),
      fmt(v, percent_digits)
    )
    cell[is.na(v)] <- if (identical(e$base, "p")) "" else cell[is.na(v)]
    # Blank the rows the statistic does not belong to.
    cell[if (identical(e$base, "p")) !is_header else is_header] <- ""
    df[[e$key]] <- cell
  }
  df
}

# The header layer handed to `export_desc_table()`.
#
# The spanners of this table are its `by` BLOCKS, not its interval
# bounds: three columns of the same domain sit under the domain's name,
# and the bound labels ride in the sub-header row like every other
# statistic of the block. That is the whole reason the exporter takes
# its header layer as an argument.
.cat_svy_header_layout <- function(x) {
  decimal_mark <- attr(x, "decimal_mark") %||% "."
  ci_level <- attr(x, "ci_level") %||% 0.95
  margin_key <- attr(x, "margin_key", exact = TRUE)
  cols <- .cat_svy_columns(x)
  keys <- vapply(cols, function(e) e$key, character(1))
  base_labels <- vapply(
    cols,
    function(e) .cat_svy_base_label(e$base),
    character(1)
  )
  # Standalone form: a bound under no spanner names its own coverage.
  standalone <- vapply(
    cols,
    function(e) .cat_svy_base_label(e$base, ci_level, decimal_mark),
    character(1)
  )
  # Full headers, for the console and for a column that spans itself:
  # the block name qualifies the statistic, the same template the
  # sibling family uses.
  full_labels <- vapply(
    seq_along(cols),
    function(i) {
      b <- cols[[i]]$block
      if (is.na(b)) {
        standalone[[i]]
      } else {
        spicy_fmt(
          "header_group_qualified",
          .cat_group_label(b, margin_key),
          standalone[[i]]
        )
      }
    },
    character(1)
  )
  names(full_labels) <- keys
  names(base_labels) <- keys
  # The exporter hands its resolvers the WHOLE column set, stub column
  # included. It is not a statistic of this table, so it is not in
  # `.cat_svy_columns()`; it still needs its header and a spanner of
  # its own, or the two resolvers disagree on the column count.
  full_labels[[.CAT_KEY_VARIABLE]] <- spicy_str("header_variable")
  base_labels[[.CAT_KEY_VARIABLE]] <- spicy_str("header_variable")
  block_of <- function(key) {
    i <- match(key, keys)
    if (is.na(i)) NA_character_ else cols[[i]]$block
  }
  base_of <- function(key) {
    i <- match(key, keys)
    if (is.na(i)) NA_character_ else cols[[i]]$base
  }

  labels_fn <- function(col_keys) {
    out <- unname(full_labels[col_keys])
    miss <- is.na(out)
    out[miss] <- col_keys[miss]
    out
  }
  spanners_fn <- function(col_keys) {
    groups <- list()
    i <- 1L
    n <- length(col_keys)
    while (i <= n) {
      b <- block_of(col_keys[[i]])
      j <- i
      # A block spans the run of adjacent columns that belong to it.
      while (j < n && !is.na(b) && identical(block_of(col_keys[[j + 1L]]), b)) {
        j <- j + 1L
      }
      if (is.na(b)) {
        # A one-way table has no block spanner. Its interval bounds
        # still pair under their coverage, exactly as the continuous
        # family renders them.
        base_i <- base_of(col_keys[[i]])
        if (
          identical(base_i, "ll") &&
            i < n &&
            identical(base_of(col_keys[[i + 1L]]), "ul")
        ) {
          groups[[length(groups) + 1L]] <- list(
            key = .CAT_KEY_PROP_CI_LL,
            label = .continuous_ci_label(ci_level, decimal_mark),
            cols = c(i, i + 1L),
            bounds = unname(base_labels[col_keys[c(i, i + 1L)]])
          )
          i <- i + 2L
          next
        }
        groups[[length(groups) + 1L]] <- list(
          key = col_keys[[i]],
          label = full_labels[[col_keys[[i]]]],
          cols = i
        )
        i <- i + 1L
      } else if (j == i) {
        groups[[length(groups) + 1L]] <- list(
          key = col_keys[[i]],
          label = full_labels[[col_keys[[i]]]],
          cols = i
        )
        i <- i + 1L
      } else {
        groups[[length(groups) + 1L]] <- list(
          key = b,
          label = .cat_group_label(b, margin_key),
          cols = i:j,
          bounds = unname(base_labels[col_keys[i:j]])
        )
        i <- j + 1L
      }
    }
    groups
  }
  list(
    # No renaming: the keys carry the block, which is what makes them
    # unique across the table, and the sub-header row carries the short
    # label. `rename_ci_cols()`'s job -- shortening a bound key because
    # its coverage moved into a spanner -- has no equivalent here.
    rename = function(df) df,
    labels = labels_fn,
    spanners = spanners_fn
  )
}

# The two identity columns the shared geometry predicates read.
.cat_svy_body_geometry <- function(x) {
  role <- x$.row_role
  data.frame(
    .row_role = role,
    .indent = ifelse(role %in% c("level", "missing"), 1L, 0L),
    stringsAsFactors = FALSE
  )
}

# The typed view.
#
# Block geometry of the sibling -- a `factor_header` row per variable
# carrying the block's own p, then one `level` (or `missing`) row per
# category -- and one `col_meta` entry per statistic column, carrying
# the BLOCK it belongs to as data so a consumer never parses "E %" back
# into a group and a statistic.
.build_categorical_svy_structured <- function(x, display_df) {
  percent_digits <- as.integer(attr(x, "percent_digits") %||% 1L)
  p_digits <- as.integer(attr(x, "p_digits") %||% 3L)
  decimal_mark <- attr(x, "decimal_mark") %||% "."
  ci_level <- attr(x, "ci_level") %||% 0.95
  margin_key <- attr(x, "margin_key", exact = TRUE)
  cols <- .cat_svy_columns(x)

  col_names <- character(0)
  col_meta <- list()
  ci_pairs <- list()
  spanners <- list()
  base_of <- list()
  for (e in cols) {
    col_names <- c(col_names, e$key)
    base_of[[e$key]] <- e$base
    meta <- list(
      token = e$token,
      display_label = {
        lab <- .cat_svy_base_label(
          e$base,
          if (identical(e$token, "prop_ci")) ci_level else NULL,
          decimal_mark
        )
        if (is.na(e$block)) {
          lab
        } else {
          spicy_fmt(
            "header_group_qualified",
            .cat_group_label(e$block, margin_key),
            lab
          )
        }
      },
      precision = switch(
        e$base,
        n = 0L,
        p = p_digits,
        deff = 2L,
        percent_digits
      )
    )
    if (!is.na(e$block)) {
      # `group`, not `level`: the same field name the sibling family
      # puts on its group columns, so a consumer written against
      # `table_categorical()` reads this table with no change. The
      # margin is told apart by a FLAG, never by its label -- "Total"
      # is a display string and is auto-renamed on collision.
      meta$group <- e$block
      if (identical(e$block, margin_key)) {
        meta$total <- TRUE
      }
    }
    if (identical(e$base, "p")) {
      meta$p_style <- .style_p_style_token()
      meta$threshold <- .style_p_floor(p_digits)
    }
    if (identical(e$token, "prop_ci")) {
      meta$ci_role <- if (identical(e$base, "ll")) {
        .CON_KEY_CI_LL
      } else {
        .CON_KEY_CI_UL
      }
      meta$ci_label <- .continuous_ci_label(ci_level, decimal_mark)
    }
    col_meta[[e$key]] <- meta
  }
  for (e in cols) {
    if (identical(e$base, "ll")) {
      ul <- .cat_key_prop_ci_ul(e$block)
      col_meta[[e$key]]$ci_pair <- ul
      col_meta[[ul]]$ci_pair <- e$key
      ci_pairs[[length(ci_pairs) + 1L]] <- list(
        label = col_meta[[e$key]]$ci_label,
        cols = .desc_col_index(col_names, c(e$key, ul))
      )
    }
  }
  # The v3 spanner contract of the descriptive families: a NAMED list
  # of column-index vectors, keyed by the group. `inline(model = )`
  # reads those names to disambiguate a token that matches one column
  # per group, so a list of unnamed records would leave every grouped
  # citation unaddressable.
  for (b in attr(x, "blocks", exact = TRUE)) {
    if (is.na(b)) {
      next
    }
    keys_b <- vapply(cols, function(e) e$key, character(1))[
      vapply(cols, function(e) identical(e$block, b), logical(1))
    ]
    spanners[[b]] <- .desc_col_index(col_names, keys_b)
  }

  rows <- list()
  for (i in seq_len(nrow(x))) {
    role <- x$.row_role[[i]]
    is_header <- identical(role, "factor_header")
    values <- list()
    display <- character(0)
    status <- character(0)
    for (nm in col_names) {
      is_p <- identical(base_of[[nm]], "p")
      # The block statistic belongs to the header row, the level
      # statistics to the level rows. The other place is an ABSENCE.
      if (identical(is_p, !is_header)) {
        next
      }
      v <- x[[nm]][[i]]
      if (length(v) == 1L) {
        values[[nm]] <- as.numeric(v)
      }
      shown <- as.character(display_df[[nm]][[i]])
      if (identical(shown, spicy_str("cell_undefined"))) {
        status[[nm]] <- "undefined"
        display[[nm]] <- shown
      }
    }
    rows[[length(rows) + 1L]] <- list(
      label = display_df[[.CAT_KEY_VARIABLE]][[i]],
      values = values,
      variable = x$variable[[i]],
      level = x$level[[i]],
      role = role,
      indent = if (is_header) 0L else 1L,
      display = display,
      status = status
    )
  }

  .desc_assemble(
    rows,
    col_names = col_names,
    col_meta = col_meta,
    format_spec = list(
      decimal_mark = decimal_mark,
      digits = percent_digits,
      percent_digits = percent_digits,
      p_digits = p_digits,
      p_style = .style_p_style_token(),
      p_threshold = .style_p_floor(p_digits),
      ci_level = ci_level
    ),
    spanners = if (length(spanners) > 0L) spanners else NULL,
    ci_pairs = ci_pairs
  )
}

# ---- the footer ------------------------------------------------------------

# The note of a design categorical table: what left the sample, how big
# it is, what design produced the numbers, how the percentages were
# bounded, and how the association was tested.
.cat_svy_note <- function(
  meta,
  degf_dom_used,
  df_user,
  na_dropped,
  user_na_dropped,
  by_na_dropped,
  group_col_name,
  decimal_mark,
  proportion_ci,
  ci_method,
  deff,
  p_value,
  chisq_statistic,
  n_negative_weights = 0L,
  test_refused = "none"
) {
  parts <- c(
    .svy_missing_note(na_dropped, "note_missing_removed"),
    .svy_missing_note(user_na_dropped, "note_declared_missing_removed")
  )
  if (by_na_dropped > 0L) {
    parts <- c(
      parts,
      spicy_fmt("note_rows_missing_by_removed", group_col_name, by_na_dropped)
    )
  }
  parts <- c(
    parts,
    .design_n_note(
      meta$n_obs,
      meta$sum_weights,
      digits = 0L,
      decimal_mark = decimal_mark
    )
  )
  # The df span the DOMAINS carry, never the caller's `df`: the design
  # line states a fact about the design, and the caller cannot change
  # it. What `df` moves is the reference distribution of the intervals,
  # and the third sentence names the number it moved to -- the same
  # division of labour as `table_continuous_svy()`.
  lines <- .design_note_lines(
    meta,
    degf_range = if (length(degf_dom_used) > 0L) {
      range(degf_dom_used)
    } else {
      NULL
    }
  )
  if (!is.null(df_user)) {
    lines[[3L]] <- spicy_fmt(
      "note_design_df_supplied",
      as.integer(df_user)
    )
  }
  parts <- c(parts, lines)
  # The same three regimes as the continuous twin, from the same
  # helper: `svychisq()` is refused under negative weights too
  # (decision 36 / ARB-2, applied twin-symmetrically), and the clause
  # says whether it reached every variable or only some of them.
  parts <- c(
    parts,
    .design_negative_weights_note(
      n_negative_weights,
      meta$n_obs,
      test_refused = test_refused
    )
  )
  if (isTRUE(proportion_ci)) {
    parts <- c(parts, spicy_fmt("note_ci_prop_method", ci_method))
  }
  if (identical(deff, "replace")) {
    parts <- c(parts, spicy_str("note_deff_replace"))
  }
  # The method line names the test that RAN. When every comparison was
  # refused there is none, and the sentence above has already said so;
  # in a mixed table it is true of the ones that were served, and the
  # scoped refusal names the rest. Same division as the continuous
  # twin, where the line is driven by `test_label`.
  if (isTRUE(p_value) && !identical(test_refused, "all")) {
    parts <- c(
      parts,
      spicy_fmt("note_group_comparison", .cat_svy_test_label(chisq_statistic))
    )
  }
  parts <- c(
    parts,
    spicy_fmt("note_gloss_pct_svy", spicy_str("header_percent_symbol")),
    spicy_fmt("note_gloss_n_svy", spicy_str("header_n_lower"))
  )
  if (!isFALSE(deff)) {
    parts <- c(parts, spicy_fmt("note_gloss_deff", spicy_str("header_deff")))
  }
  paste_note_parts(parts)
}
