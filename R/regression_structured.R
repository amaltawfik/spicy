# Structured (typed) view of the regression body.
#
# Layer 3.5: produces a numeric body that parallels the character body
# returned by render_regression_table(). Engines (Excel, gt, tinytable,
# flextable) consume this structured view directly instead of re-
# parsing the character body. This eliminates the duplicated
# string-to-numeric round-trip and gives each engine clean access to
# raw values + per-cell markers + format spec.
#
# The structured body is attached to render_regression_table()'s
# return value as `attr(body, "structured")`. The character body
# (primary return) is unchanged for backward compatibility with
# print() / clipboard / current test snapshots.
#
# The DESCRIPTIVE families (table_categorical / table_continuous /
# table_continuous_lm) attach the same view under the same attribute,
# built by R/tables_structured.R. The schema below is normative for
# them too; only the `token` vocabulary and the set of row roles a
# family emits differ.
#
# Schema:
#
#   structured = list(
#     version = integer(),          # contract version (see
#                                   #   .spicy_structured_version()).
#     body = data.frame(...),       # numeric body, CI pre-split (LL/UL),
#                                   #   followed by the four dot-prefixed
#                                   #   IDENTITY columns (v3):
#                                   #  .variable chr source variable, or
#                                   #                the fit-stat token
#                                   #  .level    chr factor level (NA
#                                   #                outside a factor)
#                                   #  .row_role chr "coef" |
#                                   #                "factor_header" |
#                                   #                "level" |
#                                   #                "reference" |
#                                   #                "fit_stat" |
#                                   #                "outcome" | "vc" |
#                                   #                "summary" | "group" |
#                                   #                "missing"
#                                   #                (the last three are
#                                   #                emitted by the
#                                   #                descriptive families)
#                                   #  .indent   int display indent (0/1)
#                                   #
#                                   # The identity of a row is DATA, filled
#                                   # at construction from the long frame
#                                   # (factor_term / factor_level / term /
#                                   # estimate_type / is_reference), never
#                                   # re-parsed from the displayed string.
#                                   # They sit AFTER the value columns so
#                                   # `body[[1L]]` is still `Variable` and
#                                   # every column index in `spanners` /
#                                   # `ci_pairs` keeps pointing at the same
#                                   # value column. Use
#                                   # `.struct_value_cols()` to iterate the
#                                   # value columns only.
#
#     cell_status = list(           # per-CELL semantics, keyed by
#                                   #   structured col name, one character
#                                   #   vector of nrow(body) per column
#                                   #   that needs one:
#                                   #     ""          nothing special
#                                   #     "reference" reference level of a
#                                   #                 factor IN THIS BLOCK
#                                   #                 (display en-dash)
#                                   #     "undefined" the statistic applies
#                                   #                 to the row but no
#                                   #                 number expresses it
#                                   #                 (display en-dash)
#                                   #   A cell with no status whose value
#                                   #   is NA is ABSENT (display blank).
#                                   #   Columns with no marked cell are
#                                   #   left out entirely.
#
#     col_meta = list(              # per-column metadata, keyed by
#                                   # structured body col name
#       <col_name> = list(
#         token = "b" | "se" | "ci_low" | "ci_high" | "p" | "stat" |
#                 "df" | "beta" | "ame" | "partial_f2" |
#                 "partial_eta2" | "partial_omega2" | "partial_chi2" |
#                 "ame_se" | "ame_p" | "fit_stat_<key>",
#         model_id = chr,
#         precision = int,          # decimals (0L for nobs, etc.)
#         p_style = NULL | "apa",   # APA = drop leading zero
#         threshold = NULL | num,   # for p columns, "<X" display
#                                   # below this value
#         ci_pair = NULL | chr,     # for ci_low/ci_high: the paired
#                                   # col name (so engines can rebuild
#                                   # the "[L, U]" spanner)
#         ci_role = NULL | "LL" | "UL",
#         ci_label = NULL | chr,    # e.g., "95% CI" for spanner
#         fit_stat = NULL | chr,    # for fit-stat cols, the token
#                                   # ("nobs", "r2", etc.)
#         value_range = NULL | num(2), # bounds a `p_style = "apa"`
#                                   # column is validated against;
#                                   # default c(0, 1). Signed
#                                   # association measures declare
#                                   # c(-1, 1).
#         level = NULL | chr,       # descriptive: the `by` level a
#                                   # per-level column reports.
#         group = NULL | chr,       # descriptive: the `by` group a
#                                   # per-group column belongs to.
#         total = NULL | TRUE,      # descriptive: the column belongs
#                                   # to the MARGIN group. A flag, not
#                                   # a label match: "Total" is a
#                                   # display string and is auto-
#                                   # renamed on collision.
#         display_cells = NULL | chr  # per-cell display override, len
#                                   # nrow(body), NA where the numeric
#                                   # value formats normally. Carries
#                                   # composite cells no single number
#                                   # can express -- today the
#                                   # "events/N" counts of the
#                                   # `n_events` token. Any engine that
#                                   # renders strings MUST prefer it.
#       ),
#       ...
#     ),
#
#     stars = NULL | list(          # significance markers (NULL when
#                                   #   `stars = FALSE`)
#       thresholds = c("***" = .001, ...),  # symbol -> p cutoff
#       markers = list(<col_name> = chr)    # per-cell marker, "" where
#                                           #   none, only for the
#                                           #   p-bearing columns that
#                                           #   carry stars
#     ),
#
#     spanners = list(              # model-level groupings on
#                                   #   structured columns (multi-model)
#       <model_label> = integer()
#     ),
#     ci_pairs = list(              # CI pairs in structured cols
#       list(label = "95% CI", cols = c(ll_idx, ul_idx)),
#       ...
#     ),
#
#     format_spec = list(           # global format defaults
#       decimal_mark = "." | ",",
#       digits = int,
#       p_digits = int,
#       effect_size_digits = int,
#       fit_digits = int,
#       ic_digits = int,
#       p_style = "apa" | "standard",
#       p_threshold = num,          # 10^(-p_digits)
#       ci_level = num
#     )
#   )
#

# Version of the structured contract produced by this spicy.
#   1: initial typed view (implicit -- views built before the field
#      existed carry no `version` at all).
#   2: `col_meta$display_cells` per-cell overrides, `stars`, the fixest
#      "Fixed effects:" block registered in `factor_header_rows` /
#      `level_rows`.
#   3: row identity moves from index vectors into body COLUMNS
#      (`.variable`, `.level`, `.row_role`, `.indent`) and cell
#      semantics into `cell_status`. The five v2 index vectors
#      (`reference_rows`, `factor_header_rows`, `fit_stat_rows`,
#      `level_rows`, `outcome_row`) and `reference_models_by_row` are
#      REMOVED, not derived: row indices are the structure that
#      corrupts as soon as two bodies are stacked or merged, and the
#      per-row reference flag was overwriting cells of estimate blocks
#      that have no per-level reference at all. A v2 object is refused
#      by `as_structured()` rather than read as if it were a v3.
.spicy_structured_version <- function() 3L

# The dot-prefixed identity columns of the v3 body, in emission order.
# They are appended AFTER the value columns so every existing column
# index (spanners, ci_pairs, engine loops) keeps its meaning.
.STRUCT_META_COLS <- c(".variable", ".level", ".row_role", ".indent")

# The row roles a v3 body may carry. Regression roles first, then the
# three the descriptive families add (R/tables_structured.R). The
# vocabulary is EXTENDED, never re-worded: a role says what a row IS,
# so it is the key a consumer matches on -- never the displayed label,
# which is a display string ("(Missing)", auto-renamed on collision,
# translatable) and can change without the row changing.
#   summary  a row that summarises one variable with no sub-key (a
#            continuous variable, a modelled outcome).
#   group    a row keyed by one level of the `by` variable.
#   missing  a row keyed by the MISSING value -- the "(Missing)"
#            category of a categorical table, the missing-`by` group
#            of a continuous one. Takes precedence over `level` /
#            `group`: what the row is, is the missing key.
.STRUCT_ROW_ROLES <- c(
  "coef",
  "factor_header",
  "level",
  "reference",
  "fit_stat",
  "outcome",
  "vc",
  "summary",
  "group",
  "missing"
)

# Value (displayable) columns of a structured body: everything but the
# `Variable` label and the dot-prefixed identity columns. Every consumer
# that iterates "the numeric columns" goes through this, so adding an
# identity column never has to be reflected in a loop bound again.
.struct_value_cols <- function(body) {
  setdiff(names(body), c("Variable", .STRUCT_META_COLS))
}

# The body as engines lay it out: the label column plus the value
# columns, identity columns dropped.
.struct_display_body <- function(body) {
  body[, c("Variable", .struct_value_cols(body)), drop = FALSE]
}

# Row indices whose label is displayed indented (factor levels, the
# rows of a subordinate block, the absorbed factors of a fixed-effects
# disclosure). Replaces the v2 `level_rows` component.
.struct_indent_rows <- function(struct) {
  ind <- struct$body[[".indent"]]
  if (is.null(ind)) {
    return(integer(0)) # nocov -- v3 bodies always carry the column
  }
  which(!is.na(ind) & ind > 0L)
}

# Row index of the optional multi-DV outcome row (integer(0) if none).
# Replaces the v2 `outcome_row` component.
.struct_outcome_row <- function(struct) {
  role <- struct$body[[".row_role"]]
  if (is.null(role)) {
    return(integer(0)) # nocov -- v3 bodies always carry the column
  }
  which(role == "outcome")
}

# Per-cell status of column `col_name` (character vector of nrow(body),
# "" where the cell carries no special semantics).
.struct_cell_status <- function(struct, col_name) {
  st <- struct$cell_status[[col_name]]
  if (is.null(st)) {
    return(rep("", nrow(struct$body)))
  }
  st
}

# Turn a per-row map of named cell overrides into per-column vectors of
# length `n_rows`. Columns with no override anywhere are dropped, so the
# contract stays free of empty vectors.
.collect_cell_overrides <- function(by_row, col_names, n_rows, empty) {
  touched <- unique(unlist(lapply(by_row, names), use.names = FALSE))
  touched <- intersect(col_names, touched)
  if (length(touched) == 0L) {
    return(list())
  }
  out <- list()
  for (col_name in touched) {
    vec <- rep(empty, n_rows)
    for (row_key in names(by_row)) {
      vals <- by_row[[row_key]]
      if (col_name %in% names(vals)) {
        vec[as.integer(row_key)] <- vals[[col_name]]
      }
    }
    out[[col_name]] <- vec
  }
  out
}

build_structured_body <- function(
  aligned,
  show_columns,
  show_fit_stats,
  reference_style,
  factor_layout,
  ci_level,
  digits,
  p_digits,
  effect_size_digits,
  fit_digits,
  ic_digits,
  decimal_mark,
  reference_label,
  outcome_labels,
  labels_from_outcomes,
  model_ids,
  label_map,
  col_spec,
  labels = NULL,
  model_outcomes = NULL,
  model_outcome_labels = NULL,
  ci_label = "CI",
  stars_map = NULL,
  re_columns = c("est", "se", "ci")
) {
  group_factor_levels <- identical(factor_layout, "grouped")
  coefs <- aligned$coefs_aligned

  # ---- Resolve per-col_spec structured-column expansion ------------------
  # For each col_spec entry, decide how many numeric cols it produces:
  #   * single field tokens (b/se/t/p/beta/ame/ame_se/ame_p/partial_*):
  #       1 col, name = col_spec$col_name
  #   * ci tokens (fields = c("ci_low","ci_high")):
  #       2 cols, names = "<col_name>: LL" and "<col_name>: UL"
  #       (or just "LL"/"UL" if user-friendly; we use the full prefix
  #       to keep multi-model disambiguation)
  #   * partial_chi2 (fields = c("estimate","df")):
  #       2 cols, names = "<col_name>" (est) and "<col_name>: df"
  # ci_label mirrors the console header: "CI" for frequentist tables,
  # "CrI" / "HDI" for all-Bayesian ones -- the rich engines (gt /
  # flextable / tinytable / Excel) display this string as the interval
  # spanner, so hardcoding "CI" here would contradict the console and
  # the documented relabel.
  ci_pct <- formatC(ci_level * 100, format = "g")
  ci_label_str <- paste0(ci_pct, "% ", ci_label)

  expanded <- list() # list of (struct_col_name, source_field, meta)
  for (cs in col_spec) {
    if (
      length(cs$fields) == 2L &&
        identical(cs$fields, c("ci_low", "ci_high"))
    ) {
      ll_name <- paste0(cs$col_name, ": LL")
      ul_name <- paste0(cs$col_name, ": UL")
      expanded[[length(expanded) + 1L]] <- list(
        name = ll_name,
        source = "ci_low",
        cs = cs,
        ci_role = "LL",
        ci_pair = ul_name,
        ci_label = ci_label_str
      )
      expanded[[length(expanded) + 1L]] <- list(
        name = ul_name,
        source = "ci_high",
        cs = cs,
        ci_role = "UL",
        ci_pair = ll_name,
        ci_label = ci_label_str
      )
    } else if (
      length(cs$fields) == 2L &&
        identical(cs$fields, c("estimate", "df"))
    ) {
      # partial_chi2: keep estimate + df as separate numeric cols.
      est_name <- cs$col_name
      df_name <- paste0(cs$col_name, ": df")
      expanded[[length(expanded) + 1L]] <- list(
        name = est_name,
        source = "estimate",
        cs = cs
      )
      expanded[[length(expanded) + 1L]] <- list(
        name = df_name,
        source = "df",
        cs = cs,
        is_df = TRUE
      )
    } else {
      # Single field.
      expanded[[length(expanded) + 1L]] <- list(
        name = cs$col_name,
        source = cs$fields[1L],
        cs = cs
      )
    }
  }
  struct_col_names <- vapply(expanded, `[[`, character(1), "name")

  # ---- Build col_meta keyed by structured col name -----------------------
  col_meta <- list()
  for (e in expanded) {
    cs <- e$cs
    token <- cs$token
    # A p-value column is any column whose SOURCE FIELD is the p-value,
    # whichever estimate block it belongs to: B, AME, or a survival
    # estimand (`rmst_p`, `risk_diff_p`). The console decides the same
    # way (`format_cell_value()` branches on `field == "p_value"`), so
    # keying on the field rather than on a list of token names keeps a
    # newly added p column from silently rendering as a generic
    # 2-decimal number -- which turned a p of .00098 into "0.00".
    is_p_col <- identical(e$source, "p_value")
    # Precision selection mirrors format_cell_value() / format_fit_stat_value()
    prec <- if (
      token %in%
        c(
          "partial_f2",
          "partial_f2_ci",
          "partial_eta2",
          "partial_eta2_ci",
          "partial_omega2",
          "partial_omega2_ci",
          "partial_chi2"
        )
    ) {
      effect_size_digits
    } else if (is_p_col) {
      p_digits
    } else if (identical(token, "pd")) {
      # Posterior probability: p-column style (see the console
      # renderer) -- the generic 2-decimal cell is blind exactly
      # where pd lives (.95 to 1).
      p_digits
    } else if (token %in% c("r2", "adj_r2")) {
      # Per-fit variance explained: same precision as the fit-statistics
      # R^2 row it can also appear as (`fit_digits`).
      fit_digits
    } else if (token %in% c("n", "n_events")) {
      # Counts, not estimates: `digits` governs the estimation columns
      # only, so N stays "364" and never "364.00" / "364.000". The
      # console renderer already formats n_obs / events as integers.
      0L
    } else if (token %in% c("ess_bulk", "ess_tail")) {
      0L # effective SAMPLE SIZES: integers, never "959.60"
    } else if (identical(token, "rhat")) {
      3L # the 1.01 convergence target needs them
    } else {
      digits
    }
    # df column inside partial_chi2 is integer-valued.
    if (isTRUE(e$is_df)) {
      prec <- 0L
    }

    # The leading-zero policy of bounded columns is spicy's APA drop
    # by default; a journal style may pin it to "standard".
    p_style <- if (is_p_col || identical(token, "pd")) {
      .style_p_style_token()
    } else {
      NULL
    }
    threshold <- if (is_p_col) .style_p_floor(p_digits) else NULL

    col_meta[[e$name]] <- list(
      token = token,
      model_id = cs$model_id,
      source_field = e$source,
      precision = as.integer(prec),
      p_style = p_style,
      threshold = threshold,
      # MCSE spans orders of magnitude across coefficient scales: the
      # string-driven engines render 2 SIGNIFICANT digits (the console
      # convention), not a fixed decimal count.
      signif = if (identical(token, "mcse")) 2L else NULL,
      ci_role = e$ci_role,
      ci_pair = e$ci_pair,
      ci_label = e$ci_label,
      is_df = isTRUE(e$is_df),
      # Bare display label for engines (e.g. "SE", "p", "AME"),
      # stripped of the dedup `.N` suffix and of any "Model X: "
      # prefix that's carried separately by `model_id`.
      display_label = cs$display_label %||% cs$col_name
    )
  }

  # ---- Build body rows --------------------------------------------------
  # Same iteration order as render_regression_table:
  #   1. outcome row (optional)
  #   2. for each term: optional factor header + coef row
  #   3. fit-stat rows
  empty_row <- as.data.frame(
    c(
      list(Variable = NA_character_),
      stats::setNames(
        rep(list(NA_real_), length(struct_col_names)),
        struct_col_names
      )
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  rows <- list()
  # Per-row display overrides, star markers and cell statuses, keyed by
  # structured col name. Collected row by row (the builders below know
  # the source long-format cells); assembled into per-column vectors
  # once the row count is final.
  display_by_row <- list()
  stars_by_row <- list()
  status_by_row <- list()
  # Row IDENTITY, filled here from the long frame. `push_row()` is the
  # single place a row enters the body, so a row can never reach the
  # contract without saying what it is.
  meta_variable <- character(0)
  meta_level <- character(0)
  meta_role <- character(0)
  meta_indent <- integer(0)
  push_row <- function(row, variable, level, role, indent = 0L) {
    rows[[length(rows) + 1L]] <<- row
    # Coerce explicitly: a `factor_term` / `factor_level` that reached
    # the frame as a factor would otherwise land as its integer code.
    meta_variable[[length(meta_variable) + 1L]] <<- as.character(variable)
    meta_level[[length(meta_level) + 1L]] <<- as.character(level)
    meta_role[[length(meta_role) + 1L]] <<- role
    meta_indent[[length(meta_indent) + 1L]] <<- as.integer(indent)
    length(rows)
  }

  # --- Outcome row (multi-DV, when applicable) ---
  # Mirrors build_outcome_row(); for the structured body we just put
  # the outcome LABEL in the corresponding first sub-column of each
  # model (and Variable = "Outcome").
  outcome_row <- .build_structured_outcome_row(
    model_outcomes = model_outcomes,
    model_outcome_labels = model_outcome_labels,
    outcome_labels = if (isTRUE(labels_from_outcomes)) {
      FALSE
    } else {
      outcome_labels
    },
    model_ids = model_ids,
    label_map = label_map,
    col_spec = col_spec,
    expanded = expanded,
    empty_row = empty_row
  )
  outcome_labels_by_col <- character(0)
  if (!is.null(outcome_row)) {
    push_row(
      outcome_row$row,
      variable = NA_character_,
      level = NA_character_,
      role = "outcome"
    )
    outcome_labels_by_col <- outcome_row$labels_by_col
  }

  # --- Body rows (per term) ---
  term_meta <- unique(coefs[, c(
    "term",
    "order_idx",
    "is_reference",
    "is_intercept",
    "factor_term",
    "factor_level"
  )])
  term_meta <- term_meta[order(term_meta$order_idx), , drop = FALSE]
  rownames(term_meta) <- NULL

  ref_level_map <- aligned$factor_ref_levels %||%
    stats::setNames(character(0), character(0))

  current_factor <- NA_character_
  for (i in seq_len(nrow(term_meta))) {
    rt <- term_meta[i, , drop = FALSE]

    if (
      isTRUE(group_factor_levels) &&
        !is.na(rt$factor_term) &&
        !identical(rt$factor_term, current_factor)
    ) {
      # Factor header row: Variable = factor label, all numerics NA.
      header_label <- .resolve_factor_header_label(
        rt$factor_term,
        reference_style,
        ref_level_map,
        labels
      )
      hdr <- empty_row
      hdr$Variable <- header_label
      push_row(
        hdr,
        variable = rt$factor_term,
        level = NA_character_,
        role = "factor_header"
      )
      current_factor <- rt$factor_term
    }
    new_factor_row <- !is.na(rt$factor_term) &&
      !identical(rt$factor_term, current_factor)
    if (is.na(rt$factor_term)) {
      current_factor <- NA_character_
    }

    built <- .build_structured_body_row(
      rt,
      coefs,
      col_spec,
      expanded,
      reference_label = reference_label,
      reference_style = reference_style,
      group_factor_levels = group_factor_levels,
      labels = labels,
      empty_row = empty_row,
      stars_map = stars_map,
      show_columns = show_columns,
      re_columns = re_columns
    )
    new_row <- built$row
    display_by_row[[as.character(length(rows) + 1L)]] <- built$display
    stars_by_row[[as.character(length(rows) + 1L)]] <- built$stars
    status_by_row[[as.character(length(rows) + 1L)]] <- built$status
    # Annotation injection for flat layout (same logic as renderer)
    if (
      identical(reference_style, "annotation") &&
        !isTRUE(group_factor_levels) &&
        isTRUE(new_factor_row) &&
        rt$factor_term %in% names(ref_level_map)
    ) {
      ref_lvl_flat <- ref_level_map[[rt$factor_term]]
      if (!is.na(ref_lvl_flat) && nzchar(ref_lvl_flat)) {
        new_row$Variable <- paste0(new_row$Variable, " [vs ", ref_lvl_flat, "]")
        current_factor <- rt$factor_term
      }
    }
    push_row(
      new_row,
      variable = if (is.na(rt$factor_term)) rt$term else rt$factor_term,
      level = rt$factor_level,
      role = .struct_term_role(
        rt,
        unique(coefs$estimate_type[coefs$term == rt$term])
      ),
      # Indent depth read off the SAME branches format_term_label()
      # uses to decide whether to prefix two spaces, instead of
      # grepping the rendered label back for leading whitespace (which
      # a user-supplied label starting with a space would defeat).
      indent = if (
        isTRUE(group_factor_levels) &&
          !isTRUE(rt$is_intercept) &&
          (isTRUE(rt$is_reference) || !is.na(rt$factor_term))
      ) {
        1L
      } else {
        0L
      }
    )
  }

  # --- Fit-stat rows ---
  fit_stats <- aligned$fit_stats_aligned
  if (
    length(show_fit_stats) > 0L && !is.null(fit_stats) && nrow(fit_stats) > 0L
  ) {
    fit_rows <- .build_structured_fit_stat_rows(
      fit_stats,
      show_fit_stats,
      model_ids,
      col_spec,
      expanded,
      empty_row = empty_row,
      digits = digits,
      fit_digits = fit_digits,
      ic_digits = ic_digits,
      p_digits = p_digits,
      n_groups_by_model = aligned$n_groups_by_model,
      fixef_by_model = aligned$fixef_by_model,
      blank_models = aligned$blank_fit_stats_models
    )
    for (fr in fit_rows) {
      # Block-shaped disclosures (today: the fixest "Fixed effects:"
      # block) carry the same row geometry as a factor group in the
      # coefficients: a header row with no cells, then one indented row
      # per absorbed factor. `.build_structured_fit_stat_rows()` names
      # the role and the indent; recording them here is what lets every
      # engine draw the block the way the console does.
      row_idx <- push_row(
        fr$row,
        variable = fr$variable,
        level = fr$level %||% NA_character_,
        role = fr$role %||% "fit_stat",
        indent = fr$indent %||% 0L
      )
      if (length(fr$status) > 0L) {
        status_by_row[[as.character(row_idx)]] <- fr$status
      }
      # Extend col_meta with per-fit-stat token annotation for the
      # column where the value lives. Precision and (for p_change) APA
      # style come from .build_structured_fit_stat_rows()'s per-token
      # logic.
      for (col_name in names(fr$col_overrides)) {
        ov <- fr$col_overrides[[col_name]]
        # We store the fit-stat-specific metadata in a per-row override
        # map (col_meta is COLUMN-level; per-row precision differences
        # are recorded by the structured body's row index + col_meta
        # union). For simplicity here we stash via a side-list.
        ov$row <- row_idx
        col_meta[[col_name]]$fit_stat_overrides <- c(
          col_meta[[col_name]]$fit_stat_overrides %||% list(),
          list(ov)
        )
      }
    }
  }

  if (length(rows) == 0L) {
    body_df <- empty_row[-1L, , drop = FALSE] # zero-row data.frame
  } else {
    body_df <- do.call(rbind, rows)
    rownames(body_df) <- NULL
  }
  # ---- Row identity columns (v3) ---------------------------------------
  # Appended AFTER the value columns: `body[[1L]]` stays `Variable` and
  # every column index already published (`spanners`, `ci_pairs`) keeps
  # pointing at the same value column.
  body_df[[".variable"]] <- as.character(meta_variable)
  body_df[[".level"]] <- as.character(meta_level)
  body_df[[".row_role"]] <- as.character(meta_role)
  body_df[[".indent"]] <- as.integer(meta_indent)

  # ---- Per-cell display overrides + star markers ------------------------
  # Both are column-shaped vectors over the final body: `display` lands
  # in each column's `col_meta` entry (it is a property of that column's
  # cells), the star markers in the top-level `stars` component (they
  # belong to a display option, not to the column's type).
  n_body_rows <- nrow(body_df)
  display_cols <- .collect_cell_overrides(
    display_by_row,
    struct_col_names,
    n_body_rows,
    empty = NA_character_
  )
  for (col_name in names(display_cols)) {
    col_meta[[col_name]][["display_cells"]] <- display_cols[[col_name]]
  }
  star_cols <- .collect_cell_overrides(
    stars_by_row,
    struct_col_names,
    n_body_rows,
    empty = ""
  )
  # Present whenever stars were requested, even when no cell qualified:
  # the footer legend documents the cutoffs either way, and a consumer
  # reading `NULL` there would conclude the table has no star
  # convention at all.
  stars <- if (is.null(stars_map)) {
    NULL
  } else {
    list(thresholds = stars_map, markers = star_cols)
  }
  # Per-cell semantics (v3). Scoped to the CELL, never to the row: a
  # reference level is a reference only in the estimate blocks that have
  # one, so the AME reference of an ordered factor no longer blanks the
  # polynomial-contrast B / p cells of the same row.
  cell_status <- .collect_cell_overrides(
    status_by_row,
    struct_col_names,
    n_body_rows,
    empty = ""
  )

  # ---- Spanners on structured columns -----------------------------------
  # The char body's spanners map "label -> integer body col indices
  # (excluding Variable)". For the structured body we map the same
  # labels to structured col indices.
  spanners <- .build_structured_spanners(struct_col_names, expanded, label_map)

  # ---- CI pairs ---------------------------------------------------------
  ci_pairs <- list()
  for (e in expanded) {
    if (identical(e$ci_role, "LL")) {
      ll_idx <- match(e$name, struct_col_names) + 1L # +1 for Variable
      ul_idx <- match(e$ci_pair, struct_col_names) + 1L
      ci_pairs[[length(ci_pairs) + 1L]] <- list(
        label = e$ci_label,
        cols = c(ll_idx, ul_idx)
      )
    }
  }

  # ---- Format spec ------------------------------------------------------
  format_spec <- list(
    decimal_mark = decimal_mark,
    digits = as.integer(digits),
    p_digits = as.integer(p_digits),
    effect_size_digits = as.integer(effect_size_digits),
    fit_digits = as.integer(fit_digits),
    ic_digits = as.integer(ic_digits),
    p_style = .style_p_style_token(),
    p_threshold = .style_p_floor(p_digits),
    ci_level = ci_level
  )

  structured <- list(
    version = .spicy_structured_version(),
    body = body_df,
    stars = stars,
    cell_status = cell_status,
    outcome_labels_by_col = outcome_labels_by_col,
    col_meta = col_meta,
    spanners = spanners,
    ci_pairs = ci_pairs,
    format_spec = format_spec
  )
  .validate_structured(structured)
  structured
}


# ---- Invariant validation (Niveau B) ------------------------------------
#
# Sanity-check the structured body BEFORE handing it to engines. Catches
# typing / range / pairing errors early (single source of failure: the
# renderer) instead of letting them propagate as silent display bugs
# in downstream outputs.
#
# Validates:
#   * Variable column is character.
#   * Every value column is numeric (or all-NA, which coerces
#     to logical -- we accept that as the empty / placeholder case).
#   * The identity columns are present, correctly typed, and carry
#     only known row roles.
#   * For each p-column (token "p" / "ame_p" / "p_change" with
#     `p_style = "apa"`): values are in [0, 1] (or NA).
#   * For each CI pair (`ci_pairs[[k]]`): `LL <= UL` (or both NA),
#     row-by-row.
#   * `precision` per col is a non-negative integer.
#   * `format_spec$decimal_mark` is "." or ",".
#
# Violations emit a single concise warning (not an error) so the
# table still renders -- this lets users see the output AND see the
# diagnostic. The renderer's caller (table_regression()) is the
# right layer to escalate to an error if desired.
.validate_structured <- function(struct) {
  body <- struct$body
  problems <- character(0)
  n_rows <- nrow(body)
  value_cols <- .struct_value_cols(body)

  if (!is.character(body[[1L]])) {
    problems <- c(problems, "Variable column is not character.")
  }
  for (col_name in value_cols) {
    col <- body[[col_name]]
    if (!is.numeric(col) && !all(is.na(col))) {
      problems <- c(
        problems,
        sprintf("Column %s is not numeric.", col_name)
      )
    }
  }

  # Identity columns: present, right type, known roles. A body that
  # loses them stops being addressable by (variable, level) -- the one
  # property v3 exists to guarantee.
  missing_meta <- setdiff(.STRUCT_META_COLS, names(body))
  if (length(missing_meta) > 0L) {
    problems <- c(
      problems,
      sprintf(
        "Identity column(s) missing: %s.",
        paste(missing_meta, collapse = ", ")
      )
    )
  } else {
    if (!is.character(body[[".row_role"]])) {
      problems <- c(problems, "`.row_role` is not character.")
    } else {
      unknown <- setdiff(unique(body[[".row_role"]]), .STRUCT_ROW_ROLES)
      if (length(unknown) > 0L) {
        problems <- c(
          problems,
          sprintf("Unknown row role(s): %s.", paste(unknown, collapse = ", "))
        )
      }
    }
    if (!is.integer(body[[".indent"]])) {
      problems <- c(problems, "`.indent` is not an integer vector.")
    }
  }

  # Per-cell statuses are column-shaped, like the display overrides and
  # the star markers: a short vector would silently drop cell semantics
  # in every engine.
  for (col_name in names(struct$cell_status)) {
    st <- struct$cell_status[[col_name]]
    if (!is.character(st) || length(st) != n_rows) {
      problems <- c(
        problems,
        sprintf(
          "Column %s: `cell_status` must be a character vector of length %d.",
          col_name,
          n_rows
        )
      )
    } else {
      bad_st <- setdiff(unique(st), c("", "reference", "undefined"))
      if (length(bad_st) > 0L) {
        problems <- c(
          problems,
          sprintf(
            "Column %s: unknown cell status(es): %s.",
            col_name,
            paste(bad_st, collapse = ", ")
          )
        )
      }
    }
  }

  # Bounded-value range. `p_style = "apa"` marks the columns that drop
  # the leading zero, which are exactly the bounded ones: p-values and
  # posterior probabilities in [0, 1], and (descriptive tables) the
  # association measures, signed in [-1, 1] for the ordinal family.
  # A column may therefore declare its own `value_range`; the default
  # is the [0, 1] of a probability.
  # Skip fit-stat cells: when the user reorders or
  # restricts `show_columns` (e.g. asks for just `c("p")`), the
  # fit-stat values (n / R² / AIC ...) are written to the FIRST
  # numeric column of the per-model block, which may happen to be
  # the `p` column. Those values aren't p-values -- they belong to a
  # fit-stat row, or (block-shaped disclosures) to a cell carrying a
  # `fit_stat` override. Validate only the coefficient cells.
  role <- body[[".row_role"]] %||% rep("coef", n_rows)
  fit_rows <- which(role == "fit_stat")
  for (col_name in value_cols) {
    meta <- struct$col_meta[[col_name]]
    if (is.null(meta)) {
      next
    }
    if (!is.null(meta$p_style)) {
      vals <- body[[col_name]]
      if (is.numeric(vals)) {
        ov_rows <- vapply(
          meta$fit_stat_overrides %||% list(),
          function(ov) as.integer(ov$row),
          integer(1)
        )
        coef_idx <- setdiff(seq_along(vals), c(fit_rows, ov_rows))
        coef_vals <- vals[coef_idx]
        declared <- meta$value_range
        rng <- declared %||% c(0, 1)
        bad <- !is.na(coef_vals) & (coef_vals < rng[1L] | coef_vals > rng[2L])
        if (any(bad)) {
          problems <- c(
            problems,
            if (is.null(declared)) {
              sprintf(
                "Column %s: %d p-value(s) outside [0, 1].",
                col_name,
                sum(bad)
              )
            } else {
              sprintf(
                "Column %s: %d value(s) outside [%s, %s].",
                col_name,
                sum(bad),
                format(rng[1L]),
                format(rng[2L])
              )
            }
          )
        }
      }
    }
  }

  # CI pair invariants
  for (cs in struct$ci_pairs) {
    if (length(cs$cols) == 2L) {
      ll_col <- names(body)[cs$cols[1L]]
      ul_col <- names(body)[cs$cols[2L]]
      ll <- body[[ll_col]]
      ul <- body[[ul_col]]
      if (is.numeric(ll) && is.numeric(ul)) {
        bad <- !is.na(ll) & !is.na(ul) & ll > ul
        if (any(bad)) {
          problems <- c(
            problems,
            sprintf(
              "CI pair %s / %s: %d row(s) have LL > UL.",
              ll_col,
              ul_col,
              sum(bad)
            )
          )
        }
      }
    }
  }

  # Precision per col
  for (col_name in names(struct$col_meta)) {
    prec <- struct$col_meta[[col_name]]$precision
    if (!is.null(prec) && (!is.numeric(prec) || prec < 0L)) {
      problems <- c(
        problems,
        sprintf(
          "Column %s: precision must be a non-negative integer (got %s).",
          col_name,
          paste(prec, collapse = " ")
        )
      )
    }
  }

  # Per-cell display overrides and star markers are column-shaped: a
  # vector shorter than the body would silently drop cells in every
  # string-driven engine.
  for (col_name in names(struct$col_meta)) {
    disp <- struct$col_meta[[col_name]][["display_cells"]]
    if (!is.null(disp) && (!is.character(disp) || length(disp) != n_rows)) {
      problems <- c(
        problems,
        sprintf(
          "Column %s: `display_cells` must be a character vector of %d.",
          col_name,
          n_rows
        )
      )
    }
  }
  for (col_name in names(struct$stars$markers)) {
    mk <- struct$stars$markers[[col_name]]
    if (!is.character(mk) || length(mk) != n_rows) {
      problems <- c(
        problems,
        sprintf(
          "Column %s: star markers must be a character vector of length %d.",
          col_name,
          n_rows
        )
      )
    }
  }

  # decimal_mark
  dm <- struct$format_spec$decimal_mark
  # Any single character: "." and "," are the usual pair, but a journal
  # style may ask for another mark (The Lancet's midline dot, U+00B7).
  if (!(is.character(dm) && length(dm) == 1L && !is.na(dm) && nchar(dm) == 1L)) {
    problems <- c(
      problems,
      sprintf("decimal_mark must be a single character (got '%s').", dm)
    )
  }

  if (length(problems) > 0L) {
    spicy_warn(
      paste0(
        "Structured regression body failed invariant checks:\n  - ",
        paste(problems, collapse = "\n  - ")
      ),
      class = "spicy_internal_invariant"
    )
  }
  invisible(struct)
}


# ---- Row role ------------------------------------------------------------

# The display role of a coefficient row, read off the long frame. A
# reference level is announced as such first (it is what a reader sees
# and what a renderer styles); a variance component next (its cells are
# not coefficients and never take stars); then any row that belongs to a
# factor / subordinate block; then a plain coefficient.
.struct_term_role <- function(rt, estimate_types) {
  if (isTRUE(rt$is_reference)) {
    return("reference")
  }
  if ("vc" %in% estimate_types) {
    return("vc")
  }
  if (!is.na(rt$factor_term)) {
    return("level")
  }
  "coef"
}


# ---- Body row builder (structured) ---------------------------------------

.build_structured_body_row <- function(
  rt,
  coefs,
  col_spec,
  expanded,
  reference_label,
  reference_style,
  group_factor_levels,
  labels,
  empty_row,
  stars_map = NULL,
  show_columns = character(0),
  re_columns = c("est", "se", "ci")
) {
  # Variable label: identical to char body's format_term_label().
  row <- empty_row
  row$Variable <- format_term_label(
    rt,
    reference_label,
    reference_style,
    group_factor_levels,
    labels
  )
  display <- character(0)
  stars <- character(0)
  status <- character(0)

  for (e in expanded) {
    cs <- e$cs
    # Outcome event counts are a COMPOSITE cell ("events/N"): the typed
    # body carries the numerator, the display override carries the
    # string the console prints. Reference levels keep theirs -- the
    # counts are data about the level, not an estimate (STROBE item 16),
    # exactly as build_body_row() exempts the token from the en-dash.
    is_events <- identical(cs$fields, c("events", "events_n"))
    # Random-effect variance rows (estimate_type = "vc") display on the B
    # (estimate / SE / CI) axis: alias "vc" to the "B" column here, mirroring
    # the char body's build_body_row().
    et_match <- if (identical(cs$estimate_type, "B")) {
      c("B", "vc")
    } else {
      cs$estimate_type
    }
    long_row <- coefs[
      coefs$model_id == cs$model_id &
        coefs$term == rt$term &
        coefs$estimate_type %in% et_match,
      ,
      drop = FALSE
    ]
    # Per-category AME columns are tagged with an `outcome_level`;
    # narrow to that category so each column pulls its own cell -- the
    # same filter build_body_row() applies. Without it every category
    # column of an ordinal / multinomial AME received the FIRST
    # category's number. A NULL/NA tag (B columns, single-outcome AME)
    # leaves the match untouched.
    if (
      !is.null(cs$outcome_level) &&
        !is.na(cs$outcome_level) &&
        "outcome_level" %in% names(long_row) &&
        nrow(long_row) > 0L
    ) {
      long_row <- long_row[
        long_row$outcome_level %in% cs$outcome_level,
        ,
        drop = FALSE
      ]
    }
    if (nrow(long_row) == 0L) {
      next
    } # term ABSENT from this model / block: cell stays NA, renders blank
    # Reference check, per CELL and per model, exactly as
    # build_body_row() does it for the character body. Scoping it to the
    # row instead (what v2 did) put an en-dash on estimate blocks that
    # have no per-level reference at all: an ordered factor shows AME
    # contrasts against a baseline level while its B / p cells hold
    # polynomial trends, and those were being blanked out.
    if (isTRUE(long_row$is_reference[1L]) && !identical(cs$token, "n_events")) {
      status[[e$name]] <- "reference"
      next
    }
    if (is_events) {
      ev <- long_row$events[1L]
      nn <- long_row$events_n[1L]
      if (!is.na(ev) && !is.na(nn)) {
        row[[e$name]] <- as.numeric(ev)
        display[[e$name]] <- paste0(
          format(as.integer(ev)),
          "/",
          format(as.integer(nn))
        )
      }
      next
    }
    val <- long_row[[e$source]][1L]
    if (!is.null(val) && length(val) == 1L) {
      row[[e$name]] <- as.numeric(val)
    }
    # Variance-component cells the console en-dashes: the statistic
    # applies to the row but no number expresses it -- either because it
    # is not computable for that component, or because `re_columns`
    # deselected it (a DISPLAY choice; the value stays in the typed body
    # and in `broom::tidy()`). The predicate is shared with
    # build_body_row(), so the character body and the typed body cannot
    # word the same cell differently again.
    if (.vc_cell_undefined(long_row, cs, re_columns)) {
      status[[e$name]] <- "undefined"
      next
    }
    # Generalised NA-with-term-present (decision 12, 2026-08-14). The
    # console's generic NA branch (format_cell_value()) prints an
    # en-dash for any NA that survives the field-specific blanks:
    # "the statistic applies to this row, no number expresses it" --
    # an aliased coefficient in a rank-deficient fit, an extractor
    # with no SE for a term. Mirror that branch exactly: the fields
    # the console deliberately BLANKS on NA are exempt
    # (.blank_on_na_fields, kept beside the console branches), and
    # every other NA-with-term-present cell is marked undefined, so a
    # Word or Excel table says what the console says instead of a
    # blank that reads as "nothing to report".
    if (
      is.na(row[[e$name]]) &&
        !e$source %in% .blank_on_na_fields()
    ) {
      status[[e$name]] <- "undefined"
      next
    }
    # Star marker for this cell, under exactly the conditions
    # format_cell_value() applies one: the B column always, beta only
    # when B is not displayed beside it, AME on its own p-value -- and
    # never on a variance component, whose optional p is a
    # model-comparison test. NA estimates carry no marker (the cell
    # renders as an en-dash).
    if (
      !is.null(stars_map) &&
        !is.na(row[[e$name]]) &&
        identical(e$source, "estimate") &&
        (identical(cs$token, "b") ||
          identical(cs$token, "ame") ||
          (identical(cs$token, "beta") && !"b" %in% show_columns)) &&
        !identical(long_row$estimate_type[1L], "vc")
    ) {
      marker <- format_stars(long_row$p_value[1L], stars_map)
      if (nzchar(marker)) {
        stars[[e$name]] <- marker
      }
    }
  }
  list(row = row, display = display, stars = stars, status = status)
}


# ---- Fit-stat rows (structured) ------------------------------------------

.build_structured_fit_stat_rows <- function(
  fit_stats,
  show_fit_stats,
  model_ids,
  col_spec,
  expanded,
  empty_row,
  digits,
  fit_digits,
  ic_digits,
  p_digits,
  n_groups_by_model = NULL,
  fixef_by_model = NULL,
  blank_models = NULL
) {
  # Each fit-stat row puts the value in the FIRST structured sub-column
  # of each model (i.e., the col_name of the first col_spec entry per
  # model, which in the structured expansion is the FIRST expanded
  # entry for that model). For CI tokens the first entry is the LL
  # column; we re-route fit-stat values to the *estimate* (first non-
  # CI) entry per model so the value lands on the natural display
  # column, matching the char body's behaviour.
  first_struct_col_per_model <- stats::setNames(
    rep(NA_character_, length(model_ids)),
    model_ids
  )
  for (e in expanded) {
    m_id <- e$cs$model_id
    if (is.na(first_struct_col_per_model[[m_id]])) {
      # Skip CI sub-columns: pick the first non-ci col_spec entry.
      if (is.null(e$ci_role)) {
        first_struct_col_per_model[[m_id]] <- e$name
      }
    }
  }

  rows <- list()
  for (tk in show_fit_stats) {
    # Fixed-effects disclosure block, numeric-typed for the structured
    # body: one row per absorbed factor labelled "FE: <factor>"
    # (modelsummary's get_gof convention), cells 1 (absorbed) / 0
    # (fixest model without this factor) / NA (non-fixest model). The
    # char console body carries the grouped Yes/No block instead.
    if (identical(tk, "fixed_effects")) {
      fe <- .fixed_effects_cells(fixef_by_model, model_ids)
      if (is.null(fe)) {
        next
      }
      # Block header row, mirroring the console body: a grouped
      # "Fixed effects:" gloss above the absorbed factors, whose own
      # rows carry the factor name bare (indented like any factor
      # level). The internal "FE: <factor>" key never reaches a reader.
      hdr <- empty_row
      hdr$Variable <- "Fixed effects:"
      rows[[length(rows) + 1L]] <- list(
        row = hdr,
        col_overrides = list(),
        variable = tk,
        level = NA_character_,
        role = "factor_header",
        indent = 0L
      )
      for (fct in fe$factors) {
        row <- empty_row
        row$Variable <- paste0("  ", fct)
        col_overrides <- list()
        for (m_id in model_ids) {
          target_col <- first_struct_col_per_model[[m_id]]
          if (is.na(target_col)) {
            next
          }
          cell <- fe$cells[fct, m_id]
          row[[target_col]] <- switch(cell, Yes = 1, No = 0, NA_real_)
          col_overrides[[target_col]] <- list(
            fit_stat = tk,
            precision = 0L,
            p_style = NULL,
            threshold = NULL
          )
        }
        rows[[length(rows) + 1L]] <- list(
          row = row,
          col_overrides = col_overrides,
          variable = tk,
          level = fct,
          role = "level",
          indent = 1L
        )
      }
      next
    }
    # n_groups: one numeric "N (<factor>)" row per grouping factor
    # (union across models), mirroring the console renderer.
    if (identical(tk, "n_groups")) {
      ngl_all <- n_groups_by_model %||% list()
      fct_union <- character(0)
      for (m_id in model_ids) {
        ng <- ngl_all[[m_id]]
        if (!is.null(ng) && length(ng) > 0L) {
          fct_union <- union(fct_union, names(ng))
        }
      }
      if (length(fct_union) == 0L) {
        next
      }
      for (fct in fct_union) {
        row <- empty_row
        row$Variable <- sprintf("N (%s)", fct)
        col_overrides <- list()
        for (m_id in model_ids) {
          target_col <- first_struct_col_per_model[[m_id]]
          if (is.na(target_col)) {
            next
          }
          ng <- ngl_all[[m_id]]
          row[[target_col]] <- if (!is.null(ng) && fct %in% names(ng)) {
            as.numeric(ng[[fct]])
          } else {
            NA_real_
          }
          col_overrides[[target_col]] <- list(
            fit_stat = tk,
            precision = 0L,
            p_style = NULL,
            threshold = NULL
          )
        }
        rows[[length(rows) + 1L]] <- list(
          row = row,
          col_overrides = col_overrides,
          variable = tk,
          level = fct,
          role = "fit_stat",
          indent = 0L
        )
      }
      next
    }
    if (!tk %in% names(fit_stats)) {
      next
    }
    # Drop the row when NO model carries a value, exactly as
    # build_fit_stats_rows() does for the console. Restricting the skip
    # to `icc` left the univariable screen -- whose model-level stats
    # are all NA by construction -- emitting empty `n` / `AIC` rows in
    # every structured-driven engine.
    if (all(is.na(fit_stats[[tk]]))) {
      next
    }
    row <- empty_row
    row$Variable <- fit_stat_label(tk)
    col_overrides <- list()
    status <- character(0)

    # Per-token precision: same logic as format_fit_stat_value()
    prec <- .fit_stat_precision(
      tk,
      digits = digits,
      fit_digits = fit_digits,
      ic_digits = ic_digits,
      p_digits = p_digits
    )
    p_style <- if (identical(tk, "p_change")) .style_p_style_token() else NULL
    threshold <- if (identical(tk, "p_change")) .style_p_floor(p_digits) else NULL
    is_change_p <- identical(tk, "p_change")

    for (m_id in model_ids) {
      # Display-blank models (multinom category pseudo-columns, the
      # univariable-screen bundle): leave the cell NA WITHOUT a
      # fit-stat override, so the string formatter renders the blank
      # instead of the mixed-table en-dash.
      if (m_id %in% blank_models) {
        next
      }
      target_col <- first_struct_col_per_model[[m_id]]
      if (is.na(target_col)) {
        next
      }
      sub <- fit_stats[fit_stats$model_id == m_id, , drop = FALSE]
      if (nrow(sub) == 0L) {
        next
      }
      val <- sub[[tk]][1L]
      # nocov start: nrow(sub) >= 1 is guaranteed by the guard above, so
      # sub[[tk]][1L] is always a scalar (NA at worst), never NULL.
      if (is.null(val)) {
        val <- NA_real_
      }
      # nocov end
      row[[target_col]] <- as.numeric(val)

      col_overrides[[target_col]] <- list(
        fit_stat = tk,
        precision = as.integer(prec),
        p_style = p_style,
        threshold = threshold
      )
      # A fit statistic that is not defined for this model's class in a
      # mixed table: the console prints an en-dash, so the cell is
      # UNDEFINED, not absent. Emitting the status here (rather than
      # re-deriving it in the string formatter) keeps one source of
      # truth for the dash. The two block-shaped disclosures keep their
      # own console convention (blank) and are handled above; the
      # per-level `n_events` counts likewise.
      if (is.na(val) && !tk %in% c("fixed_effects", "n_groups", "n_events")) {
        status[[target_col]] <- "undefined"
      }
    }
    rows[[length(rows) + 1L]] <- list(
      row = row,
      col_overrides = col_overrides,
      status = status,
      variable = tk,
      level = NA_character_,
      role = "fit_stat",
      indent = 0L
    )
  }
  rows
}

.fit_stat_precision <- function(
  token,
  digits,
  fit_digits,
  ic_digits,
  p_digits
) {
  # n_events included: the console path (format_fit_stat_value) has
  # always formatted it as an integer, but this structured-path bucket
  # missed it, so rich outputs rendered "165.00" (caught in the GEE
  # pass, 2026-07).
  is_int <- token %in%
    c("nobs", "weighted_nobs", "n_groups", "n_events", "max_cluster_size")
  is_fit <- token %in%
    c(
      "r2",
      "adj_r2",
      "omega2",
      "f2",
      "sigma",
      "rmse",
      "pseudo_r2_mcfadden",
      "pseudo_r2_nagelkerke",
      "pseudo_r2_tjur",
      "theta",
      "alpha",
      "phi",
      "scale",
      "within_r2",
      "r2_bayes",
      "r2_marginal",
      "r2_conditional",
      "icc",
      "r2_change",
      "adj_r2_change",
      "f2_change",
      "f_change"
    )
  is_ic <- token %in%
    c(
      "aic",
      "aicc",
      "bic",
      "elpd_loo",
      "looic",
      "waic",
      "qic",
      "qicu",
      "aic_change",
      "aicc_change",
      "bic_change"
    )
  is_p <- identical(token, "p_change")
  if (is_int) {
    return(0L)
  }
  if (is_p) {
    return(as.integer(p_digits))
  }
  if (is_fit) {
    return(as.integer(fit_digits))
  }
  if (is_ic) {
    return(as.integer(ic_digits))
  }
  as.integer(digits)
}


# ---- Outcome row (structured) --------------------------------------------

# Mirrors build_outcome_row() (char body): the Outcome row appears ONLY when
# the user explicitly passes `outcome_labels = c(...)` and there are >= 2
# models. The typed body stays numeric: the row is all-NA, and the label
# TEXT lives in the returned `labels_by_col` map (keyed by the first non-CI
# structured column of each model). String-producing layers -- the shared
# string-body formatter and the Excel writer -- overlay the text, so every
# engine shows the same row print() shows (finding B-structured-outcome).
.build_structured_outcome_row <- function(
  model_outcomes,
  model_outcome_labels,
  outcome_labels,
  model_ids,
  label_map,
  col_spec,
  expanded,
  empty_row
) {
  # Same suppression logic as build_outcome_row().
  if (isFALSE(outcome_labels) || is.null(outcome_labels)) {
    return(NULL)
  }
  if (!is.character(outcome_labels)) {
    return(NULL)
  } # nocov
  if (length(model_ids) <= 1L) {
    return(NULL)
  }

  # First non-CI structured sub-column of each model.
  first_col_per_model <- stats::setNames(
    rep(NA_character_, length(model_ids)),
    model_ids
  )
  for (e in expanded) {
    m_id <- e$cs$model_id
    if (is.na(first_col_per_model[[m_id]]) && is.null(e$ci_role)) {
      first_col_per_model[[m_id]] <- e$name
    }
  }

  labels_by_col <- character(0)
  for (i in seq_along(model_ids)) {
    target <- first_col_per_model[[model_ids[i]]]
    if (is.na(target)) {
      next
    } # nocov
    labels_by_col[[target]] <- outcome_labels[i]
  }
  if (length(labels_by_col) == 0L) {
    return(NULL)
  } # nocov

  row <- empty_row
  row$Variable <- "Outcome"
  list(row = row, labels_by_col = labels_by_col)
}


# ---- Factor header label resolution --------------------------------------

.resolve_factor_header_label <- function(
  factor_term,
  reference_style,
  ref_level_map,
  labels
) {
  # Mirrors build_factor_header_row()'s Variable cell content.
  lbl <- resolve_label(factor_term, labels)
  base <- paste0(lbl, ":")
  if (
    identical(reference_style, "annotation") &&
      factor_term %in% names(ref_level_map)
  ) {
    ref_lvl <- ref_level_map[[factor_term]]
    if (!is.na(ref_lvl) && nzchar(ref_lvl)) {
      return(paste0(base, " [ref: ", ref_lvl, "]"))
    }
  }
  base
}


# ---- Spanner builder (structured) ----------------------------------------

# ---- Engine helpers (shared by output_excel / output_gt / ...) ----------

# Excel `numfmt` code from precision + p_style.
#   precision = 0L   -> "0" (integer display)
#   precision = N    -> "0.<N zeros>" (e.g., "0.00")
#   p_style = "apa"  -> "#.<N zeros>" so leading-zero "0" is dropped
#                       on the displayed value (".005" not "0.005")
# `decimal_mark` is irrelevant: Excel format codes always use "."
# internally; the rendered separator follows the cell's locale.
.excel_numfmt <- function(precision, p_style) {
  precision <- as.integer(precision)
  if (is.na(precision) || precision < 0L) {
    precision <- 0L
  }
  if (precision == 0L) {
    return("0")
  }
  zeros <- strrep("0", precision)
  if (identical(p_style, "apa")) {
    return(paste0("#.", zeros))
  }
  paste0("0.", zeros)
}

# Below-threshold display text used in p-columns when |p| < threshold.
# Example: threshold = 0.001 -> "<.001" (US) or "<,001" (EU).
.below_threshold_text <- function(threshold, decimal_mark = ".") {
  if (is.null(threshold) || !is.finite(threshold) || threshold <= 0) {
    return(NULL)
  }
  paste0(
    "<",
    .strip_leading_zero(
      .format_p_floor(threshold, decimal_mark),
      decimal_mark,
      .style_p_leading_zero()
    )
  )
}

# Render a single cell of the structured body to its display string,
# applying precision, APA p-style (drop leading zero), the en-dash of a
# reference / undefined cell, and below-threshold "<.001" overrides.
# Used by engines that drive their formatters via pre-formatted strings
# (flextable, tinytable, Excel, clipboard, console-from-structured).
#
# `status` is the cell's own semantics (`cell_status`, v3): "reference"
# and "undefined" both display the en-dash, "" defers to the value --
# and an NA value with no status is an ABSENT cell, which displays
# blank. The caller passes the cell's status, never a row index: a cell
# is what it is regardless of where its row sits.
.cell_to_string <- function(
  val,
  row_idx,
  col_meta_entry,
  status = "",
  decimal_mark = ".",
  star = ""
) {
  # Per-cell display override (composite cells: the "events/N" counts).
  # It outranks every rule below, including the en-dash -- the override
  # exists precisely because no single number expresses the cell.
  disp <- col_meta_entry[["display_cells"]]
  if (!is.null(disp) && length(disp) >= row_idx && !is.na(disp[[row_idx]])) {
    return(disp[[row_idx]])
  }
  # "reference": no estimate by design, in THIS block and THIS model.
  # "undefined": the statistic applies but no number expresses it (the
  # SE of a variance component, a fit statistic undefined for this
  # model's class in a mixed table).
  if (nzchar(status)) {
    return("\u2013")
  }
  cfmt <- .resolve_cell_fmt(col_meta_entry, row_idx)
  if (is.na(val)) {
    # Absent: the term / statistic does not exist for this cell.
    return("")
  }
  # Fixed-effects disclosure cells are numeric-encoded in the
  # structured body (1 = absorbed, 0 = not, NA = non-fixest model) but
  # every string-driven engine (tinytable, flextable / Word, Excel,
  # console-from-structured) must DISPLAY the etable / esttab text
  # standard -- a raw "1" would read like a coefficient.
  if (identical(cfmt$fit_stat, "fixed_effects")) {
    return(if (val >= 0.5) "Yes" else "No")
  }
  # Significant-digits columns (MCSE): mirror the console renderer's
  # formatC g-style -- a fixed decimal count misleads across
  # coefficient scales.
  if (!is.null(cfmt$signif)) {
    out <- sub(
      "\\.$",
      "",
      formatC(val, digits = as.integer(cfmt$signif), format = "g", flag = "#")
    )
    if (!identical(decimal_mark, ".")) {
      out <- sub(".", decimal_mark, out, fixed = TRUE) # nocov
    }
    return(out)
  }
  if (!is.null(cfmt$threshold) && is.finite(val) && val < cfmt$threshold) {
    return(.below_threshold_text(cfmt$threshold, decimal_mark))
  }
  s <- format_number(val, cfmt$precision, decimal_mark)
  if (identical(cfmt$p_style, "apa")) {
    s <- sub("^0(?=[\\.,])", "", s, perl = TRUE)
    s <- sub("^-0(?=[\\.,])", "-", s, perl = TRUE)
  }
  # Significance stars suffix the displayed estimate, exactly as the
  # console renderer does -- the footer legend documents markers that
  # must exist in the body.
  if (nzchar(star)) {
    s <- paste0(s, star)
  }
  s
}

# Pad each cell of a (display) CHAR body with figure-spaces (U+2007)
# so every cell in a numeric column has the same width on both sides
# of the decimal mark. With center-aligned (or right-aligned) padded
# cells of uniform width, decimal points line up vertically -- the
# manual equivalent of gt's `cols_align_decimal()` and tinytable's
# `style_tt(align = "d")` for engines that lack a native decimal-align
# primitive (flextable).
#
# Padding rules (per numeric column, applied across rows):
#   * cells WITH a decimal mark: pad LHS to max_LHS with leading
#     figure-spaces; pad RHS to max_RHS with trailing figure-spaces.
#   * cells WITHOUT a decimal mark (e.g. integer "32"): pad LHS, then
#     insert a regular space at the decimal column position, then pad
#     trailing figure-spaces to mimic the missing RHS digits' width.
#   * en-dash, blank, and other non-numeric tokens: left as-is (they
#     will center-align in the column without a decimal anchor).
#
# `decimal_mark` is read from the structured format spec ("." or ",").
.pad_for_decimal_align <- function(body, struct) {
  decimal_mark <- struct$format_spec$decimal_mark
  fig_space <- "\u2007" # U+2007 figure space (digit-width)
  na_dash <- "\u2013" # U+2013 en dash (Phase 7c14 typography:
  # was em dash before; en dash is the
  # Chicago / NEJM / JAMA tabular "not
  # applicable" glyph).

  for (j in seq_along(body)) {
    if (j == 1L) {
      next
    } # Variable column stays as-is
    col_vals <- body[[j]]
    # Compute per-cell LHS / RHS widths for decimal-bearing cells.
    lhs <- character(length(col_vals))
    rhs <- character(length(col_vals))
    for (i in seq_along(col_vals)) {
      v <- col_vals[i]
      if (is.na(v) || !nzchar(v) || identical(v, na_dash)) {
        lhs[i] <- ""
        rhs[i] <- ""
        next
      }
      pos <- regexpr(decimal_mark, v, fixed = TRUE)
      if (pos < 0L) {
        # No decimal mark -- integer-only or special token.
        lhs[i] <- v
        rhs[i] <- ""
      } else {
        lhs[i] <- substring(v, 1L, pos - 1L)
        rhs[i] <- substring(v, pos + 1L)
      }
    }
    # Max widths only consider cells that actually have either side --
    # blank cells contribute nothing and stay un-padded.
    max_lhs <- max(nchar(lhs))
    max_rhs <- max(nchar(rhs))
    if (max_lhs == 0L && max_rhs == 0L) {
      next
    }

    for (i in seq_along(col_vals)) {
      v <- col_vals[i]
      if (is.na(v) || !nzchar(v)) {
        next
      }
      # Phase 7c24 (item g): en-dash cells (factor reference rows /
      # "not applicable" placeholders) used to skip the padding
      # entirely, so gt centred them in the column instead of
      # aligning to the decimal-mark anchor used by the numeric
      # cells. Treat the en-dash as a 1-glyph "integer" token: pad
      # LHS to (max_lhs - 1) figure-spaces, the dash itself takes
      # the units position, a digit-width space stands in for the
      # decimal mark, and the RHS is padded to max_rhs figure-spaces.
      # The dash now sits visually at the units column, aligned with
      # the leftmost digit of the longest numeric value -- the
      # publication-grade decimal alignment Stata / SAS use for
      # missing cells.
      if (identical(v, na_dash)) {
        pad_lhs <- strrep(fig_space, max(0L, max_lhs - 1L))
        pad_rhs <- strrep(fig_space, max_rhs)
        col_vals[i] <- paste0(pad_lhs, na_dash, fig_space, pad_rhs)
        next
      }
      pos <- regexpr(decimal_mark, v, fixed = TRUE)
      pad_lhs <- strrep(fig_space, max_lhs - nchar(lhs[i]))
      pad_rhs <- strrep(fig_space, max_rhs - nchar(rhs[i]))
      if (pos < 0L) {
        # Integer-only cell: pad LHS, then place a digit-width space
        # at the implicit decimal column, then trailing figure-spaces
        # to fill the (absent) RHS digits.
        col_vals[i] <- paste0(pad_lhs, v, fig_space, pad_rhs)
      } else {
        col_vals[i] <- paste0(pad_lhs, lhs[i], decimal_mark, rhs[i], pad_rhs)
      }
    }
    body[[j]] <- col_vals
  }
  body
}

# Derive a fully-formatted CHAR body (data.frame) from the structured
# typed body. Used by engines (flextable) whose native APIs are too
# coarse for per-cell precision / APA p / below-threshold overrides,
# and by console / clipboard derivers. The Variable column is preserved
# verbatim; each numeric column is replaced by its display strings.
.format_structured_to_string_body <- function(struct) {
  # Identity columns are metadata, not cells: the string body is what a
  # reader sees, so it carries `Variable` plus the value columns only.
  out <- .struct_display_body(struct$body)
  decimal_mark <- struct$format_spec$decimal_mark
  n_rows <- nrow(out)
  outcome_row <- .struct_outcome_row(struct)
  for (j in seq_along(out)) {
    col_name <- names(out)[j]
    if (j == 1L) {
      next
    } # Variable column stays as-is
    meta <- struct$col_meta[[col_name]]
    col_vals <- out[[j]]
    star_col <- struct$stars$markers[[col_name]]
    status_col <- .struct_cell_status(struct, col_name)
    formatted <- character(n_rows)
    for (i in seq_len(n_rows)) {
      formatted[i] <- .cell_to_string(
        col_vals[i],
        i,
        meta,
        status = status_col[i],
        decimal_mark = decimal_mark,
        star = if (is.null(star_col)) "" else star_col[i]
      )
    }
    # Outcome row (multi-DV): the label text lives in metadata (the typed
    # body stays numeric); overlay it on the model's first sub-column.
    if (
      length(outcome_row) == 1L &&
        col_name %in% names(struct$outcome_labels_by_col)
    ) {
      formatted[outcome_row] <- struct$outcome_labels_by_col[[col_name]]
    }
    out[[j]] <- formatted
  }
  out
}


# Resolve the per-cell precision + p_style for column `col_name` at body
# row index `row_idx`, honouring any fit_stat_overrides recorded by
# build_structured_body() (fit-stat rows can carry per-row precision
# that differs from the column default, e.g. integer "n" in the same
# column as 2-dp coefficients).
.resolve_cell_fmt <- function(col_meta_entry, row_idx) {
  prec <- col_meta_entry$precision
  p_style <- col_meta_entry$p_style
  threshold <- col_meta_entry$threshold
  fit_stat <- NULL
  if (!is.null(col_meta_entry$fit_stat_overrides)) {
    for (ov in col_meta_entry$fit_stat_overrides) {
      if (identical(ov$row, row_idx)) {
        prec <- ov$precision %||% prec
        p_style <- ov$p_style %||% p_style
        threshold <- ov$threshold %||% threshold
        fit_stat <- ov$fit_stat %||% NULL
        break
      }
    }
  }
  list(
    precision = prec,
    p_style = p_style,
    threshold = threshold,
    fit_stat = fit_stat,
    signif = col_meta_entry$signif
  )
}

.build_structured_spanners <- function(struct_col_names, expanded, label_map) {
  if (is.null(label_map) || !any(nzchar(label_map))) {
    return(NULL)
  }
  labels <- unique(unname(label_map))
  if (length(labels) <= 1L) {
    return(NULL)
  }
  # nocov start: unreachable defensive twin of the `label_map` guard above --
  # passing `any(nzchar(label_map))` means `labels` (its unique values)
  # retains at least one non-empty string, so this can never be TRUE.
  if (!any(nzchar(labels))) {
    return(NULL)
  }
  # nocov end

  out <- list()
  for (lbl in labels) {
    if (!nzchar(lbl)) {
      next
    }
    # Find struct cols whose source col_spec belongs to a model with this label.
    matching <- integer(0)
    for (i in seq_along(expanded)) {
      m_id <- expanded[[i]]$cs$model_id
      if (identical(label_map[[m_id]], lbl)) {
        matching <- c(matching, i + 1L) # +1 for Variable
      }
    }
    if (length(matching) > 0L) out[[lbl]] <- matching
  }
  # nocov start: every label in `labels` comes from a model that is also
  # in `expanded`, so each label matches at least its own model and `out`
  # can never be empty here for a consistent label_map. Defensive guard.
  if (length(out) == 0L) {
    return(NULL)
  }
  # nocov end
  out
}
