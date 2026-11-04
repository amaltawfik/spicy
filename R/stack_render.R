# The stacked table that table_stack() renders: the panels merged into one
# display body (the console) and one typed view (every other engine), row
# for row, with the indices of the rules and of the header rows the writers
# draw.
#
# Columns are matched by POSITION. The keys are those of the first panel;
# a position beyond its width takes the key of the first panel that has
# it. A panel narrower than another leaves its extra cells empty.
#
# A cell keeps exactly the format it had in its panel: the column metadata
# of the merged view are those of the panel that first holds the column,
# and `row_fmt` carries the format of every cell of every panel
# (`.resolve_cell_fmt()` of its panel), read first by `.resolve_cell_fmt()`.
# The display overrides (`display_cells`), the cell statuses and the star
# markers are each panel's, placed at its rows.
#
# Rows, panel after panel: the label row of the panel (role
# "factor_header", "Women:"), its header rows when the panels do not share
# one header (role "header": the spanner row of its column groups if it
# has any, the row of its column labels, then the row of the bounds of its
# interval if it has one, blank in the console), then its own rows, with
# their identity columns. The merged view is an internal object, never
# validated as a table's own view: a merged column can hold a p-value in
# one panel and an estimate in another.

.stack_build <- function(tables, panel_labels, title, note) {
  n <- length(tables)
  views <- lapply(tables, attr, which = "structured", exact = TRUE)
  vcols <- lapply(views, function(v) .struct_value_cols(v$body))
  dcols <- lapply(tables, function(t) names(t)[-1L])
  shared <- all(vapply(
    views,
    function(v) identical(.stack_header_sig(v), .stack_header_sig(views[[1L]])),
    logical(1)
  ))
  same_structure <- .stack_same_structure(views, tables)

  # ---- Keys, by position -------------------------------------------------
  key_at <- function(cols, j) cols[[which(lengths(cols) >= j)[[1L]]]][[j]]
  owner_of <- function(cols, j) which(lengths(cols) >= j)
  keys <- make.unique(c(
    .REG_KEY_VARIABLE,
    vapply(seq_len(max(lengths(vcols))), key_at, character(1), cols = vcols)
  ))[-1L]
  dkeys <- make.unique(c(
    .REG_KEY_VARIABLE,
    vapply(seq_len(max(lengths(dcols))), key_at, character(1), cols = dcols)
  ))[-1L]

  # ---- Rows --------------------------------------------------------------
  n_own <- vapply(views, function(v) nrow(v$body), integer(1))
  # The header rows of a panel, as the header of its table: the row of its
  # column groups, the row of its labels, and the row of the bounds of
  # its interval ("LL", "UL").
  n_hdr <- vapply(
    seq_len(n),
    function(k) {
      if (shared || k == 1L) {
        0L
      } else {
        1L +
          as.integer(length(views[[k]]$spanners) > 0L) +
          as.integer(length(views[[k]]$ci_pairs) > 0L)
      }
    },
    integer(1)
  )
  size <- 1L + n_hdr + n_own
  start <- cumsum(size) - size + 1L
  n_rows <- sum(size)
  section_rows <- as.integer(start)
  panel_rows <- lapply(seq_len(n), function(k) {
    as.integer(start[[k]] + n_hdr[[k]] + seq_len(n_own[[k]]))
  })
  offset <- vapply(panel_rows, function(r) r[[1L]] - 1L, integer(1))
  header_rows <- as.integer(unlist(lapply(seq_len(n), function(k) {
    start[[k]] + seq_len(n_hdr[[k]])
  })))
  # The label row of a panel, in the typography of a block header
  # ("Women:"); a label that already closes on a colon takes no second.
  section_text <- ifelse(
    endsWith(trimws(panel_labels), ":"),
    panel_labels,
    spicy_fmt("label_block_header", panel_labels)
  )

  # ---- Typed body --------------------------------------------------------
  tb <- list(Variable = rep("", n_rows))
  for (key in keys) {
    tb[[key]] <- rep(NA_real_, n_rows)
  }
  meta <- list(
    .variable = rep(NA_character_, n_rows),
    .level = rep(NA_character_, n_rows),
    .row_role = rep("header", n_rows),
    .indent = integer(n_rows)
  )
  tb$Variable[section_rows] <- section_text
  meta$.row_role[section_rows] <- "factor_header"
  for (k in seq_len(n)) {
    rows <- panel_rows[[k]]
    b <- views[[k]]$body
    tb$Variable[rows] <- b[[.REG_KEY_VARIABLE]]
    for (j in seq_along(vcols[[k]])) {
      tb[[keys[[j]]]][rows] <- as.numeric(b[[vcols[[k]][[j]]]])
    }
    for (m in names(meta)) {
      meta[[m]][rows] <- b[[m]]
    }
  }
  body_df <- as.data.frame(
    c(tb, meta),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  body_df[[".indent"]] <- as.integer(body_df[[".indent"]])

  # ---- Header rows -------------------------------------------------------
  # Typed side (`header_cells`: column key -> text by row) and display side
  # (`dheader`), with the spans the writers merge (`header_spans`) and the
  # rules they draw under a column group or an interval label
  # (`header_rules`), in indices of the typed display body (label column
  # = 1). The console draws the column groups over their display columns
  # (`body_spanners`).
  header_cells <- list()
  set_header <- function(key, row, text) {
    v <- header_cells[[key]] %||% rep(NA_character_, n_rows)
    v[[row]] <- text
    header_cells[[key]] <<- v
  }
  dheader <- list()
  header_spans <- list()
  header_rules <- list()
  body_spanners <- list()
  for (k in which(n_hdr > 0L)) {
    v <- views[[k]]
    row <- start[[k]]
    if (length(v$spanners)) {
      row <- row + 1L
      for (s in seq_along(v$spanners)) {
        idx <- as.integer(v$spanners[[s]])
        for (j in idx) {
          set_header(
            keys[[j - 1L]],
            row,
            if (j == idx[[1L]]) names(v$spanners)[[s]] else ""
          )
        }
        if (length(idx) >= 2L) {
          header_spans[[length(header_spans) + 1L]] <- list(
            row = row,
            cols = idx
          )
        }
        header_rules[[length(header_rules) + 1L]] <- list(
          row = row,
          cols = idx,
          kind = "spanner"
        )
      }
      dsp <- attr(tables[[k]], "spanners", exact = TRUE)
      for (s in seq_along(dsp)) {
        dheader[[length(dheader) + 1L]] <- list(
          row = row,
          col = as.integer(dsp[[s]])[[1L]],
          text = names(dsp)[[s]]
        )
      }
      body_spanners[[length(body_spanners) + 1L]] <- list(
        row = row,
        spans = lapply(dsp, as.integer)
      )
    }
    row <- row + 1L
    ci_first <- vapply(
      v$ci_pairs,
      function(p) as.integer(p$cols)[[1L]],
      integer(1)
    )
    ci_second <- vapply(
      v$ci_pairs,
      function(p) as.integer(p$cols)[[2L]],
      integer(1)
    )
    for (j in seq_along(vcols[[k]])) {
      pos <- j + 1L
      text <- if (pos %in% ci_second) {
        ""
      } else if (pos %in% ci_first) {
        v$ci_pairs[[match(pos, ci_first)]]$label
      } else {
        v$col_meta[[vcols[[k]][[j]]]]$display_label %||% vcols[[k]][[j]]
      }
      set_header(keys[[j]], row, text)
    }
    for (p in v$ci_pairs) {
      header_spans[[length(header_spans) + 1L]] <- list(
        row = row,
        cols = as.integer(p$cols)
      )
      header_rules[[length(header_rules) + 1L]] <- list(
        row = row,
        cols = as.integer(p$cols),
        kind = "ci"
      )
    }
    for (j in seq_along(dcols[[k]])) {
      dheader[[length(dheader) + 1L]] <- list(
        row = row,
        col = j + 1L,
        text = .lookup_display_label(dcols[[k]][[j]], v$col_meta)
      )
    }
    # The bounds under the interval label. The console prints an interval
    # in one column: this row stays blank in its body, and its printer
    # leaves it out.
    if (length(v$ci_pairs)) {
      row <- row + 1L
      for (j in seq_along(vcols[[k]])) {
        pos <- j + 1L
        set_header(
          keys[[j]],
          row,
          if (pos %in% ci_first) {
            spicy_str("header_ci_ll")
          } else if (pos %in% ci_second) {
            spicy_str("header_ci_ul")
          } else {
            ""
          }
        )
      }
    }
  }

  # ---- Column metadata ---------------------------------------------------
  col_meta <- list()
  cell_status <- list()
  for (j in seq_along(keys)) {
    owners <- owner_of(vcols, j)
    m <- views[[owners[[1L]]]]$col_meta[[vcols[[owners[[1L]]]][[j]]]]
    m$fit_stat_overrides <- NULL
    m$display_cells <- NULL
    # A column the first panel does not have is headed by nothing at the
    # top (gt, which would print the key of an unlabeled column, gives it
    # a no-break space).
    if (length(vcols[[1L]]) < j) {
      m$display_label <- ""
    }
    row_fmt <- vector("list", n_rows)
    disp <- header_cells[[keys[[j]]]] %||% rep(NA_character_, n_rows)
    status <- rep("", n_rows)
    for (k in owners) {
      pm <- views[[k]]$col_meta[[vcols[[k]][[j]]]]
      rows <- panel_rows[[k]]
      for (i in seq_along(rows)) {
        row_fmt[[rows[[i]]]] <- .resolve_cell_fmt(pm, i)
      }
      if (!is.null(pm$display_cells)) {
        disp[rows] <- pm$display_cells
      }
      # The Outcome row of a side-by-side panel: its labels are a metadata
      # overlay in the panel's view, cells of their own here.
      ol <- views[[k]]$outcome_labels_by_col
      orow <- which(views[[k]]$body[[".row_role"]] == "outcome")
      if (vcols[[k]][[j]] %in% names(ol) && length(orow) == 1L) {
        disp[rows[[orow]]] <- ol[[vcols[[k]][[j]]]]
      }
      st <- views[[k]]$cell_status[[vcols[[k]][[j]]]]
      if (!is.null(st)) {
        status[rows] <- st
      }
    }
    m$row_fmt <- row_fmt
    if (any(!is.na(disp))) {
      m$display_cells <- disp
    }
    col_meta[[keys[[j]]]] <- m
    if (any(nzchar(status))) {
      cell_status[[keys[[j]]]] <- status
    }
  }

  # ---- Stars -------------------------------------------------------------
  starred <- which(!vapply(views, function(v) is.null(v$stars), logical(1)))
  stars <- NULL
  if (length(starred)) {
    markers <- list()
    for (j in seq_along(keys)) {
      mk <- rep("", n_rows)
      hit <- FALSE
      for (k in intersect(owner_of(vcols, j), starred)) {
        s <- views[[k]]$stars$markers[[vcols[[k]][[j]]]]
        if (!is.null(s)) {
          mk[panel_rows[[k]]] <- s
          hit <- TRUE
        }
      }
      if (hit) {
        markers[[keys[[j]]]] <- mk
      }
    }
    stars <- list(
      thresholds = views[[starred[[1L]]]]$stars$thresholds,
      markers = markers
    )
  }

  struct <- list(
    version = .spicy_structured_version(),
    body = body_df,
    stars = stars,
    cell_status = cell_status,
    outcome_labels_by_col = character(0),
    col_meta = col_meta,
    spanners = views[[1L]]$spanners,
    ci_pairs = views[[1L]]$ci_pairs,
    format_spec = views[[1L]]$format_spec
  )

  # ---- Display body (console) --------------------------------------------
  db <- list(Variable = rep("", n_rows))
  for (dk in dkeys) {
    db[[dk]] <- rep("", n_rows)
  }
  db$Variable[section_rows] <- section_text
  for (k in seq_len(n)) {
    rows <- panel_rows[[k]]
    db$Variable[rows] <- as.character(tables[[k]][[1L]])
    for (j in seq_along(dcols[[k]])) {
      v <- as.character(tables[[k]][[j + 1L]])
      v[is.na(v)] <- ""
      db[[dkeys[[j]]]][rows] <- v
    }
  }
  for (h in dheader) {
    db[[h$col]][[h$row]] <- h$text
  }
  dbody <- as.data.frame(db, stringsAsFactors = FALSE, check.names = FALSE)
  align <- attr(tables[[1L]], "align", exact = TRUE) %||% "decimal"
  decimal_mark <- attr(tables[[1L]], "decimal_mark", exact = TRUE) %||% "."
  # Panels with the same columns are decimal-aligned as one column, so the
  # decimal marks (and the brackets of an interval) line up from the first
  # panel to the last: only the padding of a cell changes. Panels with
  # columns of their own keep the alignment of their table.
  if (same_structure && identical(align, "decimal")) {
    rows <- setdiff(seq_len(n_rows), header_rows)
    for (dk in dkeys) {
      dbody[[dk]][rows] <- trimws(dbody[[dk]][rows])
    }
    dbody <- .reg_align_char_body(
      dbody,
      .reg_ci_col_names(attr(tables[[1L]], "col_spec", exact = TRUE)),
      decimal_mark,
      rows = rows
    )
  }
  # The two bodies of a table hold the same rows, one for one; so do the
  # two bodies of the stack.
  stopifnot(identical(vapply(tables, nrow, integer(1)), n_own))

  # ---- Rules, merged cells -----------------------------------------------
  # A rule above: every rule of every panel, the first row of a panel
  # after its header rows (`group_sep_rows`), and the label row of every
  # panel but the first, which opens a section as a block header does
  # (`section_sep_rows`). The writers draw the union of the two.
  shifted <- function(name) {
    unlist(lapply(seq_len(n), function(k) {
      offset[[k]] +
        as.integer(attr(tables[[k]], name, exact = TRUE) %||% integer(0))
    }))
  }
  after_header <- vapply(
    which(n_hdr > 0L),
    function(k) panel_rows[[k]][[1L]],
    integer(1)
  )
  group_sep_rows <- sort(unique(as.integer(c(
    shifted("group_sep_rows"),
    after_header
  ))))
  section_sep_rows <- sort(unique(as.integer(c(
    shifted("section_sep_rows"),
    section_rows[-1L]
  ))))
  # `fit_stats_layout = "merged"` panels: their fit-statistic cells merged
  # across the columns of their model, in their rows only.
  fsl <- vapply(
    tables,
    function(t) attr(t, "fit_stats_layout", exact = TRUE) %||% "first_col",
    character(1)
  )
  fit_merge_specs <- list()
  for (k in which(fsl == "merged")) {
    specs <- .fit_stat_merge_ranges(
      .struct_display_body(views[[k]]$body),
      views[[k]]$spanners,
      as.integer(
        attr(tables[[k]], "group_sep_rows", exact = TRUE) %||% integer(0)
      )
    )
    for (sp in specs) {
      fit_merge_specs[[length(fit_merge_specs) + 1L]] <- list(
        row = sp$row + offset[[k]],
        cols = sp$cols
      )
    }
  }

  dl1 <- vapply(
    dcols[[1L]],
    .lookup_display_label,
    character(1),
    col_meta = views[[1L]]$col_meta
  )
  out <- dbody
  attr(out, "title") <- title
  attr(out, "note") <- note
  attr(out, "panels") <- stats::setNames(tables, panel_labels)
  attr(out, "panel_rows") <- stats::setNames(panel_rows, panel_labels)
  attr(out, "structured") <- struct
  attr(out, "section_rows") <- section_rows
  attr(out, "header_rows") <- header_rows
  attr(out, "header_spans") <- header_spans
  attr(out, "header_rules") <- header_rules
  attr(out, "body_spanners") <- body_spanners
  attr(out, "group_sep_rows") <- group_sep_rows
  attr(out, "section_sep_rows") <- section_sep_rows
  attr(out, "fit_merge_specs") <- fit_merge_specs
  attr(out, "display_labels") <- c(
    spicy_str("header_variable"),
    unname(dl1),
    rep("", length(dkeys) - length(dl1))
  )
  attr(out, "spanners") <- attr(tables[[1L]], "spanners", exact = TRUE)
  attr(out, "align") <- align
  attr(out, "decimal_mark") <- decimal_mark
  attr(out, "padding") <- max(vapply(
    tables,
    function(t) as.integer(attr(t, "padding", exact = TRUE) %||% 0L),
    integer(1)
  ))
  attr(out, "fit_stats_layout") <- if (any(fsl == "merged")) {
    "merged"
  } else {
    "first_col"
  }
  class(out) <- c("spicy_stack_table", "spicy_table", "data.frame")
  out
}

# What the header of a panel shows: the label of each value column, the
# interval pairs and the column groups. Panels with the same signature
# share one header.
.stack_header_sig <- function(view) {
  vc <- .struct_value_cols(view$body)
  list(
    labels = unname(vapply(
      vc,
      function(k) view$col_meta[[k]]$display_label %||% k,
      character(1)
    )),
    ci = lapply(view$ci_pairs, function(p) list(p$label, as.integer(p$cols))),
    spanners = if (length(view$spanners)) {
      list(names(view$spanners), unname(lapply(view$spanners, as.integer)))
    }
  )
}

# TRUE when every panel lays its columns out alike, whatever their labels:
# as many value columns, the intervals at the same places, in the typed
# view and in the display body (whose interval is one column), with the
# same decimals. Panels whose decimals differ keep their own padding:
# aligned together, the bounds of the narrower interval would be padded
# inside its brackets ("[23.18 , 24.71 ]").
.stack_same_structure <- function(views, tables) {
  layout <- function(k) {
    cs <- attr(tables[[k]], "col_spec", exact = TRUE)
    vc <- .struct_value_cols(views[[k]]$body)
    list(
      n_typed = length(vc),
      precision = unname(lapply(vc, function(c) {
        views[[k]]$col_meta[[c]]$precision
      })),
      ci = lapply(views[[k]]$ci_pairs, function(p) as.integer(p$cols)),
      n_disp = ncol(tables[[k]]) - 1L,
      ci_disp = which(vapply(
        cs,
        function(x) identical(x$fields, c("ci_low", "ci_high")),
        logical(1)
      ))
    )
  }
  ref <- layout(1L)
  all(vapply(
    seq_along(views),
    function(k) identical(layout(k), ref),
    logical(1)
  ))
}
