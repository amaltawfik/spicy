# The stacked layout of table_regression(): one panel per model, stacked
# by the primitive of R/table_stack.R.
#
# `table_regression(models, layout = "stacked")` reaches
# `.regression_stacked()` right after the class check of `models`, before
# any argument is resolved per model. The driver checks what concerns the
# LIST (the labels, the per-model `vcov` and `cluster` lists, the hierarchy
# of `nested`), then calls the public `table_regression()` once per model
# with the arguments the caller typed. Each panel is therefore, by
# construction, the table its model gets on its own, single-model defaults
# included (`all_b`, the confidence interval): decisions 1 and 5 of
# dev/stacked_layout_spec.md. Under `nested = TRUE`, panel k carries the
# change statistics of model k against model k - 1, labeled "vs" the panel
# before it (decision 2).
#
# A panel call learns that it is one from `.reg_panel$request`, set right
# before the call and taken by its first statement (`.reg_panel_take()`),
# so the request never reaches another call. The request is a list:
#   nested  NULL, or list(tokens, prev_fit, k): the change tokens of the
#           whole hierarchy, the previous model (NULL for the first one)
#           and the position of the model.
#   index   the position of the model in the list, `n` the length of the
#           list, `name` the name the messages of the side-by-side table
#           give the model ("Model 2", or its name in a named list), `id`
#           its `model_id` there.
# A panel keeps quiet what only the list can decide (the unknown names of
# `labels`, an `exponentiate` that no model applies, a `keep` or `drop`
# pattern that no model matches, the `vcov` and `cluster` of each model)
# and leaves its frames and the terms its patterns met in `.reg_panel`,
# which the driver reads once the panels are built.
#
# Messages. A message about one model names it as the side-by-side table
# does, by its name and position in the list. A message the side-by-side
# table gives once for several models ("Singular fit (Model 1, Model 2)"),
# or once for the arguments, comes with what the driver needs to merge it
# (`.reg_gather()`): the driver gives it once, naming every panel that
# raised it. Any other message of a panel passes through as it is.

.reg_panel <- new.env(parent = emptyenv())
.reg_panel$request <- NULL
.reg_panel$frames <- list()
.reg_panel$terms <- list()

.reg_panel_take <- function() {
  r <- .reg_panel$request
  .reg_panel$request <- NULL
  r
}

# What a condition of a panel carries so that the driver merges it with
# the same condition of the other panels: `key` names the condition;
# `items`, its part for this model, a named list of vectors the driver
# joins panel after panel; `build`, the message from the joined items
# (none: the first message is given as it is, once); `every`, TRUE for a
# condition the side-by-side table gives only when every model would
# (`boot_n` that no bootstrap uses). NULL outside a panel, where the
# condition is given as it is.
.reg_gather <- function(
  panel,
  key,
  items = NULL,
  build = NULL,
  every = FALSE
) {
  if (is.null(panel)) {
    return(NULL)
  }
  list(key = key, items = items, build = build, every = every)
}

# The arguments of table_regression() that the driver handles itself: the
# list, the layout, what labels or titles the stack, and the output.
.REG_STACKED_OWN_ARGS <- c(
  "models",
  "layout",
  "model_labels",
  "outcome_labels",
  "nested",
  "title",
  "note",
  "note_add",
  "output",
  "excel_path",
  "excel_sheet",
  "clipboard_delim",
  "word_path",
  "word_template"
)

# `call` is the match.call() of table_regression(), `env` its frame (the
# errors raised here name table_regression()), `public_call` its
# sys.call(), `cluster_expr` the unevaluated `cluster`.
.regression_stacked <- function(models, call, env, public_call, cluster_expr) {
  arg <- function(name) get(name, envir = env, inherits = FALSE)
  nested <- arg("nested")
  model_labels <- arg("model_labels")
  stack_title <- arg("title")
  stack_note <- arg("note")
  note_add <- arg("note_add")
  output <- arg("output")
  vcov <- arg("vcov")
  cluster <- arg("cluster")

  # ---- The arguments of the list ----------------------------------------
  .check_note_args(stack_note, note_add, call = env)
  validate_caption_arg(stack_title, "title", call = env)
  validate_logical_scalar(nested, "nested", call = env)
  if (!is.null(arg("outcome_labels"))) {
    spicy_abort(
      c(
        "`outcome_labels` does not apply to `layout = \"stacked\"`.",
        "i" = paste0(
          "A stacked table has no Outcome row: a row with the label of ",
          "its model opens each panel. Label the panels with ",
          "`model_labels`."
        )
      ),
      class = "spicy_invalid_input",
      call = env
    )
  }
  validate_model_labels(model_labels, models, call = env)
  validate_resolved_model_labels(
    .resolve_model_labels(models, model_labels),
    models,
    call = env
  )
  .reg_warn_both_labels(model_labels, models)
  validate_vcov_cluster_lists(
    vcov,
    resolve_cluster_arg(cluster, models, call = env),
    models,
    call = env
  )
  validate_nested_alignment(models, nested, call = env)
  .warn_ignored_output_args(output, names(call))
  validate_output_resources(
    output,
    arg("excel_path"),
    arg("word_path"),
    call = env
  )

  n <- length(models)
  nested_on <- isTRUE(nested) && n >= 2L
  tokens <- if (nested_on) default_nested_tokens(models)
  # What the messages of the side-by-side table call each model, and its
  # `model_id` in the machine outputs there.
  msg_names <- .model_display_names(models)
  ids <- .reg_model_ids(models)

  # ---- The arguments of each panel --------------------------------------
  typed <- setdiff(names(as.list(call))[-1L], .REG_STACKED_OWN_ARGS)
  args <- mget(typed, envir = env)
  per_model_cluster <- is.list(cluster) && identical(class(cluster), "list")
  # A cluster vector reaches each panel evaluated, which loses the name
  # the note gives it ("clusters by clinic_id"): the name is read off the
  # expression here and stamped on the vector, as table_regression_uv()
  # does (the stamp wins in table_regression()).
  cluster_names <- if (
    is.call(cluster_expr) &&
      identical(as.character(cluster_expr[[1L]]), "list") &&
      length(cluster_expr) - 1L == n
  ) {
    vapply(as.list(cluster_expr)[-1L], extract_arg_column_name, character(1))
  } else {
    rep(extract_arg_column_name(cluster_expr), n)
  }
  panel_args <- function(k) {
    a <- args
    if (is.list(vcov)) {
      a["vcov"] <- list(vcov[[k]])
    }
    cl <- if (per_model_cluster) cluster[[k]] else cluster
    if (is.atomic(cl) && !is.character(cl) && !is.na(cluster_names[k])) {
      attr(cl, "spicy_cluster_name") <- cluster_names[k]
    }
    if ("cluster" %in% names(a) || per_model_cluster) {
      a["cluster"] <- list(cl)
    }
    a
  }

  # ---- The panels --------------------------------------------------------
  # `do.call()` hands table_regression() the function itself, so an error
  # raised in it carries no name a header can print: it is re-signaled
  # with the call the user typed (the handler of table_regression_uv()).
  # A condition that comes with `spicy_gather` is held, and given once all
  # the panels are built, merged with the same condition of the others. A
  # panel left with no row (`spicy_empty_table`) does not stop the others:
  # the error names every such model, as the side-by-side table does.
  gathered <- list()
  gather <- function(cnd) {
    g <- cnd$spicy_gather
    rec <- gathered[[g$key]]
    if (is.null(rec)) {
      rec <- list(cnd = cnd, items = g$items, count = 0L)
    } else if (!is.null(g$items)) {
      rec$items <- Map(c, rec$items, g$items)
    }
    rec$count <- rec$count + 1L
    gathered[[g$key]] <<- rec
    invisible(NULL)
  }
  on.exit(
    {
      .reg_panel$request <- NULL
      .reg_panel$frames <- list()
      .reg_panel$terms <- list()
    },
    add = TRUE
  )
  .reg_panel$frames <- list()
  .reg_panel$terms <- list()
  panels <- vector("list", n)
  for (k in seq_len(n)) {
    .reg_panel$request <- list(
      nested = if (nested_on) {
        list(
          tokens = tokens,
          prev_fit = if (k > 1L) models[[k - 1L]],
          k = k
        )
      },
      index = k,
      n = n,
      name = msg_names[[k]],
      id = ids[[k]]
    )
    panels[k] <- list(tryCatch(
      withCallingHandlers(
        do.call(table_regression, c(list(models[[k]]), panel_args(k))),
        error = function(e) {
          cl <- conditionCall(e)
          if (
            inherits(e, "spicy_error") || (is.call(cl) && is.function(cl[[1L]]))
          ) {
            e$call <- public_call
            stop(e)
          }
        },
        warning = function(w) {
          if (!is.null(w$spicy_gather)) {
            gather(w)
            invokeRestart("muffleWarning")
          }
        },
        message = function(m) {
          if (!is.null(m$spicy_gather)) {
            gather(m)
            invokeRestart("muffleMessage")
          }
        }
      ),
      spicy_empty_table = gather
    ))
  }
  frames <- .reg_panel$frames

  # ---- What only the list decides ----------------------------------------
  # The order of the terms in the side-by-side table: the order of the
  # machine outputs, and of the terms a dead pattern is told among.
  term_order <- if (length(frames) == n) {
    .reg_stacked_term_order(
      frames,
      ids,
      show_intercept = arg("show_intercept"),
      intercept_position = arg("intercept_position"),
      reference_style = arg("reference_style")
    )
  }
  # A `keep` or `drop` pattern that matches no term of any model, once
  # every panel has met the patterns (one stopped before them stops the
  # table, as the side-by-side table stops before its own check).
  if (length(.reg_panel$terms) == n) {
    seen <- unique(unlist(.reg_panel$terms, use.names = FALSE))
    .reg_warn_dead_patterns(
      c(intersect(term_order, seen), setdiff(seen, term_order)),
      arg("keep"),
      arg("drop")
    )
  }
  .reg_emit_gathered(gathered, n, public_call)
  label_keys <- .reg_label_keys(models)
  .warn_unknown_label_names(
    c(
      label_keys,
      unlist(lapply(frames, function(f) {
        dv <- f$info$dv %||% NA_character_
        c(dv, .reg_bare_var(dv))
      }))
    ),
    arg("labels"),
    arg("note_labels"),
    hint = .reg_terms_sentence(label_keys)
  )
  .reg_warn_exponentiate_noop(frames, arg("exponentiate"))

  panel_labels <- .reg_stacked_panel_labels(models, model_labels, frames)
  if (nested_on) {
    for (k in seq_len(n)[-1L]) {
      panels[[k]] <- .stack_label_change_rows(panels[[k]], panel_labels[k - 1L])
    }
  }

  title_txt <- if (isFALSE(stack_title)) {
    NULL
  } else if (is.character(stack_title)) {
    stack_title
  } else {
    build_regression_title_from_frames(
      frames,
      nested = nested_on,
      decimal_mark = attr(panels[[1L]], "decimal_mark", exact = TRUE),
      stacked = TRUE
    )
  }
  # The note sentence of a hierarchy refitted by maximum likelihood is a
  # statement about the pairs, which no panel sees alone.
  extra <- list(
    inference = build_nested_ml_refit_footer_block_from_frames(
      frames,
      nested_on,
      c(arg("show_fit_stats"), tokens)
    )
  )

  .table_stack_impl(
    panels,
    panel_labels,
    title = title_txt,
    note = stack_note,
    note_add = note_add,
    note_extra = extra,
    output = output,
    excel_path = arg("excel_path"),
    excel_sheet = arg("excel_sheet"),
    clipboard_delim = arg("clipboard_delim"),
    word_path = arg("word_path"),
    word_template = arg("word_template"),
    machine = list(ids = ids, term_order = term_order)
  )
}

# The conditions the panels raised for the driver to merge, given once:
# a warning or a message with the names of every panel that raised it (a
# condition of `every` only when every panel did), then an error if a
# panel was left with no row, with the names of every such panel.
.reg_emit_gathered <- function(gathered, n, call) {
  failed <- NULL
  for (rec in gathered) {
    cnd <- rec$cnd
    g <- cnd$spicy_gather
    if (isTRUE(g$every) && rec$count < n) {
      next
    }
    # The leaf classes of the condition, which the spicy constructors
    # complete with their own.
    leaf <- setdiff(
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
    )
    if (inherits(cnd, "error")) {
      failed <- failed %||% list(message = g$build(rec$items), class = leaf)
    } else if (is.null(g$build)) {
      cnd$spicy_gather <- NULL
      (if (inherits(cnd, "warning")) warning else message)(cnd)
    } else {
      (if (inherits(cnd, "warning")) spicy_warn else spicy_inform)(
        g$build(rec$items),
        class = leaf
      )
    }
  }
  if (!is.null(failed)) {
    spicy_abort(failed$message, class = failed$class, call = call)
  }
  invisible(NULL)
}

# The order of the terms of the models side by side, the order their
# machine outputs follow (`align_frames()`). Their fit statistics play no
# part in it, and the panels of a nested stack carry the change
# statistics of their own pair only, which the side-by-side table would
# not bind: the frames are aligned without them.
.reg_stacked_term_order <- function(
  frames,
  ids,
  show_intercept,
  intercept_position,
  reference_style
) {
  bare <- lapply(frames, function(f) {
    f$info$fit_stats <- NULL
    f
  })
  align_frames(
    bare,
    model_ids = ids,
    show_intercept = show_intercept,
    intercept_position = intercept_position,
    reference_style = reference_style
  )$term_order
}

# Panel k of a nested stack as table_regression(model k) builds it, plus
# the change statistics of model k against model k - 1: the hierarchy
# attach of the side-by-side table run on that one pair. `k_offset` keeps
# the model numbers of its messages those of the whole hierarchy.
.reg_attach_panel_change <- function(
  frames,
  models,
  vcov_list,
  cluster_list,
  req
) {
  out <- attach_nested_stats_to_frames(
    c(list(list(info = list(fit_stats = NULL))), frames),
    c(list(req$prev_fit), models),
    vcov_list = c(list(NULL), vcov_list),
    cluster_list = c(list(NULL), cluster_list),
    k_offset = req$k - 2L
  )
  out[-1L]
}

# The change rows of a panel name the panel they compare with ("R2
# change vs Model 1", in the words of the table), in the display body and
# in the typed view alike: one row each, at the same index.
.stack_label_change_rows <- function(panel, vs_label) {
  st <- attr(panel, "structured")
  rows <- which(
    st$body[[".row_role"]] %in%
      "fit_stat" &
      st$body[[".variable"]] %in%
        c(.NESTED_CHANGE_TOKENS, .NESTED_WALD_CHANGE_TOKENS)
  )
  if (!length(rows)) {
    return(panel)
  }
  st$body[[.REG_KEY_VARIABLE]][rows] <- spicy_fmt(
    "fitstat_change_vs",
    st$body[[.REG_KEY_VARIABLE]][rows],
    vs_label
  )
  attr(panel, "structured") <- st
  lab <- panel[[.REG_KEY_VARIABLE]]
  lab[rows] <- spicy_fmt("fitstat_change_vs", lab[rows], vs_label)
  panel[[.REG_KEY_VARIABLE]] <- lab
  panel
}

# The label of each panel of a stacked table, by the rule of the spanners
# of the side-by-side table: `model_labels`, else the names of `models`
# (an unnamed slot filled with its default), else the outcomes when they
# all differ (their display, or their names when two displays coincide),
# else "Model k".
.reg_stacked_panel_labels <- function(models, model_labels, frames) {
  lab <- .resolve_model_labels(models, model_labels)
  if (!is.null(lab)) {
    return(lab)
  }
  n <- length(models)
  dv <- vapply(frames, function(f) f$info$dv %||% NA_character_, character(1))
  shown <- vapply(
    frames,
    function(f) f$info$dv_display %||% f$info$dv %||% NA_character_,
    character(1)
  )
  .stack_distinct_outcomes(dv, shown) %||%
    spicy_fmt("label_model_name", seq_len(n))
}

# The labels distinct outcomes give the panels of a stack: their displays,
# or their names when two displays coincide; NULL when the panels are
# fewer than two, or when an outcome is missing or shared.
.stack_distinct_outcomes <- function(dv, shown) {
  if (
    length(dv) < 2L ||
      anyNA(dv) ||
      !all(nzchar(dv)) ||
      anyDuplicated(dv)
  ) {
    return(NULL)
  }
  if (anyNA(shown) || anyDuplicated(shown)) dv else shown
}
