# The checks and the fitting shared by the functions that fit their own
# models and hand them to `table_regression()` as one bundle:
# table_regression_uv() today. Each helper keeps the message, the class
# and the `call` of the error the user sees; a helper that names the
# calling function takes it as an argument.

# The names `...` passes to `table_regression()`: every one an argument
# of it, typed in full. `models`, `layout` and the arguments the calling
# function has itself (`own_args`, the names of its formals) are not,
# and an unnamed argument has nowhere to go.
.screen_check_dots <- function(
  dot_names,
  n_dots,
  own_args,
  call = rlang::caller_env()
) {
  if (n_dots == 0L) {
    return(invisible(NULL))
  }
  if (is.null(dot_names) || any(!nzchar(dot_names))) {
    spicy_abort(
      c(
        "Every argument passed through `...` must be named.",
        "i" = "They go to `table_regression()`, by name."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  # `layout` lays out several models; the screen is one table.
  if ("layout" %in% dot_names) {
    spicy_abort(
      c(
        "`layout` does not apply to a univariable screen: the screen is one table.",
        "i" = "Stack several screens with `table_stack()`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  known <- setdiff(
    names(formals(table_regression)),
    c("models", "layout", own_args)
  )
  bad <- setdiff(dot_names, known)
  if (length(bad) == 0L) {
    return(invisible(NULL))
  }
  hints <- character(0)
  for (b in bad) {
    cand <- known[startsWith(known, b)]
    if (length(cand) == 1L) {
      hints <- c(
        hints,
        "i" = sprintf(
          "Did you mean `%s`? Arguments are matched by their full name.",
          cand
        )
      )
    }
  }
  spicy_abort(
    c(
      sprintf(
        "Unknown argument%s passed to `table_regression()` through `...`: %s.",
        if (length(bad) > 1L) "s" else "",
        paste0("`", bad, "`", collapse = ", ")
      ),
      hints
    ),
    class = "spicy_invalid_input",
    call = call
  )
}


# Normalize `family` the way stats::glm() itself does -- the name
# ("binomial") or the bare constructor (binomial) becomes the family
# object -- so every check after it can read `family$family` safely
# instead of erroring on `$` for atomic vectors / closures. `env` is the
# frame a name is looked up from: the caller of the user-facing function.
.screen_normalize_family <- function(
  family,
  env,
  call = rlang::caller_env()
) {
  if (is.character(family) && length(family) == 1L && !is.na(family)) {
    family <- tryCatch(
      get(family, mode = "function", envir = env),
      error = function(e) family
    )
  }
  if (is.function(family)) {
    family <- tryCatch(family(), error = function(e) family)
  }
  if (!inherits(family, "family")) {
    spicy_abort(
      c(
        paste0(
          "`family` must be a stats::family object, its name, or ",
          "its constructor function."
        ),
        "i" = paste0(
          "E.g. `family = binomial()`, `family = \"binomial\"`, ",
          "or `family = binomial`."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  family
}


# A gaussian/identity glm is lm by another name; the right spelling
# exists as an argument, so point straight at it (the generic "refit
# with lm()" caveat would be off-target here).
.screen_refuse_gaussian_glm <- function(
  method,
  family,
  call = rlang::caller_env()
) {
  if (
    identical(method, "glm") &&
      identical(family$family, "gaussian") &&
      identical(family$link, "identity")
  ) {
    spicy_abort(
      c(
        paste0(
          "`family = gaussian()` with the identity link is ",
          "`lm` by another name."
        ),
        "i" = "Use `method = \"lm\"` for the linear screen."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}


# Cluster contract of a bundle: ONE value per row of `data`. The
# single-model contract (length = the fit's estimation sample) cannot be
# satisfied by one vector when the Ns of the fits differ, so the vector
# is aligned per fit (.screen_cluster_for_fit()).
.screen_check_cluster <- function(
  cluster,
  n_rows,
  call = rlang::caller_env()
) {
  if (is.null(cluster)) {
    return(invisible(NULL))
  }
  if (!is.atomic(cluster)) {
    spicy_abort(
      c(
        "`cluster` must be a single vector for a univariable screen.",
        "i" = paste0(
          "Supply one value per row of `data`; per-model ",
          "cluster lists are not meaningful here."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (length(cluster) != n_rows) {
    spicy_abort(
      sprintf(
        "`cluster` must have one value per row of `data` (%d), not %d.",
        n_rows,
        length(cluster)
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}


# The rows of `data` complete on `vars`, and the cluster vector cut to
# them. `[.data.frame` drops the `label` attribute of plain vectors and
# factors (a tibble keeps it): the subset must not take the variables'
# labels away from `labels = TRUE`.
.screen_complete_cases <- function(data, vars, cluster = NULL) {
  cc <- stats::complete.cases(data[, vars])
  kept <- data[cc, , drop = FALSE]
  for (nm in vars) {
    lab <- attr(data[[nm]], "label", exact = TRUE)
    if (!is.null(lab)) {
      attr(kept[[nm]], "label") <- lab
    }
  }
  if (!is.null(cluster)) {
    cluster <- cluster[cc]
  }
  list(data = kept, cluster = cluster)
}


# Linear model on a categorical outcome: lm() cannot model a factor
# response. A two-level outcome is coded 0/1 on its second level -- the
# glm convention -- so the linear model is a linear probability model for
# P(outcome = second level). Under the DEFAULTED method the choice is
# disclosed in a classed warning (LPM coefficients are probability
# differences, comparable across models; Mood 2010); explicit
# `method = "lm"` is an informed choice and stays silent. More than two
# observed levels are refused. Returns `data`, the outcome recoded.
.screen_lpm_recode <- function(
  data,
  outcome_name,
  method_supplied,
  call = rlang::caller_env()
) {
  y <- data[[outcome_name]]
  lpm_level <- NULL
  if (is.factor(y) || is.character(y) || is.logical(y)) {
    y_fac <- if (is.logical(y)) {
      factor(y, levels = c(FALSE, TRUE))
    } else {
      droplevels(as.factor(y))
    }
    if (nlevels(y_fac) == 2L) {
      lpm_level <- levels(y_fac)[2L]
      data[[outcome_name]] <- as.integer(y_fac) - 1L
    } else {
      spicy_abort(
        c(
          sprintf(
            "The linear screen needs a numeric or two-level outcome; `%s` has %d observed levels.",
            outcome_name,
            nlevels(y_fac)
          ),
          "i" = paste0(
            "Multinomial outcomes have no univariable screen: fit ",
            "`nnet::multinom()` models and pass them to ",
            "`table_regression()`."
          )
        ),
        class = "spicy_invalid_data",
        call = call
      )
    }
  } else if (all(y[!is.na(y)] %in% c(0, 1)) && any(!is.na(y))) {
    lpm_level <- "1"
  }
  if (!is.null(lpm_level) && !method_supplied) {
    spicy_warn(
      c(
        sprintf(
          "`%s` is binary: the default linear screen fits a linear probability model for P(%s = %s).",
          outcome_name,
          outcome_name,
          lpm_level
        ),
        "i" = paste0(
          "Its coefficients are probability differences ",
          "(comparable across models; Mood 2010). Consider ",
          "`vcov = \"HC3\"` for the model's built-in ",
          "heteroskedasticity."
        ),
        "i" = paste0(
          "For the logistic screen, use `method = \"glm\"` ",
          "(odds ratios with `exponentiate = TRUE`)."
        )
      ),
      class = "spicy_model_choice"
    )
  }
  data
}


# One model of a bundle: `response ~ rhs_names`, the right-hand side
# backticked, the response as given (a backticked column name, or the
# verbatim `Surv()` expression of a Cox model). The formula's
# environment is this frame, which holds the `data` and `family` the
# call of the fit names.
.screen_fit <- function(rhs_names, response, method, data, family) {
  f <- stats::reformulate(paste0("`", rhs_names, "`"), response = response)
  environment(f) <- environment()
  switch(
    method,
    lm = stats::lm(f, data = data),
    coxph = survival::coxph(f, data = data),
    stats::glm(f, data = data, family = family)
  )
}

# .screen_fit(), its failure refused with `failed` as the headline and
# the error of the fitting function below it.
.screen_fit_or_abort <- function(
  rhs_names,
  response,
  method,
  data,
  family,
  failed,
  call = rlang::caller_env()
) {
  tryCatch(
    .screen_fit(rhs_names, response, method, data, family),
    error = function(e) {
      spicy_abort(
        c(failed, "x" = conditionMessage(e)),
        class = "spicy_invalid_data",
        call = call
      )
    }
  )
}


# Stamp the column name the user typed for `cluster` on the vector (or on
# each vector of a list): subsetting strips attributes, and the footer
# says "clusters by region" instead of "cluster vector supplied".
.screen_stamp_cluster <- function(cluster, cluster_name) {
  if (is.null(cluster) || is.na(cluster_name)) {
    return(cluster)
  }
  if (is.list(cluster)) {
    lapply(cluster, function(x) {
      attr(x, "spicy_cluster_name") <- cluster_name
      x
    })
  } else {
    attr(cluster, "spicy_cluster_name") <- cluster_name
    cluster
  }
}


# `do.call()` hands `table_regression()` the function itself, so an
# error raised in it carries no name a header can print ("Error:"), and
# a base error the deparse of its whole body. Both are re-signaled with
# `call`, the call of the function the user called. Whether the caller
# typed the title is stamped on the table, for `table_stack()`, which
# labels a panel by its title only then. The path of a file, the table
# sent to the clipboard come back invisibly, as from table_regression().
.screen_call_table_regression <- function(models, args, call, title_typed) {
  res <- withVisible(withCallingHandlers(
    do.call(table_regression, c(list(models), args)),
    error = function(e) {
      cl <- conditionCall(e)
      if (
        inherits(e, "spicy_error") || (is.call(cl) && is.function(cl[[1L]]))
      ) {
        e$call <- call
        stop(e)
      }
    }
  ))
  out <- res$value
  if (inherits(out, "spicy_regression_table")) {
    attr(out, "spicy_title_typed") <- title_typed
  }
  if (res$visible) out else invisible(out)
}


# The bundle-level cluster vector has one value per row of the data;
# each fit wants one per row of its OWN estimation sample.
.screen_cluster_for_fit <- function(cluster, fit) {
  if (!is.null(cluster) && is.atomic(cluster)) {
    om <- stats::na.action(fit)
    if (!is.null(om) && length(cluster) != .uv_fit_n(fit)) {
      cluster <- cluster[-om]
    }
  }
  cluster
}


# The frame of one fit of a bundle. The N column is the bundle's own
# (one N per block), and a bundle neither standardizes nor computes the
# Satterthwaite AME of its fits.
.screen_block_frame <- function(
  fit,
  model_id,
  vcov,
  cluster,
  boot_n,
  ci_level,
  ci_method,
  exponentiate,
  show_columns,
  show_fit_stats,
  cluster_name,
  tau,
  at_time
) {
  as_regression_frame_validated(
    fit,
    model_id = model_id,
    vcov = vcov,
    cluster = cluster,
    boot_n = boot_n,
    ci_level = ci_level,
    ci_method = ci_method,
    standardized = "none",
    exponentiate = exponentiate,
    show_columns = setdiff(show_columns, "n"),
    show_fit_stats = show_fit_stats,
    use_ame_satterthwaite = FALSE,
    cluster_name = cluster_name,
    tau = tau,
    at_time = at_time
  )
}


# The `info` of the composite frame of a bundle, pooled from the frames
# of its fits (`frames`, one per fit, in the order of `fits`). It starts
# from the first fit's, and carries `frame_class` as its own class.
.screen_pool_info <- function(frames, fits, ns, frame_class) {
  base_info <- frames[[1L]]$info
  # Per-fit flags must be pooled, not read off the first fit: any
  # rank-deficient model keeps its footer disclosure.
  singular <- vapply(
    frames,
    function(fr) isTRUE(fr$info$extras$has_singular),
    logical(1)
  )
  singular_terms <- unlist(lapply(frames[singular], function(fr) {
    fr$info$extras$singular_terms
  }))
  # Partial effect sizes are computed PER FIT and their rows survive into
  # the bundle, so the pooled frame declares the capability the wrapped
  # fits actually have -- an lm / glm / mixed screen fills the column, a
  # coxph or multinom screen leaves it empty and says so.
  any_partial_es <- any(vapply(
    frames,
    function(fr) isTRUE(fr$info$supports$partial_effect_size),
    logical(1)
  ))

  info <- base_info
  info$class <- frame_class
  # The classes the bundle stands for. `info$class` is the composite's
  # own name, which says nothing to a user reading a refusal: a message
  # about `uv_screen` names an internal object, not the models they
  # passed. Capability guards read this instead.
  info$extras$screen_classes <- unique(vapply(
    fits,
    function(f) class(f)[1L],
    character(1)
  ))
  info$n_obs <- max(ns)
  info$extras$has_singular <- any(singular)
  info$extras$singular_terms <- unique(as.character(singular_terms))
  # The composite is not class "glm", so the footer's vcov theme would
  # fall back to the frame's raw vcov_label and print a different string
  # than a glm next to it ("Model-based (asymptotic)" vs "classical
  # (Fisher information)"), splitting the note into per-model lines.
  # Resolve the label the same way the footer would for the underlying
  # fits, so the two groups dedupe to one line.
  info$vcov_label <- format_vcov_label_from_frame(list(info = base_info))
  # Model-level statistics are undefined for a bundle (one fit per
  # block): blank them so the fit-stat rows print nothing under its
  # group. The display-blank marker keeps the renderers from en-dashing
  # these cells like a mixed-table class mismatch.
  info$fit_stats <- .blank_fit_stats(info$fit_stats)
  info$extras$fit_stats_display_blank <- TRUE
  # The pooled frame is not a fit: no AME backend, no single R-squared,
  # nothing to compare a hierarchy against (`nested = TRUE` is refused
  # upstream). The partial-effect flag is the exception, because those
  # rows come from the wrapped fits and are already in `coefs`.
  info$supports$ame <- FALSE
  info$supports$partial_effect_size <- any_partial_es
  info$supports$classical_r2 <- FALSE
  info$supports$nested_lrt <- FALSE
  info$supports$standardise_refit <- FALSE
  info
}
