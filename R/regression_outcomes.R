# One exposure across several outcomes: one model per outcome, the same
# exposure and the same adjustment in each, shown as one table with one
# row (or one block of rows) per outcome. The function fits the models,
# wraps them in a `spicy_outcomes_screen` bundle and hands it to
# table_regression(); the bundle's as_regression_frame() method builds
# ONE composite frame whose rows are the exposure rows of every model,
# keyed by outcome. The checks and the fitting it shares with the
# univariable screen live in R/regression_screen.R.

#' One exposure across several outcomes
#'
#' Fits one model per outcome, each with the same exposure and the same
#' covariates, and shows the exposure's coefficients in one table: one
#' row per outcome for a numeric exposure, one block of rows per outcome
#' for a factor. The covariates enter every model and are named in the
#' note, not shown in the table. This version fits linear models
#' ([stats::lm()]) only.
#'
#' @details
#' # Sample sizes
#' By default each model is fit on its **own complete cases** (its
#' outcome, the exposure and the covariates), so N can vary across
#' outcomes. The `N` column shows each model's N on the row of its
#' outcome, or on the header of its block (the line that names the
#' outcome) when the outcome has several rows, and the note gives the
#' range whenever the Ns differ. The R-squared columns are placed the same
#' way. Pass
#' `complete_cases = TRUE` to fit every model on the rows complete on
#' all the outcomes, the exposure and the covariates; the note then gives
#' that common N.
#'
#' # Multiplicity
#' The p-values are adjusted by default (`p_adjust = "holm"`), across
#' the exposure rows of every model taken as one family: k tests for a
#' numeric exposure and k outcomes, k x (L - 1) for a factor of L levels.
#' The covariates and the intercepts are not part of the family. The
#' note counts the outcomes and the tests. `p_adjust = "none"` gives the
#' unadjusted p-values, and any method of [stats::p.adjust()] is
#' accepted.
#'
#' # Intercepts
#' Hidden by default: each model has its own intercept, which says
#' nothing about the exposure. `show_intercept = TRUE` shows them, and
#' the table then gives each outcome a block of rows, headed by the
#' outcome, opening with its model's `(Intercept)` row.
#'
#' @param data A data frame. A survey design is refused with a pointer to
#'   the design-based route (class `spicy_wrong_regime`).
#' @param outcomes The outcome columns, two at least (tidyselect, e.g.
#'   `c(bmi, wellbeing_score)` or `starts_with("life_sat")`). Numeric
#'   columns only in this version. The exposure is not part of the
#'   selection: `everything()` and the other helpers skip it, and naming
#'   it is an error.
#' @param exposure The exposure column (unquoted name, tidyselect): numeric,
#'   logical, a factor or a character column.
#' @param covariates Covariate columns entered in every model (tidyselect),
#'   or `NULL` (default) for unadjusted models. The exposure and the
#'   outcomes are not part of the selection.
#' @param method The model fitted for every outcome. `"lm"` (default) is
#'   the only value accepted in this version.
#' @param family Not accepted in this version, which fits linear models
#'   only.
#' @param complete_cases Logical, default `FALSE`. `TRUE` fits every
#'   model on the rows complete on all the outcomes, the exposure and the
#'   covariates. See *Sample sizes*.
#' @param p_adjust The adjustment of the p-values for multiple testing,
#'   across the exposure rows of all the models: `"holm"` (default),
#'   `"none"`, or any other method of [stats::p.adjust()]. See
#'   *Multiplicity*.
#' @param show_columns Passed to [table_regression()]. Default
#'   `c("n", "b", "ci", "p")`; `"n"` is the sample size of each model.
#'   `"r2"` and `"adj_r2"` give each outcome's own model R-squared (linear
#'   models), next to its N. The AME columns and `"none"` are refused.
#' @param show_intercept Display the `(Intercept)` row of each model?
#'   Default `FALSE`. See *Intercepts*.
#' @param labels How the variables are named: an outcome in its row or at
#'   the head of its block, the exposure in the title and on its rows, a
#'   covariate in the note. The four forms of [table_regression()]:
#'   `FALSE` (the column names), `TRUE` (each column's `label` attribute
#'   when it has one), a named character vector, or [var_labels()]. The
#'   names are the columns and the coefficient names (`"sexMale"` names
#'   the row of one level, in every block). The default reads
#'   `options(spicy.var_labels)`.
#' @param title Table title; `NULL` (default) builds
#'   `"Regression of each outcome on <exposure>"`.
#' @param ... Passed to [table_regression()] (`exponentiate`, `vcov`,
#'   `cluster`, `ci_level`, `digits`, `output`, ...), each by its full
#'   name: an unknown or abbreviated name is an error. `nested`, `layout`,
#'   `keep`, `drop`, `factor_layout = "flat"`, `outcome_labels` and
#'   `standardized` do not apply to a table of outcomes and are refused.
#'   `cluster` must be a single vector with one value per row of `data`;
#'   it is aligned to each model's own estimation sample.
#' @param note_labels A named character vector giving the exposure (in
#'   the title) and the covariates (in the note) a label of their own. The
#'   outcomes are named in the rows and take none.
#' @inheritParams table_categorical
#'
#' @return See [table_regression()] (same output contract). `tidy()` and
#'   `output = "long"` give one row per outcome and coefficient of the
#'   exposure, named as R names the coefficient, with the outcome in the
#'   `outcome` column.
#'
#' @seealso [table_regression_uv()] for one outcome and several candidate
#'   predictors, [table_regression()] for models fitted beforehand.
#'
#' @examples
#' # A numeric exposure: one row per outcome.
#' table_regression_outcomes(
#'   sochealth,
#'   outcomes   = c(wellbeing_score, bmi, life_sat_health),
#'   exposure   = age,
#'   covariates = sex
#' )
#'
#' # A factor exposure: one block of levels per outcome.
#' table_regression_outcomes(
#'   sochealth,
#'   outcomes   = c(wellbeing_score, bmi),
#'   exposure   = employment_status,
#'   covariates = c(age, sex),
#'   labels     = TRUE
#' )
#' @export
table_regression_outcomes <- function(
  data,
  outcomes,
  exposure,
  covariates = NULL,
  method = c("lm", "glm"),
  family = stats::binomial(),
  complete_cases = FALSE,
  p_adjust = "holm",
  show_columns = c("n", "b", "ci", "p"),
  show_intercept = FALSE,
  labels = getOption("spicy.var_labels", FALSE),
  title = NULL,
  ...,
  # After `...`, so a name must be typed in full: `note =` would
  # otherwise partially match `note_labels`.
  note_labels = NULL,
  note = NULL,
  note_add = NULL
) {
  oc_env <- rlang::current_env()
  oc_call <- sys.call()
  .screen_check_dots(
    names(as.list(substitute(list(...)))[-1L]),
    ...length(),
    names(formals(table_regression_outcomes)),
    layout_msg = c(
      paste0(
        "`layout` does not apply to `table_regression_outcomes()`: ",
        "its outcomes are blocks of one table."
      ),
      "i" = "Stack several tables with `table_stack()`."
    )
  )
  .outcomes_linear_only(!missing(method), !missing(family), method)
  .check_data_frame(data, "table_regression_outcomes")
  .check_labels_arg(labels)
  .check_note_labels_arg(note_labels)
  validate_logical_scalar(complete_cases, "complete_cases")
  validate_logical_scalar(show_intercept, "show_intercept")
  dots <- list(...)
  .outcomes_refuse_args(dots, show_columns)

  # The exposure first: the outcomes and the covariates are selected
  # without it, and an outcome is no covariate.
  exposure_name <- resolve_single_column_selection(
    rlang::enquo(exposure),
    data,
    "exposure"
  )
  outcome_names <- .resolve_selection_without(
    rlang::enquo(outcomes),
    data,
    roles = c(exposure = exposure_name),
    arg = "outcomes"
  )
  if (length(outcome_names) == 0L) {
    .abort_empty_selection(
      "outcomes",
      hint = "Pass at least two outcomes, e.g. `outcomes = c(bmi, wellbeing_score)`."
    )
  }
  if (length(outcome_names) == 1L) {
    spicy_abort(
      c(
        "`table_regression_outcomes()` needs at least two outcomes.",
        "i" = sprintf(
          "For the one model of `%s`, use `table_regression()`.",
          outcome_names
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  covariate_names <- .resolve_selection_without(
    rlang::enquo(covariates),
    data,
    roles = stats::setNames(
      c(exposure_name, outcome_names),
      c("exposure", rep("outcomes", length(outcome_names)))
    ),
    arg = "covariates"
  )
  .outcomes_numeric_only(data, outcome_names)

  # The exposure is named in the title, the covariates in the note: both
  # take `labels`, and a label of `note_labels`. The outcomes are named in
  # the rows, by table_regression(), and take no note label.
  vars <- c(outcome_names, exposure_name, covariate_names)
  .warn_unknown_names("note_labels", names(note_labels), vars)
  shown <- resolve_variable_labels(data, vars, labels)
  noted <- .note_labels_of(vars, shown, note_labels)
  .check_note_label_collisions(
    vars,
    shown,
    noted,
    note_labels[names(note_labels) %in% c(exposure_name, covariate_names)]
  )
  in_note <- vars %in% c(exposure_name, covariate_names)
  noted[!in_note] <- shown[!in_note]
  exposure_note <- noted[[match(exposure_name, vars)]]
  covariates_note <- noted[match(covariate_names, vars)]

  cluster_name <- extract_arg_column_name(
    match.call(expand.dots = TRUE)$cluster
  )
  .screen_check_cluster(
    dots$cluster,
    nrow(data),
    what = "`table_regression_outcomes()`"
  )
  dots$show_intercept <- show_intercept
  dots$p_adjust <- p_adjust

  if (isTRUE(complete_cases)) {
    cc <- .screen_complete_cases(data, vars, dots$cluster)
    data <- cc$data
    dots$cluster <- cc$cluster
  }

  rhs <- c(exposure_name, covariate_names)
  fits <- vector("list", length(outcome_names))
  for (k in seq_along(outcome_names)) {
    fits[[k]] <- .screen_fit_or_abort(
      rhs,
      paste0("`", outcome_names[k], "`"),
      "lm",
      data,
      NULL,
      failed = sprintf(
        "The model for the outcome `%s` failed to fit.",
        outcome_names[k]
      ),
      call = oc_env
    )
  }
  names(fits) <- outcome_names

  bundle <- structure(
    list(
      fits = fits,
      outcomes = outcome_names,
      exposure = exposure_name,
      covariates = covariate_names,
      covariates_note = covariates_note,
      methods = rep("lm", length(outcome_names)),
      complete_cases = isTRUE(complete_cases),
      n_data = nrow(data),
      show_intercept = show_intercept
    ),
    class = c("spicy_outcomes_screen", "spicy_screen")
  )
  dots$cluster <- .screen_stamp_cluster(dots$cluster, cluster_name)

  title_typed <- is.character(title) && nzchar(title)
  if (is.null(title)) {
    title <- spicy_fmt("title_outcomes", exposure_note)
  }

  .screen_call_table_regression(
    list(bundle),
    c(
      list(
        show_columns = show_columns,
        labels = labels,
        title = title,
        note = note,
        note_add = note_add
      ),
      dots
    ),
    call = oc_call,
    title_typed = title_typed
  )
}


# This version fits linear models only. A `family`, or a `method` other
# than "lm", is refused until the glm models are wired in.
.outcomes_linear_only <- function(
  method_supplied,
  family_supplied,
  method,
  call = rlang::caller_env()
) {
  method <- spicy_match_arg(method, c("lm", "glm"), "method")
  if (family_supplied || !identical(method, "lm")) {
    spicy_abort(
      c(
        "`table_regression_outcomes()` fits linear models only for now.",
        "i" = "Drop `method` and `family`, or fit the models and pass them to `table_regression()`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# An outcome a linear model cannot take as it is: until the linear
# probability model of a two-level outcome is wired in, every outcome
# is numeric.
.outcomes_numeric_only <- function(
  data,
  outcome_names,
  call = rlang::caller_env()
) {
  bad <- outcome_names[
    !vapply(outcome_names, function(v) is.numeric(data[[v]]), logical(1))
  ]
  if (length(bad) > 0L) {
    spicy_abort(
      c(
        sprintf(
          "`table_regression_outcomes()` takes numeric outcomes only for now, not %s.",
          paste0("`", bad, "`", collapse = ", ")
        ),
        "i" = "Code a two-level outcome 0/1, or fit the models and pass them to `table_regression()`."
      ),
      class = "spicy_invalid_data",
      call = call
    )
  }
  invisible(NULL)
}

# The arguments of `table_regression()` a table of outcomes has no use
# for: they compare nested models, pick rows the outcomes already pick,
# rename an Outcome row the table does not have, or show columns that
# are not one quantity across the outcomes.
.outcomes_refuse_args <- function(
  dots,
  show_columns,
  call = rlang::caller_env()
) {
  refuse <- function(msg, hint) {
    spicy_abort(c(msg, "i" = hint), class = "spicy_invalid_input", call = call)
  }
  if (is.character(show_columns) && "none" %in% show_columns) {
    refuse(
      "`show_columns = \"none\"` is not available for `table_regression_outcomes()`.",
      paste0(
        "Each model reports its sample size on the rows of its outcome ",
        "(`\"n\"`). To compare fitted models by their fit statistics alone, ",
        "use `table_regression(models, show_columns = \"none\")`."
      )
    )
  }
  if (is.character(show_columns) && any(.AME_COLUMN_TOKENS %in% show_columns)) {
    refuse(
      "Average marginal effects are not available in `table_regression_outcomes()`.",
      "Fit the models and pass them to `table_regression()` for their AME."
    )
  }
  if (isTRUE(dots$nested)) {
    refuse(
      "`nested = TRUE` is not meaningful for a table of outcomes.",
      "Its models have different outcomes and are not nested in one another."
    )
  }
  for (a in intersect(c("keep", "drop"), names(dots))) {
    if (!is.null(dots[[a]])) {
      refuse(
        sprintf(
          "`%s` does not apply to `table_regression_outcomes()`.",
          a
        ),
        paste0(
          "`outcomes` chooses the rows, and the covariates are not shown: ",
          "the note names them."
        )
      )
    }
  }
  if (identical(dots$factor_layout, "flat")) {
    refuse(
      "`factor_layout = \"flat\"` is not available for `table_regression_outcomes()`.",
      "A factor exposure is shown as one block of levels per outcome."
    )
  }
  if (!is.null(dots$outcome_labels)) {
    refuse(
      "`outcome_labels` does not apply to `table_regression_outcomes()`.",
      "Each outcome names its own rows: rename it with `labels`."
    )
  }
  if (!is.null(dots$standardized) && !identical(dots$standardized, "none")) {
    refuse(
      "`standardized` is not available for `table_regression_outcomes()`.",
      "Standardize the variables in `data` before the call."
    )
  }
  invisible(NULL)
}


#' Terms method for bundles of one model per outcome
#'
#' @description
#' Returns the [stats::terms()] object of the formula
#' `outcome_1 ~ exposure + covariates`, the right-hand side every model
#' fitted by [table_regression_outcomes()] shares. Non-syntactic column
#' names are backtick-quoted. Used internally to read the term labels a
#' `labels` entry can name, off every model in a table.
#'
#' @param x A `spicy_outcomes_screen` bundle (the internal object wrapping
#'   the models of [table_regression_outcomes()]).
#' @param ... Additional arguments (currently ignored).
#'
#' @return A `terms` object.
#'
#' @seealso [table_regression_outcomes()]
#' @keywords internal
#' @export
terms.spicy_outcomes_screen <- function(x, ...) {
  bt <- function(v) paste0("`", v, "`")
  stats::terms(stats::reformulate(
    bt(c(x$exposure, x$covariates)),
    response = bt(x$outcomes[[1L]])
  ))
}


#' @export
as_regression_frame.spicy_outcomes_screen <- function(
  fit,
  model_id = "M1",
  vcov = "classical",
  cluster = NULL,
  boot_n = 1000L,
  ci_level = 0.95,
  ci_method = "wald",
  standardized = "none",
  exponentiate = FALSE,
  show_columns = c("b", "se", "ci", "p"),
  show_fit_stats = NULL,
  use_ame_satterthwaite = FALSE,
  cluster_name = NULL,
  re_ci = "wald",
  tau = NULL,
  at_time = NULL,
  ...
) {
  bundle <- fit
  expo <- c(bundle$exposure, .reg_tick_names(bundle$exposure))
  frames <- vector("list", length(bundle$fits))
  blocks <- vector("list", length(bundle$fits))
  ns <- integer(length(bundle$fits))
  for (k in seq_along(bundle$fits)) {
    fr <- .screen_block_frame(
      bundle$fits[[k]],
      model_id = model_id,
      vcov = vcov,
      cluster = .screen_cluster_for_fit(cluster, bundle$fits[[k]]),
      boot_n = boot_n,
      ci_level = ci_level,
      ci_method = ci_method,
      exponentiate = exponentiate,
      show_columns = show_columns,
      show_fit_stats = show_fit_stats,
      cluster_name = cluster_name,
      tau = tau,
      at_time = at_time
    )
    frames[[k]] <- fr
    ns[k] <- .uv_fit_n(bundle$fits[[k]])
    cf <- fr$coefs
    block <- cf[cf$parent_var %in% expo, , drop = FALSE]
    # The N of the model, and its R-squared, on the first coefficient row
    # of its block, as in the univariable screen.
    first <- which(block$estimate_type == "B" & !block$is_ref)[1L]
    r2_k <- .uv_fit_r2(fr$info)
    block$n_obs <- NA_real_
    block$n_obs[first] <- as.numeric(ns[k])
    block$r2 <- NA_real_
    block$r2[first] <- r2_k[["r2"]]
    block$adj_r2 <- NA_real_
    block$adj_r2[first] <- r2_k[["adj_r2"]]
    block$is_intercept <- FALSE
    ic <- cf[cf$term == "(Intercept)", , drop = FALSE]
    if (nrow(ic) > 0L) {
      ic$n_obs <- NA_real_
      ic$r2 <- NA_real_
      ic$adj_r2 <- NA_real_
      ic$is_intercept <- TRUE
      block <- rbind(ic, block)
    }
    blocks[[k]] <- .outcomes_rekey_block(
      block,
      bundle$outcomes[[k]],
      flat = !isTRUE(bundle$show_intercept) &&
        all(
          block$term[!block$is_intercept] ==
            block$parent_var[!block$is_intercept]
        )
    )
  }
  coefs <- do.call(rbind, blocks)
  rownames(coefs) <- NULL

  info <- .screen_pool_info(frames, bundle$fits, ns, "outcomes_screen")
  # Each row's key and what it stands for in the model it comes from, for
  # tidy(), the long output and the labels of the rows.
  info$extras$outcome_keys <- data.frame(
    key = coefs$term,
    outcome = coefs$outcome,
    term = coefs$.orig_term,
    parent_var = coefs$.orig_parent,
    stringsAsFactors = FALSE
  )
  info$extras$outcome_blocks <- data.frame(
    outcome = bundle$outcomes,
    n = ns,
    stringsAsFactors = FALSE
  )
  info$extras$outcomes_design <- paste(
    c(
      spicy_str("note_outcomes_separate"),
      if (length(bundle$covariates) > 0L) {
        spicy_fmt(
          "note_outcomes_adjusted_for",
          .join_and(bundle$covariates_note)
        )
      }
    ),
    collapse = " "
  )
  # The sample sentence of the note, read by the theme of the univariable
  # screen: nothing when every model has the same N.
  info$extras$uv_disclosure <- if (isTRUE(bundle$complete_cases)) {
    spicy_fmt("note_uv_common_complete_cases", bundle$n_data)
  } else if (length(unique(ns)) > 1L) {
    spicy_fmt("note_outcomes_varying_n", min(ns), max(ns))
  }
  coefs$.orig_term <- NULL
  coefs$.orig_parent <- NULL
  new_regression_frame(coefs, info, bundle)
}

# The rows of one model, keyed by its outcome so that the blocks of the
# composite frame never share a term. A flat block (a numeric exposure,
# no intercept) is one row named after its outcome. Otherwise the block
# is headed by the outcome (`parent_var`), each row keyed
# "<outcome>: <term>" and keeping its label. The term and the variable
# each row stands for are kept, for the data outputs.
.outcomes_rekey_block <- function(block, outcome, flat) {
  block$.orig_term <- block$term
  block$.orig_parent <- ifelse(block$is_intercept, block$term, block$parent_var)
  block$outcome <- outcome
  # The intercept keeps its own key: hidden in a flat block, it stays out
  # of the outcome's row.
  row <- flat & !block$is_intercept
  block$term <- ifelse(row, outcome, paste0(outcome, ": ", block$term))
  block$parent_var <- ifelse(row | !flat, outcome, block$term)
  block$label[row] <- outcome
  block
}

# Footer theme: what the models of a table of outcomes are, read from
# extras$outcomes_design ("One model per outcome, fitted separately.
# Adjusted for age and sex.").
build_outcomes_design_footer_block_from_frames <- function(frames) {
  out <- unlist(lapply(frames, function(f) f$info$extras$outcomes_design))
  if (length(out) == 0L) {
    return(NULL)
  }
  out[[1L]]
}

# The labels of the rows of a table of outcomes: `terms` (the lookup of
# `.reg_variable_labels()`) gives each row keyed "<outcome>: <term>" the
# label of its term, so that `labels` names a level or the exposure once
# for every block. The outcome itself is labeled under its own name.
.reg_outcome_block_labels <- function(frames, terms) {
  for (f in frames) {
    keys <- f$info$extras$outcome_keys
    if (!is.data.frame(keys) || length(terms) == 0L) {
      next
    }
    keyed <- keys$key != keys$outcome &
      keys$term %in% names(terms) &
      !keys$key %in% names(terms)
    terms[keys$key[keyed]] <- terms[keys$term[keyed]]
  }
  terms
}

# The frames of a table of outcomes with each row named by its model's
# term and variable, for the themes of the note that name the variables
# of the rows (reference categories, polynomial trends): the exposure is
# named once, not once per outcome. Other frames are returned as they are.
.reg_dekey_frames <- function(frames) {
  lapply(frames, function(f) {
    keys <- f$info$extras$outcome_keys
    if (!is.data.frame(keys)) {
      return(f)
    }
    cf <- f$coefs
    i <- match(paste(cf$outcome, cf$term), paste(keys$outcome, keys$key))
    cf$term <- keys$term[i]
    cf$parent_var <- keys$parent_var[i]
    flat <- cf$label == keys$outcome[i] & cf$term == cf$parent_var
    cf$label[flat] <- cf$term[flat]
    f$coefs <- cf
    f
  })
}

# The term each row of a table of outcomes stands for in its model, in
# place of its key: tidy() and the long output name the coefficients as
# R does, with the outcome of the row in `outcome`.
.reg_dekey_aligned <- function(aligned, frames) {
  keys <- do.call(rbind, lapply(frames, function(f) f$info$extras$outcome_keys))
  if (is.null(keys)) {
    return(aligned)
  }
  ca <- aligned$coefs_aligned
  i <- match(paste(ca$outcome, ca$term), paste(keys$outcome, keys$key))
  hit <- !is.na(i)
  term <- keys$term[i[hit]]
  parent <- keys$parent_var[i[hit]]
  ca$term[hit] <- term
  ca$factor_term[hit] <- ifelse(parent == term, NA_character_, parent)
  ca$factor_level[hit] <- ifelse(
    parent == term,
    NA_character_,
    ca$factor_level[hit]
  )
  aligned$coefs_aligned <- ca
  aligned
}
