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
#' note, not shown in the table. Each outcome is fitted by [stats::lm()]
#' or [stats::glm()], as `method` and `family` say.
#'
#' @details
#' # Which model for which outcome
#' The function does not guess a model from the type of an outcome: it
#' fits what `method` and `family` say.
#'
#' | You type | Models |
#' |---|---|
#' | neither `method` nor `family` | every outcome by `lm()` |
#' | `method = "glm"` | every outcome by `glm()`, `family = binomial()` |
#' | `family = poisson()` (one family) | every outcome by `glm()` with that family |
#' | `family = list(smoking = binomial(), visits = poisson())` | the named outcomes by `glm()`, the others by `lm()` |
#' | a list and `method` | an error: the list already says which outcomes are glm |
#'
#' A family is given in any form [stats::glm()] takes: the object, its
#' name or its constructor, in a list as alone. A name of the list that is
#' not an outcome is an error, and so is `gaussian()` with the identity
#' link (leave that outcome out of the list: it is then fitted by `lm()`).
#' A two-level outcome (a factor, a logical, or 0/1) fitted by `lm()` is a
#' linear probability model of its second level, and a warning says so
#' unless `method = "lm"` was typed.
#'
#' # Scales
#' `exponentiate = TRUE` exponentiates the blocks of the glm with a log
#' or logit link (odds ratios, rate ratios) and leaves the linear models
#' on their scale. When every block has the same measure, the column is
#' headed by it ("B", "OR", "IRR"). When the measures differ, the column
#' is headed "Estimate" and each outcome names its own ("smoking (OR)",
#' "bmi (B)"); the note says which models are exponentiated. `tidy()` and
#' `output = "long"` say it row by row in their `exponentiated` column.
#' The `"t"` column is refused when linear models and glm mix: it would
#' hold a t for some rows and a z for others.
#'
#' # Sample sizes
#' By default each model is fit on its **own complete cases** (its
#' outcome, the exposure and the covariates), so N can vary across
#' outcomes. The `N` column shows each model's N on the row of its
#' outcome, or on the header of its block (the line that names the
#' outcome) when the outcome has several rows, and the note gives the
#' range whenever the Ns differ. The R-squared columns are placed the same
#' way. Pass `complete_cases = TRUE` to fit every model on the rows
#' complete on all the outcomes, the exposure and the covariates; the
#' note then gives that common N.
#'
#' # Multiplicity
#' The p-values are adjusted by default (`p_adjust = "holm"`), across
#' the exposure rows of every model taken as one family: k tests for a
#' numeric exposure and k outcomes, k x (L - 1) for a factor of L levels.
#' The covariates and the intercepts are not part of the family. The
#' note counts the outcomes and the tests. `p_adjust = "none"` gives the
#' unadjusted p-values, and any method of [stats::p.adjust()] is
#' accepted. A term-level test (`"partial_f"`, `"partial_chi2"`) counts
#' once per outcome, in a family of its own.
#'
#' [table_regression()] leaves the coefficients of one model unadjusted
#' by default, each testing its own hypothesis. One exposure tested on
#' several outcomes is the other case its help names, the design with
#' several endpoints, where the adjustment is the convention: see
#' *Multiple-comparison adjustment* there.
#'
#' # Intercepts
#' Hidden by default: each model has its own intercept, which says
#' nothing about the exposure. `show_intercept = TRUE` shows them, and
#' the table then gives each outcome a block of rows, headed by the
#' outcome, opening with its model's `(Intercept)` row.
#'
#' # Refused
#' `nested`, `layout`, `keep`, `drop`, `factor_layout = "flat"`,
#' `outcome_labels`, `standardized`, the AME columns,
#' `show_columns = "none"` and a survey design are refused, each with
#' what to do instead; so are an exposure that is also an outcome or a
#' covariate, and a single outcome (use [table_regression()]).
#'
#' @param data A data frame. A survey design is refused with a pointer to
#'   the design-based route (class `spicy_wrong_regime`).
#' @param outcomes The outcome columns, two at least (tidyselect, e.g.
#'   `c(bmi, wellbeing_score)` or `starts_with("life_sat")`). The exposure
#'   is not part of the selection: `everything()` and the other helpers
#'   skip it, and naming it is an error.
#' @param exposure The exposure column (unquoted name, tidyselect): numeric,
#'   logical, a factor or a character column.
#' @param covariates Covariate columns entered in every model (tidyselect),
#'   or `NULL` (default) for unadjusted models. The exposure and the
#'   outcomes are not part of the selection.
#' @param method The model fitted for every outcome: `"lm"` (default) or
#'   `"glm"`. Not combined with a list of families. See *Which model for
#'   which outcome*.
#' @param family The family of the glm: one family for every outcome
#'   (default `binomial()`, used when `method = "glm"`; typing a family
#'   alone selects `glm()`), or a named list giving a family to some
#'   outcomes, the others fitted by `lm()`. See *Which model for which
#'   outcome*.
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
#' @param title Table title; `NULL` (default) names the model when every
#'   outcome has the same (`"Linear regression of each outcome on
#'   <exposure>"`, `"Logistic regression of each outcome on <exposure>"`),
#'   else `"Regression of each outcome on <exposure>"`.
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
#' @return See [table_regression()] (same output contract). The first
#'   column is headed "Outcome". `tidy()` and `output = "long"` give one
#'   row per outcome and coefficient of the exposure, named as R names the
#'   coefficient, with the outcome in the `outcome` column and whether the
#'   estimate is exponentiated in `exponentiated`. `glance()` gives one row
#'   per outcome, the fit statistics of its model.
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
#'
#' # A binary outcome in a logistic model, beside two linear ones: its
#' # odds ratio next to their coefficients.
#' table_regression_outcomes(
#'   sochealth,
#'   outcomes     = c(wellbeing_score, bmi, smoking),
#'   exposure     = age,
#'   covariates   = sex,
#'   family       = list(smoking = binomial()),
#'   exponentiate = TRUE
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
  method_supplied <- !missing(method)
  family_supplied <- !missing(family)
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
  models <- .outcomes_resolve_models(
    method,
    family,
    method_supplied,
    family_supplied,
    outcome_names,
    env = parent.frame()
  )

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

  # A two-level outcome of a linear model is coded 0/1: a linear
  # probability model, disclosed unless `method = "lm"` was typed.
  for (y in outcome_names[models$methods == "lm"]) {
    data <- .screen_lpm_recode(
      data,
      y,
      method_supplied,
      texts = .outcomes_lpm_texts(y)
    )
  }

  rhs <- c(exposure_name, covariate_names)
  fits <- vector("list", length(outcome_names))
  for (k in seq_along(outcome_names)) {
    fits[[k]] <- .screen_fit_or_abort(
      rhs,
      paste0("`", outcome_names[k], "`"),
      models$methods[[k]],
      data,
      models$families[[k]],
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
      methods = models$methods,
      families = models$families,
      complete_cases = isTRUE(complete_cases),
      n_data = nrow(data),
      show_intercept = show_intercept
    ),
    class = c("spicy_outcomes_screen", "spicy_screen")
  )
  dots$cluster <- .screen_stamp_cluster(dots$cluster, cluster_name)

  title_typed <- is.character(title) && nzchar(title)
  if (is.null(title)) {
    title <- .outcomes_title(models, exposure_note)
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


# The model of each outcome, from what was typed (no type detection):
# nothing, every outcome by lm(); `method = "glm"`, every outcome by glm()
# with `family` (binomial by default); a family alone, every outcome by
# glm() with it; a named list of families, the named outcomes by glm()
# and the others by lm(). Returns list(methods, families), one element
# per outcome, the family NULL for lm().
.outcomes_resolve_models <- function(
  method,
  family,
  method_supplied,
  family_supplied,
  outcome_names,
  env,
  call = rlang::caller_env()
) {
  k <- length(outcome_names)
  if (family_supplied && is.list(family) && !inherits(family, "family")) {
    if (method_supplied) {
      spicy_abort(
        c(
          "A list of families and `method` cannot be combined.",
          "i" = paste0(
            "The list names the outcomes fitted by `glm()`, the others are ",
            "fitted by `lm()`: drop `method`."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    nms <- names(family)
    if (
      length(family) == 0L ||
        is.null(nms) ||
        any(is.na(nms) | !nzchar(nms)) ||
        anyDuplicated(nms) > 0L
    ) {
      spicy_abort(
        c(
          "A list of families must name each family by its outcome, once.",
          "i" = "E.g. `family = list(smoking = binomial(), visits = poisson())`."
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    unknown <- setdiff(nms, outcome_names)
    if (length(unknown) > 0L) {
      spicy_abort(
        c(
          sprintf(
            "`family` names %s, not among the outcomes.",
            paste0("`", unknown, "`", collapse = ", ")
          ),
          "i" = sprintf(
            "The outcomes: %s.",
            paste0("`", outcome_names, "`", collapse = ", ")
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    families <- vector("list", k)
    for (y in nms) {
      fam <- .screen_normalize_family(family[[y]], env, call = call)
      .screen_refuse_gaussian_glm(
        "glm",
        fam,
        hint = sprintf(
          "Leave `%s` out of `family`: it is then fitted by `lm()`.",
          y
        ),
        call = call
      )
      families[[match(y, outcome_names)]] <- fam
    }
    methods <- ifelse(vapply(families, is.null, logical(1)), "lm", "glm")
    return(list(methods = methods, families = families))
  }
  # A family without a method can only mean glm().
  method <- if (family_supplied && !method_supplied) {
    "glm"
  } else {
    spicy_match_arg(method, c("lm", "glm"), "method")
  }
  family <- .screen_normalize_family(family, env, call = call)
  if (identical(method, "lm")) {
    if (family_supplied) {
      if (
        identical(family$family, "gaussian") &&
          identical(family$link, "identity")
      ) {
        spicy_warn(
          "`family = gaussian()` is ignored for `method = \"lm\"`, which fits it.",
          class = "spicy_ignored_arg"
        )
      } else {
        spicy_abort(
          c(
            "`family` is not meaningful for `method = \"lm\"`.",
            "i" = "Use `method = \"glm\"` to fit that family, or drop `family`."
          ),
          class = "spicy_invalid_input",
          call = call
        )
      }
    }
    return(list(methods = rep("lm", k), families = vector("list", k)))
  }
  .screen_refuse_gaussian_glm(
    method,
    family,
    hint = "Drop `family` and `method`: the outcomes are then fitted by `lm()`.",
    call = call
  )
  list(methods = rep("glm", k), families = rep(list(family), k))
}

# The default title: "Linear regression of each outcome on income_k",
# the model named with the prefix the title of one such model takes
# (through the title bridge, as the univariable screen), when every
# outcome has the same; "Regression of each outcome on ..." when they mix.
# A prefix the bridge does not carry keeps the generic title in a
# translated table, and its own name in English.
.outcomes_title <- function(models, exposure_note) {
  prefix <- unique(vapply(
    seq_along(models$methods),
    function(k) {
      capitalize_first(.uv_title_prefix(
        models$methods[[k]],
        models$families[[k]]
      ))
    },
    character(1)
  ))
  if (length(prefix) > 1L) {
    return(spicy_fmt("title_outcomes", exposure_note))
  }
  disp <- .title_prefix_display(prefix)
  if (!is.null(disp)) {
    return(spicy_fmt("title_outcomes_model", disp, exposure_note))
  }
  if (
    is.null(.spicy_title_prefix_table(.spicy_language_option(
      getOption("spicy.language", "en")
    )))
  ) {
    return(.spicy_fmt_en("title_outcomes_model", prefix, exposure_note))
  }
  spicy_fmt("title_outcomes", exposure_note)
}

# The linear probability model warning and the refusal of a factor of
# more than two levels, worded for the outcome `y` of a table of outcomes
# (.screen_lpm_recode()).
.outcomes_lpm_texts <- function(y) {
  list(
    binary = paste0(
      "`%s` is binary: its linear model is a linear probability model ",
      "for P(%s = %s)."
    ),
    glm_hint = sprintf(
      "For a logistic model of this outcome, use `family = list(%s = binomial())`.",
      y
    ),
    levels = paste0(
      "A linear model needs a numeric or two-level outcome; ",
      "`%s` has %d observed levels."
    ),
    levels_hint = paste0(
      "Fit a `nnet::multinom()` model of it and pass it to ",
      "`table_regression()`."
    )
  )
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
  # The frames of lm() and glm() fits do not carry the same columns.
  coefs <- Reduce(.rbind_union, blocks)
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
  # The scale of each block: each fit was exponentiated by its own frame
  # where its link allows it, so the blocks may mix B, OR and IRR. The
  # column is headed by their common measure, "Estimate" when they
  # differ (each outcome then names its own, .reg_outcome_block_labels()),
  # and the composite is never exponentiated again.
  exp_k <- vapply(
    frames,
    function(fr) isTRUE(fr$info$extras$exp_applied),
    logical(1)
  )
  measure <- vapply(
    frames,
    function(fr) {
      if (isTRUE(fr$info$extras$exp_applied)) {
        fr$info$extras$exp_header
      } else {
        spicy_str("header_b")
      }
    },
    character(1)
  )
  mixed <- length(unique(measure)) > 1L
  info$extras$exp_applied <- any(exp_k)
  info$extras$exp_header <- if (mixed) {
    spicy_str("header_estimate")
  } else if (any(exp_k)) {
    measure[[1L]]
  } else {
    NA_character_
  }
  info$supports$exponentiate <- FALSE
  coefs$exponentiated <- .reg_rows_exponentiated(
    coefs,
    exp_k[match(coefs$outcome, bundle$outcomes)]
  )
  .outcomes_refuse_mixed_t(coefs, show_columns)
  info$extras$outcome_blocks <- data.frame(
    outcome = bundle$outcomes,
    n = ns,
    exp_applied = exp_k,
    measure = measure,
    mixed_scale = mixed,
    stringsAsFactors = FALSE
  )
  # The frame of each model, for the themes of the note that speak of the
  # models (.reg_expand_blocks()).
  info$extras$outcome_infos <- lapply(frames, `[[`, "info")
  # The fit statistics of each model, for glance() (.reg_block_fit_stats()):
  # the table itself shows none.
  info$extras$block_fit_stats <- lapply(frames, function(fr) {
    fr$info$fit_stats
  })
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
# for every block. The outcome itself is labeled under its own name and,
# when the blocks are on different scales, followed by its measure
# ("smoking (OR)"). Returns the labels and the frames, each table of
# outcomes stamped with the bare label of its outcomes
# (`outcome_blocks$display`), which the note names its models by.
.reg_outcome_block_labels <- function(frames, terms) {
  for (i in seq_along(frames)) {
    keys <- frames[[i]]$info$extras$outcome_keys
    if (!is.data.frame(keys)) {
      next
    }
    if (length(terms) > 0L) {
      keyed <- keys$key != keys$outcome &
        keys$term %in% names(terms) &
        !keys$key %in% names(terms)
      terms[keys$key[keyed]] <- terms[keys$term[keyed]]
    }
    ob <- frames[[i]]$info$extras$outcome_blocks
    display <- ob$outcome
    has <- display %in% names(terms)
    display[has] <- unname(terms[display[has]])
    frames[[i]]$info$extras$outcome_blocks$display <- display
    if (isTRUE(ob$mixed_scale[1L])) {
      if (is.null(terms)) {
        terms <- character(0)
      }
      terms[ob$outcome] <- spicy_fmt(
        "label_outcome_measure",
        display,
        ob$measure
      )
    }
  }
  list(terms = terms, frames = frames)
}

# The frames of the note's themes that speak of each model (its type, the
# scale of its coefficients, its standard errors, its abbreviations): a
# table of outcomes stands for one model per outcome, named by its
# outcome, each with its own frame. Other frames are returned as they are.
.reg_expand_blocks <- function(frames) {
  out <- list()
  for (f in frames) {
    infos <- f$info$extras$outcome_infos
    if (is.null(infos)) {
      out[[length(out) + 1L]] <- f
      next
    }
    ob <- f$info$extras$outcome_blocks
    for (k in seq_along(infos)) {
      sub <- f
      sub$info <- infos[[k]]
      sub$info$model_label <- (ob$display %||% ob$outcome)[[k]]
      # The table shows no fit statistic of its models.
      sub$info$fit_stats <- f$info$fit_stats
      sub$info$extras$fit_stats_display_blank <- TRUE
      sub$coefs <- f$coefs[f$coefs$outcome == ob$outcome[[k]], , drop = FALSE]
      out[[length(out) + 1L]] <- sub
    }
  }
  out
}

# The fit statistics glance() reads for a table of outcomes: one row per
# outcome, each its own model's, in the shape align_frames() gives the
# fit statistics. NULL for any other table: glance() then reads those of
# align_frames().
.reg_block_fit_stats <- function(frames, model_ids) {
  # A table of outcomes is the one model of its call.
  fs <- if (length(frames) == 1L) frames[[1L]]$info$extras$block_fit_stats
  if (is.null(fs)) {
    return(NULL)
  }
  ob <- frames[[1L]]$info$extras$outcome_blocks
  out <- Reduce(
    .rbind_union,
    lapply(seq_along(fs), function(k) {
      as.data.frame(
        .compact_fit_stats_for_legacy(
          fs[[k]],
          model_ids[[1L]],
          ob$outcome[[k]]
        ),
        stringsAsFactors = FALSE
      )
    })
  )
  rownames(out) <- NULL
  out
}

# The "t" column holds one statistic: a t for a linear model, a z for a
# glm with a fixed dispersion. Outcomes that mix the two have no header
# for it.
.outcomes_refuse_mixed_t <- function(
  coefs,
  show_columns,
  call = rlang::caller_env()
) {
  tt <- unique(coefs$test_type[coefs$estimate_type == "B" & !coefs$is_ref])
  if ("t" %in% show_columns && length(tt[!is.na(tt)]) > 1L) {
    spicy_abort(
      c(
        "The `\"t\"` column is not available when the outcomes mix t and z statistics.",
        "i" = "Its linear models report a t, its glm models a z: drop `\"t\"` from `show_columns`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
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
