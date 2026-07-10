# Univariable screening tables -- the gtsummary::tbl_uvregression
# equivalent (spec: dev/uvregression_spec.md, fully validated
# 2026-07-09). One model per candidate predictor, all rendered as one
# table (one row block per predictor), optionally merged side by side
# with the full multivariable model under "Univariable" /
# "Multivariable" spanners -- the signature layout of applied
# epidemiology (EpiRHandbook, regression chapter).
#
# Architecture: table_regression_uv() is a thin wrapper. It fits the
# models, wraps the univariable fits in a `spicy_uv_screen` bundle, and
# hands `list(Univariable = bundle, Multivariable = fit)` to
# table_regression() -- the bundle's as_regression_frame() method
# builds ONE composite frame whose coefs are the per-predictor blocks
# of the individual univariable frames. Everything else (title, footers,
# central exponentiation, p_adjust across the screen as ONE family,
# robust vcov per underlying fit, every output engine, tidy()) is the
# ordinary multi-model machinery.


#' Univariable screening table (with optional multivariable merge)
#'
#' Fits one model per candidate predictor (the *univariable screen*)
#' and renders them as a single table with one row block per
#' predictor. With `multivariable = TRUE` (default), the full model
#' containing all predictors is merged side by side under
#' `"Univariable"` / `"Multivariable"` column groups -- the standard
#' presentation of applied epidemiology (the
#' `gtsummary::tbl_uvregression()` + `tbl_merge()` workflow).
#'
#' @details
#' # Sample sizes
#' By default each univariable model is fit on its **own complete
#' cases**, so N varies across predictors -- that is what the `N`
#' column discloses (shown on the first row of each block), and a
#' table note states it whenever the Ns differ. The multivariable
#' model is fit on the complete cases of **all** its variables (its
#' `n` appears in the fit-statistics rows). Pass
#' `complete_cases = TRUE` to restrict every model -- univariable and
#' multivariable -- to the common complete-case sample.
#'
#' # Multiplicity
#' `p_adjust` (passed through to [table_regression()]) treats the
#' whole univariable screen as ONE family (all screened coefficients
#' together); the multivariable model is its own family, as in any
#' multi-model table.
#'
#' # Intercepts
#' Hidden by default on both sides (each univariable fit has its own
#' nuisance intercept), matching `gtsummary::tbl_regression()`'s
#' `intercept = FALSE` default. Pass `show_intercept = TRUE` to
#' display them.
#'
#' @param data A data frame.
#' @param outcome The outcome column (unquoted name, tidyselect). For
#'   `method = "coxph"`, a `Surv(time, status)` expression evaluated
#'   in `data` (the `tbl_uvregression` convention).
#' @param predictors Candidate predictor columns (tidyselect, e.g.
#'   `c(age, sex, education)` or `where(is.numeric)`). The outcome
#'   column(s) are dropped from the selection automatically.
#' @param method `"glm"` (default), `"lm"`, or `"coxph"` (requires
#'   the `survival` package; estimates render as HRs with
#'   `exponentiate = TRUE`).
#' @param family A [stats::family] object for `method = "glm"`.
#'   Default `binomial()`. Refused for `method = "coxph"`, and
#'   `gaussian()` with the identity link is refused too: use
#'   `method = "lm"` for the linear screen.
#' @param multivariable Logical, default `TRUE`: merge the full model
#'   (all predictors together) as a second column group.
#' @param complete_cases Logical, default `FALSE`. `TRUE` restricts
#'   ALL models to the rows complete on outcome + every predictor
#'   (common-sample comparison); the reduction is disclosed in the
#'   table note.
#' @param show_columns Passed to [table_regression()]. Default
#'   `c("n", "b", "ci", "p")` -- the `tbl_uvregression` column set;
#'   `"n"` is the per-predictor sample size. The multivariable group
#'   carries no `N` column (its single `n` is a fit-statistics row,
#'   as in the reference layouts). For binary outcomes, add
#'   `"n_events"` for outcome event counts as `events/N` per factor
#'   level (each column group counts on its own estimation sample).
#'   For `method = "coxph"`, the RMST / risk-difference families
#'   (`"rmst"`, `"risk_diff"`, ...) work with an explicit numeric
#'   `tau` / `at_time` shared by every column: each univariable fit
#'   runs its own `boot_n`-replicate bootstrap, and the multivariable
#'   group reports the covariate-adjusted estimand from the full fit.
#'   `tau = "minmax"` is refused (per-fit horizons would make the
#'   column incomparable across predictors).
#' @param title Table title; `NULL` (default) builds
#'   `"Univariable and multivariable <type> regression: <outcome>"`.
#' @param ... Passed to [table_regression()] (`exponentiate`, `vcov`,
#'   `cluster`, `p_adjust`, `digits`, `labels`, `output`, ...).
#'   `show_intercept` defaults to `FALSE` here; `nested` is not
#'   meaningful for a screen and is refused. `cluster` must be a
#'   single vector with one value per row of `data`; it is aligned to
#'   each fit's own estimation sample automatically.
#'
#' @return See [table_regression()] (same output contract).
#'
#' @references
#' Batra, N. et al. (Eds.) (2021). *The Epidemiologist R Handbook*,
#' Univariate and multivariable regression.
#' <https://epirhandbook.com/en/new_pages/regression.html>
#'
#' Sjoberg, D.D., Whiting, K., Curry, M., Lavery, J.A., &
#' Larmarange, J. (2021). Reproducible summary tables with the
#' gtsummary package. *The R Journal*, 13(1), 570-580.
#'
#' @examples
#' \donttest{
#' table_regression_uv(
#'   sochealth,
#'   outcome    = smoking,
#'   predictors = c(age, sex, education),
#'   family     = binomial(),
#'   exponentiate = TRUE
#' )
#' }
#' @export
table_regression_uv <- function(data,
                                outcome,
                                predictors,
                                method = c("glm", "lm", "coxph"),
                                family = stats::binomial(),
                                multivariable = TRUE,
                                complete_cases = FALSE,
                                show_columns = c("n", "b", "ci", "p"),
                                title = NULL,
                                ...) {
  family_supplied <- !missing(family)
  method <- match.arg(method)
  if (!is.data.frame(data)) {
    spicy_abort("`data` must be a data frame.",
                class = "spicy_invalid_input")
  }
  if (!is.logical(multivariable) || length(multivariable) != 1L ||
        is.na(multivariable)) {
    spicy_abort("`multivariable` must be TRUE/FALSE.",
                class = "spicy_invalid_input")
  }
  if (!is.logical(complete_cases) || length(complete_cases) != 1L ||
        is.na(complete_cases)) {
    spicy_abort("`complete_cases` must be TRUE/FALSE.",
                class = "spicy_invalid_input")
  }
  if (identical(method, "coxph") && family_supplied) {
    spicy_abort(
      c("`family` is not meaningful for `method = \"coxph\"`.",
        "i" = "The Cox model has no family; drop the argument."),
      class = "spicy_invalid_input"
    )
  }
  # A gaussian/identity glm is lm by another name; in the screen the
  # right spelling exists as an argument, so point straight at it
  # (the generic "refit with lm()" caveat would be off-target here).
  if (identical(method, "glm") &&
        identical(family$family, "gaussian") &&
        identical(family$link, "identity")) {
    spicy_abort(
      c(paste0("`family = gaussian()` with the identity link is ",
               "`lm` by another name."),
        "i" = "Use `method = \"lm\"` for the linear screen."),
      class = "spicy_invalid_input"
    )
  }

  if (identical(method, "coxph")) {
    .check_survival_available()
    # The outcome is a `Surv(time, status)` EXPRESSION evaluated in
    # `data` (the gtsummary tbl_uvregression convention), not a single
    # column selection.
    outcome_expr <- rlang::quo_get_expr(rlang::enquo(outcome))
    is_surv_call <- is.call(outcome_expr) &&
      deparse1(outcome_expr[[1L]]) %in% c("Surv", "survival::Surv")
    if (!is_surv_call) {
      spicy_abort(
        c(paste0("`method = \"coxph\"` needs a survival outcome: ",
                 "`outcome = Surv(time, status)`."),
          "i" = sprintf("Got `%s`.", deparse1(outcome_expr))),
        class = "spicy_invalid_input"
      )
    }
    outcome_vars <- intersect(all.vars(outcome_expr), names(data))
    if (length(outcome_vars) == 0L) {
      spicy_abort(
        "The `Surv()` outcome references no column of `data`.",
        class = "spicy_invalid_input"
      )
    }
    outcome_name <- deparse1(outcome_expr)
  } else {
    outcome_name <- resolve_single_column_selection(
      rlang::enquo(outcome), data, "outcome"
    )
    outcome_vars <- outcome_name
  }
  pred_pos <- tidyselect::eval_select(rlang::enquo(predictors), data)
  pred_names <- setdiff(names(pred_pos), outcome_vars)
  if (length(pred_names) == 0L) {
    spicy_abort(
      c("`predictors` selected no columns (besides the outcome).",
        "i" = "Pass at least one predictor, e.g. `predictors = c(age, sex)`."),
      class = "spicy_invalid_input"
    )
  }

  dots <- list(...)
  # Cluster contract for the screen: ONE value per row of `data`. The
  # single-model contract (length = the fit's estimation sample) cannot
  # be satisfied by one vector when the univariable Ns differ, so the
  # screen aligns the vector itself: per fit via na.action inside the
  # bundle's frame method, and below for the multivariable fit.
  # The user-facing column name is captured HERE (the expression is
  # evaluated before table_regression() sees it) and stamped as an
  # attribute after every subset, so the footer can say "clusters by
  # region" instead of "cluster vector supplied".
  cluster_name <- extract_arg_column_name(
    match.call(expand.dots = TRUE)$cluster
  )
  if (!is.null(dots$cluster)) {
    if (!is.atomic(dots$cluster)) {
      spicy_abort(
        c("`cluster` must be a single vector for a univariable screen.",
          "i" = paste0("Supply one value per row of `data`; per-model ",
                       "cluster lists are not meaningful here.")),
        class = "spicy_invalid_input"
      )
    }
    if (length(dots$cluster) != nrow(data)) {
      spicy_abort(
        sprintf(
          "`cluster` must have one value per row of `data` (%d), not %d.",
          nrow(data), length(dots$cluster)
        ),
        class = "spicy_invalid_input"
      )
    }
  }
  want_estimands <- any(c("rmst", "rmst_se", "rmst_ci", "rmst_p",
                          "risk_diff", "risk_diff_se", "risk_diff_ci",
                          "risk_diff_p") %in% show_columns)
  if (want_estimands && !identical(method, "coxph")) {
    spicy_abort(
      c(paste0("RMST / risk-difference columns need a survival screen: ",
               "`method = \"coxph\"`."),
        "i" = paste0("For lm / glm screens, use the AME family ",
                     "instead.")),
      class = "spicy_invalid_input"
    )
  }
  if (want_estimands && identical(dots$tau, "minmax")) {
    spicy_abort(
      c(paste0("`tau = \"minmax\"` is not available in the univariable ",
               "screen."),
        "i" = paste0("Each univariable fit would resolve its own ",
                     "horizon, making the dRMST column incomparable ",
                     "across predictors. Give one shared numeric ",
                     "`tau`.")),
      class = "spicy_invalid_input"
    )
  }
  if (isTRUE(dots$nested)) {
    spicy_abort(
      c("`nested = TRUE` is not meaningful for a univariable screen.",
        "i" = paste0("The univariable models are not nested in one ",
                     "another; compare the multivariable model to a ",
                     "reduced fit with `table_regression(list(m1, m2), ",
                     "nested = TRUE)` instead.")),
      class = "spicy_invalid_input"
    )
  }
  # gtsummary convention (tbl_regression: intercept = FALSE default).
  if (is.null(dots$show_intercept)) dots$show_intercept <- FALSE

  if (isTRUE(complete_cases)) {
    cc <- stats::complete.cases(data[, c(outcome_vars, pred_names)])
    data <- data[cc, , drop = FALSE]
    if (!is.null(dots$cluster)) dots$cluster <- dots$cluster[cc]
  }

  bt <- function(x) paste0("`", x, "`")
  # The formula LHS: a backticked column name (lm/glm) or the verbatim
  # Surv() expression (coxph).
  response_str <- if (identical(method, "coxph")) {
    outcome_name
  } else {
    bt(outcome_name)
  }
  fit_one <- function(rhs_names) {
    f <- stats::reformulate(bt(rhs_names), response = response_str)
    environment(f) <- environment()
    switch(method,
      lm    = stats::lm(f, data = data),
      coxph = survival::coxph(f, data = data),
      stats::glm(f, data = data, family = family)
    )
  }
  fits <- vector("list", length(pred_names))
  for (k in seq_along(pred_names)) {
    fits[[k]] <- tryCatch(
      fit_one(pred_names[k]),
      error = function(e) {
        spicy_abort(
          c(sprintf("The univariable model for `%s` failed to fit.",
                    pred_names[k]),
            "x" = conditionMessage(e)),
          class = "spicy_invalid_data"
        )
      }
    )
  }
  names(fits) <- pred_names

  bundle <- structure(
    list(
      fits           = fits,
      outcome        = outcome_name,
      predictors     = pred_names,
      complete_cases = isTRUE(complete_cases),
      n_data         = nrow(data)
    ),
    class = "spicy_uv_screen"
  )

  models <- list(Univariable = bundle)
  if (isTRUE(multivariable)) {
    fit_multi <- tryCatch(
      fit_one(pred_names),
      error = function(e) {
        spicy_abort(
          c("The multivariable model failed to fit.",
            "x" = conditionMessage(e)),
          class = "spicy_invalid_data"
        )
      }
    )
    models$Multivariable <- fit_multi
    # The multivariable fit goes through the ordinary single-model path,
    # whose contract wants one cluster value per row of ITS estimation
    # sample: subset the row-per-`data` vector by the fit's na.action.
    if (!is.null(dots$cluster)) {
      cl_multi <- dots$cluster
      om <- stats::na.action(fit_multi)
      if (!is.null(om)) cl_multi <- cl_multi[-om]
      dots$cluster <- list(dots$cluster, cl_multi)
    }
  }
  # Stamp the captured column name (subsetting stripped attributes).
  if (!is.null(dots$cluster) && !is.na(cluster_name)) {
    if (is.list(dots$cluster)) {
      dots$cluster <- lapply(dots$cluster, function(x) {
        attr(x, "spicy_cluster_name") <- cluster_name
        x
      })
    } else {
      attr(dots$cluster, "spicy_cluster_name") <- cluster_name
    }
  }

  if (is.null(title)) {
    type <- if (identical(method, "lm")) {
      "linear"
    } else if (identical(method, "coxph")) {
      "Cox"
    } else {
      switch(paste(family$family, family$link),
        "binomial logit"  = "logistic",
        "binomial probit" = "probit",
        "poisson log"     = "Poisson",
        family$family
      )
    }
    title <- if (isTRUE(multivariable)) {
      sprintf("Univariable and multivariable %s regression: %s",
              type, outcome_name)
    } else {
      sprintf("Univariable %s regression screen: %s",
              type, outcome_name)
    }
  }

  do.call(
    table_regression,
    c(list(models, show_columns = show_columns, title = title), dots)
  )
}


# Per-fit sample size for the screen's N column. stats::nobs() equals
# the estimation-sample size for lm/glm, but for coxph it returns the
# EVENT count under censoring (the same quirk .expected_cluster_length
# documents); fit$n is the subject count there.
.uv_fit_n <- function(fit) {
  if (inherits(fit, "coxph") && !is.null(fit$n)) {
    return(as.integer(fit$n[length(fit$n)]))
  }
  as.integer(stats::nobs(fit))
}


# The label validator (validate_predictor_labels) reads term labels
# off every model via stats::terms(); the screen's terms are simply
# outcome ~ all screened predictors.
#' @export
terms.spicy_uv_screen <- function(x, ...) {
  bt <- function(v) paste0("`", v, "`")
  stats::terms(stats::reformulate(bt(x$predictors),
                                  response = bt(x$outcome)))
}


#' @export
as_regression_frame.spicy_uv_screen <- function(fit,
                                                model_id = "M1",
                                                vcov = "classical",
                                                cluster = NULL,
                                                boot_n = 1000L,
                                                ci_level = 0.95,
                                                ci_method = "wald",
                                                standardized = "none",
                                                exponentiate = FALSE,
                                                show_columns = c("b", "se",
                                                                 "ci", "p"),
                                                show_fit_stats = NULL,
                                                use_ame_satterthwaite = FALSE,
                                                cluster_name = NULL,
                                                re_ci = "wald",
                                                tau = NULL,
                                                at_time = NULL,
                                                ...) {
  bundle <- fit
  blocks <- list()
  ns <- integer(0)
  base_info <- NULL
  any_singular <- FALSE
  singular_terms <- character(0)

  for (k in seq_along(bundle$fits)) {
    pred <- bundle$predictors[k]
    # The screen-level cluster vector has one value per row of the data;
    # each univariable fit wants one per row of its OWN estimation sample.
    cluster_k <- cluster
    if (!is.null(cluster_k) && is.atomic(cluster_k)) {
      om <- stats::na.action(bundle$fits[[k]])
      if (!is.null(om) &&
            length(cluster_k) != .uv_fit_n(bundle$fits[[k]])) {
        cluster_k <- cluster_k[-om]
      }
    }
    fr <- as_regression_frame(
      bundle$fits[[k]],
      model_id              = model_id,
      vcov                  = vcov,
      cluster               = cluster_k,
      boot_n                = boot_n,
      ci_level              = ci_level,
      ci_method             = ci_method,
      standardized          = "none",
      exponentiate          = exponentiate,
      show_columns          = setdiff(show_columns, "n"),
      show_fit_stats        = show_fit_stats,
      use_ame_satterthwaite = FALSE,
      cluster_name          = cluster_name,
      tau                   = tau,
      at_time               = at_time
    )
    if (is.null(base_info)) base_info <- fr$info
    # Per-fit flags must be pooled, not read off the first fit: any
    # rank-deficient univariable model keeps its footer disclosure.
    if (isTRUE(fr$info$extras$has_singular)) {
      any_singular <- TRUE
      singular_terms <- c(singular_terms,
                          fr$info$extras$singular_terms)
    }
    cf <- fr$coefs
    block <- cf[cf$parent_var == pred, , drop = FALSE]
    # nocov start -- defensive: lm/glm keep rank-deficient terms as
    # em-dash rows (dropped coefficients stay in the frame), so no
    # known input yields an empty block today; guards future engines.
    if (nrow(block) == 0L) {
      spicy_warn(
        sprintf(paste0("Univariable screen: predictor `%s` produced no ",
                       "estimable coefficient and was dropped."), pred),
        class = "spicy_caveat"
      )
      next
    }
    # nocov end
    n_k <- .uv_fit_n(bundle$fits[[k]])
    block$n_obs <- NA_real_
    block$n_obs[1L] <- as.numeric(n_k)
    ns <- c(ns, n_k)
    blocks[[length(blocks) + 1L]] <- block
    # Pool the estimand disclosure across the per-predictor bootstraps:
    # one shared horizon, the most pessimistic replicate count.
    es_k <- fr$info$extras$survival_estimands
    if (!is.null(es_k)) {
      es_pool <- base_info$extras$survival_estimands %||% es_k
      es_pool$boot_valid <- min(es_pool$boot_valid, es_k$boot_valid)
      es_pool$stratified <- isTRUE(es_pool$stratified) ||
        isTRUE(es_k$stratified)
      base_info$extras$survival_estimands <- es_pool
    }
  }
  if (length(blocks) == 0L) {
    # nocov start -- defensive: reachable only through the empty-block
    # path above, itself unreachable for lm/glm (see comment there).
    spicy_abort(
      "Univariable screen: no predictor produced an estimable coefficient.",
      class = "spicy_invalid_data"
    )
    # nocov end
  }
  coefs <- do.call(rbind, blocks)
  rownames(coefs) <- NULL

  info <- base_info
  info$class <- "uv_screen"
  info$n_obs <- max(ns)
  info$extras$has_singular   <- any_singular
  info$extras$singular_terms <- unique(singular_terms)
  # The composite is not class "glm", so the footer's vcov theme would
  # fall back to the frame's raw vcov_label and print a different
  # string than the multivariable glm's ("Model-based (asymptotic)"
  # vs "classical (Fisher information)"), splitting the note into
  # per-model lines. Resolve the label the same way the footer would
  # for the underlying fits, so the two groups dedupe to one line.
  info$vcov_label <- format_vcov_label_from_frame(
    list(info = base_info)
  )
  # Model-level statistics are undefined for a screen (one fit per
  # block): blank them so the fit-stat rows print nothing under the
  # Univariable group -- the per-predictor N column carries the sample
  # information instead.
  info$fit_stats <- .blank_fit_stats(info$fit_stats)
  # Disclosure line (footer theme below). Silence when nothing needs
  # saying: equal Ns without complete_cases is self-evident.
  info$extras$uv_disclosure <- if (isTRUE(bundle$complete_cases)) {
    sprintf(
      "All models fit on the %d common complete cases.",
      bundle$n_data
    )
  } else if (length(unique(ns)) > 1L) {
    sprintf(
      paste0("Each univariable model is fit on its own complete ",
             "cases; N varies by predictor (%d-%d)."),
      min(ns), max(ns)
    )
  } else {
    NULL
  }
  info$supports$ame                 <- FALSE
  info$supports$partial_effect_size <- FALSE
  info$supports$classical_r2        <- FALSE
  info$supports$nested_lrt          <- FALSE
  info$supports$standardise_refit   <- FALSE

  new_regression_frame(coefs, info, bundle)
}


# Footer theme: the univariable-screen sample disclosure, read from
# extras$uv_disclosure. Same dedupe conventions as its siblings.
build_uv_disclosure_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  notes <- vapply(frames, function(f) {
    as.character(f$info$extras$uv_disclosure %||% NA_character_)
  }, character(1))
  if (all(is.na(notes))) return(NULL)
  affected <- which(!is.na(notes))
  if (length(unique(notes[affected])) == 1L) {
    return(notes[affected][1L])
  }
  # nocov start -- a table carries at most one uv_screen frame today;
  # kept for symmetry with the sibling reference-note builders.
  per <- vapply(affected, function(k) {
    sprintf("Model %d: %s", k, notes[k])
  }, character(1))
  paste(per, collapse = "\n")
  # nocov end
}