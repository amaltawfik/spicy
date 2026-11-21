# ---------------------------------------------------------------------------
# The generic fallback of table_regression(): `fallback = "broom"` or
# `fallback = "parameters"`.
#
# A class without an as_regression_frame() method is refused by default
# (classify_unsupported_lm_class()). Under `fallback` its rows are what a
# tidier returns: one coefficient per row under the tidier's term name, no
# factor structure, the numbers as the tidier gives them. spicy does not
# know how they were computed, so the note names the tidier in place of
# the estimator, and every request that needs to know the model (a robust
# variance, AME, standardization, nested tests, exponentiate, a fit
# statistic other than n, AIC, BIC and the log-likelihood) is refused.
# ---------------------------------------------------------------------------

.FALLBACK_TIDIERS <- c(
  broom = "broom::tidy()",
  parameters = "parameters::model_parameters()"
)

# The columns of parameters::model_parameters() under the names broom
# gives them; the frame is built from the broom names.
.FALLBACK_BROOM_NAMES <- c(
  Parameter = "term",
  Coefficient = "estimate",
  SE = "std.error",
  CI_low = "conf.low",
  CI_high = "conf.high",
  p = "p.value",
  df_error = "df",
  Effects = "effect",
  Component = "component",
  Response = "response",
  Group = "group"
)

# The fit statistics a fallback frame reads.
.FALLBACK_FIT_STATS <- c("nobs", "aic", "bic", "log_lik")

.check_fallback_arg <- function(fallback, call = rlang::caller_env()) {
  # isTRUE(): one value, a name of a tidier (not NA, not TRUE).
  if (!is.null(fallback) && !isTRUE(fallback %in% names(.FALLBACK_TIDIERS))) {
    spicy_abort(
      "`fallback` must be NULL, \"broom\" or \"parameters\".",
      class = "spicy_invalid_input",
      call = call
    )
  }
}

# TRUE for a fit no frame method reads: the model `fallback` builds.
.needs_fallback <- function(fit) {
  !is.null(fit) && !is.data.frame(fit) && !.has_as_regression_frame_method(fit)
}

# The line of the refusal of an unknown class that names the way out.
.fallback_hint <- function() {
  paste0(
    "`fallback = \"broom\"` or `fallback = \"parameters\"` builds the rows ",
    "from what `broom::tidy()` or `parameters::model_parameters()` returns."
  )
}

# The requests a fallback model refuses, before any frame is built: each
# needs to know how the model was fitted. A `vcov` other than "classical"
# is refused by the variance gate already, `standardized` by the gate of
# the frames loop. Called twice by table_regression(): `nested` and
# `cluster` before they are resolved, the others once the tokens are.
.fallback_refusals <- function(
  models,
  fallback,
  panel = NULL,
  nested = FALSE,
  cluster = NULL,
  exponentiate = FALSE,
  show_columns = character(0),
  fit_stats = NULL
) {
  fb <- vapply(models, .needs_fallback, logical(1))
  if (is.null(fallback) || !any(fb)) {
    return(invisible(NULL))
  }
  call <- rlang::caller_env()
  refuse <- function(what) {
    spicy_abort(
      c(
        sprintf(
          "%s is not available for %s.",
          what,
          .reg_model_tag(models, which(fb)[1L], panel)
        ),
        "i" = sprintf(
          "Its rows come from `%s`, and spicy does not know how the model was fitted.",
          .FALLBACK_TIDIERS[[fallback]]
        )
      ),
      class = "spicy_unsupported",
      call = call
    )
  }
  ame <- intersect(show_columns, .AME_COLUMN_TOKENS)
  if (length(ame) > 0L) {
    refuse(sprintf("The AME column `\"%s\"`", ame[1L]))
  }
  if (isTRUE(nested)) {
    refuse("`nested = TRUE`")
  }
  cl <- if (is.list(cluster)) cluster[fb] else list(cluster)
  if (!all(vapply(cl, is.null, logical(1)))) {
    refuse("`cluster`")
  }
  if (isTRUE(exponentiate)) {
    refuse("`exponentiate = TRUE`")
  }
  other <- setdiff(fit_stats, .FALLBACK_FIT_STATS)
  if (length(other) > 0L && all(fb)) {
    refuse(sprintf("The fit statistic `\"%s\"`", other[1L]))
  }
}

# The frame of a fit read by its tidier.
.fallback_frame <- function(fit, fallback, ci_level = 0.95, ...) {
  label <- .FALLBACK_TIDIERS[[fallback]]
  fail <- function(msg) {
    spicy_abort(
      c(
        sprintf("`%s` could not read this `%s` fit.", label, class(fit)[1L]),
        "x" = msg
      ),
      class = "spicy_unsupported"
    )
  }
  if (!requireNamespace(fallback, quietly = TRUE)) {
    # nocov start -- both tidiers are installed wherever the tests run.
    spicy_abort(
      sprintf(
        "`fallback = \"%s\"` requires the package %s.",
        fallback,
        fallback
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  td <- tryCatch(
    as.data.frame(
      if (identical(fallback, "broom")) {
        broom::tidy(fit, conf.int = TRUE, conf.level = ci_level)
      } else {
        parameters::model_parameters(fit, ci = ci_level)
      }
    ),
    error = function(e) fail(conditionMessage(e))
  )
  # parameters names its statistic by its test: "t" or "z".
  test <- intersect(c("t", "z"), names(td))[1L]
  if (!is.na(test)) {
    names(td)[names(td) == test] <- "statistic"
  }
  hit <- names(td) %in% names(.FALLBACK_BROOM_NAMES)
  names(td)[hit] <- .FALLBACK_BROOM_NAMES[names(td)[hit]]
  if (!all(c("term", "estimate") %in% names(td))) {
    fail("Its output has no `term` and `estimate` columns.")
  }
  # Random-effect rows are not shown; the note says so.
  eff <- td[["effect"]] %||% "fixed"
  re <- !is.na(eff) & eff != "fixed"
  td <- td[!re, , drop = FALSE]
  term <- as.character(td[["term"]])
  for (s in rev(intersect(
    c("component", "response", "y.level", "group"),
    names(td)
  ))) {
    v <- as.character(td[[s]])
    if (length(unique(v)) > 1L) {
      term <- ifelse(is.na(v), term, paste0(v, ": ", term))
    }
  }
  if (anyDuplicated(term)) {
    fail(sprintf(
      "It returns the term `%s` more than once.",
      term[duplicated(term)][1L]
    ))
  }
  n <- length(term)
  num <- function(col) as.numeric(td[[col]] %||% rep(NA_real_, n))
  coefs <- data.frame(
    term = term,
    parent_var = term,
    label = term,
    factor_level_pos = rep(NA_integer_, n),
    is_ref = rep(FALSE, n),
    estimate_type = rep("B", n),
    estimate = num("estimate"),
    std_error = num("std.error"),
    ci_lower = num("conf.low"),
    ci_upper = num("conf.high"),
    statistic = num("statistic"),
    df = num("df"),
    p_value = num("p.value"),
    test_type = rep(test, n),
    stringsAsFactors = FALSE
  )
  num_or_na <- function(f) {
    x <- tryCatch(suppressWarnings(as.numeric(f(fit))), error = function(e) {
      NA_real_
    })
    if (length(x) == 1L && is.finite(x)) x else NA_real_
  }
  n_obs <- num_or_na(stats::nobs)
  if (is.na(n_obs) && requireNamespace("insight", quietly = TRUE)) {
    n_obs <- num_or_na(insight::n_obs)
  }
  if (is.na(n_obs)) {
    n_obs <- num_or_na(function(f) nrow(stats::model.frame(f)))
  }
  if (is.na(n_obs) || n_obs <= 0) {
    fail("Its number of observations could not be read.")
  }
  dv <- tryCatch(.fit_dv(fit), error = function(e) NULL)
  if (is.null(dv) && requireNamespace("insight", quietly = TRUE)) {
    dv <- tryCatch(insight::find_response(fit), error = function(e) NULL)
  }
  if (!(is.character(dv) && length(dv) == 1L && nzchar(dv))) {
    dv <- NA_character_
  }
  info <- list(
    class = class(fit)[1L],
    family = list(family = "unknown", link = "unknown"),
    dv = dv,
    dv_label = dv,
    n_obs = n_obs,
    n_groups = NULL,
    weights_kind = "none",
    random_effects = empty_random_effects(),
    fit_stats = list(
      nobs = n_obs,
      aic = num_or_na(stats::AIC),
      bic = num_or_na(stats::BIC),
      log_lik = num_or_na(stats::logLik)
    ),
    vcov_kind = "model",
    vcov_label = spicy_fmt("note_fallback_source", label),
    ci_level = as.numeric(ci_level),
    ci_method = "wald",
    supports = list(),
    extras = list(fallback = label, fallback_dropped_re = any(re))
  )
  new_regression_frame(coefs, info, fit)
}

# The sentence of the note of the fallback models, in place of their
# standard errors: where the numbers come from, and that the random-effect
# rows are not shown.
.fallback_note <- function(frames) {
  fb <- which(vapply(
    frames,
    function(f) !is.null(f$info$extras$fallback),
    logical(1)
  ))
  if (length(fb) == 0L) {
    return(NULL)
  }
  texts <- vapply(
    frames[fb],
    function(f) {
      paste(
        c(
          f$info$vcov_label,
          if (f$info$extras$fallback_dropped_re) {
            spicy_str("note_fallback_re_dropped")
          }
        ),
        collapse = " "
      )
    },
    character(1)
  )
  .note_lines_by_model(frames, fb, texts)
}
