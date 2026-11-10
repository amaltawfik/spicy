# ---------------------------------------------------------------------------
# Phase 6b: as_regression_frame() method for fixest fits.
#
# fixest::feols (OLS), feglm (general family), fepois, femlm, fenegbin,
# ... all share class "fixest" -- the family slot disambiguates them.
#
# Key quirks vs lm/glm:
#   * NO (Intercept) row in coef when fixed effects absorb it; the
#     fixed-effect grouping sizes are stashed in info$extras$fixef_sizes
#     and info$n_groups. A fit without fixed effects keeps its intercept.
#   * stats::model.frame(fit) returns an empty list. Polymorphic
#     accessor .spicy_get_xlevels() routes through
#     eval(fit$call$data, envir = fit$call_env) instead.
#   * stats::family(fit) errors for feols (OLS-only); for feglm /
#     fepois it returns the family object.
#   * Inference comes from summary(fit)$coeftable. For OLS the columns
#     are "Estimate / Std. Error / t value / Pr(>|t|)" (Wald-t on
#     degrees_freedom(fit, "t")). For GLM-like fits the columns are
#     "z value / Pr(>|z|)" (Wald z-asymptotic), except the families whose
#     dispersion fixest estimates (gaussian, Gamma, quasi-*), which
#     report t.
#   * attr(coeftable, "vcov_type") carries a human label like "IID",
#     "Clustered (Month)", "Newey-West", etc. -- surfaced directly in
#     info$vcov_label.
#   * fitstat(fit, type = c("r2", "ar2", "wr2")) gives the standard /
#     adjusted / within R^2. Default for OLS includes within R^2 which
#     is the FE-partialled-out variant.
# ---------------------------------------------------------------------------

# A fixest model with fixed effects only (`y ~ 1 | f`) has no coefficient,
# so no row to show (and vcov() is NULL): refuse it as table_regression()
# refuses any model with no row to show. Called before the frames are
# built, so the message names the model as the other refusals do
# ("Model 2", or the name the user gave it, even "M2").
.refuse_fixest_fe_only <- function(models, panel = NULL) {
  fe_only <- vapply(
    models,
    function(m) inherits(m, "fixest") && length(stats::coef(m)) == 0L,
    logical(1)
  )
  if (!any(fe_only)) {
    return(invisible(NULL))
  }
  # A panel of a stacked table is gathered with the others by its driver.
  msg <- function(it) {
    nm <- it$names
    c(
      sprintf(
        "%s %s no rows to show: %s fixed effects only.",
        paste(nm, collapse = ", "),
        if (length(nm) > 1L) "have" else "has",
        if (length(nm) > 1L) "they have" else "it has"
      ),
      "i" = paste0(
        "fixest absorbs fixed effects instead of reporting them as ",
        "coefficients."
      ),
      "i" = "Add a predictor before the `|` of the formula."
    )
  }
  items <- list(names = .model_display_names(models, panel)[fe_only])
  spicy_abort(
    msg(items),
    class = "spicy_empty_table",
    spicy_gather = .reg_gather(panel, "fe_only", items, msg)
  )
}


#' `as_regression_frame()` method for `fixest` fits.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.fixest <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  cluster = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  model_id = "M1",
  ...
) {
  .check_fixest_available()

  # Family disambiguation. feglm / fepois store fit$family as a glm-family
  # *list* (gaussian / binomial / poisson / Gamma / ...). femlm and
  # fenegbin instead store a character string: "poisson", "logit",
  # "gaussian" or "negbin" (fenegbin is "negbin", with the dispersion
  # theta in fit$theta). Every one of these is a maximum-likelihood fit
  # with no classical R^2, and the femlm / fenegbin coeftable reports
  # asymptotic Wald z ("z value" / "Pr(>|z|)", df = Inf) for all four
  # strings -- so they share the GLM-like processing path. feols (OLS) is
  # the only remaining case: fit$family is NULL and inference is Wald-t.
  is_glm <- is.character(fit$family) || is.list(fit$family)
  coefs <- .fixest_coefs(fit, ci_level = ci_level, is_glm = is_glm)
  # AME rows when requested (finding M2): response-scale avg_slopes().
  # `vcov` is always the fit's own estimator here -- the variance comes
  # from fixest's own vcov interface (whatever summary(fit)$coeftable
  # reports; "IID" unless the user asked for something else at
  # estimation or in summary()), and spicy's HC* / CR* / resampling
  # tokens are refused up front by .robust_vcov_support(), which grants
  # fixest "classical" only. A GLM with fixed effects has no AME (see
  # .fixest_ame_ok()), so none is attempted. The AME rows take the
  # distribution of the B rows: t on fixest's degrees of freedom when
  # fixest reports t, z otherwise.
  if (.fixest_ame_ok(fit, is_glm)) {
    b <- coefs[!coefs$is_ref, , drop = FALSE]
    coefs <- .attach_ame_to_frame_coefs(
      coefs,
      fit,
      ci_level,
      show_columns,
      vcov_type = vcov,
      cluster = cluster,
      df = if (all(b$test_type == "t")) b$df[1L] else Inf
    )
    coefs <- .fixest_align_ame_rows(coefs, fit)
  }
  info <- .fixest_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    is_glm = is_glm
  )

  new_regression_frame(coefs, info, fit)
}


# ---- Internal helpers -----------------------------------------------------

.check_fixest_available <- function() {
  if (!spicy_pkg_available("fixest")) {
    # nocov start: defensive Suggests guard; fixest is installed in the
    # test/CI environment, so the abort branch is never taken.
    spicy_abort(
      c(
        "Cannot extract a regression frame from a fixest fit without `fixest`.",
        "i" = "Install fixest: `install.packages(\"fixest\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


# Build the coefs tibble. Reads inference from summary(fit)$coeftable.
.fixest_coefs <- function(fit, ci_level, is_glm) {
  cf <- stats::coef(fit)
  est <- unname(cf)
  nm <- names(cf)
  V <- as.matrix(stats::vcov(fit))
  se <- sqrt(diag(V))[nm]

  sm <- summary(fit)$coeftable
  # fixest reports a t statistic for OLS and for the GLM families whose
  # dispersion it estimates (gaussian, Gamma, quasi-*), a z statistic
  # otherwise: follow the column it reports, as summary.glm() is followed.
  if (is_glm && "z value" %in% colnames(sm)) {
    # GLM-like fixest: Wald z-asymptotic.
    stat <- unname(sm[nm, "z value"])
    p_value <- unname(sm[nm, "Pr(>|z|)"])
    df <- rep(Inf, length(est))
    z_crit <- stats::qnorm(0.5 + ci_level / 2)
    ci_lower <- est - z_crit * se
    ci_upper <- est + z_crit * se
    test_type_col <- rep("z", length(est))
  } else {
    # OLS, or a GLM family with an estimated dispersion: Wald-t on the
    # degrees of freedom of fixest's own p-values and confint(). They
    # equal df.residual under iid errors and are far fewer under
    # clustered ones (by default the number of clusters minus one), so
    # the interval must take them from fixest too.
    stat <- unname(sm[nm, "t value"])
    p_value <- unname(sm[nm, "Pr(>|t|)"])
    dfr <- fixest::degrees_freedom(fit, "t")
    # nocov: defensive; degrees_freedom() is NA only when fixest could not
    # compute the variance, whose standard errors are then NA too.
    if (is.null(dfr) || !is.finite(dfr)) {
      dfr <- Inf # nocov
    }
    df <- rep(as.numeric(dfr), length(est))
    t_crit <- stats::qt(0.5 + ci_level / 2, df = dfr)
    ci_lower <- est - t_crit * se
    ci_upper <- est + t_crit * se
    test_type_col <- rep("t", length(est))
  }

  factor_meta <- detect_factor_term_meta(fit)
  ft <- vapply(
    nm,
    function(n) factor_meta[[n]]$factor_term %||% NA_character_,
    character(1)
  )
  lvl <- vapply(
    nm,
    function(n) factor_meta[[n]]$factor_level %||% NA_character_,
    character(1)
  )
  pos <- vapply(
    nm,
    function(n) factor_meta[[n]]$factor_level_pos %||% NA_integer_,
    integer(1)
  )

  parent_var <- ifelse(is.na(ft), nm, ft)
  label <- ifelse(is.na(lvl), nm, lvl)

  coefs <- data.frame(
    term = nm,
    parent_var = parent_var,
    label = label,
    factor_level_pos = as.integer(pos),
    is_ref = rep(FALSE, length(nm)),
    estimate_type = rep("B", length(nm)),
    estimate = est,
    std_error = se,
    df = as.numeric(df),
    statistic = stat,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    test_type = test_type_col,
    stringsAsFactors = FALSE
  )

  ref_rows <- .fixest_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


# Reference-row synthesis. detect_factor_terms() routes through the
# polymorphic .spicy_get_xlevels() which has a fixest branch (sourced
# from fit$call$data via fit$call_env), so factor predictors are seen.
.fixest_reference_rows <- function(fit) {
  fts <- detect_factor_terms(fit)
  if (length(fts) == 0L) {
    return(.empty_coefs_frame())
  }
  rows <- list()
  for (ft in fts) {
    if (!isTRUE(ft$reference_dropped)) {
      next
    }
    ref_lvl <- ft$reference_level
    term_name <- paste0(ft$factor_term, ref_lvl)
    ref_pos <- match(ref_lvl, ft$levels) %||% NA_integer_
    rows[[length(rows) + 1L]] <- data.frame(
      term = term_name,
      parent_var = ft$factor_term,
      label = ref_lvl,
      factor_level_pos = as.integer(ref_pos),
      is_ref = TRUE,
      estimate_type = "B",
      estimate = NA_real_,
      std_error = NA_real_,
      df = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      test_type = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(.empty_coefs_frame())
  }
  do.call(rbind, rows)
}


# Family-info resolver. Three cases:
#   * feols (OLS): fit$family is NULL -> gaussian / identity.
#   * feglm / fepois: fit$family is a glm-family *list* carrying
#     $family ("binomial", "poisson", "Gamma", ...) and $link.
#   * femlm / fenegbin: fit$family is a character string with a fixed
#     link, named here as glm names it, so the glm title and
#     exponentiate rules apply: "poisson" (log), "logit" (the binomial
#     logit), "gaussian" (identity), "negbin" (theta in fit$theta; log,
#     MASS::glm.nb()'s convention in as_regression_frame.negbin()).
.fixest_family_info <- function(fit, is_glm) {
  if (!is_glm) {
    return(list(family = "gaussian", link = "identity"))
  }
  if (is.list(fit$family)) {
    return(list(family = fit$family$family, link = fit$family$link))
  }
  switch(
    fit$family[[1L]],
    poisson = list(family = "poisson", link = "log"),
    logit = list(family = "binomial", link = "logit"),
    gaussian = list(family = "gaussian", link = "identity"),
    list(family = "negbin", link = "log")
  )
}


# Grouping factors whose INTERCEPT is genuinely absorbed. fixef_sizes /
# fixef_vars include slope-only factors too (probed fixest 0.14.2:
# y ~ x | Origin[[Year]] + Product lists Origin although no Origin
# intercept is absorbed), so a Yes/No disclosure keyed on them would
# lie. fixef_terms disambiguates: NULL means no varying slopes (every
# fixef_vars entry is an intercept); otherwise its BARE entries are the
# absorbed intercepts and the `var[[slope]]` entries are slopes.
.fixest_intercept_fixefs <- function(fit) {
  fv <- fit$fixef_vars
  if (is.null(fv) || length(fv) == 0L) {
    return(character(0))
  }
  ft <- fit$fixef_terms
  if (is.null(ft)) {
    return(fv)
  }
  intersect(fv, ft[!grepl("[[", ft, fixed = TRUE)])
}


# Build the info list for a fixest fit.
.fixest_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id,
  is_glm
) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  fam <- .fixest_family_info(fit, is_glm = is_glm)

  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  fit_stats <- .fixest_fit_stats(fit, is_glm = is_glm)

  # vcov label comes from attr(summary$coeftable, "vcov_type"):
  # "IID" / "Clustered (Month)" / "Newey-West" / "Conley" / etc.
  vt <- attr(summary(fit)$coeftable, "vcov_type") %||% "IID"
  default_label <- .fixest_vcov_label(vt)

  fixef_sizes <- fit$fixef_sizes %||% integer(0)
  n_groups <- if (length(fixef_sizes) > 0L) {
    setNames(as.integer(fixef_sizes), names(fixef_sizes))
  } else {
    NULL
  }

  exp_ok <- is_glm && !identical(fam$link, "identity")

  supports <- list(
    ame = .fixest_ame_ok(fit, is_glm),
    partial_effect_size = FALSE,
    classical_r2 = !is_glm && !.fixest_is_iv_second_stage(fit),
    nested_lrt = TRUE,
    exponentiate = exp_ok,
    standardise_refit = FALSE
  )

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = FALSE,
    weighted_n = NA_real_,
    title_prefix = .fixest_title_prefix(
      fam,
      is_glm,
      has_fe = length(fit$fixef_vars) > 0L,
      is_iv = .fixest_is_iv_second_stage(fit)
    ),
    exp_applied = FALSE,
    exp_header = NA_character_,
    fixef_sizes = fixef_sizes,
    # Factors with a genuinely absorbed intercept (slope-only factors
    # excluded) -- drives the "Fixed effects" Yes/No disclosure block.
    fixef_intercept = .fixest_intercept_fixefs(fit),
    vcov_type = vt,
    # fenegbin dispersion: theta is stored as a length-1 (named ".theta")
    # numeric on the fit; NA for non-negbin families. Mirrors the
    # extras$theta surfaced by as_regression_frame.negbin() (MASS path).
    theta = if (identical(fam$family, "negbin")) {
      as.numeric(fit$theta %||% NA_real_)
    } else {
      NA_real_
    }
  )

  list(
    class = "fixest",
    family = fam,
    dv = dv,
    dv_label = dv_label,
    n_obs = as.integer(stats::nobs(fit)),
    n_groups = n_groups,
    weights_kind = "none",
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||% default_label,
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# Fit-stats for fixest. OLS gets classical R^2 + within R^2 (the FE-
# partialled variant) via fitstat(); GLM-like gets the McFadden
# pseudo-R^2 (fixest's own "pr2": 1 - ll/ll0), matching the glm /
# ordinal / multinom default family. A negative binomial also gets its
# theta and alpha = 1/theta, the rows of MASS::glm.nb().
.fixest_fit_stats <- function(fit, is_glm) {
  r2 <- if (!is_glm) {
    tryCatch(
      fixest::fitstat(
        fit,
        type = c("r2", "ar2", "wr2"),
        verbose = FALSE,
        simplify = FALSE
      ),
      error = function(e) NULL
    )
  } else {
    NULL
  }
  if (!is_glm && .fixest_is_iv_second_stage(fit)) {
    # The R-squared of ivreg and iv_robust: 1 - RSS/TSS on the
    # structural residuals y - Xb (fit$ssr, weighted), where fitstat()
    # uses the second-stage residuals; adjusted as fixest adjusts it,
    # which is summary.ivreg()'s (n - 1) / (n - k) without fixed
    # effects. The within R-squared has no structural version: NA.
    n <- stats::nobs(fit)
    k0 <- length(fit$fixef_vars) > 0L ||
      "(Intercept)" %in% names(fit$coefficients)
    rr <- fit$ssr / fit$ssr_null
    r2 <- list(
      r2 = 1 - rr,
      ar2 = 1 - rr * (n - k0) / (n - fit$nparams),
      wr2 = NA_real_
    )
  }
  pr2 <- if (is_glm) {
    tryCatch(
      as.numeric(fixest::fitstat(fit, "pr2", simplify = TRUE)),
      error = function(e) NA_real_
    )
  } else {
    NA_real_
  }
  sigma_val <- tryCatch(stats::sigma(fit), error = function(e) NA_real_)
  # femlm / fenegbin store the family as a string, feglm / fepois as a
  # glm family object, feols not at all.
  fam_name <- if (is.list(fit$family)) fit$family$family else fit$family
  if (is_glm) {
    sigma_val <- .fixest_glm_sigma(fit, fam_name, sigma_val)
  }
  theta <- if (identical(fam_name, "negbin")) {
    as.numeric(fit$theta %||% NA_real_)
  } else {
    NA_real_
  }
  ll <- .fixest_twin_loglik(fit)
  list(
    r_squared = as.numeric(r2$r2 %||% NA_real_),
    adj_r_squared = as.numeric(r2$ar2 %||% NA_real_),
    pseudo_r2_mcfadden = pr2,
    pseudo_r2 = if (!is_glm && !is.null(r2$wr2)) {
      list(within_r2 = as.numeric(r2$wr2))
    } else if (is_glm && is.finite(pr2)) {
      list(mcfadden = pr2)
    } else {
      NULL
    },
    aic = -2 * ll + 2 * attr(ll, "df"),
    bic = -2 * ll + log(stats::nobs(fit)) * attr(ll, "df"),
    log_lik = as.numeric(ll),
    deviance = tryCatch(
      suppressWarnings(stats::deviance(fit)),
      error = function(e) NA_real_
    ),
    sigma = sigma_val,
    theta = theta,
    alpha = 1 / theta,
    nobs = as.integer(stats::nobs(fit))
  )
}


# TRUE when the fit has an average marginal effect: feols, or a GLM
# (feglm, fepois, femlm, fenegbin) without fixed effects. marginaleffects
# refuses a GLM with fixed effects, whatever its family, the gaussian
# included: it "cannot take into account the uncertainty in fixed-effects
# parameters", which fixest does not estimate (checked with
# marginaleffects 1.0.0 on fepois, feglm, femlm and fenegbin). Declaring
# it lets table_regression() refuse the request with a reason instead of
# a column of dashes. A `lean = TRUE` fit with fixed effects is refused
# too: marginaleffects cannot predict from it (marginaleffects 1.0.0).
.fixest_ame_ok <- function(fit, is_glm) {
  length(fit$fixef_vars) == 0L || (!is_glm && !isTRUE(fit$lean))
}

# TRUE for the frame of a `lean = TRUE` fixest fit with fixed effects,
# so the AME refusal can name the remedy.
.fixest_lean_fe <- function(frame) {
  fit <- attr(frame, "fit")
  inherits(fit, "fixest") && isTRUE(fit$lean) && length(fit$fixef_vars) > 0L
}


# The AME rows of a fixest fit, set on its B rows. The shared builder
# reads the data, where a logical or a factor is a level row ("long",
# "TRUE"), while a B row may keep the coefficient's name ("longTRUE":
# a factor beside i(), whose formula has no xlevels): each AME row takes
# the parent, label and level position of the B row of its term, so a
# coefficient never prints twice. The second stage of an IV fit names
# an endogenous regressor "fit_<x>" where the AME names <x>. fixest
# drops a collinear coefficient (fit$collin.var) where lm keeps an NA,
# so the AME marginaleffects computes for it (a 0) is dropped too.
.fixest_align_ame_rows <- function(coefs, fit) {
  # marginaleffects can report the response of an IV fit whose
  # endogenous regressor is transformed (`log(x)`) as a regressor, with
  # a slope of 0: it is not one.
  coefs <- coefs[
    !(coefs$estimate_type == "ame" &
      coefs$term %in% all.vars(fit[["fml"]][[2L]])),
    ,
    drop = FALSE
  ]
  is_ame <- coefs$estimate_type == "ame"
  is_b <- coefs$estimate_type == "B"
  endo_fit <- as.character(fit[["iv_endo_names_fit"]])[
    match(coefs$term, fit[["iv_endo_names"]])
  ]
  hit <- is_ame & endo_fit %in% coefs$term[is_b]
  coefs$term[hit] <- endo_fit[hit]
  # i() names a coefficient "<x>::<level>": the AME row of that level,
  # contrasted with the same reference (.fixest_ame_newdata()), goes on
  # it. An AME row of an i() variable left without a coefficient row
  # (a second reference level, or the slope of a numeric variable that
  # i() enters as categories) has no reference the reader can see: it
  # is dropped, with a warning.
  key <- paste0(coefs$parent_var, "::", coefs$label)
  hit <- is_ame & !coefs$term %in% coefs$term[is_b] & key %in% coefs$term
  coefs$term[hit] <- key[hit]
  # marginaleffects names a transformed variable by the variable
  # (`rincome` for `log(rincome)`, `Month` with a level for
  # `factor(Month)6`): its AME goes on the one coefficient that
  # transforms that variable alone, as lm() places it.
  b_terms <- coefs$term[is_b]
  b_one <- b_terms[!grepl(":", b_terms, fixed = TRUE)]
  for (i in which(is_ame & !coefs$term %in% b_terms)) {
    v <- coefs$parent_var[i]
    lvl <- if (isTRUE(coefs$label[i] != v)) coefs$label[i] else ""
    pre <- b_one[endsWith(b_one, lvl)]
    pre <- substr(pre, 1L, nchar(pre) - nchar(lvl))
    one <- vapply(
      pre,
      function(p) {
        grepl(paste0("(", v, ")"), p, fixed = TRUE) &&
          identical(
            tryCatch(all.vars(str2lang(p)), error = function(e) NULL),
            v
          )
      },
      logical(1)
    )
    if (sum(one) == 1L) {
      coefs$term[i] <- paste0(pre[one], lvl)
    }
  }
  collin <- as.character(fit[["collin.var"]])
  # Every factor of a coefficient "<x>::<a>:<z>::<b>" is an i() variable.
  i_names <- function(x) {
    unique(unlist(regmatches(x, gregexpr("[^:]+(?=::)", x, perl = TRUE))))
  }
  i_vars <- i_names(b_terms)
  # A coefficient that fixest drops for collinearity, an i() one too
  # (named "<x>::<level>"), takes its AME with it.
  collin_vars <- setdiff(i_names(collin), i_vars)
  dropped <- is_ame &
    ((coefs$term %in% collin) |
      (key %in% collin) |
      (coefs$parent_var %in% collin_vars))
  orphan <- is_ame &
    !dropped &
    coefs$parent_var %in% i_vars &
    !coefs$term %in% b_terms
  # A level absent from the estimation sample (an unused factor level,
  # which marginaleffects still contrasts) is dropped without a word.
  d <- if (any(orphan)) .fixest_model_data(fit)
  absent <- orphan &
    coefs$label != coefs$parent_var &
    coefs$parent_var %in% names(d)
  for (i in which(absent)) {
    absent[i] <- !coefs$label[i] %in% as.character(d[[coefs$parent_var[i]]])
  }
  warn <- orphan & !absent
  if (any(warn)) {
    what <- ifelse(
      coefs$label == coefs$parent_var,
      paste0("`", coefs$parent_var, "`"),
      paste0("`", coefs$parent_var, "` (level ", coefs$label, ")")
    )
    # A variable that i() enters only in an interaction (`i(g, x)`,
    # `i(f1, i.f2)`) has no row of its own, whatever its level.
    main <- i_names(b_terms[!grepl("::[^:]+:", b_terms)])
    inter <- warn & !coefs$parent_var %in% main
    spicy_warn(
      c(
        paste0(
          "No AME shown for ",
          paste(unique(what[warn]), collapse = ", "),
          ": no coefficient of the table matches it."
        ),
        "i" = if (any(inter)) {
          paste0(
            "`i()` enters ",
            paste0("`", unique(coefs$parent_var[inter]), "`", collapse = ", "),
            " only in an interaction, which has no row for its AME. ",
            "Enter a variable as a factor outside `i()` to see its AME."
          )
        },
        "i" = if (any(warn & !inter)) {
          paste0(
            "`i()` enters it as categories: its AME is shown on the rows ",
            "of its coefficients, against their reference, and nowhere ",
            "else."
          )
        }
      ),
      class = "spicy_undefined_stat"
    )
  }
  m <- match(coefs$term, b_terms)
  hit <- is_ame & !is.na(m)
  for (col in c("parent_var", "label", "factor_level_pos")) {
    coefs[[col]][hit] <- coefs[[col]][is_b][m[hit]]
  }
  coefs[!dropped & !orphan, , drop = FALSE]
}


# The data on which marginaleffects computes the AME of a fit whose i()
# factors take a reference other than their first level (`i(x, ref =
# "H")`). marginaleffects contrasts a factor with its first level, the
# coefficients with the level i() leaves out: making that level the
# first one in the estimation sample gives the AME the reference of the
# coefficients, and leaves every prediction unchanged (i() codes the
# levels by value). NULL when no factor needs it, or when the sample
# cannot be read. A level that fixest drops for collinearity has a
# coefficient, if a dropped one: it is not the reference.
.fixest_ame_newdata <- function(fit) {
  b <- grep(
    "^[^:]+::[^:]+$",
    c(names(stats::coef(fit)), as.character(fit[["collin.var"]])),
    value = TRUE
  )
  d <- if (length(b) > 0L) .fixest_model_data(fit)
  if (!is.data.frame(d) || nrow(d) != stats::nobs(fit)) {
    return(NULL)
  }
  changed <- FALSE
  for (v in intersect(unique(sub("::.*$", "", b)), names(d))) {
    if (!is.factor(d[[v]]) && !is.character(d[[v]])) {
      next
    }
    lv <- .spicy_cat_levels(d[[v]])
    lv <- lv[lv %in% d[[v]]]
    ref <- setdiff(lv, sub("^.*::", "", b[startsWith(b, paste0(v, "::"))]))
    if (length(ref) == 1L && ref != lv[1L]) {
      d[[v]] <- factor(d[[v]], levels = c(ref, setdiff(lv, ref)))
      changed <- TRUE
    }
  }
  if (changed) d
}


# TRUE for the second stage of an IV fit, the model an IV feols reports.
# Its first stage (`summary(fit, stage = 1)`, iv_stage 1) is the OLS of
# the endogenous regressor on the instruments.
.fixest_is_iv_second_stage <- function(fit) {
  isTRUE(fit$is_iv) && !isTRUE(fit$iv_stage == 1)
}


# The data of a fixest fit: its estimation sample, or the original data
# when the fit no longer records which rows it used (`lean = TRUE`,
# where fixest_data()'s estimation sample errors). `sample = "original"`
# reads the original data directly, for a caller that needs only the
# column types: subsetting a large panel to its sample copies it. NULL
# when the data cannot be found at all. fixest looks the data of a lean
# fit up from its own namespace, where `d`, `l` and `f` are functions:
# the call's data is then read as .spicy_get_xlevels_base() reads it.
.fixest_model_data <- function(fit, sample = c("estimation", "original")) {
  d <- if (identical(match.arg(sample), "estimation")) {
    tryCatch(
      fixest::fixest_data(fit, sample = "estimation"),
      error = function(e) NULL
    )
  }
  if (!is.data.frame(d)) {
    d <- tryCatch(
      fixest::fixest_data(fit, sample = "original"),
      error = function(e) NULL
    )
  }
  if (!is.data.frame(d)) {
    d <- tryCatch(
      eval(fit$call$data, envir = fit$call_env %||% globalenv()),
      error = function(e) NULL
    )
  }
  if (is.data.frame(d)) d
}


# The feglm families whose dispersion is estimated, and counted.
.FIXEST_DISP_FAMS <- c("gaussian", "Gamma", "inverse.gaussian")

# The log-likelihood of a fixest fit and, as its "df", the parameters it
# counts, as the lm() or glm() twin of the model counts them; the fixed
# effects stay counted as fixest counts them (`fit$nparams`). fixest's
# own AIC() leaves out the residual variance of a feols, and the
# log-likelihood of a feglm whose dispersion is estimated (Gaussian,
# Gamma, inverse Gaussian) is not the one glm() reports: both are
# computed here as logLik.lm() and logLik.glm() compute them. A Gaussian
# femlm is the ML fit of the same model and counts its variance too. An
# IV feols, which no twin gives a likelihood, and the Poisson and
# binomial fits keep fixest's; a fenegbin counts its theta, as
# MASS::glm.nb() does. A lean fit whose weights are gone, or a lean
# Gamma or inverse-Gaussian feglm, has no log-likelihood; a lean
# Gaussian one is computed from its deviance.
.fixest_twin_loglik <- function(fit) {
  fam_name <- if (is.list(fit$family)) fit$family$family else fit$family
  k <- fit$nparams
  out <- function(ll, df) structure(as.numeric(ll), df = df)
  weighted <- !is.null(fit$call$weights)
  w <- fit$weights %||% if (!weighted) 1
  is_ols <- identical(fit$method_type, "feols")
  if (is_ols && !.fixest_is_iv_second_stage(fit)) {
    if (is.null(w)) {
      return(out(NA_real_, k + 1))
    }
    n <- stats::nobs(fit)
    w <- rep_len(w, n)
    ll <- 0.5 *
      (sum(log(w)) - n * (log(2 * pi) + 1 - log(n) + log(fit$ssr)))
    return(out(ll, k + 1))
  }
  if (
    !is_ols && is.list(fit$family) && isTRUE(fam_name %in% .FIXEST_DISP_FAMS)
  ) {
    if (is.null(w)) {
      return(out(NA_real_, k + 1))
    }
    if (identical(fam_name, "gaussian")) {
      # glm()'s Gaussian log-likelihood needs n, the deviance and the
      # weights only, which a lean fit keeps.
      n <- stats::nobs(fit)
      w <- rep_len(w, n)
      dev <- fit$deviance
      ll <- 0.5 * (sum(log(w)) - n * (log(2 * pi * dev / n) + 1))
      return(out(ll, k + 1))
    }
    mu <- fit$fitted.values
    if (is.null(mu)) {
      return(out(NA_real_, k + 1))
    }
    y <- mu + stats::residuals(fit, type = "response")
    aic <- fit$family$aic(
      y,
      rep(1, length(y)),
      mu,
      rep_len(w, length(y)),
      stats::deviance(fit)
    )
    return(out(1 - aic / 2, k + 1))
  }
  ll <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) {
    NA_real_
  })
  # A Gaussian femlm counts its variance, a fenegbin its theta, as
  # glm() and MASS::glm.nb() count them.
  out(ll, k + isTRUE(!is_ols && fam_name %in% c("gaussian", "negbin")))
}


# The sigma row of a maximum-likelihood fixest fit, by the glm rule: the
# square root of the dispersion, 1 when the family fixes it. feglm /
# fepois store glm's own dispersion in fit$dispersion (1 for poisson and
# binomial, the Pearson estimate otherwise); femlm stores none, so its
# poisson and logit take the fixed 1 and its gaussian the sigma() of the
# fit, the same square root. Blank for the negative binomial, whose
# dispersion is the theta row. fixest's own sigma() is the square root of
# the deviance over the residual degrees of freedom: the glm sigma for
# the gaussian family only, so the other families do not use it.
.fixest_glm_sigma <- function(fit, fam_name, sigma_fit) {
  if (identical(fam_name, "negbin")) {
    return(NA_real_)
  }
  disp <- fit$dispersion
  if (is.numeric(disp) && length(disp) == 1L && is.finite(disp)) {
    return(sqrt(as.numeric(disp)))
  }
  if (isTRUE(fam_name %in% c("poisson", "logit"))) {
    return(1)
  }
  if (identical(fam_name, "gaussian")) {
    return(as.numeric(sigma_fit))
  }
  NA_real_
}


# Title prefix: the glm title of the family and link -- so a probit
# feglm is not titled logistic, and no title carries R's family code
# ("Quasipoisson") -- plus "(fixed effects)" when the fit has any. A fit
# without them is titled as lm and glm title the same model, and an IV
# feols as ivreg titles two-stage least squares, the qualifier inside
# its parentheses.
.fixest_title_prefix <- function(fam, is_glm, has_fe, is_iv = FALSE) {
  if (is_iv) {
    return(
      if (has_fe) {
        "IV regression (2SLS, fixed effects)"
      } else {
        "IV regression (2SLS)"
      }
    )
  }
  base <- if (!is_glm) {
    "Linear regression"
  } else if (identical(fam$family, "negbin")) {
    "Negative-binomial regression"
  } else {
    spicy_glm_title_prefix(fam$family, fam$link)
  }
  if (has_fe) paste0(base, " (fixed effects)") else base
}


# vcov_label normalizer. fixest's "vcov_type" attribute already carries
# a near-renderer-ready label, in English: "IID" becomes the "Classical"
# of the other engines, for cross-engine consistency, and the labels of
# fixest's built-in estimators go through the registry so that they
# follow the table language. Their variables, lag and cutoff are kept as
# fixest gives them ("Clustered (id & period)", "Newey-West (L=2)",
# "Conley (100km)"). Any other label -- a function passed as `vcov`,
# which fixest names by its call -- stays verbatim.
# A cluster on combined fixed effects (`~id^period`) is named by an
# internal fixest call, "combine_fixef_keep_names(id, period)": it is
# written back as the user wrote it, "id^period".
.fixest_vcov_label <- function(vt) {
  if (identical(vt, "IID")) {
    return(spicy_str("note_vcov_classical_plain"))
  }
  m <- gregexpr("combine_fixef_keep_names\\([^()]*\\)", vt)
  regmatches(vt, m) <- lapply(regmatches(vt, m), function(s) {
    gsub(", ", "^", substr(s, 26L, nchar(s) - 1L), fixed = TRUE)
  })
  if (identical(vt, "Heteroskedasticity-robust")) {
    return(spicy_str("note_vcov_fixest_hetero"))
  }
  if (identical(vt, "Custom")) {
    return(spicy_str("note_vcov_fixest_custom"))
  }
  part <- function(re) regmatches(vt, regexec(re, vt))[[1L]]
  p <- part("^Clustered \\((.+)\\)$")
  if (length(p)) {
    return(spicy_fmt("note_vcov_fixest_cluster", p[2L]))
  }
  p <- part("^(Newey-West|Driscoll-Kraay) \\(L=([0-9]+)\\)$")
  if (length(p)) {
    return(spicy_fmt("note_vcov_fixest_lag", p[2L], p[3L]))
  }
  p <- part("^Conley \\(([0-9.]+)(km|mi)\\)$")
  if (length(p)) {
    return(spicy_fmt("note_vcov_fixest_conley", p[2L], p[3L]))
  }
  vt
}
