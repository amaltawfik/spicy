# Absolute survival estimands for coxph fits: RMST difference over
# [0, tau] and risk (cumulative incidence) difference at a landmark
# time, by regression standardization (g-computation) from the fitted
# model -- the survival analog of the AME column (AME itself is
# deliberately disabled for Cox: a hazard ratio has no marginal effect
# on the probability scale without a time horizon).
#
# Spec: dev/survival_estimands_spec.md (validated 2026-07-09).
# Engine (native, no runtime dependency):
#   1. H0(t) from survival::basehaz(fit, centered = FALSE);
#   2. per-subject counterfactual curves
#      S_i^a(t) = exp(-H0(t) * exp(lp_i^a)), lp on the reference =
#      "zero" scale so it matches the uncentered baseline;
#   3. standardized curve S^a(t) = mean_i S_i^a(t);
#   4. RMST^a = the exact step-function integral of S^a over [0, tau];
#      risk^a(t) = 1 - S^a(t).
# Contrasts: factor predictors get one row per non-reference level
# (level vs reference, the whole data pushed through both); numeric
# predictors get the +1-unit contrast (the AME convention).
# Inference: nonparametric bootstrap over subjects (refit + recompute
# per replicate); SE = sd of replicates, Wald z / CI / p -- the
# package's bootstrap layering.


# ---- Data recovery ---------------------------------------------------------

# The counterfactual push needs the fit's ORIGINAL data (raw columns,
# so factor()/log()/interaction terms re-evaluate through predict());
# the model frame's columns are named by expression and cannot be
# modified by variable. Recover from the call, then keep the rows the
# fit actually used.
.coxph_estimand_data <- function(fit) {
  data <- tryCatch(
    eval(fit$call$data, environment(stats::formula(fit))),
    error = function(e) NULL
  )
  if (!is.data.frame(data)) {
    spicy_abort(
      c(paste0("RMST / risk-difference columns need the fit's original ",
               "data, and it could not be recovered from the model call."),
        "i" = paste0("Fit the model with a `data` argument that is ",
                     "still available (a data frame in scope).")),
      class = "spicy_invalid_input"
    )
  }
  fvars <- all.vars(stats::formula(fit))
  missing_vars <- setdiff(fvars, names(data))
  if (length(missing_vars) > 0L) {
    spicy_abort(
      sprintf(
        "Column(s) %s of the model formula are not in the recovered data.",
        paste(shQuote(missing_vars), collapse = ", ")
      ),
      class = "spicy_invalid_input"
    )
  }
  data <- data[stats::complete.cases(data[, fvars, drop = FALSE]), ,
               drop = FALSE]
  # Guard against a silent mismatch between the recovered rows and the
  # fit (e.g. the data frame changed since fitting).
  if (nrow(data) != as.integer(fit$n[length(fit$n)])) {
    spicy_abort(
      c("The recovered data does not match the fitted sample.",
        "i" = sprintf("Recovered %d complete rows; the fit used %d.",
                      nrow(data), as.integer(fit$n[length(fit$n)]))),
      class = "spicy_invalid_input"
    )
  }
  data
}


# ---- Structural gates ------------------------------------------------------

.coxph_estimand_gates <- function(fit, model_id) {
  specials <- attr(fit$terms, "specials")
  if (!is.null(specials$tt)) {
    spicy_abort(
      sprintf("RMST / risk-difference columns are not available with time-transform `tt()` terms (%s).",
              model_id),
      class = "spicy_invalid_input"
    )
  }
  y <- stats::model.response(stats::model.frame(fit))
  if (!inherits(y, "Surv") || ncol(y) != 2L) {
    spicy_abort(
      c(sprintf("RMST / risk-difference columns need right-censored single-record data (%s).",
                model_id),
        "i" = "Counting-process (start-stop) responses have no per-subject curve."),
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}


# ---- Core curves -----------------------------------------------------------

# Baseline cumulative hazard of the fit, on a common time grid, one
# column per stratum (a single column for unstratified fits). For a
# stratified Cox model the standardization keeps each subject's OWN
# stratum baseline -- only the exposure is set counterfactually, the
# design (strata) is not -- so the helper also returns each subject's
# stratum column index (`s_idx`, NULL when unstratified). Cross-
# validated against adjustedCurves::adjusted_rmst (direct method) on
# a stratified lung fit: exact.
.coxph_baseline <- function(fit) {
  bh <- survival::basehaz(fit, centered = FALSE)
  if (is.null(bh$strata)) {
    return(list(times = bh$time,
                H0    = matrix(bh$hazard, ncol = 1L),
                s_idx = NULL))
  }
  grid <- sort(unique(bh$time))
  lv <- levels(bh$strata)
  H0 <- vapply(lv, function(sl) {
    b <- bh[bh$strata == sl, , drop = FALSE]
    # Right-continuous step interpolation; 0 before the stratum's
    # first event time.
    c(0, b$hazard)[findInterval(grid, b$time) + 1L]
  }, numeric(length(grid)))
  H0 <- matrix(H0, nrow = length(grid),
               dimnames = list(NULL, lv))
  mf <- stats::model.frame(fit)
  sp <- survival::untangle.specials(stats::terms(fit), "strata")
  s_subj <- if (length(sp$vars) == 1L) {
    mf[[sp$vars]]
  } else {
    survival::strata(mf[, sp$vars, drop = FALSE], shortlabel = FALSE)
  }
  s_chr <- as.character(s_subj)
  if (!all(s_chr %in% lv)) {
    spicy_abort(                                                      # nocov start
      c("Internal mismatch between the fit's strata labels and basehaz().",
        "i" = paste0("Combine multiple stratification variables into a ",
                     "single `strata(a, b)` term and refit.")),
      class = "spicy_invalid_input"
    )                                                                 # nocov end
  }
  list(times = grid, H0 = H0, s_idx = match(s_chr, lv))
}


# Standardized survival probabilities of `newdata` pushed through the
# fit, evaluated at the baseline grid. `H0` is the per-stratum matrix
# from .coxph_baseline(); `s_idx` maps each row of `newdata` to its
# stratum column (NULL = unstratified). Returns a vector S(times).
.coxph_standardized_survival <- function(fit, newdata, H0, s_idx = NULL) {
  lp <- stats::predict(fit, newdata = newdata, type = "lp",
                       reference = "zero")
  elp <- exp(lp)
  if (is.null(s_idx)) {
    # S matrix would be length(times) x n; average over subjects
    # without materializing it.
    return(vapply(H0[, 1L], function(h) mean(exp(-h * elp)),
                  numeric(1)))
  }
  # Stratified: each subject's curve uses their own stratum baseline.
  M <- H0[, s_idx, drop = FALSE]
  rowMeans(exp(-sweep(M, 2L, elp, "*")))
}


# Exact integral over [0, tau] of the right-continuous step function
# that equals `surv[k]` on [times[k], times[k+1]) and 1 on
# [0, times[1]).
.step_rmst <- function(times, surv, tau) {
  keep <- times <= tau
  t_k <- c(0, times[keep])
  s_k <- c(1, surv[keep])
  widths <- diff(c(t_k, tau))
  sum(widths * s_k)
}


# Standardized survival at a single landmark time t (step function:
# the value at the last event time <= t).
.step_surv_at <- function(times, surv, t) {
  idx <- findInterval(t, times)
  if (idx == 0L) return(1)
  surv[idx]
}


# ---- Point estimates -------------------------------------------------------

# One fit -> the estimand contrasts for every predictor variable.
# Returns a data.frame: term, parent_var, label, factor_level_pos,
# estimand ("rmst" / "risk_diff"), estimate.
.coxph_estimand_points <- function(fit, data, want_rmst, want_risk,
                                   tau, at_time) {
  bl <- .coxph_baseline(fit)
  times <- bl$times

  curve_stats <- function(newdata) {
    s <- .coxph_standardized_survival(fit, newdata, bl$H0, bl$s_idx)
    c(rmst = if (want_rmst) .step_rmst(times, s, tau) else NA_real_,
      risk = if (want_risk) 1 - .step_surv_at(times, s, at_time)
             else NA_real_)
  }

  rhs_vars <- setdiff(all.vars(stats::formula(fit)),
                      all.vars(stats::formula(fit)[[2L]]))
  # Strata variables get no contrast row: they carry no coefficient
  # (the table has no HR row for them either), and standardization
  # deliberately holds each subject's stratum fixed.
  rhs_vars <- setdiff(rhs_vars, .coxph_strata_vars(fit))
  rows <- list()
  for (v in rhs_vars) {
    col <- data[[v]]
    if (is.factor(col) || is.character(col)) {
      lvls <- if (is.factor(col)) levels(col) else sort(unique(col))
      base <- data
      base[[v]] <- .replace_level(col, lvls[1L])
      ref_stats <- curve_stats(base)
      for (j in seq_along(lvls)[-1L]) {
        alt <- data
        alt[[v]] <- .replace_level(col, lvls[j])
        d <- curve_stats(alt) - ref_stats
        rows[[length(rows) + 1L]] <- data.frame(
          term = paste0(v, lvls[j]), parent_var = v, label = lvls[j],
          factor_level_pos = as.integer(j),
          rmst = unname(d["rmst"]), risk = unname(d["risk"]),
          stringsAsFactors = FALSE
        )
      }
    } else if (is.numeric(col)) {
      plus <- data
      plus[[v]] <- col + 1
      d <- curve_stats(plus) - curve_stats(data)
      rows[[length(rows) + 1L]] <- data.frame(
        term = v, parent_var = v, label = v,
        factor_level_pos = NA_integer_,
        rmst = unname(d["rmst"]), risk = unname(d["risk"]),
        stringsAsFactors = FALSE
      )
    }
    # Other column types (logical enters as numeric contrast via the
    # model matrix but cannot take +1): skipped -- no estimand row.
  }
  do.call(rbind, rows)
}


# Variables absorbed by strata() terms (empty for unstratified fits).
.coxph_strata_vars <- function(fit) {
  sp <- survival::untangle.specials(stats::terms(fit), "strata")
  if (length(sp$vars) == 0L) return(character(0))
  all.vars(stats::reformulate(sp$vars))
}


# Counterfactual level assignment that preserves the factor structure.
.replace_level <- function(col, level) {
  if (is.factor(col)) {
    factor(rep(level, length(col)), levels = levels(col))
  } else {
    rep(level, length(col))
  }
}


# Refit the Cox model on a bootstrap resample so that DOWNSTREAM
# lookups still work: survival::basehaz() re-evaluates the fit's
# `call$data` in the formula environment, so the resampled data must
# live in an environment attached to the formula -- a plain
# `coxph(f, data = data[idx, ])` leaves every replicate's basehaz()
# unable to find `data` / `idx` and fails silently.
.coxph_refit_on <- function(f, dboot) {
  env <- new.env(parent = environment(f) %||% baseenv())
  env$.spicy_boot_data. <- dboot
  f2 <- f
  environment(f2) <- env
  eval(
    substitute(survival::coxph(FF, data = .spicy_boot_data.),
               list(FF = f2)),
    env
  )
}


# ---- Bootstrap inference ---------------------------------------------------

.coxph_estimand_rows <- function(fit, model_id, outcome, show_columns,
                                 tau = NULL, at_time = NULL,
                                 ci_level = 0.95, boot_n = 1000L) {
  want_rmst <- any(c("rmst", "rmst_se", "rmst_ci", "rmst_p") %in%
                     show_columns)
  want_risk <- any(c("risk_diff", "risk_diff_se", "risk_diff_ci",
                     "risk_diff_p") %in% show_columns)
  if (!want_rmst && !want_risk) return(NULL)
  .coxph_estimand_gates(fit, model_id)
  data <- .coxph_estimand_data(fit)

  # tau = "minmax": the smallest, across the levels of every factor
  # predictor, of that level's largest observed time -- so the RMST
  # integral never extrapolates beyond a compared group's follow-up.
  # No factor predictor: the largest observed time.
  tau_resolved <- tau
  if (want_rmst && identical(tau, "minmax")) {
    y <- stats::model.response(stats::model.frame(fit))
    obs_time <- as.numeric(y[, 1L])
    rhs_vars <- setdiff(all.vars(stats::formula(fit)),
                        all.vars(stats::formula(fit)[[2L]]))
    rhs_vars <- setdiff(rhs_vars, .coxph_strata_vars(fit))
    level_max <- numeric(0)
    for (v in rhs_vars) {
      col <- data[[v]]
      if (is.factor(col) || is.character(col)) {
        level_max <- c(level_max,
                       tapply(obs_time, as.character(col), max))
      }
    }
    tau_resolved <- if (length(level_max)) min(level_max) else
      max(obs_time)
  }

  pts <- .coxph_estimand_points(fit, data, want_rmst, want_risk,
                                tau_resolved, at_time)
  if (is.null(pts) || nrow(pts) == 0L) return(NULL)

  # Bootstrap: resample subjects, refit, recompute every contrast.
  f <- stats::formula(fit)
  n <- nrow(data)
  boot_est <- array(
    NA_real_, dim = c(boot_n, nrow(pts), 2L),
    dimnames = list(NULL, pts$term, c("rmst", "risk"))
  )
  for (b in seq_len(boot_n)) {
    idx <- sample.int(n, n, replace = TRUE)
    rep_fit <- tryCatch(
      suppressWarnings(
        .coxph_refit_on(f, data[idx, , drop = FALSE])
      ),
      error = function(e) NULL
    )
    # A replicate whose design lost a level (or is otherwise singular)
    # carries NA coefficients that predict() would silently treat as
    # zero -- a phantom null contrast. Count it as failed instead.
    if (is.null(rep_fit) || anyNA(stats::coef(rep_fit))) next
    rep_pts <- tryCatch(
      .coxph_estimand_points(rep_fit, data[idx, , drop = FALSE],
                             want_rmst, want_risk, tau_resolved,
                             at_time),
      error = function(e) NULL
    )
    if (is.null(rep_pts)) next
    m <- match(pts$term, rep_pts$term)
    boot_est[b, , "rmst"] <- rep_pts$rmst[m]
    boot_est[b, , "risk"] <- rep_pts$risk[m]
  }

  z_crit <- stats::qnorm(0.5 + ci_level / 2)
  build <- function(estimand_key, estimate_type) {
    est <- pts[[estimand_key]]
    reps <- matrix(boot_est[, , estimand_key], nrow = boot_n)
    se <- apply(reps, 2L, stats::sd, na.rm = TRUE)
    n_valid <- apply(reps, 2L, function(x) sum(is.finite(x)))
    if (any(n_valid < boot_n * 0.5)) {
      spicy_abort(
        sprintf(
          "More than half of the %d bootstrap replicates failed for the %s column.",
          boot_n, estimate_type
        ),
        class = "spicy_resampling_failed"
      )
    }
    stat <- est / se
    data.frame(
      term             = pts$term,
      parent_var       = pts$parent_var,
      label            = pts$label,
      factor_level_pos = pts$factor_level_pos,
      is_ref           = FALSE,
      estimate_type    = estimate_type,
      estimate         = est,
      std_error        = unname(se),
      df               = Inf,
      statistic        = unname(stat),
      p_value          = 2 * stats::pnorm(-abs(unname(stat))),
      ci_lower         = est - z_crit * unname(se),
      ci_upper         = est + z_crit * unname(se),
      test_type        = "z",
      stringsAsFactors = FALSE
    )
  }

  out <- list()
  if (want_rmst) out$rmst <- build("rmst", "rmst")
  if (want_risk) out$risk <- build("risk", "risk_diff")
  keys <- c(if (want_rmst) "rmst", if (want_risk) "risk")
  boot_valid <- min(vapply(keys, function(k) {
    min(colSums(is.finite(matrix(boot_est[, , k], nrow = boot_n))))
  }, numeric(1)))
  list(
    rows       = do.call(rbind, out),
    tau        = if (want_rmst) tau_resolved else NULL,
    at_time    = if (want_risk) at_time else NULL,
    boot_n     = boot_n,
    boot_valid = as.integer(boot_valid),
    stratified = !is.null(attr(fit$terms, "specials")$strata)
  )
}


# ---- Footer ----------------------------------------------------------------

# Table note for the estimand columns, read from
# extras$survival_estimands (set by the coxph frame builder).
build_survival_estimand_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  notes <- vapply(frames, function(f) {
    es <- f$info$extras$survival_estimands
    if (is.null(es)) return(NA_character_)
    parts <- character(0)
    if (!is.null(es$tau)) {
      parts <- c(parts, sprintf(
        "dRMST = difference in restricted mean survival time over [0, %s]",
        format(es$tau)
      ))
    }
    if (!is.null(es$at_time)) {
      parts <- c(parts, sprintf(
        "dRisk = difference in cumulative incidence at %s",
        format(es$at_time)
      ))
    }
    paste0(
      paste(parts, collapse = "; "),
      sprintf(
        "; adjusted by g-computation from the fitted model%s, SEs by nonparametric bootstrap (%s replicates).",
        if (isTRUE(es$stratified)) {
          " (within-stratum baselines)"
        } else {
          ""
        },
        if (es$boot_valid < es$boot_n) {
          sprintf("%d-%d", es$boot_valid, es$boot_n)
        } else {
          format(es$boot_n)
        }
      )
    )
  }, character(1))
  if (all(is.na(notes))) return(NULL)
  affected <- which(!is.na(notes))
  if (length(unique(notes[affected])) == 1L) {
    return(notes[affected][1L])
  }
  paste(vapply(affected, function(k) {                                # nocov start
    sprintf("Model %d: %s", k, notes[k])
  }, character(1)), collapse = "\n")                                  # nocov end
}
