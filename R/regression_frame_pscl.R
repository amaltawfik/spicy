# ---------------------------------------------------------------------------
# Phase 6c: as_regression_frame() methods for pscl fits.
#
# Two model classes:
#   * hurdle   -- two-part hurdle model: a zero-or-not binary part +
#                 a positive-count part (truncated Poisson / negbin /
#                 geometric).
#   * zeroinfl -- zero-inflated count model: a count part (Poisson /
#                 negbin / geometric) + a structural-zero inflation
#                 part (binary).
#
# Both classes carry TWO components: $count (the conditional count
# regression) and $zero (the binary / censored-count part). The frame's
# coefs table covers the COUNT component; the zero component ships as a
# fully-inferenced component block (info$extras$component_blocks) that the
# orchestrator promotes to a labelled subordinate block of table rows --
# "Zero-inflation" (zeroinfl: models P(structural zero)) or "Zero hurdle"
# (hurdle: models P(y > 0); verified numerically equal to
# glm(I(y > 0) ~ ...) for the binomial/logit case). See
# dev/component_blocks_spec.md.
#
# Per-class quirks vs glm:
#   * stats::coef(fit) returns a flat vector with "count_" / "zero_"
#     prefixes. We use coef(fit, model = "count") to get the
#     unprefixed count-component coefficients.
#   * summary(fit)$coefficients is a LIST with $count / $zero
#     matrices (Estimate / Std. Error / z value / Pr(>|z|)).
#   * stats::nobs(fit) is NOT defined; fit$n carries the sample size.
#   * fit$dist is a list $count + $zero for hurdle (e.g. "poisson" /
#     "binomial"); for zeroinfl it's a scalar (count dist) plus
#     fit$link (the zero-inflation link).
#   * The vcov subset to the count component has names equal to
#     coef(fit, model = "count").
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `hurdle` fits (pscl::hurdle()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.hurdle <- function(fit,
                                        vcov = "model",
                                        vcov_label = NULL,
                                        cluster = NULL,
                                        cluster_name = NULL,
                                        ci_level = 0.95,
                                        ci_method = NULL,
                                        show_columns = character(0),
                                        model_id = "M1",
                                        ...) {
  .check_pscl_available()
  .pscl_frame(fit, vcov, vcov_label, cluster, cluster_name,
              ci_level, ci_method, show_columns, model_id,
              is_hurdle = TRUE)
}


#' `as_regression_frame()` method for `zeroinfl` fits (pscl::zeroinfl()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.zeroinfl <- function(fit,
                                          vcov = "model",
                                          vcov_label = NULL,
                                          cluster = NULL,
                                          cluster_name = NULL,
                                          ci_level = 0.95,
                                          ci_method = NULL,
                                          show_columns = character(0),
                                          model_id = "M1",
                                          ...) {
  .check_pscl_available()
  .pscl_frame(fit, vcov, vcov_label, cluster, cluster_name,
              ci_level, ci_method, show_columns, model_id,
              is_hurdle = FALSE)
}


# Shared body of the two pscl frame methods.
.pscl_frame <- function(fit, vcov, vcov_label, cluster, cluster_name,
                        ci_level, ci_method, show_columns, model_id,
                        is_hurdle) {
  coefs <- .pscl_coefs(fit, ci_level = ci_level)
  # CR* -> sandwich::vcovCL cluster sandwich (Wald z); a no-op for the default.
  # estfun/bread cover BOTH components; the coefs (count) rows are re-inferred
  # here, the zero-component block rows inside .pscl_component_block().
  # pscl coef(fit) names are "count_"/"zero_"-prefixed while the coefs terms
  # are bare count names: pass a de-prefixed estimates vector so term matching
  # lands on the right positions of the full vcovCL matrix.
  cf_full <- stats::coef(fit)
  names(cf_full) <- sub("^count_", "", names(cf_full))
  coefs <- .apply_robust_vcov_to_coefs(coefs, fit, vcov, cluster, ci_level,
                                       test = "z", estimates = cf_full)
  # Combined-response AME (avg_slopes default): the marginal effect on E[Y]
  # through BOTH components; attaches to the main (count) rows.
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns,
                                      vcov_type = vcov, cluster = cluster)
  info  <- .pscl_info(fit,
                      vcov_kind  = vcov,
                      vcov_label = vcov_label,
                      ci_level   = ci_level,
                      ci_method  = ci_method,
                      model_id   = model_id,
                      is_hurdle  = is_hurdle,
                      vcov_type  = vcov,
                      cluster    = cluster)
  if (!vcov %in% c("model", "classical")) {
    info$vcov_label <- .robust_vcov_label(vcov, cluster_name %||% NA_character_,
                                          estimator = "CL")
  }
  new_regression_frame(coefs, info, fit)
}


# ---- Internal helpers -----------------------------------------------------

.check_pscl_available <- function() {
  if (!spicy_pkg_available("pscl")) {
    # nocov start: .check_<pkg>_available() abort -- only fires when pscl
    # is NOT installed, but the pscl methods (and their tests) require pscl
    # to be loaded, so this branch is unreachable under test.
    spicy_abort(
      c(
        "Cannot extract a regression frame from a pscl fit without `pscl`.",
        "i" = "Install pscl: `install.packages(\"pscl\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


# Build the coefs tibble for a hurdle / zeroinfl fit. Reads the count
# component from coef(fit, model = "count"). Wald z-asymptotic.
.pscl_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit, model = "count")
  est <- unname(cf)
  nm  <- names(cf)

  # vcov(fit) has "count_<term>" / "zero_<term>" names; subset to the
  # count block.
  V_full <- as.matrix(stats::vcov(fit))
  prefixed <- paste0("count_", nm)
  V <- V_full[prefixed, prefixed, drop = FALSE]
  se <- sqrt(diag(V))

  # summary(fit)$coefficients$count carries z + Pr(>|z|) directly.
  smc <- summary(fit)$coefficients$count
  if (!is.null(smc) && all(c("z value", "Pr(>|z|)") %in% colnames(smc))) {
    stat    <- unname(smc[nm, "z value"])
    p_value <- unname(smc[nm, "Pr(>|z|)"])
  } else {
    stat    <- est / se                                                # nocov
    p_value <- 2 * stats::pnorm(-abs(stat))                            # nocov
  }
  df <- rep(Inf, length(est))
  z_crit <- stats::qnorm(0.5 + ci_level / 2)
  ci_lower <- est - z_crit * se
  ci_upper <- est + z_crit * se

  factor_meta <- detect_factor_term_meta(fit)
  ft  <- vapply(nm, function(n) factor_meta[[n]]$factor_term  %||% NA_character_,
                character(1))
  lvl <- vapply(nm, function(n) factor_meta[[n]]$factor_level %||% NA_character_,
                character(1))
  pos <- vapply(nm, function(n) factor_meta[[n]]$factor_level_pos %||% NA_integer_,
                integer(1))

  parent_var <- ifelse(is.na(ft),  nm,  ft)
  label      <- ifelse(is.na(lvl), nm, lvl)

  coefs <- data.frame(
    term             = nm,
    parent_var       = parent_var,
    label            = label,
    factor_level_pos = as.integer(pos),
    is_ref           = rep(FALSE, length(nm)),
    estimate_type    = rep("B", length(nm)),
    estimate         = est,
    std_error        = se,
    df               = as.numeric(df),
    statistic        = stat,
    p_value          = p_value,
    ci_lower         = ci_lower,
    ci_upper         = ci_upper,
    test_type        = rep("z", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .pscl_reference_rows(fit)
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  coefs
}


# Reference-row synthesis. detect_factor_terms() uses the generic
# .spicy_get_xlevels() (which falls through to terms + model.frame
# for hurdle / zeroinfl).
.pscl_reference_rows <- function(fit) {
  fts <- detect_factor_terms(fit)
  if (length(fts) == 0L) return(.empty_coefs_frame())
  rows <- list()
  for (ft in fts) {
    if (!isTRUE(ft$reference_dropped)) next
    ref_lvl <- ft$reference_level
    term_name <- paste0(ft$factor_term, ref_lvl)
    ref_pos <- match(ref_lvl, ft$levels) %||% NA_integer_
    rows[[length(rows) + 1L]] <- data.frame(
      term             = term_name,
      parent_var       = ft$factor_term,
      label            = ref_lvl,
      factor_level_pos = as.integer(ref_pos),
      is_ref           = TRUE,
      estimate_type    = "B",
      estimate         = NA_real_,
      std_error        = NA_real_,
      df               = NA_real_,
      statistic        = NA_real_,
      p_value          = NA_real_,
      ci_lower         = NA_real_,
      ci_upper         = NA_real_,
      test_type        = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) return(.empty_coefs_frame())
  do.call(rbind, rows)
}


# Build the info list for a hurdle / zeroinfl fit.
.pscl_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method,
                        model_id, is_hurdle, vcov_type = "model",
                        cluster = NULL) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  # Family is determined by the count distribution.
  count_dist <- if (is_hurdle) fit$dist$count %||% "poisson" else fit$dist %||% "poisson"
  zero_dist  <- if (is_hurdle) fit$dist$zero  %||% "binomial" else "binomial"
  fam <- list(family = count_dist, link = "log")

  if (is.null(ci_method)) ci_method <- "wald"

  fit_stats <- list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
    aic            = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
    bic            = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
    log_lik        = tryCatch(as.numeric(stats::logLik(fit)),
                              error = function(e) NA_real_),
    deviance       = NA_real_,
    sigma          = NA_real_,
    nobs           = as.integer(fit$n %||% NA_integer_)
  )

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = TRUE,  # IRR (count component is log link)
    standardise_refit   = FALSE
  )

  # The zero component ships as a fully-inferenced component block that the
  # orchestrator promotes to a labelled subordinate rows block (see
  # dev/component_blocks_spec.md).
  #
  # zero_link: only meaningful when the zero part is a binomial GLM. A hurdle
  # with a count-type zero.dist (poisson / negbin / geometric right-censored)
  # has NO link concept on fit$link (it is NULL) -- its zero part lives on the
  # log scale and must never be labelled "logit".
  zero_link <- if (identical(zero_dist, "binomial")) {
    fit$link %||% "logit"
  } else {
    NA_character_
  }
  component_blocks <- list(
    .pscl_component_block(fit, is_hurdle, zero_dist, zero_link, ci_level,
                          vcov_type = vcov_type, cluster = cluster)
  )
  component_blocks <- Filter(Negate(is.null), component_blocks)

  title_prefix <- if (is_hurdle) {
    paste0(.pscl_dist_title(count_dist), " hurdle regression")
  } else {
    paste0(.pscl_dist_title(count_dist), " zero-inflated regression")
  }

  # has_weights: pscl stores case weights on fit$weights (all-1 when unset).
  wts <- fit$weights
  has_weights <- !is.null(wts) && length(wts) > 0L && any(wts != 1)

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = has_weights,
    weighted_n            = if (has_weights) sum(wts) else NA_real_,
    title_prefix          = title_prefix,
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    has_zi                = TRUE,
    component_blocks      = component_blocks,
    zero_dist             = zero_dist,
    zero_link             = zero_link
  )

  list(
    class          = if (is_hurdle) "hurdle" else "zeroinfl",
    family         = fam,
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(fit$n %||% NA_integer_),
    n_groups       = NULL,
    weights_kind   = "none",
    random_effects = empty_random_effects(),
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% "Wald asymptotic (z)",
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# Title-case distribution label for the count component.
.pscl_dist_title <- function(dist) {
  switch(dist,
    poisson       = "Poisson",
    geometric     = "Geometric",
    negbin        = "Negative-binomial",
    paste0(toupper(substr(dist, 1L, 1L)), substring(dist, 2L))
  )
}


# Build the zero-component block for a hurdle / zeroinfl fit: a standardised
# `component block` (label / link / exp_ok / gloss / coefs) the orchestrator
# promotes to a labelled subordinate rows block. Full Wald inference straight
# from summary(fit)$coefficients$zero -- no refit. Under a CR* request the SEs
# come from the zero_* rows of the same whole-model sandwich::vcovCL matrix
# (estfun covers both components), so the whole table shares one estimator.
#
# The labels are per-SEMANTICS (dev/component_blocks_spec.md D1): zeroinfl
# models P(structural zero) -> "Zero-inflation"; hurdle models P(y > 0) ->
# "Zero hurdle" (verified numerically == glm(I(y > 0) ~ .) for binomial/logit).
.pscl_component_block <- function(fit, is_hurdle, zero_dist, zero_link,
                                  ci_level, vcov_type = "model",
                                  cluster = NULL) {
  smz <- summary(fit)$coefficients$zero
  if (is.null(smz) || nrow(smz) == 0L) return(NULL)                    # nocov

  nm  <- rownames(smz)
  est <- unname(smz[, "Estimate"])
  se  <- unname(smz[, "Std. Error"])
  # Robust: re-infer from the zero_* block of the whole-model cluster sandwich.
  if (!vcov_type %in% c("model", "classical")) {
    vc_full <- compute_model_vcov(fit, type = vcov_type, cluster = cluster)
    pref <- paste0("zero_", nm)
    if (all(pref %in% rownames(vc_full))) {
      se <- sqrt(diag(as.matrix(vc_full))[pref])
    }
  }
  stat <- est / se
  p    <- 2 * stats::pnorm(-abs(stat))
  z    <- stats::qnorm(0.5 + ci_level / 2)

  # Factor metadata for the ZERO component: its terms live on
  # fit$terms$zero; fit$levels carries the xlevels union of both components.
  zero_vars <- tryCatch(all.vars(stats::delete.response(fit$terms$zero)),
                        error = function(e) character(0))
  xlev <- fit$levels[names(fit$levels) %in% zero_vars]
  ft  <- rep(NA_character_, length(nm))
  lvl <- rep(NA_character_, length(nm))
  pos <- rep(NA_integer_, length(nm))
  for (i in seq_along(nm)) {
    meta <- match_coef_to_factor(nm[i], xlev)
    if (!is.null(meta)) {
      ft[i]  <- meta$factor_term
      lvl[i] <- meta$factor_level
      pos[i] <- meta$factor_level_pos %||% NA_integer_
    }
  }

  label_chr <- if (is_hurdle) "Zero hurdle" else "Zero-inflation"
  rows <- data.frame(
    term             = paste0("zero_", nm),
    label            = ifelse(is.na(lvl), nm, paste0(ft, ": ", lvl)),
    factor_level_pos = as.integer(pos),
    is_ref           = FALSE,
    estimate         = est,
    std_error        = se,
    statistic        = stat,
    p_value          = p,
    ci_lower         = est - z * se,
    ci_upper         = est + z * se,
    stringsAsFactors = FALSE
  )

  # Reference rows for zero-component factors (independent of the count side).
  for (v in names(xlev)) {
    lvls <- xlev[[v]]
    present <- lvls[paste0(v, lvls) %in% nm]
    ref <- setdiff(lvls, present)
    if (length(ref) == length(lvls)) next   # factor absent from zero coefs   # nocov
    if (length(ref) >= 1L) {
      rows <- rbind(rows, data.frame(
        term             = paste0("zero_", v, ref[1L]),
        label            = paste0(v, ": ", ref[1L]),
        factor_level_pos = as.integer(match(ref[1L], lvls)),
        is_ref           = TRUE,
        estimate         = NA_real_,
        std_error        = NA_real_,
        statistic        = NA_real_,
        p_value          = NA_real_,
        ci_lower         = NA_real_,
        ci_upper         = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
  }

  rows <- .order_component_rows(rows, xlev)

  gloss <- if (is_hurdle) {
    if (identical(zero_dist, "binomial")) {
      "Zero hurdle component: log-odds of a nonzero count."
    } else {
      sprintf(
        "Zero hurdle component: right-censored %s on the log scale.",
        zero_dist
      )
    }
  } else {
    "Zero-inflation component: log-odds of a structural (excess) zero."
  }

  list(
    label  = label_chr,
    link   = zero_link,
    exp_ok = identical(zero_link, "logit"),
    gloss  = gloss,
    coefs  = rows
  )
}


# Display order for a component block's rows: intercept first, then each
# factor's rows contiguous with the reference level in its natural `levels()`
# position, then remaining numeric terms. The appender re-keys
# factor_level_pos to this sequence and the align layer preserves it
# (block groups sort by position only).
.order_component_rows <- function(rows, xlev) {
  ft_of <- function(lbl) {
    hit <- regmatches(lbl, regexpr("^[^:]+(?=: )", lbl, perl = TRUE))
    if (length(hit)) hit else NA_character_
  }
  fts <- vapply(rows$label, ft_of, character(1), USE.NAMES = FALSE)
  fct_order <- unique(fts[!is.na(fts)])
  key1 <- ifelse(rows$label == "(Intercept)", 0L,
                 ifelse(is.na(fts), length(fct_order) + 1L,
                        match(fts, fct_order)))
  key2 <- ifelse(is.na(rows$factor_level_pos), 0L, rows$factor_level_pos)
  rows[order(key1, key2), , drop = FALSE]
}
