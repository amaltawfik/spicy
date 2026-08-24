# ---------------------------------------------------------------------------
# Phase 4b: as_regression_frame() methods for nlme fits.
#
# Two model classes:
#   * lme  -- linear mixed-effects (nlme::lme()). Gaussian-identity
#             implicit (no family slot). Wald-t with per-coefficient
#             DF from summary(fit)$tTable[, "DF"] (containment-style).
#   * gls  -- generalised least squares (nlme::gls()). No random effects;
#             supports correlation / variance structures. Wald-t with
#             df = nobs(fit) - length(coef(fit)).
#
# Per-class quirks (versus lme4):
#   * fixef() returns a flat named numeric vector (no $cond / $zi).
#   * stats::model.frame(fit) is BROKEN -- returns the random-effects /
#     correlation structure object, not the data. Polymorphic accessor
#     .spicy_get_xlevels() uses nlme::getData() instead.
#   * stats::family(fit) errors -- nlme is Gaussian-only.
#   * stats::df.residual(fit) is NULL -- inference DF lives in summary
#     or is derived from nobs - p.
#   * For lme, nlme::VarCorr(fit) returns a CHARACTER matrix (class
#     "VarCorr.lme") with columns "Variance" / "StdDev"; values must
#     be parsed via as.numeric().
#   * For lme, summary(fit)$ngrps is NULL; primary grouping factor
#     count comes from fit$dims$ngrps[[1]] (first slot; the trailing
#     "X" / "y" slots are fixed-effect / response dummies).
# ---------------------------------------------------------------------------

#' `as_regression_frame()` method for `lme` fits (nlme::lme()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.lme <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  cluster = NULL,
  cluster_name = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  standardized = "none",
  exponentiate = FALSE,
  model_id = "M1",
  ...
) {
  .check_nlme_available()

  coefs <- .lme_coefs(fit, ci_level = ci_level)
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "t",
    estimates = nlme::fixef(fit)
  )
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster
  )
  coefs <- .attach_partial_chi2_to_frame_coefs(coefs, fit, show_columns)
  coefs <- .attach_beta_to_frame_coefs(coefs, fit, standardized, ci_level)
  info <- .lme_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id
  )
  if (!vcov %in% c("model", "classical")) {
    info$vcov_label <- .robust_vcov_label(vcov, cluster_name %||% NA_character_)
  }
  # Phase 7c16: exp() on the B / beta rows for non-identity links.
  # nlme::lme is Gaussian-identity by spec, so this is currently a
  # no-op -- kept for parity with the other mixed paths.
  out <- .apply_exp_to_mixed_frame(coefs, info, fit, exponentiate)

  new_regression_frame(out$coefs, out$info, fit)
}


#' `as_regression_frame()` method for `gls` fits (nlme::gls()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.gls <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  model_id = "M1",
  ...
) {
  .check_nlme_available()

  coefs <- .gls_coefs(fit, ci_level = ci_level)
  # AME rows when requested: the registry advertises supports$ame for
  # gls, and avg_slopes() reads the fit's own vcov (the correlation /
  # variance structure is baked into it). Without this attach the AME
  # column rendered silently empty (show_columns was swallowed by
  # `...`).
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = NULL
  )
  info <- .gls_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id
  )

  new_regression_frame(coefs, info, fit)
}


# ---- Internal helpers -----------------------------------------------------

.check_nlme_available <- function() {
  if (!spicy_pkg_available("nlme")) {
    # nocov start
    spicy_abort(
      c(
        "Cannot extract a regression frame from an nlme fit without `nlme`.",
        "i" = "Install nlme: `install.packages(\"nlme\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


# Build the coefs tibble for an lme fit. Wald-t with per-coefficient DF
# pulled from summary(fit)$tTable.
.lme_coefs <- function(fit, ci_level) {
  fixef <- nlme::fixef(fit)
  V <- as.matrix(stats::vcov(fit))
  est <- unname(fixef)
  se <- sqrt(diag(V))
  nm <- names(fixef)

  tT <- summary(fit)$tTable
  df <- unname(tT[nm, "DF"])
  stat <- unname(tT[nm, "t-value"])
  p_value <- unname(tT[nm, "p-value"])
  t_crit <- stats::qt(0.5 + ci_level / 2, df = df)
  ci_lower <- est - t_crit * se
  ci_upper <- est + t_crit * se

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
    test_type = rep("t", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .nlme_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


# Build the coefs tibble for a gls fit. Wald-t with df = nobs - p.
.gls_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit)
  V <- as.matrix(stats::vcov(fit))
  est <- unname(cf)
  se <- sqrt(diag(V))
  nm <- names(cf)
  df_val <- as.numeric(stats::nobs(fit) - length(cf))

  tT <- summary(fit)$tTable
  stat <- unname(tT[nm, "t-value"])
  p_value <- unname(tT[nm, "p-value"])
  df <- rep(df_val, length(est))
  t_crit <- stats::qt(0.5 + ci_level / 2, df = df_val)
  ci_lower <- est - t_crit * se
  ci_upper <- est + t_crit * se

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
    df = df,
    statistic = stat,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    test_type = rep("t", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .nlme_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


# Reference-row synthesis shared by lme and gls. Mirrors the lm / merMod
# path but uses the polymorphic accessors (which route lme/gls through
# nlme::getData()).
.nlme_reference_rows <- function(fit) {
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


# Build the info list for an lme fit.
.lme_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id
) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label_nlme(fit, dv)

  # Group counts, one entry per grouping factor. fit$dims$ngrps is a
  # named integer vector holding the grouping factors first, then the
  # "X" / "y" fixed-effect / response dummies -- so the leading
  # length(reStruct) slots are the real ones. It runs INNERMOST-first,
  # while VarCorr and intervals() (and therefore the variance-component
  # rows) run OUTERMOST-first: reverse it so the "n_groups" token renders
  # its "N (<factor>)" rows in the same order as the block above them.
  # Reading only ngrps[1] reported the innermost level and silently
  # dropped every outer one from a nested fit. The rows are labelled the
  # way the variance-component block above them is (.lme_group_labels),
  # so "N (Side:Dog) 20" says what the 20 units are -- Dog-by-Side
  # combinations -- and matches what an lmer of the same structure calls
  # them.
  # ngrps is already innermost-first, which is the order the block above
  # them now uses (.lme_order_blocks), so the leading slots are taken
  # as-is.
  ng <- fit$dims$ngrps
  labels <- .lme_group_labels(fit)
  n_re_levels <- length(labels)
  n_groups <- if (n_re_levels > 0L && length(ng) >= n_re_levels) {
    keep <- seq_len(n_re_levels)
    setNames(as.integer(ng[keep]), unname(labels[names(ng)[keep]]))
  } else {
    NULL # nocov  (lme fits always carry >= 1 grouping factor)
  }

  re <- .lme_random_effects(fit, ci_level = ci_level)
  fit_stats <- .nlme_fit_stats(fit)

  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  # partial_effect_size = TRUE: as_regression_frame.lme() calls
  # .attach_partial_chi2_to_frame_coefs(), so `partial_chi2` rows are
  # real here. as_regression_frame.gls() does NOT, and .gls_info()
  # below keeps the flag FALSE -- the two declarations differ because
  # the two builders differ. Read by table_regression()'s capability
  # guard, decision 41.
  supports <- list(
    ame = TRUE,
    partial_effect_size = TRUE,
    classical_r2 = FALSE,
    nested_lrt = TRUE,
    exponentiate = FALSE,
    standardise_refit = TRUE
  )

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    # Boundary fit (see .lme_is_singular): drives the footer note and the
    # orchestrator's build-time caveat, exactly as lme4::isSingular()
    # does for merMod. singular_terms stays empty: the diagnostic is
    # model-level, not per-coefficient.
    has_singular = .lme_is_singular(fit),
    singular_terms = character(0),
    has_weights = FALSE,
    weighted_n = NA_real_,
    title_prefix = "Linear mixed-effects regression (nlme)",
    exp_applied = FALSE,
    exp_header = NA_character_
  )

  list(
    class = "lme",
    family = list(family = "gaussian", link = "identity"),
    dv = dv,
    dv_label = dv_label,
    n_obs = as.integer(stats::nobs(fit)),
    n_groups = n_groups,
    weights_kind = "none",
    random_effects = re,
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||% "Wald (model-based)",
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# Build the info list for a gls fit. No random effects; correlation
# structure label is surfaced in vcov_label.
.gls_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id
) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label_nlme(fit, dv)

  fit_stats <- .nlme_fit_stats(fit)

  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  supports <- list(
    ame = TRUE,
    partial_effect_size = FALSE,
    classical_r2 = FALSE,
    nested_lrt = TRUE,
    exponentiate = FALSE,
    standardise_refit = FALSE
  )

  corr_label <- .gls_corstruct_label(fit)
  default_vcov_label <- if (is.null(corr_label)) {
    "Wald (model-based)"
  } else {
    paste0("Wald (model-based, ", corr_label, ")")
  }

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    # gls has no random-effect structure to collapse, and nlme::gls()
    # refuses a rank-deficient design outright, so neither singular
    # regime can arise: the flag is a constant here, not a stub.
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = FALSE,
    weighted_n = NA_real_,
    title_prefix = "Generalised least squares (nlme)",
    exp_applied = FALSE,
    exp_header = NA_character_,
    correlation_structure = corr_label
  )

  list(
    class = "gls",
    family = list(family = "gaussian", link = "identity"),
    dv = dv,
    dv_label = dv_label,
    n_obs = as.integer(stats::nobs(fit)),
    n_groups = NULL,
    weights_kind = "none",
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||% default_vcov_label,
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# Fit-stats common to lme and gls. r_squared / adj_r_squared are NA
# (classical R^2 not defined for these models); pseudo_r2 is NULL.
.nlme_fit_stats <- function(fit) {
  # Phase 7c9a: lme fits get Nakagawa marginal / conditional R^2 via
  # performance::r2_nakagawa(). gls fits don't have random effects, so
  # the helper returns NA for both -- mirroring how lm's r_squared would
  # not be reported as "marginal" / "conditional" either.
  r2_ns <- if (inherits(fit, "lme")) {
    .nakagawa_r2(fit)
  } else {
    list(marginal = NA_real_, conditional = NA_real_)
  }
  list(
    r_squared = NA_real_,
    adj_r_squared = NA_real_,
    pseudo_r2 = NULL,
    r2_marginal = r2_ns$marginal,
    r2_conditional = r2_ns$conditional,
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    log_lik = as.numeric(stats::logLik(fit)),
    deviance = tryCatch(
      suppressWarnings(stats::deviance(fit)),
      error = function(e) NA_real_
    ),
    sigma = tryCatch(stats::sigma(fit), error = function(e) NA_real_),
    nobs = as.integer(stats::nobs(fit))
  )
}


# Boundary ("singular") fit: a random-effect covariance block has
# collapsed onto the edge of the parameter space -- a variance at 0, or a
# correlation at +/-1. nlme ships no isSingular() of its own (unlike
# lme4, which the merMod frame calls).
#
# The criterion is the glmmTMB frame's, on the same relative scale:
# det(V / sigma^2) < 1e-5 for a random-effect covariance block V. nlme
# hands it over ready-made -- fit$modelStruct$reStruct IS the covariance
# relative to sigma^2, which is also lme4's parameterisation, so the
# blocks are read as they come. Multiplying them back by sigma^2 (what
# nlme::getVarCov() does to report absolute variances) is exactly the
# step that must NOT be taken here.
#
# One block per nesting level, residual excluded by construction
# (reStruct holds the random effects only). det() rather than the
# diagonal so a boundary correlation counts as well as a collapsed
# variance.
#
# DIVERGENCE FROM performance::check_singularity.lme(), deliberate and
# larger than for glmmTMB, since that method is the weaker one in
# performance on three counts:
#   * it reads diag(nlme::getVarCov(fit)), which ERRORS on any fit with
#     more than one level of nesting; the reStruct walk answers there;
#   * it tests the diagonal, so a random-effect correlation pinned at
#     +/-1 with healthy variances is not singular for lme while it is for
#     lmer -- performance's own merMod and glmmTMB methods use det();
#   * it tests absolute variances, so its verdict moves when the response
#     is rescaled: on Orthodont, distance / 20 with a random slope is
#     flagged while distance is not, and a genuinely collapsed component
#     stops being flagged once the response is scaled up.
# Verdicts therefore differ from performance's on Gaussian fits whose
# sigma is far from 1. They track lme4::isSingular(), which is what the
# table note promises the reader.
#
# nlme optimises on the log-Cholesky scale, where an exact zero variance
# sits at -Inf: a collapsed component lands NEAR 0, never on it, and the
# tolerance is what turns "near" into a verdict. Non-finite variances
# (det() = NA) fail the comparison and read as not singular; a fit that
# stopped before converging is a separate diagnostic, carried by the
# engine's own warning -- and its variance components lose their SE and
# CI through .lme_blank_degenerate_vc(), not through this flag.
.lme_is_singular <- function(fit, tolerance = 1e-5) {
  blocks <- as.matrix(fit$modelStruct$reStruct)
  if (!is.list(blocks)) {
    return(FALSE) # nocov  (an lme fit always carries a reStruct)
  }
  any(vapply(
    blocks,
    function(v) isTRUE(det(as.matrix(v)) < tolerance),
    logical(1)
  ))
}


# How each grouping level of an lme fit is LABELLED in the table, keyed
# by the bare name nlme uses internally. Returns a named character
# vector, outermost level first: c(Dog = "Dog", Side = "Side:Dog").
#
# nlme and lme4 name the same nested block differently. `~ 1 | Dog/Side`
# and `(1 | Dog/Side)` fit the same two levels, but nlme calls the inner
# one "Side" while lme4 expands the slash into `(1|Dog) + (1|Dog:Side)`
# and calls it "Side:Dog" -- verified against lme4::VarCorr(), which is
# what the merMod frame reads. Three levels give "half:Side:Dog",
# "Side:Dog", "Dog": each level is joined to its ancestors with ":",
# innermost component first.
#
# Adopting lme4's spelling is not cosmetic. The RE rows are keyed
# `re::<group>::<term>`, so a table holding the same nested model fitted
# by both engines rendered THREE sigma rows and three N rows with holes
# instead of two aligned ones. It is also the more honest label: nlme's
# bare "Side" has 20 units on Pixel, which are Dog-by-Side combinations,
# not 20 sides -- nlme's own summary writes it "Side %in% Dog".
#
# The nesting order comes from the FITTED object, not from the call:
# lme() normalises `random = list(Dog = ~day, Side = ~1)` to the same
# `~ Dog/Side` structure as the slash form, and both leave
# modelStruct$reStruct ordered innermost-first. lme has no crossed
# regime -- every level below the outermost is nested in the ones above
# it -- so composing every non-outermost level is always right.
.lme_group_labels <- function(fit) {
  raw <- rev(names(fit$modelStruct$reStruct))
  if (length(raw) == 0L) {
    return(setNames(character(0), character(0))) # nocov  (lme has >= 1)
  }
  setNames(
    vapply(
      seq_along(raw),
      function(k) paste(rev(raw[seq_len(k)]), collapse = ":"),
      character(1)
    ),
    raw
  )
}


# Row order of the variance-component block. nlme's VarCorr lists nested
# blocks outermost-first (Dog, then Side); lme4 lists them innermost-first
# (Side:Dog, then Dog). .lme_group_labels() already adopts lme4's spelling,
# so adopt its order too: the RE block of one structure is then row-for-row
# identical across the two engines rather than merely key-compatible, and a
# multi-model table's row order stops depending on which model the user
# listed first. Stable within a block, and the residual always closes it.
#
# Only NESTED fits move. A single-level fit has one block and comes back
# untouched -- the sort is stable and every non-residual row shares one
# rank.
.lme_order_blocks <- function(vc_df, labels) {
  if (nrow(vc_df) == 0L) {
    return(vc_df) # nocov
  }
  rank_of <- rev(unname(labels)) # innermost level first
  rank <- match(vc_df$group, rank_of)
  # Residual (and anything unrecognised) closes the block.
  rank[is.na(rank)] <- length(rank_of) + 1L
  ord <- order(rank, seq_len(nrow(vc_df)))
  # A frame nothing moves in comes back untouched, rownames included:
  # the reorder must be invisible -- to the byte -- wherever it has
  # nothing to do. The merMod twin (.merMod_order_blocks) already holds
  # this; the asymmetry was noticed at review and is closed here.
  if (identical(ord, seq_len(nrow(vc_df)))) {
    return(vc_df)
  }
  out <- vc_df[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}


# Extract random-effects metadata from an lme fit. nlme::VarCorr.lme()
# returns a CHARACTER matrix with columns "Variance" / "StdDev" and
# rows labelled with the random-effect term names + "Residual".
.lme_random_effects <- function(fit, ci_level = 0.95) {
  # nlme::lme exposes the estimator via fit$method: "REML" (default)
  # or "ML". Feeds the footer's "(REML)" / "(ML)" clarification.
  method <- if (
    !is.null(fit$method) &&
      fit$method %in% c("REML", "ML")
  ) {
    fit$method
  } else {
    NA_character_
  }
  vc <- tryCatch(nlme::VarCorr(fit), error = function(e) NULL)
  # nocov start  (VarCorr() does not error for a valid lme fit)
  if (is.null(vc)) {
    return(utils::modifyList(empty_random_effects(), list(method = method)))
  }
  # nocov end
  raw <- unclass(vc)
  rn <- rownames(raw)
  variances <- suppressWarnings(as.numeric(raw[, "Variance"]))
  sds <- suppressWarnings(as.numeric(raw[, "StdDev"]))

  # Which grouping factor each row belongs to. VarCorr.lme() flattens
  # EVERY level of a nested fit into one character matrix and marks each
  # block with a "<group> =" header row -- the pdMat class sits in the
  # Variance cell, so the header does not parse as a number and is
  # skipped. Walking those headers is the only in-band record of the
  # block a row belongs to. A single-level fit carries no header at all
  # (its group name lives in attr(vc, "title")), and there
  # fit$dims$ngrps[1] IS the grouping factor.
  #
  # ngrps alone is not that record: it runs INNERMOST-first, the reverse
  # of VarCorr's block order. Reading only its first name labelled every
  # block with the innermost group -- on `random = ~ 1 | Dog/Side` both
  # levels printed as "Side", which also sent both intervals() lookups
  # below into the same reStruct block, so the two levels shared one SE
  # and one CI, and the renderer (which keys RE rows on group + term)
  # collapsed them into a single row.
  #
  # The header gives nlme's BARE name for the level ("Side"), which is
  # not what the level is: nlme's own summary prints it "Side %in% Dog",
  # and its 20 units are Dog-by-Side combinations, not 20 sides. It is
  # also not what lme4 calls the same block -- see .lme_group_labels().
  labels <- .lme_group_labels(fit)
  default_group <- labels[[1L]] %||% names(fit$dims$ngrps)[1L]
  current_group <- default_group
  rows <- list()
  for (i in seq_along(rn)) {
    if (is.na(variances[i])) {
      # Sub-header line. When it is a block header ("<group> =") it names
      # the group of every row until the next header; anything else that
      # fails to parse is simply skipped, as before.
      if (grepl("[[:space:]]=$", rn[i])) {
        hdr <- sub("[[:space:]]*=$", "", rn[i])
        current_group <- if (hdr %in% names(labels)) labels[[hdr]] else hdr
      }
      next
    }
    grp <- if (identical(rn[i], "Residual")) "Residual" else current_group
    rows[[length(rows) + 1L]] <- data.frame(
      group = grp,
      term = if (identical(rn[i], "Residual")) "" else rn[i],
      variance = variances[i],
      sd = sds[i],
      corr = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  vc_df <- if (length(rows) > 0L) do.call(rbind, rows) else data.frame()

  # Phase 7c7b: append correlation rows (off-diagonal entries from
  # the random-effects covariance matrix). lme's intervals() exposes
  # them under names like "cor((Intercept),age)" inside reStruct -- one
  # entry per grouping factor, so the walk covers them all.
  # `raw_of` maps the displayed (lme4-style) label back to nlme's bare
  # name, which is how intervals()$reStruct is keyed.
  raw_of <- setNames(names(labels), unname(labels))
  re_groups <- if (nrow(vc_df) > 0L) {
    raw_of[intersect(unname(labels), unique(vc_df$group))]
  } else {
    raw_of[0L] # nocov  (an lme fit always has one variance component)
  }
  vc_df <- .lme_append_correlation_rows(vc_df, fit, re_groups)

  # Phase 7c7a: extend with Wald SE + CI (at ci_level) via
  # nlme::intervals(). intervals() returns CIs on the SD scale (the
  # natural log-SD parametrisation backtransformed); we square to
  # convert to variance scale, and Delta-method for SE
  # (SE(sd^2) = 2*sd*SE(sd)).
  vc_df <- .lme_attach_wald_se_ci(
    vc_df,
    fit,
    ci_level = ci_level,
    raw_of = raw_of
  )

  # Last, so every row above carries its final values: put the blocks in
  # lme4's order (see .lme_order_blocks).
  vc_df <- .lme_order_blocks(vc_df, labels)

  icc <- .merMod_icc(vc_df) # reuse: same variance-ratio rule
  null_lrt <- .compute_null_model_lrt(fit)
  list(
    variance_components = vc_df,
    icc = icc,
    icc_omitted = .merMod_icc_omitted_reason(vc_df, icc),
    method = method,
    null_lrt = null_lrt
  )
}


# Phase 7c7b: append correlation rows from the random-effects
# covariance structure. For lme fits with `random = ~ X | group`, the
# intercept-slope correlation appears in intervals()$reStruct under
# rownames like "cor((Intercept),age)". The schema marker
# `is_correlation = TRUE` distinguishes correlation rows from variance
# rows for downstream renderers. `groups` is every grouping factor of the
# fit, in VarCorr block order: a nested fit can carry a correlation at
# more than one level, and intervals()$reStruct holds one entry per level.
# It is NAMED by the displayed label and VALUED by nlme's bare name (see
# .lme_group_labels), because reStruct is keyed on the bare one.
.lme_append_correlation_rows <- function(vc_df, fit, groups) {
  # Ensure schema columns even if no correlations are appended.
  if (!"is_correlation" %in% colnames(vc_df)) {
    vc_df$is_correlation <- FALSE
  }
  ci_obj <- tryCatch(
    nlme::intervals(fit, which = "var-cov"),
    error = function(e) NULL
  )
  if (is.null(ci_obj) || is.null(ci_obj$reStruct)) {
    return(vc_df)
  }

  rows_extra <- list()
  for (k in seq_along(groups)) {
    group_nm <- names(groups)[k] # displayed label, what vc_df carries
    group_ci <- ci_obj$reStruct[[unname(groups[k])]] # nlme's bare name
    if (is.null(group_ci)) {
      next # nocov
    }
    cor_rows <- grep("^cor\\(", rownames(group_ci), value = TRUE)
    if (length(cor_rows) == 0L) {
      next
    }
    for (rn in cor_rows) {
      est <- group_ci[rn, "est."]
      pair <- sub("^cor\\((.+)\\)$", "\\1", rn)
      # Normalise "(Intercept),age" (nlme's comma-only join) to the canonical
      # "(Intercept), age" (lme4's ", " join, VarCorr term order) so identical
      # random structures align across engines in a multi-model table. Match
      # both sides against the group's known variance-row terms -- robust to
      # term names that themselves contain a comma-free "," is impossible in
      # an R name, but matching keeps the split principled.
      known <- vc_df$term[
        vc_df$group == group_nm &
          !(vc_df$is_correlation %in% TRUE)
      ]
      commas <- gregexpr(",", pair, fixed = TRUE)[[1L]]
      for (pos in commas) {
        lhs <- trimws(substr(pair, 1L, pos - 1L))
        rhs <- trimws(substr(pair, pos + 1L, nchar(pair)))
        if (lhs %in% known && rhs %in% known) {
          ord <- c(lhs, rhs)[order(match(c(lhs, rhs), known))]
          pair <- paste(ord, collapse = ", ")
          break
        }
      }
      rows_extra[[length(rows_extra) + 1L]] <- data.frame(
        group = group_nm,
        term = pair,
        variance = NA_real_,
        sd = NA_real_,
        corr = est,
        is_correlation = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows_extra) == 0L) {
    return(vc_df)
  }
  extra_df <- do.call(rbind, rows_extra)
  # Insert correlation rows BEFORE the residual (so the residual stays
  # at the bottom of the group's section).
  is_resid <- vc_df$group == "Residual"
  rbind(
    vc_df[!is_resid, , drop = FALSE],
    extra_df,
    vc_df[is_resid, , drop = FALSE]
  )
}


# Attach Wald SE + CI (at ci_level) on variance scale via
# nlme::intervals(). `raw_of` maps the displayed group label back to
# nlme's bare name, which is how intervals()$reStruct is keyed (see
# .lme_group_labels).
.lme_attach_wald_se_ci <- function(
  vc_df,
  fit,
  ci_level = 0.95,
  raw_of = NULL
) {
  bare <- function(g) {
    if (!is.null(raw_of) && g %in% names(raw_of)) unname(raw_of[[g]]) else g
  }
  # nocov start  (only invoked from the defensive guards below)
  na_block <- function(df) {
    df$std_error <- NA_real_
    df$ci_lower <- NA_real_
    df$ci_upper <- NA_real_
    df$ci_method <- NA_character_
    df
  }
  # nocov end
  if (nrow(vc_df) == 0L) {
    return(na_block(vc_df)) # nocov
  }
  # Boundary fit: this guard does real work. nlme::intervals() does not
  # consistently refuse one -- it errors ("Non-positive definite
  # approximate variance-covariance") on some boundary fits and returns a
  # degenerate [0, Inf] on others, which would reach the table as an
  # interval. Suppress on the flag instead, the merMod precedent, so the
  # singular footer's claim holds by construction rather than by luck.
  if (.lme_is_singular(fit)) {
    return(na_block(vc_df))
  }

  ci_obj <- tryCatch(
    nlme::intervals(fit, level = ci_level, which = "var-cov"),
    error = function(e) NULL
  )
  if (is.null(ci_obj)) {
    return(na_block(vc_df))
  }

  vc_df$std_error <- NA_real_
  vc_df$ci_lower <- NA_real_
  vc_df$ci_upper <- NA_real_
  vc_df$ci_method <- NA_character_

  # z at the SAME level as the intervals() call: the SE is derived from
  # the interval half-width, so the two must stay coupled.
  z <- stats::qnorm(0.5 + ci_level / 2)
  is_corr <- if ("is_correlation" %in% colnames(vc_df)) {
    vc_df$is_correlation %in% TRUE
  } else {
    rep(FALSE, nrow(vc_df))
  }
  for (i in seq_len(nrow(vc_df))) {
    g <- vc_df$group[i]
    t <- vc_df$term[i]

    if (isTRUE(is_corr[i])) {
      # Correlation row: intervals reStruct exposes "cor(<pair>)" rows
      # on the natural rho scale (not transformed). Wald CI symmetric.
      # vc_df stores the pair canonically as "<t1>, <t2>" (engine-aligned);
      # nlme's rowname joins with a bare comma -- try both orders and both
      # separators via exact-string matching.
      group_ci <- ci_obj$reStruct[[bare(g)]]
      if (is.null(group_ci)) {
        next # nocov
      }
      comps <- strsplit(t, ", ", fixed = TRUE)[[1L]]
      targets <- if (length(comps) == 2L) {
        paste0(
          "cor(",
          c(
            paste(comps, collapse = ","),
            paste(rev(comps), collapse = ","),
            paste(comps, collapse = ", "),
            paste(rev(comps), collapse = ", ")
          ),
          ")"
        )
      } else {
        paste0("cor(", t, ")") # nocov
      }
      row_idx <- which(rownames(group_ci) %in% targets)[1L]
      if (is.na(row_idx)) {
        next # nocov
      }
      cor_lower <- group_ci[row_idx, "lower"]
      cor_upper <- group_ci[row_idx, "upper"]
      vc_df$std_error[i] <- (cor_upper - cor_lower) / (2 * z)
      vc_df$ci_lower[i] <- cor_lower
      vc_df$ci_upper[i] <- cor_upper
      vc_df$ci_method[i] <- "wald"
      next
    }

    if (identical(g, "Residual")) {
      sigma_ci <- ci_obj$sigma
      if (is.null(sigma_ci) || length(sigma_ci) != 3L) {
        next # nocov
      }
      sd_est <- unname(sigma_ci["est."])
      sd_lower <- unname(sigma_ci["lower"])
      sd_upper <- unname(sigma_ci["upper"])
    } else {
      group_ci <- ci_obj$reStruct[[bare(g)]]
      if (is.null(group_ci)) {
        next # nocov
      }
      target <- paste0("sd(", t, ")")
      row_idx <- match(target, rownames(group_ci))
      if (is.na(row_idx)) {
        next # nocov
      }
      sd_est <- group_ci[row_idx, "est."]
      sd_lower <- group_ci[row_idx, "lower"]
      sd_upper <- group_ci[row_idx, "upper"]
    }
    se_sd <- (sd_upper - sd_lower) / (2 * z)
    vc_df$std_error[i] <- 2 * sd_est * se_sd
    vc_df$ci_lower[i] <- max(0, sd_lower)^2
    vc_df$ci_upper[i] <- sd_upper^2
    vc_df$ci_method[i] <- "wald"
  }
  .lme_blank_degenerate_vc(vc_df)
}


# Second line of defence, independent of the singular flag: a Wald
# quantity that came back non-finite says nothing and must not reach the
# table. nlme::intervals() returns an open [0, Inf] bound on a component
# it cannot bound, which squares to an infinite variance-scale CI.
# Blanking the row renders it as the undefined glyph, like any other
# unavailable cell. Twin of .glmmTMB_blank_degenerate_vc(); the same
# limitation applies (a finite but absurd interval is the boundary
# flag's business, not this guard's).
.lme_blank_degenerate_vc <- function(vc_df) {
  bad <- !is.finite(vc_df$std_error) |
    !is.finite(vc_df$ci_lower) |
    !is.finite(vc_df$ci_upper)
  if (any(bad)) {
    vc_df$std_error[bad] <- NA_real_
    vc_df$ci_lower[bad] <- NA_real_
    vc_df$ci_upper[bad] <- NA_real_
    vc_df$ci_method[bad] <- NA_character_
  }
  vc_df
}


# Inspect the correlation structure on a gls fit. Returns a short
# label like "corCompSymm" or NULL if no structure was specified.
.gls_corstruct_label <- function(fit) {
  cs <- fit$modelStruct$corStruct
  if (is.null(cs)) {
    return(NULL)
  }
  class(cs)[1L]
}


# DV label extractor for nlme fits. stats::model.frame() is broken
# for lme / gls (returns reStruct / corStruct), so we go through
# nlme::getData() to find the response column.
.extract_dv_label_nlme <- function(fit, dv) {
  tryCatch(
    {
      d <- nlme::getData(fit)
      if (is.null(d) || !(dv %in% names(d))) {
        return(dv)
      }
      lab <- attr(d[[dv]], "label")
      if (is.character(lab) && length(lab) == 1L && nzchar(lab)) lab else dv
    },
    error = function(e) dv
  )
}
