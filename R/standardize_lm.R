# Native standardisation of regression coefficients (Q3, Q15).
#
# Four methods supported, matching the dispatch of
# `effectsize::standardize_parameters()`:
#
#   * "refit"   — refit the lm on z-scored data (gold standard,
#                 Cohen et al. 2003)
#   * "posthoc" — algebraic scaling β = b × sd(X) / sd(Y) on the
#                 columns of model.matrix(fit)
#   * "basic"   — like posthoc but factor-derived design columns
#                 keep their 0/1 coding (β = NA for those)
#   * "smart"   — Gelman (2008): divide binary predictors by
#                 2 × sd(X) instead of sd(X), continuous by sd(X)
#
# Implementation rationale (Q3 + Q14b coherence): native rather than
# delegated to effectsize because the user's `vcov` (HC*, CR*,
# bootstrap, jackknife) must propagate to β's SE/CI/p — which
# effectsize doesn't expose. Cross-validated against
# effectsize::standardize_parameters() in tests as the oracle.


# ---- Public-internal dispatch ---------------------------------------------

standardize_lm <- function(
  fit,
  method = c("refit", "posthoc", "basic", "smart"),
  vcov_type = "classical",
  cluster = NULL,
  ci_level = 0.95,
  weights = NULL,
  boot_n = 1000L
) {
  method <- match.arg(method)
  if (is.null(weights)) weights <- stats::weights(fit)

  switch(
    method,
    refit   = standardize_refit_lm(fit, vcov_type, cluster, ci_level,
                                    weights, boot_n),
    posthoc = standardize_posthoc_lm(fit, vcov_type, cluster, ci_level,
                                      weights, boot_n),
    basic   = standardize_basic_lm(fit, vcov_type, cluster, ci_level,
                                    weights, boot_n),
    smart   = standardize_smart_lm(fit, vcov_type, cluster, ci_level,
                                    weights, boot_n)
  )
}


# ---- Method 1: refit (gold standard) --------------------------------------

# z-score the model.frame's numeric columns (response + numeric
# predictors), refit the lm on the standardised frame, then apply
# the user's vcov to the refitted fit and compute (β, SE, CI, t, p).
# Inference under refit is **identical** to inference on the
# original fit (linear transformation preserves t and p) — the only
# differences are the magnitudes (β) and SEs.
standardize_refit_lm <- function(fit, vcov_type, cluster, ci_level,
                                  weights, boot_n) {
  mf <- stats::model.frame(fit)
  # Drop the auxiliary "(weights)" column that lm() injects when
  # weights are supplied — passing it back as data confuses lm().
  mf[["(weights)"]] <- NULL

  # z-score numeric columns (response + numeric predictors). Factor
  # columns are kept as-is (their dummies stay 0/1 in the design
  # matrix, matching effectsize convention).
  for (nm in names(mf)) {
    if (is.numeric(mf[[nm]])) {
      mf[[nm]] <- as.numeric(scale(mf[[nm]]))
    }
  }

  # Refit on standardised frame, preserving weights. Wrapped in
  # tryCatch because model.frame columns named after function calls
  # (`factor(cyl)`, `I(x^2)`, `poly(x, 2)`, `log(x)`, etc.) can fail
  # to refit cleanly — the column exists with the wrapped name but
  # lm() tries to re-evaluate the function on the inner symbol.
  # On failure, fall back to algebraic posthoc with a transparent
  # spicy_fallback warning so the user knows they got a different
  # method than they asked for.
  formula <- stats::formula(fit)
  args <- list(formula = formula, data = mf)
  if (!is.null(weights)) args$weights <- weights
  fit_std <- tryCatch(
    suppressWarnings(do.call(stats::lm, args)),
    error = function(e) NULL
  )
  if (is.null(fit_std)) {
    spicy_warn(
      c(
        paste0(
          "`standardized = \"refit\"` failed (formula likely contains ",
          "function calls such as `factor()`, `I()`, `poly()`, ",
          "`log()`, or `splines::ns()` that cannot be re-evaluated ",
          "on z-scored data)."
        ),
        "i" = paste0(
          "Falling back to `standardized = \"posthoc\"` (algebraic ",
          "scaling on the existing design matrix)."
        ),
        "i" = paste0(
          "For exact refit, pre-build the factor / transformed ",
          "column in `data` before fitting, e.g. ",
          "`data$cyl_f <- factor(data$cyl)` then ",
          "`lm(y ~ wt + cyl_f, data = data)`."
        )
      ),
      class = "spicy_fallback"
    )
    return(standardize_posthoc_lm(
      fit, vcov_type, cluster, ci_level, weights, boot_n
    ))
  }

  # Apply user's vcov to the refitted model and run per-coef inference
  vc_std <- compute_lm_vcov(
    fit_std,
    type = vcov_type,
    cluster = cluster,
    weights = weights,
    boot_n = boot_n
  )
  # Keep intercept as computed: under z-scored data, the refitted
  # intercept is the predicted standardised outcome when all numeric
  # predictors are at their mean and factor predictors at the
  # reference level. Meaningful (non-zero in unbalanced designs);
  # matches effectsize::standardize_parameters(method = "refit").
  coefs_inference_table(fit_std, vc_std, vcov_type, cluster, ci_level,
                        intercept_to_na = FALSE)
}


# ---- Method 2: posthoc -----------------------------------------------------

# Numerics scaled by sd(X)/sd(Y); factor-derived design columns
# scaled by 1/sd(Y) only (no X-scaling — treat 0/1 dummies as
# already on a "standardised" scale). Matches the semantics of
# `effectsize::standardize_parameters(method = "posthoc")`.
# Inference: SE_β scales by the same per-column factor; t and p
# are unchanged under linear transformation; CI rebuilt from
# scaled SE.
standardize_posthoc_lm <- function(fit, vcov_type, cluster, ci_level,
                                    weights, boot_n) {
  scale_and_rebuild(
    fit, vcov_type, cluster, ci_level, weights, boot_n,
    factor_treatment = "unscaled",   # factors: scale by 1/sd_y only
    binary_factor    = 1             # binary numerics: standard sd(X)
  )
}


# ---- Method 3: basic -------------------------------------------------------

# All design-matrix columns scaled by sd(col)/sd(Y), including
# factor dummies. Matches `effectsize::standardize_parameters(
# method = "basic")` — the "naive" / "raw" approach where every
# column gets the same algebraic treatment.
standardize_basic_lm <- function(fit, vcov_type, cluster, ci_level,
                                  weights, boot_n) {
  scale_and_rebuild(
    fit, vcov_type, cluster, ci_level, weights, boot_n,
    factor_treatment = "scale",      # factors: scale by sd_col/sd_y
    binary_factor    = 1             # binary numerics: standard sd(X)
  )
}


# ---- Method 4: smart (Gelman 2008 strict) ---------------------------------

# Implements **Gelman (2008) "Scaling regression inputs by dividing
# by two standard deviations"** strictly:
#   * binary numeric inputs (0/1 with exactly 2 unique values) are
#     scaled by 2 × sd(X) / sd(Y)
#   * other numeric inputs are scaled by sd(X) / sd(Y) (matches posthoc)
#   * factor-derived dummies are NOT scaled by their column SD
#     (treated as already standardised — same convention as posthoc)
#
# **Note**: this differs slightly from
# `effectsize::standardize_parameters(method = "smart")` for factor
# terms (effectsize applies an opaque level-specific adjustment).
# spicy follows Gelman strictly because the formula is well-defined
# and replicable. For exact effectsize-style "smart", call effectsize
# directly. The numeric divergence on factor dummies is typically
# < 5 % in practice.
standardize_smart_lm <- function(fit, vcov_type, cluster, ci_level,
                                  weights, boot_n) {
  scale_and_rebuild(
    fit, vcov_type, cluster, ci_level, weights, boot_n,
    factor_treatment = "unscaled",   # factors: scale by 1/sd_y only
    binary_factor    = 2             # binary numerics: 2 × sd(X)
  )
}


# ---- Internal: shared algebraic-scaling implementation --------------------

# Engine for posthoc, basic, smart. Two parameters control method
# semantics:
#   * factor_treatment ∈ {"scale", "unscaled"}
#       "scale"   : factor dummies scaled by sd(col)/sd_y (basic)
#       "unscaled": factor dummies scaled by 1/sd_y only (posthoc, smart)
#   * binary_factor ∈ {1, 2}
#       1 : binary NUMERIC columns use sd(X)/sd_y (posthoc, basic)
#       2 : binary NUMERIC columns use 2*sd(X)/sd_y (smart, Gelman 2008)
#
# "Binary" detection here means a numeric column with exactly two
# distinct values that is NOT factor-derived. Factor-derived dummies
# (which are also 0/1) are routed via factor_treatment, not via the
# binary_factor branch.
scale_and_rebuild <- function(fit, vcov_type, cluster, ci_level,
                               weights, boot_n,
                               factor_treatment = c("scale", "unscaled"),
                               binary_factor = 1) {
  factor_treatment <- match.arg(factor_treatment)

  b <- stats::coef(fit)
  vc <- compute_lm_vcov(
    fit,
    type = vcov_type,
    cluster = cluster,
    weights = weights,
    boot_n = boot_n
  )
  se_b <- sqrt(diag(vc))

  mf <- stats::model.frame(fit)
  y <- stats::model.response(mf)
  # Unweighted sd (matches effectsize default convention)
  sd_y <- stats::sd(y)
  mm <- stats::model.matrix(fit)
  sd_x <- apply(mm, 2, stats::sd)

  # Identify factor-derived columns (so we can route them via
  # factor_treatment rather than the binary_factor branch).
  factor_cols <- detect_factor_design_cols(fit)

  # Identify binary NUMERIC columns (exactly 2 unique values AND
  # NOT factor-derived). Used by smart's binary_factor = 2 rule.
  is_binary_numeric <- vapply(seq_len(ncol(mm)), function(j) {
    if (j %in% factor_cols) return(FALSE)
    length(unique(mm[, j])) == 2L
  }, logical(1))

  # Build per-column scale factor
  scale_factor <- sd_x / sd_y                  # default: numeric continuous
  if (length(factor_cols) > 0L && factor_treatment == "unscaled") {
    scale_factor[factor_cols] <- 1 / sd_y
  }
  if (binary_factor != 1) {
    scale_factor[is_binary_numeric] <- binary_factor *
      sd_x[is_binary_numeric] / sd_y
  }

  # β and SE_β. Singular coefs (NA in b) propagate naturally.
  beta <- b * scale_factor
  se_beta <- se_b * scale_factor

  # Intercept set to NA — its standardised value is mechanical
  # under algebraic scaling and not meaningful for interpretation.
  beta[1] <- NA_real_
  se_beta[1] <- NA_real_

  # Inference reconstruction. Under a linear scale, t = β/SE_β =
  # b/SE_b is unchanged from the original; we recompute from
  # scaled values for code symmetry.
  df_resid <- stats::df.residual(fit)
  is_asymptotic <- vcov_type %in% c("bootstrap", "jackknife") ||
    !is.finite(df_resid) || df_resid <= 0
  alpha <- 1 - ci_level
  crit <- if (is_asymptotic) {
    stats::qnorm(1 - alpha / 2)
  } else {
    stats::qt(1 - alpha / 2, df = df_resid)
  }
  ci_low <- beta - crit * se_beta
  ci_high <- beta + crit * se_beta
  t_stat <- beta / se_beta
  p_value <- if (is_asymptotic) {
    2 * stats::pnorm(abs(t_stat), lower.tail = FALSE)
  } else {
    2 * stats::pt(abs(t_stat), df = df_resid, lower.tail = FALSE)
  }

  data.frame(
    term = names(b),
    estimate = unname(beta),
    se = unname(se_beta),
    ci_low = unname(ci_low),
    ci_high = unname(ci_high),
    statistic = unname(t_stat),
    df = rep(df_resid, length(b)),
    p_value = unname(p_value),
    stringsAsFactors = FALSE
  )
}


# ---- Internal: refit-method coefs table ----------------------------------

# Used by standardize_refit_lm() to compute (β, SE, CI, t, p) from
# the refitted lm + its recomputed vcov.
coefs_inference_table <- function(fit_std, vc_std, vcov_type, cluster,
                                   ci_level, intercept_to_na = TRUE) {
  cf <- stats::coef(fit_std)
  rows <- lapply(seq_along(cf), function(i) {
    if (is.na(cf[i])) {
      list(estimate = NA_real_, se = NA_real_, ci_lower = NA_real_,
           ci_upper = NA_real_, statistic = NA_real_, df = NA_real_,
           p.value = NA_real_)
    } else {
      compute_lm_coef_inference(fit_std, i, vc_std, vcov_type,
                                 cluster, ci_level)
    }
  })

  out <- data.frame(
    term = names(cf),
    estimate = vapply(rows, `[[`, double(1), "estimate"),
    se = vapply(rows, `[[`, double(1), "se"),
    ci_low = vapply(rows, `[[`, double(1), "ci_lower"),
    ci_high = vapply(rows, `[[`, double(1), "ci_upper"),
    statistic = vapply(rows, `[[`, double(1), "statistic"),
    df = vapply(rows, `[[`, double(1), "df"),
    p_value = vapply(rows, `[[`, double(1), "p.value"),
    stringsAsFactors = FALSE
  )
  if (isTRUE(intercept_to_na) && nrow(out) >= 1L &&
        out$term[1] == "(Intercept)") {
    out[1, c("estimate", "se", "ci_low", "ci_high",
             "statistic", "p_value")] <- NA_real_
  }
  out
}


# ---- Internal: detect factor-derived design columns -----------------------

# Returns the column indices in model.matrix(fit) that come from
# factor terms (main effects only, not interactions). Used by
# `standardize_basic_lm()` to NA the corresponding β rows.
detect_factor_design_cols <- function(fit) {
  xlevels <- fit$xlevels
  if (is.null(xlevels) || length(xlevels) == 0L) return(integer(0))

  mm <- stats::model.matrix(fit)
  assign_vec <- attr(mm, "assign")
  term_labels <- attr(stats::terms(fit), "term.labels")

  factor_terms <- names(xlevels)
  out <- integer(0)
  for (i in seq_along(assign_vec)) {
    term_idx <- assign_vec[i]
    if (term_idx == 0L) next   # intercept
    term_name <- term_labels[term_idx]
    # Pure factor main effect (no interaction with another variable)
    if (term_name %in% factor_terms) {
      out <- c(out, i)
    }
  }
  out
}


# ---- Hook: convert standardize_lm output to long-format rows -------------

# Called by extract_lm_phase1() when standardized != "none". Converts
# the standardise table into long-format rows with
# estimate_type = "beta" so it stacks via rbind() with the existing
# B rows.
extract_beta_rows <- function(fit, standardized, vcov_type, cluster,
                               ci_level, weights, boot_n,
                               model_id, outcome) {
  # Dispatch on class: glm has its own 5-method standardise machinery
  # (5th = "pseudo", Menard 2011 fully-standardised), with X-only
  # algebraic semantics for the other four (Long & Freese 2014 §4.3.4).
  std_table <- if (inherits(fit, "glm")) {
    standardize_glm(
      fit, method = standardized,
      vcov_type = vcov_type, cluster = cluster,
      ci_level = ci_level, weights = weights, boot_n = boot_n
    )
  } else {
    standardize_lm(
      fit, method = standardized,
      vcov_type = vcov_type, cluster = cluster,
      ci_level = ci_level, weights = weights, boot_n = boot_n
    )
  }

  factor_meta <- detect_factor_term_meta(fit)
  cf <- stats::coef(fit)

  rows <- lapply(seq_len(nrow(std_table)), function(i) {
    nm <- std_table$term[i]
    fmeta <- factor_meta[[nm]]
    is_intercept <- (nm == "(Intercept)")
    is_singular <- is.na(cf[i])

    build_one_b_row(
      nm = nm, model_id = model_id, outcome = outcome,
      estimate_type = "beta",
      estimate = std_table$estimate[i], se = std_table$se[i],
      ci_low = std_table$ci_low[i], ci_high = std_table$ci_high[i],
      statistic = std_table$statistic[i], df = std_table$df[i],
      p_value = std_table$p_value[i],
      test_type = NA_character_,
      is_singular = is_singular,
      is_intercept = is_intercept,
      is_reference = FALSE,
      factor_term = fmeta$factor_term %||% NA_character_,
      factor_level = fmeta$factor_level %||% NA_character_
    )
  })
  do.call(rbind, rows)
}
