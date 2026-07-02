# Native standardisation of regression coefficients (Q3, Q15).
#
# Four methods supported, matching the dispatch of
# `effectsize::standardize_parameters()`:
#
#   * "refit"   -- refit the lm on z-scored data (gold standard,
#                 Cohen et al. 2003)
#   * "posthoc" -- algebraic scaling beta = b x sd(X) / sd(Y) on the
#                 columns of model.matrix(fit)
#   * "basic"   -- like posthoc but factor-derived design columns
#                 keep their 0/1 coding (beta = NA for those)
#   * "smart"   -- Gelman (2008): divide binary predictors by
#                 2 x sd(X) instead of sd(X), continuous by sd(X)
#
# Implementation rationale (Q3 + Q14b coherence): native rather than
# delegated to effectsize because the user's `vcov` (HC*, CR*,
# bootstrap, jackknife) must propagate to beta's SE/CI/p -- which
# effectsize doesn't expose. Cross-validated against
# effectsize::standardize_parameters() in tests as the oracle.


# ---- Shared helper ---------------------------------------------------------

# Standardisation refits run on the model frame, whose columns are the
# *evaluated* terms (`log(x)`, `factor(g)`, `poly(x, 2)`, ...). The refit
# formula must therefore resolve variables from `data` only: with the
# original formula environment attached, an inline transform silently
# re-evaluates against whatever the caller's environment happens to hold
# (raw, unscaled vectors), yielding wrong betas with no warning. Stripping
# the environment turns that into an error that lands in the classed
# spicy_fallback paths.
strip_formula_env <- function(formula) {
  environment(formula) <- new.env(parent = baseenv())
  formula
}


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

  out <- switch(
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
  # Record the method ACTUALLY applied (the refit path may fall back to
  # posthoc, setting the attribute itself) so the footer can disclose a
  # fallback instead of printing the refit wording over posthoc numbers.
  if (is.null(attr(out, "used_method"))) attr(out, "used_method") <- method
  out
}


# ---- Method 1: refit (gold standard) --------------------------------------

# z-score the model.frame's numeric columns (response + numeric
# predictors), refit the lm on the standardised frame, then apply
# the user's vcov to the refitted fit and compute (beta, SE, CI, t, p).
# Inference under refit is **identical** to inference on the
# original fit (linear transformation preserves t and p) -- the only
# differences are the magnitudes (beta) and SEs.
standardize_refit_lm <- function(fit, vcov_type, cluster, ci_level,
                                  weights, boot_n) {
  mf <- stats::model.frame(fit)
  # Drop the auxiliary "(weights)" column that lm() injects when
  # weights are supplied -- passing it back as data confuses lm().
  mf[["(weights)"]] <- NULL

  # z-score numeric columns (response + numeric predictors). Factor
  # columns are kept as-is (their dummies stay 0/1 in the design
  # matrix, matching effectsize convention). Matrix-valued columns
  # (`poly(x, 2)`, `splines::ns(x, 3)` bases, `cbind()` responses) are
  # left untouched: scaling them column-blind corrupts the frame, and
  # the refit below then declines into the posthoc fallback instead of
  # hard-crashing here.
  for (nm in names(mf)) {
    if (is.numeric(mf[[nm]]) && is.null(dim(mf[[nm]]))) {
      mf[[nm]] <- as.numeric(scale(mf[[nm]]))
    }
  }

  # Refit on standardised frame, preserving weights. The formula
  # environment is stripped so variables resolve from `mf` ONLY:
  # model.frame columns named after function calls (`factor(cyl)`,
  # `I(x^2)`, `poly(x, 2)`, `log(x)`, etc.) cannot re-evaluate -- lm()
  # tries the function on the inner symbol, which must fail here rather
  # than silently pick up the caller's raw unscaled vector. On failure,
  # fall back to algebraic posthoc with a transparent spicy_fallback
  # warning so the user knows they got a different method than they
  # asked for.
  formula <- strip_formula_env(stats::formula(fit))
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
    out <- standardize_posthoc_lm(
      fit, vcov_type, cluster, ci_level, weights, boot_n
    )
    attr(out, "used_method") <- "posthoc"
    return(out)
  }

  # Apply user's vcov to the refitted model and run per-coef inference
  vc_std <- compute_model_vcov(
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
# scaled by 1/sd(Y) only (no X-scaling -- treat 0/1 dummies as
# already on a "standardised" scale). Matches
# `effectsize::standardize_parameters(method = "posthoc")` on ADDITIVE
# terms only: on interaction / transformed design columns this scales by
# the SD of the PRODUCT column itself -- the SPSS beta / Stata
# `regress, beta` / SAS PROC REG STB / `lm.beta` / effectsize "basic"
# convention (effectsize "posthoc" z-scores the components instead;
# Friedrich 1982 documents why the two differ when components are
# correlated). Cross-validated against lm.beta / effectsize "basic" at
# 1e-7 on interaction rows.
# Inference: SE_beta scales by the same per-column factor; t and p
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
# method = "basic")` -- the "naive" / "raw" approach where every
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
#     scaled by 2 x sd(X) / sd(Y)
#   * other numeric inputs are scaled by sd(X) / sd(Y) (matches posthoc)
#   * factor-derived dummies are NOT scaled by their column SD
#     (treated as already standardised -- same convention as posthoc)
#
# **Note**: this differs slightly from
# `effectsize::standardize_parameters(method = "smart")` for factor
# terms (effectsize applies an opaque level-specific adjustment).
# spicy follows Gelman strictly because the formula is well-defined
# and replicable. For exact effectsize-style "smart", call effectsize
# directly. The numeric divergence on factor dummies is typically
# < 5 % in practice.
#
# Interaction / transformed design columns follow the same
# product-column convention as posthoc (SD of the column itself, not
# the components) -- and the binary rule applies to the PRODUCT column:
# a binary x binary interaction column is itself 0/1, so it gets the
# 2 x sd rule (verified: smart == 2 x posthoc on that row).
standardize_smart_lm <- function(fit, vcov_type, cluster, ci_level,
                                  weights, boot_n) {
  scale_and_rebuild(
    fit, vcov_type, cluster, ci_level, weights, boot_n,
    factor_treatment = "unscaled",   # factors: scale by 1/sd_y only
    binary_factor    = 2             # binary numerics: 2 x sd(X)
  )
}


# ---- Internal: shared algebraic-scaling implementation --------------------

# Engine for posthoc, basic, smart. Two parameters control method
# semantics:
#   * factor_treatment in {"scale", "unscaled"}
#       "scale"   : factor dummies scaled by sd(col)/sd_y (basic)
#       "unscaled": factor dummies scaled by 1/sd_y only (posthoc, smart)
#   * binary_factor in {1, 2}
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
  vc <- compute_model_vcov(
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

  # beta and SE_beta. Singular coefs (NA in b) propagate naturally.
  beta <- b * scale_factor
  se_beta <- se_b * scale_factor

  # Intercept set to NA -- its standardised value is mechanical
  # under algebraic scaling and not meaningful for interpretation.
  beta[1] <- NA_real_
  se_beta[1] <- NA_real_

  # Inference reconstruction. Under a linear scale, t = beta/SE_beta =
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

# Used by standardize_refit_lm() to compute (beta, SE, CI, t, p) from
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
      compute_coef_inference(fit_std, i, vc_std, vcov_type,
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
# `standardize_basic_lm()` to NA the corresponding beta rows.
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
  # algebraic semantics for the other four (Long & Freese 2014 Section 4.3.4).
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
      factor_level = fmeta$factor_level %||% NA_character_,
      factor_level_pos = fmeta$factor_level_pos %||% NA_integer_
    )
  })
  out <- do.call(rbind, rows)
  # Thread the method actually applied (refit may fall back to posthoc)
  # up to the extract so the footer can disclose the fallback.
  attr(out, "used_method") <- attr(std_table, "used_method")
  out
}


# ---- Phase 7c20: standardised betas for mixed-effects fits --------------
#
# Refit method only (the gold standard of Cohen et al. 2003 + Gelman 2008
# for mixed-effects too): z-score the numeric columns of the model frame,
# rebuild the same fit with the original engine + random structure, then
# extract fixed-effect coefficients in the frame schema with
# estimate_type = "beta".
#
# The posthoc / basic / smart algebraic rescalings used by the lm path
# don't transfer cleanly to mixed-effects because sd(Y) is a marginal
# quantity whose decomposition into between- vs within-cluster variance
# is ambiguous; refit avoids the ambiguity by re-estimating the model
# on the standardised data, leaving the random structure to absorb its
# share of variance.
#
# Returns a list(coefs_beta = data.frame, used_method = "refit") or NULL
# when the refit fails (e.g. formula contains an inline function call
# that can't re-evaluate on z-scored data).
.compute_beta_rows_for_mixed <- function(fit, ci_level = 0.95) {
  # Pull the model frame and z-score numeric columns. Factor / character
  # / logical columns are kept as-is (their dummies stay 0/1 in the
  # design matrix, matching the lm path's convention).
  data <- tryCatch(
    if (inherits(fit, c("lme", "gls"))) {
      nlme::getData(fit)
    } else {
      stats::model.frame(fit)
    },
    error = function(e) NULL
  )
  if (is.null(data)) return(NULL)  # nocov (model.frame/getData errors on real fits)

  # Identify the response variable -- non-Gaussian families (binomial,
  # poisson, ...) require y bounded / integer-valued, so we standardise
  # PREDICTORS ONLY for them. Gaussian / identity-link standardises
  # both response and predictors (Cohen et al. 2003 / Gelman 2008).
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  is_gaussian <- is.null(fam) || identical(fam$family, "gaussian")
  response_var <- tryCatch(
    as.character(stats::formula(fit)[[2L]]),
    error = function(e) NA_character_
  )[1L]

  for (nm in names(data)) {
    if (!is.numeric(data[[nm]]) || !is.null(dim(data[[nm]])) ||
        startsWith(nm, "(")) {
      next  # skip matrix columns (poly/ns bases, cbind() responses)
    }
    if (!is_gaussian && identical(nm, response_var)) next  # skip y on glmm
    sd_x <- stats::sd(data[[nm]], na.rm = TRUE)
    if (is.finite(sd_x) && sd_x > 0) {
      data[[nm]] <- as.numeric(scale(data[[nm]]))
    }
  }

  # lme refits on the RAW data (nlme::getData), so an inline transform
  # in the fixed formula would re-evaluate on the z-scored raw column
  # (log of a z-scored variable, ...) -- a different model, silently.
  # Decline instead; the caller warns and omits the beta rows.
  if (inherits(fit, c("lme", "gls"))) {
    vars <- tryCatch(as.list(attr(stats::terms(fit), "variables"))[-1L],
                     error = function(e) NULL)
    if (is.null(vars) || !all(vapply(vars, is.symbol, logical(1)))) {
      return(NULL)
    }
  }

  # Engine-specific refit, manually requalified so the call works in
  # any context (the captured `getCall(fit)` would fail to resolve
  # `lmer` / `glmer` / `glmmTMB` / `lme` when those packages aren't
  # attached). lme4 fits are S4 -- use stats::getCall(), not `fit$call`.
  # The call is normalised with match.call() so the model formula can be
  # replaced by name with an environment-stripped copy: for the
  # model-frame engines (lmer / glmer / glmmTMB) the data columns are
  # the *evaluated* terms, and an intact formula environment lets an
  # inline transform silently re-evaluate against the caller's raw
  # unscaled vectors instead of erroring into the warned fallback.
  fit_std <- tryCatch({
    call_copy <- stats::getCall(fit)
    if (is.null(call_copy)) return(NULL)  # nocov (real fits always carry a call)
    if (inherits(fit, "lmerModLmerTest") || inherits(fit, "lmerMod")) {
      call_copy[[1L]] <- quote(lme4::lmer)
      call_copy <- match.call(lme4::lmer, call_copy)
      call_copy$formula <- strip_formula_env(stats::formula(fit))
    } else if (inherits(fit, "glmerMod")) {
      call_copy[[1L]] <- quote(lme4::glmer)
      call_copy <- match.call(lme4::glmer, call_copy)
      call_copy$formula <- strip_formula_env(stats::formula(fit))
    } else if (inherits(fit, "glmmTMB")) {
      call_copy[[1L]] <- quote(glmmTMB::glmmTMB)
      call_copy <- match.call(glmmTMB::glmmTMB, call_copy)
      call_copy$formula <- strip_formula_env(stats::formula(fit))
    } else if (inherits(fit, "lme")) {
      call_copy[[1L]] <- quote(nlme::lme)
      call_copy <- match.call(nlme::lme, call_copy)
      call_copy$fixed <- strip_formula_env(stats::formula(fit))
    } else {
      # nocov start (only the 4 dispatched engines ever reach here)
      return(NULL)
      # nocov end
    }
    call_copy$data <- data
    suppressMessages(suppressWarnings(eval(call_copy, envir = parent.frame())))
  }, error = function(e) NULL)
  if (is.null(fit_std)) return(NULL)

  # Extract fixef + vcov from the refitted model (engine-aware), then
  # build frame-schema rows with estimate_type = "beta". Inference is
  # invariant under linear transformation so the z / p of the beta rows
  # equal those of the corresponding B rows on the original fit.
  bhat <- tryCatch(
    if (inherits(fit_std, "glmmTMB")) {
      cond <- glmmTMB::fixef(fit_std)$cond
      setNames(as.numeric(cond), names(cond))
    } else if (inherits(fit_std, c("lmerMod", "glmerMod"))) {
      lme4::fixef(fit_std)
    } else {
      nlme::fixef(fit_std)
    },
    error = function(e) NULL
  )
  if (is.null(bhat) || length(bhat) == 0L) return(NULL)  # nocov (fixef extraction never errors/empties on a converged refit)

  V <- tryCatch({
    v <- stats::vcov(fit_std)
    if (inherits(fit_std, "glmmTMB")) {
      v_cond <- tryCatch(v$cond, error = function(e) NULL)
      # nocov start (secondary fallback; `v$cond` succeeds for real glmmTMB vcov)
      if (is.null(v_cond)) v_cond <- tryCatch(v[["cond"]],
                                                error = function(e) NULL)
      # nocov end
      if (!is.null(v_cond)) v <- v_cond
    }
    as.matrix(v)
  }, error = function(e) NULL)
  if (is.null(V) || nrow(V) != length(bhat)) return(NULL)  # nocov (vcov is square + conformable on a converged refit)

  se <- sqrt(diag(V))
  z_crit <- stats::qnorm(0.5 + ci_level / 2)
  factor_meta <- tryCatch(detect_factor_term_meta(fit),
                           error = function(e) list())

  nm <- names(bhat)
  rows <- lapply(seq_along(bhat), function(i) {
    fmeta <- factor_meta[[nm[i]]]
    parent_var <- fmeta$factor_term  %||% nm[i]
    label      <- fmeta$factor_level %||% nm[i]
    pos        <- fmeta$factor_level_pos %||% NA_integer_

    stat <- bhat[i] / se[i]
    p    <- 2 * stats::pnorm(-abs(stat))

    data.frame(
      term             = nm[i],
      parent_var       = parent_var,
      label            = label,
      factor_level_pos = as.integer(pos),
      is_ref           = FALSE,
      estimate_type    = "beta",
      estimate         = as.numeric(bhat[i]),
      std_error        = as.numeric(se[i]),
      df               = Inf,
      statistic        = as.numeric(stat),
      p_value          = as.numeric(p),
      ci_lower         = as.numeric(bhat[i] - z_crit * se[i]),
      ci_upper         = as.numeric(bhat[i] + z_crit * se[i]),
      test_type        = "z",
      stringsAsFactors = FALSE
    )
  })
  list(coefs_beta = do.call(rbind, rows), used_method = "refit")
}


# Attach standardised beta rows to a frame's coefs when `standardized
# != "none"`. No-op otherwise (no token requested OR no compatible
# engine). All four mixed-effects methods (lmer / glmer / glmmTMB /
# lme) currently support only the "refit" method; other choices fall
# back to refit with a spicy_fallback warning.
.attach_beta_to_frame_coefs <- function(coefs, fit, standardized,
                                          ci_level = 0.95) {
  if (identical(standardized, "none") || is.null(standardized)) {
    return(coefs)
  }
  if (!identical(standardized, "refit")) {
    spicy_warn(
      c(
        sprintf(
          paste0("`standardized = \"%s\"` is not yet implemented for ",
                  "mixed-effects fits; falling back to \"refit\"."),
          standardized
        ),
        "i" = paste0(
          "Refit is the Cohen et al. 2003 / Gelman 2008 gold standard ",
          "and avoids the ambiguous sd(Y) decomposition that the ",
          "algebraic methods rely on."
        )
      ),
      class = "spicy_fallback"
    )
  }
  res <- .compute_beta_rows_for_mixed(fit, ci_level = ci_level)
  if (is.null(res)) {
    spicy_warn(
      c(
        paste0("Standardised coefficients unavailable: the refit on ",
               "z-scored data could not be built for this fit."),
        "i" = paste0(
          "Formulas with inline function calls (`log(x)`, `poly(x, 2)`, ",
          "...) cannot be re-evaluated on z-scored data. Pre-build the ",
          "transformed column in `data` before fitting."
        ),
        "i" = "The table shows unstandardised coefficients only."
      ),
      class = "spicy_fallback"
    )
    return(coefs)
  }
  beta_rows <- res$coefs_beta
  for (col in setdiff(colnames(coefs), colnames(beta_rows))) {
    beta_rows[[col]] <- coefs[[col]][NA_integer_]  # nocov (beta_rows already carries the full coefs schema)
  }
  beta_rows <- beta_rows[, colnames(coefs), drop = FALSE]
  rbind(coefs, beta_rows)
}
