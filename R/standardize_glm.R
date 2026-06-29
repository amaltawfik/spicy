# Native standardisation of regression coefficients for `glm` fits.
#
# Five methods, mirroring the lm dispatch + adding `"pseudo"`:
#
#   * "refit"   -- refit the glm on z-scored numeric predictors. Y stays
#                 on its observed scale (the link is fixed by the
#                 family); inference comes from the refitted glm via
#                 the user's vcov. Long & Freese (2014, \u00A74.3.4)
#                 "x-standardization".
#   * "posthoc" -- algebraic X-only scaling: \u03B2 = b x SD(X). Factor
#                 dummies keep 0/1 (no SD division). Matches
#                 `effectsize::standardize_parameters(method = "posthoc")`.
#   * "basic"   -- like posthoc but factor-derived design columns are
#                 scaled by SD(column) (treated as numeric).
#   * "smart"   -- Gelman (2008): binary numeric inputs scaled by
#                 2 x SD(X); other numerics by SD(X); factor dummies
#                 unchanged. X-only.
#   * "pseudo"  -- Menard (2004, 2011) **fully** standardised:
#                 \u03B2 = b x SD(X) / SD(Y*)
#                 where Y* is the latent variable on the link scale and
#                   SD(Y*) = sqrt(var(eta^) + var_link)
#                   eta^        : linear predictor (predict, type = "link")
#                   var_link  : logit  -> pi^2/3   (~ 3.290)
#                               probit -> 1
#                               cloglog-> pi^2/6   (Gumbel)
#                               other  -> NA (returns all-NA \u03B2)
#                 Defined for binomial families; non-binomial returns
#                 NA-vector with a `spicy_caveat` warning.
#
# Inference under all algebraic methods is exact: the t (z) statistic
# and p-value are invariant under linear rescaling. CIs are recomputed
# from the scaled SE using the same critical value as the unscaled
# inference (z for glm -- df = Inf -- matching `compute_glm_coef_inference`).


# ---- Public-internal dispatch --------------------------------------------

standardize_glm <- function(
  fit,
  method = c("refit", "posthoc", "basic", "smart", "pseudo"),
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
    refit   = standardize_refit_glm(fit, vcov_type, cluster, ci_level,
                                     weights, boot_n),
    posthoc = standardize_algebraic_glm(
                fit, vcov_type, cluster, ci_level, weights, boot_n,
                factor_treatment = "unscaled", binary_factor = 1,
                sd_y_div = 1
              ),
    basic   = standardize_algebraic_glm(
                fit, vcov_type, cluster, ci_level, weights, boot_n,
                factor_treatment = "scale", binary_factor = 1,
                sd_y_div = 1
              ),
    smart   = standardize_algebraic_glm(
                fit, vcov_type, cluster, ci_level, weights, boot_n,
                factor_treatment = "unscaled", binary_factor = 2,
                sd_y_div = 1
              ),
    pseudo  = standardize_pseudo_glm(fit, vcov_type, cluster, ci_level,
                                      weights, boot_n)
  )
}


# ---- Method 1: refit (Long & Freese x-standardization) -------------------

standardize_refit_glm <- function(fit, vcov_type, cluster, ci_level,
                                   weights, boot_n) {
  mf <- stats::model.frame(fit)
  mf[["(weights)"]] <- NULL

  # z-score numeric *predictors* only. The response stays on its
  # observed scale (the family / link is fixed and rescaling Y would
  # change the model).
  resp_name <- all.vars(stats::formula(fit)[[2L]])[1L]
  for (nm in names(mf)) {
    if (identical(nm, resp_name)) next
    if (is.numeric(mf[[nm]])) {
      mf[[nm]] <- as.numeric(scale(mf[[nm]]))
    }
  }

  formula <- stats::formula(fit)
  fam <- stats::family(fit)
  args <- list(formula = formula, family = fam, data = mf)
  if (!is.null(weights)) args$weights <- weights
  fit_std <- tryCatch(
    suppressWarnings(do.call(stats::glm, args)),
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
          "X-only scaling on the existing design matrix)."
        ),
        "i" = paste0(
          "For exact refit, pre-build the factor / transformed ",
          "column in `data` before fitting."
        )
      ),
      class = "spicy_fallback"
    )
    return(standardize_algebraic_glm(
      fit, vcov_type, cluster, ci_level, weights, boot_n,
      factor_treatment = "unscaled", binary_factor = 1, sd_y_div = 1
    ))
  }

  vc_std <- compute_lm_vcov(
    fit_std,
    type = vcov_type,
    cluster = cluster,
    weights = weights,
    boot_n = boot_n
  )
  glm_coefs_inference_table(fit_std, vc_std, vcov_type, cluster, ci_level,
                              intercept_to_na = FALSE)
}


# ---- Algebraic methods (posthoc / basic / smart / pseudo) ----------------

# Engine for the four algebraic methods. Three switches:
#   * factor_treatment in {"scale", "unscaled"}
#       "scale"   : factor dummies scaled by sd(col)         (basic)
#       "unscaled": factor dummies left unscaled (1)         (posthoc, smart, pseudo)
#   * binary_factor in {1, 2}
#       1 : binary NUMERIC columns use sd(X)                 (posthoc, basic, pseudo)
#       2 : binary NUMERIC columns use 2 x sd(X)             (smart, Gelman 2008)
#   * sd_y_div : positive scalar divisor applied to every \u03B2
#       1                       : X-only methods
#       sd(Y*) (Menard latent)  : pseudo
standardize_algebraic_glm <- function(fit, vcov_type, cluster, ci_level,
                                       weights, boot_n,
                                       factor_treatment = c("scale", "unscaled"),
                                       binary_factor = 1,
                                       sd_y_div = 1) {
  factor_treatment <- match.arg(factor_treatment)

  b <- stats::coef(fit)
  vc <- compute_lm_vcov(
    fit, type = vcov_type, cluster = cluster,
    weights = weights, boot_n = boot_n
  )
  se_b_raw <- sqrt(diag(vc))

  # Align the standard errors to the full coefficient vector by NAME.
  # `stats::coef(fit)` always returns the full length-p vector (with NA
  # entries for aliased / perfectly-collinear terms), and the design
  # matrix `model.matrix(fit)` is likewise full width p. The variance
  # matrix, however, may DROP aliased rows/columns: classical
  # `stats::vcov.glm()` pads them with NA (length p), but robust
  # estimators (`sandwich::vcovHC`, `clubSandwich::vcovCR`, ...) return a
  # reduced length-(p - k) matrix. Multiplying a length-(p - k) `se_b`
  # against the length-p `scale_factor` would silently recycle and
  # misalign every standardised SE. Reindex by name -> guaranteed length
  # p, with NA for any coefficient the vcov omitted (which then flows
  # through to an NA standardised SE for that aliased term only).
  se_b <- se_b_raw[names(b)]
  names(se_b) <- names(b)

  mm <- stats::model.matrix(fit)
  sd_x <- apply(mm, 2, stats::sd)

  factor_cols <- detect_factor_design_cols(fit)
  is_binary_numeric <- vapply(seq_len(ncol(mm)), function(j) {
    if (j %in% factor_cols) return(FALSE)
    length(unique(mm[, j])) == 2L
  }, logical(1))

  scale_factor <- sd_x / sd_y_div
  if (length(factor_cols) > 0L && factor_treatment == "unscaled") {
    scale_factor[factor_cols] <- 1 / sd_y_div
  }
  if (binary_factor != 1) {
    scale_factor[is_binary_numeric] <- binary_factor *
      sd_x[is_binary_numeric] / sd_y_div
  }

  beta <- b * scale_factor
  se_beta <- se_b * scale_factor

  # Intercept set to NA -- its standardised value is mechanical under
  # algebraic scaling and not meaningful for interpretation. Guard
  # against no-intercept models (`glm(y ~ -1 + x, ...)`): only NA the
  # first row when it actually IS the intercept.
  if (length(beta) >= 1L && identical(names(b)[1L], "(Intercept)")) {
    beta[1] <- NA_real_
    se_beta[1] <- NA_real_
  }

  # Inference reconstruction.
  #
  # Default: z-asymptotic (df = Inf) -- matches glm convention.
  #
  # CR* + cluster: Satterthwaite-corrected df via clubSandwich,
  # parallelling `compute_glm_coef_inference()` for the B row. Without
  # this, the beta row would silently revert to z while the B row uses
  # Satterthwaite, producing inconsistent CIs / p-values for what is
  # mathematically the same test (linear rescaling preserves the
  # statistic, so df / p / CI must follow the same regime).
  df_vec <- rep(Inf, length(b))
  use_satt <- startsWith(vcov_type, "CR") &&
                !is.null(cluster) &&
                spicy_pkg_available("clubSandwich")
  if (use_satt) {
    df_satt_map <- compute_satt_df_per_coef_glm(fit, vc, cluster)
    if (!is.null(df_satt_map)) {
      hits <- match(names(b), names(df_satt_map))
      df_vec[!is.na(hits)] <- df_satt_map[hits[!is.na(hits)]]
    }
  }
  alpha <- 1 - ci_level
  ci_low <- numeric(length(beta))
  ci_high <- numeric(length(beta))
  stat <- beta / se_beta
  p_value <- numeric(length(beta))
  for (i in seq_along(beta)) {
    df_i <- df_vec[i]
    if (is.finite(df_i) && df_i > 0) {
      crit_i <- stats::qt(1 - alpha / 2, df = df_i)
      p_value[i] <- 2 * stats::pt(abs(stat[i]), df = df_i, lower.tail = FALSE)
    } else {
      crit_i <- stats::qnorm(1 - alpha / 2)
      p_value[i] <- 2 * stats::pnorm(abs(stat[i]), lower.tail = FALSE)
    }
    ci_low[i]  <- beta[i] - crit_i * se_beta[i]
    ci_high[i] <- beta[i] + crit_i * se_beta[i]
  }

  data.frame(
    term = names(b),
    estimate = unname(beta),
    se = unname(se_beta),
    ci_low = unname(ci_low),
    ci_high = unname(ci_high),
    statistic = unname(stat),
    df = df_vec,
    p_value = unname(p_value),
    stringsAsFactors = FALSE
  )
}


# ---- Method 5: pseudo (Menard 2011 fully-standardised) -------------------

standardize_pseudo_glm <- function(fit, vcov_type, cluster, ci_level,
                                    weights, boot_n) {
  sd_y_star <- compute_menard_sd_y_star(fit)
  if (!is.finite(sd_y_star) || sd_y_star <= 0) {
    spicy_warn(
      c(
        paste0(
          "`standardized = \"pseudo\"` (Menard fully-standardised) is ",
          "defined for binomial families with logit / probit / cloglog ",
          "/ log links."
        ),
        "i" = sprintf(
          "Family `%s` (link `%s`) is outside this scope; \u03B2 returned as NA.",
          stats::family(fit)$family, stats::family(fit)$link
        ),
        "i" = paste0(
          "For non-binomial glms, use `standardized = \"refit\"` ",
          "(x-standardization, Long & Freese 2014 \u00A74.3.4) or one of ",
          "`\"posthoc\"` / `\"basic\"` / `\"smart\"` (X-only scaling)."
        )
      ),
      class = "spicy_caveat"
    )
    # Return an all-NA \u03B2 table so the renderer em-dashes the column.
    cf <- stats::coef(fit)
    return(data.frame(
      term = names(cf),
      estimate = rep(NA_real_, length(cf)),
      se = rep(NA_real_, length(cf)),
      ci_low = rep(NA_real_, length(cf)),
      ci_high = rep(NA_real_, length(cf)),
      statistic = rep(NA_real_, length(cf)),
      df = rep(Inf, length(cf)),
      p_value = rep(NA_real_, length(cf)),
      stringsAsFactors = FALSE
    ))
  }
  standardize_algebraic_glm(
    fit, vcov_type, cluster, ci_level, weights, boot_n,
    factor_treatment = "unscaled", binary_factor = 1,
    sd_y_div = sd_y_star
  )
}


# Latent-variable SD on the link scale (Menard 2011 eq. 6 / Long &
# Freese 2014 \u00A73.6). Defined for binomial families:
#   SD(Y*) = sqrt(var(eta^) + var_link)
# where eta^ is the linear predictor and var_link is the assumed variance
# of the latent residual under each link function (logit -> logistic
# distribution, pi^2/3; probit -> standard normal, 1; cloglog -> Gumbel,
# pi^2/6). Returns NA for non-binomial families (caller emits caveat).
compute_menard_sd_y_star <- function(fit) {
  fam <- stats::family(fit)
  if (!identical(fam$family, "binomial")) return(NA_real_)
  eta_hat <- tryCatch(
    as.numeric(stats::predict(fit, type = "link")),
    error = function(e) NULL   # nocov: predict(type="link") cannot error on
                               # a converged binomial glm; defensive only.
  )
  # `predict(type = "link")` is padded back to the ORIGINAL data length
  # when the fit used `na.action = na.exclude` (napredict() reinserts NA
  # at the dropped rows). The latent-variable SD must be computed over
  # the rows the model actually used, so strip those NA before taking the
  # variance -- otherwise a perfectly valid binomial fit would yield
  # SD(Y*) = NA and trigger the misleading "family outside scope" caveat.
  if (is.null(eta_hat)) return(NA_real_)   # nocov: NULL arm unreachable (see above).
  eta_hat <- eta_hat[is.finite(eta_hat)]
  if (length(eta_hat) < 2L) return(NA_real_)
  var_eta <- stats::var(eta_hat)
  var_link <- switch(
    fam$link,
    "logit"   = pi^2 / 3,
    "probit"  = 1,
    "cloglog" = pi^2 / 6,
    # The log link (log-binomial / relative-risk model) has NO latent-
    # threshold interpretation -- it models log(p) = X'beta multiplicatively,
    # not Y = 1{Y* > 0} with a latent residual distribution. The Menard /
    # Long & Freese latent-variable standardisation is therefore undefined
    # here; return NA so the caller emits the "outside scope" caveat (use
    # standardized = "refit" for a log-binomial fit). Was wrongly pi^2/3.
    "log"     = NA_real_,
    NA_real_
  )
  if (!is.finite(var_link)) return(NA_real_)
  sqrt(var_eta + var_link)
}


# ---- Internal: refit-method coefs table (z-asymptotic for glm) -----------

glm_coefs_inference_table <- function(fit_std, vc_std, vcov_type, cluster,
                                       ci_level, intercept_to_na = TRUE) {
  cf <- stats::coef(fit_std)
  rows <- lapply(seq_along(cf), function(i) {
    if (is.na(cf[i])) {
      list(estimate = NA_real_, se = NA_real_, ci_lower = NA_real_,
           ci_upper = NA_real_, statistic = NA_real_, df = NA_real_,
           p.value = NA_real_)
    } else {
      compute_glm_coef_inference(fit_std, i, vc_std, vcov_type,
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
