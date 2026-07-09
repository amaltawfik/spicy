# Class-generic variance-covariance + coefficient-inference backbone,
# reused by every model class via table_regression() (and by
# table_continuous_lm()). Moved here from R/lm_compute.R: nothing below is
# lm-specific. Contents:
#   * compute_model_vcov(): vcov family dispatch -- classical / HC* (sandwich) /
#     CR* (clubSandwich) / bootstrap / jackknife. The resamplers
#     (compute_resample_vcov_*) refit lm/glm and so apply only to those classes.
#   * compute_coef_inference(): single-coefficient inference; the reference
#     distribution (t vs z) follows the ESTIMATOR, not the fit class.
#   * compute_wald_test(): multi-coefficient Wald F / chi^2.
#   * compute_satt_df_per_coef(): clubSandwich Satterthwaite df map.

compute_model_vcov <- function(
  fit,
  type = "classical",
  cluster = NULL,
  weights = NULL,
  boot_n = 1000L
) {
  # D2(a) guard: the resamplers refit the model with stats::lm() / stats::glm(),
  # so for any other class they would silently fit a WRONG model on the
  # resamples (an lm on survival times, etc.). The orchestrator's per-class
  # capability check (validate_vcov_cluster_lists) already rejects this, but
  # guard here too so a direct/internal caller cannot trigger the silent misfit.
  # rms ols/lrm/Glm layer "lm"/"glm" into their class vector but use "="-style
  # coef names the stats::lm/glm refit cannot reproduce (a noisy classical
  # fallback), so exclude them explicitly -- matching the clean abort cph gets.
  if (type %in% c("bootstrap", "jackknife") &&
      (!inherits(fit, c("lm", "glm")) ||
         inherits(fit, c("ols", "lrm", "cph", "Glm")))) {
    spicy_abort(
      sprintf(
        "`vcov = \"%s\"` (resampling) is only available for lm / glm fits, not `%s`.",
        type, class(fit)[1L]
      ),
      class = "spicy_unsupported_vcov"
    )
  }
  if (identical(type, "classical")) {
    return(stats::vcov(fit))
  }

  if (identical(type, "bootstrap")) {
    return(compute_resample_vcov_bootstrap(
      fit,
      cluster = cluster,
      weights = weights,
      boot_n = boot_n
    ))
  }

  if (identical(type, "jackknife")) {
    return(compute_resample_vcov_jackknife(
      fit,
      cluster = cluster,
      weights = weights
    ))
  }

  if (startsWith(type, "HC")) {
    return(tryCatch(
      sandwich::vcovHC(fit, type = type),
      error = function(e) {
        spicy_warn(
          c(
            sprintf(
              "Robust `vcov = \"%s\"` could not be computed.",
              type
            ),
            "x" = paste0("Underlying error: ", conditionMessage(e)),
            "i" = "Falling back to the classical OLS variance; the result may contain NA."
          ),
          class = "spicy_fallback"
        )
        stats::vcov(fit)
      }
    ))
  }

  if (startsWith(type, "CR")) {
    if (is.null(cluster)) {
      spicy_abort(
        sprintf(
          "`vcov = \"%s\"` requires `cluster` to be specified.",
          type
        ), class = "spicy_invalid_input")
    }
    # Defense-in-depth: validate the cluster length here too (the public
    # validator already checks it, but a direct/internal caller bypasses that),
    # BEFORE sandwich/clubSandwich so a wrong length never surfaces their
    # locale-dependent internal error. Shared single source of truth.
    .check_cluster_length(fit, cluster)
    # Cluster-robust backend by class. The CR0..CR3 bias-reduction variants are
    # a clubSandwich concept; Cox and the sandwich::vcovCL classes have a single
    # cluster sandwich, so the requested CR* maps to that one estimator.
    #   * coxph / cph -> Lin & Wei (1989) grouped-dfbeta = coxph(cluster=); the
    #     field-standard Cox robust SE (clubSandwich gives a different,
    #     non-standard result for Cox).
    #   * ols / lrm / cph / Glm (rms) -> rms::robcov() native cluster sandwich
    #     (Huber-White; Lin-Wei for cph). Requires the fit's x = TRUE, y = TRUE.
    #   * survreg / gam / polr / clm / betareg / mlogit -> sandwich::vcovCL
    #     (clubSandwich has no usable method for these classes).
    #   * lm / glm / lmer / lme / glmmTMB -> clubSandwich bias-reduced CR*.
    # rms first: cph inherits "coxph", but robcov() == the Lin-Wei sandwich and
    # gives a clearer x/y error, so route the whole rms family through it.
    if (inherits(fit, c("ols", "lrm", "cph", "Glm"))) {
      return(.rms_robust_vcov(fit, cluster))
    }
    if (inherits(fit, "coxph")) {
      return(.coxph_cluster_robust_vcov(fit, cluster))
    }
    if (inherits(fit, c("survreg", "gam", "bam", "polr", "clm",
                        "betareg", "mlogit", "zeroinfl", "hurdle"))) {
      return(sandwich::vcovCL(fit, cluster = cluster))
    }
    if (!requireNamespace("clubSandwich", quietly = TRUE)) {
      spicy_abort(
        sprintf(
          paste0(
            "`vcov = \"%s\"` requires the 'clubSandwich' package. ",
            "Install it with install.packages(\"clubSandwich\")."
          ),
          type
        ), class = "spicy_invalid_input")
    }
    return(tryCatch(
      clubSandwich::vcovCR(fit, type = type, cluster = cluster),
      error = function(e) {
        spicy_warn(
          c(
            sprintf(
              "Cluster-robust `vcov = \"%s\"` could not be computed.",
              type
            ),
            "x" = paste0("Underlying error: ", conditionMessage(e)),
            "i" = "Falling back to the classical OLS variance; the result may contain NA."
          ),
          class = "spicy_fallback"
        )
        stats::vcov(fit)
      }
    ))
  }

  spicy_abort(
    sprintf("Unknown `vcov` type \"%s\".", type), class = "spicy_invalid_input")
}

# Internal: nonparametric / cluster bootstrap variance-covariance
# matrix of the coefficient vector. Resamples observations (or whole
# clusters when `cluster` is supplied), refits `lm()` on each
# replicate, and returns the empirical covariance of the bootstrapped
# coefficients. References: Davison & Hinkley (1997); Cameron, Gelbach
# & Miller (2008) for the cluster bootstrap.
compute_resample_vcov_bootstrap <- function(
  fit,
  cluster = NULL,
  weights = NULL,
  boot_n = 1000L
) {
  # Each replicate refits on resampled rows of the FIXED evaluated design
  # (model.matrix / model.response), not by re-evaluating the formula on
  # a resampled model frame. Formula re-evaluation cannot see wrapped
  # columns (`factor(cyl)`, `log(x)`, `poly(x, 2)`): it either failed on
  # every replicate (silently degrading to the classical fallback below)
  # or -- worse -- re-evaluated the transform against the caller's
  # environment, pairing resampled rows with UNRESAMPLED columns and
  # returning a silently wrong covariance. Row-wise transforms give
  # identical replicate coefficients either way; basis terms (`poly()`,
  # `splines::ns()`) keep the original basis, so replicate coefficients
  # stay comparable across resamples.
  mm <- stats::model.matrix(fit)
  mf <- stats::model.frame(fit)
  resp <- stats::model.response(mf)
  off <- stats::model.offset(mf)
  n_obs <- nrow(mm)
  orig_coefs <- stats::coef(fit)
  k <- length(orig_coefs)
  coef_names <- names(orig_coefs)

  if (is.null(weights)) {
    weights <- stats::weights(fit)
  }

  # Class-aware refit: glm fits must be re-fit with stats::glm.fit and
  # the original family / link, otherwise the bootstrap variance is
  # computed for a misspecified linear model on the (often binary)
  # response. The original family is captured once and reused.
  is_glm <- inherits(fit, "glm")
  fam <- if (is_glm) stats::family(fit) else NULL
  glm_ctrl <- if (is_glm) fit$control %||% stats::glm.control() else NULL
  refit_coefs <- function(boot_idx) {
    x_b <- mm[boot_idx, , drop = FALSE]
    w_b <- if (is.null(weights)) rep.int(1, length(boot_idx))
           else weights[boot_idx]
    off_b <- if (is.null(off)) NULL else off[boot_idx]
    z <- if (is_glm) {
      y_b <- if (is.matrix(resp)) resp[boot_idx, , drop = FALSE]
             else resp[boot_idx]
      tryCatch(
        suppressWarnings(stats::glm.fit(
          x = x_b, y = y_b, weights = w_b, offset = off_b,
          family = fam, control = glm_ctrl
        )),
        error = function(e) NULL
      )
    } else {
      tryCatch(
        suppressWarnings(stats::lm.wfit(x = x_b, y = resp[boot_idx],
                                         w = w_b, offset = off_b)),
        error = function(e) NULL
      )
    }
    if (is.null(z)) NULL else z$coefficients
  }

  beta_boot <- matrix(NA_real_, nrow = boot_n, ncol = k)
  colnames(beta_boot) <- coef_names

  if (is.null(cluster)) {
    # Nonparametric obs bootstrap
    for (b in seq_len(boot_n)) {
      boot_idx <- sample.int(n_obs, n_obs, replace = TRUE)
      coefs_b <- refit_coefs(boot_idx)
      if (!is.null(coefs_b)) {
        common <- intersect(names(coefs_b), coef_names)
        beta_boot[b, common] <- coefs_b[common]
      }
    }
  } else {
    # Cluster bootstrap: resample whole clusters
    unique_g <- unique(cluster)
    G <- length(unique_g)
    cl_indices <- split(seq_along(cluster), cluster)
    for (b in seq_len(boot_n)) {
      boot_g <- sample(unique_g, G, replace = TRUE)
      boot_idx <- unlist(cl_indices[as.character(boot_g)], use.names = FALSE)
      coefs_b <- refit_coefs(boot_idx)
      if (!is.null(coefs_b)) {
        common <- intersect(names(coefs_b), coef_names)
        beta_boot[b, common] <- coefs_b[common]
      }
    }
  }

  valid <- stats::complete.cases(beta_boot)
  n_valid <- sum(valid)
  if (n_valid < 10L) {
    # Pre-1.0 hard error (was: classical fallback under a "bootstrap"
    # footer -- the footer lied about the estimator actually applied).
    spicy_abort(
      c(
        sprintf(
          "Bootstrap failed: only %d of %d replicates produced a full coefficient vector.",
          n_valid, boot_n
        ),
        "i" = paste0(
          "The resampled refits are unstable (rank-deficient or ",
          "non-converged resamples). Increase `boot_n`, simplify the ",
          "model, or use an analytic `vcov` (\"HC3\", \"CR2\", ...)."
        )
      ),
      class = "spicy_resampling_failed"
    )
  }
  if (n_valid < boot_n %/% 2L) {
    spicy_warn(
      c(
        sprintf(
          "Bootstrap: %d / %d replicates failed (likely rank-deficient resamples).",
          boot_n - n_valid,
          boot_n
        ),
        "i" = sprintf(
          "The bootstrap vcov is computed from the %d valid replicates.",
          n_valid
        )
      ),
      class = "spicy_fallback"
    )
  }

  beta_boot <- beta_boot[valid, , drop = FALSE]
  vc <- stats::cov(beta_boot)
  # Percentile CIs (`ci_method = "boot_percentile"`) reuse THESE replicates
  # -- never a second resampling pass -- and the footer reports the VALID
  # replicate count (Stata's bootstrap header reports completed
  # replications, not requested ones).
  attr(vc, "beta_boot") <- beta_boot
  attr(vc, "boot_n_valid") <- n_valid
  vc
}

# Internal: leave-one-out (or leave-one-cluster-out) jackknife
# variance-covariance matrix of the coefficient vector. References:
# Quenouille (1956) and Tukey (1958) for the original jackknife;
# MacKinnon & White (1985) for the linear-regression form.
compute_resample_vcov_jackknife <- function(
  fit,
  cluster = NULL,
  weights = NULL
) {
  # Leave-one-out refits run on the FIXED evaluated design, exactly like
  # the bootstrap resampler above (see the rationale there): formula
  # re-evaluation on a subset model frame cannot see wrapped columns and
  # either failed every replicate or silently leaked the caller's
  # environment.
  mm <- stats::model.matrix(fit)
  mf <- stats::model.frame(fit)
  resp <- stats::model.response(mf)
  off <- stats::model.offset(mf)
  n_obs <- nrow(mm)
  orig_coefs <- stats::coef(fit)
  k <- length(orig_coefs)
  coef_names <- names(orig_coefs)

  if (is.null(weights)) {
    weights <- stats::weights(fit)
  }

  # Class-aware refit: glm fits must be re-fit with stats::glm.fit and
  # the original family / link (see `compute_resample_vcov_bootstrap`
  # for the same rationale).
  is_glm <- inherits(fit, "glm")
  fam <- if (is_glm) stats::family(fit) else NULL
  glm_ctrl <- if (is_glm) fit$control %||% stats::glm.control() else NULL
  refit_coefs <- function(jack_idx) {
    x_g <- mm[jack_idx, , drop = FALSE]
    w_g <- if (is.null(weights)) rep.int(1, length(jack_idx))
           else weights[jack_idx]
    off_g <- if (is.null(off)) NULL else off[jack_idx]
    z <- if (is_glm) {
      y_g <- if (is.matrix(resp)) resp[jack_idx, , drop = FALSE]
             else resp[jack_idx]
      tryCatch(
        suppressWarnings(stats::glm.fit(
          x = x_g, y = y_g, weights = w_g, offset = off_g,
          family = fam, control = glm_ctrl
        )),
        error = function(e) NULL
      )
    } else {
      tryCatch(
        suppressWarnings(stats::lm.wfit(x = x_g, y = resp[jack_idx],
                                         w = w_g, offset = off_g)),
        error = function(e) NULL
      )
    }
    if (is.null(z)) NULL else z$coefficients
  }

  if (is.null(cluster)) {
    G <- n_obs
    units <- seq_len(n_obs)
    leave_out <- function(g) which(units != g)
  } else {
    unique_g <- unique(cluster)
    G <- length(unique_g)
    leave_out <- function(g) which(cluster != unique_g[g])
  }

  beta_jack <- matrix(NA_real_, nrow = G, ncol = k)
  colnames(beta_jack) <- coef_names
  for (g in seq_len(G)) {
    coefs_g <- refit_coefs(leave_out(g))
    if (!is.null(coefs_g)) {
      common <- intersect(names(coefs_g), coef_names)
      beta_jack[g, common] <- coefs_g[common]
    }
  }

  valid <- stats::complete.cases(beta_jack)
  n_valid <- sum(valid)
  if (n_valid < 2L) {
    # Pre-1.0 hard error (was: classical fallback under a "jackknife"
    # footer -- the footer lied about the estimator actually applied).
    spicy_abort(
      c(
        "Jackknife failed: fewer than 2 valid leave-out replicates.",
        "i" = paste0(
          "The leave-out refits are unstable (rank-deficient or ",
          "non-converged subsets). Simplify the model or use an ",
          "analytic `vcov` (\"HC3\", \"CR2\", ...)."
        )
      ),
      class = "spicy_resampling_failed"
    )
  }
  beta_jack <- beta_jack[valid, , drop = FALSE]
  beta_mean <- colMeans(beta_jack)
  centered <- sweep(beta_jack, 2L, beta_mean, FUN = "-")
  scale <- (n_valid - 1L) / n_valid
  scale * crossprod(centered)
}

# Equal-tailed percentile interval from bootstrap replicates, following the
# boot::boot.ci(type = "perc") convention exactly: the (R+1)*alpha-th order
# statistics, interpolated between adjacent order statistics on the normal
# quantile scale when (R+1)*alpha is not an integer (boot:::norm.inter;
# Davison & Hinkley 1997, ch. 5). Implemented locally -- no new dependency --
# and cross-validated against boot::boot.ci in the test suite.
.boot_percentile_ci <- function(t, ci_level) {
  t <- t[is.finite(t)]
  R <- length(t)
  alpha <- c((1 - ci_level) / 2, (1 + ci_level) / 2)
  rk <- (R + 1) * alpha
  k <- trunc(rk)
  tstar <- sort(t)
  out <- numeric(2L)
  for (j in 1:2) {
    if (k[j] == 0L) {
      out[j] <- tstar[1L]           # extreme order statistic (small R)
    } else if (k[j] >= R) {
      out[j] <- tstar[R]            # extreme order statistic (small R)
    } else if (k[j] == rk[j]) {
      out[j] <- tstar[k[j]]         # integer rank: exact order statistic
    } else {
      # Interpolate between order statistics k and k + 1 on the normal
      # quantile scale (norm.inter).
      q_a  <- stats::qnorm(alpha[j])
      q_k  <- stats::qnorm(k[j] / (R + 1))
      q_k1 <- stats::qnorm((k[j] + 1L) / (R + 1))
      out[j] <- tstar[k[j]] +
        (q_a - q_k) / (q_k1 - q_k) * (tstar[k[j] + 1L] - tstar[k[j]])
    }
  }
  out
}

# Internal: single-coefficient inference (estimate, SE, statistic, df, p, CI).
# Class-generic. The reference distribution of the classical / HC* default path
# is chosen by `test`, NOT by the fit class: `test = "t"` uses df.residual(fit)
# (or `df_resid` when supplied) -- the OLS / lmer convention; `test = "z"` uses
# df = Inf -- the ML convention (glm, cox, ordinal, glmmTMB). Resampling
# (bootstrap / jackknife) is always asymptotic z; CR* is always clubSandwich
# Satterthwaite t (falling back to z when coef_test fails).
compute_coef_inference <- function(
  fit,
  coef_idx,
  vc,
  vcov_type,
  cluster = NULL,
  ci_level = 0.95,
  test = c("t", "z"),
  df_resid = NULL,
  estimates = NULL,
  ci_method = "wald"
) {
  test <- match.arg(test)
  # Point estimates are stats::coef(fit) for most classes. Classes whose
  # coef() is NOT the fixed-effect vector (e.g. merMod, where coef() returns
  # the per-group random-effect-adjusted coefficients) pass `estimates`
  # explicitly (lme4::fixef(fit)). The clubSandwich coef_test() path already
  # operates on the fixed effects, so only the estimate source needs overriding.
  cf <- if (!is.null(estimates)) estimates else stats::coef(fit)
  estimate <- unname(cf[coef_idx])

  # Rank-deficient guard: when lm()/glm() drops a perfectly collinear column it
  # keeps an NA entry in stats::coef(fit), but sandwich::vcovHC() /
  # clubSandwich::vcovCR() / coef_test() DROP that column, so the robust matrix
  # is NARROWER than the full coef vector. Indexing those by the full-vector
  # position `coef_idx` would shift every coef after a dropped one onto the
  # WRONG variance (and push the last out of range -> NA). Index by coefficient
  # NAME instead, falling back to position only when names are unavailable.
  # stats::vcov(fit) keeps the NA row/col, so the classical path is unaffected
  # either way. Dropped coefs never reach here -- build_b_rows() short-circuits
  # them to NA rows -- so a present name always resolves in the robust matrix.
  coef_name <- if (!is.null(names(cf))) names(cf)[coef_idx] else NA_character_
  .robust_pos <- function(nms) {
    if (!is.na(coef_name) && !is.null(nms) && coef_name %in% nms) {
      match(coef_name, nms)
    } else {
      coef_idx
    }
  }
  .se_at <- function(mat) {
    # diag() drops names, so take the coefficient names from the matrix
    # dimnames (sandwich / vcov keep them) for the name-based lookup.
    nms <- rownames(mat) %||% colnames(mat)
    dv <- diag(mat)
    pos <- .robust_pos(nms)
    if (is.na(pos) || pos > length(dv)) NA_real_ else sqrt(dv[[pos]])
  }

  # Resampling-based vcov: asymptotic z inference (df = Inf).
  if (vcov_type %in% c("bootstrap", "jackknife")) {
    se_est <- .se_at(vc)
    stat <- estimate / se_est
    crit <- stats::qnorm(1 - (1 - ci_level) / 2)
    pval <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
    ci_lo <- estimate - crit * unname(se_est)
    ci_hi <- estimate + crit * unname(se_est)
    # `ci_method = "boot_percentile"` (bootstrap only, validated upstream):
    # replace ONLY the CI bounds with equal-tailed percentile intervals of
    # the stored replicates -- estimate, SE, statistic and p stay Wald from
    # the bootstrap covariance (the Stata convention: normal-based table
    # CIs by default, percentile via estat bootstrap on request).
    if (identical(ci_method, "boot_percentile")) {
      bb <- attr(vc, "beta_boot")
      if (!is.null(bb) && !is.na(coef_name) && coef_name %in% colnames(bb)) {
        pci <- .boot_percentile_ci(bb[, coef_name], ci_level)
        ci_lo <- pci[1L]
        ci_hi <- pci[2L]
      }
    }
    return(list(
      estimate = estimate,
      se = unname(se_est),
      statistic = unname(stat),
      df = Inf,
      p.value = unname(pval),
      ci_lower = ci_lo,
      ci_upper = ci_hi,
      test_type = "z"
    ))
  }

  if (startsWith(vcov_type, "CR") && !is.null(cluster)) {
    ct <- tryCatch(
      clubSandwich::coef_test(
        fit,
        vcov = vc,
        cluster = cluster,
        test = "Satterthwaite"
      ),
      error = function(e) NULL
    )
    # Index the coef_test() rows by NAME (rank-deficient guard, see above):
    # coef_test() drops collinear columns, so a present coefficient may sit at a
    # different row than its full-vector position coef_idx.
    cr_pos <- if (!is.null(ct) && is.data.frame(ct)) {
      .robust_pos(rownames(ct))
    } else {
      NA_integer_
    }
    if (
      !is.null(ct) &&
        is.data.frame(ct) &&
        !is.na(cr_pos) && cr_pos <= nrow(ct) &&
        all(c("df_Satt", "p_Satt", "SE", "tstat") %in% names(ct))
    ) {
      df <- ct$df_Satt[cr_pos]
      se_est <- ct$SE[cr_pos]
      stat <- ct$tstat[cr_pos]
      pval <- ct$p_Satt[cr_pos]
      crit <- if (is.finite(df) && df > 0) {
        stats::qt(1 - (1 - ci_level) / 2, df = df)
      } else {
        stats::qnorm(1 - (1 - ci_level) / 2)
      }
      return(list(
        estimate = estimate,
        se = unname(se_est),
        statistic = unname(stat),
        df = as.double(unname(df)),
        p.value = unname(pval),
        ci_lower = estimate - crit * unname(se_est),
        ci_upper = estimate + crit * unname(se_est),
        test_type = "t"
      ))
    }
  }

  # Classical / HC* / CR-fallback default path: t (df.residual or `df_resid`)
  # for `test = "t"`, z (df = Inf) for `test = "z"`. The axis is the estimator's
  # reference distribution, not the fit class.
  se_est <- .se_at(vc)
  stat <- estimate / se_est
  if (identical(test, "z")) {
    df <- Inf
    crit <- stats::qnorm(1 - (1 - ci_level) / 2)
    pval <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
    test_type <- "z"
  } else {
    df <- if (!is.null(df_resid)) df_resid else stats::df.residual(fit)
    if (is.finite(df) && df > 0) {
      crit <- stats::qt(1 - (1 - ci_level) / 2, df = df)
      pval <- 2 * stats::pt(abs(stat), df = df, lower.tail = FALSE)
      test_type <- "t"
    } else {
      crit <- stats::qnorm(1 - (1 - ci_level) / 2)
      pval <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
      test_type <- "z"
    }
  }
  list(
    estimate = estimate,
    se = unname(se_est),
    statistic = unname(stat),
    df = as.double(df),
    p.value = unname(pval),
    ci_lower = estimate - crit * unname(se_est),
    ci_upper = estimate + crit * unname(se_est),
    test_type = test_type
  )
}

# Internal: multi-coefficient Wald F (used for the global test in
# k > 2 categorical predictors). For CR* mode uses
# clubSandwich::Wald_test() with the HTZ (Hotelling-T-squared with
# Satterthwaite df) method; for classical / HC* uses the Wald F with
# df.residual.
compute_wald_test <- function(
  fit,
  coef_idx_set,
  vc,
  vcov_type,
  cluster = NULL
) {
  cf <- stats::coef(fit)
  beta_sub <- cf[coef_idx_set]
  q <- length(beta_sub)
  df_resid_classical <- stats::df.residual(fit)

  if (q == 0L) {
    return(list(
      statistic = NA_real_,
      df1 = NA_integer_,
      df2 = NA_integer_,
      p.value = NA_real_,
      test_type = NA_character_
    ))
  }

  # Resampling-based vcov: asymptotic chi^2 (Wald) test, df = q.
  if (vcov_type %in% c("bootstrap", "jackknife")) {
    vc_sub <- vc[coef_idx_set, coef_idx_set, drop = FALSE]
    chi2 <- tryCatch(
      as.numeric(crossprod(beta_sub, solve(vc_sub, beta_sub))),
      error = function(e) NA_real_
    )
    pval <- if (is.na(chi2) || !is.finite(chi2)) {
      NA_real_
    } else {
      stats::pchisq(chi2, df = q, lower.tail = FALSE)
    }
    return(list(
      statistic = chi2,
      df1 = as.integer(q),
      df2 = Inf,
      p.value = pval,
      test_type = "chi2"
    ))
  }

  if (startsWith(vcov_type, "CR") && !is.null(cluster)) {
    constraints <- tryCatch(
      clubSandwich::constrain_zero(coef_idx_set, coefs = cf),
      error = function(e) NULL
    )
    wt <- if (!is.null(constraints)) {
      tryCatch(
        clubSandwich::Wald_test(
          fit,
          constraints = constraints,
          vcov = vc,
          cluster = cluster,
          test = "HTZ"
        ),
        error = function(e) NULL
      )
    } else {
      NULL
    }
    if (
      !is.null(wt) &&
        is.data.frame(wt) &&
        nrow(wt) >= 1L &&
        all(c("Fstat", "df_num", "df_denom", "p_val") %in% names(wt))
    ) {
      return(list(
        statistic = unname(wt$Fstat[1]),
        df1 = as.integer(unname(wt$df_num[1])),
        df2 = as.double(unname(wt$df_denom[1])),
        p.value = unname(wt$p_val[1]),
        test_type = "F"
      ))
    }
  }

  # Classical / HC* path
  vc_sub <- vc[coef_idx_set, coef_idx_set, drop = FALSE]
  global_stat <- tryCatch(
    as.numeric(crossprod(beta_sub, solve(vc_sub, beta_sub)) / q),
    error = function(e) NA_real_
  )
  global_p <- if (is.na(global_stat) || !is.finite(global_stat)) {
    NA_real_
  } else {
    stats::pf(global_stat, q, df_resid_classical, lower.tail = FALSE)
  }
  list(
    statistic = global_stat,
    df1 = as.integer(q),
    df2 = as.double(df_resid_classical),
    p.value = global_p,
    test_type = "F"
  )
}


# Map coef name -> Satterthwaite df via clubSandwich::coef_test on the
# original glm. Returns NULL on failure (e.g., clubSandwich missing,
# coef_test errored). Caller falls back to z-asymptotic.
compute_satt_df_per_coef <- function(fit, vc, cluster) {
  ct <- tryCatch(
    clubSandwich::coef_test(
      fit,
      vcov = vc,
      cluster = cluster,
      test = "Satterthwaite"
    ),
    error = function(e) NULL
  )
  if (is.null(ct) || !is.data.frame(ct) || !"df_Satt" %in% names(ct)) {
    return(NULL)
  }
  setNames(as.numeric(ct$df_Satt), rownames(ct))
}


# ---- Robust-vcov capability (C2) ------------------------------------------

# Which `vcov` types table_regression() can actually COMPUTE for this fit's
# class. Default: "classical" only -- a robust vcov the class does not (yet)
# support fails fast in validate_vcov_cluster_lists() with a clear
# spicy_unsupported_vcov error, instead of silently returning model-based SEs
# under a robust label (audit finding C2). The supported set grows per class as
# the robust path is wired + cross-validated; see dev/C2_robust_vcov_spec.md.
#
# Note: "classical" is the user-facing token for the model-based default and is
# supported by EVERY class, so default calls never error -- only an explicit
# robust request on a class that cannot honour it does.
.robust_vcov_support <- function(fit) {
  full <- c("classical", paste0("HC", 0:5), paste0("CR", 0:3),
            "bootstrap", "jackknife")
  # Cluster-robust only: clubSandwich CR* is defined, but HC* (an OLS / single-
  # level concept) and the lm/glm-refitting resamplers are not.
  cr_only <- c("classical", paste0("CR", 0:3))
  switch(
    class(fit)[1L],
    lm     = full,
    glm    = full,
    negbin = full,   # MASS::glm.nb delegates to the glm path
    # Mixed-effects: cluster-robust via clubSandwich (Inc 2). lmer / lme get
    # Satterthwaite df; glmmTMB gets the CR matrix with z inference (its
    # coef_test() Satterthwaite path is unsupported). glmer is NOT granted:
    # clubSandwich::vcovCR() errors on glmerMod, so it would fall back to
    # model-based -- better to refuse cleanly until a working backend exists.
    lmerMod         = cr_only,
    lmerModLmerTest = cr_only,
    lme             = cr_only,
    glmmTMB         = cr_only,
    # Survival (Inc 3): coxph -> Lin-Wei grouped-dfbeta; survreg -> vcovCL.
    coxph           = cr_only,
    survreg         = cr_only,
    # Inc 4: cluster sandwich via sandwich::vcovCL. svyglm uses clubSandwich
    # design-aware CR*. clm is structure-aware: scale/nominal (partial-PO) fits
    # have no sandwich estfun method, so CR* is refused for them
    # (-> spicy_unsupported_vcov up front).
    gam             = cr_only,
    bam             = cr_only,
    polr            = cr_only,
    clm             = .clm_robust_vcov_support(fit, cr_only),
    betareg         = cr_only,
    svyglm          = cr_only,
    # mlogit: CR* only. vcovHC() is NUMERICALLY WRONG for mlogit -- its meat
    # divides by nobs() (long-format rows, n x J) while estfun() has one row
    # per choice situation (n), deflating SEs by ~sqrt(J); and without a
    # hatvalues method HC1-HC5 silently equal HC0. vcovCL() sizes everything
    # off the estfun rows and matches sandwich::sandwich(), so the cluster
    # path is correct (verified against the Fishing data, 2026-07-03).
    mlogit          = cr_only,
    # Inc 4b: rms fits via rms::robcov() native cluster sandwich (needs the
    # fit's x = TRUE, y = TRUE). ols / lrm / cph / Glm.
    ols             = cr_only,
    lrm             = cr_only,
    cph             = cr_only,
    Glm             = cr_only,
    # Two-part count models (pscl): sandwich::estfun / bread work for BOTH
    # components (verified 2026-07-02), so the CL cluster sandwich covers the
    # whole model. vcovHC's type= machinery fails (no hatvalues) -> no HC*.
    zeroinfl        = cr_only,
    hurdle          = cr_only,
    # Univariable screen bundle: the request is forwarded to every
    # underlying fit, so the capability is theirs (homogeneous lm/glm).
    spicy_uv_screen = .robust_vcov_support(fit$fits[[1L]]),
    # --- classes whose robust path is wired in later C2 increments go here ---
    "classical"
  )
}


# clm with a scale (scale = ~) or nominal (nominal = ~, partial-PO) component has
# no sandwich::estfun method ("estimating functions for scale regression not
# implemented yet"), so vcovCL / CR* cannot be formed. Refuse CR* up front for
# those fits (the validate gate then emits a clear spicy_unsupported_vcov) rather
# than crashing deep inside estfun. Plain proportional-odds clm keeps cr_only.
.clm_robust_vcov_support <- function(fit, cr_only) {
  if (!is.null(fit$S.terms) || !is.null(fit$nom.terms)) "classical" else cr_only
}


# Recompute the inference columns (std_error, statistic, df, p_value, ci_lower,
# ci_upper, test_type) of a frame's B rows under a robust vcov, reusing the
# class-generic compute_model_vcov() + compute_coef_inference(). A no-op for the
# model-based default. Classes whose coef() is not the fixed-effect vector pass
# `estimates` (e.g. lme4::fixef(fit)). Estimates + row metadata are preserved;
# only the inference cells change. Shared by every non-lm/glm robust-capable
# method so the robust path is wired in exactly one place.
.apply_robust_vcov_to_coefs <- function(coefs, fit, vcov_type, cluster,
                                        ci_level, test = "t",
                                        estimates = NULL) {
  # No-op for the model-based defaults: "classical" / "model" (every class) and
  # "survey-Taylor" (the svyglm design-based default, whose SE .svyglm_coefs()
  # already computed and which compute_model_vcov() does not know).
  if (is.null(vcov_type) ||
        vcov_type %in% c("classical", "model", "survey-Taylor")) {
    return(coefs)
  }
  vc <- compute_model_vcov(fit, type = vcov_type, cluster = cluster)
  cf <- if (!is.null(estimates)) estimates else stats::coef(fit)
  b_rows <- which(coefs$estimate_type == "B" & !(coefs$is_ref %in% TRUE))
  for (r in b_rows) {
    idx <- match(coefs$term[r], names(cf))
    if (is.na(idx)) next
    inf <- compute_coef_inference(
      fit, idx, vc, vcov_type, cluster, ci_level,
      test = test, estimates = cf
    )
    coefs$std_error[r] <- inf$se
    coefs$statistic[r] <- inf$statistic
    coefs$df[r]        <- as.double(inf$df)
    coefs$p_value[r]   <- inf$p.value
    coefs$ci_lower[r]  <- inf$ci_lower
    coefs$ci_upper[r]  <- inf$ci_upper
    coefs$test_type[r] <- inf$test_type
  }
  coefs
}

# Human-readable footer label for an applied robust vcov, mirroring the
# lm/glm footer (format_vcov_label_from_frame). Used by the non-lm/glm methods
# to set info$vcov_label when a robust vcov is requested, so the footer names
# the estimator actually applied instead of the model-based default.
.robust_vcov_label <- function(vcov_type, cluster_name = NA_character_,
                               estimator = NULL) {
  if (startsWith(vcov_type, "HC")) {
    return(sprintf("heteroskedasticity-robust (%s)", estimator %||% vcov_type))
  }
  if (startsWith(vcov_type, "CR")) {
    cl <- if (is.na(cluster_name) || !nzchar(cluster_name)) {
      "cluster vector supplied"
    } else {
      sprintf("clusters by %s", cluster_name)
    }
    return(sprintf("cluster-robust (%s), %s", estimator %||% vcov_type, cl))
  }
  vcov_type
}


# Number of cluster entries a cluster-robust vcov expects for this fit: one per
# row of the score / residual matrix the sandwich sums over. For almost every
# class that equals stats::nobs(), but two classes need a class-specific count:
#   * survival::coxph: the Lin-Wei sandwich sums dfbeta residuals over clusters
#     with one dfbeta row per SUBJECT. Under censoring stats::nobs() is the EVENT
#     count, which is smaller -- using it would reject a correct subject-level
#     cluster and let a (wrong) event-length one crash in rowsum(). (rms::cph
#     also inherits "coxph" but its nobs() already counts subjects, and fit$n is
#     a c(censored, events) vector, so it is deliberately excluded here.)
#   * mlogit: estfun() is at the choice-situation level (one row per individual,
#     not per long-format alternative), so nobs() (the long count) is too big.
# Used by the orchestrator's cluster-length check (validate_vcov_cluster_lists).
.expected_cluster_length <- function(fit) {
  if (inherits(fit, "coxph") && !inherits(fit, "cph")) {
    n <- tryCatch(NROW(stats::residuals(fit, type = "dfbeta")),
                  error = function(e) NA_integer_)
    if (is.finite(n)) return(as.integer(n))
    if (!is.null(fit$n)) return(as.integer(fit$n[length(fit$n)]))
  }
  if (inherits(fit, "mlogit")) {
    n <- tryCatch(NROW(sandwich::estfun(fit)), error = function(e) NA_integer_)
    if (is.finite(n)) return(as.integer(n))
  }
  # rms fits: robcov() clusters over the design-matrix rows (= observations).
  # stats::nobs() has no method for some rms classes (e.g. Glm -> NA), so read
  # the row count off fit$x, which robust SE require to be present anyway.
  if (inherits(fit, c("ols", "lrm", "cph", "Glm")) && !is.null(fit[["x"]])) {
    return(as.integer(NROW(fit[["x"]])))
  }
  # pscl two-part models: stats::nobs() has no method; fit$n is the count.
  if (inherits(fit, c("zeroinfl", "hurdle"))) {
    return(as.integer(fit$n %||% NA_integer_))
  }
  n <- suppressWarnings(tryCatch(stats::nobs(fit), error = function(e) NA_integer_))
  if (is.null(n) || !is.finite(n)) return(NA_integer_)
  as.integer(n)
}


# Validate a cluster vector's length against what this fit's cluster-robust vcov
# requires (.expected_cluster_length()), with a clear, class-aware error checked
# BEFORE any sandwich/clubSandwich call -- so we never surface their internal,
# LOCALE-DEPENDENT messages (e.g. sandwich's "number of observations ... do not
# match", which is translated on a non-English R). Single source of truth shared
# by the public validator (validate_vcov_cluster_lists) and the internal compute
# path (compute_model_vcov), so direct/internal callers fail just as cleanly as
# table_regression(). No-op unless `cluster` is an atomic vector of wrong length.
.check_cluster_length <- function(fit, cluster, label = "`cluster`") {
  if (is.null(cluster) || !is.atomic(cluster)) {
    return(invisible(NULL))
  }
  n_exp <- .expected_cluster_length(fit)
  # If the required length can't be determined (NA), skip the check and let the
  # compute layer raise the appropriate error (e.g. rms without x/y).
  if (is.na(n_exp) || length(cluster) == n_exp) {
    return(invisible(NULL))
  }
  hint <- if (inherits(fit, "mlogit")) {
    paste0("For mlogit, `cluster` is at the choice-situation level (one entry ",
           "per individual), not per long-format alternative.")
  } else if (inherits(fit, c("coxph", "cph"))) {
    paste0("Cox cluster-robust SE need one `cluster` value per subject (row of ",
           "the model data), not per event.")
  } else {
    "Supply one `cluster` value per observation."
  }
  spicy_abort(
    c(
      sprintf("%s has length %d but the model requires length %d.",
              label, length(cluster), n_exp),
      "i" = hint
    ),
    class = "spicy_invalid_input"
  )
}


# Cluster-robust vcov for a Cox PH fit: the Lin & Wei (1989) grouped-dfbeta
# sandwich -- identical to coxph(..., cluster=) / the survival package's robust
# fit$var. clubSandwich::vcovCR is deliberately NOT used (it returns a
# different, non-standard result for Cox); this matches the field standard.
.coxph_cluster_robust_vcov <- function(fit, cluster) {
  db <- stats::residuals(fit, type = "dfbeta")
  if (is.null(dim(db))) db <- matrix(db, ncol = 1L)
  rob <- crossprod(rowsum(db, cluster))
  nm <- names(stats::coef(fit))
  if (length(nm) == nrow(rob)) dimnames(rob) <- list(nm, nm)
  rob
}


# Cluster-robust vcov for an rms fit (ols / lrm / cph / Glm) via rms::robcov(),
# the package's native Huber-White cluster sandwich (the Lin-Wei estimator for
# cph -- identical to coxph(..., cluster=)). robcov() needs the fit to carry its
# design + response matrices (x = TRUE, y = TRUE); we surface a clear, actionable
# error when they are missing instead of rms's terse "did not specify x=TRUE in
# fit". Dimnames are normalised ("Intercept" -> "(Intercept)") to match the
# coefs frame's term column (see .rms_coef_named()).
.rms_robust_vcov <- function(fit, cluster) {
  if (is.null(fit[["x"]]) || is.null(fit[["y"]])) {
    cl <- class(fit)[1L]
    spicy_abort(
      c(
        sprintf("Cluster-robust SE for an rms `%s` fit need the model matrices.",
                cl),
        "i" = sprintf(paste0("Refit with `x = TRUE, y = TRUE` (e.g. ",
                             "`%s(..., x = TRUE, y = TRUE)`) so rms::robcov() ",
                             "can form the sandwich."), cl)
      ),
      class = "spicy_invalid_input"
    )
  }
  V <- as.matrix(rms::robcov(fit, cluster = cluster)$var)
  nm <- .rms_normalise_names(names(stats::coef(fit)))
  if (length(nm) == nrow(V)) dimnames(V) <- list(nm, nm)
  V
}

# rms names its intercept "Intercept"; the coefs frame uses "(Intercept)".
.rms_normalise_names <- function(nm) {
  nm[nm == "Intercept"] <- "(Intercept)"
  nm
}

# stats::coef(rms_fit) with the intercept renamed to match coefs$term, so the
# name-based robust-vcov application (.apply_robust_vcov_to_coefs) aligns rows.
.rms_coef_named <- function(fit) {
  cf <- stats::coef(fit)
  names(cf) <- .rms_normalise_names(names(cf))
  cf
}
