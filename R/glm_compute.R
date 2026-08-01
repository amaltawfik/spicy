# Internal glm-specific computation backbone for table_regression().
# Mirrors R/lm_compute.R. Contents:
#   * Family / link helpers (title prefix, exponentiate header)
#   * Per-coef inference: z-asymptotic Wald, with CR* Satterthwaite
#     branch via clubSandwich
#   * Pseudo-R^2 family: McFadden, Nagelkerke, Tjur
#   * Term-level partial chi-square via the Type-II nested LRT
#   * apply_exponentiate_to_coefs(): exp() transform on coefs +
#     CIs + delta-method SE

# ---- Family / link introspection -----------------------------------------

# Return a small descriptor for a glm fit summarising its family
# and link, used by the rendering layer for title strings, the
# exponentiate column-header rebrand, and the gaussian-glm caveat.
spicy_glm_family_info <- function(fit) {
  fam <- stats::family(fit)
  family_name <- fam$family
  link_name <- fam$link
  list(
    family = family_name,
    link = link_name,
    is_gaussian = identical(family_name, "gaussian"),
    is_quasi = grepl("^quasi", family_name),
    # Title prefix per family / link (Q34)
    title_prefix = spicy_glm_title_prefix(family_name, link_name),
    # Column-header label for `exp(B)` per family / link
    exp_header = spicy_glm_exp_header(family_name, link_name)
  )
}

spicy_glm_title_prefix <- function(family_name, link_name) {
  if (identical(family_name, "binomial")) {
    return(switch(
      link_name,
      "logit" = "Logistic regression",
      "probit" = "Probit regression",
      "cloglog" = "Complementary log-log regression",
      "log" = "Log-binomial regression",
      "Binomial regression"
    ))
  }
  if (identical(family_name, "poisson")) {
    return("Poisson regression")
  }
  if (identical(family_name, "Gamma")) {
    return("Gamma regression")
  }
  if (identical(family_name, "inverse.gaussian")) {
    return("Inverse-Gaussian regression")
  }
  if (identical(family_name, "quasibinomial")) {
    return("Quasi-binomial regression")
  }
  if (identical(family_name, "quasipoisson")) {
    return("Quasi-Poisson regression")
  }
  if (grepl("^quasi", family_name)) {
    return("Quasi-likelihood regression")
  }
  # gaussian + identity (and any other unrecognised family) falls
  # through to the generic "Regression" prefix.
  "Regression"
}

spicy_glm_exp_header <- function(family_name, link_name) {
  # brms spells the single-trial binomial "bernoulli": same family,
  # same estimands (OR / RR / HR per link). Normalising here keeps
  # info$family$family truthful ("bernoulli") while the header map
  # stays engine-consistent with glm / rstanarm.
  if (identical(family_name, "bernoulli")) {
    family_name <- "binomial"
  }
  if (identical(family_name, "binomial") && identical(link_name, "logit")) {
    return("OR")
  }
  if (identical(family_name, "poisson") && identical(link_name, "log")) {
    return("IRR")
  }
  if (identical(family_name, "binomial") && identical(link_name, "cloglog")) {
    return("HR")
  }
  if (identical(family_name, "binomial") && identical(link_name, "log")) {
    return("RR")
  }
  if (identical(family_name, "Gamma") && identical(link_name, "log")) {
    return("MR") # mean ratio
  }
  if (identical(family_name, "cox") && identical(link_name, "log")) {
    return("HR") # hazard ratio (coxph / rms::cph)
  }
  if (identical(family_name, "cumulative") && identical(link_name, "logit")) {
    return("OR") # proportional-odds ratio (ordinal: polr / clm)
  }
  if (identical(family_name, "cumulative") && identical(link_name, "cloglog")) {
    return("HR") # proportional-hazards ratio (ordinal cloglog link)
  }
  if (identical(family_name, "multinomial") && identical(link_name, "logit")) {
    return("OR") # multinomial odds / relative-risk ratio (multinom / mlogit)
  }
  if (identical(family_name, "beta") && identical(link_name, "logit")) {
    return("OR") # betareg mean model (logit link)
  }
  if (identical(family_name, "negbin") && identical(link_name, "log")) {
    return("IRR") # negative-binomial rate ratio (fixest / pscl)
  }
  if (
    family_name %in%
      c("negbinomial", "geometric", "neg_binomial_2") &&
      identical(link_name, "log")
  ) {
    return("IRR") # brms / rstanarm count-family spellings (log link)
  }
  if (identical(family_name, "gamma") && identical(link_name, "log")) {
    return("MR") # brms lowercase gamma family (log link)
  }
  if (
    family_name %in%
      c(
        "nbinom1",
        "nbinom2",
        "truncated_poisson",
        "truncated_nbinom1",
        "truncated_nbinom2",
        "genpois",
        "compois"
      ) &&
      identical(link_name, "log")
  ) {
    return("IRR") # glmmTMB count families (log link)
  }
  if (grepl("^Negative Binomial", family_name) && identical(link_name, "log")) {
    return("IRR") # MASS::glm.nb family string is "Negative Binomial(theta)"
  }
  if (
    identical(family_name, "quasibinomial") && identical(link_name, "logit")
  ) {
    return("OR") # survey::svyglm(family = quasibinomial())
  }
  if (identical(family_name, "quasipoisson") && identical(link_name, "log")) {
    return("IRR") # survey::svyglm(family = quasipoisson())
  }
  "exp(B)"
}


# ---- Exponentiate link gate (Group D, G1) ---------------------------------

# exp(B) is a ratio only when the linear predictor is a log of something:
# log-odds (logit -> OR), log-mean / rate / risk (log -> IRR / RR / MR),
# or log-cumulative-hazard (cloglog -> HR; the grouped-time proportional-
# hazards reading of Prentice & Gloeckler 1978, which exists ONLY for
# binomial / quasibinomial / cumulative families -- a beta-mean cloglog
# coefficient has no ratio estimand). For every other non-identity link
# (probit, cauchit, inverse -- the Gamma() DEFAULT --, 1/mu^2, sqrt,
# ordinal loglog, ...) the exponentiated coefficient is a number with no
# estimand: Stata's flagship `probit` command ships no `or` / `eform`
# reporting option and SAS documents EXPB "for the logit model". The
# surveyed R packages silently exponentiate anyway (parameters even
# labels cauchit "Risk Ratio") -- exactly the mislabelling spicy gates
# against. Pre-1.0 policy: hard error over silent wrong output.
#
# Identity links must never reach this assert: the callers' warn + no-op
# guard runs first (mixed lm + logit tables keep working -- the identity
# request is satisfied vacuously, a probit request cannot be).
.exp_gate_allowed <- function(family_name, link_name) {
  if (link_name %in% c("log", "logit")) {
    return(TRUE)
  }
  # bernoulli = brms's spelling of the single-trial binomial; the
  # grouped-time proportional-hazards reading applies identically.
  identical(link_name, "cloglog") &&
    family_name %in% c("binomial", "bernoulli", "quasibinomial", "cumulative")
}

.assert_exp_link_ok <- function(family_name, link_name, model_id = NULL) {
  family_name <- as.character(family_name %||% "")[1L]
  link_name <- as.character(link_name %||% "")[1L]
  if (.exp_gate_allowed(family_name, link_name)) {
    return(invisible(TRUE))
  }

  why <- switch(
    link_name,
    "probit" = paste0(
      "Probit coefficients are shifts on a latent standard-normal ",
      "scale; their exponential has no interpretation."
    ),
    "cauchit" = paste0(
      "Cauchit coefficients are shifts on a latent standard-Cauchy ",
      "scale; their exponential has no interpretation."
    ),
    "cloglog" = sprintf(
      paste0(
        "Under a %s model the cloglog coefficient is not a log hazard ",
        "ratio (the grouped-time proportional-hazards reading exists ",
        "only for binomial and cumulative families); its exponential ",
        "has no interpretation."
      ),
      family_name
    ),
    "loglog" = paste0(
      "Ordinal loglog coefficients are not log hazard ratios (loglog ",
      "equals cloglog on the reversed response ordering); their ",
      "exponential has no interpretation."
    ),
    "inverse" = sprintf(
      paste0(
        "The coefficient acts on the inverse-mean scale (1/mu); its ",
        "exponential has no interpretation. For a mean-ratio ",
        "interpretation refit with %s(link = \"log\")."
      ),
      family_name
    ),
    "1/mu^2" = paste0(
      "The coefficient acts on the inverse-squared-mean scale ",
      "(1/mu^2); its exponential has no interpretation."
    ),
    "sqrt" = paste0(
      "The coefficient acts on the square-root-mean scale; its ",
      "exponential has no interpretation."
    ),
    sprintf(
      paste0(
        "The \"%s\" link does not place coefficients on a log scale; ",
        "their exponential has no interpretation."
      ),
      link_name
    )
  )

  # Display-facing model label: auto ids "M<k>" render as "Model <k>"
  # (matching the table spanners); user-supplied names pass through.
  label <- if (is.null(model_id) || !nzchar(model_id)) {
    ""
  } else if (grepl("^M[0-9]+$", model_id)) {
    sprintf(" (Model %s)", substring(model_id, 2L))
  } else {
    sprintf(" (model \"%s\")", model_id)
  }

  spicy_abort(
    c(
      sprintf(
        "`exponentiate = TRUE` is not meaningful for a model with link \"%s\"%s.",
        link_name,
        label
      ),
      "i" = why,
      "i" = paste0(
        "exp(B) is a ratio only for logit (odds ratio), log (rate / ",
        "risk / mean ratio), and binomial or ordinal cloglog (hazard ",
        "ratio) links."
      ),
      "i" = paste0(
        "Drop `exponentiate = TRUE` (it applies to every model in the ",
        "table) or report response-scale effects via the AME column ",
        "(`show_columns = c(\"b\", \"ame\")`)."
      )
    ),
    class = "spicy_invalid_input"
  )
}


# ---- Pseudo-R^2 family ----------------------------------------------------

# Pseudo-R^2 for glm. Three variants (the most reported in the
# applied literature) implemented from scratch -- no new package
# dependency, full control over formulas + edge cases.
#
#   * McFadden (1974)   : 1 - LL_full / LL_null
#                         Most general; defined whenever a true
#                         log-likelihood exists. Returns NA for
#                         quasi-likelihood families.
#   * Nagelkerke (1991) : (1 - exp((LL_null - LL_full) * 2 / n))
#                         / (1 - exp(LL_null * 2 / n))
#                         Cox-Snell rescaled to [0, 1]. Standard
#                         in SPSS / SAS for binary outcomes.
#                         Implemented from the original Nagelkerke
#                         (1991) log-likelihood formula, NOT the
#                         deviance-based variant used by
#                         `performance::r2()`. The two forms agree
#                         exactly for binomial (LL_saturated = 0)
#                         and disagree mildly for poisson / Gamma /
#                         inverse.gaussian (where LL_saturated != 0).
#                         For binary outcomes (the dominant use case)
#                         spicy and performance return identical
#                         values to machine precision.
#   * Tjur (2009)       : mean(prob | y=1) - mean(prob | y=0)
#                         "Coefficient of discrimination"; only
#                         defined for binary outcomes (binomial
#                         family with 0/1 y). Returns NA otherwise.
#
# All three return a finite value in [0, 1] (Nagelkerke) or
# (-Inf, 1] (McFadden and Tjur, both bounded above by 1 in
# well-specified models) when defined; NA when not applicable.
# `ll_null`: optional precomputed intercept-only log-likelihood.
# Both pseudo-R2 need the same null refit; a call site that shows
# McFadden AND Nagelkerke (the class-aware logit default) computes it
# once and passes it to both instead of refitting the null twice.
compute_pseudo_r2_mcfadden <- function(fit, ll_null = NULL) {
  if (!inherits(fit, "glm")) {
    return(NA_real_)
  }
  if (grepl("^quasi", stats::family(fit)$family)) {
    return(NA_real_)
  }
  ll_full <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) {
    NA_real_
  })
  if (is.null(ll_null)) {
    ll_null <- compute_intercept_only_loglik_glm(fit)
  }
  if (!is.finite(ll_full) || !is.finite(ll_null) || ll_null == 0) {
    # nocov start: a converged non-quasi glm always has a finite,
    # non-zero null log-likelihood (the intercept-only model is
    # never saturated for a non-degenerate response), so this guard
    # is defensive only.
    return(NA_real_)
    # nocov end
  }
  1 - (ll_full / ll_null)
}

compute_pseudo_r2_nagelkerke <- function(fit, ll_null = NULL) {
  if (!inherits(fit, "glm")) {
    return(NA_real_)
  }
  if (grepl("^quasi", stats::family(fit)$family)) {
    return(NA_real_)
  }
  ll_full <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) {
    NA_real_
  })
  if (is.null(ll_null)) {
    ll_null <- compute_intercept_only_loglik_glm(fit)
  }
  n <- stats::nobs(fit)
  if (!is.finite(ll_full) || !is.finite(ll_null) || !is.finite(n) || n <= 0) {
    # nocov start: a converged non-quasi glm always has a finite
    # log-likelihood and a positive nobs(), so this guard is
    # defensive only. (The reachable Nagelkerke NA path is the
    # upper <= 0 guard below, exercised by the low-variance Gamma
    # test in test-cov-glm_compute.R.)
    return(NA_real_)
    # nocov end
  }
  cox_snell <- 1 - exp((ll_null - ll_full) * 2 / n)
  upper <- 1 - exp(ll_null * 2 / n)
  if (!is.finite(upper) || upper <= 0) {
    return(NA_real_)
  }
  cox_snell / upper
}

# Log-likelihood of the intercept-only ("null") model for a glm,
# robust to:
#   * formula transforms on the response (`I(round(y))`, `log(y)`,
#     `cbind(y, n - y)`)
#   * `offset(...)` terms in the formula or `offset = ` argument
#   * `weights = ` argument
#
# A naive `update(fit, . ~ 1)` fails when the response side itself
# contains function calls because update reuses the LHS expression
# and tries to re-evaluate the bare symbols against the model frame
# (whose columns are named after the wrapped expression, not the
# inner symbol). It also drops the offset by default -- and an
# intercept-only fit without the offset over-attributes outcome
# variation to the intercept, producing pseudo-R^2 < 0 when the
# full model includes a real-rate offset (Long & Freese 2014 Section 3.6
# explicitly: the null model must carry the same offset as the full
# model, otherwise pseudo-R^2 is not a valid 0-1 statistic).
#
# Workaround: extract the *evaluated* response, weights, and
# offset from the model frame and refit on a fresh data.frame.
# Falls back to NA on any failure.
compute_intercept_only_loglik_glm <- function(fit) {
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(mf)) {
    return(NA_real_)
  } # nocov: a converged glm always carries a model.frame
  y <- tryCatch(stats::model.response(mf), error = function(e) NULL)
  if (is.null(y)) {
    return(NA_real_)
  } # nocov: a glm model.frame always has a response column
  fam <- stats::family(fit)
  weights <- tryCatch(stats::weights(fit), error = function(e) NULL)
  offset_vec <- tryCatch(stats::model.offset(mf), error = function(e) NULL)

  args <- list(formula = y ~ 1, family = fam, data = data.frame(y = y))
  if (!is.null(weights)) {
    args$weights <- weights
  }
  if (!is.null(offset_vec)) {
    args$offset <- offset_vec
  }
  null_fit <- tryCatch(
    suppressWarnings(do.call(stats::glm, args)),
    error = function(e) NULL
  )
  if (is.null(null_fit)) {
    return(NA_real_)
  } # nocov: intercept-only refit of a valid response always converges
  tryCatch(as.numeric(stats::logLik(null_fit)), error = function(e) NA_real_)
}

compute_pseudo_r2_tjur <- function(fit) {
  if (!inherits(fit, "glm")) {
    return(NA_real_)
  }
  fam <- stats::family(fit)
  if (!identical(fam$family, "binomial")) {
    return(NA_real_)
  }
  y <- stats::model.response(stats::model.frame(fit))
  if (is.factor(y)) {
    y <- as.integer(y) - 1L
  }
  if (!all(y %in% c(0, 1))) {
    return(NA_real_)
  }
  pi_hat <- stats::fitted(fit)
  # nocov: once y is a 0/1 vector (guard above), fitted() and the
  # response always have matching length for a converged glm.
  if (length(pi_hat) != length(y)) {
    return(NA_real_)
  }
  m1 <- mean(pi_hat[y == 1])
  m0 <- mean(pi_hat[y == 0])
  if (!is.finite(m1) || !is.finite(m0)) {
    return(NA_real_)
  }
  m1 - m0
}


# ---- exp() transform for response-scale display --------------------------

# Apply exp() to B-row (and beta-row) estimates + CI bounds, and a
# delta-method approximation to the SE. The test statistic and the
# p-value are invariant under monotone transformation and stay on
# the link scale (matches Stata `logit, or` / SPSS exp(B) / SAS
# OR convention).
#
# Reference rows (en-dash) and singular coefs (NA) pass through
# untouched. The "(Intercept)" row IS exponentiated because exp()
# of the intercept is the baseline odds / rate / ... -- meaningful
# in its own right (Stata reports it; SPSS reports it; APA Manual 7
# Section 6.46 example).
apply_exponentiate_to_coefs <- function(coefs) {
  if (is.null(coefs) || nrow(coefs) == 0L) {
    return(coefs)
  }
  is_b_or_beta <- coefs$estimate_type %in% c("B", "beta")
  is_eligible <- is_b_or_beta &
    !coefs$is_singular &
    !coefs$is_reference &
    !is.na(coefs$estimate)
  if (!any(is_eligible)) {
    return(coefs)
  }

  rows <- which(is_eligible)
  est_orig <- coefs$estimate[rows]
  se_orig <- coefs$se[rows]

  exp_est <- exp(est_orig)
  coefs$estimate[rows] <- exp_est
  coefs$ci_low[rows] <- exp(coefs$ci_low[rows])
  coefs$ci_high[rows] <- exp(coefs$ci_high[rows])
  # Delta-method: Var(g(X)) ~ (g'(X))^2 x Var(X) ; for g = exp,
  # g'(X) = exp(X), so SE_exp = exp(B) x SE_logit.
  coefs$se[rows] <- exp_est * se_orig
  # Statistic (z) and p_value: invariant under exp() -- the test of
  # H0: B = 0 <-> H0: exp(B) = 1 has the same z and p. Leave as-is.
  coefs
}


# Phase 7c16: frame-schema sibling of apply_exponentiate_to_coefs().
# The mixed-effects methods produce coefs in the frame schema
# (`std_error`, `ci_lower`, `ci_upper`, `is_ref`, NO `is_singular`)
# rather than the legacy long-format (`se`, `ci_low`, `ci_high`,
# `is_reference`, `is_singular`); operate directly on the frame-schema
# columns so we don't need a round-trip translation.
#
# AME rows pass through unchanged: marginaleffects already returns the
# response-scale effect, so exponentiating again would be wrong (and
# the "OR" / "IRR" / ... label only applies to B / beta rows).
apply_exponentiate_to_frame_coefs <- function(coefs) {
  if (is.null(coefs) || nrow(coefs) == 0L) {
    return(coefs)
  }
  is_b_or_beta <- coefs$estimate_type %in% c("B", "beta")
  is_eligible <- is_b_or_beta &
    !(coefs$is_ref %in% TRUE) &
    !is.na(coefs$estimate)
  if (!any(is_eligible)) {
    return(coefs)
  }

  rows <- which(is_eligible)
  est_orig <- coefs$estimate[rows]
  se_orig <- coefs$std_error[rows]

  exp_est <- exp(est_orig)
  coefs$estimate[rows] <- exp_est
  coefs$ci_lower[rows] <- exp(coefs$ci_lower[rows])
  coefs$ci_upper[rows] <- exp(coefs$ci_upper[rows])
  coefs$std_error[rows] <- exp_est * se_orig # Delta-method.
  # Statistic and p-value invariant under exp(): leave as-is.
  coefs
}


# Apply exp() to a frame's B / beta rows when exponentiate = TRUE, driven
# entirely by the frame's own info$family / info$supports -- NOT stats::family(),
# which errors for polr / clm / multinom / fixest / betareg / mlogit (no family()
# method). This is the class-agnostic generalisation of
# .apply_exp_to_survival_frame() / .apply_exp_to_mixed_frame(): any
# as_regression_frame.* method that sets info$family$link and
# info$supports$exponentiate can route its coefs through it before assembling
# the frame, and the title/footer layer reads info$extras$exp_applied /
# exp_header without any class-specific branching.
#
# A no-op (coefs pass through unchanged) when exponentiate is not TRUE or the
# frame does not advertise supports$exponentiate.
.apply_exp_to_frame <- function(coefs, info, exponentiate) {
  # Already exponentiated by a self-applying method (lm/glm legacy, merMod /
  # glmmTMB / nlme mixed, coxph / survreg)? Leave it untouched. This guard
  # makes a second, central application in the table_regression() orchestrator
  # a no-op, so exp() is never applied twice.
  if (isTRUE(info$extras$exp_applied)) {
    return(list(coefs = coefs, info = info))
  }
  # exp() of an identity-link coefficient is meaningless (the coef IS the
  # effect on the response scale). Some classes advertise
  # supports$exponentiate = TRUE generically (e.g. gaussian glm), so guard on
  # the link too -- mirrors the identity no-op the lm/glm and mixed paths
  # already apply. Survival reaches here only via .apply_exp_to_survival_frame,
  # which has already set exp_applied = TRUE, so its log-scale links are safe.
  if (
    !isTRUE(exponentiate) ||
      !isTRUE(info$supports$exponentiate) ||
      identical(info$family$link, "identity")
  ) {
    return(list(coefs = coefs, info = info))
  }
  # Link gate (G1): non-ratio links (probit, cauchit, inverse, ...) hard
  # error rather than silently printing a meaningless exp(B) column.
  .assert_exp_link_ok(
    info$family$family,
    info$family$link,
    model_id = coefs$model_id[1L]
  )
  coefs <- apply_exponentiate_to_frame_coefs(coefs)
  info$extras$exp_applied <- TRUE
  info$extras$exp_header <- spicy_glm_exp_header(
    info$family$family,
    info$family$link
  )
  list(coefs = coefs, info = info)
}


# ---- Partial likelihood-ratio chi-square ---------------------------------

# Term-level partial chi^2 -- the glm analog of the Type-II partial
# F-test in lm. For each model term T, compare by likelihood ratio the
# two nested models { all terms that do NOT contain T } vs { those
# terms + T } (Type II; Fox & Weisberg 2019; `car::Anova(type = 2,
# test.statistic = "LR")`):
#
#   LR = 2 * (LL_with - LL_without)  ~  chi^2(df = rank difference)
#
# Both nested models exclude every higher-order relative of T, so the
# main-effect test under interactions respects marginality and is
# invariant to the factor coding. For additive models (and for the
# highest-order term of any model) this equals `drop1(test = "LRT")`.
# For factor terms with k levels, the test is joint over all k-1
# dummies and df = k-1. Long & Freese 2014 Section 3.5 discuss the
# term-level LRT itself.
#
# Returns NULL on any failure (refit error, non-finite chi-square,
# etc.) so the caller can skip the term and the renderer en-dashes the
# cells. Quasi families (quasibinomial / quasipoisson / quasi) have no
# proper log-likelihood, so the LRT is undefined; we return NULL --
# consistent with how the pseudo-R^2 family handles them.
compute_partial_chi2_for_term <- function(fit, term_label) {
  if (!inherits(fit, "glm")) {
    return(NULL)
  }
  if (grepl("^quasi", stats::family(fit)$family)) {
    return(NULL)
  }
  out <- tryCatch(
    suppressWarnings(compute_glm_type2_lrt(fit, term_label)),
    error = function(e) NULL
  )
  if (is.null(out)) {
    return(NULL)
  }
  chi2 <- out$chi2
  df1 <- out$df
  if (!is.finite(chi2) || !is.finite(df1) || df1 < 1L || chi2 < 0) {
    return(NULL)
  }
  p_value <- stats::pchisq(chi2, df = df1, lower.tail = FALSE)
  list(chi2 = chi2, df = as.integer(df1), p_value = p_value)
}

# Internal: Type-II nested LRT for one glm term by refits on the
# fitted estimation sample. Mirrors the `stats::drop1.glm` internals --
# column subsets of the fitted model.matrix refit via glm.fit with the
# fit's own response, prior weights, offset, family, and control, so
# the user's `data` expression is never re-evaluated and the
# additive-case values are identical to the previous drop1-based ones.
# The statistic scaling follows drop1(test = "LRT") / stat.anova: raw
# deviance difference when the dispersion is fixed at 1 (binomial /
# poisson), the profile-likelihood form n * log(RSS ratio) for
# gaussian, and the dispersion-scaled deviance difference for the
# other estimated-dispersion families (Gamma, inverse.gaussian). df is
# the difference of effective ranks (collinearity-safe).
compute_glm_type2_lrt <- function(fit, term_label) {
  x <- stats::model.matrix(fit)
  asgn <- attr(x, "assign")
  if (is.null(asgn) || length(asgn) != ncol(x)) {
    return(NULL)
  }
  masks <- type2_nested_column_masks(stats::terms(fit), asgn, term_label)
  if (is.null(masks)) {
    return(NULL)
  }
  n <- nrow(x)
  y <- fit$y
  if (is.null(y)) {
    y <- stats::model.response(stats::model.frame(fit))
    if (!is.factor(y)) {
      storage.mode(y) <- "double"
    }
  }
  wt <- fit$prior.weights
  if (is.null(wt)) {
    wt <- rep.int(1, n)
  }
  dev_rank <- function(keep) {
    z <- stats::glm.fit(
      x[, keep, drop = FALSE],
      y,
      weights = wt,
      offset = fit$offset,
      family = fit$family,
      control = fit$control
    )
    list(dev = z$deviance, rank = z$rank)
  }
  base <- dev_rank(masks$keep_base)
  augmented <- if (masks$has_relatives) {
    dev_rank(masks$keep_with)
  } else {
    # No higher-order relative contains the term: the augmented model
    # IS the fitted model, so reuse its deviance and rank.
    list(dev = fit$deviance, rank = fit$rank)
  }
  df1 <- augmented$rank - base$rank
  if (!is.finite(df1) || df1 < 1L) {
    return(NULL)
  }
  chi2 <- if (identical(fit$family$family, "gaussian")) {
    n * log(base$dev / n) - n * log(augmented$dev / n)
  } else {
    dispersion <- summary(fit, dispersion = NULL)$dispersion
    (base$dev - augmented$dev) / dispersion
  }
  list(chi2 = max(0, chi2), df = df1)
}
