# Internal glm-specific computation backbone for table_regression().
# Mirrors R/lm_compute.R. Contents:
#   * Family / link helpers (title prefix, exponentiate header)
#   * Per-coef inference: the Wald test summary() reports (t when the
#     dispersion is estimated, z otherwise), with CR* Satterthwaite
#     branch via clubSandwich
#   * Pseudo-R^2 family: McFadden, Nagelkerke, Tjur
#   * Term-level partial chi-square via the Type-II nested LRT
#   * apply_exponentiate_to_coefs(): exp() transform on coefs +
#     CIs + delta-method SE

# ---- Family / link introspection -----------------------------------------

# Return a small descriptor for a glm fit summarizing its family
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
  # MASS::negative.binomial(theta) and mgcv's nb() put the theta in the
  # family name, "Negative Binomial(1.5)": the title names the family,
  # never the number.
  if (grepl("^Negative Binomial\\(", family_name)) {
    return("Negative-binomial regression")
  }
  # statmod::tweedie() names its family "Tweedie", mgcv's Tweedie(p)
  # "Tweedie(1.5)": the title names the family, as the glmmTMB, GAM and
  # univariable titles do.
  if (grepl("^Tweedie", family_name)) {
    return("Tweedie regression")
  }
  # gaussian + identity is lm's model, titled as lm titles it; a gaussian
  # with another link is no longer linear in the mean, so it is named by
  # its family. Any unrecognized family falls through to the generic
  # "Regression" prefix.
  if (identical(family_name, "gaussian")) {
    if (identical(link_name, "identity")) {
      return("Linear regression")
    }
    return("Gaussian regression")
  }
  "Regression"
}

spicy_glm_exp_header <- function(family_name, link_name) {
  # brms spells the single-trial binomial "bernoulli": same family,
  # same estimands (OR / RR / HR per link). Normalizing here keeps
  # info$family$family truthful ("bernoulli") while the header map
  # stays engine-consistent with glm / rstanarm.
  if (identical(family_name, "bernoulli")) {
    family_name <- "binomial"
  }
  if (identical(family_name, "binomial") && identical(link_name, "logit")) {
    return(spicy_str("header_exp_or"))
  }
  if (identical(family_name, "poisson") && identical(link_name, "log")) {
    return(spicy_str("header_exp_irr"))
  }
  if (identical(family_name, "binomial") && identical(link_name, "cloglog")) {
    return(spicy_str("header_exp_hr"))
  }
  if (identical(family_name, "binomial") && identical(link_name, "log")) {
    return(spicy_str("header_exp_rr"))
  }
  if (identical(family_name, "Gamma") && identical(link_name, "log")) {
    return(spicy_str("header_exp_mr")) # mean ratio
  }
  if (identical(family_name, "cox") && identical(link_name, "log")) {
    return(spicy_str("header_exp_hr")) # hazard ratio (coxph / rms::cph)
  }
  if (identical(family_name, "cumulative") && identical(link_name, "logit")) {
    return(spicy_str("header_exp_or")) # proportional-odds ratio (ordinal: polr / clm)
  }
  if (identical(family_name, "cumulative") && identical(link_name, "cloglog")) {
    return(spicy_str("header_exp_hr")) # grouped-time hazard ratio (ordinal cloglog link)
  }
  if (identical(family_name, "multinomial") && identical(link_name, "logit")) {
    return(spicy_str("header_exp_or")) # multinomial odds / relative-risk ratio (multinom / mlogit)
  }
  if (identical(family_name, "beta") && identical(link_name, "logit")) {
    return(spicy_str("header_exp_or")) # betareg mean model (logit link)
  }
  if (identical(family_name, "negbin") && identical(link_name, "log")) {
    return(spicy_str("header_exp_irr")) # negative-binomial rate ratio (fixest / pscl)
  }
  if (
    family_name %in%
      c("negbinomial", "geometric", "neg_binomial_2") &&
      identical(link_name, "log")
  ) {
    return(spicy_str("header_exp_irr")) # brms / rstanarm count-family spellings (log link)
  }
  if (identical(family_name, "gamma") && identical(link_name, "log")) {
    return(spicy_str("header_exp_mr")) # brms lowercase gamma family (log link)
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
    return(spicy_str("header_exp_irr")) # glmmTMB count families (log link)
  }
  if (grepl("^Negative Binomial", family_name) && identical(link_name, "log")) {
    return(spicy_str("header_exp_irr")) # MASS::glm.nb family string is "Negative Binomial(theta)"
  }
  if (
    identical(family_name, "quasibinomial") && identical(link_name, "logit")
  ) {
    return(spicy_str("header_exp_or")) # survey::svyglm(family = quasibinomial())
  }
  if (identical(family_name, "quasipoisson") && identical(link_name, "log")) {
    return(spicy_str("header_exp_irr")) # survey::svyglm(family = quasipoisson())
  }
  spicy_str("header_exp_generic")
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
# labels cauchit "Risk Ratio") -- exactly the mislabeling spicy gates
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

.assert_exp_link_ok <- function(
  family_name,
  link_name,
  model_id = NULL,
  call = rlang::caller_env()
) {
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
    class = "spicy_invalid_input",
    call = call
  )
}


# ---- Pseudo-R^2 family ----------------------------------------------------

# The R-squared rows a glm shows by default
# (dev/decisions/2026-11-14-pseudo-r2-defaults.md): Tjur's then McFadden's
# for a logit on a binary response, McFadden's alone for the other
# binomial fits (probit, cloglog, grouped responses), Poisson and negative
# binomial, and rms::lrm, the deviance R^2 for the continuous families.
# `models` are the glm-class fits of the table; the result is the union
# of their rows, in that order. mgcv's gam inherits glm but has its own
# frame and no R^2 row; quasi families have no likelihood, and no
# default row.
.glm_default_r2_tokens <- function(models) {
  toks <- unlist(lapply(models, .glm_default_r2_kind))
  intersect(c("pseudo_r2_tjur", "pseudo_r2_mcfadden", "r2_deviance"), toks)
}

.glm_default_r2_kind <- function(fit) {
  if (inherits(fit, "gam")) {
    return(character(0))
  }
  if (inherits(fit, "lrm")) {
    return("pseudo_r2_mcfadden")
  }
  if (inherits(fit, "Glm")) {
    fit <- .rms_as_glm(fit)
  }
  switch(
    .glm_r2_kind(fit),
    binomial = if (
      identical(stats::family(fit)$link, "logit") && .glm_binary_response(fit)
    ) {
      c("pseudo_r2_tjur", "pseudo_r2_mcfadden")
    } else {
      "pseudo_r2_mcfadden"
    },
    count = "pseudo_r2_mcfadden",
    continuous = "r2_deviance",
    character(0)
  )
}

# The family of a glm as the R^2 measures read it. McFadden's and
# Nagelkerke's R^2 need a likelihood that is a probability (binomial,
# Poisson, negative binomial): for a density they change with the unit
# of y (Cameron & Windmeijer 1997, p. 333). The deviance R^2 needs a
# deviance, which every family but the binomial gets; for the binomial
# it equals McFadden's on the trials. "other" is a family none of these
# names, with neither measure.
.glm_r2_kind <- function(fit) {
  fam <- tryCatch(stats::family(fit)$family, error = function(e) NULL)
  if (!is.character(fam) || length(fam) != 1L) {
    return("other")
  }
  if (fam %in% c("binomial", "quasibinomial")) {
    return(fam)
  }
  if (identical(fam, "poisson") || grepl("^Negative Binomial\\(", fam)) {
    return("count")
  }
  if (
    fam %in%
      c("gaussian", "Gamma", "inverse.gaussian") ||
      grepl("^Tweedie", fam)
  ) {
    return("continuous")
  }
  if (grepl("^quasi", fam)) {
    return("quasi")
  }
  "other"
}

# A glm of a continuous family, as the table's refusal of McFadden and
# Nagelkerke reads it (an rms::Glm through its glm twin; mgcv's gam,
# which inherits glm, keeps its own frame and is not one).
.glm_is_continuous <- function(m) {
  if (!inherits(m, "glm") || inherits(m, c("gam", "lrm"))) {
    return(FALSE)
  }
  if (inherits(m, "Glm")) {
    m <- .rms_as_glm(m)
  }
  identical(.glm_r2_kind(m), "continuous")
}

# A fit whose frame carries the deviance R^2: a glm (an rms::Glm too) of
# any family but the binomial. Not a design, GEE or Bayesian fit, which
# inherit glm with frames of their own.
.glm_carries_r2_deviance <- function(m) {
  if (
    !inherits(m, "glm") ||
      inherits(m, c("gam", "lrm", "geeglm", "stanreg", "brmsfit")) ||
      .is_design_fit(m)
  ) {
    return(FALSE)
  }
  if (inherits(m, "Glm")) {
    m <- .rms_as_glm(m)
  }
  .glm_r2_kind(m) %in% c("quasibinomial", "count", "continuous", "quasi")
}

# TRUE when a binomial glm was fitted to one 0/1 (or factor, or logical)
# outcome per row, FALSE for a grouped response: a cbind(successes,
# failures) matrix or a proportion.
.glm_binary_response <- function(fit) {
  y <- tryCatch(
    stats::model.response(stats::model.frame(fit)),
    error = function(e) NULL
  )
  if (is.null(y) || is.matrix(y)) {
    return(FALSE)
  }
  is.factor(y) || is.logical(y) || all(y %in% c(0, 1))
}

# Pseudo-R^2 of a glm, implemented from the definitions without a
# package dependency:
#
#   * McFadden (1974)   : 1 - LL_full / LL_null.
#   * Nagelkerke (1991) : Cox and Snell's 1 - exp(2 (LL_null - LL_full) / n)
#                         over its maximum 1 - exp(2 LL_null / n) (SAS's
#                         "max-rescaled R-square", SPSS's Nagelkerke).
#   * Tjur (2009)       : mean fitted probability of the successes minus
#                         that of the failures. Binomial only.
#   * Deviance R^2      : 1 - D_full / D_null (Cameron & Windmeijer 1997,
#                         eq. 5). Every family but the binomial, where it
#                         is McFadden's (l(y; y) = 0, their p. 333).
#
# McFadden and Nagelkerke are computed for the binomial, Poisson and
# negative binomial families only (`.glm_r2_kind()`), the deviance R^2
# for the others; NA otherwise. LL_null and D_null are those of the
# intercept-only model with the fit's offset and weights
# (`.glm_null_fit()`); for a negative binomial both deviances take the
# theta of family(fit), the fitted model's, as Cameron & Windmeijer
# (1997, p. 339, note 4) propose. A binomial fit is read as Bernoulli trials,
# whatever the form of its response: one 0/1 (or factor, or logical)
# outcome per row, cbind(successes, failures), a proportion with the
# trials as weights, 0/1 rows weighted by counts. glm() stores the
# proportion of successes of row i in fit$y and its trials times the
# user's weight in fit$prior.weights (m_i), so with p_i the fitted
# probability
#
#   LL   = sum m_i [y_i log p_i + (1 - y_i) log(1 - p_i)],   n = sum m_i,
#   Tjur = sum m_i y_i p_i / sum m_i y_i
#            - sum m_i (1 - y_i) p_i / sum m_i (1 - y_i),
#
# and a grouped fit returns the numbers of the same model fitted to its
# expanded 0/1 rows. stats::logLik() is not used there: on a grouped
# response it adds the log binomial coefficients of the rows, which made
# every measure depend on how the trials were grouped (McFadden 0.28 for
# 30 rows of 10 trials, 0.10 for the same 300 trials as 0/1 rows), and it
# rounds non-integer weights. With non-integer weights these formulas are
# the weighted log-likelihood glm() maximizes, and n = the sum of the
# weights leaves every measure unchanged when the weights are rescaled
# (what SAS's NORMALIZE option does). Poisson and negative binomial fits
# take logLik(), which weights each row by its prior weight, with the
# same n. dev/decisions/2026-11-14-pseudo-r2-defaults.md has the sources.
#
# Tjur keeps its sign. D >= 0 for a logit with an intercept (Tjur 2009,
# Corollary 1), but a fixed offset or another link can make it
# negative, which an absolute value would hide.
compute_pseudo_r2_glm <- function(fit) {
  out <- list(
    mcfadden = NA_real_,
    nagelkerke = NA_real_,
    tjur = NA_real_,
    r2_deviance = NA_real_
  )
  kind <- if (inherits(fit, "glm")) .glm_r2_kind(fit) else "other"
  if (identical(kind, "other")) {
    return(out)
  }
  if (identical(kind, "binomial")) {
    out$tjur <- compute_pseudo_r2_tjur(fit)
  }
  null_fit <- .glm_null_fit(fit)
  if (is.null(null_fit)) {
    return(out) # nocov: the intercept-only refit of a converged glm
  }
  if (kind %in% c("binomial", "quasibinomial")) {
    tr <- .glm_bernoulli_trials(fit)
    ll_full <- .bernoulli_loglik(tr$y, tr$m, tr$p)
    ll_null <- .bernoulli_loglik(tr$y, tr$m, null_fit$fitted.values)
    if (identical(kind, "binomial")) {
      out[c("mcfadden", "nagelkerke")] <- .lr_pseudo_r2(
        ll_full,
        ll_null,
        sum(tr$m)
      )
    } else {
      # The quasi-binomial deviance on the trials: -2 times the Bernoulli
      # log-likelihood, whose saturated value is 0.
      out$r2_deviance <- .r2_deviance(-2 * ll_full, -2 * ll_null)
    }
    return(out)
  }
  if (identical(kind, "count")) {
    ll_full <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) {
      NA_real_ # nocov: logLik() of a Poisson or negative binomial glm
    })
    ll_null <- tryCatch(
      as.numeric(stats::logLik(null_fit)),
      error = function(e) NA_real_ # nocov: as above
    )
    out[c("mcfadden", "nagelkerke")] <- .lr_pseudo_r2(
      ll_full,
      ll_null,
      sum(fit$prior.weights)
    )
  }
  out$r2_deviance <- .r2_deviance(fit$deviance, null_fit$deviance)
  out
}

compute_pseudo_r2_mcfadden <- function(fit) {
  compute_pseudo_r2_glm(fit)$mcfadden
}

compute_pseudo_r2_nagelkerke <- function(fit) {
  compute_pseudo_r2_glm(fit)$nagelkerke
}

compute_r2_deviance <- function(fit) {
  compute_pseudo_r2_glm(fit)$r2_deviance
}

# 1 - D_full / D_null; NA for a null deviance that is not positive (a
# constant response).
.r2_deviance <- function(dev_full, dev_null) {
  if (!is.finite(dev_full) || !is.finite(dev_null) || dev_null <= 0) {
    return(NA_real_)
  }
  1 - dev_full / dev_null
}

# McFadden's and Nagelkerke's R^2 from the log-likelihoods of the fitted
# and the intercept-only model and n. Shared by the glm and multinom
# paths. NA where undefined: a log-likelihood that is not finite, a
# null log-likelihood of 0, or a Nagelkerke maximum that is not
# positive (a null log-likelihood above 0, which a density can give).
.lr_pseudo_r2 <- function(ll_full, ll_null, n) {
  if (
    !is.finite(ll_full) ||
      !is.finite(ll_null) ||
      ll_null == 0 ||
      !is.finite(n) ||
      n <= 0
  ) {
    return(list(mcfadden = NA_real_, nagelkerke = NA_real_))
  }
  cox_snell <- 1 - exp(2 * (ll_null - ll_full) / n)
  upper <- 1 - exp(2 * ll_null / n)
  list(
    mcfadden = 1 - ll_full / ll_null,
    nagelkerke = if (is.finite(upper) && upper > 0) {
      cox_snell / upper
    } else {
      NA_real_
    }
  )
}

# The Bernoulli trials of a binomial (or quasi-binomial) glm: y the
# proportion of successes of each row, m its trials times its prior
# weight, p its fitted probability. The fit's own vectors, which a
# na.exclude fit does not pad: it reads as its na.omit twin.
.glm_bernoulli_trials <- function(fit) {
  y <- fit$y
  if (is.null(y)) {
    # glm(..., y = FALSE) keeps no response: rebuild the stored one.
    y <- stats::model.response(stats::model.frame(fit))
    if (is.factor(y)) {
      y <- as.numeric(y != levels(y)[1L])
    }
    y <- .glm_stored_response(y, stats::family(fit))$y
  }
  list(
    y = as.numeric(y),
    m = as.numeric(fit$prior.weights),
    p = as.numeric(fit$fitted.values)
  )
}

# sum m [y log p + (1 - y) log(1 - p)], with 0 log 0 = 0.
.bernoulli_loglik <- function(y, m, p) {
  sum(
    m * (ifelse(y > 0, y * log(p), 0) + ifelse(y < 1, (1 - y) * log1p(-p), 0))
  )
}

# Internal: stored-y representation of a glm response, for refits.
# A two-column binomial / quasibinomial `cbind(successes, failures)`
# response must never be passed back to `glm()` / `glm.fit()` together
# with the fit's stored weights: `stats::weights(fit)` and
# `fit$prior.weights` hold the POST-initialize weights (user weights
# times the row totals -- the binomial `initialize` multiplies the
# totals in; see the Note in `?glm`), and re-running `initialize` on
# the matrix multiplies the totals in AGAIN, so the refit runs on
# effective weights = totals^2. Convert once to the representation
# `glm.fit` stores after initialize -- y = successes / total (rows
# with total 0 get y = 0, the `add1.glm` convention), weights kept
# as passed (or the totals when none resolved) -- and a refit
# reproduces the original fit exactly. Vector and factor responses,
# other families, and wider matrices pass through untouched.
# Reference: R bug #19128 (`stats::drop1.glm` shares this defect).
.glm_stored_response <- function(y, family, wt = NULL) {
  if (
    is.matrix(y) &&
      ncol(y) == 2L &&
      family$family %in% c("binomial", "quasibinomial")
  ) {
    tot <- y[, 1L] + y[, 2L]
    list(y = ifelse(tot == 0, 0, y[, 1L] / tot), wt = wt %||% tot)
  } else {
    list(y = y, wt = wt)
  }
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
# full model includes a real-rate offset: the null model must carry
# the same offset as the full model (the Stata convention -- its
# reported pseudo-R^2 keeps the offset in the null fit; on offsets
# see Long & Freese 2014 Section 9.2.6), otherwise pseudo-R^2 is
# not a valid 0-1 statistic.
#
# Workaround: extract the *evaluated* response, weights, and
# offset from the model frame and refit on a fresh data.frame.
# A two-column `cbind()` response is converted to the stored-y
# representation first (`.glm_stored_response()` above): the
# extracted weights are already post-initialize (the row totals),
# so refitting on the raw matrix re-ran the binomial `initialize`
# and evaluated ll_null under effective weights = totals^2 --
# McFadden read 0.92 instead of 0.32 on real grouped data.
# The weights are the fit's own prior weights, which a na.exclude fit
# does not pad (stats::weights() does, and the refit then failed).
# Falls back to NULL (NA for the log-likelihood) on any failure.
.glm_null_fit <- function(fit) {
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(mf)) {
    return(NULL) # nocov: a converged glm always carries a model.frame
  }
  y <- tryCatch(stats::model.response(mf), error = function(e) NULL)
  if (is.null(y)) {
    return(NULL) # nocov: a glm model.frame always has a response column
  }
  fam <- stats::family(fit)
  offset_vec <- tryCatch(stats::model.offset(mf), error = function(e) NULL)
  cv <- .glm_stored_response(y, fam, fit$prior.weights)
  args <- list(formula = y ~ 1, family = fam, data = data.frame(y = cv$y))
  if (!is.null(cv$wt)) {
    args$weights <- cv$wt
  }
  if (!is.null(offset_vec)) {
    args$offset <- offset_vec
  }
  tryCatch(
    suppressWarnings(do.call(stats::glm, args)),
    error = function(e) NULL
  )
}

compute_intercept_only_loglik_glm <- function(fit) {
  null_fit <- .glm_null_fit(fit)
  if (is.null(null_fit)) {
    return(NA_real_)
  }
  tryCatch(as.numeric(stats::logLik(null_fit)), error = function(e) NA_real_)
}

compute_pseudo_r2_tjur <- function(fit) {
  if (!inherits(fit, "glm")) {
    return(NA_real_)
  }
  if (!identical(stats::family(fit)$family, "binomial")) {
    return(NA_real_)
  }
  tr <- .glm_bernoulli_trials(fit)
  s1 <- sum(tr$m * tr$y)
  s0 <- sum(tr$m * (1 - tr$y))
  if (!(s1 > 0 && s0 > 0)) {
    return(NA_real_)
  }
  sum(tr$m * tr$y * tr$p) / s1 - sum(tr$m * (1 - tr$y) * tr$p) / s0
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
#
# `negate = TRUE` (cumulative cloglog only): the model is parametrized
# cloglog P(Y <= j) = zeta_j - xB, so the grouped-time proportional-
# hazards ratio (Prentice & Gloeckler 1978; McCullagh 1980) is
# exp(-B), NOT exp(B) -- exp(B) would be its reciprocal. Verified
# against a person-period discrete-time cloglog GLM oracle (true
# HR = 2 recovered as exp(-B) = 1.99; exp(B) = 0.50), 2026-08-05.
# Under g(x) = exp(-x): the CI endpoints swap ([exp(-U), exp(-L)]),
# |g'(x)| = exp(-x) so SE = exp(-B) x SE_link, and the H0: B = 0
# statistic and p-value stay invariant.
apply_exponentiate_to_frame_coefs <- function(coefs, negate = FALSE) {
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

  if (isTRUE(negate)) {
    exp_est <- exp(-est_orig)
    ci_lo <- exp(-coefs$ci_upper[rows])
    ci_hi <- exp(-coefs$ci_lower[rows])
  } else {
    exp_est <- exp(est_orig)
    ci_lo <- exp(coefs$ci_lower[rows])
    ci_hi <- exp(coefs$ci_upper[rows])
  }
  coefs$estimate[rows] <- exp_est
  coefs$ci_lower[rows] <- ci_lo
  coefs$ci_upper[rows] <- ci_hi
  coefs$std_error[rows] <- exp_est * se_orig # Delta-method.
  # Statistic and p-value invariant under exp(): leave as-is.
  coefs
}


# Apply exp() to a frame's B / beta rows when exponentiate = TRUE, driven
# entirely by the frame's own info$family / info$supports -- NOT stats::family(),
# which errors for polr / clm / multinom / fixest / betareg / mlogit (no family()
# method). This is the class-agnostic generalization of
# .apply_exp_to_survival_frame() / .apply_exp_to_mixed_frame(): any
# as_regression_frame.* method that sets info$family$link and
# info$supports$exponentiate can route its coefs through it before assembling
# the frame, and the title/footer layer reads info$extras$exp_applied /
# exp_header without any class-specific branching.
#
# A no-op (coefs pass through unchanged) when exponentiate is not TRUE or the
# frame does not advertise supports$exponentiate. `call` is the function the
# user called: its refusals name it.
.apply_exp_to_frame <- function(
  coefs,
  info,
  exponentiate,
  call = rlang::caller_env()
) {
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
    model_id = coefs$model_id[1L],
    call = call
  )
  # Cumulative cloglog: the HR of the grouped-time proportional-hazards
  # reading is exp(-B) under the zeta_j - xB parametrization (see
  # apply_exponentiate_to_frame_coefs); the footer discloses the sign.
  negate_hr <- identical(info$family$family, "cumulative") &&
    identical(info$family$link, "cloglog")
  if (negate_hr) {
    # Nominal (partial-PO) rows sit on the THRESHOLD side of the
    # parametrization: their exponential is a cut-specific baseline
    # cumulative-hazard ratio, not a covariate hazard ratio. One column
    # cannot honestly mix exp(-B) HRs with exp(+a) baseline ratios, so
    # the exotic combination is refused rather than mislabeled.
    pv <- coefs$parent_var %||% rep(NA_character_, nrow(coefs))
    # Deliberately the ONE block, not `.reg_is_block()`: the refusal is
    # about the threshold-side parametrization of partial-PO terms.
    if (any(pv %in% .REG_BLOCK_NPO)) {
      spicy_abort(
        c(
          paste0(
            "`exponentiate = TRUE` is not available for a cloglog ",
            "cumulative model with `nominal =` terms."
          ),
          "i" = paste0(
            "Proportional coefficients exponentiate to hazard ",
            "ratios as exp(-B), but nominal (threshold-side) ",
            "coefficients would exponentiate to cut-specific ",
            "baseline ratios -- one column cannot mix the two."
          ),
          "i" = "Report the link-scale coefficients instead."
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }
  coefs <- apply_exponentiate_to_frame_coefs(coefs, negate = negate_hr)
  info$extras$exp_applied <- TRUE
  if (negate_hr) {
    info$extras$exp_hr_negated <- TRUE
  }
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
# dummies and df = k-1. Long & Freese 2014 Sections 3.2.2 (Wald vs
# LR tests) and 3.2.4 (LR tests with lrtest) discuss the LRT itself.
#
# Returns NULL on any failure (refit error, non-finite chi-square,
# etc.) so the caller can skip the term and the renderer en-dashes the
# cells. Quasi families (quasibinomial / quasipoisson / quasi) have no
# proper log-likelihood, so the LRT is undefined; we return NULL --
# consistent with how the pseudo-R^2 family handles them. The table
# never asks this function for a glm whose dispersion is estimated: its
# term-level test is the partial F below (`extract_partial_effect_rows()`
# routes it there).
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

# Term-level partial F of a glm whose dispersion is estimated
# (`has_estimated_dispersion()`: gaussian, Gamma, inverse.gaussian,
# Tweedie, negative.binomial(theta) in glm(), the quasi families): the F
# that `anova(reduced, full, test = "F")` and `car::Anova(fit, type = 2,
# test.statistic = "F", error.estimate = "dispersion")` report,
#
#   F = (Delta D / q) / phi  ~  F(q, df.residual(fit))
#
# with Delta D the deviance difference of the same Type-II refits as
# the chi-square above (`compute_glm_type2_lrt()`), q their rank
# difference and phi the Pearson dispersion of the full model, as
# summary() estimates it (car's default "pearson" estimate, taken at the
# final means, differs by the IRLS convergence error only).
# `drop1(test = "F")` divides by the deviance dispersion D / df.residual
# instead and is not this test. The quasi families get it too: the F
# needs no likelihood. `phi`, read once per fit by the row builder, is
# taken here when the function is called alone. Returns NULL on any
# failure, like the chi-square.
compute_partial_f_for_term <- function(fit, term_label, phi = NULL) {
  if (is.null(phi)) {
    if (!suppressWarnings(has_estimated_dispersion(fit))) {
      return(NULL)
    }
    phi <- .glm_dispersion_quiet(fit)
  }
  out <- tryCatch(
    suppressWarnings(compute_glm_type2_lrt(fit, term_label)),
    error = function(e) NULL
  )
  if (is.null(out)) {
    return(NULL)
  }
  df1 <- out$df
  df2 <- stats::df.residual(fit)
  # Defensive floor: a refit that lands a hair below the full deviance
  # gives F = 0, not a negative F.
  f <- max(0, out$dev_diff) / df1 / phi
  if (!is.finite(f) || !isTRUE(df2 > 0)) {
    return(NULL)
  }
  list(
    f = f,
    df = as.integer(df1),
    df_residual = df2,
    p_value = stats::pf(f, df1, df2, lower.tail = FALSE)
  )
}

# The dispersion summary() estimates, without its warnings (zero prior
# weights): the coefficient rows report them once already. NA when
# summary() fails.
.glm_dispersion_quiet <- function(fit) {
  tryCatch(
    suppressWarnings(summary(fit)$dispersion),
    error = function(e) NA_real_
  )
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
# the difference of effective ranks (collinearity-safe). `dev_diff`, the
# unscaled deviance difference, feeds the partial F of
# `compute_partial_f_for_term()`.
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
  wt <- fit$prior.weights
  if (is.null(y)) {
    y <- stats::model.response(stats::model.frame(fit))
    if (!is.factor(y)) {
      storage.mode(y) <- "double"
    }
    # `glm(..., y = FALSE)` + a two-column `cbind(successes, failures)`
    # response: `model.response()` gives back the raw matrix while
    # `fit$prior.weights` already stores the POST-initialize weights --
    # the exact input `.glm_stored_response()` exists for (feeding the
    # matrix to `glm.fit()` would square the totals; this site was the
    # in-house precedent before the conversion became the shared
    # helper). The converted fallback is identical to the `fit$y` path.
    # (`stats::drop1.glm()` has the same naive reconstruction and the
    # same defect on this input, so it is NOT mirrored here.)
    cv <- .glm_stored_response(y, fit$family, wt)
    y <- cv$y
    wt <- cv$wt
  }
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
  list(chi2 = max(0, chi2), df = df1, dev_diff = base$dev - augmented$dev)
}
