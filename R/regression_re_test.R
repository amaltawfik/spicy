# Opt-in per-term significance tests for random-effect variance components
# (`table_regression(re_test = )`). Fills the statistic / df / p_value columns
# of the "Random effects" rows block, which are NA by default.
#
# Never a Wald z: H0 sigma = 0 sits on the boundary of the parameter space, so
# the estimate/SE ratio is invalid there (Self & Liang 1987). Two engines:
#
#   * "lrt"  -- per-term likelihood-ratio test: refit the model WITHOUT the
#     random term (its variance + its covariances with the other terms of the
#     same bar) and compare log-likelihoods, referring the statistic to the
#     chi-bar-squared mixture 0.5 * chi2_{q-1} + 0.5 * chi2_{q}, where q =
#     1 + (number of dropped covariances) (Self & Liang 1987; Stram & Lee
#     1994). REML fits are compared on the REML likelihood -- valid for nested
#     RANDOM structures with an identical fixed part (Pinheiro & Bates 2000).
#     This is the lmerTest::ranova() reduction scheme with the boundary
#     correction ranova leaves out (its plain chi-square p is conservative).
#     Slopes are tested by removal from their bar; a bar's intercept is tested
#     only when it is the bar's ONLY term (dropping the whole bar), matching
#     ranova. Supported: lmer / glmer / glmmTMB.
#
#   * "rlrt" -- exact restricted likelihood-ratio test via RLRsim::exactRLRT()
#     (Crainiceanu & Ruppert 2004; Scheipl, Greven & Kuchenhoff 2008), the
#     finite-sample-exact test. Only defined for a Gaussian lmer fit with a
#     SINGLE variance component; anything richer errors with a pointer to
#     "lrt".
#
# The whole-block significance signal (LR test vs the no-random-effects model,
# chi-bar-squared) stays in the footer regardless of `re_test`.


# Dispatch. Returns a data.frame(group, term, statistic, df, p_value) whose
# (group, term) keys match the variance_components rows, or NULL when nothing
# is testable. Failures of individual refits leave that term untested (NA
# row) with a spicy_fallback warning rather than erroring the whole table.
.compute_re_term_tests <- function(fit, re_test) {
  if (identical(re_test, "rlrt")) {
    .re_term_tests_rlrt(fit)
  } else if (inherits(fit, "lme")) {
    .re_term_tests_lrt_lme(fit)
  } else {
    .re_term_tests_lrt(fit)
  }
}


.re_term_tests_lrt <- function(fit) {
  f <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (is.null(f)) return(NULL)                                         # nocov
  bars <- tryCatch(.re_findbars(f), error = function(e) NULL)
  if (is.null(bars) || length(bars) == 0L) return(NULL)                # nocov
  ll_full <- as.numeric(stats::logLik(fit))

  out <- list()
  for (b in bars) {
    grp <- deparse1(b[[3L]])
    tt <- stats::terms(stats::reformulate(deparse1(b[[2L]])))
    slopes <- attr(tt, "term.labels")
    has_int <- attr(tt, "intercept") == 1L
    n_terms_bar <- length(slopes) + as.integer(has_int)

    # ranova reduction scheme: test each slope; test the intercept only when
    # it is the bar's only term (the test then drops the whole bar).
    tests <- c(slopes, if (has_int && length(slopes) == 0L) "(Intercept)")
    for (s in tests) {
      # Dropping the model's ONLY bar leaves no random part -- lmer / glmer /
      # glmmTMB cannot fit that reduced model. The test is then exactly the
      # whole-block LR vs the fixed-effects-only model, already computed (on
      # the ML scale) by the null-LRT machinery: reuse it.
      if (identical(s, "(Intercept)") && length(bars) == 1L) {
        nl <- .compute_null_model_lrt(fit)
        if (!is.null(nl)) {
          out[[length(out) + 1L]] <- data.frame(
            group = grp, term = s,
            statistic = nl$chi2, df = as.numeric(nl$df),
            p_value = nl$p_chibar2,
            stringsAsFactors = FALSE
          )
        }
        next
      }
      red <- .refit_without_re_term(fit, f, bars, b, s, slopes, has_int)
      if (is.null(red)) {
        spicy_warn(
          c(
            sprintf(
              "Per-term LR test: the reduced refit for random term `%s` (%s) failed.",
              s, grp
            ),
            "i" = "The row's test columns stay NA."
          ),
          class = "spicy_fallback"
        )
        next
      }
      chi2 <- max(0, 2 * (ll_full - as.numeric(stats::logLik(red))))
      # Dropping one variance + its covariances with the (q - 1) other terms
      # of the bar: chi-bar-squared mixture 0.5 chi2_{q-1} + 0.5 chi2_{q}.
      q <- 1L + (n_terms_bar - 1L)
      p <- 0.5 * stats::pchisq(chi2, df = q, lower.tail = FALSE) +
        if (q - 1L > 0L) {
          0.5 * stats::pchisq(chi2, df = q - 1L, lower.tail = FALSE)
        } else {
          0.5 * as.numeric(chi2 <= 0)   # chi2_0 point mass at 0
        }
      out[[length(out) + 1L]] <- data.frame(
        group = grp, term = s,
        statistic = chi2, df = as.numeric(q), p_value = p,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) == 0L) return(NULL)                                  # nocov
  do.call(rbind, out)
}


# Refit `fit` with random term `s` removed from bar `b` (slopes), or with the
# whole bar removed (s == "(Intercept)", intercept-only bar). Returns NULL on
# any failure. When removing the last remaining bar the model has no random
# part left -- delegate to the null-model machinery used by the footer LRT.
.refit_without_re_term <- function(fit, f, bars, b, s, slopes, has_int) {
  bar_str <- function(bb) {
    paste0("(", deparse1(bb[[2L]]), " | ", deparse1(bb[[3L]]), ")")
  }
  this_bar <- bar_str(b)
  other_bars <- vapply(bars, bar_str, character(1))
  other_bars <- other_bars[other_bars != this_bar]

  new_bar <- if (identical(s, "(Intercept)")) {
    character(0)                       # drop the whole intercept-only bar
  } else {
    keep <- setdiff(slopes, s)
    lhs <- paste(c(if (has_int) "1" else "0", keep), collapse = " + ")
    paste0("(", lhs, " | ", deparse1(b[[3L]]), ")")
  }

  fixed_rhs <- deparse1(.re_nobars(f)[[3L]])
  rhs_parts <- c(fixed_rhs, other_bars, new_bar)
  new_f <- tryCatch(
    stats::as.formula(
      paste(deparse1(f[[2L]]), "~", paste(rhs_parts, collapse = " + ")),
      env = environment(f)
    ),
    error = function(e) NULL
  )
  if (is.null(new_f)) return(NULL)                                     # nocov

  tryCatch(
    suppressWarnings(suppressMessages(stats::update(fit, formula = new_f))),
    error = function(e) NULL
  )
}


# nlme::lme sibling of .re_term_tests_lrt(). The random structure lives in
# the `random =` argument (not the formula), so the reduced refits go through
# `update(fit, random = ...)`. Only the simple single-level formula case
# (`~ terms | group`, incl. the `random = ~ terms` shorthand) is handled;
# pdMat / multilevel list structures leave the rows untested with a warning.
.re_term_tests_lrt_lme <- function(fit) {
  rnd <- tryCatch(fit$call$random, error = function(e) NULL)
  rnd <- tryCatch(eval(rnd, envir = parent.frame()), error = function(e) rnd)
  unsupported <- function() {
    spicy_warn(
      c(
        "Per-term LR tests support only a simple `random = ~ terms | group` lme structure.",
        "i" = "The random-effect rows' test columns stay NA."
      ),
      class = "spicy_fallback"
    )
    NULL
  }
  if (!inherits(rnd, "formula") || length(rnd) != 2L) return(unsupported())
  rhs <- rnd[[2L]]
  if (is.call(rhs) && identical(rhs[[1L]], as.name("|"))) {
    lhs_expr <- rhs[[2L]]
    grp <- deparse1(rhs[[3L]])
  } else {
    # `random = ~ terms` shorthand: the group comes from the fitted object.
    lhs_expr <- rhs
    grp <- names(fit$groups)[1L]
  }
  tt <- tryCatch(stats::terms(stats::reformulate(deparse1(lhs_expr))),
                 error = function(e) NULL)
  if (is.null(tt)) return(unsupported())                               # nocov
  slopes <- attr(tt, "term.labels")
  has_int <- attr(tt, "intercept") == 1L
  n_terms_bar <- length(slopes) + as.integer(has_int)
  ll_full <- as.numeric(stats::logLik(fit))

  out <- list()
  tests <- c(slopes, if (has_int && length(slopes) == 0L) "(Intercept)")
  for (s in tests) {
    if (identical(s, "(Intercept)")) {
      # Dropping the only random term leaves no random part: reuse the
      # whole-block null LRT (ML refit vs gls/lm).
      nl <- .compute_null_model_lrt(fit)
      if (!is.null(nl)) {
        out[[length(out) + 1L]] <- data.frame(
          group = grp, term = s,
          statistic = nl$chi2, df = as.numeric(nl$df),
          p_value = nl$p_chibar2,
          stringsAsFactors = FALSE
        )
      }
      next
    }
    keep <- setdiff(slopes, s)
    new_rnd <- tryCatch(
      stats::as.formula(
        paste0("~", paste(c(if (has_int) "1" else "0", keep),
                          collapse = " + "), " | ", grp),
        env = environment(stats::formula(fit))
      ),
      error = function(e) NULL
    )
    # Re-evaluate the captured call with the reduced random structure.
    # stats::update(fit, random = ...) fails when nlme is not attached (the
    # captured call is unqualified `lme(...)`): requalify to nlme::lme first,
    # exactly like .null_lrt_lme().
    red <- if (is.null(new_rnd)) NULL else tryCatch({                  # nocov
      call_copy <- fit$call
      call_copy[[1L]] <- quote(nlme::lme)
      call_copy$random <- new_rnd
      suppressWarnings(suppressMessages(eval(call_copy, envir = parent.frame())))
    }, error = function(e) NULL)
    if (is.null(red)) {
      spicy_warn(
        c(
          sprintf(
            "Per-term LR test: the reduced refit for random term `%s` (%s) failed.",
            s, grp
          ),
          "i" = "The row's test columns stay NA."
        ),
        class = "spicy_fallback"
      )
      next
    }
    chi2 <- max(0, 2 * (ll_full - as.numeric(stats::logLik(red))))
    q <- 1L + (n_terms_bar - 1L)
    p <- 0.5 * stats::pchisq(chi2, df = q, lower.tail = FALSE) +
      if (q - 1L > 0L) {
        0.5 * stats::pchisq(chi2, df = q - 1L, lower.tail = FALSE)
      } else {
        0.5 * as.numeric(chi2 <= 0)                                    # nocov
      }
    out[[length(out) + 1L]] <- data.frame(
      group = grp, term = s,
      statistic = chi2, df = as.numeric(q), p_value = p,
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0L) return(NULL)                                  # nocov
  do.call(rbind, out)
}


.re_term_tests_rlrt <- function(fit) {
  if (!spicy_pkg_available("RLRsim")) {
    spicy_abort(
      c(
        "`re_test = \"rlrt\"` requires the RLRsim package.",
        "i" = "Install it with `install.packages(\"RLRsim\")`, or use `re_test = \"lrt\"`."
      ),
      class = "spicy_missing_pkg"
    )
  }
  # exactRLRT() is exact for ONE variance component in a Gaussian LMM
  # (lmer or lme fits are both accepted by RLRsim).
  is_lmer <- inherits(fit, "lmerMod") && !inherits(fit, "glmerMod")
  is_lme  <- inherits(fit, "lme")
  n_var <- if (is_lmer) {
    vc <- lme4::VarCorr(fit)
    sum(vapply(vc, function(m) nrow(as.matrix(m)), integer(1)))
  } else if (is_lme) {
    # One unconstrained reStruct parameter <=> a single variance component.
    length(tryCatch(stats::coef(fit$modelStruct$reStruct),
                    error = function(e) numeric(2)))
  } else {
    NA_integer_
  }
  if (!(is_lmer || is_lme) || !identical(as.integer(n_var), 1L)) {
    spicy_abort(
      c(
        paste0(
          "`re_test = \"rlrt\"` (exact restricted LRT) is only defined for a ",
          "Gaussian `lmer` / `lme` fit with a single variance component."
        ),
        "i" = "Use `re_test = \"lrt\"` (chi-bar-squared LR test) for richer random structures."
      ),
      class = "spicy_unsupported"
    )
  }
  res <- tryCatch(RLRsim::exactRLRT(fit), error = function(e) NULL)
  if (is.null(res)) return(NULL)                                       # nocov
  if (is_lmer) {
    vc <- lme4::VarCorr(fit)
    grp <- names(vc)[1L]
    term <- rownames(as.matrix(vc[[1L]]))[1L]
  } else {
    grp <- names(fit$groups)[1L]
    vcn <- rownames(nlme::VarCorr(fit))
    term <- setdiff(vcn, "Residual")[1L]
  }
  data.frame(
    group = grp, term = term,
    statistic = unname(res$statistic), df = NA_real_,
    p_value = unname(res$p.value),
    stringsAsFactors = FALSE
  )
}


# Validate the `re_test` argument: "none" (default), "lrt", or "rlrt".
.validate_re_test <- function(x) {
  choices <- c("none", "lrt", "rlrt")
  if (identical(x, choices)) return("none")          # unset default vector
  if (length(x) == 1L && !is.na(x) && x %in% choices) return(x)
  spicy_abort(
    c(
      "`re_test` must be one of \"none\", \"lrt\", or \"rlrt\".",
      "x" = sprintf(
        "You supplied %s.",
        paste(encodeString(as.character(x), quote = "\""), collapse = ", ")
      ),
      "i" = paste0(
        "There is deliberately no Wald test: a variance's null hypothesis ",
        "sits on the boundary of the parameter space."
      )
    ),
    class = "spicy_invalid_input"
  )
}


# `re_ci`: uncertainty route for the random-effect variance-component
# rows. "wald" = merDeriv observed-information SE + CI (subject to the
# spicy.re_se_max_n size cap); "profile" = lme4's profile-likelihood
# intervals (no SE, asymmetric CI -- lme4's own supported route).
.validate_re_ci <- function(x, models) {
  choices <- c("wald", "profile")
  val <- if (identical(x, choices)) {
    "wald"                                           # unset default vector
  } else if (length(x) == 1L && !is.na(x) && x %in% choices) {
    x
  } else {
    spicy_abort(
      c(
        "`re_ci` must be \"wald\" or \"profile\".",
        "x" = sprintf(
          "You supplied %s.",
          paste(encodeString(as.character(x), quote = "\""), collapse = ", ")
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  if (identical(val, "profile")) {
    # Profile CIs run through lme4's confint machinery: lmer / glmer
    # only. glmmTMB and nlme::lme have their own native CI routes
    # (TMB::sdreport; apVar), which the default already uses.
    bad <- vapply(models, function(m) {
      inherits(m, c("glmmTMB", "lme"))
    }, logical(1))
    if (any(bad)) {
      spicy_abort(
        c(
          "`re_ci = \"profile\"` is available for lme4 fits (`lmer` / `glmer`) only.",
          "x" = sprintf(
            "Model%s %s %s not fitted with lme4.",
            if (sum(bad) > 1L) "s" else "",
            paste(which(bad), collapse = ", "),
            if (sum(bad) > 1L) "are" else "is"
          ),
          "i" = paste0(
            "glmmTMB and nlme::lme variance-component CIs already come ",
            "from their own engines (TMB's sdreport; nlme's apVar) under ",
            "the default `re_ci = \"wald\"`."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
  }
  val
}
