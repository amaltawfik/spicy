# The column `ame` of the registry is the contract of each class: on the
# fixtures of helper-class-fixtures.R, a cell that starts with "yes" or
# "per" gives a numeric AME for every predictor (per category or outcome
# when the cell says so) without a fallback warning, a "no" refuses the
# AME tokens, and the AME and its SE equal the hand oracle of
# helper-ame-oracle.R wherever it can be built.
# dev/decisions/2026-11-19-capability-registry.md.

skip_if_no_ame_guard <- function() {
  skip_on_cran()
  skip_if_not_installed("marginaleffects", minimum_version = "1.0.0")
  skip_if_not_installed("numDeriv")
}

# ---- What each fixture is expected to give ---------------------------------

# The fixtures whose cell holds only under a condition, each with what the
# parenthesis of the cell gives for it and the words of the cell that say
# so.
ag_conditional <- list(
  fepois = list(
    expect = "refused",
    cell = "no for a GLM with fixed effects",
    why = "marginaleffects refuses a fixest GLM with fixed effects"
  ),
  glmmTMB_ordinal = list(
    expect = "refused",
    cell = "no for the ordinal family",
    why = "no AME backend for the ordinal family of glmmTMB"
  ),
  glmmTMB_reml = list(
    expect = "refused",
    cell = "or a REML fit",
    why = "the delta method cannot perturb the fixed effects of a REML fit"
  ),
  lrm_ordinal = list(
    expect = "per",
    cell = "per category for an ordinal outcome",
    why = "an lrm of an ordinal outcome gives one AME per category"
  )
)

# How a "no" cell (or a cell without "yes" or "per") refuses the AME, as
# observed: "error" is a spicy_invalid_input error, "empty" would be blank
# AME cells with a classed warning. All refusals are errors today.
ag_refusal <- c(
  nls = "error",
  coxph = "error",
  coxph_strata = "error",
  clmm = "error",
  mlogit = "error",
  cph = "error",
  cph_xy = "error",
  flexsurvreg = "error",
  svycoxph = "error",
  selection = "error",
  fepois = "error",
  glmmTMB_ordinal = "error",
  glmmTMB_reml = "error"
)

# The predictors of a fixture when they are not `x` alone.
ag_terms <- list(
  iv_robust = "xen",
  ivreg = "xen",
  feols_iv = "xen",
  lme_contrasts = c("x", "mnB", "mnC"),
  clm_scale = c("x", "z"),
  gam = c("x", "z"),
  gam_logit = c("x", "z"),
  bam = c("x", "z"),
  stanreg = "Days",
  brmsfit = "Days"
)

# Rows of spicy's AME that depart from the contract, each with the rows
# spicy gives (est = NA for a predictor without AME) and the reason.
# Found on 2026-11-19; the test fails once a row is mended, so that the
# entry is removed with the fix.
ag_known_rows <- list(
  feols_iv = list(
    rows = list("xen|NA" = c(est = 0, se = NA)),
    why = paste(
      "fixest's predict() of an IV fit does not move with the endogenous",
      "regressor: marginaleffects returns a slope of 0 without SE"
    )
  ),
  survreg_strata = list(
    rows = list(
      "strata(mn)B|NA" = c(est = 0, se = NA),
      "strata(mn)C|NA" = c(est = 0, se = NA)
    ),
    why = paste(
      "marginaleffects takes strata(mn) for a predictor and gives a",
      "contrast of 0 without SE, which spicy shows as an AME row"
    )
  ),
  clm_nominal = list(
    rows = list("z|lo" = c(est = NA, se = NA)),
    why = paste(
      "a nominal effect gets no AME: marginaleffects lists the location",
      "predictors only"
    )
  )
)

# AME values that depart from the oracle, each as list(key, spicy =
# c(est, se), oracle = c(est, se), why). The test fails once spicy meets
# the oracle again. Empty since the REML glmmTMB fit (SE 5.3e-08 against
# 0.0598) is refused, 2026-11-19.
ag_known_values <- list()

# ---- The hand oracles -------------------------------------------------------

# rms names the intercept "Intercept".
ag_rn <- function(x) {
  nm <- if (is.matrix(x)) colnames(x) else names(x)
  nm[nm == "Intercept"] <- "(Intercept)"
  if (is.matrix(x)) {
    dimnames(x) <- list(nm, nm)
  } else {
    names(x) <- nm
  }
  x
}

# A single-index fit through its inverse link, on a right-hand side.
ag_link <- function(rhs, linkinv, beta = NULL, V = NULL, w = NULL) {
  function(fit, d, vars) {
    ame_oracle(
      ame_prep_rhs(rhs, d),
      ame_pf_link(linkinv),
      ag_rn(beta %||% stats::coef(fit)),
      ag_rn(as.matrix(V %||% stats::vcov(fit))),
      d,
      vars,
      w
    )
  }
}

# The identity link with the coefficients of a fit (a linear model, the
# latent outcome of a tobit, the fixed part of a linear mixed model).
ag_identity <- function(rhs, beta = stats::coef, V = stats::vcov) {
  function(fit, d, vars) {
    ag_link(rhs, identity, beta(fit), V(fit))(fit, d, vars)
  }
}

# Category probabilities of a cumulative-link model, P(Y <= j) =
# F((theta_j - eta) / s), from a function of b and the design that gives
# the cut-points (n x J-1), eta and the scale s.
ag_pf_cum <- function(lev, parts, cdf = stats::plogis) {
  function(b, D) {
    p <- parts(b, D)
    cum <- cdf((p$theta - p$eta) / p$s)
    P <- cbind(cum, 1) - cbind(0, cum)
    colnames(P) <- lev
    P
  }
}

ag_prep_x <- function(nd) list(X = cbind(x = nd$x), z = nd$z, off = 0)

ag_cum_simple <- function(cuts) {
  function(b, D) {
    list(
      theta = matrix(b[cuts], nrow(D$X), length(cuts), byrow = TRUE),
      eta = drop(D$X %*% b["x"]),
      s = 1
    )
  }
}

ag_ord <- function(beta, weighted = FALSE) {
  function(fit, d, vars) {
    b <- beta(fit)
    w <- if (weighted) d$w
    cuts <- c("lo|mid", "mid|hi")
    ame_oracle(
      ag_prep_x,
      ag_pf_cum(levels(d$ord), ag_cum_simple(cuts)),
      b,
      stats::vcov(fit),
      d,
      vars,
      w
    )
  }
}

# The mean of a two-part count model: the count mean times the probability
# of a positive count (hurdle, truncated count) or of the count state
# (zero-inflation).
ag_two_part <- function(beta, V, count, zero, hurdle, theta = NULL, re = 0) {
  function(fit, d, vars) {
    prep <- function(nd) list(X = cbind("(Intercept)" = 1, x = nd$x), off = re)
    pf <- function(b, D) {
      mu <- exp(drop(D$X %*% b[count]) + D$off)
      p <- stats::plogis(b[[zero]])
      if (!hurdle) {
        return(mu * (1 - p))
      }
      p0 <- if (is.null(theta)) exp(-mu) else (theta / (theta + mu))^theta
      p * mu / (1 - p0)
    }
    ame_oracle(prep, pf, beta, V, d, vars)
  }
}

ag_glmmTMB_two_part <- function(hurdle) {
  function(fit, d, vars) {
    V <- stats::vcov(fit, full = TRUE)
    dimnames(V) <- list(unname(colnames(V)), unname(colnames(V)))
    fe <- glmmTMB::fixef(fit)
    beta <- c(fe$cond, "zi~(Intercept)" = unname(fe$zi))
    u <- glmmTMB::ranef(fit)$cond$g[as.character(d$g), 1L]
    # A zero-inflation or hurdle of `p` = plogis(zi), as pscl, so that one
    # function serves both; glmmTMB's zi is the probability of a zero.
    two <- ag_two_part(
      beta,
      V,
      c("(Intercept)", "x"),
      "zi~(Intercept)",
      hurdle,
      re = u
    )
    if (!hurdle) {
      return(two(fit, d, vars))
    }
    prep <- function(nd) list(X = cbind("(Intercept)" = 1, x = nd$x), off = u)
    pf <- function(b, D) {
      mu <- exp(drop(D$X %*% b[c("(Intercept)", "x")]) + D$off)
      (1 - stats::plogis(b[["zi~(Intercept)"]])) * mu / (1 - exp(-mu))
    }
    ame_oracle(prep, pf, beta, V, d, vars)
  }
}

ag_pscl <- function(hurdle) {
  function(fit, d, vars) {
    theta <- if (identical(fit$dist[[1L]], "negbin")) fit$theta[[1L]]
    # pscl's zero part is P(zero state) for zeroinfl and P(y > 0) for a
    # binomial hurdle.
    ag_two_part(
      stats::coef(fit),
      stats::vcov(fit),
      c("count_(Intercept)", "count_x"),
      "zero_(Intercept)",
      hurdle,
      theta = theta
    )(fit, d, vars)
  }
}

ag_gam <- function(fit, d, vars) {
  ame_oracle(
    function(nd) {
      list(X = stats::predict(fit, nd, type = "lpmatrix"), off = 0)
    },
    ame_pf_link(stats::family(fit)$linkinv),
    stats::coef(fit),
    stats::vcov(fit),
    d,
    vars
  )
}

ag_fixef_merMod <- function(fit) lme4::fixef(fit)
ag_vcov_merMod <- function(fit) as.matrix(stats::vcov(fit))
ag_fixef_tmb <- function(fit) glmmTMB::fixef(fit)$cond
ag_vcov_tmb <- function(fit) stats::vcov(fit)$cond

ag_oracles <- list(
  lm = ag_identity(~x),
  lm_weighted = ag_identity(~x),
  glm_gaussian = ag_identity(~x),
  glm_logit = function(fit, d, v) ag_link(~x, stats::plogis)(fit, d, v),
  glm_probit = function(fit, d, v) ag_link(~x, stats::pnorm)(fit, d, v),
  glm_poisson = function(fit, d, v) ag_link(~x, exp)(fit, d, v),
  glm_gamma = function(fit, d, v) ag_link(~x, exp)(fit, d, v),
  glm_quasi = function(fit, d, v) ag_link(~x, exp)(fit, d, v),
  glm.nb = function(fit, d, v) ag_link(~x, exp)(fit, d, v),
  rlm = ag_identity(~x),
  lm_robust = ag_identity(~x),
  lm_robust_fe = ag_identity(~ x - 1),
  iv_robust = ag_identity(~xen),
  ivreg = ag_identity(~xen),
  tobit = ag_identity(~x),
  rq = ag_identity(~x, V = function(fit) {
    V <- summary(fit, se = "nid", covariance = TRUE)$cov
    dimnames(V) <- list(names(stats::coef(fit)), names(stats::coef(fit)))
    V
  }),
  feols = ag_identity(~ x - 1),
  feols_nofe = ag_identity(~x),
  fepois_nofe = function(fit, d, v) ag_link(~x, exp)(fit, d, v),
  lmer = ag_identity(~x, ag_fixef_merMod, ag_vcov_merMod),
  lmer_ml = ag_identity(~x, ag_fixef_merMod, ag_vcov_merMod),
  # The conditional predictions: the estimated random intercepts enter as
  # an offset, held fixed.
  glmer = function(fit, d, vars) {
    u <- lme4::ranef(fit)$g[as.character(d$g), 1L]
    ame_oracle(
      function(nd) list(X = cbind("(Intercept)" = 1, x = nd$x), off = u),
      ame_pf_link(stats::plogis),
      lme4::fixef(fit),
      ag_vcov_merMod(fit),
      d,
      vars
    )
  },
  glmmTMB = ag_identity(~x, ag_fixef_tmb, ag_vcov_tmb),
  glmmTMB_zi = ag_glmmTMB_two_part(hurdle = FALSE),
  glmmTMB_hurdle = ag_glmmTMB_two_part(hurdle = TRUE),
  glmmTMB_disp = ag_identity(~x, ag_fixef_tmb, ag_vcov_tmb),
  lme = ag_identity(~x, nlme::fixef),
  lme_contrasts = function(fit, d, vars) {
    ame_oracle(
      function(nd) {
        list(
          X = stats::model.matrix(
            ~ x + mn,
            nd,
            contrasts.arg = list(mn = "contr.sum")
          ),
          off = 0
        )
      },
      ame_pf_link(identity),
      nlme::fixef(fit),
      stats::vcov(fit),
      d,
      c("x", "mn")
    )
  },
  gls = ag_identity(~x),
  polr = ag_ord(function(fit) c(fit$coefficients, fit$zeta)),
  clm = ag_ord(stats::coef),
  clm_scale = function(fit, d, vars) {
    parts <- function(b, D) {
      list(
        theta = matrix(b[c("lo|mid", "mid|hi")], nrow(D$X), 2L, byrow = TRUE),
        eta = D$X[, "x"] * b[["x"]],
        s = exp(D$z * b[["z"]])
      )
    }
    ame_oracle(
      ag_prep_x,
      ag_pf_cum(levels(d$ord), parts),
      stats::coef(fit),
      stats::vcov(fit),
      d,
      vars
    )
  },
  clm_nominal = function(fit, d, vars) {
    parts <- function(b, D) {
      cut <- function(j) {
        b[[paste0(j, ".(Intercept)")]] + b[[paste0(j, ".z")]] * D$z
      }
      list(
        theta = cbind(cut("lo|mid"), cut("mid|hi")),
        eta = D$X[, "x"] * b[["x"]],
        s = 1
      )
    }
    ame_oracle(
      ag_prep_x,
      ag_pf_cum(levels(d$ord), parts),
      stats::coef(fit),
      stats::vcov(fit),
      d,
      "x"
    )
  },
  multinom = function(fit, d, vars) {
    cf <- stats::coef(fit)
    beta <- stats::setNames(
      as.vector(t(cf)),
      paste0(rep(rownames(cf), each = ncol(cf)), ":", colnames(cf))
    )
    pf <- function(b, D) {
      E <- vapply(
        rownames(cf),
        function(k) exp(drop(D$X %*% b[paste0(k, ":", colnames(D$X))])),
        numeric(nrow(D$X))
      )
      P <- cbind(1, E) / (1 + rowSums(E))
      colnames(P) <- fit$lev
      P
    }
    ame_oracle(ame_prep_rhs(~x, d), pf, beta, stats::vcov(fit), d, vars)
  },
  zeroinfl = ag_pscl(hurdle = FALSE),
  zeroinfl_negbin = ag_pscl(hurdle = FALSE),
  hurdle = ag_pscl(hurdle = TRUE),
  hurdle_negbin = ag_pscl(hurdle = TRUE),
  # The response scale of a survreg is exp(lp) for its Weibull default.
  survreg = function(fit, d, v) ag_link(~x, exp)(fit, d, v),
  svyglm = function(fit, d, v) ag_link(~x, identity, w = d$w)(fit, d, v),
  svyglm_binomial = function(fit, d, v) {
    ag_link(~x, stats::plogis, w = d$w)(fit, d, v)
  },
  svyglm_quasibinomial = function(fit, d, v) {
    ag_link(~x, stats::plogis, w = d$w)(fit, d, v)
  },
  svyolr = ag_ord(function(fit) c(fit$coefficients, fit$zeta), weighted = TRUE),
  geeglm = function(fit, d, v) ag_link(~x, exp)(fit, d, v),
  gam = ag_gam,
  gam_logit = ag_gam,
  bam = ag_gam,
  betareg = function(fit, d, v) ag_link(~x, stats::plogis)(fit, d, v),
  ols = ag_identity(~x),
  ols_xy = ag_identity(~x),
  lrm = function(fit, d, v) ag_link(~x, stats::plogis)(fit, d, v),
  lrm_xy = function(fit, d, v) ag_link(~x, stats::plogis)(fit, d, v),
  lrm_ordinal = function(fit, d, vars) {
    parts <- function(b, D) {
      # P(Y >= j) = plogis(alpha_j + eta), so P(Y <= j - 1) = plogis(-alpha_j
      # - eta): cut-points -alpha with the sign of eta reversed.
      list(
        theta = matrix(-b[c("y>=mid", "y>=hi")], nrow(D$X), 2L, byrow = TRUE),
        eta = D$X[, "x"] * b[["x"]],
        s = 1
      )
    }
    ame_oracle(
      ag_prep_x,
      ag_pf_cum(levels(d$ord), parts),
      stats::coef(fit),
      stats::vcov(fit),
      d,
      vars
    )
  },
  rms_Glm = function(fit, d, v) ag_link(~x, exp)(fit, d, v),
  rms_Glm_xy = function(fit, d, v) ag_link(~x, exp)(fit, d, v),
  rms_Glm_logit = function(fit, d, v) ag_link(~x, stats::plogis)(fit, d, v)
)

# The fixtures of a "yes" or "per" cell checked for presence only.
ag_no_oracle <- list(
  feols_iv = "its AME is a known defect (ag_known_rows)",
  survreg_strata = "the strata rows are a known defect; x is checked on survreg",
  stanreg = "posterior draws, local only",
  brmsfit = "posterior draws, local only"
)

# ---- Running spicy ----------------------------------------------------------

# spicy's AME rows keyed as ame_spicy() keys them, every row kept (a blank
# cell has est NA), and the classes of the warnings raised. The Jacobian is
# Richardson's, as in ame_spicy(), except for glmmTMB: each of its
# predictions is slow, and the forward difference of marginaleffects stays
# within 1e-5 of the oracle SE on these fixtures (measured 2026-11-19).
ag_run <- function(fit) {
  op <- options(
    marginaleffects_numDeriv = if (!inherits(fit, "glmmTMB")) {
      list(method = "Richardson")
    }
  )
  on.exit(options(op), add = TRUE)
  warns <- character()
  lo <- withCallingHandlers(
    as.data.frame(table_regression(
      fit,
      show_columns = c("b", "ame", "ame_se"),
      output = "long"
    )),
    warning = function(w) {
      warns <<- c(warns, class(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  a <- lo[lo$estimate_type == "ame" & !(lo$is_reference %in% TRUE), ]
  grp <- if ("outcome_level" %in% names(a)) {
    as.character(a$outcome_level)
  } else {
    rep(NA_character_, nrow(a))
  }
  term <- a$term
  pre <- !is.na(grp) & startsWith(term, paste0(grp, ": "))
  term[pre] <- substring(term[pre], nchar(grp[pre]) + 3L)
  list(
    rows = data.frame(
      key = paste(term, grp, sep = "|"),
      est = a$estimate,
      se = a$std.error
    ),
    warnings = unique(warns)
  )
}

ag_expect <- function(id) {
  reg <- .supported_models_registry()
  cell <- reg$ame[reg$class == .CLASS_FIXTURES[[id]]$class]
  cond <- ag_conditional[[id]]$expect
  if (!is.null(cond)) {
    return(cond)
  }
  if (startsWith(cell, "per")) {
    "per"
  } else if (startsWith(cell, "yes")) {
    "yes"
  } else {
    "refused"
  }
}

# The keys spicy should give: each predictor, per category or outcome when
# the cell says so.
ag_keys <- function(id, fit) {
  terms <- ag_terms[[id]] %||% "x"
  grp <- if (ag_expect(id) == "per") {
    if (inherits(fit, "multinom")) fit$lev else c("lo", "mid", "hi")
  } else {
    NA
  }
  as.vector(outer(terms, grp, paste, sep = "|"))
}

# ---- The bookkeeping holds --------------------------------------------------

test_that("every fixture of an AME cell is checked against the oracle or named", {
  reg <- .supported_models_registry()
  ids <- names(.CLASS_FIXTURES)
  fx_class <- vapply(.CLASS_FIXTURES, `[[`, character(1), "class")
  expect_setequal(unique(fx_class), reg$class)
  offered <- ids[vapply(ids, ag_expect, character(1)) != "refused"]
  refused <- setdiff(ids, offered)
  expect_setequal(names(ag_refusal), refused)
  expect_true(all(ag_refusal %in% c("error", "empty")))
  # Each fixture offering the AME has an oracle or a reason, never both.
  expect_setequal(c(names(ag_oracles), names(ag_no_oracle)), offered)
  expect_length(intersect(names(ag_oracles), names(ag_no_oracle)), 0L)
  # The list of fixtures without a value oracle may not grow.
  expect_lte(length(ag_no_oracle), 4L)
  # A conditional expectation names a fixture whose cell has a condition.
  for (id in names(ag_conditional)) {
    cell <- reg$ame[reg$class == fx_class[[id]]]
    expect_true(grepl("(", cell, fixed = TRUE), label = id)
    expect_true(
      grepl(ag_conditional[[id]]$cell, cell, fixed = TRUE),
      label = id
    )
    expect_true(nzchar(ag_conditional[[id]]$why))
  }
  for (id in c(names(ag_known_rows), names(ag_known_values))) {
    expect_true(id %in% offered, label = id)
  }
})

# ---- One test per fixture ---------------------------------------------------

for (id in names(.CLASS_FIXTURES)) {
  local({
    id <- id
    test_that(paste0("the registry gives the AME of ", id), {
      skip_if_no_ame_guard()
      fit <- .class_fixture(id)
      expect <- ag_expect(id)

      if (expect == "refused") {
        out <- tryCatch(
          {
            r <- ag_run(fit)
            if (all(is.na(r$rows$est)) && "spicy_warning" %in% r$warnings) {
              "empty"
            } else {
              "rendered"
            }
          },
          spicy_invalid_input = function(e) "error"
        )
        expect_identical(out, ag_refusal[[id]], label = id)
        return(invisible())
      }

      r <- ag_run(fit)
      expect_false("spicy_fallback" %in% r$warnings, label = id)
      sp <- r$rows
      keys <- ag_keys(id, fit)
      known <- ag_known_rows[[id]]$rows
      # Every expected predictor has a numeric AME and SE...
      for (k in setdiff(keys, names(known))) {
        i <- match(k, sp$key)
        expect_false(is.na(i), label = paste(id, k, "row"))
        expect_true(is.finite(sp$est[i]), label = paste(id, k, "estimate"))
        expect_true(
          is.finite(sp$se[i]) && sp$se[i] > 0,
          label = paste(id, k, "SE")
        )
      }
      # ... and no other row carries a number.
      extra <- setdiff(sp$key[!is.na(sp$est)], keys)
      expect_setequal(
        extra,
        as.character(intersect(
          setdiff(names(known), keys),
          sp$key[!is.na(sp$est)]
        ))
      )
      # A known defect still holds: once mended, remove its entry.
      for (k in names(known)) {
        i <- match(k, sp$key)
        got <- if (is.na(i)) c(NA, NA) else c(sp$est[i], sp$se[i])
        expect_equal(
          unname(got),
          unname(known[[k]]),
          label = paste(id, k, "known row (mended? remove the entry)")
        )
      }

      if (is.null(ag_oracles[[id]])) {
        return(invisible())
      }
      d <- .class_fixture_data()
      vars <- unique(sub("^(mn).*$", "\\1", ag_terms[[id]] %||% "x"))
      or <- ag_oracles[[id]](fit, d, vars)
      kv <- ag_known_values[[id]]
      i <- match(or$key, sp$key)
      expect_false(anyNA(i), label = paste(id, "oracle keys"))
      if (is.null(kv)) {
        expect_equal(sp$est[i], or$est, tolerance = 1e-6, label = id)
        expect_equal(sp$se[i], or$se, tolerance = 1e-4, label = id)
      } else {
        j <- match(kv$key, or$key)
        expect_equal(or$est[j], kv$oracle[["est"]], tolerance = 1e-4)
        expect_equal(or$se[j], kv$oracle[["se"]], tolerance = 1e-4)
        expect_equal(sp$est[i][j], kv$spicy[["est"]], tolerance = 1e-4)
        expect_gt(
          abs(sp$se[i][j] / or$se[j] - 1),
          0.01,
          label = paste(id, "known value (mended? remove the entry)")
        )
      }
    })
  })
}

# ---- The refusals of a class that otherwise has the AME ---------------------

test_that("a glmmTMB fit estimated by REML refuses the AME with its reason", {
  skip_if_no_ame_guard()
  fit <- .class_fixture("glmmTMB_reml")
  e <- expect_error(
    table_regression(fit, show_columns = c("b", "ame")),
    class = "spicy_invalid_input"
  )
  m <- conditionMessage(e)
  expect_match(m, "fit estimated by REML", fixed = TRUE)
  expect_match(m, "standard error comes out near zero", fixed = TRUE)
  expect_match(m, "Refit with `REML = FALSE`", fixed = TRUE)
  # Two such fits: the reason names them.
  e2 <- expect_error(
    table_regression(list(fit, fit), show_columns = c("b", "ame")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(e2),
    "Model 1, Model 2: Under REML",
    fixed = TRUE
  )
  # Beside a fit with the AME, its cells stay empty.
  lo <- as.data.frame(suppressWarnings(table_regression(
    list(.class_fixture("glmmTMB"), fit),
    show_columns = c("b", "ame"),
    output = "long"
  )))
  a <- lo[lo$estimate_type == "ame" & is.finite(lo$estimate), ]
  expect_identical(unique(a$model_id), "M1")
})
