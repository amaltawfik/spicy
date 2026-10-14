# Cluster-robust variance of glmmTMB fits, through glmmTMB's own
# sandwich (glmmTMB::sandwich(), glmmTMB >= 1.1.15).
#
# A mixed model has no per-observation score: the observations of a
# cluster share its random effects, and the marginal log-likelihood is a
# sum over clusters only. glmmTMB's estfun() returns ONE score row per
# cluster -- the gradient of the cluster's contribution to the Laplace
# marginal log-likelihood, obtained by zeroing the weights of the other
# clusters and retaping the TMB object -- and sandwich() is
# bread %*% meat %*% bread, the bread the inverse Hessian of ALL the
# fixed parameters (vcov(full = TRUE)), the meat the cross-product of
# the scores, with no finite-sample factor: CR0, which equals
# sandwich::vcovCL(type = "HC0", cadjust = FALSE) on a fit without
# random effects (pinned in test-vcov_glmmTMB.R). Per-cluster scores
# leave nothing for the CR1-CR3 corrections to work on (no residuals,
# no hat values), so CR0 is the one type, and HC* -- one score per
# observation -- does not exist for this class.
#
# What spicy checks rather than trusts:
#   * every random-effect group sits within one cluster, so the cluster
#     contributions are independent and add up to the likelihood. The
#     default cluster is the coarsest grouping factor that contains all
#     the others. glmmTMB's own default, getGroups(), is the FIRST
#     factor of the flist, the finest level of a nested design, which
#     cuts through the coarser groups (glmmTMB then only warns). Crossed
#     random effects have no such factor. glmmTMB's warning that the
#     cluster log-likelihoods do not add up is turned into an error: the
#     matrix that comes with it is wrong;
#   * the fit is ML (glmmTMB has no sandwich for REML) and not of the
#     ordinal family (its sandwich does not cover the cut-points);
#   * glmmTMB >= 1.1.15. Up to 1.1.14, estfun() left the TMB object at
#     its last per-cluster state, and predict() shifted afterwards.
#     1.1.15 restores the parameters it saved, but saves them after a
#     call to obj$fn() and leaves last.par1 / value.best out: on a fresh
#     fit, marginaleffects' predictions still moved by 1e-3 (measured
#     2026-10-14, glmmTMB 1.1.15.2). spicy saves and restores that state
#     itself, so the user's fit, and the AME computed after the
#     sandwich, are untouched.

.GLMMTMB_SANDWICH_MIN_VERSION <- "1.1.15"

# The vcov tokens a glmmTMB fit supports: CR0, except where glmmTMB's
# sandwich does not apply.
.glmmTMB_robust_vcov_support <- function(fit) {
  if (is_reml_glmmTMB_fit(fit) || .glmmTMB_is_ordinal(fit)) {
    return("classical")
  }
  c("classical", "CR0")
}

# The refusal, with its reason, for a token a glmmTMB fit does not
# support. Shared by the public gate and the compute path.
.glmmTMB_refuse_vcov <- function(fit, type) {
  why <- if (is_reml_glmmTMB_fit(fit)) {
    c(
      "i" = "glmmTMB's sandwich is defined for maximum-likelihood fits only.",
      "i" = "Refit with `REML = FALSE` to use `vcov = \"CR0\"`."
    )
  } else if (.glmmTMB_is_ordinal(fit)) {
    c(
      "i" = paste0(
        "glmmTMB's sandwich does not cover the cut-points of the ",
        "ordinal family."
      )
    )
  } else if (startsWith(type, "CR")) {
    c(
      "i" = paste0(
        "glmmTMB's sandwich has one score per cluster, the gradient of the ",
        "cluster's marginal log-likelihood: with no residuals and no hat ",
        "values, the CR1-CR3 corrections cannot be formed."
      ),
      "i" = "Use `vcov = \"CR0\"`."
    )
  } else if (startsWith(type, "HC")) {
    c(
      "i" = paste0(
        "Observations that share random effects have no scores of their ",
        "own, so there is no per-observation (HC) sandwich."
      ),
      "i" = paste0(
        "Use `vcov = \"CR0\"`, clustered by default on the top-level ",
        "grouping factor."
      )
    )
  } else {
    c("i" = "This class supports `vcov = \"classical\"` and `\"CR0\"`.")
  }
  spicy_abort(
    c(
      sprintf("`vcov = \"%s\"` is not available for this `glmmTMB` fit.", type),
      why
    ),
    class = "spicy_unsupported_vcov"
  )
}

# Every random-effect grouping factor of the fit (conditional,
# zero-inflation and dispersion components), once per name.
.glmmTMB_grouping_factors <- function(fit) {
  fl <- do.call(
    c,
    lapply(unname(fit$modelInfo$reTrms), function(rt) as.list(rt$flist))
  )
  fl[!duplicated(names(fl))]
}

# TRUE when every level of `inner` sits within one level of `outer`.
.glmmTMB_nested_in <- function(inner, outer) {
  pairs <- unique(cbind(as.integer(inner), as.integer(outer)))
  !anyDuplicated(pairs[, 1L])
}

# The cluster of the sandwich, as a factor, with the name the footer
# gives it: the user's cluster when there is one (its name is the
# caller's), else the coarsest grouping factor that contains all the
# others. Refuses a cluster that cuts through a random-effect group,
# and crossed random effects without a cluster.
.glmmTMB_cluster <- function(fit, cluster = NULL) {
  groups <- .glmmTMB_grouping_factors(fit)
  if (!is.null(cluster)) {
    .check_cluster_length(fit, cluster)
    .check_cluster_no_na(fit, cluster)
    cl <- factor(cluster)
    cut <- !vapply(groups, .glmmTMB_nested_in, logical(1), outer = cl)
    if (any(cut)) {
      spicy_abort(
        c(
          "`cluster` does not contain the random effects of this `glmmTMB` fit.",
          "x" = sprintf(
            paste0(
              "Levels of %s fall in more than one cluster: the cluster ",
              "contributions to the likelihood are not independent."
            ),
            paste0("`", names(groups)[cut], "`", collapse = ", ")
          ),
          "i" = paste0(
            "Cluster by a variable within which every random-effect group ",
            "sits, or leave `cluster` unset to use the top-level grouping ",
            "factor."
          )
        ),
        class = "spicy_unsupported_vcov"
      )
    }
    return(list(cluster = cl, name = NA_character_))
  }
  if (length(groups) == 0L) {
    spicy_abort(
      c(
        "`vcov = \"CR0\"` requires `cluster` to be specified.",
        "i" = paste0(
          "This `glmmTMB` fit has no random effects, so there is no ",
          "grouping factor to cluster by default."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  contains_all <- vapply(
    groups,
    function(f) all(vapply(groups, .glmmTMB_nested_in, logical(1), outer = f)),
    logical(1)
  )
  if (!any(contains_all)) {
    spicy_abort(
      c(
        "`vcov = \"CR0\"` has no default cluster for this `glmmTMB` fit.",
        "x" = sprintf(
          paste0(
            "The random effects are crossed (%s): no grouping factor ",
            "contains the others, so the likelihood does not split into ",
            "independent clusters."
          ),
          paste0("`", names(groups), "`", collapse = ", ")
        ),
        "i" = paste0(
          "Pass a `cluster` within which every random-effect group sits, ",
          "if the design has one."
        )
      ),
      class = "spicy_unsupported_vcov"
    )
  }
  top <- groups[contains_all]
  pick <- which.min(vapply(top, nlevels, integer(1)))
  list(cluster = factor(top[[pick]]), name = names(top)[pick])
}

# The CR0 sandwich of every fixed parameter of a glmmTMB fit
# (glmmTMB::sandwich(full = TRUE)): the conditional coefficients under
# their own names, the zero-inflation and dispersion ones as "zi~<term>"
# and "disp~<term>", then the random-effect parameters. One computation
# serves the coefficient rows, the component blocks, the AME and the
# nested Wald test (the frame stores it in info$vcov_matrix).
.glmmTMB_cluster_robust_vcov <- function(fit, type, cluster = NULL) {
  if (
    !identical(type, "CR0") || !"CR0" %in% .glmmTMB_robust_vcov_support(fit)
  ) {
    .glmmTMB_refuse_vcov(fit, type)
  }
  found <- utils::packageVersion("glmmTMB")
  if (found < .GLMMTMB_SANDWICH_MIN_VERSION) {
    spicy_abort(
      c(
        sprintf(
          "`vcov = \"CR0\"` for a `glmmTMB` fit requires glmmTMB >= %s; %s is installed.",
          .GLMMTMB_SANDWICH_MIN_VERSION,
          format(found)
        ),
        "i" = paste0(
          "Earlier versions leave the fitted model altered by the ",
          "sandwich computation: its later predictions shift."
        ),
        "i" = "Update with `install.packages(\"glmmTMB\")`."
      ),
      class = "spicy_unsupported_vcov"
    )
  }
  cl <- .glmmTMB_cluster(fit, cluster)$cluster
  env <- fit$obj$env
  saved <- mget(
    intersect(
      c(
        "par",
        "parameters",
        "last.par",
        "last.par.best",
        "last.par.ok",
        "last.par1",
        "last.par2",
        "value.best"
      ),
      ls(env)
    ),
    envir = env
  )
  on.exit(list2env(saved, envir = env), add = TRUE)
  not_nested <- FALSE
  V <- tryCatch(
    withCallingHandlers(
      glmmTMB::sandwich(fit, full = TRUE, cluster = cl),
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("log-likelihoods of the clusters", msg, fixed = TRUE)) {
          not_nested <<- TRUE
          invokeRestart("muffleWarning")
        }
      }
    ),
    error = function(e) .abort_vcov_failed(type, e, "glmmTMB::sandwich()")
  )
  if (not_nested) {
    spicy_abort(
      c(
        "`vcov = \"CR0\"` could not be computed for this `glmmTMB` fit.",
        "x" = paste0(
          "The cluster log-likelihoods do not add up to the likelihood of ",
          "the model: the random effects are not nested in the clusters."
        ),
        "i" = "Cluster by a variable within which every random-effect group sits."
      ),
      class = "spicy_unsupported_vcov"
    )
  }
  V
}

# The AME table of a glmmTMB fit under the robust matrix `vc`.
# marginaleffects takes only TRUE / FALSE / "HC0" as the `vcov` of a
# glmmTMB fit (a matrix is refused), so the matrix goes through the
# Jacobian avg_slopes() returns: se = sqrt(diag(J V J')), the delta
# method avg_slopes() applies to a matrix it accepts. The Jacobian's
# columns are "cond~<term>", "zi~<term>", "disp~<term>"; the sandwich
# names the conditional ones without the prefix.
.glmmTMB_ame_robust <- function(ame_table, vc, ci_level) {
  J <- marginaleffects::components(ame_table, "jacobian")
  key <- sub("^cond~", "", colnames(J))
  se <- sqrt(rowSums((J %*% vc[key, key, drop = FALSE]) * J))
  crit <- stats::qnorm(1 - (1 - ci_level) / 2)
  ame_table$std.error <- se
  ame_table$statistic <- ame_table$estimate / se
  ame_table$p.value <- 2 * stats::pnorm(-abs(ame_table$statistic))
  ame_table$conf.low <- ame_table$estimate - crit * se
  ame_table$conf.high <- ame_table$estimate + crit * se
  ame_table
}
