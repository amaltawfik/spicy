# Standardized mean differences -- the Table 1 balance diagnostic.
#
# CONVENTION: Austin (2009, Stat Med 28:3083-3107; 2011, Multivar Behav
# Res 46:399-424) for a continuous variable, Yang & Dalton (2012, SAS
# Global Forum 335-2012) for a categorical one:
#
#   continuous   SMD = (m1 - m2) / sqrt((v1 + v2) / 2)
#   binary       SMD = (p1 - p2) / sqrt((p1(1-p1) + p2(1-p2)) / 2)
#   k levels     SMD = sqrt(T' S^- T), T = (P1 - P2)[-1],
#                      S = (V(P1) + V(P2))[-1, -1] / 2,
#                      V(P)_ii = p_i(1 - p_i), V(P)_ij = -p_i p_j
#
# The continuous denominator is the root MEAN of the two GROUP variances
# (each at n - 1), NOT the degrees-of-freedom pooled SD. Measured, at
# equal group sizes the two denominators are the same and the SMD IS
# Cohen's d, `identical()`; at unequal sizes they part (-0.510 vs -0.538
# on the pinned 4-versus-3 fixture). `effect_size = "hedges_g"` is a
# third number again: it applies the J correction on top of d, so it
# NEVER equals the SMD -- the ratio g / SMD is exactly J at equal n
# (0.800 at n = 3 per group, 0.958 at n = 10, verified to 1e-12), and
# converges to 1 only asymptotically. The binary denominator is the
# Bernoulli variance p(1-p), at n, never `var()` at n - 1 (19% apart on
# the pinned example). Triangulated 2026-08-19 against tableone 0.13.2
# (`ExtractSmd`) and cobalt 4.6.3 (`col_w_smd`), which agree with every
# value below to the seventeenth digit; the weighted arm is the
# frequency convention of decision 17 and reproduces the SMD of the
# expanded data exactly, on all three kernels.
#
# ESTIMATOR-PARAMETERIZED ON PURPOSE (decision 28). The kernels take
# ESTIMATES, never data: means, variances, proportion vectors. The base
# families feed them `mean()` / `stats::sd()` / `.wtd_mean()` /
# `.wtd_sd()` / `prop.table()` through the adapters at the bottom of
# this file; the future `table_*_svy()` twins will feed them
# `svymean()` / `svyvar()` without touching a line of the algebra.
#
# NO INTERVAL, EVER (decision 29-A). The SMD is a descriptive balance
# diagnostic; attaching a p or a CI to it reintroduces the test
# reasoning the balance literature asks the reader to drop.

# ---- undefined-value protocol ----------------------------------------------

# A kernel that CANNOT estimate returns NA carrying the REASON, so the
# caller can word its own disclosure without re-deriving the geometry.
# A kernel that has nothing to estimate (a group of one observation, a
# group whose weights sum to <= 1) returns a bare NA and says nothing:
# the neighbouring SD cell already discloses it.
.smd_undefined <- function(reason) {
  structure(NA_real_, spicy_smd_undefined = reason)
}

.smd_undefined_reason <- function(value) {
  attr(value, "spicy_smd_undefined", exact = TRUE)
}


# ---- kernels ----------------------------------------------------------------

# Continuous, SIGNED (group 1 - group 2). `v1` / `v2` are the GROUP
# variances at n - 1, or their weighted twins -- contractually the
# square of the very SD the table displays, so no rewritten variance
# formula can reintroduce the negative-variance NaN that
# `sum(w) - 1 < 0` would produce (`.wtd_sd()` returns NA there).
.smd_continuous <- function(m1, m2, v1, v2) {
  if (is.na(m1) || is.na(m2) || is.na(v1) || is.na(v2)) {
    return(NA_real_)
  }
  den <- (v1 + v2) / 2
  if (den == 0) {
    # Both groups constant. Same value on both sides is perfect
    # balance; different values are an infinite standardized distance.
    return(if (m1 == m2) 0 else .smd_undefined("zero_denominator"))
  }
  (m1 - m2) / sqrt(den)
}

# Binary, SIGNED. `p1` / `p2` are the proportions of the SAME level in
# the two groups -- the second level by convention (see
# `.smd_categorical_type()`).
.smd_binary <- function(p1, p2) {
  if (is.na(p1) || is.na(p2)) {
    return(NA_real_)
  }
  den <- (p1 * (1 - p1) + p2 * (1 - p2)) / 2
  if (den == 0) {
    return(if (p1 == p2) 0 else .smd_undefined("zero_denominator"))
  }
  (p1 - p2) / sqrt(den)
}

# The multinomial covariance of a profile of proportions.
.smd_multinomial_cov <- function(P) {
  S <- -outer(P, P)
  diag(S) <- P * (1 - P)
  S
}

# k levels, UNSIGNED: this is a Mahalanobis distance between two
# profiles, and a distance has no direction.
#
# `MASS::ginv()` is REQUIRED, not a convenience: a declared-but-unobserved
# level makes S singular (det = 0) and `solve()` aborts, while the
# pseudo-inverse returns exactly the value the phantom level's absence
# would give. That only holds because T lies in the image of S -- which
# is NOT a general property of `ginv()`, so it is TESTED before the
# value is published:
#
#   * Two groups constant on DIFFERENT levels: S = 0, `ginv(0) = 0`, and
#     the bare route publishes 0 -- "perfectly balanced" for the most
#     imbalanced variable there can be. tableone guards this one
#     (`all(S == 0)` in `StdDiffFromLstMeans`) and returns NaN.
#   * Two profiles on DISJOINT supports: S is singular but not zero, so
#     `all(S == 0)` never fires, and both the bare route AND tableone
#     publish a finite number (sqrt(2) on the pinned k = 4 case) where
#     the true distance is infinite -- ridge regularisation
#     `solve(S + eI)` diverges as e^(-1/2), verified.
#
# One consistency test, `T in image(S)`, covers both and fires on
# nothing else. It can only fail when the two supports are disjoint: if
# they share a level, every null direction of S is constant on the union
# of the supports, and T sums to zero there.
.smd_multinomial <- function(P1, P2) {
  if (anyNA(P1) || anyNA(P2)) {
    return(NA_real_)
  }
  # Fewer than two categories leaves NOTHING once the reference is
  # dropped, and `MASS::ginv()` aborts on a zero-dimension matrix
  # ("a dimension is zero"). A one-category variable puts both groups
  # at 100% of it -- perfect balance, 0. No categories at all has
  # nothing to estimate. Reachable: `table_categorical()` tabulates a
  # single-level factor as a real one-row block.
  if (length(P1) < 2L) {
    return(if (length(P1) == 1L) 0 else NA_real_)
  }
  .check_MASS_for_smd()
  keep <- -1L
  tvec <- (P1 - P2)[keep]
  smat <- (.smd_multinomial_cov(P1)[keep, keep, drop = FALSE] +
    .smd_multinomial_cov(P2)[keep, keep, drop = FALSE]) /
    2
  sinv <- MASS::ginv(smat)
  if (!isTRUE(all.equal(drop(smat %*% sinv %*% tvec), drop(tvec)))) {
    return(.smd_undefined(
      if (all(smat == 0)) "constant_levels" else "disjoint_support"
    ))
  }
  sqrt(drop(t(tvec) %*% sinv %*% tvec))
}

# MASS is a Suggests dependency (a Recommended package, present in every
# standard R installation). Only the k >= 3 arm reaches it: continuous
# and binary variables never do.
.check_MASS_for_smd <- function() {
  if (!spicy_pkg_available("MASS")) {
    # nocov start: MASS ships with R; unreachable in a check environment.
    spicy_abort(
      c(
        "The standardized mean difference of a categorical variable with three or more levels needs `MASS`.",
        "i" = "Install MASS: `install.packages(\"MASS\")`.",
        "i" = "Variables with two levels and continuous variables do not need it."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


# ---- dispatch ---------------------------------------------------------------

# The dispatch decides the SIGN, so it is frozen here rather than left
# to a call site. Both kernels apply to a two-level factor and agree to
# the BIT in magnitude, but `.smd_binary()` is signed and
# `.smd_multinomial()` is not: routing a binary variable through the
# multivariate kernel would make the sign disappear in silence.
.smd_categorical_type <- function(n_levels) {
  if (n_levels == 2L) "binary" else "multinomial"
}

# `est1` / `est2` are the two groups' estimates, in the shape the type
# asks for: `c(mean, variance)` for "continuous", the proportion of the
# second level for "binary", the whole profile of proportions for
# "multinomial".
.smd_pair_dispatch <- function(est1, est2, type) {
  switch(
    type,
    continuous = .smd_continuous(
      est1[[1L]],
      est2[[1L]],
      est1[[2L]],
      est2[[2L]]
    ),
    binary = .smd_binary(est1, est2),
    multinomial = .smd_multinomial(est1, est2),
    # nocov start: `type` comes from `.smd_categorical_type()` or a
    # literal at two call sites; a fourth value would be a coding error.
    spicy_abort(
      sprintf("Unknown SMD kernel type \"%s\".", type),
      class = "spicy_invalid_input"
    )
    # nocov end
  )
}


# ---- base-family adapters (iid sample, plus the decision-17 weights) --------
#
# These see vectors; the kernels above never do. The `_svy` twins will
# add their own adapters here and reuse the same kernels.

# Mean and variance of one group, from the SAME producer the M and SD
# columns read (`compute_one()`): `mean()` / `stats::sd()` unweighted,
# `.wtd_mean()` / `.wtd_sd()` under weights. A group with fewer than two
# observations (or weights summing to <= 1) yields an NA variance, hence
# a bare-NA SMD.
.smd_moments_base <- function(x, w = NULL) {
  if (is.null(w)) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) {
      return(c(NA_real_, NA_real_))
    }
    s <- if (length(x) > 1L) stats::sd(x) else NA_real_
    return(c(mean(x), s^2))
  }
  keep <- !is.na(x) & !is.na(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  if (length(x) == 0L) {
    return(c(NA_real_, NA_real_))
  }
  c(.wtd_mean(x, w), .wtd_sd(x, w)^2)
}

# Profile of proportions over `levels`, in level order. Unweighted this
# is `prop.table(table(x))`; under weights it is the same table with the
# weights summed instead of the rows counted -- which is what
# `prop.table(xtabs(w ~ x))` computes, spelled without a formula so a
# level named like a variable cannot be resolved as one.
.smd_props_base <- function(x, levels, w = NULL) {
  f <- factor(as.character(x), levels = levels)
  counts <- if (is.null(w)) {
    as.numeric(table(f))
  } else {
    as.numeric(vapply(
      split(w, f),
      function(wi) sum(wi),
      numeric(1)
    ))
  }
  total <- sum(counts)
  if (!is.finite(total) || total == 0) {
    return(rep(NA_real_, length(levels)))
  }
  counts / total
}
