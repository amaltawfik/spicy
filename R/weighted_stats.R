# Weighted descriptive statistics -- decision 17 (2026-08-15).
#
# CONVENTION: frequency-expansion. For integer weights every statistic
# below is EXACTLY the unweighted statistic of the expanded data
# rep(x, w); with all weights 1 every statistic collapses to its
# unweighted sibling byte for byte. Triangulated 2026-08-15 against
# Hmisc::wtd.* (defaults), matrixStats::weightedVar/Sd,
# DescTools::Quantile and PSPP 2.0 (dev/weights_continuous_spec.md):
#   mean = sum(w x) / sum(w)                (every tool agrees)
#   var  = sum(w (x - mean)^2) / (sum(w) - 1)
#   quantiles: type-7 positions on the cumulative-weight scale
#     (the Hmisc::wtd.quantile(type = "quantile") algorithm).
# With `rescale = TRUE` the weights are normalised to sum to n first,
# which makes the SD algebraically equal to Stata's [aweight] /
# survey::svyvar() value -- the sampling-weights reading, through the
# same `rescale` grammar the categorical family already has.
#
# Rows with NA or ZERO weight are excluded up front by the caller (a
# zero weight means zero copies; min/max must not see such a row).

# Per-variable weight vector for table_continuous(): NULL when the
# table is unweighted; otherwise the raw weights, optionally rescaled
# (`rescale = TRUE`) so the weights of the variable's SURVIVING rows
# (x observed, weight observed and positive) sum to their count --
# the normalisation freq() and cross_tab() apply, over the whole
# variable and never per group (a per-group rescale would destroy the
# relative weights across groups).
.prep_variable_weights <- function(x, w, rescale) {
  if (is.null(w)) {
    return(NULL)
  }
  if (isTRUE(rescale)) {
    ok <- !is.na(x) & !is.na(w) & w > 0
    w[ok] <- w[ok] * sum(ok) / sum(w[ok])
  }
  w
}

# Mean. Assumes w already cleaned (no NA, no zero).
.wtd_mean <- function(x, w) {
  sum(w * x) / sum(w)
}

# SD, frequency denominator sum(w) - 1. NA when sum(w) <= 1 (the
# expanded sample has fewer than two observations).
.wtd_sd <- function(x, w) {
  big_w <- sum(w)
  if (big_w <= 1) {
    return(NA_real_)
  }
  sqrt(sum(w * (x - .wtd_mean(x, w))^2) / (big_w - 1))
}

# Quantiles, type-7 positions on the cumulative-weight scale -- the
# Hmisc::wtd.quantile(type = "quantile") algorithm, transcribed so the
# pinned oracles are reproduced digit for digit. Ties are collapsed by
# summing their weights; the position 1 + (W - 1) p is looked up in
# the cumulative weights as a right-continuous step (approx method
# "constant", f = 1) and blended linearly.
.wtd_quantile7 <- function(x, w, probs) {
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  if (anyDuplicated(x)) {
    grp <- match(x, unique(x))
    w <- as.numeric(vapply(split(w, grp), sum, numeric(1)))
    x <- unique(x)
  }
  big_w <- sum(w)
  pos <- 1 + (big_w - 1) * probs
  low <- pmax(floor(pos), 1)
  high <- pmin(low + 1, big_w)
  frac <- pos %% 1
  vals <- stats::approx(
    cumsum(w),
    x,
    xout = c(low, high),
    method = "constant",
    f = 1,
    rule = 2
  )$y
  k <- length(probs)
  (1 - frac) * vals[seq_len(k)] + frac * vals[-seq_len(k)]
}
