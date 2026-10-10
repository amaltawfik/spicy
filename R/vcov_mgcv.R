# Robust and cluster-robust variance of mgcv fits (gam, bam), formed by
# spicy from the score and mgcv's penalized covariance.
#
# sandwich's own route for these classes is wrong in three ways
# (sandwich 3.1-3, mgcv 1.9-4; measurements in the upstream drafts):
#   * bread.gam() leaves out the dispersion that estfun.glm() divides
#     by, so every sandwich of a fit with a free dispersion (gaussian,
#     Gamma, inverse.gaussian, quasi) is deflated by phi^2;
#   * bam() stores signed square-root deviance residuals in $residuals,
#     which residuals(bam, "working") returns, so the score of a
#     non-Gaussian bam is wrong;
#   * residuals(gam, "working") equals the GLM working residual
#     (y - mu) / mu.eta(eta) for canonical links only.
# A gam with a canonical link and a fixed dispersion was the one right
# case. The fit is therefore wrapped in a class whose estfun(), bread()
# and hatvalues() spicy computes from y, the fitted mean and the linear
# predictor; sandwich::vcovHC() / vcovCL() keep doing the HC and CL
# arithmetic on those ingredients. The working residuals and weights
# mgcv stores ($residuals, $weights) are never read, and neither is
# bam's $hat, which is not a per-observation hat diagonal.

.MGCV_SANDWICH_CLASS <- "spicy_mgcv_sandwich"

.mgcv_sandwich_wrap <- function(fit) {
  fam <- fit$family
  # The GLM score below is the score of an exponential-family (or
  # quasi-likelihood) mean model. mgcv's extended and general families
  # (betar, ocat, scat, ziP, tw, gaulss, cox.ph, ...) have other scores,
  # or several linear predictors, and are refused rather than given a
  # wrong sandwich. nb() is the exception: given its theta, its
  # coefficient score is the GLM score with V(mu) = mu + mu^2 / theta,
  # the score MASS::glm.nb() fits use.
  if (
    inherits(fam, c("extended.family", "general.family")) &&
      !startsWith(fam$family, "Negative Binomial")
  ) {
    spicy_abort(
      c(
        sprintf(
          "A robust variance is not available for a `%s` fit with the `%s` family.",
          class(fit)[1L],
          fam$family
        ),
        "i" = paste0(
          "spicy forms the sandwich of mgcv fits from the GLM score, which ",
          "covers the exponential families, the quasi families and nb() only."
        ),
        "i" = "Use `vcov = \"classical\"` for this fit."
      ),
      class = "spicy_unsupported_vcov"
    )
  }
  class(fit) <- c(.MGCV_SANDWICH_CLASS, class(fit))
  fit
}

.mgcv_sandwich_unwrap <- function(x) {
  class(x) <- setdiff(class(x), .MGCV_SANDWICH_CLASS)
  x
}

# Per-observation GLM quantities at the fitted values, from y, mu and
# eta alone: the working weight w = pw * mu.eta(eta)^2 / V(mu), the
# working residual times that weight, pw * (y - mu) * mu.eta(eta) / V(mu),
# and the dispersion sandwich's glm methods use, sum(wres^2) / sum(w),
# fixed at 1 for binomial, poisson and negative-binomial families.
# $linear.predictors includes any offset, so mu.eta() is evaluated at
# the right point; the design matrix carries no offset column.
.mgcv_sandwich_parts <- function(fit) {
  fam <- fit$family
  mu <- fit$fitted.values
  eta <- fit$linear.predictors
  pw <- fit$prior.weights %||% rep.int(1, length(mu))
  mu_eta <- fam$mu.eta(eta)
  v <- fam$variance(mu)
  w <- pw * mu_eta^2 / v
  wres <- pw * (fit$y - mu) * mu_eta / v
  fixed <- substr(fam$family, 1L, 17L) %in%
    c("poisson", "binomial", "Negative Binomial")
  phi <- if (fixed) 1 else sum(wres^2) / sum(w)
  list(w = w, wres = wres, phi = phi)
}

# mgcv's penalized unscaled covariance, summary(fit)$cov.unscaled
# (Vp / sig2), with the coefficient names sandwich carries through.
.mgcv_cov_unscaled <- function(fit) {
  V <- fit$Vp / fit$sig2
  nm <- names(stats::coef(fit))
  dimnames(V) <- list(nm, nm)
  V
}

# Score per observation: (y - mu) / V(mu) * mu.eta(eta) * pw * X / phi.
# sandwich sets the na.action class to "omit" before it calls estfun(),
# so the rows are the fitted ones. Under na.exclude (a direct call),
# model.matrix() of a gam pads the excluded rows with NA, and naresid()
# pads the score the same way, as estfun.glm() does.
#' @exportS3Method sandwich::estfun
#' @noRd
estfun.spicy_mgcv_sandwich <- function(x, ...) {
  fit <- .mgcv_sandwich_unwrap(x)
  parts <- .mgcv_sandwich_parts(fit)
  X <- stats::model.matrix(fit)
  rval <- stats::naresid(fit$na.action, parts$wres / parts$phi) * X
  attr(rval, "assign") <- NULL
  attr(rval, "contrasts") <- NULL
  attr(rval, "model.offset") <- NULL
  rval
}

# n * cov.unscaled * phi: the penalized bread, with the dispersion the
# score divides by. n is the number of rows of the fit (summary.gam()'s
# n), the n sandwich::sandwich() divides by.
#' @exportS3Method sandwich::bread
#' @noRd
bread.spicy_mgcv_sandwich <- function(x, ...) {
  fit <- .mgcv_sandwich_unwrap(x)
  length(fit$y) * .mgcv_cov_unscaled(fit) * .mgcv_sandwich_parts(fit)$phi
}

# Hat diagonal of the penalized fit, w_i * x_i' cov.unscaled x_i: equal
# to gam's $hat, and computed the same way for bam, whose $hat is not
# the hat diagonal. vcovHC() reads it for HC2-HC5. The weights are
# padded like the rows of model.matrix(), as in estfun() above.
#' @exportS3Method stats::hatvalues
#' @noRd
hatvalues.spicy_mgcv_sandwich <- function(model, ...) {
  fit <- .mgcv_sandwich_unwrap(model)
  X <- stats::model.matrix(fit)
  w <- stats::naresid(fit$na.action, .mgcv_sandwich_parts(fit)$w)
  w * rowSums((X %*% .mgcv_cov_unscaled(fit)) * X)
}
