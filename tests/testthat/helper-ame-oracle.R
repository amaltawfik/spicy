# ---------------------------------------------------------------------------
# An AME oracle independent of marginaleffects.
#
# The AME is recomputed from the coefficients: the model's predictions
# rebuilt by hand, a centered difference for a continuous predictor (step
# 1e-4 times its range, marginaleffects' default) or the contrast with the
# reference level for a factor, averaged with the weights. Its standard
# error is the delta method on a numDeriv Jacobian, with the coefficient
# variance aligned BY NAME on the coefficient vector.
#
#   prep(nd)     the design of a data set (X and offset), built once
#   pfun(b, D)   the n x K prediction matrix at coefficients b, its
#                columns named by the outcome categories when K > 1
#
# ame_oracle() returns a data frame keyed "term|category" (NA for a
# single outcome); ame_spicy() returns spicy's AME rows under the same
# keys.
# ---------------------------------------------------------------------------
ame_oracle <- function(prep, pfun, beta, V, d, vars, w = NULL) {
  stopifnot(!is.null(names(beta)), !anyDuplicated(names(beta)))
  V <- as.matrix(V)[names(beta), names(beta), drop = FALSE]
  w <- w %||% rep(1, nrow(d))
  setups <- list()
  for (v in vars) {
    x <- d[[v]]
    if (is.factor(x)) {
      lv <- levels(x)
      for (l in lv[-1L]) {
        lo <- hi <- d
        lo[[v]] <- factor(rep(lv[1L], nrow(d)), levels = lv)
        hi[[v]] <- factor(rep(l, nrow(d)), levels = lv)
        setups[[paste0(v, l)]] <- list(lo = prep(lo), hi = prep(hi), h = 1)
      }
    } else {
      h <- 1e-4 * diff(range(x))
      lo <- hi <- d
      lo[[v]] <- x - h / 2
      hi[[v]] <- x + h / 2
      setups[[v]] <- list(lo = prep(lo), hi = prep(hi), h = h)
    }
  }
  f <- function(b) {
    names(b) <- names(beta)
    unlist(lapply(names(setups), function(k) {
      s <- setups[[k]]
      P <- as.matrix(pfun(b, s$hi)) - as.matrix(pfun(b, s$lo))
      r <- colSums(w * P) / (sum(w) * s$h)
      names(r) <- paste(k, if (ncol(P) == 1L) NA else colnames(P), sep = "|")
      r
    }))
  }
  est <- f(beta)
  J <- numDeriv::jacobian(f, unname(beta))
  data.frame(
    key = names(est),
    est = unname(est),
    se = sqrt(rowSums((J %*% V) * J))
  )
}

# The design of a right-hand side whose predvars (the coefficients of
# poly(), the centre of scale()) are fixed on the estimation data d.
ame_prep_rhs <- function(rhs, d) {
  tt <- attr(stats::model.frame(rhs, d), "terms")
  xlev <- stats::.getXlevels(tt, stats::model.frame(tt, d))
  function(nd) {
    mf <- stats::model.frame(tt, nd, xlev = xlev, na.action = stats::na.pass)
    list(
      X = stats::model.matrix(tt, mf),
      off = stats::model.offset(mf) %||% 0
    )
  }
}

# Predictions of a single-index model through its inverse link.
ame_pf_link <- function(linkinv) {
  function(b, D) linkinv(drop(D$X %*% b[colnames(D$X)]) + D$off)
}

# spicy's AME rows, keyed as ame_oracle() keys them. marginaleffects'
# default Jacobian is a forward difference whose rounding error reaches
# 1e-4 of an SE in a nonlinear model: Richardson's (numDeriv) brings it
# below 1e-7, so a 1e-6 comparison tests the computation, not the step.
ame_spicy <- function(fit, ...) {
  op <- options(marginaleffects_numDeriv = list(method = "Richardson"))
  on.exit(options(op), add = TRUE)
  lo <- as.data.frame(table_regression(
    fit,
    show_columns = c("b", "ame", "ame_se"),
    output = "long",
    ...
  ))
  a <- lo[
    lo$estimate_type == "ame" &
      !(lo$is_reference %in% TRUE) &
      !is.na(lo$estimate),
  ]
  grp <- if ("outcome_level" %in% names(a)) {
    as.character(a$outcome_level)
  } else {
    rep(NA_character_, nrow(a))
  }
  term <- a$term
  pre <- !is.na(grp) & startsWith(term, paste0(grp, ": "))
  term[pre] <- substring(term[pre], nchar(grp[pre]) + 3L)
  data.frame(
    key = paste(term, grp, sep = "|"),
    est = a$estimate,
    se = a$std.error
  )
}

# Every oracle row met in spicy's table, point and SE within `tol`
# (relative).
expect_ame_matches <- function(sp, or, tol = 1e-6) {
  testthat::expect_setequal(sp$key, or$key)
  i <- match(or$key, sp$key)
  testthat::expect_equal(sp$est[i], or$est, tolerance = tol)
  testthat::expect_equal(sp$se[i], or$se, tolerance = tol)
}

# Data for the AME tests: a continuous x and z, a three-level factor grp,
# sampling weights sw, clusters and an exposure. Two names are avoided
# because survey 4.5 evaluates its own variables inside the data on a
# replicate design: svyglm() fails on a column `g`, and svyolr() takes a
# column `w` for every replicate weight, which gives a zero variance.
ame_test_data <- function(n = 300, seed = 20261119) {
  set.seed(seed)
  d <- data.frame(
    x = stats::rnorm(n),
    z = stats::runif(n, 0, 3),
    grp = factor(sample(c("a", "b", "c"), n, TRUE)),
    sw = stats::runif(n, 0.5, 2),
    expo = stats::runif(n, 0.5, 3)
  )
  d$id <- rep(seq_len(n / 5), each = 5)
  ge <- c(a = 0, b = 0.4, c = -0.3)[as.character(d$grp)]
  lin <- 0.3 + 0.5 * d$x + ge + 0.6 * sin(d$z)
  d$y <- lin + stats::rnorm(n)
  d$yb <- stats::rbinom(n, 1, stats::plogis(lin - 0.3))
  d$cnt <- stats::rpois(
    n,
    exp(-0.5 + 0.3 * d$x + ge / 2 + 0.2 * d$z + log(d$expo))
  )
  lat <- lin + stats::rlogis(n)
  d$ord <- cut(
    lat,
    stats::quantile(lat, c(0, 1 / 3, 2 / 3, 1)),
    include.lowest = TRUE,
    labels = c("lo", "mid", "hi")
  )
  d
}
