# Association measures for contingency tables.
#
# Formulas for asymptotic standard errors follow the implementations in
# DescTools (Signorell et al.), which in turn implement the formulas from
# Agresti (2002) and Liebetrau (1983).

# ── Internal helpers ──────────────────────────────────────────────────────────

.validate_table <- function(x, min_dim = c(2L, 2L)) {
  if (!inherits(x, "table")) {
    stop("`x` must be a contingency table (class `table`).", call. = FALSE)
  }
  if (length(dim(x)) != 2L) {
    stop("`x` must be a two-dimensional table.", call. = FALSE)
  }
  if (nrow(x) < min_dim[1] || ncol(x) < min_dim[2]) {
    stop(
      sprintf("`x` must be at least %dx%d.", min_dim[1], min_dim[2]),
      call. = FALSE
    )
  }
  invisible(NULL)
}


.assoc_result <- function(
  estimate,
  se = NA_real_,
  detail = FALSE,
  conf_level = 0.95,
  p_value = NA_real_,
  lower_bound = -Inf,
  upper_bound = Inf,
  .include_se = FALSE,
  digits = 3L
) {
  if (!detail) {
    return(estimate)
  }
  if (is.null(conf_level) || is.na(conf_level)) {
    out <- if (.include_se) {
      c(estimate = estimate, se = se, p_value = p_value)
    } else {
      c(estimate = estimate, p_value = p_value)
    }
    class(out) <- "spicy_assoc_detail"
    attr(out, "digits") <- digits
    return(out)
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  ci_lower <- max(lower_bound, estimate - z * se)
  ci_upper <- min(upper_bound, estimate + z * se)
  out <- if (.include_se) {
    c(
      estimate = estimate,
      se = se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value
    )
  } else {
    c(
      estimate = estimate,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value
    )
  }
  class(out) <- "spicy_assoc_detail"
  attr(out, "digits") <- digits
  out
}


#' Print a detailed association measure result
#'
#' Formats a `spicy_assoc_detail` vector (returned by association
#' functions with `detail = TRUE`) with fixed decimal places and
#' `< 0.001` notation for small p-values.
#'
#' @param x A `spicy_assoc_detail` object.
#' @param digits Number of decimal places for the estimate, SE, and
#'   confidence interval. Defaults to 3. The p-value is always
#'   formatted separately (`< 0.001` or three decimal places).
#' @param ... Ignored.
#' @return `x`, invisibly.
#'
#' @seealso [cramer_v()], [assoc_measures()]
#'
#' @export
print.spicy_assoc_detail <- function(
  x,
  digits = attr(x, "digits") %||% 3L,
  ...
) {
  nms <- names(x)
  labels <- c(
    estimate = "Estimate",
    se = "SE",
    ci_lower = "CI lower",
    ci_upper = "CI upper",
    p_value = "p"
  )
  hdr <- labels[nms]
  vals <- vapply(
    nms,
    function(nm) {
      v <- x[[nm]]
      if (is.na(v)) {
        return("--")
      }
      if (nm == "p_value") {
        if (v < 0.001) "< 0.001" else formatC(v, format = "f", digits = 3)
      } else {
        formatC(v, format = "f", digits = digits)
      }
    },
    character(1)
  )
  widths <- pmax(nchar(hdr), nchar(vals))
  hdr_parts <- mapply(formatC, hdr, width = widths)
  val_parts <- mapply(formatC, vals, width = widths)
  cat(
    paste(hdr_parts, collapse = "  "),
    "\n",
    paste(val_parts, collapse = "  "),
    "\n",
    sep = ""
  )
  invisible(x)
}


.concordance_counts <- function(x) {
  r <- nrow(x)
  k <- ncol(x)
  pi_c <- matrix(0, r, k)
  pi_d <- matrix(0, r, k)

  for (i in seq_len(r)) {
    for (j in seq_len(k)) {
      if (i < r && j < k) {
        pi_c[i, j] <- sum(x[(i + 1):r, (j + 1):k, drop = FALSE])
      }
      if (i < r && j > 1L) {
        pi_d[i, j] <- sum(x[(i + 1):r, 1:(j - 1), drop = FALSE])
      }
    }
  }

  # Also count from other direction for symmetry
  pi_c2 <- matrix(0, r, k)
  pi_d2 <- matrix(0, r, k)
  for (i in seq_len(r)) {
    for (j in seq_len(k)) {
      if (i > 1L && j > 1L) {
        pi_c2[i, j] <- sum(x[1:(i - 1), 1:(j - 1), drop = FALSE])
      }
      if (i > 1L && j < k) {
        pi_d2[i, j] <- sum(x[1:(i - 1), (j + 1):k, drop = FALSE])
      }
    }
  }

  C <- sum(x * pi_c)
  D <- sum(x * pi_d)

  list(
    C = C,
    D = D,
    pi_c = pi_c + pi_c2,
    pi_d = pi_d + pi_d2
  )
}


# ── Nominal measures ─────────────────────────────────────────────────────────

#' Cramer's V
#'
#' `cramer_v()` computes Cramer's V for a two-way contingency table,
#' measuring the strength of association between two categorical variables.
#'
#' @param x A contingency table (of class `table`).
#' @param detail Logical. If `FALSE` (default), return the estimate
#'   as a numeric scalar. If `TRUE`, return a named numeric vector
#'   including confidence interval and p-value.
#' @param conf_level A number between 0 and 1 giving the confidence
#'   level (default `0.95`). Only used when `detail = TRUE`. Set
#'   to `NULL` to omit the confidence interval.
#' @param digits Number of decimal places used when printing the
#'   result (default `3`). Only affects the `detail = TRUE` output.
#' @param .include_se Internal parameter; do not use.
#'
#' @return When `detail = FALSE`: a single numeric value (the
#'   estimate).
#'   When `detail = TRUE` and `conf_level` is non-`NULL`:
#'   `c(estimate, ci_lower, ci_upper, p_value)`.
#'   When `detail = TRUE` and `conf_level = NULL`:
#'   `c(estimate, p_value)`.
#'   The p-value tests the null hypothesis of no association
#'   (Pearson chi-squared test).
#'
#' @details
#' Cramer's V is computed as
#' \eqn{V = \sqrt{\chi^2 / (n \cdot (k - 1))}}, where \eqn{\chi^2}
#' is the Pearson chi-squared statistic, \eqn{n} is the total count,
#' and \eqn{k = \min(r, c)}.
#' The confidence interval uses the Fisher z-transformation.
#' Standard error formulas follow the DescTools implementations
#' (Signorell et al., 2024).
#'
#' @references
#' Agresti, A. (2002). *Categorical Data Analysis* (2nd ed.). Wiley.
#'
#' Liebetrau, A. M. (1983). *Measures of Association*. Sage.
#'
#' Signorell, A. et al. (2024). *DescTools: Tools for Descriptive
#' Statistics*. R package.
#'
#' @examples
#' tab <- table(sochealth$smoking, sochealth$education)
#' cramer_v(tab)
#' cramer_v(tab, detail = TRUE)
#' cramer_v(tab, detail = TRUE, conf_level = NULL)
#'
#' @seealso [phi()], [contingency_coef()], [assoc_measures()]
#' @export
cramer_v <- function(
  x,
  detail = FALSE,
  conf_level = 0.95,
  digits = 3L,
  .include_se = FALSE
) {
  .validate_table(x)
  n <- sum(x)
  k <- min(nrow(x), ncol(x)) - 1L
  if (n <= 0 || k <= 0) {
    warning(
      "Cramer's V is undefined for this table; returning NA.",
      call. = FALSE
    )
    return(c(estimate = NA_real_))
  }
  chi <- suppressWarnings(stats::chisq.test(x, correct = FALSE))
  chi2 <- as.numeric(chi$statistic)
  V <- sqrt(chi2 / (n * k))

  if (!detail) {
    return(V)
  }

  p_value <- as.numeric(chi$p.value)

  if (is.null(conf_level) || is.na(conf_level)) {
    out <- if (.include_se) {
      c(estimate = V, se = NA_real_, p_value = p_value)
    } else {
      c(estimate = V, p_value = p_value)
    }
    class(out) <- "spicy_assoc_detail"
    attr(out, "digits") <- digits
    return(out)
  }

  z_alpha <- stats::qnorm(1 - (1 - conf_level) / 2)
  if (n > 3 && V > 0) {
    z_V <- atanh(V)
    se_z <- 1 / sqrt(n - 3)
    ci_lower <- max(0, tanh(z_V - z_alpha * se_z))
    ci_upper <- min(1, tanh(z_V + z_alpha * se_z))
  } else {
    ci_lower <- NA_real_
    ci_upper <- NA_real_
  }

  out <- if (.include_se) {
    c(
      estimate = V,
      se = NA_real_,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value
    )
  } else {
    c(
      estimate = V,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value
    )
  }
  class(out) <- "spicy_assoc_detail"
  attr(out, "digits") <- digits
  out
}


#' Phi coefficient
#'
#' `phi()` computes the phi coefficient for a 2x2 contingency table.
#'
#' @inheritParams cramer_v
#'
#' @return Same structure as [cramer_v()]: a scalar when
#'   `detail = FALSE`, a named vector when `detail = TRUE`.
#'   The p-value tests the null hypothesis of no association
#'   (Pearson chi-squared test).
#'
#' @details
#' The phi coefficient is \eqn{\phi = \sqrt{\chi^2 / n}}.
#' It is equivalent to Cramer's V for 2x2 tables and equals the
#' Pearson correlation between the two binary variables.
#' The confidence interval uses the Fisher z-transformation.
#' Standard error formulas follow the DescTools implementations
#' (Signorell et al., 2024); see [cramer_v()] for full references.
#'
#' @examples
#' tab <- table(sochealth$smoking, sochealth$sex)
#' phi(tab)
#' phi(tab, detail = TRUE)
#'
#' @seealso [cramer_v()], [yule_q()], [assoc_measures()]
#' @export
phi <- function(
  x,
  detail = FALSE,
  conf_level = 0.95,
  digits = 3L,
  .include_se = FALSE
) {
  .validate_table(x, min_dim = c(2L, 2L))
  if (nrow(x) != 2L || ncol(x) != 2L) {
    stop("`x` must be a 2x2 table for the phi coefficient.", call. = FALSE)
  }
  n <- sum(x)
  chi <- suppressWarnings(stats::chisq.test(x, correct = FALSE))
  chi2 <- as.numeric(chi$statistic)
  ph <- sqrt(chi2 / n)

  if (!detail) {
    return(ph)
  }

  p_value <- as.numeric(chi$p.value)

  if (is.null(conf_level) || is.na(conf_level)) {
    out <- if (.include_se) {
      c(estimate = ph, se = NA_real_, p_value = p_value)
    } else {
      c(estimate = ph, p_value = p_value)
    }
    class(out) <- "spicy_assoc_detail"
    attr(out, "digits") <- digits
    return(out)
  }

  z_alpha <- stats::qnorm(1 - (1 - conf_level) / 2)
  if (n > 3 && ph > 0) {
    z_ph <- atanh(ph)
    se_z <- 1 / sqrt(n - 3)
    ci_lower <- max(0, tanh(z_ph - z_alpha * se_z))
    ci_upper <- min(1, tanh(z_ph + z_alpha * se_z))
  } else {
    ci_lower <- NA_real_
    ci_upper <- NA_real_
  }

  out <- if (.include_se) {
    c(
      estimate = ph,
      se = NA_real_,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value
    )
  } else {
    c(
      estimate = ph,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value
    )
  }
  class(out) <- "spicy_assoc_detail"
  attr(out, "digits") <- digits
  out
}


#' Pearson's contingency coefficient
#'
#' `contingency_coef()` computes Pearson's contingency coefficient C
#' for a two-way contingency table.
#'
#' @inheritParams cramer_v
#'
#' @return Same structure as [cramer_v()]: a scalar when
#'   `detail = FALSE`, a named vector when `detail = TRUE`.
#'   The p-value tests the null hypothesis of no association
#'   (Pearson chi-squared test). CI values are `NA` because no
#'   standard asymptotic SE exists for C.
#'
#' @details
#' The contingency coefficient is
#' \eqn{C = \sqrt{\chi^2 / (\chi^2 + n)}}.
#' It ranges from 0 (independence) to a maximum that depends on
#' the table dimensions. No standard asymptotic standard error exists,
#' so the confidence interval is not computed.
#'
#' @examples
#' tab <- table(sochealth$smoking, sochealth$education)
#' contingency_coef(tab)
#'
#' @seealso [cramer_v()], [assoc_measures()]
#' @export
contingency_coef <- function(
  x,
  detail = FALSE,
  conf_level = 0.95,
  digits = 3L,
  .include_se = FALSE
) {
  .validate_table(x)
  n <- sum(x)
  chi <- suppressWarnings(stats::chisq.test(x, correct = FALSE))
  chi2 <- as.numeric(chi$statistic)
  C_val <- sqrt(chi2 / (chi2 + n))

  if (!detail) {
    return(C_val)
  }

  p_value <- as.numeric(chi$p.value)

  if (is.null(conf_level) || is.na(conf_level)) {
    out <- if (.include_se) {
      c(estimate = C_val, se = NA_real_, p_value = p_value)
    } else {
      c(estimate = C_val, p_value = p_value)
    }
    class(out) <- "spicy_assoc_detail"
    attr(out, "digits") <- digits
    return(out)
  }

  out <- if (.include_se) {
    c(
      estimate = C_val,
      se = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      p_value = p_value
    )
  } else {
    c(
      estimate = C_val,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      p_value = p_value
    )
  }
  class(out) <- "spicy_assoc_detail"
  attr(out, "digits") <- digits
  out
}


#' Yule's Q
#'
#' `yule_q()` computes Yule's Q coefficient of association for a 2x2
#' contingency table.
#'
#' @inheritParams cramer_v
#'
#' @return Same structure as [cramer_v()]: a scalar when
#'   `detail = FALSE`, a named vector when `detail = TRUE`.
#'   The p-value tests H0: Q = 0 (Wald z-test).
#'
#' @details
#' For a 2x2 table with cells \eqn{a, b, c, d}, Yule's Q is
#' \eqn{Q = (ad - bc) / (ad + bc)}.
#' It is equivalent to the Goodman-Kruskal Gamma for 2x2 tables.
#' The asymptotic standard error is
#' \eqn{SE = 0.5 (1 - Q^2) \sqrt{1/a + 1/b + 1/c + 1/d}}.
#' Standard error formulas follow the DescTools implementations
#' (Signorell et al., 2024); see [cramer_v()] for full references.
#'
#' @examples
#' tab <- table(sochealth$smoking, sochealth$sex)
#' yule_q(tab)
#'
#' @seealso [phi()], [gamma_gk()], [assoc_measures()]
#' @export
yule_q <- function(
  x,
  detail = FALSE,
  conf_level = 0.95,
  digits = 3L,
  .include_se = FALSE
) {
  .validate_table(x, min_dim = c(2L, 2L))
  if (nrow(x) != 2L || ncol(x) != 2L) {
    stop("`x` must be a 2x2 table for Yule's Q.", call. = FALSE)
  }
  a <- x[1, 1]
  b <- x[1, 2]
  c_ <- x[2, 1]
  d <- x[2, 2]
  ad <- a * d
  bc <- b * c_

  if (ad + bc == 0) {
    warning(
      "Yule's Q is undefined when ad + bc = 0; returning NA.",
      call. = FALSE
    )
    return(c(estimate = NA_real_))
  }

  Q <- (ad - bc) / (ad + bc)

  if (!detail) {
    return(Q)
  }

  if (any(c(a, b, c_, d) == 0)) {
    se <- NA_real_
    p_value <- NA_real_
  } else {
    se <- 0.5 * (1 - Q^2) * sqrt(1 / a + 1 / b + 1 / c_ + 1 / d)
    p_value <- 2 * stats::pnorm(-abs(Q / se))
  }

  .assoc_result(
    Q,
    se,
    detail = TRUE,
    conf_level = conf_level,
    p_value = p_value,
    lower_bound = -1,
    upper_bound = 1,
    .include_se = .include_se,
    digits = digits
  )
}


#' Goodman-Kruskal's Lambda
#'
#' `lambda_gk()` computes Goodman-Kruskal's Lambda, a proportional
#' reduction in error (PRE) measure for nominal variables.
#'
#' @inheritParams cramer_v
#' @param direction Direction of prediction:
#'   `"symmetric"` (default), `"row"` (column predicts row),
#'   or `"column"` (row predicts column).
#'
#' @return Same structure as [cramer_v()]: a scalar when
#'   `detail = FALSE`, a named vector when `detail = TRUE`.
#'   The p-value tests H0: lambda = 0 (Wald z-test).
#'
#' @details
#' Lambda measures how much prediction error is reduced when
#' the independent variable is used to predict the dependent
#' variable. It ranges from 0 (no reduction) to 1 (perfect
#' prediction). Lambda can equal zero even when variables
#' are associated if the modal category dominates in every
#' column (or row).
#' Standard error formulas follow the DescTools implementations
#' (Signorell et al., 2024); see [cramer_v()] for full references.
#'
#' @examples
#' tab <- table(sochealth$smoking, sochealth$education)
#' lambda_gk(tab)
#' lambda_gk(tab, direction = "row")
#' lambda_gk(tab, direction = "column", detail = TRUE)
#'
#' @seealso [goodman_kruskal_tau()], [uncertainty_coef()],
#'   [assoc_measures()]
#' @export
lambda_gk <- function(
  x,
  direction = c("symmetric", "row", "column"),
  detail = FALSE,
  conf_level = 0.95,
  digits = 3L,
  .include_se = FALSE
) {
  .validate_table(x)
  direction <- match.arg(direction)
  n <- sum(x)

  nr <- nrow(x)
  nc <- ncol(x)
  rsum <- rowSums(x)
  csum <- colSums(x)
  rmax <- apply(x, 1, max)
  cmax <- apply(x, 2, max)
  max_rsum <- max(rsum)
  max_csum <- max(csum)

  est <- switch(direction,
    symmetric = {
      0.5 *
        (sum(rmax, cmax) - (max_csum + max_rsum)) /
        (n - 0.5 * (max_csum + max_rsum))
    },
    column = (sum(rmax) - max_csum) / (n - max_csum),
    row = (sum(cmax) - max_rsum) / (n - max_rsum)
  )

  if (!detail) {
    return(est)
  }

  # ASE (DescTools approach)
  sigma2 <- switch(direction,
    row = {
      L_row_max <- min(which(rsum == max_rsum))
      L_row <- integer(nc)
      for (i in seq_len(nc)) {
        if (x[L_row_max, i] == max_rsum) {
          L_row[i] <- L_row_max
        } else {
          L_row[i] <- min(which(x[, i] == cmax[i]))
        }
      }
      (n - sum(cmax)) *
        (sum(cmax) + max_rsum - 2 * sum(cmax[which(L_row == L_row_max)])) /
        (n - max_rsum)^3
    },
    column = {
      L_col_max <- min(which(csum == max_csum))
      L_col <- integer(nr)
      for (i in seq_len(nr)) {
        if (x[i, L_col_max] == max_csum) {
          L_col[i] <- L_col_max
        } else {
          L_col[i] <- min(which(x[i, ] == rmax[i]))
        }
      }
      (n - sum(rmax)) *
        (sum(rmax) + max_csum - 2 * sum(rmax[which(L_col == L_col_max)])) /
        (n - max_csum)^3
    },
    symmetric = {
      l <- min(which(csum == max_csum))
      k <- min(which(rsum == max_rsum))
      li <- apply(x, 1, which.max)
      ki <- apply(x, 2, which.max)
      w <- 2 * n - max_csum - max_rsum
      v <- 2 * n - sum(rmax, cmax)
      xx <- sum(rmax[li == l], cmax[ki == k], rmax[k], cmax[l])
      y <- 8 * n - w - v - 2 * xx
      t_vec <- vapply(
        seq_along(li),
        \(i) {
          ki[li[i]] == i && li[ki[li[i]]] == li[i]
        },
        logical(1)
      )
      (w * v * y - 2 * w^2 * (n - sum(rmax[t_vec])) - 2 * v^2 * (n - x[k, l])) /
        w^4
    }
  )

  se <- sqrt(max(0, sigma2))
  p_value <- if (se > 0) {
    2 * stats::pnorm(-abs(est / se))
  } else {
    NA_real_
  }

  .assoc_result(
    est,
    se,
    detail = TRUE,
    conf_level = conf_level,
    p_value = p_value,
    lower_bound = 0,
    upper_bound = 1,
    .include_se = .include_se,
    digits = digits
  )
}


#' Goodman-Kruskal's Tau
#'
#' `goodman_kruskal_tau()` computes Goodman-Kruskal's Tau, a
#' proportional reduction in error (PRE) measure for nominal
#' variables.
#'
#' @inheritParams cramer_v
#' @param direction Direction of prediction:
#'   `"row"` (default, column predicts row) or
#'   `"column"` (row predicts column).
#'
#' @return Same structure as [cramer_v()]: a scalar when
#'   `detail = FALSE`, a named vector when `detail = TRUE`.
#'   The p-value tests H0: tau = 0 (Wald z-test).
#'
#' @details
#' Unlike [lambda_gk()], Goodman-Kruskal's Tau uses all cell
#' frequencies rather than only the modal categories, making it
#' more sensitive to association patterns where lambda may be zero.
#' Standard error formulas follow the DescTools implementations
#' (Signorell et al., 2024); see [cramer_v()] for full references.
#'
#' @examples
#' tab <- table(sochealth$smoking, sochealth$education)
#' goodman_kruskal_tau(tab)
#' goodman_kruskal_tau(tab, direction = "column", detail = TRUE)
#'
#' @seealso [lambda_gk()], [uncertainty_coef()], [assoc_measures()]
#' @export
goodman_kruskal_tau <- function(
  x,
  direction = c("row", "column"),
  detail = FALSE,
  conf_level = 0.95,
  digits = 3L,
  .include_se = FALSE
) {
  .validate_table(x)
  direction <- match.arg(direction)
  n <- sum(x)
  rsum <- rowSums(x)
  csum <- colSums(x)

  if (direction == "row") {
    # Column predicts row
    v <- sum(rsum^2) / n
    d <- 0
    for (j in seq_len(ncol(x))) {
      if (csum[j] > 0) {
        d <- d + sum(x[, j]^2) / csum[j]
      }
    }
    tau <- (d - v) / (n - v)
  } else {
    # Row predicts column
    v <- sum(csum^2) / n
    d <- 0
    for (i in seq_len(nrow(x))) {
      if (rsum[i] > 0) {
        d <- d + sum(x[i, ]^2) / rsum[i]
      }
    }
    tau <- (d - v) / (n - v)
  }

  if (!detail) {
    return(unname(tau))
  }

  # ASE (DescTools gradient approach)
  se <- .ase_gk_tau(x, direction)
  p_value <- if (!is.na(se) && se > 0) {
    2 * stats::pnorm(-abs(tau / se))
  } else {
    NA_real_
  }

  .assoc_result(
    unname(tau),
    se,
    detail = TRUE,
    conf_level = conf_level,
    p_value = unname(p_value),
    lower_bound = 0,
    upper_bound = 1,
    .include_se = .include_se,
    digits = digits
  )
}


.ase_gk_tau <- function(x, direction) {
  n <- sum(x)
  rsum <- rowSums(x)
  csum <- colSums(x)
  r <- nrow(x)
  k <- ncol(x)

  # DescTools formula exactly
  n_err_uncond <- n^2
  if (direction == "row") {
    for (j in seq_len(k)) {
      if (csum[j] > 0) {
        n_err_uncond <- n_err_uncond - n * sum(x[, j]^2 / csum[j])
      }
    }
    n_err_cond <- n^2 - sum(rsum^2)
    v <- n_err_uncond / n^2
    d <- n_err_cond / n^2
    f <- d * (v + 1) - 2 * v
    var_tau <- 0
    for (i in seq_len(r)) {
      for (j in seq_len(k)) {
        if (csum[j] > 0) {
          term <- -2 *
            v *
            (rsum[i] / n) +
            d *
              ((2 * x[i, j] / csum[j]) -
                sum((x[, j] / csum[j])^2)) -
            f
          var_tau <- var_tau + x[i, j] * term^2 / (n^2 * d^4)
        }
      }
    }
  } else {
    for (i in seq_len(r)) {
      if (rsum[i] > 0) {
        n_err_uncond <- n_err_uncond - n * sum(x[i, ]^2 / rsum[i])
      }
    }
    n_err_cond <- n^2 - sum(csum^2)
    v <- n_err_uncond / n^2
    d <- n_err_cond / n^2
    f <- d * (v + 1) - 2 * v
    var_tau <- 0
    for (i in seq_len(r)) {
      for (j in seq_len(k)) {
        if (rsum[i] > 0) {
          term <- -2 *
            v *
            (csum[j] / n) +
            d *
              ((2 * x[i, j] / rsum[i]) -
                sum((x[i, ] / rsum[i])^2)) -
            f
          var_tau <- var_tau + x[i, j] * term^2 / (n^2 * d^4)
        }
      }
    }
  }
  sqrt(max(0, var_tau))
}


#' Uncertainty Coefficient
#'
#' `uncertainty_coef()` computes the Uncertainty Coefficient
#' (Theil's U) for a two-way contingency table, based on
#' information entropy.
#'
#' @inheritParams cramer_v
#' @param direction Direction of prediction:
#'   `"symmetric"` (default), `"row"` (column predicts row),
#'   or `"column"` (row predicts column).
#'
#' @return Same structure as [cramer_v()]: a scalar when
#'   `detail = FALSE`, a named vector when `detail = TRUE`.
#'   The p-value tests H0: U = 0 (Wald z-test).
#'
#' @details
#' The uncertainty coefficient measures association using
#' Shannon entropy.
#' For `direction = "row"`:
#' \eqn{U = (H_X + H_Y - H_{XY}) / H_X}, where \eqn{H_X},
#' \eqn{H_Y} are the marginal entropies and \eqn{H_{XY}} is
#' the joint entropy.
#' The symmetric version is
#' \eqn{U = 2 (H_X + H_Y - H_{XY}) / (H_X + H_Y)}.
#' Standard error formulas follow the DescTools implementations
#' (Signorell et al., 2024); see [cramer_v()] for full references.
#'
#' @examples
#' tab <- table(sochealth$smoking, sochealth$education)
#' uncertainty_coef(tab)
#' uncertainty_coef(tab, direction = "row", detail = TRUE)
#'
#' @seealso [lambda_gk()], [goodman_kruskal_tau()], [assoc_measures()]
#' @export
uncertainty_coef <- function(
  x,
  direction = c("symmetric", "row", "column"),
  detail = FALSE,
  conf_level = 0.95,
  digits = 3L,
  .include_se = FALSE
) {
  .validate_table(x)
  direction <- match.arg(direction)
  n <- sum(x)

  rsum <- rowSums(x)
  csum <- colSums(x)

  # DescTools uses natural log on counts/n
  H_x <- -sum((rsum * log(rsum / n)) / n)
  H_y <- -sum((csum * log(csum / n)) / n)
  H_xy <- -sum(x[x > 0] * log(x[x > 0] / n) / n)

  mi <- H_x + H_y - H_xy # mutual information

  U <- switch(direction,
    row = if (H_x > 0) mi / H_x else 0,
    column = if (H_y > 0) mi / H_y else 0,
    symmetric = if (H_x + H_y > 0) 2 * mi / (H_x + H_y) else 0
  )

  if (!detail) {
    return(U)
  }

  # ASE (DescTools approach)
  se <- .ase_uncert_coef(x, direction, H_x, H_y, H_xy)
  p_value <- if (!is.na(se) && se > 0) {
    2 * stats::pnorm(-abs(U / se))
  } else {
    NA_real_
  }

  .assoc_result(
    U,
    se,
    detail = TRUE,
    conf_level = conf_level,
    p_value = p_value,
    lower_bound = 0,
    upper_bound = 1,
    .include_se = .include_se,
    digits = digits
  )
}


.ase_uncert_coef <- function(x, direction, H_x, H_y, H_xy) {
  n <- sum(x)
  rsum <- rowSums(x)
  csum <- colSums(x)
  r <- nrow(x)
  k <- ncol(x)

  # DescTools formula exactly
  if (direction == "symmetric") {
    log_outer <- log(rsum %o% csum / n^2)
    log_xn <- matrix(0, r, k)
    nz <- x > 0
    log_xn[nz] <- log(x[nz] / n)
    terms <- x * (H_xy * log_outer - (H_x + H_y) * log_xn)^2
    terms[!nz] <- 0
    sigma2 <- 4 * sum(terms) / (n^2 * (H_x + H_y)^4)
  } else {
    var_rc <- 0
    var_cr <- 0
    for (i in seq_len(r)) {
      for (j in seq_len(k)) {
        if (x[i, j] > 0) {
          var_rc <- var_rc +
            x[i, j] *
              (H_x *
                log(x[i, j] / csum[j]) +
                (H_y - H_xy) * log(rsum[i] / n))^2 /
              (n^2 * H_x^4)
          var_cr <- var_cr +
            x[i, j] *
              (H_y *
                log(x[i, j] / rsum[i]) +
                (H_x - H_xy) * log(csum[j] / n))^2 /
              (n^2 * H_y^4)
        }
      }
    }
    sigma2 <- if (direction == "row") var_rc else var_cr
  }
  sqrt(max(0, sigma2))
}


# ── Ordinal measures ─────────────────────────────────────────────────────────

#' Goodman-Kruskal Gamma
#'
#' `gamma_gk()` computes the Goodman-Kruskal Gamma statistic for a
#' two-way contingency table of ordinal variables.
#'
#' @inheritParams cramer_v
#'
#' @return Same structure as [cramer_v()]: a scalar when
#'   `detail = FALSE`, a named vector when `detail = TRUE`.
#'   The p-value tests H0: gamma = 0 (Wald z-test).
#'
#' @details
#' Gamma is computed as \eqn{\gamma = (C - D) / (C + D)}, where
#' \eqn{C} and \eqn{D} are the numbers of concordant and
#' discordant pairs. It ignores tied pairs, making it appropriate
#' for ordinal variables with many ties.
#' Standard error formulas follow the DescTools implementations
#' (Signorell et al., 2024); see [cramer_v()] for full references.
#'
#' @examples
#' tab <- table(sochealth$education, sochealth$self_rated_health)
#' gamma_gk(tab)
#' gamma_gk(tab, detail = TRUE)
#'
#' @seealso [kendall_tau_b()], [kendall_tau_c()], [somers_d()],
#'   [assoc_measures()]
#' @export
gamma_gk <- function(
  x,
  detail = FALSE,
  conf_level = 0.95,
  digits = 3L,
  .include_se = FALSE
) {
  .validate_table(x)
  cd <- .concordance_counts(x)
  C <- cd$C
  D <- cd$D

  if (C + D == 0) {
    warning("No concordant or discordant pairs; returning NA.", call. = FALSE)
    return(c(estimate = NA_real_))
  }

  G <- (C - D) / (C + D)

  if (!detail) {
    return(G)
  }

  # ASE (Agresti, 2002)
  psi <- 2 * (D * cd$pi_c - C * cd$pi_d) / (C + D)^2
  se <- sqrt(sum(x * psi^2) - (sum(x * psi))^2)

  p_value <- 2 * stats::pnorm(-abs(G / se))

  .assoc_result(
    G,
    se,
    detail = TRUE,
    conf_level = conf_level,
    p_value = p_value,
    lower_bound = -1,
    upper_bound = 1,
    .include_se = .include_se,
    digits = digits
  )
}


#' Kendall's Tau-b
#'
#' `kendall_tau_b()` computes Kendall's Tau-b for a two-way
#' contingency table of ordinal variables.
#'
#' @inheritParams cramer_v
#'
#' @return Same structure as [cramer_v()]: a scalar when
#'   `detail = FALSE`, a named vector when `detail = TRUE`.
#'   The p-value tests H0: tau-b = 0 (Wald z-test).
#'
#' @details
#' Kendall's Tau-b is computed as
#' \eqn{\tau_b = (C - D) / \sqrt{(n_0 - n_1)(n_0 - n_2)}},
#' where \eqn{n_0 = n(n-1)/2}, \eqn{n_1} is the number of
#' pairs tied on the row variable, and \eqn{n_2} is the number
#' tied on the column variable. Tau-b corrects for ties and is
#' appropriate for square tables.
#' Standard error formulas follow the DescTools implementations
#' (Signorell et al., 2024); see [cramer_v()] for full references.
#'
#' @examples
#' tab <- table(sochealth$education, sochealth$self_rated_health)
#' kendall_tau_b(tab)
#'
#' @seealso [kendall_tau_c()], [gamma_gk()], [somers_d()],
#'   [assoc_measures()]
#' @export
kendall_tau_b <- function(
  x,
  detail = FALSE,
  conf_level = 0.95,
  digits = 3L,
  .include_se = FALSE
) {
  .validate_table(x)
  n <- sum(x)
  cd <- .concordance_counts(x)
  C <- cd$C
  D <- cd$D

  rsum <- rowSums(x)
  csum <- colSums(x)

  n0 <- n * (n - 1) / 2
  n1 <- sum(rsum * (rsum - 1)) / 2 # row ties
  n2 <- sum(csum * (csum - 1)) / 2 # column ties

  denom <- sqrt((n0 - n1) * (n0 - n2))
  if (denom == 0) {
    warning("Tau-b is undefined for this table; returning NA.", call. = FALSE)
    return(c(estimate = NA_real_))
  }

  tau_b <- (C - D) / denom

  if (!detail) {
    return(tau_b)
  }

  # ASE (Brown & Benedetti, via DescTools)
  pi <- x / n
  rowsum <- rowSums(pi)
  colsum <- colSums(pi)
  pdiff <- (cd$pi_c - cd$pi_d) / n
  Pdiff <- 2 * (C - D) / n^2
  rowmat <- matrix(rep(rowsum, ncol(x)), ncol = ncol(x))
  colmat <- matrix(rep(colsum, nrow(x)), nrow = nrow(x), byrow = TRUE)
  delta1 <- sqrt(1 - sum(rowsum^2))
  delta2 <- sqrt(1 - sum(colsum^2))

  tauphi <- (2 * pdiff + Pdiff * colmat) *
    delta2 *
    delta1 +
    (Pdiff * rowmat * delta2) / delta1

  se_sq <- (sum(pi * tauphi^2) - sum(pi * tauphi)^2) /
    (delta1 * delta2)^4 /
    n
  if (se_sq < .Machine$double.eps * 10) {
    se_sq <- 0
  }
  se <- sqrt(se_sq)

  p_value <- 2 * stats::pnorm(-abs(tau_b / se))

  .assoc_result(
    tau_b,
    se,
    detail = TRUE,
    conf_level = conf_level,
    p_value = p_value,
    lower_bound = -1,
    upper_bound = 1,
    .include_se = .include_se,
    digits = digits
  )
}


#' Kendall's Tau-c (Stuart's Tau-c)
#'
#' `kendall_tau_c()` computes Stuart's Tau-c (also known as
#' Kendall's Tau-c) for a two-way contingency table of ordinal
#' variables.
#'
#' @inheritParams cramer_v
#'
#' @return Same structure as [cramer_v()]: a scalar when
#'   `detail = FALSE`, a named vector when `detail = TRUE`.
#'   The p-value tests H0: tau-c = 0 (Wald z-test).
#'
#' @details
#' Stuart's Tau-c is computed as
#' \eqn{\tau_c = 2m(C - D) / (n^2(m - 1))}, where
#' \eqn{m = \min(r, c)}. It is appropriate for rectangular
#' tables and is not restricted to the range \eqn{[-1, 1]} only for
#' square tables.
#' Standard error formulas follow the DescTools implementations
#' (Signorell et al., 2024); see [cramer_v()] for full references.
#'
#' @examples
#' tab <- table(sochealth$education, sochealth$self_rated_health)
#' kendall_tau_c(tab)
#'
#' @seealso [kendall_tau_b()], [gamma_gk()], [somers_d()],
#'   [assoc_measures()]
#' @export
kendall_tau_c <- function(
  x,
  detail = FALSE,
  conf_level = 0.95,
  digits = 3L,
  .include_se = FALSE
) {
  .validate_table(x)
  n <- sum(x)
  m <- min(nrow(x), ncol(x))
  cd <- .concordance_counts(x)
  C <- cd$C
  D <- cd$D

  if (m <= 1) {
    warning(
      "Tau-c requires at least 2 rows and 2 columns; returning NA.",
      call. = FALSE
    )
    return(c(estimate = NA_real_))
  }

  tau_c <- 2 * m * (C - D) / (n^2 * (m - 1))

  if (!detail) {
    return(tau_c)
  }

  # ASE (DescTools approach)
  sigma2 <- 4 *
    m^2 /
    ((m - 1)^2 * n^4) *
    (sum(x * (cd$pi_c - cd$pi_d)^2) - 4 * (C - D)^2 / n)
  se <- sqrt(max(0, sigma2))

  p_value <- 2 * stats::pnorm(-abs(tau_c / se))

  .assoc_result(
    tau_c,
    se,
    detail = TRUE,
    conf_level = conf_level,
    p_value = p_value,
    lower_bound = -1,
    upper_bound = 1,
    .include_se = .include_se,
    digits = digits
  )
}


#' Somers' D
#'
#' `somers_d()` computes Somers' D for a two-way contingency
#' table of ordinal variables.
#'
#' @inheritParams cramer_v
#' @param direction Direction of prediction:
#'   `"row"` (default, column predicts row),
#'   `"column"` (row predicts column),
#'   or `"symmetric"` (average of both directions).
#'
#' @return Same structure as [cramer_v()]: a scalar when
#'   `detail = FALSE`, a named vector when `detail = TRUE`.
#'   The p-value tests H0: D = 0 (Wald z-test).
#'
#' @details
#' Somers' D is an asymmetric ordinal measure defined as
#' \eqn{d = (C - D) / (C + D + T)}, where \eqn{T} is the
#' number of pairs tied on the independent variable.
#' The symmetric version is the harmonic mean of the two
#' asymmetric values.
#' Standard error formulas follow the DescTools implementations
#' (Signorell et al., 2024); see [cramer_v()] for full references.
#'
#' @examples
#' tab <- table(sochealth$education, sochealth$self_rated_health)
#' somers_d(tab, direction = "row")
#' somers_d(tab, direction = "column", detail = TRUE)
#'
#' @seealso [kendall_tau_b()], [gamma_gk()], [assoc_measures()]
#' @export
somers_d <- function(
  x,
  direction = c("row", "column", "symmetric"),
  detail = FALSE,
  conf_level = 0.95,
  digits = 3L,
  .include_se = FALSE
) {
  .validate_table(x)
  direction <- match.arg(direction)
  n <- sum(x)
  cd <- .concordance_counts(x)
  C <- cd$C
  D <- cd$D

  rsum <- rowSums(x)
  csum <- colSums(x)

  n0 <- n * (n - 1) / 2
  n1 <- sum(rsum * (rsum - 1)) / 2 # row ties
  n2 <- sum(csum * (csum - 1)) / 2 # column ties

  # DescTools: ni. = colSums for "row", rowSums for "column"
  ni <- switch(direction,
    row = csum,
    column = rsum,
    symmetric = NULL
  )

  if (direction == "symmetric") {
    # Symmetric Somers' d equals tau-b
    return(kendall_tau_b(
      x,
      detail = detail,
      conf_level = conf_level,
      .include_se = .include_se
    ))
  }

  denom <- n0 - switch(direction,
    row = n2,
    column = n1
  )
  if (denom == 0) {
    warning(
      "Somers' d is undefined for this table; returning NA.",
      call. = FALSE
    )
    return(c(estimate = NA_real_))
  }
  d_val <- (C - D) / denom

  if (!detail) {
    return(d_val)
  }

  # ASE (DescTools formula)
  wt <- n^2 - sum(ni^2)
  if (direction == "row") {
    n_minus_ni <- matrix(
      rep(n - csum, each = nrow(x)),
      nrow = nrow(x),
      ncol = ncol(x)
    )
  } else {
    n_minus_ni <- matrix(rep(n - rsum, ncol(x)), nrow = nrow(x), ncol = ncol(x))
  }
  sigma2 <- 4 /
    wt^4 *
    sum(x * (wt * (cd$pi_c - cd$pi_d) - 2 * (C - D) * n_minus_ni)^2)
  se <- sqrt(max(0, sigma2))

  p_value <- if (se > 0) {
    2 * stats::pnorm(-abs(d_val / se))
  } else {
    NA_real_
  }

  .assoc_result(
    d_val,
    se,
    detail = TRUE,
    conf_level = conf_level,
    p_value = p_value,
    lower_bound = -1,
    upper_bound = 1,
    .include_se = .include_se,
    digits = digits
  )
}


# ── Summary table ────────────────────────────────────────────────────────────

#' Association measures summary table
#'
#' `assoc_measures()` computes a range of association measures for a
#' two-way contingency table and returns them in a tidy data frame.
#'
#' @param x A contingency table (of class `table`).
#' @param type Which family of measures to compute:
#'   `"all"` (default), `"nominal"`, or `"ordinal"`.
#' @param conf_level A number between 0 and 1 giving the confidence
#'   level (default `0.95`). Set to `NULL` to omit the confidence
#'   interval.
#' @param digits Number of decimal places used when printing the
#'   result (default `3`).
#'
#' @return A data frame with columns `measure`, `estimate`, `se`,
#'   `ci_lower`, `ci_upper`, and `p_value`. For nominal measures
#'   (Cramer's V, Phi, Contingency Coef.), the p-value comes from
#'   the Pearson chi-squared test of independence. For all other
#'   measures, it is a Wald z-test of H0: measure = 0.
#'
#' @details
#' `type = "all"` (the default) returns all nominal and ordinal
#' measures. Use `type = "nominal"` or `type = "ordinal"` to
#' restrict the output to a single family.
#'
#' The nominal family includes [cramer_v()], [contingency_coef()],
#' [lambda_gk()], [goodman_kruskal_tau()], [uncertainty_coef()],
#' and (for 2x2 tables) [phi()] and [yule_q()].
#'
#' The ordinal family includes [gamma_gk()], [kendall_tau_b()],
#' [kendall_tau_c()], and [somers_d()].
#'
#' Standard error formulas follow the DescTools implementations
#' (Signorell et al., 2024).
#'
#' @examples
#' tab <- table(sochealth$smoking, sochealth$education)
#' assoc_measures(tab)
#' assoc_measures(tab, type = "nominal")
#' assoc_measures(tab, type = "ordinal")
#'
#' @references
#' Agresti, A. (2002). *Categorical Data Analysis* (2nd ed.). Wiley.
#'
#' Liebetrau, A. M. (1983). *Measures of Association*. Sage.
#'
#' Signorell, A. et al. (2024). *DescTools: Tools for Descriptive
#' Statistics*. R package.
#'
#' @seealso [cramer_v()], [gamma_gk()], [kendall_tau_b()]
#' @export
assoc_measures <- function(
  x,
  type = c("all", "nominal", "ordinal"),
  conf_level = 0.95,
  digits = 3L
) {
  .validate_table(x)
  type <- match.arg(type)
  is_2x2 <- nrow(x) == 2L && ncol(x) == 2L

  nominal_fns <- list()
  if (is_2x2) {
    nominal_fns[["Phi"]] <- \(t) {
      phi(t, detail = TRUE, conf_level = conf_level, .include_se = TRUE)
    }
    nominal_fns[["Yule's Q"]] <- \(t) {
      yule_q(t, detail = TRUE, conf_level = conf_level, .include_se = TRUE)
    }
  }
  nominal_fns[["Cramer's V"]] <- \(t) {
    cramer_v(t, detail = TRUE, conf_level = conf_level, .include_se = TRUE)
  }
  nominal_fns[["Contingency Coefficient"]] <- \(t) {
    contingency_coef(
      t,
      detail = TRUE,
      conf_level = conf_level,
      .include_se = TRUE
    )
  }
  nominal_fns[["Lambda symmetric"]] <- \(t) {
    lambda_gk(
      t,
      "symmetric",
      detail = TRUE,
      conf_level = conf_level,
      .include_se = TRUE
    )
  }
  nominal_fns[["Lambda R|C"]] <- \(t) {
    lambda_gk(
      t,
      "row",
      detail = TRUE,
      conf_level = conf_level,
      .include_se = TRUE
    )
  }
  nominal_fns[["Lambda C|R"]] <- \(t) {
    lambda_gk(
      t,
      "column",
      detail = TRUE,
      conf_level = conf_level,
      .include_se = TRUE
    )
  }
  nominal_fns[["Goodman-Kruskal's Tau R|C"]] <- \(t) {
    goodman_kruskal_tau(
      t,
      "row",
      detail = TRUE,
      conf_level = conf_level,
      .include_se = TRUE
    )
  }
  nominal_fns[["Goodman-Kruskal's Tau C|R"]] <- \(t) {
    goodman_kruskal_tau(
      t,
      "column",
      detail = TRUE,
      conf_level = conf_level,
      .include_se = TRUE
    )
  }
  nominal_fns[["Uncertainty Coefficient symmetric"]] <- \(t) {
    uncertainty_coef(
      t,
      "symmetric",
      detail = TRUE,
      conf_level = conf_level,
      .include_se = TRUE
    )
  }
  nominal_fns[["Uncertainty Coefficient R|C"]] <- \(t) {
    uncertainty_coef(
      t,
      "row",
      detail = TRUE,
      conf_level = conf_level,
      .include_se = TRUE
    )
  }
  nominal_fns[["Uncertainty Coefficient C|R"]] <- \(t) {
    uncertainty_coef(
      t,
      "column",
      detail = TRUE,
      conf_level = conf_level,
      .include_se = TRUE
    )
  }

  ordinal_fns <- list(
    "Goodman-Kruskal Gamma" = \(t) {
      gamma_gk(t, detail = TRUE, conf_level = conf_level, .include_se = TRUE)
    },
    "Kendall's Tau-b" = \(t) {
      kendall_tau_b(
        t,
        detail = TRUE,
        conf_level = conf_level,
        .include_se = TRUE
      )
    },
    "Kendall's Tau-c" = \(t) {
      kendall_tau_c(
        t,
        detail = TRUE,
        conf_level = conf_level,
        .include_se = TRUE
      )
    },
    "Somers' D R|C" = \(t) {
      somers_d(
        t,
        "row",
        detail = TRUE,
        conf_level = conf_level,
        .include_se = TRUE
      )
    },
    "Somers' D C|R" = \(t) {
      somers_d(
        t,
        "column",
        detail = TRUE,
        conf_level = conf_level,
        .include_se = TRUE
      )
    }
  )

  fns <- switch(type,
    nominal = nominal_fns,
    ordinal = ordinal_fns,
    all = c(nominal_fns, ordinal_fns)
  )

  .extract <- function(res, field) {
    if (field %in% names(res)) unname(res[[field]]) else NA_real_
  }

  rows <- lapply(names(fns), \(nm) {
    res <- tryCatch(
      suppressWarnings(fns[[nm]](x)),
      error = \(e) c(estimate = NA_real_, p_value = NA_real_)
    )
    data.frame(
      measure = nm,
      estimate = .extract(res, "estimate"),
      se = .extract(res, "se"),
      ci_lower = .extract(res, "ci_lower"),
      ci_upper = .extract(res, "ci_upper"),
      p_value = .extract(res, "p_value"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  class(out) <- c("spicy_assoc_table", "data.frame")
  attr(out, "digits") <- digits
  out
}


#' Print an association measures summary table
#'
#' Formats a `spicy_assoc_table` data frame (returned by
#' [assoc_measures()]) with fixed decimal places, aligned columns,
#' and `< 0.001` notation for small p-values.
#'
#' @param x A `spicy_assoc_table` object.
#' @param digits Number of decimal places for estimates, SE, and
#'   confidence intervals. Defaults to 3. The p-value is always
#'   formatted separately (`< 0.001` or three decimal places).
#' @param ... Ignored.
#' @return `x`, invisibly.
#'
#' @seealso [assoc_measures()]
#'
#' @export
print.spicy_assoc_table <- function(
  x,
  digits = attr(x, "digits") %||% 3L,
  ...
) {
  fmt_num <- function(v) {
    ifelse(is.na(v), "--", formatC(v, format = "f", digits = digits))
  }
  fmt_p <- function(v) {
    ifelse(
      is.na(v),
      "--",
      ifelse(v < 0.001, "< 0.001", formatC(v, format = "f", digits = 3))
    )
  }
  cols <- list(
    Measure = format(x$measure, justify = "left"),
    Estimate = fmt_num(x$estimate),
    SE = fmt_num(x$se),
    `CI lower` = fmt_num(x$ci_lower),
    `CI upper` = fmt_num(x$ci_upper),
    p = fmt_p(x$p_value)
  )
  widths <- vapply(
    names(cols),
    \(nm) max(nchar(nm), max(nchar(cols[[nm]]))),
    integer(1)
  )
  hdr <- paste(
    mapply(
      \(nm, w) formatC(nm, width = w, flag = if (nm == "Measure") "-" else ""),
      names(cols),
      widths
    ),
    collapse = "  "
  )
  cat(hdr, "\n")
  for (i in seq_len(nrow(x))) {
    row_str <- paste(
      mapply(
        \(nm, w) {
          v <- cols[[nm]][i]
          formatC(v, width = w, flag = if (nm == "Measure") "-" else "")
        },
        names(cols),
        widths
      ),
      collapse = "  "
    )
    cat(row_str, "\n")
  }
  invisible(x)
}
