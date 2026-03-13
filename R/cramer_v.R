#' Compute Cramer's V
#'
#' `cramer_v()` computes Cramer's V for a two-way frequency table, measuring the strength of association between two categorical variables.
#'
#' @param x A contingency table (of class `table`) for which to compute the statistic.
#'
#' @return A numeric vector of length 1, representing the Cramer's V statistic.
#'
#' @details
#' Cramer's V is computed as \eqn{V = \sqrt{\chi^2 / (n \cdot (k - 1))}},
#' where \eqn{\chi^2} is the Pearson chi-squared statistic, \eqn{n} is the
#' total number of observations, and \eqn{k = \min(r, c)} with \eqn{r} and
#' \eqn{c} the number of rows and columns.
#' It is suitable for nominal (unordered categorical) variables.
#'
#' @examples
#' # Example with mtcars dataset
#' data(mtcars)
#'
#' # Discretize continuous variables
#' mtcars$gear <- as.factor(mtcars$gear)
#' mtcars$cyl <- as.factor(mtcars$cyl)
#'
#' # Create contingency table
#' tab <- table(mtcars$gear, mtcars$cyl)
#'
#' # Compute Cramer's V
#' cramer_v(tab)
#'
#' @export
cramer_v <- function(x) {
  if (!inherits(x, "table")) {
    stop("`x` must be a contingency table (class `table`).", call. = FALSE)
  }
  n <- sum(x)
  k <- min(nrow(x), ncol(x)) - 1
  if (n <= 0 || k <= 0) {
    warning(
      "Cramer's V is undefined for empty or 1-dimensional tables; returning NA.",
      call. = FALSE
    )
    return(NA_real_)
  }
  chi_squared <- stats::chisq.test(x, correct = FALSE)$statistic
  sqrt(as.numeric(chi_squared) / (n * k))
}
