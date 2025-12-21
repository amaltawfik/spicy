#' Compute Cramer's V
#'
#' `cramer_v()` computes Cramer's V for a two-way frequency table, measuring the strength of association between two categorical variables.
#'
#' @param x A contingency table (of class `table`) for which to compute the statistic.
#'
#' @return A numeric vector of length 1, representing the Cramer's V statistic.
#'
#' @details
#' Cramer's V is based on the chi-squared statistic and adjusts for the size of the table.
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
    stop("`x` must be a contingency table (class `table`).")
  }
  n <- sum(x)
  chi_squared <- stats::chisq.test(x, correct = FALSE)$statistic
  k <- min(nrow(x), ncol(x)) - 1
  sqrt(as.numeric(chi_squared) / (n * k))
}
