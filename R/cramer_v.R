#' Compute Cramer's V of a two-way frequency table
#'
#' This function computes Cramer's V for a two-way frequency table
#'
#' @param tab table on which to compute the statistic
#'
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' data("starwars")
#' tab <- table(starwars$hair_color, starwars$gender)
#' cramer_v(tab)
#' }

cramer_v <-
  function(tab) {
    n <- sum(tab)
    chid <- stats::chisq.test(tab, correct = FALSE)$statistic
    dim <- min(nrow(tab), ncol(tab)) - 1
    as.numeric(sqrt(chid/(n*dim)))
  }
