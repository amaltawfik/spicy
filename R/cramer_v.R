#' Compute Cramer's V
#'
#' `cramer_v` computes Cramer's V for a two-way frequency table, which measures the strength of the association between categorical variables.
#'
#' @param tab A table on which to compute the statistic
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
