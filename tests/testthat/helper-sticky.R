# A data frame whose `[` keeps its `geometry` column whatever the
# selection, as the `[` of an sf object does. varlist() and code_book()
# must index a plain data frame, or the column comes along unasked, and
# code_book() failed on a subset without columns (sf itself is not a
# dependency of the tests).
cb_sticky <- function() {
  d <- data.frame(a = 1:3)
  d$geometry <- list(1, 2, 3)
  structure(d, class = c("spicy_sticky", "data.frame"))
}

.S3method("[", "spicy_sticky", function(x, i, ...) {
  d <- as.data.frame(x)
  keep <- if (is.character(i)) i else names(d)[i]
  structure(
    d[union(keep, "geometry")],
    class = c("spicy_sticky", "data.frame")
  )
})
