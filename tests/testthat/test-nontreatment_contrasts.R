# Factors under non-default codings (successive differences,
# sum-to-zero, Helmert, custom matrices) group under their parent
# variable like treatment and polynomial codings do. R names those
# coefficients paste0(var, colnames(contrast_matrix)) -- column index
# when the matrix has no colnames -- and no reference row exists.

.ntc_data <- function() {
  d <- sochealth[stats::complete.cases(
    sochealth[, c("wellbeing_score", "age", "self_rated_health")]
  ), ]
  d$srh <- factor(d$self_rated_health, ordered = FALSE)
  d
}


test_that("successive-difference contrasts group with named suffixes", {
  d <- .ntc_data()
  d$srh_sdif <- d$srh
  contrasts(d$srh_sdif) <- MASS::contr.sdif(4)
  fit <- lm(wellbeing_score ~ age + srh_sdif, data = d)
  td <- broom::tidy(table_regression(fit))
  rows <- td[td$factor_term %in% "srh_sdif", ]
  expect_identical(as.character(rows$factor_level),
                   c("2-1", "3-2", "4-3"))
  out <- paste(capture.output(print(table_regression(fit))),
               collapse = "\n")
  expect_match(out, "srh_sdif:", fixed = TRUE)
  # No reference row under a coding with no reference level.
  expect_false(grepl("(ref.)", out, fixed = TRUE))
})


test_that("unnamed contrast columns fall back to the column index", {
  d <- .ntc_data()
  d$srh_sum <- d$srh
  contrasts(d$srh_sum) <- stats::contr.sum(4)
  fit <- lm(wellbeing_score ~ age + srh_sum, data = d)
  td <- broom::tidy(table_regression(fit))
  rows <- td[td$factor_term %in% "srh_sum", ]
  expect_identical(as.character(rows$factor_level), c("1", "2", "3"))
})


test_that("a string contrast spec resolves through match.fun", {
  d <- .ntc_data()
  fit <- lm(wellbeing_score ~ age + srh, data = d,
            contrasts = list(srh = "contr.helmert"))
  td <- broom::tidy(table_regression(fit))
  rows <- td[td$factor_term %in% "srh", ]
  expect_identical(nrow(rows), 3L)
  expect_identical(as.character(rows$factor_level), c("1", "2", "3"))
})


test_that("treatment and polynomial codings are untouched", {
  d <- .ntc_data()
  fit_t <- lm(wellbeing_score ~ age + srh, data = d)
  td <- broom::tidy(table_regression(fit_t))
  expect_identical(as.character(td$factor_level[td$factor_term %in% "srh"]),
                   c("Fair", "Good", "Very good"))
  fit_p <- lm(wellbeing_score ~ age + self_rated_health,
              data = .ntc_data())
  tp <- broom::tidy(table_regression(fit_p))
  expect_identical(
    as.character(tp$factor_level[tp$factor_term %in% "self_rated_health"]),
    c(".L", ".Q", ".C")
  )
})
