# `extras$has_weights` was set by every frame and read by none: the
# weighted count reaches the "Weighted n" row through
# `info$fit_stats$weighted_nobs`, and the variance label through
# `info$weights_kind`. The field is gone; this sweep keeps it gone.

test_that("no frame carries extras$has_weights", {
  expect_false("has_weights" %in% names(default_extras()))
  d <- mtcars
  d$w <- seq(0.5, 2, length.out = nrow(d))
  fits <- list(
    stats::lm(mpg ~ wt, data = d, weights = w),
    stats::glm(am ~ wt, data = d, weights = w, family = quasibinomial())
  )
  for (fit in fits) {
    fr <- as_regression_frame(fit, model_id = "M1")
    expect_false("has_weights" %in% names(fr$info$extras))
    expect_equal(fr$info$fit_stats$weighted_nobs, sum(d$w))
  }
})

for (id in names(.CLASS_FIXTURES)) {
  local({
    id <- id
    test_that(paste0("the frame of ", id, " has no has_weights field"), {
      skip_on_cran()
      fit <- .class_fixture(id)
      fr <- suppressWarnings(suppressMessages(
        as_regression_frame(fit, model_id = "M1")
      ))
      expect_false("has_weights" %in% names(fr$info$extras))
    })
  })
}
