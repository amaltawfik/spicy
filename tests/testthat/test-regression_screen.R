# The parent class of the bundles that wrap fits of their own
# (`spicy_screen`, R/regression_screen.R), and the shared branches that
# read it: the robust-variance capabilities of a bundle, the intercept of
# a factor block, the outcome of each row of a composite frame.

.screen_soc <- function() {
  d <- spicy::sochealth
  d$smoking <- factor(d$smoking)
  d
}

# A bundle of fits of two classes, the shape a bundle of one model per
# outcome will take: the shared branches must hold for it.
.screen_mixed_bundle <- function() {
  d <- .screen_soc()
  structure(
    list(
      fits = list(
        stats::lm(wellbeing_score ~ age, data = d),
        stats::glm(smoking ~ age, family = stats::binomial(), data = d)
      )
    ),
    class = c("spicy_test_screen", "spicy_screen")
  )
}

test_that("the univariable screen's bundle carries the parent class", {
  d <- .screen_soc()
  frame <- NULL
  local_mocked_bindings(
    table_regression = function(models, ...) {
      frame <<- models$Univariable
      invisible(NULL)
    }
  )
  table_regression_uv(
    d,
    outcome = wellbeing_score,
    predictors = c(age, sex),
    multivariable = FALSE
  )
  expect_identical(class(frame), c("spicy_uv_screen", "spicy_screen"))
})

test_that("a bundle's robust variances are those every fit it wraps accepts", {
  b <- .screen_mixed_bundle()
  lm_vals <- .robust_vcov_support(b$fits[[1L]])
  glm_vals <- .robust_vcov_support(b$fits[[2L]])
  # The two classes differ (CR1S is a value of lm() alone), so the
  # intersection is not the first fit's set.
  expect_false(identical(lm_vals, glm_vals))
  expect_identical(.robust_vcov_support(b), intersect(lm_vals, glm_vals))
  expect_false("CR1S" %in% .robust_vcov_support(b))
  cited <- .vcov_cited(b)
  expect_identical(cited$values, intersect(lm_vals, glm_vals))
  expect_null(cited$others)
})

test_that("the bundle branches read the parent class", {
  b <- .screen_mixed_bundle()
  expect_setequal(
    .spicy_fixed_coef_names(b),
    c("(Intercept)", "age")
  )
  expect_length(.unwrap_screen_bundles(list(b)), 2L)
  expect_type(.reg_model_var_labels(b), "character")
})

test_that("an intercept in a factor block sorts before its reference", {
  coefs_long <- data.frame(
    term = c("y: a", "y: b", "y: (Intercept)"),
    factor_term = c("y", "y", "y"),
    factor_level = c("a", "b", "(Intercept)"),
    factor_level_pos = c(1L, 2L, NA),
    is_reference = c(TRUE, FALSE, FALSE),
    is_intercept = c(FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  expect_identical(
    group_factor_terms(c("y: a", "y: b", "y: (Intercept)"), coefs_long),
    c("y: (Intercept)", "y: a", "y: b")
  )
})

test_that("align_frames() takes the outcome of a row from the frame", {
  fr <- as_regression_frame(stats::lm(mpg ~ wt + hp, data = mtcars))
  # Without the column, every row takes the model's outcome.
  plain <- align_frames(list(fr), model_ids = "M1")
  expect_true(all(plain$coefs_aligned$outcome == "mpg"))
  # With it, each row keeps its own, and a missing one falls back.
  fr$coefs$outcome <- ifelse(fr$coefs$term == "wt", "y1", NA_character_)
  aligned <- align_frames(list(fr), model_ids = "M1")
  out <- aligned$coefs_aligned
  expect_identical(out$outcome[out$term == "wt"], "y1")
  expect_true(all(out$outcome[out$term != "wt"] == "mpg"))
})

test_that("a bundle cites the conditional variances its fits name", {
  skip_if_not_installed("rms")
  # An rms fit kept without x = TRUE, y = TRUE has its cluster-robust
  # variance under a condition: the bundle that wraps it says so.
  b <- structure(
    list(
      fits = list(
        stats::lm(mpg ~ wt, data = mtcars),
        rms::ols(mpg ~ wt, data = mtcars)
      )
    ),
    class = c("spicy_test_screen", "spicy_screen")
  )
  one <- .vcov_cited(b$fits[[2L]])
  expect_match(one$others, "x = TRUE, y = TRUE", fixed = TRUE)
  cited <- .vcov_cited(b)
  expect_identical(cited$others, one$others)
  expect_identical(
    cited$values,
    intersect(.vcov_cited(b$fits[[1L]])$values, one$values)
  )
})

test_that("several unknown arguments of a screen are named together", {
  expect_error(
    table_regression_uv(
      .screen_soc(),
      outcome = wellbeing_score,
      predictors = c(age, sex),
      foo = 1,
      bar = 2
    ),
    "Unknown arguments passed to `table_regression()` through `...`: `foo`, `bar`.",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
})
