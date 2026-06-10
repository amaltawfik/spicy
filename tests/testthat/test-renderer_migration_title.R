# ---------------------------------------------------------------------------
# Phase 0c sub-step C1 tests: build_regression_title_from_frames() must
# produce output byte-identical to the legacy build_regression_title()
# on every fixture combination spicy supports today.
#
# This test file is the byte-equivalence gate for the title renderer
# migration. If any future change to either function breaks parity, the
# test fails immediately and the diff points at the offending case.
# ---------------------------------------------------------------------------


# ---- Fixtures (mirror tests/testthat/test-regression_frame_adapter.R) -----

.fixture_lm_simple <- function() {
  lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
}

.fixture_lm_factor <- function() {
  d <- sochealth
  d$inc <- factor(
    as.character(d$income_group),
    levels = c("Low", "Lower middle", "Upper middle", "High")
  )
  lm(wellbeing_score ~ inc + sex, data = d)
}

.fixture_glm_logistic <- function() {
  d <- sochealth
  d$smoker_bin <- as.integer(d$smoking == "Yes")
  glm(smoker_bin ~ age + sex + physical_activity,
      data = d, family = binomial(link = "logit"))
}

.fixture_glm_poisson <- function() {
  # Toy poisson on a count outcome derived from sochealth.
  d <- sochealth
  d$counter <- as.integer(d$age %% 5)
  glm(counter ~ sex + smoking, data = d, family = poisson(link = "log"))
}

# Build legacy extract + frame for a fit
.legacy <- function(fit, model_id = "M1") {
  extract_lm_phase1(fit = fit, model_id = model_id)
}

.frame_of <- function(fit, model_id = "M1") {
  as_regression_frame(fit, model_id = model_id)
}


# ---- Single-model titles --------------------------------------------------

test_that("single lm: title is byte-identical via both paths", {
  fit <- .fixture_lm_simple()
  expect_identical(
    build_regression_title_from_frames(list(.frame_of(fit))),
    build_regression_title(list(.legacy(fit)))
  )
})

test_that("single glm logistic: title is byte-identical via both paths", {
  fit <- .fixture_glm_logistic()
  expect_identical(
    build_regression_title_from_frames(list(.frame_of(fit))),
    build_regression_title(list(.legacy(fit)))
  )
})

test_that("single glm poisson: title is byte-identical via both paths", {
  fit <- .fixture_glm_poisson()
  expect_identical(
    build_regression_title_from_frames(list(.frame_of(fit))),
    build_regression_title(list(.legacy(fit)))
  )
})


# ---- Multi-model titles ---------------------------------------------------

test_that("two lms with identical DV: title is byte-identical", {
  f1 <- .fixture_lm_simple()
  f2 <- .fixture_lm_factor()
  expect_identical(
    build_regression_title_from_frames(list(.frame_of(f1, "M1"),
                                            .frame_of(f2, "M2"))),
    build_regression_title(list(.legacy(f1, "M1"),
                                .legacy(f2, "M2")))
  )
})

test_that("two lms with different DVs: title is byte-identical", {
  f1 <- lm(wellbeing_score ~ age + sex, data = sochealth)
  f2 <- lm(bmi ~ age + sex, data = sochealth)
  expect_identical(
    build_regression_title_from_frames(list(.frame_of(f1, "M1"),
                                            .frame_of(f2, "M2"))),
    build_regression_title(list(.legacy(f1, "M1"),
                                .legacy(f2, "M2")))
  )
})

test_that("lm + glm: mixed-family title is byte-identical", {
  f1 <- .fixture_lm_simple()
  f2 <- .fixture_glm_logistic()
  expect_identical(
    build_regression_title_from_frames(list(.frame_of(f1, "M1"),
                                            .frame_of(f2, "M2"))),
    build_regression_title(list(.legacy(f1, "M1"),
                                .legacy(f2, "M2")))
  )
})

test_that("two glm logistic: title is byte-identical", {
  f1 <- .fixture_glm_logistic()
  f2 <- glm(am ~ wt + cyl, data = mtcars, family = binomial(link = "logit"))
  expect_identical(
    build_regression_title_from_frames(list(.frame_of(f1, "M1"),
                                            .frame_of(f2, "M2"))),
    build_regression_title(list(.legacy(f1, "M1"),
                                .legacy(f2, "M2")))
  )
})


# ---- Hierarchical (nested = TRUE) titles ----------------------------------

test_that("hierarchical lm: title is byte-identical", {
  f1 <- lm(wellbeing_score ~ age, data = sochealth)
  f2 <- lm(wellbeing_score ~ age + sex, data = sochealth)
  f3 <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
  expect_identical(
    build_regression_title_from_frames(
      list(.frame_of(f1, "M1"), .frame_of(f2, "M2"), .frame_of(f3, "M3")),
      nested = TRUE
    ),
    build_regression_title(
      list(.legacy(f1, "M1"), .legacy(f2, "M2"), .legacy(f3, "M3")),
      nested = TRUE
    )
  )
})

test_that("hierarchical glm logistic: title is byte-identical", {
  d <- sochealth
  d$smoker_bin <- as.integer(d$smoking == "Yes")
  f1 <- glm(smoker_bin ~ age,           data = d, family = binomial(link = "logit"))
  f2 <- glm(smoker_bin ~ age + sex,     data = d, family = binomial(link = "logit"))
  expect_identical(
    build_regression_title_from_frames(
      list(.frame_of(f1, "M1"), .frame_of(f2, "M2")),
      nested = TRUE
    ),
    build_regression_title(
      list(.legacy(f1, "M1"), .legacy(f2, "M2")),
      nested = TRUE
    )
  )
})


# ---- Empty / degenerate inputs --------------------------------------------

test_that("empty input: returns 'Regression' through both paths", {
  expect_identical(
    build_regression_title_from_frames(list()),
    "Regression"
  )
  expect_identical(
    build_regression_title_from_frames(list()),
    build_regression_title(list())
  )
})

test_that("non-list input: returns 'Regression' through both paths", {
  expect_identical(
    build_regression_title_from_frames("not a list"),
    build_regression_title("not a list")
  )
})


# ---- Per-field round-trip checks ------------------------------------------

test_that("frame info$dv matches legacy extract$outcome for every fixture", {
  for (fit in list(.fixture_lm_simple(), .fixture_lm_factor(),
                   .fixture_glm_logistic(), .fixture_glm_poisson())) {
    fr <- .frame_of(fit)
    leg <- .legacy(fit)
    expect_identical(fr$info$dv, leg$outcome,
                     info = paste("class:", class(fit)[1]))
  }
})

test_that("frame info$extras$title_prefix matches legacy extract$title_prefix", {
  for (fit in list(.fixture_lm_simple(), .fixture_lm_factor(),
                   .fixture_glm_logistic(), .fixture_glm_poisson())) {
    fr <- .frame_of(fit)
    leg <- .legacy(fit)
    expect_identical(fr$info$extras$title_prefix, leg$title_prefix,
                     info = paste("class:", class(fit)[1]))
  }
})
