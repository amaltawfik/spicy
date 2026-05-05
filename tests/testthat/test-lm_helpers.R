# Unit tests for the shared lm-input-resolution helpers in
# `R/lm_helpers.R`. These helpers are designed to be reused by
# future regression-table builders, so their contract is tested
# independently of any specific consumer.

# ---- resolve_covariates_argument ------------------------------------------

test_that("resolve_covariates_argument: NULL returns character(0)", {
  data <- data.frame(a = 1:3, b = 4:6)
  expect_identical(
    spicy:::resolve_covariates_argument(rlang::quo(NULL), data),
    character()
  )
})

test_that("resolve_covariates_argument: tidyselect bare names", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"), bmi = 1:3)
  expect_identical(
    spicy:::resolve_covariates_argument(rlang::quo(c(age, sex)), data),
    c("age", "sex")
  )
})

test_that("resolve_covariates_argument: literal character vector", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  expect_identical(
    spicy:::resolve_covariates_argument(rlang::quo(c("age", "sex")), data),
    c("age", "sex")
  )
})

test_that("resolve_covariates_argument: all_of() selector", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  expect_identical(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::all_of(c("age", "sex"))),
      data
    ),
    c("age", "sex")
  )
})

test_that("resolve_covariates_argument: starts_with() selector", {
  data <- data.frame(bmi_pre = 1:3, bmi_post = 1:3, age = 1:3)
  expect_identical(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::starts_with("bmi")),
      data
    ),
    c("bmi_pre", "bmi_post")
  )
})

test_that("resolve_covariates_argument: where(predicate) selector", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"), w = c(0.5, 1, 1.5))
  expect_identical(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::where(is.numeric)),
      data
    ),
    c("age", "w")
  )
})

test_that("resolve_covariates_argument: empty tidyselect match returns character(0)", {
  data <- data.frame(a = 1:3)
  expect_identical(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::starts_with("zzz_no_match")),
      data
    ),
    character()
  )
})

test_that("resolve_covariates_argument: empty character vector returns character(0)", {
  data <- data.frame(a = 1:3)
  expect_identical(
    spicy:::resolve_covariates_argument(rlang::quo(character()), data),
    character()
  )
})

# ---- supported atomic classes ----------------------------------------------

test_that("resolve_covariates_argument: accepts numeric / integer / logical / factor / character", {
  data <- data.frame(
    n_dbl = c(1.0, 2.0, 3.0),
    n_int = 1:3,
    l     = c(TRUE, FALSE, TRUE),
    f     = factor(c("a", "b", "a")),
    c     = c("x", "y", "z")
  )
  expect_identical(
    spicy:::resolve_covariates_argument(
      rlang::quo(c(n_dbl, n_int, l, f, c)),
      data
    ),
    c("n_dbl", "n_int", "l", "f", "c")
  )
})

# ---- error paths -----------------------------------------------------------

test_that("resolve_covariates_argument: formula syntax rejected (additive)", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(~ age + sex), data),
    class = "spicy_unsupported"
  )
})

test_that("resolve_covariates_argument: formula syntax rejected (interaction)", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(~ age * sex), data),
    class = "spicy_unsupported"
  )
})

test_that("resolve_covariates_argument: formula syntax rejected (transform)", {
  data <- data.frame(age = 1:3)
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(~ I(age^2)), data),
    class = "spicy_unsupported"
  )
})

test_that("resolve_covariates_argument: nonexistent column", {
  data <- data.frame(age = 1:3)
  expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::all_of("nonexistent")),
      data
    ),
    class = "spicy_missing_column"
  )
})

test_that("resolve_covariates_argument: nonexistent column via literal char vec", {
  # The literal-character-vector path bypasses tidyselect's existence
  # check; the explicit re-validation in the helper must catch it.
  data <- data.frame(age = 1:3)
  expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(c("age", "nonexistent")),
      data
    ),
    class = "spicy_missing_column"
  )
})

test_that("resolve_covariates_argument: list-column rejected", {
  data <- data.frame(age = 1:3)
  data$lst <- I(list(1, 2, 3))
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(c(age, lst)), data),
    class = "spicy_invalid_input"
  )
})

test_that("resolve_covariates_argument: complex column rejected", {
  data <- data.frame(age = 1:3)
  data$z <- complex(real = c(1, 2, 3), imaginary = c(0, 1, 0))
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(c(age, z)), data),
    class = "spicy_invalid_input"
  )
})

test_that("resolve_covariates_argument: overlap with `by` rejected", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(c(age, sex)),
      data,
      by_name = "sex"
    ),
    class = "spicy_invalid_input"
  )
})

test_that("resolve_covariates_argument: overlap with `select` is allowed (orchestrator deduplicates)", {
  # By convention the orchestrator silently excludes covariates from
  # the outcome list (mirroring the existing `by` auto-exclusion), so
  # the helper itself does not error on this overlap.
  data <- data.frame(bmi = 1:3, age = 1:3, sex = c("F", "M", "F"))
  expect_identical(
    spicy:::resolve_covariates_argument(
      rlang::quo(c(age, bmi)),
      data,
      select_names = "bmi"
    ),
    c("age", "bmi")
  )
})

test_that("resolve_covariates_argument: where() may overlap with by -> errors", {
  # `where(is.numeric)` would silently grab the predictor if it is
  # numeric; the overlap check forces the user to be explicit.
  data <- data.frame(age = 1:3, weight = c(70, 80, 75))
  expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::where(is.numeric)),
      data,
      by_name = "age"
    ),
    class = "spicy_invalid_input"
  )
})

test_that("resolve_covariates_argument: every error inherits from spicy_error", {
  data <- data.frame(age = 1:3)
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(~ age), data),
    class = "spicy_error"
  )
  expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(tidyselect::all_of("nope")),
      data
    ),
    class = "spicy_error"
  )
})

# ---- table_continuous_lm() end-to-end with covariates ---------------------

test_that("table_continuous_lm: numeric outcome by 2-level factor adjusted for one covariate", {
  set.seed(1L)
  n <- 80
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    age = rnorm(n, 50, 10),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  out <- table_continuous_lm(
    df,
    select = bmi,
    by = sex,
    covariates = age,
    output = "long"
  )
  expect_equal(nrow(out), 2L) # one row per level of sex
  expect_identical(attr(out, "covariates"), "age")
  # Adjusted emmeans must differ from raw group means (covariate
  # shifts the predicted means away from the marginal averages).
  raw_means <- tapply(df$bmi, df$sex, mean)
  expect_false(isTRUE(all.equal(unname(raw_means), out$emmean)))
})

test_that("table_continuous_lm: covariate auto-excluded from `select = everything()`", {
  set.seed(2L)
  n <- 60
  df <- data.frame(
    bmi = rnorm(n, 25),
    age = rnorm(n, 50),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  out <- table_continuous_lm(
    df,
    select = tidyselect::where(is.numeric),
    by = sex,
    covariates = age,
    output = "long"
  )
  # `age` would normally be in select via where(is.numeric); the
  # orchestrator must auto-exclude it because it is a covariate.
  expect_false("age" %in% out$variable)
  expect_true("bmi" %in% out$variable)
})

test_that("table_continuous_lm: numeric `by` adjusted for a categorical covariate", {
  set.seed(3L)
  n <- 90
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    age = rnorm(n, 50, 10),
    group = factor(rep(c("A", "B", "C"), each = n / 3))
  )
  out <- table_continuous_lm(
    df,
    select = bmi,
    by = age,
    covariates = group,
    output = "long"
  )
  expect_equal(nrow(out), 1L) # numeric by -> single slope row
  expect_identical(attr(out, "covariates"), "group")
  # Slope of age changes once we adjust for group: compare to raw lm
  fit <- stats::lm(bmi ~ age + group, data = df)
  expect_equal(out$estimate[1], unname(stats::coef(fit)["age"]))
})

test_that("table_continuous_lm: 3-level `by` with a covariate uses partial F", {
  set.seed(4L)
  n <- 90
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    age = rnorm(n, 50, 10),
    group = factor(rep(c("A", "B", "C"), each = n / 3))
  )
  out <- table_continuous_lm(
    df,
    select = bmi,
    by = group,
    covariates = age,
    output = "long",
    effect_size = "f2"
  )
  # Partial F via drop1 -- compare to an independent computation.
  fit <- stats::lm(bmi ~ group + age, data = df)
  d1 <- stats::drop1(fit, scope = ~group, test = "F")
  partial_f <- d1[["F value"]][2]
  partial_df1 <- d1[["Df"]][2]
  expected_f2 <- partial_f * partial_df1 / stats::df.residual(fit)
  expect_equal(out$es_value[1], expected_f2, tolerance = 1e-9)
})

test_that("table_continuous_lm: effect_size = \"d\" + covariates errors with spicy_unsupported", {
  set.seed(5L)
  n <- 60
  df <- data.frame(
    bmi = rnorm(n, 25),
    age = rnorm(n, 50),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  expect_error(
    table_continuous_lm(
      df,
      select = bmi,
      by = sex,
      covariates = age,
      effect_size = "d"
    ),
    class = "spicy_unsupported"
  )
})

test_that("table_continuous_lm: effect_size = \"g\" + covariates errors with spicy_unsupported", {
  set.seed(6L)
  n <- 60
  df <- data.frame(
    bmi = rnorm(n, 25),
    age = rnorm(n, 50),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  expect_error(
    table_continuous_lm(
      df,
      select = bmi,
      by = sex,
      covariates = age,
      effect_size = "g"
    ),
    class = "spicy_unsupported"
  )
})

test_that("table_continuous_lm: NA in covariate triggers complete-cases drop", {
  set.seed(7L)
  n <- 60
  df <- data.frame(
    bmi = rnorm(n, 25),
    age = c(NA, NA, NA, rnorm(n - 3L, 50)),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  out <- table_continuous_lm(
    df,
    select = bmi,
    by = sex,
    covariates = age,
    output = "long"
  )
  expect_equal(out$n[1], n - 3L)
})

test_that("table_continuous_lm: covariates attribute round-trips through default print path", {
  set.seed(8L)
  n <- 60
  df <- data.frame(
    bmi = rnorm(n, 25),
    age = rnorm(n, 50),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  out <- table_continuous_lm(
    df,
    select = bmi,
    by = sex,
    covariates = age,
    output = "long"
  )
  # The render layer (Commit C) will read these attributes to emit
  # the "Adjusted for: ..." footer with the chosen estimand.
  expect_identical(attr(out, "covariates"), "age")
  expect_identical(attr(out, "adjustment"), "proportional")
})

# ---- adjustment dispatch (proportional vs balanced) -----------------------

test_that("adjustment: NULL covariates -> attr is NA, both methods identical", {
  set.seed(101L)
  n <- 40
  df <- data.frame(
    bmi = rnorm(n, 25),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  out_p <- table_continuous_lm(
    df, select = bmi, by = sex,
    adjustment = "proportional", output = "long"
  )
  out_b <- table_continuous_lm(
    df, select = bmi, by = sex,
    adjustment = "balanced", output = "long"
  )
  expect_identical(attr(out_p, "adjustment"), NA_character_)
  expect_identical(attr(out_b, "adjustment"), NA_character_)
  # No covariates -> emmean = group mean, emmean_method has no effect
  expect_equal(out_p$emmean, out_b$emmean, tolerance = 1e-12)
})

test_that("adjustment: numeric-only covariates -> proportional == balanced", {
  set.seed(102L)
  n <- 80
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    age = rnorm(n, 50, 10),
    weight = rnorm(n, 70, 12),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  out_p <- table_continuous_lm(
    df, select = bmi, by = sex, covariates = c(age, weight),
    adjustment = "proportional", output = "long"
  )
  out_b <- table_continuous_lm(
    df, select = bmi, by = sex, covariates = c(age, weight),
    adjustment = "balanced", output = "long"
  )
  # With purely numeric covariates the two methods coincide because
  # `mean(numeric)` is the same regardless of weighting.
  expect_equal(out_p$emmean, out_b$emmean, tolerance = 1e-12)
  expect_equal(out_p$emmean_se, out_b$emmean_se, tolerance = 1e-12)
})

test_that("adjustment: factor covariate makes proportional and balanced diverge", {
  # Skewed observed proportions (75/25) so equal-weight (1/2 each)
  # differs visibly from proportional (0.75 / 0.25).
  set.seed(103L)
  n <- 120
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    sex = factor(rep(c("F", "M"), each = n / 2)),
    race = factor(c(rep("A", 90L), rep("B", 30L)))
  )
  out_p <- table_continuous_lm(
    df, select = bmi, by = sex, covariates = race,
    adjustment = "proportional", output = "long"
  )
  out_b <- table_continuous_lm(
    df, select = bmi, by = sex, covariates = race,
    adjustment = "balanced", output = "long"
  )
  # The emmeans must differ -- proof the dispatch actually does
  # something different rather than silently no-op.
  expect_false(isTRUE(all.equal(out_p$emmean, out_b$emmean)))
})

test_that("adjustment proportional: matches manual G-computation", {
  set.seed(104L)
  n <- 100
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    sex = factor(rep(c("F", "M"), each = n / 2)),
    race = factor(c(rep("A", 70L), rep("B", 30L)))
  )
  fit <- stats::lm(bmi ~ sex + race, data = df)

  manual <- vapply(levels(df$sex), function(lvl) {
    tmp <- df
    tmp$sex <- factor(lvl, levels = levels(df$sex))
    mean(stats::predict(fit, newdata = tmp))
  }, numeric(1))

  out <- table_continuous_lm(
    df, select = bmi, by = sex, covariates = race,
    adjustment = "proportional", output = "long"
  )
  expect_equal(out$emmean, unname(manual), tolerance = 1e-9)
})

test_that("adjustment balanced: matches manual equal-weight grid average", {
  set.seed(105L)
  n <- 100
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    age = rnorm(n, 50, 10),
    sex = factor(rep(c("F", "M"), each = n / 2)),
    race = factor(c(rep("A", 70L), rep("B", 30L)))
  )
  fit <- stats::lm(bmi ~ sex + race + age, data = df)

  # Manual balanced: cross-product of factor cov levels
  # × numeric covs at sample mean. Race has 2 levels A / B,
  # so the grid for each focal sex is {(A, mean(age)), (B, mean(age))},
  # both with equal weight 1/2.
  manual <- vapply(levels(df$sex), function(lvl) {
    grid <- data.frame(
      sex = factor(lvl, levels = levels(df$sex)),
      race = factor(levels(df$race), levels = levels(df$race)),
      age = mean(df$age)
    )
    mean(stats::predict(fit, newdata = grid))
  }, numeric(1))

  out <- table_continuous_lm(
    df, select = bmi, by = sex, covariates = c(race, age),
    adjustment = "balanced", output = "long"
  )
  expect_equal(out$emmean, unname(manual), tolerance = 1e-9)
})

test_that("adjustment proportional: oracle match against marginaleffects (point estimates)", {
  skip_if_not_installed("marginaleffects")
  set.seed(106L)
  n <- 200
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    age = rnorm(n, 50, 10),
    sex = factor(rep(c("F", "M"), each = n / 2)),
    race = factor(c(rep("A", 140L), rep("B", 60L)))
  )
  fit <- stats::lm(bmi ~ sex + race + age, data = df)

  spicy_out <- table_continuous_lm(
    df, select = bmi, by = sex, covariates = c(race, age),
    adjustment = "proportional", output = "long"
  )

  # G-computation counterfactual: replicate the data once per focal
  # level of `sex`, predict, then average within level. This is what
  # `avg_predictions(variables = "sex")` does (NOT `by = "sex"`,
  # which only groups predictions at observed levels without setting
  # the counterfactual).
  me_out <- as.data.frame(
    marginaleffects::avg_predictions(fit, variables = "sex")
  )
  ord <- match(spicy_out$level, as.character(me_out$sex))
  expect_equal(spicy_out$emmean, me_out$estimate[ord], tolerance = 1e-8)

  # NB on SEs: spicy computes SE analytically via the exact
  # quadratic form `avg_row %*% V %*% avg_row^T`, while
  # marginaleffects derives it via the numerical delta method
  # (`numDeriv`). The two agree to ~1e-5 in practice but the
  # numerical method is less precise, so this test only pins the
  # point estimates. spicy SE correctness is verified analytically
  # in the manual-reference and emmeans-oracle tests below.
})

test_that("adjustment proportional: SE matches manual quadratic form", {
  set.seed(109L)
  n <- 100
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    age = rnorm(n, 50, 10),
    sex = factor(rep(c("F", "M"), each = n / 2)),
    race = factor(c(rep("A", 70L), rep("B", 30L)))
  )

  spicy_out <- table_continuous_lm(
    df, select = bmi, by = sex, covariates = c(race, age),
    adjustment = "proportional", output = "long"
  )

  # Manual SE: the linear contrast that defines the proportional
  # emmean for each focal level is `avg_row = colMeans(design)` of
  # the design matrix built with `sex` set to the focal level for
  # all observed rows. SE = sqrt(avg_row %*% vcov(fit) %*% avg_row^T).
  fit <- stats::lm(bmi ~ sex + race + age, data = df)
  vc <- stats::vcov(fit)
  manual_se <- vapply(levels(df$sex), function(lvl) {
    nd <- df
    nd$sex <- factor(lvl, levels = levels(df$sex))
    X <- stats::model.matrix(stats::delete.response(stats::terms(fit)), nd)
    avg <- colMeans(X)
    sqrt(sum((avg %*% vc) * avg))
  }, numeric(1))
  expect_equal(spicy_out$emmean_se, unname(manual_se), tolerance = 1e-12)
})

test_that("adjustment balanced: oracle match against emmeans::emmeans default", {
  skip_if_not_installed("emmeans")
  set.seed(107L)
  n <- 200
  df <- data.frame(
    bmi = rnorm(n, 25, 4),
    age = rnorm(n, 50, 10),
    sex = factor(rep(c("F", "M"), each = n / 2)),
    race = factor(c(rep("A", 140L), rep("B", 60L)))
  )
  fit <- stats::lm(bmi ~ sex + race + age, data = df)

  spicy_out <- table_continuous_lm(
    df, select = bmi, by = sex, covariates = c(race, age),
    adjustment = "balanced", output = "long"
  )
  emm_out <- as.data.frame(emmeans::emmeans(fit, ~ sex))
  ord <- match(spicy_out$level, as.character(emm_out$sex))
  expect_equal(spicy_out$emmean, emm_out$emmean[ord], tolerance = 1e-8)
  expect_equal(spicy_out$emmean_se, emm_out$SE[ord], tolerance = 1e-8)
})

test_that("print method emits Adjusted for footer iff covariates non-empty", {
  set.seed(201L)
  df <- data.frame(
    score = rnorm(40, 50, 5),
    age   = rnorm(40, 30, 5),
    sex   = factor(rep(c("F", "M"), each = 20L))
  )

  # Without covariates: NO "Adjusted for" line.
  out_unadj <- table_continuous_lm(df, select = "score", by = sex)
  txt_unadj <- paste(
    capture.output(print(out_unadj)),
    collapse = "\n"
  )
  expect_false(grepl("Adjusted for", txt_unadj, fixed = TRUE))

  # With covariate: footer present and names the cov + method.
  out_p <- table_continuous_lm(
    df, select = "score", by = sex,
    covariates = age, adjustment = "proportional"
  )
  txt_p <- paste(capture.output(print(out_p)), collapse = "\n")
  expect_true(grepl("Adjusted for age", txt_p, fixed = TRUE))
  expect_true(grepl("(proportional)", txt_p, fixed = TRUE))

  out_b <- table_continuous_lm(
    df, select = "score", by = sex,
    covariates = age, adjustment = "balanced"
  )
  txt_b <- paste(capture.output(print(out_b)), collapse = "\n")
  expect_true(grepl("Adjusted for age", txt_b, fixed = TRUE))
  expect_true(grepl("(balanced)", txt_b, fixed = TRUE))
})

test_that("print footer lists multiple covariates separated by commas", {
  set.seed(202L)
  df <- data.frame(
    score = rnorm(40, 50, 5),
    age   = rnorm(40, 30, 5),
    weight = rnorm(40, 70, 10),
    sex   = factor(rep(c("F", "M"), each = 20L))
  )
  out <- table_continuous_lm(
    df, select = "score", by = sex,
    covariates = c(age, weight)
  )
  txt <- paste(capture.output(print(out)), collapse = "\n")
  expect_true(grepl("Adjusted for age, weight", txt, fixed = TRUE))
})

test_that("adjustment: invalid value rejected by match.arg", {
  set.seed(108L)
  n <- 40
  df <- data.frame(
    bmi = rnorm(n, 25),
    sex = factor(rep(c("F", "M"), each = n / 2))
  )
  # `match.arg()` is base R; its error message is "'arg' should be
  # one of ...". This test confirms the rejection path activates.
  expect_error(
    table_continuous_lm(
      df, select = bmi, by = sex,
      adjustment = "bogus"
    ),
    "should be one of"
  )
})
