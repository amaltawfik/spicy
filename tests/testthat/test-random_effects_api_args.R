# ---------------------------------------------------------------------------
# Phase 7c7d tests: user-facing API args
#   show_re      -- TRUE / FALSE  (suppress the whole RE panel)
#   re_scale     -- "sd" (default) / "variance"
#   re_columns   -- subset of c("est", "se", "ci"); "est" is mandatory
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_lmer_api <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}


# ---- 1. show_re = FALSE suppresses the panel ----------------------------

test_that("show_re = FALSE removes the Random effects panel from output", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_api()
  out <- capture.output(print(table_regression(fit, show_re = FALSE)))
  combined <- paste(out, collapse = "\n")
  expect_false(grepl("Random effects", combined, fixed = TRUE))
  expect_false(grepl("Subject (Intercept)", combined, fixed = TRUE))
})

test_that("show_re = TRUE (default) keeps the Random effects panel", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_api()
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Random effects", fixed = TRUE)
})

test_that("show_re = FALSE is a no-op on non-mixed fits (no panel anyway)", {
  fit <- lm(mpg ~ wt, data = mtcars)
  out <- capture.output(print(table_regression(fit, show_re = FALSE)))
  combined <- paste(out, collapse = "\n")
  expect_false(grepl("Random effects", combined, fixed = TRUE))
})

test_that("build_random_effects_footer_block_from_frames(show_re=FALSE) -> NULL", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_api()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_random_effects_footer_block_from_frames(
    list(fr),
    show_re = FALSE
  )
  expect_null(out)
})


# ---- 2. re_scale = "variance" shows variance values ---------------------

test_that("re_scale = 'variance' shows variance values, not SD", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_api()
  fr <- as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components
  var_value <- vc$variance[vc$group == "Subject"]
  sd_value <- sqrt(var_value)
  out <- capture.output(print(table_regression(fit, re_scale = "variance")))
  combined <- paste(out, collapse = "\n")
  # Variance value should appear, SD value should not.
  expect_match(combined, sprintf("%.2f", var_value), fixed = TRUE)
  expect_false(grepl(sprintf("\\s%.2f\\s", sd_value), combined))
})

test_that("re_scale = 'sd' (default) shows SD values", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_api()
  fr <- as_regression_frame(fit)
  sd_value <- sqrt(fr$info$random_effects$variance_components$variance[
    fr$info$random_effects$variance_components$group == "Subject"
  ])
  out <- capture.output(print(table_regression(fit)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, sprintf("%.2f", sd_value), fixed = TRUE)
})

test_that("invalid re_scale value triggers match.arg error", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_api()
  expect_error(table_regression(fit, re_scale = "nonsense"))
})


# ---- 3. re_columns subsets the RE-row columns (rows layout) -------------
# In the rows layout the SE / CI columns are shared with the fixed effects;
# re_columns en-dashes them on the RE rows only. A cell "has a number" when it
# shows a value, and is en-dashed / blank otherwise -- tested on the body.

.re_cell_has_num <- function(df, col) {
  r <- df[
    grepl("Subject (Intercept)", df$Variable, fixed = TRUE),
    ,
    drop = FALSE
  ]
  grepl("[0-9]", r[[col]][1])
}

test_that("re_columns = 'est' en-dashes SE and CI on the RE rows", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_api()
  df <- table_regression(
    fit,
    show_columns = c("b", "se", "ci"),
    re_columns = "est"
  )
  se_col <- grep("SE", names(df), value = TRUE)[1]
  ci_col <- grep("CI", names(df), value = TRUE)[1]
  expect_false(.re_cell_has_num(df, se_col))
  expect_false(.re_cell_has_num(df, ci_col))
  # a fixed-effect row keeps its SE (re_columns only touches the RE rows)
  expect_true(grepl("[0-9]", trimws(df[df$Variable == "Days", se_col])))
})

test_that("re_columns = c('est', 'se') keeps SE, en-dashes CI on the RE rows", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_api()
  df <- table_regression(
    fit,
    show_columns = c("b", "se", "ci"),
    re_columns = c("est", "se")
  )
  se_col <- grep("SE", names(df), value = TRUE)[1]
  ci_col <- grep("CI", names(df), value = TRUE)[1]
  expect_true(.re_cell_has_num(df, se_col))
  expect_false(.re_cell_has_num(df, ci_col))
})

test_that("re_columns = c('est', 'ci') keeps CI, en-dashes SE on the RE rows", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_api()
  df <- table_regression(
    fit,
    show_columns = c("b", "se", "ci"),
    re_columns = c("est", "ci")
  )
  se_col <- grep("SE", names(df), value = TRUE)[1]
  ci_col <- grep("CI", names(df), value = TRUE)[1]
  expect_false(.re_cell_has_num(df, se_col))
  expect_true(.re_cell_has_num(df, ci_col))
})

test_that("re_columns default (all three) shows estimate + SE + CI on RE rows", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_api()
  df <- table_regression(fit, show_columns = c("b", "se", "ci"))
  se_col <- grep("SE", names(df), value = TRUE)[1]
  ci_col <- grep("CI", names(df), value = TRUE)[1]
  expect_true(.re_cell_has_num(df, se_col))
  expect_true(.re_cell_has_num(df, ci_col))
})

# Phase 3 matrix – rd-core:re-columns-display-only
test_that("re_columns deselection is display-only: tidy/as_structured keep SE + CI", {
  skip_if_not_installed("numDeriv")
  skip_if_not_installed("broom")
  fit <- .fit_lmer_api()
  out <- table_regression(
    fit,
    show_columns = c("b", "se", "ci"),
    re_columns = "est"
  )
  # Display: en-dash on the RE SE / CI cells (covered above); the
  # analytic accessors still carry the full inferential columns.
  td <- broom::tidy(out)
  vc_rows <- td[td$estimate_type == "vc" & !grepl("Residual", td$term), ]
  expect_gt(nrow(vc_rows), 0L)
  expect_true(all(is.finite(vc_rows$std.error)))
  expect_true(all(is.finite(vc_rows$conf.low)))
  expect_true(all(is.finite(vc_rows$conf.high)))
  s <- as_structured(out)
  b <- s$body
  re_rows <- grep("σ Subject", b$Variable, fixed = TRUE)
  expect_gt(length(re_rows), 0L)
  expect_true(all(is.finite(b$SE[re_rows])))
  ci_ll <- grep("CI: LL$", names(b), value = TRUE)[1]
  ci_ul <- grep("CI: UL$", names(b), value = TRUE)[1]
  expect_true(all(is.finite(b[[ci_ll]][re_rows])))
  expect_true(all(is.finite(b[[ci_ul]][re_rows])))
})


# ---- 4. re_columns must include "est" -----------------------------------

test_that("re_columns must include 'est' (the estimate is mandatory)", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_api()
  expect_error(
    table_regression(fit, re_columns = c("se", "ci")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, re_columns = "se"),
    class = "spicy_invalid_input"
  )
})

test_that("re_columns rejects unknown tokens", {
  skip_if_not_installed("numDeriv")
  fit <- .fit_lmer_api()
  expect_error(table_regression(fit, re_columns = c("est", "pvalue")))
  expect_error(table_regression(fit, re_columns = c("est", "junk")))
})


# ---- 5. Helpers in isolation --------------------------------------------

test_that(".validate_re_columns happy path returns input", {
  expect_identical(
    spicy:::.validate_re_columns(c("est", "se", "ci")),
    c("est", "se", "ci")
  )
  expect_identical(spicy:::.validate_re_columns("est"), "est")
  expect_identical(spicy:::.validate_re_columns(c("est", "se")), c("est", "se"))
})

test_that(".validate_re_columns rejects 'est'-missing input", {
  expect_error(
    spicy:::.validate_re_columns(c("se", "ci")),
    class = "spicy_invalid_input"
  )
})

test_that(".re_components_on_scale('variance') is a no-op", {
  vc <- data.frame(
    group = c("Subject", "Residual"),
    term = c("(Intercept)", ""),
    variance = c(1378.18, 960.46),
    sd = c(37.12, 30.99),
    corr = c(NA_real_, NA_real_),
    std_error = c(13.5, 5.1),
    ci_lower = c(1100, 850),
    ci_upper = c(1700, 1100),
    stringsAsFactors = FALSE
  )
  out <- spicy:::.re_components_on_scale(vc, "variance")
  expect_equal(out$variance, vc$variance)
  expect_equal(out$std_error, vc$std_error)
})


# ---- re_* on a table with no random effects --------------------------------
# dev/decisions/2026-11-19-capability-registry.md, point 5: one
# spicy_ignored_arg warning per table, naming the arguments set away from
# their default, when no model has random-effects rows.

# The spicy_ignored_arg warnings of `expr` about the `re_*` arguments.
.re_noop_warnings <- function(expr) {
  msgs <- character(0)
  withCallingHandlers(
    expr,
    spicy_ignored_arg = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  grep("random effects", msgs, value = TRUE, fixed = TRUE)
}

test_that("a re_* argument on a table without random effects warns once", {
  m <- lm(mpg ~ wt, data = mtcars)
  w <- expect_warning(
    table_regression(m, re_scale = "variance", output = "data.frame"),
    class = "spicy_ignored_arg"
  )
  expect_match(
    conditionMessage(w),
    paste0(
      "`re_scale = \"variance\"` has no effect: no model in the table has ",
      "random effects."
    ),
    fixed = TRUE
  )
  msgs <- .re_noop_warnings(table_regression(
    list(m, lm(mpg ~ wt + hp, data = mtcars)),
    re_scale = "variance",
    re_columns = "est",
    re_test = "lrt",
    re_ci = "profile",
    output = "data.frame"
  ))
  expect_length(msgs, 1L)
  expect_match(
    msgs,
    paste0(
      "`re_scale = \"variance\"`, `re_columns = \"est\"`, ",
      "`re_test = \"lrt\"` and `re_ci = \"profile\"` have no effect"
    ),
    fixed = TRUE
  )
})

test_that("re_* at their defaults, or show_* = FALSE, do not warn", {
  m <- lm(mpg ~ wt, data = mtcars)
  expect_length(
    .re_noop_warnings(table_regression(
      m,
      re_scale = "sd",
      re_columns = c("se", "est", "ci"),
      re_test = "none",
      re_ci = "wald",
      show_re = FALSE,
      show_thresholds = FALSE,
      show_components = FALSE,
      output = "data.frame"
    )),
    0L
  )
})

test_that("one mixed model in the table silences the warning", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  mm <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  m <- lm(Reaction ~ Days, data = lme4::sleepstudy)
  expect_length(
    .re_noop_warnings(table_regression(
      list(m, mm),
      re_scale = "variance",
      output = "data.frame"
    )),
    0L
  )
  expect_length(
    .re_noop_warnings(table_regression(
      list(m, mm),
      re_scale = "variance",
      layout = "stacked",
      output = "data.frame"
    )),
    0L
  )
})

test_that("a stacked table warns once for all its panels", {
  msgs <- .re_noop_warnings(table_regression(
    list(lm(mpg ~ wt, data = mtcars), lm(mpg ~ wt + hp, data = mtcars)),
    re_test = "lrt",
    layout = "stacked",
    output = "data.frame"
  ))
  expect_length(msgs, 1L)
  expect_match(msgs, "`re_test = \"lrt\"` has no effect", fixed = TRUE)
})

test_that("table_regression_uv() passes the warning on", {
  msgs <- .re_noop_warnings(table_regression_uv(
    mtcars,
    outcome = "mpg",
    predictors = c("wt", "hp"),
    method = "lm",
    re_columns = c("est", "ci"),
    output = "data.frame"
  ))
  expect_length(msgs, 1L)
  expect_match(
    msgs,
    "`re_columns = c(\"est\", \"ci\")` has no effect",
    fixed = TRUE
  )
})

test_that("the refusals of re_test and re_ci without show_re still come first", {
  m <- lm(mpg ~ wt, data = mtcars)
  expect_error(
    table_regression(m, re_test = "lrt", show_re = FALSE),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(m, re_ci = "profile", show_re = FALSE),
    class = "spicy_invalid_input"
  )
})

test_that("show_columns = \"none\" names the re_* arguments in its own warning only", {
  msgs <- character(0)
  withCallingHandlers(
    table_regression(
      lm(mpg ~ wt, data = mtcars),
      show_columns = "none",
      re_scale = "variance",
      output = "data.frame"
    ),
    spicy_ignored_arg = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(msgs, 1L)
  expect_match(
    msgs,
    "`show_columns = \"none\"` shows no coefficient",
    fixed = TRUE
  )
})

# The criterion is the structure of each fit: the fixtures whose fit has
# random effects (lme4, glmmTMB, nlme, and clmm and the ordinal glmmTMB
# outside that family) are silent, every other one warns.
re_fixtures <- c(
  "lmer",
  "lmer_ml",
  "glmer",
  "glmer_nb",
  "glmmTMB",
  "glmmTMB_reml",
  "glmmTMB_ordinal",
  "glmmTMB_zi",
  "glmmTMB_hurdle",
  "glmmTMB_disp",
  "glmmTMB_nbinom2",
  "glmmTMB_nbinom1",
  "glmmTMB_beta",
  "lme",
  "lme_contrasts",
  "clmm"
)
for (id in names(.CLASS_FIXTURES)) {
  local({
    id <- id
    test_that(
      paste0("re_scale = \"variance\" warns iff no random effects: ", id),
      {
        skip_on_cran()
        fit <- .class_fixture(id)
        # The other warnings of a fixture (singular fit, sampler) are not
        # the subject here.
        msgs <- suppressWarnings(.re_noop_warnings(suppressMessages(
          table_regression(fit, re_scale = "variance", output = "data.frame")
        )))
        expect_length(msgs, if (id %in% re_fixtures) 0L else 1L)
      }
    )
  })
}
