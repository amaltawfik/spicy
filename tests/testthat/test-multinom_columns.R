# ---------------------------------------------------------------------------
# Multinomial outcome-as-columns layout (dev/multinom_outcome_columns_spec.md,
# user-validated 2026-07-04): a single nnet::multinom model renders
# predictors as rows and one column group per non-reference outcome
# category. Display-only -- tidy() / output = "long" keep the long form.
# ---------------------------------------------------------------------------


.fit_multinom_soc <- function(formula = employment_status ~ age + sex) {
  skip_if_not_installed("nnet")
  # do.call() embeds the formula VALUE in the stored call: helpers that
  # pass a `formula` variable otherwise break anova.multinom() later
  # (the symbol re-evaluates to base::formula in another env).
  do.call(nnet::multinom,
          list(formula, data = quote(sochealth), trace = FALSE))
}


# ---- 1. Layout: one column set per category, bare predictor rows --------

test_that("single multinom renders outcome categories as column groups", {
  fit <- .fit_multinom_soc()
  df <- as.data.frame(table_regression(fit))
  # One B/SE/p column set per non-reference category, prefixed by the
  # category name.
  for (cat in c("Student", "Unemployed", "Inactive")) {
    for (col in c("B", "SE", "p")) {
      expect_true(paste0(cat, ": ", col) %in% names(df))
    }
  }
  # No column group for the reference category (no AME requested).
  expect_false(any(grepl("^Employed: ", names(df))))
  # Rows are bare predictors: no "<category>: <term>" row labels left.
  expect_true("age" %in% df$Variable)
  expect_false(any(grepl("Student: ", df$Variable, fixed = TRUE)))
  # Factor rows keep the reference-row convention, unprefixed.
  expect_true(any(grepl("Female (ref.)", df$Variable, fixed = TRUE)))
})


# ---- 2. Numbers: per-category cells equal summary(fit) ------------------

test_that("columns-layout cells match the summary(fit) oracle", {
  fit <- .fit_multinom_soc()
  df <- as.data.frame(table_regression(fit))
  b  <- summary(fit)$coefficients
  se <- summary(fit)$standard.errors
  z  <- b / se
  p  <- 2 * pnorm(-abs(z))
  # trimws(): cells carry decimal-alignment padding.
  for (cat in c("Student", "Unemployed", "Inactive")) {
    expect_identical(trimws(df[df$Variable == "age", paste0(cat, ": B")]),
                     formatC(b[cat, "age"], format = "f", digits = 2))
    expect_identical(trimws(df[df$Variable == "age", paste0(cat, ": SE")]),
                     formatC(se[cat, "age"], format = "f", digits = 2))
    expect_identical(
      trimws(df[df$Variable == "age", paste0(cat, ": p")]),
      sub("^0", "", formatC(p[cat, "age"], format = "f", digits = 3))
    )
  }
})


# ---- 3. Default compaction: B / SE / p, CIs restorable ------------------

test_that("columns layout compacts to B/SE/p; atomic tokens restore CI", {
  fit <- .fit_multinom_soc()
  df <- as.data.frame(table_regression(fit))
  expect_false(any(grepl("CI", names(df))))
  df_ci <- as.data.frame(table_regression(fit,
                                          show_columns = c("b", "ci", "p")))
  expect_true(any(grepl("95% CI", names(df_ci), fixed = TRUE)))
  # The explicit group token auto-compacts exactly like multi-model
  # tables (documented parity).
  df_grp <- as.data.frame(table_regression(fit, show_columns = "all_b"))
  expect_false(any(grepl("CI", names(df_grp))))
})


# ---- 4. Reference-outcome footer note, both layouts ---------------------

test_that("'Reference outcome:' note prints in columns AND rows layouts", {
  fit <- .fit_multinom_soc()
  out1 <- paste(capture.output(print(table_regression(fit))),
                collapse = "\n")
  expect_match(out1, "Reference outcome: Employed.", fixed = TRUE)
  # Multi-model fallback (rows layout) keeps the note, deduped to one
  # shared line when every model has the same reference.
  fit0 <- .fit_multinom_soc(employment_status ~ age)
  out2 <- capture.output(print(table_regression(list(fit0, fit))))
  hits <- grep("Reference outcome: Employed.", out2, fixed = TRUE)
  expect_identical(length(hits), 1L)
})


# ---- 5. outcome_labels relabels the category spanners -------------------

test_that("outcome_labels overrides the category spanners", {
  fit <- .fit_multinom_soc()
  tr <- table_regression(
    fit,
    outcome_labels = c("Student vs Employed", "Unemployed vs Employed",
                       "Inactive vs Employed")
  )
  # data.frame colnames carry the full labels (the console spanner can
  # truncate to the group's column width).
  df <- as.data.frame(tr)
  expect_true("Inactive vs Employed: B" %in% names(df))
  out <- paste(capture.output(print(tr)), collapse = "\n")
  expect_match(out, "Student vs Employed", fixed = TRUE)
})

test_that("outcome_labels validates length against non-ref categories", {
  fit <- .fit_multinom_soc()
  expect_error(table_regression(fit, outcome_labels = c("a", "b")),
               class = "spicy_invalid_input")
  expect_error(table_regression(fit, outcome_labels = 1:3),
               class = "spicy_invalid_input")
})

test_that("model_labels is refused for a single multinom", {
  fit <- .fit_multinom_soc()
  expect_error(table_regression(fit, model_labels = "M1"),
               class = "spicy_invalid_input")
})


# ---- 6. AME: reference category appears as a last, AME-only group -------

test_that("per-category AME adds the reference group last, AME-only", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_multinom_soc()
  df <- as.data.frame(table_regression(fit, show_columns = c("b", "ame")))
  expect_true("Employed: AME" %in% names(df))
  # Reference group comes LAST and has empty coefficient cells.
  emp_cols <- grep("^Employed: ", names(df))
  expect_identical(max(grep(": ", names(df), fixed = TRUE)), max(emp_cols))
  age_b_ref <- df[df$Variable == "age", "Employed: B"]
  expect_false(grepl("[0-9]", age_b_ref))
  # AMEs on response-category probabilities sum to ~0 across the
  # categories for a given predictor.
  ame_cols <- grep(": AME$", names(df), value = TRUE)
  expect_identical(length(ame_cols), 4L)
  tr_long <- attr(table_regression(fit, show_columns = c("b", "ame")),
                  "spicy_long")
  ame_age <- tr_long[tr_long$estimate_type == "ame" &
                       grepl(": age$", tr_long$term), "estimate"]
  expect_identical(length(ame_age), 4L)
  expect_lt(abs(sum(ame_age)), 1e-8)
})


# ---- 7. Multi-model and nested keep the rows layout ----------------------

test_that("multi-model and nested multinom keep the rows layout", {
  fit  <- .fit_multinom_soc()
  fit0 <- .fit_multinom_soc(employment_status ~ age)
  df_m <- as.data.frame(table_regression(list(fit0, fit)))
  expect_true(any(grepl("Student: ", df_m$Variable, fixed = TRUE)))
  df_n <- as.data.frame(table_regression(list(fit0, fit), nested = TRUE))
  expect_true(any(grepl("Student: ", df_n$Variable, fixed = TRUE)))
})


# ---- 8. tidy() / output = "long" unchanged (display-only feature) --------

test_that("tidy() and output='long' keep the long prefixed form", {
  fit <- .fit_multinom_soc()
  tr <- table_regression(fit)
  long <- attr(tr, "spicy_long")
  expect_true(all(grepl("^(Student|Unemployed|Inactive): ",
                        long$term[long$term != "(Intercept)" &
                                    !long$is_intercept])))
  lg <- table_regression(fit, output = "long")
  expect_true(any(grepl("Student: ", lg$term, fixed = TRUE)))
  # keep/drop reaches the long payload identically in both layouts.
  lg_keep <- table_regression(fit, output = "long", keep = "age")
  expect_true(all(grepl("age", lg_keep$term)))
})


# ---- 9. Binary (2-level) multinom degenerates cleanly --------------------

test_that("binary multinom renders a single-group table", {
  skip_if_not_installed("nnet")
  d <- sochealth
  d$working <- factor(ifelse(d$employment_status == "Employed",
                             "Employed", "Not employed"))
  fit <- nnet::multinom(working ~ age + sex, data = d, trace = FALSE)
  df <- as.data.frame(table_regression(fit))
  # The single non-reference category still names its column group --
  # that's what tells the reader the contrast direction.
  expect_true(all(paste0("Not employed: ", c("B", "SE", "p"))
                  %in% names(df)))
  out <- paste(capture.output(print(table_regression(fit))),
               collapse = "\n")
  expect_match(out, "Reference outcome: Employed.", fixed = TRUE)
})


# ---- 10. Console snapshot -------------------------------------------------

test_that("columns-layout console render is stable", {
  fit <- .fit_multinom_soc()
  expect_snapshot(print(table_regression(fit)))
})


# ---- Internals: spanner helper edge cases ---------------------------------

test_that(".multinom_columns_spanners covers ref-present ordering", {
  # Reference present (AME case): override labels the non-ref groups,
  # the reference keeps its own name.
  sp <- .multinom_columns_spanners(
    c("Student", "Unemployed", "Employed"), "Employed",
    c("S vs E", "U vs E")
  )
  expect_identical(sp, c("S vs E", "U vs E", "Employed"))
  expect_identical(
    .multinom_columns_spanners(c("Student"), "Employed", NULL),
    "Student"
  )
})

test_that(".multinom_columns_active gates exactly", {
  skip_if_not_installed("nnet")
  fit <- .fit_multinom_soc()
  expect_true(.multinom_columns_active(fit, nested = FALSE))
  expect_true(.multinom_columns_active(list(fit), nested = FALSE))
  expect_false(.multinom_columns_active(fit, nested = TRUE))
  expect_false(.multinom_columns_active(list(fit, fit), nested = FALSE))
  expect_false(.multinom_columns_active(lm(mpg ~ wt, mtcars),
                                        nested = FALSE))
})

# ---- Adversarial-review fixes (2026-07-04 second batch) -------------------

test_that("keep/drop and show_intercept govern display AND long payload", {
  fit <- .fit_multinom_soc()
  # Anchored regex on the BARE term: pre-fix, the display kept the age
  # rows while tidy()/long returned 0 rows (two term namespaces).
  tr <- table_regression(fit, keep = "^age$")
  df <- as.data.frame(tr)
  expect_true("age" %in% df$Variable)
  long <- attr(tr, "spicy_long")
  expect_identical(sum(grepl(": age$", long$term)), 3L)
  lg <- table_regression(fit, output = "long", keep = "^age$")
  expect_identical(sum(grepl(": age$", lg$term)), 3L)
  # A prefixed-namespace regex is a no-op on BOTH views (bare terms
  # govern), not just on the display.
  lg2 <- table_regression(fit, output = "long", drop = "^Student: ")
  expect_true(any(grepl("^Student: ", lg2$term)))
  # show_intercept = FALSE reaches the long payload too.
  lg3 <- table_regression(fit, output = "long", show_intercept = FALSE)
  expect_false(any(grepl("(Intercept)", lg3$term, fixed = TRUE)))
})

test_that("reference_style = 'footer' lists bare, deduped levels", {
  fit <- .fit_multinom_soc()
  out <- paste(
    capture.output(print(table_regression(fit,
                                          reference_style = "footer"))),
    collapse = "\n"
  )
  expect_match(out, "sex = Female", fixed = TRUE)
  expect_false(grepl("Student: Female", out, fixed = TRUE))
  # One entry per factor, not one per category equation.
  expect_identical(
    lengths(regmatches(out, gregexpr("sex = Female", out, fixed = TRUE))),
    1L
  )
})

test_that("outcome_labels = FALSE is a no-op; duplicates are refused", {
  fit <- .fit_multinom_soc()
  df <- as.data.frame(table_regression(fit, outcome_labels = FALSE))
  expect_true("Student: B" %in% names(df))
  expect_error(
    table_regression(fit,
                     outcome_labels = c("Same", "Same", "Other")),
    class = "spicy_invalid_input"
  )
})

test_that("matrix-response multinom renders (response_levels fallback)", {
  skip_if_not_installed("nnet")
  set.seed(7)
  n <- 150
  x <- rnorm(n)
  counts <- t(vapply(x, function(xi) {
    p <- exp(c(0, 0.4 * xi, -0.3 * xi)); as.vector(rmultinom(1, 5, p))
  }, numeric(3)))
  colnames(counts) <- c("a", "b", "c")
  fitm <- nnet::multinom(counts ~ x, trace = FALSE)
  df <- as.data.frame(table_regression(fitm))
  # Pre-fix this rendered a silently EMPTY table (response_levels =
  # character(0) defeated the %||% fallback).
  expect_true(any(grepl("[0-9]", unlist(df[df$Variable == "x", -1L]))))
})

test_that("mixed-class multi-model qualifies the reference-outcome note", {
  skip_if_not_installed("nnet")
  d <- sochealth
  d$emp <- as.integer(d$employment_status == "Employed")
  fg <- glm(emp ~ age, data = d, family = binomial())
  fm <- nnet::multinom(employment_status ~ age, data = d, trace = FALSE)
  out <- paste(capture.output(print(table_regression(list(fg, fm)))),
               collapse = "\n")
  expect_match(out, "Model 2: Reference outcome: Employed.", fixed = TRUE)
  expect_false(grepl("\nReference outcome: Employed.", out, fixed = TRUE))
})

test_that("ordered-factor predictor gets the polynomial footer note", {
  fit <- .fit_multinom_soc(employment_status ~ age + education)
  out <- paste(capture.output(print(table_regression(fit))),
               collapse = "\n")
  # education is an ordered factor in sochealth: the .L/.Q rows must
  # carry the polynomial-trends note (detection was defeated by the
  # outcome prefix when the footer read the original frames).
  expect_match(out, "polynomial", ignore.case = TRUE)
})

test_that("differing reference outcomes get per-model footer lines", {
  skip_if_not_installed("nnet")
  d <- sochealth
  d$es2 <- stats::relevel(d$employment_status, ref = "Student")
  fit1 <- nnet::multinom(employment_status ~ age, data = d, trace = FALSE)
  fit2 <- nnet::multinom(es2 ~ age, data = d, trace = FALSE)
  out <- paste(capture.output(print(table_regression(list(fit1, fit2)))),
               collapse = "\n")
  expect_match(out, "Model 1: Reference outcome: Employed.", fixed = TRUE)
  expect_match(out, "Model 2: Reference outcome: Student.", fixed = TRUE)
})
