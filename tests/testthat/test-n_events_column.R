# show_columns = "n_events": outcome event counts as "events/N" per
# factor level (reference row included), model totals on continuous
# rows (spec: dev/show_counts_spec.md, validated 2026-07-09; format
# settled by the STROBE item 16 / NEJM "no. of events/total no."
# research recorded there).
#
# Oracle provenance: gtsummary 2.5.1 add_nevent(location = "level")
# on tbl_uvregression(sochealth[, c("smoking","age","bmi","sex")],
# glm/binomial) pins the per-level event counts (sex: Female 131,
# Male 118; age total 249) -- reproduced by the direct table()
# computations below.

.nev_soc <- function() {
  sochealth[, c("smoking", "age", "bmi", "sex", "education")]
}


test_that("events/N cells match direct counts on the estimation sample", {
  d <- .nev_soc()
  fit <- stats::glm(smoking ~ age + sex, family = stats::binomial(), data = d)
  mf <- stats::model.frame(fit)
  ev <- as.integer(mf$smoking == levels(mf$smoking)[2L])
  out <- paste(
    capture.output(print(
      table_regression(fit, show_columns = c("b", "n_events", "ci", "p"))
    )),
    collapse = "\n"
  )

  # Factor levels: events and denominator of THAT level (ref included).
  for (lv in levels(mf$sex)) {
    sel <- mf$sex == lv
    expect_match(out, paste0(sum(ev[sel]), "/", sum(sel)), fixed = TRUE)
  }
  # Continuous row: model totals.
  expect_match(out, paste0(sum(ev), "/", length(ev)), fixed = TRUE)
  # gtsummary 2.5.1 pinned level counts (provenance in header): the
  # age+sex fit keeps all 1175 smoking-complete rows, so the per-level
  # events match the add_nevent run.
  expect_identical(sum(ev[mf$sex == "Female"]), 131L)
  expect_identical(sum(ev[mf$sex == "Male"]), 118L)
  expect_identical(sum(ev), 249L)
})


test_that("the reference row shows its counts, not an en-dash", {
  d <- .nev_soc()
  fit <- stats::glm(smoking ~ sex, family = stats::binomial(), data = d)
  df <- as.data.frame(
    table_regression(fit, show_columns = c("b", "n_events", "p"))
  )
  ref_row <- df[grepl("Female", df$Variable, fixed = TRUE), ]
  ev_col <- names(df)[grepl("Events/N", names(df), fixed = TRUE)]
  expect_identical(length(ev_col), 1L)
  expect_match(ref_row[[ev_col]], "^\\s*131/606\\s*$")
  # The estimate cell keeps the reference en-dash.
  b_col <- names(df)[grepl("^\\s*B\\s*$|OR", names(df))][1]
  expect_match(ref_row[[b_col]], "–")
})


test_that("polynomial-contrast rows carry the model totals", {
  d <- .nev_soc()
  # education is an ordered factor -> .L/.Q contrast rows have no level
  # semantics; gtsummary's add_nevent shows the total there too.
  fit <- stats::glm(smoking ~ education, family = stats::binomial(), data = d)
  mf <- stats::model.frame(fit)
  ev <- as.integer(mf$smoking == levels(mf$smoking)[2L])
  out <- paste(
    capture.output(print(
      table_regression(fit, show_columns = c("b", "n_events"))
    )),
    collapse = "\n"
  )
  expect_match(out, paste0(sum(ev), "/", length(ev)), fixed = TRUE)
})


test_that("numeric 0/1 and logical responses count events correctly", {
  fit01 <- stats::glm(
    am ~ wt + factor(cyl),
    family = stats::binomial(),
    data = mtcars
  )
  out01 <- paste(
    capture.output(print(
      table_regression(fit01, show_columns = c("b", "n_events"))
    )),
    collapse = "\n"
  )
  for (cy in c(4, 6, 8)) {
    sel <- mtcars$cyl == cy
    expect_match(
      out01,
      paste0(sum(mtcars$am[sel]), "/", sum(sel)),
      fixed = TRUE
    )
  }
  d2 <- mtcars
  d2$am_l <- as.logical(d2$am)
  fit_l <- stats::glm(am_l ~ wt, family = stats::binomial(), data = d2)
  out_l <- paste(
    capture.output(print(
      table_regression(fit_l, show_columns = c("b", "n_events"))
    )),
    collapse = "\n"
  )
  expect_match(out_l, "13/32", fixed = TRUE)
})


test_that("the uv screen shows per-fit counts on both column groups", {
  d <- .nev_soc()
  out <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = smoking,
        predictors = c(age, bmi),
        show_columns = c("n", "n_events", "b", "ci", "p")
      )
    )),
    collapse = "\n"
  )
  # Univariable age: its own complete cases (1175); bmi and the
  # multivariable fit: 1163 -- and the events differ accordingly.
  cc_age <- !is.na(d$smoking) & !is.na(d$age)
  cc_bmi <- !is.na(d$smoking) & !is.na(d$bmi)
  ev <- as.integer(d$smoking == levels(d$smoking)[2L])
  expect_match(out, paste0(sum(ev[cc_age]), "/", sum(cc_age)), fixed = TRUE)
  expect_match(
    out,
    paste0(sum(ev[cc_bmi & cc_age]), "/", sum(cc_bmi & cc_age)),
    fixed = TRUE
  )
})


test_that("non-binary outcomes are refused with the model named", {
  expect_error(
    table_regression(
      stats::lm(mpg ~ wt, data = mtcars),
      show_columns = c("b", "n_events")
    ),
    class = "spicy_invalid_input"
  )
  # The gaussian/identity-glm "use lm()" caveat fires before the gate.
  expect_warning(
    expect_error(
      table_regression(
        stats::glm(mpg ~ wt, family = stats::gaussian(), data = mtcars),
        show_columns = c("b", "n_events")
      ),
      class = "spicy_invalid_input"
    ),
    class = "spicy_caveat"
  )
  # Grouped binomial (cbind response): no per-row indicator.
  cb <- data.frame(s = c(10, 20, 15), f2 = c(5, 8, 3), x = c(1, 2, 3))
  fit_cb <- stats::glm(cbind(s, f2) ~ x, family = stats::binomial(), data = cb)
  expect_error(
    table_regression(fit_cb, show_columns = c("b", "n_events")),
    class = "spicy_invalid_input"
  )
  # Multi-model: the offending model is named.
  d <- .nev_soc()
  fit_ok <- stats::glm(smoking ~ age, family = stats::binomial(), data = d)
  err <- tryCatch(
    table_regression(
      list(fit_ok, stats::lm(mpg ~ wt, data = mtcars)),
      show_columns = c("b", "n_events")
    ),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "model 2", fixed = TRUE)
})


test_that("n_events is opt-in: default layouts show no Events/N", {
  d <- .nev_soc()
  fit <- stats::glm(smoking ~ age + sex, family = stats::binomial(), data = d)
  out <- paste(capture.output(print(table_regression(fit))), collapse = "\n")
  expect_false(grepl("Events/N", out, fixed = TRUE))
  out_uv <- paste(
    capture.output(print(
      table_regression_uv(d, outcome = smoking, predictors = c(age, sex))
    )),
    collapse = "\n"
  )
  expect_false(grepl("Events/N", out_uv, fixed = TRUE))
})


test_that("binomial mixed fits count events; gaussian mixed fits refuse", {
  skip_if_not_installed("lme4")
  set.seed(11)
  db <- data.frame(
    g = factor(rep(1:15, each = 12)),
    x = rnorm(180),
    f = factor(sample(c("Lo", "Hi"), 180, TRUE))
  )
  db$y <- rbinom(
    180,
    1,
    stats::plogis(
      0.3 * db$x + 0.5 * (db$f == "Hi") + rep(rnorm(15, 0, 0.6), each = 12)
    )
  )
  fg <- suppressMessages(
    lme4::glmer(y ~ x + f + (1 | g), data = db, family = binomial())
  )
  out <- paste(
    capture.output(print(
      table_regression(fg, show_columns = c("b", "n_events", "p"))
    )),
    collapse = "\n"
  )
  for (lv in levels(db$f)) {
    sel <- db$f == lv
    expect_match(out, paste0(sum(db$y[sel]), "/", sum(sel)), fixed = TRUE)
  }
  expect_match(out, paste0(sum(db$y), "/", nrow(db)), fixed = TRUE)

  fl <- lme4::lmer(x ~ y + (1 | g), data = db)
  expect_error(
    table_regression(fl, show_columns = c("b", "n_events")),
    class = "spicy_invalid_input"
  )

  skip_if_not_installed("glmmTMB")
  ft <- glmmTMB::glmmTMB(
    y ~ x + f + (1 | g),
    data = db,
    family = stats::binomial()
  )
  out2 <- paste(
    capture.output(print(
      table_regression(ft, show_columns = c("b", "n_events", "p"))
    )),
    collapse = "\n"
  )
  sel_hi <- db$f == "Hi"
  expect_match(out2, paste0(sum(db$y[sel_hi]), "/", sum(sel_hi)), fixed = TRUE)
})
