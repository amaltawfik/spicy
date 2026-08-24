# ---------------------------------------------------------------------------
# The random-effects block of a NESTED nlme::lme fit.
#
# nlme::VarCorr.lme() flattens every level of a nested fit into one
# character matrix and marks each block with a "<group> =" header row.
# Reading the group name from fit$dims$ngrps[1] instead -- which runs
# innermost-first, the reverse of VarCorr's block order -- labelled both
# levels of `random = ~ 1 | Dog/Side` as "Side", sent both intervals()
# lookups into the same reStruct block (so the two levels shared one SE
# and one CI), and left the renderer with two rows under one identical
# (group, term) key, of which it displayed one.
#
# The levels are then labelled lme4's way ("Side:Dog", not "Side"), in
# lme4's order, so the same nested structure fitted by lme() and lmer()
# produces the same RE keys and the same N rows instead of two disjoint
# sets rendering as holes. It is also the more honest label: nlme's bare
# "Side" has 20 units on Pixel, which are Dog-by-Side combinations.
#
# Oracle: nlme::VarCorr(fit) for the point estimates and
# nlme::intervals(fit) for the CIs -- the engine's own output, read
# directly.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_lme_nested <- function() {
  skip_if_not_installed("nlme")
  # Pixel: 102 CT scans of 10 dogs, both sides of each dog scanned over
  # several days -- Side is nested in Dog, which is what the design is.
  nlme::lme(pixel ~ day, data = nlme::Pixel, random = ~ 1 | Dog / Side)
}

.fit_lme_nested_slope <- function() {
  skip_if_not_installed("nlme")
  # A random slope at the OUTER level only, written as a list rather than
  # a slash: nlme normalises consecutive list entries to the same nesting
  # (getGroupsFormula() gives ~ Dog/Side), so the labels must follow the
  # FITTED structure, not the shape of the call. Also exercises the
  # per-group correlation walk -- Dog carries a cor() row, Side does not.
  nlme::lme(
    pixel ~ day,
    data = nlme::Pixel,
    random = list(Dog = ~day, Side = ~1)
  )
}

.pixel_three_level <- function() {
  d <- nlme::Pixel
  d$half <- factor(ifelse(d$day > stats::median(d$day), "late", "early"))
  d
}

.fit_lme_single <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(distance ~ age, data = nlme::Orthodont, random = ~ age | Subject)
}


# ---- 1. Each level is labelled with its OWN grouping factor -------------

test_that("a nested lme labels each variance component with its own level", {
  fit <- .fit_lme_nested()
  vc <- spicy:::as_regression_frame(fit)$info$random_effects$variance_components

  # Innermost level first, lme4's order and lme4's composite spelling.
  expect_identical(vc$group, c("Side:Dog", "Dog", "Residual"))
  expect_identical(vc$term, c("(Intercept)", "(Intercept)", ""))

  # Oracle: nlme::VarCorr() itself, block by block. Its Variance column
  # is character, so the numbers are parsed the way the frame parses
  # them. VarCorr runs outermost-first, so it is reversed to match.
  raw <- unclass(nlme::VarCorr(fit))
  keep <- !is.na(suppressWarnings(as.numeric(raw[, "Variance"])))
  oracle_var <- unname(suppressWarnings(as.numeric(raw[keep, "Variance"])))
  oracle_sd <- unname(suppressWarnings(as.numeric(raw[keep, "StdDev"])))
  # c(Dog, Side, Residual) -> c(Side, Dog, Residual)
  reorder <- c(2L, 1L, 3L)
  expect_equal(vc$variance, oracle_var[reorder], tolerance = 1e-8)
  expect_equal(vc$sd, oracle_sd[reorder], tolerance = 1e-8)
})


# ---- 2. The two levels no longer share one SE and one CI ----------------

test_that("a nested lme gives each level its own Wald SE and CI", {
  fit <- .fit_lme_nested()
  vc <- spicy:::as_regression_frame(fit)$info$random_effects$variance_components

  # The defect: identical std_error / ci_lower / ci_upper on the two
  # random-effect rows, because both read the same intervals() block.
  expect_false(isTRUE(all.equal(vc$std_error[1L], vc$std_error[2L])))
  expect_false(isTRUE(all.equal(vc$ci_lower[1L], vc$ci_lower[2L])))
  expect_false(isTRUE(all.equal(vc$ci_upper[1L], vc$ci_upper[2L])))

  # Oracle: intervals() bounds on the SD scale, squared to the variance
  # scale the frame stores, and the Delta-method SE derived from the
  # same half-width (SE(sd^2) = 2 * sd * SE(sd)). intervals() is keyed on
  # nlme's BARE name, which is what the composite label maps back to.
  iv <- nlme::intervals(fit, which = "var-cov")
  bare <- c("Side:Dog" = "Side", "Dog" = "Dog")
  z <- stats::qnorm(0.975)
  for (k in seq_len(2L)) {
    row <- iv$reStruct[[bare[[vc$group[k]]]]]["sd((Intercept))", ]
    expect_equal(vc$ci_lower[k], unname(row[["lower"]])^2, tolerance = 1e-8)
    expect_equal(vc$ci_upper[k], unname(row[["upper"]])^2, tolerance = 1e-8)
    se_sd <- (row[["upper"]] - row[["lower"]]) / (2 * z)
    expect_equal(
      vc$std_error[k],
      unname(2 * row[["est."]] * se_sd),
      tolerance = 1e-8
    )
  }

  # And the residual keeps reading intervals()$sigma, not a reStruct block.
  expect_equal(vc$ci_lower[3L], unname(iv$sigma[["lower"]])^2, tolerance = 1e-8)
  expect_equal(vc$ci_upper[3L], unname(iv$sigma[["upper"]])^2, tolerance = 1e-8)
})


# ---- 3. The rendered table shows both levels, and both group counts -----

test_that("a nested lme renders one RE row and one N row per level", {
  fit <- .fit_lme_nested()
  out <- paste(
    capture.output(print(suppressWarnings(table_regression(fit)))),
    collapse = "\n"
  )
  # Both levels are present as rows -- the collapsed-key defect showed
  # exactly one sigma row for a two-level fit.
  expect_match(out, "\u03C3 Side:Dog (Intercept)", fixed = TRUE)
  expect_match(out, "\u03C3 Dog (Intercept)", fixed = TRUE)
  expect_match(out, "\u03C3 (Residual)", fixed = TRUE)

  # `n_groups` promises one "N (<factor>)" row per grouping factor, and
  # the composite label says what the 20 units are: Dog-by-Side
  # combinations, not 20 sides.
  expect_match(out, "N (Side:Dog)", fixed = TRUE)
  expect_match(out, "N (Dog)", fixed = TRUE)

  ng <- spicy:::as_regression_frame(fit)$info$n_groups
  expect_identical(names(ng), c("Side:Dog", "Dog"))
  expect_identical(unname(ng), c(20L, 10L))
})


# ---- 4. Three levels: every non-outermost level composes ----------------

test_that("a three-level lme composes each level onto its ancestors", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    pixel ~ day,
    data = .pixel_three_level(),
    random = ~ 1 | Dog / Side / half
  )
  fr <- spicy:::as_regression_frame(fit)
  expect_identical(
    fr$info$random_effects$variance_components$group,
    c("half:Side:Dog", "Side:Dog", "Dog", "Residual")
  )
  expect_identical(
    names(fr$info$n_groups),
    c("half:Side:Dog", "Side:Dog", "Dog")
  )
  expect_identical(unname(fr$info$n_groups), c(40L, 20L, 10L))
})


# ---- 5. A correlation belongs to the level that carries it --------------

test_that("a nested lme attributes each correlation row to its own level", {
  fit <- .fit_lme_nested_slope()
  vc <- spicy:::as_regression_frame(fit)$info$random_effects$variance_components

  is_cor <- vc$is_correlation %in% TRUE
  expect_identical(sum(is_cor), 1L)
  expect_identical(vc$group[is_cor], "Dog")
  expect_identical(vc$term[is_cor], "(Intercept), day")

  iv <- nlme::intervals(fit, which = "var-cov")
  oracle <- iv$reStruct$Dog["cor((Intercept),day)", ]
  expect_equal(vc$corr[is_cor], unname(oracle[["est."]]), tolerance = 1e-8)
  expect_equal(vc$ci_lower[is_cor], unname(oracle[["lower"]]), tolerance = 1e-8)
  expect_equal(vc$ci_upper[is_cor], unname(oracle[["upper"]]), tolerance = 1e-8)

  # The list() spec is nesting: Side is labelled composite too, and each
  # block keeps its own rows together (Dog's correlation with Dog).
  expect_identical(
    vc$group,
    c("Side:Dog", "Dog", "Dog", "Dog", "Residual")
  )
  expect_identical(
    vc$term,
    c("(Intercept)", "(Intercept)", "day", "(Intercept), day", "")
  )
})


# ---- 6. Cross-engine: lme and lmer agree on the keys and the labels -----

test_that("lme and lmer give the same RE keys for the same nested structure", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("lme4")
  d <- nlme::Pixel
  fit_lme <- nlme::lme(pixel ~ day, data = d, random = ~ 1 | Dog / Side)
  fit_lmer <- lme4::lmer(pixel ~ day + (1 | Dog / Side), data = d)
  a <- spicy:::as_regression_frame(fit_lme)
  b <- spicy:::as_regression_frame(fit_lmer)
  va <- a$info$random_effects$variance_components
  vb <- b$info$random_effects$variance_components

  # Only the KEYS and the labels: the two engines' REML details give
  # slightly different SEs, which is not what this witness is about.
  expect_identical(va$group, vb$group)
  expect_identical(va$term, vb$term)
  # The RE rows are keyed re::<group>::<term>, so equal keys are what
  # makes a multi-model table align instead of rendering holes.
  key <- function(v) paste0("re::", v$group, "::", v$term)
  expect_identical(key(va), key(vb))
  expect_identical(names(a$info$n_groups), names(b$info$n_groups))
  expect_identical(unname(a$info$n_groups), unname(b$info$n_groups))

  # End to end: one RE row per level, no orphan row from either engine.
  out <- paste(
    capture.output(print(suppressWarnings(
      table_regression(list(nlme = fit_lme, lme4 = fit_lmer))
    ))),
    collapse = "\n"
  )
  expect_identical(
    length(gregexpr("\u03C3 ", out, fixed = TRUE)[[1L]]),
    3L
  )
  expect_identical(
    length(gregexpr("N (", out, fixed = TRUE)[[1L]]),
    2L
  )
})


# ---- 7. Control: a single-level fit is unchanged -------------------------

test_that("a single-level lme still reads its group from ngrps", {
  fit <- .fit_lme_single()
  fr <- spicy:::as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components

  # VarCorr carries NO block header for a single-level fit (the group
  # name lives in attr(vc, "title")), so this is the fall-back path. One
  # level composes to itself, and the block order is untouched.
  expect_false(any(grepl("=$", rownames(unclass(nlme::VarCorr(fit))))))
  expect_identical(
    vc$group,
    c("Subject", "Subject", "Subject", "Residual")
  )
  expect_identical(
    vc$term,
    c("(Intercept)", "age", "(Intercept), age", "")
  )
  expect_identical(names(fr$info$n_groups), "Subject")
  expect_identical(unname(fr$info$n_groups), 27L)

  iv <- nlme::intervals(fit, which = "var-cov")
  expect_equal(
    vc$variance[1:2],
    unname(iv$reStruct$Subject[c("sd((Intercept))", "sd(age)"), "est."]^2),
    tolerance = 1e-8
  )
  # One correlation row, still interleaved before the residual.
  expect_identical(vc$is_correlation, c(FALSE, FALSE, TRUE, FALSE))
})


test_that(".lme_group_labels composes only the nested levels", {
  # Unit-level: the map is keyed on nlme's bare name and valued by the
  # displayed label, outermost level first.
  expect_identical(
    spicy:::.lme_group_labels(.fit_lme_nested()),
    c(Dog = "Dog", Side = "Side:Dog")
  )
  expect_identical(
    spicy:::.lme_group_labels(.fit_lme_single()),
    c(Subject = "Subject")
  )
})
