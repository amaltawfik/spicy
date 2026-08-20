# ---------------------------------------------------------------------------
# table_categorical_svy(): the design twin of table_categorical().
#
# Same four groups of witnesses as the continuous twin: survey's own
# numbers pinned at 17 significant digits, the estimand boundary, an
# oracle that does not come from survey (the counts), and one refusal
# per branch.
# ---------------------------------------------------------------------------

.svycat_design <- function(which = "clus1") {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  switch(
    which,
    clus1 = survey::svydesign(
      id = ~dnum,
      weights = ~pw,
      data = apiclus1,
      fpc = ~fpc
    ),
    strat = survey::svydesign(
      id = ~1,
      strata = ~stype,
      weights = ~pw,
      data = apistrat,
      fpc = ~fpc
    ),
    rep1 = survey::as.svrepdesign(survey::svydesign(
      id = ~dnum,
      weights = ~pw,
      data = apiclus1,
      fpc = ~fpc
    ))
  )
}

.svycat_long <- function(...) {
  suppressWarnings(table_categorical_svy(..., output = "long"))
}

# ---- oracles --------------------------------------------------------------

test_that("the percentages are svymean's, in percent", {
  out <- .svycat_long(.svycat_design("clus1"), select = stype)
  expect_identical(out$.row_role, c("factor_header", "level", "level", "level"))
  expect_identical(out$level, c(NA, "E", "H", "M"))
  expect_equal(
    out[["%"]][-1L],
    100 *
      c(0.78688524590163933, 0.07650273224043716, 0.13661202185792351),
    tolerance = 1e-12
  )
  # `n` is the OBSERVED count (decision 28), not `svytotal()`'s
  # 4873.9674682617188 / 473.85794830322266 / 846.17490768432617.
  expect_identical(out$n[-1L], c(144L, 14L, 25L))
  expect_true(is.na(out[["%"]][[1L]]))
})

test_that("the design effects are svymean(deff = )'s, per level", {
  out <- .svycat_long(.svycat_design("clus1"), select = stype, deff = TRUE)
  expect_equal(
    out$DEff[-1L],
    c(2.4011450687604481, 1.9080248395269468, 1.3977271807412359),
    tolerance = 1e-12
  )
  # `"replace"` is the other design effect, and the note declares it.
  tbl <- table_categorical_svy(
    .svycat_design("clus1"),
    select = stype,
    deff = "replace"
  )
  expect_match(attr(tbl, "note"), "WITH replacement", fixed = TRUE)
  expect_false(isTRUE(all.equal(
    tbl$DEff[[2L]],
    2.4011450687604481,
    tolerance = 1e-6
  )))
})

test_that("the percentage intervals are svyciprop's, and the point is not", {
  d <- .svycat_design("clus1")
  out <- .svycat_long(d, select = stype, proportion_ci = TRUE)
  expect_equal(
    c(out[["% CI lower"]][[2L]], out[["% CI upper"]][[2L]]),
    100 * c(0.67120113442222151, 0.86976478763647846),
    tolerance = 1e-10
  )
  # The POINT stays `svymean()`'s. `svyciprop(method = "logit")`
  # estimates 0.78688524590150888 on the transformed scale -- the same
  # number to twelve decimals and not to thirteen -- and taking the
  # point from there would make the displayed percentage move with
  # `ci_method`, which is a property of the interval.
  expect_equal(out[["%"]][[2L]], 100 * 0.78688524590163933, tolerance = 1e-12)
  wilson <- .svycat_long(
    d,
    select = stype,
    proportion_ci = TRUE,
    ci_method = "wilson"
  )
  expect_identical(wilson[["%"]], out[["%"]])
  expect_false(isTRUE(all.equal(
    wilson[["% CI lower"]][[2L]],
    out[["% CI lower"]][[2L]],
    tolerance = 1e-8
  )))
  # `"mean"` is the Wald interval, i.e. `confint(svymean(), df = degf)`.
  mean_ci <- .svycat_long(
    d,
    select = stype,
    proportion_ci = TRUE,
    ci_method = "mean"
  )
  ref <- 100 *
    as.numeric(stats::confint(
      survey::svyciprop(
        ~ I(stype == "E"),
        d,
        method = "mean",
        df = survey::degf(d)
      )
    ))
  expect_equal(
    c(mean_ci[["% CI lower"]][[2L]], mean_ci[["% CI upper"]][[2L]]),
    ref,
    tolerance = 1e-10
  )
})

test_that("the p-value is svychisq's, on the statistic asked for", {
  d <- .svycat_design("clus1")
  out <- .svycat_long(d, select = stype, by = sch.wide)
  expect_equal(out$p[[1L]], 0.021747462370796899, tolerance = 1e-12)
  expect_true(all(is.na(out$p[-1L])))

  wald <- .svycat_long(
    d,
    select = awards,
    by = sch.wide,
    chisq_statistic = "Wald"
  )
  expect_equal(wald$p[[1L]], 6.8927149345038578e-04, tolerance = 1e-12)
  chisq <- .svycat_long(
    d,
    select = awards,
    by = sch.wide,
    chisq_statistic = "Chisq"
  )
  expect_equal(chisq$p[[1L]], 8.9607421807117829e-44, tolerance = 1e-12)
  f <- .svycat_long(d, select = awards, by = sch.wide)
  expect_equal(f$p[[1L]], 1.4209570188054670e-09, tolerance = 1e-12)
  # The five statistics are genuinely different tests, not five
  # renderings of one. Pinned on `stype`, whose p-values are far from
  # zero -- on `awards` they are all so small that `all.equal()`
  # compares them absolutely and calls 1.4e-09 and 9.0e-44 equal.
  st <- vapply(
    c("F", "Chisq", "Wald", "adjWald", "saddlepoint"),
    function(k) {
      .svycat_long(d, select = stype, by = sch.wide, chisq_statistic = k)$p[[
        1L
      ]]
    },
    numeric(1)
  )
  expect_equal(
    unname(st),
    c(
      0.021747462370796899,
      0.0055532633031132187,
      0.12689630060235083,
      0.14705788690124844,
      0.026441127709637535
    ),
    tolerance = 1e-12
  )
  # "Wald" and "adjWald" differ in their denominator degrees of freedom
  # and therefore in their p-value; on a 2x2 table they coincide, which
  # is why the witness is on a 3x2 one.
  expect_false(isTRUE(all.equal(st[["Wald"]], st[["adjWald"]])))
})

test_that("`by` gives one column block per domain plus the margin", {
  d <- .svycat_design("clus1")
  out <- .svycat_long(d, select = stype, by = sch.wide)
  expect_true(all(
    c("No n", "No %", "Yes n", "Yes %", "Total n", "Total %", "p") %in%
      names(out)
  ))
  # The percentages are COLUMN percentages: each block sums to 100
  # inside its own domain.
  for (b in c("No", "Yes", "Total")) {
    expect_equal(sum(out[[paste0(b, " %")]][-1L]), 100, tolerance = 1e-9)
  }
  expect_equal(
    out[["Yes %"]][-1L],
    100 * c(0.825000000000000067, 0.068750000000000006, 0.106250000000000011),
    tolerance = 1e-12
  )
  expect_identical(out[["Yes n"]][-1L], c(132L, 11L, 17L))
  # The margin is the whole design: the same numbers as the one-way
  # table, to the bit.
  overall <- .svycat_long(d, select = stype)
  expect_equal(out[["Total %"]][-1L], overall[["%"]][-1L], tolerance = 1e-12)
  expect_identical(out[["Total n"]][-1L], overall$n[-1L])
  # `include_total = FALSE` drops it and nothing else.
  no_total <- .svycat_long(
    d,
    select = stype,
    by = sch.wide,
    include_total = FALSE
  )
  expect_false(any(grepl("^Total ", names(no_total))))
  expect_equal(no_total[["Yes %"]], out[["Yes %"]], tolerance = 1e-12)
})

test_that("a calibrated domain counts only the rows it kept", {
  # Same trap as the continuous twin: `[` on a calibrated design keeps
  # the excluded rows at weight zero, so a count that did not filter
  # on a positive weight would read the whole sample in every block.
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  cal <- survey::calibrate(
    survey::svydesign(
      id = ~dnum,
      weights = ~pw,
      data = apiclus1,
      fpc = ~fpc
    ),
    ~stype,
    pop = c(`(Intercept)` = 6194, stypeH = 755, stypeM = 1018)
  )
  out <- .svycat_long(cal, select = awards, by = stype)
  expect_identical(sum(out[["E n"]], na.rm = TRUE), 144L)
  expect_identical(sum(out[["H n"]], na.rm = TRUE), 14L)
  expect_identical(sum(out[["Total n"]], na.rm = TRUE), 183L)
})

test_that("a replicate design gives the same percentages, its own intervals", {
  lin <- .svycat_long(
    .svycat_design("clus1"),
    select = stype,
    proportion_ci = TRUE
  )
  rep <- .svycat_long(
    .svycat_design("rep1"),
    select = stype,
    proportion_ci = TRUE
  )
  # The point estimate depends on the weights alone, so it is identical.
  expect_equal(rep[["%"]], lin[["%"]], tolerance = 1e-12)
  expect_identical(rep$n, lin$n)
  # The interval comes from the variance and therefore differs: never
  # pinned equal across regimes.
  expect_false(isTRUE(all.equal(
    rep[["% CI lower"]][[2L]],
    lin[["% CI lower"]][[2L]],
    tolerance = 1e-6
  )))
  expect_match(
    attr(table_categorical_svy(.svycat_design("rep1"), select = stype), "note"),
    "Standard errors: replicate weights (survey).",
    fixed = TRUE
  )
})

# ---- levels and missing values --------------------------------------------

test_that("missing values are a level of their own by default", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dat <- apiclus1
  dat$stype[1:5] <- NA
  des <- survey::svydesign(id = ~dnum, weights = ~pw, data = dat, fpc = ~fpc)
  out <- .svycat_long(des, select = stype)
  expect_identical(out$level, c(NA, "E", "H", "M", "(Missing)"))
  expect_identical(out$n[[5L]], 5L)
  expect_equal(sum(out[["%"]][-1L]), 100, tolerance = 1e-9)
  expect_identical(
    as_structured(table_categorical_svy(des, select = stype))$body$.row_role,
    c("factor_header", "level", "level", "level", "missing")
  )
  # `drop_na = TRUE` removes them and the note says how many.
  dropped <- table_categorical_svy(des, select = stype, drop_na = TRUE)
  expect_identical(nrow(dropped), 4L)
  expect_match(
    attr(dropped, "note"),
    "Missing values removed: stype (5).",
    fixed = TRUE
  )
  expect_equal(sum(dropped[["%"]][-1L]), 100, tolerance = 1e-9)
})

test_that("`levels_keep` selects levels, as a vector or per variable", {
  d <- .svycat_design("clus1")
  out <- .svycat_long(d, select = stype, levels_keep = c("E", "M"))
  expect_identical(out$level, c(NA, "E", "M"))
  # The percentages are NOT renormalised: they are estimates of the
  # whole variable's distribution, and dropping a display row does not
  # change what the remaining ones estimate.
  full <- .svycat_long(d, select = stype)
  expect_equal(out[["%"]][[2L]], full[["%"]][[2L]], tolerance = 1e-12)
  per_var <- .svycat_long(
    d,
    select = c(stype, awards),
    levels_keep = list(stype = "E")
  )
  expect_identical(per_var$level, c(NA, "E", NA, "No", "Yes"))
  expect_error(
    table_categorical_svy(d, select = stype, levels_keep = "Z"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical_svy(d, select = stype, levels_keep = list(nope = "E")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical_svy(d, select = stype, levels_keep = list("E")),
    class = "spicy_invalid_input"
  )
})

test_that("declared missing codes leave the table and the note", {
  skip_if_not_installed("survey")
  skip_if_not_installed("haven")
  d <- data.frame(w = rep(1, 6))
  d$g <- haven::labelled_spss(
    c(1, 1, 2, 9, 2, 1),
    labels = c(a = 1, b = 2, refused = 9),
    na_values = 9
  )
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  tbl <- table_categorical_svy(des, select = g, drop_na = TRUE)
  expect_match(
    attr(tbl, "note"),
    "Declared missing values removed: g (1).",
    fixed = TRUE
  )
  expect_identical(sum(tbl$n, na.rm = TRUE), 5L)
  # `user_na = FALSE` keeps the code as an ordinary category.
  kept <- table_categorical_svy(des, select = g, user_na = FALSE)
  expect_identical(sum(kept$n, na.rm = TRUE), 6L)
})

test_that("`drop_na` also governs the missing-`by` domain", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dat <- apiclus1
  dat$sch.wide[1:4] <- NA
  des <- survey::svydesign(id = ~dnum, weights = ~pw, data = dat, fpc = ~fpc)
  kept <- .svycat_long(des, select = stype, by = sch.wide)
  expect_true("(Missing) n" %in% names(kept))
  dropped <- .svycat_long(des, select = stype, by = sch.wide, drop_na = TRUE)
  expect_false("(Missing) n" %in% names(dropped))
  expect_match(
    attr(
      table_categorical_svy(des, select = stype, by = sch.wide, drop_na = TRUE),
      "note"
    ),
    "Rows with missing sch.wide removed: 4.",
    fixed = TRUE
  )
})

# ---- refusals -------------------------------------------------------------

test_that("the two unusable svychisq statistics are refused by name", {
  d <- .svycat_design("clus1")
  err <- expect_error(
    table_categorical_svy(
      d,
      select = stype,
      by = sch.wide,
      chisq_statistic = "lincom"
    ),
    class = "spicy_unsupported"
  )
  expect_match(conditionMessage(err), "pchisqsum", fixed = TRUE)
  err2 <- expect_error(
    table_categorical_svy(
      d,
      select = stype,
      by = sch.wide,
      chisq_statistic = "wls-score"
    ),
    class = "spicy_unsupported"
  )
  expect_match(conditionMessage(err2), "reporting convention", fixed = TRUE)
  # An unknown name is still the ordinary enum error.
  expect_error(
    table_categorical_svy(d, select = stype, chisq_statistic = "nope"),
    class = "spicy_invalid_input"
  )
})

test_that("the design-only refusals fire, one per branch", {
  d <- .svycat_design("clus1")
  expect_error(
    table_categorical_svy(mtcars, select = cyl),
    class = "spicy_wrong_regime"
  )
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  tw <- survey::twophase(
    id = list(~1, ~1),
    subset = ~ I(comp.imp == "Yes"),
    data = apiclus1
  )
  expect_error(
    table_categorical_svy(tw, select = stype),
    class = "spicy_unsupported"
  )
  expect_error(
    table_categorical_svy(d, select = stype, deff = "nope"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical_svy(d, select = stype, df = 0),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical_svy(d, select = stype, percent_digits = -1),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical_svy(d, select = stype, ci_level = 0),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical_svy(d, select = stype, p_digits = 0),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical_svy(d, select = stype, decimal_mark = "ab"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical_svy(d, select = stype, labels = "x"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical_svy(d, select = stype, drop_na = NA),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical_svy(d, select = stype, p_value = "yes"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical_svy(d, select = stype, ci_method = "nope"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_categorical_svy(d, select = character(0)),
    class = "spicy_invalid_input"
  )
  expect_warning(
    table_categorical_svy(d, select = stype, p_value = TRUE),
    class = "spicy_ignored_arg"
  )
})

test_that("the missing-Suggests guard covers this entry point too", {
  local_mocked_bindings(spicy_pkg_available = function(pkg) FALSE)
  expect_error(
    table_categorical_svy(mtcars, select = cyl),
    class = "spicy_missing_pkg"
  )
})

test_that("`df` overrides the design df, and the footer changes with it", {
  d <- .svycat_design("clus1")
  out <- .svycat_long(d, select = stype, proportion_ci = TRUE, df = 100)
  ref <- 100 *
    as.numeric(stats::confint(
      survey::svyciprop(~ I(stype == "E"), d, method = "logit", df = 100)
    ))
  expect_equal(
    c(out[["% CI lower"]][[2L]], out[["% CI upper"]][[2L]]),
    ref,
    tolerance = 1e-10
  )
  note <- attr(
    table_categorical_svy(d, select = stype, proportion_ci = TRUE, df = 100),
    "note"
  )
  expect_match(note, "supplied in `df`", fixed = TRUE)
})

# ---- restitution ----------------------------------------------------------

test_that("the console table prints its blocks, its spanners and its footer", {
  d <- .svycat_design("clus1")
  expect_snapshot(table_categorical_svy(d, select = c(stype, awards)))
  expect_snapshot(table_categorical_svy(d, select = stype, by = sch.wide))
  expect_snapshot(
    table_categorical_svy(
      d,
      select = stype,
      proportion_ci = TRUE,
      deff = TRUE
    )
  )
})

test_that("every rendering engine accepts a design categorical table", {
  d <- .svycat_design("clus1")
  for (eng in c("tinytable", "gt", "flextable")) {
    skip_if_not_installed(eng)
  }
  expect_s4_class(
    table_categorical_svy(
      d,
      select = stype,
      by = sch.wide,
      proportion_ci = TRUE,
      output = "tinytable"
    ),
    "tinytable"
  )
  expect_s3_class(
    table_categorical_svy(d, select = stype, by = sch.wide, output = "gt"),
    "spicy_gt"
  )
  expect_s3_class(
    table_categorical_svy(
      d,
      select = stype,
      by = sch.wide,
      output = "flextable"
    ),
    "spicy_flextable"
  )
  skip_if_not_installed("openxlsx2")
  xl <- withr::local_tempfile(fileext = ".xlsx")
  expect_type(
    table_categorical_svy(d, select = stype, output = "excel", excel_path = xl),
    "character"
  )
  expect_true(file.exists(xl))
  skip_if_not_installed("officer")
  doc <- withr::local_tempfile(fileext = ".docx")
  expect_type(
    table_categorical_svy(d, select = stype, output = "word", word_path = doc),
    "character"
  )
  expect_true(file.exists(doc))
})

test_that("the clipboard payload carries the table", {
  d <- .svycat_design("clus1")
  skip_if_not_installed("clipr")
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(content, ...) {
      captured$text <- content
      invisible(content)
    },
    .package = "clipr"
  )
  expect_message(
    table_categorical_svy(
      d,
      select = stype,
      by = sch.wide,
      output = "clipboard"
    ),
    "Categorical table copied to clipboard."
  )
  expect_match(captured$text, "Categorical table by sch.wide", fixed = TRUE)
  expect_match(captured$text, "78.7", fixed = TRUE)
})

test_that("the typed view keys every column by its block", {
  d <- .svycat_design("clus1")
  tbl <- table_categorical_svy(
    d,
    select = stype,
    by = sch.wide,
    proportion_ci = TRUE,
    deff = TRUE
  )
  s <- as_structured(tbl)
  expect_identical(s$version, 3L)
  expect_identical(
    s$body$.row_role,
    c("factor_header", "level", "level", "level")
  )
  expect_identical(s$body$.variable, rep("stype", 4L))
  expect_identical(s$col_meta[["Yes %"]]$token, "pct")
  expect_identical(s$col_meta[["Yes %"]]$group, "Yes")
  expect_null(s$col_meta[["Yes %"]]$total)
  expect_true(isTRUE(s$col_meta[["Total %"]]$total))
  expect_identical(s$col_meta[["Yes % CI lower"]]$token, "prop_ci")
  expect_identical(s$col_meta[["Yes % CI lower"]]$ci_pair, "Yes % CI upper")
  expect_identical(s$col_meta[["Yes DEff"]]$token, "deff")
  # The spanner contract of the sibling: a NAMED list of column
  # indices, which is what `inline(model = )` reads.
  expect_identical(names(s$spanners), c("No", "Yes", "Total"))
  expect_true(is.numeric(s$spanners[["Yes"]]))
  # The p sits on the header row, the level statistics on the level
  # rows; the other place is an ABSENCE, not an undefined cell.
  expect_false(is.na(s$body$p[[1L]]))
  expect_true(all(is.na(s$body$p[-1L])))
  expect_true(is.na(s$body[["Yes %"]][[1L]]))
  expect_identical(s$cell_status, list())
})

test_that("`inline()` cites a cell of a design categorical table", {
  d <- .svycat_design("clus1")
  tbl <- table_categorical_svy(
    d,
    select = stype,
    by = sch.wide,
    proportion_ci = TRUE
  )
  expect_identical(
    inline(tbl, "stype", level = "E", column = "pct", model = "Yes"),
    "82.5"
  )
  expect_identical(
    inline(tbl, "stype", level = "E", column = "n", model = "Yes"),
    "132"
  )
  expect_identical(inline(tbl, "stype", column = "p"), ".022")
  expect_identical(
    inline(tbl, "stype", level = "E", column = "prop_ci", model = "Yes"),
    "[71.4, 89.9]"
  )
  one_way <- table_categorical_svy(d, select = stype)
  expect_identical(
    inline(one_way, "stype", level = "E", column = "pct"),
    "78.7"
  )
})

test_that("`output = \"data.frame\"` and `\"long\"` are the same frame", {
  d <- .svycat_design("clus1")
  a <- table_categorical_svy(
    d,
    select = stype,
    by = sch.wide,
    output = "data.frame"
  )
  b <- table_categorical_svy(d, select = stype, by = sch.wide, output = "long")
  expect_identical(a, b)
  expect_false(inherits(a, "spicy_categorical_svy_table"))
})

test_that("coercion keeps the frame and the provenance markers", {
  d <- .svycat_design("clus1")
  tbl <- table_categorical_svy(d, select = stype, by = sch.wide)
  df <- as.data.frame(tbl)
  expect_false(inherits(df, "spicy_categorical_svy_table"))
  expect_identical(attr(df, "group_var"), "sch.wide")
  expect_identical(attr(df, "design_meta")$degf, 14)
  skip_if_not_installed("tibble")
  expect_s3_class(tibble::as_tibble(tbl), "tbl_df")
})

test_that("`labels`, a comma mark and a journal style all reach the table", {
  d <- .svycat_design("clus1")
  tbl <- table_categorical_svy(
    d,
    select = stype,
    labels = c(stype = "School type")
  )
  expect_true(any(grepl("School type", unlist(tbl), fixed = TRUE)))
  eu <- table_categorical_svy(d, select = stype, decimal_mark = ",")
  expect_true(any(grepl("78,7", capture.output(print(eu)), fixed = TRUE)))
  expect_s3_class(
    table_categorical_svy(d, select = stype, by = sch.wide, style = "jama"),
    "spicy_categorical_svy_table"
  )
})

test_that("a group whose name collides with the margin is disambiguated", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dat <- apiclus1
  dat$g <- ifelse(dat$sch.wide == "Yes", "Total", "other")
  des <- survey::svydesign(id = ~dnum, weights = ~pw, data = dat, fpc = ~fpc)
  out <- .svycat_long(des, select = stype, by = g)
  # The user's "Total" keeps its own columns; the margin takes the
  # disambiguated key.
  expect_true("Total n" %in% names(out))
  expect_true("Total_1 n" %in% names(out))
  s <- as_structured(table_categorical_svy(des, select = stype, by = g))
  expect_true(isTRUE(s$col_meta[["Total_1 %"]]$total))
  expect_null(s$col_meta[["Total %"]]$total)
})
