# table_battery_svy(): the design twin of table_battery() (ADR
# dev/decisions/2026-11-11-table-battery-svy.md).
#
# Oracles, all outside spicy: `survey::svymean()` on the domain cut with
# `subset()`, the weighted proportions written out by hand (the sum of the
# weights of a level over the sum of the weights of the domain) on a design
# with weights alone, `survey::svychisq()` per item, and Holm's adjustment
# written out. The data are the yes-no items of `survey::apistrat` and
# `apiclus1`, and a small built battery whose weighted percentages are
# known.

.BSVY_ITEMS <- c("awards", "sch.wide", "comp.imp", "both")
.bsvy_fig <- intToUtf8(0x2007L) # figure space: the decimal-alignment padding

.bsvy_api <- function(which = "strat") {
  skip_if_not_installed("survey", "4.5")
  data(api, package = "survey", envir = environment())
  switch(
    which,
    strat = survey::svydesign(
      id = ~1,
      strata = ~stype,
      weights = ~pw,
      data = apistrat,
      fpc = ~fpc
    ),
    clus1 = survey::svydesign(
      id = ~dnum,
      weights = ~pw,
      data = apiclus1,
      fpc = ~fpc
    )
  )
}

# Ten respondents, two items on a four-level scale ("DK" observed once),
# a missing group and missing answers. Without `by`, the weights of q1 give
# Agree 6, Neutral 4, Disagree 8 and DK 2 out of 20.
.bsvy_built <- function() {
  lv <- c("Agree", "Neutral", "Disagree", "DK")
  data.frame(
    w = c(1, 2, 3, 4, 1, 2, 3, 4, 2, 2),
    g = c("A", "A", "A", "A", "B", "B", "B", "B", NA, "B"),
    q1 = factor(
      c(
        "Agree",
        "Agree",
        "Disagree",
        "Neutral",
        "Agree",
        "Disagree",
        "Disagree",
        NA,
        "Agree",
        "DK"
      ),
      levels = lv
    ),
    q2 = factor(
      c(
        "Neutral",
        "Agree",
        "Agree",
        "Agree",
        "Disagree",
        "Neutral",
        NA,
        "Agree",
        NA,
        "Agree"
      ),
      levels = lv
    ),
    stringsAsFactors = FALSE
  )
}

.bsvy_built_design <- function(d = .bsvy_built()) {
  skip_if_not_installed("survey", "4.5")
  survey::svydesign(ids = ~1, weights = ~w, data = d)
}

# The weighted percentage of each level, by hand.
.bsvy_hand <- function(x, w, levels) {
  ok <- !is.na(x) & x %in% levels
  vapply(
    levels,
    function(l) 100 * sum(w[ok & x == l]) / sum(w[ok]),
    numeric(1),
    USE.NAMES = FALSE
  )
}

# svymean() of an item on a domain, in percent, level by level.
.bsvy_svymean <- function(design, item, keep, levels) {
  dom <- subset(design, keep)
  est <- stats::coef(survey::svymean(
    stats::as.formula(paste0("~", item)),
    dom,
    na.rm = TRUE
  ))
  100 * unname(est[paste0(item, levels)])
}

# Holm's step-down adjustment, written out.
.bsvy_holm <- function(p) {
  m <- length(p)
  o <- order(p)
  adj <- pmin(1, cummax((m - seq_len(m) + 1) * p[o]))
  out <- numeric(m)
  out[o] <- adj
  out
}

# ---- the numbers ---------------------------------------------------------------

test_that("the row percentages are svymean()'s, and n the observed count", {
  des <- .bsvy_api("strat")
  df <- table_battery_svy(
    des,
    tidyselect::all_of(.BSVY_ITEMS),
    output = "data.frame"
  )
  expect_identical(df$variable, .BSVY_ITEMS)
  for (i in seq_along(.BSVY_ITEMS)) {
    v <- .BSVY_ITEMS[[i]]
    oracle <- 100 *
      unname(stats::coef(survey::svymean(
        stats::as.formula(paste0("~", v)),
        des
      )))
    expect_equal(
      unname(unlist(df[i, c("No", "Yes")])),
      oracle,
      tolerance = 1e-12
    )
    expect_identical(df$n[[i]], 200)
  }
  # Pinned: survey 4.5 on apistrat, sch.wide.
  expect_equal(df$Yes[[2L]], 82.79480114158234, tolerance = 1e-12)
})

test_that("a design with weights alone gives the weighted proportions by hand", {
  d <- .bsvy_built()
  df <- table_battery_svy(
    .bsvy_built_design(d),
    c(q1, q2),
    output = "data.frame"
  )
  lv <- c("Agree", "Neutral", "Disagree", "DK")
  expect_equal(unname(unlist(df[1L, lv])), c(30, 20, 40, 10))
  expect_equal(unname(unlist(df[1L, lv])), .bsvy_hand(d$q1, d$w, lv))
  expect_equal(unname(unlist(df[2L, lv])), .bsvy_hand(d$q2, d$w, lv))
  expect_identical(df$n, c(9, 8))
  expect_identical(
    attr(df, "note"),
    paste(
      "Note. Design: simple random sample; 9 degrees of freedom.",
      "Percentages are row percentages estimated under the design.",
      "Base: respondents who answered the item.",
      "n = observed (unweighted) count."
    )
  )
})

test_that("by: svymean() in each group, svychisq() per item, Holm by hand", {
  des <- .bsvy_api("strat")
  items <- c("awards", "comp.imp", "both")
  df <- table_battery_svy(
    des,
    tidyselect::all_of(items),
    by = sch.wide,
    output = "data.frame"
  )
  sw <- des$variables$sch.wide
  for (i in seq_along(items)) {
    for (g in c("No", "Yes")) {
      expect_equal(
        unname(unlist(df[i, paste(g, c("No", "Yes"))])),
        .bsvy_svymean(des, items[[i]], sw == g, c("No", "Yes")),
        tolerance = 1e-12
      )
      expect_identical(df[[paste(g, "n")]][[i]], as.numeric(sum(sw == g)))
    }
  }
  raw <- vapply(
    items,
    function(v) {
      survey::svychisq(
        stats::as.formula(paste0("~", v, " + sch.wide")),
        des,
        statistic = "F"
      )$p.value
    },
    numeric(1),
    USE.NAMES = FALSE
  )
  expect_equal(df$p, .bsvy_holm(raw), tolerance = 1e-12)
  expect_equal(df$p, stats::p.adjust(raw, "holm"), tolerance = 1e-12)
  note <- attr(df, "note")
  expect_match(
    note,
    "Item p-values, each item by group: design-based Pearson chi-squared (Rao-Scott second-order correction). Adjusted for 3 tests (Holm method).",
    fixed = TRUE
  )
  expect_match(note, "^Note\\. Design: stratified \\(stype\\)")
})

test_that("a data frame is refused, and so is a statistic the family refuses", {
  d <- .bsvy_built()
  expect_error(
    table_battery_svy(d, c(q1, q2)),
    "expects a survey design object",
    class = "spicy_wrong_regime"
  )
  des <- .bsvy_built_design(d)
  expect_error(
    table_battery(des, c(q1, q2)),
    "table_battery_svy(design, ...)",
    fixed = TRUE,
    class = "spicy_wrong_regime"
  )
  expect_error(
    table_battery_svy(des, c(q1, q2), by = g, chisq_statistic = "lincom"),
    class = "spicy_unsupported"
  )
  expect_error(table_battery_svy(des), class = "spicy_invalid_input")
})

test_that("the French table translates its title, the note and the missing column", {
  withr::local_options(spicy.language = "fr")
  tbl <- table_battery_svy(
    .bsvy_built_design(),
    c(q1, q2),
    by = g,
    drop_na = FALSE
  )
  expect_identical(attr(tbl, "title"), "Batterie d'items selon g")
  expect_true("A (Manquant)" %in% names(attr(tbl, "display_df")))
  note <- attr(tbl, "note")
  expect_match(
    note,
    "Pourcentages en ligne estim\u00e9s sous le plan de sondage.",
    fixed = TRUE
  )
  expect_match(
    note,
    "n = effectif observ\u00e9 (non pond\u00e9r\u00e9).",
    fixed = TRUE
  )
})

# ---- levels, top, sort, drop_na, show_n, labels -----------------------------------

test_that("levels leaves a value out of the domain, and the note names it", {
  skip_on_cran()
  d <- .bsvy_built()
  lv <- c("Agree", "Neutral", "Disagree")
  df <- table_battery_svy(
    .bsvy_built_design(d),
    c(q1, q2),
    levels = lv,
    output = "data.frame"
  )
  expect_identical(names(df), c("variable", "label", lv, "n"))
  expect_equal(unname(unlist(df[1L, lv])), 100 * c(6, 4, 8) / 18)
  expect_equal(unname(unlist(df[1L, lv])), .bsvy_hand(d$q1, d$w, lv))
  expect_identical(df$n, c(8, 8))
  expect_match(
    attr(df, "note"),
    "Answers left out of the percentages and the base: DK.",
    fixed = TRUE
  )
})

test_that("top adds up its levels, by group", {
  skip_on_cran()
  d <- .bsvy_built()
  df <- table_battery_svy(
    .bsvy_built_design(d),
    c(q1, q2),
    by = g,
    top = c("Agree", "Neutral"),
    output = "data.frame"
  )
  expect_identical(
    names(df),
    c(
      "variable",
      "label",
      "A Agree + Neutral",
      "A n",
      "B Agree + Neutral",
      "B n",
      "p"
    )
  )
  lv <- c("Agree", "Neutral", "Disagree", "DK")
  for (g in c("A", "B")) {
    in_g <- !is.na(d$g) & d$g == g
    for (i in 1:2) {
      x <- d[[c("q1", "q2")[[i]]]][in_g]
      expect_equal(
        df[[paste(g, "Agree + Neutral")]][[i]],
        sum(.bsvy_hand(x, d$w[in_g], lv)[1:2])
      )
    }
  }
  # Group B, q1: Agree 1 of 8.
  expect_equal(df[["B Agree + Neutral"]][[1L]], 12.5)
})

test_that("every sort form orders the rows on the weighted percentages", {
  skip_on_cran()
  des <- .bsvy_api("strat")
  items <- .BSVY_ITEMS
  yes <- vapply(
    .BSVY_ITEMS,
    function(v) {
      .bsvy_svymean(des, v, rep(TRUE, 200), "Yes")
    },
    numeric(1),
    USE.NAMES = FALSE
  )
  vars_of <- function(...) {
    table_battery_svy(des, items, output = "data.frame", ...)$variable
  }
  expect_identical(vars_of(sort = ""), .BSVY_ITEMS)
  # Without `top`, "+" and "-" sort on the first level shown, "No".
  expect_identical(
    vars_of(sort = "+"),
    .BSVY_ITEMS[order(100 - yes, method = "radix")]
  )
  expect_identical(
    vars_of(sort = "Yes-"),
    .BSVY_ITEMS[order(yes, decreasing = TRUE, method = "radix")]
  )
  # awards and both tie: they keep the order of `select`.
  expect_identical(
    vars_of(sort = "Yes-"),
    c("sch.wide", "comp.imp", "awards", "both")
  )
  expect_identical(
    vars_of(sort = "name-"),
    sort(.BSVY_ITEMS, decreasing = TRUE, method = "radix")
  )
  expect_identical(vars_of(sort = "name+"), sort(.BSVY_ITEMS, method = "radix"))
  # By group, the percentages of all the groups together, under the design.
  by_df <- table_battery_svy(
    des,
    tidyselect::all_of(c("awards", "comp.imp", "both")),
    by = sch.wide,
    top = "Yes",
    sort = "top+",
    output = "data.frame"
  )
  pooled <- yes[c(1L, 3L, 4L)]
  expect_identical(
    by_df$variable,
    c("awards", "comp.imp", "both")[order(pooled, method = "radix")]
  )
  expect_identical(
    table_battery_svy(
      des,
      tidyselect::all_of(c("awards", "comp.imp", "both")),
      by = sch.wide,
      top = "Yes",
      sort = "-",
      output = "data.frame"
    )$variable,
    c("awards", "comp.imp", "both")[order(
      pooled,
      decreasing = TRUE,
      method = "radix"
    )]
  )
  expect_error(
    table_battery_svy(des, items, sort = "Yes"),
    "two readings",
    class = "spicy_invalid_input"
  )
  expect_error(
    table_battery_svy(des, items, sort = "top-"),
    "needs `top`",
    class = "spicy_invalid_input"
  )
})

# Five three-level items of apistrat whose weighted and unweighted orders
# differ on "Hi".
.bsvy_cut3 <- function(v, a, b) {
  factor(
    ifelse(v > b, "Hi", ifelse(v > a, "Mid", "Lo")),
    levels = c("Lo", "Mid", "Hi")
  )
}
.bsvy_five <- function() {
  skip_if_not_installed("survey", "4.5")
  data(api, package = "survey", envir = environment())
  d <- apistrat
  d$q1 <- .bsvy_cut3(d$api00, 600, 700)
  d$q2 <- .bsvy_cut3(d$api99, 600, 700)
  d$q3 <- .bsvy_cut3(d$meals, 30, 60)
  d$q4 <- .bsvy_cut3(d$ell, 10, 30)
  d$q5 <- .bsvy_cut3(d$growth, 20, 40)
  d
}

test_that("sort reads the weighted percentages, pooled over the groups", {
  skip_on_cran()
  d <- .bsvy_five()
  des <- survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = d,
    fpc = ~fpc
  )
  its <- paste0("q", 1:5)
  hi <- vapply(
    its,
    function(v) .bsvy_svymean(des, v, rep(TRUE, 200), "Hi"),
    numeric(1),
    USE.NAMES = FALSE
  )
  weighted <- c("q1", "q5", "q3", "q2", "q4")
  expect_identical(
    its[order(hi, decreasing = TRUE, method = "radix")],
    weighted
  )
  # Unweighted, the order would be q1, q3, q5, q2, q4.
  expect_identical(
    table_battery_svy(des, its, sort = "Hi-", output = "data.frame")$variable,
    weighted
  )
  # By group, the key is that of all the groups together (the first group
  # alone would give q5, q1, q3, q4, q2).
  expect_identical(
    table_battery_svy(
      des,
      its,
      by = stype,
      sort = "Hi-",
      output = "data.frame"
    )$variable,
    weighted
  )
  # A missing group leaves the pooled key: with 60 of the 200 groups
  # missing, the order is q1, q5, q2, q3, q4 (q1, q5, q3, q2, q4 with the
  # rows of the missing group kept).
  set.seed(5)
  d$stype[sample(200, 60)] <- NA
  des4 <- survey::svydesign(id = ~1, weights = ~pw, data = d)
  key <- vapply(
    its,
    function(v) .bsvy_svymean(des4, v, !is.na(d$stype), "Hi"),
    numeric(1),
    USE.NAMES = FALSE
  )
  expect_identical(
    its[order(key, decreasing = TRUE, method = "radix")],
    c("q1", "q5", "q2", "q3", "q4")
  )
  expect_identical(
    table_battery_svy(
      des4,
      its,
      by = stype,
      sort = "Hi-",
      output = "data.frame"
    )$variable,
    c("q1", "q5", "q2", "q3", "q4")
  )
})

test_that("the pooled percentages that sort under by leave out a missing group", {
  skip_on_cran()
  d <- .bsvy_built()
  des <- .bsvy_built_design(d)
  df <- table_battery_svy(
    des,
    c(q1, q2),
    by = g,
    sort = "Agree+",
    output = "data.frame"
  )
  keep <- !is.na(d$g)
  key <- c(
    .bsvy_svymean(des, "q1", keep & !is.na(d$q1), "Agree"),
    .bsvy_svymean(des, "q2", keep & !is.na(d$q2), "Agree")
  )
  # q1: Agree 4 of 18 once the row without a group is out (6 of 20 with it).
  expect_equal(key[[1L]], 100 * 4 / 18)
  expect_identical(df$variable, c("q1", "q2")[order(key)])
  expect_match(attr(df, "note"), "Missing values removed: g (1).", fixed = TRUE)
})

test_that("drop_na = FALSE takes the missing answers into the domain", {
  skip_on_cran()
  d <- .bsvy_built()
  des <- .bsvy_built_design(d)
  df <- table_battery_svy(
    des,
    c(q1, q2),
    by = g,
    drop_na = FALSE,
    output = "data.frame"
  )
  # Group B, q1: Agree 1, Disagree 5, DK 2 and a missing answer of weight 4,
  # out of 12.
  expect_equal(
    unname(unlist(df[
      1L,
      paste("B", c("Agree", "Neutral", "Disagree", "DK", "(Missing)"))
    ])),
    100 * c(1, 0, 5, 2, 4) / 12
  )
  expect_identical(df[["B n"]], c(5, 5))
  expect_match(
    attr(df, "note"),
    "Base: all respondents, including those who did not answer the item.",
    fixed = TRUE
  )
  # The test leaves the missing answers out whatever drop_na.
  expect_equal(
    df$p,
    table_battery_svy(des, c(q1, q2), by = g, output = "data.frame")$p
  )
  long <- table_battery_svy(des, c(q1, q2), drop_na = FALSE, output = "long")
  miss <- long[long$.row_role == "missing", ]
  expect_identical(miss$level, c("(Missing)", "(Missing)"))
  # q2 without `by`: two missing answers, weights 3 and 2, out of 24.
  expect_equal(miss$pct[[2L]], 100 * 5 / 24)
  expect_identical(miss$n, c(1, 2))
})

test_that("a level named like the column of missing answers is refused", {
  skip_on_cran()
  d <- .bsvy_built()
  for (v in c("q1", "q2")) {
    x <- as.character(d[[v]])
    x[x == "DK"] <- "(Missing)"
    d[[v]] <- factor(x, levels = c("Agree", "Neutral", "Disagree", "(Missing)"))
  }
  expect_error(
    table_battery_svy(.bsvy_built_design(d), c(q1, q2), drop_na = FALSE),
    class = "spicy_label_collision"
  )
})

test_that("show_n = FALSE drops n and its gloss", {
  skip_on_cran()
  tbl <- table_battery_svy(.bsvy_built_design(), c(q1, q2), show_n = FALSE)
  expect_false("n" %in% names(attr(tbl, "display_df")))
  expect_false(grepl("unweighted", attr(tbl, "note"), fixed = TRUE))
})

test_that("labels name the items, as in table_battery()", {
  skip_on_cran()
  d <- .bsvy_built()
  attr(d$q1, "label") <- "First item"
  des <- .bsvy_built_design(d)
  expect_identical(
    table_battery_svy(
      des,
      c(q1, q2),
      labels = TRUE,
      output = "data.frame"
    )$label,
    c("First item", "q2")
  )
  df <- table_battery_svy(
    des,
    c(q1, q2),
    labels = c(q2 = "Second item"),
    output = "data.frame"
  )
  expect_identical(df$label, c("q1", "Second item"))
  expect_identical(df$variable, c("q1", "q2"))
})

# ---- designs ----------------------------------------------------------------------

test_that("a replicate design gives the same percentages, and its own test", {
  skip_on_cran()
  des <- .bsvy_api("clus1")
  rep <- survey::as.svrepdesign(des)
  items <- .BSVY_ITEMS
  lin <- table_battery_svy(des, items, by = stype, output = "data.frame")
  rdf <- table_battery_svy(rep, items, by = stype, output = "data.frame")
  pct_cols <- setdiff(names(lin), c("variable", "label", "p"))
  expect_equal(rdf[pct_cols], lin[pct_cols], tolerance = 1e-12)
  raw <- vapply(
    .BSVY_ITEMS,
    function(v) {
      survey::svychisq(
        stats::as.formula(paste0("~", v, " + stype")),
        rep
      )$p.value
    },
    numeric(1),
    USE.NAMES = FALSE
  )
  expect_equal(rdf$p, stats::p.adjust(raw, "holm"), tolerance = 1e-12)
  expect_match(
    attr(rdf, "note"),
    "Design: replicate weights (JK1), 15 replicates; 14 degrees of freedom.",
    fixed = TRUE
  )
  expect_error(
    table_battery_svy(rep, items, by = stype, chisq_statistic = "saddlepoint"),
    class = "spicy_unsupported"
  )
  wald <- table_battery_svy(
    rep,
    items,
    by = stype,
    chisq_statistic = "Wald",
    p_adjust = "none",
    output = "long"
  )
  expect_equal(
    unique(wald$p.value[wald$variable == "awards"]),
    survey::svychisq(~ awards + stype, rep, statistic = "Wald")$p.value[[1L]],
    tolerance = 1e-12
  )
  expect_identical(unique(wald$test_type), "Wald")
})

test_that("negative calibration weights keep the percentages and withhold the test", {
  skip_on_cran()
  des <- .bsvy_api("clus1")
  neg <- survey::calibrate(
    des,
    ~api99,
    c(`(Intercept)` = 6194, api99 = 6194 * 500),
    calfun = "linear"
  )
  w <- stats::weights(neg)
  expect_identical(sum(w < 0), 28L)
  expect_warning(
    df <- table_battery_svy(
      neg,
      tidyselect::all_of(.BSVY_ITEMS),
      by = stype,
      output = "data.frame"
    ),
    class = "spicy_negative_weights_no_test"
  )
  expect_true(all(is.na(df$p)))
  st <- neg$variables$stype
  for (g in c("E", "H", "M")) {
    in_g <- st == g
    expect_equal(
      unname(unlist(df[1L, paste(g, c("No", "Yes"))])),
      .bsvy_hand(neg$variables$awards[in_g], w[in_g], c("No", "Yes")),
      tolerance = 1e-10
    )
  }
  note <- attr(df, "note")
  expect_match(
    note,
    "Calibration gave 28 of 183 rows a negative weight: a weighted percentage can fall below 0 or above 100.",
    fixed = TRUE
  )
  expect_false(grepl("variance", note, fixed = TRUE))
  expect_match(note, "The group comparison is not reported", fixed = TRUE)
  expect_false(grepl("Item p-values", note, fixed = TRUE))
})

test_that("a calibrated subset counts the rows it kept, not those at weight zero", {
  skip_on_cran()
  des <- .bsvy_api("clus1")
  pos <- survey::calibrate(
    des,
    ~stype,
    pop = c(`(Intercept)` = 6194, stypeH = 755, stypeM = 1018)
  )
  keep <- pos$variables$stype != "H"
  sub <- pos[keep, ]
  # `[` keeps the rows of a calibrated design, at weight zero.
  expect_identical(nrow(sub$variables), 183L)
  df <- table_battery_svy(sub, c(awards, sch.wide), output = "data.frame")
  expect_identical(df$n, c(sum(keep), sum(keep)) + 0)
  expect_equal(
    unname(unlist(df[1L, c("No", "Yes")])),
    .bsvy_svymean(pos, "awards", keep, c("No", "Yes")),
    tolerance = 1e-12
  )
  # A level held by the rows at weight zero alone is no level of the scale.
  sub$variables$ch1 <- ifelse(
    sub$variables$stype == "H",
    "Other",
    as.character(sub$variables$awards)
  )
  sub$variables$ch2 <- as.character(sub$variables$both)
  expect_named(
    table_battery_svy(sub, c(ch1, ch2), output = "data.frame"),
    c("variable", "label", "Yes", "No", "n")
  )
})

test_that("a one-level scale is 100 by arithmetic, and its item is not tested", {
  skip_on_cran()
  des <- .bsvy_api("strat")
  tbl <- table_battery_svy(
    des,
    tidyselect::all_of(.BSVY_ITEMS),
    by = stype,
    levels = "Yes"
  )
  df <- as.data.frame(tbl)
  expect_true(all(df[["E Yes"]] == 100))
  expect_identical(
    df[["E n"]],
    vapply(
      .BSVY_ITEMS,
      function(v) {
        as.numeric(sum(
          des$variables[[v]] == "Yes" & des$variables$stype == "E"
        ))
      },
      numeric(1),
      USE.NAMES = FALSE
    )
  )
  expect_true(all(is.na(df$p)))
  expect_identical(unique(attr(tbl, "display_df")$p), "\u2013")
  note <- attr(tbl, "note")
  expect_match(note, "Not tested (no variation", fixed = TRUE)
  expect_false(grepl("Item p-values", note, fixed = TRUE))
})

test_that("an item without data in a group has no test and the note says so", {
  skip_on_cran()
  d <- .bsvy_built()
  d$q2[!is.na(d$g) & d$g == "B"] <- NA
  tbl <- table_battery_svy(.bsvy_built_design(d), c(q1, q2), by = g)
  shown <- attr(tbl, "display_df")
  expect_identical(shown[["B Agree"]][[2L]], "\u2013")
  expect_identical(shown$p[[2L]], "")
  expect_match(attr(tbl, "note"), "No data for q2 in B.", fixed = TRUE)
  # One test is left in the family.
  expect_match(
    attr(tbl, "note"),
    "Item p-value, by group: design-based",
    fixed = TRUE
  )
})

# ---- the tests and their adjustment ------------------------------------------------

test_that("p_adjust: none, BH, and the refusals of the family", {
  skip_on_cran()
  des <- .bsvy_api("strat")
  items <- c("awards", "comp.imp")
  raw <- vapply(
    c("awards", "comp.imp"),
    function(v) {
      survey::svychisq(
        stats::as.formula(paste0("~", v, " + sch.wide")),
        des
      )$p.value
    },
    numeric(1),
    USE.NAMES = FALSE
  )
  none <- table_battery_svy(
    des,
    items,
    by = sch.wide,
    p_adjust = "none",
    output = "data.frame"
  )
  expect_equal(none$p, raw, tolerance = 1e-12)
  bh <- table_battery_svy(
    des,
    items,
    by = sch.wide,
    p_adjust = "BH",
    output = "long"
  )
  expect_equal(
    unique(bh$p.value),
    stats::p.adjust(raw, "BH"),
    tolerance = 1e-12
  )
  expect_equal(unique(bh$p_unadjusted), raw, tolerance = 1e-12)
  expect_error(
    table_battery_svy(des, items, p_adjust = "holm"),
    "needs `by`",
    class = "spicy_invalid_input"
  )
  expect_warning(
    table_battery_svy(des, items, chisq_statistic = "Wald"),
    "`chisq_statistic` is ignored when `by` is not used.",
    fixed = TRUE,
    class = "spicy_ignored_arg"
  )
  nop <- table_battery_svy(
    des,
    items,
    by = sch.wide,
    p_value = FALSE,
    output = "data.frame"
  )
  expect_false("p" %in% names(nop))
})

# ---- the outputs -------------------------------------------------------------------

test_that("the machine outputs: data.frame, long, as.data.frame(), as_structured()", {
  skip_on_cran()
  des <- .bsvy_api("clus1")
  items <- .BSVY_ITEMS
  tbl <- table_battery_svy(des, items, by = stype)
  expect_s3_class(tbl, "spicy_battery_svy_table")
  df <- table_battery_svy(des, items, by = stype, output = "data.frame")
  expect_identical(as.data.frame(tbl), df)
  expect_identical(class(df), "data.frame")
  long <- table_battery_svy(des, items, by = stype, output = "long")
  expect_identical(
    names(long),
    c(
      "variable",
      "label",
      "group",
      "level",
      ".row_role",
      "n",
      "pct",
      "test_type",
      "p.value",
      "p_unadjusted"
    )
  )
  expect_identical(nrow(long), 4L * 3L * 2L)
  expect_identical(unique(long$test_type), "F")
  # n is the observed count of each level in each group.
  sub <- long[long$variable == "awards" & long$group == "E", ]
  expect_identical(
    sub$n,
    as.numeric(table(des$variables$awards[des$variables$stype == "E"]))
  )
  s <- as_structured(tbl)
  expect_identical(s$body$.row_role, rep("summary", 4L))
  expect_identical(s$body$.variable, .BSVY_ITEMS)
  expect_named(s$spanners, c("E", "H", "M"))
  expect_identical(s$col_meta[["H Yes"]]$scale_level, "Yes")
  expect_identical(s$col_meta$p$p_adjust, "holm")
  expect_identical(s$col_meta$p$test_type, rep("F", 4L))
  expect_s3_class(tibble::as_tibble(tbl), "tbl_df")
})

test_that("inline() cites a cell by item, level and group", {
  skip_on_cran()
  des <- .bsvy_api("clus1")
  tbl <- table_battery_svy(des, tidyselect::all_of(.BSVY_ITEMS), by = stype)
  oracle <- .bsvy_svymean(des, "awards", des$variables$stype == "H", "Yes")
  expect_identical(
    inline(tbl, awards, "Yes", "pct", model = "H"),
    sprintf("%.1f", oracle)
  )
  one <- table_battery_svy(des, tidyselect::all_of(.BSVY_ITEMS))
  expect_identical(
    inline(one, sch.wide, "Yes"),
    trimws(attr(one, "display_df")$Yes[[2L]])
  )
})

test_that("gt, tinytable and flextable receive the cells of the typed view", {
  skip_on_cran()
  skip_if_not_installed("gt")
  skip_if_not_installed("tinytable")
  skip_if_not_installed("flextable")
  des <- .bsvy_api("clus1")
  items <- .BSVY_ITEMS
  shown <- attr(table_battery_svy(des, items, by = stype), "display_df")
  g <- table_battery_svy(des, items, by = stype, output = "gt")
  expect_identical(names(g[["_data"]]), names(shown))
  expect_identical(
    trimws(gsub(.bsvy_fig, "", g[["_data"]][["H Yes"]])),
    shown[["H Yes"]]
  )
  expect_match(attr(g, "spicy_note"), "Rao-Scott", fixed = TRUE)
  tt <- table_battery_svy(
    des,
    items,
    by = stype,
    top = "Yes",
    output = "tinytable"
  )
  expect_identical(dim(tt@data), c(4L, 8L))
  ft <- table_battery_svy(des, items, output = "flextable")
  expect_identical(ft$col_keys, c("Variable", "No", "Yes", "n"))
})

test_that("Excel and Word read back the table", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  xlsx <- tempfile(fileext = ".xlsx")
  docx <- tempfile(fileext = ".docx")
  on.exit(unlink(c(xlsx, docx)), add = TRUE)
  des <- .bsvy_api("clus1")
  items <- .BSVY_ITEMS
  shown <- attr(table_battery_svy(des, items, by = stype), "display_df")
  table_battery_svy(des, items, by = stype, output = "excel", excel_path = xlsx)
  grid <- as.matrix(openxlsx2::wb_to_df(
    openxlsx2::wb_load(xlsx),
    col_names = FALSE
  ))
  expect_identical(unname(grid[1L, 1L]), "Item battery by stype")
  expect_identical(unname(grid[3L, 2L]), "E")
  expect_identical(unname(grid[4L, 2:4]), c("No", "Yes", "n"))
  expect_identical(unname(grid[5L, 1L]), "awards")
  expect_equal(
    as.numeric(grid[5L, 2:3]),
    round(as.numeric(shown[1L, 2:3]), 1)
  )
  expect_identical(
    unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(xlsx))),
    "Item battery"
  )

  table_battery_svy(des, items, output = "word", word_path = docx)
  summ <- officer::docx_summary(officer::read_docx(docx))
  cells <- summ[summ$content_type == "table cell", ]
  head_row <- trimws(cells$text[cells$row_id == min(cells$row_id)])
  expect_identical(head_row, c("Variable", "No", "Yes", "n"))
  body1 <- trimws(gsub(
    .bsvy_fig,
    "",
    cells$text[cells$row_id == min(cells$row_id) + 1L]
  ))
  expect_identical(
    body1,
    unname(unlist(attr(table_battery_svy(des, items), "display_df")[1L, ]))
  )
  expect_true(any(grepl(
    "Percentages are row percentages estimated under the design.",
    summ$text,
    fixed = TRUE
  )))
})

test_that("the console prints the table in French", {
  skip_on_cran()
  withr::local_options(spicy.language = "fr")
  des <- .bsvy_api("strat")
  expect_snapshot(
    table_battery_svy(
      des,
      tidyselect::all_of(.BSVY_ITEMS),
      by = stype,
      top = "Yes"
    )
  )
})

# ---- a stratum with a single primary sampling unit -------------------------
# The two designs and the sentence of the note: helper-svy-lonely.R.

test_that("a lonely PSU leaves dashes, warns once and says so in the note", {
  skip_on_cran()
  d <- .svy_lonely_design()
  withr::local_options(
    survey.lonely.psu = "fail",
    survey.adjust.domain.lonely = FALSE
  )
  items <- c("awards", "comp.imp", "both")
  # The oracle: survey stops on the design itself, for the percentages and
  # the test, and estimates the domain without the lonely stratum.
  msg <- tryCatch(survey::svymean(~awards, d), error = conditionMessage)
  expect_identical(msg, "Stratum (H) has only one PSU at stage 1")
  expect_error(survey::svychisq(~ awards + sch.wide, d), msg, fixed = TRUE)
  yes <- subset(d, sch.wide == "Yes")
  pct_yes <- unlist(lapply(items, function(it) {
    100 *
      as.numeric(stats::coef(survey::svymean(
        stats::as.formula(paste0("~", it)),
        yes
      )))
  }))
  x <- d$variables

  # Without `by`: every percentage is a dash, every base stands, and one
  # warning for the three items.
  run <- .svy_collect_warnings(table_battery_svy(
    d,
    select = c(awards, comp.imp, both),
    output = "long"
  ))
  out <- run$value
  failed <- .svy_failed_warnings(run$warnings)
  expect_length(run$warnings, 1L)
  expect_length(failed, 1L)
  expect_s3_class(failed[[1L]], "spicy_undefined_stat")
  expect_identical(failed[[1L]]$survey_message, msg)
  expect_identical(failed[[1L]]$fn, list("survey::svymean()"))
  expect_true(all(is.na(out$pct)))
  expect_equal(
    out$n,
    unlist(lapply(items, function(it) as.numeric(table(x[[it]]))))
  )
  expect_match(attr(out, "note"), .SVY_LONELY_EN, fixed = TRUE)

  # With `by`: the group that holds the lonely school is dashed, the other
  # one is survey's own, and the tests of the whole design stop.
  run <- .svy_collect_warnings(table_battery_svy(
    d,
    select = c(awards, comp.imp, both),
    by = sch.wide,
    output = "long"
  ))
  out <- run$value
  failed <- .svy_failed_warnings(run$warnings)
  expect_length(failed, 1L)
  expect_identical(
    failed[[1L]]$fn,
    list(c("survey::svymean()", "survey::svychisq()"))
  )
  expect_true(all(is.na(out$pct[out$group == "No"])))
  expect_equal(out$pct[out$group == "Yes"], pct_yes, tolerance = 1e-12)
  expect_equal(
    out$n[out$group == "No"],
    unlist(lapply(items, function(it) {
      as.numeric(table(x[[it]][x$sch.wide == "No"]))
    }))
  )
  expect_true(all(is.na(out$p.value)))
  expect_match(attr(out, "note"), .SVY_LONELY_EN, fixed = TRUE)

  # survey's domain option changes nothing here: the stratum is lonely in
  # the design, not only in the domain.
  withr::local_options(survey.adjust.domain.lonely = TRUE)
  run2 <- .svy_collect_warnings(table_battery_svy(
    d,
    select = c(awards, comp.imp, both),
    by = sch.wide,
    output = "long"
  ))
  expect_length(.svy_failed_warnings(run2$warnings), 1L)
  expect_equal(run2$value, out, ignore_attr = TRUE)

  # The note says it in the language of the table, without survey's words.
  withr::local_options(spicy.language = "fr")
  note_fr <- attr(
    suppressWarnings(table_battery_svy(d, select = c(awards, comp.imp))),
    "note"
  )
  expect_match(note_fr, .SVY_LONELY_FR, fixed = TRUE)
  expect_false(grepl("only one PSU", note_fr, fixed = TRUE))
})

test_that("under the other lonely.psu options the table is survey's, silent", {
  skip_on_cran()
  d <- .svy_lonely_design()
  for (opt in c("remove", "adjust", "average", "certainty")) {
    withr::local_options(
      survey.lonely.psu = opt,
      survey.adjust.domain.lonely = FALSE
    )
    m <- survey::svymean(~awards, d)
    expect_no_warning(
      out <- table_battery_svy(d, select = c(awards, comp.imp), output = "long")
    )
    expect_equal(
      out$pct[out$variable == "awards"],
      100 * as.numeric(stats::coef(m)),
      tolerance = 1e-12
    )
    expect_false(anyNA(out$pct))
    expect_false(grepl("A dash marks", attr(out, "note"), fixed = TRUE))
  }
})

test_that("a lonely PSU of a domain only is survey's warning, not a dash", {
  skip_on_cran()
  d <- .svy_domain_lonely_design()
  for (opt in c("fail", "remove", "adjust", "average", "certainty")) {
    withr::local_options(
      survey.lonely.psu = opt,
      survey.adjust.domain.lonely = TRUE
    )
    ref <- .svy_collect_warnings(
      survey::svymean(~awards, subset(d, grp == "B"))
    )
    expect_identical(
      vapply(ref$warnings, conditionMessage, character(1)),
      "Stratum (H) has only one PSU at stage 1"
    )
    run <- .svy_collect_warnings(table_battery_svy(
      d,
      select = c(awards, comp.imp),
      by = grp,
      output = "long"
    ))
    out <- run$value
    # survey's own warning reaches the caller, once for the table; spicy
    # adds none.
    expect_length(.svy_failed_warnings(run$warnings), 0L)
    expect_length(run$warnings, 1L)
    expect_true(startsWith(
      conditionMessage(run$warnings[[1L]]),
      "Stratum (H) has only one PSU at stage 1\n(raised "
    ))
    expect_equal(
      out$pct[out$group == "B" & out$variable == "awards"],
      100 * as.numeric(stats::coef(ref$value)),
      tolerance = 1e-12
    )
    expect_false(grepl("A dash marks", attr(out, "note"), fixed = TRUE))
  }
})

test_that("survey's own warnings are said once per table, with their count", {
  skip_on_cran()
  d <- .svy_two_domain_lonely_design()
  withr::local_options(
    survey.lonely.psu = "adjust",
    survey.adjust.domain.lonely = TRUE
  )
  # The oracle: survey on the two lonely domains, one warning per item in
  # each.
  items <- c("awards", "comp.imp", "both")
  ref <- .svy_domain_warnings(d, items, survey::svymean)
  expect_identical(
    unname(ref$msg),
    c(
      "Stratum (H) has only one PSU at stage 1",
      "Stratum (M) has only one PSU at stage 1"
    )
  )
  expect_true(all(ref$n > 1L))
  run <- .svy_collect_warnings(table_battery_svy(
    d,
    select = c(awards, comp.imp, both),
    by = grp,
    output = "long"
  ))
  out <- run$value
  # One warning per message, in the order survey raised them, each with
  # its count, survey's classes and survey's call.
  ws <- run$warnings
  expect_identical(
    vapply(ws, conditionMessage, character(1)),
    unname(mapply(.svy_counted, ref$msg, ref$n))
  )
  for (w in ws) {
    expect_identical(class(w), c("simpleWarning", "warning", "condition"))
  }
  expect_identical(conditionCall(ws[[1L]]), conditionCall(ref$first))
  # The percentages are survey's.
  for (g in c("B", "C")) {
    for (x in items) {
      expect_equal(
        out$pct[out$group == g & out$variable == x],
        100 * ref$est[[paste(g, x)]],
        tolerance = 1e-12
      )
    }
  }
})

test_that("the dash sentence sits with the design sentences", {
  skip_on_cran()
  d <- .svy_lonely_design()
  withr::local_options(
    survey.lonely.psu = "fail",
    survey.adjust.domain.lonely = FALSE
  )
  d$variables$sch.wide[2L] <- NA
  note <- attr(
    suppressWarnings(table_battery_svy(
      d,
      select = c(awards, comp.imp),
      by = sch.wide
    )),
    "note"
  )
  pos <- .svy_note_positions(
    note,
    c("Design: ", "Base: ", .SVY_LONELY_EN, "Missing values removed")
  )
  expect_true(all(pos > 0L))
  expect_false(is.unsorted(pos))
})
