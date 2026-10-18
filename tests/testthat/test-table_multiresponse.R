# table_multiresponse(): "check all that apply" items (issue #10; ADR
# dev/decisions/2026-10-17-table-multiresponse.md).
#
# Oracles. The item statistics, X2_S and the Bonferroni p are published
# (Agresti & Liu 1999, p. 941). The Rao-Scott and bootstrap values were
# pinned once from MRCV 0.4-0, installed in a temporary library on
# 2026-10-17 (MRCV is not a dependency):
#   MI.test(farmer1, I = 1, J = 5, type = "rs2", print.status = FALSE)
#   MI.test(farmer1, I = 1, J = 5, type = "bon", print.status = FALSE)
#   set.seed(20261017); MI.test(..., type = "boot", B = 1999)
# The farmer data have no empty item-by-group cell, so MRCV's 0.5
# substitution does not touch the item statistics or the Rao-Scott test.

# The Kansas farmer data of Loughin & Scherer (1998), Table 2 (p. 634):
# 262 respondents, 5 education groups, 5 sources of information (A-E), one
# count per response pattern. Rebuilt here from the paper; checked once
# against MRCV::farmer1, cell by cell (155 cells, 0 differences).
.mr_farmer <- function() {
  pats <- c(
    "A",
    "B",
    "C",
    "D",
    "E",
    "AB",
    "AC",
    "AD",
    "AE",
    "BC",
    "BD",
    "BE",
    "CD",
    "CE",
    "DE",
    "ABC",
    "ABD",
    "ABE",
    "ACD",
    "ACE",
    "ADE",
    "BCD",
    "BCE",
    "BDE",
    "CDE",
    "ABCD",
    "ABCE",
    "ABDE",
    "ACDE",
    "BCDE",
    "ABCDE"
  )
  t2 <- rbind(
    c(
      9,
      9,
      5,
      17,
      8,
      2,
      0,
      0,
      0,
      0,
      1,
      7,
      3,
      1,
      4,
      0,
      0,
      0,
      0,
      0,
      0,
      1,
      0,
      2,
      3,
      1,
      0,
      0,
      0,
      8,
      7
    ),
    c(
      0,
      3,
      4,
      3,
      1,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      0,
      0,
      0,
      1,
      0,
      0,
      0,
      2,
      0
    ),
    c(
      0,
      4,
      2,
      6,
      6,
      0,
      1,
      0,
      0,
      0,
      0,
      1,
      1,
      0,
      2,
      0,
      0,
      0,
      0,
      0,
      0,
      3,
      0,
      2,
      0,
      0,
      0,
      0,
      0,
      3,
      0
    ),
    c(
      11,
      11,
      17,
      30,
      16,
      0,
      0,
      0,
      0,
      1,
      1,
      2,
      6,
      1,
      2,
      1,
      0,
      0,
      1,
      0,
      0,
      3,
      0,
      0,
      0,
      2,
      0,
      0,
      0,
      4,
      4
    ),
    c(
      1,
      1,
      2,
      1,
      1,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      1,
      1,
      1,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      1,
      0,
      1,
      0,
      0,
      0,
      0,
      0,
      1
    )
  )
  rows <- list()
  for (i in seq_len(nrow(t2))) {
    for (k in seq_along(pats)) {
      if (t2[i, k] > 0) {
        v <- as.integer(LETTERS[1:5] %in% strsplit(pats[[k]], "")[[1L]])
        rows[[length(rows) + 1L]] <- matrix(
          rep(c(i, v), t2[i, k]),
          ncol = 6L,
          byrow = TRUE
        )
      }
    }
  }
  m <- do.call(rbind, rows)
  data.frame(
    ed = factor(m[, 1L], 1:5, c("HS", "VS", "2YC", "4YC", "Other")),
    A = m[, 2L],
    B = m[, 3L],
    C = m[, 4L],
    D = m[, 5L],
    E = m[, 6L]
  )
}

.mr_farm_tbl <- function(...) {
  table_multiresponse(.mr_farmer(), A:E, by = ed, ...)
}

# MRCV 0.4-0, pinned (see the header).
.mrcv_x2_items <- c(
  5.95958465074,
  7.89320991824,
  4.61569938054,
  1.41875609403,
  10.9486891432
)
.mrcv_bonferroni_items <- c(1, 0.477846574308, 1, 1, 0.135733852005)
.mrcv_rs2 <- c(
  statistic = 28.6789544908,
  df = 18.6009930277,
  p = 0.0632233439735
)
.mrcv_boot_p <- 0.0530265132566

# A small constructed question: four items, two groups, one respondent who
# skipped the question, one with a missing group.
.mr_toy <- function() {
  data.frame(
    g = factor(c("a", "a", "a", "b", "b", "b", "b", NA)),
    x1 = c(1, 0, 1, 0, 0, NA, 1, 1),
    x2 = c(0, 0, NA, 1, 1, NA, 1, 0),
    x3 = c(0, 0, 0, 0, 0, NA, 0, 0),
    x4 = c(1, 1, 1, 1, 1, NA, 1, 1)
  )
}

# ---- oracles: the farmer data ------------------------------------------------

test_that("the item chi-squares reproduce Agresti & Liu (1999) and MRCV", {
  # `test = "chisq"`: the published values are the Pearson statistics of
  # every item, three of which take Fisher's exact test under "auto".
  out <- .mr_farm_tbl(test = "chisq", output = "data.frame")
  items <- out[seq_len(5L), ]
  # A&L 1999, p. 941.
  expect_identical(round(items$Chi2, 2), c(5.96, 7.89, 4.62, 1.42, 10.95))
  expect_identical(round(sum(items$Chi2), 2), 30.84)
  expect_equal(items$Chi2, .mrcv_x2_items, tolerance = 1e-3)
  expect_identical(items$df, rep(4, 5L))
  # The arithmetic of chisq.test(correct = FALSE), item by item.
  farm <- .mr_farmer()
  for (j in seq_along(LETTERS[1:5])) {
    ref <- suppressWarnings(stats::chisq.test(
      table(farm$ed, farm[[LETTERS[[j]]]]),
      correct = FALSE
    ))
    expect_equal(items$Chi2[[j]], unname(ref$statistic), tolerance = 1e-12)
  }
})

test_that("the Bonferroni p-values reproduce the published global 0.136", {
  out <- .mr_farm_tbl(
    test = "chisq",
    p_adjust = "bonferroni",
    output = "data.frame"
  )
  p <- out$p[seq_len(5L)]
  expect_equal(p, .mrcv_bonferroni_items, tolerance = 1e-3)
  # A&L 1999, p. 941: the overall Bonferroni p.
  expect_identical(round(min(p), 3), 0.136)
})

test_that("the Rao-Scott global test matches MRCV", {
  tbl <- .mr_farm_tbl()
  g <- attr(tbl, "global_test")
  expect_identical(g$test, "rs2")
  expect_equal(g$statistic, .mrcv_rs2[["statistic"]], tolerance = 1e-3)
  expect_equal(g$df, .mrcv_rs2[["df"]], tolerance = 1e-3)
  expect_equal(g$p.value, .mrcv_rs2[["p"]], tolerance = 1e-3)
  expect_identical(g$n_items, 5L)
  expect_identical(g$n_groups, 5L)
  # The note names the test and carries its statistic.
  chi <- "\u03c7\u00b2"
  expect_match(
    attr(tbl, "note"),
    paste0(
      "Rao-Scott second-order",
      " corrected chi-squared test of multiple marginal independence, ",
      chi,
      "(18.60) = 28.68, p = .063."
    ),
    fixed = TRUE
  )
})

test_that("Holm is the default adjustment, and the raw p-values stay", {
  out <- .mr_farm_tbl(test = "chisq", output = "data.frame")
  items <- out[seq_len(5L), ]
  raw <- stats::pchisq(items$Chi2, 4, lower.tail = FALSE)
  expect_equal(items$p_unadjusted, raw)
  expect_equal(items$p, stats::p.adjust(raw, "holm"))
  # The smallest Holm p is the Bonferroni global p.
  expect_equal(min(items$p), .mrcv_bonferroni_items[[5L]], tolerance = 1e-3)
  # The family's sentence, after the item test: method and number of tests.
  expect_match(
    attr(out, "note"),
    paste(
      "Item p-values: Pearson chi-squared test of each item by group.",
      "P-values adjusted via stats::p.adjust(method = \"holm\"); m = 5 test(s)."
    ),
    fixed = TRUE
  )
  # The family is the items tested: an item without variation is not one.
  toy <- table_multiresponse(.mr_toy(), c(x1, x2, x3, x4), by = g)
  expect_match(
    attr(toy, "note"),
    "m = 2 test(s). Not tested (no variation, shown as \u2013): x3, x4.",
    fixed = TRUE
  )
  s <- as_structured(.mr_farm_tbl(test = "chisq"))
  expect_identical(s$col_meta$p$p_adjust, "holm")
  expect_identical(s$col_meta$p$p_adjust_m, 5L)
  expect_equal(s$col_meta$p$p_unadjusted[seq_len(5L)], raw)
})

test_that("p_adjust = \"none\" leaves the item p-values raw and says so", {
  out <- .mr_farm_tbl(test = "chisq", p_adjust = "none", output = "data.frame")
  expect_false("p_unadjusted" %in% names(out))
  expect_equal(
    out$p[seq_len(5L)],
    stats::pchisq(out$Chi2[seq_len(5L)], 4, lower.tail = FALSE)
  )
  expect_match(
    attr(out, "note"),
    "each item by group. Group comparison:",
    fixed = TRUE
  )
  expect_false(grepl("adjusted via", attr(out, "note"), fixed = TRUE))
  # Without p-values there is nothing to adjust, and nothing is said.
  expect_false(grepl(
    "adjusted via",
    attr(.mr_farm_tbl(p_value = FALSE, output = "data.frame"), "note"),
    fixed = TRUE
  ))
})

test_that("the bootstrap is reproducible and agrees with MRCV within Monte Carlo error", {
  # The item p-values of the default `test = "auto"` take Fisher's exact
  # test for three items: exact, so they draw nothing from the RNG and the
  # bootstrap draws the resamples it drew before they existed.
  set.seed(20261017)
  a <- attr(
    .mr_farm_tbl(global_test = "bootstrap", output = "data.frame"),
    "global_test"
  )
  set.seed(20261017)
  b <- attr(
    .mr_farm_tbl(global_test = "bootstrap", output = "data.frame"),
    "global_test"
  )
  expect_identical(a, b)
  expect_identical(a$test, "bootstrap")
  expect_identical(a$boot_n, 1999L)
  expect_true(is.na(a$df))
  expect_equal(a$statistic, sum(.mrcv_x2_items), tolerance = 1e-6)
  # This row order and seed: 129 resamples of 1999 at least as large. The
  # farmer data never yield a resample in which an item loses all
  # variation, so nothing is drawn again and the pin holds.
  expect_equal(a$p.value, 129 / 1999)
  expect_identical(a$boot_redraws, 0L)
  # MRCV draws other resamples (another row order of the same data); the
  # two estimates differ by less than three standard errors of their
  # difference.
  se_diff <- sqrt(2 * .mrcv_boot_p * (1 - .mrcv_boot_p) / 1999)
  expect_lt(abs(a$p.value - .mrcv_boot_p), 3 * se_diff)
  set.seed(1)
  tbl <- .mr_farm_tbl(global_test = "bootstrap", boot_n = 99)
  expect_match(attr(tbl, "note"), "(99 resamples)", fixed = TRUE)
})

test_that("the bootstrap draws again a resample in which an item loses all variation", {
  # x is chosen once in 20, so about a third of the resamples leave it
  # unchosen: each is drawn again, and the p-value still rests on the
  # `boot_n` resamples kept.
  d <- data.frame(
    g = rep(c("a", "b"), each = 10L),
    x = c(1, rep(0, 19L)),
    y = rep(c(1, 0), 10L)
  )
  boot <- function() {
    attr(
      table_multiresponse(
        d,
        c(x, y),
        by = g,
        global_test = "bootstrap",
        boot_n = 99,
        output = "data.frame"
      ),
      "global_test"
    )
  }
  set.seed(1)
  a <- boot()
  # This seed: 66 resamples drawn again on the way to 99.
  expect_identical(a$boot_redraws, 66L)
  expect_identical(a$boot_n, 99L)
  expect_identical(a$p.value * 99, round(a$p.value * 99))
  set.seed(1)
  expect_identical(boot(), a)
  # The draws are capped: running out is an error, never a p-value from
  # fewer resamples.
  z <- as.matrix(d[, c("x", "y")])
  storage.mode(z) <- "double"
  set.seed(1)
  expect_error(
    spicy:::.mr_boot(z, c(10, 10), 1, boot_n = 99, max_draws = 99),
    class = "spicy_resampling_failed"
  )
})

test_that("the Rao-Scott design-effect summary: rs_mean and rs_cv", {
  g <- attr(.mr_farm_tbl(), "global_test")
  # rs_cv is the a-hat of Thomas & Decady (2004, p. 59), 1 + a^2 =
  # sum(lambda^2) / k, with sum(lambda^2) = k^2 / df from the MRCV pin:
  # 21.5042282638 for k = 20.
  expect_equal(g$rs_cv, sqrt(21.5042282638 / 20 - 1), tolerance = 1e-9)
  expect_equal(g$rs_cv, sqrt(20 / .mrcv_rs2[["df"]] - 1), tolerance = 1e-6)
  # rs_mean is tr(M) / k, in closed form
  # sum_i (1 - a_i) sum_j p_ij (1 - p_ij) / (pbar_j (1 - pbar_j)) / k:
  # 0.969 on these data, where the correction takes 1.
  out <- .mr_farm_tbl(output = "data.frame")
  grp <- c("HS", "VS", "2YC", "4YC", "Other")
  base <- unlist(out[6L, paste(grp, "n")])
  p <- t(as.matrix(out[1:5, paste(grp, "n")])) / base
  pbar <- out[["Total n"]][1:5] / out[["Total n"]][[6L]]
  a <- base / sum(base)
  expect_equal(
    g$rs_mean,
    sum((1 - a) * colSums(t(p * (1 - p)) / (pbar * (1 - pbar)))) / 20,
    tolerance = 1e-12
  )
  expect_true(is.na(g$boot_redraws))
  # No real a-hat when sum(lambda^2) / k falls below 1: one item whose
  # groups answer 0.1 and 0.9, so lambda = 0.36.
  e <- data.frame(
    g = rep(c("a", "b"), each = 10L),
    x = c(1, rep(0, 9L), rep(1, 9L), 0)
  )
  ge <- attr(table_multiresponse(e, x, by = g), "global_test")
  expect_equal(ge$rs_mean, 0.36)
  expect_true(is.na(ge$rs_cv))
  # The bootstrap has no Rao-Scott summary.
  set.seed(1)
  gb <- attr(
    .mr_farm_tbl(global_test = "bootstrap", boot_n = 99),
    "global_test"
  )
  expect_true(is.na(gb$rs_mean))
  expect_true(is.na(gb$rs_cv))
  skip_if_not_installed("broom")
  expect_identical(broom::glance(.mr_farm_tbl())$rs_cv, g$rs_cv)
})

test_that("the Rao-Scott test draws nothing from the session's RNG", {
  set.seed(1)
  .mr_farm_tbl()
  after <- stats::runif(1L)
  set.seed(1)
  expect_identical(stats::runif(1L), after)
})

# ---- the item tests: `test` (decision of 2026-10-18) ----------------------------

# The farmer items by education: A, B and E have an expected count below 5
# in the small groups (VS, 16 respondents; Other, 14), C and D do not.
.mr_farm_types_auto <- c(
  "fisher_exact",
  "fisher_exact",
  "chi_squared",
  "chi_squared",
  "fisher_exact"
)

test_that("test = \"auto\" takes Fisher's exact test where an expected count is below 5", {
  farm <- .mr_farmer()
  out <- .mr_farm_tbl(output = "data.frame")
  items <- out[seq_len(5L), ]
  expect_identical(items$test_type, .mr_farm_types_auto)
  for (j in seq_len(5L)) {
    tab <- table(farm$ed, farm[[LETTERS[[j]]]])
    expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
    fisher <- identical(.mr_farm_types_auto[[j]], "fisher_exact")
    expect_identical(any(expected < 5), fisher, info = LETTERS[[j]])
    ref <- if (fisher) {
      stats::fisher.test(tab)$p.value
    } else {
      stats::chisq.test(tab, correct = FALSE)$p.value
    }
    expect_equal(items$p_unadjusted[[j]], ref, tolerance = 1e-12)
    # A Fisher item has no statistic and no degrees of freedom.
    expect_identical(is.na(items$Chi2[[j]]), fisher)
    expect_identical(is.na(items$df[[j]]), fisher)
  }
  # Holm runs on the p-values the tests produced.
  expect_equal(items$p, stats::p.adjust(items$p_unadjusted, "holm"))
  expect_match(
    attr(out, "note"),
    paste(
      "Item p-values: Pearson chi-squared test (C, D); Fisher exact test",
      "(A, B, E). P-values adjusted via stats::p.adjust(method = \"holm\");",
      "m = 5 test(s)."
    ),
    fixed = TRUE
  )
  long <- .mr_farm_tbl(output = "long")
  expect_identical(
    long$test_type[!duplicated(long$variable)],
    .mr_farm_types_auto
  )
  s <- as_structured(.mr_farm_tbl())
  expect_identical(s$col_meta$p$test_type, c(.mr_farm_types_auto, NA))
})

test_that("test = \"chisq\" and \"fisher\" take one test for every item", {
  farm <- .mr_farmer()
  chisq <- .mr_farm_tbl(test = "chisq", output = "data.frame")
  fisher <- .mr_farm_tbl(test = "fisher", output = "data.frame")
  expect_identical(chisq$test_type[1:5], rep("chi_squared", 5L))
  expect_identical(fisher$test_type[1:5], rep("fisher_exact", 5L))
  expect_true(all(is.na(fisher$Chi2)) && all(is.na(fisher$df)))
  for (j in seq_len(5L)) {
    tab <- table(farm$ed, farm[[LETTERS[[j]]]])
    expect_equal(
      fisher$p_unadjusted[[j]],
      stats::fisher.test(tab)$p.value,
      tolerance = 1e-12
    )
    expect_equal(
      chisq$p_unadjusted[[j]],
      stats::pchisq(.mrcv_x2_items[[j]], 4, lower.tail = FALSE),
      tolerance = 1e-6
    )
  }
  expect_equal(fisher$p[1:5], stats::p.adjust(fisher$p_unadjusted[1:5], "holm"))
  expect_match(
    attr(fisher, "note"),
    "Item p-values: Fisher exact test of each item by group.",
    fixed = TRUE
  )
})

test_that("the global test sums the Pearson statistics whatever `test` says", {
  ref <- attr(.mr_farm_tbl(test = "chisq"), "global_test")
  expect_equal(ref$statistic, .mrcv_rs2[["statistic"]], tolerance = 1e-3)
  expect_equal(ref$df, .mrcv_rs2[["df"]], tolerance = 1e-3)
  expect_equal(ref$p.value, .mrcv_rs2[["p"]], tolerance = 1e-3)
  expect_identical(attr(.mr_farm_tbl(), "global_test"), ref)
  expect_identical(attr(.mr_farm_tbl(test = "fisher"), "global_test"), ref)
  # The bootstrap refers the same sum, X2_S = 30.84, to its resamples.
  set.seed(1)
  gb <- attr(
    .mr_farm_tbl(test = "fisher", global_test = "bootstrap", boot_n = 99),
    "global_test"
  )
  expect_equal(gb$statistic, sum(.mrcv_x2_items), tolerance = 1e-6)
})

test_that("an item tested by Fisher's exact test shows a dash for its statistic", {
  tbl <- .mr_farm_tbl(statistic = TRUE)
  disp <- attr(tbl, "display_df")
  dash <- spicy_str("cell_undefined")
  fisher <- .mr_farm_types_auto == "fisher_exact"
  expect_identical(disp$Test[1:5][fisher], rep(dash, 3L))
  expect_identical(
    disp$Test[[3L]],
    paste0(spicy_str("symbol_chi_sq"), "(4) = 4.62")
  )
  s <- as_structured(tbl)
  expect_identical(s$cell_status$Test[1:5][fisher], rep("undefined", 3L))
  expect_true(all(is.na(s$body$Test[1:5][fisher])))
  # inline() refuses an undefined cell, as it does in every family: it
  # never pastes the placeholder into a sentence.
  expect_error(
    inline(tbl, A, column = "statistic"),
    class = "spicy_invalid_input"
  )
  expect_identical(inline(tbl, C, column = "statistic"), disp$Test[[3L]])
  # Without p-values `test` has nothing to choose: the statistic is the
  # chi-squared of every item.
  expect_warning(
    out <- .mr_farm_tbl(statistic = TRUE, p_value = FALSE, test = "fisher"),
    "`test` is ignored when `p_value = FALSE`",
    class = "spicy_ignored_arg"
  )
  expect_false(dash %in% attr(out, "display_df")$Test[1:5])
  skip_if_not_installed("broom")
  td <- broom::tidy(tbl)
  expect_identical(unique(td$test_type[td$variable == "A"]), "fisher_exact")
  expect_true(all(is.na(td$statistic[td$variable == "A"])))
  expect_identical(unique(td$test_type[td$variable == "C"]), "chi_squared")
})

test_that("an item table the exact enumeration cannot handle takes the Monte Carlo test", {
  # 20 groups of 200, an item chosen 3 to 8 times per group: FEXACT
  # overflows on this 20 x 2 table.
  k <- c(6, 5, 7, 4, 6, 8, 5, 6, 7, 3, 6, 5, 4, 7, 6, 5, 8, 6, 5, 4)
  d <- data.frame(
    g = factor(rep(sprintf("g%02d", 1:20), each = 200L)),
    x = unlist(lapply(k, function(m) c(rep(1, m), rep(0, 200 - m)))),
    y = rep(c(1, 0), 2000L)
  )
  tab <- cbind(k, 200 - k)
  expect_error(stats::fisher.test(tab))
  set.seed(7)
  out <- table_multiresponse(
    d,
    c(x, y),
    by = g,
    test = "fisher",
    output = "data.frame"
  )
  expect_identical(out$test_type[1:2], c("fisher_exact_mc", "fisher_exact"))
  set.seed(7)
  expect_identical(
    out$p_unadjusted[[1L]],
    stats::fisher.test(tab, simulate.p.value = TRUE, B = 2000)$p.value
  )
  expect_match(
    attr(out, "note"),
    paste(
      "Item p-values: Fisher exact test (y); Fisher exact test",
      "(Monte Carlo, 2000 replicates) (x)."
    ),
    fixed = TRUE
  )
})

test_that("with no item tested, the note names no item test", {
  d <- data.frame(
    g = rep(c("a", "b"), each = 3L),
    x = rep(1, 6L),
    y = rep(0, 6L)
  )
  tbl <- table_multiresponse(d, c(x, y), by = g)
  note <- attr(tbl, "note")
  expect_false(grepl("Item p-values", note, fixed = TRUE))
  expect_false(grepl("adjusted via", note, fixed = TRUE))
  expect_match(note, "Not tested (no variation", fixed = TRUE)
  expect_true(all(is.na(tbl$test_type)))
  expect_null(attr(tbl, "global_test"))
})

test_that("the item note names the tests in French", {
  withr::local_options(spicy.language = "fr")
  expect_match(
    attr(.mr_farm_tbl(), "note"),
    paste0(
      "Valeurs p des items\u00a0: test du khi-deux de Pearson (C, D)\u00a0; ",
      "test exact de Fisher (A, B, E)."
    ),
    fixed = TRUE
  )
  expect_match(
    attr(.mr_farm_tbl(test = "fisher"), "note"),
    "Valeurs p des items\u00a0: test exact de Fisher de chaque item selon le groupe.",
    fixed = TRUE
  )
})

test_that("a bootstrap p of zero is reported at the resolution of the resamples", {
  d <- data.frame(
    g = rep(c("a", "b"), each = 20L),
    x = rep(c(1, 0), each = 20L),
    y = rep(c(0, 1, 1, 0), 10L)
  )
  set.seed(3)
  tbl <- table_multiresponse(
    d,
    c(x, y),
    by = g,
    global_test = "bootstrap",
    boot_n = 99
  )
  expect_identical(attr(tbl, "global_test")$p.value, 0)
  expect_match(attr(tbl, "note"), "p < .011.", fixed = TRUE)
})

# ---- reading the indicators -------------------------------------------------------

test_that("logical and 0/1 columns are read as they are", {
  d <- data.frame(
    a = c(TRUE, FALSE, TRUE, NA),
    b = c(1L, 0L, 0L, 1L),
    c = c(0, 0, 1, NA)
  )
  out <- table_multiresponse(d, c(a, b, c), output = "data.frame")
  expect_identical(out$n, c(2, 2, 1, 4))
  expect_named(out, c("Variable", "n", "%"))
})

test_that("other columns need `counted`, which applies to every column", {
  d <- data.frame(
    q1 = c("Oui", "Non", NA, "Oui"),
    q2 = factor(c("Non", "Non", "Oui", "Oui"), levels = c("Non", "Oui")),
    stringsAsFactors = FALSE
  )
  expect_error(
    table_multiresponse(d, c(q1, q2)),
    class = "spicy_invalid_input"
  )
  out <- table_multiresponse(
    d,
    c(q1, q2),
    counted = "Oui",
    output = "data.frame"
  )
  expect_identical(out$n, c(2, 2, 4))
  # A numeric column coded 1/2 is not 0/1 either.
  d2 <- data.frame(z = c(1, 2, 2))
  expect_error(table_multiresponse(d2, z), class = "spicy_invalid_input")
  expect_identical(
    table_multiresponse(d2, z, counted = 2, output = "data.frame")$n,
    c(2, 3)
  )
})

test_that("a `counted` that matches nothing, or is not one value, is an error", {
  d <- data.frame(q1 = c("Oui", "Non"), q2 = c("Non", "Non"))
  expect_error(
    table_multiresponse(d, c(q1, q2), counted = "Yes"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_multiresponse(d, c(q1, q2), counted = c("Oui", "Non")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_multiresponse(d, c(q1, q2), counted = NA),
    class = "spicy_invalid_input"
  )
  # A declared level nobody chose is a known value, not a typo.
  f <- data.frame(q = factor(c("Non", "Non"), levels = c("Non", "Oui")))
  expect_identical(
    table_multiresponse(f, q, counted = "Oui", output = "data.frame")$n,
    c(0, 2)
  )
})

test_that("a labelled column takes a code or a value label, and its declared missing values are missing", {
  skip_if_not_installed("haven")
  d <- data.frame(g = c(1, 1, 2, 2, 2))
  d$y1 <- haven::labelled_spss(
    c(1, 0, 9, 1, 0),
    c(Oui = 1, Non = 0, NSP = 9),
    na_values = 9
  )
  d$y2 <- haven::labelled(c(0, 1, 1, 1, 0), c(Oui = 1, Non = 0))
  expect_error(table_multiresponse(d, c(y1, y2)), class = "spicy_invalid_input")
  by_label <- table_multiresponse(
    d,
    c(y1, y2),
    by = g,
    counted = "Oui",
    output = "data.frame"
  )
  by_code <- table_multiresponse(
    d,
    c(y1, y2),
    by = g,
    counted = 1,
    output = "data.frame"
  )
  expect_identical(by_label, by_code)
  expect_identical(by_label[["1 n"]], c(1, 1, 2))
  expect_identical(by_label[["2 n"]], c(1, 2, 3))
  # The declared 9 is missing, never a "not chosen" answer: asked about y1
  # alone, the respondent whose only answer is the 9 skipped the question
  # and leaves the base.
  long <- table_multiresponse(d, y1, by = g, counted = "Oui", output = "long")
  expect_identical(long$base, c(2, 2, 4))
})

# ---- base, percentages, order ---------------------------------------------------

test_that("base = \"valid\" leaves out the respondent who skipped the question", {
  d <- .mr_toy()
  valid <- table_multiresponse(d, c(x1, x2), by = g, output = "long")
  all <- table_multiresponse(
    d,
    c(x1, x2),
    by = g,
    base = "all",
    output = "long"
  )
  expect_identical(valid$base[1:3], c(3, 3, 6))
  expect_identical(all$base[1:3], c(3, 4, 7))
  expect_equal(valid$pct, 100 * valid$n / valid$base)
  expect_equal(all$pct, 100 * all$n / all$base)
  expect_match(
    attr(all, "note"),
    "Base: all respondents.",
    fixed = TRUE
  )
  expect_match(
    attr(valid, "note"),
    "Base: respondents who answered the question, including those who chose no item.",
    fixed = TRUE
  )
})

test_that("base = \"answered\" keeps the respondents who chose an item", {
  # Row 2 chose nothing, row 3 skipped the question (every indicator
  # missing), row 5 chose nothing with one indicator missing.
  d <- data.frame(
    g = c("a", "a", "a", "b", "b", "b"),
    x1 = c(1, 0, NA, 1, 0, 1),
    x2 = c(0, 0, NA, 1, NA, 0)
  )
  long <- function(base) {
    table_multiresponse(d, c(x1, x2), by = g, base = base, output = "long")
  }
  valid <- long("valid")
  answered <- long("answered")
  all <- long("all")
  # The counts do not move; the bases do.
  expect_identical(answered$n, valid$n)
  expect_identical(all$n, valid$n)
  expect_identical(valid$n, c(1, 2, 3, 0, 1, 1))
  expect_identical(valid$base[1:3], c(2, 3, 5))
  expect_identical(answered$base[1:3], c(1, 2, 3))
  expect_identical(all$base[1:3], c(3, 3, 6))
  expect_equal(answered$pct, 100 * answered$n / answered$base)
  expect_match(
    attr(answered, "note"),
    "Base: respondents who chose at least one item.",
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    attr(long("answered"), "note"),
    "Base\u00a0: personnes ayant coch\u00e9 au moins un item.",
    fixed = TRUE
  )
})

# Row 2 chose nothing, row 3 skipped the question (every indicator
# missing), row 5 chose nothing with one indicator missing, and row 7, which
# chose nothing, has no group.
.mr_none <- function() {
  data.frame(
    g = c("a", "a", "a", "b", "b", "b", NA),
    x1 = c(1, 0, NA, 1, 0, 1, 0),
    x2 = c(0, 0, NA, 1, NA, 0, 0)
  )
}

.mr_none_note <- function(data = .mr_none(), ...) {
  attr(
    table_multiresponse(data, c(x1, x2), output = "data.frame", ...),
    "note"
  )
}

test_that("the note counts the respondents of the base who chose no item", {
  # valid: rows 2, 5 and 7, after the base sentence; the skipped row 3 is
  # outside the base, and the note opens with it.
  expect_identical(
    .mr_none_note(),
    paste(
      "Respondents with no answer to the question removed: 1.",
      "Several answers possible: percentages are of respondents and can sum to more than 100%.",
      "Base: respondents who answered the question, including those who chose no item.",
      "3 respondents chose none of the items."
    )
  )
  # all: row 3 joins them.
  expect_match(
    .mr_none_note(base = "all"),
    "Base: all respondents. 4 respondents chose none of the items.",
    fixed = TRUE
  )
  # answered: they are outside the base, so the sentence never appears.
  for (by_g in c(FALSE, TRUE)) {
    note <- if (by_g) {
      .mr_none_note(by = g, base = "answered")
    } else {
      .mr_none_note(base = "answered")
    }
    expect_false(grepl("chose none", note, fixed = TRUE))
  }
  # With `by`: one count over the whole base, all groups together (row 7
  # leaves with its missing group), before the tests.
  expect_match(
    .mr_none_note(by = g),
    paste(
      "including those who chose no item.",
      "2 respondents chose none of the items. Item p-values:"
    ),
    fixed = TRUE
  )
  expect_match(
    .mr_none_note(by = g, base = "all"),
    "Base: all respondents. 3 respondents chose none of the items.",
    fixed = TRUE
  )
  # The table and its data frames carry the same note.
  expect_identical(
    attr(table_multiresponse(.mr_none(), c(x1, x2), by = g), "note"),
    .mr_none_note(by = g)
  )
})

test_that("one respondent who chose no item has a sentence of its own, none has none", {
  d <- .mr_none()
  expect_match(
    .mr_none_note(d[c(1L, 2L, 4L), ]),
    "chose no item. 1 respondent chose none of the items.",
    fixed = TRUE
  )
  expect_match(
    .mr_none_note(d[-5L, ], by = g),
    "chose no item. 1 respondent chose none of the items. Item p-values:",
    fixed = TRUE
  )
  expect_false(grepl(
    "chose none",
    .mr_none_note(d[c(1L, 4L, 6L), ]),
    fixed = TRUE
  ))
  expect_false(grepl("chose none", attr(.mr_farm_tbl(), "note"), fixed = TRUE))
})

test_that("the French note counts them too", {
  withr::local_options(spicy.language = "fr")
  d <- .mr_none()
  expect_match(
    .mr_none_note(base = "all"),
    "Base\u00a0: toutes les personnes. 4 personnes n'ont coch\u00e9 aucun item.",
    fixed = TRUE
  )
  expect_match(
    .mr_none_note(d[-5L, ], by = g),
    "n'ont choisi aucun item. 1 personne n'a coch\u00e9 aucun item.",
    fixed = TRUE
  )
  expect_false(grepl(
    "aucun item",
    .mr_none_note(base = "answered"),
    fixed = TRUE
  ))
  expect_match(
    .mr_none_note(),
    "Personnes retir\u00e9es faute de r\u00e9ponse \u00e0 la question\u00a0: 1.",
    fixed = TRUE
  )
})

test_that("the note counts the respondents left outside the base", {
  # Row 3 answered none of the indicators, rows 2 and 5 chose no item, and
  # row 7, which chose no item, has no group.
  removed <- "Respondents with no answer to the question removed:"
  none <- "Respondents who chose no item removed:"
  expect_match(.mr_none_note(), paste(removed, "1."), fixed = TRUE)
  expect_false(grepl(none, .mr_none_note(), fixed = TRUE))
  # answered: the respondents who chose no item leave the base as well,
  # counted apart, since they did answer.
  expect_match(
    .mr_none_note(base = "answered"),
    paste(removed, "1.", none, "3."),
    fixed = TRUE
  )
  # all: nobody leaves it.
  expect_false(grepl(removed, .mr_none_note(base = "all"), fixed = TRUE))
  expect_false(grepl(none, .mr_none_note(base = "all"), fixed = TRUE))
  # With `by`: one count for the whole table, after the rows a missing
  # group removed, which counts row 7 instead.
  expect_match(
    .mr_none_note(by = g, base = "answered"),
    paste("Rows with missing g removed: 1.", removed, "1.", none, "2."),
    fixed = TRUE
  )
  expect_match(.mr_none_note(by = g), paste(removed, "1."), fixed = TRUE)
  # Nobody left out, no sentence.
  expect_false(grepl(removed, attr(.mr_farm_tbl(), "note"), fixed = TRUE))
})

test_that("a respondent who chose no item is not counted as one who skipped", {
  # Row 2 answered and chose no item, row 3 skipped the question. Only
  # `base = "answered"` leaves row 2 out, and says so in its own sentence.
  d <- data.frame(x1 = c(1, 0, NA, 1), x2 = c(0, 0, NA, 1))
  note <- function(base) {
    attr(
      table_multiresponse(d, c(x1, x2), base = base, output = "data.frame"),
      "note"
    )
  }
  cases <- "Several answers possible: percentages are of respondents and can sum to more than 100%."
  expect_identical(
    note("valid"),
    paste(
      "Respondents with no answer to the question removed: 1.",
      cases,
      "Base: respondents who answered the question, including those who chose no item.",
      "1 respondent chose none of the items."
    )
  )
  expect_identical(
    note("answered"),
    paste(
      "Respondents with no answer to the question removed: 1.",
      "Respondents who chose no item removed: 1.",
      cases,
      "Base: respondents who chose at least one item."
    )
  )
  expect_identical(
    note("all"),
    paste(
      cases,
      "Base: all respondents. 2 respondents chose none of the items."
    )
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    note("answered"),
    paste(
      "Personnes retir\u00e9es faute de r\u00e9ponse \u00e0 la question\u00a0: 1.",
      "Personnes retir\u00e9es faute d'item coch\u00e9\u00a0: 1."
    ),
    fixed = TRUE
  )
  expect_match(
    note("valid"),
    "^Personnes retir\u00e9es faute de r\u00e9ponse \u00e0 la question\u00a0: 1\\."
  )
  expect_false(grepl("faute d'item", note("valid"), fixed = TRUE))
  expect_false(grepl("retir\u00e9es", note("all"), fixed = TRUE))
})

test_that("the note says what the percentages are of, in both languages", {
  note <- function(percent) {
    attr(
      table_multiresponse(
        .mr_farmer(),
        A:E,
        percent = percent,
        output = "data.frame"
      ),
      "note"
    )
  }
  expect_match(
    note("cases"),
    "Several answers possible: percentages are of respondents and can sum to more than 100%.",
    fixed = TRUE
  )
  expect_match(
    note("responses"),
    "Several answers possible: percentages are of responses.",
    fixed = TRUE
  )
  expect_match(
    note("both"),
    "Several answers possible: percentages of respondents can sum to more than 100%; percentages of responses sum to 100%.",
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    note("cases"),
    "Plusieurs r\u00e9ponses possibles\u00a0: les pourcentages, calcul\u00e9s sur les personnes ayant r\u00e9pondu, peuvent totaliser plus de 100\u00a0%.",
    fixed = TRUE
  )
  expect_match(
    note("both"),
    "Plusieurs r\u00e9ponses possibles\u00a0: les pourcentages de personnes peuvent totaliser plus de 100\u00a0%\u00a0; les pourcentages de r\u00e9ponses totalisent 100\u00a0%.",
    fixed = TRUE
  )
  expect_match(
    .mr_none_note(),
    "Base\u00a0: personnes ayant r\u00e9pondu \u00e0 la question, y compris celles qui n'ont choisi aucun item.",
    fixed = TRUE
  )
})

test_that("percentages of cases, of responses, or both", {
  d <- .mr_toy()
  sel <- c("x1", "x2", "x4")
  cases <- table_multiresponse(d, sel, by = g, output = "data.frame")
  resp <- table_multiresponse(
    d,
    sel,
    by = g,
    percent = "responses",
    output = "data.frame"
  )
  both <- table_multiresponse(
    d,
    sel,
    by = g,
    percent = "both",
    output = "data.frame"
  )
  items <- seq_len(3L)
  # Percentages of cases sum past 100 here; of responses, to 100.
  expect_gt(sum(cases[["Total %"]][items]), 100)
  expect_equal(sum(resp[["Total %"]][items]), 100)
  expect_equal(sum(resp[["a %"]][items]), 100)
  expect_identical(both[["a % of cases"]], cases[["a %"]])
  expect_identical(both[["a % of responses"]], resp[["a %"]])
  expect_named(
    both,
    c(
      "Variable",
      "a n",
      "a % of cases",
      "a % of responses",
      "b n",
      "b % of cases",
      "b % of responses",
      "Total n",
      "Total % of cases",
      "Total % of responses",
      "test_type",
      "Chi2",
      "df",
      "p",
      "p_unadjusted"
    )
  )
  expect_named(
    table_multiresponse(d, sel, percent = "both", output = "long"),
    c("variable", "n", "base", "pct_cases", "pct_responses")
  )
  expect_match(
    attr(resp, "note"),
    "Several answers possible: percentages are of responses.",
    fixed = TRUE
  )
})

test_that("sort = \"-\" and \"+\" order the items by total count", {
  d <- data.frame(
    a = c(1, 0, 0, 0),
    b = c(1, 1, 1, 0),
    c = c(1, 1, 0, 0),
    e = c(0, 1, 0, 1)
  )
  q <- table_multiresponse(d, c(a, b, c, e), output = "data.frame")
  s <- table_multiresponse(
    d,
    c(a, b, c, e),
    sort = "-",
    output = "data.frame"
  )
  up <- table_multiresponse(
    d,
    c(a, b, c, e),
    sort = "+",
    output = "data.frame"
  )
  expect_identical(q$Variable, c("a", "b", "c", "e", "Respondents"))
  expect_identical(s$Variable, c("b", "c", "e", "a", "Respondents"))
  expect_identical(s$n, c(3, 2, 2, 1, 4))
  expect_identical(up$Variable, c("a", "c", "e", "b", "Respondents"))
  expect_identical(up$n, c(1, 2, 2, 3, 4))
  # With `by`, the total count decides, not the first group.
  g <- data.frame(
    grp = c("u", "u", "u", "v", "v", "v", "v"),
    x = c(1, 1, 1, 0, 0, 0, 0),
    y = c(0, 0, 0, 1, 1, 1, 1)
  )
  by_grp <- table_multiresponse(
    g,
    c(x, y),
    by = grp,
    sort = "-",
    p_value = FALSE,
    output = "data.frame"
  )
  expect_identical(by_grp$Variable, c("y", "x", "Respondents"))
})

test_that("items tied on their count keep the order of select", {
  # c and e are both chosen twice: written e before c, they stay so,
  # against their order in the data.
  d <- data.frame(
    a = c(1, 0, 0, 0),
    b = c(1, 1, 1, 0),
    c = c(1, 1, 0, 0),
    e = c(0, 1, 0, 1)
  )
  rows_of <- function(sort) {
    table_multiresponse(
      d,
      c(e, c, b, a),
      sort = sort,
      output = "data.frame"
    )$Variable
  }
  expect_identical(rows_of("-"), c("b", "e", "c", "a", "Respondents"))
  expect_identical(rows_of("+"), c("a", "e", "c", "b", "Respondents"))
})

test_that("sort = \"\" keeps the order in which select delivers the columns", {
  d <- data.frame(q_a = c(1, 0, 1), q_b = c(1, 1, 1), q_c = c(0, 0, 1))
  rows_of <- function(...) {
    table_multiresponse(d, ..., output = "data.frame")$Variable
  }
  expect_identical(rows_of(c(q_c, q_a)), c("q_c", "q_a", "Respondents"))
  expect_identical(
    rows_of(c(q_c, q_a), sort = ""),
    c("q_c", "q_a", "Respondents")
  )
  expect_identical(rows_of(c("q_c", "q_a")), c("q_c", "q_a", "Respondents"))
  expect_identical(
    rows_of(tidyselect::starts_with("q_")),
    c("q_a", "q_b", "q_c", "Respondents")
  )
})

test_that("sort = \"name+\" and \"name-\" order the labels shown, in the C locale", {
  d <- data.frame(work = c(1, 0), health = c(1, 1), standard = c(0, 1))
  rows_of <- function(...) {
    table_multiresponse(
      d,
      c(work, health, standard),
      ...,
      output = "data.frame"
    )$Variable
  }
  expect_identical(
    rows_of(sort = "name+"),
    c("health", "standard", "work", "Respondents")
  )
  expect_identical(
    rows_of(sort = "name-"),
    c("work", "standard", "health", "Respondents")
  )
  # The labels are sorted, not the names, in the C locale whatever the
  # session's: capitals before lower case, an accented letter after z.
  lab <- c(
    work = "Work",
    health = "\u00e9tat de sant\u00e9",
    standard = "standard"
  )
  withr::with_collate("en_US.UTF-8", {
    up_locale <- rows_of(labels = lab, sort = "name+")
  })
  up <- rows_of(labels = lab, sort = "name+")
  expect_identical(up_locale, up)
  expect_identical(
    up,
    c("Work", "standard", "\u00e9tat de sant\u00e9", "Respondents")
  )
  expect_identical(
    rows_of(labels = lab, sort = "name-"),
    c("\u00e9tat de sant\u00e9", "standard", "Work", "Respondents")
  )
})

test_that("the sort values of the first draft are errors, not aliases", {
  d <- data.frame(a = c(1, 0), b = c(1, 1))
  for (old in c("questionnaire", "desc")) {
    expect_error(
      table_multiresponse(d, c(a, b), sort = old),
      class = "spicy_invalid_input"
    )
  }
})

test_that("an item nobody chose keeps its row, at zero", {
  d <- .mr_toy()
  out <- table_multiresponse(d, c(x1, x3), by = g, output = "data.frame")
  expect_identical(out$Variable, c("x1", "x3", "Respondents"))
  expect_identical(
    unlist(out[2L, c("a n", "b n", "Total n")], use.names = FALSE),
    c(0, 0, 0)
  )
  expect_identical(out[["Total %"]][[2L]], 0)
})

test_that("an item without variation leaves the tests, with a note", {
  d <- .mr_toy()
  # `test = "chisq"` for the Holm oracle below; under "auto" the two
  # tested items take Fisher's exact test.
  tbl <- table_multiresponse(d, c(x1, x2, x3, x4), by = g, test = "chisq")
  expect_true(all(is.na(tbl$p[3:4])))
  expect_false(anyNA(tbl$p[1:2]))
  g <- attr(tbl, "global_test")
  expect_identical(g$n_items, 2L)
  expect_match(
    attr(tbl, "note"),
    "Not tested (no variation, shown as \u2013): x3, x4.",
    fixed = TRUE
  )
  # The note names the marker those cells show.
  expect_true(grepl(
    spicy_str("cell_undefined"),
    attr(tbl, "note"),
    fixed = TRUE
  ))
  # The p cell shows the undefined dash, typed as such.
  disp <- attr(tbl, "display_df")
  expect_identical(disp$p[3:4], rep("\u2013", 2L))
  s <- as_structured(tbl)
  expect_identical(s$cell_status$p[3:4], rep("undefined", 2L))
  # Holm runs on the two items tested.
  raw <- stats::pchisq(tbl$Chi2[1:2], 1, lower.tail = FALSE)
  expect_equal(tbl$p[1:2], stats::p.adjust(raw, "holm"))
  withr::local_options(spicy.language = "fr")
  expect_match(
    attr(table_multiresponse(d, c(x1, x2, x3, x4), by = "g"), "note"),
    "Non test\u00e9 (sans variation, signal\u00e9 par \u2013)\u00a0: x3, x4.",
    fixed = TRUE
  )
})

test_that("a `by` with one group has no test and no error", {
  d <- .mr_toy()
  d <- d[d$g %in% "a", ]
  expect_no_warning(out <- table_multiresponse(d, c(x1, x2), by = g))
  expect_false(any(c("Chi2", "df", "p") %in% names(out)))
  expect_null(attr(out, "global_test"))
  # The declared level with no respondent keeps its columns.
  expect_identical(out[["b n"]], c(0, 0, 0))
  expect_identical(attr(out, "display_df")[["b %"]][1:2], rep("\u2013", 2L))
  expect_warning(
    table_multiresponse(d, c(x1, x2), by = g, p_value = TRUE),
    class = "spicy_ignored_arg"
  )
})

test_that("rows with a missing `by` are removed, and the note says how many", {
  out <- table_multiresponse(
    .mr_toy(),
    c(x1, x2),
    by = g,
    output = "data.frame"
  )
  expect_identical(out[["Total n"]][[3L]], 6)
  expect_match(
    attr(out, "note"),
    "Rows with missing g removed: 1.",
    fixed = TRUE
  )
})

# ---- the test switch ---------------------------------------------------------------

test_that("p_value defaults to the tests with `by` and to none without", {
  ref <- .mr_farm_tbl()
  expect_true(all(c("Chi2", "df", "p") %in% names(ref)))
  expect_identical(.mr_farm_tbl(p_value = TRUE), ref)
  one_way <- table_multiresponse(.mr_farmer(), A:E)
  expect_false(any(c("Chi2", "df", "p") %in% names(one_way)))
  expect_warning(
    out <- table_multiresponse(.mr_farmer(), A:E, p_value = TRUE),
    class = "spicy_ignored_arg"
  )
  expect_identical(out, one_way)
})

test_that("p_value = FALSE: the data frames are the default minus the test", {
  ref <- .mr_farm_tbl(output = "data.frame")
  out <- .mr_farm_tbl(p_value = FALSE, output = "data.frame")
  expect_identical(
    names(out),
    setdiff(names(ref), c("test_type", "Chi2", "df", "p", "p_unadjusted"))
  )
  expect_identical(unclass(out)[names(out)], unclass(ref)[names(out)])
  expect_null(attr(out, "global_test"))
  ref_l <- .mr_farm_tbl(output = "long")
  out_l <- .mr_farm_tbl(p_value = FALSE, output = "long")
  expect_identical(names(out_l), c("variable", "group", "n", "base", "pct"))
  expect_identical(out_l$n, ref_l$n)
  expect_null(attr(out_l, "global_test"))
  # The note keeps what the table shows, and no test.
  note <- attr(out, "note")
  expect_false(grepl("p-values|Group comparison", note))
})

test_that("p_value = FALSE: the console table and its typed view", {
  tbl <- .mr_farm_tbl(p_value = FALSE, statistic = FALSE)
  expect_false(any(c("Chi2", "df", "p") %in% names(tbl)))
  disp <- attr(tbl, "display_df")
  expect_false(any(c("p", "Test") %in% names(disp)))
  s <- as_structured(tbl)
  expect_identical(unique(vapply(s$col_meta, `[[`, "", "token")), c("n", "pct"))
  printed <- utils::capture.output(print(tbl))
  header <- printed[grepl("Variable", printed, fixed = TRUE)][[1L]]
  expect_match(header, "HS n", fixed = TRUE)
  expect_false(grepl("\\bp\\b", header))
})

test_that("p_value = FALSE removes the test from every rendered engine", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("clipr")
  ref_tt <- .mr_farm_tbl(output = "tinytable")
  tt <- .mr_farm_tbl(p_value = FALSE, output = "tinytable")
  expect_identical(ncol(tt@data), ncol(ref_tt@data) - 1L)
  md <- strsplit(tinytable::save_tt(tt, output = "markdown"), "\n")[[1L]]
  top <- md[grepl("Variable", md, fixed = TRUE)][[1L]]
  expect_match(top, "HS", fixed = TRUE)
  expect_false(grepl("| p ", top, fixed = TRUE))

  g <- .mr_farm_tbl(p_value = FALSE, output = "gt")
  expect_false("p" %in% names(g[["_data"]]))
  expect_true("p" %in% names(.mr_farm_tbl(output = "gt")[["_data"]]))

  ft <- .mr_farm_tbl(p_value = FALSE, output = "flextable")
  expect_identical(
    ft$col_keys,
    setdiff(.mr_farm_tbl(output = "flextable")$col_keys, "p")
  )

  docx <- tempfile(fileext = ".docx")
  xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(docx, xlsx)), add = TRUE)
  .mr_farm_tbl(p_value = FALSE, output = "word", word_path = docx)
  cells <- officer::docx_summary(officer::read_docx(docx))
  cells <- cells[cells$content_type == "table cell", ]
  wd_top <- trimws(cells$text[cells$row_id == min(cells$row_id)])
  expect_false("p" %in% wd_top)
  expect_true(all(c("HS", "Total") %in% wd_top))

  .mr_farm_tbl(p_value = FALSE, output = "excel", excel_path = xlsx)
  grid <- as.matrix(openxlsx2::wb_to_df(
    openxlsx2::wb_load(xlsx),
    col_names = FALSE
  ))
  expect_false("p" %in% grid[3L, ])
  expect_identical(ncol(grid), 13L)

  clip <- NULL
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(x, ...) {
      clip <<- x
      invisible(NULL)
    },
    .package = "clipr"
  )
  suppressMessages(.mr_farm_tbl(p_value = FALSE, output = "clipboard"))
  lines <- strsplit(clip, "\n", fixed = TRUE)[[1L]]
  clip_top <- strsplit(lines[[2L]], "\t")[[1L]]
  expect_false("p" %in% clip_top)
  expect_true(all(c("HS", "Total") %in% clip_top))
  expect_false(any(grepl("Group comparison", lines, fixed = TRUE)))
})

test_that("the engines render the tested table, title and note included", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  tt <- .mr_farm_tbl(output = "tinytable")
  expect_identical(ncol(tt@data), 14L)
  g <- .mr_farm_tbl(output = "gt")
  expect_match(attr(g, "spicy_note"), "Rao-Scott", fixed = TRUE)
  expect_true(all(
    paste0("spn_", c("HS", "VS", "2YC", "4YC", "Other", "Total")) %in%
      g[["_spanners"]]$spanner_id
  ))
  ft <- .mr_farm_tbl(output = "flextable")
  expect_identical(ft$col_keys[c(1L, 14L)], c("Variable", "p"))
})

test_that("p_adjust typed without a test is an error, its default is not", {
  expect_error(
    .mr_farm_tbl(p_value = FALSE, p_adjust = "holm"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_multiresponse(.mr_farmer(), A:E, p_adjust = "BH"),
    class = "spicy_invalid_input"
  )
  expect_no_error(.mr_farm_tbl(p_value = FALSE))
  expect_no_error(.mr_farm_tbl(p_value = FALSE, p_adjust = "none"))
  expect_error(.mr_farm_tbl(p_adjust = "nope"), class = "spicy_invalid_input")
})

test_that("statistic = TRUE adds the item chi-squared column", {
  tbl <- .mr_farm_tbl(statistic = TRUE, test = "chisq")
  disp <- attr(tbl, "display_df")
  expect_identical(utils::tail(names(disp), 2L), c("Test", "p"))
  expect_identical(disp$Test[[1L]], "\u03c7\u00b2(4) = 5.96")
  expect_identical(disp$Test[[6L]], "")
  s <- as_structured(tbl)
  expect_identical(s$col_meta$Test$token, "statistic")
  expect_equal(s$body$Test[[5L]], .mrcv_x2_items[[5L]], tolerance = 1e-6)
  # `statistic` is independent of `p_value`, as in table_continuous().
  expect_no_warning(out <- .mr_farm_tbl(statistic = TRUE, p_value = FALSE))
  disp_out <- attr(out, "display_df")
  expect_identical(utils::tail(names(disp_out), 1L), "Test")
  expect_false("p" %in% names(disp_out))
  expect_identical(disp_out$Test, disp$Test)
  expect_null(attr(out, "global_test"))
  expect_false(grepl("p-values|Group comparison", attr(out, "note")))
  df <- .mr_farm_tbl(statistic = TRUE, p_value = FALSE, output = "data.frame")
  expect_true(all(c("Chi2", "df") %in% names(df)))
  expect_false(any(c("p", "p_unadjusted") %in% names(df)))
  long <- .mr_farm_tbl(statistic = TRUE, p_value = FALSE, output = "long")
  expect_identical(
    names(long),
    c("variable", "group", "n", "base", "pct", "test_type", "chi2", "df")
  )
  # The statistic alone draws nothing from the session's RNG, even under
  # global_test = "bootstrap": nobody would see the global test.
  set.seed(1)
  expect_warning(
    .mr_farm_tbl(statistic = TRUE, p_value = FALSE, global_test = "bootstrap"),
    class = "spicy_ignored_arg"
  )
  after <- stats::runif(1L)
  set.seed(1)
  expect_identical(stats::runif(1L), after)
})

test_that("statistic is ignored, with a warning, when no item test can run", {
  expect_warning(
    out <- table_multiresponse(.mr_farmer(), A:E, statistic = TRUE),
    class = "spicy_ignored_arg"
  )
  expect_false("Test" %in% names(attr(out, "display_df")))
  d <- .mr_toy()
  d <- d[d$g %in% "a", ]
  expect_warning(
    table_multiresponse(d, c(x1, x2), by = g, statistic = TRUE),
    class = "spicy_ignored_arg"
  )
})

test_that("arguments the table cannot use are ignored with a warning", {
  expect_warning(
    table_multiresponse(.mr_farmer(), A:E, global_test = "bootstrap"),
    class = "spicy_ignored_arg"
  )
  expect_warning(
    table_multiresponse(.mr_farmer(), A:E, test = "fisher"),
    class = "spicy_ignored_arg"
  )
  expect_warning(.mr_farm_tbl(boot_n = 99), class = "spicy_ignored_arg")
})

# ---- labels, language, typed view ----------------------------------------------

test_that("labels: names by default, the attribute with TRUE, a named vector, var_labels()", {
  d <- .mr_farmer()
  attr(d$A, "label") <- "Radio"
  attr(d$B, "label") <- "Television"
  attr(d$ed, "label") <- "Education"
  plain <- table_multiresponse(d, A:C, by = ed)
  expect_identical(plain$Variable[1:3], c("A", "B", "C"))
  expect_identical(attr(plain, "title"), "Multiple responses by ed")
  lab <- table_multiresponse(d, A:C, by = ed, labels = TRUE)
  expect_identical(lab$Variable[1:3], c("Radio", "Television", "C"))
  expect_identical(attr(lab, "title"), "Multiple responses by Education")
  vec <- table_multiresponse(d, A:C, by = ed, labels = c(C = "Newspapers"))
  expect_identical(vec$Variable[1:3], c("A", "B", "Newspapers"))
  vl <- table_multiresponse(
    d,
    A:C,
    by = ed,
    labels = var_labels(C = "Newspapers")
  )
  expect_identical(vl$Variable[1:3], c("Radio", "Television", "Newspapers"))
  expect_identical(attr(vl, "title"), "Multiple responses by Education")
  expect_error(
    table_multiresponse(d, A:C, labels = NULL),
    class = "spicy_invalid_input"
  )
})

test_that("title and note can be replaced, and the note removed", {
  tbl <- .mr_farm_tbl(title = "Sources of information", note = "Source: L&S.")
  expect_identical(attr(tbl, "title"), "Sources of information")
  expect_identical(attr(tbl, "note"), "Source: L&S.")
  expect_null(attr(.mr_farm_tbl(note = FALSE), "note"))
  expect_error(.mr_farm_tbl(title = ""), class = "spicy_invalid_input")
  expect_error(.mr_farm_tbl(note = TRUE), class = "spicy_invalid_input")
})

test_that("the French table translates its title, headers, base row and notes", {
  withr::local_options(spicy.language = "fr")
  tbl <- .mr_farm_tbl(percent = "both")
  expect_identical(attr(tbl, "title"), "R\u00e9ponses multiples selon ed")
  expect_identical(
    utils::tail(tbl$Variable, 1L),
    "Personnes ayant r\u00e9pondu"
  )
  s <- as_structured(tbl)
  expect_identical(s$col_meta[["HS % of cases"]]$display_label, "HS % des cas")
  expect_identical(
    s$col_meta[["HS % of responses"]]$display_label,
    "HS % des r\u00e9ponses"
  )
  note <- attr(tbl, "note")
  expect_match(note, "Plusieurs r\u00e9ponses possibles\u00a0:", fixed = TRUE)
  expect_match(
    note,
    "Valeurs p ajust\u00e9es par stats::p.adjust(method = \"holm\")\u00a0; m = 5 test(s).",
    fixed = TRUE
  )
  expect_match(
    note,
    "\u03c7\u00b2(18,60) = 28,68, p\u00a0=\u00a00,063",
    fixed = TRUE
  )
  # The language moves the headers, never the column names.
  expect_true("HS % of cases" %in% names(tbl))
  expect_identical(attr(tbl, "display_df")[["HS % of cases"]][[1L]], "21,6")
})

test_that("the frozen column keys equal their English headers", {
  expect_identical(.MR_KEY_PCT_CASES, spicy_str("header_pct_cases"))
  expect_identical(.MR_KEY_PCT_RESPONSES, spicy_str("header_pct_responses"))
})

test_that("the typed view: one row per item, the base as a fit_stat row", {
  s <- as_structured(.mr_farm_tbl())
  expect_identical(s$body$.row_role, c(rep("summary", 5L), "fit_stat"))
  expect_identical(s$body$.variable, c(LETTERS[1:5], "nobs"))
  expect_named(s$spanners, c("HS", "VS", "2YC", "4YC", "Other", "Total"))
  expect_true(isTRUE(s$col_meta[["Total n"]]$total))
  expect_null(s$col_meta[["HS n"]]$total)
  expect_identical(s$col_meta[["HS %"]]$token, "pct")
  expect_identical(s$body[["HS n"]][[6L]], 88)
  expect_true(is.na(s$body[["HS %"]][[6L]]))
  expect_identical(
    as_structured(.mr_farm_tbl(percent = "responses"))$col_meta[["HS %"]]$token,
    "pct_responses"
  )
})

# ---- coercion and broom -----------------------------------------------------------

test_that("as.data.frame() and as_tibble() return the data.frame output", {
  tbl <- .mr_farm_tbl()
  df <- as.data.frame(tbl)
  expect_identical(df, .mr_farm_tbl(output = "data.frame"))
  expect_identical(class(df), "data.frame")
  skip_if_not_installed("tibble")
  tb <- tibble::as_tibble(tbl)
  expect_s3_class(tb, "tbl_df")
  expect_identical(names(tb), names(df))
})

test_that("tidy() reads one row per item and group, the margin flagged", {
  skip_if_not_installed("broom")
  td <- broom::tidy(.mr_farm_tbl(test = "chisq"))
  expect_named(
    td,
    c(
      "variable",
      "label",
      "group",
      "total",
      "n",
      "base",
      "proportion",
      "proportion_responses",
      "test_type",
      "statistic",
      "df",
      "p.value",
      "p.unadjusted"
    )
  )
  expect_identical(nrow(td), 30L)
  expect_identical(td$variable[1:6], rep("A", 6L))
  expect_identical(td$total, rep(c(rep(FALSE, 5L), TRUE), 5L))
  expect_identical(td$n[[1L]], 19L)
  expect_equal(td$proportion[[1L]], 19 / 88)
  # The HS group made 173 choices in all (19 + 38 + 29 + 47 + 40).
  expect_equal(td$proportion_responses[[1L]], 19 / 173)
  expect_equal(td$statistic[[1L]], .mrcv_x2_items[[1L]], tolerance = 1e-6)
  expect_type(td$df, "integer")
  # Without `by` and without a test, the same schema, NA where nothing
  # applies.
  td0 <- broom::tidy(table_multiresponse(.mr_farmer(), A:E))
  expect_identical(names(td0), names(td))
  expect_true(all(is.na(td0$group)))
  expect_false(any(td0$total))
  expect_true(all(is.na(td0$p.value)))
})

test_that("glance() reads the question and its global test", {
  skip_if_not_installed("broom")
  gl <- broom::glance(.mr_farm_tbl())
  expect_identical(nrow(gl), 1L)
  expect_identical(gl$test_type, "rs2")
  expect_equal(gl$statistic, .mrcv_rs2[["statistic"]], tolerance = 1e-3)
  expect_equal(gl$df, .mrcv_rs2[["df"]], tolerance = 1e-3)
  expect_identical(gl$nobs, 262L)
  expect_identical(gl$n_groups, 5L)
  expect_identical(gl$n_items, 5L)
  expect_identical(gl$n_items_tested, 5L)
  expect_identical(gl$p_adjust, "holm")
  gl0 <- broom::glance(.mr_farm_tbl(p_value = FALSE))
  expect_identical(names(gl0), names(gl))
  expect_true(is.na(gl0$p.value))
  expect_true(is.na(gl0$p_adjust))
  expect_true(is.na(
    broom::glance(table_multiresponse(.mr_farmer(), A:E))$n_groups
  ))
  expect_error(
    broom::tidy(structure(
      data.frame(a = 1),
      class = c("spicy_multiresponse_table", "data.frame")
    )),
    class = "spicy_invalid_data"
  )
})

# ---- validation ---------------------------------------------------------------------

test_that("the inputs are validated in the house style", {
  d <- .mr_farmer()
  expect_error(table_multiresponse(d), class = "spicy_invalid_input")
  expect_error(
    table_multiresponse(d, select = character(0)),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_multiresponse(d, c(A, nope)),
    class = "spicy_missing_column"
  )
  expect_error(
    table_multiresponse(d, A:E, by = nope),
    class = "spicy_missing_column"
  )
  expect_error(
    table_multiresponse(d, c(A, ed), by = ed),
    class = "spicy_invalid_input"
  )
  for (bad in list(0, -1, 1.5, NA, "10", c(10, 20))) {
    expect_error(
      table_multiresponse(
        d,
        A:E,
        by = ed,
        global_test = "bootstrap",
        boot_n = bad
      ),
      class = "spicy_invalid_input"
    )
  }
  for (bad in list(NA, "yes", c(TRUE, FALSE))) {
    expect_error(
      table_multiresponse(d, A:E, by = ed, p_value = bad),
      class = "spicy_invalid_input"
    )
  }
  expect_error(
    table_multiresponse(d, A:E, percent = "rows"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_multiresponse(d, A:E, by = ed, test = "exact"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_multiresponse(d, A:E, by = ed, global_test = "exact"),
    class = "spicy_invalid_input"
  )
  # The global method left `test` with no alias: its old values are
  # errors there.
  for (old in c("rs2", "bootstrap")) {
    expect_error(
      table_multiresponse(d, A:E, by = ed, test = old),
      class = "spicy_invalid_input"
    )
  }
  expect_error(
    table_multiresponse(d, A:E, p_digits = 0),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_multiresponse(d, A:E, decimal_mark = ".."),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_multiresponse(list(a = 1), a),
    class = "spicy_invalid_data"
  )
})

test_that("a group literally named Total keeps its name, and the margin moves", {
  d <- data.frame(g = c("Total", "Total", "x", "x"), a = c(1, 0, 1, 1))
  expect_warning(
    out <- table_multiresponse(d, a, by = g, output = "data.frame"),
    class = "spicy_renamed_column"
  )
  expect_true(all(c("Total n", "x n", "Total_1 n") %in% names(out)))
  expect_identical(out[["Total_1 n"]], c(3, 4))
})

test_that("a survey design is refused, with the reason", {
  skip_if_not_installed("survey")
  d <- data.frame(a = c(1, 0, 1), w = c(1, 2, 1))
  design <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  expect_error(table_multiresponse(design, a), class = "spicy_wrong_regime")
})

test_that("a subset of the table prints as the data frame it is", {
  sub <- .mr_farm_tbl()[1:2, ]
  expect_s3_class(sub, "spicy_multiresponse_table")
  printed <- utils::capture.output(print(sub))
  expect_false(any(grepl("Multiple responses", printed, fixed = TRUE)))
  expect_true(any(grepl("Variable", printed, fixed = TRUE)))
})

test_that("the console table is pinned", {
  withr::local_options(width = 120)
  expect_snapshot(print(.mr_farm_tbl()))
  expect_snapshot(print(table_multiresponse(
    .mr_toy(),
    c(x1, x2, x3, x4),
    by = g,
    percent = "both",
    statistic = TRUE
  )))
  withr::local_options(spicy.language = "fr")
  expect_snapshot(print(table_multiresponse(.mr_toy(), c(x1, x2, x3), by = g)))
})
