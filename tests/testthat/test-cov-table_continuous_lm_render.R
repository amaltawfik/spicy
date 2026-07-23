# Coverage tests for R/table_continuous_lm_render.R.
#
# Targets the bivariate-layout render helpers that the existing
# test-table_continuous_lm.R suite leaves uncovered:
#   * weighted-n + integer-n display branches in
#     build_wide_display_df_continuous_lm()
#   * the default-alignment fallback arm of the tinytable exporter
#   * the no-CI branch of the gt exporter
#   * the missing-'officer' guard of the word exporter
#   * get_test_header_lm()'s non-finite-df and unknown-test fallbacks
#
# Style mirrors the sibling suites: real fits via table_continuous_lm()
# where possible, internal helpers called with spicy::: where a branch
# is only reachable below the public match.arg() guards.

# ---- build_wide_display_df: weighted-n + integer-n branches ----------------

test_that("build_wide_display_df_continuous_lm renders integer n and weighted n", {
  df <- data.frame(
    y = c(1, 2, 3, 10),
    g = factor(c("A", "A", "B", "B")),
    w = c(1, 1, 1, 5)
  )
  out <- table_continuous_lm(
    df,
    select = y,
    by = g,
    weights = w,
    show_weighted_n = TRUE,
    output = "long"
  )

  disp <- spicy:::build_wide_display_df_continuous_lm(
    out,
    digits = 2L,
    decimal_mark = ".",
    ci_level = 0.95,
    show_statistic = TRUE,
    show_p_value = TRUE,
    show_n = TRUE,
    show_weighted_n = TRUE,
    effect_size = "none",
    r2_type = "r2",
    ci = TRUE
  )

  expect_true(all(c("n", "Weighted n") %in% names(disp)))
  # Integer-n branch: rendered as a plain integer string, no decimals.
  expect_equal(disp$n[1], "4")
  # Weighted-n branch: rendered with the requested decimal precision.
  expect_equal(disp[["Weighted n"]][1], "8.00")
})

test_that("build_wide_display_df_continuous_lm blanks NA n and NA weighted n", {
  # Hand-build a one-variable long block whose n / weighted_n are NA so
  # the is.na() arms of the n and Weighted-n formatters fire.
  out <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Petal.Width,
    show_weighted_n = FALSE,
    output = "long"
  )
  out$n <- NA_integer_
  out$weighted_n <- NA_real_

  disp <- spicy:::build_wide_display_df_continuous_lm(
    out,
    digits = 2L,
    decimal_mark = ".",
    ci_level = 0.95,
    show_statistic = TRUE,
    show_p_value = TRUE,
    show_n = TRUE,
    show_weighted_n = TRUE,
    effect_size = "none",
    r2_type = "r2",
    ci = TRUE
  )

  expect_equal(disp$n[1], "")
  expect_equal(disp[["Weighted n"]][1], "")
})

# ---- tinytable exporter: default-alignment fallback arm ---------------------

test_that("export_continuous_lm_table tinytable falls back to per-column default alignment", {
  skip_if_not_installed("tinytable")
  # The public table_continuous_lm() validates `align` via match.arg()
  # to decimal/center/right, so the final else-arm (centre most cols,
  # right-align n / Weighted n / p) is only reachable by calling the
  # internal exporter with a non-standard align. Assert it still yields
  # a valid tinytable rather than erroring.
  ddf <- data.frame(
    Variable = c("y1", "y2"),
    `M (A)` = c("1.00", "2.00"),
    p = c(".050", ".010"),
    n = c("10", "12"),
    `Weighted n` = c("10.00", "12.00"),
    check.names = FALSE
  )

  tt <- spicy:::export_continuous_lm_table(
    ddf,
    output = "tinytable",
    ci_level = 0.95,
    align = "left",
    decimal_mark = ".",
    excel_path = NULL,
    excel_sheet = "Sheet1",
    clipboard_delim = "\t",
    word_path = NULL
  )

  expect_true(inherits(tt, "tinytable"))

  # The class check alone does not prove the default-alignment else-arm
  # actually placed each column. Render to HTML (a deterministic text
  # output -- no getOption("width")/locale/quote-style dependence) and
  # recover the resolved per-column body alignment from the emitted CSS.
  # tinytable assigns each styled cell a random css_id, so we map every
  # body cell (row index i >= 1) at column j to its css_id, then read the
  # text-align off that id's CSS rule.
  html <- tinytable::save_tt(tt, output = "html")

  body_align <- function(html, j) {
    block_re <- r"{positions:\s*\[(.*?)\],\s*css_id:\s*'([^']+)'}"
    blocks <- regmatches(html, gregexpr(block_re, html, perl = TRUE))[[1]]
    pair_re <- r"{i:\s*'([0-9-]+)'\s*,\s*j:\s*([0-9]+)}"
    hit_ids <- character(0)
    for (b in blocks) {
      css_id <- regmatches(
        b,
        regexec(r"{css_id:\s*'([^']+)'}", b, perl = TRUE)
      )[[1]][2]
      pairs <- regmatches(b, gregexpr(pair_re, b, perl = TRUE))[[1]]
      for (p in pairs) {
        pm <- regmatches(p, regexec(pair_re, p, perl = TRUE))[[1]]
        ii <- as.integer(pm[2])
        jj <- as.integer(pm[3])
        if (!is.na(ii) && ii >= 1L && jj == j) {
          hit_ids <- c(hit_ids, css_id)
        }
      }
    }
    aligns <- character(0)
    for (id in unique(hit_ids)) {
      rule <- regmatches(
        html,
        regexpr(paste0(r"{\.}", id, r"{[^{]*\{[^}]*\}}"), html, perl = TRUE)
      )
      if (length(rule) && grepl("text-align", rule)) {
        aligns <- c(
          aligns,
          regmatches(
            rule,
            regexec(r"{text-align:\s*([a-z]+)}", rule, perl = TRUE)
          )[[1]][2]
        )
      }
    }
    unique(aligns)
  }

  # Column order: 1 Variable, 2 M (A), 3 p, 4 n, 5 Weighted n.
  # Else-arm contract: column 1 left, n / Weighted n / p right-aligned,
  # every remaining numeric column centred.
  expect_identical(body_align(html, 1L), "left") # label column
  expect_identical(body_align(html, 2L), "center") # generic numeric -> centre
  expect_identical(body_align(html, 3L), "right") # p -> right
  expect_identical(body_align(html, 4L), "right") # n -> right
  expect_identical(body_align(html, 5L), "right") # Weighted n -> right

  # Guard against a regression that would centre the count/p columns:
  # none of n / Weighted n / p may be centred, and the generic numeric
  # column must not be right-aligned.
  expect_false(any(c("center") %in% body_align(html, 4L)))
  expect_false(any(c("right") %in% body_align(html, 2L)))
})

# ---- gt exporter: no-CI branch ---------------------------------------------

test_that("table_continuous_lm gt output with ci = FALSE omits the CI spanner", {
  skip_if_not_installed("gt")
  # ci = FALSE drops the 95% CI LL/UL columns so has_ci is FALSE in the
  # gt exporter, taking the ci_css_sel == "" branch.
  out <- table_continuous_lm(
    sochealth,
    select = c(wellbeing_score, bmi),
    by = sex,
    ci = FALSE,
    output = "gt"
  )

  expect_s3_class(out, "gt_tbl")
  spanners <- out[["_spanners"]]
  labels <- unlist(spanners$spanner_label)
  expect_false(any(grepl("CI", labels)))
})

# ---- word exporter: missing 'officer' guard --------------------------------

test_that("export_continuous_lm_table word output aborts when officer is absent", {
  skip_if_not_installed("flextable")
  # Mock requireNamespace so flextable is available but officer is not;
  # the abort fires before any officer code runs, so no recursion.
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (identical(pkg, "officer")) {
        return(FALSE)
      }
      TRUE
    },
    .package = "base"
  )

  ddf <- data.frame(
    Variable = c("y1", "y2"),
    `M (A)` = c("1.00", "2.00"),
    p = c(".050", ".010"),
    check.names = FALSE
  )

  expect_error(
    spicy:::export_continuous_lm_table(
      ddf,
      output = "word",
      ci_level = 0.95,
      align = "decimal",
      decimal_mark = ".",
      excel_path = NULL,
      excel_sheet = "Sheet1",
      clipboard_delim = "\t",
      word_path = tempfile(fileext = ".docx")
    ),
    class = "spicy_missing_pkg"
  )
})

# ---- get_test_header_lm: non-finite df + unknown-test fallbacks -------------

test_that("get_test_header_lm prints t() when the Satterthwaite df is non-finite", {
  # format_df() returns "" for a non-finite df2, so an exact t-header
  # with infinite df2 collapses to "t()".
  block <- data.frame(
    test_type = "t",
    df1 = 1,
    df2 = Inf,
    statistic = 2,
    predictor_type = "categorical",
    estimate = c(NA, 1),
    level = c("A", "B"),
    stringsAsFactors = FALSE
  )
  expect_equal(spicy:::get_test_header_lm(block, TRUE, TRUE), "t()")
})

test_that("get_test_header_lm returns the raw test type for an unrecognised test", {
  # A test_type that is none of t/z/F/chi2 makes both single_coef and
  # multi_coef empty, so `chosen` is NA and the function short-circuits
  # to `return(test_types[1])` -- the raw label, verbatim.
  block <- data.frame(
    test_type = "wald_custom",
    df1 = 1,
    df2 = 10,
    statistic = 2,
    predictor_type = "categorical",
    estimate = c(NA, 1),
    level = c("A", "B"),
    stringsAsFactors = FALSE
  )
  expect_equal(spicy:::get_test_header_lm(block, TRUE, TRUE), "wald_custom")
})
