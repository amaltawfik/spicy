# Coverage-targeted tests for R/table_continuous.R.
#
# These exercise internal branches not reached by the existing
# table_continuous test suite:
#   * resolve_effect_size_choice(): the "none" short-circuit and the
#     four `explicit = FALSE` silent-fallback arms (parametric vs.
#     nonparametric measure / two- vs. multi-group mismatch). Through
#     the public API `effect_size` is only ever "auto" or "none" when
#     `explicit = FALSE`, so these fallbacks are only reachable by
#     calling the helper directly.
#   * compute_effect_size(): the `type = NULL` auto-resolution block and
#     the unknown-type abort.
#   * export_desc_table(): the legacy "auto" per-column alignment arm of
#     each rendering engine (tinytable / gt / flextable), unreachable
#     from table_continuous() because `align` is match.arg()'d to
#     decimal/center/right, but still selectable when the internal
#     exporter is called with align = "auto".
#
# Each block makes a real assertion; engine-specific blocks are gated by
# skip_if_not_installed().


# ---- resolve_effect_size_choice() ---------------------------------------

test_that("resolve_effect_size_choice short-circuits on 'none'", {
  expect_identical(
    spicy:::resolve_effect_size_choice("none", 2L, "welch", explicit = TRUE),
    "none"
  )
})

test_that("resolve_effect_size_choice('auto') maps to the test/group default", {
  expect_identical(
    spicy:::resolve_effect_size_choice("auto", 2L, "welch"),
    "hedges_g"
  )
  expect_identical(
    spicy:::resolve_effect_size_choice("auto", 3L, "welch"),
    "eta_sq"
  )
  expect_identical(
    spicy:::resolve_effect_size_choice("auto", 2L, "nonparametric"),
    "r_rb"
  )
  expect_identical(
    spicy:::resolve_effect_size_choice("auto", 3L, "nonparametric"),
    "epsilon_sq"
  )
})

test_that("resolve_effect_size_choice silently falls back when not explicit", {
  # parametric test + nonparametric measure -> auto parametric default
  expect_identical(
    spicy:::resolve_effect_size_choice(
      "r_rb", 2L, "welch", explicit = FALSE
    ),
    "hedges_g"
  )
  # nonparametric test + parametric measure -> auto nonparametric default
  expect_identical(
    spicy:::resolve_effect_size_choice(
      "hedges_g", 2L, "nonparametric", explicit = FALSE
    ),
    "r_rb"
  )
  # multi-group measure with 2 groups -> auto two-group default
  expect_identical(
    spicy:::resolve_effect_size_choice(
      "eta_sq", 2L, "welch", explicit = FALSE
    ),
    "hedges_g"
  )
  # two-group measure with 3 groups -> auto multi-group default
  expect_identical(
    spicy:::resolve_effect_size_choice(
      "hedges_g", 3L, "welch", explicit = FALSE
    ),
    "eta_sq"
  )
})

test_that("resolve_effect_size_choice aborts on explicit incompatible measures", {
  expect_error(
    spicy:::resolve_effect_size_choice("r_rb", 2L, "welch"),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy:::resolve_effect_size_choice("hedges_g", 2L, "nonparametric"),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy:::resolve_effect_size_choice("eta_sq", 2L, "welch"),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy:::resolve_effect_size_choice("hedges_g", 3L, "welch"),
    class = "spicy_invalid_input"
  )
})


# ---- compute_effect_size() ----------------------------------------------

test_that("compute_effect_size auto-resolves type from method/group count", {
  set.seed(1)
  x <- c(rnorm(20, 0), rnorm(20, 1))
  g <- factor(rep(c("a", "b"), each = 20))

  # type = NULL, parametric, 2 groups -> hedges_g
  row <- spicy:::compute_effect_size(x, g, 2L, "welch", 0.95, type = NULL)
  expect_identical(row$es_type, "hedges_g")
  expect_false(is.na(row$es_value))

  # type = NULL, nonparametric, 2 groups -> r_rb
  row_np <- spicy:::compute_effect_size(
    x, g, 2L, "nonparametric", 0.95, type = NULL
  )
  expect_identical(row_np$es_type, "r_rb")
})

test_that("compute_effect_size auto-resolves multi-group types", {
  set.seed(2)
  x <- c(rnorm(15, 0), rnorm(15, 1), rnorm(15, 2))
  g <- factor(rep(c("a", "b", "c"), each = 15))

  row <- spicy:::compute_effect_size(x, g, 3L, "welch", 0.95, type = NULL)
  expect_identical(row$es_type, "eta_sq")

  row_np <- spicy:::compute_effect_size(
    x, g, 3L, "nonparametric", 0.95, type = NULL
  )
  expect_identical(row_np$es_type, "epsilon_sq")
})

test_that("compute_effect_size aborts on an unknown effect-size type", {
  x <- c(1, 2, 3, 4)
  g <- factor(c("a", "a", "b", "b"))
  expect_error(
    spicy:::compute_effect_size(x, g, 2L, "welch", 0.95, type = "bogus"),
    class = "spicy_invalid_input"
  )
})


# ---- export_desc_table(): legacy "auto" alignment arm -------------------

# Helper: build the (result, display_df) pair the internal exporter
# consumes, from a grouped fit carrying n / statistic / p / ES columns so
# the per-column "auto" alignment rule visits every branch.
make_desc_inputs <- function() {
  result <- table_continuous(
    iris,
    select = c(Sepal.Length, Sepal.Width),
    by = Species,
    statistic = TRUE,
    p_value = TRUE,
    effect_size = "auto",
    output = "data.frame"
  )
  display_df <- spicy:::build_display_df(
    result,
    digits = 2L,
    decimal_mark = ".",
    ci_level = 0.95,
    show_p = TRUE,
    show_statistic = TRUE,
    show_n = TRUE,
    show_ci = TRUE,
    show_effect_size = TRUE,
    show_effect_size_ci = TRUE,
    effect_size_digits = 2L,
    p_digits = 3L
  )
  list(result = result, display_df = display_df)
}

test_that("export_desc_table tinytable honours the legacy 'auto' alignment", {
  skip_if_not_installed("tinytable")
  inputs <- make_desc_inputs()
  tt <- spicy:::export_desc_table(
    inputs$display_df,
    inputs$result,
    output = "tinytable",
    ci_level = 0.95,
    align = "auto",
    decimal_mark = ".",
    has_group = TRUE,
    show_n = TRUE,
    excel_path = NULL,
    excel_sheet = "Descriptives",
    clipboard_delim = "\t",
    word_path = NULL
  )
  expect_true(methods::is(tt, "tinytable"))
})

test_that("export_desc_table gt honours the legacy 'auto' alignment", {
  skip_if_not_installed("gt")
  inputs <- make_desc_inputs()
  tbl <- spicy:::export_desc_table(
    inputs$display_df,
    inputs$result,
    output = "gt",
    ci_level = 0.95,
    align = "auto",
    decimal_mark = ".",
    has_group = TRUE,
    show_n = TRUE,
    excel_path = NULL,
    excel_sheet = "Descriptives",
    clipboard_delim = "\t",
    word_path = NULL
  )
  expect_s3_class(tbl, "gt_tbl")
})

test_that("export_desc_table flextable honours the legacy 'auto' alignment", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  inputs <- make_desc_inputs()
  ft <- spicy:::export_desc_table(
    inputs$display_df,
    inputs$result,
    output = "flextable",
    ci_level = 0.95,
    align = "auto",
    decimal_mark = ".",
    has_group = TRUE,
    show_n = TRUE,
    excel_path = NULL,
    excel_sheet = "Descriptives",
    clipboard_delim = "\t",
    word_path = NULL
  )
  expect_s3_class(ft, "flextable")
})
