test_that("freq() works with a simple numeric vector", {
  x <- c(1, 2, 2, 3, 3, 3, NA)
  df <- freq(x, styled = FALSE)

  expect_s3_class(df, "data.frame")
  expect_true(all(c("value", "n", "prop") %in% names(df)))
  expect_equal(sum(df$n, na.rm = TRUE), length(x))
  expect_equal(round(sum(df$prop, na.rm = TRUE), 1), 1)
})


test_that("freq() works with a data.frame column", {
  df <- data.frame(cat = c("A", "B", "A", "C", "A", "B", "B", "C", "C", "C"))
  res <- freq(df, cat, styled = FALSE)

  expect_s3_class(res, "data.frame")
  expect_equal(sum(res$n, na.rm = TRUE), nrow(df))
  expect_true(any(grepl("C", res$value)))
})

test_that("freq() requires x when data is a data.frame", {
  expect_error(
    freq(mtcars, styled = FALSE),
    "must supply `x`",
    fixed = TRUE
  )
})


test_that("freq() handles labelled variables correctly", {
  library(labelled)

  x <- labelled(
    c(1, 2, 3, 1, 2, 3, 1, 2, NA),
    labels = c("Low" = 1, "Medium" = 2, "High" = 3)
  )
  var_label(x) <- "Satisfaction level"

  # Prefixed (default)
  f1 <- freq(x, labelled_levels = "prefixed", styled = FALSE)
  expect_true(any(grepl("\\[1\\]", f1$value)))

  # Labels only
  f2 <- freq(x, labelled_levels = "labels", styled = FALSE)
  expect_true(all(!grepl("\\[", f2$value)))

  # Underlying values only
  f1 <- freq(x, styled = FALSE)
  f2 <- freq(x, na_val = 1, styled = FALSE)

  # After recoding, there should be one fewer distinct category
  expect_true(length(unique(f2$value)) <= length(unique(f1$value)))
})


test_that("freq() handles weights and rescaling", {
  df <- data.frame(
    sexe = factor(c("Male", "Female", "Female", "Male", NA, "Female")),
    poids = c(12, 8, 10, 15, 7, 9)
  )

  # Weighted, rescaled
  f_rescaled <- freq(df, sexe, weights = poids, rescale = TRUE, styled = FALSE)
  total_weighted <- sum(f_rescaled$n)
  expect_true(abs(total_weighted - nrow(df)) < 1e-6)

  # Weighted, not rescaled
  f_unscaled <- freq(df, sexe, weights = poids, rescale = FALSE, styled = FALSE)
  expect_true(sum(f_unscaled$n) > nrow(df))
})

test_that("freq() rejects rescale when sum of weights is zero", {
  df <- data.frame(x = c("A", "B"), w = c(0, 0))
  expect_error(
    freq(df, x, weights = w, rescale = TRUE, styled = FALSE),
    "strictly positive sum of weights",
    fixed = TRUE
  )
})


test_that("freq() handles missing value recoding", {
  x <- labelled(
    c(1, 2, 3, 1, 2, 3, 1),
    labels = c("Low" = 1, "Medium" = 2, "High" = 3)
  )

  f1 <- freq(x, styled = FALSE)
  f2 <- freq(x, na_val = 1, styled = FALSE)

  # Compare NA frequencies in the output table
  na_count_f1 <- f1$n[f1$value == "<NA>"]
  na_count_f2 <- f2$n[f2$value == "<NA>"]

  # Handle case where <NA> row is missing
  na_count_f1 <- if (length(na_count_f1)) na_count_f1 else 0
  na_count_f2 <- if (length(na_count_f2)) na_count_f2 else 0

  # Expect the 'Low' category to be removed after recoding
  expect_false(any(grepl("Low", f2$value)))

  # Optionally, confirm total frequency unchanged (only recoded)
  expect_equal(
    round(sum(f1$n, na.rm = TRUE), 5),
    round(sum(f2$n, na.rm = TRUE), 5)
  )
})

test_that("freq() correctly sorts by frequency and name", {
  x <- c("Banana", "Apple", "Cherry", "Banana", "Apple", "Cherry", "Apple")

  f_plus <- freq(x, sort = "+", styled = FALSE)
  f_minus <- freq(x, sort = "-", styled = FALSE)
  f_name_plus <- freq(x, sort = "name+", styled = FALSE)
  f_name_minus <- freq(x, sort = "name-", styled = FALSE)

  # Frequency ascending/descending
  expect_true(f_plus$n[1] <= f_plus$n[length(f_plus$n)])
  expect_true(f_minus$n[1] >= f_minus$n[length(f_minus$n)])

  # Alphabetical order
  expect_equal(sort(f_name_plus$value), f_name_plus$value)
  expect_equal(sort(f_name_minus$value, decreasing = TRUE), f_name_minus$value)
})


test_that("freq() handles multiple data types correctly", {
  df <- data.frame(
    logical_col = c(TRUE, FALSE, TRUE, NA),
    date_col = as.Date(c("2023-01-01", "2023-01-02", "2023-01-01", NA)),
    posix_col = as.POSIXct(c(
      "2023-01-01 12:00",
      "2023-01-02 12:00",
      NA,
      "2023-01-02 12:00"
    )),
    char_col = c("a", "a", "b", NA),
    num_col = c(1, 1, 2, NA)
  )

  expect_s3_class(freq(df, logical_col, styled = FALSE), "data.frame")
  expect_s3_class(freq(df, date_col, styled = FALSE), "data.frame")
  expect_s3_class(freq(df, posix_col, styled = FALSE), "data.frame")
  expect_s3_class(freq(df, char_col, styled = FALSE), "data.frame")
  expect_s3_class(freq(df, num_col, styled = FALSE), "data.frame")
})


test_that("freq() handles invalid weight and sort arguments", {
  x <- c(1, 2, 3)
  expect_error(freq(x, weights = c(-1, 0, 1)), "must be non-negative")
  expect_error(freq(x, weights = c(1, 2)), "same length")
  expect_error(freq(x, sort = "wrong"), "Invalid value for 'sort'")
})


test_that("freq() prints styled table invisibly", {
  x <- c("A", "B", "B", "C")
  expect_invisible(freq(x, styled = TRUE))
})

test_that("freq() cum = TRUE adds cumulative columns", {
  x <- c("A", "B", "B", "C")
  res <- freq(x, cum = TRUE, styled = FALSE)
  expect_true("cum_prop" %in% names(res))
  cum_vals <- res$cum_prop[!is.na(res$cum_prop)]
  expect_equal(cum_vals[length(cum_vals)], 1)
})

test_that("freq() valid = FALSE keeps valid_prop as NA", {
  x <- c(1, 2, 2, NA)
  res <- freq(x, valid = FALSE, styled = FALSE)
  expect_true("valid_prop" %in% names(res))
  expect_true(all(is.na(res$valid_prop)))
})

test_that("freq() valid = TRUE includes valid_prop", {
  x <- c(1, 2, 2, NA)
  res <- freq(x, valid = TRUE, styled = FALSE)
  expect_true("valid_prop" %in% names(res))
})

test_that("freq() labelled_levels = 'values' shows raw values", {
  skip_if_not_installed("labelled")
  x <- labelled::labelled(c(1, 2, 3), labels = c(A = 1, B = 2, C = 3))
  res <- freq(x, labelled_levels = "values", styled = FALSE)
  expect_true(any(res$value %in% c("1", "2", "3")))
})

test_that("freq() with factor input works directly", {
  f <- factor(c("x", "y", "x", "z"))
  res <- freq(f, styled = FALSE)
  expect_s3_class(res, "data.frame")
  expect_equal(sum(res$n, na.rm = TRUE), 4)
})

test_that("freq() works with a tibble input", {
  tib <- tibble::tibble(
    x = c("A", "B", "B", "C", "A", "B"),
    w = c(1, 2, 3, 4, 5, 6)
  )

  # Bare-name column extraction
  res_unweighted <- freq(tib, x, styled = FALSE)
  expect_s3_class(res_unweighted, "data.frame")
  expect_equal(sum(res_unweighted$n), 6L)
  expect_setequal(res_unweighted$value, c("A", "B", "C"))

  # Bare-name weight resolves through the data mask (tidy-eval)
  res_weighted <- freq(tib, x, weights = w, rescale = FALSE, styled = FALSE)
  expect_equal(sum(res_weighted$n), sum(tib$w))
})

test_that("freq() works with a tibble containing a labelled column", {
  skip_if_not_installed("labelled")

  tib <- tibble::tibble(
    sat = labelled::labelled(
      c(1, 2, 3, 1, 2),
      labels = c(Low = 1, Mid = 2, High = 3)
    )
  )

  res <- freq(tib, sat, styled = FALSE)
  expect_s3_class(res, "data.frame")
  expect_equal(sum(res$n), 5L)
  # Default labelled_levels = "prefixed"
  expect_true(any(grepl("\\[1\\] Low", res$value)))
})

test_that("freq() preserves footer metadata when input is a tibble", {
  tib <- tibble::tibble(
    x = c("A", "B", "B"),
    w = c(2, 3, 5)
  )
  capture.output(res <- freq(tib, x, weights = w))

  expect_equal(attr(res, "var_name"), "x")
  expect_equal(attr(res, "data_name"), "tib")
  expect_true(isTRUE(attr(res, "weighted")))
  expect_equal(attr(res, "weight_var"), "w")
})

test_that("freq() cum + weighted works", {
  df <- data.frame(x = c("A", "B", "C"), w = c(2, 3, 5))
  res <- freq(df, x, weights = w, cum = TRUE, styled = FALSE)
  expect_true("cum_prop" %in% names(res))
})

test_that("freq() na_val with plain vector", {
  x <- c(1, 2, 3, 99, 99)
  res <- freq(x, na_val = 99, styled = FALSE)
  n_na <- res$n[is.na(res$value)]
  expect_equal(n_na, 2)
})

test_that("freq() errors with non-finite weights", {
  expect_error(freq(c(1, 2), weights = c(1, Inf)), "finite")
})

test_that("freq() rejects non-numeric weights with a clear type error", {
  # Without the upfront type guard, character weights pass the
  # `< 0` comparison via lexicographic coercion and only crash at
  # `is.finite` with a misleading "finite numeric values" message.
  expect_error(
    freq(c(1, 2, 3), weights = c("a", "b", "c")),
    "must be a numeric or logical vector",
    fixed = TRUE
  )
  expect_error(
    freq(c(1, 2, 3), weights = factor(c("a", "b", "c"))),
    "must be a numeric or logical vector",
    fixed = TRUE
  )
  expect_error(
    freq(c(1, 2, 3), weights = list(1, 2, 3)),
    "must be a numeric or logical vector",
    fixed = TRUE
  )
})

test_that("freq() accepts logical weights via implicit coercion", {
  # TRUE = 1, FALSE = 0 — common shorthand for "include / exclude"
  # weighting in survey contexts. Locks in the supported type set.
  res <- freq(
    c("A", "B", "C"),
    weights = c(TRUE, FALSE, TRUE),
    rescale = FALSE,
    styled = FALSE
  )
  expect_equal(sum(res$n), 2)
  expect_equal(res$n[res$value == "B"], 0)
})

test_that("freq() styled output has class spicy_freq_table", {
  res <- freq(c("A", "B", "A"))
  expect_s3_class(res, "spicy_freq_table")
})

test_that("freq() cum + valid shows cumulative valid column", {
  x <- c("A", "B", "B", NA)
  res <- freq(x, cum = TRUE, valid = TRUE, styled = FALSE)
  expect_true("cum_valid_prop" %in% names(res))
})

test_that("freq() styled weighted output prints invisibly", {
  df <- data.frame(x = c("A", "B", "C"), w = c(2, 3, 5))
  expect_invisible(freq(df, x, weights = w, styled = TRUE))
})

test_that("freq() labelled with non-numeric na_val warns", {
  skip_if_not_installed("labelled")
  x <- labelled::labelled(c(1, 2, 3), labels = c(A = 1, B = 2, C = 3))
  expect_warning(
    freq(x, na_val = "A", styled = FALSE),
    "underlying numeric value"
  )
})

test_that("freq() styled cum output prints invisibly", {
  expect_invisible(freq(c("A", "B", "B"), cum = TRUE, styled = TRUE))
})

test_that("freq() errors when weight variable not found", {
  df <- data.frame(x = c("A", "B", "C"))
  expect_error(
    freq(df, x, weights = nonexistent_var, styled = FALSE),
    "not found"
  )
})

test_that("freq() accepts literal `weights = NULL` as no weighting", {
  df <- data.frame(x = c("A", "B", "C"))

  expect_silent({
    res <- freq(df, x, weights = NULL, styled = FALSE)
  })
  expect_equal(sum(res$n), 3L)
})

test_that("freq() supports the `weights = if (cond) wts else NULL` pattern", {
  df <- data.frame(x = c("A", "B", "C"))
  my_w <- c(2, 3, 5)

  use_w <- FALSE
  res_off <- freq(
    df,
    x,
    weights = if (use_w) my_w else NULL,
    rescale = FALSE,
    styled = FALSE
  )
  expect_equal(sum(res_off$n), 3L)

  use_w <- TRUE
  res_on <- freq(
    df,
    x,
    weights = if (use_w) my_w else NULL,
    rescale = FALSE,
    styled = FALSE
  )
  expect_equal(sum(res_on$n), 10L)
})

test_that("freq() resolves column refs inside compound weight expressions", {
  # Tidy-eval makes `data` available as a mask, so column references
  # nested inside an `if/else` (or any other expression) work.
  df <- data.frame(x = c("A", "B", "C"), w = c(2, 3, 5))

  use_w <- TRUE
  res_on <- freq(
    df,
    x,
    weights = if (use_w) w else NULL,
    rescale = FALSE,
    styled = FALSE
  )
  expect_equal(sum(res_on$n), 10L)

  use_w <- FALSE
  res_off <- freq(
    df,
    x,
    weights = if (use_w) w else NULL,
    rescale = FALSE,
    styled = FALSE
  )
  expect_equal(sum(res_off$n), 3L)
})

test_that("freq() treats a variable holding NULL as no weighting", {
  df <- data.frame(x = c("A", "B", "C"))
  my_w <- NULL

  expect_silent({
    res <- freq(df, x, weights = my_w, styled = FALSE)
  })
  expect_equal(sum(res$n), 3L)
})

test_that("freq() with `weights = NULL` carries no weighting metadata", {
  df <- data.frame(x = c("A", "B", "C"))
  capture.output(res <- freq(df, x, weights = NULL))

  expect_false(isTRUE(attr(res, "weighted")))
  expect_null(attr(res, "weight_var"))
})

test_that("freq() cum + valid styled prints invisibly", {
  x <- c("A", "B", NA, "A")
  expect_invisible(freq(x, cum = TRUE, valid = TRUE, styled = TRUE))
})

test_that("freq() warns when data is vector and x is given", {
  expect_warning(
    res <- freq(c(1, 2, 3), x = c(4, 5, 6), styled = FALSE),
    "ignored"
  )
  expect_s3_class(res, "data.frame")
})

test_that("freq() aligns data_name on x when both data and x are vectors", {
  capture.output(
    res <- suppressWarnings(freq(c(1, 2, 3), x = c(4, 5, 6)))
  )

  expect_equal(attr(res, "var_name"), "c(4, 5, 6)")
  expect_equal(attr(res, "data_name"), attr(res, "var_name"))
})

test_that("freq() footer does not reveal the ignored `data` vector name", {
  out <- capture.output(
    suppressWarnings(freq(c(1, 2, 3), x = c(4, 5, 6)))
  )

  # The analyzed vector (x) appears in the title and the footer;
  # the ignored `data` vector must not appear anywhere.
  expect_false(any(grepl("c\\(1, 2, 3\\)", out)))
  expect_true(any(grepl("c\\(4, 5, 6\\)", out)))
})

test_that("freq() errors with invalid digits", {
  expect_error(freq(c(1, 2), digits = -1), "non-negative")
  expect_error(freq(c(1, 2), digits = "a"), "non-negative")
})

test_that("freq() rejects non-finite digits with the same friendly message", {
  # is.finite() catches NA, NaN, Inf and -Inf in one check. Without
  # this guard, `digits = NA` would crash inside the validation `if`
  # with "missing value where TRUE/FALSE needed", and `digits = Inf`
  # would crash later in `format(..., nsmall = Inf)`.
  # Since 0.11.0 the message also mentions "integer" -- digits is now
  # required to be a non-negative integer for cross-package consistency.
  for (bad in list(NA, NA_real_, NA_integer_, NaN, Inf, -Inf)) {
    expect_error(
      freq(c(1, 2), digits = bad),
      "non-negative integer",
      fixed = TRUE,
      info = paste("digits =", deparse(bad))
    )
  }
})

test_that("freq() rejects non-integer `digits` values", {
  # 0.11.0 tightens `digits` to a non-negative integer (matches the
  # rest of the table_*() / cross_tab() family).
  expect_error(freq(c(1, 2), digits = 1.5), "non-negative integer")
  expect_error(freq(c(1, 2), digits = -1), "non-negative integer")
  expect_silent(freq(c(1, 2), digits = 0L, styled = FALSE))
  expect_silent(freq(c(1, 2), digits = 3, styled = FALSE)) # 3.0 -> 3L OK
})

test_that("freq() validates `decimal_mark`", {
  expect_error(
    freq(c(1, 2), decimal_mark = ";"),
    "decimal_mark"
  )
  expect_error(
    freq(c(1, 2), decimal_mark = c(".", ",")),
    "decimal_mark"
  )
})

test_that("freq() honours `decimal_mark = ',' in the printed percentages", {
  out <- capture.output(freq(c(1, 1, 2, 2, 2), decimal_mark = ","))
  expect_true(any(grepl("60,0", out, fixed = TRUE)))
  expect_true(any(grepl("40,0", out, fixed = TRUE)))
  expect_false(any(grepl("\\b\\d+\\.\\d", out)))
})

test_that("freq() rejects pathological sort values with the friendly message", {
  # Without `is.character` / `length == 1L` / `!is.na` guards, `sort = NULL`
  # crashed with "argument is of length zero" (logical(0) inside `if`),
  # `sort = NA` crashed with "missing value where TRUE/FALSE needed",
  # and `sort = c("+", "-")` warned about length and silently took the
  # first element.
  for (bad in list(NULL, NA_character_, NA, c("+", "-"), 1L, "")) {
    if (identical(bad, "")) {
      next # "" is a valid sort value, skip
    }
    expect_error(
      freq(c(1, 2), sort = bad),
      "Invalid value for 'sort'",
      fixed = TRUE,
      info = paste("sort =", deparse(bad))
    )
  }
})

test_that("freq() validates logical arguments up front", {
  # Reuses validate_varlist_logical() — same error message format
  # as varlist() / code_book(), so users get a consistent diagnostic
  # across the package.
  expect_error(freq(c(1, 2), valid = "yes"), "`valid` must be TRUE or FALSE", fixed = TRUE)
  expect_error(freq(c(1, 2), valid = NA), "`valid` must be TRUE or FALSE", fixed = TRUE)
  expect_error(freq(c(1, 2), valid = c(TRUE, FALSE)), "`valid` must be TRUE or FALSE", fixed = TRUE)

  expect_error(freq(c(1, 2), cum = "yes"), "`cum` must be TRUE or FALSE", fixed = TRUE)
  expect_error(freq(c(1, 2), cum = NA), "`cum` must be TRUE or FALSE", fixed = TRUE)

  expect_error(freq(c(1, 2), rescale = "no"), "`rescale` must be TRUE or FALSE", fixed = TRUE)
  expect_error(freq(c(1, 2), rescale = NA), "`rescale` must be TRUE or FALSE", fixed = TRUE)

  expect_error(freq(c(1, 2), styled = "yes"), "`styled` must be TRUE or FALSE", fixed = TRUE)
  expect_error(freq(c(1, 2), styled = NA), "`styled` must be TRUE or FALSE", fixed = TRUE)
})

test_that("freq() validates sort early", {
  expect_error(freq(c(1, 2), sort = "bad"), "Invalid value for 'sort'")
})

test_that("freq() warns with NA weights and reports the count", {
  df <- data.frame(x = c("A", "B", "C"), w = c(1, NA, 3))
  expect_warning(
    res <- freq(df, x, weights = w, styled = FALSE),
    "1 NA value in `weights`"
  )
})

test_that("freq() drops NA-weighted rows and rescales over the remaining (0.11.0)", {
  # Since spicy 0.11.0 NA-weighted observations are dropped from the
  # table entirely (matches `cross_tab()`); the previous behaviour
  # (NA -> 0 with rescale to length(weights)) inflated `n_total` and
  # biased the rescale denominator.
  df <- data.frame(x = c("A", "B", "C", "D"), w = c(1, 2, NA, NA))

  res_rescaled <- suppressWarnings(
    freq(df, x, weights = w, rescale = TRUE, styled = FALSE)
  )
  res_unrescaled <- suppressWarnings(
    freq(df, x, weights = w, rescale = FALSE, styled = FALSE)
  )

  # Two rows survive (A, B); the NA-weighted C, D are dropped.
  expect_setequal(res_rescaled$value, c("A", "B"))
  expect_setequal(res_unrescaled$value, c("A", "B"))

  # Rescaled: total weighted N equals number of kept rows = 2.
  expect_equal(sum(res_rescaled$n), 2)
  # Unrescaled: total = sum of the surviving (non-NA) weights = 1 + 2 = 3.
  expect_equal(sum(res_unrescaled$n), 3)
})

test_that("freq() errors when total frequency is zero", {
  expect_error(
    freq(character(0), styled = FALSE),
    "Total frequency is zero"
  )
  expect_error(
    freq(c("A", "B"), weights = c(0, 0), rescale = FALSE, styled = FALSE),
    "Total frequency is zero"
  )
})

test_that("freq() NA representation is consistent with and without weights", {
  x <- c("A", "B", NA)
  res_plain <- freq(x, styled = FALSE)
  df <- data.frame(x = x, w = c(1, 2, 3))
  res_weighted <- freq(df, x, weights = w, styled = FALSE)

  # Both should use true NA, not "<NA>" string
  expect_true(any(is.na(res_plain$value)))
  expect_true(any(is.na(res_weighted$value)))
  expect_false(any(res_plain$value == "<NA>", na.rm = TRUE))
  expect_false(any(res_weighted$value == "<NA>", na.rm = TRUE))
})

test_that("freq() cum_valid_prop is NA for missing rows", {
  x <- c("A", "B", NA, "A")
  res <- freq(x, cum = TRUE, valid = TRUE, styled = FALSE)
  na_row <- res[is.na(res$value), ]
  expect_true(is.na(na_row$cum_valid_prop))
})

test_that("freq() styled = FALSE returns plain data.frame", {
  res <- freq(c("A", "B"), styled = FALSE)
  expect_equal(class(res), "data.frame")
  expect_false(inherits(res, "spicy_freq_table"))
})

test_that("freq() styled = FALSE strips spicy metadata attributes", {
  df <- data.frame(
    x = c("A", "B", "C"),
    w = c(1, 2, 3)
  )
  res <- freq(df, x, weights = w, cum = TRUE, styled = FALSE)
  spicy_attrs <- c(
    "digits",
    "data_name",
    "var_name",
    "var_label",
    "class_name",
    "n_total",
    "n_valid",
    "weighted",
    "rescaled",
    "weight_var"
  )
  for (a in spicy_attrs) {
    expect_null(attr(res, a), info = paste("attribute", a))
  }
  expect_setequal(
    names(attributes(res)),
    c("names", "row.names", "class")
  )
})

test_that("freq() styled = TRUE invisibly returns an object carrying metadata", {
  df <- data.frame(x = c("A", "B", "C"), w = c(1, 2, 3))
  res <- withVisible(freq(df, x, weights = w))
  expect_false(res$visible)
  expect_s3_class(res$value, "spicy_freq_table")
  expect_equal(attr(res$value, "digits"), 1)
  expect_true(isTRUE(attr(res$value, "weighted")))
  expect_equal(attr(res$value, "weight_var"), "w")
})

test_that("freq() weights from a qualified expression win over data lookup", {
  # Two data frames share a column name `w` but hold different weights.
  # freq(df1, x, weights = df2$w) must use df2$w, not df1$w.
  df1 <- data.frame(x = c("A", "B", "C"), w = c(1, 1, 1))
  df2 <- data.frame(w = c(10, 20, 30))

  res_qualified <- freq(
    df1,
    x,
    weights = df2$w,
    rescale = FALSE,
    styled = FALSE
  )
  res_bare <- freq(df1, x, weights = w, rescale = FALSE, styled = FALSE)

  expect_equal(sum(res_qualified$n), 60)
  expect_equal(sum(res_bare$n), 3)

  # Qualified expression also works with [[ ]]
  res_brackets <- freq(
    df1,
    x,
    weights = df2[["w"]],
    rescale = FALSE,
    styled = FALSE
  )
  expect_equal(sum(res_brackets$n), 60)
})

test_that("freq() cum_prop stays monotonic with sort and missing values", {
  # When sort pushes frequent categories to the top, the NA row must still
  # be placed at the end so the valid block's cum_prop rises monotonically
  # from 0 toward the total-valid share.
  x <- c("A", "A", "A", "B", "B", "C", NA, NA)
  res <- freq(x, sort = "-", cum = TRUE, styled = FALSE)

  na_pos <- which(is.na(res$value))
  expect_equal(na_pos, nrow(res))

  valid_cum <- res$cum_prop[seq_len(nrow(res) - 1L)]
  expect_true(all(diff(valid_cum) >= 0))
  expect_equal(res$cum_prop[nrow(res)], 1)

  valid_cv <- res$cum_valid_prop[seq_len(nrow(res) - 1L)]
  expect_true(all(diff(valid_cv) >= 0))
  expect_equal(valid_cv[length(valid_cv)], 1)
  expect_true(is.na(res$cum_valid_prop[nrow(res)]))
})

test_that("freq() drops unused factor levels from the output", {
  # Documented Stata-style behavior: declared-but-unobserved levels
  # are not shown. Schema-level inspection lives in varlist() /
  # code_book(factor_levels = "all").
  x <- factor(c("a", "b", "a"), levels = c("a", "b", "c"))
  res <- freq(x, styled = FALSE)

  expect_equal(nrow(res), 2L)
  expect_setequal(res$value, c("a", "b"))
  expect_false("c" %in% res$value)
})

test_that("freq() factor_levels = 'all' keeps unused factor levels with n = 0", {
  x <- factor(c("a", "b", "a"), levels = c("a", "b", "c"))
  res <- freq(x, factor_levels = "all", styled = FALSE)

  expect_equal(nrow(res), 3L)
  expect_setequal(res$value, c("a", "b", "c"))
  expect_equal(res$n[res$value == "c"], 0)
  expect_equal(res$prop[res$value == "c"], 0)
  # Sum still equals length(x): the n = 0 row contributes nothing.
  expect_equal(sum(res$n), 3L)
})

test_that("freq() factor_levels = 'all' keeps unused labelled levels", {
  skip_if_not_installed("labelled")
  x <- labelled::labelled(
    c(1, 2, 1),
    labels = c(Low = 1, Mid = 2, High = 3)
  )
  res <- freq(x, factor_levels = "all", styled = FALSE)

  expect_equal(nrow(res), 3L)
  expect_true(any(grepl("High", res$value)))
  expect_equal(res$n[grepl("High", res$value)], 0)
})

test_that("freq() factor_levels = 'all' keeps weighted unused levels at n = 0", {
  # tapply() returns NA for empty groups; freq() must coerce those
  # to 0 so the table shows the unused level with `n = 0` rather
  # than a stray `n = NA` row.
  df <- data.frame(
    x = factor(c("a", "b", "a"), levels = c("a", "b", "c")),
    w = c(2, 3, 5)
  )
  res <- freq(
    df,
    x,
    weights = w,
    factor_levels = "all",
    rescale = FALSE,
    styled = FALSE
  )

  expect_equal(nrow(res), 3L)
  expect_equal(res$n[res$value == "c"], 0)
  expect_equal(sum(res$n), sum(df$w))
})

test_that("freq() factor_levels = 'all' interacts cleanly with sort and cum", {
  x <- factor(c("a", "b", "a"), levels = c("a", "b", "c"))

  # sort = "+" (freq ascending): unused level (n = 0) comes first
  res_asc <- freq(x, factor_levels = "all", sort = "+", styled = FALSE)
  expect_equal(res_asc$value[1], "c")

  # sort = "-" (freq descending): unused level lands at the end
  res_desc <- freq(x, factor_levels = "all", sort = "-", styled = FALSE)
  expect_equal(res_desc$value[nrow(res_desc)], "c")

  # cum_prop monotone, reaches 1, stays at 1 across the n = 0 row
  res_cum <- freq(x, factor_levels = "all", cum = TRUE, styled = FALSE)
  expect_true(all(diff(res_cum$cum_prop) >= 0))
  expect_equal(res_cum$cum_prop[nrow(res_cum)], 1)
})

test_that("freq() factor_levels = 'all' interacts with na_val", {
  # na_val recodes some values as NA, but the original levels stay
  # in the factor's `levels` attribute — so factor_levels = "all"
  # still shows them with n = 0 alongside the new NA row.
  skip_if_not_installed("labelled")
  x <- labelled::labelled(
    c(1, 2, 1),
    labels = c(Low = 1, Mid = 2, High = 3)
  )
  res <- freq(x, na_val = 1, factor_levels = "all", styled = FALSE)

  # Low (recoded to NA, n = 0), Mid (1), High (unused, n = 0), NA (2)
  expect_equal(nrow(res), 4L)
  expect_true(any(is.na(res$value)))
  expect_equal(sum(res$n[!is.na(res$value)]), 1)
  expect_equal(res$n[is.na(res$value)], 2)
})

test_that("freq() factor_levels = 'all' is a no-op for plain character vectors", {
  # Numeric / character / logical have no declared levels — the
  # argument has nothing to do, output identical to "observed".
  res_obs <- freq(c("a", "b", "a"), factor_levels = "observed", styled = FALSE)
  res_all <- freq(c("a", "b", "a"), factor_levels = "all", styled = FALSE)

  expect_equal(res_obs, res_all)
})

test_that("freq() validates factor_levels", {
  expect_error(
    freq(c(1, 2), factor_levels = "bogus"),
    'must be "observed" or "all"',
    fixed = TRUE
  )
  expect_error(
    freq(c(1, 2), factor_levels = NA_character_),
    'must be "observed" or "all"',
    fixed = TRUE
  )
  expect_error(
    freq(c(1, 2), factor_levels = c("observed", "bad")),
    'must be "observed" or "all"',
    fixed = TRUE
  )
})

test_that("freq() default factor_levels preserves current behavior", {
  # Regression guard: omitting the argument equals "observed".
  x <- factor(c("a", "b", "a"), levels = c("a", "b", "c"))
  expect_equal(
    freq(x, styled = FALSE),
    freq(x, factor_levels = "observed", styled = FALSE)
  )
})

test_that("freq() drops unused labelled values from the output", {
  skip_if_not_installed("labelled")

  x <- labelled::labelled(
    c(1, 2, 1),
    labels = c(Low = 1, Mid = 2, High = 3)
  )
  res <- freq(x, styled = FALSE)

  expect_equal(nrow(res), 2L)
  expect_false(any(grepl("High", res$value)))
})

test_that("freq() handles a single-level factor", {
  f <- factor(c("only", "only", "only"))
  res <- freq(f, styled = FALSE)
  expect_equal(nrow(res), 1L)
  expect_equal(res$n, 3)
  expect_equal(res$prop, 1)
})

test_that("freq() handles an all-NA input", {
  x <- c(NA, NA, NA)
  res <- freq(x, styled = FALSE)
  expect_true(all(is.na(res$value)))
  expect_equal(sum(res$n), 3)
  expect_equal(res$prop, 1)
  expect_true(all(is.na(res$valid_prop)))
})

test_that("freq() prints integer counts even with fractional weights", {
  # Matches SPSS/Stata/SAS convention: the Freq. column is always rendered
  # as integers, regardless of `digits` (which controls percentages only).
  df <- data.frame(x = c("A", "B", "C"), w = c(1.1, 2.2, 3.3))
  out1 <- capture.output(freq(df, x, weights = w, digits = 1, rescale = FALSE))
  out3 <- capture.output(freq(df, x, weights = w, digits = 3, rescale = FALSE))
  expect_false(any(grepl("1\\.1\\b", out1)))
  expect_false(any(grepl("1\\.100", out3)))
  expect_true(any(grepl("\\b1\\b", out1)))
  expect_true(any(grepl("\\b2\\b", out1)))
  expect_true(any(grepl("\\b3\\b", out1)))
})

test_that("freq() labelled_levels accepts p/l/v shortcuts via partial matching", {
  skip_if_not_installed("labelled")
  x <- labelled::labelled(c(1, 2, 3), labels = c(Low = 1, Mid = 2, High = 3))
  expect_true(any(grepl(
    "\\[1\\]",
    freq(x, labelled_levels = "p", styled = FALSE)$value
  )))
  expect_true(all(
    !grepl("\\[", freq(x, labelled_levels = "l", styled = FALSE)$value)
  ))
  expect_true(any(
    freq(x, labelled_levels = "v", styled = FALSE)$value %in% c("1", "2", "3")
  ))
})
