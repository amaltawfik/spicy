# Shared internals for declared missing values (user-declared missing
# values carried as metadata on imported vectors): `na_values` /
# `na_range` declarations on `haven_labelled_spss` vectors, and
# tagged missing values created by `haven::tagged_na()`. Every
# primitive used here comes from the labelled package (Imports), which
# re-exports the tagged-NA tools, so no optional dependency is
# involved. The user-facing contract these helpers implement is the
# `user_na` argument documented in the "Declared missing values"
# section of `?freq`.

# TRUE when `x` carries any declared-missing metadata worth acting on:
# a `haven_labelled_spss` vector with `na_values` / `na_range`, or a
# double vector (labelled or plain) containing tagged NAs.
.has_user_na <- function(x) {
  if (inherits(x, "haven_labelled_spss")) {
    if (
      !is.null(attr(x, "na_values", exact = TRUE)) ||
        !is.null(attr(x, "na_range", exact = TRUE))
    ) {
      return(TRUE)
    }
  }
  if (is.double(x) && any(labelled::is_tagged_na(x))) {
    return(TRUE)
  }
  FALSE
}


# Logical mask of declared-missing positions in `x`: `na_values` exact
# matches, values inside the inclusive `na_range`, and tagged NAs.
# Returns all-FALSE for vectors without declarations (including
# non-atomic columns, which cannot carry them).
.user_na_mask <- function(x) {
  if (!is.atomic(x)) {
    return(rep(FALSE, length(x)))
  }
  mask <- if (inherits(x, "haven_labelled_spss")) {
    labelled::is_user_na(x)
  } else {
    rep(FALSE, length(x))
  }
  if (is.double(x)) {
    mask <- mask | labelled::is_tagged_na(x)
  }
  mask
}


# Convert declared-missing values to regular NA and drop the
# declaration (the `user_na = TRUE` convention for computations that
# cannot rely on haven's `is.na()` dispatch, e.g. `factor()`,
# `as.matrix()`, `complete.cases()`). Value labels attached to the
# declared codes are dropped along with them, so they cannot resurface
# as empty valid levels. Tagged NAs are left as they are: they are
# already `NA` for every consumer.
.user_na_to_na <- function(x) {
  if (inherits(x, "haven_labelled_spss")) {
    x <- labelled::user_na_to_na(x)
  }
  x
}


# Drop the `na_values` / `na_range` declaration but KEEP the declared
# codes as valid values (the `user_na = FALSE` escape hatch). Value
# labels are preserved. Tagged NAs are unaffected: they are genuine
# missing values and cannot be made valid.
.user_na_zap <- function(x) {
  if (inherits(x, "haven_labelled_spss")) {
    x <- labelled::remove_user_na(x)
  }
  x
}


# Per-declared-value display strings and (optionally weighted) counts
# for the observed declared-missing values of `x`. `x` must be the
# declared-missing subset (i.e. `x[.user_na_mask(x)]`), with `weights`
# subset the same way (or NULL for unweighted counts). Rows are
# ordered: declared codes in ascending order, then tagged NAs by tag.
# `labelled_levels` follows freq()'s vocabulary ("prefixed", "labels",
# "values") and controls how a declared code with a value label is
# displayed; codes without labels display as the bare code in every
# mode, and tagged NAs display as `NA(a)` (with their label when one
# is declared).
.user_na_info <- function(x, weights = NULL, labelled_levels = "prefixed") {
  if (length(x) == 0L) {
    return(data.frame(
      value = character(0),
      n = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  labs <- attr(x, "labels", exact = TRUE)
  tagged <- if (is.double(x)) {
    labelled::is_tagged_na(x)
  } else {
    rep(FALSE, length(x))
  }
  codes <- unclass(x)
  attributes(codes) <- NULL

  count_of <- function(sel) {
    if (is.null(weights)) sum(sel) else sum(weights[sel])
  }

  label_for <- function(code) {
    if (is.null(labs)) {
      return(NA_character_)
    }
    lab_codes <- unname(unclass(labs))
    hit <- which(!is.na(lab_codes) & lab_codes == code)
    if (length(hit) > 0L) names(labs)[[hit[[1L]]]] else NA_character_
  }

  display_for <- function(code_str, lab) {
    if (is.na(lab)) {
      return(code_str)
    }
    switch(
      labelled_levels,
      labels = lab,
      values = code_str,
      paste0("[", code_str, "] ", lab)
    )
  }

  rows_value <- character(0)
  rows_n <- numeric(0)

  plain_codes <- codes[!tagged]
  if (length(plain_codes) > 0L) {
    uniq <- unique(plain_codes)
    if (length(uniq) > 1L) {
      uniq <- tryCatch(sort(uniq, method = "radix"), error = function(e) uniq)
    }
    for (v in uniq) {
      sel <- !tagged & !is.na(codes) & codes == v
      rows_value <- c(
        rows_value,
        display_for(as.character(v), label_for(v))
      )
      rows_n <- c(rows_n, count_of(sel))
    }
  }

  if (any(tagged)) {
    tags <- labelled::na_tag(codes)
    tag_labs <- if (is.null(labs) || !is.double(unclass(labs))) {
      NULL
    } else {
      labs
    }
    for (tg in sort(unique(tags[tagged]))) {
      sel <- tagged & !is.na(tags) & tags == tg
      lab <- NA_character_
      if (!is.null(tag_labs)) {
        lab_tags <- labelled::na_tag(unclass(tag_labs))
        hit <- which(!is.na(lab_tags) & lab_tags == tg)
        if (length(hit) > 0L) {
          lab <- names(tag_labs)[[hit[[1L]]]]
        }
      }
      tag_str <- paste0("NA(", tg, ")")
      rows_value <- c(rows_value, display_for(tag_str, lab))
      rows_n <- c(rows_n, count_of(sel))
    }
  }

  data.frame(value = rows_value, n = rows_n, stringsAsFactors = FALSE)
}
