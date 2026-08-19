# The structural guard behind register 55. table_regression() once
# rebound its own `labels` formal inside two warning blocks, and every
# `labels =` override on mixed fits silently vanished -- no error, no
# warning, a green suite. Per-trigger witnesses only redden when their
# trigger fires; this lint closes the whole class instead: no function
# may reassign a display-family formal it received, unless the site is
# a documented NULL-resolution on the whitelist below.

test_that("no function reassigns a display-family formal it received", {
  display_formals <- c(
    "labels",
    "title",
    "note",
    "column",
    "model_labels",
    "outcome_labels",
    "show_columns",
    "show_fit_stats"
  )
  # Every entry is a deliberate resolution of the formal's NULL (or
  # multi-mode) default onto itself, before any consumer reads it --
  # re-audited 2026-08 (labels train review). Grow this list only for
  # the same shape: `x <- resolve(x)` at the top of the function.
  whitelist <- c(
    "inline:column",
    "pivot_aligned_wide:model_labels",
    "render_regression_table:model_labels",
    "table_categorical:labels",
    "table_regression:outcome_labels",
    "table_regression:show_columns",
    "table_regression:show_fit_stats",
    "table_regression:title",
    "table_regression_uv:title"
  )

  offenders <- character(0)
  walk <- function(e, fn_name, fmls) {
    if (!length(fmls)) {
      return(invisible())
    }
    if (is.call(e)) {
      op <- e[[1L]]
      if (is.symbol(op)) {
        op_chr <- as.character(op)
        if (
          op_chr %in%
            c("<-", "<<-", "=") &&
            length(e) == 3L &&
            is.symbol(e[[2L]])
        ) {
          lhs <- as.character(e[[2L]])
          if (lhs %in% fmls) {
            offenders <<- c(offenders, paste0(fn_name, ":", lhs))
          }
        }
        if (op_chr == "function") {
          # A nested function's own formals shadow the outer ones
          # legitimately: stop tracking those names inside it.
          inner <- names(e[[2L]])
          walk(e[[3L]], fn_name, setdiff(fmls, inner))
          return(invisible())
        }
      }
      for (i in seq_along(e)) {
        child <- e[[i]]
        if (missing(child)) {
          next
        }
        walk(child, fn_name, fmls)
      }
    }
    invisible()
  }

  ns <- asNamespace("spicy")
  for (nm in ls(ns, all.names = TRUE)) {
    fn <- get(nm, envir = ns)
    if (!is.function(fn) || is.primitive(fn)) {
      next
    }
    fmls <- intersect(names(formals(fn)), display_formals)
    if (length(fmls)) {
      walk(body(fn), nm, fmls)
    }
  }

  expect_identical(
    setdiff(sort(unique(offenders)), whitelist),
    character(0)
  )
  # And the whitelist carries no stale entries: every listed site must
  # still exist, or the list slowly turns into folklore.
  expect_identical(setdiff(whitelist, offenders), character(0))
})
