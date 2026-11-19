# ---------------------------------------------------------------------------
# The refusal log of the sweeps over helper-class-fixtures.R. A sweep notes
# each classed condition it meets (`.refusal_note()`), and a snapshot holds
# the deduplicated table (`.refusal_snapshot()`): one entry per message,
# with its condition classes and the sorted list of the fixtures and values
# that receive it. The snapshots freeze the text and the class of every
# refusal, so that a change to the validation shows in a diff.
#
# Nothing in the table depends on the machine or on the order of the run:
# entries and receivers are sorted in the C locale, and a character
# outside ASCII is written as <U+XXXX>, as R writes it in a locale that
# cannot show it.
# ---------------------------------------------------------------------------

.refusal_logs <- new.env(parent = emptyenv())

# Start the log `sweep` afresh (top of the test file that fills it).
.refusal_reset <- function(sweep) {
  .refusal_logs[[sweep]] <- list(rows = list(), ran = character())
}

# The fixture `id` was swept: the snapshot waits for every fixture.
.refusal_ran <- function(sweep, id) {
  .refusal_logs[[sweep]]$ran <- union(.refusal_logs[[sweep]]$ran, id)
}

# The classes of a condition that say what it is: the leaf classes of
# spicy, without the generic ones of rlang and base R.
.refusal_classes <- function(cnd) {
  setdiff(
    class(cnd),
    c(
      "rlang_error",
      "rlang_warning",
      "simpleError",
      "simpleWarning",
      "error",
      "warning",
      "condition"
    )
  )
}

.refusal_note <- function(sweep, id, value, cnd, kind = "error") {
  log <- .refusal_logs[[sweep]]
  log$rows[[length(log$rows) + 1L]] <- list(
    kind = kind,
    classes = paste(.refusal_classes(cnd), collapse = ", "),
    message = conditionMessage(cnd),
    receiver = paste0(id, ": ", value)
  )
  .refusal_logs[[sweep]] <- log
}

.refusal_ascii <- function(x) {
  x <- enc2utf8(x)
  vapply(
    x,
    function(s) {
      cp <- utf8ToInt(s)
      if (all(cp < 128L)) {
        return(s)
      }
      paste(
        ifelse(
          cp < 128L,
          vapply(cp, intToUtf8, character(1)),
          sprintf("<U+%04X>", cp)
        ),
        collapse = ""
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# The table as text: an entry per message, its receivers grouped by value.
.refusal_render <- function(rows) {
  if (length(rows) == 0L) {
    return("No refusal.")
  }
  df <- data.frame(
    kind = vapply(rows, `[[`, character(1), "kind"),
    classes = vapply(rows, `[[`, character(1), "classes"),
    message = .refusal_ascii(vapply(rows, `[[`, character(1), "message")),
    receiver = vapply(rows, `[[`, character(1), "receiver")
  )
  df <- unique(df)
  key <- paste(df$kind, df$classes, df$message, sep = "\r")
  keys <- sort(unique(key), method = "radix")
  out <- character()
  for (k in keys) {
    sub <- df[key == k, , drop = FALSE]
    out <- c(
      out,
      "----",
      paste0(sub$kind[1L], ": ", sub$classes[1L]),
      paste0("  ", strsplit(sub$message[1L], "\n", fixed = TRUE)[[1L]]),
      sprintf("Received by %d:", nrow(sub)),
      .refusal_receivers(sub$receiver)
    )
  }
  out
}

# The receivers "fixture: value", one line per value, the fixtures sorted
# and wrapped.
.refusal_receivers <- function(receiver) {
  id <- sub(": .*", "", receiver)
  value <- sub("^[^:]*: ", "", receiver)
  out <- character()
  for (v in sort(unique(value), method = "radix")) {
    ids <- sort(id[value == v], method = "radix")
    out <- c(
      out,
      strwrap(
        paste0(v, ": ", paste(ids, collapse = ", ")),
        width = 76,
        indent = 2,
        exdent = 6
      )
    )
  }
  out
}

# Snapshot the log of `sweep` once every fixture of `ids` was swept; skip
# otherwise (a package missing), since a partial table is not the table.
.refusal_snapshot <- function(sweep, ids) {
  missing <- setdiff(ids, .refusal_logs[[sweep]]$ran)
  if (length(missing) > 0L) {
    testthat::skip(paste(
      "fixtures not swept:",
      paste(missing, collapse = ", ")
    ))
  }
  rows <- Filter(
    function(r) sub(":.*", "", r$receiver) %in% ids,
    .refusal_logs[[sweep]]$rows
  )
  testthat::expect_snapshot(cat(.refusal_render(rows), sep = "\n"))
}

# The fixtures outside Stan, and the two Stan ones (local runs only, so
# they keep a snapshot of their own that CI never rewrites).
.refusal_ids <- function(stan = FALSE) {
  is_stan <- vapply(.CLASS_FIXTURES, function(f) isTRUE(f$stan), logical(1))
  names(.CLASS_FIXTURES)[is_stan == stan]
}
