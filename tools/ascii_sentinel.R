# Non-ASCII sentinel for R/.
#
# The class this closes is the INVISIBLE one. Three U+00A0 NO-BREAK
# SPACEs got into R/ during the outcome campaign: they read as ordinary
# spaces in the editor, in the diff and in the review, and only a byte
# scan told them apart. A no-break space inside a string is a different
# string; inside code it is a parse error at a column nothing points at.
#
# So the rule is an ALLOWLIST, not a denylist: every non-ASCII
# codepoint that appears in R/ is named here with the reason it earns
# its place, and anything else is a hard failure. No member of the list
# is invisible, and that is the property worth keeping -- a character
# that has to be invisible belongs in the source as a `"\uXXXX"`
# escape, the way `intToUtf8(0x200B)` is already written elsewhere in
# the package.
#
# The lists are keyed by codepoint rather than by the character, so
# this file stays pure ASCII and cannot fail its own rule.
#
# Sourced by tools/run_suite.R, which runs it before the suite: a
# 40-minute run is too long to spend finding this out at the end.

# codepoint -> what it is, and where it earns its place in R/
ASCII_SENTINEL_ALLOWED <- c(
  "00a7" = "SECTION SIGN, design-doc and APA Manual cross-references",
  "00b1" = "PLUS-MINUS SIGN, the `est +/- z * SE` prose",
  "00b2" = "SUPERSCRIPT TWO, R-squared",
  "00b3" = "SUPERSCRIPT THREE, the gt column-name collision example",
  "00d7" = "MULTIPLICATION SIGN, the `z x SE` prose",
  "00e9" = "LATIN SMALL E WITH ACUTE, the accented-label example",
  "03b2" = "GREEK SMALL BETA, the standardised-coefficient token",
  "03c6" = "GREEK SMALL PHI, the betareg precision parameter",
  "2014" = "EM DASH",
  "2248" = "ALMOST EQUAL TO"
)

# The usual suspects, named so the message can say what the eye cannot.
ASCII_SENTINEL_KNOWN <- c(
  "00a0" = "NO-BREAK SPACE (invisible: reads as a plain space)",
  "00ad" = "SOFT HYPHEN (invisible)",
  "2002" = "EN SPACE (invisible)",
  "2003" = "EM SPACE (invisible)",
  "2007" = "FIGURE SPACE (invisible)",
  "2009" = "THIN SPACE (invisible)",
  "200b" = "ZERO WIDTH SPACE (invisible)",
  "202f" = "NARROW NO-BREAK SPACE (invisible)",
  "feff" = "ZERO WIDTH NO-BREAK SPACE / BOM (invisible)",
  "2013" = "EN DASH (reads as a hyphen)",
  "2018" = "LEFT SINGLE QUOTATION MARK (reads as an apostrophe)",
  "2019" = "RIGHT SINGLE QUOTATION MARK (reads as an apostrophe)",
  "201c" = "LEFT DOUBLE QUOTATION MARK (reads as a quote)",
  "201d" = "RIGHT DOUBLE QUOTATION MARK (reads as a quote)"
)

# Every disallowed non-ASCII character under `dirs`, as a data.frame
# with one row per site: file, line, col (character offset within the
# line), codepoint, and what it is. Zero rows means the tree is clean.
ascii_sentinel_sites <- function(dirs = "R", allowed = ASCII_SENTINEL_ALLOWED) {
  files <- list.files(
    dirs,
    pattern = "[.][Rr]$",
    full.names = TRUE,
    recursive = TRUE
  )
  ok <- strtoi(names(allowed), 16L)
  file <- character(0)
  line <- integer(0)
  col <- integer(0)
  cp <- integer(0)
  for (f in files) {
    txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
    for (i in seq_along(txt)) {
      pts <- utf8ToInt(txt[i])
      # NA means the line is not valid UTF-8 at all -- a byte problem
      # of its own, reported at column 1 rather than skipped.
      if (length(pts) == 1L && is.na(pts)) {
        file <- c(file, f)
        line <- c(line, i)
        col <- c(col, 1L)
        cp <- c(cp, NA_integer_)
        next
      }
      hit <- which(pts > 127L & !(pts %in% ok))
      if (length(hit)) {
        file <- c(file, rep(f, length(hit)))
        line <- c(line, rep(i, length(hit)))
        col <- c(col, hit)
        cp <- c(cp, pts[hit])
      }
    }
  }
  what <- unname(ASCII_SENTINEL_KNOWN[sprintf("%04x", cp)])
  what[is.na(what)] <- "not on the allowlist"
  what[is.na(cp)] <- "not valid UTF-8"
  data.frame(
    file = file,
    line = line,
    col = col,
    codepoint = cp,
    what = what,
    stringsAsFactors = FALSE
  )
}

# One actionable line per site: the editor can jump to file:line:col,
# and the codepoint says which character to look for once there.
ascii_sentinel_report <- function(sites) {
  sprintf(
    "%s:%d:%d: %s -- %s",
    sites$file,
    sites$line,
    sites$col,
    ifelse(
      is.na(sites$codepoint),
      "<bad bytes>",
      sprintf("U+%04X", sites$codepoint)
    ),
    sites$what
  )
}
