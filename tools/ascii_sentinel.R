# Byte sentinel for R/.
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
# TWO rules, because the first one alone had a hole underneath it. The
# scan looked only ABOVE 0x7F, and the invisible characters do not all
# live up there: a raw U+0001 sat in `.cat_svy_block_id()` as the
# literal control byte -- correct at run time, unreadable in the source,
# and beneath the scan's floor in both directions (register n. 269).
# So a C0 CONTROL character is a hard failure too, with no allowlist at
# all: tab is the one that has a job in source text, and every other
# one belongs in the source as the same `"\uXXXX"` escape. The remedy
# is the escape, never the byte, and it costs nothing -- the string is
# identical once parsed.
#
# What the control rule does NOT cover is U+0000: `readLines()` drops
# it and the line's remainder with it. That is the one control byte R
# refuses to parse at all ("nul character not allowed"), so it can
# never ship quietly -- it fails loudly long before a byte scan, which
# is the opposite of the class this file exists for.
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

# C0 controls, for the message only -- none of them is ever allowed, so
# this list changes nothing about the verdict. Named where a name helps
# the author find the thing; the rest are reported by codepoint, which
# is all there is to see anyway.
ASCII_SENTINEL_CONTROL <- c(
  "0001" = "START OF HEADING (invisible: a raw control byte)",
  "0007" = "BELL (invisible)",
  "0008" = "BACKSPACE (invisible)",
  "000b" = "VERTICAL TAB (invisible)",
  "000c" = "FORM FEED (invisible)",
  "001b" = "ESCAPE (invisible: the lead byte of an ANSI sequence)",
  "001c" = "FILE SEPARATOR (invisible)",
  "001d" = "GROUP SEPARATOR (invisible)",
  "001e" = "RECORD SEPARATOR (invisible)",
  "001f" = "UNIT SEPARATOR (invisible)"
)

# Every disallowed character under `dirs` -- non-ASCII off the
# allowlist, and C0 control other than tab -- as a data.frame with one
# row per site: file, line, col (character offset within the line),
# codepoint, and what it is. Zero rows means the tree is clean.
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
      # Two floors, one pass. Above 0x7F the allowlist decides; below
      # 0x20 nothing does -- tab is the only control character with a
      # job in source text. LF and CR are named for the reader's sake:
      # `readLines()` has already eaten them, so they cannot appear
      # here, and the line says the rule rather than leaving it to be
      # inferred from an absence.
      hit <- which(
        (pts > 127L & !(pts %in% ok)) |
          (pts < 32L & !(pts %in% c(9L, 10L, 13L)))
      )
      if (length(hit)) {
        file <- c(file, rep(f, length(hit)))
        line <- c(line, rep(i, length(hit)))
        col <- c(col, hit)
        cp <- c(cp, pts[hit])
      }
    }
  }
  named <- c(ASCII_SENTINEL_KNOWN, ASCII_SENTINEL_CONTROL)
  what <- unname(named[sprintf("%04x", cp)])
  # A control character nobody bothered to name is still a control
  # character, and must not fall through to "not on the allowlist" --
  # there is no allowlist down there to be off.
  what[is.na(what) & !is.na(cp) & cp < 32L] <- "C0 CONTROL (invisible)"
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
