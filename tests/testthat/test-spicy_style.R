# Journal styles, and the typography a language brings with it.
#
# One block per theme, checking the rules its registry entry claims --
# one at a time, on a fixed fit, against the console text a reader
# would see. A theme that stops honouring one of its own documented
# rules fails here by name.
#
# Sources for every rule: dev/journal_styles_sources.md and the
# `rules` field of the registry entry, quoted above each block.
#
# The locale block checks the other half of the resolution: a language
# carries its own typography, under any style and under any argument.

fixed_fit <- function() {
  lm(mpg ~ wt + hp, data = mtcars)
}

fixed_data <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$am <- factor(d$am, labels = c("auto", "manual"))
  d
}

# The rendered console text of a table, as one string.
console <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}

# The table itself, with the print the constructors do swallowed.
quietly <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(expr)))
  res
}

# Put a style's argument-less levers in force the way a table call
# does, so the low-level formatters can be unit-tested directly.
# Returns the resolved style (its `p_digits` is what the table
# function would have used).
local_style <- function(style, env = parent.frame()) {
  st <- spicy:::.style_resolve(style)
  spicy:::.style_push(st[intersect(names(st), spicy:::.STYLE_FMT_FIELDS)])
  withr::defer(spicy:::.style_pop(), envir = env)
  st
}


# ---- Constructor ---------------------------------------------------------

test_that("spicy_style() keeps only the levers that were set", {
  s <- spicy_style(decimal_mark = ",", p_style = "standard")
  expect_s3_class(s, "spicy_style")
  expect_identical(sort(names(s)), sort(c("decimal_mark", "p_style")))
  expect_null(attr(s, "provenance"))
})

test_that("spicy_style() validates every field hard", {
  expect_error(spicy_style(p_style = "chicago"), class = "spicy_invalid_input")
  expect_error(spicy_style(p_digits = 0), class = "spicy_invalid_input")
  expect_error(spicy_style(p_digits = 2.5), class = "spicy_invalid_input")
  expect_error(spicy_style(p_sigfig = -1), class = "spicy_invalid_input")
  expect_error(spicy_style(p_floor = 0), class = "spicy_invalid_input")
  expect_error(spicy_style(p_floor = 1), class = "spicy_invalid_input")
  expect_error(spicy_style(p_floor = "small"), class = "spicy_invalid_input")
  expect_error(spicy_style(decimal_mark = ".."), class = "spicy_invalid_input")
  expect_error(spicy_style(decimal_mark = 1), class = "spicy_invalid_input")
  expect_error(spicy_style(ci_sep = c("a", "b")), class = "spicy_invalid_input")
  expect_error(spicy_style(ci_sep = NA), class = "spicy_invalid_input")
  expect_error(spicy_style(ci_brackets = "["), class = "spicy_invalid_input")
  expect_error(spicy_style(stars = 0.05), class = "spicy_invalid_input")
  expect_error(spicy_style(stars = TRUE), class = "spicy_invalid_input")
  expect_error(spicy_style(stars = c("*" = 2)), class = "spicy_invalid_input")
  expect_error(spicy_style(digits = -1), class = "spicy_invalid_input")
  expect_error(spicy_style(v_digits = "two"), class = "spicy_invalid_input")
  expect_error(
    spicy_style(effect_size_digits = -1),
    class = "spicy_invalid_input"
  )
  expect_error(spicy_style(fit_digits = 1.5), class = "spicy_invalid_input")
  expect_error(spicy_style(ic_digits = NA), class = "spicy_invalid_input")
  expect_error(
    spicy_style(percent_digits = "one"),
    class = "spicy_invalid_input"
  )
})

test_that("spicy_style() accepts the well-formed values of each lever", {
  s <- spicy_style(
    p_style = "standard",
    p_digits = 4,
    p_floor = 1e-4,
    p_sigfig = 2,
    decimal_mark = "·",
    ci_sep = " to ",
    ci_brackets = c("(", ")"),
    stars = c("*" = 0.05),
    digits = 3,
    effect_size_digits = 2,
    fit_digits = 2,
    ic_digits = 1,
    percent_digits = 0,
    v_digits = 2
  )
  expect_length(s, 14L)
  expect_identical(s$p_digits, 4L)
  expect_identical(s$ci_brackets, c("(", ")"))
  expect_identical(spicy_style(stars = FALSE)$stars, FALSE)
})

test_that("spicy_style() validates p_bands as a band table", {
  expect_error(spicy_style(p_bands = list()), class = "spicy_invalid_input")
  expect_error(spicy_style(p_bands = 0.01), class = "spicy_invalid_input")
  expect_error(
    spicy_style(p_bands = list(c(0.01, 3))),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy_style(p_bands = list(c(0.01, 3), c(0.001, 2), c(Inf, 2))),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy_style(p_bands = list(c(0, 3), c(Inf, 2))),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy_style(p_bands = list(c(0.01, 2.5), c(Inf, 2))),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy_style(p_bands = list(c(0.01, 3, 4), c(Inf, 2))),
    class = "spicy_invalid_input"
  )
  ok <- spicy_style(p_bands = list(c(0.01, 3), c(Inf, 2)))
  expect_length(ok$p_bands, 2L)
})

test_that("p_bands and p_sigfig are mutually exclusive", {
  expect_error(
    spicy_style(p_bands = list(c(Inf, 2)), p_sigfig = 2),
    class = "spicy_invalid_input"
  )
})

test_that("a misspelt lever is an error, never a silent no-op", {
  expect_error(spicy_style(decimalmark = ","), class = "spicy_invalid_input")
  expect_error(spicy_style(p_digit = 2), class = "spicy_invalid_input")
})

test_that("spicy_style() composes on a base theme and says it was modified", {
  s <- spicy_style("lancet", ci_sep = " to ")
  expect_identical(s$ci_sep, " to ")
  expect_identical(s$decimal_mark, "·") # the theme's lever survives
  expect_identical(attr(s, "provenance")$overrides, "ci_sep")
  expect_match(console(s), "MODIFIED")
})

test_that("composing on a base drops the exclusive p-precision sibling", {
  s <- spicy_style("lancet", p_bands = list(c(0.01, 3), c(Inf, 2)))
  expect_null(s$p_sigfig)
  expect_length(s$p_bands, 2L)
  s2 <- spicy_style("jama", p_sigfig = 2)
  expect_null(s2$p_bands)
  expect_identical(s2$p_sigfig, 2L)
})

test_that("a composed style can itself be a base", {
  s <- spicy_style(spicy_style(decimal_mark = ","), p_digits = 2)
  expect_identical(s$decimal_mark, ",")
  expect_identical(s$p_digits, 2L)
})


# ---- Registry ------------------------------------------------------------

test_that("every registry entry carries its provenance and rule list", {
  for (nm in spicy_style_names()) {
    s <- spicy_style(nm)
    prov <- attr(s, "provenance")
    expect_s3_class(s, "spicy_style")
    expect_identical(prov$name, nm)
    expect_true(nzchar(prov$journal), info = nm)
    expect_true(nzchar(prov$document), info = nm)
    expect_match(prov$url, "^https://", info = nm)
    expect_true(nzchar(prov$date), info = nm)
    expect_true(length(prov$rules) >= 1L, info = nm)
    # A theme that encodes nothing would be a name engaging an
    # institution for free: refuse it.
    expect_true(length(unclass(s)) >= 1L, info = nm)
    # And it must print its provenance, which is what "a theme
    # commits" means in practice.
    txt <- console(s)
    expect_match(txt, "not editorial conformity", info = nm)
    expect_match(txt, prov$url, fixed = TRUE, info = nm)
  }
})

test_that("the shipped themes are exactly the sourced ones", {
  # Every name here is a JOURNAL. French typography was the one entry
  # that was not, and it left: it is the language's locale now.
  expect_identical(
    spicy_style_names(),
    c("jama", "nejm", "lancet", "annals", "apa", "aer")
  )
})

test_that("an unknown style name is a hard error listing what exists", {
  fit <- fixed_fit()
  expect_error(
    table_regression(fit, style = "bmj"),
    class = "spicy_invalid_input"
  )
  err <- tryCatch(
    table_regression(fit, style = "qjecon"),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "Unknown table style")
  expect_match(err, "jama")
  expect_match(err, "lancet")
  expect_error(spicy_style("econometrica"), class = "spicy_invalid_input")
  expect_error(
    table_categorical(fixed_data(), "cyl", style = "qje"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous(mtcars, mpg, style = 42),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_lm(fixed_data(), mpg, by = am, style = "epidemiology"),
    class = "spicy_invalid_input"
  )
})


# ---- Theme by theme, rule by rule ---------------------------------------

test_that("jama: p on 2 decimals, 3 below .01, floor <.001, no leading zero", {
  # "All P values should be reported to exact numbers to 2 digits past
  #  the decimal point ... For values lower than .01, present the P
  #  value to 3 digits. Express any values lower than .001 as P<.001."
  st <- local_style("jama")
  d <- st$p_digits
  expect_identical(d, 2L)
  expect_identical(spicy:::format_p_value(0.08, ".", d), ".08")
  expect_identical(spicy:::format_p_value(0.4321, ".", d), ".43")
  expect_identical(spicy:::format_p_value(0.0032, ".", d), ".003")
  expect_identical(spicy:::format_p_value(0.00999, ".", d), ".010")
  expect_identical(spicy:::format_p_value(0.0004, ".", d), "<.001")
  # the band boundary itself is not "lower than .01"
  expect_identical(spicy:::format_p_value(0.01, ".", d), ".01")
})

test_that("jama on a real table", {
  txt <- console(table_regression(fixed_fit(), style = "jama"))
  expect_match(txt, "<\\.001") # floor, no leading zero
  expect_false(grepl("<0.001", txt, fixed = TRUE))
})

test_that("format_p_value's `leading_zero` overrides the style's answer", {
  # The hook `cross_tab()` speaks through: it has no style layer, so
  # under a comma mark it asks for the zero itself. NULL -- what every
  # other caller passes -- still asks the style.
  st <- local_style("jama")
  expect_identical(spicy:::format_p_value(0.08, ".", 3L), ".08")
  expect_identical(
    spicy:::format_p_value(0.08, ".", 3L, leading_zero = TRUE),
    "0.08"
  )
  expect_identical(
    spicy:::format_p_value(0.08, ".", 3L, leading_zero = NULL),
    ".08"
  )
  expect_identical(st$p_digits, 2L)
})

test_that("format_p_value's `leading_zero = FALSE` strips under a keeping style", {
  local_style("nejm")
  expect_identical(spicy:::format_p_value(0.08, ".", 3L), "0.08")
  expect_identical(
    spicy:::format_p_value(0.08, ".", 3L, leading_zero = FALSE),
    ".08"
  )
})

test_that("nejm: tiered p with the leading zero kept", {
  # "In general, P values larger than 0.01 should be reported to two
  #  decimal places, and those between 0.01 and 0.001 to three decimal
  #  places; P values smaller than 0.001 should be reported as
  #  P<0.001." (Statistical Reporting Guidelines, A.1.g -- full text
  #  read in a browser by the maintainer, 2026-08-14.) The rule writes
  #  "0.01" / "0.001": leading zero kept, unlike JAMA.
  st <- local_style("nejm")
  d <- st$p_digits
  expect_identical(d, 2L)
  expect_identical(spicy:::format_p_value(0.08, ".", d), "0.08")
  expect_identical(spicy:::format_p_value(0.4321, ".", d), "0.43")
  expect_identical(spicy:::format_p_value(0.0032, ".", d), "0.003")
  expect_identical(spicy:::format_p_value(0.00999, ".", d), "0.010")
  expect_identical(spicy:::format_p_value(0.0004, ".", d), "<0.001")
  # the band boundary itself is not "lower than 0.01"
  expect_identical(spicy:::format_p_value(0.01, ".", d), "0.01")
  # A.1.h pins the association digits at spicy's default.
  expect_identical(st$digits, 2L)
})

test_that("nejm on a real table", {
  txt <- console(table_regression(fixed_fit(), style = "nejm"))
  expect_match(txt, "<0\\.001", perl = TRUE) # floor, leading zero kept
  expect_false(grepl(" <.001", txt, fixed = TRUE))
})

test_that("lancet: midline dot, 2 significant figures capped at 4 dp", {
  # "Type decimal points midline (ie, 23<midline dot>4, not 23.4)."
  # "Supply p values to two significant figures (capped at four
  #  decimal places), or p<0<midline dot>0001."
  st <- local_style("lancet")
  dot <- "·"
  expect_identical(st$decimal_mark, dot)
  d <- st$p_digits
  expect_identical(d, 4L)
  expect_identical(
    spicy:::format_p_value(0.034, dot, d),
    paste0("0", dot, "034")
  )
  expect_identical(
    spicy:::format_p_value(0.0021, dot, d),
    paste0("0", dot, "0021")
  )
  expect_identical(
    spicy:::format_p_value(0.123, dot, d),
    paste0("0", dot, "12")
  )
  expect_identical(spicy:::format_p_value(0.5, dot, d), paste0("0", dot, "50"))
  # capped at four decimal places
  expect_identical(
    spicy:::format_p_value(0.00021, dot, d),
    paste0("0", dot, "0002")
  )
  # the floor, with its leading zero
  expect_identical(
    spicy:::format_p_value(0.00005, dot, d),
    paste0("<0", dot, "0001")
  )
})

test_that("lancet on a real table: dot, floor, en dash", {
  txt <- console(table_regression(fixed_fit(), style = "lancet"))
  expect_match(txt, "37·23") # midline dot on EVERY number
  expect_false(grepl("37.23", txt, fixed = TRUE))
  expect_match(txt, "<0·0001") # floor with leading zero
  expect_match(txt, "–") # en dash between the CI bounds
})

test_that("lancet: the coverage percentage takes the midline dot too", {
  # Decision 27: "97·5% CI" -- the percentage is a number in a
  # label, and the Lancet mark reaches it like every other number.
  txt <- console(table_regression(
    fixed_fit(),
    style = "lancet",
    ci_level = 0.975
  ))
  expect_match(txt, "97·5% CI", fixed = TRUE)
  expect_false(grepl("97.5% CI", txt, fixed = TRUE))
  # An integer coverage has no decimal: the header is byte-identical.
  txt95 <- console(table_regression(fixed_fit(), style = "lancet"))
  expect_match(txt95, "95% CI", fixed = TRUE)
})

test_that("annals: 3 decimals up to 0.20, 2 above, leading zero kept", {
  # "For P values between 0.001 and 0.20, please report the value to
  #  the nearest thousandth. For P values greater than 0.20, please
  #  report the value to the nearest hundredth. For P values less than
  #  0.001, report as 'P<0.001.'"
  st <- local_style("annals")
  d <- st$p_digits
  expect_identical(d, 3L)
  expect_identical(spicy:::format_p_value(0.153, ".", d), "0.153")
  expect_identical(spicy:::format_p_value(0.0456, ".", d), "0.046")
  expect_identical(spicy:::format_p_value(0.3456, ".", d), "0.35")
  expect_identical(spicy:::format_p_value(0.9, ".", d), "0.90")
  expect_identical(spicy:::format_p_value(0.0004, ".", d), "<0.001")
})

test_that("annals on a real table keeps the leading zero", {
  txt <- console(table_regression(fixed_fit(), style = "annals"))
  expect_match(txt, "<0\\.001")
  expect_false(grepl(" <.001", txt, fixed = TRUE))
})

test_that("apa pins spicy's defaults rather than changing them", {
  # "Report exact p values to two or three decimals"; "report p values
  #  less than .001 as 'p < .001.'"; no leading zero on a statistic
  #  that cannot exceed 1; two decimals for estimates;
  #  "95% CI [LL, UL]" in the official sample regression table.
  fit <- fixed_fit()
  expect_identical(
    console(table_regression(fit, style = "apa")),
    console(table_regression(fit))
  )
  st <- local_style("apa")
  expect_identical(spicy:::format_p_value(0.006, ".", st$p_digits), ".006")
  expect_identical(spicy:::format_p_value(0.0004, ".", st$p_digits), "<.001")
  expect_identical(st$digits, 2L)
  expect_identical(st$ci_brackets, c("[", "]"))
  expect_identical(st$ci_sep, ", ")
})

test_that("apa encodes no star rule, and its rule list says none", {
  # The guide's sample tables do mark .05 / .01 / .001, and spicy's own
  # default for `stars = TRUE` agrees -- but the theme pins neither,
  # because one `stars` lever carries both the thresholds and the
  # decision to show them, and the APA regression sample table shows
  # none. A `rules` entry claiming otherwise would put an unencoded
  # rule in the list that is supposed to be exact.
  s <- spicy_style("apa")
  expect_null(s$stars)
  expect_false(any(grepl("star", attr(s, "provenance")$rules)))
  txt <- console(table_regression(fixed_fit(), style = "apa"))
  expect_false(grepl("*", txt, fixed = TRUE))
  # Asking for stars gives the guide's thresholds, from the default.
  expect_identical(
    unname(spicy:::resolve_stars_thresholds(TRUE)),
    c(0.001, 0.01, 0.05)
  )
})

test_that("aer: leading zero on every decimal fraction, no stars", {
  # "Place a zero in front of the decimal point in all decimal
  #  fractions (e.g., 0.357, not .357)."
  # "Do not use asterisks to denote significance of estimation
  #  results."
  s <- spicy_style("aer")
  expect_identical(s$p_style, "standard")
  expect_identical(s$stars, FALSE)
  txt <- console(table_regression(fixed_fit(), style = "aer"))
  expect_match(txt, "<0\\.001")
  expect_false(grepl("*", txt, fixed = TRUE))
})

test_that("aer's star ban still yields to an explicit argument", {
  # The ban is written, so the theme pins it; only an argument the
  # caller types may override it -- that is the precedence rule.
  txt <- console(table_regression(fixed_fit(), style = "aer", stars = TRUE))
  expect_true(grepl("*", txt, fixed = TRUE))
})


# ---- The language's locale -----------------------------------------------

test_that("no language and no style is still the fast path", {
  # The invariant the whole suite pins: a user who sets neither pays
  # one `getOption()` more and sees the same table, byte for byte.
  expect_false(spicy:::.style_begin(NULL, quote(f()), new.env()))
  expect_null(.style_locale_defaults())
})

test_that("a language without a locale brings none", {
  withr::local_options(spicy.language = "en")
  expect_null(.style_locale_defaults())
  expect_null(spicy:::.spicy_locale_table("en"))
  expect_false(spicy:::.style_begin(NULL, quote(f()), new.env()))
})

test_that("the French locale is the comma and the leading zero", {
  # EU code 6.5: "La virgule est utilisee pour separer les unites des
  #  decimales." SI brochure 5.4.4: "le separateur decimal est
  #  toujours precede d'un zero".
  withr::local_options(spicy.language = "fr")
  expect_identical(
    .style_locale_defaults(),
    list(decimal_mark = ",", p_style = "standard")
  )
  txt <- console(table_regression(fixed_fit()))
  expect_match(txt, "37,23")
  expect_match(txt, "<0,001")
  expect_false(grepl("<,001", txt, fixed = TRUE))
  # the bound separator disambiguates itself under a comma mark, with
  # no lever of its own: `ci_bracket_separator()` reads `decimal_mark`
  expect_match(txt, "; ")
})

test_that("the French locale keeps the leading zero on bounded measures too", {
  withr::local_options(spicy.language = "fr")
  txt <- console(table_categorical(fixed_data(), "cyl", by = am))
  expect_match(txt, "0,52") # Cramer's V, leading zero kept
  expect_false(grepl(" ,52", txt, fixed = TRUE))
})

test_that("the locale reaches every family with a style layer", {
  withr::local_options(spicy.language = "fr")
  d <- fixed_data()
  expect_match(console(table_regression(fixed_fit())), "37,23")
  expect_match(console(table_categorical(d, "cyl", by = am)), "15,8")
  expect_match(console(table_continuous(d, mpg, by = am)), "17,15")
  expect_match(console(table_continuous_lm(d, mpg, by = am)), "17,15")
  expect_match(console(quietly(table_outcome(d, mpg, select = am))), "17,15")
})

test_that("the locale reaches the survey twins", {
  skip_if_not_installed("survey")
  withr::local_options(spicy.language = "fr")
  d <- fixed_data()
  d$w <- 1
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  expect_match(
    console(quietly(table_continuous_svy(des, mpg, by = am))),
    "17,15"
  )
  expect_match(
    console(quietly(table_categorical_svy(des, "cyl", by = am))),
    "[0-9],[0-9]"
  )
})

# ---- The mark alone, with no style ---------------------------------------

test_that("a typed comma keeps the leading zero in every family", {
  # The sibling of decision 43: with no `p_style` anywhere, the DEFAULT
  # of the leading zero follows the MARK -- in the reporting families
  # as it already did in the exploration pair. ",018" is not a number
  # (BIPM, SI brochure 9th ed., 5.4.4).
  d <- fixed_data()
  # A stripped zero is a comma with a non-digit before it.
  stripped <- function(txt) grepl("(^|[^0-9])[,][0-9]", txt)

  cat_txt <- console(table_categorical(
    d,
    "cyl",
    by = am,
    assoc_ci = TRUE,
    decimal_mark = ","
  ))
  expect_match(cat_txt, "0,013") # the p cell
  expect_match(cat_txt, "0,52") # the bounded association measure
  expect_false(stripped(cat_txt))

  cont <- console(table_continuous(d, c(mpg, wt), by = am, decimal_mark = ","))
  expect_match(cont, "<0,001") # the band below the floor
  expect_false(stripped(cont))

  lm_txt <- console(table_continuous_lm(
    d,
    c(mpg, wt),
    by = am,
    decimal_mark = ","
  ))
  expect_match(lm_txt, "<0,001")
  expect_false(stripped(lm_txt))

  out <- console(quietly(
    table_outcome(d, mpg, select = am, decimal_mark = ",")
  ))
  expect_match(out, "0,001")
  expect_false(stripped(out))

  reg <- console(table_regression(
    fixed_fit(),
    stars = TRUE,
    decimal_mark = ","
  ))
  expect_match(reg, "<0,001")
  expect_match(reg, "*** p < 0,001", fixed = TRUE) # the star legend
  expect_false(stripped(reg))
})

test_that("the mark reaches the survey twins too", {
  skip_if_not_installed("survey")
  d <- fixed_data()
  d$w <- 1
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  txt <- console(quietly(
    table_categorical_svy(des, "cyl", by = am, decimal_mark = ",")
  ))
  expect_match(txt, "0,0[0-9]{2}")
  expect_false(grepl("(^|[^0-9])[,][0-9]", txt))
})

test_that("the structured token follows the mark, so every engine agrees", {
  # The console formatter and the token every string-driven surface
  # reads (tinytable, flextable / Word, Excel, clipboard, inline())
  # must answer the leading-zero question the same way.
  s <- as_structured(table_regression(fixed_fit(), decimal_mark = ","))
  expect_identical(s$format_spec$p_style, "standard")
  expect_identical(s$col_meta[["p"]]$p_style, "standard")
  expect_identical(
    spicy:::.cell_to_string(0.0004, 1L, s$col_meta[["p"]], "", ","),
    "<0,001"
  )
  # A theme under the same mark keeps its own rule, everywhere.
  sj <- as_structured(table_regression(
    fixed_fit(),
    decimal_mark = ",",
    style = "jama"
  ))
  expect_identical(sj$col_meta[["p"]]$p_style, "apa")
  expect_identical(
    spicy:::.cell_to_string(0.0004, 1L, sj$col_meta[["p"]], "", ","),
    "<,001"
  )
})

test_that("an explicit p_style outranks the mark in both directions", {
  # A theme, or a composed style, is the explicit gesture; it wins under
  # any mark. Decision 43 -- unchanged by the mark-driven default.
  jama <- console(table_regression(
    fixed_fit(),
    decimal_mark = ",",
    style = "jama"
  ))
  expect_match(jama, "<,001")
  expect_false(grepl("<0,001", jama, fixed = TRUE))

  apa <- console(table_regression(
    fixed_fit(),
    decimal_mark = ",",
    style = spicy_style(p_style = "apa")
  ))
  expect_match(apa, "<,001")
  expect_false(grepl("<0,001", apa, fixed = TRUE))

  # And the other way round: "standard" keeps the zero under a point.
  std <- console(table_regression(
    fixed_fit(),
    style = spicy_style(p_style = "standard")
  ))
  expect_match(std, "<0.001")
})

test_that("a point mark is untouched by the rule", {
  # The invariant the corpus snapshot pins in full; asserted here by
  # name so a failure says what broke.
  d <- fixed_data()
  expect_match(console(table_regression(fixed_fit())), "<.001", fixed = TRUE)
  expect_match(
    console(table_categorical(d, "cyl", by = am)),
    " .52",
    fixed = TRUE
  )
  expect_match(console(table_continuous(d, mpg, by = am)), ".001", fixed = TRUE)
  expect_identical(spicy:::format_p_value(0.045, "."), ".045")
})


test_that("an unknown language fails at build exactly as it always did", {
  # `.style_locale_defaults()` now consults the language at BUILD time;
  # the abort must be the same one `spicy_str()` always raised.
  withr::local_options(spicy.language = "de")
  expect_error(table_continuous(fixed_data(), mpg), "no language set named")
})

test_that("a locale-only frame does not mask an outer style frame", {
  # No shipped entry point nests format frames today; the first bundle
  # that composes tables under an outer frame must find the historical
  # semantics: a call with no style of its own leaves the outer frame
  # in force, with the locale underneath it.
  withr::local_options(spicy.language = "fr")
  spicy:::.style_push(list(p_style = "apa", ci_sep = " to "))
  withr::defer(spicy:::.style_pop())
  txt <- console(table_regression(fixed_fit()))
  expect_match(txt, "96 to ") # outer ci_sep survives the locale push
  expect_match(txt, "<,001") # outer p_style beats the locale
  expect_match(txt, "37,23") # the locale's mark still applies
})

test_that("a theme fills only what its source states, the locale the rest", {
  # JAMA fixes no decimal mark, so a French JAMA table is JAMA's
  # p-value rules AND the French comma: the merge is lever by lever.
  withr::local_options(spicy.language = "fr")
  txt <- console(table_regression(fixed_fit(), style = "jama"))
  expect_match(txt, "37,23") # locale kept
  expect_match(txt, "<,001") # jama's floor and missing leading zero
})

test_that("a theme outranks the locale where the two meet", {
  withr::local_options(spicy.language = "fr")
  # The Lancet writes its own decimal mark: the theme wins.
  txt <- console(table_regression(fixed_fit(), style = "lancet"))
  expect_match(txt, "37\u00b723")
  expect_false(grepl("37,23", txt, fixed = TRUE))
  # APA drops the leading zero, which under a comma prints ",001" --
  # a form the SI brochure forbids. The explicit gesture still wins,
  # and ?spicy_style documents the case.
  apa <- console(table_regression(fixed_fit(), style = "apa"))
  expect_match(apa, "<,001")
  expect_match(apa, "37,23") # apa fixes no decimal mark, so the comma stays
  # apa's ", " bound separator was sourced under a dot mark; under the
  # locale's comma it would BE the mark ("[33,96, 40,50]"), so it
  # yields to the derived "; " -- as the French adaptations of APA
  # style themselves write. An unambiguous separator stays absolute.
  expect_match(apa, "96; ")
  expect_false(grepl("[0-9], -", apa))
  en_apa <- withr::with_options(
    list(spicy.language = NULL),
    console(table_regression(fixed_fit(), style = "apa"))
  )
  expect_match(en_apa, "96, ") # English apa keeps its sourced ", "
  to <- console(
    table_regression(fixed_fit(), style = spicy_style("apa", ci_sep = " to "))
  )
  expect_match(to, "96 to ")
})

test_that("the style option outranks the locale as the argument does", {
  withr::local_options(spicy.language = "fr", spicy.style = "lancet")
  expect_match(console(table_regression(fixed_fit())), "37\u00b723")
})

test_that("an argument beats the locale, for a bilingual table", {
  withr::local_options(spicy.language = "fr")
  txt <- console(table_regression(fixed_fit(), decimal_mark = "."))
  expect_match(txt, "37\\.23")
  expect_false(grepl("37,23", txt, fixed = TRUE))
  # French words, decimal point: the escape hatch is only the numbers.
  expect_match(txt, "R\u00E9gression lin\u00E9aire", fixed = TRUE)
})

test_that("the locale is figures at build time, words at print time", {
  # A formatting lever is frozen when the table is built, like every
  # argument; a label is resolved when it is printed. So a table built
  # in French and printed in English keeps its commas and loses its
  # French -- the asymmetry is documented, not accidental.
  x <- withr::with_options(
    list(spicy.language = "fr"),
    quietly(table_continuous(fixed_data(), c(mpg, wt), by = am))
  )
  txt <- console(x)
  expect_match(txt, "17,15")
  expect_match(txt, "Descriptive statistics")
  # The converse direction: built in English, printed under fr -- the
  # figures stay English (frozen at build, like every formatting
  # argument), the resolvable words turn French.
  y <- quietly(table_continuous(fixed_data(), c(mpg, wt), by = am))
  fr_txt <- withr::with_options(list(spicy.language = "fr"), console(y))
  expect_match(fr_txt, "17\\.15")
  expect_match(fr_txt, "Statistiques descriptives")
})

test_that("the structured contract reports the locale", {
  withr::local_options(spicy.language = "fr")
  s <- as_structured(quietly(table_regression(fixed_fit())))
  expect_identical(s$format_spec$decimal_mark, ",")
  expect_identical(s$format_spec$p_style, "standard")
})

test_that("style = \"fr\" is a dedicated error naming what replaced it", {
  # The name was a theme for one release cycle. It routes here from the
  # argument, from `spicy_style()`, and from the option -- and every
  # route must carry the DEDICATED message: the generic unknown-style
  # fallback shares the class, so a class check alone would stay green
  # with the dedicated branch deleted (proved by mutation in review).
  e1 <- expect_error(
    table_regression(fixed_fit(), style = "fr"),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(e1), "comes with the language")
  e2 <- expect_error(spicy_style("fr"), class = "spicy_invalid_input")
  expect_match(conditionMessage(e2), "comes with the language")
  e3 <- expect_error(
    spicy_style("fr", ci_sep = " to "),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(e3), "comes with the language")
  withr::local_options(spicy.style = "fr")
  e4 <- expect_error(
    table_continuous(fixed_data(), mpg),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(e4), "comes with the language")
})

test_that("the \"fr\" error is pinned", {
  expect_snapshot(spicy_style("fr"), error = TRUE)
})


# ---- Precedence ----------------------------------------------------------

test_that("an explicit argument beats the theme", {
  fit <- fixed_fit()
  # digits = 3 explicit beats apa's digits = 2
  expect_match(
    console(table_regression(fit, style = "apa", digits = 3)),
    "37\\.227"
  )
  # decimal_mark explicit beats lancet's midline dot
  expect_false(grepl(
    "·",
    console(table_regression(fit, style = "lancet", decimal_mark = ".")),
    fixed = TRUE
  ))
  # stars explicit beats aer's ban (see the aer block above)
})

test_that("an explicit p_digits switches the theme's p-precision rules off", {
  # Typing `p_digits` asks for that many decimals on every p-value, so
  # the theme's bands / significant figures / derived floor step aside.
  txt <- console(table_regression(fixed_fit(), style = "jama", p_digits = 4))
  expect_match(txt, "<\\.0001")
  txt2 <- console(table_regression(fixed_fit(), style = "lancet", p_digits = 2))
  expect_match(txt2, "<0·01") # the leading-zero rule is orthogonal, so it stays
})

test_that("an argument passed at its own default value still wins", {
  # `decimal_mark = "."` IS the function default, and it must still
  # beat the theme: the rule is "typed", not "different".
  expect_false(grepl(
    "·",
    console(table_regression(
      fixed_fit(),
      style = "lancet",
      decimal_mark = "."
    )),
    fixed = TRUE
  ))
})

test_that("the style argument beats the option, which beats the defaults", {
  fit <- fixed_fit()
  withr::local_options(spicy.style = "lancet")
  expect_match(console(table_regression(fit)), "37·23") # option in force
  # argument
  expect_match(
    console(table_regression(fit, style = spicy_style(decimal_mark = ","))),
    "37,23"
  )
})

test_that("options(spicy.style) reaches all four families", {
  d <- fixed_data()
  withr::local_options(spicy.style = spicy_style(decimal_mark = ","))
  expect_match(console(table_regression(fixed_fit())), "37,23")
  expect_match(console(table_categorical(d, "cyl", by = am)), "15,8")
  expect_match(console(table_continuous(d, mpg, by = am)), "17,15")
  expect_match(console(table_continuous_lm(d, mpg, by = am)), "17,15")
})

test_that("a malformed spicy.style option is refused", {
  withr::local_options(spicy.style = list(1, 2))
  expect_error(table_regression(fixed_fit()), class = "spicy_invalid_input")
})


# ---- The style survives the call that built the table --------------------

test_that("a style set at build time still governs a later print", {
  d <- fixed_data()
  x <- table_continuous(
    d,
    c(mpg, wt),
    by = am,
    style = spicy_style(decimal_mark = ",", p_style = "standard")
  )
  expect_match(console(x), "<0,001")
  y <- table_continuous_lm(d, mpg, by = am, style = "annals")
  expect_match(console(y), "<0\\.001")
})

test_that("a theme's p rule reaches the string-driven surfaces too", {
  # The console formats a p through `format_p_value()`, which asks the
  # style how many decimals THIS p gets -- The Lancet's two significant
  # figures, JAMA's third decimal below .01. The structured re-format,
  # which is the body tinytable / gt / flextable / Excel / clipboard and
  # `inline()` all render from, took the column's flat precision
  # instead: a Lancet table read 0.16 on screen and 0.1634 in Word.
  tl <- quietly(table_regression(fixed_fit(), style = "lancet"))
  local_style("lancet")
  body <- spicy:::.format_structured_to_string_body(as_structured(tl))
  # The premise: this table really does carry a p the flat precision
  # would have written differently.
  expect_true(any(nzchar(trimws(body$p))))
  expect_identical(trimws(body$p), trimws(tl$p))
})

test_that("a theme's p floor governs the typed contract, not 10^-p_digits", {
  # `col_meta$threshold` was a flat `10^(-p_digits)` in the descriptive
  # families, while the format spec beside it already used the style's
  # floor. The two part ways as soon as a theme sets them apart -- JAMA
  # asks for two decimals and a floor at .001 -- so the same table
  # floored at .01 in every string-driven output and at .001 on screen.
  tc <- quietly(table_continuous(fixed_data(), mpg, by = am, style = "jama"))
  s <- as_structured(tc)
  p_cols <- names(s$col_meta)[vapply(
    s$col_meta,
    function(m) identical(m$token, "p"),
    logical(1)
  )]
  expect_length(p_cols, 1L)
  expect_equal(s$col_meta[[p_cols]]$threshold, 1e-3)
  expect_equal(s$format_spec$p_threshold, 1e-3)
  # And with no style the two are the same number, as they always were.
  tp <- quietly(table_continuous(fixed_data(), mpg, by = am, p_digits = 4))
  sp <- as_structured(tp)
  p_cols <- names(sp$col_meta)[vapply(
    sp$col_meta,
    function(m) identical(m$token, "p"),
    logical(1)
  )]
  expect_equal(sp$col_meta[[p_cols]]$threshold, 1e-4)
})

test_that("the structured view reports the style's p contract", {
  s <- as_structured(table_regression(fixed_fit(), style = "lancet"))
  expect_identical(s$format_spec$p_style, "standard")
  expect_equal(s$format_spec$p_threshold, 1e-4)
  expect_identical(s$format_spec$decimal_mark, "·")

  s0 <- as_structured(table_regression(fixed_fit()))
  expect_identical(s0$format_spec$p_style, "apa")
  expect_equal(s0$format_spec$p_threshold, 1e-3)
})


# ---- No style asked for: nothing moves ----------------------------------

test_that("with no style every formatter takes its historical path", {
  expect_null(spicy:::.style_fmt())
  expect_identical(spicy:::format_p_value(0.0456), ".046")
  expect_identical(spicy:::format_p_value(0.0004), "<.001")
  expect_identical(spicy:::format_p_value(0.00004, digits = 4L), "<.0001")
  # The one formatter a mark moves without a style: a comma keeps the
  # leading zero, a point drops it exactly as it always has.
  expect_identical(spicy:::format_p_value(0.0456, ","), "0,046")
  expect_identical(spicy:::format_p_value(NA_real_), "")
  expect_identical(spicy:::ci_bracket_separator("."), ", ")
  expect_identical(spicy:::ci_bracket_separator(","), "; ")
  expect_identical(spicy:::.style_ci_brackets(), c("[", "]"))
  expect_identical(spicy:::.style_p_floor(3L), 1e-3)
  expect_false(spicy:::.style_p_leading_zero())
  expect_identical(spicy:::.style_p_style_token(), "apa")
  # With no `p_style` in scope the MARK answers, and both surfaces --
  # the console formatter and the token every engine reads -- answer
  # the same way.
  expect_true(spicy:::.style_p_leading_zero(","))
  expect_identical(spicy:::.style_p_style_token(","), "standard")
  expect_identical(spicy:::.style_p_decimals(0.02, 3L), 3L)
})

test_that("a style leaves no residue after the call that used it", {
  invisible(table_regression(fixed_fit(), style = "lancet"))
  expect_null(spicy:::.style_fmt())
  expect_identical(spicy:::format_p_value(0.0004), "<.001")
  # even when the call fails part-way through
  try(
    table_regression(fixed_fit(), style = "lancet", digits = -1),
    silent = TRUE
  )
  expect_null(spicy:::.style_fmt())
})

test_that("the no-style corpus is pinned across all four families", {
  # A style must be opt-in to the byte: this snapshot is the guard
  # that a lever added later cannot leak into the defaults.
  d <- fixed_data()
  fit <- fixed_fit()
  corpus <- c(
    console(table_regression(fit)),
    console(table_regression(fit, decimal_mark = ",")),
    console(table_regression(fit, stars = TRUE)),
    console(table_categorical(d, c("cyl", "am"))),
    console(table_categorical(d, "cyl", by = am, assoc_ci = TRUE)),
    console(table_continuous(d, c(mpg, wt), by = am)),
    console(table_continuous(d, c(mpg, wt), by = am, decimal_mark = ",")),
    console(table_continuous_lm(d, c(mpg, hp), by = am)),
    console(table_continuous_lm(
      d,
      mpg,
      by = am,
      effect_size = "f2",
      effect_size_ci = TRUE
    ))
  )
  expect_snapshot_value(corpus, style = "json2")
})


# ---- Composed styles reach the render ------------------------------------

test_that("a hand-composed style drives the output", {
  s <- spicy_style(
    decimal_mark = ",",
    p_style = "standard",
    ci_sep = " to ",
    ci_brackets = c("(", ")"),
    digits = 3
  )
  txt <- console(table_regression(fixed_fit(), style = s))
  expect_match(txt, "37,227")
  expect_match(txt, "\\([^)]* to [^)]*\\)")
  expect_match(txt, "<0,001")
})

test_that("ci_sep and ci_brackets reach the descriptive families", {
  d <- fixed_data()
  s <- spicy_style(ci_sep = " to ", ci_brackets = c("(", ")"))
  txt <- console(table_continuous_lm(
    d,
    mpg,
    by = am,
    effect_size = "f2",
    effect_size_ci = TRUE,
    style = s
  ))
  expect_match(txt, "\\([^)]* to [^)]*\\)")
  txt2 <- console(table_continuous(
    d,
    mpg,
    by = am,
    effect_size = "hedges_g",
    effect_size_ci = TRUE,
    style = s
  ))
  expect_match(txt2, "\\([^)]* to [^)]*\\)")
  # The categorical association CI is merged into one cell only for the
  # rendered engines, so it needs a rendered engine to be seen.
  skip_if_not_installed("gt")
  g <- table_categorical(
    d,
    "cyl",
    by = am,
    assoc_ci = TRUE,
    output = "gt",
    style = s
  )
  html <- paste(as.character(gt::as_raw_html(g)), collapse = "")
  expect_match(html, "\\([^)]* to [^)]*\\)")
})

test_that("a style-set p_floor is rendered at its own precision", {
  local_style(spicy_style(p_floor = 0.01))
  expect_identical(spicy:::format_p_value(0.005), "<.01")
})

test_that("a non-power-of-ten floor is not shown rounded", {
  local_style(spicy_style(p_floor = 0.05))
  expect_identical(spicy:::format_p_value(0.02), "<.05")
})

test_that(".signif_decimals rounds before measuring the exponent", {
  # 0.0996 at two significant figures is 0.10, one exponent up.
  expect_identical(spicy:::.signif_decimals(0.0996, 2L, cap = 6L), 2L)
  expect_identical(spicy:::.signif_decimals(0.034, 2L, cap = 6L), 3L)
  expect_identical(spicy:::.signif_decimals(0.034, 2L, cap = 2L), 2L)
  expect_identical(spicy:::.signif_decimals(0, 2L, cap = 6L), 1L)
  expect_identical(spicy:::.signif_decimals(12.3, 2L, cap = 6L), 0L)
})

test_that("a lever the function has no argument for is simply ignored", {
  # `percent_digits` belongs to table_categorical(); a regression table
  # must neither apply it nor complain about it.
  s <- spicy_style(percent_digits = 0, v_digits = 3)
  expect_silent(invisible(table_regression(fixed_fit(), style = s)))
  txt <- console(table_categorical(fixed_data(), "cyl", by = am, style = s))
  expect_match(txt, "34 ") # percentages with no decimals
})

test_that("print() shows a composed style and its star thresholds", {
  txt <- console(spicy_style(stars = c("*" = 0.05, "**" = 0.01)))
  expect_match(txt, "composed")
  expect_match(txt, "stars")
  expect_match(txt, "0.05")
  expect_match(console(spicy_style(stars = FALSE)), "FALSE")
})

test_that("a style with only argument levers needs no format context", {
  s <- spicy_style(digits = 4)
  expect_match(console(table_regression(fixed_fit(), style = s)), "37\\.2273")
  expect_null(spicy:::.style_fmt())
})
