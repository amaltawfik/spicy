# ---------------------------------------------------------------------------
# Journal styles, and the typography a language brings with it
# ---------------------------------------------------------------------------
#
# A style is DATA: a named list of format levers (`spicy_style()`), and a
# registry that maps a theme name to one such list plus its provenance
# (journal, source document, URL, consultation date, and the exact list of
# encoded rules). Adding a journal is one registry entry plus one test --
# no new mechanism.
#
# Two kinds of lever live in a style:
#
#   * levers that SHADOW a formal of a table function (`digits`,
#     `p_digits`, `decimal_mark`, `stars`, ...). The style only changes
#     the DEFAULT: an argument the user typed always wins, detected via
#     `match.call()` (which expands partial matches), so passing the
#     default value explicitly still wins.
#   * levers with NO formal (`p_style`, `p_floor`, `p_bands`,
#     `p_sigfig`, `ci_sep`, `ci_brackets`). These reach the low-level
#     formatters through a call-scoped context (pushed on entry, popped
#     by `on.exit()`), so they behave exactly like an argument passed
#     down the call stack -- never like a global setting.
#
# Provenance rule (Amal, 2026-08-14): a named theme enters spicy only
# SOURCED. Every rule below is traceable to a sentence of an official
# document of the institution, recorded in `dev/journal_styles_sources.md`
# with URL and consultation date. Rules the catalog records as NOT
# FOUND are never invented: the theme inherits the spicy default and the
# documentation says so.

# ---- Field table ----------------------------------------------------------

# The levers that shadow a formal of at least one table function. The
# style sets them as defaults; `.style_begin()` writes them into the
# calling frame only when the caller did not type them.
.STYLE_ARG_FIELDS <- c(
  "digits",
  "p_digits",
  "effect_size_digits",
  "fit_digits",
  "ic_digits",
  "percent_digits",
  "decimal_mark",
  "stars"
)

# The levers with no formal anywhere; they travel in the call-scoped
# format context and are read by the formatters -- and, for
# `note_label` and `abbreviations`, by the note builders
# (`.note_lead_keys()`, R/i18n.R; `.note_abbreviations()`,
# R/table_helpers.R).
.STYLE_FMT_FIELDS <- c(
  "p_style",
  "p_floor",
  "p_bands",
  "p_sigfig",
  "ci_sep",
  "ci_brackets",
  "note_label",
  "abbreviations"
)

.STYLE_FIELDS <- c(.STYLE_ARG_FIELDS, .STYLE_FMT_FIELDS)


# ---- Constructor ----------------------------------------------------------

#' Build or select a table style
#'
#' A *style* is a small set of number-formatting rules -- how many
#' decimals a p-value gets, where it bottoms out, whether it keeps its
#' leading zero, what the decimal mark is, how a confidence interval is
#' written -- with the word that opens the note under the table and the
#' abbreviations that note defines.
#' `spicy_style()` composes one by hand; the named themes
#' (`"jama"`, `"nejm"`, `"lancet"`, `"annals"`, `"apa"`, `"aer"` --
#' `spicy_style_names()` returns the list) are pre-composed ones, each
#' encoding rules taken verbatim from an official document of the
#' institution.
#'
#' A style is accepted by the `style` argument of the nine table
#' functions ([table_categorical()], [table_categorical_svy()],
#' [table_continuous()], [table_continuous_svy()],
#' [table_continuous_lm()], [table_outcome()], [table_multiresponse()],
#' [table_regression()] and [table_regression_uv()]), either as a theme
#' name or as the object returned here, and by
#' `options(spicy.style = )` for document-wide scope.
#'
#' # What a theme claims, and what it does not
#'
#' A theme covers **numeric formatting conformity**, the word that opens
#' the table note and the abbreviations that note defines, **only** --
#' not full editorial conformity. It does not check reporting
#' guidelines, table structure, footnote symbols, units, or anything else
#' a journal's instructions ask of a manuscript. Each theme below
#' lists the exact rules it encodes, with the sentence it encodes them
#' from; anything not listed is spicy's own default, not the journal's
#' rule.
#'
#' Themes only move **defaults**. An argument you type wins over the
#' theme, always: `table_regression(fit, style = "lancet", digits = 3)`
#' is a Lancet table with three decimals. One consequence worth
#' stating: typing `p_digits` is a request for that many decimals on
#' every p-value, so it also switches off the theme's own ways of
#' choosing p-precision -- `p_bands`, `p_sigfig`, and the `p_floor`
#' derived from them. The leading-zero rule is orthogonal and stays.
#'
#' # Resolution order
#'
#' An argument you type, then the `style` argument, then
#' `getOption("spicy.style")`, then the typography of
#' `getOption("spicy.language")`, then spicy's defaults. Within a
#' style, an explicit function argument beats the style's value for
#' the same lever.
#'
#' # The language's locale
#'
#' A language brings its typography with it, at the bottom of that
#' order. `options(spicy.language = "fr")` is therefore one gesture
#' for a coherent French report table -- French words, and numbers
#' written the French way: comma decimal mark, and the leading zero
#' French typography keeps on a p-value (`0,003`). "La virgule est
#' utilisee pour separer les unites des decimales" (European Union,
#' *Code de redaction interinstitutionnel*, French edition 2022,
#' point 6.5, `https://style-guide.europa.eu/fr`); "Si le nombre se
#' situe entre +1 et -1, le separateur decimal est toujours precede
#' d'un zero" (BIPM, *Le Systeme international d'unites*, 9th
#' edition, 2019, section 5.4.4,
#' `https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf`).
#' `"en"` brings no locale, and nothing changes for anyone who sets
#' no language. The locale reaches the exploration pair as well:
#' `freq()` and `cross_tab()` have no style layer, so the language
#' sets the DEFAULT of their `decimal_mark` and nothing else. An
#' argument you type still wins. The leading zero of a p-value works
#' the same way in both worlds: its DEFAULT follows the mark -- a
#' comma keeps it (`0,003`), a point drops it (`.003`) -- and
#' `p_style` is the explicit lever that overrides that default
#' wherever there is one, which is why a theme's rule survives any
#' mark in the `table_*()` families while the pair, having no such
#' lever, always follows its mark.
#'
#' A theme composes with a locale rather than fighting it, because a
#' theme encodes only what its own source states: `"jama"` fixes no
#' decimal mark, so JAMA under a French language gives JAMA's p-value
#' rules and the French comma. Where the two do meet, the theme wins --
#' you asked for it by name. `"lancet"` keeps its midline decimal
#' point; `"apa"` keeps its missing leading zero, which under a comma
#' prints `,003`, a form the SI brochure forbids. That is the price of
#' an explicit gesture; to keep APA's other rules and restore the
#' zero, compose the way out yourself:
#' `style = spicy_style("apa", p_style = "standard")`. One lever bends
#' the other way: a theme's `", "` interval separator was sourced
#' under a dot mark, and under a comma mark it would BE the mark -- so
#' it yields to the derived `"; "`, exactly as the French adaptations
#' of APA style themselves write (`[3,45; 6,78]`). Any other
#' separator (`" to "`, an en dash) is unambiguous and stays.
#'
#' An argument beats both, which is the escape hatch for a bilingual
#' table: `decimal_mark = "."` under a French language gives French
#' words and a decimal point. It moves only the mark: the p-value
#' keeps the locale's leading zero (`0.003`), because `p_style` is a
#' style lever with no argument of its own --
#' `style = spicy_style(p_style = "apa")` drops it again.
#'
#' See [spicy_labels()] for the language option itself.
#'
#' # Themes
#'
#' ## `"jama"` -- JAMA / JAMA Network
#'
#' Source: *Instructions for Table Creation*, JAMA Network author
#' document, 23 February 2016
#' (`https://jamanetwork.com/DocumentLibrary/InstructionsForAuthors/InstructionsForTableCreation.pdf`),
#' consulted 2026-08-14.
#'
#' Encoded:
#' \itemize{
#'   \item p-values on two decimals, three below `.01`, floored at
#'     `<.001`, with no leading zero -- "All P values should be
#'     reported to exact numbers to 2 digits past the decimal point,
#'     regardless of statistical significance. For values lower than
#'     .01, present the P value to 3 digits. Express any values lower
#'     than .001 as P<.001."
#'   \item no lead-in word before the note under the table
#'     (`note_label = ""`) -- the *AMA Manual of Style*, 11th edition
#'     (2020), section 4.1.4.10, gives table footnotes none: a footnote
#'     on the whole table is called by a letter "placed after the table
#'     title".
#'   \item the abbreviations of the note on its first line,
#'     `Abbreviations: HR, hazard ratio; OR, odds ratio.`, in alphabetical
#'     order, `Abbreviation:` for one -- section 4.1.4.10 again: that
#'     footnote comes first, "set off with an introductory word or phrase
#'     instead of a letter", and "abbreviations are expanded in
#'     alphabetical order". SD, SE and CI are not expanded (section
#'     13.11, "no need to expand"), nor are the symbols of section 19.6
#'     that do not carry "expand at first mention" (*n*, *N*, *P*, *d*,
#'     \eqn{R^2}{R^2}, \eqn{\Delta}{Delta}, \eqn{\sigma}{sigma},
#'     \eqn{\rho}{rho}), *B* and *M* among them, which that section
#'     writes *b* and \eqn{\bar{x}}{x-bar}: it lists them as usable
#'     "without expansion except where noted".
#' }
#'
#' Not encoded: everything else in that document (percentages carrying
#' their numerator and denominator, one datum per cell, footnote
#' letters, SI conversion factors) is table *construction*, not number
#' formatting. The document states no rule for the confidence-interval
#' separator or the decimal mark, so spicy's defaults apply.
#'
#' ## `"nejm"` -- The New England Journal of Medicine
#'
#' Source: NEJM Author Center, *New Manuscripts*, section *Statistical
#' Reporting Guidelines*
#' (`https://www.nejm.org/author-center/new-manuscripts`), a living
#' page; its full text was read in a browser by the package maintainer
#' on 2026-08-14 -- the site refuses automated retrieval, which is why
#' earlier surveys reported these rules as unavailable.
#'
#' Encoded:
#' \itemize{
#'   \item tiered p-values, leading zero kept -- "In general, P values
#'     larger than 0.01 should be reported to two decimal places, and
#'     those between 0.01 and 0.001 to three decimal places; P values
#'     smaller than 0.001 should be reported as P<0.001." The
#'     guideline's own text writes `0.01` / `0.001`: unlike JAMA, the
#'     leading zero stays.
#'   \item measures of association on two decimals -- "measures of
#'     association, such as odds ratios, should ordinarily be reported
#'     to two decimal places" (a pin of spicy's default).
#' }
#'
#' Not encoded: the guideline's stated exceptions to the p rule
#' (stopping-rule tests, genomewide studies) are analysis contexts the
#' style layer cannot see; its inference policy (no p-values without a
#' prespecified multiplicity plan, estimates + 95% CI instead, no
#' p-values in the Table 1 of a randomized trial) is about what to
#' report, not how to format it -- request those layouts through
#' `show_columns` and `p_value = FALSE` where you need them. No rule on
#' the abbreviations of a table note was found, so the note defines them
#' as spicy's default does.
#'
#' ## `"lancet"` -- The Lancet
#'
#' Sources: *Information for Authors*, April 2026; *Randomised trials
#' in The Lancet: formatting guidelines* and *Observational studies in
#' The Lancet: formatting guidelines*, both last updated July 2025
#' (`https://www.thelancet.com/pb-assets/Lancet/authors/tl-info-for-authors-1690986041530.pdf`),
#' consulted 2026-08-14.
#'
#' Below, the journal's midline decimal point (Unicode U+00B7, MIDDLE
#' DOT) is written `[.]` so it cannot be confused with an ordinary
#' period.
#'
#' Encoded:
#' \itemize{
#'   \item midline decimal mark, U+00B7, on every number in the table
#'     -- "Type decimal points midline (ie, 23`[.]`4, not 23.4)."
#'   \item p-values on two significant figures, capped at four
#'     decimals, floored at `p<0[.]0001`, leading zero kept --
#'     "Supply p values to two significant figures (capped at four
#'     decimal places), or p<0`[.]`0001."
#'   \item en dash between the bounds of a confidence interval. **This
#'     one is not a written rule**: the journal states none. It matches
#'     the intervals printed throughout the journal's own model tables
#'     and figures (`0[.]78 (0[.]60-1[.]00)`) and is encoded as
#'     conformity to the journal's published examples, nothing
#'     stronger.
#'   \item no lead-in word before the note under the table
#'     (`note_label = ""`). **Not a written rule either**: the model
#'     tables of the two formatting guidelines open their note on "Data
#'     are n (%)", and none of the 38 table notes of 15 research articles
#'     published in 2025-2026, checked on 2026-10-20, opens with "Note".
#'   \item the abbreviations spelled out at the end of the note, one
#'     `HR=hazard ratio.` sentence each, in alphabetical order, with *n*,
#'     *N*, *p*, SD, SE, IQR and CI left undefined. The artwork
#'     guidelines ask authors to "list and spell out all abbreviations in
#'     the table"; **the form and the exemptions are not a written
#'     rule**: they are those of the model tables of the two formatting
#'     guidelines ("Data are n (%), median (IQR), mean (SD) ...
#'     BMI=body-mass index.") and of 35 tables of 13 research articles
#'     published in 2025-2026, checked on 2026-10-20.
#' }
#'
#' Not encoded: the empty-cell filler, the ban on p-values in a
#' randomized trial's baseline table, and the absolute-rather-than-
#' relative effect rule are content decisions, not number formats.
#'
#' One caveat on the en dash. The journal's examples are ratio
#' measures, whose bounds are positive. On the identity scale a
#' negative lower bound puts a minus sign next to the dash
#' (`[-5.17--2.58]`), which reads badly. The journal states no rule
#' for that case and none is invented here; override the separator
#' when it arises:
#' `spicy_style("lancet", ci_sep = " to ")`.
#'
#' ## `"annals"` -- Annals of Internal Medicine
#'
#' Source: *Information for Authors*, American College of Physicians,
#' document publication date 08/04/2026
#' (`https://www.acpjournals.org/pb-assets/pdf/AnnalsAuthorInfo-1755188286957.pdf`),
#' consulted 2026-08-14.
#'
#' Encoded:
#' \itemize{
#'   \item p-values on three decimals up to 0.20 and two decimals above
#'     it, floored at `P<0.001`, leading zero kept -- "For P values
#'     between 0.001 and 0.20, please report the value to the nearest
#'     thousandth. For P values greater than 0.20, please report the
#'     value to the nearest hundredth. For P values less than 0.001,
#'     report as 'P<0.001.'"
#' }
#'
#' Not encoded: the percentage rule ("Report percentages to one decimal
#' place ... when sample size is \eqn{\ge}{>=} 200", no decimals below
#' 200) is conditional on a per-group sample size that the style layer
#' does not see; set `percent_digits` yourself. The `mean (SD)`
#' notation the journal asks for, and its refusal of `\eqn{\pm}{+/-}`,
#' are already how spicy renders dispersion. No rule on the
#' abbreviations of a table note was found, so the note defines them as
#' spicy's default does.
#'
#' ## `"apa"` -- APA Style, 7th edition
#'
#' Sources: *APA Style numbers and statistics guide*, last updated
#' 11 September 2024
#' (`https://apastyle.apa.org/instructional-aids/numbers-statistics-guide.pdf`);
#' *Sample tables*, last updated June 2024
#' (`https://apastyle.apa.org/style-grammar-guidelines/tables-figures/sample-tables`),
#' both consulted 2026-08-14.
#'
#' Encoded -- and this theme **pins** rather than changes: spicy's
#' defaults already follow the guide, so `style = "apa"` is a promise
#' that a future change of those defaults will not move an
#' APA-formatted table.
#' \itemize{
#'   \item p-values on three decimals, floored at `<.001`, no leading
#'     zero -- "Report exact p values to two or three decimals (e.g.,
#'     p = .006, p = .03)", "report p values less than .001 as
#'     'p < .001.'", and "Do not use a zero before a decimal when the
#'     statistic cannot be greater than 1 (proportion, correlation,
#'     level of statistical significance)."
#'   \item estimates and dispersion on two decimals -- "Report other
#'     means and standard deviations and correlations, proportions,
#'     and inferential statistics (t, F, chi-square) to two decimals."
#'   \item confidence intervals in square brackets, bounds separated by
#'     a comma -- the *Sample tables* page gives
#'     `95% CI [LL, UL]` as one of the two official layouts.
#'   \item the note under the table opens with "Note." in italics
#'     (`note_label = "Note."`) -- the *Publication Manual*, 7th edition
#'     (2020), section 7.14: the general note is "designated by the word
#'     'Note' (italicized) followed by a period".
#'   \item the abbreviations defined at the end of the general note,
#'     `CI = confidence interval; LL = lower limit`, in the order they
#'     appear in the table -- sections 7.14 and 7.15 ("define the
#'     abbreviations in the order that they appear in the table"); AIC and
#'     BIC are among those section 6.44 names. The symbols that represent
#'     statistics are left undefined ("Do not define symbols or
#'     abbreviations that represent statistics (e.g., M, SD, F, t, df, p,
#'     N, n, OR, r)", section 6.44; section 7.15 adds *SE*), and spicy
#'     counts *Mdn* (the median of Table 6.5) and Hedges's *g* among them.
#'     So are the symbols composed of Greek letters (the same two
#'     sections) and those its sample tables leave undefined (*B*,
#'     \eqn{\beta}{beta}, \eqn{R^2}{R^2}, \eqn{\Delta R^2}{Delta R2},
#'     Cohen's *d*). This is also spicy's default rule, the one `"nejm"`,
#'     `"annals"` and `"aer"` apply.
#' }
#'
#' Not encoded: the one-decimal rule for means of integer scales
#' (depends on what the variable measures, which spicy cannot know),
#' and the thousands separator (see *Known gaps* below). Nor the star
#' thresholds. The official correlation and ANOVA sample tables mark
#' `.05` / `.01` / `.001`, which is what `stars = TRUE` gives you --
#' but as spicy's own default, not as anything this theme pins: one
#' `stars` lever carries both the thresholds and the decision to show
#' them, and the APA regression sample table shows none. So the theme
#' sets no `stars`, and `style = "apa"` never switches them on.
#'
#' ## `"aer"` -- American Economic Review / AEA journals
#'
#' Source: *AER Style Guide*, American Economic Association
#' (`https://www.aeaweb.org/journals/aer/style-guide`), consulted
#' 2026-08-14. The Tables section is shared with AEJ: Applied, AEJ:
#' Macro, AEJ: Policy, JEL and AEA P&P.
#'
#' Encoded:
#' \itemize{
#'   \item leading zero on every decimal fraction, p-values included --
#'     "Place a zero in front of the decimal point in all decimal
#'     fractions (e.g., 0.357, not .357)."
#'   \item no significance stars, pinned -- "Do not use asterisks to
#'     denote significance of estimation results. Report the standard
#'     errors in parentheses." This is the only outright written ban on
#'     stars in the whole surveyed corpus, so it is encoded even though
#'     spicy already defaults to `stars = FALSE`. Ask for
#'     `show_columns = c("b", "se")` for the standard-error layout.
#' }
#'
#' Not encoded: the guide fixes no number of decimals, no p-value
#' floor, no interval format and no decimal mark, so spicy's defaults
#' apply. Horizontal-rules-only, no shading, a nine-column maximum and
#' "Panel A / Panel B" blocks are layout, not number formatting. No rule
#' on the abbreviations of a table note was found, so the note defines
#' them as spicy's default does.
#'
#' # Known gaps
#'
#' Two rules the sources state but this release does not encode:
#' \itemize{
#'   \item **thousands separator** (APA's comma, the EU code's thin
#'     no-break space). spicy formats numbers through more than one
#'     path, and a separator applied to some of them only would print
#'     `1,234` in one column and `1234` in the next -- the exact
#'     inconsistency the SI brochure forbids ("the format used should
#'     not vary within one column").
#'   \item **significant-figure rounding of estimates** (Epidemiology's
#'     `nn` / `n.n` / `0.nn`, Science's "report only significant
#'     digits"). spicy's `digits` is a decimals contract applied per
#'     column family; a blanket significant-figure mode would also
#'     round fit statistics, which those rules do not ask for.
#' }
#'
#' Journals deliberately absent: **BMJ** (its house-style page could
#' not be read from any official source, and the widely repeated
#' "95% CI 1.2 to 3.4" rule traces to no BMJ document),
#' **Econometrica** (verified negative: the official guidelines contain
#' no numeric table rule), **QJE** (its instructions page could not be
#' read), **Epidemiology** (see *Known gaps*). Naming any of them would
#' mean inventing their rules.
#'
#' @param base Optional theme to start from -- a theme name or another
#'   `spicy_style` -- whose levers the arguments below override. This
#'   is the composition escape hatch: `spicy_style("lancet",
#'   ci_sep = " to ")` is The Lancet's rules with one of them changed.
#'   The theme's provenance travels with the result and names the
#'   levers you overrode, so a modified theme never passes for the
#'   theme itself.
#' @param ... Must be empty. Any argument landing here is a misspelled
#'   lever and raises an error rather than being ignored.
#' @param p_style How p-values carry their leading zero: `"apa"` drops
#'   it (`.003`), `"standard"` keeps it (`0.003`). `NULL` leaves
#'   spicy's default, which follows the mark: dropped under a point,
#'   kept under a comma.
#' @param p_digits Decimal places for p-values (a positive integer).
#'   Under `p_sigfig` it acts as the decimal cap instead.
#' @param p_floor The value below which a p-value prints as `<floor`
#'   rather than exactly, e.g. `0.001` for `<.001`. A number strictly
#'   between 0 and 1. `NULL` uses `10^-p_digits`.
#' @param p_bands Decimal places that vary with the size of the
#'   p-value: a list of two-element numeric vectors
#'   `c(cutoff, digits)`, cutoffs strictly increasing, the last one
#'   `Inf`. `list(c(0.01, 3), c(Inf, 2))` reads as "three decimals
#'   below .01, two otherwise". The comparison is `p < cutoff`.
#'   Mutually exclusive with `p_sigfig`.
#' @param p_sigfig Significant figures for p-values (a positive
#'   integer), capped at `p_digits` decimal places. Mutually exclusive
#'   with `p_bands`.
#' @param decimal_mark A single character; the mark between the integer
#'   and the decimal part of every number in the table.
#' @param ci_sep The string between the two bounds of a confidence
#'   interval. `NULL` keeps spicy's default, which is `", "` and turns
#'   into `"; "` when the decimal mark is a comma.
#' @param ci_brackets A character vector of length two, the opening and
#'   closing delimiters of a confidence interval, e.g. `c("[", "]")` or
#'   `c("(", ")")`.
#' @param stars Significance stars: `FALSE` to suppress them, or a
#'   named numeric vector of symbol-to-threshold pairs such as
#'   `c("***" = .001, "**" = .01, "*" = .05)`.
#' @param digits,effect_size_digits,fit_digits,ic_digits Decimal places
#'   for estimates, effect sizes, fit statistics and information
#'   criteria. Each is applied only by the table functions that have
#'   the matching argument: `effect_size_digits` reaches the association
#'   measure and the SMD of [table_categorical()] too.
#' @param percent_digits Decimal places for percentages.
#' @param note_label The word that opens the note under the table, set
#'   in italics in every rendered output: `"Note."` (the default, APA
#'   Manual 7 section 7.14), `"Note:"` (Chicago Manual of Style 18,
#'   paragraphs 3.77-3.81), or `""` for no lead-in word (AMA Manual of
#'   Style 11, section 4.1.4.10). The language spells it: under
#'   `options(spicy.language = "fr")`, `"Note:"` is written "Note :",
#'   with the no-break space French puts before a colon, in every output.
#'   A `note` you type is printed as typed; its opening word is set in
#'   italics when it is the lead-in in force.
#' @param abbreviations Which abbreviations the note under the table
#'   defines: those of its headers, its spanners and the labels of its
#'   stub (the fit statistics and the random effects of a regression),
#'   not the symbols inside its cells. A definition identifies the
#'   quantity by its standard name, in one expression, the author included
#'   when the name carries one (*g* = Hedges's *g*); it never explains what
#'   the quantity measures or how to read it, which belongs to the text.
#'   `NULL` (the default) follows the rule of the theme in force, the APA
#'   rule when there is none (see *Themes*). `"all"` defines every
#'   abbreviation and symbol the table shows, abbreviated words such as Min
#'   and Max included, except *n*, *N*, *p* and the test statistics (*t*,
#'   *z*, *F*, \eqn{\chi^2}{chi-squared}), which no style and no journal
#'   defines: the setting for readers outside the field. `"none"` defines
#'   none, but keeps the disambiguations, the definitions without which the
#'   symbol is ambiguous (partial eta squared, the IQR as the width Q3 -
#'   Q1, the method of a standardized \eqn{\beta}{beta}, the design-based
#'   standard error). Either way the definitions take the form and the
#'   place of the theme in force. A `note` you type is printed as typed.
#' @param v_digits Defunct. Renamed `effect_size_digits` in spicy 0.14.0;
#'   supplying `v_digits` is an error (class `spicy_defunct`).
#'
#' @return `spicy_style()` returns an object of class `spicy_style`: a
#'   named list holding only the levers that were set. Themes returned
#'   by name carry a `provenance` attribute with the journal, its
#'   source document and the exact list of encoded rules; `print()`
#'   shows it. `spicy_style_names()` returns the character vector of
#'   available theme names.
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#'
#' # A named theme.
#' table_regression(fit, style = "jama")
#'
#' # An argument you type beats the theme.
#' table_regression(fit, style = "jama", p_digits = 4)
#'
#' # A style composed by hand.
#' table_regression(fit, style = spicy_style(decimal_mark = ",",
#'                                           p_style = "standard"))
#'
#' # A theme with one rule changed.
#' table_regression(fit, style = spicy_style("lancet", ci_sep = " to "))
#'
#' # Document-wide scope.
#' old <- options(spicy.style = "apa")
#' table_continuous(mtcars, c(mpg, wt))
#' options(old)
#'
#' # Every abbreviation defined, for readers outside the field.
#' table_continuous(mtcars, c(mpg, wt),
#'                  style = spicy_style(abbreviations = "all"))
#'
#' # What a theme encodes, and where it comes from.
#' spicy_style("lancet")
#' @export
spicy_style <- function(
  base = NULL,
  ...,
  p_style = NULL,
  p_digits = NULL,
  p_floor = NULL,
  p_bands = NULL,
  p_sigfig = NULL,
  decimal_mark = NULL,
  ci_sep = NULL,
  ci_brackets = NULL,
  stars = NULL,
  digits = NULL,
  effect_size_digits = NULL,
  fit_digits = NULL,
  ic_digits = NULL,
  percent_digits = NULL,
  note_label = NULL,
  abbreviations = NULL,
  v_digits
) {
  # Renamed in 0.14.0; refused with the new name before anything else.
  if (!missing(v_digits)) {
    .abort_renamed_arg(
      "spicy_style",
      "v_digits",
      "effect_size_digits",
      deparse1(substitute(v_digits))
    )
  }
  if (...length() > 0L) {
    # A lever spicy does not know is a typo, and a silently ignored
    # lever is exactly what a style system must not do.
    spicy_abort(
      c(
        "`spicy_style()` got unnamed or unknown arguments.",
        "i" = paste0("Levers: ", paste(.STYLE_FIELDS, collapse = ", "), "."),
        "i" = paste(
          "The first argument is a theme to start from:",
          "`spicy_style(\"lancet\", ci_sep = \" to \")`."
        )
      ),
      class = "spicy_invalid_input"
    )
  }

  p_style <- .style_check_enum(p_style, "p_style", c("apa", "standard"))
  p_digits <- .style_check_count(p_digits, "p_digits", min = 1L)
  p_sigfig <- .style_check_count(p_sigfig, "p_sigfig", min = 1L)
  p_floor <- .style_check_prob(p_floor, "p_floor")
  p_bands <- .style_check_bands(p_bands)
  decimal_mark <- .style_check_char1(decimal_mark, "decimal_mark")
  ci_sep <- .style_check_string(ci_sep, "ci_sep")
  ci_brackets <- .style_check_brackets(ci_brackets)
  stars <- .style_check_stars(stars)
  digits <- .style_check_count(digits, "digits", min = 0L)
  effect_size_digits <- .style_check_count(
    effect_size_digits,
    "effect_size_digits",
    min = 0L
  )
  fit_digits <- .style_check_count(fit_digits, "fit_digits", min = 0L)
  ic_digits <- .style_check_count(ic_digits, "ic_digits", min = 0L)
  percent_digits <- .style_check_count(
    percent_digits,
    "percent_digits",
    min = 0L
  )
  note_label <- .style_check_enum(
    note_label,
    "note_label",
    c("Note.", "Note:", "")
  )
  abbreviations <- .style_check_enum(
    abbreviations,
    "abbreviations",
    c("all", "none")
  )

  if (!is.null(p_bands) && !is.null(p_sigfig)) {
    spicy_abort(
      c(
        "`p_bands` and `p_sigfig` cannot both be set.",
        "i" = paste(
          "They are two ways of choosing how many decimals a p-value",
          "gets; pick one."
        )
      ),
      class = "spicy_invalid_input"
    )
  }

  out <- list(
    p_style = p_style,
    p_digits = p_digits,
    p_floor = p_floor,
    p_bands = p_bands,
    p_sigfig = p_sigfig,
    decimal_mark = decimal_mark,
    ci_sep = ci_sep,
    ci_brackets = ci_brackets,
    stars = stars,
    digits = digits,
    effect_size_digits = effect_size_digits,
    fit_digits = fit_digits,
    ic_digits = ic_digits,
    percent_digits = percent_digits,
    note_label = note_label,
    abbreviations = abbreviations
  )
  out <- out[!vapply(out, is.null, logical(1))]

  if (is.null(base)) {
    return(structure(out, class = "spicy_style"))
  }

  # A theme to start from, plus the levers given here. The theme's
  # provenance travels on, with the overridden levers named: a modified
  # theme must never pass for the theme itself.
  base_style <- .style_resolve_object(base)
  merged <- unclass(base_style)
  # The two ways of choosing p-value precision are exclusive, so the
  # caller's choice removes the theme's.
  if ("p_bands" %in% names(out)) {
    merged$p_sigfig <- NULL
  }
  if ("p_sigfig" %in% names(out)) {
    merged$p_bands <- NULL
  }
  merged[names(out)] <- out
  prov <- attr(base_style, "provenance")
  if (!is.null(prov) && length(out) > 0L) {
    prov$overrides <- names(out)
  }
  structure(merged, class = "spicy_style", provenance = prov)
}


# ---- Field validators -----------------------------------------------------

# Each takes `call`, the frame of `spicy_style()`, so that a lever's error
# names the function the user called rather than the helper.

.style_check_enum <- function(x, field, allowed, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || !(x %in% allowed)) {
    spicy_abort(
      sprintf(
        "`%s` must be one of %s.",
        field,
        paste0("\"", allowed, "\"", collapse = ", ")
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  x
}

# A name is refused rather than dropped, as in `validate_digit_arg()`: a
# style holds one number of decimals for every table.
.style_check_count <- function(x, field, min = 0L, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  ok <- is.numeric(x) &&
    length(x) == 1L &&
    is.null(names(x)) &&
    !is.na(x) &&
    is.finite(x) &&
    x == as.integer(x) &&
    x >= min
  if (!ok) {
    spicy_abort(
      .digit_arg_message(field, min, x),
      class = "spicy_invalid_input",
      call = call
    )
  }
  as.integer(x)
}

.style_check_prob <- function(x, field, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  ok <- is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x > 0 &&
    x < 1
  if (!ok) {
    spicy_abort(
      sprintf("`%s` must be a single number strictly between 0 and 1.", field),
      class = "spicy_invalid_input",
      call = call
    )
  }
  as.numeric(x)
}

# A decimal mark is one character. "." and "," are the usual pair, but
# The Lancet's midline dot (U+00B7) is a third, so the table families
# accept any single character -- as `validate_decimal_mark()` already
# does for the regression family.
.is_single_char <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nchar(x) == 1L
}

.style_check_char1 <- function(x, field, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || nchar(x) != 1L) {
    spicy_abort(
      sprintf("`%s` must be a single character.", field),
      class = "spicy_invalid_input",
      call = call
    )
  }
  x
}

.style_check_string <- function(x, field, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    spicy_abort(
      sprintf("`%s` must be a single string.", field),
      class = "spicy_invalid_input",
      call = call
    )
  }
  x
}

.style_check_brackets <- function(x, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 2L || anyNA(x)) {
    spicy_abort(
      c(
        "`ci_brackets` must be a character vector of length 2.",
        "i" = "For example `c(\"[\", \"]\")` or `c(\"(\", \")\")`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  unname(x)
}

.style_check_stars <- function(x, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (isFALSE(x)) {
    return(FALSE)
  }
  ok <- is.numeric(x) &&
    length(x) > 0L &&
    !is.null(names(x)) &&
    all(nzchar(names(x))) &&
    !anyNA(x) &&
    all(x > 0 & x <= 1)
  if (!ok) {
    spicy_abort(
      c(
        "`stars` must be `FALSE` or a named numeric vector of thresholds.",
        "i" = "For example `c(\"***\" = .001, \"**\" = .01, \"*\" = .05)`.",
        "i" = "Thresholds must be named, and lie in (0, 1]."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  x
}

.style_check_bands <- function(x, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  bad <- function(msg) {
    spicy_abort(
      c(
        msg,
        "i" = paste(
          "`p_bands` is a list of `c(cutoff, digits)` pairs, cutoffs",
          "strictly increasing, the last one `Inf`."
        ),
        "i" = "For example `list(c(0.01, 3), c(Inf, 2))`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (!is.list(x) || length(x) == 0L) {
    bad("`p_bands` must be a non-empty list.")
  }
  cuts <- numeric(length(x))
  for (i in seq_along(x)) {
    b <- x[[i]]
    if (!is.numeric(b) || length(b) != 2L || anyNA(b)) {
      bad(sprintf("`p_bands[[%d]]` is not a numeric pair.", i))
    }
    if (b[[1L]] <= 0) {
      bad(sprintf("`p_bands[[%d]]` has a cutoff <= 0.", i))
    }
    if (!is.finite(b[[2L]]) || b[[2L]] != as.integer(b[[2L]]) || b[[2L]] < 0) {
      bad(sprintf("`p_bands[[%d]]` has a non-integer digit count.", i))
    }
    cuts[[i]] <- b[[1L]]
  }
  if (!identical(cuts[[length(cuts)]], Inf)) {
    bad("The last `p_bands` cutoff must be `Inf`.")
  }
  if (length(cuts) > 1L && any(diff(cuts) <= 0)) {
    bad("`p_bands` cutoffs must be strictly increasing.")
  }
  lapply(x, function(b) c(cutoff = b[[1L]], digits = as.integer(b[[2L]])))
}


# ---- Registry -------------------------------------------------------------

# Themes as DATA. One entry = one journal: the pre-composed style, plus
# the provenance a named theme must carry (journal, source document, URL,
# consultation date, and the exact rules encoded). Adding a journal is an
# entry here and a test in tests/testthat/test-spicy_style.R.
#
# Every rule below is traceable to dev/journal_styles_sources.md, which
# holds the verbatim sentence it comes from. Rules the catalog records
# as NOT FOUND are not here: those themes inherit the spicy default.
.spicy_style_registry <- function() {
  list(
    jama = list(
      journal = "JAMA / JAMA Network",
      document = "Instructions for Table Creation (JAMA Network author document)",
      url = paste0(
        "https://jamanetwork.com/DocumentLibrary/InstructionsForAuthors/",
        "InstructionsForTableCreation.pdf"
      ),
      date = "2016-02-23, consulted 2026-08-14",
      rules = c(
        "p-values: 2 decimals, 3 below .01, floor <.001, no leading zero",
        paste0(
          "table note: no lead-in word (AMA Manual of Style, 11th edition, ",
          "section 4.1.4.10)"
        ),
        paste0(
          "table note: an \"Abbreviations:\" line first, alphabetical; SD, ",
          "SE, CI and the symbols of section 19.6 not expanded (AMA Manual ",
          "of Style, 11th edition, sections 4.1.4.10, 13.11 and 19.6)"
        ),
        "everything else in the source is table construction, not number format"
      ),
      # The abbreviations of the note (R/table_helpers.R,
      # `.note_abbreviations()`): the AMA Manual of Style, 11th edition,
      # section 4.1.4.10, p. 125-127 ("the abbreviations ... footnotes
      # appear first and are set off with an introductory word or phrase
      # instead of a letter. In such footnotes, abbreviations are expanded
      # in alphabetical order"; model line "Abbreviations: OR, odds ratio;
      # RR, relative risk."), section 13.11 (SD, SE, SEM and CI "no need to
      # expand") and section 19.6 (dev/journal_styles_sources.md, section
      # 5 ter).
      abbrev_rule = "jama",
      # "All P values should be reported to exact numbers to 2 digits
      # past the decimal point ... For values lower than .01, present
      # the P value to 3 digits. Express any values lower than .001 as
      # P<.001." The rule writes ".01" / ".001": no leading zero.
      # note_label: the AMA Manual of Style, 11th edition (2020), section
      # 4.1.4.10, p. 124-127, gives the table footnote no lead-in word; a
      # note on the whole table takes a letter placed after the title
      # (dev/journal_styles_sources.md, section 5 bis).
      style = spicy_style(
        p_style = "apa",
        p_digits = 2L,
        p_bands = list(c(0.01, 3), c(Inf, 2)),
        p_floor = 0.001,
        note_label = ""
      )
    ),
    nejm = list(
      journal = "The New England Journal of Medicine",
      document = paste0(
        "NEJM Author Center, New Manuscripts, section ",
        "Statistical Reporting Guidelines"
      ),
      url = paste0(
        "https://www.nejm.org/author-center/new-manuscripts",
        "#statistical-reporting-guidelines"
      ),
      date = paste0(
        "living page; full text read in a browser by the maintainer ",
        "on 2026-08-14 (the site refuses automated retrieval)"
      ),
      rules = c(
        paste0(
          "p-values: 2 decimals above 0.01, 3 decimals between 0.01 ",
          "and 0.001, floor P<0.001, leading zero kept"
        ),
        "measures of association (OR, HR, ...): 2 decimals ordinarily"
      ),
      # A.1.g: "In general, P values larger than 0.01 should be
      # reported to two decimal places, and those between 0.01 and
      # 0.001 to three decimal places; P values smaller than 0.001
      # should be reported as P<0.001." The rule writes "0.01" /
      # "0.001": leading zero kept. (Stated exceptions -- stopping
      # rules, genomewide studies -- are analysis contexts the style
      # layer cannot see; documented in ?spicy_style.)
      # A.1.h: "measures of association, such as odds ratios, should
      # ordinarily be reported to two decimal places" -- a pinning
      # lever, spicy's default already.
      style = spicy_style(
        p_style = "standard",
        p_digits = 2L,
        p_bands = list(c(0.01, 3), c(Inf, 2)),
        p_floor = 0.001,
        digits = 2L
      )
    ),
    lancet = list(
      journal = "The Lancet",
      document = paste0(
        "Information for Authors (April 2026); Randomised trials / ",
        "Observational studies formatting guidelines (July 2025)"
      ),
      url = paste0(
        "https://www.thelancet.com/pb-assets/Lancet/authors/",
        "tl-info-for-authors-1690986041530.pdf"
      ),
      date = "2026-04, consulted 2026-08-14",
      rules = c(
        "decimal mark: midline dot U+00B7 on every number",
        paste0(
          "p-values: 2 significant figures capped at 4 decimals, ",
          "floor <0\u00b70001, leading zero kept"
        ),
        paste0(
          "confidence intervals: en dash between bounds (matches the ",
          "journal's published examples; not a written rule)"
        ),
        paste0(
          "table note: no lead-in word (matches the journal's model tables ",
          "and its published tables; not a written rule)"
        ),
        paste0(
          "table note: abbreviations spelled out at its end, one ",
          "\"HR=hazard ratio.\" sentence each, alphabetical; n, N, p, SD, SE, ",
          "IQR and CI not (matches the journal's model tables and its ",
          "published tables; not a written rule)"
        )
      ),
      # The abbreviations of the note: "please list and spell out all
      # abbreviations in the table" (Artwork formatting guidelines, p. 1) is
      # the only written rule. The model tables (formatting guidelines of
      # 2025, p. 3: "Data are n (%), median (IQR), mean (SD), or n/N (%).
      # ECOG=Eastern Cooperative Oncology Group. BMI=body-mass index.") and
      # 35 tables of 13 research articles of 2025-2026, checked on
      # 2026-10-20, give the form and leave n, N, p, SD, SE, IQR and CI
      # undefined (dev/journal_styles_sources.md, section 5 ter).
      abbrev_rule = "lancet",
      # "Type decimal points midline (ie, 23[U+00B7]4, not 23.4)."
      # "Supply p values to two significant figures (capped at four
      #  decimal places), or p<0[U+00B7]0001."
      # The en dash is OBSERVED in the journal's model tables
      # ("0[U+00B7]78 (0[U+00B7]60-1[U+00B7]00)"), never stated as a rule.
      # note_label: OBSERVED as well, never stated. The model tables of the
      # two formatting guidelines (2025, p. 3) open their note on "Data are
      # n (%) ...", and none of the 38 table notes of 15 research articles
      # of 2025-2026 checked on 2026-10-20 opens with "Note" or "Notes"
      # (dev/journal_styles_sources.md, section 5 bis).
      style = spicy_style(
        decimal_mark = "\u00b7",
        p_style = "standard",
        p_sigfig = 2L,
        p_digits = 4L,
        p_floor = 0.0001,
        ci_sep = "\u2013",
        note_label = ""
      )
    ),
    annals = list(
      journal = "Annals of Internal Medicine",
      document = "Information for Authors (American College of Physicians)",
      url = paste0(
        "https://www.acpjournals.org/pb-assets/pdf/",
        "AnnalsAuthorInfo-1755188286957.pdf"
      ),
      date = "2026-08-04, consulted 2026-08-14",
      rules = c(
        "p-values: 3 decimals up to 0.20, 2 above it, floor <0.001, leading zero kept"
      ),
      # "For P values between 0.001 and 0.20, please report the value to
      #  the nearest thousandth. For P values greater than 0.20, please
      #  report the value to the nearest hundredth. For P values less
      #  than 0.001, report as 'P<0.001.'"
      # The percentage rule (1 decimal when n >= 200, none below) is
      # conditional on a per-group n the style layer does not see: NOT
      # encoded.
      style = spicy_style(
        p_style = "standard",
        p_digits = 3L,
        p_bands = list(c(0.20, 3), c(Inf, 2)),
        p_floor = 0.001
      )
    ),
    apa = list(
      journal = "American Psychological Association, 7th edition",
      document = paste0(
        "APA Style numbers and statistics guide (2024-09-11); ",
        "Sample tables (2024-06)"
      ),
      url = paste0(
        "https://apastyle.apa.org/instructional-aids/",
        "numbers-statistics-guide.pdf"
      ),
      date = "2024-09-11, consulted 2026-08-14",
      rules = c(
        "p-values: 3 decimals, floor <.001, no leading zero",
        "estimates and dispersion: 2 decimals",
        "confidence intervals: square brackets, bounds separated by a comma",
        paste0(
          "table note: opens with \"Note.\" in italics (Publication Manual, ",
          "7th edition, section 7.14)"
        ),
        paste0(
          "table note: abbreviations defined at the end, \"CI = confidence ",
          "interval\", in their order of appearance; the symbols that ",
          "represent statistics, Greek letters and the symbols its sample ",
          "tables leave undefined are not (Publication Manual, 7th edition, ",
          "sections 6.44, 7.14 and 7.15)"
        ),
        "a pinning theme: spicy's defaults already follow the guide"
      ),
      # "Report exact p values to two or three decimals"; "report p
      #  values less than .001 as 'p < .001.'"; "Do not use a zero
      #  before a decimal when the statistic cannot be greater than 1".
      # "Report other means and standard deviations and correlations,
      #  proportions, and inferential statistics ... to two decimals."
      # Sample tables: "95% CI [LL, UL]"; probability notes
      #  "*p < .05. **p < .01." and "***p < .001."
      # The star thresholds are deliberately NOT a rule of this theme,
      # so they are not in `rules` either: one `stars` lever carries
      # both the thresholds and the decision to show them, and the APA
      # regression sample table shows none -- setting it here would
      # switch stars on for every APA table. The three thresholds are
      # spicy's own default for `stars = TRUE`, and agree with the
      # guide; ?spicy_style says so under "Not encoded".
      # note_label: the Publication Manual, 7th edition (2020), section
      # 7.14, p. 203-204: the general note is "designated by the word
      # 'Note' (italicized) followed by a period"
      # (dev/journal_styles_sources.md, section 5 bis). spicy's default
      # already, pinned like the rest.
      style = spicy_style(
        p_style = "apa",
        p_digits = 3L,
        p_floor = 0.001,
        digits = 2L,
        effect_size_digits = 2L,
        ci_brackets = c("[", "]"),
        ci_sep = ", ",
        note_label = "Note."
      )
    ),
    aer = list(
      journal = "American Economic Review / AEA journals",
      document = "AER Style Guide (American Economic Association)",
      url = "https://www.aeaweb.org/journals/aer/style-guide",
      date = "undated page, consulted 2026-08-14",
      rules = c(
        "leading zero on every decimal fraction, p-values included",
        "no significance stars (the corpus's only written ban)"
      ),
      # "Place a zero in front of the decimal point in all decimal
      #  fractions (e.g., 0.357, not .357)."
      # "Do not use asterisks to denote significance of estimation
      #  results. Report the standard errors in parentheses."
      style = spicy_style(
        p_style = "standard",
        stars = FALSE
      )
    )
  )
}

#' @rdname spicy_style
#' @export
spicy_style_names <- function() {
  names(.spicy_style_registry())
}

# `name` is already known to be a single non-NA string: every caller
# routes through `.style_resolve_object()`.
.style_from_registry <- function(name, call = rlang::caller_env()) {
  # "fr" was a theme until it stopped being one: the registry's contract
  # is that a named theme IS a journal, and French typography is a
  # locale. It travels with the language now, so this name gets its own
  # message rather than the "unknown style" list it would otherwise
  # fall into. Every route -- the `style` argument,
  # `options(spicy.style = "fr")`, `spicy_style("fr")` and
  # `spicy_style("fr", ...)` as a base -- passes through here.
  if (identical(name, "fr")) {
    spicy_abort(
      c(
        "The \"fr\" style is gone: French typography now comes with the language.",
        "i" = "`options(spicy.language = \"fr\")` gives a French table -- words and numbers.",
        "i" = "For the numbers alone, compose `spicy_style(decimal_mark = \",\", p_style = \"standard\")`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  reg <- .spicy_style_registry()
  entry <- reg[[name]]
  if (is.null(entry)) {
    spicy_abort(
      c(
        sprintf("Unknown table style \"%s\".", name),
        "i" = paste0(
          "Available styles: ",
          paste0("\"", names(reg), "\"", collapse = ", "),
          "."
        ),
        "i" = paste(
          "See `?spicy_style` for what each one encodes and where it",
          "comes from, or compose your own with `spicy_style()`."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  out <- entry$style
  prov <- list(
    name = name,
    journal = entry$journal,
    document = entry$document,
    url = entry$url,
    date = entry$date,
    rules = entry$rules
  )
  # The abbreviation rule of the theme, for the themes that have one of
  # their own; the others apply spicy's default, the APA rule.
  prov$abbrev_rule <- entry$abbrev_rule
  attr(out, "provenance") <- prov
  out
}

#' Print method for table styles
#'
#' @description
#' Prints what a [spicy_style()] encodes: for a named theme, the
#' journal, the rules it applies, and the official document they come
#' from; then the levers themselves.
#'
#' @param x A `spicy_style` object.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [spicy_style()]
#' @keywords internal
#' @export
print.spicy_style <- function(x, ...) {
  prov <- attr(x, "provenance")
  if (is.null(prov)) {
    cat("<spicy_style> (composed)\n")
  } else {
    cat(sprintf("<spicy_style> \"%s\" -- %s\n", prov$name, prov$journal))
    if (!is.null(prov$overrides)) {
      cat(sprintf(
        "  MODIFIED: you overrode %s -- this is no longer the theme itself.\n",
        paste(prov$overrides, collapse = ", ")
      ))
    }
    cat(
      "\nEncoded rules (formatting only, not editorial conformity):\n"
    )
    for (r in prov$rules) {
      cat(paste0(" - ", r, "\n"))
    }
    cat(paste0("\nSource: ", prov$document, "\n"))
    cat(paste0("        ", prov$url, "\n"))
    cat(paste0("        ", prov$date, "\n"))
  }
  vals <- unclass(x)
  attr(vals, "provenance") <- NULL
  if (length(vals) > 0L) {
    cat("\nLevers:\n")
    for (nm in names(vals)) {
      cat(sprintf("  %-19s %s\n", nm, .style_fmt_lever(vals[[nm]])))
    }
  }
  invisible(x)
}

# `decimal.mark` pinned at the point on every branch: these are the
# lever values the reader would RETYPE to reproduce the style, so they
# must be printed as R parses them. Under `options(OutDec = ",")` the
# bare `format()` printed `p_floor 0,0001` and `p_bands p < 0,01 -> 3
# dp` -- numbers no one can paste back into `spicy_style()`. The style
# object carries no `decimal_mark` of its own, so there is nothing else
# for these to follow.
.style_fmt_lever <- function(v) {
  if (is.list(v)) {
    return(paste(
      vapply(
        v,
        function(b) {
          sprintf(
            "p < %s -> %d dp",
            format(b[[1L]], decimal.mark = "."),
            b[[2L]]
          )
        },
        character(1)
      ),
      collapse = "; "
    ))
  }
  if (isFALSE(v)) {
    return("FALSE")
  }
  # `note_label = ""`: an empty value would print as nothing at all.
  if (identical(v, "")) {
    return("\"\"")
  }
  if (!is.null(names(v))) {
    return(paste(
      sprintf("%s = %s", names(v), format(v, decimal.mark = ".")),
      collapse = ", "
    ))
  }
  if (is.numeric(v)) {
    return(paste(
      format(v, scientific = FALSE, trim = TRUE, decimal.mark = "."),
      collapse = " "
    ))
  }
  paste(format(v, decimal.mark = "."), collapse = " ")
}


# ---- Resolution -----------------------------------------------------------

# A theme name or a `spicy_style` -> a `spicy_style`. Anything else is
# an error naming the available themes.
.style_resolve_object <- function(style, call = rlang::caller_env()) {
  if (inherits(style, "spicy_style")) {
    return(style)
  }
  if (is.character(style) && length(style) == 1L && !is.na(style)) {
    return(.style_from_registry(style, call = call))
  }
  spicy_abort(
    c(
      "`style` must be a style name, a `spicy_style()`, or `NULL`.",
      "i" = paste0(
        "Available styles: ",
        paste0("\"", spicy_style_names(), "\"", collapse = ", "),
        "."
      )
    ),
    class = "spicy_invalid_input",
    call = call
  )
}

# `style` argument, then `options(spicy.style)`, then nothing.
# Returns a `spicy_style` or NULL.
.style_resolve <- function(style, call = rlang::caller_env()) {
  if (is.null(style)) {
    style <- getOption("spicy.style", NULL)
    if (is.null(style)) {
      return(NULL)
    }
    if (!inherits(style, "spicy_style") && !is.character(style)) {
      spicy_abort(
        c(
          "`options(spicy.style)` must be a style name or a `spicy_style()`.",
          "i" = paste0(
            "Available styles: ",
            paste0("\"", spicy_style_names(), "\"", collapse = ", "),
            "."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }
  .style_resolve_object(style, call = call)
}

# The typography a language brings with it -- the LOWEST-priority lever
# layer: an explicit argument and any style outrank it. Returns a plain
# list of levers, or NULL when no language is set, when it is "en", or
# when it ships no locale.
#
# Deliberately NOT a `spicy_style`: a locale is not a style, carries no
# provenance, and must never pass for a theme in `print()` or in the
# structured contract.
.style_locale_defaults <- function() {
  lang_opt <- getOption("spicy.language", NULL)
  if (is.null(lang_opt)) {
    return(NULL)
  }
  .spicy_locale_table(.spicy_language_option(lang_opt))
}


# ---- Call-scoped format context ------------------------------------------

# The levers that have no formal travel here. Pushed on entry to a table
# function, popped by its `on.exit()`. A stack, so nested table calls
# (a bundle building sub-tables) restore correctly.
.spicy_style_stack <- new.env(parent = emptyenv())
.spicy_style_stack$frames <- list()

# The active format context, or NULL. The fast path -- no style asked
# for -- is one `length()` call, so every formatter keeps its exact
# current behavior byte for byte.
.style_fmt <- function() {
  n <- length(.spicy_style_stack$frames)
  if (n == 0L) {
    return(NULL)
  }
  .spicy_style_stack$frames[[n]]
}

.style_push <- function(fmt) {
  .spicy_style_stack$frames <- c(.spicy_style_stack$frames, list(fmt))
  invisible(NULL)
}

.style_pop <- function() {
  n <- length(.spicy_style_stack$frames)
  if (n > 0L) {
    .spicy_style_stack$frames <- .spicy_style_stack$frames[-n]
  }
  invisible(NULL)
}

# Entry point for a table function.
#
#   spec  the user's `style` argument (name, object, or NULL)
#   call  the caller's `match.call()` -- names are already expanded, so
#         partially matched arguments count as typed
#   env   the caller's `environment()`, whose formals are rewritten
#
# Returns TRUE when a context was pushed (so `on.exit()` knows whether
# to pop).
.style_begin <- function(spec, call, env) {
  # An unknown style is the caller's error: `env` is its frame.
  style <- .style_resolve(spec, call = env)
  locale <- .style_locale_defaults()
  if (is.null(style) && is.null(locale)) {
    # The fast path: no style, no language. It costs one `getOption()`
    # more than it used to and nothing else, and the table that comes
    # out is byte for byte the one that came out before.
    return(FALSE)
  }
  # The language's typography is the layer UNDER the style: the style
  # overwrites it lever by lever, and every lever the style leaves open
  # the locale keeps. `spicy_style()` stores only the levers actually
  # set, so this merge says exactly that. The style's provenance is not
  # touched -- a locale is not a style and never passes for one.
  eff <- locale
  eff[names(style)] <- style

  typed <- names(call)
  typed <- typed[nzchar(typed)]

  for (field in intersect(names(eff), .STYLE_ARG_FIELDS)) {
    # An argument the caller typed always wins over the style, even
    # when its value equals the function's own default. A lever the
    # function has no argument for is simply not its business.
    if (field %in% typed || !exists(field, envir = env, inherits = FALSE)) {
      next
    }
    assign(field, eff[[field]], envir = env)
  }

  fmt <- eff[intersect(names(eff), .STYLE_FMT_FIELDS)]
  # The abbreviation rule of a named theme ("jama", "lancet") is no lever
  # of its own: its provenance names it, a modified theme included, and
  # it rides with the levers to the note builders, as `note_label` does.
  abbrev_rule <- attr(style, "provenance", exact = TRUE)$abbrev_rule
  if (!is.null(abbrev_rule)) {
    fmt$abbrev_rule <- abbrev_rule
  }
  if (is.null(style)) {
    # A locale-only push must not mask a frame already in force: before
    # the locale existed, a call with no style pushed nothing and the
    # outer frame governed the formatters. The outer frame therefore
    # overrides the locale, lever by lever -- it came from an explicit
    # style, and the locale is the weakest layer everywhere. (A call
    # WITH a style keeps its historical semantics: its own frame,
    # outer frame masked.) No shipped entry point nests frames today;
    # this guards the first bundle that will.
    outer <- .style_fmt()
    if (!is.null(outer)) {
      fmt[names(outer)] <- outer
    }
  }
  # `p_digits` typed by the caller is a request for THAT many decimals
  # on every p-value, so the theme's own ways of choosing p-precision
  # -- bands, significant figures, and the floor derived from them --
  # step aside. The leading-zero rule is orthogonal and stays.
  if ("p_digits" %in% typed) {
    fmt$p_bands <- NULL
    fmt$p_sigfig <- NULL
    fmt$p_floor <- NULL
  }
  .style_push(fmt)
  TRUE
}

.style_end <- function(pushed) {
  if (isTRUE(pushed)) {
    .style_pop()
  }
  invisible(NULL)
}

# The default of a lever the caller typed in a form no style can hold: a
# named `digits` of the continuous tables, one value per variable, leaves
# the variables it does not name at the number the call would have used
# without it. That is what `.style_begin()` writes into an untyped
# argument: the style's value, over the language's, over `default`, the
# function's own.
.style_arg_default <- function(spec, field, default) {
  eff <- .style_locale_defaults()
  style <- .style_resolve(spec)
  eff[names(style)] <- style
  eff[[field]] %||% default
}

# The descriptive families return RAW values and re-format them at print
# time, so the style has to survive the call that built the table: it
# rides along as a `style_fmt` attribute, next to `digits`,
# `decimal_mark` and the rest of the formatting attributes.
#
# `.style_stamp()` records it on the object (a no-op with no style, so
# the object is untouched); `.style_restore()` puts it back for the
# duration of a print / render, paired with `.style_end()`.
.style_stamp <- function(x) {
  fmt <- .style_fmt()
  if (!is.null(fmt) && length(fmt) > 0L) {
    attr(x, "style_fmt") <- fmt
  }
  x
}

.style_restore <- function(x) {
  fmt <- attr(x, "style_fmt", exact = TRUE)
  if (is.null(fmt) || length(fmt) == 0L) {
    return(FALSE)
  }
  .style_push(fmt)
  TRUE
}

# The value of `format_spec$p_style` under the active style: "standard"
# when p-values keep their leading zero, "apa" (the spicy default) when
# they drop it. Consumed by the structured contract and by the Excel
# number formats derived from it.
#
# `decimal_mark` is the table's effective mark, for the same reason
# `.style_p_leading_zero()` takes it: the token is what every
# string-driven surface reads instead of calling the console's
# formatter, so it has to answer the question the same way.
.style_p_style_token <- function(decimal_mark = ".") {
  if (.style_p_leading_zero(decimal_mark)) "standard" else "apa"
}


# ---- Formatter hooks ------------------------------------------------------

# Number of decimals a p-value gets under the active style.
#   base  the caller's `p_digits`
# Returns `base` when no style, or when the style sets neither bands nor
# significant figures.
.style_p_decimals <- function(p, base) {
  fmt <- .style_fmt()
  if (is.null(fmt)) {
    return(base)
  }
  if (!is.null(fmt$p_bands)) {
    for (b in fmt$p_bands) {
      if (p < b[[1L]]) {
        return(as.integer(b[[2L]]))
      }
    }
    # nocov start -- validation forces the last cutoff to be Inf, so
    # the loop above always returns.
    return(base)
    # nocov end
  }
  if (!is.null(fmt$p_sigfig)) {
    return(.signif_decimals(p, fmt$p_sigfig, cap = base))
  }
  base
}

# Decimal places needed to show `x` with `sigfig` significant figures,
# capped at `cap`. `signif(0.0345, 2) = 0.034` -> 3 decimals.
.signif_decimals <- function(x, sigfig, cap) {
  sigfig <- as.integer(sigfig)
  if (!is.finite(x) || x == 0) {
    return(min(as.integer(cap), sigfig - 1L))
  }
  # Round FIRST, then measure: 0.0996 at 2 significant figures is 0.10,
  # whose exponent is one higher than the raw value's.
  rounded <- signif(abs(x), sigfig)
  exponent <- floor(log10(rounded))
  need <- sigfig - 1L - as.integer(exponent)
  max(0L, min(as.integer(cap), need))
}

# The "<x" floor of a p-value: the style's, or `10^-digits`.
.style_p_floor <- function(digits) {
  fmt <- .style_fmt()
  if (!is.null(fmt) && !is.null(fmt$p_floor)) {
    return(fmt$p_floor)
  }
  10^(-digits)
}

# TRUE when a p-value keeps its leading zero.
#
# Two answers, in order. A `p_style` in scope -- typed into
# `spicy_style()`, carried by a theme, or brought by a language's
# locale -- settles it outright: an explicit gesture keeps its rule
# under any mark, which is why `"apa"` under a French language still
# prints `,003` (documented in ?spicy_style). With no `p_style` at
# all, the MARK settles it: a comma with nothing before it is not a
# number the SI brochure admits (BIPM, 9th edition, section 5.4.4),
# so a comma keeps the zero. Under a point nothing moves -- the APA
# drop is spicy's own default.
#
# `decimal_mark` is the table's effective mark, threaded from the call
# site rather than read from a global, so an object formats the same
# way whatever the session is doing while it prints.
.style_p_leading_zero <- function(decimal_mark = ".") {
  fmt <- .style_fmt()
  if (!is.null(fmt$p_style)) {
    return(identical(fmt$p_style, "standard"))
  }
  identical(decimal_mark, ",")
}

# Separator between the bounds of a confidence interval. `default` is
# what the call site would have used without a style.
.style_ci_sep <- function(default) {
  fmt <- .style_fmt()
  if (!is.null(fmt) && !is.null(fmt$ci_sep)) {
    sep <- fmt$ci_sep
    # A style's ", " was sourced under a dot decimal mark (APA writes
    # "[3.45, 6.78]"; no surveyed source states a separator for a
    # comma mark). Under a comma mark that separator IS the mark, and
    # "[33,96, 40,50]" is the very ambiguity ci_bracket_separator()
    # exists to prevent -- so the lever yields to the derived "; ",
    # exactly as the French adaptations of APA style themselves write
    # (IC 95 % [3,45; 6,78]). Every other separator (" to ", an en
    # dash, ";") is unambiguous under any mark and stays absolute.
    if (identical(sep, ", ") && identical(default, "; ")) {
      return(default)
    }
    return(sep)
  }
  default
}

# Opening / closing delimiters of a confidence interval.
.style_ci_brackets <- function() {
  fmt <- .style_fmt()
  if (!is.null(fmt) && !is.null(fmt$ci_brackets)) {
    return(fmt$ci_brackets)
  }
  c("[", "]")
}
