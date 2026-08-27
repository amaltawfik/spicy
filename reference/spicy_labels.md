# Table labels and their language

Every string a reader of a spicy table sees – column headers, row
labels, titles, table footnotes – is held under a stable key, and
`spicy_labels()` returns those keys with the label each one currently
resolves to. It is the companion of the two options that move them.

## Usage

``` r
spicy_labels(language = NULL)
```

## Arguments

- language:

  A language name (`"en"`, `"fr"`), or `NULL` (the default) to report
  the labels in force. Any `options(spicy.labels)` override applies
  either way.

## Value

A named character vector, one element per key, in registry order.

## Global options

- **`options(spicy.language = "fr")`** The language of the table, for
  the whole document – its labels and the typography its numbers are
  written in. Two sets ship: `"en"` (the default) and `"fr"`. The
  language of a report is a property of the report, so this is set once
  in a setup chunk rather than passed to each table.

- **`options(spicy.labels = list(<key> = "<label>"))`** A per-label
  override, for the case where one word has to change and a language
  does not: a named list (or named character vector) whose names are
  keys of `spicy_labels()`. An unknown key is an error.

A label resolves through `spicy.labels`, then the `spicy.language` set,
then English. A set carries only the keys it translates, so anything it
does not name falls back to English rather than erroring or coming out
blank. Both options are cleared with `NULL`.

## What a language does not change

Only DISPLAY strings translate. The column names of the exported frames
([`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
`tidy()`,
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md))
are a documented contract that user code indexes into, so
`out[["Yes %"]]` resolves under every language, and so do the block
identities, the encoded cell tokens and the mathematical glyphs. Errors,
warnings and messages stay English: they are read by developers and
quoted in bug reports.

One exception, and it follows from the same rule. A column named after a
LEVEL of `by` takes that level's own spelling – which is why `"Yes %"`
stays `"Yes %"`: your data is never translated. spicy's own missing
category is such a level, so it is the one name that does move:
`table_categorical(by = )` on a variable with missing values gives
`"(Missing) n"` in English and `"(Manquant) n"` under `"fr"` (or
whatever `row_missing_level` is overridden to). Address that column
through `row_missing_level` rather than by typing it.

## Numbers follow the language too

A language brings its typographic locale with it, so
`options(spicy.language = "fr")` is one gesture for a coherent French
report table: French words, comma decimal mark, and the leading zero
French typography keeps on a p-value (`0,003`). The sources are the
BIPM's SI brochure and the European Union's *Code de redaction
interinstitutionnel*;
[`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
quotes them. `"en"` brings no locale, and nothing changes for anyone who
sets no language.

The locale rides the style layer, so it reaches the reporting families –
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md)
and the survey twins. The language reaches every table, the exploration
pair included:
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
have no style layer, so it sets the DEFAULT of their `decimal_mark` –
the one typographic lever they carry – and an argument you type wins
over it. Under a comma their p-value keeps its leading zero
(`p = 0,659`), the form French typography requires.

The locale sits at the BOTTOM of the formatting resolution. A journal
style outranks it – `style = "jama"` under `"fr"` gives JAMA's p-values
and the French comma, `style = "lancet"` keeps the journal's midline
decimal point – and an argument you type outranks both, so
`decimal_mark = "."` gives French words with a decimal point (the
p-value keeps its locale zero;
[`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
documents that lever).

Two timing notes. Figures are frozen when a table is BUILT, like every
formatting argument, while words resolve when it is printed – so set the
language once, at the top of the document. And a comma mark makes an
Excel export write its body as text rather than live numbers, since
Excel would otherwise re-punctuate a numeric cell with the viewer's own
locale.

## See also

[`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
for the journal styles, which outrank the language's own typography.

## Examples

``` r
head(spicy_labels())
#>                   row_missing_level             row_missing_level_dedup 
#>                         "(Missing)"                      "(Missing_%d)" 
#>                note_missing_removed       note_declared_missing_removed 
#>          "Missing values removed: " "Declared missing values removed: " 
#>                   note_missing_item             note_missing_rows_total 
#>                           "%s (%d)"                "; %d rows in total" 

# The same keys in French.
fr <- spicy_labels("fr")
fr[["header_mean"]]
#> [1] "M"
fr[["row_missing_level"]]
#> [1] "(Manquant)"

# One label, not a language: the missing CATEGORY of the grouping
# variable is a refusal to answer here, not an absent value.
old <- options(spicy.labels = list(row_missing_level = "(No answer)"))
table_categorical(sochealth, select = sex, by = smoking)
#> Categorical table by smoking
#> 
#>  Variable │ No n  No %  Yes n  Yes %  (No answer) n  (No answer) %  Total n 
#> ──────────┼─────────────────────────────────────────────────────────────────
#>  Sex      │                                                                 
#>    Female │ 475   51.3   131   52.6        14            56.0         620   
#>    Male   │ 451   48.7   118   47.4        11            44.0         580   
#> 
#>  Variable │ Total %   p    Phi 
#> ──────────┼────────────────────
#>  Sex      │          .713  .01 
#>    Female │  51.7              
#>    Male   │  48.3              
options(old)
```
