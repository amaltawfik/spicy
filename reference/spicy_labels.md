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

- **`options(spicy.language = "fr")`** The language of the labels, for
  the whole document. Two sets ship: `"en"` (the default) and `"fr"`.
  The language of a report is a property of the report, so this is set
  once in a setup chunk rather than passed to each table.

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

Number FORMATTING is a separate lever. `options(spicy.language = "fr")`
translates the words; the `"fr"` style (`options(spicy.style = "fr")`,
see
[`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md))
writes the decimal comma. A French report usually wants both.

## See also

[`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
for number formatting, including the French decimal comma.

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
