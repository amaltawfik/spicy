# Convert a spicy flextable output to a plain flextable

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
and
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
return their `output = "flextable"` tables with a lightweight
`spicy_flextable` class tag whose only job is HTML note styling. Every
flextable verb already works on the tagged object; this method returns
the clean underlying
[`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
– the note is already part of its footer – for workflows that want the
untagged object (e.g. custom knit hooks,
[`flextable::save_as_docx()`](https://davidgohel.github.io/flextable/reference/save_as_docx.html)
pipelines, or composition with other flextable tooling), mirroring
`gtsummary::as_flex_table()`.

## Usage

``` r
as_flextable.spicy_flextable(x, ...)
```

## Arguments

- x:

  A `spicy_flextable` object.

- ...:

  Unused; for generic consistency.

## Value

A `flextable` object.

## Details

Rendering in Quarto / R Markdown does NOT require this conversion: the
`knit_print` method auto-detects the output format and delegates to
flextable's native rendering for non-HTML targets (Word, PowerPoint,
PDF).
