# Spicy table engine

Index page for the ASCII rendering engine that produces every spicy
console table
([`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
the `table_*()` family, and the association-measure printers). The
engine supports Unicode line drawing, ANSI colours via crayon (with
monochrome fallback), automatic colour-aware width detection,
configurable integer padding (`0L` / `2L` / `4L`), per-column alignment,
and horizontal panelling for tables wider than the console.

## User-facing entry points

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) –
  one-way frequency tables

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  – two-way cross-tabulations

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  – multi-variable summary tables

## Rendering primitives (internal API)

- [`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
  – user-facing wrapper that adds title, note, table-type-aware
  alignment defaults, and panelling.

- [`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md)
  – the underlying string renderer.

## See also

[spicy](https://amaltawfik.github.io/spicy/reference/spicy-package.md)
for the full package overview, including the API stability tiers and the
classed-condition taxonomy.
