# Changelog

## spicy 0.12.0

### New features

#### `table_regression()` — regression coefficient tables

- New top-level function building a publication-ready coefficient
  summary from one or more fitted regression models. Designed for
  **APA-strict** reporting in psychology, public health, sociology, and
  biostatistics, with optional opt-in conventions for econometrics and
  clinical trials.

- Supports `lm` and `glm` (binomial / poisson / Gamma / inverse.gaussian
  / quasi families with any link). Single fit or a named / unnamed list
  of fits side by side; `lm` and `glm` may be mixed in the same call.

- `show_columns` selects per-coefficient columns from a vocabulary of
  tokens. Estimate-related: `B`, `beta`, `SE`, `CI`, `t`, `p`. Marginal
  effects: `AME`, `AME_p`, `AME_SE`. Effect sizes: `partial_f2`,
  `partial_eta2`, `partial_omega2` (`lm`; with noncentral-*F* CIs);
  `partial_chi2` (`glm`; partial likelihood- ratio χ² via
  `drop1(test = "LRT")` — SAS PROC LOGISTIC `TYPE3`, Long & Freese
  2014).

- `vcov` selects the variance estimator: `"classical"`, `"HC0"` to
  `"HC5"` (via `sandwich`), `"CR0"` to `"CR3"` with Satterthwaite-
  corrected df (via `clubSandwich`), `"bootstrap"` (nonparametric or
  cluster), or `"jackknife"` (leave-one-out / leave-one-cluster- out).
  Single value recycled to all models, or a per-model list for
  SE-comparison side by side.

- `standardized` selects the β method: `"refit"` (Cohen et al. 2003 for
  `lm`; Long & Freese 2014 §4.3.4 x-standardization for `glm`),
  `"posthoc"`, `"basic"`, `"smart"` (Gelman 2008), or `"pseudo"` (Menard
  2011 fully-standardised; `glm` only). The user’s `vcov` flows through
  to the β SE / CI / *p*.

- `exponentiate = TRUE` switches `glm` coefficients to the response
  scale and rebrands the column header per family / link: `OR` (binomial
  logit), `IRR` (poisson log), `HR` (binomial cloglog), `RR` (binomial
  log), `MR` (Gamma log), generic `exp(B)` otherwise. Mixed-family
  multi-model output gets per-model headers (`OR / IRR (per family)`).

- `ci_method = "profile"` (`glm` only) switches confidence intervals to
  the profile-likelihood form via
  [`MASS::confint.glm()`](https://rdrr.io/pkg/MASS/man/confint.html);
  estimate / SE / *t* / *p* remain Wald.

- `p_adjust` applies a multiple-comparison correction
  ([`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html):
  `"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`,
  `"fdr"`) over the per-model coefficient family. Default `"none"`
  matches APA Manual 7 §6.46 guidance against routine adjustment of
  regression coefficients.

- `keep` and `drop` are regex filters for focal-predictor display
  (mutually exclusive). `nested = TRUE` adds an APA-style
  hierarchical-comparison footer (default tokens
  `c("r2_change", "F", "p")` for `lm`, `c("LRT", "p")` for `glm`).

- AME under cluster-robust variance gets Satterthwaite-corrected df via
  [`clubSandwich::linear_contrast()`](http://jepusto.github.io/clubSandwich/reference/linear_contrast.md)
  for `lm` and
  [`clubSandwich::coef_test()`](http://jepusto.github.io/clubSandwich/reference/coef_test.md)
  (dominant-coefficient approximation, Pustejovsky & Tipton 2018) for
  `glm`. spicy is the **first R package** to offer this for `lm`.

- Outputs: console (default `spicy_regression_table` print method),
  `data.frame`, long tibble, `gt`, `flextable`, `tinytable`, Excel,
  Word, clipboard. Full
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  integration for downstream `modelsummary` / custom workflows.

- **Multi-model column spanners**: when two or more fits share the
  table, each model’s sub-columns (`B` / `SE` / `95% CI` / `p` / …) are
  grouped under a centered spanner label drawn above the header — by
  default in the console (ASCII), and via the native spanner API in
  [`gt::tab_spanner()`](https://gt.rstudio.com/reference/tab_spanner.html),
  [`flextable::add_header_row()`](https://davidgohel.github.io/flextable/reference/add_header_row.html),
  [`tinytable::group_tt()`](https://vincentarelbundock.github.io/tinytable/man/group_tt.html),
  and
  [`openxlsx2::wb_merge_cells()`](https://janmarvin.github.io/openxlsx2/reference/wb_merge_cells.html)
  (Word follows `flextable`). The redundant `"Step 1: B"`-style prefix
  is stripped from displayed sub-column headers when a spanner is shown.
  Plain `data.frame` / `long` / `clipboard` outputs keep the unambiguous
  prefixed names.

- **DV-in-spanner smart default**: when models have all-distinct
  response variables and no explicit `model_labels` / `names(list)` is
  supplied, the bare DV name (from `formula(fit)[[2]]`, *not*
  `attr("label")`, which can be long and would distort column widths) is
  lifted into the spanner labels instead of the generic
  `"Model 1, 2, ..."`. User-supplied `model_labels` / list names always
  win.

- **`outcome_labels` row hidden by default**. With the multi-model
  spanner now carrying the DV / model name, the body Outcome row is
  redundant. `outcome_labels = NULL` (the default) hides it; pass
  `outcome_labels = c(...)` to opt back in.

- **Context-aware `show_columns` default**. A single-model table keeps
  the APA-7 §6.46 publication layout `c("B", "SE", "CI", "p")`; a
  multi-model table drops `"CI"` to fit the side-by-side layout
  (`c("B", "SE", "p")`). Override explicitly via `show_columns` when
  needed. Matches the modelsummary default.

- **Compact `padding = 0L` default**. The console renderer now uses zero
  extra column padding by default for
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  (was `2L`), matching modelsummary’s tight layout. Pass `padding = 2L`
  or `4L` to restore the previous look. Other spicy tables
  ([`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) /
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  /
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  /
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md))
  keep their original `padding = 2L` default.

- **Partial naming of the models list is accepted**.
  `list("Step 1" = m1, m2)` now works: the named slot keeps its label,
  the unnamed slot auto-fills as `"Model <position>"` (here
  `"Model 2"`). Previously this was a hard error. Duplicate explicit
  names are still rejected (they would silently collide in the
  long-format model_id key).

- **`show_columns` vocabulary — lowercase atomic tokens + group
  presets**. The token vocabulary is now all-lowercase and
  one-token-per-displayed-column (no more bundled `value [CI]` cells
  outside `partial_chi2`):

  | \<= 0.11 (mixed case + bundled) | 0.12 atomic (lowercase) |
  |----|----|
  | `"B"`, `"SE"`, `"CI"`, `"t"`, `"p"`, `"beta"` | `"b"`, `"se"`, `"ci"`, `"t"`, `"p"`, `"beta"` |
  | `"AME"` (bundled `value [CI]`) | `"ame"` + `"ame_ci"` (split) |
  | `"AME_SE"`, `"AME_p"` | `"ame_se"`, `"ame_p"` |
  | `"partial_f2"` (bundled), `"partial_eta2"`, `"partial_omega2"` | `"partial_f2"` + `"partial_f2_ci"` (split); same for eta² / omega² |
  | `"partial_chi2"` (bundled `value (df)`) | `"partial_chi2"` (kept bundled — universal `χ²(df)` convention) |

  Passing a legacy uppercase token raises an actionable migration error
  pointing at the replacement (no silent aliasing).

- **`show_columns` group tokens** (Stata / SPSS / SAS-style presets).
  Each group expands to a fixed vector of atomic tokens before
  validation:

  - `"all_b"` -\> `c("b", "se", "ci", "p")` (single-model default)
  - `"all_b_compact"` -\> `c("b", "se", "p")` (multi-model default)
  - `"all_b_full"` -\> `c("b", "se", "ci", "t", "p")`
  - `"all_beta"` -\> `c("b", "beta", "se", "ci", "p")`
  - `"all_ame"` -\> `c("ame", "ame_se", "ame_ci", "ame_p")`
  - `"all_ame_compact"` -\> `c("ame", "ame_p")`
  - `"all_f2"` / `"all_eta2"` / `"all_omega2"` -\> `partial_*` + its
    `_ci` companion

  Mix groups and atomic tokens freely:
  `show_columns = c("all_b", "ame", "ame_p")`.

- **Regression-type declared in the footer note**. Every footer now
  starts with the model’s regression type (“Linear regression.”,
  “Logistic regression.”, “Poisson regression.”, …). Multi-model
  same-type tables use the plural form (“Linear regression models.”);
  mixed-class multi-model tables enumerate per position (“Model 1:
  linear regression; Model 2: logistic regression.”). The title now also
  carries the type for `lm` (“Linear regression: mpg” instead of
  “Regression: mpg”).

- **`factor_layout = c("grouped", "flat")`** controls how factor
  predictors are rendered. `"grouped"` (default) puts the factor name on
  its own header row + indents each level (APA / `gtsummary`
  convention). `"flat"` concatenates `<var><level>` on each row, no
  header (Stata / `parameters` / `modelsummary` convention). Applies to
  **any categorical predictor** (factor, ordered, character, logical) –
  R’s \[stats::model.frame()\] coerces character / logical to factors at
  fit time.

- **`reference_style` extended to 4 modes** (was 2). Distinguishes WHERE
  the reference category is exposed:

  - `"row"` (default): explicit row `Female (ref.)` with em-dash
    (gtsummary / NEJM / BMJ convention).
  - `"annotation"`: row dropped; reference shown inline. Grouped layout
    puts `[ref: Lower]` in the factor header; flat layout attaches
    `[vs Lower]` to the **first non-reference dummy** of each factor
    (later dummies inherit).
  - `"footer"` *(new)*: row dropped; a single line
    `Reference categories: education = Lower; sex = Female.` is added to
    the footer note. SAS `PROC LOGISTIC` / SPSS “Categorical Variables
    Codings” convention. Recommended for publication-grade dense
    multi-factor tables.
  - `"none"` *(new)*: row dropped; no reference information anywhere.
    User responsibility to state the convention in the surrounding text.
    When combined with `factor_layout = "flat"` (the only combo with NO
    visual trace of the factor’s reference), an informational
    `spicy_silent_reference` message is emitted once so the silent
    omission is at least acknowledged.

- **`reference_label` scoped to `reference_style = "row"`**. The suffix
  string (“(ref.)”) only applies to the row mode. The other three modes
  use structural English wording (“ref:”, “vs”, “Reference categories:”)
  that is intentionally hard-coded.

- **AME / B-p caveat narrowed and trimmed**. The warning that fired when
  `ame` and `p` are shown without `ame_p` is now only raised when
  divergence between the two p-values is actually plausible — i.e., any
  model is a `glm` (non-identity link) or has an interaction /
  non-linear transform. For a pure additive `lm`, the two p-values are
  mathematically identical, so the caveat is suppressed. The message is
  also shorter: one main line + one hint (was three lines).

- **`cluster` accepts formula / string / vector** (in order of
  preference). Aligns the public API with the rest of the stats R
  ecosystem (sandwich, clubSandwich, fixest, marginaleffects,
  parameters):

  - `cluster = ~region` (formula): variables looked up in
    `model.frame(fit)` first, then in the original `data` argument.
    Composable for multi-way clustering (`cluster = ~region:year`).
    **Recommended.**
  - `cluster = "region"` (string): single column name, same resolution.
  - `cluster = df$region` (vector): atomic vector of length `nobs(fit)`.
    Use for derived keys (`cluster = interaction(df$a, df$b)`, etc.) or
    external data.

  Bare unquoted names (`cluster = region`) are explicitly rejected –
  they require non-standard evaluation magic that breaks under
  programmatic use (function wrapping, dynamic column choice, loops).
  The error message points to the formula form.

- **Nested model comparison moved from footer to in-table rows** (APA
  Table 7.13 / Stata `esttab` / SPSS “Model Summary” convention). When
  `nested = TRUE`, each adjacent pair (M2 vs M1, M3 vs M2, …)
  contributes one column of change stats; the FIRST model column gets
  em-dashes (no previous model to compare to). The
  `── Model comparison ──` footer block was removed.

  Change tokens (use them like any other `show_fit_stats` entry):
  `r2_change`, `adj_r2_change`, `f_change`, `f2_change`, `lrt_change`,
  `aic_change`, `aicc_change`, `bic_change`, `deviance_change`,
  `p_change`.

  Class-aware auto-injection under `nested = TRUE` (when
  `show_fit_stats` is `NULL`): lm hierarchies get
  `c("r2_change", "f_change", "p_change")`; glm hierarchies get
  `c("lrt_change", "p_change")`. Variance-explained change tokens on
  all-glm fits are rejected with a hint to `lrt_change`.

  Display: explicit `+` / `-` sign on delta values, APA-style p-value
  formatting (`<.001` etc.), suffix label “(change)” on the p-value row
  to disambiguate from the per-coefficient `p`.

  The `nested_stats` argument was removed – it duplicated
  `show_fit_stats` for the same job. Customise change-stat display by
  passing change tokens directly to `show_fit_stats`. The order of
  tokens in `show_fit_stats` controls the order of the resulting rows.

- **Polynomial-contrast note made publication-ready** and split into two
  channels:

  - **Table footer** – one sentence, statistics vocabulary only, no
    software / function-name leak. The suffix legend lists *only* the
    polynomial degrees actually shown in the table:
    `Ordered factor \`education\`: polynomial trends (.L = linear, .Q =
    quadratic).`(3-level factor);`Ordered factor \`f5\`: polynomial
    trends (.L = linear, .Q = quadratic, .C = cubic, ^4 = quartic).\`
    (5-level factor).
  - **Once-per-session pedagogy** via
    `rlang::inform(.frequency = "once")` with class
    `spicy_polynomial_contrasts_info`. The first time any
    [`ordered()`](https://rdrr.io/r/base/factor.html) factor is rendered
    in a session, an info-level message explains what polynomial
    contrasts are and how to switch to per-level (treatment) display.
    Subsequent calls in the same session stay silent.

- **Ordered-factor (`contr.poly`) grouping + auto note**. Ordered
  factors are no longer rendered as a flat list of `<var>.L`, `<var>.Q`,
  … rows mixed in with the numeric predictors. They are now grouped
  under the factor name (like a treatment factor) with sublabels `.L`,
  `.Q`, `.C`, `^k` in polynomial- degree order. No reference row is
  emitted (none exists for `contr.poly`). An auto English footer note
  explains the parameterisation and points the user to the per-level
  alternative (`factor(x, ordered = FALSE)` or
  `options(contrasts = ...)`). Other R packages (modelsummary,
  gtsummary, parameters, jtools, sjPlot) display the bare suffixes
  without grouping or context — this is the first R package to surface
  the issue.

- Auto-generated APA footer documents the SE family, the AME-
  Satterthwaite path (when active), the exponentiate transform, the
  standardisation method, the multiple-comparison adjustment, the
  significance-stars threshold, and any rank-deficient or hierarchical
  metadata — no manual annotation needed.

- Cross-validated to machine precision against
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  (B / SE / *z* / *p* / Wald CI),
  [`MASS::confint.glm()`](https://rdrr.io/pkg/MASS/man/confint.html)
  (profile CI), `performance::r2_*` (McFadden / Nagelkerke / Tjur
  pseudo-R²), and
  [`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
  (AME).

- See
  [`?table_regression`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  and
  [`vignette("table-regression")`](https://amaltawfik.github.io/spicy/articles/table-regression.md)
  for the full argument reference, runnable examples spanning every
  feature, and the cross-software equivalence map (Stata / SPSS / SAS
  conventions).

- `marginaleffects`, `parameters`, and `performance` added to `Suggests`
  (oracle packages for AME and pseudo-R² cross- validation); no runtime
  dependency change.

#### Quality and portability

- **R CMD check WARNING cleared across every `R/*.R` file.** Every
  non-ASCII byte in `R/` source has been replaced – inside string
  literals with `\uXXXX` escape sequences (runtime behaviour is
  identical) and inside comments with ASCII transliterations (`--` for
  em-dash, `->` for arrow, `beta`/`eta`/`omega`/… for Greek letters,
  `Section X.Y` for `§X.Y`, etc.). 22 files in `R/` are now byte-pure
  ASCII;
  [`tools::showNonASCIIfile()`](https://rdrr.io/r/tools/showNonASCII.html)
  reports zero hits package-wide. Documentation that needs rendered
  Greek (, , …) already uses the canonical `\eqn{}{}` markup, so the
  visible output of
  [`?table_regression`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  and the PDF manual are unaffected.

- **[`openxlsx2::wb_add_border()`](https://janmarvin.github.io/openxlsx2/reference/wb_add_border.html)
  default-args bug fixed across every caller.** The function defaults
  `top_border = bottom_border = left_border = right_border = "thin"`, so
  an explicit `top_border = "thin"` call (or any single-side call)
  paints all four sides with a “thin” rule and produces unwanted
  vertical lines on every styled cell.
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  and
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  now pass `NULL` on every unused side to draw only the intended rule –
  matching the fix already in place in
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

#### `table_regression()` – `fit_stats_layout` argument for cell-merged fit-stat rows

- **New argument** `fit_stats_layout = c("first_col", "merged")`
  controlling the placement of fit-stat values (`n`, `R²`, `AIC`, …)
  within each model’s column group:

- **Engine support for `"merged"`**: `gt` lacks a native row-spanning
  cell-merge API; `tinytable`’s `style_tt(colspan = N)` emits HTML
  `colspan` only on header rows, not body cells; `clipboard` ships TSV
  plaintext; `default` ships fixed-width ASCII. Those four outputs
  always render in `"first_col"` mode regardless of this setting.

- **Decimal alignment preserved in both modes**: every numeric column
  decimal-aligns its values. Under `"first_col"`, the B column
  decimal-aligns coefficient values plus the integer / decimal fit-stat
  values that share it (native primitives handle mixed precision via
  decimal padding). Under `"merged"`, the B column contains coefficients
  only and trivially decimal- aligns; fit-stat values move into the
  merged cell.

#### `table_regression()` – CI columns split into LL / UL across all structured outputs

- **Rich engines (gt, flextable, tinytable, excel, word) and clipboard**
  now split the bracketed CI cell `[LL, UL]` into two separate numeric
  columns under a `(95% CI)` spanner row – same layout that
  `table_continuous` / `table_continuous_lm` already use. With clean
  numeric strings in every CI cell, the native decimal-alignment
  primitives of each engine work without any custom-font / NBSP /
  `white-space: pre` hack:

  - [`gt::cols_align_decimal()`](https://gt.rstudio.com/reference/cols_align_decimal.html)
    aligns every numeric column natively; default proportional font (no
    Fira Mono override).
  - `tinytable::style_tt(align = "d")` aligns natively via `siunitx`
    (LaTeX) and padded text (HTML); default font (no monospace override,
    no NBSP pre-padding).
  - `flextable` right-aligns the numeric cells against Calibri’s tabular
    figures (no Consolas override).
  - `excel` right-aligns numerics in Calibri’s tabular figures (no
    Consolas override).
  - `clipboard` writes the two-level header (model spanner row,
    `(95% CI)` spanner row, column labels) and each bound is its own
    numeric cell – usable directly after paste, no parsing required.

- **Console (`output = "default"`)** and **`output = "data.frame"` /
  `"long"`** are unchanged: the `[LL, UL]` bracketed cell stays as the
  terminal-friendly representation and the long format already exposes
  `ci_low` / `ci_high` separately.

- New shared helpers `.split_ci_columns()` and `.parse_ci_bracketed()`
  factor the split logic out of the five dispatchers. The bracketed-CI
  parser handles plain `,` separation, `;` separation (European decimal
  mark), em-dashes on reference rows, and empty / whitespace-only cells.

#### `table_regression()` – Excel font scoped + HTML decimal alignment fixed

- **Excel**: the previous revision applied Consolas across every cell
  (title, header, body, notes); the present revision scopes Consolas to
  the **numeric body cells only**. Title, the Variable column, headers,
  and footer notes now render in Excel’s default Calibri. The pre-padded
  numeric cells still align decimal points thanks to the monospaced
  font, so swapping to Calibri for the surrounding cells does not break
  the decimal alignment.

- **gt** and **tinytable**: `cols_align_decimal()` /
  `style_tt(align = "d")` were left-aligning every cell whose content
  was not a clean numeric string (em-dashes for reference rows, `<.001`
  p-values, empty fit-stat cells in the second model). The dispatchers
  now keep the render layer’s pre-padding and apply a monospaced font +
  `white-space: pre` to numeric body cells via `tab_style` /
  `style_tt(monospace = TRUE, html_css = "white-space: pre;")`. HTML
  otherwise collapses the leading-space padding (default
  `white-space: normal`); preserving it together with a monospaced font
  is what produces the aligned decimal points across rows.

- **Non-ASCII regression**: `R/regression_dispatch.R` contained em-dash
  and `─` characters (Unicode horizontal box drawing) in comments that
  triggered the R CMD check `code files for non-ASCII characters`
  WARNING under a CI `error-on = "warning"` configuration. All such
  characters have been replaced with ASCII equivalents (`--`, `-`, `x`).

#### `table_regression()` – rich-output engines aligned with `table_continuous_lm` conventions

- **gt** now uses
  [`gt::cols_align_decimal()`](https://gt.rstudio.com/reference/cols_align_decimal.html)
  (the native decimal- alignment primitive) on every non-`Variable`
  column. The whitespace pre-padding produced by
  `render_regression_table()` is stripped before handing the data frame
  to gt so the primitive receives raw numeric strings. Default font; no
  monospace override.

- **tinytable** now uses `tinytable::style_tt(j = ..., align = "d")` per
  numeric column (native LaTeX `siunitx` / HTML CSS decimal alignment).
  Default font; no monospace override.

- **flextable** reverts to the `table_continuous_lm` pattern: the
  Consolas monospaced font and right-aligned numerics are applied ONLY
  to the body’s numeric cells, not to the title, header, Variable
  column, or footer notes. `autofit()` (no fixed `width = 1`) sizes
  columns to their content.

- **Clipboard** no longer injects U+2500 `─` rule rows. TSV cannot
  encode a continuous horizontal stroke – the tab-segmented dash rows
  used in an earlier revision pasted as visually broken bits of lines.
  The payload now matches `table_continuous_lm`’s clipboard shape
  (title, spanner, header, body, note) with the pre-padded body values
  preserved so a paste into a fixed-width context (Word with Consolas
  applied, plain-text editor) reproduces decimal alignment.

#### `table_regression()` — visual polish for rich-output engines

- **Decimal alignment** now reaches `gt`, `flextable`, `tinytable` and
  `excel`. Numeric body cells receive a monospaced font (Fira Mono /
  Consolas / `font-family: monospace`) plus centre / right alignment so
  the pre-padded values produced by `render_regression_table()` line up
  vertically across rows. Previously decimal alignment was preserved
  only by the default ASCII print (which already runs in a fixed-width
  terminal).

- **Column headers are centred** across all rich-output engines (the
  spanner row and the sub-column row in `gt`, `flextable`, `tinytable`,
  `excel`).

- **Factor-level rows are indented** in `gt` (via
  `cell_text(indent = ...)`), `flextable` (via `padding.left`) and
  `tinytable` (via `style_tt(indent = ..., html_css = ...)`). The
  whitespace prefix produced by the render layer is now trimmed from the
  displayed cell and the indent is carried by styling, so HTML / Word do
  not collapse it.

- **`flextable` width**: rich-output `flextable` now uses
  `layout = "autofit"` + `autofit()` so default CI columns no longer
  wrap onto two lines.

- **Excel borders**:
  [`openxlsx2::wb_add_border()`](https://janmarvin.github.io/openxlsx2/reference/wb_add_border.html)
  defaults every side to `"thin"`; an explicit `top_border = "thin"`
  therefore painted *all four* sides as “thin”, producing unwanted
  vertical lines on every cell of a row that had received a top / bottom
  rule, and an extra horizontal rule above the final body row. Each
  border call now passes `NULL` for the unused sides; only the intended
  rule is drawn.

#### `table_regression()` — banner overrides and APA borders across all engines

- New `title` and `note` arguments (unified API): `NULL` (default,
  auto-build), `FALSE` (suppress the banner row), or a character string
  (override verbatim). The validator rejects `TRUE` and non- scalar /
  non-character inputs with a remediation hint, so user typos like
  `title = TURE` are surfaced rather than silently mapped. Use
  `title = FALSE` when the manuscript supplies its own caption and the
  in-table banner would duplicate it.

- APA horizontal rules now reach **every** rich-output engine (`gt`,
  `flextable`, `tinytable`, `excel`), matching the layout already used
  by
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  /
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
  Five rules in the standard layout:

  1.  top rule above the spanner / header,
  2.  mid-rule under each multi-model spanner range,
  3.  sub-rule under the column header,
  4.  **new** hair rule between the last coefficient row and the first
      fit-stat row (uses the existing `group_sep_rows` cue),
  5.  bottom rule below the final body row.

- `output = "clipboard"` now mirrors the Excel layout: title row,
  spanner row, header row, body, footer note. Pasting into Excel or Word
  reproduces the full table structure (previously only the body
  data.frame was copied, dropping title, multi-model spanner, and note).

#### Covariate adjustment in `table_continuous_lm()`

- New `covariates` argument adds additive covariates to each per-outcome
  model. Estimate, SE, *p*-value and CI on `by` become
  covariate-adjusted; effect sizes adapt accordingly. Accepts a
  tidyselect expression or a literal character vector; covariates
  overlapping `select` are auto-excluded from the outcome list. Formula
  syntax (interactions, transforms) is reserved for a future release.
- New `adjustment` argument selects the estimand for the per-group
  adjusted means displayed in the table: `"proportional"` (default,
  G-computation; matches Stata `margins` and
  `marginaleffects::avg_predictions(variables)`) or `"balanced"`
  (synthetic-grid equal-weight; matches
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  default, SPSS UNIANOVA, SAS LSMEANS).
- Under adjustment, `"f2"` and `"omega2"` become **partial** *f²* / *ω²*
  via [`stats::drop1()`](https://rdrr.io/r/stats/add1.html) restricted
  to the focal term; `"d"` and `"g"` are undefined under adjustment and
  rejected.
- The default print emits an APA footer
  `Note. Adjusted for <covs> (<method>).`
- See
  [`?table_continuous_lm`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  and
  [`vignette("table-continuous-lm")`](https://amaltawfik.github.io/spicy/articles/table-continuous-lm.md)
  for full semantics and runnable examples.

### Bug fixes

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  no longer over-truncates a *p*-value in `(10^-p_digits, 0.001)` when
  `p_digits >= 4`. Example: `p = 0.000108` now correctly prints as
  `".0001"` at `p_digits = 4` (was `"<.0001"`).
- `count_n(special = ...)` returns a length-`nrow(data)` zero vector
  when no usable column survives the list-column filter, matching the
  documented contract and the `count = ...` branch (was `numeric(0)`,
  which broke
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  pipelines).
- [`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md)
  and
  [`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md)
  emit `spicy_undefined_stat` and return the fully-`NA` result on rank-1
  contingency tables (constant predicted variable), matching the
  existing pattern in
  [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
  [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
  [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md)
  and
  [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md).
- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  no longer silently overwrites a user’s y-variable level named `"N"`,
  `"Total"` or `"Values"`. The conflicting spicy-internal column is
  auto-renamed with a numbered suffix and a single
  `spicy_renamed_column` warning is emitted. Companion to the 0.11.0
  row-level fix.
- [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  on a `spicy_continuous_lm_table` keeps `df.residual` numeric, so
  Satterthwaite degrees of freedom from `vcov = "CR2"` / `"CR3"` are
  preserved verbatim instead of truncated through
  [`as.integer()`](https://rdrr.io/r/base/integer.html).

### Breaking changes

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  no longer silently truncates the export filename to 120 characters:
  very long titles surface a noisy OS-level error rather than a silently
  shortened file. **Migration**: shorten the title or pass an explicit
  `filename =`.

## spicy 0.11.0

CRAN release: 2026-05-04

### New features

#### `table_continuous_lm()`

- Cluster-robust SEs via `cluster` and four `vcov` choices
  (`"CR0"`–`"CR3"`), dispatched to `clubSandwich` with Satterthwaite df
  (`clubSandwich` in `Suggests`).
- `vcov = "bootstrap"` (nonparametric or cluster) and
  `vcov = "jackknife"` (leave-one-out / leave-one-cluster-out) variance
  estimators in pure base R, controlled by `boot_n`.
- Three new `effect_size` choices alongside `"f2"`: Cohen’s `"d"`,
  Hedges’ `"g"` (two-group only), Hays’ `"omega2"`. New `effect_size_ci`
  adds noncentral *t* / *F* CIs rendered inline as `0.18 [0.07, 0.30]`.
- `HC*` estimators delegate to
  [`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html);
  rank-deficient fits return a clean rank-by-rank covariance.

#### Harmonisation across the table family

- Shared reporting vocabulary (`decimal_mark`, `p_digits`, `align`,
  named-`labels`) now spans
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
  the three `table_*()` helpers, including APA-style p-value notation
  (`<.001` / `.045`, no leading zero).
- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)’s
  `assoc_measure` accepts a per-variable spec. When measures differ
  across rows the column collapses to `"Effect size"` and an APA-style
  `Note.` line documents the per-variable measure; `phi` on a non-2x2
  errors.
- All three `table_*()` functions gain
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  methods (`broom` in `Suggests`).

### Quality and robustness

- **Classed conditions.** Errors and warnings now carry stable classes
  (`spicy_error` / `spicy_warning` plus 11 leaf classes documented in
  [`?spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md)),
  so downstream code can dispatch via
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) /
  [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)
  instead of matching message strings. `rlang (>= 1.1.0)` required.
- **Structured cli messages.** Multi-line errors and warnings (vcov
  fallbacks, bootstrap/jackknife failures, `padding` migration, `labels`
  length mismatch) render as cli bullets.
- **Locale-deterministic ordering.** Sorts in
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md),
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  and `table_*()` use `method = "radix"`. Output is byte-stable across
  locales and platforms, matching Stata / SPSS guarantees.
- **Edge-case hardening.** A new length-guarded sort helper makes
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  /
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  /
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  / [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
  survive zero-length or all-NA `Date` / `POSIXct` / `character` columns
  and factors with no observed levels.
- **Snapshot-locked rendering.** `tests/testthat/test-snapshots.R` pins
  the exact console output of every spicy print method, so any
  unintended formatting drift surfaces as a PR diff.
- **API stability contract.**
  [`?spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md)
  documents which exports are stable, stabilising or internal. pkgdown
  reference groups exports via four `@family` tags.
- **Cross-software validation.** All 13 association measures agree with
  PSPP 2.0 (`CROSSTABS /STATISTICS=ALL`, 65 / 65 statistics on four
  datasets); Cohen’s *d* and Hedges’ *g* noncentral CIs are tested
  numerically against
  [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html)
  /
  [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html)
  (`tolerance = 1e-6`); point-estimate formulas and asymptotic standard
  errors follow `DescTools` (Signorell et al.).

### Improvements

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  warns when `correct = TRUE` is ignored on a non-2x2 sub-table, when
  `weights` contains `NA`, and notes statistics computed on a sub-table
  after empty rows / columns are pruned.
- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  validates `decimal_mark`, `p_digits` and `simulate_B` up front;
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
  validates `decimal_mark` and tightens `digits` to a non-negative
  integer.
- A user category literally named `"N"` or `"Total"` is no longer
  mis-rendered as the totals row in
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
- `table_continuous_lm(output = "long")` returns `n`, `df1`, `df2` as
  integer columns; `predictor_label` preserved on the degenerate-model
  fallback path.
- [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
  / [`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md) doc
  states the CI uses the Fisher z-transformation (point estimate and
  p-value identical to `DescTools` / SPSS).
- [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md)
  doc states entropy uses `0 log 0 = 0` (matching SPSS, PSPP, Stata,
  Cover & Thomas).

### Bug fixes

- [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  raises actionable errors on duplicate or empty new column names; trims
  whitespace and preserves the input class.
- `table_continuous_lm(output = "data.frame")` names contrast CI columns
  from `ci_level` (was hardcoded to 95 %).
- The categorical-predictor global Wald *F* degrades to `NA` on a
  singular coefficient covariance submatrix.
- The degenerate-table branch of
  [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
  [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md),
  [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
  [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md)
  and
  [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md)
  respects `detail`: scalar `NA_real_` by default, fully shaped
  `spicy_assoc_detail` when `detail = TRUE`.
- [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md)
  returns a finite estimate (was `NaN`) when a marginal is zero.
- `somers_d(direction = "symmetric")` returns the harmonic mean of the
  two asymmetric values, matching SPSS / PSPP `CROSSTABS`.
- [`print.spicy_assoc_detail()`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_detail.md)
  /
  [`print.spicy_assoc_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_table.md)
  use APA-strict `<.001` / `.045` notation, matching the rest of the
  package.
- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  /
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  honour `factor_levels = "all"` for `haven_labelled` columns:
  declared-but-unobserved labels appear in the `Values` summary.
- [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md)
  rejects `row.names.as.col` vectors of length ≠ 1 and empty strings;
  accumulates all messages from
  [`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md)
  instead of overwriting.
- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md) /
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)
  reject non-integer `min_valid >= 1` and `min_valid > ncol`; their
  `digits` requires a non-negative integer.

### Breaking changes

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  and
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  default to decimal-point alignment for numeric columns
  (`align = "decimal"`). Pass `align = "auto"` for the previous
  behaviour.
- [`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md)
  /
  [`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md):
  `padding` switches from a string enum to a non-negative integer.
  Default `2L` (was `+5L`); printed tables are roughly 40 % narrower.
  **Migration**: `"compact" -> 0L`, `"normal" -> 2L`, `"wide" -> 4L`.
- `table_categorical(assoc_measure = "auto")` on a 2x2 table picks `phi`
  instead of `cramer_v`. Numeric value unchanged (\|phi\| = V on 2x2);
  only the column label changes.
- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) drops
  observations with `NA` weights (with a warning) instead of recoding
  them to zero. Aligns with
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
- `table_continuous_lm(output = "long")` returns `NA` in `es_type` /
  `es_value` when `effect_size = "none"` (was `"f2"`), and renames
  `sum_w` to `weighted_n`.

## spicy 0.10.0

CRAN release: 2026-04-27

### New features

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  now accepts tidyselect-style variable selectors through `...`,
  matching
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  and [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md).

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  gains a `filename` argument for the base name of CSV, Excel, and PDF
  exports. When `NULL` (the default), the filename is derived from
  `title` and falls back to `"Codebook"` when needed. Filenames are
  sanitized to portable ASCII consistently across platforms.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now summarizes matrix and array columns by their dimensions, and
  counts valid, missing, and distinct observations by rows.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) gains
  a `factor_levels` argument that mirrors
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  and
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md).
  With `factor_levels = "all"`, declared-but-unobserved factor and
  labelled levels appear in the output with `n = 0`, matching SPSS
  `FREQUENCIES`; the default `"observed"` preserves the previous Stata
  `tab`-style behavior.

### Improvements

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now displays missing values as `<NA>` and `<NaN>` in the `Values`
  summary when `include_na = TRUE`, and quotes literal `"NA"`, `"NaN"`,
  and empty-string values so they cannot be confused with the missing
  markers.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now emits a column-named warning and marks the failing cell as
  `<error: ...>` when a column cannot be summarized, instead of silently
  writing `"Invalid or unsupported format"`. Remaining columns are
  unaffected.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  produces more precise Viewer titles for extraction, pipe, and literal
  `get("name")` expressions, while keeping ambiguous dynamic calls
  anonymous (`vl: <data>`).

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  now rejects partial-match names in `...` (e.g. `val = TRUE`,
  `tit = "x"`) that would otherwise be silently treated as tidyselect
  expressions, and surfaces
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  selection errors directly.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  resolves the `weights` argument via tidy-eval, so column references
  nested in compound expressions
  (e.g. `weights = if (use_w) col else NULL`) work as expected.
  Qualified expressions like `weights = df2$w` continue to take
  precedence over column lookup.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
  validates `digits`, `sort`, `weights`, and the logical scalar
  arguments (`valid`, `cum`, `rescale`, `styled`) more strictly at the
  public boundary, with clearer error messages for non-finite values,
  `NA`, multi-element inputs, and non-numeric weight vectors.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  documents the interaction of `weights` containing `NA` with
  `rescale = TRUE` (Stata `pweight` semantics) and the dropping of
  unused factor / labelled levels (Stata `tab` semantics, with
  `code_book(factor_levels = "all")` as the schema-style alternative).

### Bug fixes

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now displays labelled values in the same prefixed-label order for
  compact and `values = TRUE` summaries; previously the compact summary
  used data order.

- `varlist(values = TRUE)` now deduplicates element types when
  summarizing list-columns. Previously `list(1L, 2L, "a")` produced
  `"List(3): character, integer, integer"`; now produces
  `"List(3): character, integer"`.

- `include_na = TRUE` now correctly appends `<NA>` markers for
  list-columns in both
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  modes; previously it had no effect on this column type.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now validates column names up front and gives clearer errors for
  missing, empty, `NA`, or duplicate names.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now errors clearly when tidyselect expressions try to rename columns;
  `...` is for selecting variables, not renaming.

- `freq(data, x, weights = NULL)` now correctly treats the explicit
  `NULL` as “no weighting” instead of emitting a misleading
  `"variable 'NULL' not found"` error. Parameterized patterns like
  `weights = if (use_w) wts else NULL` are now supported.

- [`print()`](https://rdrr.io/r/base/print.html) for `spicy_freq_table`
  no longer crashes when the `var_label` attribute is `NA_character_`,
  numeric, or multi-element; the `Label:` line is silently skipped for
  any value that is not a single non-empty string.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) no
  longer surfaces the name of the ignored `data` vector in the printed
  footer when both `data` and `x` are passed as vectors. The footer now
  consistently shows the analyzed vector’s name.

## spicy 0.9.0

CRAN release: 2026-04-20

### Breaking changes

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  now enables inferential output by default when `by` is supplied. With
  a grouping variable, the `p` column from `test` is shown automatically
  (previous default hid it). This aligns the two table helpers:
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  stays descriptive when `by` is absent, and reports the test *p*-value
  when `by` is supplied, matching
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)’s
  inferential default. To preserve the previous behavior, pass
  `p_value = FALSE` explicitly. `statistic` and `effect_size` remain
  `FALSE` by default and must still be enabled consciously.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now displays observed factor levels by default in `Values`, matching
  its role as a quick inspection of the current data. Use
  `factor_levels = "all"` to display unused factor levels as well, which
  was the previous default behavior and remains the default in
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md).

### Minor improvements

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  gains a `factor_levels` argument. It defaults to `"all"` so exported
  codebooks continue to document all declared factor levels, including
  unused levels; use `"observed"` to mirror
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  output.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  prints the `Freq.` column as integers regardless of `digits`, which
  continues to control percentage precision. This matches the convention
  of SPSS, Stata, and SAS `PROC FREQ` for weighted counts and keeps the
  two numeric concepts (discrete counts vs. continuous percentages)
  visually distinct.

- `freq(..., styled = FALSE)` now returns a genuinely plain `data.frame`
  with no `spicy_freq_table` rendering metadata clinging to it, so
  [`str()`](https://rdrr.io/r/utils/str.html),
  [`dput()`](https://rdrr.io/r/base/dput.html), and downstream
  programmatic use see only the tabulation columns. The metadata
  attributes (`digits`, `data_name`, `var_name`, `var_label`,
  `class_name`, `n_total`, `n_valid`, `weighted`, `rescaled`,
  `weight_var`) are now documented in `@return` and remain available on
  the invisibly returned `spicy_freq_table` object when `styled = TRUE`
  (the default).

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  documentation now clarifies why `p_value = TRUE` and `r2 = "r2"` are
  the defaults, and robust-variance fallback warnings are now more
  explicit when a model matrix is singular.

### Bug fixes

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  correctly resolves qualified weight expressions such as
  `weights = other$w` or `weights = other[["w"]]` even when the
  referenced column name also exists in `data`. Previously the bare-name
  fallback could silently pull the weight vector from the wrong data
  frame when column names collided.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) with
  `sort` and missing values now keeps the `NA` row at the end of the
  tabulation so the printed `Cum. Percent` and `Cum. Valid Percent`
  columns stay monotonic and match the Valid → Missing → Total display
  layout. Sorting previously could push the `NA` row between valid rows
  and make cumulative percentages appear to jump.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now preserves literal `"NA"` and empty-string values in the `Values`
  summary instead of removing them as if they were missing values.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now distinguishes actual `NA` values from `NaN` in the `Values`
  summary when `include_na = TRUE`.

- `varlist(values = TRUE)` now preserves factor level order in the
  `Values` summary, matching the default compact factor display.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now validates `values`, `tbl`, and `include_na` up front and gives a
  clear error when one of them is not `TRUE` or `FALSE`.

## spicy 0.8.0

CRAN release: 2026-04-10

### New features

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  adds APA-style bivariate linear-model tables for continuous outcomes.
  It acts as the model-based companion to
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  for reporting fitted mean comparisons or slopes in an `lm` framework,
  with one predictor per model, model-based means for categorical
  predictors, optional case weights, classical or HC0-HC5 variance
  estimators, multiple output formats (ASCII, tinytable, gt, flextable,
  Excel, clipboard, and Word), `output = "data.frame"` for the wide raw
  table, `output = "long"` for the analytic long table, and configurable
  display of tests, confidence intervals, fit statistics, and effect
  sizes.

### Minor improvements

- Installed package vignettes now avoid embedding heavy HTML table and
  codebook widgets during CRAN builds, reducing package size while
  preserving rich pkgdown article rendering.

- Website and vignette coverage now includes
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  using the bundled `sochealth` data throughout and adding a dedicated
  article for model-based continuous summary tables.

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  and
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  now support dedicated display precision for effect-size columns, and
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  also supports separate precision for `R²` columns, so model fit and
  effect sizes can be formatted independently from descriptive values
  and test statistics.

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  now keeps `n` as the unweighted analytic sample size in wide and
  rendered outputs, and can optionally add a separate `Weighted n`
  column reporting the sum of case weights.

## spicy 0.7.0

CRAN release: 2026-03-30

### New features

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  is a new helper for continuous summary tables. It computes descriptive
  statistics (mean, SD, min, max, confidence interval of the mean,
  and n) for numeric variables, with tidyselect column selection,
  optional grouping via `by`, and multiple output formats (ASCII,
  tinytable, gt, flextable, Excel, clipboard, and Word).

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  gains `effect_size` and `effect_size_ci` arguments. When `by` is used,
  `effect_size = TRUE` adds an “ES” column with the appropriate measure
  (Hedges’ g, eta-squared, rank-biserial `r_rb`, or epsilon-squared)
  chosen automatically based on the test method and number of groups,
  and `effect_size_ci = TRUE` appends the confidence interval in
  brackets.

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  gains a `test` argument (`"welch"`, `"student"`, or `"nonparametric"`)
  to choose the group-comparison method, along with independent
  `p_value` and `statistic` display toggles so users can request either
  or both outputs when `by` is used.

- ASCII console tables now split oversized outputs into stacked
  horizontal panels, repeating the left-most identifier columns so wide
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  and
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  prints stay readable in narrow consoles.

### Breaking changes

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  replaces `table_apa()` as the public helper for categorical summary
  tables. It uses `select` and `by`, supports grouped cross-tabulation
  or one-way frequency-style tables when `by = NULL`, and consolidates
  output formats under a single `output` argument. Migrate existing
  `table_apa()` calls to
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  use `output = "default"` for ASCII tables and `output = "data.frame"`
  for plain data frames, and replace former `output = "wide"` /
  `style = "report"` paths with the formatted output engines.

- Excel export now uses `openxlsx2` instead of `openxlsx` for a lighter
  dependency footprint (no Rcpp compilation required).

### Minor improvements

- Package citation metadata now uses the current package title and CRAN
  DOI, so `citation("spicy")` matches `DESCRIPTION` and points to the
  package DOI.

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  and
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  now print shorter ASCII titles without appending the input data frame
  name, and no longer require `officer` for `output = "flextable"`
  alone; `officer` is now required only for Word export paths that
  actually write `.docx` files.

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  now accepts tidyselect syntax in `exclude` in addition to character
  vectors, and no longer warns that `test` is ignored when it is still
  needed to compute effect sizes.

## spicy 0.6.0

CRAN release: 2026-03-23

### New features

- New family of association measure functions for contingency tables:
  [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md),
  [`contingency_coef()`](https://amaltawfik.github.io/spicy/reference/contingency_coef.md),
  [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
  [`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md),
  [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
  [`kendall_tau_c()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_c.md),
  [`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md),
  [`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md),
  [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md),
  [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md),
  and
  [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md).
  Each returns a numeric scalar by default; pass `detail = TRUE` for a
  named vector with estimate, confidence interval, and p-value.

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  gains `assoc_measure` and `assoc_ci` arguments. When both variables
  are ordered factors, it automatically selects Kendall’s Tau-b instead
  of Cramer’s V. The note format changes from `Chi-2: 18.0 (df = 4)` to
  `Chi-2(4) = 18.0`. Numeric attributes (`chi2`, `df`, `p_value`,
  `assoc_measure`, `assoc_value`, `assoc_result`) are now attached to
  the output data frame.

- `table_apa()` now dynamically labels the association measure column
  based on the measure used, instead of always showing “Cramer’s V”. New
  `assoc_measure` and `assoc_ci` arguments are passed through to
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).

- `table_apa()` gains `output = "gt"` to produce a `gt_tbl` object with
  APA-style formatting, column spanners, and alignment.

- `table_apa()` now correctly centers spanner labels over their column
  pairs in `tinytable` and `flextable` output.

- All association measure functions and
  [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
  gain a `digits` argument (default 3) that controls the number of
  decimal places when printed. The p-value always uses 3 decimal places
  or `< 0.001`.

- `detail = TRUE` results now print with formatted output (aligned
  columns, fixed decimal places) via a new
  [`print.spicy_assoc_detail()`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_detail.md)
  method.
  [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
  output uses a new
  [`print.spicy_assoc_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_table.md)
  method with the same formatting.

- New bundled dataset `sochealth`: a simulated social-health survey (n =
  1200, 24 variables) with variable labels, ordered factors, survey
  weights, and missing values. Includes four Likert-scaled life
  satisfaction items (`life_sat_health`, `life_sat_work`,
  `life_sat_relationships`, `life_sat_standard`) for demonstrating
  [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md),
  and
  [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md).

### Bug fixes

- [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  now correctly counts `NA` values when `count = NA` and `strict = TRUE`
  are both used. List columns are now reported in verbose mode instead
  of causing silent errors.

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  rescale logic now operates on complete cases only, so the weighted
  total N matches the unweighted N when missing values are present
  (consistent with Stata behavior).

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  uses true `NA` consistently (instead of the `"<NA>"` string) in both
  weighted and unweighted paths. `cum_valid_prop` is now correctly `NA`
  for missing rows. Invalid `digits` and `sort` values are rejected with
  clear error messages.

- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md)
  and [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)
  now validate `min_valid` and `digits` arguments, rejecting
  non-numeric, negative, or multi-element values.

- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md),
  and
  [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  no longer trigger a tidyselect deprecation warning when `select`
  receives a character vector. Character vectors are now automatically
  wrapped with
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html).

- `table_apa()` now preserves the original factor level order in row
  variables instead of sorting alphabetically. When `drop_na = FALSE`,
  the `(Missing)` category is placed at the bottom of each variable’s
  levels. `percent_digits`, `p_digits`, and `v_digits` are now
  validated.

- `table_apa()` p-values no longer wrap across lines in `tinytable` HTML
  output.

### Breaking changes

- [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
  now accepts a `detail` argument. By default it returns a numeric
  scalar (as before). Pass `detail = TRUE` to get a 4-element named
  vector (`estimate`, `ci_lower`, `ci_upper`, `p_value`), or
  `detail = TRUE, conf_level = NULL` for a 2-element vector (`estimate`,
  `p_value`) without CI.

## spicy 0.5.0

CRAN release: 2026-03-14

### New features

- New `table_apa()` helper to build APA-ready cross-tab reports with
  multiple output formats (`wide`, `long`, `tinytable`, `flextable`,
  `excel`, `clipboard`, `word`).
- `table_apa()` exposes key
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  controls for weighting and inference (`weights`, `rescale`, `correct`,
  `simulate_p`, `simulate_B`) and now handles missing values explicitly
  when `drop_na = FALSE`.

### Bug fixes

- [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  no longer crashes when `special = "NaN"` is used with non-numeric
  columns. Passing `count = NA` now errors with a message directing to
  `special = "NA"`.
- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  fixes a spurious rescale warning for explicit all-ones weights and
  aligns the Cramer’s V formula with
  [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md).
- `table_apa()` no longer leaks global options on error. The
  `simulate_p` default is aligned to `FALSE`.
- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  title generation no longer crashes on unrecognizable expressions.

### Minor improvements

- [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md)
  parameter `message` renamed to `show_message`.
- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  dispatches printing correctly via S3.
- Removed unused `collapse` and `stringi` from `Imports`.

## spicy 0.4.2

CRAN release: 2026-03-06

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  hardening: improved vector-mode detection (including labelled
  vectors), stricter weight validation, safer rescaling, and clearer
  early errors (e.g., explicit `y = NULL`).
- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  statistics are now computed on non-empty margins in grouped tables,
  avoiding spurious `NA` results; internal core path refactored to
  remove `dplyr`/`tibble` from computation while preserving user-facing
  behavior.
- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  errors clearly when `x` is missing for data.frame input and validates
  rescaling when weight sums are zero/non-finite.
- [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md),
  [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  and [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)
  regex mode is hardened (`regex = TRUE` now validates/defaults `select`
  safely).
- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md)
  and [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)
  now return `NA` (with warning) when no numeric columns are selected.
- [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  now validates input type (`data.frame`/tibble required).
- [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
  now returns `NA` with warning for degenerate tables.
- Dependency optimization: `DT` and `clipr` moved to `Suggests`;
  optional runtime checks added in
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  and
  [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md).
- Tests expanded with regression coverage for all the above edge cases.

## spicy 0.4.1

CRAN release: 2025-12-21

- Fixed CRAN incoming check notes by removing non-standard top-level
  files.

## spicy 0.4.0

- Print methods have been fully redesigned to produce clean, aligned
  ASCII tables inspired by Stata’s layout. The new implementation
  improves formatting, adds optional color support, and provides more
  consistent handling of totals and column spacing.

- Output from
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  now benefits from the enhanced `print.spicy()` formatting, offering
  clearer, more readable summary tables.

- Documentation and internal tests were updated for clarity and
  consistency.

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  gains an explicit `correct` argument to control the use of Yates’
  continuity correction for Chi-squared tests in 2x2 tables. The default
  behavior remains unchanged.

- The documentation of
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  was refined and harmonized, with a clearer high-level description,
  improved parameter wording, and expanded examples.

- Minor cosmetic improvements were made to
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  output: the title prefix now uses `vl:` instead of `VARLIST`, and the
  column name `Ndist_val` was renamed to `N_distinct` for improved
  readability and consistency.

- Minor cosmetic improvement: ASCII table output no longer includes a
  closing bottom rule by default.

## spicy 0.3.0

CRAN release: 2025-10-22

- New function
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md),
  which generates a comprehensive variable codebook that can be viewed
  interactively and exported to multiple formats (copy, print, CSV,
  Excel, PDF).

## spicy 0.2.1

CRAN release: 2025-10-04

- [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  now correctly handles edge cases when the separator appears in the
  label or is missing.

## spicy 0.2.0

CRAN release: 2025-09-25

- New function
  [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  to derive and assign variable labels from headers of the form
  `"name<sep>label"` (e.g. `"name. label"`). Especially useful for
  LimeSurvey CSV exports (*Export results* -\> *CSV* -\> *Headings:
  Question code & question text*), where the default separator is
  `". "`.

## spicy 0.1.0

CRAN release: 2025-05-05

### Initial release

- Introduces a collection of tools for variable inspection, descriptive
  summaries, and data exploration.
- Provides functions to:
  - Extract variable metadata and display compact summaries
    ([`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)).
  - Compute frequency tables
    ([`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)),
    cross-tabulations
    ([`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)),
    and Cramer’s V for categorical associations
    ([`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)).
  - Generate descriptive statistics such as means
    ([`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md)),
    sums
    ([`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)),
    and counts
    ([`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md))
    with automatic handling of missing data.
  - Copy data
    ([`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md))
    directly to the clipboard for quick export.
