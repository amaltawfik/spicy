# spicy (development version)

This is the largest release the package has had. `table_regression()`
grows from `lm` / `glm` to more than thirty model classes; the summary
tables gain survey-design twins, a univariable screen, and an
outcome-first layout; six journal styles and a French output arrive with
document-wide options; declared missing values are honored package-wide;
and a long audit aligned every rendered output with the console,
cell for cell. The walk-throughs live as articles at
<https://amaltawfik.github.io/spicy/>.

## Breaking changes

* Declared missing values (`na_values`, `na_range`, tagged NAs) now
  count as missing in `freq()`, `cross_tab()`, the `table_*()` family,
  `mean_n()` / `sum_n()` / `count_n()`, and `varlist()` / `vl()` /
  `code_book()`, so numbers change for labelled survey data. The
  tabulating helpers disclose the exclusion in a note, and `freq()`
  keeps a labelled Missing row per code. `user_na = FALSE` restores the
  previous behavior. See `?freq`.
* `varlist()`, `vl()`, and `code_book()` use one missing definition for
  `N_distinct`, `N_valid`, and `NAs` on labelled data, and observed
  `na_range` codes and tagged-NA labels appear in `Values`.
* `cross_tab()` counts observations at an explicit `NA` factor level
  (`addNA()`, `factor(exclude = NULL)`) as a regular row or column:
  totals, percentages, and the chi-squared statistic include them.
  `freq()` excludes such observations from `n_valid` and the
  valid-percent denominator.
* `freq()` and `cross_tab()` replace `styled` with `output`:
  `styled = TRUE` is `output = "default"`, `styled = FALSE` is
  `output = "data.frame"`, and `styled` now errors with the replacement.
* `cross_tab(output = "data.frame")` returns a plain `data.frame` (a
  list of them with `by`) without metadata attributes. Read them from
  the default object, e.g. `attr(cross_tab(...), "p_value")`.
* `freq()` defaults to `rescale = FALSE` (raw weighted counts), matching
  `cross_tab()`, and reads `options(spicy.rescale)` the same way. Use
  `rescale = TRUE` for the previous behavior.
* `freq()`, `table_categorical()`, `table_continuous()`, and
  `table_continuous_lm()` no longer print when their result is assigned:
  a bare call still shows the table. `freq()`'s unused `...` is removed,
  so unknown arguments error.
* `options(OutDec)` no longer changes spicy's output: every number
  follows `decimal_mark` alone (default `"."`). A session that relied on
  `OutDec = ","` must now ask for the comma — `decimal_mark`, a style,
  or `options(spicy.language = "fr")`.
* `table_categorical()` defaults to `drop_na = FALSE`: missing values
  display as a `"(Missing)"` level. With `drop_na = TRUE`, a note
  reports what was removed.
* `table_categorical()` takes `labels` as a named character vector,
  falling back to the label attribute then the column name; unnamed
  positional vectors error. It also rejects `p_digits` below 1, which
  used to be rendered silently with 3 decimals.
* `table_categorical(output = "long")` always names the association
  column `effect_size` and adds `effect_size_type`; the column used to
  be named after the measure. Replace `out[["Cramer's V"]]` with
  `out$effect_size`.
* `table_categorical(output = "flextable")` no longer writes a `.docx`
  when `word_path` is supplied; the combination warns. Use
  `flextable::save_as_docx()`.
* `table_continuous_lm(output = "data.frame")` names the effect-size
  interval bounds `es_ci_lower` / `es_ci_upper`, matching the `"long"`
  output.
* `standardized = "smart"` scales continuous inputs by 2 SD and leaves
  binary inputs unscaled. The rule was applied inverted since 0.12.0,
  halving every continuous "smart" beta; those betas change.
* `table_regression(exponentiate = TRUE)` errors on links whose
  exponentiated coefficient is not a ratio (probit, cauchit, inverse,
  sqrt); identity links keep the warn-and-skip.
* `keep` / `drop` no longer match the intercept row: the patterns select
  predictors, and `show_intercept` alone controls the intercept.
* `align = "auto"` is removed from the `table_*()` functions; use
  `"decimal"` (the default), `"center"`, or `"right"`.
* The `show_fit_stats` information criteria are lowercase tokens
  (`"aic"`, `"aicc"`, `"bic"`); uppercase errors with the replacement,
  and `show_fit_stats = character(0)` errors — use `FALSE`.
* With several models, `show_columns = "all_b"` / `"all_ame"`
  auto-compact (CIs dropped); request atomic tokens to keep them.
* A robust `vcov` that cannot be computed is now an error instead of a
  warning plus the classical variance labelled robust, and a `cluster`
  containing `NA` is refused for every cluster-robust estimator. Use
  `vcov = "classical"` to ask for the model-based variance.
* `Weighted n` and `glance()`'s `weighted_nobs` are `NA` for an
  unweighted `glm()`; they used to repeat `n`, so the observed count of
  an unweighted model could read as a population.
* `as_structured()` describes each row in the body itself:
  `reference_rows` becomes `cell_status` (marking the reference cell),
  `factor_header_rows` / `fit_stat_rows` / `outcome_row` become
  `body$.row_role`, and `level_rows` becomes `body$.indent > 0`. The
  0.12.0 index vectors are removed, `version` is `3`, and a table built
  by an older spicy is refused rather than mis-read.
* `tidy()` labels AME rows `estimate_type = "ame"` (was `"AME"`), and
  the SE footer reads `"classical (Fisher information)"`.
* `count_n()` warns and returns `NA` for all rows when the selection
  resolves to zero usable columns; `mean_n()` and `sum_n()` with
  `min_valid = 0` return `NA` for rows with no valid values (was `NaN`
  and a silent `0`).
* `copy_clipboard()` arguments use snake_case (`row_names_as_col`,
  `row_names`, `col_names`); the old dot.case names error.
  `build_ascii_table()` is no longer exported — use
  `spicy_print_table()` — and the inert `column_total_line` is removed.
* Association measures with `detail = TRUE` always include an `se`
  element; the internal `.include_se` argument is removed. On degenerate
  tables, `gamma_gk()`, `kendall_tau_b()`, and `kendall_tau_c()` give an
  `NA` p-value when the asymptotic SE is zero, and `uncertainty_coef()`
  and `kendall_tau_c()` return `NA` with a classed warning at zero
  entropy or on a constant variable. `conf_level` is validated
  everywhere (`conf_level = 95` hints at `0.95`).
* The package ships a single vignette, *Get started*; the walk-throughs
  live as articles on the package site at the same URLs.
  `vignette("<name>")` no longer finds them from an installed package —
  read them at <https://amaltawfik.github.io/spicy/>, where every help
  page now links.

## New supported models

`table_regression()` grows from `lm` / `glm` to more than thirty model
classes, each rendered with the conventions of its family:
`?table_regression_models` is the registry, the site's articles are the
walk-throughs. A request a class cannot honor is refused with a classed
error, never rendered as an empty column, and every class fills the
fit-statistics block with what it has (`nobs` and `AIC` at minimum,
class-aware defaults such as pseudo-R² for ordinal and multinomial
fits).

* Mixed effects (`lmer` / `glmer`, `glmmTMB`, `lme`, `gls`): random
  effects as a block of rows (SD, correlations, residual, each with SE
  and CI), ICC, per-group N, marginal / conditional R², and a
  boundary-correct LR test of the random part. `re_ci = "profile"` gives
  profile CIs on the variance components, `re_test = "lrt"` / `"rlrt"`
  fills their test columns, and variance-component SEs are omitted above
  `options("spicy.re_se_max_n")` (default 1000).
* Mixed-effects guards: singular (boundary) structures are noted on all
  three engines, a non-converged `glmmTMB` fit withholds its
  likelihood-derived fit statistics under a note and a classed warning,
  a fit with several grouping factors explains why no single ICC exists,
  and nested `lme` levels are named as `lmer` names them, so the two
  engines line up in one table.
* GEE fits (`geeglm()`): the native sandwich SEs are the inference, the
  footer names the working correlation, fit statistics report the
  cluster structure (`"qic"` / `"qicu"` / `"scale"` opt-in), and AME and
  `exponentiate` work as for `glm`.
* Bayesian (`stan_glm()`, `stan_glmer()`, `brm()`): posterior median,
  MAD SD, and credible intervals (`ci_method = "hdi"` opt-in); no
  p-values, probability of direction opt-in; `R² (Bayes)` by default,
  `"elpd_loo"` / `"looic"` / `"waic"` opt-in; `exponentiate` and AME
  work on the draws, and multilevel fits get a `Random effects (MCMC)`
  block. A sampler-diagnostics guard (R-hat, ESS, divergences, E-BFMI)
  footnotes and warns on failures; `p_adjust`, likelihood-based fit
  statistics, and variational / optimizing fits are refused; a mixed
  frequentist-Bayesian table keeps the shared CI label and dashes the
  Bayesian p cells.
* Survival (`coxph` / `survreg`, `cph`, `flexsurvreg`): Cox tables
  report `n` and `N events` as fit statistics and the concordance as a
  footer note.
* Categorical (`multinom`, `mlogit`): a single `multinom` renders its
  outcome categories as column groups (`outcome_labels` relabels them),
  and `mlogit` uses a two-segment alternative-specific layout.
* Ordinal (`polr`, `clm`): thresholds as a labelled block
  (`show_thresholds = FALSE` to opt out) that follows a cluster-robust
  `vcov`; partial-proportional-odds and `clm` scale coefficients get
  their own blocks; aliased predictors and intercept-only fits render
  like their `lm` / `glm` counterparts.
* Two-part counts (`zeroinfl` / `hurdle`, `glmmTMB`): the zero,
  zero-inflation, and dispersion components render as labelled row
  blocks (`show_components = FALSE` to opt out), exponentiated only
  when the link yields a ratio. Dispersion tokens `"theta"` / `"alpha"`
  (`glm.nb`) and `"phi"` (`betareg`) are opt-in fit statistics.
* `fixest` (`feols`, `feglm`, `fepois`, `fenegbin`): absorbed fixed
  effects show as a `Fixed effects:` Yes / No block, with the within R²
  for `feols` and McFadden's pseudo-R² for the glm engines; the
  `"n_groups"` token counts groups for absorbed and random effects
  alike.
* Robust / IV / quantile (`estimatr`, `ivreg`, `tobit`, `rq`); plus
  `rlm`, `glm.nb`, `nls`, `gam` / `bam`, `betareg`, `selection`, and
  `ols` / `lrm` / `Glm`.
* Design-based fits (`survey::svyglm()`, replicate designs included):
  design-based SEs named in the footer (Taylor linearization, or the
  replicate scheme), Wald t at the design's residual degrees of freedom
  — average marginal effects included, averaged over the population the
  design describes — both counts (`n` and `Weighted n`), the sampling
  design named in the note, survey's design-based `AIC`
  (`show_fit_stats = "eff_p"` opt-in), and likelihood statistics absent
  rather than approximated.
* Design-weighted ordinal fits (`survey::svyolr()`): thresholds,
  per-category AME over the population, the design's residual degrees
  of freedom on every row, and `survey::regTermTest()` as the omnibus
  test.
* Design-weighted Cox fits (`survey::svycoxph()`): hazard ratios, both
  counts, concordance, and the design's degrees of freedom; the RMST
  and risk-difference columns are refused — their bootstrap ignores the
  design — with a pointer at `survey::svykm()`.

## New functions

* `table_continuous_svy()` and `table_categorical_svy()` summarize a
  `survey` design object — the design twins of `table_continuous()` and
  `table_categorical()`, with the same layout, tokens, and engines.
  Every statistic is computed by survey (means, SDs, quantiles, design
  effects, `svyciprop()` intervals, Rao-Scott chi-squared), intervals
  and tests use the design degrees of freedom, `by` gives each group
  its own domain, and the note names the design and both counts. A
  calibrated design with negative weights is disclosed and its group
  comparisons withheld; `chisq_statistic = "saddlepoint"` is refused on
  replicate designs; and handing a design to the data-frame functions
  errors with the function to call instead.
* `table_outcome()` summarizes one continuous outcome across several
  categorical variables, stacked as blocks — the transpose of
  `table_continuous()`, with the same `show_columns` tokens and
  engines. Each block carries its group comparison, and an `Overall`
  row gives the marginal summary.
* `table_regression_uv()`: univariable screening tables — one fit per
  predictor, merged side by side with the multivariable model — for
  `lm`, `glm` (any `family`), and `coxph` outcomes. A per-predictor `N`
  column shows by default (a note flags differing Ns,
  `complete_cases = TRUE` forces the common sample), a binary-looking
  outcome proceeds as a linear probability model under a classed
  warning, univariable intercepts are hidden (`show_intercept = TRUE`),
  and `p_adjust`, `exponentiate`, `vcov` / `cluster`, `labels`, the
  engines, and `tidy()` work as in `table_regression()`. Linear screens
  add `show_columns = "r2"` / `"adj_r2"` per predictor.
* `inline()` cites one table cell in running Quarto / R Markdown text:
  the returned string is exactly the displayed cell — decimals, *p*
  style, interval punctuation, journal style — so a quoted number can
  never drift from the table. Rows are addressed by variable / level
  identity, columns by their typed token, `{token}` patterns build full
  fragments (`"{b} ({ci_label} {ci}; p {p})"`), and every misaddressing
  errors with the available choices.
* `spicy_style()` builds a table style by hand or from a named theme
  (`spicy_style("lancet", ci_sep = " to ")`), every lever validated;
  `?spicy_style` lists, for each theme, the exact rules it encodes and
  the official document they come from.
* `spicy_labels()` returns every table label with the key that names
  it, in the language in force; the keys are what `options(spicy.labels)`
  takes.
* `table_regression_models()` returns the machine-readable registry of
  supported classes (family, engine, AME, `exponentiate` semantics);
  its help page is the per-family reference.

## New features

### Journal styles and languages

* New `style` argument on the four table families, and
  `options(spicy.style = )` for document-wide scope: `"jama"`,
  `"nejm"`, `"lancet"`, `"annals"`, `"apa"`, and `"aer"`. A theme
  encodes only rules taken from an official document of the journal —
  numeric formatting conformity, not full editorial conformity — and
  moves defaults only: any formatting argument you pass wins, even at
  its own default value. `decimal_mark` accepts any single character in
  every family, which is what lets `"lancet"` set its midline dot.
* `options(spicy.language = "fr")` prints table labels in French —
  headers, row labels, titles, notes — and brings French typography
  with it: the decimal comma and the leading zero French usage keeps on
  a p-value (`0,003`). A label the French set does not carry falls back
  to English, and `table_regression()` titles translate whole or not at
  all. `options(spicy.labels = )` overrides one label at a time, in any
  language. A language changes only what a reader sees — machine
  outputs, column names, errors, and warnings stay put — and composes
  with a style: where the two meet the theme wins (`"lancet"` keeps its
  midline point), and an explicit argument beats both.

### Summary tables

* `table_categorical()` and `table_continuous()` gain `smd = TRUE`: a
  standardized-mean-difference column, the balance diagnostic of a
  Table 1 — signed for two groups, a multivariate distance for more, no
  CI and no p-value, working under `weights`. The raw outputs and
  `glance()` carry `smd_type` / `smd_value` whatever the argument, and
  the weighted-comparison refusal now points at it.
* `table_continuous()` gains `weights` and `rescale`: weighted mean,
  SD, quantiles, and mean CI under a documented convention (integer
  weights reproduce row-expanded data exactly; `rescale = TRUE` matches
  Stata's `[aweight]` and `survey::svyvar()`), a `"weighted_n"` token,
  and the weights named in the note. Group tests and the median CI are
  deliberately refused under weights — `table_continuous_lm()` is the
  weighted-comparison tool.
* `table_continuous()` gains `show_columns`: `"med"`, `"q1"`, `"q3"`,
  `"iqr"`, `"med_iqr"` (`Med [Q1, Q3]`), and `"med_ci"` (exact
  order-statistic CI) alongside the defaults, per variable via a named
  list. A variable shown as a median is tested as one — Wilcoxon /
  Kruskal-Wallis with the matching rank effect size — and the note says
  which test each variable carries.
* `select` is optional in `table_categorical()` (every eligible
  categorical column, `by` excluded); `table_continuous()` gains
  `drop_na = FALSE` (`"(Missing)"` group, tests on the observed
  groups); both read `options(spicy.rescale)`.

### Regression tables

* New `show_columns` families `"rmst"` and `"risk_diff"` for `coxph`
  and `survreg` fits: covariate-adjusted differences in restricted mean
  survival time over `[0, tau]` and in cumulative incidence at
  `at_time`, by g-computation with bootstrap inference — in single
  tables and in the univariable screen (one shared horizon there). The
  horizon is explicit and required (`tau = "minmax"` opt-in), stratified
  Cox fits keep each subject's own stratum baseline, and the baseline
  hazard follows the fit's tie handling.
* New `show_columns` token `"n_events"`: event counts as `events/N`
  beside the estimates — per factor level, model totals on continuous
  rows — for binomial outcomes and right-censored `coxph` fits.
* Heteroskedasticity- and cluster-robust `vcov` across the supported
  classes, with each class's field-standard backend: `"CR1S"`
  reproduces Stata's `regress, vce(cluster)` exactly, `multinom` gets
  `"CR0"`–`"CR3"` (AME included), `rq` gets quantreg's own family
  (`"nid"` default, `"rank"`, native `"bootstrap"`, wild gradient
  cluster bootstrap), and resampling footers report the valid replicate
  count. What no backend supports is refused, never approximated.
* `ci_method = "profile"` gives profile-likelihood CIs for `glm`,
  `polr`, and `clm`; `ci_method = "boot_percentile"` reports percentile
  CIs from the same replicates as the bootstrap SEs.
* `nested = TRUE` works across the expansion with the correct test per
  class — LR chi-square for `multinom` and Cox, a Wald-type F over
  `rq` fits at one tau — and refuses what is not comparable (REML pairs
  with different fixed effects, cross-engine mixed hierarchies, mixed
  taus or classes).
* AME columns for many more classes, per outcome category for `polr` /
  `clm` / `multinom`, honoring a robust `vcov`; `broom::tidy()` gains
  `outcome_level` for the per-category rows. When a `β` column shows,
  the note names the standardization method and its conventions.

### The structured view

* `as_structured()` reads the descriptive tables too, and carries
  everything the printed table shows: significance markers and their
  cutoffs, composed display cells (`events/N`), the absorbed
  fixed-effects block, and a per-row identity that survives stacking —
  `.variable`, `.level`, `.row_role`, `.indent`, with `cell_status`
  naming reference and undefined cells — so a renderer never reads an
  en dash to find out what a cell is. A `version` field names the
  contract; a view from a newer contract is refused instead of
  mis-read.
* Table notes on the `"tinytable"` engine are set one size down, like
  the other engines, and `options(spicy.note_style)` passes Typst
  styling through (`"none"` leaves the note to the document template).

### Documentation

* Seven new articles: mixed-effects, GEE, multinomial, count and
  two-part, survival, ordinal regression tables, and categorical
  predictors (dummy coding, reference levels, joint tests, contrast
  codings).

## Bug fixes

### Corrected results

These fixes change numbers that 0.12.0 reported.

* `kendall_tau_b()` reported wrong standard errors, confidence
  intervals, and Wald p-values in every release from 0.6.0 through
  0.12.0 — the asymptotic SE mis-scaled one margin term; point
  estimates were correct. Also via `assoc_measures()` and the
  `cross_tab()` association line.
* Binomial models fitted with a two-column `cbind(successes, failures)`
  response were refitted with effectively squared weights by every
  internal resampling refit. Three surfaces were wrong: bootstrap /
  jackknife SEs, CIs, and p-values (errors up to 30% in our checks);
  the default `R² (McFadden)` / `R² (Nagelkerke)` (0.92 where the true
  value was 0.32); and `standardized = "refit"` on a pre-built matrix
  column. Fits with a 0/1, factor, or proportion-plus-weights response
  were never affected.
* Average marginal effects use the fit's prior weights: for a weighted
  `lm` / `glm` fit, the AME and its inference are the weighted average
  of the unit-level effects, so AME values change for weighted fits.
* Partial effect sizes (`partial_f2`, `partial_eta2`, `partial_omega2`,
  the `glm` `partial_chi2`) are true Type-II tests: in models with
  interactions, main effects no longer depend on the factor coding.
  `partial_chi2` is also correct for `glm` fits created with `y = FALSE`
  and a matrix response, where the internal refit double-counted the
  binomial totals.
* `ci_method = "profile"` with a robust `vcov` defers to the `vcov` and
  warns, instead of silently pairing profile CIs with robust SEs.
* `table_categorical()` computes `tau_b`, `tau_c`, `gamma`, and
  `somers_d` in declared level order under `drop_na = FALSE`; the
  values were wrong whenever that order differed from alphabetical.
* `table_continuous_lm()` reports correct estimates when `by` is an
  ordered factor, computes correct `"balanced"` adjusted means with an
  ordered-factor covariate, weights `"proportional"` predictions by the
  case weights, reports the omega-squared interval, and pins treatment
  contrasts so session-wide `options(contrasts = )` no longer alters
  the results.
* `cross_tab()` computes weighted count totals (`percent = "none"`)
  from the unrounded table, and titles fall back to a neutral
  placeholder instead of a value plucked from an inline expression.

### Tabulation and association measures

* Under `decimal_mark = ","`, every surface follows the mark: p-values
  and bounded measures keep their leading zero (`0,018`, `<0,001`),
  `cross_tab()`'s expected-count note, the significance-star legend
  (`*** p < 0,001`), and the nested change statistics (`+0,07`) read
  the comma, and an association interval separates its bounds with `;`
  (`0,45 [0,31; 0,59]`). `as_structured()` reports the matching
  `p_style`; one you ask for still wins.
* `nested = TRUE` no longer reports a negative chi-square with a
  p-value when the models are passed largest-first.
* `somers_d(direction = "symmetric")` returns `0` on equal concordant
  and discordant pairs; `cramer_v()`, `phi()`, and `contingency_coef()`
  return `NA` with a classed warning on a zero margin — and
  `assoc_measures()` / `cross_tab()` no longer swallow those warnings,
  nor the classed error of a measure that does not apply (`phi` on a
  3x2 table).
* The `tau_c` measure is labelled `"Stuart's Tau-c"` everywhere;
  several paths said `"Kendall's Tau-c"`.
* `freq()` keeps its label footer when `NA`-weight rows are dropped,
  `print()` invisibly returns the table object, `labelled_levels =
  "labels"` warns when distinct codes merge under one label,
  `valid = FALSE` drops the Valid columns instead of printing `NA`
  under a `100.0` total, and `sort = "name+"` / `"name-"` sorts
  labelled variables by code whenever codes are displayed.

### Summary tables

* `table_categorical()` displays labelled columns as `"[code] label"`
  levels in every path, keeps a declared-but-unobserved `by` level as
  an explicit zero column, and keeps both the group and the margin when
  a `by` level is named `"Total"`.
* `table_categorical()` machine outputs carry full-precision values in
  grouped tables, `output = "data.frame"` gains the documented `Chi2`
  and `df` columns, and displayed counts are integers everywhere.
* `table_categorical(correct = TRUE)` warns when Yates' correction is
  ignored on a non-2x2 table, and a `levels_keep` matching nothing
  warns with the available levels instead of dropping the variable.
* `table_categorical()`, `table_continuous()`, and
  `table_continuous_lm()` resolve `by` data-first, like tidyselect: a
  column always wins over a same-named variable in the calling
  environment. A `by` with no level to tabulate is refused with a
  pointer at `drop_na = FALSE`; a variable whose label attribute is
  `NA` falls back to the column name.
* `table_continuous()` forms groups from a non-factor `by` in order of
  first appearance, matching `table_categorical()`, and degrades per
  variable when a test or effect size fails on degenerate data — the
  affected cells become `NA` under a classed warning, the other
  variables keep their results.
* `table_continuous()` and `table_continuous_lm()` label an interval
  with its own coverage: `ci_level = 0.975` reads `97.5% CI`, not
  `98% CI` — including, for `table_continuous_lm()`, the `data.frame`
  column names.
* `table_continuous_lm()` discloses robust and resampling SEs in the
  note, accepts `cluster = ~region`, treats a value-labelled `by` as
  categorical, accepts non-syntactic covariate names and unused factor
  levels, excludes `NA`-weight rows with disclosure, and degrades
  cleanly on degenerate fits (single-level `by` refused, saturated and
  too-sparse fits warned per variable).

### Rendering and output engines

* `output = "gt"` tables keep their note wherever they are rendered —
  saved, converted to HTML, or printed non-interactively — and gt and
  flextable outputs render in Quarto / R Markdown Word, PowerPoint, and
  PDF documents, where they silently disappeared. A new
  `as_flextable()` method returns the underlying flextable.
* `output = "gt"` escapes what it interpolates: a `by` level or model
  name carrying a quote, an angle bracket, or a backslash renders as
  text, and levels differing only in whitespace or punctuation no
  longer collide on one HTML or spanner id.
* `stars = TRUE` marks the coefficients in every output, not just the
  console: gt, tinytable, flextable, Word, and clipboard shipped the
  legend footnote without a single marker in the table.
* Every cell whose statistic applies but has no number — an aliased
  coefficient, a fit statistic undefined for a model's class — shows
  the console's en dash in every rich output; a blank cell was
  indistinguishable from "not requested". `as_structured()` and the
  engines match the console body exactly.
* A `table_categorical(by = )` table carries its association-measure
  note to every output — tinytable, flextable / Word, Excel, and the
  clipboard; it used to reach the console only. The descriptive gt /
  tinytable outputs also draw the console's structure: the title, the
  `Variable` corner label, and the light rule between variable blocks.
* Factor levels are indented once, not twice, in the tinytable,
  flextable / Word, and Excel outputs, and a variable label that starts
  with the indent string is no longer mistaken for a level row.
* A table without a confidence-interval column loses its empty header
  strip (tinytable, flextable / Word) and its blank clipboard line;
  header rules land where the console draws them.
* A multi-line table note keeps one disclosure per line in the
  tinytable HTML and Typst outputs, tinytable notes escape interpolated
  labels in HTML, and tables rendered to Typst no longer force a `5pt`
  column gutter under grouped headers.
* Footer lines cite a model by its displayed label (`Baseline: ...`,
  not `Model 1: ...`), and a flextable / Word header cell stays inside
  its own model when two models share a label.
* `output = "excel"` writes what the console shows: significance stars,
  blank cells (not `#N/A`) on header rows, `align` honored, columns
  sized to their text; under `decimal_mark = ","` the body is written
  pre-formatted, so a sheet never mixes separators.
* `output = "clipboard"` quotes cells RFC 4180-style (a delimiter, a
  quote, or a line break in a cell no longer shifts columns), ships
  plain text instead of Excel formulas, and fails with a clear classed
  error on a system without a clipboard.
* Console layout survives its edge cases: `NA` cells print blank
  without knocking rows out of register, an empty cell no longer
  crashes the `table_categorical()` printer, wide characters (CJK,
  emoji) are measured as displayed, and `clipboard_delim` is validated.
* The flextable outputs of the descriptive tables carry the same
  wrapper as `table_regression()`, so notes and knit-time rendering
  behave identically.

### Regression tables

* `table_regression(m1, m2)` without `list()` errors helpfully (the
  second fit used to bind to `vcov`), an `NA` or colliding model name
  is handled instead of crashing or drawing indistinguishable columns,
  the `p_adjust` footer's family size matches the adjustment performed,
  and multi-model titles keep proper nouns capitalized.
* Factor rows follow `levels()` order instead of alphabetical, the AME
  companion tokens work without the bare `"ame"` token, ordered factors
  with AME columns show a reference row, and stars anchor on B (and
  AME), never on beta.
* Factors fit with non-default contrasts (successive differences,
  sum-to-zero, Helmert, custom matrices) group under their parent
  variable, labelled by the contrast-matrix column names, with no
  invented reference row. Logical and character predictors get the
  grouped layout too.
* The statistic column header follows each model's reference
  distribution (`z` or `t`); it was hardcoded to `t`.
* Bootstrap, jackknife, and `standardized = "refit"` refits no longer
  leak the caller's environment and work on `factor()` / `log()` /
  `poly()` formulas; a failed refit falls back with a warning instead
  of silently changing method.
* `show_fit_stats = "pseudo_r2_tjur"` errors when no model is a
  binomial `glm`; the row was silently dropped. Returns carry the
  documented `outcome` / `model_ids` provenance attributes across
  every output.

### Inspection and row-wise helpers

* `varlist()`, `vl()`, and `code_book()` render `POSIXlt` columns as
  datetimes under `values = TRUE` and show an explicit `NA` factor
  level as `<NA>` instead of dropping it; `label_from_names()` no
  longer blames the split for duplicate names already in the input.
* `count_n()` resolves `select` and `exclude` through the same
  tidyselect path as `mean_n()` / `sum_n()` (`exclude` takes positions
  too), and errors clearly on a zero-length or all-missing `count`, an
  empty `special`, or a typo beside `special = "all"`.
* The tabulating and summarizing functions reject `bit64::integer64`
  input with a classed error naming the fix; such columns were silently
  read as doubles.

## Minor improvements

* A one-way `table_continuous()` no longer draws a rule between its
  single-row blocks in the rendered outputs, matching the console, and
  `table_continuous(by = )` titles the table
  `Descriptive statistics by <label>` everywhere.
* `cross_tab()` reports excluded missing values in the table note — per
  variable, with a deduplicated row total — instead of dropping them
  silently; it accepts logical weights like `freq()` and validates
  `digits` the same way.
* The coverage percentage of an interval header follows `decimal_mark`
  (`97,5% CI`); integer coverages and the default period are unchanged,
  and the frozen descriptive column names keep the period.
* A cell with no number prints an en dash in every table;
  `table_continuous()` and the association printers used `--`.
  Placeholder cells decimal-align in the rendered outputs.
* Wide multi-model tables split into stacked panels more cleanly, and a
  new `qualify_companions` argument in `spicy_print_table()` lets a
  continuation panel name the estimate its `SE` / `p` / CI columns
  belong to (`95% CI (B)`).
* Error messages quote values the same way on every platform, keep
  backslashes intact, and the enum arguments (`output`, `align`,
  `percent`, `direction`, ...) raise classed errors naming the valid
  values. `spicy_print_table()` validates its inputs with classed
  errors.
* `copy_clipboard()` re-emits backend messages and warnings as real R
  conditions and invisibly returns what it sent; the descriptive
  tables announce their clipboard export with a classed message that
  can be muffled.
* Under `exponentiate = TRUE` with a visible SE column, the footer
  states the SE scale (delta method) and that the CI bounds are
  asymmetric.
* `table_continuous()`'s "`test` is ignored" warning states the full
  trigger condition, and `varlist()` annotates `difftime` values with
  their units.

# spicy 0.12.0

## New features

* New `table_regression()`: publication-ready coefficient summary
  for one or more fitted `lm` or `glm` models, side by side. APA
  Manual 7 formatting is the default. Highlights:

  * Robust variance: classical, HC, cluster-robust (CR) with
    Satterthwaite df, bootstrap, jackknife. Per-model `vcov`
    accepted for SE-comparison tables.
  * Standardisation: `refit`, `posthoc`, `basic`, `smart`,
    `pseudo` (the last `glm` only).
  * Average marginal effects (AME) as separate columns; AME
    inference shares the coefficient's variance estimator so B
    and AME are reported on the same inferential footing.
  * Partial effect sizes: f², η², ω² for `lm` (noncentral-F CIs);
    partial χ² for `glm`.
  * GLM response-scale reporting via `exponentiate = TRUE`, with
    family-appropriate labels (OR, IRR, HR, RR, MR, exp(B)) and
    optional profile-likelihood CIs (`ci_method = "profile"`).
  * Multiplicity correction via `p_adjust` (any
    `stats::p.adjust()` method).
  * Hierarchical comparison via `nested = TRUE` (ΔR² / F-change
    for `lm`; LRT for `glm`).
  * Display controls: variable filtering, intercept and factor
    placement, reference-row styles, multi-model labels, stars,
    decimal mark, per-column digits.
  * Outputs: console, `data.frame`, long tibble, `gt`,
    `flextable`, `tinytable`, Excel, Word, clipboard.
    `broom::tidy()` and `broom::glance()` methods supported.

  See `?table_regression` and `vignette("table-regression")`.

* `table_continuous_lm()` gains additive covariate adjustment via
  the new `covariates` argument. Two estimands for the per-group
  adjusted means: `"proportional"` (G-computation, default) and
  `"balanced"` (equal-weight synthetic grid). Under adjustment,
  `f²` and `ω²` become partial effect sizes; `d` and `g` raise an
  explanatory error. The auto-built footer documents the
  covariates and the estimand. See `vignette("table-continuous-lm")`.

* New exported `as_structured()` accessor returns a typed view of
  a `table_regression()` result for programmatic use: raw
  numerics, CI split into `LL` / `UL` columns, and a column-level
  format specification.

## Breaking changes

* `code_book()` no longer silently truncates the export filename
  to 120 characters. Very long titles now surface a clear
  OS-level error. **Migration**: shorten the title or pass an
  explicit `filename =` argument.

## Bug fixes

* `table_categorical()` no longer over-truncates a *p*-value in
  the interval `(10^-p_digits, 0.001)` when `p_digits >= 4`.
  Example: `p = 0.000108` now correctly prints as `".0001"` at
  `p_digits = 4` (was `"<.0001"`).
* `count_n(special = ...)` returns a length-`nrow(data)` zero
  vector when no usable column survives the list-column filter,
  matching the documented contract and the `count = ...` branch
  (was `numeric(0)`, which broke `dplyr::mutate()` pipelines).
* `lambda_gk()` and `goodman_kruskal_tau()` emit
  `spicy_undefined_stat` and return a fully-`NA` result on
  rank-1 contingency tables (constant predicted variable),
  matching the existing pattern in `gamma_gk()`,
  `kendall_tau_b()`, `somers_d()`, and `yule_q()`.
* `cross_tab()` no longer silently overwrites a user's y-variable
  level named `"N"`, `"Total"` or `"Values"`. The conflicting
  reserved column is auto-renamed with a numbered suffix and a
  single `spicy_renamed_column` warning is emitted.
* `broom::glance()` on a `spicy_continuous_lm_table` keeps
  `df.residual` numeric, so Satterthwaite degrees of freedom
  from `vcov = "CR2"` / `"CR3"` are preserved verbatim instead
  of being truncated through `as.integer()`.

## Minor improvements

* Console en-dash alignment: non-numeric placeholders (en-dash,
  "NA") sit at the decimal-mark column instead of the integer-
  part column (APA Manual 7 §7.13). Integer cells in mixed-
  precision columns (`n` row alongside `R²`) keep their right-
  aligned placement.
* `R/` source is byte-pure ASCII (`tools::showNonASCIIfile()`
  reports zero hits package-wide).
* `openxlsx2::wb_add_border()` calls now pass `NULL` on unused
  sides, preventing the default `"thin"` from being applied to
  all four sides of a cell when only one rule is intended.

# spicy 0.11.0

## New features

### `table_continuous_lm()`

* Cluster-robust SEs via `cluster` and four `vcov` choices
  (`"CR0"`–`"CR3"`), dispatched to `clubSandwich` with
  Satterthwaite df (`clubSandwich` in `Suggests`).
* `vcov = "bootstrap"` (nonparametric or cluster) and
  `vcov = "jackknife"` (leave-one-out / leave-one-cluster-out)
  variance estimators in pure base R, controlled by `boot_n`.
* Three new `effect_size` choices alongside `"f2"`: Cohen's
  `"d"`, Hedges' `"g"` (two-group only), Hays' `"omega2"`. New
  `effect_size_ci` adds noncentral *t* / *F* CIs rendered inline
  as `0.18 [0.07, 0.30]`.
* `HC*` estimators delegate to `sandwich::vcovHC()`;
  rank-deficient fits return a clean rank-by-rank covariance.

### Harmonisation across the table family

* Shared reporting vocabulary (`decimal_mark`, `p_digits`,
  `align`, named-`labels`) now spans `cross_tab()`, `freq()` and
  the three `table_*()` helpers, including APA-style p-value
  notation (`<.001` / `.045`, no leading zero).
* `table_categorical()`'s `assoc_measure` accepts a per-variable
  spec. When measures differ across rows the column collapses to
  `"Effect size"` and an APA-style `Note.` line documents the
  per-variable measure; `phi` on a non-2x2 errors.
* All three `table_*()` functions gain `as.data.frame()`,
  `tibble::as_tibble()`, `broom::tidy()` and `broom::glance()`
  methods (`broom` in `Suggests`).

## Quality and robustness

* **Classed conditions.** Errors and warnings now carry stable
  classes (`spicy_error` / `spicy_warning` plus 11 leaf classes
  documented in `?spicy`), so downstream code can dispatch via
  `tryCatch()` / `withCallingHandlers()` instead of matching
  message strings. `rlang (>= 1.1.0)` required.
* **Structured cli messages.** Multi-line errors and warnings
  (vcov fallbacks, bootstrap/jackknife failures, `padding`
  migration, `labels` length mismatch) render as cli bullets.
* **Locale-deterministic ordering.** Sorts in `varlist()`,
  `freq()`, `cross_tab()` and `table_*()` use
  `method = "radix"`. Output is byte-stable across locales and
  platforms, matching Stata / SPSS guarantees.
* **Edge-case hardening.** A new length-guarded sort helper makes
  `varlist()` / `code_book()` / `cross_tab()` / `freq()` survive
  zero-length or all-NA `Date` / `POSIXct` / `character` columns
  and factors with no observed levels.
* **Snapshot-locked rendering.** `tests/testthat/test-snapshots.R`
  pins the exact console output of every spicy print method, so
  any unintended formatting drift surfaces as a PR diff.
* **API stability contract.** `?spicy` documents which exports
  are stable, stabilising or internal. pkgdown reference groups
  exports via four `@family` tags.
* **Cross-software validation.** All 13 association measures
  agree with PSPP 2.0 (`CROSSTABS /STATISTICS=ALL`, 65 / 65
  statistics on four datasets); Cohen's *d* and Hedges' *g*
  noncentral CIs are tested numerically against
  `effectsize::cohens_d()` / `effectsize::hedges_g()`
  (`tolerance = 1e-6`); point-estimate formulas and asymptotic
  standard errors follow `DescTools` (Signorell et al.).

## Improvements

* `cross_tab()` warns when `correct = TRUE` is ignored on a
  non-2x2 sub-table, when `weights` contains `NA`, and notes
  statistics computed on a sub-table after empty rows / columns
  are pruned.
* `cross_tab()` validates `decimal_mark`, `p_digits` and
  `simulate_B` up front; `freq()` validates `decimal_mark` and
  tightens `digits` to a non-negative integer.
* A user category literally named `"N"` or `"Total"` is no longer
  mis-rendered as the totals row in `cross_tab()`.
* `table_continuous_lm(output = "long")` returns `n`, `df1`, `df2`
  as integer columns; `predictor_label` preserved on the
  degenerate-model fallback path.
* `cramer_v()` / `phi()` doc states the CI uses the Fisher
  z-transformation (point estimate and p-value identical to
  `DescTools` / SPSS).
* `uncertainty_coef()` doc states entropy uses `0 log 0 = 0`
  (matching SPSS, PSPP, Stata, Cover & Thomas).

## Bug fixes

* `label_from_names()` raises actionable errors on duplicate or
  empty new column names; trims whitespace and preserves the
  input class.
* `table_continuous_lm(output = "data.frame")` names contrast CI
  columns from `ci_level` (was hardcoded to 95 %).
* The categorical-predictor global Wald *F* degrades to `NA` on
  a singular coefficient covariance submatrix.
* The degenerate-table branch of `cramer_v()`, `yule_q()`,
  `gamma_gk()`, `kendall_tau_b()` and `somers_d()` respects
  `detail`: scalar `NA_real_` by default, fully shaped
  `spicy_assoc_detail` when `detail = TRUE`.
* `uncertainty_coef()` returns a finite estimate (was `NaN`) when
  a marginal is zero.
* `somers_d(direction = "symmetric")` returns the harmonic mean
  of the two asymmetric values, matching SPSS / PSPP `CROSSTABS`.
* `print.spicy_assoc_detail()` / `print.spicy_assoc_table()` use
  APA-strict `<.001` / `.045` notation, matching the rest of the
  package.
* `varlist()` / `code_book()` honour `factor_levels = "all"` for
  `haven_labelled` columns: declared-but-unobserved labels appear
  in the `Values` summary.
* `copy_clipboard()` rejects `row.names.as.col` vectors of length
  ≠ 1 and empty strings; accumulates all messages from
  `clipr::write_clip()` instead of overwriting.
* `mean_n()` / `sum_n()` reject non-integer `min_valid >= 1` and
  `min_valid > ncol`; their `digits` requires a non-negative
  integer.

## Breaking changes

* `table_continuous_lm()` and `table_categorical()` default to
  decimal-point alignment for numeric columns
  (`align = "decimal"`). Pass `align = "auto"` for the previous
  behaviour.
* `build_ascii_table()` / `spicy_print_table()`: `padding`
  switches from a string enum to a non-negative integer.
  Default `2L` (was `+5L`); printed tables are roughly 40 %
  narrower. **Migration**: `"compact" -> 0L`, `"normal" -> 2L`,
  `"wide" -> 4L`.
* `table_categorical(assoc_measure = "auto")` on a 2x2 table
  picks `phi` instead of `cramer_v`. Numeric value unchanged
  (|phi| = V on 2x2); only the column label changes.
* `freq()` drops observations with `NA` weights (with a warning)
  instead of recoding them to zero. Aligns with `cross_tab()`.
* `table_continuous_lm(output = "long")` returns `NA` in
  `es_type` / `es_value` when `effect_size = "none"` (was
  `"f2"`), and renames `sum_w` to `weighted_n`.

# spicy 0.10.0

## New features

* `code_book()` now accepts tidyselect-style variable selectors through `...`, matching `varlist()` and `vl()`.

* `code_book()` gains a `filename` argument for the base name of CSV, Excel, and PDF exports. When `NULL` (the default), the filename is derived from `title` and falls back to `"Codebook"` when needed. Filenames are sanitized to portable ASCII consistently across platforms.

* `varlist()` now summarizes matrix and array columns by their dimensions, and counts valid, missing, and distinct observations by rows.

* `freq()` gains a `factor_levels` argument that mirrors `varlist()` and `code_book()`. With `factor_levels = "all"`, declared-but-unobserved factor and labelled levels appear in the output with `n = 0`, matching SPSS `FREQUENCIES`; the default `"observed"` preserves the previous Stata `tab`-style behavior.

## Improvements

* `varlist()` now displays missing values as `<NA>` and `<NaN>` in the `Values` summary when `include_na = TRUE`, and quotes literal `"NA"`, `"NaN"`, and empty-string values so they cannot be confused with the missing markers.

* `varlist()` now emits a column-named warning and marks the failing cell as `<error: ...>` when a column cannot be summarized, instead of silently writing `"Invalid or unsupported format"`. Remaining columns are unaffected.

* `varlist()` produces more precise Viewer titles for extraction, pipe, and literal `get("name")` expressions, while keeping ambiguous dynamic calls anonymous (`vl: <data>`).

* `code_book()` now rejects partial-match names in `...` (e.g. `val = TRUE`, `tit = "x"`) that would otherwise be silently treated as tidyselect expressions, and surfaces `varlist()` selection errors directly.

* `freq()` now resolves the `weights` argument via tidy-eval, so column references nested in compound expressions (e.g. `weights = if (use_w) col else NULL`) work as expected. Qualified expressions like `weights = df2$w` continue to take precedence over column lookup.

* `freq()` validates `digits`, `sort`, `weights`, and the logical scalar arguments (`valid`, `cum`, `rescale`, `styled`) more strictly at the public boundary, with clearer error messages for non-finite values, `NA`, multi-element inputs, and non-numeric weight vectors.

* `freq()` now documents the interaction of `weights` containing `NA` with `rescale = TRUE` (Stata `pweight` semantics) and the dropping of unused factor / labelled levels (Stata `tab` semantics, with `code_book(factor_levels = "all")` as the schema-style alternative).

## Bug fixes

* `varlist()` now displays labelled values in the same prefixed-label order for compact and `values = TRUE` summaries; previously the compact summary used data order.

* `varlist(values = TRUE)` now deduplicates element types when summarizing list-columns. Previously `list(1L, 2L, "a")` produced `"List(3): character, integer, integer"`; now produces `"List(3): character, integer"`.

* `include_na = TRUE` now correctly appends `<NA>` markers for list-columns in both `varlist()` modes; previously it had no effect on this column type.

* `varlist()` now validates column names up front and gives clearer errors for missing, empty, `NA`, or duplicate names.

* `varlist()` now errors clearly when tidyselect expressions try to rename columns; `...` is for selecting variables, not renaming.

* `freq(data, x, weights = NULL)` now correctly treats the explicit `NULL` as "no weighting" instead of emitting a misleading `"variable 'NULL' not found"` error. Parameterized patterns like `weights = if (use_w) wts else NULL` are now supported.

* `print()` for `spicy_freq_table` no longer crashes when the `var_label` attribute is `NA_character_`, numeric, or multi-element; the `Label:` line is silently skipped for any value that is not a single non-empty string.

* `freq()` no longer surfaces the name of the ignored `data` vector in the printed footer when both `data` and `x` are passed as vectors. The footer now consistently shows the analyzed vector's name.

# spicy 0.9.0

## Breaking changes

* `table_continuous()` now enables inferential output by default when `by` is
  supplied. With a grouping variable, the `p` column from `test` is shown
  automatically (previous default hid it). This aligns the two table helpers:
  `table_continuous()` stays descriptive when `by` is absent, and reports the
  test *p*-value when `by` is supplied, matching `table_continuous_lm()`'s
  inferential default. To preserve the previous behavior, pass
  `p_value = FALSE` explicitly. `statistic` and `effect_size` remain `FALSE`
  by default and must still be enabled consciously.

* `varlist()` now displays observed factor levels by default in `Values`,
  matching its role as a quick inspection of the current data. Use
  `factor_levels = "all"` to display unused factor levels as well, which was
  the previous default behavior and remains the default in `code_book()`.

## Minor improvements

* `code_book()` gains a `factor_levels` argument. It defaults to `"all"` so
  exported codebooks continue to document all declared factor levels,
  including unused levels; use `"observed"` to mirror `varlist()` output.

* `freq()` now prints the `Freq.` column as integers regardless of
  `digits`, which continues to control percentage precision. This matches
  the convention of SPSS, Stata, and SAS `PROC FREQ` for weighted counts
  and keeps the two numeric concepts (discrete counts vs. continuous
  percentages) visually distinct.

* `freq(..., styled = FALSE)` now returns a genuinely plain `data.frame`
  with no `spicy_freq_table` rendering metadata clinging to it, so
  `str()`, `dput()`, and downstream programmatic use see only the
  tabulation columns. The metadata attributes (`digits`, `data_name`,
  `var_name`, `var_label`, `class_name`, `n_total`, `n_valid`,
  `weighted`, `rescaled`, `weight_var`) are now documented in
  `@return` and remain available on the invisibly returned
  `spicy_freq_table` object when `styled = TRUE` (the default).

* `table_continuous_lm()` documentation now clarifies why `p_value = TRUE`
  and `r2 = "r2"` are the defaults, and robust-variance fallback warnings
  are now more explicit when a model matrix is singular.

## Bug fixes

* `freq()` now correctly resolves qualified weight expressions such as
  `weights = other$w` or `weights = other[["w"]]` even when the referenced
  column name also exists in `data`. Previously the bare-name fallback
  could silently pull the weight vector from the wrong data frame when
  column names collided.

* `freq()` with `sort` and missing values now keeps the `NA` row at the
  end of the tabulation so the printed `Cum. Percent` and
  `Cum. Valid Percent` columns stay monotonic and match the
  Valid → Missing → Total display layout. Sorting previously could push
  the `NA` row between valid rows and make cumulative percentages appear
  to jump.

* `varlist()` now preserves literal `"NA"` and empty-string values in the
  `Values` summary instead of removing them as if they were missing values.

* `varlist()` now distinguishes actual `NA` values from `NaN` in the
  `Values` summary when `include_na = TRUE`.

* `varlist(values = TRUE)` now preserves factor level order in the
  `Values` summary, matching the default compact factor display.

* `varlist()` now validates `values`, `tbl`, and `include_na` up front and
  gives a clear error when one of them is not `TRUE` or `FALSE`.

# spicy 0.8.0

## New features

* `table_continuous_lm()` adds APA-style bivariate linear-model tables for continuous outcomes. It acts as the model-based companion to `table_continuous()` for reporting fitted mean comparisons or slopes in an `lm` framework, with one predictor per model, model-based means for categorical predictors, optional case weights, classical or HC0-HC5 variance estimators, multiple output formats (ASCII, tinytable, gt, flextable, Excel, clipboard, and Word), `output = "data.frame"` for the wide raw table, `output = "long"` for the analytic long table, and configurable display of tests, confidence intervals, fit statistics, and effect sizes.

## Minor improvements

* Installed package vignettes now avoid embedding heavy HTML table and codebook widgets during CRAN builds, reducing package size while preserving rich pkgdown article rendering.

* Website and vignette coverage now includes `table_continuous_lm()`, using the bundled `sochealth` data throughout and adding a dedicated article for model-based continuous summary tables.

* `table_continuous()` and `table_continuous_lm()` now support dedicated display precision for effect-size columns, and `table_continuous_lm()` also supports separate precision for `R²` columns, so model fit and effect sizes can be formatted independently from descriptive values and test statistics.

* `table_continuous_lm()` now keeps `n` as the unweighted analytic sample size in wide and rendered outputs, and can optionally add a separate `Weighted n` column reporting the sum of case weights.

# spicy 0.7.0

## New features

* `table_continuous()` is a new helper for continuous summary tables. It computes descriptive statistics (mean, SD, min, max, confidence interval of the mean, and n) for numeric variables, with tidyselect column selection, optional grouping via `by`, and multiple output formats (ASCII, tinytable, gt, flextable, Excel, clipboard, and Word).

* `table_continuous()` gains `effect_size` and `effect_size_ci` arguments. When `by` is used, `effect_size = TRUE` adds an "ES" column with the appropriate measure (Hedges' g, eta-squared, rank-biserial `r_rb`, or epsilon-squared) chosen automatically based on the test method and number of groups, and `effect_size_ci = TRUE` appends the confidence interval in brackets.

* `table_continuous()` gains a `test` argument (`"welch"`, `"student"`, or `"nonparametric"`) to choose the group-comparison method, along with independent `p_value` and `statistic` display toggles so users can request either or both outputs when `by` is used.

* ASCII console tables now split oversized outputs into stacked horizontal panels, repeating the left-most identifier columns so wide `freq()`, `cross_tab()`, `table_categorical()`, and `table_continuous()` prints stay readable in narrow consoles.

## Breaking changes

* `table_categorical()` replaces `table_apa()` as the public helper for categorical summary tables. It uses `select` and `by`, supports grouped cross-tabulation or one-way frequency-style tables when `by = NULL`, and consolidates output formats under a single `output` argument. Migrate existing `table_apa()` calls to `table_categorical()`, use `output = "default"` for ASCII tables and `output = "data.frame"` for plain data frames, and replace former `output = "wide"` / `style = "report"` paths with the formatted output engines.

* Excel export now uses `openxlsx2` instead of `openxlsx` for a lighter dependency footprint (no Rcpp compilation required).

## Minor improvements

* Package citation metadata now uses the current package title and CRAN DOI, so `citation("spicy")` matches `DESCRIPTION` and points to the package DOI.

* `table_categorical()` and `table_continuous()` now print shorter ASCII titles without appending the input data frame name, and no longer require `officer` for `output = "flextable"` alone; `officer` is now required only for Word export paths that actually write `.docx` files.

* `table_continuous()` now accepts tidyselect syntax in `exclude` in addition to character vectors, and no longer warns that `test` is ignored when it is still needed to compute effect sizes.

# spicy 0.6.0

## New features

* New family of association measure functions for contingency tables: `assoc_measures()`, `contingency_coef()`, `gamma_gk()`, `goodman_kruskal_tau()`, `kendall_tau_b()`, `kendall_tau_c()`, `lambda_gk()`, `phi()`, `somers_d()`, `uncertainty_coef()`, and `yule_q()`. Each returns a numeric scalar by default; pass `detail = TRUE` for a named vector with estimate, confidence interval, and p-value.

* `cross_tab()` gains `assoc_measure` and `assoc_ci` arguments. When both variables are ordered factors, it automatically selects Kendall's Tau-b instead of Cramer's V. The note format changes from `Chi-2: 18.0 (df = 4)` to `Chi-2(4) = 18.0`. Numeric attributes (`chi2`, `df`, `p_value`, `assoc_measure`, `assoc_value`, `assoc_result`) are now attached to the output data frame.

* `table_apa()` now dynamically labels the association measure column based on the measure used, instead of always showing "Cramer's V". New `assoc_measure` and `assoc_ci` arguments are passed through to `cross_tab()`.

* `table_apa()` gains `output = "gt"` to produce a `gt_tbl` object with APA-style formatting, column spanners, and alignment.

* `table_apa()` now correctly centers spanner labels over their column pairs in `tinytable` and `flextable` output.

* All association measure functions and `assoc_measures()` gain a `digits` argument (default 3) that controls the number of decimal places when printed. The p-value always uses 3 decimal places or `< 0.001`.

* `detail = TRUE` results now print with formatted output (aligned columns, fixed decimal places) via a new `print.spicy_assoc_detail()` method. `assoc_measures()` output uses a new `print.spicy_assoc_table()` method with the same formatting.

* New bundled dataset `sochealth`: a simulated social-health survey (n = 1200, 24 variables) with variable labels, ordered factors, survey weights, and missing values. Includes four Likert-scaled life satisfaction items (`life_sat_health`, `life_sat_work`, `life_sat_relationships`, `life_sat_standard`) for demonstrating `mean_n()`, `sum_n()`, and `count_n()`.

## Bug fixes

* `count_n()` now correctly counts `NA` values when `count = NA` and `strict = TRUE` are both used. List columns are now reported in verbose mode instead of causing silent errors.

* `cross_tab()` rescale logic now operates on complete cases only, so the weighted total N matches the unweighted N when missing values are present (consistent with Stata behavior).

* `freq()` now uses true `NA` consistently (instead of the `"<NA>"` string) in both weighted and unweighted paths. `cum_valid_prop` is now correctly `NA` for missing rows. Invalid `digits` and `sort` values are rejected with clear error messages.

* `mean_n()` and `sum_n()` now validate `min_valid` and `digits` arguments, rejecting non-numeric, negative, or multi-element values.

* `mean_n()`, `sum_n()`, and `count_n()` no longer trigger a tidyselect deprecation warning when `select` receives a character vector. Character vectors are now automatically wrapped with `all_of()`.

* `table_apa()` now preserves the original factor level order in row variables instead of sorting alphabetically. When `drop_na = FALSE`, the `(Missing)` category is placed at the bottom of each variable's levels. `percent_digits`, `p_digits`, and `v_digits` are now validated.

* `table_apa()` p-values no longer wrap across lines in `tinytable` HTML output.

## Breaking changes

* `cramer_v()` now accepts a `detail` argument. By default it returns a numeric scalar (as before). Pass `detail = TRUE` to get a 4-element named vector (`estimate`, `ci_lower`, `ci_upper`, `p_value`), or `detail = TRUE, conf_level = NULL` for a 2-element vector (`estimate`, `p_value`) without CI.

# spicy 0.5.0

## New features

* New `table_apa()` helper to build APA-ready cross-tab reports with multiple output formats (`wide`, `long`, `tinytable`, `flextable`, `excel`, `clipboard`, `word`).
* `table_apa()` exposes key `cross_tab()` controls for weighting and inference (`weights`, `rescale`, `correct`, `simulate_p`, `simulate_B`) and now handles missing values explicitly when `drop_na = FALSE`.

## Bug fixes

* `count_n()` no longer crashes when `special = "NaN"` is used with non-numeric columns. Passing `count = NA` now errors with a message directing to `special = "NA"`.
* `cross_tab()` fixes a spurious rescale warning for explicit all-ones weights and aligns the Cramer's V formula with `cramer_v()`.
* `table_apa()` no longer leaks global options on error. The `simulate_p` default is aligned to `FALSE`.
* `varlist()` title generation no longer crashes on unrecognizable expressions.

## Minor improvements

* `copy_clipboard()` parameter `message` renamed to `show_message`.
* `freq()` now dispatches printing correctly via S3.
* Removed unused `collapse` and `stringi` from `Imports`.

# spicy 0.4.2

* `cross_tab()` hardening: improved vector-mode detection (including labelled vectors), stricter weight validation, safer rescaling, and clearer early errors (e.g., explicit `y = NULL`).
* `cross_tab()` statistics are now computed on non-empty margins in grouped tables, avoiding spurious `NA` results; internal core path refactored to remove `dplyr`/`tibble` from computation while preserving user-facing behavior.
* `freq()` now errors clearly when `x` is missing for data.frame input and validates rescaling when weight sums are zero/non-finite.
* `count_n()`, `mean_n()`, and `sum_n()` regex mode is hardened (`regex = TRUE` now validates/defaults `select` safely).
* `mean_n()` and `sum_n()` now return `NA` (with warning) when no numeric columns are selected.
* `label_from_names()` now validates input type (`data.frame`/tibble required).
* `cramer_v()` now returns `NA` with warning for degenerate tables.
* Dependency optimization: `DT` and `clipr` moved to `Suggests`; optional runtime checks added in `code_book()` and `copy_clipboard()`.
* Tests expanded with regression coverage for all the above edge cases.

# spicy 0.4.1

* Fixed CRAN incoming check notes by removing non-standard top-level files.

# spicy 0.4.0

* Print methods have been fully redesigned to produce clean, aligned ASCII tables inspired by Stata's layout. The new implementation improves formatting, adds optional color support, and provides more consistent handling of totals and column spacing.

* Output from `freq()` and `cross_tab()` now benefits from the enhanced
  `print.spicy()` formatting, offering clearer, more readable summary tables.

* Documentation and internal tests were updated for clarity and consistency.

* `cross_tab()` gains an explicit `correct` argument to control the use of
  Yates' continuity correction for Chi-squared tests in 2x2 tables. The default
  behavior remains unchanged.

* The documentation of `cross_tab()` was refined and harmonized, with a clearer
  high-level description, improved parameter wording, and expanded examples.

* Minor cosmetic improvements were made to `varlist()` output: the title prefix
  now uses `vl:` instead of `VARLIST`, and the column name `Ndist_val` was renamed
  to `N_distinct` for improved readability and consistency.

* Minor cosmetic improvement: ASCII table output no longer includes a closing
  bottom rule by default.

# spicy 0.3.0

* New function `code_book()`, which generates a comprehensive variable
  codebook that can be viewed interactively and exported to multiple
  formats (copy, print, CSV, Excel, PDF).

# spicy 0.2.1

* `label_from_names()` now correctly handles edge cases when the
  separator appears in the label or is missing.

# spicy 0.2.0

* New function `label_from_names()` to derive and assign variable labels
  from headers of the form `"name<sep>label"` (e.g. `"name. label"`).
  Especially useful for LimeSurvey CSV exports (*Export results* ->
  *CSV* -> *Headings: Question code & question text*), where the default
  separator is `". "`.

# spicy 0.1.0

## Initial release

* Introduces a collection of tools for variable inspection, descriptive
  summaries, and data exploration.
* Provides functions to:
  * Extract variable metadata and display compact summaries (`varlist()`).
  * Compute frequency tables (`freq()`), cross-tabulations (`cross_tab()`),
    and Cramer's V for categorical associations (`cramer_v()`).
  * Generate descriptive statistics such as means (`mean_n()`), sums
    (`sum_n()`), and counts (`count_n()`) with automatic handling of
    missing data.
  * Copy data (`copy_clipboard()`) directly to the clipboard for quick export.
