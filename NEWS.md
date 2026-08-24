# spicy (development version)

## Breaking changes

* The adjusted R-squared reads `Adj. R²` everywhere. In
  `table_regression()` it is the label of a fit-statistic row and of the
  `ΔAdj. R²` change row; in `table_regression_uv()` it is also a column
  name, so code selecting that column must use the new spelling
  (`Adj.R²` had no space). `table_continuous_lm()` already used it.
* Declared missing values are now honored package-wide: codes a survey file
  declares missing (`na_values` / `na_range`, tagged NAs) count as missing
  in `freq()`, `cross_tab()`, `table_categorical()`, `table_continuous()`,
  `table_continuous_lm()`, `mean_n()` / `sum_n()` / `count_n()`, and
  `varlist()` / `vl()` / `code_book()`, so numbers change for labelled
  survey data. Nothing disappears silently: the tabulating helpers disclose
  the exclusion in the table note, and `freq()` keeps a labelled Missing row
  per code. `user_na = FALSE` restores the previous behavior. See `?freq`.
* `varlist()`, `vl()`, and `code_book()` count columns are coherent for
  labelled data: `N_distinct` uses the same missing definition as `N_valid`
  / `NAs`, and observed `na_range` codes and tagged-NA labels appear in
  `Values`.
* `cross_tab()` tabulates observations at an explicit `NA` factor level
  (`addNA()`, `factor(exclude = NULL)`) as a regular `NA` row or column:
  totals, percentages, and the chi-squared statistic include them.
* `freq()` on a factor with an explicit `NA` level excludes those
  observations from the valid-percent denominator and `n_valid`.
* `freq()` and `cross_tab()` replace `styled` with `output`: `styled = TRUE`
  is `output = "default"`, `styled = FALSE` is `output = "data.frame"`, and
  `styled` now errors (`spicy_defunct`). The `table_*()` rendering engines
  are not accepted here.
* `cross_tab(output = "data.frame")` returns a genuinely plain `data.frame`
  (a list of them with `by`): the metadata attributes are stripped. Read
  them from the default object, e.g. `attr(cross_tab(...), "p_value")`.
* `freq()` defaults to `rescale = FALSE` (raw weighted counts), matching
  `cross_tab()`, and reads `options(spicy.rescale)` the same way. Call
  `freq(..., rescale = TRUE)` for the previous behavior.
* `freq()` no longer prints as a side effect: a bare `freq(...)` still shows
  the table, but `f <- freq(...)` is silent. The unused `...` is removed, so
  unknown arguments error.
* `copy_clipboard()` arguments use snake_case: `row_names_as_col`,
  `row_names`, `col_names`. The old dot.case names error with the
  replacement.
* `build_ascii_table()` is no longer exported -- use `spicy_print_table()`
  -- and `column_total_line` is removed from both (it never had any effect).
* Association measures with `detail = TRUE` always include the standard
  error as an `se` element; the internal `.include_se` argument is gone.
* On degenerate tables, `gamma_gk()`, `kendall_tau_b()` and
  `kendall_tau_c()` give an `NA` p-value when the asymptotic SE is zero, and
  `uncertainty_coef()` and `kendall_tau_c()` return `NA` with a
  `spicy_undefined_stat` warning at zero entropy or on a constant variable.
* The association measures and `assoc_measures()` validate `conf_level`:
  anything but a single number strictly between 0 and 1 (or `NULL`) raises
  `spicy_invalid_input`, and `conf_level = 95` hints at `conf_level = 0.95`.
* `table_categorical()` defaults to `drop_na = FALSE`: missing values
  display as a `"(Missing)"` level. With `drop_na = TRUE`, a note reports
  what was removed.
* `table_categorical()` uses the family's `labels` contract -- a named
  character vector, falling back to the label attribute then the column
  name. Unnamed positional vectors error with a hint.
* `table_categorical()` rejects `p_digits` below 1 with a classed error;
  such values were silently rendered with 3 decimals.
* `table_categorical(output = "long")` always names the association column
  `effect_size` and adds `effect_size_type` giving each row's measure
  (`"cramer_v"`, `"phi"`, ...); the column used to be named after the
  measure. Replace `out[["Cramer's V"]]` with `out$effect_size`. The
  `"data.frame"` output is unchanged.
* `table_categorical(output = "flextable")` no longer writes a `.docx` when
  `word_path` is supplied; the combination warns (`spicy_ignored_arg`). Use
  `flextable::save_as_docx()`.
* `table_continuous_lm(output = "data.frame")` names the effect-size
  interval bounds `es_ci_lower` / `es_ci_upper` -- the same names the
  `"long"` output has always used. Replace `out$effect_size_ci_lower` /
  `out$effect_size_ci_upper` with `out$es_ci_lower` / `out$es_ci_upper`.
* `standardized = "smart"` scales continuous inputs by 2 SD and leaves
  binary inputs unscaled. The rule was applied inverted since 0.12.0,
  halving every continuous "smart" beta.
* `table_regression(exponentiate = TRUE)` errors on links whose
  exponentiated coefficient is not a ratio (probit, cauchit, inverse, sqrt);
  identity links keep the warn-and-skip.
* `table_regression()` exempts intercept rows from `keep` / `drop`: the
  patterns select predictors, and `show_intercept` alone controls the
  intercept.
* `align = "auto"` is removed from all `table_*()` functions; use
  `"decimal"` (default), `"center"`, or `"right"`.
* The `show_fit_stats` information criteria are lowercase tokens `"aic"`,
  `"aicc"`, `"bic"`; uppercase errors with the replacement.
  `show_fit_stats = character(0)` errors; use `FALSE` to suppress the block.
* Multi-model `show_columns = "all_b"` / `"all_ame"` auto-compact (CIs
  dropped); request atomic tokens to keep them.
* `as_structured()` describes a row in the body itself instead of in
  row-index vectors, which broke as soon as two tables were stacked or
  merged. The five index components of 0.12.0 are removed, and a table
  built by an older spicy is refused rather than read as if it were
  current. `version` is `3`.
  * `reference_rows` becomes `cell_status`, which marks the reference
    *cell* rather than the whole row.
  * `factor_header_rows` becomes `body$.row_role == "factor_header"`.
  * `fit_stat_rows` becomes `body$.row_role == "fit_stat"`.
  * `level_rows` becomes `body$.indent > 0`.
  * `outcome_row` becomes `body$.row_role == "outcome"`.
* `tidy()` labels AME rows `estimate_type = "ame"` (was `"AME"`), and the SE
  footer reads `"classical (Fisher information)"`.
* `count_n()` warns (`spicy_no_selection`) and returns `NA` for all rows
  when the selection resolves to zero usable columns; a valid selection
  whose value is simply absent still counts `0`.
* `mean_n()` and `sum_n()` with `min_valid = 0` return `NA` for rows with no
  valid values (was `NaN` and a silent `0`).
* A robust `vcov` that cannot be computed is now an error. spicy warned and
  returned the classical variance, which the table then labelled robust.
  This was reachable from ordinary calls: a `cluster` vector containing `NA`
  produced a table of classical standard errors under a cluster-robust
  footer. Use `vcov = "classical"` to ask for the model-based variance.
* `cluster` containing `NA` is refused for every cluster-robust estimator.
  For `coxph()` it used to return a variance that treated the observations
  with a missing id as a cluster of their own. Refit without those
  observations, or impute the ids.
* `table_regression(nested = TRUE)` errors on `nlme::gls()` / `nlme::lme()`
  fits estimated by REML whose fixed effects differ, and on any pair whose
  own `anova()` method refuses the comparison (for example one fit by REML
  and one by ML). The restricted likelihood is not comparable across
  different fixed-effect specifications, nor across estimation methods.
  Refit both with `method = "ML"`. Comparisons that differ only in the
  random structure are unaffected.

## New supported models

`table_regression()` gains support for more than thirty model classes
beyond `lm` / `glm`. See `?table_regression_models` for the registry and
per-family behaviour, and the new vignettes for walk-throughs. Requests a
class cannot honour are refused with a classed error (`spicy_unsupported_vcov`,
`spicy_unsupported_standardized`) instead of rendering an empty column.

* Mixed effects (`lmer` / `glmer`, `glmmTMB`, `lme`, `gls`): random effects
  as a block of rows (SD, correlations, residual, with SE and CI), ICC,
  per-group N and marginal / conditional R² as fit statistics, and a
  boundary-corrected LR test against the model without random effects.
* GEE fits (`geeglm()`): its own sandwich SEs (clustered on `id =`, or the
  `std.err =` jackknife) are the inference, the footer gives the working
  correlation and alpha, fit stats report the cluster structure (`"qic"` /
  `"qicu"` / `"scale"` opt-in), with AME and `exponentiate` as for `glm`.
* Bayesian (`stan_glm()`, `brm()`): posterior median, MAD SD, and
  equal-tailed credible intervals (`95% CrI`, or `ci_method = "hdi"`); no
  p-values, probability of direction opt-in (`show_columns = "pd"`).
  `R² (Bayes)` is a default fit stat, `"elpd_loo"` / `"looic"` / `"waic"`
  opt-in. `exponentiate = TRUE` works on the draws, AME included, and
  multilevel fits get a `Random effects (MCMC)` block.
* Survival (`coxph` / `survreg`, `cph`, `flexsurvreg`): Cox tables report
  `n` and `N events` as fit statistics and the concordance as a footer note.
* Categorical (`multinom`, `mlogit`): a single `multinom` renders outcome
  categories as column groups (`outcome_labels` relabels them), and `mlogit`
  uses a two-segment alternative-specific layout.
* Ordinal (`polr`, `clm`): thresholds render as a labelled block
  (`show_thresholds = FALSE` to opt out), and partial-proportional-odds
  terms and `clm(scale = ~)` scale coefficients get their own blocks, the
  latter kept on the log scale under `exponentiate = TRUE`. An aliased
  `clm` predictor (rank-deficient design) renders as undefined, like an
  aliased `lm` or `glm` coefficient.
* Robust / IV / panel (`estimatr`, `ivreg`, `feols` and
  friends); beta, Tobit, and two-part counts (`betareg`, `tobit`, `zeroinfl`
  / `hurdle`); plus `rlm`, `glm.nb`, `rq`, `gam` / `bam`, `nls`,
  `ols` / `lrm` / `Glm`, and `selection`.
* Design-based generalized linear models (`survey::svyglm()`, and
  replicate-weight designs): design-based standard errors, Wald t at the
  design's residual degrees of freedom, and average marginal effects
  averaged over the population the design describes rather than over the
  sample. Both counts are reported -- the observed `n` and the
  `Weighted n`, which is the sum of the sampling weights and not of the
  first replicate's. The `AIC` row is survey's design-based criterion
  (Lumley & Scott 2015), with `show_fit_stats = "eff_p"` for the effective
  number of design parameters beside it; `BIC` needs a maximal model and
  stays blank, and the deviance, log-likelihood and residual scale are
  absent rather than reported on the scale of the sum of the weights.
* Design-weighted ordinal models (`survey::svyolr()`): the cut-points as a
  Thresholds block, per-category average marginal effects averaged over the
  population, and the design's residual degrees of freedom for every row.
  Statistics that need a likelihood -- AIC, BIC, deviance, pseudo-R² -- are
  absent rather than approximated; the omnibus test is
  `survey::regTermTest()`.
* Design-weighted Cox models (`survey::svycoxph()`, and replicate-weight
  designs): hazard ratios, n and the number of events, concordance in the
  note, and the design's residual degrees of freedom. RMST and
  risk-difference columns are refused for these fits -- their uncertainty
  comes from resampling subjects, which ignores the strata and clusters --
  with a message naming the cause and pointing at `survey::svykm()`.

## New functions

* `spicy_labels()` returns every table label with the key that names it,
  in the language currently in force. `spicy_labels("fr")` shows the
  French set. The keys are what `options(spicy.labels)` takes.

* `table_continuous_svy()` summarizes continuous variables from a
  `survey` design object -- `survey::svydesign()` or
  `survey::as.svrepdesign()` -- instead of a data frame. Every
  statistic is computed by survey: `svymean()` for the mean, its
  standard error and its design effect, `svyvar()` for the standard
  deviation, `svyquantile()` for the quantiles, and `svyttest()` /
  `regTermTest()` / `svyranktest()` for the group comparison. Intervals
  and tests use the design degrees of freedom, `by =` gives each group
  its own domain (and its own df), and the table note states the
  design, the variance method and the sample size in both counts.
  `show_columns` takes the tokens of `table_continuous()` plus `"se"`
  and `"deff"`; every output engine, `as_structured()` and `inline()`
  work as usual.

* `table_categorical_svy()` is the categorical half of the same pair:
  counts and estimated percentages from a `survey` design, with the
  block layout of `table_categorical()`. `survey::svymean()` estimates
  the percentages and their design effects, `survey::svyciprop()`
  their confidence intervals (`proportion_ci = TRUE`, seven methods),
  and `survey::svychisq()` tests the association -- Rao-Scott
  corrected by default. `n` is the observed count; the note gives the
  estimated population beside it.

* `table_outcome()` summarizes one continuous outcome across the levels
  of several categorical variables, stacked as blocks -- the inverse
  layout of `table_continuous()`. Each block reports a group comparison
  (`p`, optional test statistic and effect size), and an `Overall` row
  gives the marginal summary. Statistics are chosen with the same
  `show_columns` tokens as `table_continuous()`, and every output engine
  is available. See `vignette("table-outcome")`.
* `inline()` cites one table cell in running Quarto / R Markdown text:
  the returned string is exactly the displayed cell -- same decimals,
  *p* style, interval punctuation, journal style -- so a number quoted
  in a sentence can never drift from the number printed in the table.
  Rows are addressed by variable / level identity (never by display
  label), columns by their typed token, `"ci"` composes the interval,
  `{token}` patterns build full fragments
  (`"{b} ({ci_label} {ci}; p {p})"`), and every misaddressing errors
  with the list of available choices. A statistic that belongs to a
  whole variable rather than to one of its levels -- the *p* of a
  `table_categorical()` block, its association measure, its SMD -- is
  cited without a `level`. A variable carrying a real level named
  `"(Missing)"` is addressed by that name, and the auto-renamed
  missing category by its own (`"(Missing_1)"`).
* `table_regression_uv()`: univariable screening tables -- one fit per
  predictor, one row block each, merged side by side with the multivariable
  model. Supports `lm` (the linear default), `glm` (selected by `family`, in
  any form `glm()` accepts), and `coxph` (`outcome = Surv(time, status)`).
* In the linear screen, a binary-looking outcome proceeds as a linear
  probability model, in a classed warning pointing to `vcov = "HC3"` and
  `method = "glm"`. A per-predictor `N` column shows by default, with a note
  when Ns differ, and `complete_cases = TRUE` forces the common sample.
* Univariable intercepts are hidden by default; `show_intercept = TRUE` adds
  each fit's own. `p_adjust` covers the whole screen, and `exponentiate`,
  `vcov` / `cluster` (the footer names the cluster column), `labels`, the
  output engines and `tidy()` work as in `table_regression()`.
* `table_regression_models()`: the machine-readable registry of supported
  model classes (family, engine, AME, exponentiate semantics); its help page
  is the per-family reference.
* `spicy_style()`: build a table style, or fetch one of the named journal
  themes. `?spicy_style` lists, for each theme, the exact rules it encodes
  and the official document they come from.

## New features

* `options(spicy.language = "fr")` prints table labels in French --
  headers, row labels, titles and table notes -- for a whole document.
  `"en"` is the default and is unchanged. A label the French set does
  not carry falls back to English, so nothing comes out blank.

* `options(spicy.labels = list(row_missing_level = "(No answer)"))`
  overrides one label at a time, in any language. It is the layer to
  reach for when a single word has to change and a language does not.

* A language changes only what a reader sees. The column names of
  `as.data.frame()`, `tidy()` and `as_structured()` stay put, so
  `out[["Yes %"]]` resolves whatever the language, and so do the
  mathematical glyphs. Errors and warnings stay in English.

* Number formatting stays a separate lever: `options(spicy.language =
  "fr")` translates the words, `style = "fr"` writes the decimal comma.
  A French report usually wants both.

* `table_regression()` names the variance estimator the design actually
  uses: Taylor linearisation, replicate weights and their scheme, or a
  two-phase design.

* Average marginal effects of a survey-weighted model answer to the
  design's residual degrees of freedom, like the coefficient rows above
  them, so one `p` header covers one reference distribution.

* A regression under a survey design reports both counts by default -- the
  observed `n` and the `Weighted n` the estimates describe -- like the
  descriptive tables.

* The note of a survey regression names the sampling design and the residual
  degrees of freedom its tests use, so the table can be read without the
  design object at hand.

* A design whose calibration produced negative weights says so in the
  note: the exact count, and what follows from it -- a weighted mean can
  fall outside the observed range, and a variance can come out negative.
  `table_continuous_svy()` and `table_categorical_svy()` then do not
  report the group comparison of the variables concerned, because a
  design-based test is not defined when the weights change sign; the
  note says which comparisons were withheld and the call warns
  (`spicy_negative_weights_no_test`). Estimates and counts are
  unaffected, and so are calibrated designs whose weights all stay
  positive.

* A design table and a survey regression name the variance the same way:
  `Std. errors: Design-based (Taylor linearisation)`, or
  `(replicate weights, JK1)` on a replicate design.

* `show_fit_stats = "eff_p"` reports the effective number of parameters of a
  `survey::svyglm()` design.

* `table_categorical_svy()` refuses `chisq_statistic = "saddlepoint"` on a
  replicate-weights design, where survey computes its p-value without the
  denominator degrees of freedom and it comes out too small. The same option
  on a `survey::svydesign()` design is correct and still accepted.

* `table_categorical_svy()` tests the same table on a calibrated design as
  on any other: the displayed `"(Missing)"` category is descriptive and does
  not enter the comparison, so `drop_na = FALSE` gives the p-value of
  `drop_na = TRUE` instead of `NaN`.

* `vignette("survey-tables")` gains a section on regression under a design,
  and `?table_continuous_svy` / `?table_categorical_svy` state that the two
  are experimental.

* Handing a `survey` design object to `table_continuous()`,
  `table_categorical()`, `table_continuous_lm()` or `table_outcome()` now
  errors with the function to call instead, rather than with
  `` `data` must be a data.frame ``. The design-based standard errors,
  degrees of freedom and tests cannot be recovered from the weights
  alone, so the answer is a different function, not a coercion.

* `table_continuous()` and `table_categorical()` gain `smd = TRUE`, an
  `SMD` column with the standardized mean difference between the two
  groups of `by` -- the balance diagnostic of a Table 1. It is signed
  (group 1 minus group 2, in display order) for a continuous or
  two-category variable and unsigned for a variable with more, where it
  is a multivariate distance; it carries no confidence interval and no
  p-value; and it requires exactly two groups. It works under `weights`
  and is rounded with `effect_size_digits` / `v_digits`, so the journal
  styles reach it. Do not read it for `effect_size = "hedges_g"`: the
  SMD is Cohen's *d* when the two groups are the same size, while *g*
  applies the small-sample correction on top and so never equals it.
  See the "Standardized mean difference" section of
  `?table_continuous` and `?table_categorical`.
* The grouped raw outputs of `table_continuous()` (`"data.frame"` /
  `"long"`) always carry `smd_type` and `smd_value`, `NA` when
  `smd = FALSE`, so the schema does not move with the argument.
  `glance()` on a `spicy_continuous_table` gains the same two columns,
  before `n_total` and present even without `by` (`NA` there, like its
  other comparison columns) -- index that frame by name, not by
  position. The categorical `"long"` output gains `smd` / `smd_type`
  only when they are requested, as its association columns do;
  `glance()` on a `spicy_categorical_table` does not carry the SMD.
* `table_continuous()` weighted group comparisons now say that they
  refuse tests and effect sizes specifically, and point at
  `smd = TRUE`, which passes under weights.
* `table_continuous()` gains `weights` and `rescale`: weighted mean,
  SD, quantiles, extremes and mean CI under a documented convention
  (integer weights reproduce the row-expanded data exactly, all
  weights 1 reproduce the unweighted table; `rescale = TRUE` gives
  the sampling-weights reading, whose SD equals Stata's `[aweight]` /
  `survey::svyvar()`). A new `"weighted_n"` column token reports the
  sum of weights (the raw `"data.frame"` / `"long"` outputs always
  carry a `weighted_n` column, `NA` without weights), and the table
  names its weights in the note. Group
  tests and the median CI are deliberately refused under weights --
  `table_continuous_lm()` is the weighted-comparison tool. See the
  Weights section of `?table_continuous` for the formulas and the
  cross-software correspondence.
* New `style` argument on `table_regression()`, `table_categorical()`,
  `table_continuous()` and `table_continuous_lm()`: `"jama"`, `"nejm"`,
  `"lancet"`, `"annals"`, `"apa"`, `"aer"` and `"fr"`. Each theme encodes only rules
  taken from an official document of the institution, listed one by one in
  `?spicy_style` -- numeric formatting conformity, not full editorial
  conformity. An unknown name errors and names the ones that exist.
* Themes move defaults only. Any formatting argument you pass wins over the
  theme, even at its own default value.
* New `options(spicy.style = )` for document-wide scope, like the language
  of a report. The `style` argument overrides it per call.
* `spicy_style()` composes a style by hand -- p-value decimals, bands or
  significant figures, the `<` floor, the leading zero, decimal mark,
  interval separator and brackets, stars, per-family digits -- and can start
  from a theme: `spicy_style("lancet", ci_sep = " to ")`. Every field is
  validated; a misspelt lever errors instead of being ignored.
* `table_categorical()`, `table_continuous()` and `table_continuous_lm()`
  accept any single character as `decimal_mark`, like `table_regression()`
  already did. This is what lets `"lancet"` set the midline dot.
* `as_structured()` now reads the descriptive tables --
  `table_categorical()`, `table_continuous()` and
  `table_continuous_lm()` -- and returns the schema it returns for
  `table_regression()`: a numeric body carrying the identity of every
  row in `.variable` / `.level` / `.row_role` / `.indent`. New roles
  `"summary"`, `"group"` and `"missing"` name the descriptive rows.
* Table notes rendered by the `"tinytable"` engine are set one size down
  (`0.9em`, black), like the notes of the other engines. New
  `options(spicy.note_style)`: `"none"` leaves the note to the document
  template, and any other string is added to the Typst `text()` call around
  it, e.g. `"fill: luma(89)"` for a grey note.
* Table notes rendered by the `"tinytable"` engine escape the labels they
  interpolate when the output is HTML. A variable or level label containing
  markup used to reach the footer as markup. Typst and LaTeX output is
  unchanged.
* `table_continuous()` gains `show_columns`: pick the statistics the table
  shows -- `"med"`, `"q1"`, `"q3"`, `"iqr"`, the compact `"med_iqr"`
  (`Med [Q1, Q3]`), and `"med_ci"` (exact order-statistic CI of the median)
  alongside the usual `"m"`, `"sd"`, `"min"`, `"max"`, `"ci"`, `"n"`. Pass a
  named list to give each variable its own selection. A variable shown as a
  median is tested as one: its default test becomes Wilcoxon /
  Kruskal-Wallis with the matching rank effect size, per variable, and the
  note says which test each variable carries. The default display is
  unchanged.
* New `show_columns` tokens `"r2"` and `"adj_r2"` for linear
  `table_regression_uv()` screens: the share of outcome variance each
  predictor explains on its own, one value per predictor block. The
  multivariable model keeps reporting its R-squared in the fit-statistics
  rows; `glm` and `coxph` screens are refused.
* New `show_columns` token families `"rmst"` and `"risk_diff"` for `coxph`
  and `survreg`: covariate-adjusted differences in restricted mean survival
  time over `[0, tau]` and in cumulative incidence at `at_time`, by
  g-computation, with bootstrap SEs, CIs, and p-values.
* The horizon is explicit and required (`tau = "minmax"` picks the smallest
  per-group maximum follow-up, and is refused in the univariable screen,
  which takes both families at one shared horizon); factors get one row per
  level, continuous predictors the +1-unit contrast.
* Stratified Cox fits keep each subject's own stratum baseline; stratified
  `survreg` fits are refused.
* The baseline hazard behind these columns follows the tie handling of the
  fit, as `survfit()` and `basehaz()` do: a `ties = "breslow"` fit gives a
  Breslow baseline, the default Efron fit an Efron one. Documented in
  `?table_regression` and `vignette("table-regression-survival")`.
* New `show_columns` token `"n_events"`: event counts as `events/N` next to
  the estimates -- per factor level (reference row included), model totals
  on continuous rows -- for binomial outcomes (`glm`, `glmer`, `glmmTMB`)
  and right-censored `coxph` fits.
* Heteroskedasticity- and cluster-robust `vcov` for the supported classes.
  Resampling footers report the valid replicate count, and a bootstrap /
  jackknife whose replicates nearly all fail raises
  `spicy_resampling_failed`.
* `vcov = "CR1S"` for `lm` fits reproduces Stata's `regress, vce(cluster)`
  exactly (CR1S scaling with `t(G - 1)` inference, named in the footer);
  `"CR2"` stays the recommended modern choice, and `"CR1S"` on a `glm` is
  refused.
* Cluster-robust `vcov` (`"CR0"`-`"CR3"`) for `multinom`, AME columns
  included (needs sandwich >= 3.1-2); `HC*` stays refused. `glmmTMB` and
  `svyglm` are classical-only -- for survey fits, declare the clustering in
  `svydesign(ids = )`.
* `rq` fits get their own `vcov` family: the robust `"nid"` sandwich by
  default, with `"iid"`, `"ker"`, `"rank"` (rank-inversion CIs, no SE / t /
  p) and a native `"bootstrap"` opt-in; `cluster` runs the wild gradient
  cluster bootstrap, and `HC*`, `CR*` and `"jackknife"` are refused.
* `ci_method = "profile"` gives profile-likelihood CIs for `glm`, `polr`,
  and `clm`; new `ci_method = "boot_percentile"` (with `vcov = "bootstrap"`)
  reports percentile bootstrap CIs from the same replicates as the SEs.
* `re_ci = "profile"` gives profile-likelihood CIs for the variance
  components of `lmer` / `glmer` fits (no SE column; the footer discloses
  the method), and `re_test = "lrt"` / `"rlrt"` fills the test columns of
  the `Random effects` rows.
* Variance-component SEs are omitted on large mixed fits, above
  `options("spicy.re_se_max_n")` (default 1000), with a note and a warning
  giving the override.
* `glmmTMB` and `lme` fits report a singular (boundary) random-effect
  structure the way `lmer` / `glmer` ones do: a table note, a warning, and
  no SE or CI on the collapsed variance components. For `glmmTMB` the check
  covers the zero-inflation and dispersion components too.
* Nested `lme` fits (`random = ~ 1 | A/B`) show one `Random effects` row
  per level, each with its own label, SE and CI, plus one `N (<factor>)`
  row per grouping factor. Levels are named the way `lmer` names them
  (`B:A`), so the same nested model fitted with either engine lines up in
  a multi-model table.
* A `glmmTMB` fit whose optimizer did not converge says so: a table note
  names the engine's own diagnosis and a `spicy_nonconvergence` warning
  (under `spicy_caveat`) fires. No fit statistic derived from such a fit is
  reported -- ICC, R², AIC and BIC are all withheld, since they would be
  computed from the values the optimizer stopped at. The estimates and the
  variance components still print: they are what the model object holds.
  `spicy_nonconvergence` is the only warning raised; the anonymous
  `NaNs produced` warnings the engine emits alongside it are suppressed.
* Under a cluster-robust `vcov`, the ordinal Thresholds block (`polr` /
  `clm`) takes its SEs, z, p and CIs from the same sandwich as the slopes.
* AME columns are available for many more classes, and per outcome category
  for `polr` / `clm` / `multinom`; their SEs, CIs, and p-values honour a
  robust `vcov`. Classes with no AME backend are refused with a pointer to
  `?table_regression_models`.
* `nested = TRUE` works for `multinom` (LR chi-square rows), defaults to LRT
  rows for Cox comparisons, and compares nested `rq` fits through a
  Wald-type F (all fits at one tau; mixing taus or classes is refused).
* `fixest` tables show their absorbed fixed effects by default: a
  `Fixed effects:` block with a Yes / No row per factor (blank for
  non-fixest models), plus the within R-squared `"within_r2"` for `feols`
  and McFadden's pseudo-R² for `feglm` / `fepois`.
* The `"n_groups"` token renders one `N (<factor>)` row per grouping factor
  -- absorbed fixed effects and crossed or nested random effects alike.
* Two-part models show their full model: the zero component of `zeroinfl` /
  `hurdle` and the `ziformula` / `dispformula` components of `glmmTMB`
  render as labelled row blocks (`show_components = FALSE` to opt out),
  exponentiated only when the link yields a ratio.
* Class-aware fit-statistics defaults: McFadden's and Nagelkerke's pseudo-R²
  for ordinal and multinomial fits, `nobs` and `AIC` for every other class
  instead of a blank block, and plain counts (e.g. `N (Subject)`) in the
  `N (groups)` row when models share one grouping factor.
* New opt-in `show_fit_stats` dispersion tokens: `"theta"` (the NB2
  dispersion) and `"alpha"` (its reciprocal) for `glm.nb`, and `"phi"` for
  `betareg`. Refused for other families, and `"phi"` also when the precision
  has covariates (`y ~ x | z`).
* Bayesian tables run a sampler-diagnostics guard (R-hat, ESS, divergences,
  E-BFMI) whose failures add a footer line and a `spicy_bayes_diagnostics`
  warning; `"rhat"` / `"ess_bulk"` / `"ess_tail"` / `"mcse"` are available
  as per-coefficient columns.
* All-Bayesian tables drop the p column from the defaults, refuse an
  explicit `"p"` / `"t"` request, expand the `"all_b*"` presets without
  them, and carry no `ame_p`; mixed frequentist + Bayesian tables keep the
  shared `95% CI` label and dash the Bayesian p cells.
* `p_adjust` and likelihood-based fit statistics are refused for
  all-Bayesian tables, standardized betas are limited to `"posthoc"` /
  `"basic"` / `"smart"` on fixed-effects fits, and variational / optimizing
  fits are refused with a refit hint.
* When a `β` column is displayed, the table note names the standardisation
  method and its factor-dummy convention, states the interaction convention,
  and is fallback-aware.
* `broom::tidy()` gains an `outcome_level` column naming the response
  category of per-category rows (ordinal and multinomial AMEs).
* `select` is optional in `table_categorical()`: when omitted, every
  eligible categorical column is tabulated (factor, character, logical,
  labelled), excluding `by`. An explicit `select` is taken verbatim, so
  numeric-coded categoricals can be tabulated by naming them.
* `table_continuous()` gains `drop_na`: the default `TRUE` keeps the
  historical behavior, `FALSE` shows rows with a missing `by` value as a
  `"(Missing)"` group, with the test and effect size still computed on the
  observed groups. Both modes disclose removed values in a table note.
* `table_categorical()` reads `options(spicy.rescale)` like `cross_tab()`;
  an explicitly supplied `rescale` still wins.
* Seven new vignettes: *Mixed-effects*, *GEE (population-averaged)*,
  *Multinomial*, *Count and two-part*, *Survival*, *Ordinal regression
  tables*, and *Categorical predictors*, a guide to dummy coding, reference
  levels, joint tests of a factor, and contrast codings.
* `as_structured()` carries everything the printed table shows, so a custom
  renderer no longer has to reconstruct it: significance markers and their
  cutoffs (`stars`), the display string of cells no single number can
  express such as the `events/N` counts (`col_meta$display_cells`), and the
  absorbed fixed-effects block as a header row plus one row per factor. A
  new `version` field names the contract, and a view built by a newer
  version than the one reading it is refused instead of mis-read.
* `as_structured()` also gives every row an identity that does not depend on
  where the row sits: `body$.variable` (the source variable, or the
  fit-statistic token), `body$.level` (the factor level), `body$.row_role`
  (`"coef"`, `"factor_header"`, `"level"`, `"reference"`, `"fit_stat"`,
  `"outcome"`, `"vc"`) and `body$.indent`. `cell_status` says the same per
  cell -- `"reference"` when a cell is a reference level of its estimate
  block, `"undefined"` when the statistic applies but no number expresses
  it -- so a renderer never has to read an en-dash back to find out.

## Bug fixes

* `nested = TRUE` works for every model class that has a likelihood. Nested
  comparisons of `survival::survreg()`, `MASS::polr()`, `ordinal::clm()`,
  `nlme::gls()`, `nls()`, `MASS::rlm()`, `pscl::zeroinfl()` and
  `pscl::hurdle()` fits failed with an untranslated base R error.

* `nested = TRUE` reports the likelihood-ratio change test for classes that
  ship no `anova()` method -- `betareg::betareg()`, `pscl::zeroinfl()`,
  `pscl::hurdle()`, `mlogit::mlogit()`, `flexsurv::flexsurvreg()` and
  `fixest` -- instead of a row of dashes. The class-aware defaults give
  those classes the likelihood-ratio rows instead of empty
  change-in-R-squared and F rows.

* `nested = TRUE` no longer reports a negative chi-square with a p-value
  when the models are passed largest-first.

* `output = "gt"`: two `by` levels differing only in whitespace (`"A B"` and
  `"A-B"`) no longer collapse to the same HTML id.

* `inline()` says whether an empty cell's column is empty throughout the
  table, and names the per-group columns that can be addressed instead.

* `weighted_nobs` is `NA` for an unweighted `glm()`, in
  `table_regression()` and in `glance()`. It used to equal `n`:
  `stats::weights()` returns the prior weights of a `glm()`, a vector of
  ones on an unweighted fit, and their sum was reported as a weighted
  count. In a table holding a survey regression, where the row shows for
  every model, the observed count read as a population. An unweighted
  `lm()` was already `NA`, because `stats::weights()` returns `NULL` for
  it.

* `output = "gt"` tables carry their table note into the saved file.
  `gt::gtsave()`, `gt::as_raw_html()` and a non-interactive `print()` used
  to produce a table without the missing-value disclosure, the test note or
  the column glosses the console prints. The interactive HTML display is
  unchanged: the note still renders outside the table grid, once.

* `output = "gt"` renders valid HTML when a `by` level or a model name
  contains a double quote, an angle bracket or a backslash. gt builds the
  `headers` attribute of every body cell from the column name without
  escaping it, so a level such as `Q"1` closed the attribute early and the
  rest of the name was re-parsed as markup. Affects `table_categorical()`,
  `table_continuous_lm()`, `table_regression()` and the design twins --
  where `table_categorical_svy()` did not even render: the same raw name
  reached a CSS selector and the style compiler stopped with an error
  instead of returning a table. The column names
  `output = "data.frame"` publishes are unchanged, the header still shows
  the level as it stands in the data, and a table of ordinary names renders
  exactly as before.

* `output = "tinytable"` escapes cell text in `table_categorical()`,
  `table_continuous()`, `table_continuous_lm()` and `table_outcome()`. A
  level or variable label containing markup was rendered as markup: a label
  holding `</td></tr><tr><td>` split its own row, so the HTML table had more
  rows than the object and the statistics were redistributed across them,
  and a label holding a script element was emitted live. gt and flextable
  already escaped.

* `output = "gt"` tables from `table_categorical()`, `table_continuous()`
  and `table_continuous_lm()` carry their title, like the other output
  engines.
* Standard errors, confidence intervals, and p-values from
  `vcov = "jackknife"` and `vcov = "bootstrap"` (cluster variants and
  `ci_method = "boot_percentile"` included) were wrong for binomial and
  quasibinomial models fitted with a two-column
  `cbind(successes, failures)` response: every resampling refit re-applied
  the binomial totals to the weights, so replicates were effectively
  fitted with squared weights. The error grows with the spread of the
  totals (from under 1% up to over 30% in our checks). Fits with a 0/1,
  factor, or proportion-plus-weights response were never affected.
* `R² (McFadden)` and `R² (Nagelkerke)` -- shown by default for logistic
  models -- were wrong for binomial models fitted with a two-column
  `cbind(successes, failures)` response, and badly so: the intercept-only
  refit behind both statistics re-applied the binomial totals to the
  weights, so the null model was effectively fitted with squared weights
  (McFadden read 0.92 where the true value was 0.32 in our checks). Fits
  with a 0/1, factor, or proportion-plus-weights response were never
  affected.
* `standardized = "refit"` on a glm whose response is a pre-built
  two-column matrix column (`d$Y <- cbind(s, f)`; `glm(Y ~ ...)`)
  refitted with those same doubled weights and reported slightly wrong
  standardized coefficients; it now refits on the proportion scale with
  the correct weights. The inline `cbind(...)` form keeps its documented
  fallback to `"posthoc"`.
* `table_regression()` refuses two models that would share a column
  label. A name colliding with the `"Model <position>"` label another
  slot takes by default -- `list("Model 2" = m1, m2)` -- used to draw
  two column groups nothing could tell apart, differently in each
  output engine, and made `inline(model = )` cite a different model
  than the one asked for. The error names the label and both positions.
* An `NA` in `names(models)` no longer crashes `table_regression()`
  deep in the renderer: it is treated as an unnamed slot and
  auto-filled like an empty name. A multi-valued `model` in
  `inline()` gets a classed error instead of a base R condition
  failure.
* `inline()` addresses each interval by its own token, so a table
  carrying more than one -- `ci` with `med_ci`, or `ci` with `ame_ci`
  -- can cite either. Both used to raise an ambiguity error naming
  `model`, which does not apply to a single-model or descriptive
  table, and `column = "ci"` on a `table_categorical()` now errors
  with the available tokens instead of composing the association
  interval of a row that has none.
* `inline()` reads `{ci_label}` from the interval the pattern quotes:
  `"{med} ({ci_label} {med_ci})"` on a table showing both intervals
  now says `Med 95% CI`, the header the table itself displays, instead
  of the mean interval's `95% CI`.
* `inline()` refuses to cite an interval it cannot compose, as it
  already did for `b`, `se` and `p` and as `?inline` documents: cells
  that are a reference level or an undefined statistic (it used to
  return `[–, –]`), and cells that are simply blank, such as
  `column = "assoc_ci"` on a level row of `table_categorical()`, where
  the association sits on the variable row (it used to return `[, ]`).
  The wording is the one the scalar tokens already used.
* A named style reaches every output, not only the console. The rules
  that vary with the p-value itself -- The Lancet's two significant
  figures, JAMA's third decimal below `.01` -- and a floor set apart
  from the decimals were applied when the table was printed but not
  when it was rendered, so `style = "lancet"` gave `0.16` on screen and
  `0.1634` in `tinytable`, `gt`, `flextable`, Word, Excel and the
  clipboard.
* `inline()` quotes the cell the table shows under a style. The style
  levers with no argument of their own (`p_bands`, `p_sigfig`,
  `p_floor`, `ci_sep`, `ci_brackets`) did not survive the call that
  built the table, so a sentence citing a Lancet table wrote its
  interval `[2.14, 5.65]` where the table printed `[2.14–5.65]`.
* A bare `inline(tbl, outcome)` on `table_continuous_lm(by = )` across
  a factor cites the mean difference. It returned the sample size: the
  contrast is token `"delta"`, and only a numeric `by` produces the
  `"b"` the default looked for.
* `inline()` cites a group's marginal mean on `table_continuous_lm()`:
  `inline(tbl, bmi, "Female", "emmean")`. That family puts its groups
  in the column headers, so `level` names the group there. Token
  `"emmean"` had no address at all: the ambiguity error named `model`,
  which the table has no spanners for, and listed no choices.
* `table_continuous_lm(by = , output = "gt")` renders when two `by`
  levels differ only in punctuation or in a non-ASCII character
  (`"a b"` / `"a.b"`, `"R²"` / `"R³"`). Their spanner ids collided and
  gt refused the table.
* `table_continuous()` and `table_continuous_lm()` label an interval
  with its own coverage: `ci_level = 0.975` reads `97.5% CI`, not
  `98% CI`. The percentage was rounded to a whole number, in the
  console header, in the rendered spanner, in the median-interval note,
  and -- for `table_continuous_lm()`, whose interval columns carry it
  -- in the column names of the `data.frame` output. Levels with a
  whole-number percentage (0.90, 0.95, 0.99, ...) are unchanged.
* A printed interval column pushed onto a continuation panel by a width
  split names its estimand at a fractional `ci_level` too: it reads
  `97.5% CI (B)` where it used to repeat the bare `97.5% CI`. Only
  whole-number coverages were recognised.
* `table_categorical()` refuses a `by` variable with no level to
  tabulate (`spicy_invalid_data`) instead of building a table with two
  unnamed columns and no rows, which the rendering engines then failed
  on with three unrelated errors. When every observation is missing
  `by`, the message points at `drop_na = FALSE`, which tabulates them
  as their own category.
* `table_categorical(by = , output = "gt")` renders when a `by` level
  contains a double quote. The level names the group columns, which
  are addressed by a CSS attribute selector, and the unescaped quote
  aborted gt's style compiler ("unterminated attribute selector").
* The significance-star legend of `table_regression(stars = TRUE)` follows
  the table's `decimal_mark`: a comma table now reads `p < ,001`, not
  `p < .001`.
* The confidence interval of an association measure in
  `table_categorical()` separates its bounds with `;` under
  `decimal_mark = ","`, as every other interval in the package already
  did; `0,45 [0,31, 0,59]` was ambiguous.
* Printing a `table_categorical()` table with an empty cell no longer
  fails with "missing value where TRUE/FALSE needed"; column widths are
  measured as they are displayed.
* A variable whose `label` attribute is `NA` falls back to the column
  name in `table_categorical()`, `table_continuous()` and
  `table_continuous_lm()`. The stub used to read `NA`, and
  `table_continuous()` failed outright with "missing value where
  TRUE/FALSE needed".
* A missing cell no longer disturbs the console layout of any table: it
  renders as an empty cell instead of leaving its row unpadded and every
  separator of the table out of register. A missing column *name* --
  `NA` in `names()`, or in `spicy_print_table(display_labels = )` --
  does the same thing to the header and is now blank too, on every
  panel of a table wide enough to be split.
* A variable label written in wide characters (CJK, emoji) no longer
  overflows a narrow console in `table_continuous()`: column widths are
  measured as they are displayed rather than counted as characters.
* The same measure now governs `table_continuous_lm()`, which used to
  split such a table across two panels on a console wide enough for one.
* An empty or non-string `clipboard_delim` raises a classed error instead
  of silently building an unusable payload.
* A `table_categorical()` variable label that happens to start with the
  indent string is no longer mistaken for a level row: every output now
  reads the block geometry from the table's typed row roles instead of
  parsing the label text back.
* The descriptive tables' `output = "clipboard"` shares the regression
  validator's pre-flight: on a system without a clipboard (headless
  session) they fail with the same clear `spicy_unsupported` error
  instead of an internal one from further down.
* `table_categorical()` draws its light rule between variable blocks in the
  `"gt"` and `"flextable"` outputs too, labels the first `"gt"` column
  `Variable` like every other engine, and `table_continuous_lm()` carries
  its title to the `"tinytable"` caption.
* Tables rendered to Typst no longer force a `5pt` column gutter when they
  carry grouped column headers (`by` groups, multi-model headers, CI
  spanners). All tables in a document now share the same column grid, and a
  document-level `#set table(column-gutter: ...)` rule becomes effective
  again.
* `table_categorical()` and `table_continuous()` carry their missing-value
  disclosure to every output: `"tinytable"`, `"gt"`, `"flextable"` and
  `"word"` now show it as a table note, and `"data.frame"` keeps it in the
  `missing_note` attribute. It used to reach the console print only, so a
  report rendered with `warning: false` showed a table computed on fewer
  observations than it announced.
* `table_continuous_lm()` keeps the same ledger: per-variable missing
  counts for outcomes and covariates (declared missing values listed
  separately), then the rows dropped for a missing `by` value or a
  missing weight. The `n` column shows the effect of the exclusions;
  the note shows the cause.
* Footer lines that cite a model use the model's displayed label: a
  table headed `Baseline / Adjusted` is footnoted `Baseline: ...`, not
  `Model 1: ...` -- a name that appeared nowhere in the table. Tables
  without custom labels keep the historical `Model 1` wording.
* `table_categorical()` and `table_continuous()` also carry their title to
  the `"tinytable"` output, as the table caption; a `by` table carries its
  association-measure note there too.
* A table without a confidence-interval column had its `"tinytable"` header
  rules one row too low -- no rule above the table, the per-model rules
  under the column labels instead of under the model names -- and carried an
  empty header strip below the labels.
* The `"tinytable"` output draws every rule the console draws between
  blocks: above `Thresholds:` and `Random effects:` in `table_regression()`,
  and between variable blocks in `table_categorical()`.
* A multi-line table note keeps one disclosure per line in the
  `"tinytable"` HTML and Typst output; the lines used to run together into
  a single sentence.
* `table_categorical()` without `by` labels the first `"tinytable"` column
  `Variable`, like the console and like the `by` version.
* Factor levels in a `"tinytable"` regression table are indented once, not
  twice.
* `table_categorical()`, `table_continuous()` and `table_continuous_lm()`
  carry their title to `output = "word"`, as the numbered caption the
  regression tables already get; the document used to open with an untitled
  table, and a `by` table lost the name of its grouping variable with it.
* A `by` table's association-measure note reaches the `"flextable"` and
  `"word"` outputs, beside the missing-value disclosure.
* Factor levels in a `"flextable"` or `"word"` regression table are indented
  once, not twice.
* A table without a confidence-interval column no longer carries an empty
  header strip in the `"flextable"` and `"word"` outputs: the column labels
  and the rules land where the console draws them.
* A column header in a `"flextable"` or `"word"` table stays inside its own
  model. Two models sharing a label used to merge into a single header cell
  straddling both, two confidence intervals into one `95% CI` spanning four
  columns.
* The `"flextable"` and `"word"` outputs draw every rule the console draws
  between blocks, not only the first: `Thresholds:` and `Random effects:`
  open with one too.
* `table_categorical()`, `table_continuous()` and `table_continuous_lm()`
  carry their title to `output = "flextable"`, as the table caption.
* Factor levels in a `"flextable"` or `"word"` `table_categorical()` are
  indented once, not twice.
* `output = "excel"` writes the cells the console shows: the `Events/N`
  counts keep their denominator (reference levels included), significance
  stars reach the estimates the note's legend documents, and absorbed fixed
  effects read `Yes` / `No` instead of `1` / `0`.
* `output = "excel"` with `decimal_mark = ","` no longer mixes separators:
  a sheet used to show `65.07` next to its own `<,001` because a numeric
  cell follows the reader's locale. The body is written pre-formatted
  instead; the default `"."` still writes real numbers.
* `table_categorical(output = "excel")` writes blank cells on
  variable-header rows. They used to be Excel error cells (`#N/A`), which
  spread the error to any `SUM()` over the column.
* `table_continuous(align = , output = "excel")` reaches the workbook:
  `"center"` centres every numeric column and `"right"` right-aligns
  them, as they already did on the console and in the `tinytable`, `gt`,
  `flextable` and `word` outputs. Both were silently ignored. The
  default `"decimal"` is unchanged -- Excel cells are unpadded, so it
  keeps the engine's convention of right-aligning the counts and the
  *p*-value and centring the rest. See `?table_continuous`.
* `table_categorical()` and `table_continuous()` carry their title and
  their missing-value / association notes to `output = "excel"`, like every
  other output; `table_continuous_lm()` gains the title. The table starts
  on row 3, below the title.
* Every Excel export sizes its columns to the text they carry: row labels
  such as `WHO-5 wellbeing index (0-100)` used to open clipped by the
  default column width.
* Factor levels in an `"excel"` regression table are indented once, not
  twice.
* `output = "clipboard"` survives any `clipboard_delim`: a cell holding the
  delimiter, a double quote or a line break is now quoted RFC 4180-style.
  A level label with a comma used to shift every following value one column
  to the right under `clipboard_delim = ","`, and `decimal_mark = ","` split
  every number into two cells.
* `output = "clipboard"` pastes numbers as numbers: the decimal-alignment
  padding is gone from the payload. It used the figure space U+2007, which a
  spreadsheet does not read as whitespace, so a padded cell landed as text
  in the middle of a numeric column.
* `table_categorical(output = "clipboard")` ships plain text: p-values and
  association measures used to arrive wrapped in an Excel formula
  (`="  .424"`), visible verbatim in a text editor or a word processor, and
  the wrapper turned blank cells into non-blank ones.
* `table_categorical()`, `table_continuous()` and `table_continuous_lm()`
  carry their title and their missing-value / association notes to
  `output = "clipboard"`, like every other output.
* A `table_continuous()` or `table_continuous_lm()` table without a
  confidence-interval column no longer pastes a blank line between its
  header and its body.
* `kendall_tau_b()` reported wrong SEs, confidence intervals and Wald
  p-values in every release from 0.6.0 through 0.12.0 -- its asymptotic
  standard error mis-scaled one margin term; point estimates were correct.
  Also affects `assoc_measures()` and the `cross_tab()` association line.
* `somers_d(direction = "symmetric")` returns `0` on equal concordant and
  discordant pairs, `cramer_v()` / `phi()` / `contingency_coef()` return
  `NA` with a `spicy_undefined_stat` warning on a zero margin, and
  `assoc_measures()` / `cross_tab()` stop swallowing those warnings.
* `cross_tab()`, `table_categorical()`, and `assoc_measures()` label the
  `tau_c` measure `"Stuart's Tau-c"` everywhere; several paths said
  `"Kendall's Tau-c"`.
* `cross_tab()` and `table_categorical()` surface the classed error a
  measure raises when it does not apply (e.g. `phi` on a 3x2 table);
  `assoc_measure = "auto"` counts levels under the table's `user_na` regime.
* `cross_tab()` computes weighted count Totals (`percent = "none"`) from the
  unrounded table, rounded once for display, and falls back to a neutral `x`
  / `y` / `weights` placeholder in titles and the weight footer instead of a
  data value plucked from an inline expression.
* `freq()` keeps its label footer when `NA`-weight rows are dropped, and
  `print()` invisibly returns the table object itself, not the rebuilt
  display frame.
* `freq()` warns (`spicy_caveat`) when `labelled_levels = "labels"` merges
  distinct codes sharing a label, and `valid = FALSE` drops the Valid
  Percent column -- and `Cum. Valid Percent` under `cum = TRUE` -- instead
  of printing `NA` under a `100.0` Total.
* `freq(sort = "name+")` / `"name-"` on labelled variables sorts by the
  underlying code whenever the code is displayed; string collation ranked
  `[10]` ahead of `[2]`. The `labelled_levels = "labels"` sort is unchanged.
* `table_categorical()` computes the ordinal association measures (`tau_b`,
  `tau_c`, `gamma`, `somers_d`) in declared level order under
  `drop_na = FALSE`; an internal re-sort to alphabetical order made them
  wrong when the orders differed.
* `table_categorical()` displays the value labels of labelled columns as
  `"[code] label"` levels in every path, and keeps a `by` level that is
  declared but never observed as an explicit zero column (`0` n, `0.0` %).
* `table_categorical()` keeps both the group and the margin when a `by`
  level is named `"Total"`: the margin is auto-renamed `"Total_1"` with a
  `spicy_renamed_column` warning, and `tidy()` / `glance()` drop the real
  margin, not the user's group.
* `table_categorical()` machine outputs (`"data.frame"`, `"long"`) carry
  full-precision values in grouped tables (`percent_digits = 2` renders
  `33.33`), and `output = "data.frame"` gains the documented `Chi2` and `df`
  columns the long output already had; displayed counts
  are integers everywhere, cells and `Total` margin under one rule.
* `table_categorical(correct = TRUE)` on a non-2x2 table warns once that
  Yates' correction is ignored, and `levels_keep` matching nothing warns
  (`spicy_no_selection`) with the available level strings instead of
  dropping the variable; for labelled columns the strings to match are the
  displayed `"[code] label"` levels, not the bare label text.
* `table_categorical()`, `table_continuous()`, and `table_continuous_lm()`
  resolve `by` data-first, like tidyselect: a column always wins over a
  same-named variable in the calling environment.
* `table_continuous_lm()` reports correct estimated means, SEs, CIs, and
  binary difference when `by` is an ordered factor; every `M` column was
  wrong. A categorical `by` now uses explicit treatment contrasts, so
  session-wide `options(contrasts = ...)` no longer alters the results.
* `table_continuous_lm()` computes correct `"balanced"` adjusted means when
  a covariate is an ordered factor, averages the `"proportional"`
  predictions with the case weights, and reports the omega-squared interval
  (not the eta-squared one) under `effect_size = "omega2"`.
* `table_continuous_lm()` treats a labelled `by` with value labels as
  categorical (groups over the raw codes; without value labels it stays
  continuous), fits cleanly when a factor
  covariate declares an unused level, accepts non-syntactic covariate names,
  and excludes `NA`-weight rows, disclosing dropped rows in the note.
* `table_continuous_lm()` errors when `by` has a single observed level and
  reports `NA` inference with a classed warning on a saturated fit; an
  outcome with too few observed groups degrades with a warning naming it,
  leaving the others intact.
* `table_continuous_lm()` discloses robust and resampling SEs in its table
  note, carries its notes into every rich output, and accepts
  `cluster = ~region`.
* `table_continuous()` forms groups from a non-factor `by` (character,
  numeric, labelled) in order of first appearance, matching
  `table_categorical()`; the test and effect size follow the displayed
  order.
* `table_continuous()` degrades per variable when a test or effect size
  fails on degenerate data: the affected cells become `NA` with a classed
  warning naming the variable, and the other variables keep their results.
* Average marginal effects now use the fit's prior weights: for a weighted
  `lm` / `glm` / `geeglm` fit the AME (with SE / CI / p) is the weighted
  average of the unit-level slopes, so AME values change for weighted fits.
  `svyglm` is unaffected.
* `table_regression()` partial effect sizes are now true Type-II tests:
  `partial_f2` / `partial_eta2` / `partial_omega2` (with CIs), the `glm`
  `partial_chi2`, and the mixed-model Wald chi-square change in models with
  interactions, where main effects no longer depend on the factor coding.
* `table_regression()` reports correct `partial_chi2` for `glm` fits created
  with `y = FALSE` and a matrix `cbind(successes, failures)` response: the
  internal refit multiplied the binomial totals into the weights twice,
  inflating every chi-square.
* Factor coefficient and AME rows follow `levels()` order (was
  alphabetical); ordered factors with AME columns show a reference row;
  `ame_ci` / `ame_p` / `ame_se` populate without the bare `"ame"` token;
  stars anchor on B (and AME), never on beta.
* Factors under non-default contrast codings (successive differences,
  sum-to-zero, Helmert, custom matrices) group under their parent variable,
  labelled by the contrast-matrix column names. No reference row is shown --
  none exists under those codings.
* Logical predictors get the grouped factor layout the documentation
  promises (an `is_smoker:` header with indented `FALSE (ref.)` / `TRUE`
  rows), and character predictors align their AME rows with the grouped
  levels.
* The statistic column header follows each model's actual reference
  distribution (`z` or `t`); it was hardcoded to `t`.
* Bootstrap / jackknife and `standardized = "refit"` refits no longer leak
  the caller's environment and now work on `factor()` / `log()` / `poly()`
  formulas; a failed refit falls back with a warning instead of silently
  changing method.
* In mixed-class tables, a fit statistic not defined for a model's class
  renders an en-dash in that model's cell (console and rich outputs); the
  blank cell was indistinguishable from "not requested". The first-column
  dash of the nested change statistics is unchanged.
* `show_fit_stats = "pseudo_r2_tjur"` is refused with a classed error when
  no model in the set is a binomial-family `glm`; the row was silently
  dropped.
* `table_regression()` returns carry the documented provenance attributes
  `outcome` and `model_ids`, `output = "data.frame"` carries the same pair,
  `as.data.frame()` no longer drops `col_spec`, and `output = "long"`
  returns the long tibble its documentation promised.
* `as_structured()` and the rich output engines match the console body
  exactly (blank vs en-dash reference cells, the multi-outcome `Outcome`
  row); the structured schema gains `outcome_labels_by_col`.
* Every cell whose statistic applies but has no number -- an aliased
  coefficient in a rank-deficient fit, a term an extractor returns without a
  standard error -- shows the console's en-dash in every rich output instead
  of a blank that reads as "nothing to report". The emission rule mirrors
  the console branch exactly, exemptions included.
* The standard error, confidence interval and p-value of a random-effect
  variance component show the same en-dash in `"gt"`, `"tinytable"`,
  `"flextable"`, `"word"`, `"excel"` and the clipboard as in the console.
  They were blank, and `re_columns` was ignored outside the console.
* When a factor's estimate blocks do not share a reference level -- an
  ordered factor with `show_columns = c("b", "ame")`, where the AME
  contrasts against a baseline while B holds polynomial trends -- the
  reference en-dash stays in the block that has one instead of blanking the
  B and p cells beside it.
* `output = "excel"` rules off `Thresholds:`, `Random effects:` and the
  other subordinate blocks, like every other output.
* `stars = TRUE` marks the coefficients in every output, not just the
  console: `output = "gt"`, `"tinytable"`, `"flextable"`, `"word"` and
  `"clipboard"` used to ship the legend footnote without a single marker in
  the table.
* An `output = "gt"` table keeps its note wherever it is rendered -- saved
  to a file, converted to HTML, or printed outside an interactive session.
  It reached the interactive display only, so a published table lost the
  model family, the standard errors, and the star legend.
* `table_regression(m1, m2)` without `list()` errors with a helpful message;
  colliding model labels no longer break `output = "gt"`; the `p_adjust`
  footer's family size matches the adjustment performed; and multi-model
  titles keep proper nouns capitalised.
* `ci_method = "profile"` with a robust `vcov` defers to the `vcov` and
  warns, and the singular-fit note states the fact, leaving the advice to a
  build-time warning.
* `gt` and `flextable` outputs now render in Quarto / R Markdown **Word**,
  PowerPoint, and PDF documents, where they silently disappeared. A new
  `as_flextable()` method returns the underlying flextable for manual
  composition.
* The flextable outputs of `table_categorical()`, `table_continuous()`, and
  `table_continuous_lm()` carry the same `spicy_flextable` wrapper as
  `table_regression()`, so notes and knit-time rendering behave identically.
* `varlist()`, `vl()`, and `code_book()` render `POSIXlt` columns as
  datetime values under `values = TRUE`, and show an explicit `NA` factor
  level as `<NA>` in `Values` instead of dropping it.
* `label_from_names()` no longer blames the split for duplicate column names
  that already existed in the input (`check.names = FALSE` data).
* `count_n()` resolves `select` and `exclude` through the same tidyselect
  path as `mean_n()` and `sum_n()` (`exclude` now takes positions as well as
  names), and raises a classed error (`spicy_invalid_input`) when `count` is
  zero-length or all-missing, when `special` is empty, and for a typo
  supplied alongside `special = "all"`. A rejected `count = NaN` now points
  to `special = "NaN"` rather than to `special = "NA"`, which counts both.
* The tabulating and summarising functions reject `bit64::integer64` input
  with a classed error (`spicy_invalid_data`) naming the fix: convert with
  `as.integer()` / `as.numeric()`, or `as.character()` for codes wider than
  2^53. `count_n()` still works.
* `table_regression()` on a `survey::svycoxph()` fit gives a clear refusal
  instead of failing with `No AIC for survey models` after printing six
  lines of design description. Design-based Cox models are not supported
  yet; `summary(fit)` and `survey::regTermTest()` cover them meanwhile.
## Minor improvements

* A one-way `table_continuous()` (no `by`) draws no rule between its
  variable blocks in the rich engines (tinytable, gt, flextable, Word,
  Excel), matching the console: each block is a single row, so the rule
  carried no information. A by-group table still rules between variables.
* `spicy_print_table()` gains `qualify_companions`. When a width split
  pushes an `SE`, a `p` or an interval onto a continuation panel, this
  says whether that column should name the estimate it belongs to
  (`95% CI` becoming `95% CI (B)`). It is `FALSE` by default, because a
  `p` that tests a whole block belongs to no column and naming one for
  it would say something false about what was compared; the coefficient
  tables, where the neighbour on the left really is the carrier, ask
  for it.
* `as_structured()` no longer carries a `measure` field in the
  `col_meta` of a `table_categorical()` association column. It was
  undocumented and its value was the name of the element holding it
  (`col_meta$"Effect size"$measure` was `"Effect size"`); the column's
  token and `display_label` are unchanged.
* Error messages quote a value the same way on every platform: `"value"`,
  with double quotes, on Windows, macOS and Linux alike. The messages used
  shell quoting, which renders `'value'` on Unix, so an error read
  differently depending on where it was raised. A backslash in the value
  also reaches the reader now: `keep = "\\bnope\\b"` used to be reported
  back as `"nope"`, a pattern nobody had written.
* A cell with no number -- a statistic that does not apply to the row, a
  reference level -- prints an en dash (`–`) in every table.
  `table_continuous()`, `assoc_measures()` and the association printers
  used `--`; `table_regression()` already used the en dash. The typed
  view of `as_structured()` carries it in `display_cells`. Where that
  placeholder was what set a column's width, the column tightens by one
  character.
* The coverage percentage of an interval header follows `decimal_mark`:
  at `ci_level = 0.975` with `decimal_mark = ","` the spanner, the
  column headers, the CI notes and `inline()`'s `{ci_label}` read
  `97,5% CI` (and `97·5% CI` under the Lancet style) in all four table
  families and every engine. Integer coverages (90, 95, 99) and the
  default period are byte-identical, and the frozen column names of the
  descriptive families keep the period (`97.5% CI LL`). In the
  regression family the header is the column's programmatic name, so
  at a fractional coverage under a non-default mark the
  `as_structured()` and `data.frame` column names move with it --
  code that must not depend on `decimal_mark` should read
  `broom::tidy()` or `output = "long"`, whose names never change.
* `excel_sheet` defaults to `NULL` in the four table functions and
  resolves to the same sheet names as before (`"Regression"`,
  `"Categorical"`, `"Descriptives"`, `"Linear models"`). Behaviour is
  unchanged; an explicit name still wins.
* `table_continuous(by = )` titles the table `Descriptive statistics by
  <label>`: the grouping variable is stated in the console header and in
  every rendered caption, like the other `by` tables of the family.
* The `Weighted n` column of `table_continuous()` right-aligns with `n`
  instead of being centred, matching `table_continuous_lm()`. Visible in
  Excel workbooks at every `align` value.
* `cross_tab()` discloses excluded missing values in the table note -- per
  variable, with a deduplicated row total, and rows dropped for a missing
  `by` -- instead of dropping them silently.
* `cross_tab()` accepts logical weights, coerced to 1/0 like `freq()`; warns
  (`spicy_ignored_arg`) on a third positional argument in vector mode; and
  validates `digits` with the same classed error as `freq()`, in the
  function and its `print()` method.
* Invalid values for the enum arguments (`output`, `align`, `percent`,
  `assoc_measure`, `direction`, `method`, ...) raise a classed
  `spicy_invalid_input` error naming the argument and its valid values, and
  `freq()`'s `sort` error lists `""` (no sorting).
* `copy_clipboard()` re-emits clipboard backend messages and warnings as
  real R conditions, and signals the "`row_names_as_col` has no effect"
  notice as a classed warning (`spicy_ignored_arg`); its invisible return
  value is the object actually sent to the clipboard.
* `table_categorical()`, `table_continuous()`, and `table_continuous_lm()`
  announce their clipboard export with a classed message (`spicy_info`), so
  it can be muffled like every other spicy signal.
* `spicy_print_table()` raises classed errors when `x` is not a data frame
  or `display_labels` does not have one label per column, and
  `table_regression()`'s internal invariant check warns with a classed
  condition (`spicy_internal_invariant`).
* Wide multi-model tables split into stacked panels more cleanly:
  continuation panels carry no empty stub rows, and over-wide column
  spanners truncate with a visible ellipsis.
* Under `exponentiate = TRUE` with a visible SE column, the footer states
  the SE scale (delta method) and that the CI bounds are asymmetric.
* Placeholder cells decimal-align in the `gt` / `flextable` / `tinytable` /
  Word / Excel outputs; `"deviance"` prints at 1 decimal; the descriptive
  tables use a single font in Word outputs.
* `table_continuous()`'s "`test` is ignored" warning states the full trigger
  condition (`p_value`, `statistic`, `effect_size`, and `effect_size_ci` all
  turned off) instead of naming only the first two toggles.
* `varlist()`, `vl()`, and `code_book()` annotate `difftime` values with
  their units in `Values` (e.g. `1.5, 2.5 (hours)`), and `varlist()` /
  `vl()` return tibble columns without stray names attributes.

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
