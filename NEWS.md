# spicy (development version)

## Breaking changes

* `table_regression()` and `table_regression_uv()` test the terms of a
  `glm()` whose dispersion is estimated (gaussian, Gamma, inverse
  Gaussian, Tweedie, `negative.binomial(theta)`, the quasi families)
  with the partial F test, in the new column `partial_f`: the deviance
  change over its degrees of freedom and over the dispersion
  `summary()` estimates, on the residual degrees of freedom, the F that
  `anova(reduced, full, test = "F")` and `car::Anova(type = 2,
  test.statistic = "F", error.estimate = "dispersion")` report.
  `partial_chi2` is refused for these models. It gave a likelihood-ratio
  chi-square scaled by the dispersion, and nothing for the quasi
  families, which are now tested. Binomial, Poisson and `MASS::glm.nb()`
  fits keep `partial_chi2`, unchanged. A table that mixes both kinds of
  model asks for both columns: asking for one alone is refused rather
  than leaving a model without its test.

* `table_regression()`, `table_regression_uv()` and `table_stack()` test a
  `glm()` whose dispersion is estimated (gaussian, Gamma, inverse Gaussian,
  Tweedie and the quasi families) as `summary()` tests it: a t statistic
  on the residual degrees of freedom instead of a z statistic. The
  p-values, the Wald confidence intervals, the statistic header, the
  standardized coefficients and the average marginal effects follow. With
  `nested = TRUE`, the change test of such a pair is the F test `anova()`
  reports (`f_change`; `lrt_change` is refused), and under an `HC*` vcov a
  Wald F-change. `feglm()` fits of these families follow their `glm()`
  twins. Binomial, Poisson and `MASS::glm.nb()` fits keep their z test and
  likelihood-ratio test.

* `table_regression()`: in `tidy()` and `output = "long"`, the terms of
  the zero-inflation, zero-hurdle and dispersion rows are keyed by their
  block, so that the zero parts of a `pscl` and a `glmmTMB` fit of the
  same model share their rows: `"Zero-inflation::(Intercept)"` and
  `"Zero hurdle::ment"` instead of `"zero_(Intercept)"` and `"zero_ment"`
  (`pscl`), `"Zero-inflation::(Intercept)"` and
  `"Dispersion::(Intercept)"` instead of `"zi.(Intercept)"` and
  `"disp.(Intercept)"` (zero-inflated `glmmTMB` models). The zero part of
  a glmmTMB hurdle model (a zero-truncated family, `ziGamma()`,
  `lognormal()` or `beta_family()` with `ziformula`) is the block
  `"Zero hurdle, Pr(y = 0)"` in `inline()`, `as.data.frame()` and
  `tidy()`; it was `"Zero-inflation"`.

* `table_regression()`: the terms of an rms model (`ols()`, `lrm()`,
  `cph()`, `Glm()`) are spelled as in `lm()`: `"cyl6"`, not rms's
  `"cyl=6"`, `"age:sexmale"`, not `"age * sex=male"`, and a logical
  predictor is `"youngTRUE"` under a `FALSE (ref.)` row. This is the
  name of the row in `tidy()`, `output = "long"` and
  `factor_layout = "flat"`, and the name `keep`, `drop` and `labels`
  take. An rms term now shares its row with its AME and with the same
  term of an `lm()`, `glm()`, `polr()` or `coxph()` model in the table,
  and an interaction is a row of its own, no longer a level under its
  first factor.

* `table_regression()`: the cut-points of an ordinal `rms::lrm()` model
  are named and signed as those of `MASS::polr()` in `tidy()` and
  `output = "long"`: rms's `"y>=4"` becomes `"3|4"`, with the sign
  reversed.

* `table_regression()` shows a logical predictor of a fixest model as
  `lm()` does ("FALSE (ref.)", then "TRUE") instead of one "xTRUE" row,
  and likewise a `factor()` term or a factor or character predictor
  beside a transformed term or `i()`: a parent row, then one row per
  level, the reference first.

* `table_regression()` no longer turns on the R-squared, within
  R-squared and AIC rows for an IV `feols()` model, as for `ivreg()` and
  `iv_robust()`: the 2SLS R-squared is not the classical one and 2SLS
  is not a likelihood fit. Asked for, or turned on by another model of
  the table, the R-squared is the one `ivreg()` and `iv_robust()`
  report, computed on the structural residuals, and the within
  R-squared is blank. The first stage keeps its R-squared.

* `table_regression()` adds "fixed effects" to the title of an
  `lm_robust()` or `iv_robust()` model with `fixed_effects`, as for
  fixest ("Linear regression (robust SE, fixed effects)"). Beside a
  fixest model with fixed effects, the table takes their common title
  ("Linear regression (fixed effects) comparison: y").

* `table_regression()` refuses `vcov = "CR1"` for negative-binomial
  `pscl::zeroinfl()` and `pscl::hurdle()` models (`dist = "negbin"` or
  `zero.dist = "negbin"`), whose sandwich leaves out the score of theta
  and is not the full sandwich of the fit. When the count part is
  negative binomial, the error points to glmmTMB, whose sandwich
  includes theta and whose zero hurdle is binomial. When only the zero
  hurdle is negative binomial, it points to the default binomial zero
  hurdle. Poisson and geometric models keep `"CR1"`.

* `table_regression(nested = TRUE)` signs the same change rows in every
  output: differences between the current and the previous model (ΔR²,
  ΔAIC, ΔBIC, ...) carry a `+` or `-` in gt, tinytable, flextable, Word,
  Excel, the clipboard and `inline()` as in the console, and test
  statistics (F-change, χ², Wald) and the deviance drop are no longer
  signed in the console. Those outputs also print the F-change with
  `digits` and the Deviance row and its change with `ic_digits`, as the
  console does.

* `table_continuous()`, `table_continuous_svy()` and `table_outcome()`
  show a `show_columns` vector in the order it is written:
  `c("med", "q1", "q3", "m", "sd", "n")` puts the median first. A token
  written twice counts once instead of being an error. An interval
  written before its statistic (`"ci"` before `"m"`, `"med_ci"` before
  `"med"` or `"med_iqr"`) is an error. The per-variable list form keeps
  the canonical order.

* `table_categorical()`, `table_continuous()`, `table_continuous_lm()`
  and `table_outcome()` leave the rows of zero weight out of the sample
  size of the note, as `nobs()` does. So do the `n` column of
  `table_continuous_lm()` and the `nobs` of its `glance()`. Weighted
  statistics do not change.

* Every table with `by` refuses a level of `by` that is the empty
  string. `table_categorical()` showed 0 for that group, and the survey
  tables failed.

* `table_regression()`: a `glmmTMB` beta, ordered beta or beta-binomial
  mixed model no longer reports the Nakagawa R2. The residual variance
  it relied on for these families is that of a log link, not of their
  logit link. The R2 of a beta model changed when the outcome was
  recoded 1 - y, and that of a beta-binomial model went to 1 as the
  overdispersion vanished instead of joining that of the binomial model.

## New features

* `table_categorical()`, `table_categorical_svy()`,
  `table_multiresponse()`, `table_battery()` and `table_battery_svy()`
  print the groups of `by` in the console as the rich outputs do: the
  label of each group sits over its columns, headed `n`, `%`, instead
  of `Female n`, `Female %`. A table split into panels breaks between
  groups. The column names of `as.data.frame()`, `tidy()`,
  `as_structured()` and the rendered outputs do not change.

* `table_regression()` gains `ame_type`. `"slope"` (default, unchanged)
  reports the average slope dY/dX of a continuous predictor, `"unit"`
  the average change in the prediction for a one-unit increase from the
  observed value, `"sd"` the same for an increase of one standard
  deviation. Factors, logicals and 0/1 predictors stay contrasts with
  their reference level. The note names the quantity, under the default
  too.

* `table_regression()` with `exponentiate = TRUE`: the note says what a
  displayed intercept estimates, since it is not a ratio: the baseline
  odds, risk, rate or mean, or a quantile of the survival time for
  `survreg()`. Zero-inflation and hurdle blocks on a logit link name
  theirs.

* `table_battery()` and `table_battery_svy()` gain `tidy()` and
  `glance()` methods.

* `as_structured()` of `table_continuous_lm()` gives each mean column
  the header printed over the levels, in `col_meta$<col>$spanner_label`.

* `table_regression()` gains `partial_f`, the Type II partial F test of
  each term, for `lm()` (the F of `car::Anova(type = 2)`) and for a
  `glm()` whose dispersion is estimated. The cell reads "F (df1, df2)",
  the term and residual degrees of freedom. `tidy()` and the long output
  carry them in `df` and `df.residual`. A gaussian `glm()` and its
  `lm()` twin print the same F.

* New `table_battery()` tabulates a battery of items rated on one scale
  (a Likert grid): one row per item, one column per level, row
  percentages and their base `n`. `levels` sets the scale and keeps the
  other answers out of the base, `top` adds a column summing some
  levels, `sort` orders the items (`"top-"`, `"name+"`, `"<level>-"`),
  and `drop_na = FALSE` adds a column of missing answers. With `by`,
  each group gets a block of columns and each item a chi-squared or
  Fisher's exact test, Holm-adjusted by default. `inline()` cites its
  cells (#13).

* New `table_battery_svy()`, the survey-design twin of `table_battery()`:
  the same table from a `survey::svydesign()` or replicate-weights
  design. The percentages are estimated under the design on each item's
  respondents, `n` is their observed count and, with `by`, each item
  gets a design-based test (`chisq_statistic`, Rao-Scott by default),
  Holm-adjusted by default. `table_battery()` points a design to it.
  A cell the design cannot estimate shows a dash, with a warning and a
  note sentence, as in `table_categorical_svy()`.

* `table_categorical()` and `table_categorical_svy()` gain
  `factor_levels`, as in `freq()`: `"all"` shows the declared levels
  with no observation, at 0 in every column. These rows change no
  percentage of the other rows and no test (#12).

* New `table_stack()`, experimental, stacks regression tables one under
  the other: each table becomes a panel opened by a row with its name
  (or its entry in `panel_labels`), under one column header when the
  panels share their columns and with its own header otherwise. The
  notes are merged as for the models side by side, a sentence that
  holds for some panels only opening with their names. Tables built in
  different languages, with different decimal marks or alignments, or
  under styles that format a cell differently are refused.

* `table_regression()` gains `layout = "stacked"`: one panel per model,
  each with the columns of the model alone, its confidence interval
  included. Under `nested = TRUE` the change statistics of each model
  sit in its panel, labeled "vs" the model before it. `tidy()`,
  `glance()` and `output = "long"` return the rows of the models side
  by side, with a leading `panel` column.

* `table_regression()`: glmmTMB models take `vcov = "CR1"`, glmmTMB's
  cluster sandwich scaled by G/(G - 1) for G clusters, as for the other
  likelihood classes. It covers the coefficients, the zero-inflation
  and dispersion blocks, the joint tests, the AME and the
  `nested = TRUE` Wald test, with Wald z as under `"CR0"`.

* `table_regression()`: `show_fit_stats = "delta"` shows the NB1
  dispersion δ of glmmTMB `nbinom1` models (variance μ(1 + δ)).
  `"theta"` and `"alpha"` now also work for `lme4::glmer.nb()`,
  `fixest::fenegbin()`, mgcv `nb()`, glmmTMB `nbinom2` models and
  negative-binomial `pscl::zeroinfl()` and `pscl::hurdle()` models
  (the theta of the count part), and `"phi"` for mgcv `betar()` and
  glmmTMB `beta_family()`, `betabinomial()` and `ordbeta()` models.
  These rows accept a table that also holds models without the
  parameter (Poisson next to negative binomial, NB1 next to NB2), whose
  cells stay blank. A theta or phi fixed in the call
  (`negative.binomial(theta)` in `glm()`, mgcv `nb(theta)` or
  `betar(theta)`) has no row. A negative-binomial `lme4::glmer()`
  always shows its theta.

* Word (flextable), gt (HTML, Word and RTF) and tinytable HTML tables
  now write a negative number with the minus sign (U+2212), in the cells
  and in the numbers of the note, as Typst and LaTeX already did. The
  data frame, the console, Excel, the clipboard and markdown keep the
  hyphen, as does `tinytable::save_tt()` to `.docx`, which writes
  markdown (use `output = "word"` for Word).
  `spicy_style(minus_sign = "ascii")` keeps it everywhere.

* French titles (`options(spicy.language = "fr")`) now cover every model
  that `table_regression()` and `table_regression_uv()` accept, quantile
  regressions included ("Régression quantile (τ = 0,50)"). The titles
  spell out "(risques proportionnels)", "(doubles moindres carrés)" and
  "(t de Student)", the univariable Cox title reads "Régression de Cox
  univariable à risques proportionnels", the type note of several GAMs
  reads "GAM de Poisson.", and a family the titles do not know keeps a
  French title ("Régression univariable et multivariable"). The survival
  note names the distribution in French ("Distribution : log-normale")
  and the standard errors of `estimatr` and fixest models are
  translated ("robustes à l'hétéroscédasticité (HC2)", "robustes aux grappes
  (id)", "Newey-West (L = 2)").

## Improvements and bug fixes

* In the rich outputs (tinytable, gt, flextable, Word, Excel, clipboard)
  a term-level test is one column, as in the console: "F" with
  "18.08 (1, 28)", "χ²" with "4.52 (2)". They used to split it into
  columns that all carried the statistic's header ("χ² | χ²"). Excel
  keeps the statistic as a number with the degrees of freedom in its
  number format and writes no df column. `output = "data.frame"`,
  `as_structured()` and `tidy()` keep the separate numeric columns.

* The survey-design tables say survey's own warnings once. Under
  `options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely =
  TRUE)`, a stratum lonely within a domain made survey warn at every
  estimate, up to six times for one table. Each distinct warning is now
  raised once per table, with the number of times survey raised it. The
  warning that a cell could not be estimated (class
  `spicy_svy_estimation_failed`) carries the call of the table function,
  so its header names the table.

* The model-change rows of `table_regression()` (F-change, Wald
  F-change, the LR χ² change, Wald χ² (change), "F-change vs Model k"
  in a stacked hierarchy) show their degrees of freedom in the cell,
  "12.38 (1, 29)" for an F and "9.12 (1)" for a chi-square, as the
  partial tests do. The rich outputs print the same cell, Excel keeps
  the number with a format that displays the df, and `output =
  "data.frame"` keeps the number. A non-integer df (a CR2 denominator,
  a gam) prints with one decimal.

* `table_regression()` refuses the HC variances (`vcov = "HC0"` to
  `"HC5"`) for an `lm()` or `glm()` fitted with zero prior weights.
  `sandwich::vcovHC()` counts those rows in n while the hat values skip
  them, so every HC standard error was off (HC0 by n_nonzero/n, HC2 to
  HC5 by more). The error says to drop the rows and refit.

* `table_categorical_svy()` and `table_continuous_svy()` say why a cell
  shows a dash. Under the default `options(survey.lonely.psu = "fail")`,
  a stratum with a single primary sampling unit stops survey, and the
  table showed dashes without a word. The call now warns once, quoting
  survey's messages (class `spicy_svy_estimation_failed`), and the note
  says "A dash marks an estimate the design could not provide: a stratum
  has a single primary sampling unit." Any other failure on a shown cell
  gets "A dash marks an estimate the design could not provide."

* `table_categorical_svy(proportion_ci = TRUE)` under `ci_method = "logit"`
  or `"likelihood"` leaves the percentage CIs of a domain whose weights
  change sign blank, warns (class `spicy_negative_weights_no_ci`) naming
  the five methods that accept negative weights, and says so in the
  note. The CIs were dashes with no explanation.

* `table_categorical_svy(proportion_ci = TRUE, drop_na = TRUE)` gives the
  percentage CIs of a variable with missing values on a calibrated or
  post-stratified design. Every interval was a dash, under every
  `ci_method`.

* `table_regression(standardized = "refit")` on a `MASS::glm.nb()` fit
  tests the standardized coefficients at a dispersion of 1, as the fit
  itself is tested. Their standard errors were scaled by the Pearson
  dispersion of the refit, and their p-values could be far too small.

* `cross_tab()` writes the chi-squared statistic of its note with the
  symbol, `χ²(df) = …` (was `Chi-2(df) = …`).

* `table_multiresponse()` labels the row of respondents who left the
  question blank "(No answer to the question)", as its note says (was
  "(No data for the question)").

* `table_regression()` names the ratio of an ordinal cloglog model "the
  grouped-time hazard ratio" in its note (was "proportional-hazards
  ratio").

* Under the `jama` and `lancet` themes, a French table spells out
  "Moyenne" and "Médiane" as an English one spells out "Mean" and
  "Median".

* Table notes name the statistical method, never the package that
  computes it: "P-values: Satterthwaite t-test." instead of "...
  (lmerTest).", "Percentage CIs: Wilson score." instead of "wilson
  (survey::svyciprop).", "Quantiles: Hyndman-Fan type 1." instead of
  `qrule = "math"`. The hint to refit a `lme4::lmer()` model with
  lmerTest is now a message, shown once per session, and the remedies
  for unreliable PSIS-LOO and WAIC values moved from the note to their
  warnings. Table titles no longer carry the engine either: "Linear
  mixed-effects regression" instead of "... (glmmTMB)", likewise for
  nlme, rms, mlogit, rstanarm and brms models.

* `table_regression()`: glmmTMB titles name every family, e.g.
  "Negative-binomial (NB1) mixed-effects regression" for `nbinom1` or
  "Conway-Maxwell-Poisson mixed-effects regression" for `compois`, and
  name a two-part model in the words `pscl` uses: "Zero-inflated Poisson
  mixed-effects regression" and "Poisson hurdle mixed-effects regression"
  instead of "Poisson mixed-effects regression (glmmTMB) (zero-inflated)"
  and "Truncated_poisson mixed-effects regression (glmmTMB)
  (zero-inflated)". A model with `ziformula` and a family without zeros
  (`ziGamma()`, `lognormal()`, `beta_family()`) is titled as a hurdle
  model ("Gamma hurdle regression"), and a model without any
  random-effect term takes the fixed-effects title of its family
  ("Poisson regression"). A `pscl` and a `glmmTMB` fit of the same model
  in one table share its title. The random-effects test names its null
  model as the title does instead of the engine's family code, e.g.
  "negative-binomial regression (θ = 1.71)", "zero-inflated Poisson
  regression" or "cumulative complementary log-log regression"; when the
  zero-inflation or dispersion formula has a random term, which the null
  keeps, the line gives the test without naming its null.

* `table_regression()`: the zero part of a hurdle model is labeled by the
  event it models: "Zero hurdle, Pr(y > 0)" for `pscl::hurdle()`, and
  "Zero hurdle, Pr(y = 0)" for a glmmTMB hurdle model. The glmmTMB zero
  part was shown as "Zero-inflation" and read as the log-odds of a
  structural (excess) zero; it is the log-odds of a zero. The `pscl`
  block keeps its name, `"Zero hurdle"`, in `inline()`, `tidy()` and
  `as.data.frame()`. A zero-inflated and a hurdle model in one table show
  each zero part in its own block only (both were printed under both
  blocks), the zero-inflation parts of a `pscl` and a `glmmTMB` fit of
  the same model share their rows, and when models share a block but not
  its note line, as two `pscl::hurdle()` fits with different `zero.dist`
  do, the note gives each model's line instead of the first model's
  alone. With `reference_style = "footer"`, the reference level of such a
  block is written after the block's caption ("Zero hurdle, Pr(y > 0):
  fem: Men") instead of "Zero hurdle = fem: Men", and the caption is
  translated in French. In French tables the component rows display the
  French colon ("mined : no") as the main block does, and so does the
  reference line of the note; their names in `inline()` and `tidy()` do
  not change with the language.

* `table_regression()`: the random-effects test of an `ordinal::clmm()`
  model with the cloglog or loglog link and a single random intercept
  compared two models that are not nested, and printed a wrong
  chi-bar-squared and p-value: `clmm()` fits the loglog model there when
  asked for cloglog, and the reverse. The null model is now refitted with
  the link actually fitted. `exponentiate = TRUE` is refused only for
  such a fit: a cloglog `clmm()` with other random effects reports hazard
  ratios and is titled "(proportional hazards)", as `clm()` is.

* `table_regression()` titles name the model in words, never with an R
  code or a fitted number: "Negative-binomial GAM" instead of "Negative
  Binomial(1.597) GAM", "Bayesian negative-binomial regression" instead
  of "Bayesian neg_binomial_2 regression", "Log-normal AFT regression"
  for `dist = "loggaussian"`. They follow the family and the link: a
  probit GAM or `rms::Glm()` model is no longer titled logistic, nor a
  survey Poisson model with an identity link "linear", and an ordinal
  `rms::lrm()` model is titled as the proportional-odds model it is.
  Ordinal titles spell the link out ("Cumulative complementary log-log
  regression (proportional hazards)"), pscl models read "Zero-inflated
  Poisson regression", family names follow sentence case ("Bayesian
  gamma regression"), and the survival notes name the distribution the
  same way ("Distribution: Extreme value"). mgcv's `cox.ph()`, `cnorm()`,
  `clog()`, `cpois()`, `bcg()` and `gfam()` GAMs are titled "Cox
  proportional hazards GAM", "Censored normal GAM", "Censored logistic
  GAM", "Censored Poisson GAM", "Censored Box-Cox normal GAM" and
  "Grouped-family GAM" instead of "Cox PH GAM", "Cnorm(1.338) GAM" and
  the like; a family from outside mgcv keeps the name its author gave it
  ("Generalized additive model (GAM, myfam)"). The note of several GAMs
  or Heckman models of one type names the model once: "Poisson GAMs.",
  "Heckman selection models." instead of "Poisson GAM models." and
  "... model models.".

* `table_regression()`: a `survival::survreg()` model with the
  extreme-value distribution models the time itself, as the Gaussian,
  logistic and t distributions do, so `exponentiate = TRUE` no longer
  turns its coefficients into time ratios, and the message says why.
  These four are titled "Censored regression (Gaussian)", "(logistic)",
  "(extreme value)" and "(Student t)" instead of "Gaussian AFT
  regression", "Extreme AFT regression" and the like, and the note spells
  the distribution as the title does ("Distribution: Extreme value",
  "Student t" instead of "Extreme", "Student-t"). A distribution passed
  to `survreg()` as a list no longer fails. The parameters of a flexsurv
  distribution keep two significant digits in the note ("rate = 0.00079"
  instead of "rate = 0.00").

* `table_regression()`: a `mgcv::gam(family = cox.ph())` model shows
  hazard ratios under `exponentiate = TRUE`, as `survival::coxph()` does.
  It warned about an identity link and showed the coefficients.

* `table_regression()`: a `glm()` with the Tweedie family of `statmod` or
  `mgcv` is titled "Tweedie regression", as `table_regression_uv()`
  titles it, instead of "Regression".

* `table_regression()`: an `estimatr` model (`lm_robust()`,
  `iv_robust()`) with `clusters` and `se_type = "stata"` is labeled
  "Cluster-robust (Stata CR1)" instead of "Robust (Stata HC1)".

* `table_regression()` refuses with a clear error an mgcv model fitted
  from a list of formulas (`gaulss()`, `ziplss()`, `twlss()`,
  `multinom()`, `mvn()`, ...). It failed with an internal error about the
  response name.

* `table_regression()`: a fixest model without fixed effects is titled
  as `lm()` and `glm()` title the same model ("Linear regression",
  "Poisson regression"), an IV `feols()` model as `ivreg()` titles it
  ("IV regression (2SLS)"), and "(fixed effects)" appears only in the
  title of a model that has fixed effects. A table of pooled and
  fixed-effects models of one family takes the family's title ("Linear
  regression comparison: y"), in French too, and its "Fixed effects"
  rows show which model absorbs which.

* `table_regression()`: `fixest::feglm()` models with a Gaussian, Gamma
  or quasi family no longer fail, and their t tests are read as fixest
  reports them. fixest titles follow the link ("Probit regression (fixed
  effects)") and no longer show R's family code ("Quasipoisson").

* `table_regression()`: `fixest::femlm()` Poisson, logit and Gaussian
  models no longer fail with "subscript out of bounds". They are read as
  the maximum-likelihood fits they are, with fixest's z tests and a
  title that names their family.

* `table_regression()`: fixest models tested with t (`feols()`, and
  `feglm()` with a family whose dispersion is estimated: Gaussian,
  Gamma, inverse Gaussian or quasi) take the degrees of freedom of
  fixest's own p-values for their confidence intervals, as `confint()`
  does, and for their AME. Under clustered standard errors the
  intervals used the residual degrees of freedom and the AME the normal
  distribution, so both were too narrow and could disagree with the
  p-value.

* `table_regression()`: the AME of a fixest model sits on the row of
  its coefficient, as for `lm()`: on each level of a factor or logical,
  on the coefficient of a transformed variable (`log(x)`, `factor(x)`),
  on the endogenous regressor of an IV `feols()` model, also for a model
  fitted with `lean = TRUE` without fixed effects. Only one contrast per
  factor used to appear, on a row of its own. The AME of a factor in
  `i()` is contrasted with the reference `i()` gives its coefficients;
  with `i(x, ref = )` it used to be contrasted with the first level of
  `x`, a reference the table did not show. A level that `i()` leaves
  without a coefficient row, and a factor that `i()` enters only in an
  interaction, get no AME, with a warning; a coefficient that fixest
  drops for collinearity, and the response of an IV model with a
  transformed endogenous regressor, no longer show an AME of 0.

* `table_regression()`: the AME of a weighted fixest model with rows
  dropped for missing values is weighted. It used to be unweighted.

* `table_regression()` refuses an AME on a fixest GLM with fixed effects
  (`feglm()`, `fepois()`, `femlm()`, `fenegbin()`, whatever the family),
  as it does for the other models that have none: marginaleffects,
  which computes it, cannot take into account the uncertainty of the
  fixed effects. It refuses it too for a `feols()` model with fixed
  effects fitted with `lean = TRUE`, which marginaleffects cannot
  predict from, and says to re-estimate it with `lean = FALSE`. It used
  to warn and leave the column empty. Other `feols()` models and a GLM
  without fixed effects keep their AME.

* `table_regression()` refuses a fixest model with fixed effects only
  (`y ~ 1 | f`), which has no coefficient to show, with an error that
  names the model as the list names it, in a stacked table too. It used
  to fail with a base R error.

* `table_regression()` names a cluster on combined fixed effects as it
  was written ("Clustered (id^period)") instead of fixest's internal
  function.

* `table_regression()`: an ordinal `rms::lrm()` model shows its
  cut-points in the "Thresholds" block, as `MASS::polr()` does, instead
  of as coefficients that `exponentiate = TRUE` turned into odds ratios.
  Its AME are effects on the probability of each outcome category, as
  for `MASS::polr()` ("AME Poor", "AME Fair", ...); they were effects on
  the cumulative probabilities P(Y >= j), under a note that said
  otherwise.

* `table_regression()`: an `rms::lrm()` model reports the McFadden and
  Nagelkerke R² and the residual deviance, with the values `glm()` and
  `MASS::polr()` give the same model, so its default fit rows are
  theirs. They used to be dashed beside a `glm()` or `polr()` fit, and
  absent when asked for in `show_fit_stats`.

* `table_regression()`: an `rms::Glm()` model reports the AIC, BIC,
  log-likelihood and McFadden and Nagelkerke R² of its `glm()` twin. Its
  AIC and log-likelihood were wrong for every family but the binomial,
  its BIC was missing and its R² were dashed. The BIC of an `rms::cph()`
  model counts the events, as for `survival::coxph()` (it counted the
  subjects), and an `rms::ols()` model reports the residual sum of
  squares of `lm()` as its deviance (it was dashed).

* `table_regression()`: the AME of an `rms::lrm()` or `MASS::polr()`
  model with an offset include the offset. They were computed at offset
  0, because the `predict()` of these models leaves the offset out. An
  `rms::ols()` model no longer shows an AME of 0 for its offset.

* `table_regression()`: under `vcov = "CR0"`, the AME of a binary
  `rms::lrm()`, an `rms::ols()` and an `rms::Glm()` model take the
  cluster-robust standard errors of the coefficients. The `lrm()` and
  `ols()` AME fell back to the model-based ones with a warning, and a
  `Glm()` table failed.

* `table_regression()`: the null model of the pseudo-R² of a
  `MASS::polr()` or `ordinal::clm()` model keeps the offset, as for
  `glm()` and `rms::lrm()`, and keeps the thresholds of a `clm()` fitted
  with `threshold = "equidistant"` or `"symmetric"`. It left the offset
  out and had flexible thresholds.

* `table_regression()`: the AME of an `mgcv::gam()` model are computed
  on the data it was fitted to (with marginaleffects 1.0.0 or later). A
  model fitted inside a function took its AME over a global object of
  the same name as its data, when there was one.

* `table_regression()`: with `p_adjust`, the note of an ordinal model
  counts only the coefficients that were adjusted (it counted the
  cut-points, and the scale effects of a `clm()`, which are never
  adjusted), and with the AME p-values shown (`"ame_p"`) it counts the
  tests the AME p-values were adjusted for: one per category and
  predictor for an ordinal or multinomial model. It gave the number of
  coefficients. When some models of the table have no test of a family
  (no AME, or an intercept-only model), the note names the models it
  counts, in a stacked table too.

* `table_regression()`: under `decimal_mark = ","`, the cut-points
  listed in the note (`show_thresholds = FALSE`) are separated by
  semicolons, which cannot be read as decimal commas.

* `table_regression()` no longer shows an AME row for a variable that
  enters the model only through `offset()` (`offset(log(exposure))`,
  for example): an offset has no fitted effect. The row showed the
  slope of the offset variable, with an SE and a p-value.

* `table_regression()` with a formula `cluster` (`cluster = ~id`) uses
  the rows the fit kept after `subset` and the removal of missing
  values. A fit with `subset` failed on the length of the cluster.

* `table_categorical()` and `table_categorical_svy()` show a declared
  level that `levels_keep` names when it has no observation, with a
  count of 0. `table_categorical()` dropped it silently from a vector
  (the variable, with a warning, when it was the only level kept) and
  refused it in a named list; `table_categorical_svy()` refused it in
  both forms (#12).

* `table_categorical()`, `table_continuous()` and `table_continuous_lm()`
  no longer merge the empty header cells of their Word and flextable
  outputs into one cell.

* `table_regression()`: the AIC and BIC of a fixest model are those of
  its `lm()`, `glm()` or `MASS::glm.nb()` twin. A `feols()` model counts
  its residual variance, a Gaussian, Gamma or inverse-Gaussian `feglm()`
  its dispersion and a `fenegbin()` its theta, which fixest's own
  `AIC()` leaves out. With `nested = TRUE`, the likelihood-ratio test of
  two `feglm()` models is the test `anova()` gives their `glm()` twins,
  for a quasi-Poisson or quasi-binomial pair too.

* `table_regression()`: with `nested = TRUE`, the AICc row of a model
  without an AICc of its own (fixest, Cox, ordinal and other likelihood
  models) showed the change of AICc. It shows the AICc, and the change
  of AICc counts the parameters the AIC counts (it counted one too many
  for a `glm()` without a dispersion).

* `table_regression()`: `labels` keyed by the term of an interaction
  (`"age:wc"`) labels its row (`"age:wcyes"`) when the term has one row.
  A term with several rows warns and lists them, in a stacked table too.

* `table_regression()`: `p_adjust` leaves out of the family the
  intercepts of a multinomial model and of a
  `sampleSelection::selection()` model, as it does other intercepts, and
  `show_intercept = FALSE`, `keep` and `drop` treat the intercepts of a
  `sampleSelection::selection()` model as they do the intercept of other
  models.

* `table_regression()`: `vcov = "CR1"` for `survreg`, `gam`, `polr`,
  `clm`, `betareg`, `mlogit`, `multinom`, `zeroinfl` and `hurdle` models
  counts only the clusters present in the data. Unused levels of a
  factor `cluster` counted as clusters, which made the standard errors
  slightly too small.

* `table_regression()`: `vcov = "CR0"` or `"CR1"` with a single cluster
  is an error for every class. The score of that one cluster is the
  score of the whole sample, so glmmTMB standard errors came out near 0
  and the other likelihood classes failed with "non-conformable arrays".

* `table_regression()`: the note of a `pscl::hurdle()` zero part with
  `zero.dist = "poisson"`, `"negbin"` or `"geometric"` names the
  distribution ("right-censored negative binomial") instead of pscl's
  code.

* `table_regression()` with `show_columns = "p"` no longer prints a
  negative fit statistic (a ΔAIC, a negative AIC) as `<.001`, or an R²
  without its leading zero, in gt, tinytable, flextable, Word, Excel,
  the clipboard and `inline()`; `output = "excel"` writes a p (change)
  below the display floor as `<.001`, as the other outputs do.

* `table_regression()`: the note on skipped variance-component SE and CI
  of large mixed models names the threshold instead of the internal
  option ("... not computed for n above 1000").

* `table_regression()`: the Word table (`output = "flextable"`) now uses
  a fixed layout with the column widths flextable computes, as the other
  table functions do, so that LibreOffice places the "95% CI" spanner
  over its two columns.

* `table_regression()`: a `reference_label` that opens on `,`, `;` or
  `:` attaches to the level without a space, so `reference_label = ", ref."`
  gives `T0 (2018), ref.` instead of `T0 (2018) (ref.)`.

* `table_regression()` gives a clear error for a model fitted with
  `rms::lrm.fit()`, which has no formula, and suggests refitting with
  `rms::lrm()`. It used to fail with "invalid formula". It refuses an
  `rms::psm()` model the same way, pointing to `survival::survreg()`,
  which it supports.

* `table_regression()`: `labels` renames the intercept of an rms model,
  `"(Intercept)"`, and a name of `labels` or `note_labels` that matches
  nothing in the table warns with the list of the table's terms.

* `table_regression()`: the σ̂ of a `glm()` is the square root of the
  dispersion, so a Gaussian glm shows the same residual standard
  deviation as `lm()`. It showed the dispersion itself (σ²).

* `table_regression()`: a Gaussian `glm()` with the identity link is
  titled "Linear regression", as `lm()` is, and one with another link
  "Gaussian regression" instead of "Regression".

* `table_regression()`: σ̂ is a residual scale only. It is blank for
  negative-binomial models whose theta has its own row (it showed 1 for
  `MASS::glm.nb()`, `lme4::glmer.nb()` and mgcv's `nb()`) and for
  mgcv's other extended families that fix the scale at 1, such as
  `betar()`, `scat()`, `ziP()` and `ocat()` (it showed 1); `tw()` keeps
  the square root of its estimated scale. glmmTMB models no longer show
  their family's dispersion parameter under it. For fixest GLMs it is
  the square root of the dispersion, as for `glm()`, instead of
  fixest's `sigma()`, the square root of the deviance over the residual
  degrees of freedom. A stratified `survival::survreg()` model, which
  has one scale per stratum, leaves it blank instead of showing the
  first stratum's scale.

* `table_regression()`: the τ of a quantile-regression title and note
  follows the table's decimal mark, so `decimal_mark = ","` writes
  "τ = 0,50", and keeps the decimals it needs, up to four ("τ = 0.975"
  instead of "τ = 0.97"); two models whose τ round to the same two
  decimals (0.12 and 0.125) are no longer reported as one model type.

* `table_regression()` refuses an ordinal `rstanarm::stan_polr()` model
  with a clear error, and brms models with an ordinal (`cumulative`,
  `sratio`, `cratio`, `acat`), `categorical`, `multinomial` or
  compositional (`dirichlet`, `dirichlet2`, `logistic_normal`) family.
  Their cut-points, or the coefficients of every category in one column,
  were shown as plain coefficients (as odds ratios under
  `exponentiate = TRUE`), next to a Bayesian R² that brms flags as
  invalid for these families, and `stan_polr()` failed on its default
  fit statistics.

* `table_regression()`: gt tables now rule off the "Thresholds",
  "Random effects" and component blocks, as the console, tinytable,
  flextable and Excel already did.

* `table_regression()`: the console no longer draws the rule of a total
  above a row whose label contains the word "Total".

* `table_regression()`: a `keep`, `drop`, `show_intercept = FALSE` or
  `show_thresholds = FALSE` that leaves no coefficient row now stops
  with `spicy_empty_table`, naming the model and the setting, instead of
  returning a table with a header only.

* `table_regression()`: a multinomial model placed beside other models
  now keeps its intercepts under `keep` or `drop` and hides them under
  `show_intercept = FALSE`, as the other models do. `keep` dropped them
  and `show_intercept = FALSE` kept them. `tidy()` now marks them in
  `is_intercept`.

* `table_regression()`: the standard-error note writes each model's
  label in lower case after the model's name ("Model 1: robust (HC1)"),
  as the other sentences of the note do. A label that opens with an
  acronym or a name keeps its capital ("HC1", "Newey-West (L=1)").

* `table_regression_uv()` titles name the family as `table_regression()`
  does ("inverse-Gaussian" instead of "Inverse.gaussian") and a Cox model
  "Cox proportional hazards regression", as the other tables do.

* The AME column needs marginaleffects 0.29.0 or later.

* The console of `table_continuous()`, `table_continuous_svy()`,
  `table_outcome()`, `table_continuous_lm()` and of a one-way
  `table_categorical_svy()` prints "95% CI" over the bounds "LL" and
  "UL", as the rich outputs do. Column names do not change.

* `table_battery()` and `table_battery_svy()` keep the statistic under
  the group when `top` and `show_n = FALSE` leave one column per group,
  in the console and in every rich output.

* `table_regression()`: the AME of a `glmmTMB` binomial or
  beta-binomial model with a `cbind()` response is weighted by the
  trials whatever the glmmTMB version. Before glmmTMB 1.1.15 it was
  unweighted.

* `table_regression()` gives every reason when it refuses the AME of
  fixest fits: a lean `feols()` with fixed effects beside a GLM with
  fixed effects names both, and a lean GLM with fixed effects is no
  longer told to re-estimate with `lean = FALSE`.

* `table_regression()` no longer prints the line "Random effect
  variances not available" in the console when it computes the R2 of a
  singular mixed model.

* `inline()` explains why the percentage of the `(Missing)` row of
  `table_categorical()` is empty under `percent_base = "valid"`, and
  points to its count.

* `table_categorical_svy()`, `table_continuous_svy()`: the range of
  design degrees of freedom in the note is written "(7–14)".

* A group label with wide characters or decomposed accents no longer
  shifts the labels that follow it in the console of
  `table_regression()`.

* `table_regression()`: the note sentence on an exponentiated intercept
  names the row by the label it shows, the one `labels` gives to
  `"(Intercept)"`, instead of "(Intercept)".

# spicy 0.14.1

* Three tests compare their floating-point results with a tolerance, so
  that they pass under an alternative BLAS such as MKL. No change to the
  code.

# spicy 0.14.0

This release adds `table_multiresponse()` for "check all that apply"
questions and makes `code_book()` a codebook you can deliver, as an
object, an Excel workbook, or a PDF. The descriptive tables share one
contract for labels, p-values, notes, and machine outputs, and come out
ready to publish: decked heads, the minus sign in Typst and LaTeX,
decimals per variable, and notes you can replace, extend, or relabel.
Some numbers change: `table_categorical()` computes its percentages on
the observed values and picks its test per variable. The tables show
column names unless `labels = TRUE`, and their machine outputs take the
`broom` names.

## Breaking changes

### Percentages and counts

* `table_categorical()` computes its percentages on the observed values:
  the `(Missing)` row shows its count and no percentage.
  `percent_base = "total"` gives back the percentages of 0.13.0.

* A variable never observed in a group of `by` shows a dash there, not
  missing values, and the group leaves its `Total` and test: "No data
  for X in G.", or "No data for X." without `by` (#11).

### Tests and effect sizes

* `table_categorical()` defaults to `test = "auto"`: chi-squared when
  all expected counts are 5 or more or weighted counts are not whole
  (with a warning), else Fisher's exact test; `"chisq"` restores 0.13.0.

* `effect_size` takes `"eta2"`, `"epsilon2"`, `"cohens_d"`, and
  `"hedges_g"`, as `es_type` does; the old values (`"eta_sq"`, `"d"`,
  ...) and `TRUE` or `FALSE` (use `"auto"`, `"none"`) are errors.

* `table_categorical()` and `spicy_style()` rename `v_digits` to
  `effect_size_digits` (`v_digits` is a `spicy_defunct` error), and a
  style's value now reaches the association measure and the SMD.

### Labels and names

* The descriptive tables show column names by default: `labels = TRUE`
  or `options(spicy.var_labels = TRUE)` reads the `label` attribute. The
  regression tables print as before but refuse `labels = NULL`.

* A labeled `by` heads its groups with their value labels
  (`[1] Women n`, not `1 n`) in every table, as do the levels of a
  labeled `select` in `table_outcome()`.

* The descriptive tables refuse a level read as their `(Missing)` row or
  `Total` margin, and `table_outcome()` a variable shown as "Overall":
  rename it, or relabel the row with `options(spicy.labels)`.

* An unknown name in `labels`, `note_labels`, a named `digits`, or a
  `show_columns` list warns (class `spicy_unknown_name`) and is left
  out, where some tables refused it.

### Machine outputs

* `tidy()` of `table_categorical_svy()` reports `proportion`, between 0
  and 1, with its interval on that scale and in table order (it gave the
  percentage); its `"long"` output was its wide frame.

* The long outputs, `tidy()`, and `glance()` take the `broom` names (see
  `?spicy`): `test_type`, `statistic`, `df`, `df.residual`, and
  `p.value` (were `statistic_type`, `chi2`, `df1`, `df2`, `p`).

* The effect size is `es_type`, `es_value`, `es_ci_lower`, and
  `es_ci_upper` (were `effect_size_type`, `effect_size`, `assoc_*`,
  `ci_lower`, `ci_upper`), and the SMD `smd_value` (`smd`).

* The sizes are `nobs`, `weighted_nobs`, and `weighted_n` (were
  `n_total`, `n_obs`, or `n`, `weighted.nobs`, `weighted.n`), and the
  fit `r.squared` and `adj.r.squared` (`r2`, `adj_r2`).

* `tidy()` gives a mean or coefficient, its standard error, and its
  interval as `estimate`, `std.error`, `conf.low`, and `conf.high`; the
  long outputs keep `mean`, `se`, `ci_lower`, and `ci_upper`.

* `variable` holds the column name and `label` the label; `tidy()` and
  `glance()` called `variable` `outcome` in `table_categorical()`,
  `table_continuous()`, and `table_continuous_lm()`.

* `output = "data.frame"` and `as.data.frame()` give the printed table
  with its numbers: the keys (`variable`, `label`, `level`, `group`,
  `.row_role`), then the columns as printed (`95% CI LL`).

* That frame splits a cell of several numbers (`q1`, `q3`). It was the
  long frame in `table_continuous()`, its twin, and `table_outcome()`,
  text in `table_regression()`, and levels in `table_categorical()`.

* `table_categorical()` had no `label` column and gave the label in
  `outcome` and in the `variable` of `"long"`; the `Chi2` of its old
  data frame is the `statistic` of `"long"`.

* The long outputs of `table_categorical()` and `table_continuous()`
  keep the effect-size and SMD columns, `NA`, when not requested;
  `table_outcome()` drops its always-`NA` SMD columns.

* `glance()` of `table_categorical()` lists the variables in the order
  of `select` and gives each measure in `es_type` (`"phi"`,
  `"cramer_v"`); `tidy()` keeps the `Total` margin (filter on `total`).

* `table_continuous_lm()` names `by` in the attributes `group_var` and
  `group_label`; a *t*, *z*, or chi-squared has its own `df` and an
  empty `df.residual`, and `glance()` gives the test the table shows.

* Every table carries its whole note in `attr(x, "note")`, on the
  printed object and on the frames of `output = "data.frame"` and
  `"long"`; the `missing_note` attribute is now `note`.

* `as_structured()` reads contract version 4 and refuses a view built by
  an older spicy; the `format_spec` of `table_categorical()` says
  `effect_size_digits` (was `v_digits`).

### Headers and columns

* The continuous tables name the median `Mdn` in headers and data frames
  (`Mdn [Q1, Q3]`, `Mdn 95% CI LL`; was `Med`), and print `Median` and
  `Mean` under `jama` and `lancet`: a script indexing `Med` must change.

* In French, the effect size of `table_continuous()` is headed
  `Taille d'effet` (was `ES`, which now heads the standard errors of the
  regression tables).

* `table_continuous_lm()` with a categorical `by` sets `M` over the
  group labels and heads the difference `Δ` in every output, the note
  giving its order (`Δ = B − A.`), where it printed `Δ (B - A)`.

* `table_categorical()` heads one association measure with its symbol
  (V, φ, τb, γ), also in its data frame, and its bounds `95% CI LL` and
  `95% CI UL`; the `header_ci_*` keys of `spicy.labels` are gone.

### Order of groups and levels

* The tables sort a character `by` in the C locale (capitals first,
  alike on every machine), put `FALSE` first in a logical `by`, and sort
  a numeric one ascending, where they took the order of appearance.

* A labeled `by` follows its value labels, or its codes when one is
  unlabeled. `table_continuous_lm()` sorted text in the machine's
  locale, which chose its reference group.

* The levels in `table_categorical()` and its survey twin, the groupings
  of `table_outcome()`, and the values of a character variable in
  `freq()` follow the same order.

### `inline()`

* `inline()` selects a group of `by` with `model` in every table,
  `inline(tbl, bmi, model = "Female")` (`column = "emmean"` for a mean
  of `table_continuous_lm()`); `level` is a `spicy_defunct` error there.

* `inline()` cites a statistic of the variable (*p*, test, effect size,
  SMD) without `model` (the categorical tables also take a group), and
  in French picks an unnamed model by `"Modèle 1"`, not `"Model 1"`.

### Notes

* Every table note is one paragraph, opening with the lead-in of the
  style ("Note." by default): the data, the missing values, the tests,
  the definitions. The stars legend follows on its own line (#11).

* The descriptive notes open with the sample size, unweighted ("N = 159:
  Women (n = 80), Men (n = 79)."), name the test behind the p-values
  ("P-values:", was "Group comparison:"), and flag weighting.

* By default, a note ends with the definitions of the abbreviations of
  its headers and fit rows ("AIC = Akaike information criterion"),
  leaving M, SD, SE, B, OR, *p*, and the like undefined.

* The `jama` and `lancet` themes use no lead-in word: `jama` lists the
  abbreviations first ("Abbreviations: HR, hazard ratio."), `lancet` at
  the end ("HR=hazard ratio."), neither defining SD, SE, or CI.

* The missing values of `by` are listed with the others ("Missing values
  removed: score (3), sex (2)."), and each association measure of
  `table_categorical()` gets a sentence ("Phi: smoking.").

### Input checks

* The tables refuse, with one error, a `by`, weights, or cluster column
  in `select`, the outcome among `predictors`, and the covariates in
  `select`, which they crossed, dropped, or described as variables.

* A position in `select` counts every column, `by` included; a position
  naming such a column is refused, and `everything()` and the other
  helpers skip it, as the survey twins skip the columns of the design.

* A column not in the data is an error (class `spicy_missing_column`)
  that names it and the close name it may be a typo of; a name in a
  character `exclude` was ignored.

* The tables refuse data with no rows, a `by` that no observation holds
  under `drop_na = TRUE`, a `select` with no column of the right type,
  and, in `table_categorical()`, a `NaN` weight.

* The tables refuse a fractional `digits` or `boot_n`, `p_digits = 0`,
  an output path not ending in `.xlsx` or `.docx` or in no existing
  directory, and unknown `...` arguments of `table_regression_uv()`.

### Regression tables

* `table_regression(nested = TRUE)` with a robust or cluster-robust
  `vcov` tests each added block with a Wald test on that matrix, so the
  change statistics differ; the classical tests are unchanged.

* `table_regression()` refuses a robust `vcov` for the extended `mgcv`
  families other than `nb()` (`tw()`, `betar()`, ...), whose score is
  not the GLM score.

* `table_regression()` refuses a cluster-robust type a class does not
  compute: all but `"CR1"` for `survreg`, `gam`, `polr`, and the like,
  all but `"CR0"` for `coxph` and `rms`. The note says `CR1`, not `CL`.

* `table_regression()` and `table_regression_uv()` return the file path,
  invisibly, with `output = "excel"` or `"word"`, and name an interval
  column with a point whatever `decimal_mark` (`97.5% CI LL`).

* `table_regression()` words its `p_adjust` note for the reader (within
  each model), opens a Bayesian interval note with the model's label,
  and prints a `note` string after the lead-in, above the stars legend.

### Codebook and data

* `code_book()` returns a `spicy_codebook` object, printed as the list
  of variables, instead of opening a `DT` widget; `output` writes the
  file, and `filename` and `include_na` are defunct.

* In `code_book()`, `values` is a number of categories (default 100), an
  explicit `NA` level of a factor counts as missing, and `difftime`
  columns no longer list their values.

* `sochealth` gains a multiple-response question, five logical `info_*`
  items after `dentist_12m`: later columns move five places, and
  `table_categorical(sochealth)` tabulates 18 variables, not 13.

## New features

### Multiple-response questions

* New `table_multiresponse()`, experimental, tabulates a "check all that
  apply" question: one row per item with its count and the percentage of
  respondents who chose it; `inline()` cites its cells (#10, #11).

* `table_multiresponse()` gives the percentages of respondents, of
  responses, or both (`percent`), a row for the base ("respondents who
  answered the question") and one for those who chose no item.

* With `by`, `table_multiresponse()` tests each item (Holm-adjusted by
  default) and runs a global test that allows for several answers per
  respondent, Rao-Scott corrected or bootstrapped.

### Descriptive tables

* `table_categorical()`, `table_continuous()`, `table_outcome()`, and
  the survey twins gain `p_adjust`: the note adds "Adjusted for 3 tests
  (Holm method).", and the raw p-values stay as `p_unadjusted`.

* `table_categorical()` gains `p_value` (#9): `FALSE` drops the test
  from every output but `glance()`, which keeps its columns `NA`; with
  `assoc_measure = "none"` the table is purely descriptive.

* `table_categorical()` gains `percent = "row"`, whose percentages sum
  to 100 across the groups; under `by`, its note and that of its survey
  twin say which percentages they show.

* `table_categorical(total_row = TRUE)` closes each variable with a
  Total row, set apart by a thin rule, which under row percentages gives
  the base rate they are read against.

* `table_categorical()` gains `test = "fisher"` (Monte Carlo for large
  tables), and for 2x2 tables `"midp"` (Fisher mid-p) and `"n1"`
  (Campbell's N-1 chi-squared); the long output gains `test_type`.

* In `table_continuous()`, `table_continuous_svy()`, and
  `table_continuous_lm()`, `digits` takes a named vector: one value per
  variable for the numbers on its scale, the others at the default.

### Labels, notes, and styles

* `labels` takes four forms in every table: `FALSE`, `TRUE`, a named
  vector (`c(bmi = "Body mass index")`), or the new `var_labels()`,
  which keeps the `label` attribute of the columns it does not name.

* Every table function takes `note`, whose string replaces the general
  note, and `note_add`, which adds sentences at its end; each keeps the
  title it prints in `attr(x, "title")`.

* Every table function gains `note_labels`, the label a variable takes
  in the title and the notes, so `labels` can keep a whole question in
  the rows: `note_labels = c(sex = "sex at birth")`.

* `spicy_style()` gains `note_label`, the word opening a note ("Note."
  by default, "Note:", or `""`), and `abbreviations`, which
  abbreviations a note defines (the theme's rule, `"all"`, or `"none"`).

### Outputs and engines

* Typst and LaTeX output (LaTeX only for `gt`) and the PDF of
  `code_book()` write a negative number with the minus sign (U+2212);
  `spicy_style(minus_sign = "ascii")` keeps the hyphen.

* The long output and `tidy()` mark added rows by keys (`.row_role`,
  `.group_role`, `total`), `tidy()` of a table taking `by` always has a
  `group` column, and every result inherits `spicy_table`.

* `tidy()` of `table_continuous()` and `table_outcome()` gains
  `weighted_n`, `median`, `q1`, and `q3`, and `glance()` of
  `table_categorical()` gains `smd_type` and `smd_value`.

### Regression tables

* With `labels = TRUE`, the regression tables name the outcome, numeric
  terms, and factor headers by their `label` ("Linear regression: Body
  mass index"); levels and interactions keep their names.

* `table_regression()` supports cumulative-link mixed models,
  `ordinal::clmm()` fits and `glmmTMB::glmmTMB()` fits of the ordinal
  family, with their thresholds and random effects.

* `table_regression(vcov = "CR0")` gives cluster-robust standard errors
  for `glmmTMB()` fits (`glmmTMB` >= 1.1.15), clustered by default on
  the top grouping factor of the random effects.

### Codebook

* `code_book()` builds a codebook in three parts (`header`, `variables`,
  `values`), which `output` writes to Excel, Typst source, or a PDF
  (cover, variable sheets, index) through the Typst that Quarto bundles.

* `code_book()` gains `subtitle`, `authors`, `notes` (with italics,
  bold, code, and links), `source`, `range`, `decimal_mark`, and PDF
  settings (`font`, `font_size`, `colors`, `paper`, ...).

* `code_book()` documents declared missing values (rows in `values`, a
  summary, `declared_codes`) and names each type in plain words, in
  English or French (categorical nominal, numeric, date, ...).

### French output

* Under `options(spicy.language = "fr")`, the regression and continuous
  tables write French headers (`ES`, `IC à 95 %` over `LI` and `LS`,
  `R² ajusté`, `Modèle 1`); column names stay English.

* `table_regression()` writes `(réf.)` in French (`reference_label`
  defaults to the label of the language) and translates the reference
  footer; a label you supply is printed as typed.

* `table_regression()` writes in French the standard-error line of every
  model class (#7), the p-value line, the notes on adjustment,
  exponentiation, and standardization, and the Bayesian and GEE notes.

* In French, `table_regression()` translates its random-effects test and
  nested models (`modèles emboîtés`), `table_regression_uv()` its title,
  and `table_continuous_lm()` and `table_outcome()` write `ajusté sur`.

## Minor improvements and bug fixes

### All tables

* The table functions warn (class `spicy_ignored_arg`) when a typed
  argument has no effect, such as `excel_path` without
  `output = "excel"` or `adjustment` without covariates.

* Errors name the function called, not a helper, word each argument
  check one way, and carry the classes listed in `?spicy`; a named
  `digits` where one number is expected is refused, with a hint.

* A column name given to a table function, or to the `select` of
  `mean_n()`, `sum_n()`, and `count_n()`, means that column even when an
  object of that name exists in the calling environment.

* The notes name a variable as the rows and title do (not by its column
  name), join a list with semicolons when a label holds a comma, and no
  longer double a label's own "?" ("Do you smoke?.").

* Labels that state a subtraction take the minus sign (Q3 − Q1, the SMD
  direction), a range in a note an en dash (12–30), the singular-fit
  note ±1, and Cramér's V its accent in English.

* The console no longer draws a rule above a row whose label holds
  "Total" ("Total score"), and the Excel files leave an empty cell empty
  rather than holding an empty text.

### Rendering

* The lead-in of a note is in italics in every output, and LaTeX output
  keeps the Greek letters and decimal alignment under all three engines,
  with Cohen's f² in math.

* LaTeX output of `tinytable` compiles when a note holds `%` or `_`,
  centers each spanner over its columns, and scales a table wider than
  the text block down to it, note included.

* `output = "gt"` sets the title at the size of the table text (the
  descriptive tables used 125%), flush left in LaTeX, where a one-column
  header has no rule and a wide table is scaled down.

* Typst output prints the note as text (a URL or the stars legend broke
  it), keeps words whole ("Fe-male"), aligns wrapped level labels, and
  widens or spans columns under long labels (#11).

### `inline()`

* `inline()` gives one message for a column the table does not carry
  ("No column with token "p" in this table."), and says how to select a
  group with `model` when a variable has no levels.

* `inline()` matches a label exactly before it reads a fit statistic (a
  term "N" cited the number of observations), and refuses a `variable`
  that matches two rows, with an address for each.

* `inline()` cites the `partial_chi2` of `table_regression()`, and
  refuses a `table_continuous_lm()` row of means without `column`,
  giving the address of a mean.

### Categorical tables

* `table_categorical()` titles a grouped table with the label of `by`
  ("Categorical table by Sex"), no longer fails when two variables share
  a label, and shows a column named twice in `select` once.

* `table_categorical()` reads a list in `levels_keep` per variable
  (`list(smoking = "Yes")`), as its survey twin does, and its `glance()`
  leaves the association interval `NA` unless `assoc_ci = TRUE`.

* `table_categorical()` and its survey twin show a dash, not a blank, in
  the test and association cells of a variable they cannot compare, and
  name no association measure for it in the note.

* `table_categorical()` reports the rows a missing weight removed in its
  note, where it warned up to three times; it and `table_continuous()`
  no longer warn about `options(spicy.rescale)`.

* `table_categorical(output = "gt")` shows the note on the association
  measure and the SMD and gives each spanner its own id;
  `output = "excel"` draws the rule between variable blocks.

* `table_categorical_svy()` gives a percentage of 0 or 100 the exact
  binomial interval on its unweighted count, whatever `ci_method`, and
  says so, where it printed a zero-width interval or a dash.

* `table_categorical_svy()` without `select` tabulates only categorical
  columns; its note counts a declared missing value once and no longer
  says it removed what its `(Missing)` row shows.

* `table_categorical_svy()` names its test only when one ran, in the
  note and `glance()`, whose `nobs` counts the whole sample under
  `include_total = FALSE`; its note spells "chi-squared".

* With `labels = TRUE`, the survey twins keep their labels when rows
  with a missing `by` are dropped, and `table_categorical_svy()` heads
  the bounds of a proportion `95% CI LL` and `95% CI UL`.

### Continuous tables

* `table_continuous()`, `table_continuous_svy()`, `table_outcome()`, and
  `table_continuous_lm()` show a dash in a comparison cell they cannot
  compute; a blank cell only means one that does not apply.

* These tables draw no inference on a constant variable, nor a
  parametric one on groups each constant: the test, *p*, and effect size
  show a dash and warn (`spicy_undefined_stat`), not p = 1.000 or noise.

* `table_continuous()` no longer warns when `drop_na = TRUE` removes
  rows with a missing `by`, which the note counts; an effect size
  printed with its interval is headed `ES [95% CI]`.

* With `ci = FALSE`, `table_continuous()`, its survey twin, and
  `table_continuous_lm()` render one header row, as do `table_outcome()`
  without intervals and `table_categorical_svy()` without `by`.

* With `p_value = FALSE`, `output = "long"` keeps no test column in
  `table_continuous()`, `table_outcome()`, the survey twins, and
  `table_continuous_lm()` (default `NULL`); `glance()` keeps them `NA`.

* `table_continuous_lm()` fits no model to an outcome with no variance
  and shows a dash for what an exact fit leaves undefined, and no longer
  stops when a level of `by` has no complete observation.

* `table_continuous_lm()` lists its covariates by label, keeps its note
  in `gt::gtsave()`, leaves out the contrast column under
  `contrast = "none"`, and accepts a bootstrap of 10 replicates.

* `table_continuous_svy()` compares a numeric `by` as groups (it tested
  a slope), compares an ordered `by` of two levels, gives each
  comparison its own degrees of freedom, and warns about `drop_na`.

* The note of `table_outcome()` names its marginal row as relabeled, no
  longer names a test that failed, and says each block compares the
  outcome "without controlling for the other variables".

### Regression tables

* `table_regression(nested = TRUE)` divides the deviance difference by
  the dispersion for `glm()`, `mgcv::gam()`, and `mgcv::bam()` fits that
  estimate one, and keeps the `mgcv` test when df round under one (#6).

* `table_regression()` computes the cluster-robust standard errors of
  `mgcv::gam()` and `mgcv::bam()` fits itself: only a `gam()` with a
  canonical link and a fixed dispersion had them right.

* With a cluster-robust `vcov`, `table_regression()` uses that matrix
  for the joint tests (`partial_chi2`) and the standardized coefficients
  of `lmer` and `lme` fits, which were model-based.

* `table_regression()` handles `glmer()` fits with a Gamma,
  inverse-Gaussian, or non-identity gaussian family, and no longer fails
  on them or on `glmer.nb()` fits once `merDeriv` is loaded.

* `table_regression()` titles a `glmer.nb()` fit "Negative-binomial
  mixed-effects regression", gives the null theta two decimals, and
  calls a non-identity gaussian null "gaussian regression".

* `table_regression()` calls the `partial_chi2` of mixed fits a Wald
  test, labels the `mgcv` standard errors "Bayesian (mgcv posterior
  covariance)", and words those of `glm.nb()` as those of a glm.

* `table_regression()` names the link of a non-logit zero part, speaks
  of the interval in the exponentiation note only when shown, writes the
  cloglog formula in math signs, and names the classes it supports.

### Other functions

* `cross_tab()` sets `<` apart in its note (`p < .001`), heads its
  corner `Valeurs` in French, and refuses a string where a column is
  expected (`cross_tab(df, "sex", smoking)` tabulated "sex").

* `freq()` writes a large declared missing code as it is (`100000`, not
  `1e+05`), and `varlist()` and `code_book()` keep only the selected
  columns of an `sf` object.

# spicy 0.13.0

`table_regression()` now covers more than thirty model classes and gains
a univariable screen, the summary tables get survey-design twins, six
journal styles and a French output arrive, and declared missing values
are honored package-wide. The walk-throughs live as articles at
<https://amaltawfik.github.io/spicy/>.

## Breaking changes

* Declared missing values (`na_values`, `na_range`, tagged NAs) now count
  as missing in `freq()`, `cross_tab()`, the `table_*()` family, the
  row-wise helpers, and `varlist()` / `code_book()`, so numbers change
  for labelled survey data. The tables disclose the exclusion in a note.
  `user_na = FALSE` restores the previous behavior.

* `varlist()`, `vl()`, and `code_book()` use one missing definition for
  `N_distinct`, `N_valid`, and `NAs` on labelled data.

* `cross_tab()` counts observations at an explicit `NA` factor level as a
  regular row or column. `freq()` excludes them from `n_valid` and the
  valid percent.

* `freq()` and `cross_tab()` replace `styled` with `output`.
  `styled = FALSE` becomes `output = "data.frame"`, and `styled` now
  errors with the replacement.

* `cross_tab(output = "data.frame")` returns a plain `data.frame` without
  metadata attributes. Read them from the default object, for example
  `attr(cross_tab(...), "p_value")`.

* `freq()` defaults to `rescale = FALSE`, matching `cross_tab()`. Use
  `rescale = TRUE` for the previous behavior.

* `freq()`, `table_categorical()`, `table_continuous()`, and
  `table_continuous_lm()` no longer print when their result is assigned.
  `freq()` drops its unused `...`, so unknown arguments error.

* `options(OutDec)` no longer changes spicy's output. Every number
  follows `decimal_mark` alone, or the style or language in force.

* `table_categorical()` defaults to `drop_na = FALSE`, showing missing
  values as a `"(Missing)"` level. Its `labels` must be a named vector,
  and `p_digits` below 1 errors.

* `table_categorical(output = "long")` names the association column
  `effect_size` and adds `effect_size_type`. Replace
  `out[["Cramer's V"]]` with `out$effect_size`.

* `table_categorical(output = "flextable")` no longer writes a `.docx`
  when `word_path` is supplied. Use `flextable::save_as_docx()`.

* `table_continuous_lm(output = "data.frame")` names the effect-size
  interval bounds `es_ci_lower` / `es_ci_upper`, like the `"long"`
  output.

* `standardized = "smart"` scales continuous inputs by 2 SD and leaves
  binary inputs unscaled. The rule was applied inverted since 0.12.0,
  so those betas change.

* `table_regression(exponentiate = TRUE)` errors on links whose
  exponentiated coefficient is not a ratio (probit, cauchit, inverse,
  sqrt).

* `keep` / `drop` no longer match the intercept row. `show_intercept`
  alone controls it.

* `align = "auto"` is removed. Use `"decimal"` (the default),
  `"center"`, or `"right"`.

* The `show_fit_stats` information criteria are lowercase tokens
  (`"aic"`, `"aicc"`, `"bic"`). `show_fit_stats = character(0)` errors.
  Use `FALSE` to suppress the block.

* With several models, `show_columns = "all_b"` / `"all_ame"` drop the
  CI columns. Request atomic tokens to keep them.

* A robust `vcov` that cannot be computed is now an error, and a
  `cluster` containing `NA` is refused. It used to warn and label the
  classical variance robust.

* `Weighted n` and `glance()`'s `weighted_nobs` are `NA` for an
  unweighted `glm()`. They used to repeat `n`.

* `as_structured()` describes each row in the body itself
  (`body$.row_role`, `body$.indent`, `cell_status`). The 0.12.0
  row-index vectors are removed, `version` is `3`, and a view built by
  an older spicy is refused.

* `tidy()` labels AME rows `estimate_type = "ame"` (was `"AME"`).

* `count_n()` warns and returns `NA` when the selection resolves to no
  usable column. `mean_n()` and `sum_n()` with `min_valid = 0` return
  `NA` for rows with no valid values.

* `copy_clipboard()` arguments use snake_case. `build_ascii_table()` is
  no longer exported. Use `spicy_print_table()`.

* Association measures with `detail = TRUE` always include an `se`
  element. On degenerate tables, `gamma_gk()`, `kendall_tau_b()`,
  `kendall_tau_c()`, and `uncertainty_coef()` return `NA` with a classed
  warning instead of a spurious value. `conf_level` is validated
  everywhere.

* The package ships a single vignette, *Get started*. The walk-throughs
  live as articles on the package site, at the same URLs, and
  `vignette("<name>")` no longer finds them.

## New features

* `table_regression()` supports more than thirty model classes beyond
  `lm` / `glm`: mixed effects (`lmer`, `glmer`, `glmmTMB`, `lme`, `gls`),
  GEE, Bayesian (`stan_glm()`, `stan_glmer()`, `brm()`), survival
  (`coxph`, `survreg`, `cph`, `flexsurvreg`), ordinal (`polr`, `clm`),
  multinomial (`multinom`, `mlogit`), two-part counts (`zeroinfl`,
  `hurdle`), `fixest`, `estimatr`, `ivreg`, `tobit`, `rq`, `rlm`,
  `glm.nb`, `nls`, `gam`, `betareg`, `selection`, `rms`, and the
  design-based `svyglm`, `svyolr`, and `svycoxph`.
  `?table_regression_models` is the registry. A request a class cannot
  honor is refused with a classed error, never rendered as an empty
  column.

* Each family renders with its own conventions. Mixed models report
  their random effects as a block of rows with SE and CI, the ICC, and a
  boundary-correct test of the random part. Ordinal models report their
  thresholds. Two-part models report every component. `fixest` and
  `estimatr` fits disclose their absorbed fixed effects as a
  `Fixed effects:` block. Bayesian fits report posterior medians, MAD
  SD, and credible intervals, with no p-values and a sampler-diagnostics
  guard.

* New `table_continuous_svy()` and `table_categorical_svy()` summarize a
  `survey` design object, with every statistic computed by survey and
  the design degrees of freedom throughout. Design-based regressions
  (`svyglm()`, `svyolr()`, `svycoxph()`) report both counts and name
  their variance estimator in the note.

* New `table_outcome()` summarizes one continuous outcome across several
  categorical variables, one block per grouping, with the group
  comparison and an `Overall` row.

* New `table_regression_uv()` builds univariable screening tables for
  `lm`, `glm`, and `coxph` outcomes, one fit per predictor merged beside
  the multivariable model, with a per-predictor `N` column.

* New `inline()` cites one table cell in Quarto or R Markdown text. The
  returned string is exactly the displayed cell, so a quoted number can
  never drift from the table.

* New `style` argument on the four table families, and
  `options(spicy.style = )` for a whole document: `"jama"`, `"nejm"`,
  `"lancet"`, `"annals"`, `"apa"`, and `"aer"`. A theme applies the
  rules its journal publishes, as defaults, so any argument you pass
  wins. `spicy_style()` builds a style by hand or from a theme.

* `options(spicy.language = "fr")` prints table labels in French and
  brings French typography with it: a decimal comma and a leading zero
  on p-values (`0,003`). Machine outputs, column names, and messages
  stay in English. `options(spicy.labels = )` overrides one label at a
  time, and `spicy_labels()` lists them.

* New `show_columns` families `"rmst"` and `"risk_diff"` for `coxph` and
  `survreg` fits: covariate-adjusted differences in restricted mean
  survival time and in cumulative incidence, by g-computation with
  bootstrap inference, in single tables and in the univariable screen.

* New `show_columns` token `"n_events"` shows event counts as `events/N`
  beside the estimates, for binomial outcomes and `coxph` fits.

* Heteroskedasticity- and cluster-robust `vcov` across the supported
  classes, with each class's field-standard backend. `"CR1S"`
  reproduces Stata's `regress, vce(cluster)` exactly. What no backend
  supports is refused, never approximated.

* `ci_method = "profile"` gives profile-likelihood CIs for `glm`,
  `polr`, and `clm`. `ci_method = "boot_percentile"` reports percentile
  CIs from the bootstrap replicates.

* `nested = TRUE` works across the new classes with the correct test for
  each, and refuses hierarchies that are not comparable.

* AME columns are available for many more classes, per outcome category
  for ordinal and multinomial models, and honor a robust `vcov`.
  `broom::tidy()` gains `outcome_level` for those rows.

* `table_categorical()` and `table_continuous()` gain `smd = TRUE`, a
  standardized-mean-difference column, the balance diagnostic of a
  Table 1.

* `table_continuous()` gains `weights` and `rescale`, under a documented
  convention that matches Stata's `[aweight]` and `survey::svyvar()`.
  Group tests are refused under weights. `table_continuous_lm()` is the
  tool for that.

* `table_continuous()` gains `show_columns` with median tokens (`"med"`,
  `"q1"`, `"q3"`, `"iqr"`, `"med_iqr"`, `"med_ci"`), per variable via a
  named list. A variable shown as a median is tested as one.

* `select` is optional in `table_categorical()`, and `table_continuous()`
  gains `drop_na = FALSE`.

* `as_structured()` reads the descriptive tables too, and carries
  everything the printed table shows, with a per-row identity
  (`.variable`, `.level`, `.row_role`) that survives stacking.

* Seven new articles on the package site: mixed-effects, GEE,
  multinomial, count and two-part, survival, ordinal regression tables,
  and categorical predictors.

## Minor improvements and bug fixes

The first eight fixes change numbers that 0.12.0 reported.

* `kendall_tau_b()` reported wrong standard errors, confidence
  intervals, and p-values in every release from 0.6.0 through 0.12.0.
  Point estimates were correct. `assoc_measures()` and `cross_tab()`
  were affected too.

* Binomial models fitted with a `cbind(successes, failures)` response
  were refitted with squared weights by every internal refit. Bootstrap
  and jackknife inference, the default McFadden and Nagelkerke R², and
  `standardized = "refit"` were wrong for them. Fits with a 0/1, factor,
  or proportion-plus-weights response were never affected.

* Average marginal effects use the fit's prior weights, so AME values
  change for weighted fits.

* Partial effect sizes are true Type-II tests. In models with
  interactions, main effects no longer depend on the factor coding.

* `ci_method = "profile"` with a robust `vcov` defers to the `vcov` and
  warns.

* `table_categorical()` computes the ordinal association measures in
  declared level order under `drop_na = FALSE`.

* `table_continuous_lm()` reports correct estimates when `by` is an
  ordered factor and correct `"balanced"` adjusted means with an
  ordered-factor covariate. It pins treatment contrasts, so
  `options(contrasts = )` no longer alters the results.

* `cross_tab()` computes weighted totals from the unrounded table.

* Under `decimal_mark = ","` every surface follows the mark. P-values
  keep their leading zero (`0,018`), and the star legend, the change
  statistics, and the association intervals read the comma.

* `nested = TRUE` no longer reports a negative chi-square with a p-value
  when the models are passed largest-first.

* `cramer_v()`, `phi()`, and `contingency_coef()` return `NA` with a
  classed warning on a zero margin, and
  `somers_d(direction = "symmetric")` returns `0` on equal concordant
  and discordant pairs.

* The `tau_c` measure is labeled `"Stuart's Tau-c"` everywhere.

* `freq()` keeps its label footer when `NA`-weight rows are dropped,
  warns when distinct codes merge under one label, and sorts labelled
  variables by code under `sort = "name+"`.

* `cross_tab()` reports excluded missing values in the table note,
  accepts logical weights like `freq()`, and no longer swallows the
  warnings of its association measures.

* `table_categorical()` displays labelled columns as `"[code] label"`
  levels in every path, and keeps both the group and the margin when a
  `by` level is named `"Total"`. Its machine outputs carry
  full-precision values and the documented `Chi2` and `df` columns.

* `table_categorical()`, `table_continuous()`, and
  `table_continuous_lm()` resolve `by` data-first, like tidyselect. A
  `by` with no level to tabulate is refused.

* `table_continuous()` forms groups from a non-factor `by` in order of
  first appearance, and degrades per variable when a test fails on
  degenerate data.

* `table_continuous()` and `table_continuous_lm()` label an interval
  with its own coverage (`97.5% CI`, not `98% CI`).

* `table_continuous_lm()` discloses robust and resampling SEs in the
  note, treats a value-labelled `by` as categorical, and degrades
  cleanly on degenerate fits.

* `output = "gt"` tables keep their note when saved or printed
  non-interactively. gt and flextable outputs render in Quarto and
  R Markdown Word, PowerPoint, and PDF documents, where they silently
  disappeared. New `as_flextable()` returns the underlying flextable.

* `output = "gt"` escapes a `by` level or model name carrying a quote,
  an angle bracket, or a backslash.

* `stars = TRUE` marks the coefficients in every output, not just the
  console.

* A cell whose statistic applies but has no number shows the console's
  en dash in every rich output, instead of a blank.

* A `table_categorical(by = )` table carries its association note to
  every output. The descriptive gt and tinytable outputs also draw the
  title and the rule between variable blocks.

* Factor levels are indented once, not twice, in the tinytable, Word,
  and Excel outputs.

* Tables without a confidence-interval column lose their empty header
  strip, multi-line notes keep one disclosure per line, and Typst output
  no longer forces a column gutter under grouped headers.

* Footer lines cite a model by its displayed label, not `Model 1`.

* `output = "excel"` writes the significance stars, blank cells instead
  of `#N/A`, honors `align`, and sizes its columns to the text.

* `output = "clipboard"` quotes cells that contain the delimiter, ships
  plain text instead of Excel formulas, and errors clearly on a system
  without a clipboard.

* Console layout survives `NA` cells, empty cells, and wide characters
  (CJK, emoji).

* `table_regression(m1, m2)` without `list()` errors helpfully, and an
  `NA` or colliding model name no longer crashes the table.

* Factor rows follow `levels()` order instead of alphabetical, and
  factors fit with non-default contrasts (successive differences,
  sum-to-zero, Helmert) group under their parent variable.

* A factor level containing `:` (`"Part-time: 50-89%"`) stays inside
  its variable block. It used to be mistaken for an interaction term.

* The statistic column header follows each model's reference
  distribution (`z` or `t`).

* Bootstrap, jackknife, and `standardized = "refit"` refits no longer
  leak the caller's environment and work on `factor()` / `log()` /
  `poly()` formulas.

* `varlist()` and `code_book()` render `POSIXlt` columns and `difftime`
  units, and show an explicit `NA` factor level as `<NA>`.

* `count_n()` resolves `select` and `exclude` through the same
  tidyselect path as `mean_n()` / `sum_n()`, and errors clearly on an
  unusable `count` or `special`.

* The tabulating and summarizing functions reject `bit64::integer64`
  input with a classed error naming the fix.

* Error messages quote values the same way on every platform, and the
  enum arguments raise classed errors naming the valid values.

* `copy_clipboard()` re-emits backend messages and warnings as real R
  conditions.

* An `estimatr` fit reports R² and adjusted R² by default like `lm`,
  discloses its absorbed fixed effects, and labels its model type after
  its `se_type`.

# spicy 0.12.0

## New features

* New `table_regression()`: publication-ready coefficient summary
  for one or more fitted `lm` or `glm` models, side by side. APA
  Manual 7 formatting is the default. Highlights:

  * Robust variance: classical, HC, cluster-robust (CR) with
    Satterthwaite df, bootstrap, jackknife. Per-model `vcov`
    accepted for SE-comparison tables.
  * Standardization: `refit`, `posthoc`, `basic`, `smart`,
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

### Harmonization across the table family

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
  are stable, stabilizing or internal. pkgdown reference groups
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
* `varlist()` / `code_book()` honor `factor_levels = "all"` for
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
  behavior.
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
