# Phase 3 — matrice promesse-test CONSOLIDEE (2026-07-31)

616 promesses (dont 30 zones-critique), 456 couvertes, 160 trous: {'partial': 139, 'uncovered': 21}
Trous par source: {'rd-core': 47, 'rd-vcov-classes': 37, 'rd-methods': 26, 'rd-uv-estimands': 14, 'vignettes-news': 25, 'critic': 11}

### critic:desc-stale-regression-scope [uncovered]
DESCRIPTION:6-20 (Description field)
PROMESSE: INCIDENTAL DEFECT (not behavioral): the CRAN Description still scopes regression tables to 'lm', 'glm', and mixed-effects fits (lme4, glmmTMB, nlme) with only the RE panel and Nakagawa R-squared called out, although 30+ classes, AME, survival estimands and the univariable screen have shipped. Refresh before the 0.13 CRAN submission.
TEST ATTENDU: Doc-review item, not unit-testable; optionally a spell/consistency check that DESCRIPTION mentions table_regression's actual scope. UNCOVERED by construction.
PREUVE: Doc-review defect by construction: DESCRIPTION's stale scope (lm/glm/mixed only) is not unit-testable and no consistency check ties the Description field to the registry. Release action: refresh DESCRIPTION before the 0.13 CRAN submission.

### critic:pkgrd-stability-tiers-complete [uncovered]
man/spicy-package.Rd:26-56 (R/spicy-package.R)
PROMESSE: Every public function belongs to a named API-stability tier (Stable / Stabilising / Internal). DEFECT: table_regression(), table_regression_uv(), table_regression_models(), and as_structured() appear in no tier at all, so the flagship 0.12 family carries no stability promise — an omission to fix before the 0.13 release.
TEST ATTENDU: UNCOVERED: no test compares getNamespaceExports('spicy') (minus S3 methods and internals) against the union of the tier lists in the Rd. Testable as a doc-consistency test parsing R/spicy-package.R; the fix itself is a doc edit assigning the table_regression family (presumably Stabilising).
PREUVE: No test compares getNamespaceExports('spicy') against the tier lists in R/spicy-package.R (grep for getNamespaceExports / tier / stability in tests/testthat: zero relevant hits). The documented defect (table_regression family absent from all tiers) has no doc-consistency guard.

### rd-core:excel-sheet-default [uncovered]
man/table_regression.Rd:667-668
PROMESSE: excel_sheet defaults to "Regression".
TEST ATTENDU: The written workbook's first sheet is named Regression; excel_sheet='Tab1' renames it.
PREUVE: No test asserts the workbook sheet name. Excel tests pass excel_sheet explicitly without reading it back (test-regression_dispatch_engines.R line 22 excel_sheet='Reg'; wb_to_df calls always use sheet = 1), and test-cov-regression_dispatch.R lines 491-508 pass 'Regression' as an explicit argument to the internal output_excel, never exercising or verifying the default.

### rd-core:factor-layout-scope [uncovered]
man/table_regression.Rd:323-326
PROMESSE: factor_layout applies to any categorical predictor: factor, ordered, character, or logical.
TEST ATTENDU: A character and a logical predictor render with grouped header/indent exactly like a factor.
PREUVE: aucun test ne rend un prédicteur character ou logical via factor_layout grouped (header + indentation); les tests factor_layout n'utilisent que des factors/ordered

### rd-core:fit-digits [uncovered]
man/table_regression.Rd:602-605
PROMESSE: fit_digits (default 2L) sets decimals for r2, adj_r2, r2_change, adj_r2_change, omega2, f2, f2_change, sigma, rmse.
TEST ATTENDU: fit_digits=4 renders R2 with 4 decimals while AIC keeps ic_digits.
PREUVE: fit_digits appears only as a pass-through argument in internal coverage tests (test-cov-regression_render.R:282, test-cov-regression_structured.R:154) with no assertion on decimal behavior. Behavioral fit_digits=4 tests exist only for table_continuous_lm (test-table_continuous_lm.R:296) and ic_digits for AIC (test-polish_7c22.R:102), not for table_regression's fit_digits token scope.

### rd-core:fit-stats-layout-decimal-align [uncovered]
man/table_regression.Rd:424-429
PROMESSE: Decimal alignment of every numeric column is preserved in both fit_stats_layout modes.
TEST ATTENDU: Console output under both modes: B-column decimal points line up across coefficient and fit-stat rows.
PREUVE: aucun test n'asserte l'alignement décimal des colonnes numériques sur les lignes fit-stat dans les deux modes de layout

### rd-core:show-columns-p-is-b-p [uncovered]
man/table_regression.Rd:262-264,771-773
PROMESSE: The "p" token is always the B/beta p-value; the AME-specific p-value requires "ame_p".
TEST ATTENDU: show_columns=c('ame','p') p column equals the B p-values, not the AME p-values.
PREUVE: aucun test n'asserte que la colonne 'p' porte les p de B (et non d'AME) quand des colonnes AME sont demandées; les tests rendent p et ame_p sans vérifier le mapping des valeurs

### rd-core:vcov-glmnb-theta-held [uncovered]
man/table_regression.Rd:79-82
PROMESSE: For glm.nb bootstrap, theta is held at its full-sample estimate per replicate (differs from Stata nbreg vce(bootstrap)).
TEST ATTENDU: Instrument the replicate refits: every replicate uses the original fit's theta, not a re-estimated one.
PREUVE: aucun test n'instrumente les réplicats glm.nb pour vérifier que theta reste à l'estimation pleine (grep theta/glm.nb/boot sans résultat dans tests/testthat/)

### rd-methods:asdf-rownames-optional-ignored [uncovered]
man/as.data.frame.spicy_regression_table.Rd:16-18
PROMESSE: Les arguments row.names et optional de as.data.frame() sont ignorés — la disposition des lignes de la table est préservée quelle que soit leur valeur.
TEST ATTENDU: expect_identical(as.data.frame(tbl), as.data.frame(tbl, row.names = letters[1:nrow(tbl)], optional = TRUE)) (mêmes cellules, même ordre).
PREUVE: Aucun test n'appelle as.data.frame(tbl, row.names=..., optional=...) sur un spicy_regression_table (grep row.names ne touche que copy_clipboard).

### rd-methods:flextable-verbs-work-on-tagged [uncovered]
man/as_flextable.spicy_flextable.Rd:21-22 (« Every flextable verb already works on the tagged object »)
PROMESSE: Tous les verbes flextable fonctionnent directement sur l'objet taggé sans conversion préalable.
TEST ATTENDU: expect_no_error(flextable::bold(ft, part='header')); expect_no_error(flextable::width(ft, width=1)); expect_no_error(flextable::fontsize(ft, size=9))
PREUVE: Aucun test n'applique un verbe flextable (bold/width/fontsize/...) à l'objet taggé spicy_flextable (grep flextable::(bold|width|fontsize|autofit|set_caption) sans résultat).

### rd-methods:tidy-glance-dots-ignored [uncovered]
man/tidy.spicy_regression_table.Rd:16-17
PROMESSE: L'argument ... de tidy() et glance() est ignoré (présent uniquement pour la compatibilité avec les génériques broom) — passer des arguments supplémentaires ne change pas le résultat et n'erre pas.
TEST ATTENDU: expect_identical(tidy(tbl), tidy(tbl, conf.int = FALSE, foo = 1)); expect_identical(glance(tbl), glance(tbl, bar = 2))
PREUVE: Aucun test ne passe d'arguments supplémentaires à tidy()/glance() sur un spicy_regression_table (aucun appel tidy(tbl, conf.int=...) ni comparaison d'identité avec/sans ... dans tests/testthat/).

### rd-vcov-classes:cr-token-maps-single-estimator [uncovered]
man/table_regression.Rd:1039-1041; R/vcov.R:107-117
PROMESSE: Pour les classes à sandwich cluster unique (Cox, classes vcovCL, rms), les variantes CR0-CR3 n'existent pas: le CR* demandé mappe sur l'unique estimateur disponible.
TEST ATTENDU: Sur un coxph ou polr, les SE sous "CR0", "CR1", "CR2", "CR3" sont identiques entre elles.
PREUVE: Aucun test n'affirme l'identité des SE sous CR0/CR1/CR2/CR3 sur une classe à sandwich unique (coxph, polr, rms); les tests utilisent CR2 (ou CR0 pour pscl) isolément

### rd-vcov-classes:glmnb-theta-held [uncovered]
man/table_regression.Rd:79-84
PROMESSE: Pour glm.nb, le bootstrap tient theta à son estimation plein-échantillon (convention différente de Stata nbreg vce(bootstrap) qui le réestime par réplicat).
TEST ATTENDU: Bootstrap sur un glm.nb: les réplicats sont refittés avec la famille negative.binomial(theta_plein_echantillon); vérifiable en comparant à un refit manuel glm.fit avec fam fixe.
PREUVE: grep theta/negative.binomial dans tests/: seulement test-regression_frame_MASS.R (extras theta) et test-cov100-vcov_glm.R (set d'estimateurs); aucun test bootstrap glm.nb ne vérifie theta tenu à l'estimation plein-échantillon

### rd-vcov-classes:help-topic-aliases [uncovered]
man/table_regression_models.Rd:4-11,27-32
PROMESSE: La page est joignable via 8 alias: ?table_regression_models, _mixed, _ordinal, _counts, _categorical, _survival, _robust, _bayesian.
TEST ATTENDU: help("table_regression_survival") etc. résolvent tous vers cette page (Rd \alias présents).
PREUVE: Aucun test ne vérifie les 8 alias Rd (grep table_regression_survival/_mixed/_bayesian dans tests/: zéro occurrence)

### rd-vcov-classes:rq-vcov-dimnames [uncovered]
R/vcov.R:996-1001
PROMESSE: La matrice vcov rq retournée est nommée par les noms de coefficients (dimnames = names(coef(fit))) et summary est appelé avec hs=TRUE, covariance=TRUE.
TEST ATTENDU: rownames(compute_model_vcov(rq,"classical")) == names(coef(fit)).
PREUVE: Aucun test ne vérifie dimnames(compute_model_vcov(rq, 'classical')) == names(coef(fit)) ni les arguments hs=TRUE/covariance=TRUE (les tests rq passent par les SE, pas par la matrice nommée)

### vignettes-news:align-auto-removed [uncovered]
NEWS.md:125-126
PROMESSE: align = "auto" is removed from all table_*() functions; valid values are "decimal" (default), "center", "right".
TEST ATTENDU: align = "auto" errors.
PREUVE: Aucun test n'asserte que align = 'auto' erre sur table_regression (ni sur les autres table_*); seuls les tests positifs existent ('align – decimal is default...', 'align – right / center are accepted'). Le rejet repose sur match.arg non testé.

### vignettes-news:ci-header-tracks-level [uncovered]
table-regression.Rmd:98-101
PROMESSE: The CI column header tracks ci_level: ci_level = 0.99 relabels the column 99% CI.
TEST ATTENDU: table_regression(fit, ci_level = 0.99) header reads 99% CI and the bounds widen accordingly.
PREUVE: Aucun test table_regression ne vérifie qu'un ci_level non défaut relabelle l'en-tête ('99% CI') ni n'élargit les bornes; seuls les garde-fous de plage existent (test-table_regression.R 'ci_level – out of range errors spicy_invalid_input'). Le seul test '90% CI' porte sur table_continuous.

### vignettes-news:counts-offset-silent [uncovered]
table-regression-counts.Rmd:103-107
PROMESSE: An offset(log(exposure)) term is absorbed silently — no spurious coefficient row appears.
TEST ATTENDU: Poisson glm with offset: no offset row in the table.
PREUVE: No test asserts the absence of an offset coefficient row in a rendered table. Existing offset tests cover different behaviors: null-model pseudo-R2 keeps the offset (test-regression_glm.R:1583) and bootstrap refits thread the offset (test-env_leak_refits.R:69).

### vignettes-news:effect-size-type2-anova [uncovered]
table-regression.Rmd:206-209
PROMESSE: The partial F-test underlying the effect sizes is computed on a Type-II ANOVA reference (car::Anova).
TEST ATTENDU: On an unbalanced two-factor lm, the F values match car::Anova(fit, type = 2).
PREUVE: Aucun test ne compare les F partiels de table_regression à car::Anova(fit, type=2) sur un lm à deux facteurs déséquilibré; la convention Type-II n'est affirmée que dans les commentaires de R/regression_partial.R (validation offline non capturée en test).

### vignettes-news:multimodel-empty-blocks [uncovered]
table-regression-counts.Rmd:271-272
PROMESSE: In mixed multi-model tables, the zero block simply stays empty in the columns of models that have none.
TEST ATTENDU: list(Poisson, ZIP): zero rows blank under the Poisson column.
PREUVE: No test builds a mixed multi-model table (e.g. list(Poisson glm, zeroinfl)) and asserts the zero block stays blank under the component-less model's columns. test-component_blocks.R multi-model test (:211) uses the same zeroinfl twice (gloss dedup only); the geeglm 'blank alien cells' test covers fit stats, not component blocks.

### vignettes-news:uv-default-news-contradiction [uncovered]
NEWS.md:212 vs NEWS.md:5-6
PROMESSE: NEWS 'New functions' still claims glm is the uv default ("Supports glm (default), lm, and coxph"), contradicting the breaking-change entry that the default is now lm. Doc bug: one statement is stale.
TEST ATTENDU: Check formals(table_regression_uv)$method / observed default against both NEWS sentences; fix the stale sentence before release.
PREUVE: Bug documentaire NEWS.md:212 vs NEWS.md:5-6 : aucun test ne garde la cohérence des deux phrases NEWS; le défaut lm est testé (test-regression_uv.R 'default method is the linear screen...') mais la phrase périmée 'Supports glm (default)' reste à corriger à la main.

### critic:desc-apa-broom-methods [partial]
DESCRIPTION:16-18 (Description field)
PROMESSE: All table outputs follow APA conventions and expose broom-compatible tidy()/glance() methods for downstream pipelines.
TEST ATTENDU: PARTIAL: broom methods tested per class (test-regression_broom.R, test-table_categorical.R:2108-2182, test-table_continuous.R / test-table_continuous_lm.R broom tests) but no test enumerates ALL table classes (freq/cross_tab outputs included? — the field says 'all table outputs') against methods('tidy')/methods('glance'); APA formatting is covered indirectly via print snapshots only.
PREUVE: Broom methods tested per class: tests/testthat/test-regression_broom.R:11,122; test-table_categorical.R:2108; test-table_continuous.R:2529,2569,2599; test-table_continuous_lm.R equivalents. But no test enumerates ALL table classes against methods('tidy')/methods('glance') (grep methods('tidy'): no hits), leaving the 'all table outputs' claim (freq/cross_tab included?) unpinned; APA formatting is only covered indirectly through print snapshots.

### critic:pkgrd-broom-columns-stabilising [partial]
man/spicy-package.Rd:59-71
PROMESSE: tidy()/glance() on spicy_categorical_table, spicy_continuous_table, spicy_continuous_lm_table, and spicy_regression_table follow broom column conventions, and the produced column set will not be silently renamed or re-semanticized within 0.y.z.
TEST ATTENDU: PARTIAL: per-class column checks exist — tests/testthat/test-regression_broom.R 'tidy – returns broom-canonical column names' and 'glance – broom-canonical column names'; test-table_categorical.R 'tidy() returns long-format with broom-conventional columns (cross-tab)'; test-table_continuous(.lm).R have broom tests — but all use expect_true(all(expected %in% names())), which tolerates silent renames of columns outside the tested subset. A frozen expect_identical(names(...)) or snapshot per class would pin the stability contract.
PREUVE: Per-class broom tests confirmed: tests/testthat/test-regression_broom.R:11 (tidy) and :122 (glance), test-table_categorical.R:2108, test-table_continuous.R:2529-2599 — but both regression checks use expect_true(all(expected %in% names(...))) (lines 33, 134), which tolerates silent renames/re-semanticizing of columns outside the tested subset. No expect_identical(names(...)) or column-set snapshot pins the 0.y.z stability contract.

### critic:pkgrd-broom-df-types [partial]
man/spicy-package.Rd:72-77
PROMESSE: Numeric broom columns keep downstream-expected types: df is integer, df.residual is numeric double, so Satterthwaite-corrected fractional dfs from cluster-robust vcov modes are preserved verbatim (matching lmerTest::glance()/afex).
TEST ATTENDU: PARTIAL: tests/testthat/test-regression_broom.R 'glance – df.residual is numeric (not integer) – Satterthwaite-safe' covers the df.residual half; frame-level carriage covered by tests/testthat/test-cov-cluster-robust-df.R 'estimatr lm_robust CR2: per-coefficient (varying) df is carried through'. Never asserted: tidy()$df is integer-typed, and a fractional Satterthwaite df arriving verbatim in the tidy()/glance() output of a CR2 fit.
PREUVE: df.residual half covered: tests/testthat/test-regression_broom.R:146-151 'glance – df.residual is numeric (not integer) – Satterthwaite-safe' (expect_type double); frame-level carriage: test-cov-cluster-robust-df.R:30 'estimatr lm_robust CR2: per-coefficient (varying) df is carried through' and :9 (non-finite Satterthwaite fallback). Never asserted: tidy()$df is integer-typed (no expect_type/typeof on td$df anywhere), nor a fractional Satterthwaite df arriving verbatim in tidy()/glance() output of a CR2 fit.

### critic:pkgrd-cond-defunct-contract [partial]
man/spicy-package.Rd:121-124
PROMESSE: spicy_defunct errors (pre-1.0 hard removals) name the replacement in the message AND are signaled together with spicy_invalid_input so generic input handlers still catch them.
TEST ATTENDU: PARTIAL: leaf class asserted in tests/testthat/test-freq.R 'freq() styled is defunct with a migration error' and tests/testthat/test-cross_tab.R 'cross_tab styled is defunct with a migration error'; the source co-signals both classes (R/cross_tab.R:1400). Not asserted anywhere: the replacement being named in the message, or catchability via class = spicy_invalid_input.
PREUVE: Leaf class asserted: tests/testthat/test-freq.R:741-744 and test-cross_tab.R:1124-1133 (expect_error class = 'spicy_defunct' only). No test asserts the replacement is named in the message (no regexp/conditionMessage check on any defunct error), and no test catches these via class = 'spicy_invalid_input' to prove the co-signal.

### critic:pkgrd-cond-internal-invariant-dual [partial]
man/spicy-package.Rd:125-131,172-175
PROMESSE: spicy_internal_invariant exists as an error leaf (result cannot be trusted) and as a warning leaf where the output still renders, so the user sees both the table and the diagnostic.
TEST ATTENDU: PARTIAL: error leaf covered (tests/testthat/test-cov-lm_compute.R:496-498, align_design_to_coef), warning leaf class covered (tests/testthat/test-regression_structured.R:184-186 on .validate_structured); the render-through guarantee — table still prints alongside the warning — is not asserted.
PREUVE: Error leaf: tests/testthat/test-cov-lm_compute.R:496-499 (align_design_to_coef aborts, class spicy_internal_invariant). Warning leaf: test-regression_structured.R:184-191 (.validate_structured warns with spicy_internal_invariant + spicy_warning). The render-through guarantee — the table still prints alongside the warning-leaf diagnostic — is not asserted anywhere.

### critic:readme-registry-30plus [partial]
README.md:48-104 (Supported models table)
PROMESSE: table_regression() accepts a single fit or a list of fits from every engine in the 11-family registry table (30+ classes, lm/glm through Bayesian) and renders each with its family's conventions.
TEST ATTENDU: COVERED for registry-dispatch drift: tests/testthat/test-table_regression_models.R 'the registry matches the as_regression_frame dispatch methods' (every registry class has a method and vice versa); per-class rendering covered by the test-regression_frame_*.R family (svyglm, geepack, fixest, mgcv, quantreg/AER, merMod, nls, survival, ...). A remaining strengthening: one smoke test fitting each registry class end-to-end through table_regression().
PREUVE: Registry-dispatch drift guard verified: tests/testthat/test-table_regression_models.R:6 'the registry matches the as_regression_frame dispatch methods' (every registry class has a method and vice versa, umbrella classes excluded). Per-class rendering is a mosaic (test-regression_frame_*.R x ~20 files, test-exponentiate-classes.R, test-ame-by-class.R, test-regression_dispatch_engines.R), but most frame test files exercise as_regression_frame(), not table_regression() end-to-end (only 4 of the frame files call table_regression). No single smoke test fits each registry class (single + list) through table_regression(); the README's 'every engine renders' promise rests on indirect composition.

### critic:s3-undoc-terms-uv-screen [partial]
R/regression_uv.R:637 + NAMESPACE:70
PROMESSE: terms.spicy_uv_screen is exported with no Rd. Behavioral promise: returns stats::terms of outcome ~ all screened predictors (backtick-safe), which the label validator relies on.
TEST ATTENDU: PARTIAL: exercised only indirectly through label validation (tests/testthat/test-regression_uv.R 'labels pass through to the row stubs'); no direct test that terms(screen) yields the outcome ~ predictors formula, including non-syntactic names (the backtick branch). Missing Rd is the defect; a direct unit test on the returned formula would close the behavior.
PREUVE: No direct test of the method: grep for terms.spicy_uv_screen / terms(screen...) in tests/testthat finds nothing. Exercised only indirectly through label validation (tests/testthat/test-regression_uv.R:233 'labels pass through to the row stubs'); the returned outcome ~ predictors formula and the backtick (non-syntactic name) branch are unasserted. Missing Rd is the additional defect.

### critic:vig-postprocess-native-objects [partial]
vignettes/summary-tables-reporting.Rmd:316-390
PROMESSE: All four helpers return regular gt / tinytable / flextable objects that native package verbs (gt::tab_style, tinytable::style_tt, flextable::theme_booktabs |> autofit |> fontsize) keep working on — including table_regression() outputs.
TEST ATTENDU: PARTIAL: class identity is asserted in engine tests and as_flextable() returns the clean object (test-quarto_word_rendering.R 'as_flextable() returns the clean engine object, note intact'), but no test pipes a spicy-returned table through the vignette's own native styling chains and asserts success (the spicy_flextable/spicy_gt class tags make this worth a regression test).
PREUVE: Class identity asserted in engine tests and tests/testthat/test-quarto_word_rendering.R:80 'as_flextable() returns the clean engine object, note intact', but no test pipes a spicy-returned table through the vignette's native styling chains — grep for gt::tab_style / tinytable::style_tt / flextable::theme_booktabs in tests/testthat finds no post-processing usage (the only autofit hit is spicy's own construction at test-regression_dispatch_engines.R:548). The spicy_gt/spicy_flextable class tags make this worth a regression test.

### critic:vig-shared-output-grammar [partial]
vignettes/summary-tables-reporting.Rmd:44-47,76-79
PROMESSE: table_categorical(), table_continuous(), table_continuous_lm(), and table_regression() share the same output formats (gt, tinytable, flextable, word, excel, clipboard) and the same decimal_mark, digits, p_digits, labels, and align arguments — one reporting workflow moves across all four.
TEST ATTENDU: PARTIAL: each function has per-engine output tests (86 output=word/excel/clipboard hits across 10 files, incl. test-table_categorical.R, test-table_continuous.R, test-table_continuous_lm.R, test-table_regression.R, test-regression_dispatch_engines.R) but no cross-family parity test asserting the four accept the identical output vocabulary and share those formals; a formals-intersection test would pin the 'same grammar' claim as a unit.
PREUVE: Per-engine/per-format tests are plentiful (102 output = word/excel/clipboard hits across 10 files incl. test-table_categorical.R (22), test-table_continuous.R (26), test-table_continuous_lm.R (9), test-table_regression.R (6), test-regression_dispatch_engines.R (24)), but no cross-family parity test asserts the four helpers accept the identical output vocabulary or share decimal_mark/digits/p_digits/labels/align formals (only formals() test in the suite is about varlist).

### rd-core:align-decimal-ci-anchors [partial]
man/table_regression.Rd:615-622
PROMESSE: align="decimal" (default) pre-pads cells so decimal marks line up; for CI cells the left bracket, LL decimal point, comma separator, UL decimal point, and right bracket are each independently aligned across rows; "center" and "right" apply uniformly.
TEST ATTENDU: Console output: decimal-point character positions identical down each numeric column; CI anchors column-aligned; align='right' right-justifies.
PREUVE: Default + CI anchors tested: test-table_regression.R 'align – decimal is default; padding applied to numeric cols'; test-regression_glm.R 'align_ci_strings: en-dash and blank cells centered in column' (uniform widths + open/close bracket positions identical), European decimal-mark variant; test-polish_7c24.R '.pad_for_decimal_align pads en-dash cells'. But 'center'/'right' are only checked as accepted attributes ('align – right / center are accepted', 'print – align = center propagates') — no test asserts right-justification or per-anchor (LL decimal, comma, UL decimal) positions independently.

### rd-core:bayes-diagnostics-guard [partial]
man/table_regression.Rd:1117-1124
PROMESSE: Every Bayesian table runs an automatic sampler-diagnostics guard (R-hat >= 1.01, ESS below 100/chain floored at 400, divergences, E-BFMI < 0.2): problems add a footer line and a warning classed spicy_bayes_diagnostics nested under spicy_caveat; clean fits print nothing.
TEST ATTENDU: A short-chain problematic fit warns with class spicy_bayes_diagnostics (catchable as spicy_caveat) and adds a footer line; a clean fit adds neither.
PREUVE: Problematic-fit half tested: test-stan_bayes_gates_re.R 'diagnostic columns render and the convergence guard fires' (:255, under-sampled fit → 'Sampler diagnostics:' footer + warning) and 'Pareto-k and p_waic diagnostics are surfaced, not silenced' (:556-609, every guard warning carries spicy_bayes_diagnostics nested under spicy_caveat/spicy_warning). Not tested: a clean fit adds neither footer nor warning, and the individual thresholds (R-hat 1.01, E-BFMI 0.2, divergences) are not exercised separately.

### rd-core:boot-n-default [partial]
man/table_regression.Rd:157-159
PROMESSE: boot_n is a single positive integer, default 1000L, giving the number of bootstrap replicates.
TEST ATTENDU: formals default is 1000L; boot_n=0 or c(10,20) is rejected; boot_n=50 runs 50 replicates.
PREUVE: test-regression_validate_branches.R "validate_boot_n – non-integer / negative errors" + test-cov100-titlefooter.R "cluster bootstrap footer names the scheme, count, and cluster" (compte de réplicats = boot_n); le défaut 1000L n'est pas épinglé

### rd-core:ci-level-default-scope [partial]
man/table_regression.Rd:119-120
PROMESSE: ci_level (default 0.95) applies to all reported CIs: B, beta, AME, and partial effect sizes.
TEST ATTENDU: ci_level=0.90 changes B, beta, AME, and partial_* CI bounds simultaneously; default matches confint(fit, level=.95).
PREUVE: test-regression_frame_lm.R "coefs CI matches confint(fit)" (défaut 0.95) + test-random_effects_rows.R "ci_level is honored by the vc CI paths of all mixed engines"; pas de test qu'un même ci_level déplace simultanément B, beta, AME et partial_*

### rd-core:ci-method-profile-lm-refused [partial]
man/table_regression.Rd:133
PROMESSE: ci_method="profile" with lm raises spicy_invalid_input.
TEST ATTENDU: expect_error(table_regression(lm_fit, ci_method='profile'), class='spicy_invalid_input').
PREUVE: test-ordinal-profile-ci.R "profile is rejected for classes without a profile path" teste survreg (classe spicy_invalid_input); le cas lm spécifiquement n'est pas testé

### rd-core:clipboard-delim-payload [partial]
man/table_regression.Rd:670-677
PROMESSE: clipboard_delim defaults to "\t"; the clipboard payload mirrors the Excel layout (title row, spanner row, header, body, footer note) but is plain text without rules, merging, decimal alignment, monospace, or indentation.
TEST ATTENDU: Mock clipr::write_clip: payload lines are tab-separated and include title, spanner, header, body, note rows in order.
PREUVE: tests/testthat/test-regression_dispatch_engines.R: 'output = clipboard delegates to clipr::write_clip' (l.48) mocks write_clip with clipboard_delim='\t' but only asserts the payload contains 'Variable'; tab-separation and the title/spanner/header/body/note row order are never verified.

### rd-core:description-glm-families [partial]
man/table_regression.Rd:753-754
PROMESSE: glm coverage spans binomial / poisson / Gamma / inverse.gaussian / quasi families with any link.
TEST ATTENDU: A fit from each listed family (e.g. quasipoisson, Gamma(log)) renders without error with correct SEs.
PREUVE: Gamma(log) renders with title (test-regression_glm.R l.81), quasipoisson/quasibinomial through table_regression (l.677, l.1451), poisson (l.78). inverse.gaussian is only exercised for glmer (test-cov-merMod.R l.44) and as unit-level family labels (test-cov100-frames_misc.R l.150) — no end-to-end plain-glm inverse.gaussian table, and per-family SE correctness is not oracled.

### rd-core:digits-scope-default [partial]
man/table_regression.Rd:588-591
PROMESSE: digits (default 2L) sets decimals for b, beta, se, ci, t, f_change, lrt_change, deviance, deviance_change, ame, ame_se, weighted_nobs.
TEST ATTENDU: digits=3 changes B/SE/CI/t/AME formatting to 3 decimals; p-values and AIC unchanged.
PREUVE: Helper-level digits contract tested: test-regression_glm.R 'AUDIT B8: small values respect digits contract' / 'format_number stays fixed-decimal' (format_number(x, 2L)); test-table_regression.R 'digit args – non-positive integer errors spicy_invalid_input'. No test asserts that table_regression(digits = 3) reformats B/SE/CI/t/AME to 3 decimals while p/AIC stay unchanged (digits=3L appears only in the acceptance test test-regression_glm.R:1353 with expect_no_error).

### rd-core:effect-size-digits [partial]
man/table_regression.Rd:598-600
PROMESSE: effect_size_digits (default 2L) sets decimals for partial_f2, partial_eta2, partial_omega2.
TEST ATTENDU: effect_size_digits=3 changes only the partial_* columns.
PREUVE: tests/testthat/test-regression_render.R: 'render – partial_omega2 column uses effect_size_digits' (l.205, asserts exactly 3 decimals with effect_size_digits=3). partial_eta2 rendered at l.184 but digit count not asserted; partial_f2 digits and the 2L default are never verified.

### rd-core:fit-stats-change-tokens-nested-only [partial]
man/table_regression.Rd:894-899
PROMESSE: Change tokens (r2_change, adj_r2_change, f_change, f2_change, lrt_change, aic_change, aicc_change, bic_change, deviance_change, p_change) are active under nested=TRUE.
TEST ATTENDU: Each token under nested=TRUE renders its change row; under nested=FALSE they are refused or inert per implementation.
PREUVE: acceptés sous nested=TRUE (test-table_regression.R "show_fit_stats – change tokens accepted under nested = TRUE" + test-regression_nested.R "user can override change tokens"); le comportement sous nested=FALSE (refus/inertie) n'est pas testé

### rd-core:fit-stats-default-mixed-union [partial]
man/table_regression.Rd:386-389,905-906
PROMESSE: Mixed lm + glm tables default to the union of both defaults, and the renderer en-dashes per cell the stat not defined for a given model class.
TEST ATTENDU: list(lm, glm) default: R2 cell of the glm column is an en-dash; pseudo-R2 cell of the lm column is an en-dash.
PREUVE: variantes testées (test-regression_frame_geepack.R "geeglm sits next to an lm in a mixed table (blank alien cells)"; test-stan_bayes_gates_re.R table mixte); l'union lm+glm avec en-dash par cellule n'est pas testée telle quelle

### rd-core:fit-stats-layout-first-col [partial]
man/table_regression.Rd:398-403
PROMESSE: fit_stats_layout="first_col" (default) places each fit-stat value in the FIRST numeric sub-column of each model (typically B) with the remaining sub-columns left empty for that row.
TEST ATTENDU: Default: R2 value sits in the B sub-column; SE/LL/UL/p cells of that row are empty.
PREUVE: test-regression_dispatch_engines.R "fit_stats_layout enum validates + propagates to attr" (défaut first_col); le placement dans la première sous-colonne numérique avec cellules restantes vides n'est pas asserté

### rd-core:fit-stats-layout-merged-support [partial]
man/table_regression.Rd:404-423
PROMESSE: fit_stats_layout="merged" merges the model's numeric sub-columns into one centred wide cell; merging is supported by excel, flextable, and word only -- gt, tinytable, clipboard, and default always render first_col regardless.
TEST ATTENDU: flextable output under 'merged' has merged cells for fit rows; the console/gt render of the same call is byte-identical to first_col.
PREUVE: test-regression_dispatch_engines.R "Excel fit_stats_layout = 'merged' inserts merged cells" + "flextable ... emits colspan in fit-stat rows" + "'merged' warns for engines without body-cell merge (tinytable, gt)"; word merged et l'identité console/first_col ne sont pas testés

### rd-core:fit-stats-nevents-cox-blank [partial]
man/table_regression.Rd:832-834
PROMESSE: Fit-stat token "n_events" is the Cox number of events, blank for other classes.
TEST ATTENDU: coxph shows the event count; an lm column with the same token shows a blank/dash cell.
PREUVE: valeur cox couverte (test-survival_footer.R "coxph: events as fit-stat row"); la cellule blanche du token n_events pour une classe non-Cox n'est pas testée (seul l'analogue n_groups l'est, test-cov100-dispatch_render.R)

### rd-core:fit-stats-r2-lm-only [partial]
man/table_regression.Rd:835-836
PROMESSE: "r2", "adj_r2", "omega2" are variance-explained tokens for lm only.
TEST ATTENDU: lm values match summary(fit)$r.squared etc.; requesting them on an all-glm table errors or en-dashes per the mixed rule.
PREUVE: valeurs lm couvertes (test-fit-stats-defaults.R, test-table_regression.R "custom tokens (omega2, sigma, AIC) appear"); le refus/en-dash de r2/adj_r2/omega2 sur all-glm n'est testé que pour un fit mixte (test-random_effects_rows.R "m4: lm-only fit-stat tokens on a mixed fit are rejected with a pointer")

### rd-core:fit-stats-tjur-binomial-only [partial]
man/table_regression.Rd:837-840
PROMESSE: "pseudo_r2_tjur" (Tjur 2009) is binomial only.
TEST ATTENDU: Binomial glm value matches mean(fitted|y=1) - mean(fitted|y=0); poisson glm requesting it is refused.
PREUVE: test-regression_frame_lm.R "carries pseudo_r2 with all three methods" (tjur fini, binomial) + garde-fous NA (test-cov-glm_compute.R); le refus explicite pour un glm poisson n'est pas testé

### rd-core:i18n-attrs-postprocessable [partial]
man/table_regression.Rd:1276-1281
PROMESSE: Output is English; user-facing strings are overridable via reference_label, model_labels, outcome_labels, labels; title and footer are post-processable via attr(result, "title") and attr(result, "note").
TEST ATTENDU: attr(result,'title') and attr(result,'note') are character and re-assignable before printing.
PREUVE: attr(result,'title')/attr(result,'note') are character and asserted extensively (test-table_regression.R:16-18, :682-731; test-regression_broom.R 'as.data.frame – strips spicy classes, keeps title/note'); label overrides tested (labels :570-592, outcome_labels/model_labels in test-multinom_columns.R). No test re-assigns attr(result,'title'/'note') on a regression table and prints (that pattern is tested only for cross_tab, test-cross_tab.R:801).

### rd-core:models-fit-only-api [partial]
man/table_regression.Rd:70-71
PROMESSE: Raw data + formula is not accepted -- fit-only API.
TEST ATTENDU: table_regression(y ~ x) or table_regression(data = df, formula = ...) errors rather than fitting a model.
PREUVE: test-regression_validate_branches.R "validate_models_input – data.frame errors spicy_unsupported with redirect to lm()" couvre data.frame; l'entrée formule brute (y ~ x) n'est pas testée

### rd-core:multinom-tidy-long-structured-wide [partial]
man/table_regression.Rd:992-995
PROMESSE: tidy() and output="long" always return the long form ('<category>: <term>' rows) whatever the display; as_structured() mirrors the displayed table (one column set per category).
TEST ATTENDU: tidy() of a columns-layout multinom table is long; as_structured() has one column set per category.
PREUVE: Long form covered: test-multinom_columns.R 'tidy() and output='long' keep the long prefixed form' (line 184, '<category>: <term>' rows in both tidy and output='long'). But no test that as_structured() of a columns-layout multinom mirrors the display with one column set per category (no as_structured call in test-multinom_columns.R; test-regression_structured.R and test-structured-parity.R only use lm fits).

### rd-core:output-values [partial]
man/table_regression.Rd:657-662,1243-1258
PROMESSE: output selects the return type: "default" (printable spicy_regression_table), "data.frame"/"long" (raw data), "gt"/"flextable"/"tinytable" (rich tables requiring the Suggests package), "excel" (writes via openxlsx2 to excel_path), "clipboard" (clipr::write_clip), "word" (flextable::save_as_docx to word_path).
TEST ATTENDU: Each output value returns/produces the documented object type; missing Suggests package gives an actionable error.
PREUVE: tests/testthat/test-regression_dispatch_engines.R exercises excel (l.20/35), clipboard mocked (l.48), word (l.85), tinytable (l.105), gt (l.148), flextable (l.164); data.frame in test-table_regression.R l.141; long in test-regression_broom.R l.204; default class in test-table_regression.R l.10. The 'missing Suggests package gives an actionable error' branch for render engines is not tested (tests use skip_if_not_installed; spicy_missing_pkg tests exist only for model classes, not output engines).

### rd-core:p-adjust-methods-default [partial]
man/table_regression.Rd:231-236
PROMESSE: p_adjust is one of "none" (default), "holm", "hochberg", "hommel", "bonferroni", "BH"/"fdr", "BY", delegated to stats::p.adjust.
TEST ATTENDU: Each method's adjusted p equals stats::p.adjust(raw_p, method) over the coefficient family.
PREUVE: test-regression_transform.R "bonferroni multiplies p by family size" + "holm respects monotonicity" + "invalid p_adjust errors"; hochberg/hommel/BH/BY/fdr et l'égalité avec stats::p.adjust pour ces méthodes ne sont pas testés

### rd-core:p-digits-apa [partial]
man/table_regression.Rd:593-596
PROMESSE: p_digits (default 3L) formats p, ame_p, p_change APA-strict: leading zero stripped, and small values as <.001 (scaling with p_digits, e.g. <.0001).
TEST ATTENDU: p=0.0004 renders '<.001' at p_digits=3 and '.0004' or '<.0001' appropriately at p_digits=4; no p cell starts with '0.'.
PREUVE: The shared helper is pinned exactly in test-table_continuous_lm.R 'format_p_value derives threshold from digits' (line 1660: .045, <.001 at 3, <.0001 at 4 — leading zero stripped, threshold scales), and table-level leading-zero stripping is asserted for a real table_regression in test-multinom_columns.R 'columns-layout cells match the summary(fit) oracle' (line 41, sub('^0','',...)). But no test drives table_regression(p_digits=) through the rendered p/ame_p/p_change cells; only type validation exists (test-table_regression.R line 437, p_digits='three' errors).

### rd-core:padding-default-headers-centered [partial]
man/table_regression.Rd:624-630
PROMESSE: padding (non-negative integer, default 0L) adds extra characters to each data column's auto width in the default print; headers stay centered above the data region regardless of padding.
TEST ATTENDU: padding=4 widens every column by 4 characters vs padding=0; header centering preserved; padding=-1 rejected.
PREUVE: Widening tested: test-regression_glm.R 'table_regression: padding arg controls column spacing' (line 2405, padding=4L wider than 0L) and 'padding = 0L produces the most compact output' (line 2417, default attr 0L); negative/NA/legacy-string padding rejected at the shared renderer level in test-tables_ascii.R (lines 52, 68, build_ascii_table padding=-1L errors). Not tested: header centering above the data region under padding, and the exact per-column +N widening.

### rd-core:re-columns-display-only [partial]
man/table_regression.Rd:468-476
PROMESSE: re_columns is a subset of c("est","se","ci") with "est" mandatory; deselected SE/CI cells render as en-dash on RE rows only; display-only -- broom::tidy() and as_structured() always carry the full SE + CI.
TEST ATTENDU: re_columns='est': RE SE/CI cells are dashes but tidy() of the same object has numeric std.error/conf.low; re_columns='se' (no est) errors.
PREUVE: test-random_effects_api_args.R "re_columns = 'est' en-dashes SE and CI on the RE rows" (+ variantes est/se, est/ci) + "re_columns must include 'est'"; le fait que tidy()/as_structured() portent toujours SE+CI complets sous désélection n'est pas testé

### rd-core:re-test-never-corr-residual [partial]
man/table_regression.Rd:520-523
PROMESSE: Correlation and residual rows are never tested (correlation tested jointly with its slope; residual has no zero-variance null); the whole-block footer LR test is unaffected by re_test.
TEST ATTENDU: re_test='lrt' table: correlation and Residual rows keep empty p cells; footer LRT identical to re_test='none'.
PREUVE: test-re_test.R "lrt works for glmmTMB and never tests rho / residual" couvre corr/résiduel; l'invariance du LRT de bloc du footer sous re_test n'est pas testée

### rd-core:reference-label-default [partial]
man/table_regression.Rd:373-376
PROMESSE: reference_label default is "(ref.)"; it is ignored by the annotation/footer/none modes.
TEST ATTENDU: reference_label='(base)' changes the row suffix; the same argument under reference_style='footer' changes nothing.
PREUVE: test-regression_render.R "render – reference_label customisation" + défaut '(ref.)' asserté incidemment; l'ignorance de reference_label sous annotation/footer/none n'est pas testée

### rd-core:reference-ordered-ame-synthetic-row [partial]
man/table_regression.Rd:362-371
PROMESSE: Ordered factors with "ame" in show_columns get a synthetic reference row anchored on levels()[1] (since contr.poly .L/.Q/.C rows have no per-level reference), with the same reference_style handling; the '[vs <ref>]' annotation attaches to the first AME row, not the polynomial-trend rows.
TEST ATTENDU: lm with an ordered predictor + show_columns=c('b','ame'): a levels()[1] reference row appears; annotation mode marks the first AME row.
PREUVE: test-regression_ame_factor_ordering.R "ordered factor + AME: synthetic reference row is emitted" + "ordered factor WITHOUT AME: no synthetic reference row" + "2-level ordered factor + AME: ref row + .L + 1 AME row, in order"; le marquage annotation '[vs <ref>]' sur la première ligne AME n'est pas testé

### rd-core:return-class-attributes [partial]
man/table_regression.Rd:733-742
PROMESSE: output="default" returns an object of classes c("spicy_regression_table","spicy_table","data.frame") carrying rendering attributes (title, note, align, padding) and provenance attributes (outcome, model_ids); other output values return the format-specific object (gt_tbl, flextable, tinytable, data.frame, tbl_df, or invisible(x) for side-effect outputs).
TEST ATTENDU: class(result) equals the documented vector; attr(result,'title'), 'note', 'align', 'padding', 'outcome', 'model_ids' are non-NULL; output='clipboard' returns invisibly.
PREUVE: Classes + title/note: test-table_regression.R 'default output: spicy_regression_table class + attrs' (l.10-21); align attr (l.597, l.608); padding attr test-regression_glm.R l.2421. But attr(result,'outcome') and attr(result,'model_ids') are never directly asserted (only reachable via tidy() columns in test-regression_broom.R), and invisibility of side-effect outputs is only a comment (l.24) not an assertion.

### rd-core:show-columns-atomic-tokens [partial]
man/table_regression.Rd:242-254,767-799
PROMESSE: show_columns accepts the atomic tokens b, se, ci, t, p, beta, n, n_events, pd, rhat, ess_bulk, ess_tail, mcse, ame, ame_se, ame_ci, ame_p, rmst(_se/_ci/_p), risk_diff(_se/_ci/_p), partial_f2(_ci), partial_eta2(_ci), partial_omega2(_ci), partial_chi2.
TEST ATTENDU: Each token on an eligible model class adds exactly one column; an unknown token errors.
PREUVE: la plupart des tokens sont testés individuellement (ame_* test-ame_mixed.R, pd/rhat/mcse test-stan_bayes_gates_re.R, rmst/risk_diff test-survival_estimands.R, n_events test-n_events_column.R, partial_* test-table_regression.R) + "show_columns – unknown token errors"; pas de test d'exhaustivité (ex. token 't' seul jamais testé)

### rd-core:show-columns-beta-autoinject [partial]
man/table_regression.Rd:818-820
PROMESSE: If standardized != "none" and "beta" is not requested, beta is auto-injected after "b".
TEST ATTENDU: standardized='refit' with show_columns=c('b','p') displays b, beta, p in that order.
PREUVE: test-table_regression.R "standardized != 'none' auto-injects 'beta'" (présence); la position 'après b' (b, beta, p) n'est pas assertée

### rd-core:show-columns-dedup-order [partial]
man/table_regression.Rd:815-818
PROMESSE: Duplicates after group expansion are deduplicated, and token order controls displayed column order.
TEST ATTENDU: c('all_b','se') yields one SE column; c('p','b') renders p before B.
PREUVE: dédup testée (test-cov-regression_validate.R "duplicate atom across group + literal is de-duplicated"); l'ordre des tokens contrôlant l'ordre des colonnes affichées n'est pas testé

### rd-core:show-columns-group-token-expansions [partial]
man/table_regression.Rd:801-813
PROMESSE: Group tokens expand to fixed atomic vectors: all_b->b,se,ci,p; all_b_compact->b,se,p; all_b_full->b,se,ci,t,p; all_beta->b,beta,se,ci,p; all_ame->ame,ame_se,ame_ci,ame_p; all_ame_compact->ame,ame_p; all_f2/all_eta2/all_omega2->partial_* + its _ci.
TEST ATTENDU: show_columns='all_b_full' output columns equal show_columns=c('b','se','ci','t','p') output columns, for each preset.
PREUVE: test-cov-regression_validate.R "expand_show_columns – mixed group/atomic expands group, keeps atomic" (all_ame) + dédup all_b; les expansions figées all_b_full/all_beta/all_ame_compact/all_f2/all_eta2/all_omega2 ne sont pas testées

### rd-core:show-columns-n-uv-populated [partial]
man/table_regression.Rd:780-783
PROMESSE: "n" is per-row N populated by table_regression_uv() screens; models without per-row N data drop the column.
TEST ATTENDU: table_regression_uv output shows per-block N; a plain multivariable fit requesting 'n' does not render an N column.
PREUVE: test-regression_uv.R "per-predictor N is shown and the differing-N note fires" (N par bloc dans le screen uv); l'absence de colonne N pour un fit multivariable simple demandant 'n' n'est pas testée

### rd-core:show-columns-partial-chi2-glm-format [partial]
man/table_regression.Rd:776-779
PROMESSE: partial_chi2 (glm only) is the LR chi-square via drop1(test='LRT'), rendered as value (df) to disambiguate factor terms (k-1 df) from numeric terms (1 df).
TEST ATTENDU: glm + 'partial_chi2': values match drop1(fit, test='LRT') and cells display 'x.xx (df)'; lm + 'partial_chi2' errors.
PREUVE: test-regression_glm.R "glm: partial_chi2 matches drop1(test = 'LRT') to machine precision" + "factor term shares term-level chi2 across dummies" (df k-1); le format d'affichage 'x.xx (df)' et le refus pour lm ne sont pas assertés

### rd-core:show-columns-partial-lm-only [partial]
man/table_regression.Rd:773-776
PROMESSE: partial_f2, partial_eta2, partial_omega2 (each with a paired _ci companion) are lm only.
TEST ATTENDU: lm + 'partial_eta2','partial_eta2_ci' renders; glm + 'partial_eta2' errors.
PREUVE: test-table_regression.R "partial_eta2 + partial_eta2_ci render as atomic columns" + oracles effectsize (test-cov-lm_compute.R); le refus de partial_eta2/f2/omega2 sur glm n'est pas testé

### rd-core:title-note-scope-limit [partial]
man/table_regression.Rd:652-655
PROMESSE: Validation messages, the spanner row, and the in-body change-stat rows are not affected by title/note -- they belong to the table structure, not the banner.
TEST ATTENDU: note=FALSE on a nested table keeps the change rows and spanners intact.
PREUVE: Banner suppression itself is tested (test-regression_dispatch_engines.R '`title = FALSE` and `note = FALSE` suppress both banners', line 271) and a multi-model tinytable with note=FALSE/title=FALSE still renders its body (test-cov-regression_dispatch.R line 359). But no test combines title/note toggles with nested=TRUE to assert change-stat rows, spanners, or validation messages are unaffected.

### rd-core:vcov-matrix-lm-glm-all [partial]
man/table_regression.Rd:1005-1006
PROMESSE: lm, glm, glm.nb support all of classical, HC*, CR*, bootstrap, jackknife.
TEST ATTENDU: Each estimator runs on lm/glm/glm.nb without error and matches its oracle (sandwich/clubSandwich/manual resample).
PREUVE: glm: all HC0–HC5 run (test-regression_glm.R 'AUDIT: all HC* variants work for glm'), HC1 pinned to sandwich::vcovHC (test-cov100-vcov_glm.R:176-177), CR2 oracle/E2E tests, bootstrap/jackknife refit-as-glm tests (AUDIT B4) and boot.ci-exact percentile (test-boot_percentile.R). lm: HC3 only a differs-from-classical sanity check (test-regression_frame_lm.R:309), CR2 tested. glm.nb: only the capability set is asserted ('.robust_vcov_support grants glm.nb the full estimator set', test-cov100-vcov_glm.R:96) — no per-estimator run/oracle on glm.nb.

### rd-core:vcov-scalar-recycled-list-mixed [partial]
man/table_regression.Rd:75-77
PROMESSE: A scalar vcov is recycled to all models; a list (one string per model) allows mixed estimators.
TEST ATTENDU: list(m1,m2) with vcov='HC3' applies HC3 to both; vcov=list('classical','HC3') gives model-specific SEs matching sandwich::vcovHC per model.
PREUVE: test-stat_header_and_guards.R "vcov validation still accepts strings and string lists" (list('classical','HC3') accepté); pas de test d'égalité SE par modèle vs sandwich pour un scalaire recyclé sur 2+ modèles

### rd-core:weights-from-fit [partial]
man/table_regression.Rd:1268-1274
PROMESSE: There is no weights argument: weights are extracted from the fit via stats::weights() and used automatically in vcov, AME, standardisation, and weighted_nobs.
TEST ATTENDU: 'weights' is not in formals(table_regression); a weighted lm's weighted_nobs equals sum(weights) and AME uses the weights.
PREUVE: weighted_nobs = sum(w) tested for geeglm (test-regression_frame_geepack.R l.186-208) and lm_robust (test-cov100-frames_misc.R l.124); standardisation pulls stats::weights(fit) (test-cov-standardize_glm.R l.36). But no test asserts 'weights' is absent from formals(table_regression), and no test verifies AME honours the fit's weights.

### rd-core:word-features [partial]
man/table_regression.Rd:695-704
PROMESSE: output="word" (word_path required, default NULL) inherits flextable styling and adds: an auto-numbered caption via Word's SEQ field (consecutive numbering across calls), a re-printed header row on each page break, row split prevention, and an APA-styled italic 'Note.' line (APA 7 s7.14).
TEST ATTENDU: Written .docx contains a SEQ field in the caption, header-repeat and keep-together table properties, and an italic Note. prefix; output='word' without word_path errors.
PREUVE: Written docx + required path tested: test-regression_dispatch_engines.R 'output = word writes a docx file via flextable + officer'; test-regression_validate_branches.R 'validate_output_resources – word without path errors'; italic 'Note.' chunk behavior pinned in test-cov-regression_dispatch.R 'flextable footer accepts a custom note without the Note. prefix' (2-chunk italic split). No test asserts the SEQ auto-numbered caption field, header re-print on page break, or row split prevention in the written docx.

### rd-core:word-template-honoured [partial]
man/table_regression.Rd:711-731
PROMESSE: word_template (default NULL = stock template): the template's header, footer, page size, margins, and named styles are honoured; the table is appended to the template body; the caption is tagged with the Word named style "Table Caption" so its appearance follows the template's style definition.
TEST ATTENDU: A custom .docx template with a modified 'Table Caption' style: the produced file uses the template's page setup and the caption carries that style id.
PREUVE: test-cov-regression_dispatch.R 'output = 'word' renders from a user-supplied template' (line 382) checks only that the file is produced from a minimal template, and 'errors when the template file does not exist' (line 402) checks the spicy_invalid_input guard. No test verifies the template's page setup/header/footer/named styles are honoured or that the caption carries the 'Table Caption' style (grep for 'Table Caption' in tests/testthat: no hits).

### rd-methods:as-structured-col-meta-fields [partial]
man/as_structured.Rd:56-58 + exemple ligne 74
PROMESSE: col_meta est une liste par colonne, indexée par le nom de colonne structuré, contenant token, model_id, precision, p-style, below-threshold et les infos de paire/rôle/label CI ; s$col_meta$B existe pour une table lm par défaut.
TEST ATTENDU: expect_true(!is.null(s$col_meta$B)); expect_true(all(names(s$col_meta) %in% names(s$body) | TRUE)); vérifier la présence des champs token/model_id/precision dans chaque entrée.
PREUVE: tests/testthat/test-regression_structured.R, "structured body: col_meta carries token + precision + p_style" (token/precision/p_style/threshold pour B et p; col_meta$B existe), mais les champs model_id et paire/rôle/label CI ne sont pas testés.

### rd-methods:as-structured-engine-contract-shared [partial]
man/as_structured.Rd:21-26 + 34-36
PROMESSE: La vue structurée est exactement le contrat que consomment les moteurs de sortie internes (Excel, gt, tinytable, flextable, clipboard) — un renderer custom bâti dessus voit les mêmes valeurs que les sorties intégrées.
TEST ATTENDU: Comparer les valeurs numériques de s$body (ex. colonne B) aux cellules correspondantes du workbook openxlsx2 produit par output='excel' et au body du gt_tbl : identiques au niveau de précision affiché.
PREUVE: test-cov100-dispatch_render.R test excel (l'intercept du classeur == coef(m1) à 1e-10) et test-structured-parity.R (string body structuré == console pour ref/outcome), mais aucune comparaison directe s$body ↔ cellules gt/tinytable/flextable/clipboard.

### rd-methods:as-structured-format-spec [partial]
man/as_structured.Rd:63-64
PROMESSE: format_spec contient les défauts globaux de format : marque décimale, digits, style de p, niveau de CI, etc.
TEST ATTENDU: expect_true(all(c('digits','ci_level') %in% names(s$format_spec)) || champs équivalents documentés) ; ci_level == 0.95 par défaut.
PREUVE: tests/testthat/test-regression_structured.R "schema invariants" (decimal_mark, p_style, p_threshold) + "European decimal mark propagates to format_spec"; mais digits et ci_level (présents dans le format_spec, R/regression_structured.R:443+) ne sont jamais vérifiés.

### rd-methods:as-structured-missing-attr-refused [partial]
R/regression_dispatch.R:2642-2651 (gate documenté par le contrat >= 0.12.0)
PROMESSE: Un objet classé spicy_regression_table mais sans attribut « structured » est refusé avec une erreur spicy_invalid_input mentionnant table_regression() >= 0.12.0.
TEST ATTENDU: x <- table_regression(fit); attr(x,'structured') <- NULL; expect_error(as_structured(x), class='spicy_invalid_input', regexp='0\\.12\\.0')
PREUVE: tests/testthat/test-cov-regression_dispatch.R, test_that("as_structured() errors when no structured view is attached") — classe spicy_invalid_input testée après attr(tbl,'structured') <- NULL, mais le message mentionnant '>= 0.12.0' n'est pas pinné (pas de regexp).

### rd-methods:as-structured-na-for-nonapplicable [partial]
man/as_structured.Rd:44-46
PROMESSE: Les cellules sans valeur — lignes de référence de facteur, lignes non applicables en multi-modèles, en-têtes de facteur — sont NA dans body (pas de tiret ni de chaîne vide).
TEST ATTENDU: Pour un modèle avec factor(cyl) : expect_true(all(is.na(unlist(s$body[s$reference_rows, -1])))); idem pour s$factor_header_rows.
PREUVE: Aucun test n'asserte directement is.na(s$body[reference_rows/factor_header_rows, -1]). Preuve indirecte: test-structured-parity.R "M3: reference row is blank (not en-dash) for models lacking the factor" (string body dérivé du NA) et test-cov100-dispatch_render.R "excel: outcome row is overlaid and reference dashes skip models..." (cellules NA dans le classeur).

### rd-methods:as-structured-outcome-labels-by-col [partial]
man/as_structured.Rd:53-55
PROMESSE: outcome_labels_by_col est renseigné pour la outcome_row lorsque outcome_labels est explicite avec deux modèles ou plus, avec le label d'affichage indexé par le nom de la première colonne structurée de chaque modèle.
TEST ATTENDU: table_regression(list(m1,m2), outcome_labels=c('A','B')) : expect_equal(unname(s$outcome_labels_by_col), c('A','B')); names() correspond à la première colonne structurée de chaque modèle.
PREUVE: test-structured-parity.R "B-structured-outcome: as_structured() carries the Outcome row" (labels MPG/HP overlay du string body) et test excel de test-cov100-dispatch_render.R (label dans la 1re sous-colonne de chaque modèle), mais la composante outcome_labels_by_col elle-même (noms = 1re colonne structurée, valeurs = labels) n'est jamais inspectée.

### rd-methods:as-structured-reference-models-by-row [partial]
man/as_structured.Rd:49-52
PROMESSE: reference_models_by_row est indexé par l'indice de ligne de chaque ligne de référence (converti en chaîne) et contient les model_id des seuls modèles qui contiennent réellement le facteur ; les renderers n'affichent le marqueur de référence que dans ces colonnes-là.
TEST ATTENDU: Deux modèles dont un seul contient factor(cyl) : expect_equal(names(s$reference_models_by_row), as.character(s$reference_rows)); le vecteur ne contient que le model_id du modèle avec le facteur ; le rendu (gt/excel) laisse blanc l'autre modèle.
PREUVE: Le comportement renderer est testé (test-structured-parity.R "M3: reference row is blank..." pour le string body; test-cov100-dispatch_render.R test excel: en-dash seulement sous le modèle avec le facteur), mais names(s$reference_models_by_row) et son contenu (model_id) ne sont jamais inspectés directement.

### rd-methods:as-structured-row-indices-integer [partial]
man/as_structured.Rd:47-48
PROMESSE: reference_rows, factor_header_rows, fit_stat_rows, level_rows et outcome_row sont des indices de ligne entiers valides dans body.
TEST ATTENDU: expect_true(is.integer(s$reference_rows)); expect_true(all(s$fit_stat_rows %in% seq_len(nrow(s$body)))) pour chaque composante d'indices.
PREUVE: Les indices sont utilisés comme indices valides (test-regression_structured.R "schema invariants" indexe body avec reference_rows; test-structured-parity.R "B-structured-outcome" indexe avec outcome_row), mais aucun test ne vérifie is.integer() ni l'appartenance à seq_len(nrow(body)) pour les 5 composantes.

### rd-methods:asdf-preserves-title-note [partial]
man/as.data.frame.spicy_regression_table.Rd:29
PROMESSE: Les attributs title et note sont préservés sur le data.frame retourné par as.data.frame() (et as_tibble()).
TEST ATTENDU: expect_identical(attr(as.data.frame(tbl), 'title'), attr(tbl, 'title')); idem pour 'note' ; idem via as_tibble().
PREUVE: test-regression_broom.R "as.data.frame – strips spicy classes, keeps title/note" vérifie title et note sur le data.frame; la préservation via as_tibble() n'est pas testée ("as_tibble – returns tbl_df" ne vérifie que la classe).

### rd-methods:asdf-roundtrip-output-dataframe [partial]
man/as.data.frame.spicy_regression_table.Rd:31-34 (details)
PROMESSE: as.data.frame(table_regression(fit)) est équivalent à table_regression(fit, output = 'data.frame') — round-trip garanti entre les deux voies.
TEST ATTENDU: expect_identical(as.data.frame(table_regression(fit)), table_regression(fit, output = 'data.frame'))
PREUVE: Les deux voies sont testées séparément (test-table_regression.R "output = 'data.frame' returns plain data.frame"; test-regression_broom.R "as.data.frame – same row content as default output") mais aucun test ne compare as.data.frame(table_regression(fit)) à table_regression(fit, output='data.frame').

### rd-methods:astibble-returns-tbldf [partial]
man/as.data.frame.spicy_regression_table.Rd:21-23
PROMESSE: as_tibble() sur un spicy_regression_table retourne un tbl_df (mêmes cellules que la vue data.frame).
TEST ATTENDU: tb <- tibble::as_tibble(tbl); expect_s3_class(tb, 'tbl_df'); expect_identical(as.data.frame(tb)[], as.data.frame(tbl)[] à attributs près).
PREUVE: tests/testthat/test-regression_broom.R, test_that("as_tibble – returns tbl_df") vérifie la classe tbl_df; l'égalité des cellules avec la vue data.frame n'est pas testée.

### rd-methods:flextable-output-tagged-class [partial]
man/as_flextable.spicy_flextable.Rd:18-21
PROMESSE: table_regression() et table_continuous_lm() avec output='flextable' retournent un flextable portant le tag de classe spicy_flextable (dont le seul rôle est le styling HTML de la note).
TEST ATTENDU: ft <- table_regression(fit, output='flextable'); expect_s3_class(ft, 'spicy_flextable'); expect_s3_class(ft, 'flextable'); idem pour table_continuous_lm().
PREUVE: table_continuous_lm: explicite (test-tclm_notes.R "flextable output adds the note footer..." expect_s3_class(ft,'spicy_flextable')). Pour table_regression, seul 'flextable' est asserté (test-table_regression.R "output = 'flextable' returns a flextable object"); le tag n'est prouvé qu'indirectement via le dispatch knitr::knit_print (test-quarto_word_rendering.R).

### rd-methods:glance-column-contract [partial]
man/tidy.spicy_regression_table.Rd:40-41
PROMESSE: glance() garantit les colonnes : model_id, outcome, nobs, weighted_nobs, r.squared, adj.r.squared, omega2, sigma, rmse, f2, AIC, AICc, BIC, deviance, df.residual.
TEST ATTENDU: expect_identical(names(glance(tbl)), c('model_id','outcome','nobs','weighted_nobs','r.squared','adj.r.squared','omega2','sigma','rmse','f2','AIC','AICc','BIC','deviance','df.residual'))
PREUVE: test-cov-regression_broom.R "glance – zero-row fit-stats attr → empty broom-shaped tibble" pinne exactement les 15 colonnes via expect_named, mais seulement sur la branche VIDE; sur un modèle réel, "glance – broom-canonical column names" ne vérifie qu'un sous-ensemble de 6 noms via %in%.

### rd-methods:output-class-mapping [partial]
man/table_regression.Rd:739-742 (Value)
PROMESSE: Pour les autres valeurs d'output, le retour est l'objet du format : gt_tbl (gt), flextable (flextable), tinytable (tinytable), data.frame ('data.frame'), tbl_df ('long'), et invisible(x) pour les sorties à effet de bord (excel/word/clipboard).
TEST ATTENDU: expect_s3_class(table_regression(fit, output='gt'), 'gt_tbl'); ... output='tinytable' -> 'tinytable'; output='long' -> 'tbl_df'; expect_invisible(table_regression(fit, output='excel', file=tempfile(fileext='.xlsx')))
PREUVE: test-table_regression.R : gt_tbl ("output = 'gt' returns a gt_tbl object"), flextable, tinytable, data.frame testés; 'long' vérifie les colonnes broom mais pas la classe tbl_df; l'invisibilité du retour pour excel/word/clipboard n'est jamais assertée (expect_invisible absent des chemins output).

### rd-methods:print-header-display-labels-deduped [partial]
R/regression_dispatch.R:2679-2700 (contrat display vs programmatique, adossé au schéma col_meta de as_structured.Rd:56-58)
PROMESSE: En multi-colonnes dupliquées, la console affiche les labels nus (B | 95% CI | p | AME | 95% CI | p) tandis que la vue data.frame conserve les noms programmatiques dédupliqués/préfixés (95% CI.2, Model X: ...).
TEST ATTENDU: Table avec show_ame : capture.output(print(tbl)) ne contient pas '95% CI.2' ; names(as.data.frame(tbl)) contient le nom dédupliqué unique.
PREUVE: test-table_regression.R "spanner – multi-model print strips 'Label: ' prefix from headers" (console sans 'A: B', names() avec 'Model 1: B' via "two models: per-model column groups") + unité make_unique_col_name (test-cov-regression_render.R "appends .3 after two prior collisions"); mais aucun test avec show_ame vérifiant que la console ne montre pas '95% CI.2' pendant que names(as.data.frame(tbl)) le conserve.

### rd-methods:print-honors-padding-attr [partial]
man/table_regression.Rd:625-628 (padding consommé par le print par défaut) + R/regression_dispatch.R:2716-2721
PROMESSE: print() utilise l'attribut padding stocké au call-site par table_regression(), sauf si l'utilisateur le surcharge via print(x, padding=).
TEST ATTENDU: t0 <- table_regression(fit, padding=0L); t4 <- table_regression(fit, padding=4L); les largeurs de capture.output diffèrent ; capture.output(print(t0, padding=4L)) == capture.output(print(t4)) (mêmes largeurs).
PREUVE: test-regression_glm.R "table_regression: padding arg controls column spacing" (padding call-site consommé par print) et "padding = 0L ..." (attr stocké); la surcharge print(x, padding=) n'est testée nulle part.

### rd-methods:regression-table-carries-attrs [partial]
man/table_regression.Rd:736-739 (Value)
PROMESSE: Le résultat porte les attributs de rendu title, note, align, padding et les attributs de provenance outcome, model_ids, consommés par la méthode print et les méthodes broom.
TEST ATTENDU: tbl <- table_regression(fit); expect_true(all(c('title','note','align','padding','outcome','model_ids') %in% names(attributes(tbl))))
PREUVE: title/note (test-table_regression.R "default output: ... class + attrs"), align ("align – 'decimal' is default..."), padding (test-regression_glm.R "padding = 0L produces the most compact output") testés; les attributs de provenance outcome et model_ids ne sont jamais vérifiés sur l'objet retourné.

### rd-methods:regression-table-classes [partial]
man/table_regression.Rd:733-735 (Value)
PROMESSE: Avec output='default', l'objet retourné est un data.frame de classes exactement c('spicy_regression_table','spicy_table','data.frame').
TEST ATTENDU: expect_identical(class(table_regression(fit)), c('spicy_regression_table','spicy_table','data.frame'))
PREUVE: tests/testthat/test-table_regression.R, "table_regression – default output: spicy_regression_table class + attrs" vérifie l'héritage des trois classes via expect_s3_class, mais pas expect_identical(class(out), c(...)) — l'exactitude/ordre du vecteur de classes n'est pas pinnée.

### rd-methods:tidy-column-contract [partial]
man/tidy.spicy_regression_table.Rd:36-37
PROMESSE: tidy() garantit exactement les colonnes : model_id, outcome, outcome_level, term, estimate_type, estimate, std.error, conf.low, conf.high, statistic, df, p.value, test_type, is_intercept, factor_term, factor_level.
TEST ATTENDU: expect_identical(names(tidy(tbl)), c('model_id','outcome','outcome_level','term','estimate_type','estimate','std.error','conf.low','conf.high','statistic','df','p.value','test_type','is_intercept','factor_term','factor_level'))
PREUVE: test-regression_broom.R "tidy – returns broom-canonical column names" ne fait qu'un all(expected %in% names(td)) et sa liste omet outcome_level (couvert séparément dans test-clm_scale_block.R); pas d'expect_identical(names(tidy(tbl)), ...) exact.

### rd-methods:tidy-drops-reference-and-singular [partial]
man/tidy.spicy_regression_table.Rd:35-36
PROMESSE: tidy() supprime les lignes placeholder de niveau de référence des facteurs et les coefficients singuliers (estimate NA) — aucune ligne avec estimate NA n'apparaît.
TEST ATTENDU: Modèle avec factor(cyl) : aucun term de niveau de référence dans tidy(tbl)$term ; modèle avec colinéarité parfaite (coef NA) : le term aliasé est absent ; expect_false(any(is.na(tidy(tbl)$estimate)))
PREUVE: test-regression_broom.R "tidy – drops reference rows (no estimable values)" couvre les références (cyl4 absent); mais "tidy – drops singular coefs (NA estimates)" utilise un modèle SANS coefficient aliasé (mpg~wt+cyl) — aucun test tidy() sur un vrai modèle colinéaire (le cas aliasé n'est testé que côté AME/standardize: test-correctness-sweep.R, test-cov-standardize.R).

### rd-methods:tidy-estimate-type-domain [partial]
man/tidy.spicy_regression_table.Rd:31-32
PROMESSE: estimate_type prend ses valeurs dans c('B','beta','ame','partial_f2','partial_eta2','partial_omega2').
TEST ATTENDU: Sur des tables activant chaque option (show_beta, show_ame, show_effect_size) : expect_true(all(tidy(tbl)$estimate_type %in% c('B','beta','ame','partial_f2','partial_eta2','partial_omega2')))
PREUVE: Chaque valeur est exercée isolément (test-regression_broom.R partial_eta2; test-regression_glm.R estimate_type 'ame'/'beta'; test-clm_scale_block.R 'B'/'ame'), mais aucun test n'asserte que tidy()$estimate_type reste dans le domaine complet c('B','beta','ame','partial_f2','partial_eta2','partial_omega2').

### rd-methods:tidy-glance-return-type [partial]
man/tidy.spicy_regression_table.Rd:19-22
PROMESSE: tidy() et glance() retournent un tbl_df quand tibble est installé, sinon un data.frame simple.
TEST ATTENDU: Avec tibble installé : expect_s3_class(tidy(tbl), 'tbl_df'); expect_s3_class(glance(tbl), 'tbl_df'). (Branche sans tibble : mock requireNamespace.)
PREUVE: test-cov-regression_broom.R "glance – empty result is a tibble" (tbl_df); aucun test n'asserte tidy() -> tbl_df sur le chemin normal, ni la branche fallback data.frame sans tibble (le mock requireNamespace n'existe que pour spicy_continuous_table, test-cov100-descriptive.R).

### rd-methods:tidy-row-grain [partial]
man/tidy.spicy_regression_table.Rd:31-32
PROMESSE: tidy() retourne exactement une ligne par combinaison (model_id, term, estimate_type, outcome_level).
TEST ATTENDU: td <- tidy(tbl); expect_false(any(duplicated(td[c('model_id','term','estimate_type','outcome_level')]))) sur une table multi-modèles avec beta et ame.
PREUVE: Approché par test-regression_broom.R "tidy – multi-model: model_id distinguishes rows" (2 lignes wt) et test-clm_scale_block.R (nrow(ame_age) == 4L, une par niveau), mais aucun test d'unicité duplicated() sur (model_id, term, estimate_type, outcome_level).

### rd-uv-estimands:boot-n-default-1000-validated [partial]
man/table_regression.Rd:157-159 (\item{boot_n})
PROMESSE: boot_n is a single positive integer defaulting to 1000L, controlling the number of bootstrap replicates (used by the estimand bootstrap and vcov = "bootstrap").
TEST ATTENDU: boot_n = 0, -1, 2.5, c(10,20) all error; boot_n = 50 with a seed gives a reproducible CI distinct from boot_n = 1000; default resolves to 1000 replicates.
PREUVE: test-regression_validate_branches.R, test_that("validate_boot_n – non-integer / negative errors") covers only "oops" and -50 at the internal-validator level; boot_n = 0, 2.5, c(10,20), the 1000L default resolution, and a boot_n = 50 vs 1000 behavioral difference are untested.

### rd-uv-estimands:coxph-requires-survival-renders-hr [partial]
man/table_regression_uv.Rd:31-33 (\item{method})
PROMESSE: method = "coxph" requires the survival package and renders estimates as HRs with exponentiate = TRUE.
TEST ATTENDU: coxph screen output: coefficient column headed HR, values equal exp(coef(fit)) of each univariable coxph; with survival unavailable (mocked requireNamespace), a clear actionable error.
PREUVE: HR side covered: test-regression_uv_coxph.R, test_that("univariable rows reproduce the per-predictor coxph fits") (estimates = exp(coef) of each univariable coxph) and test_that("titles, HR note, and the survival footer render") ('HR = hazard ratio'). The requires-survival half is untested: no requireNamespace mock anywhere in the uv tests (grep 'requireNamespace' in test-*uv* returns nothing).

### rd-uv-estimands:estimand-token-families-accepted [partial]
man/table_regression.Rd:250-251,791-798 (\item{show_columns} + Vocabulary tokens)
PROMESSE: show_columns accepts the eight survival-estimand tokens: "rmst", "rmst_se", "rmst_ci", "rmst_p" and "risk_diff", "risk_diff_se", "risk_diff_ci", "risk_diff_p", each rendering its own column in token order.
TEST ATTENDU: Each token individually and in combination produces the corresponding column; token order controls column order; unknown variants error.
PREUVE: test-survival_estimands.R, test_that("the full table renders estimand columns with inference") exercises rmst, rmst_ci, rmst_p, risk_diff, risk_diff_ci together; but rmst_se, risk_diff_se and risk_diff_p are never requested in any test (grep returns nothing), and token-order control of column order is unasserted.

### rd-uv-estimands:example-runs [partial]
man/table_regression_uv.Rd:138-147 (\examples)
PROMESSE: The donttest example (sochealth, outcome smoking, family = binomial(), exponentiate = TRUE) runs without error and produces the OR screen.
TEST ATTENDU: Run the example body verbatim; no error/warning beyond documented ones; OR-labeled columns.
PREUVE: No test runs the \examples body verbatim (donttest; grep 'education' as uv predictor and family=binomial()+exponentiate combo returns nothing); the nearest equivalent is the snapshot test test-regression_uv.R test_that("console snapshot: screen + multivariable merge") (method="glm", age/bmi/sex, exponentiate = TRUE, OR columns) which omits education and the family= spelling.

### rd-uv-estimands:gaussian-identity-refused-glm [partial]
man/table_regression_uv.Rd:42-44 (\item{family})
PROMESSE: gaussian() with the identity link is refused for the glm screen, with the error directing to method = "lm".
TEST ATTENDU: expect_error(table_regression_uv(..., method = "glm", family = gaussian()), regexp mentioning method = "lm"); same for family = "gaussian" and bare gaussian. Non-identity gaussian links (gaussian("log")) are NOT caught by this gate.
PREUVE: test-regression_uv.R, test_that("default titles follow the family; custom title wins") errors on family = gaussian() (no method) with class spicy_invalid_input only; the 'use method = "lm"' message is not asserted, the "gaussian"/bare-gaussian forms are untested, and the gaussian("log") non-catch is untested.

### rd-uv-estimands:lm-multilevel-outcome-refused [partial]
man/table_regression_uv.Rd:124-125 (Why the default screen is linear)
PROMESSE: An outcome with more than two observed levels is refused under method = "lm" (a multinomial outcome has no linear screen).
TEST ATTENDU: expect_error(table_regression_uv(df, outcome = factor_3_levels, predictors = ...), classed condition). Also check: a 3-level factor with only 2 OBSERVED levels in data is allowed (doc says "observed levels").
PREUVE: test-regression_uv.R, test_that("a >2-level outcome under the default is refused with guidance") covers the refusal (class spicy_invalid_data on education); the 'observed levels' nuance (3-level factor with only 2 observed levels allowed) is untested.

### rd-uv-estimands:lm-nongaussian-family-refused [partial]
man/table_regression_uv.Rd:44-46 (\item{family})
PROMESSE: With method = "lm", any non-gaussian family is refused the same way (error directing to method = "glm").
TEST ATTENDU: expect_error(table_regression_uv(..., method = "lm", family = binomial()), regexp mentioning method = "glm").
PREUVE: test-regression_uv.R, test_that("family with method = 'lm': non-gaussian refused, gaussian ignored with a warning"): expect_error(..., "not meaningful", class = "spicy_invalid_input") for family = binomial(); the promised redirection to method = "glm" in the message is not asserted.

### rd-uv-estimands:lpm-01-coding-second-level-named [partial]
man/table_regression_uv.Rd:121-124 (Why the default screen is linear)
PROMESSE: A two-level factor (or logical) outcome is coded 0/1 on its SECOND level (the glm convention), and the LPM warning names the modeled probability.
TEST ATTENDU: factor(c("no","yes")) outcome: coefficients equal lm(as.integer(y == "yes") ~ x); warning message names the second level ("yes") as the modeled probability; same for logical (TRUE modeled).
PREUVE: test-regression_uv.R, test_that("binary outcome under the default warns (LPM) and fits lm") compares only abs(estimate) to abs(lm coef) — the second-level 0/1 coding direction is NOT verified, the warning message naming the modeled level is unasserted, and the logical-outcome case is untested.

### rd-uv-estimands:n-events-fit-stat-cox-default [partial]
man/table_regression.Rd:832-834,906-907 (show_fit_stats)
PROMESSE: As a show_fit_stats token, "n_events" reports the number of events for Cox models (alongside n, the field convention) and renders blank for other classes; Cox fits' class-aware default fit-stats are c("nobs", "n_events", "aic").
TEST ATTENDU: Default table_regression(coxph_fit) shows N, N events, AIC fit-stat rows with N events = fit$nevent; in a mixed coxph + glm table the glm cell for n_events is blank/en-dashed, not an error.
PREUVE: Cox default covered: test-survival_footer.R, test_that("table_regression() coxph: events as fit-stat row, C in footer") (default print shows 'n 228' + 'N events 165') and test-fit-stats-defaults.R, test_that("survreg + coxph fall back to nobs + AIC") (n + AIC); rms::cph row in test_that("rms::cph: events move to the n_events fit-stat row"). The blank/en-dash n_events cell for non-Cox models in a mixed coxph + glm table is untested.

### rd-uv-estimands:n-token-populated-by-uv-else-dropped [partial]
man/table_regression.Rd:781-783 (Vocabulary tokens, sample columns)
PROMESSE: The "n" token is the per-row N populated by table_regression_uv() screens (each predictor block its own fit); models without per-row N data DROP the column silently rather than erroring.
TEST ATTENDU: table_regression(single lm, show_columns = c("n","b","p")): no error and no N column in the output; the same tokens in table_regression_uv() do show N.
PREUVE: The uv-populated half is covered (_snaps/regression_uv.md default N column; test-n_events_column.R uv test), but the silent-drop half is untested: no test calls table_regression(single fit, show_columns containing "n") and asserts no error + no N column (grep for show_columns = c("n", ... outside uv contexts returns nothing; the drop logic lives untested at R/regression_render.R:187-198, 753).

### rd-uv-estimands:predictors-outcome-autodropped [partial]
man/table_regression_uv.Rd:27-29 (\item{predictors})
PROMESSE: The outcome column(s) are dropped from the predictor tidyselect automatically (including both Surv components for coxph); tidyselect helpers like where(is.numeric) are accepted.
TEST ATTENDU: predictors = everything() (or where(is.numeric)) with the outcome inside the selection: no row block for the outcome (nor for time/status under coxph), no error, no outcome-on-outcome fit.
PREUVE: test-regression_uv.R, test_that("the outcome is auto-dropped from tidyselect predictors") covers dplyr::everything() with a glm outcome; but the coxph case (both Surv components time/status dropped) and where(is.numeric) helpers are never tested — no everything()/where() call exists in test-regression_uv_coxph.R or test-uv_estimands.R.

### rd-uv-estimands:show-intercept-default-false [partial]
man/table_regression_uv.Rd:77,131-136 (\item{...} + Intercepts section)
PROMESSE: show_intercept defaults to FALSE in the screen: intercepts hidden on both univariable and multivariable sides (matching gtsummary tbl_regression); show_intercept = TRUE displays them.
TEST ATTENDU: Default output contains no (Intercept) rows in either group; show_intercept = TRUE adds them on both sides.
PREUVE: test-regression_uv.R, test_that("intercepts are hidden by default; show_intercept shows only the multivariable one"): default hidden is fully covered, but show_intercept = TRUE yields exactly 1 intercept row (Multivariable only) — the univariable-side display promised by the Rd wording ('Pass show_intercept = TRUE to display them', hidden 'on both sides') is by design never shown, so the doc-vs-behavior gap is the untested/contradicted half.

### rd-uv-estimands:uv-estimand-bootstrap-per-fit [partial]
man/table_regression_uv.Rd:66-67 (\item{show_columns})
PROMESSE: Each univariable fit runs its own boot_n-replicate bootstrap for the estimand inference (SE/CI/p).
TEST ATTENDU: With boot_n = small value and a fixed seed: per-block rmst_se/rmst_ci are reproducible and match a manual per-predictor bootstrap of the univariable coxph fit (not a shared bootstrap of the full model).
PREUVE: test-uv_estimands.R only verifies POINT estimates against the direct per-predictor fits (test_that("uv-screen estimands equal the directly fit models")); no test requests rmst_se/rmst_ci in the screen or compares per-block bootstrap inference to a manual per-predictor bootstrap; seed reproducibility of the uv bootstrap is unasserted.

### rd-uv-estimands:value-same-output-contract [partial]
man/table_regression_uv.Rd:82-84 (\value)
PROMESSE: The return value follows the same output contract as table_regression(): a spicy_regression_table supporting tidy(), as.data.frame(), as_structured(), print, knit_print, and the output formats.
TEST ATTENDU: Object inherits spicy_regression_table; tidy(), as.data.frame(), as_structured() work; output = "tinytable"/"flextable" render as for table_regression().
PREUVE: tidy() and print() are exercised throughout test-regression_uv.R (and expect_s3_class spicy_regression_table in the family test), output = "long" in test-uv_estimands.R; but as.data.frame(), as_structured(), knit_print and output = "tinytable"/"flextable" are never called on a table_regression_uv() result (grep as_structured/tinytable/flextable in test-*uv* returns nothing).

### rd-vcov-classes:boot-n-default-1000 [partial]
man/table_regression.Rd:157-159
PROMESSE: boot_n (nombre de réplicats quand vcov="bootstrap") est un entier positif unique, défaut 1000L.
TEST ATTENDU: formals(table_regression)$boot_n == 1000L; boot_n=0 ou vecteur => erreur de validation.
PREUVE: test-regression_validate_branches.R 'validate_boot_n – non-integer / negative errors' couvre la validation; la valeur par défaut 1000L (formals) n'est testée nulle part

### rd-vcov-classes:bootstrap-half-failed-warning [partial]
R/vcov.R:314-329
PROMESSE: Si plus de la moitié des réplicats bootstrap échouent (mais >= 10 valides), un warning classé spicy_fallback est émis et le vcov est calculé sur les réplicats valides.
TEST ATTENDU: Cas construit avec ~40% de réplicats valides: warning spicy_fallback mentionnant les comptes échec/total; le résultat reste bootstrap (pas classical).
PREUVE: test-table_continuous_lm.R 'compute_resample_vcov_bootstrap warns when over half of replicates fail' (~75% d'échecs, message 'replicates failed'+'60'); ni la classe spicy_fallback ni le fait que le résultat reste bootstrap (pas classical) ne sont affirmés

### rd-vcov-classes:cluster-bootstrap-resamples-clusters [partial]
man/table_regression.Rd:949-950; R/vcov.R:277-291
PROMESSE: "bootstrap" avec cluster rééchantillonne des clusters ENTIERS (Cameron-Gelbach-Miller); sans cluster, bootstrap non paramétrique par observation.
TEST ATTENDU: Avec set.seed fixé, le vcov cluster-bootstrap diffère du vcov obs-bootstrap et reproduit un cluster-bootstrap manuel (mêmes tirages sample(unique_g)).
PREUVE: test-table_continuous_lm.R 'cluster bootstrap differs from obs bootstrap on paired data' (même seed, SE différents) + 'compute_resample_vcov_bootstrap cluster path produces a finite vcov' + test-cov100-titlefooter.R 'cluster bootstrap footer names the scheme, count, and cluster'; pas de reproduction manuelle du tirage sample(unique_g) (oracle Cameron-Gelbach-Miller)

### rd-vcov-classes:cluster-formula-interaction [partial]
man/table_regression.Rd:105,1051-1053,1330
PROMESSE: La forme formule compose l'interaction de deux variables: cluster=~region:year clusterise sur la clé croisée.
TEST ATTENDU: cluster=~region:age_group == cluster=interaction(df$region, df$age_group) (SE identiques).
PREUVE: test-regression_glm.R 'cluster - formula with interaction (~region:year)' vérifie seulement le footer 'clusters by region:year'; l'égalité numérique avec cluster=interaction(region, year) n'est pas testée

### rd-vcov-classes:cr-missing-clubsandwich-error [partial]
R/vcov.R:145-156
PROMESSE: CR* sur une classe clubSandwich sans le package installé lève spicy_invalid_input avec l'instruction install.packages("clubSandwich") (guard Suggests actionnable).
TEST ATTENDU: Avec clubSandwich masqué (callr + libpath), lm + CR2 => erreur classée mentionnant install.packages.
PREUVE: test-table_continuous_lm.R 'compute_model_vcov simulates clubSandwich missing for CR types' (mock requireNamespace, message contient 'clubSandwich'); ni la classe spicy_invalid_input ni le hint install.packages("clubSandwich") ne sont affirmés

### rd-vcov-classes:cr-only-class-set [partial]
man/table_regression.Rd:1011-1016; R/vcov.R:818-858
PROMESSE: lmer, lme, glmmTMB, coxph, survreg, mgcv::gam/bam, polr, clm, betareg, svyglm, nnet::multinom et rms (ols/lrm/cph/Glm) supportent classical + CR* seulement; HC* et les resamplers (qui refitteraient lm/glm) y sont refusés.
TEST ATTENDU: Pour chaque classe listée: vcov="HC3" ET vcov="bootstrap" => spicy_unsupported_vcov; vcov="CR2"+cluster réussit.
PREUVE: Refus testés: gam (test-robust-vcov-glm-classes.R 'gam HC* and bootstrap are refused'), rms ('rms HC* / bootstrap are refused'), coxph ('survival HC* / bootstrap are refused'), lmer HC3 (test-robust-vcov-mixed.R), multinom (test-robust-vcov-multinom.R), pscl HC (test-component_blocks.R) + sets .robust_vcov_support pour bam/lrm/cph/Glm (test-cov100-vcov_glm.R); pas de test de refus HC*/resamplers pour lme, glmmTMB, polr, clm, betareg, svyglm, survreg

### rd-vcov-classes:cr-vcovcr-failure-fallback-warning [partial]
R/vcov.R:157-173
PROMESSE: Si clubSandwich::vcovCR échoue au calcul, warning classé spicy_fallback (erreur sous-jacente incluse) et fallback au vcov classique — jamais silencieux.
TEST ATTENDU: Cas d'échec vcovCR: warning spicy_fallback « Falling back to the classical »; matrice retournée == stats::vcov(fit).
PREUVE: test-table_continuous_lm.R 'compute_model_vcov CR fallback warns when clubSandwich errors' (message 'Cluster-robust' + erreur sous-jacente) et 'compute_model_vcov CR fallback returns the classical vcov after warning'; la classe spicy_fallback n'est pas affirmée

### rd-vcov-classes:hc-failure-fallback-warning [partial]
R/vcov.R:72-90
PROMESSE: Si sandwich::vcovHC échoue, un warning classé spicy_fallback est émis (avec l'erreur sous-jacente) et le vcov classique est utilisé — la dégradation n'est jamais silencieuse.
TEST ATTENDU: Fit construit pour faire échouer vcovHC: warning spicy_fallback contenant "Falling back to the classical"; résultat == stats::vcov(fit).
PREUVE: test-table_continuous_lm.R 'compute_model_vcov falls back to classical vcov when sandwich errors' (warning 'Falling back to the classical OLS variance' + erreur sous-jacente) et 'compute_model_vcov HC fallback returns the classical vcov after warning'; la classe spicy_fallback du warning n'est pas affirmée

### rd-vcov-classes:lm-glm-regime-shared-b-ame [partial]
man/table_regression.Rd:959-963
PROMESSE: Pour lm/glm, B et AME partagent le même régime inférentiel: classical/HC* -> t avec df.residual (lm) / z (glm).
TEST ATTENDU: lm + HC3: df des B ET des AME == df.residual(fit); glm + HC3: df == Inf pour les deux familles de colonnes.
PREUVE: Côté glm couvert: test-regression_glm.R 'glm AME: HC* vcov uses z-asymptotic (no Satterthwaite)' et 'glm AME: classical vcov uses z-asymptotic'; côté lm, df AME == df.residual sous HC* non affirmé (seul le df des B est vérifié dans test-cov100-vcov_glm.R '.apply_robust_vcov_to_coefs...')

### rd-vcov-classes:registry-Glm [partial]
man/table_regression_models.Rd:74
PROMESSE: Registre Glm (rms::Glm): AME oui, exponentiate dépendant du lien.
TEST ATTENDU: Glm poisson + exponentiate => IRR; ame disponible.
PREUVE: test-regression_frame_rms.R 'Glm Poisson: supports$exponentiate = TRUE' + schéma; l'en-tête IRR effectif et l'AME Glm ne sont pas testés

### rd-vcov-classes:registry-brmsfit [partial]
man/table_regression_models.Rd:76
PROMESSE: Registre brmsfit (brms::brm): AME oui (draws), exponentiate dépendant du lien, block Random effects si multiniveau.
TEST ATTENDU: brm gaussian simple: pas de bloc RE; brm avec (1|g): bloc RE; ame draws-native.
PREUVE: test-regression_frame_stan.R (frame brmsfit: médiane/MAD SD, pd, supports Bayésiens, exp TRUE logit) + test-stan_bayes_gates_re.R 'brms family spellings map to the shared exp machinery'; le bloc RE pour un brm multiniveau (vs absence en simple) n'est pas testé (tests brms lourds locaux-only)

### rd-vcov-classes:registry-cph [partial]
man/table_regression_models.Rd:66
PROMESSE: Registre cph (rms::cph): AME NON, exponentiate HR.
TEST ATTENDU: cph: exp => HR; demande d'ame => refus.
PREUVE: test-regression_frame_rms.R (frame cph, famille cox/log, Nagelkerke+Dxy) + test-robust-vcov-rms.R (CR* Lin-Wei); l'en-tête HR exponentié et le refus d'AME spécifiques à cph ne sont pas testés (seul coxph l'est)

### rd-vcov-classes:registry-fixest [partial]
man/table_regression_models.Rd:51
PROMESSE: Registre fixest (feols/feglm/fepois/fenegbin): supporté, AME oui; exponentiate OR/IRR pour feglm.
TEST ATTENDU: table_regression(feols_fit) et feglm poisson + exponentiate => IRR; ame disponible.
PREUVE: test-regression_frame_fixest.R (feols/fepois frames, 'fepois: supports$exponentiate = TRUE (IRR)') + test-exponentiate-classes.R 'exponentiate: fixest fepois (Poisson log) -> IRR'; AME fixest non testé

### rd-vcov-classes:registry-flexsurvreg [partial]
man/table_regression_models.Rd:67
PROMESSE: Registre flexsurvreg: AME non, exponentiate TR/HR selon la distribution, block « distribution parameters ».
TEST ATTENDU: flexsurvreg weibull: bloc paramètres de distribution rendu; exp => TR ou HR selon dist; ame refusé.
PREUVE: test-regression_frame_flexsurv_selection.R (frame, shape/scale stashés en extras, supports$exponentiate TRUE) + test-stat_header_and_guards.R 'AME request on an incapable class is refused...' (flexsurv ame refusé); le rendu du bloc 'distribution parameters' dans la table n'est pas testé

### rd-vcov-classes:registry-gls [partial]
man/table_regression_models.Rd:56
PROMESSE: Registre gls (nlme::gls): supporté, AME oui, pas de blocks.
TEST ATTENDU: table_regression(gls_fit) rend sans bloc RE; ame disponible.
PREUVE: test-regression_frame_nlme.R 'as_regression_frame.gls produces a schema-valid frame' + 'gls: info$n_groups is NULL (no random effects)' + 'gls: random_effects$icc is NA'; AME gls non testé

### rd-vcov-classes:registry-iv-robust [partial]
man/table_regression_models.Rd:47
PROMESSE: Registre iv_robust (estimatr::iv_robust): supporté, AME oui.
TEST ATTENDU: table_regression(iv_robust_fit) rend; ame disponible.
PREUVE: test-regression_frame_estimatr.R 'as_regression_frame.iv_robust produces a schema-valid frame' + 'iv_robust: SE / p byte-match summary'; AME iv_robust non testé

### rd-vcov-classes:registry-ivreg [partial]
man/table_regression_models.Rd:48
PROMESSE: Registre ivreg (AER::ivreg): supporté, AME oui.
TEST ATTENDU: table_regression(ivreg_fit) rend; ame disponible.
PREUVE: test-regression_frame_quantreg_AER.R 'as_regression_frame.ivreg produces a schema-valid frame' + 'ivreg: SE / p byte-match summary' + oracle parameters; AME ivreg non testé

### rd-vcov-classes:registry-lm-robust [partial]
man/table_regression_models.Rd:46
PROMESSE: Registre lm_robust (estimatr::lm_robust): supporté, AME oui.
TEST ATTENDU: table_regression(lm_robust_fit) rend; ame disponible.
PREUVE: test-regression_frame_estimatr.R (frame lm_robust, SE/df byte-match, vcov_label) + test-cov-cluster-robust-df.R 'estimatr lm_robust CR2: per-coefficient (varying) df...' (via table_regression); la disponibilité AME pour lm_robust n'est pas testée

### rd-vcov-classes:registry-lrm [partial]
man/table_regression_models.Rd:73
PROMESSE: Registre lrm (rms::lrm): AME oui, exponentiate OR.
TEST ATTENDU: lrm + exponentiate => OR; ame disponible.
PREUVE: test-regression_frame_rms.R 'lrm: supports$exponentiate = TRUE (odds ratios)' + frame lrm (Nagelkerke, C-index); l'AME lrm et l'en-tête OR effectif ne sont pas testés

### rd-vcov-classes:registry-mlogit [partial]
man/table_regression_models.Rd:61
PROMESSE: Registre mlogit: AME NON, exponentiate OR, lignes par alternative.
TEST ATTENDU: mlogit: demande d'ame => refus (pas de méthode slopes); exp => OR; rendu per-alternative.
PREUVE: test-ame-by-class.R 'mlogit advertises no AME' (supports$ame FALSE) + test-mlogit_two_segment.R (rendu par alternative, oracle summary); exponentiate OR sur mlogit non testé

### rd-vcov-classes:registry-negbin [partial]
man/table_regression_models.Rd:43
PROMESSE: Registre negbin (MASS::glm.nb): AME oui; exponentiate IRR.
TEST ATTENDU: glm.nb + exponentiate=TRUE => en-tête IRR; colonne ame remplie.
PREUVE: test-exponentiate-classes.R 'exponentiate: MASS::glm.nb -> IRR' + test-regression_frame_MASS.R (frame negbin) + set vcov complet (cov100); l'AME sur glm.nb n'est testé nulle part

### rd-vcov-classes:registry-ols [partial]
man/table_regression_models.Rd:72
PROMESSE: Registre ols (rms::ols): AME oui, pas d'exponentiate.
TEST ATTENDU: table_regression(ols_fit): ame disponible; exponentiate identité => warn/no-op.
PREUVE: test-regression_frame_rms.R 'ols: Wald-t...' + test-robust-vcov-rms.R (CR* robcov); ni l'AME ni le no-op exponentiate=TRUE sur ols ne sont testés

### rd-vcov-classes:registry-selection [partial]
man/table_regression_models.Rd:71
PROMESSE: Registre selection (sampleSelection::selection): AME non, block composant de sélection.
TEST ATTENDU: selection fit: le composant sélection rend en bloc; ame refusé.
PREUVE: test-regression_frame_flexsurv_selection.R 'table_regression() body shows separate selection vs outcome rows' + blocs selection/outcome dans le frame; le refus d'AME pour selection n'est pas testé

### rd-vcov-classes:registry-tobit [partial]
man/table_regression_models.Rd:49
PROMESSE: Registre tobit (AER::tobit): supporté, AME oui.
TEST ATTENDU: table_regression(tobit_fit) rend; ame disponible.
PREUVE: test-regression_frame_quantreg_AER.R 'as_regression_frame.tobit produces a schema-valid frame' + dv/censure/famille; AME tobit non testé

### rd-vcov-classes:rq-footer-names-estimator [partial]
man/table_regression_models.Rd:202-206
PROMESSE: Pour rq, le footer nomme l'estimateur choisi (nid par défaut, iid, ker, rank, bootstrap).
TEST ATTENDU: attr(result,"note") d'un rq par défaut mentionne nid; sous vcov="bootstrap" mentionne le bootstrap.
PREUVE: Frame-level label tested: test-rq_vcov_family.R:21 expect_match(fr$info$vcov_label, 'nid') for the default and :74 'Rank inversion' for vcov='rank'. No test asserts attr(result,'note') of a rendered rq table names the estimator, and the bootstrap label variant is not asserted (the bootstrap test :84 pins SEs/inference only).

### rd-vcov-classes:rq-nid-token-accepted [partial]
R/vcov.R:873,895-904
PROMESSE: rq accepte aussi le token explicite "nid" (identique à "classical").
TEST ATTENDU: vcov="nid" et vcov="classical" sur le même rq donnent des tables identiques.
PREUVE: test-rq_vcov_family.R 'rq cluster works only through the wild gradient bootstrap' prouve que le token 'nid' est reconnu (atteint la garde cluster, pas un refus unknown-token); l'identité table nid == classical n'est testée nulle part

### rd-vcov-classes:rq-rank-no-vcov-matrix-abort [partial]
R/vcov.R:928-946
PROMESSE: rq "rank" combiné à une demande nécessitant une matrice vcov (colonnes AME) aborte spicy_unsupported_vcov avec le hint d'utiliser "nid" ou "bootstrap".
TEST ATTENDU: rq + vcov="rank" + show_columns AME => erreur classée mentionnant « no variance-covariance matrix exists ».
PREUVE: test-rq_vcov_family.R 'rq rank inversion renders CIs only...' teste le refus rank+ame mais avec classe spicy_invalid_input (pas la spicy_unsupported_vcov promise) ; 'rq internal vcov backend refuses what the gate refuses' épingle compute_model_vcov(rank) => spicy_unsupported_vcov 'rank-inversion'; le hint 'no variance-covariance matrix exists'/nid-bootstrap n'est pas vérifié

### rd-vcov-classes:same-fit-multi-vcov [partial]
man/table_regression.Rd:954-957,1350-1357
PROMESSE: Le même fit peut apparaître plusieurs fois dans la liste avec des estimateurs différents pour comparer les SE côte à côte (vcov=list + cluster=list, NULL admis par position).
TEST ATTENDU: L'exemple list(Classical=fit,HC3=fit,CR2=fit), vcov=list("classical","HC3","CR2"), cluster=list(NULL,NULL,~region) rend une table 3 colonnes-modèles avec SE distinctes.
PREUVE: test-regression_glm.R 'cluster - list of mixed forms (formula / string / vector) for multi-model' et test-table_regression.R 'cluster_name – list(...) with named elements per model' répètent le même fit avec listes vcov/cluster (CR2 partout); l'exemple documenté classical/HC3/CR2 côte à côte avec NULL par position n'est testé nulle part

### rd-vcov-classes:shared-programmatic-methods [partial]
man/table_regression_models.Rd:97-98
PROMESSE: Sémantique partagée: tout est disponible programmatiquement pour TOUTES les classes — broom::tidy(), glance(), as_structured(), as.data.frame().
TEST ATTENDU: Pour un échantillon de chaque famille du registre, les quatre méthodes retournent sans erreur des objets non vides.
PREUVE: tidy()/glance()/as.data.frame()/as_tibble() are tested for lm/glm tables (test-regression_broom.R lines 11-197) and as_structured() for lm and several classes ad hoc (test-regression_structured.R, test-structured-parity.R, test-fixest_fe_block.R line 189, test-component_blocks.R line 229, test-stan_bayes_gates_re.R line 767). But no systematic per-family sweep asserts all four methods return non-empty objects for every registry class; test-table_regression_models.R only checks registry-vs-dispatch and doc-table consistency.

### rd-vcov-classes:vcov-scalar-recycled-list-mixed [partial]
man/table_regression.Rd:75-76,954-957
PROMESSE: Un vcov scalaire est recyclé à tous les modèles; une liste (une chaîne par modèle) permet des estimateurs mixtes; idem pour cluster.
TEST ATTENDU: table_regression(list(m1,m2), vcov="HC3") applique HC3 aux deux; vcov=list("classical","HC3") donne des SE différentes par colonne-modèle.
PREUVE: test-stat_header_and_guards.R 'vcov validation still accepts strings and string lists' (list(classical,HC3) rend) + test-cov-regression_validate.R / test-regression_validate_branches.R (erreurs de forme); aucun test n'affirme le recyclage d'un scalaire sur tous les modèles ni des SE distinctes par colonne sous liste mixte

### rd-vcov-classes:vcov-tokens-accepted [partial]
man/table_regression.Rd:73-75
PROMESSE: L'argument vcov accepte exactement les tokens "classical", "HC0"-"HC5", "CR0"-"CR3", "bootstrap", "jackknife" (hors famille rq).
TEST ATTENDU: Sur un lm, chaque token de la liste passe sans erreur (avec cluster pour CR*); un token hors liste (ex. "HC9") échoue avec spicy_invalid_input/spicy_unsupported_vcov.
PREUVE: test-table_regression.R 'vcov – unknown type errors spicy_invalid_input' (HC99 refusé classé) + test-cov100-vcov_glm.R '.robust_vcov_support grants glm.nb (negbin) the full estimator set' (identité exacte des 13 tokens) + test-table_continuous_lm.R boucle HC0-HC5 == vcovHC; mais aucun test ne boucle les 13 tokens sur lm/glm via l'API publique

### rd-vcov-classes:wald-test-regimes [partial]
R/vcov.R:661-762
PROMESSE: Le test global multi-coefficients (prédicteurs catégoriels k>2) suit l'estimateur: resampling -> chi2 (df=q); CR* -> clubSandwich::Wald_test HTZ (F, df Satterthwaite); classical/HC* -> Wald F avec df.residual.
TEST ATTENDU: p_global d'un facteur à 3 niveaux: sous CR2 == Wald_test(test="HTZ")$p_val; sous HC3 == pf(stat, q, df.residual); sous bootstrap == pchisq.
PREUVE: test-table_continuous_lm.R 'CR2 matches clubSandwich::Wald_test() for k>2 categorical' (HTZ) + 'bootstrap on 3-level categorical produces chi2 header' + 'compute_wald_test falls back when clubSandwich Wald_test errors'; le régime HC* -> F(df.residual) n'est pas testé directement, et tout passe par table_continuous_lm (fonction partagée), pas par le p_global de table_regression

### rd-vcov-classes:weights-auto-into-vcov [partial]
man/table_regression.Rd:1268-1273; R/vcov.R:214-216,365-367
PROMESSE: Pas d'argument weights: les poids du fit (stats::weights) sont extraits automatiquement dans tous les calculs vcov (y c. les refits bootstrap/jackknife).
TEST ATTENDU: lm pondéré + bootstrap: les réplicats sont refittés en lm.wfit avec les poids rééchantillonnés; les SE diffèrent du même modèle non pondéré.
PREUVE: test-env_leak_refits.R 'glm bootstrap threads family, weights, and offset through glm.fit' et test-table_continuous_lm.R 'compute_resample_vcov_bootstrap reproducibility and weighted refit' passent weights EXPLICITEMENT à compute_model_vcov; l'extraction automatique stats::weights(fit) via table_regression sans argument n'est pas testée

### vignettes-news:ame-classes-populated [partial]
NEWS.md:280-283,580-583
PROMESSE: AME columns are available and populated for betareg, mgcv::gam, svyglm, survreg, fixest, estimatr, quantreg::rq, AER::ivreg, and rms fits (and per outcome category for polr/clm/multinom); classes with no AME backend are refused with a pointer to ?table_regression_models; AME SEs/CIs/p honour a robust vcov.
TEST ATTENDU: Each listed class renders non-empty AME cells matching avg_slopes; a no-backend class errors with the pointer.
PREUVE: Oracle-pinned AME for betareg, gam, svyglm, survreg (test-ame-by-class.R:84-117), polr/clm/multinom per category (:157-203), rq (test-rq_vcov_family.R 'rq AME rows share the coefficient rows' vcov', :277), geeglm (:547); no-backend refusal with registry pointer (test-stat_header_and_guards.R:77, flexsurvreg); robust-vcov-honouring AME (test-ame-by-class.R:254, test-robust-vcov-multinom.R:183). Not evidenced: populated AME tests for fixest, estimatr, AER::ivreg, and rms fits.

### vignettes-news:ame-missing-pkg-vs-fallback [partial]
table-regression.Rmd:280-285
PROMESSE: Requesting AME without marginaleffects installed is a hard classed error (spicy_missing_pkg, with install hint); a computation failure inside avg_slopes() warns (spicy_fallback) and dashes the AME cells, leaving the rest of the table intact.
TEST ATTENDU: Mock requireNamespace FALSE: error class spicy_missing_pkg; mock avg_slopes error: spicy_fallback warning + dashed AME column, other columns intact.
PREUVE: Moitié fallback couverte: test-cov-ame.R 'extract_ame_marginaleffects warns and returns empty when avg_slopes fails' (spicy_fallback) + test-regression_glm.R 'AME cell renders en-dash when estimate is NA'. La moitié erreur dure spicy_missing_pkg (marginaleffects absent) est marquée nocov dans R/regression_ame.R:352-361 et n'est testée nulle part (le mock de test-cov-ame.R:211 teste un retour NULL, pas l'erreur classée).

### vignettes-news:bayes-diagnostics-guard [partial]
NEWS.md:165-169; table-regression-bayesian.Rmd:158-173
PROMESSE: Every Bayesian fit is checked before rendering: R-hat >= 1.01, ESS < 100 per chain (floor 400), any divergent transition, or E-BFMI < 0.2 adds a 'Sampler diagnostics:' footer line and fires a classed spicy_bayes_diagnostics warning; a clean fit prints nothing; the guard reads all sampled parameters including group-level ones.
TEST ATTENDU: Deliberately short 2-chain multilevel fit: footer + warning; a converged fit: neither.
PREUVE: The ESS arm is tested end to end: test-stan_bayes_gates_re.R 'diagnostic columns render and the convergence guard fires' (line 255: under-sampled fit → 'Sampler diagnostics:' footer + warning, values match posterior::summarise_draws), and the spicy_bayes_diagnostics class is asserted at lines 594-606. Not tested: the R-hat >= 1.01, divergent-transition, and E-BFMI < 0.2 arms; that a clean fit prints nothing; and that the guard reads group-level parameters.

### vignettes-news:bayes-tidy-na-p [partial]
table-regression-bayesian.Rmd:502-513
PROMESSE: tidy() on a Bayesian table returns estimate = posterior median, std.error = MAD SD, conf.low/high = credible bounds; p.value, statistic, and df are NA.
TEST ATTENDU: tidy columns match the draws summaries; p.value all NA.
PREUVE: Frame level: test-regression_frame_stan.R 'coefs$p_value is NA_real_ for every row' (l.138) and 'estimate matches posterior median; std_error matches posterior MAD SD' (l.162); broom::tidy round-trips draws medians/CrI for an RE row (test-stan_bayes_gates_re.R l.77-89). But no test asserts tidy() output columns p.value/statistic/df are all NA on a Bayesian table.

### vignettes-news:brms-parity [partial]
table-regression-bayesian.Rmd:475-491
PROMESSE: brm() fits reach the same table through the same code path and match the rstanarm rendering row for row (RE block built from sd_*/cor_* draws, identical refusals).
TEST ATTENDU: Shared-frame tests: brms vs rstanarm equivalents produce matching structures.
PREUVE: Shared machinery and refusal parity are tested: test-regression_frame_stan.R (brmsfit schema, median/MAD SD line 162, parameters:: oracle line 292), test-stan_bayes_gates_re.R 'brms family spellings map to the shared exp machinery' (line 615) and 'brmsfit algebraic betas: engine-invariant, oracle-exact' (line 1292, same scale factor as rstanarm). But no test builds a multilevel brm and checks the RE block from sd_*/cor_* draws, nor a row-for-row structural comparison of a brms vs rstanarm rendered table (the RE-block oracle test uses stan_glmer only, line 61).

### vignettes-news:cox-ame-refused [partial]
table-regression-survival.Rmd:152-162; supported-models.Rmd:69-80
PROMESSE: AME is refused for coxph with an explanation (no committed response scale); the estimand columns rmst/risk_diff are the alternative; rms::cph and flexsurvreg support neither AME nor the estimand columns.
TEST ATTENDU: show_columns="ame" on coxph errors with the explanation; on cph/flexsurvreg both ame and rmst error.
PREUVE: coxph AME refusal with explanation covered: test-cox-exponentiate-ame.R 'AME tokens are rejected for Cox models with a clear error' + 'Cox frames advertise supports$ame = FALSE'; test-stat_header_and_guards.R :77 pins the 'not defined for Cox models' message and the flexsurvreg universal no-backend refusal pointing at ?table_regression_models; rmst/risk_diff alternative extensively tested (test-survival_estimands*.R). Not tested: AME refusal on rms::cph specifically, nor rmst/risk_diff refusal on cph/flexsurvreg (the estimand class gate is tested only with lm, test-survival_estimands.R:204).

### vignettes-news:effect-size-broadcast-factors [partial]
table-regression.Rmd:223-226
PROMESSE: For a k-level factor, the joint (k−1)-df effect size is broadcast across all non-reference dummy rows, and the reference row leaves effect-size cells blank.
TEST ATTENDU: 3-level factor: both non-reference rows show identical η² cells; reference row blank.
PREUVE: test-cov-partial.R: 'as_regression_frame(lm, partial_f2) emits one value per term' vérifie la valeur jointe partagée par les k-1 dummies, mais la cellule vide de la ligne de référence pour les effect sizes n'est pas assertée.

### vignettes-news:effect-size-tokens [partial]
table-regression.Rmd:176-184
PROMESSE: Per-coefficient effect-size tokens partial_f2, partial_eta2, partial_omega2 each have a <token>_ci companion with noncentral-F CIs; shortcuts "all_f2", "all_eta2", "all_omega2" expand to point estimate + CI.
TEST ATTENDU: show_columns = "all_eta2" yields both η² and its CI columns; values match car::Anova-based computation.
PREUVE: test-table_regression.R: 'partial_eta2 + partial_eta2_ci render as atomic columns' et test-regression_render.R: 'partial_eta2 + partial_eta2_ci as separate cells' couvrent tokens+CI; l'expansion des raccourcis est testée pour all_b/all_ame (test-cov-regression_validate.R 'expand_show_columns – mixed group/atomic') mais jamais pour all_f2/all_eta2/all_omega2, et aucun oracle valeur (car::Anova/effectsize) pour eta2 côté table_regression.

### vignettes-news:eta2-omega2-shared-steiger-ci [partial]
table-regression.Rmd:213-222
PROMESSE: Partial η² and partial ω² share a single Steiger noncentral-F interval (MBESS convention; deliberately differs from effectsize's ω² bounds); the interval brackets the bias-corrected point estimate even when the lower bound clips at zero.
TEST ATTENDU: eta2_ci and omega2_ci cells are identical; lower bound 0 allowed while containing the ω² point.
PREUVE: test-cov-lm_compute.R: 'model-level omega2/f2 CI bounds satisfy the Steiger defining equations' + "partial omega2 CI matches effectsize's partial omega2 CI" testent l'inversion Steiger des helpers partagés, mais l'identité cellule à cellule eta2_ci == omega2_ci dans une table table_regression n'est jamais assertée.

### vignettes-news:fixest-keep-own-vcov [partial]
supported-models.Rmd:122-124
PROMESSE: fixest fits keep the variance estimator they were computed with — the footer carries fixest's own label (IID, clustered, Newey-West, ...) — and spicy's HC*/CR* tokens are refused for them.
TEST ATTENDU: feols(cluster=~f) footer names the clustered estimator; vcov="HC3" on fixest errors.
PREUVE: The fit's own estimator label is tested: test-regression_frame_fixest.R 'feols default: vcov_label normalises 'IID' to 'Classical'' (line 175) and 'feols clustered: vcov_label includes 'Clustered'' (line 182). But no test asserts that spicy's HC*/CR* vcov tokens are refused for fixest fits (no fixest + vcov='HC3' error test found in tests/testthat), nor a Newey-West label pass-through.

### vignettes-news:ordinal-ame-matrix [partial]
table-regression-ordinal.Rmd:167-226
PROMESSE: Ordinal AME renders as a per-category matrix (one AME column per response category) whose rows sum to ≈0; cells are on the probability scale and the footer note states it; the matrix appears only when the response has more than two categories (single-outcome models keep one AME column); show_columns = "ame" drops the coefficient column.
TEST ATTENDU: polr AME row sums ≈ 0 (rounding); values match avg_slopes per group; footer states percentage-point reading.
PREUVE: Per-category values/SE/CI/p pinned to avg_slopes for polr and clm (test-ame-by-class.R:157/:169, xval_percat matches every (term, category) pair to 1e-10); AME-only column behavior and ref-group-last plus row-sum ≈ 0 tested for multinom (test-multinom_columns.R:142-167). Not tested for polr/clm: row sums ≈ 0, the probability-scale footer note, and the >2-categories gating of the matrix layout.

### vignettes-news:ordinal-cluster-consistent [partial]
NEWS.md:275-279; table-regression-ordinal.Rmd:229-250
PROMESSE: Ordinal fits honour CR0–CR3 via sandwich::vcovCL, applied consistently: the Thresholds rows and the AME columns take SEs/z/p/CIs from the same vcovCL matrix as the slopes; HC* and bootstrap/jackknife are refused with spicy_unsupported_vcov.
TEST ATTENDU: polr + CR2: threshold SEs equal the sandwich values (not model-based); HC3 request errors with the class.
PREUVE: CR* consistency on thresholds is directly tested: test-robust-vcov-glm-classes.R 'polr CR* matches sandwich::vcovCL on the slope block' (line 56, threshold SEs from the SAME vcovCL matrix, differ from classical) and 'clm CR* matches sandwich::vcovCL (thresholds before slopes)' (line 88). AME-from-same-matrix is only tested via the shared machinery on betareg (test-ame-by-class.R line 254), not an ordinal fit, and no test asserts HC*/bootstrap are refused with spicy_unsupported_vcov specifically for polr/clm (generic non-lm/glm bootstrap refusal exists only for coxph/ols, test-cov100-vcov_glm.R line 20).

### vignettes-news:ordinal-exponentiate-links [partial]
table-regression-ordinal.Rmd:133-142,163-165
PROMESSE: Ordinal exponentiate gives cumulative ORs under logit, HR under cloglog (header and footer relabel), and is refused with a clear error under probit or cauchit; Thresholds rows stay on the log-odds scale (never exponentiated) and the footer flags this.
TEST ATTENDU: polr(method="cloglog") header HR; method="probit" + exponentiate errors; threshold cells unchanged under exponentiate with footer flag.
PREUVE: Covered: polr logit -> OR (test-exponentiate-classes.R l.21), thresholds NOT exponentiated under exponentiate=TRUE (test-ordinal-thresholds-rows.R l.91), clm loglog refused (test-exponentiate_gate.R l.57). Not tested on ordinal fits: cloglog -> HR header (only glm cloglog, test-exponentiate_gate.R l.105), probit/cauchit refusal for polr/clm specifically (only glm cauchit l.11 and glmer probit l.76), and the footer flag for log-scale thresholds under exponentiation.

### vignettes-news:rich-outputs-word-ppt-pdf [partial]
NEWS.md:466-469
PROMESSE: gt and flextable outputs render in Quarto/R Markdown Word, PowerPoint, and PDF documents; an as_flextable() method returns the underlying flextable.
TEST ATTENDU: Render a test Rmd to docx: the table appears; as_flextable(out) returns a flextable object.
PREUVE: test-quarto_word_rendering.R: 'end-to-end: rmarkdown -> Word document contains the table' + 'docx target: spicy_flextable delegates to native openxml' + 'as_flextable() returns the clean engine object, note intact' couvrent Word et as_flextable; aucun test PowerPoint (pptx) ni PDF.

### vignettes-news:standardized-caveat-interactions [partial]
table-regression.Rmd:165-173
PROMESSE: With interactions or transformed terms (I(), poly(), log(), ns()), standardization emits a classed spicy_caveat warning and prints a method-specific caveat line in the footer, without blocking the table.
TEST ATTENDU: lm with x1*x2 + standardized = "refit": warning of class spicy_caveat fires and the footer carries the caveat line.
PREUVE: test-standardize_interactions.R: 'non-fallback refit keeps the refit footer wording' + 'fallback-aware footer: refit failure names the posthoc convention' testent la ligne caveat du footer, mais le warning classé spicy_caveat sur interactions est seulement étouffé (muffleWarning), jamais asserté par expect_warning(class='spicy_caveat').

### vignettes-news:titles-polish [partial]
NEWS.md:598-601
PROMESSE: Titles are link-aware for binomial mixed/survey fits (probit not titled Logistic), Tobit titles name the response, ordinal titles name the shared-slopes assumption by link, and proper nouns keep capitals in multi-model titles.
TEST ATTENDU: Probit glmer title says Probit; tobit title includes the response; multi-model title preserves capitals.
PREUVE: Mixed probit link-aware title: test-cov100-mixed.R 'glmmTMB title prefix covers binomial links' ('Probit mixed-effects regression'); Bayesian probit (test-stan_bayes_gates_re.R:745); Tobit names the response: test-regression_frame_quantreg_AER.R:205-215 ('Tobit regression: affairs'); ordinal shared-slopes by link: test-cov-regression_frame_ordinal.R:40-49 ('Cumulative probit regression (parallel slopes)'); proper nouns: test-nested_lrt_cox.R 'hierarchical/multi-model titles keep proper nouns capitalized'. Not tested: probit titles for glmer (lme4 path) and survey (svyglm titles tested only for gaussian/logit/poisson).

### vignettes-news:two-part-combined-ame [partial]
table-regression-counts.Rmd:283-298
PROMESSE: The AME of a two-part model is the effect on the overall expected count E(Y), combining count and zero processes (avg_slopes on the full model); zero-component rows carry no AME of their own.
TEST ATTENDU: zeroinfl AME matches avg_slopes(fit); zero rows dashed in the AME column.
PREUVE: The combined-E(Y) claim is tested: test-component_blocks.R 'pscl AME column is populated and matches avg_slopes (response)' (line 181, zeroinfl AME equals marginaleffects::avg_slopes on the full model). The second clause — zero-component rows carry no AME (dashed) — has no assertion (no en-dash/AME-blank check on zero rows in test-component_blocks.R).

### vignettes-news:uv-default-linear-screen [partial]
NEWS.md:5-15
PROMESSE: table_regression_uv() defaults to method = "lm"; supplying any family selects method = "glm" automatically; a binary-looking outcome under the lm default proceeds as a linear probability model with a classed warning pointing to vcov = "HC3" and method = "glm".
TEST ATTENDU: Call table_regression_uv(df, outcome = binary_y, predictors = ...) with no family: expect an lm screen and a classed warning mentioning HC3 and method = "glm"; with family = binomial() expect a glm screen and no such warning.
PREUVE: test-regression_uv.R: 'default method is the linear screen on a continuous outcome', 'binary outcome under the default warns (LPM) and fits lm' (classe spicy_model_choice), 'family without method selects the glm screen'. Le comportement est testé mais le CONTENU de l'avertissement (pointeur vers vcov='HC3' et method='glm') n'est jamais asserté.

### vignettes-news:vignettes-exist [partial]
NEWS.md:311-317
PROMESSE: Seven new vignettes ship: mixed, GEE, multinomial, counts, survival, ordinal, plus categorical-predictors (and the supported-models map), all listed in the pkgdown navbar/articles index.
TEST ATTENDU: Files exist under vignettes/, build without error, and appear in _pkgdown.yml.
PREUVE: tests/testthat/test-vignette-index.R enforces bidirectional consistency between vignettes/*.Rmd and the pkgdown navbar (l.102), articles index (l.145), and the Get-started 'Learn more' map (l.51) — but it does not pin the seven named vignettes' existence, and building without error is left to R CMD check, not testthat.

