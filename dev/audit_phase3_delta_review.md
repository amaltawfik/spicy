# Revue du delta Phase 3 (2026-08-03) — confirmees

## D1. [major/type2] R/glm_compute.R
Type-II glm LRT: y=FALSE + cbind() response double-counts binomial totals
CLAIM: In compute_glm_type2_lrt (new in 776978a9), when the fit was created with glm(..., y = FALSE) and a matrix response cbind(successes, failures), the fallback branch sets y to the two-column matrix from model.response(model.frame(fit)) while wt is fit$prior.weights, which for a matrix-response binomial already equals the row totals. glm.fit's binomial initialize then multiplies the totals in again (effective weights = tot^2), so every partial chi2 
EVIDENCE: Repro (R 4.6.1, devtools::load_all): d with tot in 3:8, succ ~ binom; fit_no <- glm(cbind(succ, tot-succ) ~ A + B, family=binomial, y=FALSE). table_regression(fit_no, show_columns="partial_chi2") renders chi2(A) = 2052.48 (2) and chi2(B) = 1754.00 (1); the identical fit with y stored renders 72.54 (2) and 24.17 (1). Internally spicy:::compute_partial_chi2_for_term gives p = 3.8e-62 vs the correct 3.5e-10 on the interaction variant. Note: stats::d

## D2. [major/conformites] R/regression_ame.R
Weighted-AME fix silently inert under na.action = na.exclude
CLAIM: The T-fix lot (cd3c2bf6) makes AME extraction honour the fit's prior weights via .spicy_ame_fit_wts() (R/regression_ame.R:373), but stats::weights() applies napredict/naresid padding: under na.action = na.exclude the returned vector contains NA, the !all(is.finite(w)) guard returns NULL, and the AME reverts to the unweighted average with no warning. The same weighted model produces different AME values under na.omit vs na.exclude, and the Rd Weig
EVIDENCE: Probe: glm(am ~ wt + hp, weights = w, na.action = na.exclude) on mtcars with one NA in wt: weights(fit) has length 32 for 31 estimation rows with anyNA TRUE; spicy:::.spicy_ame_fit_wts(fit) returns NULL; table_regression(show_columns = c('b','ame')) gives AME(wt) = -0.3781 (unweighted), while the identical fit with na.action = na.omit gives the documented weighted AME(wt) = -0.4363. Also hits the closed-form path: .ame_contrast_row_average()'s le

## D3. [major/conformites] R/regression_ame.R
Bayesian draws-native AME path never passes the prior weights
CLAIM: The lot's commit says AME honours prior weights 'on every path', and .spicy_ame_fit_wts() even returns the weights for stanreg fits, but .compute_bayes_ame_table() calls marginaleffects::avg_slopes(fit, conf_level = ci_level) (R/regression_ame.R:1008) without wts, so weighted rstanarm fits get the equal-weights AME average while every frequentist path in the same table would weight it — an inconsistency introduced by the fix itself and a live vio
EVIDENCE: Probe: rstanarm::stan_glm(am ~ wt + hp, family = binomial(), weights = w, seed = 11) on mtcars: spicy:::.spicy_ame_fit_wts(fit) returns the 32 weights, yet the table AME(wt) = -0.41765 equals avg_slopes(fit) with equal weights, while avg_slopes(fit, wts = w) — the convention the lot installed for glm — gives -0.43985 (AME(hp): 0.0019468 vs 0.0020458).

## D4. [minor/conformites] R/regression_ame.R
Frequency-weighted polr: B rows weighted, per-category AME averaged unweighted
CLAIM: For MASS::polr fits with case/frequency weights, stats::weights() returns nothing (length 0), so .spicy_ame_fit_wts() yields NULL and the per-category AME averages with equal weights while the coefficients and SEs of the same table are fully weight-aware — another path where the lot's 'AME honours the fit's prior weights on every path' claim does not hold, silently.
EVIDENCE: Probe: MASS::polr(Sat ~ Infl + Cont, weights = Freq, data = MASS::housing): spicy:::.spicy_ame_fit_wts(fit) has length 0; the table's per-category AME (e.g. InflHigh/Low = -0.268346) matches avg_slopes(fit) unweighted, not avg_slopes(fit, wts = housing$Freq) = -0.266416. The weights exist in the fit (model frame '(weights)') but the helper only tries stats::weights().

## D5. [major/conformites] R/regression_ame.R
Character predictor + AME: de-aligned stray row and lost per-level AME
CLAIM: The layout-scope conformity (rd-core:factor-layout-scope, 'factor, ordered, character, or logical') was fixed for logical only: the is_factor_var predicate at R/regression_ame.R:492/685/927 tests is.factor(mf[[col]]) || is.logical(mf[[col]]) but not is.character, and model.frame keeps character columns as character. AME rows for a character predictor therefore keep the bare term for every contrast, de-align from the grouped B rows, collapse to a 
EVIDENCE: Probe: d$gear_ch <- as.character(d$gear); table_regression(lm(mpg ~ wt + gear_ch, d), show_columns = c('b','ame')) renders the grouped block 'gear_ch:' with levels 4 and 5 having EMPTY AME cells, plus an extra flat row 'gear_ch' below the block showing AME 2.16 (the '4 - 3' contrast); the '5 - 3' AME is absent from the table. The same model with factor(gear) aligns both AMEs onto their level rows.

## D6. [minor/conformites] R/regression_dispatch.R
Excel engine omitted from the per-cell en-dash conformity
CLAIM: The T-fix commit states class-inappropriate fit statistics 'render the documented per-cell en-dash in every engine', and console/data.frame/word (flextable char body) now do, but output_excel() writes the structured numeric body with na.strings = '' and its en-dash overlay (na_dash, R/regression_dispatch.R:2084-2151) is applied only to reference rows — a class-inappropriate fit-stat cell stays blank in the workbook, contradicting the fix's own co
EVIDENCE: Probe: table_regression(list(lm(mpg~wt), glm(am~wt, binomial)), show_fit_stats = c('nobs','r2'), output = 'excel'); wb_to_df readback shows the R² row with 0.7528 under the lm model and an empty (NA) cell under the glm model, while print()/output='data.frame' of the same table render '–' there (pinned by the new test 'mixed lm + glm alien fit-stat cells render an en-dash').

## D7. [minor/conformites] R/regression_uv.R
uv intercept term keys collide with keep/drop predictor regexes
CLAIM: T-fix-2 keys each univariable intercept as '<pred>: (Intercept)' (R/regression_uv.R:767). apply_keep_drop_filter() matches regexes against the term column with no is_intercept exemption, so any keep/drop pattern containing a predictor name now also matches that predictor's uv intercept: with show_intercept = TRUE and keep = 'wt', the univariable wt block still shows its intercept while the multivariable '(Intercept)' row is filtered out — an asym
EVIDENCE: Probe: table_regression_uv(mtcars-based d, outcome = mpg, predictors = c(wt, cyl), show_intercept = TRUE, keep = 'wt') renders '(Intercept) | 37.29 ...' under Univariable but no intercept row under Multivariable ('wt: (Intercept)' matches the regex 'wt'; '(Intercept)' does not).

## D8. [minor/textes] vignettes/as-structured.Rmd
Custom-renderer example ignores the per-row precision overrides its own prose showcases
CLAIM: The 'Building your own renderer' section claims the render_kable example works 'using the display precision from col_meta' and that 'the structured view carries everything a renderer needs', but the example reads only col_meta[[nm]]$precision and ignores col_meta's fit_stat_overrides (which record precision 0 for the nobs row, shown two chunks earlier in the s$col_meta$B output). The built vignette therefore visibly renders the fit-stat rows at t
EVIDENCE: Knitted the vignette with knitr::knit (devtools::load_all, R 4.6.1): the kable output line reads '|n | 1175.00| | ...' and '|R² | 0.02|' (scratchpad/as-structured.md line 344), while the col_meta chunk earlier in the same vignette prints fit_stat_overrides[[1]] = list(fit_stat = 'nobs', precision = 0, row = 9). render_kable() in the Rmd (lines 159-172) uses only s$col_meta[[nm]]$precision and s$format_spec$digits.

## D9. [minor/textes] vignettes/table-regression.Rmd
Contradictory Long & Freese section citations for the same partial_chi2 statistic
CLAIM: Both delta-rewritten descriptions of the partial_chi2 token cite Long & Freese 2014 for the likelihood-ratio chi-square but give incompatible section numbers: the Rd bullet (R/table_regression.R lines 44-46, man/table_regression.Rd line ~780) says 'Long & Freese 2014 Section 3.5', while the vignette paragraph (vignettes/table-regression.Rmd line 713) says 'Long & Freese 2014 §3.2.2, §3.2.4' — and a third passage on nested LRTs (R/table_regression
EVIDENCE: git diff 8a96739..HEAD shows both passages rewritten in this delta with the divergent citations retained: the Rd bullet now reads '(Long & Freese 2014 Section 3.5)' and the vignette now reads 'on the LRT itself see Long & Freese 2014 §3.2.2, §3.2.4'. The reference PDFs (dev/ methods library) are not present in the worktree (dev/_source/ is empty), so the correct section could not be pinned from source, but the two rewritten texts cannot both be r

## Refutees
- uv intercept rows carry factor_level '(Intercept)' in the long/tidy schema | Reproduced exactly: uv intercept rows (new in lot T-fix, cd3c2bf6) carry factor_level "(Intercept)" with factor_term NA in output="long" and tidy(), while the multivariable intercept in the same frame
- New as-structured vignette is announced nowhere in NEWS | Confirmed on all points. Commit 1125fa6b adds vignettes/as-structured.Rmd (205 lines) wired into vignettes/spicy.Rmd line 222 and _pkgdown.yml lines 71/108, but touches no NEWS.md; grep of NEWS.md fin
- NEWS overstates uv family auto-selection: gaussian() errors instead of selecting glm | The facts reproduce but the finding is not a defect of the delta. (1) The NEWS statement is mechanically accurate: routing at R/regression_uv.R:168-170 keys purely on whether family was supplied, so a
