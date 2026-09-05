# survey 4.5 : deff() sur svyby et svychisq() sur plans répliqués

- **Cible** : survey 4.5 (sources R-Forge r405)
- **Canal** : courriel au mainteneur, faute de tracker public
- **Envoyé** : 2026-08-21 04:20
- **Statut** : envoyé, sans réponse connue au 2026-09-06
- **Côté spicy** : les tables svy rapportent DEff et les statistiques de svychisq() (R/i18n.R, table_continuous_svy et son jumeau catégoriel) ; vérifier si un contournement de deff() sur svyby est en place et le référencer ici

---

*Dossier tel qu'envoyé, conservé verbatim ci-dessous.*

Subject: survey 4.5: two bugs with tested patches (deff.svyby; svychisq on replicate designs)

Dear Professor Lumley,

While validating an R package of mine against survey, I ran into two bugs in survey 4.5 that I believe are worth your attention. Both are present in the R-Forge sources at r405. The complete write-ups -- scope tables, suggested patches (applied and tested against the package's own tests/, with identical results on patched and unpatched builds), and regression checks -- follow below my signature, so everything is in this one message. I could not find a public tracker for survey, hence this email.

1. deff() on an svyby object returns the confidence-limit columns whenever vartype includes "ci" -- the combination used in the example on ?svyby:

     b <- svyby(~api99+api00, ~stype, dclus1, svymean,
                deff=TRUE, vartype="ci")
     deff(b)            # ci_u.api99: 652.51, 677.57, 672.42 ...
     b[, "DEff.api99"]  # stored values are correct: 5.90, 2.21, 2.23

   The stored DEff columns are right; only the extractor is off by one column block ("ci" produces two blocks, the svyby attribute counts one). Related: ftable() drops and mislabels rows on the same objects, and deff="replace" objects cannot be read at all. Details and a patch in full report 1, below.

2. svychisq(statistic="lincom" or "saddlepoint") on replicate-weight designs still calls pchisqsum() with no denominator df, while the linearized and two-phase methods use pFsum(ddf = d0*nu) -- the migration NEWS 3.24-1 records as done. On as.svrepdesign(dclus1) with ~sch.wide + stype:

     lincom       0.0227   (with ddf: 0.0401)
     saddlepoint  0.0237   (with ddf: 0.0415)

   That is about 1.77x too small, and the gap grows as the design df falls (8x at ddf = 5). A second, smaller bug in the same function clobbers $method, so saddlepoint reports "Rao & Scott adjustment". Details, the patch, and an appendix of five smaller verified items in full report 2, below.

With my thanks for the survey package,

Amal Tawfik
Haute Ecole de Sante Vaud (HESAV), HES-SO

======================================================================
FULL WRITE-UPS BELOW
======================================================================

REPORT 1 -- deff() on an svyby object returns the confidence-limit columns when vartype includes "ci"
----------------------------------------------------------------------

survey 4.5, R 4.6.1.

deff() applied to an svyby object silently returns the wrong columns whenever vartype includes "ci". It returns confidence limits instead of design effects -- no error, no warning, just numbers of a completely different magnitude.

This is the combination used in the example on ?svyby.

library(survey)
data(api)
dclus1 <- svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)

## the example from ?svyby
b <- svyby(~api99+api00, ~stype, dclus1, svymean, deff=TRUE, vartype="ci")

deff(b)
#   ci_u.api99 ci_u.api00
# E   652.5114   692.6976
# H   677.5702   693.0897
# M   672.4177   693.3934

## the DEff columns are present and correct, deff() just doesn't select them
b[, c("DEff.api99","DEff.api00")]
#   DEff.api99 DEff.api00
# E   5.895734   6.583674
# H   2.211866   2.228259
# M   2.226990   2.163900

Expected: the DEff.* columns (5.90, 2.21, 2.23 ...). Obtained: the ci_u.* columns (652.51, 677.57, 672.42 ...).

The values stored by svyby are right -- they match a per-group svymean(~api99+api00, subset(dclus1, stype==lv), deff=TRUE) to all printed digits. Only the extractor is wrong.

Scope. The bug fires exactly when "ci" %in% vartype:

vartype                                                                deff() returns
---------------------------------------------------------------------  --------------
"se", "cv", "cvpct", "var", c("se","var"), c("se","cv","cvpct","var")  correct
"ci", c("se","ci"), c("ci","cv"), c("se","ci","var")                   wrong columns

It affects svytotal as well, where the magnitude is starker -- on svyby(~enroll, ~stype, dclus1, svytotal, deff=TRUE, vartype="ci") a true deff of 125.04 is reported as 3347139.2 -- and both design paths (svyby.survey.design2 and the default method, which is the one replicate-weight designs dispatch to).

Two related failures in the same code.

## (a) ftable() mislabels and drops a row
ftable(svyby(~api99+api00, ~stype, dclus1, svymean, deff=TRUE, vartype="ci"))
#                   api99    api00
# stype
# E     svymean  607.7917 648.8681
#       NA       563.0720 605.0385     <- should be ci_l
#       DEff     652.5114 692.6976     <- these are ci_u; the DEff row is gone

## (b) deff="replace" objects cannot be read at all
deff(svyby(~api99+api00, ~stype, dclus1, svymean, deff="replace"))
#> Error: invalid argument type
ftable(svyby(~api99+api00, ~stype, dclus1, svymean, deff="replace"))
#> Error: non-numeric argument to binary operator

deff="replace" on svyby was added deliberately (NEWS 3.28-3, "svyby didn't allow deff="replace""), so these look like oversights rather than unsupported use.

ftable() also mislabels the CI rows when deff is not requested at all (ftable(svyby(..., vartype="ci")) labels one row NA and omits the other).

Cause
-----

svyby stores vars = length(vartype) in the "svyby" attribute, but vartype="ci" occupies two column blocks. svyby's own nstats calculation accounts for this (nstats<-nr/(1+ keep.var*(length(vartype)+ ("ci" %in% vartype)) + hasdeff)) and so does SE.svyby, which rebuilds an expanded vartype vector -- but deff.svyby and ftable.svyby use the raw vars, landing one block short.

Separately, deffs stores the deff argument verbatim, so !aa$deffs and 1 + info$deffs fail when it is the string "replace".

Suggested patch (applied and tested)
------------------------------------

Fixing the two accessors, rather than the stored attribute, also repairs objects already saved to disk.

--- a/R/surveyby.R
+++ b/R/surveyby.R
+## The "svyby" attribute records vars=length(vartype) and deffs=deff (which
+## may be the string "replace").  Neither is what the accessors need:
+## vartype="ci" produces TWO column blocks (ci_l, ci_u), and a character
+## deffs is not usable in a logical context.
+svyby_nvarblocks<-function(info){
+    if (!info$vars) 0 else info$vars + ("ci" %in% info$vartype)
+}
+svyby_hasdeff<-function(info){
+    isTRUE(is.character(info$deffs) || info$deffs)
+}
+
 deff.svyby<-function(object,...){
     aa<-attr(object,"svyby")
-    if (!aa$deffs) stop("object does not have design effect information")
-    object[,max(aa$margins)+aa$nstats*(1+aa$vars)+(1:aa$nstats)]
+    if (!svyby_hasdeff(aa)) stop("object does not have design effect information")
+    object[,max(aa$margins)+aa$nstats*(1+svyby_nvarblocks(aa))+(1:aa$nstats)]
 }

--- a/R/ftable.svystat.R
+++ b/R/ftable.svystat.R
-    senames<-c(se="SE",cv="cv",cvpct="cv%",var="Var")[info$vartype]
-    if (info$vars || info$deffs) {
-        dims <- c(dims, 1 + info$vars + info$deffs)
+    ## expand vartype the same way SE.svyby does: "ci" occupies two columns
+    allvartype <- c("se","ci","ci","cv","cvpct","var")
+    senames <- c("SE","ci_l","ci_u","cv","cv%","Var")[allvartype %in% info$vartype]
+    nse <- if (info$vars) length(senames) else 0
+    ndeff <- if (svyby_hasdeff(info)) 1L else 0L
+    if (nse || ndeff) {
+        dims <- c(dims, 1 + nse + ndeff)
         dimnames <- c(dimnames,
                       list(sub("^statistic\\.(.*)$", "\\1", info$variables)),
                       list(c(info$statistic,
-                             if (info$vars) senames,
-                             if (info$deffs) "DEff")))
+                             if (nse) senames,
+                             if (ndeff) "DEff")))
       }

After the patch, on the same session:

deff(svyby(..., deff=TRUE, vartype="ci"))       -> DEff.api99 5.895734 ... (correct)
deff(svyby(..., deff=TRUE, vartype=c("se","ci")))-> correct
deff(svyby(..., deff="replace"))                 -> 5.721546 6.389161 ... (== per-group
                                                    svymean(deff="replace"), verified)
ftable(svyby(..., deff=TRUE, vartype="ci"))      -> rows svymean / ci_l / ci_u / DEff
ftable(svyby(..., deff="replace"))               -> works
ftable(svyby(..., vartype="ci"))                 -> rows svymean / ci_l / ci_u

Regression checks, all unchanged: vartype="se" (default), c("se","var"), keep.var=FALSE, keep.var=FALSE + vartype="ci", deff absent (still errors with the same message), ftable default, ftable without deff, SE() with c("se","ci"), replicate designs.

R CMD INSTALL + the package's 59 tests/*.R files: same result as the unpatched build -- only DBIcheck.R fails (RSQLite not installed here) and only inf_cal_test.R differs, which calls rbinom() without set.seed() and differs from itself across runs on the unpatched build too.

======================================================================

REPORT 2 -- svychisq(statistic="lincom"/"saddlepoint") ignores the denominator df on replicate-weight designs
----------------------------------------------------------------------

survey 4.5, R 4.6.1.

svychisq.survey.design computes the lincom and saddlepoint p-values with pFsum(..., ddf = d0*nu); svychisq.svyrep.design still calls pchisqsum() with no ddf. Replicate-weight users therefore get the pre-3.24-1 behaviour, which is anti-conservative, and the help page does not mention it.

R/surveychisq.R:

# survey.design method
117:  pearson$p.value<-pFsum(..., method="integration",  ddf=d0*nu)
122:  pearson$p.value<-pFsum(..., method="saddlepoint",  ddf=d0*nu)

# twophase / twophase2 method
254:  pearson$p.value<-pFsum(..., method="integration",  ddf=d0*nu)
259:  pearson$p.value<-pFsum(..., method="saddlepoint",  ddf=d0*nu)

# svyrep.design method
389:  pearson$p.value<-pchisqsum(..., method="integration")     # no ddf
394:  pearson$p.value<-pchisqsum(..., method="saddlepoint")     # no ddf

The replicate method is the only one of the three not migrated. NEWS records the migration as done:

> 3.24-1 CHANGE: svychisq() statistic="lincom" and "saddlepoint" now use the linear combination of F statistics from pFsum().

and ?svychisq states that the degf computation is the *only* replicate-design difference:

> "For designs using replicate weights the code is essentially the same as for designs with sampling structure... The exception is that the degrees of freedom is computed as one less than the rank of the matrix of replicate weights (by degf)."

Reproducible effect. nu is already in scope in the replicate method (R/surveychisq.R:307, nu <- degf(design)), and d0 is computed at :376, so the only change needed is the tail function.

library(survey)
data(api)
dclus1 <- svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
rclus1 <- as.svrepdesign(dclus1)
degf(dclus1); degf(rclus1)   # both 14 -- the documented exception is a no-op here

svychisq(~sch.wide + stype, dclus1, statistic="lincom")$p.value  # 0.02547176
svychisq(~sch.wide + stype, rclus1, statistic="lincom")$p.value  # 0.02271592

With the replicate branch switched to pFsum(..., ddf = d0*nu) (patch below), the same design and data give:

statistic    survey 4.5   patched
-----------  -----------  -----------
lincom       0.022715919  0.040114077
saddlepoint  0.023652565  0.041509820

so the released values are 1.77x (lincom) and 1.75x (saddlepoint) too small. The discrepancy is a pure denominator-df effect and grows as the design df falls -- precisely the regime the Thomas-Rao ddf machinery exists for:

ddf  pchisqsum (no ddf)  pFsum(ddf)  ratio
---  ------------------  ----------  -----
5    0.00773792          0.06409374  8.28
10   0.00773792          0.03168587  4.10
20   0.00773792          0.01811438  2.34
50   0.00773792          0.01142431  1.48
200  0.00773792          0.00859601  1.11

An internal-consistency signal: on the linearized design the p-values order F (0.0217) < lincom (0.0255) < saddlepoint (0.0264). On the replicate design 4.5 gives lincom (0.0227) < saddlepoint (0.0237) < F (0.0358) -- inverted. After the patch the replicate design orders F (0.0358) < lincom (0.0401) < saddlepoint (0.0415), matching the linearized pattern.

tests/ contains no svychisq test (grep -rn "svychisq" tests/ returns nothing), which is consistent with this surviving unnoticed.

Second bug in the same function: the method label is clobbered
------------------------------------------------------------

R/surveychisq.R:410 overwrites pearson$method unconditionally *after* the if/else chain, so the labels set at :392 and :397 are dead. The survey.design method has no such trailing line.

statistic    $method on survey.design                      $method on svyrep.design
-----------  --------------------------------------------  -------------------------------------
lincom       Pearson's X^2: asymptotic exact distribution  Pearson's X^2: Rao & Scott adjustment
saddlepoint  Pearson's X^2: saddlepoint approximation      Pearson's X^2: Rao & Scott adjustment

A user printing svychisq(..., rclus1, statistic="saddlepoint") is told they ran the Rao-Scott second-order test.

Suggested patch (applied and tested)
------------------------------------

--- a/R/surveychisq.R
+++ b/R/surveychisq.R
   if (match.arg(statistic)=="F"){
     pearson$statistic<-pearson$statistic/sum(diag(Delta))
     pearson$p.value<-pf(pearson$statistic, d0, d0*nu, lower.tail=FALSE)
     attr(pearson$statistic,"names")<-"F"
     pearson$parameter<-c(ndf=d0,ddf=d0*nu)
+    pearson$method<-"Pearson's X^2: Rao & Scott adjustment"
   }  else if (match.arg(statistic)=="lincom") {
-    pearson$p.value<-pchisqsum(pearson$statistic, rep(1,ncol(Delta)), eigen(Delta,only.values=TRUE)$values,
-                              lower.tail=FALSE,method="integration")
+    ## use pFsum with the design denominator df, as svychisq.survey.design
+    ## does (NEWS 3.24-1); pchisqsum() ignores nu and is anti-conservative.
+    pearson$p.value<-pFsum(pearson$statistic, rep(1,ncol(Delta)), eigen(Delta,only.values=TRUE)$values,
+                              lower.tail=FALSE,method="integration",ddf=d0*nu)
     pearson$parameter<-NULL
     pearson$method<-"Pearson's X^2: asymptotic exact distribution"
   }else if  (match.arg(statistic)=="saddlepoint") {
-    pearson$p.value<-pchisqsum(pearson$statistic, rep(1,ncol(Delta)), eigen(Delta,only.values=TRUE)$values,
-                              lower.tail=FALSE,method="saddlepoint")
+    pearson$p.value<-pFsum(pearson$statistic, rep(1,ncol(Delta)), eigen(Delta,only.values=TRUE)$values,
+                              lower.tail=FALSE,method="saddlepoint",ddf=d0*nu)
     pearson$parameter<-NULL
     pearson$method<-"Pearson's X^2: saddlepoint approximation"
   }  else {
     pearson$p.value<-pchisq(pearson$statistic/mean(diag(Delta)),
                                df=NCOL(Delta),lower.tail=FALSE)
     pearson$parameter<-c(df=NCOL(Delta))
+    pearson$method<-"Pearson's X^2: Rao & Scott adjustment"
   }

   if (returnNA){ ... }

   pearson$data.name<-deparse(sys.call(-1))
-  pearson$method<-"Pearson's X^2: Rao & Scott adjustment"
   pearson

F, Chisq, Wald and adjWald are unchanged on both design types (0.021747462 / 0.005553263 / 0.126896301 / 0.147057887 linearized; 0.035773399 / 0.011517296 / 0.126896301 / 0.147057887 replicate -- identical before and after). The 59 files in tests/ behave the same as on the unpatched build.

---

======================================================================

APPENDIX -- five smaller, separable items
----------------------------------------------------------------------

Smaller, separable items, best read as an appendix; each is verified but none is as consequential as the above.

1. degf.svyrep.design(tol=) is inert. Documented twice (man/svychisq.Rd:35 usage, :51 "Tolerance for qr in computing the matrix rank") but R/surveyrep.R:2054 hardcodes qr(weights(design,"analysis"), tol=1e-5)$rank-1. With the degf cache cleared: degf(d, tol=0.5) → 14, while qr(W, tol=0.5)$rank-1 → 2; tol=0.999999 → 14 vs 0.

2. statistic="Chisq" returns an incoherent htest. The F branch replaces pearson$statistic (:111); the Chisq branch applies the design-effect adjustment only inside pchisq() (:126-131) and returns the unadjusted statistic. Result: X-squared = 11.941, df = 2, p-value = 0.005553, but pchisq(11.940892081, 2, lower.tail=FALSE) = 0.002553102378; the statistic actually behind the p-value is 10.386739083. statistic="F" is self-consistent. This is a reporting-coherence bug rather than a doc mismatch -- the p-value itself looks right.

3. wls-score is rejected by the two-phase methods. R/surveychisq.R:144 omits it from statistic=c(...) while :9 and :282 include it; ?svychisq documents it without scoping it to particular design classes. On a twophase2 object, statistic="wls-score" errors with 'arg' should be one of "F","Chisq","Wald","adjWald","lincom","saddlepoint". Possibly deliberate; if so, a line in the Rd would spare the surprise.

4. Dangling citation. man/svychisq.Rd:83 cites "Thomas and Rao (1990)"; the \references section (:151) lists only Thomas & Rao (1987). Cosmetic.

5. Found in passing, in a neighbouring function. R/pFsum.R:2 -- if (ddf==Inf) return(pchisqsum(x,df=df,a=a,lower.tail=lower.tail,...)) does not forward method=, which is a named formal and therefore not in .... So pFsum(..., ddf=Inf, method="saddlepoint") silently returns the *satterthwaite* value: all three method= settings give 0.007459330, whereas pchisqsum(method="saddlepoint") gives 0.007737920 and pchisqsum(method="integration") 0.007732790.
