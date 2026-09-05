# R-core : drop1() sur glm(y = FALSE) avec réponse cbind()

- **Cible** : R 4.6.1 et R-devel r90350
- **Canal** : R Bugzilla #19128, https://bugs.r-project.org/show_bug.cgi?id=19128
- **Envoyé** : 2026-08-06
- **Statut** : déposé, à suivre
- **Côté spicy** : aucun contournement, signalement seul

---

*Dossier tel qu'envoyé, conservé verbatim ci-dessous.*

# Signalement R-core : drop1() sur glm(y = FALSE) + réponse cbind()

**DEPOSE le 2026-08-06 — R Bugzilla #19128**
https://bugs.r-project.org/show_bug.cgi?id=19128
(Product R / Component Models / Version R-devel trunk / Severity normal.)
Texte soumis : dev/r_core_drop1_TO_PASTE.md.

Canal : bugs.r-project.org (compte requis — demande à R-core si besoin)
ou, à défaut, la liste R-devel.

Deux relectures mot à mot le 2026-08-05 : chaque nombre, numéro de
ligne et citation ci-dessous est adossé à une exécution capturée DE
MES MAINS ou au fichier add.R de r-devel téléchargé le jour même.
Fautes attrapées et corrigées en chemin : « thirty lines above »
(faux, ~250 lignes) ; une citation qui sautait la ligne 520 sans le
dire ; « deviance-derived » (inexact pour Rao, statistique de
score) ; « also present in current r-devel » en tête de repro (lu
dans la source, pas exécuté) ; un cas de la matrice de tests vacueux
(mauvais nom de colonne Rao → all.equal(NULL, NULL) == TRUE) ; des
chiffres Rao/F issus d'un jeu au flux RNG décalé, remplacés par ceux
du jeu exact du repro (Rao 1.0065 → 2.6039 ; F 0.3991 → 121.71) ;
et à la relecture finale : trois affirmations qui ne reposaient que
sur des captures d'agents, ré-épinglées moi-même (Note ?glm
verbatim ; déviance du refit interne = glm(prop, w = tot²) =
377.3918815 à 1e-12 ; stepAIC ~B vs ~A+B), plus quatre retouches de
langue (« and the p-values », idiome « multiplies the weights by
the totals a second time », « holds », guillemets Rd restitués dans
la citation de ?glm).

---

**Summary:** drop1() on a binomial glm fitted with a two-column
matrix response (cbind(successes, failures)) and y = FALSE inflates
every column computed from the per-term refits: AIC, LRT, F, Rao
score, and the p-values. The same call with the default y = TRUE
is correct. add1.glm() already performs the required response
conversion when it rebuilds the model frame; drop1.glm() does not.

**Reproduction (R 4.6.1 and R-devel r90350):**

```r
set.seed(42)
n <- 60
d <- data.frame(
  A   = factor(rep(c("a", "b", "c"), each = 20)),
  B   = factor(rep(c("u", "v"), 30)),
  tot = sample(3:8, n, TRUE)
)
d$succ <- rbinom(n, d$tot, plogis(0.3 + 0.5*(d$A=="b") - 0.4*(d$B=="v")))

fit_y  <- glm(cbind(succ, tot - succ) ~ A + B, binomial, d)
fit_no <- glm(cbind(succ, tot - succ) ~ A + B, binomial, d, y = FALSE)
identical(deviance(fit_y), deviance(fit_no))  # TRUE -- same fit

drop1(fit_y,  test = "LRT")$LRT[2]   # 1.006   (correct)
drop1(fit_no, test = "LRT")$LRT[2]   # 306.81  (inflated)
drop1(fit_y,  test = "LRT")$AIC      # 188.74 185.75 189.92
drop1(fit_no, test = "LRT")$AIC      # 188.74 491.55 508.04
```

test = "Rao" is affected as well (Rao score for A on these data:
1.0065 with y stored, 2.6039 with y = FALSE), as is test = "F" on
the quasibinomial analogue (F value for A: 0.3991 vs 121.71) --
all the reported columns flow through the same distorted refits.
The practical stakes go beyond drop1() itself: on these same data,
step() (and MASS::stepAIC) select ~ B with the default fit but
~ A + B with y = FALSE -- model selection is silently inverted,
with no warning anywhere.

**Mechanism** (src/library/stats/R/add.R; line numbers from current
r-devel, where the code is identical to 4.6.1). drop1.glm()
reconstructs the response and takes the weights at lines 515-519
and 521:

```r
    y <- object$y
    if(is.null(y)) {
        y <- model.response(model.frame(object))
        if(!is.factor(y)) storage.mode(y) <- "double"
    }
```

and, after one commented-out line, at line 521:

```r
    wt <- object$prior.weights %||% rep.int(1, n)
```

With y = FALSE and a matrix response, model.response() returns the
two-column matrix, while object$prior.weights already holds the
post-initialize weights: the original weights times the binomial
totals (rowSums of the matrix). This is the documented invariant --
the Note in ?glm states: "If a 'binomial' 'glm' model was specified
by giving a two-column response, the weights returned by
'prior.weights' are the total numbers of cases (factored by the
supplied case weights) and the component 'y' of the result is the
proportion of successes." The per-term glm.fit() refits then re-run
the binomial initialize on the matrix response, which multiplies
the weights by the totals a second time -- the refits effectively
use weights * totals^2 (I verified this directly: the internal
refit's prior.weights equal totals^2, and its deviance matches a
glm fitted on proportions with weights = totals^2 to the last
digit). The resulting table is even
inconsistent with itself: the `<none>` row comes from the stored fit
on the true scale, while the dropped-term rows come from the
distorted refits, so the differences are between incommensurable
quantities. With the default y = TRUE, object$y holds the
post-initialize proportions and everything is consistent.

add1.glm(), in the same file, already deals with this: when it
rebuilds the model frame (its is.null(x) branch), it converts the
matrix response to proportions and adjusts the weights (lines
261-268):

```r
        y <- model.response(m)
        if(!is.factor(y)) storage.mode(y) <- "double"
        ## binomial case has adjusted y and weights
        if(NCOL(y) == 2) {
            n <- y[, 1] + y[, 2]
            y <- ifelse(n == 0, 0, y[, 1]/n)
            wt <- (wt %||% rep.int(1, length(y))) * n
        }
```

There, wt comes from model.weights(m) (line 258) -- the
pre-initialize user weights -- hence the multiplication by n. In
drop1.glm() the weights come from object$prior.weights, which is
already post-initialize, so only the response conversion is needed,
not the weight adjustment.

**Proposed patch** (drop1.glm):

```diff
     y <- object$y
     if(is.null(y)) {
         y <- model.response(model.frame(object))
         if(!is.factor(y)) storage.mode(y) <- "double"
+        ## binomial matrix response: prior.weights already holds
+        ## user weights * totals, so only the response needs the
+        ## proportions form (cf. the corresponding case in add1.glm)
+        if(NCOL(y) == 2L) {
+            n0 <- y[, 1L] + y[, 2L]
+            y <- ifelse(n0 == 0, 0, y[, 1L]/n0)
+        }
     }
```

I applied this patch locally (R 4.6.1) and verified. Repaired: with
the patch, the y = FALSE tables above become identical() to their
y = TRUE counterparts for test = "LRT" and test = "Rao" (AIC column
included), also with user-supplied prior weights; the quasibinomial
F results agree to within 1e-12. Unchanged: vector 0/1 responses,
factor responses, poisson and gaussian fits with y = FALSE, and
y = TRUE calls all give identical() drop1() tables with and without
the patch.

I could not find an existing report of this in the bug tracker or
the r-devel archives; my apologies if I missed one.

Versions: reproduced with R --vanilla on R 4.6.1 (2026-06-24 ucrt)
and on R Under development (unstable) (2026-08-04 r90350 ucrt),
both x86_64-w64-mingw32 (Windows 11). The relevant code in
src/library/stats/R/add.R is unchanged in current r-devel (also
checked 2026-08-05 against the r-svn GitHub mirror).

---

## Contre-vérification profonde (2026-08-05, interne)

Sur demande d'Amal (« vérifie en profondeur que c'est bien une
erreur de R »), deux étages :

1. Vérité terrain sans drop1 (scratchpad/drop1_ground_truth.R) :
trois oracles indépendants — différence de déviances de fits
explicites, 2×(logLik plein − réduit), anova(réduit, plein) —
donnent tous 1.005957621 ; drop1 y=TRUE colle à 1e-12, y=FALSE
donne 306.81. Précédent interne : anova.glm séquentielle gère
y=FALSE correctement.

2. Trois avocats du diable (workflow refute-drop1-bug), mandat
inversé « prouvez que ce n'est PAS un bug » : verdict unanime BUG.
Défenses démontées : « hors contrat » (aucun texte ; la branche
is.null(y) de drop1 revendique le support ; anova/summary/
influence.measures corrects sous y=FALSE), « autre paramétrisation
valide » (les coefficients du refit interne ne sont le MLE d'aucun
modèle ; table incohérente avec elle-même), « lecture fausse »
(mécanisme vérifié maillon par maillon, déviance 377.3919 = fit à
poids tot² au chiffre près), « patch risqué » (12 scénarios dont
totaux nuls, poids a priori nuls, na.exclude, offset,
quasibinomial : tous identiques à la référence). Apports intégrés
au brouillon : Note de ?glm, step()/stepAIC inversés, ligne
`<none>` incommensurable. Correction attrapée chez nous : les chiffres
Rao/F initiaux venaient d'un dataset au flux RNG décalé (w=runif
avant rbinom) — recalculés sur le jeu exact du repro (Rao 1.0065
vs 2.6039 ; F quasibinomial 0.3991 vs 121.71 ; step ~B vs ~A+B),
concordants avec les deux agents. Numéros de ligne : deux lectures
indépendantes concordantes (moi + agent sémantique, grep -n sur
téléchargements séparés : 515-521 / 261-268) ; deux agents ont
donné d'autres numéros (505-511, ~626-632) — erreurs de comptage
de leur côté, mais leur conseil tient : le rapport cite le contenu
verbatim, les numéros ne sont qu'indicatifs datés.

## Contexte spicy (interne, ne pas envoyer)

Découvert en corrigeant D1 (revue delta Phase 3) : notre
compute_glm_type2_lrt reconstruisait y comme drop1 le fait et
partageait le défaut ; notre fix reflète le chemin fit$y (commit
lot T-fix-3, fd853899). Repro d'origine : scratchpad
drop1_upstream.R ; matrice initiale : drop1_patch_matrix.R (8/8
dont un cas Rao vacueux) ; batterie corrigée et complète :
drop1_word_checks2.R (colonnes gardées par stopifnot, identical()
stricts, inflations F/Rao directes, prior.weights pondérés w*tot,
fit$y proportions).

---

## Texte soumis à Bugzilla (verbatim)

**Summary:** drop1() on a binomial glm fitted with a two-column matrix response (cbind(successes, failures)) and y = FALSE inflates every column computed from the per-term refits: AIC, LRT, F, Rao score, and the p-values. The same call with the default y = TRUE is correct. add1.glm() already performs the required response conversion when it rebuilds the model frame; drop1.glm() does not.

**Reproduction (R 4.6.1 and R-devel r90350):**

```
set.seed(42)
n <- 60
d <- data.frame(
  A   = factor(rep(c("a", "b", "c"), each = 20)),
  B   = factor(rep(c("u", "v"), 30)),
  tot = sample(3:8, n, TRUE)
)
d$succ <- rbinom(n, d$tot, plogis(0.3 + 0.5*(d$A=="b") - 0.4*(d$B=="v")))

fit_y  <- glm(cbind(succ, tot - succ) ~ A + B, binomial, d)
fit_no <- glm(cbind(succ, tot - succ) ~ A + B, binomial, d, y = FALSE)
identical(deviance(fit_y), deviance(fit_no))  # TRUE -- same fit

drop1(fit_y,  test = "LRT")$LRT[2]   # 1.006   (correct)
drop1(fit_no, test = "LRT")$LRT[2]   # 306.81  (inflated)
drop1(fit_y,  test = "LRT")$AIC      # 188.74 185.75 189.92
drop1(fit_no, test = "LRT")$AIC      # 188.74 491.55 508.04
```

test = "Rao" is affected as well (Rao score for A on these data: 1.0065 with y stored, 2.6039 with y = FALSE), as is test = "F" on the quasibinomial analogue (F value for A: 0.3991 vs 121.71) -- all the reported columns flow through the same distorted refits. The practical stakes go beyond drop1() itself: on these same data, step() (and MASS::stepAIC) select ~ B with the default fit but ~ A + B with y = FALSE -- model selection is silently inverted, with no warning anywhere.

**Mechanism** (src/library/stats/R/add.R; line numbers from current r-devel, where the code is identical to 4.6.1). drop1.glm() reconstructs the response and takes the weights at lines 515-519 and 521:

```
    y <- object$y
    if(is.null(y)) {
        y <- model.response(model.frame(object))
        if(!is.factor(y)) storage.mode(y) <- "double"
    }
```

and, after one commented-out line, at line 521:

```
    wt <- object$prior.weights %||% rep.int(1, n)
```

With y = FALSE and a matrix response, model.response() returns the two-column matrix, while object$prior.weights already holds the post-initialize weights: the original weights times the binomial totals (rowSums of the matrix). This is the documented invariant -- the Note in ?glm states: "If a 'binomial' 'glm' model was specified by giving a two-column response, the weights returned by 'prior.weights' are the total numbers of cases (factored by the supplied case weights) and the component 'y' of the result is the proportion of successes." The per-term glm.fit() refits then re-run the binomial initialize on the matrix response, which multiplies the weights by the totals a second time -- the refits effectively use weights * totals^2 (I verified this directly: the internal refit's prior.weights equal totals^2, and its deviance matches a glm fitted on proportions with weights = totals^2 to the last digit). The resulting table is even inconsistent with itself: the `<none>` row comes from the stored fit on the true scale, while the dropped-term rows come from the distorted refits, so the differences are between incommensurable quantities. With the default y = TRUE, object$y holds the post-initialize proportions and everything is consistent.

add1.glm(), in the same file, already deals with this: when it rebuilds the model frame (its is.null(x) branch), it converts the matrix response to proportions and adjusts the weights (lines 261-268):

```
        y <- model.response(m)
        if(!is.factor(y)) storage.mode(y) <- "double"
        ## binomial case has adjusted y and weights
        if(NCOL(y) == 2) {
            n <- y[, 1] + y[, 2]
            y <- ifelse(n == 0, 0, y[, 1]/n)
            wt <- (wt %||% rep.int(1, length(y))) * n
        }
```

There, wt comes from model.weights(m) (line 258) -- the pre-initialize user weights -- hence the multiplication by n. In drop1.glm() the weights come from object$prior.weights, which is already post-initialize, so only the response conversion is needed, not the weight adjustment.

**Proposed patch** (drop1.glm):

```
     y <- object$y
     if(is.null(y)) {
         y <- model.response(model.frame(object))
         if(!is.factor(y)) storage.mode(y) <- "double"
+        ## binomial matrix response: prior.weights already holds
+        ## user weights * totals, so only the response needs the
+        ## proportions form (cf. the corresponding case in add1.glm)
+        if(NCOL(y) == 2L) {
+            n0 <- y[, 1L] + y[, 2L]
+            y <- ifelse(n0 == 0, 0, y[, 1L]/n0)
+        }
     }
```

I applied this patch locally (R 4.6.1) and verified. Repaired: with the patch, the y = FALSE tables above become identical() to their y = TRUE counterparts for test = "LRT" and test = "Rao" (AIC column included), also with user-supplied prior weights; the quasibinomial F results agree to within 1e-12. Unchanged: vector 0/1 responses, factor responses, poisson and gaussian fits with y = FALSE, and y = TRUE calls all give identical() drop1() tables with and without the patch.

I could not find an existing report of this in the bug tracker or the r-devel archives; my apologies if I missed one.

Versions: reproduced with R --vanilla on R 4.6.1 (2026-06-24 ucrt) and on R Under development (unstable) (2026-08-04 r90350 ucrt), both x86_64-w64-mingw32 (Windows 11). The relevant code in src/library/stats/R/add.R is unchanged in current r-devel (also checked 2026-08-05 against the r-svn GitHub mirror).
