# ADR 2026-09-05 — Intercept exponentié sous l'en-tête de ratio (OR / IRR / RR / MR)

**Statut** : décidé le 2026-11-13 (option 1). Mis en œuvre dans le
commit « Name the exponentiated intercept in the note » de la branche
`lot-regression`.

## Contexte

Avec `exponentiate = TRUE`, `table_regression()` exponentie aussi
l'intercept et l'affiche sous l'en-tête de ratio du modèle, choisi par
`spicy_glm_exp_header()` (`R/glm_compute.R`). Exemple, Gamma à lien log
simulée (n = 180, HC3) :

```
 Variable    │   MR       95% CI       p
 (Intercept) │   2.84  [2.57, 3.14]  <.001
 x           │   1.39  [1.29, 1.50]  <.001
Note. MR = mean ratio.
```

exp(b0) n'est pas un rapport : c'est la valeur de base au profil de
référence — cote de base (logit), taux de base (Poisson log), risque de
base (binomial log), moyenne de base (Gamma log). La ligne est juste,
l'en-tête la désigne mal. C'est la seule quantité de la table qui échappe
au principe « jamais une valeur sous un en-tête qui la nomme mal »,
appliqué partout ailleurs (exp refusé pour le lien inverse de `Gamma()`,
exp par bloc selon le lien dans `.append_component_rows()`).

Pratiques existantes : Stata (`eform`, `or`, `irr`) affiche `_cons`
exponentié et ajoute une note « `_cons` estimates baseline odds » (resp.
baseline incidence rate) ; gtsummary masque l'intercept par défaut ;
broom et modelsummary exponentient sans nommer.

## Options

1. **Afficher et nommer l'estimand dans une note** (précédent Stata). Une
   chaîne par estimand dans le registre i18n (`R/i18n.R`, `R/i18n_fr.R`),
   sélectionnée par la même logique famille × lien que l'en-tête :
   « Intercept: baseline odds / rate / risk / mean at the reference
   profile ». Aucun changement d'API ni de table ; conserve une
   information utile.
2. **Masquer l'intercept quand `exponentiate = TRUE`**, note « intercept
   omitted », argument `show_intercept` pour le récupérer. Perd une
   information juste pour éviter de la nommer.
3. **Statu quo**, aligné sur Stata sans la note.

## Décision

Option 1, décidée par Amal le 2026-11-13. Quand `exponentiate = TRUE`
et qu'un intercept exponentié est affiché (colonne `b`, `se` ou `ci`,
`show_intercept = TRUE`), la note nomme ce qu'il estime, après la phrase
d'exponentiation : « The (Intercept) row estimates the baseline odds. »,
« La ligne (Intercept) estime la cote de base. ». La ligne garde son
libellé « (Intercept) » dans les deux langues (le pont français n'a pas
de mot pour elle), et la note le reprend. La forme « (Intercept):
baseline odds. », essayée d'abord, donnait « Dentist: (Intercept):
baseline odds. » dans une pile de `table_stack()`, qui préfixe chaque
phrase du nom de son panneau.

1. L'estimand se lit sur la famille et le lien du cadre
   (`info$family`), la donnée dont `spicy_glm_exp_header()` tire
   l'en-tête :
   - lien logit (binomial, quasi-binomial, multinomial, bêta,
     bêta-binomial) : cote de base (*baseline odds*) ;
   - lien log et famille binomiale : risque de base (*baseline risk*) ;
   - lien log et famille de comptage (Poisson, binomiale négative et
     leurs variantes de glmmTMB, brms et rstanarm) : taux de base
     (*baseline rate*) ;
   - lien log et famille gaussienne, Gamma, inverse gaussienne, Tweedie,
     lognormale, bêta ou quasi : moyenne de base (*baseline mean*) ;
   - lien cloglog et famille binomiale : risque cumulé de base (*baseline
     cumulative hazard*), car exp(η) = −log(1 − p) ;
   - `survreg` : durée de survie moyenne de base (loi exponentielle),
     médiane (lognormale, log-logistique), 63,2e centile (Weibull,
     Rayleigh), le quantile de la loi qui vaut exp(η) quelle que soit
     l'échelle.
   Toute autre combinaison ne reçoit pas de phrase : mieux vaut ne rien
   dire que nommer faux. Les modèles ordinaux n'ont pas d'intercept mais
   des seuils, jamais exponentiés, et le modèle de Cox n'en a pas.
2. Plusieurs modèles côte à côte : la phrase ne nomme aucun modèle quand
   tous ceux qui affichent un intercept ont le même estimand. Sinon elle
   donne chaque estimand suivi de ses modèles, nommés comme dans les
   autres notes par modèle : « The (Intercept) row estimates the baseline
   odds (Model 1); the baseline rate (Model 2). ». Un `lm` à côté d'un
   logit fait nommer le logit : « … the baseline odds (Model 2). ».
3. Les blocs de composantes (zéro-inflation, partie zéro d'un modèle à
   obstacle) ne sont exponentiés que sous un lien logit. Leur glose
   reçoit alors la même phrase, « The (Intercept) row estimates the
   baseline odds. », la cote de l'événement que la glose nomme déjà
   (zéro structurel, effectif non nul…). Les blocs laissés sur l'échelle
   du lien n'ont pas de phrase.
4. Chaînes : `note_exp_intercept` (le gabarit), `_models` (un estimand
   suivi de ses modèles) et une clé par estimand, article compris, en
   anglais et en français (« la cote de base », « le taux de base », « le
   risque de base », « la moyenne de base », « le risque cumulé de
   base », les trois durées de survie).

Classes vérifiées le 2026-11-13 : glm (logit, Poisson, quasi-Poisson,
binomial log, quasi-binomial, Gamma log, gaussien log, cloglog),
`glm.nb`, glmer, glmmTMB (nbinom2, zéro-inflation, obstacle, bêta),
fixest sans effets fixes (`feglm`, `fepois`), svyglm, gam, geeglm,
survreg (Weibull, lognormale, exponentielle, log-logistique), multinom,
mlogit, betareg, pscl (`zeroinfl`, `hurdle`), rms (`lrm` binaire,
`Glm`), brms (logit, ajustement en cache). Sans phrase : coxph, polr.

`show_intercept` existait déjà (défaut `TRUE`) : l'option 2 n'est plus
qu'un réglage de l'utilisateur.

## Conséquences

- Une phrase de plus dans la note d'un tableau exponentié qui montre
  son intercept.
- Un intercept renommé par `labels` garde « (Intercept) » dans la note.

## Pointeurs

- `R/glm_compute.R` : `spicy_glm_exp_header()`, gate `exponentiate` pour
  les liens non ratio.
- `R/i18n.R` : `header_exp_or` / `_irr` / `_rr` / `_mr`, `note_abbrev_mr`.
- `R/table_regression.R` : `.apply_exp_to_frame()`.
- Test existant : `tests/testthat/test-regression_glm.R`,
  « AUDIT: Gamma(log) exponentiate header is MR (mean ratio) ».
