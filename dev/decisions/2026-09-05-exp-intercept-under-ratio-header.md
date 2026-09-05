# ADR 2026-09-05 — Intercept exponentié sous l'en-tête de ratio (OR / IRR / RR / MR)

**Statut** : proposé (2026-09-05). À arbitrer avant implémentation.

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

En attente. Recommandation de la discussion du 2026-09-05 : option 1.
Le modèle de Cox n'a pas d'intercept, rien à faire pour lui. Un
`show_intercept` peut venir plus tard.

## Conséquences (si option 1)

- Une note de bas de tableau supplémentaire quand `exponentiate = TRUE`
  et qu'un intercept est affiché.
- Chaînes i18n en et fr à ajouter ; snapshots des tests à mettre à jour.

## Pointeurs

- `R/glm_compute.R` : `spicy_glm_exp_header()`, gate `exponentiate` pour
  les liens non ratio.
- `R/i18n.R` : `header_exp_or` / `_irr` / `_rr` / `_mr`, `note_abbrev_mr`.
- `R/table_regression.R` : `.apply_exp_to_frame()`.
- Test existant : `tests/testthat/test-regression_glm.R`,
  « AUDIT: Gamma(log) exponentiate header is MR (mean ratio) ».
