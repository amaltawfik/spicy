# ADR 2026-10-18 — Argument `test` des tableaux catégoriels : χ² ou test exact, choisi par la règle de Cochran

**Statut** : accepté (2026-10-18, Amal : « go », défaut `"auto"` retenu après avis).

## Contexte

`table_categorical()` (publiée en 0.13.0) calcule toujours un χ² de Pearson et tait l'avertissement de `chisq.test()` sur les petits effectifs attendus. Un lot « warning classé » a été écrit puis abandonné le 2026-10-18 : un tableau de publication ne doit ni avertir ni imprimer un diagnostic d'effectifs attendus. Les normes demandent autre chose : un test valide pour les données (SAMPL ; AMA ch. 19 : χ² inapproprié avec de petits effectifs attendus, test exact de Fisher) et une note qui nomme le test de chaque p (JAMA, AMA §19.1.3, APA §7.14). gtsummary (`add_p()`) applique exactement cela : χ² si tous les effectifs attendus valent au moins 5, test exact de Fisher sinon, test nommé par variable en note. Stata (`tabulate, exact`), SAS (`EXACT FISHER`) et SPSS (Exact Tests) offrent le Fisher r×c ; les experts (Agresti ; Lydersen, Fagerland & Laake 2009 ; Campbell 2007) recommandent de changer de test plutôt que de signaler. Pour `cross_tab()`, outil de console, la note d'effectifs attendus à la SPSS reste la bonne réponse et ne change pas.

## Décision

1. `table_categorical()` gagne `test = c("auto", "chisq", "fisher")`, même sens que `test` dans `table_continuous()` (quel test par ligne) :
   - `"auto"` (défaut) : par variable, χ² de Pearson si tous les effectifs attendus sont au moins 5, sinon test exact de Fisher (généralisation r×c de `fisher.test()`) ; si l'énumération exacte échoue (espace de travail), test exact par Monte-Carlo avec `simulate_B` réplications. Règle de gtsummary, oracle gtsummary.
   - `"chisq"` : le comportement de la 0.13.0.
   - `"fisher"` : exact partout.
   - La note nomme le test de chaque variable sur le modèle de `table_continuous()` (« Group comparison: Pearson chi-squared test (sex, education); Fisher exact test (smoking). »), en anglais et en français par le registre. Les cadres `data.frame` et `long` portent le type de test par variable ; la statistique χ² et ses ddl sont NA pour un Fisher. `correct` et `simulate_p` ne concernent que le χ² ; `p_adjust` s'applique aux p quels qu'ils soient.
2. `table_multiresponse()` (non publiée) : la méthode du test global passe de `test` à `global_test = c("rs2", "bootstrap")` ; `test = c("auto", "chisq", "fisher")` gouverne les tests par item (p affiché et `p_unadjusted`, Holm sur ces p), avec la même règle. Le test global reste calculé sur les composantes de Pearson, comme la théorie l'exige ; un item testé par Fisher n'a pas de statistique affichée (tiret).
3. `table_categorical_svy()` : inchangée. Le test de Rao-Scott n'a pas d'équivalent exact et survey ne signale rien.
4. Pas automatisés, documentés : les tests exacts sont conservateurs sur les grands tableaux ; regrouper des modalités clairsemées change l'hypothèse testée et reste une décision de l'analyste. Le χ² « N−1 » et le mid-p des 2×2 ne sont pris par défaut par aucun outil de référence : hors périmètre 0.14.

## Rupture

Les tableaux clairsemés de `table_categorical()` publiés en 0.13.0 obtiennent d'autres p sous `"auto"` ; `test = "chisq"` restitue la sortie de 0.13.0. Admissible avant la 1.0, documenté dans NEWS. Rien à protéger pour `table_multiresponse()`.
