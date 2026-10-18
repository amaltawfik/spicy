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

## Complément (2026-10-18) — options `"midp"` et `"n1"` pour les 2×2

Amal : « fais ce qu'il y a de plus pro et robuste pour un package de référence sur 15 ans ». Les méthodologistes préfèrent au Fisher conditionnel, conservateur, le mid-p de Fisher ou le χ² « N−1 » (Lydersen, Fagerland & Laake 2009, *Stat Med* 28:1159-1175, §9 p. 1174 : « The traditional Fisher's exact test should practically never be used » ; Campbell 2007, *Stat Med* 26:3661-3675 ; Agresti 2019 p. 49 : « we recommend using the mid P-value »). Aucun outil de référence ne les prend par défaut : `"auto"` reste sur Fisher, et les deux entrent en options, pour les tableaux 2×2 (mémo d'oracles : scratchpad/midp/memo.md, valeurs reproduites en R de base et confrontées à exact2x2, epitools, DescTools, contingencytables, et aux valeurs publiées).

- `test = "n1"` : la règle de Campbell 2007 (p. 18-19) : χ² de Pearson multiplié par (N−1)/N, référé à χ²(1), quand le plus petit effectif attendu vaut au moins 1 ; sinon test exact de Fisher (règle d'Irwin = `fisher.test()` bilatéral). Campbell ne défend le N−1 que sous cette restriction (p. 11-13). Oracle : `DescTools::MHChisqTest()` (forme de Mantel-Haenszel), Busing et al. 2016 (table de Campbell : p = 0,1270).
- `test = "midp"` : le mid-p de Fisher **central** : deux fois le plus petit des deux mid-p unilatéraux, plafonné à 1 (Agresti 2019 p. 49 ; Lydersen, Laake & Fagerland 2011, *Stat Med* 30:891-892 : « version twice the smallest tail », niveau réel maximal 0,057 ; Campbell 2007 p. 12-13 : la version doublée est la plus proche du niveau nominal). C'est la définition d'`exact2x2::fisher.exact(midp = TRUE)` et d'`epitools::ormidp.test()`, les oracles. La variante par probabilités de tables de Lydersen 2009 (éq. 12, défaut de `contingencytables`, dont l'implémentation compare des probabilités par `==` et se trompe sur 37,6 % des tables à marges égales) n'est pas retenue ; l'aide dit laquelle est calculée et que les deux coïncident à marges égales. Valeurs publiées reproduites : thé d'Agresti 0,25714 ; table I de Lydersen 2009 0,0297 ; lampasona 2013 0,046.
- Hors 2×2 (variable à plus de deux modalités, ou `by` à plus de deux groupes) : la variable suit la règle `"auto"` (Pearson si tous les attendus ≥ 5, sinon Fisher r×c) et la note nomme le test de chaque variable. Pas d'erreur : un tableau mêle des variables de dimensions différentes.
- Marge nulle (table non informative, Lydersen 2009 p. 1162) : p = NA, tiret, pour tous les tests.
- Égalités de probabilités : tolérance relative de `fisher.test()` (1 + 1e-7).
- Noms en note : « 'N−1' chi-squared test », « Fisher mid-p test » ; en français « test du khi-deux « N−1 » », « test mid-p de Fisher ».
- Même argument, mêmes valeurs pour les items 2×2 de `table_multiresponse()` ; le test global garde ses composantes de Pearson.

## Rupture

Les tableaux clairsemés de `table_categorical()` publiés en 0.13.0 obtiennent d'autres p sous `"auto"` ; `test = "chisq"` restitue la sortie de 0.13.0. Admissible avant la 1.0, documenté dans NEWS. Rien à protéger pour `table_multiresponse()`.
