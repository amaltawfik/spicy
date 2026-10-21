# ADR 2026-10-20 — Les abréviations dans la note des tableaux

**Statut** : accepté (2026-10-20, Amal : « oui » à la recommandation complète, avec « Mdn »). Remplace la décision du 18.10 (issue #11) qui faisait définir par défaut toutes les abréviations de `table_continuous()` (« M = moyenne ; … »).

## Contexte

La décision du 18.10 visait un rapport destiné à des non-spécialistes. La collecte du 20.10 (`dev/journal_styles_sources.md`, section 5 ter) montre que les styles divergent :

- L'APA interdit de définir M, SD, SE, n, p et OR, et fait définir CI.
- L'AMA ne développe ni SD ni CI, mais ouvre la note sur une ligne « Abbreviations: » qui développe OR, HR, RR et IQR.
- Le Lancet laisse SD, IQR et CI sans définition et définit le reste au format « BMI=body-mass index. ».
- Personne ne définit n, N ou p.

Aujourd'hui spicy ne suit aucune de ces règles. La note des régressions définit OR mais pas CI, et celle des tableaux continus ne glose que IQR.

## Décisions

1. **Un levier `abbreviations` dans `spicy_style()`.** Il vaut pour les neuf fonctions de tableaux et suit le thème, comme `note_label`. Non réglé, il applique la règle du thème. `"all"` définit toute abréviation affichée, sauf n, N et p, qu'aucun style ni aucune revue ne définit : c'est la valeur pour un lectorat non spécialiste. `"none"` retire les définitions simples.
2. **Règle et forme par thème.** Tout ce qui n'est pas exempté ci-dessous est défini (APA §6.25, AMA §4.1.7, Lancet « spell out all abbreviations »).

   | Thème | Non définis | Forme et place |
   |---|---|---|
   | défaut et `"apa"` | les symboles que l'APA nomme (M, SD, SE, F, t, df, p, N, n, OR, r : §6.44 et §7.15) et ceux que ses tableaux modèles laissent sans définition (B, β, R², ΔR²) | « CI = confidence interval; IQR = interquartile range. » à la fin de la note générale, dans l'ordre d'apparition (§7.14) |
   | `"jama"` | SD, SE, SEM et CI (§13.11), puis n, N, P et les autres entrées de §19.6 qui ne portent pas « expand at first mention » | première ligne « Abbreviations: HR, hazard ratio; IQR, interquartile range. », ordre alphabétique, « Abbreviation: » au singulier (§4.1.4.10) |
   | `"lancet"` | n, N, p, SD, SE, IQR et CI (tableau modèle de 2025 et pratique de 35 tableaux) | « HR=hazard ratio. », une phrase par abréviation, ordre alphabétique, à la fin de la note générale |

   `"nejm"`, `"annals"` et `"aer"` n'ont pas de règle trouvée. Ils héritent du défaut, selon la règle de provenance des thèmes (2026-08-14, en tête de `R/spicy_style.R`).
3. **Une définition qui en dit plus que le sigle reste sous toute règle**, `"none"` compris : η² partiel, ω² corrigé du biais, χ² de Wald ou du rapport de vraisemblance, la méthode de standardisation de β, IQR comme largeur Q3 − Q1, pd, MCSE. Elle renseigne sur le calcul. Elle prend la forme du thème. Les phrases de méthode (sens de la différence standardisée, intervalle de la médiane) ne sont pas des abréviations et restent à leur place.
4. **Les mots abrégés** (Min, Max, Freq., Cum., Pos., Adj.) ne se définissent que sous `"all"`, comme « No. », que JAMA Network Open ne développe dans aucun de ses 15 tableaux.
5. **L'en-tête anglais de la médiane** passe de « Med » à « Mdn », le symbole de la Table 6.5 de l'APA. Le français garde « Méd. ».
6. **Une seule mécanique** pour la famille : un registre qui associe chaque concept affiché (et non sa chaîne, qui change avec la langue) à son développement en anglais et en français, et un collecteur qui ne retient que les abréviations effectivement affichées. Les exemptions se déclarent par concept.

## Précisions à la mise en œuvre (2026-10-21)

- **Périmètre du collecteur** : les en-têtes, les chapeaux et les libellés de lignes du tableau (statistiques d'ajustement comme AIC, BIC, ICC, RMSE, WAIC ; lignes d'effets aléatoires). L'APA nomme AIC et BIC parmi les abréviations à définir (§6.44). Hors périmètre : les symboles à l'intérieur des cellules et les mots de la prose de la note (ANOVA, OLS, GEE, REML, PSU).
- **Jamais définies, sous aucune règle ni sous `"all"`** : n, N, p et les statistiques de test (t, z, F, χ², W, H), qu'aucun style ni aucune revue observée ne définit.
- **APA** : la liste du §6.44 est un « e.g. » ; Mdn, z et g sont exemptés au même titre que M, SD et d. Les lettres grecques (§6.44, §7.15) et R² avec ses variantes (tableau modèle p. 219) sont exemptés.
- **jama** : B et M sont le *b* et le x̄ de la liste §19.6, donc exemptés.
- **Pseudo-R²** (McFadden, Nagelkerke, Tjur, R² bayésien) : défini sous lancet et `"all"` comme « pseudo coefficient of determination », chaque ligne sous son propre libellé.
- **Les clés de colonnes gardent « Med »** (contrat de la 0.13.0) ; seul l'en-tête affiché devient « Mdn ».
- Un `note_label` saisi sous jama précède la ligne des abréviations.

## Conséquences

Ruptures visibles, chacune dans NEWS :

- La note par défaut des régressions perd « OR = odds ratio » et gagne « CI = confidence interval ».
- Le thème `jama` ouvre la note sur sa ligne « Abbreviations: ».
- L'en-tête « Med » devient « Mdn ».

Un test de famille par thème et par valeur du levier empêche qu'une fonction s'en écarte.
