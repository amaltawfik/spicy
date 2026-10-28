# ADR 2026-10-20 — Cohérence de la famille des tableaux

**Statut** : accepté (2026-10-20, Amal : « go pour tout », puis « si oui go » pour l'ordre des groupes, après la recommandation ci-dessous).

## Contexte

Amal : « un package de 15 ans doit être cohérent, robuste et stable ». Un audit en lecture seule des neuf fonctions de tableaux (main 7e39680e, comparaison avec v0.13.0, sondes dans `dev/_source/coherence_audit_2026-10-20/`, registre n°392) a relevé une cinquantaine d'écarts. Les bogues, la validation et les conditions classées relèvent des règles maison existantes et partent sans décision (lot A). Les points ci-dessous changent des contrats publiés en 0.13.0 : ils sont regroupés dans la 0.14.0 pour que les utilisateurs ne migrent qu'une fois avant la 1.0.

## Décisions

1. **Vocabulaire des colonnes machine.** Un seul vocabulaire dans `tidy()`, `glance()` et les sorties `data.frame` et `long` des tableaux : les noms de broom quand broom en définit un (`estimate`, `std.error`, `conf.low`, `conf.high`, `statistic`, `df`, `df.residual`, `p.value`, `nobs`), une seule orthographe pour les autres (taille d'effet, différence standardisée, effectif pondéré, nombre de groupes). `variable` désigne la colonne de `select`, `label` son libellé affiché.
2. **Tailles d'effet.** Une seule orthographe des valeurs d'`effect_size` dans toute la famille ; l'ancienne est refusée avec un message qui donne la nouvelle (règle maison : pas d'alias).
3. **`v_digits`** devient `effect_size_digits`, comme dans les autres fonctions ; l'ancien nom est refusé avec un message.
4. **Ordre des groupes d'un `by` texte ou logique.** Ordre alphabétique de la locale C (`method = "radix"`), et `FALSE` avant `TRUE`, dans toutes les fonctions. C'est déjà l'ordre de `freq()`, `cross_tab()` et `code_book()`, et celui de `dplyr::arrange()` depuis dplyr 1.1.0 : il ne dépend ni de l'ordre des lignes ni de la locale de la machine, donc un même script donne le même tableau sur Windows, Linux et macOS. Un facteur garde partout l'ordre de ses niveaux : c'est le moyen de choisir un autre ordre. Rupture pour six fonctions, qui suivaient l'ordre d'apparition dans les lignes.
5. **Sens d'`output`.** `"data.frame"` est le tableau affiché, une ligne par ligne imprimée, colonnes numériques ; `"long"` une ligne par statistique. `as.data.frame(x)` rend la même chose que `output = "data.frame"`.
6. **Proportions** entre 0 et 1 dans `tidy()`, comme broom, y compris dans la variante survey.
7. **Ligne Total** gardée dans `tidy()` partout, signalée par une colonne.
8. **Sorties Excel et Word** : la fonction rend le chemin du fichier, de façon invisible, partout, `table_regression()` comprise.
9. **`table_outcome()`** perd ses deux colonnes de différence standardisée, toujours vides faute d'argument `smd`.
10. **`inline()`** accepte `model =` pour désigner un groupe en colonnes dans toutes les fonctions ; `level =` reste l'adresse d'une modalité en ligne.
11. **`drop_na`** garde ses défauts (`FALSE` pour les tableaux catégoriels, `TRUE` pour les continus) ; l'aide énonce la règle commune au lieu d'invoquer un comportement historique.
12. **`boot_n`** : un seul validateur et un seul message ; les défauts restent propres à chaque fonction et sont justifiés dans l'aide.
13. **`table_multiresponse()`** est classée expérimentale pour sa première sortie, selon la définition de la page `?spicy` (nouvelle depuis 0.13.0).

## Conséquences

Puces de rupture dans NEWS 0.14.0 pour chaque contrat publié qui change. Un test de famille par règle (vocabulaire, ordre des groupes, sens d'`output`, retour des sorties fichier) empêche qu'un frère s'en écarte de nouveau.

## Complément du 2026-10-28 : les noms inconnus des arguments par variable

14. **Une seule politique pour les arguments nommés par variable** (`labels`, `note_labels`, `digits`, `show_columns`) : un nom qui ne correspond à aucune colonne du tableau déclenche un avertissement classé (`spicy_unknown_name`) qui nomme l'argument et le nom, puis l'appel continue sans lui. Décision d'Amal du 2026-10-28, sur recommandation. Jusque-là `labels` et `digits` ignoraient le nom en silence (un vecteur commun à tout un projet reste possible, mais une faute de frappe passait sans bruit) et `show_columns` le refusait par une erreur (la faute était vue, mais le vecteur commun devenait impossible). L'avertissement garde les deux avantages ; qui veut le silence le coupe par `suppressWarnings()`. La 0.14.0 n'étant pas publiée, aucune puce de rupture pour `labels`, `note_labels` et `digits` ; `show_columns` passe d'une erreur à un avertissement, à dire dans sa puce si le refus datait de 0.13.0. À mettre en œuvre dans le lot M, avec une attente de parité de famille.
