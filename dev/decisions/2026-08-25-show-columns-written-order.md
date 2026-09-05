# ADR 2026-08-25 — `show_columns` : honorer l'ordre d'écriture des jetons ?

**Statut** : proposé (constat du 2026-08-25, promu en ADR le 2026-09-06). À arbitrer avant implémentation.

Comportement documenté, donc choix de design et non bug : la doc publiée dit que les colonnes suivent l'ordre canonique quel que soit l'ordre écrit. Le besoin, les options et l'impact sont ci-dessous, tels que consignés depuis le projet HESAV Healthy Campus.

---

# À vérifier : `show_columns` ne respecte pas l'ordre d'écriture

Déposé le 25.08.2026 depuis le projet HESAV Healthy Campus. Rien n'est
modifié dans le code — cette note consigne le constat et une piste.

## Constat, mesuré sur l'arbre de dev (0.12.0.9000, install du 25.08)

Trois essais, trois formes, même résultat : la colonne `Med` sort toujours
après `M`, quel que soit l'ordre des jetons.

| Essai | Jetons | Colonnes rendues |
| --- | --- | --- |
| vecteur | `c("med", "q1", "q3", "m", "sd", "n")` | M, SD, Med, Q1, Q3, n |
| vecteur | `c("m", "sd", "med", "q1", "q3", "n")` | M, SD, Med, Q1, Q3, n |
| liste | `list(.default = c("med", "m", "n"))` | M, Med, n |

Mécanisme : `resolve_continuous_show_columns()` passe tout par
`order_continuous_tokens()`, qui retrie sur `.continuous_column_tokens`
(l'ordre canonique). L'attribut le montre : jetons `med,q1,q3,m,sd,n`
→ `attr(out, "show_columns")` vaut `m,sd,med,q1,q3,n`.

La doc publiée est cohérente avec ce comportement (« Columns appear in the
canonical order of the table above, whatever order they were written in »,
reference/table_continuous.html#choosing-the-statistics) : ce n'est pas un
bug de code, c'est un choix de design à réexaminer.

## Le besoin

Un tableau de variables franchement asymétriques (pas quotidiens, volumes
IPAQ) veut la **médiane en tête**, le protocole IPAQ la prescrivant (5.1,
5.4). L'auteur du tableau écrit `c("med", "q1", "q3", "m", "sd", "n")` et
attend cet ordre-là.

## Piste, si le design est réexaminé

- **Vecteur simple** : honorer l'ordre d'écriture — un vecteur est UNE
  consigne d'ordre explicite ; doublons réduits à la première occurrence
  (`unique()` suffit, la validation amont écarte déjà les jetons inconnus).
- **Forme liste par variable** : garder le canonique — des jeux de jetons
  différents entre variables n'ont pas d'ordre écrit commun à honorer
  (l'union `m, sd, med_iqr, n` du test existant resterait telle quelle).
- Deux points de contact dans `resolve_continuous_show_columns()` : la
  branche vecteur (`global <- order_continuous_tokens(show_columns)`) et
  l'union finale (`union_tokens <- order_continuous_tokens(...)`), qui
  re-trie même ce que la branche vecteur aurait préservé.
- Doc à ajuster (la phrase « canonical order … whatever order ») et un test
  à ajouter : « median first stays first », plus un cas de doublon.

## Impact à évaluer avant de trancher

- `table_continuous_svy()` ordonne ses propres jetons (`SE`, `DEff`) par un
  vecteur distinct : vérifier qu'il n'hérite pas du changement sans le dire.
- Les parités d'engines (tinytable / flextable / gt / xlsx / word) doivent
  suivre le même ordre : les tests `engine_parity_*` le diront.
- Le spanner du CI (`LL`/`UL`) suppose peut-être des positions relatives
  (`ci` après `m`) : vérifier le rendu quand `ci` précède `m` dans l'ordre
  écrit — ou l'interdire explicitement, comme l'orphelin `ci` sans `m`
  l'est déjà.
