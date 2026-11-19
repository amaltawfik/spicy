# ADR 2026-11-19 — Les termes lissés d'un GAM forment un bloc du tableau

**Statut** : principe approuvé par Amal le 2026-11-19. Mis en œuvre le même jour, branche `lot-gam-smooth-block`. Les points `keep` / `drop`, colonne de la statistique, `p_adjust` et étoiles ont été tranchés par Amal au cours du lot.

## Contexte

`table_regression()` ne montrait que les coefficients paramétriques d'un `mgcv::gam()` ou `bam()`. Le modèle `type ~ s(age) + bmi + glu + npreg` (logit, REML, données `rbind(MASS::Pima.tr, MASS::Pima.te)`, 532 observations) n'affichait aucune ligne pour l'âge. Pour `s(age)`, `summary()` donne pourtant edf 3,631, Ref.df 4,562, χ² 19,24 et p = 0,00175. Un modèle fait seulement de termes lissés (`type ~ s(age) + s(glu) + te(bmi, ped)`) n'affichait que la constante. La table des termes lissés était déjà calculée par le cadre (`info$extras$smooth_terms`, `R/regression_frame_mgcv.R`) et rien ne la lisait. La note ne signalait pas l'omission.

## La règle

1. Un bloc « Smooth terms (edf) », français « Termes lissés (ddl effectifs) », est affiché par défaut sous les coefficients paramétriques, avant tout autre bloc. Il compte une ligne par terme lissé, libellée comme mgcv la libelle : `s(age)`, `te(bmi,ped)`, `s(age):typeYes` pour un `by`, `s(agegrp)` pour un `bs = "re"`.
2. La colonne B porte les degrés de liberté effectifs et la colonne p la valeur p du test approché de mgcv. L'erreur standard, l'intervalle et la statistique sont des cellules sans objet, rendues par un tiret comme dans les autres blocs. Un χ² ou un F n'a pas sa place sous un en-tête z ou t. Les colonnes standardisées, AME et partielles restent vides. La ligne AME d'une variable lissée ne bouge pas.
3. Une phrase de note : « Smooth terms: effective degrees of freedom (edf) and approximate test of each term. », française « Termes lissés : degrés de liberté effectifs et test approché de chaque terme. ». Elle nomme la méthode, pas le package.
4. `exponentiate = TRUE` ne touche pas ces lignes. Aucune étoile ne marque l'edf. Les étoiles du package ne se posent que sur l'estimation, jamais sur la colonne p, il n'y avait donc rien d'autre à aligner.
5. Sous une variance robuste, le test de mgcv repose toujours sur sa propre matrice de covariance (Vp). La ligne garde son edf, la valeur p devient un tiret, et la note ajoute « The test of the smooth terms is not computed under a robust variance. ». Dans un tableau où un seul modèle est robuste, la phrase nomme ce modèle.
6. Les termes lissés sont des prédicteurs du modèle. Ils ne sont pas des paramètres de structure comme les seuils ou les effets aléatoires. Leurs lignes obéissent donc à `keep` et `drop` comme les coefficients, par leur libellé mgcv. `keep = "bmi"` retire `s(age)`, et `drop = "^s\\("` retire le bloc avec son en-tête et sa note. C'est la seule exception à l'exemption des blocs (`apply_keep_drop_filter()`). L'aide de `keep` le dit.
7. Les tests des termes lissés entrent dans la famille de `p_adjust`, avec les coefficients, comme ceux des composantes. L'aide de `p_adjust` le dit.
8. Plusieurs modèles : les lignes s'alignent par libellé de terme. Un `glm` à côté d'un GAM laisse ses cellules vides. `nested = TRUE` garde ses statistiques de changement.
9. `tidy()` et `output = "long"` portent ces lignes avec un type propre, `estimate_type = "edf"`, sur le modèle de `"vc"`. `estimate` est l'edf, `df` le Ref.df, `statistic` et `p.value` le test, `test_type` `"F"` ou `"X2"` comme `summary.gam()`. Sous une variance robuste, les trois derniers sont `NA`.
10. Pas de nouvel argument. Sans la colonne B, il n'y a pas de bloc, comme pour les composantes et les effets aléatoires.

## Mise en œuvre

Le bloc passe par le rail des blocs existants. L'identité « Smooth terms » est en tête de `.REG_BLOCK_TERMS` (`R/regression_align.R`), avec la clé de légende `label_block_smooth_terms`. Le tri en bas de tableau, l'en-tête, le filet de section et le libellé des lignes suivent donc sans code nouveau. `.append_smooth_term_rows()` (`R/regression_frame_mgcv.R`) matérialise les lignes dans la boucle de `table_regression()`, après l'exponentiation et avant `p_adjust`. Les deux corps du rendu (`build_body_row()` et `.build_structured_body_row()`) lisent `"edf"` dans la colonne B, comme `"vc"`. `.vc_cell_undefined()` met la cellule de statistique en tiret, et le corps typé garde la valeur. `build_smooth_terms_footer_block_from_frames()` écrit la note, qui disparaît avec le bloc. Dans `as_structured()` et `output = "data.frame"`, une ligne de bloc dont le libellé est le terme reçoit ce terme comme `.level`.

## Ce qui a été mesuré

Les quatre nombres de chaque terme (edf, Ref.df, statistique, p) égalent `summary(fit)$s.table` à 1e-10 pour un logit, une gaussienne (statistique F), un Poisson, un `nb()`, un `te()`, un `by =`, un `bs = "re"`, un `bam()` et un modèle sans terme paramétrique. Un GAM sans terme lissé n'a ni bloc, ni ligne `"edf"`, ni phrase de note. Avec `p_adjust = "holm"`, les valeurs p de `bmi`, `glu` et `s(age)` égalent `p.adjust()` appliqué aux trois ensemble, à 1e-12. Le bloc a été vérifié dans la console, `output = "data.frame"` et `"long"`, gt, tinytable, flextable, Excel et `as_structured()`. Le tiret y tient la place de l'erreur standard, de l'intervalle et de la statistique.

Le tableau du modèle Pima après le lot :

```
 Variable            │   B      SE       95% CI        p
─────────────────────┼─────────────────────────────────────
 (Intercept)         │  -8.13  0.84  [-9.77, -6.49]  <.001
 bmi                 │   0.08  0.02  [ 0.04,  0.11]  <.001
 glu                 │   0.04  0.00  [ 0.03,  0.04]  <.001
 npreg               │   0.05  0.05  [-0.04,  0.14]   .244
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
 Smooth terms (edf): │
   s(age)            │   3.63   –          –          .002
```

## Alternatives écartées

- Un argument `show_smooth` de plus, sur le modèle de `show_thresholds`. Le bloc montre une partie du modèle que le tableau cachait, et rien ne justifiait de le cacher par défaut. Qui veut le retirer dispose de `drop = "^s\\("`, puisque les termes lissés obéissent à `keep` et `drop`. Un argument aurait doublé ce chemin.
- L'AME porté par la ligne du terme lissé. L'AME de l'âge est une pente moyenne, le test du terme porte sur toute la courbe. Mettre l'un dans la ligne de l'autre aurait fait lire la pente comme le résumé du terme. La ligne AME de la variable reste où elle était, parmi les effets moyens.
- La statistique affichée par défaut, puis dans la colonne `"t"` sur demande. La valeur p dit déjà le résultat du test. Le χ² ou le F, sous un en-tête z ou t, aurait été lu comme un z ou un t. La statistique et le Ref.df restent dans `tidy()` et `output = "long"`.
- L'exemption de `keep` et `drop`, comme pour les autres blocs. Elle aurait laissé `s(age)` dans un tableau filtré par `keep = "bmi"` et rendu `drop = "^s\\("` inerte sans avertissement.
