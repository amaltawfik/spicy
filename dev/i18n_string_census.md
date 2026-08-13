# Registre i18n de spicy — étage 1 : extraction à défaut byte-identique

**Statut :** spécification arrêtée. Décision Amal du 2026-08-13.
**Base de code :** `main` @ `f1ff6661`. Les numéros de ligne cités sont ceux de
ce commit ; ils sont **indicatifs** (un `git grep` du littéral les retrouve).
**Antécédent :** `dev/output_labels_i18n.md` (constat de terrain, HESAV Healthy
Campus) — ce fichier-ci le remplace comme plan d'action et tranche les pistes
qui y étaient laissées ouvertes.

---

## 0. Objet et contrat

### 0.1 Ce qu'est l'étage 1

Sortir de `R/` **toutes les chaînes que le lecteur d'un tableau voit** et les
poser dans un **registre interne à clés stables**. Rien d'autre.

> **Contrat non négociable de l'étage 1 : la sortie du paquet reste identique à
> l'octet.** Aucun snapshot de test ne bouge. Aucun `NEWS` utilisateur. Aucun
> argument public nouveau. Aucune option nouvelle. C'est un refactor pur : les
> mêmes octets, produits depuis un seul endroit au lieu de trois cents.

Un lot qui fait bouger un snapshot est un lot **raté**, sauf si le déplacement
est explicitement listé comme correction dans ce fichier (section 4.7) et
approuvé séparément.

### 0.2 Ce qu'est l'étage 2 (hors périmètre ici, mais dimensionnant)

```r
options(spicy.language = "fr")                    # table de libellés livrée
options(spicy.labels = c(row_missing_level = "(Sans réponse)"))  # surcharge
```

Ordre de résolution prévu : `spicy.labels` (surcharge par clé) → table de la
langue → **défaut anglais** (toujours présent, toujours le repli). Une clé
absente d'une table de langue retombe en anglais sans erreur.

L'étage 1 doit rendre l'étage 2 *ennuyeux* : il ne restera qu'à écrire le corps
de `spicy_str()` et une table `fr`.

### 0.3 Pourquoi pas `gettext()` / les `.po` de R

R offre nativement `gettextf()` et un mécanisme de traduction par catalogue
`.po`. Écarté, pour trois raisons :

1. **La clé de gettext est la chaîne anglaise elle-même.** Toute correction de
   formulation (une virgule, un pluriel) invalide silencieusement la traduction.
   Le paquet corrige ses libellés régulièrement ; une clé stable est la seule
   défense.
2. **Le déclencheur est `LC_MESSAGES`**, c'est-à-dire la locale du système. Le
   besoin réel est l'inverse : un utilisateur en locale anglaise qui rend un
   rapport francophone (cas HESAV). Il faut une option de paquet, pas une locale.
3. **Pas de surcharge par clé.** `spicy.labels` (renommer *un* libellé, par
   exemple `(Missing)` → `(Sans réponse)`) est impossible avec gettext.

Le registre est donc un vecteur de caractères nommé, interne au paquet.

### 0.4 Forme technique imposée

Nouveau fichier `R/i18n.R` (pas de champ `Collate` dans le `DESCRIPTION` : la
collation alphabétique suffit) :

```r
# Registre des chaînes d'affichage. Clés stables, valeurs = défaut anglais.
# Toute chaîne vue par le lecteur d'un tableau vit ici et nulle part ailleurs.
.spicy_strings <- c(
  row_missing_level = "(Missing)",
  note_missing_removed = "Missing values removed: ",
  # ...
)

# Libellé brut. Erreur dure sur clé inconnue : une clé absente est un bug de
# développement, jamais une condition d'exécution.
spicy_str <- function(key) {
  val <- .spicy_strings[[key]]        # [[ ]] : erreur si la clé n'existe pas
  val
}

# Libellé interpolé. Le gabarit est un format sprintf ; les trous sont des
# DONNÉES (noms de variables, effectifs), jamais des mots à traduire.
spicy_fmt <- function(key, ...) {
  sprintf(spicy_str(key), ...)
}
```

À l'étage 2, seul le corps de `spicy_str()` change (consultation de
`getOption("spicy.labels")`, puis de la table de langue, puis du défaut).
`spicy_fmt()` reste inchangé.

---

## 1. Conventions du registre

### 1.1 Règle de déduplication

> **Une ligne du registre = une chaîne unique par (texte, sens).**

- Même texte + même sens, à N endroits → **une clé**, N consommateurs listés.
  C'est le cas le plus fréquent et le gain principal du lot.
- Même texte + **sens différent** → **deux clés**. Exemple : `ω²` est le
  fit-stat oméga-carré *global* et l'en-tête de colonne de l'oméga-carré
  *partiel*. Une langue ne les distinguera pas, mais le paquet peut vouloir
  les faire diverger (glose, exposant) sans effet de bord.
- Textes différents par la **casse** = textes différents (`N` ≠ `n`).
- Symboles mathématiques nus (`β`, `χ²`, `σ`) : **une seule clé par glyphe**,
  quel que soit le nombre de rôles. Aucune locale ne les fera diverger ; les
  éclater ne créerait que de la dérive de graphie et d'encodage.

### 1.2 Nommage des clés

Préfixe = rôle du consommateur :

| Préfixe | Rôle |
| --- | --- |
| `header_` | apparaît **uniquement** en en-tête de colonne / spanner |
| `row_` | apparaît **uniquement** en libellé de ligne |
| `cell_` | apparaît **uniquement** en contenu de cellule |
| `label_` | **partagé** entre plusieurs rôles (en-tête *et* ligne, ou en-tête *et* note) |
| `title_` | titre de tableau, légende, nom d'onglet Excel |
| `note_` | note de bas de tableau (texte imprimé sous le tableau) |
| `test_` | nom de test statistique |
| `stat_` | nom de mesure d'association |
| `fitstat_` | libellé de ligne de statistique d'ajustement (régression) |
| `symbol_` | glyphe mathématique nu, gelé |
| `marker_` | marqueur typographique gelé (`<NA>`, `…`, `*`) |
| `punct_` | séparateur / ponctuation |
| `col_` | **nom de colonne data-side** : inscrit, jamais traduit |

Pas de segment de famille dans la clé (`cat_`, `cont_`, `reg_`) **sauf** quand
il faut lever une collision de sens (`header_lm_es_g` n'existe pas : voir §3).
Le registre est plat et global — c'est ce qui rend les duplications visibles.

### 1.3 Colonne `statut`

| Statut | Signification |
| --- | --- |
| `câblé` | l'étage 1 route le site d'appel par le registre |
| `gelé` | inscrit pour **verrouiller** la graphie et l'encodage ; jamais traduit (acronymes, symboles, marqueurs R) — l'étage 1 le câble quand même, pour que la valeur ne puisse plus dériver |
| `réservé` | inscrit, **non câblé** à l'étage 1 : dette documentée, gain nul à court terme |
| `data` | nom de colonne ou niveau de facteur : **ne jamais traduire, ne jamais router** |

### 1.4 Conventions d'écriture dans les tableaux ci-dessous

- `␣` marque un **espace significatif** en tête ou en fin de chaîne. Le texte
  réel contient une espace, pas ce caractère.
- `\n` est l'échappement C tel qu'il apparaît dans la source.
- `\|` est une **barre verticale littérale**, échappée pour ne pas casser le
  markdown. Le texte réel ne contient pas la barre oblique inverse.
- Les gabarits sont donnés en **format `sprintf`**. Quand le site d'appel
  utilise aujourd'hui `paste0()`, la conversion en `sprintf` fait partie du
  câblage, et la byte-identité doit être vérifiée par test (§5.1).

### 1.5 Règle des gabarits

> **Un gabarit se traduit entier, jamais par morceaux.**

`paste0("Categorical table by ", var)` n'entre pas au registre comme préfixe :
il entre comme `"Categorical table by %s"`. La position du trou est une
propriété de la langue (`"Tableau catégoriel selon %s"` garde l'ordre,
`"%s comparison"` → `"Comparaison de %s"` l'inverse). Un préfixe seul est
intraduisible.

Corollaire : **un trou répété prend la forme positionnelle** `%1$s`. Voir
`note_gloss_med_ci`, où le même pourcentage apparaît deux fois.

Corollaire 2 : un gabarit `sprintf` qui contient un `%` littéral doit
l'échapper en `%%`. Les chaînes qui **sont** un `%` (l'en-tête `header_pct`)
passent par `spicy_str()`, jamais par `spicy_fmt()`.

### 1.6 Règle clé / libellé — non négociable

> **Aucune mécanique du paquet ne doit dépendre d'un texte affiché.**
> Ni un `%in%`, ni un `grepl`, ni un `sub`, ni un nom de colonne, ni un niveau
> de facteur.

C'est la contrainte structurante de tout le lot. Elle se décline en §4.1–4.5.
Là où la mécanique dépend aujourd'hui du texte, **l'étage 1 doit d'abord
introduire le jeton interne**, et seulement ensuite câbler le libellé.
Un lot qui câble un libellé sans avoir découplé sa mécanique est un lot raté.

---

## 2. Le registre

Total ≈ **340 entrées**, dont ≈ **250 câblées** à l'étage 1.
Les entrées `réservé` et les lacunes de recensement (§2.15) portent le reste.

### 2.1 Ponctuation et séparateurs

Inscrits pour que l'étage 2 puisse appliquer la typographie française (espace
insécable avant `;` et `:`). **Non câblés à l'étage 1** : router trois cents
séparateurs pour zéro gain visible serait un diff ingérable. Les sites listés
sont ceux que le recensement a explicitement identifiés comme porteurs.

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `punct_item_sep` | `,␣` | — | `table_categorical.R:282,286` (glose mesures), `regression_titlefooter.R:730,2113` (légendes), `cross_tab.R` (notes manquants) | non | réservé |
| `punct_clause_sep` | `;␣` | — | `table_continuous.R:1655`, `regression_titlefooter.R:570,812,998,2216` | non | réservé |
| `punct_kv_sep` | `:␣` | — | `table_categorical.R:282`, `regression_titlefooter.R:43` (titre) | non | réservé |
| `punct_sentence_end` | `.` | — | fins de notes assemblées par `paste0` | non | réservé |
| `punct_alt_sep` | `␣/␣` | — | `regression_titlefooter.R:1943` (en-têtes de ratio multiples) | non | réservé |
| `punct_range_dash` | `-` | — | `note_vcov_bootstrap_reps_range`, `note_uv_varying_n` (devrait être un tiret demi-cadratin) | non | réservé |

### 2.2 Symboles mathématiques — gelés

Une clé par **glyphe**, tous rôles confondus (§1.1). Jamais traduits ; inscrits
pour verrouiller la graphie, l'encodage et la largeur calculée par
`crayon::col_nchar()`.

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `symbol_t` | `t` | — | `regression_render.R:630` (en-tête), `table_continuous_lm_render.R:1065` | `get_test_header_lm()` (partiel) | gelé |
| `symbol_z` | `z` | — | `regression_align.R:311`, `table_continuous_lm_render.R:1053` | `get_test_header_lm()` (partiel) | gelé |
| `symbol_chi_sq` | `χ²` | — | `regression_render.R:778`, `table_continuous_lm_render.R:1059` | non | gelé |
| `symbol_beta` | `β` | — | `regression_render.R:654`, notes `regression_titlefooter.R:496,498,679,686,692` | non | gelé |
| `symbol_hedges_g` | `g` | — | `table_continuous.R:2106` (préfixe de cellule ES), `table_continuous_lm_render.R:1094` (en-tête ES) | non | gelé |
| `symbol_cohens_d` | `d` | — | `table_continuous_lm_render.R:1093` | `format_effect_size_header_lm()` | gelé |
| `symbol_eta_sq_global` | `η²` | — | `table_continuous.R:2107` (eta² d'ANOVA, **global**) | non | gelé |
| `symbol_eta_sq_partial` | `η²` | — | `regression_render.R:756` (eta² **partiel**) | non | gelé |
| `symbol_epsilon_sq` | `ε²` | — | `table_continuous.R:2109` | non | gelé |
| `symbol_omega_sq_global` | `ω²` | — | `regression_render.R:1572` (fit-stat global) | `fit_stat_label()` | gelé |
| `symbol_omega_sq_partial` | `ω²` | — | `regression_render.R:766`, `table_continuous_lm_render.R:1095` | non | gelé |
| `symbol_f2_global` | `f²` | — | `regression_render.R:1596` (fit-stat global) | `fit_stat_label()` | gelé |
| `symbol_f2_partial` | `f²` | — | `regression_render.R:746`, `table_continuous_lm_render.R:1092` | non | gelé |
| `symbol_r2` | `R²` | — | `regression_render.R:1570`, `table_continuous_lm_render.R:1103` | `fit_stat_label()` / `format_r2_header_lm()` | gelé |
| `symbol_sigma_hat` | `σ̂` | — | `regression_render.R:1594` | `fit_stat_label()` | gelé |
| `symbol_r_rb` | `r_rb` | — | `table_continuous.R:2108` | non | gelé |
| `symbol_star_001` | `***` | — | `regression_render.R:1766`, `regression_titlefooter.R:715` | **non — dupliqué, ordre inversé** (§4.7) | gelé |
| `symbol_star_01` | `**` | — | idem | non | gelé |
| `symbol_star_05` | `*` | — | idem | non | gelé |

> Note d'encodage : `σ̂` (`regression_render.R:1594`) et `χ̄²`
> (`regression_titlefooter.R:1585`) sont des **caractères composés**
> (base + diacritique combinant). Sensibles à la normalisation Unicode et au
> calcul de largeur ASCII. Le registre les fige ; ne jamais les ressaisir à la
> main, toujours les copier depuis la source. Voir aussi `dev/fix_nonascii.R`.

### 2.3 Marqueurs typographiques et placeholders

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `marker_na` | `<NA>` | — | `varlist-values.R:257` (comptage), `varlist-values.R:403` (niveau déclaré) | non | gelé |
| `marker_nan` | `<NaN>` | — | `varlist-values.R:259` | non | gelé |
| `marker_ellipsis_values` | `...` | — | `varlist-values.R:106` (résumé compact) | non | gelé |
| `marker_truncation_ellipsis` | `…` (U+2026) | — | `tables_ascii.R:413` (spanner tronqué) | non | gelé |
| `marker_varlist_transformed` | `*` | — | `varlist-title.R` (suffixe de titre Viewer) | `varlist_title()` | gelé |
| `cell_undefined` | `--` | — | `table_continuous.R:2066,2206` **et cité dans** `note_gloss_med_ci_undefined` (1684) | non | câblé |
| `cell_na` | `NA` | — | `freq_print.R:58` (`fmt_pct`, proportion non définie) | non | gelé |
| `row_na_label` | `NA` | — | `freq_print.R:74` (ligne NA système), `cross_tab.R:623` (niveau NA promu) | non | **data** (§4.2) |

> `cell_undefined` (`--`) et le vide **structurel** `""` de
> `table_continuous.R:2181-2184` (« cette colonne appartient à une autre
> variable ») sont deux concepts distincts. Ne pas fusionner : le premier dit
> « non calculable », le second dit « sans objet ». Le second n'entre pas au
> registre (ce n'est pas un mot).
>
> `regression_titlefooter.R:1655` cite le tiret demi-cadratin `–` de cellule
> *dans le texte de la note* `note_singular_rank_deficient`. Si le glyphe de
> cellule change, la note ment. Enregistrer le glyphe de cellule vide de la
> régression au lot 6 et faire dériver la note de la même clé.

### 2.4 `freq()` et `cross_tab()`

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `header_category` | `Category` | — | `freq_print.R:76` | non | câblé — **verrou** §4.1 |
| `label_values` | `Values` | — | `freq_print.R:77`, `cross_tab.R:751`, `varlist.R:188,283` | non | câblé — **verrou** §4.1/§4.2 |
| `header_freq` | `Freq.` | — | `freq_print.R:78` | non | câblé |
| `header_percent` | `Percent` | — | `freq_print.R:79` | non | câblé |
| `header_valid_percent` | `Valid Percent` | — | `freq_print.R:85` | non | câblé |
| `header_cum_percent` | `Cum. Percent` | — | `freq_print.R:89` | non | câblé |
| `header_cum_valid_percent` | `Cum. Valid Percent` | — | `freq_print.R:91` | non | câblé |
| `row_valid` | `Valid` | — | `freq_print.R:97` | non | câblé |
| `row_missing_block` | `Missing` | — | `freq_print.R:98` | non | câblé |
| `label_total` | `Total` | — | `freq_print.R:103` (ligne), `cross_tab.R:753` (colonne), `cross_tab.R:823,853,877` (ligne), `table_categorical.R:1957` (colonne de marge) | non | câblé — **verrou** §4.1/§4.3 |
| `label_total_dedup` | `Total_%d` | 1 trou : index | `table_categorical.R:1961` | non | câblé — couplé à `label_total` |
| `header_n_upper` | `N` | — | `cross_tab.R:758`, `regression_render.R:614` | `build_column_spec()` (côté régression) | câblé |
| `row_n` | `N` | — | `cross_tab.R:832` (ligne d'effectifs, `percent = "column"`) | non | câblé |
| `note_label` | `Label: %s` | 1 trou : label utilisateur | `freq_print.R:156` | non | câblé |
| `note_class` | `Class: %s` | 1 trou : classe R | `freq_print.R:161` (aujourd'hui `paste()`) | non | câblé |
| `note_data` | `Data: %s` | 1 trou : nom du data frame | `freq_print.R:162` (aujourd'hui `paste()`) | non | câblé |
| `note_weight` | `Weight: %s` | 1 trou : nom du poids | `freq_print.R:167` (`paste()`), `cross_tab.R:1108` (`paste0`) | non — **deux graphies** (§4.7) | câblé |
| `note_weight_applied` | `Weight: (applied)` | — | `freq_print.R:169` | non | câblé |
| `note_weight_rescaled` | `␣(rescaled)` | — | `freq_print.R:173` (espace via `paste`), `cross_tab.R:1110` (espace dans le littéral) | non — **deux graphies** (§4.7) | câblé |
| `title_freq` | `Frequency table: %s` | 1 trou : nom de variable | `freq_print.R:183` | **non** — à créer | câblé |
| `title_crosstab` | `Crosstable: %s x %s%s` | 3 trous : var ligne, var colonne, suffixe de pourcentage | `cross_tab.R:1096-1098` | **non** — à créer | câblé |
| `title_crosstab_by` | `␣x␣` | — | `cross_tab.R:1098` (titre) | non | câblé (absorbé par `title_crosstab`) |
| `title_crosstab_group` | `%s \| %s = %s` | 3 trous : titre, nom du `by`, niveau | `cross_tab.R:1102` | non | câblé |
| `title_percent_row` | `␣(Row %)` | — | `cross_tab.R:1091` | non | câblé — **verrou** §4.1 |
| `title_percent_column` | `␣(Column %)` | — | `cross_tab.R:1092` | non | câblé — **verrou** §4.1 |
| `title_percent_none` | `␣(N)` | — | `cross_tab.R:1093` | non | câblé — **verrou** §4.1 |
| `note_p_prefix_lt` | `p␣%s` | 1 trou : p formatée (`<.001`) | `cross_tab.R:992` | non | câblé |
| `note_p_prefix_eq` | `p = %s` | 1 trou : p formatée | `cross_tab.R:994` | non | câblé |
| `test_chisq` | `Chi-2(%s) = %s, %s` | 3 trous : ddl, statistique, p formatée | `cross_tab.R:1003` (assemblé en morceaux) | non | câblé — §4.7 (nom du test incohérent) |
| `note_chisq_simulated` | `␣(simulated)` | — | `cross_tab.R:1009` | non | câblé |
| `note_assoc_ci` | `, 95% CI [` | — | `cross_tab.R:1025` (crochet fermant ligne 1029) | non | câblé — §4.7 (95 codé en dur) |
| `note_yates_applied` | `Yates continuity correction applied.` | — | `cross_tab.R:1037` (le `\n` de tête est de l'assemblage, **pas** du registre) | non | câblé |
| `note_stats_subtable` | `Stats computed on %dx%d sub-table after dropping empty rows / columns.` | 2 trous : nrow, ncol | `cross_tab.R:1043` | non | câblé |
| `note_warning_prefix` | `Warning:␣` | — | `cross_tab.R:1066` (texte de tableau, **pas** une condition R) | non | câblé |
| `note_expected_lt5` | `%d expected cell%s < 5 (%s%%).` | 3 trous : compte, marque de pluriel, pourcentage | `cross_tab.R:1068-1074` (aujourd'hui éclaté, pluriel manuel) | non | câblé — §4.4 |
| `note_expected_lt1` | `%d expected cell%s < 1.` | 2 trous : compte, marque de pluriel | `cross_tab.R:1077-1080` | non | câblé — §4.4 |
| `note_min_expected` | `␣Minimum expected = %s` | 1 trou : valeur | `cross_tab.R:1082` | non | câblé |
| `note_expected_advice` | `. Consider %s or set globally via %s.` | 2 trous : `` `simulate_p = TRUE` ``, `` `options(spicy.simulate_p = TRUE)` `` — **code, jamais traduit** | `cross_tab.R:1084` | non | câblé |

### 2.5 `varlist()` / `code_book()`

Les six premières lignes sont des **noms de colonnes d'un tibble public**
documenté par `@returns`. Statut `data` : inscrites pour être verrouillées, pas
pour être traduites. Si l'étage 2 veut les traduire un jour, ce sera au **rendu
Viewer / DT uniquement**, via `display_labels`, jamais sur le tibble retourné.

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `header_variable` | `Variable` | — | `varlist.R:186,281` **+ toutes les familles** (§2.6, §2.7, §2.9) | non | **data** |
| `header_label` | `Label` | — | `varlist.R:187,282` | non | data |
| `label_values` | `Values` | — | `varlist.R:188,283` (cf. §2.4) | non | data |
| `header_class` | `Class` | — | `varlist.R:189,284` | non | data |
| `header_n_distinct` | `N_distinct` | — | `varlist.R:190,285` | non | data |
| `header_n_valid` | `N_valid` | — | `varlist.R:191,286` | non | data |
| `header_nas` | `NAs` | — | `varlist.R:192,287` | non | data |
| `title_varlist` | `vl: %s` | 1 trou : nom de la source | `varlist-title.R:9` | `varlist_title()` | câblé |
| `title_varlist_anonymous` | `vl: <data>` | — (`<data>` est un littéral affiché, pas un trou) | `varlist-title.R:5` | `varlist_title()` | câblé |
| `title_varlist_empty` | `vl: (no columns selected)` | — | `varlist.R:201` (`#nocov`) | **non** — court-circuite `varlist_title()` (§4.7) | câblé |
| `value_summary_matrix` | `Matrix(%s)` | 1 trou : dimensions jointes par `␣x␣` | `varlist-values.R:268` | non | câblé |
| `value_summary_array` | `Array(%s)` | 1 trou : dimensions | `varlist-values.R:268` | non | câblé |
| `value_summary_list` | `List(%d)` | 1 trou : longueur | `varlist-values.R:284` | non | câblé |
| `value_summary_list_types` | `%s: %s` | 2 trous : base, types `typeof()` **non traduits** | `varlist-values.R:288` | non | câblé |
| `value_summary_units` | `␣(%s)` | 1 trou : unité `difftime` (vocabulaire base R, **non traduit**) | `varlist-values.R:113,158` (deux assemblages différents) | non | câblé — §4.7 |
| `value_summary_error` | `<error: %s>` | 1 trou : `conditionMessage()`, **reste anglais** | `varlist-values.R:48` | non | câblé |
| `value_summary_invalid` | `Error: invalid values` | — | `varlist-values.R:148` | non | câblé — §4.7 (graphie incohérente) |

### 2.6 `table_categorical()`

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `title_categorical` | `Categorical table` | — | `table_categorical_print.R:110` (console), `table_categorical.R:1590` (caption tinytable) | **oui — `.categorical_title()`** | câblé |
| `title_categorical_by` | `Categorical table by %s` | 1 trou : nom du `by` | `table_categorical_print.R:112`, `table_categorical.R:2613` | **oui — `.categorical_title()`** | câblé |
| `header_variable` | `Variable` | — | `table_categorical.R:1432,1459,1585,1728,2298,2379,2543,2547,2599,2736,2752,2766` | non | câblé — **verrou** §4.2 |
| `header_n_lower` | `n` | — | `table_categorical.R:1459,1585,1672,1728,2552,2593,2754`, `table_continuous.R:2232`, `regression_render.R:1567` (fit-stat `nobs`) | `fit_stat_label()` (côté régression) | câblé — **verrou** §4.2 |
| `header_pct` | `%` | — (**valeur, pas gabarit** : `spicy_str()` uniquement) | `table_categorical.R:1437,1459,1585,1672,1728,2552,2593,2755` | non | câblé — **verrou** §4.2 |
| `header_p` | `p` | — | `table_categorical.R:2303,2381,2545,2550,2604,2742,2782`, `table_continuous.R:2266`, `regression_render.R:631,689,713,726` | `build_column_spec()` (régression) | câblé — **verrou** §4.2 |
| `header_ci_lower` | `CI lower` | — | `table_categorical.R:2309,2344,2387,3027,3029,3059` | non | câblé — **verrou** §4.2 |
| `header_ci_upper` | `CI upper` | — | `table_categorical.R:2309,2345,2387,3027,3029,3060` | non | câblé — **verrou** §4.2 |
| `header_effect_size` | `Effect size` | — | `table_categorical.R:2215` (mesures hétérogènes) | non | câblé — **verrou** §4.2 (valeur lue par `glance()`) |
| `row_missing_level` | `(Missing)` | — | `table_categorical.R:1102`, `table_continuous.R:1091` | non | câblé — **verrou** §4.3 |
| `row_missing_level_dedup` | `(Missing_%d)` | 1 trou : index | `table_categorical.R:1106`, `table_continuous.R:1098` | non | câblé — couplé à `row_missing_level` |
| `note_missing_removed` | `Missing values removed:␣` | préfixe d'un assemblage (items + `note_missing_rows_total` + `.`) | `table_categorical.R:935`, `table_continuous.R:892`, `cross_tab.R:1151` | **non — candidat n°1** | câblé |
| `note_declared_missing_removed` | `Declared missing values removed:␣` | idem | `table_categorical.R:948`, `table_continuous.R:905`, `cross_tab.R:1182` | non | câblé |
| `note_missing_item` | `%s (%d)` | 2 trous : nom de variable, effectif | `table_categorical.R:937,950`, `table_continuous.R:894,907`, `cross_tab.R:1133,1136,1168,1171` | non | câblé |
| `note_missing_rows_total` | `; %d rows in total` | 1 trou : effectif | `cross_tab.R:1148,1177` | non | câblé |
| `note_rows_missing_by_removed` | `Rows with missing %s removed: %d.` | 2 trous : nom du `by`, effectif | `table_categorical.R:960`, `table_continuous.R:918`, `table_continuous_lm.R:1545`, `cross_tab.R:1195` | non | câblé |
| `note_rows_missing_weights` | `Rows with missing %s removed: %d.` | 2 trous : nom du poids, effectif | `table_continuous_lm.R:1554` | non | câblé — §3 (même texte, sujet différent) |
| `note_weights_fallback` | `weights` | — | `table_continuous_lm.R:1555` (repli quand le nom de la colonne de poids est irrécupérable) | non | câblé |
| `note_prefix` | `Note.␣` | — | `table_categorical.R:286`, `table_continuous_lm_print.R:181`, `regression_titlefooter.R:144` | non | câblé — **verrou** §4.5 |
| `note_assoc_measure_item` | `%s: %s` | 2 trous : libellé de mesure (`stat_*`), variables jointes par `punct_item_sep` | `table_categorical.R:282` | non | câblé |
| `title_excel_sheet_categorical` | `Categorical` | — | `table_categorical.R:750` (défaut d'argument) | non | câblé — §4.6 (31 car. max, `: \ / ? * [ ]` interdits) |
| `col_level_dataframe` | `Level` | — | `table_categorical.R:1433,2299` | non | **data** |
| `col_chi2_dataframe` | `Chi2` | — | `table_categorical.R:2301` | non | **data** |
| `col_df_dataframe` | `df` | — | `table_categorical.R:2302` | non | **data** (collision avec `glance()$df`) |

### 2.7 Mesures d'association — partagées

Trois familles produisent le **même** nom de mesure : l'en-tête de colonne de
`table_categorical()` (via `.assoc_label()`), la note de `cross_tab()` et les
libellés de ligne de `assoc_measures()`. **Une clé par mesure.**

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `stat_cramer_v` | `Cramer's V` | — | `table_categorical.R:12` + repli `:1139`, `cross_tab.R:930`, `assoc.R:1805` | **oui côté catégoriel — `.assoc_label()`** | câblé — **verrou** §4.2 |
| `stat_phi` | `Phi` | — | `table_categorical.R:13`, `cross_tab.R:931`, `assoc.R:1798` | `.assoc_label()` | câblé |
| `stat_tau_b` | `Kendall's Tau-b` | — | `table_categorical.R:14`, `cross_tab.R:933`, `assoc.R:1884` | `.assoc_label()` | câblé |
| `stat_tau_c` | `Stuart's Tau-c` | — | `table_categorical.R:15`, `cross_tab.R:938`, `assoc.R:1895` | `.assoc_label()` | câblé |
| `stat_gamma` | `Goodman-Kruskal Gamma` | — | `table_categorical.R:16`, `cross_tab.R:932`, `assoc.R:1881` | `.assoc_label()` | câblé |
| `stat_somers_d` | `Somers' D` | — | `table_categorical.R:17`, `cross_tab.R:939` | `.assoc_label()` | câblé |
| `stat_lambda` | `Lambda` | — | `table_categorical.R:18`, `cross_tab.R:940` | `.assoc_label()` | câblé |

> Les **variantes directionnelles** de `assoc.R` (`Somers' D R\|C`, `Somers' D
> C\|R`, `Lambda symmetric`, `Lambda R\|C`, `Lambda C\|R`, `Goodman-Kruskal's
> Tau R\|C`, lignes ≈1815-1910) sont des chaînes distinctes, **non recensées
> exhaustivement**. Voir §2.15.
>
> Le repli `switch()` de `.assoc_label()` (`table_categorical.R:19`) renvoie le
> **jeton brut** pour une mesure inconnue. Échappatoire non traduite à
> conserver telle quelle.

### 2.8 `table_continuous()`

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `title_continuous` | `Descriptive statistics` | — | `table_continuous_print.R:140`, `table_continuous.R:2461` | **oui — `.continuous_title()`** | câblé |
| `header_group` | `Group` | — | `table_continuous.R:2192`, gt `:2603,2605`, id de spanner `:2631` | non | câblé — **verrou** §4.2 (l'id gt `spn_Group` doit rester ASCII) |
| `header_mean` | `M` | — | `table_continuous.R:2196` | non | câblé — **verrou** §4.2 |
| `header_sd` | `SD` | — | `table_continuous.R:2198` | non | câblé (fr : « ET » — vraie décision) |
| `header_median` | `Med` | — | `table_continuous.R:2200` **+ préfixe de 5 en-têtes dérivés** | non | câblé — **verrou** §4.5 (`sub("^Med ", …)`) |
| `header_iqr` | `IQR` | — | `table_continuous.R:2202` | non | câblé (couplé à `note_gloss_iqr`) |
| `header_q1` | `Q1` | — | `table_continuous.R:2218` + `header_med_iqr` + `note_gloss_iqr` | non | câblé |
| `header_q3` | `Q3` | — | `table_continuous.R:2220` + idem | non | câblé |
| `header_med_iqr` | `Med [Q1, Q3]` | composite de `header_median`/`_q1`/`_q3` | `table_continuous.R:2216` | non | câblé — §4.7 (séparateur d'en-tête vs de cellule) |
| `header_min` | `Min` | — | `table_continuous.R:2222` | non | câblé |
| `header_max` | `Max` | — | `table_continuous.R:2224` | non | câblé |
| `header_test` | `Test` | — | `table_continuous.R:2247,2394`, `table_continuous_print.R:50` | non | câblé — **verrou** §4.2 |
| `header_effect_size_abbrev` | `ES` | — | `table_continuous.R:2269,2396` | non | câblé (≠ `header_effect_size`, §3) |
| `header_ci_ll_full` | `%s CI LL` | 1 trou : pourcentage | `table_continuous.R:2147,2310,2388`, `table_continuous_lm_render.R:20,150,330` | **non — 6 sites, 2 constructions** | câblé |
| `header_ci_ul_full` | `%s CI UL` | 1 trou : pourcentage | `table_continuous.R:2148,2311,2389`, `table_continuous_lm_render.R:21,151` | non | câblé |
| `header_med_ci_ll_full` | `Med %s CI LL` | 1 trou : pourcentage | `table_continuous.R:2149,2312,2392` | non | câblé |
| `header_med_ci_ul_full` | `Med %s CI UL` | 1 trou : pourcentage | `table_continuous.R:2150,2313,2392` | non | câblé |
| `header_ll` | `LL` | — | `table_continuous.R:2310,2325,2335,2444,2582-2584,2668,2682`, `table_continuous_lm_render.R:396,514,700,829,979,990`, `regression_dispatch.R:241,404,776,1430` | non | câblé — **verrou** §4.2 |
| `header_ul` | `UL` | — | `table_continuous.R:2311,2326,2445,2582`, lm render, `regression_dispatch.R:242,405,777,1434` | non | câblé — **verrou** §4.2 |
| `header_med_ll` | `Med LL` | composite `header_median` + `header_ll` | `table_continuous.R:2312` | non | câblé — **verrou** §4.5 |
| `header_med_ul` | `Med UL` | composite | `table_continuous.R:2313` | non | câblé — **verrou** §4.5 |
| `header_ci_spanner` | `%s%% %s` | 2 trous : pourcentage, libellé d'intervalle | `table_continuous.R:2325`, `table_continuous_lm_render.R:534,987-988`, `regression_render.R:607` | non — **3 helpers concurrents** | câblé (unification, §4.7) |
| `header_med_ci_spanner` | `Med %s CI` | 1 trou : pourcentage | `table_continuous.R:2326` | non | câblé |
| `cell_test_wilcoxon` | `W = %s` | 1 trou : statistique | `table_continuous.R:2083` | non | câblé |
| `cell_test_kruskal` | `H(%s) = %s` | 2 trous : ddl, statistique | `table_continuous.R:2086` | non | câblé |
| `cell_test_t` | `t(%s) = %s` | 2 trous : ddl (fractionnaire possible), statistique | `table_continuous.R:2093` | non | câblé |
| `cell_test_f` | `F(%s, %s) = %s` | 3 trous : ddl1, ddl2, statistique | `table_continuous.R:2101` | non | câblé — §4.7 (séparateur de ddl) |
| `cell_es_assignment` | `%s = %s` | 2 trous : symbole ES, valeur | `table_continuous.R:2121` | non | câblé |
| `note_group_comparison` | `Group comparison: %s.` | 1 trou : label unique **ou** liste jointe | `table_continuous.R:1642` **et** `:1645-1657` (même gabarit, 2 sites) | non | câblé |
| `note_group_comparison_item` | `%s (%s)` | 2 trous : label de test, variables | `table_continuous.R:1651` | non | câblé |
| `test_wilcoxon_rank_sum` | `Wilcoxon rank-sum test` | — | `table_continuous.R:1617` | **oui — `continuous_test_label()`** | câblé |
| `test_kruskal_wallis` | `Kruskal-Wallis test` | — | `table_continuous.R:1619` | `continuous_test_label()` | câblé |
| `test_student_t` | `Student t-test` | — | `table_continuous.R:1621` | `continuous_test_label()` | câblé |
| `test_oneway_anova` | `one-way ANOVA` | — | `table_continuous.R:1621` | `continuous_test_label()` | câblé — §4.4 (casse mi-phrase) |
| `test_welch_t` | `Welch t-test` | — | `table_continuous.R:1622` (branche par défaut) | `continuous_test_label()` | câblé |
| `test_welch_oneway_anova` | `Welch one-way ANOVA` | — | `table_continuous.R:1622` (branche par défaut) | `continuous_test_label()` | câblé — §4.4 |
| `note_gloss_iqr` | `%s = interquartile range (%s - %s).` | 3 trous : `header_iqr`, `header_q3`, `header_q1` | `table_continuous.R:1667` | non | câblé |
| `note_gloss_med_iqr` | `%s = median [first quartile, third quartile].` | 1 trou : `header_med_iqr` | `table_continuous.R:1672` | non | câblé |
| `note_gloss_med_ci` | `Med %1$s CI = exact order-statistic confidence interval for the median (coverage at least %1$s).` | **1 trou répété** → forme positionnelle obligatoire | `table_continuous.R:1677` | non | câblé — §4.4 |
| `note_gloss_med_ci_undefined` | `"%s" where the sample is too small for this level.` | 1 trou : `cell_undefined` | `table_continuous.R:1684` | non | câblé |
| `title_excel_sheet_continuous` | `Descriptives` | — | `table_continuous.R:597` | non | câblé — §4.7 (ne colle pas au titre du tableau) |

### 2.9 `table_continuous_lm()`

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `title_continuous_lm_by` | `Continuous outcomes by %s` | 1 trou : `by_label` | `table_continuous_lm_print.R:98` | **non — à créer** (`.tclm_title()`) | câblé |
| `title_continuous_lm_by_fallback` | `Predictor` | — | `table_continuous_lm_print.R:23` (repli de `attr(x, "by_label")`) | non | câblé |
| `header_lm_mean_level` | `M (%s)` | 1 trou : niveau du `by` | `table_continuous_lm_render.R:38,85,169,211` | **non — 4 reconstructions** | câblé — **verrou** §4.2 |
| `header_lm_delta` | `Δ (%s - %s)` | 2 trous : niveau 2, niveau 1 | `table_continuous_lm_render.R:996` | **oui — `get_delta_label_lm()`** | câblé — **verrou** §4.2 |
| `header_b` | `B` | — | `table_continuous_lm_render.R:48,179,96,238`, `regression_render.R:623` | `build_column_spec()` (régression) | câblé — **verrou** §4.2 |
| `label_weighted_n` | `Weighted n` | — | `table_continuous_lm_render.R:76,203,431,861`, `regression_render.R:1569` (fit-stat) | `fit_stat_label()` (régression) | câblé — **verrou** §4.2 |
| `header_lm_test_chi2_df` | `χ²(%s)` | 1 trou : ddl | `table_continuous_lm_render.R:1057` | **oui — `get_test_header_lm()`** | câblé — **verrou** §4.2 |
| `header_lm_test_t_df` | `t(%s)` | 1 trou : ddl (fractionnaire sous CR*) | `table_continuous_lm_render.R:1063` | `get_test_header_lm()` | câblé |
| `header_lm_test_f_df` | `F(%s, %s)` | 2 trous : ddl1, ddl2 | `table_continuous_lm_render.R:1071` | `get_test_header_lm()` | câblé |
| `header_lm_test_f` | `F` | — | `table_continuous_lm_render.R:1079` | `get_test_header_lm()` | gelé |
| `header_lm_adj_r2` | `Adj. R²` | composite : abréviation + `symbol_r2` | `table_continuous_lm_render.R:1104` | **oui — `format_r2_header_lm()`** | câblé (seul mot anglais des en-têtes lm) |
| `note_adjusted_for` | `Adjusted for %s (%s).` | 2 trous : covariables jointes, **libellé** d'estimand | `table_continuous_lm_print.R:153` | `.tclm_note_text()` | câblé — §4.4 |
| `note_adjustment_proportional` | `proportional` | — | valeur du jeton `adjustment` (`table_continuous_lm.R:997`) insérée verbatim | non | câblé — §3 (exception à « pas de valeurs de jeton ») |
| `note_adjustment_balanced` | `balanced` | — | idem | non | câblé — §3 |
| `note_std_errors_single` | `Std. errors: %s.` | 1 trou : label vcov | `table_continuous_lm_print.R:165`, `regression_titlefooter.R:317` | non — **deux familles** | câblé |
| `note_std_errors_multi` | `Std. errors:\n%s` | 1 trou : lignes indentées par modèle | `regression_titlefooter.R:326` | non | câblé |
| `note_vcov_hc` | `heteroskedasticity-robust (%s)` | 1 trou : jeton HC* **non traduit** | `table_continuous_lm_print.R:197`, `regression_titlefooter.R:265` | `.tclm_vcov_label()` (lm) | câblé |
| `note_vcov_cr` | `cluster-robust (%s), %s` | 2 trous : jeton CR*, fragment cluster | `regression_titlefooter.R:281` ; forme **sans** fragment `table_continuous_lm_print.R:200` | `.tclm_vcov_label()` | câblé — §4.7 |
| `note_vcov_cluster_by` | `, clusters by %s` | 1 trou : nom de colonne | `table_continuous_lm_print.R:207` | `.tclm_vcov_label()` | câblé — §4.4 (fragment recollé) |
| `note_vcov_bootstrap_reps` | `␣(%d replicates)` | 1 trou : compte | `table_continuous_lm_print.R:215`, `regression_titlefooter.R:293` | non | câblé — §4.4 (pas de singulier) |
| `note_vcov_bootstrap_reps_range` | `␣(%d-%d replicates)` | 2 trous : min, max | `table_continuous_lm_print.R:217` | non | câblé |
| `note_vcov_jackknife_plain` | `jackknife` | — | `table_continuous_lm_print.R:222` | `.tclm_vcov_label()` | câblé (≠ `note_vcov_jackknife`, §3) |
| `title_excel_sheet_continuous_lm` | `Linear models` | — | `table_continuous_lm.R:1048` | non | câblé |

### 2.10 `table_regression()` — en-têtes de colonnes

Source quasi unique : `build_column_spec()` (`regression_render.R:571`).

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `header_events_n` | `Events/N` | — | `regression_render.R:621` | `build_column_spec()` | câblé |
| `header_se` | `SE` | — | `regression_render.R:624,680,703,720` (**4 sites**) | `build_column_spec()` | câblé — **verrou** §4.1 |
| `header_ci_label_confidence` | `CI` | — | `regression_render.R:576` (défaut d'argument), `table_regression.R:3024` (résolution) | non — **2 sites à synchroniser** | câblé |
| `header_ci_label_credible` | `CrI` | — | `table_regression.R:3017` | non | gelé |
| `header_ci_label_hdi` | `HDI` | — | `table_regression.R:3022` | non | gelé |
| `header_pd` | `pd` | — | `regression_render.R:634` | `build_column_spec()` | gelé (glosé par `note_abbrev_pd`) |
| `header_rhat` | `R-hat` | — | `regression_render.R:637` | `build_column_spec()` | gelé |
| `header_ess_bulk` | `ESS (bulk)` | — | `regression_render.R:641` | `build_column_spec()` | câblé |
| `header_ess_tail` | `ESS (tail)` | — | `regression_render.R:646` | `build_column_spec()` | câblé |
| `header_mcse` | `MCSE` | — | `regression_render.R:650` | `build_column_spec()` | gelé |
| `header_rmst` | `dRMST (%s)` | 1 trou : horizon τ formaté | `regression_render.R:675` | `build_column_spec()` | câblé |
| `header_rmst_no_horizon` | `dRMST` | — | `regression_render.R:677` (`#nocov`) | `build_column_spec()` | câblé |
| `header_risk_diff` | `dRisk (%s)` | 1 trou : `at_time` | `regression_render.R:695` | `build_column_spec()` | câblé |
| `header_risk_diff_no_horizon` | `dRisk` | — | `regression_render.R:697` (`#nocov`) | `build_column_spec()` | câblé |
| `header_ame` | `AME` | — | `regression_render.R:718` | `build_column_spec()` | gelé |
| `header_with_ci_suffix` | `%s %s` | 2 trous : en-tête d'effet, en-tête d'IC | `regression_render.R:751,761,771` (f², η², ω²) | non | câblé |
| `header_model_prefixed` | `%s: %s` | 2 trous : label de modèle, en-tête court | `regression_render.R:840` | non | câblé |
| `header_ame_by_category` | `%s %s` | 2 trous : `header_ame`, catégorie de réponse (**donnée**) | `regression_render.R:833` | non | câblé |
| `header_exp_or` | `OR` | — | `glm_compute.R:75,93,99,102,138` (5 sites) | **oui — `spicy_glm_exp_header()`** | câblé — **verrou** §4.2 |
| `header_exp_irr` | `IRR` | — | `glm_compute.R:78,105,112,130,133,141` (6 sites) | `spicy_glm_exp_header()` | câblé |
| `header_exp_hr` | `HR` | — | `glm_compute.R:81,90,96` | `spicy_glm_exp_header()` | câblé |
| `header_exp_rr` | `RR` | — | `glm_compute.R:84` | `spicy_glm_exp_header()` | câblé |
| `header_exp_mr` | `MR` | — | `glm_compute.R:87,115` | `spicy_glm_exp_header()` | câblé |
| `header_exp_generic` | `exp(B)` | — | `glm_compute.R:143` | `spicy_glm_exp_header()` | câblé — **verrou** §4.2 (clé de `exp_defs`) |
| `header_companion_qualified` | `%s (%s)` | 2 trous : en-tête compagnon, en-tête porteur | `tables_ascii.R:894` | non | câblé — **verrou** §4.1 |

### 2.11 `table_regression()` — lignes, blocs, cellules

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `row_intercept` | `(Intercept)` | — | `regression_render.R:1231` (via `resolve_label()`) ; **nom R du coefficient** dans `regression_align.R:135`, `regression_titlefooter.R:1999-2000`, `regression_uv.R:828,840`, `regression_frame_*` | `resolve_label()` | câblé — **verrou** §4.3 |
| `row_reference_suffix` | `(ref.)` | — | `regression_render.R:65` (défaut), `table_regression.R:1432` (signature publique) | non — **2 sites** | câblé |
| `row_reference_annotation_flat` | `␣[vs %s]` | 1 trou : niveau de référence | `regression_render.R:351`, `regression_structured.R:442` | non — **dupliqué** | câblé |
| `row_factor_header_reference` | `␣[ref: %s]` | 1 trou : niveau | `regression_render.R:1746`, `regression_structured.R:1229` | non — dupliqué | câblé |
| `row_factor_header_colon` | `%s:` | 1 trou : nom de variable | `regression_render.R:1737`, `regression_structured.R:1222` | non — dupliqué | câblé |
| `row_outcome` | `Outcome` | — | `regression_render.R:1350`, `regression_structured.R:1207` | non — dupliqué | câblé |
| `row_block_fixed_effects` | `Fixed effects:` | — (**colon inclus dans le littéral**, §4.7) | `regression_render.R:1435`, `regression_structured.R:941` | non | câblé |
| `row_n_groups` | `N (%s)` | 1 trou : nom du facteur de groupement | `regression_render.R:1467`, `regression_structured.R:989` | non — dupliqué | câblé |
| `cell_fixed_effect_yes` | `Yes` | — | `regression_render.R:1557`, `regression_structured.R:1332` | non | câblé |
| `cell_fixed_effect_no` | `No` | — | idem | non | câblé |
| `label_block_thresholds` | `Thresholds` | — | `regression_render.R:281,1264`, `regression_align.R:244,419`, `regression_frame_ordinal.R:953` (`parent_var`), `regression_titlefooter.R:784,915` | non | câblé — **verrou §4.3 SÉVÈRE** |
| `label_block_nonproportional` | `Non-proportional effects` | — | `regression_render.R:282,1265`, `regression_align.R:420`, `regression_frame_ordinal.R:771` | non | câblé — **verrou §4.3** |
| `label_block_scale_effects` | `Scale effects` | — | `regression_render.R:283,1266`, `regression_align.R:243,421`, `regression_frame_ordinal.R:709`, `regression_titlefooter.R:2256` (`%in%`) | non | câblé — **verrou §4.3** |
| `label_block_random_effects` | `Random effects` | — | `regression_render.R:284,1267`, `regression_align.R:245,254,422`, `regression_titlefooter.R:1482,1577` | non | câblé — **verrou §4.3** |
| `label_block_zero_inflation` | `Zero-inflation` | — | `regression_render.R:285,1268`, `regression_align.R:240,423`, `regression_frame_glmmTMB.R:317`, `regression_frame_pscl.R:492` | non | câblé — **verrou §4.3** |
| `label_block_zero_hurdle` | `Zero hurdle` | — | `regression_render.R:286,1269`, `regression_align.R:241,424`, `regression_frame_pscl.R:492` | non | câblé — **verrou §4.3** |
| `label_block_dispersion` | `Dispersion` | — | `regression_render.R:287,1270`, `regression_align.R:242,425`, `regression_frame_glmmTMB.R:339` | non | câblé — **verrou §4.3** |
| `row_re_residual` | `σ (Residual)` | — | `regression_titlefooter.R:1536` | **oui — `.re_panel_label()`** | câblé — **verrou** §4.5 |
| `row_re_correlation` | `ρ %s (%s)` | 2 trous : groupe, terme | `regression_titlefooter.R:1539` | `.re_panel_label()` | câblé |
| `row_re_sd_intercept` | `σ %s (Intercept)` | 1 trou : groupe | `regression_titlefooter.R:1542` | `.re_panel_label()` | câblé (le `(Intercept)` doit suivre `row_intercept`) |
| `row_re_sd_slope` | `σ %s %s` | 2 trous : groupe, terme | `regression_titlefooter.R:1544` | `.re_panel_label()` | câblé |
| `row_re_variance_prefix` | `σ²` | réécriture `sub("^σ ", "σ² ", label)` | `regression_titlefooter.R:1475` | non | câblé — **verrou §4.5, à refactorer** |
| `label_model_default` | `Model %d` | 1 trou : index | `regression_render.R:145`, `regression_align.R:483`, `table_regression.R:1969,2636,2669,2996` | **non — 6 constructions** | câblé |
| `label_group_univariable` | `Univariable` | — | `regression_uv.R:597` (posé comme `names(models)`) | non | câblé — **verrou** §4.2 |
| `label_group_multivariable` | `Multivariable` | — | `regression_uv.R:611` | non | câblé — **verrou** §4.2 |

### 2.12 `table_regression()` — libellés de fit-stats

Source unique : `fit_stat_label()` (`regression_render.R:1564`). Les entrées
`R²` / `Adj.R²` alimentent **aussi** des en-têtes de colonnes
(`regression_render.R:735,740`) : une clé, deux rôles.

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `fitstat_n_events` | `N events` | — | `regression_render.R:1568` | `fit_stat_label()` | câblé |
| `fitstat_r2` | `R²` | — | `regression_render.R:1570` + en-tête `:735` | `fit_stat_label()` | gelé |
| `fitstat_adj_r2` | `Adj.R²` | — | `regression_render.R:1571` + en-tête `:740` | `fit_stat_label()` | câblé (« Adj. » traduisible) |
| `fitstat_pseudo_r2` | `R² (%s)` | 1 trou : **nom propre non traduit** (McFadden / Nagelkerke / Tjur / Bayes) | `regression_render.R:1573-1575,1584` | `fit_stat_label()` | câblé (gabarit unifié, §4.7) |
| `fitstat_r2_qualified` | `R² (%s)` | 1 trou : **qualificatif traduisible** (within / marginal / conditional) | `regression_render.R:1578,1588,1589` | `fit_stat_label()` | câblé |
| `label_r2_within` | `within` | — | `regression_render.R:1578` | — | câblé |
| `label_r2_marginal` | `marginal` | — | `regression_render.R:1588` | — | câblé |
| `label_r2_conditional` | `conditional` | — | `regression_render.R:1589` | — | câblé |
| `fitstat_theta` | `θ (dispersion)` | — | `regression_render.R:1576` | `fit_stat_label()` | câblé |
| `fitstat_alpha` | `α (= 1/θ)` | — | `regression_render.R:1577` | `fit_stat_label()` | gelé (formule) |
| `fitstat_phi` | `φ (precision)` | — | `regression_render.R:1579` | `fit_stat_label()` | câblé |
| `fitstat_qic` | `QIC` | — | `regression_render.R:1580` | `fit_stat_label()` | gelé |
| `fitstat_qicu` | `QICu` | — | `regression_render.R:1581` | `fit_stat_label()` | gelé |
| `fitstat_scale` | `Scale` | — | `regression_render.R:1582` | `fit_stat_label()` | câblé (≠ `label_block_scale_effects`, §3) |
| `fitstat_max_cluster_size` | `Max cluster size` | — | `regression_render.R:1583` | `fit_stat_label()` | câblé |
| `fitstat_elpd_loo` | `ELPD (LOO)` | — | `regression_render.R:1585` | `fit_stat_label()` | gelé |
| `fitstat_looic` | `LOOIC` | — | `regression_render.R:1586` | `fit_stat_label()` | gelé |
| `fitstat_waic` | `WAIC` | — | `regression_render.R:1587` | `fit_stat_label()` | gelé |
| `fitstat_icc` | `ICC` | — | `regression_render.R:1590` | `fit_stat_label()` | gelé |
| `fitstat_rmse` | `RMSE` | — | `regression_render.R:1595` | `fit_stat_label()` | gelé |
| `fitstat_aic` | `AIC` | — | `regression_render.R:1597` | `fit_stat_label()` | gelé |
| `fitstat_aicc` | `AICc` | — | `regression_render.R:1598` | `fit_stat_label()` | gelé |
| `fitstat_bic` | `BIC` | — | `regression_render.R:1599` | `fit_stat_label()` | gelé |
| `fitstat_deviance` | `Deviance` | — | `regression_render.R:1600` | `fit_stat_label()` | câblé |
| `fitstat_change_prefix` | `Δ%s` | 1 trou : libellé de base | `regression_render.R:1602,1603,1605,1606,1607,1608,1609,1610` (8 littéraux à unifier) | `fit_stat_label()` | câblé — §4.7 |
| `fitstat_f_change` | `F-change` | — | `regression_render.R:1604` | `fit_stat_label()` | câblé — §4.7 (seule exception au préfixe Δ) |
| `fitstat_p_change` | `p (change)` | — | `regression_render.R:1611` | `fit_stat_label()` | câblé |

> `n_groups` n'a **pas** d'entrée dans `fit_stat_label()` : son libellé est le
> gabarit `row_n_groups` (§2.11). Commentaire explicite en
> `regression_render.R:1591-1593` — ne pas « corriger » cette absence.

### 2.13 `table_regression()` — titres

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper source unique | Statut |
| --- | --- | --- | --- | --- | --- |
| `title_prefix_generic` | `Regression` | — | `regression_titlefooter.R:14,29,36,177`, `glm_compute.R:63` (repli) | `spicy_glm_title_prefix()` (partiel) | câblé |
| `title_prefix_linear` | `Linear regression` | — | `regression_extract.R:231`, `regression_frame_lm.R:328` | non | câblé |
| `title_single_model` | `%s: %s` | 2 trous : préfixe, variable réponse | `regression_titlefooter.R:43` | non | câblé |
| `title_nested` | `Hierarchical %s: %s` | 2 trous | `regression_titlefooter.R:51` | non | câblé — **verrou** §4.5 (`lowercase_first()`) |
| `title_multi_same_dv` | `%s comparison: %s` | 2 trous | `regression_titlefooter.R:54` | non | câblé (ordre inversé en fr) |
| `title_multi_diff_dv` | `%s comparison` | 1 trou | `regression_titlefooter.R:56` | non | câblé |
| `title_uv_and_mv` | `Univariable and multivariable %s regression: %s` | 2 trous : type, variable réponse | `regression_uv.R:652` | non | câblé |
| `title_uv_screen` | `Univariable %s regression screen: %s` | 2 trous | `regression_uv.R:657` | non | câblé |
| `title_uv_type_linear` | `linear` | — | `regression_uv.R:638` | non | câblé |
| `title_uv_type_cox` | `Cox` | — | `regression_uv.R:640` | non | gelé (nom propre) |
| `title_uv_type_logistic` | `logistic` | — | `regression_uv.R:644` | non | câblé |
| `title_uv_type_probit` | `probit` | — | `regression_uv.R:645` | non | câblé |
| `title_uv_type_poisson` | `Poisson` | — | `regression_uv.R:646` | non | gelé (nom propre) |
| `title_excel_sheet_default` | `Regression` | — | `regression_dispatch.R:40`, `table_regression.R:1467` | non — **2 sites** | câblé — §4.6 |
| `title_word_caption_prefix` | `Table␣` | — | `regression_dispatch.R:2629` (`officer::run_autonum`) | non | câblé (fr : `Tableau␣`) |
| `title_word_caption_post` | `:␣` | — | `regression_dispatch.R:2630` (`post_label`) | non | câblé — solidaire du précédent |

**Préfixes de titre par classe** — `spicy_glm_title_prefix()`
(`glm_compute.R:32-64`), 11 chaînes : `Logistic regression`,
`Probit regression`, `Complementary log-log regression`,
`Log-binomial regression`, `Binomial regression`, `Poisson regression`,
`Gamma regression`, `Inverse-Gaussian regression`, `Quasi-binomial regression`,
`Quasi-Poisson regression`, `Quasi-likelihood regression`.
→ clés `title_prefix_<famille>_<lien>`, toutes `câblé`, source unique déjà en
place. **Les ~38 autres `title_prefix` des `regression_frame_*.R` sont une
lacune de recensement : voir §2.15.**

### 2.14 `table_regression()` — notes de bas de tableau

Le plus gros bloc du registre (≈ 110 entrées). Toutes proviennent de
`regression_titlefooter.R` sauf mention contraire.

**Préfixe et structure**

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper | Statut |
| --- | --- | --- | --- | --- | --- |
| `note_prefix` | `Note.␣` | — | `:144` (+ §2.6) | non | câblé — **verrou §4.5 CRITIQUE** |
| `note_model_prefix` | `Model %d: %s` | 2 trous : index, texte | `:190,322,823,862,952,1124,1172,1626,1734,1771,1814,2318,2354`, `regression_uv.R:951`, `regression_survival_estimands.R:843` (**≈15 sites**) | non | câblé |
| `note_model_prefix_indented` | `␣␣Model %d: %s` | 2 trous ; **2 espaces de tête significatifs** | `:322` (bloc Std. errors) | non | câblé |
| `note_stars_legend_entry` | `%s p < %s` | 2 trous : symbole, seuil formaté APA | `:726` | non | câblé |
| `note_model_type_single` | `%s.` | 1 trou : préfixe capitalisé | `:182` | non | câblé — **verrou §4.5** (`capitalize_first()`) |
| `note_model_type_all_same` | `%s models.` | 1 trou ; **pluriel anglais accolé** | `:185` | non | câblé — §4.4 |

**Erreurs types (vcov)**

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper | Statut |
| --- | --- | --- | --- | --- | --- |
| `note_vcov_classical_glm` | `classical (Fisher information)` | — | `:262` | non | câblé |
| `note_vcov_classical_lm` | `classical (OLS)` | — | `:262` | non | câblé (OLS → MCO ? décision fr) |
| `note_vcov_cluster_vector` | `cluster vector supplied` | — | `:269` | non | câblé |
| `note_vcov_cluster_named` | `clusters by %s` | 1 trou : nom de colonne | `:271` (+ recopié en dur `:298,304`) | non | câblé — §4.7 |
| `note_vcov_cr1s` | `cluster-robust (CR1S, Stata vce(cluster), t(G-1)), %s` | 1 trou | `:277` | non | câblé (référence Stata préservée) |
| `note_vcov_bootstrap` | `nonparametric bootstrap%s` | 1 trou : fragment réplicats | `:296` ; forme nue `table_continuous_lm_print.R:219` | non | câblé |
| `note_vcov_bootstrap_cluster` | `cluster bootstrap%s, clusters by %s` | 2 trous | `:298` | non | câblé |
| `note_vcov_jackknife` | `jackknife (leave-one-out)` | — | `:302` | non | câblé |
| `note_vcov_jackknife_cluster` | `jackknife (leave-one-cluster-out), clusters by %s` | 1 trou | `:304` | non | câblé |
| `note_vcov_wald_asymptotic` | `Wald asymptotic (z)` | — | **12 frames** : `regression_frame_survival.R:535,625`, `_ordinal.R:472,877`, `_multinom.R:396`, `_pscl.R:413`, `_glmmTMB.R:378`, `_MASS.R:273`, `_mlogit_betareg.R:288,580`, `_flexsurv_selection.R:335,602` | **non — 12 duplications** | câblé |

**Intervalles**

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Helper | Statut |
| --- | --- | --- | --- | --- | --- |
| `note_ci_profile` | `%s%% CIs: profile likelihood.` | 1 trou | `:361` | non | câblé |
| `note_ci_bootstrap_percentile` | `%s%% CIs: bootstrap percentile.` | 1 trou | `:377` | non | câblé |
| `note_ci_posterior_mixed` | `Model %d: %s%% CI is an equal-tailed posterior credible interval.` | 2 trous | `:406-412` (chaîne coupée en 2 morceaux) | non | câblé |

**Abréviations** (jointes par `;␣`, closes par `.` — `:570`)

| Clé | Texte verbatim | Consommateurs | Statut |
| --- | --- | --- | --- |
| `note_abbrev_ame` | `AME = average marginal effect` | `:459` | câblé |
| `note_abbrev_ame_percat` | `AME = average marginal effect on a response-category probability` | `:456` | câblé |
| `note_abbrev_beta` | `β = standardised coefficient` | `:496` | câblé |
| `note_abbrev_beta_method` | `β = standardised coefficient (%s)` — 1 trou | `:498` | câblé |
| `note_standardized_gloss_refit` | `"refit": outcome and numeric predictors z-scored, factor dummies on 0/1` | `:471-474` | câblé |
| `note_standardized_gloss_posthoc` | `"posthoc": B × SD(X)/SD(Y) for numeric predictors, B/SD(Y) for factor dummies` | `:475-478` | câblé |
| `note_standardized_gloss_basic` | `"basic": every design column scaled by its SD, dummies included (SPSS / Stata Beta)` | `:479-482` | câblé |
| `note_standardized_gloss_smart` | `"smart": numeric predictors ÷ 2 SD (Gelman 2008), binaries and dummies on 0/1` | `:483-486` | câblé |
| `note_standardized_gloss_pseudo` | `"pseudo": latent-scale SD(X)/SD(Y*) for numeric predictors, 1/SD(Y*) for factor dummies` | `:487-490` | câblé |
| `note_abbrev_or` | `OR = odds ratio` | `:516` | câblé — **verrou** §4.2 |
| `note_abbrev_irr` | `IRR = incidence rate ratio` | `:517` | câblé |
| `note_abbrev_hr` | `HR = hazard ratio` | `:518` | câblé |
| `note_abbrev_rr` | `RR = risk ratio` | `:519` | câblé |
| `note_abbrev_mr` | `MR = mean ratio` | `:520` | câblé |
| `note_abbrev_expb` | `exp(B) = exponentiated coefficient` | `:521` | câblé |
| `note_abbrev_f2` | `f² = Cohen's partial f²` | `:530` | câblé |
| `note_abbrev_eta2` | `η² = partial eta-squared` | `:533` | câblé |
| `note_abbrev_omega2` | `ω² = bias-corrected partial omega-squared` | `:536` | câblé |
| `note_abbrev_chi2` | `χ² = partial likelihood-ratio chi-squared` | `:539` | câblé |
| `note_abbrev_pd` | `pd = probability of direction (share of the posterior on the dominant side of zero; Makowski et al. 2019)` | `:547-551` | câblé (citation conservée : BARG) |
| `note_abbrev_mcse` | `MCSE = Monte Carlo standard error of the posterior median (Vehtari et al. 2021)` | `:560-563` | câblé |

**AME, standardisation, seuils**

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Statut |
| --- | --- | --- | --- | --- |
| `note_ame_satterthwaite` | `AME inference: t-test with Satterthwaite df%s.` | 1 trou | `:620` | câblé |
| `note_ame_satterthwaite_qual_both` | `␣(closed-form for lm; dominant-coefficient approximation for glm)` | — | `:614` | câblé |
| `note_ame_satterthwaite_qual_glm` | `␣(dominant-coefficient approximation)` | — | `:616` | câblé |
| `note_standardized_caveat_algebraic` | `Standardised β: interaction / transformed terms are scaled by the SD of the product (or transformed) design column; differs from "refit" when components are correlated.` | — | `:679-683` | câblé — **verrou §4.5** |
| `note_standardized_caveat_refit` | `Standardised β: after refit on z-scored data, an interaction's β is the coefficient of the product of the z-scored components.` | — | `:686-689` | câblé |
| `note_standardized_caveat_fallback` | `Standardised β: "refit" failed; algebraic (posthoc) scaling applied. %s` | 1 trou : **reste du caveat amputé par regex** | `:692-696` | câblé — **verrou §4.5, à refactorer** |
| `note_thresholds_rows_gloss` | `Thresholds: latent-scale category cut-points` | — | `:784` | câblé |
| `note_thresholds_scale_suffix` | `␣(%s, not exponentiated)` | 1 trou : libellé d'échelle | `:801` | câblé |
| `note_scale_log_odds` | `log-odds scale` | — | `:795` | câblé |
| `note_scale_log_cumhazard` | `log-cumulative-hazard scale` | — | `:797` | câblé |
| `note_scale_link` | `link scale` | — | `:799` | câblé |
| `note_thresholds_compact` | `Thresholds: %s.` | 1 trou : paires jointes par `,␣` | `:915` | câblé |

**GEE, survie, distributions**

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Statut |
| --- | --- | --- | --- | --- |
| `note_gee_corstr_alpha` | `GEE working correlation: %s (alpha = %s).` | 2 trous | `:883` | câblé |
| `note_gee_corstr_params` | `GEE working correlation: %s (%d correlation parameters).` | 2 trous | `:889` | câblé |
| `note_gee_corstr_plain` | `GEE working correlation: %s.` | 1 trou | `:895` | câblé |
| `note_survival_concordance_se` | `Concordance C = %.2f (SE = %.2f)` | 2 trous | `:987` | câblé |
| `note_survival_concordance` | `Concordance C = %.2f` | 1 trou | `:990` (`#nocov`) | câblé |
| `note_survival_distribution` | `Distribution: %s` | 1 trou | `:1007,1024` | câblé |
| `note_survival_scale` | `scale = %.2f` | 1 trou | `:1010` | câblé |
| `note_survival_aux_param` | `%s = %.2f` | 2 trous (nom flexsurv non traduit) | `:1027` | câblé |
| `note_dist_weibull` | `Weibull` | — | `:1040` | gelé |
| `note_dist_weibull_ph` | `Weibull (PH)` | — | `:1041` | gelé |
| `note_dist_lognormal` | `Log-normal` | — | `:1042-1043`, `regression_frame_survival.R:639` | câblé |
| `note_dist_gompertz` | `Gompertz` | — | `:1044` | gelé |
| `note_dist_gamma` | `Gamma` | — | `:1045` | gelé |
| `note_dist_exponential` | `Exponential` | — | `:1046-1047` | câblé |
| `note_dist_loglogistic` | `Log-logistic` | — | `:1048-1049` | câblé |
| `note_dist_gengamma` | `Generalised gamma` | — | `:1050` | câblé |
| `note_dist_genf` | `Generalised F` | — | `:1051` | câblé |
| `note_dist_gaussian` | `Gaussian` | — | `:1052` | câblé |
| `note_dist_logistic` | `Logistic` | — | `regression_frame_survival.R:643` **seulement** | câblé — §4.7 |
| `note_dist_student_t` | `Student-t` | — | `regression_frame_survival.R:644` **seulement** | câblé — §4.7 |

**Effets aléatoires**

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Statut |
| --- | --- | --- | --- | --- |
| `note_re_header_method` | `Random effects (%s)` | 1 trou : REML / ML | `:1575` | câblé |
| `note_re_header` | `Random effects` | — | `:1577` | câblé (même texte que `label_block_random_effects`, §3) |
| `note_re_null_lrt` | `LR test vs %s, χ̄²(%d) = %.2f, p %s` | 4 trous | `:1585` | câblé (encodage composé §2.2) |
| `note_re_null_model_fallback` | `no-random model` | — | `:1586` | câblé |
| `note_re_null_model_linear` | `linear regression` | — | `regression_frame.R:934,1050,1131` (**3 sites**) | câblé (minuscule mi-phrase, ≠ `title_prefix_linear`) |
| `note_re_null_model_family` | `%s regression` | 1 trou : famille | `regression_frame.R:937,1053` | câblé (ordre inversé en fr) |
| `note_p_panel_na` | `= NA` | — | `:1524` | gelé |
| `note_re_test_lrt` | `Random-effect p-values: LR test vs the reduced random structure, chi-bar-squared reference.` | — | `:1105-1108` | câblé |
| `note_re_test_rlrt` | `Random-effect p-value: exact restricted LRT (simulated null distribution).` | — | `:1109-1112` | câblé — §4.7 (nombre incohérent) |
| `note_re_profile_ci` | `Random-effect variance components: profile likelihood CIs; no SE (asymmetric intervals).` | — | `:1685-1688` | câblé |
| `note_re_se_skipped` | `Random-effect variance components: SE and CI not computed (n = %s exceeds the spicy.re_se_max_n cap).` | 1 trou | `:1712-1715` | câblé — §4.4 (`big.mark` en dur) |
| `note_singular_mixed` | `Singular fit: random-effect variance component(s) estimated at the boundary (0); their Wald SE and CI are omitted.` | — | `:1650-1653` | câblé — §4.4 (`(s)`) |
| `note_singular_rank_deficient` | `Rank-deficient model: dropped coefficient(s) shown as –.` | — | `:1655` | câblé — §2.3 |

**Inférence des modèles mixtes**

| Clé | Texte verbatim | Consommateurs | Statut |
| --- | --- | --- | --- |
| `note_mixed_inference_cr_satterthwaite` | `p-values: Satterthwaite t-test, cluster-robust df (clubSandwich).` | `:1197-1200` | câblé |
| `note_mixed_inference_satterthwaite` | `p-values: Satterthwaite t-test (lmerTest).` | `:1203` | câblé |
| `note_mixed_inference_wald_lmer` | `p-values: Wald-z, large-sample approximation. Load \`lmerTest\` for Satterthwaite t-tests.` | `:1207-1210` | câblé — §4.7 (**seul conseil d'action**) |
| `note_mixed_inference_wald_asymptotic` | `p-values: Wald-z asymptotic (%s).` — 1 trou : nom de paquet | `:1211` (lme4), `:1212` (glmmTMB) | câblé (unification, §4.7) |
| `note_mixed_inference_containment` | `p-values: t-test with containment df (nlme).` | `:1213` | câblé |

**Blocs de composantes**

| Clé | Texte verbatim | Consommateurs | Statut |
| --- | --- | --- | --- |
| `note_component_exp_or` | `␣Coefficients exponentiated and displayed as odds ratios.` | `:1310-1312` | câblé |
| `note_component_not_exp` | `␣Left on the link scale (not exponentiated).` | `:1314` | câblé |
| `note_component_robust_scope` | `Robust SEs apply to the conditional component; zero-inflation / dispersion SEs are model-based.` | `:1328-1331` | câblé |
| `note_component_gloss_zero_inflation` | `Zero-inflation component: log-odds of a structural (excess) zero.` | `regression_frame_glmmTMB.R:320`, `regression_frame_pscl.R:547` | câblé |
| `note_component_gloss_dispersion` | `Dispersion component: log scale.` | `regression_frame_glmmTMB.R:342` | câblé |
| `note_component_gloss_hurdle_binomial` | `Zero hurdle component: log-odds of a nonzero count.` | `regression_frame_pscl.R:539` | câblé |
| `note_component_gloss_hurdle_count` | `Zero hurdle component: right-censored %s on the log scale.` — 1 trou | `regression_frame_pscl.R:541-543` | câblé |

**Exponentiation** (bloc le plus complexe du paquet)

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Statut |
| --- | --- | --- | --- | --- |
| `note_exp_scope_all` | `Coefficients` | — | `:1859` | câblé |
| `note_exp_scope_partial` | `%s: coefficients` | 1 trou : `Model %d` joints | `:1861-1864` | câblé |
| `note_exp_with_se_single` | `%s exponentiated and displayed as %s; SE on the %s scale (%s); %s (asymmetric).%s` | **6 trous** | `:1925-1928` | câblé — traduire d'un bloc |
| `note_exp_with_se_multi` | `%s exponentiated and displayed as %s (per family); SE on the displayed ratio scale (%s); %s (asymmetric).%s` | 5 trous | `:1938-1941` | câblé |
| `note_exp_no_se_single` | `%s exponentiated and displayed as %s; %s.%s` | 4 trous | `:1952` | câblé |
| `note_exp_no_se_multi` | `%s exponentiated and displayed as %s (per family); %s.%s` | 4 trous | `:1960` | câblé |
| `note_exp_se_gloss_bayes` | `posterior MAD SD of the exponentiated draws` | — | `:1881` | câblé (graphie « MAD SD » figée) |
| `note_exp_se_gloss_mixed` | `delta method; posterior MAD SD of the exponentiated draws for the Bayesian model(s)` | — | `:1883-1886` | câblé |
| `note_exp_se_gloss_delta` | `delta method` | — | `:1888` | câblé |
| `note_exp_ci_gloss_hdi` | `CI: highest-density interval of the exponentiated draws` | — | `:1898` | câblé |
| `note_exp_ci_gloss_standard` | `CI bounds exponentiated` | — | `:1900` | câblé |
| `note_exp_hr_negated` | `␣HR is the grouped-time proportional-hazards ratio exp(-B): the cumulative parametrisation cloglog P(Y <= j) = zeta_j - xB places the hazard on -B (Prentice & Gloeckler 1978; McCullagh 1980).` | — | `:1912-1917` | câblé |

**Multiplicité, références, polynômes**

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Statut |
| --- | --- | --- | --- | --- |
| `note_p_adjust` | `P-values adjusted via stats::p.adjust(method = %s); %s.` | 2 trous | `:2015-2018` | câblé |
| `note_p_adjust_family_size_same` | `m = %d coefficient(s) per model` | 1 trou | `:2008` | câblé — §4.4 |
| `note_p_adjust_family_size_varies` | `m = (%s) coefficient(s) per model` | 1 trou | `:2010` | câblé |
| `note_reference_alternative` | `Reference alternative: %s.` | 1 trou | `:1761` | câblé |
| `note_reference_outcome` | `Reference outcome: %s.` | 1 trou | `:1800` | câblé |
| `note_reference_categories` | `Reference categories: %s.` | 1 trou | `:2216` | câblé |
| `note_kv_pair` | `%s = %s` | 2 trous | `:2104` (légende poly), `:2210` (catégories de référence) | câblé — §3 |
| `note_poly_prefix_plural` | `Ordered factors␣` | — | `:2111` | câblé — §4.4 |
| `note_poly_prefix_singular` | `Ordered factor␣` | — | `:2111` | câblé — §4.4 |
| `note_poly_suffix` | `: polynomial trends (%s).` | 1 trou : légende | `:2113-2115` | câblé |
| `note_poly_degree_linear` | `linear` | — | `:2088` | câblé |
| `note_poly_degree_quadratic` | `quadratic` | — | `:2089` | câblé |
| `note_poly_degree_cubic` | `cubic` | — | `:2090` | câblé |
| `note_poly_degree_quartic` | `quartic` | — | `:2126` | câblé |
| `note_poly_degree_quintic` | `quintic` | — | `:2127` | câblé |
| `note_poly_degree_sextic` | `sextic` | — | `:2128` | câblé |
| `note_poly_degree_generic` | `degree-%d` | 1 trou | `:2129` | câblé |
| `note_scale_effects_gloss` | `Scale effects: covariate effects on the log standard deviation of the latent response` | — | `:2263-2266` | câblé |
| `note_scale_effects_not_exp` | `␣(log scale, not exponentiated: their exponential is a ratio of latent SDs, not an odds ratio)` | — | `:2276-2279` | câblé |

**Bayésien (`regression_frame_stan.R`, rendu par des builders du périmètre)**

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Statut |
| --- | --- | --- | --- | --- |
| `note_loo_accuracy` | `Predictive accuracy by %s; %s.` | 2 trous | `regression_frame_stan.R:1316` | câblé |
| `note_loo_method_both` | `PSIS-LOO / WAIC` | — | `:1306` | gelé |
| `note_loo_method_loo` | `PSIS-LOO` | — | `:1309` | gelé |
| `note_loo_method_waic` | `WAIC` | — | `:1311` | gelé |
| `note_loo_se_elpd` | `SE(ELPD) = %.1f` | 1 trou | `:1297` | câblé |
| `note_loo_se_waic` | `SE(WAIC) = %.1f` | 1 trou | `:1300` | câblé |
| `note_loo_pareto_k` | `PSIS-LOO unreliable for %d of %d observations (Pareto k > %.2f); consider loo::loo_moment_match(), refitting the flagged folds (k_threshold = 0.7), or K-fold CV.` | 3 trous | `:1327-1332` | câblé |
| `note_loo_waic_unreliable` | `WAIC approximation unreliable for %d observation(s) (p_waic > 0.4); prefer PSIS-LOO (\`show_fit_stats = "elpd_loo"\`).` | 1 trou | `:1356-1361` | câblé |
| `note_stan_convergence_problems` | `Sampler diagnostics: %s. Do not report as-is; run longer or reparameterize (Vehtari et al. 2021).` | 1 trou | `:1809-1813` | câblé — §4.7 (graphie US) |
| `note_stan_convergence_partial` | `Sampler diagnostics: R-hat and ESS within targets; %s.` | 1 trou | `:1801-1806` (`#nocov`) | câblé |
| `note_stan_problem_rhat` | `max R-hat = %.3f (target < 1.01)` | 1 trou | `:1770` | câblé |
| `note_stan_problem_ess` | `min ESS = %d (target > %d)` | 2 trous | `:1776` | câblé |
| `note_stan_problem_divergent` | `%d divergent transition%s` | 2 trous ; **pluriel calculé en R** | `:1782` | câblé — §4.4 |
| `note_stan_problem_bfmi` | `min E-BFMI = %.2f (target > 0.2)` | 1 trou | `:1786` | câblé |
| `note_stan_divergent_unavailable` | `divergent-transition count unavailable for this fit` | — | `:1791` (`#nocov`) | câblé |

**Univariable et estimands de survie**

| Clé | Texte verbatim | Gabarit / trous | Consommateurs | Statut |
| --- | --- | --- | --- | --- |
| `note_uv_common_complete_cases` | `All models fit on the %d common complete cases.` | 1 trou | `regression_uv.R:901` | câblé |
| `note_uv_varying_n` | `Each univariable model is fit on its own complete cases; N varies by predictor (%d-%d).` | 2 trous | `regression_uv.R:906-911` | câblé |
| `note_estimand_rmst` | `dRMST = difference in restricted mean survival time over [0, %s]` | 1 trou : τ | `regression_survival_estimands.R:784` | câblé |
| `note_estimand_risk` | `dRisk = difference in cumulative incidence at %s` | 1 trou | `:793` | câblé |
| `note_estimand_method` | `; adjusted by g-computation from the fitted model%s, SEs by nonparametric bootstrap (%s replicates).` | 2 trous | `:814` | câblé |
| `note_estimand_stratified` | `␣(within-stratum baselines)` | — | `:816` | câblé |
| `note_estimand_skipped_terms` | `␣Transformed terms (%s) have no absolute-effect row: the contrast is defined per raw variable; rescale the variable in the data instead of the formula.` | 1 trou | `:800` | câblé — §4.7 (conseil d'action) |

### 2.15 Lacunes connues du recensement — à compléter au lot 0

Le recensement a été mené par trois lecteurs sur un périmètre de fichiers
nommés. Quatre gisements sont **partiellement** couverts. Le lot 0 les complète
mécaniquement, avec ces recettes :

| Gisement | Recette | Volume estimé |
| --- | --- | --- |
| Préfixes de titre par classe | `git grep -n "title_prefix" R/` → 49 occurrences, dont ≈ 38 hors `spicy_glm_title_prefix()` (`regression_frame_*.R`, `regression_extract.R`). Exemples confirmés : `Robust linear regression (M-estimator)`, `Heckman selection model`, `Discrete-choice multinomial logit (mlogit)`, `Beta regression`, `Multinomial logistic regression`, `Linear mixed-effects regression (nlme)`, `Generalised least squares (nlme)`, `Non-linear least squares regression`, `IV regression (2SLS)`, `Cox proportional hazards regression`. **Plus les gabarits** (`Quantile regression (τ = %.2f)`). | ≈ 38 |
| `vcov_label` par classe | `git grep -n "vcov_label" R/` — au-delà des 12 `Wald asymptotic (z)` : `Wald (model-based)` (`_nlme.R:379`), `Classical` (`_nls.R:186`, `_quantreg_AER.R:567`), `Bayesian (REML-implied)` (`_mgcv.R:347`), `Design-based (Taylor linearisation)` (`_svyglm.R:331`), plus les générateurs `.geeglm_vcov_label()`, `.fixest_vcov_label()`, `.estimatr_vcov_label()`. Tous s'affichent dans la note `Std. errors:`. | ≈ 15 |
| Libellés de `assoc.R` | `git grep -nE '"(Somers\|Lambda\|Goodman\|Kendall\|Stuart\|Cramer\|Phi)' R/assoc.R` (la barre d'alternance est échappée ici pour le markdown : la taper telle quelle) — variantes directionnelles `R\|C` / `C\|R` et `symmetric` (≈ lignes 1798-1910). | ≈ 12 |
| Libellés d'échelle / de famille des frames | `git grep -n "family_label\|scale_label" R/` | ≈ 8 |

**Total attendu après complétion : ≈ 415 entrées.**

---

## 3. Résolution des collisions de clés

Les trois lecteurs ont travaillé en parallèle. Voici les arbitrages, tous
appliqués dans les tableaux ci-dessus.

### 3.1 Même clé proposée, textes/sens différents → clés séparées

| Clé proposée | Conflit | Arbitrage |
| --- | --- | --- |
| `row_missing` | `freq_print.R:98` = `Missing` (titre de bloc) **vs** `table_categorical.R:1102` = `(Missing)` (niveau de facteur) | `row_missing_block` / `row_missing_level`. Sens et mécaniques radicalement différents : le second est un niveau de facteur (§4.3). |
| `header_n` | `cross_tab.R:758` + `regression_render.R:614` = `N` **vs** `table_categorical`/`table_continuous` = `n` | `header_n_upper` / `header_n_lower`. Casse différente = texte différent = clé différente. Le fit-stat `nobs` (`n` minuscule) **rejoint** `header_n_lower` : même texte, même sens. |
| `header_total` / `row_total` | `Total` en colonne (cross_tab, categorical) **et** en ligne (freq, cross_tab) | **Une** clé `label_total`. Même mot, même sens ; le préfixe `label_` marque le partage de rôles. Les contraintes mécaniques diffèrent par site (§4.1) mais une contrainte n'est pas une identité. |
| `header_ci_ll` / `header_ll` | `regression_dispatch.R` vs `table_continuous.R` | **Une** clé `header_ll` (idem `header_ul`). Trois familles, même sous-en-tête. |
| `header_weighted_n` / `fitstat_weighted_nobs` | en-tête lm vs ligne fit-stat | **Une** clé `label_weighted_n`. Texte identique, sens identique. |
| `header_ci_spanner` / `header_ci_template` | `paste0(ci_pct, " CI")` (continuous, lm) vs `"%s%% %s"` (régression, avec `ci_label`) | **Une** clé `header_ci_spanner` = `%s%% %s`. Les familles descriptives passent `header_ci_label_confidence` en second trou : sortie byte-identique, un seul gabarit à traduire, l'ordre pourcentage/label devient traduisible partout d'un coup. |
| `%s = %s` | légende polynomiale (`:2104`) **et** catégories de référence (`:2210`) | **Une** clé `note_kv_pair`. Gabarit de ponctuation pure, sans mot : le dupliquer n'apporterait rien. |
| `note_vcov_jackknife` | `regression_titlefooter.R:302` = `jackknife (leave-one-out)` **vs** `table_continuous_lm_print.R:222` = `jackknife` | `note_vcov_jackknife` / `note_vcov_jackknife_plain`. Textes différents. **À unifier plus tard** (§4.7). |
| `note_rows_missing_by_removed` | même gabarit, sujet = variable de groupe **vs** variable de poids | Deux clés (`…_by_removed`, `…_weights`). Le texte est identique aujourd'hui, mais une langue à accords voudra « Lignes sans valeur de *pondération* » ≠ « sans valeur de *groupe* ». Deux clés coûtent une ligne de registre et sauvent la traduction. |

### 3.2 Deux clés proposées, texte et sens identiques → une clé

| Clés proposées | Arbitrage |
| --- | --- |
| `header_assoc_cramer_v` (table_categorical) + `stat_cramer_v` (cross_tab) + libellé `assoc.R` | **Une** clé `stat_cramer_v` (et sœurs). Trois familles nomment la même mesure ; les laisser diverger est exactement le bug que le registre existe pour empêcher. |
| `cell_es_hedges_g` (cellule continuous) + `header_lm_es_g` (en-tête lm) | **Une** clé `symbol_hedges_g`. Le lecteur qui les séparait craignait qu'« une locale renomme l'un et pas l'autre » — c'est l'inverse : c'est *la même statistique*, la divergence serait le défaut. |
| `note_missing_removed` recensé 3 fois | **Une** clé, 3 consommateurs. Candidat n°1 du lot 1. |
| `note_prefix` recensé 3 fois | **Une** clé, 3 consommateurs + 4 regex dépendantes (§4.5). |

### 3.3 Même glyphe, statistique différente → clés séparées malgré tout

| Glyphe | Clés | Raison |
| --- | --- | --- |
| `η²` | `symbol_eta_sq_global` / `symbol_eta_sq_partial` | eta² d'ANOVA (table_continuous) ≠ eta² partiel (régression). La glose diffère déjà (`note_abbrev_eta2` ne parle que du partiel). |
| `ω²` | `symbol_omega_sq_global` / `symbol_omega_sq_partial` | idem |
| `f²` | `symbol_f2_global` / `symbol_f2_partial` | idem |
| `Scale` | `fitstat_scale` / `label_block_scale_effects` | paramètre d'échelle GEE ≠ bloc d'effets d'échelle CLM. Traduire les deux par « Échelle » serait faux dans un des deux cas. |
| `Random effects` | `label_block_random_effects` / `note_re_header` | en-tête de bloc de lignes ≠ tête de note. Textes identiques aujourd'hui ; le premier est **aussi une clé de données** (`parent_var`), le second non. Les séparer permet de découpler la mécanique sans toucher la note. |

### 3.4 Cas frontière tranchés

| Cas | Décision |
| --- | --- |
| `proportional` / `balanced` (jetons d'argument insérés verbatim dans `note_adjusted_for`) | **Au registre** (`note_adjustment_proportional/_balanced`), par exception à la règle « pas de valeurs de jeton ». Motif : ce sont des mots anglais que le lecteur voit *dans une phrase*. Le jeton d'argument, lui, reste `"proportional"` — le registre ne fournit que son **affichage**. |
| `weights` (repli quand le nom de la colonne de poids est irrécupérable) | Idem : `note_weights_fallback`, au registre. |
| Messages presse-papiers (`Categorical table copied to clipboard.`, etc.) | **Hors registre** (§6). `spicy_inform()` = condition R. |
| Noms d'onglets Excel (`Categorical`, `Descriptives`, `Linear models`, `Regression`) | **Au registre**, malgré leur statut de défaut d'argument : ce sont des mots que le lecteur du classeur voit. Contrainte Excel à respecter (§4.6). |

---

## 4. Pièges d'implémentation

### 4.1 Verrous structurels : du code qui lit du texte affiché

Ces sites décident d'une **mise en page** en inspectant une chaîne destinée à
l'œil. Chacun est une bombe à retardement : traduire le libellé ne produit
aucune erreur, seulement une sortie subtilement fausse.

| Site | Ce qu'il fait | Chaîne dont il dépend | Correction exigée AVANT câblage |
| --- | --- | --- | --- |
| `tables_ascii.R:446` | `grep("\\b(Total\|Column_Total)\\b", rows_txt)` — trace le filet horizontal avant la ligne de total quand `total_row_idx` est `NULL` | `label_total` | `freq_print.R` doit **poser `total_row_idx`**. `cross_tab()` le pose déjà (`:1232`). `Column_Total` n'est plus produit nulle part : littéral orphelin. |
| `tables_ascii.R:501` | `names(df) %in% c("Row_Total", "Total")` — trace la barre verticale avant la colonne de marge | `label_total` | Passer par un attribut (`margin_col_idx`) ou par `display_labels`. `Row_Total` est orphelin. |
| `tables_ascii.R:722` | `grepl("^Category$", names(x))` — choisit l'alignement (freq = 2 colonnes à gauche, cross = 1) | `header_category` | Le `print` de `freq()` doit fournir `align_left_cols` explicitement, ou renommer via `display_labels`. |
| `tables_ascii.R:876` | `grepl("^([0-9]+% C(r)?I\|SE\|p)$", …)` — repère les colonnes « compagnon » lors du découpage en panneaux | `header_ci_spanner`, `header_se`, `header_p` | Faire porter la qualité de « compagnon » par la **spec de colonne** (`build_column_spec()` la connaît déjà), pas par une regex sur l'en-tête. |
| `cross_tab.R:1475` | `grepl("%", title)` — choisit le nombre de décimales par défaut | `title_percent_row/_column/_none` | Poser un attribut `percent_mode` sur l'objet et le lire. |
| `tt_theme.R:240`, `regression_dispatch.R:1160`, `:1747` | `sub("^Note\\.", "<em>Note.</em>", …)` — italique APA | `note_prefix` | Générer la regex **depuis la clé**, ou mieux : marquer le préfixe hors du texte. |
| `regression_dispatch.R:1704-1707` | `startsWith(x, "Note.")` puis `substring(x, 6L)` | `note_prefix` | Le décalage `6L` code en dur `nchar("Note. ")`. Doit devenir `nchar(spicy_str("note_prefix")) + 1L`, ou disparaître. |
| `table_continuous.R:2365` | `sub("^Med ", "", …)` — construit la sous-ligne d'en-tête sous le spanner médiane | `header_median` | Regex générée depuis la clé, ou séparation préfixe/suffixe dans la spec de colonne. |
| `regression_titlefooter.R:1475` | `sub("^σ ", "σ² ", label)` — passe les libellés RE à l'échelle variance | `row_re_*` | `.re_panel_label()` doit prendre l'échelle **en argument** et choisir le gabarit, au lieu de réécrire un libellé déjà construit. |
| `regression_titlefooter.R:695` | `sub("^Standardised β: ", "", algebraic)` — recompose le caveat de repli | `note_standardized_caveat_algebraic` | Extraire le corps du caveat dans sa propre clé et composer les deux variantes, au lieu d'amputer. |
| `regression_titlefooter.R:205-215` | `lowercase_first()` avec liste de noms propres en dur (`Cox`, `Poisson`, `Weibull`, `Bayesian`, `Tweedie`) | tous les `title_prefix_*` | Mécanique de casse **anglaise**. Remplacer par des libellés pré-casés : chaque préfixe fournit sa forme titre et sa forme mi-phrase. |
| `regression_titlefooter.R:147-152` | `capitalize_first()` | idem | Idem. |
| `varlist-values.R:226` | met entre guillemets les valeurs `"NA"`, `"NaN"`, `""` pour les distinguer des marqueurs | `marker_na`, `marker_nan` | Si les marqueurs bougent, revoir ce filtre. Raison supplémentaire de les geler. |

### 4.2 Chaînes qui sont AUSSI des clés de données

Un libellé qui sert de nom de colonne, de nom d'élément de liste ou de valeur
retournée par `glance()` **ne peut pas** être traduit sans casser du code
utilisateur. La règle est uniforme :

> **Le nom interne reste ASCII et anglais. Le libellé se substitue au rendu**,
> via `display_labels=` (déjà disponible dans `build_ascii_table()` et
> `spicy_print_table()`, `tables_ascii.R:193,709`, et déjà utilisé par
> `regression_dispatch.R:2854-2917`).

Inventaire des chaînes concernées :

| Chaîne | Rôle data | Conséquence d'une traduction naïve |
| --- | --- | --- |
| `Variable` | nom de colonne de `output = "data.frame"`, de `varlist()` (tibble public), du corps de régression lu par `body[[1L]]`, `broom`, `as_structured()` | casse tout pipeline aval et les lookups internes (`r0$Variable`, `startsWith(df[[1]], indent_text)`) |
| `Values`, `Total`, `N` (cross_tab) | noms de colonnes soumis à l'anti-collision `make_unique_col_name()`, cités **verbatim** dans le warning `spicy_renamed_column` | le warning ment ; la colonne de marge n'est plus reconnue |
| `n`, `%` (table_categorical) | composants des clés composites `paste0(gr, " n")`, `paste0(gr, " %")` (`:2300,2333,2334,2474,2475`) | les colonnes de groupe deviennent introuvables |
| `p`, `CI lower`, `CI upper` | lookups `df[["CI lower"]]` dans `merge_ci_inline` (`:2524-2536`), `to_excel_text` (`:3059`), `clip_body$p` (`:3054`) | IC non fusionnés, colonnes Excel vides |
| `Effect size` + les `stat_*` | deviennent des **noms de colonnes** (`names(long_raw)[.assoc] <- measure_col`, `:2240`) puis des **valeurs** de `glance()$assoc_type` | traduire change une valeur de `glance()` |
| `Group` | lookups gt `:2603,2605` et **id de spanner** `spn_Group` `:2631` | id gt cassé ou non-ASCII |
| `M`, `SD`, `Med`, `Min`, `Max`, `n`, `Test`, `p`, `ES`, `LL`, `UL` | clés de colonne de `desc_spanner_groups()` / `build_header_rows()` ; lookups littéraux `:2394-2397,2444,2496,2499,2582,2831,2833,2959,2963` | spanners désalignés, colonnes mal alignées, **sans erreur** |
| `M (%s)`, `Δ (…)`, `B`, `Weighted n`, en-têtes de test lm | `out[i, <name>]`, `out[[delta_name]]`, `out[[test_header]]`, `which(col_keys %in% c("n","Weighted n","p"))` (`:431,861`) | perte d'alignement à droite, colonnes introuvables |
| `OR`/`IRR`/`HR`/`RR`/`MR`/`exp(B)` | **noms d'éléments** du vecteur `exp_defs` (`regression_titlefooter.R:516-521`) : le lookup se fait par l'en-tête affiché | l'abréviation disparaît silencieusement de la note |
| `Univariable`, `Multivariable` | posés comme `names(models)` → deviennent `model_label` (`table_regression.R:2993-2997`) | spanners du screen univariable cassés |
| `Thresholds`, `Non-proportional effects`, `Scale effects`, `Random effects`, `Zero-inflation`, `Zero hurdle`, `Dispersion` | valeurs de `coefs$parent_var`, testées par `%in%` dans **5 fichiers** | **tous les gates de bloc sautent** : blocs absents ou mal ordonnés |
| `(Intercept)` | nom R du coefficient, clé de matching partout | ligne d'intercept introuvable, notes fausses |
| `N_distinct`, `N_valid`, `NAs`, `Label`, `Class` | colonnes du tibble public de `varlist()`, documentées dans `@returns` | rupture de contrat documenté |

### 4.3 Mécanique par CLÉ, jamais par libellé — contrainte non négociable

Trois mécaniques dépendent aujourd'hui d'un **libellé** et doivent basculer sur
un **jeton** avant tout câblage. C'est la partie non triviale du lot.

**(a) Le niveau `(Missing)`**

`.add_missing_level()` (`table_categorical.R:46`) ajoute `(Missing)` comme
**niveau de facteur**. Trois mécaniques en dépendent :

1. **Ordonnancement** : le niveau doit rester en dernier (`:1351-1353`, `:2265`).
2. **Anti-collision** : si les données contiennent déjà un niveau littéralement
   nommé `(Missing)`, le code cherche `(Missing_1)`, `(Missing_2)`…
   (`:1103-1113`).
3. **Exclusion du khi-deux** : le niveau ne doit **jamais** entrer dans le test
   (commentaire `:2053-2055`).

> Ces trois mécaniques doivent être pilotées par `attr(x, "missing_level")` —
> la valeur effectivement posée, quelle qu'elle soit — et jamais par une
> comparaison au littéral `"(Missing)"`. C'est la condition pour que
> `spicy.labels = c(row_missing_level = "(Sans réponse)")` fonctionne à
> l'étage 2 sans faire entrer les manquants dans le khi-deux.

**(b) La marge `Total`**

`table_categorical.R:1957-1974` : `Total` est simultanément (1) le libellé de
marge, (2) la **clé** `attr(x, "total_group")` utilisée par `tidy()`/`glance()`
pour **écarter** les lignes de marge (`table_categorical_print.R:277,366`), et
(3) la cible du garde anti-collision (renommage en `Total_1`, warning `:1974`).

> Si le libellé est traduit et la clé non, le garde cesse de protéger un
> utilisateur dont un niveau de `by` s'appelle « Total » dans sa langue.
> Si les deux bougent, la regex de renommage et l'attribut doivent bouger
> ensemble. Solution : la clé est un jeton interne fixe
> (`"__spicy_total__"`) ; le libellé et le garde de collision se lisent
> tous deux depuis `label_total`.
> **`cross_tab()` a sa propre mécanique de marge : les deux doivent être
> traduites dans le même lot**, sinon un tableau et son `cross_tab()` source
> se contredisent.

**(c) Les blocs subordonnés de la régression**

`parent_var ∈ {Thresholds, Non-proportional effects, Scale effects, Random
effects, Zero-inflation, Zero hurdle, Dispersion}` est testé par `%in%` dans
`regression_render.R`, `regression_align.R`, `regression_titlefooter.R`,
`regression_frame_ordinal.R`, `regression_frame_glmmTMB.R`,
`regression_frame_pscl.R`.

> Introduire un jeton (`"thresholds"`, `"random_effects"`, …) porté par les
> frames, et une table jeton → `label_block_*` consultée **au seul moment du
> rendu**. Sans cela, aucune traduction de bloc n'est possible.
> Note : `regression_align.R:240-245` ne liste pas `Non-proportional effects`,
> alors que `:419-425` le liste. **Vérifier si c'est intentionnel** avant de
> figer la table de jetons.

### 4.4 Gabarits éclatés, pluriels et casse codés à l'anglaise

| Site | Problème | Traitement à l'étage 1 |
| --- | --- | --- |
| `cross_tab.R:1068-1080` | `paste0(n, " expected cell", if (n > 1) "s" else "", " < 5 (…")` — le `s` du pluriel est une chaîne **séparée** | Reconstituer **un gabarit par forme** (`note_expected_lt5`), le `s` restant un trou à l'étage 1 (byte-identique). L'étage 2 remplacera par un mécanisme singulier/pluriel du registre. |
| `regression_frame_stan.R:1782` | `if (n_div > 1L) "s" else ""` | Idem. |
| `regression_titlefooter.R:1650,1655,2008,2010,1883,1356` | `(s)` de pluriel optionnel anglais | Ne se transpose pas en français (accord). Inscrit tel quel ; l'étage 2 devra fournir deux formes. |
| `regression_titlefooter.R:185` | `%s models.` — pluriel accolé | Idem. |
| `regression_titlefooter.R:2111` | pluriel choisi par `if/else` sur **deux préfixes** (`Ordered factor` / `Ordered factors`) puis concaténé | Deux clés à l'étage 1 ; gabarit complet à l'étage 2. |
| `table_continuous.R:1621-1622` | `one-way ANOVA`, `Welch one-way ANOVA` en minuscule **parce qu'ils sont mi-phrase** | La casse est une propriété du **gabarit**, pas du libellé. Inscrire tel quel ; documenter que le gabarit `note_group_comparison` impose la position. |
| `table_continuous.R:1677` | `note_gloss_med_ci` : le **même** trou apparaît deux fois | Forme positionnelle `%1$s` **obligatoire** dès l'étage 1. |
| `table_continuous_lm_print.R:207,215,219` | fragments recollés commençant par une espace (`, clusters by␣`, `␣(%d replicates)`) | Inscrire les fragments à l'étage 1 (byte-identique). **Signaler** qu'une langue à ordre de proposition différent exigera un gabarit unique — dette de l'étage 2. |
| `regression_titlefooter.R:1712` | `format(n, big.mark = ",")` — séparateur de milliers **codé en dur à l'anglaise**, incohérent avec `decimal_mark` | **Vrai bug i18n**, indépendant du registre. À corriger dans son propre commit (§4.7). |

### 4.5 Regex de réécriture sur libellé construit

Récapitulées en §4.1. Elles partagent un antipattern : **construire un libellé,
puis le réécrire**. Toutes doivent devenir « choisir le bon gabarit ». Sites :
`sub("^Note\\.")` ×3, `substring(…, 6L)` ×1, `sub("^Med ")` ×1,
`sub("^σ ")` ×1, `sub("^Standardised β: ")` ×1, `lowercase_first()`,
`capitalize_first()`.

### 4.6 Contraintes de format des artefacts

- **Onglets Excel** : 31 caractères maximum, caractères `: \ / ? * [ ]`
  interdits. `wb_add_worksheet()` échoue durement. Toute traduction de
  `title_excel_sheet_*` doit être validée par un test.
- **Ids gt** : `spn_Group` (`table_continuous.R:2631`) est un identifiant, pas
  un libellé. Il reste ASCII.
- **Non-ASCII et `R CMD check`** : le registre concentrera tous les glyphes
  non-ASCII du paquet dans un seul fichier. Vérifier qu'ils restent encodés en
  échappements `\uXXXX` là où le paquet le fait déjà (cf. `dev/fix_nonascii.R`).

### 4.7 Duplications et incohérences : unifier ou pas

**À unifier dans le lot correspondant** (sortie identique, dette supprimée) :

| Cas | Décision |
| --- | --- |
| `note_missing_removed` / `_declared_` / `note_missing_item` × 3 fichiers | Unifier. Aucun helper commun aujourd'hui — c'est le gain n°1. |
| `note_prefix` × 3 fichiers + 4 regex | Unifier, et générer les regex depuis la clé. |
| `Wald asymptotic (z)` × 12 frames | Unifier. |
| `Model %d` × 6 constructions | Unifier ; sinon le spanner et l'auto-remplissage de `names(models)` divergeront. |
| `Model %d: %s` × ≈15 sites | Unifier. Candidat le plus évident du paquet. |
| `header_ci_ll_full` / `_ul_full` × 6 sites, 2 constructions | Unifier (`table_continuous` passe par `ci_pct`, `table_continuous_lm_render` inline deux fois). |
| `header_ci_spanner` × 3 helpers (`build_header_rows`, `build_header_rows_lm`, `build_column_spec`) | Unifier le gabarit. Les trois helpers restent, mais lisent la même clé. |
| `.surv_title_dist()` (13 entrées) vs `.survreg_dist_title()` (7 entrées) | **Unifier les deux tables** : elles répondent au même besoin et divergent déjà (`Logistic` et `Student-t` manquent dans la première). Faire avant la mise au registre. |
| `note_weight` / `note_weight_rescaled` : deux graphies (espace dans le littéral vs ajouté par `paste`) | Unifier sur la forme **avec** espace dans le littéral (`␣(rescaled)`), en adaptant l'assemblage. Vérifier la byte-identité. |
| `p-values: Wald-z asymptotic (lme4)` / `(glmmTMB)` | Unifier en gabarit `(%s)`. |
| `note_vcov_cluster_named` recopié en dur `:298,304` | Faire appeler le fragment au lieu de le recopier. |
| Les 8 littéraux `Δ…` de `fit_stat_label()` | Unifier en gabarit `Δ%s` réutilisant le libellé de base. **Sauf `F-change`**, qui n'a pas de `Δ`. |
| `title_varlist_empty` court-circuite `varlist_title()` | Faire passer par le helper. |
| Défaut de seuils d'étoiles défini **deux fois**, dans un **ordre inversé** (`resolve_stars_thresholds()` vs `build_stars_footer_block()`) | **Bug latent** : si l'un change, la légende ment. Unifier, dans son propre commit. |

**À NE PAS unifier** (sens différents, malgré un texte identique) :
`symbol_*_global` / `_partial` ; `fitstat_scale` / `label_block_scale_effects` ;
`label_block_random_effects` / `note_re_header` ; `row_missing_block` /
`row_missing_level` ; `cell_na` / `row_na_label` ; `header_effect_size` (mot
complet, catégoriel) / `header_effect_size_abbrev` (`ES`, continu) ;
`title_crosstab_by` (` x ` du titre) / le ` x ` d'aplatissement d'`interaction()`
(`cross_tab.R:400`) / le ` x ` de dimensions de `varlist` (`varlist-values.R:270`)
— trois usages, trois sens.

**Incohérences signalées, à arbitrer hors registre** (ne pas corriger en
douce dans un lot d'extraction ; chacune mérite son commit et sa ligne de
`NEWS` si elle change une sortie) :

1. `Chi-2` (`cross_tab.R:1003`) vs `chi-squared` (doc et messages) vs `χ²`
   (régression) : trois graphies du même test.
2. `, 95% CI [` de `cross_tab.R:1025` : le **95 est codé en dur** alors que le
   séparateur interne vient de `ci_bracket_separator(decimal_mark)`.
3. `header_med_iqr` affiche `[Q1, Q3]` avec une virgule alors que les cellules
   utilisent `; ` quand `decimal_mark` est `,` (`table_continuous.R:2141`) :
   en-tête et cellules se contredisent sous convention européenne.
4. `cell_test_f` / `header_lm_test_f_df` : le séparateur de ddl reste `, `
   même sous `decimal_mark = ","`, contrairement à `ci_sep` (`:2132`).
5. `title_excel_sheet_continuous` = `Descriptives` ≠ titre du tableau
   `Descriptive statistics`. Idem pour `Linear models` vs
   `Continuous outcomes by …`.
6. `note_re_test_lrt` dit `p-values` (pluriel), `note_re_test_rlrt` dit
   `p-value` (singulier).
7. `reparameterize` (US, `regression_frame_stan.R:1811`) vs `standardised` /
   `Generalised` (UK) partout ailleurs.
8. `note_mixed_inference_wald_lmer` et `note_estimand_skipped_terms` portent un
   **conseil d'action** alors que la doctrine du paquet veut que les notes
   énoncent des faits (commentaires `regression_titlefooter.R:1646-1649`,
   `:1693-1695`).
9. `value_summary_error` (`<error: …>`) vs `value_summary_invalid`
   (`Error: invalid values`) : deux graphies pour le même genre de cellule.
10. `note_rows_missing_by_removed` a une grammaire (verbe final) différente des
    deux notes de manquants voisines, alors que le commentaire annonce
    « same grammar ».
11. `row_block_fixed_effects` porte le `:` dans le littéral, alors que les
    autres blocs le reçoivent du rendu.
12. `big.mark = ","` codé en dur (`regression_titlefooter.R:1712`).
13. `table_categorical` / `table_continuous` posent une caption en console et en
    tinytable, mais **aucune** en gt / flextable / word / excel.

### 4.8 Chaînes déjà à source unique : le registre les absorbe

Ces helpers sont les **points d'extraction naturels** : le lot correspondant
remplace un littéral par un `spicy_str()` dans le helper, et rien d'autre ne
bouge. Ce sont les lots les moins risqués — les faire en premier dans chaque
famille.

`.categorical_title()`, `.continuous_title()`, `.categorical_note()`,
`.assoc_label()`, `continuous_test_label()`, `.tclm_note_text()`,
`.tclm_vcov_label()`, `get_delta_label_lm()`, `get_test_header_lm()`,
`format_effect_size_header_lm()`, `format_r2_header_lm()`,
`fit_stat_label()`, `build_column_spec()`, `resolve_label()`,
`spicy_glm_exp_header()`, `spicy_glm_title_prefix()`, `.re_panel_label()`,
`.surv_title_dist()`, `varlist_title()`.

À l'inverse, **ces chaînes n'ont aucun helper** et doivent en recevoir un
pendant l'extraction : le titre de `table_continuous_lm()`
(`table_continuous_lm_print.R:98`), le titre de `freq()`
(`freq_print.R:183`), le titre de `cross_tab()` (`cross_tab.R:1096`),
`header_lm_mean_level` (4 reconstructions), `label_model_default`
(6 constructions), `header_ci_ll_full`/`_ul_full` (6 sites).

---

## 5. Plan d'implémentation de l'étage 1

### 5.1 Test de non-régression, valable pour TOUS les lots

```sh
Rscript -e "devtools::test()"        # 0 snapshot modifié
Rscript -e "devtools::check()"       # 0 error, 0 warning, 0 note
```

> Le critère d'acceptation d'un lot est : **`git diff` sur
> `tests/testthat/_snaps/` est vide.** Si un snapshot bouge, le lot a changé la
> sortie : soit c'est un bug d'extraction (corriger), soit c'est une correction
> volontaire listée en §4.7 (la sortir du lot et lui faire son propre commit
> avec sa ligne de `NEWS`).

Test dédié `tests/testthat/test-i18n.R`, posé au lot 0 :

1. `expect_snapshot(.spicy_strings)` — le registre lui-même est sous snapshot :
   toute modification d'un défaut devient visible en revue.
2. Clés uniques : `expect_false(anyDuplicated(names(.spicy_strings)) > 0)`.
3. Aucune clé morte : chaque nom de `.spicy_strings` apparaît au moins une fois
   dans `R/` (grep sur les sources installées).
4. `spicy_str()` sur une clé inconnue lève une erreur.
5. Cohérence des gabarits : toute valeur contenant `%` et consommée par
   `spicy_fmt()` a un nombre de `%s`/`%d` compatible avec ses appels
   (vérification par appel réel dans les tests de famille).

### 5.2 Les lots

Ordre imposé : **infrastructure → besoin de terrain → familles descriptives →
régression**. Chaque lot est un commit.

| Lot | Contenu | Fichiers | Chaînes | Risque |
| --- | --- | --- | --- | --- |
| **0** | `R/i18n.R` (`.spicy_strings` vide, `spicy_str()`, `spicy_fmt()`), `test-i18n.R`, complétion du recensement (§2.15) et mise à jour de ce fichier | +2 | 0 câblée | nul |
| **1** | **Famille « manquants »** : `row_missing_level`, `row_missing_level_dedup`, `note_missing_removed`, `note_declared_missing_removed`, `note_missing_item`, `note_missing_rows_total`, `note_rows_missing_by_removed`, `note_rows_missing_weights`, `note_weights_fallback`. **Inclut le découplage §4.3(a)** : `attr(x, "missing_level")` pilote ordre / collision / exclusion du khi-deux | `table_categorical.R`, `table_continuous.R`, `table_continuous_lm.R`, `cross_tab.R` | ≈ 9 | **élevé** (mécanique) |
| **2** | **Titres** : `title_categorical`, `title_categorical_by`, `title_continuous`, `title_continuous_lm_by` (+ helper à créer), `title_continuous_lm_by_fallback`, `title_freq` (+ helper), `title_crosstab` + `_by` + `_group` + `title_percent_*` (**inclut la neutralisation du `grepl("%", title)`, §4.1**), `title_varlist*`, `title_excel_sheet_*` | 8 | ≈ 20 | moyen |
| **3a** | En-têtes `freq()` + **pose de `total_row_idx` et `align_left_cols`** (§4.1) | `freq_print.R`, `tables_ascii.R` | ≈ 12 | **élevé** (verrous) |
| **3b** | En-têtes et notes `cross_tab()` : `label_values`, `label_total`, `header_n_upper`, `row_n`, `row_total`, notes chi-deux / effectifs théoriques / poids. **Inclut le découplage de la marge, §4.3(b)** | `cross_tab.R`, `tables_ascii.R` | ≈ 25 | **élevé** |
| **3c** | En-têtes `table_categorical()` + `stat_*` via `.assoc_label()` + `label_total`/`_dedup` côté catégoriel | `table_categorical.R` | ≈ 20 | moyen-élevé |
| **3d** | En-têtes `table_continuous()` (dont `header_median` et la regex `^Med␣`, §4.5) | `table_continuous.R` | ≈ 28 | moyen-élevé |
| **3e** | En-têtes `table_continuous_lm()` (dont les 4 reconstructions de `M (%s)`) | `table_continuous_lm_render.R` | ≈ 15 | moyen |
| **4** | Notes des familles descriptives : gloses `table_continuous()`, `test_*`, `note_group_comparison*`, `note_adjusted_for`, `note_std_errors*` et `note_vcov_*` de la famille lm, `note_prefix` **avec génération des regex depuis la clé** (§4.5) | 5 | ≈ 35 | **élevé** (`note_prefix`) |
| **5** | Régression, en-têtes + fit-stats : `build_column_spec()`, `fit_stat_label()`, `spicy_glm_exp_header()`, `header_ll`/`_ul`, `header_ci_spanner` unifié | `regression_render.R`, `regression_align.R`, `glm_compute.R`, `regression_dispatch.R` | ≈ 70 | faible (`display_labels` déjà en place) |
| **6** | Régression, blocs et lignes : `label_block_*` **avec introduction des jetons `parent_var` (§4.3c)**, `row_re_*` **avec suppression de la réécriture σ→σ²**, `row_intercept`, styles de référence, `label_model_default` | 8 | ≈ 25 | **très élevé** |
| **7** | Régression, titres : `title_*` + les ≈ 50 préfixes par classe + **remplacement de `lowercase_first()`/`capitalize_first()` par des libellés pré-casés** | `regression_titlefooter.R`, `glm_compute.R`, tous les `regression_frame_*.R` | ≈ 55 | élevé |
| **8a** | Notes régression — vcov et intervalles (`note_vcov_*` ×20, `note_ci_*` ×3, `note_std_errors_*`) | 2 + frames | ≈ 30 | faible |
| **8b** | Notes régression — abréviations et standardisation (`note_abbrev_*` ×21, `note_standardized_*` ×8, dont le refactor `sub("^Standardised β: ")`) | 1 | ≈ 29 | moyen |
| **8c** | Notes régression — effets aléatoires, mixtes, composantes (`note_re_*`, `note_mixed_*`, `note_component_*`, `note_singular_*`) | 4 | ≈ 25 | moyen |
| **8d** | Notes régression — exponentiation (les 4 gabarits géants + gloses) | 1 | ≈ 12 | moyen |
| **8e** | Notes régression — survie, GEE, distributions (**inclut l'unification `.surv_title_dist()` / `.survreg_dist_title()`**, §4.7) | 3 | ≈ 30 | moyen |
| **8f** | Notes régression — bayésien, multiplicité, polynômes, univariable, estimands | 4 | ≈ 35 | faible |
| **9** | `varlist()` / `code_book()` : titres et marqueurs uniquement (les 7 colonnes restent `data`) | 3 | ≈ 15 | faible |
| **10** | Symboles et marqueurs restants (`symbol_*`, `marker_*`, `cell_undefined`, étoiles + **unification des seuils d'étoiles**, §4.7) | 4 | ≈ 25 | faible |

**Total ≈ 415 chaînes, 19 commits.**

Les lots 3a–3e, 8a–8f sont indépendants entre eux : parallélisables sur des
worktrees séparés. Les lots 1, 6 et 7 touchent des mécaniques et doivent être
faits **seuls**, revus, et testés avant le lot suivant.

### 5.3 Ce qu'un lot doit contenir, exactement

1. Les entrées ajoutées à `.spicy_strings`, **dans l'ordre du registre** (§2).
2. Le remplacement des littéraux par `spicy_str()` / `spicy_fmt()` aux sites
   listés en colonne « consommateurs ».
3. Le découplage clé/libellé **s'il est requis** par §4.1–4.3 — dans le **même**
   commit, jamais après.
4. Aucun changement de sortie. `git diff tests/testthat/_snaps/` vide.
5. Pas de ligne de `NEWS` : l'étage 1 est invisible pour l'utilisateur. Le
   `NEWS` viendra à l'étage 2, quand l'option existera.

---

## 6. Ce qui est explicitement HORS registre

| Catégorie | Exemples | Pourquoi |
| --- | --- | --- |
| **Conditions R** : erreurs, warnings, messages | `spicy_abort()`, `spicy_warn()`, `spicy_inform()` — y compris `Categorical table copied to clipboard.` (`table_categorical.R:1908,3239`), `Descriptive statistics copied to clipboard.` (`table_continuous.R:3067`), `Linear-model table copied to clipboard.` (`table_continuous_lm_render.R:971`), `spicy_renamed_column`, `spicy_summary_failed` | Décision Amal : les conditions restent en anglais. Elles sont lues par un développeur, apparaissent dans les rapports de bug, les recherches web et les traces `R CMD check`. Les traduire fragmenterait le support. Les trois messages presse-papiers sont le cas frontière (un lecteur normal les voit) : **écartés quand même**, parce que ce sont des conditions et qu'un utilisateur peut les faire taire par `suppressMessages()`. |
| **Valeurs de jetons d'arguments** | `"welch"`, `"cramer_v"`, `"HC3"`, `"CR2"`, `"exchangeable"`, `"refit"`, `"proportional"` | Ce sont des identifiants d'API. Traduire un jeton casserait tous les scripts. **Exception** : quand un jeton est inséré **verbatim dans une phrase** (`proportional` dans `note_adjusted_for`), son *affichage* entre au registre — le jeton, lui, ne bouge pas (§3.4). |
| **Noms de colonnes de sorties publiques** | `Variable`, `Level`, `Chi2`, `df`, `N_distinct`, `N_valid`, `NAs`, `Label`, `Class`, `Values` du tibble `varlist()` | Contrat documenté par `@returns` ; du code utilisateur en dépend. Inscrits au registre avec le statut `data` pour être **verrouillés**, pas traduits. Une traduction éventuelle se ferait au rendu seulement. |
| **Valeurs de données** | niveaux de facteurs, noms de variables, labels de variables, `typeof()`, unités `difftime` | Ce sont les données de l'utilisateur, pas notre texte. Elles remplissent les trous des gabarits. |
| **Noms propres, paquets, fonctions, options** | `Cox`, `Poisson`, `Weibull`, `Satterthwaite`, `Gelman 2008`, `clubSandwich`, `lmerTest`, `stats::p.adjust`, `loo::loo_moment_match()`, `options(spicy.simulate_p = TRUE)`, `spicy.re_se_max_n`, `SPSS`, `Stata`, `vce(cluster)` | Identifiants et citations. Préservés à l'identique **à l'intérieur** des gabarits, comme trous non traduisibles ou comme littéraux du gabarit. |
| **Documentation roxygen et vignettes** | y compris les endroits qui citent verbatim des chaînes du registre (`freq.R:137` cite « Declared missing values »; `varlist()` cite `<NA>`, `<NaN>`, `...`, `*`) | Autre chantier. **Mais** : toute modification d'une chaîne du registre citée dans la doc doit mettre la doc à jour. Le lot concerné vérifie par grep. |
| **Regex et identifiants internes** | `^Category$`, `c("Row_Total","Total")`, `spn_Group`, `FE: <factor>`, `": LL"` / `": UL"` de `regression_structured.R:195-196` | Ce sont des **clés**. Leur existence est le problème (§4.1), pas leur contenu. Elles ne sont pas traduites : elles sont **supprimées ou générées depuis une clé**. |
| **Sorties de paquets tiers** | messages de `parameters`, `marginaleffects`, `flexsurv`, noms de structures `geepack` | Hors de notre contrôle. |

---

## 7. Annexe — recettes de vérification

```sh
# Un littéral a-t-il encore des occurrences hors du registre ?
git grep -n '"Missing values removed: "' -- R/ | grep -v 'R/i18n.R'

# Combien de chaînes restent à extraire dans un fichier ?
git grep -cn '"' R/regression_titlefooter.R

# Les snapshots ont-ils bougé ? (critère d'acceptation d'un lot)
git status --porcelain tests/testthat/_snaps/

# Les clés du registre sont-elles toutes consommées ?
Rscript -e 'devtools::load_all(); ks <- names(spicy:::.spicy_strings);
  src <- paste(unlist(lapply(list.files("R", full.names = TRUE), readLines)), collapse = "\n");
  print(ks[!vapply(ks, function(k) grepl(k, src, fixed = TRUE), logical(1))])'

# Non-ASCII concentrés dans le registre : liste des glyphes
Rscript -e 'devtools::load_all(); v <- spicy:::.spicy_strings;
  print(v[grepl("[^\\x01-\\x7F]", v, perl = TRUE)])'
```
