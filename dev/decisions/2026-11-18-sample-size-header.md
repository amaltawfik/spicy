# ADR 2026-11-18 — `spicy_style(sample_size = )` : l'effectif dans la note, dans les chapeaux ou nulle part

**Statut** : design approuvé par Amal le 2026-11-18 (« go sur les 2 design »). Mis en œuvre le même jour. Il rouvre les alternatives (a) et (e) de l'ADR `2026-10-28-note-effectif.md`, tranchées « la note seule » et « pas de levier pour l'instant ».

## Contexte

Depuis le lot L, l'effectif ouvre la note : « N = 1200: Female (n = 620), Male (n = 580). » C'est la convention de l'APA, dont les tableaux modèles gardent des chapeaux nus (`dev/journal_styles/APA_SampleTables_apastyle_2024-06.txt` : « Note. N = 150 (n = 50 for each condition). »).

Les revues médicales font autrement. Les gabarits de JAMA (`JAMA_InstructionsForTableCreation_2016.pdf` : « Intervention (n=200) Control (n=201) ») et du Lancet (`Lancet_RCT_FormattingGuidelines_2025.pdf` : « Drug A (n=500) Drug B (n=502) ») placent l'effectif de chaque groupe dans son chapeau de colonne. gtsummary et table1 font de même par défaut. Un chercheur en santé ne pouvait pas obtenir ce Table 1 sans retoucher le tableau.

## Décision

Un réglage de style, `spicy_style(sample_size = c("note", "header", "none"))`, posé une fois pour un rapport comme `note_label` et `abbreviations`. Le défaut `"note"` ne change aucune sortie existante.

- **`"header"`** : sous `by`, chaque chapeau de groupe porte son effectif, « Female (n = 620) », de même que la colonne Total et la colonne des `by` manquants. Cela vaut pour les trois familles dont les groupes sont des colonnes : `table_categorical()`, `table_categorical_svy()` et `table_continuous_lm()`, sur tous les moteurs. La note garde la phrase d'ensemble, « N = 1200. », sans la liste des groupes. Sans `by` et dans les familles dont les groupes sont des lignes, `"header"` se comporte comme `"note"`.
- **`"none"`** : la phrase d'effectif disparaît de la note dans toutes les familles qui en ont une. La ligne Base de `table_multiresponse()` et les batteries ne changent pas.
- **Enquêtes** : n est l'effectif non pondéré, celui que la note définit déjà. gtsummary met le N pondéré dans ses chapeaux.
- Les sorties de données (`as.data.frame()`, `tidy()`, `glance()`, `as_structured()`) sont identiques sous les trois valeurs.

L'effectif du chapeau s'écrit avec des espaces insécables dans la parenthèse. Sans elles, Typst coupait « Central (n / = 245) ».

## Ce qui a été mesuré

Typst, `table_categorical(by = sex)` avec note, largeurs naturelles en points :

| Réglage | Libellés | Colonnes de groupe | Total |
|:--|--:|:--|--:|
| `"note"` | 98,2 | 25,3 et 29,3 par groupe | environ 401 |
| `"header"` | 98,2 | 35 à 44 par colonne | environ 421 |

Le tableau tient dans la page sans réduction et chaque chapeau reste sur une ligne. Avec six groupes, les chapeaux passent proprement sur deux lignes (« Central / (n = 245) »).

## Limites

- Avec huit groupes et un niveau manquant, les planchers ne tiennent plus dans la page et la réduction proportionnelle de tinytable reprend : les colonnes p et V se chevauchent. Le défaut existe déjà sans le réglage pour un tableau de cette largeur.
- Les thèmes de revues ne sont pas modifiés dans ce lot. JAMA et le Lancet prendraient `"header"` d'après leurs gabarits, qui écrivent aussi « (n=200) » sans espaces. C'est une décision à part.
- Non vérifiés : la compilation LaTeX, un fichier ouvert dans Word, Typst pour `table_continuous_lm()` et pour le tableau d'enquête, le français dans les moteurs riches.

## Conséquences

- Taille : R/ +201/−34 sur 12 fichiers, dont une bonne part de commentaires et d'aide. Un fichier de tests et son fichier de snapshots, tous deux neufs.
- Une puce de NEWS dans les nouveautés.

## Garde

`tests/testthat/test-sample_size_style.R` : les trois valeurs pour chacune des trois familles, chaque moteur (console, tinytable en HTML, LaTeX, Typst et markdown, gt, flextable, Word, Excel, presse-papiers), le français, `drop_na`, un niveau de `by` manquant, les poids, l'absence de `by`, la validation d'une valeur fausse.

## Amendement du 2026-11-19

Décision d'Amal : les thèmes `"jama"` et `"lancet"` posent `sample_size = "header"`. Cela lève la deuxième limite ci-dessus. Les autres thèmes gardent la note : le gabarit de l'APA met l'effectif dans la note (« Note. N = 150 (n = 50 for each condition). »), les documents des Annals et de l'AER ne montrent pas d'effectif de groupe et `dev/journal_styles/` n'a aucun document de NEJM.

L'effectif s'écrit « (n = 620) », avec les espaces de spicy, et non « (n=200) » comme les gabarits. Aucun réglage de thème ne règle l'espace autour du signe égal. Le seul « = » sans espaces du Lancet est celui des définitions d'abréviations (`note_abbrev_pair_lancet`, « HR=hazard ratio. »). Ses notes écrivent « Δ = Male − Female » et « χ²(2·00) = 1·07, p = 0·58 », et ses cellules « <0·0001 ».

Les thèmes étant publiés depuis la 0.13.0, le changement figure dans les « Breaking changes » de NEWS. Un `sample_size` composé sur le thème l'emporte (`spicy_style("jama", sample_size = "note")`), de même qu'un argument `style` sur `options(spicy.style = "jama")`, qu'il remplace en entier.

Vérifié dans `test-sample_size_style.R` pour les deux thèmes et les trois familles : les chapeaux portent l'effectif sur chaque moteur, la note garde « N = … », le réglage de l'utilisateur l'emporte, les sorties de données (`as.data.frame()`, sortie longue, `as_structured()`, `tidy()`, `glance()`) sont identiques à celles du thème réglé sur `"note"`.
