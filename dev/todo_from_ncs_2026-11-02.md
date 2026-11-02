# Retours d'usage — projet NCS infirmières scolaires (2 novembre 2026)

Observés sur `table_regression(output = "flextable")` dans un rapport Word
rendu par Quarto puis converti en PDF par LibreOffice (spicy 0.14.0/0.14.1,
modèles lmerModLmerTest, glmerMod avec effets croisés, clmm).

1. **Signe moins des valeurs négatives.** Les cellules écrivent le trait
   d'union ASCII (`-0.4`, `-4.16` dans les notes de seuils), alors que la
   typographie attend le signe moins U+2212 (`−0.4`). Le texte de l'article,
   mis en forme à part, utilise U+2212 : l'incohérence se voit entre prose et
   tableaux. Proposition : option de style (`spicy_style(minus = "\u2212")`)
   ou conversion par défaut en sortie flextable, gt, tinytable et Word.

2. **Note native qui cite une option interne.** Pour un glmer à 16 314
   réponses, la note dit : « Random-effect variance components: SE and CI not
   computed (n = 16314 exceeds the spicy.re_se_max_n cap) ». Le nom d'option
   n'a pas sa place dans une note destinée à un lecteur de revue. Proposition :
   « SE and CI of the variance components not computed for n > 10 000 (see
   `?table_regression`) », le nom d'option restant dans la documentation.

3. **En-tête « 95% CI » au-dessus de deux colonnes LL / UL** dans LibreOffice :
   le libellé fusionné se cale au-dessus de LL seul et « p » se retrouve au
   bout du filet (Word rend correctement). À vérifier si la cellule fusionnée
   de l'en-tête porte bien `merge_h` plutôt qu'un simple alignement.
   *Constat :* la fusion est réelle (spans 2 puis 0 dans l'objet,
   `<w:gridSpan w:val="2"/>` dans le docx), un test l'épingle
   (`test-engine_parity_flextable.R`). En `layout = "autofit"`, flextable
   n'écrit pas de `<w:tblGrid>` et LibreOffice reconstruit la grille sur
   la première ligne : « 95% CI » tombe sur LL seul et « p » sur UL
   (vérifié par conversion headless en ODT). Avec `layout = "fixed"` la
   grille est écrite et LibreOffice lit l'en-tête correctement. Seul
   `table_regression()` pose `autofit` (`output_flextable()`). Les
   familles descriptives gardent le `fixed` par défaut de flextable.
   *Décision (2026-11-02) :* `table_regression()` passe en `layout = "fixed"`,
   comme les autres familles ; test et NEWS.

4. **Étiquettes des modalités de référence** : `T0 (2018) (ref.)` empile deux
   parenthèses quand le niveau porte déjà une parenthèse. Proposition :
   `T0 (2018), ref.` ou `reference_style = "annotation"` par défaut quand le
   libellé contient une parenthèse.

Fichiers témoins : `C:/Users/at/Documents/Projets/ncs-infirmieres-scolaires/reports/article-draft.qmd`
(Supplementary Tables S5 à S9) et `rapport.qmd`.
