# Trois décisions d'API — dossier pour Amal (2026-08-13)

Chacune tient en un choix. Mes recommandations sont argumentées ;
rien n'est implémenté avant ton arbitrage.

## 1. i18n des libellés de sortie — DÉCIDÉ 2026-08-13 : OUI à la reco
## (deux étages, lot dédié post-parité)

Le besoin : « (Missing) », « Missing values removed: ... », les
en-têtes (M, SD...) et gloses sortent en anglais au milieu d'un
rapport français. Tes trois pistes : argument par fonction / option
de package / jeu de libellés par langue (modèle lssdoc chrome_lang).

RECOMMANDATION : la piste 3, en DEUX étages.
- Étage 1 (mécanique) : toutes les chaînes d'affichage sortent du
  code vers un registre interne unique (une liste nommée par clé
  stable). Zéro changement visible, mais la traduction devient
  POSSIBLE et les chaînes cessent d'être éparpillées (on en a
  recensé dans 6 fichiers).
- Étage 2 (API) : options(spicy.language = "fr") + jeux "en"
  (défaut, identique à aujourd'hui) et "fr" livrés, extensibles par
  l'utilisateur (spicy.labels = liste de surcharge, pour les cas
  Healthy Campus où il faut UN libellé, pas une langue).
  Pourquoi une option de package malgré ta réserve sur l'état
  global : la langue d'un RAPPORT est par nature globale au
  document (posée une fois dans le setup) ; un argument par appel
  serait répété 34 fois dans ton rapport réel. lssdoc a déjà
  tranché pareil (chrome_lang). La réserve reste vraie pour tout le
  reste du package — la langue est l'exception défendable.
- Contraintes non négociables intégrées : défaut anglais intact
  (snapshots, tests) ; la mécanique (Missing_1 sur collision,
  niveau hors khi-deux) traverse par CLÉ, pas par libellé.
COÛT : étage 1 moyen (recensement + registre + tests inchangés),
étage 2 petit. À planifier comme un lot dédié post-parité.

## 2. Gouttière Typst — DÉCIDÉ 2026-08-13 : GO les deux volets
## (ta note
dev/gouttiere_tinytable_group_tt.md)

Le fait : tinytable code column-gutter: 5pt en dur dès qu'il y a
group_tt(j=) ; 16/34 tableaux de ton rapport l'avaient, 18 non —
grille non homogène, irrécupérable côté document.

RECOMMANDATION : les deux voies de ta note, ensemble.
- spicy NEUTRALISE la gouttière dans son finaliseur Typst (la
  substitution que tu esquisses), pour TOUS ses tableaux : la
  géométrie appartient au document, et la grille spicy redevient
  homogène (réponse à ta question « homogène ou déléguée » : les
  deux — homogène PARCE QUE déléguée : 0pt partout chez nous, le
  document pose la sienne s'il en veut une).
- ET issue upstream tinytable (la gouttière devrait être une option
  de group_tt, pas un effet de bord) — dossier au protocole habituel,
  avec ta mesure 21pt vs 16pt comme evidence. Tu postes.

## 3. note_size / mise en forme des notes Typst (défaut 4 de ta note
dev/uv_r2_colonne_spec.md)

Le fait : table.footer est un argument structurel Typst qu'aucune
règle show n'atteint ; tes notes sortaient à 9.5pt noir au corps du
texte, le gabarit demandant 8pt gris.

RECOMMANDATION : corps réduit APPLIQUÉ PAR DÉFAUT par spicy (comme
l'alignement décimal), pas un argument note_size : une note
subordonnée au contenu est une règle typographique, pas une
préférence. Implémentation : le finaliseur Typst enveloppe la note
de #text(0.85em, fill: luma(89))[...] (em relatif : suit le corps du
tableau) — et l'échappement de note étant maintenant corrigé, le
balisage passe. Opt-out documenté si un gabarit veut reprendre la
main (spicy.note_style = "none").

## Ordre suggéré si tu valides les trois

2 (petit, avec l'issue tinytable) -> 3 (petit) -> 1 (lot dédié).
