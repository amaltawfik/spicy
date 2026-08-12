# Une gouttière de 5 pt s'invite dans les tableaux à groupes de colonnes

*Constaté sur spicy 0.12.0.9000 et tinytable 0.17.0, en composant le rapport
HESAV Healthy Campus. Ce n'est pas un défaut de spicy, mais il le traverse et
personne ne l'a décidé.*

## Le fait

`tinytable:::typst_add_gutter()` écrit `column-gutter: 5pt,` dans l'appel
`#table()` dès que le tableau porte des groupes de colonnes, c'est-à-dire dès
que `nrow(x@group_data_j) > 0`.

Or spicy emploie `group_tt(j = )` pour tous ses en-têtes multi-niveaux : les
en-têtes de groupe de `table_categorical(by = )`, le bandeau `95% CI` qui coiffe
`LL` et `UL`, les colonnes de moyennes par groupe de `table_continuous_lm()`.

Conséquence mesurée sur un rapport de 34 tableaux : **16 reçoivent la gouttière,
18 non**. La grille de colonnes du document n'est donc pas homogène, et l'écart
n'est pas anodin — un chiffre mesure 4,75 pt de large, quand le corridor qui le
sépare de son voisin en mesure 21 (deux insets de 8 pt plus 5 pt de gouttière)
dans un tableau et 16 dans le suivant.

## Pourquoi ce n'est pas rattrapable depuis la feuille de style

Une valeur passée en argument à `#table()` l'emporte sur toute règle `set` du
document : `#set table(column-gutter: 0pt)` reste sans effet. Vérifié.

C'est le même mécanisme que pour `stroke`, `align` et `fill`, que tinytable pose
également en arguments directs, et que pour `table.hline` et `table.footer`, qui
sont des arguments structurels qu'aucune règle `show` n'atteint. La géométrie du
tableau échappe entièrement au gabarit du document.

## Ce que spicy pourrait faire

Le paquet ne contrôle pas le code de tinytable, mais il contrôle **quand** il
appelle `group_tt()`, et il pourrait neutraliser la gouttière après coup, comme
il applique déjà d'autres finitions.

Deux voies, par ordre de préférence :

1. Signaler le problème en amont, chez tinytable : la gouttière devrait être une
   option, non un effet de bord de `group_tt()`. Un `column-gutter` codé en dur
   décide de la mise en pages à la place du document qui reçoit le tableau.
2. En attendant, neutraliser dans le finaliseur Typst de spicy, qui existe déjà,
   par une substitution sur la chaîne :

   ```r
   table@table_string <- sub("column-gutter: 5pt,\n", "", table@table_string,
                             fixed = TRUE)
   ```

   Le choix de la valeur revient alors au document, ce qui est sa place.

Dans les deux cas, la question à trancher est de savoir si spicy veut une grille
**homogène entre ses propres tableaux** — auquel cas la gouttière doit être
toujours présente ou toujours absente — ou s'il laisse la géométrie au document.
La situation actuelle ne fait ni l'un ni l'autre : elle dépend d'un détail de
structure que l'appelant n'a pas choisi.

## Comment le voir

```r
x <- table_continuous(d, select = c("a", "b"), by = "g", output = "tinytable")
y <- table_continuous(d, select = c("a", "b"), output = "tinytable")
grepl("column-gutter", x@table_string)   # TRUE  : a des groupes de colonnes
grepl("column-gutter", y@table_string)   # FALSE : n'en a pas
```

## Contournement en place

Aucun dans le rapport Healthy Campus : l'inégalité a été constatée et
documentée, mais l'inset a été ramené de 8 à 6 pt sur tous les tableaux, ce qui
réduit l'écart relatif sans le supprimer. Le corriger supposerait la
substitution ci-dessus, qui a sa place dans le paquet plutôt que dans chaque
projet.
