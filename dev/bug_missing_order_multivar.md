# `table_categorical()` : « (Missing) » remonte en tête dès la deuxième variable

*Constaté sur 0.12.0.9000, en composant le rapport HESAV Healthy Campus.*

## Symptôme

Quand plusieurs variables sont décrites dans le même tableau, la ligne
`(Missing)` se place correctement en fin de bloc pour la **première** variable à
non-réponses, puis **en tête** de bloc pour toutes les suivantes.

```r
table_categorical(d, select = c("age4e", "sexe", "enfant"), output = "data.frame")
#>       Variable           Level   n
#> 1  Classe d'âge 20 ans ou moins  24
#> ...
#> 5  Classe d'âge       (Missing)   8   <- correct
#> 6          Sexe       (Missing)   2   <- devrait être en 8
#> 7          Sexe    Sexe féminin 293
#> 8          Sexe   Sexe masculin  83
#> 9       Enfants       (Missing)   5   <- devrait être en 11
```

Avec une **seule** variable, le comportement est correct. Les tests isolés ne
voient donc rien : il faut au moins deux variables à non-réponses, et le défaut
n'apparaît qu'à partir de la seconde.

Indépendant de `ordered`, de l'attribut `label`, de `output` (`"data.frame"`,
`"long"` et `"tinytable"` sont tous touchés) et de la version installée.

## Cause

Branche sans `by`, `R/table_categorical.R`.

La construction par variable est juste — `lv_use` place bien
`missing_end` en dernier (l. 1339-1343), et `var_level_order` est capturé avant
`.add_missing_level()`. Mais les blocs sont ensuite triés sur un ordre de
niveaux **commun à tout le tableau** :

```r
all_level_order <- c(all_level_order, lv_use)          # l. 1351, accumulation
...
long_raw$level <- factor(long_raw$level,
                         levels = unique(all_level_order))   # l. 1391-1394
long_raw <- long_raw[order(long_raw$variable, long_raw$level,
                           method = "radix"), ]              # l. 1396-1400
```

`unique()` retient la **première** occurrence. `"(Missing)"` entre dans la
séquence à la position où la première variable à non-réponses l'a introduit, et
y reste : pour `age4e` il est en 5ᵉ position, donc avant `"Sexe féminin"` (6ᵉ) et
`"Oui"` (8ᵉ). Le tri sur `level` le fait remonter.

Le même mécanisme réordonne, plus discrètement, tout niveau **homonyme** partagé
par deux variables : l'ordre de la première variable citée s'impose aux autres.
Un `Oui/Non` suivi d'un `Non/Oui` sortira `Oui/Non` deux fois.

## Correctif

Le tri doit être **par variable**, pas global. Remplacer le facteur global par un
rang calculé dans chaque bloc — par exemple en mémorisant `lv_use` par variable
plutôt qu'en le concaténant :

```r
# à la place de : all_level_order <- c(all_level_order, lv_use)
level_order[[select_names[i]]] <- lv_use

# et à la place du facteur global :
long_raw$.rank <- unlist(lapply(split(long_raw$level, long_raw$variable, drop = TRUE),
                                \(lv) match(lv, level_order[[...]])))
long_raw <- long_raw[order(long_raw$variable, long_raw$.rank, method = "radix"), ]
```

La branche `levels_keep` reste inchangée : l'ordre y est fourni par
l'utilisateur·rice, donc volontairement global.

Test de non-régression à ajouter : deux variables à non-réponses **et** aux
niveaux disjoints, plus un cas à niveaux homonymes d'ordre inversé.

## Contournement en attendant

`levels_keep` accepte l'union des niveaux et n'agit alors pas comme un filtre :

```r
niveaux_union <- function(data, select) {
  lv <- unlist(lapply(data[select], \(x) levels(as.factor(x))), use.names = FALSE)
  c(setdiff(unique(lv), "(Missing)"), "(Missing)")
}
table_categorical(d, select = v, levels_keep = niveaux_union(d, v))
```

Vérifié cellule par cellule contre l'appel sans `levels_keep` : mêmes lignes,
mêmes effectifs, seul l'ordre change. Fonctionne aussi sous `by`. C'est ce que
fait le rapport Healthy Campus (`reports/rapport.qmd`, fonction `tab()`).
