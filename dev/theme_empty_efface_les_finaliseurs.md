# `theme_empty()` détruit les finaliseurs de tinytable, et avec eux ses options Typst

*Constaté sur spicy 0.12.0.9000 et tinytable 0.17.0, en composant le rapport
HESAV Healthy Campus.*

## Symptôme

Les options Typst de tinytable n'ont aucun effet sur les tableaux produits par
`spicy`, alors qu'elles fonctionnent sur un `tt()` nu.

```r
options(tinytable_typst_multipage = TRUE)

tt(d)                   # -> le code Typst contient  breakable: true
table_categorical(d, …) # -> le code Typst contient  breakable: false
table_continuous(d, …)  # -> idem
```

Dans un rapport de 34 tableaux, l'option atteignait le seul tableau construit par
un `tt()` direct. Aucun message, aucun avertissement : l'option est simplement
sans effet.

## Cause

tinytable applique ses réglages de sortie par un finaliseur, empilé dans le slot
`lazy_finalize` de l'objet. `theme_typst()` y range la fonction qui, entre
autres, substitue `breakable: false` par `breakable: true` quand `multipage` est
demandé.

`theme_empty()` vide ce slot :

```r
theme_empty <- function(x, ...) {
    x@lazy_format <- list(); x@lazy_style <- list()
    x@lazy_prepare <- list(); x@lazy_finalize <- list()   # <- le finaliseur meurt ici
    return(x)
}
```

`spicy` l'appelle en cinq endroits :

- `R/table_categorical.R:1553`
- `R/table_categorical.R:2537`
- `R/table_continuous.R:1860`
- `R/table_continuous_lm_render.R:413`
- `R/regression_dispatch.R:504`

L'intention est bonne : partir d'un tableau sans habillage pour poser le style
maison. L'effet de bord ne l'est pas : tout ce que tinytable règle par
finaliseur, y compris ce qui relève du format de sortie et non de l'apparence,
disparaît avec.

## Ce que cela coûte

Ce n'est pas une préférence esthétique perdue. Le finaliseur Typst porte la
coupure des tableaux longs sur plusieurs pages. Sans lui, un tableau plus haut
qu'une page ne se coupe pas : selon le contexte, il bascule en bloc à la page
suivante en laissant jusqu'aux deux tiers d'une page blanche, ou bien ses
dernières lignes s'impriment les unes sur les autres en bas de page. Le second
cas perd des données à l'affichage, sans le signaler.

Le même mécanisme emportera toute option de sortie que tinytable ajoutera plus
tard par finaliseur : le défaut est structurel, pas limité à `multipage`.

## Pistes de correctif

Trois voies, de la plus locale à la plus propre.

1. Ne vider que ce qui concerne l'apparence : remplacer l'appel à
   `theme_empty()` par une remise à zéro de `lazy_format`, `lazy_style` et
   `lazy_prepare`, en laissant `lazy_finalize` intact. C'est le changement
   minimal, et il suffit à rétablir les options de format.
2. Sauvegarder `x@lazy_finalize` avant l'appel et le réinstaller après.
   Équivalent, plus explicite, mais à répéter aux cinq endroits.
3. Centraliser : les cinq appels font la même chose. Une fonction interne
   unique (`.tt_nu()`) éviterait qu'un sixième point d'appel réintroduise le
   défaut sans qu'on s'en aperçoive.

La première est préférable. Elle exprime l'intention réelle — retirer
l'habillage, pas les réglages de sortie.

## Test de non-régression

```r
options(tinytable_typst_multipage = TRUE)
x <- table_categorical(d, select = c("a", "b"), output = "tinytable")
expect_true(grepl("breakable: true", x@table_string, fixed = TRUE))
```

À doubler d'un test sur `table_continuous()` et sur la voie régression, les
trois fichiers étant touchés.

## Contournement en attendant

Poser la règle du côté du document plutôt que du tableau. En Typst :

```typst
#show figure: set block(breakable: true)
```

C'est ce que fait le rapport Healthy Campus (`reports/_style-rapport.typ`).
La règle ne vaut toutefois que si chaque tableau porte une légende `tbl-cap` :
sans elle, tinytable émet sa propre règle au niveau du document, où elle annule
celle-ci pour tout ce qui suit.
