# ADR 2026-11-19 — Les objets qui héritent d'une classe prise en charge sans être ce modèle

**Statut** : approuvé par Amal le 2026-11-19. Mis en œuvre le même jour, branche `lot-inherited-subclasses`. Rupture assumée (version < 1).

## Contexte

`table_regression()` choisit sa méthode `as_regression_frame()` sur la classe de l'objet. Un objet dont la classe hérite d'une classe prise en charge reçoit donc la méthode du parent, même quand il décrit un autre modèle. `survival::clogit()` renvoie un objet de classes `clogit` et `coxph` : le tableau sortait sous le titre « Cox proportional hazards regression », avec une ligne « N events » et une note de concordance, pour une régression logistique conditionnelle.

## Balayage

Mesuré le 2026-11-19 sur `main` (7ceb7996). Pour chaque objet : ce que `table_regression()` en faisait, et, quand un tableau sortait, la comparaison de ses coefficients, erreurs standard et p avec `summary()`.

| Objet | Classes | Avant | Nombres comparés à `summary()` | Verdict |
|---|---|---|---|---|
| `survival::clogit()` | `clogit`, `coxph` | tableau de Cox | égaux | refus |
| `coxph` multi-états | `coxphms`, `coxph` | tableau de Cox, une ligne par transition. Colonnes RMST et différence de risque : erreur brute de survival (`basehaz`) | égaux (erreur standard robuste) | refus |
| `coxph` à fragilité creuse, `frailty(inst)` | `coxph.penal`, `coxph` | tableau, colonnes RMST et différence de risque calculées | égaux pour `age` | tableau gardé, colonnes RMST et différence de risque refusées |
| `coxph` à fragilité non creuse, `frailty(inst, sparse = FALSE)` | `coxph.penal`, `coxph` | tableau avec `age` et 18 lignes `gamma:k` | `summary()` donne `age` et un test du terme de fragilité | refus |
| `coxph` à terme `pspline(age)` | `coxph.penal`, `coxph` | tableau avec 12 lignes `ps(age)k` | `summary()` donne deux tests, linéaire et non linéaire | refus |
| `coxph` à terme `ridge(age, sex)` | `coxph.penal`, `coxph` | tableau | égaux | gardé |
| `lm(cbind(y1, y2) ~ x)` | `mlm`, `lm` | erreur brute (`get1index`) | | refus |
| `aov()` | `aov`, `lm` | erreur brute (`summary()` de `aov` n'a pas de R²) | | refus |
| `aov()` avec `Error()` | `aovlist`, `listof` | refus classé existant (`spicy_unsupported`) | | inchangé |
| `quantreg::rq()` à plusieurs `tau` | `rqs` | refus classé existant | | inchangé |
| estimation multiple de fixest | `fixest_multi` | refus classé existant | | inchangé |
| `nlme::nlme()` | `nlme`, `lme` | tableau « Linear mixed-effects regression », ICC | erreurs standard de 1,8 % plus petites que `summary()` (2,417 contre 2,462 pour `Asym`), statistiques et p égaux | refus |
| `nlme::gnls()` | `gnls`, `gls` | tableau « Generalized least squares » | égaux | refus |
| `survey::svysurvreg()` | `svysurvreg`, `survreg` | tableau « Weibull AFT regression », note « Wald asymptotic (z) », aucun plan | égaux (erreurs standard du plan) | refus |
| `rms::Rq()` | `Rq`, `rms`, `rq` | erreur brute de rms | | refus |
| `rms::Gls()` | `Gls`, `rms`, `gls` | erreur brute de rms | | refus |
| `rms::psm()` | `psm`, `rms`, `survreg` | refus classé existant | | inchangé |
| `rms::orm()` | `orm`, `rms` | refus classé existant | | inchangé |
| `lme4::nlmer()` | `nlmerMod` | refus classé existant | | inchangé |
| `glm()`, famille `MASS::negative.binomial(2)` | `glm`, `lm` | tableau | égaux | gardé |
| `glm()`, famille `statmod::tweedie()` | `glm`, `lm` | tableau | égaux | gardé |
| `svyglm()` sur plan à poids répliqués | `svrepglm`, `svyglm`, `glm`, `lm` | tableau du plan | méthode `svyglm` prévue pour lui | gardé |
| `sampleSelection::heckit()` | `selection` | tableau en deux étapes | méthode `selection` prévue pour lui | gardé |

## Règle

- Tableau juste, mêmes nombres que `summary()` et habillage correct : on ne touche à rien.
- Nombres justes mais habillage faux, lignes sans équivalent dans `summary()`, ou erreur brute : refus classé `spicy_unsupported_class`. Le message nomme la classe de l'objet et dit en une phrase ce qu'il est. Quand un modèle pris en charge existe pour la même chose, une seconde phrase le donne.
- Un `coxph` à fragilité dont la table des coefficients est juste garde son tableau. Seules les colonnes RMST et différence de risque sont refusées (`spicy_invalid_input`), parce que la g-computation standardise sur les coefficients fixes et le risque de base et ignorerait l'effet aléatoire.

## Ce qui est refusé

`clogit`, `coxphms`, `coxph.penal` dès qu'un terme pénalisé met ses coefficients de base ou de grappe dans `coef()` (`pspline()`, `frailty(sparse = FALSE)`), `svysurvreg`, `mlm`, `aov`, `nlme`, `gnls`, `Rq` et `Gls` de rms. Les colonnes RMST et différence de risque d'un `coxph` à fragilité creuse.

Le contrôle est une fonction, `.refuse_inherited_subclass()` dans `R/regression_validate.R`. `table_regression()` l'appelle avant de résoudre le moindre argument, par `.refuse_out_of_scope_model()`. Les méthodes des parents (`lm`, `lme`, `gls`, `rq`, `coxph`, `survreg`) l'appellent aussi, si bien que `as_regression_frame()` et `table_regression_uv()` refusent de même.

## Ce qui est laissé

Le `coxph` à fragilité creuse et le `coxph` à terme `ridge()`, dont les lignes sont celles de `summary()`. Les `glm()` à famille d'un autre paquet (`MASS::negative.binomial()`, `statmod::tweedie()`). Les objets déjà refusés par une erreur classée (`aovlist`, `rqs`, `fixest_multi`, `psm`, `orm`, `nlmerMod`) gardent leur message.

## Limites

- Le support de `clogit` serait une fonctionnalité à part : titre, lignes d'ajustement et note propres à la régression logistique conditionnelle. Le message de refus ne l'annonce pas.
- Les colonnes RMST et différence de risque d'un `coxph` à terme `ridge()` restent calculées. La règle ne tranche pas ce cas, qui n'a pas d'effet aléatoire.
- `aov()` pourrait être rendu comme un `lm()` en lisant `summary.lm()`. Le refus renvoie à `lm()`, qui donne le même tableau.
- La liste suit les classes repérées dans les paquets des Suggests. Une sous-classe d'un autre paquet passe encore par la méthode de son parent.

## Garde

`tests/testthat/test-inherited_subclasses.R` : un test par objet refusé (le refus, sa classe de condition, par `table_regression()` et par `as_regression_frame()`), un test par objet gardé (égalité avec `summary()` des estimations, erreurs standard et p), et le refus des colonnes RMST et différence de risque d'un `coxph` à fragilité creuse.
