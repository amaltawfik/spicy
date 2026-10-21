# Rapports upstream

Un fichier par rapport envoyé ou déposé, en-tête normalisé (cible, canal,
envoyé, statut, côté spicy), corps conservé tel qu'envoyé. Les brouillons
non envoyés restent dans `dev/` sous le motif ignoré `*_draft.md` ; ils
entrent ici le jour de l'envoi. Les rapports sans dossier local sont
listés pour mémoire.

| Cible | Rapport | Date | Statut (2026-09-06) | Dossier |
|---|---|---|---|---|
| survey 4.5 | courriel au mainteneur : deff() sur svyby, svychisq() sur plans répliqués | 2026-08-21 | envoyé, sans réponse | [survey-4.5-deff-svyby-svychisq-report.md](survey-4.5-deff-svyby-svychisq-report.md) |
| quantreg 6.1 | courriel au mainteneur : nobs() absente, cluster bootstrap | 2026-07-23 | envoyé, sans réponse | [quantreg-nobs-bic-boot-cluster.md](quantreg-nobs-bic-boot-cluster.md) |
| DescTools | [#184](https://github.com/AndriSignorell/DescTools/issues/184) SomersDelta ASE | 2026-07-23 | ouvert | [desctools-somersdelta-row-ase.md](desctools-somersdelta-row-ase.md) |
| parameters | [#1243](https://github.com/easystats/parameters/issues/1243) standardize_parameters posthoc, brmsfit | 2026-07-22 | ouvert | [parameters-standardize-posthoc-brmsfit.md](parameters-standardize-posthoc-brmsfit.md) |
| merDeriv | [#10](https://github.com/nctingwang/merDeriv/issues/10) vcov(full = TRUE) cubique | 2026-07-22 | ouvert | [merderiv-vcov-full-cubic-cost.md](merderiv-vcov-full-cubic-cost.md) |
| pandoc | [#11772](https://github.com/jgm/pandoc/issues/11772) raw openxml avec xmlns supprimé | 2026-07-23 | fermé « not planned » 2026-07-24 | [pandoc-docx-raw-openxml-xmlns.md](pandoc-docx-raw-openxml-xmlns.md) |
| R-core | [Bugzilla #19128](https://bugs.r-project.org/show_bug.cgi?id=19128) drop1() sur glm(y = FALSE) avec cbind() | 2026-08-06 | déposé | [r-core-drop1-glm-cbind-y-false.md](r-core-drop1-glm-cbind-y-false.md) |
| tinytable | [#674](https://github.com/vincentarelbundock/tinytable/issues/674) gouttière Typst de group_tt(j) | 2026-08-13 | corrigé dans tinytable 0.18.0 ; garde-fou conservé, décision dans R/tt_theme.R | [tinytable-typst-column-gutter.md](tinytable-typst-column-gutter.md) |
| cardx | [#352](https://github.com/pharmaverse/cardx/issues/352) min/max ignorent le plan de sondage | 2026-08-15 | corrigé upstream 2026-08-16 | — |
| performance | [#937](https://github.com/easystats/performance/issues/937) check_singularity.lme() à plusieurs niveaux | 2026-08-21 | corrigé upstream 2026-09-27, PR #953, performance 0.18.2.3 (dev) | — |
| tableone | [#114](https://github.com/kaz-yos/tableone/issues/114) SMD catégoriel faux sur niveaux disjoints | 2026-08-21 | ouvert | — |
| data.table | [#7887](https://github.com/Rdatatable/data.table/issues/7887) as.data.table() récursif sur Surv | 2026-08-23 | ouvert | — |
| covr | [#641](https://github.com/r-lib/covr/issues/641) package_coverage() et pkgload::load_all() | 2026-08-23 | ouvert | — |
| ordinal 2026.7-26 | [#76](https://github.com/runehaubo/ordinal/issues/76) clmm() permute les liens cloglog et loglog (switch C), patch de 8 lignes testé | 2026-10-09 | ouvert | [ordinal-clmm-cloglog-loglog-links.md](ordinal-clmm-cloglog-loglog-links.md) |
| sandwich 3.1-3 | courriel au mainteneur : bread.gam() omet la dispersion, sandwich d'un gam déflaté de φ² pour les familles à dispersion libre, patch testé | 2026-10-10 | corrigé en dev le 2026-10-12 (codeberg #1, PR #2) ; vcovCL gaussien passe à HC1 par défaut, oracles à surveiller | [sandwich-bread-gam-dispersion.md](sandwich-bread-gam-dispersion.md) |
| mgcv 1.9-4 | courriel au mainteneur : residuals(type = "working") de bam() = résidus de déviance, de gam() à lien non canonique = résidu de Newton (y − μ)/(mu.eta·α), $weights = poids de Fisher | 2026-10-10 | envoyé, sans réponse | [mgcv-working-residuals.md](mgcv-working-residuals.md) |
| gt 1.3.0 | attribut headers= sur le nom brut de colonne | non envoyé | dossier prêt | [gt-headers-attribute-raw-column-name.md](gt-headers-attribute-raw-column-name.md) |
| survey 4.5 | courriel au mainteneur : calibrate() plante sur des poids répliqués non compressés, trimWeights() ne rogne pas les poids d'un plan répliqué (estimation non rognée, erreur type rognée), svytotal() plante après rognage avec fpc ; deux correctifs d'une ligne testés | 2026-10-21 | envoyé | [survey-4.5-replicate-designs-report.md](survey-4.5-replicate-designs-report.md) |
| surveytable 0.10.0 | [#2](https://github.com/CDCgov/surveytable/issues/2) svyciprop_adjusted() rend NaN à une proportion de 1 (la norme NCHS traite 0 et son complément pareil), tab() remplace un intervalle valide par [p, p] quand l'erreur type est nulle ; deux correctifs d'une ligne testés | 2026-10-21 | ouvert | [surveytable-kg-bounds-zero-width.md](surveytable-kg-bounds-zero-width.md) |
| contingencytables 3.1.0 | [#62](https://github.com/ocbe-uio/contingencytables/issues/62) Fisher_midP_test_2x2() et Fisher_exact_test_2x2() manquent les égalités de tables (comparaison stricte), correctif de deux lignes testé | 2026-10-20 | PR [#63](https://github.com/ocbe-uio/contingencytables/pull/63) approuvée et fusionnée le 2026-10-21, issue fermée | [contingencytables-fisher-ties.md](contingencytables-fisher-ties.md) |
| contingencytables 3.1.0 | [#68](https://github.com/ocbe-uio/contingencytables/issues/68) une cellule vide compte pour deux observations (1:0) dans linear_by_linear_test_rxc() et quatre autres fonctions ; 6 730 tableaux 2×2 sur 6 750 faux ; correctif seq_len() testé sur 17 775 tableaux | 2026-10-21 | ouvert | [contingencytables-empty-cells.md](contingencytables-empty-cells.md) |
| contingencytables 3.1.0 | [#69](https://github.com/ocbe-uio/contingencytables/issues/69) Exact_cond_midP_tests_rxc() donne des p unilatéraux (queue supérieure) pour les tests exacts linéaire et de Jonckheere-Terpstra ; un tableau et son miroir donnent 0,9999 et 0,0003 ; correctif par la statistique au carré, vérifié par énumération sur 955 tableaux et contre coin | 2026-10-21 | ouvert | [contingencytables-exact-rxc-one-sided.md](contingencytables-exact-rxc-one-sided.md) |
| contingencytables 3.1.0 | [#70](https://github.com/ocbe-uio/contingencytables/issues/70) statistic = "LR" : une cellule vide remplacée par 1 dans la statistique, et une énumération de tableaux impossibles qui donne p = 1 (le cas de #60) ; correctif par-dessus la PR #63, vérifié par énumération sur 17 775 tableaux | 2026-10-21 | ouvert | [contingencytables-lr-statistic.md](contingencytables-lr-statistic.md) |
| contingencytables 3.1.0 | [#71](https://github.com/ocbe-uio/contingencytables/issues/71) Exact_cond_midP_tests_rxc() manque les égalités entre tableaux (comparaison stricte, le défaut de #62 dans la fonction r×c) : p = 0,33 au lieu de 1 ; même correctif que la PR #63, vérifié par énumération exacte sans tolérance | 2026-10-21 | ouvert | [contingencytables-exact-rxc-ties.md](contingencytables-exact-rxc-ties.md) |

À la fermeture d'un rapport : noter la version corrigée dans l'en-tête du
dossier et retirer le contournement côté spicy dans le même commit.
