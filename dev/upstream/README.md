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
| performance | [#937](https://github.com/easystats/performance/issues/937) check_singularity.lme() à plusieurs niveaux | 2026-08-21 | ouvert | — |
| tableone | [#114](https://github.com/kaz-yos/tableone/issues/114) SMD catégoriel faux sur niveaux disjoints | 2026-08-21 | ouvert | — |
| data.table | [#7887](https://github.com/Rdatatable/data.table/issues/7887) as.data.table() récursif sur Surv | 2026-08-23 | ouvert | — |
| covr | [#641](https://github.com/r-lib/covr/issues/641) package_coverage() et pkgload::load_all() | 2026-08-23 | ouvert | — |
| gt 1.3.0 | attribut headers= sur le nom brut de colonne | non envoyé | dossier prêt | [gt-headers-attribute-raw-column-name.md](gt-headers-attribute-raw-column-name.md) |

À la fermeture d'un rapport : noter la version corrigée dans l'en-tête du
dossier et retirer le contournement côté spicy dans le même commit.
