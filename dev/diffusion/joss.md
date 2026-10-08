# Dossier JOSS

Le papier est dans `paper/` : `paper.md` (1 180 mots, JOSS demande 750 à
1 750), `paper.bib` généré depuis `master.bib` (tag `projet-spicy`, 16
entrées, toutes les citations résolues), `BIBLIOGRAPHIE.md` (contrat du
dépôt references). Le dossier est hors tarball (`^paper$` dans
`.Rbuildignore`).

## Portée et impact : pas de soumission maintenant (2026-10-09)

Page relue le 2026-10-09 :
<https://joss.readthedocs.io/en/latest/submitting.html#scope-and-significance>.
JOSS juge deux choses. L'effort scientifique (« substantial scholarly
effort ») et l'impact sur la recherche (« research impact »). Depuis
2025 le second compte autant que le premier : les éditeurs demandent
des preuves que le logiciel est utilisé au-delà de son auteur.

État de spicy, critère par critère.

- Effort : rempli. Trois ans de développement, versions CRAN depuis
  0.12.0, suite de près de 19 000 expectations, conventions validées
  contre SPSS, Stata et les packages de référence, site documenté.
- Fonctionnalité de recherche : rempli. Le package fait un travail de
  recherche (tables, modèles, codebook), pas un utilitaire.
- Impact : NON rempli. Aucune publication ne cite spicy, aucun usage
  hors HESAV n'est documenté, aucune contribution externe. Les 809
  téléchargements mensuels ne sont pas une preuve d'usage (les miroirs
  et les CI en font une part). L'énoncé « research impact statement »
  du papier ne peut aujourd'hui citer que l'enseignement et le soutien
  méthodologique de l'auteur lui-même, ce que JOSS lit comme un usage
  interne.

Décision proposée : ne pas soumettre maintenant. Un rejet pour défaut
d'impact est public (issue « pre-review » visible) et ferme la porte
pour plusieurs mois.

Ce qu'il faut construire d'ici six à douze mois, dans l'ordre d'effet :

1. Au moins une publication qui cite spicy par son `citation("spicy")`
   (DoMiRéFAS, Healthy Campus ou toute étude dont les tables viennent
   du package). Une citation vérifiable vaut plus que tout le reste.
2. Des traces d'usage externe : issues ou questions d'utilisateurs hors
   HESAV, mention dans un cours ou un tutoriel tiers, dépendance
   inverse sur CRAN.
3. Une contribution externe, même mineure (issue avec correctif, PR).
4. Les codebooks livrés à un dépôt de données (FORS, Zenodo) avec
   mention de l'outil.

Reprendre ce dossier à la 0.14.0 et récrire le « research impact
statement » avec ces éléments nommés. Sans au moins le point 1, ne pas
soumettre.

## À confirmer par Amal avant soumission

1. **Affiliation et ROR.** `04j47fz63` vient de DESCRIPTION ; vérifier
   qu'il désigne bien HESAV (ou HES-SO) sur <https://ror.org>.
2. **Research impact statement.** Le texte dit : enseignement de la
   statistique à HESAV, soutien méthodologique aux projets de recherche
   de l'école, tables et codebooks d'enquêtes en cours, téléchargements
   CRAN « plusieurs centaines par mois » (809 sur les 30 derniers jours
   au 8 octobre 2026). JOSS demande des usages concrets : ajouter un ou
   deux projets nommés (DoMiRéFAS, Healthy Campus) si leur mention est
   possible, et toute publication qui a utilisé spicy.
3. **AI usage disclosure.** JOSS l'exige. Le paragraphe dit que la
   conception et la maintenance sont de l'auteur, que l'implémentation,
   les tests et la documentation ont été écrits avec l'assistance de
   Claude sous ses spécifications et sa relecture, que chaque décision
   est consignée dans le dépôt (`dev/decisions/`), et que le papier a
   été rédigé avec la même assistance puis révisé par l'auteur. Relire
   mot à mot : c'est la phrase la plus lue par les relecteurs.
4. **Date** du front matter : celle de la soumission.
5. **Remerciements** : le relecteur de l'issue #8 (R core) n'est pas
   nommé ; le nommer ou retirer la phrase.
6. **Six mois d'historique public** : JOSS les demande ; le dépôt
   GitHub et les versions CRAN les couvrent.

## Prérequis JOSS, état

- Licence OSI : MIT, en place.
- Dépôt public, issues et PR ouvertes : oui.
- Tests et documentation : suite de 18 800 expectations, couverture
  100 % visée, site pkgdown.
- Community guidelines : `CONTRIBUTING.md` et `CODE_OF_CONDUCT.md`
  ajoutés le 2026-10-08.
- Version taguée : la soumission se fait sur une version publiée ; la
  0.14.0 (CRAN) sera le bon moment, avec le tag `v0.14.0`.
- Zenodo : à l'acceptation, archiver la version taguée (Zenodo
  s'intègre au dépôt GitHub en deux clics) et donner le DOI à JOSS.

## Soumission

1. Relire `paper/paper.md` ; régénérer la bibliographie après toute
   citation nouvelle : `Rscript tools/make_project.R projet-spicy
   C:/Users/at/Documents/R/Packages/spicy/paper --bib paper.bib` depuis
   `~/Documents/references` (ajouter d'abord l'entrée à `master.bib`).
2. Contrôler le rendu : <https://joss.readthedocs.io/en/latest/paper.html>
   décrit la prévisualisation (action GitHub `openjournals/openjournals-draft-action`,
   ou le service web de JOSS).
3. Soumettre sur <https://joss.theoj.org/papers/new> : URL du dépôt,
   branche `main`, chemin `paper/paper.md`.
4. Répondre aux relecteurs dans l'issue de revue (délai attendu : deux
   semaines par réponse).

## Points relevés par la génération de la bibliographie

- Les capitales de `apa2020publication` et `arelbundock2024interpret`
  sont protégées depuis le 2026-10-09 (`master.bib` 88eecf6, `paper.bib`
  régénéré).
- Le champ `note` (« R package version x ») disparaît en APA ; un champ
  `version` l'afficherait. À décider.
- Le PDF ICPSR du magasin est la 4e édition (2009) ; la 6e, citée, est en
  accès libre à l'URL de l'entrée.
