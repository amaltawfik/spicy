# CRAN + GitHub Release Workflow — spicy
# Ce script guide et automatise les principales étapes d'une release CRAN :
# - vérification et soumission au CRAN
# - tag et publication GitHub
# - mise à jour du site pkgdown
# - passage à la version de développement
#
# À exécuter depuis le répertoire racine du package.

# Charger les packages nécessaires -----------------
library(usethis)
library(devtools)
library(pkgdown)

# Préparation avant la soumission CRAN----

# Mettre à jour la version selon le type de changement :
# "major" → 1.x.y, "minor" → x.1.y, "patch" → x.y.1
use_version("patch") # par exemple, passe de 0.2.1 → 0.2.2

# Générer la documentation
devtools::document()

# Vérifier localement le package
devtools::check()

# Vérifier sur Windows (optionnel mais conseillé)
# Vérifie via R-hub (ou winbuilder)
# Envoie le package à Win-builder, un service de R Core qui compile et teste les packages sur Windows.
devtools::check_win_release() # R actuel sur Windows (important)
devtools::check_win_devel() # R de développement (prévoir la future version)
devtools::check_win_oldrelease() # R précédent (compatibilité rétro)

rhub::rhub_check(
  platforms = c("windows", "macos", "ubuntu-release")
)

# Soumettre au CRAN (interactif)
devtools::release()

# Après acceptation par le CRAN ----
# À exécuter uniquement une fois la validation CRAN reçue

# Créer le tag GitHub (par ex. v0.2.2)
usethis::use_github_tag()

# Créer la release GitHub à partir du tag et du NEWS.md
usethis::use_github_release()

# (Optionnel) Ouvrir la page de release dans le navigateur :
# browseURL("https://github.com/amaltawfik/spicy/releases")

# Mise à jour du site pkgdown ----

# Reconstruire et pousser le site
pkgdown::build_site()
system("git add docs/")
system("git commit -am 'Update pkgdown site for CRAN release'")
system("git push")

# Passer en version de développement ----

# Incrémente automatiquement la version (ex. 0.2.2 → 0.2.2.9000)
usethis::use_dev_version()

# Commit et push
system("git commit -am 'Start development version'")
system("git push")

# FIN DU WORKFLOW
