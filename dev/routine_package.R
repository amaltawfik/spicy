library(devtools)
load_all()
document()

# 1. Mise à jour du README
devtools::build_readme()

# 2. Mise à jour de la documentation automatique
devtools::document()


devtools::test()
styler::style_pkg() # reformate tout le code R du package
lintr::lint_package() # lance le linting et affiche les recommandations


# 3. Vérification complète du package
devtools::check()


urlchecker::url_check()
# si besoin :
urlchecker::url_update() # remplace automatiquement quand c’est possible


# 4. (Optionnel) Rebuild du site web
pkgdown::build_site()

# Avant release CRAN
devtools::spell_check()

devtools::check(remote = TRUE, manual = TRUE)

# Envoie le package à Win-builder, un service de R Core qui compile et teste les packages sur Windows.
devtools::check_win_release() # R actuel sur Windows (important)
devtools::check_win_devel() # R de développement (prévoir la future version)
devtools::check_win_oldrelease() # R précédent (compatibilité rétro)

rhub::rhub_check(
  platforms = c("windows", "macos", "ubuntu-release")
)


# POUR SOUMISSION AU CRAN


# https://r-pkgs.org/release.html#sec-release-cran-comments

devtools::release() # Si tu veux publier sur CRAN


usethis::use_github_release()
