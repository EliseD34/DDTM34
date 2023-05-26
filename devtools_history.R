# Depends
usethis::use_package('sf')
usethis::use_package('shiny')
usethis::use_package('dplyr')
usethis::use_package('leaflet')
usethis::use_package('rio')
usethis::use_package('gepaf')
usethis::use_package('classInt')
usethis::use_package('RJSONIO')
usethis::use_package('isoband')
usethis::use_package('RANN')
usethis::use_package('progress')
usethis::use_package('rlang')
usethis::use_package('stringr')

# Suggest
usethis::use_testthat()
usethis::use_pkgdown()

# Run tests unitaires
devtools::test()

# Vignettes
usethis::use_vignette(name = "A-Debuter", title = "Débuter avec le package {metric.osrm}")
usethis::use_vignette(name = "B-Preparer", title = "Préparer ses données avec le package {metric.osrm}")
usethis::use_vignette(name = "C-Calculer", title = "Calculer une matrice de temps et de distance avec le package {metric.osrm}")
usethis::use_vignette(name = "D-Equipements", title = "Résoudre les problématiques d'accès aux équipements avec le package {metric.osrm}")
usethis::use_vignette(name = "E-Isochrones", title = "Calculer des isochrones avec le package {metric.osrm}")
usethis::use_vignette(name = "F-Visualiser", title = "Visualiser les trajets sur une carte avec le package {metric.osrm}")

# Run to build the website

pkgdown::build_site()

# Generation CI
# usethis::use_gitlab_ci()

# Build ignore
usethis::use_build_ignore("devtools_history.R")
