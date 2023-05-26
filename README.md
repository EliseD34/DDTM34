
# Metric-OSRM-package (version 1.1.3)

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![minimum R version](https://img.shields.io/badge/R%3E%3D-3.4-blue.svg)](https://git.lab.sspcloud.fr/metric-osrm/metric-osrm-package/blob/master/DESCRIPTION)
[![pipeline status](https://git.lab.sspcloud.fr/metric-osrm/metric-osrm-package/badges/master/pipeline.svg)](https://git.lab.sspcloud.fr/metric-osrm/metric-osrm-package/-/commits/master)
[![coverage report](https://git.lab.sspcloud.fr/metric-osrm/metric-osrm-package/badges/master/coverage.svg)](https://metric-osrm.pages.lab.sspcloud.fr/metric-osrm-package/coverage.html)

<!-- badges: end -->

# :package: {metric.osrm} <img src="man/figures/canvas_package.png" width=110 align="right"/>

Le package `metric.osrm` a pour vocation de __faciliter l'interrogation d'un serveur de routage de type OSRM__ (Open Source Routing Machine). Il propose :

* Des fonctions de __préparation et conversion des données en entrée__ : transformation de codes communes (codes INSEE ou LAU) ou d'adresses en coordonnées, conversion entre différents formats d'objets (sf, sp ou data.frame) ;

* Des fonctions de __calcul de matrice de temps et de distances__ entre sources et destinations (en face à face, par croisement, aller simple ou aller-retour, __accès au(x) point(s) d'intérêt le(s) plus proche(s)__ dans un rayon de x km, ...) et de __calcul d'isocourbes rapides__ (isochrones, isodistances) ;

* Des fonctions de calcul d'__indicateurs__ associés et la __visualisation__ cartographique de certains résultats.

La documentation complète du package `metric.osrm` est disponible sur [ce site](https://metric-osrm.pages.lab.sspcloud.fr/metric-osrm-package). Toutes les fonctions sont décrites dans la rubrique "Reference". Pour découvrir toutes les fonctionnalités du package, nous vous conseillons de lire attentivement les différents "articles" de cette documentation. Pour bien commencer : [Débuter avec le package {metric.osrm}](https://metric-osrm.pages.lab.sspcloud.fr/metric-osrm-package/articles/A-Debuter.html).

⚠️ Le package `metric.osrm` a été exclusivement testé sur un serveur OSRM déployé et mis à disposition à titre expérimental sur le SSPCLoud. Ce serveur (OSRM v5.26.0) exploite actuellement des données routières OpenStreetMap datant du 25/02/2022. Il permet uniquement des calculs de trajets routiers en voiture (profil car.lua par défaut).

⚠️ Si le package `metric.osrm` offre à l'utilisateur la possibilité de définir son propre serveur de routage, il a toutefois été développé en ayant pour cible un champ géographique précis (France entière + régions européennes frontalières de la métropole, cf. article ["Débuter avec {metric.osrm}"](https://metric-osrm.pages.lab.sspcloud.fr/metric-osrm-package/articles/A-Debuter.html)) dont les spécificités font l'objet de traitements au sein du code de certaines fonctions (traitement des îles et autres réseaux routiers "isolés" lors des calculs de trajets par exemple). Son fonctionnement sur un serveur différent n'est donc pas garanti.

Plus d'informations à propos d'OSRM sur ce site : http://project-osrm.org/

Des tutoriels pour déployer son propre serveur OSRM sont disponibles sur [le github du projet](https://github.com/Project-OSRM/osrm-backend), le plus simple étant d'utiliser [une image docker](https://github.com/Project-OSRM/osrm-backend#using-docker). Le fichier source des fonds routiers (.pbf) et le fichier de paramétrage utilisé (car.lua) sont à disposition des intéressés sur demande.

## Installation et chargement du package

Installation sur poste de travail :

Pour R >= 3.4.0,

``` r
install.packages("remotes")

remotes::install_gitlab(repo = 'metric-osrm/metric-osrm-package',
                        host = 'git.lab.sspcloud.fr')
						
library(metric.osrm)
```

Pour la mise à jour du package, il suffit de le télécharger de la même façon et de redémarrer votre Rstudio ou de faire un Session/Restart R.

Avant d'utiliser le package `metric.osrm`, vous devrez spécifier l'URL d'un serveur de calcul OSRM et un profil.

``` r 
# On précise l'adresse d'un serveur, ici celle du serveur expérimental déployé sur la plateforme du SSPCloud

options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/") 

# On précise le profil de routage, ici "driving" car le serveur SSPCloud ne propose que du calcul de trajet routier en voiture

options(osrm.profile = "driving") 

```

## Besoin d'aide

Que vous ayez une question ou une suggestion d'amélioration du package, rédigez un "issue" sur cette [page](https://git.lab.sspcloud.fr/metric-osrm/metric-osrm-package/issues). 

Si vous rencontrez un bug, merci de le faire remonter au plus vite avec un exemple reproductible en publiant également un [issue](https://git.lab.sspcloud.fr/metric-osrm/metric-osrm-package/issues).


Le code source du package est disponible [ici](https://git.lab.sspcloud.fr/metric-osrm/metric-osrm-package).

## .gitlab-ci.yml

Le fichier .gitlab-ci.yml décrit les étapes de construction du projet :
- vérifie les dépendances et le code du package ;
- exécute les tests unitaires ;
- publie la [couverture de tests](https://metric-osrm.pages.lab.sspcloud.fr/metric-osrm-package/coverage.html) ;
- publie le [site pour la documentation](https://metric-osrm.pages.lab.sspcloud.fr/metric-osrm-package) du package.
