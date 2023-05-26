# metric.osrm 1.1.3

#### UPDATES

* Nouvelle table de passage en géographie des communes 2023 pour les communes françaises, identifiées par leur code commune Insee (tables de passage disponible entre 2017 et 2023).
* Faute de disponibilité d'une version officielle de la table Eurostat LAU 2021, la table de passage pour les communes étrangères demeure en géographie 2020 (seul millésime disponible dans le package).
* Précision dans la documentation des commandes pour pouvoir récupérer l'ensemble des codes communes Insee ou LAU d'un millésime donné avec leurs coordonnées : metric.osrm:::tablePassage20xx et metric.osrm:::tablePassageLAU20xx où xx est l'année du millésime désiré (20 uniquement pour les LAU).


#### BUGFIXES

* Fonction metricOsrmTable : correction d'un cas particulier de calcul d'accessibilité (présence de non-calculés dans le rayon de recherche)
* Fonction adresseToCoord : suppression d'une rustine Latin1-UTF8 pour l'encodage des réponses de l'API Adresse
* Fonction  metricOsrmIso : élargissement de la liste possible des options("osrm.profile") pris en compte par la fonction ("car","driving","routed-car", "bike","bicycle","cycling","routed-bike","walk","foot","walking","routed-foot"). Le paramètre osrm.profile y est utilisé pour déterminer la taille par défaut du territoire sur lequel la matrice d'échantillon de points est dessinée. Bien évidemment, pour que le calcul fasse sens, il faut, outre ce paramètre de profil, que le serveur OSRM utilisé fasse des calculs correspondant à ce mode de transport ; pour rappel, le serveur expérimental du SSPCloud propose uniquement des calculs de trajets routiers en voiture.        
* Correction/adaptation de divers tests.

# metric.osrm 1.1.2

#### BUG FIX

* Fonction adresseToCoord : amélioration de la gestion de l'encodage des réponses de l'API Adresse (api-adresse.data.gouv.fr).

# metric.osrm 1.1.1

#### BUG FIX

* Maintenance adaptative : le test de la geometry par la fonction methods::is() doit obligatoirement renvoyer une seule condition. Ce test est présent lorsqu'un objet sf est importé et lors du calcul des isochrones.

# metric.osrm 1.1.0

#### UPDATES

* Nouvelle table de passage en géographie des communes 2022 pour les communes françaises, identifiées par leur code commune Insee (tables de passage disponible entre 2017 et 2022).
* Nouvelle table de passage en géographie des communes 2020 pour les communes étrangères, identifiées par leur code LAU (seul le millésime 2020 est disponible).
* Nouvelle source (IGN) pour les coordonnées des chefs-lieux des arrondissements/secteurs de Paris, Lyon et Marseille. Déplacement manuel d'un chef-lieu (13202/13203) pour corriger son rapprochement au réseau routier.

#### BUG FIX

* Correction d'un bug bloquant lors d'un calcul en croisement cartésien et aller simple dans le cas où les identifiants de points sont identiques mais pas leurs coordonnées (entre millésime, chx, centroide ou pos différents).

# metric.osrm 1.0.2

#### UPDATE

* Mise à jour des vignettes Calculer l'accès aux équipements les plus proches avec le package {metric.osrm} et Calculer des isochrones ou des isodistances avec le package {metric.osrm} avec la mise à jour du package oceanis 1.8.4.

# metric.osrm 1.0.1

#### BUG FIX

* Correction d'un bug non bloquant : des doublons dans les ID ne permettaient pas le calcul de tous les couples attendus (le unique des points en entrée se fait dorénavant sur l'id et les coordonnées plutôt que sur l'id seul). Il est tout de même fortement conseillé d'utiliser des id uniques comme identifiants de points.

# metric.osrm 1.0.0

#### INFO

* Version de livraison du package metric.osrm.

#### BUG FIX

* Correction adaptative au passage de sf 1.0.0 pour la fonction st_make_grid_metric.R.

#### IMPROVEMENT

* Le fond d'habillage pour les cartes de visualisation des routes est rendu plus foncé pour une meilleure visibilité.

# metric.osrm 0.4.6

#### IMPROVEMENT

* Correction sur la documentation (reformulation, mise en forme, typo...)

# metric.osrm 0.4.5

#### BUG FIX

* Correction d'un bug touchant les isochrones lors d'un cas particulier si existence de couples non calculables dans la table.

# metric.osrm 0.4.4

#### IMPROVEMENT

* Remplacement des fonds de tuiles StadiaMap par OpenStreetMap-Fr pour l'affichage des tracés de routes simples et proportionnelles.

# metric.osrm 0.4.3

#### IMPROVEMENTS

* Ajout de barres de progression pour la géolocalisation des adresses.
* Ajout du paramètre interactive = FALSE (par défaut) à la fonction adresseToCoord.

# metric.osrm 0.4.2

#### BUG FIX

* Correction de bugs divers sur les isocourbes pour les DOM.

# metric.osrm 0.4.1

#### FEATURES

* Géographie des communes 2021 disponible pour la fonction codeComToCOord.
* Optimisation des temps de calculs pour metricOsrmTable.
