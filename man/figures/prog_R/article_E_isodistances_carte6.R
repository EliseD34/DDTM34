iso <- metricOsrmIso(loc = dt_maternites,
                     breaks = c(10,20,40,60), # kilomètres
                     exclude = NULL,
                     res = 240,
                     returnclass = "sf",
                     fusion = TRUE,
                     courbes = "isodistances")

# convertion en projection locale UTM40S (EPSG 2975)
# des isochrones autour de la maternité de Saint-Paul
iso1 <- convertTo(from = iso[[1]],
                  toEpsg = 2975)

# création de l'objet sf des maternités en projection locale UTM40S (EPSG 2975)
sf_maternite <- convertTo(from = dt_maternites,
                          fromEpsg = 4326,
                          to="sf",
                          toEpsg = 2975)

# on sélectionne toutes les maternités de la Réunion
sf_maternite$code <- c("97407","97410","97411","97411","97415","97416","97422")
sf_maternite <- sf_maternite[,c("id","code","geometry")]

# on récupère la bbox de La Réunion (les coordonnées du rectangle autour de La Réunion).
bbox <- sf::st_bbox(reg04)

# création d'un fond de mer à partir de la bbox. On élargi les limites du rectangle.
pol <- matrix(c(bbox$xmin-8000,bbox$ymin-8000,
                bbox$xmin-8000,bbox$ymax+13000,
                bbox$xmax+14000,bbox$ymax+13000,
                bbox$xmax+14000,bbox$ymin-8000,
                bbox$xmin-8000,bbox$ymin-8000),
              ncol=2,
              byrow=TRUE)

mer <- sf::st_sf(geometry = sf::st_sfc(sf::st_geometry(sf::st_polygon(list(pol))),
                                       crs=2975))

# En utilisant les fonctions plot_xxx du package oceanis,
# il est possible de personnaliser les fonds à représenter.
# Ici, nous allons utiliser la fonction plot_typo_symboles
# pour représenter les maternités sous forme de points.

# Pour personnaliser les fonds, on ajoute des colonnes à l'objet sf
# (COL, BORDER, EPAISSSEUR, FONT).

# personnalisation du fond de mer
mer$COL <- "#AFC9E0"
mer$BORDER <- "#AFC9E0"

# personnalisation des isochrones avec une palette de couleurs
iso1$COL <- oceanis::recup_palette(stylePalette = "defaut", nbNeg = 0, nbPos = 5)[[1]][1:4]

# personnalisation des fonds d'habillage
# pour gérer la superposition des couches sur la carte,
# on crée un 1er fond communal avec une couleur de fond qui sera sous les isochrones
com_reg04_fond <- com_reg04
com_reg04_fond$COL <- oceanis::recup_palette(stylePalette = "defaut", nbNeg = 0, nbPos = 5)[[1]][5]
# puis un 2ème fond communal pour la couleur de la bordure qui sera au-dessus
# des isochrones
com_reg04_border <- com_reg04
com_reg04_border$BORDER <- "grey"

reg04$BORDER <- "darkgrey"
reg04$EPAISSEUR <- 4

# préparation des étiquettes : on écrit uniquement Saint-Denis comme étiquette
etiquettes <- oceanis::coordonnees_etiquettes(fondMaille = com_reg04,
                                              listeCode = c("97411"))
etiquettes$FONT <- 2

# appel de la fonction plot_typo_symboles d'oceanis pour créer la carte de base
oceanis::plot_typo_symboles(fondPoints = sf_maternite,
                            # l'ordre des fonds est important : le 1er fond est dessous,
                            # le dernier au-dessus.
                            listFonds = list(mer,com_reg04_fond,iso1,
                                             com_reg04_border,reg04),
                            emprise = "974",
                            couleurs = rep("blue",7), # couleur du point
                            types = rep(1,7), # type de point (voir graphics::pch)
                            tailles = rep(2,7), # taille du point
                            # position de la légende en coordonnées locales métriques
                            # (EPSG 2975). Il s'agit juste de la légende du point et non
                            # des classes de temps que nous ajouterons après.
                            xLeg = 373000,
                            yLeg = 7694000,
                            titreCarte = "Carte d'isodistances autour des maternités de La Réunion",
                            sourceCarte = "Insee, distancier Metric-OSRM, © les contributeurs d’OpenStreetMap et du projet OSRM",
                            etiquettes = etiquettes,
                            labels = c("Maternités de La Réunion"),
                            # limite de la carte à afficher
                            xlim = c(bbox$xmin+2000,bbox$xmax+2000),
                            ylim = c(bbox$ymin-2000,bbox$ymax+10000))

# Les étapes suivantes permettent juste d'ajouter une légende et une barre d'échelle
# avec des plot successifs, histoire de créer une carte complète.

# Ajout d'une légende de classes

# On crée 3 rectangles correspondant à chacune des classes d'isochrones
# (entre 20 et 30 min, entre 10 et 20 min et entre 0 et 10 min).
# Les valeurs ont été choisies de manière relative par rapport à la position de la légende
# du point, toujours en projection locale métrique (EPSG 2975)
rectangle1 <- matrix(c(372000,7690000,
                       372000,7688000,
                       376000,7688000,
                       376000,7690000,
                       372000,7690000),
                     # les 1er et dernier points sont identiques pour fermer le polygone
                     ncol=2, byrow=TRUE)
fond_leg_iso_60 <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rectangle1))),
                             crs=2975)

rectangle2 <- matrix(c(372000,7686000,
                       372000,7684000,
                       376000,7684000,
                       376000,7686000,
                       372000,7686000),
                     ncol=2, byrow=TRUE)
fond_leg_iso_40_60 <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rectangle2))),
                                crs=2975)

rectangle3 <- matrix(c(372000,7682000,
                       372000,7680000,
                       376000,7680000,
                       376000,7682000,
                       372000,7682000),
                     ncol=2, byrow=TRUE)
fond_leg_iso_20_40 <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rectangle3))),
                                crs=2975)

rectangle4 <- matrix(c(372000,7678000,
                       372000,7676000,
                       376000,7676000,
                       376000,7678000,
                       372000,7678000),
                     ncol=2, byrow=TRUE)
fond_leg_iso_10_20 <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rectangle4))),
                                crs=2975)

rectangle5 <- matrix(c(372000,7674000,
                       372000,7672000,
                       376000,7672000,
                       376000,7674000,
                       372000,7674000),
                     ncol=2, byrow=TRUE)
fond_leg_iso_0_10 <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rectangle5))),
                               crs=2975)

# on colorise les rectangles selon la même palette
# et on affiche les rectangles sur la carte
plot(sf::st_geometry(fond_leg_iso_60),
     col = rev(oceanis::recup_palette(stylePalette = "defaut", nbNeg = 0, nbPos = 5)[[1]])[1],
     border = "transparent",
     add = TRUE)
plot(sf::st_geometry(fond_leg_iso_40_60),
     col = rev(oceanis::recup_palette(stylePalette = "defaut", nbNeg = 0, nbPos = 5)[[1]])[2],
     border = "transparent",
     add = TRUE)
plot(sf::st_geometry(fond_leg_iso_20_40),
     col = rev(oceanis::recup_palette(stylePalette = "defaut", nbNeg = 0, nbPos = 5)[[1]])[3],
     border = "transparent",
     add = TRUE)
plot(sf::st_geometry(fond_leg_iso_10_20),
     col = rev(oceanis::recup_palette(stylePalette = "defaut", nbNeg = 0, nbPos = 5)[[1]])[4],
     border = "transparent",
     add = TRUE)
plot(sf::st_geometry(fond_leg_iso_0_10),
     col = rev(oceanis::recup_palette(stylePalette = "defaut", nbNeg = 0, nbPos = 5)[[1]])[5],
     border = "transparent",
     add = TRUE)

# on ajoute les labels de légende
text(377000, 7689000, labels = "Plus de 60 km", cex = 0.9, pos = 4)
text(377000, 7685000, labels = "Entre 40 km et 60 km", cex = 0.9, pos = 4)
text(377000, 7681000, labels = "Entre 20 km et 40 km", cex = 0.9, pos = 4)
text(377000, 7677000, labels = "Entre 10 km et 20 km", cex = 0.9, pos = 4)
text(377000, 7673000, labels = "Moins de 10 km", cex = 0.9, pos = 4)

# Placement d'une barre d'échelle

# on crée les lignes de la barre d'échelle
barre_echelle = sf::st_linestring(matrix(c(sf::st_bbox(reg04)$xmax,
                                           sf::st_bbox(reg04)$ymin,
                                           sf::st_bbox(reg04)$xmax+5000,
                                           sf::st_bbox(reg04)$ymin),
                                         ncol = 2,
                                         byrow=TRUE))
tick1_echelle = sf::st_linestring(matrix(c(sf::st_bbox(reg04)$xmax,
                                           sf::st_bbox(reg04)$ymin,
                                           sf::st_bbox(reg04)$xmax,
                                           sf::st_bbox(reg04)$ymin+1000),
                                         ncol = 2,
                                         byrow=TRUE))
tick2_echelle = sf::st_linestring(matrix(c(sf::st_bbox(reg04)$xmax+5000,
                                           sf::st_bbox(reg04)$ymin,
                                           sf::st_bbox(reg04)$xmax+5000,
                                           sf::st_bbox(reg04)$ymin+1000),
                                         ncol = 2,
                                         byrow=TRUE))

# on affiche la barre d'échelle
plot(barre_echelle, lwd = 3, add = T)
plot(tick1_echelle, lwd = 2, add = T)
plot(tick2_echelle, lwd = 2, add = T)

# on affiche la valeur de l'échelle
text(sf::st_bbox(reg04)$xmax+2500, sf::st_bbox(reg04)$ymin-700,labels="5 km",cex=0.9)
