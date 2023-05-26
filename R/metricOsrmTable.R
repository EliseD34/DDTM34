#' @name metricOsrmTable
#'
#' @title Calculer les temps de trajet et les distances entre deux groupes de points
#'
#' @description La fonction metricOsrmTable permet de calculer des temps de trajet et des distances entre deux points
#' ou deux groupes de points en face à face ou par croisement.
#'
#' @param src vecteur numérique de longueur 3 (id/lon/lat), data.frame (colonnes id/lon/lat),
#' objet sf ou objet sp (SpatialPointsDataFrame ou SpatialPolygonsDataFrame).
#' @param dst vecteur numérique de longueur 3 (id/lon/lat), data.frame (colonnes id/lon/lat),
#' objet sf ou objet sp (SpatialPointsDataFrame ou SpatialPolygonsDataFrame).
#' @param duree booléen. Si TRUE (par défaut), la fonction retourne la durée.
#' @param distance booléen. Si TRUE (par défaut), la fonction retourne la distance.
#' @param faceAFace booléen. Si TRUE (par défaut), les couples de points sont pris en face à face
#' entre src et dst. Si src et dst n’ont pas la même dimension, les points en trop de la table la plus grande sont ignorés.
#' Si FALSE, les couples sont formés selon le croisement en produit cartésien entre src et dst.
#' @param allerRetour booléen. Si faceAFace = FALSE et allerRetour = TRUE (par défaut), tous les couples sont formés entre src et dst (hors stables). Ignoré si faceAFace = TRUE.
#' @param stable booléen. Si stable = FALSE (par défaut), les stables sont supprimés dans la table en sortie. Sinon (stable = TRUE), la durée et la distance renvoient 0.
#' @param rayonMax numérique. Si faceAFace = FALSE, distance maximale en kilomètres pour les couples formés entre src et dst.
#' @param nbDstVolOiseau numérique. Si faceAFace = FALSE, nombre maximal de dst par src les plus proches à vol d'oiseau.
#' @param nbDstMeasure numérique. Si faceAFace = FALSE, nombre maximal de dst par src les plus proches en temps ou en distance.
#' @param optiMeasure texte. Si faceAFace = FALSE et nbDstMeasure > 0, choix du critère des dst les plus proches par src : "duree" ou "distance".
#' @param emprise texte. Zone géographique contenant les coordonnées. A préciser si rayonMax > 0, sinon l'argument est ignoré.
#' A choisir parmi "FRM" (par défaut) pour la France métropolitaine et ses régions transfrontalières,
#' "971" pour la Guadeloupe, "972" pour la Martinique, "973" pour la Guyane, "974" pour la Réunion, "976" pour Mayotte ou "999" pour une autre zone.
#' @param exclude texte. Permet aux trajets d'éviter les autoroutes (“motorway”), les péages (“toll”) ou les ferries (“ferry”). Par défaut NULL.
#' @param interactive booléen. Choix du contexte d'exécution. Si TRUE, contexte shiny. Par défaut FALSE.
#'
#' @return data.frame avec les colonnes "ID","idSrc","lonSrc","latSrc","idDst","lonDst","latDst", "duree" et/ou "distance".
#'
#' @details Les sources (src) correspondent aux points de départ et les destinations (dst) aux points d’arrivée.
#'
#' Les id doivent toujours être en 1ère position du vecteur ou en 1ère colonne du data.frame.
#'
#' Si src et dst sont des vecteurs ou des data.frame, les coordonnées doivent être dans le système
#' géographique non projeté WGS84 (longitude et latitude, EPSG : 4326).
#'
#' La fonction convertTo permet de convertir des coordonnees en WGS84 (EPSG : 4326).
#'
#' Les objets spatiaux sf ou sp peuvent être dans n’importe quel système, projeté ou non.
#' Mais en sortie, les coordonnées seront dans le système WGS84 (EPSG : 4326).
#'
#' Si faceAFace = FALSE et allerRetour = TRUE, tous les couples sont formés. Par exemple si scr = c(src_Pau,src_Agen) et dst = c(dst_Pau,dst_Agen),
#' alors les couples 'src_Pau' vers 'dst_Agen' et 'src_Agen' vers 'dst_Pau' seront calculés (hors stables 'src_Pau-dst_Pau' et 'src_Agen-dst_Agen').
#' Si faceAFace = FALSE et allerRetour = FALSE, un seul couple est calculé, de 'src_Pau' vers 'dst_Agen'.
#'
#' Les stables peuvent être supprimés de la table de résultats en spécifiant stable = FALSE. Si TRUE, les stables seront repérables avec les valeurs de durée et de distance à 0.
#'
#' Lorsque faceAFace = FALSE, les arguments rayonMax, nbDstVolOiseau, nbDstMeasure et optiMeasure permettent de filtrer les résultats selon la structure des src et dst.
#'
#' rayonMax et nbDstVolOiseau permettent de filtrer en amont les couples src->dst pour éviter des calculs vers le serveur inutiles et chronophages.
#'
#' nbDstMeasure et optiMeasure permettent de filtrer les résultats après le requêtage au serveur. Ne réduit donc pas les temps de calcul.
#'
#' Si faceAFace = TRUE, les arguments allerRetour, rayonMax, nbDstVolOiseau, nbDstMeasure et optiMeasure sont ignorés.
#'
#' Si le filtre rayonMax est utilisé (si rayonMax > 0), alors il est indispensable de préciser l'emprise de la zone couverte par les coordonnées.
#' Par défaut, l'emprise correspond à la France métropolitaine et ses régions transfrontalières (emprise = "FRM"). Le système de coordonnées projetées, utilisé alors pour effectuer
#' les calculs de distances à vol d'oiseau, est le Lambert93 (EPSG 2154). Pour les DOM, il faut préciser le code départemantal du DOM ("971","972","973","974" ou "976").
#' Pour information, les codes EPSG en vigueur dans les DOM sont 5490 pour la Guadeloupe et la Martinique, 2972 pour la Guyane, 2975 pour la Réunion et 4471 pour Mayotte.
#' Pour toutes autres zones, il faut alors préciser emprise = "999". Dans ce cas, la projection Mercator sera utilisée (EPSG 3395).
#'
#' @importFrom sf st_as_sf st_crs st_collection_extract st_centroid st_transform st_coordinates st_geometry
#' @importFrom RJSONIO fromJSON
#' @importFrom RANN nn2
#' @export
#'
#' @examples
#' # Specification d'un serveur osrm obligatoire pour executer les exemples
#' options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
#'
#' # Specification du profil
#' options(osrm.profile = "driving")
#'
#' # Construction des sources et des destinations.
#' sources <-  data.frame(id = c("A","B","C"),
#'                        lon = c(4.92,4.86,4.72),
#'                        lat = c(46.15,46.08,45.92),
#'                        stringsAsFactors = FALSE)
#'
#' destinations <-data.frame(id = c("B","C","D"),
#'                           lon = c(4.86,4.72,4.67),
#'                           lat = c(46.08,45.92,45.83),
#'                           stringsAsFactors = FALSE)
#'
#' # 3 couples de points calcules en face a face.
#' metricOsrmTable(src = sources,
#'                 dst = destinations,
#'                 faceAFace = TRUE) # par defaut
#'
#' # 3 couples de points calcules selon le produit cartesien.
#' metricOsrmTable(src = sources,
#'                 dst = destinations,
#'                 faceAFace = FALSE)
#'
#' # 3 couples de points calcules selon le produit cartesien
#' # et avec les stables conserves.
#' metricOsrmTable(src = sources,
#'                 dst = destinations,
#'                 faceAFace = FALSE,
#'                 stable = TRUE)
#'
#' # 3 couples de points calcules selon le produit cartesien,
#' # sans les stables et sans aller-retour.
#' metricOsrmTable(src = sources,
#'                 dst = destinations,
#'                 faceAFace = FALSE,
#'                 stable = FALSE,
#'                 allerRetour = FALSE)
#'
#' ### Utilisation des filtres
#'
#' # Construction des sources et des destinations.
#' sources <-  data.frame(id = c("C1","C2","C3"),
#'                        lon = c(4.92,4.86,4.72),
#'                        lat = c(46.15,46.08,45.92),
#'                        stringsAsFactors = FALSE)
#'
#' destinations <-data.frame(id = c("E1","E2","E3"),
#'                           lon = c(4.63,4.75,4.67),
#'                           lat = c(45.95,45.88,45.83),
#'                           stringsAsFactors = FALSE)
#'
#' ## Les sources et les destinations ne sont pas de meme nature.
#' ## Par exemple, les sources peuvent etre des carreaux
#' ## et les destinations des equipements.
#'
#' # On selectionne n dst situees a moins de 40km a vol d'oiseau de chaque src.
#' metricOsrmTable(src = sources,
#'                 dst = destinations,
#'                 faceAFace = FALSE,
#'                 stable = TRUE,
#'                 rayonMax = 40,
#'                 emprise = "FRM")
#' # Les distances calculees par la route peuvent etre superieures a 40km :
#' # le trajet par la route est forcement plus long en distance qu'a vol d'oiseau.
#'
#' # On selectionne n dst situees a moins de 40km a vol d'oiseau de chaque src
#' # puis la plus proche parmi celles-ci.
#' metricOsrmTable(src = sources,
#'                 dst = destinations,
#'                 faceAFace = FALSE,
#'                 stable = TRUE,
#'                 rayonMax = 40,
#'                 nbDstVolOiseau = 1,
#'                 emprise = "FRM")
#' # Les stables, s'il y en a, doivent etre conserves
#' # car src et dst ne sont pas de meme nature.
#'
#' # On selectionne n dst situees a moins de 40km a vol d'oiseau de chaque src
#' # puis les 2 dst les plus proches parmi celles-ci.
#' metricOsrmTable(src = sources,
#'                 dst = destinations,
#'                 faceAFace = FALSE,
#'                 stable = TRUE,
#'                 rayonMax = 40,
#'                 nbDstVolOiseau = 2,
#'                 emprise = "FRM")
#'
#' # On selectionne n dst situees a moins de 40km a vol d'oiseau de chaque src
#' # puis les 2 dst les plus proches parmi celles-ci.
#' # Enfin, on conserve uniquement 1 dst la plus proche en temps de parcours par la route.
#' metricOsrmTable(src = sources,
#'                 dst = destinations,
#'                 faceAFace = FALSE,
#'                 stable = TRUE,
#'                 rayonMax = 40,
#'                 nbDstVolOiseau = 2,
#'                 nbDstMeasure = 1,
#'                 optiMeasure = "duree",
#'                 emprise = "FRM")
#'
metricOsrmTable <- function(src, dst, duree = TRUE, distance = TRUE, faceAFace = TRUE, allerRetour = TRUE, stable = FALSE, rayonMax = 0, nbDstVolOiseau = 0, nbDstMeasure = 0, optiMeasure = c("duree", "distance"), emprise = "FRM", exclude = NULL, interactive = FALSE)
{
  ### verification des parametres
  verifParamSrcDst(src = src, dst = dst)

  ### Conversion de src et dst en data.frame
  src <- convertToDf(objet = src)
  dst <- convertToDf(objet = dst)

  if(!faceAFace)
  {
    ### Suppression des points en doublon
    src <- unique(src)
    dst <- unique(dst)
  }

  if(faceAFace)
  {
    res <- osrmTableFaceAFace(src = src, dst = dst, duree = duree, distance = distance, exclude = exclude, interactive = interactive)

  }else if(!faceAFace & allerRetour & rayonMax == 0 & nbDstVolOiseau == 0)
  {
    res <- osrmTableCartesien(src = src, dst = dst, duree = duree, distance = distance, exclude = exclude, interactive = interactive)

  }else if(!faceAFace & !allerRetour & rayonMax == 0 & nbDstVolOiseau == 0)
  {
    res <- osrmTableAllerRetour(src = src, dst = dst, duree = duree, distance = distance, exclude = exclude, interactive = interactive)

  }else if(!faceAFace & (rayonMax > 0 | nbDstVolOiseau > 0))
  {
    ### Recuperation du code epsg a partir de l'emprise declaree
    code_epsg <- switch(emprise,
                        "FRM"="2154",# Lambert 93
                        "971"="5490",# UTM 20 N
                        "972"="5490",# UTM 20 N
                        "973"="2972",# UTM 22 N
                        "974"="2975",# UTM 40 S
                        "976"="4471",# UTM 38 S
                        "999"="3395") # Mercator

    res <- osrmTableFiltre(src = src, dst = dst, duree = duree, distance = distance, exclude = exclude, rayonMax = rayonMax, nbDstVolOiseau = nbDstVolOiseau, nbDstMeasure = nbDstMeasure, optiMeasure = optiMeasure, code_epsg = code_epsg, interactive = interactive)
  }

  if(is.null(res))
  {
    message(paste0("[WARNING] Aucun couple n'a \u00e9t\u00e9 calcul\u00e9."))
    message(paste0("V\u00e9rifiez que les filtres ne soient pas trop restrictifs"))
    message(paste0("ou que les sources et les destinations ne soient pas trop \u00e9loign","\u00e9","es."))
    return(NULL)
  }

  # Suppression des stables

  if(!stable)
  {
    if(any(res$duree %in% 0) & any(res$distance %in% 0)) res <- res[!res$duree == 0 & !res$distance == 0,]
  }

  if(nrow(res) > 0)
  {
    res <- res[order(res$idSrc),]
    res$ID <- 1:nrow(res)
    row.names(res) <- res$ID

    # Avertissement pour les couples non calculés

    message <- FALSE
    nbNonCalcules <- 0

    if(duree)
    {
      res$duree <- round(res$duree/60,2)

      if(!is.na(any(res$duree < 0)))
      {
        if(any(res$duree < 0))
        {
          nbNonCalcules <- nrow(res[res$duree < 0,])
          res[res$duree < 0, "duree"] <- -999999
          message <- TRUE
        }
      }
    }

    if(distance)
    {
      res$distance <- round(res$distance/1000,3)

      if(!is.na(any(res$distance < 0)))
      {
        if(any(res$distance < 0))
        {
          res[res$distance < 0, "distance"] <- -999999
          message <- TRUE
        }
      }
    }

    if(message)
    {
      if(nbNonCalcules == 1)
      {
        message(paste0("[WARNING] ",nbNonCalcules," couple n'a pas \u00e9t\u00e9 calcul\u00e9."))
        message(paste0("Il est rep\u00e9rable dans la table des r\u00e9sultats avec une dur","\u00e9","e et une distance \u00e0 -999999."))
        message(paste0("Pensez \u00e0 le retirer avant toute analyse de la table."))
      }
      if(nbNonCalcules > 1)
      {
        message(paste0("[WARNING] ",nbNonCalcules," couples n'ont pas \u00e9t\u00e9 calcul\u00e9s."))
        message(paste0("Ils sont rep\u00e9rables dans la table des r\u00e9sultats avec une dur","\u00e9","e et une distance \u00e0 -999999."))
        message(paste0("Pensez \u00e0 les retirer avant toute analyse de la table."))
      }
    }
  }else
  {
    message(paste0("[WARNING] Le table de r\u00e9sultats est vide."))
    message(paste0("Il n'y a que des couples de stables dans la table en entr","\u00e9","e et l'argument stable = FALSE."))
  }

  return(res)
}
