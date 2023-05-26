#' @name indIsoSpatial
#'
#' @title Calculer des indicateurs en volume par regroupement de mailles selon leur appartenance spatiale
#'
#' @description La fonction indIsoSpatial permet de sommer une ou plusieurs variables (la population par exemple)
#' d'une maille (maille communale ou grille par exemple) selon leur appartenance spatiale à une zone particulière (polygones d'isochrones par exemple).
#'
#' @param pol objet sf (POLYGON ou MULTIPOLYGON) ou sp (SpatialPolygons ou SpatialPolygonsDataFrame).
#' @param maille objet sf (POLYGON ou MULTIPOLYGON) ou sp (SpatialPolygonsDataFrame).
#' @param var vecteur texte. Noms des variables en volume à sommer de l'objet maille.
#' @param pond vecteur numérique. Noms des variables de pondération de l'objet maille. A appliquer aux variables à sommer. Par défaut NULL.
#' @param choixIntersect numérique. 1, 2 ou 3 (voir details). Par défaut 1.
#' @param return vecteur texte. "pol", "maille" ou c("pol","maille") (voir details). Par défaut "pol".
#' @param idGroup texte. Nom de la variable identifiant les pol et constituant les groupes de maille si return contient "maille". Par défaut NULL.
#'
#' @return Si return == "pol", l'objet pol, enrichi des variables sommées issues de l'objet maille.
#'
#' Si return == "maille", l'objet maille, enrichi d'une variable indiquant le groupe d'appartenance de la maille à pol et les valeurs sommées pour chaque groupe.
#'
#' Si return == c("pol","maille"), une liste contenant pol et maille est renvoyée, enrichie des variables décrites ci-dessus.
#'
#' @details Si les variables de pondérations sont renseignées, le vecteur des variables de pondérations doit respecter le même ordre que le vecteur des variables à sommer.
#'
#' Le paramètre choixIntersect vaut 1, 2 ou 3. Si une maille intersecte pol :
#'
#' choix 1 : la totalité de la variable de la maille intersectée est comptabilisée dans la somme.
#'
#' choix 2 : la maille est tronquée selon le découpage de pol. Les valeurs à sommer sont recalculées selon le prorata de la surface de la maille tronquée.
#'
#' choix 3 : la totalité de la variable de la maille intersectée est exclue de la somme.
#'
#' Les arguments pol et maille doivent être dans le même système de projection, peu importe le système.
#'
#' @importFrom sf st_as_sf st_transform st_cast st_intersects st_intersection st_area
#' @importFrom methods is
#' @export
#'
#' @examples
#' # Specification d'un serveur osrm obligatoire pour executer les exemples
#' options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
#'
#' # Specification du profil
#' options(osrm.profile = "driving")
#'
#' # Appel a la fonction metricOsrmIso
#' iso <- metricOsrmIso(loc = data.frame(lon = 4.92,lat = 46.15))
#'
#' # Conversion en projection metrique Lambert 93 (EPSG 2154)
#' iso_L93 <- convertTo(from = iso[[1]],
#'                      toEpsg = 2154)
#'
#' # Construction d'une grille
#' grille <- sf::st_as_sf(x = sf::st_make_grid(iso_L93, cellsize = 52000))
#'
#' # Ajout d'une variable en volume a la grille
#' grille$vol <- c(15,11,26,5,3,17)
#'
#' # Calcul de l'indicateur spatial
#' ind_res <- indIsoSpatial(pol = iso_L93,
#'                          maille = grille,
#'                          var = "vol",
#'                          choixIntersect = 1,
#'                          return = c("pol","maille"),
#'                          idGroup = "id")
#'
indIsoSpatial <- function(pol, maille, var, pond = NULL, choixIntersect = 1, return = "pol", idGroup = NULL)
{
  if(any(!return %in% c("pol","maille")))
  {
    stop(simpleError("return doit etre 'pol', 'maille' ou c('pol','maille')."))
  }

  if(!choixIntersect %in% c(1,2,3))
  {
    stop(simpleError("choixIntersect doit etre 1, 2 ou 3."))
  }

  if(methods::is(pol, "Spatial")) {
    pol <- sf::st_as_sf(pol)
  }
  if(methods::is(maille, "Spatial")) {
    maille <- sf::st_as_sf(maille)
  }

  if(length(which(names(maille) %in% var)) != length(var))
  {
    stop(simpleError("Toutes les variables du vecteur var doivent etre presentes dans l'objet maille."))
  }

  if(any(return %in% "maille") & !any(names(pol) %in% idGroup))
  {
    stop(simpleError("Si return contient 'maille', idGroup doit etre la variable identifiant de pol."))
  }

  if(!is.null(pond))
  {
    if(length(var) != length(pond))
    {
      stop(simpleError("Le vecteur var doit etre de meme longueur que le vecteur pond"))
    }
    if(length(which(names(maille) %in% pond)) != length(pond))
    {
      stop(simpleError("Toutes les variables du vecteur pond doivent etre presentes dans l'objet maille."))
    }
    pondNULL <- FALSE
  }else if(is.null(pond))
  {
    for(i in 1:length(var))
    {
      maille[,paste0("pond",i)] <- 1
      pond <- c(pond,paste0("pond",i))
    }
    pondNULL <- TRUE
  }else
  {}

  pol <- sf::st_transform(pol, crs = 2154)
  maille <- sf::st_transform(maille, crs = 2154)

  for(i in 1:length(var))
  {
    pol[,var[i]] <- NA
  }

  if(any(return %in% "maille")) listMaille <- list()

  pb <- progress::progress_bar$new(
    format = paste0("Calcul en cours pour ",nrow(pol)," polygones -  [:bar] :percent :elapsed"),
    total = nrow(pol), clear = FALSE, width= 80
  )

  pb$tick(0)

  for(i in 1:nrow(pol))
  {
    # on garde tous les carreaux inclus dans le polygone et ceux qui touchent
    carr_intersects <- as.numeric(sf::st_intersects(maille,pol[i,]))
    carr_intersects <- data.frame(i=carr_intersects,idx=1:length(carr_intersects), stringsAsFactors = FALSE)
    if(any(is.na(carr_intersects$i))) carr_intersects <- carr_intersects[!is.na(carr_intersects$i),]

    if(choixIntersect == 1)
    {
      maille_1 <- maille[carr_intersects$idx,]

      calcul_1 <- round(apply(as.data.frame(as.data.frame(maille_1)[,var])*as.data.frame(as.data.frame(maille_1)[,pond]),2,sum),1)
      calcul_2 <- 0
      calcul_3 <- 0

      if(any(return %in% "maille"))
      {
        maille_1$groupe_pol <- as.data.frame(pol)[i,idGroup]
        for(j in 1:length(var))
        {
          maille_1[,paste0("sum_",var[j])] <- calcul_1[j]
        }
        maille_2 <- NULL
        maille_3 <- NULL
      }
    }

    if(choixIntersect %in% c(2,3))
    {
      # On repère d'abord les carreaux qui touchent ...
      carr_touches <- sf::st_cast(pol[i,], to = "MULTILINESTRING")
      carr_touches <- as.numeric(sf::st_intersects(maille,carr_touches))
      carr_touches <- data.frame(i=carr_touches,idx=1:length(carr_touches), stringsAsFactors = FALSE)
      if(any(is.na(carr_touches$i))) carr_touches <- carr_touches[!is.na(carr_touches$i),]
      maille_4 <- maille[carr_touches$idx,]

      # ... puis on les enlève du résultat d'intersects
      carr_contains <- carr_intersects[!carr_intersects$idx %in% carr_touches$idx,]
      maille_3 <- maille[carr_contains$idx,]

      calcul_3 <- round(apply(as.data.frame(as.data.frame(maille_3)[,var])*as.data.frame(as.data.frame(maille_3)[,pond]),2,sum),1)

      if(choixIntersect == 3)
      {
        calcul_1 <- 0
        calcul_2 <- 0

        maille_1 <- NULL
        maille_2 <- NULL

        if(any(return %in% "maille"))
        {
          maille_3$groupe_pol <- as.data.frame(pol)[i,idGroup]
          for(j in 1:length(var))
          {
            maille_3[,paste0("sum_",var[j])] <- calcul_3[j]
          }
        }
      }
    }

    if(choixIntersect == 2)
    {
      # On tronque les carreaux qui touchent à partir de maille_4
      suppressWarnings(carr_intersection <- sf::st_intersection(maille_4,pol[i,]))
      carr_intersection <- carr_intersection[,names(maille_4)]
      surf_intersection <- as.numeric(sf::st_area(carr_intersection))
      surf_carr_entier <- max(as.numeric(sf::st_area(maille_3)))
      rapport_surf_intersection <- surf_intersection/surf_carr_entier

      calcul_1 <- 0
      calcul_2 <- round(calcul_3 + apply(as.data.frame(as.data.frame(maille_4)[,var])*as.data.frame(as.data.frame(maille_4)[,pond])*rapport_surf_intersection,2,sum),1)
      calcul_3 <- 0

      if(any(return %in% "maille"))
      {
        maille_1 <- NULL

        # On colle à tous les carreaux inclus entièrement dans le polygone (maille_3)
        maille_2 <- rbind(carr_intersection,maille_3)

        maille_2$groupe_pol <- as.data.frame(pol)[i,idGroup]
        for(j in 1:length(var))
        {
          maille_2[,paste0("sum_",var[j])] <- calcul_2[j]
        }

        maille_3 <- NULL
      }
    }

    if(any(return %in% "pol"))
    {
      pol[i,var] <- calcul_1 + calcul_2 + calcul_3
      attr(pol, "agr") <- c("id","min","max","center",var)
    }

    if(any(return %in% "maille"))
    {
      listMaille[[i]] <- rbind(maille_1,maille_2,maille_3)
    }

    pb$tick()
  }

  if(any(return %in% "maille"))
  {
    maille <- do.call(rbind, listMaille)
    maille <- maille[order(as.numeric(row.names(maille))),]

    if(pondNULL) maille <- maille[,-which(names(maille) %in% pond)]
  }

  if(length(return) == 1 & any(return %in% "pol")) return(pol)
  if(length(return) == 1 & any(return %in% "maille")) return(maille)
  if(length(return) == 2) return(list(pol,maille))
}
