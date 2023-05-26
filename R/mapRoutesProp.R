#' @name mapRoutesProp
#'
#' @title Visualiser sur une carte les traces avec une epaisseur variable
#'
#' @description La fonction mapRoutesProp permet de visualiser les tracés avec une épaisseur variable selon le nombre de passages par tronçon de route.
#'
#' @param res liste d'objets sp, sf ou data.frame.
#' @param fonds liste d'objets sp ou sf.
#' @param nbLargeurs numérique. Nombre de tracés de largeurs différentes. Par défaut 5.
#' @param nbFlux vecteur numérique. Nombre de flux (ou de trajets) par tracé. Par défaut 1.
#' @param opaciteOSM numérique. Entre 0 et 1, opacité du fond OpenStreetMap. Par défaut 0.5.
#' @param interactive booléen. Choix du contexte d'exécution. Si TRUE, contexte shiny. Par défaut FALSE.
#' @param mapProxy objet leaflet ou leaflet_proxy. Pour l'intégration des fonctions leaflet dans les applications shiny. Par defaut NULL.
#'
#' @return une liste de deux éléments : un objet leaflet et un objet sf LINESTRING ou MULTILINESTRING.
#'
#' @details Plus le nombre de trajets à représenter est important, plus le temps de calcul sera élevé.
#'
#' En entrée, l'objet res doit être une liste de résultats de la fonction metricOsrmRoute. Les résultats peuvent être des objets sf, sp ou data.frame selon le choix du paramètre returnclass de metricOsrmRoute.
#'
#' Pour optimiser les temps de calcul, il est préférable de choisir returnclass = NULL pour que la fonction metricOsrmRoute retourne un data.frame de coordonnées pour chaque trajet.
#'
#' En sortie, la fonction mapRoutesProp renvoie une liste de deux éléments :
#'
#' - la carte leaflet qu'il est possible d'afficher directement dans le viewer ;
#'
#' - un objet sf, qui indique le poids (colonne weight) de chaque tronçon de route et sa largeur (colonne classes).
#'
#' A partir de cet objet sf, il est possible de filtrer les poids pour ne représenter que des tronçons pertinents.
#' Par exemple, les routes étant empruntées plus de 5 fois (weight > 5).
#'
#' Pour une visualisation sur une carte des tracés de routes après avoir appliqué un filtre sur les poids, vous pouvez utiliser la fonction mapRoutes.
#'
#' La colonne classes permet de modifier la largeur des tronçons de route par poids.
#'
#' L'argument nbLargeurs permet de choisir le nombre de largeurs différentes pour l'ensemble des tracés.
#'
#' L'argument nbFlux permet de renseigner le nombre de flux pour chaque tracé. Par défaut, chaque tracé correspond à un trajet.
#'
#' @importFrom shiny showModal modalDialog HTML withProgress incProgress removeModal
#' @importFrom sf st_sf st_sfc st_as_sf st_transform st_bbox st_geometry st_length st_coordinates st_multilinestring st_linestring st_line_merge st_combine st_collection_extract
#' @importFrom leaflet leaflet invokeMethod fitBounds addScaleBar addPolygons addPolylines leafletOptions tileOptions getMapData pathOptions scaleBarOptions clearGroup
#' @importFrom methods is
#' @importFrom classInt classIntervals
#' @importFrom dplyr group_by summarise n %>%
#' @importFrom rlang .data
#' @importFrom stringr str_split
#' @export
#'
#' @examples
#' # Specification d'un serveur osrm obligatoire pour executer les exemples
#' options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
#'
#' # Specification du profil
#' options(osrm.profile = "driving")
#'
#' # Calcul de deux traces de route.
#' dt1_route_f <- metricOsrmRoute(src = data.frame(lon = 4.92,
#'                                                 lat = 46.15),
#'                               dst = data.frame(lon = 4.72,
#'                                                 lat = 45.92),
#'                               overview = "full",
#'                               returnclass = NULL)
#'
#' dt2_route_f <- metricOsrmRoute(src = data.frame(lon = 4.92,
#'                                                 lat = 46.15),
#'                               dst = data.frame(lon = 4.82,
#'                                                 lat = 45.85),
#'                               overview = "full",
#'                               returnclass = NULL)
#'
#' # Visualisation des deux traces de route.
#' # Le 1er trace est emprunte 2 fois, le second 3 fois.
#' # Le trace commun est donc emprunte 5 fois.
#' mapRoutesProp(res = list(dt1_route_f[,c(ncol(dt1_route_f)-1,ncol(dt1_route_f))],
#'                          dt2_route_f[,c(ncol(dt2_route_f)-1,ncol(dt2_route_f))]),
#'               nbLargeurs = 3,
#'               nbFlux = c(2,3))
#'
mapRoutesProp <- function(res, fonds = NULL, nbLargeurs = 5, nbFlux = 1, opaciteOSM = 0.5, interactive = FALSE, mapProxy = NULL)
{
  if(!is.list(res))
  {
    stop(simpleError("res doit etre une liste d'objets sp, sf ou data.frame."))
  }
  if(!any(class(res[[1]]) == "data.frame" | testSf(res[[1]]) | methods::is(res[[1]], "Spatial")))
  {
    stop(simpleError("res doit etre une liste d'objets sp, sf ou data.frame."))
  }
  if(!is.null(fonds))
  {
    if(!is.list(fonds))
    {
      stop(simpleError("fonds doit etre une liste d'objets sp ou sf."))
    }
    if(!any(testSf(fonds[[1]]) | methods::is(fonds[[1]], "Spatial")))
    {
      stop(simpleError("fonds doit etre une liste d'objets sp ou sf."))
    }
  }

  if(!is.null(mapProxy))
  {
    if(any(class(mapProxy) %in% "leaflet_proxy")) # Contexte shiny/proxy
    {
      leaflet::clearGroup(mapProxy,
                          group = "carte_routes_prop")
    }
  }

  if(is.null(mapProxy) | (!is.null(mapProxy) & is.character(mapProxy)))
  {
    if(!is.null(fonds))
    {
      for(i in 1:length(fonds))
      {
        # si fonds est un objet sp, on le transforme d'abord en objet sf
        if (methods::is(fonds[[i]], "Spatial"))
        {
          fonds[[i]] <- sf::st_as_sf(x = fonds[[i]])
        }
        fonds[[i]] <- sf::st_transform(fonds[[i]], crs = 4326)
      }
    }

    # si coord est un objet sp, on le transforme d'abord en objet sf
    if (methods::is(res[[1]], "Spatial")) {
      for(i in 1:length(res))
      {
        res[[i]] <- sf::st_as_sf(x = res[[i]])
      }
    }

    if(methods::is(res[[1]], "sf"))
    {
      if(interactive)
      {
        shiny::showModal(shiny::modalDialog(shiny::HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Calcul des routes proportionnelles...</font> "), size = "m", footer = NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        shiny::withProgress(message = "Calculs en cours... ",{

          res <- do.call(rbind,res)

          geom_L93 <- sf::st_transform(res, crs=2154)
          geom_L93$length <- as.numeric(sf::st_length(geom_L93))
          geom_L93 <- geom_L93[order(geom_L93$length, decreasing = T),]
          geom_L93 <- sf::st_transform(res, crs=4326)

          couples <- lapply(1:(nrow(geom_L93)), function(x){

            shiny::incProgress(1/(nrow(geom_L93)+4))

            coord_un_trace <- as.data.frame(unique(sf::st_coordinates(geom_L93[x,])[,1:2]))

            couples_un_trace <- data.frame(id = x, cbind(coord_un_trace[-nrow(coord_un_trace),], coord_un_trace[-1,]))
            names(couples_un_trace) <- c("id", "X1", "Y1", "X2", "Y2")
            couples_un_trace
          })

          shiny::incProgress(1/(nrow(geom_L93)+4))

          couples <- do.call(rbind,couples)

          if(length(nbFlux) == 1 && nbFlux == 1){
            couples_poids <- couples %>%
              dplyr::group_by(.data$X1,.data$Y1,.data$X2,.data$Y2) %>%
              dplyr::summarise(NB = dplyr::n())

            couples_poids$IDS <- 0
            shiny::incProgress(1/(nrow(geom_L93)+4))
          }else{
            couples_poids <- couples %>%
              dplyr::group_by(.data$X1,.data$Y1,.data$X2,.data$Y2) %>%
              dplyr::mutate(IDS = paste(unique(.data$id), collapse = "-")) %>%
              dplyr::mutate(IDS = paste0(.data$IDS,"-0")) %>%
              dplyr::group_by(.data$X1,.data$Y1,.data$X2,.data$Y2,.data$IDS) %>%
              dplyr::ungroup()

            list_ids <- lapply(sapply(stringr::str_split(couples_poids$IDS, "-"), function(x) x),FUN=as.numeric)
            nbs <- round(sapply(list_ids, function(x) sum(nbFlux[x], na.rm = TRUE)),0)

            couples_poids <- couples_poids %>% dplyr::mutate(NB = nbs)

            rm(list_ids,nbs)
            shiny::incProgress(1/(nrow(geom_L93)+4))
          }

          couples_poids <- couples_poids[order(couples_poids$NB),]
          poids_troncon <- unique(couples_poids[,c("IDS","NB")])
          graph_poids <- list()
          shiny::incProgress(1/(nrow(geom_L93)+4))

          graph_poids <- lapply(1:nrow(poids_troncon), function(x){

            un_poids <- couples_poids[couples_poids$NB %in% poids_troncon[x,"NB"],c("X1","Y1","X2","Y2")]
            aa <- unlist(apply(un_poids, 1, list), recursive=F)
            aa <- lapply(aa, function(y) matrix(as.numeric(y),byrow=T,ncol=2))

            sf::st_sf(geometry=sf::st_sfc(sf::st_line_merge(sf::st_combine(sf::st_geometry(sf::st_multilinestring(aa)))), crs=4326),weight=poids_troncon[x,"NB"],idTrace=poids_troncon[x,"IDS"])

          })

          graph_poids <- do.call(rbind,graph_poids)
          names(graph_poids) <- c("weight","idTrace","geometry")
          if(length(unique(graph_poids$idTrace)) == 1)
          {
            if(unique(graph_poids$idTrace) == 0) graph_poids <- graph_poids[,c("weight","geometry")]
          }
          graph_poids <- sf::st_transform(graph_poids, crs=4326)

          shiny::incProgress(1/(nrow(geom_L93)+4))
        })

        shiny::removeModal()
      }else
      {
        res <- do.call(rbind,res)

        geom_L93 <- sf::st_transform(res, crs=2154)
        geom_L93$length <- as.numeric(sf::st_length(geom_L93))
        geom_L93 <- geom_L93[order(geom_L93$length, decreasing = T),]
        geom_L93 <- sf::st_transform(res, crs=4326)

        pb3 <- progress::progress_bar$new(
          format = "Calcul des routes proportionnelles... : [:bar] :percent :elapsed",
          total = nrow(geom_L93)+4, clear = FALSE, width= 60
        )

        pb3$tick(0)

        couples <- lapply(1:(nrow(geom_L93)), function(x){

          coord_un_trace <- as.data.frame(unique(sf::st_coordinates(geom_L93[x,])[,1:2]))

          couples_un_trace <- data.frame(id = x, cbind(coord_un_trace[-nrow(coord_un_trace),], coord_un_trace[-1,]))
          names(couples_un_trace) <- c("id", "X1", "Y1", "X2", "Y2")

          pb3$tick()

          couples_un_trace
        })

        couples <- do.call(rbind,couples)

        if(length(nbFlux) == 1 && nbFlux == 1){
          couples_poids <- couples %>%
            dplyr::group_by(.data$X1,.data$Y1,.data$X2,.data$Y2) %>%
            dplyr::summarise(NB = dplyr::n())

          couples_poids$IDS <- 0

          pb3$tick()
        }else{
          couples_poids <- couples %>%
            dplyr::group_by(.data$X1,.data$Y1,.data$X2,.data$Y2) %>%
            dplyr::mutate(IDS = paste(unique(.data$id), collapse = "-")) %>%
            dplyr::mutate(IDS = paste0(.data$IDS,"-0")) %>%
            dplyr::group_by(.data$X1,.data$Y1,.data$X2,.data$Y2,.data$IDS) %>%
            dplyr::ungroup()

          list_ids <- lapply(sapply(stringr::str_split(couples_poids$IDS, "-"), function(x) x),FUN=as.numeric)
          nbs <- round(sapply(list_ids, function(x) sum(nbFlux[x], na.rm = TRUE)),0)

          couples_poids <- couples_poids %>% dplyr::mutate(NB = nbs)

          rm(list_ids,nbs)

          pb3$tick()
        }

        couples_poids <- couples_poids[order(couples_poids$NB),]
        poids_troncon <- unique(couples_poids[,c("IDS","NB")])
        graph_poids <- list()
        pb3$tick()

        graph_poids <- lapply(1:nrow(poids_troncon), function(x){

          un_poids <- couples_poids[couples_poids$NB %in% poids_troncon[x,"NB"],c("X1","Y1","X2","Y2")]
          aa <- unlist(apply(un_poids, 1, list), recursive=F)
          aa <- lapply(aa, function(y) matrix(as.numeric(y),byrow=T,ncol=2))

          sf::st_sf(geometry=sf::st_sfc(sf::st_line_merge(sf::st_combine(sf::st_geometry(sf::st_multilinestring(aa)))), crs=4326),weight=poids_troncon[x,"NB"],idTrace=poids_troncon[x,"IDS"])

        })

        pb3$tick()

        graph_poids <- do.call(rbind,graph_poids)
        names(graph_poids) <- c("weight","idTrace","geometry")
        if(length(unique(graph_poids$idTrace)) == 1)
        {
          if(unique(graph_poids$idTrace) == 0) graph_poids <- graph_poids[,c("weight","geometry")]
        }
        graph_poids <- sf::st_transform(graph_poids, crs=4326)
        pb3$tick()
      }
    }

    if(any(class(res) %in% "list" & class(res[[1]]) %in% "data.frame"))
    {
      ### A partir d'un objet data.frame

      if(interactive)
      {
        shiny::showModal(shiny::modalDialog(shiny::HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Calcul des routes proportionnelles...</font> "), size = "m", footer = NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

        shiny::withProgress(message = "Calculs en cours...  ",{

          couples <- lapply(1:length(res), function(x){

            shiny::incProgress(1/(length(res)+4))

            coord_un_trace <- unique(res[[x]][,c(ncol(res[[x]])-1, ncol(res[[x]]))])
            coord_un_trace <- data.frame(id=x,coord_un_trace[-nrow(coord_un_trace),],coord_un_trace[-1,])
            names(coord_un_trace) <- c("id","X1","Y1","X2","Y2")
            coord_un_trace

          })

          couples <- do.call(rbind,couples)

          shiny::incProgress(1/(length(res)+4))

          if(length(nbFlux) == 1 && nbFlux == 1){
            couples_poids <- couples %>%
              dplyr::group_by(.data$X1,.data$Y1,.data$X2,.data$Y2) %>%
              dplyr::summarise(NB = dplyr::n())

            couples_poids$IDS <- 0
            shiny::incProgress(1/(length(res)+4))
          }else{
            couples_poids <- couples %>%
              dplyr::group_by(.data$X1,.data$Y1,.data$X2,.data$Y2) %>%
              dplyr::mutate(IDS = paste(unique(.data$id), collapse = "-")) %>%
              dplyr::mutate(IDS = paste0(.data$IDS,"-0")) %>%
              dplyr::group_by(.data$X1,.data$Y1,.data$X2,.data$Y2,.data$IDS) %>%
              dplyr::ungroup()

            list_ids <- lapply(sapply(stringr::str_split(couples_poids$IDS, "-"), function(x) x),FUN=as.numeric)
            nbs <- round(sapply(list_ids, function(x) sum(nbFlux[x], na.rm = TRUE)),0)

            couples_poids <- couples_poids %>% dplyr::mutate(NB = nbs)

            rm(list_ids,nbs)
            shiny::incProgress(1/(length(res)+4))
          }

          couples_poids <- couples_poids[order(couples_poids$NB),]
          poids_troncon <- unique(couples_poids[,c("IDS","NB")])
          graph_poids <- list()
          shiny::incProgress(1/(length(res)+4))

          graph_poids <- lapply(1:nrow(poids_troncon), function(x){

            un_poids <- couples_poids[couples_poids$NB %in% poids_troncon[x,"NB"],c("X1","Y1","X2","Y2")]
            aa <- unlist(apply(un_poids, 1, list), recursive=F)
            aa <- lapply(aa, function(y) matrix(as.numeric(y),byrow=T,ncol=2))

            sf::st_sf(geometry=sf::st_sfc(sf::st_line_merge(sf::st_combine(sf::st_geometry(sf::st_multilinestring(aa)))), crs=4326),weight=poids_troncon[x,"NB"],idTrace=poids_troncon[x,"IDS"])

          })

          graph_poids <- do.call(rbind,graph_poids)
          names(graph_poids) <- c("weight","idTrace","geometry")
          if(length(unique(graph_poids$idTrace)) == 1)
          {
            if(unique(graph_poids$idTrace) == 0) graph_poids <- graph_poids[,c("weight","geometry")]
          }
          graph_poids <- sf::st_transform(graph_poids, crs=4326)

          shiny::incProgress(1/(length(res)+4))
        })

        shiny::removeModal()
      }else
      {
        pb3 <- progress::progress_bar$new(
          format = "Calcul des routes proportionnelles... : [:bar] :percent :elapsed",
          total = length(res)+4, clear = FALSE, width= 60
        )

        pb3$tick(0)

        couples <- lapply(1:length(res), function(x){

          coord_un_trace <- unique(res[[x]][,c(ncol(res[[x]])-1, ncol(res[[x]]))])
          coord_un_trace <- data.frame(id=x,coord_un_trace[-nrow(coord_un_trace),],coord_un_trace[-1,])
          names(coord_un_trace) <- c("id","X1","Y1","X2","Y2")

          pb3$tick()
          coord_un_trace
        })

        couples <- do.call(rbind,couples)

        if(length(nbFlux) == 1 && nbFlux == 1){
          couples_poids <- couples %>%
            dplyr::group_by(.data$X1,.data$Y1,.data$X2,.data$Y2) %>%
            dplyr::summarise(NB = dplyr::n())

          couples_poids$IDS <- 0
          pb3$tick()
        }else{
          couples_poids <- couples %>%
            dplyr::group_by(.data$X1,.data$Y1,.data$X2,.data$Y2) %>%
            dplyr::mutate(IDS = paste(unique(.data$id), collapse = "-")) %>%
            dplyr::mutate(IDS = paste0(.data$IDS,"-0")) %>%
            dplyr::group_by(.data$X1,.data$Y1,.data$X2,.data$Y2,.data$IDS) %>%
            dplyr::ungroup()

          list_ids <- lapply(sapply(stringr::str_split(couples_poids$IDS, "-"), function(x) x),FUN=as.numeric)
          nbs <- round(sapply(list_ids, function(x) sum(nbFlux[x], na.rm = TRUE)),0)

          couples_poids <- couples_poids %>% dplyr::mutate(NB = nbs)

          rm(list_ids,nbs)

          pb3$tick()
        }

        couples_poids <- couples_poids[order(couples_poids$NB),]
        poids_troncon <- unique(couples_poids[,c("IDS","NB")])
        graph_poids <- list()
        pb3$tick()

        graph_poids <- lapply(1:nrow(poids_troncon), function(x){

          un_poids <- couples_poids[couples_poids$NB %in% poids_troncon[x,"NB"],c("X1","Y1","X2","Y2")]
          aa <- unlist(apply(un_poids, 1, list), recursive=F)
          aa <- lapply(aa, function(y) matrix(as.numeric(y),byrow=T,ncol=2))

          sf::st_sf(geometry=sf::st_sfc(sf::st_line_merge(sf::st_combine(sf::st_geometry(sf::st_multilinestring(aa)))), crs=4326),weight=poids_troncon[x,"NB"],idTrace=poids_troncon[x,"IDS"])

        })
        pb3$tick()

        graph_poids <- do.call(rbind,graph_poids)
        names(graph_poids) <- c("weight","idTrace","geometry")
        if(length(unique(graph_poids$idTrace)) == 1)
        {
          if(unique(graph_poids$idTrace) == 0) graph_poids <- graph_poids[,c("weight","geometry")]
        }
        graph_poids <- sf::st_transform(graph_poids, crs=4326)

        pb3$tick()
      }
    }

    #################################
    ### Visualisation de la carte ###
    #################################

    if(is.null(getOption("urlTemplate")) | is.null(getOption("attribution")))
    {
      options(urlTemplate = "http://{s}.tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png")
      options(attribution = paste0("Insee, distancier Metric-OSRM, \u00a9 les contributeurs d'<a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> et du <a href='http://project-osrm.org/'>projet OSRM</a>"))
    }

    m <- leaflet::leaflet(padding = 0,
                          options = leaflet::leafletOptions(
                            preferCanvas = TRUE,
                            transition = 2
                          )
    ) %>%

      leaflet::addTiles(urlTemplate = getOption("urlTemplate"),
                        attribution = getOption("attribution"),
                        options = leaflet::tileOptions(opacity = opaciteOSM)) %>%

      leaflet::fitBounds(lng1 = as.numeric(sf::st_bbox(graph_poids)[1]),
                         lat1 = as.numeric(sf::st_bbox(graph_poids)[2]),
                         lng2 = as.numeric(sf::st_bbox(graph_poids)[3]),
                         lat2 = as.numeric(sf::st_bbox(graph_poids)[4])) %>%

      leaflet::addScaleBar(position = 'bottomright',
                           options = leaflet::scaleBarOptions(metric = TRUE, imperial = FALSE))

    if(!is.null(fonds))
    {
      for(i in 1:length(fonds))
      {
        fonds[[i]] <- sf::st_transform(fonds[[i]], crs = 4326)

        m <- leaflet::addPolygons(map = m, data = fonds[[i]], opacity = 1,
                                  stroke = TRUE, color = "#606060", weight = 1,
                                  options = leaflet::pathOptions(clickable = F),
                                  fill = F,
                                  group = "carte_routes_prop_init")
      }
    }
  }else # Contexte shiny/proxy
  {
    graph_poids <- res[[1]]

    m <- mapProxy
  }

  if(interactive) shiny::showModal(shiny::modalDialog(shiny::HTML("<i class=\"fa fa-spinner fa-spin fa-2x fa-fw\"></i><font size=+1>Affichage de la carte...</font> "), size = "m", footer = NULL, style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))

  if(length(unique(graph_poids$weight)) > 1)
  {
    suppressWarnings(bornes <- classInt::classIntervals(as.numeric(graph_poids$weight),nbLargeurs,style="kmeans",rtimes=10,intervalClosure="left"))
    bornes <- bornes$brks

    for(i in 1:(length(bornes)-1))
    {
      graph_poids[graph_poids$weight>=bornes[i] & graph_poids$weight<bornes[i+1],"classes"] <- i
    }
    graph_poids[graph_poids$weight==bornes[i+1],"classes"] <- i
  }else
  {
    graph_poids[,"classes"] <- 1
  }

  graph_poids$col <- "#002C00"

  if("idTrace" %in% names(graph_poids))
  {
    graph_poids <- graph_poids[,c("weight","classes","col","idTrace","geometry")]
  }else
  {
    graph_poids <- graph_poids[,c("weight","classes","col","geometry")]
  }

  graph_poids <- suppressWarnings(sf::st_collection_extract(graph_poids, type = "LINESTRING"))

  m <- leaflet::addPolylines(map = m, data = graph_poids,
                             color = graph_poids$col,
                             weight = graph_poids$classes,
                             options = leaflet::pathOptions(clickable = T),
                             popup = paste0("Nombre de passages : ", graph_poids$weight),
                             opacity = 1,
                             group = "carte_routes_prop")

  if(interactive) shiny::removeModal()

  return(list(m, graph_poids))
}
