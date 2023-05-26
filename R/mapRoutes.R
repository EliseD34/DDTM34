#' @name mapRoutes
#'
#' @title Visualiser le trace des trajets sur une carte
#'
#' @description La fonction mapRoutes permet de visualiser les tracés des routes sur une carte.
#'
#' @param res liste d'objets sp (LINES) ou sf (LINESTRING ou MULTILINESTRING).
#' @param fonds liste d'objets sp ou sf.
#' @param largeurRoute numérique. Largeur de la route.
#' @param opaciteOSM numérique. Entre 0 et 1, opacité du fond OpenStreetMap. Par défaut 0.5.
#' @param mapProxy objet leaflet ou leaflet_proxy. Pour l'intégration des fonctions leaflet dans les applications shiny. Par defaut NULL.
#' @return objet leaflet
#'
#' @details Plus le nombre de routes est important, plus le temps de calcul est élévé.
#'
#' En entrée, l'objet res doit être une liste d'objet géométrique composé de lignes LINES si sp ou MULTILINESTRING si sf.
#'
#' Cet objet peut être une liste de résultats de la fonction metricOsrmRoute renvoyant des tracés de route. Les résultats peuvent être des objets sp ou sf
#' selon le choix du paramètre returnclass de metricOsrmRoute.
#'
#' L'objet res peut également être le résultat de la fonction mapRoutesProp (2ème élément de la liste retournée).
#'
#' Cet objet comportera alors deux variables weight et classes (logiquement filtrés) pour représenter l'épaisseur des tracés sur la carte selon leur poids (voir fonction mapRoutesProp).
#' Dans ce cas, l'argument largeurRoute sera ignoré.
#'
#' En sortie, la fonction mapRoutes renvoie la carte leaflet qu'il est possible d'afficher directement dans le viewer.
#'
#' @importFrom sf st_as_sf st_bbox st_transform
#' @importFrom leaflet leaflet invokeMethod fitBounds addScaleBar addPolygons addPolylines leafletOptions tileOptions getMapData pathOptions scaleBarOptions clearGroup
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
#' # Calcul de deux traces de route.
#' sf1_route_f <- metricOsrmRoute(src = data.frame(lon = 4.92,
#'                                                 lat = 46.15),
#'                               dst = data.frame(lon = 4.72,
#'                                                 lat = 45.92),
#'                               overview = "full",
#'                               returnclass = "sf")
#'
#' sf2_route_f <- metricOsrmRoute(src = data.frame(lon = 4.92,
#'                                                 lat = 46.15),
#'                               dst = data.frame(lon = 4.82,
#'                                                 lat = 45.85),
#'                               overview = "full",
#'                               returnclass = "sf")
#'
#' # Visualisation des deux traces de route.
#' mapRoutes(res = list(sf1_route_f, sf2_route_f))
#'
mapRoutes <- function(res, fonds = NULL, largeurRoute = 1, opaciteOSM = 0.5, mapProxy = NULL)
{
  if(!is.list(res))
  {
    stop(simpleError("res doit etre une liste d'objets sp ou sf."))
  }
  if(!any(testSf(res[[1]]) | methods::is(res[[1]], "Spatial")))
  {
    stop(simpleError("res doit etre une liste d'objets sp ou sf."))
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
                          group = "carte_routes")
    }
  }

  # si coord est un objet sp, on le transforme d'abord en objet sf
  if (methods::is(res[[1]], "Spatial")) {
    for(i in length(res))
    {
      res[[i]] <- sf::st_as_sf(x = res[[i]])
    }
  }

  res <- do.call(rbind,res)

  if(length(which(names(res) %in% c("weight", "classes"))) == 2)
  {
    weight <- res$classes
  }else
  {
    weight <- largeurRoute
  }

  if(length(which(names(res) %in% "col")) == 1)
  {
    col <- res$col
  }else
  {
    col <- "darkslateblue"
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

    leaflet::fitBounds(lng1 = as.numeric(sf::st_bbox(res)[1]),
                       lat1 = as.numeric(sf::st_bbox(res)[2]),
                       lng2 = as.numeric(sf::st_bbox(res)[3]),
                       lat2 = as.numeric(sf::st_bbox(res)[4])) %>%

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
                                  group = "carte_routes_init")
      }
    }
  }else # Contexte shiny/proxy
  {
    m <- mapProxy
  }

  m <- leaflet::addPolylines(map = m, data = res, opacity = 100,
                             stroke = TRUE, color = col, weight = weight,
                             options = leaflet::pathOptions(clickable = F),
                             fill = F,
                             group = "carte_routes")

  return(m)
}
