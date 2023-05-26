#' @name metricOsrmIso
#'
#' @title Calculer des isochrones ou des isodistances autour d'un ou plusieurs points
#'
#' @description La fonction metricOsrmIso permet de créer des courbes d’isochrones ou d'isodistances mesurant l’accessibilité
#' en temps de parcours ou en distance autour d’un ou plusieurs points.
#'
#' @param loc vecteur numérique (id/lon/lat ou lon/lat), data.frame (3 colonnes id/lon/lat ou 2 colonnes lon/lat), objet sf ou sp (SpatialPointsDataFrame ou SpatialPolygonsDataFrame) précisant le(s) point(s)
#' de départ, centre(s) des isochrones.
#' @param breaks vecteur numérique. Séquence de valeurs numériques indiquant les temps en minutes relatifs aux courbes isochrones ou les distances en kilomètres relatives aux courbes isodistances.
#' @param res numérique. Valeur numérique indiquant la résolution des courbes isochrones, la précision des contours.
#' @param returnclass texte. "sf" (par défaut) ou "sp". Renvoie un objet sf (MULTIPOLYGON) ou sp (SpatialPolygonDataFrame).
#' @param fusion booléen. Si TRUE (par défaut), fusion des courbes isochrones si il y a plusieurs points loc (voir details).
#' @param courbes texte. "isochrones" (par défaut) ou "isodistances". Choix des courbes iso : isochrones ou isodistances.
#' @param exclude texte. Permet aux trajets d'éviter les autoroutes (“motorway”), les péages (“toll”) ou les ferries (“ferry”). Par défaut NULL.
#' @param interactive booléen. Choix du contexte d'exécution. Si TRUE, contexte shiny. Par défaut FALSE.
#'
#' @return Une liste d'un ou plusieurs objets sf (MULTIPOLYGON) ou sp (SpatialPolygonDataFrame).
#'
#' @details Le temps de calcul peut être important si le nombre de points de départ (loc) et la résolution (res) sont élevés.
#'
#' Le nombre de couples calculé est égal au nombre de points de départ x (résolution)².
#'
#' Si fusion = TRUE, une résolution élevée peut être nécessaire si les points de départ sont très distants.
#'
#' Le nombre de breaks n’influe pas sur le temps de calcul.
#'
#' Si fusion = TRUE (par défaut), les courbes d'isochrones ou d'isodistances fusionnent pour former autant de polygones que de classes (nombre de breaks).
#' Cas d'utilisation : pour mesurer l'accessibilité d'un type d'équipement le plus proche en temps.
#'
#' Si fusion = FALSE, les courbes d'isochrones ou d'isodistances sont calculées autour de chaque point.
#' Cas d'utilisation : pour mesurer séparément l'accessibilité d'un ou plusieurs équipements.
#'
#' @importFrom sf st_sf st_sfc st_as_sf st_crs st_centroid st_geometry read_sf st_transform st_bbox st_coordinates st_distance st_buffer st_union st_intersects st_point st_polygon st_contains st_dimension st_make_valid st_collection_extract
#' @importFrom RJSONIO fromJSON
#' @importFrom methods is as
#' @importFrom progress progress_bar
#' @importFrom isoband iso_to_sfg
#' @export
#'
#' @examples
#' # Specification d'un serveur osrm obligatoire pour executer les exemples
#' options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
#'
#' # Specification du profil
#' options(osrm.profile = "driving")
#'
#' # Calcul d'isochrones a partir d'un point.
#' iso1 <- metricOsrmIso(loc = data.frame(lon = 4.92,
#'                                        lat = 46.15),
#'                       courbes = "isochrones")
#'
#' plot(sf::st_geometry(iso1[[1]]))
#'
#' # Calcul de deux isochrones separees.
#' iso2 <- metricOsrmIso(loc = data.frame(lon = c(4.92,4.98),
#'                                        lat = c(46.15,46.30)),
#'                       breaks = c(0,30,60),
#'                       res = 20,
#'                       fusion = FALSE,
#'                       courbes = "isochrones")
#'
#' plot(sf::st_geometry(iso2[[2]]), border = "blue")
#' plot(sf::st_geometry(iso2[[1]]), border = "red", add = TRUE)
#'
#' # Calcul d'isochrones fusionnees a partir de deux points.
#' iso3 <- metricOsrmIso(loc = data.frame(lon = c(4.92,4.98),
#'                                        lat = c(46.15,46.30)),
#'                       breaks = c(0,30,60),
#'                       res = 20,
#'                       fusion = TRUE,
#'                       courbes = "isochrones")
#'
#' plot(sf::st_geometry(iso3[[1]]))
#'
#' # Calcul d'isodistances fusionnees a partir de deux points.
#' iso4 <- metricOsrmIso(loc = data.frame(lon = c(4.92,4.98),
#'                                        lat = c(46.15,46.30)),
#'                       breaks = c(0,30,60),
#'                       res = 20,
#'                       fusion = TRUE,
#'                       courbes = "isodistances")
#'
#' plot(sf::st_geometry(iso4[[1]]))
#'
metricOsrmIso <-
function (loc, breaks = seq(from = 0, to = 60, length.out = 5), res = 30, returnclass = "sf", fusion = TRUE, courbes = "isochrones", exclude = NULL, interactive = FALSE)
{
  oprj <- NA
  if (methods::is(loc, "Spatial")) {
    loc <- sf::st_as_sf(loc)
  }
  if (testSf(loc)) {
    oprj <- sf::st_crs(loc)
    loc <- suppressWarnings(sf::st_centroid(loc))
  }else if (methods::is(loc, "data.frame")){
    if(dim(loc)[2]==2)
    {
      loc <- sf::st_sf(id = 1, geometry = sf::st_geometry(sf::st_as_sf(data.frame(lon = loc[,1], lat = loc[,2]), coords = c("lon", "lat"), crs = 4326)), stringsAsFactors = FALSE)
    }else if(dim(loc)[2]==3)
    {
      loc <- sf::st_sf(id = loc[,1], geometry = sf::st_geometry(sf::st_as_sf(data.frame(lon = loc[,2], lat = loc[,3]), coords = c("lon", "lat"), crs = 4326)), stringsAsFactors = FALSE)
    }else
    {
      e <- "Le data.frame doit comporter 2 colonnes lon et lat ou 3 colonnes id, lon et lat."
      stop(e, call. = FALSE)
    }
  }else if (methods::is(loc, "numeric")){
    if(length(loc)==2)
    {
      loc <- sf::st_sf(id = 1, geometry = sf::st_geometry(sf::st_as_sf(data.frame(lon = loc[1], lat = loc[2]), coords = c("lon", "lat"), crs = 4326)), stringsAsFactors = FALSE)
    }else if(length(loc)==3)
    {
      loc <- sf::st_sf(id = loc[1], geometry = sf::st_geometry(sf::st_as_sf(data.frame(lon = loc[2], lat = loc[3]), coords = c("lon", "lat"), crs = 4326)), stringsAsFactors = FALSE)
    }else
    {
      e <- "Le vecteur numeric doit comporter 2 valeurs lon et lat ou 3 valeurs id, lon et lat."
      stop(e, call. = FALSE)
    }
  }else
  {
    e <- "loc doit etre un objet Spatial, data.frame ou vecteur numeric"
    stop(e, call. = FALSE)
  }

  loc_dt <- sfToDf(loc)

  loc <- sf::st_transform(loc, 3857)
  breaks <- unique(sort(breaks))
  tmax <- max(breaks)
  if (options("osrm.profile") %in% c("walk","foot","walking","routed-foot")) {
    speed = 10 * 1000/60
  }
  if (options("osrm.profile") %in% c("bike","bicycle","cycling","routed-bike")) {
    speed = 20 * 1000/60
  }
  if (options("osrm.profile") %in% c("car","driving","routed-car")) {
    speed = 130 * 1000/60
  }
  dmax <- tmax * speed

  if(!is.null(getOption("osrm.server")))
  {
    if(getOption("osrm.server") %in% url)
    {
      emprisePbf <- emprise_pbf
      suppressWarnings(sf::st_crs(emprisePbf) <- 3857)
    }else
    {
      emprisePbf <- NULL
    }
  }else
  {
    emprisePbf <- NULL
  }

  if(nrow(loc)>1)
  {
    bbox <- sf::st_bbox(loc)
    long_x <- bbox[3]-bbox[1]
    long_y <- bbox[4]-bbox[2]
    cote <- max(long_x,long_y)

    lon <- bbox[1]+long_x/2
    lat <- bbox[2]+long_y/2
  }else
  {
    lon <- sf::st_coordinates(loc)[1]
    lat <- sf::st_coordinates(loc)[2]
    cote <- 0
  }

  grid <- data.frame(lon = lon, lat = lat)
  grid <- sf::st_as_sf(grid, coords = c("lon", "lat"), crs = 3857)

  sgrid <- rgrid(loc = grid[1,], dmax = dmax+cote/2, res = res)

  if(!is.null(emprisePbf))
  {
    sgrid_keep <- sf::st_intersects(sgrid, emprisePbf)
    sgrid_select <- sgrid[lengths(sgrid_keep) > 0,]
  }else
  {
    sgrid_select <- sgrid
  }

  lsgr <- nrow(sgrid_select)
  f500 <- lsgr%/%500
  r500 <- lsgr%%500

  matDurDist <- matrix(nrow = nrow(loc_dt), ncol = nrow(sgrid_select))

  if(!interactive)
  {
    pb <- progress::progress_bar$new(
      format = paste0("Etape 1/2 : calcul en cours de ",nrow(loc_dt)*nrow(sgrid_select)," couples -  [:bar] :percent :elapsed"),
      total = nrow(loc_dt), clear = FALSE, width= 80
    )

    pb$tick(0)

    if(any(courbes %in% "isochrones"))
    {
      measure <- "duree"
    }else if(length(courbes) == 1)
    {
      if(courbes == "isodistances")
      {
        measure <- "distance"
      }else
      {
        measure <- "duree"
      }
    }else
    {
      courbes <- "isochrones"
      measure <- "duree"
    }

    for(i in 1:nrow(loc_dt))
    {
      listDurDist <- list()
      listDest <- list()

      if (f500 > 0) {

        for (j in 1:f500) {

          st <- (j - 1) * 500 + 1
          en <- j * 500

          dmat <- osrmTable_1n_n1(src = loc_dt[i,], dst = sfToDf(sgrid_select[st:en,]), duree = TRUE, distance = TRUE, exclude = exclude)

          if(any(dmat$duree < 0))
          {
            dmat[dmat$duree < 0, c("duree", "distance")] <- NA
            if(any(is.na(dmat$duree)))
            {
              list_dmat <- list()
              for(k in 1:nrow(sfToDf(sgrid_select[(en + 1):(en + 500), ])))
              {
                list_dmat[[k]] <- osrmTable_1n_n1(src = loc_dt[i,], dst = sfToDf(sgrid_select[st:en,])[k,], duree = TRUE, distance = TRUE, exclude = exclude)
              }
              dmat <- do.call(rbind, list_dmat)
              if(any(dmat$duree < 0))
              {
                dmat[dmat$duree < 0, c("duree", "distance")] <- NA
              }
            }
          }

          listDurDist[[j]] <- dmat[,measure]
          listDest[[j]] <- data.frame(lon = dmat$lonDst, lat = dmat$latDst, stringsAsFactors = FALSE)
        }
        if (r500 > 0) {

          dmat <- osrmTable_1n_n1(src = loc_dt[i,], dst = sfToDf(sgrid_select[(en + 1):(en + r500), ]), duree = TRUE, distance = TRUE, exclude = exclude)

          if(any(dmat$duree < 0))
          {
            dmat[dmat$duree < 0, c("duree", "distance")] <- NA
            if(any(is.na(dmat$duree)))
            {
              list_dmat <- list()
              for(k in 1:nrow(sfToDf(sgrid_select[(en + 1):(en + r500), ])))
              {
                list_dmat[[k]] <- osrmTable_1n_n1(src = loc_dt[i,], dst = sfToDf(sgrid_select[(en + 1):(en + r500), ])[k,], duree = TRUE, distance = TRUE, exclude = exclude)
              }
              dmat <- do.call(rbind, list_dmat)
              if(any(dmat$duree < 0))
              {
                dmat[dmat$duree < 0, c("duree", "distance")] <- NA
              }
            }
          }

          listDurDist[[j+1]] <- dmat[,measure]
          listDest[[j+1]] <- data.frame(lon = dmat$lonDst, lat = dmat$latDst, stringsAsFactors = FALSE)
        }
      }else {

        dmat <- osrmTable_1n_n1(src = loc_dt[i,], dst = sfToDf(sgrid_select), duree = TRUE, distance = TRUE, exclude = exclude)

        if(any(dmat$duree < 0))
        {
          dmat[dmat$duree < 0, c("duree", "distance")] <- NA
          if(any(is.na(dmat$duree)))
          {
            list_dmat <- list()
            for(k in 1:nrow(sfToDf(sgrid_select)))
            {
              list_dmat[[k]] <- osrmTable_1n_n1(src = loc_dt[i,], dst = sfToDf(sgrid_select)[k,], duree = TRUE, distance = TRUE, exclude = exclude)
            }
            dmat <- do.call(rbind, list_dmat)
            if(any(dmat$duree < 0))
            {
              dmat[dmat$duree < 0, c("duree", "distance")] <- NA
            }
          }
        }

        listDurDist[[1]] <- dmat[,measure]
        listDest[[1]] <- data.frame(lon = dmat$lonDst, lat = dmat$latDst, stringsAsFactors = FALSE)
      }
      matDurDist[i,] <- do.call(c, listDurDist)
      destinations <- do.call(rbind, listDest)

      pb$tick()
    }
  }else
  {
    shiny::withProgress(message = paste0("Etape 1/2 : calcul en cours de ",nrow(loc_dt)*nrow(sgrid_select)," couples"),{

      shiny::incProgress(1/nrow(loc_dt))

      if(any(courbes %in% "isochrones"))
      {
        measure <- "duree"
      }else if(length(courbes) == 1)
      {
        if(courbes == "isodistances")
        {
          measure <- "distance"
        }else
        {
          measure <- "duree"
        }
      }else
      {
        courbes <- "isochrones"
        measure <- "duree"
      }

      for(i in 1:nrow(loc_dt))
      {
        listDurDist <- list()
        listDest <- list()

        if (f500 > 0) {
          for (j in 1:f500) {

            st <- (j - 1) * 500 + 1
            en <- j * 500

            dmat <- osrmTable_1n_n1(src = loc_dt[i,], dst = sfToDf(sgrid_select[st:en,]), duree = TRUE, distance = TRUE, exclude = exclude)

            if(any(dmat$duree < 0))
            {
              dmat[dmat$duree < 0, c("duree", "distance")] <- NA
              if(any(is.na(dmat$duree)))
              {
                list_dmat <- list()
                for(k in 1:nrow(sfToDf(sgrid_select[(en + 1):(en + 500), ])))
                {
                  list_dmat[[k]] <- osrmTable_1n_n1(src = loc_dt[i,], dst = sfToDf(sgrid_select[st:en,])[k,], duree = TRUE, distance = TRUE, exclude = exclude)
                }
                dmat <- do.call(rbind, list_dmat)
                if(any(dmat$duree < 0))
                {
                  dmat[dmat$duree < 0, c("duree", "distance")] <- NA
                }
              }
            }

            listDurDist[[j]] <- dmat[,measure]
            listDest[[j]] <- data.frame(lon = dmat$lonDst, lat = dmat$latDst, stringsAsFactors = FALSE)
          }
          if (r500 > 0) {

            dmat <- osrmTable_1n_n1(src = loc_dt[i,], dst = sfToDf(sgrid_select[(en + 1):(en + r500), ]), duree = TRUE, distance = TRUE, exclude = exclude)

            if(any(dmat$duree < 0))
            {
              dmat[dmat$duree < 0, c("duree", "distance")] <- NA
              if(any(is.na(dmat$duree)))
              {
                list_dmat <- list()
                for(k in 1:nrow(sfToDf(sgrid_select[(en + 1):(en + r500), ])))
                {
                  list_dmat[[k]] <- osrmTable_1n_n1(src = loc_dt[i,], dst = sfToDf(sgrid_select[(en + 1):(en + r500), ])[k,], duree = TRUE, distance = TRUE, exclude = exclude)
                }
                dmat <- do.call(rbind, list_dmat)
                if(any(dmat$duree < 0))
                {
                  dmat[dmat$duree < 0, c("duree", "distance")] <- NA
                }
              }
            }

            listDurDist[[j+1]] <- dmat[,measure]
            listDest[[j+1]] <- data.frame(lon = dmat$lonDst, lat = dmat$latDst, stringsAsFactors = FALSE)
          }
        }else {

          dmat <- osrmTable_1n_n1(src = loc_dt[i,], dst = sfToDf(sgrid_select), duree = TRUE, distance = TRUE, exclude = exclude)

          if(any(dmat$duree < 0))
          {
            dmat[dmat$duree < 0, c("duree", "distance")] <- NA
            if(any(is.na(dmat$duree)))
            {
              list_dmat <- list()
              for(k in 1:nrow(sfToDf(sgrid_select)))
              {
                list_dmat[[k]] <- osrmTable_1n_n1(src = loc_dt[i,], dst = sfToDf(sgrid_select)[k,], duree = TRUE, distance = TRUE, exclude = exclude)
              }
              dmat <- do.call(rbind, list_dmat)
              if(any(dmat$duree < 0))
              {
                dmat[dmat$duree < 0, c("duree", "distance")] <- NA
              }
            }
          }

          listDurDist[[1]] <- dmat[,measure]
          listDest[[1]] <- data.frame(lon = dmat$lonDst, lat = dmat$latDst, stringsAsFactors = FALSE)
        }
        matDurDist[i,] <- do.call(c, listDurDist)
        destinations <- do.call(rbind, listDest)

        shiny::incProgress(1/nrow(loc_dt))
      }
    })
  }

  if(measure == "duree")
  {
    if(fusion)
    {
      measures <- suppressWarnings(matrix(apply(matDurDist, 2, function(x) min(x, na.rm=T))/60, nrow=1))
      if(any(measures %in% "Inf")) measures[measures %in% "Inf"] <- NA
    }else
    {
      measures <- matDurDist/60
    }
  }else
  {
    if(fusion)
    {
      measures <- suppressWarnings(matrix(apply(matDurDist, 2, function(x) min(x, na.rm=T))/1000, nrow=1))
      if(any(measures %in% "Inf")) measures[measures %in% "Inf"] <- NA
    }else
    {
      measures <- matDurDist/1000
    }
  }

  if(!interactive)
  {
    pb <- progress::progress_bar$new(
      format = paste0("Etape 2/2 : formation de ",dim(measures)[1]," ", courbes," -  [:bar] :percent :elapsed"),
      total = dim(measures)[1], clear = FALSE, width= 80
    )

    pb$tick(0)

    list_iso <- list()

    for(i in 1:dim(measures)[1])
    {
      rpt <- sf::st_as_sf(destinations, coords = c("lon", "lat"), crs = 4326)
      rpt <- sf::st_transform(rpt, sf::st_crs(loc))
      rpt$measures <- measures[i,]
      if(any(is.na(rpt$measures))) rpt <- rpt[!is.na(rpt$measures),]
      b <- as.numeric(sf::st_distance(sgrid[1, ], sgrid[2, ])/2)
      xx <- st_make_grid_metric(x = sf::st_buffer(sf::st_union(sgrid), b), n = c(res,res))
      bbox_rpt <- sf::st_bbox(rpt)
      sfbbox_rpt <- sf::st_sfc(sf::st_polygon(list(rbind(c(bbox_rpt[1]-50000, bbox_rpt[2]-50000),
                                                         c(bbox_rpt[3]+50000, bbox_rpt[2]-50000),
                                                         c(bbox_rpt[3]+50000, bbox_rpt[4]+50000),
                                                         c(bbox_rpt[1]-50000, bbox_rpt[4]+50000),
                                                         c(bbox_rpt[1]-50000, bbox_rpt[2]-50000)))),
                               crs = sf::st_crs(xx))

      inter <- sf::st_intersects(xx, sfbbox_rpt)
      xx <- xx[lengths(inter) > 0,]
      if(length(xx) == 0)
      {
        e <- "Aucun r\u00e9seau routier n'est disponible pour le calcul d'isocourbes"
        stop(e, call. = FALSE)
      }
      inter <- sf::st_intersects(sgrid, xx)
      sgrid2 <- sgrid[lengths(inter) > 0,]
      inter <- sf::st_contains(xx, rpt)
      sgrid2$measures <- unlist(lapply(inter, function(x) mean(rpt[["measures"]][x], na.rm = TRUE)))

      if(nrow(sgrid2) > 0)
      {
        sgrid2[is.nan(sgrid2$measures), "measures"] <- tmax + 1
        sgrid2[sgrid2$measures > tmax, "measures"] <- tmax + 1
      }else
      {
        e <- "Aucun r\u00e9seau routier n'est disponible pour le calcul d'isocourbes"
        stop(e, call. = FALSE)
      }

      if(min(sgrid2$measures) > tmax) {
        e <- "Utilisez des 'breaks' plus faibles ou augmenter 'res'"
        stop(e, call. = FALSE)
      }
      iso <- isopoly(x = sgrid2, breaks = breaks, var = "measures")
      if (!is.na(oprj)) {
        iso <- sf::st_transform(x = iso, oprj)
      }else {
        iso <- sf::st_transform(x = iso, 4326)
      }

      if (returnclass == "sp") {
        iso <- suppressWarnings(methods::as(iso, "Spatial"))
      }

      list_iso[[i]] <- iso

      pb$tick()
    }
  }else
  {
    shiny::withProgress(message = paste0("Etape 2/2 : formation de ",dim(measures)[1]," ", courbes),{

      shiny::incProgress(1/dim(measures)[1])

      list_iso <- list()

      for(i in 1:dim(measures)[1])
      {
        rpt <- sf::st_as_sf(destinations, coords = c("lon", "lat"), crs = 4326)
        rpt <- sf::st_transform(rpt, sf::st_crs(loc))
        rpt$measures <- measures[i,]
        if(any(is.na(rpt$measures))) rpt <- rpt[!is.na(rpt$measures),]
        b <- as.numeric(sf::st_distance(sgrid[1, ], sgrid[2, ])/2)
        xx <- st_make_grid_metric(x = sf::st_buffer(sf::st_union(sgrid), b), n = c(res,res))
        bbox_rpt <- sf::st_bbox(rpt)
        sfbbox_rpt <- sf::st_sfc(sf::st_polygon(list(rbind(c(bbox_rpt[1]-50000, bbox_rpt[2]-50000),
                                                           c(bbox_rpt[3]+50000, bbox_rpt[2]-50000),
                                                           c(bbox_rpt[3]+50000, bbox_rpt[4]+50000),
                                                           c(bbox_rpt[1]-50000, bbox_rpt[4]+50000),
                                                           c(bbox_rpt[1]-50000, bbox_rpt[2]-50000)))),
                                 crs = sf::st_crs(xx))

        inter <- sf::st_intersects(xx, sfbbox_rpt)
        xx <- xx[lengths(inter) > 0,]
        if(length(xx) == 0)
        {
          e <- "Aucun r\u00e9seau routier n'est disponible pour le calcul d'isocourbes"
          stop(e, call. = FALSE)
        }
        inter <- sf::st_intersects(sgrid, xx)
        sgrid2 <- sgrid[lengths(inter) > 0,]

        inter <- sf::st_contains(xx, rpt)
        sgrid2$measures <- unlist(lapply(inter, function(x) mean(rpt[["measures"]][x], na.rm = TRUE)))
        if(nrow(sgrid2) > 0)
        {
          sgrid2[is.nan(sgrid2$measures), "measures"] <- tmax + 1
          sgrid2[sgrid2$measures > tmax, "measures"] <- tmax + 1
        }else
        {
          e <- "Aucun r\u00e9seau routier n'est disponible pour le calcul d'isocourbes"
          stop(e, call. = FALSE)
        }
        if (min(sgrid2$measures) > tmax) {
          e <- "Utilisez des 'breaks' plus faibles ou augmenter 'res'"
          stop(e, call. = FALSE)
        }
        iso <- isopoly(x = sgrid2, breaks = breaks, var = "measures")
        if (!is.na(oprj)) {
          iso <- sf::st_transform(x = iso, oprj)
        }else {
          iso <- sf::st_transform(x = iso, 4326)
        }

        if (returnclass == "sp") {
          iso <- suppressWarnings(methods::as(iso, "Spatial"))
        }

        list_iso[[i]] <- iso

        shiny::incProgress(1/dim(measures)[1])
      }
    })
  }

  return(list_iso)
}
