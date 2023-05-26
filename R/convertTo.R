#' @name convertTo
#'
#' @title Convertir des coordonnees d'un systeme geographique a un autre et dans un format different (sp, sf ou data.frame)
#'
#' @description La fonction convertTo permet de convertir un objet sf, sp ou un data.frame de coordonnées dans un système géographique différent et dans un autre format, au choix entre sp, sf ou data.frame.
#'
#' @param from objet sf ou sp avec un CRS valide, ou un data.frame de deux colonnes (coordonnées) ou 3 colonnes (id en 1ère colonne puis les coordonnées).
#' @param to texte. Type d'objet souhaité en sortie : "sf", "sp" ou "data.frame". Si NULL (par défaut), le type de sortie sera le même que celui de l'objet en entrée.
#' @param fromEpsg texte ou numérique. Si from est un data.frame, fromEpsg doit correspondre au code EPSG des coordonnées à convertir.
#' @param toEpsg texte ou numérique. Code EPSG souhaité pour l'objet à convertir.
#' @param interactive booléen. Choix du contexte d'exécution. Si TRUE, contexte shiny. Par défaut FALSE.
#'
#' @return Un objet sf, sp ou un data.frame
#'
#' @details Le code EPSG du système WGS84 est communément 4326.
#'
#' En France métropolitaine, le système de projection en vigueur est le Lambert 93, code EPSG 2154.
#'
#' Pour la Guadeloupe et la Martinique, le code EPSG est 5490 pour la projection UTM 20 N.
#'
#' Pour la Guyane, le code EPSG est 2972 pour la projection UTM 22 N.
#'
#' Pour la Réunion, le code EPSG est 2975 pour la projection UTM 40 S.
#'
#' Pour Mayotte, le code EPSG est 4471 pour la projection UTM 38 S.
#'
#' La projection Mercator pour représenter la mappemonde a pour code EPSG 3395.
#'
#' @importFrom sf st_sf st_sfc st_as_sf st_crs st_geometry st_transform st_coordinates st_point as_Spatial
#' @importFrom shiny withProgress incProgress
#' @importFrom methods is
#' @export
#'
#' @examples
#' # Creation d'un data.frame de 4 coordonnees en Lambert 93.
#' coord <- data.frame(id = c(1:4),
#'                     X = c(897740.5,901367.8,874261.9,897740.5),
#'                     Y = c(6272912,6251706,6291801,6272912),
#'                     stringsAsFactors = FALSE)
#'
#' # Conversion des coordonnees en WGS84 (EPSG 4326).
#' # Transformation du data.frame en objet spatial sf.
#' coord_sf_WGS84 <- convertTo(from = coord,
#'                             to = "sf",
#'                             fromEpsg = 2154,
#'                             toEpsg = 4326)
#'
#' # Transformation de l'objet sf en objet spatial sp.
#' coord_sp_WGS84 <- convertTo(from = coord_sf_WGS84,
#'                             to = "sp")
#'
#' # Transformation de l'objet sp en data.frame.
#' # Conversion des coordonnees en Lambert 93 (EPSG 2154).
#' coord_dt_L93 <- convertTo(from = coord_sp_WGS84,
#'                           to = "data.frame",
#'                           toEpsg = 2154)
#'
#' # Creation d'un objet sf : un point en coordonnees Lambert93 (EPSG 2154).
#' objet_sf_points <- sf::st_sf(geometry = sf::st_sfc(
#'                                         sf::st_geometry(
#'                                         sf::st_point(
#'                                         c(897740.5,6272912.0)
#'                                         )),
#'                              crs=2154))
#'
#' # Transformation de l'objet sf en data.frame.
#' # Conversion implicite des coordonnees en WGS84 (EPSG 4326).
#' coord_dt_WGS84 <- convertTo(from = objet_sf_points,
#'                             to = "data.frame")
#'
convertTo <- function(from, to = NULL, fromEpsg = NULL, toEpsg = 4326, interactive = FALSE)
{
  # si from est un objet sp, on le transforme d'abord en objet sf
  if(methods::is(from, "Spatial"))
  {
    from <- sf::st_as_sf(x = from)
    typeFrom <- "sp"
  }else if(testSf(from))
  {
    typeFrom <- "sf"
  }else if(length(class(from))==1 & any(class(from)=="data.frame"))
  {
    typeFrom <- "data.frame"
  }else
  {
    stop(simpleError(paste0("from doit \u00eatre un objet sp ou sf avec un CRS renseign\u00e9, ou un data.frame de 2 colonnes (coordonn","\u00e9","es) ou 3 colonnes (id et coordonn","\u00e9","es).")))
  }

  if(typeFrom == "data.frame")
  {
    if(!is.null(fromEpsg))
    {
      fromEpsg <- as.numeric(fromEpsg)
    }else # fromEpsg est obligatoire si from est une liste
    {
      stop(simpleError(paste0("Veuillez pr","\u00e9","ciser un code epsg dans fromEpsg correspondant \u00e0 un syst\u00e8me de projection.")))
    }
  }

  if(!is.null(toEpsg))
  {
    toEpsg <- as.numeric(toEpsg)
  }else
  {
    stop(simpleError(paste0("Veuillez pr","\u00e9","ciser un code epsg dans toEpsg correspondant \u00e0 un syst\u00e8me de projection.")))
  }

  # si from est un data.frame de coordonnees
  if(typeFrom=="data.frame")
  {
    if(ncol(from)<2 | ncol(from)>3)
    {
      stop(simpleError(paste0("Le data.frame doit \u00eatre compos\u00e9  de 2 colonnes (les coordonn","\u00e9","es en type num\u00e9rique) ou de 3 colonnes (un id et les coordonn","\u00e9","es en type num\u00e9rique).")))
    }else if(ncol(from)==2)
    {
      col <- 0
    }else if(ncol(from)==3)
    {
      col <- 1
    }else
    {}

    if(!is.numeric(from[,col+1]) | !is.numeric(from[,col+2]))
    {
      stop(simpleError(paste0("Le data.frame doit \u00eatre compos\u00e9  de 2 colonnes (les coordonn","\u00e9","es en type num\u00e9rique) ou de 3 colonnes (un id et les coordonn","\u00e9","es en type num\u00e9rique).")))
    }

    st_un_multipoint = function(x) {
      oprj <- sf::st_crs(x)
      g <- sf::st_geometry(x)
      j <- rep(seq_len(nrow(x)), sapply(g, nrow))
      x <- x[j,]
      sf::st_geometry(x) <- sf::st_sfc(do.call(c, lapply(g, function(geom) lapply(1:nrow(geom), function(i) sf::st_point(geom[i,])))))
      x <- sf::st_sf(x, crs = oprj)
      x
    }

    nb_boucles <- nrow(from) %/% 10000
    reste <- nrow(from) %% 10000
    list_pts_coord <- list()
    i <- 0

    if(!interactive)
    {
      pb <- progress::progress_bar$new(
        format = "Calcul en cours - \u00e9tape 1 [:bar] :percent :elapsed",
        total = nb_boucles, clear = FALSE, width= 60
      )

      pb$tick(0)

      if(nb_boucles > 0)
      {
        for(i in 1:nb_boucles)
        {
          pb$tick()
          subset_from <- from[((i-1)*10000+1):(i*10000),]
          pts_coord <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipoint(matrix(c(as.numeric(subset_from[[col+1]]),as.numeric(subset_from[[col+2]])),ncol=2)))), crs=fromEpsg)
          list_pts_coord[[i]] <- st_un_multipoint(pts_coord)
        }
      }
      if(reste > 0)
      {
        subset_from <- from[(i*10000+1):(i*10000+reste),]
        pts_coord <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipoint(matrix(c(as.numeric(subset_from[[col+1]]),as.numeric(subset_from[[col+2]])),ncol=2)))), crs=fromEpsg)
        list_pts_coord[[i+1]] <- st_un_multipoint(pts_coord)
      }
    }else if(interactive)
    {
      shiny::withProgress(message = "Patientez le temps des calculs - \u00e9tape 1",{

        if(nb_boucles > 0)
        {
          for(i in 1:nb_boucles)
          {
            shiny::incProgress(1/nb_boucles)
            subset_from <- from[((i-1)*10000+1):(i*10000),]
            pts_coord <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipoint(matrix(c(as.numeric(subset_from[[col+1]]),as.numeric(subset_from[[col+2]])),ncol=2)))), crs=fromEpsg)
            list_pts_coord[[i]] <- st_un_multipoint(pts_coord)
          }
        }
        if(reste > 0)
        {
          subset_from <- from[(i*10000+1):(i*10000+reste),]
          pts_coord <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipoint(matrix(c(as.numeric(subset_from[[col+1]]),as.numeric(subset_from[[col+2]])),ncol=2)))), crs=fromEpsg)
          list_pts_coord[[i+1]] <- st_un_multipoint(pts_coord)
        }
      })
    }

    if(length(list_pts_coord) > 10)
    {
      nb_boucles <- length(list_pts_coord) %/% 10
      reste <- length(list_pts_coord) %% 10
      list2_pts_coord <- list()
      i <- 0

      if(!interactive)
      {
        pb <- progress::progress_bar$new(
          format = "Calcul en cours - \u00e9tape 2 [:bar] :percent :elapsed",
          total = nb_boucles, clear = FALSE, width= 60
        )

        pb$tick(0)

        if(nb_boucles > 0)
        {
          for(i in 1:nb_boucles)
          {
            pb$tick()
            list2_pts_coord[[i]] <- do.call(rbind,list_pts_coord[((i-1)*10+1):(i*10)])
          }
        }
        if(reste > 0)
        {
          list2_pts_coord[[i+1]] <- do.call(rbind,list_pts_coord[(i*10+1):(i*10+reste)])
        }
      }else if(interactive)
      {
        shiny::withProgress(message = "Patientez le temps des calculs - \u00e9tape 2",{

          if(nb_boucles > 0)
          {
            for(i in 1:nb_boucles)
            {
              shiny::incProgress(1/nb_boucles)
              list2_pts_coord[[i]] <- do.call(rbind,list_pts_coord[((i-1)*10+1):(i*10)])
            }
          }
          if(reste > 0)
          {
            list2_pts_coord[[i+1]] <- do.call(rbind,list_pts_coord[(i*10+1):(i*10+reste)])
          }
        })
      }
    }else
    {
      list2_pts_coord <- list_pts_coord
    }

    pts_coord <- do.call(rbind,list2_pts_coord)

    if(col==1)
    {
      pts_coord$id <- as.data.frame(from)[,1]
      pts_coord <- pts_coord[,c("id","geometry")]
      from <- pts_coord
    }else if(col == 0)
    {
      from <- pts_coord
    }else
    {}
  }

  coordWGS84 <- sf::st_transform(from, crs = toEpsg)

  if(any(to == "data.frame") | (typeFrom == "data.frame" & is.null(to)))
  {
    if(!any(class(sf::st_geometry(coordWGS84)) %in% "sfc_MULTIPOLYGON"))
    {
      if(ncol(coordWGS84)==1)
      {
        coordWGS84 <- data.frame(lon = as.numeric(clean_coord(sf::st_coordinates(coordWGS84)[,1])), lat = as.numeric(clean_coord(sf::st_coordinates(coordWGS84)[,2])), stringsAsFactors = FALSE)
      }else if(ncol(coordWGS84) > 1)
      {
        coordWGS84 <- data.frame(id = as.data.frame(coordWGS84)[,1], lon = as.numeric(clean_coord(sf::st_coordinates(coordWGS84)[,1])), lat = as.numeric(clean_coord(sf::st_coordinates(coordWGS84)[,2])), stringsAsFactors = FALSE)
      }else
      {}
    }else if(any(class(sf::st_geometry(coordWGS84)) %in% "sfc_MULTIPOLYGON"))
    {
      if(ncol(coordWGS84)==1)
      {
        coordWGS84 <- data.frame(lon = as.numeric(clean_coord(sf::st_coordinates(coordWGS84)[,1])), lat = as.numeric(clean_coord(sf::st_coordinates(coordWGS84)[,2])), stringsAsFactors = FALSE)
      }else if(ncol(coordWGS84) > 1)
      {
        list_coordWGS84 <- list()
        for(i in 1:nrow(coordWGS84))
        {
          list_coordWGS84[[i]] <- data.frame(id = as.data.frame(coordWGS84)[i,1], lon = as.numeric(clean_coord(sf::st_coordinates(coordWGS84[i,])[,1])), lat = as.numeric(clean_coord(sf::st_coordinates(coordWGS84[i,])[,2])), stringsAsFactors = FALSE)
        }
        coordWGS84 <- do.call(rbind,list_coordWGS84)
      }else
      {}
    }
  }else if(any(to == "sp") | (typeFrom == "sp" & is.null(to)))
  {
    coordWGS84 <- suppressWarnings(sf::as_Spatial(coordWGS84))
    if(class(coordWGS84)[1] == "SpatialPointsDataFrame")
    {
      dimnames(coordWGS84@coords)[[2]] <- c("X1","X2")
      coordWGS84@coords[,1] <- as.numeric(clean_coord(coordWGS84@coords[,1]))
      coordWGS84@coords[,2] <- as.numeric(clean_coord(coordWGS84@coords[,2]))
      dimnames(coordWGS84@bbox)[[1]] <- c("X1","X2")
      coordWGS84@bbox[1,] <- as.numeric(clean_coord(coordWGS84@bbox[1,]))
      coordWGS84@bbox[2,] <- as.numeric(clean_coord(coordWGS84@bbox[2,]))
    }else if(class(coordWGS84)[1] == "SpatialPolygonsDataFrame")
    {
      for(i in 1:length(coordWGS84@polygons))
      {
        for(j in 1:length(coordWGS84@polygons[[i]]@Polygons))
        {
          coordWGS84@polygons[[i]]@Polygons[[j]]@coords[,1] <- as.numeric(clean_coord(coordWGS84@polygons[[i]]@Polygons[[j]]@coords[,1]))
          coordWGS84@polygons[[i]]@Polygons[[j]]@coords[,2] <- as.numeric(clean_coord(coordWGS84@polygons[[i]]@Polygons[[j]]@coords[,2]))
        }
      }
      coordWGS84@bbox[1,] <- as.numeric(clean_coord(coordWGS84@bbox[1,]))
      coordWGS84@bbox[2,] <- as.numeric(clean_coord(coordWGS84@bbox[2,]))
    }
  }
  else if(any(to == "sf") | (typeFrom == "sf" & is.null(to)))
  {
    # coordWGS84 est deja un objet sf
  }else
  {
    stop(simpleError(paste0("L'argument to doit \u00eatre \u00e9gal \u00e0 sp, sf, data.frame ou laiss\u00e9 \u00e0 NULL.")))
  }

  return(coordWGS84)
}
