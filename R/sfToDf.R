# Transforme un objet sf en data.frame
#
# La fonction sfToDf permet de transformer un objet sf en data.frame.
#
# @param x objet sf (POINT, MULTIPOINT, POLYGON, MULTIPOLYGON, GEOMETRY ou GEOMETRYCOLLECTION)
#
# @return data.frame
sfToDf <- function (x)
{
  if (is.na(sf::st_crs(x))) {
    stop(paste0("L'entr","\u00e9","e ne dispose pas d'un syst\u00e8me de r","\u00e9","f\u00e9rence de coordonn","\u00e9","es valide."))
  }
  
  if (methods::is(sf::st_geometry(x), c("sfc_GEOMETRY"))) {
    x <- sf::st_collection_extract(x, "POLYGON", warn = FALSE)
  }else if (methods::is(sf::st_geometry(x), c("sfc_GEOMETRYCOLLECTION"))) {
    x <- sf::st_collection_extract(x, "POLYGON", warn = FALSE)
  }else if (methods::is(sf::st_geometry(x), c("sfc_POLYGON"))) {
    sf::st_geometry(x) <- suppressWarnings(sf::st_centroid(x = sf::st_geometry(x),
                                          of_largest_polygon = T))
  }else if (methods::is(sf::st_geometry(x), c("sfc_MULTIPOLYGON"))) {
    sf::st_geometry(x) <- suppressWarnings(sf::st_centroid(x = sf::st_geometry(x),
                                                           of_largest_polygon = T))
  }else{}
  x <- sf::st_transform(x = x, crs = 4326)
  coords <- sf::st_coordinates(x)
  x <- data.frame(id = as.data.frame(x)[,1], lon = as.numeric(clean_coord(coords[,1])), lat = as.numeric(clean_coord(coords[, 2])), stringsAsFactors = FALSE)
  if (any(names(x)!=c("id","lon","lat"))) {
    stop(paste0("L'entr","\u00e9","e ne dispose pas d'identifiant."))
  }
  return(x)
}
