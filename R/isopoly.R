isopoly <-
function (x, breaks, xcoords = "COORDX", ycoords = "COORDY", var = "OUTPUT")
{
  vmin <- min(x[[var]], na.rm = TRUE)
  vmax <- max(x[[var]], na.rm = TRUE)
  breaks <- sort(unique(c(vmin, breaks[breaks > vmin & breaks <
                                         vmax], vmax)))
  m <- matrix(data = x[[var]], nrow = length(unique(x[[xcoords]])),
              dimnames = list(unique(x[[xcoords]]), unique(x[[ycoords]])))
  lev_low = breaks[1:(length(breaks) - 1)]
  lev_high = breaks[2:length(breaks)]
  raw <- isoband::isobands(x = as.numeric(rownames(m)), y = as.numeric(colnames(m)),
                           z = t(m), levels_low = lev_low, levels_high = c(lev_high[-length(lev_high)],
                                                                           vmax + 1e-10))
  bands <- isoband::iso_to_sfg(raw)
  iso <- sf::st_sf(id = 1:length(bands), min = lev_low, max = lev_high,
               geometry = sf::st_sfc(bands), crs = sf::st_crs(x))
  iso[1, "min"] <- 0
  iso$center = iso$min + (iso$max - iso$min)/2
  sf::st_geometry(iso) <- sf::st_make_valid(sf::st_geometry(iso))
  if (methods::is(sf::st_geometry(iso), c("sfc_GEOMETRYCOLLECTION"))) {
    sf::st_geometry(iso) <- sf::st_collection_extract(sf::st_geometry(iso),
                                                  "POLYGON")
  }else if (methods::is(sf::st_geometry(iso), c("sfc_GEOMETRY"))) {
    sf::st_geometry(iso) <- sf::st_collection_extract(sf::st_geometry(iso),
                                                      "POLYGON")
  }
  iso <- iso[-nrow(iso), ]
  return(iso)
}
