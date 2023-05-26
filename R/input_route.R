input_route <-
function (x, id, single = TRUE)
{
  if (single) {
    if (is.vector(x)) {
      if (length(x) == 2) {
        id <- id
        i <- 0
      }
      else {
        i <- 1
        id <- x[i]
      }
      lon <- clean_coord(x[i + 1])
      lat <- clean_coord(x[i + 2])
    }
    if (methods::is(x, "Spatial")) {
      x <- sf::st_as_sf(x[1, ])
    }
    if (is.data.frame(x)) {
      if (methods::is(x, "sf")) {
        x <- sfToDf(x)
        i <- 1
        id <- x[1, i]
      }
      else {
        if (length(x) == 2) {
          i <- 0
          id <- id
        }
        else {
          i <- 1
          id <- x[1, i]
        }
      }
      lon <- clean_coord(x[1, i + 1])
      lat <- clean_coord(x[1, i + 2])
    }
    return(list(id = id, lon = lon, lat = lat))
  }
  else {
    if (methods::is(x, "Spatial")) {
      x <- sf::st_as_sf(x)
    }
    if (is.data.frame(x)) {
      if (methods::is(x, "sf")) {
        x <- sfToDf(x)
        i <- 1
        id1 <- x[1, 1]
        id2 <- x[nrow(x), 1]
      }
      else {
        if (length(x) == 2) {
          i <- 0
          id1 <- "src"
          id2 <- "dst"
        }
        else {
          i <- 1
          id1 <- x[1, 1]
          id2 <- x[nrow(x), 1]
        }
      }
      lon <- clean_coord(x[, i + 1])
      lat <- clean_coord(x[, i + 2])
    }
    return(list(id1 = id1, id2 = id2, lon = lon, lat = lat))
  }
}
