rgrid <-
function (loc, dmax, res)
{
  boxCoordX <- seq(from = sf::st_coordinates(loc)[1,1] - dmax,
                   to = sf::st_coordinates(loc)[1,1] + dmax,
                   length.out = res)
  boxCoordY <- seq(from = sf::st_coordinates(loc)[1,2] - dmax,
                   to = sf::st_coordinates(loc)[1,2] + dmax,
                   length.out = res)
  sgrid <- expand.grid(boxCoordX, boxCoordY)
  sgrid <- data.frame(ID = seq(1, nrow(sgrid), 1), COORDX = sgrid[,1], COORDY = sgrid[, 2])
  sgrid <- sf::st_as_sf(sgrid, coords = c("COORDX", "COORDY"), crs = sf::st_crs(loc), remove = FALSE)
  return(sgrid)
}
