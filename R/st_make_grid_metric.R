st_make_grid_metric <- function (x,
                                 n = c(10, 10),
                                 offset = sf::st_bbox(x)[c("xmin", "ymin")],
                                 crs = sf::st_crs(x))
{
  bb <- sf::st_bbox(x)

  n <- rep(n, length.out = 2)
  nx <-n[1]
  ny <- n[2]

  xc <- seq(offset[1], bb[3], length.out = nx + 1)
  yc <- seq(offset[2], bb[4], length.out = ny + 1)

  ret <- vector("list", nx * ny)
  square <- function(x1, y1, x2, y2) sf::st_polygon(list(matrix(c(x1, x2, x2, x1, x1, y1, y1, y2, y2, y1), 5)))
  for (i in 1:nx) for (j in 1:ny) ret[[(j - 1) * nx +  i]] <- square(xc[i], yc[j], xc[i + 1], yc[j + 1])

  ret <- sf::st_sf(sf::st_sfc(ret, crs = crs))

  return(ret)
}
