### devtools::test()

### library(covr)
### covr <- file_coverage("R/convertToDf.R", "tests/testthat/test-convertToDf.R")
### report(covr)

library(testthat)

test_that('Transforme un vecteur, un objet sp ou sf en data.frame (id,lon,lat) OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("id","lon","lat")
  exp3 <- data.frame(id=1,lon=5.24979,lat=43.44794)
  exp4 <- data.frame(id=1,lon=5.36346,lat=43.52259)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  v <- c(1,5.24979,43.44794)

  objet_sf <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_point(x=c(5.24979,43.44794))), crs=4326))

  expect_equal(class(convertToDf(objet=v)), exp1)
  expect_equal(names(convertToDf(objet=v)), exp2)

  expect_equal(class(convertToDf(objet=objet_sf)), exp1)
  expect_equal(names(convertToDf(objet=objet_sf)), exp2)

  ##################################################################
  # TEST 2 : Cas nominal : vecteur, objet sp et sf POINT, crs 2154 #
  ##################################################################

  v <- c(1,5.24979,43.44794)

  pol <- sp::Polygons(list(sp::Polygon(cbind(c(897740.5,901367.8,874261.9,897740.5),c(6272912,6251706,6291801,6272912)))), "sp1")
  suppressWarnings(objet_sp <- sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(list(pol), proj4string=sp::CRS("+init=epsg:2154")), data.frame(id=1), match.ID = FALSE))

  pol <- matrix(c(897740.5,6272912,901367.8,6251706,874261.9,6291801,897740.5,6272912),ncol=2, byrow=TRUE)
  objet_sf <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon(list(pol))), crs=2154))

  expect_identical(convertToDf(objet=v), exp3)
  expect_identical(convertToDf(objet=objet_sp), exp4)
  expect_identical(convertToDf(objet=objet_sf), exp4)

  ##########################################
  # TEST 3 : Exception : objet sf sans CRS #
  ##########################################

  pol <- matrix(c(897740.5,6272912,901367.8,6251706,874261.9,6291801,897740.5,6272912),ncol=2, byrow=TRUE)
  objet_sf <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon(list(pol)))))

  expect_error(convertToDf(objet=objet_sf))
})
