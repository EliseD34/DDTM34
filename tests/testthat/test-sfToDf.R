### devtools::test()

### library(covr)
### covr <- file_coverage("R/sfToDf.R", "tests/testthat/test-sfToDf.R")
### report(covr)

library(testthat)

test_that('Transforme un objet sf en data.frame (id,lon,lat) OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("id","lon","lat")
  exp3 <- data.frame(id=1,lon=5.24979,lat=43.44794)
  exp4 <- data.frame(id=1,lon=5.36346,lat=43.52259)
  exp5 <- data.frame(id=rep(1,4),lon=c(5.44553,5.48212,5.16171,5.44553),lat=c(43.52772,43.33602,43.70377,43.52772))

  ###############################
  # TEST 1 : Test object return #
  ###############################

  objet_sf <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_point(x=c(5.24979,43.44794))), crs=4326))

  expect_equal(class(sfToDf(x=objet_sf)), exp1)
  expect_equal(names(sfToDf(x=objet_sf)), exp2)

  ###########################################################
  # TEST 2 : Cas nominal : objet sf POINT, crs 4326 et 2154 #
  ###########################################################

  objet_sf1 <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_point(x=c(5.24979,43.44794))), crs=4326))
  objet_sf2 <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_point(x=c(882170.5,6263577))), crs=2154))

  expect_identical(sfToDf(x=objet_sf1), exp3)
  expect_identical(sfToDf(x=objet_sf2), exp3)

  #################################################################
  # TEST 3 : Cas nominal : objet sf POLYGON et GEOMETRY, crs 2154 #
  #################################################################

  pts <- matrix(c(897740.5,6272912,901367.8,6251706,874261.9,6291801,897740.5,6272912),ncol=2, byrow=TRUE)
  objet_sf1 <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon(list(pts))), crs=2154))
  objet_sf2 <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_geometrycollection(list(sf::st_polygon(list(pts))))), crs=2154))

  expect_identical(sfToDf(x=objet_sf1), exp4)
  expect_identical(sfToDf(x=objet_sf2), exp5)

  ####################################################
  # TEST 4 : Exception: : pas de crs dans l'objet sf #
  ####################################################

  objet_sf <- sf::st_sf(id=1, geometry=sf::st_geometry(sf::st_point(x=c(5.24979,43.44794))))
  objet_df <- data.frame(x=1)

  expect_error(sfToDf(x=objet_sf))
  expect_error(sfToDf(x=objet_df))

  ##################################################
  # TEST 5 : Exception : pad de id dans l'objet sf #
  ##################################################

  objet_sf <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_point(x=c(5.24979,43.44794))), crs=4326))

  expect_error(sfToDf(x=objet_sf))
})
