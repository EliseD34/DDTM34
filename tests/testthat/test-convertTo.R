### devtools::test()

### library(covr)
### covr <- file_coverage("R/convertTo.R", "tests/testthat/test-convertTo.R")
### report(covr)

library(testthat)

test_that('Convertir des coordonnees dans le systeme geographique de reference WGS84 (systeme GPS) OK', {

  local_edition(2)
    
  # resultats attendus

  exp1 <- "data.frame"
  exp2 <- "SpatialPointsDataFrame"
  exp3 <- "SpatialPolygonsDataFrame"
  exp4 <- "sf"

  exp5 <- data.frame(lon = c(5.44553,5.48212,5.16171,5.44553), lat = c(43.52772,43.33602,43.70377,43.52772), stringsAsFactors = FALSE)
  exp6 <- sp::SpatialPointsDataFrame(list(matrix(c(exp5[[1]],exp5[[2]]),ncol=2)), proj4string=sp::CRS("+init=epsg:4326"), data.frame(row.names = c(1:(length(exp5[[1]])))), match.ID = FALSE)

  pol <- sp::Polygons(list(sp::Polygon(cbind(c(5.44553,5.48212,5.16171,5.44553),c(43.52772,43.33602,43.70377,43.52772)))), "1")
  exp7 <- sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(list(pol), proj4string=sp::CRS("+init=epsg:4326")), data.frame(row.names = 1), match.ID = TRUE)

  st_un_multipoint = function(x) {
    oprj <- sf::st_crs(x)
    g <- sf::st_geometry(x)
    i <- rep(seq_len(nrow(x)), sapply(g, nrow))
    x <- x[i,]
    sf::st_geometry(x) <- sf::st_sfc(do.call(c, lapply(g, function(geom) lapply(1:nrow(geom), function(i) sf::st_point(geom[i,])))))
    x <- sf::st_sf(x, crs = oprj)
    x
  }
  points <- data.frame(lon = c(5.44553011427495,5.48211962326431,5.16170966171976,5.44553011427495), lat = c(43.5277225065116,43.3360163344221,43.7037708025951,43.5277225065116), stringsAsFactors = FALSE)
  exp8 <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipoint(matrix(c(points$lon,points$lat),ncol=2)))), crs=4326)
  exp8 <- st_un_multipoint(exp8)

  pol <- matrix(c(5.44553011427495,43.5277225065116,5.48211962326431,43.3360163344221,5.16170966171976,43.7037708025951,5.44553011427495,43.5277225065116),ncol=2, byrow=TRUE)
  exp9 <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon(list(pol))), crs=4326))

  pol1 <- matrix(c(5.44553011427495,43.5277225065116,5.48211962326431,43.3360163344221,5.16170966171976,43.7037708025951,5.44553011427495,43.5277225065116),ncol=2, byrow=TRUE)
  pol2 <- matrix(c(5.4099605565719,43.564524450755,5.5195960355932,43.065163861669,5.1258485006086,43.740483850748,5.4099605565719,43.564524450755),ncol=2, byrow=TRUE)
  exp10 <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipolygon(list(list(pol1),list(pol2)))), crs=4326))
  exp11 <- data.frame(lon = c(5.44553,5.48212,5.16171,5.44553,5.40996,5.51960,5.12585,5.40996), lat = c(43.52772,43.33602,43.70377,43.52772,43.56452,43.06516,43.74048,43.56452), stringsAsFactors = FALSE)

  exp12 <- sf::st_sf(id = 1, geometry=sf::st_sfc(sf::st_geometry(sf::st_multipolygon(list(list(pol1),list(pol2)))), crs=4326))
  exp13 <- data.frame(id = 1, lon = c(5.44553,5.48212,5.16171,5.44553,5.40996,5.51960,5.12585,5.40996), lat = c(43.52772,43.33602,43.70377,43.52772,43.56452,43.06516,43.74048,43.56452), stringsAsFactors = FALSE)

  exp14 <- data.frame(id = c(1:4), lon = c(5.44553,5.48212,5.16171,5.44553), lat = c(43.52772,43.33602,43.70377,43.52772), stringsAsFactors = FALSE)

  # donnees

  coord <- data.frame(X=c(897740.5,901367.8,874261.9,897740.5),Y=c(6272912,6251706,6291801,6272912), stringsAsFactors = FALSE)
  coordId <- data.frame(id=c(1:4),X=c(897740.5,901367.8,874261.9,897740.5),Y=c(6272912,6251706,6291801,6272912), stringsAsFactors = FALSE)
  coordWGS84 <- data.frame(lon=c(5.44553011427495,5.48211962326431,5.16170966171976,5.44553011427495),lat=c(43.5277225065116,43.3360163344221,43.7037708025951,43.5277225065116), stringsAsFactors = FALSE)

  suppressWarnings(objet_sp_points <- sp::SpatialPointsDataFrame(list(matrix(c(coord[[1]],coord[[2]]),ncol=2)), proj4string=sp::CRS("+init=epsg:2154"), data.frame(row.names = c(1:(length(coord[[1]])))), match.ID = FALSE))

  pol1 <- sp::Polygons(list(sp::Polygon(cbind(c(897740.5,901367.8,874261.9,897740.5),c(6272912,6251706,6291801,6272912)))), "1")
  suppressWarnings(objet_sp_polygon <- sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(list(pol1), proj4string=sp::CRS("+init=epsg:2154")), data.frame(row.names = 1), match.ID = FALSE))

  objet_sf_points <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_point(c(coord[[1]][1],coord[[2]][1]))), crs=2154))
  for(i in 2:length(coord[[1]]))
  {
    objet_sf_points <- rbind(objet_sf_points,sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_point(c(coord[[1]][i],coord[[2]][i]))), crs=2154)))
  }

  pol2 <- matrix(c(897740.5,6272912,901367.8,6251706,874261.9,6291801,897740.5,6272912),ncol=2, byrow=TRUE)
  objet_sf_polygon <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon(list(pol2))), crs=2154))

  pol3 <- matrix(c(894740.5,6276912,905367.8,6221706,871261.9,6295801,894740.5,6276912),ncol=2, byrow=TRUE)
  objet_sf1_multipolygon <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipolygon(list(list(pol2),list(pol3)))), crs=2154))
  objet_sf2_multipolygon <- sf::st_sf(id=c(1), geometry=sf::st_sfc(sf::st_geometry(sf::st_multipolygon(list(list(pol2),list(pol3)))), crs=2154))

  objet_sp_error <- sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(list(pol1)), data.frame(id=1), match.ID = FALSE)

  objet_sf_error <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_point(c(coord[[1]][1],coord[[2]][1])))))

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(convertTo(from = coord, to = NULL, fromEpsg = 2154, toEpsg = 4326)), exp1)
  expect_equal(class(convertTo(from = coord, to = "sp", fromEpsg = 2154))[1], exp2)
  expect_equal(class(convertTo(from = coord, to = "sf", fromEpsg = 2154))[1], exp4)
  expect_equal(class(convertTo(from = coordWGS84, to = NULL, fromEpsg = 4326, toEpsg = "2154")), exp1)

  expect_equal(class(convertTo(from = objet_sp_points, to = NULL, toEpsg = 4326))[1], exp2)
  expect_equal(class(convertTo(from = objet_sp_points, to = "data.frame")), exp1)
  expect_equal(class(convertTo(from = objet_sp_points, to = "sf"))[1], exp4)

  expect_equal(class(convertTo(from = objet_sp_polygon, to = NULL))[1], exp3)
  expect_equal(class(convertTo(from = objet_sp_polygon, to = "data.frame")), exp1)
  expect_equal(class(convertTo(from = objet_sp_polygon, to = "sf"))[1], exp4)

  expect_equal(class(convertTo(from = objet_sf_points, to = NULL))[1], exp4)
  expect_equal(class(convertTo(from = objet_sf_points, to = "data.frame")), exp1)
  expect_equal(class(convertTo(from = objet_sf_points, to = "sp"))[1], exp2)

  expect_equal(class(convertTo(from = objet_sf_polygon, to = NULL))[1], exp4)
  expect_equal(class(convertTo(from = objet_sf_polygon, to = "data.frame")), exp1)
  expect_equal(class(convertTo(from = objet_sf_polygon, to = "sp"))[1], exp3)

  expect_equal(class(convertTo(from = objet_sf1_multipolygon, to = NULL))[1], exp4)
  expect_equal(class(convertTo(from = objet_sf1_multipolygon, to = "data.frame")), exp1)
  expect_equal(class(convertTo(from = objet_sf1_multipolygon, to = "sp"))[1], exp3)

  expect_equal(class(convertTo(from = objet_sf2_multipolygon, to = NULL))[1], exp4)
  expect_equal(class(convertTo(from = objet_sf2_multipolygon, to = "data.frame")), exp1)
  expect_equal(class(convertTo(from = objet_sf2_multipolygon, to = "sp"))[1], exp3)

  #########################################################################################################
  # TEST 2 : Cas nominal : combinaisons entre les 3 input/ouput possibles avec sp et sf POINTS et POLYGON #
  #########################################################################################################

  expect_identical(convertTo(from = coord, to = NULL, fromEpsg = 2154), exp5)
  expect_equivalent(convertTo(from = coord, to = "sp", fromEpsg = 2154), exp6)
  expect_equal(convertTo(from = coord, to = "sf", fromEpsg = 2154), exp8)
  expect_identical(convertTo(from = coordId, to = NULL, fromEpsg = "2154", toEpsg = "4326"), exp14)
  expect_identical(convertTo(from = coordWGS84, to = "sf", fromEpsg = "4326"), exp8)

  expect_equivalent(convertTo(from = objet_sp_points, to = NULL, toEpsg = 4326), exp6)
  expect_identical(convertTo(from = objet_sp_points, to = "data.frame"), exp5)
  expect_equal(convertTo(from = objet_sp_points, to = "sf"), exp8)

  expect_equivalent(convertTo(from = objet_sp_polygon, to = NULL, toEpsg = 4326), exp7)
  expect_identical(convertTo(from = objet_sp_polygon, to = "data.frame"), exp5)
  expect_equal(convertTo(from = objet_sp_polygon, to = "sf"), exp9)

  expect_equal(convertTo(from = objet_sf_points, to = NULL, toEpsg = 4326), exp8)
  expect_identical(convertTo(from = objet_sf_points, to = "data.frame"), exp5)
  expect_equivalent(convertTo(from = objet_sf_points, to = "sp"), exp6)

  expect_equal(convertTo(from = objet_sf_polygon, to = NULL, toEpsg = 4326), exp9)
  expect_identical(convertTo(from = objet_sf_polygon, to = "data.frame"), exp5)
  expect_equivalent(convertTo(from = objet_sf_polygon, to = "sp"), exp7)

  expect_equal(convertTo(from = objet_sf1_multipolygon, to = NULL, toEpsg = 4326), exp10)
  expect_identical(convertTo(from = objet_sf1_multipolygon, to = "data.frame"), exp11)

  expect_equal(convertTo(from = objet_sf2_multipolygon, to = NULL, toEpsg = 4326), exp12)
  expect_identical(convertTo(from = objet_sf2_multipolygon, to = "data.frame"), exp13)

  ##########################################
  # TEST 3 : Exception : objet sf sans CRS #
  ##########################################

  expect_error(convertTo(from = "data.frame"))
  expect_error(convertTo(from = objet_sf_error))
  expect_error(convertTo(from = objet_sp_error))

  expect_error(convertTo(from = coord))
  expect_error(convertTo(from = coord, fromEpsg = 2154, to = "list"))
  expect_error(convertTo(from = coord, fromEpsg = 2154, to = "data.frame", toEpsg = NULL))
  expect_error(convertTo(from = data.frame(a=1, stringsAsFactors = FALSE), fromEpsg = 2154))
  expect_error(convertTo(from = data.frame(a="1",b="2", stringsAsFactors = FALSE), fromEpsg = 2154))
  expect_error(convertTo(from = data.frame(a="1",b="2",c="3",d="4", stringsAsFactors = FALSE), fromEpsg = 2154))
})
