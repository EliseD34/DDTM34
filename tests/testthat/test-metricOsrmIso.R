### devtools::test()

### library(covr)
### covr <- file_coverage("R/metricOsrmIso.R", "tests/testthat/test-metricOsrmIso.R")
### report(covr)

library(testthat)

test_that('Calcul les isochrones autour d un point ou plusieurs points OK', {
  local_edition(2)
  
  # resultats attendus
  exp0 <- "list"
  exp1 <- c("sf","data.frame")
  exp2 <- c("id","min","max","geometry","center")

  exp3 <- "SpatialPolygonsDataFrame"
  exp4 <- c("id","min","max","center")

## FONCTIONNE EN LOCAL
#  pol <- list(matrix(c(5.392000,43.286462,5.387134,43.28292,5.392000,43.279378,5.396866,43.28292,5.392000,43.286462),ncol=2, byrow=TRUE))
#  exp5 <- sf::st_sf(id=as.integer(1), min=0, max=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_multipolygon(list(pol))), crs=4326))
#  exp5$center <- 0.5
#  exp5 <- list(exp5)
#  exp5bis <- sf::st_sf(id=as.integer(1), min=0, max=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon(pol)), crs=4326))
#  exp5bis$center <- 0.5
#  exp5bis <- list(exp5bis)
  
## FONCTIONNE EN LIGNE
  pol <- matrix(c(5.392000,43.286462,
                  5.396866,43.28292,
                  5.392000,43.279378,
                  5.387134,43.28292,
                  5.392000,43.286462),ncol=2, byrow=TRUE)
  exp5 <- sf::st_sf(id=as.integer(1), min=0, max=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon(list(pol))), crs=4326))
  exp5$center <- 0.5
  exp5 <- list(exp5)
  
  exp6 <- methods::as(exp5[[1]], "Spatial")
  exp6@polygons[[1]]@Polygons[[1]]@area <- 0
  exp6@polygons[[1]]@area <- 0
  exp6 <- list(exp6)

## FONCTIONNE EN LOCAL
#  pol <- list(matrix(c(5.395065070782183,43.28615564032864,5.389354929217814,43.28680636994476,5.387393239446251,43.28537837904707,5.389354929217814,43.28395035462744,5.395065070782183,43.28460110783416,5.396132820830802,43.28537837904707,5.395065070782183,43.28615564032864),ncol=2, byrow=TRUE))
#  exp7 <- sf::st_sf(id=as.integer(1), min=0, max=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_multipolygon(list(pol))), crs=4326))
#  exp7$center <- 0.5
#  exp7 <- list(exp7)
#  exp7bis <- sf::st_sf(id=as.integer(1), min=0, max=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon((pol))), crs=4326))
#  exp7bis$center <- 0.5
#  exp7bis <- list(exp7bis)

## FONCTIONNE EN LIGNE
  pol <- matrix(c(5.395065070782183,43.28615564032864,
                  5.396132820830802,43.28537837904707,
                  5.395065070782183,43.28460110783416,
                  5.389354929217814,43.28395035462744,
                  5.387393239446251,43.28537837904707,
                  5.389354929217814,43.28680636994476,
                  5.395065070782183,43.28615564032864),ncol=2, byrow=TRUE)
  exp7 <- sf::st_sf(id=as.integer(1), min=0, max=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon(list(pol))), crs=4326))
  exp7$center <- 0.5
  exp7 <- list(exp7)

  exp8 <- methods::as(exp7[[1]], "Spatial")
  exp8@polygons[[1]]@Polygons[[1]]@area <- 0
  exp8@polygons[[1]]@area <- 0
  exp8 <- list(exp8)

  #donnees

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  loc1 <- c(1,5.39200,43.28292)

  loc2 <- c(5.39200,43.28292)

  loc3 <- data.frame(lonSrc = c(5.39200),
                     latSrc = c(43.28292),
                     stringsAsFactors = F)

  loc4 <- data.frame(idSrc = c("Site Delpuech"),
                     lonSrc = c(5.39200),
                     latSrc = c(43.28292),
                     stringsAsFactors = F)

  loc5 <- data.frame(idDst = c("Site Delpuech","Site Menpenti"),
                     lonDst = c(5.39200,5.39242),
                     latDst = c(43.28292,43.28368),
                     stringsAsFactors = F)

  points <- list(lng = c(5.39200,5.39242), lat = c(43.28292,43.28368))

  loc6 <- sf::st_sf(idSrc = "Site Delpuech", geometry=sf::st_sfc(sf::st_geometry(sf::st_point(c(points$lng[1],points$lat[1]))), crs=4326), stringsAsFactors = FALSE)
  loc6 <- rbind(loc6,sf::st_sf(idSrc = "Site Menpenti", geometry=sf::st_sfc(sf::st_geometry(sf::st_point(c(points$lng[2],points$lat[2]))), crs=4326), stringsAsFactors = FALSE))

  loc7 <- suppressWarnings(sp::SpatialPointsDataFrame(list(matrix(c(points[[1]],points[[2]]),ncol=2)), proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "), data.frame(idSrc = c("Site Delpuech","Site Menpenti")), match.ID = FALSE))

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(metricOsrmIso(loc = loc3, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = "isochrones")), exp0)

  expect_equal(class(metricOsrmIso(loc = loc3, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = "isochrones")[[1]]), exp1)
  expect_equal(names(metricOsrmIso(loc = loc3, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = "isochrones")[[1]]), exp2)

  expect_equal(class(metricOsrmIso(loc = loc3, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sp", fusion = TRUE, courbes = "isochrones")[[1]])[[1]], exp3)
  expect_equal(names(metricOsrmIso(loc = loc3, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sp", fusion = TRUE, courbes = "isochrones")[[1]]), exp4)

  #####################################################
  # TEST 2 : Cas nominal : duree, distance et exclude #
  #####################################################

  ### loc : vecteur numeric
  # sortie sf
  expect_equal(metricOsrmIso(loc = loc1, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = "isochrones"), exp5)
  expect_equal(metricOsrmIso(loc = loc1, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = FALSE, courbes = "isochrones"), exp5)
  expect_equal(metricOsrmIso(loc = loc2, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = "isochrones"), exp5)
  expect_equal(metricOsrmIso(loc = loc2, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = "distance"), exp5)
  expect_equal(metricOsrmIso(loc = loc1, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = FALSE, courbes = "isodistances"), exp5)
  expect_equal(metricOsrmIso(loc = loc2, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = "autre"), exp5)
  expect_equal(metricOsrmIso(loc = loc2, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = c("autre","isodistances")), exp5)
  # sortie sp
  res <- metricOsrmIso(loc = loc1, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sp", fusion = TRUE, courbes = "isochrones")
  res[[1]]@polygons[[1]]@Polygons[[1]]@area <- 0
  res[[1]]@polygons[[1]]@area <- 0
  expect_equal(res, exp6)
  res <- metricOsrmIso(loc = loc2, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sp", fusion = TRUE, courbes = "isochrones")
  res[[1]]@polygons[[1]]@Polygons[[1]]@area <- 0
  res[[1]]@polygons[[1]]@area <- 0
  expect_equal(res, exp6)
  res <- metricOsrmIso(loc = loc2, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sp", fusion = TRUE, courbes = "isodistances")
  res[[1]]@polygons[[1]]@Polygons[[1]]@area <- 0
  res[[1]]@polygons[[1]]@area <- 0
  expect_equal(res, exp6)

  ### loc : data.frame
  # sortie sf
  expect_equal(metricOsrmIso(loc = loc3, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = "isochrones"), exp5)
  expect_equal(metricOsrmIso(loc = loc3, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = FALSE, courbes = "isochrones"), exp5)
  expect_equal(metricOsrmIso(loc = loc4, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = "isochrones"), exp5)
  expect_equal(metricOsrmIso(loc = loc5, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 8, returnclass = "sf", fusion = TRUE, courbes = "isochrones"), exp7)

  # sortie sp
  res <- metricOsrmIso(loc = loc4, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sp", fusion = TRUE, courbes = "isochrones")
  res[[1]]@polygons[[1]]@Polygons[[1]]@area <- 0
  res[[1]]@polygons[[1]]@area <- 0
  expect_equal(res, exp6)
  res <- metricOsrmIso(loc = loc5, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 8, returnclass = "sp", fusion = TRUE, courbes = "isochrones")
  res[[1]]@polygons[[1]]@Polygons[[1]]@area <- 0
  res[[1]]@polygons[[1]]@area <- 0
  expect_equal(res, exp8)

  ### loc : sf
  # sortie sf
  expect_equal(metricOsrmIso(loc = loc6, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 8, returnclass = "sf", fusion = TRUE, courbes = "isochrones"), exp7)
  # sortie sp
  res <- metricOsrmIso(loc = loc6, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 8, returnclass = "sp", fusion = TRUE, courbes = "isochrones")
  res[[1]]@polygons[[1]]@Polygons[[1]]@area <- 0
  res[[1]]@polygons[[1]]@area <- 0
  expect_equal(res, exp8)

  ### loc : sp
  # sortie sf - expect_equivalent remplace par expect_equal(ignore_attr = TRUE) car deprecated mais ne fonctionne pas
  expect_equivalent(metricOsrmIso(loc = loc7, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 8, returnclass = "sf", fusion = TRUE, courbes = "isochrones"), exp7)
  # sortie sp
  res <- metricOsrmIso(loc = loc7, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 8, returnclass = "sp", fusion = TRUE, courbes = "isochrones")
  res[[1]]@polygons[[1]]@Polygons[[1]]@area <- 0
  res[[1]]@polygons[[1]]@area <- 0
  res[[1]]@proj4string <- exp8[[1]]@proj4string
  expect_equal(res, exp8)

  ######################
  # TEST 3 : Exception #
  ######################

  expect_error(metricOsrmIso(loc = loc1[1], breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = "isochrones"))
  expect_error(metricOsrmIso(loc = as.data.frame(loc5[,1]), breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = "isochrones"))
  expect_error(metricOsrmIso(loc = list(loc3), breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 5, returnclass = "sf", fusion = TRUE, courbes = "isochrones"))
  expect_error(metricOsrmIso(loc = loc1, breaks = seq(from = 0, to = 1, length.out = 2), exclude = NULL, res = 2, returnclass = "sf", fusion = TRUE, courbes = "isochrones"))
})
