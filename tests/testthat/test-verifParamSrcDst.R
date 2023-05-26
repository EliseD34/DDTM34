### devtools::test()

### library(covr)
### covr <- file_coverage("R/verifParamSrcDst.R", "tests/testthat/test-verifParamSrcDst.R")
### report(covr)

library(testthat)
library(sp)
library(methods)

test_that('Verification des parametres src et dst OK', {

  #donnees

  src1 <- c(1,5.39200,43.28292)
  dst1 <- c(2,5.39242,43.28368)

  src2 <- data.frame(idSrc = c("Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.39200,5.39242),
                     latSrc = c(43.28292,43.28368),
                     stringsAsFactors = F)
  dst2 <- data.frame(idDst = c("Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37107),
                     latDst = c(43.28368,43.47900),
                     stringsAsFactors = F)

  pol <- sp::Polygons(list(sp::Polygon(cbind(c(897740.5,901367.8,874261.9,897740.5),c(6272912,6251706,6291801,6272912)))), "sp1")
  src3 <- suppressWarnings(sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(list(pol), proj4string=sp::CRS("+init=epsg:2154")), data.frame(id=1,lon=5.36346,lat=43.52259), match.ID = FALSE))
  points <- list(c(897740.5,901367.8,874261.9,897740.5),c(6272912,6251706,6291801,6272912))
  dst3 <- suppressWarnings(sp::SpatialPointsDataFrame(list(matrix(c(points[[1]],points[[2]]),ncol=2)), proj4string=sp::CRS("+init=epsg:2154"), data.frame(id=c(1:4),x=c(897740.5,901367.8,874261.9,897740.5),y=c(6272912,6251706,6291801,6272912)), match.ID = FALSE))

  pol <- matrix(c(897740.5,6272912,901367.8,6251706,874261.9,6291801,897740.5,6272912),ncol=2, byrow=TRUE)
  src4 <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon(list(pol))), crs=2154))
  dst4 <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_point(x=c(5.24979,43.44794))), crs=4326))

  ########################
  # TEST 1 : Cas nominal #
  ########################

  expect_silent(verifParamSrcDst(src = src1, dst = dst1))
  expect_silent(verifParamSrcDst(src = src2, dst = dst2))
  expect_silent(verifParamSrcDst(src = src3, dst = dst3))
  expect_silent(verifParamSrcDst(src = src4, dst = dst4))

  ######################
  # TEST 2 : Exception #
  ######################

  expect_error(verifParamSrcDst(src = src1[-1], dst = dst1[-1]))
  expect_error(verifParamSrcDst(src = c(1,NA,3), dst = c(1,2,NA)))
  expect_error(verifParamSrcDst(src = c("1","2","3"), dst = c("1","2","3")))

  expect_error(verifParamSrcDst(src = src2[,-1], dst = dst2))
  expect_error(verifParamSrcDst(src = src2, dst = dst2[,-1]))
  src2_err <- src2
  dst2_err <- dst2
  src2_err$lonSrc[1:2] <- NA
  src2_err$latSrc[1:2] <- NA
  dst2_err$lonDst[1:2] <- NA
  dst2_err$latDst[1:2] <- NA
  expect_error(verifParamSrcDst(src = src2_err, dst = dst2_err))
  src2_err <- src2
  dst2_err <- dst2
  src2_err$lonSrc[1:2] <- c("5.39200","5.39242")
  src2_err$latSrc[1:2] <- c("43.28292","43.28368")
  dst2_err$lonDst[1:2] <- c("5.39242","5.37107")
  dst2_err$latDst[1:2] <- c("43.28368","43.47900")
  expect_error(verifParamSrcDst(src = src2_err, dst = dst2_err))

  pol <- sp::Polygons(list(sp::Polygon(cbind(c(897740.5,901367.8,874261.9,897740.5),c(6272912,6251706,6291801,6272912)))), "sp1")
  src3_err <- suppressWarnings(sp::SpatialPolygons(list(pol), proj4string=CRS("+init=epsg:2154")))
  dst3_err <- src3_err
  expect_error(verifParamSrcDst(src = src3_err, dst = dst3_err))

  ligne <- matrix(c(897740.5,6272912,901367.8,6251706,874261.9,6291801),ncol=2, byrow=TRUE)
  src4_err <- sf::st_sf(id=1, geometry=sf::st_sfc(sf::st_geometry(sf::st_linestring(ligne)), crs=2154))
  dst4_err <- src4_err
  expect_error(verifParamSrcDst(src = src4_err, dst = dst4_err))
  pol <- matrix(c(897740.5,6272912,901367.8,6251706,874261.9,6291801,897740.5,6272912),ncol=2, byrow=TRUE)
  src4_err <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon(list(pol))), crs=2154))
  dst4_err <- src4_err
  expect_error(verifParamSrcDst(src = src4_err, dst = dst4_err))
})
