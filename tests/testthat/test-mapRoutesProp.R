### devtools::test()

### library(covr)
### covr <- file_coverage("R/mapRoutesProp.R", "tests/testthat/test-mapRoutesProp.R")
### report(covr)

library(testthat)

test_that('Visualiser sur une carte les traces avec une epaisseur variable OK', {

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  # donnees

  src1 <- c("Site Menpenti","5.39242","43.28368")
  dst1 <- c("Site Delpuech","5.39200","43.28292")

  src2 <- c("Site Menpenti","5.39242","43.28368")
  dst2 <- c("Site Delpuech","5.39200","43.28292")

  res_sf1 <- metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", returnclass = "sf")
  res_dt1 <- metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", returnclass = NULL)
  res_sp1 <- metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", returnclass = "sp")

  res_sf2 <- metricOsrmRoute(src = dst2, dst = src2, overview = "simplified", returnclass = "sf")
  res_dt2 <- metricOsrmRoute(src = dst2, dst = src2, overview = "simplified", returnclass = NULL)
  res_sp2 <- metricOsrmRoute(src = dst2, dst = src2, overview = "simplified", returnclass = "sp")

  arm13210 <- matrix(c(5.452244,43.25746,5.433829,43.26022,5.427531,43.26537,5.414402,43.26944,5.409344,43.27513,5.399826,43.27462,5.388737,43.28315,5.391944,43.28476,5.413891,43.29170,5.425802,43.29014,5.426052,43.28672,5.439820,43.28453,5.454034,43.27176,5.458094,43.26211,5.452244,43.25746),ncol=2, byrow=TRUE)
  arm13210 <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon(list(arm13210))), crs=4326))

  # resultats attendus

  exp1 <- "list"
  exp2 <- c("leaflet","htmlwidget")
  exp3 <- 8
  exp4 <- c("x","width","height","sizingPolicy","dependencies","elementId","preRenderHook","jsHooks")

  exp5 <- c("sf","data.frame")
  exp6 <- c("weight","classes","col","geometry")
  exp6bis <- c("weight","classes","col","idTrace","geometry")

  exp7 <- "L.CRS.EPSG3857"
  exp8 <- "addTiles"
  exp9 <- "addScaleBar"
  exp10 <- "addPolygons"
  exp11 <- "addPolylines"

  exp12 <- c(43.25746,43.29170)
  exp13 <- c(5.388737,5.458094)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  road_sf <- mapRoutesProp(res = list(res_sf1), fonds = list(arm13210), opaciteOSM = 0.5)
  road_dt <- mapRoutesProp(res = list(res_dt1[,c(ncol(res_dt1)-1,ncol(res_dt1))]), fonds = list(arm13210), opaciteOSM = 0.5)

  expect_equal(class(road_sf), exp1)
  expect_equal(class(road_sf[[1]]), exp2)
  expect_equal(length(road_sf[[1]]), exp3)
  expect_equal(names(road_sf[[1]]), exp4)
  expect_equal(class(road_sf[[2]]), exp5)
  expect_equal(names(road_sf[[2]]), exp6)

  expect_equal(class(road_dt), exp1)
  expect_equal(class(road_dt[[1]]), exp2)
  expect_equal(length(road_dt[[1]]), exp3)
  expect_equal(names(road_dt[[1]]), exp4)
  expect_equal(class(road_dt[[2]]), exp5)
  expect_equal(names(road_dt[[2]]), exp6)

  ########################
  # TEST 2 : Cas nominal #
  ########################

  expect_equal(road_sf[[1]]$x$options$crs$crsClass, exp7)
  expect_equal(road_sf[[1]]$x$calls[[1]]$method, exp8)
  expect_equal(road_sf[[1]]$x$calls[[2]]$method, exp9)
  expect_equal(road_sf[[1]]$x$calls[[3]]$method, exp10)
  expect_equal(road_sf[[1]]$x$calls[[4]]$method, exp11)

  expect_equal(road_dt[[1]]$x$options$crs$crsClass, exp7)
  expect_equal(road_dt[[1]]$x$calls[[1]]$method, exp8)
  expect_equal(road_dt[[1]]$x$calls[[2]]$method, exp9)
  expect_equal(road_dt[[1]]$x$calls[[3]]$method, exp10)
  expect_equal(road_dt[[1]]$x$calls[[4]]$method, exp11)

  expect_equal(road_sf[[1]]$x$limits$lat, exp12)
  expect_equal(road_sf[[1]]$x$limits$lng, exp13)

  expect_equal(road_dt[[1]]$x$limits$lat, exp12)
  expect_equal(road_dt[[1]]$x$limits$lng, exp13)

  road_sf <- mapRoutesProp(res = list(res_sf1,res_sf2), fonds = list(arm13210), opaciteOSM = 0.5)
  road_dt <- mapRoutesProp(res = list(res_dt1[,c(ncol(res_dt1)-1,ncol(res_dt1))],res_dt2[,c(ncol(res_dt2)-1,ncol(res_dt2))]), fonds = list(arm13210), opaciteOSM = 0.5)
  road_sp <- mapRoutesProp(res = list(res_sp1,res_sp2), fonds = list(arm13210), opaciteOSM = 0.5)

  expect_equal(road_sf[[2]]$weight,c(1,2))
  expect_equal(road_sf[[2]]$classes,c(1,2))

  expect_equal(road_dt[[2]]$weight,c(1,2))
  expect_equal(road_dt[[2]]$classes,c(1,2))

  expect_equal(road_sp[[2]]$weight,c(1,2))
  expect_equal(road_sp[[2]]$classes,c(1,2))

  road_sf <- mapRoutesProp(res = list(res_sf1,res_sf2), fonds = list(arm13210), nbFlux = c(2,3), opaciteOSM = 0.5)
  road_dt <- mapRoutesProp(res = list(res_dt1[,c(ncol(res_dt1)-1,ncol(res_dt1))],res_dt2[,c(ncol(res_dt2)-1,ncol(res_dt2))]), fonds = list(arm13210), nbFlux = 3, opaciteOSM = 0.5)
  road_sp <- mapRoutesProp(res = list(res_sp1,res_sp2), fonds = list(arm13210), nbFlux = c(4,3,2,1), opaciteOSM = 0.5)

  road_sf <- mapRoutesProp(res = list(res_sf1,res_sf2), nbFlux = c(2,3), opaciteOSM = 0.5)
  road_dt <- mapRoutesProp(res = list(res_dt1[,c(ncol(res_dt1)-1,ncol(res_dt1))],res_dt2[,c(ncol(res_dt2)-1,ncol(res_dt2))]), nbFlux = 3, opaciteOSM = 0.5)
  road_sp <- mapRoutesProp(res = list(res_sp1,res_sp2), nbFlux = c(4,3,2,1), opaciteOSM = 0.5)

  ######################
  # TEST 3 : Exception #
  ######################

  expect_error(mapRoutesProp(res = list(), fonds = list(arm13210), opaciteOSM = 0.5))
  expect_error(expect_warning(mapRoutesProp(res = res_sf1, fonds = NULL, opaciteOSM = 0.5)))
  expect_error(mapRoutesProp(res = list(list(res_sf1)), fonds = NULL, opaciteOSM = 0.5))
  expect_error(mapRoutesProp(res = list(res_sf1)[[2]], fonds = NULL, opaciteOSM = 0.5))
  expect_error(expect_warning(mapRoutesProp(res = list(res_sf1)$geometry, fonds = NULL, opaciteOSM = 0.5)))
  expect_error(mapRoutesProp(res = list(res_sf1,res_sf2), fonds = "fonds", opaciteOSM = 0.5))
  expect_error(mapRoutesProp(res = list(res_sf1,res_sf2), fonds = list("fonds"), opaciteOSM = 0.5))
})
