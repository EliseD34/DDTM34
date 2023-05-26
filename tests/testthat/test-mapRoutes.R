### devtools::test()

### library(covr)
### covr <- file_coverage("R/mapRoutes.R", "tests/testthat/test-mapRoutes.R")
### report(covr)

library(testthat)

test_that('Visualiser le trace des routes sur une carte OK', {

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  # donnees

  src <- c("Site Menpenti","5.39242","43.28368")
  dst <- c("Site Delpuech","5.39200","43.28292")

  res <- list(metricOsrmRoute(src = src, dst = dst, overview = "simplified", returnclass = "sf"))
  res_sp <- list(metricOsrmRoute(src = src, dst = dst, overview = "simplified", returnclass = "sp"))

  res_dt1 <- metricOsrmRoute(src = src, dst = dst, overview = "simplified", returnclass = NULL)
  res_dt2 <- metricOsrmRoute(src = dst, dst = src, overview = "simplified", returnclass = NULL)

  arm13210 <- matrix(c(5.452244,43.25746,5.433829,43.26022,5.427531,43.26537,5.414402,43.26944,5.409344,43.27513,5.399826,43.27462,5.388737,43.28315,5.391944,43.28476,5.413891,43.29170,5.425802,43.29014,5.426052,43.28672,5.439820,43.28453,5.454034,43.27176,5.458094,43.26211,5.452244,43.25746),ncol=2, byrow=TRUE)
  arm13210 <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_polygon(list(arm13210))), crs=4326))

  road_dt <- mapRoutesProp(res = list(res_dt1[,c(ncol(res_dt1)-1,ncol(res_dt1))],res_dt2[,c(ncol(res_dt2)-1,ncol(res_dt2))]), fonds = list(arm13210), opaciteOSM = 0.5)

  # resultats attendus

  exp1 <- c("leaflet","htmlwidget")
  exp2 <- 8
  exp3 <- c("x","width","height","sizingPolicy","dependencies","elementId","preRenderHook","jsHooks")

  exp4 <- "L.CRS.EPSG3857"
  exp5 <- "addTiles"
  exp6 <- "addScaleBar"
  exp7 <- "addPolygons"
  exp8 <- "addPolylines"
  exp9 <- "http://{s}.tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png"
  exp10 <- data.frame(lng = sf::st_coordinates(arm13210)[,1],
                      lat = sf::st_coordinates(arm13210)[,2],
                      stringsAsFactors = FALSE)
  exp11 <- "#606060"
  exp12 <- data.frame(lng = c(5.39242,5.39196,5.39201,5.3922,5.39356,5.39366,5.39383,5.39379,5.3937,5.39349,5.39276,5.39253,5.3925,5.392),
                      lat = c(43.28368,43.28452,43.2846,43.28469,43.2848,43.28421,43.28351,43.2834,43.28331,43.28334,43.28313,43.28295,43.28288,43.28292),
                      stringsAsFactors = FALSE)
  exp13 <- "darkslateblue"
  exp14 <- c(43.25746,43.29170)
  exp15 <- c(5.388737,5.458094)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  road <- mapRoutes(res = res, fonds = list(arm13210), opaciteOSM = 0.5)

  expect_equal(class(road), exp1)
  expect_equal(length(road), exp2)
  expect_equal(names(road), exp3)

  ########################
  # TEST 2 : Cas nominal #
  ########################

  expect_equal(road$x$options$crs$crsClass, exp4)

  expect_equal(road$x$calls[[1]]$method, exp5)
  expect_equal(road$x$calls[[2]]$method, exp6)
  expect_equal(road$x$calls[[3]]$method, exp7)
  expect_equal(road$x$calls[[4]]$method, exp8)

  expect_equal(road$x$calls[[1]]$args[[1]], exp9)

  expect_equal(road$x$calls[[3]]$args[[1]][[1]][[1]][[1]], exp10)
  expect_equal(road$x$calls[[3]]$args[[4]]$color, exp11)

  expect_equal(road$x$calls[[4]]$args[[1]][[1]][[1]][[1]], exp12)
  expect_equal(road$x$calls[[4]]$args[[4]]$color, exp13)

  expect_equal(road$x$limits$lat, exp14)
  expect_equal(road$x$limits$lng, exp15)

  road_sp <- mapRoutes(res = res_sp, fonds = list(arm13210), opaciteOSM = 0.5)

  expect_equal(road, road_sp)

  road_weight <- mapRoutes(res = list(road_dt[[2]]), fonds = list(arm13210), opaciteOSM = 0.5)

  expect_equal(length(road_weight$x$calls[[4]]$args[[1]]), 2)
  expect_equal(road_weight$x$calls[[4]]$args[[4]]$weight, c(1,2))

  road <- mapRoutes(res = res, opaciteOSM = 0.5)
  road_sp <- mapRoutes(res = res_sp, opaciteOSM = 0.5)

  expect_equal(road, road_sp)

  ######################
  # TEST 3 : Exception #
  ######################

  expect_error(mapRoutes(res = data.frame(), fonds = list(arm13210), opaciteOSM = 0.5))
  expect_error(mapRoutes(res = as.data.frame(res), fonds = NULL, opaciteOSM = 0.5))
  expect_error(mapRoutes(res = list(res), fonds = NULL, opaciteOSM = 0.5))
  expect_error(mapRoutes(res = res, fonds = "fonds", opaciteOSM = 0.5))
  expect_error(mapRoutes(res = res, fonds = list("fonds"), opaciteOSM = 0.5))
})
