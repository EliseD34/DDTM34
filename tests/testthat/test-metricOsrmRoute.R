### devtools::test()

### library(covr)
### covr <- file_coverage("R/metricOsrmRoute.R", "tests/testthat/test-metricOsrmRoute.R")
### report(covr)

library(testthat)

test_that('Calcul du temps de trajet, de la distance entre deux points ainsi que la geometrie du trace OK', {
  
  local_edition(2)
    
  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("src","dst","duree","distance","lon","lat")

  exp3 <- c("sf","data.frame")
  exp4 <- c("src","dst","duree","distance","geometry")

  exp5 <- "SpatialLinesDataFrame"
  exp6 <- c("src","dst","duree","distance")

  exp7 <- data.frame(src = rep("Site Delpuech",13),
                     dst = rep("Site Menpenti",13),
                     duree = rep(1.62,13),
                     distance = rep(0.911,13),
                     lon = c(5.392,5.39011,5.39066,5.39071,5.3922,5.39356,5.39366,5.39383,5.39379,5.3937,5.39349,5.39276,5.39242),
                     lat = c(43.28292,43.28305,43.28477,43.28484,43.28469,43.2848,43.28421,43.28351,43.2834,43.28331,43.28334,43.28313,43.28368),
                     stringsAsFactors = F)

  exp7bis <- data.frame(src = rep("src",13),
                        dst = rep("dst",13),
                        duree = rep(1.62,13),
                        distance = rep(0.911,13),
                        lon = c(5.392,5.39011,5.39066,5.39071,5.3922,5.39356,5.39366,5.39383,5.39379,5.3937,5.39349,5.39276,5.39242),
                        lat = c(43.28292,43.28305,43.28477,43.28484,43.28469,43.2848,43.28421,43.28351,43.2834,43.28331,43.28334,43.28313,43.28368),
                        stringsAsFactors = F)

  ligne <- matrix(c(5.39200,43.28292,
                    5.39011,43.28305,
                    5.39066,43.28477,
                    5.39071,43.28484,
                    5.39220,43.28469,
                    5.39356,43.28480,
                    5.39366,43.28421,
                    5.39383,43.28351,
                    5.39379,43.28340,
                    5.39370,43.28331,
                    5.39349,43.28334,
                    5.39276,43.28313,
                    5.39242,43.28368),ncol=2, byrow=TRUE)
  exp8 <- sf::st_sf(src = "Site Delpuech", dst = "Site Menpenti", duree = 1.62, distance = 0.911, geometry=sf::st_sfc(sf::st_geometry(sf::st_linestring(ligne)), crs=4326), stringsAsFactors = FALSE)
  row.names(exp8) <- "Site Delpuech_Site Menpenti"

  ligne <- sp::Lines(list(sp::Line(cbind(c(5.392,5.39011,5.39066,5.39071,5.3922,5.39356,5.39366,5.39383,5.39379,5.3937,5.39349,5.39276,5.39242),c(43.28292,43.28305,43.28477,43.28484,43.28469,43.2848,43.28421,43.28351,43.2834,43.28331,43.28334,43.28313,43.28368)))), "sp1")
  suppressWarnings(exp9 <- sp::SpatialLinesDataFrame(sp::SpatialLines(list(ligne), proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), data.frame(src = "Site Delpuech", dst = "Site Menpenti", duree = 1.56, distance = 0.913, stringsAsFactors = FALSE), match.ID = FALSE))
  row.names(exp9) <- "Site Delpuech_Site Menpenti"

  exp10 <- data.frame(src = rep("Site Delpuech",39),
                      dst = rep("Site Menpenti",39),
                      duree = rep(1.62,39),
                      distance = rep(0.911,39),
                      lon = c(5.392,5.39187,5.39109,5.39035,5.39011,5.39014,5.39018,5.39039,5.39049,5.39052,5.39063,5.39065,5.39066,5.39071,5.39084,
                              5.39152,5.39155,5.3919,5.39206,5.3922,5.39231,5.39237,5.39356,5.39357,5.39366,5.39374,5.39383,5.39379,5.3937,5.39363,
                              5.39358,5.39349,5.39342,5.39323,5.39284,5.39276,5.39271,5.39242,5.39242),
                      lat = c(43.28292,43.28292,43.28297,43.28303,43.28305,43.28313,43.28326,43.2839,43.28422,43.28431,43.28466,43.28476,43.28477,
                              43.28484,43.28483,43.28477,43.28477,43.28473,43.28471,43.28469,43.2847,43.2847,43.2848,43.28475,43.28421,43.28388,
                              43.28351,43.2834,43.28331,43.28333,43.28335,43.28334,43.28332,43.28327,43.28314,43.28313,43.28318,43.28368,43.28368),
                      stringsAsFactors = F)

  exp11 <- data.frame(src = rep("Site Delpuech",26),
                      dst = rep("Site Aix-en-Provence",26),
                      duree = rep(30.07,26),
                      distance = rep(29.413,26),
                      lon = c(5.392,5.39071,5.39356,5.39242,5.3855,5.38056,5.37509,5.37852,5.37693,5.36367,5.36085,5.35123,5.35134,5.35511,5.36321,
                              5.36748,5.39452,5.39658,5.40612,5.40808,5.40455,5.39969,5.39589,5.38269,5.37536,5.37108),
                      lat = c(43.28292,43.28484,43.2848,43.28368,43.28644,43.30203,43.30459,43.325,43.34054,43.35548,43.37427,43.38344,43.39425,
                              43.40127,43.40826,43.41963,43.43302,43.45184,43.46228,43.47177,43.47003,43.4725,43.47853,43.48423,43.478,43.47903),
                      stringsAsFactors = F)

  exp11bis <- data.frame(src = rep("src",26),
                         dst = rep("dst",26),
                         duree = rep(30.07,26),
                         distance = rep(29.413,26),
                         lon = c(5.392,5.39071,5.39356,5.39242,5.3855,5.38056,5.37509,5.37852,5.37693,5.36367,5.36085,5.35123,5.35134,5.35511,
                                 5.36321,5.36748,5.39452,5.39658,5.40612,5.40808,5.40455,5.39969,5.39589,5.38269,5.37536,5.37108),
                         lat = c(43.28292,43.28484,43.2848,43.28368,43.28644,43.30203,43.30459,43.325,43.34054,43.35548,43.37427,43.38344,43.39425,
                                 43.40127,43.40826,43.41963,43.43302,43.45184,43.46228,43.47177,43.47003,43.4725,43.47853,43.48423,43.478,43.47903),
                         stringsAsFactors = F)

  ligne <- matrix(c(5.39200,43.28292,
                    5.39071,43.28484,
                    5.39356,43.28480,
                    5.39242,43.28368,
                    5.38550,43.28644,
                    5.38056,43.30203,
                    5.37509,43.30459,
                    5.37852,43.32500,
                    5.37693,43.34054,
                    5.36367,43.35548,
                    5.36085,43.37427,
                    5.35123,43.38344,
                    5.35134,43.39425,
                    5.35511,43.40127,
                    5.36321,43.40826,
                    5.36748,43.41963,
                    5.39452,43.43302,
                    5.39658,43.45184,
                    5.40612,43.46228,
                    5.40808,43.47177,
                    5.40455,43.47003,
                    5.39969,43.47250,
                    5.39589,43.47853,
                    5.38269,43.48423,
                    5.37536,43.47800,
                    5.37108,43.47903),ncol=2, byrow=TRUE)
  exp12 <- sf::st_sf(src = "Site Delpuech", dst = "Site Aix-en-Provence", duree = 30.07, distance = 29.413, geometry = sf::st_sfc(sf::st_geometry(sf::st_linestring(ligne)), crs=4326), stringsAsFactors = FALSE)
  row.names(exp12) <- "Site Delpuech_Site Aix-en-Provence"

  ligne <- sp::Lines(list(sp::Line(cbind(c(5.39200,5.39071,5.39358,5.39242,5.38551,5.38056,5.37509,5.37852,5.37693,5.36367,5.36085,5.35123,5.35134,5.35511,5.36321,
                                           5.36748,5.37747,5.38962,5.39462,5.39658,5.40612,5.40817,5.40455,5.39959,5.39589,5.38269,5.37536,5.37107),
                                         c(43.28292,43.28484,43.28480,43.28368,43.28642,43.30203,43.30459,43.32500,43.34054,43.35548,43.37427,43.38344,43.39425,43.40127,
                                           43.40826,43.41963,43.42538,43.42910,43.43316,43.45184,43.46228,43.47183,43.47003,43.47262,43.47855,43.48423,43.47800,43.47900)))), "sp1")
  suppressWarnings(exp13 <- sp::SpatialLinesDataFrame(sp::SpatialLines(list(ligne), proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), data.frame(src = "Site Delpuech", dst = "Site Aix-en-Provence", duree = 29.96, distance = 29.432, stringsAsFactors = FALSE), match.ID = FALSE))
  row.names(exp13) <- "Site Delpuech_Site Aix-en-Provence"

  exp14 <- c(1.620,0.911)
  names(exp14) <- c("duree","distance")

  exp15 <- data.frame(src = rep("Site Delpuech",31),
                      dst = rep("Site Aix-en-Provence",31),
                      duree = rep(37.59,31),
                      distance = rep(29.164,31),
                      lon = c(5.392,5.39071,5.39356,5.39242,5.3855,5.38056,5.37321,5.36893,5.36624,5.36243,5.35504,5.35442,5.3496,5.34572,
                              5.3376,5.3382,5.34427,5.34966,5.35586,5.35909,5.36404,5.36579,5.36415,5.3706,5.36329,5.36032,5.3534,5.35617,5.36075,5.36551,5.37108),
                      lat = c(43.28292,43.28484,43.2848,43.28368,43.28644,43.30203,43.30496,43.31403,43.31327,43.32194,43.32767,43.33575,
                              43.33767,43.34646,43.35297,43.35985,43.36802,43.37264,43.37373,43.38666,43.39201,43.39613,43.40711,43.42773,
                              43.43555,43.44992,43.45896,43.48494,43.48633,43.48089,43.47903),
                      stringsAsFactors = F)

  #donnees

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  src1 <- c("Site Delpuech","5.39200","43.28292")
  dst1 <- c("Site Menpenti","5.39242","43.28368")

  src2 <- data.frame(idSrc = c("Site Delpuech"),
                     lonSrc = c(5.39200),
                     latSrc = c(43.28292),
                     stringsAsFactors = F)

  dst2 <- data.frame(idDst = c("Site Menpenti"),
                     lonDst = c(5.39242),
                     latDst = c(43.28368),
                     stringsAsFactors = F)

  src3 <- sf::st_sf(id="Site Delpuech", geometry=sf::st_sfc(sf::st_geometry(sf::st_point(x=c(5.39200,43.28292))), crs=4326), stringsAsFactors = FALSE)
  dst3 <- sf::st_sf(id="Site Menpenti", geometry=sf::st_sfc(sf::st_geometry(sf::st_point(x=c(5.39242,43.28368))), crs=4326), stringsAsFactors = FALSE)

  src4 <- sp::SpatialPointsDataFrame(list(matrix(c(5.39200,43.28292),ncol=2)), data.frame(id="Site Delpuech", stringsAsFactors = FALSE), proj4string=sp::CRS("+init=epsg:4326"))
  dst4 <- sp::SpatialPointsDataFrame(list(matrix(c(5.39242,43.28368),ncol=2)), data.frame(id="Site Menpenti", stringsAsFactors = FALSE), proj4string=sp::CRS("+init=epsg:4326"))

  loc1 <- data.frame(idSrc = c("Site Delpuech","Site Menpenti","Site Aix-en-Provence"),
                     lonSrc = c(5.39200,5.39242,5.37107),
                     latSrc = c(43.28292,43.28368,43.47900),
                     stringsAsFactors = F)

  loc2 <- sf::st_sf(id=c("Site Delpuech"), geometry=sf::st_sfc(sf::st_geometry(sf::st_point(x=c(5.39200,43.28292))), crs=4326), stringsAsFactors = FALSE)
  loc2 <- rbind(loc2,sf::st_sf(id=c("Site Menpenti"), geometry=sf::st_sfc(sf::st_geometry(sf::st_point(x=c(5.39242,43.28368))), crs=4326), stringsAsFactors = FALSE))
  loc2 <- rbind(loc2,sf::st_sf(id=c("Site Aix-en-Provence"), geometry=sf::st_sfc(sf::st_geometry(sf::st_point(x=c(5.37107,43.47900))), crs=4326), stringsAsFactors = FALSE))

  loc3 <- sp::SpatialPointsDataFrame(list(matrix(c(5.39200,43.28292,5.39242,43.28368,5.37107,43.47900),ncol=2, byrow=TRUE)), data.frame(id=c("Site Delpuech","Site Menpenti","Site Aix-en-Provence"), stringsAsFactors = FALSE), proj4string=sp::CRS("+init=epsg:4326"))

  loc4 <- data.frame(lonSrc = c(5.39200,5.39242,5.37107),
                     latSrc = c(43.28292,43.28368,43.47900),
                     stringsAsFactors = F)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", exclude = NULL, returnclass = NULL)), exp1)
  expect_equal(names(metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", exclude = NULL, returnclass = NULL)), exp2)

  expect_equal(class(metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", exclude = NULL, returnclass = "sf")), exp3)
  expect_equal(names(metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", exclude = NULL, returnclass = "sf")), exp4)

  expect_equal(class(metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", exclude = NULL, returnclass = "sp"))[1], exp5)
  expect_equal(names(metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", exclude = NULL, returnclass = "sp")), exp6)

  #####################################################
  # TEST 2 : Cas nominal : duree, distance et exclude #
  #####################################################

  expect_identical(metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", exclude = NULL, returnclass = NULL), exp7)
  expect_identical(metricOsrmRoute(src = src2, dst = dst2, overview = "simplified", exclude = NULL, returnclass = NULL), exp7)
  expect_identical(metricOsrmRoute(src = src3, dst = dst3, overview = "simplified", exclude = NULL, returnclass = NULL), exp7)
  expect_identical(metricOsrmRoute(src = src4, dst = dst4, overview = "simplified", exclude = NULL, returnclass = NULL), exp7)

  res <- metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", exclude = NULL, returnclass = "sf")
  expect_equal(res, exp8)

  res <- metricOsrmRoute(src = src2, dst = dst2, overview = "simplified", exclude = NULL, returnclass = "sf")
  expect_equal(res, exp8)

  res <- metricOsrmRoute(src = src3, dst = dst3, overview = "simplified", exclude = NULL, returnclass = "sf")
  expect_equal(res, exp8)

  res <- metricOsrmRoute(src = src4, dst = dst4, overview = "simplified", exclude = NULL, returnclass = "sf")
  expect_equal(res, exp8)

  res <- metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", exclude = NULL, returnclass = "sp")
  expect_equivalent(res, exp9)

  res <- metricOsrmRoute(src = src2, dst = dst2, overview = "simplified", exclude = NULL, returnclass = "sp")
  expect_equivalent(res, exp9)

  res <- metricOsrmRoute(src = src3, dst = dst3, overview = "simplified", exclude = NULL, returnclass = "sp")
  expect_equivalent(res, exp9)

  res <- metricOsrmRoute(src = src4, dst = dst4, overview = "simplified", exclude = NULL, returnclass = "sp")
  expect_equivalent(res, exp9)

  expect_identical(metricOsrmRoute(src = src1, dst = dst1, overview = "full", exclude = NULL, returnclass = NULL), exp10)

  expect_identical(metricOsrmRoute(loc = loc1, overview = "simplified", exclude = NULL, returnclass = NULL), exp11)
  expect_identical(metricOsrmRoute(loc = loc2, overview = "simplified", exclude = NULL, returnclass = NULL), exp11)
  expect_identical(metricOsrmRoute(loc = loc3, overview = "simplified", exclude = NULL, returnclass = NULL), exp11)

  res <- metricOsrmRoute(loc = loc1, overview = "simplified", exclude = NULL, returnclass = "sf")
  expect_equal(res, exp12)

  res <- metricOsrmRoute(loc = loc2, overview = "simplified", exclude = NULL, returnclass = "sf")
  expect_equal(res, exp12)

  res <- metricOsrmRoute(loc = loc3, overview = "simplified", exclude = NULL, returnclass = "sf")
  expect_equal(res, exp12)

  res <- metricOsrmRoute(loc = loc1, overview = "simplified", exclude = NULL, returnclass = "sp")
  expect_equivalent(res, exp13)

  res <- metricOsrmRoute(loc = loc2, overview = "simplified", exclude = NULL, returnclass = "sp")
  expect_equivalent(res, exp13)

  res <- metricOsrmRoute(loc = loc3, overview = "simplified", exclude = NULL, returnclass = "sp")
  expect_equivalent(res, exp13)

  #######################################
  # TEST 3 : Cas nominal : autres tests #
  #######################################

  expect_identical(metricOsrmRoute(src = src1[-1], dst = dst1[-1], overview = "simplified", exclude = NULL, returnclass = NULL), exp7bis)
  expect_identical(metricOsrmRoute(src = src2[,-1], dst = dst2[,-1], overview = "simplified", exclude = NULL, returnclass = NULL), exp7bis)
  expect_identical(metricOsrmRoute(src = src1, dst = dst1, overview = "simplified", exclude = "motorway", returnclass = NULL), exp7)
  expect_identical(metricOsrmRoute(src = src1, dst = dst1, overview = FALSE, exclude = NULL, returnclass = NULL), exp14)
  expect_identical(metricOsrmRoute(loc = loc1, overview = "simplified", exclude = "motorway", returnclass = NULL), exp15)
  expect_identical(metricOsrmRoute(loc = loc4, overview = "simplified", exclude = NULL, returnclass = NULL), exp11bis)
})
