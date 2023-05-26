### devtools::test()

### library(covr)
### covr <- file_coverage("R/indTableSrcDst.R", "tests/testthat/test-indTableSrcDst.R")
### report(covr)

library(testthat)

test_that('Calcul des indicateurs en volume par source (src) ou par destination (dst) OK', {

  st_un_multipoint = function(x) {
    oprj <- sf::st_crs(x)
    g <- sf::st_geometry(x)
    i <- rep(seq_len(nrow(x)), sapply(g, nrow))
    x <- x[i,]
    sf::st_geometry(x) <- sf::st_sfc(do.call(c, lapply(g, function(geom) lapply(1:nrow(geom), function(i) sf::st_point(geom[i,])))))
    x <- sf::st_sf(x, crs = oprj)
    x
  }

  # resultats attendus

  exp1 <- "list"
  exp2 <- c("sf","data.frame")
  exp3 <- c("idSrc","idDst","duree","geometry")
  exp4 <- c("idDst","nbSrc","geometry")

  ptsSrc <- data.frame(lng = c(5.39200,5.39242,5.37107,5.46476), lat = c(43.28292,43.28368,43.47900,43.31246), stringsAsFactors = FALSE)
  ptsSrc <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipoint(matrix(c(ptsSrc$lng,ptsSrc$lat),ncol=2)))), crs=4326)
  ptsSrc <- st_un_multipoint(ptsSrc)

  sfPtsDst <- data.frame(lng = c(5.47678,5.38385,5.38219), lat = c(43.29028,43.28571,43.44144), stringsAsFactors = FALSE)
  sfPtsDst <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipoint(matrix(c(sfPtsDst$lng,sfPtsDst$lat),ncol=2)))), crs=4326)
  sfPtsDst <- st_un_multipoint(sfPtsDst)

  dtDst <- data.frame(idDst = c("A","B","C"),
                      nbSrc = c(1,2,1),
                      stringsAsFactors = FALSE)
  sfPtsDst <- sf::st_sf(dtDst, geometry=sf::st_geometry(sfPtsDst))

  dtSrc <- data.frame(idSrc = c("1","2","3","4"),
                      idDst = c("B","B","A","C"),
                      duree = c(1.45,1.05,33.34,22.82),
                      stringsAsFactors = FALSE)
  sfPtsSrc <- sf::st_sf(dtSrc, geometry=sf::st_geometry(ptsSrc))

  exp5 <- list(sfPtsSrc, sfPtsDst)

  dtSrc <- data.frame(idSrc = c("1","2","3","4"),
                      idDst = c("B","B","A","C"),
                      duree = c(1.45,1.05,33.34,22.82),
                      distance = c(1.103,0.943,8.319,3.253),
                      stringsAsFactors = FALSE)
  sfPtsSrc <- sf::st_sf(dtSrc, geometry=sf::st_geometry(ptsSrc))

  exp6 <- list(sfPtsSrc, sfPtsDst)

  ptsSrc <- data.frame(lng = c(5.37107,5.39200,5.39242,5.46476), lat = c(43.47900,43.28292,43.28368,43.31246), stringsAsFactors = FALSE)
  ptsSrc <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipoint(matrix(c(ptsSrc$lng,ptsSrc$lat),ncol=2)))), crs=4326)
  ptsSrc <- st_un_multipoint(ptsSrc)

  dtSrc <- data.frame(idSrc = c("3","1","2","4"),
                      idDst = c("A","B","B","C"),
                      duree = c(33.34,1.45,1.05,22.82),
                      pop = c(1164,1204,806,976),
                      part_pop = c(100.0,59.9,40.1,100.0),
                      stringsAsFactors = FALSE)
  sfPtsSrc <- sf::st_sf(dtSrc, geometry=sf::st_geometry(ptsSrc))

  dtDst <- data.frame(idDst = c("A","B","C"),
                      nbSrc = c(1,2,1),
                      pop = c(1164,2010,976),
                      stringsAsFactors = FALSE)
  sfPtsDst <- sf::st_sf(dtDst, geometry=sf::st_geometry(sfPtsDst))

  exp7 <- list(sfPtsSrc, sfPtsDst)

  dtSrc <- data.frame(idSrc = c("3","1","2","4"),
                      idDst = c("A","B","B","C"),
                      duree = c(33.34,1.45,1.05,22.82),
                      distance = c(8.319,1.103,0.943,3.253),
                      pop = c(1164,1204,806,976),
                      part_pop = c(100.0,59.9,40.1,100.0),
                      stringsAsFactors = FALSE)
  sfPtsSrc <- sf::st_sf(dtSrc, geometry=sf::st_geometry(ptsSrc))

  exp8 <- list(sfPtsSrc, sfPtsDst)

  dtSrc <- data.frame(idSrc = c("3","1","2","4"),
                      idDst = c("A","B","B","C"),
                      duree = c(33.34,1.45,1.05,22.82),
                      pop = c(1164,1204,806,976),
                      part_pop = c(100.0,59.9,40.1,100.0),
                      hommes = c(580,600,406,490),
                      part_hommes = c(100.000,59.642,40.358,100.000),
                      femmes = c(584,604,400,486),
                      part_femmes = c(100.000,60.159,39.841,100.000),
                      stringsAsFactors = FALSE)
  sfPtsSrc <- sf::st_sf(dtSrc, geometry=sf::st_geometry(ptsSrc))

  dtDst <- data.frame(idDst = c("A","B","C"),
                      nbSrc = c(1,2,1),
                      pop = c(1164,2010,976),
                      hommes = c(580,1006,490),
                      femmes = c(584,1004,486),
                      stringsAsFactors = FALSE)
  sfPtsDst <- sf::st_sf(dtDst, geometry=sf::st_geometry(sfPtsDst))

  exp9 <- list(sfPtsSrc, sfPtsDst)

  dtSrc <- data.frame(idSrc = c("3","1","2","4"),
                      idDst = c("A","B","B","C"),
                      duree = c(33.34,1.45,1.05,22.82),
                      distance = c(8.319,1.103,0.943,3.253),
                      pop = c(1164,1204,806,976),
                      part_pop = c(100.0,59.9,40.1,100.0),
                      hommes = c(580,600,406,490),
                      part_hommes = c(100.000,59.642,40.358,100.000),
                      femmes = c(584,604,400,486),
                      part_femmes = c(100.000,60.159,39.841,100.000),
                      stringsAsFactors = FALSE)
  sfPtsSrc <- sf::st_sf(dtSrc, geometry=sf::st_geometry(ptsSrc))

  exp10 <- list(sfPtsSrc, sfPtsDst)

  ptsSrc <- data.frame(lng = c(5.39200,5.37107,5.46476), lat = c(43.28292,43.47900,43.31246), stringsAsFactors = FALSE)
  ptsSrc <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipoint(matrix(c(ptsSrc$lng,ptsSrc$lat),ncol=2)))), crs=4326)
  ptsSrc <- st_un_multipoint(ptsSrc)

  dtSrc <- data.frame(idSrc = c("1","3","4"),
                      idDst = c("B","A","C"),
                      duree = c(1.45,33.34,22.82),
                      stringsAsFactors = FALSE)
  sfPtsSrc <- sf::st_sf(dtSrc, geometry=sf::st_geometry(ptsSrc))

  sfPtsDst <- data.frame(lng = c(5.47678,5.38385,5.38219), lat = c(43.29028,43.28571,43.44144), stringsAsFactors = FALSE)
  sfPtsDst <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipoint(matrix(c(sfPtsDst$lng,sfPtsDst$lat),ncol=2)))), crs=4326)
  sfPtsDst <- st_un_multipoint(sfPtsDst)

  dtDst <- data.frame(idDst = c("A","B","C"),
                      nbSrc = c(1,1,1),
                      stringsAsFactors = FALSE)
  sfPtsDst <- sf::st_sf(dtDst, geometry=sf::st_geometry(sfPtsDst))

  exp11 <- list(sfPtsSrc, sfPtsDst)

  dtSrc <- data.frame(idSrc = c("1","3","4"),
                      idDst = c("B","A","C"),
                      distance = c(1.45,33.34,22.82),
                      stringsAsFactors = FALSE)
  sfPtsSrc <- sf::st_sf(dtSrc, geometry=sf::st_geometry(ptsSrc))

  exp12 <- list(sfPtsSrc, sfPtsDst)

  ptsSrc <- data.frame(lng = c(5.39200,5.46476), lat = c(43.28292,43.31246), stringsAsFactors = FALSE)
  ptsSrc <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipoint(matrix(c(ptsSrc$lng,ptsSrc$lat),ncol=2)))), crs=4326)
  ptsSrc <- st_un_multipoint(ptsSrc)

  dtSrc <- data.frame(idSrc = c("1","4"),
                      idDst = c("B","C"),
                      duree = c(1.45,22.82),
                      stringsAsFactors = FALSE)
  sfPtsSrc <- sf::st_sf(dtSrc, geometry=sf::st_geometry(ptsSrc))

  sfPtsDst <- data.frame(lng = c(5.38385,5.38219), lat = c(43.28571,43.44144), stringsAsFactors = FALSE)
  sfPtsDst <- sf::st_sf(geometry=sf::st_sfc(sf::st_geometry(sf::st_multipoint(matrix(c(sfPtsDst$lng,sfPtsDst$lat),ncol=2)))), crs=4326)
  sfPtsDst <- st_un_multipoint(sfPtsDst)

  dtDst <- data.frame(idDst = c("B","C"),
                      nbSrc = c(1,1),
                      stringsAsFactors = FALSE)
  sfPtsDst <- sf::st_sf(dtDst, geometry=sf::st_geometry(sfPtsDst))

  exp13 <- list(sfPtsSrc, sfPtsDst)

  dtSrc <- data.frame(idSrc = c("1","4"),
                      idDst = c("B","C"),
                      distance = c(1.45,22.82),
                      stringsAsFactors = FALSE)
  sfPtsSrc <- sf::st_sf(dtSrc, geometry=sf::st_geometry(ptsSrc))

  exp14 <- list(sfPtsSrc, sfPtsDst)

  #donnees

  res1 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.38385,5.47678,5.38219),
                     latDst = c(43.28571,43.28571,43.29028,43.44144),
                     duree = c(1.45,1.05,33.34,22.82),
                     stringsAsFactors = FALSE)

  res2 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.38385,5.47678,5.38219),
                     latDst = c(43.28571,43.28571,43.29028,43.44144),
                     duree = c(1.45,1.05,33.34,22.82),
                     distance = c(1.103,0.943,8.319,3.253),
                     stringsAsFactors = FALSE)

  res3 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.38385,5.47678,5.38219),
                     latDst = c(43.28571,43.28571,43.29028,43.44144),
                     duree = c(1.45,1.05,33.34,22.82),
                     pop = c(1204,806,1164,976),
                     stringsAsFactors = FALSE)

  res4 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.38385,5.47678,5.38219),
                     latDst = c(43.28571,43.28571,43.29028,43.44144),
                     duree = c(1.45,1.05,33.34,22.82),
                     distance = c(1.103,0.943,8.319,3.253),
                     pop = c(1204,806,1164,976),
                     stringsAsFactors = FALSE)

  res5 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.38385,5.47678,5.38219),
                     latDst = c(43.28571,43.28571,43.29028,43.44144),
                     duree = c(1.45,1.05,33.34,22.82),
                     pop = c(1204,806,1164,976),
                     hommes = c(600,406,580,490),
                     femmes = c(604,400,584,486),
                     stringsAsFactors = FALSE)

  res6 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.38385,5.47678,5.38219),
                     latDst = c(43.28571,43.28571,43.29028,43.44144),
                     duree = c(1.45,1.05,33.34,22.82),
                     distance = c(1.103,0.943,8.319,3.253),
                     pop = c(1204,806,1164,976),
                     hommes = c(600,406,580,490),
                     femmes = c(604,400,584,486),
                     stringsAsFactors = FALSE)

  res7 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.38385,5.47678,5.38219),
                     latDst = c(43.28571,43.28571,43.29028,43.44144),
                     duree = c(1.45,-999999.0,33.34,22.82),
                     stringsAsFactors = FALSE)

  res8 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.38385,5.47678,5.38219),
                     latDst = c(43.28571,43.28571,43.29028,43.44144),
                     distance = c(1.45,-999999.0,33.34,22.82),
                     stringsAsFactors = FALSE)

  res9 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.38385,5.47678,5.38219),
                     latDst = c(43.28571,43.28571,43.29028,43.44144),
                     duree = c(1.45,-999999.0,-999999.0,22.82),
                     stringsAsFactors = FALSE)


  res10 <- data.frame(ID = c(1:4),
                      idSrc = c("1","2","3","4"),
                      lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                      latSrc = c(43.28292,43.28368,43.47900,43.31246),
                      idDst = c("B","B","A","C"),
                      lonDst = c(5.38385,5.38385,5.47678,5.38219),
                      latDst = c(43.28571,43.28571,43.29028,43.44144),
                      distance = c(1.45,-999999.0,-999999.0,22.82),
                      stringsAsFactors = FALSE)

  res13 <- data.frame(ID = c(1:4),
                      idSrc = c("1","2","2","4"),
                      lonSrc = c(5.39200,5.39242,5.39242,5.46476),
                      latSrc = c(43.28292,43.28368,43.28368,43.31246),
                      idDst = c("B","B","A","C"),
                      lonDst = c(5.38385,5.38385,5.47678,5.38219),
                      latDst = c(43.28571,43.28571,43.29028,43.44144),
                      distance = c(1.45,0,33.34,22.82),
                      stringsAsFactors = FALSE)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(indTableSrcDst(res = res1)), exp1)
  expect_equal(class(indTableSrcDst(res = res1)[[1]]), exp2)
  expect_equal(class(indTableSrcDst(res = res1)[[2]]), exp2)

  expect_equal(names(indTableSrcDst(res = res1)[[1]]), exp3)
  expect_equal(names(indTableSrcDst(res = res1)[[2]]), exp4)

  #####################################################
  # TEST 2 : Cas nominal : duree, distance, variables #
  #####################################################

  expect_equal(indTableSrcDst(res = res1), exp5)
  expect_equal(indTableSrcDst(res = res1[,c(2,1,3:8)]), exp5)
  expect_equal(indTableSrcDst(res = res2), exp6)
  expect_equal(indTableSrcDst(res = res3), exp7)
  expect_equal(indTableSrcDst(res = res4), exp8)
  expect_equal(indTableSrcDst(res = res5), exp9)
  expect_equal(indTableSrcDst(res = res6), exp10)
  expect_equal(indTableSrcDst(res = res7), exp11)
  expect_equal(indTableSrcDst(res = res8), exp12)
  expect_equal(indTableSrcDst(res = res9), exp13)
  expect_equal(indTableSrcDst(res = res10), exp14)
  expect_null(indTableSrcDst(res = res13))

  #######################
  # TEST 3 : Exceptions #
  #######################

  expect_error(indTableSrcDst(res = res1[,-1]))
  expect_error(indTableSrcDst(res = res1[,-2]))
  expect_error(indTableSrcDst(res = res1[,-8]))
  names(res1)[1] <- "id"
  expect_error(indTableSrcDst(res = res1))
  names(res1)[1] <- "ID"
  names(res1)[8] <- "duration"
  expect_error(indTableSrcDst(res = res1))
  names(res2)[8] <- "duration"
  expect_error(indTableSrcDst(res = res2))
  res4[,"pop"] <- as.character(res4[,"pop"])
  expect_error(indTableSrcDst(res = res4))
})
