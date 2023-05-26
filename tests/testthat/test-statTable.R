### devtools::test()

### library(covr)
### covr <- file_coverage("R/statTable.R", "tests/testthat/test-statTable.R")
### report(covr)

library(testthat)

test_that('Renvoie des stats sur les resultats des calculs OSRM OK', {

  # resultats attendus

  exp1 <- "list"
  exp2 <- 11
  exp3 <- c("nbCouples","nbIdDep","nbIdArr","tempsMax","tempsMin","tempsMoyenne","tempsMediane","distanceMax","distanceMin","distanceMoyenne","distanceMediane")

  exp4 <- list(nbCouples = as.integer(4),
               nbIdDep = as.integer(4),
               nbIdArr = as.integer(3),
               tempsMax = 12.11,
               tempsMin = 1.05,
               tempsMoyenne = 4.66,
               tempsMediane = 2.73,
               distanceMax = 8.319,
               distanceMin = 0.943,
               distanceMoyenne = 3.405,
               distanceMediane = 2.178)

  exp5 <- list(nbCouples = as.integer(4),
               nbStables = as.integer(1),
               nbIdDep = as.integer(4),
               nbIdArr = as.integer(3),
               tempsMax = 12.11,
               tempsMin = 0,
               tempsMoyenne = 4.39,
               tempsMediane = 2.73,
               distanceMax = 8.319,
               distanceMin = 0,
               distanceMoyenne = 3.169,
               distanceMediane = 2.178)

  exp6 <- list(nbCouples = as.integer(4),
               nbStables = as.integer(2),
               nbIdDep = as.integer(4),
               nbIdArr = as.integer(4),
               tempsMax = 12.11,
               tempsMin = 0,
               tempsMoyenne = 3.39,
               tempsMediane = 0.73,
               distanceMax = 8.319,
               distanceMin = 0,
               distanceMoyenne = 2.356,
               distanceMediane = 0.551)

  exp7 <- list(nbCouples = as.integer(1),
               nbIdDep = as.integer(1),
               nbIdArr = as.integer(1),
               tempsMax = 28.81,
               tempsMin = 28.81,
               tempsMoyenne = 28.81,
               tempsMediane = 28.81,
               distanceMax = 28.675,
               distanceMin = 28.675,
               distanceMoyenne = 28.675,
               distanceMediane = 28.675)

  exp8 <- list(nbCouples = as.integer(1),
               nbIdDep = as.integer(1),
               nbIdArr = as.integer(1),
               tempsMax = 28.81,
               tempsMin = 28.81,
               tempsMoyenne = 28.81,
               tempsMediane = 28.81,
               distanceMax = 28.675,
               distanceMin = 28.675,
               distanceMoyenne = 28.675,
               distanceMediane = 28.675)

  exp9 <- list(nbCouples = as.integer(4),
               nbIdDep = as.integer(4),
               nbIdArr = as.integer(3),
               distanceMax = 8.319,
               distanceMin = 0.943,
               distanceMoyenne = 3.405,
               distanceMediane = 2.178)

  exp10 <- list(nbCouples = as.integer(4),
               nbStables = as.integer(1),
               nbIdDep = as.integer(4),
               nbIdArr = as.integer(3),
               distanceMax = 8.319,
               distanceMin = 0,
               distanceMoyenne = 3.169,
               distanceMediane = 2.178)

  exp11 <- list(nbCouples = as.integer(4),
               nbStables = as.integer(2),
               nbIdDep = as.integer(4),
               nbIdArr = as.integer(4),
               distanceMax = 8.319,
               distanceMin = 0,
               distanceMoyenne = 2.356,
               distanceMediane = 0.551)

  exp12 <- list(nbCouples = as.integer(1),
               nbIdDep = as.integer(1),
               nbIdArr = as.integer(1),
               distanceMax = 28.675,
               distanceMin = 28.675,
               distanceMoyenne = 28.675,
               distanceMediane = 28.675)

  exp13 <- list(nbCouples = as.integer(1),
               nbIdDep = as.integer(1),
               nbIdArr = as.integer(1),
               distanceMax = 28.675,
               distanceMin = 28.675,
               distanceMoyenne = 28.675,
               distanceMediane = 28.675)

  exp14 <- list(nbCouples = as.integer(1),
               nbIdDep = as.integer(1),
               nbIdArr = as.integer(1),
               tempsMax = 28.81,
               tempsMin = 28.81,
               tempsMoyenne = 28.81,
               tempsMediane = 28.81)

  exp15 <- list(nbCouples = as.integer(1),
                nbIdDep = as.integer(1),
                nbIdArr = as.integer(1),
                tempsMax = 28.81,
                tempsMin = 28.81,
                tempsMoyenne = 28.81,
                tempsMediane = 28.81)

  exp16 <- list(nbCouples = as.integer(2),
                nbStables = as.integer(1),
               nbIdDep = as.integer(2),
               nbIdArr = as.integer(2),
               tempsMax = 28.81,
               tempsMin = 0,
               tempsMoyenne = 14.4,
               tempsMediane = 14.4)

  exp17 <- list(nbCouples = as.integer(3),
                nbStables = as.integer(2),
                nbIdDep = as.integer(2),
                nbIdArr = as.integer(3),
                tempsMax = 28.81,
                tempsMin = 0,
                tempsMoyenne = 9.6,
                tempsMediane = 0)

  exp18 <- list(nbCouples = as.integer(0),
                nbIdDep = as.integer(0),
                nbIdArr = as.integer(0),
                tempsMax = "--",
                tempsMin = "--",
                tempsMoyenne = "--",
                tempsMediane = "--",
                distanceMax = "--",
                distanceMin = "--",
                distanceMoyenne = "--",
                distanceMediane = "--")

  # donnees

  res1 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.38385,5.47678,5.38219),
                     latDst = c(43.28571,43.28571,43.29028,43.44144),
                     duree = c(1.46,1.05,12.11,4),
                     distance = c(1.103,0.943,8.319,3.253),
                     stringsAsFactors = FALSE)

  res2 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.39242,5.47678,5.38219),
                     latDst = c(43.28571,43.28368,43.29028,43.44144),
                     duree = c(1.46,0,12.11,4),
                     distance = c(1.103,0,8.319,3.253),
                     stringsAsFactors = FALSE)

  res3 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","2","A","4"),
                     lonDst = c(5.38385,5.39242,5.47678,5.46476),
                     latDst = c(43.28571,43.28368,43.29028,43.31246),
                     duree = c(1.46,0,12.11,0),
                     distance = c(1.103,0,8.319,0),
                     stringsAsFactors = FALSE)

  res4 <- data.frame(ID = c(1:4),
                     idSrc = c("C","A","C","A"),
                     lonSrc = c(-61.31882,5.39200,-61.31882,5.39200),
                     latSrc = c(15.95387,43.28292,15.95387,43.28292),
                     idDst = c("D","D","B","B"),
                     lonDst = c(-61.76451,-61.76451,5.37107,5.37107),
                     latDst = c(16.06016,16.06016,43.47900,43.47900),
                     duree = c(-999999.00,-999999.00,-999999.00,28.81),
                     distance = c(-999999.000,-999999.000,-999999.000,28.675),
                     stringsAsFactors = FALSE)

  res5 <- data.frame(ID = c(1:2),
                     idSrc = c("C","A"),
                     lonSrc = c(-61.31882,5.39200),
                     latSrc = c(15.95387,43.28292),
                     idDst = c("B","B"),
                     lonDst = c(5.37107,5.37107),
                     latDst = c(43.47900,43.47900),
                     duree = c(-999999.00,28.81),
                     distance = c(-999999.000,28.675),
                     stringsAsFactors = FALSE)

  res6 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.38385,5.47678,5.38219),
                     latDst = c(43.28571,43.28571,43.29028,43.44144),
                     distance = c(1.103,0.943,8.319,3.253),
                     stringsAsFactors = FALSE)

  res7 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","B","A","C"),
                     lonDst = c(5.38385,5.39242,5.47678,5.38219),
                     latDst = c(43.28571,43.28368,43.29028,43.44144),
                     distance = c(1.103,0,8.319,3.253),
                     stringsAsFactors = FALSE)

  res8 <- data.frame(ID = c(1:4),
                     idSrc = c("1","2","3","4"),
                     lonSrc = c(5.39200,5.39242,5.37107,5.46476),
                     latSrc = c(43.28292,43.28368,43.47900,43.31246),
                     idDst = c("B","2","A","4"),
                     lonDst = c(5.38385,5.39242,5.47678,5.46476),
                     latDst = c(43.28571,43.28368,43.29028,43.31246),
                     distance = c(1.103,0,8.319,0),
                     stringsAsFactors = FALSE)

  res9 <- data.frame(ID = c(1:4),
                     idSrc = c("C","A","C","A"),
                     lonSrc = c(-61.31882,5.39200,-61.31882,5.39200),
                     latSrc = c(15.95387,43.28292,15.95387,43.28292),
                     idDst = c("D","D","B","B"),
                     lonDst = c(-61.76451,-61.76451,5.37107,5.37107),
                     latDst = c(16.06016,16.06016,43.47900,43.47900),
                     distance = c(-999999.000,-999999.000,-999999.000,28.675),
                     stringsAsFactors = FALSE)

  res10 <- data.frame(ID = c(1:2),
                     idSrc = c("C","A"),
                     lonSrc = c(-61.31882,5.39200),
                     latSrc = c(15.95387,43.28292),
                     idDst = c("B","B"),
                     lonDst = c(5.37107,5.37107),
                     latDst = c(43.47900,43.47900),
                     distance = c(-999999.000,28.675),
                     stringsAsFactors = FALSE)

  res11 <- data.frame(ID = c(1:2),
                     idSrc = c("C","A"),
                     lonSrc = c(-61.31882,5.39200),
                     latSrc = c(15.95387,43.28292),
                     idDst = c("B","B"),
                     lonDst = c(5.37107,5.37107),
                     latDst = c(43.47900,43.47900),
                     duree = c(-999999.00,28.81),
                     stringsAsFactors = FALSE)

  res12 <- data.frame(ID = c(1:3),
                      idSrc = c("C","C","A"),
                      lonSrc = c(-61.31882,-61.31882,5.39200),
                      latSrc = c(15.95387,15.95387,43.28292),
                      idDst = c("B","B","B"),
                      lonDst = c(5.37107,5.37107,5.37107),
                      latDst = c(43.47900,43.47900,43.47900),
                      duree = c(-999999.00,-999999.00,28.81),
                      stringsAsFactors = FALSE)

  res13 <- data.frame(ID = c(1:2),
                      idSrc = c("C","A"),
                      lonSrc = c(-61.31882,5.39200),
                      latSrc = c(15.95387,43.28292),
                      idDst = c("C","B"),
                      lonDst = c(-61.31882,5.37107),
                      latDst = c(15.95387,43.47900),
                      duree = c(0,28.81),
                      stringsAsFactors = FALSE)

  res14 <- data.frame(ID = c(1:3),
                      idSrc = c("C","A","A"),
                      lonSrc = c(-61.31882,5.39200,5.39200),
                      latSrc = c(15.95387,43.28292,43.28292),
                      idDst = c("C","B","A"),
                      lonDst = c(-61.31882,5.37107,5.39200),
                      latDst = c(15.95387,43.47900,43.28292),
                      duree = c(0,28.81,0),
                      stringsAsFactors = FALSE)

  res15 <- data.frame(ID = c(1:2),
                      idSrc = c("C","C"),
                      lonSrc = c(-61.31882,-61.31882),
                      latSrc = c(15.95387,15.95387),
                      idDst = c("B","B"),
                      lonDst = c(5.37107,5.37107),
                      latDst = c(43.47900,43.47900),
                      duree = c(-999999.00,-999999.00),
                      distance = c(-999999.00,-999999.00),
                      stringsAsFactors = FALSE)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(statTable(res = res1)), exp1)
  expect_equal(length(statTable(res = res1)), exp2)
  expect_equal(names(statTable(res = res1)), exp3)

  ########################
  # TEST 2 : Cas nominal #
  ########################

  expect_identical(statTable(res = res1), exp4)
  expect_identical(statTable(res = res2), exp5)
  expect_identical(statTable(res = res3), exp6)
  expect_identical(statTable(res = res4), exp7)
  expect_identical(statTable(res = res5), exp8)
  expect_identical(statTable(res = res6), exp9)
  expect_identical(statTable(res = res7), exp10)
  expect_identical(statTable(res = res8), exp11)
  expect_identical(statTable(res = res9), exp12)
  expect_identical(statTable(res = res10), exp13)
  expect_identical(statTable(res = res11), exp14)
  expect_identical(statTable(res = res12), exp15)
  expect_identical(statTable(res = res13), exp16)
  expect_identical(statTable(res = res14), exp17)
  expect_identical(statTable(res = res15), exp18)

  ######################
  # TEST 3 : Exception #
  ######################

  expect_error(statTable(res = res1[,-1]))
  expect_error(statTable(res = res1[,-2]))
  expect_error(statTable(res = res1[,-c(8,9)]))
  names(res1)[1] <- "id"
  expect_error(statTable(res = res1))
  names(res1)[1] <- "ID"
  names(res1)[8] <- "duration"
  expect_error(statTable(res = res1))
})
