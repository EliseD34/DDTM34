### devtools::test()

### library(covr)
### covr <- file_coverage("R/calculs_faceaface.R", "tests/testthat/test-calculs_faceaface.R")
### report(covr)

library(testthat)

test_that('Calcul la duree et/ou la distance face a face OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("ID","idSrc","lonSrc","latSrc","idDst","lonDst","latDst","duree","distance")

  exp3 <- data.frame(ID = c(1,2,3,4),
                     idSrc = c("Site Delpuech","Site Delpuech","Site Menpenti","Site Aix-en-Provence"),
                     lonSrc = c(5.39200, 5.39200, 5.39242, 5.37108),
                     latSrc = c(43.28292, 43.28292, 43.28368, 43.47902),
                     idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Autre"),
                     lonDst = c(5.39242, 5.37108, 5.39200, 5.39242),
                     latDst = c(43.28368, 43.47902, 43.28292, 43.28368),
                     duree = round(c(97,1732,62,1774),0),
                     distance = round(c(912,28656,567,27248),0),
                     stringsAsFactors = F)

  exp4 <- data.frame(ID = c(1,2,3,4),
                     idSrc = c("Site Delpuech","Autre","Site Menpenti","Site Aix-en-Provence"),
                     lonSrc = c(5.392,5.392,5.39242,5.37108),
                     latSrc = c(43.28292,43.28292,43.28368,43.47902),
                     idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Site Menpenti"),
                     lonDst = c(5.39242,5.37108,5.392,5.39242),
                     latDst = c(43.28368,43.47902,43.28292,43.28368),
                     duree = round(c(97,1732,62,1774),0),
                     distance = round(c(912,28656,567,27248),0),
                     stringsAsFactors = F)

  exp5 <- data.frame(ID = c(1,2),
                     idSrc = c("Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.392,5.39242),
                     latSrc = c(43.28292,43.28368),
                     idDst = c("Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37108),
                     latDst = c(43.28368,43.47902),
                     duree = round(c(97,1707),0),
                     distance = round(c(912,28496),0),
                     stringsAsFactors = F)

  exp6 <- data.frame(ID = c(1,2),
                     idSrc = c("Site Delpuech","Site Delpuech"),
                     lonSrc = c(5.392,5.392),
                     latSrc = c(43.28292,43.28292),
                     idDst = c("Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37108),
                     latDst = c(43.28368,43.47902),
                     duree = round(c(97,1732),0),
                     distance = round(c(912,28656),0),
                     stringsAsFactors = F)

  exp7 <- data.frame(ID = c(1,2,3,4,5),
                     idSrc = c("Site Delpuech","Site Delpuech","Site Delpuech","Site Delpuech","Site Delpuech"),
                     lonSrc = c(5.392,5.392,5.392,5.392,5.392),
                     latSrc = c(43.28292,43.28292,43.28292,43.28292,43.28292),
                     idDst = c("Site Menpenti","Site Aix-en-Provence","Autre","Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37108,5.392,5.39242,5.37108),
                     latDst = c(43.28368,43.47902,43.28292,43.28368,43.47902),
                     duree = round(c(97,1732,0,97,1732),0),
                     distance = round(c(912,28656,0,912,28656),0),
                     stringsAsFactors = F)

  #donnees

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  couples1 <- data.frame(idSrc = c("Site Delpuech","Site Delpuech","Site Menpenti","Site Aix-en-Provence"),
                         lonSrc = c(5.39200,5.39200,5.39242,5.37107),
                         latSrc = c(43.28292,43.28292,43.28368,43.47900),
                         idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Autre"),
                         lonDst = c(5.39242,5.37107,5.39200,5.39242),
                         latDst = c(43.28368,43.47900,43.28292,43.28368),
                         ID = c(1,2,3,4),
                         stringsAsFactors = F)

  couples2 <- data.frame(idSrc = c("Site Delpuech","Autre","Site Menpenti","Site Aix-en-Provence"),
                         lonSrc = c(5.39200,5.39200,5.39242,5.37107),
                         latSrc = c(43.28292,43.28292,43.28368,43.47900),
                         idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Site Menpenti"),
                         lonDst = c(5.39242,5.37107,5.39200,5.39242),
                         latDst = c(43.28368,43.47900,43.28292,43.28368),
                         ID = c(1,2,3,4),
                         stringsAsFactors = F)

  couples3 <- data.frame(idSrc = c("Site Delpuech","Site Menpenti"),
                         lonSrc = c(5.39200,5.39242),
                         latSrc = c(43.28292,43.28368),
                         idDst = c("Site Menpenti","Site Aix-en-Provence"),
                         lonDst = c(5.39242,5.37107),
                         latDst = c(43.28368,43.47900),
                         ID = c(1,2),
                         stringsAsFactors = F)

  couples4 <- data.frame(idSrc = c("Site Delpuech","Site Delpuech"),
                         lonSrc = c(5.39200,5.39200),
                         latSrc = c(43.28292,43.28292),
                         idDst = c("Site Menpenti","Site Aix-en-Provence"),
                         lonDst = c(5.39242,5.37107),
                         latDst = c(43.28368,43.47900),
                         ID = c(1,2),
                         stringsAsFactors = F)

  couples5 <- data.frame(idSrc = c("Site Delpuech","Site Delpuech","Site Delpuech","Site Delpuech","Site Delpuech"),
                         lonSrc = c(5.39200,5.39200,5.39200,5.39200,5.39200),
                         latSrc = c(43.28292,43.28292,43.28292,43.28292,43.28292),
                         idDst = c("Site Menpenti","Site Aix-en-Provence","Autre","Site Menpenti","Site Aix-en-Provence"),
                         lonDst = c(5.39242,5.37107,5.39200,5.39242,5.37107),
                         latDst = c(43.28368,43.47900,43.28292,43.28368,43.47900),
                         ID = c(1,2,3,4,5),
                         stringsAsFactors = F)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(calculs_faceaface(couplesUniques = couples1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)), exp1)
  expect_equal(names(calculs_faceaface(couplesUniques = couples1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)), exp2)

  ###################################################################
  # TEST 2 : Cas nominal : duration, distance, exclude et faceAFace #
  ###################################################################

  res <- calculs_faceaface(couplesUniques = couples1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp3)

  res <- calculs_faceaface(couplesUniques = couples2, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp4)

  res <- calculs_faceaface(couplesUniques = couples3, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp5)

  res <- calculs_faceaface(couplesUniques = couples4, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp6)

  res <- calculs_faceaface(couplesUniques = couples5, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp7)

  ##################################################################
  # TEST 3 : Exception : formats src, dst et exclude non conformes #
  ##################################################################

  expect_error(calculs_faceaface(couplesUniques = couples1[,-7], duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE))
  expect_error(calculs_faceaface(couplesUniques = couples1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = TRUE))
})
