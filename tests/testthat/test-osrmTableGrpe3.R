### devtools::test()

### library(covr)
### covr <- file_coverage("R/osrmTableGrpe3.R", "tests/testthat/test-osrmTableGrpe3.R")
### report(covr)

library(testthat)

test_that('Calcul la duree et/ou la distance en face a face OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("ID","idSrc","lonSrc","latSrc","idDst","lonDst","latDst","duree","distance")

  exp3 <- data.frame(ID = c(1,2,3,4),
                     idSrc = c("Site Delpuech","Site Delpuech","Site Menpenti","Site Aix-en-Provence"),
                     lonSrc = c(5.392,5.392,5.39242,5.37108),
                     latSrc = c(43.28292,43.28292,43.28368,43.47902),
                     idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Site Menpenti"),
                     lonDst = c(5.39242,5.37108,5.392,5.39242),
                     latDst = c(43.28368,43.47902,43.28292,43.28368),
                     duree = round(c(97,1732,62,1774),0),
                     distance = round(c(912,28656,567,27248),0),
                     stringsAsFactors = F)

  #donnees

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  couples1 <- data.frame(idSrc = c("Site Delpuech","Site Delpuech","Site Menpenti","Site Aix-en-Provence"),
                         lonSrc = c(5.39200,5.39200,5.39242,5.37107),
                         latSrc = c(43.28292,43.28292,43.28368,43.47900),
                         idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Site Menpenti"),
                         lonDst = c(5.39242,5.37107,5.39200,5.39242),
                         latDst = c(43.28368,43.47900,43.28292,43.28368),
                         ID = c(1,2,3,4),
                         stringsAsFactors = F)

  couples2 <- data.frame(ID = c(1,2,3,4),
                         idSrc = c("Site Delpuech","Site Delpuech","Site Menpenti","Site Aix-en-Provence"),
                         lonSrc = c(5.39200,5.39200,5.39242,5.37107),
                         latSrc = c(43.28292,43.28292,43.28368,43.47900),
                         idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Site Menpenti"),
                         lonDst = c(5.39242,5.37107,5.39200,5.39242),
                         latDst = c(43.28368,43.47900,43.28292,43.28368),
                         stringsAsFactors = F)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(osrmTableGrpe3(couples = couples1, duree = TRUE, distance = TRUE, exclude = NULL)), exp1)
  expect_equal(names(osrmTableGrpe3(couples = couples1, duree = TRUE, distance = TRUE, exclude = NULL)), exp2)

  ###################################################################
  # TEST 2 : Cas nominal : duration, distance, exclude et faceAFace #
  ###################################################################

  res <- osrmTableGrpe3(couples = couples1, duree = TRUE, distance = TRUE, exclude = NULL)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp3)

  res <- osrmTableGrpe3(couples = couples2, duree = TRUE, distance = TRUE, exclude = NULL)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp3)

  ##################################################################
  # TEST 3 : Exception : formats src, dst et exclude non conformes #
  ##################################################################

  expect_error(osrmTableGrpe3(couples = couples1[,-7], duree = TRUE, distance = TRUE, exclude = NULL))
  couples3 <- couples1
  names(couples3) <- c("idSrc","lonSrc","latSrc","idDst","lonDst","latDst","id")
  expect_error(osrmTableGrpe3(couples = couples3, duree = TRUE, distance = TRUE, exclude = NULL))
})
