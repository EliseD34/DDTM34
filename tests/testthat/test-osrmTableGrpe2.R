### devtools::test()

### library(covr)
### covr <- file_coverage("R/osrmTableGrpe2.R", "tests/testthat/test-osrmTableGrpe2.R")
### report(covr)

library(testthat)

test_that('Calcul la duree et/ou la distance 1n ou n1 < 1000 et > 2 OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("ID","idSrc","lonSrc","latSrc","idDst","lonDst","latDst","duree","distance")

  exp3 <- data.frame(ID = c(1,2,3,4,5),
                     idSrc = c("Site Delpuech","Site Delpuech","Site Delpuech","Site Delpuech","Site Delpuech"),
                     lonSrc = c(5.392,5.392,5.392,5.392,5.392),
                     latSrc = c(43.28292,43.28292,43.28292,43.28292,43.28292),
                     idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Autre1","Autre2"),
                     lonDst = c(5.39242,5.37108,5.392,5.39242,5.39242),
                     latDst = c(43.28368,43.47902,43.28292,43.28368,43.28368),
                     duree = c(97,1732,0,97,97),
                     distance = c(912,28656,0,912,912),
                     stringsAsFactors = F)

  exp4 <- data.frame(ID = c(1,2,3,4,5),
                     idSrc = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Autre1","Autre2"),
                     lonSrc = c(5.39242,5.37108,5.392,5.39242,5.39242),
                     latSrc = c(43.28368,43.47902,43.28292,43.28368,43.28368),
                     idDst = c("Site Delpuech","Site Delpuech","Site Delpuech","Site Delpuech","Site Delpuech"),
                     lonDst = c(5.392,5.392,5.392,5.392,5.392),
                     latDst = c(43.28292,43.28292,43.28292,43.28292,43.28292),
                     duree = c(62,1774,0,62,62),
                     distance = c(567,27256,0,567,567),
                     stringsAsFactors = F)

  #donnees

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  couples1 <- data.frame(idSrc = c("Site Delpuech","Site Delpuech","Site Delpuech","Site Delpuech","Site Delpuech"),
                         lonSrc = c(5.39200,5.39200,5.39200,5.39200,5.39200),
                         latSrc = c(43.28292,43.28292,43.28292,43.28292,43.28292),
                         idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Autre1","Autre2"),
                         lonDst = c(5.39242,5.37107,5.39200,5.39242,5.39242),
                         latDst = c(43.28368,43.47900,43.28292,43.28368,43.28368),
                         ID = c(1,2,3,4,5),
                         stringsAsFactors = F)

  couples2 <- data.frame(idSrc = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Autre1","Autre2"),
                         lonDst = c(5.39242,5.37107,5.39200,5.39242,5.39242),
                         latDst = c(43.28368,43.47900,43.28292,43.28368,43.28368),
                         idDst = c("Site Delpuech","Site Delpuech","Site Delpuech","Site Delpuech","Site Delpuech"),
                         lonSrc = c(5.39200,5.39200,5.39200,5.39200,5.39200),
                         latSrc = c(43.28292,43.28292,43.28292,43.28292,43.28292),
                         ID = c(1,2,3,4,5),
                         stringsAsFactors = F)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(osrmTableGrpe2(couples = couples1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)), exp1)
  expect_equal(names(osrmTableGrpe2(couples = couples1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)), exp2)

  ###################################################################
  # TEST 2 : Cas nominal : duration, distance, exclude et faceAFace #
  ###################################################################

  res <- osrmTableGrpe2(couples = couples1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp3)

  res <- osrmTableGrpe2(couples = couples2, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp4)

  ##################################################################
  # TEST 3 : Exception : formats src, dst et exclude non conformes #
  ##################################################################

  expect_error(osrmTableGrpe2(couples = couples1[,-7], duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE))
  expect_error(osrmTableGrpe2(couples = couples1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = TRUE))
})
