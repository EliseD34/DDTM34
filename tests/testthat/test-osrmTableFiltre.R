### devtools::test()

### library(covr)
### covr <- file_coverage("R/osrmTableFiltre.R", "tests/testthat/test-osrmTableFiltre.R")
### report(covr)

library(testthat)

test_that('Calcul la duree et/ou la distance cartesien plus filtre OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("ID","idSrc","lonSrc","latSrc","idDst","lonDst","latDst","duree","distance")

  exp3 <- data.frame(ID = c(1:5),
                     idSrc = c("Site Delpuech","Site Delpuech","Site Menpenti","Site Menpenti","Site Aix-en-Provence"),
                     lonSrc = c(5.392,5.392,5.39242,5.39242,5.37108),
                     latSrc = c(43.28292,43.28292,43.28368,43.28368,43.47902),
                     idDst = c("Site Delpuech","Site Menpenti","Site Menpenti","Site Delpuech","Site Aix-en-Provence"),
                     lonDst = c(5.392,5.39242,5.39242,5.392,5.37108),
                     latDst = c(43.28292,43.28368,43.28368,43.28292,43.47902),
                     duree = round(c(0,97,0,62,0),0),
                     distance = round(c(0,912,0,567,0),0),
                     stringsAsFactors = F)

  exp4 <- data.frame(ID = c(1:3),
                     idSrc = c("Site Aix-en-Provence","Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.37108,5.392,5.39242),
                     latSrc = c(43.47902,43.28292,43.28368),
                     idDst = c("Site Aix-en-Provence","Site Delpuech","Site Menpenti"),
                     lonDst = c(5.37108,5.392,5.39242),
                     latDst = c(43.47902,43.28292,43.28368),
                     duree = round(c(0,0,0),0),
                     distance = round(c(0,0,0),0),
                     stringsAsFactors = F)

  exp5 <- data.frame(ID = c(1:2),
                     idSrc = c("Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.392,5.39242),
                     latSrc = c(43.28292,43.28368),
                     idDst = c("Autre","Autre"),
                     lonDst = c(5.37108,5.37108),
                     latDst = c(43.47902,43.47902),
                     duree = round(c(1732,1707),0),
                     distance = round(c(28656,28496),0),
                     stringsAsFactors = F)

  exp6 <- data.frame(ID = c(1:2),
                     idSrc = c("Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.392,5.39242),
                     latSrc = c(43.28292,43.28368),
                     idDst = c("Site Aix-en-Provence","Site Aix-en-Provence"),
                     lonDst = c(5.37108,5.37108),
                     latDst = c(43.47902,43.47902),
                     duree = round(c(1732,1707),0),
                     distance = round(c(28656,28496),0),
                     stringsAsFactors = F)

  #donnees

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  src1 <- data.frame(idSrc = c("Site Delpuech","Site Menpenti","Site Aix-en-Provence"),
                     lonSrc = c(5.39200,5.39242,5.37107),
                     latSrc = c(43.28292,43.28368,43.47900),
                     stringsAsFactors = F)

  dst1 <- data.frame(idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech"),
                     lonDst = c(5.39242,5.37107,5.39200),
                     latDst = c(43.28368,43.47900,43.28292),
                     stringsAsFactors = F)

  src2 <- data.frame(idSrc = c("Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.39200,5.39242),
                     latSrc = c(43.28292,43.28368),
                     stringsAsFactors = F)

  dst2 <- data.frame(idDst = c("Site Aix-en-Provence","Autre"),
                     lonDst = c(5.37107,5.37107),
                     latDst = c(43.47900,43.47900),
                     stringsAsFactors = F)

  src3 <- data.frame(idSrc = c("Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.39200,5.39242),
                     latSrc = c(43.28292,43.28368),
                     stringsAsFactors = F)

  dst3 <- data.frame(idDst = c("Site Aix-en-Provence","Site Aix-en-Provence"),
                     lonDst = c(5.37107,5.39242),
                     latDst = c(43.47900,5.39242),
                     stringsAsFactors = F)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(osrmTableFiltre(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 1, nbDstVolOiseau = 0, nbDstMeasure = 0, optiMeasure = "duree", code_epsg = 2154, interactive = FALSE)), exp1)
  expect_equal(names(osrmTableFiltre(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 1, nbDstVolOiseau = 0, nbDstMeasure = 0, optiMeasure = "duree", code_epsg = 2154, interactive = FALSE)), exp2)

  ###################################################################
  # TEST 2 : Cas nominal : duration, distance, exclude et faceAFace #
  ###################################################################

  res <- osrmTableFiltre(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 1, nbDstVolOiseau = 0, nbDstMeasure = 0, optiMeasure = "duree", code_epsg = 2154, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp3)

  res <- osrmTableFiltre(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 1, nbDstVolOiseau = 2, nbDstMeasure = 0, optiMeasure = "duree", code_epsg = 2154, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp3)

  res <- osrmTableFiltre(src = src2, dst = dst2, duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 1, nbDstVolOiseau = 2, nbDstMeasure = 1, optiMeasure = "duree", code_epsg = 2154, interactive = FALSE)
  expect_null(res)

  res <- osrmTableFiltre(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 1, nbDstVolOiseau = 2, nbDstMeasure = 1, optiMeasure = "duree", code_epsg = 2154, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp4)

  res <- osrmTableFiltre(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 1, nbDstVolOiseau = 2, nbDstMeasure = 1, optiMeasure = "distance", code_epsg = 2154, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp4)

  res <- osrmTableFiltre(src = src2, dst = dst2, duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 0, nbDstVolOiseau = 2, nbDstMeasure = 1, optiMeasure = "duree", code_epsg = 2154, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp5)

  res <- osrmTableFiltre(src = src3, dst = dst3, duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 0, nbDstVolOiseau = 2, nbDstMeasure = 1, optiMeasure = "duree", code_epsg = 2154, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp6)

  ##################################################################
  # TEST 3 : Exception : formats src, dst et exclude non conformes #
  ##################################################################

  expect_error(osrmTableFiltre(src = src1[,-1], dst = dst1[,-1], duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 1, nbDstVolOiseau = 0, nbDstMeasure = 0, optiMeasure = "duree", code_epsg = 2154, interactive = FALSE))
  expect_error(osrmTableFiltre(src = src1[,c(2,3,1)], dst = dst1[,c(2,3,1)], duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 1, nbDstVolOiseau = 0, nbDstMeasure = 0, optiMeasure = "duree", code_epsg = 2154, interactive = FALSE))
  expect_error(osrmTableFiltre(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 1, nbDstVolOiseau = 0, nbDstMeasure = 0, optiMeasure = "duree", code_epsg = 2154, interactive = TRUE))
  expect_error(osrmTableFiltre(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 0, nbDstVolOiseau = 0, nbDstMeasure = 0, optiMeasure = "duree", code_epsg = 2154, interactive = FALSE))
  expect_error(osrmTableFiltre(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, rayonMax = 1, nbDstVolOiseau = 0, nbDstMeasure = 1, optiMeasure = "autre", code_epsg = 2154, interactive = FALSE))
})
