### devtools::test()

### library(covr)
### covr <- file_coverage("R/metricOsrmTable.R", "tests/testthat/test-metricOsrmTable.R")
### report(covr)

library(testthat)

test_that('Calcul la duree et/ou la distance face a face ou produit cartesien OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("ID","idSrc","lonSrc","latSrc","idDst","lonDst","latDst","duree","distance")

  exp3_faf <- data.frame(ID = c(1:4),
                     idSrc = c("Site Aix-en-Provence","Site Delpuech","Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.37108,5.392,5.392,5.39242),
                     latSrc = c(43.47902,43.28292,43.28292,43.28368),
                     idDst = c("Autre","Site Menpenti","Site Aix-en-Provence","Site Delpuech"),
                     lonDst = c(5.39242,5.39242,5.37108,5.392),
                     latDst = c(43.28368,43.28368,43.47902,43.28292),
                     duree = c(29.57,1.62,28.87,1.03),
                     distance = c(27.248,0.912,28.656,0.567),
                     stringsAsFactors = F)

  exp3_pc <- data.frame(ID = c(1:8),
                     idSrc = c("Site Aix-en-Provence","Site Aix-en-Provence","Site Aix-en-Provence","Site Delpuech","Site Delpuech","Site Delpuech","Site Menpenti","Site Menpenti"),
                     lonSrc = c(5.37108,5.37108,5.37108,5.392,5.392,5.392,5.39242,5.39242),
                     latSrc = c(43.47902,43.47902,43.47902,43.28292,43.28292,43.28292,43.28368,43.28368),
                     idDst = c("Site Menpenti","Site Delpuech","Autre","Site Menpenti","Site Aix-en-Provence","Autre","Site Aix-en-Provence","Site Delpuech"),
                     lonDst = c(5.39242,5.392,5.39242,5.39242,5.37108,5.39242,5.37108,5.392),
                     latDst = c(43.28368,43.28292,43.28368,43.28368,43.47902,43.28368,43.47902,43.28292),
                     duree = c(29.57,29.57,29.57,1.62,28.87,1.62,28.45,1.03),
                     distance = c(27.248,27.256,27.248,0.912,28.656,0.912,28.496,0.567),
                     stringsAsFactors = F)

  exp4_faf <- data.frame(ID = c(1:2),
                     idSrc = c("Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.392,5.39242),
                     latSrc = c(43.28292,43.28368),
                     idDst = c("Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37108),
                     latDst = c(43.28368,43.47902),
                     duree = c(1.62,28.45),
                     distance = c(0.912,28.496),
                     stringsAsFactors = F)

  exp4_pc <- data.frame(ID = c(1:3),
                     idSrc = c("Site Delpuech","Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.392,5.392,5.39242),
                     latSrc = c(43.28292,43.28292,43.28368),
                     idDst = c("Site Menpenti","Site Aix-en-Provence","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37108,5.37108),
                     latDst = c(43.28368,43.47902,43.47902),
                     duree = c(1.62,28.87,28.45),
                     distance = c(0.912,28.656,28.496),
                     stringsAsFactors = F)

  exp4_faf2 <- exp4_faf[-c(1,2),]

  exp5_faf <- data.frame(ID = c(1:2),
                     idSrc = c("Site Delpuech","Site Delpuech"),
                     lonSrc = c(5.392,5.392),
                     latSrc = c(43.28292,43.28292),
                     idDst = c("Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37108),
                     latDst = c(43.28368,43.47902),
                     duree = c(1.62,28.87),
                     distance = c(0.912,28.656),
                     stringsAsFactors = F)

  exp5_pc <- data.frame(ID = c(1:2),
                     idSrc = c("Site Delpuech","Site Delpuech"),
                     lonSrc = c(5.392,5.392),
                     latSrc = c(43.28292,43.28292),
                     idDst = c("Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37108),
                     latDst = c(43.28368,43.47902),
                     duree = c(1.62,28.87),
                     distance = c(0.912,28.656),
                     stringsAsFactors = F)

  exp6 <- data.frame(ID = c(1:1),
                     idSrc = c("Site Delpuech"),
                     lonSrc = c(5.392),
                     latSrc = c(43.28292),
                     idDst = c("Site Menpenti"),
                     lonDst = c(5.39242),
                     latDst = c(43.28368),
                     duree = c(1.62),
                     distance = c(0.912),
                     stringsAsFactors = F)

  exp7 <- data.frame(ID = c(1:1),
                     idSrc = "Saint-Louis",
                     lonSrc = -61.31882,
                     latSrc = 15.95387,
                     idDst = "Vieux Habitants",
                     lonDst = -61.76451,
                     latDst = 16.06015,
                     duree = -999999,
                     distance = -999999,
                     stringsAsFactors = FALSE)

  exp8 <- data.frame(ID = c(1:4),
                     idSrc = c("A","A","Saint-Louis","Saint-Louis"),
                     lonSrc = c(5.392,5.392,-61.31882,-61.31882),
                     latSrc = c(43.28292,43.28292,15.95387,15.95387),
                     idDst = c("Vieux Habitants","B","Vieux Habitants","B"),
                     lonDst = c(-61.76451,5.37108,-61.76451,5.37108),
                     latDst = c(16.06015,43.47902,16.06015,43.47902),
                     duree = c(-999999,28.87,-999999,-999999),
                     distance = c(-999999,28.656,-999999,-999999),
                     stringsAsFactors = FALSE)

  exp9 <- data.frame(ID = c(1:1),
                     idSrc = "A",
                     lonSrc = 5.392,
                     latSrc = 43.28292,
                     idDst = "B",
                     lonDst = 5.39224,
                     latDst = 43.28401,
                     duree = 1.67,
                     distance = 0.952,
                     stringsAsFactors = FALSE)

  exp10 <- data.frame(ID = c(1:6),
                     idSrc = c("A","A","A","B","B","C"),
                     lonSrc = c(5.392,5.392,5.392,5.39224,5.39224,5.39242),
                     latSrc = c(43.28292,43.28292,43.28292,43.28401,43.28401,43.28368),
                     idDst = c("B","C","D","C","D","D"),
                     lonDst = c(5.39224,5.39242,5.37108,5.39242,5.37108,5.37108),
                     latDst = c(43.28401,43.28368,43.47902,43.28368,43.47902,43.47902),
                     duree = c(1.67,1.62,28.87,0.98,28.4,28.45),
                     distance = c(0.952,0.912,28.656,0.519,28.456,28.496),
                     stringsAsFactors = FALSE)

  exp11 <- data.frame(ID = c(1:8),
                      idSrc = c("A","A","A","B","B","B","C","C"),
                      lonSrc = c(5.39200,5.39200,5.39200,5.39224,5.39224,5.39224,5.39242,5.39242),
                      latSrc = c(43.28292,43.28292,43.28292,43.28401,43.28401,43.28401,43.28368,43.28368),
                      idDst = c("B","C","D","B","C","D","C","D"),
                      lonDst = c(5.39224,5.39242,5.37108,5.39224,5.39242,5.37108,5.39242,5.37108),
                      latDst = c(43.28401,43.28368,43.47902,43.28401,43.28368,43.47902,43.28368,43.47902),
                      duree = c(1.67,1.62,28.87,0.00,0.98,28.40,0.00,28.45),
                      distance = c(0.952,0.912,28.656,0.000,0.519,28.456,0.000,28.496),
                      stringsAsFactors = FALSE)

  exp12 <- data.frame(ID = c(1:8),
                      idSrc = c("A","A","A","B","B","B","C","C"),
                      lonSrc = c(5.39200,5.39200,5.39200,5.39224,5.39224,5.39224,5.39242,5.39242),
                      latSrc = c(43.28292,43.28292,43.28292,43.28401,43.28401,43.28401,43.28368,43.28368),
                      idDst = c("B","C","D","B","C","D","C","D"),
                      lonDst = c(5.39224,5.39242,5.37108,5.39224,5.39242,5.37108,5.39242,5.37108),
                      latDst = c(43.28401,43.28368,43.47902,43.28401,43.28368,43.47902,43.28368,43.47902),
                      distance = c(0.952,0.912,28.656,0.000,0.519,28.456,0.000,28.496),
                      stringsAsFactors = FALSE)

  exp13 <- data.frame(ID = c(1:1),
                      idSrc = "A",
                      lonSrc = 5.39200,
                      latSrc = 43.28292,
                      idDst = "B",
                      lonDst = 5.37108,
                      latDst = 43.47902,
                      duree = 28.87,
                      distance = 28.656,
                      stringsAsFactors = FALSE)

  #donnees

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  src1 <- data.frame(idSrc = c("Site Delpuech","Site Delpuech","Site Menpenti","Site Aix-en-Provence"),
                     lonSrc = c(5.39200,5.39200,5.39242,5.37107),
                     latSrc = c(43.28292,43.28292,43.28368,43.47900),
                     stringsAsFactors = F)

  dst1 <- data.frame(idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Autre"),
                     lonDst = c(5.39242,5.37107,5.39200,5.39242),
                     latDst = c(43.28368,43.47900,43.28292,43.28368),
                     stringsAsFactors = F)

  src2 <- data.frame(idSrc = c("Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.39200,5.39242),
                     latSrc = c(43.28292,43.28368),
                     stringsAsFactors = F)

  dst2 <- data.frame(idDst = c("Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37107),
                     latDst = c(43.28368,43.47900),
                     stringsAsFactors = F)

  src3 <- data.frame(idSrc = c("Site Delpuech","Site Delpuech"),
                     lonSrc = c(5.39200,5.39200),
                     latSrc = c(43.28292,43.28292),
                     stringsAsFactors = F)

  dst3 <- data.frame(idDst = c("Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37107),
                     latDst = c(43.28368,43.47900),
                     stringsAsFactors = F)

  src4 <- data.frame(idSrc = "Saint-Louis",
                     lonSrc = -61.31882,
                     latSrc = 15.95387,
                     stringsAsFactors = F)

  dst4 <- data.frame(idDst = "Vieux Habitants",
                     lonDst = -61.76451,
                     latDst = 16.06016,
                     stringsAsFactors = F)

  src5 <- data.frame(idSrc = c("A","B"),
                     lonSrc = c(5.39200,5.39290),
                     latSrc = c(43.28292,43.28421),
                     stringsAsFactors = F)

  dst5 <- data.frame(idDst = c("A","B"),
                     lonDst = c(5.39200,5.39290),
                     latDst = c(43.28292,43.28421),
                     stringsAsFactors = F)

  src6 <- data.frame(idSrc = c("A","B","C"),
                     lonSrc = c(5.39200,5.39290,5.39242),
                     latSrc = c(43.28292,43.28421,43.28368),
                     stringsAsFactors = F)

  dst6 <- data.frame(idDst = c("B","C","D"),
                     lonDst = c(5.39290,5.39242,5.37107),
                     latDst = c(43.28421,43.28368,43.47900),
                     stringsAsFactors = F)

  src7 <- data.frame(idSrc = "A",
                     lonSrc = 5.39200,
                     latSrc = 43.28292,
                     stringsAsFactors = F)

  dst7 <- data.frame(idDst = "B",
                     lonDst = 5.37107,
                     latDst = 43.47900,
                     stringsAsFactors = F)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(metricOsrmTable(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = TRUE, interactive = FALSE)), exp1)
  expect_equal(names(metricOsrmTable(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = TRUE, interactive = FALSE)), exp2)

  expect_equal(class(metricOsrmTable(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, interactive = FALSE)), exp1)
  expect_equal(names(metricOsrmTable(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, interactive = FALSE)), exp2)

  ###################################################################
  # TEST 2 : Cas nominal : duration, distance, exclude et faceAFace #
  ###################################################################

  res <- metricOsrmTable(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = TRUE, interactive = FALSE)
  expect_identical(res, exp3_faf)

  res <- metricOsrmTable(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, interactive = FALSE)
  expect_identical(res, exp3_pc)

  res <- metricOsrmTable(src = src2, dst = dst2, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = TRUE, interactive = FALSE)
  expect_identical(res, exp4_faf)

  res <- metricOsrmTable(src = src2, dst = dst2, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, interactive = FALSE)
  expect_identical(res, exp4_pc)

  res <- metricOsrmTable(src = src2, dst = src2, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = TRUE, stable = FALSE, interactive = FALSE)
  expect_identical(res, exp4_faf2)

  res <- metricOsrmTable(src = src3, dst = dst3, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = TRUE, interactive = FALSE)
  expect_identical(res, exp5_faf)

  res <- metricOsrmTable(src = src3, dst = dst3, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, interactive = FALSE)
  expect_identical(res, exp5_pc)

  res <- metricOsrmTable(src = src3, dst = dst3, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, rayonMax = 1, nbDstVolOiseau = 0, nbDstMeasure = 0, optiMeasure = "duree", interactive = FALSE)
  expect_identical(res, exp6)

  res <- metricOsrmTable(src = src3, dst = dst3, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, rayonMax = 1, nbDstVolOiseau = 1, nbDstMeasure = 0, optiMeasure = "duree", interactive = FALSE)
  expect_identical(res, exp6)

  res <- metricOsrmTable(src = src3, dst = dst3, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, rayonMax = 1, nbDstVolOiseau = 1, nbDstMeasure = 1, optiMeasure = "duree", interactive = FALSE)
  expect_identical(res, exp6)

  expect_message(res <- metricOsrmTable(src = src4, dst = dst4, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, interactive = FALSE))
  expect_identical(res, exp7)

  expect_message(res <- metricOsrmTable(src = rbind(src4,src7), dst = rbind(dst4,dst7), duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, interactive = FALSE))
  expect_identical(res, exp8)

  res <- metricOsrmTable(src = src5, dst = dst5, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, allerRetour = FALSE, interactive = FALSE)
  expect_identical(res, exp9)

  res <- metricOsrmTable(src = src6, dst = dst6, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, allerRetour = FALSE, interactive = FALSE)
  expect_identical(res, exp10)

  res <- metricOsrmTable(src = src6, dst = dst6, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, allerRetour = FALSE, stable = TRUE, interactive = FALSE)
  expect_identical(res, exp11)

  res <- metricOsrmTable(src = src6, dst = dst6, duree = FALSE, distance = TRUE, exclude = NULL, faceAFace = FALSE, allerRetour = FALSE, stable = TRUE, interactive = FALSE)
  expect_identical(res, exp12)

  res <- metricOsrmTable(src = src7, dst = dst7, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, allerRetour = FALSE, stable = TRUE, interactive = FALSE)
  expect_identical(res, exp13)

  res <- metricOsrmTable(src = src7, dst = dst7, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, rayonMax = 5, nbDstMeasure = 1, optiMeasure = "duree", interactive = FALSE)
  expect_null(res)

  res <- metricOsrmTable(src = src7, dst = dst7, duree = TRUE, distance = TRUE, stable = FALSE, exclude = NULL, faceAFace = FALSE, rayonMax = 5, nbDstMeasure = 1, optiMeasure = "duree", interactive = FALSE)
  expect_null(res)

  ##################################################################
  # TEST 3 : Exception : formats src, dst et exclude non conformes #
  ##################################################################

  expect_error(metricOsrmTable(src = src1[-1], dst = dst1[-1], duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = TRUE, interactive = FALSE))
  expect_error(metricOsrmTable(src = src1[-1], dst = dst1[-1], duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, interactive = FALSE))

  expect_error(metricOsrmTable(src = src1[,c(2,3,1)], dst = dst1[,c(2,3,1)], duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = TRUE, interactive = FALSE))
  expect_error(metricOsrmTable(src = src1[,c(2,3,1)], dst = dst1[,c(2,3,1)], duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, interactive = FALSE))

  expect_error(metricOsrmTable(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = TRUE, interactive = TRUE))
  expect_error(metricOsrmTable(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, interactive = TRUE))

  expect_error(metricOsrmTable(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, faceAFace = FALSE, rayonMax = 1, nbDstVolOiseau = 1, nbDstMeasure = 1, optiMeasure = "autre", interactive = FALSE))
})
