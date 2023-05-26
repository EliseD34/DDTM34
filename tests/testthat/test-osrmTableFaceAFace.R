### devtools::test()

### library(covr)
### covr <- file_coverage("R/osrmTableFaceAFace.R", "tests/testthat/test-osrmTableFaceAFace.R")
### report(covr)

library(testthat)

test_that('Calcul la duree et/ou la distance face a face OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("ID","idSrc","lonSrc","latSrc","idDst","lonDst","latDst","duree","distance")

  exp3 <- data.frame(ID = c(1:4),
                     idSrc = c("Site Delpuech","Site Delpuech","Site Menpenti","Site Aix-en-Provence"),
                     lonSrc = c(5.392,5.392,5.39242,5.37108),
                     latSrc = c(43.28292,43.28292,43.28368,43.47902),
                     idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Autre"),
                     lonDst = c(5.39242,5.37108,5.392,5.39242),
                     latDst = c(43.28368,43.47902,43.28292,43.28368),
                     duree = round(c(97,1732,62,1774),0),
                     distance = round(c(912,28656,567,27248),0),
                     stringsAsFactors = F)

  exp4 <- data.frame(ID = c(1:4),
                     idSrc = c("Site Delpuech","Autre","Site Menpenti","Site Aix-en-Provence"),
                     lonSrc = c(5.392,5.392,5.39242,5.37108),
                     latSrc = c(43.28292,43.28292,43.28368,43.47902),
                     idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Site Menpenti"),
                     lonDst = c(5.39242,5.37108,5.392,5.39242),
                     latDst = c(43.28368,43.47902,43.28292,43.28368),
                     duree = round(c(97,1732,62,1774),0),
                     distance = round(c(912,28656,567,27248),0),
                     stringsAsFactors = F)

  exp5 <- data.frame(ID = c(1:2),
                     idSrc = c("Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.392,5.39242),
                     latSrc = c(43.28292,43.28368),
                     idDst = c("Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37108),
                     latDst = c(43.28368,43.47902),
                     duree = round(c(97,1707),0),
                     distance = round(c(912,28496),0),
                     stringsAsFactors = F)

  exp6 <- data.frame(ID = c(1:2),
                     idSrc = c("Site Delpuech","Site Delpuech"),
                     lonSrc = c(5.392,5.392),
                     latSrc = c(43.28292,43.28292),
                     idDst = c("Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37108),
                     latDst = c(43.28368,43.47902),
                     duree = round(c(97,1732),0),
                     distance = round(c(912,28656),0),
                     stringsAsFactors = F)

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

  src2 <- data.frame(idSrc = c("Site Delpuech","Autre","Site Menpenti","Site Aix-en-Provence"),
                     lonSrc = c(5.39200,5.39200,5.39242,5.37107),
                     latSrc = c(43.28292,43.28292,43.28368,43.47900),
                     stringsAsFactors = F)

  dst2 <- data.frame(idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Site Menpenti"),
                     lonDst = c(5.39242,5.37107,5.39200,5.39242),
                     latDst = c(43.28368,43.47900,43.28292,43.28368),
                     stringsAsFactors = F)

  src3 <- data.frame(idSrc = c("Site Delpuech","Site Menpenti"),
                     lonSrc = c(5.39200,5.39242),
                     latSrc = c(43.28292,43.28368),
                     stringsAsFactors = F)

  dst3 <- data.frame(idDst = c("Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37107),
                     latDst = c(43.28368,43.47900),
                     stringsAsFactors = F)

  src4 <- data.frame(idSrc = c("Site Delpuech","Site Delpuech"),
                     lonSrc = c(5.39200,5.39200),
                     latSrc = c(43.28292,43.28292),
                     stringsAsFactors = F)

  dst4 <- data.frame(idDst = c("Site Menpenti","Site Aix-en-Provence"),
                     lonDst = c(5.39242,5.37107),
                     latDst = c(43.28368,43.47900),
                     stringsAsFactors = F)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(osrmTableFaceAFace(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)), exp1)
  expect_equal(names(osrmTableFaceAFace(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)), exp2)

  ###################################################################
  # TEST 2 : Cas nominal : duration, distance, exclude et faceAFace #
  ###################################################################

  res <- osrmTableFaceAFace(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp3)

  res <- osrmTableFaceAFace(src = src2, dst = dst2, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp4)

  res <- osrmTableFaceAFace(src = src3, dst = dst3, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp5)

  res <- osrmTableFaceAFace(src = src4, dst = dst4, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp6)

  res <- osrmTableFaceAFace(src = src1, dst = dst4, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp6)

  res <- osrmTableFaceAFace(src = src4, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp6)

  ##################################################################
  # TEST 3 : Exception : formats src, dst et exclude non conformes #
  ##################################################################

  expect_error(osrmTableFaceAFace(src = src1[,-1], dst = dst1[,-1], duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE))
  expect_error(osrmTableFaceAFace(src = src1, dst = dst1, duree = TRUE, distance = TRUE, exclude = NULL, interactive = TRUE))
})
