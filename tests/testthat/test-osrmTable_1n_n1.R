### devtools::test()

### library(covr)
### covr <- file_coverage("R/osrmTable_1n_n1.R", "tests/testthat/test-osrmTable_1n_n1.R")
### report(covr)

library(testthat)

test_that('Calcul la durée et/ou la distance 1n ou n1 OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("idSrc","lonSrc","latSrc","idDst","lonDst","latDst","duree","distance")

  exp3 <- data.frame(idSrc = "Site Delpuech", lonSrc = 5.392, latSrc = 43.28292, idDst = "Site Menpenti", lonDst = 5.39242, latDst = 43.28368, duree = round(97,0), distance = round(912,0), stringsAsFactors = FALSE)
  exp4 <- data.frame(idSrc = "Site Delpuech", lonSrc = 5.392, latSrc = 43.28292, idDst = "Site Aix-en-Provence", lonDst = 5.37108, latDst = 43.47902, duree = round(2183,0), distance = round(28405,0), stringsAsFactors = FALSE)
  exp5 <- data.frame(idSrc = "Site Delpuech", lonSrc = 5.392, latSrc = 43.28292, idDst = "Site Menpenti", lonDst = 5.39242, latDst = 43.28368, stringsAsFactors = FALSE)
  exp6 <- data.frame(idSrc = c("Site Delpuech","Site Delpuech"), lonSrc = c(5.392,5.392), latSrc = c(43.28292,43.28292), idDst = c("Site Menpenti","Site Aix-en-Provence"), lonDst = c(5.39242,5.37108), latDst = c(43.28368,43.47902), duree = round(c(97,1732),0), distance = round(c(912,28656),0), stringsAsFactors = FALSE)
  exp7 <- data.frame(idSrc = c("Site Delpuech","Site Menpenti"), lonSrc = c(5.392,5.39242), latSrc = c(43.28292,43.28368), idDst = c("Site Aix-en-Provence","Site Aix-en-Provence"), lonDst = c(5.37108,5.37108), latDst = c(43.47902,43.47902), duree = round(c(1732,1707),0), distance = round(c(28656,28496),0), stringsAsFactors = FALSE)

  #donnees

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  src1 <- data.frame(id = "Site Delpuech", lon = 5.39201, lat = 43.28299, stringsAsFactors = F)
  dst1 <- data.frame(id = "Site Menpenti", lon = 5.39249, lat = 43.28370, stringsAsFactors = F)
  dst2 <- data.frame(id = "Site Aix-en-Provence", lon = 5.37109, lat = 43.47905, stringsAsFactors = F)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(osrmTable_1n_n1(src = src1, dst = dst1, duree = TRUE, distance= TRUE, exclude = NULL)), exp1)
  expect_equal(names(osrmTable_1n_n1(src = src1, dst = dst1, duree = TRUE, distance= TRUE, exclude = NULL)), exp2)

  ###################################################################
  # TEST 2 : Cas nominal : duration, distance, exclude et faceAFace #
  ###################################################################

  # 11
  res <- osrmTable_1n_n1(src = src1, dst = dst1, duree = TRUE, distance= TRUE, exclude = NULL)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp3)

  res <- osrmTable_1n_n1(src = src1, dst = dst2, duree = TRUE, distance= TRUE, exclude = "motorway")
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp4)

  expect_identical(osrmTable_1n_n1(src = src1, dst = dst1, duree = FALSE, distance= FALSE, exclude = NULL), exp5)

  # 1n
  res <- osrmTable_1n_n1(src = src1, dst = rbind(dst1,dst2), duree = TRUE, distance= TRUE, exclude = NULL)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp6)

  # n1
  res <- osrmTable_1n_n1(src = rbind(src1,dst1), dst = dst2, duree = TRUE, distance= TRUE, exclude = NULL)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp7)

  ##################################################################
  # TEST 3 : Exception : formats src, dst et exclude non conformes #
  ##################################################################

  expect_error(osrmTable_1n_n1(src = list(src1), dst = list(dst1), duree = TRUE, distance= TRUE, exclude = NULL))
  expect_error(osrmTable_1n_n1(src = data.frame(lon = 5.39201, lat = 43.28299, stringsAsFactors = F), dst = data.frame(lon = 5.39249, lat = 43.28370, stringsAsFactors = F), duree = TRUE, distance= TRUE, exclude = NULL))
  expect_error(osrmTable_1n_n1(src = src1, dst = dst1, duree = NULL, distance= NULL, exclude = NULL))
})
