### devtools::test()

### library(covr)
### covr <- file_coverage("R/osrmTable_11_nm.R", "tests/testthat/test-osrmTable_11_nm.R")
### report(covr)

library(testthat)

test_that('Calcul la duree et/ou la distance en face a face ou par croisement OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("idSrc","lonSrc","latSrc","idDst","lonDst","latDst","duree","distance")

  exp3 <- data.frame(idSrc = "Site Delpuech", lonSrc = 5.392, latSrc = 43.28292, idDst = "Site Menpenti", lonDst = 5.39242, latDst = 43.28368, duree = round(97,0), distance = round(912,0), stringsAsFactors = FALSE)
  exp4 <- data.frame(idSrc = "Site Delpuech", lonSrc = 5.392, latSrc = 43.28292, idDst = "Site Aix-en-Provence", lonDst = 5.37108, latDst = 43.47902, duree = round(2183,0), distance = round(28405,0), stringsAsFactors = FALSE)
  exp5 <- data.frame(idSrc = "Site Delpuech", lonSrc = 5.392, latSrc = 43.28292, idDst = "Site Menpenti", lonDst = 5.39242, latDst = 43.28368, stringsAsFactors = FALSE)
  exp6 <- data.frame(idSrc = c("Site Delpuech","Site Menpenti"), lonSrc = c(5.392,5.39242), latSrc = c(43.28292,43.28368), idDst = c("Site Menpenti","Site Aix-en-Provence"), lonDst = c(5.39242,5.37108), latDst = c(43.28368,43.47902), duree = round(c(97,1707),0), distance = round(c(912,28496),0), stringsAsFactors = FALSE)
  exp7 <- data.frame(idSrc = "Site Delpuech", lonSrc = 5.392, latSrc = 43.28292, idDst = "Site Aix-en-Provence", lonDst = 5.37108, latDst = 43.47902, duree = round(1732,0), distance = round(28656,0), stringsAsFactors = FALSE)
  exp8 <- data.frame(idSrc = c("Site Delpuech","Site Menpenti","Site Delpuech","Site Menpenti"), lonSrc = c(5.392,5.39242,5.392,5.39242), latSrc = c(43.28292,43.28368,43.28292,43.28368), idDst = c("Site Menpenti","Site Menpenti","Site Aix-en-Provence","Site Aix-en-Provence"), lonDst = c(5.39242,5.39242,5.37108,5.37108), latDst = c(43.28368,43.28368,43.47902,43.47902), duree = round(c(97,0,1732,1707),0), distance = round(c(912,0,28656,28496),0), stringsAsFactors = FALSE)
  exp9 <- data.frame(idSrc = "Saint-Louis", lonSrc = -61.31882, latSrc = 15.95387, idDst = "Vieux Habitants", lonDst = -61.76451, latDst = 16.06015, duree = -60, distance = -1000, stringsAsFactors = FALSE)

  #donnees

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  src1 <- data.frame(id = "Site Delpuech", lon = 5.39201, lat = 43.28299, stringsAsFactors = F)
  dst1 <- data.frame(id = "Site Menpenti", lon = 5.39249, lat = 43.28370, stringsAsFactors = F)
  dst2 <- data.frame(id = "Site Aix-en-Provence", lon = 5.37109, lat = 43.47905, stringsAsFactors = F)

  src3 <- data.frame(id = "Saint-Louis", lon = -61.31882, lat = 15.95387, stringsAsFactors = F)
  dst3 <- data.frame(id = "Vieux Habitants", lon = -61.76451, lat = 16.06016, stringsAsFactors = F)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(osrmTable_11_nm(src = src1, dst = dst1, duree = TRUE, distance= TRUE, exclude = NULL, faceAFace = TRUE)), exp1)
  expect_equal(names(osrmTable_11_nm(src = src1, dst = dst1, duree = TRUE, distance= TRUE, exclude = NULL, faceAFace = TRUE)), exp2)

  ###################################################################
  # TEST 2 : Cas nominal : duration, distance, exclude et faceAFace #
  ###################################################################

  # Face a face : 11
  res <- osrmTable_11_nm(src = src1, dst = dst1, duree = TRUE, distance= TRUE, exclude = NULL, faceAFace = TRUE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp3)

  res <- osrmTable_11_nm(src = src1, dst = dst2, duree = TRUE, distance= TRUE, exclude = "motorway", faceAFace = TRUE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp4)

  expect_identical(osrmTable_11_nm(src = src1, dst = dst1, duree = FALSE, distance= FALSE, exclude = NULL, faceAFace = TRUE), exp5)

  res <- osrmTable_11_nm(src = rbind(src1,dst1), dst = rbind(dst1,dst2), duree = TRUE, distance= TRUE, exclude = NULL, faceAFace = TRUE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp6)

  res <- osrmTable_11_nm(src = rbind(src1,dst1), dst = dst2, duree = TRUE, distance= TRUE, exclude = NULL, faceAFace = TRUE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp7)

  # Produit cartesien : nm
  res <- osrmTable_11_nm(src = rbind(src1,dst1), dst = rbind(dst1,dst2), duree = TRUE, distance= TRUE, exclude = NULL, faceAFace = FALSE)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp8)

  res <- osrmTable_11_nm(src = src3, dst = dst3, duree = TRUE, distance= TRUE, exclude = NULL, faceAFace = FALSE)
  expect_identical(res, exp9)

  ##################################################################
  # TEST 3 : Exception : formats src, dst et exclude non conformes #
  ##################################################################

  expect_error(osrmTable_11_nm(src = list(src1), dst = list(dst1), duree = TRUE, distance= TRUE, exclude = NULL, faceAFace = TRUE))
})
