### devtools::test()

### library(covr)
### covr <- file_coverage("R/calculs_faceaface_groupe1.R", "tests/testthat/test-calculs_faceaface_groupe1.R")
### report(covr)

library(testthat)

test_that('Calcul la duree et/ou la distance groupe 1 > 1000 couples OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("ID","idSrc","lonSrc","latSrc","idDst","lonDst","latDst","duree","distance")

  exp3 <- data.frame(ID = c(1,2),
                     idSrc = c("Site Delpuech","Site Delpuech"), lonSrc = c(5.392,5.392), latSrc = c(43.28292,43.28292),
                     idDst = c("Site Menpenti","Site Aix-en-Provence"), lonDst = c(5.39242,5.37108), latDst = c(43.28368,43.47902),
                     duree = round(c(97,1732),0), distance = round(c(912,28656),0),
                     stringsAsFactors = F)

  exp4 <- data.frame(ID = c(1,4),
                     idSrc = c("Site Delpuech","Site Aix-en-Provence"), lonSrc = c(5.392,5.37108), latSrc = c(43.28292,43.47902),
                     idDst = c("Site Menpenti","Site Menpenti"), lonDst = c(5.39242,5.39242), latDst = c(43.28368,43.28368),
                     duree = round(c(97,1774),0), distance = round(c(912,27248),0),
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
  cptSrc1 <- table(couples1[,1])
  cptDst1 <- table(couples1[,4])

  couples2 <- data.frame(idSrc = c("Site Delpuech","Autre","Site Menpenti","Site Aix-en-Provence"),
                         lonSrc = c(5.39200,5.39200,5.39242,5.37107),
                         latSrc = c(43.28292,43.28292,43.28368,43.47900),
                         idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Site Menpenti"),
                         lonDst = c(5.39242,5.37107,5.39200,5.39242),
                         latDst = c(43.28368,43.47900,43.28292,43.28368),
                         ID = c(1,2,3,4),
                         stringsAsFactors = F)
  cptSrc2 <- table(couples2[,1])
  cptDst2 <- table(couples2[,4])

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(calculs_faceaface_groupe1(cptSrc = cptSrc1, cptDst = cptDst1, couples = couples1, duree = TRUE, distance = TRUE, exclude = NULL)), exp1)
  expect_equal(names(calculs_faceaface_groupe1(cptSrc = cptSrc1, cptDst = cptDst1, couples = couples1, duree = TRUE, distance = TRUE, exclude = NULL)), exp2)

  ########################################################
  # TEST 2 : Cas nominal : duration, distance et exclude #
  ########################################################

  res <- calculs_faceaface_groupe1(cptSrc = cptSrc1, cptDst = cptDst1, couples = couples1, duree = TRUE, distance = TRUE, exclude = NULL)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp3)

  res <- calculs_faceaface_groupe1(cptSrc = cptSrc2, cptDst = cptDst2, couples = couples2, duree = TRUE, distance = TRUE, exclude = NULL)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp4)

  #########################################################
  # TEST 3 : Exception : formats src et dst non conformes #
  #########################################################

  expect_error(calculs_faceaface_groupe1(cptSrc = c(1,2,1), cptDst = c(1,1,1,1), couples = couples1, duree = TRUE, distance = TRUE, exclude = NULL))
  expect_error(calculs_faceaface_groupe1(cptSrc = cptSrc1, cptDst = cptDst1, couples = couples1[,-7], duree = TRUE, distance = TRUE, exclude = NULL))
  couples3 <- couples1
  names(couples3) <- c("idSrc","lonSrc","latSrc","idDst","lonDst","latDst","id")
  expect_error(calculs_faceaface_groupe1(cptSrc = cptSrc1, cptDst = cptDst1, couples = couples3, duree = TRUE, distance = TRUE, exclude = NULL))
  expect_error(calculs_faceaface_groupe1(cptSrc = cptSrc1, cptDst = cptDst1, couples = couples1[,c(7,1:6)], duree = TRUE, distance = TRUE, exclude = NULL))
})
