### devtools::test()

### library(covr)
### covr <- file_coverage("R/calculs_faceaface_groupe2.R", "tests/testthat/test-calculs_faceaface_groupe2.R")
### report(covr)

library(testthat)

test_that('Calcul la duree et/ou la distance groupe 2 < 1000 et > 2 couples OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("ID","idSrc","lonSrc","latSrc","idDst","lonDst","latDst","duree","distance")

  exp3 <- data.frame(ID = c(1,2),
                     idSrc = c("Site Delpuech","Site Delpuech"), lonSrc = c(5.392,5.392), latSrc = c(43.28292,43.28292),
                     idDst = c("Site Menpenti","Site Aix-en-Provence"), lonDst = c(5.39242,5.37108), latDst = c(43.28368,43.47902),
                     duree = round(c(97,1732),0), distance = round(c(912,28655),0),
                     stringsAsFactors = F)


  exp4 <- data.frame(ID = c(1,4),
                     idSrc = c("Site Delpuech","Site Aix-en-Provence"), lonSrc = c(5.392,5.37108), latSrc = c(43.28292,43.47902),
                     idDst = c("Site Menpenti","Site Menpenti"), lonDst = c(5.39242,5.39242), latDst = c(43.28368,43.28368),
                     duree = round(c(97,1774),0), distance = round(c(912,27248),0),
                     stringsAsFactors = F)

  #donnees

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  couples <- list(data.frame(ID = c(1,2),
                             idSrc = c("Site Delpuech","Site Delpuech"), lonSrc = c(5.392,5.392), latSrc = c(43.28292,43.28292),
                             idDst = c("Site Menpenti","Site Aix-en-Provence"), lonDst = c(5.39242,5.37108), latDst = c(43.28368,43.47902),
                             stringsAsFactors = F),
                  data.frame(ID = c(1,4),
                             idSrc = c("Site Delpuech","Site Aix-en-Provence"), lonSrc = c(5.392,5.37108), latSrc = c(43.28292,43.47902),
                             idDst = c("Site Menpenti","Site Menpenti"), lonDst = c(5.39242,5.39242), latDst = c(43.28368,43.28368),
                             stringsAsFactors = F)
             )

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(calculs_faceaface_groupe2(id = 1, dt_id = couples[[1]], duree = TRUE, distance = TRUE, exclude = NULL)), exp1)
  expect_equal(names(calculs_faceaface_groupe2(id = 1, dt_id = couples[[1]], duree = TRUE, distance = TRUE, exclude = NULL)), exp2)

  ########################################################
  # TEST 2 : Cas nominal : duration, distance et exclude #
  ########################################################

  res <- calculs_faceaface_groupe2(id = 1, dt_id = couples[[1]], duree = TRUE, distance = TRUE, exclude = NULL)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp3)

  res <- calculs_faceaface_groupe2(id = 4, dt_id = couples[[2]], duree = TRUE, distance = TRUE, exclude = NULL)
  res$duree <- round(res$duree,0)
  res$distance <- round(res$distance,0)
  expect_identical(res, exp4)

  #########################################################
  # TEST 3 : Exception : formats src et dst non conformes #
  #########################################################

  expect_error(calculs_faceaface_groupe2(id = 2, dt_id = couples[[1]], duree = TRUE, distance = TRUE, exclude = NULL))
  expect_error(calculs_faceaface_groupe2(id = 1, dt_id = couples[[3]], duree = TRUE, distance = TRUE, exclude = NULL))
  expect_error(calculs_faceaface_groupe2(id = 1, dt_id = couples, duree = TRUE, distance = TRUE, exclude = NULL))
})
