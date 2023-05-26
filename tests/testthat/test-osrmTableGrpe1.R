### devtools::test()

### library(covr)
### covr <- file_coverage("R/osrmTableGrpe1.R", "tests/testthat/test-osrmTableGrpe1.R")
### report(covr)

library(testthat)

test_that('Calcul la duree et/ou la distance 1n ou n1 >1000 OK', {

  # resultats attendus
  exp1 <- NULL

  #donnees

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  couples <- data.frame(idSrc = c("Site Delpuech","Site Delpuech","Site Menpenti","Site Aix-en-Provence"),
                         lonSrc = c(5.39200,5.39200,5.39242,5.37107),
                         latSrc = c(43.28292,43.28292,43.28368,43.47900),
                         idDst = c("Site Menpenti","Site Aix-en-Provence","Site Delpuech","Autre"),
                         lonDst = c(5.39242,5.37107,5.39200,5.39242),
                         latDst = c(43.28368,43.47900,43.28292,43.28368),
                         ID = c(1,2,3,4),
                         stringsAsFactors = F)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(osrmTableGrpe1(couples = couples, duree = TRUE, distance = TRUE, exclude = NULL, interactive = FALSE), exp1)

})
