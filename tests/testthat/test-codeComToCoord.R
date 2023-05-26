### devtools::test()

### library(covr)
### covr <- file_coverage("R/codeComToCoord.R", "tests/testthat/test-codeComToCoord.R")
### report(covr)

library(testthat)

test_that('Fonction de passage entre code commune INSEE et coordonnees au chef-lieu (chx) ou au centroide ou a un point sur la commune OK', {

  # resultats attendus

  exp1 <- "data.frame"
  exp2 <- c("code","lon","lat")
  exp3 <- data.frame(code=c("13117","13206"),lon=c(5.24980,5.38236),lat=c(43.44799,43.27021), stringsAsFactors = F)
  exp4 <- data.frame(code=c("13117","13206"),lon=c(5.26358,5.38105),lat=c(43.44978,43.28710), stringsAsFactors = F)
  exp5 <- data.frame(code=c("13117","13206"),lon=c(5.26193,5.38127),lat=c(43.45075,43.28788), stringsAsFactors = F)
  exp6 <- data.frame(code="13206",lon=5.38236,lat=43.27021, stringsAsFactors = F)
  exp7 <- exp6[-1,]

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(codeComToCoord(codeInsee = c("13117","13206"), geo = "2020")), exp1)
  expect_equal(names(codeComToCoord(codeInsee = c("13117","13206"), geo = "2020")), exp2)

  ########################
  # TEST 2 : Cas nominal #
  ########################

  expect_identical(codeComToCoord(codeInsee = c("13117","13206"), geo = "2020"), exp3)
  expect_identical(codeComToCoord(codeInsee = c("13117","13206"), geo = "2019", type = "centroide"), exp4)
  expect_identical(codeComToCoord(codeInsee = c("13117","13206"), geo = "2019", type = "pos"), exp5)

  expect_identical(codeComToCoord(codeInsee = c("13220","13206"), geo = "2020"), exp6)
  expect_identical(codeComToCoord(codeInsee = c("13220","13230","13206"), geo = "2020"), exp6)

  ######################
  # TEST 3 : Exception #
  ######################

  expect_error(codeComToCoord(codeInsee = c("13117","13206"), geo = "2015"))
  expect_identical(codeComToCoord(codeInsee = c("13300"), geo = "2020"), exp7)
})
