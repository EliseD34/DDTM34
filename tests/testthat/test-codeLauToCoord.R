### devtools::test()

### library(covr)
### covr <- file_coverage("R/codeLauToCoord.R", "tests/testthat/test-codeLauToCoord.R")
### report(covr)

library(testthat)

test_that('Fonction de passage entre code LAU de communes etrangeres et coordonnees au centroide ou a un point situé sur la commune OK', {

  # resultats attendus

  exp1 <- "data.frame"
  exp2 <- c("code","lon","lat")
  exp3 <- data.frame(code=c("BE_21004","BE_62063","LU_0304","DE_08111000"),lon=c(4.35172,5.59313,6.12919,9.18974),lat=c(50.85455,50.62404,49.60757,48.77909), stringsAsFactors = F)
  exp4 <- data.frame(code=c("BE_21004","BE_62063","LU_0304","DE_08111000"),lon=c(4.37592,5.59056,6.12678,9.17213),lat=c(50.87289,50.63171,49.61403,48.77481), stringsAsFactors = F)
  exp5 <- data.frame(code=c("13117","13206"),lon=c(5.26193,5.38127),lat=c(43.45075,43.28788), stringsAsFactors = F)
  exp6 <- data.frame(code="13206",lon=5.38401,lat=43.28584, stringsAsFactors = F)
  exp7 <- exp6[-1,]

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(codeLauToCoord(codePays = c("BE","LU"), codeLau = c("21004","0304"))), exp1)
  expect_equal(names(codeLauToCoord(codePays = c("BE","LU"), codeLau = c("21004","0304"))), exp2)

  ########################
  # TEST 2 : Cas nominal #
  ########################

  expect_identical(codeLauToCoord(codePays = c("BE","BE","LU","DE"),
                                  codeLau = c("21004","62063","0304","08111000"),
                                  type = "pos"), exp3)
  expect_identical(codeLauToCoord(codePays = c("BE","BE","LU","DE"),
                                  codeLau = c("21004","62063","0304","08111000"),
                                  type = "centroide"), exp4)


  ######################
  # TEST 3 : Exception #
  ######################

  expect_identical(codeLauToCoord(codePays = c("FR","BE","BE","LU","DE"),
                                  codeLau = c("57463","21004","62063","0304","08111000"),
                                  type = "pos"), exp3)

  expect_identical(codeLauToCoord(codePays = c("FR","FR","BE","BE","LU","DE"),
                                  codeLau = c("57463","67482","21004","62063","0304","08111000"),
                                  type = "pos"), exp3)

  expect_identical(codeLauToCoord(codePays = c("BE","BE","LU","DE","DE"),
                                  codeLau = c("21004","62063","0304","08111000","11000000"),
                                  type = "pos"), exp3)
  expect_identical(codeLauToCoord(codePays = c("BE","BE","LU","DE","DE","NL"),
                                  codeLau = c("21004","62063","0304","08111000","11000000","GM0363"),
                                  type = "pos"), exp3)
})
