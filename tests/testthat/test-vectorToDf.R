### devtools::test()
### covr <- file_coverage("R/vectorToDf.R", "tests/testthat/test-vectorToDf.R")
### covr
### report(covr)

library(testthat)

test_that('Transforme un vecteur en data.frame (id,lon,lat) OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("id","lon","lat")

  ###############################
  # TEST 1 : Test object return #
  ###############################

  v <- c(1,5.24979,43.44794)

  expect_equal(class(vectorToDf(vector=v)), exp1)
  expect_equal(names(vectorToDf(vector=v)), exp2)
})
