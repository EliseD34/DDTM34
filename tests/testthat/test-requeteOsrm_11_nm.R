### devtools::test()

### library(covr)
### covr <- file_coverage("R/requeteOsrm_11_nm.R", "tests/testthat/test-requeteOsrm_11_nm.R")
### report(covr)

library(testthat)

test_that('Calcul la duree et/ou la distance en face a face ou par croisement OK', {

  # resultats attendus
  exp1 <- "list"
  exp2 <- c("code","sources","destinations","durations","distances")

  exp3 <- list(code = "Ok",
               sources = list(list(hint = NA,
                                   distance = 8.357533,
                                   name = "Boulevard Vincent Delpuech",
                                   location = c(5.392002,43.282915)
                                  )
                              ),
               destinations = list(list(hint = NA,
                                   distance = 5.875323,
                                   name = "Rue Menpenti",
                                   location = c(5.392423,43.283680)
                                  )
                              ),
               durations = list(round(97,0)),
               distances = list(round(912,0))
          )

  exp4 <- list(code = "Ok",
               sources = list(list(hint = NA,
                                   distance = 8.357533,
                                   name = "Boulevard Vincent Delpuech",
                                   location = c(5.392002,43.282915)
               )
               ),
               destinations = list(list(hint = NA,
                                        distance = 2.916638,
                                        name = NA,
                                        location = c(5.371079,43.479025)
               )
               ),
               durations = list(round(2183,0)),
               distances = list(round(28405,0))
  )

  exp5 <- list(code = "Ok",
               sources = list(list(hint = NA,
                                   distance = 8.357533,
                                   name = "Boulevard Vincent Delpuech",
                                   location = c(5.392002,43.282915)
                              ),
                              list(hint = NA,
                                   distance = 5.875323,
                                   name = "Rue Menpenti",
                                   location = c(5.392423,43.283680)
                              )
                        ),
               destinations = list(list(hint = NA,
                                        distance = 5.875323,
                                        name = "Rue Menpenti",
                                        location = c(5.392423,43.283680)
                                  ),
                                  list(hint = NA,
                                       distance = 2.916638,
                                       name = NA,
                                       location = c(5.371079,43.479025)
                                  )
               ),
               durations = list(round(c(97,1732),0),round(c(0,1707),0)),
               distances = list(round(c(912,28656),0),round(c(0,28496),0))
  )

  #donnees

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  src1 <- data.frame(id = "Site Delpuech", lon = 5.39201, lat = 43.28299, stringsAsFactors = F)
  dst1 <- data.frame(id = "Site Menpenti", lon = 5.39249, lat = 43.28370, stringsAsFactors = F)
  dst2 <- data.frame(id = "Site Aix-en-Provence", lon = 5.37109, lat = 43.47905, stringsAsFactors = F)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(requeteOsrm_11_nm(src = src1, idx_src = 1, dst = dst1, idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL)), exp1)
  expect_equal(names(requeteOsrm_11_nm(src = src1, idx_src = 1, dst = dst1, idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL)), exp2)

  ########################################################
  # TEST 2 : Cas nominal : duration, distance et exclude #
  ########################################################

  # Face a face : 11
  res <- requeteOsrm_11_nm(src = src1, idx_src = 1, dst = dst1, idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL)
  res$sources[[1]]$hint <- NA
  res$destinations[[1]]$hint <- NA
  res$durations[[1]] <- round(res$durations[[1]],0)
  res$distances[[1]] <- round(res$distances[[1]],0)
  expect_identical(res, exp3)

  res <- requeteOsrm_11_nm(src = src1, idx_src = 1, dst = dst2, idx_dst = 1, measure = c("duration","distance"), exclude_str = "&exclude=motorway")
  res$sources[[1]]$hint <- NA
  res$destinations[[1]]$hint <- NA
  res$destinations[[1]]$name <- NA
  res$durations[[1]] <- round(res$durations[[1]],0)
  res$distances[[1]] <- round(res$distances[[1]],0)
  expect_identical(res, exp4)

  # Produit cartesien : nm
  res <- requeteOsrm_11_nm(src = rbind(src1,dst1), idx_src = c(1,2), dst = rbind(dst1,dst2), idx_dst = c(1,2), measure = c("duration","distance"), exclude_str = NULL)
  res$sources[[1]]$hint <- NA
  res$sources[[2]]$hint <- NA
  res$destinations[[1]]$hint <- NA
  res$destinations[[2]]$hint <- NA
  res$destinations[[2]]$name <- NA
  res$durations[[1]] <- round(res$durations[[1]],0)
  res$durations[[2]] <- round(res$durations[[2]],0)
  res$distances[[1]] <- round(res$distances[[1]],0)
  res$distances[[2]] <- round(res$distances[[2]],0)
  expect_identical(res, exp5)

  options(osrm.server = NULL)
  options(osrm.profile = NULL)
  expect_error(requeteOsrm_11_nm(src = src1, idx_src = 1, dst = dst1, idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL))

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr")
  options(osrm.profile = "driving")

  res <- requeteOsrm_11_nm(src = src1, idx_src = 1, dst = dst1, idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL)
  res$sources[[1]]$hint <- NA
  res$destinations[[1]]$hint <- NA
  res$durations[[1]] <- round(res$durations[[1]],0)
  res$distances[[1]] <- round(res$distances[[1]],0)
  expect_identical(res, exp3)

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")
})
