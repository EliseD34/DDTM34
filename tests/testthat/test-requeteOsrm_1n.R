### devtools::test()

### library(covr)
### covr <- file_coverage("R/requeteOsrm_1n.R", "tests/testthat/test-requeteOsrm_1n.R")
### report(covr)

library(testthat)

test_that('Calcul la duree et/ou la distance 1n OK', {

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
               durations = list(round(c(97,1732),0)),
               distances = list(round(c(912,28656),0))
  )

  exp6 <- list(code = "Ok",
               sources = list(list(hint = NA,
                                   distance = 5.875323,
                                   name = "Rue Menpenti",
                                   location = c(5.392423,43.283680)
               )
               ),
               destinations = list(list(hint = NA,
                                        distance = 5.875323,
                                        name = "Rue Menpenti",
                                        location = c(5.392423,43.283680)
               )
               ),
               durations = list(0),
               distances = list(0)
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

  expect_equal(class(requeteOsrm_1n(src = src1, idx_src = 0, dst = dst1, idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL)), exp1)
  expect_equal(names(requeteOsrm_1n(src = src1, idx_src = 0, dst = dst1, idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL)), exp2)

  ########################################################
  # TEST 2 : Cas nominal : duration, distance et exclude #
  ########################################################

  # Face a face : 11
  res <- requeteOsrm_1n(src = src1, idx_src = 0, dst = dst1, idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL)
  res$sources[[1]]$hint <- NA
  res$destinations[[1]]$hint <- NA
  res$durations[[1]] <- round(res$durations[[1]],0)
  res$distances[[1]] <- round(res$distances[[1]],0)
  expect_identical(res, exp3)

  res <- requeteOsrm_1n(src = src1, idx_src = 0, dst = dst2, idx_dst = 1, measure = c("duration","distance"), exclude_str = "&exclude=motorway")
  res$sources[[1]]$hint <- NA
  res$destinations[[1]]$hint <- NA
  res$destinations[[1]]$name <- NA
  res$durations[[1]] <- round(res$durations[[1]],0)
  res$distances[[1]] <- round(res$distances[[1]],0)
  expect_identical(res, exp4)

  # Produit cartesien : 1n
  res <- requeteOsrm_1n(src = src1, idx_src = 0, dst = rbind(dst1,dst2), idx_dst = c(1,2), measure = c("duration","distance"), exclude_str = NULL)
  res$sources[[1]]$hint <- NA
  res$destinations[[1]]$hint <- NA
  res$destinations[[2]]$hint <- NA
  res$destinations[[2]]$name <- NA
  res$durations[[1]] <- round(res$durations[[1]],0)
  res$distances[[1]] <- round(res$distances[[1]],0)
  expect_identical(res, exp5)

  #########################################################
  # TEST 3 : Exception : formats src et dst non conformes #
  #########################################################

  expect_error(requeteOsrm_1n(src = data.frame(lon = 5.39201, lat = 43.28299, stringsAsFactors = F), idx_src = 0, dst = data.frame(lon = 5.39249, lat = 43.28370, stringsAsFactors = F), idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL))

  ###########################################################
  # TEST 4 : Exception : idx_src et idx_dst non compatibles #
  ###########################################################

  res <- requeteOsrm_1n(src = src1, idx_src = 1, dst = dst1, idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL)
  res$sources[[1]]$hint <- NA
  res$destinations[[1]]$hint <- NA
  res$durations[[1]] <- round(res$durations[[1]],0)
  res$distances[[1]] <- round(res$distances[[1]],0)
  expect_identical(res, exp6)

  res <- requeteOsrm_1n(src = src1, idx_src = 0, dst = dst1, idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL)
  res$sources[[1]]$hint <- NA
  res$destinations[[1]]$hint <- NA
  res$durations[[1]] <- round(res$durations[[1]],0)
  res$distances[[1]] <- round(res$distances[[1]],0)
  expect_identical(res, exp3)

  options(osrm.server = NULL)
  options(osrm.profile = NULL)
  expect_error(requeteOsrm_1n(src = src1, idx_src = 0, dst = dst1, idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL))

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr")
  options(osrm.profile = "driving")

  res <- requeteOsrm_1n(src = src1, idx_src = 0, dst = dst1, idx_dst = 1, measure = c("duration","distance"), exclude_str = NULL)
  res$sources[[1]]$hint <- NA
  res$destinations[[1]]$hint <- NA
  res$durations[[1]] <- round(res$durations[[1]],0)
  res$distances[[1]] <- round(res$distances[[1]],0)
  expect_identical(res, exp3)

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")
})
