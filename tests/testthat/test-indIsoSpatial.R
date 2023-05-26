### devtools::test()

### library(covr)
### covr <- file_coverage("R/indIsoSpatial.R", "tests/testthat/test-indIsoSpatial.R")
### report(covr)

library(testthat)

test_that('Calcul des indicateurs en volume par regroupement de mailles selon leur appartenance spatiale OK', {

  options(osrm.server = "https://metric-osrm-backend.lab.sspcloud.fr/")
  options(osrm.profile = "driving")

  #donnees

  dst <- data.frame(id = 1,
                    lng = 5.44142,
                    lat = 43.53573,
                    stringsAsFactors = FALSE)

  iso <- metricOsrmIso(loc = dst,
                       breaks = seq(from = 0, to = 3, length.out = 2),
                       exclude = NULL,
                       res = 11,
                       returnclass = "sf",
                       fusion = TRUE,
                       courbes = "isochrones")
  iso <- sf::st_transform(iso[[1]], crs = 2154)

  grille <- sf::st_as_sf(sf::st_make_grid(iso, cellsize = 300))
  grille <- grille[-c(1,5,21,25),]
  names(grille) <- "geometry"
  attr(grille, "sf_column") <- "geometry"
  grille2 <- grille

  grille$id <- c(1:nrow(grille))
  grille$pop <- c(348.0,281.0,288.5,102.5,180.0,92.5,173.0,522.0,602.5,180.0,804.0,
                  103.0,155.0,138.0,788.5,307.0,237.0,533.5,91.0,122.4,98.8)
  grille$pop_2 <- grille$pop*2
  grille$pop_pond <- 0.75
  grille$pop_pond_2 <- 0.5
  grille <- grille[,c(2:6,1)]

  grille2 <- grille[c(6,10,11,12,16),]

  # resultats attendus

  exp1 <- c("sf","data.frame")
  exp2 <- c("id","min","max","geometry","center","pop")

  exp3 <- cbind(iso, pop = 6148.2)
  exp3 <- exp3[,c("id","min","max","geometry","center","pop")]
  attr(exp3, "agr") <- c("id","min","max","center","pop")

  exp4 <- cbind(iso, pop = 3469)
  exp4 <- exp4[,c("id","min","max","geometry","center","pop")]
  attr(exp4, "agr") <- c("id","min","max","center","pop")

  exp5 <- cbind(iso, pop = 1486.5)
  exp5 <- exp5[,c("id","min","max","geometry","center","pop")]
  attr(exp5, "agr") <- c("id","min","max","center","pop")

  exp6 <- cbind(iso, pop = 4611.1)
  exp6 <- exp6[,c("id","min","max","geometry","center","pop")]
  attr(exp6, "agr") <- c("id","min","max","center","pop")

  exp7 <- cbind(iso, pop = 2601.7)
  exp7 <- exp7[,c("id","min","max","geometry","center","pop")]
  attr(exp7, "agr") <- c("id","min","max","center","pop")

  exp8 <- cbind(iso, pop = 1114.9)
  exp8 <- exp8[,c("id","min","max","geometry","center","pop")]
  attr(exp8, "agr") <- c("id","min","max","center","pop")

  exp9 <- cbind(iso, pop = 6148.2, pop_2 = 12296.4)
  exp9 <- exp9[,c("id","min","max","geometry","center","pop","pop_2")]
  attr(exp9, "agr") <- c("id","min","max","center","pop","pop_2")

  exp10 <- cbind(iso, pop = 3469, pop_2 = 6937.9)
  exp10 <- exp10[,c("id","min","max","geometry","center","pop","pop_2")]
  attr(exp10, "agr") <- c("id","min","max","center","pop","pop_2")

  exp11 <- cbind(iso, pop = 1486.5, pop_2 = 2973)
  exp11 <- exp11[,c("id","min","max","geometry","center","pop","pop_2")]
  attr(exp11, "agr") <- c("id","min","max","center","pop","pop_2")

  exp12 <- cbind(iso, pop = 4611.1, pop_2 = 6148.2)
  exp12 <- exp12[,c("id","min","max","geometry","center","pop","pop_2")]
  attr(exp12, "agr") <- c("id","min","max","center","pop","pop_2")

  exp13 <- cbind(iso, pop = 2601.7, pop_2 = 3469)
  exp13 <- exp13[,c("id","min","max","geometry","center","pop","pop_2")]
  attr(exp13, "agr") <- c("id","min","max","center","pop","pop_2")

  exp14 <- cbind(iso, pop = 1114.9, pop_2 = 1486.5)
  exp14 <- exp14[,c("id","min","max","geometry","center","pop","pop_2")]
  attr(exp14, "agr") <- c("id","min","max","center","pop","pop_2")

  exp15 <- cbind(grille, groupe_pol = 1, sum_pop = 6148.2, sum_pop_2 = 12296.4)
  exp16 <- cbind(as.data.frame(grille)[,-ncol(grille)], groupe_pol = 1, sum_pop = 3469, sum_pop_2 = 6937.9)
  exp17 <- cbind(grille2, groupe_pol = 1, sum_pop = 1486.5, sum_pop_2 = 2973)

  exp18 <- cbind(grille, groupe_pol = 1, sum_pop = 4611.1, sum_pop_2 = 6148.2)
  exp19 <- cbind(as.data.frame(grille)[,-ncol(grille)], groupe_pol = 1, sum_pop = 2601.7, sum_pop_2 = 3469)
  exp20 <- cbind(grille2, groupe_pol = 1, sum_pop = 1114.9, sum_pop_2 = 1486.5)

  exp21 <- list(exp12, exp18)
  exp22 <- list(exp13, exp19)
  exp23 <- list(exp14, exp20)

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = NULL, choixIntersect = 1, return = "pol", idGroup = NULL)), exp1)
  expect_equal(names(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = NULL, choixIntersect = 1, return = "pol", idGroup = NULL)), exp2)

  ############################################################
  # TEST 2 : Cas nominal : var, pond, return, choixIntersect #
  ############################################################

  expect_identical(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = NULL, choixIntersect = 1, return = "pol", idGroup = NULL), exp3)
  expect_identical(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = NULL, choixIntersect = 2, return = "pol", idGroup = NULL), exp4)
  expect_identical(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = NULL, choixIntersect = 3, return = "pol", idGroup = NULL), exp5)

  expect_identical(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = "pop_pond", choixIntersect = 1, return = "pol", idGroup = NULL), exp6)
  expect_identical(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = "pop_pond", choixIntersect = 2, return = "pol", idGroup = NULL), exp7)
  expect_identical(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = "pop_pond", choixIntersect = 3, return = "pol", idGroup = NULL), exp8)

  expect_identical(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = NULL, choixIntersect = 1, return = "pol", idGroup = NULL), exp9)
  expect_identical(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = NULL, choixIntersect = 2, return = "pol", idGroup = NULL), exp10)
  expect_identical(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = NULL, choixIntersect = 3, return = "pol", idGroup = NULL), exp11)

  expect_identical(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = c("pop_pond","pop_pond_2"), choixIntersect = 1, return = "pol", idGroup = NULL), exp12)
  expect_identical(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = c("pop_pond","pop_pond_2"), choixIntersect = 2, return = "pol", idGroup = NULL), exp13)
  expect_identical(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = c("pop_pond","pop_pond_2"), choixIntersect = 3, return = "pol", idGroup = NULL), exp14)

  expect_equal(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = NULL, choixIntersect = 1, return = "maille", idGroup = "id"), exp15)
  res <- indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = NULL, choixIntersect = 2, return = "maille", idGroup = "id")
  expect_equal(as.data.frame(res)[,-ncol(res)], exp16)
  expect_equal(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = NULL, choixIntersect = 3, return = "maille", idGroup = "id"), exp17)

  expect_equal(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = c("pop_pond","pop_pond_2"), choixIntersect = 1, return = "maille", idGroup = "id"), exp18)
  res <- indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = c("pop_pond","pop_pond_2"), choixIntersect = 2, return = "maille", idGroup = "id")
  expect_equal(as.data.frame(res)[,-ncol(res)], exp19)
  expect_equal(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = c("pop_pond","pop_pond_2"), choixIntersect = 3, return = "maille", idGroup = "id"), exp20)

  expect_equal(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = c("pop_pond","pop_pond_2"), choixIntersect = 1, return = c("pol","maille"), idGroup = "id"), exp21)
  res <- indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = c("pop_pond","pop_pond_2"), choixIntersect = 2, return = c("pol","maille"), idGroup = "id")
  res[[2]] <- as.data.frame(res[[2]])[,-ncol(res[[2]])]
  expect_equal(res, exp22)
  expect_equal(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop_2"), pond = c("pop_pond","pop_pond_2"), choixIntersect = 3, return = c("pol","maille"), idGroup = "id"), exp23)

  #######################
  # TEST 3 : Exceptions #
  #######################

  expect_error(indIsoSpatial(pol = iso, maille = grille, var = "evol", pond = NULL, choixIntersect = 1, return = "pol", idGroup = NULL))
  expect_error(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = "popPond", choixIntersect = 1, return = "pol", idGroup = NULL))
  expect_error(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = NULL, choixIntersect = 4, return = "pol", idGroup = NULL))
  expect_error(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = NULL, choixIntersect = 1, return = "ligne", idGroup = NULL))
  expect_error(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = NULL, choixIntersect = 1, return = "maille", idGroup = "ID"))
  expect_error(indIsoSpatial(pol = iso, maille = grille, var = "pop", pond = c("pop_pond","pop_pond_2"), choixIntersect = 1, return = "maille", idGroup = "id"))
  expect_error(indIsoSpatial(pol = iso, maille = grille, var = c("pop","pop2"), pond = "pop_pond", choixIntersect = 1, return = "maille", idGroup = "id"))
})
