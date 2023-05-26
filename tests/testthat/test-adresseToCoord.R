### devtools::test()

### library(covr)
### covr <- file_coverage("R/adresseToCoord.R", "tests/testthat/test-adresseToCoord.R")
### report(covr)

library(testthat)

test_that('Geolocalise des adresses et renvoie leurs coordonnees en WGS84 (EPSG : 4326) OK', {

  # resultats attendus
  exp1 <- "data.frame"
  exp2 <- c("ADRESSES","ADRESSES_GEOLOC","LON","LAT", "SCORE")
  exp3 <- data.frame(ADRESSES=c("17 rue Menpenti 13010 Marseille","37-39 boulevard Vincent Delpluech 13006 Marseille"),
                     ADRESSES_GEOLOC=c("17 Rue menpenti 13010 Marseille","39 Boulevard vincent delpuech 13006 Marseille"),
                     LON=c(5.39250,5.39202),
                     LAT=c(43.2837,43.2830),
                     SCORE=c(0.96534,0.77010),
                     stringsAsFactors = F)
  exp4 <- data.frame(ADRESSES=c("177"),
                     ADRESSES_GEOLOC="unknown",
                     LON=0,
                     LAT=0,
                     SCORE=0,
                     stringsAsFactors = F)
  exp5 <- data.frame(ADRESSES=c("17 rue Menpenti","37-39 boulevard Vincent Delpluech"),
                     ADRESSES_GEOLOC=c("17 Rue menpenti 13010 Marseille","39 Boulevard vincent delpuech 13006 Marseille"),
                     LON=c(5.39250,5.39202),
                     LAT=c(43.2837,43.2830),
                     SCORE=c(0.96529,0.72168), # 0.9652863636363636, 0.7216760330578512
                     stringsAsFactors = F)
  exp6 <- data.frame(ADRESSES=c("Abeilles Marseille","Abeilles Marseille","Abeilles Marseille","Chapelle Marseille","Chapelle Marseille","Chapelle Marseille"),
                     ADRESSES_GEOLOC=c("Rue des abeilles 13001 Marseille","Allée des abeilles 13016 Marseille","Impasse abeille 13003 Marseille",
                                       "Boulevard de la chapelle 13009 Marseille","Rue de la chapelle 13003 Marseille","Traverse de la chapelle 13011 Marseille"),
                     LON=c(5.38589,5.33855,5.37802,5.41172,5.37037,5.51193),
                     LAT=c(43.30134,43.36358,43.31516,43.27183,43.31639,43.30201),
                     SCORE=c(0.69535,0.69001,0.52373,0.69835,0.69241,0.69003),
                     stringsAsFactors = F)
  exp7 <- data.frame(ADRESSES="",
                     ADRESSES_GEOLOC="unknown",
                     LON=0,
                     LAT=0,
                     SCORE=0,
                     stringsAsFactors = F)
  exp8 <- data.frame(ADRESSES=c("Allee des Abeilles Marseille"),
                     ADRESSES_GEOLOC=c("Allée des abeilles 13016 Marseille"),
                     LON=c(5.33855),
                     LAT=c(43.36358),
                     SCORE=c(0.96273),
                     stringsAsFactors = F)

  #donnees

  adr1 = c("17 rue Menpenti 13010 Marseille", "37-39 boulevard Vincent Delpluech 13006 Marseille")
  adr2 = c("17 rue Menpenti", "37-39 boulevard Vincent Delpluech")
  adr3 = c("Abeilles Marseille","Chapelle Marseille")
  adr4 = ""
  adr5 = c("Allée des Abeilles Marseille")

  ###############################
  # TEST 1 : Test object return #
  ###############################

  expect_equal(class(adresseToCoord(adresses = adr1, nbEchos = 1, codePostal = NULL, codeInsee = NULL)), exp1)
  expect_equal(names(adresseToCoord(adresses = adr1, nbEchos = 1, codePostal = NULL, codeInsee = NULL)), exp2)

  #######################################################
  # TEST 2 : Cas nominal : avec codePostal et codeInsee #
  #######################################################

  expect_identical(adresseToCoord(adresses = adr1, nbEchos = 1, codePostal = NULL, codeInsee = NULL), exp3)
  expect_identical(adresseToCoord(adresses = "177", nbEchos = 1, codePostal = NULL, codeInsee = NULL), exp4)
  #expect_identical(adresseToCoord(adresses = adr2, nbEchos = 1, codePostal = c("13010","13006"), codeInsee = NULL), exp5)
  #Neutralisation du test suivant pour cause de résultat aléatoire de la BAN. Passe une fois sur deux en local comme en ligne.
  #expect_identical(adresseToCoord(adresses = adr2, nbEchos = 1, codePostal = NULL, codeInsee = c("13210","13206")), exp5)
  #expect_identical(adresseToCoord(adresses = adr2, nbEchos = 1, codePostal = c("13010","13006"), codeInsee = c("13210","13206")), exp5)
  res <- adresseToCoord(adresses = adr3, nbEchos = 3, codePostal = NULL, codeInsee = NULL)
  expect_identical(res, exp6)
  expect_identical(adresseToCoord(adresses = adr4, nbEchos = 1, codePostal = NULL, codeInsee = NULL), exp7)
  expect_identical(adresseToCoord(adresses = adr5, nbEchos = 1, codePostal = NULL, codeInsee = NULL), exp8)

  ##########################################
  # TEST 3 : Exception : valeur non string #
  ##########################################

  expect_error(adresseToCoord(adresses = 17, nbEchos = 1, codePostal = NULL, codeInsee = NULL))
  expect_error(adresseToCoord(adresses = "17", nbEchos = 1, codePostal = 13010, codeInsee = NULL))
  expect_error(adresseToCoord(adresses = "17", nbEchos = 1, codePostal = NULL, codeInsee = 13206))
  expect_error(adresseToCoord(adresses = 17, nbEchos = 0, codePostal = NULL, codeInsee = NULL))
  expect_error(adresseToCoord(adresses = 17, nbEchos = NULL, codePostal = NULL, codeInsee = NULL))
})
