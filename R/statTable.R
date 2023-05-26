#' @name statTable
#'
#' @title Calculer des statistiques sur les resultats de la fonction metricOsrmTable.
#'
#' @description La fonction statTable permet de calculer des statistiques simples sur les résultats de la fonction metricOsrmTable (comptages, valeurs min, valeurs max, moyennes, médianes).
#'
#' @param res data.frame. Résultat de la fonction metricOsrmTable avec les colonnes duree et/ou distance.
#'
#' @return Une liste de valeurs statistiques
#'
#' @details La table res doit correspondre à la table de résultats de la fonction metricOsrmTable avec les colonnes suivantes :
#' "ID","idSrc","lonSrc","latSrc","idDst","lonDst","latDst", "duree" et/ou "distance".
#'
#' Les statistiques calculées sont :
#'
#' - le nombre de couples calculés ;
#'
#' - le nombre de sources (src) ;
#'
#' - le nombre de destinations (dst).
#'
#' Pour la duree et la distance :
#'
#' - la valeur min ;
#'
#' - la valeur max ;
#'
#' - la moyenne ;
#'
#' - la médiane.
#'
#' @export
#'
#' @examples
#' # Reproduction d'une table de resultats avec la fonction metricOsrmTable
#' res <- data.frame(ID = c(1:4),
#'                   idSrc = c("1","2","3","4"),
#'                   lonSrc = c(5.39200,5.39242,5.37107,5.46476),
#'                   latSrc = c(43.28292,43.28368,43.47900,43.31246),
#'                   idDst = c("B","B","A","C"),
#'                   lonDst = c(5.38385,5.38385,5.47678,5.38219),
#'                   latDst = c(43.28571,43.28571,43.29028,43.44144),
#'                   duree = c(87.3,62.8,726.8,239.7),
#'                   distance = c(1103.3,942.7,8318.6,3252.6),
#'                   pop = c(1204,806,1164,976),
#'                   stringsAsFactors = FALSE)
#'
#' res_ind <- statTable(res = res)
#'
statTable <- function(res)
{
  measure <- c()
  if(any(names(res) %in% "duree")) measure <- c(measure,"duree")
  if(any(names(res) %in% "distance")) measure <- c(measure,"distance")

  if(!identical(names(res)[1:7], c("ID","idSrc","lonSrc","latSrc","idDst","lonDst","latDst"))) # si les 7 premieres colonnes ne correspondent pas, erreur
  {
    stop(simpleError("Les 7 premieres variables de res doivent correspondre aux 7 premieres variables du tableau de resultats de la fonction metricOsrmTable : 'ID','idSrc','lonSrc','latSrc','idDst','lonDst','latDst' suivies d'au moins d'une des deux variables 'duree' et 'distance'."))
  }
  if(is.null(measure)) # si il n'y a pas de colonnes duree et/ou distance, erreur
  {
    stop(simpleError("Les 7 premieres variables de res doivent correspondre aux 7 premieres variables du tableau de resultats de la fonction metricOsrmTable : 'ID','idSrc','lonSrc','latSrc','idDst','lonDst','latDst' suivies d'au moins d'une des deux variables 'duree' et 'distance'."))
  }
  if(!names(res)[8] %in% measure) # si la 8ème colonne n'est ni duree, ni distance, erreur
  {
    stop(simpleError("Les 7 premieres variables de res doivent correspondre aux 7 premieres variables du tableau de resultats de la fonction metricOsrmTable : 'ID','idSrc','lonSrc','latSrc','idDst','lonDst','latDst' suivies d'au moins d'une des deux variables 'duree' et 'distance'."))
  }

  if(any(measure %in% "duree") & any(measure %in% "distance"))
  {
    if(any(res$duree %in% -999999.00))
    {
      nbNonCalc <- length(res$duree[res$duree %in% -999999.00])
      if(nbNonCalc == 1) message(paste0("[WARNING] Il y a ",nbNonCalc," couple non calcul\u00e9 dans la table de r\u00e9sultats (duree = -999999.00). Il a \u00e9t\u00e9 supprim\u00e9 pour le calcul des indicateurs stats."))
      if(nbNonCalc > 1) message(paste0("[WARNING] Il y a ",nbNonCalc," couples non calcul\u00e9s dans la table de r\u00e9sultats (duree = -999999.00). Ils ont \u00e9t\u00e9 supprim\u00e9s pour le calcul des indicateurs stats."))

      res <- res[!res$duree %in% -999999.00,]
    }

    if(any(res$duree %in% 0) & any(res$distance %in% 0))
    {
      nbStables <- length(res$duree[res$duree %in% 0])
      if(nbStables == 1)
      {
        message(paste0("[INFO] Il y a ",nbStables," couple de stable dans la table de r\u00e9sultats (duree = 0)."))
        message(paste0("Il a \u00e9t\u00e9 pris en compte pour le calcul des indicateurs stats."))
      }
      if(nbStables > 1)
      {
        message(paste0("[INFO] Il y a ",nbStables," couples de stables dans la table de r\u00e9sultats (duree = 0)."))
        message(paste0("Ils ont \u00e9t\u00e9 pris en compte pour le calcul des indicateurs stats."))
      }
    }else
    {
      nbStables <- 0
    }
  }

  if(any(measure %in% "duree") & !any(measure %in% "distance"))
  {
    if(any(res$duree %in% -999999.00))
    {
      nbNonCalc <- length(res$duree[res$duree %in% -999999.00])
      if(nbNonCalc == 1) message(paste0("[WARNING] Il y a ",nbNonCalc," couple non calcul\u00e9 dans la table de r\u00e9sultats (duree = -999999.00). Il a \u00e9t\u00e9 supprim\u00e9 pour le calcul des indicateurs stats."))
      if(nbNonCalc > 1) message(paste0("[WARNING] Il y a ",nbNonCalc," couples non calcul\u00e9s dans la table de r\u00e9sultats (duree = -999999.00). Ils ont \u00e9t\u00e9 supprim\u00e9s pour le calcul des indicateurs stats."))

      res <- res[!res$duree %in% -999999.00,]
    }

    if(any(res$duree %in% 0))
    {
      nbStables <- length(res$duree[res$duree %in% 0])
      if(nbStables == 1)
      {
        message(paste0("[INFO] Il y a ",nbStables," couple de stable dans la table de r\u00e9sultats (duree = 0)."))
        message(paste0("Il a \u00e9t\u00e9 pris en compte pour le calcul des indicateurs stats."))
      }
      if(nbStables > 1)
      {
        message(paste0("[INFO] Il y a ",nbStables," couples de stables dans la table de r\u00e9sultats (duree = 0)."))
        message(paste0("Ils ont \u00e9t\u00e9 pris en compte pour le calcul des indicateurs stats."))
      }
    }else
    {
      nbStables <- 0
    }
  }

  if(any(measure %in% "distance") & !any(measure %in% "duree"))
  {
    if(any(res$distance %in% -999999.00))
    {
      nbNonCalc <- length(res$distance[res$distance %in% -999999.00])
      if(nbNonCalc == 1)
      {
        message(paste0("[WARNING] Il y a ",nbNonCalc," couple non calcul\u00e9 dans la table de r\u00e9sultats (distance = -999999.00)."))
        message(paste0("Il a \u00e9t\u00e9 supprim\u00e9 pour le calcul des indicateurs stats."))
      }
      if(nbNonCalc > 1)
      {
        message(paste0("[WARNING] Il y a ",nbNonCalc," couples non calcul\u00e9s dans la table de r\u00e9sultats (distance = -999999.00)."))
        message(paste0("Ils ont \u00e9t\u00e9 supprim\u00e9s pour le calcul des indicateurs stats."))
      }

      res <- res[!res$distance %in% -999999.00,]
    }

    if(any(res$distance %in% 0))
    {
      nbStables <- length(res$distance[res$distance %in% 0])
      if(nbStables == 1)
      {
        message(paste0("[INFO] Il y a ",nbStables," couple de stable dans la table de r\u00e9sultats (distance = 0)."))
        message(paste0("Il a \u00e9t\u00e9 pris en compte pour le calcul des indicateurs stats."))
      }
      if(nbStables > 1)
      {
        message(paste0("[INFO] Il y a ",nbStables," couples de stables dans la table de r\u00e9sultats (distance = 0)."))
        message(paste0("Ils ont \u00e9t\u00e9 pris en compte pour le calcul des indicateurs stats."))
      }
    }else
    {
      nbStables <- 0
    }
  }

  stats <- list()

  stats$nbCouples <- nrow(res)
  if(nbStables > 0) stats$nbStables <- nbStables
  stats$nbIdDep <- length(unique(as.data.frame(res)[,"idSrc"]))
  stats$nbIdArr <- length(unique(as.data.frame(res)[,"idDst"]))

  if(any(measure %in% "duree"))
  {
    if(nrow(res) > 0)
    {
      stats$tempsMax <- max(as.data.frame(res)[,"duree"], na.rm = TRUE)
      stats$tempsMin <- min(as.data.frame(res)[,"duree"], na.rm = TRUE)
      stats$tempsMoyenne <- round(mean(as.data.frame(res)[,"duree"], na.rm = TRUE),2)
      stats$tempsMediane <- round(stats::median(as.data.frame(res)[,"duree"], na.rm = TRUE),2)
    }else
    {
      stats$tempsMax <- "--"
      stats$tempsMin <- "--"
      stats$tempsMoyenne <- "--"
      stats$tempsMediane <- "--"
    }
  }

  if(any(measure %in% "distance"))
  {
    if(nrow(res) > 0)
    {
      stats$distanceMax <- max(as.data.frame(res)[,"distance"], na.rm = TRUE)
      stats$distanceMin <- min(as.data.frame(res)[,"distance"], na.rm = TRUE)
      stats$distanceMoyenne <- round(mean(as.data.frame(res)[,"distance"], na.rm = TRUE),3)
      stats$distanceMediane <- round(stats::median(as.data.frame(res)[,"distance"], na.rm = TRUE),3)
    }else
    {
      stats$distanceMax <- "--"
      stats$distanceMin <- "--"
      stats$distanceMoyenne <- "--"
      stats$distanceMediane <- "--"
    }
  }

  return(stats)
}
