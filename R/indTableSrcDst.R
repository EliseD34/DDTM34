#' @name indTableSrcDst
#'
#' @title Calculer des indicateurs en volume par source (src) ou par destination (dst)
#'
#' @description La fonction indTableSrcDst permet de calculer par destination (dst) la somme d'une ou plusieurs variables en volume issues des sources (src), la population par exemple.
#'
#' La fonction permet également de calculer la part des variables en volume pour chaque source dans l'ensemble des sources ayant une même destination.
#'
#' @param res data.frame. Résultat de la fonction metricOsrmTable avec nbDstVolOiseau = 1 ou nbDstMeasure = 1.
#'
#' @return liste de deux objets sf.
#'
#' Le 1er élément comporte une source (src) par observation. Les variables en volume sont couplées à une variable indiquant la part de ce volume dans l'ensemble des sources ayant la même destination (en pourcentage).
#'
#' Le 2ème élément comporte une destination (dst) par observation (voir details).
#'
#' @details La table res doit etre calculée à partir de la fonction metricOsrmTable avec les arguments nbDstVolOiseau = 1 ou nbDstMeasure = 1.
#' En effet, il ne faut qu'un seul résultat par source : la destination la plus proche d'une source à vol d'oiseau, en temps ou en distance.
#'
#' L'ordre des colonnes de res en entrée de indTableSrcDst doit être obligatoirement le même que celui obtenu en sortie de la fonction metricOsrmTable.
#'
#' Pour calculer les indicateurs, il faut ajouter des variables en volume à la suite du tableau de résultats (la population par source par exemple).
#'
#' En sortie, le 2ème élément de la liste indique les indicateurs en volume par destination. Il s'agit de la somme de toutes les variables de sources pour chaque destination.
#' Il donne également un comptage du nombre de sources allant vers chaque destination.
#' Si il n'y a pas de variable en volume ajoutée au tableau de résultats, seul l'indicateur de comptage est indiqué.
#'
#' Les deux éléments sont des objets sf. Il est alors possible, par exemple, de réaliser une carte en ronds proportionnels par destination selon un indicateur calculé.
#'
#' @importFrom stats aggregate
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
#' res_ind <- indTableSrcDst(res = res)
#'
indTableSrcDst <- function(res)
{
  measure <- c()
  if(any(names(res) %in% "duree")) measure <- c(measure,"duree")
  if(any(names(res) %in% "distance")) measure <- c(measure,"distance")

  if(!identical(names(res)[1:7], c("ID","idSrc","lonSrc","latSrc","idDst","lonDst","latDst"))) # si les 7 premieres colonnes ne correspondent pas, erreur
  {
    if(identical(names(res)[1:7], c("idSrc","ID","lonSrc","latSrc","idDst","lonDst","latDst")))
    {
      res <- res[,c(2,1,3:ncol(res))] # si merge de la table de resultat, les 2 premieres colonnes seront inversees, dc on les met dans le bon ordre si l'utilisateur ne l'a pas deja fait.
    }else
    {
      stop(simpleError("Les 7 premieres variables de res doivent correspondre aux 7 premieres variables du tableau de resultats de la fonction metricOsrmTable : 'ID','idSrc','lonSrc','latSrc','idDst','lonDst','latDst' suivies d'au moins d'une des deux variables 'duree' et 'distance'."))
    }
  }
  if(is.null(measure)) # si il n'y a pas de colonnes duree et/ou distance, erreur
  {
    stop(simpleError("Les 7 premieres variables de res doivent correspondre aux 7 premieres variables du tableau de resultats de la fonction metricOsrmTable : 'ID','idSrc','lonSrc','latSrc','idDst','lonDst','latDst' suivies d'au moins d'une des deux variables 'duree' et 'distance'."))
  }
  if(length(measure)==1)
  {
    if(!names(res)[8] %in% measure) # si la 8ème colonne n'est ni duree, ni distance, erreur
    {
      stop(simpleError("Les 7 premieres variables de res doivent correspondre aux 7 premieres variables du tableau de resultats de la fonction metricOsrmTable : 'ID','idSrc','lonSrc','latSrc','idDst','lonDst','latDst' suivies d'au moins d'une des deux variables 'duree' et 'distance'."))
    }
  }

  if(any(measure %in% "duree"))
  {
    if(any(res$duree %in% -999999.00))
    {
      nbNonCalc <- length(res$duree[res$duree %in% -999999.00])
      if(nbNonCalc == 1)
      {
        message(paste0("[WARNING] Il y a ",nbNonCalc," couple non calcul\u00e9 dans la table de r\u00e9sultats (duree = -999999.00)."))
        message(paste0("Il a \u00e9t\u00e9 supprim\u00e9 pour le calcul des indicateurs par source et par destination"))
      }
      if(nbNonCalc > 1)
      {
        message(paste0("[WARNING] Il y a ",nbNonCalc," couples non calcul\u00e9s dans la table de r\u00e9sultats (duree = -999999.00)."))
        message(paste0("Ils ont \u00e9t\u00e9 supprim\u00e9s pour le calcul des indicateurs par source et par destination."))
      }
      res <- res[!res$duree %in% -999999.00,]
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
        message(paste0("Il a \u00e9t\u00e9 supprim\u00e9 pour le calcul des indicateurs par source et par destination."))
      }
      if(nbNonCalc > 1)
      {
        message(paste0("[WARNING] Il y a ",nbNonCalc," couples non calcul\u00e9s dans la table de r\u00e9sultats (distance = -999999.00)."))
        message(paste0("Ils ont \u00e9t\u00e9 supprim\u00e9s pour le calcul des indicateurs par source et par destination."))
      }
      res <- res[!res$distance %in% -999999.00,]
    }
  }

  if(length(res$idSrc) != length(unique(res$idSrc)))
  {
    message(paste0("[WARNING] Il existe plusieurs sources identiques alors qu'il n'en faut qu'une seule pour le calcul de l'indicateur."))
    message(paste0("Veuillez filtrer la table de r\u00e9sultats ou r","\u00e9","ex","\u00e9","cutez la fonction metricOsrmTable "))
    message(paste0("avec les arguments nbDstVolOiseau = 1 ou nbDstMeasure = 1."))
    return(NULL)
  }

  sf_src <- unique(convertTo(from = res[,c(2:4)], to = "sf", fromEpsg = 4326))
  sf_src[,names(res)[8:length(names(res))]] <- res[,8:ncol(res)]
  names(sf_src)[1] <- "idSrc"
  names(attr(sf_src, "agr"))[1] <- "idSrc"

  sf_dst <- unique(convertTo(from = res[,c(5:7)], to = "sf", fromEpsg = 4326))
  names(sf_dst)[1] <- "idDst"
  names(attr(sf_dst, "agr"))[1] <- "idDst"

  parSrc <- merge(sf_src, res[,c("idSrc","idDst")], by = "idSrc")
  parSrc <- parSrc[,c(1,ncol(parSrc)-1,2:(ncol(parSrc)-2),ncol(parSrc))]

  parDstNb <- stats::aggregate(parSrc$idDst, by = list(parSrc$idDst), FUN = length)
  names(parDstNb) <- c("idDst","nbSrc")

  if(ncol(parSrc) > 5 | (ncol(parSrc) > 4 & length(measure) == 1))
  {
    parDstSum <- stats::aggregate(parSrc[,(3+length(measure)):(ncol(parSrc)-1)], by = list(parSrc$idDst), FUN = sum)
    names(parDstSum) <- c("idDst",names(parSrc)[(3+length(measure)):(length(names(parSrc))-1)],"geometry")
    parDst <- merge(parDstNb, as.data.frame(parDstSum)[,c(1,2:(ncol(parDstSum)-1))], by = "idDst")
    parDst <- merge(sf_dst, parDst, by = "idDst")
  }else
  {
    parDst <- merge(sf_dst, parDstNb, by = "idDst")
  }

  if(ncol(parDst) > 3)
  {
    var <- names(parDst)[3:(ncol(parDst)-1)]

    parSrc <- merge(parSrc, as.data.frame(parDst)[,c("idDst",var)], by = "idDst")
    names(parSrc) <- c("idDst","idSrc",measure,var,paste0("tot_",var),"geometry")

    parSrc[,paste0("part_",var)] <- round(as.data.frame(parSrc)[,var] / as.data.frame(parSrc)[,paste0("tot_",var)]*100,3)

    parSrc <- parSrc[,c("idSrc","idDst",measure,as.vector(sapply(1:length(var), function(x) c(var[x],paste0("part_",var)[x]))),"geometry")]
    names(attr(parSrc, "agr")) <- names(parSrc)[-length(names(parSrc))]
  }

  return(list(parSrc,parDst))
}
