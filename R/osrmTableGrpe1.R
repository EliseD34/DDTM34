# Calcul la durée et/ou la distance
#
# La fonction osrmTableGrpe1 permet de calculer la durée et la distance d'un point vers un groupe de points
# ou d'un groupe de points vers un point. Chaque groupe est composé d'au moins 1000 points
#
# @param couples data.frame (7 colonnes "idSrc","lonSrc","latSrc","idDst","lonDst","latDst","ID").
# @param duree booleen. La fonction retourne la durée. Par défaut à TRUE.
# @param distance booleen. La fonction retourne la distance. Par défaut à TRUE.
# @param exclude string. Exclu un type de route pour le calcul du trajet. Par défaut à NULL.
#
# @return data.frame
osrmTableGrpe1 <-
function(couples, duree, distance, exclude, interactive)
{
  list_res_1n_n1 <- list()
  cpt <- 0

  couples_save <- couples

  cptSrc <- table(couples[,1])
  cptDst <- table(couples[,4])

  while(any(cptSrc[which.max(cptSrc)]>=1000 | cptDst[which.max(cptDst)]>=1000))
  {
    if(cptSrc[which.max(cptSrc)]>=cptDst[which.max(cptDst)])
    {
      id <- names(cptSrc[which.max(cptSrc)])
      couplesMax <- couples[couples[,1] %in% id,]
    }else
    {
      id <- names(cptDst[which.max(cptDst)])
      couplesMax <- couples[couples[,4] %in% id,]
    }

    couples <- couples[-which(couples$ID %in% couplesMax$ID),]

    cptSrc <- table(couples[,1])
    cptDst <- table(couples[,4])

    cpt <- cpt + 1
  }

  cpt <- cpt - 1

  if(cpt > 0)
  {
    couples <- couples_save

    cptSrc <- table(couples[,1])
    cptDst <- table(couples[,4])

    if(interactive)
    {
      shiny::withProgress(message = "Calculs en cours - 1/3 : ",{
        for(i in 1:cpt)
        {
          list_res_1n_n1[[i]] <- calculs_faceaface_groupe1(cptSrc, cptDst, couples, duree, distance, exclude)

          couples <- couples[-which(couples$ID %in% list_res_1n_n1[[i]][,1]),]

          cptSrc <- table(couples[,1])
          cptDst <- table(couples[,4])

          shiny::incProgress(1/cpt)
        }
      })
    }else if(!interactive)
    {
      pb1 <- progress::progress_bar$new(
        format = "Calcul en cours - 1/3 : [:bar] :percent :elapsed",
        total = cpt, clear = FALSE, width= 60
      )

      pb1$tick(0)

      for(i in 1:cpt)
      {
        list_res_1n_n1[[i]] <- calculs_faceaface_groupe1(cptSrc, cptDst, couples, duree, distance, exclude)

        couples <- couples[-which(couples$ID %in% list_res_1n_n1[[i]][,1]),]

        cptSrc <- table(couples[,1])
        cptDst <- table(couples[,4])

        pb1$tick()
      }
    }else
    {
      pb1 <- progress::progress_bar$new(
        format = "Calcul en cours - 1/3 : [:bar] :percent :elapsed",
        total = 2, clear = FALSE, width= 60
      )

      for (i in 1:2) {
        pb1$tick()
        Sys.sleep(0.2)
      }

      res_1g <- NULL
    }

    res_1g <- do.call(rbind,list_res_1n_n1)

  }else
  {
    pb1 <- progress::progress_bar$new(
      format = "Calcul en cours - 1/3 : [:bar] :percent :elapsed",
      total = 2, clear = FALSE, width= 60
    )

    for (i in 1:2) {
      pb1$tick()
      Sys.sleep(0.2)
    }

    res_1g <- NULL
  }

  return(res_1g)
}
