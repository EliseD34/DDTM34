calculs_faceaface <- function(couplesUniques, duree, distance, exclude, interactive)
{
  ### 1er groupe : groupes >= 1000 couples (1n ou n1) : calcul de 1 à n (n = 500 max)
  res_1g <- osrmTableGrpe1(couples = couplesUniques, duree = duree, distance = distance, exclude = exclude, interactive = interactive)

  if(!is.null(res_1g))
  {
    couplesUniques <- couplesUniques[-which(couplesUniques$ID %in% res_1g$ID),]
  }else
  {
    res_1g <- NULL
  }

  ### 2eme groupe : groupes entre 5 et 999 couples (1n ou n1) : calcul de 1 à n (n = 500 max)
  if(nrow(couplesUniques)>0)
  {
    res_2g <- osrmTableGrpe2(couples = couplesUniques, duree = duree, distance = distance, exclude = exclude, interactive = interactive)
    if(!is.null(res_2g))
    {
      couplesUniques <- couplesUniques[-which(couplesUniques$ID %in% res_2g$ID),]
    }
  }else
  {
    res_2g <- NULL
  }

  ### 3eme groupe : groupes de 1 à 4 couple(s) en face a face (11) : calcul par croisement de n x n (n=100 max) puis conservation de la diagonale
  if(nrow(couplesUniques)>0)
  {
    nb_boucles <- (nrow(couplesUniques)%/%100)+1
    list_res_3g <- list()

    if(interactive)
    {
      shiny::withProgress(message = "Calculs en cours - 3/3 : ",{
        for(i in 1:nb_boucles)
        {
          if(nrow(couplesUniques)>=100)
          {
            nb_row <- 100
          }else
          {
            nb_row <- nrow(couplesUniques)%%100
          }
          if(nb_row > 0)
          {
            list_res_3g[[i]] <- osrmTableGrpe3(couples = couplesUniques[1:nb_row,], duree = duree, distance = distance, exclude = exclude)
            couplesUniques <- couplesUniques[-(1:nb_row),]
          }
          shiny::incProgress(1/nb_boucles)
        }
      })
    }else if(!interactive)
    {
      pb3 <- progress::progress_bar$new(
        format = "Calcul en cours - 3/3 : [:bar] :percent :elapsed",
        total = nb_boucles, clear = FALSE, width= 60
      )

      pb3$tick(0)

      for(i in 1:nb_boucles)
      {
        if(nrow(couplesUniques)>=100)
        {
          nb_row <- 100
        }else
        {
          nb_row <- nrow(couplesUniques)%%100
        }
        if(nb_row > 0)
        {
          list_res_3g[[i]] <- osrmTableGrpe3(couples = couplesUniques[1:nb_row,], duree = duree, distance = distance, exclude = exclude)
          couplesUniques <- couplesUniques[-(1:nb_row),]
        }
        pb3$tick()
      }
    }

    res_3g <- do.call(rbind,list_res_3g)
  }else
  {
    pb3 <- progress::progress_bar$new(
      format = "Calcul en cours - 3/3 : [:bar] :percent :elapsed",
      total = 2, clear = FALSE, width= 60
    )

    for (i in 1:2) {
      pb3$tick()
      Sys.sleep(0.2)
    }

    res_3g <- NULL
  }

  # Fusion des 3 tables
  res <- rbind(res_1g,res_2g)
  res <- rbind(res,res_3g)
  res <- res[order(res$ID),]
  row.names(res) <- c(1:nrow(res))

  return(res)
}
