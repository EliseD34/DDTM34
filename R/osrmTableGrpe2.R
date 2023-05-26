# Calcul la durée et/ou la distance
#
# La fonction osrmTableGrpe2 permet de calculer la durée et la distance d'un point vers un groupe de points
# ou d'un groupe de points vers un point. Chaque groupe est composé d'au moins 2 points et moins de 1000 points.
#
# @param couples data.frame (7 colonnes "idSrc","lonSrc","latSrc","idDst","lonDst","latDst","ID").
# @param duree booleen. La fonction retourne la durée. Par défaut à TRUE.
# @param distance booleen. La fonction retourne la distance. Par défaut à TRUE.
# @param exclude string. Exclu un type de route pour le calcul du trajet. Par défaut à NULL.
#
# @return data.frame
osrmTableGrpe2 <-
function(couples, duree, distance, exclude, interactive)
{
  names(couples) <- c("idSrc","lonSrc","latSrc","idDst","lonDst","latDst","ID")

  # on détermine laquelle des 2 colonnes on fera les calculs pour id unique
  # en plus de l'ID, il faut aussi les coordonnées pour s'assurer du bon nombre distinct de points
  lengthSrcId <- nrow(unique(couples[,c(1,2,3)]))
  lengthDstId <- nrow(unique(couples[,c(4,5,6)]))

  # on garde la liste la plus petite pour avoir un minimum de requêtes à envoyer
  if(lengthSrcId>=lengthDstId)
  {
    id <- 4
  }else if(lengthSrcId<lengthDstId)
  {
    id <- 1
  }else{}

  # Création de l'index
  count <- as.numeric(table(couples[,id]))
  table_idx <- data.frame(id=unique(sort(couples[,id])),COUNT=count, stringsAsFactors=F)
  table_idx_5_999 <- table_idx[table_idx$COUNT>=5,]

  list_res_1n_n1 <- list()

  if(nrow(table_idx_5_999)>0)
  {
    table_idx_5_999$ID2 <- c(1:nrow(table_idx_5_999))

    # Fusion de base avec table_idx pour récupérer l'idx
    couples_5_999 <- merge(couples,table_idx_5_999,by.x = names(couples)[id], by.y = "id")
    couples_5_999 <- couples_5_999[order(couples_5_999$COUNT),]

    list_id <- split(couples_5_999[,c("ID","idSrc","lonSrc","latSrc","idDst","lonDst",'latDst')],couples_5_999[,names(couples)[id]])

    table_idx_5_999 <- table_idx_5_999[rev(order(table_idx_5_999$COUNT)),]

    list_id <- list_id[table_idx_5_999$ID]

    if(interactive)
    {
      shiny::withProgress(message = "Calculs en cours - 2/3 : ",{
        for(i in 1:length(list_id))
        {
          list_res_1n_n1[[i]] <- calculs_faceaface_groupe2(id, dt_id = list_id[[i]], duree, distance, exclude)

          shiny::incProgress(1/length(list_id))
        }
      })
    }else if(!interactive)
    {
      pb2 <- progress::progress_bar$new(
        format = "Calcul en cours - 2/3 : [:bar] :percent :elapsed",
        total = length(list_id), clear = FALSE, width= 60
      )

      pb2$tick(0)

      for(i in 1:length(list_id))
      {
        list_res_1n_n1[[i]] <- calculs_faceaface_groupe2(id, dt_id = list_id[[i]], duree, distance, exclude)

        pb2$tick()
      }
    }else
    {}
  }

  res_2g <- do.call(rbind,list_res_1n_n1)

  if(is.null(res_2g))
  {
    pb2 <- progress::progress_bar$new(
      format = "Calcul en cours - 2/3 : [:bar] :percent :elapsed",
      total = 2, clear = FALSE, width= 60
    )

    for (i in 1:2) {
      pb2$tick()
      Sys.sleep(0.2)
    }
  }

  return(res_2g)
}
