# Calcul la durée et/ou la distance en face à face ou par croisement
# La fonction osrmTable_11_nm permet de calculer la durée et la distance entre couples de points
# en face à face ou par croisement.
#
# @param src data.frame (3 colonnes id/lon/lat).
# @param dst data.frame (3 colonnes id/lon/lat).
# @param duree booleen. La fonction retourne la durée. Par défaut à TRUE.
# @param distance booleen. La fonction retourne la distance. Par défaut à TRUE.
# @param exclude string. Exclu un type de route pour le calcul du trajet. Par défaut à NULL.
# @param faceAFace boolean. Si TRUE (par défaut), les couples de points sont pris en face à face
# entre src et dst. Les points en trop, si src et dst n’ont pas la même dimension, sont ignorés.
# Si FALSE, les couples sont formés à partir du croisement en produit cartésien entre src et dst.
# 10000 couples max sont acceptés (nombre de points de src x nombre de points de dst < 10000),
# au-delà le traitement se fera en face à face.
#
# @return data.frame
osrmTable_11_nm <- function(src, dst, duree, distance, exclude, faceAFace = TRUE)
{
  list_res_duree <- list()
  list_res_distance <- list()
  res_destinations <- data.frame()
  res_sources <- data.frame()
  measure <- NULL
  if(duree) measure <- "duration"
  if(distance) measure <- c(measure,"distance")
  if(is.null(measure)) measure <- "duration"
  if(is.null(exclude))
  {
    exclude_str <- ""
  }else{
    exclude <- paste(exclude, sep = "", collapse = ",")
    exclude_str <- paste("&exclude=", exclude, sep = "")
  }

  if(!faceAFace) # produit cartesien
  {
    idx_src <- 1:nrow(src)
    idx_dst <- 1:nrow(dst)
  }else # couple a couple
  {
    if(nrow(src)<=nrow(dst)) # on cale sur src
    {
      idx_src <- 1:nrow(src)
      idx_dst <- 1:nrow(src)
    }else if(nrow(src)>nrow(dst))
    {
      idx_src <- 1:nrow(dst) # on cale sur dst
      idx_dst <- 1:nrow(dst)
    }
  }

  res <- requeteOsrm_11_nm(src = src, idx_src = idx_src, dst = dst, idx_dst = idx_dst, measure = measure, exclude_str = exclude_str)

  if(duree)
  {
    nc <- FALSE
    for(i in 1:length(res$durations))
    {
      for(j in 1:length(res$durations[[i]]))
      {
        if(!is.null(res$durations[[i]][[j]]))
        {
          if(res$durations[[i]][[j]] < 0)
          {
            res$durations[[i]][[j]] <- abs(res$durations[[i]][[j]])
          }
        }else
        {
          res$durations[[i]][[j]] <- -60
          nc <- TRUE
        }
      }
    }

    if(nc) res$durations <- lapply(res$durations, function(x)
                                  {
                                    if(is.list(x))
                                    {
                                      do.call(c,x)
                                    }else
                                    {
                                      x
                                    }
                                  })

    res_duree <- data.frame(res$durations)
    dimnames(res_duree) <- list(1:length(idx_dst),1:length(idx_src))
  }

  if(distance)
  {
    nc <- FALSE
    for(i in 1:length(res$distances))
    {
      for(j in 1:length(res$distances[[i]]))
      {
        if(!is.null(res$distances[[i]][[j]]))
        {
          if(res$distances[[i]][[j]] < 0)
          {
            res$distances[[i]][[j]] <- abs(res$distances[[i]][[j]])
          }
        }else
        {
          res$distances[[i]][[j]] <- -1000
          nc <- TRUE
        }
      }
    }

    if(nc) res$distances <- lapply(res$distances, function(x)
                                  {
                                    if(is.list(x))
                                    {
                                      do.call(c,x)
                                    }else
                                    {
                                      x
                                    }
                                  })

    res_distance <- data.frame(res$distances)
    dimnames(res_distance) <- list(1:length(idx_dst),1:length(idx_src))
  }

  coords <- coordFormat(res = res, src = src[1:length(idx_src),], dst = dst[1:length(idx_dst),], faceAFace = faceAFace)
  res_sources <- rbind(res_sources,coords$sources)
  res_destinations <- rbind(res_destinations,coords$destinations)

  names(res_sources) <- c("idSrc","lonSrc","latSrc")
  names(res_destinations) <- c("idDst","lonDst","latDst")

  if(duree)
  {
    if(faceAFace) # couple a couple
    {
      res_duree_save <- res_duree
      res_duree <- NULL
      for(i in 1:nrow(res_duree_save))
      {
        res_duree <- rbind(res_duree,data.frame(duree=res_duree_save[i,i]))
      }
    }
    else if(!faceAFace) # produit cartesien
    {
      res_duree <- data.frame(duree=do.call(c,list(t(res_duree))))
    }else
    {}
  }

  if(distance)
  {
    if(faceAFace) # couple a couple
    {
      res_distance_save <- res_distance
      res_distance <- NULL
      for(i in 1:nrow(res_distance_save))
      {
        res_distance <- rbind(res_distance,data.frame(distance=res_distance_save[i,i]))
      }
    }else if(!faceAFace) # produit cartesien
    {
      res_distance <- data.frame(distance=do.call(c,list(t(res_distance))))
    }else{}
  }

  res_11_nm <- cbind(res_sources,res_destinations)
  if(duree) res_11_nm <- cbind(res_11_nm,res_duree)
  if(distance) res_11_nm <- cbind(res_11_nm,res_distance)

  res_11_nm$idSrc <- as.character(res_11_nm$idSrc)
  res_11_nm$idDst <- as.character(res_11_nm$idDst)

  row.names(res_11_nm) <- c(1:nrow(res_11_nm))

  return(res_11_nm)
}
