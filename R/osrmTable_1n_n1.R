# Calcul la durée et/ou la distance
#
# La fonction osrmTable_1n_n1 permet de calculer la durée et la distance d'un point vers un groupe de points
# ou d'un groupe de points vers un point.
#
# @param src data.frame (3 colonnes id/lon/lat).
# @param dst data.frame (3 colonnes id/lon/lat).
# @param duree booleen. La fonction retourne la durée. Par défaut à TRUE.
# @param distance booleen. La fonction retourne la distance. Par défaut à TRUE.
# @param exclude string. Exclu un type de route pour le calcul du trajet. Par défaut à NULL.
#
# @return data.frame
osrmTable_1n_n1 <- function(src, dst, duree, distance, exclude)
{
  nb_row <- 500 # nombre de couples max par requete 1->n ou n->1
  res_duree <- data.frame()
  res_distance <- data.frame()
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
  src_seul <- FALSE
  dst_seul <- FALSE

  if(nrow(src)==1)
  {
    if(nrow(dst)==nb_row)
    {
      nb_boucle <- 0
    }else
    {
      nb_boucle <- nrow(dst)%/%nb_row
    }
    src_seul <- TRUE
  }else if(nrow(dst)==1)
  {
    if(nrow(src)==nb_row)
    {
      nb_boucle <- 0
    }else
    {
      nb_boucle <- nrow(src)%/%nb_row
    }
    dst_seul <- TRUE
  }else{}

  list_res_1n_n1 <- list()
  for(i in 0:nb_boucle)
  {
    if(dst_seul & nrow(src) > 0)
    {
      if(nrow(src)<nb_row)
      {
        idx_src <- 1:nrow(src)
      }else
      {
        idx_src <- 1:nb_row
      }

      idx_dst <- idx_src[length(idx_src)]

      res <- requeteOsrm_n1(src = src, idx_src = idx_src, dst = dst, idx_dst = idx_dst, measure = measure, exclude_str = exclude_str)

    }else if(src_seul & nrow(dst) > 0)
    {
      idx_src <- 0

      if(nrow(dst)<nb_row)
      {
        idx_dst <- 1:nrow(dst)
      }else
      {
        idx_dst <- 1:nb_row
      }

      res <- requeteOsrm_1n(src = src, idx_src = idx_src, dst = dst, idx_dst = idx_dst, measure = measure, exclude_str = exclude_str)

    }else{}

    if(nrow(src) > 0 & nrow(dst) > 0)
    {
      if(duree)
      {
        # Cas des couples non calculés
        if(length(which(sapply(res$durations, function(x) is.null(x[[1]])))) > 0)
        {
          idx_0 <- which(sapply(res$durations, function(x) is.null(x[[1]])))
          for(j in 1:length(idx_0))
          {
            res$durations[[idx_0[j]]] <- -60
          }
        }
        # Cas des valeurs négatives
        if(length(which(sapply(res$durations, function(x) x[[1]] < 0))) > 0)
        {
          idx_0 <- which(sapply(res$durations, function(x) x[[1]] < 0))
          for(j in 1:length(idx_0))
          {
            if(any(res$durations[[idx_0[j]]] != -60)) res$durations[[idx_0[j]]] <- abs(res$durations[[idx_0[j]]])
          }
        }

        aa <- lapply(1:length(res$durations[[1]]), function(x) if(is.null(res$durations[[1]][[x]])) res$durations[[1]][[x]] <<- -60)
        rm(aa)
        res_duree <- data.frame(duree=unlist(res$durations))
      }
      if(distance)
      {
        # Cas des couples non calculés
        if(length(which(sapply(res$distances, function(x) is.null(x[[1]])))) > 0)
        {
          idx_0 <- which(sapply(res$distances, function(x) is.null(x[[1]])))
          for(j in 1:length(idx_0))
          {
            res$distances[[idx_0[j]]] <- -1000
          }
        }
        
        # Cas des valeurs négatives
        if(length(which(sapply(res$distances, function(x) x[[1]] < 0))) > 0)
        {
          idx_0 <- which(sapply(res$distances, function(x) x[[1]] < 0))
          for(j in 1:length(idx_0))
          {
            if(any(res$distances[[idx_0[j]]] != -1000)) res$distances[[idx_0[j]]] <- abs(res$distances[[idx_0[j]]])
          }
        }

        aa <- lapply(1:length(res$distances[[1]]), function(x) if(is.null(res$distances[[1]][[x]])) res$distances[[1]][[x]] <<- -1000)
        rm(aa)
        res_distance <- data.frame(distance=unlist(res$distances))
      }
      coords <- coordFormat(res = res, src = src[1:length(idx_src),], dst = dst[1:length(idx_dst),])
      res_sources <- coords$sources
      res_destinations <- coords$destinations

      names(res_sources) <- c("idSrc","lonSrc","latSrc")
      names(res_destinations) <- c("idDst","lonDst","latDst")

      res_1n_n1 <- cbind(res_sources,res_destinations)
      if(duree) res_1n_n1 <- cbind(res_1n_n1,res_duree)
      if(distance) res_1n_n1 <- cbind(res_1n_n1,res_distance)

      list_res_1n_n1[[i+1]] <- res_1n_n1

      if(dst_seul)
      {
        src <- src[-idx_src,]
      }else if(src_seul)
      {
        dst <- dst[-idx_dst,]
      }
    }
  }

  res_1n_n1 <- do.call(rbind,list_res_1n_n1)
  res_1n_n1$idSrc <- as.character(res_1n_n1$idSrc)
  res_1n_n1$idDst <- as.character(res_1n_n1$idDst)

  row.names(res_1n_n1) <- c(1:nrow(res_1n_n1))

  return(res_1n_n1)
}
