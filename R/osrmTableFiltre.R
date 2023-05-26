osrmTableFiltre <- function(src, dst, duree, distance, exclude, rayonMax, nbDstVolOiseau, nbDstMeasure, optiMeasure, code_epsg, interactive)
{
  src <- unique(src)
  dst <- unique(dst)

  if(any(duplicated(as.data.frame(dst)[,1])))
  {
    message(paste0("[WARNING] Attention, il y a des doublons dans les identifiants des destinations. Ils devraient \u00eatre unique si rayonMax ou nbDstVolOiseau > 0."))
  }

  rayonMax <- rayonMax*1000

  coord_src <- convertTo(from = src, fromEpsg = 4326, toEpsg = code_epsg)

  coord_dst <- convertTo(from = dst, fromEpsg = 4326, toEpsg = code_epsg)

  mat <- NULL

  if(rayonMax > 0 & nbDstVolOiseau > 0) # n dst par src les plus proches dans un rayon de x km max
  {
    if(nbDstVolOiseau > nrow(dst)) nbDstVolOiseau <- nrow(dst)
    res <- tryCatch({
            mat <- RANN::nn2(coord_dst[,-1], coord_src[,-1], k = nbDstVolOiseau, searchtype = 'radius', radius = rayonMax)
    },error = function(err){
      message(paste0("Aucun point n'a \u00e9t\u00e9 trouv\u00e9 \u00e0 moins de ",rayonMax/1000," km \u00e0 vol d'oiseau"))
    })
  }else if(rayonMax == 0 & nbDstVolOiseau > 0) # n dst par src les plus proches dans l'ensemble des src
  {
    if(nbDstVolOiseau > nrow(dst)) nbDstVolOiseau <- nrow(dst)
    res <- tryCatch({
            mat <- RANN::nn2(coord_dst[,-1], coord_src[,-1], k = nbDstVolOiseau)
    },error = function(err){
      message(paste0("Aucun point n'a \u00e9t\u00e9 trouv\u00e9"))
    })
  }else if(rayonMax > 0 & nbDstVolOiseau == 0) # tous les dst par src dans un rayon de x km max
  {
    res <- tryCatch({
            mat <- RANN::nn2(coord_dst[,-1], coord_src[,-1], k = nrow(dst), searchtype = 'radius', radius = rayonMax)
    },error = function(err){
      message(paste0("Aucun point n'a \u00e9t\u00e9 trouv\u00e9 \u00e0 moins de ",rayonMax/1000," km \u00e0 vol d'oiseau"))
    })
  }else
  {
    # tous les dst dans l'ensemble des src
    # rayonMax == 0 & nbPlusProche == 0 --> fct osrmTableCartesien
    stop(simpleError("Veuillez svp utiliser la fonction osrmTableCartesien."))
  }

  if(!is.null(mat))
  {
    idx <- lapply(1:ncol(mat$nn.idx), function(x) {
                  dt <- data.frame(idx_src = 1:nrow(mat$nn.idx),
                                   idx_dst = mat$nn.idx[,x],
                                   stringsAsFactors = FALSE)
                  if(any(which(dt$idx_dst == 0)) > 0) dt <- dt[-which(dt$idx_dst == 0),]
                  return(dt)
            })

    idx <- do.call(rbind, idx)
    idx <- idx[order(idx$idx_src),]

    if(nrow(idx) > 0)
    {
      src <- src[idx$idx_src,]
      dst <- dst[idx$idx_dst,]

      res <- osrmTableFaceAFace(src = src, dst = dst, duree = duree, distance = distance, exclude = exclude, interactive = interactive)
      # on met les -999999 (en realite -60 et -1000 issus de osrmTableFaceAFace) en 999999 
      # pour eviter qu ils ne se retrouvent en tete lors du classement de list_dtIdSrcMin
      res$duree[res$duree == -60.0]  <- 999999
      res$distance[res$distance == -1000.0] <- 999999
      if(nbDstMeasure > 0) # on filtre apres les calculs pour garder les n dst par src les plus proches en temps ou en distance
      {
        list_dtIdSrc <- split(res,res$idSrc)

        if(optiMeasure == "duree")
        {
          list_dtIdSrcMin <- lapply(1:length(list_dtIdSrc), function (x) list_dtIdSrc[[x]][order(list_dtIdSrc[[x]]$duree)[1:nbDstMeasure],])
        }else if(optiMeasure == "distance")
        {
          list_dtIdSrcMin <- lapply(1:length(list_dtIdSrc), function (x) list_dtIdSrc[[x]][order(list_dtIdSrc[[x]]$distance)[1:nbDstMeasure],])
        }else
        {
          stop(simpleError("optiMeasure doit etre 'duree' ou 'distance'."))
        }
        
        res <- do.call(rbind,list_dtIdSrcMin)
        # on remet les -999999 en -60 et -1000
        res$duree[res$duree == 999999]  <- -60.0
        res$distance[res$distance == 999999] <- -1000.0
      }
 
      if(any(is.na(res))) res <- res[!is.na(res$ID),]

      res$ID <- c(1:nrow(res))
      row.names(res) <- res$ID
    }else
    {
      res <- NULL
    }
  }else
  {
    res <- NULL
  }

  return(res)
}
