osrmTableAllerRetour <- function(src, dst, duree, distance, exclude, interactive)
{
  # Traitement des couples aller-retour

  src_comm <- src[src$id %in% dst$id,]
  dst_comm <- dst[dst$id %in% src$id,]

  if(nrow(src_comm) > 1)
  {
    # On calcule les couples AR puis sélection que des allers simples
    res_comm <- osrmTableCartesien(src = src_comm, dst = dst_comm, duree = duree, distance = distance, exclude = exclude, interactive = interactive)

    res_comm <- res_comm[order(res_comm$idSrc),]

    aa <- matrix(nrow = nrow(src_comm), ncol = nrow(src_comm))
    i <- 0
    for(j in 1:(nrow(src_comm)))
    {
      aa[,j] <- c(rep(1,i), rep(0,nrow(src_comm)-i))
      i <- i + 1
    }

    res_comm$id <- c(aa)
    res_comm <- res_comm[res_comm$id %in% 0,-ncol(res_comm)]
  }else
  {
    res_comm <- data.frame()
  }

  # Traitement des couples uniques

  src_diff <- src[!src$id %in% dst$id,]
  dst_diff <- dst[!dst$id %in% src$id,]

  if(nrow(src_diff) > 0 & nrow(dst_diff) > 0)
  {
    res_diff_diff <- osrmTableCartesien(src = src_diff, dst = dst_diff, duree = duree, distance = distance, exclude = exclude, interactive = interactive)
  }else
  {
    res_diff_diff <- data.frame()
  }

  if(nrow(src_diff) > 0 & nrow(dst_comm) > 0)
  {
    res_diff_comm <- osrmTableCartesien(src = src_diff, dst = dst_comm, duree = duree, distance = distance, exclude = exclude, interactive = interactive)
  }else
  {
    res_diff_comm <- data.frame()
  }

  if(nrow(src_comm) > 0 & nrow(dst_diff) > 0)
  {
    res_comm_diff <- osrmTableCartesien(src = src_comm, dst = dst_diff, duree = duree, distance = distance, exclude = exclude, interactive = interactive)
  }else
  {
    res_comm_diff <- data.frame()
  }

  # Concaténation des tables

  res <- rbind(res_comm,res_comm_diff,res_diff_comm,res_diff_diff)

  res$ID <- 1:nrow(res)

  return(res)
}
