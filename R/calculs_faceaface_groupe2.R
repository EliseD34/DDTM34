calculs_faceaface_groupe2 <- function(id, dt_id, duree, distance, exclude){

  if(id==4)
  {
    src_1n_n1 <- dt_id[,c("idSrc","lonSrc","latSrc")]
    dst_1n_n1 <- dt_id[1,c("idDst","lonDst","latDst")]
  }else if(id==1)
  {
    src_1n_n1 <- dt_id[1,c("idSrc","lonSrc","latSrc")]
    dst_1n_n1 <- dt_id[,c("idDst","lonDst","latDst")]
  }else
  {
    src_1n_n1 <- NULL
    dst_1n_n1 <- NULL
  }

  res_1n_n1 <- osrmTable_1n_n1(src = src_1n_n1, dst = dst_1n_n1, duree = duree, distance = distance, exclude = exclude)
  res_1n_n1 <- cbind(data.frame(ID=dt_id$ID),res_1n_n1)

  return(res_1n_n1)
}
