osrmTableFaceAFace <- function(src, dst, duree, distance, exclude, interactive)
{
  if(nrow(src)>nrow(dst) & nrow(dst)>1)
  {
    src <- src[1:nrow(dst),]
  }else if(nrow(dst)>nrow(src) & nrow(src)>1)
  {
    dst <- dst[1:nrow(src),]
  }else # nrow(src)==nrow(dst)
  {}

  # on cree une table avec les couples uniques en face a face
  couplesUniques <- unique(cbind(src,dst))
  names(couplesUniques) <- c("idSrc","lonSrc","latSrc","idDst","lonDst","latDst")

  # On cree un identifiant par couple
  couplesUniques$ID <- c(1:nrow(couplesUniques)) # ID : identifiant de couples

  res <- calculs_faceaface(couplesUniques, duree, distance, exclude, interactive)

  return(res)
}
