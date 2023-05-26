calculs_faceaface_groupe1 <- function(cptSrc, cptDst, couples, duree, distance, exclude)
{
  if(cptSrc[which.max(cptSrc)]>=cptDst[which.max(cptDst)])
  {
    id <- names(cptSrc[which.max(cptSrc)])
    couplesMax <- couples[couples[,1] %in% id,]
    src_1n_n1 <- couplesMax[1,c(1:3)]
    dst_1n_n1 <- couplesMax[,c(4:6)]
    ID_1n_n1 <- data.frame(ID=couplesMax$ID)
  }else
  {
    id <- names(cptDst[which.max(cptDst)])
    couplesMax <- couples[couples[,4] %in% id,]
    src_1n_n1 <- couplesMax[,c(1:3)]
    dst_1n_n1 <- couplesMax[1,c(4:6)]
    ID_1n_n1 <- data.frame(ID=couplesMax$ID)
  }

  res_1n_n1 <- osrmTable_1n_n1(src = src_1n_n1, dst = dst_1n_n1, duree = duree, distance = distance, exclude = exclude)
  res_1n_n1 <- cbind(ID_1n_n1,res_1n_n1)

  return(res_1n_n1)
}
