# Calcul la durée et/ou la distance
#
# La fonction osrmTableGrpe3 permet de calculer la durée et la distance entre deux points.
#
# @param couples data.frame (7 colonnes "idSrc","lonSrc","latSrc","idDst","lonDst","latDst","ID").
# @param duree booleen. La fonction retourne la durée. Par défaut à TRUE.
# @param distance booleen. La fonction retourne la distance. Par défaut à TRUE.
# @param exclude string. Exclu un type de route pour le calcul du trajet. Par défaut à NULL.
#
# @return data.frame
osrmTableGrpe3 <-
function(couples, duree, distance, exclude)
{
  ID_11_nm <- data.frame(ID=couples$ID)
  src_11_nm <- couples[,c("idSrc","lonSrc","latSrc")]
  dst_11_nm <- couples[,c("idDst","lonDst","latDst")]

  res_11_nm <- osrmTable_11_nm(src = src_11_nm, dst = dst_11_nm, duree = duree, distance = distance, exclude = exclude, faceAFace = TRUE)
  res_3g <- cbind(ID_11_nm,res_11_nm)

  return(res_3g)
}
