verifParamSrcDst <- function(src,dst)
{
  msg_error1<-msg_error2<-msg_error3<-msg_error4<-msg_error5<-msg_error6<-msg_error7<-msg_error8<-msg_error9<-msg_error10 <- NULL
  msg_error11<-msg_error12<-msg_error13<-msg_error14<-msg_error15<-msg_error16 <- NULL

  if(is.vector(src))
  {
    if(length(src)!=3) msg_error1 <- "Le vecteur src doit comporter 3 valeurs (un identifiant puis les coordonnees lon, lat) / "
    if(any(is.na(src)) | !is.numeric(src)) msg_error2 <- "Le vecteur src doit etre numerique et sans valeur manquante / "
  }
  if(is.vector(dst))
  {
    if(length(dst)!=3) msg_error3 <- "Le vecteur dst doit comporter 3 valeurs (un identifiant puis les coordonnees lon, lat) / "
    if(any(is.na(dst)) | !is.numeric(dst)) msg_error4 <- "Le vecteur dst doit etre numerique et sans valeur manquante / "
  }
  if(any(class(src)=="data.frame") & length(class(src))==1)
  {
    if(ncol(src)!=3)
    {
      msg_error5 <- "Le data.frame src doit comporter 3 colonnes (un identifiant puis les coordonnees lon, lat) / "
    }else
    {
      if(any(is.na(src[,2])) | !is.numeric(src[,2])) msg_error6 <- "La colonne lon doit etre numerique et sans valeur manquante / "
      if(any(is.na(src[,3])) | !is.numeric(src[,3])) msg_error7 <- "La colonne lat doit etre numerique et sans valeur manquante / "
    }
  }
  if(any(class(dst)=="data.frame") & length(class(dst))==1)
  {
    if(ncol(dst)!=3)
    {
      msg_error8 <- "Le data.frame dst doit comporter 3 colonnes (un identifiant puis les coordonnees lon, lat) / "
    }else
    {
      if(any(is.na(dst[,2])) | !is.numeric(dst[,2])) msg_error9 <- "La colonne lon doit etre numerique et sans valeur manquante / "
      if(any(is.na(dst[,3])) | !is.numeric(dst[,3])) msg_error10 <- "La colonne lat doit etre numerique et sans valeur manquante / "
    }
  }
  if(testSf(src))
  {
    if(!any(class(sf::st_geometry(src)) %in% c("sfc_POINT","sfc_POLYGON","sfc_MULTIPOLYGON"))) msg_error11 <- "L'objet sf src doit etre un sfc_POINT, sfc_POLYGON ou sfc_MULTIPOLYGON / "
    if(ncol(src)<2) msg_error12 <- "L'objet sf src doit comporter au moins 2 colonnes (un identifiant et la geometry) / "
  }
  if(testSf(dst))
  {
    if(!any(class(sf::st_geometry(dst)) %in% c("sfc_POINT","sfc_POLYGON","sfc_MULTIPOLYGON"))) msg_error13 <- "L'objet sf dst doit etre un sfc_POINT, sfc_POLYGON ou sfc_MULTIPOLYGON / "
    if(ncol(dst)<2) msg_error14 <- "L'objet sf dst doit comporter au moins 2 colonnes (un identifiant et la geometry) / "
  }
  if(methods::is(src, "Spatial"))
  {
    if(!class(src)[[1]] %in% c("SpatialPointsDataFrame","SpatialPolygonsDataFrame")) msg_error15 <- "L'objet sp doit etre un SpatialPointsDataFrame ou un SpatialPolygonsDataFrame / "
  }
  if(methods::is(dst, "Spatial"))
  {
    if(!class(dst)[[1]] %in% c("SpatialPointsDataFrame","SpatialPolygonsDataFrame")) msg_error16 <- "L'objet sp doit etre un SpatialPointsDataFrame ou un SpatialPolygonsDataFrame / "
  }

  if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4),!is.null(msg_error5),!is.null(msg_error6),
         !is.null(msg_error7),!is.null(msg_error8),!is.null(msg_error9),!is.null(msg_error10),!is.null(msg_error11),!is.null(msg_error12),
         !is.null(msg_error13),!is.null(msg_error14),!is.null(msg_error15),!is.null(msg_error16)))
  {
    stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4,msg_error5,msg_error6,msg_error7,msg_error8,
                            msg_error9,msg_error10,msg_error11,msg_error12,msg_error13,msg_error14,msg_error15,msg_error16)))
  }
}
