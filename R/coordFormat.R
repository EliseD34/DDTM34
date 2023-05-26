coordFormat <- function (res, src, dst, faceAFace = TRUE)
{
  sources <- data.frame()
  destinations <- data.frame()
  nbSrc <- length(res$sources)
  nbDst <- length(res$destinations)

  if(nbSrc==1)
  {
    aa <- lapply(1:nbSrc, function(x) sources <<- rbind(sources,data.frame(id=rep(src$id[x],nbDst),lon=rep(as.numeric(clean_coord(res$sources[[x]]$location[1])),nbDst),lat=rep(as.numeric(clean_coord(res$sources[[x]]$location[2])),nbDst), stringsAsFactors = FALSE)))
    aa <- lapply(1:nbDst, function(x) destinations <<- rbind(destinations,data.frame(id=dst$id[x],lon=as.numeric(clean_coord(res$destinations[[x]]$location[1])),lat=as.numeric(clean_coord(res$destinations[[x]]$location[2])), stringsAsFactors = FALSE)))
  }else if(nbDst==1)
  {
    aa <- lapply(1:nbSrc, function(x) sources <<- rbind(sources,data.frame(id=src$id[x],lon=as.numeric(clean_coord(res$sources[[x]]$location[1])),lat=as.numeric(clean_coord(res$sources[[x]]$location[2])), stringsAsFactors = FALSE)))
    aa <- lapply(1:nbDst, function(x) destinations <<- rbind(destinations,data.frame(id=rep(dst$id[x],nbSrc),lon=rep(as.numeric(clean_coord(res$destinations[[x]]$location[1])),nbSrc),lat=rep(as.numeric(clean_coord(res$destinations[[x]]$location[2]),nbSrc)), stringsAsFactors = FALSE)))
  }else{
    aa <- lapply(1:nbSrc, function(x) sources <<- rbind(sources,data.frame(id=src$id[x],lon=as.numeric(clean_coord(res$sources[[x]]$location[1])),lat=as.numeric(clean_coord(res$sources[[x]]$location[2])), stringsAsFactors = FALSE)))
    aa <- lapply(1:nbDst, function(x) destinations <<- rbind(destinations,data.frame(id=dst$id[x],lon=as.numeric(clean_coord(res$destinations[[x]]$location[1])),lat=as.numeric(clean_coord(res$destinations[[x]]$location[2])), stringsAsFactors = FALSE)))
  }

  if(!faceAFace)
  {
    sources <- sources[rep(1:nbSrc,nbDst),]
    destinations <- destinations[rep(1:nbDst, each=nbSrc),]
  }

  return(list(sources = sources, destinations = destinations))
}
