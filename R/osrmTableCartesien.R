osrmTableCartesien <- function(src, dst, duree, distance, exclude, interactive)
{
  nb_boucles_src <- nrow(src)%/%100
  nb_boucles_dst <- nrow(dst)%/%100
  reste_src <- nrow(src)%%100
  reste_dst <- nrow(dst)%%100

  if(nrow(src) < 100)
  {
    nb_src <- nrow(src)
  }else
  {
    nb_src <- 100
  }

  if(nrow(dst) < 100)
  {
    nb_dst <- nrow(dst)
  }else
  {
    nb_dst <- 100
  }

  if(interactive)
  {
    shiny::withProgress(message = "Patientez le temps des calculs",{

      list_res <- list()
      k <- 1
      i <- 0
      if(nb_boucles_src > 0)
      {
        for(i in 1:nb_boucles_src)
        {
          j <- 0
          if(nb_boucles_dst > 0)
          {
            for(j in 1:nb_boucles_dst)
            {
              shiny::incProgress(1/(nb_boucles_src*nb_boucles_dst))

              list_res[[k]] <- osrmTable_11_nm(src[((i-1)*nb_src+1):(i*nb_src),],
                                               dst[((j-1)*nb_dst+1):(j*nb_dst),],
                                               duree,
                                               distance,
                                               exclude,
                                               faceAFace = FALSE)

              k <- k + 1
            }
          }
          if(reste_dst > 0)
          {
            list_res[[k]] <- osrmTable_11_nm(src[((i-1)*nb_src+1):(i*nb_src),],
                                             dst[(j*nb_dst+1):(j*nb_dst+reste_dst),],
                                             duree,
                                             distance,
                                             exclude,
                                             faceAFace = FALSE)

            k <- k + 1
          }
        }
      }
      if(reste_src > 0)
      {
        j <- 0
        if(nb_boucles_dst > 0)
        {
          for(j in 1:nb_boucles_dst)
          {
            list_res[[k]] <- osrmTable_11_nm(src[(i*nb_src+1):(i*nb_src+reste_src),],
                                             dst[((j-1)*nb_dst+1):(j*nb_dst),],
                                             duree,
                                             distance,
                                             exclude,
                                             faceAFace = FALSE)

            k <- k + 1
          }
        }
        if(reste_dst > 0)
        {
          list_res[[k]] <- osrmTable_11_nm(src[(i*nb_src+1):(i*nb_src+reste_src),],
                                           dst[(j*nb_dst+1):(j*nb_dst+reste_dst),],
                                           duree,
                                           distance,
                                           exclude,
                                           faceAFace = FALSE)

          k <- k + 1
        }
      }
    })
  }else if(!interactive)
  {
    pb <- progress::progress_bar$new(
            format = "Calcul en cours [:bar] :percent :elapsed",
            total = nb_boucles_src*nb_boucles_dst, clear = FALSE, width= 60
          )

    pb$tick(0)

    list_res <- list()
    k <- 1
    i <- 0
    if(nb_boucles_src > 0)
    {
      for(i in 1:nb_boucles_src)
      {
        j <- 0
        if(nb_boucles_dst > 0)
        {
          for(j in 1:nb_boucles_dst)
          {
            pb$tick()
            list_res[[k]] <- osrmTable_11_nm(src[((i-1)*nb_src+1):(i*nb_src),],
                                             dst[((j-1)*nb_dst+1):(j*nb_dst),],
                                             duree,
                                             distance,
                                             exclude,
                                             faceAFace = FALSE)

            k <- k + 1
          }
        }
        if(reste_dst > 0)
        {
          list_res[[k]] <- osrmTable_11_nm(src[((i-1)*nb_src+1):(i*nb_src),],
                                           dst[(j*nb_dst+1):(j*nb_dst+reste_dst),],
                                           duree,
                                           distance,
                                           exclude,
                                           faceAFace = FALSE)

          k <- k + 1
        }
      }
    }
    if(reste_src > 0)
    {
      j <- 0
      if(nb_boucles_dst > 0)
      {
        for(j in 1:nb_boucles_dst)
        {
          list_res[[k]] <- osrmTable_11_nm(src[(i*nb_src+1):(i*nb_src+reste_src),],
                                           dst[((j-1)*nb_dst+1):(j*nb_dst),],
                                           duree,
                                           distance,
                                           exclude,
                                           faceAFace = FALSE)

          k <- k + 1
        }
      }
      if(reste_dst > 0)
      {
        list_res[[k]] <- osrmTable_11_nm(src[(i*nb_src+1):(i*nb_src+reste_src),],
                                         dst[(j*nb_dst+1):(j*nb_dst+reste_dst),],
                                         duree,
                                         distance,
                                         exclude,
                                         faceAFace = FALSE)

        k <- k + 1
      }
    }

  }else{}

  res <- do.call(rbind,list_res)
  res$idSrc <- as.character(res$idSrc)
  res$idDst <- as.character(res$idDst)

  # On cree un identifiant par couple
  res$ID <- c(1:nrow(res)) # ID : identifiant de couples
  res <- res[,c("ID",names(res)[-ncol(res)])]

  return(res)
}
