# Calcul la durée et/ou la distance
#
# La fonction requeteOsrm_n1 permet de calculer la durée et la distance entre un groupe de points
# et un point.
#
# @param src data.frame (n lignes et 3 colonnes id/lon/lat).
# @param idx_src vecteur numérique de longueur n où n est le nombre de couples pour la requête.
# @param dst data.frame (1 ligne et 3 colonnes id/lon/lat).
# @param idx_dst valeur numérique.
# @param measure vecteur caractères. Choix des options entre "duration" et "distance".
# @param exclude_str string. Exclu un type de route pour le calcul du trajet. String formé pour la requête.
# Si exclude = NULL, exclude_str = "".
#
# @return list
requeteOsrm_n1 <-
function(src, idx_src, dst, idx_dst, measure, exclude_str)
{
  if(!is.null(getOption("osrm.server")))
  {
    if(substr(getOption("osrm.server"),nchar(getOption("osrm.server")),nchar(getOption("osrm.server"))) == "/")
    {
      server <- getOption("osrm.server")
    }else
    {
      server <- paste0(getOption("osrm.server"), "/")
    }
  }else
  {
    server <- getOption("osrm.server")
  }

  req <- paste(paste0(server, "table/v1/",
                      getOption("osrm.profile"), "/"),
               paste(clean_coord(src[idx_src,2]),clean_coord(src[idx_src,3]), sep = ",",collapse = ";"),
               paste0(";",clean_coord(dst[,2]),",",clean_coord(dst[,3])),
               "?sources=", paste(idx_src-1, collapse = ";"),
               "&destinations=", paste(idx_dst, collapse = ";"),
               "&annotations=", paste(measure, collapse = ","),
               exclude_str, sep = "")

  req <- utils::URLencode(req)

  res <- tryCatch({

    RJSONIO::fromJSON(req)

  },error = function(err){
    message("Un encombrement du r\u00e9seau a eu lieu mais le calcul continue.")

    # On attend 5 secondes que le réseau redevienne fluide avant de relancer la requête
    Sys.sleep(5)

    res <- tryCatch({

      RJSONIO::fromJSON(req)

    },error = function(err){
      message("Le r\u00e9seau ne s'est pas lib\u00e9r\u00e9. Nouvelle relance de la requ\u00eate x1.")

      # On attend maintenant 10 secondes que le réseau redevienne fluide avant de relancer la requête
      Sys.sleep(10)

      res <- tryCatch({

        RJSONIO::fromJSON(req)

      },error = function(err){
        message("Le r\u00e9seau ne s'est pas lib\u00e9r\u00e9. Nouvelle relance de la requ\u00eate x2.")

        # On attend 20 secondes que le réseau redevienne fluide avant de relancer la requête
        Sys.sleep(30)

        res <- tryCatch({

          RJSONIO::fromJSON(req)

        },error = function(err){
          message("Le r\u00e9seau ne s'est pas lib\u00e9r\u00e9. Nouvelle relance de la requ\u00eate x3.")

          # On attend 60 secondes que le réseau redevienne fluide avant de relancer la requête
          Sys.sleep(60)

          res <- tryCatch({

            RJSONIO::fromJSON(req)

          },error = function(err){
            message("Le r\u00e9seau ne s'est toujours pas lib\u00e9r\u00e9. Arr\u00eat du traitement.")
            message(err)
          })
        })
      })
    })
  })

  # Attente pour laisser le temps à la requête de faire l'aller-retour vers le serveur. Sinon risque de plantage de connexion.
  Sys.sleep(0.01)

  return(res)
}
