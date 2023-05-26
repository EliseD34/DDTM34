#' @name adresseToCoord
#'
#' @title Geolocaliser des adresses et renvoyer leurs coordonnees en WGS84 (EPSG 4326)
#'
#' @description La fonction adresseToCoord permet de récupérer les coordonnées WGS84 (longitude/latitude code epsg 4326) de plusieurs adresses.
#'
#' La fonction utilise l'API Adresses (documentation : https://geo.api.gouv.fr/adresse).
#'
#' @param adresses vecteur texte Adresses à géolocaliser.
#' @param nbEchos valeur numérique. Nombre de résultats maximum par adresse en cas de doute sur la géolocalisation. Par défaut 1.
#' @param codePostal vecteur texte. Par défaut NULL.
#' @param codeInsee vecteur texte. Par défaut NULL.
#' @param interactive booléen. Choix du contexte d'exécution. Si TRUE, contexte shiny. Par défaut FALSE.
#'
#' @return Un data.frame
#'
#' @details En plus des coordonnées lon et lat, un score entre 0 et 1 est proposé, indiquant la pertinence de la géolocalisation.
#'
#' Le codePostal et le codeInsee sont des arguments optionnels qui permettent d'améliorer la géolocalisation en cas de doute sur un libellé d'adresse.
#'
#' Le code postal peut aussi être mentionné directement dans les libellés d'adresses.
#'
#' Le nombre d'echos permet à la fonction de retourner jusqu'à n echos si la géolocalisation renvoie plusieurs possibilités. Seuls les échos les plus pertinents sont proposés.
#'
#' @importFrom RJSONIO fromJSON
#' @importFrom shiny withProgress incProgress
#' @importFrom progress progress_bar
#' @export
#'
#' @examples
#' # Exemple 1 : avec une seule adresse
#' adresseToCoord(adresses = "88 avenue Verdier Montrouge",
#'                nbEchos = 1)
#'
#' # Exemple 2 : avec un vecteur d'adresses
#' adresses <- c("1 Rue des Abeilles 13001 Marseille",
#'               "1 Allee des Abeilles 13016 Marseille",
#'               "1 Impasse Abeille 13003 Marseille",
#'               "1 Impasse de la Chapelle 13013 Marseille",
#'               "1 Boulevard de la Chapelle 13009 Marseille",
#'               "1 Boulevard de la Chapelle 13014 Marseille")
#'
#' adresseToCoord(adresses = adresses,
#'                nbEchos = 1) # un resultat par adresse
#'
#' adresseToCoord(adresses = adresses,
#'                nbEchos = 2) # 2 resultats max possibles par adresse
#'
adresseToCoord <- function(adresses, nbEchos = 1, codePostal = NULL, codeInsee = NULL, interactive = FALSE)
{
  msg_error1 <- msg_error2 <- msg_error3 <- msg_error4 <- NULL

  if(!is.character(adresses)) msg_error1 <- "Le parametre adresses doit etre un vecteur caractere / "
  if(!is.numeric(nbEchos)) msg_error2 <- "Le parametre nbEchos doit etre une valeur numerique / "
  if(!is.null(codePostal)) if(!is.character(codePostal)) msg_error3 <- "Le parametre codePostal doit etre un vecteur caractere / "
  if(!is.null(codeInsee)) if(!is.character(codeInsee)) msg_error4 <- "Le parametre codeInsee doit etre un vecteur caractere / "

  if(any(!is.null(msg_error1),!is.null(msg_error2),!is.null(msg_error3),!is.null(msg_error4)))
  {
    stop(simpleError(paste0(msg_error1,msg_error2,msg_error3,msg_error4)))
  }

  options(adresse.server = "https://api-adresse.data.gouv.fr/")

  supprAccent <- function(text) {
    text <- gsub("['`^~\"]", " ", text)
    text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT//IGNORE")
    text <- gsub("['`^~\"]", "", text)
    return(text)
  }

  dt_adresses <- data.frame(ADRESSES=adresses, stringsAsFactors = FALSE)

  # Conversion en UTF-8 uniquement si les libellés d'adresses n'y sont pas déjà
  if(!"UTF-8" %in% Encoding(dt_adresses$ADRESSES)){
    dt_adresses$ADRESSES <- iconv(dt_adresses$ADRESSES, to = "UTF-8")
  }

  # Suppression des accents en entree pour que la fonction RJSONIO::fromjson puisse traiter la requete
  dt_adresses$ADRESSES <- supprAccent(dt_adresses$ADRESSES)

  if(!is.null(codePostal)) dt_codePostal <- data.frame(codePostal=codePostal, stringsAsFactors = FALSE)
  if(!is.null(codeInsee)) dt_codeInsee <- data.frame(codeInsee=codeInsee, stringsAsFactors = FALSE)

  if(!interactive)
  {
    pb <- progress::progress_bar$new(
      format = paste0("G\u00e9olocalisation en cours de ",nrow(dt_adresses)," adresses -  [:bar] :percent :elapsed"),
      total = nrow(dt_adresses), clear = FALSE, width= 80
    )

    pb$tick(0)

    res <- t(lapply(1:nrow(dt_adresses),function(x) {

      pb$tick()

      if(nchar(dt_adresses[x,]) == 0)
      {
        return(data.frame(ADRESSES = dt_adresses[x,],
                          ADRESSES_GEOLOC = "unknown",
                          LON = 0,
                          LAT = 0,
                          SCORE = 0,
                          stringsAsFactors = FALSE))
      }

      if(!is.null(codePostal))
      {
        req <- paste0(getOption("adresse.server"),"search/?q=",paste(unlist(strsplit(as.character(tolower(dt_adresses[x,]))," ")),collapse="+"),"&postcode=",dt_codePostal[x,],"&limit=",nbEchos)
      }else if(!is.null(codeInsee))
      {
        req <- paste0(getOption("adresse.server"),"search/?q=",paste(unlist(strsplit(as.character(tolower(dt_adresses[x,]))," ")),collapse="+"),"&citycode=",dt_codeInsee[x,],"&limit=",nbEchos)
      }else
      {
        req <- paste0(getOption("adresse.server"),"search/?q=",paste(unlist(strsplit(as.character(tolower(dt_adresses[x,]))," ")),collapse="+"),"&limit=",nbEchos)
      }

      # On specifie explicitement que l'encodage en sortie doit etre du latin1, sinon il considere qu'il est unknown meme si implicitement c'est du latin1.
      # Sinon, cela qui peut generer des caracteres non reconnus a l'affichage si la reponse contient des caracteres speciaux.
      # MAJ 2023 - suppression de cette rustine, on précise désormais UTF8 pour RJSONIO
      #a <- RJSONIO::fromJSON(req, encoding = "latin1")
      
      a <- RJSONIO::fromJSON(req, encoding = "UTF-8")
      if(length(a[["features"]])>0)
      {
        res <- lapply(1:length(a[["features"]]),function(y){
                                                data.frame(ADRESSES = dt_adresses[x,],
                                                           ADRESSES_GEOLOC = a[["features"]][[y]][["properties"]][["label"]],
                                                           LON = a[["features"]][[y]][["geometry"]][["coordinates"]][1],
                                                           LAT = a[["features"]][[y]][["geometry"]][["coordinates"]][2],
                                                           SCORE = a[["features"]][[y]][["properties"]][["score"]],
                                                           stringsAsFactors = FALSE)
        })

        res <- do.call("rbind", res)

        # On convertit la sortie latin1 en UTF-8
        #res$ADRESSES_GEOLOC <- iconv(res$ADRESSES_GEOLOC, from = "latin1", to = "UTF-8")

        return(res)
      }else{
        return(data.frame(ADRESSES = dt_adresses[x,],
                          ADRESSES_GEOLOC = "unknown",
                          LON = 0,
                          LAT = 0,
                          SCORE = 0,
                          stringsAsFactors = FALSE))
      }


    }))
  }else
  {
    shiny::withProgress(message = paste0("G\u00e9olocalisation en cours de ",nrow(dt_adresses)," adresses"),{

      res <- t(lapply(1:nrow(dt_adresses),function(x) {

        shiny::incProgress(1/nrow(dt_adresses))

        if(nchar(dt_adresses[x,]) == 0)
        {
          return(data.frame(ADRESSES = dt_adresses[x,],
                            ADRESSES_GEOLOC = "unknown",
                            LON = 0,
                            LAT = 0,
                            SCORE = 0,
                            stringsAsFactors = FALSE))
        }

        if(!is.null(codePostal))
        {
          req <- paste0(getOption("adresse.server"),"search/?q=",paste(unlist(strsplit(as.character(tolower(dt_adresses[x,]))," ")),collapse="+"),"&postcode=",dt_codePostal[x,],"&limit=",nbEchos)
        }else if(!is.null(codeInsee))
        {
          req <- paste0(getOption("adresse.server"),"search/?q=",paste(unlist(strsplit(as.character(tolower(dt_adresses[x,]))," ")),collapse="+"),"&citycode=",dt_codeInsee[x,],"&limit=",nbEchos)
        }else
        {
          req <- paste0(getOption("adresse.server"),"search/?q=",paste(unlist(strsplit(as.character(tolower(dt_adresses[x,]))," ")),collapse="+"),"&limit=",nbEchos)
        }

        # On specifie explicitement que l'encodage en sortie doit etre du latin1, sinon il considere qu'il est unknown meme si implicitement c'est du latin1.
        # Sinon, cela qui peut generer des caracteres non reconnus a l'affichage si la reponse contient des caracteres speciaux.
        # MAJ 2023 - suppression de cette rustine, on précise désormais UTF8 pour RJSONIO
        
        a <- RJSONIO::fromJSON(req, encoding = "UTF-8")

        if(length(a[["features"]])>0)
        {
          res <- lapply(1:length(a[["features"]]),function(y){
            data.frame(ADRESSES = dt_adresses[x,],
                       ADRESSES_GEOLOC = a[["features"]][[y]][["properties"]][["label"]],
                       LON = a[["features"]][[y]][["geometry"]][["coordinates"]][1],
                       LAT = a[["features"]][[y]][["geometry"]][["coordinates"]][2],
                       SCORE = a[["features"]][[y]][["properties"]][["score"]],
                       stringsAsFactors = FALSE)
          })

          res <- do.call("rbind", res)

          # On convertit la sortie latin1 en UTF-8
          #res$ADRESSES_GEOLOC <- iconv(res$ADRESSES_GEOLOC, from = "latin1", to = "UTF-8")

          return(res)
        }else{
          return(data.frame(ADRESSES = dt_adresses[x,],
                            ADRESSES_GEOLOC = "unknown",
                            LON = 0,
                            LAT = 0,
                            SCORE = 0,
                            stringsAsFactors = FALSE))
        }
      }))
    })
  }

  coordAdresses <- do.call("rbind", res)

  coordAdresses$LON <- as.numeric(clean_coord(coordAdresses$LON))
  coordAdresses$LAT <- as.numeric(clean_coord(coordAdresses$LAT))
  coordAdresses$SCORE <- as.numeric(clean_coord(coordAdresses$SCORE))

  return(coordAdresses)
}
