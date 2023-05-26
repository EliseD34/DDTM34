#' @name codeComToCoord
#'
#' @title Fonction de passage entre code commune INSEE et coordonnees au chef-lieu (chx) ou au centroide ou a un point sur la commune (pos).
#'
#' @description La fonction codeComToCoord permet de récupérer des coordonnées WGS84 (EPSG 4326) de plusieurs communes à partir des codes communes INSEE.
#'
#' Les points en sortie correspondent aux chefs-lieux de communes (chx), aux centroïdes ou à des points situés obligatoirement à l'intérieur de la commune,
#' calculés selon l'algorithme spécifique "point_on_surface".
#'
#' Selon la morphologie du contour de la commune, le centroïde peut être situé en-dehors de ses limites.
#' Pour que le point soit obligatoirement situé dans la commune, il faut alors spécifier type = "pos" (point_on_surface).
#'
#' Il est possible de choisir une géographie de 2017 à l'année de mise à jour du package.
#' Par exemple, si le package a été mis à jour en géographie 2023, il peut transformer en coordonnées lon/lat des codes communes des COG 2017 à 2023.
#'
#' @param codeInsee vecteur texte.
#' @param geo texte. Par défaut, l'année courante.
#' @param type texte. Type de point souhaité. A choisir parmi "chx" par défaut (chef-lieu de la commune), "centroide" (barycentre) ou "pos" (point sur la surface).
#'
#' @return Un data.frame de trois colonnes "code", "lon" et "lat".
#'
#' @importFrom rio import
#' @export
#'
#' @examples
#' # Renvoie les coordonnees des chefs-lieux des communes de Montrouge (92049)
#' # et de Malakoff (92046) en geographie 2021.
#' codeComToCoord(codeInsee = c("92049","92046"),
#'                geo = "2020",
#'                type = "chx")
#'
#' # Renvoie les coordonnees des centroides des communes de Montrouge (92049)
#' # et de Malakoff (92046) en geographie 2021.
#' codeComToCoord(codeInsee = c("92049","92046"),
#'                geo = "2020",
#'                type = "centroide")
#'
#' # Renvoie les coordonnees "point_on_surface" des communes de Montrouge (92049)
#' # et de Malakoff (92046) en geographie 2021.
#' codeComToCoord(codeInsee = c("92049","92046"),
#'                geo = "2020",
#'                type = "pos")
#'
#' # Renvoie les coordonnees des chefs-lieux des communes d'Ancteville (50007)
#' # et de Saint-Sauveur-Lendelin (50550) en geographie 2018.
#' codeComToCoord(codeInsee = c("50007","50550"),
#'                geo = "2018",
#'                type = "chx")
#'
#' # Au 01/01/2019, les communes d'Ancteville (50007),
#' # de Saint-Sauveur-Lendelin (50550) et autres communes
#' # ont fusionne pour former Saint-Sauveur-Villages (50550).
#'
#' # Renvoie les coordonnees du chef-lieu de la commune
#' # de Saint-Sauveur-Villages (50550) en geographie 2019.
#' # Et affiche un avertissement si le code commune est introuvable
#' # pour la geographie donnee.
#' codeComToCoord(codeInsee = c("50007","50550"),
#'                geo = "2019",
#'                type = "chx")
#'
codeComToCoord <- function(codeInsee, geo = format(Sys.time(), format = "%Y"), type = "chx")
{
  # import de la table
  pointCom <- tryCatch({

    base::get(paste0("tablePassage",geo))

  },error = function(err){
    stop(simpleError(paste0("La g\u00e9ographie des communes n'est pas disponible pour le mill\u00e9sime ",geo)))
  })

  # filtre
  selectPoint <- pointCom[pointCom$code %in% unique(codeInsee),]

  # pour conserver l'ordre des codeInsee de la table en entree
  codeInsee <- data.frame(code=codeInsee, stringsAsFactors = FALSE)
  codeInsee$id <- c(1:nrow(codeInsee))
  coordCom <- merge(selectPoint, codeInsee, by = "code")
  coordCom <- coordCom[order(coordCom$id),]

  if(type == "centroide")
  {
    coordCom <- coordCom[c("code","centroid_lon","centroid_lat")]
  }else if(type == "pos")
  {
    coordCom <- coordCom[c("code","pos_lon","pos_lat")]
  }else
  {
    coordCom <- coordCom[c("code","chx_lon","chx_lat")]
  }

  names(coordCom) <- c("code","lon","lat")
  coordCom$lon <- round(coordCom$lon,5)
  coordCom$lat <- round(coordCom$lat,5)
  if(nrow(coordCom) > 0) row.names(coordCom) <- c(1:nrow(coordCom))

  if(nrow(codeInsee) != nrow(coordCom))
  {
    comNonGeoloc <- codeInsee[!codeInsee$code %in% coordCom$code, "code"]
    if(length(comNonGeoloc) == 1)
    {
      message(paste0("[WARNING] Il y a ",length(comNonGeoloc)," commune non g\u00e9olocalis","\u00e9","e."))
      message(paste0("Veuillez v\u00e9rifier que le mill\u00e9sime de la g\u00e9ographie des codes communes "))
      message(paste0("correspond bien \u00e0 l'argument geo."))
    }
    if(length(comNonGeoloc) > 1)
    {
      message(paste0("[WARNING] Il y a ",length(comNonGeoloc)," communes non g\u00e9olocalis","\u00e9","es."))
      message(paste0("Veuillez v\u00e9rifier que le mill\u00e9sime de la g\u00e9ographie des codes communes"))
      message(paste0("correspond bien \u00e0 l'argument geo. "))
    }
    message(paste0("Liste des codes communes non g\u00e9olocalis","\u00e9","es : "))
    message(paste(comNonGeoloc, collapse = ", "))
  }

  return(coordCom)
}
