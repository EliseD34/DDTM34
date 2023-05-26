#' @name codeLauToCoord
#'
#' @title Fonction de passage entre code LAU de communes etrangeres et coordonnees au centroide ou a un point situe sur la commune.
#'
#' @description La fonction codeLauToCoord permet de récupérer des coordonnées WGS84 (EPSG 4326) de plusieurs communes étrangères à partir des codes LAU.
#'
#' Les points en sortie correspondent aux centroïdes ou à des points situés obligatoirement à l'intérieur de la commune,
#' calculés selon l'algorithme spécifique "point_on_surface".
#'
#' Selon la morphologie du contour de la commune, le centroïde peut être situé en-dehors de ses limites.
#' Pour que le point soit obligatoirement dans la commune, il faut alors spécifier type = "pos" (point_on_surface).
#'
#' Les codes pays, sur 2 caractères, sont consultables sur insee.fr (https://www.insee.fr/fr/information/2028273).
#'
#' Les codes LAU sont consultables sur le site d'eurostat (https://ec.europa.eu/eurostat/fr/web/nuts/local-administrative-units).
#'
#' Les communes LAU sont en géographie n-1 (dernier millésime validé par eurostat).
#' Le champ couvert par metric.osrm contient les LAU de plusieurs régions frontalières à la France, hors Andorre et Monaco.
#' La liste exhaustive des codes LAU entrant dans le champ de metric.osrm est disponible depuis un fichier csv en exécutant cette commande :
#'
#' code_gisco <- rio::import(system.file("extdata","listeCodesGISCO.csv", package = "metric.osrm"))
#'
#' Il s'agit du code GISCO qui est la concaténation du code pays et du code LAU. En effet, un code LAU d'un pays peut être identique à celui d'un autre.
#' Afin de garantir l'unicité du code, il est préférable d'utiliser le code GISCO, issu d'eurostat (https://ec.europa.eu/eurostat/fr/web/gisco).
#'
#' Les codes communes français sont ignorés. Utilisez, pour eux, la fonction codeComToCoord.
#'
#' @param codePays vecteur texte. Code pays sur 2 caractères selon la codification des pays et des territoires étrangers.
#' @param codeLau vecteur texte. Code LAU de communes étrangères.
#' @param type texte. Type de point souhaité. A choisir parmi "centroide" (barycentre de la commune) ou "pos" par défaut (point sur la commune).
#'
#' @return Un data.frame de trois colonnes "code", "lon" et "lat".
#'
#' @importFrom rio import
#' @export
#'
#' @examples
#' # Renvoie les coordonnees des points sur la surface des communes de Bruxelles (21004)
#' # de Liege (62063), de Luxembourg (0304) et de Stuttgard (08111000).
#' codeLauToCoord(codePays = c("BE","BE","LU","DE"),
#'                codeLau = c("21004","62063","0304","08111000"),
#'                type = "pos")
#'
#' # Renvoie les coordonnees des centroides des communes des communes de Bruxelles (21004)
#' # de Liege (62063), de Luxembourg (0304) et de Stuttgard (08111000).
#' codeLauToCoord(codePays = c("BE","BE","LU","DE"),
#'                codeLau = c("21004","62063","0304","08111000"),
#'                type = "centroide")
#'
#' # Ajout de Metz (57463) et de Strasbourg (67482).
#' codeLauToCoord(codePays = c("FR","FR","BE","BE","LU","DE"),
#'                codeLau = c("57463","67482","21004","62063","0304","08111000"),
#'                type = "pos")
#'
#' # Ajout d'une commune etrangere hors champ de metric.osrm, Berlin (11000000).
#' codeLauToCoord(codePays = c("BE","BE","LU","DE","DE"),
#'                codeLau = c("21004","62063","0304","08111000","11000000"),
#'                type = "pos")
#'
codeLauToCoord <- function(codePays, codeLau, type = "pos")
{
  # import de la table
  pointCom <- tablePassageLAU2020

  # concatenation du code pays et du LAU : code gisco
  codeGisco <- paste0(codePays,"_",codeLau)

  # filtre
  selectPoint <- pointCom[pointCom$code %in% unique(codeGisco),]

  # pour conserver l'ordre des codeLau de la table en entree
  codeGisco <- data.frame(code=codeGisco, stringsAsFactors = FALSE)
  codeGisco$id <- c(1:nrow(codeGisco))
  coordCom <- merge(selectPoint, codeGisco, by = "code")
  coordCom <- coordCom[order(coordCom$id),]

  if(type == "pos")
  {
    coordCom <- coordCom[c("code","pos_lon","pos_lat")]
  }else
  {
    coordCom <- coordCom[c("code","centroid_lon","centroid_lat")]
  }

  names(coordCom) <- c("code","lon","lat")
  coordCom$lon <- round(coordCom$lon,5)
  coordCom$lat <- round(coordCom$lat,5)
  if(nrow(coordCom) > 0) row.names(coordCom) <- c(1:nrow(coordCom))

  if(nrow(codeGisco) != nrow(coordCom))
  {
    if(any(codePays %in% "FR"))
    {
      if(length(codePays[codePays %in% "FR"]) == 1)
      {
        message(paste0("[WARNING] Il y a ",length(codePays[codePays %in% "FR"])," commune fran","\u00e7","aise."))
        message(paste0("Pour elle seulement, veuillez utiliser la fonction codeComToCoord."))
      }
      if(length(codePays[codePays %in% "FR"]) > 1)
      {
        message(paste0("[WARNING] Il y a ",length(codePays[codePays %in% "FR"])," communes fran","\u00e7","aises."))
        message(paste0("Pour elles seulement, veuillez utiliser la fonction codeComToCoord."))
      }
    }

    comNonGeoloc <- codeGisco[!codeGisco$code %in% coordCom$code, "code"]
    comNonGeoloc <- comNonGeoloc[substr(comNonGeoloc,1,2) != "FR"]

    if(length(comNonGeoloc) > 0)
    {
      if(length(comNonGeoloc) == 1)
      {
        message(paste0("[WARNING] Il y a ",length(comNonGeoloc)," commune non g\u00e9olocalis","\u00e9","e ou hors du champ de metric.osrm."))
      }
      if(length(comNonGeoloc) > 1)
      {
        message(paste0("[WARNING] Il y a ",length(comNonGeoloc)," communes non g\u00e9olocalis","\u00e9","es ou hors du champ de metric.osrm."))
      }
      message(paste0("Liste des codes communes non g\u00e9olocalis","\u00e9","es ou hors-champ : "))
      message(paste(comNonGeoloc, collapse = ", "))
    }
  }

  return(coordCom)
}
