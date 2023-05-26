convertToDf <- function(objet)
{
  # Si objet est un vecteur
  if(is.vector(objet))
  {
    objet <- vectorToDf(vector = objet)
  }

  # si objet est un objet sp, on le transforme d'abord en objet sf
  if (methods::is(objet, "Spatial")) {
    objet <- sf::st_as_sf(x = objet)
  }

  # si objet est un objet sf, on le transforme en data.frame
  if (testSf(x = objet)) {
    objet <- sfToDf(x = objet)
  }

  names(objet) <- c("id", "lon", "lat")

  return(objet)
}
