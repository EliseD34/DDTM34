# Test si l'objet est un objet sf
#
# La fonction testSf permet de tester si l'objet passé en paramètre est un objet sf.
#
# @param x objet sf. Si autre, ignoré
#
# @return TRUE ou FALSE
testSf <-
function (x)
{
  if (methods::is(x, "sf")) {
    if (is.na(sf::st_crs(x))) {
      stop(paste("Your input (", quote(x), ") does not have a valid coordinate reference system.",
                 sep = ""), call. = F)
    }
    return(TRUE)
  }
  return(FALSE)
}
