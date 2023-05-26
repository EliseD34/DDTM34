vectorToDf <-
function(vector)
{
  dt <- data.frame(id=vector[1],lon=vector[2],lat=vector[3])
  return(dt)
}
