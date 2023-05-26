clean_coord <-
function (x)
{
  format(round(as.numeric(x), 5), scientific = FALSE, justify = "none",
         trim = TRUE, nsmall = 5, digits = 5)
}
