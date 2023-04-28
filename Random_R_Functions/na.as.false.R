na.as.false <- function(x){
  x[is.na(x)] <- FALSE
  return(x)
}
