na_as_false <- function(x){
  x[is.na(x)] <- FALSE
  return(x)
}
