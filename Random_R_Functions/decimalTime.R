decimalTime <- function(x) {
	.converter <- function(x) {
		x = as.character(x)
	  x = as.numeric(unlist(strsplit(x, "[:]")))
	  if(length(x)==2) x = x[1]+x[2]/60
	  if(length(x)==3) x = x[1]+x[2]/60+x[3]/3600
	  return(x)
	}
	if(length(x)<1) stop("Input appears to be empty.")
	if(length(x)==1) output <- .converter(x)
	if(length(x)>1) output <- unlist(lapply(x,.converter))
	return(output)
}
