#' Import dataset with multiple objects directly into a list
#' 
#' @param source The source file
#' 
dataToList <- function(source){
	e <- new.env()
	load(source, envir = e)
	return(as.list(e))
}
