dmm.to.ddd <- function(x,degreesymb="°") { 
	if( degreesymb != "°" )	cat("Degree symbol changed to '",degreesymb,"'.\n",sep="")
	temp <- as.numeric(strsplit(x,degreesymb)[[1]])
	output <- temp[1]+(temp[2]/60)
	return(output)
}
