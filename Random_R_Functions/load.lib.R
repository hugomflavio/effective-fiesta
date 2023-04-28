load.lib <- function( filename ) {
	library("readODS")
	temp <- read.ods(as.character(filename))
	temp2 <- temp[[1]]
	colnames(temp2) <- temp2[1,]
	file <- temp2[-1,]
	return(file)
}

