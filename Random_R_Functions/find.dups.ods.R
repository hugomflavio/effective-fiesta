find.dups.ods <- function( filename ) {
	library("readODS")
	temp <- read.ods(as.character(filename))
	temp2 <- temp[[1]]
	colnames(temp2) <- temp2[1,]
	file <- temp2[-1,]

	dups <- duplicated(file$DI)
	file$Dup[dups] <- "DUP!"

	notdups <- !dups
	file$Dup[notdups] <- "-"

	maybedups <- file$DI==""
	file$Dup[maybedups] <- "?"

	return(file$Dup)
}

