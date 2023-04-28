# use ris.to.csv() instead

organize.DTU.refs <- function(file,name="scriptoutput_DTU.csv") {
		raw <- readChar(file, file.info(file)$size)
	#filtering unwanted characters
		filtered.raw <- gsub("\nER  -","",raw)

	#breaking the string
		temp <- strsplit(filtered.raw,"\n\n") #break each entry
		spltvec <- vector("list",length(temp[[1]])) #create repository
		for (i in c(1:length(temp[[1]])) ) {spltvec[i] <- strsplit(temp[[1]][i],"\n")} #split each entry in the repository
		spltvec <- lapply( spltvec, FUN= function(x) { strsplit(x,"  - ") } ) #split the ID from the Info

	#preparing string for data transfer (turn the sub-lists into dataframes)
		tabularrefs <- vector("list",length(spltvec))
		for( i in 1:length(spltvec) ) {
			tabularrefs[[i]] <- matrix(nrow=length(spltvec[[i]]),ncol=2)
			for( k in 1:length(spltvec[[i]]) ) {
				tabularrefs[[i]][k,1] <- spltvec[[i]][[k]][1]
				tabularrefs[[i]][k,2] <- spltvec[[i]][[k]][2]
			}
		}

	#creating the final object
		repo <- character(0)
		for( i in 1:length(tabularrefs) ){
			repo <- c(repo, tabularrefs[[i]][,1])
		}

		finaltable <- matrix(data=unique(repo),nrow=length(unique(repo)),ncol=1)
		finaltable <- as.data.frame(finaltable)

	#proceed to data fransfer
		for( i in 1:length(tabularrefs) ) {
			rowspots <- match(tabularrefs[[i]][,1],finaltable[,1])
			finaltable[,i+1]<-NA
			for( k in 1:length(rowspots) ) {
				if( is.na(finaltable[rowspots[k],i+1]) ) {
					finaltable[rowspots[k],i+1] <- tabularrefs[[i]][k,2]
				} else {
					finaltable[rowspots[k],i+1] <- paste(finaltable[rowspots[k],i+1], tabularrefs[[i]][k,2], sep="; ")
				}
			}
		}

		finaltable <- t(finaltable)
		
	#replace column names

		colnames(finaltable) <- finaltable[1,]
		finaltable <- finaltable[-1,]
	
	#remove empty columns

		decision <- vector()
		
		for( i in 1:ncol(finaltable) ){
			if( all(is.na(finaltable[,i])) ){
				decision[i] <- FALSE
			} else {
				decision[i] <- TRUE
			}
		}	

		to.print <- finaltable[,decision]

	write.csv(to.print,name,row.names=FALSE,na="")

	if( file.exists("scriptoutput_DTU.csv") ){
		cat("MESSAGE: The file 'scriptoutput_DTU.csv' has been successfuly added to your workspace\n")
	} else {
		cat("WARNING: Something went wrong, output file was not created\n")
	}
}
