organize.BOn.refs <- function(file) {
		raw <- readChar(file, file.info(file)$size)
	#filtering unwanted characters
		filtered.raw <- gsub("Registro:","xxslicexxREG-",raw)

	#breaking the string
		temp1 <- strsplit(filtered.raw,"xxslicexx")
		spltvec <- vector("list",length(temp1[[1]]))
		for (i in c(1:length(temp1[[1]])) ) {spltvec[i] <- strsplit(temp1[[1]][i],"\n")}
		spltvec <- spltvec[-1]
		spltvec <- lapply( spltvec, FUN= function(x) { strsplit(x,"- ") } )

	#creating the final object
		tempvec <- vector()
		for(i in 1:length(tabularrefs) ) {
			tempvec <- append(tempvec,tabularrefs[[i]][,1])
		}
		breaks <- unique(tempvec)
		finaltable <- matrix(data=breaks,nrow=length(breaks),ncol=1)
		finaltable <- as.data.frame(finaltable)

	#preparing string for data transfer
		tabularrefs <- vector("list",length(spltvec))
		for( i in 1:length(spltvec) ) {
			tabularrefs[[i]] <- matrix(nrow=length(spltvec[[i]]),ncol=2)
			for( k in 1:length(spltvec[[i]]) ) {
				tabularrefs[[i]][k,1] <- spltvec[[i]][[k]][1]
				tabularrefs[[i]][k,2] <- spltvec[[i]][[k]][2]
			}
		}

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

	write.csv(t(finaltable),"scriptoutput_BOn.csv",row.names=FALSE,na="")

	if( file.exists("scriptoutput_BOn.csv") ){
		cat("MESSAGE: The file 'scriptoutput_BOn.csv' has been successfuly added to your workspace\n")
	} else {
		cat("WARNING: Something went wrong, output file was not created\n")
	}
}

