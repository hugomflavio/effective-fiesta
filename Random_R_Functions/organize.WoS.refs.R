organize.WoS.refs <- function(file,name="scriptoutput_WoS.csv") {
		raw <- readChar(file, file.info(file)$size)
		raw <- sub("FN Thomson Reuters Web of Science™\nVR 1.0","\n", raw)
	#filtering unwanted characters
		filtered.raw <- sub("\n\nEF","",raw)
		filtered.raw <- gsub("\nER","",filtered.raw)

	#breaking the string
		temp1 <- gsub("\n   "," + ",filtered.raw)
		temp2 <- strsplit(temp1,"\n\n")
		spltvec <- vector("list",length(temp2[[1]]))
		for (i in c(1:length(temp2[[1]])) ) {spltvec[i] <- strsplit(temp2[[1]][i],"\n")}
		spltvec <- spltvec[-1]
		spltvec <- lapply( spltvec, FUN= function(x) { sub(" ",";",x) } )
		spltvec <- lapply( spltvec, FUN= function(x) { strsplit(x,";") } )

	#creating the final object
		breaks<-c("PT","AU","BA","CA","GP","RI","OI","BE","Z2","TI","X1","Y1","Z1","FT","PN","AE","Z3","SO","S1","SE","BS","VL","IS","SI","MA","BP","EP","AR","DI","D2","SU","PD","PY","AB","X4","Y4","Z4","AK","CT","CY","SP","CL","HO","TC","Z8","ZR","ZB","ZS","Z9","U1","U2","SN","EI","BN","UT","PM")
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
		for(i in 1:length(tabularrefs) ) {
			rowspots <- match(tabularrefs[[i]][,1],finaltable[,1])
			finaltable[,i+1]<-NA
			for( k in 1:length(rowspots) ) {
				finaltable[rowspots[k],i+1] <- tabularrefs[[i]][k,2]
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

	if( file.exists("scriptoutput_WoS.csv") ){
		cat("MESSAGE: The file 'scriptoutput_WoS.csv' has been successfuly added to your workspace\n")
	} else {
		cat("WARNING: Something went wrong, output file was not created\n")
	}
}

