ris.to.csv <- function(file,name="ris_output.csv") {
 raw <- readChar(file, file.info(file)$size)
# break each entry
 temp <- strsplit(raw,"\nER") 
# create repository
 spltvec <- vector("list",length(temp[[1]])) 
# split each entry in the repository
 for (i in c(1:length(temp[[1]])) ) {spltvec[i] <- strsplit(temp[[1]][i],"\n(?=[^ ])",perl=TRUE)} 
# remove extra line breaks (change to semi-colons on authors and none for the rest)
 for(i in 1:length(spltvec)){ 
 for(j in 1:length(spltvec[[i]])){ 
  if(grepl("^AU",spltvec[[i]][j])){
   spltvec[[i]][j] <- gsub("\n",";",spltvec[[i]][j]) 
  }else{
   spltvec[[i]][j] <- gsub("\n","",spltvec[[i]][j]) 
  } 
 }
}
# split the ID from the Info
 for(i in 1:2) spltvec <- lapply( spltvec, FUN= function(x) { gsub("  "," ",x) } ) 
 spltvec <- lapply( spltvec, FUN= function(x) { sub(" ","BREAKHERE",x) } ) 
 spltvec <- lapply( spltvec, FUN= function(x) { strsplit(x,"BREAKHERE") } ) 
# preparing string for data transfer (turn the sub-lists into dataframes)
 tabularrefs <- vector("list",length(spltvec))
 for( i in 1:length(spltvec) ) {
  tabularrefs[[i]] <- matrix(nrow=length(spltvec[[i]]),ncol=2)
  for( k in 1:length(spltvec[[i]]) ) {
   tabularrefs[[i]][k,1] <- spltvec[[i]][[k]][1]
   tabularrefs[[i]][k,2] <- spltvec[[i]][[k]][2]
  }
 }
# creating the final object
 repo <- character(0)
 for( i in 1:length(tabularrefs) ){
  repo <- c(repo, tabularrefs[[i]][,1])
 }
 finaltable <- matrix(data=unique(repo),nrow=length(unique(repo)),ncol=1)
 finaltable <- as.data.frame(finaltable)
# proceed to data fransfer
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
# replace column names
 colnames(finaltable) <- finaltable[1,]
 finaltable <- finaltable[-1,]
# remove empty columns
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
 if( file.exists(name) ){
  cat("MESSAGE: The file '", paste(name),"' has been successfuly added to your workspace\n",sep="")
 } else {
  cat("WARNING: Something went wrong, output file was not created\n")
 }
}
