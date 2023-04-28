#  Script: Prepare respirometer output for data analysis
#
#  User functions:
#  import.resp.data(file)
#
#  Internal functions:
#  .break.lines()
#  .count.chambers()


import.resp.data <- function(file) {
	raw <- readChar(file, file.info(file)$size)
	Sys.setlocale('LC_ALL','C') #because the originating system is danish

	temp <- .break.lines(raw,"\r\n")

	temp <- lapply(temp, FUN= function(x) { sub(",",".",x) } )

	ChamberN <- .count.chambers(temp)

	first.row <- sum(10,ChamberN)
	last.row <- length(temp)-first.row

	column.headers <- as.vector(temp[[(first.row-1)]])

	output <- matrix(nrow=1,ncol=length(column.headers))
	output <- as.data.frame(output)
	colnames(output) <- column.headers

	for( i in 1:last.row ){
		for( k in 1:length(column.headers) ){
			output[i,k] <- temp[[i+(first.row-1)]][k]
		}
	}

	return(output)
}

.break.lines <- function(input,breakpoint){
	holder <- strsplit(input,breakpoint)
	output <- vector("list",length(holder[[1]]))
	for( i in c(1:length(holder[[1]])) ){
		output[i] <- strsplit(holder[[1]][i],"\t")
	}
	return(output)
}

.count.chambers <- function(input){
	output <- 0
	for( i in 7:10 ){
		output <- sum(output,grepl("Chamber",input[[i]][1]))
	}
	return(output)
}
