fileRbind <- function( files = list.files() ){
	file.list <- list()
	for( i in files ){
		file.list[[length(file.list)+1]] <- read.csv(i)
	}
	rm(i)
	output <- file.list[[1]]
	for( i in 2:length(file.list) ){
		output <- rbind(output,file.list[[i]])
	}
	rm(i)
	return(output)
}