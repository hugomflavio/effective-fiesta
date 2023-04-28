extract.by.group <- function(lib,column){
	selected.rows <- lib[,as.character(column)] == "Y"
	output <- lib[selected.rows,]
	return(output)
}

