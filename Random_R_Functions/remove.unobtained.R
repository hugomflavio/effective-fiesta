remove.unobtained <- function(lib){
	filter <- lib$Obtained !="N"
	lib <- lib[filter,]
	return(lib)
}

