pply.second.filter <- function(lib){
	filter <- lib$Lotic !="N" & lib$Agriculture !="N"
	lib <- lib[filter,]
	return(lib)
}
