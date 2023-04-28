dms.to.ddd <- function(x, symb = "°|'|\"") { 
	.converter <- function(dms, symb = symb) {
		values <- strsplit(as.character(dms),symb)[[1]]
		output <- as.numeric(values[1])+(as.numeric(values[2])/60)+(as.numeric(values[3])/3600)
		if( values[4] == "W" | values[4] == "S" ) output <- -output
		return(output)
	}
	if (is.data.frame(x) | is.matrix(x)) {
		return(as.data.frame(apply(x, c(1,2), function(i) .converter(dms = i, symb = symb))))
	} else {
		return(sapply(x, function(i) .converter(dms = i, symb = symb)))
	} 
}
