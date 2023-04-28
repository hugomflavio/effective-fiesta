my.percent <- function(x,precision=1,symbol=TRUE) {
    if ( length(x) == 0 )
        return(character())
    x <- round((x*100), precision)
    if( symbol ){
	    return(paste0(x, "%"))
	} else {
		return(x)
	}
}
