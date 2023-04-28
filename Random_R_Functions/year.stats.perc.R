year.stats.perc <- function(input,library) {
	years <- sort(unique(library$year))
    pubs.per.year <- table(library$year)
    output <- input
	for( i in 1:length(years)){
      	output[,i] <- input[,i]/pubs.per.year[i]
    }
    for( j in 1:nrow(input) ){
    	output[j,ncol(input)] <- sum(input[j,1:ncol(input)-1])/nrow(library)
    }
    return(output)
}


